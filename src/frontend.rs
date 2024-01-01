use std::{collections::HashMap, hash::Hash, ops::Index, process::id};

use anyhow::{bail, Result};
use full_moon::{
    ast,
    tokenizer::{Symbol, TokenKind, TokenReference, TokenType},
};
use regex::internal::Inst;

use crate::{
    ir::{
        Block, Cfg, FunDef, GlobalIdGenerator, Instruction, Label, LabelGenerator, LocalId,
        LocalIdGenerator, Terminator,
    },
    pico8_num::Pico8Num,
};

#[derive(Clone, Debug)]
enum Hint {
    Normalize,
}

#[derive(Clone, Debug)]
enum StreamElement {
    Label(Label),
    Instruction(LocalId, Instruction),
    Hint(Hint),
    Terminator(LocalId, Terminator),
    Function(FunDef),
}

impl StreamElement {
    fn map_local_ids(&self, mut f: impl FnMut(LocalId) -> LocalId) -> Self {
        match self {
            Self::Label(_) => self.clone(),
            Self::Instruction(id, instruction) => Self::Instruction(f(*id), instruction.clone()),
            Self::Hint(_) => self.clone(),
            Self::Terminator(id, terminator) => Self::Terminator(f(*id), terminator.clone()),
            // Intentionally not mapping the ids in fun_def because they refer to the inner
            // CFG, not the one that this stream is for.
            Self::Function(fun_def) => Self::Function(fun_def.clone()),
        }
    }
}

struct Stream(Vec<StreamElement>);

struct StreamBuildResult {
    cfg: Cfg,
    fun_defs: Vec<FunDef>,
    new_ids_by_old_ids: HashMap<LocalId, LocalId>,
}

impl Stream {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn from_element(element: StreamElement) -> Self {
        Self(vec![element])
    }

    fn from_streams(streams: impl IntoIterator<Item = Self>) -> Self {
        Self(streams.into_iter().flat_map(|s| s.0).collect())
    }

    fn map_local_ids(&self, mut f: impl FnMut(LocalId) -> LocalId) -> Self {
        Self(self.0.iter().map(|el| el.map_local_ids(&mut f)).collect())
    }

    fn make_local_ids_dense(&self) -> (Self, HashMap<LocalId, LocalId>) {
        let mut new_ids_by_old_ids = HashMap::new();
        let mut id_generator = LocalIdGenerator::new();
        let stream = self.map_local_ids(|old_id| match new_ids_by_old_ids.get(&old_id) {
            Some(new_id) => *new_id,
            None => {
                let new_id = id_generator.next();
                new_ids_by_old_ids.insert(old_id, new_id);
                new_id
            }
        });
        (stream, new_ids_by_old_ids)
    }

    fn build_inner(self) -> (Cfg, Vec<FunDef>) {
        struct BlockBuilder {
            instructions: Vec<(LocalId, Instruction)>,
            terminator: Option<(LocalId, Terminator)>,
            hint_normalize: bool,
        }

        impl BlockBuilder {
            fn new() -> Self {
                Self {
                    instructions: Vec::new(),
                    terminator: None,
                    hint_normalize: false,
                }
            }

            fn build_and_clear(&mut self) -> Block {
                Block {
                    instructions: std::mem::take(&mut self.instructions),
                    terminator: self.terminator.take().unwrap(),
                    hint_normalize: std::mem::take(&mut self.hint_normalize),
                }
            }
        }

        let mut block_builder = BlockBuilder::new();
        let mut named_blocks = HashMap::new();
        let mut fun_defs = Vec::new();

        for el in self.0.into_iter().rev() {
            match el {
                StreamElement::Label(label) => {
                    let block = block_builder.build_and_clear();
                    named_blocks.insert(label, block);
                }
                StreamElement::Instruction(id, instruction) => {
                    block_builder.instructions.push((id, instruction));
                }
                StreamElement::Hint(Hint::Normalize) => {
                    block_builder.hint_normalize = true;
                }
                StreamElement::Terminator(id, terminator) => {
                    block_builder.terminator = Some((id, terminator));
                }
                StreamElement::Function(fun_def) => {
                    fun_defs.push(fun_def);
                }
            }
        }

        let entry = block_builder.build_and_clear();

        (
            Cfg {
                entry,
                named: named_blocks,
            },
            fun_defs,
        )
    }

    fn build(self) -> StreamBuildResult {
        let (stream, new_ids_by_old_ids) = self.make_local_ids_dense();
        let (cfg, fun_defs) = stream.build_inner();
        StreamBuildResult {
            cfg,
            fun_defs,
            new_ids_by_old_ids,
        }
    }
}

fn identifier_from_token_reference(token_reference: &TokenReference) -> Result<&str> {
    match token_reference.token_type() {
        TokenType::Identifier { identifier } => Ok(identifier),
        _ => bail!("Expected identifier, found {:?}", token_reference),
    }
}

struct Compiler {
    local_id_generator: LocalIdGenerator,
    global_id_generator: GlobalIdGenerator,
    label_generator: LabelGenerator,
}

impl Compiler {
    fn new() -> Self {
        Self {
            local_id_generator: LocalIdGenerator::new(),
            global_id_generator: GlobalIdGenerator::new(),
            label_generator: LabelGenerator::new(),
        }
    }

    fn gen_id_and_stream(&mut self, instruction: Instruction) -> (LocalId, Stream) {
        let id = self.local_id_generator.next();
        let stream = Stream::from_element(StreamElement::Instruction(id, instruction));
        (id, stream)
    }

    fn add_terminator_if_needed(&mut self, stream: &mut Stream, terminator: Terminator) {
        if let Some(StreamElement::Terminator(_, _)) = stream.0.last() {
            return;
        }
        stream.0.push(StreamElement::Terminator(
            self.local_id_generator.next(),
            terminator,
        ));
    }

    fn compile_identifier(
        &mut self,
        identifier: &str,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match locals.get(identifier) {
            Some(&id) => Ok((id, Some(identifier.to_string()), Stream(vec![]))),
            None => {
                let (id, stream) = self.gen_id_and_stream(Instruction::GetGlobal {
                    name: identifier.to_string(),
                    create_if_missing,
                });
                Ok((id, Some(identifier.to_string()), stream))
            }
        }
    }

    fn compile_token_reference(
        &mut self,
        token_reference: &TokenReference,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match token_reference.token_type() {
            TokenType::Identifier { identifier } => {
                self.compile_identifier(identifier, locals, create_if_missing)
            }
            _ => Err(anyhow!("not implemented")),
        }
    }

    fn compile_lhs_expression(
        &mut self,
        expression: &ast::Expression,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match expression {
            ast::Expression::Var(ast::Var::Name(identifier)) => {
                self.compile_token_reference(identifier, locals, create_if_missing)
            }
            ast::Expression::Var(ast::Var::Expression(expression)) => {
                let (lhs_id, lhs_hint, lhs_stream) = match expression.prefix() {
                    ast::Prefix::Expression(expression) => {
                        self.compile_lhs_expression(expression, locals, create_if_missing)?
                    }
                    ast::Prefix::Name(name) => {
                        self.compile_token_reference(name, locals, create_if_missing)?
                    }
                    _ => Err(anyhow!("not implemented"))?,
                };
                let only_suffix = match expression.suffixes().collect::<Vec<_>>().as_slice() {
                    &[suffix] => suffix,
                    _ => bail!(
                        "expressions with multiple suffixes (call/indexing) are not supported"
                    ),
                };
                match only_suffix {
                    ast::Suffix::Call(_) => {
                        panic!("calls should not be compiled as LHS expressions")
                    }
                    ast::Suffix::Index(ast::Index::Brackets {
                        brackets: _,
                        expression,
                    }) => {
                        let (rhs_id, rhs_hint, rhs_stream) =
                            self.compile_rhs_expression(expression, locals, None)?;
                        let hint = match (lhs_hint, rhs_hint) {
                            (Some(lhs_hint), Some(rhs_hint)) => {
                                Some(format!("{}[{}]", lhs_hint, rhs_hint))
                            }
                            (Some(lhs_hint), None) => Some(lhs_hint),
                            (None, Some(rhs_hint)) => Some(rhs_hint),
                            (None, None) => None,
                        };
                        let (result_id, result_stream) =
                            self.gen_id_and_stream(Instruction::GetIndex {
                                receiver: lhs_id,
                                index: rhs_id,
                                create_if_missing,
                            });
                        Ok((result_id, hint, result_stream))
                    }
                    ast::Suffix::Index(ast::Index::Dot { dot: _, name }) => {
                        let hint = match lhs_hint {
                            Some(lhs_hint) => Some(format!("{}.{}", lhs_hint, name)),
                            None => Some(name.to_string()),
                        };
                        let (result_id, result_stream) =
                            self.gen_id_and_stream(Instruction::GetField {
                                receiver: lhs_id,
                                field: name.to_string(),
                                create_if_missing,
                            });
                        Ok((result_id, hint, result_stream))
                    }
                    _ => Err(anyhow!("not implemented")),
                }
            }
            _ => unimplemented!(),
        }
    }

    /*
      and compile_rhs_expression (c : Ctxt.t) (expr : ast)
      (hint_from_parent : string option) : Ir.local_id * string option * stream =
    let no_hint (id, stream) = (id, None, stream) in
    match expr with
    | Table (Elist assignments) ->
        let table_id = gen_local_id () in
        let assignment_code =
          List.concat_map
            (function
              | Assign (Ident field_name, value_expr) ->
                  let field_id = gen_local_id () in
                  let hint =
                    match hint_from_parent with
                    | Some h -> Some (h ^ "." ^ field_name)
                    | None -> Some field_name
                  in
                  let value_id, _, value_code =
                    compile_rhs_expression c value_expr hint
                  in
                  value_code
                  >:: I (field_id, Ir.GetField (table_id, field_name, true))
                  >:: I (gen_local_id (), Ir.Store (field_id, value_id))
              | _ -> failwith "only Assign is allowed in Table expression")
            (List.rev assignments)
        in
        ( table_id,
          None,
          List.rev
            [
              I (table_id, Ir.Alloc);
              I (gen_local_id (), Ir.StoreEmptyTable table_id);
            ]
          >@ assignment_code )
    | Number s ->
        no_hint @@ gen_id_and_stream (Ir.NumberConstant (Pico_number.of_string s))
    | Bool "true" -> no_hint @@ gen_id_and_stream (Ir.BoolConstant true)
    | Bool "false" -> no_hint @@ gen_id_and_stream (Ir.BoolConstant false)
    | Bool "nil" -> no_hint @@ gen_id_and_stream Ir.NilConstant
    | String s ->
        no_hint @@ gen_id_and_stream (Ir.StringConstant (parse_lua_string s))
    | Unop (op, inner_expr) ->
        let inner_id, inner_hint, inner_stream =
          compile_rhs_expression c inner_expr hint_from_parent
        in
        let result_id, result_stream =
          gen_id_and_stream (UnaryOp (String.trim op, inner_id))
        in
        (result_id, inner_hint, inner_stream >@ result_stream)
    | Binop ("and", left_expr, right_expr) ->
        no_hint @@ compile_and_or true c left_expr right_expr
    | Binop ("or", left_expr, right_expr) ->
        no_hint @@ compile_and_or false c left_expr right_expr
    | Binop (op, left_expr, right_expr) ->
        let left_id, lhs_hint, left_stream =
          compile_rhs_expression c left_expr hint_from_parent
        in
        let right_id, _, right_stream =
          compile_rhs_expression c right_expr
            (match op with "=" -> lhs_hint | _ -> hint_from_parent)
        in
        let result_id, binop_stream =
          gen_id_and_stream (BinaryOp (left_id, String.trim op, right_id))
        in
        (result_id, None, left_stream >@ right_stream >@ binop_stream)
    | FunctionE fun_ast ->
        let name =
          match hint_from_parent with
          | Some name -> Some name
          | None -> Some "anonymous"
        in
        no_hint @@ compile_closure c fun_ast name
    | Pexp inner_expr -> compile_rhs_expression c inner_expr hint_from_parent
    | Clist [ callee_expr; Args (Elist arg_exprs) ] ->
        let callee_id, callee_hint, callee_code =
          compile_rhs_expression c callee_expr hint_from_parent
        in
        let arg_ids, arg_codes =
          List.split
          @@ List.map
               (fun expr ->
                 let id, _, stream =
                   compile_rhs_expression c expr hint_from_parent
                 in
                 (id, stream))
               arg_exprs
        in
        let result_id = gen_local_id () in
        ( result_id,
          callee_hint,
          callee_code
          >@ List.concat @@ List.rev arg_codes
          >:: I (result_id, Ir.Call (callee_id, arg_ids)) )
    | _ ->
        let lhs_id, lhs_hint, lhs_stream = compile_lhs_expression c expr false in
        if lhs_id == -1 then unsupported_ast expr
        else
          let rhs_id = gen_local_id () in
          (rhs_id, lhs_hint, lhs_stream >:: I (rhs_id, Ir.Load lhs_id))
       */

    fn compile_rhs_expression(
        &mut self,
        expression: &ast::Expression,
        locals: &HashMap<String, LocalId>,
        hint_from_parent: Option<String>,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match expression {
            ast::Expression::TableConstructor(table) => {
                let table_id = self.local_id_generator.next();
                let mut stream = Stream(vec![
                    StreamElement::Instruction(table_id, Instruction::Alloc),
                    StreamElement::Instruction(
                        self.local_id_generator.next(),
                        Instruction::StoreEmptyTable { target: table_id },
                    ),
                ]);
                for field in table.fields() {
                    match field {
                        ast::Field::NameKey {
                            key,
                            equal: _,
                            value,
                        } => {
                            let name = identifier_from_token_reference(key)?;
                            let field_id = self.local_id_generator.next();
                            let hint = match &hint_from_parent {
                                Some(h) => Some(format!("{}.{}", h, name)),
                                None => Some(name.to_string()),
                            };
                            let (value_id, _, value_stream) =
                                self.compile_rhs_expression(value, locals, hint)?;
                            stream.0.push(StreamElement::Instruction(
                                field_id,
                                Instruction::GetField {
                                    receiver: table_id,
                                    field: name.to_string(),
                                    create_if_missing: true,
                                },
                            ));
                            stream.0.push(StreamElement::Instruction(
                                self.local_id_generator.next(),
                                Instruction::Store {
                                    target: field_id,
                                    source: value_id,
                                },
                            ));
                        }
                        _ => unimplemented!(),
                    }
                }
                Ok((table_id, None, stream))
            }
            ast::Expression::Number(number) => {
                let (id, stream) = self.gen_id_and_stream(Instruction::NumberConstant {
                    value: match number.token_type() {
                        TokenType::Number { text } => Pico8Num::from_str(text)?,
                        _ => bail!("expected number literal"),
                    },
                });
                Ok((id, None, stream))
            }
            ast::Expression::Symbol(symbol) => {
                let instruction = match symbol.token_type() {
                    TokenType::Symbol {
                        symbol: Symbol::True,
                    } => Instruction::BoolConstant { value: true },
                    TokenType::Symbol {
                        symbol: Symbol::False,
                    } => Instruction::BoolConstant { value: false },
                    TokenType::Symbol {
                        symbol: Symbol::Nil,
                    } => Instruction::NilConstant,
                    _ => bail!("unsupported symbol {:?}", symbol),
                };
                let (id, stream) = self.gen_id_and_stream(instruction);
                Ok((id, None, stream))
            }
            ast::Expression::String(string) => {
                let string = match string.token_type() {
                    TokenType::StringLiteral {
                        literal,
                        multi_line: None,
                        quote_type: _,
                    } => literal.to_string(),
                    TokenType::StringLiteral {
                        multi_line: Some(_),
                        ..
                    } => bail!("multiline strings are not supported"),
                    _ => bail!("expected string literal"),
                };
                let (id, stream) =
                    self.gen_id_and_stream(Instruction::StringConstant { value: string });
                Ok((id, None, stream))
            }
            _ => unimplemented!(),
        }
    }
}
