use std::{collections::HashMap, hash::Hash};

use anyhow::{bail, Result};
use full_moon::{
    ast,
    tokenizer::{TokenKind, TokenReference, TokenType},
};

use crate::ir::{
    Block, Cfg, FunDef, GlobalIdGenerator, Instruction, Label, LabelGenerator, LocalId,
    LocalIdGenerator, Terminator,
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

    /*
      let rec compile_lhs_expression (c : Ctxt.t) (expr : ast)
      (create_if_missing : bool) : Ir.local_id * string option * stream =
    match expr with
    | Ident name -> (
        match Ctxt.lookup_opt name c with
        | Some id -> (id, Some name, [])
        | None ->
            let id, stream =
              gen_id_and_stream (Ir.GetGlobal (name, create_if_missing))
            in
            (id, Some name, stream))
    | Clist [ lhs_expr; Key1 rhs_expr ] ->
        let lhs_id, lhs_hint, lhs_stream =
          compile_rhs_expression c lhs_expr None
        in
        let rhs_id, rhs_hint, rhs_stream =
          compile_rhs_expression c rhs_expr None
        in
        let hint =
          match (lhs_hint, rhs_hint) with
          | Some lhs_hint, Some rhs_hint -> Some (lhs_hint ^ "[" ^ rhs_hint ^ "]")
          | Some lhs_hint, None -> Some lhs_hint
          | None, Some rhs_hint -> Some rhs_hint
          | None, None -> None
        in
        let result_id = gen_local_id () in
        ( result_id,
          hint,
          lhs_stream >@ rhs_stream
          >:: I (result_id, Ir.GetIndex (lhs_id, rhs_id, create_if_missing)) )
    | Clist [ lhs_expr; Key2 (Ident field_name) ] ->
        let lhs_id, lhs_hint, lhs_stream =
          compile_rhs_expression c lhs_expr None
        in
        let hint =
          match lhs_hint with
          | Some h -> Some (h ^ "." ^ field_name)
          | None -> Some field_name
        in
        let result_id = gen_local_id () in
        ( result_id,
          hint,
          lhs_stream
          >:: I (result_id, Ir.GetField (lhs_id, field_name, create_if_missing))
        )
    | _ -> unsupported_ast expr
    */

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
                        (result_id, hint, result_stream)
                    }
                    ast::Suffix::Index(ast::Index::Dot { dot: _, name }) => todo!(),
                    _ => Err(anyhow!("not implemented"))?,
                };

                todo!()
            }
            _ => unimplemented!(),
        }
    }

    fn compile_rhs_expression(
        &mut self,
        expression: &ast::Expression,
        locals: &HashMap<String, LocalId>,
        hint_from_parent: Option<String>,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        todo!()
    }
}
