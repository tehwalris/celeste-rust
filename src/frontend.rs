use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::Index,
    process::id,
};

use anyhow::{bail, Result};
use full_moon::{
    ast::{self, Return},
    tokenizer::{Symbol, TokenKind, TokenReference, TokenType},
};
use itertools::Itertools;
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

    fn compile_and_or(
        &mut self,
        is_and: bool,
        left_expr: &ast::Expression,
        right_expr: &ast::Expression,
        locals: &HashMap<String, LocalId>,
    ) -> Result<(LocalId, Stream)> {
        let (left_id, _, left_stream) = self.compile_rhs_expression(left_expr, locals, None)?;
        let (right_id, _, right_stream) = self.compile_rhs_expression(right_expr, locals, None)?;
        let result_id = self.local_id_generator.next();
        let left_label = self.label_generator.next("and_or_left");
        let right_label = self.label_generator.next("and_or_right");
        let continue_label = self.label_generator.next("and_or_continue");
        let join_label = self.label_generator.next("and_or_join");
        let (true_label, false_label) = if is_and {
            (continue_label.clone(), join_label.clone())
        } else {
            (join_label.clone(), continue_label.clone())
        };
        let result_stream = Stream::from_streams(vec![
            left_stream,
            Stream(vec![
                StreamElement::Terminator(
                    self.local_id_generator.next(),
                    Terminator::UnconditionalBranch {
                        target: left_label.clone(),
                    },
                ),
                StreamElement::Label(left_label.clone()),
                StreamElement::Terminator(
                    self.local_id_generator.next(),
                    Terminator::ConditionalBranch {
                        condition: left_id,
                        true_target: true_label.clone(),
                        false_target: false_label.clone(),
                    },
                ),
                StreamElement::Label(continue_label),
            ]),
            right_stream,
            Stream(vec![
                StreamElement::Terminator(
                    self.local_id_generator.next(),
                    Terminator::UnconditionalBranch {
                        target: right_label.clone(),
                    },
                ),
                StreamElement::Label(right_label.clone()),
                StreamElement::Terminator(
                    self.local_id_generator.next(),
                    Terminator::UnconditionalBranch {
                        target: join_label.clone(),
                    },
                ),
                StreamElement::Label(join_label.clone()),
                StreamElement::Instruction(
                    result_id,
                    Instruction::Phi {
                        branches: vec![(left_label, left_id), (right_label, right_id)],
                    },
                ),
            ]),
        ]);
        Ok((result_id, result_stream))
    }

    fn compile_closure(
        &mut self,
        function: &ast::FunctionBody,
        base_name: &str,
        locals: &HashMap<String, LocalId>,
    ) -> Result<(LocalId, Stream)> {
        let params = function
            .parameters()
            .iter()
            .map(|param| match param {
                ast::Parameter::Name(name) => identifier_from_token_reference(name),
                _ => bail!("unsupported parameter {:?}", param),
            })
            .collect::<Result<Vec<_>>>()?;
        let unique_params: HashSet<&str> = params.iter().cloned().collect();
        assert!(params.len() == unique_params.len());

        let inner_arg_val_ids: Vec<_> = params
            .iter()
            .map(|_| self.local_id_generator.next())
            .collect();
        let inner_arg_var_ids: Vec<_> = params
            .iter()
            .map(|_| self.local_id_generator.next())
            .collect();

        let mut arg_var_stream = Stream::new();
        for (val_id, var_id) in inner_arg_val_ids.iter().zip_eq(inner_arg_var_ids.iter()) {
            arg_var_stream
                .0
                .push(StreamElement::Instruction(*var_id, Instruction::Alloc));
            arg_var_stream.0.push(StreamElement::Instruction(
                self.local_id_generator.next(),
                Instruction::Store {
                    target: *var_id,
                    source: *val_id,
                },
            ));
        }

        let sorted_outer_locals: Vec<_> = locals
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .sorted_by_key(|(k, _)| k.clone())
            .collect();

        let inner_capture_ids: Vec<_> = sorted_outer_locals
            .iter()
            .map(|_| self.local_id_generator.next())
            .collect();
        let mut inner_locals = HashMap::new();
        for ((name, _outer_id), inner_id) in
            sorted_outer_locals.iter().zip_eq(inner_capture_ids.iter())
        {
            inner_locals.insert(name.to_string(), *inner_id);
        }
        for (name, inner_id) in params.iter().zip_eq(inner_arg_var_ids.iter()) {
            inner_locals.insert(name.to_string(), *inner_id);
        }

        let mut inner_stream = self.compile_block(function.block(), None, &inner_locals)?;

        inner_stream.0.extend(arg_var_stream.0);
        match inner_stream.0.last() {
            Some(StreamElement::Terminator(_, Terminator::Return { .. })) => {}
            _ => {
                inner_stream.0.push(StreamElement::Terminator(
                    self.local_id_generator.next(),
                    Terminator::Return { value: None },
                ));
            }
        }

        let inner_build_result = inner_stream.build();
        let inner_capture_ids_remapped: Vec<_> = inner_capture_ids
            .into_iter()
            .map(|id| inner_build_result.new_ids_by_old_ids.get(&id).cloned())
            .collect();
        let outer_capture_ids: Vec<_> = sorted_outer_locals
            .iter()
            .zip_eq(inner_capture_ids_remapped.iter())
            .filter_map(|((_, outer_id), inner_id)| match inner_id {
                Some(_) => Some(*outer_id),
                None => None,
            })
            .collect();

        let global_name = self.global_id_generator.next(base_name);
        let fun_def = FunDef {
            name: global_name.clone(),
            capture_ids: inner_capture_ids_remapped
                .into_iter()
                .filter_map(|id| id)
                .collect(),
            arg_ids: inner_arg_val_ids
                .into_iter()
                .map(|id| inner_build_result.new_ids_by_old_ids.get(&id).cloned())
                .collect(),
            cfg: inner_build_result.cfg,
        };

        let closure_id = self.local_id_generator.next();
        let mut outer_stream = Stream(
            inner_build_result
                .fun_defs
                .into_iter()
                .map(|fun_def| StreamElement::Function(fun_def))
                .collect(),
        );
        outer_stream.0.push(StreamElement::Function(fun_def));
        outer_stream
            .0
            .push(StreamElement::Instruction(closure_id, Instruction::Alloc));
        outer_stream.0.push(StreamElement::Instruction(
            self.local_id_generator.next(),
            Instruction::StoreClosure {
                target: closure_id,
                fun_def: global_name,
                captures: outer_capture_ids,
            },
        ));

        Ok((closure_id, outer_stream))
    }

    fn compile_rhs_expression(
        &mut self,
        expression: &ast::Expression,
        locals: &HashMap<String, LocalId>,
        hint_from_parent: Option<&str>,
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
                            let (value_id, _, value_stream) = self.compile_rhs_expression(
                                value,
                                locals,
                                hint.as_ref().map(|s| s.as_str()),
                            )?;
                            stream.0.extend(value_stream.0);
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
            ast::Expression::UnaryOperator { unop, expression } => {
                let (inner_id, inner_hint, inner_stream) =
                    self.compile_rhs_expression(expression, locals, hint_from_parent)?;
                let (result_id, result_stream) = self.gen_id_and_stream(Instruction::UnaryOp {
                    op: match unop {
                        ast::UnOp::Minus(_) => "-",
                        ast::UnOp::Not(_) => "not",
                        ast::UnOp::Hash(_) => "#",
                        _ => bail!("unsupported unary operator {:?}", unop),
                    }
                    .to_string(),
                    arg: inner_id,
                });
                Ok((
                    result_id,
                    inner_hint,
                    Stream::from_streams(vec![inner_stream, result_stream]),
                ))
            }
            ast::Expression::BinaryOperator {
                lhs,
                binop: ast::BinOp::And { .. },
                rhs,
            } => {
                let (id, stream) = self.compile_and_or(true, lhs, rhs, locals)?;
                Ok((id, None, stream))
            }
            ast::Expression::BinaryOperator {
                lhs,
                binop: ast::BinOp::Or { .. },
                rhs,
            } => {
                let (id, stream) = self.compile_and_or(false, lhs, rhs, locals)?;
                Ok((id, None, stream))
            }
            ast::Expression::BinaryOperator { lhs, binop, rhs } => {
                let (lhs_id, lhs_hint, lhs_stream) =
                    self.compile_rhs_expression(lhs, locals, hint_from_parent)?;
                let (rhs_id, _, rhs_stream) =
                    self.compile_rhs_expression(rhs, locals, hint_from_parent)?;
                let (result_id, result_stream) = self.gen_id_and_stream(Instruction::BinaryOp {
                    left: lhs_id,
                    op: match binop {
                        ast::BinOp::And(_) => unreachable!(),
                        ast::BinOp::Caret(_) => "^",
                        ast::BinOp::GreaterThan(_) => ">",
                        ast::BinOp::GreaterThanEqual(_) => ">=",
                        ast::BinOp::LessThan(_) => "<",
                        ast::BinOp::LessThanEqual(_) => "<=",
                        ast::BinOp::Minus(_) => "-",
                        ast::BinOp::Or(_) => unreachable!(),
                        ast::BinOp::Percent(_) => "%",
                        ast::BinOp::Plus(_) => "+",
                        ast::BinOp::Slash(_) => "/",
                        ast::BinOp::Star(_) => "*",
                        ast::BinOp::TildeEqual(_) => "~=",
                        ast::BinOp::TwoDots(_) => "..",
                        ast::BinOp::TwoEqual(_) => "==",
                        _ => bail!("unsupported binary operator {:?}", binop),
                    }
                    .to_string(),
                    right: rhs_id,
                });
                Ok((
                    result_id,
                    lhs_hint,
                    Stream::from_streams(vec![lhs_stream, rhs_stream, result_stream]),
                ))
            }
            ast::Expression::Function((_, function)) => {
                let name = match hint_from_parent {
                    Some(name) => name,
                    None => "anonymous",
                };
                let (id, stream) = self.compile_closure(function, name, locals)?;
                Ok((id, None, stream))
            }
            _ => unimplemented!(),
        }
    }

    fn compile_block(
        &mut self,
        block: &ast::Block,
        break_label: Option<&Label>,
        locals: &HashMap<String, LocalId>,
    ) -> Result<Stream> {
        let mut stream = Stream::new();
        let mut locals = locals.clone();
        for statement in block.stmts() {
            let (extra_stream, new_locals) =
                self.compile_statement(statement, break_label, locals)?;
            stream.0.extend(extra_stream.0);
            locals = new_locals;
        }
        if let Some(last_statement) = block.last_stmt() {
            let extra_stream = self.compile_last_statement(last_statement, break_label, locals)?;
            stream.0.extend(extra_stream.0);
        }
        Ok(stream)
    }

    fn compile_statement(
        &mut self,
        statement: &ast::Stmt,
        break_label: Option<&Label>,
        locals: HashMap<String, LocalId>,
    ) -> Result<(Stream, HashMap<String, LocalId>)> {
        todo!()
    }

    fn compile_last_statement(
        &mut self,
        statement: &ast::LastStmt,
        break_label: Option<&Label>,
        locals: HashMap<String, LocalId>,
    ) -> Result<Stream> {
        todo!()
    }
}
