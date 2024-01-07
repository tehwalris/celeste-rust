use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};
use full_moon::{
    ast::{self},
    tokenizer::{Symbol, TokenReference, TokenType},
};
use itertools::Itertools;

use crate::{
    ir::{
        BinaryOp, Block, Cfg, FunDef, GlobalIdGenerator, Instruction, Label, LabelGenerator,
        LocalId, LocalIdGenerator, Terminator, UnaryOp,
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

    fn compile_prefix_as_lhs(
        &mut self,
        prefix: &ast::Prefix,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match prefix {
            ast::Prefix::Expression(expression) => {
                self.compile_lhs_expression(expression, locals, create_if_missing)
            }
            ast::Prefix::Name(name) => {
                self.compile_token_reference(name, locals, create_if_missing)
            }
            _ => Err(anyhow!("not implemented")),
        }
    }

    fn compile_prefix_as_rhs(
        &mut self,
        prefix: &ast::Prefix,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        let (lhs_id, lhs_hint, lhs_stream) =
            self.compile_prefix_as_lhs(prefix, locals, create_if_missing)?;
        let (result_id, result_stream) =
            self.gen_id_and_stream(Instruction::Load { source: lhs_id });
        Ok((
            result_id,
            lhs_hint,
            Stream::from_streams(vec![lhs_stream, result_stream]),
        ))
    }

    fn compile_var(
        &mut self,
        var: &ast::Var,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match var {
            ast::Var::Name(identifier) => {
                self.compile_token_reference(identifier, locals, create_if_missing)
            }
            ast::Var::Expression(expression) => {
                let (lhs_id, lhs_hint, lhs_stream) =
                    self.compile_prefix_as_lhs(expression.prefix(), locals, create_if_missing)?;
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

    fn compile_lhs_expression(
        &mut self,
        expression: &ast::Expression,
        locals: &HashMap<String, LocalId>,
        create_if_missing: bool,
    ) -> Result<(LocalId, Option<String>, Stream)> {
        match expression {
            ast::Expression::Var(var) => self.compile_var(var, locals, create_if_missing),
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
                        ast::UnOp::Minus(_) => UnaryOp::Minus,
                        ast::UnOp::Not(_) => UnaryOp::Not,
                        ast::UnOp::Hash(_) => UnaryOp::Hash,
                        _ => bail!("unsupported unary operator {:?}", unop),
                    },
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
                        ast::BinOp::Caret(_) => BinaryOp::Caret,
                        ast::BinOp::GreaterThan(_) => BinaryOp::GreaterThan,
                        ast::BinOp::GreaterThanEqual(_) => BinaryOp::GreaterThanEqual,
                        ast::BinOp::LessThan(_) => BinaryOp::LessThan,
                        ast::BinOp::LessThanEqual(_) => BinaryOp::LessThanEqual,
                        ast::BinOp::Minus(_) => BinaryOp::Minus,
                        ast::BinOp::Or(_) => unreachable!(),
                        ast::BinOp::Percent(_) => BinaryOp::Percent,
                        ast::BinOp::Plus(_) => BinaryOp::Plus,
                        ast::BinOp::Slash(_) => BinaryOp::Slash,
                        ast::BinOp::Star(_) => BinaryOp::Star,
                        ast::BinOp::TildeEqual(_) => BinaryOp::TildeEqual,
                        ast::BinOp::TwoDots(_) => BinaryOp::TwoDots,
                        ast::BinOp::TwoEqual(_) => BinaryOp::TwoEqual,
                        _ => bail!("unsupported binary operator {:?}", binop),
                    },
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
            ast::Expression::Parentheses {
                contained: _,
                expression,
            } => self.compile_rhs_expression(expression, locals, hint_from_parent),
            ast::Expression::FunctionCall(call) => {
                let (callee_id, callee_hint, callee_stream) =
                    self.compile_prefix_as_rhs(call.prefix(), locals, false)?;
                let arguments = match call.suffixes().collect::<Vec<_>>().as_slice() {
                    &[ast::Suffix::Call(ast::Call::AnonymousCall(
                        ast::FunctionArgs::Parentheses {
                            parentheses: _,
                            arguments,
                        },
                    ))] => arguments,
                    _ => bail!("expected single anonymous function call suffix"),
                };
                let (arg_ids, arg_streams): (Vec<_>, Vec<_>) = arguments
                    .iter()
                    .map(|arg| self.compile_rhs_expression(arg, locals, None))
                    .collect::<Result<Vec<_>>>()?
                    .into_iter()
                    .map(|(id, _, stream)| (id, stream))
                    .unzip();
                let (result_id, result_stream) = self.gen_id_and_stream(Instruction::Call {
                    closure: callee_id,
                    args: arg_ids,
                });
                Ok((
                    result_id,
                    callee_hint,
                    Stream::from_streams(vec![
                        callee_stream,
                        Stream::from_streams(arg_streams),
                        result_stream,
                    ]),
                ))
            }
            _ => {
                let (lhs_id, lhs_hint, lhs_stream) =
                    self.compile_lhs_expression(expression, locals, false)?;
                let (result_id, result_stream) =
                    self.gen_id_and_stream(Instruction::Load { source: lhs_id });
                Ok((
                    result_id,
                    lhs_hint,
                    Stream::from_streams(vec![lhs_stream, result_stream]),
                ))
            }
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
        mut locals: HashMap<String, LocalId>,
    ) -> Result<(Stream, HashMap<String, LocalId>)> {
        match statement {
            ast::Stmt::Assignment(assignment) => {
                let (lhs_id, lhs_hint, lhs_stream) = self.compile_var(
                    assignment
                        .variables()
                        .iter()
                        .exactly_one()
                        .or(Err(anyhow!("expected exactly one variable")))?,
                    &locals,
                    true,
                )?;
                let (rhs_id, _, rhs_stream) = self.compile_rhs_expression(
                    assignment
                        .expressions()
                        .iter()
                        .exactly_one()
                        .or(Err(anyhow!("expected exactly one expression")))?,
                    &locals,
                    lhs_hint.as_ref().map(|s| s.as_str()),
                )?;
                let stream = Stream::from_streams(vec![
                    lhs_stream,
                    rhs_stream,
                    Stream::from_element(StreamElement::Instruction(
                        self.local_id_generator.next(),
                        Instruction::Store {
                            target: lhs_id,
                            source: rhs_id,
                        },
                    )),
                ]);
                Ok((stream, locals))
            }
            ast::Stmt::LocalAssignment(local_assignment) => {
                let name = identifier_from_token_reference(
                    local_assignment
                        .names()
                        .iter()
                        .exactly_one()
                        .or(Err(anyhow!("expected exactly one variable")))?,
                )?;
                let expression = local_assignment
                    .expressions()
                    .iter()
                    .at_most_one()
                    .or(Err(anyhow!("expected at most one expression")))?;
                let (lhs_id, mut stream) = self.gen_id_and_stream(Instruction::Alloc);
                locals.insert(name.to_string(), lhs_id);
                if let Some(expression) = expression {
                    let (rhs_id, _, rhs_stream) =
                        self.compile_rhs_expression(expression, &locals, None)?;
                    stream.0.push(StreamElement::Instruction(
                        lhs_id,
                        Instruction::Store {
                            target: lhs_id,
                            source: rhs_id,
                        },
                    ));
                }
                Ok((stream, locals))
            }
            ast::Stmt::FunctionCall(call) => {
                if let ast::Prefix::Name(name) = call.prefix() {
                    if identifier_from_token_reference(name)? == "_hint_normalize" {
                        return Ok((
                            Stream::from_element(StreamElement::Hint(Hint::Normalize)),
                            locals,
                        ));
                    }
                }
                let (_, _, stream) = self.compile_rhs_expression(
                    &ast::Expression::FunctionCall(call.clone()),
                    &locals,
                    None,
                )?;
                Ok((stream, locals))
            }
            ast::Stmt::If(if_statement) => {
                let mut branches: Vec<(&ast::Expression, &ast::Block)> = vec![];
                branches.push((if_statement.condition(), if_statement.block()));
                if let Some(else_ifs) = if_statement.else_if() {
                    for else_if in else_ifs {
                        branches.push((else_if.condition(), else_if.block()));
                    }
                }
                let true_expression =
                    ast::Expression::Symbol(TokenReference::symbol("true").unwrap());
                if let Some(else_block) = if_statement.else_block() {
                    branches.push((&true_expression, else_block));
                }

                let condition_labels: Vec<_> = branches
                    .iter()
                    .map(|_| self.label_generator.next("if_condition"))
                    .collect();
                let body_labels: Vec<_> = branches
                    .iter()
                    .map(|_| self.label_generator.next("if_body"))
                    .collect();
                let join_label = self.label_generator.next("if_join");

                let first_condition_label = condition_labels.first().unwrap().clone();
                let condition_label_pairs = condition_labels
                    .into_iter()
                    .chain(std::iter::once(join_label.clone()))
                    .tuple_windows();

                let stream = Stream::from_streams(vec![
                    Stream::from_element(StreamElement::Terminator(
                        self.local_id_generator.next(),
                        Terminator::UnconditionalBranch {
                            target: first_condition_label,
                        },
                    )),
                    Stream::from_streams(
                        branches
                            .iter()
                            .zip_eq(condition_label_pairs)
                            .zip_eq(body_labels.into_iter())
                            .map(
                                |(
                                    ((condition, body), (condition_label, next_condition_label)),
                                    body_label,
                                )| {
                                    let (condition_id, _, condition_stream) =
                                        self.compile_rhs_expression(condition, &locals, None)?;
                                    let mut body_stream =
                                        self.compile_block(body, break_label, &locals)?;
                                    self.add_terminator_if_needed(
                                        &mut body_stream,
                                        Terminator::UnconditionalBranch {
                                            target: join_label.clone(),
                                        },
                                    );
                                    Ok(Stream::from_streams(vec![
                                        Stream::from_element(StreamElement::Label(condition_label)),
                                        condition_stream,
                                        Stream::from_element(StreamElement::Terminator(
                                            self.local_id_generator.next(),
                                            Terminator::ConditionalBranch {
                                                condition: condition_id,
                                                true_target: body_label.clone(),
                                                false_target: next_condition_label,
                                            },
                                        )),
                                        Stream::from_element(StreamElement::Label(body_label)),
                                        body_stream,
                                    ]))
                                },
                            )
                            .collect::<Result<Vec<_>>>()?,
                    ),
                    Stream::from_element(StreamElement::Label(join_label)),
                ]);

                Ok((stream, locals))
            }
            ast::Stmt::FunctionDeclaration(function) => {
                if function.name().method_name().is_some() {
                    return Err(anyhow!("methods are not supported"));
                }
                // TODO What happens if you declare "local x" and then write "function x() end"?
                // Will the function be local or global?
                let name = identifier_from_token_reference(
                    function
                        .name()
                        .names()
                        .iter()
                        .exactly_one()
                        .or(Err(anyhow!("expected exactly one name")))?,
                )?;
                let (name_id, _, name_stream) = self.compile_identifier(name, &locals, true)?;

                let (closure_id, closure_stream) =
                    self.compile_closure(function.body(), name, &locals)?;

                Ok((
                    Stream::from_streams(vec![
                        closure_stream,
                        name_stream,
                        Stream::from_element(StreamElement::Instruction(
                            self.local_id_generator.next(),
                            Instruction::Store {
                                target: name_id,
                                source: closure_id,
                            },
                        )),
                    ]),
                    locals,
                ))
            }
            ast::Stmt::NumericFor(for_statement) => {
                if for_statement.step().is_some() {
                    return Err(anyhow!("explicit step is not supported"));
                }

                let (start_id, _, start_stream) =
                    self.compile_rhs_expression(for_statement.start(), &locals, None)?;
                let (end_id, _, end_stream) =
                    self.compile_rhs_expression(for_statement.end(), &locals, None)?;

                let val_id = self.local_id_generator.next();
                let var_id = self.local_id_generator.next();
                let step_id = self.local_id_generator.next();
                let next_val_id = self.local_id_generator.next();
                let continue_id = self.local_id_generator.next();
                let init_end_label = self.label_generator.next("for_init_end");
                let head_label = self.label_generator.next("for_head");
                let body_start_label = self.label_generator.next("for_body_start");
                let body_end_label = self.label_generator.next("for_body_end");
                let join_label = self.label_generator.next("for_join");

                let mut locals_for_body = locals.clone();
                locals_for_body.insert(
                    identifier_from_token_reference(for_statement.index_variable())?.to_string(),
                    var_id,
                );
                let body_stream =
                    self.compile_block(for_statement.block(), Some(&join_label), &locals_for_body)?;

                let stream = Stream::from_streams(vec![
                    start_stream,
                    end_stream,
                    Stream(vec![
                        StreamElement::Instruction(
                            step_id,
                            Instruction::NumberConstant {
                                value: Pico8Num::from_i16(1),
                            },
                        ),
                        StreamElement::Terminator(
                            self.local_id_generator.next(),
                            Terminator::UnconditionalBranch {
                                target: init_end_label.clone(),
                            },
                        ),
                        StreamElement::Label(init_end_label.clone()),
                        StreamElement::Terminator(
                            self.local_id_generator.next(),
                            Terminator::UnconditionalBranch {
                                target: head_label.clone(),
                            },
                        ),
                        StreamElement::Label(head_label.clone()),
                        StreamElement::Instruction(
                            val_id,
                            Instruction::Phi {
                                branches: vec![
                                    (init_end_label.clone(), start_id),
                                    (body_end_label.clone(), next_val_id),
                                ],
                            },
                        ),
                        StreamElement::Instruction(
                            continue_id,
                            Instruction::BinaryOp {
                                left: val_id,
                                op: BinaryOp::LessThanEqual,
                                right: end_id,
                            },
                        ),
                        StreamElement::Terminator(
                            self.local_id_generator.next(),
                            Terminator::ConditionalBranch {
                                condition: continue_id,
                                true_target: body_start_label.clone(),
                                false_target: join_label.clone(),
                            },
                        ),
                        StreamElement::Label(body_start_label),
                        StreamElement::Instruction(var_id, Instruction::Alloc),
                        StreamElement::Instruction(
                            self.local_id_generator.next(),
                            Instruction::Store {
                                target: var_id,
                                source: val_id,
                            },
                        ),
                    ]),
                    body_stream,
                    Stream(vec![
                        StreamElement::Instruction(
                            next_val_id,
                            Instruction::BinaryOp {
                                left: val_id,
                                op: BinaryOp::Plus,
                                right: step_id,
                            },
                        ),
                        StreamElement::Terminator(
                            self.local_id_generator.next(),
                            Terminator::UnconditionalBranch {
                                target: body_end_label.clone(),
                            },
                        ),
                        StreamElement::Label(body_end_label),
                        StreamElement::Terminator(
                            self.local_id_generator.next(),
                            Terminator::UnconditionalBranch { target: head_label },
                        ),
                        StreamElement::Label(join_label),
                    ]),
                ]);

                Ok((stream, locals))
            }
            _ => unimplemented!(),
        }
    }

    fn compile_last_statement(
        &mut self,
        statement: &ast::LastStmt,
        break_label: Option<&Label>,
        locals: HashMap<String, LocalId>,
    ) -> Result<Stream> {
        match statement {
            ast::LastStmt::Return(return_stmt) => {
                match return_stmt
                    .returns()
                    .iter()
                    .at_most_one()
                    .or(Err(anyhow!("multiple return values are not supported")))?
                {
                    Some(expr) => {
                        let (expr_id, _, expr_stream) =
                            self.compile_rhs_expression(expr, &locals, None)?;
                        Ok(Stream::from_streams(vec![
                            expr_stream,
                            Stream::from_element(StreamElement::Terminator(
                                self.local_id_generator.next(),
                                Terminator::Return {
                                    value: Some(expr_id),
                                },
                            )),
                        ]))
                    }
                    None => Ok(Stream::from_element(StreamElement::Terminator(
                        self.local_id_generator.next(),
                        Terminator::Return { value: None },
                    ))),
                }
            }
            ast::LastStmt::Break(_) => Ok(Stream::from_element(StreamElement::Terminator(
                self.local_id_generator.next(),
                Terminator::UnconditionalBranch {
                    target: break_label.unwrap().clone(),
                },
            ))),
            _ => unimplemented!(),
        }
    }
}

pub fn compile(ast: &ast::Ast) -> Result<(Cfg, Vec<FunDef>)> {
    let mut compiler = Compiler::new();
    let stream = compiler.compile_block(ast.nodes(), None, &HashMap::new())?;
    let build_result = stream.build();
    Ok((build_result.cfg, build_result.fun_defs))
}
