use std::{collections::HashMap, error::Error, alloc::System};
use std::fmt::{self, Formatter, Display};

use super::*;

enum Either<T1, T2> {
    Left(T1),
    Right(T2),
}
use Either::*;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum ClassDataKind {
    Static,
    Field,
}
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum SubroutineDataKind {
    Argument,
    Variable,
}

struct SymbolTable {
    class_name : String,

    class_symbols: HashMap<String, (Type, ClassDataKind, u32)>,
    subroutine_symbols: HashMap<String, (Type, SubroutineDataKind, u32)>,

    class_symbol_count: HashMap<ClassDataKind, u32>,
    subroutine_symbol_count: HashMap<SubroutineDataKind, u32>,

    label_counter: LabelCounter,
}

struct LabelCounter(u32);
impl LabelCounter {
    fn new() -> Self {
        LabelCounter(0)
    }
    fn generate(&mut self) -> String {
        let label = format!("L{}", self.0);
        self.0 += 1;
        label
    }
}

impl SymbolTable {
    fn new(class_name: &str) -> Self {
        SymbolTable {
            class_name: class_name.to_string(),
            class_symbols: HashMap::new(),
            subroutine_symbols: HashMap::new(),
            class_symbol_count: HashMap::new(),
            subroutine_symbol_count: HashMap::new(),
            label_counter: LabelCounter::new(),
        }
    }
    // return 1 + greatest index present of that symbol kind
    fn next_idx(&mut self, kind: Either<&ClassDataKind, &SubroutineDataKind>) 
        -> u32 {
        match kind {
            Left(class_kind) => {
                let count = self.class_symbol_count
                    .entry(class_kind.clone())
                    .and_modify(|v| *v += 1)
                    .or_insert(0);
                *count
            },
            Right(subroutine_kind) => {
                let count = self.subroutine_symbol_count
                    .entry(subroutine_kind.clone())
                    .and_modify(|v| *v += 1)
                    .or_insert(0);
                *count
            }
        }
    }
    fn insert_symbol(
        &mut self,
        var_name: &str,
        var_type: Type,
        kind: Either<ClassDataKind, SubroutineDataKind>,
    ) 
    {
        match kind {
            Left(class_data_kind) => {
                let idx = self.next_idx(Left(&class_data_kind));
                self.class_symbols
                    .insert(
                        var_name.to_string(),
                        (var_type, class_data_kind.clone(), idx));
            }
            Right(subroutine_data_kind) => {
                let idx = self.next_idx(Right(&subroutine_data_kind));
                self.subroutine_symbols
                    .insert(
                        var_name.to_string(),
                        (var_type, subroutine_data_kind.clone(), idx));
            }
        }
    }
    fn get_segment_location(&self, var_name: &str) -> Option<(String, Type)> {
        // try lookup in subroutine table first
        match self.subroutine_symbols.get(var_name) {
            Some((ty, kind, idx)) => {
                let segment = match kind {
                    SubroutineDataKind::Argument => "argument",
                    SubroutineDataKind::Variable => "local",
                };
                return Some((format!("{} {}", segment, idx), ty.clone()));
            },
            None => ()
        };
        // try lookup in class symbol table
        match self.class_symbols.get(var_name) {
            Some((ty, kind, idx)) => {
                let segment = match kind {
                    ClassDataKind::Static => "static",
                    ClassDataKind::Field => "this",
                };
                return Some((format!("{} {}", segment, idx), ty.clone()));
            },
            None => None
        }
    }
    fn num_field_vars(&self) -> u32 {
        if let Some(val) = self.class_symbol_count.get(&ClassDataKind::Field) {
            *val
        } else {
            0
        }
    }
    fn reset_subroutine_table(&mut self) {
        self.subroutine_symbols = HashMap::new();
        self.subroutine_symbol_count = HashMap::new();
    }
}

impl From<ClassVarKind> for ClassDataKind {
    fn from(kind: ClassVarKind) -> Self {
        match kind {
            ClassVarKind::Static => ClassDataKind::Static,
            ClassVarKind::Field => ClassDataKind::Field,
        }
    }
}
pub fn class_to_vm(class: &Class) -> Vec<String> {
    let mut symbol_table = SymbolTable::new(&class.name);
    let mut out = vec![];

    for ClassVarDecl { kind, _type, names } in &class.var_decls {
        // add class data member to class level symbol table
        for name in names {
            symbol_table.insert_symbol(
                &name,
                _type.clone(),
                Left(kind.clone().into()),
            );
        }
    }

    for sub_dec in &class.sub_decls {
        symbol_table.reset_subroutine_table();
        out.extend(sub_dec.to_vm(&mut symbol_table));
    }
    out
}
trait VM {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String>;
}

impl VM for SubroutineDecl {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String> {
        let mut out = vec![];
        // prepend with "" to space functions
        out.push("".to_string());

        // declare function, with num of local variables
        out.push(
            format!("function {}.{} {}",
                symbol_table.class_name,
                self.name,
                self.body.var_decls
                    .iter()
                    .map(|(_, names)| names.len())
                    .sum::<usize>(),
            ));

        match self.kind {
            SubroutineKind::Method => {
                // put 'this' -> Object(classname), arg 0 intto symbol table
                symbol_table.insert_symbol(
                    "this",
                    Type::Object(symbol_table.class_name.clone()),
                    Right(SubroutineDataKind::Argument),
                );
                // align the 'this' poitner
                out.push("push argument 0".to_string());
                out.push("pop pointer 0".to_string());

                // add args to symbol table
                for (_type, name) in &self.args {
                    symbol_table.insert_symbol(
                        &name,
                        _type.clone(),
                        Right(SubroutineDataKind::Argument));
                }
            },
            SubroutineKind::Constructor => {
                let size = symbol_table.num_field_vars();
                // allocate segment on heap for this object
                out.push(format!("push constant {}", size));
                out.push("call Memory.alloc 1".to_string());
                // put returned addr into pointer 0
                // this has the side effect of correctly allinging the 'this' pointer
                out.push("pop pointer 0".to_string());

                // put 'this' in subroutine symbol table, used when we 'return this'
                // symbol_table.insert_symbol(
                //     token_identifier!("this"),
                //     Type::ClassName(symbol_table.class_name.clone()),
                //     Err(SubroutineKind::Pointer)
                // );
            },
            SubroutineKind::Function => (),
        };

        // insert args to subroutine table
        for (_type, name) in &self.args {
            symbol_table.insert_symbol(
                &name,
                _type.clone(),
                Right(SubroutineDataKind::Argument));
        }
        out.extend(self.body.to_vm(symbol_table));
        out
    }
}
impl VM for SubroutineBody {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String> {
        let mut out = vec![];
        // add variable declarations to symbol table
        for (_type, names) in &self.var_decls {
            for name in names {
                symbol_table.insert_symbol(
                    &name,
                    _type.clone(),
                    Right(SubroutineDataKind::Variable));
            }
        }
        // translate statements to vm
        for statement in &self.statements {
            out.extend(statement.to_vm(symbol_table));
        }
        out
    }
}

impl VM for Statement {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String> {
        let mut out = vec![];
        match self {
            Self::Let { var_name, index, expr } =>  {
                if let Some(index_expr) = index {
                    // put address of (var_name(pointer) + index) into temp 0
                    out.push(format!("push {}",
                        symbol_table.get_segment_location(var_name).unwrap().0));
                    out.extend(index_expr.to_vm(symbol_table));
                    out.push("add".to_string());
                    out.push("pop temp 0".to_string());

                    // evaluate expr onto stack
                    out.extend(expr.to_vm(symbol_table));
                    
                    // set that 0 to *(var_name + index), then push expr into that 0
                    out.push("push temp 0".to_string());
                    out.push("pop pointer 1".to_string());
                    out.push("push that 0".to_string());
                }
                else {
                    // evaluate expr
                    out.extend(expr.to_vm(symbol_table));
                    // find segment location of variable
                    let segment_location = symbol_table
                        .get_segment_location(var_name)
                        .unwrap().0;
                    // pop evaluated expr into segment location
                    out.push(format!("pop {}", &segment_location));
                }

            },
            Self::If {cond, if_true, if_false} => {
                let l1 = symbol_table.label_counter.generate();
                let l2 = symbol_table.label_counter.generate();
                // evaluate condition expressoin
                out.extend(cond.to_vm(symbol_table));
                out.push("not".to_string());
                out.push(format!("if-goto {}", l1));
                // extend if true statements onto stack
                for statement in if_true {
                    out.extend(statement.to_vm(symbol_table));
                }
                out.push(format!("goto {}", l2));
                out.push(format!("label {}", l1));
                // extend if false statements onto stack
                if let Some(statements) = if_false {
                    for statement in statements {
                        out.extend(statement.to_vm(symbol_table));
                    }
                }
                out.push(format!("label {}", l2));
            },
            Self::While { cond, statements } => {
                let l1 = symbol_table.label_counter.generate();
                let l2 = symbol_table.label_counter.generate();
                out.push(format!("label {}", l1));
                // extend cond expression onto stack
                out.extend(cond.to_vm(symbol_table));
                out.push("not".to_string());
                out.push(format!("if-goto {}", l2));
                for statement in statements {
                    out.extend(statement.to_vm(symbol_table));
                }
                out.push(format!("goto {}", l1));
                out.push(format!("label {}", l2));
            },
        
            Self::Do(subroutine_call) => {
                // call subroutine
                out.extend(subroutine_call.to_vm(symbol_table));
                // subroutine will always pop one value onto stack- remove it
                out.push("pop temp 0".to_string());
            },
            Self::Return(opt_expr) => {
                // extend expr onto stack, then push 'return'
                // we should always return something, so if no expr return 0
                if let Some(expr) = opt_expr {
                    out.extend(expr.to_vm(symbol_table));
                } else {
                    out.push("push constant 0".to_string());
                }
                out.push("return".to_string());
            },
        }
        out
    }
}

impl VM for Op {
    fn to_vm(&self, _: &mut SymbolTable) -> Vec<String> {
        vec![match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "call Math.multiply 2",
            Self::Div => "call Math.divide 2",
            Self::And => "and",
            Self::Or => "or",
            Self::Lt => "lt",
            Self::Gt => "gt",
            Self::Eq => "eq",
        }.to_string()]
    }
}
impl VM for UnaryOp {
    fn to_vm(&self, _: &mut SymbolTable) -> Vec<String> {
        match self {
            Self::Neg => vec!["neg".to_string()],
            Self::Not => vec!["not".to_string()],
        }
    }
}
impl VM for Expr {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String> {
        let mut out = vec![];

        let term = self.first.as_ref();
        out.extend(term.to_vm(symbol_table));

        for (op, term) in &self.rest {
            // evaluate term on stack, call op
            out.extend(term.to_vm(symbol_table));
            out.extend(op.to_vm(symbol_table));
        }
        out
    }
}
impl VM for Term {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String> {
       match self {
            Self::IntConst(i) => vec![format!("push constant {}", i)],
            Self::StrConst(s) => {
                let mut out = vec![];
                out.push(format!("push constant {}", s.len()));
                out.push("call String.new 1".to_string());
                for c in s.chars() {
                    out.push(format!("push constant {}", c));
                    out.push("call String.appendChar 2".to_string());
                }
                out
            },
            Self::KeyConst(k) => match k {
                KeyConst::True => vec!["push constant 0".to_string()],
                KeyConst::False => vec!["push constant 1".to_string()],
                // 'this', for when we call "return this" from a constructor
                KeyConst::This => vec!["push pointer 0".to_string()],
                KeyConst::Null => vec!["push constant 0".to_string()],
            },
            Self::Var(var_name) => {
                let segment_location = symbol_table
                    .get_segment_location(&var_name)
                    .unwrap().0;
                vec![format!("push {}", segment_location)]
            },
            Self::IndexedVar { name, expr } => {
                let mut out = vec![];
                // push symbol mapping of object (a pointer)
                out.push(format!("push {}",
                    symbol_table.get_segment_location(name).unwrap().0));
                // evalauate expr onto stack
                out.extend(expr.to_vm(symbol_table));
                out.push("add".to_string());
                // set that to look at the object's data value of expr
                out.push("push pointer 1".to_string());
                out.push("pop that 0".to_string());
                out
            }
            Self::BracketedExpr(expr) => {
                expr.to_vm(symbol_table)
            },
            // evaluated term on stack, extend 'op'
            Self::UnaryOp(u_op, term) => {
                let mut out = vec![];
                out.extend(term.to_vm(symbol_table));
                out.extend(u_op.to_vm(symbol_table));
                out
            },
            Self::SubCall(sub_call) => sub_call.to_vm(symbol_table),
        }
    }
}

impl VM for SubCall {
    fn to_vm(&self, symbol_table: &mut SymbolTable) -> Vec<String> {
        /*
        f(x1, ...) is a method
        a.f(x1, ...) =>
        if a is in our symbol table, then a.f is a method
        else it is a static function, where a is the name of the class type
        contructor is effectively a static method here
        */
        let mut out = vec![];
        let mut n_args = 0;
        let class_name: String;

        // f(..) hence method
        if self.name_1 == None {
            // find segment location of 'this'
            let segment_location = symbol_table.get_segment_location("this").unwrap().0;
            // extend pointer to object onto stack
            out.push(format!("push {}", segment_location));
            n_args += 1;
            class_name = symbol_table.class_name.clone();
        // a.f(..), a is in symbol table, hence a is a method
        } else if let Some((segment_location, _)) =
            symbol_table.get_segment_location(self.name_1.as_ref().unwrap()) {
            out.push(format!("push {}", segment_location));
            n_args += 1;
            class_name = self.name_1.as_ref().unwrap().clone();
        // a.f(..), f is a static function
        } else {
            class_name = self.name_1.as_ref().unwrap().clone();
        }

        // extend arguments onto stack
        for arg in &self.args {
            out.extend(arg.to_vm(symbol_table));
            n_args += 1;
        }
        // call class method/function on args
        out.push(format!("call {}.{} {}", class_name, self.name_2, n_args));
        out
    }
}
