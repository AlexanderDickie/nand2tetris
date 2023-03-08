use chumsky::{prelude::*, text::ident};
use vm::class_to_vm;
use core::panic;
use std::{fs, path::Path};

mod vm;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Keyword(Keyword),
    Symbol(Symbol),
    Ident(String),
    IntConst(String),
    StrConst(String),
    Comment,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,
    LessThan,
    GreaterThan,
    Equal,
    Tilde,
}

#[macro_export]
macro_rules! token_keyword (
    ($k:ident) => ({
        Token::Keyword(Keyword::$k)
    });
);
#[macro_export]
macro_rules! token_symbol (
    ($k:ident) => ({
        Token::Symbol(Symbol::$k)
    });
);
#[macro_export]
macro_rules! token_identifier (
    ($k:expr) => ({
        Token::Identifier($k.into())
    });
);
#[macro_export]
macro_rules! token_integer_constant (
    ($k:expr) => ({
        Token::IntegerConstant($k)
    });
);
#[macro_export]
macro_rules! token_string_constant (
    ($k:expr) => ({
        Token::String($k)
    });
);
pub fn lexer() -> impl Parser<char, Vec<Token>, Error=Simple<char>> {
    // this will allow 111abc parsed as Token::Int and Token::Ident
    let keyword = choice((
        just("class").to(token_keyword!(Class)),
        just("constructor").to(token_keyword!(Constructor)),
        just("function").to(token_keyword!(Function)),
        just("method").to(token_keyword!(Method)),
        just("field").to(token_keyword!(Field)),
        just("static").to(token_keyword!(Static)),
        just("var").to(token_keyword!(Var)),
        just("int").to(token_keyword!(Int)),
        just("char").to(token_keyword!(Char)),
        just("boolean").to(token_keyword!(Boolean)),
        just("void").to(token_keyword!(Void)),
        just("true").to(token_keyword!(True)),
        just("false").to(token_keyword!(False)),
        just("null").to(token_keyword!(Null)),
        just("this").to(token_keyword!(This)),
        just("let").to(token_keyword!(Let)),
        just("do").to(token_keyword!(Do)),
        just("if").to(token_keyword!(If)),
        just("else").to(token_keyword!(Else)),
        just("while").to(token_keyword!(While)),
        just("return").to(token_keyword!(Return)),
    ));

    let symbol = choice((
        just("(").to(token_symbol!(LeftParen)),
        just(")").to(token_symbol!(RightParen)),
        just("{").to(token_symbol!(LeftBrace)),
        just("}").to(token_symbol!(RightBrace)),
        just("[").to(token_symbol!(LeftBracket)),
        just("]").to(token_symbol!(RightBracket)),
        just(".").to(token_symbol!(Dot)),
        just(",").to(token_symbol!(Comma)),
        just(";").to(token_symbol!(Semicolon)),
        just("+").to(token_symbol!(Plus)),
        just("-").to(token_symbol!(Minus)),
        just("*").to(token_symbol!(Asterisk)),
        just("/").to(token_symbol!(Slash)),
        just("&").to(token_symbol!(Ampersand)),
        just("|").to(token_symbol!(Pipe)),
        just("<").to(token_symbol!(LessThan)),
        just(">").to(token_symbol!(GreaterThan)),
        just("=").to(token_symbol!(Equal)),
        just("~").to(token_symbol!(Tilde)),
    ));

    let _ident = ident()
        .map(|i| Token::Ident(i));

    let int_const = text::int(10)
        .map(|s: String| Token::IntConst(s.to_string()));

    let str_const = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|s| Token::StrConst(s));

    let comment1 = just("//")
        .then(take_until(just("\n")))
        .map(|_| Token::Comment);
    let comment2 = just("/**")
        .then(take_until(just("*/")))
        .map(|_| Token::Comment);
    let comment = comment1.or(comment2).padded();

    comment
        .or(keyword)
        .or(symbol)
        .or(int_const)
        .or(str_const)
        .or(_ident)
        .padded()
        .repeated()
}
#[derive(Clone, Debug)]
struct Expr {
    first: Box<Term>,
    rest: Vec<(Op, Term)>,
}
#[derive(Clone, Debug)]
enum Term {
    IntConst(u32),
    StrConst(String),
    KeyConst(KeyConst),
    Var(String),
    IndexedVar{
        name: String,
        expr: Expr,
    },
    BracketedExpr(Expr),
    UnaryOp(UnaryOp, Box<Term>),
    SubCall(SubCall),
}
#[derive(Clone, Debug)]
struct SubCall {
    name_1: Option<String>,
    name_2: String,
    args: Vec<Expr>,
}
#[derive(Clone, Debug)]
enum KeyConst {
    True,
    False,
    Null,
    This,
}
#[derive(Clone, Debug)]
enum UnaryOp {
    Neg,
    Not,
}
#[derive(Clone, Debug)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Lt,
    Gt,
    Eq,
}
fn parse_expr() -> impl Parser<Token, Expr, Error=Simple<Token>> + Clone{
    recursive(|expr| {

        let term = recursive( |term| {
            let consts = select! {
                Token::IntConst(i) => Term::IntConst(i.parse::<u32>().unwrap()),
                Token::StrConst(s) => Term::StrConst(s),
            };

            let keyword = select! {
                Token::Keyword(Keyword::True) => Term::KeyConst(KeyConst::True),
                Token::Keyword(Keyword::False) => Term::KeyConst(KeyConst::False),
                Token::Keyword(Keyword::Null) => Term::KeyConst(KeyConst::Null),
                Token::Keyword(Keyword::This) => Term::KeyConst(KeyConst::This),
            };

            let ident = select! { Token::Ident(ident) => ident.clone()};

            let var_name = ident.map(|i| Term::Var(i));

            let indexed_var = ident
                .then(
                    expr.clone().delimited_by(
                            just(token_symbol!(LeftBracket)),
                            just(token_symbol!(RightBracket))
                        ))
                .map(|(name, expr)| Term::IndexedVar { name, expr});

            let bracketed_expr = 
                expr.clone()
                .delimited_by(just(token_symbol!(LeftParen)), just(token_symbol!(RightParen)))
                .map(|_expr| Term::BracketedExpr(_expr));

            let unary_op = select! {
                Token::Symbol(Symbol::Minus) => UnaryOp::Neg,
                Token::Symbol(Symbol::Tilde) => UnaryOp::Not,
            }
                .then(term.clone())
                .map(|(op, _term)| Term::UnaryOp(op, Box::new(_term)));

            keyword
                .or(parse_sub_call(expr).map(|sc| Term::SubCall(sc)))
                .or(indexed_var)
                .or(bracketed_expr)
                .or(consts)
                .or(unary_op)
                .or(var_name)
        });

        let op = select! {
            Token::Symbol(Symbol::Plus) => Op::Add,
            Token::Symbol(Symbol::Minus) => Op::Sub,
            Token::Symbol(Symbol::Asterisk) => Op::Mul,
            Token::Symbol(Symbol::Slash) => Op::Div,
            Token::Symbol(Symbol::Ampersand) => Op::And,
            Token::Symbol(Symbol::Pipe) => Op::Or,
            Token::Symbol(Symbol::LessThan) => Op::Lt,
            Token::Symbol(Symbol::GreaterThan) => Op::Gt,
            Token::Symbol(Symbol::Equal) => Op::Eq,
        };

        // term (op term)*
        term.clone()
            .then(op.clone().then(term.clone()).repeated())
            .map(|(first, rest)| Expr { first: Box::new(first), rest})
    })
}
// these parse_... are less like functions and more like objects
// take parse_expr as param to avoid stack overflow, else
// parse_sub_call would call parse_expr and vice versa
fn parse_sub_call(parse_expr: impl Parser<Token, Expr, Error=Simple<Token>> + Clone)
    -> impl Parser<Token, SubCall, Error=Simple<Token>> {
    let ident = select! {Token::Ident(i) => i.clone()};
    let expression_list = 
        parse_expr.clone()
            .then(
                just(token_symbol!(Comma))
                    .ignore_then(parse_expr.clone())
                    .repeated()
            )
            .map(|(first, mut rest)| {
                rest.insert(0, first);
                rest
            })
            .or(empty().map(|_| vec![]));

    let sc_1 = ident
        .then_ignore(just(token_symbol!(Dot)))
        .then(ident)
        .map(|(name_1, name_2)| (Some(name_1), name_2));
    let sc_2 = ident.map(|name_2| (None, name_2));

    let sub_call = sc_1
        .or(sc_2)
        .then(
            expression_list
                .delimited_by(just(token_symbol!(LeftParen)), just(token_symbol!(RightParen)))
        )
        .map(|((name_1, name_2), args)| SubCall{ name_1, name_2, args});
    sub_call
}

#[derive(Debug)]
enum Statement {
    Let {
        var_name: String,
        index: Option<Expr>,
        expr: Expr,
    },
    If {
        cond: Expr,
        if_true: Vec<Statement>,
        if_false: Option<Vec<Statement>>,
    },
    While {
        cond: Expr,
        statements: Vec<Statement>,
    },
    Do (SubCall) ,
    Return(Option<Expr>),
}
fn parse_statements()
    -> impl Parser<Token, Vec<Statement>, Error=Simple<Token>> {
    recursive(|statements| {
        use Statement::*;
        let ident = select! {Token::Ident(i) => i.clone()};

        let _let = just(token_keyword!(Let))
            .ignore_then(ident)
            .then(
                parse_expr().delimited_by(
                    just(token_symbol!(LeftBracket)),
                    just(token_symbol!(RightBracket)),
                ).map(|e| Some(e))
                .or(empty().map(|_| None))
            )
            .then_ignore(just(token_symbol!(Equal)))
            .then(parse_expr())
            .then_ignore(just(token_symbol!(Semicolon)))
            .map(|((ident, opt_idx), expr)| 
                Let { var_name: ident, index: opt_idx, expr }
            );

        let _if = just(token_keyword!(If))
            .ignore_then(parse_expr().delimited_by(
                just(token_symbol!(LeftParen)),
                just(token_symbol!(RightParen)),
            ))
            .then(statements.clone().delimited_by(
                just(token_symbol!(LeftBrace)),
                just(token_symbol!(RightBrace)),
            ))
            .then(
                just(token_keyword!(Else))
                .ignore_then(statements.clone().delimited_by(
                    just(token_symbol!(LeftBrace)),
                    just(token_symbol!(RightBrace)),
                )).map(|s| Some(s))
                .or(empty().map(|_| None))
            ).map(
                |((cond, if_true), if_false)| 
                If { cond, if_true, if_false }
            );

        let _while = just(token_keyword!(While))
            .ignore_then(parse_expr().delimited_by(
                just(token_symbol!(LeftParen)),
                just(token_symbol!(RightParen)),
            ))
            .then(statements.clone().delimited_by(
                just(token_symbol!(LeftBrace)),
                just(token_symbol!(RightBrace)),
            ))
            .map(|(cond, _statements)| While { cond, statements: _statements });

        let _do = just(token_keyword!(Do))
            .ignore_then(parse_sub_call(parse_expr()))
            .then_ignore(just(token_symbol!(Semicolon)))
            .map(|sc| Do(sc));

        let _return = just(token_keyword!(Return))
            .ignore_then(
                parse_expr().map(|e| Some(e))
                .or(empty().map(|_| None))
            )
            .then_ignore(just(token_symbol!(Semicolon)))
            .map(|opt_expr| Return(opt_expr));

        _let.or(_if).or(_while).or(_do).or(_return).repeated()
    })
}
#[derive(Debug, Clone)]
enum Type {
    Int,
    Char,
    Bool,
    Object(String),
}
#[derive(Debug, Clone)]
enum ClassVarKind {Static, Field}
#[derive(Debug)]
struct ClassVarDecl {
    kind: ClassVarKind,
    _type: Type,
    names: Vec<String>,
}
#[derive(Debug)]
enum SubroutineKind {
    Constructor, Function, Method,
}
#[derive(Debug)]
struct SubroutineDecl {
    kind: SubroutineKind,
    name: String,
    return_type: Option<Type>,
    args: Vec<(Type, String)>,
    body: SubroutineBody,
}
#[derive(Debug)]
struct SubroutineBody {
    var_decls: Vec<(Type, Vec<String>)>,
    statements: Vec<Statement>,
}
#[derive(Debug)]
pub struct Class {
    name: String,
    var_decls: Vec<ClassVarDecl>,
    sub_decls: Vec<SubroutineDecl>,
}
pub fn parse_class() -> impl Parser<Token, Class, Error=Simple<Token>> {
    let ident = select! {Token::Ident(i) => i.clone()};

    let parse_type = select! {
        Token::Keyword(Keyword::Int) => Type::Int,
        Token::Keyword(Keyword::Char) => Type::Char,
        Token::Keyword(Keyword::Boolean) => Type::Bool,
        Token::Ident(i) => Type::Object(i.clone()),
    };

    let class_var_kind = select! {
        Token::Keyword(Keyword::Static) => ClassVarKind::Static,
        Token::Keyword(Keyword::Field) => ClassVarKind::Field,
    };
    let class_var_decl = 
        class_var_kind
        .then(parse_type)
        .then(ident)
        .then(just(token_symbol!(Comma))
            .ignore_then(ident)
            .repeated())
        .then_ignore(just(token_symbol!(Semicolon)))
        .map(|(((kind, _type), first), mut rest)| {
            rest.insert(0, first);
            ClassVarDecl {kind, _type, names: rest}
        });

    let parameter_list = 
        parse_type
        .then(ident)
        .map(|(t, n)| Some((t, n)))
        .or(empty().map(|_| None))
        .then(
            just(token_symbol!(Comma))
            .ignore_then(parse_type)
            .then(ident)
            .repeated()
        )
        .map(|(opt, mut rest)| {
            if let Some(tup) = opt {
                rest.insert(0, tup);
            }
            rest
        });

    let subroutine_kind = select! {
        Token::Keyword(Keyword::Constructor) => SubroutineKind::Constructor,
        Token::Keyword(Keyword::Function) => SubroutineKind::Function,
        Token::Keyword(Keyword::Method) => SubroutineKind::Method,
    };
    let var_decl = 
        just(token_keyword!(Var))
        .ignore_then(parse_type)
        .then(ident)
        .then(
            just(token_symbol!(Comma))
            .ignore_then(ident)
            .repeated()
        )
        .then_ignore(just(token_symbol!(Semicolon)))
        .map(|((_type, first), mut rest)| {
            rest.insert(0, first);
            (_type, rest)
        });
    let subroutine_body = 
        var_decl.repeated()
        .then(parse_statements())
        .delimited_by(
            just(token_symbol!(LeftBrace)),
            just(token_symbol!(RightBrace)),
        )
        .map(|(var_decls, statements)| SubroutineBody {var_decls, statements});
        
    let subroutine_decl = 
        subroutine_kind
        .then(
            parse_type.map(|t| Some(t))
            .or(just(token_keyword!(Void)).map(|_| None))
        )
        .then(ident)
        .then(parameter_list.delimited_by(
            just(token_symbol!(LeftParen)),
            just(token_symbol!(RightParen)),
        ))
        .then(subroutine_body)
        .map(|((((kind, return_type), name), args), body)| 
            SubroutineDecl {
                kind, name, return_type, args, body,
            }
        );

    just(token_keyword!(Class))
        .ignore_then(ident)
        .then_ignore(just(token_symbol!(LeftBrace)))
        .then(class_var_decl.repeated())
        .then(subroutine_decl.repeated())
        .then_ignore(just(token_symbol!(RightBrace)))
        .map(|((name, var_decls), sub_decls)| Class {name, var_decls, sub_decls})
}


fn main() {
    // read directory
    let path = std::env::args().nth(1).unwrap();
    let path = Path::new(&path);
    if !path.is_dir() {
        panic!("path not a directory");
    }

    for file in path.read_dir().unwrap() {
        let file_path = file.unwrap().path();

        if file_path.extension().unwrap() == "jack" {
            let tokens = lexer().parse(fs::read_to_string(&file_path).unwrap()).expect("lex error");
            let tokens = tokens
                .into_iter()
                .filter(|t| *t != Token::Comment)
                .collect::<Vec<_>>();
            let class = parse_class().parse(tokens).expect("failed to parse class");
            let vm = class_to_vm(&class);
            fs::write(file_path.with_extension("vm"), vm.join("\n")).expect("failed to write");
        }
    }
}



