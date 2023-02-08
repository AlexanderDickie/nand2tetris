use std::fs;
use std::fmt;
use std::path::Path;
use std::fmt::Display;
use nom::multi::many1;
use nom::{
    sequence::tuple,
    character::complete::{
        space0,
        space1,
        digit1,
        anychar,
        char as char1,
    },
    combinator::{
        map_res,
        all_consuming,
    },
    branch::alt,
    IResult,
    bytes::complete::tag,
    error::ErrorKind,
    multi::many_till
};

mod asm;

////////////////////////////////////////////////////////////

#[derive(Debug)]
struct Pop {
    segment: Segment,
    idx: u16,
}
impl ASMDis for Pop {}

#[derive(Debug)]
struct Push {
    segment: Segment,
    idx: u16,
}
impl ASMDis for Push {}

#[derive(Debug)]
enum Segment {
    Argument,
    Local,
    Static,
    Constant,
    This,
    That,
    Pointer,
    Temp,
}

#[derive(Debug)]
enum ArithmeticLogical {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}
impl ASMDis for ArithmeticLogical {}

#[derive(Debug)]
struct Label {
    name: String,
}
impl ASMDis for Label {}

#[derive(Debug)]
struct GoTo {
    name: String,
}
impl ASMDis for GoTo {}

#[derive(Debug)]
struct GoToIf {
    name: String,
}
impl ASMDis for GoToIf {}

#[derive(Debug)]
struct FunctionDef {
    name: String,
    n_locals: u16,
}
impl ASMDis for FunctionDef {}

#[derive(Debug)]
struct FunctionCall {
    name: String,
    n_args: u16,
}
impl ASMDis for FunctionCall {}

#[derive(Debug)]
struct Return {}
impl ASMDis for Return {}

trait ASMDis : ASM + fmt::Debug {}

trait ASM {
    fn to_asm(&self, scope_info: &mut ScopeInfo) -> String;
}

#[derive(Debug)]
struct ScopeInfo {
    file_stem: String,
    cur_function: Option<(String, u32)>, // (name, num returns calls so far in function)
    private_label_counter: u32,
}

impl ScopeInfo {
    fn new(file_stem: &str) -> Self {
        ScopeInfo {
            file_stem: file_stem.to_owned(),
            cur_function: None,
            private_label_counter: 0,
        }
    }

    fn build_static_variable_label(&self, i: u16) -> String {
        format!("{}.{}", self.file_stem, i)
    }


    fn build_private_jump_label(&mut self) -> String {
        /*
        label used for internal logic of assembly implementation (lt, gt, etc), not used when translating 
        a vm command goto/label/...
        */
        self.private_label_counter += 1;
        format!("{}PL.{}", self.cur_function.as_ref().unwrap().0, self.private_label_counter)
    }

    fn build_internal_jump_label(&self, label: &str) -> String {
        /*
        label used for a vm command such as label L/ goto L/...
        */
        if let Some((function_name, _)) = &self.cur_function {
            format!("{}${}", function_name, label)
        } else {
            format!("{}", label)
        }
    }

    fn enter_function(&mut self, name: &str) {self.cur_function = Some((name.to_owned(), 0))}

    fn build_return_label(&mut self) -> String {
        if let Some((function_name, n_returns)) = &mut self.cur_function {
            *n_returns += 1;
            format!("{}$ret.{}", function_name, n_returns)
        } else {
            // for the boostrap code, we call sys.init when we are not within the scope of a function
            "$return".to_string()
        }
    }

    fn build_function_def_label(&self) -> String {
        format!("{}", self.cur_function.as_ref().unwrap().0)
    }
}

impl Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Segment::*;
        match self {
            Argument => write!(f, "ARG"),
            Local => write!(f, "LCL"),
            This => write!(f, "THIS"),
            That => write!(f, "THAT"),
            _ => panic!("shouldnt need to display this segment"),
        }
    }
}

impl Display for ArithmeticLogical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ArithmeticLogical::*;
        match self {
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Neg => write!(f, "neg"),
            Eq => write!(f, "eq"),
            Gt => write!(f, "gt"),
            Lt => write!(f, "lt"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Not => write!(f, "not"),
        }
    }
}

////////////////////////////////////////////////////////////
enum Line {
    Command(Box<dyn ASMDis>),
    Comment,
    Blank,
}

fn parse_line(i: &str) -> IResult<&str, Line> {
    alt((
        map_res(parse_command, |command| Ok::<Line, ErrorKind>(Line::Command(command))),
        map_res(parse_comment, |_| Ok::<Line, ErrorKind>(Line::Comment)),
        map_res(space0, |_| Ok::<Line, ErrorKind>(Line::Blank)),
    ))(i)
}

fn parse_command(i: &str) -> IResult<&str, Box<dyn ASMDis>> {
    // parse leading whitespace
    let (i, _) = space0(i)?;
    // parse command
    let (i, command) = alt((
        map_res(parse_pop, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))), // type will fail if r is not a Pop struct 
        map_res(parse_push, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_arithmetic_logical, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_label, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_goto, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_gotoif, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_functiondef, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_functioncall, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
        map_res(parse_return, |r| Ok::<Box<dyn ASMDis>, ErrorKind>(Box::new(r))),
    ))(i)?;
    // parse these two possiblities, fail if these patterns are not completely matched
    // pop local 1 // comment here
    // pop local 1
    alt((
        all_consuming(space0),
        all_consuming(parse_comment),
    ))(i)?;

    Ok(("", command))
}

fn parse_arithmetic_logical(i: &str) -> IResult<&str, ArithmeticLogical> {
    use ArithmeticLogical::*;
    alt((
        map_res(tag("add"), |_| Ok::<ArithmeticLogical, ErrorKind>(Add)),
        map_res(tag("sub"), |_| Ok::<ArithmeticLogical, ErrorKind>(Sub)),
        map_res(tag("neg"), |_| Ok::<ArithmeticLogical, ErrorKind>(Neg)),
        map_res(tag("eq"), |_| Ok::<ArithmeticLogical, ErrorKind>(Eq)),
        map_res(tag("gt"), |_| Ok::<ArithmeticLogical, ErrorKind>(Gt)),
        map_res(tag("lt"), |_| Ok::<ArithmeticLogical, ErrorKind>(Lt)),
        map_res(tag("and"), |_| Ok::<ArithmeticLogical, ErrorKind>(And)),
        map_res(tag("or"), |_| Ok::<ArithmeticLogical, ErrorKind>(Or)),
        map_res(tag("not"), |_| Ok::<ArithmeticLogical, ErrorKind>(Not)),
    ))(i)
}

fn parse_segment(i: &str) -> IResult<&str, Segment> {
    use Segment::*;
    alt((
        map_res(tag("argument"), |_| Ok::<Segment, ErrorKind>(Argument)),
        map_res(tag("local"), |_| Ok::<Segment, ErrorKind>(Local)),
        map_res(tag("static"), |_| Ok::<Segment, ErrorKind>(Static)),
        map_res(tag("constant"), |_| Ok::<Segment, ErrorKind>(Constant)),
        map_res(tag("this"), |_| Ok::<Segment, ErrorKind>(This)),
        map_res(tag("that"), |_| Ok::<Segment, ErrorKind>(That)),
        map_res(tag("pointer"), |_| Ok::<Segment, ErrorKind>(Pointer)),
        map_res(tag("temp"), |_| Ok::<Segment, ErrorKind>(Temp)),
    ))(i)
}

fn parse_number(i: &str) -> IResult<&str, u16> {
    map_res(digit1, |s: &str| s.parse::<u16>())(i)
}

// parse eg "pop static 1", does not parse chars before pop or chars after 1
fn parse_pop(i: &str) -> IResult<&str, Pop> {
    // parse "pop"
    let (i, _) = tag("pop")(i)?;
    // parse >0 whitespace
    let (i, _) = space1(i)?;
    // parse segment
    let (i, segment) = parse_segment(i)?;
    // parse >0 whitespace
    let (i, _) = space1(i)?;
    // parse idx
    let (i, idx) = parse_number(i)?;

    Ok((i, Pop {segment, idx}))
}

// parse eg "push static 1", does not parse chars before push or chars after 1
fn parse_push(i: &str) -> IResult<&str, Push> {
    // parse "push"
    let (i, _) = tag("push")(i)?;
    // parse >0 whitespace
    let (i, _) = space1(i)?;
    // parse segment
    let (i, segment) = parse_segment(i)?;
    // parse >0 whitespace
    let (i, _) = space1(i)?;
    // parse idx
    let (i, idx) = parse_number(i)?;

    Ok((i, Push {segment, idx}))
}


fn  parse_comment(i: &str) -> IResult<&str, &str> {
    let _ =
        tuple((
            space0,
            tag("//"),
        ))
    (i)?;
    // dont care about contents of comment after the //
    Ok(("", ""))
}

fn parse_name(s: &str) -> IResult<&str, String> {
    let (i, v) = alt((
        map_res(many_till(anychar, char1(' ')), |r| Ok::<Vec<char>, ErrorKind>(r.0)),
        many1(anychar),
    ))(s)?;
    let s = v.into_iter().collect();
    Ok((i, s))
}

fn parse_label(i: &str) -> IResult<&str, Label> {
    // parse "label"
    let (i, _) = tag("label")(i)?;
    //parse >0 whitespace   
    let (i, _) = space1(i)?;
    // parse label name
    let (i, name) = parse_name(i)?;
    Ok((i, Label {name}))
}

fn parse_goto(i: &str) -> IResult<&str, GoTo> {
    // parse "goto"
    let (i, _) = tag("goto")(i)?;
    //parse >0 whitespace   
    let (i, _) = space1(i)?;
    // parse label name
    let (i, name) = parse_name(i)?;
    Ok((i, GoTo {name}))
}

fn parse_gotoif(i: &str) -> IResult<&str, GoToIf> {
    // parse "if-goto"
    let (i, _) = tag("if-goto")(i)?;
    //parse >0 whitespace   
    let (i, _) = space1(i)?;
    // parse label name
    let (i, name) = parse_name(i)?;
    Ok((i, GoToIf {name}))
}

fn parse_functiondef(i: &str) -> IResult<&str, FunctionDef> {
    // parse "function"
    let (i, _) = tag("function")(i)?;
    //parse >0 whitespace   
    let (i, _) = space1(i)?;
    // parse function name
    let (i, name) = parse_name(i)?;
    // parse >=0 whitespace
    let (i, _) = space0(i)?;
    // parse num_locals
    let (i, n_locals) = parse_number(i)?;
    Ok((i, FunctionDef {name, n_locals}))
}

fn parse_functioncall(i: &str) -> IResult<&str, FunctionCall> {
    // parse "call"
    let (i, _) = tag("call")(i)?;
    //parse >0 whitespace   
    let (i, _) = space1(i)?;
    // parse function name
    let (i, name) = parse_name(i)?;
    // parse >=0 whitespace
    let (i, _) = space0(i)?;
    // parse n_args
    let (i, n_args) = parse_number(i)?;
    Ok((i, FunctionCall {name, n_args}))
}

fn parse_return(i: &str) -> IResult<&str, Return> {
    // parse "return"
    let (i, _) = tag("return")(i)?;
    Ok((i, Return {}))
}

////////////////////////////////////////////////////////////

fn bootstrap() -> String {
    let mut out = 
"// BOOTSTRAP CODE
// SP = 256
@256
D = A
@SP
M = D
".to_string();
    let mut scope_info = ScopeInfo::new(""); // no file_stem context 
    let call_sys_init = FunctionCall {name: "Sys.init".to_owned(), n_args: 0};
    let call_sys_init_asm = call_sys_init.to_asm(&mut scope_info);

    out.push_str(&call_sys_init_asm);
    out
}

fn translate_file(path: &Path) -> Option<String> {
    let file_stem = path.file_stem()?.to_str()?;
    let file = fs::read_to_string(path).ok()?;

    let mut scope_info = ScopeInfo::new(file_stem);
    let mut out = String::new();

    for line in file.lines() {
        let (_, parsed_line) = parse_line(line).unwrap();
        if let Line::Command(command) = parsed_line {
            out.push_str("\n\n// COMMAND \n");

            out.push_str("// ");
            out.push_str(line);
            out.push_str("\n");

            out.push_str(&command.to_asm(&mut scope_info));
        }
    }

    Some(out)
}

fn main() -> Result<(), Box<dyn std::error::Error>>{
    let mut args = std::env::args();
    let in_path = args.nth(1).unwrap();
    let in_path = Path::new(&in_path);

    // add bootstrap to top of output file
    let mut out = bootstrap();

    if in_path.is_dir() {
        println!("translating directory");

        // for each file in directory, translate file to asm
        for file in in_path.read_dir()? {

            println!("{:?}", file);
            let file_path = file?.path();
            let translated_file = translate_file(&file_path)
                .ok_or(format!("error translating file {:?}", file_path))?;

            out.push_str(&translated_file);
        }
        let dir_name = in_path.components().last()
            .ok_or("Error getting dir name component")?.as_os_str();
        let out_path = in_path.join(dir_name).with_extension("asm");

        println!("wrote to {}", out_path.to_str().unwrap());
        fs::write(out_path, out).unwrap();
    } else {
        // translate single file
        println!("translating file");
        out.push_str(&translate_file(in_path).ok_or("error translating file")?);

        let out_path = in_path.with_extension("asm");
        println!("wrote to {}", out_path.to_str().unwrap());
        fs::write(out_path, out).unwrap();
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn scope_info_generic() {
        let mut si = ScopeInfo::new("yoo");
        si.enter_function("Sys.init");

        println!("{}", si.build_return_label());
        println!("{}", si.build_return_label());
        println!("{}", si.build_internal_jump_label("LT"));
        println!("{}", si.build_internal_jump_label("LT"));
        println!("{}", si.build_private_jump_label());
        println!("{}", si.build_private_jump_label());
        println!("{}", si.build_function_def_label());
    }

    #[test]
    fn bootstrap_generic() {
        let bs = bootstrap();
        println!("{}", bs);
    }
 
}
