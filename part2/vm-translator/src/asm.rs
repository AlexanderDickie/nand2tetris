use super::*;
impl ASM for Push {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        use Segment::*;
        return match self.segment {
            Constant => {
                let value = self.idx;
                format!(
"// D = value
@{value}
D = A
//*SP = D
@SP
A = M
M = D
//SP += 1
@SP
M = M + 1")
            },
            Temp => {
                let idx = (5 + self.idx).to_string();
                format!(
"// D = *(segstart + idx)
@{idx}
D = M
// *SP = D
@SP
A = M
M = D
// *SP += 1
@SP
M = M + 1")
            },

            Static => {
                let static_variable_label = scope_info.build_static_variable_label(self.idx);
                format!(
"// D = *(STATIC.idx)
@{static_variable_label}
D = M
// *SP = D
@SP
A = M
M = D
// *SP += 1
@SP
M = M + 1")
            },
                Pointer => {
                let idx = 3 + self.idx;
                format!(
"// D = *(STATIC.idx)
@{idx}
D = M
// *SP = D
@SP
A = M
M = D
// increment stack pointer
@SP
M = M + 1")
            }

            _ => {
                let segment = self.segment.to_string();
                let idx = self.idx.to_string();
                format!(
"//D = segstart + idx
@{segment}
D = M
@{idx}
D = D + A
// D = *(segstart + idx)
A = D
D = M
// *SP = D
@SP
A = M
M = D
// increment stack pointer
@SP
M = M + 1")
            },
        };
    }
}

impl ASM for Pop {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        match self.segment {
            Segment::Temp => {
                let idx = (5 + self.idx).to_string();
                return format!(
"// D = segstart + idx
@{idx}
D = A
// *SP = D
@SP
A = M
M = D
// D = *(SP-1) 
A = A - 1
// *(segstart+idx) = D
D = M
A = A + 1
A = M
M = D
// SP -= 1
@SP
M = M - 1");
            },
                Segment::Static => {
                let static_variable_label = scope_info.build_static_variable_label(self.idx);
                format!(
"// D = STATIC.idx
@{static_variable_label}
D = A
// *SP = D
@SP
A = M
M = D
// D = *(SP-1) 
A = A - 1
// *(segstart+idx) = D
D = M
A = A + 1
A = M
M = D
// SP -= 1
@SP
M = M - 1")
            },
                Segment::Pointer =>{
                let idx = 3 + self.idx;
                format!(
"// D = idx
@{idx}
D = A
// *SP = D
@SP
A = M
M = D
// D = *(SP-1) 
A = A - 1
// *(segstart+idx) = D
D = M
A = A + 1
A = M
M = D
// SP -= 1
@SP
M = M - 1")
            }

            _ => {
                let segment = self.segment.to_string();
                let idx = self.idx.to_string();
                format!(
                "// D = segstart + idx
@{segment}
D = M
@{idx}
D = D + A
// *SP = D
@SP
A = M
M = D
// D = *(SP-1) 
A = A - 1
// *(segstart+idx) = D
D = M
A = A + 1
A = M
M = D
// SP -= 1
@SP
M = M - 1")
            }
        }
    }
}

impl ASM for ArithmeticLogical  {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        use ArithmeticLogical::*;
        match self {
            Add =>
                "// D = *(SP-1)
@SP
A = M
A = A - 1
D = M
//D = D + *(SP-2)
A = A - 1
D = D + M
// *(SP-2) = D
M = D
// SP -= 1
@SP
M = M - 1"
                    .to_string(),
            Sub => 
                "// D = *(SP-1)
@SP
A = M-1
D = M
//D = -D + (*SP-2)
A = A - 1
D = M - D
// *(SP-2) = D
M = D
// SP -= 1
@SP
M = M - 1"
                    .to_string(),
            Neg => 
                "@SP
A = M - 1
M = -M".to_string(),


            Eq => {
                let label_eq = scope_info.build_private_jump_label();
                let label_end = scope_info.build_private_jump_label();
                format!(
                "//D = *(SP-1) - *(SP-2)
@SP
A = M - 1
D = M
A = A - 1
D = D - M
//if D == 0, goto label_eq
@{label_eq}
D;JEQ
// D != 0
D = 0
@{label_end}
0;JMP
({label_eq})
D = -1
({label_end})
// *(SP-2) = D
@SP
A = M - 1
A = A - 1
M = D
// SP -= 1
@SP
M = M - 1")
            }
            Lt => {
                let label_lt = scope_info.build_private_jump_label();
                let label_end = scope_info.build_private_jump_label();
                format!(
                "//D = *(SP-1) - *(SP-2)
@SP
A = M - 1
D = M
A = A - 1
D = D - M
//if D > 0, goto label_lt 
@{label_lt}
D;JGT
// D <= 0
D = 0
@{label_end}
0;JMP
({label_lt})
D = -1
({label_end})
// *(SP-2) = D
@SP
A = M - 1
A = A - 1
M = D
// SP -= 1
@SP
M = M - 1")
            }
            Gt => {
                let label_gt = scope_info.build_private_jump_label();
                let label_end = scope_info.build_private_jump_label();
                format!(
                "//D = *(SP-1) - *(SP-2)
@SP
A = M - 1
D = M
A = A - 1
D = D - M
//if D < 0, goto label_gt 
@{label_gt}
D;JLT
// D <= 0
D = 0
@{label_end}
0;JMP
({label_gt})
D = -1
({label_end})
// *(SP-2) = D
@SP
A = M - 1
A = A - 1
M = D
// SP -= 1
@SP
M = M - 1")
            },
            And => format!(
            "// D = *(SP-1)
@SP
A = M - 1
D = M
// D = *(SP-2) & D
A = A - 1
D = M & D
// *(SP-2) = D
M = D
// SP -= 1
@SP
M = M - 1"),
            Or => format!(
            "// D = *(SP-1)
@SP
A = M - 1
D = M
// D = *(SP-2) | D
A = A - 1
D = M | D
// *(SP-2) = D
M = D
// SP -= 1
@SP
M = M - 1"),
            Not => format!(
            "// *(SP-1) = -1 - *(SP-1)
@1
D = -A
@SP
A = M - 1
M = D - M"),

        }
    }
}

impl ASM for Label {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        let label = scope_info.build_internal_jump_label(&self.name);
        format!(
"({label})"
    )
    }
}

impl ASM for GoTo {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        let label = scope_info.build_internal_jump_label(&self.name);
        format!(
"@{label}
0;JMP"
    )
    }
}

impl ASM for GoToIf {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        // if stop of stack != 0, goto label
        let label = scope_info.build_internal_jump_label(&self.name);
        format!(
"// D = *(SP-1)
@SP
A = M - 1
D = M
// SP -= 1
@SP
M = M - 1
// if D != 0 goto label
@{label}
D; JNE"
    )
    }
}

impl ASM for FunctionDef {
    /*
    function f nvars
    push 0 onto the stack nvar times
    */
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        /*
        we enter a new function here, so update the scope_info
        in order for us to generate relevant return labels, and 
        jump labels when translating to asm
        */
        scope_info.enter_function(&self.name);
        let function_def_label = scope_info.build_function_def_label();
        let mut out = String::new();

        out.push_str(&format!(
"// label (file_stem.functionname)
({})"
            , function_def_label));

        out.push_str(&
"// *SP = 0
@0
D = A
@SP
A = M
M = D
// SP += 1
@SP
M = M + 1
".repeat(self.n_locals as usize));
        out
    }
}

fn push_value(val: &str) -> String {
    /*
    push val onto the stack
    */
    format!(
"// D = val
@{val}
D = A
// *SP = D
@SP
A = M
M = D
// SP += 1
@SP
M = M + 1")
}

fn push_get_value(addr: &str) -> String {
    /* 
    push *addr on the stack
    */
    format!(
"// D = *val
@{addr}
A = M
D = A
// *SP = D
@SP
A = M
M = D
// SP += 1
@SP
M = M + 1")
}
impl ASM for FunctionCall {
    fn to_asm(&self, scope_info: &mut ScopeInfo ) -> String {
        let return_label = scope_info.build_return_label();
        let mut out = String::new();
        // a function always returns a value onto the stack, except when caller is our bootstrap code
        // if we have no arguments, we need to push a dummy argument on as a placeholder for our return value
        if self.n_args == 0 && self.name != "Sys.init" {
        out.push_str(&push_value("0"));
        }
        out.push_str(&vec![
            push_value(&return_label),
            push_get_value("LCL"),
            push_get_value("ARG"),
            push_get_value("THIS"),
            push_get_value("THAT"),
        ].join("\n"));

        // ARG = SP - (5 + n_args)
        // if we have no arguments, we still pushed a dummy argument on, so make room
        // except if the caller was our bootstrap code
        let n_args = if self.name == "Sys.init" {0} else {u16::max(1, self.n_args)};
        let dif = 5 + n_args;
        out.push_str(
&format!(
"
// D = SP
@SP
D = M
// D = SP - dif
@{dif}
D = D - A
// ARG = SP - dif
@ARG
M = D "));

        // LCL = SP
        out.push_str(
"
// LCL = SP
@SP
D = M
@LCL
M = D");

        // goto function
        let calle_function_name = &self.name;
        out.push_str(
&format!(
"
@{calle_function_name}
0; JMP")

        );
        // put return label here
        out.push_str(
&format!(
"
({return_label})")
        );

        out
    }
}

impl ASM for Return {
    fn to_asm(&self, _: &mut ScopeInfo ) -> String {
        /*
        restore the caller's arg, lcl, this, that, push our top
        of stack onto caller's top of stack, jump to caller's
        return label
        */
"// *ARG = *(SP-1)
@SP
A = M - 1
D = M
@ARG
A = M
M = D

// SP = ARG + 1
@ARG
D = M + 1
@SP
M = D

// R13 = LCL - 1
@LCL
D = M - 1
@R13
M = D

// THAT = *(R13) (old THAT)
@R13
A = M
D = M
@THAT
M = D

// R13 -= 1
@R13
M = M - 1

// THIS = *(R13) (old THIS)
@R13
A = M
D = M
@THIS
M = D

// R13 -= 1
@R13
M = M - 1

// ARG = *(R13) (old ARG)
@R13
A = M
D = M
@ARG
M = D

// R13 -= 1
@R13
M = M - 1

// LCL = *(R13) (old LCL)
@R13
A = M
D = M
@LCL
M = D

// R13 -= 1
@R13
M = M - 1

// jump to retaddr (*R13)
@R13
A = M
A = M 
// set A = *(R13), then 0;JMP jumps to A which is return addr
0; JMP".to_string()
    }
}
