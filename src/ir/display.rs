// IR 格式化输出
use crate::ir::*;
use std::fmt;

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for global in &self.global_vars {
            writeln!(f, "{}", global)?;
        }
        
        if !self.global_vars.is_empty() {
            writeln!(f)?;
        }
        
        for (i, func) in self.functions.iter().enumerate() {
            if i > 0 { writeln!(f)?; }
            writeln!(f, "{}", func)?;
        }
        
        Ok(())
    }
}

impl fmt::Display for GlobalVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@{} = {} {}", 
            self.name,
            if self.is_const { "constant" } else { "global" },
            self.ty
        )?;
        if let Some(init) = &self.init {
            write!(f, " {}", init)?;
        }
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "define {} @{}(", self.return_type, self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{} %{}", param.ty, param.name)?;
        }
        writeln!(f, ") {{")?;
        
        for bb in &self.basic_blocks {
            writeln!(f, "{}:", bb.label)?;
            
            for phi in &bb.phi_nodes {
                write!(f, "  %{} = phi {} ", phi.result.id, phi.result.ty)?;
                for (i, (val, label)) in phi.incoming.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "[{}, %{}]", val, label)?;
                }
                writeln!(f)?;
            }
            
            for inst in &bb.instructions {
                writeln!(f, "  {}", inst)?;
            }
            
            if let Some(term) = &bb.terminator {
                writeln!(f, "  {}", term)?;
            }
        }
        
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Binary { result, op, left, right } => {
                write!(f, "%{} = {} {} {}, {}", result.id, op, result.ty, left, right)
            }
            Instruction::Unary { result, op, operand } => {
                write!(f, "%{} = {} {} {}", result.id, op, result.ty, operand)
            }
            Instruction::Call { result, func, args } => {
                if let Some(res) = result {
                    write!(f, "%{} = ", res.id)?;
                }
                write!(f, "call @{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Instruction::Alloca { result, ty } => {
                write!(f, "%{} = alloca {}", result.id, ty)
            }
            Instruction::Load { result, ptr } => {
                write!(f, "%{} = load {}, {}* {}", result.id, result.ty, result.ty, ptr)
            }
            Instruction::Store { value, ptr } => {
                write!(f, "store {}, {}", value, ptr)
            }
            Instruction::GetElementPtr { result, base, indices } => {
                write!(f, "%{} = getelementptr {}", result.id, base)?;
                for idx in indices {
                    write!(f, ", {}", idx)?;
                }
                Ok(())
            }
            Instruction::Cast { result, value, to_ty } => {
                write!(f, "%{} = cast {} to {}", result.id, value, to_ty)
            }
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminator::Return(Some(val)) => write!(f, "ret {}", val),
            Terminator::Return(None) => write!(f, "ret void"),
            Terminator::Jump(label) => write!(f, "br label %{}", label),
            Terminator::Branch { cond, true_label, false_label } => {
                write!(f, "br i1 {}, label %{}, label %{}", cond, true_label, false_label)
            }
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

