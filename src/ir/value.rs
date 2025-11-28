// IR 值和寄存器
use crate::ir::Type;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Const(Constant),
    Reg(VirtualReg),
    Param(usize),
    Global(String),
}

impl Value {
    pub fn is_const(&self) -> bool {
        matches!(self, Value::Const(_))
    }

    pub fn as_const(&self) -> Option<&Constant> {
        match self {
            Value::Const(c) => Some(c),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Const(c) => write!(f, "{}", c),
            Value::Reg(r) => write!(f, "%{}", r.id),
            Value::Param(i) => write!(f, "%arg{}", i),
            Value::Global(name) => write!(f, "@{}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Int(i64),
    Void,
    Array(Vec<Constant>),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Int(n) => write!(f, "{}", n),
            Constant::Void => write!(f, "void"),
            Constant::Array(arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VirtualReg {
    pub id: usize,
    pub ty: Type,
}

impl VirtualReg {
    pub fn new(id: usize, ty: Type) -> Self {
        VirtualReg { id, ty }
    }
}

