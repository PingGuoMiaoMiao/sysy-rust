// IR 类型系统
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Int,
    Bool,
    Ptr(Box<Type>),
    Array {
        elem_ty: Box<Type>,
        size: usize,
    },
    Function {
        return_ty: Box<Type>,
        param_tys: Vec<Type>,
    },
}

impl Type {
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    pub fn ptr_to(inner: Type) -> Self {
        Type::Ptr(Box::new(inner))
    }

    pub fn array(elem_ty: Type, size: usize) -> Self {
        Type::Array {
            elem_ty: Box::new(elem_ty),
            size,
        }
    }

    pub fn deref(&self) -> Option<&Type> {
        match self {
            Type::Ptr(inner) => Some(inner),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Int => write!(f, "i32"),
            Type::Bool => write!(f, "i1"),
            Type::Ptr(inner) => write!(f, "{}*", inner),
            Type::Array { elem_ty, size } => write!(f, "[{} x {}]", size, elem_ty),
            Type::Function { return_ty, param_tys } => {
                write!(f, "{} (", return_ty)?;
                for (i, ty) in param_tys.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
        }
    }
}

