// IR 模块
pub mod types;
pub mod inst;
pub mod value;
pub mod display;

pub use types::*;
pub use inst::*;
pub use value::*;

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub global_vars: Vec<GlobalVar>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            functions: Vec::new(),
            global_vars: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub name: String,
    pub ty: Type,
    pub is_const: bool,
    pub init: Option<Constant>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub basic_blocks: Vec<BasicBlock>,
    pub local_vars: Vec<LocalVar>,
}

impl Function {
    pub fn new(name: String, params: Vec<Parameter>, return_type: Type) -> Self {
        Function {
            name,
            params,
            return_type,
            basic_blocks: Vec::new(),
            local_vars: Vec::new(),
        }
    }

    pub fn entry_block(&self) -> Option<&BasicBlock> {
        self.basic_blocks.first()
    }

    pub fn entry_block_mut(&mut self) -> Option<&mut BasicBlock> {
        self.basic_blocks.first_mut()
    }

    pub fn find_block(&self, label: &str) -> Option<&BasicBlock> {
        self.basic_blocks.iter().find(|bb| bb.label == label)
    }

    pub fn find_block_mut(&mut self, label: &str) -> Option<&mut BasicBlock> {
        self.basic_blocks.iter_mut().find(|bb| bb.label == label)
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct LocalVar {
    pub name: String,
    pub ty: Type,
    pub alloca_id: usize,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    pub predecessors: Vec<String>,
    pub successors: Vec<String>,
    pub phi_nodes: Vec<PhiNode>,
}

impl BasicBlock {
    pub fn new(label: String) -> Self {
        BasicBlock {
            label,
            instructions: Vec::new(),
            terminator: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
            phi_nodes: Vec::new(),
        }
    }

    pub fn add_predecessor(&mut self, pred: String) {
        if !self.predecessors.contains(&pred) {
            self.predecessors.push(pred);
        }
    }

    pub fn add_successor(&mut self, succ: String) {
        if !self.successors.contains(&succ) {
            self.successors.push(succ);
        }
    }
}

#[derive(Debug, Clone)]
pub struct PhiNode {
    pub result: VirtualReg,
    pub incoming: Vec<(Value, String)>,
}

