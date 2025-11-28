// SSA 构造
use crate::ir::*;
use std::collections::{HashMap, HashSet, VecDeque};
pub fn construct_ssa(module: &mut Module) -> Result<(), String> {
    for func in &mut module.functions {
        construct_ssa_for_function(func)?;
    }
    Ok(())
}

fn construct_ssa_for_function(func: &mut Function) -> Result<(), String> {
    if func.basic_blocks.is_empty() {
        return Ok(());
    }

    // 1. 构建 CFG
    build_cfg(func);
    
    // 2. 计算支配树
    let dom_tree = compute_dominance_tree(func);
    
    // 3. 计算支配边界（Dominance Frontier）
    let df = compute_dominance_frontier(func, &dom_tree);
    
    // 4. 插入 Phi 节点
    insert_phi_nodes(func, &df)?;
    
    // 5. 重命名变量（Variable Renaming）
    rename_variables(func, &dom_tree)?;
    
    Ok(())
}

// CFG 构建

fn build_cfg(func: &mut Function) {
    // 清空现有的 CFG 信息
    for bb in &mut func.basic_blocks {
        bb.predecessors.clear();
        bb.successors.clear();
    }
    
    // 根据终结指令构建 CFG
    for i in 0..func.basic_blocks.len() {
        let label = func.basic_blocks[i].label.clone();
        let successors = get_successors(&func.basic_blocks[i]);
        
        for succ_label in &successors {
            // 添加后继
            func.basic_blocks[i].successors.push(succ_label.clone());
            
            // 在后继块中添加前驱
            if let Some(succ_bb) = func.find_block_mut(succ_label) {
                succ_bb.add_predecessor(label.clone());
            }
        }
    }
}

fn get_successors(bb: &BasicBlock) -> Vec<String> {
    match &bb.terminator {
        Some(Terminator::Jump(label)) => vec![label.clone()],
        Some(Terminator::Branch { true_label, false_label, .. }) => {
            vec![true_label.clone(), false_label.clone()]
        }
        Some(Terminator::Return(_)) | Some(Terminator::Unreachable) | None => vec![],
    }
}

// 支配树
type DominanceTree = HashMap<String, String>;

fn compute_dominance_tree(func: &Function) -> DominanceTree {
    let mut dom_tree = HashMap::new();
    
    if func.basic_blocks.is_empty() {
        return dom_tree;
    }
    
    let entry_label = &func.basic_blocks[0].label;
    
    // 初始化：所有块的支配者集合
    let mut doms: HashMap<String, HashSet<String>> = HashMap::new();
    
    // 入口块只被自己支配
    let mut entry_set = HashSet::new();
    entry_set.insert(entry_label.clone());
    doms.insert(entry_label.clone(), entry_set);
    
    // 其他块初始为所有块
    let all_blocks: HashSet<String> = func.basic_blocks.iter()
        .map(|bb| bb.label.clone())
        .collect();
    
    for bb in &func.basic_blocks {
        if bb.label != *entry_label {
            doms.insert(bb.label.clone(), all_blocks.clone());
        }
    }
    
    // 迭代计算支配关系
    let mut changed = true;
    while changed {
        changed = false;
        
        for bb in &func.basic_blocks {
            if bb.label == *entry_label {
                continue;
            }
            
            // Dom(b) = {b} ∪ (∩ Dom(p) for all predecessors p)
            let mut new_dom = HashSet::new();
            new_dom.insert(bb.label.clone());
            
            if !bb.predecessors.is_empty() {
                // 计算所有前驱的支配者交集
                let mut intersection = doms.get(&bb.predecessors[0])
                    .cloned()
                    .unwrap_or_default();
                
                for pred_label in &bb.predecessors[1..] {
                    if let Some(pred_dom) = doms.get(pred_label) {
                        intersection = intersection.intersection(pred_dom)
                            .cloned()
                            .collect();
                    }
                }
                
                new_dom.extend(intersection);
            }
            
            if new_dom != *doms.get(&bb.label).unwrap() {
                doms.insert(bb.label.clone(), new_dom);
                changed = true;
            }
        }
    }
    
    // 从支配关系计算直接支配者（idom）
    for bb in &func.basic_blocks {
        if bb.label == *entry_label {
            continue;
        }
        
        let mut dominators: Vec<String> = doms.get(&bb.label)
            .unwrap()
            .iter()
            .filter(|d| **d != bb.label)
            .cloned()
            .collect();
        
        // 找到最近的支配者（直接支配者）
        if !dominators.is_empty() {
            // 简化：选择距离最近的前驱作为直接支配者
            // 正确的实现应该找到唯一的 idom
            dominators.sort_by_key(|d| {
                // 计算从 entry 到 d 的距离
                doms.get(d).map(|s| s.len()).unwrap_or(0)
            });
            dominators.reverse();
            
            dom_tree.insert(bb.label.clone(), dominators[0].clone());
        }
    }
    
    dom_tree
}

// 支配边界
type DominanceFrontier = HashMap<String, HashSet<String>>;

fn compute_dominance_frontier(func: &Function, dom_tree: &DominanceTree) -> DominanceFrontier {
    let mut df: DominanceFrontier = HashMap::new();
    
    // 初始化
    for bb in &func.basic_blocks {
        df.insert(bb.label.clone(), HashSet::new());
    }
    
    // 计算支配关系（传递闭包）
    let dominates = compute_dominates(func, dom_tree);
    
    // 对每条边 (x, y)
    for bb in &func.basic_blocks {
        let x = &bb.label;
        
        for y in &bb.successors {
            // 找到所有支配 x 但不严格支配 y 的块
            for b in func.basic_blocks.iter().map(|bb| &bb.label) {
                let b_dominates_x = dominates.get(b)
                    .map(|set| set.contains(x))
                    .unwrap_or(false);
                
                let b_strictly_dominates_y = b != y && dominates.get(b)
                    .map(|set| set.contains(y))
                    .unwrap_or(false);
                
                if b_dominates_x && !b_strictly_dominates_y {
                    df.get_mut(b).unwrap().insert(y.clone());
                }
            }
        }
    }
    
    df
}

/// 计算支配关系的传递闭包
fn compute_dominates(func: &Function, dom_tree: &DominanceTree) -> HashMap<String, HashSet<String>> {
    let mut dominates: HashMap<String, HashSet<String>> = HashMap::new();
    
    // 初始化：每个块支配自己
    for bb in &func.basic_blocks {
        let mut set = HashSet::new();
        set.insert(bb.label.clone());
        dominates.insert(bb.label.clone(), set);
    }
    
    // 通过支配树传递计算
    for bb in &func.basic_blocks {
        let mut current = bb.label.clone();
        let start = bb.label.clone();
        
        // 向上遍历支配树
        while let Some(idom) = dom_tree.get(&current) {
            dominates.get_mut(idom).unwrap().insert(start.clone());
            current = idom.clone();
        }
    }
    
    dominates
}

// Phi 节点插入

fn insert_phi_nodes(func: &mut Function, df: &DominanceFrontier) -> Result<(), String> {
    // 收集所有被赋值的变量
    let mut defined_vars: HashSet<usize> = HashSet::new();  // 虚拟寄存器 ID
    let mut def_sites: HashMap<usize, HashSet<String>> = HashMap::new();  // 变量 -> 定义它的基本块
    
    for bb in &func.basic_blocks {
        for inst in &bb.instructions {
            if let Some(def_reg) = get_defined_reg(inst) {
                defined_vars.insert(def_reg.id);
                def_sites.entry(def_reg.id)
                    .or_insert_with(HashSet::new)
                    .insert(bb.label.clone());
            }
        }
    }
    
    // 为每个变量插入 Phi 节点
    for var_id in defined_vars {
        let mut work_list: VecDeque<String> = VecDeque::new();
        let mut phi_inserted: HashSet<String> = HashSet::new();
        
        // 初始化工作列表为所有定义该变量的块
        if let Some(def_blocks) = def_sites.get(&var_id) {
            for block in def_blocks {
                work_list.push_back(block.clone());
            }
        }
        
        // 迭代插入 Phi 节点
        while let Some(block) = work_list.pop_front() {
            // 对该块的每个支配边界
            if let Some(frontier) = df.get(&block) {
                for df_block in frontier {
                    if phi_inserted.contains(df_block) {
                        continue;
                    }
                    
                    // 在 df_block 中插入 Phi 节点
                    let bb = func.find_block_mut(df_block)
                        .ok_or_else(|| format!("Block {} not found", df_block))?;
                    
                    // 创建 Phi 节点
                    let phi_result = VirtualReg::new(var_id, Type::Int);  // TODO: 正确的类型
                    let incoming = Vec::new();  // 稍后在重命名阶段填充
                    
                    bb.phi_nodes.push(PhiNode {
                        result: phi_result,
                        incoming,
                    });
                    
                    phi_inserted.insert(df_block.clone());
                    
                    // 如果这是第一次在 df_block 中定义该变量，加入工作列表
                    if !def_sites.get(&var_id).unwrap().contains(df_block) {
                        work_list.push_back(df_block.clone());
                        def_sites.get_mut(&var_id).unwrap().insert(df_block.clone());
                    }
                }
            }
        }
    }
    
    Ok(())
}

fn get_defined_reg(inst: &Instruction) -> Option<&VirtualReg> {
    match inst {
        Instruction::Binary { result, .. } => Some(result),
        Instruction::Unary { result, .. } => Some(result),
        Instruction::Call { result, .. } => result.as_ref(),
        Instruction::Alloca { result, .. } => Some(result),
        Instruction::Load { result, .. } => Some(result),
        Instruction::GetElementPtr { result, .. } => Some(result),
        Instruction::Cast { result, .. } => Some(result),
        Instruction::Store { .. } => None,
    }
}

// 变量重命名

fn rename_variables(func: &mut Function, dom_tree: &DominanceTree) -> Result<(), String> {
    // 变量重命名栈：变量 ID -> 版本号栈
    let mut stacks: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut counter: HashMap<usize, usize> = HashMap::new();
    
    if func.basic_blocks.is_empty() {
        return Ok(());
    }
    
    let entry_label = func.basic_blocks[0].label.clone();
    rename_block(func, &entry_label, dom_tree, &mut stacks, &mut counter)?;
    
    Ok(())
}

fn rename_block(
    func: &mut Function,
    block_label: &str,
    dom_tree: &DominanceTree,
    stacks: &mut HashMap<usize, Vec<usize>>,
    counter: &mut HashMap<usize, usize>,
) -> Result<(), String> {
    // 1. 处理 Phi 节点
    // TODO: 为 Phi 节点的结果创建新版本
    
    // 2. 处理指令
    // TODO: 重写使用和定义
    
    // 3. 填充后继块中 Phi 节点的参数
    // TODO: 根据当前块作为前驱的信息填充 Phi 节点
    
    // 4. 递归处理支配树中的子节点
    let children: Vec<String> = dom_tree.iter()
        .filter(|(_, idom)| *idom == block_label)
        .map(|(child, _)| child.clone())
        .collect();
    
    for child in children {
        rename_block(func, &child, dom_tree, stacks, counter)?;
    }
    
    // 5. 恢复栈（弹出在此块中压入的版本）
    // TODO: 实现栈恢复
    
    Ok(())
}

// 调试用

#[allow(dead_code)]
fn print_cfg(func: &Function) {
    println!("=== CFG for {} ===", func.name);
    for bb in &func.basic_blocks {
        println!("{}:", bb.label);
        println!("  Predecessors: {:?}", bb.predecessors);
        println!("  Successors: {:?}", bb.successors);
    }
}

#[allow(dead_code)]
fn print_dom_tree(func: &Function, dom_tree: &DominanceTree) {
    println!("=== Dominance Tree for {} ===", func.name);
    for (block, idom) in dom_tree {
        println!("{} idom: {}", block, idom);
    }
}

#[allow(dead_code)]
fn print_df(df: &DominanceFrontier) {
    println!("=== Dominance Frontier ===");
    for (block, frontier) in df {
        println!("DF({}): {:?}", block, frontier);
    }
}

