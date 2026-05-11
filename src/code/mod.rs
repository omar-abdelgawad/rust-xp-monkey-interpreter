use std::{fmt::Display, ops::Deref};

// The fields will be laid out in big endian
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Instructions(pub Vec<u8>);

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        while i < self.len() {
            let def = lookup(self[i]);
            // TODO: the fact that Instructions type ownes its data kind of makes it hard to write
            // a good api for the functions without cloning everything or changing the type
            let (operands, read) = read_operands(&def, &self[i + 1..]);
            writeln!(f, "{:04} {}", i, self.fmt_instruction(&def, &operands))?;
            i += 1 + read as usize;
        }
        Ok(())
    }
}

impl Deref for Instructions {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Instructions {
    pub fn new(instructions: Vec<u8>) -> Self {
        Self(instructions)
    }
    pub fn fmt_instruction(&self, def: &Definition, operands: &[i64]) -> String {
        let operand_count = def.operand_widths.len();
        if operands.len() != operand_count {
            return format!(
                "ERROR: operand len {} doesn't match defined {}\n",
                operands.len(),
                operand_count
            );
        }
        match operand_count {
            0 => def.name.to_string(),
            1 => format!("{} {}", def.name, operands[0]),
            2 => format!("{} {} {}", def.name, operands[0], operands[1]),
            _ => format!("ERROR: unhandled operand_count for {\n}", def.name),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Opcode {
    Constant = 0x00,
    Add = 0x01,
    Pop = 0x02,
    Sub = 0x03,
    Mul = 0x04,
    Div = 0x05,
    True = 0x06,
    False = 0x07,
    Equal = 0x08,
    NotEqual = 0x09,
    GreaterThan = 0x0a,
    Minus = 0x0b,
    Bang = 0x0c,
    JumpNotTruthy = 0x0d,
    Jump = 0x0e,
    Null = 0x0f,
    GetGlobal = 0x10,
    SetGlobal = 0x11,
    Array = 0x12,
    Hash = 0x13,
    Index = 0x14,
    Call = 0x15,
    ReturnValue = 0x16,
    Return = 0x17,
    GetLocal = 0x18,
    SetLocal = 0x19,
    GetBuiltin = 0x1a,
    Closure = 0x1b,
    GetFree = 0x1c,
    CurrentClosure = 0x1d,
}
use Opcode as Op;

impl TryFrom<u8> for Op {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        // NOTE: don't forget to add any new opcode here as it won't give a compiler error
        Ok(unsafe { std::mem::transmute::<u8, Op>(value) })

        // TODO: I am trying to optimize try_from but it might be best to just use unsafe here
        //const OP_CONSTANT: u8 = Op::Constant as u8;
        //const OP_ADD: u8 = Op::Add as u8;
        //const OP_POP: u8 = Op::Pop as u8;
        //const OP_SUB: u8 = Op::Sub as u8;
        //const OP_MUL: u8 = Op::Mul as u8;
        //const OP_DIV: u8 = Op::Div as u8;
        //const OP_TRUE: u8 = Op::True as u8;
        //const OP_FALSE: u8 = Op::False as u8;
        //const OP_EQUAL: u8 = Op::Equal as u8;
        //const OP_NOT_EQUAL: u8 = Op::NotEqual as u8;
        //const OP_GREATER_THAN: u8 = Op::GreaterThan as u8;
        //const OP_MINUS: u8 = Op::Minus as u8;
        //const OP_BANG: u8 = Op::Bang as u8;
        //const OP_JUMP_NOT_TRUTHY: u8 = Op::JumpNotTruthy as u8;
        //const OP_JUMP: u8 = Op::Jump as u8;
        //const OP_NULL: u8 = Op::Null as u8;
        //const OP_GET_GLOBAL: u8 = Op::GetGlobal as u8;
        //const OP_SET_GLOBAL: u8 = Op::SetGlobal as u8;
        //const OP_ARRAY: u8 = Op::Array as u8;
        //const OP_HASH: u8 = Op::Hash as u8;
        //const OP_INDEX: u8 = Op::Index as u8;
        //const OP_CALL: u8 = Op::Call as u8;
        //const OP_RETURN_VALUE: u8 = Op::ReturnValue as u8;
        //const OP_RETURN: u8 = Op::Return as u8;
        //const OP_GET_LOCAL: u8 = Op::GetLocal as u8;
        //const OP_SET_LOCAL: u8 = Op::SetLocal as u8;
        //const OP_GET_BUILTIN: u8 = Op::GetBuiltin as u8;
        //const OP_CLOSURE: u8 = Op::Closure as u8;
        //const OP_GET_FREE: u8 = Op::GetFree as u8;
        //const OP_CURRENT_CLOSURE: u8 = Op::CurrentClosure as u8;
        //match value {
        //    OP_CONSTANT => Ok(Op::Constant),
        //    OP_ADD => Ok(Op::Add),
        //    OP_POP => Ok(Op::Pop),
        //    OP_SUB => Ok(Op::Sub),
        //    OP_MUL => Ok(Op::Mul),
        //    OP_DIV => Ok(Op::Div),
        //    OP_TRUE => Ok(Op::True),
        //    OP_FALSE => Ok(Op::False),
        //    OP_EQUAL => Ok(Op::Equal),
        //    OP_NOT_EQUAL => Ok(Op::NotEqual),
        //    OP_GREATER_THAN => Ok(Op::GreaterThan),
        //    OP_MINUS => Ok(Op::Minus),
        //    OP_BANG => Ok(Op::Bang),
        //    OP_JUMP_NOT_TRUTHY => Ok(Op::JumpNotTruthy),
        //    OP_JUMP => Ok(Op::Jump),
        //    OP_NULL => Ok(Op::Null),
        //    OP_GET_GLOBAL => Ok(Op::GetGlobal),
        //    OP_SET_GLOBAL => Ok(Op::SetGlobal),
        //    OP_ARRAY => Ok(Op::Array),
        //    OP_HASH => Ok(Op::Hash),
        //    OP_INDEX => Ok(Op::Index),
        //    OP_CALL => Ok(Op::Call),
        //    OP_RETURN_VALUE => Ok(Op::ReturnValue),
        //    OP_RETURN => Ok(Op::Return),
        //    OP_GET_LOCAL => Ok(Op::GetLocal),
        //    OP_SET_LOCAL => Ok(Op::SetLocal),
        //    OP_GET_BUILTIN => Ok(Op::GetBuiltin),
        //    OP_CLOSURE => Ok(Op::Closure),
        //    OP_GET_FREE => Ok(Op::GetFree),
        //    OP_CURRENT_CLOSURE => Ok(Op::CurrentClosure),
        //    _ => Err(format!("unknown opcode {value}")),
        //}
    }
}

pub fn make(op: Op, operands: &[i64]) -> Vec<u8> {
    /* for some reason the book says that it used the hashmap directly instead of lookup function
     * for a good reason such as easier testing but I don't really get it and making a global
     * mapping at compile time in rust will be a big hassle even if I used a dependency so I don't
     * care for the time being */
    let def = lookup(op as u8);
    assert_eq!(def.operand_widths.len(), operands.len());
    let mut instruction_len = 1; // any opcode is 1 byte
    for w in def.operand_widths.iter() {
        instruction_len += w;
    }

    let mut instruction = vec![0; instruction_len as usize];
    instruction[0] = op as u8;

    let mut offset = 1;
    for (i, o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let bytes = (*o as u16).to_be_bytes();
                instruction[offset..offset + 2].copy_from_slice(&bytes);
            }
            1 => {
                instruction[offset] = *o as u8;
            }
            _ => panic!(),
        }
        offset += width as usize;
    }
    instruction
}

// for debugging purposes
pub struct Definition {
    name: &'static str,
    operand_widths: Vec<u8>, // number of bytes of each operand only
}
impl Definition {
    fn new(name: &'static str, operand_widths: Vec<u8>) -> Self {
        Self {
            name,
            operand_widths,
        }
    }
}

pub fn lookup(opcode: u8) -> Definition {
    let (name, operand_widths) = match Op::try_from(opcode) {
        Ok(Op::Constant) => ("OpConstant", vec![2]),
        Ok(Op::Add) => ("OpAdd", vec![]),
        Ok(Op::Pop) => ("OpPop", vec![]),
        Ok(Op::Sub) => ("OpSub", vec![]),
        Ok(Op::Mul) => ("OpMul", vec![]),
        Ok(Op::Div) => ("OpDiv", vec![]),
        Ok(Op::True) => ("OpTrue", vec![]),
        Ok(Op::False) => ("OpFalse", vec![]),
        Ok(Op::Equal) => ("OpEqual", vec![]),
        Ok(Op::NotEqual) => ("OpNotEqual", vec![]),
        Ok(Op::GreaterThan) => ("OpGreaterThan", vec![]),
        Ok(Op::Minus) => ("OpMinus", vec![]),
        Ok(Op::Bang) => ("OpBang", vec![]),
        Ok(Op::JumpNotTruthy) => ("OpJumpNotTruthy", vec![2]),
        Ok(Op::Jump) => ("OpJump", vec![2]),
        Ok(Op::Null) => ("OpNull", vec![]),
        Ok(Op::GetGlobal) => ("OpGetGlobal", vec![2]),
        Ok(Op::SetGlobal) => ("OpSetGlobal", vec![2]),
        Ok(Op::Array) => ("OpArray", vec![2]), // max array size is 65536
        Ok(Op::Hash) => ("OpHash", vec![2]),   // max hash size is 65536/2
        Ok(Op::Index) => ("OpIndex", vec![]),
        Ok(Op::Call) => ("OpCall", vec![1]), // max num of args is 256
        Ok(Op::ReturnValue) => ("OpReturnValue", vec![]),
        Ok(Op::Return) => ("OpReturn", vec![]),
        Ok(Op::GetLocal) => ("OpGetLocal", vec![1]), // maximum local bindings is 256
        Ok(Op::SetLocal) => ("OpSetLocal", vec![1]), // maximum local bindings is 256
        Ok(Op::GetBuiltin) => ("OpGetBuiltin", vec![1]), // maximum builtins is 256
        Ok(Op::Closure) => ("OpClosure", vec![2, 1]), // const_ind for comp_fn and num_free_variables on the stack
        Ok(Op::GetFree) => ("OpGetFree", vec![1]),
        Ok(Op::CurrentClosure) => ("OpCurrentClosure", vec![]),
        Err(op) => panic!("opcode {op:?} undefined"),
    };
    Definition::new(name, operand_widths)
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<i64>, i64) {
    let mut operands = vec![0i64; def.operand_widths.len()];
    let mut offset = 0;
    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => operands[i] = read_u16(&ins[offset..offset + 2]) as i64,
            1 => operands[i] = read_u8(&ins[offset..offset + 1]) as i64,
            _ => panic!("unsupported operand width"),
        }
        offset += *width as usize;
    }
    (operands, offset as i64)
}

pub fn read_u16(x: &[u8]) -> u16 {
    u16::from_be_bytes([x[0], x[1]])
}
pub fn read_u8(x: &[u8]) -> u8 {
    x[0]
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_make() {
        let tests = vec![
            (
                Op::Constant,
                vec![65534], // 0xfffe in big endian
                vec![Op::Constant as u8, 255, 254],
            ),
            (Op::Add, vec![], vec![Op::Add as u8]),
            (Op::GetLocal, vec![255], vec![Op::GetLocal as u8, 255u8]),
            (
                Op::Closure,
                vec![65534, 255],
                vec![Op::Closure as u8, 255u8, 254u8, 255u8],
            ),
        ];
        let mut errors = Vec::new();
        for (op, operands, expected) in tests {
            let instruction = make(op, &operands);
            if instruction.len() != expected.len() {
                errors.push(format!(
                    "instruction has wrong length. want={}, got={}",
                    expected.len(),
                    instruction.len()
                ));
            }
            for (i, b) in expected.iter().enumerate() {
                if instruction[i] != expected[i] {
                    errors.push(format!(
                        "wrong byte at pos {}. want={}, got={}",
                        i, b, instruction[i]
                    ));
                }
            }
        }
        if !errors.is_empty() {
            panic!("Test failed:\n{}", errors.join("\n"));
        }
    }
    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Op::Add, &[]),
            make(Op::GetLocal, &[1]),
            make(Op::Constant, &[2]),
            make(Op::Constant, &[65535]),
            make(Op::Closure, &[65535, 255]),
        ];
        let expected = r#"0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpClosure 65535 255
"#;
        let mut concatted = Instructions::default();
        for ins in instructions {
            concatted.0.extend(ins); // probably can be one liner with flat_map
        }
        if concatted.to_string() != expected {
            panic!(
                "instructions wrongly formatted.\nwant={}, got={}",
                expected, concatted
            )
        }
    }

    #[test]
    fn test_read_operands() {
        let tests: Vec<(Opcode, &[i64], i64)> = vec![
            (Op::Constant, &[65535], 2),
            (Op::GetLocal, &[255], 1),
            (Op::Closure, &[65535, 255], 3),
        ];
        for (op, operands, bytes_read) in tests {
            let instruction = make(op, operands);
            let def = lookup(op as u8);
            let (operands_read, n) = read_operands(&def, &(instruction[1..]));
            assert_eq!(n, bytes_read, "wrong. want={}, got={}", n, bytes_read);
            for (i, want) in operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}
