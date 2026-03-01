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
            0 => format!("{}", def.name),
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
        match value {
            val if val == Op::Constant as u8 => Ok(Op::Constant),
            val if val == Op::Add as u8 => Ok(Op::Add),
            val if val == Op::Pop as u8 => Ok(Op::Pop),
            val if val == Op::Sub as u8 => Ok(Op::Sub),
            val if val == Op::Mul as u8 => Ok(Op::Mul),
            val if val == Op::Div as u8 => Ok(Op::Div),
            val if val == Op::True as u8 => Ok(Op::True),
            val if val == Op::False as u8 => Ok(Op::False),
            val if val == Op::Equal as u8 => Ok(Op::Equal),
            val if val == Op::NotEqual as u8 => Ok(Op::NotEqual),
            val if val == Op::GreaterThan as u8 => Ok(Op::GreaterThan),
            val if val == Op::Minus as u8 => Ok(Op::Minus),
            val if val == Op::Bang as u8 => Ok(Op::Bang),
            val if val == Op::JumpNotTruthy as u8 => Ok(Op::JumpNotTruthy),
            val if val == Op::Jump as u8 => Ok(Op::Jump),
            val if val == Op::Null as u8 => Ok(Op::Null),
            val if val == Op::GetGlobal as u8 => Ok(Op::GetGlobal),
            val if val == Op::SetGlobal as u8 => Ok(Op::SetGlobal),
            val if val == Op::Array as u8 => Ok(Op::Array),
            val if val == Op::Hash as u8 => Ok(Op::Hash),
            val if val == Op::Index as u8 => Ok(Op::Index),
            val if val == Op::Call as u8 => Ok(Op::Call),
            val if val == Op::ReturnValue as u8 => Ok(Op::ReturnValue),
            val if val == Op::Return as u8 => Ok(Op::Return),
            val if val == Op::GetLocal as u8 => Ok(Op::GetLocal),
            val if val == Op::SetLocal as u8 => Ok(Op::SetLocal),
            val if val == Op::GetBuiltin as u8 => Ok(Op::GetBuiltin),
            val if val == Op::Closure as u8 => Ok(Op::Closure),
            val if val == Op::GetFree as u8 => Ok(Op::GetFree),
            val if val == Op::CurrentClosure as u8 => Ok(Op::CurrentClosure),
            _ => Err(format!("unknown opcode {value}")),
        }
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
    name: String,
    operand_widths: Vec<u8>, // number of bytes of each operand only
}
impl Definition {
    fn new(name: String, operand_widths: Vec<u8>) -> Self {
        Self {
            name,
            operand_widths,
        }
    }
}

pub fn lookup(opcode: u8) -> Definition {
    //let opcode: Op = unsafe { std::mem::transmute(opcode) };
    match Op::try_from(opcode) {
        Ok(Op::Constant) => Definition::new("OpConstant".into(), vec![2]),
        Ok(Op::Add) => Definition::new("OpAdd".into(), vec![]),
        Ok(Op::Pop) => Definition::new("OpPop".into(), vec![]),
        Ok(Op::Sub) => Definition::new("OpSub".into(), vec![]),
        Ok(Op::Mul) => Definition::new("OpMul".into(), vec![]),
        Ok(Op::Div) => Definition::new("OpDiv".into(), vec![]),
        Ok(Op::True) => Definition::new("OpTrue".into(), vec![]),
        Ok(Op::False) => Definition::new("OpFalse".into(), vec![]),
        Ok(Op::Equal) => Definition::new("OpEqual".into(), vec![]),
        Ok(Op::NotEqual) => Definition::new("OpNotEqual".into(), vec![]),
        Ok(Op::GreaterThan) => Definition::new("OpGreaterThan".into(), vec![]),
        Ok(Op::Minus) => Definition::new("OpMinus".into(), vec![]),
        Ok(Op::Bang) => Definition::new("OpBang".into(), vec![]),
        Ok(Op::JumpNotTruthy) => Definition::new("OpJumpNotTruthy".into(), vec![2]),
        Ok(Op::Jump) => Definition::new("OpJump".into(), vec![2]),
        Ok(Op::Null) => Definition::new("OpNull".into(), vec![]),
        Ok(Op::GetGlobal) => Definition::new("OpGetGlobal".into(), vec![2]),
        Ok(Op::SetGlobal) => Definition::new("OpSetGlobal".into(), vec![2]),
        Ok(Op::Array) => Definition::new("OpArray".into(), vec![2]), // max array size is 65536
        Ok(Op::Hash) => Definition::new("OpHash".into(), vec![2]),   // max hash size is 65536/2
        Ok(Op::Index) => Definition::new("OpIndex".into(), vec![]),
        Ok(Op::Call) => Definition::new("OpCall".into(), vec![1]), // max num of args is 256
        Ok(Op::ReturnValue) => Definition::new("OpReturnValue".into(), vec![]),
        Ok(Op::Return) => Definition::new("OpReturn".into(), vec![]),
        Ok(Op::GetLocal) => Definition::new("OpGetLocal".into(), vec![1]), // maximum local bindings is 256
        Ok(Op::SetLocal) => Definition::new("OpSetLocal".into(), vec![1]), // maximum local bindings is 256
        Ok(Op::GetBuiltin) => Definition::new("OpGetBuiltin".into(), vec![1]), // maximum builtins is 256
        Ok(Op::Closure) => Definition::new("OpClosure".into(), vec![2, 1]), // const_ind for comp_fn and num_free_variables on the stack
        Ok(Op::GetFree) => Definition::new("OpGetFree".into(), vec![1]),
        Ok(Op::CurrentClosure) => Definition::new("OpCurrentClosure".into(), vec![]),
        Err(op) => panic!("opcode {op:?} undefined"),
    }
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
