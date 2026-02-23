use std::{fmt::Display, ops::Deref};

#[derive(Debug, Clone, Default)]
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
            1 => format!("{} {}", def.name, operands[0]),
            _ => format!("ERROR: unhandled operand_count for {\n}", def.name),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Opcode {
    Constant = 0x00,
    Add = 0x01,
}
use Opcode as Op;

impl TryFrom<u8> for Op {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            val if val == Op::Constant as u8 => Ok(Op::Constant),
            val if val == Op::Add as u8 => Ok(Op::Add),
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
        Err(op) => panic!("opcode {op:?} undefined"),
    }
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<i64>, i64) {
    let mut operands = vec![0i64; def.operand_widths.len()];
    let mut offset = 0;
    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => operands[i] = read_u16(&ins[offset..offset + 2]) as i64,
            _ => panic!("unsupported operand width"),
        }
        offset += *width as usize;
    }
    (operands, offset as i64)
}

pub fn read_u16(x: &[u8]) -> u16 {
    u16::from_be_bytes([x[0], x[1]])
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
            make(Op::Constant, &[1]),
            make(Op::Constant, &[2]),
            make(Op::Constant, &[65535]),
        ];
        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
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
        let tests = vec![(Op::Constant, &[65535], 2)];
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
