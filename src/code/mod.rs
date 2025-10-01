use std::collections::HashMap;

pub struct Instructions(Vec<u8>);

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Opcode {
    Constant = 0,
}
use Opcode as Op;

impl TryFrom<u8> for Op {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            val if val == Op::Constant as u8 => Ok(Op::Constant),
            _ => Err(()),
        }
    }
}

pub fn make(op: Op, operands: &[i64]) -> Vec<u8> {
    let def = lookup(op as u8);
    assert_eq!(def.operand_widths.len(), operands.len());
    let mut instruction_len = 1;
    for w in def.operand_widths.iter() {
        instruction_len += w;
    }

    let mut instruction = vec![0; instruction_len.into()];
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
    return instruction;
}

// for debugging purposes
pub struct Definition {
    name: String,
    operand_widths: Vec<u8>,
}

pub fn lookup(opcode: u8) -> Definition {
    //let opcode: Op = unsafe { std::mem::transmute(opcode) };
    match Op::try_from(opcode) {
        Ok(Op::Constant) => Definition {
            name: "OpConstant".into(),
            operand_widths: vec![2],
        },
        Err(op) => panic!("opcode {op:?} underfined"),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_make() {
        let tests = vec![(
            Op::Constant,
            vec![65534],
            vec![Op::Constant as u8, 255, 254],
        )];
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
}
