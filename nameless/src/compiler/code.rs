#[derive(Debug, Clone)]
pub struct Instructions(Vec<u8>);

impl std::fmt::Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut index = 0;
        let mut string = String::new();

        while index < self.len() {
            let definition = Definition::lookup(OpCode::from_byte(self[index.clone()]));
            let (operands, read) = read_operands(&definition, &self[(index + 1)..]);

            string += &format!("{:0>4} {}", index, fmt_instruction(&definition, operands));
            index += 1 + read;
        }

        write!(f, "{}", string)
    }
}

impl<I> std::ops::Index<I> for Instructions
where
    I: std::slice::SliceIndex<[u8]>,
{
    type Output = <I as std::slice::SliceIndex<[u8]>>::Output;
    fn index(&self, index: I) -> &Self::Output {
        &self.0[index]
    }
}

impl<I> std::ops::IndexMut<I> for Instructions
where
    I: std::slice::SliceIndex<[u8]>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl Instructions {
    pub fn new(ins: Vec<u8>) -> Instructions {
        Instructions(ins)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn bytes(&self) -> &Vec<u8> {
        &self.0
    }

    pub fn push(&mut self, other: &Instructions) {
        self.0.extend(other.bytes());
    }

    pub fn iter(&self) -> std::slice::Iter<u8> {
        self.0.iter()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OpCode {
    Constant,
    True,
    False,
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    GreaterThan,
    LowerThan,
    Not,
    JumpNotTruthy,
    Jump,
    Invalid,
}

impl OpCode {
    pub fn from_byte(byte: u8) -> OpCode {
        match byte {
            0 => OpCode::Constant,
            1 => OpCode::True,
            2 => OpCode::False,
            3 => OpCode::Pop,
            4 => OpCode::Add,
            5 => OpCode::Sub,
            6 => OpCode::Mul,
            7 => OpCode::Div,
            8 => OpCode::Equal,
            9 => OpCode::NotEqual,
            10 => OpCode::GreaterThan,
            11 => OpCode::LowerThan,
            12 => OpCode::Not,
            13 => OpCode::JumpNotTruthy,
            14 => OpCode::Jump,
            _ => OpCode::Invalid,
        }
    }
}

/// # Definition
/// OpCode definition, holds Opcode name and operand widths
pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<u32>,
}

impl Definition {
    pub fn lookup(op: OpCode) -> Definition {
        match op {
            OpCode::Invalid => Definition {
                name: "Invalid".into(),
                operand_widths: vec![],
            },
            OpCode::Constant => Definition {
                name: "Constant".into(),
                operand_widths: vec![2],
            },
            OpCode::Pop => Definition {
                name: "Pop".into(),
                operand_widths: vec![],
            },
            OpCode::Add => Definition {
                name: "Add".into(),
                operand_widths: vec![],
            },
            OpCode::Sub => Definition {
                name: "Sub".into(),
                operand_widths: vec![],
            },
            OpCode::Mul => Definition {
                name: "Mul".into(),
                operand_widths: vec![],
            },
            OpCode::Div => Definition {
                name: "Div".into(),
                operand_widths: vec![],
            },
            OpCode::Equal => Definition {
                name: "Equal".into(),
                operand_widths: vec![],
            },
            OpCode::NotEqual => Definition {
                name: "NotEqual".into(),
                operand_widths: vec![],
            },
            OpCode::GreaterThan => Definition {
                name: "GreaterThan".into(),
                operand_widths: vec![],
            },
            OpCode::LowerThan => Definition {
                name: "LowerThan".into(),
                operand_widths: vec![],
            },
            OpCode::Not => Definition {
                name: "Not".into(),
                operand_widths: vec![],
            },
            OpCode::JumpNotTruthy => Definition {
                name: "JumpNotTruthy".into(),
                operand_widths: vec![2],
            },
            OpCode::Jump => Definition {
                name: "Jump".into(),
                operand_widths: vec![2],
            },
            OpCode::True => Definition {
                name: "True".into(),
                operand_widths: vec![],
            },
            OpCode::False => Definition {
                name: "False".into(),
                operand_widths: vec![],
            },
        }
    }
}

/// # make
/// Create a single bytecode instruction that's made up of an opcode and an number of operands
pub fn make(op: OpCode, operands: Vec<u32>) -> Instructions {
    let def = Definition::lookup(op);

    let mut instruction = {
        let instrunction_len = (1 + def.operand_widths.iter().sum::<u32>()) as usize;
        let mut vector = Vec::with_capacity(instrunction_len);
        vector.push(op as u8);
        vector
    };

    for (index, &operand) in operands.iter().enumerate() {
        let width = def.operand_widths[index];
        match width {
            2 | _ => {
                instruction.extend((operand as u16).to_be_bytes().iter());
            }
        }
    }

    Instructions::new(instruction)
}

fn read_operands(definition: &Definition, instruction: &[u8]) -> (Vec<u32>, usize) {
    let mut offset = 0;

    let mut operands = Vec::with_capacity(definition.operand_widths.len());

    for &width in definition.operand_widths.iter() {
        match width {
            2 | _ => operands.push(read_be_u16(&instruction[offset..(offset + 2)]) as u32),
        }

        offset += width as usize;
    }

    (operands, offset)
}

pub fn read_be_u16(input: &[u8]) -> u16 {
    let bytes = {
        let mut array: [u8; 2] = Default::default();
        array.copy_from_slice(input);
        array
    };

    u16::from_be_bytes(bytes)
}

fn fmt_instruction(definition: &Definition, operands: Vec<u32>) -> String {
    let count = definition.operand_widths.len();

    if operands.len() != count {
        return format!(
            "ERROR: operand count {} does not match defined {}\n",
            operands.len(),
            count
        );
    }

    match count {
        0 => format!("{}\n", definition.name),
        1 => format!("{} {}\n", definition.name, operands[0]),
        _ => format!("Unhandled operand count {}", count),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_make() {
        struct TestCase {
            op: OpCode,
            operand: Vec<u32>,
            expected: Instructions,
        }

        let tests = vec![
            TestCase {
                op: OpCode::Constant,
                operand: vec![65534],
                expected: Instructions::new(vec![OpCode::Constant as u8, 255, 254]),
            },
            TestCase {
                op: OpCode::Add,
                operand: vec![],
                expected: Instructions::new(vec![OpCode::Add as u8]),
            },
        ];

        for test in tests.iter() {
            let instruction = make(test.op, test.operand.clone());

            assert_eq!(instruction.len(), test.expected.len());

            for (index, &byte) in test.expected.iter().enumerate() {
                assert_eq!(instruction[index], byte);
            }
        }
    }

    #[test]
    fn test_read_operands() {
        struct TestCase {
            op: OpCode,
            operands: Vec<u32>,
            bytes_read: usize,
        }

        let tests = vec![TestCase {
            op: OpCode::Constant,
            operands: vec![65535],
            bytes_read: 2,
        }];

        for test in tests.iter() {
            let instruction = make(test.op, test.operands.clone());

            let definition = Definition::lookup(test.op);

            let (operands_read, n) = read_operands(&definition, &instruction[1..]);

            assert_eq!(test.bytes_read, n);

            for (i, &want) in test.operands.iter().enumerate() {
                assert_eq!(want, operands_read[i])
            }
        }
    }

    #[test]
    fn test_instruction_string() {
        let instructions = {
            let vector = vec![
                make(OpCode::Add, vec![]),
                make(OpCode::Constant, vec![2]),
                make(OpCode::Constant, vec![65535]),
            ];

            let mut concatted = Instructions::new(Vec::new());

            for ins in vector.iter() {
                concatted.push(ins);
            }

            concatted
        };

        let expected = r#"0000 Add
0001 Constant 2
0004 Constant 65535
"#;

        assert_eq!(expected, format!("{}", instructions), "asasd");
    }
}
