use bitflags::bitflags;
use lazy_static::lazy_static;

#[derive(Debug, Clone)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    Indirect,
    Relative,
    NoneAddressing,
}

#[derive(Debug, Clone)]
pub enum Register {
    A,
    X,
    Y,
}

#[derive(Clone)]
pub struct OpCode {
    pub opcode: u8,
    pub instr: &'static str,
    pub bytes: u8,
    pub cycles: u8,
    pub addressing_mode: AddressingMode,
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = {
    let mut opcodes = vec![OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing); 256];

    // arithmetic & logic
    // ADC AND ASL BIT CMP DEC EOR LSR ORA ROL ROR SBC
    opcodes[0x69] = OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate);
    opcodes[0x65] = OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x75] = OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x6d] = OpCode::new(0x6d, "ADC", 3, 4, AddressingMode::Absolute);
    opcodes[0x7d] = OpCode::new(0x7d, "ADC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0x79] = OpCode::new(0x79, "ADC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0x61] = OpCode::new(0x61, "ADC", 2, 6, AddressingMode::IndirectX);
    opcodes[0x71] = OpCode::new(0x71, "ADC", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0x29] = OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate);
    opcodes[0x25] = OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x35] = OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x2d] = OpCode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute);
    opcodes[0x3d] = OpCode::new(0x3d, "AND", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0x39] = OpCode::new(0x39, "AND", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0x21] = OpCode::new(0x21, "AND", 2, 6, AddressingMode::IndirectX);
    opcodes[0x31] = OpCode::new(0x31, "AND", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0x0a] = OpCode::new(0x0a, "ASL", 1, 2, AddressingMode::NoneAddressing /*accumulator*/);
    opcodes[0x06] = OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage);
    opcodes[0x16] = OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0x0e] = OpCode::new(0x0e, "ASL", 3, 6, AddressingMode::Absolute);
    opcodes[0x1e] = OpCode::new(0x1e, "ASL", 3, 7, AddressingMode::AbsoluteX);

    opcodes[0x24] = OpCode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x2C] = OpCode::new(0x2C, "BIT", 3, 4, AddressingMode::Absolute);

    opcodes[0xc9] = OpCode::new(0xc9, "CMP", 2, 2, AddressingMode::Immediate);
    opcodes[0xc5] = OpCode::new(0xc5, "CMP", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xd5] = OpCode::new(0xd5, "CMP", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0xcd] = OpCode::new(0xcd, "CMP", 2, 4, AddressingMode::Absolute);
    opcodes[0xdd] = OpCode::new(0xdd, "CMP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0xd9] = OpCode::new(0xd9, "CMP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0xc1] = OpCode::new(0xc1, "CMP", 2, 6, AddressingMode::IndirectX);
    opcodes[0xd1] = OpCode::new(0xd1, "CMP", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0xc6] = OpCode::new(0xc6, "DEC", 2, 5, AddressingMode::ZeroPage);
    opcodes[0xd6] = OpCode::new(0xd6, "DEC", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0xce] = OpCode::new(0xce, "DEC", 3, 6, AddressingMode::Absolute);
    opcodes[0xde] = OpCode::new(0xde, "DEC", 3, 7, AddressingMode::AbsoluteX);

    opcodes[0x49] = OpCode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate);
    opcodes[0x45] = OpCode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x55] = OpCode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x4d] = OpCode::new(0x4d, "EOR", 2, 4, AddressingMode::Absolute);
    opcodes[0x5d] = OpCode::new(0x5d, "EOR", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0x59] = OpCode::new(0x59, "EOR", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0x41] = OpCode::new(0x41, "EOR", 2, 6, AddressingMode::IndirectX);
    opcodes[0x51] = OpCode::new(0x51, "EOR", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0x4a] = OpCode::new(0x4a, "LSR", 1, 2, AddressingMode::NoneAddressing /*accumulator*/);
    opcodes[0x46] = OpCode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage);
    opcodes[0x56] = OpCode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0x4e] = OpCode::new(0x4e, "LSR", 3, 6, AddressingMode::Absolute);
    opcodes[0x5e] = OpCode::new(0x5e, "LSR", 3, 7, AddressingMode::AbsoluteX);

    opcodes[0x09] = OpCode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate);
    opcodes[0x05] = OpCode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x15] = OpCode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x0d] = OpCode::new(0x0d, "ORA", 3, 4, AddressingMode::Absolute);
    opcodes[0x1d] = OpCode::new(0x1d, "ORA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0x19] = OpCode::new(0x19, "ORA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0x01] = OpCode::new(0x01, "ORA", 2, 6, AddressingMode::IndirectX);
    opcodes[0x11] = OpCode::new(0x11, "ORA", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0x2a] = OpCode::new(0x2a, "ROL", 1, 2, AddressingMode::NoneAddressing /*accumulator*/);
    opcodes[0x26] = OpCode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage);
    opcodes[0x36] = OpCode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0x2e] = OpCode::new(0x2e, "ROL", 3, 6, AddressingMode::Absolute);
    opcodes[0x3e] = OpCode::new(0x3e, "ROL", 3, 7, AddressingMode::AbsoluteX);

    opcodes[0x6a] = OpCode::new(0x6a, "ROR", 1, 2, AddressingMode::NoneAddressing /*accumulator*/);
    opcodes[0x66] = OpCode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage);
    opcodes[0x76] = OpCode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0x6e] = OpCode::new(0x6e, "ROR", 3, 6, AddressingMode::Absolute);
    opcodes[0x7e] = OpCode::new(0x7e, "ROR", 3, 7, AddressingMode::AbsoluteX);

    opcodes[0xe9] = OpCode::new(0xe9, "SBC", 2, 2, AddressingMode::Immediate);
    opcodes[0xe5] = OpCode::new(0xe5, "SBC", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xf5] = OpCode::new(0xf5, "SBC", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0xed] = OpCode::new(0xed, "SBC", 3, 4, AddressingMode::Absolute);
    opcodes[0xfd] = OpCode::new(0xfd, "SBC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0xf9] = OpCode::new(0xf9, "SBC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0xe1] = OpCode::new(0xe1, "SBC", 2, 6, AddressingMode::IndirectX);
    opcodes[0xf1] = OpCode::new(0xf1, "SBC", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY);

    /*
    0wy2liopcodes[0xA] = OpCode::new(0xpA, "AND", 2, 4, AddressingMode::Immediate);j0
    */

    // Control Flow
    // BCC BCS BEQ BMI BNE BPL BVC BVS JMP JSR RTS
    opcodes[0x90] = OpCode::new(0x90, "BCC", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
    opcodes[0xb0] = OpCode::new(0xb0, "BCS", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
    opcodes[0xf0] = OpCode::new(0xf0, "BEQ", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
    opcodes[0x30] = OpCode::new(0x30, "BMI", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
    opcodes[0xd0] = OpCode::new(0xd0, "BNE", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
    opcodes[0x50] = OpCode::new(0x50, "BVC", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
    opcodes[0x70] = OpCode::new(0x70, "BVS", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);

    opcodes[0x4c] = OpCode::new(0x4c, "JMP", 3, 3, AddressingMode::Absolute);
    opcodes[0x6c] = OpCode::new(0x6c, "JMP", 3, 5, AddressingMode::Indirect);

    opcodes[0x20] = OpCode::new(0x20, "JSR", 3, 6, AddressingMode::Absolute);
    opcodes[0x60] = OpCode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing);


    // Interrupts
    // BRK RTI
    opcodes[0x00] = OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing);
    opcodes[0x40] = OpCode::new(0x40, "RTI", 1, 6, AddressingMode::NoneAddressing);

    // Status Register
    // CLC CLD CLI CLV SEC SED SEI
    opcodes[0x18] = OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xd8] = OpCode::new(0xd8, "CLD", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0x58] = OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xb8] = OpCode::new(0xb8, "CLV", 1, 2, AddressingMode::NoneAddressing);

    opcodes[0x38] = OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xf8] = OpCode::new(0xf8, "SED", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0x78] = OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing);


    // A,X,Y Registers
    // CPX CPY DEX DEY INC INX INY LDA LDX LDY STA STX STY
    // TAX TAY TSX TXA TXS TYA
    opcodes[0xe0] = OpCode::new(0xe0, "CPX", 2, 2, AddressingMode::Immediate);
    opcodes[0xe4] = OpCode::new(0xe4, "CPX", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xec] = OpCode::new(0xec, "CPX", 3, 4, AddressingMode::Absolute);

    opcodes[0xc0] = OpCode::new(0xc0, "CPY", 2, 2, AddressingMode::Immediate);
    opcodes[0xc4] = OpCode::new(0xc4, "CPY", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xcc] = OpCode::new(0xcc, "CPY", 3, 4, AddressingMode::Absolute);

    opcodes[0xca] = OpCode::new(0xca, "DEX", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0x88] = OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing);

    opcodes[0xe6] = OpCode::new(0xe6, "INC", 2, 5, AddressingMode::ZeroPage);
    opcodes[0xf6] = OpCode::new(0xf6, "INC", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0xee] = OpCode::new(0xee, "INC", 3, 6, AddressingMode::Absolute);
    opcodes[0xfe] = OpCode::new(0xfe, "INC", 3, 7, AddressingMode::AbsoluteX);

    opcodes[0xe8] = OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xc8] = OpCode::new(0xc8, "INY", 1, 2, AddressingMode::NoneAddressing);

    opcodes[0xa9] = OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate);
    opcodes[0xa5] = OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xb5] = OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0xad] = OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute);
    opcodes[0xbd] = OpCode::new(0xbd, "LDA", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0xb9] = OpCode::new(0xb9, "LDA", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0xa1] = OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::IndirectX);
    opcodes[0xb1] = OpCode::new(0xb1, "LDA", 2, 5/* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0xa2] = OpCode::new(0xa2, "LDX", 2, 2, AddressingMode::Immediate);
    opcodes[0xa6] = OpCode::new(0xa6, "LDX", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xb6] = OpCode::new(0xb6, "LDX", 2, 4, AddressingMode::ZeroPageY);
    opcodes[0xae] = OpCode::new(0xae, "LDX", 3, 4, AddressingMode::Absolute);
    opcodes[0xbe] = OpCode::new(0xbe, "LDX", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteY);

    opcodes[0xa0] = OpCode::new(0xa0, "LDY", 2, 2, AddressingMode::Immediate);
    opcodes[0xa4] = OpCode::new(0xa4, "LDY", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xb4] = OpCode::new(0xb4, "LDY", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0xac] = OpCode::new(0xac, "LDY", 3, 4, AddressingMode::Absolute);
    opcodes[0xbc] = OpCode::new(0xbc, "LDY", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteX);

    opcodes[0x85] = OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x95] = OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x8d] = OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute);
    opcodes[0x9d] = OpCode::new(0x9d, "STA", 3, 5/* +1 if page crossed */, AddressingMode::AbsoluteX);
    opcodes[0x99] = OpCode::new(0x99, "STA", 3, 5/* +1 if page crossed */, AddressingMode::AbsoluteY);
    opcodes[0x81] = OpCode::new(0x81, "STA", 2, 6, AddressingMode::IndirectX);
    opcodes[0x91] = OpCode::new(0x91, "STA", 2, 6/* +1 if page crossed */, AddressingMode::IndirectY);

    opcodes[0x86] = OpCode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x96] = OpCode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPageY);
    opcodes[0x8e] = OpCode::new(0x8e, "STX", 3, 4, AddressingMode::Absolute);

    opcodes[0x84] = OpCode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x94] = OpCode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x8c] = OpCode::new(0x8c, "STY", 3, 4, AddressingMode::Absolute);

    opcodes[0xaa] = OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xa8] = OpCode::new(0xa8, "TAY", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xba] = OpCode::new(0xba, "TSX", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0x8a] = OpCode::new(0x8a, "TXA", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0x9a] = OpCode::new(0x9a, "TXS", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0x98] = OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing);


    // NOP
    opcodes[0xea] = OpCode::new(0xea, "NOP", 1, 2, AddressingMode::NoneAddressing);

    // Stack Related
    // PHA PHP PLA PLP
    opcodes[0x48] = OpCode::new(0x48, "PHA", 1, 3, AddressingMode::NoneAddressing);
    opcodes[0x08] = OpCode::new(0x08, "PHP", 1, 3, AddressingMode::NoneAddressing);
    opcodes[0x68] = OpCode::new(0x68, "PLA", 1, 4, AddressingMode::NoneAddressing);
    opcodes[0x28] = OpCode::new(0x28, "PLP", 1, 4, AddressingMode::NoneAddressing);

    opcodes
    };
}

impl OpCode {
    pub fn new(
        opcode: u8,
        instr: &'static str,
        bytes: u8,
        cycles: u8,
        addressing_mode: AddressingMode,
    ) -> Self {
        Self {
            opcode,
            instr,
            bytes,
            cycles,
            addressing_mode,
        }
    }
}

bitflags! {
    pub struct Flag: u8 {

    const CARRY = 0b0000_0001;
    const ZERO = 0b0000_0010;
    const INTERRUPT_DISABLE = 0b0000_0100;
    const DECIMAL_MODE = 0b0000_1000;
    const BREAK_CMD = 0b0001_0000;
    const ONE = 0b0010_0000;
    const OVERFLOW = 0b0100_0000;
    const NEGATIVE = 0b1000_0000;
    }
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: Flag,
    pub program_counter: u16,
    pub stack_pointer: u8,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: Flag::empty(),
            program_counter: 0,
            stack_pointer: 0xff,
            memory: [0; 0xffff],
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::ZeroPageX => self
                .mem_read(self.program_counter)
                .wrapping_add(self.register_x) as u16,
            AddressingMode::ZeroPageY => self
                .mem_read(self.program_counter)
                .wrapping_add(self.register_y) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::AbsoluteX => self
                .mem_read_u16(self.program_counter)
                .wrapping_add(self.register_x as u16),

            AddressingMode::AbsoluteY => self
                .mem_read_u16(self.program_counter)
                .wrapping_add(self.register_y as u16),

            AddressingMode::IndirectX => {
                let base = self.mem_read(self.program_counter);
                self.mem_read_u16(base.wrapping_add(self.register_x) as u16)
            }
            AddressingMode::IndirectY => {
                let base = self.mem_read(self.program_counter);
                self.mem_read_u16(base as u16)
                    .wrapping_add(self.register_y as u16)
            }
            AddressingMode::Indirect => {
                let base = self.mem_read_u16(self.program_counter);
                self.mem_read_u16(base as u16)
            }
            AddressingMode::Relative => {
                self.program_counter + self.mem_read(self.program_counter) as u16
            }
            AddressingMode::NoneAddressing => panic!("mode {:?} is not supported", mode),
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        self.mem_read(pos) as u16 | self.mem_read(pos.wrapping_add(1)) as u16 * 256
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.mem_write(pos, (data & 0xff) as u8);
        self.mem_write(pos.wrapping_add(1), (data >> 8) as u8);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000)
    }

    pub fn run(&mut self) {
        loop {
            let opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let instr = &CPU_OPS_CODES[opscode as usize];
            // println!("{:#04x}: {}", opscode, instr.instr);
            match opscode {
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }

                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x0a => {
                    self.asl_accumulator();
                }
                0x24 | 0x2c => {
                    self.bit(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.compare_register(Register::A, &instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x4a => self.lsr_accumulator(),
                0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x2a | 0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x6a | 0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }

                // Control Flow
                0x90 => {
                    self.branch_if_flag_status(Flag::CARRY, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xb0 => {
                    self.branch_if_flag_status(Flag::CARRY, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xf0 => {
                    self.branch_if_flag_status(Flag::ZERO, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xd0 => {
                    self.branch_if_flag_status(Flag::ZERO, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x30 => {
                    self.branch_if_flag_status(Flag::NEGATIVE, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x10 => {
                    self.branch_if_flag_status(Flag::NEGATIVE, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x50 => {
                    self.branch_if_flag_status(Flag::OVERFLOW, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x70 => {
                    self.branch_if_flag_status(Flag::OVERFLOW, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x4c | 0x6c => {
                    self.jmp(&instr.addressing_mode);
                }
                0x20 => {
                    self.jsr(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x60 => self.rts(),

                // Interrupts
                0x00 => return,
                0x40 => self.rti(),

                // Status Register
                0x18 => self.status.remove(Flag::CARRY),
                0xd8 => self.status.remove(Flag::DECIMAL_MODE),
                0x58 => self.status.remove(Flag::INTERRUPT_DISABLE),
                0xb8 => self.status.remove(Flag::OVERFLOW),

                0x38 => self.status.insert(Flag::CARRY),
                0xf8 => self.status.insert(Flag::DECIMAL_MODE),
                0x78 => self.status.insert(Flag::INTERRUPT_DISABLE),

                // A, X, Y Register
                0xe0 | 0xe4 | 0xec => {
                    self.compare_register(Register::X, &instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xc0 | 0xc4 | 0xcc => {
                    self.compare_register(Register::Y, &instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xca => self.dex(),
                0x88 => self.dey(),
                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xe8 => self.inx(),
                0xc8 => self.iny(),
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x86 | 0x96 | 0x8e => {
                    self.stx(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x84 | 0x94 | 0x8c => {
                    self.sty(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xaa => self.tax(),
                0xa8 => self.tay(),
                0xba => self.tsx(),
                0x8a => self.txa(),
                0x9a => self.txs(),
                0x98 => self.tya(),

                // NOP
                0xea => {}

                // Stack Related
                0x48 => self.pha(),
                0x08 => self.php(),
                0x68 => self.pla(),
                0x28 => self.plp(),

                _ => todo!(),
            }
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = Flag::empty();
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn interpret(&mut self, program: Vec<u8>) {
        self.load_and_run(program);
    }

    // Arithmetic & logic
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let (result, overflow) = self.register_a.overflowing_add(value);
        self.register_a = result;
        if overflow {
            self.status.insert(Flag::CARRY);
            self.status.insert(Flag::OVERFLOW);
        }
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn asl(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let (result, overflow) = value.overflowing_shl(1);
        self.mem_write(addr, result);
        self.set_overflow_flag(overflow);
        self.set_zero_and_negative_status_flag(result);
    }
    fn asl_accumulator(&mut self) {
        let (result, overflow) = self.register_a.overflowing_shl(1);
        self.register_a = result;
        self.set_overflow_flag(overflow);
        self.set_zero_and_negative_status_flag(result);
    }
    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a & value;

        self.set_zero_and_negative_status_flag(result);
        self.set_overflow_flag(Flag::from_bits(result).unwrap().contains(Flag::OVERFLOW));
    }
    fn compare_register(&mut self, register: Register, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let reg_val = match register {
            Register::A => self.register_a,
            Register::X => self.register_x,
            Register::Y => self.register_y,
        };

        self.status.set(Flag::CARRY, reg_val >= value);
        self.status.set(Flag::ZERO, reg_val == value);
        self.status.set(Flag::NEGATIVE, (reg_val.wrapping_sub(value) & 0x80) == 0x80);
    }
    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_sub(1);
        self.mem_write(addr, result);
        self.set_zero_and_negative_status_flag(result);
    }
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a ^= value;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn lsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value >> 1;
        self.mem_write(addr, result);

        self.status.set(Flag::CARRY, value & 0x01 == 1);
        self.set_zero_and_negative_status_flag(result);
    }
    fn lsr_accumulator(&mut self) {
        self.status.set(Flag::CARRY, self.register_a & 0x01 == 1);
        self.register_a = self.register_a >> 1;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a |= value;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn rol(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.status.set(Flag::CARRY, value & 0x80 == 0x80);
        let result = value << 1 | (self.status.bits() & Flag::CARRY.bits());
        self.mem_write(addr, result);
        self.set_zero_and_negative_status_flag(result);
    }
    fn ror(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.status.set(Flag::CARRY, value & 0x01 == 0x01);
        let result = value >> 1 | (self.status.bits() & Flag::CARRY.bits());
        self.mem_write(addr, result);
        self.set_zero_and_negative_status_flag(result);
    }
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let (result, overflow) = self.register_a.overflowing_sub(value);
        self.register_a = result;
        if overflow {
            self.status.remove(Flag::CARRY);
            self.status.insert(Flag::OVERFLOW);
        }
        self.set_zero_and_negative_status_flag(self.register_a);
    }

    // Control Flow
    fn branch_if_flag_status(&mut self, flag: Flag, is_set: bool) {
        if self.status.contains(flag) == is_set {
            self.program_counter = self.program_counter.wrapping_add_signed((self.mem_read(self.program_counter) as i8) as i16);
        }
    }
    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.program_counter = addr;
    }
    fn jsr(&mut self, mode: &AddressingMode) {
        self.mem_write_u16(self.stack_pointer as u16 - 1, self.program_counter);
        self.stack_pointer -= 2;

        let addr = self.get_operand_address(mode);
        self.program_counter = addr - 1;
    }
    fn rts(&mut self) {
        self.stack_pointer += 2;
        self.program_counter = self.mem_read_u16(self.stack_pointer as u16 - 1);
    }

    // Interrupts
    fn rti(&mut self) {
        self.plp();
        self.stack_pointer += 2;
        self.program_counter = self.mem_read_u16(self.stack_pointer as u16 - 1);
    }

    // A, X, Y Register
    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.set_zero_and_negative_status_flag(self.register_y);
    }
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(1);
        self.mem_write(addr, result);
        self.set_zero_and_negative_status_flag(result);
    }
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.set_zero_and_negative_status_flag(self.register_y);
    }
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.set_zero_and_negative_status_flag(self.register_y);
    }
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }
    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }
    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.set_zero_and_negative_status_flag(self.register_y);
    }
    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.set_zero_and_negative_status_flag(self.register_a);
    }

    // Stack Related
    fn pha(&mut self) {
        self.mem_write(self.stack_pointer as u16, self.register_a);
        self.stack_pointer -= 1;
    }
    fn php(&mut self) {
        self.mem_write(self.stack_pointer as u16, self.status.bits());
        self.stack_pointer -= 1;
    }
    fn pla(&mut self) {
        self.stack_pointer += 1;
        self.register_a = self.mem_read(self.stack_pointer as u16);
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn plp(&mut self) {
        self.stack_pointer += 1;
        let result = self.mem_read(self.stack_pointer as u16);
        self.status = Flag::from_bits(result).unwrap();
    }

    fn set_zero_and_negative_status_flag(&mut self, value: u8) {
        self.status.set(Flag::ZERO, value == 0);
        self.status.set(Flag::NEGATIVE, value & 0b1000_0000 > 0);
    }
    fn set_overflow_flag(&mut self, value: bool) {
        self.status.set(Flag::OVERFLOW, value);
    }
}

fn main() {
    let input = vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00];

    let mut c = CPU::new();
    c.load_and_run(input);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status.contains(Flag::ZERO) == false);
        assert!(cpu.status.contains(Flag::NEGATIVE) == false);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.contains(Flag::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        assert_eq!(cpu.register_x, 0xc1);
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_0x69_adc_add_with_carry() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xf0, 0x69, 0x25, 0x00]);
        assert_eq!(cpu.register_a, 0x15);
        assert!(cpu.status.contains(Flag::CARRY));
    }

    #[test]
    fn test_0x69_adc_add_carry_not_set() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x10, 0x69, 0x25, 0x00]);
        assert_eq!(cpu.register_a, 0x35);
        assert_eq!(cpu.status.contains(Flag::CARRY), false);
    }

    #[test]
    fn test_0x29_and() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x29, 0x06, 0x00]);
        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x0a_asl() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x0a, 0x00]);
        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0x24_bit_no_set_zero() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x85, 0x10, 0xa9, 0x04, 0x24, 0x10, 0x00]);
        assert_eq!(cpu.status.contains(Flag::ZERO), false);
    }

    #[test]
    fn test_0x24_bit_set_zero() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x85, 0x10, 0xa9, 0x0a, 0x24, 0x10, 0x00]);
        assert_eq!(cpu.status.contains(Flag::ZERO), true);
    }

    #[test]
    fn test_0xe0_cpx_compare_x_eq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x10, 0xe0, 0x10, 0x00]);
        assert_eq!(cpu.status.contains(Flag::CARRY), true);
        assert_eq!(cpu.status.contains(Flag::ZERO), true);
        assert_eq!(cpu.status.contains(Flag::NEGATIVE), false);
    }

    #[test]
    fn test_0xe0_cpx_compare_x_geq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x10, 0xe0, 0x05, 0x00]);
        assert_eq!(cpu.status.contains(Flag::CARRY), true);
        assert_eq!(cpu.status.contains(Flag::ZERO), false);
        assert_eq!(cpu.status.contains(Flag::NEGATIVE), false);
    }

    #[test]
    fn test_0xe0_cpx_compare_x_lt() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x02, 0xe0, 0x03, 0x00]);
        assert_eq!(cpu.status.contains(Flag::CARRY), false);
        assert_eq!(cpu.status.contains(Flag::ZERO), false);
        assert_eq!(cpu.status.contains(Flag::NEGATIVE), true);
    }

    // #[ignore]
    #[test]
    fn test_branching() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![
            0xa2, 0x08, 0xca, 0x8e, 0x00, 0x02, 0xe0, 0x03, 0xd0, 0xf8, 0x8e, 0x01, 0x02, 0x00,
        ]);
        assert_eq!(cpu.register_x, 0x03);
    }
}
