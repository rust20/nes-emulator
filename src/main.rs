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
    NoneAddressing,
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
    // adc and asl bit cmp dec eor lsr ora rol ror sbc
    opcodes[0x69] = OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate);
    opcodes[0x65] = OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x75] = OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x6d] = OpCode::new(0x6d, "ADC", 3, 4, AddressingMode::Absolute);
    opcodes[0x7d] = OpCode::new(0x7d, "ADC", 3, 4, AddressingMode::AbsoluteX);
    opcodes[0x79] = OpCode::new(0x79, "ADC", 3, 4, AddressingMode::AbsoluteY);
    opcodes[0x61] = OpCode::new(0x61, "ADC", 2, 6, AddressingMode::IndirectX);
    opcodes[0x71] = OpCode::new(0x71, "ADC", 2, 5, AddressingMode::IndirectY);

    opcodes[0x29] = OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate);
    opcodes[0x25] = OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x35] = OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x2d] = OpCode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute);
    opcodes[0x3d] = OpCode::new(0x3d, "AND", 3, 4, AddressingMode::AbsoluteX);
    opcodes[0x39] = OpCode::new(0x39, "AND", 3, 4, AddressingMode::AbsoluteY);
    opcodes[0x21] = OpCode::new(0x21, "AND", 2, 6, AddressingMode::IndirectX);
    opcodes[0x31] = OpCode::new(0x31, "AND", 2, 5, AddressingMode::IndirectY);

    opcodes[0x0a] = OpCode::new(0x0a, "ASL", 1, 2, AddressingMode::NoneAddressing /*accumulator*/);
    opcodes[0x06] = OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage);
    opcodes[0x16] = OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPageX);
    opcodes[0x0e] = OpCode::new(0x0e, "ASL", 3, 6, AddressingMode::Absolute);
    opcodes[0x1e] = OpCode::new(0x1e, "ASL", 3, 7, AddressingMode::AbsoluteX);

    /*
    0wy2liopcodes[0xA] = OpCode::new(0xpA, "AND", 2, 4, AddressingMode::Immediate);j0
    */

    // control flow
    // bcc bcs beq bmi bne bpl bvc bvs mp jsr rts

    // interrupts
    // brk rti

    // status register
    // clc cld cli clv sec sed sei

    // a,x,y registers
    // cpx cpy dex dey inc inx iny lda ldx ldy sta stx sty tax tay tsx txa txs tya

    // NOP

    // stack related
    // pha php pla plp


    opcodes[0x00] = OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing);
    opcodes[0x00] = OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing);
    opcodes[0xaa] = OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing);
    opcodes[0xe8] = OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing);

    opcodes[0xa9] = OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate);
    opcodes[0xa5] = OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage);
    opcodes[0xb5] = OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0xad] = OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute);
    opcodes[0xbd] = OpCode::new(0xbd, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::AbsoluteX);
    opcodes[0xb9] = OpCode::new(0xb9, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::AbsoluteY);
    opcodes[0xa1] = OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::IndirectX);
    opcodes[0xb1] = OpCode::new(0xb1, "LDA", 2, 5/*+1 if page crossed*/, AddressingMode::IndirectY);

    opcodes[0x85] = OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage);
    opcodes[0x95] = OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPageX);
    opcodes[0x8d] = OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute);
    opcodes[0x9d] = OpCode::new(0x9d, "STA", 3, 5, AddressingMode::AbsoluteX);
    opcodes[0x99] = OpCode::new(0x99, "STA", 3, 5, AddressingMode::AbsoluteY);
    opcodes[0x81] = OpCode::new(0x81, "STA", 2, 6, AddressingMode::IndirectX);
    opcodes[0x91] = OpCode::new(0x91, "STA", 2, 6, AddressingMode::IndirectY);

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
            println!("{:#04x}", opscode);
            let instr = &CPU_OPS_CODES[opscode as usize];
            match opscode {
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }

                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31  => {
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
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&instr.addressing_mode);
                    self.program_counter += instr.bytes as u16 - 1;
                }

                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&instr.addressing_mode);
                    self.program_counter += instr.cycles as u16 - 1;
                }

                0xaa => self.tax(),
                0xe8 => self.inx(),
                0x00 => return,
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

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let (_, overflow) = self.register_a.overflowing_add(value);
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


    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
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
}
