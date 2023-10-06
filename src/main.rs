// use std::collections::HashMap;

#[derive(Debug)]
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
    NoneAdressing,
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
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
            AddressingMode::NoneAdressing => panic!("mode {:?} is not supported", mode),
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
            match opscode {
                0xa9 => {
                    self.lda(&AddressingMode::Immediate);
                    self.program_counter += 1;
                }
                0xa5 => {
                    self.lda(&AddressingMode::ZeroPage);
                    self.program_counter += 1;
                }
                0xb5 => {
                    self.lda(&AddressingMode::ZeroPageX);
                    self.program_counter += 1;
                }
                0xad => {
                    self.lda(&AddressingMode::Absolute);
                    self.program_counter += 2;
                }
                0xbd => {
                    self.lda(&AddressingMode::AbsoluteX);
                    self.program_counter += 2;
                }
                0xb9 => {
                    self.lda(&AddressingMode::AbsoluteY);
                    self.program_counter += 2;
                }
                0xa1 => {
                    self.lda(&AddressingMode::IndirectX);
                    self.program_counter += 1;
                }
                0xb1 => {
                    self.lda(&AddressingMode::IndirectY);
                    self.program_counter += 1;
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
        self.status = 0x00;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn interpret(&mut self, program: Vec<u8>) {
        self.load_and_run(program);
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

    fn set_zero_and_negative_status_flag(&mut self, value: u8) {
        // set zero flag
        if value == 0 {
            self.status |= 0b0000_0010;
        } else {
            self.status &= !0b0000_0010;
        }

        // set negative flag
        if value & 0b1000_0000 > 0 {
            self.status |= 0b1000_0000;
        } else {
            self.status &= !0b1000_0000;
        }
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
        assert!((cpu.status & 0b0000_0010) == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
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
