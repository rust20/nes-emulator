// use std::collections::HashMap;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xffff],
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x800..(0x800 + program.len())].copy_from_slice(&program[..]);
        self.program_counter = 0x800;
    }

    pub fn run(&mut self) {
        loop {
            let opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;
            match opscode {
                0xa9 => {
                    let param = self.mem_read(self.program_counter);
                    self.program_counter += 1;
                    self.lda(param);
                }
                0xaa => self.tax(),
                0xe8 => self.inx(),
                0x00 => return,
                _ => todo!(),
            }
        }
    }

    pub fn interpret(&mut self, program:Vec<u8>) {
        self.load_and_run(program);
    }

    fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_zero_and_negative_status_flag(self.register_x);
    }
    fn inx(&mut self) {
        self.register_x = if self.register_x == 0xff {
            0
        } else {
            self.register_x + 1
        };
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

    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!((cpu.status & 0b0000_0010) == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.interpret(vec![0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        assert_eq!(cpu.register_x, 0xc1);
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.interpret(vec![0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1);
    }
}
