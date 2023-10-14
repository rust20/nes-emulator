use bitflags::bitflags;
use lazy_static::lazy_static;

use rand::Rng;
use std::collections::HashMap;
use std::{thread, time::Duration};

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
use sdl2::EventPump;

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
    pub static ref NES_TAG: Vec<u8> = vec![0x4e, 0x45, 0x53, 0x1a];
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
    opcodes[0x10] = OpCode::new(0x10, "BPL", 2, 2 /*+1 if branch success, +2 if to a new page*/, AddressingMode::Relative);
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
    #[derive(Copy, Clone, Debug)]
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

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        self.mem_read(pos) as u16 | ((self.mem_read(pos.wrapping_add(1)) as u16) << 8)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.mem_write(pos, (data & 0xff) as u8);
        self.mem_write(pos.wrapping_add(1), (data >> 8) as u8);
    }
}

#[derive(Debug, PartialEq)]
pub enum Mirroring {
    VERTICAL,
    HORIZONTAL,
    FOURSCREEN,
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
}

const PRG_ROM_PAGE_SIZE: usize = 0;
const CHR_ROM_PAGE_SIZE: usize = 0;

impl Rom {
    pub fn new(raw: &Vec<u8>) -> Result<Rom, String> {
        if &raw[0..4] != NES_TAG.to_vec() {
            return Err("File not in iNES file format".to_string());
        }

        let mapper = (raw[7] & 0xf0) | (raw[6] >> 4);

        let ines_ver = (raw[7] >> 2) & 0b11;
        if ines_ver != 0 {
            return Err("iNES2.0 format is not supported".to_string());
        }

        let four_screen = raw[6] & 0b1000 != 0;
        let vertical_mirroring = raw[6] & 0b1 != 0;
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FOURSCREEN,
            (false, true) => Mirroring::VERTICAL,
            (false, false) => Mirroring::HORIZONTAL,
        };

        let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;
        let skip_trainer = raw[6] & 0b100 != 0;

        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        Ok(Rom {
            prg_rom: raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
            mapper,
            screen_mirroring,
        })
    }
}

pub struct Bus {
    cpu_vram: [u8; 2048],
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_vram: [0; 2048],
            rom,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr & 0x7fff;
        }
        self.rom.prg_rom[addr as usize]
    }
}

const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x1fff;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3fff;
const PRG_ROM_SPACE: u16 = 0x8000;
const PRG_ROM_SPACE_END: u16 = 0xffff;

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRROR_END => {
                let mirror_down_addr = addr & 0x07ff;
                self.cpu_vram[mirror_down_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0x2007;
                todo!("ppu not supported yet")
                // self.cpu_vram[mirror_down_addr as usize]
            }
            PRG_ROM_SPACE..=PRG_ROM_SPACE_END => self.read_prg_rom(addr),
            _ => {
                println!("ignoring mem access at {}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRROR_END => {
                let mirror_down_addr = addr & 0b111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("ppu not supported yet");
                // self.cpu_vram[mirror_down_addr as usize]
            }
            PRG_ROM_SPACE..=PRG_ROM_SPACE_END => {
                panic!("attempt to write to cartridge rom space");
            }
            _ => {
                println!("ignoring mem access at {}", addr);
            }
        }
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

    pub cpu_cycle: u64,

    pub random_state: Vec<u8>,
    fn_stack: Vec<String>,
    instr_addr_map: HashMap<Vec<u8>, String>,
    last_jump: String,
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data
    }
}

impl CPU {
    pub fn new() -> Self {

        let mut instr = HashMap::new();

        // jsr
        instr.insert(vec![0x20, 0x06, 0x06], "init".to_string()                         );
        instr.insert(vec![0x20, 0x0d, 0x06], "initSnake".to_string()                    ); 
        instr.insert(vec![0x20, 0x19, 0x07], "drawApple".to_string()                    ); 
        instr.insert(vec![0x20, 0x20, 0x07], "drawSnake".to_string()                    ); 
        instr.insert(vec![0x20, 0x2a, 0x06], "generateApplePosition".to_string()        ); 
        instr.insert(vec![0x20, 0x2a, 0x06], "generateApplePossition".to_string()       ); 
        instr.insert(vec![0x20, 0x2d, 0x07], "spinWheels".to_string()                   ); 
        instr.insert(vec![0x20, 0x38, 0x06], "loop".to_string()                         ); 
        instr.insert(vec![0x20, 0x4d, 0x06], "readKeys".to_string()                     ); 
        instr.insert(vec![0x20, 0x8d, 0x06], "checkCollision".to_string()               ); 
        instr.insert(vec![0x20, 0x94, 0x06], "checkAppleCollision".to_string()          ); 
        instr.insert(vec![0x20, 0xa8, 0x06], "checkSnakeCollision".to_string()          ); 
        instr.insert(vec![0x20, 0xc3, 0x06], "updateSnake".to_string()                  ); 


        // jmp
        instr.insert(vec![0x4c, 0x38, 0x06], "loop".to_string());
        instr.insert(vec![0x4c, 0xaa, 0x06], "snakeCollisionLoop".to_string());
        instr.insert(vec![0x4c, 0x35, 0x07], "gameOver".to_string());


        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: Flag::ONE,
            program_counter: 0,
            stack_pointer: 0xff,
            memory: [0; 0xffff],

            cpu_cycle: 0,

            random_state: Vec::new(),
            fn_stack: Vec::new(),
            instr_addr_map: instr,
            last_jump: "".to_string(),
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
            // AddressingMode::NoneAddressing => panic!("mode {:?} is not supported", mode),
            AddressingMode::NoneAddressing => 0,
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        // self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        // self.mem_write_u16(0xFFFC, 0x8000)

        self.memory[0x0600..(0x0600 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x0600)
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    fn trace(&mut self) -> String {
        let opscode = self.mem_read(self.program_counter);
        let instr = &CPU_OPS_CODES[opscode as usize];
        let opscode1 = opscode;
        let opscode2 : String= if instr.bytes > 1 {
            let raw_opscode2 = self.mem_read(self.program_counter+1);
            format!("{:02X}", raw_opscode2)
        } else {
            String::from("  ")
        };
        let opscode3 : String= if instr.bytes > 2 {
            let raw_opscode3 = self.mem_read(self.program_counter+2);
            format!("{:02X}", raw_opscode3)
        } else {
            String::from("  ")
        };
        let instr_arg = match instr.addressing_mode {
            AddressingMode::Immediate => {
                let val = self.mem_read(self.program_counter+1);
                format!("#${:02X}", val)
            },
            AddressingMode::ZeroPage => {
                let val = self.mem_read(self.program_counter+1);
                format!("${:02X}", val)
            },
            AddressingMode::ZeroPageX => {
                let val = self.mem_read(self.program_counter+1);
                format!("${:02X},X", val)
            },
            AddressingMode::ZeroPageY => {
                let val = self.mem_read(self.program_counter+1);
                format!("${:02X},Y", val)
            },
            AddressingMode::Absolute => {
                let lo = self.mem_read(self.program_counter+1);
                let hi = self.mem_read(self.program_counter+2);
                format!("${:02X}{:02X}", hi, lo)
            },
            AddressingMode::AbsoluteX => {
                let lo = self.mem_read(self.program_counter+1);
                let hi = self.mem_read(self.program_counter+2);
                format!("${:02X}{:02X},X", hi, lo)
            },
            AddressingMode::AbsoluteY => {
                let lo = self.mem_read(self.program_counter+1);
                let hi = self.mem_read(self.program_counter+2);
                format!("${:02X}{:02X},Y", hi, lo)
            },
            AddressingMode::IndirectX => {
                let lo = self.mem_read(self.program_counter+1);
                format!("(${:02X},X)", lo)
            },
            AddressingMode::IndirectY => {
                let lo = self.mem_read(self.program_counter+1);
                format!("(${:02X}),Y", lo)
            },
            AddressingMode::Indirect => {
                let lo = self.mem_read(self.program_counter+1);
                let hi = self.mem_read(self.program_counter+2);
                format!("(${:02X}{:02X})", hi, lo)
            },
            AddressingMode::Relative => {
                let val = self.mem_read(self.program_counter+1);
                format!("${:02X}", val)
            },
            AddressingMode::NoneAddressing => String::from(""),
        };
        // "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
        // "DEY                             "
        let mut instr_placeholder = instr.instr.to_uppercase().to_string();
        instr_placeholder.push_str(" ");
        instr_placeholder.push_str(instr_arg.as_str());

        if instr.instr == "RTS" {
            self.fn_stack.pop();
            println!("end: {:?}", self.fn_stack.join(" -> "))
        }

        if instr.instr == "JSR" {
            let pc = self.program_counter as usize;
            let label = self.instr_addr_map.get(&(self.memory[pc..pc+3]).to_vec()).unwrap();
            self.fn_stack.push(label.to_string());
            println!("\nenter: {:?}", self.fn_stack.join(" -> "))
        }

        if instr.instr == "JMP" {
            let pc = self.program_counter as usize;
            let label = self.instr_addr_map.get(&(self.memory[pc..pc+3]).to_vec()).unwrap();
            self.last_jump = label.to_string();
            println!("\nJUMP TO: {:?}", label);
        }

        // print_screen_state(self);
        // print_snake_positions(self);
        check_snake_change(self);

        // let start = if instr.instr == "JSR" || instr.instr == "JMP" {"\n"} else {""};
        let end = if instr.instr == "RTS" {"\n"} else {""};
        // return format!("");
        format!(
            "{}{:04X} {:02X} {} {} {: <32}A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} {}{}\n",
            "",
            self.program_counter,
            opscode1,
            opscode2,
            opscode3,
            instr_placeholder,
            self.register_a,
            self.register_x,
            self.register_y,
            self.status.bits(),
            self.stack_pointer,
            print_game_state(self),
            end,
        )
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            callback(self);
            // thread::sleep(Duration::from_millis(1));
            let opscode = self.mem_read(self.program_counter);
            let instr = &CPU_OPS_CODES[opscode as usize];
            if opscode != instr.opcode {
                panic!(
                    "error {} not registered, should be {}",
                    opscode, instr.instr
                );
            }
            print!("{}", self.trace());
            self.program_counter += 1;
            self.cpu_cycle += 1;

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
                    // bcc -> carry clear
                    self.branch_if_flag_status(Flag::CARRY, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xb0 => {
                    // bcc -> carry set
                    self.branch_if_flag_status(Flag::CARRY, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xf0 => {
                    // beq -> zero set
                    self.branch_if_flag_status(Flag::ZERO, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0xd0 => {
                    // beq -> zero clear
                    self.branch_if_flag_status(Flag::ZERO, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x30 => {
                    // bmi -> negative set
                    self.branch_if_flag_status(Flag::NEGATIVE, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x10 => {
                    // bpl -> negative clear
                    self.branch_if_flag_status(Flag::NEGATIVE, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x50 => {
                    // bvc -> overflow clear
                    self.branch_if_flag_status(Flag::OVERFLOW, false);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x70 => {
                    // bvs -> overflow set
                    self.branch_if_flag_status(Flag::OVERFLOW, true);
                    self.program_counter += instr.bytes as u16 - 1;
                }
                0x4c | 0x6c => self.jmp(&instr.addressing_mode),

                0x20 => self.jsr(&instr.addressing_mode),
                0x60 => self.rts(),

                // Interrupts
                // 0x00 => self.brk(),
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

                _ => todo!("{opscode} not implemented at {}", self.program_counter),
            }
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = Flag::empty();
        self.program_counter = self.mem_read_u16(0xFFFC);
        self.stack_pointer = 0xff;
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
            // self.status.insert(Flag::OVERFLOW);
        }
        self.status.set(Flag::OVERFLOW, (result & 0x80) == 0x80);
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
        self.status
            .set(Flag::NEGATIVE, (reg_val.wrapping_sub(value) & 0x80) == 0x80);
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
            // self.status.insert(Flag::OVERFLOW);
        }
        self.status.set(Flag::OVERFLOW, (result & 0x80) == 0x80);
        self.set_zero_and_negative_status_flag(self.register_a);
    }

    // Control Flow
    fn branch_if_flag_status(&mut self, flag: Flag, is_set: bool) {
        if self.status.contains(flag) == is_set {
            self.program_counter = self
                .program_counter
                .wrapping_add_signed((self.mem_read(self.program_counter) as i8) as i16);
        }
    }
    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.program_counter = addr;
    }
    fn jsr(&mut self, mode: &AddressingMode) {
        self.mem_write_u16(
            self.stack_pointer as u16 - 1 + 0x100,
            self.program_counter + 2,
        );
        self.stack_pointer -= 2;

        let addr = self.get_operand_address(mode);
        self.program_counter = addr;
    }
    fn rts(&mut self) {
        self.stack_pointer = self.stack_pointer + 2;
        self.program_counter = self.mem_read_u16(self.stack_pointer as u16 - 1 + 0x100);
    }

    // Interrupts
    fn brk(&mut self) {
        self.mem_write_u16(self.stack_pointer as u16 - 1 + 0x100, self.program_counter);
        self.mem_write(
            self.stack_pointer as u16 - 2 + 0x100,
            (self.status | Flag::BREAK_CMD).bits(),
        );
        self.stack_pointer -= 3;

        self.program_counter = self.mem_read_u16(0xFFFd);
        self.status.insert(Flag::BREAK_CMD);
    }
    fn rti(&mut self) {
        self.plp();
        self.stack_pointer += 2;
        self.program_counter = self.mem_read_u16(self.stack_pointer as u16 - 1 + 0x100);
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
        self.mem_write(self.stack_pointer as u16 + 0x100, self.register_a);
        self.stack_pointer -= 1;
    }
    fn php(&mut self) {
        self.mem_write(
            self.stack_pointer as u16 + 0x100,
            (self.status | Flag::BREAK_CMD).bits(),
        );
        self.stack_pointer -= 1;
    }
    fn pla(&mut self) {
        self.stack_pointer += 1;
        self.register_a = self.mem_read(self.stack_pointer as u16 + 0x100);
        self.set_zero_and_negative_status_flag(self.register_a);
    }
    fn plp(&mut self) {
        self.stack_pointer += 1;
        let result = self.mem_read(self.stack_pointer as u16 + 0x100);
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
    // let input = vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00];
    //
    // absolutes: 
    // jsr
    // jmp 0x4c
    //
    // branchings: relative
    //
    //
    let input = vec![0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02, 0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9, 0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0, 0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60, 0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0, 0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9, 0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6, 0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10, 0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea, 0xea, 0xca, 0xd0, 0xfb, 0x60];

    let _input = vec![
        0x20, 0x06, 0x06, // jsr init
        0x20, 0x38, 0x06, // jsr loop

        // init
        0x20, 0x0d, 0x06, // jsr initSnake
        0x20, 0x2a, 0x06, // jsr generateApplePossition
        0x60, //rts

        // init snake
        0xa9, 0x02, // lda
        0x85, 0x02, // sta
        0xa9, 0x04, // lda
        0x85, 0x03, // sta
        0xa9, 0x11, // lda
        0x85, 0x10, // sta
        0xa9, 0x10, // lda
        0x85, 0x12, // sta
        0xa9, 0x0f, // lda
        0x85, 0x14, // sta
        0xa9, 0x04, // lda
        0x85, 0x11, // sta
        0x85, 0x13, // sta
        0x85, 0x15, // sta

        0x60, // rts
        
        // generate apple possition
        0xa5, 0xfe, 0x85,
        0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01,
        0x60, //rts

        0x20, 0x4d, 0x06, // jsr readKeys
        0x20, 0x8d, 0x06, // jsr checkCollision
        0x20, 0xc3, 0x06, // jsr updateSnake
        0x20, 0x19, 0x07, // jsr drawApple
        0x20, 0x20, 0x07, // jsr drawSnake
        0x20, 0x2d, 0x07, // jsr spinWheels

        0x4c, 0x38, 0x06, // jmp loop 

        // readkeys
        0xa5, 0xff, // lda
        0xc9, 0x77, // cmp
        0xf0, 0x0d, // beq upKey
        0xc9, 0x64, // cmp
        0xf0, 0x14, // beq rightKey
        0xc9, 0x73, // cmp
        0xf0, 0x1b, // beq downKey
        0xc9, 0x61, // cmp
        0xf0, 0x22, // beq leftKey
        0x60, // rts
        
        // upkey
        0xa9, 0x04, 0x24, 0x02,
        0xd0, 0x26, // bne illegalMove
        0xa9, 0x01, 0x85, 0x02,
        0x60, // rts

        // rightKey
        0xa9, 0x08, 0x24, 0x02, 
        0xd0, 0x1b, // bne illegalMove
        0xa9, 0x02, 0x85, 0x02, 
        0x60, // rts

        // downKey
        0xa9, 0x01, 0x24, 0x02, 
        0xd0, 0x10, // bne illegalMove
        0xa9, 0x04, 0x85, 0x02, 
        0x60, // rts

        // leftKey
        0xa9, 0x02, 0x24, 0x02, 
        0xd0, 0x05, //bne illegalMove
        0xa9, 0x08, 0x85, 0x02, 
        0x60, // rts

        // illegalMove
        0x60, // rts

        // checkCollision:
        0x20, 0x94, 0x06, // jsr checkAppleCollision
        0x20, 0xa8, 0x06, // jsr checkSnakeCollision
        0x60, // rts

        // checkAppleCollision
        0xa5, 0x00, // lda
        0xc5, 0x10, // cmp
        0xd0, 0x0d, // bne doneCheckingAppleCollision
        0xa5, 0x01, // lda
        0xc5, 0x11, // cmp
        0xd0, 0x07, // bne doneCheckingAppleCollision
        0xe6, 0x03, // inc
        0xe6, 0x03, // inc

        0x20, 0x2a, 0x06, //jsr generateApplePosition

        // doneCheckingAppleCollision
        0x60,

        // checkSnakeCollision
        0xa2, 0x02, 

        // snakeCollisionLoop
        0xb5, 0x10, 0xc5, 0x10, 
        0xd0, 0x06, // bne continueCollisionLoop

        // maybeCollidded
        0xb5, 0x11,
        0xc5, 0x11,
        0xf0, 0x09, // beq didCollide

        // continueCollisionLoop
        0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 
        0x4c, 0xaa, 0x06, // jmp snakeCollisionLoop

        // didCollide
        0x4c, 0x35, 0x07, //jmp gameOver

        // didntCollide
        0x60, //rts

        // updateSnake
        0xa6, 0x03, 0xca, 0x8a, 

        // updateLoop
        0xb5, 0x10, // lda
        0x95, 0x12, // sta
        0xca, //dex
        0x10, 0xf9, // bpl updateLoop
        0xa5, 0x02, // lda
        0x4a, // lsr
        0xb0, 0x09, // bcs up
        0x4a, // lsr
        0xb0, 0x19, // bcs right
        0x4a, // lsr
        0xb0, 0x1f, //bcs down
        0x4a, // lsr
        0xb0, 0x2f, //bcs left

        // up
        0xa5, 0x10, 0x38, 0xe9, 0x20,
        0x85, 0x10, 
        0x90, 0x01, // bcc upup
        0x60, // rts

        // upup
        0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11,
        0xf0, 0x28,  // beq collision
        0x60, // rts

        // right
        0xe6,
        0x10, 0xa9, 0x1f, 0x24, 0x10, 
        0xf0, 0x1f, // beq collision
        0x60, // rts

        // down
        0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10,
        0xb0, 0x01, //bcs downdown
        0x60, // rts

        // downdown
        0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 
        0xf0, 0x0c, // beq collision
        0x60, // rts

        // left
        0xc6, 0x10, 0xa5, 0x10, 0x29, 0x1f, 0xc9, 0x1f, 
        0xf0, 0x01, // beq collision
        0x60, // rts

        // collision
        0x4c, 0x35, 0x07, // jmp gameOver

        // drawApple
        0xa0, 0x00, 0xa5, 0xfe,
        0x91, 0x00,
        0x60, // rts

        // drawSnake
        0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10,
        0x60, // rts

        // spinWheels
        0xa2, 0x00, // lda

        // spinLoop
        0xea, // nop
        0xea, // nop
        0xca, // dex
        0xd0, 0xfb, // bne spinLoop
        0x60, // rts
    ];

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("snake game", (32.0 * 10.0) as u32, (32.0 * 10.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();

    let mut screen_state = [0 as u8; 32 * 3 * 32];
    let mut rng = rand::thread_rng();

    let mut cpu = CPU::new();
    cpu.load(input);
    cpu.reset();

    cpu.run_with_callback(move |cpu| {
        handle_user_input(cpu, &mut event_pump);
        cpu.mem_write(0xfe, rng.gen_range(1..16));

        if read_screen_state(cpu, &mut screen_state) {
            texture.update(None, &screen_state, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        ::std::thread::sleep(std::time::Duration::new(0, 70_000));
    });
}

fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => {}
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => {
                cpu.mem_write(0xff, 0x77);
            }
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => {
                cpu.mem_write(0xff, 0x73);
            }
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => {
                cpu.mem_write(0xff, 0x61);
            }
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => {
                cpu.mem_write(0xff, 0x64);
            }
            _ => {}
        }
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => sdl2::pixels::Color::BLACK,
        1 => sdl2::pixels::Color::WHITE,
        2 | 9 => sdl2::pixels::Color::GREY,
        3 | 10 => sdl2::pixels::Color::RED,
        4 | 11 => sdl2::pixels::Color::GREEN,
        5 | 12 => sdl2::pixels::Color::BLUE,
        6 | 13 => sdl2::pixels::Color::MAGENTA,
        7 | 14 => sdl2::pixels::Color::YELLOW,
        _ => sdl2::pixels::Color::CYAN,
    }
}

fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32 * 3 * 32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        let (b1, b2, b3) = color(color_idx).rgb();

        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true;
        }

        frame_idx += 3;
    }
    update
}

fn print_snake_positions(cpu: &CPU) {
    print!("{}: ", cpu.cpu_cycle);
    for i in 0x200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        let idx = i - 0x200;
        if color_idx == 1{
            print!("[{:X} {} {} {}] ",i,  idx, idx/32, idx%32);
        }
    }
    // println!("");
}


fn check_snake_change(cpu: &mut CPU) {
    let snake_length = cpu.mem_read(0x03);
    let snake_body = &cpu.memory[0x10..(0x10+snake_length) as usize];
    // let curr_snake = snake_body.to_vec().into_iter().map(|x| x as u16).collect::<Vec<u16>>().to_vec();
    let curr_snake = snake_body;

    if !curr_snake.eq(&cpu.random_state) {
        println!("{}: changed from {} to {}", cpu.cpu_cycle, format_vec(&cpu.random_state), format_vec(&curr_snake.to_vec()));
        // print_screen_state(cpu);
        cpu.random_state = curr_snake.to_vec();
    }

    // for i in 0x200..0x600 {
    //     let color_idx = cpu.mem_read(i as u16);
    //     let idx = i - 0x200;
    //     if color_idx == 1{
    //         curr_snake.push(idx);
    //     }
    // }
    // if !curr_snake.eq(&cpu.random_state) {
    //     println!("{}: changed from {:?} to {:?}", cpu.cpu_cycle, cpu.random_state, curr_snake);
    //     cpu.random_state = curr_snake;
    // }

}

fn print_game_state(cpu: &CPU) -> String {
    
// ; $00-01 => screen location of apple, stored as two bytes, where the first
// ;           byte is the least significant.
// ; $10-11 => screen location of snake head stored as two bytes
// ; $12-?? => snake body (in byte pairs)
// ; $02    => direction ; 1 => up    (bin 0001)
//                       ; 2 => right (bin 0010)
//                       ; 4 => down  (bin 0100)
//                       ; 8 => left  (bin 1000)
// ; $03    => snake length, in number of bytes, not segments
    let snake_head = cpu.mem_read_u16(0x10);
    let snake_length = cpu.mem_read(0x03);
    let snake_dir = cpu.mem_read(0x02);
    let snake_body = &cpu.memory[0x10..(0x10+snake_length) as usize];

    format!("h:{:04X} l:{} d:{} b:{}", snake_head, snake_length, snake_dir, format_vec(&snake_body.to_vec()))
}

fn format_vec(v: &Vec<u8>) -> String {
    let mut addr: Vec<String> = Vec::new();
    for i in 0..((v.len()/2) as usize) {
        addr.push(format!("{:02X}{:02X}", v[i+1], v[i]))
    }
    format!("[{}]", addr.join(", "))
}

fn print_screen_state(cpu: &CPU) {
    for i in 0x200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        print!("{:X}", color_idx);
        if (i-0x200+1) % 32 == 0 {
            println!("");
        }
    }
    println!("");
}

#[cfg(test)]
mod test {
    use super::*;

    // test todo
    // [x] adc
    // [ ] bcc
    // [ ] bcs
    // [ ] beq
    // [ ] bit
    // [ ] bpl
    // [ ] clc
    // [x] cmp
    // [ ] dec
    // [ ] dex
    // [ ] inc
    // [ ] lda
    // [ ] lsr
    // [ ] rts
    // [ ] sbc
    // [ ] sec
    // [ ] sta

    #[test]
    fn test_adc_positive_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x7f, 0x69, 0x01]);
        assert_eq!(cpu.status.contains(Flag::OVERFLOW), true);
    }

    #[test]
    fn test_adc_negative_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x7f, 0x69, 0x80]);
        assert_eq!(cpu.status.contains(Flag::OVERFLOW), true);
    }

    #[test]
    fn test_adc_no_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x40, 0x69, 0x20]);
        assert_eq!(cpu.status.contains(Flag::OVERFLOW), false);
    }

    #[test]
    fn test_asl_shift_left() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x40, 0x0a]);
        assert_eq!(cpu.register_a, 0x80);
    }

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
    fn test_0x16_asl() {
        let mut cpu = CPU::new();
        // LDA #$20
        // STA $16
        // LDX #$06
        // ASL $10,X
        // BRK
        cpu.load_and_run(vec![0xa9, 0x20, 0x85, 0x16, 0xa2, 0x06, 0x16, 0x10, 0x00]);
        assert_eq!(cpu.memory[0x16], 0x40);
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

    // LDA #$20
    // EOR #$10
    // BRK
    #[test]
    fn test_0x49_eor_xor() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x20, 0x49, 0x10, 0x00]);
        assert_eq!(cpu.register_a, 0x30);
    }

    // LDA #$20
    // LSR
    // BRK
    #[test]
    fn test_0x4a_lsr_logical_shift_right_accumulator() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x20, 0x4a, 0x00]);
        assert_eq!(cpu.register_a, 0x10);
    }

    // LDA #$20
    // STA $0020
    // LSR $0020
    // BRK
    #[test]
    fn test_0x4e_lsr_logical_shift_right_absolute() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x20, 0x8d, 0x20, 0x00, 0x4e, 0x20, 0x00, 0x00]);
        assert_eq!(cpu.memory[0x0020], 0x10);
    }

    // LDA #$60
    // ORA #$30
    // BRK
    #[test]
    fn test_0x09_ora_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x60, 0x09, 0x30, 0x00]);
        assert_eq!(cpu.register_a, 0x70);
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

    #[test]
    fn test_jump() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![
            0xa9, 0x03, 0x4c, 0x08, 0x06, 0x00, 0x00, 0x00, 0x8d, 0x00, 0x02,
        ]);
        assert_eq!(cpu.register_a, 0x03);
    }

    #[test]
    fn test_addressing_relative() {
        let mut cpu = CPU::new();
        /*
        LDA #$01
        CMP #$02
        BNE notequal
        STA $22
        notequal:
        BRK
        */
        cpu.load_and_run(vec![0xa9, 0x01, 0xc9, 0x02, 0xd0, 0x02, 0x85, 0x22, 0x00]);
        assert_eq!(cpu.register_a, 0x01);
    }

    #[test]
    fn test_0xc6_dec_decrement() {
        let mut cpu = CPU::new();
        /*
        LDA #$10
        STA $20
        DEC $20
        BRK
        */
        cpu.load_and_run(vec![0xa9, 0x10, 0x85, 0x20, 0xc6, 0x20, 0x00]);
        assert_eq!(cpu.memory[0x20], 0x0f);
    }

    #[test]
    fn test_jsr_rts() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![
            0x20, 0x09, 0x06, // jsr init
            0x20, 0x0c, 0x06, // jsr look
            0x20, 0x12, 0x06, // jsr end
            // init:
            0xa2, 0x00, // ldx #$00
            0x60, //rts
            // loop:
            0xe8, // inx
            0xe0, 0x05, // cpx #$05
            0xd0, 0xfb, // bne loop
            0x60, //rts
            // end:
            0x00,
        ]);
        assert_eq!(cpu.register_x, 0x05);
    }

    #[test]
    fn test_stack() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![
            0xa2, 0x00, 0xa0, 0x00, 0x8a, 0x99, 0x00, 0x02, 0x48, 0xe8, 0xc8, 0xc0, 0x10, 0xd0,
            0xf5, 0x68, 0x99, 0x00, 0x02, 0xc8, 0xc0, 0x20, 0xd0, 0xf7,
        ]);

        for i in 0x00..0x10 {
            println!("{} {}", cpu.memory[(0x0200 as u16 + i as u16) as usize], i);
            assert_eq!(cpu.memory[(0x0200 as u16 + i as u16) as usize], i);
        }
        for i in 0x00..0x10 {
            println!(
                "{} {}",
                cpu.memory[(0x0210 as u16 + i as u16) as usize],
                0x0f - i
            );
            assert_eq!(cpu.memory[(0x0210 as u16 + i as u16) as usize], 0x0f - i);
        }
    }
}
