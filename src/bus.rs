use crate::cartridges::Rom;
use crate::ppu::{NesPPU, PpuRegister};

pub struct Bus {
    cpu_ram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: NesPPU,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_ram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu: NesPPU::new(rom.chr_rom, rom.screen_mirroring),
        }
    }
}

pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}


const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

const CONTROLLER_REG: u16 = 0x2000;
const MASK_REG: u16 = 0x2001;
const STATUS_REG: u16 = 0x2002;
const OAM_ADDRESS_REG: u16 = 0x2003;
const OAM_DATA_REG: u16 = 0x2004;
const SCROLL_REG: u16 = 0x2005;
const ADDRESS_REG: u16 = 0x2006;
const DATA_REG: u16 = 0x2007;
const OAM_DMA_REG: u16 = 0x4014;


impl Mem for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_addr = addr & 0b00000111_11111111;
                self.cpu_ram[mirror_addr as usize]
            }

            CONTROLLER_REG | MASK_REG | OAM_ADDRESS_REG | SCROLL_REG | ADDRESS_REG | OAM_DMA_REG => {
                panic!("Attempt to read from write-only PPU address {:x}", addr);
            }

            STATUS_REG => self.ppu.read_status(),
            OAM_DATA_REG => self.ppu.read_oam_data(),
            DATA_REG => self.ppu.read_data(),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }
            0x8000..=0xffff => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem access at {}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_addr = addr & 0b00000111_11111111;
                self.cpu_ram[mirror_addr as usize] = data;
            }

            CONTROLLER_REG => self.ppu.write_to_ctl(data),
            MASK_REG => self.ppu.write_to_mask(data),
            STATUS_REG => panic!("attempt to write to PPU status register"),
            OAM_ADDRESS_REG => self.ppu.write_to_oam_addr(data),
            OAM_DATA_REG => self.ppu.write_to_oam_data(data),
            SCROLL_REG => self.ppu.write_to_scroll(data),
            ADDRESS_REG => self.ppu.write_to_ppu_addr(data),
            DATA_REG => self.ppu.write_to_data(data),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_write(mirror_down_addr, data);
            }
            0x8000..=0xffff => {
                panic!("Attempt to write to Cartridge ROM space")
            }
            _ => {
                println!("Ignoring mem access at {}", addr);
            }
        }
    }
}

impl Bus {
    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }
        self.prg_rom[addr as usize]
    }
}