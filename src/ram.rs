pub struct RAM {
    pub memory: [u8; 0xFFFF]
}

impl RAM {
    pub fn new() -> RAM {
        RAM{
            memory: [0; 0xFFFF]
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.memory[addr as usize ]
    }
    pub fn write(& mut self, addr:u16, data:u8) {
        self.memory[addr as usize] = data
    }

    pub fn read_u16(&self, addr:u16) -> u16 {
        let lo = self.read(addr) as u16;
        let hi = self.read(addr.wrapping_add(1)) as u16;
        (hi << 8) | lo
    }

    pub fn write_u16(&mut self, addr:u16, data: u16) {
        let lo = (data & 0xff) as u8;
        let hi = (data >> 8) as u8;
        self.write(addr, lo);
        self.write(addr.wrapping_add(1), hi)
    }
}