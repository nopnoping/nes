use bitflags::bitflags;
use crate::ram::RAM;
use crate::asm::{AddressingMode, OpCode};
use crate::asm::ASM;

bitflags! {
    pub struct CpuFlags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL_MODE      = 0b00001000;
        const BREAK             = 0b00010000;
        const BREAK2            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIV           = 0b10000000;
    }
}
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    ram: RAM,
}


impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            ram: RAM::new(),
        }
    }

    // main
    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        self.program_counter = self.ram.read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.ram.write_program(0x8000, program);
        self.ram.write_u16(0xFFFC, 0x8000);
    }

    pub fn run(&mut self) {
        loop {
            let code = self.ram.read(self.program_counter);
            let asm = ASM::compile_opcode(code);
            let len = asm.get_len();

            self.program_counter += 1;
            let program_counter_cache = self.program_counter;

            match asm {
                ASM::ADC(op_code) => {}

                ASM::AND(op_code) => self.and(&op_code.mode),
                ASM::EOR(op_code) => self.eor(&op_code.mode),
                ASM::ORA(op_code) => self.ora(&op_code.mode),

                ASM::ASL(op_code) => self.asl(&op_code.mode),
                ASM::LSR(op_code) => self.lsr(&op_code.mode),
                ASM::ROL(op_code) => self.rol(&op_code.mode),
                ASM::ROR(op_code) => self.ror(&op_code.mode),

                ASM::BCC(_) => self.bcc(),
                ASM::BCS(_) => self.bcs(),
                ASM::BEQ(_) => {}
                ASM::BIT(_) => {}
                ASM::BMI(_) => {}
                ASM::BNE(_) => {}
                ASM::BPL(_) => {}
                ASM::BVC(_) => {}
                ASM::BVS(_) => {}
                ASM::CLC(_) => {}
                ASM::CLD(_) => {}
                ASM::CLI(_) => {}
                ASM::CLV(_) => {}
                ASM::CMP(_) => {}
                ASM::CPX(_) => {}
                ASM::CPY(_) => {}
                ASM::DEC(_) => {}
                ASM::DEX(_) => {}
                ASM::DEY(_) => {}
                ASM::INC(_) => {}
                ASM::INX(_) => self.inx(),
                ASM::INY(_) => {}
                ASM::JMP(_) => {}
                ASM::JSR(_) => {}
                ASM::LDA(op_code) => self.lda(&op_code.mode),
                ASM::LDX(_) => {}
                ASM::LDY(_) => {}
                ASM::NOP(_) => {}
                ASM::PHA(_) => {}
                ASM::PHP(_) => {}
                ASM::PLA(_) => {}
                ASM::PLP(_) => {}
                ASM::RTI(_) => {}
                ASM::RTS(_) => {}
                ASM::SBC(_) => {}
                ASM::SEC(_) => {}
                ASM::SED(_) => {}
                ASM::SEI(_) => {}
                ASM::STA(op_code) => self.sta(&op_code.mode),
                ASM::STX(_) => {}
                ASM::STY(_) => {}
                ASM::TAX(_) => self.tax(),
                ASM::TAY(_) => {}
                ASM::TSX(_) => {}
                ASM::TXA(_) => {}
                ASM::TXS(_) => {}
                ASM::TYA(_) => {}
                ASM::BRK(_) => return,
            }

            if program_counter_cache == self.program_counter {
                self.program_counter += (len - 1) as u16;
            }
        }
    }
}

impl CPU {
    // asm fn
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_a |= value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        let mut data;
        let mut addr = 0;
        match mode {
            AddressingMode::NoneAddressing => {
                data = self.register_a;
            }
            _ => {
                addr = self.get_operand_address(mode);
                data = self.ram.read(addr);
            }
        }

        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data = data << 1;
        match mode {
            AddressingMode::NoneAddressing => self.register_a = data,
            _ => self.ram.write(addr, data)
        }
        self.update_zero_and_negative_flags(data);
    }
    fn lsr(&mut self, mode: &AddressingMode) {
        let mut data;
        let mut addr = 0;
        match mode {
            AddressingMode::NoneAddressing => {
                data = self.register_a;
            }
            _ => {
                addr = self.get_operand_address(mode);
                data = self.ram.read(addr);
            }
        }

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data = data >> 1;
        match mode {
            AddressingMode::NoneAddressing => self.register_a = data,
            _ => self.ram.write(addr, data)
        }
        self.update_zero_and_negative_flags(data);
    }
    fn rol(&mut self, mode: &AddressingMode) {
        let mut data;
        let mut addr = 0;
        match mode {
            AddressingMode::NoneAddressing => {
                data = self.register_a;
            }
            _ => {
                addr = self.get_operand_address(mode);
                data = self.ram.read(addr);
            }
        }

        let last_carry = self.status.contains(CpuFlags::CARRY);

        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data = data << 1;
        if last_carry {
            data |= 1;
        }
        match mode {
            AddressingMode::NoneAddressing => self.register_a = data,
            _ => self.ram.write(addr, data)
        }
        self.update_zero_and_negative_flags(data);
    }
    fn ror(&mut self, mode: &AddressingMode) {
        let mut data;
        let mut addr = 0;
        match mode {
            AddressingMode::NoneAddressing => {
                data = self.register_a;
            }
            _ => {
                addr = self.get_operand_address(mode);
                data = self.ram.read(addr);
            }
        }

        let last_carry = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data = data >> 1;
        if last_carry {
            data |= 0x80;
        }
        match mode {
            AddressingMode::NoneAddressing => self.register_a = data,
            _ => self.ram.write(addr, data)
        }
        self.update_zero_and_negative_flags(data);
    }

    fn bcc(&mut self) {
        self.branch(!self.status.contains(CpuFlags::CARRY));
    }
    fn bcs(&mut self) {
        self.branch(self.status.contains(CpuFlags::CARRY));
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.ram.write(addr, self.register_a)
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    // help fn
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if result & 0b1000_0000 != 0 {
            self.status.insert(CpuFlags::NEGATIV);
        } else {
            self.status.remove(CpuFlags::NEGATIV);
        }
    }

    fn set_carry_flag(&mut self) {
        self.status.insert(CpuFlags::CARRY);
    }

    fn clear_carry_flag(&mut self) {
        self.status.remove(CpuFlags::CARRY);
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.ram.read(self.program_counter) as u16;
            let jump_addr = self.program_counter
                .wrapping_add(1)
                .wrapping_add(jump);
            self.program_counter = jump_addr;
        }
    }
    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.ram.read(self.program_counter) as u16,
            AddressingMode::Absolute => self.ram.read_u16(self.program_counter),
            AddressingMode::ZeroPage_X => {
                let pos = self.ram.read(self.program_counter);
                pos.wrapping_add(self.register_x) as u16
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.ram.read(self.program_counter);
                pos.wrapping_add(self.register_y) as u16
            }
            AddressingMode::Absolute_X => {
                let base = self.ram.read_u16(self.program_counter);
                base.wrapping_add(self.register_x as u16)
            }
            AddressingMode::Absolute_Y => {
                let base = self.ram.read_u16(self.program_counter);
                base.wrapping_add(self.register_y as u16)
            }
            AddressingMode::Indirect_X => {
                let base = self.ram.read(self.program_counter);
                let ptr = base.wrapping_add(self.register_x);
                let lo = self.ram.read(ptr as u16);
                let hi = self.ram.read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.ram.read(self.program_counter);
                let ptr = base.wrapping_add(self.register_y);
                let lo = self.ram.read(ptr as u16);
                let hi = self.ram.read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status.bits() & 0b0000_0010, 0);
        assert_eq!(cpu.status.bits() & 0b1000_0000, 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.status.bits() & 0b0000_0010, 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.ram.write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_and() {
        let mut cpu = CPU::new();

        cpu.ram.write(0x10, 0x20);

        cpu.load_and_run(vec![0xa5, 0x10, 0x29, 0x21, 0x00]);

        assert_eq!(cpu.register_a, 0x20);
    }

    #[test]
    fn test_ora() {
        let mut cpu = CPU::new();

        cpu.ram.write(0x10, 0x20);

        cpu.load_and_run(vec![0xa5, 0x10, 0x09, 0x21, 0x00]);

        assert_eq!(cpu.register_a, 0x20 | 0x21);
    }

    #[test]
    fn test_eor() {
        let mut cpu = CPU::new();

        cpu.ram.write(0x10, 0x20);

        cpu.load_and_run(vec![0xa5, 0x10, 0x49, 0x21, 0x00]);

        assert_eq!(cpu.register_a, 0x20 ^ 0x21);
    }

    #[test]
    fn test_asl() {
        let mut cpu = CPU::new();

        cpu.ram.write(0x10, 0x20);

        cpu.load_and_run(vec![0xa9, 0x10, 0x0a, 0x00]);
        assert_eq!(cpu.register_a, 0x10 << 1);
        cpu.load_and_run(vec![0x06, 0x10, 0xa5, 0x10, 0x00]);
        assert_eq!(cpu.register_a, 0x20 << 1);
    }
}