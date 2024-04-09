use bitflags::bitflags;
use crate::cpu::ram::RAM;
use crate::cpu::asm::AddressingMode;
use crate::cpu::asm::ASM;

mod asm;
mod ram;

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

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;
const PROGRAM_BASE: u16 = 0x600;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub ram: RAM,
    pub debug: bool,
}


impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_pointer: 0,
            ram: RAM::new(),
            debug: false,
        }
    }

    // main
    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        self.program_counter = self.ram.read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.ram.write_program(PROGRAM_BASE, program);
        self.ram.write_u16(0xFFFC, PROGRAM_BASE);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
        where
            F: FnMut(&mut CPU),
    {
        loop {
            let code = self.ram.read(self.program_counter);
            let asm = ASM::compile_opcode(code);
            let len = asm.get_len();

            self.program_counter += 1;
            let program_counter_cache = self.program_counter;

            match &asm {
                ASM::AND(op_code) => self.and(&op_code.mode),
                ASM::EOR(op_code) => self.eor(&op_code.mode),
                ASM::ORA(op_code) => self.ora(&op_code.mode),

                ASM::ASL(op_code) => self.asl(&op_code.mode),
                ASM::LSR(op_code) => self.lsr(&op_code.mode),
                ASM::ROL(op_code) => self.rol(&op_code.mode),
                ASM::ROR(op_code) => self.ror(&op_code.mode),

                ASM::BCC(_) => self.bcc(),
                ASM::BCS(_) => self.bcs(),
                ASM::BEQ(_) => self.beq(),
                ASM::BNE(_) => self.bne(),
                ASM::BMI(_) => self.bmi(),
                ASM::BPL(_) => self.bpl(),
                ASM::BVC(_) => self.bvc(),
                ASM::BVS(_) => self.bvs(),

                ASM::CLC(_) => self.clc(),
                ASM::SEC(_) => self.sec(),
                ASM::CLD(_) => self.cld(),
                ASM::SED(_) => self.sed(),
                ASM::CLI(_) => self.cli(),
                ASM::SEI(_) => self.sei(),
                ASM::CLV(_) => self.clv(),

                ASM::CMP(op_code) => self.cmp(&op_code.mode),
                ASM::CPX(op_code) => self.cpx(&op_code.mode),
                ASM::CPY(op_code) => self.cpy(&op_code.mode),

                ASM::DEC(op_code) => self.dec(&op_code.mode),
                ASM::INC(op_code) => self.inc(&op_code.mode),
                ASM::DEX(_) => self.dex(),
                ASM::INX(_) => self.inx(),
                ASM::DEY(_) => self.dey(),
                ASM::INY(_) => self.iny(),

                ASM::JMP(op_code) => self.jmp(&op_code.mode),
                ASM::JSR(_) => self.jsr(),

                ASM::LDA(op_code) => self.lda(&op_code.mode),
                ASM::LDX(op_code) => self.ldx(&op_code.mode),
                ASM::LDY(op_code) => self.ldy(&op_code.mode),

                ASM::PHA(_) => self.pha(),
                ASM::PHP(_) => self.php(),
                ASM::PLA(_) => self.pla(),
                ASM::PLP(_) => self.plp(),

                ASM::RTI(_) => self.rti(),
                ASM::RTS(_) => self.rts(),

                ASM::STA(op_code) => self.sta(&op_code.mode),
                ASM::STX(op_code) => self.stx(&op_code.mode),
                ASM::STY(op_code) => self.sty(&op_code.mode),

                ASM::TAX(_) => self.tax(),
                ASM::TAY(_) => self.tay(),
                ASM::TSX(_) => self.tsx(),
                ASM::TXA(_) => self.txa(),
                ASM::TXS(_) => self.txs(),
                ASM::TYA(_) => self.tya(),

                ASM::ADC(op_code) => self.adc(&op_code.mode),
                ASM::SBC(op_code) => self.sbc(&op_code.mode),
                ASM::BIT(op_code) => self.bit(&op_code.mode),

                ASM::NOP(_) => {}
                ASM::BRK(_) => return,
            }

            if program_counter_cache == self.program_counter {
                self.program_counter += (len - 1) as u16;
            }
            if self.debug {
                print!("{:?}\n", &asm);
            }
            callback(self);
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

    fn bcs(&mut self) { self.branch(self.status.contains(CpuFlags::CARRY)); }
    fn bcc(&mut self) { self.branch(!self.status.contains(CpuFlags::CARRY)); }
    fn beq(&mut self) { self.branch(self.status.contains(CpuFlags::ZERO)); }
    fn bne(&mut self) { self.branch(!self.status.contains(CpuFlags::ZERO)); }
    fn bmi(&mut self) { self.branch(self.status.contains(CpuFlags::NEGATIV)); }
    fn bpl(&mut self) { self.branch(!self.status.contains(CpuFlags::NEGATIV)); }
    fn bvs(&mut self) { self.branch(self.status.contains(CpuFlags::OVERFLOW)); }
    fn bvc(&mut self) { self.branch(!self.status.contains(CpuFlags::OVERFLOW)); }

    fn clc(&mut self) { self.clear_carry_flag(); }
    fn sec(&mut self) { self.set_carry_flag(); }
    fn cld(&mut self) { self.status.remove(CpuFlags::DECIMAL_MODE); }
    fn sed(&mut self) { self.status.insert(CpuFlags::DECIMAL_MODE); }
    fn cli(&mut self) { self.status.remove(CpuFlags::INTERRUPT_DISABLE); }
    fn sei(&mut self) { self.status.insert(CpuFlags::INTERRUPT_DISABLE); }
    fn clv(&mut self) { self.status.remove(CpuFlags::OVERFLOW); }

    fn cmp(&mut self, mode: &AddressingMode) { self.compare(mode, self.register_a); }
    fn cpx(&mut self, mode: &AddressingMode) { self.compare(mode, self.register_x); }
    fn cpy(&mut self, mode: &AddressingMode) { self.compare(mode, self.register_y); }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.ram.read(addr);

        data = data.wrapping_sub(1);
        self.ram.write(addr, data);
        self.update_zero_and_negative_flags(data);
    }
    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.ram.read(addr);

        data = data.wrapping_add(1);
        self.ram.write(addr, data);
        self.update_zero_and_negative_flags(data);
    }
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Absolute => {
                let pc = self.ram.read_u16(self.program_counter);
                self.program_counter = pc;
            }
            AddressingMode::Indirect => {
                let mem_address = self.ram.read_u16(self.program_counter);

                let indirect_ref = if mem_address & 0x00FF == 0x00FF {
                    let lo = self.ram.read(mem_address);
                    let hi = self.ram.read(mem_address & 0xFF00);
                    (hi as u16) << 8 | (lo as u16)
                } else {
                    self.ram.read_u16(mem_address)
                };

                self.program_counter = indirect_ref;
            }
            _ => {}
        }
    }
    fn jsr(&mut self) {
        self.stack_push_u16(self.program_counter + 2 - 1);
        let target_pc = self.ram.read_u16(self.program_counter);
        self.program_counter = target_pc;
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.ram.read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn pha(&mut self) { self.stack_push(self.register_a); }
    fn php(&mut self) { self.stack_push(self.status.bits()); }
    fn pla(&mut self) {
        self.register_a = self.stack_pop();
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn plp(&mut self) {
        self.status = CpuFlags::from_bits_truncate(self.stack_pop());
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    fn rti(&mut self) {
        self.status = CpuFlags::from_bits_truncate(self.stack_pop());
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);

        self.program_counter = self.stack_pop_u16();
    }
    fn rts(&mut self) {
        self.program_counter = self.stack_pop_u16() + 1;
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.ram.write(addr, self.register_a);
    }
    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.ram.write(addr, self.register_x);
    }
    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.ram.write(addr, self.register_y);
    }


    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
        self.update_zero_and_negative_flags(self.stack_pointer);
    }
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.ram.read(addr);
        let r = self.register_a & data;
        if r == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        self.status.set(CpuFlags::OVERFLOW, data & 0b0100_0000 > 0);
        self.status.set(CpuFlags::NEGATIV, data & 0b1000_0000 > 0);
    }
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.ram.read(addr);
        self.add_to_register_a(data);
    }
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.ram.read(addr);
        self.add_to_register_a((data as i8).wrapping_neg().wrapping_sub(1) as u8)
    }

    // help fn
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.set(CpuFlags::ZERO, result == 0);
        self.status.set(CpuFlags::NEGATIV, result & 0b1000_0000 > 0);
    }

    fn set_carry_flag(&mut self) { self.status.insert(CpuFlags::CARRY); }
    fn clear_carry_flag(&mut self) { self.status.remove(CpuFlags::CARRY); }
    fn set_zero_flag(&mut self) { self.status.insert(CpuFlags::ZERO); }
    fn clr_zero_flag(&mut self) { self.status.remove(CpuFlags::ZERO); }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.ram.read(STACK + self.stack_pointer as u16)
    }
    fn stack_push(&mut self, data: u8) {
        self.ram.write(STACK + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }
    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;
        hi << 8 | lo
    }
    fn stack_push_u16(&mut self, data: u16) {
        let lo = (data & 0xff) as u8;
        let hi = (data >> 8) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn add_to_register_a(&mut self, data: u8) {
        let sum = (self.register_a as u16)
            .wrapping_add(data as u16)
            .wrapping_add(self.status.contains(CpuFlags::CARRY) as u16);

        self.status.set(CpuFlags::CARRY, sum > 0xff);

        let r = sum as u8;
        self.status.set(CpuFlags::OVERFLOW, (r ^ self.register_a) & (r ^ data) & 0x80 != 0);

        self.register_a = r;
        self.update_zero_and_negative_flags(r);
    }
    fn compare(&mut self, mode: &AddressingMode, base: u8) {
        let addr = self.get_operand_address(mode);
        let data = self.ram.read(addr);

        if base >= data {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        self.update_zero_and_negative_flags(base.wrapping_sub(data));
    }
    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.ram.read(self.program_counter) as i8;
            let jump_addr = self.program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);
            self.program_counter = jump_addr;
        }
    }
    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.ram.read(self.program_counter) as u16,
            AddressingMode::Absolute => self.ram.read_u16(self.program_counter),
            AddressingMode::Indirect => 0,
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

                let lo = self.ram.read(base as u16);
                let hi = self.ram.read(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                deref_base.wrapping_add(self.register_y as u16)
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
