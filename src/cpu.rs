use bitflags::bitflags;
use crate::cpu::asm::AddressingMode;
use crate::cpu::asm::ASM;
use crate::bus::{Bus, Mem};
use crate::cpu::interrupt::{Interrupt, NMI};

pub mod trace;
mod asm;
mod interrupt;

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
const PROGRAM_BASE: u16 = 0x8600;
const PROGRAM_START_PTR: u16 = 0xFFFC;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Bus,
}

impl Mem for CPU {
    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data)
    }
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus,
        }
    }

    // main
    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        // self.program_counter = self.mem_read_u16(0xFFFC);
        self.program_counter = PROGRAM_BASE;
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.mem_write(PROGRAM_BASE + i, program[i as usize]);
        }
        // self.mem_write_u16(PROGRAM_START_PTR, PROGRAM_BASE);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
        where
            F: FnMut(&mut CPU),
    {
        loop {
            if let Some(_nmi) = self.bus.poll_nmi_status() {
                self.interrupt(&NMI);
            }
            callback(self);
            let code = self.mem_read(self.program_counter);
            let asm = ASM::compile_opcode(code);
            let op = asm.get_desc().1;
            let len = op.len;

            self.program_counter += 1;
            let program_counter_cache = self.program_counter;

            match &asm {
                ASM::AND(op_code) => self.and(&op_code.mode),
                ASM::EOR(op_code) => self.eor(&op_code.mode),
                ASM::ORA(op_code) => self.ora(&op_code.mode),

                ASM::ASL(op_code) => { self.asl(&op_code.mode); }
                ASM::LSR(op_code) => { self.lsr(&op_code.mode); }
                ASM::ROL(op_code) => { self.rol(&op_code.mode); }
                ASM::ROR(op_code) => { self.ror(&op_code.mode); }

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
                ASM::INC(op_code) => { self.inc(&op_code.mode); }
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

                ASM::NOP(_) | ASM::_NOP(_) => {}
                ASM::BRK(_) => return,

                // unofficial
                ASM::_DCP(op_code) => self._dcp(&op_code.mode),
                ASM::_RLA(op_code) => self._rla(&op_code.mode),
                ASM::_SLO(op_code) => self._slo(&op_code.mode),
                ASM::_SRE(op_code) => self._sre(&op_code.mode),
                ASM::_AXS(op_code) => self._axs(&op_code.mode),
                ASM::_ARR(op_code) => self._arr(&op_code.mode),
                ASM::_SBC(op_code) => self._sbc(&op_code.mode),
                ASM::_ANC(op_code) => self._anc(&op_code.mode),
                ASM::_ALR(op_code) => self._alr(&op_code.mode),
                ASM::_RRA(op_code) => self._rra(&op_code.mode),
                ASM::_ISB(op_code) => self._isb(&op_code.mode),
                ASM::_LXA(op_code) => self._lxa(&op_code.mode),
                ASM::_XAA(op_code) => self._xaa(&op_code.mode),
                ASM::_LAS(op_code) => self._las(&op_code.mode),
                ASM::_TAS(op_code) => self._tas(&op_code.mode),
                ASM::_AHX(op_code) => self._ahx(&op_code.mode),
                ASM::_SHX(op_code) => self._shx(&op_code.mode),
                ASM::_SHY(op_code) => self._shy(&op_code.mode),
                ASM::_LAX(op_code) => self._lax(&op_code.mode),
                ASM::_SAX(op_code) => self._sax(&op_code.mode),
            }

            self.bus.tick(op.cycles);
            if program_counter_cache == self.program_counter {
                self.program_counter += (len - 1) as u16;
            }
        }
    }
}

// asm
impl CPU {
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a |= value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) -> u8 {
        match mode {
            AddressingMode::NoneAddressing => {
                let mut data = self.register_a;
                self.status.set(CpuFlags::CARRY, data >> 7 == 1);
                data = data << 1;
                self.register_a = data;
                self.update_zero_and_negative_flags(self.register_a);
                data
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut data = self.mem_read(addr);
                self.status.set(CpuFlags::CARRY, data >> 7 == 1);
                data = data << 1;
                self.mem_write(addr, data);
                self.update_zero_and_negative_flags(data);
                data
            }
        }
    }
    fn lsr(&mut self, mode: &AddressingMode) -> u8 {
        match mode {
            AddressingMode::NoneAddressing => {
                let mut data = self.register_a;
                self.status.set(CpuFlags::CARRY, data & 1 == 1);
                data = data >> 1;
                self.register_a = data;
                self.update_zero_and_negative_flags(self.register_a);
                data
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut data = self.mem_read(addr);
                self.status.set(CpuFlags::CARRY, data & 1 == 1);
                data = data >> 1;
                self.mem_write(addr, data);
                self.update_zero_and_negative_flags(data);
                data
            }
        }
    }
    fn rol(&mut self, mode: &AddressingMode) -> u8 {
        match mode {
            AddressingMode::NoneAddressing => {
                let mut data = self.register_a;
                let old_carry = self.status.contains(CpuFlags::CARRY);

                self.status.set(CpuFlags::CARRY, data >> 7 == 1);
                data = (data << 1).wrapping_add(old_carry as u8);

                self.register_a = data;
                self.update_zero_and_negative_flags(data);
                data
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut data = self.mem_read(addr);
                let old_carry = self.status.contains(CpuFlags::CARRY);

                self.status.set(CpuFlags::CARRY, data >> 7 == 1);
                data = (data << 1).wrapping_add(old_carry as u8);

                self.mem_write(addr, data);
                self.status.set(CpuFlags::NEGATIV, data >> 7 == 1);
                data
            }
        }
    }
    fn ror(&mut self, mode: &AddressingMode) -> u8 {
        match mode {
            AddressingMode::NoneAddressing => {
                let mut data = self.register_a;
                let old_carry = self.status.contains(CpuFlags::CARRY);

                self.status.set(CpuFlags::CARRY, data & 1 == 1);
                data = data >> 1;
                if old_carry {
                    data |= 0b1000_0000;
                }

                self.register_a = data;
                self.update_zero_and_negative_flags(data);
                data
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut data = self.mem_read(addr);
                let old_carry = self.status.contains(CpuFlags::CARRY);

                self.status.set(CpuFlags::CARRY, data & 1 == 1);
                data = data >> 1;
                if old_carry {
                    data |= 0b1000_0000;
                }

                self.mem_write(addr, data);
                self.status.set(CpuFlags::NEGATIV, data >> 7 == 1);
                data
            }
        }
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
        let mut data = self.mem_read(addr);

        data = data.wrapping_sub(1);
        self.mem_write(addr, data);
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
    fn inc(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        data = data.wrapping_add(1);
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
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
                let pc = self.mem_read_u16(self.program_counter);
                self.program_counter = pc;
            }
            AddressingMode::Indirect => {
                let mem_address = self.mem_read_u16(self.program_counter);

                let indirect_ref = if mem_address & 0x00FF == 0x00FF {
                    let lo = self.mem_read(mem_address);
                    let hi = self.mem_read(mem_address & 0xFF00);
                    (hi as u16) << 8 | (lo as u16)
                } else {
                    self.mem_read_u16(mem_address)
                };

                self.program_counter = indirect_ref;
            }
            _ => {}
        }
    }
    fn jsr(&mut self) {
        self.stack_push_u16(self.program_counter + 2 - 1);
        let target_pc = self.mem_read_u16(self.program_counter);
        self.program_counter = target_pc;
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn pha(&mut self) { self.stack_push(self.register_a); }
    fn php(&mut self) {
        let mut flags = self.status.clone();
        flags.insert(CpuFlags::BREAK);
        flags.insert(CpuFlags::BREAK2);
        self.stack_push(flags.bits());
    }
    fn pla(&mut self) {
        self.register_a = self.stack_pop();
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn plp(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    fn rti(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);

        self.program_counter = self.stack_pop_u16();
    }
    fn rts(&mut self) {
        self.program_counter = self.stack_pop_u16() + 1;
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
        self.update_zero_and_negative_flags(self.register_x);
    }
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
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
    }
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
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
        let data = self.mem_read(addr);
        self.add_to_register_a(data);
    }
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8)
    }

    // unofficial
    fn _dcp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        data = data.wrapping_sub(1);
        self.mem_write(addr, data);
        if data <= self.register_a {
            self.status.insert(CpuFlags::CARRY);
        }

        self.update_zero_and_negative_flags(self.register_a.wrapping_sub(data));
    }
    fn _rla(&mut self, mode: &AddressingMode) {
        let data = self.rol(mode);
        self.and_with_register_a(data);
    }
    fn _slo(&mut self, mode: &AddressingMode) {
        let data = self.asl(mode);
        self.or_with_register_a(data);
    }
    fn _sre(&mut self, mode: &AddressingMode) {
        let data = self.lsr(mode);
        self.xor_with_register_a(data);
    }
    fn _skb(&mut self) {}
    fn _axs(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let x_and_a = self.register_x & self.register_a;
        let result = x_and_a.wrapping_sub(data);

        if data <= x_and_a {
            self.status.insert(CpuFlags::CARRY);
        }
        self.update_zero_and_negative_flags(result);

        self.register_x = result;
    }
    fn _arr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.and_with_register_a(data);
        self.ror(&AddressingMode::NoneAddressing);
        let result = self.register_a;
        let bit_5 = (result >> 5) & 1;
        let bit_6 = (result >> 6) & 1;

        if bit_6 == 1 {
            self.status.insert(CpuFlags::CARRY)
        } else {
            self.status.remove(CpuFlags::CARRY)
        }

        if bit_5 ^ bit_6 == 1 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW);
        }

        self.update_zero_and_negative_flags(result);
    }
    fn _sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.sub_from_register_a(data);
    }
    fn _anc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.and_with_register_a(data);
        if self.status.contains(CpuFlags::NEGATIV) {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }
    }
    fn _alr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.and_with_register_a(data);
        self.lsr(&AddressingMode::NoneAddressing);
    }
    fn _rra(&mut self, mode: &AddressingMode) {
        let data = self.ror(mode);
        self.add_to_register_a(data);
    }
    fn _isb(&mut self, mode: &AddressingMode) {
        let data = self.inc(mode);
        self.sub_from_register_a(data);
    }
    fn _lax(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register_a(data);
        self.register_x = self.register_a;
    }
    fn _sax(&mut self, mode: &AddressingMode) {
        let data = self.register_a & self.register_x;
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, data);
    }
    fn _lxa(&mut self, mode: &AddressingMode) {
        self.lda(mode);
        self.tax();
    }
    fn _xaa(&mut self, mode: &AddressingMode) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.and_with_register_a(data);
    }
    fn _las(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        data = data & self.stack_pointer;
        self.register_a = data;
        self.register_x = data;
        self.stack_pointer = data;
        self.update_zero_and_negative_flags(data);
    }
    fn _tas(&mut self, mode: &AddressingMode) {
        let data = self.register_a & self.register_x;
        self.stack_pointer = data;
        let mem_address =
            self.mem_read_u16(self.program_counter) + self.register_y as u16;

        let data = ((mem_address >> 8) as u8 + 1) & self.stack_pointer;
        self.mem_write(mem_address, data)
    }
    fn _ahx(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Absolute_Y => {
                let mem_address =
                    self.mem_read_u16(self.program_counter) + self.register_y as u16;

                let data = self.register_a & self.register_x & (mem_address >> 8) as u8;
                self.mem_write(mem_address, data)
            }
            AddressingMode::Indirect_Y => {
                let pos: u8 = self.mem_read(self.program_counter);
                let mem_address = self.mem_read_u16(pos as u16) + self.register_y as u16;
                let data = self.register_a & self.register_x & (mem_address >> 8) as u8;
                self.mem_write(mem_address, data)
            }
            _ => {}
        }
    }
    fn _shx(&mut self, mode: &AddressingMode) {
        let mem_address =
            self.mem_read_u16(self.program_counter) + self.register_y as u16;

        let data = self.register_x & ((mem_address >> 8) as u8 + 1);
        self.mem_write(mem_address, data)
    }
    fn _shy(&mut self, mode: &AddressingMode) {
        let mem_address =
            self.mem_read_u16(self.program_counter) + self.register_x as u16;
        let data = self.register_y & ((mem_address >> 8) as u8 + 1);
        self.mem_write(mem_address, data)
    }
}

// help
impl CPU {
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.set(CpuFlags::ZERO, result == 0);
        self.status.set(CpuFlags::NEGATIV, result & 0b1000_0000 > 0);
    }

    fn set_carry_flag(&mut self) { self.status.insert(CpuFlags::CARRY); }
    fn clear_carry_flag(&mut self) { self.status.remove(CpuFlags::CARRY); }
    fn and_with_register_a(&mut self, data: u8) {
        self.set_register_a(data & self.register_a);
    }
    fn xor_with_register_a(&mut self, data: u8) {
        self.set_register_a(data ^ self.register_a);
    }
    fn or_with_register_a(&mut self, data: u8) {
        self.set_register_a(data | self.register_a);
    }
    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn sub_from_register_a(&mut self, data: u8) {
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK + (self.stack_pointer as u16))
    }
    fn stack_push(&mut self, data: u8) {
        self.mem_write(STACK + (self.stack_pointer as u16), data);
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
        let data = self.mem_read(addr);

        if base >= data {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        self.update_zero_and_negative_flags(base.wrapping_sub(data));
    }
    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.mem_read(self.program_counter) as i8;
            let jump_addr = self.program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);
            self.program_counter = jump_addr;
        }
    }
    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            _ => self.get_absolute_address(mode, self.program_counter),
        }
    }
    pub fn get_absolute_address(&mut self, mode: &AddressingMode, addr: u16) -> u16 {
        match mode {
            AddressingMode::ZeroPage => self.mem_read(addr) as u16,
            AddressingMode::Absolute => self.mem_read_u16(addr),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(addr);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(addr);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(addr);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(addr);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(addr);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(addr);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            _ => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn interrupt(&mut self, interrupt: &Interrupt) {
        self.stack_push_u16(self.program_counter);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, interrupt.b_flag_mask & 0b010000 == 1);
        flag.set(CpuFlags::BREAK2, interrupt.b_flag_mask & 0b100000 == 1);

        self.stack_push(flag.bits);
        self.status.insert(CpuFlags::INTERRUPT_DISABLE);

        self.bus.tick(interrupt.cpu_cycles);
        self.program_counter = self.mem_read_u16(interrupt.vector_addr);
    }
}
