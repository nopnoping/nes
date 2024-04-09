#[derive(Debug)]
#[allow(non_camel_case_types)]
#[derive(PartialEq)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub struct OpCode {
    pub code: u8,
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, len: u8, cycles: u8, mode: AddressingMode) -> OpCode {
        OpCode {
            code,
            len,
            cycles,
            mode,
        }
    }
}

pub enum ASM {
    ADC(OpCode),
    AND(OpCode),
    ASL(OpCode),
    BCC(OpCode),
    BCS(OpCode),
    BEQ(OpCode),
    BIT(OpCode),
    BMI(OpCode),
    BNE(OpCode),
    BPL(OpCode),
    BRK(OpCode),
    BVC(OpCode),
    BVS(OpCode),
    CLC(OpCode),
    CLD(OpCode),
    CLI(OpCode),
    CLV(OpCode),
    CMP(OpCode),
    CPX(OpCode),
    CPY(OpCode),
    DEC(OpCode),
    DEX(OpCode),
    DEY(OpCode),
    EOR(OpCode),
    INC(OpCode),
    INX(OpCode),
    INY(OpCode),
    JMP(OpCode),
    JSR(OpCode),
    LDA(OpCode),
    LDX(OpCode),
    LDY(OpCode),
    LSR(OpCode),
    NOP(OpCode),
    ORA(OpCode),
    PHA(OpCode),
    PHP(OpCode),
    PLA(OpCode),
    PLP(OpCode),
    ROL(OpCode),
    ROR(OpCode),
    RTI(OpCode),
    RTS(OpCode),
    SBC(OpCode),
    SEC(OpCode),
    SED(OpCode),
    SEI(OpCode),
    STA(OpCode),
    STX(OpCode),
    STY(OpCode),
    TAX(OpCode),
    TAY(OpCode),
    TSX(OpCode),
    TXA(OpCode),
    TXS(OpCode),
    TYA(OpCode),
}


impl ASM {
    pub fn compile_opcode(code: u8) -> ASM {
        match code {
            0x00 => ASM::BRK(OpCode::new(0x00, 1, 7, AddressingMode::NoneAddressing)),

            0xEA => ASM::NOP(OpCode::new(0xEA, 1, 2, AddressingMode::NoneAddressing)),

            0x69 => ASM::ADC(OpCode::new(0x69, 2, 2, AddressingMode::Absolute)),
            0x65 => ASM::ADC(OpCode::new(0x65, 2, 3, AddressingMode::ZeroPage)),
            0x75 => ASM::ADC(OpCode::new(0x75, 2, 4, AddressingMode::ZeroPage_X)),
            0x6d => ASM::ADC(OpCode::new(0x6d, 3, 4, AddressingMode::Absolute)),
            0x7d => ASM::ADC(OpCode::new(0x7d, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0x79 => ASM::ADC(OpCode::new(0x79, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0x61 => ASM::ADC(OpCode::new(0x61, 2, 6, AddressingMode::Indirect_X)),
            0x71 => ASM::ADC(OpCode::new(0x71, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0xe9 => ASM::SBC(OpCode::new(0xe9, 2, 2, AddressingMode::Immediate)),
            0xe5 => ASM::SBC(OpCode::new(0xe5, 2, 3, AddressingMode::ZeroPage)),
            0xf5 => ASM::SBC(OpCode::new(0xf5, 2, 4, AddressingMode::ZeroPage_X)),
            0xed => ASM::SBC(OpCode::new(0xed, 3, 4, AddressingMode::Absolute)),
            0xfd => ASM::SBC(OpCode::new(0xfd, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0xf9 => ASM::SBC(OpCode::new(0xf9, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0xe1 => ASM::SBC(OpCode::new(0xe1, 2, 6, AddressingMode::Indirect_X)),
            0xf1 => ASM::SBC(OpCode::new(0xf1, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0x29 => ASM::AND(OpCode::new(0x29, 2, 2, AddressingMode::Immediate)),
            0x25 => ASM::AND(OpCode::new(0x25, 2, 3, AddressingMode::ZeroPage)),
            0x35 => ASM::AND(OpCode::new(0x35, 2, 4, AddressingMode::ZeroPage_X)),
            0x2d => ASM::AND(OpCode::new(0x2d, 3, 4, AddressingMode::Absolute)),
            0x3d => ASM::AND(OpCode::new(0x3d, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0x39 => ASM::AND(OpCode::new(0x39, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0x21 => ASM::AND(OpCode::new(0x21, 2, 6, AddressingMode::Indirect_X)),
            0x31 => ASM::AND(OpCode::new(0x31, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0x49 => ASM::EOR(OpCode::new(0x49, 2, 2, AddressingMode::Immediate)),
            0x45 => ASM::EOR(OpCode::new(0x45, 2, 3, AddressingMode::ZeroPage)),
            0x55 => ASM::EOR(OpCode::new(0x55, 2, 4, AddressingMode::ZeroPage_X)),
            0x4d => ASM::EOR(OpCode::new(0x4d, 3, 4, AddressingMode::Absolute)),
            0x5d => ASM::EOR(OpCode::new(0x5d, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0x59 => ASM::EOR(OpCode::new(0x59, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0x41 => ASM::EOR(OpCode::new(0x41, 2, 6, AddressingMode::Indirect_X)),
            0x51 => ASM::EOR(OpCode::new(0x51, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0x09 => ASM::ORA(OpCode::new(0x09, 2, 2, AddressingMode::Immediate)),
            0x05 => ASM::ORA(OpCode::new(0x05, 2, 3, AddressingMode::ZeroPage)),
            0x15 => ASM::ORA(OpCode::new(0x15, 2, 4, AddressingMode::ZeroPage_X)),
            0x0d => ASM::ORA(OpCode::new(0x0d, 3, 4, AddressingMode::Absolute)),
            0x1d => ASM::ORA(OpCode::new(0x1d, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0x19 => ASM::ORA(OpCode::new(0x19, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0x01 => ASM::ORA(OpCode::new(0x01, 2, 6, AddressingMode::Indirect_X)),
            0x11 => ASM::ORA(OpCode::new(0x11, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0x0a => ASM::ASL(OpCode::new(0x0a, 1, 2, AddressingMode::NoneAddressing)),
            0x06 => ASM::ASL(OpCode::new(0x06, 2, 5, AddressingMode::ZeroPage)),
            0x16 => ASM::ASL(OpCode::new(0x16, 2, 6, AddressingMode::ZeroPage_X)),
            0x0e => ASM::ASL(OpCode::new(0x0e, 3, 6, AddressingMode::Absolute)),
            0x1e => ASM::ASL(OpCode::new(0x1e, 3, 7, AddressingMode::Absolute_X)),

            0x4a => ASM::LSR(OpCode::new(0x4a, 1, 2, AddressingMode::NoneAddressing)),
            0x46 => ASM::LSR(OpCode::new(0x46, 2, 5, AddressingMode::ZeroPage)),
            0x56 => ASM::LSR(OpCode::new(0x56, 2, 6, AddressingMode::ZeroPage_X)),
            0x4e => ASM::LSR(OpCode::new(0x4e, 3, 6, AddressingMode::Absolute)),
            0x5e => ASM::LSR(OpCode::new(0x5e, 3, 7, AddressingMode::Absolute_X)),

            0x2a => ASM::ROL(OpCode::new(0x2a, 1, 2, AddressingMode::NoneAddressing)),
            0x26 => ASM::ROL(OpCode::new(0x26, 2, 5, AddressingMode::ZeroPage)),
            0x36 => ASM::ROL(OpCode::new(0x36, 2, 6, AddressingMode::ZeroPage_X)),
            0x2e => ASM::ROL(OpCode::new(0x2e, 3, 6, AddressingMode::Absolute)),
            0x3e => ASM::ROL(OpCode::new(0x3e, 3, 7, AddressingMode::Absolute_X)),

            0x6a => ASM::ROR(OpCode::new(0x6a, 1, 2, AddressingMode::NoneAddressing)),
            0x66 => ASM::ROR(OpCode::new(0x66, 2, 5, AddressingMode::ZeroPage)),
            0x76 => ASM::ROR(OpCode::new(0x76, 2, 6, AddressingMode::ZeroPage_X)),
            0x6e => ASM::ROR(OpCode::new(0x6e, 3, 6, AddressingMode::Absolute)),
            0x7e => ASM::ROR(OpCode::new(0x7e, 3, 7, AddressingMode::Absolute_X)),

            0xe6 => ASM::INC(OpCode::new(0xe6, 2, 5, AddressingMode::ZeroPage)),
            0xf6 => ASM::INC(OpCode::new(0xf6, 2, 6, AddressingMode::ZeroPage_X)),
            0xee => ASM::INC(OpCode::new(0xee, 3, 6, AddressingMode::Absolute)),
            0xfe => ASM::INC(OpCode::new(0xfe, 3, 7, AddressingMode::Absolute_X)),

            0xe8 => ASM::INX(OpCode::new(0xe8, 1, 2, AddressingMode::NoneAddressing)),
            0xc8 => ASM::INY(OpCode::new(0xc8, 1, 2, AddressingMode::NoneAddressing)),

            0xc6 => ASM::DEC(OpCode::new(0xc6, 2, 5, AddressingMode::ZeroPage)),
            0xd6 => ASM::DEC(OpCode::new(0xd6, 2, 6, AddressingMode::ZeroPage_X)),
            0xce => ASM::DEC(OpCode::new(0xce, 3, 6, AddressingMode::Absolute)),
            0xde => ASM::DEC(OpCode::new(0xde, 3, 7, AddressingMode::Absolute_X)),

            0xca => ASM::DEX(OpCode::new(0xca, 1, 2, AddressingMode::NoneAddressing)),
            0x88 => ASM::DEY(OpCode::new(0x88, 1, 2, AddressingMode::NoneAddressing)),

            0xc9 => ASM::CMP(OpCode::new(0xc9, 2, 2, AddressingMode::Immediate)),
            0xc5 => ASM::CMP(OpCode::new(0xc5, 2, 3, AddressingMode::ZeroPage)),
            0xd5 => ASM::CMP(OpCode::new(0xd5, 2, 4, AddressingMode::ZeroPage_X)),
            0xcd => ASM::CMP(OpCode::new(0xcd, 3, 4, AddressingMode::Absolute)),
            0xdd => ASM::CMP(OpCode::new(0xdd, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0xd9 => ASM::CMP(OpCode::new(0xd9, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0xc1 => ASM::CMP(OpCode::new(0xc1, 2, 6, AddressingMode::Indirect_X)),
            0xd1 => ASM::CMP(OpCode::new(0xd1, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0xc0 => ASM::CPY(OpCode::new(0xc0, 2, 2, AddressingMode::Immediate)),
            0xc4 => ASM::CPY(OpCode::new(0xc4, 2, 3, AddressingMode::ZeroPage)),
            0xcc => ASM::CPY(OpCode::new(0xcc, 3, 4, AddressingMode::Absolute)),

            0xe0 => ASM::CPX(OpCode::new(0xe0, 2, 2, AddressingMode::Immediate)),
            0xe4 => ASM::CPX(OpCode::new(0xe4, 2, 3, AddressingMode::ZeroPage)),
            0xec => ASM::CPX(OpCode::new(0xec, 3, 4, AddressingMode::Absolute)),

            0x4c => ASM::JMP(OpCode::new(0x4c, 3, 3, AddressingMode::Absolute)),
            0x6c => ASM::JMP(OpCode::new(0x6c, 3, 5, AddressingMode::Indirect)),

            0x20 => ASM::JSR(OpCode::new(0x20, 3, 6, AddressingMode::NoneAddressing)),
            0x60 => ASM::RTS(OpCode::new(0x60, 1, 6, AddressingMode::NoneAddressing)),

            0x40 => ASM::RTI(OpCode::new(0x40, 1, 6, AddressingMode::NoneAddressing)),

            0xd0 => ASM::BNE(OpCode::new(0xd0, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0x70 => ASM::BVS(OpCode::new(0x70, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0x50 => ASM::BVC(OpCode::new(0x50, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0x30 => ASM::BMI(OpCode::new(0x30, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0xf0 => ASM::BEQ(OpCode::new(0xf0, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0xb0 => ASM::BCS(OpCode::new(0xb0, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0x90 => ASM::BCC(OpCode::new(0x90, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),
            0x10 => ASM::BPL(OpCode::new(0x10, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing)),

            0x24 => ASM::BIT(OpCode::new(0x24, 2, 3, AddressingMode::ZeroPage)),
            0x2c => ASM::BIT(OpCode::new(0x2c, 3, 4, AddressingMode::Absolute)),


            /* Stores, Loads */
            0xa9 => ASM::LDA(OpCode::new(0xa9, 2, 2, AddressingMode::Immediate)),
            0xa5 => ASM::LDA(OpCode::new(0xa5, 2, 3, AddressingMode::ZeroPage)),
            0xb5 => ASM::LDA(OpCode::new(0xb5, 2, 4, AddressingMode::ZeroPage_X)),
            0xad => ASM::LDA(OpCode::new(0xad, 3, 4, AddressingMode::Absolute)),
            0xbd => ASM::LDA(OpCode::new(0xbd, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),
            0xb9 => ASM::LDA(OpCode::new(0xb9, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),
            0xa1 => ASM::LDA(OpCode::new(0xa1, 2, 6, AddressingMode::Indirect_X)),
            0xb1 => ASM::LDA(OpCode::new(0xb1, 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y)),

            0xa2 => ASM::LDX(OpCode::new(0xa2, 2, 2, AddressingMode::Immediate)),
            0xa6 => ASM::LDX(OpCode::new(0xa6, 2, 3, AddressingMode::ZeroPage)),
            0xb6 => ASM::LDX(OpCode::new(0xb6, 2, 4, AddressingMode::ZeroPage_Y)),
            0xae => ASM::LDX(OpCode::new(0xae, 3, 4, AddressingMode::Absolute)),
            0xbe => ASM::LDX(OpCode::new(0xbe, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y)),

            0xa0 => ASM::LDY(OpCode::new(0xa0, 2, 2, AddressingMode::Immediate)),
            0xa4 => ASM::LDY(OpCode::new(0xa4, 2, 3, AddressingMode::ZeroPage)),
            0xb4 => ASM::LDY(OpCode::new(0xb4, 2, 4, AddressingMode::ZeroPage_X)),
            0xac => ASM::LDY(OpCode::new(0xac, 3, 4, AddressingMode::Absolute)),
            0xbc => ASM::LDY(OpCode::new(0xbc, 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X)),

            0x85 => ASM::STA(OpCode::new(0x85, 2, 3, AddressingMode::ZeroPage)),
            0x95 => ASM::STA(OpCode::new(0x95, 2, 4, AddressingMode::ZeroPage_X)),
            0x8d => ASM::STA(OpCode::new(0x8d, 3, 4, AddressingMode::Absolute)),
            0x9d => ASM::STA(OpCode::new(0x9d, 3, 5, AddressingMode::Absolute_X)),
            0x99 => ASM::STA(OpCode::new(0x99, 3, 5, AddressingMode::Absolute_Y)),
            0x81 => ASM::STA(OpCode::new(0x81, 2, 6, AddressingMode::Indirect_X)),
            0x91 => ASM::STA(OpCode::new(0x91, 2, 6, AddressingMode::Indirect_Y)),

            0x86 => ASM::STX(OpCode::new(0x86, 2, 3, AddressingMode::ZeroPage)),
            0x96 => ASM::STX(OpCode::new(0x96, 2, 4, AddressingMode::ZeroPage_Y)),
            0x8e => ASM::STX(OpCode::new(0x8e, 3, 4, AddressingMode::Absolute)),

            0x84 => ASM::STY(OpCode::new(0x84, 2, 3, AddressingMode::ZeroPage)),
            0x94 => ASM::STY(OpCode::new(0x94, 2, 4, AddressingMode::ZeroPage_X)),
            0x8c => ASM::STY(OpCode::new(0x8c, 3, 4, AddressingMode::Absolute)),


            /* Flags clear */

            0xD8 => ASM::CLD(OpCode::new(0xD8, 1, 2, AddressingMode::NoneAddressing)),
            0x58 => ASM::CLI(OpCode::new(0x58, 1, 2, AddressingMode::NoneAddressing)),
            0xb8 => ASM::CLV(OpCode::new(0xb8, 1, 2, AddressingMode::NoneAddressing)),
            0x18 => ASM::CLC(OpCode::new(0x18, 1, 2, AddressingMode::NoneAddressing)),
            0x38 => ASM::SEC(OpCode::new(0x38, 1, 2, AddressingMode::NoneAddressing)),
            0x78 => ASM::SEI(OpCode::new(0x78, 1, 2, AddressingMode::NoneAddressing)),
            0xf8 => ASM::SED(OpCode::new(0xf8, 1, 2, AddressingMode::NoneAddressing)),

            0xaa => ASM::TAX(OpCode::new(0xaa, 1, 2, AddressingMode::NoneAddressing)),
            0xa8 => ASM::TAY(OpCode::new(0xa8, 1, 2, AddressingMode::NoneAddressing)),
            0xba => ASM::TSX(OpCode::new(0xba, 1, 2, AddressingMode::NoneAddressing)),
            0x8a => ASM::TXA(OpCode::new(0x8a, 1, 2, AddressingMode::NoneAddressing)),
            0x9a => ASM::TXS(OpCode::new(0x9a, 1, 2, AddressingMode::NoneAddressing)),
            0x98 => ASM::TYA(OpCode::new(0x98, 1, 2, AddressingMode::NoneAddressing)),

            /* Stack */
            0x48 => ASM::PHA(OpCode::new(0x48, 1, 3, AddressingMode::NoneAddressing)),
            0x68 => ASM::PLA(OpCode::new(0x68, 1, 4, AddressingMode::NoneAddressing)),
            0x08 => ASM::PHP(OpCode::new(0x08, 1, 3, AddressingMode::NoneAddressing)),
            0x28 => ASM::PLP(OpCode::new(0x28, 1, 4, AddressingMode::NoneAddressing)),

            _ => {
                panic!("unknown op code!")
            }
        }
    }

    pub fn get_len(&self) -> u8 {
        match self {
            ASM::ADC(op_code) => op_code.len,
            ASM::AND(op_code) => op_code.len,
            ASM::ASL(op_code) => op_code.len,
            ASM::BCC(op_code) => op_code.len,
            ASM::BCS(op_code) => op_code.len,
            ASM::BEQ(op_code) => op_code.len,
            ASM::BIT(op_code) => op_code.len,
            ASM::BMI(op_code) => op_code.len,
            ASM::BNE(op_code) => op_code.len,
            ASM::BPL(op_code) => op_code.len,
            ASM::BRK(op_code) => op_code.len,
            ASM::BVC(op_code) => op_code.len,
            ASM::BVS(op_code) => op_code.len,
            ASM::CLC(op_code) => op_code.len,
            ASM::CLD(op_code) => op_code.len,
            ASM::CLI(op_code) => op_code.len,
            ASM::CLV(op_code) => op_code.len,
            ASM::CMP(op_code) => op_code.len,
            ASM::CPX(op_code) => op_code.len,
            ASM::CPY(op_code) => op_code.len,
            ASM::DEC(op_code) => op_code.len,
            ASM::DEX(op_code) => op_code.len,
            ASM::DEY(op_code) => op_code.len,
            ASM::EOR(op_code) => op_code.len,
            ASM::INC(op_code) => op_code.len,
            ASM::INX(op_code) => op_code.len,
            ASM::INY(op_code) => op_code.len,
            ASM::JMP(op_code) => op_code.len,
            ASM::JSR(op_code) => op_code.len,
            ASM::LDA(op_code) => op_code.len,
            ASM::LDX(op_code) => op_code.len,
            ASM::LDY(op_code) => op_code.len,
            ASM::LSR(op_code) => op_code.len,
            ASM::NOP(op_code) => op_code.len,
            ASM::ORA(op_code) => op_code.len,
            ASM::PHA(op_code) => op_code.len,
            ASM::PHP(op_code) => op_code.len,
            ASM::PLA(op_code) => op_code.len,
            ASM::PLP(op_code) => op_code.len,
            ASM::ROL(op_code) => op_code.len,
            ASM::ROR(op_code) => op_code.len,
            ASM::RTI(op_code) => op_code.len,
            ASM::RTS(op_code) => op_code.len,
            ASM::SBC(op_code) => op_code.len,
            ASM::SEC(op_code) => op_code.len,
            ASM::SED(op_code) => op_code.len,
            ASM::SEI(op_code) => op_code.len,
            ASM::STA(op_code) => op_code.len,
            ASM::STX(op_code) => op_code.len,
            ASM::STY(op_code) => op_code.len,
            ASM::TAX(op_code) => op_code.len,
            ASM::TAY(op_code) => op_code.len,
            ASM::TSX(op_code) => op_code.len,
            ASM::TXA(op_code) => op_code.len,
            ASM::TXS(op_code) => op_code.len,
            ASM::TYA(op_code) => op_code.len,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_compile_code() {
        let asm = ASM::compile_opcode(0xaa);
        if let ASM::TAX(op) = asm {
            assert_eq!(op.code, 0xaa);
            assert_eq!(op.mode, AddressingMode::NoneAddressing);
        } else {
            panic!("err")
        }
    }
}
