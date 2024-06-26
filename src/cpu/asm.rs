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

#[derive(Debug)]
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

#[derive(Debug)]
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
    /* unofficial */
    _DCP(OpCode),
    _RLA(OpCode),
    _SLO(OpCode),
    _SRE(OpCode),
    _NOP(OpCode),
    _AXS(OpCode),
    _ARR(OpCode),
    _SBC(OpCode),
    _ANC(OpCode),
    _ALR(OpCode),
    _RRA(OpCode),
    _ISB(OpCode),
    _LXA(OpCode),
    _XAA(OpCode),
    _LAS(OpCode),
    _TAS(OpCode),
    _AHX(OpCode),
    _SHX(OpCode),
    _SHY(OpCode),
    _LAX(OpCode),
    _SAX(OpCode),
}


impl ASM {
    pub fn compile_opcode(code: u8) -> ASM {
        match code {
            0x00 => ASM::BRK(OpCode::new(0x00, 1, 7, AddressingMode::NoneAddressing)),

            0xEA => ASM::NOP(OpCode::new(0xEA, 1, 2, AddressingMode::NoneAddressing)),

            0x69 => ASM::ADC(OpCode::new(0x69, 2, 2, AddressingMode::Immediate)),
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

            /* unofficial */
            0xc7 => ASM::_DCP(OpCode::new(0xc7, 2, 5, AddressingMode::ZeroPage)),
            0xd7 => ASM::_DCP(OpCode::new(0xd7, 2, 6, AddressingMode::ZeroPage_X)),
            0xCF => ASM::_DCP(OpCode::new(0xCF, 3, 6, AddressingMode::Absolute)),
            0xdF => ASM::_DCP(OpCode::new(0xdF, 3, 7, AddressingMode::Absolute_X)),
            0xdb => ASM::_DCP(OpCode::new(0xdb, 3, 7, AddressingMode::Absolute_Y)),
            0xd3 => ASM::_DCP(OpCode::new(0xd3, 2, 8, AddressingMode::Indirect_Y)),
            0xc3 => ASM::_DCP(OpCode::new(0xc3, 2, 8, AddressingMode::Indirect_X)),

            0x27 => ASM::_RLA(OpCode::new(0x27, 2, 5, AddressingMode::ZeroPage)),
            0x37 => ASM::_RLA(OpCode::new(0x37, 2, 6, AddressingMode::ZeroPage_X)),
            0x2F => ASM::_RLA(OpCode::new(0x2F, 3, 6, AddressingMode::Absolute)),
            0x3F => ASM::_RLA(OpCode::new(0x3F, 3, 7, AddressingMode::Absolute_X)),
            0x3b => ASM::_RLA(OpCode::new(0x3b, 3, 7, AddressingMode::Absolute_Y)),
            0x33 => ASM::_RLA(OpCode::new(0x33, 2, 8, AddressingMode::Indirect_Y)),
            0x23 => ASM::_RLA(OpCode::new(0x23, 2, 8, AddressingMode::Indirect_X)),

            0x07 => ASM::_SLO(OpCode::new(0x07, 2, 5, AddressingMode::ZeroPage)),
            0x17 => ASM::_SLO(OpCode::new(0x17, 2, 6, AddressingMode::ZeroPage_X)),
            0x0F => ASM::_SLO(OpCode::new(0x0F, 3, 6, AddressingMode::Absolute)),
            0x1f => ASM::_SLO(OpCode::new(0x1f, 3, 7, AddressingMode::Absolute_X)),
            0x1b => ASM::_SLO(OpCode::new(0x1b, 3, 7, AddressingMode::Absolute_Y)),
            0x03 => ASM::_SLO(OpCode::new(0x03, 2, 8, AddressingMode::Indirect_X)),
            0x13 => ASM::_SLO(OpCode::new(0x13, 2, 8, AddressingMode::Indirect_Y)),

            0x47 => ASM::_SRE(OpCode::new(0x47, 2, 5, AddressingMode::ZeroPage)),
            0x57 => ASM::_SRE(OpCode::new(0x57, 2, 6, AddressingMode::ZeroPage_X)),
            0x4F => ASM::_SRE(OpCode::new(0x4F, 3, 6, AddressingMode::Absolute)),
            0x5f => ASM::_SRE(OpCode::new(0x5f, 3, 7, AddressingMode::Absolute_X)),
            0x5b => ASM::_SRE(OpCode::new(0x5b, 3, 7, AddressingMode::Absolute_Y)),
            0x43 => ASM::_SRE(OpCode::new(0x43, 2, 8, AddressingMode::Indirect_X)),
            0x53 => ASM::_SRE(OpCode::new(0x53, 2, 8, AddressingMode::Indirect_Y)),

            0xCB => ASM::_AXS(OpCode::new(0xCB, 2, 2, AddressingMode::Immediate)),
            0x6B => ASM::_ARR(OpCode::new(0x6B, 2, 2, AddressingMode::Immediate)),
            0xeb => ASM::_SBC(OpCode::new(0xeb, 2, 2, AddressingMode::Immediate)),

            0x0b => ASM::_ANC(OpCode::new(0x0b, 2, 2, AddressingMode::Immediate)),
            0x2b => ASM::_ANC(OpCode::new(0x2b, 2, 2, AddressingMode::Immediate)),

            0x4b => ASM::_ALR(OpCode::new(0x4b, 2, 2, AddressingMode::Immediate)),

            0x80 => ASM::_NOP(OpCode::new(0x80, 2, 2, AddressingMode::Immediate)),
            0x82 => ASM::_NOP(OpCode::new(0x82, 2, 2, AddressingMode::Immediate)),
            0x89 => ASM::_NOP(OpCode::new(0x89, 2, 2, AddressingMode::Immediate)),
            0xc2 => ASM::_NOP(OpCode::new(0xc2, 2, 2, AddressingMode::Immediate)),
            0xe2 => ASM::_NOP(OpCode::new(0xe2, 2, 2, AddressingMode::Immediate)),
            0x04 => ASM::_NOP(OpCode::new(0x04, 2, 3, AddressingMode::ZeroPage)),
            0x44 => ASM::_NOP(OpCode::new(0x44, 2, 3, AddressingMode::ZeroPage)),
            0x64 => ASM::_NOP(OpCode::new(0x64, 2, 3, AddressingMode::ZeroPage)),
            0x14 => ASM::_NOP(OpCode::new(0x14, 2, 4, AddressingMode::ZeroPage_X)),
            0x34 => ASM::_NOP(OpCode::new(0x34, 2, 4, AddressingMode::ZeroPage_X)),
            0x54 => ASM::_NOP(OpCode::new(0x54, 2, 4, AddressingMode::ZeroPage_X)),
            0x74 => ASM::_NOP(OpCode::new(0x74, 2, 4, AddressingMode::ZeroPage_X)),
            0xd4 => ASM::_NOP(OpCode::new(0xd4, 2, 4, AddressingMode::ZeroPage_X)),
            0xf4 => ASM::_NOP(OpCode::new(0xf4, 2, 4, AddressingMode::ZeroPage_X)),
            0x0c => ASM::_NOP(OpCode::new(0x0c, 3, 4, AddressingMode::Absolute)),
            0x1c => ASM::_NOP(OpCode::new(0x1c, 3, 4 /*or 5*/, AddressingMode::Absolute_X)),
            0x3c => ASM::_NOP(OpCode::new(0x3c, 3, 4 /*or 5*/, AddressingMode::Absolute_X)),
            0x5c => ASM::_NOP(OpCode::new(0x5c, 3, 4 /*or 5*/, AddressingMode::Absolute_X)),
            0x7c => ASM::_NOP(OpCode::new(0x7c, 3, 4 /*or 5*/, AddressingMode::Absolute_X)),
            0xdc => ASM::_NOP(OpCode::new(0xdc, 3, 4 /* or 5*/, AddressingMode::Absolute_X)),
            0xfc => ASM::_NOP(OpCode::new(0xfc, 3, 4 /* or 5*/, AddressingMode::Absolute_X)),

            0x67 => ASM::_RRA(OpCode::new(0x67, 2, 5, AddressingMode::ZeroPage)),
            0x77 => ASM::_RRA(OpCode::new(0x77, 2, 6, AddressingMode::ZeroPage_X)),
            0x6f => ASM::_RRA(OpCode::new(0x6f, 3, 6, AddressingMode::Absolute)),
            0x7f => ASM::_RRA(OpCode::new(0x7f, 3, 7, AddressingMode::Absolute_X)),
            0x7b => ASM::_RRA(OpCode::new(0x7b, 3, 7, AddressingMode::Absolute_Y)),
            0x63 => ASM::_RRA(OpCode::new(0x63, 2, 8, AddressingMode::Indirect_X)),
            0x73 => ASM::_RRA(OpCode::new(0x73, 2, 8, AddressingMode::Indirect_Y)),

            0xe7 => ASM::_ISB(OpCode::new(0xe7, 2, 5, AddressingMode::ZeroPage)),
            0xf7 => ASM::_ISB(OpCode::new(0xf7, 2, 6, AddressingMode::ZeroPage_X)),
            0xef => ASM::_ISB(OpCode::new(0xef, 3, 6, AddressingMode::Absolute)),
            0xff => ASM::_ISB(OpCode::new(0xff, 3, 7, AddressingMode::Absolute_X)),
            0xfb => ASM::_ISB(OpCode::new(0xfb, 3, 7, AddressingMode::Absolute_Y)),
            0xe3 => ASM::_ISB(OpCode::new(0xe3, 2, 8, AddressingMode::Indirect_X)),
            0xf3 => ASM::_ISB(OpCode::new(0xf3, 2, 8, AddressingMode::Indirect_Y)),

            0x02 => ASM::_NOP(OpCode::new(0x02, 1, 2, AddressingMode::NoneAddressing)),
            0x12 => ASM::_NOP(OpCode::new(0x12, 1, 2, AddressingMode::NoneAddressing)),
            0x22 => ASM::_NOP(OpCode::new(0x22, 1, 2, AddressingMode::NoneAddressing)),
            0x32 => ASM::_NOP(OpCode::new(0x32, 1, 2, AddressingMode::NoneAddressing)),
            0x42 => ASM::_NOP(OpCode::new(0x42, 1, 2, AddressingMode::NoneAddressing)),
            0x52 => ASM::_NOP(OpCode::new(0x52, 1, 2, AddressingMode::NoneAddressing)),
            0x62 => ASM::_NOP(OpCode::new(0x62, 1, 2, AddressingMode::NoneAddressing)),
            0x72 => ASM::_NOP(OpCode::new(0x72, 1, 2, AddressingMode::NoneAddressing)),
            0x92 => ASM::_NOP(OpCode::new(0x92, 1, 2, AddressingMode::NoneAddressing)),
            0xb2 => ASM::_NOP(OpCode::new(0xb2, 1, 2, AddressingMode::NoneAddressing)),
            0xd2 => ASM::_NOP(OpCode::new(0xd2, 1, 2, AddressingMode::NoneAddressing)),
            0xf2 => ASM::_NOP(OpCode::new(0xf2, 1, 2, AddressingMode::NoneAddressing)),

            0x1a => ASM::_NOP(OpCode::new(0x1a, 1, 2, AddressingMode::NoneAddressing)),
            0x3a => ASM::_NOP(OpCode::new(0x3a, 1, 2, AddressingMode::NoneAddressing)),
            0x5a => ASM::_NOP(OpCode::new(0x5a, 1, 2, AddressingMode::NoneAddressing)),
            0x7a => ASM::_NOP(OpCode::new(0x7a, 1, 2, AddressingMode::NoneAddressing)),
            0xda => ASM::_NOP(OpCode::new(0xda, 1, 2, AddressingMode::NoneAddressing)),
            0xfa => ASM::_NOP(OpCode::new(0xfa, 1, 2, AddressingMode::NoneAddressing)),

            0xab => ASM::_LXA(OpCode::new(0xab, 2, 3, AddressingMode::Immediate)),
            0x8b => ASM::_XAA(OpCode::new(0x8b, 2, 3, AddressingMode::Immediate)),
            0xbb => ASM::_LAS(OpCode::new(0xbb, 3, 2, AddressingMode::Absolute_Y)),
            0x9b => ASM::_TAS(OpCode::new(0x9b, 3, 2, AddressingMode::Absolute_Y)),
            0x93 => ASM::_AHX(OpCode::new(0x93, 2, /* guess */ 8, AddressingMode::Indirect_Y)),
            0x9f => ASM::_AHX(OpCode::new(0x9f, 3, /* guess */ 4/* or 5*/, AddressingMode::Absolute_Y)),
            0x9e => ASM::_SHX(OpCode::new(0x9e, 3, /* guess */ 4/* or 5*/, AddressingMode::Absolute_Y)),
            0x9c => ASM::_SHY(OpCode::new(0x9c, 3, /* guess */ 4/* or 5*/, AddressingMode::Absolute_X)),

            0xa7 => ASM::_LAX(OpCode::new(0xa7, 2, 3, AddressingMode::ZeroPage)),
            0xb7 => ASM::_LAX(OpCode::new(0xb7, 2, 4, AddressingMode::ZeroPage_Y)),
            0xaf => ASM::_LAX(OpCode::new(0xaf, 3, 4, AddressingMode::Absolute)),
            0xbf => ASM::_LAX(OpCode::new(0xbf, 3, 4, AddressingMode::Absolute_Y)),
            0xa3 => ASM::_LAX(OpCode::new(0xa3, 2, 6, AddressingMode::Indirect_X)),
            0xb3 => ASM::_LAX(OpCode::new(0xb3, 2, 5, AddressingMode::Indirect_Y)),

            0x87 => ASM::_SAX(OpCode::new(0x87, 2, 3, AddressingMode::ZeroPage)),
            0x97 => ASM::_SAX(OpCode::new(0x97, 2, 4, AddressingMode::ZeroPage_Y)),
            0x8f => ASM::_SAX(OpCode::new(0x8f, 3, 4, AddressingMode::Absolute)),
            0x83 => ASM::_SAX(OpCode::new(0x83, 2, 6, AddressingMode::Indirect_X)),

            _ => {
                panic!("unknown op code!")
            }
        }
    }

    pub fn get_desc(&self) -> (String, &OpCode) {
        match self {
            ASM::ADC(op_code) => (String::from("ADC"), op_code),
            ASM::AND(op_code) => (String::from("AND"), op_code),
            ASM::ASL(op_code) => (String::from("ASL"), op_code),
            ASM::BCC(op_code) => (String::from("BCC"), op_code),
            ASM::BCS(op_code) => (String::from("BCS"), op_code),
            ASM::BEQ(op_code) => (String::from("BEQ"), op_code),
            ASM::BIT(op_code) => (String::from("BIT"), op_code),
            ASM::BMI(op_code) => (String::from("BMI"), op_code),
            ASM::BNE(op_code) => (String::from("BNE"), op_code),
            ASM::BPL(op_code) => (String::from("BPL"), op_code),
            ASM::BRK(op_code) => (String::from("BRK"), op_code),
            ASM::BVC(op_code) => (String::from("BVC"), op_code),
            ASM::BVS(op_code) => (String::from("BVS"), op_code),
            ASM::CLC(op_code) => (String::from("CLC"), op_code),
            ASM::CLD(op_code) => (String::from("CLD"), op_code),
            ASM::CLI(op_code) => (String::from("CLI"), op_code),
            ASM::CLV(op_code) => (String::from("CLV"), op_code),
            ASM::CMP(op_code) => (String::from("CMP"), op_code),
            ASM::CPX(op_code) => (String::from("CPX"), op_code),
            ASM::CPY(op_code) => (String::from("CPY"), op_code),
            ASM::DEC(op_code) => (String::from("DEC"), op_code),
            ASM::DEX(op_code) => (String::from("DEX"), op_code),
            ASM::DEY(op_code) => (String::from("DEY"), op_code),
            ASM::EOR(op_code) => (String::from("EOR"), op_code),
            ASM::INC(op_code) => (String::from("INC"), op_code),
            ASM::INX(op_code) => (String::from("INX"), op_code),
            ASM::INY(op_code) => (String::from("INY"), op_code),
            ASM::JMP(op_code) => (String::from("JMP"), op_code),
            ASM::JSR(op_code) => (String::from("JSR"), op_code),
            ASM::LDA(op_code) => (String::from("LDA"), op_code),
            ASM::LDX(op_code) => (String::from("LDX"), op_code),
            ASM::LDY(op_code) => (String::from("LDY"), op_code),
            ASM::LSR(op_code) => (String::from("LSR"), op_code),
            ASM::NOP(op_code) => (String::from("NOP"), op_code),
            ASM::ORA(op_code) => (String::from("ORA"), op_code),
            ASM::PHA(op_code) => (String::from("PHA"), op_code),
            ASM::PHP(op_code) => (String::from("PHP"), op_code),
            ASM::PLA(op_code) => (String::from("PLA"), op_code),
            ASM::PLP(op_code) => (String::from("PLP"), op_code),
            ASM::ROL(op_code) => (String::from("ROL"), op_code),
            ASM::ROR(op_code) => (String::from("ROR"), op_code),
            ASM::RTI(op_code) => (String::from("RTI"), op_code),
            ASM::RTS(op_code) => (String::from("RTS"), op_code),
            ASM::SBC(op_code) => (String::from("SBC"), op_code),
            ASM::SEC(op_code) => (String::from("SEC"), op_code),
            ASM::SED(op_code) => (String::from("SED"), op_code),
            ASM::SEI(op_code) => (String::from("SEI"), op_code),
            ASM::STA(op_code) => (String::from("STA"), op_code),
            ASM::STX(op_code) => (String::from("STX"), op_code),
            ASM::STY(op_code) => (String::from("STY"), op_code),
            ASM::TAX(op_code) => (String::from("TAX"), op_code),
            ASM::TAY(op_code) => (String::from("TAY"), op_code),
            ASM::TSX(op_code) => (String::from("TSX"), op_code),
            ASM::TXA(op_code) => (String::from("TXA"), op_code),
            ASM::TXS(op_code) => (String::from("TXS"), op_code),
            ASM::TYA(op_code) => (String::from("TYA"), op_code),
            ASM::_DCP(op_code) => (String::from("*DCP"), op_code),
            ASM::_RLA(op_code) => (String::from("*RLA"), op_code),
            ASM::_SLO(op_code) => (String::from("*SLO"), op_code),
            ASM::_SRE(op_code) => (String::from("*SRE"), op_code),
            ASM::_NOP(op_code) => (String::from("*NOP"), op_code),
            ASM::_AXS(op_code) => (String::from("*AXS"), op_code),
            ASM::_ARR(op_code) => (String::from("*ARR"), op_code),
            ASM::_SBC(op_code) => (String::from("*SBC"), op_code),
            ASM::_ANC(op_code) => (String::from("*ANC"), op_code),
            ASM::_ALR(op_code) => (String::from("*ALR"), op_code),
            ASM::_RRA(op_code) => (String::from("*RRA"), op_code),
            ASM::_ISB(op_code) => (String::from("*ISB"), op_code),
            ASM::_LXA(op_code) => (String::from("*LXA"), op_code),
            ASM::_XAA(op_code) => (String::from("*XAA"), op_code),
            ASM::_LAS(op_code) => (String::from("*LAS"), op_code),
            ASM::_TAS(op_code) => (String::from("*TAS"), op_code),
            ASM::_AHX(op_code) => (String::from("*AHX"), op_code),
            ASM::_SHX(op_code) => (String::from("*SHX"), op_code),
            ASM::_SHY(op_code) => (String::from("*SHY"), op_code),
            ASM::_LAX(op_code) => (String::from("*LAX"), op_code),
            ASM::_SAX(op_code) => (String::from("*SAX"), op_code),
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
