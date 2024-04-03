#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
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
    pub fn compile_opcode(op_code: u8) -> ASM {
        match op_code {
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

            _ => {
                panic!()
            }
        }
    }
}
