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
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(len:u8, cycles:u8, mode: AddressingMode) -> OpCode {
        OpCode{
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
    pub fn compile_opcode(op_code:u8) -> ASM {
        match op_code {
            0x00 => ASM::BRK(OpCode::new(1, 7, AddressingMode::NoneAddressing)),
            0xEA => ASM::NOP(OpCode::new(1, 2, AddressingMode::NoneAddressing)),

            0x69 => ASM::ADC(OpCode::new(2, 2, AddressingMode::Absolute)),

            _ => {
            panic!()
        } }
    }
}
