use std::arch::asm;
use crate::cpu::asm::{AddressingMode, ASM};
use crate::bus::Mem;
use crate::cpu::CPU;

pub fn trace(cpu: &mut CPU) -> String {
    let code = cpu.mem_read(cpu.program_counter);
    let asm = ASM::compile_opcode(code);
    asm.fmt_asm(cpu)
}

impl ASM {
    fn fmt_asm(&self, cpu: &mut CPU) -> String {
        let (asm_str, op_code) = self.get_desc();

        let begin = cpu.program_counter;
        let mut hex_dump = vec![];
        hex_dump.push(op_code.code);

        let (mem_addr, stored_value) = match op_code.mode {
            AddressingMode::Immediate | AddressingMode::NoneAddressing | AddressingMode::Indirect => (0, 0),
            _ => {
                let addr = cpu.get_absolute_address(&op_code.mode, begin + 1);
                (addr, cpu.mem_read(addr))
            }
        };

        let tmp = match op_code.len {
            1 => match op_code.code {
                0x0a | 0x4a | 0x2a | 0x6a => "A ".to_string(),
                _ => String::from(""),
            },
            2 => {
                let address = cpu.mem_read(begin + 1);
                hex_dump.push(address);
                match op_code.mode {
                    AddressingMode::Immediate => format!("#${:02x}", address),
                    AddressingMode::ZeroPage => format!("${:02x} = {:02x}", mem_addr, stored_value),
                    AddressingMode::ZeroPage_X => format!(
                        "${:02x},X @ {:02x} = {:02x}",
                        address, mem_addr, stored_value
                    ),
                    AddressingMode::ZeroPage_Y => format!(
                        "${:02x},Y @ {:02x} = {:02x}",
                        address, mem_addr, stored_value
                    ),
                    AddressingMode::Indirect_X => format!(
                        "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                        address,
                        (address.wrapping_add(cpu.register_x)),
                        mem_addr,
                        stored_value
                    ),
                    AddressingMode::Indirect_Y => format!(
                        "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                        address,
                        (mem_addr.wrapping_sub(cpu.register_y as u16)),
                        mem_addr,
                        stored_value
                    ),
                    AddressingMode::NoneAddressing => {
                        // assuming local jumps: BNE, BVS, etc....
                        let address: usize =
                            (begin as usize + 2).wrapping_add((address as i8) as usize);
                        format!("${:04x}", address)
                    }
                    _ => panic!(
                        "unexpected addressing mode {:?} has ops-len 2. code {:02x}",
                        op_code.mode, op_code.code
                    ),
                }
            }
            3 => {
                let address_lo = cpu.mem_read(begin + 1);
                let address_hi = cpu.mem_read(begin + 2);
                hex_dump.push(address_lo);
                hex_dump.push(address_hi);

                let address = cpu.mem_read_u16(begin + 1);

                match op_code.mode {
                    AddressingMode::Absolute => {
                        if asm_str == "JMP" {
                            format!("${:04x}", address)
                        } else {
                            format!("${:04x} = {:02x}", mem_addr, stored_value)
                        }
                    }
                    AddressingMode::Absolute_X => format!(
                        "${:04x},X @ {:04x} = {:02x}",
                        address, mem_addr, stored_value
                    ),
                    AddressingMode::Absolute_Y => format!(
                        "${:04x},Y @ {:04x} = {:02x}",
                        address, mem_addr, stored_value
                    ),
                    AddressingMode::Indirect => {
                        let jmp_addr = if address & 0x00FF == 0x00FF {
                            let lo = cpu.mem_read(address);
                            let hi = cpu.mem_read(address & 0xFF00);
                            (hi as u16) << 8 | (lo as u16)
                        } else {
                            cpu.mem_read_u16(address)
                        };

                        // let jmp_addr = cpu.mem_read_u16(address);
                        format!("(${:04x}) = {:04x}", address, jmp_addr)
                    }
                    AddressingMode::NoneAddressing => format!("${:04x}", address),
                    _ => panic!(
                        "unexpected addressing mode {:?} has ops-len 3. code {:02x}",
                        op_code.mode, op_code.code
                    ),
                }
            }
            _ => String::from(""),
        };

        let hex_str = hex_dump.iter()
            .map(|z| format!("{:02x}", z))
            .collect::<Vec<String>>()
            .join(" ");
        let asm_str = format!("{:04x}  {:8} {: >4} {}", begin, hex_str, asm_str, tmp)
            .trim()
            .to_string();

        format!(
            "{:47} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
            asm_str, cpu.register_a, cpu.register_x, cpu.register_y, cpu.status, cpu.stack_pointer,
        )
            .to_ascii_uppercase()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::bus::Bus;
    use crate::cartridges::test::test_rom;

    #[test]
    fn test_format_trace() {
        let mut bus = Bus::new(test_rom());
        bus.mem_write(100, 0xa2);
        bus.mem_write(101, 0x01);
        bus.mem_write(102, 0xca);
        bus.mem_write(103, 0x88);
        bus.mem_write(104, 0x00);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_a = 1;
        cpu.register_x = 2;
        cpu.register_y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
            result[2]
        );
    }

    #[test]
    fn test_format_mem_access() {
        let mut bus = Bus::new(test_rom());
        // ORA ($33), Y
        bus.mem_write(100, 0x11);
        bus.mem_write(101, 0x33);

        //data
        bus.mem_write(0x33, 00);
        bus.mem_write(0x34, 04);

        //target cell
        bus.mem_write(0x400, 0xAA);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_y = 0;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
            result[0]
        );
    }
}