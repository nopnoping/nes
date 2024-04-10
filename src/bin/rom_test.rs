use nes::bus::Bus;
use nes::cartridges::Rom;
use nes::cpu::CPU;
use nes::trace::trace;

fn main() {
    //load the game
    let bytes: Vec<u8> = std::fs::read("/Users/yifanjun/rust_proj/nes/nestest.nes").unwrap();
    let rom = Rom::new(&bytes).unwrap();

    let bus = Bus::new(rom);
    let mut cpu = CPU::new(bus);
    cpu.reset();
    cpu.program_counter = 0xC000;

    cpu.run_with_callback(move |cpu| {
        println!("{}", trace(cpu));
    });
}