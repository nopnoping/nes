pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Frame {
            data: vec![0; (Frame::WIDTH) * (Frame::HEIGHT) * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = x * 3 + y * 3 * Frame::WIDTH;
        if base + 2 < self.data.len() {
            (self.data[base], self.data[base + 1], self.data[base + 2]) = rgb;
        }
    }
}