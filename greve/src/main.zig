const std = @import("std");
const debug = std.debug;
const format = std.fmt.format;

pub fn main() error!void {
    /// u8 u8 u8 u8 u8 u8 u8 u8
    var bytes: [1<<2]u8 = undefined;

    const n0: u2 = 0b01;
    const n1: u2 = 0b10;
    const n2: u4 = n0 + n1;
    const n3: u2 = n0 + n1;

    bytes[0] = n0;
    bytes[1] = n1;
    bytes[2] = n2;
    bytes[3] = n3;

    debug.warn("All your base are belong to us.\n");
    for (bytes) |byte, i| {
        debug.warn("Byte[{}] => {}, ", i, byte);
    }
    debug.warn("\n");
}
