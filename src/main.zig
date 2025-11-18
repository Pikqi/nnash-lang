const std = @import("std");
const nnash = @import("nnash");
const Parser = nnash.parser.Parser;
const Lexer = nnash.lexer.Lexer;

pub fn main() !void {
    var buff: [1024]u8 = undefined;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.skip();

    const stdout = std.fs.File.stdout();
    var buffered_writer = stdout.writer(&buff);
    var writer = &buffered_writer.interface;

    var file_name: []const u8 = "";

    if (args.next()) |arg| {
        file_name = arg;
    } else {
        try writer.print("No filename passed", .{});
        try writer.flush();
        return;
    }

    var nash = try nnash.Nash.parseFile(file_name, alloc);
    try nash.parser.printAST(writer);
    defer nash.deinit();
}
