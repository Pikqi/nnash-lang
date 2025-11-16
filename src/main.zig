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

    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    const file_contents = try file.readToEndAlloc(alloc, 1024 * 1024 * 4);
    defer alloc.free(file_contents);

    var lexer = try Lexer.init(file_contents, alloc);
    defer lexer.deinit();
    try lexer.scanTokens();

    const tokens = try lexer.lexems.toOwnedSlice(alloc);
    defer alloc.free(tokens);

    var arena = std.heap.ArenaAllocator.init(alloc);

    var parser = Parser.init(tokens, &arena);
    defer parser.deinit();
    try parser.parse();
    try parser.printAST(writer);
}
