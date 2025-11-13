const std = @import("std");
const lexer_mod = @import("Lexical/lexer.zig");
const parser_mod = @import("parser/parser.zig");
const Parser = parser_mod.Parser;
const Lexer = @import("Lexical/lexer.zig").Lexer;
const testing = std.testing;
const example_folder = "src/test_files/examples";

// test "example 1" {
//     const example1 = @embedFile("../../test_files/examples/1.nsh");
//
//     var lexer = try Lexer.init(example1, testing.allocator);
//     defer lexer.deinit();
//     try lexer.scanTokens();
//     for (lexer.lexems.items) |lexem| {
//         std.debug.print("lexem: {t}\n", .{lexem.type});
//     }
// }

test "Tokenize all files" {
    const folder = try std.fs.cwd().openDir(example_folder, .{ .iterate = true });
    var iterator = try folder.walk(testing.allocator);
    defer iterator.deinit();
    while (try iterator.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        const lexems = try tokenizeFile(entry.path);
        testing.allocator.free(lexems);
    }
}

test "parse AST 1" {
    const lexems = try tokenizeFile("0.nsh");
    defer testing.allocator.free(lexems);
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    var parser = Parser.init(lexems, &arena);
    defer parser.deinit();
    var buff: [1024]u8 = undefined;

    parser.parse();
    const stdout = std.fs.File.stderr().writer(&buff);
    var writer = stdout.interface;
    try parser.printAST(&writer);
}

fn tokenizeFile(file_path: []const u8) ![]lexer_mod.Lexem {
    const dir = try std.fs.cwd().openDir(example_folder, .{});
    const file = try dir.openFile(file_path, .{});

    const contents = try file.readToEndAlloc(testing.allocator, 1024 * 1024 * 4);
    defer testing.allocator.free(contents);

    var lexer = try Lexer.init(contents, testing.allocator);
    defer lexer.deinit();
    try lexer.scanTokens();
    return lexer.lexems.toOwnedSlice(testing.allocator);
}
