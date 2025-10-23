const std = @import("std");
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

test "parse all files" {
    const folder = try std.fs.cwd().openDir(example_folder, .{ .iterate = true });
    var iterator = try folder.walk(testing.allocator);
    defer iterator.deinit();
    while (try iterator.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        try tokenizeFile(entry.path);
    }
}

fn tokenizeFile(file_path: []const u8) !void {
    const dir = try std.fs.cwd().openDir(example_folder, .{});
    const file = try dir.openFile(file_path, .{});

    const contents = try file.readToEndAlloc(testing.allocator, 1024 * 1024 * 4);
    defer testing.allocator.free(contents);

    var lexer = try Lexer.init(contents, testing.allocator);
    defer lexer.deinit();
    try lexer.scanTokens();
}
