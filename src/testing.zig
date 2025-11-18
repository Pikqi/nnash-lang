const std = @import("std");
const lexer_mod = @import("Lexical/lexer.zig");
const parser_mod = @import("parser/parser.zig");
const Parser = parser_mod.Parser;
const Lexer = @import("Lexical/lexer.zig").Lexer;
const testing = std.testing;
const Nash = @import("Nash.zig");

const parsable_folder = "src/test_files/parsable";
const non_parsable_folder = "src/test_files/parsing_error";

test "Tokenize all files" {
    const folder = try std.fs.cwd().openDir(parsable_folder, .{ .iterate = true });
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

test "parsable files" {
    var folder = try std.fs.cwd().openDir(parsable_folder, .{ .iterate = true });
    defer folder.close();
    var iterator = try folder.walk(testing.allocator);
    defer iterator.deinit();
    while (try iterator.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        const path = try folder.realpathAlloc(testing.allocator, entry.path);
        defer testing.allocator.free(path);
        var nash = try Nash.parseFile(path, testing.allocator, null);
        defer nash.deinit();
    }
}

test "not parsable files" {
    var folder = try std.fs.cwd().openDir(non_parsable_folder, .{ .iterate = true });
    defer folder.close();
    var iterator = try folder.walk(testing.allocator);
    defer iterator.deinit();
    while (try iterator.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        const path = try folder.realpathAlloc(testing.allocator, entry.path);
        defer testing.allocator.free(path);
        const nash = Nash.parseFile(path, testing.allocator, null);
        try testing.expectError(error.SyntaxError, nash);
    }
}

test "Lexical erorrs" {
    var folder = try std.fs.cwd().openDir(non_parsable_folder, .{ .iterate = true });
    defer folder.close();
    var iterator = try folder.walk(testing.allocator);
    defer iterator.deinit();
    while (try iterator.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }
        const path = try folder.realpathAlloc(testing.allocator, entry.path);
        defer testing.allocator.free(path);
        const nash = Nash.parseFile(path, testing.allocator, null);
        try testing.expectError(error.SyntaxError, nash);
    }
}

fn tokenizeFile(file_path: []const u8) ![]lexer_mod.Lexem {
    const dir = try std.fs.cwd().openDir(parsable_folder, .{});
    const file = try dir.openFile(file_path, .{});

    const contents = try file.readToEndAlloc(testing.allocator, 1024 * 1024 * 4);
    defer testing.allocator.free(contents);

    var lexer = try Lexer.init(contents, testing.allocator);
    defer lexer.deinit();
    try lexer.scanTokens();
    return lexer.lexems.toOwnedSlice(testing.allocator);
}
