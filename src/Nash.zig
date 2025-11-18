const std = @import("std");
const root = @import("root.zig");
const Lexer = root.lexer.Lexer;
const Parser = root.parser.Parser;

lexer: Lexer,
parser: Parser,
tokens: []root.lexer.Lexem,
alloc: std.mem.Allocator,
file_contents: []const u8,

const Self = @This();

pub fn parseFile(file_path: []const u8, alloc: std.mem.Allocator, writer: ?*std.Io.Writer) !Self {
    const file = try std.fs.cwd().openFile(file_path, .{ .mode = .read_only });
    const contents = try file.readToEndAlloc(alloc, 1024 * 1024 * 4);

    var lexer = try Lexer.init(contents, alloc);
    errdefer {
        alloc.free(contents);
        file.close();
        lexer.deinit();
    }
    try lexer.scanTokens();
    const tokens = try lexer.lexems.toOwnedSlice(alloc);

    var parser = try Parser.init(tokens, alloc);

    errdefer {
        if (writer) |w| {
            parser.print_error_msg(w, contents) catch {};
        }
        parser.deinit();
        alloc.free(tokens);
    }
    try parser.parse();
    return .{
        .lexer = lexer,
        .parser = parser,
        .tokens = tokens,
        .file_contents = contents,
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    self.lexer.deinit();
    self.parser.deinit();
    self.alloc.free(self.tokens);
    self.alloc.free(self.file_contents);
}
