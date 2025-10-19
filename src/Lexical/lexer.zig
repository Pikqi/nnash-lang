const std = @import("std");
const Allocator = std.mem.Allocator;
const ScannerCore = @import("scanner.zig").ScannerCore;

const isDigit = std.ascii.isDigit;
const isAlphabetic = std.ascii.isAlphabetic;

const LexerError = error{
    CharacterInIntLiteral,
    UnkownToken,
};

const TokenType = enum {
    // Arithmetic
    ADD,
    SUB,
    TIMES,
    DIV,
    MOD,
    // logic
    LT,
    LE,
    GT,
    GE,
    EQ,
    NEQ,
    //blocks
    WHILE,
    ENDWHILE,
    FUN_DEC,
    END_FUN_DEC,
    RETURN,
    // Parens and brackets
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACKET_DOUBLE,
    RBRACKET_DOUBLE,
    // Types
    INT,
    FLOAT,
    STRING,
    BOOL,
    //Literals
    INT_LIT,
    FLOAT_LIT,
    STRING_LIT,
    VOID_LIT, //maybe?
    // misc
    IDENTIFIER,
    STATMENT_END,
    COMMA,
    COLON,
    ASSIGN,
    BUILTIN_FUN,
    PIPE,
    EOF,
};
fn isIdentifierStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}
fn isIdentifierPart(c: u8) bool {
    return isIdentifierStart(c) or isDigit(c);
}

const Lexem = struct {
    type: TokenType,
    str: ?[]const u8 = null,
    line: u64 = 0,
    col_start: u64 = 0,
    col_end: u64 = 0,
    value: ?u64 = 0,
};

pub const Lexer = struct {
    lexems: std.ArrayList(Lexem),
    allocator: Allocator,
    sc: ScannerCore,
    source: []const u8,
    error_msg: ?[]const u8 = null,
    print_msg_on_erorr: bool = true,

    pub fn init(content: []const u8, alloc: Allocator) !Lexer {
        return Lexer{
            .allocator = alloc,
            .lexems = try std.ArrayList(Lexem).initCapacity(alloc, 100),
            .source = content,
            .sc = ScannerCore{ .source = content },
        };
    }
    pub fn scanTokens(self: *Lexer) !void {
        errdefer {
            self.generateErrorMessageInvalidToken() catch unreachable;
            if (self.print_msg_on_erorr) {
                std.debug.print("{s}", .{self.error_msg.?});
            }
        }
        while (!self.sc.isAtEnd()) {
            self.sc.startToken();
            try self.scanToken();
        }
    }
    fn generateErrorMessageInvalidToken(self: *Lexer) !void {
        var writer = std.Io.Writer.Allocating.init(self.allocator);
        try writer.writer.print("Invalid token found at line: {d} col: {d}\n", .{ self.sc.token_line, self.sc.token_col });
        var a = std.mem.splitAny(u8, self.source, "\n");
        var line: []const u8 = undefined;
        for (0..self.sc.token_line) |_| {
            line = a.next().?;
        }
        const line_dupe = try self.allocator.dupe(u8, line);
        defer self.allocator.free(line_dupe);
        std.mem.replaceScalar(u8, line_dupe, '\t', ' ');
        try writer.writer.print("{s}\n", .{line_dupe});
        for (0..self.sc.token_col - 1) |_| {
            try writer.writer.print(" ", .{});
        }
        try writer.writer.print("^\n", .{});
        const result = try writer.toOwnedSlice();
        if (self.error_msg != null) {
            self.allocator.free(self.error_msg.?);
        }
        self.error_msg = result;
    }

    fn scanToken(self: *Lexer) !void {
        const char_optional = self.sc.advance();
        if (char_optional == null) {
            return;
        }
        const c = char_optional.?;
        switch (c) {
            '(' => try self.add(.LPAREN),
            ')' => try self.add(.RPAREN),
            ',' => try self.add(.COMMA),
            ':' => try self.add(.COLON),
            '+' => try self.add(.ADD),
            '-' => try self.add(.SUB),
            '*' => try self.add(.TIMES),
            '/' => try self.add(.DIV),
            '%' => try self.add(.MOD),
            ' ', '\n', '\t' => {},
            '>' => {
                if (self.sc.match('>')) {
                    try self.add(.ASSIGN);
                } else if (self.sc.match('=')) {
                    try self.add(.GE);
                } else {
                    try self.add(.GT);
                }
            },
            '<' => {
                if (self.sc.match('<')) {
                    try self.add(.RETURN);
                } else if (self.sc.match('=')) {
                    try self.add(.LE);
                } else {
                    try self.add(.LT);
                }
            },
            '[' => try self.add(if (self.sc.match('[')) .LBRACKET_DOUBLE else .LBRACKET),
            ']' => try self.add(if (self.sc.match(']')) .RBRACKET_DOUBLE else .RBRACKET),
            '!' => try self.add(if (self.sc.match('=')) .NEQ else .STATMENT_END),
            else => {
                if (isDigit(c)) {
                    try self.number();
                } else if (isIdentifierStart(c)) {
                    try self.indentifierOrKeyword();
                } else if (c == '@') {
                    try self.builtIn();
                } else {
                    return LexerError.UnkownToken;
                }
            },
        }
    }
    fn add(self: *Lexer, t: TokenType) !void {
        const lex = self.getLexemFromType(t);
        try self.lexems.append(self.allocator, lex);
    }

    fn getLexemFromType(self: *Lexer, t: TokenType) Lexem {
        return Lexem{
            .type = t,
            .col_start = self.sc.token_col,
            .col_end = self.sc.curr_col,
            .line = self.sc.token_line,
        };
    }

    pub fn number(self: *Lexer) !void {
        while (!self.sc.isAtEnd() and isDigit(self.sc.peek().?)) {
            _ = self.sc.advance();
        }
        const text = self.sc.source[self.sc.token_start..self.sc.curr];
        const next_char = self.sc.peek();
        if (next_char) |next| {
            if (isAlphabetic(next)) {
                return LexerError.CharacterInIntLiteral;
            }
        }
        try self.addLiteralInt(text);
    }

    pub fn indentifierOrKeyword(self: *Lexer) !void {
        while (!self.sc.isAtEnd() and isIdentifierPart(self.sc.peek().?)) {
            _ = self.sc.advance();
        }
        const text = self.sc.source[self.sc.token_start..self.sc.curr];
        const k = keywords.get(text);
        if (k) |keyword| {
            try self.add(keyword);
        } else {
            try self.addIdentifier(text);
        }
    }

    pub fn builtIn(self: *Lexer) !void {
        while (!self.sc.isAtEnd() and isAlphabetic(self.sc.peek().?)) {
            _ = self.sc.advance();
        }
        const text = self.sc.source[self.sc.token_start + 1 .. self.sc.curr];
        try self.addBuiltin(text);
    }

    fn addLiteralInt(self: *Lexer, str: []const u8) !void {
        const parsed = try std.fmt.parseInt(u64, str, 10);
        var lexem = self.getLexemFromType(.INT_LIT);
        lexem.str = str;
        lexem.value = parsed;
        try self.lexems.append(self.allocator, lexem);
    }

    fn addIdentifier(self: *Lexer, str: []const u8) !void {
        var lexem = self.getLexemFromType(.IDENTIFIER);
        lexem.str = str;
        try self.lexems.append(self.allocator, lexem);
    }
    fn addBuiltin(self: *Lexer, str: []const u8) !void {
        var lexem = self.getLexemFromType(.BUILTIN_FUN);
        lexem.str = str;
        try self.lexems.append(self.allocator, lexem);
    }

    pub fn deinit(self: *Lexer) void {
        self.lexems.clearAndFree(self.allocator);
        self.lexems.deinit(self.allocator);
        if (self.error_msg) |err_msg| {
            self.allocator.free(err_msg);
        }
        self.* = undefined;
    }
};

const keywords = std.static_string_map.StaticStringMap(TokenType).initComptime(&.{
    .{ "while", TokenType.WHILE },
    .{ "elihw", TokenType.ENDWHILE },
    .{ "fun", TokenType.FUN_DEC },
    .{ "nuf", TokenType.END_FUN_DEC },
    .{ "int", TokenType.INT },
    .{ "string", TokenType.STRING },
    .{ "boole", TokenType.BOOL },
    .{ "float", TokenType.FLOAT },
});

test "Lexer simple lexems only" {
    const input = "<> <= >= >> << ! ()[][[]] -+*/,:!=100";
    const expected_lexem_types = [_]TokenType{ .LT, .GT, .LE, .GE, .ASSIGN, .RETURN, .STATMENT_END, .LPAREN, .RPAREN, .LBRACKET, .RBRACKET, .LBRACKET_DOUBLE, .RBRACKET_DOUBLE, .SUB, .ADD, .TIMES, .DIV, .COMMA, .COLON, .NEQ, .INT_LIT };
    var lexer = try Lexer.init(input, std.testing.allocator);
    try lexer.scanTokens();
    try std.testing.expectEqual(expected_lexem_types.len, lexer.lexems.items.len);

    for (lexer.lexems.items, expected_lexem_types) |my_lexem, expected_lexem| {
        try std.testing.expectEqual(expected_lexem, my_lexem.type);
    }

    defer lexer.deinit();
}

test "Lexer number literals" {
    const input = "100 20 30 01111 0000999999999";
    const expected_lexem_values = [_]u64{ 100, 20, 30, 1111, 999999999 };
    var lexer = try Lexer.init(input, std.testing.allocator);
    try lexer.scanTokens();
    try std.testing.expectEqual(expected_lexem_values.len, lexer.lexems.items.len);

    for (lexer.lexems.items, expected_lexem_values) |my_lexem, expected_lexem| {
        try std.testing.expectEqual(expected_lexem, my_lexem.value.?);
    }

    defer lexer.deinit();
}
test "Lexer bad number" {
    const input = "10 20bcd";
    var lexer = try Lexer.init(input, std.testing.allocator);
    lexer.print_msg_on_erorr = false;
    try std.testing.expectError(LexerError.CharacterInIntLiteral, lexer.scanTokens());

    defer lexer.deinit();
}

test "Lexer keywords" {
    const input = "while elihw fun nuf int string boole float i_am_not_a_keyword";
    const expected_lexem_types = [_]TokenType{ .WHILE, .ENDWHILE, .FUN_DEC, .END_FUN_DEC, .INT, .STRING, .BOOL, .FLOAT, .IDENTIFIER };
    var lexer = try Lexer.init(input, std.testing.allocator);
    try lexer.scanTokens();
    try std.testing.expectEqual(expected_lexem_types.len, lexer.lexems.items.len);

    for (lexer.lexems.items, expected_lexem_types) |my_lexem, expected_lexem| {
        try std.testing.expectEqual(expected_lexem, my_lexem.type);
    }
    try std.testing.expectEqualStrings("i_am_not_a_keyword", lexer.lexems.getLast().str.?);

    defer lexer.deinit();
}

test "Lexer builtin functions" {
    const input = "@print @max";
    const expected_lexem_types = [_]TokenType{ .BUILTIN_FUN, .BUILTIN_FUN };
    var lexer = try Lexer.init(input, std.testing.allocator);
    try lexer.scanTokens();
    try std.testing.expectEqual(expected_lexem_types.len, lexer.lexems.items.len);

    for (lexer.lexems.items, expected_lexem_types) |my_lexem, expected_lexem| {
        try std.testing.expectEqual(expected_lexem, my_lexem.type);
    }
    try std.testing.expectEqualStrings("print", lexer.lexems.items[0].str.?);
    try std.testing.expectEqualStrings("max", lexer.lexems.items[1].str.?);

    defer lexer.deinit();
}
