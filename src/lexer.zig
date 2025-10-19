const std = @import("std");
const Allocator = std.mem.Allocator;

const isDigit = std.ascii.isDigit;
const isAlphabetic = std.ascii.isAlphabetic;

const LexerError = error{
    CharacterInIntLiteral,
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
    //Literals
    INT_LIT,
    FLOAT_LIT,
    STRING_LIT,
    VOID_LIT, //maybe?
    // misc
    STATMENT_END,
    COMMA,
    COLON,
    ASSIGN,
    BUILTIN_FUN,
    PIPE,
    EOF,
};
fn isIdentifierStart(char: u8) bool {
    return std.ascii.isAlphabetic(char) or char == '_';
}

// todo keywords map
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

    pub fn init(content: []const u8, alloc: Allocator) !Lexer {
        return Lexer{
            .allocator = alloc,
            .lexems = try std.ArrayList(Lexem).initCapacity(alloc, 100),
            .source = content,
            .sc = ScannerCore{ .source = content },
        };
    }
    pub fn scanTokens(self: *Lexer) !void {
        while (!self.sc.isAtEnd()) {
            self.sc.startToken();
            try self.scanToken();
        }
    }

    pub fn scanToken(self: *Lexer) !void {
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
                    // } else if (isIdentifierStart(c)) {}
                } else {
                    return error.UnkownLex;
                }
            },
        }
    }
    fn add(self: *Lexer, t: TokenType) !void {
        const lex = self.getLexemFromType(t);
        try self.lexems.append(self.allocator, lex);
    }

    fn addString(self: *Lexer, t: TokenType, str: []const u8) !void {
        var lex = self.getLexemFromType(t);
        lex.str = str;
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

    pub fn identifier(self: *Lexer) !void {
        _ = self; // autofix
    }

    pub fn indentifierOrKeyword(self: *Lexer) !void {
        _ = self; // autofix
    }

    pub fn addLiteralInt(self: *Lexer, str: []const u8) !void {
        const parsed = try std.fmt.parseInt(u64, str, 10);
        var lexem = self.getLexemFromType(.INT_LIT);
        lexem.str = str;
        lexem.value = parsed;
        try self.lexems.append(self.allocator, lexem);
    }

    pub fn deinit(self: *Lexer) void {
        self.lexems.clearAndFree(self.allocator);
        self.lexems.deinit(self.allocator);
        self.* = undefined;
    }
};

pub const ScannerCore = struct {
    source: []const u8,
    // curr actually points to the next char to be lexed
    curr: usize = 0,
    curr_line: u64 = 0,
    curr_col: u64 = 0,

    token_start: u64 = 0,
    token_line: u64 = 0,
    token_col: u64 = 0,
    pub fn isAtEnd(self: *ScannerCore) bool {
        return self.curr >= self.source.len;
    }

    pub fn advance(self: *ScannerCore) ?u8 {
        if (self.isAtEnd()) {
            return null;
        }
        const curr_char = self.source[self.curr];
        if (curr_char == '\n') {
            self.curr_line += 1;
            self.curr_col = 0;
        } else {
            self.curr_col += 1;
        }
        self.curr += 1;
        return curr_char;
    }

    pub fn startToken(self: *ScannerCore) void {
        self.token_start = self.curr;
        self.token_line = self.curr_line;
        self.token_col = self.curr_col;
    }

    pub fn match(self: *ScannerCore, char: u8) bool {
        if (self.isAtEnd() or self.source[self.curr] != char) {
            return false;
        }
        self.curr += 1;
        self.curr_col += 1;
        return true;
    }
    pub fn matchSlice(self: *ScannerCore, str: []const u8) bool {
        if (self.curr + str.len - 1 >= self.source.len) {
            return false;
        }
        const is_match = std.mem.eql(
            u8,
            self.source[self.curr .. self.curr + str.len],
            str,
        );

        if (is_match) {
            self.curr += str.len;
            self.curr_col += str.len;
        }
        return is_match;
    }

    pub fn peek(self: *ScannerCore) ?u8 {
        if (self.isAtEnd()) {
            return null;
        }
        return self.source[self.curr];
    }
    pub fn peekNext(self: *ScannerCore) ?u8 {
        if (self.curr + 1 >= self.source.len) {
            return null;
        }
        return self.source[self.curr + 1];
    }
};

// ********** Lexer tests **********
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
    try std.testing.expectError(LexerError.CharacterInIntLiteral, lexer.scanTokens());

    defer lexer.deinit();
}

// ********** Scanner core tests **********
test "ScannerCore basics" {
    const s1 = "Some nice string";
    var sc = ScannerCore{ .source = s1[0..] };
    const first = sc.advance();
    try std.testing.expectEqual('S', first.?);

    const first_match = sc.matchSlice("ome");
    try std.testing.expect(first_match);

    const second = sc.advance();

    try std.testing.expectEqual(' ', second.?);
    try std.testing.expectEqual('n', sc.advance());
    const third = sc.match('i');
    try std.testing.expect(third);
    try std.testing.expectEqual('c', sc.peek().?);
    try std.testing.expectEqual('e', sc.peekNext());

    const fourth = sc.advance().?;
    try std.testing.expectEqual('c', fourth);

    var last_token: u8 = 0;
    while (sc.advance()) |tok| {
        last_token = tok;
    }
    try std.testing.expectEqual('g', last_token);
}
