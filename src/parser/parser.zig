const std = @import("std");
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const lexer = @import("../Lexical/lexer.zig");
const Lexem = lexer.Lexem;
const TokenType = lexer.TokenType;
const ast_printer = @import("ast_printer.zig");

const ParserError = error{
    SyntaxError,
};

pub const Parser = struct {
    tokens: []Lexem,
    current: usize = 0,
    arena: *ArenaAllocator,
    alloc: Allocator,
    program: ?ast.Program = null,
    fn reportError(self: *Self, msg: []const u8) void {
        _ = self; // autofix
        std.debug.print("{s}", .{msg});
    }

    const Self = @This();
    pub fn init(tokens: []Lexem, arena: *ArenaAllocator) Self {
        return Self{
            .tokens = tokens,
            .arena = arena,
            .alloc = arena.allocator(),
        };
    }
    pub fn deinit(self: *Self) void {
        _ = self.arena.reset(.free_all);
    }
    pub fn printAST(self: *const Self, writer: *std.Io.Writer) !void {
        if (self.program == null) {
            try writer.print("Program not parsed", .{});
            return;
        }
        try ast_printer.dump(writer, self.program.?);
    }
    pub fn parse(self: *Self) void {
        self.program = .{
            // todo
            .top_items = ArrayList(*ast.TopItem).initCapacity(self.alloc, 20) catch unreachable,
        };
        self.parseProgram() catch {
            self.deinit();
        };
    }

    fn parseProgram(self: *Self) !void {
        while (!self.isAtEnd()) {
            try self.program.?.top_items.append(self.alloc, try self.topItem());
        }
    }

    fn topItem(self: *Self) !*ast.TopItem {
        const ti = try self.alloc.create(ast.TopItem);
        switch (self.peek().type) {
            .FUN_DEC => {
                // todo function declaration
            },
            .WHILE => {
                // todo while block
            },
            .IF => {
                // todo while block
            },
            else => {
                ti.* = .{ .simple = try self.topSimple() };
            },
        }
        return ti;
    }
    fn topSimple(self: *Self) !*ast.TopSimpleItem {
        const ts = try self.alloc.create(ast.TopSimpleItem);
        if (self.matchType() != null) {
            ts.* = .{ .varDeclaration = try self.varDeclaration() };
        }
        return ts;
    }
    // var_decl = type COLON (IDENT | IDENT array_dims)
    fn varDeclaration(self: *Self) !*ast.VarDeclaration {
        const vd = try self.alloc.create(ast.VarDeclaration);
        const type_lexem = self.matchType();
        if (type_lexem == null) {
            self.reportError("Tried to parse type declaration but previous token is not a type");
            return error.SyntaxError;
        }
        switch (type_lexem.?.type) {
            .INT => vd.type = .INT,
            .FLOAT => vd.type = .FLOAT,
            .STRING => vd.type = .STRING,
            .BOOL => vd.type = .BOOL,
            .VOID_LIT => vd.type = .VOID,
            else => unreachable,
        }
        _ = self.advance(); // consume type
        _ = try self.consume(.COLON, "Expected : after type");
        const ident_token = try self.consume(.IDENT, "Expected identifier");
        vd.ident = try self.alloc.dupe(u8, ident_token.str.?);
        _ = try self.consume(.STATMENT_END, "expected statement end");

        if (self.match(.LBRACKET) != null) {
            self.reportError("Array dims not implemented");
            return error.SyntaxError;
        }
        return vd;
    }
    // Utilities
    fn check(self: Self, token_type: TokenType) bool {
        return self.peek().type == token_type;
    }
    fn checkNext(self: *Parser, expected: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.tokens[self.current + 1].type == expected;
    }
    fn match(self: *Self, token_type: TokenType) ?Lexem {
        if (self.check(token_type)) {
            _ = self.advance();
            return self.previous();
        }
        return null;
    }

    fn matchOneOf(self: *Self, comptime expecteds: []const TokenType) ?Lexem {
        inline for (expecteds) |t_type| {
            if (self.check(t_type)) return self.advance();
        }
        return null;
    }
    fn matchType(self: *Self) ?Lexem {
        if (self.check(.INT) or self.check(.STRING) or self.check(.BOOL) or self.check(.FLOAT) or self.check(.VOID_LIT)) {
            return self.peek();
        }
        return null;
    }

    fn consume(self: *Parser, expected: TokenType, msg: []const u8) ParserError!Lexem {
        if (!self.check(expected)) {
            self.reportError(msg);
            return ParserError.SyntaxError;
        }
        _ = self.advance();
        return self.previous();
    }

    fn advance(self: *Parser) Lexem {
        if (self.isAtEnd()) return self.peek();

        self.current += 1;
        return self.previous();
    }
    fn peek(self: Self) Lexem {
        if (self.current >= self.tokens.len) {
            return self.tokens[self.current - 1];
        }
        return self.tokens[self.current];
    }

    fn previous(self: Self) Lexem {
        return self.tokens[self.current - 1];
    }
    fn isAtEnd(self: Self) bool {
        return self.current == self.tokens.len;
    }
};
