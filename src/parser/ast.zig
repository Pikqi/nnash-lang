const std = @import("std");
const ArrayList = std.ArrayList;
const Lexem = @import("../Lexical/lexer.zig").Lexem;

const Types = enum {
    INT,
    STRING,
    VOID,
    FLOAT,
    BOOL,
};

// program = {top_item} EOF
pub const Program = struct {
    top_items: ArrayList(*TopItem),
    const Self = @This();
    pub fn dump(self: *const Self) void {
        for (self.top_items.items) |i| {
            i.dump();
        }
    }
};

pub const TopItem = union(enum) {
    block: *SomeBlock,
    simple: *TopSimpleItem,
};

pub const SomeBlock = union(enum) {
    ifBlock: IfBlock,
    whileBlock: WhileBlock,
    funBlock: FunBlock,
};
pub const IfBlock = struct {
    condition: Condition,
    blockStatements: ArrayList(BlockUnion),
};
pub const WhileBlock = struct {
    condition: Condition,
    blockStatements: ArrayList(BlockUnion),
};
pub const FunBlock = struct {};

pub const Block = struct {
    stmtOrBlockList: ArrayList(BlockUnion),
};

pub const BlockUnion = union(enum) { statement: Statement, blocks: SomeBlock };

pub const Statement = union(enum) {
    varDeclaration: VarDeclaration,
    varDeclarationAsign: VarDeclarationAsign,
    callMaybeAssign: CallMaybeAssign,
    assignStatement: AssignStatement,
    returnStatement: ReturnStatement,
};

// top_simple = (var_decl | var_decl_assign | call_and_maybe_assign | assign_stmt) STATMENT_END;
pub const TopSimpleItem = union(enum) {
    varDeclaration: *VarDeclaration,
    varDeclarationAsign: *VarDeclarationAsign,
    assignStatement: *AssignStatement,
    callExpression: *CallExpression,
};

pub const VarDeclaration = struct {
    type: Types,
    ident: []const u8,
    // dimensions: ?[]usize = null,
};

pub const VarDeclarationAsign = struct {
    varDeclaration: *VarDeclaration,
    expr: *Expression,
};

pub const CallMaybeAssign = struct {};

pub const AssignStatement = struct {
    expr: *Expression,
    ident: []u8,
};

pub const ReturnStatement = struct {};

pub const Condition = struct {};

pub const Expression = union(enum) {
    aExpression: *AExpression,
    callExpression: *CallExpression,
};
pub const AExpression = struct {
    muls: []*Mul,
    ops: []PlusMinus,
};
pub const PlusMinus = enum { PLUS, MINUS };
pub const TimesDiv = enum { TIMES, DIV };
pub const Mul = struct {
    leftUnary: *Unary,
    operand: ?*TimesDiv,
    rightUnary: ?*Unary,
};
pub const Unary = struct {
    sign: ?PlusMinus,
    power: *Power,
};

pub const Power = struct {
    primary: *Primary,
    pow: ?*Power,
};

pub const Primary = union(enum) {
    number: Number,
    expr: *Expression,
};
pub const Number = union(enum) {
    ident: Lexem,
    int_lit: Lexem,
    float_lit: Lexem,
};

pub const Literal = enum {};
pub const IndexedIdent = struct {};
pub const Grouping = struct {
    expr: Expression,
};

pub const ArtihOp = struct {};

pub const CallExpression = struct {};
