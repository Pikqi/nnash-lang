const std = @import("std");
const ArrayList = std.ArrayList;
const Lexem = @import("../Lexical/lexer.zig").Lexem;

pub const Types = enum {
    INT,
    STRING,
    VOID,
    FLOAT,
    BOOL,
};

// program = {top_item} EOF
pub const Program = struct {
    top_items: ArrayList(TopItem),
    const Self = @This();
    pub fn dump(self: *const Self) void {
        for (self.top_items.items) |i| {
            i.dump();
        }
    }
};

pub const TopItem = union(enum) {
    block: SomeBlock,
    simple: TopSimpleItem,
};

pub const SomeBlock = union(enum) {
    ifBlock: *IfBlock,
    whileBlock: WhileBlock,
    funBlock: FunBlock,
};
pub const IfBlock = struct {
    condition: Condition,
    blockStatements: []TopItem,
    elseBlock: ?union(enum) {
        elif: *IfBlock,
        elseStatements: []TopItem,
    },
};
pub const WhileBlock = struct {
    condition: Condition,
    blockStatements: []TopItem,
};
pub const FunBlock = struct {
    name: []const u8,
    params: []VarDeclaration,
    blockStatements: []TopItem,
    returnType: Types,
};

// top_simple = (var_decl | var_decl_assign | call_and_maybe_assign | assign_stmt) STATMENT_END;
pub const TopSimpleItem = union(enum) {
    varDeclaration: VarDeclaration,
    varDeclarationAsign: VarDeclarationAsign,
    assignStatement: AssignStatement,
    callExpression: CallExpression,
    returnStatement: ReturnStatement,
};

pub const VarDeclaration = struct {
    type: Types,
    ident: []const u8,
    // dimensions: ?[]usize = null,
};

pub const VarDeclarationAsign = struct {
    varDeclaration: VarDeclaration,
    expr: Expression,
};

pub const CallMaybeAssign = struct {};

pub const AssignStatement = struct {
    expr: Expression,
    lvalue: LValue,
};
pub const LValue = struct {
    ident: []u8,
    index: ?*Tuple,
};

pub const ReturnStatement = union(enum) {
    expression: Expression,
    void: void,
};

pub const Condition = union(enum) {
    literal: bool,
    nested: struct {
        expressions: []Expression,
        operator: []RelOperator,
    },
};

pub const RelOperator = enum {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NEQ,
};

pub const Expression = union(enum) {
    aExpression: AExpression,
    callExpression: CallExpression,
    tupleExpression: *Tuple,
};
pub const AExpression = struct {
    muls: []Mul,
    ops: []PlusMinus,
};
pub const PlusMinus = enum { PLUS, MINUS };
pub const TimesDiv = enum { TIMES, DIV };
pub const Mul = struct {
    leftUnary: Unary,
    operand: ?TimesDiv = null,
    rightUnary: ?Unary = null,
};
pub const Unary = struct {
    sign: ?PlusMinus = null,
    power: *Power,
};

pub const Power = struct {
    primary: Primary,
    pow: ?*Power = null,
};

pub const Primary = union(enum) {
    primaryToken: PrimaryToken,
    expr: Expression,
};
pub const PrimaryToken = union(enum) {
    ident: Lexem,
    int_lit: Lexem,
    float_lit: Lexem,
    str_lit: Lexem,
    bool_lit: Lexem,
};

pub const Literal = enum {};
pub const IndexedIdent = struct {};
pub const Grouping = struct {
    expr: Expression,
};

pub const ArtihOp = struct {};

pub const CallExpression = struct {
    args: *Tuple,
    callExpressionContinue: *CallExpressionContinue,
};

pub const CallExpressionContinue = struct {
    name: []u8,
    callExpressionContinue: ?*CallExpressionContinue,
};

pub const Tuple = struct {
    exprs: []Expression,
};
