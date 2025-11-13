pub const Parser = @import("parser.zig").Parser;
pub const ast_printer = @import("ast_printer.zig");

test "import" {
    _ = @import("./ast.zig");
    _ = @import("./parser.zig");
}
