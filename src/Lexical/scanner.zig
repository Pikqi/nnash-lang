const std = @import("std");

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
