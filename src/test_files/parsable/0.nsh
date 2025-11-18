# comment
int:i!
0>>i!
(1 + 3) * 2 >>i!
-1 + 3 * 2 >>i!

2 * 2  ^ 3 ^ 4 >>i!

"string literal" >> int:moj_string!

[1, 2] | @max!
([1, 2] | @max | print | some_other_function) + 3 >> lval[0]!

# [([void] | funk1), ([0] | sqrt)] >> lval!

while [i < 10]
	int:i!
	<< 1!
	#[i] | @print!
	while [true]
		1 + i >> int:i!
	elihw
	while [ [true] | somefunc]
		1 + i >> int:i!
	elihw
elihw

fun fibonacci [[int:a, string:b]]:int
	[a-b] | print!
nuf


if [a<b]:
	int:i!
else if [a<b]:
	int:i!
	int:i!
else 
	int:i!
	int:i!
fi
