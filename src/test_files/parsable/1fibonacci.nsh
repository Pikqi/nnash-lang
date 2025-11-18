fun fib[[int:a]]:int
	int:a!
	int:b!
	int:c!
	0 >> a!
	1 >> b!
	if [n = 0]:
		<< 0!
	fi
	if [n = 1]:
		<< 1!
	fi
	while [i<n]
		a + b >> tmp!
		b >> a!
		tmp >> b!
	elihw
	<< b!
nuf

[10] | fib | @print!
