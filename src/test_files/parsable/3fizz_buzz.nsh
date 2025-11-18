
fun fizzbuzz[[int:n]]:void
	int:i!
	while [i<n]
		if [i % 15 = 0]:
			["fizzbuzz"] | @print!
		else if [i % 3 = 0]:
			["fizz"] | @print!
		else if [i % 5 = 0]:
			["buzz"] | @print!
		else
			[i] | @print!
		fi
	elihw
nuf
