
fun fibRec[[int:n]]:int
	if [n <= 1]:
		<< n!
	else
		[n-1] | fibRec >> int:f1!
		[n-2] | fibRec >> int:f2!
		f1 + f2 >> int:r!
		<< r!
	fi
nuf

