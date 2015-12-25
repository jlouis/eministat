# The Eministat library

Suppose you have two data sets, obtained from benchmarks of programs. The `eministat` library then computes basic statistical properties of the data sets and reports these on stdout. It can be used from the Erlang shell to verify that two samples are different according the the student's t-test.

## Acknowledgements

This program owes everything to Poul-Henning Kamp and his `ministat` application for FreeBSD.

# Example

Suppose you have two data sets:

	Chameleon = eministat:ds_from_list("chameleon", [150, 400, 720, 500, 930]).
	Iguana = eministat:ds_from_list("iguana", [50, 200, 150, 400, 750, 400, 150]).
	
Then you can analyse these on a 95.0 confidence interval:

	5> eministat:x(95.0, Iguana, Chameleon).
	    N           Min           Max        Median           Avg        Stddev
	x   7     50.000000     750.00000     200.00000     300.00000     238.04761
	+   5     150.00000     930.00000     500.00000     540.00000     299.08193
	No difference proven at 95.0% confidence
	ok

Now, for the purpose of showing the output when there is a difference, proven, we lower the CI to 80% in the following:

	6> eministat:x(80.0, Iguana, Chameleon).
	    N           Min           Max        Median           Avg        Stddev
	x   7     50.000000     750.00000     200.00000     300.00000     238.04761
	+   5     150.00000     930.00000     500.00000     540.00000     299.08193
	Difference at 80.0% confidence
	        240.000 +/- 212.215
	        80.0000% +/- 70.7384%
	        (Student's t, pooled s = 264.159)
	ok
