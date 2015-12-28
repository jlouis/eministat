# The Eministat library

Suppose you have two data sets, obtained from benchmarks of programs. The `eministat` library then computes basic statistical properties of the data sets and reports these on stdout. It can be used from the Erlang shell to verify that two samples are different according the the student's t-test.

## Acknowledgements

This program owes everything to Poul-Henning Kamp and his `ministat` application for FreeBSD.

# Examples

Suppose you have measured the blade thickness in (μs) of a Ligustrum, one in the sun and one in the shade. You wonder if there is any measurable difference between these two data sets. This is the question `eministat` can answer:

	1> l(eministat).
	{module,eministat}
	2> rr(eministat).
	[dataset,plot]
	3> Sun = eministat:ligustrum_sun().
	#dataset{name = "sun",
	         points = [100,150,200,210,210,300,300],
	         sy = 1470.0,syy = 3.407e5,n = 7}
	4> Shade = eministat:ligustrum_shade().
	#dataset{name = "shade",
	         points = [120,125,130,160,170,200,200],
	         sy = 1105.0,syy = 181425.0,n = 7}
	5> eministat:x(95.0, Sun, Shade).
	x sun
	+ shade
	+--------------------------------------------------------------------------+
	|x      + + +      x   +   +          *  x                                x|
	|                                     +  x                                x|
	|             |__________________________A__________________________|      |
	|         |___________AM___________|                                       |
	+--------------------------------------------------------------------------+
	Dataset: x N=7 CI=95.0000
	Statistic     Value     [     Bias] (SE)
	Min:          100.00000
	Median:       210.00000 [   0.2656] (± 37.0278)
	Max:          300.00000
	Average:      210.00000 [-1.034e-2] (± 25.4438)
	Std. Dev:     73.029674 [   -7.156] (± 15.5258)

	Dataset: + N=7 CI=95.0000
	Statistic     Value     [     Bias] (SE)
	Min:          120.00000
	Median:       160.00000 [   -4.263] (± 23.0715)
	Max:          200.00000
	Average:      157.85714 [ 1.422e-2] (± 11.9664)
	Std. Dev:     34.139071 [   -3.070] (± 5.82677)

	No difference proven at 95.0% confidence
	ok

This means for the 7 samples of each Ligustrum, the null hypothesis failed to be rejected, so for this data set, there is no significant difference. Either collect more samples, or be satisfied with the result.

In programming, we typically measure the performance of different algorithms and pick "the best". But looks can be deceiving. We could wonder if `lists:reverse/1` is faster than tail-recursive reverse variant

	tail_reverse(L) -> tail_reverse(L, []).

	tail_reverse([], Acc) -> Acc;
	tail_reverse([X | Xs], Acc) -> tail_reverse(Xs, [X | Acc]).

which may or may not be true. We can use the `eministat:s/3` function to sample The above:

	L = lists:seq(1, 100000),
	Rev2 = eministat:s("tail_reverse/1", fun() -> tail_reverse(L) end, 50).

This will sample 50 runs of our `tail_reverse/1` function. Likewise, we can grab the output of `lists:reverse/1`:

	Rev1 = eministat:s("lists:reverse/1", fun() -> lists:reverse(L) end, 50).

And finally, we can ask `eministat` if there is any difference between the data sets:

	7> eministat:x(95.0, L1, L2). 
	x lists:reverse/1
	+ tail_reverse/1
	+--------------------------------------------------------------------------+
	| xxxxxxxx+*+**++++x+++++++++ + x x  +                     +              +|
	|  xxxx  x+*+xx+ ++ ++++++                                                 |
	|  xxx    +*+ x+  + +  + +                                                 |
	|  xxx     *+  +         +                                                 |
	|  xxx     *             +                                                 |
	|  xxx     +                                                               |
	|  xxx     +                                                               |
	||___M__A_____|                                                            |
	|        |________M_A___________|                                          |
	+--------------------------------------------------------------------------+
	Dataset: x N=50 CI=95.0000
	Statistic     Value     [     Bias] (SE)
	Min:          186.00000
	1st Qu.       285.00000
	Median:       346.00000 [   -1.563] (± 28.3694)
	3rd Qu.       740.00000
	Max:          2388.0000
	Average:      533.26000 [-5.954e-2] (± 62.5871)
	Std. Dev:     448.29863 [   -15.59] (± 98.5641)
	Outliers: 0 + 2 = 2
	
	Dataset: + N=50 CI=95.0000
	Statistic     Value     [     Bias] (SE)
	Min:          722.00000
	1st Qu.       845.00000
	Median:       1274.0000 [    2.540] (± 124.018)
	3rd Qu.       1685.0000
	Max:          5114.0000
	Average:      1416.7400 [   0.1380] (± 111.954)
	Std. Dev:     796.76860 [   -32.35] (± 194.016)
	Outliers: 0 + 2 = 2
	
	Difference at 95.0% confidence
	        883.480 ± 256.514
	        165.675% ± 48.1030%
	        (Student's t, pooled s = 646.456)
	ok

In this case, we are told there is a significant difference between the two runs of about 880μs, with an uncertainty of about 256μs. Hence we can rely on the call to `lists:reverse/1` being faster than a tail recursive variant for lists of size `100000`.

Both datasets have 2 outliers, which are values far from the 

# Usage

To use `eministat`, your data must be normally distributed. You will have to make a reasonable guess that they are, before you can use the mathematics. Once you know that to be the case, you can use `eministat` to analyze your data sets.

The `eministat` application supports 3 major functions:

	DataSet = eministat:ds_from_list(Name, DataPoints),

will construct a new data set from a list of already measured data points.

The function `s/3` can be used to sample

	DataSet = eministat:s(Name, Function, N)

will run `Function` `N` times and collect the run-time in microseconds for each sample. It will then stuff the resulting data points into a dataset with `Name`.

Finally, to analyze two data sets or more, use the `x/3` function

	eministat:x(ConfidenceLevel, DataSet1, DataSet2), %% or
	eministat:x(ConfidenceLevel, BaseSet, [DataSet, …]),

where the `ConfidenceLevel` is one of `[80.0, 90.0, 95.0, 98.0, 99.0, or 99.5]`. The output is as above on stdout.

# Description of the output

TODO
