# The Eministat library

Suppose you have two data sets, obtained from benchmarks of programs. The `eministat` library then computes basic statistical properties of the data sets and reports these on stdout. It can be used from the Erlang shell to verify that two samples are different according the the student's t-test.

## Acknowledgements

This program owes everything to Poul-Henning Kamp and his `ministat` application for FreeBSD, which was the first inspiration. It also draw a lot on Bryan O'Sullivan's Criterion benchmark suite of Haskell fame. Finally, it uses some ideas by Brent Boyer for computing outlier variance.

# Examples

Suppose you have measured the blade thickness in (μs) of a Ligustrum, one in the sun and one in the shade. You wonder if there is any measurable difference between these two data sets. This is the question `eministat` can answer:

	7> eministat:x(95.0, Sun, Shade).
	x sun
	+ shade
	+--------------------------------------------------------------------------+
	|x      + + +      x   +   +          *  x                                x|
	|                                     +  x                                x|
	|             |__________________________A__________________________|      |
	|         |___________AM___________|                                       |
	+--------------------------------------------------------------------------+
	Dataset: x N=7 CI=95.0000
	Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
	Min:            100.000
	1st Qu.         150.000
	Median:         210.000
	3rd Qu.         210.000
	Max:            300.000
	Average:        210.000 [     0.144571] (      158.571 ‥       258.571)
	Std. Dev:       73.0297 [     -6.93032] (      46.0848 ‥       101.770)
	
	Outliers: 0/0 = 0 (μ=210.145, σ=66.0994)
	        Outlier variance:      0.843583 (severe, the data set is probably unusable)
	
	------
	
	Dataset: + N=7 CI=95.0000
	Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
	Min:            120.000
	1st Qu.         125.000
	Median:         160.000
	3rd Qu.         170.000
	Max:            200.000
	Average:        157.857 [    -0.137286] (      135.714 ‥       182.857)
	Std. Dev:       34.1391 [     -3.10808] (      23.4267 ‥       41.9041)
	
	Outliers: 0/0 = 0 (μ=157.720, σ=31.0310)
	        Outlier variance:      0.555845 (severe, the data set is probably unusable)
	
	No difference proven at 95.0% confidence
	------
	
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

	8> Rev1 = eministat_ts:reverse_1().
	{dataset,"lists:reverse/1",
	         [3078,3085,3095,3096,3110,3126,3142,3239,3272,3513],
	         31756.0,101009964.0,10}
	9> Rev2 = eministat_ts:reverse_2().
	{dataset,"tail_reverse/1",
	         [4104,4140,4155,4157,4161,4173,4185,4189,4191,4192,4197,
	          4203,4213,4225,4461],
	         62946.0,264234764.0,15}
	10> eministat:x(95.0, Rev1, Rev2).
	x lists:reverse/1
	+ tail_reverse/1
	+--------------------------------------------------------------------------+
	|  xxxx    x x           x                              + +++++           +|
	|  xxx                                                    ++++             |
	|                                                         + +              |
	|                                                           +              |
	|                                                           +              |
	||___M__A______|                                                           |
	|                                                       |___A___|          |
	+--------------------------------------------------------------------------+
	Dataset: x N=10 CI=95.0000
	Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
	Min:            3078.00
	1st Qu.         3095.00
	Median:         3110.00
	3rd Qu.         3239.00
	Max:            3513.00
	Average:        3175.60 [     0.593130] (      3118.60 ‥       3297.30)
	Std. Dev:       135.651 [     -14.7128] (      61.9459 ‥       216.422)
	
	Outliers: 0/1 = 1 (μ=3176.19, σ=120.938)
	        Outlier variance:    9.14370e-2 (slight)
	
	------
	
	Dataset: + N=15 CI=95.0000
	Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
	Min:            4104.00
	1st Qu.         4157.00
	Median:         4189.00
	3rd Qu.         4197.00
	Max:            4461.00
	Average:        4196.40 [   7.09000e-2] (      4170.67 ‥       4260.60)
	Std. Dev:       79.3589 [     -9.85382] (      26.3869 ‥       143.088)
	
	Outliers: 0/1 = 1 (μ=4196.47, σ=69.5050)
	        Outlier variance:    6.22222e-2 (slight)
	
	Difference at 95.0% confidence
	        1020.80 ± 88.7257
	        32.1451% ± 2.79398%
	        (Student's t, pooled s = 105.042)
	------
	
	ok

In this case, we are told there is a significant difference between the two runs of about 1020.80μs, with an uncertainty of about 89μs. Hence we can rely on the call to `lists:reverse/1` being faster than a tail recursive variant for lists of size `100000`.

Both datasets have 1 outlier, which are values far from the mean. An analysis also shows the analysis unlikely to be affected by outliers, as the variance is only slight.

# Usage

To use `eministat`, your data must be normally distributed. You will have to make a reasonable guess that they are, before you can use the mathematics. Once you know that to be the case, you can use `eministat` to analyze your data sets.

The `eministat` application supports 3 major functions:

	DataSet = eministat_ds:from_list(Name, DataPoints),

will construct a new data set from a list of already measured data points.

The function `s/3` can be used to sample

	DataSet = eministat:s(Name, Function, N)

will run `Function` `N` times and collect the run-time in microseconds for each sample. It will then stuff the resulting data points into a dataset with `Name`. The s/3 function will first warm up by running the test for 3 seconds before actually starting to measure. This avoids numerous problems with CPU frequency scaling. It also garbage collects before each measurement.

Finally, to analyze two data sets or more, use the `x/3` function

	eministat:x(ConfidenceLevel, DataSet1, DataSet2), %% or
	eministat:x(ConfidenceLevel, BaseSet, [DataSet, …]),

where the `ConfidenceLevel` is one of `[80.0, 90.0, 95.0, 98.0, 99.0, or 99.5]`. The output is as above on stdout.

# Description of the output

The `eministat` output contains 4 sections:

	HISTOGRAM
	For each dataset:
		VITALS
		OUTLIER ANALYSIS
	STUDENT'S T
	
## HISTOGRAM

	x lists:reverse/1
	+ tail_reverse/1
	+--------------------------------------------------------------------------+
	|  xxxx    x x           x                              + +++++           +|
	|  xxx                                                    ++++             |
	|                                                         + +              |
	|                                                           +              |
	|                                                           +              |
	||___M__A______|                                                           |
	|                                                       |___A___|          |
	+--------------------------------------------------------------------------+

The histogram part shows you an overview of the data distribution in ASCII art. In this case, there are two data sets, `+` and `x` and for each value in the data set, they are plotted in ASCII ,so you can see the distribution. If points are overlapping, a `*` would be printed in the diagram. Below the distribution plot, you have two bars, one for each dataset. The `M` and `A` are the median and average/mean respectively. In the case of the 2nd dataset, the median and average are at the same point, so only the `A` is plotted. The part with `|____…____|` signifies 1 standard deviation assuming a normally distributed data set.

## VITALS

	Dataset: x N=10 CI=95.0000
	Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
	Min:            3078.00
	1st Qu.         3095.00
	Median:         3110.00
	3rd Qu.         3239.00
	Max:            3513.00
	Average:        3175.60 [     0.593130] (      3118.60 ‥       3297.30)
	Std. Dev:       135.651 [     -14.7128] (      61.9459 ‥       216.422)

This part notes the vitals of the dataset in question. `N` is the number of observations, `CI` is the set confidence interval. Then follows a standard statistical summary of the data set: Min, Max, Median, Quartiles. The average (mean) of the data set is computed as well as the standard deviation.

In order to be more precise than a single point estimate, we also provide *intervals* for the average and standard deviation with a lower bound and an upper bound. That is, rather than telling you the mean is 3175 as a point, `eministat` tells you that the mean obtained from this particular sample is the interval 3118‥3297.

It is important to stress that the procedure for computing the interval uses a 95% confidence (it is configurable). This does *not* mean that there is a 95% chance the interval contains the true population mean. The true mean is an unknown and no procedure can divine what it is. Rather the 95% refers to the *configuration* of the interval estimation procedure: if you were to repeat this experiment thousands of times, and you would compute a (different) confidence interval for each of those thouand samples, then it would tend to be the case that 95% of those intervals would contain the true population mean.

The system uses a bias-corrected accelerated bootstrap method to compute the bounds on the interval. It also computes the bias from the bootstrap to the sample parameter. In the above example, you see the bias of -14 in the std. deviation which means that the bootstrap procedure usually obtains a smaller standard deviation than the sample's estimate.

## OUTLIERS
	
	Outliers: 0/1 = 1 (μ=3176.19, σ=120.938)
	        Outlier variance:    9.14370e-2 (slight)
	
We use a simple criterion for outliers. Define `IQR = Q3 - Q1` to be the interquartile range. Then any point which is further away from the mean than 1.5IQR is deemed an outlier. We report those as `Lower/Upper` where lower outliers are below the mean and upper outliers are above the mean.

We also compute outlier variance, which is a measurement of how likely it is the case the outliers are affecting the result of the computation. The range is `unaffected‥slight‥moderate‥severe`. In the case of severe variance, it is worth looking at if you can improve the sampling to avoid it.

Common problems are garbage collection, CPU frequency scaling, other work being done on the benchmarking machine as a background task, or other kinds of interference.

## STUDENT's T

	Difference at 95.0% confidence
	        1020.80 ± 88.7257
	        32.1451% ± 2.79398%
	        (Student's t, pooled s = 105.042)

This section only makes sense given the following rules:

* The datasets are normally distributed, or tend to be
* The datasets have equal variance

Assuming we have 2 or more datasets, we are running a 2-sample Student's T-test pairing the first set against each of the other sets. If the two datasets are far apart, then we have significance that the two data sets are really different. But in the case where the two data sets overlap a lot, then it is not a priori given the two datasets differ, as we saw in the example of the ligustrum experiment above.

We report a series of numbers: the difference in the means ± the interval for the Student's T-test.

In general, you have to pass this test for your measurements to have any meaning when comparing two benchmarks. Otherwise, the overlap in runtime is such that there is little reason to believe one way of solving the problem is faster than the other.



