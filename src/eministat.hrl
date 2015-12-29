%% Datasets in eministat are these beasts
-record(dataset, {
	name :: string(),
	points :: [float()],
	sy :: float(),
	syy :: float(),
	n :: integer()
}).
