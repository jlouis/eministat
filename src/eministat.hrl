%% Globally recognized data structures in eministat.
%%
%% These could be local to the eministat_ds module, but for the convenience-factor, we keep
%% These known inside the library globally.

%% Datasets in eministat are these beasts
-record(dataset, {
	name :: string(),
	points :: [float()],
	sy :: float(),
	syy :: float(),
	n :: integer()
}).
