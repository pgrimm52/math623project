
data_long_2 <- ratings %>%
	mutate(timestamp = as_datetime(timestamp)) %>%
	filter(timestamp > ymd("2003-05-16")) %>% # because of data availability issue
	dplyr::select(-timestamp)

data_wide_2 <- data_long_2 %>%
	spread(key = movieId, value = rating) %>%
	dplyr::select(-userId) %>%
	as.matrix()

holdout_2 <- gen_holdout_indices(data = data_wide_2, frac = 0.2)

fit_acc_2 <- fit_model_simple(
	data = data_wide_2,
	holdout_indices = holdout_2,
	lam = lambda_best_debias_acc,
	debias = TRUE)

confusionMatrix(
	factor(fit_acc_2$predicted),
	factor(data_wide_2[holdout_2]))
