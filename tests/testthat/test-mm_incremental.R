# Test incremental model

test_that("Incrememtal reverse - classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	inc = MM_Incremental$new(cc, metric = "F1", task_type = "classif", decision = "prob", subset = NULL)
	inc_rev = inc$train_incremental_reverse()
})

test_that("Incrememtal forward - classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	inc = MM_Incremental$new(cc, metric = "F1", task_type = "classif", decision = "prob", subset = NULL)
	inc_fwd = inc$train_incremental_forward()
})


test_that("Incrememtal reverse - multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	inc = MM_Incremental$new(cc, metric = "F1", task_type = "multilabel", decision = "prob", subset = NULL)
	inc_rev = inc$train_incremental_reverse()
})

test_that("Incrememtal forward - multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	inc = MM_Incremental$new(cc, metric = "F1", task_type = "multilabel", decision = "prob", subset = NULL)
	inc_fwd = inc$train_incremental_forward()
})