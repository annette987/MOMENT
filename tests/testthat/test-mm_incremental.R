# Test incremental model

test_that("Incrememtal reverse", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	inc = MM_Incremental$new(cc, subset = NULL)
	inc_rev = inc$train_incremental_reverse()
})

test_that("Incrememtal forward", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	inc = MM_Incremental$new(cc, subset = NULL)
	inc_fwd = inc$train_incremental_forward()
})