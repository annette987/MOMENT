# Test incremental model

test_that("Incrememtal reverse", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	inc = MM_Incremental$new(cc, subset = NULL)
	inc_rev = inc$train_incremental_reverse()
})

test_that("Incrememtal forward", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	inc = MM_Incremental$new(cc, subset = NULL)
	inc_fwd = inc$train_incremental_forward()
})