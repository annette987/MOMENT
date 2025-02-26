# Test data explorer

test_that("Data explorer", {
	cc = make_config(test_path("config_gbm_boruta_4C.xlsx"))
	exp = MM_Explorer$new(cc)
	res_exp = exp$learn(cc, "explore")
})




