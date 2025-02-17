# Test Mix of Experts model

test_that("Mix of experts with hard vote", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	moe_v = MM_MoE$new(cc, model_type = "VOTE", subset = NULL, validate = FALSE)
	res_moe_v = moe_v$learn(cc)
	expect_results(res_moe_v)
})

test_that("Mix of experts with adaboost, {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	moe_a = MM_MoE$new(cc, model_type = "ADA", subset = NULL, validate = FALSE)
	res_moe_a = moe_a$learn(cc)
	expect_results(res_moe_a)
})


