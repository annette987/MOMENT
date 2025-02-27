# Test multi-modal adaboost model

test_that("Adaboost with hard vote", {
	cc = make_config(test_path("config_gbm_boruta_4C.xlsx"))
	ada_h = MM_Adaboost$new(cc, decision = 'hard', subset = NULL, validate = FALSE)
	res_ada_h = ada_h$learn()
	expect_results(res_ada_h)
})

test_that("Adaboost with soft vote", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	ada_s = MM_Adaboost$new(cc, decision = 'soft', subset = NULL, validate = FALSE)
	res_ada_s = ada_s$learn()
	expect_results(res_ada_s)
})

test_that("Adaboost with meta learner", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	ada_m = MM_Adaboost$new(cc, decision = 'meta', subset = NULL, validate = FALSE)
	res_ada_m = ada_m$learn()
	expect_results(res_ada_m)
})



