# Test multi-modal adaboost model

test_that("Adaboost with hard vote - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	ada_h = MM_Adaboost$new(cc, task_type = "classif", decision = 'hard', subset = NULL, validate = FALSE)
	res_ada_h = ada_h$learn()
	expect_results(res_ada_h)
})

test_that("Adaboost with soft vote - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	ada_s = MM_Adaboost$new(cc, task_type = "classif", decision = 'soft', subset = NULL, validate = FALSE)
	res_ada_s = ada_s$learn()
	expect_results(res_ada_s)
})

test_that("Adaboost with meta learner - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	ada_m = MM_Adaboost$new(cc, task_type = "classif", decision = 'meta', subset = NULL, validate = FALSE)
	res_ada_m = ada_m$learn()
	expect_results(res_ada_m)
})


test_that("Adaboost with hard vote - Multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	ada_h = MM_Adaboost$new(cc, task_type = "multilabel", decision = 'hard', subset = NULL, validate = FALSE)
	res_ada_h = ada_h$learn()
	expect_results(res_ada_h)
})

test_that("Adaboost with soft vote - Multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	ada_s = MM_Adaboost$new(cc, task_type = "multilabel", decision = 'soft', subset = NULL, validate = FALSE)
	res_ada_s = ada_s$learn()
	expect_results(res_ada_s)
})

test_that("Adaboost with meta learner - Multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	ada_m = MM_Adaboost$new(cc, task_type = "multilabel", decision = 'meta', subset = NULL, validate = FALSE)
	res_ada_m = ada_m$learn()
	expect_results(res_ada_m)

