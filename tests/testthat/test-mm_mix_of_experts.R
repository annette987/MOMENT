# Test Mix of Experts model

test_that("Mix of experts with soft vote - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	moe_v = MM_MoE$new(cc, model_type = "vote", task_type = "classif", decision = "soft", subset = NULL, validate = FALSE)
	res_moe_v = moe_v$learn(cc)
	expect_results(res_moe_v)
})

test_that("Mix of experts with adaboost - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	moe_a = MM_MoE$new(cc, model_type = "ada", task_type = "classif", decision = "soft", subset = NULL, validate = FALSE)
	res_moe_a = moe_a$learn(cc)
	expect_results(res_moe_a)
})

test_that("Mix of experts with soft vote - Multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	moe_v = MM_MoE$new(cc, model_type = "vote", task_type = "multilabel", decision = "soft", subset = NULL, validate = FALSE)
	res_moe_v = moe_v$learn(cc)
	expect_results(res_moe_v)
})

test_that("Mix of experts with adaboost - Multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	moe_a = MM_MoE$new(cc, model_type = "ada", task_type = "multilabel", decision = "soft", subset = NULL, validate = FALSE)
	res_moe_a = moe_a$learn(cc)
	expect_results(res_moe_a)
})