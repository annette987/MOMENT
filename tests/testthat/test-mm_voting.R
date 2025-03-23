# Test multi-modal voting ensemble model
library(MOMENT)

test_that("Hard voting - classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	vote_ens_h = MM_Voting$new(cc, task_type = "classif", decision = 'hard', subset = NULL, validate = FALSE)
	res_ens_h = vote_ens_h$learn()
	expect_results(res_ens_h)
})

test_that("Soft voting - classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	vote_ens_s = MM_Voting$new(cc, task_type = "classif", decision = 'soft', subset = NULL, validate = FALSE)
	res_ens_s = vote_ens_s$learn()
	expect_results(res_ens_s)
})

test_that("Hard voting - multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	vote_ens_h = MM_Voting$new(cc, task_type = "multilabel", decision = 'hard', subset = NULL, validate = FALSE)
	res_ens_h = vote_ens_h$learn()
	expect_results(res_ens_h)
})

test_that("Soft voting - multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	vote_ens_s = MM_Voting$new(cc, task_type = "multilabel", decision = 'soft', subset = NULL, validate = FALSE)
	res_ens_s = vote_ens_s$learn()
	expect_results(res_ens_s)
})



