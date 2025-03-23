# Test meta learner model
# CHECK IF WE NEED TO PASS DECISION - OR IS THIS DETERMINED BY THE MODEL?

test_that("Meta-learner - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	meta = MM_Meta_Learner$new(cc, task_type = "classif", decision = 'hard', subset = NULL, validate = FALSE)
	res_meta = meta$learn()
	expect_results(res_meta)
})

test_that("Meta-learner - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	meta = MM_Meta_Learner$new(cc, task_type = "classif", decision = 'soft', subset = NULL, validate = FALSE)
	res_meta = meta$learn()
	expect_results(res_meta)
})

test_that("Meta-learner - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	meta = MM_Meta_Learner$new(cc, task_type = "multilabel", decision = 'hard', subset = NULL, validate = FALSE)
	res_meta = meta$learn()
	expect_results(res_meta)
})

test_that("Meta-learner - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	meta = MM_Meta_Learner$new(cc, task_type = "multilabel", decision = 'soft', subset = NULL, validate = FALSE)
	res_meta = meta$learn()
	expect_results(res_meta)
})


