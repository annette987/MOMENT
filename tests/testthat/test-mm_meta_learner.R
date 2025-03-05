# Test meta learner model

test_that("Meta-learner", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	meta = MM_Meta_Learner$new(cc, subset = NULL, validate = FALSE)
	res_meta = meta$learn()
	expect_results(res_meta)
})


