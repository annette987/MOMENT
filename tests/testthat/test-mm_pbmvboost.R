# Test PB-MVBoost model
# CHECK IF WE NEED TO PASS DECISION - OR IS THIS DETERMINED BY THE MODEL?

test_that("PB-MVBoost - Classification", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	pbmv = PB_MVBoost$new(cc, nrounds = 10, decision_tree_depth = 2, task_type = "classif", decision = "hard", subset = NULL, validate = FALSE)
	res_pbmv = pbmv$learn()
	expect_results(res_pbmv)
})

test_that("PB-MVBoost - Multilabel", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	pbmv = PB_MVBoost$new(cc, nrounds = 10, decision_tree_depth = 2, task_type = "multilabel", decision = "hard", subset = NULL, validate = FALSE)
	res_pbmv = pbmv$learn()
	expect_results(res_pbmv)
})


