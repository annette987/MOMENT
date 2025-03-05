# Test PB-MVBoost model

test_that("PB-MVBoost", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	pbmv = PB_MVBoost$new(cc, nrounds = 10, decision_tree_depth = 2, subset = NULL, validate = FALSE)
	res_pbmv = pbmv$learn()
	expect_results(res_pbmv)
})


