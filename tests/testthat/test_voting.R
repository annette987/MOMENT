# Test multi-modal voting ensemble model
library(MOMENT)

test_that("Hard voting", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	vote_ens_h = MM_Voting$new(cc, decision = 'hard', subset = NULL, validate = FALSE)
	res_ens_h = vote_ens_h$learn()
	expect_results(res_ens_h)
})

test_that("Soft voting", {
	cc = config(test_path("config_gbm_boruta_4C.xlsx"))
	vote_ens_s = MM_Voting$new(cc, decision = 'soft', subset = NULL, validate = FALSE)
	res_ens_s = vote_ens_s$learn()
	expect_results(res_ens_s)
})



