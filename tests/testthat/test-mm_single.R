# Test analysis of a single modality


test_that("Classification - single modality", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	active_learners = LRN_GBM

	single_c = MM_Single$new(cc, TASK_CLASSIF, "prob", concat = FALSE, validate = FALSE)
	single_c_res = single_c$learn(active_learners)

	for (i in 1: length(single_c_res)) {
		single_c_res[[i]]$write(paste0("classfn", i))
	}
})


test_that("Survival Analysis - single modality", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	active_learners = LRN_COX
	
	single_s = MM_Single$new(cc, TASK_SURV, "response", concat = FALSE, validate = FALSE)
	single_s_res = single_s$learn(active_learners)

	for (i in 1: length(single_s_res)) {
		single_s_res[[i]]$write(paste0("surv", i))
	}
})
