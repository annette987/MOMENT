# Test data explorer

Sys.unsetenv("R_TESTS")
options(warn = 2)

test_that("Data explorer", {
	cc = make_config(test_path("testdata", "sample_config.xlsx"))
	exp = MM_Explorer$new(cc)
	res_exp = exp$learn(cc, "explore")
})
