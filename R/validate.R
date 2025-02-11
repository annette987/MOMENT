validate = function(limo)
{
		limo$vtasks = create_validation_tasks(limo$tasks, data_dir, config, MMOmicsConst$TASK_CLASSIF, ovr_class, big, subset, filter_zeroes, filter_missings, filter_corr, filter_var)
		vdat = getTaskData(limo$vtasks[[1]])
		print(paste0("Number of rows in validation tasks = ", nrow(vdat)))
		ri_v = makeFixedHoldoutInstance(c(1:nrow(dat)), c((nrow(dat) + 1):nrow(vdat)), nrow(vdat))
		
		vroc = ROCMultiClass$new()
		vperf = Performance$new(classfn.measures)
		vfeats = MM_Features$new(vtasks)

		vresponses = train_base_mods(vtasks, learners, ri_v, 1, decision, classes, vfeats, vperf)
		vresponses = get_final_decision(vresponses, classes, decision)
		vroc$calc(vresponses$truth, vresponses$response, as.list(classes))
		vroc$calc_mc_roc(as.factor(vresponses$truth), as.factor(vresponses$response))

		vpred_resp = make_mlr_prediction(vresponses, vtasks[[1]]$task.desc)
		vperf$calculate(vpred_resp)
		
		vresult_file = paste0(result_file, "_validate")
		vroc$write(vresult_file)
		vperf$write(vresult_file)
		
		vfeats$write(vresult_file)
}