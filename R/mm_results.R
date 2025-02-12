#-----------------------------------------------------------------------------------------------------------------------------
# DATA STRUCTURES TO COLLECT RESULTS
#-----------------------------------------------------------------------------------------------------------------------------	

MM_Results = R6::R6Class("MM_Results", 
	public = list(
		roc = NULL,
		perf = NULL,
		stab = NULL,
		feats = NULL,
		predn = NULL,
		plots   = NULL,
		classes = NULL,
		task_desc = NULL,
		model_type = NULL,
		decision = NULL,
			
		initialize = function(classes, tasks, measures, model_type = "VOTE", decision = "response")
		{
			self$classes = classes
			self$task_desc = tasks[[1]]$task.desc
			self$predn = Prediction$new()
			self$roc = ROCMultiClass$new()
			self$perf = Performance$new(measures)
			self$stab = Stability$new(classes)			
			self$feats = Features$new(tasks)
			self$model_type = model_type
			self$decision = decision
		},
		
		save_responses = function(responses, rpt, fold)
		{
			stopifnot(self$model_type != "SURV")
			stopifnot(inherits(responses, "data.frame") && (all(c("response", "truth") %in% colnames(responses))) && self$model_type != "SURV")
			responses$rpt = rpt
			responses$fold = fold
			private$responses = rbind(private$responses, responses)	
			pred_resp = self$make_mlr_prediction(responses, self$task_desc, self$decision, "PredictionClassif")
			self$perf$calculate(pred_resp, self$tasks[[1]])
			self$roc$calc(responses$truth, responses$response, as.list(self$classes))
		},
		
		save_predictions = function(pred, task = NULL, model = NULL)
		{
			if (self$model_type == "SURV") {
				predn_class = "PredictionSurv"
				check_cols  = c("response", "truth.time", "truth.event")
			} else {
				predn_class = "PredictionClassif"
				check_cols  = c("response", "truth")
			}
			
			stopifnot(all(check_cols %in% colnames(pred$data)))
			private$responses = rbind(private$responses, pred$data)			
			self$perf$calculate(pred, task, model)
			if (self$model_type != "SURV") {
				self$roc$calc(pred$data$truth, pred$data$response, as.list(self$classes))
			}
		},
		
		save_features = function(model, task, method, fold_num)
		{
			self$feats$save(model, task, self$classes, method, fold_num)
		},
		
		# Create an mlr prediction object using the specified results
		make_mlr_prediction = function(results, task_desc, decision = "response", predn_class = "PredictionClassif") {
			p = BBmisc::makeS3Obj(c(predn_class, "Prediction"),
				predict.type = decision,
				data = results,
				hv_threshold = NA_real_,
				task.desc = task_desc,
				time = NULL,
				error = NULL,
				dump = NULL
			)
			return(p)
		},
		
		complete = function(title)
		{
			if (self$model_type != "SURV") {
				self$roc$calc_mc_roc(as.factor(private$responses$truth), as.factor(private$responses$response))
			}
			self$stab$save_all(self$model_type, self$feats$featsel)
#			self$predn$save(private$responses)
		},
		
		write = function(result_file_prefix, suffix = NULL)
		{
			if (self$model_type != "SURV") {
				self$roc$write(result_file_prefix, suffix)
				self$roc$plot("TITLE", result_file_prefix)
			}
			self$perf$write(result_file_prefix, suffix)
			self$feats$complete()
			self$feats$write(result_file_prefix, suffix)
			self$stab$write(result_file_prefix, suffix)
#			self$predn$write(result_file_prefix, suffix)
		}
	),
	
	private = list(	
		responses = NULL
	)

)