#This code is implements the C-bound optimization problem for the Multiview learning algorithm PB-MVBoost.

#Related Paper:
#Multiview Boosting by Controlling the Diversity and the Accuracy of View-specific Voters
#by Anil Goyal, Emilie Morvant, Pascal Germain and Massih-Reza Amini

#Link to the paper:
#https://arxiv.org/abs/1808.05784

#Author__="Anil Goyal"
#Author of the R implementation - Annette Spooner


#This class solves the C-bound optimization problem for the Multiview learning algorithm PB-MVBoost.
#It learns the weights for over the views for our algorithm.

MV_Cbound_opt = R6::R6Class("MV_Cbound_opt", list(
		initial_guess = NULL,
		risk_vector = NULL,
		disagreement_vector = NULL,

#		:param initial_guess: vector for the initial guess of weights
#		:param risk_vector: Risk vector
#		:param disagreement_vector: Vector for disagreement values
    initialize = function(initial_guess, risk_vector, disagreement_vector) {
        self$initial_guess = initial_guess
        self$risk_vector = risk_vector
        self$disagreement_vector = disagreement_vector
		},


#  	Objective function
    func_obj = function(x, r, d, sign = 1) {
			num = 1 - 2 * (sum(x*r))
			den = 1 - 2 * (sum(x*d))
			return(sign * ((num)**2 / den))
		},


#		Derivative of objective function
    func_deriv = function(x, r, d, sign = 1) {

			num = 1 - 2 * (sum(x*r))
			den = 1 - 2 * (sum(x*d))

			dfdx= sign * ((-1 * 4 * r * num * den + 2 * d * (num)**2) / (den ** 2))
			return (dfdx)
		},


#		Learns weights
#		:param self:
#		:return:

    learn_weights = function() {
		  BBMisc::requirePackages("nloptr")
			res = nloptr::slsqp(x0 = self$initial_guess,
									fn = self$func_obj, 										  
								  gr = self$func_deriv,
									lower = rep(0, length(self$initial_guess)),
									upper = rep(Inf, length(self$initial_guess)),
									heq = {function (x) (sum(x) - 1)},
									heqjac = {function(x) as.matrix(x)},
									nl.info = TRUE,
									r = self$risk_vector, 
									d = self$disagreement_vector,
									sign = -1)	
	
			if (res$convergence < 0) {
					return(self$initial_guess)
			} else {
					return(res$par)
			}
		})
)