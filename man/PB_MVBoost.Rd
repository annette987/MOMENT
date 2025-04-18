% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mm_pbmvboost.R
\docType{package}
\name{PB_MVBoost}
\alias{PB_MVBoost}
\title{R6 Class representing a multi-modal PB-MVBoost classifier}
\description{
Creates a multi-modal PB-MVBoost classifier, based on boosting.
}
\details{
This classifier is based on a two-level multiview learning approach. It learns the distribution over view-specific
classifiers and the distribution over views in one step following a boosing approach.
}
\references{
Multiview Boosting by Controlling the Diversity and the Accuracy of View-specific Voters
by Anil Goyal, Emilie Morvant, Pascal Germain and Massih-Reza Amini
https://arxiv.org/abs/1808.05784

Original Author = "Anil Goyal"
Author of the R implementation - Annette Spooner
}
