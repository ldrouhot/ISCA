% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISCA_modeling.R
\name{ISCA_modeling}
\alias{ISCA_modeling}
\title{ISCA Modeling}
\usage{
ISCA_modeling(data, model_spec, weights = NULL, n_clusters, draws = 500)
}
\arguments{
\item{data}{The dataset including all relevant variables and the random assignments from the first ISCA_random_assignments()-function.}

\item{model_spec}{A model specification similar to the lm()-function.}

\item{weights}{A vector specifying the variable in which the weights are stored. The default is NONE.}

\item{n_clusters}{Specification of the number of clusters. This value should be equal to the number of clusters specified in the first and second step.}

\item{draws}{Specification of the number of probabilistic draws. The number of draws should be equal to the number of draws specified in the first and second step. If not specified, the default is 500.}
}
\value{
The output is a table containing the regression coefficients, standard error and p-value for each regression term and cluster across all iterations. It also contains the regression coefficient, standard error and p-value for a pooled model, that is a model with all clusters combined.
}
\description{
Function to compute an OLS regression across all clusters and iterations.
}
\examples{
data(sim_data)
ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, 
majority_group=1, minority_group=c(0), fuzzifier = 1.5, n_clusters=4, 
draws=5, cluster_vars= c("female", "age", "education", "income"))
ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
model_spec="religiosity ~ native + female + age + education + discrimination", 
draws = 5, n_clusters = 4);
}
