% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_apply_.R
\name{apply_to_df_}
\alias{apply_to_df_}
\title{Put the results of 'apply_' in a data frame}
\usage{
apply_to_df_(.list, estimator_name = "estimator", value = "value")
}
\arguments{
\item{.list}{the output of \code{\link{apply_}}.}

\item{estimator_name}{character string with the name to be given to the 
column with the type of the estimated statistics (default: "estimator").}

\item{value}{character string with the name of the column that will contain
the values of the estimated statistics (default: "value").}
}
\description{
Put the results of 'apply_' in a data frame
}
\examples{
str(invented_wages)

# First create a list of function calls (with formulas)
fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))

# And then apply them to the data
tmp <- apply_(invented_wages, fun_list = fun)
tmp
str(tmp)

# The results of 'apply_' are stored in a list. 'apply_to_df_' transforms
# them in a data frame:
apply_to_df_(tmp)

}
