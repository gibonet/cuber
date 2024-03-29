% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_apply_.R
\name{apply_df_}
\alias{apply_df_}
\title{Apply a list of function calls to a data frame and return the results in a data frame}
\usage{
apply_df_(.data, fun_list = list(n = ~nrow(.data)), ...)
}
\arguments{
\item{.data}{a data frame.}

\item{fun_list}{a named list of formulas with the desired function calls to
be evalutated on the data frame. The default value (\code{list(n = ~nrow(.data))})
returns the number of rows (observations) and stores it in a column named \code{n}.}

\item{...}{additional optional arguments passed to \code{\link{apply_to_df_}} 
 (\code{estimator_name} and \code{value}).
 
 @examples 
data(invented_wages)
str(invented_wages)

# First create a list of function calls (with formulas)
fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))

# And then apply them to the data
apply_df_(invented_wages, fun_list = fun)}
}
\description{
Apply a list of function calls to a data frame and return the results in a data frame
}
