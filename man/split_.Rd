% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_.R
\name{split_}
\alias{split_}
\title{Split a data frame in groups based on variables}
\usage{
split_(.data, .vars, sep = ".", ...)
}
\arguments{
\item{.data}{a data frame.}

\item{.vars}{character vector with the names of the columns for which the 
data frame will be divided.}

\item{sep}{a character string to separate the terms.}

\item{...}{additional potential arguments passed to \code{\link{split}}}
}
\value{
a list whose elements are the data frames with the groups defined by
 the combinations of the chosen variables.
}
\description{
Split a data frame in groups based on variables
}
\examples{
data(invented_wages)
split_(invented_wages, .vars = c("gender"))
split_(invented_wages, .vars = c("gender", "sector"))
}
