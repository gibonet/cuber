
# Una versione di split che prenda come argomento un vettore
# character con i nomi delle variabili su cui splittare

#' Split a data frame in groups based on variables
#' 
#' @param .data a data frame.
#' @param .vars character vector with the names of the columns for which the 
#' data frame will be divided.
#' @param sep a character string to separate the terms.
#' @param ... additional potential arguments passed to \code{\link{split}}
#' 
#' @return a list whose elements are the data frames with the groups defined by
#'  the combinations of the chosen variables.
#' 
#' @examples 
#' data(invented_wages)
#' split_(invented_wages, .vars = c("gender"))
#' split_(invented_wages, .vars = c("gender", "sector"))
#' @export
split_ <- function(.data, .vars, sep = ".", ...){
  f2 <- lapply(.vars, function(z) .data[[z]])
  split(.data, f2, sep = sep, ...)
}

# split_(invented_wages, .vars = c("gender", "sector"))
# split_(invented_wages, .vars = c("gender"))
