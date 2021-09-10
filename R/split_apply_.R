

#' Apply a list of function calls to a data frame and return the results in a list
#' 
#' @param .data a data frame.
#' @param fun_list a named list of formulas with the desired function calls to
#' be evalutated on the data frame. The default value (\code{list(n = ~nrow(.data))})
#' returns the number of rows (observations) and stores it in a column named \code{n}.
#' 
#' @return a named list with the results of the function calls applied to the data frame.
#' 
#' @examples 
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And then apply them to the data
#' apply_(invented_wages, fun_list = fun)
#' @export
apply_ <- function(.data, fun_list = list(n = ~nrow(.data))){
  res_list <- lapply(fun_list, function(f) eval(f[[2]], envir = .data))
  res_list
}


#' Put the results of 'apply_' in a data frame
#' 
#' @param .list the output of \code{\link{apply_}}.
#' @param estimator_name character string with the name to be given to the 
#'  column with the type of the estimated statistics (default: "estimator").
#' @param value character string with the name of the column that will contain
#'  the values of the estimated statistics (default: "value").
#' 
#' @examples 
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And then apply them to the data
#' tmp <- apply_(invented_wages, fun_list = fun)
#' tmp
#' str(tmp)
#' 
#' # The results of 'apply_' are stored in a list. 'apply_to_df_' transforms
#' # them in a data frame:
#' apply_to_df_(tmp)
#' 
#' @export
apply_to_df_ <- function(.list, estimator_name = "estimator", value = "value"){
  .names <- names(.list)
  df <- data.frame(V1 = .names, V2 = unlist(.list))
  colnames(df) <- c(estimator_name, value)
  row.names(df) <- as.character(1:nrow(df))
  return(df)
}




#' Apply a list of function calls to a data frame and return the results in a data frame
#' 
#' @inheritParams apply_
#' @param ... additional optional arguments passed to \code{\link{apply_to_df_}} 
#'  (\code{estimator_name} and \code{value}).
#'  
#'  @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And then apply them to the data
#' apply_df_(invented_wages, fun_list = fun)
#' 
#' @export
apply_df_ <- function(.data, fun_list = list(n = ~nrow(.data)), ...){
  res_list <- apply_(.data, fun_list)
  res_df <- apply_to_df_(res_list, ...)
  res_df
}
# apply_df_(invented_wages, fun_list = fun)




# split_apply_: split_ + apply_

#' Apply a list of function calls to groups of a data frame
#' 
#' @param .data a data frame.
#' @param .vars character vector with the names of the columns for which the 
#' data frame will be divided.
#' @param sep a character string to separate the terms.
#' @param fun_list a named list of formulas with the desired function calls to
#' be evalutated on the data frame. The default value (\code{list(n = ~nrow(.data))})
#' returns the number of rows (observations) and stores it in a column named \code{n}.
#' 
#' @return a nested list
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And then apply them to subsets of data defined by the combinations of variables
#' split_apply_(invented_wages, .vars = "gender", fun_list = fun)
#' split_apply_(invented_wages, .vars = c("gender", "sector"), fun_list = fun)
#' split_apply_(invented_wages, .vars = c("gender", "sector", "education"), fun_list = fun)
#' @export
split_apply_ <- function(.data, .vars, sep = ".", fun_list = list(n = ~nrow(.data))){
  .data_split <- split_(.data, .vars, sep = sep)
  res <- lapply(.data_split, function(x) apply_(x, fun_list = fun_list))
  res
}
# split_apply_(invented_wages, .vars = vars, fun_list = fun)
# tmp <- split_apply_(invented_wages, .vars = vars, fun_list = fun)
# str(tmp)
# unlist(tmp, recursive = FALSE)


# Adesso scrivo una funzione che mette i risultati di split_apply_ in un data frame
# (l'output di split_apply_ è una nested list)

#' Put the results of 'split_apply_' in a data frame
#' 
#' @param .nested_list a nested list, the output of \code{split_apply_}.
#' @param sep a character string to separate the terms.
#' @param estimator_name character string with the name to be given to the
#' column with the type of the estimated statistics (default: "estimator").
#' @param value character string with the name of the column that will contain 
#' the values of the estimated statistics (default: "value").
#' 
#' @return a data frame 
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And then apply them to subsets of data defined by the combinations of variables
#' tmp <- split_apply_(invented_wages, .vars = c("gender", "sector"), fun_list = fun)
#' str(tmp)     # a list
#' 
#' res2df(tmp)  # a data frame
#' 
#' @export
res2df <- function(.nested_list, sep = ".", estimator_name = "estimator", value = "value"){
  unnested <- unlist(.nested_list, recursive = FALSE)
  .names <- names(unnested)
  
  if(sep != ".") .names <- gsub(pattern = ".", replacement = sep, x = .names, fixed = TRUE)
  
  .names_split <- strsplit(.names, split = sep, fixed = TRUE)
  mat <- Reduce(rbind, .names_split)
  row.names(mat) <- NULL
  
  df <- as.data.frame(mat)
  colnames(df)[ncol(df)] <- estimator_name
  df[[value]] <- unlist(unnested)
  
  return(df)
}
# Rimangono i nomi delle colonne da impostare (...)
# res2df(tmp)


# split_apply_ + res2df (così recupero anche i nomi delle variabili)

#' Apply a list of function calls to groups of a data frame
#' 
#' @inheritParams split_apply_
#' @param ... additional optional arguments passed to \code{\link{res2df}} 
#' (\code{estimator_name} and \code{value}).
#' 
#' @return a data frame with the function calls estimated for the groups 
#' defined by the combinations of variables.
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And then apply them to subsets of data defined by the combinations of variables
#' split_apply_df_(invented_wages, .vars = "gender", fun_list = fun)
#' split_apply_df_(invented_wages, .vars = c("gender", "sector"), fun_list = fun)
#' 
#' @export
split_apply_df_ <- function(.data, .vars, sep = ".", fun_list = list(n = ~nrow(.data)), ...){
  res <- split_apply_(.data, .vars, sep = sep, fun_list)
  df <- res2df(res, sep, ...)
  colnames(df)[1:length(.vars)] <- .vars
  df
}
# split_apply_df_(invented_wages, .vars = vars, fun_list = fun)
# split_apply_df_(invented_wages, .vars = vars, fun_list = fun, estimator_name = "statistica", value = "stima")



# E adesso posso iniziare con le creazioni dei cubi (qualche prova e variante)
# split_apply_df_ + combn_char

#' Create a data cube and store it in a list
#' 
#' @inheritParams split_apply_df_
#' 
#' @return a list whose elements are data frame containing the estimates for 
#' each combination of variables.
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas) and
#' # a vector with names of the desired variables
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' vars <- c("gender", "sector")
#' 
#' # And then create the data cube, with the estimates of all the combinations
#' # of variables
#' cube_(invented_wages, .vars = vars, fun_list = fun)
#' cube_(invented_wages, .vars = vars, fun_list = fun, 
#'       estimator_name = "statistica", value = "stima")
#' 
#' @export
cube_ <- function(.data, .vars, sep = ".", fun_list = list(n = ~nrow(.data)), ...){
  list_vars <- combn_char(.vars)
  unlist_vars <- unlist(list_vars, recursive = FALSE)
  
  res <- lapply(unlist_vars, function(x) split_apply_df_(.data, .vars = x, sep = sep, fun_list, ...))
  res_ <- apply_df_(.data, fun_list, ...)
  
  res[[length(res) + 1L]] <- res_

  return(res)
}
# cube_(invented_wages, .vars = vars, fun_list = fun)
# cube_(invented_wages, .vars = vars, fun_list = fun, estimator_name = "statistica", value = "stima")



# cube_ + risultato tutto in un data frame "impilato" (i data frame della lista 
# prodotta da cube_ impilati uno sotto l'altro)

#' Create a data cube and store it in a data frame
#' 
#' @inheritParams split_apply_df_
#' @param .total character string with the value to give to the "total" combinations
#' (default: "Totale")
#' 
#' @return data frame containing the estimates for each combination of variables.
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # First create a list of function calls (with formulas) and
#' # a vector with names of the desired variables
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' vars <- c("gender", "sector")
#' 
#' # And then create the data cube, with the estimates of all the combinations
#' # of variables
#' cube_df_(invented_wages, .vars = vars, fun_list = fun)
#' cube_df_(invented_wages, .vars = vars, fun_list = fun, 
#'          estimator_name = "statistica", value = "stima")
#' 
#' @export
cube_df_ <- function(.data, .vars, sep = ".", fun_list = list(n = ~nrow(.data)), .total = "Totale", ...){
  res <- cube_(.data, .vars, sep = sep, fun_list, ...)
  
  list_colnames <- lapply(res, colnames)
  colnames_all <- Reduce(union, list_colnames)

  for(i in seq_along(res)){
    missing_columns <- setdiff(colnames_all, list_colnames[[i]])
    res[[i]][ , missing_columns] <- .total
    res[[i]] <- res[[i]][ , colnames_all]
  }
  
  res_df <- Reduce(rbind, res)
  res_df
}
# cube_df_(invented_wages, .vars = vars, fun_list = fun)
# tmp <- cube_df_(invented_wages, .vars = vars, fun_list = fun, .total = "TOTALE")
# tmp



# Una variante di cube_ che, al posto di fare le elaborazioni per tutte le 
# combinazioni di variabili, fa i calcoli per le combinazioni di variabili 
# che vengono passate in una lista di vettori character.

#' Create a (partial) data cube and store it in a list
#' 
#' @param .data a data frame.
#' @param .list_vars a list of character vectors with the combinations of 
#' variables for which functions will be evaluated.
#' @param sep a character string to separate the terms.
#' @param fun_list a named list of formulas with the desired function calls
#'  to be evalutated on the data frame. The default value (\code{list(n = ~nrow(.data))})
#' returns the number of rows (observations) and stores it in a column named \code{n}.
#' @param .all logical, indicating if functions' evaluations on the complete 
#' dataset have to be done.
#' @param ... additional optional arguments passed to \code{\link{res2df}} 
#' (\code{estimator_name} and \code{value}).
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # Create a list of character vectors with the combinations of variables
#' # for which we want to evalutate function calls
#' list_all_vars <- unlist(combn_char(c("gender", "sector")), recursive = FALSE)
#' list_all_vars           # All the combinations of the two variables        
#' list_all_vars[c(1, 3)]  # Keep only the first and the third
#' 
#' # Then create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And finally create the data cube, but only on the given combinations of
#' # variables
#' cube_vars_(invented_wages, .list_vars = list_all_vars[c(2, 3)], fun_list = fun)
#' 
#' # Setting .all = TRUE adds the evalutations on the complete dataset
#' cube_vars_(invented_wages, .list_vars = list_all_vars[c(2, 3)], fun_list = fun, .all = TRUE)
#' 
#' @export
cube_vars_ <- function(.data, .list_vars, sep = ".", fun_list = list(n = ~nrow(.data)), .all = FALSE, ...){
  res <- lapply(.list_vars, function(x) split_apply_df_(.data, .vars = x, sep = sep, fun_list, ...))
  if(.all){
    res_ <- apply_df_(.data, fun_list, ...)
    res[[length(res) + 1L]] <- res_
  }
  return(res)
}

last_columns <- function(x){
  k <- ncol(x)
  res_index <- c(k-1, k)
  res <- colnames(x)[res_index]
  res
}


#' Put the results of a data cube in a data frame
#' 
#' @param .cube a data cube. This can be the output of \code{\link{cube_}} or
#'  \code{\link{cube_vars_}}
#' @param .total character string with the value to give to the "total" 
#' combinations (default: "Totale")
#' 
#' @export
cube_to_df_ <- function(.cube, .total = "Totale"){
  list_colnames <- lapply(.cube, colnames)
  colnames_all <- Reduce(union, list_colnames)
  
  last_cols <- last_columns(.cube[[1]])
  first_cols <- setdiff(colnames_all, last_cols)
  colnames_all <- c(first_cols, last_cols)
  
  for(i in seq_along(.cube)){
    missing_columns <- setdiff(colnames_all, list_colnames[[i]])
    .cube[[i]][ , missing_columns] <- .total
    .cube[[i]] <- .cube[[i]][ , colnames_all]
  }
  
  res_df <- Reduce(rbind, .cube)
  res_df
}

#' Create a (partial) data cube and store it in a data frame
#' 
#' @inheritParams cube_vars_
#' @param .total character string with the value to give to the "total" 
#' combinations (default: "Totale").
#' 
#' @examples 
#' data(invented_wages)
#' str(invented_wages)
#' 
#' # Create a list of character vectors with the combinations of variables
#' # for which we want to evalutate function calls
#' list_all_vars <- unlist(combn_char(c("gender", "sector")), recursive = FALSE)
#' list_all_vars           # All the combinations of the two variables        
#' list_all_vars[c(1, 3)]  # Keep only the first and the third
#' 
#' # Then create a list of function calls (with formulas)
#' fun <- list(m1 = ~weighted.mean(wage, sample_weights), s = ~sum(sample_weights))
#' 
#' # And finally create the data cube, but only on the given combinations of
#' # variables
#' cube_vars_df_(invented_wages, .list_vars = list_all_vars[c(2, 3)], fun_list = fun)
#' 
#' # Setting .all = TRUE adds the evalutations on the complete dataset
#' cube_vars_df_(invented_wages, .list_vars = list_all_vars[c(2, 3)], fun_list = fun, .all = TRUE)
#' 
#' @export
cube_vars_df_ <- function(.data, .list_vars, sep = ".", fun_list = list(n = ~nrow(.data)), 
                          .all = FALSE, .total = "Totale", ...){
  res <- cube_vars_(.data, .list_vars, sep = sep, fun_list, .all, ...)
  res_df <- cube_to_df_(res, .total)
  return(res_df)
}


