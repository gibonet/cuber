
# Partendo da un numero intero, k, genera una lista i cui elementi saranno 
# delle matrici con degli indici in riga e ogni colonna che rappresenta una
# combinazione. Le combinazioni k - (k-i) vengono generate per `i` che va da 
# 1 a k (combn(3, 1), combn(3, 2), combn(3, 3))
combn_l <- function(k) lapply(1:k, function(x) combn(k, m = k - x + 1))
# combn_l(3)


# Funzione che genera tutte le combinazioni degli elementi di un vettore character

#' Generate all combinations of the elements of a character vector
#' 
#' @param x a character vector
#' 
#' @return a nested list. A list whose elements are lists containing the 
#' character vectors with the combinations of their elements.
#' 
#' @examples 
#' combn_char(c("gender", "sector"))
#' combn_char(c("gender", "sector", "education"))
#' @export
combn_char <- function(x){
  l <- length(x)
  comb_vars <- combn_l(l)
  
  list_comb <- vector(mode = "list", length = l)
  for(i in seq_along(comb_vars)){
    list_comb_i <- vector(mode = "list", length = ncol(comb_vars[[i]]))
    
    for(j in seq_along(list_comb_i)){
      k <- comb_vars[[i]][ , j]
      list_comb_i[[j]] <- x[k]
    }
    list_comb[[i]] <- list_comb_i
  }
  return(list_comb)
}

# vars <- c("gender", "sector", "education")
# list_vars <- combn_char(vars)

