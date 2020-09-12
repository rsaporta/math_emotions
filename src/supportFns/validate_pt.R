## confirms that 'pt' is of type x, y
.validate_pt <- function(pt) {
  if (!is.null(ncol(pt))) {
    if (ncol(pt) != 2) 
      stop("pt has ", ncol(pt), ".  Cannot process")
    if (is.null(colnames(pt)))
      colnames(pt) <- c("x", "y")
    pt <- list(x = pt[, "x"], y = pt[, "y"])
  }

  stopifnot(length(pt) == 2)

  if (is.null(names(pt))) {
    names(pt) <- c("x", "y")
  }

  stopifnot(!is.null(names(pt)) && names(pt) == c("x", "y"))
  return(pt)
}
