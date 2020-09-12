rando <- function(a, N = 100) {
  stopifnot(length(a) == 2)
  stopifnot(!is.na(a))
  seq(from = min(a), to = max(a), length.out = N)
}
