#' @param pt the points to check
#' @param x alternate definition for pt; ignored when pt is explicit
#' @param y alternate definition for pt; ignored when pt is explicit
#' @param RADIUS radius of the circle
#' @param CENTER the center of the circle.  remains a '##TODO'
point_in_circle <- function(
    pt = list(x = x, y = y)
  , RADIUS
  , CENTER = c(x = 0, y = 0)
  , x
  , y
) {

  if (!identical(CENTER, c(x = 0, y = 0)))
    stop("Not yet implemented for CENTER != origin")

  if (!missing(pt)) {
    if (!missing(x) || !missin(y))
      warning("'x' and 'y' are ignored when 'pt' is set explicitly.")
  } 

  pt %<>% .validate_pt()

  stopifnot(length(RADIUS) == 1L, is.numeric(RADIUS))

  z <- (pt[["x"]]^2) + (pt[["y"]]^2)

  return (z <= RADIUS)
}
