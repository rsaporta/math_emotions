sign_three_pts <- function(p1, p2, p3
) {
  ## https://stackoverflow.com/a/2049593/1492421
  ret <- (p1[["x"]] - p3[["x"]]) * (p2[["y"]] - p3[["y"]]) - (p2[["x"]] - p3[["x"]]) * (p1[["y"]] - p3[["y"]])
  return(ret)
}


#' @param pt the points to check
#' @param x alternate definition for pt; ignored when pt is explicit
#' @param y alternate definition for pt; ignored when pt is explicit
#' @param VERTEX_1 One of the three vertices of the triangle to be checked
#' @param VERTEX_2 One of the three vertices of the triangle to be checked
#' @param VERTEX_3 One of the three vertices of the triangle to be checked
point_in_triangle <- function(
    pt = list(x = x, y = y)
  , VERTEX_1
  , VERTEX_2
  , VERTEX_3
  , x
  , y
) {

  if (!missing(pt)) {
    if (!missing(x) || !missin(y))
      warning("'x' and 'y' are ignored when 'pt' is set explicitly.")
  } 

  pt %<>% .validate_pt()

  d1 <- sign_three_pts(pt, VERTEX_1, VERTEX_2)
  d2 <- sign_three_pts(pt, VERTEX_2, VERTEX_3)
  d3 <- sign_three_pts(pt, VERTEX_3, VERTEX_1)

  has_neg <- (d1 < 0) | (d2 < 0) | (d3 < 0)
  has_pos <- (d1 > 0) | (d2 > 0) | (d3 > 0)

  return (!(has_neg & has_pos))
}

