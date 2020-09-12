setProjectIfNot(projName = "math_emotions")
setWidth(121, confirm = FALSE)


PLANE = list(x = c(-10, 10), y = c(-10, 10))
VERTEX_1 <- c(x =  -5, y = -3.5)
VERTEX_2 <- c(x =   5, y = -3.5)
VERTEX_3 <- c(x =   0, y =  7)


in_triangle <- function(x, y) {
  point_in_triangle(x = x, y = y, VERTEX_1 = VERTEX_1, VERTEX_2 = VERTEX_2, VERTEX_3 = VERTEX_3)
}

rando <- function(a, N = 100) {
  stopifnot(length(a) == 2)
  stopifnot(!is.na(a))
  seq(from = min(a), to = max(a), length.out = N)
}

N = 250
DT <- CJ(x = rando(PLANE$x, N = N), y = rando(PLANE$y, N = N))
if (nrow(DT) >= 1e5) stop("DT is too large. It has ", formnumb(nrow(DT)), " rows")
DT[, CIRCLE := point_in_circle(x = x, y = y, RADIUS = 22)]
DT[, TRIANGLE := in_triangle(x = x, y = y)]
DT[, color := "OUT"]
DT[( TRIANGLE & !CIRCLE), color := "TRIANGLE"]
DT[(!TRIANGLE &  CIRCLE), color := "CIRCLE"]
DT[( TRIANGLE &  CIRCLE), color := "BOTH"]


## VALIDATE
verboseMsg(verbose, "VALIDATING DT via graph", level.verbose = level.verbose)
lib(ggplot2)
P.confirm_triangle_circle <- 
  ggplot(data = DT) + 
    geom_point(aes(x = x, y = y, color = color)) + 
#|    geom_point(aes(x = x, y = y, color = TRIANGLE)) + 
#|    scale_colour_manual(values=c("TRUE" = "yellow", "FALSE" = "blue"), guide = FALSE) + 
    # geom_abline(intercept = 4, slope = -4/3) + 
    # geom_point(aes(x = 1, y = 2), color = "red") +
    scale_x_continuous(breaks = pretty_breaks()) + 
    scale_y_continuous(breaks = pretty_breaks()) +
    nolegend() + 
    labs(title = "This is to confirm that CIRCLE and TRIANGLE overlap well", x = NULL, y = NULL)

print(P.confirm_triangle_circle)
