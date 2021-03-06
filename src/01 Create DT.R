setProjectIfNot(projName = "math_emotions")
setWidth(121, confirm = FALSE)


A <- 9L
PLANE = list(x = c(-A, A), y = c(-A, A))
RADIUS <- 22L
VERTEX_1 <- c(x =  -5, y = -3.5)
VERTEX_2 <- c(x =   5, y = -3.5)
VERTEX_3 <- c(x =   0, y =  7)


in_triangle <- function(x, y) {
  point_in_triangle(x = x, y = y, VERTEX_1 = VERTEX_1, VERTEX_2 = VERTEX_2, VERTEX_3 = VERTEX_3)
}
in_fuzzy_triangle <- function(x, y, s = 10) {
  s %<>% divide(100)
  point_in_triangle(x = x, y = y, VERTEX_1 = VERTEX_1 * c(1 + s, 1 - s), VERTEX_2 = VERTEX_2 * c(1 + s, 1 + s), VERTEX_3 = (VERTEX_3 * c(1 - s, 1 - s)) %>% ifelse(. == 0,  4 * s, .)) |
  point_in_triangle(x = x, y = y, VERTEX_1 = VERTEX_1 * c(1 + s, 1 - s), VERTEX_2 = VERTEX_2 * c(1 + s, 1 + s), VERTEX_3 = (VERTEX_3 * c(1 - s, 1 + s)) %>% ifelse(. == 0,  2 * s, .)) |
  point_in_triangle(x = x, y = y, VERTEX_1 = VERTEX_1 * c(1 + s, 1 - s), VERTEX_2 = VERTEX_2 * c(1 + s, 1 + 2*s), VERTEX_3 = (VERTEX_3 * c(1 - s, 1 + s)) %>% ifelse(. == 0,  2 * s, .)) |
  point_in_triangle(x = x, y = y, VERTEX_1 = VERTEX_1 * c(1 - s, 1 + s), VERTEX_2 = VERTEX_2 * c(1 - s, 1 - s), VERTEX_3 = (VERTEX_3 * c(1 + s, 1 + s)) %>% ifelse(. == 0, -4 * s, .))
}

is_on_circle <- function(x, y, RADIUS, tolerance = 0.05) {
  z <- (x^2) + (y^2)
  equals(z, RADIUS, tolerance = tolerance, na.check = FALSE)
}

N = 300
DT <- CJ(x = rando(PLANE$x, N = N), y = rando(PLANE$y, N = N))
if (nrow(DT) >= 1e5) stop("DT is too large. It has ", formnumb(nrow(DT)), " rows")

DT[, W := (((x/ 2.5) ^ 2) + ((y/ 2.5) ^ 2))^2]
DT[, CIRCLE := point_in_circle(x = x, y = y, RADIUS = RADIUS)]
DT[, TRIANGLE := in_triangle(x = x, y = y)]
DT[, FUZZY_TRIANGLE := in_fuzzy_triangle(x = x, y = y)]

DT[, pt_is_on_circle := is_on_circle(x = x, y = y, RADIUS = RADIUS, tolerance = 50 / N)]


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
    geom_point(data = DT[(pt_is_on_circle)], aes(x = x, y = y), color = "black", size = 0.5) + 
    geom_point(data = DT[(FUZZY_TRIANGLE)], aes(x = x, y = y), color = "red", size = 0.4, alpha = 0.1) + 
    scale_x_continuous(breaks = pretty_breaks()) + 
    scale_y_continuous(breaks = pretty_breaks()) +
    nolegend() + 
    labs(title = "This is to confirm that CIRCLE and TRIANGLE overlap well", x = NULL, y = NULL)

print(P.confirm_triangle_circle)
