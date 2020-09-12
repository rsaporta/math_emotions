SECONDS_GIF <- 10
SECONDS_PAUSE <- ceiling(max(SECONDS_GIF / 4, 2))

library(ggplot2)
library(scales)
library(gganimate)

if (FALSE) {
  Plots.circle_then_trianlge <- 
  lapply(ITERS, function(i) {
      subt_info <- 
        if (i >= TRIANGLE_FILL_BEGINS_AT) paste("TRIANGLE FILL: ", i - TRIANGLE_FILL_BEGINS_AT + 1L)
        else if (i >= CIRCLE_LINE_BEGINS_AT)   paste("CIRCLE LINE: ",   i - CIRCLE_LINE_BEGINS_AT   + 1L)
        else ""

      ggplot(data = DT[plot_numb <= i][!(TRIANGLE)]) + 
        geom_point(aes(x = x, y = y), color = "darkcyan") + 
        scale_x_continuous(breaks = pretty_breaks(), lim = PLANE$x) + 
        scale_y_continuous(breaks = pretty_breaks(), lim = PLANE$y) +
        nolegend() + 
        labs(title = "We cannot observe emotions. Only the facts aruond them", x = NULL, y = NULL, subtitle = sprintf("i = %i | %s", i, subt_info))
  })

  printToPDF(Plots.circle_then_trianlge, ncol = 2L, height.per = 6, width.per = 6)
}

DT.plotting <- lapply(ITERS[c(1,4,10)], function(iter) {
  DT[plot_numb <= iter][!(TRIANGLE)][, the_iter := iter]
}) %>%
rbindlist()

for (i in 1:3) {
  DT.plotting %<>% rbind(
    DT[!is.na(plot_numb)][!(TRIANGLE)][, plot_numb := max(plot_numb) + i][, the_iter := maxn(DT.plotting$the_iter) + 1L]
  )
}

## NOT NEEDED
#| DT.plotting[, the_iter := factor(the_iter, levels = sunique(the_iter), labels = LETTERS[seq.int(uniqueN(the_iter))])]

P.animated <- 
# ggplot(data = DT[!(TRIANGLE)][!is.na(plot_numb)]) + 
ggplot(data = DT.plotting) + 
# ggplot(data = DT[!is.na(plot_numb) & !TRIANGLE][plot_numb < N.PLOTS]) + 
  geom_point(aes(x = x, y = y), color = "darkcyan") + 
  scale_x_continuous(breaks = pretty_breaks(), lim = PLANE$x) + 
  scale_y_continuous(breaks = pretty_breaks(), lim = PLANE$y) +
  nolegend() + 
  transition_manual(
    # transition_length = 0.3,
    # filter_length = 1.2,
    # wrap = FALSE,
    # keep = TRUE,
    plot_numb,
    cumulative = TRUE
) +   enter_fade()
  # ) +
  # ggtitle(
  #   'Filter: {closest_filter}',
  #   subtitle = '{closest_expression}'
  # ) +
  # exit_recolour(colour = 'grey') +
  # exit_shrink(size = 0.5)

  # transition_states(the_iter, 0.2, 1.2, FALSE) 
  # labs(title = "We cannot observe emotions. Only the facts aruond them", x = NULL, y = NULL , subtitle = "i = {the_iter}") +


{
# debugonce(gganimate:::assign_filters)
f.animation <- plots.p("circle_to_triangle", ext = "gif")
G.animated <- 
  animate(
    P.animated
    , duration = SECONDS_GIF
    , nframes = N.PLOTS * 3
    # , end_pause = 0
    # , fps = 100
)
anim_save(animation = G.animated, filename = f.animation, fps = 100)
openChromeTabs(f.animation)
}

#| # Setting `keep = TRUE` allows you to keep the culled data on display. Only
#| # exit functions will be used in that case (as elements enters from the
#| # result of the exit function)
#| anim_filter_2 <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
#|   geom_point() +
#|   transition_filter(
#|     transition_length = 2,
#|     filter_length = 1,
#|     Setosa = Species == 'setosa',
#|     Long = Petal.Length > 4,
#|     Wide = Petal.Width > 2,
#|     keep = TRUE
#|   ) +
#|   ggtitle(
#|     'Filter: {closest_filter}',
#|     subtitle = '{closest_expression}'
#|   ) +
#|   exit_recolour(colour = 'grey') +
#|   exit_shrink(size = 0.5)
#| 
#| f.2 <- plots.p("anim_filter_2", ext = "gif")
#| print(anim_filter_2)
#| anim_save(anmation = anim_filter_2, filename = f.2) 
#| .o(f.2)
#| 
