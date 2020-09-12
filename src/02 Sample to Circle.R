N.PLOTS <- 4

DT[, plot_numb := NA_integer_]
for (i in seq.int(N.PLOTS)) {
  inds.circle <- DT[, .I[is.na(plot_numb) &  pt_is_on_circle & !TRIANGLE]]
  inds.avail  <- DT[, .I[is.na(plot_numb) & !pt_is_on_circle & !CIRCLE & !TRIANGLE]]

  SIZE <- nrow(DT) * (0.006 * sqrt(i))

  if (i <= 2) {
    SIZE.circle <- 0L
    SIZE.avail  <- round(SIZE)
  } else {
    SIZE.circle <- (SIZE * 0.05) %>% round() %>% min(length(inds.circle))
    SIZE.avail  <- (SIZE * 0.95) %>% round() %>% min(length(inds.avail))
  }

  inds <- c(
      sample(inds.circle, size = SIZE.circle, prob = DT[inds.circle, W])
    , sample(inds.avail,  size = SIZE.avail,  prob = DT[inds.avail,  W])
  )
  inds %<>% unique()

  DT[inds, plot_numb := i]
}
print(DT[, .N, keyby = plot_numb])

Plots.circle <- 
lapply(seq.int(N.PLOTS), function(i) {
    ggplot(data = DT[plot_numb <= i]) + 
      geom_point(aes(x = x, y = y), color = "darkcyan") + 
      scale_x_continuous(breaks = pretty_breaks(), lim = PLANE$x) + 
      scale_y_continuous(breaks = pretty_breaks(), lim = PLANE$y) +
      nolegend() + 
      labs(title = "This is to confirm that CIRCLE and TRIANGLE overlap well", x = NULL, y = NULL, subtitle = sprintf("i = %i", i))
})

printToPDF(Plots.circle, nrow = 2L, height.per = 6, width.per = 6)
