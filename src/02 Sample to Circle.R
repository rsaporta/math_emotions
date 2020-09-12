N.PLOTS <- 5

CIRCLE_LINE_BEGINS_AT   <- ceiling(N.PLOTS * 1/3) - 1L
TRIANGLE_FILL_BEGINS_AT <- ceiling(N.PLOTS * 1/2)

DT[, plot_numb := NA_integer_]
for (i in seq.int(N.PLOTS)) {
  if (i <= TRIANGLE_FILL_BEGINS_AT)
    inds.avail  <- DT[, .I[is.na(plot_numb) & !CIRCLE & !TRIANGLE]]
  else
    inds.avail  <- DT[, .I[is.na(plot_numb) & !TRIANGLE]]

  SIZE <- 
    nrow(DT) %>%
      times(.05 / N.PLOTS) %>%
      times(sqrt(i)) %>%
      round() %>%
      min(length(inds.avail))

  if (i == 1)
    SIZE %<>% min(10)

  if (i < CIRCLE_LINE_BEGINS_AT)
    PROBS <- DT[inds.avail,  W * ifelse(pt_is_on_circle, 12, 1) * ifelse(CIRCLE, 6, 1)]
  else if (i >= N.PLOTS - 2L)
    PROBS <- DT[inds.avail,  1 / sqrt(sqrt(W))]
  else if (i > TRIANGLE_FILL_BEGINS_AT)
    PROBS <- DT[inds.avail,  1 / (W^2)]
  else 
    PROBS <- NULL

  inds <- 
    inds.avail %>%
      sample(size = SIZE,  prob = PROBS)

  if (i >= CIRCLE_LINE_BEGINS_AT) {
    inds.circle_line <- 
      DT[, .I[is.na(plot_numb) & pt_is_on_circle & !TRIANGLE] %>% 
        {sample(., size = length(.) * 0.10)}
      ]
    if (i == CIRCLE_LINE_BEGINS_AT)
      inds.circle_line %<>% {sample(., length(.) / 2)}
    inds %<>% c(inds.circle_line)
  }

  ## sample a few random dots inside the circle
  if (i == TRIANGLE_FILL_BEGINS_AT - 1L) {
    inds.inside_circle <- 
      DT[, .I[is.na(plot_numb) & CIRCLE & !TRIANGLE] %>% 
        {sample(., size = sum(!is.na(plot_numb)) * 0.01)}
      ]
    inds.circle_line2 <- 
      DT[, .I[is.na(plot_numb) & pt_is_on_circle & !TRIANGLE] %>% 
        {sample(., size = sum(!is.na(plot_numb)) * 0.01)}
      ]
    inds %<>% c(inds.inside_circle, inds.circle_line2)
  }


  # if (i >= TRIANGLE_FILL_BEGINS_AT) {
  #   inds.triangle <- 
  #     DT[, .I[is.na(plot_numb) & CIRCLE & !TRIANGLE] %>% 
  #       {sample(., size = length(.) * 0.05)}
  #     ]
  #   if (i == TRIANGLE_FILL_BEGINS_AT)
  #     inds.triangle %<>% {sample(., length(.) / 2)}
  #   inds %<>% c(inds.triangle)
  # }

  inds %<>% unique()
  verboseMsg(verbose, fmt = "i is %2s | SIZE = %5s | length(inds) = %5s", i, SIZE, length(inds), level.verbose = level.verbose)
  DT[inds, plot_numb := i]
}
print(DT[, .N, keyby = plot_numb])
print(DT[!(pt_is_on_circle), .N, keyby = list(CIRCLE, plot_numb)])

Plots.circle <- 
lapply(seq.int(N.PLOTS), function(i) {
    subt_info <- 
      if (i >= TRIANGLE_FILL_BEGINS_AT) paste("TRIANGLE FILL: ", i - TRIANGLE_FILL_BEGINS_AT + 1L)
      else if (i >= CIRCLE_LINE_BEGINS_AT)   paste("CIRCLE LINE: ",   i - CIRCLE_LINE_BEGINS_AT   + 1L)
      else ""

    ggplot(data = DT[plot_numb <= i][!(TRIANGLE)]) + 
      geom_point(aes(x = x, y = y), color = "darkcyan") + 
      scale_x_continuous(breaks = pretty_breaks(), lim = PLANE$x) + 
      scale_y_continuous(breaks = pretty_breaks(), lim = PLANE$y) +
      nolegend() + 
      labs(title = "This is to confirm that CIRCLE and TRIANGLE overlap well", x = NULL, y = NULL, subtitle = sprintf("i = %i | %s", i, subt_info))
})

printToPDF(Plots.circle, ncol = 2L, height.per = 6, width.per = 6)
