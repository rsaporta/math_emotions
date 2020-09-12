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
