anim <- ggplot(airquality, aes(Day, Temp, group = Month)) +
  geom_line() +
  transition_reveal(Day)

# Non-paths will only show the current position, not the history
anim1 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
  geom_line() +
  geom_point(colour = 'red', size = 3) +
  transition_reveal(Day)

# Points can be kept by giving them a unique group and set `keep = TRUE` (the
# default)
anim2 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
  geom_line() +
  geom_point(aes(group = seq_along(Day))) +
  geom_point(colour = 'red', size = 3) +
  transition_reveal(Day)

f.1 <- plots.p("anim1", ext = "gif")
print(anim1)
anim_save(anmation = anim1, filename = f.1) 
.o(f.1)

f.2 <- plots.p("anim2", ext = "gif")
print(anim2)
anim_save(anmation = anim2, filename = f.2) 
.o(f.2)



=================================

anim_filter_1 <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
  geom_point() +
  transition_filter(
    transition_length = 2,
    filter_length = 1,
    Setosa = Species == 'setosa',
    Long = Petal.Length > 4,
    Wide = Petal.Width > 2
  ) +
  ggtitle(
    'Filter: {closest_filter}',
    subtitle = '{closest_expression}'
  ) +
  enter_fade() +
  exit_fly(y_loc = 0)


f.1 <- plots.p("anim_filter_1", ext = "gif")
print(anim_filter_1)
anim_save(anmation = anim_filter_1, filename = f.1) 
.o(f.1)


# Setting `keep = TRUE` allows you to keep the culled data on display. Only
# exit functions will be used in that case (as elements enters from the
# result of the exit function)
anim_filter_2 <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
  geom_point() +
  transition_filter(
    transition_length = 2,
    filter_length = 1,
    Setosa = Species == 'setosa',
    Long = Petal.Length > 4,
    Wide = Petal.Width > 2,
    keep = TRUE
  ) +
  ggtitle(
    'Filter: {closest_filter}',
    subtitle = '{closest_expression}'
  ) +
  exit_recolour(colour = 'grey') +
  exit_shrink(size = 0.5)

f.2 <- plots.p("anim_filter_2", ext = "gif")
print(anim_filter_2)
anim_save(anmation = anim_filter_2, filename = f.2) 
.o(f.2)

