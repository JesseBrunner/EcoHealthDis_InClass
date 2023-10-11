library(tidyverse)

df <- tibble(Dice = c(1, 2, 3, 4, 5, 7, 10, 15, 20, 30, 40),
             Prob = 1-(1-1/6)^Dice)



ggplot(df, aes(Dice, Prob)) +
  geom_point() +
  geom_blank() +
  scale_x_continuous("Dice rolled",
                     breaks = df$Dice,
                     minor_breaks = NULL) +
  scale_y_continuous("Probability of at least one 6",
                     limits = c(0,1),
                     breaks = 0:10/10) +
  theme_bw() + theme(text=element_text(size = 20))

last_plot() +
  scale_x_log10("Dice rolled",
                breaks = df$Dice,
                minor_breaks = NULL)
