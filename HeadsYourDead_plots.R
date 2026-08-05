library(tidyverse)
library(ggpubr)

a <- tibble(x=0:18,
       y=36*4*(5/6)^x) %>% 
  ggplot(., aes(x,y)) + 
  #geom_point() +
  scale_y_continuous(expression(Number~alive~(n[x])), breaks = 0:15*10, minor_breaks = 1:150) + 
  scale_x_continuous("Time step (x)", minor_breaks = 0:18) + 
  theme_bw()

b <- a + scale_y_log10(expression(Number~alive~(n[x])), breaks = 0:15*10, minor_breaks = 1:150)

ggarrange(a,b, nrow = 1)
ggsave("Nx_x.pdf", width = 6, height = 6)  
