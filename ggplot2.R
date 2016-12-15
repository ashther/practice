library(tidyverse)
library(ggthemes)
library(ggthemr)
library(wesanderson)
library(gridExtra)

set.seed(1234)
x <- c(rnorm(500, mean = -1), rnorm(500, mean = 1.5))
y <- c(rnorm(500, mean = 1), rnorm(500, mean = 1.7))
group <- as.factor(rep(c(1,2), each = 500))
df <- data.frame(x, y, group)

ggthemr('light')
on.exit(ggthemr_reset())

xdensity <- ggplot(df, aes(x)) + 
  geom_density(aes(color = group, fill = group), size = 1.2, alpha = 0.3) + 
  scale_color_manual(values = wes_palette('Moonrise2', 2)) + 
  scale_fill_manual(values = wes_palette('Moonrise2', 2))

ydensity <- ggplot(df, aes(y)) + 
  geom_density(aes(color = group, fill = group), size = 1.2, alpha = 0.3) + 
  scale_color_manual(values = wes_palette('Moonrise2', 2)) + 
  scale_fill_manual(values = wes_palette('Moonrise2', 2))

xyscatter <- ggplot(df, aes(x, y, color = group)) + 
  geom_point() + 
  theme(legend.position = c(0, 1), 
        legend.justification = c(0, 1)) + 
  scale_color_manual(values = wes_palette('Moonrise2', 2))

blankplot <- ggplot() + 
  geom_blank(aes(1, 1)) + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank())

grid.arrange(xdensity, blankplot, xyscatter, ydensity, 
             nrow = 2, widths = c(4, 2), heights = c(2, 4))
