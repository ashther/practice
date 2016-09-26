
library(plotly)
library(magrittr)

plot_ly(df, 
        x = DATE, 
        y = MTD_APPROVED_VOL, 
        type = 'bar', 
        name = 'MTD_APPROVED_VOL') %>% 
  add_trace(x = DATE, 
            y = MTD_APPROVED_RATE, 
            yaxis = 'y2', 
            text = MTD_APPROVED_RATE, 
            mode = 'text+lines',
            textposition = 'inside', 
            name = 'MTD_APPROVED_RATE') %>% 
  layout(xaxis = list(range = c(0, 10)),
         yaxis2 = list(side = 'right', 
                       overlaying = 'y', 
                       title = 'MTD_APPROVED_RATE', 
                       range = c(35, 60)),
         yaxis = list(range = c(0, 1.5 * max(df$MTD_APPROVED_VOL))), 
         legend = list(x = 0, y = 1),
         title = 'SEP_APPROVED_VOL & RATE', 
         margin = list(r = 50))
