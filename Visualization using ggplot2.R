library(dplyr)
library(tidyr)
library(ggplot2)

#set working directory
setwd("/Users/pritijain/Downloads")

#import precipitation data
precip <- read.csv("pcp_final.csv")

#filter several years and one state and make a side-by-side box plot
precip_reduc <- precip %>%
  filter(Year >=2002 & Year <= 2003) %>%
  filter(State == "Virginia")
  
#plotting the box plot
bp <- precip_reduc %>%
  ggplot(aes(x = Month, y = Precip, fill = State)) +
  geom_boxplot()
  scale_fill_manual(values = c("Virginia" = "pink"))
bp

#filter one year and a few states and make a column chart
precip_reduc2 <- precip %>%
  filter(Year == 1985) %>%
  filter(State == "Texas" | State == "California")

#to colors from color palettes installed in R.
install.packages("RColorBrewer")
library(RColorBrewer)

#plotting the column chart
col <- precip_reduc2 %>%
     ggplot(aes(x = reorder(Month, Precip), y = Precip, color = State, fill = State)) +
     geom_col() +
     coord_flip() +
     scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set2")
col



