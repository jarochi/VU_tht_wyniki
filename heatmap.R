library(dplyr)
library(readODS)
library(reshape2)
library(ggplot2)
library(ggbeeswarm)
library(tidyverse)

peptydy <- read.csv("amyloid_heatmap.csv")
peptydy <- peptydy[1:(nrow(peptydy)-1),]
peptydy <- peptydy %>% mutate(match = ifelse(peptydy$database == peptydy$tht, "yes", "no"))

peptydy <- peptydy %>% gather("method", "result", 2:4)




ggplot(peptydy, aes(x = method, y = peptide, fill = result)) +
  geom_tile() +
  geom_tile(aes(fill = result), color = "white") +
  # scale_fill_manual(values=c("red", "lightgreen")) +
  scale_x_discrete(limits=c("database","tht","match")) +
  theme_bw()



