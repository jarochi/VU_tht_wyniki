library(dplyr)
library(readODS)
library(reshape2)
library(ggplot2)
library(ggbeeswarm)
library(tidyverse)

peptides <- read.csv("VU_peptides.csv")
colnames(peptides)[5:9] <- c("day 0", "day 1", "day 2", "day 3", "day 4")

# peptides <- peptides[1:(nrow(peptides)-1),]
peptides <- peptides[,-c(1, 3, 4)]

peptides <- peptides %>% gather("day", "emission", 2:6)

# pep_order <- filter(peptides, method == "AmyLoad") %>% arrange(result) %>%  pull(peptide) %>% as.character

# peptides[["peptide"]] <- factor(peptides[["peptide"]], levels = pep_order)

ggplot(peptides, aes(x = day, y = peptides, fill = emission)) +
  geom_tile() +
  geom_tile(aes(fill = emission), color = "white") +
  # scale_fill_manual(values=c("red", "lightgreen")) +
  # scale_x_discrete(limits=c("AmyLoad","ThT","match")) +
  theme_bw()



