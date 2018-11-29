library(dplyr)
library(readODS)
library(reshape2)
library(ggplot2)
library(ggbeeswarm)
library(tidyverse)
library(DT)
library(reshape2)

peptydy <- read.csv("amyloid_heatmap.csv")
peptydy <- peptydy[1:(nrow(peptydy)-1),]
peptydy <- peptydy %>% mutate(match = ifelse(peptydy[["AmyLoad"]] == peptydy[["ThT"]], "amyloid", "non-amyloid"))

peptydy <- peptydy %>% gather("method", "result", 2:4)

pep_order <- filter(peptydy, method == "AmyLoad") %>% arrange(result) %>%  pull(peptide) %>% as.character

peptydy[["peptide"]] <- factor(peptydy[["peptide"]], levels = pep_order)
peptydy_plot <- filter(peptydy, method != "match") %>%
  droplevels() %>% 
  ggplot(aes(x = method, y = peptide, fill = result)) +
  geom_tile() +
  geom_tile(aes(fill = result), color = "white") +
  # scale_fill_manual(values=c("red", "lightgreen")) +
  scale_x_discrete(limits=c("AmyLoad", "ThT")) +
  theme_bw() +
  theme(text = element_text(size=9))


marlena <- read.csv("amyloid_heatmap_Marlena.csv")
marlena <- marlena[1:(nrow(marlena)-1),] %>% 
  gather("method", "result", 2:5)

pep_order_m <- filter(marlena, method == "AmyLoad") %>% arrange(result) %>%  pull(peptide) %>% as.character

marlena[["peptide"]] <- factor(marlena[["peptide"]], levels = pep_order_m)


peptydy_intensity <- read.csv("amyloid_repeats.csv") %>% gather("day_rep", "intensity", 2:16) %>% 
  mutate(day = sapply(strsplit(day_rep, split = "_"), first),
         rep = sapply(strsplit(day_rep, split = "_"), last),
         day = gsub(pattern = "X", replacement = "", x = day)) 

ThT_median <- filter(peptydy_intensity, peptides == "THT") %>% 
  pull(intensity) %>% 
  median


#peptydy_intensity_plot <- 
peptydy_intensity_casted <- inner_join(peptydy_intensity, peptydy, by=c("peptides" = "peptide")) %>% 
  filter(method %in% c("AmyLoad", "ThT")) %>%
  dcast(peptides + day + intensity~ method) %>% 
  mutate(both = paste0(AmyLoad, ThT)) 

plot_intensities <- function(dat){
  ggplot(dat, aes(x = day, y = intensity)) +
  geom_point() +
  geom_hline(yintercept = ThT_median, color = "red") +
  facet_wrap(~ peptides) +
  theme_bw() +
  xlab("Dzień") +
  ylab("Fluorescencja (ThT)") +
  theme(strip.text.x = element_text(margin = margin(0, 0, 0, 0, "cm"))) +
  ggtitle(paste0("AmyLoad: ", dat[["AmyLoad"]][1], "; ThT: ", dat[["ThT"]][1]))
  }

intensities_plots <- lapply(split(peptydy_intensity_casted, peptydy_intensity_casted[["both"]]), plot_intensities)

library(patchwork)

(intensities_plots[[1]] + intensities_plots[[2]])/(intensities_plots[[3]] + intensities_plots[[4]])

