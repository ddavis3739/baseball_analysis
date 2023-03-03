# load packages
require(ggplot2); library(car); require(Hmisc); library(knitr)
require(dplyr); require(tidyr); library(ggsci); library(data.table); library(ggraph)
require(phyloseq)
require(psadd)
require(ggtree)
library(microViz)
library(jsonlite)
library(ggpubr)
library(multcompView)
require(gridExtra)
library(randomcoloR)
library(MiscMetabar)
# library(microbiomeMarker)
require(tidyverse)
library(tidygraph)
library(igraph)
# library(EcoSimR)
library(DT)
library(vegan)

mytheme <- theme_bw() + theme(axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              #panel.border = element_blank(),
                              #panel.background = element_blank(),
                              axis.text=element_text(size=11, color = 'black'),
                              axis.title=element_text(size=12),
                              legend.text=element_text(size=12))
# Pitch tempo measures the median time between pitches 
# (in other words pitch release to pitch release). 
# Only pitches that follow a take (called strike or called ball) 
# and are thrown to the same batter are considered for this metric.

pitchT <- read_csv('/Users/andrewdavis/Downloads/pitchMedianPlayer.csv')
pitchT$Season <- as.factor(pitchT$Season)
# bases empty

Tempo_empty_lm <- lm(Tempo_empty ~ Season, pitchT)
Tempo_empty_anova <- aov(Tempo_empty_lm)
Tempo_empty_anova_summary <- summary(Tempo_empty_anova)
Tempo_empty_tukey <- TukeyHSD(Tempo_empty_anova)

# tukey values
# table with factors and 3rd quantile
tk <- 
  pitchT %>%
  group_by(Season) %>%
  summarise(mean=mean(Tempo_empty), quant = quantile(Tempo_empty, probs = 0.75)) %>%
  arrange(desc(mean))

# compact letter display
cld <- multcompLetters4(Tempo_empty_anova, Tempo_empty_tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Season)
tk$cld <- cld$Letters


# Box plots to represent differences in shannon


pitchT %>%
  ggplot(aes(x = Season, 
             y = Tempo_empty)) +
  geom_boxplot() + 
  geom_jitter(aes(color = Pitches_empty), alpha = .25, height = 0) + 
  mytheme + 
  labs(x = "Season", y = "Median Pitch Time by Player") +
  stat_compare_means(method = "anova") + 
  geom_text(data = tk, aes(x = Season, y = quant, label = cld),  
            size = 3, vjust=-1, hjust =-1) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = 'Median Pitch Time change by Season (Bases Empty)') +
  scale_color_gradient(position="bottom" , low = "blue", high = "red") 



# bases empty

Tempo_onBase_lm <- lm(Tempo_onBase ~ Season, pitchT)
Tempo_onBase_anova <- aov(Tempo_onBase_lm)
Tempo_onBase_anova_summary <- summary(Tempo_onBase_anova)
Tempo_onBase_tukey <- TukeyHSD(Tempo_onBase_anova)

# tukey values
# table with factors and 3rd quantile
tk <- 
  pitchT %>%
  group_by(Season) %>%
  summarise(mean=mean(Tempo_onBase), quant = quantile(Tempo_onBase, probs = 0.75)) %>%
  arrange(desc(mean))

# compact letter display
cld <- multcompLetters4(Tempo_onBase_anova, Tempo_onBase_tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Season)
tk$cld <- cld$Letters


# Box plots to represent differences in shannon


pitchT %>%
  ggplot(aes(x = Season, 
             y = Tempo_onBase)) +
  geom_boxplot() + 
  geom_jitter(aes(color = Pitches_onBase), alpha = .25, height = 0) + 
  mytheme + 
  labs(x = "Season", y = "Median Pitch Time by Player") +
  stat_compare_means(method = "anova") + 
  geom_text(data = tk, aes(x = Season, y = quant, label = cld),  
            size = 3, vjust=-1, hjust =-1) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = 'Median Pitch Time change by Season (Runners On-Base)')+ 
  scale_color_gradient(position="bottom" , low = "blue", high = "red") 

################################
### TEAM
pitchT <- read_csv('/Users/andrewdavis/Downloads/pitchMedianTeam.csv')
pitchT$Season <- as.factor(pitchT$Season)
# bases empty

Tempo_empty_lm <- lm(Tempo_empty ~ Season, pitchT)
Tempo_empty_anova <- aov(Tempo_empty_lm)
Tempo_empty_anova_summary <- summary(Tempo_empty_anova)
Tempo_empty_tukey <- TukeyHSD(Tempo_empty_anova)

# tukey values
# table with factors and 3rd quantile
tk <- 
  pitchT %>%
  group_by(Season) %>%
  summarise(mean=mean(Tempo_empty), quant = quantile(Tempo_empty, probs = 0.75)) %>%
  arrange(desc(mean))

# compact letter display
cld <- multcompLetters4(Tempo_empty_anova, Tempo_empty_tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Season)
tk$cld <- cld$Letters


# Box plots to represent differences in shannon


pitchT %>%
  ggplot(aes(x = Season, 
             y = Tempo_empty)) +
  geom_boxplot() + 
  geom_jitter(aes(color = Pitches_empty), alpha = .25, height = 0) + 
  mytheme + 
  labs(x = "Season", y = "Median Pitch Time by Team") +
  stat_compare_means(method = "anova") + 
  geom_text(data = tk, aes(x = Season, y = quant, label = cld),  
            size = 3, vjust=-1, hjust =-1) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = 'Median Pitch Time change by Season (Bases Empty)') +
  scale_color_gradient(position="bottom" , low = "blue", high = "red") 



# bases empty

Tempo_onBase_lm <- lm(Tempo_onBase ~ Season, pitchT)
Tempo_onBase_anova <- aov(Tempo_onBase_lm)
Tempo_onBase_anova_summary <- summary(Tempo_onBase_anova)
Tempo_onBase_tukey <- TukeyHSD(Tempo_onBase_anova)

# tukey values
# table with factors and 3rd quantile
tk <- 
  pitchT %>%
  group_by(Season) %>%
  summarise(mean=mean(Tempo_onBase), quant = quantile(Tempo_onBase, probs = 0.75)) %>%
  arrange(desc(mean))

# compact letter display
cld <- multcompLetters4(Tempo_onBase_anova, Tempo_onBase_tukey)
# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Season)
tk$cld <- cld$Letters


# Box plots to represent differences in shannon


pitchT %>%
  ggplot(aes(x = Season, 
             y = Tempo_onBase)) +
  geom_boxplot() + 
  geom_jitter(aes(color = Pitches_onBase), alpha = .25, height = 0) + 
  mytheme + 
  labs(x = "Season", y = "Median Pitch Time by Team") +
  stat_compare_means(method = "anova") + 
  geom_text(data = tk, aes(x = Season, y = quant, label = cld),  
            size = 3, vjust=-1, hjust =-1) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = 'Median Pitch Time change by Season (Runners On-Base)')+ 
  scale_color_gradient(position="bottom" , low = "blue", high = "red") 



