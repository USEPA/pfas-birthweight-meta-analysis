# PFNA data pre-processing

library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(metafor)
library(meta)
library(knitr)
library(kableExtra)
library(ggplot2)
library(RColorBrewer)

setwd("C:/Users/alarsen/OneDrive - Environmental Protection Agency (EPA)/Projects/IRIS/PFAS/pfas-birthweight-meta-analysis")

source("utility_functions.R")

# Primary data set -------------------------------------------------------------

betas <- read_excel("PFNA_Unit_Conversion_v6.xlsx") %>%
  as.data.frame() %>%
  select(c("Study2", "Sample Size", "Population", "Confidence",
           "BetaOrig", "BetaOrigLCL", "BetaOrigUCL", "BetaUnit",
           "BetaNgml", "BetaNgmlSE", "BetaNgmlLCL", "BetaNgmlUCL",
           "BetaLnNgml", "BetaLnNgmlSE", "BetaLnNgmlLCL", "BetaLnNgmlUCL")) %>%
  rename(Study = Study2) %>% 
  separate(col = Study, into = c("Author", "Year"), sep = ",", remove = F) %>% 
  mutate(across(c("Study", "Year"), str_replace, " Boy", "a")) %>%
  mutate(across(c("Study", "Year"), str_replace, " Girl", "b")) %>%
  mutate(Confidence = factor(Confidence, levels = c("High", "Medium", "Low")),
         Population = factor(Population)) %>%
  mutate(Confidence2 = factor(ifelse(Confidence == "Low", "Low", "High+Medium"), levels = c("High+Medium", "Low"))) %>%
  mutate_at(c("BetaNgml", "BetaNgmlSE", "BetaNgmlLCL", "BetaNgmlUCL"), as.double)

# pool studies that report boys and girls separately

betas <- betas %>%
  pool_studies("Lind") %>%
  pool_studies("Robledo") %>%
  pool_studies("Wang")

# join sample timing data to betas

sampling <- read_excel("Sample_Timing.xlsx", sheet = "Sample_Timing") %>%
  unite("Study", Author:Year, sep = ", ")

# TODO re-code to take medians over means, etc. if more than one is reported

sampling2 <- sampling %>%
  select(Study, Mean:Exact) %>%
  pivot_longer(Mean:Exact, names_to = "Summary_Type") %>%
  rename(GA = value) %>%
  drop_na()
sampling <- left_join(sampling, sampling2, by = "Study")

betas <- betas  %>%     ## PFNA
  select(!c(Author, Year)) %>%
  left_join(sampling[, c("Study", "GA", "Bin", "Sampling_1", "Sampling_2", "Sampling_3")], by = "Study") %>%
  mutate(Bin = factor(Bin), 
         Sampling_1 = factor(Sampling_1), 
         Sampling_2 = factor(Sampling_2), 
         Sampling_3 = factor(Sampling_3))
write.csv(betas, "betas_pfna.csv", row.names = F)

# Data set for Sensitivity Analysis on Kwon, 2016 ------------------------------

betas_kwon <- read_excel("PFNA_Unit_Conversion_v2_kwon_options.xlsx") %>%
  as.data.frame() %>%
  select(c("Study2", "Sample Size", "Population", "Confidence",
           "BetaOrig", "BetaOrigLCL", "BetaOrigUCL", "BetaUnit",
           "BetaNgml", "BetaNgmlSE", "BetaNgmlLCL", "BetaNgmlUCL",
           "BetaLnNgml", "BetaLnNgmlSE", "BetaLnNgmlLCL", "BetaLnNgmlUCL")) %>%
  rename(Study = Study2, Sample.Size = `Sample Size`) %>% 
  separate(col = Study, into = c("Author", "Year"), sep = ",", remove = F) %>% 
  filter(Author == "Kwon" & BetaUnit != "g/ln(ng/ml)") %>%
  mutate(Confidence = factor(Confidence, levels = c("High", "Medium", "Low")),
         Population = factor(Population)) %>%
  mutate(Confidence2 = factor(ifelse(Confidence == "Low", "Low", "High+Medium"), levels = c("High+Medium", "Low"))) %>%
  mutate_at(c("BetaNgml", "BetaNgmlSE", "BetaNgmlLCL", "BetaNgmlUCL"), as.double)

sampling <- read_excel("Sample_Timing.xlsx", sheet = "Sample_Timing") %>%
  unite("Study", Author:Year, sep = ", ")

# TODO re-code to take medians over means, etc. if more than one is reported

sampling2 <- sampling %>%
  select(Study, Mean:Exact) %>%
  pivot_longer(Mean:Exact, names_to = "Summary_Type") %>%
  rename(GA = value) %>%
  drop_na()
sampling <- left_join(sampling, sampling2, by = "Study")

betas_kwon <- betas_kwon %>%
  select(!c(Author, Year)) %>%
  left_join(sampling[, c("Study", "GA", "Bin", "Sampling_1", "Sampling_2", "Sampling_3")], by = "Study") %>%
  mutate(Bin = factor(Bin), 
         Sampling_1 = factor(Sampling_1), 
         Sampling_2 = factor(Sampling_2), 
         Sampling_3 = factor(Sampling_3)) %>%
  bind_rows(read.csv("betas_pfna.csv"))

write.csv(betas_kwon, "betas_pfna_kwon_options.csv")
