#### Preamble ####
# Purpose: Models for consistency and decency
# Author: Rohan Alexander
# Date: 7 November 2023
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: -
# Any other information needed? The data mutate mirrors those in paper.qmd.


#### Workspace setup ####
# Not loading MASS because of the select conflict
library(tidyverse)
library(rstanarm)


#### Read data ####
data <- read_csv("raw_data/responses_with_human_coding.csv")

data <-
    data |>
    mutate(Prompt_n = factor(Prompt_n,
                            levels = c("Name", "Describe", "Simulate", "Example")),
           Temperature = factor(Temperature),
           Role_n = factor(Role_n, levels = c("Helpful", "Expert")),
           Shot_n = factor(Shot_n, levels = c("Zero", "One", "Few")),
           Version = factor(Version)
    )

data <- 
    data |>
    rowwise() |>
    mutate(
        consistency = round(mean(c(consistency_coder_1, consistency_coder_2), na.rm = TRUE)),
        decency = round(mean(c(decency_coder_1, decency_coder_2), na.rm = TRUE))) |>
    ungroup()

### Model data ####
# Use MASS while getting something working
consistency_model_MASS <-
    MASS::polr(factor(consistency) ~ Version + Prompt_n + Temperature + Role_n + Shot_n, data = data)

decency_model_MASS <-
    MASS::polr(factor(decency) ~ Version + Prompt_n + Temperature + Role_n + Shot_n, data = data)

# Use rstanarm for what we report in the paper
consistency_model_rstanarm <-
    stan_polr(
        factor(consistency) ~ Version + Prompt_n + Temperature + Role_n + Shot_n,
        data = data,
        prior = R2(0.3, "mean"),
        seed = 853)

saveRDS(
  consistency_model_rstanarm,
  file = "model/consistency_model_rstanarm.rds"
)

decency_model_rstanarm <-
   stan_polr(
        factor(decency) ~ Version + Prompt_n + Temperature + Role_n + Shot_n,
        data = data,
        prior = R2(0.3, "mean"),
        seed = 853)

saveRDS(
  decency_model_rstanarm,
  file = "model/decency_model_rstanarm.rds"
)
