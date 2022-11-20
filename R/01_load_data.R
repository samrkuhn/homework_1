library(tidyverse)
library(here)
library(tidymodels)
library(corrplot)
library(MASS)
library(gt)

training_set <- readr::read_csv(here::here("data", "moneyball-training-data (1).csv")) |> 
  janitor::clean_names()

#Sum NAs across columns
training_set |> 
  summarise(across(everything(), ~ sum(is.na(.))))

#Replace missing values (NAs) with median values
training_set <- training_set |> 
  mutate(across(everything(), ~tidyr::replace_na(., median(., na.rm = TRUE))))

#Verify results
#Sum NAs across columns
training_set |> 
  summarise(across(everything(), ~ sum(is.na(.))))

#Summary statistics

##Median
training_set |> 
  dplyr::select(-index) |> 
  summarise(across(everything(), ~ median(.))) |> 
  glimpse()

##Mean
training_set |> 
  dplyr::select(-index) |> 
  summarise(across(everything(), ~ mean(.))) |> 
  glimpse()

##Standard Deviation
training_set |> 
  dplyr::select(-index) |> 
  summarise(across(everything(), ~ sd(.))) |> 
  glimpse()

# Log transformation
training_set |> 
  mutate(across(
    .cols = c("team_batting_h", "team_batting_3b", "team_pitching_bb", 
              "team_batting_so", "team_baserun_sb", "team_baserun_cs",
              "team_batting_hbp", "team_batting_so", "team_fielding_e"),
    .fns = log
  ))

