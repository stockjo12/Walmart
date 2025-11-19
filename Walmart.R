### SET UP ###
#Downloading libraries
library(tidyverse)
library(tidymodels)
library(beepr)

#Bringing in Data
library(vroom)
library(janitor)
setwd("~/Library/CloudStorage/OneDrive-BrighamYoungUniversity/STAT 348/Coding/Walmart")
first <- vroom("train.csv")  |>
  clean_names() |>
  mutate(is_holiday = as.integer(is_holiday))
one <- vroom("test.csv") |>
  clean_names()
store <- vroom("stores.csv") |>
  clean_names()
features <- vroom("features.csv") |>
  clean_names() |>
  mutate(is_holiday = as.integer(is_holiday))

#Wrangling Data
#Making Training Data
train <- first |> 
  left_join(features, by = c("store", "date")) |>
  mutate(across(c(mark_down1, mark_down2, mark_down3, mark_down4, mark_down5), 
                ~ coalesce(., 0)),  # Changing NAs to 0
         total_markdown = rowSums(across(c(mark_down1, mark_down2, mark_down3, 
                                           mark_down4, mark_down5))),  #Making Total Markdown
         mark_down_flag = if_else(total_markdown > 0, 1, 0)) |> #Indicator Variable of Markdown
  select(-mark_down1, -mark_down2, -mark_down3, 
         -mark_down4, -mark_down5, everything()) #Getting rid of old Markdown Variables

#Making Test Data
test <- one |> 
  left_join(features, by = c("store", "date")) |>
  mutate(across(c(mark_down1, mark_down2, mark_down3, mark_down4, mark_down5), 
                ~ coalesce(., 0)),  # Changing NAs to 0
         total_markdown = rowSums(across(c(mark_down1, mark_down2, mark_down3, 
                                           mark_down4, mark_down5))),  #Making Total Markdown
         mark_down_flag = if_else(total_markdown > 0, 1, 0)) |> #Indicator Variable of Markdown
  select(-mark_down1, -mark_down2, -mark_down3, 
         -mark_down4, -mark_down5, everything()) #Getting rid of old Markdown Variables

### FEATURE ENGINEERING ###
library(embed)
#Making Recipe
walmart_recipe <- recipe(type ~ ., data = train) |>
  step_lencode_mixed("is_holiday", outcome = type)

### WORK FLOW ###
#MODEL TYPE
#TBD


#Creating Workflows


### CROSS VALIDATION ###
#Defining Grids of Values


#Splitting Data


#Run Cross Validations


#Find Best Tuning Parameters


#Finalizing Workflow


### SUBMISSION ###
#Making Predictions


#Formatting Predictions for Kaggle


#Saving CSV File

