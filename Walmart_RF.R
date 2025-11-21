### SET UP ###
#Downloading libraries
library(tidyverse)
library(tidymodels)
library(beepr)
library(vroom)
library(janitor)
library(ranger)
library(embed)
library(doParallel)

#Bringing in Data
# setwd("~/Library/CloudStorage/OneDrive-BrighamYoungUniversity/STAT 348/Coding/Walmart")
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
         -mark_down4, -mark_down5, everything()) |> #Getting rid of old Markdown Variables
  arrange(store, date)

#Making Test Data
test <- one |> 
  left_join(features, by = c("store", "date")) |>
  mutate(across(c(mark_down1, mark_down2, mark_down3, mark_down4, mark_down5), 
                ~ coalesce(., 0)),  # Changing NAs to 0
         total_markdown = rowSums(across(c(mark_down1, mark_down2, mark_down3, 
                                           mark_down4, mark_down5))),  #Making Total Markdown
         mark_down_flag = if_else(total_markdown > 0, 1, 0)) |> #Indicator Variable of Markdown
  select(-mark_down1, -mark_down2, -mark_down3, 
         -mark_down4, -mark_down5, everything()) |> #Getting rid of old Markdown Variables
  arrange(store, date)

### FEATURE ENGINEERING ###
#Making Recipe
walmart_recipe <- recipe(weekly_sales ~ ., data = train) |>
  step_mutate(decdate = decimal_date(date)) |>
  step_mutate(
    super_bowl = date %in% as.Date(c("2010-02-12","2011-02-11","2012-02-10","2013-02-08")),
    labor_day  = date %in% as.Date(c("2010-09-10","2011-09-09","2012-09-07","2013-09-06")),
    thanksgiving = date %in% as.Date(c("2010-11-26","2011-11-25","2012-11-23","2013-11-29")),
    christmas  = date %in% as.Date(c("2010-12-31","2011-12-30","2012-12-28","2013-12-27"))
  ) |>
  step_mutate(
    super_bowl = as.integer(super_bowl),
    labor_day = as.integer(labor_day),
    thanksgiving = as.integer(thanksgiving),
    christmas = as.integer(christmas)
  ) |>
  step_impute_bag(cpi, unemployment,
                  impute_with = imp_vars(decdate, store)) |>
  step_date(date, features = "doy") |>
  step_range(date_doy, min = 0, max = pi) |>
  step_mutate(sindoy = sin(date_doy), cosdoy = cos(date_doy)) |>
  step_lag(weekly_sales, lag = c(1, 2, 3, 4, 52)) |>
  step_rm(date, date_doy)
juiced <- juice(prep(walmart_recipe))

### WORK FLOW ###
#Random Forest
#Defining Model
forest_model <- rand_forest(mtry = tune(), #27
                            min_n = tune(), #2
                            trees = 50) |>
  set_engine("ranger") |>
  set_mode("regression")

#Creating Workflows
forest_wf <- workflow() |>
  add_recipe(walmart_recipe)|>
  add_model(forest_model)

### CROSS VALIDATION ###
#Defining Grids of Values
max <- ncol(juiced) - 1
forest_grid <- grid_regular(mtry(range = c(1, max)),
                            min_n(),
                            levels = 2)

#Splitting Data
forest_folds <- vfold_cv(train, 
                         v = 3, 
                         repeats = 1)

#Run Cross Validations
forest_results <- forest_wf |>
  tune_grid(resamples = forest_folds,
            grid = forest_grid,
            metrics = metric_set(mae)) 
beepr::beep()

#Find Best Tuning Parameters
forest_best <- forest_results |>
  select_best(metric = "mae")

#Finalizing Workflow
final_fwf <- forest_wf |>
  finalize_workflow(forest_best) |>
  fit(data = train)
beepr::beep()

### SUBMISSION ###
#Making Predictions
forest_pred <- predict(final_fwf, new_data = test) 

#Formatting Predictions for Kaggle
kaggle_forest <- forest_pred |>
  rename(Weekly_Sales = .pred) |>
  bind_cols(test) |> 
  select(Id, Weekly_Sales) 

#Saving CSV File
vroom_write(x=kaggle_forest, file="./Forest_Test.csv", delim=",")
