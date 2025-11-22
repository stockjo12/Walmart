### SET UP ###
#Downloading libraries
library(tidyverse)
library(tidymodels)
library(beepr)
library(vroom)
library(janitor)
library(prophet)
library(patchwork)

#Bringing in Data
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

## Impute Missing CPI and Unemployment
walmart_recipe <- recipe(weekly_sales ~ ., data = train) |>
  step_mutate(decdate = decimal_date(date)) |>
  step_impute_bag(cpi, unemployment,
                  impute_with = imp_vars(decdate, store))
prep <- prep(walmart_recipe)
juiced <- juice(prep)
baked <- bake(prep, new_data = test)

#Making Dataset for Prophet Model for Store 1 Department 2
Store1 <- 1
Dept2 <- 2
sd_train12 <- juiced |>
  filter(store == Store1, dept == Dept2) |>
  rename(y = weekly_sales, ds = date)
sd_test12 <- baked |>
  filter(store == Store1, dept == Dept2) |>
  rename(ds = date)

#Making Dataset for Prophet Model for Store 2 Department 40
Store2 <- 2
Dept40 <- 40
sd_train240 <- juiced |>
  filter(store == Store2, dept == Dept40) |>
  rename(y = weekly_sales, ds = date)
sd_test240 <- baked |>
  filter(store == Store2, dept == Dept40) |>
  rename(ds = date)

### WORK FLOW ###
#Facebook Prophet
#Defining Model for Store 1 Department 2
prophet_model12 <- prophet() |>
  add_regressor("cpi") |>
  add_regressor("unemployment") |>
  add_regressor("total_markdown") |>
  add_regressor("mark_down_flag") |>
  fit.prophet(df=sd_train12)

#Defining Model for Store 2 Department 40
prophet_model240 <- prophet() |>
  add_regressor("cpi") |>
  add_regressor("unemployment") |>
  add_regressor("total_markdown") |>
  add_regressor("mark_down_flag") |>
  fit.prophet(df=sd_train240)

#Making Predictions for Store 1 Department 2
predict(prophet_model12, df = sd_test12)

#Making Predictions for Store 1 Department 2
predict(prophet_model240, df = sd_test240)

#Making Predictions with Fitted Values for Store 1 Department 2
fitted_vals12 <- predict(prophet_model12, df=sd_train12) 
test_preds12 <- predict(prophet_model12, df=sd_test12) 

#Making Predictions with Fitted Values for Store 1 Department 2
fitted_vals240 <- predict(prophet_model240, df=sd_train240) 
test_preds240 <- predict(prophet_model240, df=sd_test240) 

#Plot Fitted and Forecast for Store 1 Department 2
plot12 <- ggplot() +
  geom_line(data = sd_train12, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals12, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds12, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="",
       title = "Store 1 Department 2",
       x = "Year",
       y = "Sales ($)")

#Plot Fitted and Forecast for Store 2 Department 40
plot240 <- ggplot() +
  geom_line(data = sd_train240, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals240, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds240, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="",
       title = "Store 2 Department 40",
       x = "Year",
       y = "Sales ($)")

#Combining Plots Together
plots <- plot12 + plot240

#Saving Plots
ggsave("Facebook_Prophet.pdf", plots,
       width = 12, height = 6)
