## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 5
)

ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"))
)

## -----------------------------------------------------------------------------
library(tidymodels)

data("penguins")

penguins <- 
  penguins %>%
  drop_na()

penguins

## -----------------------------------------------------------------------------
# split data into training and testing sets
set.seed(123)
penguins_split <- initial_split(penguins)
penguins_test <- testing(penguins_split)
penguins_train <- training(penguins_split)

# create a workflow
penguins_wf <-
  workflow() %>%
  
  # add preprocessing steps
  add_recipe(
    recipe(body_mass_g ~ ., data = penguins_train) %>%
      step_dummy(all_nominal_predictors())
  ) %>%
  
  # add xgboost model specification
  add_model(
    boost_tree("regression") %>% set_engine("xgboost")
  )

# fit to training data & predict on test data
set.seed(234)
penguins_preds <-
  penguins_wf %>%
  fit(penguins_train) %>%
  predict(penguins_test)

## -----------------------------------------------------------------------------
penguins_preds %>%
  bind_cols(penguins_test) %>%
  ggplot(aes(x = body_mass_g,
             y = .pred)) +
  geom_point() +
  geom_segment(aes(x = 3000, xend = 6000,
                   y = 3000, yend = 6000),
               linetype = "dashed",
               color = "gray") +
  labs(title = "Single XGBoost Model Predictions")

## ---- eval=FALSE--------------------------------------------------------------
#  library(workboots)
#  
#  # create 2000 models from bootstrap resamples and make predictions on the test set
#  set.seed(345)
#  penguins_preds_boot <-
#    penguins_wf %>%
#    predict_boots(
#      n = 2000,
#      training_data = penguins_train,
#      new_data = penguins_test
#    )
#  
#  penguins_preds_boot

## ---- echo=FALSE--------------------------------------------------------------
library(workboots)

# load data from workboots_support (avoid re-fitting on knit)
penguins_preds_boot <-readr::read_rds("https://github.com/markjrieke/workboots_support/blob/main/data/penguins_preds.rds?raw=true") 
penguins_preds_boot

## -----------------------------------------------------------------------------
penguins_preds_boot %>%
  summarise_predictions()

## -----------------------------------------------------------------------------
penguins_preds_boot %>%
  summarise_predictions() %>%
  bind_cols(penguins_test) %>%
  ggplot(aes(x = body_mass_g,
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_abline(linetype = "dashed",
              color = "gray") + 
  geom_errorbar(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "XGBoost Model Prediction Intervals from Bootstrap Resampling",
       subtitle = "Error bars represent the 95% prediction interval")

