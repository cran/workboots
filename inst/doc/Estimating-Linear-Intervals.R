## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

ggplot2::theme_set(
  ggplot2::theme_minimal() +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"))
)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(tidymodels)

# setup our data
data("ames")
ames_mod <- ames %>% select(First_Flr_SF, Sale_Price)

# baseline plot
ames_mod %>%
  ggplot(aes(x = First_Flr_SF, y = Sale_Price)) +
  geom_point(alpha = 0.25) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))

## -----------------------------------------------------------------------------
# log transform
ames_mod <-
  ames_mod %>%
  mutate(across(everything(), log10))

# split into train/test data
set.seed(918)
ames_split <- initial_split(ames_mod)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

# train a linear model
set.seed(314)
ames_lm <- lm(Sale_Price ~ First_Flr_SF, data = ames_train)

# predict on new data with a prediction interval
ames_lm_pred_int <-
  ames_lm %>%
  predict(ames_test, interval = "predict") %>%
  as_tibble()

ames_lm_pred_int %>%
  
  # rescale predictions to match the original dataset's scale
  bind_cols(ames_test) %>%
  mutate(across(everything(), ~10^.x)) %>%
  
  # plot!
  ggplot(aes(x = First_Flr_SF)) +
  geom_point(aes(y = Sale_Price),
             alpha = 0.25) +
  geom_line(aes(y = fit),
            size = 1) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.25) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))

## ---- eval=FALSE--------------------------------------------------------------
#  library(workboots)
#  
#  # setup a workflow with a linear model
#  ames_wf <-
#    workflow() %>%
#    add_recipe(recipe(Sale_Price ~ First_Flr_SF, data = ames_train)) %>%
#    add_model(linear_reg())
#  
#  # generate bootstrap predictions on ames test
#  set.seed(713)
#  ames_boot_pred_int <-
#    ames_wf %>%
#    predict_boots(
#      n = 2000,
#      training_data = ames_train,
#      new_data = ames_test
#    )

## ---- echo=FALSE--------------------------------------------------------------
library(workboots)

options(timeout = 120)

# load data from workboots_support (avoid re-fitting on knit)
ames_boot_pred_int <- readr::read_rds("https://github.com/markjrieke/workboots_support/blob/main/data/ames_boot_pred_int.rds?raw=true")

## -----------------------------------------------------------------------------
ames_boot_pred_int %>%
  summarise_predictions() %>%
  
  # rescale predictions to match original dataset's scale
  bind_cols(ames_lm_pred_int) %>%
  bind_cols(ames_test) %>%
  mutate(across(.pred:Sale_Price, ~10^.x)) %>%
  
  # plot!
  ggplot(aes(x = First_Flr_SF)) +
  geom_point(aes(y = Sale_Price),
             alpha = 0.25) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  # add prediction interval created by lm()
  geom_line(aes(y = fit),
            size = 1) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.25) +
  
  # add prediction interval created by workboots
  geom_point(aes(y = .pred),
             color = "blue",
             alpha = 0.25) +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                color = "blue",
                alpha = 0.25,
                width = 0.0125)

## -----------------------------------------------------------------------------
ames_boot_pred_int %>%
  
  # generate a 95% prediction interval
  summarise_predictions(interval_width = 0.95) %>%
  rename(.pred_lower_95 = .pred_lower,
         .pred_upper_95 = .pred_upper) %>%
  select(-.pred) %>%
  
  # generate 80% prediction interval
  summarise_predictions(interval_width = 0.80) %>%
  rename(.pred_lower_80 = .pred_lower,
         .pred_upper_80 = .pred_upper) %>%
  
  # rescale predictions to match original dataset's scale
  bind_cols(ames_test) %>%
  mutate(across(.pred_lower_95:Sale_Price, ~10^.x)) %>%
  
  # plot!
  ggplot(aes(x = First_Flr_SF)) +
  geom_point(aes(y = Sale_Price),
             alpha = 0.25) +
  geom_line(aes(y = .pred),
            size = 1,
            color = "blue") +
  geom_ribbon(aes(ymin = .pred_lower_95,
                  ymax = .pred_upper_95),
              alpha = 0.25,
              fill = "blue") +
  geom_ribbon(aes(ymin = .pred_lower_80,
                  ymax = .pred_upper_80),
              alpha = 0.25,
              fill = "blue") +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))

## ---- eval=FALSE--------------------------------------------------------------
#  # generate linear model confidence interval for reference
#  ames_lm_conf_int <-
#    ames_lm %>%
#    predict(ames_test, interval = "confidence") %>%
#    as_tibble()
#  
#  # generate bootstrap predictions on test set
#  set.seed(867)
#  ames_boot_conf_int <-
#    ames_wf %>%
#    predict_boots(
#      n = 2000,
#      training_data = ames_train,
#      new_data = ames_test,
#      interval = "confidence"
#    )

## ---- echo=FALSE--------------------------------------------------------------
# load data from workboots_support (avoid re-fitting on knit)
ames_boot_conf_int <- readr::read_rds("https://github.com/markjrieke/workboots_support/blob/main/data/ames_boot_conf_int.rds?raw=true")

ames_lm_conf_int <-
  ames_lm %>%
  predict(ames_test, interval = "confidence") %>%
  as_tibble()

## -----------------------------------------------------------------------------
ames_boot_conf_int %>%
  summarise_predictions() %>%
  
  # rescale predictions to match original dataset's scale
  bind_cols(ames_lm_conf_int) %>%
  bind_cols(ames_test) %>%
  mutate(across(.pred:Sale_Price, ~10^.x)) %>%

  # plot!
  ggplot(aes(x = First_Flr_SF)) +
  geom_point(aes(y = Sale_Price),
             alpha = 0.25) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  # add prediction interval created by lm()
  geom_line(aes(y = fit),
            size = 1) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.25) +
  
  # add prediction interval created by workboots
  geom_point(aes(y = .pred),
             color = "blue",
             alpha = 0.25) +
  geom_ribbon(aes(ymin = .pred_lower,
                  ymax = .pred_upper),
              fill = "blue",
              alpha = 0.25)

