rm(list = ls())
gc()
library(data.table)
library(ggplot2)
library(ranger)
library(glmnet)
set.seed(123)

data_path <- 'E:/CS-GO-Analytics/Processed Data/'

CalibratedProbs <- function(df_, model_, interval_width_){
  # add columns that show how well predicted probabilities match reality
  df_[, eval(paste0('probs_grp', model_)) := cut(get(paste0('probs', model_)), breaks = seq(0, 1, interval_width_))]
  df_[, eval(paste0('probs_grp_midpoint', model_)) := (as.numeric(gsub('.*,|]', '', get(paste0('probs_grp', model_)))) - 
                                                         as.numeric(gsub(',.*|\\(', '', get(paste0('probs_grp', model_))))) / 2 + 
        as.numeric(gsub(',.*|\\(', '', get(paste0('probs_grp', model_))))]
  df_[, eval(paste0('probs_grp_mean', model_)) := mean(get(paste0('probs', model_))), by = eval(paste0('probs_grp', model_))]
  df_[, eval(paste0('actual_T_win', model_)) := sum(winner_side == 'Terrorist') / .N, by = eval(paste0('probs_grp', model_))]
  # add columns to show accuracy
  df_[get(paste0('probs', model_)) >= .5, eval(paste0('pred_winner', model_)) := 'Terrorist']
  df_[get(paste0('probs', model_)) < .5, eval(paste0('pred_winner', model_)) := 'CounterTerrorist']
  df_[, eval(paste0('correct', model_)) := get(paste0('pred_winner', model_)) == winner_side]
  return(df_)
}

LogLoss <- function(df_, pred_col_, actual_col_){
  ll <- df_[, (get(actual_col_) * log(get(pred_col_))) + ((1 - get(actual_col_)) * log(1 - get(pred_col_)))]
  return((-1 / df_[, .N]) * sum(ll))
}

RMSE <- function(predicted, actual, n){
  sqrt(sum((predicted - actual) ^ 2) / n)
}

###################################################################################################################

# PREP DATA

# read in data
df <- fread(paste0(data_path, 'processed_damage.csv'))

# relevel variables
df[, round_type := relevel(factor(round_type), ref = 'NORMAL')]
df[, map := relevel(factor(map), ref = 'de_dust2')]
df[, RoundState := relevel(factor(RoundState), ref = 'T4CT4')]
df[, BombLocation := relevel(factor(BombLocation), ref = 'DUST2 None')]
df[, BombPlant := relevel(factor(BombPlant), ref = 'FALSE')]

# split data - sample rounds, not rows
pct <- .6
rounds <- df[, unique(paste(file, round))]
samp <- sample.int(n = length(rounds), size = floor(pct * length(rounds)), replace = F)
samp1 <- samp[1: floor(length(samp) / 2)]
samp2 <- samp[(floor(length(samp) / 2) + 1): length(samp)]

train_df <- df[paste(file, round) %in% rounds[samp1]]
test_df <- df[paste(file, round) %in% rounds[samp2]]

rm(pct, samp, samp1, samp2, rounds, df)
gc()

###################################################################################################################

# TRAIN MODELS

# collect model names
models_LR <- c('LR1', 'LR2', 'LR3', 'LR4')
models <- models_LR

# logistic regression
# outcome = T wins round
# train a series of models to illustrate incremental changes in performance
LR1 <- glm(T_win ~ RoundState + T_HP_remaining + CT_HP_remaining, 
           data = train_df, family = 'binomial')
summary(LR1)
LR2 <- glm(T_win ~ t_eq_val + ct_eq_val + T_HP_remaining + CT_HP_remaining + seconds + round_type + RoundState + BombLocation, 
           data = train_df, family = 'binomial')
summary(LR2)
LR3 <- glm(T_win ~ Tval_minus_CTval + T_HP_minus_CT_HP + seconds + round_type + RoundState + BombLocation, 
           data = train_df, family = 'binomial')
summary(LR3)
LR4 <- glm(T_win ~ Tval_minus_CTval + T_HP_minus_CT_HP + CT_HP_remaining + seconds + round_type + RoundState + BombLocation + 
             RoundState * T_HP_minus_CT_HP, 
           data = train_df, family = 'binomial')
summary(LR4)

# logistic regression: predict on the test set and remove models from memory
for(mod in models_LR){
  test_df[, eval(paste0('probs', mod)) := predict(get(mod), newdata = test_df, type = 'response')]
  test_df <- CalibratedProbs(df_ = test_df, model_ = mod, interval_width_ = .04)
  rm(list = mod)
}
gc()

###################################################################################################################

# EVALUATE MODELS

# accuracy and log loss
for(mod in models){
  print(paste0('Model: ', mod))
  print(paste0('Accuracy: ', test_df[, sum(get(paste0('correct', mod)) == T) / .N]))
  print(paste0('Log Loss: ', LogLoss(df_ = test_df, pred_col_ = paste0('probs', mod), 
                                     actual_col_ = 'T_win')))
  print(paste0('RMSE: ', RMSE(test_df[, get(paste0('probs', mod))], test_df[, T_win], test_df[, .N])))
}

# plot observed share of T wins vs predicted probability of T win
plot_df <- data.table()
for(mod in models){
  plot_df <- rbind(plot_df, unique(test_df[, .(probs_grp_midpoint = get(paste0('probs_grp_midpoint', mod)), 
                                               model = mod, actual_T_win = get(paste0('actual_T_win', mod)))]))
}
ggplot(plot_df, aes(x = probs_grp_midpoint, y = actual_T_win, color = model)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Midpoint of predicted interval', y = 'Observed T win share')

plot_df <- data.table()
for(mod in models){
  plot_df <- rbind(plot_df, unique(test_df[, .(probs_grp_mean = get(paste0('probs_grp_mean', mod)), 
                                               model = mod, actual_T_win = get(paste0('actual_T_win', mod)))]))
}
ggplot(plot_df, aes(x = probs_grp_mean, y = actual_T_win, color = model)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Mean prediction', y = 'Observed T win share')

###################################################################################################################



