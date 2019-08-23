rm(list = ls())
gc()
library(data.table)
library(ggplot2)
set.seed(123)

data_path <- 'E:/CS-GO-Analytics/Processed Data/'

CalibratedProbs <- function(df_, interval_width_){
  # add columns that show how well predicted probabilities match reality
  df_[, probs_grp := cut(probs, breaks = seq(0, 1, interval_width_))]
  df_[, probs_grp_midpoint := (as.numeric(gsub('.*,|]', '', probs_grp)) - as.numeric(gsub(',.*|\\(', '', probs_grp))) / 2 + 
        as.numeric(gsub(',.*|\\(', '', probs_grp))]
  df_[, probs_grp_mean := mean(probs), by = probs_grp]
  df_[, actual_T_win := sum(winner_side == 'Terrorist') / .N, by = probs_grp]
  # add columns to show accuracy
  df_[probs >= .5, pred_winner := 'Terrorist']
  df_[probs < .5, pred_winner := 'CounterTerrorist']
  return(df_)
}

###################################################################################################################

# read in data
df <- fread(paste0(data_path, 'processed_damage.csv'))

# subset data on kills
df <- df[isKill == T]

# create outcome
df[, T_win := winner_side == 'Terrorist']

# create state of game variables to indicate number of T and CT remaining
df[, RoundState := paste0('T', t_alive, 'CT', ct_alive)]

# create Tkill and CTkill flags
df[, Tkill := att_side == 'Terrorist']
df[, CTkill := att_side == 'CounterTerrorist']

# create bomb planted flag
df[, BombPlant := is_bomb_planted == TRUE]

###################################################################################################################

# split data - sample rounds, not rows
pct <- .6
rounds <- df[, unique(paste(file, round))]
samp <- sample.int(n = length(rounds), size = floor(pct * length(rounds)), replace = F)

train_df <- df[paste(file, round) %in% rounds[samp]]
test_df <- df[!(paste(file, round) %in% rounds[samp])]

rm(samp, rounds, df)
gc()

###################################################################################################################

# logistic regression
# outcome = T wins round
mod <- glm(T_win ~ Tkill + BombPlant + RoundState + Tkill * BombPlant + map + round_type, 
           data = train_df, family = 'binomial')
summary(mod)

# predict on the test set
test_df[, probs := predict(mod, newdata = test_df, type = 'response')]
test_df <- CalibratedProbs(test_df, .04)

# accuracy
test_df[, sum(pred_winner == winner_side) / .N]

# plot observed share of T wins vs predicted probability of T win
plot_df <- unique(test_df[, .(probs_grp_midpoint, actual_T_win)][order(probs_grp_midpoint)])
ggplot(plot_df, aes(x = probs_grp_midpoint, actual_T_win)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Midpoint of predicted interval', y = 'Observed T win share')

plot_df <- unique(test_df[, .(probs_grp_mean, actual_T_win)][order(probs_grp_mean)])
ggplot(plot_df, aes(x = probs_grp_mean, actual_T_win)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Mean prediction', y = 'Observed T win share')






