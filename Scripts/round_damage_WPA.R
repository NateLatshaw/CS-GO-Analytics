rm(list = ls())
gc()
library(data.table)
library(ggplot2)
set.seed(123)

data_path <- 'E:/CS-GO-Analytics/Processed Data/'
output_path <- 'E:/CS-GO-Analytics/Output/Win Probability Added/'
models_path <- 'E:/CS-GO-Analytics/Output/Models/'

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
pct <- .4
rounds <- df[, unique(paste(file, round))]
samp <- sample.int(n = length(rounds), size = floor(pct * length(rounds)), replace = F)

train_df <- df[paste(file, round) %in% rounds[samp]]
test_df <- df[!(paste(file, round) %in% rounds[samp])]

rm(pct, samp, rounds, df)
gc()

###################################################################################################################

# WIN PROBABILITY ADDED

formula <- T_win ~ t_eq_val + ct_eq_val + T_HP_remaining + CT_HP_remaining + seconds + round_type + RoundState + BombLocation

# fit model
mod <- glm(formula, data = train_df, family = 'binomial')

# predictions
test_df[, Twin_prob := predict(mod, type = 'response', newdata = test_df)]

# initial win probability before 1st kill: use all coefficients except RoundState, seconds, and HP remaining
sub_design_mat <- model.matrix(~ t_eq_val + ct_eq_val + round_type + BombLocation, data = test_df)
initial_coefs <- coef(mod)[!grepl('RoundState|seconds|HP_remaining', names(coef(mod)))]
test_df[, initial_Twin_prob := plogis(as.vector(matrix(initial_coefs, nrow = 1) %*% t(sub_design_mat)))]
initial_probs_df <- test_df[first_round_damage == T]
initial_probs_df[, Twin_prob_previous := initial_Twin_prob]

# create covariates of previous state
setkey(test_df, file, round, tick, inRoundCounter)
test_df[vic_side == 'Terrorist', lag_T_HP_remaining := hp_dmg + T_HP_remaining]
test_df[vic_side == 'Terrorist', lag_CT_HP_remaining := CT_HP_remaining]
test_df[vic_side == 'CounterTerrorist', lag_T_HP_remaining := T_HP_remaining]
test_df[vic_side == 'CounterTerrorist', lag_CT_HP_remaining := hp_dmg + CT_HP_remaining]
test_df[, lag_seconds := seconds - 1]
#test_df[, .(file, round, att_side, vic_side, hp_dmg, T_HP_remaining, lag_T_HP_remaining, CT_HP_remaining, lag_CT_HP_remaining)]

# calculate probabilities for previous state
test_df <- copy(test_df[first_round_damage == F])
test_df[, `:=`(tmp_T_HP = T_HP_remaining, tmp_CT_HP = CT_HP_remaining, tmp_seconds = seconds)]
test_df[, `:=`(T_HP_remaining = lag_T_HP_remaining, CT_HP_remaining = lag_CT_HP_remaining, seconds = lag_seconds, 
               tmp_RoundState = RoundState)]
test_df[, RoundState := relevel(factor(RoundState_previous), ref = 'T4CT4')]
test_df[, Twin_prob_previous := predict(mod, type = 'response', newdata = test_df)]
test_df[, `:=`(RoundState = NULL, T_HP_remaining = NULL, CT_HP_remaining = NULL, seconds = NULL)]
test_df[, `:=`(T_HP_remaining = tmp_T_HP, CT_HP_remaining = tmp_CT_HP, seconds = tmp_seconds, RoundState = tmp_RoundState)]
test_df[, `:=`(lag_T_HP_remaining = NULL, lag_CT_HP_remaining = NULL, lag_seconds = NULL,  
               tmp_T_HP = NULL, tmp_CT_HP = NULL, tmp_seconds = NULL, tmp_RoundState = NULL)]
test_df <- rbind(test_df, initial_probs_df)
stopifnot(test_df[is.na(Twin_prob_previous), .N] == 0)

# compute WPA
test_df[att_side == 'Terrorist', WPA := Twin_prob - Twin_prob_previous]
test_df[att_side == 'CounterTerrorist', WPA := (1 - Twin_prob) - (1 - Twin_prob_previous)]
stopifnot(test_df[att_side != 'None' & is.na(WPA), .N] == 0)

###################################################################################################################

# WPA boxplot by RoundState

plot_df <- test_df[att_side != 'None' & att_side != vic_side, .(RoundState = as.character(RoundState), WPA)][order(RoundState)]

png(paste0(output_path, 'WPA_by_RoundState_damage_model.png'))
ggplot(plot_df, aes(x = RoundState, y = WPA)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(breaks = seq(-1, 1, .1)) + 
  labs(x = '\nRound state at the end of the damage event', y = 'WPA\n') + 
  geom_abline(slope = 0, intercept = 0, color = 'red', linetype = 'dashed') + 
  geom_abline(slope = 0, intercept = test_df[att_side != 'None', quantile(WPA, probs = .9)], color = 'blue', linetype = 'dashed')
dev.off()

###################################################################################################################

# save data

#fwrite(df, paste0(data_path, 'damage_with_wpa.csv'))

