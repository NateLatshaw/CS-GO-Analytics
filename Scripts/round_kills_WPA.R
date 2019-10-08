rm(list = ls())
gc()
library(data.table)
library(ggplot2)
set.seed(123)

data_path <- 'E:/CS-GO-Analytics/Processed Data/'
output_path <- 'E:/CS-GO-Analytics/Output/Win Probability Added/'

###################################################################################################################

# PREP DATA

# read in data
df <- fread(paste0(data_path, 'processed_damage.csv'))

# subset data on kills
df <- df[isKill == T]

# relevel variables
df[, round_type := relevel(factor(round_type), ref = 'NORMAL')]
df[, map := relevel(factor(map), ref = 'de_dust2')]
df[, RoundState := relevel(factor(RoundState), ref = 'T4CT4')]
df[, BombLocation := relevel(factor(BombLocation), ref = 'DUST2 None')]
df[, BombPlant := relevel(factor(BombPlant), ref = 'FALSE')]

###################################################################################################################

# WIN PROBABILITY ADDED

formula <- T_win ~ t_eq_val + ct_eq_val + seconds + round_type + RoundState + BombLocation

# fit model
mod <- glm(formula, data = df, family = 'binomial')

# predictions
df[, Twin_prob := predict(mod, type = 'response')]

# initial win probability before 1st kill: use all coefficients except RoundState and seconds
sub_design_mat <- model.matrix(~ t_eq_val + ct_eq_val + round_type + BombLocation, data = df)
initial_coefs <- coef(mod)[!grepl('RoundState|seconds', names(coef(mod)))]
df[, initial_Twin_prob := plogis(as.vector(matrix(initial_coefs, nrow = 1) %*% t(sub_design_mat)))]

# calculate probabilities for previous state
initial_probs_df <- df[RoundState %in% c('T4CT5', 'T5CT4')]
initial_probs_df[, Twin_prob_previous := .5]
df <- copy(df[RoundState_previous != 'T5CT5'])
df[, tmp := RoundState]
df[, RoundState := relevel(factor(RoundState_previous), ref = 'T4CT4')]
df[, Twin_prob_previous := predict(mod, type = 'response', newdata = df)]
df[, RoundState := NULL]
df[, RoundState := tmp]
df[, tmp := NULL]
df <- rbind(df, initial_probs_df)
stopifnot(df[is.na(Twin_prob_previous), .N] == 0)

# adjust previous probabilities for the first kill of each round
df[RoundState %in% c('T4CT5', 'T5CT4'), Twin_prob_previous := initial_Twin_prob]

# compute WPA
df[att_side == 'Terrorist', WPA := Twin_prob - Twin_prob_previous]
df[att_side == 'CounterTerrorist', WPA := (1 - Twin_prob) - (1 - Twin_prob_previous)]
stopifnot(df[is.na(WPA), .N] == 0)

###################################################################################################################

# WPA boxplot by RoundState

plot_df <- df[, .(RoundState = as.character(RoundState), WPA)][order(RoundState)]

png(paste0(output_path, 'WPA_by_RoundState_kills_model.png'))
ggplot(plot_df, aes(x = RoundState, y = WPA)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(breaks = seq(-1, 1, .1)) + 
  labs(x = '\nRound state at the end of the kill', y = 'WPA\n') + 
  geom_abline(slope = 0, intercept = 0, color = 'red', linetype = 'dashed') + 
  geom_abline(slope = 0, intercept = df[, quantile(WPA, probs = .9)], color = 'blue', linetype = 'dashed')
dev.off()

###################################################################################################################

# save data

fwrite(df, paste0(data_path, 'kills_with_wpa.csv'))

