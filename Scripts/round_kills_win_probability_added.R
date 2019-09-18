rm(list = ls())
gc()
library(data.table)
library(ggplot2)
library(ranger)
set.seed(123)

data_path <- 'E:/CS-GO-Analytics/Processed Data/'

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

# subset data on kills
df <- df[isKill == T]

###################################################################################################################

# WIN PROBABILITY ADDED

# fit model
mod <- glm(T_win ~ t_eq_val + ct_eq_val + seconds + round_type + RoundState + BombLocation, 
           data = df, family = 'binomial')

# predictions
df[, Twin_prob := predict(mod, type = 'response')]

# initial probability of winning for T5CT5, calculate by map
initial_probs <- unique(df[, .(file, round, winner_side, map_name)])
initial_probs <- initial_probs[, .(T_win_share = sum(winner_side == 'Terrorist') / .N, N = .N), map_name]
initial_probs_df <- copy(df[RoundState_previous == 'T5CT5'])
setkey(initial_probs, map_name)
setkey(initial_probs_df, map_name)
initial_probs_df[initial_probs, Twin_prob_previous := i.T_win_share]

# prior win probability for T5CT5 with bomb planted, calculate across all maps due to lack of data
plant_first_probs <- unique(df[RoundState %in% c('T4CT5', 'T5CT4') & !grepl('None', BombLocation), 
                                     .(file, round, winner_side, map_name, RoundState)])
plant_first_probs <- plant_first_probs[, .(T_win_share = sum(winner_side == 'Terrorist') / .N, N = .N)]

# calculate probabilities for previous state
df <- copy(df[RoundState_previous != 'T5CT5'])
df[, tmp := RoundState]
df[, RoundState := relevel(factor(RoundState_previous), ref = 'T4CT4')]
df[, Twin_prob_previous := predict(mod, type = 'response', newdata = df)]
df[, RoundState := NULL]
df[, RoundState := tmp]
df[, tmp := NULL]
df <- rbind(df, initial_probs_df)
stopifnot(df[is.na(Twin_prob_previous), .N] == 0)

# adjust previous probabilities when the bomb is planted before a kill
df[RoundState %in% c('T4CT5', 'T5CT4') & !grepl('None', BombLocation), Twin_prob_previous := plant_first_probs$T_win_share]

# compute WPA
df[att_side == 'Terrorist', WPA := Twin_prob - Twin_prob_previous]
df[att_side == 'CounterTerrorist', WPA := (1 - Twin_prob) - (1 - Twin_prob_previous)]
stopifnot(df[is.na(WPA), .N] == 0)
stopifnot(df[WPA < 0, .N] == 0)

###################################################################################################################

# save data

fwrite(df, paste0(data_path, 'kills_with_wpa.csv'))

