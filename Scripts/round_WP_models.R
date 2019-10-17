rm(list = ls())
gc()
library(data.table)
library(ggplot2)
library(zoo)
set.seed(123)

data_path <- 'E:/CS-GO-Analytics/Processed Data/'
wp_plots_path <- 'E:/CS-GO-Analytics/Output/Win Probability Plots/'
calibration_plots_path <- 'E:/CS-GO-Analytics/Output/Model Calibration Plots/'

###################################################################################################################
# DEFINE FUNCTIONS

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

PlotDamageWP <- function(df_, file_, round_){
  # create plot df
  plot_df <- df[file == file_ & round == round_, .(seconds, Twin_prob = probsDamageModel, winner_side, round_start_seconds)]
  plot_df <- merge(plot_df, data.table(seconds = seq(plot_df[, unique(round_start_seconds)], plot_df[, max(seconds)], by = .001)), by = 'seconds', all.y = T)
  plot_df[, idx := 1:.N]
  plot_df[idx == 1, Twin_prob := .5]
  plot_df[, idx := NULL]
  plot_df[, Twin_prob := na.locf(Twin_prob)]
  plot_df[, CTwin_prob := 1 - Twin_prob]
  plot_df[, round_start_seconds := NULL]
  # ensure win probability ends at 1 for the winning side
  if(plot_df[winner_side == 'Terrorist', .N] > 0){
    plot_df <- rbind(plot_df, data.table(seconds = plot_df[, max(seconds) + .1], Twin_prob = 1, CTwin_prob = 0, winner_side = 'Terrorist'))
  } else {
    plot_df <- rbind(plot_df, data.table(seconds = plot_df[, max(seconds) + .1], Twin_prob = 0, CTwin_prob = 1, winner_side = 'CounterTerrorist'))
  }
  plot_df[, winner_side := NULL]
  plot_df <- melt(plot_df, id.vars = 'seconds', variable.name = 'Side', value.name = 'prob')
  plot_df[Side == 'Twin_prob', Side := 'T']
  plot_df[Side == 'CTwin_prob', Side := 'CT']
  plot_df[, Side := relevel(factor(Side), ref = 'CT')]
  plot_df <- plot_df[order(seconds, -Side)]
  # generate plot
  p <- ggplot(plot_df, aes(x = seconds, y = prob, color = Side)) + 
    geom_line(size = 1.5, alpha = .6) + 
    scale_color_manual(values = c('#0000FF', '#FF0000')) + 
    labs(x = 'Time in round', y = 'Win probability\n', title = paste0('Match: ', file_), subtitle = paste0('Round: ', round_)) + 
    theme_bw() + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank(), 
          text = element_text(size = 16), legend.position = 'bottom') + 
    scale_y_continuous(breaks = seq(0, 1, .1)) + 
    geom_abline(slope = 0, intercept = 0) + 
    geom_abline(slope = 0, intercept = 1) + 
    geom_abline(slope = 0, intercept = .5, linetype = 2)
  return(p)
}

PlotCalibration <- function(df_, models_){
  # loop through models to create calibration metrics and plot
  # plot observed share of T wins vs predicted probability of T win
  plot_df <- data.table()
  for(mod in models_){
    df_copy <- copy(df_)
    if(grepl('kills', tolower(mod))){
      df_copy <- df_copy[isKill == T]
    }
    df_copy <- CalibratedProbs(df_ = df_copy, model_ = mod, interval_width_ = .04)
    plot_df <- rbind(plot_df, unique(df_copy[, .(probs_grp_midpoint = get(paste0('probs_grp_midpoint', mod)), 
                                                 model = mod, actual_T_win = get(paste0('actual_T_win', mod)))]))
    print('#################################')
    print(paste0('Model: ', mod))
    print(paste0('Accuracy: ', df_copy[, sum(get(paste0('correct', mod)) == T) / .N]))
    print(paste0('Log Loss: ', LogLoss(df_ = df_copy, pred_col_ = paste0('probs', mod), actual_col_ = 'T_win')))
    print(paste0('RMSE: ', RMSE(df_copy[, get(paste0('probs', mod))], df_copy[, T_win], df_copy[, .N])))
  }
  plot_df[model == 'KillsModel', model := 'Kills Model']
  plot_df[model == 'DamageModel', model := 'Damage Model']
  p <- ggplot(plot_df, aes(x = probs_grp_midpoint, y = actual_T_win, color = model)) + 
    geom_point(size = 5, alpha = .6) + 
    scale_color_manual(values = c('#FF0000', '#0000FF')) + 
    geom_abline(slope = 1, intercept = 0) + 
    labs(x = '\nMidpoint of predicted interval', y = 'Observed T win share\n') + 
    theme_bw() + 
    theme(legend.title = element_blank(), text = element_text(size = 16), plot.title = element_text(hjust = 0.5), 
          legend.position = 'bottom')
  return(p)
}

###################################################################################################################
# READ IN AND PREP DATA

# read in data
df <- fread(paste0(data_path, 'processed_damage.csv'))

# relevel variables
df[, round_type := relevel(factor(round_type), ref = 'NORMAL')]
df[, map := relevel(factor(map), ref = 'de_dust2')]
df[, RoundState := relevel(factor(RoundState), ref = 'T4CT4')]
df[, BombLocation := relevel(factor(BombLocation), ref = 'DUST2 None')]
df[, BombPlant := relevel(factor(BombPlant), ref = 'FALSE')]

# set up folds - sample rounds, not rows
n_folds <- 2
rounds <- unique(df[, .(file, round)])
rounds[, fold := sample(1:n_folds, size = rounds[, .N], replace = T)]
setkey(df, file, round)
setkey(rounds, file, round)
df[rounds, fold := i.fold]

###################################################################################################################
# TRAIN MODELS

kills_formula <- T_win ~ t_eq_val + ct_eq_val + seconds + round_type + RoundState + BombLocation
damage_formula <- T_win ~ t_eq_val + ct_eq_val + T_HP_remaining + CT_HP_remaining + seconds + round_type + RoundState + BombLocation

for(i in 1:n_folds){
  print(paste0('Fold: ', i))
  # kills model
  KillsModel <- glm(kills_formula, data = df[isKill == T & fold != i], family = 'binomial')
  print('kills model trained')
  df[isKill == T & fold == i, probsKillsModel := predict(KillsModel, newdata = df[isKill == T & fold == i], type = 'response')]
  rm(KillsModel)
  # damage model
  DamageModel <- glm(damage_formula, data = df[fold != i], family = 'binomial')
  print('damage model trained')
  df[fold == i, probsDamageModel := predict(DamageModel, newdata = df[fold == i], type = 'response')]
  rm(DamageModel)
}
df[, fold := NULL]

###################################################################################################################
# TEMP

# save file
fwrite(df, paste0(data_path, 'processed_damage_with_wp.csv'))
# load file
df <- fread(paste0(data_path, 'processed_damage_with_wp.csv'))

###################################################################################################################
# PLOTS

# PLOT WP
png(paste0(wp_plots_path, 'damage_wp_plot.png'), height = 480, width = 600)
PlotDamageWP(df_ = df, file_ = 'esea_match_13785855.dem', round_ = 15)
dev.off()

# PLOT MODEL CALIBRATION
png(paste0(calibration_plots_path, 'model_calibration.png'), height = 480, width = 600)
PlotCalibration(df_ = df, models_ = c('KillsModel', 'DamageModel'))
dev.off()

###################################################################################################################


