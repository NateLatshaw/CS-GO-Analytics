rm(list = ls())
gc()
library(data.table)
library(zoo)

data_path <- 'E:/CS-GO-Analytics/Raw Data/'
output_path <- 'E:/CS-GO-Analytics/Processed Data/'

# read in round-level data

rounds <- rbind(fread(paste0(data_path, 'esea_meta_demos.part1.csv'), na.strings = ''), 
                fread(paste0(data_path, 'esea_meta_demos.part2.csv'), na.strings = ''))

# read in damage-level data

damage <- rbind(fread(paste0(data_path, 'esea_master_dmg_demos.part1.csv'), integer64 = 'numeric', na.strings = ''), 
                fread(paste0(data_path, 'esea_master_dmg_demos.part2.csv'), integer64 = 'numeric', na.strings = ''))

damage[, uniqueN(round), by = file][, quantile(V1, seq(0, 1, .1))]
damage[, uniqueN(file)] # 14,927
damage[, uniqueN(paste(file, round))] # 380,967
damage[, .N] # 10,538,182

# read in kill-level data
# seconds = number of seconds into round, not seconds of game

kills <- rbind(fread(paste0(data_path, 'esea_master_kills_demos.part1.csv'), na.strings = ''), 
               fread(paste0(data_path, 'esea_master_kills_demos.part2.csv'), na.strings = ''))

# read in map coordinate scaling data

maps <- fread(paste0(data_path, 'map_data.csv'))
setnames(maps, 'V1', 'map')
maps[, map_name := gsub('de_', '', map)]
substr(maps$map_name, 1, 1) <- toupper(substr(maps$map_name, 1, 1))
maps[map_name == 'Dust2', map_name := 'Dust II']
maps[map_name == 'Cbble', map_name := 'Cobblestone']

###################################################################################################################

# kills data: convert seconds to number of seconds in game
# fix missing rounds by inferring times from others where possible
# remove rounds where kills do not have start seconds

setkey(rounds, file, round)
setkey(kills, file, round)
kills[rounds, start_seconds := i.start_seconds]
kills[rounds, end_seconds := i.end_seconds]

tmp <- unique(kills[file %in% kills[is.na(start_seconds), file], .(file, round, start_seconds, end_seconds)])
tmp[, keep := is.na(start_seconds)]
setkey(tmp, file, round)
tmp[, tmp_round := shift(round), by = file]
tmp[, tmp_start := shift(end_seconds), by = file]
tmp[is.na(start_seconds) & (round - tmp_round == 1), start_seconds := tmp_start]
tmp <- tmp[keep == T]
setkey(kills, file, round)
kills[tmp, start_seconds := i.start_seconds]

kills[, seconds := seconds + start_seconds]
kills <- kills[!(paste(file, round) %in% kills[is.na(start_seconds), paste(file, round)])] 

# kills data: remove records where kills cannot be properly merged onto damage df

kills <- kills[!paste(file, round) %in% 
                 kills[, .N, .(file, round, tick, att_side)][N > 1, paste(file, round)]]

###################################################################################################################

# damage data: remove rounds not in kills data

damage <- damage[paste(file, round) %in% kills[, paste(file, round)]]

# damage data: flag kills

kills[, true := T]
setkey(damage, file, round, tick, att_side)
setkey(kills, file, round, tick, att_side)

damage[kills, isKill := i.true]
damage[is.na(isKill), isKill := F]

# sometimes multiple people damage someone at the same time of a kill
# here we assign the kill to whoever does the most damage

damage <- damage[order(file, round, tick, att_side, -hp_dmg, att_id)]

damage[, tmp := 1:.N, by = .(file, round, tick, att_side)]
damage[tmp != 1, isKill := F]
damage[, tmp := NULL]

# confirm that the number of kills in each round matches between kills and damage dfs
# remove rounds that do not align

tmp <- damage[, .(DamageKills = sum(isKill)), by = .(file, round)]
tmp2 <- kills[, .(Kills = .N), by = .(file, round)]
tmp <- merge(tmp, tmp2, by = c('file', 'round'), all.x = T)
tmp <- tmp[Kills != DamageKills, paste(file, round)]

damage[, tmp_col := paste(file, round)]
damage <- damage[!(tmp_col %in% tmp)]
damage[, tmp_col := NULL]
kills <- kills[!(paste(file, round) %in% tmp)]
rm(tmp2)
stopifnot(damage[, sum(isKill)] == kills[, .N])

# damage data: identify number of people alive on each team

setkey(damage, file, round, tick)
setkey(kills, file, round, tick)
damage[kills, t_alive := i.t_alive]
damage[kills, ct_alive := i.ct_alive]

damage[, idx := 1:.N, by = .(file, round)]
damage[idx == 1 & is.na(t_alive), t_alive := 5]
damage[idx == 1 & is.na(ct_alive), ct_alive := 5]
damage[, idx := NULL]
damage[, t_alive := na.locf(t_alive), by = .(file, round)]
damage[, ct_alive := na.locf(ct_alive), by = .(file, round)]

# damage data: remove rounds with a negative number of people remaining

damage <- damage[!(paste(file, round) %in% damage[t_alive < 0, paste(file, round)])]
damage <- damage[!(paste(file, round) %in% damage[ct_alive < 0, paste(file, round)])]

# damage data: merge on round-level information
# remove rounds not found in round data

damage <- damage[paste(file, round) %in% rounds[, paste(file, round)]]

setkey(damage, file, round)
setkey(rounds, file, round)

damage[rounds, map := i.map]
damage[rounds, winner_team := i.winner_team]
damage[rounds, winner_side := i.winner_side]
damage[rounds, round_type := i.round_type]
damage[rounds, ct_eq_val := i.ct_eq_val]
damage[rounds, t_eq_val := i.t_eq_val]

# damage data: remove rounds without any winner

damage <- damage[(!paste(file, round) %in% damage[winner_side == 'None', paste(file, round)])]

# damage data: scale player location coordinates

setkey(damage, map)
setkey(maps, map)

damage[maps, EndX := i.EndX]
damage[maps, EndY := i.EndY]
damage[maps, ResX := i.ResX]
damage[maps, ResY := i.ResY]
damage[maps, StartX := i.StartX]
damage[maps, StartY := i.StartY]
damage[maps, map_name := map_name]

damage[, att_pos_x := (ResX * (att_pos_x - StartX)) / (EndX - StartX)]
damage[, att_pos_y := (ResY * (att_pos_y - StartY)) / (EndY - StartY)]
damage[, vic_pos_x := (ResX * (vic_pos_x - StartX)) / (EndX - StartX)]
damage[, vic_pos_y := (ResY * (vic_pos_y - StartY)) / (EndY - StartY)]

damage[, `:=`(EndX = NULL, EndY = NULL, ResX = NULL, ResY = NULL, StartX = NULL, StartY = NULL)]

###################################################################################################################

# save damage file

fwrite(damage, paste0(output_path, 'processed_damage.csv'))
