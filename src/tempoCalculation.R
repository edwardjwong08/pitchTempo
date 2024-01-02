library(tidyverse)
library(data.table)
library(splitstackshape)
library(baseballr)

setwd("G:/Shared drives/SABER_Talk/oldData")
#df <- fread('pbp1012.csv')

df2 <- mlb_pbp(567109)
df1 <- mlb_pbp(566701)
df <- rbind(df2, df1, fill=T)
df <- read_csv("pbp1023.csv")
#clear nonpitches out
df$details.code[is.na(df$details.code)] <- "remove"
df <- df %>% separate(about.startTime, into = c("startTimeD", "startTimeT"), sep = "T")
#problems with end time
df <- df %>% separate(about.endTime, into = c("endTimeD", "endTimeT"), sep = "T")
df <- df %>% separate(endTimeT, into = c("endTimeT", "drop"), sep = "Z")

df <- df %>% separate(startTimeT, into = c("startTimeT", "drop1"), sep = "Z")
df <- mutate(df, startTime1 = paste(startTimeD,startTimeT, sep = " "))
df <- mutate(df, endTime1 = paste(endTimeD,endTimeT, sep = " "))

df1 <- df %>% filter((details.code %in% c("*B", "PSO", "VP", "AC", "remove")))
df <- df %>% filter(!(details.code %in% c("*B", "PSO", "VP", "AC", "remove")))


df1$details.code <- str_replace(df1$details.code, "\\bB\\b", "4")
df1$details.code <- gsub('\\bPSO\\b', '5', df1$details.code)
df1$details.code <- gsub('\\bVP\\b', 'Z', df1$details.code)
df1$details.code <- gsub('\\bAC\\b', '6', df1$details.code)
df1$details.code <- gsub('\\bremove\\b', 'U', df1$details.code)

df <- rbind(df, df1)

df <- df %>% group_by(game_pk) %>% arrange(startTime1)

#leadPK <- lead(df$game_pk, n=1)
#leadBatter <- lead(df$matchup.batter.id, n=1)
#leadIndex <- lead(df$index, n = 1)
#leadStartTime <- lead(df$startTime, n = 1)

df <- data.frame(df)
for (i in seq_len(nrow(df))){
  print(i)
  df <- try(df %>% mutate(tempo = difftime(lead(df$startTime1), df$startTime1)))
  if(inherits(df, "try-error")){
    next
  }
  
}
df <- df %>% relocate(tempo, .before = drop1)

#fix, change code differences
df$tempo <- ifelse(grepl("K|1|2|4|D|E|H|PSO|remove|VP|W|X|5|6|Z|U", df$details.code), 0, df$tempo)
#df$tempo <- ifelse(lag(grepl("K|1|2|4|D|E|H|PSO|remove|VP|W|X|5|6|Z|U", df$details.code)) & lag(df$matchup.batter.id) == df$matchup.batter.id, 0, df$tempo)
#df$tempo <- ifelse(lag(df$matchup.batter.id) != df$matchup.batter.id, 0, df$tempo)
df$tempo <- ifelse((lead(df$pitchNumber)-1) != df$pitchNumber, 0, df$tempo)
df$tempo <- ifelse(lead(df$game_pk) != df$game_pk, 0, df$tempo)
df$tempo <- ifelse(df$isPitch == F, 0, df$tempo)
df$season <- substr(df$game_date, 1, 4)
df$countFull <-  paste0(df$count.balls.start, "-", df$count.strikes.start)

sched <- map_df(.x = seq.Date(as.Date('2022-01-01'), 
                              as.Date('2023-11-01'), 
                              'day'), 
                ~get_game_pks_mlb(date = .x, 
                                  level_ids = 1)
)
series <- sched %>% select(c(game_pk, seriesDescription))
series <- as.data.frame(series)
df <- merge(pbp2223new, series, by = "game_pk", all.x=T)
#tmp <- filter(series, game_pk == 566701)
#tmp1 <- filter(series, game_pk == 567109)
#df <- merge(df, tmp, by="game_pk", all.x = T)
#df <- merge(df, tmp1, by="game_pk", all.x = T)
df$seriesDescription <- df$seriesDescription.x
df3 <- df
df3 <- subset(df3, select = -c(drop1, drop, isPitch, type, details.event, details.ballColor, player.link, details.fromCatcher, 
                               hitData.hardness, actionPlayId, details.eventType,position.type, battingOrder, result.type, result.eventType,
                               result.description, about.startTime, about.endTime, about.isComplete, about.captivatingIndex, count.balls.end, count.strikes.end,
                               matchup.batter.link, matchup.pitchHand.description, batted.ball.result, home_level_id, home_level_name, home_parentOrg_name,
                               home_parentOrg_id, home_league_id, away_level_id, away_level_name, away_parentOrg_name, away_parentOrg_id, away_league_id,
                               batting_team, fielding_team, last.pitch.of.ab, details.trailColor, injuryType, umpire.link, details.isOut, details.disengagementNum,
                               isSubstitution, replacedPlayer.id, replacedPlayer.link, result.isOut, matchup.postOnSecond.id, matchup.postOnSecond.fullName, 
                               matchup.postOnSecond.link, matchup.postOnFirst.id, matchup.postOnFirst.fullName, matchup.postOnFirst.link,
                               matchup.postOnThird.id, matchup.postOnThird.fullName, matchup.postOnThird.link, reviewDetails.isOverturned, reviewDetails.inProgress,
                               reviewDetails.reviewType, reviewDetails.challengeTeamId, startTime, endTime))

