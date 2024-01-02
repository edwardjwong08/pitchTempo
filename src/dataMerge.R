library(tidyverse)

df <- read_csv("pbp1315new.csv")
df1 <- read_csv("pbp1618new.csv")

df3 <- full_join(df, df1, by = "game_pk" )

write.csv(df3, "G:/Shared drives/SABER_Talk/oldData/pbp1318adjusted.csv", row.names=T)

df <- read_csv("pbp1921new.csv")
#fix this value
#df1 <- read_csv("pbp2223new.csv")
df1 <- read_csv("pbp1318adjusted.csv")
df3 <- full_join(df, df1, by = "game_pk" )

write.csv(df3, "G:/Shared drives/SABER_Talk/oldData/pbp1321adjusted.csv", row.names=T)

df <- read_csv("pbp1321adjusted.csv")
df1 <- read_csv("pbp1012.csv")
df3 <- full_join(df, df1, by = "game_pk" )

write.csv(df3, "G:/Shared drives/SABER_Talk/oldData/pbp1021adjusted.csv", row.names=T)
