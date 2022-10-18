# Proportional stacked area chart PE-BARZIL state deputies


setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/proportional_stacked_area_chart_codes")

library(dplyr)
library(ggplot2)




PARTIDOS <- c("PHS", "PSB", "PR",
                    "PSB", "PT", "PSB",
                    "PSDB", "PSDB", "PDT",
                    "PSDB", "PSDB", "PTB",
                    "PV", "PSB", "PSDB",
                    "PTC", "PTB", "PTB",
                    "PDT", "PMDB", "PR ",
                    "PTB", "PT", "PSB",
                    "PTB", "PSB", "PSB",
                    "PCdoB", "PTB", "PT",
                    "PHS", "DEM", "PSB",
                    "PSC", "PDT", "PSB",
                    "PSB", "PMN", "PSB",
                    "PTC", "PRP", "PTC",
                    "PR ", "PT", "PTB",
                    "PT", "DEM", "PSB", "PSB"
                    )
df_2010 <- data.frame(PARTIDOS)

df_2010$ANO <- "2010"

PARTIDOS <- c("PP", "PSB", "PSB",
                   "PMDB", "PSB", "PDT",
                   "PTB", "PSD", "PSD",
                   "PR ", "PT", "PSB",
                   "PSB", "PSB", "PT",
                   "PSB", "PSB", "PTC",
                   "PSB", "PR ", "PRB",
                   "PSB", "PSB", "PSDB",
                   "PSB", "DEM", "PTB",
                   "PSB", "PTB", "PSB",
                   "PR ", "PSB", "PMDB",
                   "PTB", "PTB", "PSL",
                   "PDT", "PMDB", "PT",
                   "PTB", "PSOL", "PHS",
                   "PP", "PRP", "PP",
                   "SD ", "SD ", "PP", "PROS"
                    )
df_2014 <- data.frame(PARTIDOS)

df_2014$ANO <- "2014"

PARTIDOS <- c("PSB", "PP", "PSC",
                   "PT", "PSD", "PSB",
                   "PSB", "PSB", "PSB",
                   "PSB", "PSD", "PSC",
                   "PSC", "PSB",
                   "PSB", "DEM", "MDB",
                   "PRB", "PP", "PP",
                   "DEM", "PSDB", "DEM",
                   "SD ", "PP", "PR ",
                   "PSOL", "PSB", "PTB",
                   "PP", "PP", "PP",
                   "PR ", "PSB", "PTB",
                   "PDT", "PT", "PCdoB",
                   "PP", "PP", "PP",
                   "PSC", "PRTB", "PSD",
                   "PSC", "AVANTE", "PSB", "PT", "PHS"
                    )
df_2018 <- data.frame(PARTIDOS)

df_2018$ANO <- "2018"

PARTIDOS <- c("PP", "PL", "PSB", "PSB",
                   "UB", "PSB", "PT",
                   "PV", "UB", "PSB",
                   "SD ", "PT", "PSB",
                   "UB", "SD ", "PSB",
                   "Republicanos", "PP", "PP",
                   "PSB", "PSB", "PP",
                   "PP", "PSDB", "PP",
                   "SD ", "Republicanos", "PV",
                   "UB", "PL", "PSDB",
                   "PP", "PSB", "PSB",
                   "PSB", "PP", "PSB",
                   "PL", "PSB", "PSB",
                   "PT", "PCdoB", "PSOL",
                   "PL", "UB", "PV",
                   "Patriota", "PSDB", "PL"
                    )
df_2022 <- data.frame(PARTIDOS)

df_2022$ANO <- "2022"




df_2010_2022 <- rbind(df_2010, df_2014, df_2018, df_2022)



df_2010_2022$QUANTIDADE <- 1



df_2010_2014_2018_2022 <- df_2010_2022 %>%
  group_by(ANO, PARTIDOS) %>%
  summarise(QUANTIDADE = sum(QUANTIDADE))






# PARTIES EVOLUTION 
df_2010_2014_2018_2022$PARTIDOS <- gsub("DEM", "UB", df_2010_2014_2018_2022$PARTIDOS)
df_2010_2014_2018_2022$PARTIDOS <- gsub("PSL", "UB", df_2010_2014_2018_2022$PARTIDOS)
df_2010_2014_2018_2022$PARTIDOS <- gsub("UB", "União Brasil", df_2010_2014_2018_2022$PARTIDOS)
df_2010_2014_2018_2022$PARTIDOS <- gsub("SD ", "Solidariedade", df_2010_2014_2018_2022$PARTIDOS)
df_2010_2014_2018_2022$PARTIDOS <- gsub("PRB", "Republicanos", df_2010_2014_2018_2022$PARTIDOS)
df_2010_2014_2018_2022$PARTIDOS <- gsub("PR ", "PL", df_2010_2014_2018_2022$PARTIDOS)
df_2010_2014_2018_2022$PARTIDOS <- gsub("PMDB", "MDB", df_2010_2014_2018_2022$PARTIDOS)





# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
df_2010_2014_2018_2022$TOTAL_ELEITOS <- 49

# ADD PROPORTION
df_2010_2014_2018_2022$PROPORCAO <- df_2010_2014_2018_2022$QUANTIDADE/ df_2010_2014_2018_2022$TOTAL_ELEITOS

# ADD PERCENT
df_2010_2014_2018_2022$PERCENT <- df_2010_2014_2018_2022$PROPORCAO *100

# FORMAT PERCENT
df_2010_2014_2018_2022$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_2010_2014_2018_2022$PERCENT), "%")





write.csv2(df_2010_2014_2018_2022, "2010_2014_2018_2022_pe_deputados_estaduais.csv", row.names = FALSE)





partidos_colores <- c(
  "PMN"="tomato4",
  "PCdoB"="red4",
  "PSOL"="orange",
  "PV"="green",
  "PT"="red",
  "PDT"="firebrick",
  "PSB"="gold",
  "Solidariedade"="darkorange4",
  "MDB"="grey",
  "PSD"="aquamarine1",
  "PSDB"="royalblue",
  "AVANTE"="brown1",
  "PRP"="slategray1",
  "PROS"="tan1",
  "PP"="tan4",
  "PTC"="lightskyblue1",
  "PRTB"="chartreuse4",
  "Patriota"="darkolivegreen1",
  "PHS"="cadetblue2",
  "PSC"="cyan",
  "União Brasil"="deepskyblue",
  "PL"="darkblue",
  "PTB"="darkslategrey",
  "Republicanos"="mediumblue"
)



df_2010_2014_2018_2022$PARTIDOS <- factor(df_2010_2014_2018_2022$PARTIDOS,
  levels=c(
    "PMN", "PSOL", "PCdoB", "PT", "PV", "PDT", "PSB", "Solidariedade",
    "MDB", "PSD", "PSDB", "AVANTE", "PRP", "PROS", "PP", "PTC",
    "PRTB", "Patriota", "PHS", "PSC", "União Brasil", "PL", "PTB", "Republicanos"
  )
)


df_2010_2014_2018_2022$ANO <- as.Date(df_2010_2014_2018_2022$ANO, format="01-01-%Y")




df_2010_2014_2018_2022 %>%
  ggplot(aes(x=ANO, y=PROPORCAO, fill=PARTIDOS)) +
  geom_area() +
  scale_fill_manual(values = partidos_colores)




