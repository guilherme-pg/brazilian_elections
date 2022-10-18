# CORRELATION VOTES PRESIDENT X GOVERNADOR IN DECISIVE BRAZILIAN STATES





# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/scatter")


library(dplyr)
library(ggplot2)


options(scipen=999)





# general data
VOTES_MG <- read.table("../../general_election_data/votacao_secao_2022_MG.csv", header=TRUE, sep=";")

VOTES_BR <- read.table("../../general_election_data/votacao_secao_2022_BR.csv", header=TRUE, sep=";")



president_mg <- filter(VOTES_BR, SG_UF=="MG")

president_mg_by_city <- president_mg %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- president_mg_by_city %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
president_mg_by_city$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(president_mg_by_city$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]

# ADD PROPORTION
president_mg_by_city$PROPORCAO <- president_mg_by_city$QT_VOTOS/ president_mg_by_city$TOTAL_VOTOS

# ADD PERCENT
president_mg_by_city$PERCENT <- president_mg_by_city$PROPORCAO *100

# FORMAT PERCENT
president_mg_by_city$PERCENT_FORMAT <- paste0(sprintf("%4.2f", president_mg_by_city$PERCENT), "%")

# select ONLY BOLSONARO VOTES BY CITY
bolsonaro_mg <- filter(president_mg_by_city, NM_VOTAVEL=="JAIR MESSIAS BOLSONARO")






governador_mg <- filter(VOTES_MG, DS_CARGO=="GOVERNADOR")

governador_mg_by_city <- governador_mg %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- governador_mg_by_city %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
governador_mg_by_city$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(governador_mg_by_city$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]

# ADD PROPORTION
governador_mg_by_city$PROPORCAO <- governador_mg_by_city$QT_VOTOS/ governador_mg_by_city$TOTAL_VOTOS

# ADD PERCENT
governador_mg_by_city$PERCENT <- governador_mg_by_city$PROPORCAO *100

# FORMAT PERCENT
governador_mg_by_city$PERCENT_FORMAT <- paste0(sprintf("%4.2f", governador_mg_by_city$PERCENT), "%")

# select ONLY BOLSONARO VOTES BY CITY
zema_mg <- filter(governador_mg_by_city, NM_VOTAVEL=="ROMEU ZEMA NETO")




colnames(zema_mg)[colnames(zema_mg) == "PERCENT"] <- "PERCENT_PR"
colnames(bolsonaro_mg)[colnames(bolsonaro_mg) == "PERCENT"] <- "PERCENT_PR"







GOV_X_PRE <- merge(bolsonaro_mg[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], zema_mg[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)



GOV_X_PRE %>%
  ggplot(aes(x=PERCENT_GOV, y=PERCENT_PR, size=TOTAL_VOTOS)) +
  geom_point(alpha=0.5) +
  geom_abline(slope = 1, intercept = 0, lwd=.5) +
  lims(x=c(0, 100), y=c(0, 100))



# PLOT 

# X = GOVERNADOR (ZEMA, KALIL, TARCISISO)
# Y = BOZONARO






