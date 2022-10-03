# elected federal deputies and total party votes



# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/bar_col_codes")



library(dplyr)
library(ggplot2)


options(scipen=999)



votos_br <- read.table("../../general_election_data/votacao_candidato_munzona_2018_PE.csv", header=TRUE, sep=";")
votos_pe_partido <- read.table("../../general_election_data/votacao_partido_munzona_2018_PE.csv", header=TRUE, sep=";")


votos_legenda <- votos_pe_partido %>%
  filter(DS_CARGO == "Deputado Federal") %>%
  group_by(SG_PARTIDO) %>%
  summarise(QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA))




df_federais_pe_2018 = filter(votos_br, DS_CARGO=="Deputado Federal")

df_federais_pe_2018 <- select(df_federais_pe_2018, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, DS_CARGO, NM_CANDIDATO, DS_SITUACAO_CANDIDATURA, SG_PARTIDO, NM_PARTIDO, DS_SIT_TOT_TURNO, QT_VOTOS_NOMINAIS)




# GROUP VOTES BY STATE to check the total votes
total_votos_state <- df_federais_pe_2018 %>%
  group_by(SG_UF) %>%
  summarise(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS))


# SUM PARTY VOTE
total_legenda <- sum(votos_legenda$QT_VOTOS_LEGENDA)
total_votos_state$QT_VOTOS_NOMINAIS <- total_votos_state$QT_VOTOS_NOMINAIS + total_legenda


# group by CANDIDATE
df_federais_by_candidate <- df_federais_pe_2018 %>%
  group_by(NM_CANDIDATO, SG_UF, SG_PARTIDO, DS_SIT_TOT_TURNO) %>%
  summarise(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS))
  


# ADD COLUMN WITH TOTAL VOTES BY state
df_federais_by_candidate$TOTAL_VOTOS <- total_votos_state$QT_VOTOS_NOMINAIS[ match(df_federais_by_candidate$SG_UF, total_votos_state$SG_UF)  ]

# ADD PROPORTION
df_federais_by_candidate$PROPORCAO <- df_federais_by_candidate$QT_VOTOS_NOMINAIS/ df_federais_by_candidate$TOTAL_VOTOS

# ADD PERCENT
df_federais_by_candidate$PERCENT <- df_federais_by_candidate$PROPORCAO *100

# FORMAT PERCENT
df_federais_by_candidate$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_federais_by_candidate$PERCENT), "%")


df_federais_by_candidate %>%
  filter(SG_PARTIDO=="PP") %>%
  ggplot(aes(x=NM_CANDIDATO, y=QT_VOTOS_NOMINAIS)) +
  geom_col()
















