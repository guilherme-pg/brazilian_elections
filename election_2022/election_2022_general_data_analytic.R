# GENERAL 2022 ELECTION DATA DIVISION

# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/election_2022")

library(dplyr)
library(ggplot2)



# VOTES

votos_pe <- read.table("../../general_election_data/votacao_secao_2022_PE.csv", header=TRUE, sep=";")


d_estaduais <- votos_pe %>%
  filter(DS_CARGO=="DEPUTADO ESTADUAL") %>%
  select(DS_CARGO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, NM_VOTAVEL, QT_VOTOS)

d_federais <- votos_pe %>%
  filter(DS_CARGO=="DEPUTADO FEDERAL") %>%
  select(DS_CARGO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, NM_VOTAVEL, QT_VOTOS)

senador <- votos_pe %>%
  filter(DS_CARGO=="SENADOR") %>%
  select(DS_CARGO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, NM_VOTAVEL, QT_VOTOS)

governador <- votos_pe %>%
  filter(DS_CARGO=="GOVERNADOR") %>%
  select(DS_CARGO, NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, NM_VOTAVEL, QT_VOTOS)


write.csv(d_estaduais, "../../general_election_data/2022_election_pe_deputados_estaduais.csv", row.names = FALSE)
write.csv(d_federais, "../../general_election_data/2022_election_pe_deputados_federais.csv", row.names = FALSE)
write.csv(senador, "../../general_election_data/2022_election_pe_senador.csv", row.names = FALSE)
write.csv(governador, "../../general_election_data/2022_election_pe_governador.csv", row.names = FALSE)



# VERIFY
votos_pe <- read.table("../../general_election_data/2022_election_pe_governador.csv", header=TRUE, sep=",")



# VOTER PROFILE

voter_profile_pe <- read.table("../../general_election_data/perfil_eleitor_secao_2022_PE.csv", header=TRUE, sep=";")



voter_by_schooling <- voter_profile_pe %>%
  group_by(DS_GRAU_ESCOLARIDADE, DS_GENERO, DS_FAIXA_ETARIA, NM_MUNICIPIO) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))



voter_by_schooling %>%
  group_by(DS_GRAU_ESCOLARIDADE, DS_GENERO) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL)) %>%
  ggplot() +
  geom_col(aes(x=DS_GRAU_ESCOLARIDADE, y=QT_ELEITORES_PERFIL, fill=DS_GENERO), position = 'dodge') +
  theme(
    axis.text.x = element_text(hjust=1, angle = 45, size=8),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) +
  labs(y="Votos")
  








