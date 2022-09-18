# DASHBOARD 2018 PE DEPUTADOS FEDERAIS

# VOTES DATASET LINK


# MAP DATASET LINK
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html


setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/map_codes/map_federal_parliamentarians_pe")

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(forcats)
library(RColorBrewer) # esquema de cores



# ELECTIONS DATASET
df_partidos_2018 <- read.table("../../../general_election_data/votacao_partido_munzona_2018_PE.csv", header=TRUE, sep=";")
df_candidatos_2018 <- read.table("../../../general_election_data/votacao_candidato_munzona_2018_PE.csv", header=TRUE, sep=";")


# Loading map file
MAPAPE=st_read("../../../maps/2018_PE_26MUE250GC_SIR.shp")

# SEPARAR PARTIDOS POR CORES
coresPartidos <- c("PT"="red",
                   "PSDB"="cornflowerblue",
                   "PSB"="gold",
                   "PSC"="cyan",
                   "PP"="burlywood4",
                   "DEM"="dodgerblue3",
                   "PSD"="chartreuse3",
                   "PC do B"="firebrick1",
                   "PSOL"="orange3",
                   "PR"="blue",
                   "SOLIDARIEDADE"="chocolate2",
                   "PPS"="mediumvioletred",
                   "PDT"="tomato4",
                   "PRTB"="olivedrab1",
                   "PROS"="darkorange",
                   "AVANTE"="cadetblue3",
                   "REDE"="mediumspringgreen",
                   "MDB"="yellow3",
                   "PTB"="forestgreen",
                   "PSL"="darkblue",
                   "PATRI"="olivedrab",
                   "PRB"="mediumblue",
                   "PRP"="orange3",
                   "PHS"="maroon",
                   "PV"="green4",
                   "PSTU"="tomato4",
                   "PCB"="red4",
                   "PODE"="palegreen",
                   "DC"="royalblue",
                   "PMN"="orchid4",
                   "PPL"="salmon4"
                  )

# REMOVE FERNANDO DE NORONHA's insular territory line
# MAPAPE_sem_fn <- MAPAPE[ !(MAPAPE$NM_MUN == "Fernando de Noronha"),  ]

options(scipen=999)



# adjust letter case
# ADD COLUMN WITH MUNICIPALITY NAME TITLE CASE
df_candidatos_2018$NM_MUN_TITLE_c <- str_to_title(df_candidatos_2018$NM_MUNICIPIO)

# adjust the name of the municipalities
df_candidatos_2018$NM_MUN_TITLE_c <- gsub(" E ", " e ", df_candidatos_2018$NM_MUN_TITLE_c)
df_candidatos_2018$NM_MUN_TITLE_c <- gsub(" De ", " de ", df_candidatos_2018$NM_MUN_TITLE_c)
df_candidatos_2018$NM_MUN_TITLE_c <- gsub(" Da ", " da ", df_candidatos_2018$NM_MUN_TITLE_c)
df_candidatos_2018$NM_MUN_TITLE_c <- gsub(" Do ", " do ", df_candidatos_2018$NM_MUN_TITLE_c)
df_candidatos_2018$NM_MUN_TITLE_c <- gsub(" Dos ", " dos ", df_candidatos_2018$NM_MUN_TITLE_c)
df_candidatos_2018$NM_MUN_TITLE_c <- gsub(" Das ", " das ", df_candidatos_2018$NM_MUN_TITLE_c)

# ADD COLUMN WITH MUNICIPALITY NAME TITLE CASE
df_candidatos_2018$NM_URNA_CANDIDATO <- str_to_title(df_candidatos_2018$NM_URNA_CANDIDATO)

# adjust the name of the municipalities
df_candidatos_2018$NM_URNA_CANDIDATO <- gsub(" E ", " e ", df_candidatos_2018$NM_URNA_CANDIDATO)
df_candidatos_2018$NM_URNA_CANDIDATO <- gsub(" De ", " de ", df_candidatos_2018$NM_URNA_CANDIDATO)
df_candidatos_2018$NM_URNA_CANDIDATO <- gsub(" Da ", " da ", df_candidatos_2018$NM_URNA_CANDIDATO)
df_candidatos_2018$NM_URNA_CANDIDATO <- gsub(" Do ", " do ", df_candidatos_2018$NM_URNA_CANDIDATO)
df_candidatos_2018$NM_URNA_CANDIDATO <- gsub(" Dos ", " dos ", df_candidatos_2018$NM_URNA_CANDIDATO)
df_candidatos_2018$NM_URNA_CANDIDATO <- gsub(" Das ", " das ", df_candidatos_2018$NM_URNA_CANDIDATO)





# TOTAL VOTES <- legenda + nominais
df_partidos_2018$TOTAL_VOTOS <- df_partidos_2018$QT_VOTOS_LEGENDA + df_partidos_2018$QT_VOTOS_NOMINAIS

# FILTER CARGO
df_federais <- filter(df_partidos_2018, DS_CARGO=="Deputado Federal")
df_federais_candidatos <- filter(df_candidatos_2018, DS_CARGO== "Deputado Federal" & DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP") )




# GROUP BY CANDIDATES
df_federais_eleitos <- df_federais_candidatos %>%
  group_by(NM_URNA_CANDIDATO,SG_PARTIDO, DS_SIT_TOT_TURNO) %>%
  summarise(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS))


# reorder df
df_federais_eleitos <- df_federais_eleitos[order(df_federais_eleitos$QT_VOTOS_NOMINAIS, decreasing = TRUE), ]


# PLOT elected candidates
df_federais_eleitos %>%
  ggplot(aes(x= QT_VOTOS_NOMINAIS, y= fct_reorder(NM_URNA_CANDIDATO, QT_VOTOS_NOMINAIS), fill= SG_PARTIDO)) +
  geom_col() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(colour = "grey92"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust=1, angle=0.0),
    legend.position = "none"
  ) +
  geom_text(aes(label=SG_PARTIDO), hjust= -.5, angle=0, size=3) +
  scale_fill_manual(values = coresPartidos) +
  lims(x= c(0, 500000))





# GROUP ELECTION DATA
df_2018_federais <- df_federais %>%
  group_by(SG_PARTIDO, NM_MUN_TITLE_c) %>%
  summarise(TOTAL_VOTOS = sum(TOTAL_VOTOS))






# TRANSFORM: Add proportion of votes in a party to total votes

# new df with total votes by municipality
total_votos_mun <- df_2018_federais %>%
  group_by(NM_MUN_TITLE_c) %>%
  summarise(TOTAL_VOTOS = sum(TOTAL_VOTOS))

colnames(df_2018_federais) <- c("SG_PARTIDO", "NM_MUN_TITLE_c", "VOTOS_PARTIDO")
# str(df_2018_estaduais)

# NEW COLUMN to show total votes by municipality
df_2018_federais$TOTAL_VOTOS <- total_votos_mun$TOTAL_VOTOS[ match(df_2018_federais$NM_MUN_TITLE_c, total_votos_mun$NM_MUN_TITLE_c)]

# add PROPORTION
df_2018_federais$PROPORCAO <- df_2018_federais$VOTOS_PARTIDO / df_2018_federais$TOTAL_VOTOS

# add PERCENTAGE
df_2018_federais$PERCENT <- df_2018_federais$PROPORCAO *100

# FORMAT PERCENTAGE
df_2018_federais$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_2018_federais$PERCENT), "%")

# set only 1 dataframe with election data and geographic data
df_2018_federais$GEOMETRY <- MAPAPE$geometry[ match(df_2018_federais$NM_MUN_TITLE_c, MAPAPE$NM_MUNICIP) ]






# PLOT by party
df_2018_federais %>%
  filter(SG_PARTIDO=="PT") %>%
  ggplot() +
  geom_sf(aes( geometry=GEOMETRY, fill=PERCENT)) +
  theme(
    plot.title = element_text(hjust=.5, vjust=7),
    plot.subtitle = element_text(hjust = .5, vjust=7),
    panel.background = element_rect(fill="white"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.title = element_blank()
  ) +
  scale_fill_distiller(type="seq", palette= "Reds", direction=1, limits=c(0, 100)) +
  labs(title= "Distribuição de votos para Deputados FEDERAIS em Pernambuco na eleição de 2018",
       subtitle= "Porcentagem de votos no PT (em relação ao total de votos) por município")


ggsave("PE_2018_deputado_FEDERAL_porcentagem_PT_municipios.jpg")









