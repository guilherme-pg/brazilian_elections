# BRAZIL 2018 presidential election 2 turn by municipality population



# DATASET of 2018 elections
# https://dadosabertos.tse.jus.br/dataset/resultados-2014/resource/9df2487a-7d41-4e1f-8ca1-a9dbe43fdd02

# DATASET of 2018 elections
# https://dadosabertos.tse.jus.br/dataset/resultados-2010/resource/24996eb7-2b7c-4ef6-a427-9d9229bc1b5c


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/map_codes/map_brazil_president_2_turn_2018_by_population")



#FILTER BY ELECTIONS 2 TURN
#votos_2018 <- read.table("../../../general_election_data/votacao_secao_2018_BR.csv", sep=";", header=TRUE)
#votos_presidente = filter(votos_2018, NR_TURNO==2)
#rm(votos_2018)
#write.csv(votos_presidente, "brazil_2018_president_2_turn.csv", row.names = FALSE)


library(dplyr)
library(ggplot2)
library(stringr)
library(sf)


# LOADING DATASETS
# VOTES
votos_br <- read.table("../../../general_election_data/brazil_2018_president_2_turn.csv", header=TRUE, sep=",")

# BR MAP
MAPA_BR <- st_read("../../../maps/2018_BRMUE250GC_SIR.shp")






votos_pr_2 <- votos_br %>%
  group_by(CD_MUNICIPIO, NM_VOTAVEL, NM_MUNICIPIO, SG_UF) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))



# GROUP VOTES BY MUNICIPALITY to check the total votes
total_votos_mun <- votos_pr_2 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
votos_pr_2$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(votos_pr_2$CD_MUNICIPIO, total_votos_mun$CD_MUNICIPIO)  ]

# ADD PROPORTION
votos_pr_2$PROPORCAO <- votos_pr_2$QT_VOTOS/ votos_pr_2$TOTAL_VOTOS

# ADD PERCENT
votos_pr_2$PERCENT <- votos_pr_2$PROPORCAO *100

# FORMAT PERCENT
votos_pr_2$PERCENT_FORMAT <- paste0(sprintf("%4.2f", votos_pr_2$PERCENT), "%")


# MUNICIPALITIES NAMES CORRECTIONS
votos_pr_2$NM_MUNICIPIO <- gsub("-D'", " D'", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("CAEM", "CAÉM", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("QUINJINGUE", "QUIJINGUE", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("SANTO ESTEVÃO", "SANTO ESTÊVÃO", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("LUIS CORREIA", "LUÍS CORREIA", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub(" D ", " D'", votos_pr_2$NM_MUNICIPIO)

MAPA_BR$NM_MUNICIP <- gsub("CAMACAN", "CAMACÃ", MAPA_BR$NM_MUNICIP)
MAPA_BR$NM_MUNICIP <- gsub("AÇU", "ASSÚ", MAPA_BR$NM_MUNICIP)
MAPA_BR$NM_MUNICIP <- gsub("IPANGUASSÚ", "IPANGUAÇU", MAPA_BR$NM_MUNICIP)
MAPA_BR$NM_MUNICIP <- gsub("JANUÁRIO CICCO", "BOA SAÚDE", MAPA_BR$NM_MUNICIP)
MAPA_BR$NM_MUNICIP <- gsub("AUGUSTO SEVERO", "CAMPO GRANDE", MAPA_BR$NM_MUNICIP)
MAPA_BR$NM_MUNICIP <- gsub("GRACHO CARDOSO", "GRACCHO CARDOSO", MAPA_BR$NM_MUNICIP)


MAPA_BR <- rename(MAPA_BR, "NM_MUNICIPIO"="NM_MUNICIP")


votos_mun <- merge(votos_pr_2, MAPA_BR, all=TRUE)


# CHECK NA VALUES in rows
rows_with_na <- subset(votos_mun, is.na(votos_mun$CD_GEOCMU))

# require: votos em cidades estrangeiras.


# FILTER BY VALID VOTES
df_votos_validos <- filter(votos_mun, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")

df_votos_validos <- select(df_votos_validos, CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL, QT_VOTOS, geometry)

# GROUP TOTAL VOTES BY MUNICIPALITY
total_votos_val <- df_votos_validos %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))



# MATCH counties 
df_votos_validos$TOTAL_VOTOS <- total_votos_val$QT_VOTOS[ match(df_votos_validos$CD_MUNICIPIO, total_votos_val$CD_MUNICIPIO) ]

# ADD PROPORTION
df_votos_validos$PROPORCAO <- df_votos_validos$QT_VOTOS / df_votos_validos$TOTAL_VOTOS

# ADD PERCENT
df_votos_validos$PERCENT <- df_votos_validos$PROPORCAO *100

# FORMAT PERCENT
df_votos_validos$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_votos_validos$PERCENT), "%")

# ADD UF'S
df_votos_validos$SG_UF <- votos_NE$SG_UF[ match( df_votos_validos$CD_MUNICIPIO,  votos_NE$CD_MUNICIPIO) ]

# DISTINGUISH THE WINNER OF EACH MUNICIPALTY BY THE MAJORITY OF THE VALID VOTES
votos_validos <- filter(df_votos_validos, PERCENT > 50)


# TRANSFORM THE VALUES OF A VARIABLE (INVERTING) TO DISTINGUISH BY OPPOSITE COLORS
# APPLY THE TRANSFORMATION BY COLUMN ACCORDING CONDITION
votos_validos <- votos_validos %>%
  mutate(PERCENT_TRANSF = abs(case_when(NM_VOTAVEL=="JAIR MESSIAS BOLSONARO" ~ PERCENT-100, NM_VOTAVEL=="FERNANDO HADDAD" ~ PERCENT)))



# DISTINGUISH AND IDENTIFY BY COLOR THE GRADES OF THE CANDIDATES PERCENTS
coloresBlueRed <- c(
  "#003467",
  "#0c539c",
  "#1176e0",
  "#4392e9",
  "#82c1ff",
  "grey91",
  "#ff8282",
  "#e94343",
  "#e01111",
  "#9c0c0c",
  "#6a0000"
)
breaks <- c(0, 20, 30, 40, 50, 60, 70, 80, 100)





# PLOT THE PROPORTIONS OF EACH MUNICIPALY BY CANDIDATE 
# PLOT THE MAIN STATE
votos_validos %>%
  ggplot() +
  geom_sf(aes(geometry=geometry, fill=PERCENT_TRANSF), 
          color=NA) +
  theme_void() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    plot.background = element_rect(fill="black", color=NA),
    plot.margin = margin(t=10, l=100, r=100, b=10),
    plot.title = element_text(hjust=.5, colour = "white"),
    plot.subtitle = element_text(hjust=.5, colour= "white"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(colour = "white"),
    legend.key.width = unit(2, "cm"),
    legend.background = element_blank(),
  ) +
  labs(
    title = "Votos Válidos no 2º Turno das Eleições para Presidente de 2018 no Nordeste",
    subtitle = "Desempenho dos candidados por município em porcentagem") +
  scale_fill_gradientn( colors= coloresBlueRed, 
                        limits=c(0, 100),
                        breaks=c(0, 25, 50, 75, 100),
                        labels=c("100%", "Jair Bolsonaro", "50%", "Fernando Haddad", "100%")
  ) +
  guides(fill = guide_colourbar(ticks = FALSE, 
                                title.position = "bottom")
  )





ggsave("map_president_2_turn_2018_br_v1.jpg")






