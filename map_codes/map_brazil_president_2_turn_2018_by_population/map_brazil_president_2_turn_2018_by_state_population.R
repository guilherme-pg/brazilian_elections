# BRAZIL 2018 presidential election 2 turn by state population



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


# TO IMPROVE: USE libraries individually
library(dplyr)
library(ggplot2)
library(stringr)
library(sf)

library(RColorBrewer)
library(colorspace)
library(cartogram)


# LOADING DATASETS
# VOTES
votos_br <- read.table("../../../general_election_data/brazil_2018_president_2_turn.csv", header=TRUE, sep=",")

# BR MAP
MAPA_BR <- st_read("../../../maps/2018_state_BRUFE250GC_SIR.shp")




votos_pr_2 <- votos_br %>%
  group_by(SG_UF, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))


# GROUP VOTES BY STATE to check the total votes
total_votos_state <- votos_pr_2 %>%
  group_by(SG_UF) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY state
votos_pr_2$TOTAL_VOTOS <- total_votos_state$QT_VOTOS[ match(votos_pr_2$SG_UF, total_votos_state$SG_UF)  ]

# ADD PROPORTION
votos_pr_2$PROPORCAO <- votos_pr_2$QT_VOTOS/ votos_pr_2$TOTAL_VOTOS

# ADD PERCENT
votos_pr_2$PERCENT <- votos_pr_2$PROPORCAO *100

# FORMAT PERCENT
votos_pr_2$PERCENT_FORMAT <- paste0(sprintf("%4.2f", votos_pr_2$PERCENT), "%")


# TO IMPROVE: add var with abbreviation instead change names
# MAPA_BR$SG_UF <- c()

MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "ACRE" ] <- "AC"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "ALAGOAS" ] <- "AL"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "AMAPÁ" ] <- "AP"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "AMAZONAS" ] <- "AM"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "BAHIA" ] <- "BA"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "CEARÁ" ] <- "CE"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "DISTRITO FEDERAL" ] <- "DF"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "ESPÍRITO SANTO" ] <- "ES"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "GOIÁS" ] <- "GO"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "MARANHÃO" ] <- "MA"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "MATO GROSSO" ] <- "MT"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "MATO GROSSO DO SUL" ] <- "MS"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "MINAS GERAIS" ] <- "MG"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "PARÁ" ] <- "PA"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "PARAÍBA" ] <- "PB"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "PARANÁ" ] <- "PR"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "PERNAMBUCO" ] <- "PE"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "PIAUÍ" ] <- "PI"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "RIO DE JANEIRO" ] <- "RJ"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "RIO GRANDE DO NORTE" ] <- "RN"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "RIO GRANDE DO SUL" ] <- "RS"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "RONDÔNIA" ] <- "RO"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "RORAIMA" ] <- "RR"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "SANTA CATARINA" ] <- "SC"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "SÃO PAULO" ] <- "SP"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "SERGIPE" ] <- "SE"
MAPA_BR$NM_ESTADO[ MAPA_BR$NM_ESTADO == "TOCANTINS" ] <- "TO"


MAPA_BR <- rename(MAPA_BR, "SG_UF"="NM_ESTADO")


# REORDER BY ALPHABETIC ORDER STATES NAMES
MAPA_BR <- MAPA_BR[order(MAPA_BR$SG_UF),]


# FILTER BY VALID VOTES
df_votos_validos <- filter(votos_pr_2, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")

df_votos_validos <- select(df_votos_validos, SG_UF, NM_VOTAVEL, QT_VOTOS)

# GROUP TOTAL VOTES BY state
total_votos_val <- df_votos_validos %>%
  group_by(SG_UF) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))



# MATCH counties 
df_votos_validos$TOTAL_VOTOS <- total_votos_val$QT_VOTOS[ match(df_votos_validos$SG_UF, total_votos_val$SG_UF) ]

# ADD PROPORTION
df_votos_validos$PROPORCAO <- df_votos_validos$QT_VOTOS / df_votos_validos$TOTAL_VOTOS

# ADD PERCENT
df_votos_validos$PERCENT <- df_votos_validos$PROPORCAO *100

# FORMAT PERCENT
df_votos_validos$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_votos_validos$PERCENT), "%")




# DISTINGUISH THE WINNER OF EACH MUNICIPALTY BY THE MAJORITY OF THE VALID VOTES
votos_validos <- filter(df_votos_validos, PERCENT > 50)


# TRANSFORM THE VALUES OF A VARIABLE (INVERTING) TO DISTINGUISH BY OPPOSITE COLORS
# APPLY THE TRANSFORMATION BY COLUMN ACCORDING CONDITION

# difference -100 to 100, where 0 is the middle
votos_validos <- votos_validos %>%
  mutate(PERCENT_100100 = case_when(NM_VOTAVEL=="JAIR MESSIAS BOLSONARO" ~ PERCENT*-1, NM_VOTAVEL=="FERNANDO HADDAD" ~ PERCENT))

# difference 0 to 100, where 50 is the middle
votos_validos <- votos_validos %>%
  mutate(PERCENT_0100 = abs(case_when(NM_VOTAVEL=="JAIR MESSIAS BOLSONARO" ~ PERCENT-100, NM_VOTAVEL=="FERNANDO HADDAD" ~ PERCENT)))



# exclude votos estrangeiros
votos_validos <- votos_validos[!(votos_validos$SG_UF == 'ZZ'),  ]


# merge data
votos_states <- merge(MAPA_BR, votos_validos, all=TRUE)



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



# reprojecting maps
votos_validos_trans <- st_transform(votos_states, 5880)

votos_validos_cart <- votos_validos_trans %>%
  cartogram_dorling(weight = 'TOTAL_VOTOS')


# centroids for labels
centroids <- st_centroid(votos_validos_cart$geometry)

# latitude and longitude dataframe
pontos <- data.frame(st_coordinates(centroids))

# add column with states abbreviation
pontos$SG_UF <- votos_validos_cart$SG_UF




# PLOT THE MAIN STATE
votos_validos_cart %>%
  ggplot() +
  geom_sf(aes(geometry=geometry, fill=PERCENT_0100), color=NA) +
  geom_text(pontos, mapping=aes(x=X, y=Y, 
                                label=SG_UF), 
            color="grey28", size=2) +
  theme_void() +
  theme(
    panel.border = element_rect(colour = "lightgoldenrodyellow", fill=NA),
    plot.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    plot.margin = margin(t=10, l=100, r=100, b=10),
    plot.title = element_text(hjust=.5, colour = "black"),
    plot.subtitle = element_text(hjust=.5, colour= "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(colour = "black"),
    legend.key.width = unit(1.7, "cm"),
    legend.background = element_blank()
  ) +
  labs(
    title = "Votos Válidos no 2º Turno das Eleições para Presidente de 2018",
    subtitle = "Desempenho dos candidados por Estado em porcentagem") +
  scale_fill_gradientn(colors=coloresBlueRed,
                        limits=c(0, 100),
                        breaks=c(0, 25, 50, 75, 100),
                        labels=c("100%", "Jair Bolsonaro", 
                                 "50%", "Fernando Haddad", "100%"
                                 )) +
  guides(fill = guide_colourbar(ticks = FALSE, 
                                  title.position = "bottom")
  )


# save last plot
ggsave("map_president_2_turn_2018_br_by_state_population_V3.jpg")



