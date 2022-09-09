# BRAZILIAN SWING STATES (MG, RJ and SP) ELECTION 2014, turn 2 for president


# 2014 presidential vote database
#  https://dadosabertos.tse.jus.br/dataset/resultados-2014/resource/28aad28a-e658-4e80-ae4f-30bebbcf6bbd

# maps dataset organizacao_do_territorio > malhas_territoriais > malhas_municipais >
# municipios_2014 > UFs > MG, RJ and SP > mg_municipios, rj_municipios and sp_municipios
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html


# SET FOLDER
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/brazilian_swing_states")

library(dplyr)
library(ggplot2)
library(stringr)
library(sf)


# LOADING DATASETS

# VOTES
votos_2014 <- read.table("../general_election_data/votacao_secao_2014_BR.csv", header=TRUE, sep=";")

# MAPS
MAPA_MG <- st_read("../maps/2014_MG_31MUE250GC_SIR.shp")
MAPA_RJ <- st_read("../maps/2014_RJ_33MUE250GC_SIR.shp")
MAPA_SP <- st_read("../maps/2014_SP_35MUE250GC_SIR.shp")


# adding column with states
MAPA_MG$SG_UF <- "MG"
MAPA_RJ$SG_UF <- "RJ"
MAPA_SP$SG_UF <- "SP"

# change county column name
MAPA_MG <- rename(MAPA_MG, "NM_MUNICIPIO"="NM_MUNICIP")
MAPA_RJ <- rename(MAPA_RJ, "NM_MUNICIPIO"="NM_MUNICIP")
MAPA_SP <- rename(MAPA_SP, "NM_MUNICIPIO"="NM_MUNICIP")


#FILTER BY ELECTIONS 2 TURN
votos_mg <- filter(votos_2014, NR_TURNO==2 & SG_UF =="MG")
votos_rj <- filter(votos_2014, NR_TURNO==2 & SG_UF =="RJ")
votos_sp <- filter(votos_2014, NR_TURNO==2 & SG_UF =="SP")

votos_2014_pr_2_t <- rbind(votos_mg, votos_rj, votos_sp)

votos_pr_2 <- votos_2014_pr_2_t %>%
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
votos_pr_2$NM_MUNICIPIO <- gsub(" D O", " D'O", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("PAU D ALHO", "PAU-D'ALHO", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("SÃO LUÍS DO PARAITINGA", "SÃO LUIZ DO PARAITINGA", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub(" D ", "-D'", votos_pr_2$NM_MUNICIPIO)
votos_pr_2$NM_MUNICIPIO <- gsub("SEM PEIXE", "SEM-PEIXE", votos_pr_2$NM_MUNICIPIO)

MAPA_SP$NM_MUNICIPIO <- gsub("SÃO JOÃO DO PAU D'ALHO", "SÃO JOÃO DO PAU-D'ALHO", MAPA_SP$NM_MUNICIPIO)
MAPA_SP$NM_MUNICIPIO <- gsub("FLORÍNIA", "FLORÍNEA", MAPA_SP$NM_MUNICIPIO)
MAPA_SP$NM_MUNICIPIO <- gsub("ITAÓCA", "ITAOCA", MAPA_SP$NM_MUNICIPIO)
MAPA_SP$NM_MUNICIPIO <- gsub("SÃO LUÍS DO PARAITINGA", "SÃO LUIZ DO PARAITINGA", MAPA_SP$NM_MUNICIPIO)
MAPA_SP$NM_MUNICIPIO <- gsub("BIRITIBA-MIRIM", "BIRITIBA MIRIM", MAPA_SP$NM_MUNICIPIO)

MAPA_MG$NM_MUNICIPIO <- gsub("-D'Á", "-D'Á", MAPA_MG$NM_MUNICIPIO)
MAPA_MG$NM_MUNICIPIO <- gsub("PASSA-VINTE", "PASSA VINTE", MAPA_MG$NM_MUNICIPIO)
MAPA_MG$NM_MUNICIPIO <- gsub("PINGO D'ÁGUA", "PINGO-D'ÁGUA", MAPA_MG$NM_MUNICIPIO)



# MERGE DATAS
votos_mun_mg <- merge(votos_pr_2, MAPA_MG)
votos_mun_rj <- merge(votos_pr_2, MAPA_RJ)
votos_mun_sp <- merge(votos_pr_2, MAPA_SP)

votos_estados <- rbind(votos_mun_mg, votos_mun_rj, votos_mun_sp)

# FILTER BY VALID VOTES
df_votos_validos <- filter(votos_estados, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")

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
df_votos_validos$SG_UF <- votos_2014_pr_2_t$SG_UF[ match( df_votos_validos$CD_MUNICIPIO,  votos_2014_pr_2_t$CD_MUNICIPIO) ]

# DISTINGUISH THE WINNER OF EACH MUNICIPALTY BY THE MAJORITY OF THE VALID VOTES
votos_validos <- filter(df_votos_validos, PERCENT > 50)


# TRANSFORM THE VALUES OF A VARIABLE (INVERTING) TO DISTINGUISH BY OPPOSITE COLORS
# APPLY THE TRANSFORMATION BY COLUMN ACCORDING CONDITION
votos_validos <- votos_validos %>%
  mutate(PERCENT_TRANSF = abs(case_when(NM_VOTAVEL=="AÉCIO NEVES DA CUNHA" ~ PERCENT-100, NM_VOTAVEL=="DILMA VANA ROUSSEFF" ~ PERCENT)))



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
    title = "Votos Válidos no 2º Turno das Eleições para Presidente de 2014 em MG, RJ e SP",
    subtitle = "Desempenho dos candidados por município em porcentagem") +
  scale_fill_gradientn( colors= coloresBlueRed, 
                        limits=c(0, 100),
                        breaks=c(0, 25, 50, 75, 100),
                        labels=c("100%", "Aécio Neves", "50%", "Dilma Rousseff", "100%")
  ) +
  guides(fill = guide_colourbar(ticks = FALSE, 
                                title.position = "bottom")
  )



# save plot
ggsave("2014_map_MG_RJ_SP_PRESIDENT_2_turn.jpg", width = 9, height = 7)
