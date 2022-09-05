# votes distribution in PERNAMBUCO by candidate in 2 turn of 2014

setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/pe_distribuicao")

library(dplyr)
library(ggplot2)
library(sf)

# LOADING MAP
MAPA=st_read("26MUE250GC_SIR.shp")

# ELECTION DATASET
df_2014_pr <- read.table("../general_election_data/votacao_secao_2014_BR.csv", header=TRUE, sep=";")





# FILTER BY ELECTIONS 2 TURN
df_2014_pr_2_t <- filter(df_2014_pr, NR_TURNO==2 & SG_UF=="PE")

df_pr_2 <- df_2014_pr_2_t %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL, CD_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))



# ADEQUATE COLUMNS WITH MUNICIPALITY NAMES - TITLE CASE
library(stringr)
MAPA$NM_MUNICIP <- str_to_title(MAPA$NM_MUNICIP)

MAPA$NM_MUNICIP <- gsub(" E ", " e ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" De ", " de ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Da ", " da ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Do ", " do ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Dos ", " dos ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Das ", " das ", MAPA$NM_MUNICIP)

df_pr_2$NM_MUNICIPIO <- str_to_title(df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_VOTAVEL <- str_to_title(df_pr_2$NM_VOTAVEL)

df_pr_2$NM_MUNICIPIO <- gsub(" E ", " e ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" De ", " de ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Da ", " da ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Do ", " do ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Dos ", " dos ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Das ", " das ", df_pr_2$NM_MUNICIPIO)



# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- df_pr_2 %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
df_pr_2$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(df_pr_2$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]

# ADD PROPORTION
df_pr_2$PROPORCAO <- df_pr_2$QT_VOTOS/ df_pr_2$TOTAL_VOTOS

# ADD PERCENT
df_pr_2$PERCENT <- df_pr_2$PROPORCAO *100

# FORMAT PERCENT
df_pr_2$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_pr_2$PERCENT), "%")

# VETOR OF COLOR CLASSIFICATION BY DIFFERENCE OF WINNING CANDIDATE BY MUNICIPALITY
coresVotos <- c("Dilma Vana Rousseff"="red", 
                "Aécio Neves Da Cunha"="blue")

# ADD MUNICIPALY GEOMETRY
df_pr_2$GEOMETRY <- MAPA$geometry[ match(df_pr_2$NM_MUNICIPIO, MAPA$NM_MUNICIP) ]







# FILTER BY VALID VOTES
df_votos_validos <- filter(df_pr_2, NM_VOTAVEL != "Voto Nulo" & NM_VOTAVEL != "Voto Branco")

df_votos_validos <- select(df_votos_validos, NM_MUNICIPIO, NM_VOTAVEL, QT_VOTOS, GEOMETRY)

# GROUP TOTAL VOTES BY MUNICIPALITY
total_votos_val <- df_votos_validos %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# MATCH MUNICIPALITIES 
df_votos_validos$TOTAL_VOTOS <- total_votos_val$QT_VOTOS[ match(df_votos_validos$NM_MUNICIPIO, total_votos_val$NM_MUNICIPIO) ]

# ADD PROPORTION
df_votos_validos$PROPORCAO <- df_votos_validos$QT_VOTOS / df_votos_validos$TOTAL_VOTOS

# ADD PERCENT
df_votos_validos$PERCENT <- df_votos_validos$PROPORCAO *100

# FORMAT PERCENT
df_votos_validos$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_votos_validos$PERCENT), "%")

# DISTINGUISH THE WINNER OF EACH MUNICIPALTY BY THE MAJORITY OF THE VALID VOTES
votos_validos <- filter(df_votos_validos, PERCENT > 50)




# DISTINGUISH AND IDENTIFY BY COLOR THE GRADES OF THE CANDIDATES PERCENTS
coresProporcap <- c(
  "#000082",
  "#2020a1",
  "#3f3fc0",
  "#5f5fdf",
  "#7e7efe",
  "grey91",
  "#fe7f7f",
  "#df5f5f",
  "#c04040",
  "#a12020",
  "#820000"
)
breaks <- c(0, 20, 30, 40, 50, 60, 70, 80, 100)




# TRANSFORM THE VALUES OF A VARIABLE (INVERTING) TO DISTINGUISH BY OPPOSITE COLORS
# APPLY THE TRANSFORMATION BY COLUMN ACCORDING CONDITION
votos_validos <- votos_validos %>%
  mutate(PERCENT_TRANSF = abs(case_when(NM_VOTAVEL=="Aécio Neves Da Cunha" ~ PERCENT-100, NM_VOTAVEL=="Dilma Vana Rousseff" ~ PERCENT)))




# PLOT THE PROPORTIONS OF EACH MUNICIPALY BY CANDIDATE 

# PLOT THE MAIN STATE
main_pe <- votos_validos %>%
  filter(NM_MUNICIPIO != "Fernando de Noronha") %>%
  ggplot() +
  geom_sf(aes(geometry=GEOMETRY, fill=PERCENT_TRANSF), 
          color=NA) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    plot.margin = margin(l=100, t=80, r=100, b=80),
    plot.title = element_text(hjust=.5, vjust = 10),
    plot.subtitle = element_text(hjust=.5, vjust = 13),
    panel.border = element_rect(colour = "lightgoldenrodyellow", fill=NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(2, "cm"),
    legend.background = element_blank(),
    legend.margin = margin(t=10)
  ) +
  labs(
    title = "2º Turno das Eleições 2014 em Pernambuco",
    subtitle = "Desempenho dos candidados por município em porcentagem") +
  scale_fill_gradientn( colors= coresProporcap, 
                        limits=c(0, 100),
                        breaks=c(0, 25, 50, 75, 100),
                        labels=c("100%", "Aécio Neves", "50%", "Dilma Rousseff", "100%")
  ) +
  guides(fill = guide_colourbar(ticks = FALSE, 
                                title.position = "bottom")
  )


# PLOT THE INSULAR state ISLAND
re_fn <- votos_validos %>%
  filter(NM_MUNICIPIO == "Fernando de Noronha") %>%
  ggplot() +
  geom_sf(aes(geometry=GEOMETRY, fill=PERCENT_TRANSF), color=NA) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    panel.border = element_rect(colour = "grey75", fill=NA),
    legend.position = "none"
  ) +
  scale_fill_gradientn( colors= coresProporcap, 
                        limits=c(0, 100),
                        breaks=c(0, 25, 50, 75, 100),
                        labels=c("100%", "Aécio Neves", "50%", "Dilma Rousseff", "100%")
  ) +
  lims( x=c(-32.50, -32.35),
        y=c(-3.90, -3.78)
        )


# JOIN THE MAIN STATE + INSULAR MUNICIPALITY
ggdraw() +
  draw_plot(main_pe) +
  draw_plot(re_fn,
            scale = .2,
            height = .35,
            x = .37,
            y = .57
  )



# SAVE THE PLOT IMAGE
ggsave("map_PE_2014_PRESIDENTE_2_turn.jpg")






# PLOT THE DIFFERENCE OF MUNICIPALY WINNER BY CANDIDATE MOST VOTED (ABOVE 50%)
# plot MAIN STATE (CONTINENTAL)
main_mg <- votos_validos %>%
  filter(NM_MUNICIPIO != "Fernando de Noronha") %>%
  ggplot() +
  geom_sf(aes(geometry=GEOMETRY, fill=NM_VOTAVEL)) +
  theme_void() +
  theme(
    plot.margin = margin(t=80, r=80),
    plot.title = element_text(hjust=.5, vjust = 15),
    plot.subtitle = element_text(hjust=.5, vjust = 15),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = coresVotos) +
  labs(fill= "Candidatos",
       title = "2º Turno das Eleições 2018 em MG",
       subtitle = "Distribuição de Candidatos com maioria de votos por município"
  )

