# 2022 ELECTION PE MAP FOR GOVERNADOR 1 TURN


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/election_2022")

library(dplyr)
library(ggplot2)
library(sf)

options(scipen=999)




# general data
votos_pe <- read.table("../../general_election_data/2022_election_pe_governador.csv", header=TRUE, sep=",")








# LOADING MAP
MAPA=st_read("../../maps/2018_PE_26MUE250GC_SIR.shp")




df_pr_2 <- votos_pe %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL, CD_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))



# ADEQUATE COLUMNS WITH MUNICIPALITY NAMES - TITLE CASE
MAPA$NM_MUNICIP <- stringr::str_to_title(MAPA$NM_MUNICIP)

MAPA$NM_MUNICIP <- gsub(" E ", " e ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" De ", " de ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Da ", " da ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Do ", " do ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Dos ", " dos ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub(" Das ", " das ", MAPA$NM_MUNICIP)
MAPA$NM_MUNICIP <- gsub("São Caitano", "São Caetano", MAPA$NM_MUNICIP)

df_pr_2$NM_MUNICIPIO <- stringr::str_to_title(df_pr_2$NM_MUNICIPIO)

df_pr_2$NM_MUNICIPIO <- gsub(" E ", " e ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" De ", " de ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Da ", " da ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Do ", " do ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Dos ", " dos ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub(" Das ", " das ", df_pr_2$NM_MUNICIPIO)
df_pr_2$NM_MUNICIPIO <- gsub("São Caitano", "São Caetano", df_pr_2$NM_MUNICIPIO)





# ZONE DIVISIONS: RMR, zona da mata, agreste, sertão e são francisco
pe_mun_meso <- read.table("../../metadados/PE_municipios_mesoregioes.csv", sep=";", header=TRUE)


# region dataset corrections
pe_mun_meso$MESO_REGIAO <- gsub("Agrete", "Agreste", pe_mun_meso$MESO_REGIAO)
pe_mun_meso$MESO_REGIAO <- gsub("São Francisco", "Sertão", pe_mun_meso$MESO_REGIAO)

















# change candidates names
df_pr_2$NM_VOTAVEL <- gsub("MARÍLIA VALENÇA ROCHA ARRAES DE ALENCAR", "Marília Arraes", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("RAQUEL TEIXEIRA LYRA LUCENA", "Raquel Lyra", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("ANDERSON FERREIRA RODRIGUES", "Anderson Ferreira", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("MIGUEL DE SOUZA LEÃO COELHO", "Miguel Coelho", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("DANILO JORGE DE BARROS CABRAL", "Danilo Cabral", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("JOÃO ARNALDO NOVAES JÚNIOR", "João Arnaldo", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("JONES MANOEL DA SILVA", "Jones Manoel", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("WELLINGTON DUARTE CARNEIRO", "Wellington Carneiro", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("VOTO BRANCO", "Voto Branco", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("VOTO NULO", "Voto Nulo", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("CLAUDIA MACHADO RIBEIRO", "Claudia Ribeiro", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("JADILSON FRANCISCO DE ANDRADE", "Jadilson Andrade", df_pr_2$NM_VOTAVEL)
df_pr_2$NM_VOTAVEL <- gsub("UBIRACY OLÍMPIO DA SILVA", "Ubiracy Olímpio", df_pr_2$NM_VOTAVEL)


# VETOR OF COLOR CLASSIFICATION BY DIFFERENCE OF WINNING CANDIDATE BY MUNICIPALITY
coresVotos_1 <- c("Marília Arraes"="red", 
                "Raquel Lyra"="purple", 
                "Anderson Ferreira"="darkblue", 
                "Miguel Coelho"="dodgerblue", 
                "Danilo Cabral"="gold", 
                "Jones Manoel"="darkred", 
                "Wellington Carneiro"="green4", 
                "João Arnaldo"="darkorange",
                "Voto Nulo"="grey66",
                "Voto Branco"="grey77",
                "Claudia Ribeiro"="indianred4",
                "Jadilson Andrade"="steelblue4",
                "Ubiracy Olímpio"="darkgoldenrod")

coresVotos_2 <- c("Marília Arraes"="red", 
                  "Danilo Cabral"="gold",
                  "Raquel Lyra"="purple", 
                  "Miguel Coelho"="dodgerblue",
                  "Anderson Ferreira"="darkblue" 
                  )
coresVotos_3 <- c("Marília Arraes"="firebrick", 
                  "Danilo Cabral"="gold",
                  "Raquel Lyra"="purple", 
                  "Miguel Coelho"="dodgerblue",
                  "Anderson Ferreira"="darkblue",
                  "Brancos e Nulos"="grey66",
                  "Outros"="seagreen"
                  )



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
ordered_valid_votes <- df_votos_validos[order(df_votos_validos$NM_MUNICIPIO, -df_votos_validos$QT_VOTOS), ]


# GET THE MOST VOTED CANDIDATES
grouped_valid_votes <- ordered_valid_votes %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(PERCENT = max(PERCENT))


# GET THE SECOND CANDIDATE MOST VOTED
without_most_voted <- ordered_valid_votes[!(ordered_valid_votes$PERCENT %in% grouped_valid_votes$PERCENT), ]

second_most_voted <- without_most_voted %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(PERCENT = max(PERCENT))


grouped_valid_votes$NM_VOTAVEL <- ordered_valid_votes$NM_VOTAVEL[ match(grouped_valid_votes$PERCENT, ordered_valid_votes$PERCENT) ]
grouped_valid_votes$QT_VOTOS <- ordered_valid_votes$QT_VOTOS[ match(grouped_valid_votes$PERCENT, ordered_valid_votes$PERCENT) ]
grouped_valid_votes$GEOMETRY <- ordered_valid_votes$GEOMETRY[ match(grouped_valid_votes$PERCENT, ordered_valid_votes$PERCENT) ]
grouped_valid_votes$PERCENT_FORMAT <- ordered_valid_votes$PERCENT_FORMAT[ match(grouped_valid_votes$PERCENT, ordered_valid_votes$PERCENT) ]


second_most_voted$NM_VOTAVEL <- ordered_valid_votes$NM_VOTAVEL[ match(second_most_voted$PERCENT, ordered_valid_votes$PERCENT) ]
second_most_voted$GEOMETRY <- ordered_valid_votes$GEOMETRY[ match(second_most_voted$PERCENT, ordered_valid_votes$PERCENT) ]



#BY DENSITY
# merge data
#votes_pe <- merge(MAPA, grouped_valid_votes, all=TRUE)

# reprojecting maps
#valid_votes_trans <- st_transform(votes_pe, 5880)


# not working
#votos_validos_cart <- valid_votes_trans %>%
#  cartogram::cartogram_dorling(weight = 'QT_VOTOS')


























# PLOT THE PROPORTIONS OF EACH MUNICIPALY BY CANDIDATE 

# MAP PLOT THE MAIN STATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main_pe <- grouped_valid_votes %>%
  filter(NM_MUNICIPIO != "Fernando de Noronha") %>%
  ggplot() +
  geom_sf(aes(geometry=GEOMETRY, fill=NM_VOTAVEL), 
          color=NA) +
  theme_void() +
  theme(
    panel.border = element_rect(colour = "lightgoldenrodyellow", fill=NA),
    plot.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    plot.margin = margin(l=30, t=30, r=30, b=30),
    plot.title = element_text(hjust=.5, vjust = 5),
    plot.subtitle = element_text(hjust=.5, vjust = 6),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_blank()
  ) +
  scale_fill_manual(values = coresVotos_2) +
  labs(
    title = "1º Turno das Eleições 2022 em Pernambuco para Governador",
    subtitle = "Candidatos com segunda maior quantidade de votos válidos por município") +
  lims(
    y=c(-10, -7)
  )


# PLOT THE INSULAR state ISLAND
re_fn <- grouped_valid_votes %>%
  filter(NM_MUNICIPIO == "Fernando de Noronha") %>%
  ggplot() +
  geom_sf(aes(geometry=GEOMETRY, fill=NM_VOTAVEL), color=NA) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    panel.border = element_rect(colour = "grey75", fill=NA),
    legend.position = "none"
  ) +
  scale_fill_manual(values = coresVotos_2) +
  lims( x=c(-32.50, -32.35),
        y=c(-3.90, -3.78)
  )


# JOIN THE MAIN STATE + INSULAR MUNICIPALITY
cowplot::ggdraw() +
  cowplot::draw_plot(main_pe) +
  cowplot::draw_plot(re_fn,
            scale = .2,
            height = .35,
            x = .42,
            y = .6
  )



# SAVE THE PLOT IMAGE
#ggsave("map_PE_2022_GOVERNADOR_1_turn_v2.jpg")




# average votes per candidate (percent) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10 cities with best performance
# performance in the most populous cities

populous_cities <- c("Recife",
                     "Jaboatão dos Guararapes",
                     "Olinda",
                     "Caruaru",
                     "Petrolina",
                     "Paulista",
                     "Cabo de Santo Agostinho",
                     "Camaragibe",
                     "Garanhuns",
                     "Vitória de Santo Antão",
                     "Igarassu",
                     "São Lourenço da Mata",
                     "Santa Cruz do Capibaribe",
                     "Abreu e Lima",
                     "Ipojuca"
                     )



# AGREGAR CANDIDATOS NANICOS EM OUTROS
minor_candidates <- c(
            "Jones Manoel",
            "Wellington Carneiro",
            "João Arnaldo",
            "Claudia Ribeiro",
            "Jadilson Andrade",
            "Ubiracy Olímpio"
        )
df_pr_2$NM_VOTAVEL[df_pr_2$NM_VOTAVEL %in% minor_candidates] = "Outros"


# AGREGAR BRANCOS E NULOS
df_pr_2$NM_VOTAVEL[df_pr_2$NM_VOTAVEL %in% c("Voto Nulo", "Voto Branco")] = "Brancos e Nulos"


# reorder candidates names column
df_pr_2$NM_VOTAVEL <- factor(df_pr_2$NM_VOTAVEL, levels = c(
  "Marília Arraes",
  "Danilo Cabral",
  "Raquel Lyra",
  "Miguel Coelho",
  "Anderson Ferreira",
  "Brancos e Nulos",
  "Outros"
))



# bar plot OF ALL CITIES
df_pr_2 %>%
  ggplot() +
  geom_col(aes(x=forcats::fct_reorder(NM_MUNICIPIO, TOTAL_VOTOS, .desc=TRUE),
               y=PERCENT, 
               fill=NM_VOTAVEL),
           width = 1) +
  theme(
    plot.background = element_rect(fill="grey90", color=NA),
    plot.margin = unit(c(.5,.5,.5,.5), "cm"),
    plot.title = element_text(hjust=.5),
    plot.subtitle = element_text(hjust=.5),
    panel.background = element_rect(fill="grey90", color=NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin=margin(t=-.1, unit="cm")),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.background = element_rect(fill="grey90", color=NA),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = coresVotos_3) +
    labs(x="Mais Votos                                                                                          Menos Votos",
    title = "Votos no 1º Turno das Eleições para Governador de 2022 em PE",
    subtitle = "Desempenho por município em ordem decrescente pela proporção de votos"
  )


# SAVE THE PLOT IMAGE
#ggsave("cols_PE_2022_GOVERNADOR_1_turn_v1.jpg")








# BY REGIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x= NM_CANDIDATOS, y= QT_VOTOS, by=REGION

df_pr_2$MESO_REGIAO <- pe_mun_meso$MESO_REGIAO[ match(df_pr_2$NM_MUNICIPIO, pe_mun_meso$NM_MUNICIPIO)  ]





breaks2 = c(50000, 100000, 150000)
valores2 = c("50 mil", "100 mil", "150 mil")

breaks = c(100000, 200000, 300000, 400000, 500000)
valores = c("100 mil", "200 mil", "300 mil", "400 mil", "500 mil")





# PLOT BY REGION
df_pr_2 %>%
  ggplot() +
  geom_col(aes(x=NM_VOTAVEL, 
               y=QT_VOTOS, 
               fill=NM_VOTAVEL)) +
  theme(
    plot.background = element_rect(fill="grey95", color=NA),
    plot.margin = unit(c(.5,.5,.5,1), "cm"),
    plot.title = element_text(hjust=-1),
    plot.subtitle = element_text(hjust=.6, margin = margin(0, 0, 15, 0)),
    panel.background = element_rect(fill="grey95", color=NA),
    panel.grid = element_line(color= "grey55", linetype=2),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.background = element_blank(),
    legend.title = element_blank()
  )+
  scale_fill_manual(values = coresVotos_3) +
  scale_y_continuous(breaks=breaks, labels = valores) +
  labs(title="Total de Votos no 1º Turno das Eleições para Governador de PE em 2022",
       subtitle="Desempenho dos Candidatos por Região"
  ) +
  facet_wrap(~ MESO_REGIAO)





#ggsave("grouped_cols_PE_2022_GOVERNADOR_1_turn_v1.jpg")






# distribution: votes by city
df_pr_2 %>%
  ggplot() +
  geom_point(aes(x=TOTAL_VOTOS, y=QT_VOTOS, color=NM_VOTAVEL)) +
  scale_color_manual(values = coresVotos_3)













