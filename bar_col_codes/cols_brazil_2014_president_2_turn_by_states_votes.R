# BRAZIL 2014 presidential election 2 turn by state votes



# DATASET of 2014 elections
# https://dadosabertos.tse.jus.br/dataset/resultados-2014/resource/9df2487a-7d41-4e1f-8ca1-a9dbe43fdd02

# DATASET of 2014 elections
# https://dadosabertos.tse.jus.br/dataset/resultados-2010/resource/24996eb7-2b7c-4ef6-a427-9d9229bc1b5c


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/bar_col_codes")



#FILTER BY ELECTIONS 2 TURN
#votos_2014 <- read.table("../../general_election_data/votacao_secao_2014_BR.csv", sep=";", header=TRUE)
#votos_presidente = filter(votos_2014, NR_TURNO==2)
#rm(votos_2014)
#write.csv(votos_presidente, "brazil_2014_president_2_turn.csv", row.names = FALSE)



# TO IMPROVE: USE libraries individually
library(dplyr)
library(ggplot2)


options(scipen=999)

# LOADING DATASETS
# VOTES
votos_br <- read.table("../../general_election_data/brazil_2014_president_2_turn.csv", header=TRUE, sep=",")




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






# FILTER BY VALID VOTES
df_votos_validos <- filter(votos_pr_2, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")

df_votos_validos <- select(df_votos_validos, SG_UF, NM_VOTAVEL, QT_VOTOS)
votos_bn <- select(votos_pr_2, SG_UF, NM_VOTAVEL, QT_VOTOS)


# GROUP TOTAL VOTES BY state
total_votos_val <- df_votos_validos %>%
  group_by(SG_UF) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

total_votos_bn <- votos_bn %>%
  group_by(SG_UF) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))


# MATCH counties 
df_votos_validos$TOTAL_VOTOS <- total_votos_val$QT_VOTOS[ match(df_votos_validos$SG_UF, total_votos_val$SG_UF) ]
votos_bn$TOTAL_VOTOS <- total_votos_bn$QT_VOTOS[ match(votos_bn$SG_UF, total_votos_bn$SG_UF) ]

# ADD PROPORTION
df_votos_validos$PROPORCAO <- df_votos_validos$QT_VOTOS / df_votos_validos$TOTAL_VOTOS
votos_bn$PROPORCAO <- votos_bn$QT_VOTOS / votos_bn$TOTAL_VOTOS

# ADD PERCENT
df_votos_validos$PERCENT <- df_votos_validos$PROPORCAO *100
votos_bn$PERCENT <- votos_bn$PROPORCAO *100

# FORMAT PERCENT
df_votos_validos$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_votos_validos$PERCENT), "%")
votos_bn$PERCENT_FORMAT <- paste0(sprintf("%4.2f", votos_bn$PERCENT), "%")


# REORDER BY VOTES
#df_votos_validos <- df_votos_validos[order(df_votos_validos$NM_VOTAVEL, decreasing =TRUE), ]
df_votos_validos <- df_votos_validos[order(df_votos_validos$TOTAL_VOTOS, decreasing =TRUE), ]
votos_bn <- votos_bn[order(votos_bn$TOTAL_VOTOS, decreasing =TRUE), ]


# SELECT FIRST 10 CITIES WITH MORE VOTES
greatest_states <- df_votos_validos[1:20, ]
greatest_states_bn <- votos_bn[1:32, ]


# ADJUST NAMES
greatest_states$NM_VOTAVEL[ greatest_states$NM_VOTAVEL == "DILMA VANA ROUSSEFF" ] <- "DILMA ROUSSEFF"
greatest_states$NM_VOTAVEL[ greatest_states$NM_VOTAVEL == "AÉCIO NEVES DA CUNHA" ] <- "AÉCIO NEVES"

greatest_states$NM_VOTAVEL <- stringr::str_to_title( greatest_states$NM_VOTAVEL)
greatest_states_bn$NM_VOTAVEL <- stringr::str_to_title( greatest_states_bn$NM_VOTAVEL)



valores <- c("2,5 M", "5 M", "7,5 M", "10 M", "15 M")
breaks <- c(2500000, 5000000, 7500000, 10000000, 15000000)
colores <- c("Dilma Rousseff"="firebrick1", 
             "Aécio Neves"="dodgerblue4")

colores_bn <- c("Dilma Rousseff"="firebrick1", 
                "Aécio Neves"="dodgerblue4",
                "Voto Nulo"="grey50",
                "Voto Branco"="grey75")
windowsFonts("Arial" = windowsFont("Arial"))


aaa <- greatest_states %>%
  forcats::fct_reorder(SG_UF, TOTAL_VOTOS, .desc=TRUE)


greatest_states$NM_VOTAVEL <- factor(greatest_states$NM_VOTAVEL, levels=c("Dilma Rousseff", "Aécio Neves"))

# PLOT COL
greatest_states %>%
  ggplot(aes(x=forcats::fct_reorder(SG_UF, TOTAL_VOTOS, .desc=TRUE), 
             y=QT_VOTOS,
             fill=NM_VOTAVEL,
             label=scales::percent(PERCENT))
  ) +
  geom_col(position = "dodge", alpha=.9) +
  theme(
    text = element_text(family="Arial"),
    plot.title = element_text(margin=margin(t=.5, unit='cm')),
    plot.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    panel.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    panel.grid.major.y = element_line(colour = "grey85", linetype="dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin=margin(r=.5, unit="cm")),
    axis.ticks = element_blank(),
    axis.text.x = element_text(vjust = 8),
    legend.position = "bottom",
    legend.margin = margin(t = -.4, b=.5, unit='cm'),
    legend.background = element_rect(fill="lightgoldenrodyellow", color=NA),
    legend.title = element_blank()
  ) +
  labs(y = "Votos",
       title = "Votos Válidos no 2º Turno das Eleições para Presidente de 2014",
       subtitle = "Desempenho nos 10 Estados com mais votos"
  ) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(breaks=breaks, labels = valores) +
  expand_limits( y=c(0, 16000000))




# save last plot
ggsave("f1_col_president_2_turn_2014_br_by_greatest_state.jpg")


