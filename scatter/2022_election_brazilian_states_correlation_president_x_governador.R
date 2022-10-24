# CORRELATION VOTES PRESIDENT X GOVERNADOR IN DECISIVE BRAZILIAN STATES





# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/de_pe_na_eleicao/codes_and_plots/scatter")


library(dplyr)
library(ggplot2)


options(scipen=999)





# general data
VOTES_state <- read.table("../../general_election_data/votacao_secao_2022_MG.csv", header=TRUE, sep=";")

VOTES_BR <- read.table("../../general_election_data/votacao_secao_2022_BR.csv", header=TRUE, sep=";")



president_mg <- filter(VOTES_BR, SG_UF=="MG")

governador_by_city <- president_mg %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- governador_by_city %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
governador_by_city$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(governador_by_city$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]

# ADD PROPORTION
governador_by_city$PROPORCAO <- governador_by_city$QT_VOTOS/ governador_by_city$TOTAL_VOTOS

# ADD PERCENT
governador_by_city$PERCENT <- governador_by_city$PROPORCAO *100

# FORMAT PERCENT
governador_by_city$PERCENT_FORMAT <- paste0(sprintf("%4.2f", governador_by_city$PERCENT), "%")

colnames(governador_by_city)[colnames(governador_by_city) == "PERCENT"] <- "PERCENT_PR"

# select ONLY BOLSONARO VOTES BY CITY
pr_candidate <- filter(governador_by_city, NM_VOTAVEL=="JAIR MESSIAS BOLSONARO")
pr_candidate <- filter(governador_by_city, NM_VOTAVEL=="LUIZ INÁCIO LULA DA SILVA")





governador_mg <- filter(VOTES_state, DS_CARGO=="GOVERNADOR")

governador_by_city <- governador_mg %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- governador_by_city %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
governador_by_city$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(governador_by_city$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]

# ADD PROPORTION
governador_by_city$PROPORCAO <- governador_by_city$QT_VOTOS/ governador_by_city$TOTAL_VOTOS

# ADD PERCENT
governador_by_city$PERCENT <- governador_by_city$PROPORCAO *100

# FORMAT PERCENT
governador_by_city$PERCENT_FORMAT <- paste0(sprintf("%4.2f", governador_by_city$PERCENT), "%")

colnames(governador_by_city)[colnames(governador_by_city) == "PERCENT"] <- "PERCENT_GOV"

# select ONLY ZEMA VOTES BY CITY
gov_candidate <- filter(governador_by_city, NM_VOTAVEL=="ROMEU ZEMA NETO")





GOV_X_PRE <- merge(pr_candidate[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], gov_candidate[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)




GOV_X_PRE %>%
  ggplot() +
  geom_point(aes(x=PERCENT_GOV, y=PERCENT_PR, size=TOTAL_VOTOS), alpha=0.5, colour="darkorange") +
  geom_abline(slope = 1, intercept =0, lwd=.5, colour="grey35", linetype='dashed') +
  lims(x=c(0, 100), y=c(0, 100)) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill="grey95", color=NA),
    plot.title = element_text(vjust=3, hjust=.5),
    plot.subtitle = element_text(vjust=3, hjust=.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust=3),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill="grey95", color=NA),
    legend.key = element_rect(fill = NA)
  ) +
  labs(x= "Romeu Zema",
       y= "Lula",
       size = "Municípios até",
       title= "Correlation by percentage of votes between President and Governor",
       subtitle = "First round of 2022"
       ) +
  scale_size_continuous(breaks = c(25000, 100000, 500000, 1500000),
                        labels = c("25 mil", "100 mil", "500 mill", "1.5 milhões")
                        )


ggsave("points_2022_PRESIDENT_x_GOVERNOR_MG_1_turn.jpg")

# PLOT 

# X = GOVERNADOR (ZEMA, KALIL, TARCISISO)
# Y = BOZONARO






