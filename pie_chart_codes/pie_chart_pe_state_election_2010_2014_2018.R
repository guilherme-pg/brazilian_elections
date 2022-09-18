# legislative Assembly COMPOSITION 2010, 2014 AND 2018


# DATASET of 2018 elections
# 

# DATASET of 2014 elections
# https://dadosabertos.tse.jus.br/dataset/resultados-2014/resource/9df2487a-7d41-4e1f-8ca1-a9dbe43fdd02

# DATASET of 2010 elections
# https://dadosabertos.tse.jus.br/dataset/resultados-2010/resource/24996eb7-2b7c-4ef6-a427-9d9229bc1b5c


setwd("C:/Users/guima/Desktop/Data Science/Projetos/de_pe_na_eleicao/codes_and_plots/pie_chart_codes")


df_estaduais <- read.table("../../general_election_data/df_estaduais_pe_2010_2014_2018.csv", sep=",", header=TRUE)




table(df_estaduais$DS_SIT_TOT_TURNO)



