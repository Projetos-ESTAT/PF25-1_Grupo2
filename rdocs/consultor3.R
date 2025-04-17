source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
# codigo primeira análise
dados <- read.csv("C:/Users/Usuario/Downloads/Dados PcD.csv")
library(ggplot2)
grafico1 = ggplot(dados) +
  aes(x = Quant_Recurso_PCD , y = Quant_Recurso_Total) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Quantidade de recursos PCD)",
    y = "Quantidade de recursos totais)"
  ) +
  theme_estat()

grafico1

# codigo segunda análise 
centro_oeste <- dados %>%
  filter(UF %in% c("Goiás", "Mato Grosso", "Mato Grosso do Sul", "Distrito Federal")) %>%
  select(UF, Ano, Quant_Recurso_PCD)

variacao_recursos <- centro_oeste %>%
  group_by(UF, Ano) %>%
  summarise(Total_Recursos = sum(Quant_Recurso_PCD, na.rm = TRUE)) %>%
  arrange(UF, Ano)

estatisticas <- variacao_recursos %>%
  group_by(UF) %>%
  summarise(
    Media = mean(Total_Recursos, na.rm = TRUE),
    Variância = var(Total_Recursos, na.rm = TRUE),
    Desvio_Padrao = sd(Total_Recursos, na.rm = TRUE)
  )
knitr::kable(estatisticas, caption = "Estatísticas descritivas por UF")

dados_largos <- variacao_recursos %>%
  pivot_wider(names_from = UF, values_from = Total_Recursos)
matriz_correlacao <- cor(dados_largos[, -1], use = "complete.obs")
print(matriz_correlacao)
