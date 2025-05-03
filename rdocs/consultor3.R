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

media_pcd <- mean(dados$Quant_Recurso_PCD, na.rm = TRUE)
media_total <- mean(dados$Quant_Recurso_Total, na.rm = TRUE)
mediana_pcd <- median(dados$Quant_Recurso_PCD, na.rm = TRUE)
mediana_total <- median(dados$Quant_Recurso_Total, na.rm = TRUE)
variancia_pcd <- var(dados$Quant_Recurso_PCD, na.rm = TRUE)
variancia_total <- var(dados$Quant_Recurso_Total, na.rm = TRUE)
desvio_pcd <- sd(dados$Quant_Recurso_PCD, na.rm = TRUE)
desvio_total <- sd(dados$Quant_Recurso_Total, na.rm = TRUE)
correlacao <- cor(dados$Quant_Recurso_PCD, dados$Quant_Recurso_Total, method = "pearson", use = "complete.obs")

tabela_resumo <- data.frame(
  Medida = c("Média", "Mediana", "Variância", "DP", "Corr de Pearson"),
  Quant_Recurso_PCD = c(media_pcd, mediana_pcd, variancia_pcd, desvio_pcd, correlacao),
  Quant_Recurso_Total = c(media_total, mediana_total, variancia_total, desvio_total, correlacao)
)




# codigo segunda análise 
library(dplyr)
centro_oeste <- dados %>%
  filter(UF %in% c("Goiás", "Mato Grosso", "Mato Grosso do Sul", "Distrito Federal")) %>%
  select(UF, Ano, Quant_Recurso_PCD)
#código
variacao_recursos <- centro_oeste %>%
  group_by(UF, Ano) %>%
  summarise(
    Total_Recursos = round(sum(Quant_Recurso_PCD, na.rm = TRUE)/1e6, 1)
  ) %>%
  arrange(UF, Ano)

#código
estatisticas <- variacao_recursos %>%
  group_by(UF) %>%
  summarise(
    Media = mean(Total_Recursos, na.rm = TRUE),
    Variância = var(Total_Recursos, na.rm = TRUE),
    Desvio_Padrao = sd(Total_Recursos, na.rm = TRUE)
  )
knitr::kable(estatisticas, caption = "Estatísticas descritivas por UF")

#código
library(knitr)
estatisticas_manual <- c("Média", "DP", "Variância")
df <- data.frame(
  Estatística = estatisticas_manual,
  Distrito_Federal = c(206.5, 141.7, 200.0e12),
  Goias = c(619.7, 429.2, 184.2e12),
  Mato_Grosso = c(337.7, 209.3, 43.8e12),
  Mato_Grosso_do_Sul = c(301.2, 206.3, 42.5e12)
)
# Formatando os valores com sufixo "Milhões" ou "Trilhões"
formatar_valor <- function(valor, estat) {
  if (estat == "Variância") {
    paste0(formatC(valor / 1e12, format = "f", digits = 1, big.mark = "."), " T")
  } else {
    paste0(formatC(valor, format = "f", digits = 1, big.mark = "."), " M")
  }
}
# Aplicando formatação a cada coluna
df_formatada <- df
df_formatada$Distrito_Federal <- mapply(formatar_valor, df$Distrito_Federal, df$Estatística)
df_formatada$Goias <- mapply(formatar_valor, df$Goias, df$Estatística)
df_formatada$Mato_Grosso <- mapply(formatar_valor, df$Mato_Grosso, df$Estatística)
df_formatada$Mato_Grosso_do_Sul <- mapply(formatar_valor, df$Mato_Grosso_do_Sul, df$Estatística)
# Renderizando a tabela com kable
kable(df_formatada, format = "latex", booktabs = TRUE,
      caption = "Medidas resumo da(o) [nome da variável]",
      col.names = c("Estatística", "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul"),
      align = "lrrrr")

grafico_boxplot <- ggplot(centro_oeste) +
  aes(x = UF, y = Quant_Recurso_PCD) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Estados", y = "Valores da variação de recursos (em R$ milhões)") +
  theme_estat()
grafico_boxplot



# código terceira análise 
library(dplyr)
benef_por_regiao <- dados %>%
  filter(Ano >= 2013 & Ano <= 2023) %>%
  group_by(Reg, Ano) %>%
  summarise(Total_Beneficiarios_PCD = sum(Quant_Beneficiario_PCD, na.rm = TRUE)) %>%
  arrange(Reg, Ano)

benef_por_regiao

benef_por_regiao <- dados %>%
  filter(Ano >= 2012 & Ano <= 2023) %>%
  group_by(Reg, Ano) %>%
  summarise(Total_Beneficiarios_PCD = sum(Quant_Beneficiario_PCD, na.rm = TRUE)) %>%
  arrange(Reg, Ano) %>%
  # Garantir que todos os anos apareçam para cada região
  complete(Ano = full_seq(2012:2023, 1), fill = list(Total_Beneficiarios_PCD = 0))

# Criar o gráfico de linha corrigido
gráfico_análise3 <- ggplot(benef_por_regiao, aes(x = Ano, y = Total_Beneficiarios_PCD, colour = Reg, group = Reg)) +
  geom_line(size = 1) + 
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ano", y = "Quantidade de beneficiários PCD", 
       title = "Evolução de Beneficiários PCD por Região (2012-2023)") +
  scale_x_continuous(breaks = 2012:2023, labels = 2012:2023) +
  theme_estat() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
gráfico_análise3
