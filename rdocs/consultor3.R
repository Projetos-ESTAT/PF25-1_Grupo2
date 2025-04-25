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

#código
library(knitr)
estatisticas <- c("Média", "Desvio Padrão", "Variância")
df <- data.frame(
  Estatística = estatisticas,
  Distrito_Federal = c(206.5, 141.7, 200.0e12),
  Goias = c(619.7, 429.2, 184.2e12),
  Mato_Grosso = c(337.7, 209.3, 43.8e12),
  Mato_Grosso_do_Sul = c(301.2, 206.3, 42.5e12)
)
# Formatando os valores com sufixo "Milhões" ou "Trilhões"
formatar_valor <- function(valor, estat) {
  if (estat == "Variância") {
    paste0(formatC(valor / 1e12, format = "f", digits = 1, big.mark = "."), " Trilhões")
  } else {
    paste0(formatC(valor, format = "f", digits = 1, big.mark = "."), " Milhões")
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
gráfico_análise2 <- ggplot(variacao_recursos) +
  aes(x=Ano, y=Total_Recursos,colour= UF, group=UF) +
  geom_line(size=1) + geom_point(colour="#A11D21",size=2) +
  labs(x="Ano", y="Quantidade de Recursos PCD (R$ milhões)") +
  theme_estat()+
  theme(legend.position = "right")
gráfico_análise2

# código terceira análise 
library(dplyr)
benef_por_regiao <- dados %>%
  filter(Ano >= 2013 & Ano <= 2023) %>%
  group_by(Reg, Ano) %>%
  summarise(Total_Beneficiarios_PCD = sum(Quant_Beneficiario_PCD, na.rm = TRUE)) %>%
  arrange(Reg, Ano)
print(benef_por_regiao)

#código
estatisticas_regiao <- benef_por_regiao %>%
  group_by(Reg) %>%
  summarise(
    Media = mean(Total_Beneficiarios_PCD, na.rm = TRUE),
    Mediana = median(Total_Beneficiarios_PCD, na.rm = TRUE),
    Variancia = var(Total_Beneficiarios_PCD, na.rm = TRUE),
    Desvio_Padrao = sd(Total_Beneficiarios_PCD, na.rm = TRUE),
    Minimo = min(Total_Beneficiarios_PCD, na.rm = TRUE),
    Maximo = max(Total_Beneficiarios_PCD, na.rm = TRUE)
  )
estatisticas_regiao_rounded <- estatisticas_regiao %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
print(estatisticas_regiao_rounded)

#código
tabela_media <- data.frame(
  Estatística = c("Média", "Mediana", "Variância", "Desvio Padrão", "Mínimo", "Máximo"),
  O_que_mede = c(
    "Tendência central (valor típico)",
    "Valor do meio (central) dos dados",
    "Dispersão ao quadrado, medida da variação",
    "Dispersão (em valores reais) ao redor da média",
    "Valor mais baixo registrado",
    "Valor mais alto registrado"
  )
)

#gráfico de linha 
gráfico_análise3 <- ggplot(benef_por_regiao) +
  aes(x=Ano, y=Total_Beneficiarios_PCD,colour=Reg,  group= Reg) +
  geom_line(size=1) + geom_point(colour="#A11D21",size=2) +
  labs(x="Ano", y="Quantidade beneficiários PCD") +
  theme_estat()+
  theme(legend.position = "right")
gráfico_análise3

