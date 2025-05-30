source("C:/Users/vinic/Downloads/PF25-1_Grupo2-main/rdocs/source/packages.R")

# codigo primeira análise
dados <- read.csv("C:/Users/vinic/Downloads/Dados PcD.csv")
library(ggplot2)
library(scales)

grafico1 <- ggplot(dados) +
  aes(x = Quant_Recurso_PCD , y = Quant_Recurso_Total) +
  geom_point(colour = "#A11D21", size = 3) +
  scale_x_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(
    x = "Quantidade de recursos PCD",
    y = "Quantidade de recursos totais"
  ) +
  theme_estat()
grafico1

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


# variacao_recursos %>%
# summarize(
#     Média = round(mean(Total_Recursos), 2),
#     `Desvio Padrão` = round(sd(Total_Recursos), 2),
#     Variância = round(var(Total_Recursos), 2),
#     Mínimo = round(min(Total_Recursos), 2),
#     `1º Quartil` = round(quantile(Total_Recursos, 0.25), 2),
#     Mediana = round(quantile(Total_Recursos, 0.5), 2),
#     `3º Quartil` = round(quantile(Total_Recursos, 0.75), 2),
#     Máximo = round(max(Total_Recursos), 2)
#   ) %>%
#   t() %>%
#   as.data.frame() %>%
#   rownames_to_column(var = "Medida") %>%
#   write.csv()


# código terceira análise 
library(dplyr)
benef_por_regiao <- dados %>%
  filter(Ano >= 2013 & Ano <= 2023) %>%
  group_by(Reg, Ano) %>%
  summarise(Total_Beneficiarios_PCD = sum(Quant_Beneficiario_PCD, na.rm = TRUE)) %>%
  arrange(Reg, Ano)

benef_por_regiao

benef_por_regiao <- dados %>%
  filter(Ano >= 2013 & Ano <= 2023) %>%
  group_by(Reg, Ano) %>%
  summarise(Total_Beneficiarios_PCD = sum(Quant_Beneficiario_PCD, na.rm = TRUE)) %>%
  arrange(Reg, Ano) %>%
  # Garantir que todos os anos apareçam para cada região
  complete(Ano = full_seq(2013:2023, 1), fill = list(Total_Beneficiarios_PCD = 0))

# GRAFICO ANALISE 3 -------------------------------------------------------


# Criar o gráfico de linha corrigido
gráfico_análise3 <- ggplot(benef_por_regiao, aes(x = Ano, y = Total_Beneficiarios_PCD, colour = Reg)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(x = "Ano", y = "Quantidade de beneficiários PCD", 
       color = "Região") +
  scale_x_continuous(breaks = 2013:2023, labels = 2013:2023) +
  theme_estat() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
gráfico_análise3

