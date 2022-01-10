# Analise exploratoria qualitativa

# Carregar pacotes

library(tibble)
library(tidyverse)
library(skimr)
library(ggthemes)
library(hrbrthemes)
library(extrafont)

font_import()


# Carregar dataframe dos dados limpos e organizados

df <- readRDS("~/R/DissAnalysis/data_geral_clean.rds")


# Verificar quantos dados faltantes por coluna

colSums(is.na(df))

# Verificar quantos dados faltantes por linha

rowSums(is.na(df))

# Estatistica descritiva basica e frequencias

summary(df)

summarize(df)

skim(df)


# Codigo modificavel para verificar os dados faltantes

data_miss <- data_geral_clean %>% 
  filter(is.na(VARIAVEL))


# Descritivo por nivel da variavel de escolha

df %>% 
  group_by(sex) %>% 
  skim()


# Especificando a variavel a ser mostrada a estatistica

df %>% 
  group_by(sex) %>% 
  skim(year, atd_class)



sex_year <- df %>% 
  filter(species == "rat") %>% 
  group_by(sex, year) %>% 
  count(sex, year)
  


# Tabela com variaveis numéricas


tab_numeric <- skim(df) %>%
  yank("numeric") %>% 
    as_tibble() 


# Figuras variaveis fator


#1. species + strain + sex


#2. atd_type + atd class


#3. measure_unit + fst_protcol + measurement_method



# Figuras variaveis character







# Figuras



sex_year %>% 
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") + 
  scale_color_few() +
  theme_ipsum()


sex_year %>% 
  ggplot(aes(x = year, fill = sex)) +
  geom_bar() +
  theme_classic() +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") +
  scale_x_continuous()
