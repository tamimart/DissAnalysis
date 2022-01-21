# Referência: ~ref da dissertação~
# ETAPA 2: Análise exploratoria 

# Carregar pacotes

library(tidyverse)
library(skimr)
library(summarytools)
library(writexl)
library(tibble)


library(ggcorrplot)
library(ggradar)


# Carregar dataframe dos dados limpos e organizados

df <- data_geral_clean <- readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")


# Conferir variáveis e valores ------- 


# Verificar quantos dados faltantes por coluna

colSums(is.na(df))


# Estatistica descritiva basica e frequencias


summarize(df)

skim(df)

dfSummary(df,
          plain.ascii  = FALSE,
          style        = 'grid',
          graph.magnif = 0.85,
          varnumbers = FALSE,
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp")

# Codigo modificavel para verificar os dados faltantes

data_miss <- data_geral_clean %>% 
  filter(is.na(VARIAVEL))



# Tabelas estatísticas -------


# Descritivo por nivel da variavel de escolha


teste <- df %>% 
  group_by(species) %>% 
  skim()


teste

# Especificando a variavel a ser mostrada a estatistica

df %>% 
  group_by(sex) %>% 
  skim(year, atd_class)



sex_year <- df %>% 
  filter(species == "rat") %>% 
  group_by(sex, year) %>% 
  count(sex, year)



# Tabela com variaveis numéricas

# Geral

tab_numeric <- df %>% 
  skim %>%
  yank("numeric")

print(tab_numeric)

tab_numeric <- tab_numeric %>% 
  mutate(n = 562 - (n_missing))

# Ratos


tab_numeric_rat <- df %>% 
  filter(species == "rat") %>% 
  skim %>%
  yank("numeric")

tab_numeric_rat <- tab_numeric %>% 
  mutate(n = 234 - (n_missing))

# Camundongos


tab_numeric_mice <- df %>% 
  filter(species == "mice") %>% 
  skim %>%
  yank("numeric")

tab_numeric_mice <- tab_numeric %>% 
  mutate(n = 328 - (n_missing))


# Salvando tabelas

write_xlsx(tab_numeric,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\teste.xlsx")

write.table(tab_numeric, file = "teste.txt", sep = ",", quote = FALSE, row.names = F) # Depois colar conteudo .txt no word e transformar em tabela

