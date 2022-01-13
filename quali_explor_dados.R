# Analise exploratoria qualitativa

# Carregar pacotes

library(tidyverse)
library(skimr)
library(extrafont)
library(summarytools)
library(writexl)
library(patchwork)
library(tibble)
library(ggthemes)
library(MetBrewer)
library(hrbrthemes)


font_import()
loadfonts(device = "win")

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


# Gráficos das variaveis -------


# Figuras variaveis fator

# EX
df %>% ggplot(aes(x = year, y = n, color = sex)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") +
    theme_classic() +
    scale_color_manual(values=met.brewer("Cross", 2))


# Populacao -----

#Freq especie

fsp <- df %>% ggplot(aes(x = species)) +
  geom_bar() +
  labs(y = "Nº de estudos", x = "Espécie", title = "a") +
  scale_x_discrete(labels = c('Rato','Camundongo')) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# especie no tempo


spt <- df  %>% 
  ggplot(aes(x = year, fill = species)) +
  geom_bar()+
  scale_fill_discrete(labels = c('Camundongo', 'Rato')) +
  labs(y = "Frequência", x = "Ano", title = "b", fill = "Espécie") + 
  theme_classic() +
  scale_fill_manual(values=met.brewer("Cross", 2)) +
  theme(axis.text=element_text(size=12, hjust = 1),
        axis.title=element_text(size=12))

# Freq sexo

fse <- df %>% 
  ggplot(aes(x = forcats::fct_infreq(sex))) +
  geom_bar() +
  labs(y = "Nº de estudos", x = "Sexo", title = "c") +
  scale_x_discrete(labels = c("Macho", "Fêmea", "Ambos", "Sem informação")) +
  theme_classic() +
  scale_color_brewer(values=met.brewer("Cross", 2))


# sexo no tempo

set <- df  %>% 
  ggplot(aes(x = year, fill = sex, order = (sex))) +
  geom_bar()+
  scale_fill_discrete(labels = c("Fêmea", "Macho", "Ambos", "Sem informação")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  labs(y = "Frequência", x = "Ano", title = "d", fill = "Sexo") + 
  theme_classic() +
  scale_fill_manual(values=met.brewer("Cross", 4)) + # Preferi esse
  theme(axis.text=element_text(size=10, angle = 0, color = "grey10", family = "Palatino"),
        axis.title=element_text(size=11, family = "Palatino"),
        plot.title=element_text(size=14, face = "bold", family = "Palatino"))

names(pdfFonts())
# Todas juntas
fonts()

fsp + spt + fse + set


# Figuras variaveis character







# Figuras



sex_year %>% 
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") + 
  theme_bw()


sex_year %>% 
  ggplot(aes(x = year, fill = sex)) +
  geom_bar() +
  theme_classic() +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") +
  scale_x_continuous()
