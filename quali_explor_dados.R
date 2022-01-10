# Analise exploratoria qualitativa

# Carregar pacotes

library(tibble)
library(tidyverse)
library(skimr)
library(ggthemes)
library(hrbrthemes)
library(MetBrewer)
library(extrafont)
library(summarytools)
library(writexl)


# Carregar dataframe dos dados limpos e organizados

df <- data_geral_clean <- readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")


# Verificar quantos dados faltantes por coluna

colSums(is.na(df))


# Estatistica descritiva basica e frequencias


summarize(df)

skim(df)

dfSummary(df)

# Codigo modificavel para verificar os dados faltantes

data_miss <- data_geral_clean %>% 
  filter(is.na(VARIAVEL))


# Descritivo por nivel da variavel de escolha

teste <- df %>% 
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



# Salvando tabelas

write_xlsx(teste,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\teste.xlsx")

write.table(teste, file = "teste.txt", sep = ",", quote = FALSE, row.names = F) # Depois colar conteudo .txt no word e transformar em tabela


# Figuras variaveis fator

# EX
df %>% ggplot(aes(x = year, y = n, color = sex)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") +
    theme_classic +
    scale_color_manual(values=met.brewer("Cross", 3))


# Populacao -----

#Freq especia

fsp <- df %>% ggplot(aes(x = species)) +
  geom_bar() +
  labs(y = "Nº de estudos", x = "Espécie") +
  scale_x_discrete(labels = c('Rato','Camundongo')) +
  theme_classic()

# especie no tempo

species_time <- df %>% 
  group_by(species, year) %>% 
  count(species, year)

species_time  %>% 
  ggplot(aes(x = year, y = n, color = species)) +
  geom_point() +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_hue(labels = c('Camundongo', 'Rato')) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por espécie ao decorrrer do tempo", color = "Espécie") + 
  theme_classic()


spt <- df  %>% 
  ggplot(aes(x = year, fill = species)) +
  geom_bar()+
  scale_fill_discrete(label = c('Camundongo', 'Rato')) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por espécie ao decorrrer do tempo", fill = "Espécie") + 
  theme_classic()

# Freq sexo

fse <- df %>% 
  ggplot(aes(x = forcats::fct_infreq(sex))) +
  geom_bar() +
  labs(y = "Nº de estudos", x = "Sexo") +
  scale_x_discrete(labels = c("Macho", "Fêmea", "Ambos", "Sem informação")) +
  theme_classic()


# sexo no tempo

sex_time <- df %>% 
  group_by(sex, year) %>% 
  count(sex, year)

sex_time  %>% 
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_point(size = 0.5) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_hue(labels = c("Fêmea", "Macho", "Ambos", "Sem informação")) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", color = "Sexo") + 
  theme_classic() + 
  scale_color_manual(values=met.brewer("Cross", 4))

set <- df  %>% 
  ggplot(aes(x = year, fill = sex, order = (sex))) +
  geom_bar()+
  scale_fill_discrete(label = c("Fêmea", "Macho", "Ambos", "Sem informação")) +
  labs(y = "Frequência", x = "Ano", title = "Número de estudos por sexo ao decorrrer do tempo", fill = "Sexo") + 
  theme_classic() +
  scale_fill_manual(values=met.brewer("Cross", 4)) # Preferi esse


# Todas juntas

figure <- ggarrange(fsp, spt, fse, set,
                    ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                bottom = text_grob("Data source: \n mtcars data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                right = "I'm done, thanks :-)!",
                fig.lab = "Figure 1", fig.lab.face = "bold"
)





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
