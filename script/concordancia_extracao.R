# Referência: MARTINS, T. Efeito de antidepressivos em roedores no teste do nado forçado e influência de covariáveis: uma revisão sistemática e meta-análise. Orientador: Cilene Lino de Oliveira. Dissertação — Programa de Pós-Graduação em Farmacologia, Universidade Federal de Santa Catarina, Florianópolis, 2022.
# ETAPA 4: Concordância na extração dos dados


# Carregar pacotes

install_github("cran/rel")
library(remotes)   # para baixar pacote de repositorio
library(pacman)    # calcular concordancia
library(tidyverse) # manipular dados
library(readxl)    # ler excel
library(diffdf)    # ver diferença entre dfs

pacman::p_load(dplyr, rel, irr)


# tratar todos os df (revisor1 e revisor2) no script "limp_org_dados" e salvar com nomes (Data_200FST_primeirorevisor, Data_200FST_segundorevisor) na pasta "data"

# carregar arquivos já tratados

meus_dados_r1 <- read_excel("data/Data_200FST_primeirorevisor.xlsx")

meus_dados_r2 <- read_excel("data/Data_200FST_segundorevisor.xlsx")


# transformar todas colunas em character

meus_dados_r1 <- meus_dados_r1 %>%
  mutate_all(as.character)

meus_dados_r2 <- meus_dados_r2 %>%
  mutate_all(as.character)

# selecionar variaveis de cada categoria (info, quanti, quali) e colocar dados em formato longo, para comparar apenas uma coluna com todos os dados

#r1

meus_dados_r1_info <- meus_dados_r1 %>% 
  select(year, language, country, source, species:others_tests) %>% 
  pivot_longer(cols = everything(),
    values_to = "extraido",
             names_to = "coluna"
             )
  
  meus_dados_r1_quanti <- meus_dados_r1 %>% 
    select(source:N) %>% 
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna"
  ) 
  
  meus_dados_r1_quali <- meus_dados_r1 %>% 
    select(rob1:camarades11) %>% 
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna"
  )
  
  
#r2
  
meus_dados_r2_info <- meus_dados_r2 %>% 
    select(year, language, country, source, species:others_tests) %>% 
    pivot_longer(cols = everything(),
                 values_to = "extraido",
                 names_to = "coluna"
    )
  
meus_dados_r2_quanti <- meus_dados_r2 %>% 
    select(source:N) %>% 
    pivot_longer(cols = everything(),
                 values_to = "extraido",
                 names_to = "coluna"
    ) 
  
meus_dados_r2_quali <- meus_dados_r2 %>% 
    select(rob1:camarades11) %>% 
    pivot_longer(cols = everything(),
                 values_to = "extraido",
                 names_to = "coluna"
    )
  
  
  
# juntar df REVISOR 1 versus REVISOR 2

concordancia_revisores_info <- data.frame(meus_dados_r1_info$extraido, meus_dados_r2_info$extraido)
concordancia_revisores_quanti <- data.frame(meus_dados_r1_quanti$extraido, meus_dados_r2_quanti$extraido)
concordancia_revisores_quali <- data.frame(meus_dados_r1_quali$extraido, meus_dados_r2_quali$extraido)


# ANÁLISE DE CONCORDANCIA ENTRE REVISORES ------ 

#info

rel::ckap(concordancia_revisores_info[1:2], conf.level = 0.95) ## Cálculo kappa e do IC 95%:
irr::agree(concordancia_revisores_info[1:2]) #  Cálculo da concordância

diffdf(meus_dados_r1_info,
       meus_dados_r2_info) # ver onde estao as diferencas

#quanti
rel::ckap(concordancia_revisores_quanti[1:2], conf.level = 0.95) ## Cálculo kappa e do IC 95%:
irr::agree(concordancia_revisores_quanti[1:2]) #  Cálculo da concordância

diffdf(meus_dados_r1_quanti,
       meus_dados_r2_quanti)

#quali
rel::ckap(concordancia_revisores_quali[1:2], conf.level = 0.95) ## Cálculo kappa e do IC 95%:
irr::agree(concordancia_revisores_quali[1:2]) #  Cálculo da concordância

diffdf(meus_dados_r1_quali,
       meus_dados_r2_quali)
