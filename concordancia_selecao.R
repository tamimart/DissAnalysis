# Referência: MARTINS, T. Efeito de antidepressivos em roedores no teste do nado forçado e influência de covariáveis: uma revisão sistemática e meta-análise. Orientador: Cilene Lino de Oliveira. Dissertação — Programa de Pós-Graduação em Farmacologia, Universidade Federal de Santa Catarina, Florianópolis, 2022.
# ETAPA 2: Concordância na seleção dos artigos


# Carregar pacotes

install_github("cran/rel")
library(remotes)
library(pacman)
library(tidyverse)
library(readxl)

pacman::p_load(dplyr, rel, irr)

# EMBASE -----
# Carregar o banco de dados (EMBASE)

Selection_embase <- read_excel("data/Selection_embase.xlsx")

col_order <- c("full_r1",
          "full_r2",
          "reason1_r1",
          "reason1_r2",
          "reason2_r1",
          "reason2_r2")
  
Selection_embase <- Selection_embase[, col_order]



glimpse(Selection_embase)                             

# Atribuição EMBASE------


## Cálculo do kappa e IC 95%:

rel::ckap(Selection_embase[1:2], conf.level = 0.95)


#  Cálculo da concordância

irr::agree(Selection_embase[1:2])

# Razão de exclusão EMBASE -------



## Cálculo do kappa IC 95%:

rel::ckap(Selection_embase[3:4], conf.level = 0.95)


#  Cálculo da concordância

irr::agree(Selection_embase[3:4])



# WPS ------

# Carregar os dados (WPS)

Selection_wps <- read_excel("data/Selection_wps.xlsx")

col_order <- c("absfull_r1",
               "absfull_r2",
               "reason_r1",
               "reason_r2")



Selection_wps <- Selection_wps[, col_order]



glimpse(Selection_wps)
                            
# Atribuição WPS ---------


## Cálculo do kappa e IC 95%:
rel::ckap(Selection_wps[1:2], conf.level = 0.95)

#  Cálculo da concordância

irr::agree(Selection_wps[1:2])

# Razão de exclusão WPS ------


## Cálculo do kappa e IC 95%:
rel::ckap(Selection_wps[3:4], conf.level = 0.95)

#  Cálculo da concordância

irr::agree(Selection_wps[3:4])
