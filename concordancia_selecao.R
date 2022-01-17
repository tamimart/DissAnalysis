# Concordância na seleção dos artigos


# Carregar pacotes

install.packages('BiocManager')
library(pacman)
library(tidyverse)
pacman::p_load(dplyr, rel, irr)

# EMBASE -----
# Carregar o banco de dados (EMBASE)

Selection_embase <- read_excel("data/Selection_embase.xlsx")


glimpse(Selection_embase)                             


# Cálculo do kappa (2 observadores)

irr::kappa2(dados[-1])


## Cálculo do IC 95%:
rel::ckap(dados[-1], conf.level = 0.95)

#  Cálculo da concordância

irr::agree(dados[-1])




# WPS ------

# Carregar os dados (WPS)

Selection_wps <- read_excel("data/Selection_wospubscopus.xlsx", sheet = "SELEÇÃO (numero)")

# Transformar os dados (WPS)


glimpse(Selection_embase)                             

# Cálculo do kappa (2 observadores)

irr::kappa2(dados[-1])


## Cálculo do IC 95%:
rel::ckap(dados[-1], conf.level = 0.95)

#  Cálculo da concordância

irr::agree(dados[-1])
