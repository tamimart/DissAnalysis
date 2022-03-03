# Concordância na extração dos dados


# Carregar pacotes

install_github("cran/rel")
library(remotes)
library(pacman)
library(tidyverse)
library(readxl)

pacman::p_load(dplyr, rel, irr)


# qualitativo ------

# Carregar o banco de dados e preparação

meus_dados_library <- read_excel("data/DataExtraction_RsGeral_sorteioI.xlsx", sheet = "Library")

meus_dados_library <- meus_dados_library %>% 
  filter(Included == TRUE) # Retirar linhas dos artigos excluidos na etapa de extracao

extraction_final_quali <- read_excel("data/DataExtraction_RsGeral_sorteioI.xlsx", sheet = "Extraction info")

extraction_r1_quali <- read_excel("data/DataExtraction_RsGeral_sorteioI_primeirorevisor.xlsx", sheet = "Extraction info")

extraction_r2_quali <- read_excel("data/DataExtraction_RsGeral_sorteioI_segundorevisor.xlsx", sheet = "Extraction info")


# juntar dados da biblioteca com dados extraidos


extraction_final_quali  <- dplyr::left_join(meus_dados_library, extraction_final_quali , by = "First author")

extraction_r1_quali  <- dplyr::left_join(meus_dados_library, extraction_r1_quali , by = "First author")

extraction_r2_quali  <- dplyr::left_join(meus_dados_library, extraction_r2_quali , by = "First author")

# transformar todas colunas em character


extraction_final_quali <- extraction_final_quali %>%
  mutate_all(as.character)

extraction_r1_quali <- extraction_r1_quali %>%
  mutate_all(as.character)

extraction_r2_quali <- extraction_r2_quali %>%
  mutate_all(as.character)

# colocar dados em formato longo, para comparar apenas uma coluna com todos os dados



extraction_final_quali <- extraction_final_quali %>% 
  pivot_longer(!c(IDGeral, ID, Included, authors, `First author`, year.x, title, line),
             values_to = "extraido",
             names_to = "coluna"
             )

extraction_r1_quali <- extraction_r1_quali %>% 
  pivot_longer(!c(IDGeral, ID, Included, authors, `First author`, year.x, title, line),
               values_to = "extraido",
               names_to = "coluna"
  )


extraction_r2_quali <- extraction_r2_quali %>% 
  pivot_longer(!c(IDGeral, ID, Included, authors, `First author`, year.x, title, line),
               values_to = "extraido",
               names_to = "coluna"
  )



glimpse(Selection_embase)                             

# Atribuição qualitativo------

# Cálculo do kappa 

irr::kappa2(Selection_embase[1:2])


## Cálculo do IC 95%:

rel::ckap(Selection_embase[1:2], conf.level = 0.95)


#  Cálculo da concordância

irr::agree(Selection_embase[1:2])

# Razão de exclusão EMBASE -------


# Cálculo do kappa 

irr::kappa2(Selection_embase[3:4])


## Cálculo do IC 95%:

rel::ckap(Selection_embase[3:4], conf.level = 0.95)


#  Cálculo da concordância

irr::agree(Selection_embase[3:4])


