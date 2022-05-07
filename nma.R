# Referência: ~ref da dissertação~
# ETAPA 9: meta-analise em rede

# Carregar pacotes

library(tidyverse) # manipulacao de dados
library(metafor)   # pacota para rodar meta-análise
library(readxl)    # ler arquivo do excel
library(writexl)   # salvar excel
library(esc)       # calcular tamano de efeito
library(lubridate) # manipular datas
library(netmeta)
library(dmetar)
library(gridExtra)

devtools::install_github("MathiasHarrer/dmetar") 
library(dmetar)

# ler planilha

df <- read_excel("data/Data_200FST.xlsx") # Carregar planilha

# mudar tipo da data p fator

df <- df %>%
  mutate(year = as.numeric(format(as.Date(df$year, format = "%d/%m/%Y"),"%Y")))

glimpse(df)

# Adicionar rotulo com numero sequencial para cada estudo dentro de uma publicacao

df <- df %>% 
  mutate(label = paste(df$first_author, as.character(df$year), sep = ", ")) %>% 
  select(label, everything())

df <- df %>% 
  mutate(label = paste(df$label, df$seq, sep = "- "))


# Set homogeneo para cada espécie

df_c <- df %>% 
  mutate(atd_type = as.factor(atd_type),
         comparator = as.factor(comparator)) %>% 
  filter(species == "mice",
         sex == "M",
         strain == "swiss",
         model_phenotype == "NA",
         bioterium_lightcycle == "12/12 normal",
         fst_protocol == "test6score4") %>% 
  group_by(label, atd_type) %>% 
  slice_tail() #fica a maior dose se o tratamento for repetido


write_xlsx(df_c,"data/df_c.xlsx") # salvar em excel

# IMPORTANTE =  abrir o excel e adicionar copiar para uma nova linha todas possibilidades de comparacoes para estudos com mais de 2 braços

df_c <- read_excel("data/df_c.xlsx")


# MUDAR NOME DOS TRATAMENTOS

levels(df_c$atd_type)[match("nortriptyline", levels(df_c$atd_type))] <- "nortriptilina"
levels(df_c$atd_type)[match("imipramine", levels(df_c$atd_type))] <- "imipramina"
levels(df_c$atd_type)[match("fluoxetine", levels(df_c$atd_type))] <- "fluoxetina"
levels(df_c$atd_type)[match("amitriptyline", levels(df_c$atd_type))] <- "amitriptilina"
levels(df_c$comparator)[match("vehicle", levels(df_c$comparator))] <- "veículo"
levels(df_c$comparator)[match("imipramine", levels(df_c$comparator))] <- "imipramina"

# Calcular tamanho de efeito em SDM hedges g - formato t1 - t2 

Efeito_c <- escalc(measure = "SMD", n2i = ctr_n_corr, n1i = atd_n_round, m2i = ctr_mean, m1i = atd_mean, 
                 sd2i = ctr_sd, sd1i = atd_sd, data = df_c, 
                 append = TRUE)


# mudar as colunas yi e vi pra TE e seTE

Efeito_c <- Efeito_c %>% 
  rename(TE = yi)

# converter variancia em seTE em erro padrao

Efeito_c <- Efeito_c %>% 
  mutate(seTE = sqrt(Efeito_c$vi)/sqrt(Efeito_c$N)) # criar nova variavel transformando variancia em SE

as.matrix(table(Efeito_c$label))

#NMA camundongo

nma_c <- netmeta(
  data = Efeito_c,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = comparator,
  treat2 = atd_type,
  sm = "SMD",
  random = TRUE,
  fixed = FALSE,
  details.chkmultiarm = TRUE,
  tol.multiarm = .5,
  tol.multiarm.se = .5,
  reference.group = "vehicle",
  sep.trts = " vs ",
  small = "bad"
)

nma_c


# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_c)

# plot: definir rótulos e entrada do tamanho da amostra

nma_c$trts <- c("amitriptilina", "citalopram", "fluoxetina", "imipramina", "nortriptilina", "Veículo")

n_atd <- Efeito_c %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de acada tratamento

n_ctr <- Efeito_c %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento

pointsizes <- c(34, 8, 10, 60, 38, 97) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 5)

netgraph(
  nma_c,
  labels = nma_c$trts,
  points = TRUE,
  cex = 1,
  cex.points = sqrtpointsizes,
  multiarm = FALSE,
  thickness = "number.of.studies",
  plastic = FALSE,
  col = "#ff9400",
  col.points = "orangered2",
  start = "circle",
  iterate = FALSE,
  seq = nma_c$trts
)

# em 3d

netgraph(
  nma_c,
  labels = nma_c$trts,
  points = TRUE,
  cex = 1,
  cex.points = sqrtpointsizes,
  multiarm = FALSE,
  thickness = "number.of.studies",
  plastic = FALSE,
  col = "#ff9400",
  col.points = "#FE7700",
  start = "circle",
  iterate = FALSE,
  seq = nma_c$trts,
  dim = "3d"
)


# visualizacao de evi direta e indireta

d.evidence <- direct.evidence.plot(nma_c)

d.evidence


# rank NMA estimates using P-scores (R?cker & Schwarzer, 2015) 

randomnetrank <- netrank(nma_c, small.values = "bad")

randomnetrank

plot(
  name = "Ranqueamento",
  randomnetrank,
  comb.random = TRUE,
  col = "black",
  low = "#ec2b2b",
  high = "#82c236",
  legend = TRUE,
  angle = 45,
  hjust.x = 1,
  vjust.x = 1,
  hjust.y = 1,
  vjust.y = 0,
  nchar.trts = 12,
  main.face = "bold",
  axis.size = 12
  
)


# Forestplot com todos tratammentos versus controle

forest(nma_c,
       leftcols = c("studlab", "k", "pscore"),
       small.values = "good",
       sortva = Pscore,
       reference.group = "vehicle",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favorece veículo",
       label.right = "Favorece antidepressivo",
       smlab = paste("Antidepressivos vs Veículo \n", "Imobilidade"))



# TABELA COM COMPARACOES
# POSITO em favor da coluna, NEGATIVO em favor da linha

compable_c <- netleague(nma_c, 
                      bracket = "(",
                      digits = 2)

compable_c

write_xlsx(compable_c$random, "data/tablenma_c.xlsx")


# ver como comparações contribuiram  para as outras )

netcontrib(nma_c)

# netheat

netheat(nma_c, nchar.trts = 6, showall = F, random = TRUE, seq = nma_c$trts)

dev.off()



# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010)

randomsplitobject <- netsplit(nma_c)
randomsplitobject

netsplit(nma_c) %>% forest(label.left = "Favorece 2º tratamento",
                         label.right = "Favorece 1º tratamento")


# Set homogeneo para rato

Efeito_r <- Efeito %>% 
  filter(species == "rat",
         sex == "M",
         strain == "wistar",
         model_phenotype == "NA",
         bioterium_lightcycle == "12/12 normal",
         fst_protocol == "pre15test5")

write_xlsx(Efeito_r,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\data\\Data_nma_r.xlsx") # salvar em excel



# NMA rato