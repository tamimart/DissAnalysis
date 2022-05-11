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

# devtools::install_github("MathiasHarrer/dmetar") # rodar uma vez só

# # Preparo do df - RODAR UM VEZ SÓ
# # ler planilha
# 
# df <- read_excel("data/Data_200FST.xlsx") # Carregar planilha
# 
# # mudar tipo da data p fator
# 
# df <- df %>%
#   mutate(year = as.numeric(format(as.Date(df$year, format = "%d/%m/%Y"),"%Y")))
# 
# glimpse(df)
# 
# # Adicionar rotulo com numero sequencial para cada estudo dentro de uma publicacao
# 
# df <- df %>%
#   mutate(label = paste(df$first_author, as.character(df$year), sep = ", ")) %>%
#   select(label, everything())
# 
# df <- df %>%
#   mutate(label = paste(df$label, df$seq, sep = "- "))
# 
# 
# # Set homogeneo para camundongo
# 
# df_c <- df %>%
#   mutate(atd_type = as.factor(atd_type),
#          comparator = as.factor(comparator)) %>%
#   filter(species == "mice",
#          sex == "M",
#          strain == "swiss",
#          model_phenotype == "NA",
#          bioterium_lightcycle == "12/12 normal",
#          fst_protocol == "test6score4") %>%
#   group_by(label, atd_type) %>%
#   slice_tail() #fica a maior dose se o tratamento for repetido numa mesma publicacao
# 
# 
# write_xlsx(df_c,"data/df_c.xlsx") # salvar em excel
# 
# # Set homogeneo para rato
# 
# df_r <- df %>%
#    mutate(atd_type = as.factor(atd_type),
#           comparator = as.factor(comparator)) %>%
#    filter(species == "rat",
#           sex == "M",
#           strain == "wistar",
#           model_phenotype == "NA",
#           bioterium_lightcycle == "12/12 normal",
#           fst_protocol == "pre15test5") %>%
#    group_by(label, atd_type) %>%
#    slice_tail() #fica a maior dose se o tratamento for repetido numa mesma publicacao
# 
# write_xlsx(df_r,"data/df_r.xlsx") # salvar em excel

# # IMPORTANTE =  abri o excel e copiei para uma nova linha todas possibilidades de comparacoes para estudos com mais de 2 braços

# NMA CAMUNDONGOS ----- 

df_c <- read_excel("data/df_c.xlsx")


# MUDAR NOME DOS TRATAMENTOS

df_c <- df_c %>% 
  mutate(atd_type = as.factor(atd_type),
                 comparator = as.factor(comparator))

levels(df_c$atd_type)[match("nortriptyline", levels(df_c$atd_type))] <- "nortriptilina"
levels(df_c$atd_type)[match("imipramine", levels(df_c$atd_type))] <- "imipramina"
levels(df_c$atd_type)[match("fluoxetine", levels(df_c$atd_type))] <- "fluoxetina"
levels(df_c$atd_type)[match("amitriptyline", levels(df_c$atd_type))] <- "amitriptilina"
levels(df_c$comparator)[match("vehicle", levels(df_c$comparator))] <- "veículo"
levels(df_c$comparator)[match("imipramine", levels(df_c$comparator))] <- "imipramina"

df_c$atd_type
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

# meta-analise em rede

nma_c <- netmeta(
  data = Efeito_c,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = comparator,
  treat2 = atd_type,
  sm = "SMD",
  method.tau = "REML",
  random = TRUE,
  fixed = FALSE,
  details.chkmultiarm = TRUE,
  tol.multiarm = .5,
  tol.multiarm.se = .5,
  reference.group = "veículo",
  sep.trts = " vs ",
  small = "good",
  method = "Inverse"
  )

nma_c


# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_c)

# plot: definir rótulos e entrada do tamanho da amostra

Efeito_c %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de acada tratamento

Efeito_c %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento

pointsizes <- c(34, 8, 10, 65, 38, 97) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 2)

png("Fig/rede_c.png", height = 600, width = 600)

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

dev.off()

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


d.evidence <- direct.evidence.plot(nma_c, random = TRUE)


d.evidence


# rank NMA estimates using P-scores (R?cker & Schwarzer, 2015) - aqui o parametro de smallvalues é inverso (pq é referente aos valores obtidos no objetivo nma_c)

png("Fig/ranking_c.png", height = 400, width = 600)

randomnetrank <- netrank(nma_c, small.values = "bad")

randomnetrank

plot(
  name = "Ranqueamento",
  randomnetrank,
  random = TRUE,
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

dev.off()

# Forestplot com todos tratammentos versus controle - aqui o parametro de smallvalues é inverso

png("Fig/forest_nma_c.png", height = 400, width = 600)

forest(nma_c,
       leftcols = c("studlab", "k", "pscore"),
       small.values = "bad",
       sortva = Pscore,
       reference.group = "veículo",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favorece veículo",
       label.right = "Favorece antidepressivo",
       smlab = paste("Antidepressivos vs Veículo \n", "Imobilidade"))

dev.off()

# TABELA COM COMPARACOES
# POSITIVO em favor da coluna, NEGATIVO em favor da linha

compable_c <- netleague(nma_c, 
                      bracket = "(",
                      digits = 2,
                      seq = randomnetrank)

compable_c

write_xlsx(compable_c$random, "data/tablenma_c.xlsx")


# ver como comparações contribuiram  para as outras )

nma_contrib_c <- netcontrib(nma_c)

nma_contrib_c <- as.data.frame(nma_contrib_c$random)
nma_contrib_c$comp <- row.names(nma_contrib_c) # adicionar nome das linhas como coluna na copia
nma_contrib_c <- nma_contrib_c %>%
  select(comp, everything())

write_xlsx(nma_contrib_c, "data/nma_contrib_c.xlsx")


# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010)

randomsplitobject <- netsplit(nma_c, digits = 2)
randomsplitobject

png("Fig/split_c.png", height = 800, width = 600)

netsplit(nma_c) %>% forest(show = "with.direct",
                           label.left = "Favorece 2º tratamento",
                         label.right = "Favorece 1º tratamento")

dev.off()

# NMA RATOS ----- 

df_r <- read_excel("data/df_r.xlsx")


# MUDAR NOME DOS TRATAMENTOS

df_r <- df_r %>% 
  mutate(atd_type = as.factor(atd_type),
         comparator = as.factor(comparator))

df_r$comparator


levels(df_r$atd_type)[match("imipramine", levels(df_r$atd_type))] <- "imipramina"
levels(df_r$atd_type)[match("fluoxetine", levels(df_r$atd_type))] <- "fluoxetina"
levels(df_r$atd_type)[match("amitriptyline", levels(df_r$atd_type))] <- "amitriptilina"
levels(df_r$atd_type)[match("mianserin", levels(df_r$atd_type))] <- "mianserina"
levels(df_r$atd_type)[match("clomipramine", levels(df_r$atd_type))] <- "clomipramina"
levels(df_r$atd_type)[match("fluvoxamine", levels(df_r$atd_type))] <- "fluvoxamina"
levels(df_r$atd_type)[match("desipramine", levels(df_r$atd_type))] <- "desipramina"
levels(df_r$atd_type)[match("amoxapine", levels(df_r$atd_type))] <- "amoxapina"

levels(df_r$comparator)[match("vehicle", levels(df_r$comparator))] <- "veículo"
levels(df_r$comparator)[match("amitriptyline", levels(df_r$comparator))] <- "amitriptilina"
levels(df_r$comparator)[match("clomipramine", levels(df_r$comparator))] <- "clomipramina"
levels(df_r$comparator)[match("desipramine", levels(df_r$comparator))] <- "desipramina"
levels(df_r$comparator)[match("imipramine", levels(df_r$comparator))] <- "imipramina"

# Calcular tamanho de efeito em SDM hedges g - formato t1 - t2 

Efeito_r <- escalc(measure = "SMD", n2i = ctr_n_corr, n1i = atd_n_round, m2i = ctr_mean, m1i = atd_mean, 
                   sd2i = ctr_sd, sd1i = atd_sd, data = df_r, 
                   append = TRUE)


# mudar as colunas yi e vi pra TE e seTE

Efeito_r <- Efeito_r %>% 
  rename(TE = yi)

# converter variancia em seTE em erro padrao

Efeito_r <- Efeito_r %>% 
  mutate(seTE = sqrt(Efeito_r$vi)/sqrt(Efeito_r$N)) # criar nova variavel transformando variancia em SE
 
as.matrix(table(Efeito_r$label))

# meta-analise em rede

png("Fig/rede_r.png", height = 600, width = 600)


nma_r <- netmeta(
  data = Efeito_r,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = comparator,
  treat2 = atd_type,
  sm = "SMD",
  method.tau = "REML",
  random = TRUE,
  fixed = FALSE,
  tol.multiarm = 3,
  tol.multiarm.se = 3,
  reference.group = "veículo",
  sep.trts = " vs ",
  small = "good",
  method = "Inverse"
)

dev.off()

nma_r 

# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_r)

# plot: definir rótulos e entrada do tamanho da amostra


Efeito_r %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de acada tratamento

Efeito_r %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento

pointsizes <- c(38, 30, 16, 38, 78, 97, 30, 186, 60, 234) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 2)

png("Fig/rede_r.png", height = 600, width = 600)

netgraph(
  nma_r,
  labels = nma_r$trts,
  points = TRUE,
  cex = 1,
  cex.points = sqrtpointsizes,
  multiarm = FALSE,
  thickness = "number.of.studies",
  plastic = FALSE,
  col = "#ec2b2b",
  col.points = "#a6243a",
  start = "circle",
  iterate = FALSE,
  seq = nma_r$trts
)

dev.off()

# em 3d

netgraph(
  nma_r,
  labels = nma_r$trts,
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
  seq = nma_r$trts,
  dim = "3d"
)


# visualizacao de evi direta e indireta

d.evidence <- direct.evidence.plot(nma_r, random = TRUE)

d.evidence


# rank NMA estimates using P-scores (R?cker & Schwarzer, 2015) 

randomnetrank <- netrank(nma_r, small.values = "bad")

png("Fig/ranking_r.png", height = 400, width = 600)

randomnetrank

plot(
  name = "Ranqueamento",
  randomnetrank,
  random = TRUE,
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

dev.off()

# Forestplot com todos tratammentos versus controle

png("Fig/forest_nma_r.png", height = 400, width = 600)

forest(nma_r,
       leftcols = c("studlab", "k", "pscore"),
       small.values = "bad",
       sortva = Pscore,
       reference.group = "veículo",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favorece veículo",
       label.right = "Favorece antidepressivo",
       smlab = paste("Antidepressivos vs Veículo \n", "Imobilidade"))

dev.off()

# TABELA COM COMPARACOES
# POSITO em favor da coluna, NEGATIVO em favor da linha

compable_r <- netleague(nma_r, 
                        bracket = "(",
                        digits = 2,
                        seq = randomnetrank)

compable_r

write_xlsx(compable_r$random, "data/tablenma_r.xlsx")


# ver como comparações contribuiram  para as outras )

nma_contrib_r <- netcontrib(nma_r) #CONFERIR DEPOIS

nma_contrib_r <- as.data.frame(nma_contrib_r$random)
nma_contrib_r$comp <- row.names(nma_contrib_r) # adicionar nome das linhas como coluna na copia
nma_contrib_r <- nma_contrib_r %>%
  select(comp, everything())

write_xlsx(nma_contrib_r, "data/nma_contrib_r.xlsx")

# netheat

png("Fig/heat_r.png", height = 800, width = 800)

netheat(nma_r, nchar.trts = 8, reference = "veículo", random = TRUE, seq = nma_r$trts, cex = 10)

dev.off()



# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010)

randomsplitobject <- netsplit(nma_r)
randomsplitobject

png("Fig/split_r.png", height = 1200, width = 600)

netsplit(nma_r) %>% forest(show = "with.direct",
                           label.left = "Favorece 2º tratamento",
                           label.right = "Favorece 1º tratamento")

dev.off()

# Preparar dfs para CINeMA


cinema_c <- df_c %>%
  select(label, comparator, atd_type, ctr_mean, atd_mean, ctr_sd, atd_sd, ctr_n_corr, atd_n_round) %>% 
  rename(id = label,
         t1 = comparator,
         y1 = ctr_mean,
         sd1 = ctr_sd,
         n1 = ctr_n_corr,
         t2 = atd_type,
         y2 = atd_mean,
         sd2 = atd_sd,
         n2 = atd_n_round)



cinema_c$rob <- c("M","M","M","M","H","M","M","M","M","M","M","L","L")
cinema_c$Indirectness <- "H"  
  
write_csv(cinema_c,"data/cinema_c.csv") # salvar em excel



cinema_r <- df_r %>%
  select(label, comparator, atd_type, ctr_mean, atd_mean, ctr_sd, atd_sd, ctr_n_corr, atd_n_round) %>% 
  rename(id = label,
         t1 = comparator,
         y1 = ctr_mean,
         sd1 = ctr_sd,
         n1 = ctr_n_corr,
         t2 = atd_type,
         y2 = atd_mean,
         sd2 = atd_sd,
         n2 = atd_n_round)


cinema_r$rob <- c("M", "M", "M", "L", "L", "L", "M", "M", "M", "L", "L", "L", "L", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "L", "M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","L","L","L")
cinema_r$Indirectness <- "H"

write_csv(cinema_r,"data/cinema_r.csv") # salvar em excel
