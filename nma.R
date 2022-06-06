# Referência: MARTINS, T. Efeito de antidepressivos em roedores no teste do nado forçado e influência de covariáveis: uma revisão sistemática e meta-análise. Orientador: Cilene Lino de Oliveira. Dissertação — Programa de Pós-Graduação em Farmacologia, Universidade Federal de Santa Catarina, Florianópolis, 2022.
# ETAPA 9: meta-analise em rede

# Carregar pacotes

library(tidyverse) # manipulacao de dados
library(readxl)    # ler arquivo do excel
library(writexl)   # salvar excel
library(esc)       # calcular tamano de efeito
library(lubridate) # manipular datas
library(netmeta)   # para ma em rede
library(dmetar)
library(gridExtra)

# # devtools::install_github("MathiasHarrer/dmetar") # rodar uma vez só
# 
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


# # IMPORTANTE =  abri o excel e copiei para novas colunas todas possibilidades de comparacoes para estudos com mais de 2 braços (formato wide), tambem exclui colunas desnecessarias

# NMA CAMUNDONGOS ----- 

df_c <- read_excel("data/df_c.xlsx")


# MUDAR NOME DOS TRATAMENTOS

df_c <- df_c %>% 
  mutate(atd_type = as.factor(atd_type),
         atd_type2 = as.factor(atd_type2),
                 comparator = as.factor(comparator))

levels(df_c$atd_type)[match("nortriptyline", levels(df_c$atd_type))] <- "nortriptilina"
levels(df_c$atd_type)[match("imipramine", levels(df_c$atd_type))] <- "imipramina"
levels(df_c$atd_type)[match("fluoxetine", levels(df_c$atd_type))] <- "fluoxetina"
levels(df_c$atd_type)[match("amitriptyline", levels(df_c$atd_type))] <- "amitriptilina"
levels(df_c$comparator)[match("vehicle", levels(df_c$comparator))] <- "veículo"
levels(df_c$atd_type2)[match("imipramine", levels(df_c$atd_type2))] <- "imipramina"


df_c$atd_type
# Calcular tamanho de efeito em SDM hedges g - formato t1 - t2 


Efeito_c <- pairwise(list(as.character(atd_type), as.character(atd_type2), as.character(comparator)),
                   n = list(atd_n_round, atd_n_round2, ctr_n_corr),
                   mean = list(atd_mean, atd_mean2, ctr_mean),
                   sd = list(atd_sd, atd_sd2, ctr_sd),
                   data = df_c, studlab = label, sm = "SMD")


Efeito_c
# ver quantos braços cada estudo

as.matrix(table(Efeito_c$label))

# meta-analise em rede

nma_c <- netmeta(
  data = Efeito_c,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  sm = "SMD",
  method.tau = "REML",
  random = TRUE,
  fixed = FALSE,
  details.chkmultiarm = TRUE,
  tol.multiarm = .5,
  reference.group = "veículo",
  sep.trts = " vs ",
  small = "good"
 )

nma_c


# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_c)

# plot: definir rótulos e entrada do tamanho da amostra

Efeito_c %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de acada tratamento

Efeito_c %>% 
  group_by(atd_type2) %>% 
  summarise(sum(atd_n_round2)) # acessar total n de acada tratamento


Efeito_c %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento

pointsizes <- c(51, 8, 10, 82, 38, 114) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 2)

png("Fig/rede_c.png", height = 600, width = 600)

netgraph(
  nma_c,
  labels = nma_c$trts,
  points = TRUE,
  cex = 1.5,
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


plot(d.evidence)


# rank NMA estimates using P-scores (R?cker & Schwarzer, 2015) - aqui o parametro de smallvalues é inverso (pq é referente aos valores obtidos no objetivo nma_c)

png("Fig/ranking_c.png", height = 400, width = 600)

randomnetrank <- netrank(nma_c, small.values = "good")

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
       small.values = "good",
       sortva = -Pscore,
       reference.group = "veículo",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favorece antidepressivo",
       label.right = "Favorece veículo",
       smlab = paste("Duração da \n", "Imobilidade"))

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
nma_contrib_c

# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010)

randomsplitobject <- netsplit(nma_c, digits = 2)
randomsplitobject

png("Fig/split_c.png", height = 800, width = 600)

netsplit(nma_c) %>% forest(show = "with.direct",
                           label.left = "Favorece 1º tratamento",
                         label.right = "Favorece 2º tratamento")

dev.off()


# NMA RATOS ----- 

df_r <- read_excel("data/df_r.xlsx")


# MUDAR NOME DOS TRATAMENTOS

df_r <- df_r %>% 
  mutate(atd_type = as.factor(atd_type),
         atd_type2 = as.factor(atd_type2),
         atd_type3 = as.factor(atd_type3),
         atd_type4 = as.factor(atd_type4),
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
levels(df_r$atd_type2)[match("clomipramine", levels(df_r$atd_type2))] <- "clomipramina"
levels(df_r$atd_type3)[match("desipramine", levels(df_r$atd_type3))] <- "desipramina"
levels(df_r$atd_type4)[match("imipramine", levels(df_r$atd_type4))] <- "imipramina"

levels(df_r$comparator)[match("vehicle", levels(df_r$comparator))] <- "veículo"

# Calcular tamanho de efeito em SDM hedges g - formato t1 - t2 


Efeito_r <- pairwise(list(as.character(comparator), as.character(atd_type), as.character(atd_type2), as.character(atd_type3), as.character(atd_type4)),
                     n = list(ctr_n_corr, atd_n_round, atd_n_round2, atd_n_round3, atd_n_round4),
                     mean = list(ctr_mean, atd_mean, atd_mean2, atd_mean3, atd_mean4),
                     sd = list(ctr_sd, atd_sd,atd_sd2, atd_sd3, atd_sd4),
                     data = df_r, studlab = label, sm = "SMD")

Efeito_r

# ver quantos braços cada estudo
as.matrix(table(Efeito_r$label))

# meta-analise em rede


nma_r <- netmeta(
  data = Efeito_r,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  sm = "SMD",
  method.tau = "REML",
  random = TRUE,
  fixed = FALSE,
  reference.group = "veículo",
  sep.trts = " vs ",
  small = "bad"
)

nma_r 

# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_r)

# plot: definir rótulos e entrada do tamanho da amostra


Efeito_r %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de acada tratamento

Efeito_r %>% 
  group_by(atd_type2) %>% 
  summarise(sum(atd_n_round2))

Efeito_r %>% 
  group_by(atd_type3) %>% 
  summarise(sum(atd_n_round3))

Efeito_r %>% 
  group_by(atd_type4) %>% 
  summarise(sum(atd_n_round4))

Efeito_r %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento


pointsizes <- c(50, 30, 16, 50, 90, 97, 30, 198, 60, 258) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 2)

png("Fig/rede_r.png", height = 600, width = 600)

netgraph(
  nma_r,
  labels = nma_r$trts,
  points = TRUE,
  cex = 1.5,
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

randomnetrank <- netrank(nma_r, small.values = "good")

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
       small.values = "good",
       sortva = -Pscore,
       reference.group = "veículo",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favorece antidepressivo",
       label.right = "Favorece veículo",
       smlab = paste("Duração da \n", "Imobilidade"))

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

png("Fig/heat_r.png", height = 700, width = 700)

netheat(nma_r, nchar.trts = 5, reference = "veículo", random = TRUE, seq = nma_r$trts, cex = 20)

dev.off()



# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010)

randomsplitobject <- netsplit(nma_r)
randomsplitobject

png("Fig/split_r.png", height = 1250, width = 600)

netsplit(nma_r) %>% forest(show = "with.direct",
                           label.left = "Favorece 1º tratamento",
                           label.right = "Favorece 2º tratamento")

dev.off()


# Preparar dfs para CINeMA -------


cinema_c <- Efeito_c%>%
  select(label, treat1, treat2, mean1, mean2, sd1, sd2, n1, n2) %>% 
  rename(id = label,
         t1 = treat1,
         y1 = mean1,
         sd1 = sd1,
         n1 = n1,
         t2 = treat2,
         y2 = mean2,
         sd2 = sd2,
         n2 = n2)



cinema_c$rob <- c("M","M","M","M","M","M","M","M","M","M","M","M","M")
cinema_c$Indirectness <- "L"  
  
write_csv(cinema_c,"data/cinema_c.csv") # salvar em excel



cinema_r <- Efeito_r %>%
  select(label, treat1, treat2, mean1, mean2, sd1, sd2, n1, n2) %>% 
  rename(id = label,
         t1 = treat1,
         y1 = mean1,
         sd1 = sd1,
         n1 = n1,
         t2 = treat2,
         y2 = mean2,
         sd2 = sd2,
         n2 = n2)


cinema_r$rob <- c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M")
cinema_r$Indirectness <- "L"

write_csv(cinema_r,"data/cinema_r.csv") # salvar em excel


# transformar resultado em .xlsx

cinema_results_c <- read.csv("data/cinema_c_random_SMD_Report.csv")
write_xlsx(cinema_results_c,"data/cinema_results_c.xlsx")


# ratos: tive que fazer a avaliação CINeMA manual, pq o site acusava inconsistencia e pedia para afrouxar com o parametro tol.multiarms, mas não tenho acesso ao codigo por tras do site.

# extrai valores do CI e PI para avaliar imprecisão e heterogeneidade 

PI <- randomsplitobject$predict
CI <- randomsplitobject$random

cinema_heterogeneity <- left_join(PI, CI, by = "comparison")
cinema_heterogeneity <- cinema_heterogeneity %>% 
  select(comparison, TE, lower.x, upper.x, lower.y, upper.y) %>% 
  rename(lPI = lower.x, 
        uPI = upper.x, 
         lCI = lower.y, 
          uCI = upper.y)


library(ggplot2)

cinema <- ggplot(data = cinema_heterogeneity) +  
  geom_rect(fill = "grey",xmin = 0,xmax = Inf,
            ymin = -.5,ymax = .5, alpha = 0.5, color = "grey") +
  geom_pointrange(aes(
    x = comparison,
    y = TE,
    ymin = lPI,
    ymax = uPI,
    color = "estimativa + PI"
  )) +
  geom_pointrange(aes(
    x = comparison,
    y = TE,
    ymin = lCI,
    ymax = uCI,
    color = "estimativa + CI"
  )) +
  scale_colour_manual(values = c("black", "red")) +
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() +
  theme_bw()  

print(cinema) #visualizar plot para fazer conferencia 

