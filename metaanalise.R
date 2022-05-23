# Referência: MARTINS, T. Efeito de antidepressivos em roedores no teste do nado forçado e influência de covariáveis: uma revisão sistemática e meta-análise. Orientador: Cilene Lino de Oliveira. Dissertação — Programa de Pós-Graduação em Farmacologia, Universidade Federal de Santa Catarina, Florianópolis, 2022.
# ETAPA 8: Meta-analise, analise de subgrupos, metaregressao, vies de publicacao

# Carregar pacotes

library(tidyverse) # manipulacao de dados
library(bitops)    # operadores
library(metafor)   # pacota para rodar meta-análise
library(Formula)
library(readxl)    # ler arquivo do excel
library(writexl)   # salvar excel
library(extrafont) # fonte extra
library(cowplot)   # alinhamento e anotacao plot
library(metapower) # calculo de poder
library(esc)       # calcular tamano de efeito
library(lubridate) # manipular datas
library(weightr)   # testar vies de publicação
library("devtools")# para baixar pacote remoto

#devtools::install_github("dsquintana/metameta") 
#library(metameta) # para calcular poder dos estudos incluidos


df <- read_excel("data/Data_200FST.xlsx") # Carregar planilha

# mudar tipo da data p fator

df <- df %>%
   mutate(year = as.numeric(format(as.Date(df$year, format = "%d/%m/%Y"),"%Y")))

glimpse(df)

# separa metodo do detalhe do metodo

mmd <- df %>% # separar variavel em duas
  select(measurement_method) %>% 
  separate(col = measurement_method, sep = ", ", into = c("measurement_method", "measurement_method_detail"))


df <- df %>%
  mutate(measurement_method_detail = as.factor(mmd$measurement_method_detail),
         measurement_method =  as.factor(mmd$measurement_method)) # adicionar variaveis separadas no df mãe


# criar funcao para converter hedges g para cohens d

g_to_d <- function(vg, vn) {
  vd <- vg / (1 - 3 / (4 * (vn) - 9))
  return(vd)
}


# Calcular tamanho de efeito em SDM hedges g


Efeito <- escalc(measure = "SMD", n1i = ctr_n_corr, n2i = atd_n_round, m1i = ctr_mean, m2i = atd_mean, 
                 sd1i = ctr_sd, sd2i = atd_sd, data = df, 
                 append = TRUE)


# Metaanalise por modelo de efeitos aleatorios --------------------------

Teste <- rma(yi, vi, data = Efeito, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))


Teste

# Gerar intervalo de confianca e predicao 

predict(Teste, digits = 3)

# Calculo do poder geral

df %>% 
  summarize(mean(N)) #obter N total

g_to_d(vg = 1.7483, vn = 15.6) # converter hedges g para cohens d (sera usado no mpower())

poder_geral <- mpower(effect_size = 1.852365, study_size = 15.6, k = 561, i2 = 0.83, es_type = "d") # calcular o poder 

print(poder_geral)
plot_mpower(poder_geral)


# Plot e save forestplot 

pdf("Fig/floresta_toda.pdf", height = 120, width = 25)

floresta <- forest(
  Teste,
  cex = 1,
  ylim = c(-2, 567),
  slab = (paste(
    Efeito$first_author, as.character(Efeito$year), sep = ", "
  )),
  mlab = "",
  order = Efeito$yi,
  xlab = "Hedges g",
  xlim =  c(-40, 40),
  showweight = T,
  cex.lab = 2,
  cex.axis = 1.5,
  col = "blue",
  border = "blue",
  fonts = "sans"
)

# Adicionar textos


op <- par(cex = 0.75, font = 2, family = "sans")
text(c(-6, 7.75),     568, font = 2.5,
     cex = 2, c("Control", "Antidepressant"))
text(c(90),
     568,
     font = 2.5,
     cex = 2,
     c("Weights Hedges g [95% CI]"))
text(c(-35),
     568,
     font = 2.5,
     cex = 2,
     c("Author(s), year"))

text(-40, -1, pos = 4, cex = 2, bquote(paste("RE Model (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
                                           ", df = ", .(Teste$k - Teste$p),
                                           ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
                                           I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
                                           tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))

dev.off() 

# Análise de sensibilidade ------------------------------------

# verificar outliers e casos influentes - nao esta na dissertacao

png("Fig/baujat.png")
baujat(Teste, symbol = "slab") # xaxis = indica se o estudo é um outlier e yaxis mostra a influencia do estudo sobre o resultado
dev.off() 

# ver quais estudos estao influenciando em diversos aspectos 

png("Fig/influence2.png")
inf <- influence(Teste)
plot(inf)
dev.off()

tinf <- print(inf) # criar tabela com os resultados
tinf$id <- Efeito$line # add coluna de id
tinf$sr <- Efeito$study_reference # add coluna de referencia
write_xlsx(tinf,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\data\\influence2.xlsx") # salvar em excel


# rodar de novo
leave1 <- leave1out(Teste, digits = 3) #colocar resultado num objeto
leave1df <- as.data.frame(leave1) #transformar obj lista em df
final_df <- as.data.frame(t(leave1df)) # inverter linhas e colunas
copia_final_df <- final_df # fazer uma copia
copia_final_df$rn <- row.names(final_df) # adicionar nome das linhas como coluna na copia
copia_final_df <- copia_final_df %>%
  select(rn, everything()) # trazer coluna dos nomes para frente do df

write_xlsx(copia_final_df, "C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\data\\leave2.xlsx") # salvar em excel

# Análise de Vies de publicação -----------------------------------

# [gráfico de funil]

png("Fig/funil.png", height = 900, width = 600)

par(mfrow = c(2, 1), oma = c(0,1,0,0))

funil <-
  metafor::funnel(
    Teste,
    yaxis = "sei",
    addtau2 = FALSE,
    xlab = "Tamanho de efeito",
    ylab = "Erro padrão",
    back = "gray94",
    level = c(90, 95, 99),
    shade = c("white", "#82c236", "#009c7e"),
    hlines = "white",
    lty = 2,
    pch = 19,
    col = 25,
    label = FALSE,
    offset = 0.1,
    legend = TRUE,
    ci.res = 1000,
    cex.lab = 1.7, 
    cex.axis = 1.5
  )


funil2 <-
  metafor::funnel(
    Teste,
    yaxis = "sqrtninv",
    addtau2 = FALSE,
    xlab = "Tamanho de efeito",
    ylab = "1/√n",
    back = "gray94",
    hlines = "white",
    lty = 2,
    pch = 19,
    col = 25,
    label = FALSE,
    offset = 0.1,
    legend = FALSE,
    ci.res = 1000,
    cex.lab = 1.7, 
    cex.axis = 1.5
  )
dev.off()



# [eggers regression]
# evidencia de vies de estudos pequenos

regtest(Teste, model = "rma", predictor = "sei")

regtest(Teste, model = "rma", predictor = "sqrtninv")

# [trim and fill]

faltantes <- metafor::trimfill(Teste, side = "left", estimator = "R0", maxiter = 100, verbose = FALSE) #R0 preferivel quando a MA tem >k Rothstein HR, Sutton AJ, Borenstein M. Publication Bias in Meta-Analysis: Prevention, Assessment and Adjustments. Chichester, UK: John Wiley & Sons; 2005. An advantage of estimator "R0" is that it provides a test of the null hypothesis that the number of missing studies (on the chosen side) is zero

print(faltantes)

png("Fig/funil_ef.png", height = 900, width = 600)

par(mfrow = c(2, 1), oma = c(0,1,0,0))

funil_ef1 <- metafor::funnel(
  faltantes,
  yaxis = "sei",
  addtau2 = FALSE,
  xlab = "Tamanho de efeito",
  ylab = "Erro padrão",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#82c236", "#009c7e"),
  hlines = "white",
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7, 
  cex.axis = 1.5
)

funil_ef2 <- metafor::funnel(
  faltantes,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  xlab = "Tamanho de efeito",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "aquamarine", "aquamarine3"),
  hlines = "white",
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7, 
  cex.axis = 1.5
)

dev.off()

funil_ef

# [weight function model]
#Teste especcifico para publication bias - aumenta o peso dos estudos que sao menos provaveis de serem publicados e diminiu o peso daqueles que sao mais provaveis de serem publicados - baseado no valor de p
#likehood test alfa 0.10

wf <- weightfunct(Efeito$yi, Efeito$vi, table = TRUE)

wf


# Vies de publicacao POR ESPECIE

Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)

regtest(Teste_mice, model = "rma", predictor = "sei")
regtest(Teste_mice, model = "rma", predictor = "sqrtninv")

Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
regtest(Teste_rat, model = "rma", predictor = "sei")
regtest(Teste_rat, model = "rma", predictor = "sqrtninv")

# [trim and fill]

faltantes_m <- metafor::trimfill(Teste_mice, side = "left", estimator = "R0", maxiter = 100, verbose = FALSE) #R0 preferivel quando a MA tem >k Rothstein HR, Sutton AJ, Borenstein M. Publication Bias in Meta-Analysis: Prevention, Assessment and Adjustments. Chichester, UK: John Wiley & Sons; 2005. An advantage of estimator "R0" is that it provides a test of the null hypothesis that the number of missing studies (on the chosen side) is zero
faltantes_r <- metafor::trimfill(Teste_rat, side = "left", estimator = "R0", maxiter = 100, verbose = FALSE) #R0 preferivel quando a MA tem >k Rothstein HR, Sutton AJ, Borenstein M. Publication Bias in Meta-Analysis: Prevention, Assessment and Adjustments. Chichester, UK: John Wiley & Sons; 2005. An advantage of estimator "R0" is that it provides a test of the null hypothesis that the number of missing studies (on the chosen side) is zero

faltantes_m
faltantes_r 



png("Fig/funil_esp.png", height = 900, width = 1200)

par(mfrow = c(2, 2), oma = c(0,1,0,0))

funil_esp1_m <- metafor::funnel(
  faltantes_m,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Camundongo",
  xlab = "Tamanho de efeito",
  ylab = "Erro padrão",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#ff9400", "#FE7700"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(13,0), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7, 
  cex.axis = 1.5,
  cex.main = 2.2
)

funil_esp1_r <- metafor::funnel(
  faltantes_r,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Rato",
  xlab = "Tamanho de efeito",
  ylab = "Erro padrão",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#ec2b2b", "#a6243a"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(13,0), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7, 
  cex.axis = 1.5,
  cex.main = 2.2
)

funil_esp2_m <- metafor::funnel(
  faltantes_m,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  xlab = "Tamanho de efeito",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#ff9400", "#FE7700"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(0.400,0.100), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7, 
  cex.axis = 1.5
)

funil_esp2_r <- metafor::funnel(
  faltantes_r,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  xlab = "Tamanho de efeito",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#a6243a", "#ec2b2b"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(0.400,0.100), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7, 
  cex.axis = 1.5
)

dev.off()


# [weight function model]
#Teste especcifico para publication bias - aumenta o peso dos estudos que sao menos provaveis de serem publicados e diminiu o peso daqueles que sao mais provaveis de serem publicados - baseado no valor de p
#likehood test alfa 0.10

wmice <- Efeito %>% 
  filter(species == "mice")

wfm <- weightfunct(wmice$yi, wmice$vi, table = TRUE)

wfm

wrat <- Efeito %>% 
  filter(species == "rat")

wfr <- weightfunct(wrat$yi, wrat$vi, table = TRUE)

wfr


# outra forma

library(metaviz)

viz_sunset(Teste, 
           method = "REML",
           contours = TRUE,
           true_effect = 1.5, 
           power_contours =  "continuous")

# Analise de subgrupos --------------------------------


# [POPULAÇÃO]------
#especie [mice] ----

Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)
Teste_mice

#sexo


Teste_macho_m <- rma(yi, vi, subset = (sex == "M" & species == "mice"), data = Efeito)
Teste_macho_m

Teste_femea_m <- rma(yi, vi, subset = (sex == "F" & species == "mice"), data = Efeito)
Teste_femea_m

Teste_sexoambos_m <- rma(yi, vi, subset = (sex == "M and F" & species == "mice"), data = Efeito)
Teste_sexoambos_m


Efeito %>% 
  filter(sex == "M and F" & species == "mice") %>% 
  select(authors) # mesma publicação?


Teste_sexoind_m <- rma(yi, vi, subset = (sex == "NA" & species == "mice"), data = Efeito)
Teste_sexoind_m

Efeito %>% 
  filter(sex == "NA" & species == "mice") %>% 
  select(authors) # mesma publicação?

#linhagem

Teste_swiss <- rma(yi, vi, subset = (strain == "swiss" & species == "mice"), data = Efeito)
Teste_swiss

Teste_CD1 <- rma(yi, vi, subset = (strain == "CD-1" & species == "mice"), data = Efeito)
Teste_CD1

Teste_C57BL <- rma(yi, vi, subset = (strain == "C57BL" & species == "mice"), data = Efeito)
Teste_C57BL

Teste_BALB <- rma(yi, vi, subset = (strain == "BALB" & species == "mice"), data = Efeito)
Teste_BALB

Teste_ddY <- rma(yi, vi, subset = (strain == "ddY" & species == "mice"), data = Efeito)
Teste_ddY

Efeito %>% 
  filter(strain == "ddY") %>% 
  select(authors) # mesma publicação?

Teste_laca <- rma(yi, vi, subset = (strain == "laca" & species == "mice"), data = Efeito)
Teste_laca

Efeito %>% 
  filter(strain == "laca") %>% 
  select(authors) # mesma publicação?

Teste_OF1 <- rma(yi, vi, subset = (strain == "OF1" & species == "mice"), data = Efeito)
Teste_OF1

Efeito %>% 
  filter(strain == "OF1") %>% 
  select(authors) # mesma publicação?

Teste_NMRI <- rma(yi, vi, subset = (strain == "NMRI" & species == "mice"), data = Efeito)
Teste_NMRI

Efeito %>% 
  filter(strain == "NMRI") %>% 
  select(authors) # mesma publicação?

Teste_sabra <- rma(yi, vi, subset = (strain == "sabra" & species == "mice"), data = Efeito)
Teste_sabra

Efeito %>% 
  filter(strain == "sabra") %>% 
  select(authors) # mesma publicação?

Teste_BKTO <- rma(yi, vi, subset = (strain == "BKTO" & species == "mice"), data = Efeito)
Teste_BKTO

Efeito %>% 
  filter(strain == "BKTO") %>% 
  select(authors) # mesma publicação?

Teste_NA_m <- rma(yi, vi, subset = (strain == "NA" & species == "mice"), data = Efeito)
Teste_NA_m

Efeito %>% 
  filter(strain == "NA") %>% 
  select(authors) # mesma publicação?

Teste_DBA2 <- rma(yi, vi, subset = (strain == "DBA/2" & species == "mice"), data = Efeito)
Teste_DBA2

Efeito %>% 
  filter(strain == "DBA/2") %>% 
  select(authors) # mesma publicação?

Teste_B6SJL <- rma(yi, vi, subset = (strain == "B6SJL (R406W transgenic)" & species == "mice"), data = Efeito)
Teste_B6SJL 

Efeito %>% 
  filter(strain == "B6SJL (R406W transgenic)") %>% 
  select(authors) # mesma publicação?


Teste_129S6 <- rma(yi, vi, subset = (strain == "129S6" & species == "mice"), data = Efeito)
Teste_129S6 # k < 3

Teste_SPF <- rma(yi, vi, subset = (strain == "SPF" & species == "mice"), data = Efeito)
Teste_SPF # k < 3

levels(Efeito$strain)


# estresse 

Teste_stress_m <- rma(yi, vi, subset = (model_phenotype != "NA" & species == "mice"), data = Efeito)
Teste_stress_m

Teste_nostress_m <- rma(yi, vi, subset = (model_phenotype == "NA" & species == "mice"), data = Efeito)
Teste_nostress_m



# Ciclo de luz

normal1212_m <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 normal" & species == "mice"), data = Efeito)
normal1212_m

doze_doze_m <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12" & species == "mice"), data = Efeito)
doze_doze_m

inverso_m <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 reverse" & species == "mice"), data = Efeito)
inverso_m

cycle_NA_m <- rma(yi, vi, subset = (bioterium_lightcycle == "NA" & species == "mice"), data = Efeito)
cycle_NA_m 

natural_m <- rma(yi, vi, subset = (bioterium_lightcycle == "natural" & species == "mice"), data = Efeito)
natural_m # k < 3

dez_q_m <- rma(yi, vi, subset = (bioterium_lightcycle == "10/14" & species == "mice"), data = Efeito)
dez_q_m # k < 3


#especie [rat] ----

Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
Teste_rat

#sexo


Teste_macho_r <- rma(yi, vi, subset = (sex == "M" & species == "rat"), data = Efeito)
Teste_macho_r

Teste_femea_r <- rma(yi, vi, subset = (sex == "F" & species == "rat"), data = Efeito)
Teste_femea_r

Teste_sexoambos_r <- rma(yi, vi, subset = (sex == "M and F" & species == "rat"), data = Efeito)
Teste_sexoambos_r

Efeito %>% 
  filter(sex == "M and F" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_sexoind_r <- rma(yi, vi, subset = (sex == "NA" & species == "rat"), data = Efeito)
Teste_sexoind_r

Efeito %>% 
  filter(sex == "NA" & species == "rat") %>% 
  select(authors) # mesma publicação?

#linhagem

Teste_wistar <- rma(yi, vi, subset = (strain == "wistar" & species == "rat"), data = Efeito)
Teste_wistar

Teste_sd <- rma(yi, vi, subset = (strain == "sprague dawley" & species == "rat"), data = Efeito)
Teste_sd

Teste_LE <- rma(yi, vi, subset = (strain == "long-evans" & species == "rat"), data = Efeito)
Teste_LE

Efeito %>% 
  filter(strain == "long-evans" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_FS <- rma(yi, vi, subset = (strain == "flinders sensitive" & species == "rat"), data = Efeito)
Teste_FS

Efeito %>% 
  filter(strain == "flinders sensitive" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_CDCOBS <- rma(yi, vi, subset = (strain == "CD-COBS" & species == "rat"), data = Efeito)
Teste_CDCOBS

Efeito %>% 
  filter(strain == "CD-COBS" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_WK <- rma(yi, vi, subset = (strain == "wistar kyoto" & species == "rat"), data = Efeito)
Teste_WK 

Efeito %>% 
  filter(strain == "wistar kyoto" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_FR <- rma(yi, vi, subset = (strain == "flinders resistant" & species == "rat"), data = Efeito)
Teste_FR

Efeito %>% 
  filter(strain == "flinders resistant" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_BN <- rma(yi, vi, subset = (strain == "brown norway" & species == "rat"), data = Efeito)
Teste_BN # k < 3

Teste_NA_r <- rma(yi, vi, subset = (strain == "NA" & species == "rat"), data = Efeito)
Teste_NA_r # k < 3

Teste_CD1 <- rma(yi, vi, subset = (strain == "CD-1" & species == "rat"), data = Efeito)
Teste_CD1 # k < 3


# estresse 

Teste_stress_r <- rma(yi, vi, subset = (model_phenotype != "NA" & species == "rat"), data = Efeito)
Teste_stress_r

Teste_nostress_r <- rma(yi, vi, subset = (model_phenotype == "NA" & species == "rat"), data = Efeito)
Teste_nostress_r


# Ciclo de luz

normal1212_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 normal" & species == "rat"), data = Efeito)
normal1212_r

doze_doze_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12" & species == "rat"), data = Efeito)
doze_doze_r

cycle_NA_r <- rma(yi, vi, subset = (bioterium_lightcycle == "NA" & species == "rat"), data = Efeito)
cycle_NA_r 

Efeito %>% 
  filter(bioterium_lightcycle == "NA" & species == "rat") %>% 
  select(authors) # mesma publicação?

natural_r <- rma(yi, vi, subset = (bioterium_lightcycle == "natural" & species == "rat"), data = Efeito)
natural_r 

inverso_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 reverse" & species == "rat"), data = Efeito)
inverso_r

Efeito %>% 
  filter(bioterium_lightcycle == "12/12 reverse" & species == "rat") %>% 
  select(authors) # mesma publicação?

dez_q_r <- rma(yi, vi, subset = (bioterium_lightcycle == "10/14" & species == "rat"), data = Efeito)
dez_q_r 

Efeito %>% 
  filter(bioterium_lightcycle == "10/14" & species == "rat") %>% 
  select(authors) # mesma publicação?


# [INTERVENÇÃO]------ 
# [mice] -----

#TCA

Teste_TCA_m <- rma(yi, vi, subset = (atd_class == "tricyclic" & species == "mice"), data = Efeito)
Teste_TCA_m

Teste_imi_m <- rma(yi, vi, subset = (atd_type == "imipramine" & species == "mice"), data = Efeito)
Teste_imi_m

Teste_des_m <- rma(yi, vi, subset = (atd_type == "desipramine" & species == "mice"), data = Efeito)
Teste_des_m

Teste_ami_m <- rma(yi, vi, subset = (atd_type == "amitriptyline" & species == "mice"), data = Efeito)
Teste_ami_m

Teste_clo_m <- rma(yi, vi, subset = (atd_type == "clomipramine" & species == "mice"), data = Efeito)
Teste_clo_m

Efeito %>% 
  filter(atd_type == "clomipramine" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_nor_m <- rma(yi, vi, subset = (atd_type == "nortriptyline" & species == "mice"), data = Efeito)
Teste_nor_m

Efeito %>% 
  filter(atd_type == "nortriptyline" & species == "mice") %>% 
  select(authors) # mesma publicação?

#SSRI

Teste_SSRI_m <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "mice"), data = Efeito)
Teste_SSRI_m

Teste_flu_m <- rma(yi, vi, subset = (atd_type == "fluoxetine" & species == "mice"), data = Efeito)
Teste_flu_m

Teste_par_m <- rma(yi, vi, subset = (atd_type == "paroxetine" & species == "mice"), data = Efeito)
Teste_par_m

Efeito %>% 
  filter(atd_type == "paroxetine" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_esc_m <- rma(yi, vi, subset = (atd_type == "escitalopram" & species == "mice"), data = Efeito)
Teste_esc_m

Efeito %>% 
  filter(atd_type == "escitalopram" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_cit_m <- rma(yi, vi, subset = (atd_type == "citalopram" & species == "mice"), data = Efeito)
Teste_cit_m

Efeito %>% 
  filter(atd_type == "citalopram" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_fluv_m <- rma(yi, vi, subset = (atd_type == "fluvoxamine" & species == "mice"), data = Efeito)
Teste_fluv_m

Efeito %>% 
  filter(atd_type == "fluvoxamine" & species == "mice") %>% 
  select(authors) # mesma publicação?

#SNRI

Teste_SNRI_m <- rma(yi, vi, subset = (atd_class == "SNRI" & species == "mice"), data = Efeito, control = list(stepadj = 0.5, maxiter = 1000)) # add parametros para ajustar comprimento do passo e maximo de iterações para a convergencia ocorrer (https://stackoverflow.com/questions/68817204/why-did-the-fisher-scoring-algorithm-not-converge-after-adjusting)
Teste_SNRI_m

Teste_ven_m <- rma(yi, vi, subset = (atd_type == "venlafaxine" & species == "mice"), data = Efeito)
Teste_ven_m

Efeito %>% 
  filter(atd_type == "venlafaxine" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_tra_m <- rma(yi, vi, subset = (atd_type == "tramadol" & species == "mice"), data = Efeito)
Teste_tra_m

Efeito %>% 
  filter(atd_type == "tramadol" & species == "mice") %>% 
  select(authors) # mesma publicação?

#IMAO

Teste_IMAO_m <- rma(yi, vi, subset = (atd_class == "IMAO" & species == "mice"), data = Efeito)
Teste_IMAO_m

Teste_sel_m <- rma(yi, vi, subset = (atd_type == "selegiline" & species == "mice"), data = Efeito)
Teste_sel_m

Efeito %>% 
  filter(atd_type == "selegiline" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_moc_m <- rma(yi, vi, subset = (atd_type == "moclobemide" & species == "mice"), data = Efeito)
Teste_moc_m

Efeito %>% 
  filter(atd_type == "moclobemide" & species == "mice") %>% 
  select(authors) # mesma publicação?

#NDRI

Teste_NDRI_m <- rma(yi, vi, subset = (atd_class == "NDRI" & species == "mice"), data = Efeito)
Teste_NDRI_m

#TECA

Teste_TeCA_m <- rma(yi, vi, subset = (atd_class == "teca" & species == "mice"), data = Efeito)
Teste_TeCA_m

Teste_map_m <- rma(yi, vi, subset = (atd_type == "maprotiline" & species == "mice"), data = Efeito)
Teste_map_m

Efeito %>% 
  filter(atd_type == "maprotiline" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_mia_m <- rma(yi, vi, subset = (atd_type == "mianserin" & species == "mice"), data = Efeito)
Teste_mia_m

Efeito %>% 
  filter(atd_type == "mianserin" & species == "mice") %>% 
  select(authors) # mesma publicação?

# VIA adm

Teste_IP_m <- rma(yi, vi, subset = (treatment_via == "IP" & species == "mice"), data = Efeito)
Teste_IP_m

Teste_oral_m <- rma(yi, vi, subset = (treatment_via == "oral" & species == "mice"), data = Efeito)
Teste_oral_m

Teste_gav_m <- rma(yi, vi, subset = (treatment_via == "gavage" & species == "mice"), data = Efeito)
Teste_gav_m

Teste_subc_m <- rma(yi, vi, subset = (treatment_via == "subcutaneous" & species == "mice"), data = Efeito)
Teste_subc_m

Teste_viaNA_m <- rma(yi, vi, subset = (treatment_via == "NA" & species == "mice"), data = Efeito)
Teste_viaNA_m

Efeito %>% 
  filter(treatment_via == "NA" & species == "mice") %>% 
  select(authors) # mesma publicação?

# [rat] -----
#TCA

Teste_TCA_r <- rma(yi, vi, subset = (atd_class == "tricyclic" & species == "rat"), data = Efeito)
Teste_TCA_r

Teste_imi_r <- rma(yi, vi, subset = (atd_type == "imipramine" & species == "rat"), data = Efeito)
Teste_imi_r

Teste_des_r <- rma(yi, vi, subset = (atd_type == "desipramine" & species == "rat"), data = Efeito)
Teste_des_r

Teste_ami_r <- rma(yi, vi, subset = (atd_type == "amitriptyline" & species == "rat"), data = Efeito)
Teste_ami_r

Teste_clo_r <- rma(yi, vi, subset = (atd_type == "clomipramine" & species == "rat"), data = Efeito)
Teste_clo_r

Efeito %>% 
  filter(atd_type == "clomipramine" & species == "rat") %>% 
  select(authors) # mesma publicação?


#SSRI

Teste_SSRI_r <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "rat"), data = Efeito)
Teste_SSRI_r

Teste_flu_r <- rma(yi, vi, subset = (atd_type == "fluoxetine" & species == "rat"), data = Efeito)
Teste_flu_r

Teste_ser_r <- rma(yi, vi, subset = (atd_type == "sertraline" & species == "rat"), data = Efeito)
Teste_ser_r

Teste_par_r <- rma(yi, vi, subset = (atd_type == "paroxetine" & species == "rat"), data = Efeito)
Teste_par_r

Efeito %>% 
  filter(atd_type == "paroxetine" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_fluv_r <- rma(yi, vi, subset = (atd_type == "fluvoxamine" & species == "rat"), data = Efeito)
Teste_fluv_r

Efeito %>% 
  filter(atd_type == "fluvoxamine" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_cit_r <- rma(yi, vi, subset = (atd_type == "citalopram" & species == "rat"), data = Efeito)
Teste_cit_r

Efeito %>% 
  filter(atd_type == "citalopram" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_esc_r <- rma(yi, vi, subset = (atd_type == "escitalopram" & species == "rat"), data = Efeito)
Teste_esc_r

Efeito %>% 
  filter(atd_type == "escitalopram" & species == "rat") %>% 
  select(authors) # mesma publicação?


#SNRI

Teste_SNRI_r <- rma(yi, vi, subset = (atd_class == "SNRI" & species == "rat"), data = Efeito)
Teste_SNRI_r

Teste_ven_r <- rma(yi, vi, subset = (atd_type == "venlafaxine" & species == "rat"), data = Efeito)
Teste_ven_r

Teste_desv_r <- rma(yi, vi, subset = (atd_type == "desvenlafaxine" & species == "rat"), data = Efeito)
Teste_desv_r

Efeito %>% 
  filter(atd_type == "desvenlafaxine" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_reb_r <- rma(yi, vi, subset = (atd_type == "reboxetine" & species == "rat"), data = Efeito)
Teste_reb_r

Efeito %>% 
  filter(atd_type == "reboxetine" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_sib_r <- rma(yi, vi, subset = (atd_type == "sibutramine" & species == "rat"), data = Efeito)
Teste_sib_r

Efeito %>% 
  filter(atd_type == "sibutramine" & species == "rat") %>% 
  select(authors) # mesma publicação?

#TECA

Teste_TeCA_r <- rma(yi, vi, subset = (atd_class == "teca" & species == "rat"), data = Efeito)
Teste_TeCA_r

Teste_mia_r <- rma(yi, vi, subset = (atd_type == "mianserin" & species == "rat"), data = Efeito)
Teste_mia_r

Teste_amo_r <- rma(yi, vi, subset = (atd_type == "amoxapine" & species == "rat"), data = Efeito)
Teste_amo_r


#IMAO

Teste_IMAO_r <- rma(yi, vi, subset = (atd_class == "IMAO" & species == "rat"), data = Efeito)
Teste_IMAO_r


# VIA adm

Teste_IP_r <- rma(yi, vi, subset = (treatment_via == "IP" & species == "rat"), data = Efeito)
Teste_IP_r

Teste_oral_r <- rma(yi, vi, subset = (treatment_via == "oral" & species == "rat"), data = Efeito)
Teste_oral_r

Teste_subc_r <- rma(yi, vi, subset = (treatment_via == "subcutaneous" & species == "rat"), data = Efeito)
Teste_subc_r

Teste_gav_r <- rma(yi, vi, subset = (treatment_via == "gavage" & species == "rat"), data = Efeito)
Teste_gav_r

Teste_mi_r <- rma(yi, vi, subset = (treatment_via == "microinjection (dorsal hippocampus)" & species == "rat"), data = Efeito)
Teste_mi_r


Efeito %>% 
  filter(treatment_via == "microinjection (dorsal hippocampus)" & species == "rat") %>% 
  select(authors) # mesma publicação?


Teste_od_r <- rma(yi, vi, subset = (treatment_via == "oral (dietary treatment)" & species == "rat"), data = Efeito)
Teste_od_r

Efeito %>% 
  filter(treatment_via == "oral (dietary treatment)" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_in_r <- rma(yi, vi, subset = (treatment_via == "intranasal" & species == "rat"), data = Efeito)
Teste_in_r


Efeito %>% 
  filter(treatment_via == "intranasal" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_viaNA_r <- rma(yi, vi, subset = (treatment_via == "NA" & species == "rat"), data = Efeito)
Teste_viaNA_r # k < 3

# [OUTCOME] ------
# [mice]----

#protocol


fst_pro_m <- Efeito %>% 
  filter(species == "mice") %>% 
  group_by(fst_protocol) %>% 
  summarise(soma = n()) %>% 
  arrange(desc(soma)) # ver quais protocolos foram usados e deixar só com pelo menos 3 estudos


Teste_T6S4_m <- rma(yi, vi, subset = (fst_protocol == "test6score4" & species == "mice"), data = Efeito)
Teste_T6S4_m

Teste_T6_m <- rma(yi, vi, subset = (fst_protocol == "test6" & species == "mice"), data = Efeito)
Teste_T6_m

Teste_PT15T6S4_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6score4" & species == "mice"), data = Efeito)
Teste_PT15T6S4_m

Teste_PT15T6_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6" & species == "mice"), data = Efeito)
Teste_PT15T6_m

Teste_PT15T5_m <- rma(yi, vi, subset = (fst_protocol == "pre15test5" & species == "mice"), data = Efeito)
Teste_PT15T5_m

Efeito %>% 
  filter(fst_protocol == "pre15test5" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T5_m <- rma(yi, vi, subset = (fst_protocol == "test5" & species == "mice"), data = Efeito)
Teste_T5_m

Efeito %>% 
  filter(fst_protocol == "test5" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_PT5T5_m <- rma(yi, vi, subset = (fst_protocol == "pre5test5" & species == "mice"), data = Efeito)
Teste_PT5T5_m

Efeito %>% 
  filter(fst_protocol == "pre5test5" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T5S4_m <- rma(yi, vi, subset = (fst_protocol == "test5score4" & species == "mice"), data = Efeito)
Teste_T5S4_m

Efeito %>% 
  filter(fst_protocol == "test5score4" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T7S6_m <- rma(yi, vi, subset = (fst_protocol == "test7score6" & species == "mice"), data = Efeito)
Teste_T7S6_m

Efeito %>% 
  filter(fst_protocol == "test7score6" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T6S5_m <- rma(yi, vi, subset = (fst_protocol == "test6score5" & species == "mice"), data = Efeito)
Teste_T6S5_m

Efeito %>% 
  filter(fst_protocol == "test6score5" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T9_m <- rma(yi, vi, subset = (fst_protocol == "test9" & species == "mice"), data = Efeito)
Teste_T9_m

Efeito %>% 
  filter(fst_protocol == "test9" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_PT15T6S5_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6score5" & species == "mice"), data = Efeito)
Teste_PT15T6S5_m

Efeito %>% 
  filter(fst_protocol == "pre15test6score5" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T10_m <- rma(yi, vi, subset = (fst_protocol == "test10" & species == "mice"), data = Efeito)
Teste_T10_m

Efeito %>% 
  filter(fst_protocol == "test10" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_PT15TNA_m <- rma(yi, vi, subset = (fst_protocol == "pre15test?" & species == "mice"), data = Efeito)
Teste_PT15TNA_m

Efeito %>% 
  filter(fst_protocol == "pre15test?" & species == "mice") %>% 
  select(authors) # mesma publicação?

Teste_T15S5_m <- rma(yi, vi, subset = (fst_protocol == "test15score5" & species == "mice"), data = Efeito)
Teste_T15S5_m

Efeito %>% 
  filter(fst_protocol == "test15score5" & species == "mice") %>% 
  select(authors) # mesma publicação?

# method

Teste_metNA_m <- rma(yi, vi, subset = (measurement_method == "NA" & species == "mice"), data = Efeito)
Teste_metNA_m

Teste_metvideo_m <- rma(yi, vi, subset = (measurement_method == "video analysis" & species == "mice"), data = Efeito)
Teste_metvideo_m

Teste_metmanual_m <- rma(yi, vi, subset = (measurement_method == "manually" & species == "mice"), data = Efeito)
Teste_metmanual_m

# [rat] ----

#protocol

fst_pro_r <- Efeito %>% 
  filter(species == "rat") %>% 
  group_by(fst_protocol) %>% 
  summarise(soma = n()) %>% 
  arrange(desc(soma)) # ver quais protocolos foram usados e deixar só com pelo menos 3 estudos


Teste_PT15T5_r <- rma(yi, vi, subset = (fst_protocol == "pre15test5" & species == "rat"), data = Efeito)
Teste_PT15T5_r

Teste_T5_r <- rma(yi, vi, subset = (fst_protocol == "test5" & species == "rat"), data = Efeito)
Teste_T5_r

Teste_PT13T6_r <- rma(yi, vi, subset = (fst_protocol == "pre13test6" & species == "rat"), data = Efeito)
Teste_PT13T6_r

Efeito %>% 
  filter(fst_protocol == "pre13test6" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_PTNAT6S4_r <- rma(yi, vi, subset = (fst_protocol == "pre?test6score4" & species == "rat"), data = Efeito)
Teste_PTNAT6S4_r

Efeito %>% 
  filter(fst_protocol == "pre?test6score4" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_PT5T5_r <- rma(yi, vi, subset = (fst_protocol == "pre5test5" & species == "rat"), data = Efeito)
Teste_PT5T5_r

Efeito %>% 
  filter(fst_protocol == "pre5test5" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_T15_r <- rma(yi, vi, subset = (fst_protocol == "test15" & species == "rat"), data = Efeito)
Teste_T15_r

Efeito %>% 
  filter(fst_protocol == "test15" & species == "rat") %>% 
  select(authors) # mesma publicação?

Teste_PT15T6S5_r <- rma(yi, vi, subset = (fst_protocol == "pre15test6score5" & species == "rat"), data = Efeito)
Teste_PT15T6S5_r

Efeito %>% 
  filter(fst_protocol == "pre15test6score5" & species == "rat") %>% 
  select(authors) # mesma publicação?


# method

Teste_metNA_r <- rma(yi, vi, subset = (measurement_method == "NA" & species == "rat"), data = Efeito)
Teste_metNA_r

Teste_metvideo_r <- rma(yi, vi, subset = (measurement_method == "video analysis" & species == "rat"), data = Efeito)
Teste_metvideo_r

Teste_metmanual_r <- rma(yi, vi, subset = (measurement_method == "manually" & species == "rat"), data = Efeito)
Teste_metmanual_r



# [PUBLICATION] ----- 

# revista revisada por pares

rma(yi, vi, subset = (species == "mice" & camarades1 == "Yes"), data = Efeito)

# revista predatoria

rma(yi, vi, subset = (species == "mice" & camarades1 == "Unclear, predatory"), data = Efeito)

# possibilidade para avaliar ano atraves de analise de subgrupo de periodos de tempo - ver com profa

initial_m <- Efeito %>%
  filter(species == "mice",
         year <= "2006-01-01")

final_m <- Efeito %>%
  filter(species == "mice",
         year > "2006-01-01")

Teste_initial_m <- rma(yi, vi, data = initial_m)
Teste_initial_m

Teste_final_m <- rma(yi, vi, data = final_m)
Teste_final_m

# Metaregressao -------------------------------------


# idade  e peso 

png("Fig/Reg_idade_peso.png", height = 800, width = 1200)

metareg_age_c <- rma(yi, vi, subset = species == "mice", mods = ~ age, data = Efeito)
metareg_age_r <- rma(yi, vi, subset = species == "rat", mods = ~ age, data = Efeito)
metareg_peso_c <- rma(yi, vi, subset = species == "mice", mods = ~ weight, data = Efeito)
metareg_peso_r <- rma(yi, vi, subset = species == "rat", mods = ~ weight, data = Efeito)


par(mfrow = c(2, 2), oma = c(0,2,0,1))

regplot(metareg_age_c, xlab = "Idade (dias)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Camundongo", cex.main = 2.3, cex.lab = 2, cex.axis = 1.8)
regplot(metareg_age_r, xlab = "Idade (dias)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Rato", cex.main = 2.3, cex.lab = 2, cex.axis = 1.8)
regplot(metareg_peso_c, xlab = "Peso (g)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 1.8)
regplot(metareg_peso_r, xlab = "Peso (g)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 1.8)

dev.off()

metareg_age_c
metareg_age_r
metareg_peso_c
metareg_peso_r


# dose
png("Fig/Reg_dose.png", height = 400, width = 1000)

metareg_dose_c <- rma(yi, vi, subset = species == "mice" & dose_unit == "mg/kg", mods = ~dose, data = Efeito) 
metareg_dose_r <- rma(yi, vi, subset = species == "rat" & dose_unit == "mg/kg", mods = ~dose, data = Efeito) 

par(mfrow = c(1, 2), oma = c(0,2,0,1))

regplot(metareg_dose_c, xlab = "Dose (mg/kg)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Camundongo", cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.2)
regplot(metareg_dose_r, xlab = "Dose (mg/kg)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Rato", cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.2)

dev.off()

metareg_dose_c
metareg_dose_r


# profundidade agua


png("Fig/Reg_pa.png", height = 400, width = 1000)

metareg_pa_c <- rma(yi, vi, subset = species == "mice", mods = ~ water_depth, data = Efeito)
metareg_pa_r <- rma(yi, vi, subset = species == "rat", mods = ~ water_depth, data = Efeito)

par(mfrow = c(1, 2), oma = c(0,2,0,1))

regplot(metareg_pa_c, xlab = "Profundidade da água (cm)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Camundongo", cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.2)
regplot(metareg_pa_r, xlab = "Profundidade da água (cm)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Rato", cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.2)

dev.off()

metareg_pa_c 
metareg_pa_r

# ano e qualidade

Efeito$rob1 <- ifelse(Efeito$rob1 == 'Unclear', 0, ifelse(Efeito$rob1 == 'Yes', 1, -1)) # tranformar atribuicoes em pontos
Efeito$rob2 <- ifelse(Efeito$rob2 == 'Unclear', 0, ifelse(Efeito$rob2 == 'Yes', 1, -1))
Efeito$rob3 <- ifelse(Efeito$rob3 == 'Unclear', 0, ifelse(Efeito$rob3 == 'Yes', 1, -1))
Efeito$rob4 <- ifelse(Efeito$rob4 == 'Unclear', 0, ifelse(Efeito$rob4 == 'Yes', 1, -1))
Efeito$rob5 <- ifelse(Efeito$rob5 == 'Unclear', 0, ifelse(Efeito$rob5 == 'Yes', 1, -1))
Efeito$rob6 <- ifelse(Efeito$rob6 == 'Unclear', 0, ifelse(Efeito$rob6 == 'Yes', 1, -1))
Efeito$rob7 <- ifelse(Efeito$rob7 == 'Unclear', 0, ifelse(Efeito$rob7 == 'Yes', 1, -1))
Efeito$rob8 <- ifelse(Efeito$rob8 == 'Unclear', 0, ifelse(Efeito$rob8 == 'Yes', 1, -1))
Efeito$rob9 <- ifelse(Efeito$rob9 == 'Unclear', 0, ifelse(Efeito$rob9 == 'Yes', 1, -1))
Efeito$rob10 <- ifelse(Efeito$rob10 == 'Unclear', 0, ifelse(Efeito$rob10 == 'Yes', 1, -1))
                      
Efeito <- Efeito %>% 
  mutate(pont_quali = rob1 + rob2 + rob3 + rob4 + rob5 + rob6 + rob7 + rob8 + rob9 + rob10) # Nova variavel com pontuacao rob

png("Fig/Reg_ano_quali.png", height = 800, width = 1200)

metareg_quali_c <- rma(yi, vi, subset = species == "mice", mods = ~pont_quali, data = Efeito) 
metareg_quali_r <- rma(yi, vi, subset = species == "rat", mods = ~pont_quali, data = Efeito) 
metareg_ano_c <- rma(yi, vi, subset = species == "mice", mods = ~year, data = Efeito) 
metareg_ano_r <- rma(yi, vi, subset = species == "rat", mods = ~year, data = Efeito) 

par(mfrow = c(2, 2), oma = c(0,2,0,1))

regplot(metareg_ano_c, xlab = "Ano", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Camundongo", cex.main = 2.3, cex.lab = 1.8, cex.axis = 1.8)
regplot(metareg_ano_r, xlab = "Ano", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"),  main = "Rato", cex.main = 2.3, cex.lab = 1.8, cex.axis = 1.8)
regplot(metareg_quali_c, xlab = "Pontuação Qualidade (ROB SYRCLE)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 1.8, cex.axis = 1.8)
regplot(metareg_quali_r, xlab = "Pontuação Qualidade (ROB SYRCLE)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 1.8, cex.axis = 1.8, xlim = c(0,7))

dev.off()

metareg_ano_c
metareg_ano_r
metareg_quali_c
metareg_quali_r

# ano x qualidade

metareg_ano_quali <- rma(yi, vi, mods = ~ species + year, data = Efeito) 
metareg_ano_quali

plot_metareg_ano <- regplot(metareg_ano_quali, xlab = "Ano", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"))


