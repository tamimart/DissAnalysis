# Referência: ~ref da dissertação~
# ETAPA 8: Meta-analise, analise de subgrupos, metaregressao, vies de publicacao

# Carregar pacotes


library(bitops)    # operadores
library(metafor)   # pacota para rodar meta-análise
library(Formula)
library(dplyr)     # manipulacao dos dados
library(readxl)    # ler arquivo do excel
library(extrafont) # fonte extra
library(cowplot)   # alinhamento e anotacao plot
library(metapower) # calculo de poder
library(esc)       # calcular tamano de efeito


# Carregar planilha

df <- read_excel("data/Data_200FST.xlsx")


df <- df %>% 
  mutate(year = format(as.Date(df$year, format = "%d/%m/%Y"),"%Y")) # mudar tipo da data

# criei funcao para converter hedges g para cohens d

g_to_d <- function(vg, vn) {
  vd <- vg / (1 - 3 / (4 * (vn + vn) - 9))
  return(vd)
}


# Calcular tamanho de efeito em SDM hedges g


Efeito <- escalc(measure = "SMD", n1i = ctr_n_corr, n2i = atd_n_round, m1i = ctr_mean, m2i = atd_mean, 
                 sd1i = ctr_sd, sd2i = atd_sd, data = df, 
                 append = TRUE)


# Metaanalise por modelo de efeitos aleatorios

Teste <- rma(yi, vi, data = Efeito, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))


Teste

# Gerar intervalo de confianca e predicao 

predict(Teste, digits = 3)

# Calculo do poder geral

df %>% 
  summarize(mean(N)) #obter N total

g_to_d(vg = 1.7483, vn = 7.8) # converter hedges g para cohens d (sera usado no mpower())

poder_geral <- mpower(effect_size = 1.852365, study_size = 7.8, k = 561, i2 = 0.83, es_type = "d") # calcular o poder 

print(poder_geral)
plot_mpower(poder_geral)


# Plot e save forestplot - MELHOR

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


# # DETALHES SOBRE INFLUENCIA DOS ESTUDOS - Ver que estudos estao influenciando em diversos aspectos -video Quintana
# 
baujat.rma(Teste, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))
# 
# 
# sav <- baujat(Teste, symbol=19, xlim=c(0,20), slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")) # Destacar papers com influencia "extrema" sobre ES e heterogeneidade
# sav <- sav[sav$x >= 10 | sav$y >= 0.10,]
# text(sav$x, sav$y, sav$slab, pos=1)
# 
# 
# 
# inf <- influence(Teste)
# print(inf)
# plot(inf)

# Vies de publicação

# [gráfico de funil]

# [eggers regression]

# [trim and fill]

# [weight function model]




# Analise de subgrupos


#[POPULAÇÃO]

#especie [mice]

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

Teste_B6SJL <- rma(yi, vi, subset = (strain == "B6SJL" & species == "mice"), data = Efeito)
Teste_B6SJL # k < 3
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


#especie [rat]

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

Teste_FR <- rma(yi, vi, subset = (strain == "flinders resistent" & species == "rat"), data = Efeito)
Teste_FR # k < 3

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

inverso_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 reverse" & species == "rat"), data = Efeito)
inverso_r

Efeito %>% 
  filter(bioterium_lightcycle == "12/12 reverse" & species == "rat") %>% 
  select(authors) # mesma publicação?

cycle_NA_r <- rma(yi, vi, subset = (bioterium_lightcycle == "NA" & species == "rat"), data = Efeito)
cycle_NA_r 

Efeito %>% 
  filter(bioterium_lightcycle == "NA" & species == "rat") %>% 
  select(authors) # mesma publicação?

natural_r <- rma(yi, vi, subset = (bioterium_lightcycle == "natural" & species == "rat"), data = Efeito)
natural_r 

dez_q_r <- rma(yi, vi, subset = (bioterium_lightcycle == "10/14" & species == "mice"), data = Efeito)
dez_q_r # k < 3



# [INTERVENÇÃO]

Teste_TCA <- rma(yi, vi, subset = (atd_class == "tricyclic"), data = Efeito)
Teste_TCA_m <- rma(yi, vi, subset = (atd_class == "tricyclic" & species == "mice"), data = Efeito)
Teste_TCA_r <- rma(yi, vi, subset = (atd_class == "tricyclic" & species == "rat"), data = Efeito)


Teste_SSRI <- rma(yi, vi, subset = (atd_class == "SSRI"), data = Efeito)
Teste_SSRI_m <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "mice"), data = Efeito)
Teste_SSRI_r <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "rat"), data = Efeito)

Teste_TCA 
Teste_SSRI 
Teste_TCA_r
Teste_TCA_m
Teste_SSRI_m
Teste_SSRI_r

# top dois farmacos de cada classe 


# Metaregressao

metareg_dose <- rma(yi, vi, mods = ~last_bf_outcome, data = Efeito) 
metareg_dose


# Salvar todos resultados

save(list = c("Teste"), file = "Metaresult.txt")

