# Referência: ~ref da dissertação~
# ETAPA 8: Meta-analise, analise de subgrupos, metaregressao, vies de publicacao

# Carregar pacotes


library(bitops)
library(metafor)
library(robumeta)
library(Formula)
library(dplyr)
library(readxl)
library(extrafont)
library(cowplot)

# Carregar planilha

df <- read_excel("data/Data_200FST.xlsx")


df <- df %>% 
  mutate(year = format(as.Date(Efeito$year, format="%d/%m/%Y"),"%Y"))

# provisório: coloquei valor "1" nos n corrigidos que deram "0"

df <- df %>% 
  mutate(ctr_n_corr = replace(ctr_n_corr, ctr_n_corr == 0, 1))




# Calcular tamanho de efeito em SDM hedges g


Efeito <- escalc(measure = "SMD", n1i = ctr_n_corr, n2i = atd_n_round, m1i = ctr_mean, m2i = atd_mean, 
                 sd1i = ctr_sd, sd2i = atd_sd, data = df, 
                 append = TRUE)


# Metaanalise por modelo de efeitos aleatorios

Teste <- rma(yi, vi, data = Efeito, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))


Teste


# Gerar intervalo de confianca e predicao 

predict(Teste, digits = 3)


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


# Plot e save forestplot - MELHOR

pdf("floresta_toda.pdf", height = 120, width = 25)

floresta <- forest(
  Teste,
  cex = 1,
  ylim = c(-2, 570),
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
text(c(-6, 8.75),     570, font = 2.5,
     cex = 2, c("Control", "Antidepressant"))
text(c(183),
     570,
     font = 2.5,
     cex = 2,
     c("Weights Hedges g [95% CI]"))
text(c(-40),
     570,
     font = 2.5,
     cex = 2,
     c("Author(s), year"))

text(-50, -1, pos=4, cex=1.3, bquote(paste("RE Model (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
                                           ", df = ", .(Teste$k - Teste$p),
                                           ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
                                           I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
                                           tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))


dev.off() 

# Plotar grafico em floresta salvando ao mesmo tempo - OUTRA FORMA - PARECE NOTA FISCAL

# 
# ks <- c(567)
# 
# for (k in ks) {
#   
#   vi <- runif(k, .05, 1)
#   yi <- rnorm(k, 0, sqrt(vi))
#   
#   
#   png(paste0("forest_k=", formatC(k, format = "f", flag = "0", width = 3, digits = 0), ".png"), height = 200 + 40*k^.85)
# 
#   
#   forest(Teste, xlim = c(-150,200), alim = c(-20, 30), efac = 30/(k + 10), cex = 1, yaxs = "i", ylim = c(0,k + 3), slab = (paste(
#     Efeito$first_author, as.character(Efeito$year), sep = ", "
#   )),
#   mlab = paste("RE Model for ALL Studies", bquote(paste(.(text),
#                                                           " (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
#                                                           ", df = ", .(Teste$k - Teste$p),
#                                                           ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
#                                                           I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
#                                                           tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")"))),
#   order = Efeito$yi,
#   showweight = T,
#   xlab = "Hedges g")
#   
#   text(-150, k + 2, "Author(s), year", pos = 4)
#   text( 200, k + 2, "Weights Hedges g [95% CI]", pos = 2)
#   text(-50, -3, pos=4, cex=1.3, bquote(paste("RE Model (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
#                                              ", df = ", .(Teste$k - Teste$p),
#                                              ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
#                                              I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
#                                              tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))
#   dev.off()
#   
# }


# Analise de subgrupos

#especie

Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)

Teste_rat
Teste_mice


#sexo

Teste_femea <- rma(yi, vi, subset = (sex == "F"), data = Efeito)
Teste_macho <- rma(yi, vi, subset = (sex == "M"), data = Efeito)
Teste_sexoind <- rma(yi, vi, subset = (sex == "NA"), data = Efeito)
Teste_sexoambos <- rma(yi, vi, subset = (sex == "M and F"), data = Efeito)

Teste_femea
Teste_macho
Teste_sexoind
Teste_sexoambos


# sexo e especie

Teste_femea_m <- rma(yi, vi, subset = (sex == "F" & species == "mice"), data = Efeito)
Teste_femea_r <- rma(yi, vi, subset = (sex == "F" & species == "rat"), data = Efeito)
Teste_macho_m <- rma(yi, vi, subset = (sex == "M" & species == "mice"), data = Efeito)
Teste_macho_r <- rma(yi, vi, subset = (sex == "M" & species == "rat"), data = Efeito)
Teste_sexoind_m <- rma(yi, vi, subset = (sex == "NA" & species == "mice"), data = Efeito)
Teste_sexoind_r <- rma(yi, vi, subset = (sex == "NA" & species == "rat"), data = Efeito)
Teste_sexoambos_m <- rma(yi, vi, subset = (sex == "M and F" & species == "mice"), data = Efeito)
Teste_sexoambos_r <- rma(yi, vi, subset = (sex == "M and F" & species == "rat"), data = Efeito)

Teste_femea_m 
Teste_femea_r 
Teste_macho_m 
Teste_macho_r 
Teste_sexoind_m 
Teste_sexoind_r 
Teste_sexoambos_m 
Teste_sexoambos_r 


# estresse 

Teste_nostress <- rma(yi, vi, subset = (model_phenotype == "NA"), data = Efeito)
Teste_nostress_m <- rma(yi, vi, subset = (model_phenotype == "NA" & species == "mice"), data = Efeito)
Teste_nostress_r <- rma(yi, vi, subset = (model_phenotype == "NA" & species == "rat"), data = Efeito)
Teste_stress <- rma(yi, vi, subset = (model_phenotype != "NA"), data = Efeito)
Teste_stress_m <- rma(yi, vi, subset = (model_phenotype != "NA" & species == "mice"), data = Efeito)
Teste_stress_r <- rma(yi, vi, subset = (model_phenotype != "NA" & species == "rat"), data = Efeito)

Teste_nostress
Teste_nostress_m
Teste_nostress_r
Teste_stress
Teste_stress_m
Teste_stress_r

# Linhagens

Teste_m_swiss <- rma(yi, vi, subset = (strain == "swiss" & species == "mice"), data = Efeito)
Teste_m_cd1 <- rma(yi, vi, subset = (strain == "CD-1" & species == "mice"), data = Efeito)
Teste_m_c57bl <- rma(yi, vi, subset = (strain == "C57BL" & species == "mice"), data = Efeito)

Teste_m_swiss 
Teste_m_cd1 
Teste_m_c57bl 


Teste_r_wistar <- rma(yi, vi, subset = (strain == "wistar" & species == "rat"), data = Efeito)
Teste_r_spragued <- rma(yi, vi, subset = (strain == "sprague dawley" & species == "rat"), data = Efeito)
Teste_r_wistar 
Teste_r_spragued 


# classe

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

