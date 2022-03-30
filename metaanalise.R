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
# baujat.rma(Teste, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", "))
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



# Plotar grafico em floresta salvando ao mesmo tempo 


ks <- c(567)

for (k in ks) {
  
  vi <- runif(k, .05, 1)
  yi <- rnorm(k, 0, sqrt(vi))
  
  
  png(paste0("forest_k=", formatC(k, format = "f", flag = "0", width = 3, digits = 0), ".png"), height = 200 + 40*k^.85)

  
  forest(Teste, xlim = c(-150,200), alim = c(-20, 30), efac = 30/(k + 10), cex = 1, yaxs = "i", ylim = c(0,k + 3), slab = (paste(
    Efeito$first_author, as.character(Efeito$year), sep = ", "
  )),
  mlab = paste("RE Model for ALL Studies", bquote(paste(.(text),
                                                          " (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
                                                          ", df = ", .(Teste$k - Teste$p),
                                                          ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
                                                          I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
                                                          tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")"))),
  order = Efeito$yi,
  showweight = T,
  xlab = "Hedges g")
  
  text(-150, k + 2, "Author(s), year", pos = 4)
  text( 200, k + 2, "Weights Hedges g [95% CI]", pos = 2)
  text(-50, -3, pos=4, cex=1.3, bquote(paste("RE Model (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
                                             ", df = ", .(Teste$k - Teste$p),
                                             ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
                                             I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
                                             tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))
  dev.off()
  
}


# Outra forma - MELHOR


png("floresta.png", width = 2000, height = 4000)


floresta <- forest(
  Teste,
  cex = .6,
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
  fonts = "Gadugi"
)

# Adicionar textos


op <- par(cex = 0.75, font = 2, family = "Gadugi")
 text(c(-6, 8.75),     572, font = 2.5,
      cex = 2, c("Control", "Antidepressant"))
 text(c(185),
     572,
     font = 2.5,
     cex = 2,
     c("Weights Hedges g [95% CI]"))
 text(c(-40),
     572,
     font = 2.5,
     cex = 2,
     c("Author(s), year"))
 
 text(-50, -3, pos = 4, cex = 1.3, bquote(paste("RE Model (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
              ", df = ", .(Teste$k - Teste$p),
              ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
              I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
              tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))


dev.off() 



# Analise de subgrupos#

Teste_F <- rma(yi, vi, subset = (sex == "F"), data = Efeito)
Teste_M <- rma(yi, vi, subset = (sex == "M"), data = Efeito)
Teste_semsexo <- rma(yi, vi, subset = (sex == "NA"), data = Efeito)
Teste_ambos <- rma(yi, vi, subset = (sex == "M and F"), data = Efeito)

# Metaregressao

metareg_dose <- rma(yi, vi, mods = ~dose, data = Efeito) 
metareg_dose

# Salvar todos resultados

save(list = c("Teste", "Teste_F", "Teste_M", "Teste_semsexo", "Teste_ambos"), file = "Metaresult.RData")

