# Referência: ~ref da dissertação~
# ETAPA 1: Calculo do k (estudos) para alcançar poder de pelo menos 80%


# Carregar pacote necessario
library(esc)
library(metapower)

# criar funcao para converter hedges g para cohens d

g_to_d <- function(vg, vn) {
  vd <- vg / (1 - 3 / (4 * (vn) - 9))
  return(vd)
}

# converter hedges g para cohens d
g_to_d(vg = 0.5, vn = 12)

# Confirmar se a conversao está correta
hedges_g(d = 0.54166667, totaln = 12) 


kmestrado <- mpower(effect_size = .54166667, study_size = 6, k = 200, i2 = .90, es_type = "d") # calcular o poder 

print(kmestrado) # mostrar resultados de poder 

png("Fig/poder.png", height = 350, width = 600)

plot_mpower(kmestrado) # plotar grafico do poder

dev.off()

# possivel poder na comparacao de subgrupos para uma diferença de 0.5

subgrupos <- subgroup_power(n_groups = 2, effect_size = c(0.1, 0.6), study_size = 14, k = 200, i2 = .90, es_type = "d") # calcular o poder 
subgrupos
