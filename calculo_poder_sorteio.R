# Referência: ~ref da dissertação~
# Calculo do k estudos para alcançar poder de pelo menos 80%

# Carregar pacote necessario
library(esc)
library(metapower)

hedges_g(d = 0.54166666, totaln = 12) # Confirmar se a conversao manual do cohens d está correta


kmestrado <- mpower(effect_size = .54166666, study_size = 6, k = 200, i2 = .90, es_type = "d") # calcular o poder 

print(kmestrado) # mostrar resultados de poder 

plot_mpower(kmestrado) # plotar grafico do poder

