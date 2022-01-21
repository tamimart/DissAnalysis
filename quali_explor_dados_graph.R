# Referência: ~ref da dissertação~

# ETAPA 3: Visualização dos dados - Gráficos
# Carregar pacotes

library(tidyverse)
library(summarytools)
library(patchwork)
library(gghighlight)
library(MetBrewer)
library(ggtext)
library(cowplot)
library(ggrepel)
library(ggmap)


library(ggcorrplot)


library(ggradar)


# Carregar dataframe dos dados limpos e organizados

df <- data_geral_clean <- readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")

# Gráficos das variaveis -------


# Figuras variaveis fator

# Um exemplo

# df %>% ggplot(aes(x = year, y = n, color = sex)) +
#   geom_line(size = 1, alpha = 0.8) +
#   labs(y = "Frequência", x = "Ano", 
#        title = "Número de estudos por sexo ao decorrrer do tempo", 
#        color = "Sexo") +
#     theme_classic() +
#     scale_color_manual(values=met.brewer("Cross", 2))




theme_set(theme_minimal(base_family = "serif")) # Estabelecer tema e fonte para todos gráficos

# Renomear os níveis

df$species <- factor(df$species, levels = c("mice", "rat"), 
                     labels = c("Camundongo", "Rato")) # Ordenar niveis do fator espécie

df$sex <- factor(df$sex, levels = c("M", "F", "M and F", "NA"), 
                 labels = c("Macho", "Fêmea", "Ambos", "Sem info")) # Ordenar niveis do fator sexo

df$model_phenotype <- factor(df$model_phenotype, 
                             levels = c("1 saline injection for 4 days",                
                                        "ACTH (100microg)",                             
                                        "amphetamine withdrawal",                       
                                        "antidepressant withdrawal",                    
                                        "Bacillus Calmette–Guérin (BCG)",               
                                        "CMS",                                          
                                        "CUMs",                                         
                                        "CUS" ,                                         
                                        "depressed",                                   
                                        "forced swim",                                  
                                        "FS and CMS",                                   
                                        "high emotional",                               
                                        "Isolation-Rearing",                            
                                        "low emotional",                                
                                        "LPS",                                          
                                        "maternal-separation",                          
                                        "melanin-concentrating hormone 50ng",           
                                        "mother exposed to Chlorpyrifos (CPF)",         
                                        "mother exposed to DDT",                        
                                        "NA",                                           
                                        "olfactory bulbectomy",                         
                                        "ovariectomized",                               
                                        "ovariectomized +1 saline injection for 4 days",
                                        "pentylenetetrazol-kindled seizures",           
                                        "prenatal stress procedure",                    
                                        "PTSD-like",                                    
                                        "reserpine",                                    
                                        "restraint-stress",                             
                                        "streptozotocin",                               
                                        "stroke (Middle Cerebral Artery occlusion)",    
                                        "subchronic stress:restraint–water immersion",  
                                        "temporal lobe epilepsy (pilocarpine)",         
                                        "wheel running + restraint-stress"), 
                             labels = c("Injeção salina por 4 dias",                
                                        "ACTH (100µg)",                             
                                        "Retirada de anfetamina",                       
                                        "Retirada de antidepressivo",                    
                                        "Bacillus Calmette–Guérin (BCG)",               
                                        "CMS - Estresse leve crônico",                                          
                                        "CUMs - Estresse leve imprevisível crônico",                                         
                                        "CUS - Estresse imprevisível crônico" ,                                         
                                        "Deprimido",                                   
                                        "Natação forçada",                                  
                                        "Natação forçada + Estresse leve crônico",                                   
                                        "Emocional alto",                               
                                        "Isolamento",                            
                                        "Emocional baixo",                                
                                        "Lipopolissacarídeo",                                          
                                        "Separação maternal",                          
                                        "Hormônio concentrador de melanina (50ng)",           
                                        "Progenitora exposta à Chlorpyrifos (CPF)",         
                                        "Progenitora exposta à DDT",                        
                                        "NA",                                           
                                        "Bulbectomia olfatória",                         
                                        "Ovacteriomizada",                               
                                        "Ovacteriomizada + injeção salina por 4 dias",
                                        "Convulsões por pentilenotetrazol",           
                                        "Estresse pré-natal",                    
                                        "Tipo TEPT",                                    
                                        "Reserpina",                                    
                                        "Estresse por contenção",                             
                                        "Estreptozotocina",                               
                                        "AVC (por oclusão da artéria cerebral média)",    
                                        "Estresse subcrônico: contenção em água",  
                                        "Epilepsia do lobo temporal (c/ pilocarpina)",         
                                        "Roda de corrida + estresse por contenção"))




#MetBrewer preferidos: Signac, Renoir, Thomas, VanGogh1

# Publicação ----

# Figura0: Paises 


world <- map_data("world")
world <- subset(world, region != "Antarctica")

# criando novo df com as colunas importantes da referencia

countries <- df %>% 
  select(country, year, study_reference, language) %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  group_by(country) %>% 
  summarise(N = n()) %>% 
  mutate(region = country)


# junto base de dados dos paises e meus dados

dados_public <- dplyr::left_join(world, countries, by = "region")

# crio uma colunas com media da lat e long para rótulo

rotulo <- dados_public %>%
  group_by(region) %>%
  summarise(longr = mean(long), latr = mean(lat))

rotulo <- dplyr::left_join(countries, rotulo, by = "region") # Mantenho so os paises presentes no meu df
rotulo <- rotulo %>% 
  select(region, longr, latr, N) # seleciono essas colunas

rotulo$region <- factor(rotulo$region, levels = c("China", "Japan", "USA", "India", "Brazil", "Italy"), 
                        labels = c("China", "Japão","EUA", "Índia", "Brasil", "Italia")) # Renomeio niveis que vao virar rotulo



# 1 - dados
g1 <- ggplot(dados_public, aes(x = long, y = lat, map_id = region))

# 2 - mapa mundo
g2 <- g1 + geom_map(map = dados_public,
                   aes(map_id = region),
                   color="white", size=0.1)

# 3 - Coloração dos países por frequência
g3 <- g2 + geom_map(map = dados_public,
                   aes(map_id = region, fill = N),
                   color="white", size=0.1) + 
  scale_fill_gradientn(colours = met.brewer("Signac", 5),
                      na.value = "grey80",
                      name="Número de publicações",
                      limits=c(0, 30),
                      guide = guide_colourbar(title.position = "top",
                                              barwidth = 7,
                                              barheight = 0.3)) +
  theme(panel.grid=element_blank(), 
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 7),
        legend.position = "top")

# 4 - Estabeleço o tamanho
g4 <- g3 + expand_limits(x = world$long, y = world$lat)

# 5 - Adiciono o rótulo dos países mais frequentes
g5 <- g4 + geom_text_repel(data = subset(rotulo, N >11),
                           aes(x= longr,
                               y = latr,
                               label = region),
                           color = "black", 
                               size = 2,
                               segment.size = 0.3,
                     box.padding = 1,
                     segment.curvature = -0.1,
                     segment.ncp = 3,
                     segment.angle = 20)
g5
save_plot(filename = "Figura0.png", plot = g5, dpi = 300)

# Populacao -----

# Figura1: Sexo, especie, tempo ----


f1a <- df %>% #Freq especie
  group_by(species) %>% 
  summarise(counts = sum(N)) %>% 
  ggplot(aes( x = species, y = counts, fill = species, label = counts)) +
  geom_bar(color = "black", stat="identity", size = .2) +
  geom_text(size = 2.5, family ="serif", position = position_dodge(width=0.9), vjust = -0.25) +
  ylim(0,5397)+
  labs(y = "Nº de animais", x = "Espécie", title = "a") +
  scale_fill_manual(values=met.brewer("Signac", 2))+
  theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=9, hjust = 1),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0))

f1b <- df  %>% # especie no tempo
  ggplot(aes(x = year, fill = species)) +
  geom_bar(color = "black", size = .2)+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  labs(y = "Nº de estudos", x = "Ano", title = "b", fill = "Espécie") + 
  scale_fill_manual(values=met.brewer("Signac", 2)) +
  theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
        axis.title=element_text(size=9, hjust = 1),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,0,0,10))


f1c <- df %>% # Freq sexo
  group_by(sex) %>% 
  summarise(counts = sum(N)) %>% 
  ggplot(aes(x = sex, y = counts, fill = sex, label = counts)) +
  geom_bar(color = "black", stat="identity", size = .2) +
  geom_text(size = 2.5, family ="serif", position = position_dodge(width=0.9), vjust = -0.25) +
  scale_fill_manual(values=c("#692b75", "#006f9f", "#009c7e", "#82c236"))+
  ylim(0, 6906)+ # 300 a mais para caber a anotacao da maior barra
  labs(y = "Nº de animais", x = "Sexo", title = "c") +
  theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=9, hjust = 1),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0))

f1d <- df  %>% # sexo no tempo
  ggplot(aes(x = year, fill = sex, order = sex)) +
  geom_bar(color = "black", size = .2)+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  labs(y = "Nº de estudos", x = "Ano", title = "d") + 
  scale_fill_manual(values=c("#692b75", "#006f9f", "#009c7e", "#82c236")) + # Preferi esse
  theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
        axis.title=element_text(size=9, hjust = 1),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,0,0,10))



# Combinar e salvar


Figura1 <- f1a + f1b + f1c + f1d + plot_layout(heights = c(6, 6), widths = c(3, 6))
Figura1


save_plot(filename = "Figura1.png", plot = Figura1, dpi = 300)

# Figura 2: Linhagens ---- 
# Strain


f2a <- df %>%
  group_by(strain) %>% 
  filter(species == "Camundongo") %>% 
  summarise(counts = n()) %>% 
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(x = factor(strain), y = counts, fill = strain,label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + 
  labs(y = "Nº de estudos", x = "Linhagem", title = "a") +
  scale_y_continuous(n.breaks = 5)+  
  coord_flip() +
  ylim(0,150) +
  gghighlight(counts > 25, calculate_per_facet = TRUE, label_key = strain) +
  scale_fill_manual(values=c("#ffe170", "#fec200", "#ff9400"))+
  geom_text(color = "mintcream", size = 2, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=6.5, angle = 0, color = "grey20"),
        axis.title=element_text(size=8),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic", hjust = 0),
        legend.position = "none",
        plot.margin = margin(10,10,0,10))

f2b <- df %>%
  group_by(strain) %>% 
  filter(species == "Camundongo") %>% 
  summarise(counts = sum(N)) %>% 
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(x = factor(strain), y = counts, fill = strain, label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + 
  labs(y = "Nº de animais", x = "Linhagem", title = "b") + 
  coord_flip() +
  ylim(0,2100) +
  gghighlight(counts > 500, calculate_per_facet = TRUE, label_key = strain) +
  scale_fill_manual(values=c("#ffe170", "#fec200", "#ff9400"))+
  geom_text(color = "mintcream", size = 2, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=6.5, angle = 0, color = "grey20"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title=element_text(size=8),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic", hjust = 0),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,10,0,0))

f2c <- df %>%
  group_by(strain) %>% 
  filter(species == "Rato") %>% 
  summarise(counts = n()) %>% 
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(x = factor(strain), y = counts, fill = strain, label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + 
  labs(y = "Nº de estudos", x = "Linhagem", title = "c") +
  scale_y_continuous(n.breaks = 5)+  
  coord_flip() +
  ylim(0,150) +
  gghighlight(counts > 25, calculate_per_facet = TRUE, label_key = strain) +
  scale_fill_manual(values=c("#ec2b2b", "#a6243a"))+
  geom_text(color = "mintcream", size = 2, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=6.5, angle = 0, color = "grey20"),
        axis.title=element_text(size=8),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic", hjust = 0),
        legend.position = "none",
        plot.margin = margin(0,10,10,10))

f2d <- df %>%
  group_by(strain) %>% 
  filter(species == "Rato") %>% 
  summarise(counts = sum(N)) %>% 
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(x = factor(strain), y = counts, fill = strain, label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + 
  labs(y = "Nº de animais", x = "Linhagem", title = "d") + 
  coord_flip() +
  ylim(0,2100) +
  gghighlight(counts > 500, calculate_per_facet = TRUE, label_key = strain) +
  scale_fill_manual(values=c("#ec2b2b", "#a6243a"))+ 
  geom_text(color = "mintcream", size = 2, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=6.5, angle = 0, color = "grey0"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title=element_text(size=8),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic", hjust = 0),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(0,10,10,0))

Figura2 <- (f2a + f2b + plot_spacer()) / (f2c + f2d + plot_spacer()) +
  plot_layout(heights = c(10, 5.5), guides = 'collect') 


Figura2
save_plot(filename = "Figura2.png", plot = Figura2, dpi = 300)


# Figura3 e 4: Idade e peso ---- 
# Age and weight


# f3a <- df %>% 
#   group_by(species, sex, age) %>% 
#   summarise(counts = sum(N)) %>% 
#   ggplot(aes(x = age, y = counts, fill = sex)) +
#   geom_bar(color = "black")+
#   facet_grid(sex~species, scales = "free_x") +
#   labs(y = "Nº de estudos", x = "Idade (dias)") +
#   scale_x_continuous(n.breaks = 10)+
#   scale_fill_manual(values=met.brewer("Signac", 4))+
#   theme_bw(base_family = "serif")+
#   theme(axis.text=element_text(size=12, angle = 0, color = "grey30"),
#         axis.title=element_text(size=12, hjust = 0),
#         axis.title.y=element_text(margin = margin(r=5)),
#         axis.title.x=element_text(margin = margin(t=5)),
#         legend.position = "none",
#         strip.background = element_rect(fill="white", color = "black"),
#         strip.text = element_text(colour = 'black', size = 12),
#         panel.grid.major = element_line(color = "grey90", size =.5),
#         plot.margin = margin(20,20,20,20))


f3 <- df %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(color = "black", size = 0.2)+
  facet_grid(sex~species, scales = "free_x") +
  labs(y = "Nº de estudos", x = "Idade (dias)") +
  scale_x_continuous(n.breaks = 10)+
  scale_fill_manual(values=c("#692b75", "#006f9f", "#009c7e", "#82c236"))+
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
        axis.title=element_text(size=9, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 9),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(0,0,20,0))


f4 <- df %>%
  ggplot(aes(x = weight, fill = sex)) +
  geom_histogram(color = "black", size = 0.2)+
  facet_grid(sex~species, scales = "free_x") +
  labs(y = "Nº de estudos", x = "Peso (g)") +
  scale_x_continuous(n.breaks = 7)+
  scale_fill_manual(values=met.brewer("Signac", 4))+
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
        axis.title=element_text(size=9, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 9),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(0,0,20,0))


save_plot(filename = "Figura3.png", plot = f3, dpi = 300)
save_plot(filename = "Figura4.png", plot = f4, dpi = 300)


# Figura 5: modelo  -----

# Modelo


df %>% group_by(study_reference, model_phenotype) %>%
  slice(1) %>% 
  group_by(model_phenotype, species) %>%
  filter(model_phenotype == "NA",) %>%
  summarise(counts = n()) # calcular quantas publicaçoes são NA

f5a <- df %>%
  group_by(study_reference, model_phenotype) %>%
  slice(1) %>% 
  group_by(model_phenotype, species) %>%
  filter(model_phenotype != "NA",) %>%
  summarise(counts = n()) %>%
  ggplot(aes(x = factor(model_phenotype), y = counts, fill = model_phenotype, label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + 
  labs(y = "Nº de publicações", x = "Modelo") +
  scale_fill_manual(values = c("#fec200", "#a6243a", "#f24a7a", "#fb7285", "#b376a2", "#a94f93", "#006f9f", "#009c7e", "#82c236", "#ffe170"))+
  coord_flip() +
  ylim(0,8)+
  facet_wrap(~species, strip.position = "top")+
  gghighlight(counts >= 2, calculate_per_facet = TRUE, label_key = model_phenotype) +
  geom_text(color = "mintcream", size = 2.5, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=7, angle = 0, color = "grey20"),
        axis.title=element_text(size=9),
        axis.title.x=element_text(margin = margin(t=5)),
        axis.title.y=element_blank(),
        panel.grid.major = element_line(color = "grey90", size =.2),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 7),
        plot.margin = margin(10,10,10,0))


df %>% group_by(model_phenotype, species) %>% 
  filter(model_phenotype == "NA",) %>% 
  summarise(counts = n())  # calcular quanto estudos sao NA

f5b <- df %>%
  group_by(model_phenotype, species) %>% 
  filter(model_phenotype != "NA",) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(x = factor(model_phenotype), y = counts, fill = model_phenotype, label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + # Mudar essa cor rs
  labs(y = "Nº de estudos", x = "Modelo") +
  scale_fill_manual(values = c("#fec200", "#a6243a", "#692b75", "#006f9f", "#009c7e", "#82c236"))+
  coord_flip() +
  ylim(0,8)+
  facet_wrap(~species, strip.position = "top")+
  gghighlight(counts > 3, calculate_per_facet = TRUE, label_key = model_phenotype) +
  geom_text(color = "mintcream", size = 2.5, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=7, angle = 0, color = "grey30"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=9),
        axis.title.x=element_text(margin = margin(t=5)),
        axis.title.y=element_blank(),
        panel.grid.major = element_line(color = "grey90", size =.2),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 7),
        plot.margin = margin(10,10,10,0))



df %>%
  group_by(model_phenotype, species) %>% 
  filter(model_phenotype == "NA") %>%  
  summarise(counts = sum(N)) # calcular quanto estudos sao NA

f5c <- df %>%
  group_by(model_phenotype, species) %>% 
  filter(model_phenotype != "NA") %>%  
  summarise(counts = sum(N)) %>% 
  ggplot(aes(x = factor(model_phenotype), y = counts, fill = model_phenotype, label = counts)) +
  geom_bar(color = "black", size = 0.2, stat="identity") + # Mudar essa cor rs
  labs(y = "Nº de animais", x = "Modelo animal") + 
  scale_fill_manual(values = c("#fec200", "#a6243a", "#a94f93", "#006f9f", "#009c7e"))+
  coord_flip() +
  ylim(0,150) +
  facet_wrap(~species, strip.position = "top")+
  gghighlight(counts > 75, calculate_per_facet = TRUE, label_key = model_phenotype) +
  geom_text(color =  "mintcream", size = 2.5, family ="serif", position = position_dodge(width=0.9), hjust = 1.1) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=7, angle = 0, color = "grey30"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=9),
        axis.title.y=element_blank(), 
        axis.title.x=element_text(margin = margin(t=5)),
        panel.grid.major = element_line(color = "grey90", size =.2),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 7),
        plot.margin = margin(10,10,10,0))

Figura5 <- f5a + f5b + f5c

Figura5


save_plot(filename = "Figura5.png", plot = Figura5, dpi = 300)

