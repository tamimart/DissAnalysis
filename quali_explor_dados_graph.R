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
library(skimr)
library(extrafont)
library(remotes)


# Rodar uma vez: remotes::install_version("Rttf2pt1", version = "1.3.8")

library(ggcorrplot)
library(ggradar)

# Instalar fontes

# Rodar uma vez: font_import(paths = "C:\\Windows\\Fonts") 
loadfonts(device = "win")
fonts() # Ver opcoes de fonte

# Carregar dataframe dos dados limpos e organizados

df <- data_geral_clean <- readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")

# Colocar tema base para todos os próximos gráficos


theme_set(theme_minimal(base_family = "Gadugi")) # Estabelecer tema e fonte para todos gráficos

# Renomear os níveis


df$language <- factor(df$language, levels = c("English", "Persian", "Chinese"), 
                      labels = c("Inglês", "Persa", "Mandarim"))


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


df$bioterium_lightcycle <- factor(df$bioterium_lightcycle, levels = c("12/12 normal", "12/12", "NA", "12/12 reverse", "natural", "10/14"), 
                 labels = c("12/12h normal", "12/12h", "Sem info", "12/12h inverso", "Natural", "10/14h")) # Ordenar niveis do fator luz do bioterio


#MetBrewer preferidos: Signac, Renoir, Thomas, VanGogh1

# PUBLICAÇÃO ----
# Figura0: Paises e idioma ---------


world <- map_data("world")
world <- subset(world, region != "Antarctica")

# criando novo df com as colunas importantes da referencia

countries <- df %>% 
  select(country, study_reference, language, year) %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  group_by(country) %>% 
  summarise(N = n()) %>% # coluna com N de publi por país
  mutate(region = country)


# junto base de dados dos paises e meus dados

dados_public <- dplyr::left_join(world, countries, by = "region")

# crio um df com média da lat e long para rótulo

rotulo <- dados_public %>%
  group_by(region) %>%
  summarise(longr = mean(long), latr = mean(lat))

rotulo <- dplyr::left_join(countries, rotulo, by = "region") # Mantenho so os paises presentes no meu df
rotulo <- rotulo %>% 
  select(region, longr, latr, N) # seleciono essas colunas


# Traduzo termos para pt

summary(as.factor(rotulo$region)) # Visualizar os fatores

rotulo$region <- factor(rotulo$region, 
                        levels = c("Australia", "Bangladesh", "Brazil", "Cameroon", "Canada", "China",  
                                   "Denmark", "Egypt", "France", "Germany", "Greece", "Hungary",
                                   "India", "Iran", "Ireland", "Israel", "Italy", "Japan",
                                   "Malaysia", "Mexico", "Netherlands", "Nigeria", "Pakistan", "Poland", 
                                   "Saudi Arabia", "South Africa", "South Korea", "Spain", "Sweden", "Switzerland",
                                   "Taiwan", "Thailand", "UK", "Uruguay", "USA"),
                        labels = c("Austrália", "Bangladesh", "Brasil", "Camarões", "Canadá", "China",  
                                   "Dinamarca", "Egito", "França", "Alemanha", "Grécia", "Hungria",
                                   "Índia", "Irã", "Irlanda", "Israel", "Itália", "Japão",
                                   "Malásia", "México", "Países Baixos", "Nigéria", "Paquistão", "Polônia", 
                                   "Arábia Saudita", "África do sul", "Coreia do Sul", "Espanha", "Suécia", "Suíça",
                                   "Taiwan", "Tailândia", "Reino Unido", "Uruguai", "EUA")) # Renomeio fatores para pt

# Mudar os valores do EUA, pq Alaska deixou a média longe do maior território

rotulo <- rotulo %>%  
  mutate(latr=replace(latr, region == "EUA", 38),
         longr=replace(longr, region == "EUA", -110)) 
  

# Figura 0

F0 <- ggplot(dados_public, aes(x = long, y = lat, map_id = region)) + # dados
  geom_map(map = dados_public, # mapa mundo
           aes(map_id = region),
           color = "white",
           size = 0.1) +
  geom_map(map = dados_public, #Coloração dos países por frequência
    aes(map_id = region, fill = N),
    color = "white",
    size = 0.1) +
  labs(caption = "Até 2017") +
  scale_fill_gradientn(
    colours = met.brewer("Signac", 5),
    na.value = "grey80",
    name = "Número de publicações",
    limits = c(0, 30),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = 5,
      barheight = 0.2)) +
  theme(panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 6, hjust = .5),
    legend.text = element_text(size = 5),
    legend.position = "top",
    plot.caption = element_text(hjust = 0.5, size = 5.5, face ="bold"),
    plot.margin = margin(0,0,5,0)) +
  expand_limits(x = world$long, y = world$lat) + #Estabeleço o tamanho
  geom_text_repel(data = subset(rotulo, N > 10), #Adiciono o rótulo dos países mais frequentes
    aes(x = longr,
        y = latr,
        label = region),
    color = "black",
    size = 2,
    segment.size = 0.3,
    box.padding = 0.8,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 10,
  )
F0
# separado por anos 

# Por ano

countries_year <- df %>% 
  select(country, study_reference, language, year) %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  group_by(country, year) %>% 
  summarise(N = n()) %>% # coluna com N de publi por país
  mutate(region = country)


# junto base de dados dos paises e meus dados

dados_public_year <- dplyr::left_join(world, countries_year, by = "region")


# Até 1996

dados_public_year_1996 <- dados_public_year %>% 
  filter(year <= '1996-01-01')


F01996 <- ggplot(dados_public, aes(x = long, y = lat, map_id = region)) + # dados
  geom_map(map = dados_public, # mapa mundo
           aes(map_id = region),
           color = "white",
           fill = "grey80",
           size = 0.1) +
  geom_map(map = dados_public_year_1996, #Coloração dos países por frequência
           aes(map_id = region, fill = N),
           color = "white",
           size = 0.1) +
  labs(caption = "Até 1996") +
  scale_fill_gradientn(
    colours = met.brewer("Signac", 5),
    na.value = "gray80",
    name = "Número de publicações",
    limits = c(0, 30),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = 7,
      barheight = 0.3)) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust=0.5, size=rel(0.5), face ="bold")) 

# Até 2006

dados_public_year_2006 <- dados_public_year %>% 
  filter(year <= '2006-01-01')


F02006 <- ggplot(dados_public, aes(x = long, y = lat, map_id = region)) + # dados
  geom_map(map = dados_public, # mapa mundo
           aes(map_id = region),
           color = "white",
           fill = "grey80",
           size = 0.1) +
  geom_map(map = dados_public_year_2006, #Coloração dos países por frequência
           aes(map_id = region, fill = N),
           color = "white",
           size = 0.1) +
  labs(caption = "Até 2006") +
  scale_fill_gradientn(
    colours = met.brewer("Signac", 5),
    na.value = "gray80",
    name = "Número de publicações",
    limits = c(0, 30),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = 7,
      barheight = 0.3)) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust=0.5, size=rel(0.5), face ="bold")) 


# Idioma

F00 <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  group_by(language) %>% 
  ggplot(aes(x = language, fill = language)) +
  geom_bar(stat = "count", color = "white") +
  labs(title = "Idioma") +
  geom_text(stat='count', aes(label=..count..), hjust=-.3, size = 1.5) +
  scale_fill_manual(values= c("#006f9f", "#ffe170", "grey80")) + 
  ylim(0,220)+
  coord_flip() +
  theme(axis.text = element_text(size=5, hjust = 0, color = "grey20"),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.title=element_text(size=5, hjust = .07, face = "bold", margin=margin(0,0,0,0)),
        plot.title.position = "plot",
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,5,0,0)
  )


#Salvar

Figura0 <- (F0 + inset_element(F00, left = 0.05, bottom = 0.05, right = 0.28, top = 0.25)) / (plot_spacer() | F01996 | F02006 | plot_spacer()) + plot_layout(heights = c(10,2), widths = c(8, 8))
Figura0
save_plot(filename = "Figura0.png", plot = Figura0, dpi = 300) # Salvar gráfico


# Figura F0a - Publicação no ano ---- 

Figura0a <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(color = "#006f9f", size = 1.5) +
  labs(y = "Nº de publicações", x = "Ano") + 
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  scale_y_continuous(breaks = c(5,10,15,20,25))+
theme(axis.text=element_text(size=8, angle = 0, color = "grey20"),
      axis.title=element_text(size=9, hjust = 1),
      axis.title.y=element_text(margin = margin(r=5)),
      axis.title.x=element_text(margin = margin(t=5)))

save_plot(filename = "Figura0a.png", plot = Figura0a, dpi = 300) # Salvar gráfico  
  


# POPULAÇÃO -----

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
  theme(axis.text=element_text(size=7, angle = 0, color = "grey20"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=8, hjust = 1),
        plot.title=element_text(size=10, face = "italic"),
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
  theme(axis.text=element_text(size=7, angle = 0, color = "grey20"),
        axis.title=element_text(size=8, hjust = 1),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic"),
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
  theme(axis.text=element_text(size=7, angle = 0, color = "grey20"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=8, hjust = 1),
        plot.title=element_text(size=10, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0))

f1d <- df  %>% # sexo no tempo
  ggplot(aes(x = year, fill = sex, order = sex)) +
  geom_bar(color = "black", size = .2)+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  labs(y = "Nº de estudos", x = "Ano", title = "d") + 
  scale_fill_manual(values=c("#692b75", "#006f9f", "#009c7e", "grey80")) + # Preferi esse
  theme(axis.text=element_text(size=7, angle = 0, color = "grey20"),
        axis.title=element_text(size=8, hjust = 1),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic"),
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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=5.5, angle = 0, color = "grey20"),
        axis.title=element_text(size=7),
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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=5.5, angle = 0, color = "grey20"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title=element_text(size=7),
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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=5.5, angle = 0, color = "grey20"),
        axis.title=element_text(size=7),
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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=5.5, angle = 0, color = "grey0"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.title=element_text(size=7),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic", hjust = 0),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(0,10,10,0))

Figura2 <- (f2a + f2b + plot_spacer()) / (f2c + f2d + plot_spacer()) +
  plot_layout(heights = c(11, 6), guides = 'collect') 


Figura2
save_plot(filename = "Figura2.png", plot = Figura2, dpi = 300)


# Figura3 e 4: Idade e peso ---- 

# Idade
# criar um df com os valores da media e dp arredondados
age_ss <- df %>% 
  group_by(species, sex) %>% 
  skim(age) %>% 
  mutate(numeric.mean = round(numeric.mean, 1),
         numeric.sd = round(numeric.sd, 1))

# vincular valores da media e sd com os fatores sexo e especie
labels_age <- data.frame(species = c("Camundongo", "Camundongo", "Camundongo", "Camundongo", "Rato", "Rato", "Rato", "Rato"),
                     sex = c("Macho", "Fêmea", "Ambos", "Sem info", "Macho", "Fêmea", "Ambos", "Sem info"),
                     label = paste(age_ss$numeric.mean,  age_ss$numeric.sd, sep = "±"))

f3 <- df %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(color = "black", size = 0.2)+
  facet_grid(fct_infreq(sex)~species, scales = "free_x") +
  geom_text(data=labels_age, aes(label = label), x=Inf , y=35, hjust = 1.1, color = "grey20", size =2)+
  labs(y = "Nº de estudos", x = "Idade (dias)") +
  scale_x_continuous(n.breaks = 10)+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values=c("#692b75", "#006f9f", "#009c7e", "grey80"))+
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(20,0,20,0))
f3


# Peso
# criar um df com os valores da media e dp arredondados
weight_ss <- df %>% 
  group_by(species, sex) %>% 
  skim(weight) %>% 
  mutate(numeric.mean = round(numeric.mean, 1),
         numeric.sd = round(numeric.sd, 1))

# vincular valores da media e sd com os fatores sexo e especie
labels_weight <- data.frame(species = c("Camundongo", "Camundongo", "Camundongo", "Camundongo", "Rato", "Rato", "Rato", "Rato"),
                     sex = c("Macho", "Fêmea", "Ambos", "Sem info", "Macho", "Fêmea", "Ambos", "Sem info"),
                     label = paste(weight_ss$numeric.mean,  weight_ss$numeric.sd, sep = "±"))


f4 <- df %>%
  ggplot(aes(x = weight, fill = sex)) +
  geom_histogram(color = "black", size = 0.2)+
  facet_grid(fct_infreq(sex)~species, scales = "free_x") +
  geom_text(data=labels_weight, aes(label = label), x=Inf , y=35, hjust = 1.1, color = "grey20", size =2)+
  labs(y = "Nº de estudos", x = "Peso (g)") +
  scale_x_continuous(n.breaks = 7)+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values=c("#692b75", "#006f9f", "#009c7e", "grey80"))+
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(20,0,20,0))

f4

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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7),
        axis.title.x=element_text(margin = margin(t=5)),
        axis.title.y=element_blank(),
        panel.grid.major = element_line(color = "grey90", size =.2),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 6),
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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey30"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=7),
        axis.title.x=element_text(margin = margin(t=5)),
        axis.title.y=element_blank(),
        panel.grid.major = element_line(color = "grey90", size =.2),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 6),
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
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey30"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=7),
        axis.title.y=element_blank(), 
        axis.title.x=element_text(margin = margin(t=5)),
        panel.grid.major = element_line(color = "grey90", size =.2),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 6),
        plot.margin = margin(10,10,10,0))

Figura5 <- f5a + f5b + f5c

Figura5


save_plot(filename = "Figura5.png", plot = Figura5, dpi = 300)


# ACONDICIONAMENTO -----

# Figura6: ciclo, umi, temp -----

# Ciclo


F6a <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  group_by(bioterium_lightcycle) %>% 
  ggplot(aes(x = fct_infreq(bioterium_lightcycle), fill = bioterium_lightcycle)) +
  geom_bar(color = "black", size = .2)+
  labs(y = "Nº de publicações", x = "Ciclo de luz do biotério",title = "a") +
  scale_fill_manual(values= c("#a94f93", "#b376a2", "grey80", "#f39da4", "#f24a7a", "#a6243a")) +
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=8, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=10, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,0,10,10))
  
# Temp

# filtrar as publicacoes e remover sem informação
filtro_bioterium_temp <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  filter(bioterium_temp != "NA") %>% 
  select(bioterium_temp)

m_t <- data.frame(label = paste(round(mean(filtro_bioterium_temp$bioterium_temp),1), round(sd(filtro_bioterium_temp$bioterium_temp),1), sep = "±")) # criei um data com o valor da media e dp arredondados


F6b <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  ggplot(aes(x = bioterium_temp)) +
  geom_histogram(color = "black", size = 0.2 , fill = "#a6243a")+
  geom_text(data=m_t, aes(label=label), x=Inf , y=53, hjust = 1.1, color = "grey20", size =2)+
  geom_vline(data = filtro_bioterium_temp, aes(xintercept = mean(bioterium_temp)), col="#fec200",size=.5)+
  geom_vline(data = filtro_bioterium_temp, aes(xintercept = (sd(bioterium_temp)) + 22.44694), col="#ffe170",size=.5, linetype = "dashed")+
  geom_vline(data = filtro_bioterium_temp, aes(xintercept = (22.44694 - sd(bioterium_temp))), col="#ffe170",size=.5, linetype = "dashed")+
  labs(y = "Nº de publicações", x = "Temperatura do biotério (°C)") +
  labs(title = "b")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title= element_text(size = 10, face ="italic"),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(10,0,10,10))


#  Umi

# filtrar as publicacoes e remover sem informação
filtro_bioterium_umid <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  filter(bioterium_umid != "NA") %>% 
  select(bioterium_umid)

m_u <- data.frame(label = paste(round(mean(filtro_bioterium_umid$bioterium_umid),1), round(sd(filtro_bioterium_umid$bioterium_umid),1), sep = "±")) # criei um data com o valor da media e dp arredondados


F6c <- df %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  ggplot(aes(x = bioterium_umid)) +
  geom_histogram(color = "black", size = 0.2 , fill = "#692b75")+
  geom_text(data=m_u, aes(label=label), x=Inf , y=29, hjust = 1.1, color = "grey20", size =2)+
  geom_vline(data = filtro_bioterium_umid, aes(xintercept = mean(bioterium_umid)), col="#fec200",size=.5)+
  geom_vline(data = filtro_bioterium_umid, aes(xintercept = (sd(bioterium_umid)) + 55.3), col="#ffe170",size=.5, linetype = "dashed")+
  geom_vline(data = filtro_bioterium_umid, aes(xintercept = (55.3 - sd(bioterium_umid))), col="#ffe170",size=.5, linetype = "dashed")+
  labs(y = "Nº de publicações", x = "Umidade do biotério (%)") +
  labs(title = "c")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title= element_text(size = 10, face ="italic"),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(10,0,10,10))



Figura6 <-  F6a + (F6b / F6c) + plot_layout(widths = c(6,4))
Figura6

save_plot(filename = "Figura6.png", plot = Figura6, dpi = 300)

# Figura 7: Volume caixa x animais por caixa x animal / volume ------

cage_m <- df %>% 
  select(cage_measures, weight, study_reference, animals_percage, species) %>% 
  group_by(study_reference) %>% 
  slice(1) %>% 
  separate(col = cage_measures, sep = c("x","X","×"), into = c("c1", "c2", "c3")) %>% 
  mutate(c1 = as.numeric(c1),
         c2 = as.numeric(c2),
         c3 = as.numeric(c3)) %>% 
  filter(c2 != "NA") 


cage_3d <- cage_m %>% 
  filter(c3 != "NA") %>% # retirei estudos que nao deram medidas dos três lados da caixa
  mutate(volume_cx = as.numeric(c1 * c2 * c3), # separei as medidas da caixa
         animals_percage = as.numeric(animals_percage))
cage_3d <- cage_3d %>% 
  mutate(vol_panimal = as.numeric(volume_cx/animals_percage), # nova variavel: volume de caixa por animal
         vol_ppeso = as.numeric(((volume_cx/animals_percage)/weight))) # nova variavel: volume de caixa por peso do animal


  
# criar um df com os valores da media e dp arredondados 
vol_panimal_meansd <- cage_3d  %>% 
  group_by(species) %>% 
  skim(vol_panimal) %>% 
  mutate(numeric.mean = round(numeric.mean, 1),
         numeric.sd = round(numeric.sd, 1))

vol_ppeso_meansd <- cage_3d  %>% 
  group_by(species) %>% 
  skim(vol_ppeso) %>% 
  mutate(numeric.mean = round(numeric.mean, 1),
         numeric.sd = round(numeric.sd, 1))


# vincular valores da media e sd com especie
vol_panimal_label <- data.frame(species = c("Camundongo", "Rato"),
                         label = paste(vol_panimal_meansd$numeric.mean,  vol_panimal_meansd$numeric.sd, sep = "±"))


vol_ppeso_label <- data.frame(species = c("Camundongo", "Rato"),
                                label = paste(vol_ppeso_meansd$numeric.mean,  vol_ppeso_meansd$numeric.sd, sep = "±"))

# Plotar grafico de volume de caixa por animal

F7a <- cage_3d %>%
  ggplot(aes(x = vol_panimal, fill = species)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid(~species, scales = "free_x")+
  geom_text(data= vol_panimal_label, aes(label = label), x=Inf , y=2.9, hjust = 1.1, color = "grey20", size =2)+
  labs(y = "Nº de publicações", x = "Volume de caixa por animal (cm³)") +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c("#ff9400", "#ec2b2b"))+
  labs(title = "a")+
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title= element_text(size = 10, face ="italic"),
        plot.title.position = "panel",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(20,0,0,0))
  
F7a

# Plotar grafico de volume de caixa por peso de animal

F7b <- cage_3d %>%
  ggplot(aes(x = vol_ppeso, fill = species)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid(~species, scales = "free_x")+
  geom_text(data= vol_ppeso_label, aes(label = label), x=Inf , y=2.9, hjust = 1.1, color = "grey20", size =2)+
  labs(y = "Nº de publicações", x = "Volume de caixa por peso do animal (cm³/g)") +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c("#ff9400", "#ec2b2b"))+
  labs(title = "b")+
  theme_bw(base_family = "Gadugi")+
  theme(axis.text=element_text(size=6, angle = 0, color = "grey20"),
        axis.title=element_text(size=7, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title= element_text(size = 10, face ="italic"),
        plot.title.position = "panel",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 8),
        panel.grid.major = element_line(color = "grey90", size =.3),
        plot.margin = margin(10,0,20,0))

F7b

Figura7 <- F7a / F7b + plot_layout(heights = c(5,5))
Figura7

save_plot(filename = "Figura7.png", plot = Figura7, dpi = 300)

# INTERVENÇÃO

# 
