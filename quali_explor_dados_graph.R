# Referência: ~ref da dissertação~

# ETAPA 3: Visualização dos dados (Gráficos)

# Carregar pacotes ----


library(tidyverse)    # manipulacao de dados e plotar
library(summarytools) # estatisticas descritivas
library(patchwork)    # juntar plots
library(gghighlight)  # destacar nos plots
library(MetBrewer)    # dar cor
library(ggtext)       # adicionar texto 
library(cowplot)      # salvar plot
library(ggrepel)      # dicionar rotulo
library(ggmap)        # plotar mapa
library(skimr)        # resumir estatistica
library(extrafont)    # adicionar fontes
library(remotes)      # baixar pacotes remotamente
library(colorspace)   # manipular cor
library(ggdist)       # plotar densidade
library(gghalves)     # plotar pontos
library(writexl)      # salvar tabela


library(ggcorrplot)
library(ggradar)

# Instalar fontes ----

# Rodar uma vez:
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import(paths = "C:\\Windows\\Fonts")
# loadfonts(device = "win")
# fonts() # Ver opcoes de fonte

# Carregar dataframe dos dados limpos e organizados

df <-
  data_geral_clean <-
  readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")

# Estabelecer tema base para todos os próximos gráficos

theme_set(theme_minimal(base_family = "Gadugi"))

# Renomear os níveis de variaveis categóricas para PT

df$language <-
  factor(
    df$language,
    levels = c("English", "Persian", "Chinese"),
    labels = c("Inglês", "Persa", "Mandarim")
    )


df$species <- factor(
  df$species,
  levels = c("mice", "rat"),
  labels = c("Camundongo", "Rato")
) # Ordenar niveis do fator espécie

df$sex <- factor(
  df$sex,
  levels = c("M", "F", "M and F", "NA"),
  labels = c("Macho", "Fêmea", "Ambos", "Sem info")
) # Ordenar niveis do fator sexo


df$model_phenotype <- factor(
  df$model_phenotype,
  levels = c(
    "ACTH (100microg)",
    "stroke (Middle Cerebral Artery occlusion)",  
    "Bacillus Calmette–Guérin (BCG)",
    "olfactory bulbectomy",
    "pentylenetetrazol-kindled seizures",
    "CMS",
    "CUMs",
    "CUS",
    "depressed",
    "high emotional",
    "low emotional",
    "temporal lobe epilepsy (pilocarpine)",
    "streptozotocin",
    "prenatal stress procedure",
    "restraint-stress",
    "subchronic stress:restraint–water immersion",
    "melanin-concentrating hormone 50ng",
    "1 saline injection for 4 days",
    "Isolation-Rearing",
    "LPS",
    "NA",
    "forced swim",
    "FS and CMS",
    "ovariectomized",
    "ovariectomized +1 saline injection for 4 days",
    "mother exposed to Chlorpyrifos (CPF)",
    "mother exposed to DDT",
    "reserpine",
    "amphetamine withdrawal",
    "antidepressant withdrawal",
    "wheel running + restraint-stress",
    "maternal-separation",
    "PTSD-like"
  ),
  labels = c(
    "ACTH (100µg)",
    "AVC (por oclusão da artéria cerebral média)",
    "Bacillus Calmette–Guérin (BCG)",
    "Bulbectomia olfatória",
    "Convulsões por pentilenotetrazol",
    "CMS - Estresse leve crônico",
    "CUMs - Estresse leve imprevisível crônico",
    "CUS - Estresse imprevisível crônico" ,
    "Deprimido",
    "Emocional alto",
    "Emocional baixo",
    "Epilepsia do lobo temporal (c/ pilocarpina)",
    "Estreptozotocina",
    "Estresse pré-natal",
    "Estresse por contenção",
    "Estresse subcrônico: contenção em água",
    "Hormônio concentrador de melanina (50ng)",
    "Injeção salina por 4 dias",
    "Isolamento",
    "Lipopolissacarídeo",
    "NA",
    "Natação forçada",
    "Natação forçada + Estresse leve crônico",
    "Ovacteriomizada",
    "Ovacteriomizada + injeção salina por 4 dias",
    "Progenitora exposta à Chlorpyrifos (CPF)",
    "Progenitora exposta à DDT",
    "Reserpina",
    "Retirada de anfetamina",
    "Retirada de antidepressivo",
    "Roda de corrida + estresse por contenção",
    "Separação maternal",
    "Tipo TEPT"
  )
)

# Ordernar os niveis de luz

df$bioterium_lightcycle <-
  factor(
    df$bioterium_lightcycle,
    levels = c(
      "12/12 normal",
      "12/12",
      "NA",
      "12/12 reverse",
      "natural",
      "10/14"
    ),
    labels = c(
      "12/12 normal",
      "12/12",
      "Sem info",
      "12/12 inverso",
      "Natural",
      "10/14"
    )
  ) 

# Ordernar os niveis classes

levels(df$atd_class)

df$atd_class <-
  factor(
    df$atd_class,
    levels = c(
      "IMAO", 
      "melatonergic agonist", 
      "multimodal", 
      "NDRA", 
      "NDRI",       
      "NRI",         
      "SNRI",
      "SSRI",
      "teca",
      "tricyclic" 
    ),
    labels = c(
      "IMAO", 
      "Agonista melatoninérgico", 
      "Multimodal", 
      "ALDN", 
      "IRND",       
      "IRN",         
      "IRSN",
      "ISRS",
      "TeCA",
      "TCA" 
    )
  ) 


# Ordenar os niveis antidepressivos

levels(df$atd_type)

df$atd_type <-
  factor(
    df$atd_type,
    levels = c(
      "agomelatine",
      "amineptine",
      "amitriptyline",
      "amoxapine",
      "amphetamine",
      "bupropion",       
      "citalopram",      
      "clomipramine",
      "desipramine",
      "desvenlafaxine",
      "duloxetine",
      "escitalopram",
      "fluoxetine",
      "fluvoxamine",
      "imipramine",     
      "maprotiline",
      "mianserin",
      "milnacipran",
      "moclobemide",
      "nortriptyline",
      "paroxetine",
      "reboxetine",
      "selegiline",
      "sertraline",
      "sibutramine",
      "tramadol",
      "tranylcypromine",
      "venlafaxine",    
      "vilazodone",      
      "viloxazine",      
      "vortiexetine" 
    ),
    labels = c(
      "agomelatina",
      "amineptina",
      "amitriptilina",
      "amoxapina",
      "anfetamina",
      "bupropiona",       
      "citalopram",      
      "clomipramina",
      "desipramina",
      "desvenlafaxina",
      "duloxetina",
      "escitalopram",
      "fluoxetina",
      "fluvoxamina",
      "imipramina",     
      "maprotilina",
      "mianserina",
      "milnaciprano",
      "moclobemida",
      "nortriptilina",
      "paroxetina",
      "reboxetina",
      "selegilina",
      "sertralina",
      "sibutramina",
      "tramadol",
      "tranilcipromina",
      "venlafaxina",    
      "vilazodona",      
      "viloxazina",      
      "vortioxetina" 
    )
  ) 

# ordernar freq adm

df <- df %>% 
  mutate(treatment_freq = as.factor(treatment_freq)) # alterar variavel para categorica

df$treatment_freq <-
  factor(
    df$treatment_freq,
    levels = c(
      "1",
      "2",
      "3",
      "NA"
    ),
    labels = c(
      "1",
      "2",
      "3",
      "Sem info"
    )
  ) 

# ordernar via adm

levels(df$treatment_via)

df$treatment_via <-
  factor(
    df$treatment_via,
    levels = c(
      "gavage",
      "intranasal",       
      "IP",                       
      "microinfusionIL",
      "microinjection (dorsal hippocampus)",
      "NA",
      "oral", 
      "oral (dietary treatment)",           
      "subcutaneous",
      "tablet" 
    ),
    labels = c(
      "Gavagem",
      "Intranasal",       
      "Intraperitoneal",                       
      "Microinfusão (IL)",
      "Microinjeção (hipocampo)",
      "Sem info",
      "Oral", 
      "Oral (dieta)",           
      "Subcutânea",
      "Tablete"
    )
  ) 



# PUBLICAÇÃO
## Figura0: Paises e idioma ---------

# Importar dados de localização

world <- map_data("world")
world <- subset(world, region != "Antarctica")

# Criar novo df com as colunas importantes da referencia

countries <- df %>%
  select(country, study_reference, language, year) %>%
  group_by(study_reference) %>%
  slice(1) %>%
  group_by(country) %>%
  summarise(N = n()) %>% # coluna com N de publi por país
  mutate(region = country)


# Juntar base de dados dos paises e meus dados

dados_public <- dplyr::left_join(world, countries, by = "region")

# Criar um df com média da lat e long para rótulo

rotulo <- dados_public %>%
  group_by(region) %>%
  summarise(longr = mean(long), latr = mean(lat))

rotulo <-
  dplyr::left_join(countries, rotulo, by = "region") # Mantenho so os paises presentes no meu df
rotulo <- rotulo %>%
  select(region, longr, latr, N) # seleciono essas colunas


# Traduzo termos para pt

summary(as.factor(rotulo$region)) # Visualizar os fatores

rotulo$region <- factor(
  rotulo$region,
  levels = c(
    "Australia",
    "Bangladesh",
    "Brazil",
    "Cameroon",
    "Canada",
    "China",
    "Denmark",
    "Egypt",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "India",
    "Iran",
    "Ireland",
    "Israel",
    "Italy",
    "Japan",
    "Malaysia",
    "Mexico",
    "Netherlands",
    "Nigeria",
    "Pakistan",
    "Poland",
    "Saudi Arabia",
    "South Africa",
    "South Korea",
    "Spain",
    "Sweden",
    "Switzerland",
    "Taiwan",
    "Thailand",
    "UK",
    "Uruguay",
    "USA"
  ),
  labels = c(
    "Austrália",
    "Bangladesh",
    "Brasil",
    "Camarões",
    "Canadá",
    "China",
    "Dinamarca",
    "Egito",
    "França",
    "Alemanha",
    "Grécia",
    "Hungria",
    "Índia",
    "Irã",
    "Irlanda",
    "Israel",
    "Itália",
    "Japão",
    "Malásia",
    "México",
    "Países Baixos",
    "Nigéria",
    "Paquistão",
    "Polônia",
    "Arábia Saudita",
    "África do sul",
    "Coreia do Sul",
    "Espanha",
    "Suécia",
    "Suíça",
    "Taiwan",
    "Tailândia",
    "Reino Unido",
    "Uruguai",
    "EUA"
  )
) # Renomeio fatores para pt

# Mudar os valores do EUA, pq Alaska deixou a média longe do maior território

rotulo <- rotulo %>%
  mutate(
    latr = replace(latr, region == "EUA", 38),
    longr = replace(longr, region == "EUA",-110)
  )


# Plotar graficos

# Todos anos até 2017

F0 <-
  ggplot(dados_public, aes(x = long, y = lat, map_id = region)) + # dados
  geom_map(map = dados_public,
           # mapa mundo
           aes(map_id = region),
           color = "white",
           size = 0.1) +
  geom_map(
    map = dados_public,
    #Coloração dos países por frequência
    aes(map_id = region, fill = N),
    color = "white",
    size = 0.1
  ) +
  labs(caption = "Até 2017") +
  scale_fill_gradientn(
    colours = met.brewer("Signac", 5),
    na.value = "grey80",
    name = "Número de publicações",
    limits = c(0, 30),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = 5,
      barheight = 0.2
    )
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 6, hjust = .5),
    legend.text = element_text(size = 5),
    legend.position = "top",
    plot.caption = element_text(hjust = 0.5, size = 6),
    plot.margin = margin(0, 0, 5, 0)
  ) +
  expand_limits(x = world$long, y = 85) + #Estabeleço o tamanho
  geom_text_repel(
    data = subset(rotulo, N > 10),
    #Adiciono o rótulo dos países mais frequentes
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
  ) +
  coord_quickmap() # ajusta proporcao certa


# Separar publicações por ano

countries_year <- df %>%
  select(country, study_reference, language, year) %>%
  group_by(study_reference) %>%
  slice(1) %>%
  group_by(country, year) %>%
  summarise(N = n()) %>% # coluna com N de publi por país
  mutate(region = country)


# Juntar base de dados dos paises e meus dados

dados_public_year <-
  dplyr::left_join(world, countries_year, by = "region")


# Até 1996

dados_public_year_1996 <- dados_public_year %>%
  filter(year <= '1996-01-01')


F01996 <-
  ggplot(dados_public, aes(x = long, y = lat, map_id = region)) + # dados
  geom_map(
    map = dados_public,
    # mapa mundo
    aes(map_id = region),
    color = "white",
    fill = "grey80",
    size = 0.1
  ) +
  geom_map(
    map = dados_public_year_1996,
    #Coloração dos países por frequência
    aes(map_id = region, fill = N),
    color = "white",
    size = 0.1
  ) +
  labs(caption = "Até 1996") +
  scale_fill_gradientn(
    colours = met.brewer("Signac", 5),
    na.value = "gray80",
    name = "Número de publicações",
    limits = c(0, 30),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = 7,
      barheight = 0.3
    )
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5, size = rel(0.6))
  ) +
  coord_quickmap() 

# Até 2006

dados_public_year_2006 <- dados_public_year %>%
  filter(year <= '2006-01-01')


F02006 <-
  ggplot(dados_public, aes(x = long, y = lat, map_id = region)) + # dados
  geom_map(
    map = dados_public,
    # mapa mundo
    aes(map_id = region),
    color = "white",
    fill = "grey80",
    size = 0.1
  ) +
  geom_map(
    map = dados_public_year_2006,
    #Coloração dos países por frequência
    aes(map_id = region, fill = N),
    color = "white",
    size = 0.1
  ) +
  labs(caption = "Até 2006") +
  scale_fill_gradientn(
    colours = met.brewer("Signac", 5),
    na.value = "gray80",
    name = "Número de publicações",
    limits = c(0, 30),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = 7,
      barheight = 0.3
    )
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5, size = rel(0.6))
  ) +
  coord_quickmap()


# Idiomas

F0idi <- df %>%
  group_by(study_reference) %>%
  slice(1) %>%
  group_by(language) %>%
  ggplot(aes(x = language, fill = language)) +
  geom_bar(stat = "count", color = "white") +
  labs(title = "Idioma") +
  geom_text(
    stat = 'count',
    aes(label = ..count..),
    hjust = 0,
    size = 1.5
  ) +
  scale_fill_manual(values = c("#006f9f", "#ffe170", "grey80")) +
  ylim(0, 220) +
  coord_flip() +
  theme(
    axis.text = element_text(
      size = 5,
      hjust = 0,
      color = "grey20"
    ),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      size = 5.5,
      hjust = .08,
      margin = margin(0, 0, 0, 0)
    ),
    plot.title.position = "plot",
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 3, 3, 3),
    plot.background = element_rect(
      colour = "grey80",
      fill = NA,
      size = .5
    )
  )


# Combinar e salvar

Figura0 <-
  (F0 + inset_element(
    F0idi,
    left = 0.05,
    bottom = 0.05,
    right = 0.28,
    top = 0.25
  )) / (plot_spacer() |
          F01996 |
          F02006 |
          plot_spacer()) + plot_layout(heights = c(10, 2), widths = c(8, 8))
save_plot(filename = "Figura0.png",
          plot = Figura0,
          dpi = 300) # Salvar gráfico


## Figura0a - Quantidade de publicações no ano ----

Figura0a <- df %>%
  group_by(study_reference) %>%
  slice(1) %>%
  group_by(year) %>%
  ggplot(aes(x = year)) +
  geom_bar(fill = "#82c236") +
  labs(y = "Nº de publicações", x = "Ano") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_y_continuous(breaks = c(4, 8, 12, 16, 20, 24, 28), expand = c(0, 0)) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 9,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 10, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(20, 10, 20, 10)
  )

save_plot(filename = "Figura0a.png",
          plot = Figura0a,
          dpi = 300) # Salvar gráfico



# POPULAÇÃO 
## Figura1: Sexo, especie, tempo ----


f1a <- df %>%
  group_by(species) %>%
  summarise(counts = sum(N)) %>% # N por espécie
  ggplot(aes(
    x = species,
    y = counts,
    fill = species,
    label = counts
  )) +
  geom_bar(stat = "identity") + # mesmo que geom_col() sem stat
  geom_text(
    size = 2.5,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  scale_y_continuous(limits = c(0, 7000), expand = c(0, 0)) +
  labs(y = "Nº de animais", x = "Espécie", title = "a") +
  scale_fill_manual(values = met.brewer("Signac", 2)) +
  theme(
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 8, hjust = 1),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f1b <- df  %>% # especie no tempo
  ggplot(aes(x = year, fill = species)) +
  geom_bar() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(y = "Nº de estudos",
       x = "Ano",
       title = "b",
       fill = "Espécie") +
  scale_fill_manual(values = met.brewer("Signac", 2)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,60), n.breaks = 4) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 0, 10)
  )

f1c <- df %>%
  group_by(sex) %>%
  summarise(counts = sum(N)) %>% # N por sexo
  ggplot(aes(
    x = sex,
    y = counts,
    fill = sex,
    label = counts
  )) +
  geom_bar(stat = "identity") +
  geom_text(
    size = 2.5,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  scale_y_continuous(limits = c(0, 7000), expand = c(0, 0)) + # 300 a mais para caber a anotacao da maior barra
  labs(y = "Nº de animais", x = "Sexo", title = "c") +
  theme(
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 8, hjust = 1),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f1d <- df  %>% # sexo no tempo
  ggplot(aes(x = year, fill = sex, order = sex)) +
  geom_bar() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(y = "Nº de estudos", x = "Ano", title = "d") +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,60), n.breaks = 4) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 0, 10)
  )



# Combinar e salvar

Figura1 <-
  f1a + f1b + f1c + f1d + plot_layout(heights = c(6, 6), widths = c(3, 6))
save_plot(filename = "Figura1.png",
          plot = Figura1,
          dpi = 300)

## Figura2: Linhagens ----
# Strain
  
f2a <- df %>%
  filter(species == "Camundongo") %>%
  group_by(study_reference) %>% 
  distinct(strain) %>% 
  group_by(strain) %>% 
  summarise(counts = n()) %>% 
ggplot(aes(
    x = factor(strain),
    y = counts,
    fill = strain,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de publicações", x = "Linhagem", title = "a") +
  scale_y_continuous(n.breaks = 5, expand = c(0,0), limits = c(0,60)) +
  coord_flip() +
  gghighlight(counts > 15,
              calculate_per_facet = TRUE,
              label_key = strain) +
  scale_fill_manual(values = c("#ffe170", "#fec200", "#ff9400")) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 5.5,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10, hjust = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 10, 0, 10)
  )

f2b <- df %>%
  group_by(strain) %>%
  filter(species == "Camundongo") %>%
  summarise(counts = n()) %>%
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(
    x = factor(strain),
    y = counts,
    fill = strain,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de estudos", x = "Linhagem", title = "b") +
  scale_y_continuous(n.breaks = 5, expand = c(0,0), limits = c(0,150)) +
  coord_flip() +
  gghighlight(counts > 25,
              calculate_per_facet = TRUE,
              label_key = strain) +
  scale_fill_manual(values = c("#ffe170", "#fec200", "#ff9400")) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 5.5,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10, hjust = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 10, 0, 0)
  )

f2c <- df %>%
  group_by(strain) %>%
  filter(species == "Camundongo") %>%
  summarise(counts = sum(N)) %>%
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(
    x = factor(strain),
    y = counts,
    fill = strain,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de animais", x = "Linhagem", title = "c") +
  scale_y_continuous(expand = c(0,0), limits = c(0,2100)) +
  coord_flip() +
  gghighlight(counts > 500,
              calculate_per_facet = TRUE,
              label_key = strain) +
  scale_fill_manual(values = c("#ffe170", "#fec200", "#ff9400")) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 5.5,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10, hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 10, 0, 0)
  )


f2d <- df %>%
  filter(species == "Rato") %>%
  group_by(study_reference) %>% 
  distinct(strain) %>% 
  group_by(strain) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(
    x = factor(strain),
    y = counts,
    fill = strain,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de publicações", x = "Linhagem", title = "d") +
  scale_y_continuous(n.breaks = 5, expand = c(0,0), limits = c(0,60)) +
  coord_flip() +
  gghighlight(counts > 15,
              calculate_per_facet = TRUE,
              label_key = strain) +
  scale_fill_manual(values = c("#ec2b2b", "#a6243a")) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 5.5,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10, hjust = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 10, 0, 10)
  )


f2e <- df %>%
  group_by(strain) %>%
  filter(species == "Rato") %>%
  summarise(counts = n()) %>%
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(
    x = factor(strain),
    y = counts,
    fill = strain,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de estudos", x = "Linhagem", title = "e") +
  scale_y_continuous(n.breaks = 5, expand = c(0,0), limits = c(0, 150)) +
  coord_flip() +
  gghighlight(counts > 25,
              calculate_per_facet = TRUE,
              label_key = strain) +
  scale_fill_manual(values = c("#ec2b2b", "#a6243a")) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 5.5,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10, hjust = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(0, 10, 10, 0)
  )

f2f <- df %>%
  group_by(strain) %>%
  filter(species == "Rato") %>%
  summarise(counts = sum(N)) %>%
  mutate(counts = as.numeric(counts)) %>%
  ggplot(aes(
    x = factor(strain),
    y = counts,
    fill = strain,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de animais", x = "Linhagem", title = "f") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2100)) +
  coord_flip() +
  gghighlight(counts > 500,
              calculate_per_facet = TRUE,
              label_key = strain) +
  scale_fill_manual(values = c("#ec2b2b", "#a6243a")) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 5.5,
      angle = 0,
      color = "grey0"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10, hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(0, 10, 10, 0)
  )

Fig2t <- (f2a + f2b + f2c) / (f2d + f2e + f2f) + plot_layout(heights = c(11, 6), guides = 'collect')


save_plot(filename = "Figura2.png",
          plot = Fig2t,
          dpi = 300)


## Figura3 e 4: Idade e peso ----

# Idade

# criar um df com os valores da media e dp arredondados

age_ss <- df %>%
  group_by(species, sex) %>%
  skim(age) %>%
  mutate(numeric.mean = round(numeric.mean, 1),
         numeric.sd = round(numeric.sd, 1))

# Vincular valores da media e sd com os fatores sexo e especie
labels_age <-
  data.frame(
    species = c(
      "Camundongo",
      "Camundongo",
      "Camundongo",
      "Camundongo",
      "Rato",
      "Rato",
      "Rato",
      "Rato"
    ),
    sex = c(
      "Macho",
      "Fêmea",
      "Ambos",
      "Sem info",
      "Macho",
      "Fêmea",
      "Ambos",
      "Sem info"
    ),
    label = paste(age_ss$numeric.mean,  age_ss$numeric.sd, sep = "±")
  )

f3 <- df %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid(fct_infreq(sex) ~ species, scales = "free_x") +
  geom_text(
    data = labels_age,
    aes(label = label),
    x = Inf ,
    y = 35,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) +
  labs(y = "Nº de estudos", x = "Idade (dias)") +
  scale_x_continuous(n.breaks = 10) +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(20, 0, 20, 0)
  )

# Peso

# Criar um df com os valores da media e dp arredondados
weight_ss <- df %>%
  group_by(species, sex) %>%
  skim(weight) %>%
  mutate(numeric.mean = round(numeric.mean, 1),
         numeric.sd = round(numeric.sd, 1))

# Vincular valores da media e sd com os fatores sexo e especie
labels_weight <-
  data.frame(
    species = c(
      "Camundongo",
      "Camundongo",
      "Camundongo",
      "Camundongo",
      "Rato",
      "Rato",
      "Rato",
      "Rato"
    ),
    sex = c(
      "Macho",
      "Fêmea",
      "Ambos",
      "Sem info",
      "Macho",
      "Fêmea",
      "Ambos",
      "Sem info"
    ),
    label = paste(weight_ss$numeric.mean,  weight_ss$numeric.sd, sep = "±")
  )


f4 <- df %>%
  ggplot(aes(x = weight, fill = sex)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid(fct_infreq(sex) ~ species, scales = "free_x") +
  geom_text(
    data = labels_weight,
    aes(label = label),
    x = Inf ,
    y = 35,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) +
  labs(y = "Nº de estudos", x = "Peso (g)") +
  scale_x_continuous(n.breaks = 7) +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(20, 0, 20, 0)
  )

# Salvar

save_plot(filename = "Figura3.png",
          plot = f3,
          dpi = 300)
save_plot(filename = "Figura4.png",
          plot = f4,
          dpi = 300)

## Figura3-4: Idade e peso ----

f34a <- df %>%
  filter(species == "Camundongo") %>%
  ggplot(aes(x = sex, y = age, fill = sex, color = sex)) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  scale_color_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  geom_boxplot(
    width = .2, fill = "white",
    size = .3, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .5, ## bandwidth
    width = .67, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .3, size = .1
  ) +
  labs(y = "Idade (dias)",
       x = "Camundongo",
       fill = "Sexo",
       title = "a") +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0.5),
    axis.title.y = element_text(hjust = 1, margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, 0, 5, 0)
  )

f34b <- df %>%
  filter(species == "Rato") %>%
  ggplot(aes(x = sex, y = age, fill = sex, color = sex)) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "grey80")) +
  scale_color_manual(values = c("#692b75", "#006f9f", "grey80")) +
  geom_boxplot(
    width = .2, fill = "white",
    size = .3, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .5, ## bandwidth
    width = .67, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .3, size = .1
  ) +
  labs(y = "Idade (dias)",
       x = "Rato",
       fill = "Sexo",
       title = "b") +
  scale_y_continuous(n.breaks = 6) +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0.5),
    axis.title.y = element_text(hjust = 1, margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, 0, 5, 0)
  )



f34c <- df %>%
  filter(species == "Camundongo") %>%
  ggplot(aes(x = sex, y = weight, color = sex, fill = sex)) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  scale_color_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  geom_boxplot(
    width = .2, fill = "white",
    size = .3, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .5, ## bandwidth
    width = .67, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .3, size = .1
  ) +
  labs(y = "Peso (g)",
       x = "Camundongo",
       fill = "Sexo",
       title = "c") +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0.5),
    axis.title.y = element_text(hjust = 1, margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f34d <- df %>%
  filter(species == "Rato") %>%
  ggplot(aes(x = sex, y = weight, fill = sex, color = sex)) +
  scale_fill_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  scale_color_manual(values = c("#692b75", "#006f9f", "#009c7e", "grey80")) +
  geom_boxplot(
    width = .2, fill = "white",
    size = .3, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .5, ## bandwidth
    width = .67, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .3, size = .1
  ) +
  labs(y = "Peso (g)",
       x = "Rato",
       fill = "Sexo",
       title = "d") +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0.5),
    axis.title.y = element_text(hjust = 1, margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Combinar e salvar

f34 <-
  (f34a + f34b) /  (f34c + f34d) + plot_layout(heights = c(8, 8))
save_plot(filename = "Figura3e4_ppt.png",
          plot = f34,
          dpi = 300)

## Figura5: Modelo animal -----

df %>% group_by(study_reference, model_phenotype) %>%
  slice(1) %>%
  group_by(model_phenotype, species) %>%
  filter(model_phenotype == "NA",) %>%
  summarise(counts = n()) # calcular quantas publicaçoes são NA

# Modelos por publicação

f5a <- df %>%
  group_by(study_reference) %>% 
  distinct(model_phenotype, species) %>% # deixar so um estudo por referencia para modelo e especie distintas
  group_by(model_phenotype, species) %>% 
  filter(model_phenotype != "NA", ) %>%
  summarise(counts = n()) %>% 
  ggplot(aes(
    x = factor(model_phenotype),
    y = counts,
    fill = model_phenotype,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de publicações", x = "Modelo") +
  scale_fill_manual(values = c("#82c236", "#692b75")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 8), expand = c(0, 0), n.breaks = 3) +
  facet_wrap( ~ species, strip.position = "top") +
  gghighlight(counts >= 3,
              calculate_per_facet = TRUE,
              label_key = model_phenotype) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.x = element_text(size = 5),
    axis.title = element_text(size = 7),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 6),
    plot.margin = margin(10, 10, 0, 0)
  )


# Modelos por estudo

df %>% group_by(model_phenotype, species) %>%
  filter(model_phenotype == "NA", ) %>%
  summarise(counts = n())  # calcular quanto estudos sao NA

f5b <- df %>%
  group_by(model_phenotype, species) %>%
  filter(model_phenotype != "NA", ) %>%
  summarise(counts = n()) %>% 
  ggplot(aes(
    x = factor(model_phenotype),
    y = counts,
    fill = model_phenotype,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de estudos", x = "Modelo") +
  scale_fill_manual(values = c("#f24a7a","#82c236", "#006f9f","#692b75")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 12), expand = c(0, 0), n.breaks = 4) +
  facet_wrap( ~ species, strip.position = "top") +
  gghighlight(counts > 5,
              calculate_per_facet = TRUE,
              label_key = model_phenotype) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey30"
    ),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 6),
    plot.margin = margin(10, 10, 0, 0)
  )

# Modelos em qtd de animais

df %>%
  group_by(model_phenotype, species) %>%
  filter(model_phenotype == "NA") %>%
  summarise(counts = sum(N)) # calcular quanto animais sao NA

f5c <- df %>%
  group_by(model_phenotype, species) %>%
  filter(model_phenotype != "NA") %>%
  summarise(counts = sum(N)) %>%
  ggplot(aes(
    x = factor(model_phenotype),
    y = counts,
    fill = model_phenotype,
    label = counts
  )) +
  geom_bar(color = "black", size = 0.2, stat = "identity") +
  labs(y = "Nº de animais", x = "Modelo animal") +
  scale_fill_manual(values = c("#f24a7a", "#82c236","#006f9f", "#692b75", "#ff9400")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0)) +
  facet_wrap( ~ species, strip.position = "top") +
  gghighlight(counts > 75,
              calculate_per_facet = TRUE,
              label_key = model_phenotype) +
  geom_text(
    color = "mintcream",
    size = 2,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    hjust = 1.1
  ) +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey30"
    ),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 6),
    plot.margin = margin(10, 10, 0, 0)
  )

# Combinar e salvar

Figura5 <- f5a + f5b + f5c

Figura5
save_plot(filename = "Figura5.png",
          plot = Figura5,
          dpi = 300)


# ACONDICIONAMENTO
## Figura 6: Ciclo, temperatura, umidade -----

# Ciclo

F6a <- df %>%
  group_by(study_reference, bioterium_lightcycle) %>%
  slice(1) %>%
  group_by(bioterium_lightcycle) %>%
  count() %>%
  ggplot(aes(
    x = fct_infreq(bioterium_lightcycle),
    y = n,
    fill = bioterium_lightcycle,
    label = n
  )) +
  geom_bar(stat = "identity") +
  labs(y = "Nº de publicações", x = "Ciclo de luz do biotério (h/h)", title = "a") +
  scale_fill_manual(values = c("#fec200", "#fec200", "grey80", "#fec200", "#fec200", "#fec200")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  geom_text(
    size = 2.5,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(10, 0, 10, 10)
  )

# Temperatura

# filtrar as publicacoes e remover sem informação
filtro_bioterium_temp <- df %>%
  group_by(study_reference) %>%
  slice(1) %>%
  filter(bioterium_temp != "NA") %>%
  select(bioterium_temp)

m_t <-
  data.frame(label = paste(round(
    mean(filtro_bioterium_temp$bioterium_temp), 1
  ), round(
    sd(filtro_bioterium_temp$bioterium_temp), 1
  ), sep = "±")) # criei um data com o valor da media e dp arredondados


F6b <- df %>%
  group_by(study_reference) %>%
  slice(1) %>%
  ggplot(aes(x = bioterium_temp)) +
  geom_histogram(fill = "#a6243a") +
  scale_y_continuous(expand = c(0,0), limits = c(0,60), n.breaks = 4) +
  geom_text(
    data = m_t,
    aes(label = label),
    x = Inf ,
    y = 57,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) +
  geom_vline(data = filtro_bioterium_temp,
             aes(xintercept = mean(bioterium_temp)),
             col = "gold2",
             size = .5) +
  geom_vline(
    data = filtro_bioterium_temp,
    aes(xintercept = (sd(bioterium_temp)) + 22.44694),
    col = "gold",
    size = .5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = filtro_bioterium_temp,
    aes(xintercept = (22.44694 - sd(bioterium_temp))),
    col = "gold",
    size = .5,
    linetype = "dashed"
  ) +
  labs(y = "Nº de publicações", x = "Temperatura do biotério (°C)") +
  labs(title = "b") +
  theme(
    axis.line = element_line(size = .2),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 10, 10)
  )

F6b
#  Umidade

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
  geom_histogram(fill = "#006f9f") +
  scale_y_continuous(expand = c(0,0), limits = c(0,30), n.breaks = 4) +
  scale_x_continuous(n.breaks = 6) +
  geom_text(
    data = m_u,
    aes(label = label),
    x = Inf ,
    y = 28.5,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) +
  geom_vline(data = filtro_bioterium_umid,
             aes(xintercept = mean(bioterium_umid)),
             col = "gold2",
             size = .5) +
  geom_vline(
    data = filtro_bioterium_umid,
    aes(xintercept = (sd(bioterium_umid)) + 55.3),
    col = "gold",
    size = .5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = filtro_bioterium_umid,
    aes(xintercept = (55.3 - sd(bioterium_umid))),
    col = "gold",
    size = .5,
    linetype = "dashed"
  ) +
  labs(y = "Nº de publicações", x = "Umidade do biotério (%)") +
  labs(title = "c") +
  theme(
    axis.line = element_line(size = .2),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 10, 10)
  )



Figura6 <-  F6a + (F6b / F6c) + plot_layout(widths = c(6, 4))
Figura6

save_plot(filename = "Figura6.png",
          plot = Figura6,
          dpi = 300)

## Figura 7: Volume caixa por animal, volume de caixa por peso do animal ----

cage_m <- df %>%
  select(cage_measures,
         weight,
         study_reference,
         animals_percage,
         species) %>%
  group_by(study_reference) %>%
  slice(1) %>%
  separate(
    col = cage_measures,
    sep = c("x", "X", "×"),
    into = c("c1", "c2", "c3")
  ) %>%
  mutate(c1 = as.numeric(c1),
         c2 = as.numeric(c2),
         c3 = as.numeric(c3)) %>%
  filter(c2 != "NA") # separei medidas da caixa em novas variaveis


cage_3d <- cage_m %>%
  filter(c3 != "NA") %>% # retirei estudos que nao deram medidas dos três lados da caixa
  mutate(volume_cx = as.numeric(c1 * c2 * c3),# calculei volume da caixa
         animals_percage = as.numeric(animals_percage))
cage_3d <- cage_3d %>%
  mutate(vol_panimal = as.numeric(volume_cx / animals_percage), # nova variavel: volume de caixa por animal
         vol_ppeso = as.numeric(((
           volume_cx / animals_percage
         ) / weight))) # nova variavel: volume de caixa por peso do animal


# Criar um df com os valores da media e dp arredondados

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


# Vincular valores da media e sd com especie

vol_panimal_label <- data.frame(
  species = c("Camundongo", "Rato"),
  label = paste(
    vol_panimal_meansd$numeric.mean,
    vol_panimal_meansd$numeric.sd,
    sep = "±"
  )
)


vol_ppeso_label <- data.frame(
  species = c("Camundongo", "Rato"),
  label = paste(
    vol_ppeso_meansd$numeric.mean,
    vol_ppeso_meansd$numeric.sd,
    sep = "±"
  )
)

# Plotar grafico de volume de caixa por animal

F7a <- cage_3d %>%
  ggplot(aes(x = vol_panimal, fill = species)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid( ~ species, scales = "free_x") +
  geom_text(
    data = vol_panimal_label,
    aes(label = label),
    x = Inf ,
    y = 2.9,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) +
  labs(y = "Nº de publicações", x = "Volume de caixa por animal (cm³)") +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c("#ff9400", "#ec2b2b")) +
  labs(title = "a") +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "panel",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(20, 0, 0, 0)
  )


# Plotar grafico de volume de caixa por peso de animal

F7b <- cage_3d %>%
  ggplot(aes(x = vol_ppeso, fill = species)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid( ~ species, scales = "free_x") +
  geom_text(
    data = vol_ppeso_label,
    aes(label = label),
    x = Inf ,
    y = 2.9,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) +
  labs(y = "Nº de publicações", x = "Volume de caixa por peso do animal (cm³/g)") +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c("#ff9400", "#ec2b2b")) +
  labs(title = "b") +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 0),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "panel",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 20, 0)
  )

# Combinar e salvar

Figura7 <- F7a / F7b + plot_layout(heights = c(5, 5))

save_plot(filename = "Figura7.png",
          plot = Figura7,
          dpi = 300)


## Figura7ab: Volume caixa por animal, volume de caixa por peso do animal ----

# Plotar grafico de volume de caixa por animal

f7ab <- cage_3d %>%
  ggplot(aes(x = species, y = vol_panimal, color = species, fill = species)) +
  scale_fill_manual(values = c("#ff9400", "#ec2b2b"), guide = "none") +
  scale_color_manual(values = c("#ff9400", "#ec2b2b"), guide = "none") +
  geom_boxplot(
    width = .2, fill = "white",
    size = .5, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .33, ## bandwidth
    width = .5, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .5, size = .5
  ) +
  labs(y = "Volume de caixa por animal (cm³)",  title = "a") +
  scale_y_continuous(n.breaks = 8, expand = c(0,0), limits = c(0, 35000)) +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 0, 5, 20)
  )
f7ab

# Plotar grafico de volume de caixa por peso de animal

f7ba <- cage_3d %>%
  ggplot(aes(x = species, y = vol_ppeso, fill = species, color = species)) +
  scale_fill_manual(values = c("#ff9400", "#ec2b2b"), guide = "none") +
  scale_color_manual(values = c("#ff9400", "#ec2b2b"), guide = "none") +
  geom_boxplot(
    width = .2, fill = "white",
    size = .5, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .33, ## bandwidth
    width = .5, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .5, size = .5
  ) +
  labs(y = "Volume de caixa por peso do animal (cm³/g)",  title = "b") +
  scale_y_continuous(n.breaks = 8, expand = c(0,0), limits = c(0, 250)) +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 0, 5, 20)
  )

# Combinar e salvar

Figura7ab <- f7ab | f7ba

save_plot(filename = "Figura7_ppt.png",
          plot = Figura7ab,
          dpi = 300)

# INTERVENÇÃO
## Figura8: classes no tempo ----

f8a <- df %>%
  group_by(atd_class) %>%
  summarise(counts = n()) %>% 
  mutate(atd_class = fct_reorder(atd_class, desc(counts))) %>% # reordernar categoria de acordo com frequencia
  ggplot(aes(
    x = fct_infreq(atd_class),
    y = counts,
    fill = fct_infreq(atd_class),
    label = counts
  )) +
  geom_bar(stat = "identity") +
  geom_text(
    size = 2.5,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  labs(y = "Nº de estudos", x = "Classe", title = "a") +
  scale_fill_manual(values = met.brewer("Signac", 10)) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) +
  scale_y_continuous(limits = c(0, 265), expand = c(0, 0)) +
  theme(
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 8, hjust = 1),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(15, 0, 0, 0)
  )

f8b <- df  %>% 
  ggplot(aes(x = year, fill = fct_infreq(atd_class))) +
  geom_bar() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(y = "Nº de estudos",
       x = "Ano",
       title = "b",
       fill = "Classe de antidepressivo") +
  scale_fill_manual(values = met.brewer("Signac", 10)) +
  scale_y_continuous(expand = c(0,0), n.breaks = 4) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(10, 0, 0, 10),
    panel.grid.major = element_line(color = "grey90", size = .1)
  )

Figura8 <- f8a / f8b
Figura8
# Salvar

save_plot(filename = "Figura8.png",
          plot = Figura8,
          dpi = 300)


## Figura9: Classe, antidepressivos e doses CAMUNDONGO -----

# classe e antidepressivo CAMUNDONGO

f9a <- df %>%
  filter(dose_unit == "mg/kg",
    species == "Camundongo") %>%
  ggplot(aes(
    x = fct_lump(fct_infreq(atd_class), n = 6, other_level = "Outros"),
    fill = fct_lump_n(fct_infreq(atd_type), n = 15, other_level = "Outros"))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count",
            color = "mintcream",
            size = 2,
            family = "Gadugi",
            position = "stack",
            vjust = 1.2) +
  labs(y = "Nº de estudos", x = "Classe", title = "a") +
  scale_fill_manual(
    values = c(
      "#ffe170",
      "#fec200",
      "#ff9400",
      "#ec2b2b",
      "#a6243a",
      "#f24a7a",
      "#f39da4",
      "#b376a2",
      "#a94f93",
      "#692b75",
      "#006f9f",
      "turquoise2",
      "#009c7e",
      "#82c236",
      "olivedrab2",
      "grey80"
    ),
    name = "Antidepressivos"
  ) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) + # legenda em mais de uma linha
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    legend.text = element_text(size = 4, color = "grey20"),
    legend.title = element_text(size = 5),
    plot.margin = margin(10, 0, 10, 10),
    legend.key.size = unit(.8, "line"),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank()
  )

f9a
# dose CAMUNDONGO

f9b <- df %>% 
  filter(dose_unit == "mg/kg",
         species == "Camundongo") %>% 
  ggplot(aes(x = dose, y = fct_lump_n(fct_rev(fct_infreq(atd_type)), n = 15, other_level = "Outros"), color = fct_lump_n(fct_infreq(atd_type), n = 15, other_level = "Outros"), fill = fct_lump_n(fct_infreq(atd_type), n = 15, other_level = "Outros"))) + 
  ggridges::stat_density_ridges(
    alpha = .7, size = .2, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2
  ) + 
  labs(x = "Dose", title = "b", y = "Fármacos") +
  scale_fill_manual(
    values = c(
      "#ffe170",
      "#fec200",
      "#ff9400",
      "#ec2b2b",
      "#a6243a",
      "#f24a7a",
      "#f39da4",
      "#b376a2",
      "#a94f93",
      "#692b75",
      "#006f9f",
      "turquoise2",
      "#009c7e",
      "#82c236",
      "olivedrab2",
      "grey80"
    )) +
  scale_color_manual(
    values = c(
      "#ffe170",
      "#fec200",
      "#ff9400",
      "#ec2b2b",
      "#a6243a",
      "#f24a7a",
      "#f39da4",
      "#b376a2",
      "#a94f93",
      "#692b75",
      "#006f9f",
      "turquoise2",
      "#009c7e",
      "#82c236",
      "olivedrab2",
      "grey80"
    )) +
  scale_x_continuous(expand = c(0, 0), n.breaks = 6, limits = c(-15, 120)) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(t = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    legend.key.size = unit(.5, "line"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) 
   
f9b

# Juntar
Figura9 <- f9a | f9b 
Figura9
#Salvar
save_plot(filename = "Figura9.png",
          plot = Figura9,
          dpi = 300)


## Figura10: Classe, antidepressivos e doses Rato  -----

f10a <- df %>%
  filter(dose_unit == "mg/kg",
    species == "Rato") %>%
  ggplot(aes(
    x = fct_lump(fct_infreq(atd_class), n = 4, other_level = "Outros"),
    fill = fct_lump_n(fct_infreq(atd_type), n = 12, other_level = "Outros"))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count",
            color = "mintcream",
            size = 2,
            family = "Gadugi",
            position = "stack",
            vjust = 1.2) +
  labs(y = "Nº de estudos", x = "Classe", title = "a") +
  scale_fill_manual(
    values = c(
      "#ffe170",
      "#fec200",
      "#ff9400",
      "#ec2b2b",
      "#a6243a",
      "#f24a7a",
      "#f39da4",
      "#a94f93",
      "#692b75",
      "#006f9f",
      "#009c7e",
      "#82c236",
      "grey80"
    ),
    name = "Antidepressivos"
  ) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    legend.text = element_text(size = 4, color = "grey20"),
    legend.title = element_text(size = 5),
    plot.margin = margin(10, 0, 10, 10),
    legend.key.size = unit(.8, "line"),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.major.x = element_blank()
  )
f10a
# dose Rato

f10b <- df %>% 
  filter(dose_unit == "mg/kg",
         species == "Rato") %>% 
  ggplot(aes(x = dose, y = fct_lump_n(fct_rev(fct_infreq(atd_type)), n = 12, other_level = "Outros"), color = fct_lump_n(fct_infreq(atd_type), n = 12, other_level = "Outros"), fill = fct_lump_n(fct_infreq(atd_type), n = 12, other_level = "Outros"))) + 
  ggridges::stat_density_ridges(
    alpha = .7, size = .1, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2
  ) + 
  labs(x = "Dose", title = "b", y = "Fármacos") +
  scale_fill_manual(
    values = c(
      "#ffe170",
      "#fec200",
      "#ff9400",
      "#ec2b2b",
      "#a6243a",
      "#f24a7a",
      "#f39da4",
      "#a94f93",
      "#692b75",
      "#006f9f",
      "#009c7e",
      "#82c236",
      "grey80"
    )) +
  scale_color_manual(
    values = c(
      "#ffe170",
      "#fec200",
      "#ff9400",
      "#ec2b2b",
      "#a6243a",
      "#f24a7a",
      "#f39da4",
      "#a94f93",
      "#692b75",
      "#006f9f",
      "#009c7e",
      "#82c236",
      "grey80"
    )) +
  scale_x_continuous(expand = c(0, 0), n.breaks = 6, limits = c(-15, 120)) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = 1),
    axis.title.y = element_text(margin = margin(t = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    legend.key.size = unit(.5, "line"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) 

f10b

Figura10 <- f10a + f10b
Figura10

save_plot(filename = "Figura10.png",
          plot = Figura10,
          dpi = 300)

# isolar estudos com outras unidades de dose que nao mg/kg
atd <- df %>% 
  filter(dose_unit != "mg/kg") %>% 
  group_by(study_reference) %>% 
  distinct(dose_unit) %>% 
  count(dose_unit, sort = T)

# criar a salvar tabela 
unidades_dose <- tibble(atd)
write.table(atd , file = "data\\dose_otherunits.xlsx")

## Figura11 e 12: Via de administração x frequencia adm x tempo de adm ----

# Vias de adm e frequencia de adm

f11a <- df %>% 
  filter(species == "Camundongo") %>% 
  ggplot(aes(x = fct_infreq(treatment_via), fill = treatment_freq)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = ..count..), stat = "count",
            color = "black",
            size = 2,
            family = "Gadugi",
            position = position_dodge2(preserve = "single", width = 1),
            vjust = -.25,
  ) +
  scale_y_continuous(limits = c(0, 230), expand = c(0, 0)) +
  labs(y = "Nº de estudos", x = "Via de administração", title = "a") +
  scale_fill_manual(values = c("1" = "#fec200", "2" = "#009c7e", "3" = "#a94f93")) +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 7, hjust = 1),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f11a

# vias de adm, frequencia de adm e duracao do tratamento

f11b <- df %>%
  filter(species == "Camundongo") %>%
  ggplot(aes(y = treatment_duration, x = fct_infreq(treatment_via))) +
  geom_point(
    aes(color = treatment_freq),
    position = position_jitterdodge(),
    size = .8,
    shape = 19,
    alpha = .5
  ) +
  labs(y = "Duração do tratamento (dias)", x = "Via de administração", title = "b") +
  scale_color_manual(
    name = "Administrações/dia",
    values = c(
      "1" = "#fec200",
      "2" = "#009c7e",
      "3" = "#a94f93",
      "Sem info" = "grey30"
    )
  ) +
  scale_y_continuous(expand = c(.01, 0), limits = c(0, 120)) +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10, 0,-10,-10),
    legend.spacing.x = unit(0.001,'cm'),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.minor.y = element_line(color = "grey90", size = .1)
  )
f11b

F11 <- (f11a + plot_spacer() + plot_layout(widths = c(6,3))) / (f11b + plot_spacer() + plot_layout(widths = c(6,3))) +  plot_layout(heights = c(3,8))

F11
save_plot(filename = "Figura11.png",
          plot = F11,
          dpi = 300)

# stat da duracao da adm por via de adm

my_skim <- skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE), iqr = ~ IQR(., na.rm = TRUE)),
                     base = sfl(complete = n_complete))

stat_t_d_cam <- df %>%
  filter(species == "Camundongo") %>% 
  group_by(treatment_via) %>%
  my_skim(treatment_duration) %>%
  tibble::as_tibble()


write_xlsx(stat_t_d_cam,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\res\\treat_dur_stat_cam.xlsx")


# RATOS

# Vias de adm e frequencia de adm

f12a <- df %>% 
  filter(species == "Rato") %>% 
  ggplot(aes(x = fct_lump_n(fct_infreq(treatment_via), n = 8, other_level = "Outros"), fill = treatment_freq)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = ..count..), stat = "count",
            color = "black",
            size = 2,
            family = "Gadugi",
            position = position_dodge2(preserve = "single", width = 1),
            vjust = -.25,
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) +
  labs(y = "Nº de estudos", x = "Via de administração", title = "a") +
  scale_fill_manual(values = c("1" = "#fec200", "2" = "#009c7e", "3" = "#a94f93", "Sem info" = "grey80")) +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 7, hjust = 1),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f12a

# vias de adm, frequencia de adm e duracao do tratamento

f12b <- df %>%
  filter(species == "Rato") %>%
  ggplot(aes(y = treatment_duration, x = fct_lump_n(fct_infreq(treatment_via), n = 8, other_level = "Outros"))) +
  geom_point(
    aes(color = treatment_freq),
    position = position_jitterdodge(),
    size = .8,
    shape = 19,
    alpha = .5
  ) +
  labs(y = "Duração do tratamento (dias)", x = "Via de administração", title = "b") +
  scale_color_manual(
    name = "Administrações/dia",
    values = c(
      "1" = "#fec200",
      "2" = "#009c7e",
      "3" = "#a94f93",
      "Sem info" = "grey30"
    )
  ) +
  scale_y_continuous(expand = c(.001, 0), limits = c(0, 50)) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) +
  theme_classic(base_family = "Gadugi") +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10, 0,-10,-10),
    legend.spacing.x = unit(0.001,'cm'),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.minor.y = element_line(color = "grey90", size = .1)
  )

f12b

F12 <- f12a  / f12b  +  plot_layout(heights = c(3,8))

F12
save_plot(filename = "Figura12.png",
          plot = F12,
          dpi = 300)

# stat da duracao da adm por via de adm

my_skim <- skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE), iqr = ~ IQR(., na.rm = TRUE)),
                     base = sfl(complete = n_complete))

stat_t_d_rat <- df %>%
  filter(species == "Rato") %>% 
  group_by(treatment_via) %>%
  my_skim(treatment_duration) %>%
  tibble::as_tibble()


write_xlsx(stat_t_d_rat,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\res\\treat_dur_stat_rat.xlsx")


# DESFECHO

##Figura13: protocolo x metodos de analise / tamanho x diametro cuba / altura agua x temperatura agua -----






#CAMUNDONGO


f2a <- df %>%
  filter(species == "Camundongo") %>%
  group_by(study_reference) %>% 
  distinct(strain) %>% 
  group_by(strain) %>% 
  summarise(counts = n()) 
  


f13a <- df %>%
  filter(species == "Camundongo") %>%
  group_by(study_reference) %>% 
  distinct(fst_protocol) %>% 
  group_by(fst_protocol) %>% 
  summarise(counts = n()) %>% 
  mutate(fst_protocol = fct_reorder(fst_protocol, desc(counts))) %>%
  ggplot(aes(x = fct_lump_n(fst_protocol, n = 3, other_level = "Outros"), y = counts)) +
  geom_col(fill = "turquoise2") +
  labs(y = "Nº de publicações", x = "Protocolo de nado forçado", title = "a") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60)) +
  geom_text(aes(label = counts),
    size = 2.5,
    family = "Gadugi",
    position = position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(10, 0, 10, 10)
  )


f13a


#RATO



#STAT NADO

my_skim <- skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE), iqr = ~ IQR(., na.rm = TRUE)),
                     base = sfl(complete = n_complete))

stat_nado <- df %>%
  group_by(species) %>%
  my_skim(fst_protocol, measurement_method, cylinder_height, cylinder_diameter, water_depth, water_temperature) %>%
  tibble::as_tibble()

write_xlsx(stat_nado,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\res\\stat_nado.xlsx")

