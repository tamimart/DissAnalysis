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


# Instalar fontes ----

# Rodar uma vez:
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import(paths = "C:\\Windows\\Fonts")
# loadfonts(device = "win")
# fonts() # Ver opcoes de fonte


# Transformação dos dados e padronizações ----- 

# Carregar dataframe dos dados limpos e organizados

df <-
  data_geral_clean <-
  readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")

# Estabelecer tema base para todos os próximos gráficos

theme_set(theme_minimal(base_family = "Gadugi"))

# Estabeler dados de resumo estatistico

my_skim <- skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE), 
                                   iqr = ~ IQR(., na.rm = TRUE)),
                     base = sfl(complete = n_complete,
                                missing = n_missing,
                                n = length))

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
      "subcutaneous"
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
      "Subcutânea"
    )
  ) 



df %>% 
  filter(treatment_via == "Tablete")
#protocolo tnf

df$fst_protocol <-
  factor(
    df$fst_protocol,
    levels = c("NA",
               "pre?test6score4",
               "pre13test6",
               "pre15score5",
               "pre15test?",
               "pre15test10",
               "pre15test15",
               "pre15test5",
               "pre15test5(d1)test5(d7)",
               "pre15test6",
               "pre15test6score4",
               "pre15test6score5",
               "pre20test5",
               "pre5test5",
               "pre6test6score5",
               "pre7x15test15",
               "test10",
               "test15",
               "test15score13",
               "test15score5",           
               "test15score5to10",
               "test15score6",
               "test5",
               "test5score4",
               "test5scorefirst2",
               "test6",
               "test6score4",
               "test6score5",
               "test7score6",
               "test9"           
    ),
    labels = c(
      "Sem info",
      "Pré-teste ?' + teste 6' + score 4'final",
      "Pré-teste 13' + teste 6'",
      "Pré-teste 15' + score 5'final",
      "Pré-teste 15' + teste ?'",
      "Pré-teste 15' + teste 10'",
      "Pré-teste 15' + teste 15'",
      "Pré-teste 15' + teste 5'",
      "Pré-teste 15' + teste 5' (dia 1) + teste 5' (dia 7)",
      "Pré-teste 15' + teste 6'",
      "Pré-teste 15' + teste 6' + score 4'final",
      "Pré-teste 15' + teste 6' + score 5'final",
      "Pré-teste 20' + teste 5'",
      "Pré-teste 5' + teste 5'",
      "Pré-teste 6' + teste 6' + score 5'final",
      "Pré-teste 7x15' + teste 15'",
      "Teste 10'",
      "Teste 15'",
      "Teste 15' + score 13'final",
      "Teste 15' + score 5'final",           
      "Teste 15' + score 5'meio",
      "Teste 15' + score 6'final",
      "Teste 5'",
      "Teste 5' + score 4'final",
      "Teste 5' + score 2'inicial",
      "Teste 6'",
      "Teste 6' + score 4'final",
      "Teste 6 + score 5'final",
      "Teste 7' + score 6'final",
      "Teste 9'"
    )
  ) 


#fst analise

df$measurement_method <-
  factor(
    df$measurement_method,
    levels = c(
      "manually",
      "manually, chronometers",
      "manually, score60sinterval",
      "video analysis, automated",
      "NA",
      "Unclear, score5sinterval",            
      "Unclear",
      "video analysis",
      "video analysis, chronometers",
      "video analysis, manual",
      "video analysis, manual and automated", 
      "video analysis, score5sinterval"
    ),
    labels = c(
      "Manual",
      "Manual com cronômetro",
      "Manual, intervalos de 60s",
      "Videoanálise automatizada",
      "Sem info",
      "Incerto, intervalos de 60s",            
      "Incerto",
      "Videoanálise",
      "Videoanálise com cronômetro",
      "Videoanálise manual",
      "Videoanálise manual e automatizada", 
      "Videoanálise, intervalos de 5s"
    )
  ) 



# PUBLICAÇÃO
## Figura1: Paises e idioma ---------

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

F1 <-
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


F11996 <-
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


F12006 <-
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

F1idi <- df %>%
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

Figura1 <-
  (F1 + inset_element(
    F1idi,
    left = 0.05,
    bottom = 0.05,
    right = 0.28,
    top = 0.25
  )) / (plot_spacer() |
          F11996 |
          F12006 |
          plot_spacer()) + plot_layout(heights = c(10, 2), widths = c(8, 8))
save_plot(filename = "Figura1.png",
          plot = Figura1,
          dpi = 300) # Salvar gráfico


## Figura2 - Quantidade de publicações no ano ----

Figura2 <- df %>%
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

save_plot(filename = "Figura2.png",
          plot = Figura2,
          dpi = 300) # Salvar gráfico



# POPULAÇÃO 
## Figura3: Sexo, especie, tempo ----


f3a <- df %>%
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

f3b <- df  %>% # especie no tempo
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

f3c <- df %>%
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

f3d <- df  %>% # sexo no tempo
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

Figura3 <-
  f3a + f3b + f3c + f3d + plot_layout(heights = c(6, 6), widths = c(3, 6))
save_plot(filename = "Figura3.png",
          plot = Figura3,
          dpi = 300)

## Figura4: Linhagens ----
# Strain
  
f4a <- df %>%
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

f4b <- df %>%
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

f4c <- df %>%
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


f4d <- df %>%
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


f4e <- df %>%
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

f4f <- df %>%
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

Fig4t <- (f4a + f4b + f4c) / (f4d + f4e + f4f) + plot_layout(heights = c(11, 6), guides = 'collect')


save_plot(filename = "Figura4.png",
          plot = Fig4t,
          dpi = 300)


## Figura5 e Figura6: Idade e peso ----

# Idade

# criar um df com os valores da media e dp arredondados

age_ss <- df %>%
  group_by(species, sex) %>%
  my_skim(age) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))

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
    label = paste(age_ss$numeric.median, " ", "(", age_ss$numeric.p25,"-",age_ss$numeric.p75,")", sep = "")
  )

f5 <- df %>%
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
  my_skim(weight) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))

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
    label = paste(weight_ss$numeric.median, " ", "(",  weight_ss$numeric.p25, "-", weight_ss$numeric.p75, ")", sep = "")
  )


f6 <- df %>%
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

save_plot(filename = "Figura5.png",
          plot = f5,
          dpi = 300)
save_plot(filename = "Figura6.png",
          plot = f6,
          dpi = 300)

## Figura5e6ppt: Idade e peso ----

f56a <- df %>%
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

f56b <- df %>%
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



f56c <- df %>%
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

f56d <- df %>%
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

f56 <-
  (f56a + f56b) /  (f56c + f56d) + plot_layout(heights = c(8, 8))
save_plot(filename = "Figura5e6_ppt.png",
          plot = f56,
          dpi = 300)

## Figura7: Modelo animal -----

df %>% group_by(study_reference, model_phenotype) %>%
  slice(1) %>%
  group_by(model_phenotype, species) %>%
  filter(model_phenotype == "NA",) %>%
  summarise(counts = n()) # calcular quantas publicaçoes são NA

# Modelos por publicação

f7a <- df %>%
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

f7b <- df %>%
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

f7c <- df %>%
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

Figura7 <- f7a + f7b + f7c

Figura7
save_plot(filename = "Figura7.png",
          plot = Figura7,
          dpi = 300)


# ACONDICIONAMENTO
## Figura 8: Ciclo, temperatura, umidade -----

# Ciclo

F8a <- df %>%
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
  scale_fill_manual(values = c("#fec200", "#ffe170", "grey80", "#ffe170", "#ffe170", "#ffe170")) +
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

filtro_bioterium_temp <- filtro_bioterium_temp %>% 
  ungroup() %>% 
  my_skim(bioterium_temp) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))

m_t <-
  data.frame(label = paste(filtro_bioterium_temp$numeric.median, " ", "(",  filtro_bioterium_temp$numeric.p25, "-", filtro_bioterium_temp$numeric.p75, ")", sep = ""))

F8b <- df %>%
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
             aes(xintercept = numeric.p50),
             col = "gold2",
             size = .5) +
  geom_vline(
    data = filtro_bioterium_temp,
    aes(xintercept = numeric.p25),
    col = "gold",
    size = .5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = filtro_bioterium_temp,
    aes(xintercept = numeric.p75),
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

F8b
#  Umidade

# filtrar as publicacoes e remover sem informação


filtro_bioterium_umid <- df %>%
  group_by(study_reference) %>%
  slice(1) %>%
  filter(bioterium_umid != "NA") %>%
  select(bioterium_umid) 

filtro_bioterium_umid <- filtro_bioterium_umid %>% 
  ungroup() %>% 
  my_skim(bioterium_umid) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))

m_u <-
  data.frame(label = paste(filtro_bioterium_umid$numeric.median, " ", "(",  filtro_bioterium_umid$numeric.p25, "-", filtro_bioterium_umid$numeric.p75, ")", sep = ""))


F8c <- df %>%
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
             aes(xintercept = numeric.p50),
             col = "gold2",
             size = .5) +
  geom_vline(
    data = filtro_bioterium_umid,
    aes(xintercept = numeric.p25),
    col = "gold",
    size = .5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = filtro_bioterium_umid,
    aes(xintercept = numeric.p75),
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



Figura8 <-  F8a + (F8b / F8c) + plot_layout(widths = c(6, 4))
Figura8

save_plot(filename = "Figura8.png",
          plot = Figura8,
          dpi = 300)

## Figura 9: Volume caixa por animal, volume de caixa por peso do animal ----

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

vol_panimal_miqr <- cage_3d  %>%
  group_by(species) %>%
  my_skim(vol_panimal) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))

vol_ppeso_miqr <- cage_3d  %>%
  group_by(species) %>%
  my_skim(vol_ppeso) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))


# Vincular valores da media e sd com especie

vol_panimal_label <- data.frame(
  species = c("Camundongo", "Rato"),
    label = paste(vol_panimal_miqr$numeric.median, " ", "(",  vol_panimal_miqr$numeric.p25, "-", vol_panimal_miqr$numeric.p75, ")", sep = "")
)


vol_ppeso_label <- data.frame(
  species = c("Camundongo", "Rato"),
  label = paste(vol_ppeso_miqr$numeric.median, " ", "(",  vol_ppeso_miqr$numeric.p25, "-", vol_ppeso_miqr$numeric.p75, ")", sep = "")
)

# Plotar grafico de volume de caixa por animal

F9a <- cage_3d %>%
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

F9b <- cage_3d %>%
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

Figura9 <- F9a / F9b + plot_layout(heights = c(5, 5))

save_plot(filename = "Figura9.png",
          plot = Figura9,
          dpi = 300)


## Figura9ppt: Volume caixa por animal, volume de caixa por peso do animal ----

# Plotar grafico de volume de caixa por animal

f9ab <- cage_3d %>%
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
f9ab

# Plotar grafico de volume de caixa por peso de animal

f9ba <- cage_3d %>%
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

Figura9ab <- f9ab | f9ba

save_plot(filename = "Figura9_ppt.png",
          plot = Figura9ab,
          dpi = 300)

# INTERVENÇÃO
## Figura10: classes no tempo ----

f10a <- df %>%
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

f10b <- df  %>% 
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

Figura10 <- f10a / f10b
Figura10
# Salvar

save_plot(filename = "Figura10.png",
          plot = Figura10,
          dpi = 300)


## Figura11: Classe, antidepressivos e doses CAMUNDONGO -----

# classe e antidepressivo CAMUNDONGO

f11a <- df %>%
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
  labs(y = "Nº de estudos", x = "Classes administradas em camundongos", title = "a") +
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

f11a
# dose CAMUNDONGO

f11b <- df %>% 
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
   
f11b

# Juntar
Figura11 <- f11a | f11b 
Figura11
#Salvar
save_plot(filename = "Figura11.png",
          plot = Figura11,
          dpi = 300)


## Figura12: Classe, antidepressivos e doses RATO  -----

f12a <- df %>%
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
  labs(y = "Nº de estudos", x = "Classes administradas em ratos", title = "a") +
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
f12a
# dose Rato

f12b <- df %>% 
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

f12b

Figura12 <- f12a + f12b
Figura12

save_plot(filename = "Figura12.png",
          plot = Figura12,
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

## Figura13 e Figura14: Via de administração x frequencia adm x tempo de adm ----

# Vias de adm e frequencia de adm

f13a <- df %>% 
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
  labs(y = "Nº de estudos", x = "Via de administração em camundongos", title = "a") +
  scale_fill_manual(values = c("1" = "#fec200", "2" = "#009c7e", "3" = "#a94f93")) +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.x = element_text(size = 7, hjust = 1, vjust = 35),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f13a

# vias de adm, frequencia de adm e duracao do tratamento

stat_t_d_cam <- df %>%
  filter(species == "Camundongo") %>% 
  group_by(treatment_via) %>%
  my_skim(treatment_duration) %>%
  tibble::as_tibble()

f13b <- df %>%
  filter(species == "Camundongo") %>%
  ggplot(aes(y = treatment_duration, x = fct_infreq(treatment_via))) +
  geom_point(
    aes(color = treatment_freq),
    position = position_jitterdodge(),
    size = .8,
    shape = 19,
    alpha = .5
  ) +
  labs(y = "Duração do tratamento (dias)", x = "Via de administração em camundongos", title = "b") +
  scale_color_manual(
    name = "Administrações/dia:",
    values = c(
      "1" = "#fec200",
      "2" = "#009c7e",
      "3" = "#a94f93",
      "Sem info" = "grey30"
    )
  ) +
  scale_y_continuous(expand = c(.01, 0), limits = c(0, 120)) +
  geom_pointrange(data = stat_t_d_cam, aes(x = treatment_via, y = numeric.p50,  ymin = numeric.p25, ymax = numeric.p75), colour = "black", size = .2, fatten = .1) +
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
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.position = "top",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10, 0,-10,-10),
    legend.spacing.x = unit(0.1,'mm'),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.minor.y = element_line(color = "grey90", size = .1)
  )
f13b

F13 <- (f13a + plot_spacer() + plot_layout(widths = c(6,3))) / (f13b + plot_spacer() + plot_layout(widths = c(6,3))) +  plot_layout(heights = c(3,8))

F13
save_plot(filename = "Figura13.png",
          plot = F13,
          dpi = 300)


# RATOS

# Vias de adm e frequencia de adm

f14a <- df %>% 
  filter(species == "Rato") %>% 
  ggplot(aes(x = fct_infreq(treatment_via), fill = treatment_freq)) +
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
  labs(y = "Nº de estudos", x = "Via de administração em ratos", title = "a") +
  scale_fill_manual(values = c("1" = "#fec200", "2" = "#009c7e", "3" = "#a94f93", "Sem info" = "grey80")) +
  theme(
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.x = element_text(size = 7, hjust = 1, vjust = 40),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f14a

# vias de adm, frequencia de adm e duracao do tratamento

stat_t_d_rat <- df %>%
  filter(species == "Rato") %>% 
  group_by(treatment_via) %>%
  my_skim(treatment_duration) %>%
  tibble::as_tibble()

f14b <- df %>%
  filter(species == "Rato") %>%
  ggplot(aes(y = treatment_duration, x = fct_infreq(treatment_via))) +
  geom_point(
    aes(color = treatment_freq),
    position = position_jitterdodge(),
    size = .8,
    shape = 19,
    alpha = .5
  ) +
  labs(y = "Duração do tratamento (dias)", x = "Via de administração em ratos", title = "b") +
  scale_color_manual(
    name = "Administrações/dia:",
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
  geom_pointrange(data = stat_t_d_rat, aes(x = treatment_via, y = numeric.p50,  ymin = numeric.p25, ymax = numeric.p75), colour = "black", size = .2, fatten = .1) +
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
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.position = "top",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10, 0,-10,-10),
    legend.spacing.x = unit(0.1,'mm'),
    panel.grid.major.y = element_line(color = "grey90", size = .1),
    panel.grid.minor.y = element_line(color = "grey90", size = .1)
  )

f14b

F14 <- f14a  / f14b  +  plot_layout(heights = c(3,8))

F14
save_plot(filename = "Figura14.png",
          plot = F14,
          dpi = 300)



# DESFECHO

##Figura15 e Figura 16 : protocolo x metodos de analise ----

# fst protocol

f15a <- df %>%
  filter(species == "Camundongo") %>%
  group_by(study_reference) %>%
  distinct(fst_protocol) %>%
  group_by(fst_protocol) %>%
  ggplot(aes(
    x = fct_lump_n(
      fct_infreq(fst_protocol),
      n = 9,
      other_level = "Outros"
    ),
    fill = fct_lump_n(fct_infreq(fst_protocol),
                      n = 9)
  )) +
  geom_bar() +
  scale_fill_manual(values = c(
    "#FE7700",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400"
  )) +
  labs(y = "Nº de publicações", x = "Camundongo", title = "a", subtitle = "Protocolo do nado forçado") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 67)) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 13)
  ) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
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
    axis.title.x = element_text(margin = margin(t = 5), hjust = .98),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 8, hjust = .5),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f15a

f15b <- df %>%
  filter(species == "Rato") %>%
  group_by(study_reference) %>%
  distinct(fst_protocol) %>%
  group_by(fst_protocol) %>%
  ggplot(aes(x = fct_lump_n(
    fct_infreq(fst_protocol),
    n = 3,
    other_level = "Outros"
  ), fill = fct_lump_n(fct_infreq(fst_protocol),
                       n = 3))) +
  geom_bar() +
  scale_fill_manual(values = c(
    "#a6243a",
    "#ec2b2b",
    "#ec2b2b",
    "#ec2b2b",
    "#ec2b2b",
    "#ec2b2b",
    "#ec2b2b"
  )) +
  labs(y = "Nº de publicações", x = "Rato", title = "b") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 67)) +
  scale_x_discrete(labels = function(x)
    str_wrap(x, width = 13)) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
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
    axis.title.x = element_text(margin = margin(t = 5), hjust = .98),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(10, 0, 10, 10)
  )

f15b


Figura15 <- f15a / f15b

save_plot(filename = "Figura15.png",
          plot = Figura15,
          dpi = 300)



#metodo de analise

f16a <- df %>%
  filter(species == "Camundongo") %>%
  group_by(study_reference) %>%
  distinct(measurement_method) %>%
  group_by(measurement_method) %>%
  ggplot(aes(
    x = fct_lump_n(
      fct_infreq(measurement_method),
      n = 6,
      other_level = "Outros"
    ),
    fill = fct_lump_n(fct_infreq(measurement_method), n = 6)
  )) +
  geom_bar() +
  scale_fill_manual(values = c(
    "grey80",
    "#FE7700",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400",
    "#ff9400"
  )) +
  labs(y = "Nº de publicações", x = "Camundongo", title = "a", subtitle = "Método de análise do nado forçado") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
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
    axis.title.x = element_text(margin = margin(t = 5), hjust = .98),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 8, hjust = .5),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

f16a

f16b <- df %>%
  filter(species == "Rato") %>%
  group_by(study_reference) %>%
  distinct(measurement_method) %>%
  group_by(measurement_method) %>%
  ggplot(aes(
    x = fct_lump_n(
      fct_infreq(measurement_method),
      n = 9,
      other_level = "Outros"
    ),
    fill = fct_lump_n(fct_infreq(measurement_method), n = 9)
  )) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "grey80",
      "#a6243a",
      "#ec2b2b",
      "#ec2b2b",
      "#ec2b2b",
      "#ec2b2b",
      "#ec2b2b",
      "#ec2b2b",
      "#ec2b2b",
      "#ec2b2b"
    )
  ) +
  labs(y = "Nº de publicações", x = "Rato", title = "b") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 15)
  ) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
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
    axis.title.x = element_text(margin = margin(t = 5), hjust = .98),
    plot.title = element_text(size = 10),
    plot.title.position = "plot",
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(10, 0, 10, 10)
  )

f16b

Figura16 <- f16a / f16b
Figura16
save_plot(filename = "Figura16.png",
          plot = Figura16,
          dpi = 300)



#Figura17 e Figura 18: tamanho x diametro cuba / altura agua x temperatura agua -----

# DIMENSOES CUBA

# rotulo

stat_nado <- df %>%
  filter(!is.na(is.numeric(cylinder_height)), !is.na(is.numeric(cylinder_diameter)), !is.na(is.numeric(water_depth)), !is.na(is.numeric(water_temperature))) %>% 
  group_by(study_reference, cylinder_height, cylinder_diameter, water_depth, water_temperature, species) %>% 
  distinct(study_reference, cylinder_height, cylinder_diameter, water_depth, water_temperature, species) %>% 
  group_by(species) %>%
  my_skim(cylinder_height, cylinder_diameter, water_depth, water_temperature) %>%
  mutate(numeric.p.50 = round(numeric.p50, 1),
       numeric.p25 = round(numeric.p25, 1),
       numeric.p75 = round(numeric.p75, 1))

# df do rotulo

label_stat_nado <-
  data.frame(label = paste(stat_nado$numeric.median, " ", "(",  stat_nado$numeric.p25, "-", stat_nado$numeric.p75, ")", sep = ""), species = stat_nado$species, variable = stat_nado$skim_variable, y_height = stat_nado$numeric.p50, x_diameter = stat_nado$numeric.p50)


f17a2 <-  df %>%
  filter(!is.na(is.numeric(cylinder_height)), !is.na(is.numeric(cylinder_diameter))) %>% 
  group_by(study_reference, cylinder_height, cylinder_diameter, species) %>% 
  distinct(study_reference, cylinder_height, cylinder_diameter, species) %>%
  ggplot(aes(x = cylinder_diameter, y = cylinder_height, color = species), na.rm = T) +
  geom_jitter(alpha = .5, size = 1) +
  geom_text(data = filter(label_stat_nado, species == "Camundongo", variable == "cylinder_diameter"),
    aes(label = str_wrap(label, width = 7), x = as.numeric(x_diameter), y = as.numeric(y_height)),
    y = 3.5, # esse valor vai predominar ao dentro do aes
    color = "#FE7700",
    size = 2
  ) +
  geom_text(data = filter(label_stat_nado, species == "Camundongo", variable == "cylinder_height"),
            aes(label = str_wrap(label, width = 7), x = as.numeric(x_diameter), y = as.numeric(y_height)),
            x = 5,
            color = "#FE7700",
            size = 2
  ) +
  geom_text(data = filter(label_stat_nado, species == "Rato", variable == "cylinder_diameter"),
            aes(label = str_wrap(label, width = 7), x = as.numeric(x_diameter), y = as.numeric(y_height)),
            y = 3.5,
            color = "#ec2b2b",
            size = 2
  ) +
  geom_text(data = filter(label_stat_nado, species == "Rato", variable == "cylinder_height"),
            aes(label = str_wrap(label, width = 7), x = as.numeric(x_diameter), y = as.numeric(y_height)),
            x = 5,
            color = "#ec2b2b",
            size = 2
  ) +
  scale_color_manual(name = "Espécie:", values = c("#ff9400", "#ec2b2b")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 85)) +
  labs(x = "Diâmetro do cilindro (cm)", y = "Altura do cilindro (cm)") +
  theme_bw(base_family = "Gadugi") +
  coord_fixed() +
  theme(
    axis.line = element_line(size = .2),
    axis.text = element_text(
      size = 7,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 8, hjust = .5),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.title.x = element_text(margin = margin(t = 5)),
    plot.title = element_text(size = 10),
    legend.position = "panel",
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(0, 0, 0, 0)
  )
f17a2

f17a1 <-  df %>%
  filter(is.na(cylinder_height) & !is.na(as.numeric(cylinder_diameter))) %>% 
  group_by(study_reference, cylinder_height, cylinder_diameter, species) %>% 
  distinct(study_reference, cylinder_height, cylinder_diameter, species) %>%
  ggplot(aes(x = cylinder_diameter, y = "Sem info" ,color = species), na.rm = T) +
  geom_jitter(alpha = .5, size = 1) +
  scale_color_manual(name = "Espécie:", values = c("#ff9400", "#ec2b2b")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 85)) +
  scale_y_discrete(expand = c(0, 5)) +
  coord_fixed() +
  theme_bw(base_family = "Gadugi") +
  theme(
    axis.line = element_line(size = .2),
    axis.line.x = element_blank(),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "panel",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10, 0,-10,-10),
    legend.spacing.x = unit(0.1,'mm'),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(0, -10, -10, 25),
    axis.text.y = element_text(angle = 90, hjust = .5),
    axis.ticks = element_blank()
  )
f17a1


f17a <- f17a1 / f17a2 
f17a


save_plot(filename = "Figura17a.png",
          plot = f17a,
          dpi = 300)
   
# PROFUNDIDADE DA AGUA
         
# Gerar estatistica para plot e rotulo CAMUNDONGO
        
label_wd_c <- df %>% 
  filter(species == "Camundongo") %>% 
  group_by(study_reference) %>%
  distinct(water_depth, year) %>%
  group_by(year) %>%
  my_skim(water_depth)
  


f17b <- label_wd_c %>% 
  ggplot(aes(x = year, y = numeric.p50)) +
  geom_col(fill = "#ff9400") +
  labs(subtitle = "Profundidade da água no nado forçado (cm)", x = "Camundongo") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0),  limits = c(0,45)) +
  geom_vline(xintercept = as.numeric(as.Date("1994-01-01")),
    col = "grey30",
    size = .3,
    linetype = "dashed"
  ) +
  geom_text(aes(label = "ABEL et al., 1994"), 
            y = 37,
            x = as.numeric(as.Date("1994-01-01")),
            color = "black",
            size = 2,
            hjust = 1.1,
            family = "Gadugi") +
  geom_curve(aes(x = as.Date("1992-01-01"), y = 40, xend = as.Date("1993-08-01"), yend = 43), 
             colour = "black", 
             size = .5, 
             curvature = -0.2,
             arrow = arrow(length = unit(0.03, "npc")))  +
  geom_text(data = label_wd_c, aes(label = complete, x = year, y = 2),
            color = "mintcream",
            size = 2.5,
            family = "Gadugi"
  ) +
  geom_pointrange(data = label_wd_c, aes(x = year, y = numeric.p50,  ymin = numeric.p25, ymax = numeric.p75), colour = "black", size = .2, fatten = .1) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_blank(),
    plot.title.position = "panel",
    plot.subtitle = element_text(size = 8, hjust = .5, margin = margin(b = 10)),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(0, 0, 15, 0)
  )
f17b


# Gerar estatistica para plot e rotulo RATO

label_wd_r <- df %>% 
  filter(species == "Rato") %>% 
  group_by(study_reference) %>%
  distinct(water_depth, year) %>%
  group_by(year) %>%
  my_skim(water_depth)

f17c <- label_wd_r %>% 
  ggplot(aes(x = year, y = numeric.p50)) +
  geom_col(fill = "#ec2b2b") +
  labs(x = "Rato") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,45)) +
  geom_vline(xintercept = as.numeric(as.Date("1994-01-01")),
             col = "grey30",
             size = .3,
             linetype = "dashed"
  ) +
  geom_text(data = label_wd_r, aes(label = complete, x = year, y = 2),
            color = "mintcream",
            size = 2.5,
            family = "Gadugi"
  ) +
  geom_pointrange(data = label_wd_r, aes(x = year, y = numeric.p50,  ymin = numeric.p25, ymax = numeric.p75), colour = "black", size = .2, fatten = .1) +
  theme(
    axis.line = element_line(size = .3),
    axis.text = element_text(
      size = 6,
      angle = 0,
      color = "grey20"
    ),
    axis.title = element_text(size = 7, hjust = 1),
    axis.title.y = element_blank(),
    plot.title.position = "panel",
    axis.title.x = element_text(margin = margin(t = 5)),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(0, 0, 0, 0)
  )


f17bc <- f17b / f17c 

save_plot(filename = "Figura17bc.png",
          plot = f17bc,
          dpi = 300)

# TEMPERATURA AGUA

# filtro com rotulo estatistica

filtro_temp_agua_c <- df %>%
  filter(species == "Camundongo") %>% 
  group_by(study_reference) %>%
  slice(1) %>%
  filter(!is.na(is.numeric(water_temperature))) %>%
  select(water_temperature, species) 

filtro_temp_agua_c <- filtro_temp_agua_c %>% 
  ungroup() %>% 
  my_skim(water_temperature) %>%
  mutate(numeric.median = round(numeric.median), 1,
         numeric.p25 = round(numeric.p25), 1,
         numeric.p75 = round(numeric.p75), 1)


f17d <- df %>%  
  filter(species == "Camundongo") %>%
  group_by(study_reference) %>%
  distinct(water_temperature) %>%
  ggplot(aes(x = water_temperature)) +
  geom_histogram(fill = "#ff9400") +
  scale_y_continuous(expand = c(0, 0), n.breaks = 4) +
  scale_x_continuous(n.breaks = 4, limits = c(19,35)) +
geom_text(
  aes(
    label = paste(
      filtro_temp_agua_c$numeric.median,
      " ",
      "(",
      filtro_temp_agua_c$numeric.p25,
      "-",
      filtro_temp_agua_c$numeric.p75,
      ")",
      sep = ""
    )
  ),
  x = Inf ,
  y = 48,
  hjust = 1.1,
  color = "black",
  size = 2
) 
  geom_vline(
    data = filtro_temp_agua_c,
    aes(xintercept = numeric.p25),
    col = "black",
    size = .5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = filtro_temp_agua_c,
    aes(xintercept = numeric.p75),
    col = "black",
    size = .5,
    linetype = "dashed"
  ) ++ geom_vline(
  data = filtro_temp_agua_c,
  aes(xintercept = numeric.p50),
  col = "gold2",
  size = .5
) +
  labs(
    y = "Nº de publicações",
    subtitle = "Temperatura da água no nado forçado (°C)",
    x = "Camundongo",
    title = "d"
  ) +
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
    plot.subtitle = element_text(size = 8, hjust = .5, margin = margin(b = 10)),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 10, 10)
  )

f17d


# filtro com rotulo estatistica

filtro_temp_agua_r <- df %>%
  filter(species == "Rato") %>% 
  group_by(study_reference) %>%
  slice(1) %>%
  filter(!is.na(is.numeric(water_temperature))) %>%
  select(water_temperature, species) 

filtro_temp_agua_r <- filtro_temp_agua_r %>% 
  ungroup() %>% 
  my_skim(water_temperature) %>%
  mutate(numeric.median = round(numeric.median), 1,
         numeric.p25 = round(numeric.p25), 1,
         numeric.p75 = round(numeric.p75), 1)



f17e <- df %>%  
  filter(species == "Rato") %>%
  group_by(study_reference) %>%
  distinct(water_temperature) %>%
  ggplot(aes(x = water_temperature)) +
  geom_histogram(fill = "#ec2b2b") +
  scale_y_continuous(expand = c(0, 0), n.breaks = 4) +
  scale_x_continuous(n.breaks = 4, limits = c(19,35)) +
  geom_text(
    aes(
      label = paste(filtro_temp_agua_r$numeric.median, " ", "(",  filtro_temp_agua_r$numeric.p25, "-", filtro_temp_agua_r$numeric.p75, ")", sep = "")
    ),
    x = Inf ,
    y = 49,
    hjust = 1.1,
    color = "grey20",
    size = 2
  ) + geom_vline(
    data = filtro_temp_agua_c,
    aes(xintercept = numeric.p50),
    col = "black",
    size = .5
  ) +
  geom_vline(
    data = filtro_temp_agua_c,
    aes(xintercept = numeric.p25),
    col = "black",
    size = .5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = filtro_temp_agua_c,
    aes(xintercept = numeric.p75),
    col = "black",
    size = .5,
    linetype = "dashed"
  ) +
  labs(
    y = "Nº de publicações",
    x = "Rato"
  ) +
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
    plot.subtitle = element_text(size = 8, hjust = .5, margin = margin(b = 10)),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(colour = 'black', size = 8),
    panel.grid.major = element_line(color = "grey90", size = .1),
    plot.margin = margin(10, 0, 10, 10)
  )

f17e

f17de <- f17d / f17e 


f17bcde <- f17bc | f17de + plot_layout(widths = c(7,3))
f17bcde
