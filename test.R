
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
library(styler)

library(ggcorrplot)



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
    "1 saline injection for 4 days",
    "ACTH (100microg)",
    "amphetamine withdrawal",
    "antidepressant withdrawal",
    "Bacillus Calmette–Guérin (BCG)",
    "CMS",
    "CUMs",
    "CUS",
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
    "wheel running + restraint-stress"
  ),
  labels = c(
    "Injeção salina por 4 dias",
    "ACTH (100µg)",
    "Retirada de anfetamina",
    "Retirada de antidepressivo",
    "Bacillus Calmette–Guérin (BCG)",
    "CMS - Estresse leve crônico",
    "CUMs - Estresse leve imprevisível crônico",
    "CUS - Estresse imprevisível crônico",
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
    "Roda de corrida + estresse por contenção"
  )
)


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
  ) # Ordenar niveis do fator luz do bioterio




# gganimate

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
    longr = replace(longr, region == "EUA", -110)
  )


# Plotar graficos

# Todos anos até 2017

F0 <-
  ggplot(dados_public, aes(x = long, y = lat, map_id = region, frame = year)) + # dados
  geom_map(
    map = dados_public,
    # mapa mundo
    aes(map_id = region),
    color = "white",
    size = 0.1
  ) +
  geom_map(
    map = dados_public,
    # Coloração dos países por frequência
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
  expand_limits(x = world$long, y = 85) + # Estabeleço o tamanho
  geom_text_repel(
    data = subset(rotulo, N > 10),
    # Adiciono o rótulo dos países mais frequentes
    aes(
      x = longr,
      y = latr,
      label = region
    ),
    color = "black",
    size = 2,
    segment.size = 0.3,
    box.padding = 0.8,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 10
  )

animation::ani.options(interval=1)
x <- gganimate(F0, filename = "fig/teste.png",
               ani.width = 750,
               ani.height = 750)


# scatter plot

f3 <- df %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(color = "black", size = 0.2) +
  facet_grid(fct_infreq(sex) ~ species, scales = "free_x") +
  geom_text(
    data = labels_age,
    aes(label = label),
    x = Inf,
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
    strip.text = element_text(colour = "black", size = 8),
    panel.grid.major = element_line(color = "grey90", size = .3),
    plot.margin = margin(20, 0, 20, 0)
  )
