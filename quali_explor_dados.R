# Analise exploratoria qualitativa

# Carregar pacotes

library(tidyverse)
library(skimr)
library(summarytools)
library(writexl)
library(patchwork)
library(gghighlight)
library(tibble)
library(MetBrewer)
library(ggtext)
library(cowplot)

library(ggmap)
library(ggcorrplot)
library(ggrepel)

library(ggradar)


# Carregar dataframe dos dados limpos e organizados

df <- data_geral_clean <- readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")


# Conferir variáveis e valores ------- 


# Verificar quantos dados faltantes por coluna

colSums(is.na(df))


# Estatistica descritiva basica e frequencias


summarize(df)

skim(df)

dfSummary(df,
          plain.ascii  = FALSE,
          style        = 'grid',
          graph.magnif = 0.85,
          varnumbers = FALSE,
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp")

# Codigo modificavel para verificar os dados faltantes

data_miss <- data_geral_clean %>% 
  filter(is.na(VARIAVEL))



# Tabelas estatísticas -------


# Descritivo por nivel da variavel de escolha


teste <- df %>% 
  group_by(species) %>% 
  skim()


teste

# Especificando a variavel a ser mostrada a estatistica

df %>% 
  group_by(sex) %>% 
  skim(year, atd_class)



sex_year <- df %>% 
  filter(species == "rat") %>% 
  group_by(sex, year) %>% 
  count(sex, year)
  


# Tabela com variaveis numéricas

# Geral

tab_numeric <- df %>% 
  skim %>%
  yank("numeric")

print(tab_numeric)

tab_numeric <- tab_numeric %>% 
  mutate(n = 562 - (n_missing))

# Ratos


tab_numeric_rat <- df %>% 
  filter(species == "rat") %>% 
  skim %>%
  yank("numeric")

tab_numeric_rat <- tab_numeric %>% 
  mutate(n = 234 - (n_missing))

# Camundongos


tab_numeric_mice <- df %>% 
  filter(species == "mice") %>% 
  skim %>%
  yank("numeric")

tab_numeric_mice <- tab_numeric %>% 
  mutate(n = 328 - (n_missing))


# Salvando tabelas

write_xlsx(tab_numeric,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\teste.xlsx")

write.table(tab_numeric, file = "teste.txt", sep = ",", quote = FALSE, row.names = F) # Depois colar conteudo .txt no word e transformar em tabela


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

#MetBrewer preferidos: Signac, Renoir, Thomas, VanGogh1


# Populacao -----

# Figura1: sex x especies x yerar ----- 



df$species <- factor(df$species, levels = c("mice", "rat"), 
                 labels = c("Camundongo", "Rato")) # Ordenar niveis do fator espécie

df$sex <- factor(df$sex, levels = c("M", "F", "M and F", "NA"), 
                 labels = c("Macho", "Fêmea", "Ambos", "Sem info")) # Ordenar niveis do fator sexo


f1a <- df %>% #Freq especie
  group_by(species) %>% 
  summarise(counts = sum(N)) %>% 
  ggplot(aes( x = species, y = counts, fill = species, label = counts)) +
  geom_bar(color = "black", stat="identity", size = .2) +
  geom_text(size = 2.5, family ="serif", position = position_dodge(width=0.9), vjust = -0.25) +
  ylim(0,5397)+
  labs(y = "Nº de animais", x = "Espécie", title = "a") +
  scale_fill_manual(values=met.brewer("Signac", 2))+
  theme(axis.text=element_text(size=7, angle = 0, color = "grey30"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=7, hjust = 1),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(10,10,10,10))

f1b <- df  %>% # especie no tempo
  ggplot(aes(x = year, fill = species)) +
  geom_bar(color = "black", size = .2)+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  labs(y = "Nº de estudos", x = "Ano", title = "b", fill = "Espécie") + 
  scale_fill_manual(values=met.brewer("Signac", 2)) +
  theme(axis.text=element_text(size=7, angle = 0, color = "grey30"),
        axis.title=element_text(size=7, hjust = 1),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(20,10,10,20))


f1c <- df %>% # Freq sexo
  group_by(sex) %>% 
  summarise(counts = sum(N)) %>% 
  ggplot(aes(x = sex, y = counts, fill = sex, label = counts)) +
  geom_bar(color = "black", stat="identity", size = .2) +
  geom_text(size = 2.5, family ="serif", position = position_dodge(width=0.9), vjust = -0.25) +
  scale_fill_manual(values=met.brewer("Signac", 4))+
  ylim(0, 6906)+ # 300 a mais para caber a anotacao da maior barra
  labs(y = "Nº de animais", x = "Sexo", title = "c") +
  theme(axis.text=element_text(size=7, angle = 0, color = "grey30"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=7, hjust = 1),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(10,10,10,10))

f1d <- df  %>% # sexo no tempo
  ggplot(aes(x = year, fill = sex, order = sex)) +
  geom_bar(color = "black", size = .2)+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  labs(y = "Nº de estudos", x = "Ano", title = "d") + 
  scale_fill_manual(values=met.brewer("Signac", 4)) + # Preferi esse
  theme(axis.text=element_text(size=7, angle = 0, color = "grey30"),
        axis.title=element_text(size=8, hjust = 1),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=11, face = "italic"),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,10,10,20))



# Combinar e salvar


Figura1 <- f1a + f1b + f1c + f1d + plot_layout(heights = c(6, 6), widths = c(3, 6))
Figura1


save_plot(filename = "Figura1.png", plot = Figura1, dpi = 300)

# Figura 2: model x age x weight -----

# Modelo


f2a <- df %>%
  group_by(model_phenotype,
           species) %>% 
  filter(model_phenotype != "NA") %>%  
  summarise(counts = sum(N)) %>% 
  ggplot(aes(x = model_phenotype, y = counts, fill = model_phenotype)) +
  geom_bar(color = "black", stat="identity") + # Mudar essa cor rs
  labs(y = "Nº de animais", x = "Modelo animal") + 
  scale_fill_manual(values=met.brewer("Signac", 9))+
  coord_flip() +
  facet_wrap(~species, strip.position = "top")+
  gghighlight(counts > 40, calculate_per_facet = TRUE, label_key = model_phenotype) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=10, angle = 0, color = "grey30"),
        axis.title=element_text(size=12),
        axis.title.y=element_blank(), 
        axis.title.x=element_text(margin = margin(t=5)),
        panel.grid.major = element_line(color = "grey90", size =.5),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 12),
        plot.margin = margin(10,10,10,0))


f2a
f2b <- df %>%
  group_by(model_phenotype,
           species) %>% 
  filter(model_phenotype != "NA",) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(x = forcats::fct_infreq(model_phenotype), y = counts, fill = model_phenotype)) +
  geom_bar(color = "black", stat="identity") + # Mudar essa cor rs
  labs(y = "Nº de estudos", x = "Modelo") +
  scale_fill_manual(values=met.brewer("Signac", 10))+
  coord_flip() +
  facet_wrap(~species, strip.position = "top")+
  gghighlight(counts > 3, calculate_per_facet = TRUE, label_key = model_phenotype) +
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=10, angle = 0, color = "grey30"),
        axis.title=element_text(size=12),
        axis.title.x=element_text(margin = margin(t=5)),
        axis.title.y=element_blank(),
        panel.grid.major = element_line(color = "grey90", size =.5),
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 12),
        plot.margin = margin(10,10,10,10))



f2 <- f2b + f2a
f2
ggsave(f2, filename = "Figura2.png", dpi = 300, path = "Fig")

# Age


f3a <- df %>%
  ggplot(aes(x = age, fill = species)) +
  geom_histogram(color = "black")+
  facet_grid(sex~species, scales = "free_x") +
  labs(y = "Nº de estudos", x = "Idade (dias)") +
  scale_x_continuous(n.breaks = 10)+
  scale_fill_manual(values=met.brewer("Signac", 2))+
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=12, angle = 0, color = "grey30"),
        axis.title=element_text(size=12, hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 12),
        panel.grid.major = element_line(color = "grey90", size =.5),
        plot.margin = margin(20,20,20,20))
f3a


ggsave(f3a, filename = "Figura3.png", dpi = 300, path = "Fig")


# weight

f4a <- df %>%
  ggplot(aes(x = weight, fill = species)) +
  geom_histogram(color = "black")+
  facet_grid(sex~species, scales = "free_x") +
  labs(y = "Nº de estudos", x = "Peso (g)") +
  scale_x_continuous(n.breaks = 7)+
  scale_fill_manual(values=met.brewer("Signac", 2))+
  theme_bw(base_family = "serif")+
  theme(axis.text=element_text(size=12, angle = 0, color = "grey30"),
        axis.title=element_text(size=12, face = "bold", hjust = 0),
        axis.title.y=element_text(margin = margin(r=5)),
        axis.title.x=element_text(margin = margin(t=5)),
        legend.position = "none",
        strip.background = element_rect(fill="white", color = "black"),
        strip.text = element_text(colour = 'black', size = 12, face = "bold"),
        panel.grid.major = element_line(color = "grey90", size =.5),
        plot.margin = margin(20,20,20,20))
f4a


# Strain

f5a <- df %>%
  group_by(strain) %>% 
  filter(species == "Rato") %>% 
  summarise(counts = n()) %>% 
  mutate(strain = fct_reorder(strain, counts, .desc = FALSE),
         counts = as.numeric(counts)) %>%
  ggplot(aes(x = forcats::fct_infreq(strain), y = counts)) +
  geom_bar(color = "black", stat="identity", fill = "#82c236") + # Mudar essa cor rs
  labs(y = "Nº de estudos", x = "Linhagem") +
  scale_y_continuous()+  
  coord_flip() +
  theme(axis.text=element_text(size=10, angle = 0, color = "grey30"),
        axis.title=element_text(size=10, face = "bold"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=14, face = "bold", hjust = 1),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,10,10,10))
f5a

f5b <- df %>%
  group_by(strain) %>% 
  filter(species == "Rato") %>% 
  summarise(counts = sum(N)) %>% 
  mutate(strain = fct_reorder(strain, counts, .desc = FALSE),
         counts = as.numeric(counts)) %>%
  ggplot(aes(x = forcats::fct_infreq(strain), y = counts)) +
  geom_bar(color = "black", stat="identity", fill = "#a94f93") + # Mudar essa cor rs
  labs(y = "Nº de animais", x = "Linhagem") + 
  coord_flip() +
  theme(axis.text=element_text(size=10, angle = 0, color = "grey30"),
        axis.title=element_text(size=10, face = "bold"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=14, face = "bold", hjust = 1),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,10,10,10))

f5b

f5c <- df %>%
  group_by(strain) %>% 
  filter(species == "Camundongo") %>% 
  summarise(counts = n()) %>% 
  mutate(strain = fct_reorder(strain, counts, .desc = FALSE),
         counts = as.numeric(counts)) %>%
  ggplot(aes(x = forcats::fct_infreq(strain), y = counts)) +
  geom_bar(color = "black", stat="identity", fill = "#b376a2") + # Mudar essa cor rs
  labs(y = "Nº de estudos", x = "Linhagem") +
  scale_y_continuous()+  
  coord_flip() +
  theme(axis.text=element_text(size=10, angle = 0, color = "grey30"),
        axis.title=element_text(size=10, face = "bold"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=14, face = "bold", hjust = 1),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,10,10,10))

f5d <- df %>%
  group_by(strain) %>% 
  filter(species == "Camundongo") %>% 
  summarise(counts = sum(N)) %>% 
  mutate(strain = fct_reorder(strain, counts, .desc = FALSE),
         counts = as.numeric(counts)) %>%
  ggplot(aes(x = forcats::fct_infreq(strain), y = counts)) +
  geom_bar(color = "black", stat="identity", fill = "#a94f93") + # Mudar essa cor rs
  labs(y = "Nº de animais", x = "Linhagem") + 
  coord_flip() +
  theme(axis.text=element_text(size=10, angle = 0, color = "grey30"),
        axis.title=element_text(size=10, face = "bold"),
        axis.title.y=element_blank(),
        axis.title.x=element_text(margin = margin(t=5)),
        plot.title=element_text(size=14, face = "bold", hjust = 1),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(10,10,10,10))


f5a + f5b + f5c + f5d + plot_annotation(
  title = 'Linhagens',
  subtitle = 'Ratos (superior) e Camundongos (Inferior)',
  theme = theme(plot.title = element_text(size = 16),
                plot.title.position = "plot"))


# Figuras variaveis character




