# Referência: ~ref da dissertação~

# ETAPA 5: Analise da qualidade ROB SYRCLE e CAMARADES adaptado

# Carregar pacotes ----


library(tidyverse)    # manipulacao de dados e plotar
library(patchwork)    # juntar plots
library(extrafont)    # adicionar fontes
library(cowplot)      # salvar plot


# Carregar dataframe dos dados limpos e organizados

df <-
  data_geral_clean <-
  readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")

# Isolar variáveis do ROB SYRCLE


df_rob <- df %>% 
  mutate(estudo = str_c(first_author, ", ", lubridate::year(year))) %>% 
  select(starts_with("rob"), estudo)


df_rob <- df_rob %>% 
  pivot_longer(!estudo,
               names_to = "pergunta",
               values_to = "atribuicao",
               )

df_rob$pergunta <- fct_relevel(df_rob$pergunta, "rob1", "rob2", "rob3", "rob4", "rob5", "rob6", "rob7", "rob8", "rob9", "rob10")

# Renomear os níveis 

df_rob$atribuicao <-
  factor(
    df_rob$atribuicao,
    levels = c("Yes", "No", "Unclear"),
    labels = c("Baixo risco", "Alto risco", "Incerto") # mudar
  )

df_rob$pergunta <-
  factor(
    df_rob$pergunta,
    levels = c("rob1", "rob2", "rob3", "rob4", "rob5", "rob6", "rob7", "rob8", "rob9", "rob10"),
    labels = c("A alocação de tratamento foi adequadamente gerada e aplicada?",
               "Os grupos (controle e tratado) eram similares no início do experimento?", 
               "A alocação foi adequadamente escondida?", 
               "Os animais foram acondicionados aleatoriamente?", 
               "Os investigadores eram cegos quanto ao tratamento durante os experimentos?",
               "Os animais foram selecionados aleatoriamente para acessar o desfecho?", 
               "A avaliação do resultado foi cega?",
               "Dados incompletos foram adequadamente endereçados?", 
               "Os relatos do estudo são livres de seleção de desfecho relatado?", 
               "O estudo está aparentemente livre de algum outro problema que poderia resultar em alto risco de viés?")
  )


# Estabelecer tema base para todos os próximos gráficos

theme_set(theme_minimal(base_family = "Gadugi"))



# Visualização ROB SYRCLE

v_factor_levels <- c("Alto risco", "Incerto", "Baixo risco")

robplot <- df_rob %>% 
  group_by(estudo) %>% 
  distinct(estudo, pergunta, atribuicao) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = v_factor_levels), y = ..count..)) +
  geom_bar(position = "fill") + 
  scale_fill_manual("Julgamento do risco de viés", values = c("Baixo risco" = "#82c236", "Incerto" = "#fec200", "Alto risco" = "#ec2b2b")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 55)
  ) +
  coord_flip()  + 
  theme(axis.ticks.x = element_line(size = .3),
    axis.line = element_line(size = .3),
        axis.text = element_text(
          size = 7,
          color = "grey20"
        ),
    axis.line.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.text = element_text(size = 6, color = "grey20"),
        legend.title = element_text(size = 7),
        plot.margin = margin(5, 0, 0, 5),
        legend.key.size = unit(.8, "line"),
        panel.grid.major.y = element_line(color = "grey90", size = .1),
        panel.grid.major.x = element_blank()
  )

robplot

save_plot(filename = "Figura18.png",
          plot = robplot,
          dpi = 300)



