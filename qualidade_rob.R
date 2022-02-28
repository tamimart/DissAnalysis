# Referência: ~ref da dissertação~

# ETAPA 5: Analise da qualidade ROB SYRCLE e CAMARADES adaptado

# Carregar pacotes ----


library(tidyverse)    # manipulacao de dados e plotar
library(patchwork)    # juntar plots
library(extrafont)    # adicionar fontes
library(cowplot)      # salvar plot
library(robvis)
library(readxl)

# Carregar dataframe dos dados limpos e organizados

df <-
  data_geral_clean <-
  readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")

# Isolar variáveis do ROB SYRCLE

df_rob <- df %>% 
  mutate(Study = str_c(first_author, ", ", lubridate::year(year))) %>% 
  select(starts_with("rob"), Study) 


df_rob <- df_rob %>% 
  distinct() # deixar uma linha por publicação

df_rob <- df_rob %>% 
  rename("A alocação de tratamento foi adequadamente gerada e aplicada?" = rob1,
         "Os grupos (controle e tratado) eram similares no início do experimento?" = rob2,
         "A alocação foi adequadamente escondida?" = rob3,
         "Os animais foram acondicionados aleatoriamente?" = rob4, 
         "Os investigadores eram cegos quanto ao tratamento durante os experimentos?" = rob5, 
         "Os animais foram selecionados aleatoriamente para acessar o desfecho?" = rob6,
         "A avaliação do resultado foi cega?" = rob7, 
         "Dados incompletos foram adequadamente endereçados?" = rob8, 
         "Os relatos do estudo são livres de seleção de desfecho relatado?" = rob9,
         "O estudo está aparentemente livre de algum outro problema que poderia resultar em alto risco de viés?" = rob10) %>% 
         mutate(Weight = as.numeric(1),
                overall = "Unclear") %>% 
  relocate(Study, everything())




df_rob_long <- df_rob %>% # colocar em modo longo
  pivot_longer(!c(Study, Weight),
               names_to = "pergunta",
               values_to = "atribuicao",
               ) 


# Renomear os níveis 

df_rob_long$atribuicao <-
  factor(
    df_rob_long$atribuicao,
    levels = c("Yes", "No", "Unclear"),
    labels = c("Baixo risco", "Alto risco", "Incerto") # colocar significado das atribuições
  )

df_rob_long$pergunta <-
  fct_relevel(
    df_rob_long$pergunta, "A alocação de tratamento foi adequadamente gerada e aplicada?",
               "Os grupos (controle e tratado) eram similares no início do experimento?", 
               "A alocação foi adequadamente escondida?", 
               "Os animais foram acondicionados aleatoriamente?", 
               "Os investigadores eram cegos quanto ao tratamento durante os experimentos?",
               "Os animais foram selecionados aleatoriamente para acessar o desfecho?", 
               "A avaliação do resultado foi cega?",
               "Dados incompletos foram adequadamente endereçados?", 
               "Os relatos do estudo são livres de seleção de desfecho relatado?", 
               "O estudo está aparentemente livre de algum outro problema que poderia resultar em alto risco de viés?") # renomear para a pergunta completa) # estabeler ordem para as perguntas


# Estabelecer tema base para todos os próximos gráficos

theme_set(theme_minimal(base_family = "Gadugi"))



# Visualização ROB SYRCLE Resumo

v_factor_levels <- c("Alto risco", "Incerto", "Baixo risco")

robplot <- df_rob_long %>% 
  group_by(Study) %>% 
  distinct(Study, pergunta, atribuicao) %>% 
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

# Visualização ROB SYRCLE Traffic light


rob <- read_excel("data/rob.xlsx")



robplot_farol <- rob_traffic_light(data = rob[1:10,], tool = "ROB1", colour = c("#82c236", "#fec200", "#ec2b2b"), psize = 5)


robplot_farol <- robplot_farol + geom_point(size = 5)




save_plot(filename = "Figura19.png",
          plot = robplot_farol,
          dpi = 300)

robplot_farol


rob_summary(df_rob, tool = "ROB1", overall = FALSE, weighted = FALSE)
