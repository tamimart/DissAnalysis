# Referência: MARTINS, T. Efeito de antidepressivos em roedores no teste do nado forçado e influência de covariáveis: uma revisão sistemática e meta-análise. Orientador: Cilene Lino de Oliveira. Dissertação — Programa de Pós-Graduação em Farmacologia, Universidade Federal de Santa Catarina, Florianópolis, 2022.
# ETAPA 7: Analise da qualidade ROB SYRCLE e CAMARADES adaptado 

# Carregar pacotes ----


library(tidyverse)    # manipulacao de dados e plotar
library(patchwork)    # juntar plots
library(extrafont)    # adicionar fontes
library(cowplot)      # salvar plot
library(robvis)       # pacote fechado para visualizacao de qualidade
library(readxl)       # ler excel

# Carregar dataframe dos dados limpos e organizados

df <- read_xlsx("data/Data_200FST.xlsx")


# Estabelecer tema base para todos os próximos gráficos

theme_set(theme_minimal(base_family = "Gadugi"))

# ROB ------


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
       #  mutate(Weight = as.numeric(1),
       #         overall = "Unclear") %>% # tirar o # se quiser adicionar um julgamento geral
  relocate(Study, everything())




df_rob_long <- df_rob %>% # colocar em modo longo
  pivot_longer(!c(Study),
               names_to = "pergunta",
               values_to = "atribuicao",
               ) 


# Renomear os níveis 

df_rob_long$atribuicao <-
  factor(
    df_rob_long$atribuicao,
    levels = c("Yes", "No", "Unclear"),
    labels = c("Baixo", "Alto", "Incerto") # colocar significado das atribuições
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





# Visualização ROB SYRCLE Resumo

v_factor_levels <- c("Alto", "Incerto", "Baixo")

robplot <- df_rob_long %>% 
  group_by(Study) %>% 
  distinct(Study, pergunta, atribuicao) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = v_factor_levels), y = ..count..)) +
  geom_bar(position = "fill") + 
  scale_fill_manual("Julgamento do risco de viés", values = c("Baixo" = "#82c236", "Incerto" = "#fec200", "Alto" = "#ec2b2b")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 55)
  ) +
  coord_flip()  + 
  theme(axis.ticks.x = element_line(size = .3),
    axis.line = element_line(size = .3),
        axis.text = element_text(
          size = 8,
          color = "grey20"
        ),
    axis.line.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.text = element_text(size = 7, color = "grey20"),
        legend.title = element_text(size = 8),
        plot.margin = margin(5, 0, 0, 5),
        legend.key.size = unit(.8, "line"),
        panel.grid.major.y = element_line(color = "grey90", size = .1),
        panel.grid.major.x = element_blank()
  )


robplot

save_plot(filename = "Figura18.png",
          plot = robplot,
          dpi = 300,
          path = "Fig")

# ver n de cada fator em cada pergunta

my_skim(df_rob)

# CAMARADES ----

# Isolar variáveis do CAMARADES

df_camarades <- df %>% 
  mutate(Study = str_c(first_author, ", ", lubridate::year(year))) %>% 
  select(starts_with("camarades"), Study) 

df_camarades <- df_camarades %>% 
  distinct() # deixar uma linha por publicação

df_camarades <- df_camarades %>% 
  rename("Publicação revisada por pares" = camarades1,
         "Estudo seguiu algum guia" = camarades2,
         "Declaração de conformidade com os regulamentos de bem-estar animal" = camarades3,
         "Declaração de possíveis conflitos de interesse" = camarades4, 
         "Relato das condições de acondicionamento ou ações para melhora do bem estar dos animais experimentais" = camarades5, 
         "Relato da espécie/linhagem ou características específicas dos animais" = camarades6,
         "Relato do fenótipo de interesse" = camarades7, 
         "Relato da idade, peso ou estágio de vida dos animais" = camarades8, 
         "Relato do sexo dos animais" = camarades9,
         "Relato sobre o método do teste comportamental e aquisição dos desfechos comportamentais" = camarades10,
         "Relato do cálculo amostral" = camarades11) # adicionar topicos

  
df_camarades_longo <- df_camarades %>% # colocar em modo longo
  pivot_longer(!c(Study),
               names_to = "pergunta",
               values_to = "atribuicao",
  ) 


df_camarades_longo$pergunta <- # ordernar topicos
  fct_relevel(
    df_camarades_longo$pergunta, "Publicação revisada por pares",
    "Estudo seguiu algum guia",
    "Declaração de conformidade com os regulamentos de bem-estar animal",
    "Declaração de possíveis conflitos de interesse", 
    "Relato das condições de acondicionamento ou ações para melhora do bem estar dos animais experimentais", 
    "Relato da espécie/linhagem ou características específicas dos animais",
    "Relato do fenótipo de interesse", 
    "Relato da idade, peso ou estágio de vida dos animais", 
    "Relato do sexo dos animais",
    "Relato sobre o método do teste comportamental e aquisição dos desfechos comportamentais",
    "Relato do cálculo amostral")


df_camarades_longo$atribuicao <-  
  factor(
    df_camarades_longo$atribuicao,
    levels = c("No", "Unclear, predatory", "Yes", "Unclear", "Yes, ARRIVE", "Yes, lab animals", "Yes, no conflict"),
    labels = c("Não", "Incerto", "Sim", "Incerto", "Sim", "Sim", "Sim") # renomear atribuições para portugues. OBS categoria que especifica "sem conflito" não é necessária, deixei apenas como "sim"
  )




df_camarades_longo$atribuicao <- # ordernar atribuicoes
  fct_relevel(
    df_camarades_longo$atribuicao, "Não", "Incerto", "Sim")



c_factor_levels <- c("Não", "Incerto", "Sim") # reordenar niveis


# plotar qualidade camarades
camaradesplot <- df_camarades_longo %>% 
  group_by(Study) %>% 
  distinct(Study, pergunta, atribuicao) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = c_factor_levels), y = ..count..)) +
  geom_bar(position = "fill") + 
  scale_fill_manual("Julgamento", values = c("Sim" = "#82c236", "Incerto" = "#fec200", "Não" = "#ec2b2b")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 53)
  ) +
  coord_flip()  + 
  theme(axis.ticks.x = element_line(size = .3),
        axis.line = element_line(size = .3),
        axis.text = element_text(
          size = 8,
          color = "grey20"
        ),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.text = element_text(size = 7, color = "grey20"),
        legend.title = element_text(size = 8),
        plot.margin = margin(5, 0, 0, 5),
        legend.key.size = unit(.8, "line"),
        panel.grid.major.y = element_line(color = "grey90", size = .1),
        panel.grid.major.x = element_blank()
  )

camaradesplot

save_plot(filename = "Figura20.png",
          plot = camaradesplot,
          dpi = 300,
          path = "Fig")

# ver n de cada fator em cada pergunta

my_skim(df_camarades)


# NÃO ESTÁ NA DISSERTACAO ------


# comparação da pontuação ROB para artigos que sao de revista predatorias e não sao


nao_pred <- df %>% 
  filter(camarades1 == "Yes") %>% 
  select(starts_with("rob"))

nao_pred <- nao_pred %>% 
  pivot_longer(everything(),
               names_to = "pergunta",
               values_to = "atribuicao")
fnao_pred <- nao_pred %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao), y = ..count..)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Julgamento", values = c("Yes" = "green", "Unclear" = "yellow", "No" = "red")) +
  labs(title = "nao predatoria")



pred <- df %>% 
  filter(camarades1 == "Unclear, predatory") %>% 
  select(starts_with("rob"))

pred <- pred %>% 
  pivot_longer(everything(),
               names_to = "pergunta",
               values_to = "atribuicao")
fpred <- pred %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao), y = ..count..)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Julgamento", values = c("Yes" = "green", "Unclear" = "yellow", "No" = "red")) +
  labs(title = "predatoria")


comp_pred <- fpred + fnao_pred

comp_pred

# comparação da pontuação CAMARADES para artigos que sao de revista predatorias e não sao


nao_pred_c <- df %>% 
  filter(camarades1 == "Yes") %>% 
  select(starts_with("camarades"))

nao_pred_c <- nao_pred_c %>% 
  pivot_longer(everything(),
               names_to = "pergunta",
               values_to = "atribuicao")
fnao_pred_c <- nao_pred_c %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao), y = ..count..)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Julgamento", values = c("Yes" = "green", "Yes, ARRIVE" = "green1", "Yes, lab animals" = "green3", "Yes, no conflict" = "greenyellow", "Unclear" = "yellow", "Unclear, predatory" = "yellow2", "No" = "red")) +
  labs(title = "nao predatoria")



pred_c <- df %>% 
  filter(camarades1 == "Unclear, predatory") %>% 
  select(starts_with("camarades"))

pred_c <- pred_c %>% 
  pivot_longer(everything(),
               names_to = "pergunta",
               values_to = "atribuicao")
fpred_c <- pred_c %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao), y = ..count..)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Julgamento", values = c("Yes" = "green", "Yes, ARRIVE" = "green1", "Yes, lab animals" = "green3", "Yes, no conflict" = "greenyellow", "Unclear" = "yellow", "Unclear, predatory" = "yellow2", "No" = "red")) +
  labs(title = "predatoria")


comp_pred_c <- fpred_c + fnao_pred_c

comp_pred_c

# Visualização ROB SYRCLE Traffic light 


rob <- read_excel("data/rob.xlsx")


robplot_sinal <- rob_traffic_light(data = rob[1:10,], tool = "ROB1", colour = c("#82c236", "#fec200", "#ec2b2b"))

robplot_sinal <-
  robplot_sinal + theme(
    text = element_text(size = 5),
    plot.caption = element_text(
      size = 5,
      hjust = 0,
      vjust = 5
    ),
    legend.text = element_text(size = 5),
    fill = guide_legend(override.aes = list(size = 2)))

robplot_sinal


save_plot(filename = "robplot_sinal.png",
          plot = robplot_sinal,
          dpi = 300,
          path = "Fig")
