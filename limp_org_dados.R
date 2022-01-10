# Limpar e organizar dados

# Carregar pacotes

library(readxl)
library(tidyverse)
library(lubridate)
library(skimr)

# Importando aba especifica do excel 
# As colunas em comum (de identificação) deverao ter os mesmos nomes em cada aba que sera fundida


meus_dados_library <- read_excel("data/DataExtraction_RsGeral_sorteioI.xlsx", sheet = "Library")

meus_dados_info <- read_excel("data/DataExtraction_RsGeral_sorteioI.xlsx", sheet = "Extraction info")

meus_dados_outcome <- read_excel("data/DataExtraction_RsGeral_sorteioI.xlsx", sheet = "FST imm. duration")

meus_dados_quality <- read_excel("data/DataExtraction_RsGeral_sorteioI.xlsx", sheet = "References Quality")


# Retirar linhas dos artigos excluidos na etapa de extracao

meus_dados_library <- meus_dados_library %>% 
  filter(Included == TRUE)


# Retirar linhas vazias

meus_dados_quality <- meus_dados_quality %>%
  filter(`First author` != 0,
         `4 - Os animais foram acondicionados aleatoriamente?` != "NA")

# Juntar infos em uma so planilha. Juncao determinada por variaveis em comum.


data1 <- dplyr::left_join(meus_dados_library, meus_dados_info, by = c("First author", "year"))

data2 <- dplyr::left_join(data1, meus_dados_outcome, by = "line")

data_geral <- dplyr::left_join(data2, meus_dados_quality, by = "ID")

# Olhar planilha e caracteristicas das variaveis

glimpse(data_geral)

view(data_geral)


# Selecionando colunas que ficarao na planilha final

data_geral <- data_geral %>% # Excluir colunas/variaveis repetidas e desnecessarias
  select(everything(), - ...25, - mgkg, - Included, - `First author.y`, - comp.y, `First author`, - FSTApparatus_conditions, - `Housing conditions`, - year.y, - source.y, -`First author.y`, - comp.y, -`Escale (mm)`, -`Escale (s or %)`, -`mean CTRL or ATD (mm)`, -`SEM CTRL or ATD (mm)`, -`mean ADT (mm)` , -`SEM ADT (mm)`) 
  
  
data_geral <- data_geral %>% # Renomear colunas/variaveis
  rename(obs_quali = `10`,
         ROB1 = `1- A alocação de tratamento foi adequadamente gerada e aplicada? (*)`,
         ROB2 = `2 - Os grupos (controle e tratado) eram similares no início do experimento?`,
         ROB3 = `3 - A alocação foi adequadamente escondida?`,
         ROB4 = `4 - Os animais foram acondicionados aleatoriamente?`,
         ROB5 = `5 - Os investigadores eram cegos quanto ao tratamento durante os experimentos?`,
         ROB6 = `6 - Os animais foram selecionados aleatoriamente para acessar o desfecho?`,
         ROB7 = `7 - A avaliação do resultado foi cega?`,
         ROB8 = `8- Dados incompletos foram adequadamente endereçados?  (*)`,
         ROB9 = `9 - Os relatos do estudo são livres de seleção de desfecho relatado? (*)`,
         ROB10 = `10 - O estudo está aparentemente livre de algum outro problema que poderia resultar em alto risco de viés? (*)`,
         CAMARADES1 = `11- Publicação revisada por pares (*)`,
         CAMARADES2 = `12- Estudo seguiu algum guia, e.g. ARRIVE guidelines.`,
         CAMARADES3 = `13- Declaração de conformidade com os regulamentos de bem-estar animal. (*)`,
         CAMARADES4 = `14- Declaração de possíveis conflitos de interesse. (*)`,
         CAMARADES5 = `15 - Relato das condições de acondicionamento ou ações para melhora do bem estar dos animais experimentais, e.g. ambiente enriquecido.`,
         CAMARADES6 = `16- Relato da espécie/linhagem ou características específicas dos animais, e.g. knockouts.`,
         CAMARADES7 = `17- Relato do fenótipo de interesse, e.g. estressado e/ou depressivo. (*)`,
         CAMARADES8 = `18- Relato da idade, peso ou estágio de vida dos animais.`,
         CAMARADES9 = `19- Relato do sexo dos animais.`,
         CAMARADES10 = `20- Relato sobre o método do teste comportamental e aquisição dos desfechos comportamentais.`,
         CAMARADES11 = `21- Relato do cálculo amostral. (*)`,
         first_author = `First author.x`,
         sex = `Sex (M, F)`,
         species = `Species (Rat, Mice)`,
         weight = `Body Weight (g)`,
         bioterium_temp = `Bioterium_temperature(C°)`,
         bioterium_umid = `Bioterium_umidity(%)`,
         comparator = `Comparator (CTRL or ATD: Antidepressant dose)`,
         atd_type = `Type ATD`,
         atd_class = `ATD class`,
         treatment_duration =`duration treatment (n° days)`,
         treatment_via = `Treatment type (IP, oral)`,
         treatment_freq = `Treatment frequency/day`,
         last_bf_outcome = `Last adm before outcome (h)`,
         fst_protocol = `FST protocol`,
         measurement_method = `Measurement method`,
         cylinder_height = `cylinder_height(cm)`,
         cylinder_diameter = `cylinder_diameter(cm)`,
         water_depth = `water_depth(cm)`,
         water_temperature = `water_temperature(C°)`,
         others_tests = `Others behavioural tests before  FST`,
         treemore_arms = OBSERVAÇÃO,
         year = year.x,
         source = source.x,
         seq = comp.x,
         measure_unit = `Measure/Unity`,
         obs_design = Observations,
         n_comparisons = `N Comparisons`,
         atd_sd = `SDM ADT`,
         atd_n_round = `N ADT (rounded)`,
         atd_n_ext =`N ATD (extraction)`,
         atd_se = `SEM ADT`,
         atd_mean = `mean ADT (s ou %)`,
         ctr_sd = `SDM CTRL or ATD`,
         ctr_n_round = `N CTRL OR ATD (rounded)`,
         ctr_n_ext = `N CTRL OR ATD (extraction)`,
         ctr_se = `SEM CTRL or ATD`,
         ctr_mean = `mean CTRL or ATD(s or %)`,
         study_reference = `Selected studies reference (style=Numbered)`,
         model_phenotype = `Stress-Model/phenotype`) %>% 
  rename_with(., ~ tolower(gsub(".", "_", .x, fixed = TRUE))) 




# NAO RODAR DE NOVO - Percebi que alguns estudos estao sme o n, aqui esta o filtro para localizar esses artigos:

artigos_sem_n_ctr <- data_geral %>% 
  filter(ctr_n_round == "NA") %>% 
  select(first_author, line)

artigos_sem_n_atd <- data_geral %>% 
  filter(atd_n_round == "NA") %>% 
  select(first_author)  # sao os mesmo artigos 

# Entrei em contato com os ultimos tres autores via email ou researchgate, aguardando resposta


# Transformando variaveis de acordo com suas caracteristicas: character, factor, numeric... 
# No caso das numericas, se havia texto, esses serao transformados em "NA"

data_geral <- data_geral %>%
  mutate(ctr_n_round = as.numeric(ctr_n_round),
         atd_n_round = as.numeric(atd_n_round),
         id = as.character(id),
         idgeral = as.character(idgeral),
         year = as.numeric(year),
         line = as.character(line),
         sex = as.factor(sex),
         dose = as.numeric(dose),
         strain = as.factor(strain),
         treatment_via = as.factor(treatment_via),
         country = as.factor(country),
         species = as.factor(species),
         treatment_duration = as.numeric(treatment_duration),
         treatment_freq = as.numeric(treatment_freq),
         last_bf_outcome = as.numeric(last_bf_outcome),
         cylinder_height = as.numeric(cylinder_height),
         cylinder_diameter = as.numeric(cylinder_diameter),
         water_depth = as.numeric(water_depth),
         comparator = as.factor(comparator),
         atd_class = as.factor(atd_class),
         atd_type = as.factor(atd_type),
         fst_protocol = as.factor(fst_protocol),
         measurement_method = as.factor(measurement_method),
         measure_unit = as.factor(measure_unit),
         atd_n_round = as.integer(atd_n_round),
         rob1 = as.factor(rob1),
         rob2 = as.factor(rob2),
         rob3 = as.factor(rob3),
         rob4 = as.factor(rob4),
         rob5 = as.factor(rob5),
         rob6 = as.factor(rob6),
         rob7 = as.factor(rob7),
         rob8 = as.factor(rob8),
         rob9 = as.factor(rob9),
         rob10 = as.factor(rob10),
         camarades1 = as.factor(camarades1),
         camarades2 = as.factor(camarades2),
         camarades3 = as.factor(camarades3),
         camarades4 = as.factor(camarades4),
         camarades5 = as.factor(camarades5),
         camarades6 = as.factor(camarades6),
         camarades7 = as.factor(camarades7),
         camarades8 = as.factor(camarades8),
         camarades9 = as.factor(camarades9),
         camarades10 = as.factor(camarades10),
         camarades11 = as.factor(camarades11)
         )


# Nova coluna com n do comparador corrigido de acordo com o numero de comparacoes e arredondado

data_geral <- data_geral %>%
  mutate(ctr_n_corr = as.integer(ctr_n_round / n_comparisons)) 

# Reorganizar ordem das variaveis


colnames(data_geral) # Obter nome das colunas


col_order <- c("line", # Colocar na ordem desejada
               "idgeral",
               "id",
               "study_reference",
               "authors",
               "first_author",
               "year",
               "title",
               "language",
               "country",
               "source",
               "seq",
               "outcome",
               "treemore_arms",
               "measure_unit",
               "ctr_mean",
               "ctr_sd",
               "ctr_se",
               "ctr_n_ext",
               "ctr_n_round",
               "ctr_n_corr",
               "n_comparisons",
               "atd_mean",
               "atd_sd",
               "atd_se",
               "atd_n_ext",
               "atd_n_round",
               "obs_design",
               "species",
               "strain",
               "sex",
               "age",
               "weight",
               "model_phenotype",
               "cage_measures",
               "animals_percage",
               "bioterium_lightcycle",
               "bioterium_temp",
               "bioterium_umid",
               "comparator",
               "atd_type",
               "atd_class",
               "dose",
              "treatment_duration", 
              "treatment_freq",
              "treatment_via",
              "last_bf_outcome",
              "fst_protocol",
              "measurement_method",
              "cylinder_height",
              "cylinder_diameter",
              "water_depth",
              "water_temperature",
              "others_tests",
              "rob1",
              "rob2",
              "rob3",
              "rob4",
              "rob5",
              "rob6",
              "rob7",
              "rob8",
              "rob9",
              "rob10",
              "camarades1",
              "camarades2",
              "camarades3",
              "camarades4",
              "camarades5",
              "camarades6",
              "camarades7",
              "camarades8",
              "camarades9",
              "camarades10",
              "camarades11",
              "obs_quali")   
 
data_geral_reord <- data_geral[, col_order] # Adicionar nova sequencia de colunas

# NAO RODAR DE NOVO - Verificar qual coluna sumiu depois de ordenar as variaveis: Era uma repetida de primeiro autor, e dose_mgkg (se a dose está nessa unidade)...

diffdf::diffdf(data_geral, data_geral_reord) # MODO 1

janitor::compare_df_cols(data_geral, data_geral_reord) # MODO 2 


# Verificar os padrao dos valores

skim(data_geral_reord) # Percebi que nas colunas de qualidade alguns niveis de fatores estavam escritos de duas formas


# Verificando os niveis de todas variaveis

sapply(data_geral_reord, levels)


# Corrigindo os valores escritos de forma errada e emergindo os que foram escritos de maneiras diferentes

#atd_type

levels(data_geral_reord$atd_type)[match("bupropiona",levels(data_geral_reord$atd_type))] <- "bupropion" #substituir valor

summary(data_geral_reord$atd_type)


#camarades3

levels(data_geral_reord$camarades3)[match("yes",levels(data_geral_reord$camarades3))] <- "Yes"

summary(data_geral_reord$camarades3)


#measurement_method


levels(data_geral_reord$measurement_method)[match("VIdeo analysis",levels(data_geral_reord$measurement_method))] <- "video analysis"

levels(data_geral_reord$measurement_method)[match("score5sinterval",levels(data_geral_reord$measurement_method))] <- "Unclear, score5sinterval"

levels(data_geral_reord$measurement_method)[match("manually, digital chronometers",levels(data_geral_reord$measurement_method))] <- "manually, chronometers"

levels(data_geral_reord$measurement_method)[match("MicroAct Scratching Test",levels(data_geral_reord$measurement_method))] <- "video analysis, automated"

levels(data_geral_reord$measurement_method)[match("video analysis, automatically analysis",levels(data_geral_reord$measurement_method))] <- "video analysis, automated"

summary(data_geral_reord$measurement_method)


# Strain

levels(data_geral_reord$strain)[match(c("balb/c", "BALB/C", "BALB/c", "balb/CJ", "BALB/CJ", "BALB/CByJ", "Balb/CJ"), levels(data_geral_reord$strain))] <- "BALB" #substituir valor

levels(data_geral_reord$strain)[match(c("CB57BL/6J", "C57BL/6J", "C57/BL6","C57BL/6", "C57BL/6N"),levels(data_geral_reord$strain))] <- "C57BL" 

levels(data_geral_reord$strain)[match(c("CD", "CD1", "ICR"), levels(data_geral_reord$strain))] <- "CD-1" 

levels(data_geral_reord$strain)[match("Kumming", levels(data_geral_reord$strain))] <- "kunming" 

levels(data_geral_reord$strain)[match(c("SD", "sprague-dawley"), levels(data_geral_reord$strain))] <- "sprague dawley" 

levels(data_geral_reord$strain)[match("wistar-kyoto", levels(data_geral_reord$strain))] <- "wistar kyoto"

levels(data_geral_reord$strain)[match("Slc:ddY", levels(data_geral_reord$strain))] <- "ddY"

summary(data_geral_reord$strain)


# Identificando e verificando os estudos com valores que parecem erros (modificavel)

fst_errado <- data_geral_reord %>% 
  filter(fst_protocol == "test6test6") %>% 
  select(line, first_author, fst_protocol)



# Salvar df limpo e corrigido para posterior analise

saveRDS(data_geral_reord, "data_geral_clean.rds")



colnames(data_geral_clean)
