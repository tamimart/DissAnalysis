# Referência: ~ref da dissertação~
# ETAPA 2: Análise exploratoria 

# Carregar pacotes

library(tidyverse)    # manipulacao de dados
library(skimr)        # resumir estatistica
library(summarytools) # estatisticas descritivas
library(writexl)      # salvar tabela
library(tibble)       # manejo de dataframe
library(tidylog)      # feedback das operacoes tidy

# Carregar dataframe dos dados limpos e organizados

df <- data_geral_clean <- readRDS("C:/Users/Tamires/OneDrive - UFSC/PC LAB/DissAnalysis/data_geral_clean.rds")


# setar estatistica padrao

my_skim <- skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE), 
                                   iqr = ~ IQR(., na.rm = TRUE)),
                     base = sfl(complete = n_complete,
                                missing = n_missing,
                                n = length))

skimmer_function_list
# Rename categorias ----

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


levels(df$strain)[match("NA", levels(df$strain))] <- "Sem info" # mudar termo na linhagem


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
    "foot shock stress",
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
    "Estresse por choque no pé",
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

levels(df$measurement_method)


df$measurement_method <-
  factor(
    df$measurement_method,
    levels = c(
      "manually",
      "manually, chronometers",
      "manually, score60sinterval",
      "video analysis, automated",
      "NA",
      "NA, score5sinterval",            
      "video analysis",
      "video analysis, chronometers",
      "video analysis, manual",
      "video analysis, manual and automated", 
      "video analysis, score5sinterval"
    ),
    labels = c(
      "Manual",
      "Manual, Cronômetro",
      "Manual, Intervalos de 60s",
      "Videoanálise, Automatizada",
      "Sem info",
      "Sem info, Intervalos de 60s",            
      "Videoanálise",
      "Videoanálise, Cronômetro",
      "Videoanálise, Manual",
      "Videoanálise, Manual e automatizada", 
      "Videoanálise, Intervalos de 5s"
    )
  ) 



# Criar nova variavel com detalhe do método de analise do nado forçado


mmd <- df %>% # separar variavel em duas
  select(measurement_method) %>% 
  separate(col = measurement_method, sep = ", ", into = c("measurement_method", "measurement_method_detail"))


df <- df %>%
  mutate(measurement_method_detail = as.factor(mmd$measurement_method_detail),
         measurement_method =  as.factor(mmd$measurement_method)) # adicionar variaveis separadas no df mãe


levels(df$measurement_method_detail) # ver os niveis


df <- df %>% 
  mutate(measurement_method_detail = as.character(measurement_method_detail),
         measurement_method_detail = ifelse(is.na(measurement_method_detail), "Sem info", measurement_method_detail),
         measurement_method_detail =  as.factor(measurement_method_detail)) # transformar NAs em fator "sem info"




# EXEMPLOS Conferir variáveis e valores ------- 


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



# EXEMPLOS Tabelas estatísticas -------


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




## ESTATISTICAS AUXILIARES -----

# Figura1
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
  arrange(N, decreasing = T) 


# outros recortes de ano


countries_year <- df %>%
  select(country, study_reference, language, year) %>%
  group_by(study_reference) %>%
  slice(1) %>% 
  group_by(country, year) %>%
  summarise(N = n()) %>%  # coluna com N de publi por país
  mutate(region = country) 

# 1996

countries_year_1996 <- countries_year %>%
  filter(year <= '1996-01-01') %>% 
  group_by(country) %>% 
  summarise(N = n()) %>%
  arrange(N, decreasing = T)

#2006

countries_year_2006 <- countries_year %>%
  filter(year <= '2006-01-01') %>% 
  group_by(country) %>% 
  summarise(N = n()) %>%
  arrange(N, decreasing = T)

# Figura2

Figura2 <- df %>%
  group_by(study_reference) %>%
  slice(1) %>%
  group_by(year) %>% 
  count(sort = T)

# Figura3

df %>% 
  group_by(species) %>% 
  my_skim()

df %>% 
  group_by(sex) %>% 
  my_skim()


# Figura4

x

# Figura5

age_ss <- df %>%
  group_by(species, sex) %>%
  my_skim(age) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))


# Figura6

weight_ss <- df %>%
  group_by(species, sex) %>%
  my_skim(weight) %>%
  mutate(numeric.median = round(numeric.median, 1),
         numeric.p25 = round(numeric.p25, 1),
         numeric.p75 = round(numeric.p75, 1))


peso_si <- df %>% 
  filter(sex == "Sem info",
         weight >= 375) # verificar se os estudos sem info de sexo (ratos) sao do mesmo autor


# Figura7

model_npubli <- df %>% group_by(study_reference) %>% 
  distinct(study_reference, model_phenotype) %>% 
  filter(model_phenotype != "NA") %>%   
  summarise(counts = n())   # calcular quantas publicaçoes são NA

model_model <- df %>% group_by(study_reference) %>% 
  distinct(study_reference, model_phenotype) %>% 
  group_by(model_phenotype) %>%   
  summarise(counts = n())   # calcular quantas publicaçoes são NA


# Figura8

#b

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

#c 

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

# Figura9

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


# Figura10
x

# Figura11

#b NA


doseunit_c <- df %>%
  filter(species == "Camundongo") %>%  # detalhe dos estudos que relataram outra dose
  group_by(dose_unit, atd_type) %>% 
  my_skim(dose)


df %>%
  filter(species == "Camundongo",
         dose_unit == "mg/kg")  #quantos estudos?
  
  


# Figura12
#b NA

doseunit_r <- df %>%
  filter(species == "Rato")  %>% # detalhe dos estudos que relataram outra dose
  group_by(dose_unit, atd_type) %>% 
  my_skim(dose)


df %>%
  filter(species == "Rato",
         dose_unit == "mg/kg")  #quantos estudos?


# Figura13


stat_t_d_cam <- df %>%
  filter(species == "Camundongo") %>% 
  group_by(treatment_via) %>%
  my_skim(treatment_duration) %>%
  tibble::as_tibble()


# write_xlsx(stat_t_d_cam,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\res\\treat_dur_stat_cam.xlsx")


# Figura14

stat_t_d_rat <- df %>%
  filter(species == "Rato") %>% 
  group_by(treatment_via) %>%
  my_skim(treatment_duration) %>%
  tibble::as_tibble()


# write_xlsx(stat_t_d_rat,"C:\\Users\\Tamires\\OneDrive - UFSC\\PC LAB\\DissAnalysis\\res\\treat_dur_stat_rat.xlsx")


# Figura15 


stat_nado_protocol <- df %>%
  group_by(study_reference, fst_protocol, species) %>% 
  distinct(study_reference, fst_protocol, species) %>% 
  select(study_reference, fst_protocol, species) %>% 
  group_by(species) %>% 
  my_skim(fst_protocol) %>%
  tibble::as_tibble()

# Figura 16

stat_nado_met <- df %>%
  group_by(study_reference, measurement_method, measurement_method_detail, species) %>% 
  distinct(study_reference, measurement_method, measurement_method_detail, species) %>% 
  select(study_reference, measurement_method, measurement_method_detail, species) %>% 
  group_by(species) %>% 
  my_skim(measurement_method, measurement_method_detail) %>%
  tibble::as_tibble()


# Figura 17 

#a

stat_nado <- df %>%
  group_by(
    study_reference,
    cylinder_height,
    cylinder_diameter,
    species
  ) %>%
  distinct(
    study_reference,
    cylinder_height,
    cylinder_diameter,
    species
  ) %>%
  group_by(species, as.logical(cylinder_height), as.logical(cylinder_diameter)) %>%
  my_skim(cylinder_height,
          cylinder_diameter) %>%
  mutate(
    numeric.p.50 = round(numeric.p50, 1),
    numeric.p25 = round(numeric.p25, 1),
    numeric.p75 = round(numeric.p75, 1)
  )

#b

stat_nado_temp <- df %>%
  group_by(
    study_reference,
    water_temperature,
    species
  ) %>%
  distinct(
    study_reference,
    water_temperature,
    species
  ) %>%
  group_by(species, as.logical(water_temperature)) %>%
  my_skim(water_temperature) %>%
  mutate(
    numeric.p.50 = round(numeric.p50, 1),
    numeric.p25 = round(numeric.p25, 1),
    numeric.p75 = round(numeric.p75, 1)
  )


# QUALIDADE





