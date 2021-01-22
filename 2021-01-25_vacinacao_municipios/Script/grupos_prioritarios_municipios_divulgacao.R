################################################################################
################################################################################
################################################################################
############ C Á L C U L O   G R U P O S   P R I O R I T Á R I O S #############
######################## V A C I N A   C O V I D - 1 9 #########################
################################################################################
################ AUTOR: Marco Brancher - IMPULSO | JANEIRO 2020 ################
################################################################################
################################################################################
################################################################################

### Limpa memória
rm(list = ls())

### Leitura de pacotes necessários
library(dplyr)
library(readxl)
library(readr)
library(openxlsx)
library(PNADcIBGE)
library(PNSIBGE)

### FUNÇÃO LEFT
left = function(text, num_char) {substr(text, 1, num_char)}

### Definição da pasta com as bases de dados necessárias
setwd("PASTA COM OS DADOS")

### Download base com código de municípios, nome de UF e código de UF formatada
cities_id <- read_csv(url("http://datasource.coronacidades.org/br/cities/farolcovid/main")) %>% 
    select(state_id, state_name, city_id, city_name) %>% 
    mutate(state_code = left(city_id, 2),
           city_id_6 = left(city_id, 6))



################################################################################
########## população por município e faixa etária - Min. Saúde - 2020 ##########
################################################################################
pop_mun_2020 <- read_delim("pop_faixas_etarias_mun.csv", ";", 
                           escape_double = FALSE, trim_ws = TRUE) %>% 
    mutate(UF = left(municipio, 2))

### Cálculo das populações de pessoas com menos de 60 anos e total por UF
pop_uf_2020 <- pop_mun_2020 %>% 
    group_by(UF) %>% 
    summarise(populacao_2020 = sum(Total),
              populacao_menos_60_2020 = sum(`0_4`, `5_9`, `10_14`, `15_19`, 
                                            `20_24`, `25_29`, `30_34`, `35_39`,
                                            `40_44`, `45_49`, `50_54`, `55_59`))



################################################################################
#################### população indígena aldeada - DSEI 2020 ####################
################################################################################
indigenas <- read_excel("populacao_indigena_dsei.xlsx", 
                        col_types = c("numeric", "skip", "skip", "numeric"))

### Renomeia variáveis
colnames(indigenas) <- c("municipio", "indigenas")



################################################################################
################### população institucionalizada - SUAS - 2019 #################
################################################################################
censo_SUAS_original <- read_excel("censo_SUAS.xlsx") 

### Seleciona variáveis de interesse e calcula número de pessoas acima de 60 anos 
### institucionalizadas e o número de portadores de deficiência entre 18 e 59 anos
censo_SUAS_2019 <- censo_SUAS_original %>% 
    select(IBGE, q1_1, q12, q15_1_7, q15_1_8, q15_1_9, q15_1_10, q15_2_7, 
           q15_2_8, q15_2_9, q15_2_10) %>% 
    mutate(UF = left(IBGE,2),
           idosos = q15_1_9 + q15_1_10 + q15_2_9 + q15_2_10, #pessoas acima de 60
           deficientes = if_else(q1_1 == "Exclusivamente pessoas adultas com Deficiência",
                                 q15_1_7 + q15_1_8 + q15_2_7 + q15_2_8,
                                 0)) %>% #deficientes entre 18 e 59
    group_by(IBGE) %>% 
    summarise(idosos = sum(idosos)*2,
              deficientes = sum(deficientes)*2) %>% 
    mutate(IBGE = as.numeric(IBGE),
           institucionalizados = idosos + deficientes) %>% 
    select(IBGE, institucionalizados) 

### Renomeia variáveis
colnames(censo_SUAS_2019) <- c("municipio", "institucionalizados")



################################################################################
################### trabalhadores da saúde - CNES - nov.2020 ###################
################################################################################
trabalhadores_saude <- read_delim("trabalhadores_saude_detalhado.csv", ";", 
                                  escape_double = FALSE, 
                                  trim_ws = TRUE) %>% 
    group_by(CO_MUNICIPIO) %>% 
    summarise(trab_saude = sum(TOTAL))
    


################################################################################
############################### PNAD 2019 - IBGE ###############################
################################################################################
pnad_txt <- "PASTA COM TXT DA PNAD 2019" 
pnad_input <- "PASTA COM INPUT DA PNAD 2019" 

### Leitura PNS
PNAD_2019_original <- read_pnadc(pnad_txt, input= pnad_input)

### Cálculo do número de profissionais de saúde por UF e faixa etária a partir 
### dos códigos de ocupação selecionados
prof_saude_2019_desag <- PNAD_2019_original %>% 
    select(UF, V4010, V1032, V2009) %>% 
    mutate(ocup = case_when(V4010 == 1342 ~ "Profissionais da saúde",
                            V4010 == 1343 ~ "Profissionais da saúde",
                            V4010 == 2211 ~ "Profissionais da saúde",
                            V4010 == 2212 ~ "Profissionais da saúde",
                            V4010 == 2221 ~ "Profissionais da saúde",
                            V4010 == 2222 ~ "Profissionais da saúde",
                            V4010 == 2230 ~ "Profissionais da saúde",
                            V4010 == 2240 ~ "Profissionais da saúde",
                            V4010 == 2261 ~ "Profissionais da saúde",
                            V4010 == 2261 ~ "Profissionais da saúde",
                            V4010 == 2263 ~ "Profissionais da saúde",
                            V4010 == 2264 ~ "Profissionais da saúde",
                            V4010 == 2265 ~ "Profissionais da saúde",
                            V4010 == 2266 ~ "Profissionais da saúde",
                            V4010 == 2267 ~ "Profissionais da saúde",
                            V4010 == 2269 ~ "Profissionais da saúde",
                            V4010 == 3211 ~ "Profissionais da saúde",
                            V4010 == 3112 ~ "Profissionais da saúde",
                            V4010 == 3213 ~ "Profissionais da saúde",
                            V4010 == 3214 ~ "Profissionais da saúde",
                            V4010 == 3221 ~ "Profissionais da saúde",
                            V4010 == 3222 ~ "Profissionais da saúde",
                            V4010 == 3230 ~ "Profissionais da saúde",
                            V4010 == 3251 ~ "Profissionais da saúde",
                            V4010 == 3252 ~ "Profissionais da saúde",
                            V4010 == 3253 ~ "Profissionais da saúde",
                            V4010 == 3254 ~ "Profissionais da saúde",
                            V4010 == 3255 ~ "Profissionais da saúde",
                            V4010 == 3256 ~ "Profissionais da saúde",
                            V4010 == 3257 ~ "Profissionais da saúde",
                            V4010 == 3258 ~ "Profissionais da saúde",
                            V4010 == 3259 ~ "Profissionais da saúde",
                            V4010 == 5321 ~ "Profissionais da saúde",
                            V4010 == 5322 ~ "Profissionais da saúde",
                            V4010 == 5329 ~ "Profissionais da saúde"),
           faixa_etaria = case_when(V2009 < 60 ~ "60 -",
                                    V2009 >= 60 & V2009 <= 64 ~ "60 - 64",
                                    V2009 >= 65 & V2009 <= 69 ~ "65 - 69",
                                    V2009 >= 70 & V2009 <= 74 ~ "70 - 74",
                                    V2009 >= 75 & V2009 <= 79 ~ "75 - 79",
                                    TRUE ~ "80 +")) %>% 
    filter(is.na(ocup) == FALSE)

### Cria base de dados com número de trabalhadores da saúde por UF em 2019
prof_saude_2019_uf <- prof_saude_2019_desag %>% 
    group_by(UF, ocup) %>% 
    summarise(prof_saude_uf = sum(V1032)) %>% 
    ungroup() %>% 
    select(UF, prof_saude_uf)

### Cria base de dados com número de trabalhadores da saúde por UF e idade em 2019
prof_saude_2019_uf_idade <- prof_saude_2019_desag %>% 
    group_by(UF, ocup, faixa_etaria) %>% 
    summarise(prof_saude_uf_idade = sum(V1032)) %>% 
    ungroup() %>% 
    select(UF, faixa_etaria, prof_saude_uf_idade)

### Merge bases 
prof_saude_2019_uf_idade <- left_join(prof_saude_2019_uf_idade, prof_saude_2019_uf,
                                      by = "UF")

### Cálculo das proporções de trabalhadores da saúde por faixa etária e UF
prof_saude_2019_uf_idade <- prof_saude_2019_uf_idade %>% 
    mutate(share = prof_saude_uf_idade / prof_saude_uf,
           share_saude_60_menos = if_else(faixa_etaria == "60 -", share, 0),
           share_saude_60_64 = if_else(faixa_etaria == "60 - 64", share, 0),
           share_saude_65_69 = if_else(faixa_etaria == "65 - 69", share, 0),
           share_saude_70_74 = if_else(faixa_etaria == "70 - 74", share, 0),
           share_saude_75_79 = if_else(faixa_etaria == "75 - 79", share, 0),
           share_saude_80_mais = if_else(faixa_etaria == "80 +", share, 0)) %>% 
    group_by(UF) %>% 
    summarise( share_saude_60_menos = sum(share_saude_60_menos),
               share_saude_60_64 = sum(share_saude_60_64),
               share_saude_65_69 = sum(share_saude_65_69),
               share_saude_70_74 = sum(share_saude_70_74),
               share_saude_75_79 = sum(share_saude_75_79),
               share_saude_80_mais = sum(share_saude_80_mais)) %>% 
    ungroup()



################################################################################
############################### PNS 2019 - IBGE ################################
################################################################################
pns_txt <- "PASTA COM TXT DA PNS 2019" 
pns_input <- "PASTA COM INPUT DA PNS 2019" 

### Leitura PNS
pns2019_original <- read_pns(pns_txt, pns_input)

### Seleciona variáveis de interesse da PNS
pns2019 <- pns2019_original %>% 
    select(V0001, V0024, UPA_PNS, V0006_PNS, C00301,
           V0026, V0031, B001, C006, C008, 
           C009, I00102, J007, P00104, P00404, 
           P005, P050, P052, Q00201, Q00202, 
           Q03001, Q03002, Q06306, Q06307, Q06308, 
           Q06309, Q06310, Q074, Q079, Q092, 
           Q11604, Q11605, Q11606, Q11807, Q120, 
           Q12102, Q12104, Q12105, Q12106, Q12107, 
           Q12108, Q12109, Q121010, Q121011, Q121012, 
           Q121013, Q121014, Q121015, Q124, V0029, 
           V00291, V00282, V00292, Q12501) %>% 
    mutate(id = paste0(V0001, V0024, UPA_PNS, V0006_PNS, C00301))

### Renomeia variáveis da PNS
colnames(pns2019) <- c("UF", "Estrato", "UPA", "num_ordem_dom", 
                          "num_ordem_morador", "sit_censitaria", "tipo_area", 
                          "cadastro_ESF", "sexo", "idade", "raça", "plano_saude", 
                          "doenca_cronica", "peso_kg", "altura_cm", "gravida", 
                          "fumante_atual", "fumante_passado", "hipertensao", 
                          "hipertensao_gravidez", "diabetes", "diabetes_gravidez", 
                          "doenca_cardiaca","infarto", "angina", "insuf_cardiaca", 
                          "arritmia", "asma", "artrite", "depressão", 
                          "doenca_cronica_pulmao", "enfisema", "bronquite", 
                          "usa_oxigenio", "cancer", "cancer_pele", "cancer_pulmao", 
                          "cancer_colon",  "cancer_estomago", "cancer_mama", 
                          "cancer_utero", "cancer_prostata", "cancer_boca", 
                          "cancer_bexiga", "cancer_leucemia", "cancer_cerebro", 
                          "cancer_ovario",  "cancer_tireoide", "insuf_renal", 
                          "peso_sem_calibracao", "peso_com_calibracao", 
                          "proj_pop", "proj_pop_moradores_selec",  "transplante", 
                          "id")

### Cálculo da população total e de maiores de 60 anos na PNS
pop_total_pns <- pns2019 %>%
    mutate(peso_com_calibracao_menos_60 = if_else(idade<60, 
                                                  peso_com_calibracao, 
                                                  0)) %>% 
    group_by(UF) %>% 
    summarise(populacao_pns = round(sum(peso_com_calibracao, na.rm = TRUE),0),
              populacao_menos_60_pns = round(sum(peso_com_calibracao_menos_60, 
                                                 na.rm = TRUE),0))

### Cria variáveis indicadoras de comorbidades selecionadas
comorbidades_2019 <- pns2019 %>% 
    mutate(imc = peso_kg / ((altura_cm/100)*(altura_cm/100)),
           obesidade = if_else(imc >= 40, 
                               "sim", 
                               "não"),
           gravida = if_else(gravida == 1, 
                             "sim", 

           hipertensao = if_else(hipertensao == 1 & hipertensao_gravidez != 1, 
                                 "sim", 
                                 "não"),
           diabetes = if_else(diabetes == 1 & diabetes_gravidez != 1, 
                              "sim", 
                              "não"),
           doenca_cardiaca = if_else(doenca_cardiaca == 1, 
                                     "sim", 
                                     "não"),
           asma = if_else(asma == 1, 
                          "sim", 
                          "não"),
           doenca_cronica_pulmao = if_else(doenca_cronica_pulmao == 1, 
                                           "sim", 
                                           "não"),
           doenca_pulmonar = if_else(doenca_cronica_pulmao == 1 | asma == 1, 
                                     "sim", 
                                     "não"),
           cancer = if_else(cancer == 1, 
                            "sim",
                            "não"),
           insuf_renal = if_else(insuf_renal == 1, 
                                 "sim", 
                                 "não"),
           transplante = if_else(transplante == 1, 
                                 "sim", 
                                 "não"),
           faixa_etaria = if_else(idade < 60, 
                                  "60-", 
                                  if_else(idade <= 64, 
                                          "60-64", 
                                          if_else(idade <= 69,
                                                  "65-69",
                                                  if_else(idade <= 74,
                                                          "70-74",
                                                          if_else(idade <= 79,
                                                                  "75-79",
                                                                  "80+"))))))%>% 
    select(UF, id, sit_censitaria, tipo_area, idade, raça, diabetes, hipertensao,
           doenca_pulmonar, insuf_renal, doenca_cardiaca, transplante, 
           cancer, obesidade, peso_com_calibracao) %>% 
    mutate(comorbidade = if_else(diabetes == "sim" |
                                     hipertensao == "sim" |
                                     doenca_pulmonar == "sim" |
                                     insuf_renal == "sim" |
                                     doenca_cardiaca == "sim" |
                                     transplante == "sim" |
                                     cancer == "sim" |
                                     obesidade == "sim",
                                 1,
                                 0)) %>% 
    select(UF, id, sit_censitaria, tipo_area, idade, raça, comorbidade,
           peso_com_calibracao) %>% 
    filter(is.na(peso_com_calibracao) == FALSE) %>% 
    mutate(idade_60 = if_else(idade<60,
                              1,
                              0),
           comorbidade = if_else(is.na(comorbidade) == TRUE,
                                 0,
                                 1),
           pop_comorbidade_menos_60 = peso_com_calibracao*idade_60*comorbidade, 
           pop_comorbidade_total = peso_com_calibracao*comorbidade) %>% 
    group_by(UF) %>% 
    summarise(pop_comorbidade_menos_60_2019 = round(sum(pop_comorbidade_menos_60),0),
              pop_comorbidade_total_2019 = round(sum(pop_comorbidade_total),0)) %>% 
    ungroup() 


comorbidades_2020 <- merge(comorbidades_2019, pop_total_pns,
                           by.x = "UF",
                           by.y = "UF")

comorbidades_2020 <- merge(comorbidades_2020, pop_uf_2020,
                           by.x = "UF",
                           by.y = "UF")

### Aplica fator de multiplicação nos resultados da PNS para adequar as populações
comorbidades_2020 <- comorbidades_2020 %>% 
    mutate(pop_comorbidade_menos_60_2020 = round(pop_comorbidade_menos_60_2019 
                                                 / populacao_menos_60_pns * 
                                                     populacao_menos_60_2020,0),
           pop_comorbidade_total_2020 = round(pop_comorbidade_total_2019 /
                                        populacao_pns * populacao_2020,0)) %>% 
    select(UF, pop_comorbidade_menos_60_2020, pop_comorbidade_total_2020, 
           populacao_2020, populacao_menos_60_2020)

### Renomeia variáveis PNS
colnames(comorbidades_2020) <- c("UF", "comorbidade_uf_menos_60",
                                 "comorbidade_uf", "pop_uf", "pop_uf_menos_60")



################################################################################
############################## A G R E G A Ç Ã O ###############################
################################################################################

### Dados demográficos
vacinacao_quant_bruto <- pop_mun_2020 %>% 
    mutate(UF = left(municipio,2), 
           pop_80_mais = `>80`,
           pop_75_79 = `75_79`,
           pop_70_74 = `70_74`,
           pop_65_69 = `65_69`,
           pop_60_64 = `60_64`,
           pop_menos_60 = `0_4`+`5_9`+`10_14`+`15_19`+`20_24`+ 
               `25_29`+`30_34`+`35_39`+`40_44`+`45_49`+`50_54`+`55_59`,
           pop_total = Total) %>% 
    select(UF, municipio, 
           pop_60_64, pop_65_69, pop_70_74, 
           pop_75_79, pop_80_mais, pop_menos_60, pop_total)

### Merge com número de trabalhadores da saúde
vacinacao_quant_bruto <- merge(vacinacao_quant_bruto, trabalhadores_saude,
                              by.x = "municipio",
                              by.y = "CO_MUNICIPIO")

### Merge com número de pessoas com pelo menos uma comorbidade e menos de 60 anos
vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, comorbidades_2020,
                                  by = "UF") %>% 
    mutate(comorbidade_total = round(pop_total * comorbidade_uf / pop_uf,0),
           comorbidade_menos_60 = round(pop_menos_60 * comorbidade_uf_menos_60 /
                                            pop_uf_menos_60,0))

### Merge com proporção de trabalhadores da saúde por faixa etária
vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, prof_saude_2019_uf_idade,
                                  by = "UF")

### Merge com dados do Censo SUAS
vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, censo_SUAS_2019,
                                   by = "municipio")

###Merge com número de indígenas aldeados
vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, indigenas,
                                   by = "municipio") %>% 
    mutate(indigenas = if_else(is.na(indigenas) == TRUE,
                               0,
                               indigenas),
           institucionalizados = if_else(is.na(institucionalizados) == TRUE,
                                         0,
                                         institucionalizados))



################################################################################
############### C Á L C U L O   V A L O R E S   L Í Q U I D O S ################
################################################################################

vacinacao_quant_liquido_mun <- vacinacao_quant_bruto %>% 
    group_by(municipio) %>% 
    mutate(pop_80_mais = round(pop_80_mais - (share_saude_80_mais * trab_saude) 
                               - (pop_80_mais * (institucionalizados / 
                                                     (pop_80_mais + pop_75_79 + 
                                                          pop_70_74 + pop_65_69 
                                                      + pop_60_64))),0),
           pop_75_79 = round(pop_75_79 - (share_saude_75_79 * trab_saude) - 
                                 (pop_75_79 * (institucionalizados / 
                                                   (pop_80_mais + pop_75_79 + 
                                                        pop_70_74 + pop_65_69 + 
                                                        pop_60_64))),0),
           pop_70_74 = round(pop_70_74 - (share_saude_70_74 * trab_saude) - 
                                 (pop_70_74 * (institucionalizados / 
                                                   (pop_80_mais + pop_75_79 + 
                                                        pop_70_74 + pop_65_69 + 
                                                        pop_60_64))),0),
           pop_65_69 = round(pop_65_69 - (share_saude_65_69 * trab_saude) - 
                                 (pop_65_69 * (institucionalizados / 
                                                   (pop_80_mais + pop_75_79 + 
                                                        pop_70_74 + pop_65_69 + 
                                                        pop_60_64))),0),
           pop_60_64 = round(pop_60_64 - (share_saude_60_64 * trab_saude) - 
                                 (pop_60_64 * (institucionalizados / 
                                                   (pop_80_mais + pop_75_79 + 
                                                        pop_70_74 + pop_65_69 + 
                                                        pop_60_64))),0),
           comorbidades = round(comorbidade_menos_60 - (trab_saude * 
                                                            (comorbidade_total /
                                                                 pop_total)) - 
                                    (indigenas * (comorbidade_total / pop_total)),
                                0),
           pop_restante = round(pop_total - trab_saude - indigenas - 
                                    institucionalizados - pop_80_mais - 
                                    pop_75_79 - pop_70_74 - pop_65_69 - 
                                    pop_60_64 - comorbidades,0)) %>% 
    select(UF, municipio, trab_saude, indigenas, institucionalizados, 
           pop_80_mais, pop_75_79, pop_70_74, pop_65_69, pop_60_64, 
           comorbidades, pop_restante, pop_total)



################################################################################
###################### O R G A N I Z A Ç Ã O   D A D O S #######################
################################################################################

### Organização dos dados por município
vacinacao_quant_liquido_mun <- merge(cities_id, vacinacao_quant_liquido_mun,
                                     by.x = "city_id_6",
                                     by.y = "municipio") %>% 
    select(state_code, state_id, state_name, city_id, city_id_6, city_name,
           trab_saude, indigenas, institucionalizados, pop_80_mais, pop_75_79, 
           pop_70_74, pop_65_69, pop_60_64, comorbidades, pop_restante, pop_total)

### Consolidação em base por UF
vacinacao_quant_liquido_uf <- vacinacao_quant_liquido_mun %>% 
    group_by(state_name) %>% 
    summarise(trab_saude = sum(trab_saude), 
              indigenas = sum(indigenas),
              institucionalizados = sum(institucionalizados),  
              pop_80_mais = sum(pop_80_mais), 
              pop_75_79 = sum(pop_75_79),  
              pop_70_74 = sum(pop_70_74),  
              pop_65_69 = sum(pop_65_69),  
              pop_60_64 = sum(pop_60_64),  
              comorbidades = sum(comorbidades),  
              pop_restante = sum(pop_restante),  
              pop_total = sum(pop_total))



################################################################################
############### S A L V A   A R Q U I V O S   E M   E X C E L ##################
################################################################################

write.xlsx(vacinacao_quant_liquido_mun, "Quantitativo_vacina_liq_mun.xlsx")
write.xlsx(vacinacao_quant_liquido_uf, "Quantitativo_vacina_liq_uf.xlsx")