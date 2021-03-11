################################################################################
################################################################################
################################################################################
############ C Á L C U L O   G R U P O S   P R I O R I T Á R I O S #############
######################## V A C I N A   C O V I D - 1 9 #########################
################################################################################
################ AUTOR: Marco Brancher - IMPULSO | MARÇO 2020 ##################
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
############################# RAIS 2019 e CAGED 2020 ###########################
################################################################################
setwd("PASTA COM OS DADOS")
### Leitura dos microdados da RAIS 2019
RAIS_NORTE<-read_delim("RAIS_VINC_PUB_NORTE.txt",  locale = locale(encoding = "windows-1252"), delim = ";")%>% 
    select(`CNAE 2.0 Classe`, `Vínculo Ativo 31/12`, Município) %>% 
    mutate(cnae_2 = as.numeric(left(`CNAE 2.0 Classe`,2)),
           cnae_3 = as.numeric(left(`CNAE 2.0 Classe`,3)),
           cnae_4 = as.numeric(left(`CNAE 2.0 Classe`,4))) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) %>% 
    rename(municipio = Município)
### Leitura dos microdados da RAIS 2019
RAIS_NORDESTE<-read_delim("RAIS_VINC_PUB_NORDESTE.txt",  locale = locale(encoding = "windows-1252"), delim = ";")%>% 
    select(`CNAE 2.0 Classe`, `Vínculo Ativo 31/12`, Município) %>% 
    mutate(cnae_2 = as.numeric(left(`CNAE 2.0 Classe`,2)),
           cnae_3 = as.numeric(left(`CNAE 2.0 Classe`,3)),
           cnae_4 = as.numeric(left(`CNAE 2.0 Classe`,4))) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) %>% 
    rename(municipio = Município)
### Leitura dos microdados da RAIS 2019
RAIS_CENTRO_OESTE<-read_delim("RAIS_VINC_PUB_CENTRO_OESTE.txt",  locale = locale(encoding = "windows-1252"), delim = ";")%>%
    select(`CNAE 2.0 Classe`, `Vínculo Ativo 31/12`, Município) %>% 
    mutate(cnae_2 = as.numeric(left(`CNAE 2.0 Classe`,2)),
           cnae_3 = as.numeric(left(`CNAE 2.0 Classe`,3)),
           cnae_4 = as.numeric(left(`CNAE 2.0 Classe`,4))) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) %>% 
    rename(municipio = Município)
### Leitura dos microdados da RAIS 2019
RAIS_SUL<-read_delim("RAIS_VINC_PUB_SUL.txt",  locale = locale(encoding = "windows-1252"), delim = ";")%>% 
    select(`CNAE 2.0 Classe`, `Vínculo Ativo 31/12`, Município) %>% 
    mutate(cnae_2 = as.numeric(left(`CNAE 2.0 Classe`,2)),
           cnae_3 = as.numeric(left(`CNAE 2.0 Classe`,3)),
           cnae_4 = as.numeric(left(`CNAE 2.0 Classe`,4))) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) %>% 
    rename(municipio = Município)
### Leitura dos microdados da RAIS 2019
RAIS_MG_ES_RJ<-read_delim("RAIS_VINC_PUB_MG_ES_RJ.txt",  locale = locale(encoding = "windows-1252"), delim = ";")%>% 
    select(`CNAE 2.0 Classe`, `Vínculo Ativo 31/12`, Município) %>% 
    mutate(cnae_2 = as.numeric(left(`CNAE 2.0 Classe`,2)),
           cnae_3 = as.numeric(left(`CNAE 2.0 Classe`,3)),
           cnae_4 = as.numeric(left(`CNAE 2.0 Classe`,4))) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) %>% 
    rename(municipio = Município)
### Leitura dos microdados da RAIS 2019
RAIS_SP<-read_delim("RAIS_VINC_PUB_SP.txt",  locale = locale(encoding = "windows-1252"), delim = ";")%>% 
    select(`CNAE 2.0 Classe`, `Vínculo Ativo 31/12`, Município) %>% 
    mutate(cnae_2 = as.numeric(left(`CNAE 2.0 Classe`,2)),
           cnae_3 = as.numeric(left(`CNAE 2.0 Classe`,3)),
           cnae_4 = as.numeric(left(`CNAE 2.0 Classe`,4))) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) %>% 
    rename(municipio = Município)

### Agregação microdados RAIS
RAIS <- rbind(RAIS_NORTE, RAIS_NORDESTE, RAIS_CENTRO_OESTE, RAIS_SUL, 
              RAIS_MG_ES_RJ, RAIS_SP)

rm(RAIS_NORTE, RAIS_NORDESTE, RAIS_CENTRO_OESTE, RAIS_SUL, RAIS_MG_ES_RJ, RAIS_SP)

### Agregação microdados RAIS por município
RAIS <- RAIS %>% 
    mutate(setor = case_when(cnae_2 == 80 ~ "Segurança e Salvamento",
                             cnae_2 == 51 ~ "Transporte Aéreo",
                             cnae_2 == 50 ~ "Transporte Aquaviário",
                             cnae_3 == 492 ~ "Transporte Rodoviário Coletivo",
                             cnae_3 == 491 ~ "Transporte Ferroviário e Metroviário",
                             cnae_3 == 493 ~ "Camioneiros",
                             cnae_3 == 523 ~ "Portuários",
                             cnae_4 == 8424 ~ "Segurança e Salvamento",
                             cnae_4 == 8425 ~ "Segurança e Salvamento",
                             TRUE ~ "Indústria")) %>% 
    group_by(municipio, setor) %>% 
    summarise(estoque = sum(`Vínculo Ativo 31/12`))

### Leitura dados CAGED 2020
caged_2020_01 <- read_delim("CAGEDESTAB202001.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_02 <- read_delim("CAGEDESTAB202002.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_03 <- read_delim("CAGEDESTAB202003.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_04 <- read_delim("CAGEDESTAB202004.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_05 <- read_delim("CAGEDESTAB202005.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_06 <- read_delim("CAGEDESTAB202006.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_07 <- read_delim("CAGEDESTAB202007.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_08 <- read_delim("CAGEDESTAB202008.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_09 <- read_delim("CAGEDESTAB202009.txt", ";", escape_double = FALSE, trim_ws = TRUE)%>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_10 <- read_delim("CAGEDESTAB202010.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_11 <- read_delim("CAGEDESTAB202011.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 
caged_2020_12 <- read_delim("CAGEDESTAB202012.txt", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(município, subclasse, admitidos, desligados) %>% 
    mutate(saldo = admitidos - desligados,
           cnae_2 = as.numeric(left(subclasse,2)),
           cnae_3 = as.numeric(left(subclasse,3)),
           cnae_4 = as.numeric(left(subclasse,4))) %>% 
    rename(municipio = município, 
           cnae = subclasse) %>% 
    filter(cnae_2 %in% c(10,33) | cnae_2 == 80 | cnae_2 == 51 | cnae_2 == 50 | cnae_3 == 492 | 
               cnae_3 == 491 | cnae_3 == 493 | cnae_3 == 523 | cnae_4 == 8424 | 
               cnae_4 == 8425) 

### Agregação dados CAGED 2020
CAGED <- rbind(caged_2020_01,caged_2020_02, caged_2020_03, caged_2020_04, caged_2020_05,
               caged_2020_06, caged_2020_07, caged_2020_08, caged_2020_09, caged_2020_10, 
               caged_2020_11, caged_2020_12)

rm(caged_2020_01,caged_2020_02, caged_2020_03, caged_2020_04, caged_2020_05,
   caged_2020_06, caged_2020_07, caged_2020_08, caged_2020_09, caged_2020_10, 
   caged_2020_11, caged_2020_12)

### Agregação dados CAGED 2020 por município
CAGED <- CAGED %>% 
    mutate(cnae_2 = as.numeric(left(cnae,2)),
           cnae_3 = as.numeric(left(cnae,3)),
           cnae_4 = as.numeric(left(cnae,4)),
           setor = case_when(cnae_2 == 80 ~ "Segurança e Salvamento",
                             cnae_2 == 51 ~ "Transporte Aéreo",
                             cnae_2 == 50 ~ "Transporte Aquaviário",
                             cnae_3 == 492 ~ "Transporte Rodoviário Coletivo",
                             cnae_3 == 491 ~ "Transporte Ferroviário e Metroviário",
                             cnae_3 == 493 ~ "Camioneiros",
                             cnae_3 == 523 ~ "Portuários",
                             cnae_4 == 8424 ~ "Segurança e Salvamento",
                             cnae_4 == 8425 ~ "Segurança e Salvamento",
                             TRUE ~ "Indústria")) %>% 
    group_by(municipio,setor) %>% 
    summarise(admitidos = sum(admitidos),
              desligados = sum(desligados),
              saldo = sum(saldo)) %>% 
    select(municipio, setor, saldo)

### União RAIS e CAGED
emprego <- merge(RAIS, CAGED,
                 by = c("municipio", "setor"),
                 all = TRUE) 
rm(RAIS, CAGED)

emprego[is.na(emprego)] <- 0

### Cálculo do estoque de empregados por município
emprego <- emprego %>% 
    mutate(emprego = if_else(estoque + saldo >= 0,
                             estoque + saldo,
                             0),
           trab_seg_salv = if_else(setor == "Segurança e Salvamento", 1, 0),
           trab_tr_ferr = if_else(setor == "Transporte Ferroviário e Metroviário", 1, 0),
           trab_tr_aereo = if_else(setor == "Transporte Aéreo", 1, 0),
           trab_tr_aqua = if_else(setor == "Transporte Aquaviário", 1, 0),
           trab_tr_rod_col = if_else(setor == "Transporte Rodoviário Coletivo", 1, 0),
           trab_camioneiros = if_else(setor == "Camioneiros", 1, 0),
           trab_porto = if_else(setor == "Portuários", 1, 0),
           trab_ind = if_else(setor == "Indústria", 1, 0),
           trab_seg_salv = estoque * trab_seg_salv,
           trab_tr_ferr = estoque * trab_tr_ferr,
           trab_tr_aereo = estoque * trab_tr_aereo,
           trab_tr_aqua = estoque * trab_tr_aqua,
           trab_tr_rod_col = estoque * trab_tr_rod_col,
           trab_camioneiros = estoque * trab_camioneiros,
           trab_porto = estoque * trab_porto,
           trab_ind = estoque * trab_ind) %>% 
    group_by(municipio) %>% 
    summarise(trab_seg_salv = sum(trab_seg_salv),
              trab_tr_ferr = sum(trab_tr_ferr),
              trab_tr_aereo = sum(trab_tr_aereo),
              trab_tr_aqua = sum(trab_tr_aqua),
              trab_tr_rod_col = sum(trab_tr_rod_col),
              trab_camioneiros = sum(trab_camioneiros),
              trab_porto = sum(trab_porto),
              trab_ind = sum(trab_ind))

################################################################################
########################### Professores - INEP - 2020 ##########################
################################################################################
### Leitura dos microdados das bases de docentes do INEP
docentes_co <- read_delim("microdados_educacao_basica_2020/DADOS/docentes_co.CSV", 
                          "|", escape_double = FALSE, 
                          col_types = cols_only(
                              IN_AUTISMO = col_double(), 
                              IN_BAIXA_VISAO = col_double(), 
                              IN_CEGUEIRA = col_double(), 
                              IN_DEF_AUDITIVA = col_double(), 
                              IN_DEF_FISICA = col_double(), 
                              IN_DEF_INTELECTUAL = col_double(), 
                              IN_DEF_MULTIPLA = col_double(), 
                              IN_SUPERDOTACAO = col_double(), 
                              IN_SURDEZ = col_double(), 
                              IN_SURDOCEGUEIRA = col_double(),
                              ID_DOCENTE = col_character(),
                              TP_TIPO_ATENDIMENTO_TURMA = col_number(),
                              TP_TIPO_DOCENTE = col_number(),
                              CO_UF = col_character(),
                              CO_MUNICIPIO_END = col_double(),
                              CO_MUNICIPIO = col_double(),
                              NU_IDADE = col_number()))%>% 
    mutate(etapa_ensino = "basico",
           CO_MUNICIPIO_END = if_else(is.na(CO_MUNICIPIO_END)==TRUE, CO_MUNICIPIO, CO_MUNICIPIO_END),
           TP_TIPO_DOCENTE = if_else(is.na(TP_TIPO_DOCENTE)==TRUE,0,1,),
           IN_CEGUEIRA = if_else(is.na(IN_CEGUEIRA)==TRUE,0,1,),
           IN_DEF_AUDITIVA = if_else(is.na(IN_DEF_AUDITIVA)==TRUE,0,1,),
           IN_DEF_FISICA = if_else(is.na(IN_DEF_FISICA)==TRUE,0,1,),
           IN_DEF_MULTIPLA = if_else(is.na(IN_DEF_MULTIPLA)==TRUE,0,1,),
           IN_SURDEZ = if_else(is.na(IN_SURDEZ)==TRUE,0,1,)) %>% 
    filter(TP_TIPO_ATENDIMENTO_TURMA %in% c(1,2),
           TP_TIPO_DOCENTE %in% c(1,4),
           IN_CEGUEIRA != 1,
           IN_DEF_AUDITIVA != 1,
           IN_DEF_FISICA != 1,
           IN_DEF_MULTIPLA != 1,
           IN_SURDEZ != 1) 
docentes_co <- docentes_co[which(!duplicated(docentes_co$ID_DOCENTE)),]

### Leitura dos microdados das bases de docentes do INEP
docentes_ne <- read_delim("microdados_educacao_basica_2020/DADOS/docentes_nordeste.CSV", 
                          "|", escape_double = FALSE, 
                          col_types = cols_only(
                              IN_AUTISMO = col_double(), 
                              IN_BAIXA_VISAO = col_double(), 
                              IN_CEGUEIRA = col_double(), 
                              IN_DEF_AUDITIVA = col_double(), 
                              IN_DEF_FISICA = col_double(), 
                              IN_DEF_INTELECTUAL = col_double(), 
                              IN_DEF_MULTIPLA = col_double(), 
                              IN_SUPERDOTACAO = col_double(), 
                              IN_SURDEZ = col_double(), 
                              IN_SURDOCEGUEIRA = col_double(),
                              ID_DOCENTE = col_character(),
                              TP_TIPO_ATENDIMENTO_TURMA = col_number(),
                              TP_TIPO_DOCENTE = col_number(),
                              CO_UF = col_character(),
                              CO_MUNICIPIO_END = col_double(),
                              CO_MUNICIPIO = col_double(),
                              NU_IDADE = col_number()))%>% 
    mutate(etapa_ensino = "basico",
           CO_MUNICIPIO_END = if_else(is.na(CO_MUNICIPIO_END)==TRUE, CO_MUNICIPIO, CO_MUNICIPIO_END),
           TP_TIPO_DOCENTE = if_else(is.na(TP_TIPO_DOCENTE)==TRUE,0,1,),
           IN_CEGUEIRA = if_else(is.na(IN_CEGUEIRA)==TRUE,0,1,),
           IN_DEF_AUDITIVA = if_else(is.na(IN_DEF_AUDITIVA)==TRUE,0,1,),
           IN_DEF_FISICA = if_else(is.na(IN_DEF_FISICA)==TRUE,0,1,),
           IN_DEF_MULTIPLA = if_else(is.na(IN_DEF_MULTIPLA)==TRUE,0,1,),
           IN_SURDEZ = if_else(is.na(IN_SURDEZ)==TRUE,0,1,)) %>% 
    filter(TP_TIPO_ATENDIMENTO_TURMA %in% c(1,2),
           TP_TIPO_DOCENTE %in% c(1,4),
           IN_CEGUEIRA != 1,
           IN_DEF_AUDITIVA != 1,
           IN_DEF_FISICA != 1,
           IN_DEF_MULTIPLA != 1,
           IN_SURDEZ != 1) 
docentes_ne <- docentes_ne[which(!duplicated(docentes_ne$ID_DOCENTE)),]

### Leitura dos microdados das bases de docentes do INEP
docentes_no <- read_delim("microdados_educacao_basica_2020/DADOS/docentes_norte.CSV", 
                          "|", escape_double = FALSE, 
                          col_types = cols_only(
                              IN_AUTISMO = col_double(), 
                              IN_BAIXA_VISAO = col_double(), 
                              IN_CEGUEIRA = col_double(), 
                              IN_DEF_AUDITIVA = col_double(), 
                              IN_DEF_FISICA = col_double(), 
                              IN_DEF_INTELECTUAL = col_double(), 
                              IN_DEF_MULTIPLA = col_double(), 
                              IN_SUPERDOTACAO = col_double(), 
                              IN_SURDEZ = col_double(), 
                              IN_SURDOCEGUEIRA = col_double(),
                              ID_DOCENTE = col_character(),
                              TP_TIPO_ATENDIMENTO_TURMA = col_number(),
                              TP_TIPO_DOCENTE = col_number(),
                              CO_UF = col_character(),
                              CO_MUNICIPIO_END = col_double(),
                              CO_MUNICIPIO = col_double(),
                              NU_IDADE = col_number()))%>% 
    mutate(etapa_ensino = "basico",
           CO_MUNICIPIO_END = if_else(is.na(CO_MUNICIPIO_END)==TRUE, CO_MUNICIPIO, CO_MUNICIPIO_END),
           TP_TIPO_DOCENTE = if_else(is.na(TP_TIPO_DOCENTE)==TRUE,0,1,),
           IN_CEGUEIRA = if_else(is.na(IN_CEGUEIRA)==TRUE,0,1,),
           IN_DEF_AUDITIVA = if_else(is.na(IN_DEF_AUDITIVA)==TRUE,0,1,),
           IN_DEF_FISICA = if_else(is.na(IN_DEF_FISICA)==TRUE,0,1,),
           IN_DEF_MULTIPLA = if_else(is.na(IN_DEF_MULTIPLA)==TRUE,0,1,),
           IN_SURDEZ = if_else(is.na(IN_SURDEZ)==TRUE,0,1,)) %>% 
    filter(TP_TIPO_ATENDIMENTO_TURMA %in% c(1,2),
           TP_TIPO_DOCENTE %in% c(1,4),
           IN_CEGUEIRA != 1,
           IN_DEF_AUDITIVA != 1,
           IN_DEF_FISICA != 1,
           IN_DEF_MULTIPLA != 1,
           IN_SURDEZ != 1) 
docentes_no <- docentes_no[which(!duplicated(docentes_no$ID_DOCENTE)),]

### Leitura dos microdados das bases de docentes do INEP
docentes_se <- read_delim("microdados_educacao_basica_2020/DADOS/docentes_sudeste.CSV", 
                          "|", escape_double = FALSE, 
                          col_types = cols_only(
                              IN_AUTISMO = col_double(), 
                              IN_BAIXA_VISAO = col_double(), 
                              IN_CEGUEIRA = col_double(), 
                              IN_DEF_AUDITIVA = col_double(), 
                              IN_DEF_FISICA = col_double(), 
                              IN_DEF_INTELECTUAL = col_double(), 
                              IN_DEF_MULTIPLA = col_double(), 
                              IN_SUPERDOTACAO = col_double(), 
                              IN_SURDEZ = col_double(), 
                              IN_SURDOCEGUEIRA = col_double(),
                              ID_DOCENTE = col_character(),
                              TP_TIPO_ATENDIMENTO_TURMA = col_number(),
                              TP_TIPO_DOCENTE = col_number(),
                              CO_UF = col_character(),
                              CO_MUNICIPIO_END = col_double(),
                              CO_MUNICIPIO = col_double(),
                              NU_IDADE = col_number()))%>% 
    mutate(etapa_ensino = "basico",
           CO_MUNICIPIO_END = if_else(is.na(CO_MUNICIPIO_END)==TRUE, CO_MUNICIPIO, CO_MUNICIPIO_END),
           TP_TIPO_DOCENTE = if_else(is.na(TP_TIPO_DOCENTE)==TRUE,0,1,),
           IN_CEGUEIRA = if_else(is.na(IN_CEGUEIRA)==TRUE,0,1,),
           IN_DEF_AUDITIVA = if_else(is.na(IN_DEF_AUDITIVA)==TRUE,0,1,),
           IN_DEF_FISICA = if_else(is.na(IN_DEF_FISICA)==TRUE,0,1,),
           IN_DEF_MULTIPLA = if_else(is.na(IN_DEF_MULTIPLA)==TRUE,0,1,),
           IN_SURDEZ = if_else(is.na(IN_SURDEZ)==TRUE,0,1,)) %>% 
    filter(TP_TIPO_ATENDIMENTO_TURMA %in% c(1,2),
           TP_TIPO_DOCENTE %in% c(1,4),
           IN_CEGUEIRA != 1,
           IN_DEF_AUDITIVA != 1,
           IN_DEF_FISICA != 1,
           IN_DEF_MULTIPLA != 1,
           IN_SURDEZ != 1) 
docentes_se <- docentes_se[which(!duplicated(docentes_se$ID_DOCENTE)),]

### Leitura dos microdados das bases de docentes do INEP
docentes_su <- read_delim("microdados_educacao_basica_2020/DADOS/docentes_sul.CSV", 
                          "|", escape_double = FALSE, 
                          col_types = cols_only(
                              IN_AUTISMO = col_double(), 
                              IN_BAIXA_VISAO = col_double(), 
                              IN_CEGUEIRA = col_double(), 
                              IN_DEF_AUDITIVA = col_double(), 
                              IN_DEF_FISICA = col_double(), 
                              IN_DEF_INTELECTUAL = col_double(), 
                              IN_DEF_MULTIPLA = col_double(), 
                              IN_SUPERDOTACAO = col_double(), 
                              IN_SURDEZ = col_double(), 
                              IN_SURDOCEGUEIRA = col_double(),
                              ID_DOCENTE = col_character(),
                              TP_TIPO_ATENDIMENTO_TURMA = col_number(),
                              TP_TIPO_DOCENTE = col_number(),
                              CO_UF = col_character(),
                              CO_MUNICIPIO_END = col_double(),
                              CO_MUNICIPIO = col_double(),
                              NU_IDADE = col_number()))%>% 
    mutate(etapa_ensino = "basico",
           CO_MUNICIPIO_END = if_else(is.na(CO_MUNICIPIO_END)==TRUE, CO_MUNICIPIO, CO_MUNICIPIO_END),
           TP_TIPO_DOCENTE = if_else(is.na(TP_TIPO_DOCENTE)==TRUE,0,1,),
           IN_CEGUEIRA = if_else(is.na(IN_CEGUEIRA)==TRUE,0,1,),
           IN_DEF_AUDITIVA = if_else(is.na(IN_DEF_AUDITIVA)==TRUE,0,1,),
           IN_DEF_FISICA = if_else(is.na(IN_DEF_FISICA)==TRUE,0,1,),
           IN_DEF_MULTIPLA = if_else(is.na(IN_DEF_MULTIPLA)==TRUE,0,1,),
           IN_SURDEZ = if_else(is.na(IN_SURDEZ)==TRUE,0,1,)) %>% 
    filter(TP_TIPO_ATENDIMENTO_TURMA %in% c(1,2),
           TP_TIPO_DOCENTE %in% c(1,4),
           IN_CEGUEIRA != 1,
           IN_DEF_AUDITIVA != 1,
           IN_DEF_FISICA != 1,
           IN_DEF_MULTIPLA != 1,
           IN_SURDEZ != 1) 
docentes_su <- docentes_su[which(!duplicated(docentes_su$ID_DOCENTE)),]

### Leitura dos microdados das bases de docentes do ensino superior do INEP
docente_superior_original <- read_delim("microdados_educacao_superior_2019/dados/SUP_DOCENTE_2019.CSV", 
                                        "|", escape_double = FALSE, 
                                        col_types = cols(IN_DEFICIENCIA_AUDITIVA = col_double(), 
                                                         IN_DEFICIENCIA_BAIXA_VISAO = col_double(), 
                                                         IN_DEFICIENCIA_CEGUEIRA = col_double(), 
                                                         IN_DEFICIENCIA_FISICA = col_double(), 
                                                         IN_DEFICIENCIA_INTELECTUAL = col_character(), 
                                                         IN_DEFICIENCIA_MULTIPLA = col_double(), 
                                                         IN_DEFICIENCIA_SURDEZ = col_double(), 
                                                         IN_DEFICIENCIA_SURDOCEGUEIRA = col_double()), 
                                        trim_ws = TRUE) %>% 
    mutate(etapa_ensino = "superior",
           IN_DEFICIENCIA_AUDITIVA = if_else(is.na(IN_DEFICIENCIA_AUDITIVA)==TRUE,0,1,),
           IN_DEFICIENCIA_CEGUEIRA = if_else(is.na(IN_DEFICIENCIA_CEGUEIRA)==TRUE,0,1,),
           IN_DEFICIENCIA_FISICA = if_else(is.na(IN_DEFICIENCIA_FISICA)==TRUE,0,1,),
           IN_DEFICIENCIA_MULTIPLA = if_else(is.na(IN_DEFICIENCIA_MULTIPLA)==TRUE,0,1,),
           IN_DEFICIENCIA_SURDEZ = if_else(is.na(IN_DEFICIENCIA_SURDEZ)==TRUE,0,1,),
           IN_DEFICIENCIA_SURDOCEGUEIRA = if_else(is.na(IN_DEFICIENCIA_SURDOCEGUEIRA)==TRUE,0,1,)) %>% 
    filter(TP_SITUACAO == 1, 
           IN_ATUACAO_EAD != 1,
           IN_DEFICIENCIA_AUDITIVA != 1,
           IN_DEFICIENCIA_CEGUEIRA != 1,
           IN_DEFICIENCIA_FISICA != 1,
           IN_DEFICIENCIA_MULTIPLA != 1,
           IN_DEFICIENCIA_SURDEZ != 1,
           IN_DEFICIENCIA_SURDOCEGUEIRA != 1) %>% 
    select(ID_DOCENTE, NU_IDADE, etapa_ensino, CO_IES)
docente_superior_original <- docente_superior_original[which(!duplicated(docente_superior_original$ID_DOCENTE)),]

### Leitura dos microdados da bases de instituições de ensino superior do INEP
IES <- read_delim("microdados_educacao_superior_2019/dados/SUP_IES_2019.CSV", 
                  "|", escape_double = FALSE, trim_ws = TRUE) %>% 
    select(CO_IES, CO_MUNICIPIO)

### União das bases de docentes do ensino superior e instituiçoes de ensino superior
docente_superior <- merge(docente_superior_original, IES, by = "CO_IES") %>% 
    select(ID_DOCENTE, NU_IDADE, etapa_ensino, CO_MUNICIPIO) %>% 
    rename(CO_MUNICIPIO_END = CO_MUNICIPIO)

rm(docente_superior_original, IES)

### União das bases de docentes da educação básica
docentes_original <- rbind(docentes_co, docentes_ne, docentes_no, docentes_se, docentes_su) 

rm(docentes_co, docentes_ne, docentes_no, docentes_se, docentes_su)

docentes_original <- docentes_original %>% 
    select(ID_DOCENTE, NU_IDADE, CO_MUNICIPIO_END, etapa_ensino)

### União das bases de docentes da educação básica e educação superior
docentes_mun <- merge(docentes_original, docente_superior, by = "ID_DOCENTE", all = TRUE)

rm(docentes_original, docente_superior)

docentes_mun <- docentes_mun[which(!duplicated(docentes_mun$ID_DOCENTE)),]

### Criação de variáveis de idade e tipo de ensino
docentes_mun <- docentes_mun %>% 
    mutate(etapa_ensino = if_else(is.na(etapa_ensino.x) == FALSE,
                                  etapa_ensino.x,
                                  etapa_ensino.y),
           municipio = if_else(is.na(CO_MUNICIPIO_END.x) == FALSE,
                               CO_MUNICIPIO_END.x,
                               CO_MUNICIPIO_END.y),
           NU_IDADE = if_else(is.na(NU_IDADE.x) == FALSE,
                              NU_IDADE.x,
                              NU_IDADE.y),
           faixa_etaria = case_when(NU_IDADE < 60 ~ "60 -",
                                    NU_IDADE >= 60 & NU_IDADE <= 64 ~ "60 - 64",
                                    NU_IDADE >= 65 & NU_IDADE <= 69 ~ "65 - 69",
                                    NU_IDADE >= 70 & NU_IDADE <= 74 ~ "70 - 74",
                                    NU_IDADE >= 75 & NU_IDADE <= 79 ~ "75 - 79",
                                    NU_IDADE >= 80 & NU_IDADE <= 84 ~ "80 - 84",
                                    NU_IDADE >= 75 & NU_IDADE <= 79 ~ "85 - 89",
                                    TRUE ~ "90 +"),
           idade_60_menos = if_else(faixa_etaria == "60 -",
                                    1,
                                    0),
           idade_60_64 = if_else(faixa_etaria == "60 - 64",
                                 1,
                                 0),
           idade_65_69 = if_else(faixa_etaria == "65 - 69",
                                 1,
                                 0),
           idade_70_74 = if_else(faixa_etaria == "70 - 74", 
                                 1,
                                 0),
           idade_75_79 = if_else(faixa_etaria == "75 - 79",
                                 1,
                                 0),
           idade_80_84 = if_else(faixa_etaria == "80 - 84",
                                 1,
                                 0),
           idade_85_89 = if_else(faixa_etaria == "85 - 89",
                                 1,
                                 0),
           idade_90_mais = if_else(faixa_etaria == "90 +",
                                   1,
                                   0),
           ensino_basico = if_else(etapa_ensino == "basico", 1, 0),
           ensino_superior = if_else(etapa_ensino == "superior", 1, 0),
           basico_60_menos = ensino_basico * idade_60_menos,
           basico_60_64 = ensino_basico * idade_60_64,
           basico_65_69 = ensino_basico * idade_65_69,
           basico_70_74 = ensino_basico * idade_70_74,
           basico_75_79 = ensino_basico * idade_75_79,
           basico_80_84 = ensino_basico * idade_80_84,
           basico_85_89 = ensino_basico * idade_85_89,
           basico_90_mais = ensino_basico * idade_90_mais,
           superior_60_menos = ensino_superior * idade_60_menos,
           superior_60_64 = ensino_superior * idade_60_64,
           superior_65_69 = ensino_superior * idade_65_69,
           superior_70_74 = ensino_superior * idade_70_74,
           superior_75_79 = ensino_superior * idade_75_79,
           superior_80_84 = ensino_superior * idade_80_84,
           superior_85_89 = ensino_superior * idade_85_89,
           superior_90_mais = ensino_superior * idade_90_mais) %>%
    select(ID_DOCENTE, municipio, basico_60_menos, basico_60_64, basico_65_69,
           basico_70_74, basico_75_79, basico_80_84, basico_85_89, basico_90_mais,
           superior_60_menos, superior_60_64, superior_65_69, superior_70_74,
           superior_75_79, superior_80_84, superior_85_89, superior_90_mais)

###Agregação por município
docentes_mun <- docentes_mun %>% 
    group_by(municipio) %>% 
    summarise(basico_60_menos = sum(basico_60_menos),
              basico_60_64 = sum(basico_60_64),
              basico_65_69 = sum(basico_65_69),
              basico_70_74 = sum(basico_70_74),
              basico_75_79 = sum(basico_75_79),
              basico_80_84 = sum(basico_80_84),
              basico_85_89 = sum(basico_85_89),
              basico_90_mais = sum(basico_90_mais),
              superior_60_menos = sum(superior_60_menos),
              superior_60_64 = sum(superior_60_64),
              superior_65_69 = sum(superior_65_69),
              superior_70_74 = sum(superior_70_74),
              superior_75_79 = sum(superior_75_79),
              superior_80_84 = sum(superior_80_84),
              superior_85_89 = sum(superior_85_89),
              superior_90_mais = sum(superior_90_mais)) %>% 
    mutate(municipio = as.numeric(left(municipio, 6)))

docentes_uf <- docentes_mun %>% 
    mutate(UF = left(municipio,2)) %>% 
    group_by(UF) %>% 
    summarise(basico_60_menos = sum(basico_60_menos),
              superior_60_menos = sum(superior_60_menos))

write.xlsx(docentes_mun,"docentes_mun.xlsx")

#rm(docentes_mun)

################################################################################
################ Outros trabalhadores da educação - INEP - 2020 ################
################################################################################
### Leitura dos microdados de escolas da educação básica
escolas_original <- read_delim("microdados_educacao_basica_2020/DADOS/escolas.CSV", 
                               "|", escape_double = FALSE, trim_ws = TRUE) 

### Criação de variáveis para realização de filtros e agregação por município
escolas_mun <- escolas_original %>% 
    select(CO_UF, CO_MUNICIPIO, QT_PROF_ADMINISTRATIVOS, QT_PROF_SERVICOS_GERAIS,
           QT_PROF_BIBLIOTECARIO, QT_PROF_SAUDE, QT_PROF_COORDENADOR,
           QT_PROF_FONAUDIOLOGO, QT_PROF_NUTRICIONISTA, QT_PROF_PSICOLOGO,
           QT_PROF_ALIMENTACAO, QT_PROF_PEDAGOGIA, QT_PROF_SECRETARIO,
           QT_PROF_SEGURANCA, QT_PROF_MONITORES, QT_PROF_GESTAO, 
           QT_PROF_ASSIST_SOCIAL) %>% 
    mutate(QT_PROF_ADMINISTRATIVOS = if_else(QT_PROF_ADMINISTRATIVOS == 88888,
                                             0,
                                             QT_PROF_ADMINISTRATIVOS),
           QT_PROF_SERVICOS_GERAIS = if_else(QT_PROF_SERVICOS_GERAIS == 88888,
                                             0,
                                             QT_PROF_SERVICOS_GERAIS),
           QT_PROF_BIBLIOTECARIO = if_else(QT_PROF_BIBLIOTECARIO == 88888,
                                           0,
                                           QT_PROF_BIBLIOTECARIO),
           QT_PROF_SAUDE = if_else(QT_PROF_SAUDE == 88888,
                                   0,
                                   QT_PROF_SAUDE),
           QT_PROF_COORDENADOR = if_else(QT_PROF_COORDENADOR == 88888,
                                         0,
                                         QT_PROF_COORDENADOR),
           QT_PROF_FONAUDIOLOGO = if_else(QT_PROF_FONAUDIOLOGO == 88888,
                                          0,
                                          QT_PROF_FONAUDIOLOGO),
           QT_PROF_NUTRICIONISTA = if_else(QT_PROF_NUTRICIONISTA == 88888,
                                           0,
                                           QT_PROF_NUTRICIONISTA),
           QT_PROF_PSICOLOGO = if_else(QT_PROF_PSICOLOGO == 88888,
                                       0,
                                       QT_PROF_PSICOLOGO),
           QT_PROF_ALIMENTACAO = if_else(QT_PROF_ALIMENTACAO == 88888,
                                         0,
                                         QT_PROF_ALIMENTACAO),
           QT_PROF_PEDAGOGIA = if_else(QT_PROF_PEDAGOGIA == 88888,
                                       0,
                                       QT_PROF_PEDAGOGIA),
           QT_PROF_SECRETARIO = if_else(QT_PROF_SECRETARIO == 88888,
                                        0,
                                        QT_PROF_SECRETARIO),
           QT_PROF_SEGURANCA = if_else(QT_PROF_SEGURANCA == 88888,
                                       0,
                                       QT_PROF_SEGURANCA),
           QT_PROF_MONITORES = if_else(QT_PROF_MONITORES == 88888,
                                       0,
                                       QT_PROF_MONITORES),
           QT_PROF_GESTAO = if_else(QT_PROF_GESTAO == 88888,
                                    0,
                                    QT_PROF_GESTAO),
           QT_PROF_ASSIST_SOCIAL = if_else(QT_PROF_ASSIST_SOCIAL == 88888,
                                           0,
                                           QT_PROF_ASSIST_SOCIAL),
           trab_educ_basica = QT_PROF_ADMINISTRATIVOS + QT_PROF_SERVICOS_GERAIS +
               QT_PROF_BIBLIOTECARIO + QT_PROF_SAUDE + QT_PROF_COORDENADOR +
               QT_PROF_FONAUDIOLOGO + QT_PROF_NUTRICIONISTA + QT_PROF_PSICOLOGO +
               QT_PROF_ALIMENTACAO + QT_PROF_PEDAGOGIA + QT_PROF_SECRETARIO +
               QT_PROF_SEGURANCA + QT_PROF_MONITORES + QT_PROF_GESTAO + 
               QT_PROF_ASSIST_SOCIAL) %>% 
    select(CO_UF, CO_MUNICIPIO, trab_educ_basica) %>% 
    group_by(CO_MUNICIPIO) %>% 
    summarise(trab_educ_basica = sum(trab_educ_basica, na.rm = TRUE ))

rm(escolas_original)

### Leitura dos microdados das universidades
universidade_original <- read_delim("microdados_educacao_superior_2019/dados/SUP_IES_2019.CSV", 
                                    "|", escape_double = FALSE, trim_ws = TRUE) 

### Criação de variáveis para realização de filtros
universidade_mun <- universidade_original %>% 
    select(CO_UF, CO_MUNICIPIO, QT_TEC_TOTAL) %>% 
    group_by(CO_MUNICIPIO) %>% 
    summarise(trab_educ_sup = sum(QT_TEC_TOTAL, na.rm = TRUE ))

rm(universidade_original)

### União das bases de escolas e de universidades
trab_educ_mun <- merge(escolas_mun, universidade_mun,
                       by.x = "CO_MUNICIPIO",
                       by.y = "CO_MUNICIPIO",
                       all.x = TRUE,
                       all.y = TRUE)

rm(escolas_mun, universidade_mun)

trab_educ_mun <- trab_educ_mun %>% 
    mutate(trab_educ_basica = if_else(is.na(trab_educ_basica) == TRUE,
                                      0,
                                      trab_educ_basica),
           trab_educ_sup = if_else(is.na(trab_educ_sup) == TRUE,
                                   0,
                                   trab_educ_sup),
           UF = left(CO_MUNICIPIO, 2))

write.xlsx(trab_educ_mun,"trab_educ_mun.xlsx")  

################################################################################
############### variação população 2010-2020 - Min. Saúde - 2020 ###############
################################################################################
setwd("/Volumes/GoogleDrive/Shared drives/Projetos e pesquisas/13. Ferramenta Vacinação")

var_pop <- read_delim("variacao_pop_2010_2020.csv", 
                      ";", escape_double = FALSE, 
                      col_types = cols(municipio = col_number(), 
                                       pop_2010 = col_number(), 
                                       pop_2020 = col_number()), 
                      trim_ws = TRUE)

var_pop <- var_pop %>% 
    mutate(var = pop_2020 / pop_2010)

################################################################################
######## população acima de 80 anos detalhada por UF - Min. Saúde - 2020 #######
################################################################################
pop_uf_detalhe <- read_excel("pop_faixas_etarias_uf_detalhe.xlsx", 
                             sheet = "UF") %>% 
    mutate(prop_80_84 = pop_80_84 / (pop_80_84 + pop_85_89 + pop_90_mais),
           prop_85_89 = pop_85_89 / (pop_80_84 + pop_85_89 + pop_90_mais),
           prop_90_mais = pop_90_mais / (pop_80_84 + pop_85_89 + pop_90_mais)) %>% 
    select(UF, prop_80_84, prop_85_89, prop_90_mais)

################################################################################
######## população com deficiência grave entre 18 e 60 anos - CENSO 2010 #######
################################################################################
deficientes_2010 <- read_excel("deficientes_2010.xlsx", 
                               col_types = c("text", "numeric")) 
cities_id_2 <- cities_id %>% 
    mutate(city_name = paste0(city_name," (", state_id, ")"))

deficientes_2010 <- merge(cities_id_2, deficientes_2010,
                          by.x = "city_name",
                          by.y = "municipio") %>% 
    select(city_id_6, def_18_59) %>% 
    rename(municipio = city_id_6) %>% 
    group_by(municipio) %>% 
    summarise(def_18_59 = sum(def_18_59))

deficientes_2020 <- merge(deficientes_2010, var_pop,
                          by = "municipio")

rm(deficientes_2010)

deficientes_2020 <- deficientes_2020 %>% 
    mutate(deficientes = round(def_18_59 * var,0)) %>% 
    select(municipio, deficientes) %>% 
    mutate(municipio = as.numeric(municipio))

deficientes_2020[is.na(deficientes_2020)] <- 0

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

pop_mun_2020 <- merge(pop_mun_2020, pop_uf_detalhe,
                      by = "UF")

pop_mun_2020 <- pop_mun_2020 %>% 
    mutate(`80_84` = round(`>80` * prop_80_84,0),
           `85_89` = round(`>80` * prop_85_89,0),
           `>90` = round(`>80` * prop_90_mais,0)) %>% 
    select(UF, municipio,`0_4`, `5_9`, `10_14`, `15_19`, `20_24`, `25_29`, `30_34`, 
           `35_39`, `40_44`, `45_49`, `50_54`, `55_59`, `60_64`, `65_69`, `70_74`,
           `75_79`, `80_84`, `85_89`, `>90`)

################################################################################
############## ribeirinhos, quilombolas e militares - PNI - 2020 ###############
################################################################################
dados_MS_original <- read_excel("Estimativas_PNI.xlsx", 
                                col_types = c("numeric", "numeric", "text", 
                                              "text", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"),
                                sheet = "MUN", skip = 1) 

colnames(dados_MS_original) <- c("UF", "municipio", "city_name", "state_name", 
                                 "indigenas_MS", "trab_saude_MS", "pop_80_mais_MS", 
                                 "pop_trad_MS", "quilombolas_MS", "pop_75_79_MS", 
                                 "pop_70_74_MS", "pop_65_69_MS", "pop_60_64_MS", 
                                 "pop_def_MS", "pop_priv_liberdade_MS", 
                                 "militares_MS", "trab_educ_basica_MS", 
                                 "trab_educ_superior_MS", "total_MS")

dados_MS <- dados_MS_original %>% 
    select(municipio, pop_trad_MS, quilombolas_MS, militares_MS) %>% 
    mutate(pop_trad_MS = round(pop_trad_MS,0),
           quilombolas_MS = round(quilombolas_MS,0),
           militares_MS = round(militares_MS,0))

dados_MS[is.na(dados_MS)] <- 0

rm(dados_MS_original)

################################################################################
################## população em situação de rua - PNI - 2020 ###################
################################################################################
dados_MS_UF_original <- read_excel("Estimativas_PNI.xlsx", 
                                col_types = c("numeric", "text", "numeric",
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric",
                                              "numeric"),
                                sheet = "UF", skip = 1) 

colnames(dados_MS_UF_original) <- c("UF",  "state_name", "pop_idosa_inst", 
                                    "pop_def_inst", "indigenas_MS", "trab_saude_MS", 
                                    "pop_80_mais_MS", "pop_trad_MS", "quilombolas_MS",
                                    "pop_75_79_MS", "pop_70_74_MS", "pop_65_69_MS", 
                                    "pop_60_64_MS", "comorbidades_MS", 
                                    "pop_priv_liberdade_MS", "pop_sit_rua_MS", 
                                    "forca_seguranca_MS", "militares_MS", "pop_def_MS", 
                                    "trab_educ_basica_MS", "trab_educ_superior_MS", 
                                    "camioneiros_MS", "transp_rod_MS", "transp_ferr_MS",
                                    "transp_aereo_MS", "portuarios_MS", "transp_aquav_MS",
                                    "trab_ind_MS", "total_MS")

dados_MS_UF <- dados_MS_UF_original %>% 
    select(UF, pop_sit_rua_MS) %>% 
    mutate(pop_sit_rua_MS = round(pop_sit_rua_MS,0))

rm(dados_MS_UF_original)

pop_2020 <- merge(pop_mun_2020, pop_uf_2020,
                     by = "UF") %>% 
    mutate(total_2020 = `0_4` + `5_9` + `10_14` + `15_19` + `20_24` + `25_29` +
               `30_34` + `35_39` + `40_44` + `45_49` + `50_54` + `55_59` + 
               `60_64` + `65_69` + `70_74` +`75_79` + `80_84` + `85_89` + `>90`) %>% 
    select(UF, municipio, total_2020, populacao_2020) %>% 
    mutate(share_mun = total_2020 / populacao_2020) %>% 
    select(UF, municipio, share_mun)

pop_sit_rua <- merge(pop_2020, dados_MS_UF,
                     by = "UF") %>% 
    mutate(pop_sit_rua = round(share_mun * pop_sit_rua_MS,0)) %>% 
    select(municipio, pop_sit_rua)


################################################################################
########## trab.saúde, indígenas e priv. liberdade- INFLUENZA - 2020 ###########
################################################################################
dados_LAI_original <- read_excel("campanha_influenza_2020.xlsx", 
                                col_types = c("numeric", "numeric", "text", 
                                              "text", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric"),
                                sheet = "MUN", skip = 1) 

colnames(dados_LAI_original) <- c("UF", "municipio", "city_name", "state_name", 
                                 "pop_0_2", "pop_2_4", "pop_5_5", "pop_gestante", 
                                 "pop_puerpera", "pop_55_59", "pop_60_64",
                                 "pop_65_69", "pop_70_74", "pop_75_79", 
                                 "pop_80_mais", "trab_saude_MS", "indigenas_MS", 
                                 "comorbidades_MS", "pop_priv_liberdade_MS",
                                 "trab_sist_prisional_MS", "professores_MS", 
                                 "forca_seguranca_MS", "total_MS")

dados_LAI <- dados_LAI_original %>% 
    select(municipio, indigenas_MS, trab_saude_MS, pop_priv_liberdade_MS) %>% 
    mutate(indigenas_MS = round(indigenas_MS,0),
           trab_saude_MS = round(trab_saude_MS,0),
           pop_priv_liberdade_MS = round(pop_priv_liberdade_MS,0))

dados_LAI[is.na(dados_LAI)] <- 0

rm(dados_LAI_original)

################################################################################
################### população institucionalizada - SUAS - 2019 #################
################################################################################
censo_SUAS_original <- read_excel("censo_SUAS.xlsx") 

### Seleciona variáveis de interesse e calcula número de pessoas acima de 60 anos 
### institucionalizadas e o número de portadores de deficiência entre 18 e 59 anos
censo_SUAS_2019 <- censo_SUAS_original %>% 
    select(IBGE, q1_1, q1_2, q15_1_7, q15_1_8, q15_1_9, q15_1_10, q15_2_7, 
           q15_2_8, q15_2_9, q15_2_10) %>% 
    mutate(UF = left(IBGE,2),
           idosos = q15_1_9 + q15_1_10 + q15_2_9 + q15_2_10, #pessoas acima de 60
           deficientes = if_else(q1_1 == "Exclusivamente pessoas adultas com Deficiência",
                                 q15_1_7 + q15_1_8 + q15_2_7 + q15_2_8,
                                 0)) %>% #deficientes entre 18 e 59
    group_by(IBGE) %>% 
    summarise(idosos_instit = sum(idosos)*2, #multiplicamos por 2 para incluir estabelecimentos não credenciados
              deficientes_instit = sum(deficientes)*2) %>% 
    mutate(IBGE = as.numeric(IBGE))

colnames(censo_SUAS_2019) <- c("municipio", "idosos_inst", "deficientes_inst")

censo_SUAS_2019[is.na(censo_SUAS_2019)] <- 0

rm(censo_SUAS_original)

################################################################################
############################### PNAD 2019 - IBGE ###############################
################################################################################
PNAD_2019_original <- read_pnadc("/Volumes/GoogleDrive/Shared drives/Projetos e pesquisas/06. Rio Grande do Sul/01. GT | Atividade Econômica/Impacto RME - PNAD/PNADC_2019_visita1.txt",
                                 input= "/Volumes/GoogleDrive/Shared drives/Projetos e pesquisas/06. Rio Grande do Sul/01. GT | Atividade Econômica/Impacto RME - PNAD/input_PNADC_2019_visita1.txt")

### Cálculo do número de profissionais de saúde por UF e faixa etária a partir 
### dos códigos de ocupação selecionados
PNAD_2019 <- PNAD_2019_original %>% 
    select(UF, V4010, V4013, V1032, V2009) %>% 
    filter(is.na(V4010) == FALSE) %>% 
    filter(is.na(V4013) == FALSE) %>% 
    mutate(cnae = case_when(V4013 == 10010 ~ "Indústria",
                            V4013 == 10021 ~ "Indústria",
                            V4013 == 10022 ~ "Indústria",
                            V4013 == 10030 ~ "Indústria",
                            V4013 == 10091 ~ "Indústria",
                            V4013 == 10092 ~ "Indústria",
                            V4013 == 10093 ~ "Indústria",
                            V4013 == 10099 ~ "Indústria",
                            V4013 == 11000 ~ "Indústria",
                            V4013 == 12000 ~ "Indústria",
                            V4013 == 13001 ~ "Indústria",
                            V4013 == 13002 ~ "Indústria",
                            V4013 == 14001 ~ "Indústria",
                            V4013 == 14002 ~ "Indústria",
                            V4013 == 15011 ~ "Indústria",
                            V4013 == 15012 ~ "Indústria",
                            V4013 == 15020 ~ "Indústria",
                            V4013 == 16001 ~ "Indústria",
                            V4013 == 16002 ~ "Indústria",
                            V4013 == 17001 ~ "Indústria",
                            V4013 == 17002 ~ "Indústria",
                            V4013 == 18000 ~ "Indústria",
                            V4013 == 19010 ~ "Indústria",
                            V4013 == 19020 ~ "Indústria",
                            V4013 == 19030 ~ "Indústria",
                            V4013 == 20010 ~ "Indústria",
                            V4013 == 20020 ~ "Indústria",
                            V4013 == 20090 ~ "Indústria",
                            V4013 == 21000 ~ "Indústria",
                            V4013 == 22010 ~ "Indústria",
                            V4013 == 22020 ~ "Indústria",
                            V4013 == 23010 ~ "Indústria",
                            V4013 == 23091 ~ "Indústria",
                            V4013 == 23099 ~ "Indústria",
                            V4013 == 24001 ~ "Indústria",
                            V4013 == 24002 ~ "Indústria",
                            V4013 == 24003 ~ "Indústria",
                            V4013 == 25001 ~ "Indústria",
                            V4013 == 25002 ~ "Indústria",
                            V4013 == 26010 ~ "Indústria",
                            V4013 == 26020 ~ "Indústria",
                            V4013 == 26030 ~ "Indústria",
                            V4013 == 26041 ~ "Indústria",
                            V4013 == 26042 ~ "Indústria",
                            V4013 == 27010 ~ "Indústria",
                            V4013 == 27090 ~ "Indústria",
                            V4013 == 28000 ~ "Indústria",
                            V4013 == 29001 ~ "Indústria",
                            V4013 == 29002 ~ "Indústria",
                            V4013 == 29003 ~ "Indústria",
                            V4013 == 30010 ~ "Indústria",
                            V4013 == 30020 ~ "Indústria",
                            V4013 == 30030 ~ "Indústria",
                            V4013 == 30090 ~ "Indústria",
                            V4013 == 31000 ~ "Indústria",
                            V4013 == 32001 ~ "Indústria",
                            V4013 == 32002 ~ "Indústria",
                            V4013 == 32003 ~ "Indústria",
                            V4013 == 32009 ~ "Indústria",
                            V4013 == 33001 ~ "Indústria",
                            V4013 == 33002 ~ "Indústria",
                            V4013 == 49010 ~ "Transporte ferroviário e metroferroviário",
                            V4013 == 49030 ~ "Transporte rodoviário de passageiros",
                            V4013 == 49040 ~ "Transporte rodoviário de carga",
                            V4013 == 49090 ~ "Transporte rodoviário de passageiros",
                            V4013 == 50000 ~ "Transporte Aquaviário",
                            V4013 == 51000 ~ "Transporte Aéreo",
                            V4013 == 52010 ~ "Transporte rodoviário de carga",
                            V4013 == 52020 ~ "Transporte rodoviário de carga",
                            V4013 == 53001 ~ "Transporte rodoviário de carga",
                            V4013 == 53002 ~ "Transporte rodoviário de carga",
                            V4013 == 80000 ~ "Segurança",
                            V4013 == 84014 ~ "Defesa",
                            V4013 == 85011 ~ "Educação Básica",
                            V4013 == 85012 ~ "Educação Básica",
                            V4013 == 85013 ~ "Educação Básica",
                            V4013 == 85014 ~ "Educação Superior",
                            V4013 == 85021 ~ "Educação Básica",
                            V4013 == 85029 ~ "Educação Básica",
                            V4013 == 86001 ~ "Saúde",
                            V4013 == 86002 ~ "Saúde",
                            V4013 == 86003 ~ "Saúde",
                            V4013 == 86004 ~ "Saúde",
                            V4013 == 86009 ~ "Saúde",
                            V4013 == 87000 ~ "Saúde",
                            V4013 == 88000 ~ "Saúde"),
           ocup = case_when(V4010 == 1321 ~ "Indústria",
                            V4010 == 1341 ~ "Saúde",
                            V4010 == 1342 ~ "Saúde",
                            V4010 == 1343 ~ "Saúde",
                            V4010 == 1344 ~ "Saúde",
                            V4010 == 1345 ~ "Educação Básica",
                            V4010 == 2211 ~ "Saúde",
                            V4010 == 2212 ~ "Saúde",
                            V4010 == 2221 ~ "Saúde",
                            V4010 == 2222 ~ "Saúde",
                            V4010 == 2230 ~ "Saúde",
                            V4010 == 2240 ~ "Saúde",
                            V4010 == 2250 ~ "Saúde",
                            V4010 == 2261 ~ "Saúde",
                            V4010 == 2262 ~ "Saúde",
                            V4010 == 2263 ~ "Saúde",
                            V4010 == 2264 ~ "Saúde",
                            V4010 == 2265 ~ "Saúde",
                            V4010 == 2266 ~ "Saúde",
                            V4010 == 2267 ~ "Saúde",
                            V4010 == 2269 ~ "Saúde",
                            V4010 == 2310 ~ "Educação Superior",
                            V4010 == 2320 ~ "Educação Superior",
                            V4010 == 2330 ~ "Educação Básica",
                            V4010 == 2341 ~ "Educação Básica",
                            V4010 == 2342 ~ "Educação Básica",
                            V4010 == 2351 ~ "Educação Básica",
                            V4010 == 2352 ~ "Educação Básica",
                            V4010 == 2353 ~ "Educação Básica",
                            V4010 == 2359 ~ "Educação Básica",
                            V4010 == 2635 ~ "Saúde",
                            V4010 == 3122 ~ "Indústria",
                            V4010 == 3151 ~ "Transporte ferroviário e metroviário",
                            V4010 == 3152 ~ "Transporte Aquaviário",
                            V4010 == 3153 ~ "Transporte Aéreo",
                            V4010 == 3154 ~ "Transporte Aéreo",
                            V4010 == 3155 ~ "Transporte Aéreo",
                            V4010 == 3211 ~ "Saúde",
                            V4010 == 3212 ~ "Saúde",
                            V4010 == 3213 ~ "Saúde",
                            V4010 == 3214 ~ "Saúde",
                            V4010 == 3221 ~ "Saúde",
                            V4010 == 3222 ~ "Saúde",
                            V4010 == 3230 ~ "Saúde",
                            V4010 == 3240 ~ "Saúde",
                            V4010 == 3251 ~ "Saúde",
                            V4010 == 3252 ~ "Saúde",
                            V4010 == 3253 ~ "Saúde",
                            V4010 == 3254 ~ "Saúde",
                            V4010 == 3255 ~ "Saúde",
                            V4010 == 3256 ~ "Saúde",
                            V4010 == 3257 ~ "Saúde",
                            V4010 == 3258 ~ "Saúde",
                            V4010 == 3259 ~ "Saúde",
                            V4010 == 3344 ~ "Saúde",
                            V4010 == 3351 ~ "Segurança",
                            V4010 == 3353 ~ "Segurança",
                            V4010 == 3355 ~ "Segurança",
                            V4010 == 3359 ~ "Segurança",
                            V4010 == 3412 ~ "Saúde",
                            V4010 == 4323 ~ "Transporte rodoviário de carga",
                            V4010 == 4412 ~ "Transporte rodoviário de carga",
                            V4010 == 5111 ~ "Transporte Aéreo",
                            V4010 == 5112 ~ "Transporte rodoviário de passageiros",
                            V4010 == 5411 ~ "Segurança",
                            V4010 == 5412 ~ "Segurança",
                            V4010 == 5413 ~ "Guardiões de presídios",
                            V4010 == 5414 ~ "Segurança",
                            V4010 == 5419 ~ "Guardiões de presídios",
                            V4010 == 8322 ~ "Transporte rodoviário de passageiros",
                            V4010 == 8331 ~ "Transporte rodoviário de passageiros",
                            V4010 == 8332 ~ "Transporte rodoviário de carga",
                            V4010 == 8350 ~ "Transporte Aquaviário",
                            V4010 == 9329 ~ "Indústria",
                            V4010 == 0110 ~ "Defesa",
                            V4010 == 0210 ~ "Defesa",
                            V4010 == 0411 ~ "Defesa",
                            V4010 == 0412 ~ "Defesa",
                            V4010 == 0511 ~ "Defesa",
                            V4010 == 0512 ~ "Defesa"),
           faixa_etaria = case_when(V2009 < 60 ~ "60 -",
                                    V2009 >= 60 & V2009 <= 64 ~ "60 - 64",
                                    V2009 >= 65 & V2009 <= 69 ~ "65 - 69",
                                    V2009 >= 70 & V2009 <= 74 ~ "70 - 74",
                                    V2009 >= 75 & V2009 <= 79 ~ "75 - 79",
                                    V2009 >= 80 & V2009 <= 84 ~ "80 - 84",
                                    V2009 >= 85 & V2009 <= 89 ~ "85 - 89",
                                    TRUE ~ "90 +"),
           cnae = if_else(is.na(cnae) == TRUE,
                          ocup, 
                          cnae)) %>% 
    filter(is.na(cnae) == FALSE) %>% 
    select(UF, cnae, faixa_etaria, V1032)

rm(PNAD_2019_original)

### Cria base de dados com número de trabalhadores da saúde por UF em 2019
prof_2019_uf <- PNAD_2019 %>% 
    group_by(UF, cnae) %>% 
    summarise(prof_uf = sum(V1032)) %>% 
    ungroup() 

### Cria base de dados com número de trabalhadores da saúde por UF e idade em 2019
prof_2019_uf_idade <- PNAD_2019 %>% 
    group_by(UF, cnae, faixa_etaria) %>% 
    summarise(prof_uf_idade = sum(V1032)) %>% 
    ungroup() 

### Merge bases 
prof_2019_uf_idade <- left_join(prof_2019_uf_idade, prof_2019_uf,
                                by = c("UF", "cnae"))

### Cálculo das proporções de trabalhadores da saúde por faixa etária e UF
prof_2019_uf_idade <- prof_2019_uf_idade %>% 
    mutate(share = prof_uf_idade / prof_uf,
           share_saude_60_menos = if_else(cnae == "Saúde" & faixa_etaria == "60 -", share, 0),
           share_saude_60_64 = if_else(cnae == "Saúde" & faixa_etaria == "60 - 64", share, 0),
           share_saude_65_69 = if_else(cnae == "Saúde" & faixa_etaria == "65 - 69", share, 0),
           share_saude_70_74 = if_else(cnae == "Saúde" & faixa_etaria == "70 - 74", share, 0),
           share_saude_75_79 = if_else(cnae == "Saúde" & faixa_etaria == "75 - 79", share, 0),
           share_saude_80_84 = if_else(cnae == "Saúde" & faixa_etaria == "80 - 84", share, 0),
           share_saude_85_89 = if_else(cnae == "Saúde" & faixa_etaria == "85 - 89", share, 0),
           share_saude_90_mais = if_else(cnae == "Saúde" & faixa_etaria == "90 +", share, 0),
           share_ed_basica_60_menos = if_else(cnae == "Educação Básica" & faixa_etaria == "60 -", share, 0),
           share_ed_basica_60_64 = if_else(cnae == "Educação Básica" & faixa_etaria == "60 - 64", share, 0),
           share_ed_basica_65_69 = if_else(cnae == "Educação Básica" & faixa_etaria == "65 - 69", share, 0),
           share_ed_basica_70_74 = if_else(cnae == "Educação Básica" & faixa_etaria == "70 - 74", share, 0),
           share_ed_basica_75_79 = if_else(cnae == "Educação Básica" & faixa_etaria == "75 - 79", share, 0),
           share_ed_basica_80_84 = if_else(cnae == "Educação Básica" & faixa_etaria == "80 - 84", share, 0),
           share_ed_basica_85_89 = if_else(cnae == "Educação Básica" & faixa_etaria == "85 - 89", share, 0),
           share_ed_basica_90_mais = if_else(cnae == "Educação Básica" & faixa_etaria == "90 +", share, 0),
           share_ed_superior_60_menos = if_else(cnae == "Educação Superior" & faixa_etaria == "60 -", share, 0),
           share_ed_superior_60_64 = if_else(cnae == "Educação Superior" & faixa_etaria == "60 - 64", share, 0),
           share_ed_superior_65_69 = if_else(cnae == "Educação Superior" & faixa_etaria == "65 - 69", share, 0),
           share_ed_superior_70_74 = if_else(cnae == "Educação Superior" & faixa_etaria == "70 - 74", share, 0),
           share_ed_superior_75_79 = if_else(cnae == "Educação Superior" & faixa_etaria == "75 - 79", share, 0),
           share_ed_superior_80_84 = if_else(cnae == "Educação Superior" & faixa_etaria == "80 - 84", share, 0),
           share_ed_superior_85_89 = if_else(cnae == "Educação Superior" & faixa_etaria == "85 - 89", share, 0),
           share_ed_superior_90_mais = if_else(cnae == "Educação Superior" & faixa_etaria == "90 +", share, 0),
           share_tr_rod_carga_60_menos = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "60 -", share, 0),
           share_tr_rod_carga_60_64 = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "60 - 64", share, 0),
           share_tr_rod_carga_65_69 = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "65 - 69", share, 0),
           share_tr_rod_carga_70_74 = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "70 - 74", share, 0),
           share_tr_rod_carga_75_79 = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "75 - 79", share, 0),
           share_tr_rod_carga_80_84 = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "80 - 84", share, 0),
           share_tr_rod_carga_85_89 = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "85 - 89", share, 0),
           share_tr_rod_carga_90_mais = if_else(cnae == "Transporte rodoviário de carga" & faixa_etaria == "90 +", share, 0),
           share_industria_60_menos = if_else(cnae == "Indústria" & faixa_etaria == "60 -", share, 0),
           share_industria_60_64 = if_else(cnae == "Indústria" & faixa_etaria == "60 - 64", share, 0),
           share_industria_65_69 = if_else(cnae == "Indústria" & faixa_etaria == "65 - 69", share, 0),
           share_industria_70_74 = if_else(cnae == "Indústria" & faixa_etaria == "70 - 74", share, 0),
           share_industria_75_79 = if_else(cnae == "Indústria" & faixa_etaria == "75 - 79", share, 0),
           share_industria_80_84 = if_else(cnae == "Indústria" & faixa_etaria == "80 - 84", share, 0),
           share_industria_85_89 = if_else(cnae == "Indústria" & faixa_etaria == "85 - 89", share, 0),
           share_industria_90_mais = if_else(cnae == "Indústria" & faixa_etaria == "90 +", share, 0),
           share_defesa_60_menos = if_else(cnae == "Defesa" & faixa_etaria == "60 -", share, 0),
           share_defesa_60_64 = if_else(cnae == "Defesa" & faixa_etaria == "60 - 64", share, 0),
           share_defesa_65_69 = if_else(cnae == "Defesa" & faixa_etaria == "65 - 69", share, 0),
           share_defesa_70_74 = if_else(cnae == "Defesa" & faixa_etaria == "70 - 74", share, 0),
           share_defesa_75_79 = if_else(cnae == "Defesa" & faixa_etaria == "75 - 79", share, 0),
           share_defesa_80_84 = if_else(cnae == "Defesa" & faixa_etaria == "80 - 84", share, 0),
           share_defesa_85_89 = if_else(cnae == "Defesa" & faixa_etaria == "85 - 89", share, 0),
           share_defesa_90_mais = if_else(cnae == "Defesa" & faixa_etaria == "90 +", share, 0),
           share_seguranca_60_menos = if_else(cnae == "Segurança" & faixa_etaria == "60 -", share, 0),
           share_seguranca_60_64 = if_else(cnae == "Segurança" & faixa_etaria == "60 - 64", share, 0),
           share_seguranca_65_69 = if_else(cnae == "Segurança" & faixa_etaria == "65 - 69", share, 0),
           share_seguranca_70_74 = if_else(cnae == "Segurança" & faixa_etaria == "70 - 74", share, 0),
           share_seguranca_75_79 = if_else(cnae == "Segurança" & faixa_etaria == "75 - 79", share, 0),
           share_seguranca_80_84 = if_else(cnae == "Segurança" & faixa_etaria == "80 - 84", share, 0),
           share_seguranca_85_89 = if_else(cnae == "Segurança" & faixa_etaria == "85 - 89", share, 0),
           share_seguranca_90_mais = if_else(cnae == "Segurança" & faixa_etaria == "90 +", share, 0),
           share_guarda_pris_60_menos = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "60 -", share, 0),
           share_guarda_pris_60_64 = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "60 - 64", share, 0),
           share_guarda_pris_65_69 = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "65 - 69", share, 0),
           share_guarda_pris_70_74 = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "70 - 74", share, 0),
           share_guarda_pris_75_79 = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "75 - 79", share, 0),
           share_guarda_pris_80_84 = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "80 - 84", share, 0),
           share_guarda_pris_85_89 = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "85 - 89", share, 0),
           share_guarda_pris_90_mais = if_else(cnae == "Guardiões de presídios" & faixa_etaria == "90 +", share, 0),
           share_tr_rod_pass_60_menos = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "60 -", share, 0),
           share_tr_rod_pass_60_64 = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "60 - 64", share, 0),
           share_tr_rod_pass_65_69 = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "65 - 69", share, 0),
           share_tr_rod_pass_70_74 = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "70 - 74", share, 0),
           share_tr_rod_pass_75_79 = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "75 - 79", share, 0),
           share_tr_rod_pass_80_84 = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "80 - 84", share, 0),
           share_tr_rod_pass_85_89 = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "85 - 89", share, 0),
           share_tr_rod_pass_90_mais = if_else(cnae == "Transporte rodoviário de passageiros" & faixa_etaria == "90 +", share, 0),
           share_tr_aqua_60_menos = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "60 -", share, 0),
           share_tr_aqua_60_64 = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "60 - 64", share, 0),
           share_tr_aqua_65_69 = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "65 - 69", share, 0),
           share_tr_aqua_70_74 = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "70 - 74", share, 0),
           share_tr_aqua_75_79 = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "75 - 79", share, 0),
           share_tr_aqua_80_84 = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "80 - 84", share, 0),
           share_tr_aqua_85_89 = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "85 - 89", share, 0),
           share_tr_aqua_90_mais = if_else(cnae == "Transporte Aquaviário" & faixa_etaria == "90 +", share, 0),
           share_tr_aereo_60_menos = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "60 -", share, 0),
           share_tr_aereo_60_64 = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "60 - 64", share, 0),
           share_tr_aereo_65_69 = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "65 - 69", share, 0),
           share_tr_aereo_70_74 = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "70 - 74", share, 0),
           share_tr_aereo_75_79 = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "75 - 79", share, 0),
           share_tr_aereo_80_84 = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "80 - 84", share, 0),
           share_tr_aereo_85_89 = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "85 - 89", share, 0),
           share_tr_aereo_90_mais = if_else(cnae == "Transporte Aéreo" & faixa_etaria == "90 +", share, 0),
           share_tr_ferrov_60_menos = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "60 -", share, 0),
           share_tr_ferrov_60_64 = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "60 - 64", share, 0),
           share_tr_ferrov_65_69 = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "65 - 69", share, 0),
           share_tr_ferrov_70_74 = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "70 - 74", share, 0),
           share_tr_ferrov_75_79 = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "75 - 79", share, 0),
           share_tr_ferrov_80_84 = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "80 - 84", share, 0),
           share_tr_ferrov_85_89 = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "85 - 89", share, 0),
           share_tr_ferrov_90_mais = if_else(cnae == "Transporte ferroviário e metroferroviário" & faixa_etaria == "90 +", share, 0)) %>% 
    group_by(UF) %>% 
    summarise(share_saude_60_menos = sum(share_saude_60_menos),
              share_saude_60_64 = sum(share_saude_60_64),
              share_saude_65_69 = sum(share_saude_65_69),
              share_saude_70_74 = sum(share_saude_70_74),
              share_saude_75_79 = sum(share_saude_75_79),
              share_saude_80_84 = sum(share_saude_80_84),
              share_saude_85_89 = sum(share_saude_85_89),
              share_saude_90_mais = sum(share_saude_90_mais),
              share_ed_basica_60_menos = sum(share_ed_basica_60_menos),
              share_ed_basica_60_64 = sum(share_ed_basica_60_64),
              share_ed_basica_65_69 = sum(share_ed_basica_65_69),
              share_ed_basica_70_74 = sum(share_ed_basica_70_74),
              share_ed_basica_75_79 = sum(share_ed_basica_75_79),
              share_ed_basica_80_84 = sum(share_ed_basica_80_84),
              share_ed_basica_85_89 = sum(share_ed_basica_85_89),
              share_ed_basica_90_mais = sum(share_ed_basica_90_mais),
              share_ed_superior_60_menos = sum(share_ed_superior_60_menos),
              share_ed_superior_60_64 = sum(share_ed_superior_60_64),
              share_ed_superior_65_69 = sum(share_ed_superior_65_69),
              share_ed_superior_70_74 = sum(share_ed_superior_70_74),
              share_ed_superior_75_79 = sum(share_ed_superior_75_79),
              share_ed_superior_80_84 = sum(share_ed_superior_80_84),
              share_ed_superior_85_89 = sum(share_ed_superior_85_89),
              share_ed_superior_90_mais = sum(share_ed_superior_90_mais),
              share_tr_rod_carga_60_menos = sum(share_tr_rod_carga_60_menos),
              share_tr_rod_carga_60_64 = sum(share_tr_rod_carga_60_64),
              share_tr_rod_carga_65_69 = sum(share_tr_rod_carga_65_69),
              share_tr_rod_carga_70_74 = sum(share_tr_rod_carga_70_74),
              share_tr_rod_carga_75_79 = sum(share_tr_rod_carga_75_79),
              share_tr_rod_carga_80_84 = sum(share_tr_rod_carga_80_84),
              share_tr_rod_carga_85_89 = sum(share_tr_rod_carga_85_89),
              share_tr_rod_carga_90_mais = sum(share_tr_rod_carga_90_mais),
              share_industria_60_menos = sum(share_industria_60_menos),
              share_industria_60_64 = sum(share_industria_60_64),
              share_industria_65_69 = sum(share_industria_65_69),
              share_industria_70_74 = sum(share_industria_70_74),
              share_industria_75_79 = sum(share_industria_75_79),
              share_industria_80_84 = sum(share_industria_80_84),
              share_industria_85_89 = sum(share_industria_85_89),
              share_industria_90_mais = sum(share_industria_90_mais),
              share_defesa_60_menos = sum(share_defesa_60_menos),
              share_defesa_60_64 = sum(share_defesa_60_64),
              share_defesa_65_69 = sum(share_defesa_65_69),
              share_defesa_70_74 = sum(share_defesa_70_74),
              share_defesa_75_79 = sum(share_defesa_75_79),
              share_defesa_80_84 = sum(share_defesa_80_84),
              share_defesa_85_89 = sum(share_defesa_85_89),
              share_defesa_90_mais = sum(share_defesa_90_mais),
              share_seguranca_60_menos = sum(share_seguranca_60_menos),
              share_seguranca_60_64 = sum(share_seguranca_60_64),
              share_seguranca_65_69 = sum(share_seguranca_65_69),
              share_seguranca_70_74 = sum(share_seguranca_70_74),
              share_seguranca_75_79 = sum(share_seguranca_75_79),
              share_seguranca_80_84 = sum(share_seguranca_80_84),
              share_seguranca_85_89 = sum(share_seguranca_85_89),
              share_seguranca_90_mais = sum(share_seguranca_90_mais),
              share_guarda_pris_60_menos = sum(share_guarda_pris_60_menos),
              share_guarda_pris_60_64 = sum(share_guarda_pris_60_64),
              share_guarda_pris_65_69 = sum(share_guarda_pris_65_69),
              share_guarda_pris_70_74 = sum(share_guarda_pris_70_74),
              share_guarda_pris_75_79 = sum(share_guarda_pris_75_79),
              share_guarda_pris_80_84 = sum(share_guarda_pris_80_84),
              share_guarda_pris_85_89 = sum(share_guarda_pris_85_89),
              share_guarda_pris_90_mais = sum(share_guarda_pris_90_mais),
              share_tr_rod_pass_60_menos = sum(share_tr_rod_pass_60_menos),
              share_tr_rod_pass_60_64 = sum(share_tr_rod_pass_60_64),
              share_tr_rod_pass_65_69 = sum(share_tr_rod_pass_65_69),
              share_tr_rod_pass_70_74 = sum(share_tr_rod_pass_70_74),
              share_tr_rod_pass_75_79 = sum(share_tr_rod_pass_75_79),
              share_tr_rod_pass_80_84 = sum(share_tr_rod_pass_80_84),
              share_tr_rod_pass_85_89 = sum(share_tr_rod_pass_85_89),
              share_tr_rod_pass_90_mais = sum(share_tr_rod_pass_90_mais),
              share_tr_aqua_60_menos = sum(share_tr_aqua_60_menos),
              share_tr_aqua_60_64 = sum(share_tr_aqua_60_64),
              share_tr_aqua_65_69 = sum(share_tr_aqua_65_69),
              share_tr_aqua_70_74 = sum(share_tr_aqua_70_74),
              share_tr_aqua_75_79 = sum(share_tr_aqua_75_79),
              share_tr_aqua_80_84 = sum(share_tr_aqua_80_84),
              share_tr_aqua_85_89 = sum(share_tr_aqua_85_89),
              share_tr_aqua_90_mais = sum(share_tr_aqua_90_mais),
              share_tr_aereo_60_menos = sum(share_tr_aereo_60_menos),
              share_tr_aereo_60_64 = sum(share_tr_aereo_60_64),
              share_tr_aereo_65_69 = sum(share_tr_aereo_65_69),
              share_tr_aereo_70_74 = sum(share_tr_aereo_70_74),
              share_tr_aereo_75_79 = sum(share_tr_aereo_75_79),
              share_tr_aereo_80_84 = sum(share_tr_aereo_80_84),
              share_tr_aereo_85_89 = sum(share_tr_aereo_85_89),
              share_tr_aereo_90_mais = sum(share_tr_aereo_90_mais),
              share_tr_ferrov_60_menos = sum(share_tr_ferrov_60_menos),
              share_tr_ferrov_60_64 = sum(share_tr_ferrov_60_64),
              share_tr_ferrov_65_69 = sum(share_tr_ferrov_65_69),
              share_tr_ferrov_70_74 = sum(share_tr_ferrov_70_74),
              share_tr_ferrov_75_79 = sum(share_tr_ferrov_75_79),
              share_tr_ferrov_80_84 = sum(share_tr_ferrov_80_84),
              share_tr_ferrov_85_89 = sum(share_tr_ferrov_85_89),
              share_tr_ferrov_90_mais = sum(share_tr_ferrov_90_mais)) %>% 
    ungroup()

################################################################################
################## Trab. Sistema Prisional - PNAD 2019 - IBGE ##################
################################################################################
trab_sist_pris <- PNAD_2019 %>% 
    filter(cnae == "Guardiões de presídios") %>% 
    filter(faixa_etaria == "60 -") %>% 
    group_by(UF, faixa_etaria) %>% 
    summarise(trab_sist_pris = round(sum(V1032),0))

trab_sist_pris_mun <- dados_LAI %>% 
    select(municipio, pop_priv_liberdade_MS) %>% 
    mutate(UF = left(municipio,2)) %>% 
    group_by(UF) %>% 
    mutate(prop_pop_priv_lib = pop_priv_liberdade_MS / sum(pop_priv_liberdade_MS))


trab_sist_pris_mun <- merge(trab_sist_pris_mun, trab_sist_pris,
                            by = "UF") %>% 
    mutate(trab_sist_pris_mun = round(trab_sist_pris * prop_pop_priv_lib,0)) %>% 
    select(municipio, trab_sist_pris_mun)

################################################################################
############################### PNS 2019 - IBGE ################################
################################################################################
pns2019_original <- read_pns("PNS_2019.txt",
                             "input_PNS_2019.txt")

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

rm(pns2019_original)

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
           plano_saude = if_else(plano_saude == 1, 
                                 "sim", 
                                 "não"),
           doenca_cronica = if_else(doenca_cronica == 1, 
                                    "sim", 
                                    "não"),
           gravida = if_else(gravida == 1, 
                             "sim", 
                             "não"),
           fumante = if_else(fumante_atual == 1 | fumante_atual == 2 | 
                                 fumante_passado == 1 | fumante_passado == 2, 
                             "sim", 
                             "não"),
           hipertensao = if_else(hipertensao == 1 & hipertensao_gravidez != 1, 
                                 "sim", 
                                 "não"),
           diabetes = if_else(diabetes == 1 & diabetes_gravidez != 1, 
                              "sim", 
                              "não"),
           doenca_cardiaca = if_else(doenca_cardiaca == 1, 
                                     "sim", 
                                     "não"),
           infarto = if_else(infarto == 1, 
                             "sim", 
                             "não"),
           angina = if_else(angina == 1, 
                            "sim", 
                            "não"),
           insuf_cardiaca = if_else(insuf_cardiaca == 1, 
                                    "sim", 
                                    "não"),
           arritmia = if_else(arritmia == 1, 
                              "sim", 
                              "não"),
           asma = if_else(asma == 1, 
                          "sim", 
                          "não"),
           artrite = if_else(artrite == 1, 
                             "sim", 
                             "não"),
           depressão = if_else(depressão == 1, 
                               "sim", 
                               "não"),
           doenca_cronica_pulmao = if_else(doenca_cronica_pulmao == 1, 
                                           "sim", 
                                           "não"),
           doenca_pulmonar = if_else(doenca_cronica_pulmao == 1 | asma == 1, 
                                     "sim", 
                                     "não"),
           enfisema = if_else(enfisema == 1, 
                              "sim", 
                              "não"),
           bronquite = if_else(bronquite == 1, 
                               "sim", 
                               "não"),
           usa_oxigenio = if_else(usa_oxigenio == 1, 
                                  "sim", 
                                  "não"),
           cancer = if_else(cancer == 1, 
                            "sim",
                            "não"),
           cancer_pele = if_else(cancer_pele == 1, 
                                 "sim", 
                                 "não"),
           cancer_pulmao = if_else(cancer_pulmao == 1, 
                                   "sim", 
                                   "não"),
           cancer_colon = if_else(cancer_colon == 1, 
                                  "sim", 
                                  "não"),
           cancer_estomago = if_else(cancer_estomago == 1,
                                     "sim", 
                                     "não"),
           cancer_mama = if_else(cancer_mama == 1, 
                                 "sim", 
                                 "não"),
           cancer_utero = if_else(cancer_utero == 1, 
                                  "sim", 
                                  "não"),
           cancer_prostata = if_else(cancer_prostata == 1, 
                                     "sim",
                                     "não"),
           cancer_boca = if_else(cancer_boca == 1, 
                                 "sim", 
                                 "não"),
           cancer_bexiga = if_else(cancer_bexiga == 1, 
                                   "sim", 
                                   "não"),
           cancer_leucemia = if_else(cancer_leucemia == 1, 
                                     "sim", 
                                     "não"),
           cancer_cerebro = if_else(cancer_cerebro == 1, 
                                    "sim", 
                                    "não"),
           cancer_ovario = if_else(cancer_ovario == 1, 
                                   "sim", 
                                   "não"),
           cancer_tireoide = if_else(cancer_tireoide == 1, 
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
                                                                  if_else(idade <= 84,
                                                                          "80-84",
                                                                          if_else(idade <= 89,
                                                                                  "85-89",
                                                                                  "90+")))))))) %>%
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

rm(pns2019)

### Aplica fator de multiplicação nos resultados da PNS para adequar as populações
comorbidades_2020 <- merge(comorbidades_2019, pop_total_pns,
                           by.x = "UF",
                           by.y = "UF")

comorbidades_2020 <- merge(comorbidades_2020, pop_uf_2020,
                           by.x = "UF",
                           by.y = "UF")

comorbidades_2020 <- comorbidades_2020 %>% 
    mutate(pop_comorbidade_menos_60_2020 = round(pop_comorbidade_menos_60_2019 
                                                 / populacao_menos_60_pns * 
                                                     populacao_menos_60_2020,0),
           pop_comorbidade_total_2020 = round(pop_comorbidade_total_2019 /
                                                  populacao_pns * populacao_2020,0)) %>% 
    select(UF, pop_comorbidade_menos_60_2020, pop_comorbidade_total_2020, 
           populacao_2020, populacao_menos_60_2020)

colnames(comorbidades_2020) <- c("UF", "comorbidade_uf_menos_60",
                                 "comorbidade_uf", "pop_uf", "pop_uf_menos_60")


################################################################################
############################## A G R E G A Ç Ã O ###############################
################################################################################
dados_MS <- merge(dados_MS, dados_LAI,
                  by = "municipio")

dados_MS <- merge(dados_MS, pop_sit_rua,
                  by = "municipio")

trab_educ_mun <- merge(trab_educ_mun, prof_2019_uf_idade,
                       by = "UF")

trab_educ_mun <- trab_educ_mun %>% 
    mutate(trab_educ_basica_mun_60_menos = round(trab_educ_basica * share_ed_basica_60_menos,0),
           trab_educ_superior_mun_60_menos = round(trab_educ_sup * share_ed_superior_60_menos,0)) %>% 
    rename(municipio = CO_MUNICIPIO) %>% 
    select(municipio, trab_educ_basica_mun_60_menos, trab_educ_superior_mun_60_menos)

### Dados demográficos
vacinacao_quant_bruto <- pop_mun_2020 %>% 
    mutate(UF = left(municipio,2), 
           pop_90_mais = `>90`,
           pop_85_89 = `85_89`,
           pop_80_84 = `80_84`,
           pop_75_79 = `75_79`,
           pop_70_74 = `70_74`,
           pop_65_69 = `65_69`,
           pop_60_64 = `60_64`,
           pop_menos_60 = `0_4`+`5_9`+`10_14`+`15_19`+`20_24`+ `25_29`+`30_34`+
               `35_39`+`40_44`+`45_49`+`50_54`+`55_59`,
           pop_total = `0_4`+`5_9`+`10_14`+`15_19`+`20_24`+ `25_29`+`30_34`+
               `35_39`+`40_44`+`45_49`+`50_54`+`55_59`+`60_64`+`65_69`+`70_74`+
               `75_79`+`80_84`+`85_89`+`>90`) %>% 
    select(UF, municipio, pop_menos_60, pop_60_64, pop_65_69, pop_70_74, 
           pop_75_79, pop_80_84, pop_85_89, pop_90_mais,  pop_total)

#vacinacao_quant_bruto <- merge(vacinacao_quant_bruto, trabalhadores_saude,
#                              by.x = "municipio",
#                              by.y = "CO_MUNICIPIO")

#vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, indigenas,
#                                   by = "municipio") 

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, comorbidades_2020,
                                   by = "UF")

vacinacao_quant_bruto <- vacinacao_quant_bruto %>% 
    mutate(comorbidade_total = round(pop_total * comorbidade_uf / pop_uf,0),
           comorbidade_menos_60 = round(pop_menos_60 * comorbidade_uf_menos_60 /
                                            pop_uf_menos_60,0))

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, prof_2019_uf_idade,
                                   by = "UF")

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, censo_SUAS_2019,
                                   by = "municipio")

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, dados_MS,
                                   by = "municipio") 

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, docentes_mun,
                                   by = "municipio") 

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, trab_educ_mun,
                                   by = "municipio") 

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, deficientes_2020,
                                   by = "municipio") 

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, emprego,
                                   by = "municipio") 

vacinacao_quant_bruto <- left_join(vacinacao_quant_bruto, trab_sist_pris_mun,
                                   by = "municipio") 

vacinacao_quant_bruto[is.na(vacinacao_quant_bruto)] <- 0

################################################################################
############### C Á L C U L O   V A L O R E S   L Í Q U I D O S ################
################################################################################
vacinacao_quant_liquido_mun <- vacinacao_quant_bruto %>% 
    group_by(municipio) %>% 
    mutate(idosos_instit_liq = round(idosos_inst,0),
           deficientes_instit_liq = round(deficientes_inst,0),
           indigenas_liq = round(indigenas_MS,0),
           trab_saude_liq = round(trab_saude_MS,0),
           pop_90_mais_liq = round(pop_90_mais - (share_saude_90_mais * trab_saude_MS) - (pop_90_mais * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           pop_85_89_liq = round(pop_85_89 - (share_saude_85_89 * trab_saude_MS) - (pop_85_89 * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           pop_80_84_liq = round(pop_80_84 - (share_saude_80_84 * trab_saude_MS) - (pop_80_84 * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           pop_75_79_liq = round(pop_75_79 - (share_saude_75_79 * trab_saude_MS) - (pop_75_79 * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           pop_trad_liq = round(pop_trad_MS,0), #preciso tirar os ribeirinhos com mais de 75 anos?
           quilombolas_liq = round(quilombolas_MS,0), #preciso tirar os quilombolas com mais de 75 anos?
           pop_70_74_liq = round(pop_70_74 - (share_saude_70_74 * trab_saude_MS) - (pop_70_74 * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           pop_65_69_liq = round(pop_65_69 - (share_saude_65_69 * trab_saude_MS) - (pop_65_69 * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           pop_60_64_liq = round(pop_60_64 - (share_saude_60_64 * trab_saude_MS) - (pop_60_64 * (idosos_inst / (pop_90_mais + pop_85_89 + pop_80_84 + pop_75_79 + pop_70_74 + pop_65_69 + pop_60_64))),0), # preciso retirar os indígenas acima de 80 anos?
           comorbidades_liq = round(comorbidade_menos_60 - (deficientes_inst * (comorbidade_total / pop_total)) - (indigenas_MS * (comorbidade_total / pop_total)) - (trab_saude_MS * (comorbidade_total /pop_total)) - (pop_trad_MS * (comorbidade_total / pop_total)) - (quilombolas_MS * (comorbidade_total / pop_total)),0),
           deficientes_liq = round(deficientes - deficientes_instit_liq - trab_saude_liq * (deficientes / pop_total) - pop_trad_liq * (deficientes / pop_total) - quilombolas_liq * (deficientes / pop_total) - comorbidade_menos_60 * (deficientes / pop_total),0), 
           pop_sit_rua_liq = round(pop_sit_rua,0), 
           pop_priv_liberdade_liq = round(pop_priv_liberdade_MS,0),
           trab_sist_prisional_liq = round(trab_sist_pris_mun - (trab_sist_pris_mun * (comorbidade_menos_60 / pop_total)),0),
           trab_educ_basica_liq = round((basico_60_menos + trab_educ_basica_mun_60_menos) - ((basico_60_menos + trab_educ_basica_mun_60_menos) * (comorbidade_menos_60 / pop_total)),0), 
           trab_educ_superior_liq = round((superior_60_menos + trab_educ_superior_mun_60_menos) - ((superior_60_menos + trab_educ_superior_mun_60_menos) * (comorbidade_menos_60 / pop_total)),0), 
           trab_seguranca_liq = round(trab_seg_salv - (trab_seg_salv * (comorbidade_menos_60 / pop_total)) - (share_seguranca_60_64 + share_seguranca_65_69 + share_seguranca_70_74 + share_seguranca_75_79 + share_seguranca_80_84 + share_seguranca_85_89 + share_seguranca_90_mais) * trab_seg_salv,0), 
           militares_liq = round(militares_MS - (militares_MS * (comorbidade_menos_60 / pop_total)) - (share_defesa_60_64 + share_defesa_65_69 + share_defesa_70_74 + share_defesa_75_79 + share_defesa_80_84 + share_defesa_85_89 + share_defesa_90_mais) * militares_MS,0),
           trab_transp_coletivo_liq = round(trab_tr_rod_col - (trab_tr_rod_col * (comorbidade_menos_60 / pop_total)) - (share_tr_rod_pass_60_64 + share_tr_rod_pass_65_69 + share_tr_rod_pass_70_74 + share_tr_rod_pass_75_79 + share_tr_rod_pass_80_84 + share_tr_rod_pass_85_89 + share_tr_rod_pass_90_mais) * trab_tr_rod_col,0),
           trab_transp_ferroviario_liq = round(trab_tr_ferr - (trab_tr_ferr * (comorbidade_menos_60 / pop_total)) - (share_tr_ferrov_60_64 + share_tr_ferrov_65_69 + share_tr_ferrov_70_74 + share_tr_ferrov_75_79 + share_tr_ferrov_80_84 + share_tr_ferrov_85_89 + share_tr_ferrov_90_mais) * trab_tr_ferr,0),
           trab_transp_aereo_liq = round(trab_tr_aereo - (trab_tr_aereo * (comorbidade_menos_60 / pop_total)) - (share_tr_aereo_60_64 + share_tr_aereo_65_69 + share_tr_aereo_70_74 + share_tr_aereo_75_79 + share_tr_aereo_80_84 + share_tr_aereo_85_89 + share_tr_aereo_90_mais) * trab_tr_aereo,0),
           trab_transp_aqua_liq = round(trab_tr_aqua - (trab_tr_aqua * (comorbidade_menos_60 / pop_total)) - (share_tr_aqua_60_64 + share_tr_aqua_65_69 + share_tr_aqua_70_74 + share_tr_aqua_75_79 + share_tr_aqua_80_84 + share_tr_aqua_85_89 + share_tr_aqua_90_mais) * trab_tr_aqua,0),
           camioneiros_liq = round(trab_camioneiros - (trab_camioneiros * (comorbidade_menos_60 / pop_total)) - (share_tr_rod_carga_60_64 + share_tr_rod_carga_65_69 + share_tr_rod_carga_70_74 + share_tr_rod_carga_75_79 + share_tr_rod_carga_80_84 + share_tr_rod_carga_85_89 + share_tr_rod_carga_90_mais) * trab_camioneiros,0),
           trab_porto_liq = round(trab_porto - (trab_porto * (comorbidade_menos_60 / pop_total)) - (share_tr_aqua_60_64 + share_tr_aqua_65_69 + share_tr_aqua_70_74 + share_tr_aqua_75_79 + share_tr_aqua_80_84 + share_tr_aqua_85_89 + share_tr_aqua_90_mais) * trab_porto,0),
           trab_ind_liq = round(trab_ind - (trab_ind * (comorbidade_menos_60 / pop_total)) - (share_industria_60_64 + share_industria_65_69 + share_industria_70_74 + share_industria_75_79 + share_industria_80_84 + share_industria_85_89 + share_industria_90_mais) * trab_ind,0),
           pop_restante = round(pop_total - idosos_instit_liq - deficientes_instit_liq - 
                                    indigenas_liq - trab_saude_liq - pop_90_mais_liq - 
                                    pop_85_89_liq - pop_80_84_liq - pop_75_79_liq - 
                                    pop_trad_liq - quilombolas_liq - pop_70_74_liq - 
                                    pop_65_69_liq - pop_60_64_liq - comorbidades_liq - 
                                    deficientes_liq - pop_sit_rua_liq - pop_priv_liberdade_liq - 
                                    trab_sist_prisional_liq - trab_educ_basica_liq - 
                                    trab_educ_superior_liq - trab_seguranca_liq - 
                                    militares_liq - trab_transp_coletivo_liq - 
                                    trab_transp_ferroviario_liq - trab_transp_aereo_liq - 
                                    trab_transp_aqua_liq - camioneiros_liq - 
                                    trab_porto_liq - trab_ind_liq,0)) %>% 
    select(UF, municipio, idosos_instit_liq, deficientes_instit_liq, indigenas_liq,
           trab_saude_liq, pop_90_mais_liq, pop_85_89_liq, pop_80_84_liq, pop_75_79_liq,
           pop_trad_liq, quilombolas_liq, pop_70_74_liq, pop_65_69_liq,
           pop_60_64_liq, comorbidades_liq, deficientes_liq, pop_sit_rua_liq, 
           pop_priv_liberdade_liq, trab_sist_prisional_liq, trab_educ_basica_liq, 
           trab_educ_superior_liq, trab_seguranca_liq, militares_liq, trab_transp_coletivo_liq, 
           trab_transp_ferroviario_liq, trab_transp_aereo_liq, trab_transp_aqua_liq, 
           camioneiros_liq, trab_porto_liq, trab_ind_liq, pop_restante, pop_total)

################################################################################
###################### O R G A N I Z A Ç Ã O   D A D O S #######################
################################################################################

### Organização dos dados por município
vacinacao_quant_liquido_mun <- merge(cities_id, vacinacao_quant_liquido_mun,
                                     by.x = "city_id_6",
                                     by.y = "municipio") %>% 
    select(state_code, state_id, state_name, city_id, city_id_6, city_name,
           idosos_instit_liq, deficientes_instit_liq, indigenas_liq,
           trab_saude_liq, pop_90_mais_liq, pop_85_89_liq, pop_80_84_liq, pop_75_79_liq,
           pop_trad_liq, quilombolas_liq, pop_70_74_liq, pop_65_69_liq,
           pop_60_64_liq, comorbidades_liq, deficientes_liq, pop_sit_rua_liq, 
           pop_priv_liberdade_liq, trab_sist_prisional_liq, trab_educ_basica_liq, 
           trab_educ_superior_liq, trab_seguranca_liq, militares_liq, trab_transp_coletivo_liq, 
           trab_transp_ferroviario_liq, trab_transp_aereo_liq, trab_transp_aqua_liq, 
           camioneiros_liq, trab_porto_liq, trab_ind_liq, pop_restante, pop_total)

vacinacao_quant_liquido_mun[vacinacao_quant_liquido_mun<0] <- 0

colnames(vacinacao_quant_liquido_mun) <- c("Código Estado",
                                           "Sigla Estado",
                                           "Estado",
                                           "Código Município - 7 dígitos",
                                           "Código Município - 6 dígitos",
                                           "Município",
                                           "Pessoas com 60 anos ou mais institucionalizadas",
                                           "Pessoas com Deficiência Institucionalizadas",
                                           "Povos indígenas Vivendo em Terras Indígenas", 
                                           "Trabalhadores de Saúde", 
                                           "Pessoas de 90 anos ou mais",
                                           "Pessoas de 85 a 89 anos", 
                                           "Pessoas de 80 a 84 anos", 
                                           "Pessoas de 75 a 79 anos", 
                                           "Povos e Comunidades tradicionais Ribeirinhas", 
                                           "Povos e Comunidades tradicionais Quilombolas", 
                                           "Pessoas de 70 a 74 anos", 
                                           "Pessoas de 65 a 69 anos", 
                                           "Pessoas de 60 a 64 anos", 
                                           "População com pelo menos uma comorbidade", 
                                           "Pessoas com Deficiência Permanente Grave", 
                                           "Pessoas em Situação de Rua", 
                                           "População Privada de Liberdade", 
                                           "Funcionários do Sistema de Privação de Liberdade", 
                                           "Trabalhadores da Educação do Ensino Básico", 
                                           "Trabalhadores da Educação do Ensino Superior", 
                                           "Forças de Segurança e Salvamento", 
                                           "Forças Armadas",
                                           "Trabalhadores de Transporte Coletivo Rodoviário de Passageiros",
                                           "Trabalhadores de Transporte Metroviário e Ferroviário",
                                           "Trabalhadores de Transporte Aéreo",
                                           "Trabalhadores de Transporte Aquaviário",
                                           "Caminhoneiros",
                                           "Trabalhadores Portuários",
                                           "Trabalhadores Industriais",
                                           "População restante",
                                           "População total")

################################################################################
################ S A L V A   A R Q U I V O   E M   E X C E L ###################
################################################################################
write.xlsx(vacinacao_quant_liquido_mun,"Quantitativo_vacina_liq_mun_dados_abertos.xlsx")
