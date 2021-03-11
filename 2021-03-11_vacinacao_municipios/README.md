# CÁLCULO DOS GRUPOS PRIORITÁRIOS POR MUNICÍPIO

Este repositório contêm os dados e scripts utilizados para a estimação do número de indivíduos em cada grupo prioritário para a imunização contra a COVID-19 para os municípios brasileiros.

O trabalho se pautou em dados públicos para calcular a intersecção entre cada um dos grupos, de modo a apresentar os quantitativos líquidos de cada grupo. Assim, por exemplo, foram descontados do grupo de 60 a 64 anos todos os indivíduos que fariam parte desta faixa etária mas que foram vacinados em fases anteriores da campanha de imunização.


## Diretório `code`:

#### 1-capturar-tweets
Código usado na análise


## Diretório `raw`:

#### 1-pop_faixas_etarias_mun.csv
Arquivo com dados demográficos de 2020 por faixa etária e município do Ministério da Saúde 

#### 2-Censo_SUAS.xlsx
Variáveis do Censo SUAS de 2019 selecionadas para a análise

## Diretório `treated`:

#### 1-Trabalhadores_saude_detalhado.csv 
Arquivo com o número de trabalhadores da saúde extraído do CNES de novembro de 2020 por município

#### 2-População_indigena.xlsx
Distribuição da população indígena aldeada de acordo com o Departamento de Saúde Indígena - DESAI novembro de 2020
