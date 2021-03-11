# CÁLCULO DOS GRUPOS PRIORITÁRIOS POR MUNICÍPIO

Este repositório contêm os dados e scripts utilizados para a estimação do número de indivíduos em cada grupo prioritário para a imunização contra a COVID-19 para os municípios brasileiros.

O trabalho se pautou em dados públicos para calcular a intersecção entre cada um dos grupos, de modo a apresentar os quantitativos líquidos de cada grupo. Assim, por exemplo, foram descontados do grupo de 60 a 64 anos todos os indivíduos que fariam parte desta faixa etária mas que foram vacinados em fases anteriores da campanha de imunização.


## Diretório `Raw`:
#### Contém os arquivos necessários para executar o script de estimação dos grupos prioritários líquidos: 

censo_SUAS.xlsx

campanha_influenza_2020.xlsx

deficientes_2010.xlsx

pop_faixas_etarias_mun.csv

pop_faixas_etarias_uf_detalhe.xlsx

variacao_pop_2010_2020.csv

## Diretório `Script`:
#### Contém script em R que executar a estimação dos grupos prioritários líquidos: 
grupos_prioritarios_municipios_divulgacao.R 
