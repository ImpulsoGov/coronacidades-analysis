# Análises sobre COVID-19

Repositório com o código de análises de Covid-19 nos municípios e estados brasileiros para mídia e interno.

### Organização

    ├── LICENSE
    ├── README.md                  <- Esse arquivo com instruções gerais! :)
    ├── data
    │   ├── output                 <- Tabelas de resultados
    │   ├── treated                <- Dados tratados
    │   └── raw                    <- Dados orginais (caso não tenha url)
    ├── notebooks                  <- Notebooks com as análises
    ├── scripts                    <- Scripts (caso use para tratamento)
    ├── requirements.txt           <- Pacotes do ambiente de análise


## Como fazer análises

1️⃣ [Criar um branch para suas análises](#1-criando-seu-branch)

2️⃣ [Ativar o ambiente virtual de modelagem](#2-ativando-ambiente-de-modelagem)

3️⃣ [Criar seu notebook em `notebooks`](#3-criando-seu-notebook)

4️⃣ [Puxar os dados da API](#4-puxando-dados-da-api)

5️⃣ [Subir sua análise no repo via _pull request_](#5-subindo-análise-no-repositório)

### 1. Criando seu branch

Depois de clonar o repositório no seu computador, crie uma branch para desenvolver suas análises.

```bash
$ git checkout -b analysis_[usuario] # ex: git checkout -b analysis_fernandascovino
```

Caso já tenha passado um tempo que você criou o branch e queira subir outro notebook, lembre-se puxar as atualizações do `master` para seu branch:

```bash
$ git checkout analysis_[usuario] # ex: git checkout -b analysis_fernandascovino

$ git pull

$ git merge master

# Para checar as mudanças
$ git status
```

💬 Concentre suas análises nesse branch para evitar problemas de versionamento

### 2. Ativando ambiente de modelagem

```bash
# Instale o 'make'
$ sudo apt-get install -y make

# Crie o virtualenv
$ make create-env

# Ative o ambiente
$ . .coronacidades-analysis/bin/activate

# Abra o jupyter
$ jupyter notebook

# Mude o kernel do notebook para .coronacidades-analysis
```

### 3. Criando seu notebook

Boas práticas:

- Nomeclatuta: `[data]_[conteudo].ipynb` 
> ex: `06-02 Onda de Mortes por Covid-19.ipynb`

- Primeira célula deve conter a descrição da análise.

- Deixe sempre o código limpo e legível :)

- Caso use um pacote novo, adicione na lista de pacotes em `requirements.txt`

### 4. Puxando dados da API

Todos os dados da API podem ser acessados aqui: http://datasource.coronacidades.org:7000/v1/, veja a lista de tabelas [aqui](https://github.com/ImpulsoGov/simulacovid-datasource/blob/master/README.md).

⚠️ **Não suba tabelas para o repositório caso não seja necessário!** Verifique sempre se o dado é sensível e/ou pesado ⚠️

- Caso você use outros arquivos na sua análise, coloque dentro da pasta `data/raw`
- Caso você gere arquivos na sua análise, coloque dentro da pasta `data/output`


### 5. Subindo análise no repositório

Tudo pronto para mostrar suas análises para outr@s colaborador@s? Então, na sua cópia local, adicione os arquivos para criar o _pull request_:

```bash
# Veja o que você mudou, e verifique se você está na sua branch!
$ git status

# Adicione o notebook no track
$ git add notebooks/[nome do notebook] # ex: git add notebooks/06-02 Onda de Mortes por Covid-19.ipynb

# Adicione uma msg sobre sua analise
$ git commit -m "[breve msg explicando o que foi feito]"

# Envie seu notebook para o GitHub subindo o seu branch!
$ git push

```

Depois de dar `push`, você verá no GitHub um aviso em amarelo que seu branch foi modificado. Lá terá um botão para `Create pull request` - pronto!

## Licença

Os códigos desse projeto estão licenciados sob *MIT License* - veja detalhes de uso e compartilhamento em [LICENSE.md](LICENSE.md).
