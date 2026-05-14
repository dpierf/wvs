# Analisando pobreza subjetiva e valores sociais por meio da WVS

Repositório do projeto de análise de valores sociais e pobreza subjetiva a partir dos
dados longitudinais da World Values Survey (WVS, 1981-2022), composto por dois módulos
complementares: um pipeline de Machine Learning e um dashboard interativo.

---

## Sobre o projeto

Este projeto é desenvolvido em duas frentes diferentes e complementares:
- Utilizando ferramentas de DataViz, avalia-se em que medida valores e percepções
  sociais mudam ao longo do tempo, entre países e em subgrupos sociodemográficos
- Recorrendo a _ensemble_, tenta-se inferir quais as principais variáveis para a
  autopercepção de pobreza dos respondentes, sem o uso de variáveis econômicas diretas.

A frente de DataViz cobre todas as 7 Waves (ondas) da pesquisa, enquanto a etapa de
machine learning prepara modelos para as waves 5 a 7 (2005–2022), permitindo comparações
temporais sobre a relação entre dimensões culturais e pobreza subjetiva.

O dashboard interativo deste produto pode ser visto em 🔗 [Dashboard WVS](https://huggingface.co/spaces/dpierf/wvs)

---

## Fonte de dados

INGLEHART, R. et al. (eds.). **World Values Survey: All Rounds -
Country-Pooled Datafile**. Madrid; Vienna: JD Systems Institute;
WVSA Secretariat, 2022. Dataset Version 5.0.
Disponível em: https://doi.org/10.14281/18241.17.

> Os dados não estão incluídos neste repositório. Para reproduzir as análises,
> faça o download do arquivo `WVS TimeSeries 1981 2022 Rds v5 0.zip` diretamente
> no site da [World Values Survey](https://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp) e
> salve-o na pasta `data/`. Não necessário descompactá-lo, nem renomeá-lo.

---

## Estrutura do repositório
```
├── main.R                   # Ponto de entrada: carrega pacotes, funções e executa o pipeline
├── R/
│   ├── config.R             # Parâmetros e constantes configuráveis
│   ├── pipeline.R           # Funções de preparação, feature selection, treino e avaliação
│   ├── dataviz.R            # Funções de visualização e orquestrador de gráficos
│   └── dashboard.R          # Aplicação Shiny (dashboard interativo)
├── data/                    # Diretório para o arquivo de dados (não versionado)
├── outputs/                 # Diretório para resultados visuais dos modelos (não versionado)
│   ├── models/              # Modelos LightGBM salvos (não versionado)
│   ├── plots/               # Gráficos gerados pelo pipeline (não versionado)
│   └── resultados_lgbm.rds  # Desempenho dos modelos LightGBM (não versionado)
└── README.md
```

---

## Requisitos

- R >= 4.2
- Pacotes R (instalados automaticamente via `main.R`):

```r
lightgbm, ggplot2, scales, cowplot, Information, rstatix, mRMRe,
glmnet, shapviz, rsample, tidyr, patchwork, countrycode, haven,
shiny, bslib, plotly, labelled, shinyWidgets, here, dplyr, pROC
```

---

## Como executar

1. Baixe o arquivo indicado na seção 'Fonte de Dados';
2. Abra o arquivo `main.R` e siga as etapas indicadas.

--- 

## Dashboard interativo

Após abarir `main.R`, e executar as seções 0. e 1. do arquivo, o dashboard pode ser iniciado com:

```r
baseWVS <- funs$carregar_base(vars$PATH_DATA, wave_min = 1)  # Base original, completa
baseVIZ <- funs$preparar_base(baseWVS)                       # Base enxuta para dashboard
rm(baseWVS) ; gc() ; cat('\014')                             # Removendo original e limpando
funs$dashboard_wvs(baseVIZ)                                  # Inicialização do dashboard
```

---

## Configuração

Os principais parâmetros do pipeline podem ser ajustados em `R/config.R`:

| Parâmetro | Descrição | Padrão |
|---|---|---|
| `WAVES`             | Waves a processar no ML                           | `c(5, 6, 7)` |
| `MISS_THRESHOLD`    | Limite de missing para remoção                    | `0.20`       |
| `FREQ_THRESHOLD`    | Limite máximo de respondentes na mesma categoria  | `0.95`       |
| `IV_THRESHOLD`      | Limite inferior de discriminação para manutenção  | `0.02`       |
| `CV_THRESHOLD`      | Limite infrior de associação para manutenção      | `0.05`       |
| `PROP_MRMR`         | Proporção de candidatas para mRMR                 | `0.60`       |
| `MAX_MRMR`          | Teto de variáveis para mRMR                       | `150`        |
| `LGB_ROUNDS`        | Máximo de iterações (árvores) do LightGBM         | `1000`       |
| `LGB_EARLY_STOP`    | Para o treino se AUC não melhora após N rodadas   | `50`         |
| `LGB_LEARNING_RATE` | Tamanho do passo de aprendizado                   | `0.05`       |
| `LGB_NUM_LEAVES`    | Complexidade máxima de cada árvore                | `31`         |
| `LGB_MIN_DATA`      | Mínimo de observações por folha                   | `20`         |
| `LGB_FEAT_FRAC`     | Fração de variáveis amostradas por árvore         | `0.8`        |  
| `LGB_BAG_FRAC`      | Fração de observações amostradas por árvore       | `0.8`        |
| `LGB_BAG_FREQ`      | Frequência de bagging (a cada N iterações)        | `5`          |
| `VARS_EXCLUIR`      | Lista de atributos removidos antes do Feature Selection | `c(...)` |
| `VARS_DASHBOARD`    | Lista de atributos a serem utilizados no Dashboard WVS  | `c(...)` |

---

## Reprodutibilidade

As sementes aleatórias estão fixadas em `config.R` (`SEED_SPLIT` e `SEED_LGB`).
Para resultados idênticos, mantenha os valores padrão e utilize a mesma versão
do arquivo de dados (WVS v5.0).

---

## Desenvolvimento

O desenvolvimento dos códigos aqui disponibilizados contou com o auxílio extensivo de inteligência
artificial generativa ao longo de todo o processo, incluindo arquitetura do pipeline, escrita e
revisão de código, decisões metodológicas e documentação. A ferramenta utilizada para tal fim foi:

> ANTHROPIC. Claude Sonnet 4.6. San Francisco: Anthropic, 2025. Disponível em: https://claude.ai. Acesso em: 13 mai. 2026.

O uso de IA não substitui a responsabilidade intelectual do autor sobre as escolhas metodológicas, interpretações e resultados apresentados.


---

## Licença

O código deste projeto está licenciado sob a
[Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

Você pode usar, modificar e distribuir o material para qualquer finalidade,
inclusive comercial, desde que a autoria original seja devidamente citada.

Os dados da World Values Survey estão sujeitos aos
[termos de uso da WVS](https://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp)
e não podem ser redistribuídos. Faça o download diretamente da fonte oficial.
