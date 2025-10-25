# trab-r-probabilidade
# Análise Estatística: Agricultura Familiar e Financiamento Rural no Semiárido Brasileiro

## 💻 Projeto de Probabilidade e Estatística

---

## 📄 Descrição Geral

Este projeto é um trabalho acadêmico de Probabilidade e Estatística focado na **Análise da Agricultura Familiar e dos Motivos de Falta de Financiamento Rural** no Brasil, com um foco especial nas regiões do **Semiárido Nordestino e de Minas Gerais**.

O objetivo principal é aplicar técnicas de estatística descritiva (medidas de tendência central, dispersão, assimetria e curtose), visualização de dados (gráficos de barras e boxplots) e análise multivariada (teste t de Student e agrupamento hierárquico) para comparar os padrões e desafios enfrentados pela Agricultura Familiar versus a Agricultura Não Familiar.

### Fonte dos Dados
Os dados utilizados são provenientes do **IBGE - Censo Agropecuário**, que detalham o número de estabelecimentos rurais e os principais motivos alegados pelos produtores para a não obtenção de financiamento.

---
## 🚀 Estrutura do Repositório

O repositório contém os seguintes arquivos e diretórios gerados pelo script `analise_financiamento.R`:
```
.
├── Q1_estatisticas_descritivas.csv # Resultados detalhados da Estatística Descritiva
├── Q2_boxplot_motivos.png # Boxplot da distribuição dos motivos
├── Q2_barras_comparativo.png # Gráfico de barras comparando Familiar vs Não Familiar
├── Q3_distribuicoes.png # Gráfico de densidade dos principais motivos
├── Q4_dendrograma_regioes.png # Dendrograma de agrupamento hierárquico
├── Q4_heatmap_regional.png # Heatmap de características regionais padronizadas
└── analise_financiamento.R # Código fonte em R
```

---

## 🛠️ Requisitos e Execução

### Requisitos de Software
Para rodar a análise, você precisa ter instalado:

* **R** (versão 4.0 ou superior recomendada)
* **RStudio** (recomendado para edição e execução)

### Bibliotecas R
O script `analise_financiamento.R` utiliza as seguintes bibliotecas:

```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(tibble)
library(reshape2)