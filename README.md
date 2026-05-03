# Democracia Mobilizada: Protesto e Valores Democráticos

Este repositório contém os dados e scripts utilizados no artigo:

**“Democracia Mobilizada: o papel do protesto na formação de valores democráticos”**
(Daniel Leonel da Rocha)

## 📌 Descrição

O artigo investiga se e como o engajamento em protestos está associado a diferentes concepções de democracia, considerando múltiplas dimensões (participativa, liberal, social e majoritária/autoritarista).

A análise utiliza dados do projeto ReDem e modelos estatísticos com ponderação amostral.

## 📊 Dados

O arquivo `base_democracia_mobilizada.csv` contém:

* Itens de concepções de democracia (P02A–P22)
* Variável de engajamento em protesto (`grupo_protesto`)
* Variáveis de controle:

  * idade
  * renda
  * educação
  * ideologia
  * sexo
* Peso amostral (`FATOR_POND`)

Os dados já estão tratados e prontos para replicação.

## ⚙️ Metodologia

Foram estimados modelos de regressão com desenho amostral complexo (`svyglm`), avaliando a associação entre engajamento em protesto e valores democráticos, com controles sociodemográficos.

Também são exploradas interações com ideologia política.

## 🔁 Replicação

Para replicar os resultados:

1. Abrir o script em R (`scripts/analise.R`)
2. Instalar os pacotes necessários
3. Executar o código

## 📁 Estrutura do repositório

* `data/` → base de dados
* `scripts/` → código da análise
* `output/` → gráficos e resultados

## 👤 Autor

Daniel Leonel da Rocha

## 📄 Observações

Este repositório foi construído como parte de um projeto de pós-doutorado e tem como objetivo garantir transparência e replicabilidade dos resultados.
