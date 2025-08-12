# Modelo de Regressão Burr XII unitária inflacionada em zero ou uns: formulação e estudo de simulação

Este repositório contém os materiais relacionados à pesquisa "Modelo de Regressão Burr XII unitária inflacionada em zero ou uns: formulação e estudo de simulação". O projeto está em andamento, com a etapa de simulações concluída e planos para a aplicação futura dos modelos na análise de taxas de evasão no ensino superior.

---

## Resumo da Pesquisa

A distribuição Burr XII unitária (UBXII) é uma ferramenta útil para modelar dados contínuos restritos ao intervalo (0, 1). No entanto, ela não acomoda dados que incluem valores exatos de zero e/ou um.

Para superar essa limitação, este trabalho propõe modelos de regressão inflacionados em zeros (ZIUBXII) ou uns (OIUBXII), que combinam uma distribuição discreta para lidar com a inflação e a distribuição UBXII para os valores no intervalo aberto.

O modelo proposto, que pertence à família GAMLSS (Generalized Additive Models for Location, Scale and Shape), permite relacionar os parâmetros da distribuição a covariáveis por meio de preditores lineares. A estimação é realizada pelo método da máxima verossimilhança.

Simulações foram conduzidas para avaliar o desempenho dos estimadores, e os resultados preliminares indicam que o viés e o erro quadrático médio diminuem à medida que o tamanho da amostra aumenta, confirmando a robustez da nossa abordagem.

Futuramente, planejamos aplicar esses modelos para analisar as taxas de evasão de calouros em cursos de ensino superior brasileiros, contribuindo para o **4º Objetivo de Desenvolvimento Sustentável (ODS 4)**: Educação de Qualidade.

---

## Palavras-chave
Dados Unitários; Distribuição Burr XII; Educação para Todos; Modelos Inflacionados; Reparametrização.

---

## Autores
* **Beatriz Woos Buffon** - Universidade Federal de Santa Maria
* **Renata Rojas Guerra** - Universidade Federal de Santa Maria
* **Laís Helen Loose** - Universidade Federal de Santa Maria

---

## Financiamento e Agradecimentos
Esta pesquisa foi financiada pelo:
* Instituto Serrapilheira (concessão nº 2211-41692)
* FAPERGS (concessão nº 23/2551-0001595-1)
* CNPq (concessão nº 306274/2022-1), concedida a Renata Rojas Guerra.

O conteúdo deste trabalho é de exclusiva responsabilidade dos autores e não representa necessariamente a opinião oficial das agências de fomento.

---

### Estrutura do Repositório

```bash
.
├── application/     # Materiais e dados da aplicação prática
│   ├── data_dictionary_2018.xls  # Dicionário de dados
│   ├── esqueleto_regressao       # Scripts de regressão
│   └── teste1                    # Arquivos de teste
├── codes/           # Scripts R com a formulação e funções dos modelos
│   ├── OIUBXII.R                    # Modelo inflacionado em zeros
│   ├── OIUBXII_internal_deriv.R     # Modelo inflacionado em zeros com mudança dentro das derivadas do GAMLSS
│   ├── ZIUBXII.R                    # Modelo inflacionado em uns
│   ├── ZIUBXII_internal_deriv.R     # Modelo inflacionado em uns com mudança dentro das derivadas do GAMLSS
│   ├── dist_burrxii.R               # Funções da Distribuição BurrXII
│   ├── dist_burrxii_inflacionada.R  # Funções da Distribuição BurrXII
│   └── score_vector.R               # Vetor Escore da Distribuição BurrXII
├── refs/            # Artigos e referências bibliográficas
└── README.md
---
```
### Contato
* [Linkedin](https://www.linkedin.com/in/beatriz-woos-buffon-102ab5191/)
* [Pórtfólio](https://beatrizbuffon21.github.io/)

