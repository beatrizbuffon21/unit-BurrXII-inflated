# Modelo de Regressão Burr XII unitária inflacionada em zero ou uns: formulação e estudo de simulação

### Status do Projeto
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

## Como Citar
Caso este trabalho seja publicado, você pode usar a seguinte referência:

*A ser definido.*

---

## Repositório
Neste repositório você pode encontrar:
* `scripts/`: Scripts em R com as implementações dos modelos ZIUBXII e OIUBXII.
* `simulacoes/`: Código e resultados dos estudos de simulação.
* `aplicacao/`: Código e análise dos dados de evasão (em desenvolvimento).
* `documentos/`: Artigos, apresentações e outros materiais relacionados ao projeto.

---

### Contribua
Contribuições são bem-vindas! Sinta-se à vontade para abrir uma *issue* para discutir ideias ou enviar um *pull request* com melhorias.

---

### Licença
Este projeto está sob a licença [MIT](link_para_licenca_ou_outra_licenca).
