esqueleto ziubxii do modelo com intercepto

library(readr)
library(dplyr)
library(gamlss)
library(tibble)
library(dummy)
library(moments)
library(lobstr)
library(gridExtra)

dados <- read.csv("C:/Users/Cliente/Downloads/rscripts-bia-20250723T143126Z-1-001/rscripts-bia/aplicacao/EVASAO.csv", header=TRUE)

# ============================================================
# LISTA DE CURSOS — 
# ============================================================
lista_curso1 <- c(
  "ADMINISTRAÇÃO", "PEDAGOGIA", "DIREITO", "CIÊNCIAS CONTÁBEIS", "EDUCAÇÃO FÍSICA", "ENFERMAGEM", "ENGENHARIA CIVIL",
  "PSICOLOGIA", "ENGENHARIA DE PRODUÇÃO", "FISIOTERAPIA", "CIÊNCIAS BIOLÓGICAS", "GESTÃO DE RECURSOS HUMANOS",
  "ARQUITETURA E URBANISMO", "NUTRIÇÃO", "FARMÁCIA", "ENGENHARIA MECÂNICA", "ENGENHARIA ELÉTRICA",
  "ANÁLISE E DESENVOLVIMENTO DE SISTEMAS", "MATEMÁTICA", "LOGÍSTICA", "BIOMEDICINA", "ODONTOLOGIA",
  "SISTEMAS DE INFORMAÇÃO", "SERVIÇO SOCIAL", "CIÊNCIA DA COMPUTAÇÃO", "QUÍMICA", "HISTÓRIA", "MEDICINA",
  "MEDICINA VETERINÁRIA", "AGRONOMIA", "MARKETING", "GEOGRAFIA", "FÍSICA",  "ESTÉTICA E COSMÉTICA",
  "PROCESSOS GERENCIAIS",
  "GESTÃO FINANCEIRA", "JORNALISMO", "ENGENHARIA QUÍMICA", "GASTRONOMIA", "CIÊNCIAS ECONÔMICAS",
  "REDES DE COMPUTADORES", "ENGENHARIA DE COMPUTAÇÃO", "FILOSOFIA", "ENGENHARIA DE CONTROLE E AUTOMAÇÃO",
  "GESTÃO COMERCIAL",  "GESTÃO DA TECNOLOGIA DA INFORMAÇÃO",
  "RADIOLOGIA", "CIÊNCIAS SOCIAIS", "ENGENHARIA AMBIENTAL", "ENGENHARIA AMBIENTAL E SANITÁRIA", "DESIGN GRÁFICO",
  "DESIGN", "DESIGN DE INTERIORES", "MÚSICA", "RELAÇÕES INTERNACIONAIS", "TURISMO",
  "DESIGN DE MODA", "TEOLOGIA", "PUBLICIDADE E PROPAGANDA", "ZOOTECNIA", "GESTÃO AMBIENTAL", "ARTES VISUAIS",
  "COMÉRCIO EXTERIOR", "SISTEMA DE INFORMAÇÃO", 
  "ENGENHARIA DE ALIMENTOS", "ENGENHARIA AGRONÔMICA", "SISTEMAS PARA INTERNET", "FONOAUDIOLOGIA", "JOGOS DIGITAIS",
  "GESTÃO DA QUALIDADE", "AUTOMAÇÃO INDUSTRIAL", "LETRAS", "ENGENHARIA FLORESTAL", "GESTÃO PÚBLICA", "ALIMENTOS",
  "FOTOGRAFIA", "GESTÃO HOSPITALAR", "AGRONEGÓCIO", "COMUNICAÇÃO SOCIAL", "GESTÃO DA PRODUÇÃO INDUSTRIAL",
  "TEATRO", "ENGENHARIA DE SOFTWARE", "ENGENHARIA DE MATERIAIS",
  "PRODUÇÃO AUDIOVISUAL", "RELAÇÕES PÚBLICAS", "ENGENHARIA DE PETRÓLEO", "ENGENHARIA ELETRÔNICA", "DANÇA",
  "MECATRÔNICA INDUSTRIAL", "ENGENHARIA MECATRÔNICA", "HOTELARIA", "COMPUTAÇÃO", "ESTATÍSTICA", "TERAPIA OCUPACIONAL",
  "EVENTOS", "GESTÃO DE TURISMO", "ENGENHARIA DA COMPUTAÇÃO", "CINEMA E AUDIOVISUAL", "GEOLOGIA", "BIBLIOTECONOMIA",
  "ENGENHARIA DE PRODUÇÃO MECÂNICA", "PRODUÇÃO MULTIMÍDIA", "MODA", "AGROECOLOGIA", "BIOLOGIA", "BIOTECNOLOGIA",
  "GESTÃO DE SEGURANÇA PRIVADA", "SEGURANÇA NO TRABALHO", "CONSTRUÇÃO DE EDIFÍCIOS", "ENGENHARIA DE MINAS",
  "ENGENHARIA DE PESCA", "SECRETARIADO EXECUTIVO", "ARTES CÊNICAS", "MANUTENÇÃO INDUSTRIAL", "SEGURANÇA DA INFORMAÇÃO",
  "CIÊNCIAS NATURAIS", "QUÍMICA INDUSTRIAL", "ADMINISTRAÇÃO PÚBLICA",
  "DESIGN DE PRODUTO", "INTERDISCIPLINAR EM CIÊNCIA E TECNOLOGIA", "FABRICAÇÃO MECÂNICA",
  "ENGENHARIA DE TELECOMUNICAÇÕES", "EDUCAÇÃO DO CAMPO",
  "ENGENHARIA AGRÍCOLA", "CIÊNCIAS DA COMPUTAÇÃO")

# ============================================================
# objeto que vai acumular os resultados
# ============================================================
resultados <- tibble()

# ============================================================
# LOOP — não mude nada abaixo desta linha
# ============================================================
for (curso_atual in lista_curso1) {
  
  message("\n==============================")
  message("Processando: ", curso_atual)
  message("==============================")
  
  # bloco original — só o nome do curso muda
  X <- dados |>
    filter(NO_CURSO == curso_atual, MATRICULAS > 9) |>
    mutate(TX_EVASAO = TX_EVASAO,
           TP_ORG_ACAD = as.factor(TP_ORGANIZACAO_ACADEMICA),
           RZINSCRITOS = QT_INSCRITO_TOTAL / QT_VG_TOTAL,
           RZFEM = QT_ING_FEM / QT_ING,
           RZ_ING_RESERVA = QT_ING_RESERVA_VAGA / QT_ING,
           RZ_ING_PUBLICA = QT_ING_RVREDEPUBLICA / QT_ING,
           RZETICO = QT_ING_RVETNICO / QT_ING,
           RZDEF = QT_ING_RVPDEF / QT_ING,
           RZSOCIAL = QT_ING_RVSOCIAL_RF / QT_ING,
           RZDEF2 = QT_ING_DEFICIENTE / QT_ING,
           RZATVEXT = QT_ING_ATIV_EXTRACURRICULAR / QT_ING,
           RZ_0_17 = QT_ING_0_17 / QT_ING,
           RZ_18_24 = QT_ING_18_24 / QT_ING,
           RZ_25_29 = QT_ING_25_29 / QT_ING,
           RZ_30_34 = QT_ING_30_34 / QT_ING,
           RZ_35_39 = QT_ING_35_39 / QT_ING,
           RZ_40_49 = QT_ING_40_49 / QT_ING,
           RZ_50_59 = QT_ING_50_59 / QT_ING,
           RZ_60 = QT_ING_60_MAIS / QT_ING,
           RZ_BRANCA = QT_ING_BRANCA / QT_ING,
           RZ_PRETA = QT_ING_PRETA / QT_ING,
           RZ_PARDA = QT_ING_PARDA / QT_ING,
           RZ_AMARELA = QT_ING_AMARELA / QT_ING,
           RZ_INDIGENA = QT_ING_INDIGENA / QT_ING,
           RZ_FINANC = QT_ING_FINANC / QT_ING,
           RZ_FIES = QT_ING_FIES / QT_ING,
           RZ_PROUNII = QT_ING_PROUNII / QT_ING,
           RZ_PROUNIP = QT_ING_PROUNIP / QT_ING,
           RZ_PUBLICA = QT_ING_PROCESCPUBLICA / QT_ING,
           RZ_PRIVADA = QT_ING_PROCESCPRIVADA / QT_ING
    ) |>
    select(TX_EVASAO, IN_CAPITAL, TP_ORG_ACAD, TP_REDE, NO_REGIAO,
           TP_GRAU_ACADEMICO, TP_MODALIDADE_ENSINO, RZINSCRITOS, RZFEM, QT_ING,
           QT_ING_APOIO_SOCIAL,
           RZ_ING_RESERVA, RZ_ING_PUBLICA, RZETICO, RZATVEXT,
           RZ_0_17, RZ_18_24, RZ_30_34, RZ_40_49, RZ_BRANCA, RZ_PRETA, RZ_PARDA,
           RZ_AMARELA, RZ_INDIGENA, RZ_FIES, RZ_PROUNIP,
           RZ_PUBLICA, RZ_PRIVADA)

  X$REDE <- ifelse(X$TP_REDE == 1, 1, 0)

  REGIAO <- dummy(as.data.frame(X$NO_REGIAO))[, -1]
  ORGANIZACAOACADEMICA <- dummy(as.data.frame(X$TP_ORG_ACAD))[, -1]

  dados1 <- cbind(X, REGIAO, ORGANIZACAOACADEMICA) |>
    select(-TP_REDE, -TP_ORG_ACAD, -NO_REGIAO, TX_EVASAO, IN_CAPITAL)

  # converte dummies character para numeric
  dados1 <- dados1 |> mutate(across(where(is.character), as.numeric))

  n1 <- sum(dados1$TX_EVASAO == 1)
  if (n1 > 0) {
    message("Removendo ", n1, " linha(s) com TX_EVASAO == 1")
    dados1 <- dados1 %>% filter(TX_EVASAO < 1)
  }

  n_obs      <- nrow(dados1)
  n_zeros    <- sum(dados1$TX_EVASAO == 0)
  prop_zero  <- mean(dados1$TX_EVASAO == 0)

  message("n = ", n_obs, " | zeros = ", n_zeros, " | prop_zero = ", round(prop_zero, 3))

  # ---------- ZIUBXII ----------
  source("C:/Users/Cliente/Documents/ic - renata/ZIUBXII_internal_deriv.R")
  source("C:/Users/Cliente/Documents/ic - renata/ZIKUM.R")
  ZIUBXII_ok <- tryCatch({
    ZIUBXII <- gamlss(
      TX_EVASAO ~ .,
      sigma.formula = ~ 1,
      nu.formula = ~ 1,
      family = ZIUBXII(),
      n.cyc = 500,
      method = CG(),
      data = dados1,
      trace = TRUE
    )
    ZIUBXII
  }, error = function(e) {
    message("ZIUBXII falhou: ", e$message)
    NULL
  })

  # ---------- BETA ----------
  BETA_ok <- tryCatch({
    BETA <- gamlss(
      TX_EVASAO ~ .,
      sigma.formula = ~ 1,
      nu.formula = ~ 1,
      method = CG(),
      mu.step = .1,
      n.cyc = 500,
      family = BEINF0(),
      data = dados1,
      trace = TRUE
    )
    BETA
  }, error = function(e) {
    message("BETA falhou: ", e$message)
    NULL
  })

  # ---------- KUMA ----------
  kuma_ok <- tryCatch({
    kuma <- gamlss(
      TX_EVASAO ~ .,
      sigma.formula = ~ 1,
      family = ZIKUM(mu.link = "logit", sigma.link = "log"),
      data = dados1,
      trace = TRUE
    )
    kuma
  }, error = function(e) {
    message("KUMA falhou: ", e$message)
    NULL
  })

  # ---------- coleta resultados ----------
  linha <- tibble(
    curso          = curso_atual,
    n              = n_obs,
    zeros          = n_zeros,
    prop_zero      = round(prop_zero, 3),

    AIC_ZIUBXII    = if (!is.null(ZIUBXII_ok)) round(AIC(ZIUBXII_ok), 3) else NA,
    AIC_BETA       = if (!is.null(BETA_ok))    round(AIC(BETA_ok),    3) else NA,
    AIC_KUMA       = if (!is.null(kuma_ok))    round(AIC(kuma_ok),    3) else NA,

    BIC_ZIUBXII    = if (!is.null(ZIUBXII_ok)) round(BIC(ZIUBXII_ok), 3) else NA,
    BIC_BETA       = if (!is.null(BETA_ok))    round(BIC(BETA_ok),    3) else NA,
    BIC_KUMA       = if (!is.null(kuma_ok))    round(BIC(kuma_ok),    3) else NA,

    conv_ZIUBXII   = if (!is.null(ZIUBXII_ok)) ZIUBXII_ok$converged else FALSE,
    conv_BETA      = if (!is.null(BETA_ok))    BETA_ok$converged    else FALSE,
    conv_KUMA      = if (!is.null(kuma_ok))    kuma_ok$converged    else FALSE,

    vencedor_AIC   = {
      vals <- c(ZIUBXII = if (!is.null(ZIUBXII_ok)) AIC(ZIUBXII_ok) else Inf,
                BETA    = if (!is.null(BETA_ok))    AIC(BETA_ok)    else Inf,
                KUMA    = if (!is.null(kuma_ok))    AIC(kuma_ok)    else Inf)
      names(which.min(vals))
    },
    vencedor_BIC   = {
      vals <- c(ZIUBXII = if (!is.null(ZIUBXII_ok)) BIC(ZIUBXII_ok) else Inf,
                BETA    = if (!is.null(BETA_ok))    BIC(BETA_ok)    else Inf,
                KUMA    = if (!is.null(kuma_ok))    BIC(kuma_ok)    else Inf)
      names(which.min(vals))
    }
  )

  resultados <- bind_rows(resultados, linha)
}

# ============================================================
# TABELA FINAL
# ============================================================
message("\n====== RESULTADOS FINAIS ======")
print(resultados)

# salva em CSV
write.csv(resultados, "resultados_2.csv", row.names = FALSE)
message("Arquivo salvo: resultados_2.csv")


dados <- read.csv("C:/Users/Cliente/Documents/ic - renata/resultados_ziubxii_intercepto/resultados_2.csv")
dados$AIC_ZIUBXII <- format(dados$AIC_ZIUBXII, scientific = FALSE)
dados$BIC_ZIUBXII <- format(dados$BIC_ZIUBXII, scientific = FALSE)
View(dados)





