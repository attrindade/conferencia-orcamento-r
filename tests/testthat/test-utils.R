# Utilidades de R/utils.R: números e códigos no formato do SIOP e formatação
# brasileira usada no relatório. Testes unitários, sem o pipeline.

test_that("FASES tem as seis fases na ordem do contrato", {
  expect_identical(
    FASES,
    c("projeto_lei", "dotacao_inicial", "dotacao_atual", "empenhado", "liquidado", "pago")
  )
})

test_that("numero_br converte milhar com ponto e decimal com vírgula", {
  expect_equal(numero_br("1.234,56"), 1234.56)
  expect_equal(numero_br("1.234.567.890,12"), 1234567890.12)
  expect_equal(numero_br("1234"), 1234)
  expect_equal(numero_br("0,5"), 0.5)
  expect_equal(numero_br("-1.000,25"), -1000.25)
  expect_equal(numero_br(" 2.000 "), 2000)
})

test_that("numero_br trata vazio e traço como zero, como a referência em Python", {
  expect_equal(numero_br(c("", "-")), c(0, 0))
})

test_that("numero_br mantém NA e avisa quando o texto não é número", {
  expect_true(is.na(numero_br(NA_character_)))
  expect_no_warning(numero_br(NA_character_))
  expect_warning(resultado <- numero_br(c("10,5", "abc")), "formato brasileiro")
  expect_equal(resultado[1], 10.5)
  expect_true(is.na(resultado[2]))
})

test_that("separar_codigo pega o texto antes do primeiro ' - '", {
  expect_identical(separar_codigo("Z101 - Programa Fictício Horizonte"), "Z101")
  # O nome da ação também tem " - ": só o primeiro separador conta.
  expect_identical(separar_codigo("ZA01 - Bolsas Fictícias - Etapa Dois"), "ZA01")
  # Zero à esquerda e letras no código ficam como estão.
  expect_identical(separar_codigo("0Z01 - Ação Fictícia"), "0Z01")
  expect_identical(separar_codigo("  Z2600 - Ministério Fictício do Ensino  "), "Z2600")
  # Sem separador, o próprio texto (aparado).
  expect_identical(separar_codigo(" 3 "), "3")
  # Hífen sem espaços não é separador.
  expect_identical(separar_codigo("Bem-Estar-Fictício"), "Bem-Estar-Fictício")
  expect_identical(separar_codigo(c("1 - A", "2 - B")), c("1", "2"))
})

test_that("separar_nome pega o texto depois do primeiro ' - '", {
  expect_identical(separar_nome("Z101 - Programa Fictício Horizonte"), "Programa Fictício Horizonte")
  expect_identical(separar_nome("ZA01 - Bolsas Fictícias - Etapa Dois"), "Bolsas Fictícias - Etapa Dois")
  expect_identical(separar_nome("Sem código"), "Sem código")
})

test_that("formatar_reais escreve no padrão brasileiro", {
  expect_identical(formatar_reais(1234.56), "R$ 1.234,56")
  expect_identical(formatar_reais(0.5), "R$ 0,50")
  expect_identical(formatar_reais(123456789012.3), "R$ 123.456.789.012,30")
  expect_identical(formatar_reais(-1234.5), "-R$ 1.234,50")
  expect_identical(formatar_reais(1234.56, casas = 0), "R$ 1.235")
  expect_true(is.na(formatar_reais(NA_real_)))
})

test_that("formatar_pct, formatar_data e juntar_com_e escrevem no padrão brasileiro", {
  expect_identical(formatar_pct(1.5), "1,50%")
  expect_identical(formatar_pct(0.346, casas = 1), "0,3%")
  expect_true(is.na(formatar_pct(NA_real_)))
  expect_identical(formatar_data(as.Date("2026-10-29")), "29/10/2026")
  expect_true(is.na(formatar_data(as.Date(NA))))
  expect_identical(juntar_com_e(c("1", "3", "4", "5")), "1, 3, 4 e 5")
  expect_identical(juntar_com_e("3"), "3")
})

test_that("carregar_config resolve caminhos relativos contra a raiz do projeto", {
  expect_identical(
    normalizePath(CFG$caminhos$gabaritos, winslash = "/", mustWork = FALSE),
    normalizePath(file.path(RAIZ, "config/gabaritos.csv"), winslash = "/", mustWork = FALSE)
  )
  expect_true(file.exists(CFG$caminhos$recorte_acoes))
  # Relativo ou não no config.yml, o caminho carregado é absoluto.
  expect_match(CFG$caminhos$ipca, "^([A-Za-z]:)?/")
  expect_error(carregar_config(file.path(tempdir(), "nao_existe.yml")), "configura")
})

test_that("a configuração tem as chaves e os valores coerentes que o pipeline usa", {
  # Tolerâncias fixadas antes de rodar (as mesmas na conferência privada e na vitrine).
  expect_equal(CFG$tolerancias$reais, 1)
  expect_equal(CFG$tolerancias$pontos_percentuais, 0.05)
  expect_equal(CFG$tolerancias$contagem, 0)
  # A trava do IPCA só faz sentido se o fator certo e o errado forem diferentes.
  expect_type(FATOR_CERTO, "double")
  expect_type(FATOR_ERRADO, "double")
  expect_gt(abs(FATOR_CERTO - FATOR_ERRADO), TOLERANCIA_FATOR)
  p <- CFG$periodo
  expect_true(p$ano_inicio <= p$ultimo_ano_safra_antiga && p$ultimo_ano_safra_antiga <= p$ano_fim)
  expect_true(ANO_IPCA >= p$ano_inicio && ANO_IPCA <= p$ano_fim)
  expect_true(length(unlist(CFG$denominador$gnd_incluidos)) > 0)
  expect_type(ACOES_OBRIGATORIAS, "character")
  expect_true(file.exists(CFG$caminhos$causas_conhecidas))
})

test_that("registrar_procedencia guarda tamanho e hash e recusa arquivo ausente", {
  arq <- tempfile(fileext = ".csv")
  gravar_utf8(c("a,b", "1,2"), arq)
  proc <- registrar_procedencia(c(teste = arq))
  expect_identical(names(proc), c("papel", "arquivo", "caminho", "tamanho_bytes", "modificado_em", "sha256", "registrado_em"))
  expect_identical(proc$papel, "teste")
  expect_equal(proc$tamanho_bytes, file.size(arq))
  expect_match(proc$sha256, "^[0-9a-f]{64}$")
  expect_s3_class(proc$modificado_em, "POSIXct")
  expect_error(registrar_procedencia(c(sumido = file.path(tempdir(), "nao_existe.csv"))), "sumido")
})
