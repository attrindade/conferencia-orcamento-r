# Recorte de ações e deflação pelo IPCA (R/recorte.R e R/deflacionar.R).

base_com_acoes <- function(acoes, anos = rep(ANO_IPCA, length(acoes)), valores = seq_along(acoes)) {
  dplyr::bind_cols(
    tibble::tibble(ano = as.integer(anos), acao_cod = acoes),
    purrr::map_dfr(valores, fases_iguais)
  )
}

recorte_minimo <- function(acoes, origem = rep("palavra-chave", length(acoes))) {
  tibble::tibble(acao_cod = acoes, acao_nome = paste("Ação", acoes), programas = "Z101",
                 origem = origem, primeiro_ano = 2005L, ultimo_ano = 2025L, n_linhas = 1L)
}

# Tabela de fatores com o ano anterior ao conferido e o próprio ano conferido,
# este com o fator certo da configuração.
ipca_valido <- function(anos = c(ANO_IPCA - 1L, ANO_IPCA), fatores = c(1.15, FATOR_CERTO)) {
  tibble::tibble(ano = as.integer(anos), fator = fatores)
}

test_that("aplicar_recorte mantém só as ações do recorte e não duplica linha", {
  base <- base_com_acoes(c("ZA01", "ZA01", "ZM01", "ZX99"))
  rec <- recorte_minimo(c("ZA01", "ZM01", "ZY88"))
  depois <- suppressMessages(aplicar_recorte(base, rec))
  expect_identical(depois$acao_cod, c("ZA01", "ZA01", "ZM01"))
  expect_identical(names(depois), names(base))
  expect_message(aplicar_recorte(base, rec), "Recorte aplicado")
})

test_that("resumir_recorte conta o que ficou de fora e aponta obrigatória ausente", {
  base <- base_com_acoes(c("ZA01", "ZA01", "ZX99"))
  rec <- recorte_minimo(c("ZA01", "ZY88"), origem = c("palavra-chave", "inclusão manual"))
  depois <- suppressMessages(aplicar_recorte(base, rec))
  res <- resumir_recorte(base, depois, rec, acoes_obrigatorias = c("ZM01", "ZA01"))
  expect_identical(names(res), c("linhas_antes", "linhas_depois", "linhas_removidas", "acoes_no_recorte",
                                 "acoes_na_base", "acoes_da_base_fora_do_recorte", "acoes_do_recorte_sem_linha",
                                 "inclusoes_manuais", "obrigatorias_ausentes"))
  expect_equal(res$linhas_removidas, 1)
  expect_equal(res$acoes_da_base_fora_do_recorte, 1)
  expect_equal(res$acoes_do_recorte_sem_linha, 1)
  expect_equal(res$inclusoes_manuais, 1)
  expect_identical(res$obrigatorias_ausentes, "ZM01")
  completo <- resumir_recorte(base, depois, rec, acoes_obrigatorias = "ZA01")
  expect_true(is.na(completo$obrigatorias_ausentes))
})

test_that("ler_recorte preserva o zero à esquerda e recusa ação repetida", {
  arq <- tempfile(fileext = ".csv")
  cab <- "acao_cod,acao_nome,programas,origem,primeiro_ano,ultimo_ano,n_linhas"
  gravar_utf8(c(cab, "0Z01,Bolsas Fictícias,Z101,palavra-chave,2005,2007,250",
                "ZM01,Incentivo Fictício,Z199,inclusão manual,2023,2025,9"), arq)
  rec <- ler_recorte(arq)
  expect_identical(rec$acao_cod, c("0Z01", "ZM01"))
  expect_type(rec$primeiro_ano, "integer")
  gravar_utf8(c(cab, "0Z01,A,Z101,palavra-chave,2005,2007,1", "0Z01,B,Z101,palavra-chave,2005,2007,1"), arq)
  expect_error(ler_recorte(arq), "0Z01")
  gravar_utf8(c("acao_cod,acao_nome", "0Z01,A"), arq)
  expect_error(ler_recorte(arq), "Faltam")
})

test_that("conferir_fator_ipca aceita o fator certo do ano conferido (config.yml)", {
  cfg <- cfg_mini(ANO_IPCA - 1L, ANO_IPCA, ano_conferido = ANO_IPCA)
  expect_no_error(suppressMessages(conferir_fator_ipca(ipca_valido(), cfg)))
})

test_that("conferir_fator_ipca recusa o fator errado conhecido (config.yml)", {
  # Período de um ano só, para que a recusa venha do fator e não de ano faltando.
  cfg <- cfg_mini(ANO_IPCA, ANO_IPCA, ano_conferido = ANO_IPCA)
  expect_error(conferir_fator_ipca(ipca_valido(ANO_IPCA, FATOR_ERRADO), cfg), "antigo")
})

test_that("conferir_fator_ipca recusa fator diferente do esperado e ano sem fator", {
  cfg <- cfg_mini(ANO_IPCA, ANO_IPCA, ano_conferido = ANO_IPCA)
  # Um milionésimo acima do certo: passa da tolerância de 1e-7.
  expect_error(conferir_fator_ipca(ipca_valido(ANO_IPCA, FATOR_CERTO + 1e-6), cfg), "esperado")
  cfg2 <- cfg_mini(ANO_IPCA - 2L, ANO_IPCA, ano_conferido = ANO_IPCA)
  expect_error(conferir_fator_ipca(ipca_valido(), cfg2), as.character(ANO_IPCA - 2L))
  repetido <- ipca_valido(c(ANO_IPCA - 1L, ANO_IPCA - 1L, ANO_IPCA), c(1, 1, FATOR_CERTO))
  expect_error(conferir_fator_ipca(repetido, cfg2), "repetido")
})

test_that("deflacionar multiplica cada fase pelo fator do ano, sem perder linha", {
  cfg <- cfg_mini(ANO_IPCA - 1L, ANO_IPCA, ano_conferido = ANO_IPCA)
  base <- base_com_acoes(c("ZA01", "ZM01", "ZA01"), anos = c(ANO_IPCA - 1L, ANO_IPCA, ANO_IPCA),
                         valores = c(100, 200, 300))
  real <- suppressMessages(deflacionar(base, ipca_valido(), cfg))
  expect_equal(nrow(real), nrow(base))
  expect_identical(names(real), c(names(base), paste0(FASES, "_real")))
  expect_equal(real$pago_real, c(100 * 1.15, 200 * FATOR_CERTO, 300 * FATOR_CERTO))
  expect_equal(real$projeto_lei_real, real$projeto_lei * c(1.15, FATOR_CERTO, FATOR_CERTO))
  # Os valores nominais não mudam.
  expect_equal(real$pago, base$pago)
})

test_that("deflacionar para quando a base tem ano sem fator ou o fator é o antigo", {
  cfg <- cfg_mini(ANO_IPCA - 1L, ANO_IPCA, ano_conferido = ANO_IPCA)
  fora <- base_com_acoes("ZA01", anos = ANO_IPCA - 4L)
  expect_error(suppressMessages(deflacionar(fora, ipca_valido(), cfg)), as.character(ANO_IPCA - 4L))
  errado <- ipca_valido(fatores = c(1.15, FATOR_ERRADO))
  expect_error(deflacionar(base_com_acoes("ZA01"), errado, cfg), "antigo")
})

# ---- integração ------------------------------------------------------------

test_that("o fator do IPCA do ano conferido usado pelo pipeline é o da configuração", {
  ipca <- ler_alvo("ipca")
  f <- ipca$fator[ipca$ano == ANO_IPCA]
  expect_length(f, 1)
  # Mesma tolerância de conferir_fator_ipca(): 1e-7.
  expect_lte(abs(f - FATOR_CERTO), TOLERANCIA_FATOR)
  expect_gt(abs(f - FATOR_ERRADO), TOLERANCIA_FATOR)
  expect_true(all(seq(CFG$periodo$ano_inicio, CFG$periodo$ano_fim) %in% ipca$ano))
})

test_that("as ações obrigatórias estão no recorte, como inclusão manual, e na base (pipeline)", {
  rec <- ler_alvo("recorte")
  base_real <- ler_alvo("base_real")
  for (acao in ACOES_OBRIGATORIAS) {
    expect_true(acao %in% rec$acao_cod, label = acao)
    expect_identical(rec$origem[rec$acao_cod == acao], "inclusão manual", label = acao)
    expect_true(acao %in% base_real$acao_cod, label = acao)
    # A ação só aparece nos anos que a lista do recorte registra para ela.
    anos <- base_real$ano[base_real$acao_cod == acao]
    expect_true(all(anos >= rec$primeiro_ano[rec$acao_cod == acao] & anos <= rec$ultimo_ano[rec$acao_cod == acao]),
                label = acao)
  }
  res <- ler_alvo("resumo_recorte")
  expect_true(is.na(res$obrigatorias_ausentes))
})

test_that("o recorte não perde linha nem ação até a base real", {
  base_juventude <- ler_alvo("base_juventude")
  base_recortada <- ler_alvo("base_recortada")
  base_real <- ler_alvo("base_real")
  res <- ler_alvo("resumo_recorte")
  expect_equal(res$linhas_removidas, 0)
  expect_equal(res$acoes_da_base_fora_do_recorte, 0)
  expect_equal(nrow(base_recortada), nrow(base_juventude))
  expect_equal(nrow(base_real), nrow(base_recortada))
  expect_identical(range(base_real$ano), as.integer(c(CFG$periodo$ano_inicio, CFG$periodo$ano_fim)))
  # Toda fase real existe e não tem NA (NA sumiria das somas do relatório).
  for (fase in FASES) {
    expect_false(anyNA(base_real[[paste0(fase, "_real")]]), label = paste("NA em", fase, "real"))
    expect_equal(sum(base_real[[fase]]), sum(base_juventude[[fase]]), tolerance = 0)
  }
})

test_that("recorte_na_uniao (pipeline) mostra que o recorte bate por programa e ação só nos primeiros anos", {
  rnu <- ler_alvo("recorte_na_uniao")
  expect_identical(names(rnu), c("ano", "valor_juventude", "valor_uniao_mesma_acao",
                                 "valor_uniao_mesmo_programa_acao", "excesso_acao", "excesso_programa_acao"))
  tol <- CFG$tolerancias$reais
  ate <- CFG$testes$recorte_bate_na_uniao_ate
  skip_if(is.null(ate), "Sem testes: recorte_bate_na_uniao_ate em config.yml.")
  primeiros <- rnu[rnu$ano <= ate, ]
  # Por (ano, programa, ação) bate ao centavo até o ano medido.
  expect_lte(max(abs(primeiros$excesso_programa_acao)), tol)
  # E passa a sobrar no ano seguinte.
  expect_gt(abs(rnu$excesso_programa_acao[rnu$ano == ate + 1L]), tol)
  # Por (ano, ação) já sobra no primeiro ano (ações genéricas de outros programas).
  expect_gt(rnu$excesso_acao[rnu$ano == CFG$periodo$ano_inicio], tol)
})
