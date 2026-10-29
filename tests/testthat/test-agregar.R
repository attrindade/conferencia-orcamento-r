# Agregações da base deflacionada (R/agregar.R).

# Base real minúscula: dois anos, dois programas, duas ações, dois GND.
# Nas colunas reais o fator é 2 em 2022 e 1 em 2023, para que nominal e real
# sejam diferentes e uma troca entre eles apareça no teste.
base_real_mini <- function() {
  b <- tibble::tribble(
    ~ano,  ~acao_cod, ~programa_cod, ~programa_nome,      ~gnd_cod, ~valor,
    2022L, "ZA01",    "Z101",        "Fictício A",        "3",      100,
    2022L, "ZM01",    "Z199",        "Fictício Alicerce", "5",      10,
    2023L, "ZA01",    "Z101",        "Fictício B",        "3",      300,
    2023L, "ZM01",    "Z199",        "Fictício Alicerce", "5",      600
  )
  fator <- ifelse(b$ano == 2022L, 2, 1)
  for (f in FASES) {
    # A dotação atual é o dobro do empenhado, para a taxa de execução dar 50%.
    peso <- if (f == "dotacao_atual") 2 else 1
    b[[f]] <- b$valor * peso
    b[[paste0(f, "_real")]] <- b$valor * peso * fator
  }
  dplyr::select(b, -"valor")
}

test_that("totais_por_ano soma nominal e real por ano e fase", {
  t <- totais_por_ano(base_real_mini())
  expect_identical(names(t), c("ano", "fase", "nominal", "real"))
  expect_identical(t$fase, rep(FASES, 2))
  expect_equal(t$nominal[t$ano == 2022L & t$fase == "pago"], 110)
  expect_equal(t$real[t$ano == 2022L & t$fase == "pago"], 220)
  expect_equal(t$real[t$ano == 2023L & t$fase == "dotacao_atual"], 1800)
})

test_that("soma por fase = soma das linhas, por qualquer caminho de agregação", {
  b <- base_real_mini()
  per <- totais_periodo(totais_por_ano(b))
  gnd <- totais_por_gnd(b)
  acao <- totais_por_acao_ano(b)
  for (f in FASES) {
    linhas_real <- sum(b[[paste0(f, "_real")]])
    expect_equal(per$real[per$fase == f], linhas_real)
    expect_equal(per$nominal[per$fase == f], sum(b[[f]]))
    expect_equal(sum(gnd$real[gnd$fase == f]), linhas_real)
    expect_equal(sum(acao$real[acao$fase == f]), linhas_real)
    expect_equal(sum(gnd$pct_real[gnd$fase == f]), 100)
  }
  expect_equal(per$media_anual_real[per$fase == "pago"], (220 + 900) / 2)
})

test_that("valor ausente aparece como NA no total, em vez de sumir", {
  b <- base_real_mini()
  b$pago_real[1] <- NA
  t <- totais_por_ano(b)
  expect_true(is.na(t$real[t$ano == 2022L & t$fase == "pago"]))
  expect_false(is.na(t$real[t$ano == 2023L & t$fase == "pago"]))
  expect_true(is.na(totais_periodo(t)$real[FASES == "pago"]))
})

test_that("taxa_execucao tem uma linha por ano e uma do período (ano = NA)", {
  tx <- taxa_execucao(totais_por_ano(base_real_mini()))
  expect_identical(names(tx), c("ano", "empenhado_real", "dotacao_atual_real", "taxa_pct"))
  expect_equal(nrow(tx), 3)
  expect_equal(sum(is.na(tx$ano)), 1)
  expect_equal(tx$taxa_pct, rep(50, 3))
  expect_equal(tx$empenhado_real[is.na(tx$ano)], 220 + 900)
})

test_that("participacao_executivo divide nominal por nominal e leva a marca de estimado", {
  t <- totais_por_ano(base_real_mini())
  den <- tibble::tibble(ano = rep(c(2022L, 2023L), each = 6), fase = rep(FASES, 2),
                        executivo_nominal = rep(c(11000, 90000), each = 6), estimado = rep(c(FALSE, TRUE), each = 6))
  p <- participacao_executivo(t, den, fase = "empenhado")
  expect_identical(names(p), c("ano", "empenhado_juventude", "empenhado_executivo", "participacao_pct", "estimado"))
  expect_equal(p$participacao_pct, c(1, 1))
  expect_identical(p$estimado, c(FALSE, TRUE))
})

test_that("concentracao_programas ordena por valor e conta quantos chegam ao limiar", {
  conc <- concentracao_programas(base_real_mini(), cfg_mini(2022, 2023))
  expect_identical(names(conc$tabela), c("programa_cod", "programa_nome", "valor_real", "pct", "pct_acumulado", "posicao"))
  # Dotação atual real: Z199 = 20×2 + 1200 = 1240; Z101 = 400 + 600 = 1000.
  expect_identical(conc$tabela$programa_cod, c("Z199", "Z101"))
  expect_equal(conc$tabela$valor_real, c(1240, 1000))
  expect_equal(conc$tabela$pct_acumulado[2], 100)
  # Nome do ano mais recente.
  expect_identical(conc$tabela$programa_nome[conc$tabela$programa_cod == "Z101"], "Fictício B")
  expect_equal(conc$n_ate_limiar, 2)
  expect_equal(n_ate_limiar(c(50, 30, 20), 0.8), 2)
  expect_equal(n_ate_limiar(c(50, 29, 21), 0.8), 3)
})

test_that("contagens e ranking_anos contam por código e ordenam pelo valor real", {
  b <- base_real_mini()
  n <- contagens(b)
  expect_equal(n$n_programas, 2)
  expect_equal(n$n_acoes, 2)
  expect_equal(n$n_linhas, 4)
  # Nominal: 2023 maior. Real (fator 2 em 2022): 2022 = 440, 2023 = 1800.
  expect_identical(ranking_anos(totais_por_ano(b), fase = "dotacao_atual", n = 2), c(2023L, 2022L))
  b$dotacao_atual_real[b$ano == 2022L] <- b$dotacao_atual_real[b$ano == 2022L] * 10
  expect_identical(ranking_anos(totais_por_ano(b), fase = "dotacao_atual", n = 1), 2022L)
})

# ---- integração ------------------------------------------------------------

test_that("soma por fase = soma das linhas na base real do pipeline", {
  base_real <- ler_alvo("base_real")
  per <- ler_alvo("totais_periodo")
  ano <- ler_alvo("totais_ano")
  gnd <- ler_alvo("totais_gnd")
  acao <- ler_alvo("totais_acao_ano")
  tol <- CFG$tolerancias$reais
  for (f in FASES) {
    linhas_real <- sum(base_real[[paste0(f, "_real")]])
    linhas_nominal <- sum(base_real[[f]])
    expect_lte(abs(per$real[per$fase == f] - linhas_real), tol)
    expect_lte(abs(per$nominal[per$fase == f] - linhas_nominal), tol)
    expect_lte(abs(sum(ano$real[ano$fase == f]) - linhas_real), tol)
    expect_lte(abs(sum(gnd$real[gnd$fase == f]) - linhas_real), tol)
    expect_lte(abs(sum(acao$real[acao$fase == f]) - linhas_real), tol)
    expect_lte(abs(sum(acao$nominal[acao$fase == f]) - linhas_nominal), tol)
  }
  # Todos os anos do período × 6 fases, sem buraco.
  n_anos <- CFG$periodo$ano_fim - CFG$periodo$ano_inicio + 1
  expect_equal(nrow(ano), n_anos * length(FASES))
  expect_false(anyNA(ano$real))
})

test_that("contagens do pipeline batem com os gabaritos de programas e de ações por código", {
  n <- ler_alvo("contagens")
  gab <- ler_alvo("gabaritos")
  for (m in c("n_programas", "n_acoes")) {
    esperado <- as.numeric(gab$valor_esperado[gab$metrica == m])
    skip_if(length(esperado) == 0, paste("Sem gabarito de", m))
    expect_equal(n[[m]], esperado[1], label = m)
  }
})

test_that("participação do pipeline: só os anos depois da safra antiga têm denominador estimado", {
  p <- ler_alvo("participacao")
  ultimo <- CFG$periodo$ultimo_ano_safra_antiga
  expect_identical(p$ano, seq(as.integer(CFG$periodo$ano_inicio), as.integer(CFG$periodo$ano_fim)))
  expect_identical(p$ano[p$estimado], p$ano[p$ano > ultimo])
  expect_false(anyNA(p$participacao_pct))
})
