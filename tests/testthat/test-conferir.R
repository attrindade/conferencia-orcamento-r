# Conferência contra os gabaritos e veredito (R/conferir.R).

# Números de controle que a configuração manda conferir (testes: ancoras em
# config.yml), com o valor copiado da fonte original. Vazio quando a
# configuração não traz nenhum.
ANCORAS <- unlist(CFG$testes$ancoras)
if (is.null(ANCORAS)) ANCORAS <- character()

test_that("gabaritos.csv é lido sem erro e tem os números de controle com os mesmos valores", {
  gab <- ler_gabaritos(CFG$caminhos$gabaritos)
  expect_identical(names(gab), c("id", "descricao", "fonte", "metrica", "fase", "ano", "filtro",
                                 "unidade", "valor_esperado", "observacao"))
  expect_false(anyDuplicated(gab$id) > 0)
  expect_length(setdiff(names(ANCORAS), gab$id), 0)
  for (id in intersect(names(ANCORAS), gab$id)) {
    valor <- gab$valor_esperado[gab$id == id]
    if (gab$unidade[gab$id == id] == "lista") {
      expect_identical(valor, ANCORAS[[id]], label = id)
    } else {
      expect_equal(as.numeric(valor), as.numeric(ANCORAS[[id]]), label = id)
    }
  }
  # Nenhum gabarito com os prefixos de id proibidos na configuração.
  for (prefixo in unlist(CFG$testes$prefixos_de_id_proibidos)) {
    expect_false(any(startsWith(gab$id, prefixo)), label = prefixo)
  }
})

test_that("ler_gabaritos aceita as unidades do contrato, inclusive fator, e recusa as outras", {
  arq <- tempfile(fileext = ".csv")
  cab <- "id,descricao,fonte,metrica,fase,ano,filtro,unidade,valor_esperado,observacao"
  gravar_utf8(c(cab, "a,d,f,total_ano,pago,2010,,fator,1.0811234,",
                "b,d,f,ano_maximo_participacao,empenhado,,,lista,2010,"), arq)
  gab <- ler_gabaritos(arq)
  expect_identical(gab$unidade, c("fator", "lista"))
  expect_equal(tolerancia_da_unidade(c("fator", "lista"), CFG), c(TOLERANCIA_FATOR, 0))
  gravar_utf8(c(cab, "a,d,f,total_ano,pago,2010,,metro,1,"), arq)
  expect_error(ler_gabaritos(arq), "Unidade desconhecida")
})

test_that("linha_divergencia decide confere ou diverge pela tolerância", {
  ok <- linha_divergencia("a", "d", "f", "reais", 100.9, 100, 1)
  expect_identical(names(ok), c("id", "descricao", "fonte", "unidade", "valor_r", "valor_gabarito", "diferenca",
                                "tolerancia", "dentro_tolerancia", "status", "provavel_causa", "encaminhamento"))
  expect_identical(ok$status, "confere")
  expect_true(ok$dentro_tolerancia)
  fora <- linha_divergencia("b", "d", "f", "reais", 101.5, 100, 1)
  expect_identical(fora$status, "diverge")
  expect_false(fora$dentro_tolerancia)
  # A fronteira exata (diferença = tolerância) confere, mesmo com ruído de ponto flutuante.
  expect_identical(linha_divergencia("c", "d", "f", "pct", 0.35, 0.3, 0.05)$status, "confere")
  # Sem valor no R não confere.
  expect_identical(linha_divergencia("e", "d", "f", "reais", NA_real_, 100, 1)$status, "diverge")
})

test_that("veredito conta conferidos, explicadas e divergências sem explicação", {
  div <- dplyr::bind_rows(
    linha_divergencia("a", "d", "f", "reais", 1, 1, 1),
    linha_divergencia("b", "d", "f", "reais", 1, 1, 1),
    linha_divergencia("c", "d", "f", "reais", 9, 1, 1, status = "explicada", provavel_causa = "causa"),
    linha_divergencia("d", "d", "f", "reais", 9, 1, 1)
  )
  v <- veredito(div)
  expect_equal(v$total, 4)
  expect_equal(v$conferidos, 2)
  expect_equal(v$explicadas, 1)
  expect_equal(v$divergencias, 1)
  expect_identical(v$texto, "4 números comparados: 2 conferem, 1 tem diferença explicada e 1 ficou sem explicação")
  # Sem nenhuma pendente, a frase diz "nenhum".
  v0 <- veredito(div[1:3, ])
  expect_identical(v0$texto, "3 números comparados: 2 conferem, 1 tem diferença explicada e nenhum ficou sem explicação")
})

test_that("ler_causas_conhecidas lê o YAML e recusa causa sem texto", {
  arq <- tempfile(fileext = ".yml")
  gravar_utf8(c(
    "z_explicada:",
    "  anos: [2010, 2011]",
    "  acoes: [\"0Z01\"]",
    "  provavel_causa: \"Causa fictícia verificada.\"",
    "  encaminhamento: \"Nada a fazer.\""
  ), arq)
  causas <- ler_causas_conhecidas(arq)
  expect_identical(causas$id, "z_explicada")
  expect_identical(causas$anos[[1]], c(2010L, 2011L))
  expect_identical(causas$acoes[[1]], "0Z01")
  vazia <- suppressMessages(ler_causas_conhecidas(""))
  expect_equal(nrow(vazia), 0)
  gravar_utf8(c("z_sem_texto:", "  provavel_causa: \"Só a causa.\""), arq)
  expect_error(ler_causas_conhecidas(arq), "encaminhamento")
})

base_real_conferir <- function() {
  b <- tibble::tibble(ano = c(2022L, 2023L), acao_cod = "ZA01", programa_cod = "Z101",
                      programa_nome = "P", gnd_cod = "3")
  for (f in FASES) {
    b[[f]] <- c(100, 900)
    b[[paste0(f, "_real")]] <- c(200, 900)
  }
  b
}

causas_teste <- function() {
  tibble::tibble(id = c("z_explicada", "z_ano_maximo"), provavel_causa = c("causa a", "causa b"),
                 encaminhamento = c("fazer a", "fazer b"), anos = list(integer(), integer()),
                 acoes = list(character(), character()))
}

test_that("conferir_gabaritos só explica divergência com causa registrada antes", {
  gab <- tibble::tibble(
    id = c("total_2023", "z_explicada", "desconhecido", "ranking", "sem_linha"),
    descricao = "d", fonte = "f",
    metrica = c("total_ano", "n_programas_limiar", "total_ano", "ranking_dotacao", "total_ano"),
    fase = c("pago", "dotacao_atual", "pago", "dotacao_atual", "pago"),
    ano = c(2023L, NA, 2022L, NA, 1999L),
    filtro = NA_character_,
    unidade = c("reais", "contagem", "reais", "lista", "reais"),
    valor_esperado = c("900.4", "6", "999", "2023|2022", "1"),
    observacao = NA_character_
  )
  totais <- totais_por_ano(base_real_conferir())
  resultados <- list(
    totais_ano = totais,
    concentracao = list(n_ate_limiar = 7L, n_ate_limiar_pago = 7L, fase = "dotacao_atual")
  )
  div <- conferir_gabaritos(gab, resultados, CFG, causas = causas_teste())
  expect_identical(div$id, gab$id)
  st <- stats::setNames(div$status, div$id)
  expect_identical(st[["total_2023"]], "confere")
  expect_identical(st[["z_explicada"]], "explicada")
  expect_identical(div$provavel_causa[div$id == "z_explicada"], "causa a")
  expect_identical(st[["desconhecido"]], "diverge")
  expect_match(div$provavel_causa[div$id == "desconhecido"], "Sem causa")
  expect_identical(st[["ranking"]], "confere")
  expect_identical(div$diferenca[div$id == "ranking"], 0)
  expect_identical(st[["sem_linha"]], "diverge")
})

test_that("ano_maximo_participacao confere o ano do máximo, e não só o valor", {
  # Números inventados, sem relação com nenhuma série real.
  participacao <- tibble::tibble(ano = 2010:2013, participacao_pct = c(2.4137, 2.5821, 2.6093, 2.5012))
  gab <- tibble::tibble(
    id = c("z_ano_maximo", "certo", "sem_causa"), descricao = "Ano de maior participação", fonte = "f",
    metrica = "ano_maximo_participacao", fase = "empenhado", ano = NA_integer_, filtro = NA_character_,
    unidade = "lista", valor_esperado = c("2011", "2012", "2011"), observacao = NA_character_
  )
  expect_identical(calcular_metrica(gab[1, ], list(participacao = participacao)), "2012")
  div <- conferir_gabaritos(gab, list(participacao = participacao), CFG, causas = causas_teste())
  st <- stats::setNames(div$status, div$id)
  expect_identical(st[["z_ano_maximo"]], "explicada")
  expect_identical(st[["certo"]], "confere")
  expect_identical(st[["sem_causa"]], "diverge")
  # A descrição leva as duas listas, que o relatório separa em colunas.
  expect_match(div$descricao[div$id == "certo"], "(R: 2012; gabarito: 2012)", fixed = TRUE)
  expect_equal(div$diferenca[div$id == "sem_causa"], 1)
})

test_that("causa registrada só explica o valor que ela descreve (casos adulterados)", {
  # Causas fictícias com os campos de verificação: o valor esperado de uma
  # contagem, o ano esperado de uma lista e a maior diferença coberta.
  causas <- normalizar_causas(tibble::tibble(
    id = c("z_contagem", "z_ano", "z_reais"),
    provavel_causa = c("causa da contagem", "causa do ano", "causa dos reais"),
    encaminhamento = "fazer algo",
    valor_r_esperado = c("7", "2012", NA),
    max_diferenca = c(NA, NA, 50)
  ))
  gab <- tibble::tibble(
    id = c("z_contagem", "z_ano", "z_reais"), descricao = "d", fonte = "f",
    metrica = c("n_programas_limiar", "ano_maximo_participacao", "total_ano"),
    fase = c("dotacao_atual", "empenhado", "pago"), ano = c(NA, NA, 2023L), filtro = NA_character_,
    unidade = c("contagem", "lista", "reais"), valor_esperado = c("6", "2011", "860"),
    observacao = NA_character_
  )
  participacao <- tibble::tibble(ano = 2010:2013, participacao_pct = c(2.4137, 2.5821, 2.6093, 2.5012))
  resultados <- function(n_limiar, part, base) list(
    concentracao = list(n_ate_limiar = n_limiar, n_ate_limiar_pago = n_limiar, fase = "dotacao_atual"),
    participacao = part, totais_ano = totais_por_ano(base)
  )
  # Valores que a causa descreve: as três linhas ficam explicadas (900 - 860 = 40, até 50).
  ok <- conferir_gabaritos(gab, resultados(7L, participacao, base_real_conferir()), CFG, causas = causas)
  expect_identical(ok$status, rep("explicada", 3))
  # Adulterados: 15 programas em vez de 7, máximo em 2010 em vez de 2012 e
  # diferença de 9.140 (pago real de 2023 = 10.000), acima dos 50 cobertos.
  part_adulterada <- dplyr::mutate(participacao, participacao_pct = c(9.9999, 2.5821, 2.6093, 2.5012))
  base_adulterada <- base_real_conferir()
  base_adulterada$pago_real[2] <- 10000
  ruim <- conferir_gabaritos(gab, resultados(15L, part_adulterada, base_adulterada), CFG, causas = causas)
  expect_identical(ruim$status, rep("diverge", 3))
  expect_true(all(startsWith(ruim$provavel_causa, "Causa registrada não cobre o valor atual")))
  expect_match(ruim$provavel_causa[1], "descreve o valor 7 e o R obteve 15", fixed = TRUE)
  expect_match(ruim$provavel_causa[2], "descreve 2012 e o R obteve 2010", fixed = TRUE)
  expect_match(ruim$encaminhamento[1], "Pendente de decisão", fixed = TRUE)
})

test_that("checar_safras_juventude só explica anos, ações e tamanho já registrados", {
  cfg <- cfg_mini(2010, 2011)
  comp <- tibble::tibble(ano = c(2010L, 2011L), fase = "dotacao_atual", valor_nova = c(100, 130),
                         valor_antiga = c(100, 100), diferenca = c(0, 30), n_linhas_nova = c(1L, 2L),
                         n_linhas_antiga = c(1L, 1L), identico = c(TRUE, FALSE))
  dif <- tibble::tibble(ano = 2011L, programa_cod = "Z101", acao_cod = "ZA01")
  causas <- normalizar_causas(tibble::tibble(
    id = "estrutura_juventude_safras", provavel_causa = "linhas a mais", encaminhamento = "nada",
    anos = list(2011L), acoes = list("ZA01"), max_diferenca = 40
  ))
  expect_identical(checar_safras_juventude(comp, cfg, causas, diferencas = dif)$status, "explicada")
  # Ação nova que a causa não cita.
  dif2 <- tibble::tibble(ano = 2011L, programa_cod = "Z101", acao_cod = c("ZA01", "ZB02"))
  lin <- checar_safras_juventude(comp, cfg, causas, diferencas = dif2)
  expect_identical(lin$status, "diverge")
  expect_match(lin$provavel_causa, "ZB02", fixed = TRUE)
  # Diferença maior do que a registrada.
  comp_grande <- dplyr::mutate(comp, diferenca = c(0, 3000))
  expect_identical(checar_safras_juventude(comp_grande, cfg, causas, diferencas = dif)$status, "diverge")
  # Ano que a causa não cita.
  comp_ano <- dplyr::mutate(comp, identico = c(FALSE, FALSE), diferenca = c(5, 30))
  expect_identical(checar_safras_juventude(comp_ano, cfg, causas, diferencas = dif)$status, "diverge")
})

# ---- integração ------------------------------------------------------------

test_that("divergencias (pipeline) tem as colunas e os status da seção 8", {
  div <- ler_alvo("divergencias")
  expect_identical(names(div), c("id", "descricao", "fonte", "unidade", "valor_r", "valor_gabarito", "diferenca",
                                 "tolerancia", "dentro_tolerancia", "status", "provavel_causa", "encaminhamento"))
  expect_true(all(div$status %in% c("confere", "diverge", "explicada")))
  expect_true(all(div$unidade %in% UNIDADES))
  expect_false(anyDuplicated(div$id) > 0)
  # Cada gabarito vira uma linha.
  gab <- ler_alvo("gabaritos")
  expect_length(setdiff(gab$id, div$id), 0)
  # "confere" só dentro da tolerância; "explicada" sempre com causa escrita.
  expect_true(all(div$dentro_tolerancia[div$status == "confere"]))
  explicadas <- div[div$status == "explicada", ]
  expect_true(all(!is.na(explicadas$provavel_causa) & nzchar(explicadas$provavel_causa)))
  # Todo gabarito explicado tem a causa registrada em causas_conhecidas.yml.
  explicados_gab <- intersect(explicadas$id, gab$id)
  expect_length(setdiff(explicados_gab, CAUSAS$id), 0)
})

test_that("cada número de controle da configuração confere no R", {
  skip_if(length(ANCORAS) == 0, "Sem testes: ancoras em config.yml.")
  div <- ler_alvo("divergencias")
  # Primeiro: todos os ids existem. Sem isso, um id errado passaria em silêncio.
  expect_length(setdiff(names(ANCORAS), div$id), 0)
  for (id in names(ANCORAS)) {
    expect_identical(div$status[div$id == id], "confere", label = id)
  }
})

test_that("as checagens estruturais estão em divergencias e conferem", {
  div <- ler_alvo("divergencias")
  st <- stats::setNames(div$status, div$id)
  confere <- c(
    "estrutura_uniao_safras_identicas",   # identidade da União até o último ano completo
    "estrutura_recorte_cobre_base",
    "estrutura_acoes_obrigatorias",
    "estrutura_soma_por_fase",
    "estrutura_denominador_python",
    paste0("estrutura_fator_ipca_", ANO_IPCA)
  )
  # Sem o cache do Python (caso da vitrine pública) essa checagem é pulada
  # pelo próprio pipeline e a linha não existe.
  cache <- CFG$caminhos$executivo_python
  if (!nzchar(cache) || !file.exists(cache)) confere <- setdiff(confere, "estrutura_denominador_python")
  expect_length(setdiff(c(confere, "estrutura_juventude_safras"), names(st)), 0)
  for (id in intersect(confere, names(st))) expect_identical(st[[id]], "confere", label = id)
  # A diferença entre as safras do recorte é conhecida (causas_conhecidas.yml): explicada.
  expect_identical(st[["estrutura_juventude_safras"]], "explicada")
  expect_identical(div$unidade[div$id == paste0("estrutura_fator_ipca_", ANO_IPCA)], "fator")
})

test_that("veredito do pipeline: nenhuma divergência sem explicação", {
  v <- ler_alvo("veredito")
  div <- ler_alvo("divergencias")
  diverge <- div[div$status == "diverge", ]
  detalhe <- paste0(diverge$id, " (R ", format(diverge$valor_r, big.mark = ".", decimal.mark = ","),
                    " × gabarito ", format(diverge$valor_gabarito, big.mark = ".", decimal.mark = ","), ")",
                    collapse = "; ")
  expect_equal(v$divergencias, 0, label = paste("divergências sem explicação:", detalhe))
  expect_equal(v$total, nrow(div))
  expect_equal(v$conferidos + v$explicadas + v$divergencias, v$total)
})

test_that("saidas/divergencias.csv foi gravado e bate com o alvo", {
  arquivos <- ler_alvo("arquivos_saida")
  csv <- grep("divergencias\\.csv$", arquivos, value = TRUE)
  expect_length(csv, 1)
  # O pipeline guarda o caminho relativo à raiz; os testes rodam de dentro de
  # tests/testthat.
  csv <- resolver_caminho(csv)
  expect_true(file.exists(csv))
  lido <- readr::read_csv(csv, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  div <- ler_alvo("divergencias")
  expect_equal(nrow(lido), nrow(div))
  expect_identical(lido$status, div$status)
  # Sem notação científica, e os números do arquivo são os mesmos do alvo.
  numericas <- c("valor_r", "valor_gabarito", "diferenca", "tolerancia")
  for (col in numericas) {
    expect_false(any(grepl("e[-+]?[0-9]", lido[[col]], ignore.case = TRUE)), label = col)
    expect_equal(as.numeric(lido[[col]]), div[[col]], tolerance = 1e-12, label = col)
  }
})
