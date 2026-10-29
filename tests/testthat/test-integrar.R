# Integração das safras e denominador da participação (R/integrar.R).

# Tabela por ano/órgão/GND no formato de ler_uniao_consolidada()$por_orgao_gnd.
linha_orgao <- function(ano, orgao, gnd, valor) {
  dplyr::bind_cols(tibble::tibble(ano = as.integer(ano), orgao_cod = orgao, gnd_cod = gnd), fases_iguais(valor))
}

linha_gnd <- function(ano, gnd, valor) {
  dplyr::bind_cols(tibble::tibble(ano = as.integer(ano), gnd_cod = gnd), fases_iguais(valor))
}

test_that("montar_denominador soma só o Executivo nos GND do config", {
  cfg <- cfg_mini(2005, 2005, ultimo = 2005)
  # Regra própria do teste, para não depender dos órgãos da configuração.
  cfg$denominador$gnd_incluidos <- c("1", "3", "4", "5")
  cfg$denominador$orgaos_excluidos <- c("Z0100", "Z7100")
  cfg$denominador$prefixo_orgaos_excluidos <- "1"
  consolidada <- dplyr::bind_rows(
    linha_orgao(2005, "Z2600", "3", 100),  # Executivo, entra
    linha_orgao(2005, "Z3600", "4", 10),   # Executivo, entra
    linha_orgao(2005, "Z0100", "3", 50),   # Câmara fictícia: na lista de excluídos
    linha_orgao(2005, "1Z100", "1", 30),   # Judiciário fictício: prefixo "1"
    linha_orgao(2005, "Z2600", "2", 20),   # GND 2 (juros): fora
    linha_orgao(2005, "Z7100", "5", 7)     # Encargos fictícios: na lista
  )
  den <- montar_denominador(consolidada, linha_gnd(2005, "3", 999), cfg)
  expect_identical(names(den), c("ano", "fase", "executivo_nominal", "estimado"))
  expect_identical(den$fase, FASES)
  expect_type(den$ano, "integer")
  expect_equal(den$executivo_nominal, rep(110, 6))
  expect_false(any(den$estimado))
})

test_that("montar_denominador estima o ano seguinte à safra antiga pela fração Executivo/União", {
  cfg <- cfg_mini(2005, 2006, ultimo = 2005)
  cfg$denominador$gnd_incluidos <- c("1", "3", "4", "5")
  cfg$denominador$orgaos_excluidos <- "Z0100"
  cfg$denominador$prefixo_orgaos_excluidos <- "1"
  consolidada <- dplyr::bind_rows(
    linha_orgao(2005, "Z2600", "3", 100),
    linha_orgao(2006, "Z2600", "3", 60),   # 2006 incompleto na safra antiga
    linha_orgao(2006, "Z0100", "3", 40),
    linha_orgao(2006, "Z2600", "2", 500)   # GND fora do denominador
  )
  nova <- dplyr::bind_rows(linha_gnd(2005, "3", 100), linha_gnd(2006, "3", 300), linha_gnd(2006, "2", 999))
  den <- montar_denominador(consolidada, nova, cfg)
  d2006 <- den[den$ano == 2006L, ]
  # 300 (União, GND incluídos, safra nova) × 60 / (60 + 40) = 180
  expect_equal(d2006$executivo_nominal, rep(180, 6))
  expect_true(all(d2006$estimado))
  expect_false(any(den$estimado[den$ano == 2005L]))
})

test_that("montar_denominador para quando falta ano na safra antiga", {
  cfg <- cfg_mini(2005, 2006, ultimo = 2005)
  expect_error(
    montar_denominador(linha_orgao(2005, "Z2600", "3", 1), linha_gnd(2006, "3", 1), cfg),
    "2006"
  )
})

test_that("comparar_safras_uniao marca idêntico só quando a diferença cabe na tolerância", {
  cfg <- cfg_mini(2005, 2006, ultimo = 2005)
  consolidada <- dplyr::bind_rows(
    linha_orgao(2005, "Z2600", "3", 100), linha_orgao(2005, "Z0100", "1", 50),
    linha_orgao(2006, "Z2600", "3", 10)
  )
  nova <- dplyr::bind_rows(linha_gnd(2005, "3", 100), linha_gnd(2005, "1", 50.5), linha_gnd(2006, "3", 90))
  comp <- comparar_safras_uniao(consolidada, nova, cfg)
  expect_identical(names(comp), c("ano", "fase", "valor_consolidada", "valor_nova", "diferenca", "identico"))
  expect_true(all(comp$identico[comp$ano == 2005L]))
  expect_equal(comp$diferenca[comp$ano == 2005L], rep(0.5, 6))
  expect_false(any(comp$identico[comp$ano == 2006L]))
  expect_equal(comp$diferenca[comp$ano == 2006L], rep(80, 6))
})

test_that("comparar_safras_juventude acusa linha a mais mesmo quando ela soma zero", {
  cfg <- cfg_mini(2005, 2006, ultimo = 2006)
  base <- function(anos, valores) dplyr::bind_cols(tibble::tibble(ano = as.integer(anos)), purrr::map_dfr(valores, fases_iguais))
  nova <- base(c(2005, 2005, 2006, 2006), c(10, 20, 5, 0))
  antiga <- base(c(2005, 2005, 2006), c(10, 20, 5))
  comp <- comparar_safras_juventude(nova, antiga, cfg)
  expect_identical(names(comp), c("ano", "fase", "valor_nova", "valor_antiga", "diferenca",
                                  "n_linhas_nova", "n_linhas_antiga", "identico"))
  expect_true(all(comp$identico[comp$ano == 2005L]))
  # Mesma soma, mas uma linha a mais: não é idêntico.
  expect_equal(comp$diferenca[comp$ano == 2006L], rep(0, 6))
  expect_false(any(comp$identico[comp$ano == 2006L]))
})

test_that("montar_base_juventude filtra o período do config", {
  cfg <- cfg_mini(2005, 2006)
  nova <- dplyr::bind_cols(tibble::tibble(ano = c(2004L, 2005L, 2006L, 2007L)), purrr::map_dfr(1:4, fases_iguais))
  expect_identical(suppressMessages(montar_base_juventude(nova, cfg))$ano, c(2005L, 2006L))
})

# ---- integração ------------------------------------------------------------

test_that("União: safra antiga e safra nova idênticas até o último ano completo da antiga (pipeline)", {
  comp <- ler_alvo("comparacao_safras_uniao")
  ultimo <- CFG$periodo$ultimo_ano_safra_antiga
  ate <- comp[comp$ano <= ultimo, ]
  # Todos os anos × 6 fases: nenhum par pode faltar.
  expect_equal(nrow(ate), (ultimo - CFG$periodo$ano_inicio + 1) * length(FASES))
  expect_true(all(ate$identico), label = paste("anos diferentes:", paste(unique(ate$ano[!ate$identico]), collapse = ", ")))
  expect_lte(max(abs(ate$diferenca)), CFG$tolerancias$reais)
  # Depois do último ano completo, a safra antiga está incompleta: as fases da
  # execução, que andam ao longo do ano, têm de aparecer como diferentes.
  # Projeto de lei e dotação inicial são fixados no começo do ano e podem sair
  # iguais nas duas safras, por isso não entram nesta expectativa.
  depois <- comp[comp$ano > ultimo, ]
  expect_equal(nrow(depois), (CFG$periodo$ano_fim - ultimo) * length(FASES))
  execucao <- c("dotacao_atual", "empenhado", "liquidado", "pago")
  expect_false(any(depois$identico[depois$fase %in% execucao]))
})

test_that("Juventude: as safras só diferem nos anos e nas ações já explicados (pipeline)", {
  # Anos e ações vêm do registro estrutura_juventude_safras de causas_conhecidas.yml.
  causa <- CAUSAS[CAUSAS$id == "estrutura_juventude_safras", ]
  expect_equal(nrow(causa), 1)
  anos_conhecidos <- causa$anos[[1]]
  acoes_conhecidas <- causa$acoes[[1]]
  comp <- ler_alvo("comparacao_safras_juventude")
  anos_dif <- sort(unique(comp$ano[!comp$identico]))
  expect_true(all(anos_dif %in% anos_conhecidos), label = paste("anos diferentes:", paste(anos_dif, collapse = ", ")))
  expect_true(all(comp$identico[!comp$ano %in% anos_conhecidos]))
  dif <- ler_alvo("diferencas_juventude")
  expect_true(all(dif$ano %in% anos_conhecidos))
  # As ações de onde vêm as linhas a mais.
  expect_true(all(dif$acao_cod %in% acoes_conhecidas),
              label = paste("ações:", paste(sort(unique(dif$acao_cod)), collapse = ", ")))
})

test_that("denominador em R bate com o cache do Python até o último ano completo, seis fases (pipeline)", {
  den <- ler_alvo("denominador")
  caminho <- CFG$caminhos$executivo_python
  skip_if(!nzchar(caminho) || !file.exists(caminho), "Sem o cache do denominador em Python (opcional).")
  cache <- readr::read_csv(caminho, col_types = readr::cols(.default = readr::col_double()), progress = FALSE) |>
    dplyr::select("ano", dplyr::starts_with("executivo_")) |>
    tidyr::pivot_longer(-"ano", names_to = "fase", names_prefix = "executivo_", values_to = "python") |>
    dplyr::mutate(ano = as.integer(.data$ano))
  ultimo <- CFG$periodo$ultimo_ano_safra_antiga
  comp <- dplyr::inner_join(dplyr::filter(den, .data$ano <= ultimo), cache, by = c("ano", "fase"))
  expect_equal(nrow(comp), (ultimo - CFG$periodo$ano_inicio + 1) * length(FASES))
  pior <- comp[which.max(abs(comp$executivo_nominal - comp$python)), ]
  expect_lte(abs(pior$executivo_nominal - pior$python), CFG$tolerancias$reais)
  expect_false(any(den$estimado[den$ano <= ultimo]))
  expect_true(all(den$estimado[den$ano > ultimo]))
})
