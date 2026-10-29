# Agregações da base de juventude já recortada e deflacionada (`base_real`).
#
# Regra geral: toda soma usa na.rm = FALSE. Um valor ausente na base tem de
# aparecer como NA no total, e não sumir em silêncio, porque um total "certo"
# com uma linha perdida é justamente o erro que a conferência procura.
#
# Percentuais saem na escala 0 a 100 (12,5 quer dizer 12,5%), a mesma dos
# gabaritos e do relatório.

# Colunas reais que `deflacionar()` acrescenta à base: projeto_lei_real etc.
colunas_reais <- function() paste0(FASES, "_real")

# Confere, antes de somar, que a base tem as colunas nominais e reais de todas
# as fases. Sem isso um erro de digitação viraria "coluna não encontrada" no
# meio de um summarise, sem dizer de onde veio.
conferir_base_real <- function(base_real) {
  esperadas <- c("ano", "acao_cod", "programa_cod", "programa_nome", "gnd_cod",
                 FASES, colunas_reais())
  faltam <- setdiff(esperadas, names(base_real))
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "A base deflacionada não tem as colunas esperadas.",
      "x" = "Faltam: {.val {faltam}}.",
      "i" = "Ela deve vir de {.fn deflacionar} (R/deflacionar.R)."
    ))
  }
  invisible(base_real)
}

# Soma nominal e real por um conjunto de chaves e devolve no formato longo
# (uma linha por chave e fase), com as fases na ordem de FASES.
somar_por <- function(base_real, chaves) {
  conferir_base_real(base_real)
  base_real |>
    dplyr::group_by(dplyr::across(dplyr::all_of(chaves))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(c(FASES, colunas_reais())), \(x) sum(x, na.rm = FALSE)),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(FASES, colunas_reais())),
      names_to = "coluna", values_to = "valor"
    ) |>
    dplyr::mutate(
      tipo = dplyr::if_else(endsWith(coluna, "_real"), "real", "nominal"),
      fase = sub("_real$", "", coluna)
    ) |>
    dplyr::select(-"coluna") |>
    tidyr::pivot_wider(names_from = "tipo", values_from = "valor") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(chaves)), match(.data$fase, FASES)) |>
    dplyr::select(dplyr::all_of(chaves), "fase", "nominal", "real")
}

totais_por_ano <- function(base_real) {
  somar_por(base_real, "ano")
}

totais_periodo <- function(totais_ano) {
  totais_ano |>
    dplyr::group_by(.data$fase) |>
    dplyr::summarise(
      nominal = sum(.data$nominal, na.rm = FALSE),
      real = sum(.data$real, na.rm = FALSE),
      # média sobre os anos que existem na tabela, e não 21 fixo: se um ano
      # faltar, a média muda e a conferência acusa
      n_anos = dplyr::n_distinct(.data$ano),
      .groups = "drop"
    ) |>
    dplyr::mutate(media_anual_real = .data$real / .data$n_anos) |>
    dplyr::arrange(match(.data$fase, FASES)) |>
    dplyr::select("fase", "nominal", "real", "media_anual_real")
}

totais_por_gnd <- function(base_real) {
  somar_por(base_real, "gnd_cod") |>
    dplyr::group_by(.data$fase) |>
    dplyr::mutate(pct_real = 100 * .data$real / sum(.data$real, na.rm = FALSE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(match(.data$fase, FASES), .data$gnd_cod) |>
    dplyr::select("gnd_cod", "fase", "real", "pct_real")
}

totais_por_acao_ano <- function(base_real) {
  somar_por(base_real, c("acao_cod", "ano"))
}

# Taxa de execução = empenhado ÷ dotação atual, em valores reais. A linha com
# ano = NA é o período inteiro calculado como razão dos totais (como no
# Relatório), e não como média das taxas anuais.
taxa_execucao <- function(totais_ano) {
  largo <- totais_ano |>
    dplyr::filter(.data$fase %in% c("empenhado", "dotacao_atual")) |>
    dplyr::select("ano", "fase", "real") |>
    tidyr::pivot_wider(names_from = "fase", values_from = "real") |>
    dplyr::arrange(.data$ano)
  anual <- largo |>
    dplyr::transmute(
      ano = as.integer(.data$ano),
      empenhado_real = .data$empenhado,
      dotacao_atual_real = .data$dotacao_atual
    )
  periodo <- tibble::tibble(
    ano = NA_integer_,
    empenhado_real = sum(anual$empenhado_real, na.rm = FALSE),
    dotacao_atual_real = sum(anual$dotacao_atual_real, na.rm = FALSE)
  )
  dplyr::bind_rows(anual, periodo) |>
    dplyr::mutate(taxa_pct = 100 * .data$empenhado_real / .data$dotacao_atual_real)
}

# Participação da juventude no gasto do Executivo. Nominal ÷ nominal porque,
# dentro de um mesmo ano, o deflator é o mesmo no numerador e no denominador.
# `fase` vem de cfg$denominador$fase (o _targets.R passa); o padrão é o do
# contrato. Os nomes das colunas seguem o contrato mesmo se a fase mudar.
participacao_executivo <- function(totais_ano, denominador, fase = "empenhado") {
  juventude <- totais_ano |>
    dplyr::filter(.data$fase == !!fase) |>
    dplyr::select("ano", empenhado_juventude = "nominal")
  executivo <- denominador |>
    dplyr::filter(.data$fase == !!fase) |>
    dplyr::select("ano", empenhado_executivo = "executivo_nominal", "estimado")
  sem_denominador <- setdiff(juventude$ano, executivo$ano)
  if (length(sem_denominador) > 0) {
    cli::cli_warn("Sem denominador do Executivo para {.val {sem_denominador}}: a participação desses anos fica NA.")
  }
  juventude |>
    dplyr::left_join(executivo, by = "ano") |>
    dplyr::mutate(
      ano = as.integer(.data$ano),
      participacao_pct = 100 * .data$empenhado_juventude / .data$empenhado_executivo
    ) |>
    dplyr::arrange(.data$ano) |>
    dplyr::select("ano", "empenhado_juventude", "empenhado_executivo", "participacao_pct", "estimado")
}

# Nome de cada código no ano mais recente em que ele aparece. Um mesmo código
# de programa muda de nome entre PPAs (por isso a contagem por código e nome
# dá mais programas que a contagem por código); havendo dois nomes no mesmo ano, fica o de
# maior valor, para o resultado não depender da ordem das linhas.
nome_mais_recente <- function(base_real, cod, nome, valor) {
  base_real |>
    dplyr::group_by(.data[[cod]], .data$ano, .data[[nome]]) |>
    dplyr::summarise(peso = sum(.data[[valor]], na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data[[cod]], dplyr::desc(.data$ano), dplyr::desc(.data$peso), .data[[nome]]) |>
    dplyr::distinct(.data[[cod]], .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(cod, nome)))
}

# Quantos itens, do maior para o menor, são necessários para o acumulado
# alcançar o limiar. Compara razão com razão (e não percentuais
# arredondados) para não errar na fronteira.
n_ate_limiar <- function(valores, limiar) {
  ordenados <- sort(valores, decreasing = TRUE)
  acumulado <- cumsum(ordenados) / sum(ordenados)
  which(acumulado >= limiar)[1]
}

concentracao_programas <- function(base_real, cfg) {
  conferir_base_real(base_real)
  limiar <- cfg$concentracao$limiar
  coluna <- paste0(cfg$concentracao$fase, "_real")
  por_programa <- base_real |>
    dplyr::group_by(.data$programa_cod) |>
    dplyr::summarise(
      valor_real = sum(.data[[coluna]], na.rm = FALSE),
      pago_real = sum(.data$pago_real, na.rm = FALSE),
      .groups = "drop"
    )
  nomes <- nome_mais_recente(base_real, "programa_cod", "programa_nome", coluna)
  tabela <- por_programa |>
    dplyr::left_join(nomes, by = "programa_cod") |>
    # desempate pelo código, para a posição não depender da ordem de leitura
    dplyr::arrange(dplyr::desc(.data$valor_real), .data$programa_cod) |>
    dplyr::mutate(
      pct = 100 * .data$valor_real / sum(.data$valor_real, na.rm = FALSE),
      pct_acumulado = cumsum(.data$pct),
      posicao = dplyr::row_number()
    ) |>
    dplyr::select("programa_cod", "programa_nome", "valor_real", "pct", "pct_acumulado", "posicao")
  list(
    tabela = tabela,
    n_ate_limiar = n_ate_limiar(por_programa$valor_real, limiar),
    n_ate_limiar_pago = n_ate_limiar(por_programa$pago_real, limiar),
    limiar = limiar,
    fase = cfg$concentracao$fase
  )
}

contagens <- function(base_real) {
  tibble::tibble(
    n_programas = dplyr::n_distinct(base_real$programa_cod),
    n_acoes = dplyr::n_distinct(base_real$acao_cod),
    n_linhas = nrow(base_real)
  )
}

ranking_anos <- function(totais_ano, fase = "dotacao_atual", n = 4) {
  anos_em_ordem_de_valor(totais_ano, fase, n)
}

# Corpo de ranking_anos() com outro nome: dentro do pipeline o alvo
# `ranking_anos` esconde a função de mesmo nome, e conferir.R precisa dela.
anos_em_ordem_de_valor <- function(totais_ano, fase, n) {
  totais_ano |>
    dplyr::filter(.data$fase == !!fase) |>
    dplyr::arrange(dplyr::desc(.data$real)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull("ano") |>
    as.integer()
}
