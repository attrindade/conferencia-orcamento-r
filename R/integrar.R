# Integração das safras: compara o que as extrações do SIOP dizem sobre os
# mesmos anos e monta as bases que o resto do pipeline usa. Diferença entre
# safras não é corrigida aqui: vira tabela, e a tabela vai para divergências.

# Soma as seis fases por ano em formato longo (ano, fase, valor), com o número
# de linhas da base em cada ano.
somar_por_ano_fase <- function(base, anos) {
  base |>
    dplyr::filter(.data$ano %in% anos) |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(n_linhas = dplyr::n(), dplyr::across(dplyr::all_of(FASES), \(x) sum(x))) |>
    tidyr::pivot_longer(dplyr::all_of(FASES), names_to = "fase", values_to = "valor")
}

ordenar_fases <- function(df) {
  df |>
    dplyr::arrange(.data$ano, match(.data$fase, FASES))
}

comparar_safras_juventude <- function(nova, antiga, cfg) {
  anos <- seq(cfg$periodo$ano_inicio, cfg$periodo$ultimo_ano_safra_antiga)
  tol <- cfg$tolerancias$reais
  n <- somar_por_ano_fase(nova, anos) |> dplyr::rename(valor_nova = "valor", n_linhas_nova = "n_linhas")
  a <- somar_por_ano_fase(antiga, anos) |> dplyr::rename(valor_antiga = "valor", n_linhas_antiga = "n_linhas")
  # full_join para que um ano presente em só uma safra apareça (com NA), em vez de sumir.
  dplyr::full_join(n, a, by = c("ano", "fase")) |>
    dplyr::mutate(
      ano = as.integer(.data$ano),
      diferenca = .data$valor_nova - .data$valor_antiga,
      identico = !is.na(.data$diferenca) & abs(.data$diferenca) <= tol &
        !is.na(.data$n_linhas_nova) & .data$n_linhas_nova == .data$n_linhas_antiga
    ) |>
    dplyr::select("ano", "fase", "valor_nova", "valor_antiga", "diferenca", "n_linhas_nova", "n_linhas_antiga", "identico") |>
    ordenar_fases()
}

# Mostra onde as safras de juventude diferem, por ano, programa e ação, com as
# palavras-chave que levaram a linha ao recorte. É o que explica a diferença.
detalhar_diferencas_juventude <- function(nova, antiga, cfg) {
  anos <- seq(cfg$periodo$ano_inicio, cfg$periodo$ultimo_ano_safra_antiga)
  tol <- cfg$tolerancias$reais
  resumir <- function(base, sufixo) {
    base |>
      dplyr::filter(.data$ano %in% anos) |>
      dplyr::group_by(.data$ano, .data$programa_cod, .data$acao_cod) |>
      dplyr::summarise(
        n_linhas = dplyr::n(),
        dplyr::across(dplyr::all_of(FASES), \(x) sum(x)),
        palavras_chave = juntar_palavras(.data$palavras_chave),
        .groups = "drop"
      ) |>
      dplyr::rename_with(\(x) paste0(x, sufixo), -c("ano", "programa_cod", "acao_cod"))
  }
  juntas <- dplyr::full_join(resumir(nova, "_nova"), resumir(antiga, "_antiga"), by = c("ano", "programa_cod", "acao_cod"))

  # Ausente em uma safra = zero linhas e zero reais naquela safra.
  zerar <- c("n_linhas_nova", "n_linhas_antiga", paste0(FASES, "_nova"), paste0(FASES, "_antiga"))
  juntas <- dplyr::mutate(juntas, dplyr::across(dplyr::all_of(zerar), \(x) dplyr::coalesce(as.double(x), 0)))

  # Difere se muda o número de linhas ou qualquer fase, não só a dotação atual.
  difere_fase <- Reduce(`|`, lapply(FASES, \(f) abs(juntas[[paste0(f, "_nova")]] - juntas[[paste0(f, "_antiga")]]) > tol))
  juntas |>
    dplyr::filter(.data$n_linhas_nova != .data$n_linhas_antiga | difere_fase) |>
    dplyr::transmute(
      ano = as.integer(.data$ano),
      programa_cod = .data$programa_cod,
      acao_cod = .data$acao_cod,
      n_linhas_nova = as.integer(.data$n_linhas_nova),
      n_linhas_antiga = as.integer(.data$n_linhas_antiga),
      dotacao_atual_nova = .data$dotacao_atual_nova,
      dotacao_atual_antiga = .data$dotacao_atual_antiga,
      diferenca = .data$dotacao_atual_nova - .data$dotacao_atual_antiga,
      palavras_chave = dplyr::coalesce(.data$palavras_chave_nova, .data$palavras_chave_antiga)
    ) |>
    dplyr::arrange(.data$ano, .data$programa_cod, .data$acao_cod)
}

# Une as palavras-chave das linhas de um grupo (cada linha traz "a,b") em uma
# lista única e ordenada; NA quando nenhuma linha tem palavra-chave.
juntar_palavras <- function(x) {
  termos <- unlist(strsplit(x[!is.na(x)], ",", fixed = TRUE))
  termos <- sort(unique(trimws(termos[nzchar(trimws(termos))])))
  if (length(termos) == 0) NA_character_ else paste(termos, collapse = ",")
}

# União inteira (todos os órgãos e GND) nas duas safras. Até o último ano
# completo da safra antiga elas têm de ser iguais; o ano seguinte difere por
# construção (a safra antiga foi extraída com o ano em curso).
comparar_safras_uniao <- function(consolidada_por_orgao_gnd, uniao_nova, cfg) {
  anos <- seq(cfg$periodo$ano_inicio, cfg$periodo$ano_fim)
  tol <- cfg$tolerancias$reais
  somar <- function(df) {
    df |>
      dplyr::filter(.data$ano %in% anos) |>
      dplyr::group_by(.data$ano) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(FASES), \(x) sum(x))) |>
      tidyr::pivot_longer(dplyr::all_of(FASES), names_to = "fase", values_to = "valor")
  }
  dplyr::full_join(
    somar(consolidada_por_orgao_gnd) |> dplyr::rename(valor_consolidada = "valor"),
    somar(uniao_nova) |> dplyr::rename(valor_nova = "valor"),
    by = c("ano", "fase")
  ) |>
    dplyr::mutate(
      ano = as.integer(.data$ano),
      diferenca = .data$valor_nova - .data$valor_consolidada,
      identico = !is.na(.data$diferenca) & abs(.data$diferenca) <= tol
    ) |>
    dplyr::select("ano", "fase", "valor_consolidada", "valor_nova", "diferenca", "identico") |>
    ordenar_fases()
}

# Numerador do pipeline (decisão D1): safra nova em todos os anos do período.
montar_base_juventude <- function(nova, cfg) {
  anos <- seq(cfg$periodo$ano_inicio, cfg$periodo$ano_fim)
  base <- dplyr::filter(nova, .data$ano %in% anos)
  n_base <- formatC(nrow(base), format = "d", big.mark = ".", decimal.mark = ",")
  n_lidas <- formatC(nrow(nova), format = "d", big.mark = ".", decimal.mark = ",")
  cli::cli_alert_info("Base de juventude {cfg$periodo$ano_inicio} a {cfg$periodo$ano_fim}: {n_base} linhas (de {n_lidas} lidas).")
  base
}

# Denominador da participação (decisão D2): gasto do Executivo nos GND do
# config, sem os órgãos excluídos e sem o Judiciário (órgãos 1xxxx).
# Até o último ano completo da safra antiga, soma direta. Depois disso a
# safra nova não tem órgão, então aplica-se a fração Executivo/União da safra
# antiga ao total da União da safra nova, o mesmo método da implementação de
# referência.
montar_denominador <- function(consolidada_por_orgao_gnd, uniao_nova, cfg) {
  den <- cfg$denominador
  gnd <- as.character(unlist(den$gnd_incluidos))
  excluidos <- as.character(unlist(den$orgaos_excluidos))
  prefixo <- as.character(den$prefixo_orgaos_excluidos)
  ultimo_completo <- cfg$periodo$ultimo_ano_safra_antiga
  anos <- seq(cfg$periodo$ano_inicio, cfg$periodo$ano_fim)

  nos_gnd <- dplyr::filter(consolidada_por_orgao_gnd, .data$gnd_cod %in% gnd, .data$ano %in% anos)
  # Órgão sem código conta como Executivo, como na referência em Python
  # (lá o teste é "não está na lista e não começa com 1").
  e_executivo <- !(nos_gnd$orgao_cod %in% excluidos) &
    (is.na(nos_gnd$orgao_cod) | !startsWith(nos_gnd$orgao_cod, prefixo))

  somar <- function(df, nome) {
    df |>
      dplyr::group_by(.data$ano) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(FASES), \(x) sum(x))) |>
      tidyr::pivot_longer(dplyr::all_of(FASES), names_to = "fase", values_to = nome)
  }
  executivo_antiga <- somar(nos_gnd[e_executivo, ], "executivo_antiga")
  uniao_antiga <- somar(nos_gnd, "uniao_antiga")
  uniao_nova_gnd <- somar(dplyr::filter(uniao_nova, .data$gnd_cod %in% gnd, .data$ano %in% anos), "uniao_nova")

  faltam <- setdiff(anos, executivo_antiga$ano)
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "A base consolidada da União não tem os anos {.val {faltam}}.",
      "i" = "O ano estimado precisa da fração Executivo/União da própria safra antiga."
    ))
  }

  executivo_antiga |>
    dplyr::left_join(uniao_antiga, by = c("ano", "fase")) |>
    dplyr::left_join(uniao_nova_gnd, by = c("ano", "fase")) |>
    dplyr::mutate(
      estimado = .data$ano > ultimo_completo,
      executivo_nominal = dplyr::if_else(
        .data$estimado,
        .data$uniao_nova * .data$executivo_antiga / .data$uniao_antiga,
        .data$executivo_antiga
      ),
      ano = as.integer(.data$ano)
    ) |>
    dplyr::select("ano", "fase", "executivo_nominal", "estimado") |>
    ordenar_fases()
}

# Lista de ações do recorte de juventude, derivada da própria safra nova.
# Não roda no pipeline: o script scripts/derivar_recorte.R grava o resultado
# em entradas/recorte_acoes.csv, que passa a ser a entrada (decisão D3).
derivar_recorte <- function(nova) {
  nome_recente <- nova |>
    dplyr::filter(!is.na(.data$acao_nome), .data$acao_nome != "") |>
    dplyr::count(.data$acao_cod, .data$ano, .data$acao_nome) |>
    # No ano mais recente, o nome mais frequente; empate resolvido em ordem
    # alfabética para que o arquivo saia igual a cada execução.
    dplyr::arrange(.data$acao_cod, dplyr::desc(.data$ano), dplyr::desc(.data$n), .data$acao_nome) |>
    dplyr::distinct(.data$acao_cod, .keep_all = TRUE) |>
    dplyr::select("acao_cod", "acao_nome")

  nova |>
    dplyr::group_by(.data$acao_cod) |>
    dplyr::summarise(
      programas = paste(sort(unique(.data$programa_cod)), collapse = "|"),
      origem = dplyr::if_else(any(.data$flag_palavra_chave), "palavra-chave", "inclusão manual"),
      primeiro_ano = as.integer(min(.data$ano)),
      ultimo_ano = as.integer(max(.data$ano)),
      n_linhas = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(nome_recente, by = "acao_cod") |>
    dplyr::select("acao_cod", "acao_nome", "programas", "origem", "primeiro_ano", "ultimo_ano", "n_linhas") |>
    dplyr::arrange(.data$acao_cod)
}
