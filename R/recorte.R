# Recorte de juventude: leitura da lista de ações, aplicação sobre a base e
# conferências do que o recorte cobre (contrato, seção 6.5).
#
# O recorte é uma ENTRADA do pipeline (DECISOES.md, D3): a lista de ações vem
# de entradas/recorte_acoes.csv e não é refeita aqui. Estas funções só
# conferem que a lista e a base contam a mesma história.

# Colunas que derivar_recorte() grava (contrato, seção 6.4). Ficam aqui para
# que ler_recorte() possa dizer exatamente o que falta no arquivo.
COLUNAS_RECORTE <- c(
  "acao_cod", "acao_nome", "programas", "origem",
  "primeiro_ano", "ultimo_ano", "n_linhas"
)

#' Lê a lista de ações do recorte de juventude
#'
#' @param caminho Caminho de `recorte_acoes.csv`.
#' @return Tibble com as colunas de `derivar_recorte()`.
ler_recorte <- function(caminho) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c(
      "Não encontrei o arquivo do recorte de ações.",
      "x" = "Caminho procurado: {.path {caminho}}",
      "i" = "Confira {.field caminhos: recorte_acoes} em config/config.yml ou rode {.code scripts/derivar_recorte.R}."
    ))
  }

  # Tudo como texto na leitura: códigos de ação como "0Z01" perderiam o zero
  # à esquerda se o readr adivinhasse número. Os anos e a contagem são
  # convertidos depois, com conferência.
  recorte <- readr::read_csv(
    caminho,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE
  )

  faltam <- setdiff(COLUNAS_RECORTE, names(recorte))
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "O arquivo do recorte não tem todas as colunas esperadas.",
      "x" = "Faltam: {.field {faltam}}",
      "i" = "Arquivo: {.path {caminho}}. Gere de novo com {.code scripts/derivar_recorte.R}."
    ))
  }

  recorte <- recorte |>
    dplyr::mutate(
      acao_cod = stringr::str_trim(.data$acao_cod),
      primeiro_ano = as.integer(.data$primeiro_ano),
      ultimo_ano = as.integer(.data$ultimo_ano),
      n_linhas = as.integer(.data$n_linhas)
    )

  # Uma ação repetida faria o semi_join continuar certo, mas as contagens do
  # resumo (ações no recorte, inclusões manuais) sairiam infladas.
  duplicadas <- unique(recorte$acao_cod[duplicated(recorte$acao_cod)])
  if (length(duplicadas) > 0) {
    cli::cli_abort(c(
      "O recorte tem ação repetida; cada código de ação deve aparecer uma vez.",
      "x" = "Repetidas: {.val {duplicadas}}",
      "i" = "Arquivo: {.path {caminho}}."
    ))
  }

  if (anyNA(recorte$acao_cod) || any(recorte$acao_cod == "")) {
    cli::cli_abort(c(
      "O recorte tem linha sem código de ação.",
      "i" = "Arquivo: {.path {caminho}}."
    ))
  }

  recorte
}

#' Aplica o recorte de ações a uma base linha a linha
#'
#' @param base Tibble com a coluna `acao_cod` (esquema `BASE_JUVENTUDE`).
#' @param recorte Saída de `ler_recorte()`.
#' @return `base` só com as linhas cujas ações estão no recorte.
aplicar_recorte <- function(base, recorte) {
  # semi_join e não inner_join: o recorte tem uma linha por ação, mas assim
  # nenhuma coluna do recorte entra na base e nenhuma linha pode ser duplicada.
  depois <- dplyr::semi_join(base, recorte, by = "acao_cod")

  # Milhar com ponto, como se lê no Brasil; decimal.mark vírgula evita o
  # aviso do R quando os dois separadores seriam ponto.
  milhar <- function(n) format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  antes_n <- nrow(base)
  depois_n <- nrow(depois)
  cli::cli_alert_info(
    "Recorte aplicado: {milhar(antes_n)} linhas antes, {milhar(depois_n)} depois ({milhar(antes_n - depois_n)} removidas)."
  )

  depois
}

#' Resume o efeito do recorte em uma linha
#'
#' @param base_antes Base antes de `aplicar_recorte()`.
#' @param base_depois Base depois de `aplicar_recorte()`.
#' @param recorte Saída de `ler_recorte()`.
#' @param acoes_obrigatorias Códigos que precisam estar na base recortada
#'   (de `recorte: acoes_obrigatorias` em config.yml; por exemplo, `"ZM01"`).
#' @return Tibble de uma linha.
resumir_recorte <- function(base_antes, base_depois, recorte,
                            acoes_obrigatorias = character()) {
  acoes_base <- unique(base_antes$acao_cod)
  acoes_recorte <- unique(recorte$acao_cod)
  acoes_depois <- unique(base_depois$acao_cod)

  ausentes <- setdiff(acoes_obrigatorias, acoes_depois)

  tibble::tibble(
    linhas_antes = nrow(base_antes),
    linhas_depois = nrow(base_depois),
    linhas_removidas = nrow(base_antes) - nrow(base_depois),
    acoes_no_recorte = length(acoes_recorte),
    acoes_na_base = length(acoes_base),
    # Ação da base fora do recorte = linha que o recorte joga fora. Na safra
    # nova esperamos zero, porque o recorte foi derivado dela.
    acoes_da_base_fora_do_recorte = length(setdiff(acoes_base, acoes_recorte)),
    # Ação do recorte sem linha na base = lista desatualizada ou base errada.
    acoes_do_recorte_sem_linha = length(setdiff(acoes_recorte, acoes_base)),
    inclusoes_manuais = sum(recorte$origem == "inclusão manual", na.rm = TRUE),
    obrigatorias_ausentes = if (length(ausentes) == 0) {
      NA_character_
    } else {
      paste(ausentes, collapse = "|")
    }
  )
}

#' Testa se o recorte se refaz a partir da base da União
#'
#' Documenta por que o recorte não se refaz pela base da União: aplicar os
#' códigos do recorte por (ano, ação) traz ações genéricas de outros
#' programas; por (ano, programa, ação) pode bater nos primeiros anos e sobrar
#' depois, quando o filtro original passa a usar campos que a base da União
#' não tem (como o objetivo do plano plurianual).
#'
#' @param uniao_por_programa_acao Tibble `ano, programa_cod, acao_cod` e fases
#'   (de `ler_uniao_consolidada()$por_programa_acao`).
#' @param base_juventude Base de juventude linha a linha (já recortada).
#' @param fase Nome da fase a comparar.
#' @return Tibble por ano: `ano, valor_juventude, valor_uniao_mesma_acao,
#'   valor_uniao_mesmo_programa_acao, excesso_acao, excesso_programa_acao`.
#'   Anos são os da base de juventude. Quando a base da União não tem nenhuma
#'   linha do ano, os valores da União saem `NA` (e não zero, que pareceria
#'   um resultado). A consolidada tem incompletos os anos depois de
#'   `ultimo_ano_safra_antiga`: essas linhas saem calculadas, mas não servem de
#'   comparação (o relatório as deixa de fora).
testar_recorte_na_uniao <- function(uniao_por_programa_acao, base_juventude,
                                    fase = "dotacao_atual") {
  for (tabela in list(
    list(df = uniao_por_programa_acao, nome = "uniao_por_programa_acao"),
    list(df = base_juventude, nome = "base_juventude")
  )) {
    faltam <- setdiff(c("ano", "programa_cod", "acao_cod", fase), names(tabela$df))
    if (length(faltam) > 0) {
      cli::cli_abort(c(
        "A tabela {.var {tabela$nome}} não tem as colunas necessárias para comparar a fase {.val {fase}}.",
        "x" = "Faltam: {.field {faltam}}"
      ))
    }
  }

  uniao <- uniao_por_programa_acao |>
    dplyr::mutate(ano = as.integer(.data$ano))
  juv <- base_juventude |>
    dplyr::mutate(ano = as.integer(.data$ano))

  valor_juventude <- juv |>
    dplyr::summarise(valor_juventude = sum(.data[[fase]]), .by = "ano")

  # Os pares e trios vêm de cada ano da base de juventude, e não de um
  # conjunto único de códigos: uma ação que entrou no recorte só em 2016 não
  # deve puxar valores da União de 2005.
  pares <- dplyr::distinct(dplyr::select(juv, "ano", "acao_cod"))
  trios <- dplyr::distinct(dplyr::select(juv, "ano", "programa_cod", "acao_cod"))

  mesma_acao <- uniao |>
    dplyr::semi_join(pares, by = c("ano", "acao_cod")) |>
    dplyr::summarise(valor_uniao_mesma_acao = sum(.data[[fase]]), .by = "ano")

  mesmo_programa_acao <- uniao |>
    dplyr::semi_join(trios, by = c("ano", "programa_cod", "acao_cod")) |>
    dplyr::summarise(valor_uniao_mesmo_programa_acao = sum(.data[[fase]]), .by = "ano")

  anos_na_uniao <- unique(uniao$ano)

  valor_juventude |>
    dplyr::left_join(mesma_acao, by = "ano") |>
    dplyr::left_join(mesmo_programa_acao, by = "ano") |>
    dplyr::mutate(
      # Ano presente na União mas sem nenhuma ação em comum: aí sim é zero.
      valor_uniao_mesma_acao = dplyr::if_else(
        is.na(.data$valor_uniao_mesma_acao) & .data$ano %in% anos_na_uniao,
        0, .data$valor_uniao_mesma_acao
      ),
      valor_uniao_mesmo_programa_acao = dplyr::if_else(
        is.na(.data$valor_uniao_mesmo_programa_acao) & .data$ano %in% anos_na_uniao,
        0, .data$valor_uniao_mesmo_programa_acao
      ),
      excesso_acao = .data$valor_uniao_mesma_acao - .data$valor_juventude,
      excesso_programa_acao = .data$valor_uniao_mesmo_programa_acao - .data$valor_juventude
    ) |>
    dplyr::arrange(.data$ano) |>
    dplyr::select(
      "ano", "valor_juventude", "valor_uniao_mesma_acao",
      "valor_uniao_mesmo_programa_acao", "excesso_acao", "excesso_programa_acao"
    )
}
