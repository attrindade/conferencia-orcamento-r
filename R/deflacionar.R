# Deflação pelo IPCA (contrato, seção 6.5).
#
# Os fatores do arquivo do IPCA já são multiplicativos, com base no mês de
# referência de config.yml (projeto: data_referencia_precos): valor real =
# valor nominal × fator do ano. Um fator errado muda todos os valores reais do
# ano sem deixar sinal; por isso a conferência do fator vem antes de qualquer
# conta. O fator certo e o errado conhecido ficam em config.yml (ipca:).

#' Confere a tabela de fatores do IPCA antes de deflacionar
#'
#' Aborta se faltar fator para algum ano do período, se houver ano repetido,
#' se o fator do ano conferido diferir do esperado em mais de 1e-7 ou se for
#' igual ao fator errado conhecido.
#'
#' @param ipca Tibble `ano, fator` (de `ler_ipca()`).
#' @param cfg Configuração (de `carregar_config()`).
#' @return `ipca`, invisível.
conferir_fator_ipca <- function(ipca, cfg) {
  tolerancia <- TOLERANCIA_FATOR
  ano_conferido <- as.integer(cfg$ipca$ano_conferido)
  esperado <- as.numeric(cfg$ipca$fator_esperado)
  errado <- as.numeric(cfg$ipca$fator_errado_conhecido)
  periodo <- seq.int(as.integer(cfg$periodo$ano_inicio), as.integer(cfg$periodo$ano_fim))

  faltam <- setdiff(c("ano", "fator"), names(ipca))
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "A tabela do IPCA não tem as colunas esperadas.",
      "x" = "Faltam: {.field {faltam}}",
      "i" = "Confira {.field caminhos: ipca} em config/config.yml e a função {.fn ler_ipca}."
    ))
  }

  anos <- as.integer(ipca$ano)

  # Ano repetido multiplicaria as linhas da base na junção com o IPCA, e a
  # soma em reais sairia inflada sem nenhum aviso.
  repetidos <- unique(anos[duplicated(anos)])
  if (length(repetidos) > 0) {
    cli::cli_abort(c(
      "A tabela do IPCA tem ano repetido.",
      "x" = "Anos repetidos: {.val {repetidos}}",
      "i" = "Confira o arquivo de fatores do IPCA."
    ))
  }

  sem_fator <- setdiff(periodo, anos[!is.na(ipca$fator)])
  if (length(sem_fator) > 0) {
    cli::cli_abort(c(
      "Falta fator do IPCA para ano do período {periodo[1]} a {periodo[length(periodo)]}.",
      "x" = "Anos sem fator: {.val {sem_fator}}",
      "i" = "Use uma extração do IPCA que cubra o período ou ajuste {.field periodo} em config/config.yml."
    ))
  }

  fator <- ipca$fator[anos == ano_conferido]
  if (length(fator) != 1 || is.na(fator)) {
    cli::cli_abort(c(
      "Não há fator do IPCA para o ano conferido ({ano_conferido}).",
      "i" = "Confira {.field ipca: ano_conferido} em config/config.yml e o arquivo do IPCA."
    ))
  }

  # Comparar com o errado vem primeiro: se a correção não foi aplicada, a
  # mensagem certa é "fator antigo", e não só "diferente do esperado".
  if (abs(fator - errado) <= tolerancia) {
    cli::cli_abort(c(
      "O fator do IPCA de {ano_conferido} é o antigo, que está errado.",
      "x" = "Encontrado: {numero_virgula(fator, 7)}, igual a {.field ipca: fator_errado_conhecido} em config/config.yml.",
      "i" = "Esperado: {numero_virgula(esperado, 7)}. Use uma extração do IPCA com o fator corrigido."
    ))
  }

  if (abs(fator - esperado) > tolerancia) {
    cli::cli_abort(c(
      "O fator do IPCA de {ano_conferido} não é o esperado.",
      "x" = "Encontrado: {numero_virgula(fator, 7)}; esperado: {numero_virgula(esperado, 7)} (diferença acima de 0,0000001).",
      "i" = "Confira o arquivo do IPCA e {.field ipca: fator_esperado} em config/config.yml."
    ))
  }

  cli::cli_alert_success(
    "Fator do IPCA de {ano_conferido} conferido: {numero_virgula(fator, 7)}, o esperado em config/config.yml."
  )
  invisible(ipca)
}

#' Deflaciona as fases da base para preços de referência
#'
#' @param base Tibble com `ano` e as colunas de `FASES` em reais nominais.
#' @param ipca Tibble `ano, fator`.
#' @param cfg Configuração.
#' @return `base` com as colunas `<fase>_real` (`valor × fator`), sem
#'   arredondar: arredondar linha a linha acumularia erro nas somas.
deflacionar <- function(base, ipca, cfg) {
  conferir_fator_ipca(ipca, cfg)

  faltam <- setdiff(c("ano", FASES), names(base))
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "A base a deflacionar não tem as colunas esperadas.",
      "x" = "Faltam: {.field {faltam}}"
    ))
  }

  fatores <- ipca |>
    dplyr::transmute(ano = as.integer(.data$ano), .fator_ipca = as.numeric(.data$fator))

  n_antes <- nrow(base)
  com_fator <- base |>
    dplyr::mutate(ano = as.integer(.data$ano)) |>
    dplyr::left_join(fatores, by = "ano", relationship = "many-to-one")

  # A conferência acima cobre o período do config; a base pode trazer um ano
  # de fora dele, e aí o valor real sairia NA sem aviso.
  anos_sem_fator <- unique(com_fator$ano[is.na(com_fator$.fator_ipca)])
  if (length(anos_sem_fator) > 0) {
    cli::cli_abort(c(
      "A base tem ano sem fator do IPCA.",
      "x" = "Anos: {.val {anos_sem_fator}}",
      "i" = "Filtre a base ao período de config/config.yml ou use um IPCA que cubra esses anos."
    ))
  }

  if (nrow(com_fator) != n_antes) {
    cli::cli_abort("A junção com o IPCA mudou o número de linhas ({n_antes} para {nrow(com_fator)}).")
  }

  com_fator |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(FASES),
      ~ .x * .fator_ipca,
      .names = "{.col}_real"
    )) |>
    dplyr::select(-".fator_ipca")
}
