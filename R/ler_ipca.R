# Fatores de correção pelo IPCA (multiplicativos, base no mês de referência de
# config.yml). A conferência do fator do ano conferido fica em
# conferir_fator_ipca() (R/deflacionar.R).

ler_ipca <- function(caminho) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c("Arquivo não encontrado: {.path {caminho}}", "i" = "Confira {.code caminhos:} em {.path config/config.yml}."))
  }
  # Lido como texto para converter de forma explícita: o arquivo usa ponto
  # decimal, diferente das bases do SIOP.
  bruto <- readr::read_csv(caminho, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  conferir_colunas(bruto, c("ano", "fator_correcao"), caminho)

  ipca <- tibble::tibble(
    ano = suppressWarnings(as.integer(trimws(bruto$ano))),
    fator = suppressWarnings(as.numeric(trimws(bruto$fator_correcao)))
  )
  if (anyNA(ipca$ano) || anyNA(ipca$fator)) {
    cli::cli_abort(c(
      "Há ano ou fator que não é número em {.path {basename(caminho)}}.",
      "i" = "O arquivo deve ter {.code ano,fator_correcao} com ponto decimal."
    ))
  }
  if (anyDuplicated(ipca$ano)) {
    cli::cli_abort("Ano repetido em {.path {basename(caminho)}}: {.val {ipca$ano[duplicated(ipca$ano)]}}.")
  }
  dplyr::arrange(ipca, ano)
}
