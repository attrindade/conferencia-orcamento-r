# Procedência: registra de onde veio cada número. O hash SHA-256 permite a
# qualquer pessoa conferir, depois, se está usando exatamente o mesmo arquivo.

registrar_procedencia <- function(arquivos) {
  if (is.null(names(arquivos)) || any(!nzchar(names(arquivos)))) {
    cli::cli_abort("{.arg arquivos} precisa ser um vetor nomeado {.code papel = caminho}.")
  }
  arquivos <- unlist(arquivos)

  # Caminho vazio é arquivo opcional não configurado (ex.: cache do Python na
  # vitrine): fica fora da procedência, com aviso, em vez de travar o pipeline.
  vazios <- is.na(arquivos) | !nzchar(arquivos)
  if (any(vazios)) {
    cli::cli_alert_info("Sem arquivo configurado para: {.val {names(arquivos)[vazios]}} (opcional).")
    arquivos <- arquivos[!vazios]
  }

  ausentes <- !file.exists(arquivos)
  if (any(ausentes)) {
    linhas <- paste0(names(arquivos)[ausentes], ": ", arquivos[ausentes])
    cli::cli_abort(c(
      "Arquivo de entrada não encontrado.",
      purrr::set_names(linhas, rep("x", length(linhas))),
      "i" = "Confira {.code caminhos:} em {.path config/config.yml}."
    ))
  }

  info <- file.info(arquivos)
  tibble::tibble(
    papel = names(arquivos),
    arquivo = basename(arquivos),
    caminho = normalizePath(arquivos, winslash = "/"),
    tamanho_bytes = as.double(info$size),
    modificado_em = info$mtime,
    sha256 = unname(vapply(arquivos, cli::hash_file_sha256, character(1))),
    registrado_em = Sys.time()
  )
}
