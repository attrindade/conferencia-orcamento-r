# Leitura do orçamento da União inteira, usado como denominador da
# participação e para conferir se as duas safras do SIOP batem.

COLUNAS_UNIAO_CONSOLIDADA <- c(
  ano = "Ano", orgao = "Órgão Orçamentário", programa = "Programa",
  acao = "Ação", gnd = "Grupo de Despesa",
  projeto_lei = "Projeto de Lei", dotacao_inicial = "Dotação Inicial",
  dotacao_atual = "Dotação Atual", empenhado = "Empenhado",
  liquidado = "Liquidado", pago = "Pago"
)

# Escrito por extenso (sem usar FASES) porque os arquivos de R/ são lidos em
# ordem alfabética e utils.R vem depois deste.
COLUNAS_UNIAO_NOVA <- c(
  ano = "ano", gnd = "grupo_despesa",
  projeto_lei = "projeto_lei", dotacao_inicial = "dotacao_inicial",
  dotacao_atual = "dotacao_atual", empenhado = "empenhado",
  liquidado = "liquidado", pago = "pago"
)

# A base consolidada real passa de 1 GB: não cabe agregar no R. O duckdb lê o arquivo
# uma vez, agrega no nível mais fino necessário (ano, órgão, GND, programa,
# ação) e as duas tabelas pedidas saem dessa agregação, sem reler o arquivo.
ler_uniao_consolidada <- function(caminho, cfg) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c("Arquivo não encontrado: {.path {caminho}}", "i" = "Confira {.code caminhos:} em {.path config/config.yml}."))
  }
  con <- conectar_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  mapa <- COLUNAS_UNIAO_CONSOLIDADA
  origem <- paste0("read_csv(", sql_caminho(caminho), ", delim = ',', quote = '\"', header = true, all_varchar = true)")
  cabecalho <- DBI::dbGetQuery(con, paste("DESCRIBE SELECT * FROM", origem))$column_name
  conferir_colunas(cabecalho, unname(mapa), caminho)

  # Código = texto antes do primeiro " - ", igual a separar_codigo() no R.
  codigo <- function(col) paste0("trim(split_part(", sql_coluna(mapa[[col]]), ", ' - ', 1))")
  numeros <- vapply(mapa[FASES], sql_numero, character(1), milhar_ponto = TRUE)
  somas <- paste0("sum(", numeros, ") AS ", FASES)
  # Conta, por fase, quantos valores não viraram número: precisa ser zero.
  falhas <- paste0("count_if(", numeros, " IS NULL) AS falha_", FASES)

  # As linhas "Total" (Ano não numérico) são subtotais da planilha e
  # duplicariam a soma: ficam de fora.
  sql <- paste0(
    "CREATE TEMP TABLE agregada AS SELECT CAST(trim(", sql_coluna(mapa[["ano"]]), ") AS INTEGER) AS ano, ",
    codigo("orgao"), " AS orgao_cod, ", codigo("gnd"), " AS gnd_cod, ",
    codigo("programa"), " AS programa_cod, ", codigo("acao"), " AS acao_cod, ",
    paste(c(somas, falhas), collapse = ", "),
    ", count(*) AS n_linhas FROM ", origem,
    " WHERE TRY_CAST(trim(", sql_coluna(mapa[["ano"]]), ") AS INTEGER) IS NOT NULL",
    " GROUP BY ALL"
  )
  DBI::dbExecute(con, sql)

  n_falhas <- DBI::dbGetQuery(con, paste0("SELECT ", paste0("sum(falha_", FASES, ") AS ", FASES, collapse = ", "), " FROM agregada"))
  n_falhas <- unlist(n_falhas)
  if (any(n_falhas > 0, na.rm = TRUE)) {
    problema <- n_falhas[n_falhas > 0]
    exemplos <- exemplos_sem_formato(con, origem, mapa[[names(problema)[1]]], milhar_ponto = TRUE)
    cli::cli_abort(c(
      "Valores que não consegui converter em {.path {basename(caminho)}}.",
      "x" = "{paste0(names(problema), ': ', problema, ' linha(s)', collapse = '; ')}",
      if (length(exemplos) > 0) c("x" = "Exemplos fora do formato: {.val {exemplos}}"),
      "i" = "Confira o formato dos números (milhar com ponto, decimal com vírgula)."
    ))
  }

  agrupar <- function(chaves) {
    tibble::as_tibble(DBI::dbGetQuery(con, paste0(
      "SELECT ", paste(chaves, collapse = ", "), ", ",
      paste0("sum(", FASES, ") AS ", FASES, collapse = ", "),
      " FROM agregada GROUP BY ALL ORDER BY ", paste(chaves, collapse = ", ")
    )))
  }
  por_orgao_gnd <- agrupar(c("ano", "orgao_cod", "gnd_cod"))
  por_programa_acao <- agrupar(c("ano", "programa_cod", "acao_cod"))
  por_orgao_gnd$ano <- as.integer(por_orgao_gnd$ano)
  por_programa_acao$ano <- as.integer(por_programa_acao$ano)

  list(por_orgao_gnd = por_orgao_gnd, por_programa_acao = por_programa_acao)
}

# A base nova da União já vem somada por ano e GND (sem órgão), por isso só
# serve para conferir a União inteira e estimar o ano que a antiga não fecha.
ler_uniao_nova <- function(caminho) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c("Arquivo não encontrado: {.path {caminho}}", "i" = "Confira {.code caminhos:} em {.path config/config.yml}."))
  }
  con <- conectar_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  mapa <- COLUNAS_UNIAO_NOVA
  origem <- paste0("read_csv(", sql_caminho(caminho), ", delim = ';', header = true, all_varchar = true)")
  cabecalho <- DBI::dbGetQuery(con, paste("DESCRIBE SELECT * FROM", origem))$column_name
  conferir_colunas(cabecalho, unname(mapa), caminho)

  numeros <- vapply(mapa[FASES], sql_numero, character(1), milhar_ponto = FALSE)
  sql <- paste0(
    "SELECT TRY_CAST(trim(", sql_coluna(mapa[["ano"]]), ") AS INTEGER) AS ano, ",
    "trim(split_part(", sql_coluna(mapa[["gnd"]]), ", ' - ', 1)) AS gnd_cod, ",
    paste0("sum(", numeros, ") AS ", FASES, collapse = ", "),
    ", ", paste0("count_if(", numeros, " IS NULL) AS falha_", FASES, collapse = ", "),
    " FROM ", origem, " GROUP BY ALL ORDER BY ano, gnd_cod"
  )
  bruto <- tibble::as_tibble(DBI::dbGetQuery(con, sql))

  n_falhas <- colSums(bruto[paste0("falha_", FASES)])
  if (any(n_falhas > 0) || anyNA(bruto$ano)) {
    fase_falha <- FASES[n_falhas > 0]
    exemplos <- if (length(fase_falha) > 0) exemplos_sem_formato(con, origem, mapa[[fase_falha[1]]], milhar_ponto = FALSE) else character()
    cli::cli_abort(c(
      "Valores que não consegui converter em {.path {basename(caminho)}}.",
      if (length(fase_falha) > 0) c("x" = "{paste0(fase_falha, ': ', n_falhas[n_falhas > 0], ' linha(s)', collapse = '; ')}"),
      if (length(exemplos) > 0) c("x" = "Exemplos fora do formato: {.val {exemplos}}"),
      "i" = "Confira o ano e o formato dos números (sem milhar, decimal com vírgula)."
    ))
  }
  bruto$ano <- as.integer(bruto$ano)
  bruto[c("ano", "gnd_cod", FASES)]
}
