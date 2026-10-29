# Leitura do recorte de juventude do SIOP nas duas safras (a extração nova e
# a anterior, com cabeçalho em português). As duas saem no mesmo esquema
# (BASE_JUVENTUDE) para que a comparação entre safras seja linha a linha.

# Esquema comum às duas safras, na ordem do contrato. As fases vão por extenso
# (sem usar FASES) porque os arquivos de R/ são lidos em ordem alfabética e
# utils.R vem depois deste.
BASE_JUVENTUDE <- c(
  "ano", "orgao_cod", "orgao_nome", "uo_cod", "programa_cod", "programa_nome",
  "acao_cod", "acao_nome", "gnd_cod", "modalidade_cod", "funcao_cod", "eixo",
  "flag_palavra_chave", "flag_programa_relacionado", "palavras_chave",
  "projeto_lei", "dotacao_inicial", "dotacao_atual", "empenhado", "liquidado", "pago"
)

# Para quando o arquivo mudou de formato: é melhor parar dizendo qual coluna
# falta do que somar uma coluna errada.
conferir_colunas <- function(df_ou_nomes, esperadas, arquivo) {
  nomes <- if (is.character(df_ou_nomes)) df_ou_nomes else names(df_ou_nomes)
  faltam <- setdiff(esperadas, nomes)
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "O arquivo {.path {basename(arquivo)}} não tem as colunas esperadas.",
      "x" = "Faltam: {.val {faltam}}",
      "i" = "Confira se o caminho em {.path config/config.yml} aponta para a extração certa do SIOP."
    ))
  }
  invisible(TRUE)
}

# Mapa "nome no esquema comum" -> "nome no arquivo" de cada safra.
COLUNAS_JUVENTUDE_NOVA <- c(
  ano = "ano", orgao = "orgao_orcamentario", uo = "unidade_orcamentaria",
  programa = "programa", acao = "acao", gnd = "grupo_despesa",
  modalidade = "modalidade_aplicacao", funcao = "funcao", eixo = "eixo_tematico",
  flag_palavra_chave = "flag_palavra_chave",
  flag_programa_relacionado = "flag_programa_relacionado",
  palavras_chave = "palavras_chave_encontradas",
  projeto_lei = "projeto_lei", dotacao_inicial = "dotacao_inicial",
  dotacao_atual = "dotacao_atual", empenhado = "empenhado",
  liquidado = "liquidado", pago = "pago"
)

# A safra antiga também tem colunas "... Corrigido": ficam de fora de propósito,
# porque não estão deflacionadas (medido na fase 0).
COLUNAS_JUVENTUDE_ANTIGA <- c(
  ano = "Ano", orgao = "Órgão Orçamentário", uo = "Unidade Orçamentária",
  programa = "Programa", acao = "Ação", gnd = "Grupo de Despesa",
  modalidade = "Modalidade de Aplicação", funcao = "Função", eixo = "Eixo Temático",
  flag_palavra_chave = "flag_palavra_chave",
  flag_programa_relacionado = "flag_programa_relacionado",
  palavras_chave = "palavras_chave_encontradas",
  projeto_lei = "Projeto de Lei", dotacao_inicial = "Dotação Inicial",
  dotacao_atual = "Dotação Atual", empenhado = "Empenhado",
  liquidado = "Liquidado", pago = "Pago"
)

# Formatos aceitos para os números, conferidos antes de converter. Sem essa
# conferência, "1.234" em um arquivo sem separador de milhar viraria 1,234, e
# "1234.56" em um arquivo com milhar viraria 123456, sem nenhum aviso.
#   com milhar "."  : 1234 ou 1.234.567, com decimal opcional ",5"
#   sem milhar      : 1234567, com decimal opcional ",5"
FORMATO_NUMERO_MILHAR <- "-?([0-9]{1,3}(\\.[0-9]{3})+|[0-9]+)(,[0-9]+)?"
FORMATO_NUMERO_SIMPLES <- "-?[0-9]+(,[0-9]+)?"

# Expressão SQL que converte texto em número. A safra nova não tem separador
# de milhar; a antiga usa "." como milhar. Vazio e "-" valem zero (convenção
# do SIOP, igual à referência em Python). O que não casar com o formato
# esperado vira NULL e é barrado depois (contado como falha), em vez de virar
# zero ou outro número.
sql_numero <- function(coluna, milhar_ponto) {
  bruto <- paste0("trim(", sql_coluna(coluna), ")")
  formato <- if (milhar_ponto) FORMATO_NUMERO_MILHAR else FORMATO_NUMERO_SIMPLES
  limpo <- if (milhar_ponto) paste0("replace(replace(", bruto, ", '.', ''), ',', '.')") else paste0("replace(", bruto, ", ',', '.')")
  paste0(
    "CASE WHEN coalesce(", bruto, ", '') IN ('', '-') THEN 0",
    " WHEN regexp_full_match(", bruto, ", '", formato, "') THEN TRY_CAST(", limpo, " AS DOUBLE)",
    " ELSE NULL END"
  )
}

ler_juventude_nova <- function(caminho) {
  ler_juventude_safra(caminho, COLUNAS_JUVENTUDE_NOVA, milhar_ponto = FALSE)
}

ler_juventude_antiga <- function(caminho) {
  ler_juventude_safra(caminho, COLUNAS_JUVENTUDE_ANTIGA, milhar_ponto = TRUE)
}

# Lê tudo como texto (all_varchar) e converte de forma explícita: assim nenhum
# código como "0001" perde o zero à esquerda e nenhum número é adivinhado.
ler_juventude_safra <- function(caminho, mapa, milhar_ponto) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c("Arquivo não encontrado: {.path {caminho}}", "i" = "Confira {.code caminhos:} em {.path config/config.yml}."))
  }
  con <- conectar_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  origem <- paste0("read_csv(", sql_caminho(caminho), ", delim = ';', header = true, all_varchar = true)")
  cabecalho <- DBI::dbGetQuery(con, paste("DESCRIBE SELECT * FROM", origem))$column_name
  conferir_colunas(cabecalho, unname(mapa), caminho)

  texto <- c("orgao", "uo", "programa", "acao", "gnd", "modalidade", "funcao", "eixo", "palavras_chave", "flag_palavra_chave", "flag_programa_relacionado")
  selecao <- c(
    paste0("TRY_CAST(trim(", sql_coluna(mapa[["ano"]]), ") AS INTEGER) AS ano"),
    paste0("trim(", sql_coluna(mapa[texto]), ") AS ", texto),
    paste0(vapply(mapa[FASES], sql_numero, character(1), milhar_ponto = milhar_ponto), " AS ", FASES)
  )
  bruto <- DBI::dbGetQuery(con, paste("SELECT", paste(selecao, collapse = ",\n "), "FROM", origem))

  base <- tibble::tibble(
    ano = as.integer(bruto$ano),
    orgao_cod = separar_codigo(bruto$orgao),
    orgao_nome = separar_nome(bruto$orgao),
    uo_cod = separar_codigo(bruto$uo),
    programa_cod = separar_codigo(bruto$programa),
    programa_nome = separar_nome(bruto$programa),
    acao_cod = separar_codigo(bruto$acao),
    acao_nome = separar_nome(bruto$acao),
    gnd_cod = separar_codigo(bruto$gnd),
    modalidade_cod = separar_codigo(bruto$modalidade),
    funcao_cod = separar_codigo(bruto$funcao),
    # Eixo vazio vira "na", o mesmo rótulo que a extração já usa para "sem eixo".
    eixo = dplyr::if_else(is.na(bruto$eixo) | bruto$eixo == "", "na", bruto$eixo),
    flag_palavra_chave = texto_para_logico(bruto$flag_palavra_chave),
    flag_programa_relacionado = texto_para_logico(bruto$flag_programa_relacionado),
    palavras_chave = dplyr::if_else(is.na(bruto$palavras_chave) | bruto$palavras_chave == "", NA_character_, bruto$palavras_chave)
  )
  base <- dplyr::bind_cols(base, tibble::as_tibble(bruto[FASES]))

  # Se algum valor não casou com o formato, busca até três exemplos para a
  # mensagem de erro dizer o que foi encontrado.
  exemplos <- character()
  fase_falha <- FASES[vapply(FASES, function(f) anyNA(base[[f]]), logical(1))]
  if (length(fase_falha) > 0) {
    exemplos <- exemplos_sem_formato(con, origem, mapa[[fase_falha[1]]], milhar_ponto)
  }
  barrar_conversoes_falhas(base, caminho, exemplos)
  base[BASE_JUVENTUDE]
}

# A extração grava as flags como "True"/"False" (texto do Python).
texto_para_logico <- function(x) {
  dplyr::case_when(x == "True" ~ TRUE, x == "False" ~ FALSE, .default = NA)
}

# Depois da conversão não pode haver NA em ano, flags nem valores: NA ali
# significa texto fora do formato esperado, e somar por cima esconderia o erro.
barrar_conversoes_falhas <- function(base, caminho, exemplos = character()) {
  colunas <- c("ano", "flag_palavra_chave", "flag_programa_relacionado", FASES)
  n_na <- vapply(base[colunas], function(x) sum(is.na(x)), integer(1))
  if (any(n_na > 0)) {
    problema <- n_na[n_na > 0]
    cli::cli_abort(c(
      "Valores que não consegui converter em {.path {basename(caminho)}}.",
      "x" = "{paste0(names(problema), ': ', problema, ' linha(s)', collapse = '; ')}",
      if (length(exemplos) > 0) c("x" = "Exemplos fora do formato: {.val {exemplos}}"),
      "i" = "Abra o arquivo e veja o formato dessas colunas (separador de milhar e decimal, flags True/False)."
    ))
  }
  invisible(TRUE)
}

# Até três valores de uma coluna que não casam com o formato numérico
# esperado (para a mensagem de erro). Consulta à parte, feita só quando há
# falha, para não pesar na leitura normal.
exemplos_sem_formato <- function(con, origem, coluna, milhar_ponto) {
  expressao <- sql_numero(coluna, milhar_ponto)
  sql <- paste0("SELECT DISTINCT trim(", sql_coluna(coluna), ") AS v FROM ", origem,
                " WHERE (", expressao, ") IS NULL LIMIT 3")
  tryCatch(DBI::dbGetQuery(con, sql)$v, error = function(e) character())
}
