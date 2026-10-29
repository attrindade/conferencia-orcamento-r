# Utilidades comuns a todo o pipeline: fases, configuração, códigos do SIOP,
# números no formato brasileiro e formatação para o relatório.

# Ordem fixa das fases da despesa. Todas as tabelas seguem esta ordem para que
# o relatório e os testes possam comparar tabelas sem reordenar.
FASES <- c("projeto_lei", "dotacao_inicial", "dotacao_atual", "empenhado", "liquidado", "pago")

# Tolerância para fatores de correção (unidade "fator"): sete casas decimais,
# a precisão com que o arquivo do IPCA traz cada fator. A mesma vale na trava
# conferir_fator_ipca() e na conferência de gabaritos com unidade "fator".
TOLERANCIA_FATOR <- 1e-7

# Lê config/config.yml e resolve os caminhos relativos contra a raiz do projeto.
# Resolver aqui evita que cada função dependa da pasta de onde o R foi aberto.
#
# resolver = FALSE mantém os caminhos relativos como estão (só apara espaços).
# O pipeline usa essa opção: ele sempre roda na raiz do projeto, e um caminho
# relativo guardado nos resultados continua certo se a pasta for movida. Um
# caminho absoluto ficaria apontando para a pasta antiga sem avisar.
carregar_config <- function(caminho = here::here("config/config.yml"), resolver = TRUE) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c(
      "Não encontrei o arquivo de configuração.",
      "x" = "Caminho procurado: {.path {caminho}}",
      "i" = "Rode a partir da pasta do projeto ou confira se {.path config/config.yml} existe."
    ))
  }
  cfg <- yaml::read_yaml(caminho)
  ajustar <- if (resolver) resolver_caminho else limpar_caminho
  cfg$caminhos <- lapply(cfg$caminhos, ajustar)
  if (!is.null(cfg$ferramentas)) cfg$ferramentas <- lapply(cfg$ferramentas, ajustar)
  cfg
}

# Caminho vazio vira "", o resto só perde os espaços das pontas.
limpar_caminho <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return("")
  trimws(x)
}

# Texto de configuração com um padrão: sem a chave (ou com ela vazia), vale o
# padrão. Assim o código nunca quebra por falta de uma chave nova na
# configuração de outro projeto.
texto_da_config <- function(valor, padrao) {
  if (is.null(valor) || !nzchar(paste(valor, collapse = ""))) padrao else paste(valor, collapse = " ")
}

# Quem decide o que fazer com uma divergência sem explicação (relatorio:
# responsavel_decisao em config.yml).
responsavel_decisao <- function(cfg) {
  texto_da_config(cfg$relatorio$responsavel_decisao, "pessoa responsável pela conferência")
}

# Número com vírgula decimal e as casas pedidas (ex.: fatores de correção com
# sete casas), para mensagens e textos em português.
numero_virgula <- function(x, casas) {
  formatC(as.numeric(x), format = "f", digits = casas, decimal.mark = ",")
}

# Onde está o Quarto, nesta ordem: o caminho de config.yml (ferramentas:
# quarto), o PATH do sistema e o pacote {quarto}. Devolve "" se não achar, para
# quem chama decidir a mensagem.
localizar_quarto <- function(caminho_config = "") {
  if (!is.null(caminho_config) && nzchar(caminho_config) && file.exists(caminho_config)) {
    return(caminho_config)
  }
  no_path <- Sys.which("quarto")
  if (nzchar(no_path)) return(unname(no_path))
  do_pacote <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
  if (!is.null(do_pacote) && nzchar(do_pacote) && file.exists(do_pacote)) return(do_pacote)
  ""
}

# Caminho vazio fica vazio: é como a configuração diz que um arquivo opcional
# (por exemplo, o cache do Python na vitrine) não está disponível.
resolver_caminho <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return("")
  x <- trimws(x)
  absoluto <- grepl("^([A-Za-z]:)?[/\\\\]", x)
  if (absoluto) x else here::here(x)
}

# Os campos do SIOP vêm como "código - nome" (ex.: "Z101 - Programa Fictício").
# O código é o que identifica a ação ou o programa entre anos; o nome muda.
separar_codigo <- function(x) {
  x <- trimws(x)
  pos <- regexpr(" - ", x, fixed = TRUE)
  ifelse(!is.na(pos) & pos > 0, trimws(substr(x, 1, pos - 1)), x)
}

# Sem " - " devolve o próprio texto, como a referência em Python (cod_nome),
# para que nenhum nome fique vazio por causa do formato.
separar_nome <- function(x) {
  x <- trimws(x)
  pos <- regexpr(" - ", x, fixed = TRUE)
  ifelse(!is.na(pos) & pos > 0, trimws(substr(x, pos + 3, nchar(x))), x)
}

# Converte texto com milhar "." e decimal "," em número. Vazio e "-" valem zero,
# convenção das planilhas do SIOP (a referência em Python faz o mesmo).
# Texto que não é número vira NA com aviso, para não sumir em silêncio na soma.
numero_br <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & (x == "" | x == "-")] <- "0"
  limpo <- gsub(".", "", x, fixed = TRUE)
  limpo <- sub(",", ".", limpo, fixed = TRUE)
  valor <- suppressWarnings(as.numeric(limpo))
  falhou <- !is.na(x) & is.na(valor)
  if (any(falhou)) {
    cli::cli_warn("{sum(falhou)} valor{?es} não {?é/são} número no formato brasileiro (ex.: {.val {utils::head(x[falhou], 3)}}).")
  }
  valor
}

# Formatos brasileiros usados no relatório. Valores ausentes viram NA de texto
# para que a tabela mostre a falta em vez de um zero falso.
formatar_reais <- function(x, casas = 2) {
  texto <- formatC(abs(x), format = "f", digits = casas, big.mark = ".", decimal.mark = ",")
  texto <- paste0(ifelse(!is.na(x) & x < 0, "-", ""), "R$ ", texto)
  texto[is.na(x)] <- NA_character_
  texto
}

# Recebe o valor já em pontos percentuais (1.5 vira "1,50%"), como as colunas
# *_pct do pipeline.
formatar_pct <- function(x, casas = 2) {
  texto <- paste0(formatC(x, format = "f", digits = casas, big.mark = ".", decimal.mark = ","), "%")
  texto[is.na(x)] <- NA_character_
  texto
}

# Junta itens como se escreve em português: "1, 3, 4 e 5".
juntar_com_e <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(utils::head(x, -1), collapse = ", "), "e", utils::tail(x, 1))
}

formatar_data <- function(x) {
  if (inherits(x, "POSIXt")) x <- as.Date(x, tz = Sys.timezone())
  texto <- format(as.Date(x), "%d/%m/%Y")
  texto[is.na(x)] <- NA_character_
  texto
}

# shared_home = FALSE evita o aviso do duckdb sobre a pasta de extensões
# compartilhada. Quem abre a conexão fecha com
# on.exit(DBI::dbDisconnect(con, shutdown = TRUE)).
conectar_duckdb <- function() {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
}

# Caminho com aspas simples escapadas, para entrar no SQL do duckdb.
sql_caminho <- function(caminho) {
  paste0("'", gsub("'", "''", normalizePath(caminho, winslash = "/", mustWork = TRUE), fixed = TRUE), "'")
}

# Nome de coluna entre aspas duplas, porque as bases antigas têm acento e espaço.
sql_coluna <- function(nome) paste0('"', gsub('"', '""', nome, fixed = TRUE), '"')
