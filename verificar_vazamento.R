# Checagem anti-vazamento da vitrine.
#
# Procura, em todos os arquivos de texto que podem ir para o repositório,
# qualquer coisa que identifique os dados reais da conferência privada:
#   - códigos e nomes de ação e de programa do recorte, e as palavras-chave do
#     filtro;
#   - os números-gabarito e os valores calculados (em reais, percentuais,
#     contagens e listas de anos), em várias formas de escrever;
#   - todos os fatores de correção do IPCA do arquivo privado;
#   - os números do capítulo de orçamento (capítulo 6) do Relatório Final;
#   - nomes de pessoas entrevistadas e da equipe;
#   - caminhos de pasta de usuário (C:/Users/<nome>) e endereços de e-mail.
# A lista proibida nunca é copiada para cá: é lida da pasta privada na hora,
# para que a própria checagem não vaze nada.
#
# Uso, a partir da raiz do repositório:
#   Rscript verificar_vazamento.R --privado <pasta da conferência privada>
#     (ou defina a variável de ambiente CONFERENCIA_PRIVADO)
# Opções:
#   --raiz <pasta>             checa os arquivos desta pasta (todos), e não os
#                              que o git lista na vitrine; usado pelos hooks,
#                              que exportam para uma pasta temporária o que vai
#                              para o commit ou para o push
#   --relatorio-final <arq>    padrão: "Relatório Final.md" na pasta acima da
#                              privada (ou CONFERENCIA_RELATORIO_FINAL)
#   --manifesto <arq>          lista das entrevistas; padrão:
#                              revisor_app/static/manifesto.json na pasta acima
#                              da privada (ou CONFERENCIA_MANIFESTO)
#   --config, --recorte, --gabaritos <arq>   trocam os arquivos privados
#
# Resultado (status de saída):
#   0  tudo checado e nada encontrado
#   1  encontrou algo: lista arquivo:linha: termo [tipo]. Se também houver
#      arquivo não checado, ele aparece na mesma saída.
#   2  NÃO CHECADO: sem pasta privada, listas pequenas demais, arquivo que não
#      deu para ler, ou erro da própria checagem. Nunca conta como "passou".

# Pisos de sanidade: se a pasta privada indicada render listas menores que
# estas, ela não é a pasta certa (ou está incompleta) e a checagem não vale.
PISOS <- c(
  "ações no recorte" = 100,
  "códigos de ação ou programa" = 100,
  "nomes de ação ou programa" = 100,
  "palavras-chave" = 3,
  "valores dos gabaritos" = 100,
  "fatores do IPCA" = 15,
  "números do capítulo de orçamento" = 50,
  "nomes de pessoas" = 10
)
# Capítulo de orçamento do Relatório Final: do título numerado 6 até o
# próximo título numerado.
CAPITULO_INICIO <- "^\\s*6\\.\\s*#"
CAPITULO_FIM <- "^\\s*[0-9]+\\.\\s*#"

# Status 2 sai por aqui: a mensagem diz o que faltou e o que fazer.
sair_sem_checar <- function(...) {
  cat("\nNÃO CHECADO:", ..., "\n", sep = " ")
  cat("A checagem anti-vazamento não terminou. Isto NÃO significa que o repositório está limpo.\n")
  quit(save = "no", status = 2)
}

# ---------------------------------------------------------------------------
# Argumentos
# ---------------------------------------------------------------------------
ler_argumentos <- function(args) {
  validos <- c("--privado", "--config", "--recorte", "--gabaritos", "--raiz",
               "--relatorio-final", "--manifesto")
  saida <- list()
  i <- 1
  while (i <= length(args)) {
    if (!args[i] %in% validos || i == length(args)) {
      sair_sem_checar(sprintf("argumento não reconhecido ou sem valor: %s. Válidos: %s <valor>.",
                              args[i], paste(validos, collapse = ", ")))
    }
    saida[[sub("^--", "", args[i])]] <- args[i + 1]
    i <- i + 2
  }
  saida
}

# Valor de um argumento, senão de uma variável de ambiente, senão o padrão.
opcao <- function(args, nome, variavel = "", padrao = NA_character_) {
  if (!is.null(args[[nome]])) return(args[[nome]])
  if (nzchar(variavel) && nzchar(Sys.getenv(variavel))) return(Sys.getenv(variavel))
  padrao
}

# ---------------------------------------------------------------------------
# Leitura de texto: UTF-8 (com ou sem BOM), UTF-16 (com BOM, ou sem BOM mas
# com cara de UTF-16) e latin1. O que não der para ler vira NA, e o arquivo
# entra na lista de não checados.
# ---------------------------------------------------------------------------
ler_texto <- function(caminho) {
  tam <- file.size(caminho)
  if (is.na(tam)) return(list(texto = NA_character_, nota = "listado, mas não existe na pasta"))
  if (tam == 0) return(list(texto = "", nota = ""))
  b <- readBin(caminho, "raw", n = tam)
  decodificar <- function(bytes, de) {
    out <- tryCatch(iconv(list(bytes), from = de, to = "UTF-8"), error = function(e) NA_character_)
    if (length(out) != 1) NA_character_ else out
  }
  if (length(b) >= 3 && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) b <- b[-(1:3)]
  if (length(b) >= 2 && identical(b[1:2], as.raw(c(0xff, 0xfe)))) {
    return(list(texto = decodificar(b[-(1:2)], "UTF-16LE"), nota = "lido como UTF-16"))
  }
  if (length(b) >= 2 && identical(b[1:2], as.raw(c(0xfe, 0xff)))) {
    return(list(texto = decodificar(b[-(1:2)], "UTF-16BE"), nota = "lido como UTF-16"))
  }
  nul <- b == as.raw(0)
  if (any(nul)) {
    # UTF-16 sem BOM: os bytes nulos ficam quase todos nas posições pares
    # (little endian) ou ímpares (big endian). Fora disso é binário.
    par <- seq_along(b) %% 2 == 0
    if (length(b) %% 2 == 0 && mean(nul[par]) > 0.3 && !any(nul[!par])) {
      return(list(texto = decodificar(b, "UTF-16LE"), nota = "lido como UTF-16"))
    }
    if (length(b) %% 2 == 0 && mean(nul[!par]) > 0.3 && !any(nul[par])) {
      return(list(texto = decodificar(b, "UTF-16BE"), nota = "lido como UTF-16"))
    }
    return(list(texto = NA_character_, nota = "binário (não é texto)"))
  }
  txt <- rawToChar(b)
  if (validUTF8(txt)) return(list(texto = txt, nota = ""))
  list(texto = decodificar(b, "latin1"), nota = "lido como latin1")
}

principal <- function() {
  args <- ler_argumentos(commandArgs(trailingOnly = TRUE))
  raiz_script <- local({
    arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(arg) == 1) normalizePath(dirname(sub("^--file=", "", arg)), winslash = "/")
    else normalizePath(".", winslash = "/")
  })
  privado <- opcao(args, "privado", "CONFERENCIA_PRIVADO")
  if (is.na(privado) || !nzchar(privado)) {
    sair_sem_checar("não sei onde está a pasta privada da conferência.",
                    "Rode com --privado <pasta> ou defina a variável de ambiente CONFERENCIA_PRIVADO.")
  }
  if (!dir.exists(privado)) {
    sair_sem_checar(sprintf("a pasta privada não existe nesta máquina: %s.", privado),
                    "Confira --privado ou CONFERENCIA_PRIVADO.")
  }
  privado <- normalizePath(privado, winslash = "/")
  acima <- dirname(privado)

  # Pacotes: a vitrine pode não ter o renv restaurado ainda; nesse caso usamos
  # a biblioteca do renv da pasta privada, que tem os mesmos pacotes.
  PACOTES <- c("yaml", "stringi", "DBI", "duckdb", "jsonlite")
  faltando <- function() PACOTES[!vapply(PACOTES, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltando()) > 0) {
    libs <- Sys.glob(file.path(privado, "renv", "library", "*", "*", "*"))
    .libPaths(c(libs, .libPaths()))
  }
  if (length(faltando()) > 0) {
    sair_sem_checar("faltam os pacotes", paste(faltando(), collapse = ", "),
                    "(rode renv::restore() na vitrine).")
  }

  # -------------------------------------------------------------------------
  # Arquivos privados
  # -------------------------------------------------------------------------
  arq_config <- opcao(args, "config", padrao = file.path(privado, "config", "config.yml"))
  if (!file.exists(arq_config)) {
    sair_sem_checar(sprintf("não achei o config.yml privado em %s;", arq_config),
                    "sem ele não sei onde estão o recorte, a base, os gabaritos e o IPCA.")
  }
  cfg <- yaml::read_yaml(arq_config)
  resolver <- function(p) {
    if (is.null(p) || !nzchar(p)) return(NA_character_)
    if (grepl("^([A-Za-z]:|/)", p)) p else file.path(privado, p)
  }
  exigir <- function(caminho, papel) {
    if (is.na(caminho) || !file.exists(caminho)) {
      sair_sem_checar(sprintf("não achei %s em %s.", papel, caminho))
    }
    caminho
  }
  arq_recorte   <- exigir(opcao(args, "recorte", padrao = resolver(cfg$caminhos$recorte_acoes)),
                          "a lista de ações do recorte (recorte_acoes)")
  arq_base      <- exigir(resolver(cfg$caminhos$juventude_nova), "a base do recorte (juventude_nova)")
  arq_gabaritos <- exigir(opcao(args, "gabaritos", padrao = resolver(cfg$caminhos$gabaritos)),
                          "os gabaritos")
  arq_ipca      <- exigir(resolver(cfg$caminhos$ipca), "o arquivo de fatores do IPCA (caminhos: ipca)")
  arq_relatorio <- exigir(opcao(args, "relatorio-final", "CONFERENCIA_RELATORIO_FINAL",
                                file.path(acima, "Relatório Final.md")),
                          "o Relatório Final (--relatorio-final)")
  arq_manifesto <- exigir(opcao(args, "manifesto", "CONFERENCIA_MANIFESTO",
                                file.path(acima, "revisor_app", "static", "manifesto.json")),
                          "a lista das entrevistas (--manifesto)")
  arq_diverg    <- file.path(resolver(cfg$caminhos$saidas), "divergencias.csv")

  # -------------------------------------------------------------------------
  # Pasta checada e exceções
  # -------------------------------------------------------------------------
  modo_pasta <- !is.null(args$raiz)
  raiz <- if (modo_pasta) args$raiz else raiz_script
  if (!dir.exists(raiz)) sair_sem_checar(sprintf("a pasta a checar não existe: %s.", raiz))
  raiz <- normalizePath(raiz, winslash = "/")
  # Exceções e pastas ignoradas ficam à vista em config/vazamento.yml (a versão
  # da pasta checada). Sem ele, a checagem fica mais rígida (nenhuma exceção).
  regras_arq <- file.path(raiz, "config", "vazamento.yml")
  regras <- if (file.exists(regras_arq)) yaml::read_yaml(regras_arq) else {
    cat("Aviso: config/vazamento.yml não encontrado; sem exceções.\n")
    list(pastas_ignoradas = ".git")
  }
  lista_de <- function(x) as.character(unlist(x))

  # -------------------------------------------------------------------------
  # Montagem da lista proibida
  # -------------------------------------------------------------------------
  dobrar <- function(x) stringi::stri_trans_general(x, "Latin-ASCII; Lower")
  sem_espaco_especial <- function(x) gsub("[\u00a0\u202f\u2007\u2009\u200a]", " ", x)
  # Para nomes: sem acento, sem caixa e com espaços (inclusive quebras) reduzidos a um.
  dobrar_texto <- function(x) gsub("\\s+", " ", dobrar(sem_espaco_especial(x)), perl = TRUE)
  separar_codigo <- function(x) trimws(sub(" - .*$", "", x))
  separar_nome <- function(x) ifelse(grepl(" - ", x, fixed = TRUE), trimws(sub("^.*? - ", "", x, perl = TRUE)), NA_character_)
  ler_csv <- function(caminho) {
    utils::read.csv(caminho, colClasses = "character", encoding = "UTF-8", fileEncoding = "UTF-8-BOM",
                    check.names = FALSE, na.strings = character())
  }
  q <- function(x) paste0("\\Q", x, "\\E")
  # Fronteiras de número: não vale pedaço de outro número ("12,345" não casa
  # com "2,34") nem de palavra.
  ANTES_NUM <- "(?<![0-9A-Za-z])(?<![0-9][.,])"
  DEPOIS_NUM <- "(?![0-9A-Za-z])(?![.,][0-9])"
  CTX_PCT <- "\\s?(?:%|por\\s+cento|p\\.\\s?p\\.|pontos?\\b)"
  CTX_DINHEIRO_DEPOIS <- "\\s?(?i:bi\\b|bilh|mi\\b|milh)"
  CTX_DINHEIRO_ANTES <- "R\\$\\s?"
  numero_puro <- function(s) paste0(ANTES_NUM, q(s), DEPOIS_NUM)
  numero_pct <- function(s) paste0(ANTES_NUM, q(s), CTX_PCT)
  SEM_TAMANHO <- "(?!\\s?(?i:[kmg]b\\b|bytes?\\b))"
  # Algarismos significativos de um número escrito (sem separadores, sem zeros
  # à esquerda e sem zeros finais). Formas que valem sozinhas, sem contexto,
  # precisam de pelo menos 3: "0,00" ou "1,000" não identificam nada.
  significativos <- function(s) nchar(sub("0+$", "", sub("^0+", "", gsub("[^0-9]", "", s))))
  numero_dinheiro <- function(s) {
    paste0("(?:", CTX_DINHEIRO_ANTES, q(s), DEPOIS_NUM, "|", ANTES_NUM, q(s), CTX_DINHEIRO_DEPOIS, ")")
  }
  ponto_virgula <- function(s) unique(c(s, chartr(".", ",", s)))
  # termo: o que aparece no relatório da checagem; fixo: filtro rápido;
  # regex: a busca de verdade; campo: "num" (texto com espaços normalizados)
  # ou "dob" (sem acento e sem caixa).
  termo_df <- function(termo, tipo, fixo, regex, campo) {
    if (length(termo) == 0) return(NULL)
    data.frame(termo = termo, tipo = tipo, fixo = fixo, regex = regex, campo = campo)
  }

  recorte <- ler_csv(arq_recorte)
  if (!all(c("acao_cod", "acao_nome") %in% names(recorte))) {
    sair_sem_checar("o recorte não tem as colunas acao_cod e acao_nome:", arq_recorte)
  }
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  base <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT programa, acao, palavras_chave_encontradas AS chaves
       FROM read_csv('%s', delim = ';', header = true, all_varchar = true)",
    gsub("'", "''", arq_base)))
  DBI::dbDisconnect(con, shutdown = TRUE)

  programas_recorte <- if ("programas" %in% names(recorte)) unlist(strsplit(recorte$programas, "|", fixed = TRUE)) else character()
  codigos <- unique(c(recorte$acao_cod, separar_codigo(base$acao), separar_codigo(base$programa), programas_recorte))
  codigos <- unique(dobrar(codigos[nzchar(codigos)]))
  nomes <- unique(c(recorte$acao_nome, separar_nome(base$acao), separar_nome(base$programa)))
  nomes <- unique(dobrar_texto(nomes[!is.na(nomes)]))
  chaves <- unique(dobrar_texto(trimws(unlist(strsplit(base$chaves[!is.na(base$chaves)], ",", fixed = TRUE)))))
  # Textos com menos de 4 caracteres casariam com qualquer coisa e não
  # identificam o recorte sozinhos.
  nomes <- nomes[nchar(nomes) >= 4]
  chaves <- chaves[nchar(chaves) >= 4]
  codigos <- codigos[nchar(codigos) >= 4]

  eh_ano <- function(x) grepl("^[0-9]{4}$", x) & suppressWarnings(as.integer(x)) %in% 1990:2035
  regex_codigo <- function(c) {
    if (eh_ano(c)) {
      # Códigos de programa iguais a anos: só vale a forma de código
      # ("2012 - Nome") ou dentro de uma lista separada por "|".
      return(paste0("(?<![0-9a-z])", q(c), "(?=\\s+-\\s)|(?<=\\|)", q(c), "(?![0-9a-z])|(?<![0-9a-z])", q(c), "(?=\\|)"))
    }
    paste0("(?<![0-9a-z])(?<![0-9][.,])", q(c), "(?![0-9a-z])(?![.,][0-9])")
  }
  # O "_" conta como parte da palavra para que nomes de variável como abc_xyz
  # não disparem por um pedaço.
  regex_texto <- function(t) paste0("(?<![0-9a-z_])", q(t), "(?![0-9a-z_])")

  termos <- list(
    termo_df(codigos, "código de ação ou programa", codigos, vapply(codigos, regex_codigo, ""), "dob"),
    termo_df(nomes, "nome de ação ou programa", nomes, regex_texto(nomes), "dob"),
    termo_df(chaves, "palavra-chave do recorte", chaves, regex_texto(chaves), "dob")
  )

  # Valores em reais: formas longas valem sozinhas; formas curtas (bilhões com
  # 3 casas, milhões) só com contexto de dinheiro, para não casar por acaso.
  # Bilhões com 1 e 2 casas continuam valendo sozinhos, como na primeira versão.
  variantes_reais <- function(v) {
    v <- abs(v)
    if (is.na(v) || v == 0) return(NULL)
    inteiro <- formatC(round(v), format = "f", digits = 0, big.mark = "")
    milhar <- formatC(round(v), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
    centavos <- sprintf("%.2f", v)
    milhar_centavos <- formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
    puros <- c(inteiro, milhar, ponto_virgula(centavos), milhar_centavos)
    curtos <- character()
    bilhoes <- character()
    if (v >= 1e9) {
      bilhoes <- c(ponto_virgula(sprintf("%.1f", v / 1e9)), ponto_virgula(sprintf("%.2f", v / 1e9)))
      curtos <- c(curtos, ponto_virgula(sprintf("%.3f", v / 1e9)))
    }
    if (v >= 1e6) {
      for (casas in 1:2) {
        m <- v / 1e6
        curtos <- c(curtos, ponto_virgula(sprintf(paste0("%.", casas, "f"), m)),
                    formatC(m, format = "f", digits = casas, big.mark = ".", decimal.mark = ","))
      }
    }
    puros <- unique(puros[nchar(puros) >= 4 & significativos(puros) >= 3])
    bilhoes <- setdiff(unique(bilhoes[nchar(bilhoes) >= 4]), puros)
    curtos <- setdiff(unique(curtos[nchar(curtos) >= 3]), c(puros, bilhoes))
    rbind(termo_df(puros, "valor em reais", puros, numero_puro(puros), "num"),
          # Bilhões com 1 e 2 casas valem sem contexto, menos quando o número é
          # um tamanho de arquivo ("12,7 KB", na tabela de procedência).
          termo_df(bilhoes, "valor em reais", bilhoes, paste0(numero_puro(bilhoes), SEM_TAMANHO), "num"),
          termo_df(curtos, "valor em reais (com R$, bi ou mi)", curtos, numero_dinheiro(curtos), "num"))
  }
  # Percentuais: com 3 casas ou mais, e na precisão completa, valem sozinhos
  # (é como aparecem em código e em planilha); com 1 ou 2 casas, só com "%",
  # "por cento", "p.p." ou "pontos".
  variantes_pct <- function(v) {
    v <- abs(v)
    if (is.na(v) || v == 0) return(NULL)
    completo <- format(v, digits = 15, scientific = FALSE, trim = TRUE)
    longos <- c(ponto_virgula(sprintf("%.3f", v)), ponto_virgula(sprintf("%.4f", v)))
    if (grepl("\\.[0-9]{3,}$", completo)) longos <- c(longos, ponto_virgula(completo))
    curtos <- c(ponto_virgula(sprintf("%.2f", v)), ponto_virgula(sprintf("%.1f", v)))
    if (!grepl("\\.[0-9]{3,}$", completo)) curtos <- c(curtos, ponto_virgula(completo))
    longos <- unique(longos[nchar(longos) >= 4 & significativos(longos) >= 3])
    curtos <- setdiff(unique(curtos[nchar(curtos) >= 3]), longos)
    # Com 3 casas, sem "%": não vale quando o número está escrito como
    # dinheiro ("R$ 4,321 bi" não é percentual).
    rbind(termo_df(longos, "percentual", longos,
                   paste0("(?<!R\\$)(?<!R\\$ )", numero_puro(longos), "(?!\\s?(?i:bi\\b|bilh|mi\\b|milh))"), "num"),
          termo_df(curtos, "percentual (com %, por cento, p.p. ou pontos)", curtos, numero_pct(curtos), "num"))
  }
  # Contagens (n de programas, de ações) só com a palavra ao lado; listas de
  # anos (ranking) como sequência.
  variantes_contagem <- function(v) {
    if (is.na(v) || v < 10) return(NULL)
    s <- format(v, scientific = FALSE, trim = TRUE)
    termo_df(paste(s, "(contagem)"), "contagem", s,
             paste0("(?<![0-9a-z.,])", q(s), "\\s+(?:programas?|acoes|acao|linhas)\\b"), "dob")
  }
  variantes_lista <- function(s) {
    partes <- strsplit(s, "|", fixed = TRUE)[[1]]
    if (length(partes) < 3 || !all(eh_ano(partes))) return(NULL)
    rx <- paste0("(?<![0-9])", paste(partes, collapse = "(?:\\s*[,;|/]\\s*|\\s+e\\s+|\\s+)"), "(?![0-9])")
    termo_df(paste(partes, collapse = ", "), "lista de anos", partes[1], rx, "dob")
  }
  valores_de <- function(df, coluna_valor) {
    if (!all(c(coluna_valor, "unidade") %in% names(df))) return(NULL)
    out <- list()
    for (i in seq_len(nrow(df))) {
      u <- df$unidade[i]
      s <- df[[coluna_valor]][i]
      v <- suppressWarnings(as.numeric(s))
      if (!u %in% c("reais", "pct", "contagem", "lista")) next
      out[[i]] <- switch(u,
        reais = variantes_reais(v),
        pct = variantes_pct(v),
        contagem = variantes_contagem(v),
        lista = variantes_lista(s),
        NULL)
    }
    do.call(rbind, out)
  }
  gabaritos <- ler_csv(arq_gabaritos)
  n_valores <- sum(gabaritos$unidade %in% c("reais", "pct"))
  numeros <- list(valores_de(gabaritos, "valor_esperado"))
  if (file.exists(arq_diverg)) {
    diverg <- ler_csv(arq_diverg)
    # Só os valores; a diferença costuma ser pequena e casaria por acaso.
    numeros <- c(numeros, list(valores_de(diverg, "valor_r"), valores_de(diverg, "valor_gabarito")))
  } else {
    cat("Aviso: saidas/divergencias.csv privado ainda não existe; checo os gabaritos sem os valores calculados.\n")
  }

  # Todos os fatores do IPCA do arquivo privado (e os dois fatores do ano
  # conferido na config), com 7 e 4 casas.
  ipca <- ler_csv(arq_ipca)
  col_fator <- setdiff(names(ipca), "ano")[1]
  fatores <- c(suppressWarnings(as.numeric(ipca[[col_fator]])), cfg$ipca$fator_esperado, cfg$ipca$fator_errado_conhecido)
  fatores <- unique(fatores[!is.na(fatores) & abs(fatores - 1) > 1e-12])
  fatores_txt <- unique(unlist(lapply(fatores, function(f) {
    ponto_virgula(c(sprintf("%.7f", f), sprintf("%.4f", f), format(f, digits = 15, scientific = FALSE, trim = TRUE)))
  })))
  fatores_txt <- fatores_txt[nchar(fatores_txt) >= 5 & significativos(fatores_txt) >= 3]
  numeros <- c(numeros, list(termo_df(fatores_txt, "fator do IPCA", fatores_txt, numero_puro(fatores_txt), "num")))

  # Números do capítulo de orçamento do Relatório Final com 3 ou mais
  # algarismos significativos (sem contar zeros à esquerda nem zeros finais).
  # Anos ficam de fora. Com 1 ou 2 casas decimais e sem milhar, ou inteiros de
  # 3 algarismos, só valem com contexto (%, R$, bi, mi, ações, programas),
  # como aparecem no texto.
  rel <- readLines(arq_relatorio, warn = FALSE, encoding = "UTF-8")
  ini <- grep(CAPITULO_INICIO, rel)[1]
  if (is.na(ini)) sair_sem_checar("não achei o capítulo de orçamento (título numerado 6) em", arq_relatorio)
  fim <- grep(CAPITULO_FIM, rel)
  fim <- c(fim[fim > ini], length(rel) + 1)[1] - 1
  capitulo <- gsub("\\\\", "", sem_espaco_especial(paste(rel[ini:fim], collapse = "\n")))
  toks <- stringi::stri_match_all_regex(capitulo, "(R\\$\\s?)?(?<![0-9.,])([0-9]+(?:[.,][0-9]+)*)(\\s?(?:%|bi\\b|bilh|mi\\b|milh|a[cç][oõ]es|programas?))?")[[1]]
  numeros_capitulo <- list()
  for (k in seq_len(nrow(toks))) {
    tok <- toks[k, 3]
    ctx_antes <- !is.na(toks[k, 2])
    ctx_depois <- if (is.na(toks[k, 4])) "" else trimws(toks[k, 4])
    milhar_br <- grepl("^[0-9]{1,3}(\\.[0-9]{3})+(,[0-9]+)?$", tok)
    if (milhar_br) {
      inteira <- gsub(".", "", sub(",.*$", "", tok), fixed = TRUE)
      dec <- if (grepl(",", tok)) sub("^.*,", "", tok) else ""
    } else if (grepl("^[0-9]+[.,][0-9]+$", tok)) {
      inteira <- sub("[.,].*$", "", tok)
      dec <- sub("^[0-9]+[.,]", "", tok)
    } else if (grepl("^[0-9]+$", tok)) {
      inteira <- tok
      dec <- ""
    } else next
    if (!nzchar(dec) && eh_ano(inteira)) next
    # Inteiro com zero à esquerda é código de programa ou de ação, que já está
    # na lista de códigos.
    if (!nzchar(dec) && grepl("^0[0-9]", inteira)) next
    digitos <- sub("0+$", "", sub("^0+", "", paste0(inteira, dec)))
    if (nchar(digitos) < 3) next
    inteira_milhar <- formatC(as.numeric(inteira), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
    formas <- if (nzchar(dec)) {
      c(paste0(inteira, ",", dec), paste0(inteira, ".", dec), paste0(inteira_milhar, ",", dec))
    } else c(inteira, inteira_milhar)
    formas <- unique(formas)
    com_contexto <- (nzchar(dec) && nchar(dec) <= 2 && !milhar_br) || (!nzchar(dec) && nchar(inteira) <= 3)
    if (!com_contexto) {
      numeros_capitulo[[k]] <- termo_df(formas, "número do capítulo de orçamento", formas, numero_puro(formas), "num")
    } else if (ctx_antes || grepl("^(bi|bilh|mi|milh)", ctx_depois, ignore.case = TRUE)) {
      numeros_capitulo[[k]] <- termo_df(formas, "número do capítulo de orçamento (com R$, bi ou mi)", formas, numero_dinheiro(formas), "num")
    } else if (ctx_depois == "%") {
      numeros_capitulo[[k]] <- termo_df(formas, "número do capítulo de orçamento (com %)", formas, numero_pct(formas), "num")
    } else if (nzchar(ctx_depois)) {
      numeros_capitulo[[k]] <- termo_df(paste(formas, "(contagem)"), "número do capítulo de orçamento (contagem)", formas,
                                        paste0("(?<![0-9a-z.,])", q(formas), "\\s+(?:programas?|acoes|acao)\\b"), "dob")
    } else {
      # Sem contexto no próprio capítulo (célula de tabela, por exemplo): vale
      # com qualquer um dos contextos.
      numeros_capitulo[[k]] <- rbind(
        termo_df(formas, "número do capítulo de orçamento (com R$, bi ou mi)", formas, numero_dinheiro(formas), "num"),
        termo_df(formas, "número do capítulo de orçamento (com %)", formas, numero_pct(formas), "num"))
    }
  }
  numeros_capitulo <- unique(do.call(rbind, numeros_capitulo))
  n_capitulo <- length(unique(numeros_capitulo$fixo))

  # Pessoas: entrevistadas (manifesto e lista de participantes do Relatório) e
  # equipe (bloco EQUIPE do Relatório). O autor e as demais pessoas listadas em
  # config/vazamento.yml (pessoas_permitidas) ficam de fora. Só os nomes são
  # lidos; nada das transcrições.
  manifesto <- jsonlite::fromJSON(arq_manifesto, simplifyVector = TRUE)
  titulos <- if (is.data.frame(manifesto) && "titulo" %in% names(manifesto)) manifesto$titulo else character()
  pessoas <- trimws(sub("^Entrevista\\s+", "", titulos))
  rx_nome <- "\\p{Lu}[\\p{L}'’.]+(?:\\s+(?:(?:d[aeo]s?|e)\\s+)?\\p{Lu}[\\p{L}'’.]+){1,6}"
  primeiro_cap <- grep("^\\s*1\\.\\s*#", rel)[1]
  capa <- sem_espaco_especial(gsub("\\\\", "", rel[seq_len(if (is.na(primeiro_cap)) min(length(rel), 120) else primeiro_cap)]))
  equipe <- stringi::stri_match_first_regex(capa, paste0("^\\s*\\**\\s*(", rx_nome, ")\\s*-\\s*\\p{L}"))[, 2]
  part_ini <- grep("^\\s*[0-9]+\\.\\s*#.*Participantes", rel)[1]
  entrevistados <- character()
  if (!is.na(part_ini)) {
    part_fim <- grep(CAPITULO_FIM, rel)
    part_fim <- c(part_fim[part_fim > part_ini], length(rel) + 1)[1] - 1
    trecho <- sem_espaco_especial(rel[part_ini:part_fim])
    entrevistados <- stringi::stri_match_first_regex(trecho, paste0("^\\s*(", rx_nome, ")\\s*\\("))[, 2]
  }
  todas <- unique(trimws(c(pessoas, equipe, entrevistados)))
  todas <- todas[!is.na(todas) & nzchar(todas)]
  # Também "primeiro e último nome" de nomes compostos (Ana Maria Souza -> Ana Souza).
  partes <- strsplit(todas, "\\s+")
  curtas <- vapply(partes, function(p) if (length(p) >= 3) paste(p[1], p[length(p)]) else NA_character_, "")
  pessoas_txt <- unique(dobrar_texto(c(todas, curtas[!is.na(curtas)])))
  permitidas <- dobrar_texto(lista_de(regras$pessoas_permitidas))
  # Quem está em pessoas_permitidas sai inteiro, inclusive a forma curta.
  permitidas_curtas <- vapply(strsplit(permitidas, " "), function(p) if (length(p) >= 3) paste(p[1], p[length(p)]) else NA_character_, "")
  pessoas_txt <- setdiff(pessoas_txt, c(permitidas, permitidas_curtas))
  pessoas_txt <- pessoas_txt[nchar(pessoas_txt) >= 4]
  n_pessoas <- length(unique(dobrar_texto(todas)))

  termos <- c(termos, numeros,
              list(numeros_capitulo,
                   termo_df(pessoas_txt, "nome de pessoa", pessoas_txt, regex_texto(pessoas_txt), "dob"),
                   termo_df("C:/Users/<nome>", "caminho de pasta de usuário", "users",
                            "(?<![a-z])[a-z]:[\\\\/]+users[\\\\/]+(?![<%$&])[a-z0-9._-]+", "dob"),
                   termo_df("<endereço de e-mail>", "e-mail", "@",
                            "[a-z0-9._%+-]+@[a-z0-9-]+(?:\\.[a-z0-9-]+)*\\.[a-z]{2,}", "dob")))
  termos <- unique(do.call(rbind, termos))
  termos <- termos[nzchar(termos$fixo), ]

  # Pisos de sanidade.
  contagens <- c(
    "ações no recorte" = nrow(recorte),
    "códigos de ação ou programa" = length(codigos),
    "nomes de ação ou programa" = length(nomes),
    "palavras-chave" = length(chaves),
    "valores dos gabaritos" = n_valores,
    "fatores do IPCA" = sum(!is.na(suppressWarnings(as.numeric(ipca[[col_fator]])))),
    "números do capítulo de orçamento" = n_capitulo,
    "nomes de pessoas" = n_pessoas
  )
  cat(sprintf("Pasta privada: %s\n", privado))
  cat(sprintf("Relatório Final: %s\nLista das entrevistas: %s\n", arq_relatorio, arq_manifesto))
  cat("Listas lidas (mínimo exigido entre parênteses):\n")
  # stri_pad conta letras, e não bytes: os acentos não desalinham a coluna.
  cat(sprintf("  %s %6d  (%d)\n", stringi::stri_pad_right(names(contagens), 34), contagens, PISOS[names(contagens)]), sep = "")
  abaixo <- names(contagens)[contagens < PISOS[names(contagens)]]
  if (length(abaixo) > 0) {
    sair_sem_checar("a pasta privada rendeu listas pequenas demais para ser a pasta certa:",
                    paste(abaixo, collapse = "; "), ". Confira --privado ou CONFERENCIA_PRIVADO.")
  }

  # -------------------------------------------------------------------------
  # Arquivos a checar
  # -------------------------------------------------------------------------
  ignoradas <- lista_de(regras$pastas_ignoradas)
  ignorada <- function(rel) any(vapply(ignoradas, function(p) rel == p || startsWith(rel, paste0(p, "/")), logical(1)))
  listar_arquivos <- function() {
    if (modo_pasta) {
      return(list.files(raiz, recursive = TRUE, all.files = TRUE, no.. = TRUE))
    }
    # O que importa é o que pode ser publicado: os arquivos que o git rastreia
    # ou rastrearia (os ignorados pelo .gitignore não vão para o GitHub). Sem
    # git, a checagem olha a pasta inteira, o que é mais rígido.
    git <- Sys.which("git")
    if (nzchar(git) && dir.exists(file.path(raiz, ".git"))) {
      saida <- suppressWarnings(system2(git, c("-C", shQuote(raiz), "-c", "core.quotepath=off", "ls-files",
                                               "--cached", "--others", "--exclude-standard"),
                                        stdout = TRUE, stderr = FALSE))
      if (is.null(attr(saida, "status"))) return(unique(enc2utf8(saida)))
    }
    cat("Aviso: sem git nesta pasta; checo todos os arquivos.\n")
    list.files(raiz, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  }
  todos <- listar_arquivos()
  todos <- todos[!vapply(todos, ignorada, logical(1))]

  # Conteúdo embutido (exceção configurável, ignorar_conteudo_embutido em
  # config/vazamento.yml): um HTML gerado com tudo dentro, como o relatório de
  # exemplo, traz fontes, estilos e bibliotecas em "data:" e em <script> e
  # <style>. Esse código de terceiros casa por acaso com códigos e números
  # curtos. Ele é trocado por nada, mantendo as quebras de linha (os números de
  # linha não mudam). Os dados das tabelas interativas ficam em <script
  # type="application/json"> e continuam checados.
  embutido <- isTRUE(regras$ignorar_conteudo_embutido)
  n_embutidos <- 0L
  apagar_trecho <- function(txt, padrao) {
    m <- gregexpr(padrao, txt, perl = TRUE)[[1]]
    if (m[1] == -1) return(txt)
    n_embutidos <<- n_embutidos + length(m)
    trechos <- regmatches(txt, list(m))[[1]]
    regmatches(txt, list(m)) <- list(gsub("[^\n]", "", trechos))
    txt
  }
  sem_embutido <- function(txt, rel_arq) {
    txt <- apagar_trecho(txt, "data:[a-zA-Z]+/[a-zA-Z0-9.+-]+(?:;[a-zA-Z0-9=.-]+)*,[^\"')\\s>]*")
    if (tolower(tools::file_ext(rel_arq)) %in% c("html", "htm")) {
      txt <- apagar_trecho(txt, "(?is)<script(?![^>]*application/json)[^>]*>.*?</script>")
      txt <- apagar_trecho(txt, "(?is)<style[^>]*>.*?</style>")
    }
    txt
  }
  nao_checados <- data.frame(arquivo = character(), motivo = character())
  notas <- character()
  pedacos <- list()
  for (rel_arq in todos) {
    lido <- tryCatch(ler_texto(file.path(raiz, rel_arq)),
                     error = function(e) list(texto = NA_character_, nota = paste("erro de leitura:", conditionMessage(e))))
    if (!is.na(lido$texto) && embutido) lido$texto <- sem_embutido(lido$texto, rel_arq)
    conteudo <- if (is.na(lido$texto)) character() else strsplit(lido$texto, "\r?\n")[[1]]
    if (is.na(lido$texto)) {
      nao_checados <- rbind(nao_checados, data.frame(arquivo = rel_arq, motivo = lido$nota))
    } else if (nzchar(lido$nota)) {
      notas <- c(notas, sprintf("%s (%s)", rel_arq, lido$nota))
    }
    # Linha 0 é o próprio nome do arquivo: um nome de arquivo também vaza.
    pedacos[[length(pedacos) + 1]] <- data.frame(arquivo = rel_arq, linha = c(0L, seq_along(conteudo)),
                                                 texto = c(rel_arq, conteudo))
  }
  linhas <- do.call(rbind, pedacos)
  if (is.null(linhas)) linhas <- data.frame(arquivo = character(), linha = integer(), texto = character())
  linhas$num <- sem_espaco_especial(linhas$texto)
  # Milhar separado por espaço ("12 345 678") vira número corrido.
  linhas$num_junto <- gsub("(?<=[0-9]) (?=[0-9]{3}(?![0-9]))", "", linhas$num, perl = TRUE)
  # Separador de campo colado no número ("2005,1.2345678" em um CSV, "1;2"):
  # sem isto, a regra que impede casar pedaço de número ("12,345") esconderia
  # o segundo campo. Só a vírgula seguida de número com ponto decimal e o
  # ponto e vírgula viram espaço.
  linhas$num_campos <- gsub("(?<=[0-9]),(?=-?[0-9]+\\.[0-9])", " ", gsub(";", " ", linhas$num, fixed = TRUE), perl = TRUE)
  linhas$dob <- dobrar_texto(linhas$texto)
  # Pares de linhas seguidas, para nomes partidos em duas linhas (sem o
  # marcador de comentário do começo da segunda).
  mesmo_arquivo <- c(linhas$arquivo[-1] == linhas$arquivo[-nrow(linhas)], FALSE)
  seguinte <- c(sub("^\\s*(?:#'|#+|//|\\*|>|--|<!--)\\s*", "", linhas$dob[-1], perl = TRUE), "")
  linhas$par <- ifelse(mesmo_arquivo, paste(linhas$dob, seguinte), NA_character_)

  ext_doc <- tolower(lista_de(regras$extensoes_de_documentacao))
  pastas_doc <- lista_de(regras$pastas_de_documentacao)
  em_pasta_doc <- function(rel) vapply(rel, function(r) any(startsWith(r, paste0(pastas_doc, "/"))), logical(1), USE.NAMES = FALSE)
  linhas$documentacao <- tolower(tools::file_ext(linhas$arquivo)) %in% ext_doc |
    (length(pastas_doc) > 0 & em_pasta_doc(linhas$arquivo))

  permitidos <- dobrar_texto(lista_de(regras$termos_permitidos_em_documentacao))
  permitidos_sempre <- dobrar_texto(lista_de(regras$termos_permitidos_em_qualquer_arquivo))

  cat("Exceções em uso (config/vazamento.yml da pasta checada):\n")
  cat(sprintf("  pastas e arquivos ignorados: %s\n", paste(ignoradas, collapse = ", ")))
  cat(sprintf("  termos permitidos em qualquer arquivo: %s\n", paste(permitidos_sempre, collapse = ", ")))
  cat(sprintf("  termos permitidos só em documentação (%s): %s\n", paste(c(ext_doc, paste0(pastas_doc, "/")), collapse = ", "),
              if (length(permitidos) > 0) paste(permitidos, collapse = ", ") else "nenhum"))
  cat(sprintf("  pessoas permitidas: %s\n", if (length(permitidas) > 0) paste(lista_de(regras$pessoas_permitidas), collapse = ", ") else "nenhuma"))
  cat(sprintf("  conteúdo embutido (data:, e <script> e <style> em HTML, menos os dados em JSON): %s\n",
              if (embutido) sprintf("ignorado (%d trechos)", n_embutidos) else "checado"))

  # -------------------------------------------------------------------------
  # Busca: primeiro um filtro rápido por texto exato, depois a expressão
  # regular só nas linhas candidatas.
  # -------------------------------------------------------------------------
  textuais <- c("nome de ação ou programa", "palavra-chave do recorte", "nome de pessoa")
  achados <- list()
  buscar <- function(onde, k) {
    cand <- which(stringi::stri_detect_fixed(onde, termos$fixo[k]))
    if (length(cand) == 0) return(integer())
    cand[stringi::stri_detect_regex(onde[cand], termos$regex[k])]
  }
  for (k in seq_len(nrow(termos))) {
    tipo <- termos$tipo[k]
    if (tipo %in% textuais && termos$fixo[k] %in% permitidos_sempre) next
    if (termos$campo[k] == "num") {
      cand <- union(union(buscar(linhas$num, k), buscar(linhas$num_junto, k)), buscar(linhas$num_campos, k))
    } else {
      cand <- buscar(linhas$dob, k)
      if (tipo %in% textuais) {
        # Nome partido entre duas linhas: conta na primeira, se não apareceu
        # inteiro em nenhuma das duas.
        no_par <- setdiff(buscar(linhas$par, k), c(cand, cand - 1L))
        cand <- sort(union(cand, no_par))
      }
    }
    if (tipo %in% textuais && termos$fixo[k] %in% permitidos) cand <- cand[!linhas$documentacao[cand]]
    if (length(cand) == 0) next
    achados[[length(achados) + 1]] <- data.frame(arquivo = linhas$arquivo[cand], linha = linhas$linha[cand],
                                                 termo = termos$termo[k], tipo = tipo)
  }

  n_tipos <- table(termos$tipo)
  cat(sprintf("Checagem anti-vazamento: %d arquivos, %d linhas, %d termos proibidos.\n",
              length(todos), nrow(linhas), nrow(termos)))
  cat(sprintf("  %s %6d\n", stringi::stri_pad_right(names(n_tipos), 55), as.integer(n_tipos)), sep = "")
  cat(sprintf("Pasta checada: %s%s\n", raiz, if (modo_pasta) " (todos os arquivos)" else " (arquivos que o git publicaria)"))
  if (length(notas) > 0) cat("Lidos com outra codificação:\n", paste0("  ", notas, "\n"), sep = "")

  mostrar_nao_checados <- function() {
    cat(sprintf("\nNÃO CHECADO: %d arquivo(s) não puderam ser lidos (arquivo que não é texto, ou listado pelo git e ausente da pasta):\n", nrow(nao_checados)))
    cat(sprintf("  %s: %s\n", nao_checados$arquivo, nao_checados$motivo), sep = "")
    cat("Se for um arquivo de terceiros sem texto nosso, ponha a pasta em pastas_ignoradas de config/vazamento.yml, com o motivo comentado.\n")
  }
  if (length(achados) > 0) {
    achados <- unique(do.call(rbind, achados))
    achados <- achados[order(achados$arquivo, achados$linha), ]
    cat(sprintf("\nVAZAMENTO: %d ocorrência(s). Corrija antes de publicar ou de fazer commit:\n", nrow(achados)))
    cat(sprintf("  %s:%d: %s [%s]\n", achados$arquivo, achados$linha, achados$termo, achados$tipo), sep = "")
    cat("(linha 0 = o nome do arquivo)\n")
    if (nrow(nao_checados) > 0) mostrar_nao_checados()
    quit(save = "no", status = 1)
  }
  if (nrow(nao_checados) > 0) {
    mostrar_nao_checados()
    cat("A checagem não encontrou vazamento no resto, mas não terminou. Isto NÃO significa que o repositório está limpo.\n")
    quit(save = "no", status = 2)
  }
  cat("OK: nenhum termo da conferência privada encontrado.\n")
  quit(save = "no", status = 0)
}

# Qualquer erro do R vira status 2 (não checado), nunca 1: o status 1 é só
# para "encontrou".
tryCatch(principal(), error = function(e) {
  sair_sem_checar("erro na própria checagem:", conditionMessage(e))
})
