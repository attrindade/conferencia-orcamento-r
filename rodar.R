# Roda a conferência inteira com um comando, a partir da raiz do projeto:
#
#   Rscript rodar.R            refaz só o que mudou desde a última execução
#   Rscript rodar.R --do-zero  apaga os resultados guardados (_targets/) e refaz tudo
#
# Quatro etapas: (1) confere o ambiente e os arquivos de entrada; (2) roda o
# pipeline do {targets}; (3) roda os testes; (4) mostra o veredito e onde está
# o relatório. Sai com status 1 se algum teste falhar ou se houver divergência
# sem explicação, para que outro programa (ou a pessoa) perceba o problema.

# ---- Pasta do projeto e renv ------------------------------------------------
# Quando o Rscript é chamado de outra pasta, o .Rprofile do projeto não roda e
# o renv não é ativado; aqui o script vai para a própria pasta e ativa o renv,
# para usar sempre as mesmas versões de pacote.
arquivo_script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
if (length(arquivo_script) == 1) {
  raiz <- dirname(normalizePath(arquivo_script, winslash = "/"))
  if (!identical(normalizePath(getwd(), winslash = "/"), raiz)) {
    setwd(raiz)
    if (file.exists("renv/activate.R")) source("renv/activate.R")
  }
}

# Sem o {cli} nenhuma mensagem abaixo funciona (todas usam o pacote). Em uma
# cópia nova do projeto, antes do renv::restore(), o R pararia com um erro em
# inglês ("there is no package called cli"); aqui sai uma instrução clara.
if (!requireNamespace("cli", quietly = TRUE)) {
  cat(
    "\nFaltam os pacotes de R deste projeto (o primeiro que faltou foi o cli).\n",
    "Rode antes, uma vez, de dentro desta pasta:\n\n",
    "  Rscript -e \"renv::restore()\"\n\n",
    "Ele instala as versões registradas em renv.lock (precisa de internet na primeira vez).\n",
    "Depois rode de novo: Rscript rodar.R\n\n",
    sep = ""
  )
  quit(save = "no", status = 1)
}

do_zero <- "--do-zero" %in% commandArgs(trailingOnly = TRUE)
inicio_geral <- Sys.time()

source("R/utils.R", encoding = "UTF-8")

# Duração em português: "48,2 s" ou "1 min 35 s".
duracao_texto <- function(segundos) {
  if (segundos < 60) return(paste(formatC(segundos, format = "f", digits = 1, decimal.mark = ","), "s"))
  inteiro <- round(segundos)
  paste(inteiro %/% 60, "min", inteiro %% 60, "s")
}

# Encerra com status 1 e uma dica do que fazer, em vez de um erro cru do R.
# As mensagens são modelos do {cli}: são interpretadas no ambiente de quem
# chamou, para que "{.path {caminho}}" encontre a variável de lá.
falhar <- function(mensagem, dica, envir = parent.frame()) {
  cli::cli_alert_danger(mensagem, .envir = envir)
  cli::cli_alert_info(dica, .envir = envir)
  quit(save = "no", status = 1)
}

# Cabeçalho "Etapa n de 4", execução e tempo gasto.
rodar_etapa <- function(n, titulo, codigo) {
  cli::cli_h2("Etapa {n} de 4: {titulo}")
  inicio <- Sys.time()
  resultado <- codigo()
  segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  cli::cli_alert_info("Etapa {n} levou {duracao_texto(segundos)}.")
  invisible(resultado)
}

cli::cli_h1("Conferência do orçamento em R")
if (do_zero) cli::cli_alert_warning("Opção {.code --do-zero}: os resultados guardados serão apagados e tudo será refeito.")

# ---- Etapa 1: ambiente ---------------------------------------------------------
PACOTES <- c(
  "targets", "tarchetypes", "duckdb", "DBI", "dplyr", "tidyr", "readr", "purrr",
  "stringr", "cli", "yaml", "here", "testthat", "quarto", "gt", "ggplot2",
  "scales", "knitr", "rmarkdown"
)
# O cache do Python é opcional (fica vazio na vitrine pública); a pasta de
# saídas é criada se não existir. Os demais caminhos são obrigatórios.
OPCIONAIS <- c("executivo_python")

conferir_ambiente <- function() {
  versao_r <- getRversion()
  if (versao_r < "4.6.0") {
    cli::cli_alert_warning("R {versao_r}: o projeto foi montado com R 4.6.1; versões anteriores podem dar resultados diferentes.")
  } else {
    cli::cli_alert_success("R {versao_r}")
  }

  cfg <- tryCatch(carregar_config(), error = function(e) {
    falhar("{conditionMessage(e)}", "Confira se {.path config/config.yml} existe e se o comando foi rodado de dentro da pasta do projeto.")
  })
  cli::cli_alert_info("Projeto: {cfg$projeto$nome}")

  # Quarto: o caminho de config.yml; se não existir, o do PATH; senão, o que o
  # pacote {quarto} encontrar.
  quarto_cfg <- if (is.null(cfg$ferramentas$quarto)) "" else cfg$ferramentas$quarto
  quarto <- localizar_quarto(quarto_cfg)
  if (!nzchar(quarto)) {
    falhar(
      "Não encontrei o Quarto (nem em {.path {quarto_cfg}}, nem no PATH, nem pelo pacote {.pkg quarto}).",
      "Instale o Quarto (https://quarto.org) ou corrija {.code ferramentas: quarto:} em {.path config/config.yml}."
    )
  }
  if (nzchar(quarto_cfg) && !identical(quarto, quarto_cfg)) {
    cli::cli_alert_warning("O Quarto de {.path config/config.yml} não existe nesta máquina; usando {.path {quarto}}.")
  }
  versao_quarto <- tryCatch(system2(quarto, "--version", stdout = TRUE, stderr = TRUE)[1], error = function(e) "?")
  cli::cli_alert_success("Quarto {versao_quarto}")
  # O relatório é renderizado pelo Quarto, que precisa saber qual R usar.
  Sys.setenv(QUARTO_PATH = quarto, QUARTO_R = R.home("bin"))

  faltam_pacotes <- PACOTES[!vapply(PACOTES, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltam_pacotes) > 0) {
    falhar(
      "Faltam pacotes: {.pkg {faltam_pacotes}}.",
      "Rode {.code renv::restore()} na pasta do projeto e tente de novo."
    )
  }
  cli::cli_alert_success("{length(PACOTES)} pacotes disponíveis")

  dir.create(cfg$caminhos$saidas, showWarnings = FALSE, recursive = TRUE)
  arquivos <- cfg$caminhos[setdiff(names(cfg$caminhos), "saidas")]
  for (papel in names(arquivos)) {
    caminho <- arquivos[[papel]]
    if (!nzchar(caminho) && papel %in% OPCIONAIS) {
      cli::cli_alert_warning("{papel}: não configurado (opcional); a checagem que depende dele será pulada.")
    } else if (!file.exists(caminho) && papel %in% OPCIONAIS) {
      cli::cli_alert_warning("{papel}: {.path {caminho}} não existe (opcional); a checagem que depende dele será pulada.")
    } else if (file.exists(caminho)) {
      cli::cli_alert_success("{papel}: {.path {basename(caminho)}}")
    } else if (papel == "recorte_acoes") {
      falhar(
        "Falta a lista de ações do recorte: {.path {caminho}}.",
        "Gere com {.code Rscript scripts/derivar_recorte.R} e revise o arquivo antes de rodar de novo."
      )
    } else {
      falhar(
        "Arquivo de entrada não encontrado ({papel}): {.path {caminho}}.",
        "Confira {.code caminhos:} em {.path config/config.yml}."
      )
    }
  }
  cfg
}

cfg <- rodar_etapa(1, "conferir o ambiente e os arquivos de entrada", conferir_ambiente)

# ---- Etapa 2: pipeline ---------------------------------------------------------
rodar_pipeline <- function() {
  if (do_zero && dir.exists("_targets")) {
    unlink("_targets", recursive = TRUE)
    cli::cli_alert_info("Resultados guardados apagados.")
  }
  cli::cli_alert_info("Calculando as etapas que mudaram. Do zero, leva cerca de um minuto e meio; sem mudanças, alguns segundos.")
  # reporter = "silent": o {targets} não imprime uma linha (em inglês) por
  # etapa; o resumo em português vem logo abaixo.
  tryCatch(
    targets::tar_make(reporter = "silent"),
    error = function(e) {
      # Só os erros desta rodada: os metadados guardam também erros antigos,
      # de etapas que não rodaram agora, e mostrá-los levaria a pessoa a
      # corrigir o problema errado. A mensagem de tar_make() não é mostrada
      # porque termina com um rodapé em inglês; a de cada etapa basta.
      falhou <- tryCatch({
        prog <- targets::tar_progress()
        prog$name[prog$progress == "errored"]
      }, error = function(e2) character())
      erros <- tryCatch(
        targets::tar_meta(names = dplyr::any_of(falhou), fields = "error"),
        error = function(e2) NULL
      )
      if (!is.null(erros) && nrow(erros) > 0) {
        for (i in seq_len(nrow(erros))) {
          cli::cli_alert_danger("Etapa {.field {erros$name[i]}}:")
          cli::cli_verbatim(erros$error[i])
        }
      }
      falhar(
        "O cálculo parou com erro em {length(falhou)} etapa{?s}{if (length(falhou) > 0) paste0(': ', paste(falhou, collapse = ', ')) else ''}.",
        "Leia a mensagem acima, corrija e rode {.code Rscript rodar.R} de novo (só o que falta será refeito)."
      )
    }
  )
  progresso <- targets::tar_progress()
  refeitos <- progresso$name[progresso$progress %in% c("completed", "built")]
  em_dia <- progresso$name[progresso$progress == "skipped"]
  cli::cli_alert_success("{length(refeitos)} etapa{?s} recalculada{?s}; {length(em_dia)} já estava{?m} em dia.")
  # A lista vai montada à mão: o {cli} encurtaria uma lista longa com "…" e
  # terminaria com "and", em inglês.
  if (length(refeitos) > 0) cli::cli_bullets(c("*" = "Recalculadas: {paste(refeitos, collapse = ', ')}."))
  invisible(progresso)
}

rodar_etapa(2, "rodar o pipeline (targets)", rodar_pipeline)

# ---- Etapa 3: testes -----------------------------------------------------------
rodar_testes <- function() {
  arquivos_teste <- list.files("tests/testthat", pattern = "^test.*\\.R$")
  if (length(arquivos_teste) == 0) {
    cli::cli_alert_danger("Nenhum arquivo de teste em {.path tests/testthat}.")
    return(list(passaram = 0, falharam = 1, pulados = 0))
  }
  # reporter = "silent": a contagem e as falhas saem abaixo, em português.
  resultado <- testthat::test_dir("tests/testthat", reporter = "silent", stop_on_failure = FALSE, stop_on_warning = FALSE)
  tabela <- as.data.frame(resultado)
  # Cada linha é um bloco test_that(); conta como falha se teve expectativa
  # falhada ou erro.
  falharam <- sum(tabela$failed > 0 | tabela$error)
  pulados <- sum(tabela$skipped & !(tabela$failed > 0 | tabela$error))
  passaram <- nrow(tabela) - falharam - pulados
  if (falharam > 0) {
    cli::cli_alert_danger("{falharam} bloco{?s} de teste com falha; {passaram} passaram; {pulados} pulado{?s}.")
    # Para cada bloco com falha: arquivo, nome do bloco e a primeira mensagem.
    for (bloco in resultado) {
      problemas <- Filter(function(r) inherits(r, c("expectation_failure", "expectation_error")), bloco$results)
      if (length(problemas) == 0) next
      cli::cli_alert_danger("{.file {bloco$file}}: {bloco$test}")
      linhas <- strsplit(conditionMessage(problemas[[1]]), "\n", fixed = TRUE)[[1]]
      cli::cli_verbatim(paste0("    ", utils::head(linhas, 4)))
    }
  } else {
    cli::cli_alert_success("{passaram} bloco{?s} de teste passaram; {pulados} pulado{?s}; nenhum falhou.")
  }
  list(passaram = passaram, falharam = falharam, pulados = pulados)
}

testes <- rodar_etapa(3, "rodar os testes", rodar_testes)

# ---- Etapa 4: veredito ---------------------------------------------------------
mostrar_veredito <- function() {
  store <- here::here("_targets")
  veredito <- targets::tar_read(veredito, store = store)
  cli::cli_alert_info(veredito$texto)

  arquivos_relatorio <- tryCatch(targets::tar_read(relatorio, store = store), error = function(e) character())
  html <- grep("\\.html$", arquivos_relatorio, value = TRUE)
  if (length(html) == 0) html <- here::here("relatorio/relatorio.html")
  if (file.exists(html[1])) {
    cli::cli_alert_success("Relatório: {.path {normalizePath(html[1], winslash = '/')}}")
  } else {
    cli::cli_alert_warning("Relatório não encontrado em {.path {html[1]}}.")
  }
  veredito
}

veredito <- rodar_etapa(4, "veredito", mostrar_veredito)

total_txt <- duracao_texto(as.numeric(difftime(Sys.time(), inicio_geral, units = "secs")))
problemas <- character()
if (testes$falharam > 0) problemas <- c(problemas, "teste falhou")
if (!is.null(veredito$divergencias) && veredito$divergencias > 0) problemas <- c(problemas, "há divergência sem explicação")
if (length(problemas) > 0) {
  falhar(
    "Conferência terminou com problema em {total_txt}: {paste(problemas, collapse = '; ')}.",
    "Abra {.path saidas/divergencias.csv} (status \"diverge\") e a saída dos testes acima."
  )
}
cli::cli_alert_success("Conferência concluída em {total_txt}, sem divergência sem explicação.")
