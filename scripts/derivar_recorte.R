# Gera a lista de ações que formam o recorte de juventude (caminhos:
# recorte_acoes em config.yml), tirada da extração nova do SIOP
# (caminhos: juventude_nova).
#
# Por que isto é um script e não parte do pipeline: o recorte é uma ENTRADA da
# conferência (decisão D3). O pipeline confere os números a partir dele; se o
# recorte fosse recalculado a cada execução, uma mudança na extração mudaria o
# recorte sem ninguém perceber. Rodar de novo só quando houver extração nova,
# e revisar a diferença do arquivo antes de aceitar.
#
# Uso, a partir da raiz do projeto:
#   Rscript scripts/derivar_recorte.R                 (usa config/config.yml)
#   Rscript scripts/derivar_recorte.R outra/config.yml

args <- commandArgs(trailingOnly = TRUE)
caminho_config <- if (length(args) >= 1) args[[1]] else here::here("config/config.yml")

for (arquivo in c("utils.R", "ler_juventude.R", "integrar.R")) {
  source(here::here("R", arquivo), encoding = "UTF-8")
}

cli::cli_h1("Derivar o recorte de ações de juventude")
cfg <- carregar_config(caminho_config)

cli::cli_alert_info("Lendo {.path {basename(cfg$caminhos$juventude_nova)}}")
nova <- ler_juventude_nova(cfg$caminhos$juventude_nova)
recorte <- derivar_recorte(nova)

# As ações obrigatórias (recorte: acoes_obrigatorias em config.yml) entram no
# recorte por inclusão manual. Se alguma sumir ou mudar de origem, a extração
# mudou de critério: melhor parar aqui.
obrigatorias <- as.character(unlist(cfg$recorte$acoes_obrigatorias))
for (acao in obrigatorias) {
  linha <- dplyr::filter(recorte, .data$acao_cod == acao)
  if (nrow(linha) != 1 || linha$origem != "inclusão manual") {
    cli::cli_abort(c(
      "A ação obrigatória {.val {acao}} não está no recorte como inclusão manual.",
      "i" = "Confira se {.code caminhos: juventude_nova} em config.yml aponta para a extração certa."
    ))
  }
}

destino <- cfg$caminhos$recorte_acoes
dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(recorte, destino, na = "")

cli::cli_alert_success("Gravado {.path {destino}}")
cli::cli_bullets(c(
  "*" = "{nrow(recorte)} ações; {sum(recorte$origem == 'palavra-chave')} por palavra-chave e {sum(recorte$origem == 'inclusão manual')} por inclusão manual.",
  "*" = "{sum(recorte$n_linhas)} linhas da base cobertas (a base tem {nrow(nova)})."
))
