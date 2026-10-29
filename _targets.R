# Pipeline da conferência ({targets}). Cada alvo é um passo com nome; o
# {targets} só refaz o que mudou desde a última execução (um arquivo de
# entrada, uma função em R/ ou a configuração).
#
# Rodar tudo: Rscript rodar.R (na raiz do projeto). Ver um resultado:
# targets::tar_read(totais_ano). O que é específico do projeto (caminhos,
# ações obrigatórias, causas conhecidas, gabaritos) fica em config/; este
# arquivo é o mesmo na conferência privada e na vitrine pública.

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "cli", "DBI", "dplyr", "duckdb", "ggplot2", "gt", "here", "knitr",
    "purrr", "readr", "scales", "stringr", "tibble", "tidyr", "yaml"
  ),
  format = "rds"
)

tar_source("R")

# Os arquivos de R/, listados toda vez que este arquivo é lido. O {targets}
# guarda o conteúdo desta variável junto com o alvo arq_funcoes: se um arquivo
# for criado ou apagado em R/, a lista muda e o alvo é refeito (e o relatório
# com ele). Caminhos relativos, porque o pipeline sempre roda na raiz do
# projeto: um caminho absoluto continuaria apontando para a pasta antiga se o
# projeto fosse movido.
ARQUIVOS_R <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))

# O tar_quarto() abaixo inspeciona o relatorio.qmd quando este arquivo é lido,
# e para isso precisa achar o Quarto. O rodar.R já define QUARTO_PATH; isto
# cobre quem chama targets::tar_make() direto (config.yml, PATH ou {quarto}).
if (!nzchar(Sys.getenv("QUARTO_PATH"))) {
  quarto_cfg <- yaml::read_yaml(here::here("config/config.yml"))$ferramentas$quarto
  quarto_achado <- localizar_quarto(if (is.null(quarto_cfg)) "" else quarto_cfg)
  if (nzchar(quarto_achado)) Sys.setenv(QUARTO_PATH = quarto_achado)
}

# O contrato dá a cinco alvos o mesmo nome da função que os calcula
# (totais_periodo, taxa_execucao, contagens, ranking_anos e veredito). Um alvo
# cujo comando chama a função de mesmo nome pareceria, para o {targets},
# depender de si mesmo. Os comandos usam estes apelidos (a função é a mesma)
# e os nomes originais saem do ambiente global, onde só os alvos devem ter
# esses nomes. Fora do pipeline (testes, uso direto) as funções seguem com o
# nome do contrato.
calcular_totais_periodo <- totais_periodo
calcular_taxa_execucao <- taxa_execucao
calcular_contagens <- contagens
calcular_ranking_anos <- ranking_anos
calcular_veredito <- veredito
rm(totais_periodo, taxa_execucao, contagens, ranking_anos, veredito)

list(
  # ---- configuração e arquivos de entrada (format = "file": se o arquivo
  # mudar, tudo o que depende dele é refeito)
  # Caminho relativo à raiz, e a configuração lida sem transformar os
  # caminhos relativos em absolutos (resolver = FALSE): assim nenhum resultado
  # guardado aponta para a pasta antiga se o projeto for movido.
  tar_target(arq_config, "config/config.yml", format = "file"),
  tar_target(cfg, carregar_config(arq_config, resolver = FALSE)),
  # As funções de R/ também como arquivos: o relatório usa as de formatação
  # (R/utils.R) e precisa ser refeito quando elas mudam, mesmo que nenhum
  # número mude.
  tar_target(arq_funcoes, ARQUIVOS_R, format = "file"),
  # Sem caminho configurado, nenhuma causa é conhecida (mais rígido).
  tar_target(arq_causas, {
    caminho <- cfg$caminhos$causas_conhecidas
    if (is.null(caminho) || !nzchar(caminho)) character(0) else caminho
  }, format = "file"),
  tar_target(causas, ler_causas_conhecidas(arq_causas)),
  # Ações que o recorte tem de conter (recorte: acoes_obrigatorias em config.yml).
  tar_target(acoes_obrigatorias, as.character(unlist(cfg$recorte$acoes_obrigatorias))),
  tar_target(arq_juventude_nova, cfg$caminhos$juventude_nova, format = "file"),
  tar_target(arq_juventude_antiga, cfg$caminhos$juventude_antiga, format = "file"),
  tar_target(arq_uniao_consolidada, cfg$caminhos$uniao_consolidada, format = "file"),
  tar_target(arq_uniao_nova, cfg$caminhos$uniao_nova, format = "file"),
  tar_target(arq_ipca, cfg$caminhos$ipca, format = "file"),
  tar_target(arq_recorte, cfg$caminhos$recorte_acoes, format = "file"),
  tar_target(arq_gabaritos, cfg$caminhos$gabaritos, format = "file"),
  # Cache do denominador da implementação de referência (opcional). Como
  # arquivo: se ele mudar, a checagem cruzada é refeita. Sem caminho, ou com
  # o arquivo ausente, fica vazio e a checagem é pulada com aviso.
  tar_target(arq_executivo_python, {
    caminho <- cfg$caminhos$executivo_python
    if (is.null(caminho) || !nzchar(caminho) || !file.exists(caminho)) character(0) else caminho
  }, format = "file"),

  # ---- procedência: de onde veio cada número (tamanho, data e hash). Inclui
  # a configuração e as causas conhecidas, que também mudam o resultado.
  tar_target(
    procedencia,
    registrar_procedencia(c(
      juventude_nova = arq_juventude_nova,
      juventude_antiga = arq_juventude_antiga,
      uniao_consolidada = arq_uniao_consolidada,
      uniao_nova = arq_uniao_nova,
      ipca = arq_ipca,
      recorte_acoes = arq_recorte,
      gabaritos = arq_gabaritos,
      causas_conhecidas = arq_causas,
      executivo_python = arq_executivo_python,
      config = arq_config
    ))
  ),

  # ---- leitura
  tar_target(juventude_nova, ler_juventude_nova(arq_juventude_nova)),
  tar_target(juventude_antiga, ler_juventude_antiga(arq_juventude_antiga)),
  tar_target(uniao_consolidada, ler_uniao_consolidada(arq_uniao_consolidada, cfg)),
  tar_target(uniao_nova, ler_uniao_nova(arq_uniao_nova)),
  tar_target(ipca, ler_ipca(arq_ipca)),
  tar_target(recorte, ler_recorte(arq_recorte)),

  # ---- integração das safras
  tar_target(comparacao_safras_juventude, comparar_safras_juventude(juventude_nova, juventude_antiga, cfg)),
  tar_target(diferencas_juventude, detalhar_diferencas_juventude(juventude_nova, juventude_antiga, cfg)),
  tar_target(comparacao_safras_uniao, comparar_safras_uniao(uniao_consolidada$por_orgao_gnd, uniao_nova, cfg)),
  tar_target(base_juventude, montar_base_juventude(juventude_nova, cfg)),
  tar_target(denominador, montar_denominador(uniao_consolidada$por_orgao_gnd, uniao_nova, cfg)),

  # ---- recorte e deflação
  tar_target(base_recortada, aplicar_recorte(base_juventude, recorte)),
  tar_target(
    resumo_recorte,
    resumir_recorte(base_juventude, base_recortada, recorte, acoes_obrigatorias = acoes_obrigatorias)
  ),
  tar_target(recorte_na_uniao, testar_recorte_na_uniao(uniao_consolidada$por_programa_acao, base_recortada)),
  tar_target(base_real, deflacionar(base_recortada, ipca, cfg)),

  # ---- agregações
  tar_target(totais_ano, totais_por_ano(base_real)),
  tar_target(totais_periodo, calcular_totais_periodo(totais_ano)),
  tar_target(totais_gnd, totais_por_gnd(base_real)),
  tar_target(totais_acao_ano, totais_por_acao_ano(base_real)),
  tar_target(taxa_execucao, calcular_taxa_execucao(totais_ano)),
  tar_target(participacao, participacao_executivo(totais_ano, denominador, fase = cfg$denominador$fase)),
  tar_target(concentracao, concentracao_programas(base_real, cfg)),
  tar_target(contagens, calcular_contagens(base_real)),
  tar_target(ranking_anos, calcular_ranking_anos(totais_ano, fase = "dotacao_atual", n = 4)),

  # ---- conferência
  tar_target(gabaritos, ler_gabaritos(arq_gabaritos)),
  tar_target(divergencias, {
    # base_real, ipca, diferencas_juventude e o cache do Python vão junto
    # para as checagens estruturais (soma das linhas, fator do IPCA, ações
    # das safras e denominador); o resto são os resultados da seção 6.7
    resultados <- list(
      totais_ano = totais_ano, totais_periodo = totais_periodo, totais_gnd = totais_gnd,
      totais_acao_ano = totais_acao_ano, taxa_execucao = taxa_execucao,
      participacao = participacao, concentracao = concentracao, contagens = contagens,
      denominador = denominador, comparacao_safras_juventude = comparacao_safras_juventude,
      comparacao_safras_uniao = comparacao_safras_uniao, resumo_recorte = resumo_recorte,
      base_real = base_real, ipca = ipca, diferencas_juventude = diferencas_juventude,
      executivo_python = arq_executivo_python
    )
    dplyr::bind_rows(
      conferir_gabaritos(gabaritos, resultados, cfg, causas = causas),
      checagens_estruturais(resultados, cfg, acoes_obrigatorias = acoes_obrigatorias, causas = causas)
    )
  }),
  tar_target(veredito, calcular_veredito(divergencias)),
  tar_target(arquivos_saida, gravar_saidas(divergencias, procedencia, cfg), format = "file"),

  # ---- relatório em HTML (Quarto)
  # quiet = TRUE: as mensagens do Quarto e do pandoc (em inglês) não vão para
  # o terminal; um erro de renderização continua aparecendo.
  tar_quarto(relatorio, "relatorio/relatorio.qmd", quiet = TRUE)
)
