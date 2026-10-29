# Carrega as funções de R/ e a configuração para os testes.
#
# O testthat roda os arquivos helper-*.R antes dos testes, no mesmo ambiente
# dos testes. Por isso o source() fica no topo do arquivo (e não dentro de uma
# função): assim as funções ficam visíveis para todos os test_that().

# A raiz é achada pelo arquivo .here, que fica na raiz do projeto. Funciona
# tanto com o test_dir() chamado da raiz quanto de dentro de tests/testthat.
#
# Nenhum valor do projeto fica escrito nos testes: os unitários usam dados
# inventados (códigos começando com "Z"), e os de integração leem o que
# esperar da configuração e dos gabaritos. Assim os mesmos testes rodam na
# conferência privada e na vitrine com dados sintéticos.
RAIZ <- here::here()
STORE <- file.path(RAIZ, "_targets")

# Ordem alfabética, a mesma do tar_source() do _targets.R, para que os testes
# vejam as funções exatamente como o pipeline as vê.
for (arquivo_r in sort(list.files(file.path(RAIZ, "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(arquivo_r, local = environment(), encoding = "UTF-8")
}
rm(arquivo_r)

CFG <- carregar_config(file.path(RAIZ, "config/config.yml"))

# Ações obrigatórias e causas conhecidas, da mesma configuração que o pipeline usa.
ACOES_OBRIGATORIAS <- as.character(unlist(CFG$recorte$acoes_obrigatorias))
CAUSAS <- suppressMessages(ler_causas_conhecidas(CFG$caminhos$causas_conhecidas))

# Fatores do IPCA da configuração: o certo e o errado conhecido do ano conferido.
FATOR_CERTO <- as.numeric(CFG$ipca$fator_esperado)
FATOR_ERRADO <- as.numeric(CFG$ipca$fator_errado_conhecido)
ANO_IPCA <- as.integer(CFG$ipca$ano_conferido)

# Cópia da configuração com um período curto. Os testes unitários usam dois ou
# três anos de dados; com o período real (2005 a 2025) as funções cobrariam
# fator do IPCA e denominador para 21 anos que o teste não monta.
cfg_mini <- function(ano_inicio, ano_fim, ultimo = ano_fim, ano_conferido = CFG$ipca$ano_conferido) {
  cfg <- CFG
  cfg$periodo$ano_inicio <- as.integer(ano_inicio)
  cfg$periodo$ano_fim <- as.integer(ano_fim)
  cfg$periodo$ultimo_ano_safra_antiga <- as.integer(ultimo)
  cfg$ipca$ano_conferido <- as.integer(ano_conferido)
  cfg
}

# Lê um alvo do pipeline. Sem a pasta _targets/ o pipeline ainda não rodou e o
# teste de integração é pulado. Com a pasta presente e o alvo ausente o teste
# falha de propósito: isso é pipeline incompleto, não ausência de pipeline.
ler_alvo <- function(nome) {
  testthat::skip_if_not(dir.exists(STORE), "Pipeline ainda não rodou (sem _targets/): teste de integração pulado.")
  targets::tar_read_raw(nome, store = STORE)
}

# Grava texto em UTF-8 sem conversão. No Windows, writeLines() pode gravar o
# cabeçalho acentuado em outra codificação, e o teste falharia pela
# codificação e não pela lógica da leitura.
gravar_utf8 <- function(linhas, caminho, bom = FALSE) {
  texto <- paste0(paste(enc2utf8(linhas), collapse = "\n"), "\n")
  bytes <- charToRaw(texto)
  if (bom) bytes <- c(as.raw(c(0xEF, 0xBB, 0xBF)), bytes)
  writeBin(bytes, caminho)
  invisible(caminho)
}

# Linha com o mesmo valor em todas as fases: deixa os dados de teste curtos.
fases_iguais <- function(valor) {
  tibble::as_tibble(stats::setNames(as.list(rep(as.double(valor), length(FASES))), FASES))
}
