# Leitura das bases do SIOP e do IPCA. Os unitários gravam arquivos de poucas
# linhas, inventados, no formato de cada extração (separadores, milhar,
# decimal, BOM); os de integração conferem o que o pipeline leu dos arquivos
# configurados em config.yml.

# ---- arquivos minúsculos no formato de cada safra --------------------------

csv_juventude_nova <- function() {
  arq <- tempfile(fileext = ".csv")
  gravar_utf8(c(
    "ano;orgao_orcamentario;unidade_orcamentaria;programa;acao;grupo_despesa;modalidade_aplicacao;funcao;eixo_tematico;flag_palavra_chave;flag_programa_relacionado;palavras_chave_encontradas;projeto_lei;dotacao_inicial;dotacao_atual;empenhado;liquidado;pago",
    "2023;Z2600 - Ministério Fictício do Ensino;Z2601 - Unidade Fictícia;Z199 - Programa Fictício Alicerce;ZM01 - Incentivo Fictício - Permanência;5 - Inversões Financeiras;90 - Aplicações Diretas;12 - Educação;Eixo Fictício;False;False;;0;6000000000,5;6100000000;6000000000;6000000000;6000000000",
    "2005;Z3600 - Ministério Fictício do Cuidado;Z3601 - Unidade Fictícia;Z102 - Programa Fictício Travessia;0Z01 - Proteção Social Fictícia;3 - Outras Despesas Correntes;30 - Estados;08 - Assistência Social;;True;True;chave-alfa,chave-beta;100;200,25;300;250;240;-"
  ), arq, bom = TRUE)
  arq
}

csv_juventude_antiga <- function() {
  arq <- tempfile(fileext = ".csv")
  gravar_utf8(c(
    "Ano;Órgão Orçamentário;Unidade Orçamentária;Programa;Ação;Grupo de Despesa;Modalidade de Aplicação;Função;Eixo Temático;flag_palavra_chave;flag_programa_relacionado;palavras_chave_encontradas;Projeto de Lei;Dotação Inicial;Dotação Atual;Empenhado;Liquidado;Pago;Pago Corrigido",
    "2005;Z3600 - Ministério Fictício do Cuidado;Z3601 - Unidade Fictícia;Z102 - Programa Fictício Travessia;0Z01 - Proteção Social Fictícia;3 - Outras Despesas Correntes;30 - Estados;08 - Assistência Social;Assistência;True;True;chave-alfa;1.234.567,89;2.000;3.000,50;2.500;2.400;;999.999,99"
  ), arq, bom = TRUE)
  arq
}

conferir_esquema_base_juventude <- function(base) {
  expect_identical(names(base), BASE_JUVENTUDE)
  expect_type(base$ano, "integer")
  for (col in c("orgao_cod", "orgao_nome", "uo_cod", "programa_cod", "programa_nome", "acao_cod",
                "acao_nome", "gnd_cod", "modalidade_cod", "funcao_cod", "eixo", "palavras_chave")) {
    expect_type(base[[col]], "character")
  }
  expect_type(base$flag_palavra_chave, "logical")
  expect_type(base$flag_programa_relacionado, "logical")
  for (fase in FASES) expect_type(base[[fase]], "double")
}

test_that("o esquema BASE_JUVENTUDE é o do contrato (seção 6.3)", {
  expect_identical(BASE_JUVENTUDE, c(
    "ano", "orgao_cod", "orgao_nome", "uo_cod", "programa_cod", "programa_nome",
    "acao_cod", "acao_nome", "gnd_cod", "modalidade_cod", "funcao_cod", "eixo",
    "flag_palavra_chave", "flag_programa_relacionado", "palavras_chave",
    "projeto_lei", "dotacao_inicial", "dotacao_atual", "empenhado", "liquidado", "pago"
  ))
})

test_that("ler_juventude_nova devolve o esquema comum, com códigos e tipos certos", {
  base <- ler_juventude_nova(csv_juventude_nova())
  conferir_esquema_base_juventude(base)
  expect_equal(nrow(base), 2)
  manual <- base[base$ano == 2023L, ]
  expect_identical(manual$acao_cod, "ZM01")
  expect_identical(manual$programa_cod, "Z199")
  expect_identical(manual$gnd_cod, "5")
  expect_identical(manual$orgao_cod, "Z2600")
  expect_identical(manual$orgao_nome, "Ministério Fictício do Ensino")
  # Só o primeiro " - " separa código e nome.
  expect_identical(manual$acao_nome, "Incentivo Fictício - Permanência")
  expect_false(manual$flag_palavra_chave)
  # Palavra-chave vazia vira NA; decimal com vírgula na safra nova.
  expect_true(is.na(manual$palavras_chave))
  expect_equal(manual$dotacao_inicial, 6000000000.5)
  chave <- base[base$ano == 2005L, ]
  expect_identical(chave$acao_cod, "0Z01")
  expect_identical(chave$funcao_cod, "08")
  expect_identical(chave$eixo, "na")
  expect_true(chave$flag_palavra_chave)
  expect_identical(chave$palavras_chave, "chave-alfa,chave-beta")
  expect_equal(chave$dotacao_inicial, 200.25)
  # Traço vale zero, como no SIOP.
  expect_equal(chave$pago, 0)
})

test_that("ler_juventude_antiga lê milhar com ponto e ignora as colunas 'Corrigido'", {
  base <- ler_juventude_antiga(csv_juventude_antiga())
  conferir_esquema_base_juventude(base)
  expect_equal(base$projeto_lei, 1234567.89)
  expect_equal(base$dotacao_inicial, 2000)
  expect_equal(base$dotacao_atual, 3000.5)
  # Pago vazio vale zero; a coluna "Pago Corrigido" (999.999,99) não entra.
  expect_equal(base$pago, 0)
  expect_identical(base$eixo, "Assistência")
  expect_identical(base$acao_cod, "0Z01")
})

test_that("a leitura de juventude para quando falta coluna", {
  arq <- tempfile(fileext = ".csv")
  gravar_utf8(c("ano;acao;dotacao_atual", "2005;ZA01 - X;10"), arq)
  expect_error(ler_juventude_nova(arq), "Faltam")
  expect_error(ler_juventude_nova(file.path(tempdir(), "nao_existe.csv")), "config")
})

test_that("a leitura de juventude para quando um valor não vira número", {
  arq <- csv_juventude_nova()
  linhas <- readLines(arq, encoding = "UTF-8")
  linhas[3] <- sub(";100;", ";cem;", linhas[3], fixed = TRUE)
  gravar_utf8(sub("^﻿", "", linhas), arq)
  expect_error(ler_juventude_nova(arq), "projeto_lei")
})

test_that("conferir_colunas lista as colunas que faltam", {
  expect_true(conferir_colunas(c("a", "b"), c("a", "b"), "x.csv"))
  expect_error(conferir_colunas(c("a"), c("a", "b", "c"), "x.csv"), "Faltam")
})

test_that("ler_uniao_consolidada descarta a linha Total e agrega por órgão/GND e por programa/ação", {
  arq <- tempfile(fileext = ".csv")
  cab <- "Ano,Órgão Orçamentário,Função,Programa,Ação,Grupo de Despesa,Modalidade de Aplicação,Natureza de Despesa,Fonte,Projeto de Lei,Dotação Inicial,Dotação Atual,Empenhado,Liquidado,Pago,Eixo Temático"
  gravar_utf8(c(
    cab,
    "Total,,,,,,,,,9.999.999,9.999.999,9.999.999,9.999.999,9.999.999,9.999.999,",
    "2005,\"Z2600 - Ministério Fictício do Ensino, Cultura\",12 - Educação,Z101 - Programa Fictício Horizonte,ZA01 - Bolsas Fictícias,3 - Outras Despesas Correntes,90,339039,100,1.000,2.000,3.000,4.000,5.000,6.000,Educação",
    "2005,Z2600 - Ministério Fictício do Ensino,12 - Educação,Z101 - Programa Fictício Horizonte,ZA01 - Bolsas Fictícias,3 - Outras Despesas Correntes,30,339039,100,10,20,30,40,50,60,Educação",
    "2005,1Z100 - Tribunal Fictício,01 - Legislativa,Z001 - Programa Fictício do Legislativo,ZB03 - Funcionamento Fictício,1 - Pessoal,90,319011,100,1,1,1,1,1,-,"
  ), arq, bom = TRUE)
  uniao <- ler_uniao_consolidada(arq, CFG)
  expect_named(uniao, c("por_orgao_gnd", "por_programa_acao"), ignore.order = TRUE)
  og <- uniao$por_orgao_gnd
  expect_identical(names(og), c("ano", "orgao_cod", "gnd_cod", FASES))
  expect_type(og$ano, "integer")
  expect_setequal(og$ano, 2005L)
  ensino <- og[og$orgao_cod == "Z2600" & og$gnd_cod == "3", ]
  expect_equal(nrow(ensino), 1)
  expect_equal(ensino$projeto_lei, 1010)
  expect_equal(ensino$pago, 6060)
  expect_equal(og$pago[og$orgao_cod == "1Z100"], 0)
  # A linha "Total" (9.999.999) não pode entrar em nenhuma soma.
  expect_equal(sum(og$projeto_lei), 1011)
  pa <- uniao$por_programa_acao
  expect_identical(names(pa), c("ano", "programa_cod", "acao_cod", FASES))
  expect_equal(pa$empenhado[pa$acao_cod == "ZA01"], 4040)
})

test_that("ler_uniao_nova lê a base por ano e GND", {
  arq <- tempfile(fileext = ".csv")
  gravar_utf8(c(
    "ano;grupo_despesa;projeto_lei;dotacao_inicial;dotacao_atual;empenhado;liquidado;pago",
    "2025;3 - Outras Despesas Correntes;100;200;300;400;500;600,5",
    "2025;1 - Pessoal e Encargos Sociais;1;2;3;4;5;6"
  ), arq, bom = TRUE)
  un <- ler_uniao_nova(arq)
  expect_identical(names(un), c("ano", "gnd_cod", FASES))
  expect_type(un$ano, "integer")
  expect_setequal(un$gnd_cod, c("1", "3"))
  expect_equal(un$pago[un$gnd_cod == "3"], 600.5)
})

test_that("ler_ipca lê ano inteiro e fator com ponto decimal", {
  arq <- tempfile(fileext = ".csv")
  gravar_utf8(c("ano,fator_correcao", "2024,1.0512", "2023,1.0811234"), arq)
  ipca <- ler_ipca(arq)
  expect_identical(names(ipca), c("ano", "fator"))
  expect_type(ipca$ano, "integer")
  expect_identical(ipca$ano, c(2023L, 2024L))
  expect_equal(ipca$fator[ipca$ano == 2023L], 1.0811234)
  gravar_utf8(c("ano,fator_correcao", "2023,1.31", "2023,1.32"), arq)
  expect_error(ler_ipca(arq), "repetido")
})

# ---- integração: o que o pipeline leu dos arquivos configurados -------------

test_that("juventude_nova (pipeline) tem o esquema comum e todas as linhas do recorte", {
  nova <- ler_alvo("juventude_nova")
  conferir_esquema_base_juventude(nova)
  # A lista de ações do recorte foi derivada desta mesma base: a soma das
  # linhas por ação tem de ser o número de linhas lidas.
  recorte <- ler_alvo("recorte")
  expect_equal(nrow(nova), sum(recorte$n_linhas))
  expect_identical(range(nova$ano), as.integer(c(CFG$periodo$ano_inicio, CFG$periodo$ano_fim)))
  for (fase in FASES) expect_false(anyNA(nova[[fase]]), label = paste("NA em", fase))
  expect_false(anyNA(nova$acao_cod))
  expect_false(any(grepl(" - ", nova$acao_cod, fixed = TRUE)))
})

test_that("juventude_antiga (pipeline) sai no mesmo esquema da nova", {
  antiga <- ler_alvo("juventude_antiga")
  conferir_esquema_base_juventude(antiga)
  expect_true(nrow(antiga) > 0)
})

test_that("uniao_consolidada e uniao_nova (pipeline) têm as colunas do contrato", {
  cons <- ler_alvo("uniao_consolidada")
  expect_identical(names(cons$por_orgao_gnd), c("ano", "orgao_cod", "gnd_cod", FASES))
  expect_identical(names(cons$por_programa_acao), c("ano", "programa_cod", "acao_cod", FASES))
  periodo <- as.integer(c(CFG$periodo$ano_inicio, CFG$periodo$ano_fim))
  expect_identical(range(cons$por_orgao_gnd$ano), periodo)
  nova <- ler_alvo("uniao_nova")
  expect_identical(names(nova), c("ano", "gnd_cod", FASES))
  expect_true(all(seq(periodo[1], periodo[2]) %in% nova$ano))
})

test_that("procedencia (pipeline) registra cada arquivo configurado e a configuração, com hash", {
  proc <- ler_alvo("procedencia")
  # Todo caminho configurado que existe (menos a pasta de saídas) e o próprio
  # config.yml. Um opcional vazio ou ausente fica de fora.
  caminhos <- CFG$caminhos[setdiff(names(CFG$caminhos), "saidas")]
  presentes <- names(caminhos)[vapply(caminhos, function(x) nzchar(x) && file.exists(x), logical(1))]
  expect_setequal(proc$papel, c(presentes, "config"))
  expect_true(all(grepl("^[0-9a-f]{64}$", proc$sha256)))
})
