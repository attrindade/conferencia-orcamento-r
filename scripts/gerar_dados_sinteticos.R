# Gera os dados sintéticos da vitrine:
#   dados_sinteticos/  os cinco arquivos de entrada, no mesmo formato das fontes reais, e um sexto com o fator de inflação errado, para demonstração
#   entradas/recorte_acoes.csv  a lista de ações do recorte
#   config/gabaritos.csv        os números esperados, calculados aqui a partir da verdade inventada
#
# Uso, a partir da raiz do repositório:  Rscript scripts/gerar_dados_sinteticos.R
#
# Tudo é inventado: órgãos, programas, ações (códigos começam com "Z", que o SIOP
# não usa), palavras-chave, valores (escala de dezenas de milhões) e fatores de
# inflação. O período (2005 a 2025) é o mesmo do projeto. Os anos de cada
# fenômeno (o ano do fator de inflação conferido, os anos em que as safras
# diferem, o ano em que uma ação muda de programa, os anos da inclusão manual
# obrigatória) foram escolhidos sem relação com os dados reais.
#
# Só usa o R básico de propósito: este script tem de rodar antes de o renv
# restaurar os pacotes, e os gabaritos têm de ser calculados por um caminho
# diferente do pipeline (que usa duckdb e dplyr). Se os dois caminhos chegarem
# ao mesmo número, a conferência vale alguma coisa.

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)
set.seed(20261029)

# ---------------------------------------------------------------------------
# Onde gravar: a raiz é a pasta acima de scripts/, de onde quer que se rode.
# ---------------------------------------------------------------------------
raiz <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg) == 1) {
    normalizePath(file.path(dirname(sub("^--file=", "", arg)), ".."), winslash = "/")
  } else {
    normalizePath(".", winslash = "/")
  }
})
pasta_dados <- file.path(raiz, "dados_sinteticos")
for (p in c(pasta_dados, file.path(raiz, "entradas"), file.path(raiz, "config"))) {
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
}

FASES <- c("projeto_lei", "dotacao_inicial", "dotacao_atual", "empenhado", "liquidado", "pago")
ANOS <- 2005:2025
ULTIMO_ANO_SAFRA_ANTIGA <- 2024
ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS <- 2008:2011
ANO_IPCA_CONFERIDO <- 2019
ANO_MUDANCA_DE_PROGRAMA <- 2015   # a ZA05 muda de programa neste ano
ARQUIVO_IPCA_COM_ERRO <- "ipca_fatores_com_erro.csv"
GND_INCLUIDOS <- c("1", "3", "4", "5")
ORGAOS_EXCLUIDOS <- c("Z0100", "Z7100", "Z9300")
PREFIXO_ORGAOS_EXCLUIDOS <- "1"
LIMIAR_CONCENTRACAO <- 0.80

# ---------------------------------------------------------------------------
# 1. Fatores de inflação fictícios (base 2025 = 1). Taxas anuais sorteadas
#    entre 3% e 9%, para a série ter a cara de uma deflação real sem copiar
#    nenhum fator verdadeiro.
# ---------------------------------------------------------------------------
taxas <- round(runif(length(ANOS), 0.03, 0.09), 4)
fator <- numeric(length(ANOS))
fator[length(ANOS)] <- 1
for (i in rev(seq_len(length(ANOS) - 1))) fator[i] <- fator[i + 1] * (1 + taxas[i + 1])
fator <- round(fator, 7)
names(fator) <- ANOS
# O "fator errado conhecido" é um valor plausível, mas diferente do certo, para
# o ano conferido. Fica no config.yml, para a trava recusá-lo; o arquivo de
# fatores leva o certo, e um segundo arquivo leva o errado, para demonstração.
fator_errado_conhecido <- round(fator[as.character(ANO_IPCA_CONFERIDO)] * 1.0412, 7)

# ---------------------------------------------------------------------------
# 2. Cadastro inventado
# ---------------------------------------------------------------------------
ORGAOS <- c(
  Z2000 = "Ministério Fictício da Cidadania",
  Z2500 = "Ministério Fictício da Fazenda",
  Z2600 = "Ministério Fictício do Ensino",
  Z3600 = "Ministério Fictício do Cuidado",
  Z5100 = "Ministério Fictício do Esporte",
  Z5300 = "Ministério Fictício das Obras",
  Z0100 = "Câmara Fictícia dos Representantes",
  Z7100 = "Encargos Financeiros Fictícios",
  Z9300 = "Operações Oficiais de Crédito Fictícias",
  "1Z100" = "Tribunal Fictício Superior",
  "1Z200" = "Justiça Fictícia Federal"
)
PROGRAMAS <- c(
  Z101 = "Programa Fictício Horizonte",
  Z102 = "Programa Fictício Travessia",
  Z103 = "Programa Fictício Ponte",
  Z104 = "Programa Fictício Farol",
  Z199 = "Programa Fictício Alicerce",
  Z801 = "Programa Fictício de Gestão Pública",
  Z802 = "Programa Fictício de Obras, Logística e Transporte",
  Z901 = "Operações Especiais Fictícias: Dívida",
  Z903 = "Transferências Fictícias a Entes",
  Z904 = "Financiamentos Fictícios",
  Z001 = "Programa Fictício do Legislativo",
  Z002 = "Programa Fictício do Judiciário"
)
GND <- c(
  "1" = "Pessoal e Encargos Sociais", "2" = "Juros e Encargos da Dívida",
  "3" = "Outras Despesas Correntes", "4" = "Investimentos",
  "5" = "Inversões Financeiras", "6" = "Amortização da Dívida",
  "9" = "Reserva de Contingência"
)
MODALIDADES <- c("90" = "Aplicações Diretas", "40" = "Transferências a Municípios",
                 "30" = "Transferências a Estados e ao Distrito Federal")
rotulo <- function(cod, nomes) paste0(cod, " - ", unname(nomes[cod]))

# Ações do recorte. Cada linha cria uma situação que o pipeline precisa tratar:
#  - ZG01 é genérica: aparece no recorte só dentro do Z101, mas a base da União
#    tem a mesma ação em outros programas, com valores muito maiores;
#  - ZA05 muda de programa em ANO_MUDANCA_DE_PROGRAMA e, a partir daí, a base
#    da União tem linhas da mesma ação e programa que não entram no recorte;
#  - ZD01 (palavra-chave nova) e ZM02 (inclusão manual) só existem nos anos de
#    ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS e só na safra nova: são as linhas a mais
#    entre as safras;
#  - ZM01 é inclusão manual sem palavra-chave, obrigatória no recorte.
ACOES <- data.frame(
  acao_cod   = c("ZA01", "ZA02", "ZG01", "ZA03", "ZA04", "ZA05", "ZD01", "ZM02", "ZM01"),
  acao_nome  = c("Bolsas Fictícias Aurora",
                 "Centros Comunitários Fictícios",
                 "Custeio Fictício Bússola",
                 "Apoio Fictício a Projetos Locais, Redes e Coletivos",
                 "Esporte Comunitário Fictício",
                 "Atendimento Integrado Fictício",
                 "Proteção Social Fictícia",
                 "Ação Fictícia Incluída à Mão",
                 "Oficinas Fictícias de Música Ipê"),
  orgao      = c("Z2600", "Z2000", "Z2000", "Z3600", "Z5100", "Z3600", "Z3600", "Z2000", "Z2000"),
  funcao     = c("12 - Educação", "14 - Direitos da Cidadania", "14 - Direitos da Cidadania",
                 "08 - Assistência Social", "27 - Desporto e Lazer", "08 - Assistência Social",
                 "08 - Assistência Social", "14 - Direitos da Cidadania", "13 - Cultura"),
  eixo       = c("Eixo Fictício Formação", "Eixo Fictício Convivência", "",
                 "Eixo Fictício Convivência", "Eixo Fictício Esporte", "Eixo Fictício Cuidado",
                 "Eixo Fictício Cuidado", "", "Eixo Fictício Convivência"),
  palavras   = c("chave-alfa", "chave-beta", "chave-alfa", "chave-alfa,chave-gama",
                 "chave-beta", "chave-gama", "chave-delta", "", ""),
  colunas_kw = c("programa,acao", "acao", "programa", "programa,acao", "acao", "acao",
                 "acao", "", ""),
  flag_kw    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  ano_ini    = c(2005, 2005, 2005, 2005, 2005, 2005, min(ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS),
                 min(ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS), 2017),
  ano_fim    = c(2025, 2025, 2025, 2025, 2025, 2025, max(ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS),
                 max(ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS), 2022),
  linhas     = c(3, 2, 1, 3, 2, 2, 2, 2, 2),
  escala     = c(9e6, 5e6, 2e6, 4e6, 6e6, 7e6, 3e6, 1.5e6, 8e6)
)
programa_da_acao <- function(acao, ano) {
  switch(acao,
         ZA01 = "Z101", ZA02 = "Z101", ZG01 = "Z101", ZA03 = "Z102", ZA04 = "Z103",
         ZA05 = if (ano < ANO_MUDANCA_DE_PROGRAMA) "Z102" else "Z104",
         ZD01 = "Z103", ZM02 = "Z199", ZM01 = "Z199")
}
PROGRAMA_RELACIONADO <- c(Z101 = TRUE, Z102 = FALSE, Z103 = FALSE, Z104 = FALSE, Z199 = TRUE)

# Tendência real do recorte por ano (sem inflação), sorteada uma vez para o
# ranking de anos ter uma ordem clara.
tendencia <- setNames(round(runif(length(ANOS), 0.6, 1.6), 3), ANOS)

# ---------------------------------------------------------------------------
# 3. Linhas do recorte, safra nova (a verdade)
# ---------------------------------------------------------------------------
sortear_fases <- function(dotacao_atual) {
  # Mantém a ordem lógica das fases: o empenho não passa da dotação, a
  # liquidação não passa do empenho, o pagamento não passa da liquidação.
  dotacao_atual <- unname(dotacao_atual)
  pl  <- round(dotacao_atual * runif(1, 0.85, 1.05))
  di  <- round(pl * runif(1, 0.97, 1.03))
  emp <- round(dotacao_atual * runif(1, 0.70, 0.98))
  liq <- round(emp * runif(1, 0.85, 1.00))
  pag <- round(liq * runif(1, 0.90, 1.00))
  c(projeto_lei = pl, dotacao_inicial = di, dotacao_atual = dotacao_atual,
    empenhado = emp, liquidado = liq, pago = pag)
}

linhas <- list()
for (k in seq_len(nrow(ACOES))) {
  a <- ACOES[k, ]
  for (ano in a$ano_ini:a$ano_fim) {
    prog <- programa_da_acao(a$acao_cod, ano)
    for (i in seq_len(a$linhas)) {
      gnd <- if (a$acao_cod %in% c("ZA02", "ZA04") && i == 2) "4" else
        if (a$acao_cod == "ZG01") "1" else "3"
      mod <- if (a$acao_cod %in% c("ZA03", "ZM01") && i >= 2) "40" else
        if (a$acao_cod == "ZA04" && i == 2) "30" else "90"
      da <- round(a$escala * tendencia[as.character(ano)] / fator[as.character(ano)] *
                    rlnorm(1, 0, 0.35))
      v <- sortear_fases(da)
      # Crédito extraordinário: aparece só na dotação atual, sem projeto de lei
      # nem dotação inicial. Existe nas fontes reais e testa as somas com zero.
      if (a$acao_cod == "ZA02" && ano == 2009 && i == 1) v[c("projeto_lei", "dotacao_inicial")] <- 0
      linhas[[length(linhas) + 1]] <- data.frame(
        ano = ano, orgao_cod = a$orgao, funcao = a$funcao,
        uo = paste0(substr(a$orgao, 1, 4), "1 - Unidade Central Fictícia (", unname(ORGAOS[a$orgao]), ")"),
        programa_cod = prog, acao_cod = a$acao_cod, acao_nome = a$acao_nome,
        localizador = sprintf("L%03d - Localidade Fictícia %d", i, i),
        gnd_cod = gnd, modalidade_cod = mod,
        fonte = if (i %% 2 == 1) "100 - Recursos Fictícios Ordinários" else "300 - Recursos Fictícios Vinculados",
        objetivo = if (ano >= ANO_MUDANCA_DE_PROGRAMA) "Z0001 - Objetivo Fictício de Inclusão" else "[valor não detalhado]",
        eixo = a$eixo, palavras = a$palavras, colunas_kw = a$colunas_kw,
        flag_kw = a$flag_kw, flag_prog = unname(PROGRAMA_RELACIONADO[prog]),
        t(v), check.names = FALSE
      )
    }
  }
}
nova <- do.call(rbind, linhas)
nova <- nova[order(nova$ano, nova$acao_cod, nova$localizador), ]
rownames(nova) <- NULL

# ---------------------------------------------------------------------------
# 4. Safra antiga do recorte: igual à nova, menos as linhas que só a nova tem
#    (ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS) e com 2025 incompleto (extração feita
#    antes de o ano fechar).
# ---------------------------------------------------------------------------
so_na_nova <- nova$acao_cod %in% c("ZD01", "ZM02") & nova$ano %in% ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS
antiga <- nova[!so_na_nova, ]
incompleto_2025 <- function(df, frac_linhas, frac_execucao) {
  # Metade das linhas ainda não existia e a execução estava pela metade.
  e25 <- which(df$ano == 2025)
  manter <- e25[seq_len(ceiling(length(e25) * frac_linhas))]
  df <- df[df$ano != 2025 | seq_len(nrow(df)) %in% manter, ]
  r <- df$ano == 2025
  for (f in c("empenhado", "liquidado", "pago")) df[[f]][r] <- round(df[[f]][r] * frac_execucao)
  df
}
antiga <- incompleto_2025(antiga, 0.5, 0.6)

# ---------------------------------------------------------------------------
# 5. Base da União (a verdade inteira), no nível de detalhe da base consolidada
# ---------------------------------------------------------------------------
chave_uniao <- c("ano", "orgao_cod", "funcao", "programa_cod", "acao_cod", "gnd_cod",
                 "modalidade_cod", "fonte")
uniao_recorte <- aggregate(nova[FASES], by = nova[chave_uniao], FUN = sum)
uniao_recorte$acao_nome <- ACOES$acao_nome[match(uniao_recorte$acao_cod, ACOES$acao_cod)]

ACOES_FORA <- c(
  ZB01 = "Administração Fictícia da Unidade",
  ZB02 = "Obras Fictícias de Infraestrutura",
  ZB03 = "Funcionamento Fictício do Parlamento",
  ZB04 = "Julgamento Fictício de Causas",
  ZB05 = "Repasses Fictícios Constitucionais",
  ZB06 = "Concessão Fictícia de Financiamentos",
  ZB07 = "Reserva Fictícia de Contingência",
  ZB08 = "Amortização Fictícia da Dívida",
  ZB09 = "Juros Fictícios da Dívida",
  ZG01 = "Custeio Fictício Bússola",
  ZA05 = "Atendimento Integrado Fictício"
)
# Linhas fora do recorte: (órgão, função, programa, ação, GND, modalidade, escala em reais de 2025)
FORA <- data.frame(
  orgao_cod    = c("Z2000", "Z2500", "Z2600", "Z3600", "Z5100", "Z5300", "Z2600", "Z3600",
                   "Z2500", "Z5300", "Z5300", "Z5300", "Z3600",
                   "Z0100", "1Z100", "1Z200", "Z7100", "Z9300", "Z2500", "Z2500", "Z2500"),
  funcao       = c("04 - Administração", "04 - Administração", "12 - Educação", "08 - Assistência Social",
                   "27 - Desporto e Lazer", "26 - Transporte", "12 - Educação", "08 - Assistência Social",
                   "04 - Administração", "26 - Transporte", "26 - Transporte", "26 - Transporte",
                   "08 - Assistência Social",
                   "01 - Legislativa", "02 - Judiciária", "02 - Judiciária", "28 - Encargos Especiais",
                   "28 - Encargos Especiais", "28 - Encargos Especiais", "28 - Encargos Especiais",
                   "99 - Reserva de Contingência"),
  programa_cod = c("Z801", "Z801", "Z801", "Z801", "Z801", "Z801", "Z801", "Z801",
                   "Z801", "Z801", "Z802", "Z802", "Z104",
                   "Z001", "Z002", "Z002", "Z903", "Z904", "Z901", "Z901", "Z901"),
  acao_cod     = c("ZB01", "ZB01", "ZB01", "ZB01", "ZB01", "ZB01", "ZB01", "ZB01",
                   "ZG01", "ZG01", "ZB02", "ZB02", "ZA05",
                   "ZB03", "ZB04", "ZB04", "ZB05", "ZB06", "ZB09", "ZB08", "ZB07"),
  gnd_cod      = c("1", "1", "1", "1", "1", "1", "3", "3",
                   "3", "3", "4", "5", "3",
                   "1", "1", "3", "3", "5", "2", "6", "9"),
  modalidade_cod = c("90", "90", "90", "90", "90", "90", "90", "40",
                     "90", "90", "90", "90", "90",
                     "90", "90", "90", "30", "90", "90", "90", "90"),
  escala       = c(1.2e9, 2.5e9, 3.1e9, 1.8e9, 0.4e9, 0.9e9, 4.2e9, 2.2e9,
                   0.8e9, 0.5e9, 1.6e9, 0.3e9, 0.15e9,
                   0.7e9, 1.1e9, 0.4e9, 3.5e9, 0.6e9, 6.0e9, 4.5e9, 0.2e9),
  ano_ini      = c(rep(2005, 12), ANO_MUDANCA_DE_PROGRAMA, rep(2005, 8))
)
# Encolhe a União inteira para a participação sintética do recorte ficar em
# alguns por cento, escala bem diferente da real. Os percentuais sintéticos
# ainda vão para os gabaritos com 4 casas, para nunca coincidirem por acaso com
# um número real na checagem anti-vazamento.
ESCALA_UNIAO <- 0.07
linhas_fora <- list()
for (k in seq_len(nrow(FORA))) {
  f <- FORA[k, ]
  for (ano in f$ano_ini:max(ANOS)) {
    da <- round(ESCALA_UNIAO * f$escala * runif(1, 0.9, 1.15) / fator[as.character(ano)])
    v <- sortear_fases(da)
    # Reserva de contingência não se executa: só aparece no projeto e nas dotações.
    if (f$acao_cod == "ZB07") v[c("empenhado", "liquidado", "pago")] <- 0
    linhas_fora[[length(linhas_fora) + 1]] <- data.frame(
      ano = ano, orgao_cod = f$orgao_cod, funcao = f$funcao, programa_cod = f$programa_cod,
      acao_cod = f$acao_cod, gnd_cod = f$gnd_cod, modalidade_cod = f$modalidade_cod,
      fonte = "100 - Recursos Fictícios Ordinários", t(v),
      acao_nome = unname(ACOES_FORA[f$acao_cod]), check.names = FALSE
    )
  }
}
uniao <- rbind(uniao_recorte, do.call(rbind, linhas_fora))
uniao <- uniao[order(uniao$ano, uniao$orgao_cod, uniao$programa_cod, uniao$acao_cod, uniao$gnd_cod), ]
rownames(uniao) <- NULL

# Base consolidada (safra antiga): igual à verdade até 2024; 2025 incompleto.
consolidada <- incompleto_2025(uniao, 0.85, 0.65)
consolidada$dotacao_atual[consolidada$ano == 2025] <-
  round(consolidada$dotacao_atual[consolidada$ano == 2025] * 0.97)

# União por GND (safra nova): a verdade inteira, inclusive 2025 completo.
uniao_nova <- aggregate(uniao[FASES], by = uniao[c("ano", "gnd_cod")], FUN = sum)
uniao_nova <- uniao_nova[order(uniao_nova$ano, uniao_nova$gnd_cod), ]

# ---------------------------------------------------------------------------
# 6. Gravação, imitando separadores, números, BOM e quebras de linha (CRLF)
# ---------------------------------------------------------------------------
gravar_linhas <- function(linhas_txt, caminho, bom = FALSE) {
  con <- file(caminho, open = "wb")
  on.exit(close(con))
  if (bom) writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  writeBin(charToRaw(enc2utf8(paste0(paste(linhas_txt, collapse = "\r\n"), "\r\n"))), con)
}
inteiro <- function(x) formatC(x, format = "f", digits = 0, big.mark = "")
milhar_br <- function(x) formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
aspas_se_preciso <- function(x) ifelse(grepl("[,\"]", x), paste0("\"", gsub("\"", "\"\"", x), "\""), x)
verdadeiro_falso <- function(x) ifelse(x, "True", "False")
rotulo_gnd <- function(g) paste0(g, " - ", GND[g])
rotulo_mod <- function(m) paste0(m, " - ", MODALIDADES[m])
natureza <- function(g, m) paste0(g, "3", m, "0000 - ", MODALIDADES[m])
rotulo_acao <- function(df) paste0(df$acao_cod, " - ", df$acao_nome)

arquivos <- c(
  juventude_nova    = "juventude_safra_nova.csv",
  juventude_antiga  = "juventude_safra_antiga.csv",
  uniao_consolidada = "uniao_consolidada_safra_antiga.csv",
  uniao_nova        = "uniao_por_gnd_safra_nova.csv",
  ipca              = "ipca_fatores.csv"
)

# 6.1 Recorte, safra nova: ";" e números sem separador de milhar.
cab_nova <- c("ano", "orgao_orcamentario", "funcao", "unidade_orcamentaria", "programa", "acao",
              "localizador", "regiao", "uf", "municipio", "plano_orcamentario", "grupo_despesa",
              "modalidade_aplicacao", "natureza_despesa", "resultado_primario", "fonte", "subfuncao",
              "origem_credito", "objetivo", "projeto_lei", "dotacao_inicial", "dotacao_atual",
              "empenhado", "liquidado", "pago", "classificacao_poder_esfera", "flag_funcao_subfuncao",
              "palavras_chave_encontradas", "colunas_palavra_chave", "flag_palavra_chave",
              "flag_programa_relacionado", "eixo_tematico")
campos_comuns <- function(df) {
  list(
    ano = df$ano, orgao = rotulo(df$orgao_cod, ORGAOS), funcao = df$funcao, uo = df$uo,
    programa = rotulo(df$programa_cod, PROGRAMAS), acao = rotulo_acao(df),
    localizador = df$localizador, regiao = "NA - Nacional", uf = "-", municipio = "-",
    po = "[valor não detalhado]", gnd = rotulo_gnd(df$gnd_cod), mod = rotulo_mod(df$modalidade_cod),
    nat = natureza(df$gnd_cod, df$modalidade_cod), rp = "2 - Primária discricionária fictícia",
    fonte = df$fonte, subfuncao = "Z01 - Subfunção Fictícia", origem = "A - Inicial (LOA)",
    objetivo = df$objetivo
  )
}
cc <- campos_comuns(nova)
linhas_nova <- paste(
  cc$ano, cc$orgao, cc$funcao, cc$uo, cc$programa, cc$acao, cc$localizador, cc$regiao, cc$uf,
  cc$municipio, cc$po, cc$gnd, cc$mod, cc$nat, cc$rp, cc$fonte, cc$subfuncao, cc$origem, cc$objetivo,
  inteiro(nova$projeto_lei), inteiro(nova$dotacao_inicial), inteiro(nova$dotacao_atual),
  inteiro(nova$empenhado), inteiro(nova$liquidado), inteiro(nova$pago),
  "Executivo", "False", nova$palavras, nova$colunas_kw, verdadeiro_falso(nova$flag_kw),
  verdadeiro_falso(nova$flag_prog), nova$eixo, sep = ";")
gravar_linhas(c(paste(cab_nova, collapse = ";"), linhas_nova),
              file.path(pasta_dados, arquivos[["juventude_nova"]]), bom = TRUE)

# 6.2 Recorte, safra antiga: cabeçalho em português, milhar com ponto e as seis
#     colunas "Corrigido", que aqui NÃO estão deflacionadas (repetem o valor
#     nominal). O pipeline não pode usá-las.
cab_antiga <- c("Ano", "Órgão Orçamentário", "Classificacao (Poder/Esfera)", "Função",
                "Unidade Orçamentária", "Programa", "Ação", "Localizador", "Região", "UF", "Município",
                "Plano Orçamentário", "Grupo de Despesa", "Modalidade de Aplicação", "Natureza de Despesa",
                "Resultado Primário", "Fonte", "Subfunção", "Origem do Crédito", "Objetivo",
                "Projeto de Lei", "Dotação Inicial", "Dotação Atual", "Empenhado", "Liquidado", "Pago",
                "flag_funcao_subfuncao", "palavras_chave_encontradas", "colunas_palavra_chave",
                "flag_palavra_chave", "flag_programa_relacionado", "Projeto de Lei Corrigido",
                "Dotação Inicial Corrigido", "Dotação Atual Corrigido", "Empenhado Corrigido",
                "Liquidado Corrigido", "Pago Corrigido", "Eixo Temático", "codigo_acao", "flag_juventude")
ca <- campos_comuns(antiga)
corrigido <- function(x) sprintf("%.1f", x)
colunas_kw_antiga <- gsub("acao", "Ação", gsub("programa", "Programa", antiga$colunas_kw))
linhas_antiga <- paste(
  ca$ano, ca$orgao, "Executivo", ca$funcao, ca$uo, ca$programa, ca$acao, ca$localizador, ca$regiao,
  ca$uf, ca$municipio, ca$po, ca$gnd, ca$mod, ca$nat, ca$rp, ca$fonte, ca$subfuncao, ca$origem,
  ca$objetivo,
  milhar_br(antiga$projeto_lei), milhar_br(antiga$dotacao_inicial), milhar_br(antiga$dotacao_atual),
  milhar_br(antiga$empenhado), milhar_br(antiga$liquidado), milhar_br(antiga$pago),
  "False", antiga$palavras, colunas_kw_antiga, verdadeiro_falso(antiga$flag_kw),
  verdadeiro_falso(antiga$flag_prog),
  corrigido(antiga$projeto_lei), corrigido(antiga$dotacao_inicial), corrigido(antiga$dotacao_atual),
  corrigido(antiga$empenhado), corrigido(antiga$liquidado), corrigido(antiga$pago),
  antiga$eixo, antiga$acao_cod, "True", sep = ";")
gravar_linhas(c(paste(cab_antiga, collapse = ";"), linhas_antiga),
              file.path(pasta_dados, arquivos[["juventude_antiga"]]), bom = TRUE)

# 6.3 Base consolidada da União: "," como separador (campo com vírgula vai
#     entre aspas), milhar com ponto, sem centavos, linha "Total" logo depois
#     do cabeçalho e colunas "Corrigido" calculadas com o fator errado do ano
#     conferido (o pipeline não pode usá-las).
fator_antes_da_errata <- fator
fator_antes_da_errata[as.character(ANO_IPCA_CONFERIDO)] <- fator_errado_conhecido
cab_uniao <- c("Ano", "Órgão Orçamentário", "Função", "Programa", "Ação", "Grupo de Despesa",
               "Modalidade de Aplicação", "Natureza de Despesa", "Fonte", "Projeto de Lei",
               "Dotação Inicial", "Dotação Atual", "Empenhado", "Liquidado", "Pago",
               "Projeto de Lei Corrigido", "Dotação Inicial Corrigido", "Dotação Atual Corrigido",
               "Empenhado Corrigido", "Liquidado Corrigido", "Pago Corrigido", "Eixo Temático")
fc <- fator_antes_da_errata[as.character(consolidada$ano)]
corr_uniao <- sapply(FASES, function(f) consolidada[[f]] * fc)
linhas_uniao <- paste(
  consolidada$ano, aspas_se_preciso(rotulo(consolidada$orgao_cod, ORGAOS)),
  aspas_se_preciso(consolidada$funcao),
  aspas_se_preciso(rotulo(consolidada$programa_cod, PROGRAMAS)),
  aspas_se_preciso(rotulo_acao(consolidada)), rotulo_gnd(consolidada$gnd_cod),
  rotulo_mod(consolidada$modalidade_cod), natureza(consolidada$gnd_cod, consolidada$modalidade_cod),
  consolidada$fonte,
  milhar_br(consolidada$projeto_lei), milhar_br(consolidada$dotacao_inicial),
  milhar_br(consolidada$dotacao_atual), milhar_br(consolidada$empenhado),
  milhar_br(consolidada$liquidado), milhar_br(consolidada$pago),
  sprintf("%.2f", corr_uniao[, 1]), sprintf("%.2f", corr_uniao[, 2]), sprintf("%.2f", corr_uniao[, 3]),
  sprintf("%.2f", corr_uniao[, 4]), sprintf("%.2f", corr_uniao[, 5]), sprintf("%.2f", corr_uniao[, 6]),
  "na", sep = ",")
linha_total <- paste(c("Total", rep("", 8),
                       milhar_br(colSums(consolidada[FASES])),
                       sprintf("%.2f", colSums(corr_uniao)), "na"), collapse = ",")
gravar_linhas(c(paste(cab_uniao, collapse = ","), linha_total, linhas_uniao),
              file.path(pasta_dados, arquivos[["uniao_consolidada"]]))

# 6.4 União por GND, safra nova: ";" e números sem milhar.
linhas_un <- paste(uniao_nova$ano, rotulo_gnd(uniao_nova$gnd_cod),
                   inteiro(uniao_nova$projeto_lei), inteiro(uniao_nova$dotacao_inicial),
                   inteiro(uniao_nova$dotacao_atual), inteiro(uniao_nova$empenhado),
                   inteiro(uniao_nova$liquidado), inteiro(uniao_nova$pago), sep = ";")
gravar_linhas(c(paste(c("ano", "grupo_despesa", FASES), collapse = ";"), linhas_un),
              file.path(pasta_dados, arquivos[["uniao_nova"]]))

# 6.5 Fatores de inflação: "," e ponto decimal. Um segundo arquivo traz o fator
#     errado do ano conferido, para quem quiser ver a trava do pipeline recusá-lo.
gravar_ipca <- function(f, nome) {
  gravar_linhas(c("ano,fator_correcao", paste(names(f), sprintf("%.7f", f), sep = ",")),
                file.path(pasta_dados, nome))
}
gravar_ipca(fator, arquivos[["ipca"]])
gravar_ipca(fator_antes_da_errata, ARQUIVO_IPCA_COM_ERRO)

# ---------------------------------------------------------------------------
# 7. Lista de ações do recorte (formato de derivar_recorte)
# ---------------------------------------------------------------------------
rec <- do.call(rbind, lapply(split(nova, nova$acao_cod), function(d) {
  data.frame(
    acao_cod = d$acao_cod[1],
    acao_nome = d$acao_nome[which.max(d$ano)],
    programas = paste(sort(unique(d$programa_cod)), collapse = "|"),
    origem = if (any(d$flag_kw)) "palavra-chave" else "inclusão manual",
    primeiro_ano = min(d$ano), ultimo_ano = max(d$ano), n_linhas = nrow(d)
  )
}))
rec <- rec[order(rec$acao_cod), ]
gravar_linhas(c(paste(names(rec), collapse = ","),
                apply(rec, 1, function(r) paste(aspas_se_preciso(trimws(r)), collapse = ","))),
              file.path(raiz, "entradas", "recorte_acoes.csv"))

# ---------------------------------------------------------------------------
# 8. Gabaritos, calculados direto da verdade (nova, uniao, consolidada, fator)
# ---------------------------------------------------------------------------
FONTE <- "scripts/gerar_dados_sinteticos.R (verdade sintética)"
base <- nova[nova$ano %in% ANOS, ]
real <- base
for (f in FASES) real[[f]] <- base[[f]] * fator[as.character(base$ano)]

gab <- list()
add <- function(id, descricao, metrica, fase, ano, filtro, unidade, valor, observacao = "") {
  gab[[length(gab) + 1]] <<- data.frame(
    id = id, descricao = descricao, fonte = FONTE, metrica = metrica, fase = fase,
    ano = ifelse(is.na(ano), "", as.character(ano)), filtro = filtro, unidade = unidade,
    valor_esperado = valor, observacao = observacao)
}
reais <- function(x) sprintf("%.2f", x)
pct <- function(x) sprintf("%.4f", x)  # 4 casas: com 2, um percentual sintético poderia coincidir com um total real arredondado em bilhões

soma_real_ano <- sapply(FASES, function(f) tapply(real[[f]], real$ano, sum))
for (f in FASES) for (ano in ANOS) {
  add(sprintf("total_ano_%s_%d", f, ano), sprintf("Total do recorte em %d, %s, em reais de 2025", ano, f),
      "total_ano", f, ano, "", "reais", reais(soma_real_ano[as.character(ano), f]))
}
for (f in FASES) {
  tot <- sum(real[[f]])
  add(paste0("total_periodo_", f), sprintf("Total do recorte de 2005 a 2025, %s", f),
      "total_periodo", f, NA, "", "reais", reais(tot))
  add(paste0("media_anual_", f), sprintf("Média anual do recorte de 2005 a 2025, %s", f),
      "media_anual", f, NA, "", "reais", reais(tot / length(ANOS)))
}
for (f in c("dotacao_atual", "empenhado")) for (g in sort(unique(real$gnd_cod))) {
  add(sprintf("total_gnd_%s_%s", g, f), sprintf("Total do período no GND %s, %s", g, f),
      "total_gnd", f, NA, paste0("gnd_cod=", g), "reais", reais(sum(real[[f]][real$gnd_cod == g])))
  add(sprintf("total_gnd_%s_%s_pct", g, f), sprintf("Peso do GND %s no total do período, %s", g, f),
      "total_gnd", f, NA, paste0("gnd_cod=", g), "pct",
      pct(100 * sum(real[[f]][real$gnd_cod == g]) / sum(real[[f]])),
      "Percentual do total da fase no período, valores reais.")
}
# Mesma métrica em valor nominal ("valor=nominal" no filtro, gramática do
# conferir.R): cobre o caminho sem deflação.
add("total_ano_dotacao_atual_2010_nominal", "Total do recorte em 2010, dotacao_atual, nominal",
    "total_ano", "dotacao_atual", 2010, "valor=nominal", "reais",
    reais(sum(base$dotacao_atual[base$ano == 2010])), "Valor nominal")
# Peso de uma ação no total do ano (razão dentro do ano: o deflator se cancela).
add("total_acao_ZM01_2021_empenhado_pct", "Peso da ação ZM01 no empenhado de 2021",
    "total_acao_ano", "empenhado", 2021, "acao_cod=ZM01", "pct",
    pct(100 * sum(base$empenhado[base$acao_cod == "ZM01" & base$ano == 2021]) /
          sum(base$empenhado[base$ano == 2021])),
    "Percentual do empenhado total do ano.")
for (x in list(c("ZM01", 2021, "dotacao_atual"), c("ZA01", 2010, "empenhado"),
               c("ZD01", 2011, "dotacao_atual"), c("ZG01", 2005, "pago"))) {
  sel <- real$acao_cod == x[1] & real$ano == as.integer(x[2])
  add(sprintf("total_acao_%s_%s_%s", x[1], x[2], x[3]),
      sprintf("Ação %s em %s, %s", x[1], x[2], x[3]),
      "total_acao_ano", x[3], as.integer(x[2]), paste0("acao_cod=", x[1]), "reais",
      reais(sum(real[[x[3]]][sel])))
}
add("taxa_execucao_periodo", "Empenhado sobre dotação atual, período inteiro, em reais de 2025",
    "taxa_execucao_periodo", "empenhado", NA, "", "pct",
    pct(100 * sum(real$empenhado) / sum(real$dotacao_atual)))

# Denominador: gasto do Executivo, nominal, empenhado. De 2005 a 2024 vem da
# base consolidada; em 2025 a fração Executivo/União da consolidada (incompleta)
# é aplicada ao total da União da safra nova.
e_executivo <- function(df) {
  df$gnd_cod %in% GND_INCLUIDOS & !df$orgao_cod %in% ORGAOS_EXCLUIDOS &
    !startsWith(df$orgao_cod, PREFIXO_ORGAOS_EXCLUIDOS)
}
exec_cons <- tapply(consolidada$empenhado[e_executivo(consolidada)],
                    consolidada$ano[e_executivo(consolidada)], sum)
uniao_cons_gnd <- tapply(consolidada$empenhado[consolidada$gnd_cod %in% GND_INCLUIDOS],
                         consolidada$ano[consolidada$gnd_cod %in% GND_INCLUIDOS], sum)
uniao_nova_gnd <- tapply(uniao_nova$empenhado[uniao_nova$gnd_cod %in% GND_INCLUIDOS],
                         uniao_nova$ano[uniao_nova$gnd_cod %in% GND_INCLUIDOS], sum)
executivo <- exec_cons
executivo["2025"] <- uniao_nova_gnd["2025"] * exec_cons["2025"] / uniao_cons_gnd["2025"]
emp_nominal <- tapply(base$empenhado, base$ano, sum)
for (ano in ANOS) {
  est <- ano > ULTIMO_ANO_SAFRA_ANTIGA
  add(sprintf("participacao_%d", ano),
      sprintf("Participação do recorte no empenhado do Executivo em %d", ano),
      "participacao_ano", "empenhado", ano, "", "pct",
      pct(100 * emp_nominal[as.character(ano)] / executivo[as.character(ano)]),
      if (est) "Denominador estimado (fração Executivo/União da safra antiga sobre a União da safra nova)" else "Valores nominais")
}
# Ano de maior participação: um valor dentro da tolerância não garante que o
# ano citado como máximo seja o máximo, por isso ele tem gabarito próprio.
participacao <- 100 * emp_nominal[as.character(ANOS)] / executivo[as.character(ANOS)]
ordem_part <- sort(participacao, decreasing = TRUE)
if (ordem_part[1] - ordem_part[2] < 1e-3) stop("Ano de maior participação ambíguo: troque a semente.")
add("ano_maximo_participacao", "Ano de maior participação do recorte no empenhado do Executivo",
    "ano_maximo_participacao", "empenhado", NA, "", "lista", names(ordem_part)[1],
    "Ano com a maior participação, empenhado nominal sobre empenhado nominal do Executivo.")
for (ano in ANOS[ANOS <= ULTIMO_ANO_SAFRA_ANTIGA]) {
  add(sprintf("denominador_%d", ano), sprintf("Empenhado do Executivo em %d (nominal)", ano),
      "denominador_executivo_ano", "empenhado", ano, "", "reais",
      reais(executivo[as.character(ano)]), "Valor nominal")
}

add("n_programas", "Programas distintos no recorte", "n_programas", "", NA, "", "contagem",
    length(unique(base$programa_cod)))
add("n_acoes", "Ações distintas no recorte", "n_acoes", "", NA, "", "contagem",
    length(unique(base$acao_cod)))

# Concentração: quantos programas, do maior para o menor, somam o limiar.
conc <- sort(tapply(real$dotacao_atual, real$programa_cod, sum), decreasing = TRUE)
acum <- cumsum(conc) / sum(conc)
n_limiar <- which(acum >= LIMIAR_CONCENTRACAO)[1]
if (any(abs(acum - LIMIAR_CONCENTRACAO) < 0.005)) stop("Concentração ambígua perto do limiar: troque a semente.")
add("n_programas_limiar", "Programas que somam 80% da dotação atual do período",
    "n_programas_limiar", "dotacao_atual", NA, "", "contagem", n_limiar)

ranking <- names(sort(soma_real_ano[, "dotacao_atual"], decreasing = TRUE))[1:4]
dif_ranking <- -diff(sort(soma_real_ano[, "dotacao_atual"], decreasing = TRUE)[1:5])
if (any(dif_ranking < 1000)) stop("Ranking de anos ambíguo: troque a semente.")
add("ranking_dotacao", "Os quatro anos de maior dotação atual, em reais de 2025", "ranking_dotacao",
    "dotacao_atual", NA, "", "lista", paste(ranking, collapse = "|"))

gab <- do.call(rbind, gab)
gravar_linhas(c(paste(names(gab), collapse = ","),
                apply(gab, 1, function(r) paste(aspas_se_preciso(trimws(r)), collapse = ","))),
              file.path(raiz, "config", "gabaritos.csv"))

# ---------------------------------------------------------------------------
# 9. Resumo e conferências do próprio gerador
# ---------------------------------------------------------------------------
# A safra antiga tem de ser idêntica à nova em todos os anos completos, menos
# nos de ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS; se não for, o gerador quebrou o
# fenômeno.
iguais <- sapply(ANOS[ANOS <= ULTIMO_ANO_SAFRA_ANTIGA], function(a) {
  isTRUE(all.equal(colSums(nova[nova$ano == a, FASES]), colSums(antiga[antiga$ano == a, FASES]))) &&
    sum(nova$ano == a) == sum(antiga$ano == a)
})
esperado <- !(ANOS[ANOS <= ULTIMO_ANO_SAFRA_ANTIGA] %in% ANOS_SAFRA_NOVA_COM_LINHAS_A_MAIS)
stopifnot(identical(unname(iguais), esperado))
# A União consolidada tem de bater com a safra nova de 2005 a 2024 e não em 2025.
por_ano_cons <- aggregate(consolidada[FASES], by = consolidada["ano"], FUN = sum)
por_ano_nova <- aggregate(uniao_nova[FASES], by = uniao_nova["ano"], FUN = sum)
dif <- as.matrix(por_ano_cons[FASES]) - as.matrix(por_ano_nova[FASES])
stopifnot(all(dif[por_ano_cons$ano <= ULTIMO_ANO_SAFRA_ANTIGA, ] == 0),
          all(dif[por_ano_cons$ano == 2025, ] != 0))

cat("Dados sintéticos gravados em", pasta_dados, "\n")
for (n in names(arquivos)) {
  caminho <- file.path(pasta_dados, arquivos[[n]])
  cat(sprintf("  %-18s %-38s %6d linhas\n", n, arquivos[[n]], length(readLines(caminho, warn = FALSE)) - 1))
}
cat(sprintf("  recorte_acoes.csv: %d ações; gabaritos.csv: %d números esperados\n", nrow(rec), nrow(gab)))
cat(sprintf("  Fator de %d: certo %.7f, errado conhecido %.7f\n", ANO_IPCA_CONFERIDO,
            fator[as.character(ANO_IPCA_CONFERIDO)], fator_errado_conhecido))
# Maior diferença entre as safras do recorte, por ano e fase, nos anos
# completos da safra antiga: é o valor que config/causas_conhecidas.yml
# registra como limite da diferença explicada.
anos_completos <- ANOS[ANOS <= ULTIMO_ANO_SAFRA_ANTIGA]
dif_safras <- sapply(FASES, function(f) {
  tapply(nova[[f]], factor(nova$ano, anos_completos), sum, default = 0) -
    tapply(antiga[[f]], factor(antiga$ano, anos_completos), sum, default = 0)
})
cat(sprintf("  Safras do recorte: diferem em %s; maior diferença %s reais\n",
            paste(anos_completos[rowSums(dif_safras != 0) > 0], collapse = ", "),
            formatC(max(abs(dif_safras)), format = "f", digits = 0)))
