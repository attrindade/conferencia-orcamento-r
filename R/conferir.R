# Conferência dos resultados em R contra os números-gabarito (config/gabaritos.csv)
# e checagens estruturais que não têm um número externo para comparar.
#
# Princípio: divergência é resultado. Nada aqui ajusta valor para "passar".
# Uma linha fora da tolerância só vira "explicada" quando a causa está
# documentada em uma fonte conferida (config/causas_conhecidas.yml, lido por
# ler_causas_conhecidas()) e o valor obtido é o que a causa descreve; o resto
# é "diverge" e fica pendente de decisão de quem responde pela conferência
# (relatorio: responsavel_decisao em config.yml).
#
# Nada específico de um projeto fica escrito aqui: códigos de ação, causas e
# valores vêm da configuração e dos gabaritos, para que o mesmo código rode
# sobre os dados reais e sobre os dados sintéticos da vitrine.

METRICAS <- c(
  "total_ano", "total_periodo", "media_anual", "total_gnd", "total_acao_ano",
  "taxa_execucao_periodo", "participacao_ano", "denominador_executivo_ano",
  "n_programas", "n_acoes", "n_programas_limiar", "ranking_dotacao",
  "ano_maximo_participacao"
)
# "fator" é a unidade dos fatores de correção (tolerância fixa, TOLERANCIA_FATOR).
UNIDADES <- c("reais", "pct", "contagem", "lista", "fator")
COLUNAS_GABARITOS <- c(
  "id", "descricao", "fonte", "metrica", "fase", "ano", "filtro",
  "unidade", "valor_esperado", "observacao"
)
COLUNAS_DIVERGENCIAS <- c(
  "id", "descricao", "fonte", "unidade", "valor_r", "valor_gabarito",
  "diferenca", "tolerancia", "dentro_tolerancia", "status",
  "provavel_causa", "encaminhamento"
)

# ---------------------------------------------------------------- gabaritos

ler_gabaritos <- function(caminho) {
  if (!file.exists(caminho)) {
    cli::cli_abort(c(
      "Arquivo de gabaritos não encontrado: {.file {caminho}}.",
      "i" = "Confira {.field caminhos: gabaritos} em config/config.yml."
    ))
  }
  # tudo como texto: a coluna valor_esperado mistura números e listas
  # ("2011|2010|2009"), e adivinhar o tipo estragaria uma ou outra
  gab <- readr::read_csv(
    caminho,
    col_types = readr::cols(.default = readr::col_character()),
    na = "", locale = readr::locale(encoding = "UTF-8"), progress = FALSE
  )
  faltam <- setdiff(COLUNAS_GABARITOS, names(gab))
  if (length(faltam) > 0) {
    cli::cli_abort(c(
      "O arquivo de gabaritos não tem as colunas esperadas.",
      "x" = "Faltam: {.val {faltam}}.",
      "i" = "Arquivo: {.file {caminho}}. Formato na seção 7 do CONTRATO.md."
    ))
  }
  repetidos <- unique(gab$id[duplicated(gab$id)])
  if (length(repetidos) > 0) {
    cli::cli_abort("Há {.field id} repetido em {.file {caminho}}: {.val {repetidos}}.")
  }
  metrica_invalida <- setdiff(gab$metrica, METRICAS)
  if (length(metrica_invalida) > 0) {
    cli::cli_abort("Métrica desconhecida em {.file {caminho}}: {.val {metrica_invalida}}. Válidas: {.val {METRICAS}}.")
  }
  unidade_invalida <- setdiff(gab$unidade, UNIDADES)
  if (length(unidade_invalida) > 0) {
    cli::cli_abort("Unidade desconhecida em {.file {caminho}}: {.val {unidade_invalida}}. Válidas: {.val {UNIDADES}}.")
  }
  numericos <- gab$unidade != "lista"
  convertidos <- suppressWarnings(as.numeric(gab$valor_esperado[numericos]))
  ilegiveis <- gab$id[numericos][is.na(convertidos)]
  if (length(ilegiveis) > 0) {
    cli::cli_abort(c(
      "Valor esperado que não é número em {.file {caminho}}: {.val {ilegiveis}}.",
      "i" = "Use ponto como separador decimal e nenhum separador de milhar."
    ))
  }
  gab |>
    dplyr::mutate(ano = as.integer(.data$ano)) |>
    dplyr::select(dplyr::all_of(COLUNAS_GABARITOS))
}

# Lê o campo `filtro` dos gabaritos. Gramática (definida pelo agente C):
# pares chave=valor separados por ";"; vários valores em uma chave separados
# por "|". Exemplos: "gnd_cod=3|4", "acao_cod=ZA01", "valor=nominal".
# Vazio = sem filtro.
ler_filtro <- function(filtro) {
  if (is.null(filtro) || length(filtro) == 0 || is.na(filtro) || !nzchar(trimws(filtro))) {
    return(list())
  }
  pares <- strsplit(trimws(filtro), ";", fixed = TRUE)[[1]]
  saida <- list()
  for (par in pares) {
    partes <- strsplit(par, "=", fixed = TRUE)[[1]]
    if (length(partes) != 2) {
      cli::cli_abort("Filtro mal escrito: {.val {filtro}}. Use {.code chave=valor} ou {.code chave=a|b}.")
    }
    saida[[trimws(partes[1])]] <- trimws(strsplit(partes[2], "|", fixed = TRUE)[[1]])
  }
  saida
}

# Valores reais por padrão; "valor=nominal" no filtro pede o nominal.
coluna_valor <- function(filtro) {
  if (identical(filtro$valor, "nominal")) "nominal" else "real"
}

exigir_resultado <- function(resultados, nome, id) {
  if (is.null(resultados[[nome]])) {
    cli::cli_abort(c(
      "O gabarito {.val {id}} precisa do resultado {.field {nome}}, que não veio na lista {.arg resultados}.",
      "i" = "Confira a montagem do alvo {.field divergencias} em _targets.R."
    ))
  }
  resultados[[nome]]
}

# Um único valor, ou NA quando o filtro não acha nada. NA vira "diverge" em
# conferir_gabaritos() com a causa "o R não produziu valor".
um_valor <- function(x) {
  if (length(x) == 0) return(NA_real_)
  if (length(x) > 1) cli::cli_abort("A métrica devolveu {length(x)} valores; esperava um. Revise o filtro do gabarito.")
  as.numeric(x)
}

calcular_metrica <- function(gabarito_linha, resultados) {
  g <- as.list(gabarito_linha)
  filtro <- ler_filtro(g$filtro)
  col <- coluna_valor(filtro)
  ano_g <- g$ano
  fase_g <- g$fase

  switch(g$metrica,
    total_ano = {
      t <- exigir_resultado(resultados, "totais_ano", g$id)
      um_valor(t[[col]][t$ano == ano_g & t$fase == fase_g])
    },
    total_periodo = {
      t <- exigir_resultado(resultados, "totais_periodo", g$id)
      um_valor(t[[col]][t$fase == fase_g])
    },
    media_anual = {
      t <- exigir_resultado(resultados, "totais_periodo", g$id)
      um_valor(t$media_anual_real[t$fase == fase_g])
    },
    total_gnd = {
      t <- exigir_resultado(resultados, "totais_gnd", g$id)
      sel <- t$fase == fase_g & t$gnd_cod %in% filtro$gnd_cod
      if (!any(sel)) return(NA_real_)
      if (g$unidade == "pct") sum(t$pct_real[sel]) else sum(t$real[sel])
    },
    total_acao_ano = {
      t <- exigir_resultado(resultados, "totais_acao_ano", g$id)
      # ano vazio = período inteiro
      no_ano <- if (is.na(ano_g)) rep(TRUE, nrow(t)) else t$ano == ano_g
      sel <- t$fase == fase_g & t$acao_cod %in% filtro$acao_cod & no_ano
      if (!any(sel)) return(NA_real_)
      valor <- sum(t[[col]][sel])
      if (g$unidade != "pct") return(valor)
      # percentual sobre o total da mesma fase no mesmo ano (ou no período)
      ta <- exigir_resultado(resultados, "totais_ano", g$id)
      no_ano_ta <- if (is.na(ano_g)) rep(TRUE, nrow(ta)) else ta$ano == ano_g
      100 * valor / sum(ta[[col]][ta$fase == fase_g & no_ano_ta])
    },
    taxa_execucao_periodo = {
      t <- exigir_resultado(resultados, "taxa_execucao", g$id)
      um_valor(t$taxa_pct[is.na(t$ano)])
    },
    participacao_ano = {
      t <- exigir_resultado(resultados, "participacao", g$id)
      um_valor(t$participacao_pct[t$ano == ano_g])
    },
    denominador_executivo_ano = {
      t <- exigir_resultado(resultados, "denominador", g$id)
      um_valor(t$executivo_nominal[t$ano == ano_g & t$fase == fase_g])
    },
    n_programas = um_valor(exigir_resultado(resultados, "contagens", g$id)$n_programas),
    n_acoes = um_valor(exigir_resultado(resultados, "contagens", g$id)$n_acoes),
    n_programas_limiar = {
      conc <- exigir_resultado(resultados, "concentracao", g$id)
      if (identical(fase_g, "pago")) {
        um_valor(conc$n_ate_limiar_pago)
      } else if (is.null(conc$fase) || is.na(fase_g) || identical(fase_g, conc$fase)) {
        um_valor(conc$n_ate_limiar)
      } else {
        cli::cli_abort("O gabarito {.val {g$id}} pede a concentração por {.val {fase_g}}, mas ela foi calculada por {.val {conc$fase}} (config.yml).")
      }
    },
    ranking_dotacao = {
      t <- exigir_resultado(resultados, "totais_ano", g$id)
      n <- length(strsplit(g$valor_esperado, "|", fixed = TRUE)[[1]])
      fase_r <- if (is.na(fase_g)) "dotacao_atual" else fase_g
      paste(anos_em_ordem_de_valor(t, fase = fase_r, n = n), collapse = "|")
    },
    ano_maximo_participacao = {
      # O ano de maior participação, como texto: é uma lista de um item só e
      # passa pela mesma comparação exata do ranking. Um valor dentro da
      # tolerância não garante que o ano citado como máximo seja o máximo.
      t <- exigir_resultado(resultados, "participacao", g$id)
      if (nrow(t) == 0 || all(is.na(t$participacao_pct))) return(NA_character_)
      as.character(t$ano[which.max(t$participacao_pct)])
    },
    cli::cli_abort("Métrica sem cálculo definido: {.val {g$metrica}}.")
  )
}

tolerancia_da_unidade <- function(unidade, cfg) {
  tol <- cfg$tolerancias
  vapply(unidade, function(u) {
    switch(u,
      reais = as.numeric(tol$reais),
      pct = as.numeric(tol$pontos_percentuais),
      contagem = as.numeric(tol$contagem),
      lista = 0,
      fator = TOLERANCIA_FATOR,
      NA_real_
    )
  }, numeric(1), USE.NAMES = FALSE)
}

# Folga numérica mínima para a comparação com a tolerância: uma diferença
# igual à tolerância (por exemplo 0,35 - 0,30 = 0,05) pode sair em ponto
# flutuante como 0,0500000000001. Não muda nenhum veredito.
FOLGA_PONTO_FLUTUANTE <- 1e-9

# Causas conhecidas e verificadas ANTES de rodar, em uma fonte conferida, lidas
# de config/causas_conhecidas.yml (caminhos: causas_conhecidas). Só são usadas
# se o número cair fora da tolerância; dentro dela, o status é "confere" de
# qualquer jeito. Sem arquivo configurado, nenhuma causa é conhecida: tudo o
# que cair fora da tolerância fica "diverge" (mais rígido, nunca mais frouxo).
#
# Campos de cada causa:
#   provavel_causa, encaminhamento  obrigatórios
#   valor_r_esperado  opcional: o valor que a causa descreve (número, ou o
#                     texto da lista, como "2019"); outro valor não é explicado
#   max_diferenca     opcional: maior diferença, em valor absoluto, que a causa
#                     cobre (na unidade da linha)
#   anos, acoes       opcionais: anos e códigos de ação em que a diferença pode
#                     aparecer (checagem das duas safras do recorte)
#   nota, junto_de    opcionais: nota para o relatório e o id da linha ao lado
#                     da qual ela aparece (para um número que confere, mas
#                     merece ressalva)
ler_causas_conhecidas <- function(caminho) {
  vazia <- normalizar_causas(tibble::tibble(id = character(), provavel_causa = character(),
                                            encaminhamento = character()))
  if (is.null(caminho) || length(caminho) == 0 || is.na(caminho) || !nzchar(caminho)) {
    cli::cli_inform("Sem arquivo de causas conhecidas configurado: nenhuma divergência será dada como explicada.")
    return(vazia)
  }
  if (!file.exists(caminho)) {
    cli::cli_abort(c(
      "Arquivo de causas conhecidas não encontrado: {.file {caminho}}.",
      "i" = "Confira {.field caminhos: causas_conhecidas} em config/config.yml."
    ))
  }
  lista <- yaml::read_yaml(caminho)
  if (length(lista) == 0) return(vazia)
  sem_texto <- names(lista)[!vapply(lista, function(x) {
    is.list(x) && is.character(x$provavel_causa) && nzchar(x$provavel_causa) &&
      is.character(x$encaminhamento) && nzchar(x$encaminhamento)
  }, logical(1))]
  if (length(sem_texto) > 0) {
    cli::cli_abort(c(
      "Causa sem {.field provavel_causa} ou sem {.field encaminhamento} em {.file {caminho}}: {.val {sem_texto}}.",
      "i" = "Uma divergência só vira explicada com a causa e o que fazer escritos."
    ))
  }
  # Campo opcional de texto: NA quando ausente.
  campo_texto <- function(x, nome) {
    v <- x[[nome]]
    if (is.null(v) || length(v) == 0) NA_character_ else paste(as.character(unlist(v)), collapse = "|")
  }
  campo_numero <- function(x, nome) {
    v <- x[[nome]]
    if (is.null(v) || length(v) == 0) return(NA_real_)
    n <- suppressWarnings(as.numeric(v[[1]]))
    if (is.na(n)) {
      cli::cli_abort("Campo {.field {nome}} que não é número em {.file {caminho}}: {.val {v[[1]]}}.")
    }
    n
  }
  tibble::tibble(
    id = names(lista),
    provavel_causa = vapply(lista, function(x) x$provavel_causa, character(1), USE.NAMES = FALSE),
    encaminhamento = vapply(lista, function(x) x$encaminhamento, character(1), USE.NAMES = FALSE),
    anos = unname(lapply(lista, function(x) as.integer(unlist(x$anos)))),
    acoes = unname(lapply(lista, function(x) as.character(unlist(x$acoes)))),
    valor_r_esperado = vapply(lista, campo_texto, character(1), nome = "valor_r_esperado", USE.NAMES = FALSE),
    max_diferenca = vapply(lista, campo_numero, numeric(1), nome = "max_diferenca", USE.NAMES = FALSE),
    nota = vapply(lista, campo_texto, character(1), nome = "nota", USE.NAMES = FALSE),
    junto_de = vapply(lista, campo_texto, character(1), nome = "junto_de", USE.NAMES = FALSE)
  )
}

# Completa a tabela de causas com as colunas opcionais que faltarem (por
# exemplo, em uma tabela montada à mão em um teste), para o resto do código não
# precisar perguntar se cada coluna existe.
normalizar_causas <- function(causas) {
  n <- nrow(causas)
  falta <- function(col) !col %in% names(causas)
  if (falta("anos")) causas$anos <- rep(list(integer()), n)
  if (falta("acoes")) causas$acoes <- rep(list(character()), n)
  for (col in c("valor_r_esperado", "nota", "junto_de")) {
    if (falta(col)) causas[[col]] <- rep(NA_character_, n)
  }
  if (falta("max_diferenca")) causas$max_diferenca <- rep(NA_real_, n)
  causas
}

# A causa registrada cobre o resultado desta rodada? Confere cada campo de
# verificação que a causa tiver (valor_r_esperado, max_diferenca, anos,
# acoes). Sem nenhum desses campos, a causa vale para qualquer valor, como
# antes; com eles, uma mudança nos dados que a causa não descreve deixa de ser
# explicada. Devolve lista(cobre, motivo).
avaliar_causa <- function(conhecida, unidade, valor_r, obtido_texto, diferenca, tolerancia,
                          anos_encontrados = integer(), acoes_encontradas = character()) {
  motivos <- character()
  esperado <- conhecida$valor_r_esperado
  if (!is.na(esperado)) {
    if (identical(unidade, "lista")) {
      if (!identical(as.character(obtido_texto), esperado)) {
        motivos <- c(motivos, sprintf("a causa descreve %s e o R obteve %s",
                                      gsub("|", ", ", esperado, fixed = TRUE),
                                      gsub("|", ", ", as.character(obtido_texto), fixed = TRUE)))
      }
    } else {
      esperado_n <- suppressWarnings(as.numeric(esperado))
      if (is.na(valor_r) || is.na(esperado_n) ||
          abs(valor_r - esperado_n) > tolerancia + FOLGA_PONTO_FLUTUANTE) {
        motivos <- c(motivos, sprintf("a causa descreve o valor %s e o R obteve %s",
                                      format(esperado_n, scientific = FALSE, big.mark = ".", decimal.mark = ","),
                                      format(valor_r, scientific = FALSE, big.mark = ".", decimal.mark = ",")))
      }
    }
  }
  if (!is.na(conhecida$max_diferenca)) {
    if (is.na(diferenca) || abs(diferenca) > conhecida$max_diferenca + FOLGA_PONTO_FLUTUANTE) {
      motivos <- c(motivos, sprintf("a causa cobre diferença de até %s e a diferença foi %s",
                                    format(conhecida$max_diferenca, scientific = FALSE, big.mark = ".", decimal.mark = ","),
                                    format(abs(diferenca), scientific = FALSE, big.mark = ".", decimal.mark = ",")))
    }
  }
  anos_ok <- conhecida$anos[[1]]
  if (length(anos_ok) > 0) {
    fora <- setdiff(anos_encontrados, anos_ok)
    if (length(fora) > 0) motivos <- c(motivos, paste0("há diferença em anos que a causa não cita (", juntar_com_e(sort(fora)), ")"))
  }
  acoes_ok <- conhecida$acoes[[1]]
  if (length(acoes_ok) > 0) {
    fora <- setdiff(acoes_encontradas, acoes_ok)
    if (length(fora) > 0) motivos <- c(motivos, paste0("há diferença em ações que a causa não cita (", juntar_com_e(sort(fora)), ")"))
  }
  list(cobre = length(motivos) == 0, motivo = paste(motivos, collapse = "; "))
}

# Aplica a causa registrada a uma linha fora da tolerância: "explicada" se a
# causa cobre o valor desta rodada; senão "diverge", com a nota de que a causa
# registrada não cobre o valor atual (e a causa antiga, para comparação).
aplicar_causa <- function(lin, conhecida, avaliacao, cfg) {
  if (avaliacao$cobre) {
    lin$status <- "explicada"
    lin$provavel_causa <- conhecida$provavel_causa
    lin$encaminhamento <- conhecida$encaminhamento
  } else {
    lin$status <- "diverge"
    lin$provavel_causa <- paste0("Causa registrada não cobre o valor atual: ", avaliacao$motivo,
                                 ". Causa registrada antes: ", conhecida$provavel_causa)
    lin$encaminhamento <- paste0("Pendente de decisão (", responsavel_decisao(cfg),
                                 "): ver o que mudou nos dados e rever a causa em config/causas_conhecidas.yml.")
  }
  lin
}

linha_divergencia <- function(id, descricao, fonte, unidade, valor_r, valor_gabarito,
                              tolerancia, status = NULL, provavel_causa = NA_character_,
                              encaminhamento = NA_character_, diferenca = NULL) {
  if (is.null(diferenca)) diferenca <- valor_r - valor_gabarito
  dentro <- !is.na(diferenca) && abs(diferenca) <= tolerancia + FOLGA_PONTO_FLUTUANTE
  if (is.null(status)) status <- if (dentro) "confere" else "diverge"
  tibble::tibble(
    id = id, descricao = descricao, fonte = fonte, unidade = unidade,
    valor_r = as.numeric(valor_r), valor_gabarito = as.numeric(valor_gabarito),
    diferenca = as.numeric(diferenca), tolerancia = as.numeric(tolerancia),
    dentro_tolerancia = dentro, status = status,
    provavel_causa = provavel_causa, encaminhamento = encaminhamento
  )
}

conferir_gabaritos <- function(gabaritos, resultados, cfg,
                               causas = ler_causas_conhecidas(cfg$caminhos$causas_conhecidas)) {
  causas <- normalizar_causas(causas)
  linhas <- purrr::map(seq_len(nrow(gabaritos)), function(i) {
    g <- gabaritos[i, ]
    obtido <- calcular_metrica(g, resultados)
    tol <- tolerancia_da_unidade(g$unidade, cfg)
    descricao <- g$descricao
    if (g$unidade == "lista") {
      # lista não cabe nas colunas numéricas: vai por extenso na descrição e
      # a diferença conta quantas posições não batem
      esperado_v <- strsplit(g$valor_esperado, "|", fixed = TRUE)[[1]]
      obtido_v <- strsplit(obtido, "|", fixed = TRUE)[[1]]
      n_dif <- sum(esperado_v != obtido_v[seq_along(esperado_v)], na.rm = FALSE)
      n_dif <- ifelse(is.na(n_dif), length(esperado_v), n_dif)
      descricao <- paste0(descricao, " (R: ", gsub("|", ", ", obtido, fixed = TRUE),
                          "; gabarito: ", gsub("|", ", ", g$valor_esperado, fixed = TRUE), ")")
      lin <- linha_divergencia(g$id, descricao, g$fonte, g$unidade, NA_real_, NA_real_,
                               tol, diferenca = n_dif)
    } else {
      lin <- linha_divergencia(g$id, descricao, g$fonte, g$unidade, obtido,
                               as.numeric(g$valor_esperado), tol)
    }
    if (is.na(lin$diferenca)) {
      lin$provavel_causa <- "O R não produziu valor para esta métrica (filtro, ano ou fase sem linhas)."
      lin$encaminhamento <- "Conferir o filtro do gabarito e se o dado existe na base."
    } else if (lin$status == "diverge") {
      conhecida <- causas[causas$id == g$id, ]
      if (nrow(conhecida) == 1) {
        avaliacao <- avaliar_causa(conhecida, g$unidade, lin$valor_r, obtido, lin$diferenca, tol)
        lin <- aplicar_causa(lin, conhecida, avaliacao, cfg)
      } else {
        lin$provavel_causa <- "Sem causa verificada (o id não está em config/causas_conhecidas.yml)."
        lin$encaminhamento <- paste0("Pendente de decisão (", responsavel_decisao(cfg),
                                     "): conferir a fonte do gabarito e o cálculo em R.")
      }
    }
    lin
  })
  saida <- dplyr::bind_rows(linhas)
  if (nrow(saida) == 0) return(divergencias_vazia())
  saida
}

divergencias_vazia <- function() {
  tibble::tibble(
    id = character(), descricao = character(), fonte = character(), unidade = character(),
    valor_r = double(), valor_gabarito = double(), diferenca = double(), tolerancia = double(),
    dentro_tolerancia = logical(), status = character(),
    provavel_causa = character(), encaminhamento = character()
  )
}

# ---------------------------------------------------------------- checagens estruturais

checar_uniao_identica <- function(comparacao, cfg) {
  ultimo <- cfg$periodo$ultimo_ano_safra_antiga
  c_ <- comparacao[comparacao$ano <= ultimo, ]
  maior <- max(abs(c_$diferenca), na.rm = FALSE)
  anos_dif <- sort(unique(c_$ano[!c_$identico]))
  depois <- sort(unique(comparacao$ano[comparacao$ano > ultimo]))
  lin <- linha_divergencia(
    "estrutura_uniao_safras_identicas",
    paste0("Orçamento da União inteira, safra antiga (base consolidada) × safra nova, ",
           cfg$periodo$ano_inicio, " a ", ultimo, ", todas as fases: maior diferença em reais",
           if (length(anos_dif) > 0) paste0(" (anos diferentes: ", paste(anos_dif, collapse = ", "), ")") else ""),
    paste0("Checagem interna: as duas safras da União, comparadas na seção Duas safras",
           if (length(depois) > 0) paste0("; ", paste(depois, collapse = ", "),
                                          " fica de fora porque está incompleto na safra antiga") else ""),
    "reais", maior, 0, cfg$tolerancias$reais
  )
  if (lin$status == "diverge") {
    lin$provavel_causa <- "As duas safras deveriam ser idênticas até o último ano completo da antiga (periodo: ultimo_ano_safra_antiga em config.yml)."
    lin$encaminhamento <- paste0("Pendente de decisão (", responsavel_decisao(cfg), "): o denominador de ",
                                 cfg$periodo$ano_inicio, " a ", ultimo, " depende dessa igualdade.")
  }
  lin
}

# A diferença entre as safras do recorte só vira "explicada" se o registro
# "estrutura_juventude_safras" de config/causas_conhecidas.yml cobrir o que foi
# medido nesta rodada: os anos diferentes (anos), as ações de onde vêm as
# diferenças (acoes) e o tamanho da maior diferença (max_diferenca).
# `diferencas` é a tabela por ano e ação de detalhar_diferencas_juventude();
# sem ela, as ações não são conferidas.
checar_safras_juventude <- function(comparacao, cfg, causas = ler_causas_conhecidas(cfg$caminhos$causas_conhecidas),
                                    diferencas = NULL) {
  causas <- normalizar_causas(causas)
  maior <- max(abs(comparacao$diferenca), na.rm = FALSE)
  anos_dif <- sort(unique(comparacao$ano[!comparacao$identico]))
  id <- "estrutura_juventude_safras"
  lin <- linha_divergencia(
    id,
    paste0("Recorte de juventude, safra antiga × safra nova, ",
           min(comparacao$ano), " a ", max(comparacao$ano), ": maior diferença em reais",
           if (length(anos_dif) > 0) paste0(" (anos diferentes: ", paste(anos_dif, collapse = ", "), ")") else ""),
    "Checagem interna: as duas safras do recorte de juventude, comparadas na seção Duas safras",
    "reais", maior, 0, cfg$tolerancias$reais
  )
  if (lin$status == "diverge") {
    conhecida <- causas[causas$id == id, ]
    if (nrow(conhecida) == 1) {
      acoes_dif <- if (is.null(diferencas)) character() else sort(unique(as.character(diferencas$acao_cod)))
      # Sem a lista de anos no registro, nenhum ano diferente está explicado.
      if (length(conhecida$anos[[1]]) == 0) conhecida$anos[[1]] <- NA_integer_
      avaliacao <- avaliar_causa(conhecida, "reais", lin$valor_r, NA_character_, lin$diferenca,
                                 cfg$tolerancias$reais, anos_encontrados = anos_dif,
                                 acoes_encontradas = acoes_dif)
      lin <- aplicar_causa(lin, conhecida, avaliacao, cfg)
      if (!avaliacao$cobre) {
        lin$encaminhamento <- paste0("Pendente de decisão (", responsavel_decisao(cfg),
                                     "): ver a tabela de ações da seção Duas safras e rever a causa em config/causas_conhecidas.yml.")
      }
    } else {
      lin$provavel_causa <- "As safras diferem e não há causa registrada em config/causas_conhecidas.yml."
      lin$encaminhamento <- paste0("Pendente de decisão (", responsavel_decisao(cfg),
                                   "): ver a tabela de ações da seção Duas safras.")
    }
  }
  lin
}

checar_recorte_cobre_base <- function(resumo) {
  problemas <- resumo$linhas_removidas + resumo$acoes_da_base_fora_do_recorte
  lin <- linha_divergencia(
    "estrutura_recorte_cobre_base",
    paste0("O recorte de ações cobre toda a base de juventude (linhas removidas: ", resumo$linhas_removidas,
           "; ações da base fora do recorte: ", resumo$acoes_da_base_fora_do_recorte, ")"),
    "Checagem interna: lista de ações do recorte × base de juventude (seção O recorte em números)",
    "contagem", problemas, 0, 0
  )
  if (lin$status == "diverge") {
    lin$provavel_causa <- "Há linhas ou ações da base que o arquivo de recorte não lista."
    lin$encaminhamento <- "Regerar entradas/recorte_acoes.csv com scripts/derivar_recorte.R e conferir as ações que faltam."
  }
  lin
}

checar_acoes_obrigatorias <- function(resumo, totais_acao_ano, acoes_obrigatorias) {
  na_base <- acoes_obrigatorias %in% totais_acao_ano$acao_cod
  ausentes_recorte <- if (is.na(resumo$obrigatorias_ausentes)) character() else
    strsplit(resumo$obrigatorias_ausentes, "|", fixed = TRUE)[[1]]
  presentes <- sum(na_base & !(acoes_obrigatorias %in% ausentes_recorte))
  lin <- linha_divergencia(
    "estrutura_acoes_obrigatorias",
    paste0("Ações obrigatórias presentes no recorte e na base (",
           if (length(acoes_obrigatorias) > 0) paste(acoes_obrigatorias, collapse = ", ") else "nenhuma configurada", ")"),
    "Checagem interna: ações obrigatórias da configuração × recorte e base (seção O recorte em números)",
    "contagem", presentes, length(acoes_obrigatorias), 0
  )
  if (lin$status == "diverge") {
    lin$provavel_causa <- "Uma inclusão manual sumiu do recorte ou da base."
    lin$encaminhamento <- "Conferir a lista de ações do recorte (caminhos: recorte_acoes) e a extração do SIOP."
  }
  lin
}

# Três caminhos independentes até o mesmo total por fase: a soma direta das
# linhas, a soma por ano, por GND e por ação. Se algum discordar, há linha
# perdida ou duplicada em uma agregação.
checar_somas_por_fase <- function(resultados, cfg) {
  per <- resultados$totais_periodo
  difs <- c()
  if (!is.null(resultados$base_real)) {
    b <- resultados$base_real
    direto_real <- vapply(FASES, function(f) sum(b[[paste0(f, "_real")]], na.rm = FALSE), numeric(1))
    direto_nom <- vapply(FASES, function(f) sum(b[[f]], na.rm = FALSE), numeric(1))
    difs <- c(difs, direto_real - per$real[match(FASES, per$fase)],
              direto_nom - per$nominal[match(FASES, per$fase)])
  }
  gnd <- resultados$totais_gnd |>
    dplyr::group_by(.data$fase) |>
    dplyr::summarise(real = sum(.data$real, na.rm = FALSE), .groups = "drop")
  acao <- resultados$totais_acao_ano |>
    dplyr::group_by(.data$fase) |>
    dplyr::summarise(real = sum(.data$real, na.rm = FALSE), nominal = sum(.data$nominal, na.rm = FALSE),
                     .groups = "drop")
  difs <- c(difs,
            gnd$real[match(FASES, gnd$fase)] - per$real[match(FASES, per$fase)],
            acao$real[match(FASES, acao$fase)] - per$real[match(FASES, per$fase)],
            acao$nominal[match(FASES, acao$fase)] - per$nominal[match(FASES, per$fase)])
  maior <- max(abs(difs), na.rm = FALSE)
  lin <- linha_divergencia(
    "estrutura_soma_por_fase",
    paste0("Total de cada fase igual por quatro caminhos (soma das linhas",
           if (is.null(resultados$base_real)) " [não disponível nesta execução]" else "",
           ", por ano, por GND e por ação): maior diferença em reais"),
    "Checagem interna: somas das linhas, por ano, por grupo de despesa e por ação (seção A série)",
    "reais", maior, 0, cfg$tolerancias$reais
  )
  if (lin$status == "diverge") {
    lin$provavel_causa <- "Uma das agregações perdeu ou duplicou linhas, ou há NA em algum valor."
    lin$encaminhamento <- "Procurar NA nas colunas de valor da base e revisar R/agregar.R."
  }
  lin
}

# Denominador em R × cache da implementação de referência em Python (todas as
# fases, do primeiro ano até o último ano completo da safra antiga). Sem o
# cache configurado (caso da vitrine) a checagem é pulada, com aviso.
#
# O caminho chega como argumento: no pipeline ele vem do alvo de arquivo
# arq_executivo_python, para que uma mudança no cache refaça esta checagem.
# Vazio (character(0) ou "") quer dizer que o cache não está disponível.
checar_denominador_python <- function(denominador, cfg, caminho = cfg$caminhos$executivo_python) {
  if (is.null(caminho) || length(caminho) == 0 || is.na(caminho[1]) || !nzchar(caminho[1])) {
    cli::cli_alert_warning("Sem cache do denominador da implementação de referência ({.field caminhos: executivo_python} vazio): checagem pulada.")
    return(NULL)
  }
  if (!file.exists(caminho)) {
    cli::cli_alert_warning("O cache do denominador não existe em {.path {caminho}}: checagem pulada.")
    return(NULL)
  }
  cache <- readr::read_csv(caminho, col_types = readr::cols(.default = readr::col_double()),
                           progress = FALSE) |>
    dplyr::select("ano", dplyr::starts_with("executivo_")) |>
    tidyr::pivot_longer(-"ano", names_to = "fase", values_to = "python",
                        names_prefix = "executivo_") |>
    dplyr::mutate(ano = as.integer(.data$ano))
  ultimo <- cfg$periodo$ultimo_ano_safra_antiga
  comp <- denominador |>
    dplyr::filter(.data$ano <= ultimo) |>
    dplyr::inner_join(cache, by = c("ano", "fase"))
  n_esperado <- (ultimo - cfg$periodo$ano_inicio + 1) * length(FASES)
  maior <- if (nrow(comp) == n_esperado) max(abs(comp$executivo_nominal - comp$python), na.rm = FALSE) else NA_real_
  gnd <- as.character(unlist(cfg$denominador$gnd_incluidos))
  lin <- linha_divergencia(
    "estrutura_denominador_python",
    paste0("Gasto do Executivo (GND ", juntar_com_e(gnd), ") calculado em R × valor guardado pela versão em Python, ",
           cfg$periodo$ano_inicio, " a ", ultimo, ", seis fases, valores nominais: maior diferença em reais (",
           nrow(comp), " de ", n_esperado, " pares comparados)"),
    # O nome do arquivo fica na fonte: é por ele que o relatório põe a linha
    # na família do gasto do Executivo (relatorio: familias, fonte_regex).
    paste0("Checagem interna: gasto do Executivo calculado aqui × arquivo da versão em Python (", basename(caminho), ")"),
    "reais", maior, 0, cfg$tolerancias$reais
  )
  if (lin$status == "diverge") {
    lin$provavel_causa <- if (is.na(maior)) "Faltam anos ou fases na comparação." else "Regra de órgãos ou GND diferente entre R e Python."
    lin$encaminhamento <- "Comparar ano a ano o gasto do Executivo da seção Participação com o arquivo da versão em Python; a regra está em config.yml (denominador)."
  }
  lin
}

# Registra o fator de correção usado. Esta linha não tem como divergir dentro
# do pipeline: deflacionar() para a conferência antes, com erro, se o fator
# for outro (conferir_fator_ipca). Ela fica no relatório como registro do
# fator conferido, e a descrição diz isso.
checar_fator_ipca <- function(ipca, cfg) {
  ano_c <- cfg$ipca$ano_conferido
  fator <- ipca$fator[ipca$ano == ano_c]
  fator <- if (length(fator) == 1) fator else NA_real_
  arquivo <- if (!is.null(cfg$caminhos$ipca) && nzchar(cfg$caminhos$ipca)) basename(cfg$caminhos$ipca) else "arquivo do IPCA"
  lin <- linha_divergencia(
    paste0("estrutura_fator_ipca_", ano_c),
    paste0("Fator de correção do IPCA de ", ano_c, " usado na deflação (o errado conhecido é ",
           numero_virgula(cfg$ipca$fator_errado_conhecido, 7),
           "). É um registro: com outro fator, a conferência para antes desta etapa"),
    paste0("Checagem interna: fator esperado na configuração × ", arquivo),
    "fator", fator, cfg$ipca$fator_esperado, TOLERANCIA_FATOR
  )
  if (lin$status == "diverge") {
    lin$provavel_causa <- "O arquivo do IPCA não traz o fator esperado para o ano conferido."
    lin$encaminhamento <- "Usar uma extração do IPCA com o fator corrigido e conferir a seção ipca: de config/config.yml."
  }
  lin
}

checagens_estruturais <- function(resultados, cfg,
                                  acoes_obrigatorias = as.character(unlist(cfg$recorte$acoes_obrigatorias)),
                                  causas = ler_causas_conhecidas(cfg$caminhos$causas_conhecidas)) {
  linhas <- list()
  if (!is.null(resultados$comparacao_safras_uniao)) {
    linhas$uniao <- checar_uniao_identica(resultados$comparacao_safras_uniao, cfg)
  }
  if (!is.null(resultados$comparacao_safras_juventude)) {
    linhas$juventude <- checar_safras_juventude(resultados$comparacao_safras_juventude, cfg, causas,
                                                diferencas = resultados$diferencas_juventude)
  }
  if (!is.null(resultados$resumo_recorte)) {
    linhas$recorte <- checar_recorte_cobre_base(resultados$resumo_recorte)
    linhas$obrigatorias <- checar_acoes_obrigatorias(resultados$resumo_recorte, resultados$totais_acao_ano,
                                                     acoes_obrigatorias)
  }
  linhas$somas <- checar_somas_por_fase(resultados, cfg)
  if (!is.null(resultados$denominador)) {
    # No pipeline, o caminho vem do alvo de arquivo (resultados$executivo_python,
    # que pode ser character(0)); fora dele, da configuração.
    caminho_python <- if ("executivo_python" %in% names(resultados)) resultados$executivo_python else cfg$caminhos$executivo_python
    linhas$denominador <- checar_denominador_python(resultados$denominador, cfg, caminho_python)
  }
  # o fator vem do alvo ipca; sem ele, do arquivo apontado no config
  ipca <- resultados$ipca
  if (is.null(ipca) && !is.null(cfg$caminhos$ipca) && file.exists(cfg$caminhos$ipca)) {
    ipca <- readr::read_csv(cfg$caminhos$ipca, col_types = "id", progress = FALSE,
                            col_names = c("ano", "fator"), skip = 1)
  }
  if (!is.null(ipca)) linhas$ipca <- checar_fator_ipca(ipca, cfg)
  ausentes <- setdiff(c("comparacao_safras_uniao", "comparacao_safras_juventude", "resumo_recorte",
                        "denominador"), names(resultados))
  if (length(ausentes) > 0) {
    cli::cli_warn("Checagens estruturais sem os resultados {.val {ausentes}}: essas linhas ficam de fora.")
  }
  dplyr::bind_rows(linhas)
}

# ---------------------------------------------------------------- veredito e saídas

veredito <- function(divergencias) {
  total <- nrow(divergencias)
  conferidos <- sum(divergencias$status == "confere")
  n_diverge <- sum(divergencias$status == "diverge")
  explicadas <- sum(divergencias$status == "explicada")
  # A frase diz primeiro quantos números foram comparados, para "213 de 216"
  # não ser lido como "só 213 foram checados".
  n_txt <- function(n) format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  plural <- function(n, nenhum, um, varios) {
    if (n == 0) nenhum else if (n == 1) paste("1", um) else paste(n_txt(n), varios)
  }
  texto <- paste0(
    plural(total, "nenhum número comparado", "número comparado", "números comparados"), ": ",
    plural(conferidos, "nenhum confere", "confere", "conferem"), ", ",
    plural(explicadas, "nenhum tem diferença explicada", "tem diferença explicada", "têm diferença explicada"), " e ",
    plural(n_diverge, "nenhum ficou sem explicação", "ficou sem explicação", "ficaram sem explicação")
  )
  list(total = total, conferidos = conferidos, divergencias = n_diverge,
       explicadas = explicadas, texto = texto)
}

# Números como texto, sem notação científica (7.78e-4 vira 0.000778...) e com
# ponto decimal, para a planilha abrir igual em qualquer programa. Quinze
# algarismos significativos, a precisão de um double; NA vira vazio.
numero_sem_expoente <- function(x) {
  out <- vapply(x, function(v) {
    if (is.na(v)) return("")
    format(v, scientific = FALSE, digits = 15, trim = TRUE, drop0trailing = TRUE)
  }, character(1), USE.NAMES = FALSE)
  out
}

gravar_saidas <- function(divergencias, procedencia, cfg) {
  pasta <- cfg$caminhos$saidas
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  arq_div <- file.path(pasta, "divergencias.csv")
  arq_proc <- file.path(pasta, "procedencia.csv")
  # readr grava UTF-8, separador vírgula e ponto decimal. As colunas de número
  # vão antes para texto sem notação científica: sozinho, o readr gravaria
  # diferenças pequenas como 7.78e-4, o que confunde quem abre a planilha.
  div_csv <- dplyr::mutate(divergencias, dplyr::across(dplyr::where(is.double), numero_sem_expoente))
  proc_csv <- dplyr::mutate(procedencia, dplyr::across(c("tamanho_bytes"), numero_sem_expoente))
  readr::write_csv(div_csv, arq_div, na = "")
  readr::write_csv(proc_csv, arq_proc, na = "")
  cli::cli_inform(c("v" = "Gravados {.file {arq_div}} e {.file {arq_proc}}."))
  c(arq_div, arq_proc)
}
