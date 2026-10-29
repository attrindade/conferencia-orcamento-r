# Conferência do orçamento em R (vitrine com dados sintéticos)

> **Summary (English).** This repository is the public showcase of an independent, reproducible check, written in R, of the federal youth budget figures published by Instituto Cíclica in the PPJ project (a review of 20 years of Brazilian federal youth policy). The check recomputes every published figure through a path independent from the one that produced it (reading two extractions of the official budget system, applying the youth selection as an input list, deflating by an inflation index with a guard against a known wrong factor, aggregating) and compares each result with the published value under tolerances fixed in advance. Because the real data and figures are private, the same code runs here on **synthetic data**: invented agencies, programmes, actions and values, written in exactly the same file formats as the real sources. An automated leak check, which reads the forbidden list from the private folder at run time, blocks any real code, name, keyword or figure from reaching this repository.

## O que é

Uma conferência independente, em R, dos números de orçamento federal de juventude publicados pelo Instituto Cíclica. Aqui ela roda sobre dados sintéticos, com o mesmo formato das fontes reais, para que o método possa ser lido, rodado e criticado por qualquer pessoa.

## Por que existe

O projeto PPJ, do Instituto Cíclica, analisou 20 anos da política federal de juventude no Brasil, e parte do trabalho foram números de orçamento: quanto foi previsto, autorizado, empenhado e pago em ações de juventude, ano a ano, corrigido pela inflação. Esses números passam por muitas etapas (extração do sistema oficial, filtro do que conta como juventude, deflação, somas) e um erro em qualquer uma delas muda o resultado sem deixar sinal. Um fator de inflação errado em um único ano, por exemplo, muda todos os valores daquele ano e passa despercebido se ninguém refizer a conta.

A conferência refaz as contas por outro caminho e compara com o que foi publicado, com tolerâncias decididas antes de olhar o resultado. Esta vitrine existe para mostrar o método em público sem expor os dados do projeto.

## O resultado em uma linha

**209 números comparados: 208 conferem, 1 tem diferença explicada e nenhum ficou sem explicação** (rodada do zero de `Rscript rodar.R --do-zero` em 29/10/2026, com 68 blocos de teste passando e 1 pulado: o que compara com a implementação em Python, que só existe na versão privada). A única diferença explicada é a das duas safras do recorte, criada de propósito pelo gerador. O relatório desta rodada está em `docs/relatorio.html`.

## Como rodar em três comandos

Pré-requisitos: R 4.6 e Quarto 1.10 ou mais novos. No terminal, a partir da pasta do repositório:

```sh
Rscript -e "renv::restore()"              # 1. instala as versões exatas dos pacotes
Rscript scripts/gerar_dados_sinteticos.R  # 2. (opcional) refaz os dados sintéticos; saem idênticos aos do repositório
Rscript rodar.R                           # 3. roda a conferência, os testes e gera o relatório
```

**No Windows**, o comando `Rscript` muitas vezes não está no PATH (a lista de pastas onde o terminal procura programas). Nesse caso, use o caminho completo do programa, com barras normais. Exemplos:

- instalação com scoop: `C:/Users/<seu usuário>/scoop/apps/r/current/bin/x64/Rscript.exe rodar.R`
- instalador oficial do R: `"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" rodar.R`

O caminho do Quarto fica em `config/config.yml` (chave `ferramentas: quarto:`). Se ele não existir na sua máquina, o `rodar.R` procura o Quarto no PATH e depois pelo pacote `{quarto}`; só é preciso ajustar o caminho se nenhum dos dois o encontrar.

## O que cada checagem faz

| Checagem | O que compara | Tolerância |
|---|---|---|
| Gabaritos de totais | Total do recorte por ano e por fase, total e média do período, por tipo de despesa (GND) e por ação, corrigidos pela inflação | R$ 1,00 por número |
| Gabaritos de percentuais | Taxa de execução (empenhado sobre dotação) e participação do recorte no gasto do Executivo, ano a ano | cinco centésimos de ponto |
| Denominador | Gasto empenhado do Executivo por ano, que é a base da participação | R$ 1,00 |
| Contagens e listas | Número de programas e de ações; quantos programas somam 80% da dotação; os quatro anos de maior dotação | exata |
| Ano de maior participação | Se o ano de maior participação no gasto do Executivo é o que o gabarito diz (um valor dentro da tolerância não garante que o ano citado como máximo seja o máximo) | exata |
| Duas safras da União | As duas extrações da base da União têm de ser idênticas nos anos completos | R$ 1,00 |
| Duas safras do recorte | Onde as extrações diferem, a diferença é listada por ação e vira divergência explicada | registro |
| Cobertura do recorte | A lista de ações cobre todas as linhas, e as ações obrigatórias estão presentes | exata |
| Soma por fase | A soma das linhas é igual à soma por fase (nada se perde nas agregações) | R$ 1,00 |
| Trava da inflação | O fator do ano conferido é o certo, e o fator errado conhecido é recusado | sete casas decimais |
| Testes automáticos | Cada função faz o que promete (leitura, conversão de números, formatação em reais) | todos passam |

### Checagem anti-vazamento

Antes de publicar ou de fazer *commit* (registrar uma versão no git), rode, informando onde está a pasta privada da conferência:

```sh
Rscript verificar_vazamento.R --privado <pasta da conferência privada>
```

Em vez de `--privado`, dá para definir a variável de ambiente `CONFERENCIA_PRIVADO`. Não há pasta padrão: sem uma das duas, a checagem não roda.

Ela lê da pasta privada, na hora, a lista do que não pode aparecer aqui e procura cada item em todos os arquivos que podem ir para o repositório:

- códigos e nomes de ações e programas do recorte real e as palavras-chave do filtro, sem diferenciar maiúsculas de minúsculas nem acentos, inclusive nomes partidos entre duas linhas;
- os números publicados e calculados, em várias formas de escrever: inteiros, com milhar, com centavos, em bilhões (uma, duas e três casas) e em milhões (uma e duas casas; as formas curtas só contam ao lado de "R$", "bi" ou "mi"), e com espaço ou espaço não separável no lugar do ponto de milhar;
- percentuais: com três casas ou mais, e na precisão completa, valem sozinhos (é como aparecem em código e em planilha); com uma ou duas casas, só ao lado de "%", "por cento", "p.p." ou "pontos";
- todos os fatores de correção da inflação do arquivo privado;
- os números do capítulo de orçamento do relatório do projeto com três algarismos significativos ou mais (anos ficam de fora);
- nomes das pessoas entrevistadas e da equipe, menos o do autor;
- caminhos de pasta de usuário (`C:/Users/<nome>`) e endereços de e-mail.

Arquivos em UTF-16 e em latin1 são lidos e checados; o que não puder ser lido como texto é listado como não checado. Em HTML gerado com tudo dentro (o relatório de exemplo), as fontes, os estilos e as bibliotecas de terceiros embutidos não são lidos, porque casam por acaso com códigos e números curtos; os dados das tabelas, o texto e as legendas são (exceção `ignorar_conteudo_embutido` em `config/vazamento.yml`).

Resultado: `0` nada encontrado; `1` encontrou (lista arquivo, linha e termo); `2` não pôde checar (sem a pasta privada, pasta privada com listas pequenas demais para ser a certa, arquivo que não deu para ler ou erro da própria checagem), o que **nunca** conta como aprovado. A saída mostra a pasta privada usada, o tamanho de cada lista e as exceções em uso, que ficam em `config/vazamento.yml`, cada uma com o motivo.

Para a checagem rodar sozinha, ative os *hooks* (scripts que o git executa automaticamente) uma vez por cópia do repositório:

```sh
git config core.hooksPath hooks
```

- `hooks/pre-commit` confere o que está no índice do git, isto é, exatamente o que vai para o commit (e não a pasta de trabalho);
- `hooks/pre-push` confere cada commit que vai ser enviado, para que um vazamento apagado depois não fique no histórico publicado.

Os dois exigem a variável `CONFERENCIA_PRIVADO` e bloqueiam o commit ou o push com qualquer resultado diferente de `0`. No Windows, o git executa os hooks mesmo sem a marca de executável; em Linux e macOS, depois do primeiro `git add`, rode também `git update-index --chmod=+x hooks/pre-commit hooks/pre-push`. Os hooks procuram o R em `$RSCRIPT`, depois na instalação do scoop e depois no PATH.

## Como ler o relatório

O relatório (`relatorio/relatorio.html`, gerado pelo passo 3) abre com o **veredito** em uma tela: quantos números foram conferidos e quantas divergências existem. Cada resultado tem um de três status, sempre com ícone e texto, não só cor:

- **confere**: dentro da tolerância;
- **explicada**: fora da tolerância, com causa conhecida e verificada (suposições aparecem como "inferido");
- **diverge**: fora da tolerância e sem explicação.

Depois vêm as checagens, uma a uma, em linguagem simples (o que foi comparado, com o quê, a tolerância e o resultado), a série 2005 a 2025 por fase, e a seção "Como este relatório foi feito", com a impressão digital (hash SHA-256) de cada arquivo usado, as versões dos programas e o comando para refazer tudo. Um glossário explica dotação, empenho, liquidação, pagamento, deflação e safra.

## Estrutura das pastas

```
conferencia-orcamento-r/
├── README.md                    este arquivo
├── DECISOES.md                  por que dados sintéticos, como foram gerados, o que é proibido
├── config/
│   ├── config.yml               caminhos, período, tolerâncias (o código lê tudo daqui)
│   ├── gabaritos.csv            números esperados (gerados junto com os dados)
│   ├── causas_conhecidas.yml    causas das divergências explicadas, registradas antes de rodar
│   └── vazamento.yml            exceções da checagem anti-vazamento, comentadas
├── dados_sinteticos/            as cinco fontes inventadas, no formato das reais, e ipca_fatores_com_erro.csv (demonstração da trava)
├── entradas/recorte_acoes.csv   a lista de ações do recorte (sintética)
├── scripts/gerar_dados_sinteticos.R
├── verificar_vazamento.R        checagem anti-vazamento
├── hooks/pre-commit             roda a checagem sobre o que vai para cada commit
├── hooks/pre-push               roda a checagem sobre cada commit que vai ser enviado
├── site/                        fonte do site (Quarto)
├── docs/                        site gerado, pronto para o GitHub Pages
│   └── relatorio.html           cópia de relatorio/relatorio.html, o relatório de exemplo
├── R/, _targets.R, tests/, relatorio/, rodar.R   o pipeline (cópia exata do código da conferência privada)
├── renv.lock, renv/, .Rprofile  versões exatas dos pacotes (renv)
└── LICENSE, LICENSE-docs        licenças (MIT e CC BY 4.0)
```

Para refazer o site: `cd site && quarto render` (o resultado vai para `docs/`). O relatório de exemplo não é gerado pelo site: depois de `Rscript rodar.R`, copie `relatorio/relatorio.html` para `docs/relatorio.html`. O `quarto render` não apaga essa cópia.

O código e alguns comentários citam o `CONTRATO.md` e as decisões D1 a D9 da conferência. Esses documentos ficam na versão privada, porque descrevem os dados reais; as decisões desta vitrine estão em `DECISOES.md` (V1 a V7).

## Limitações conhecidas

- Os dados sintéticos mostram que o método funciona; não dizem nada sobre o orçamento real.
- A conferência confere contas, não escolhas: o recorte (quais ações contam como juventude) é entrada, não é refeito nem julgado aqui.
- A base completa da União não tem os campos que o filtro original usa em parte dos anos; por isso o recorte não pode ser refeito a partir dela (o relatório mostra a comparação).
- O denominador do último ano é estimado, porque a extração mais nova da União não traz os órgãos.
- A conferência cruzada com a implementação de referência em Python só existe na versão privada (`executivo_python` fica vazio aqui).
- A checagem anti-vazamento só roda onde existe a pasta privada, procura termos exatos (uma paráfrase de um nome real não seria pega), não procura anos soltos nem números com menos de três algarismos significativos, que não identificam nada, e não lê imagens.

## Uso de IA

O código foi escrito com apoio de IA (Claude Code) sob especificação e revisão de André Trevisol Trindade; o desenho, as decisões analíticas e a validação contra as fontes oficiais são dele.

## Como citar

Trindade, André Trevisol. *Conferência independente do orçamento federal de juventude em R: vitrine com dados sintéticos*. Instituto Cíclica, projeto PPJ, 2026. Disponível em: https://github.com/attrindade/conferencia-orcamento-r.

## Licença

- código: licença MIT (arquivo `LICENSE`);
- documentação (textos, site e relatório): Creative Commons Atribuição 4.0 Internacional, CC BY 4.0 (arquivo `LICENSE-docs`).
