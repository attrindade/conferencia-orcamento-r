# Decisões da vitrine

Registro das decisões sobre esta vitrine pública, com data, motivo e alternativa descartada. As decisões do método da conferência (numerador, denominador, recorte, tolerâncias) estão resumidas no site, página Método. As decisões D1 a D9 e o `CONTRATO.md` citados no código ficam na versão privada, porque descrevem os dados reais.

## 29/10/2026

### V1. Dados sintéticos, e não uma amostra dos dados reais

- **Decisão:** a vitrine roda o mesmo código da conferência privada sobre dados inteiramente inventados, gerados por `scripts/gerar_dados_sinteticos.R`.
- **Motivo:** os dados reais e os números publicados pertencem ao projeto e não são desta vitrine. Uma amostra, uma versão anonimizada ou valores multiplicados por uma constante ainda deixariam reconhecer ações, programas ou proporções reais. Dados inventados mostram o método sem expor nada.
- **Descartado:** publicar uma amostra dos dados reais; publicar só o código, sem dados (ninguém conseguiria rodar).

### V2. Como os dados foram gerados

- Semente fixa (`set.seed(20261029)`): quem rodar o gerador obtém arquivos idênticos byte a byte (conferido rodando duas vezes e comparando os hashes).
- **Mesmo formato das fontes reais**: mesmos nomes de coluna, separadores (`;` ou `,`), números (sem milhar, ou milhar com ponto), marca de BOM no início dos arquivos que a têm, quebras de linha do Windows, a linha "Total" no topo da base da União e as colunas "Corrigido" que as fontes trazem e o pipeline não pode usar.
- **Tudo inventado**: órgãos, programas e ações com códigos que começam com "Z" (o sistema oficial não usa), nomes com "Fictício", palavras-chave do tipo `chave-alfa`, fatores de inflação sorteados, valores na escala de dezenas de milhões. O período (2005 a 2025) é o mesmo do projeto; os anos de cada fenômeno foram escolhidos sem relação com os dados reais (V7).
- **Mesmos fenômenos do método**, para o pipeline ser testado de verdade: as duas safras idênticas na maior parte dos anos e com linhas a mais na nova de 2008 a 2011 (uma ação por palavra-chave nova e uma inclusão manual); a safra antiga com o último ano incompleto; uma ação genérica compartilhada entre programas na base da União; uma ação que, a partir de 2015, tem na base da União linhas que não entram no recorte; uma inclusão manual obrigatória sem palavra-chave, de 2017 a 2022; um fator de inflação "errado conhecido" para 2019 (fica na configuração, para a trava recusá-lo; o arquivo de fatores leva o certo, e `ipca_fatores_com_erro.csv` leva o errado, para demonstração).
- **Gabaritos calculados pelo próprio gerador**, só com o R básico, a partir da verdade inventada e por um caminho independente do pipeline (que usa duckdb e dplyr). O gerador também confere, antes de gravar, que os fenômenos acima ficaram como descritos (as safras idênticas onde devem e diferentes onde devem).
- Percentuais dos gabaritos com quatro casas decimais, para que nenhum coincida por acaso com um número real arredondado (achado da própria checagem anti-vazamento, na primeira versão com duas casas).

### V3. O que é proibido na vitrine e como a checagem garante

- **Proibido:** códigos e nomes de ação e de programa do recorte real, as palavras-chave do filtro real, os números-gabarito reais (inclusive os do capítulo de orçamento do relatório do projeto), os fatores de inflação reais conferidos e qualquer valor da lista de divergências real.
- **Como se garante:** `verificar_vazamento.R` lê essas listas **na hora**, da pasta privada, e procura cada termo nos arquivos que podem ir para o repositório (os que o git rastreia ou rastrearia; os ignorados pelo `.gitignore` não são publicados). Valores são procurados em várias formatações (ponto, vírgula, milhar, arredondamento em bilhões e em milhões). A lista nunca é gravada aqui. A versão endurecida de 29/10/2026 está em V7.
- **Sem a pasta privada, a checagem não passa**: sai com status 2 e a mensagem "NÃO CHECADO". O hook de pre-commit bloqueia o commit nesse caso também.
- **Exceções explícitas** em `config/vazamento.yml`: as palavras genéricas do tema (juventude, jovem, jovens), que também estão na lista real de palavras-chave (ver V4), e o nome do autor.
- **Escolhas para não gerar alarme falso**, cada uma comentada no código: textos com menos de 4 caracteres não entram; contagens e listas de anos só entram com contexto (ver V7); percentuais com uma ou duas casas só contam ao lado de "%" ou equivalente; códigos de programa iguais a um ano (há programas reais numerados como anos) só contam na forma de código ("código - nome" ou dentro de uma lista separada por "|").
- **Descartado:** guardar uma lista proibida cifrada ou com hashes na vitrine (ainda seria informação sobre o recorte, e daria trabalho manter); confiar só em revisão manual.

### V4. Palavras genéricas do tema permitidas em qualquer arquivo (decisão D9 da conferência privada)

- **Decisão:** "juventude", "jovem" e "jovens" passam a ser permitidas em qualquer arquivo da vitrine, inclusive em código (`config/vazamento.yml`, chave `termos_permitidos_em_qualquer_arquivo`; `verificar_vazamento.R` pula esses três termos). Antes eram permitidas só em documentação.
- **Motivo:** o código copiado da conferência privada fala em "recorte de juventude" em comentários e mensagens, e a vitrine cita o projeto com autorização da coordenação (29/10). As três palavras sozinhas não revelam o filtro; a lista de palavras-chave, sim.
- **O que não mudou:** as demais palavras-chave do recorte real, e qualquer lista delas, continuam proibidas em todo lugar. Nenhuma outra regra da checagem foi afrouxada.
- **Descartado:** reescrever o código para evitar a palavra "juventude", o que deixaria código e documentação falando de coisas diferentes.

### V5. Código copiado sem mudança; o que é do projeto fica em `config/`

- **Decisão:** `R/`, `_targets.R`, `rodar.R`, `tests/`, `relatorio/` e `scripts/derivar_recorte.R` são cópias exatas da conferência privada (29/10/2026). Tudo o que é específico de um projeto vem da configuração: `config/config.yml` (ações obrigatórias, textos e famílias do relatório, valores que os testes conferem) e o novo `config/causas_conhecidas.yml` (causas das divergências explicadas).
- **Falsos positivos corrigidos na origem, não na regra:** a primeira checagem do código neutro achou oito números de estilo ou de exemplo que coincidiam com valores reais arredondados em bilhões (coordenadas com casas decimais no desenho dos ícones do relatório e exemplos de percentual e de valor nos testes). Os ícones passaram para uma grade de 240 × 240 com coordenadas inteiras e os exemplos trocaram de número; a regra da checagem não mudou.
- **`renv.lock` fora da busca:** o arquivo de versões dos pacotes é gerado pelo renv a partir dos metadados públicos dos pacotes. Na primeira checagem, 18 ocorrências nele eram todas blocos de quatro dígitos de identificadores ORCID de autores de pacotes, iguais por acaso a códigos de ação. O arquivo entrou em `pastas_ignoradas` de `config/vazamento.yml`, com o motivo comentado, como já acontecia com `docs/site_libs`; o script da checagem não mudou.
- **Gerador:** `scripts/gerar_dados_sinteticos.R` passou a escrever também o gabarito `ano_maximo_participacao` (o ano de maior participação, comparado de forma exata), para que a métrica nova da conferência rode também aqui.

### V6. Site em `docs/` e licença

- O site é um projeto Quarto em `site/`, gerado em `docs/` (a pasta que o GitHub Pages publica na opção "main /docs"). Nada foi publicado.
- Licença: MIT para o código, CC BY 4.0 para a documentação, aprovadas pelo autor em 29/10/2026. Repositório público em https://github.com/attrindade/conferencia-orcamento-r; a criação no GitHub e o envio são feitos pelo autor.

### V7. Dados sintéticos sem espelhar o real e checagem endurecida (29/10/2026)

Motivo: a revisão anti-vazamento independente mostrou que a vitrine repetia fatos do projeto real sem repetir nenhum valor: o ano do fator de inflação corrigido, os anos em que as safras diferem, o ano até onde o recorte bate na base da União e uma inclusão manual obrigatória com nome e anos parecidos com os de uma ação real. Mostrou também que a checagem deixava passar 13 de 17 vazamentos de teste.

- **Dados:** o fator errado conhecido passou para 2019 e o arquivo de demonstração se chama `ipca_fatores_com_erro.csv`, sem ano no nome; as safras diferem de 2008 a 2011; a ação que muda de programa muda em 2015 (o recorte bate na União até 2014); a inclusão manual obrigatória (`ZM01`) virou uma ação de cultura, de 2017 a 2022, com escala parecida com a das outras ações. Os nomes sintéticos foram conferidos contra os nomes reais de ação, programa, órgão, unidade, eixo, objetivo e plano orçamentário da base privada, por trigramas de palavras (sequências de três palavras): nenhum trigrama em comum, fora o marcador padrão do sistema oficial "[valor não detalhado]". A semente continua a mesma; o gerador roda duas vezes com o mesmo resultado byte a byte.
- **Checagem:** sem pasta privada padrão no código (exige `--privado` ou `CONFERENCIA_PRIVADO`); piso de sanidade para as listas lidas; percentuais com três casas ou mais e na precisão completa sem exigir "%", e com uma ou duas casas ao lado de "%", "por cento", "p.p." ou "pontos"; todos os fatores do IPCA do arquivo privado; bilhões com três casas e milhões com uma e duas casas, só com contexto de dinheiro; espaços e espaço não separável normalizados; códigos sem diferenciar maiúsculas; números do capítulo de orçamento do Relatório Final com três algarismos significativos ou mais; contagens ao lado de "ações" ou "programas" e o ranking de anos como sequência; nomes de pessoas entrevistadas e da equipe; caminhos `C:/Users/<nome>` e e-mails; arquivos UTF-16 e latin1 lidos; o que não der para ler fica como "não checado" (status 2), e o status 1 ficou só para "encontrou".
- **Hooks:** o `pre-commit` confere o conteúdo do índice do git (exportado para uma pasta temporária), e não a pasta de trabalho; o novo `pre-push` confere cada commit que vai ser enviado.
- **Escolhas registradas:** os bilhões com uma e duas casas continuam valendo sem contexto, como na primeira versão (só deixam de valer quando o número é um tamanho de arquivo, como "12,7 KB"); o contexto de dinheiro só é exigido nas formas curtas novas. Percentual com três casas sem "%" não vale quando está escrito como dinheiro ("R$ ... bi"). Formas sem contexto precisam de três algarismos significativos ("0,00" e "1,000" não identificam nada).
- **Exceção nova, à vista em `config/vazamento.yml` (`ignorar_conteudo_embutido`):** o relatório de exemplo é um HTML com tudo dentro, e as fontes, estilos e bibliotecas de terceiros embutidos em "data:", `<script>` e `<style>` deram 33 alarmes falsos em uma simulação. Esses trechos são apagados antes da busca, sem mudar os números de linha; os dados das tabelas interativas (`<script type="application/json">`), o texto, as tabelas e as legendas continuam checados. A saída mostra quantos trechos foram apagados.
- **Descartado:** pedir ao autor a liberação desses anos como públicos. Deslocar os anos sintéticos não custa nada ao método e tira a dúvida.
