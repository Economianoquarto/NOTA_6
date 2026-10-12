# Choques de demanda na MIP do Piauí — Nota 6

Aplicativo Shiny para simular mudanças na demanda final da matriz de
insumo-produto de 12 setores contida em `dados/nota_6.xlsx`.

## Como executar

Abra uma sessão limpa do R na raiz do projeto e execute:

```r
shiny::runApp("NOTA_6")
```

## Entradas e resultados

O usuário escolhe um setor e informa um choque em R$ milhões ou como percentual
da demanda final original. Choques negativos são aceitos até o limite que mantém
a demanda final do setor não negativa.

O aplicativo calcula `delta_X = B %*% delta_y`, em que `B = solve(I - A)`, e
apresenta impactos sobre produção, valor adicionado bruto e emprego. Valores
monetários estão em R$ milhões; emprego está em número estimado de pessoas.

Também são apresentados multiplicadores, índices de ligação, índices puros,
coeficientes de variação e campo de influência, seguindo a metodologia do script
`vale_perobelli/mip_pi_com_png.R`.

Os resultados representam efeitos contábeis de um modelo aberto de Leontief com
coeficientes técnicos fixos. Não são previsões nem estimativas causais.

## Conferência no Excel

A aba `22_Simulacao` de `dados/nota_6.xlsx` faz a mesma simulação com fórmulas do
Excel, em linguagem simples. Escolha o setor, a forma de entrada e o tamanho do
choque nas células amarelas e compare os resultados com os cartões e a tabela do
site. Na planilha, a tabela segue a ordem dos setores; no site, ela é ordenada
pelo tamanho do impacto. A aba usa a inversa de Leontief da aba `17_Leontief` e
as abas `x`, `v`, `e`, `y` e `set`, as mesmas lidas pelo aplicativo.

## Dependências

O aplicativo utiliza os pacotes `shiny`, `bslib`, `DT`, `readxl`, `dplyr`,
`ggplot2`, `scales` e `ggrepel`.

## Documento Quarto

O arquivo `nota_6_choques.qmd` apresenta a mesma simulação em formato de artigo
interativo, com espaço para texto metodológico e interpretação econômica. Para
visualizá-lo localmente, execute na pasta `NOTA_6`:

```bash
quarto preview nota_6_choques.qmd
```

O documento usa `server: shiny` e, portanto, deve ser publicado em um serviço
que execute R e Shiny, como o Posit Connect ou o Posit Connect Cloud. Uma versão
estática do HTML não preserva a simulação interativa.

O pacote R `quarto` é útil para publicar a partir de uma sessão do R, mas não é
necessário para editar ou visualizar o documento quando o Quarto CLI já está
instalado.

No Positron, o Preview usa o Quarto CLI que acompanha o próprio Positron, por
causa da configuração `"quarto.useBundledQuartoInPositron": true` nas
configurações do usuário. O Quarto CLI instalado no sistema (1.1.251, que é o
chamado pelo comando `quarto` no terminal) não serve este documento: com ele, o
Preview falha logo após o knit porque o processo R não encontra o `.qmd`. Essa
instalação também precisa ser atualizada antes da publicação no **Posit Connect
Cloud**, cujo provedor de publicação foi incorporado em versões posteriores do
Quarto.

## Verificação

Para executar os testes numéricos e reativos:

```r
setwd("NOTA_6")
source("tests/test_app.R")
```
