###############################################################################
# SHINY — CHOQUES DE DEMANDA NA MIP DO PIAUÍ (NOTA 6)
#
# A planilha em dados/nota_6.xlsx é apenas lida. Os cálculos do modelo são
# feitos uma vez na inicialização; somente o vetor de choque muda na sessão.
###############################################################################

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

TOL <- 1e-8
EPSILON_CAMPO <- 0.001

ler_vetor <- function(caminho, aba) {
  dados <- read_excel(
    caminho,
    sheet = aba,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  unname(unlist(dados, use.names = FALSE))
}

carregar_modelo <- function(caminho) {
  abas_obrigatorias <- c("Z", "y", "x", "v", "e", "c", "sp", "set")

  if (!file.exists(caminho)) {
    stop("Arquivo de dados não encontrado: ", caminho, call. = FALSE)
  }

  abas_arquivo <- excel_sheets(caminho)
  abas_ausentes <- setdiff(abas_obrigatorias, abas_arquivo)

  if (length(abas_ausentes) > 0) {
    stop(
      "A planilha não contém as abas obrigatórias: ",
      paste(abas_ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  Z <- as.matrix(read_excel(
    caminho,
    sheet = "Z",
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  storage.mode(Z) <- "double"

  y <- as.numeric(ler_vetor(caminho, "y"))
  x <- as.numeric(ler_vetor(caminho, "x"))
  v <- as.numeric(ler_vetor(caminho, "v"))
  e <- as.numeric(ler_vetor(caminho, "e"))
  c <- as.numeric(ler_vetor(caminho, "c"))
  sp <- as.numeric(ler_vetor(caminho, "sp"))
  setores <- as.character(ler_vetor(caminho, "set"))

  n <- length(x)

  if (n != 12L) {
    stop("A MIP deve conter exatamente 12 setores; foram encontrados ", n, ".", call. = FALSE)
  }

  if (!identical(dim(Z), c(n, n))) {
    stop("A aba Z deve formar uma matriz 12 x 12.", call. = FALSE)
  }

  vetores_numericos <- list(y = y, x = x, v = v, e = e, c = c, sp = sp)
  tamanhos_validos <- vapply(vetores_numericos, length, integer(1)) == n
  finitos <- vapply(vetores_numericos, function(z) all(is.finite(z)), logical(1))

  if (!all(tamanhos_validos)) {
    stop("As abas y, x, v, e, c e sp devem ter 12 observações.", call. = FALSE)
  }

  if (!all(finitos) || !all(is.finite(Z))) {
    stop("As matrizes e os vetores econômicos devem conter somente números finitos.", call. = FALSE)
  }

  if (length(setores) != n || anyNA(setores) || any(trimws(setores) == "")) {
    stop("A aba set deve conter um nome válido para cada um dos 12 setores.", call. = FALSE)
  }

  if (anyDuplicated(setores)) {
    stop("Os nomes dos setores na aba set devem ser únicos.", call. = FALSE)
  }

  if (any(x <= 0)) {
    stop("Todos os valores do vetor de produção x devem ser positivos.", call. = FALSE)
  }

  A <- sweep(Z, MARGIN = 2, STATS = x, FUN = "/")
  matriz_sistema <- diag(n) - A
  determinante <- det(matriz_sistema)

  if (!is.finite(determinante) || abs(determinante) < .Machine$double.eps) {
    stop("A matriz I - A não é invertível.", call. = FALSE)
  }

  raio_espectral <- max(Mod(eigen(A, only.values = TRUE)$values))

  if (!is.finite(raio_espectral) || raio_espectral >= 1) {
    stop("O raio espectral de A deve ser menor que 1.", call. = FALSE)
  }

  B <- solve(matriz_sistema)

  list(
    Z = Z,
    y = y,
    x = x,
    v = v,
    e = e,
    c = c,
    sp = sp,
    setores = setores,
    n = n,
    A = A,
    B = B,
    coef_vab = v / x,
    coef_emprego = e / x,
    raio_espectral = raio_espectral,
    condicao_sistema = kappa(matriz_sistema),
    erro_balanco = max(abs(x - rowSums(Z) - y))
  )
}

calcular_indicadores <- function(modelo) {
  n <- modelo$n
  B <- modelo$B
  A <- modelo$A
  y <- modelo$y
  setores <- modelo$setores

  multiplicadores <- data.frame(
    setor = setores,
    producao = colSums(B),
    emprego_simples = colSums(diag(modelo$coef_emprego) %*% B),
    emprego_tipo_I = colSums(diag(modelo$coef_emprego) %*% B) / modelo$coef_emprego,
    stringsAsFactors = FALSE
  )

  ligacoes <- data.frame(
    setor = setores,
    para_tras = colMeans(B) / mean(B),
    para_frente = rowMeans(B) / mean(B),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      setor_chave = if_else(
        para_tras > 1 & para_frente > 1,
        "Setor-chave",
        "Não"
      )
    )

  indices_puros <- matrix(NA_real_, nrow = n, ncol = 3)

  # Em cada iteração, um setor é separado do restante da economia.
  for (s in seq_len(n)) {
    y_setor <- y[s]
    y_restante <- y[-s]
    A_setor <- A[s, s]
    A_setor_restante <- matrix(A[s, -s], nrow = 1)
    A_restante_setor <- matrix(A[-s, s], ncol = 1)
    A_restante <- A[-s, -s, drop = FALSE]

    inversa_setor <- 1 / (1 - A_setor)
    inversa_restante <- solve(diag(n - 1) - A_restante)

    ligacao_pura_tras <- sum(
      inversa_restante %*% A_restante_setor %*% inversa_setor %*% y_setor
    )
    ligacao_pura_frente <- as.numeric(
      inversa_setor %*% A_setor_restante %*% inversa_restante %*% y_restante
    )

    indices_puros[s, ] <- c(
      ligacao_pura_tras,
      ligacao_pura_frente,
      ligacao_pura_tras + ligacao_pura_frente
    )
  }

  medias_indices_puros <- colMeans(indices_puros)
  indices_puros_normalizados <- sweep(
    indices_puros,
    MARGIN = 2,
    STATS = medias_indices_puros,
    FUN = "/"
  )

  indices_puros_tabela <- data.frame(
    setor = setores,
    PBLN = indices_puros_normalizados[, 1],
    PFLN = indices_puros_normalizados[, 2],
    PTL = indices_puros_normalizados[, 3],
    stringsAsFactors = FALSE
  )

  coeficientes_variacao <- data.frame(
    setor = setores,
    dispersao_coluna = apply(B, 2, sd) / colMeans(B),
    dispersao_linha = apply(B, 1, sd) / rowMeans(B),
    stringsAsFactors = FALSE
  )

  campo <- matrix(0, nrow = n, ncol = n)
  perturbacao <- matrix(0, nrow = n, ncol = n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      perturbacao[i, j] <- EPSILON_CAMPO
      inversa_perturbada <- solve(diag(n) - (A + perturbacao))
      variacao_inversa <- (inversa_perturbada - B) / EPSILON_CAMPO
      campo[i, j] <- sum(variacao_inversa^2)
      perturbacao[i, j] <- 0
    }
  }

  indices_campo <- expand.grid(
    fornecedor = setores,
    consumidor = setores,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  indices_campo$influencia <- as.vector(campo)
  indices_campo$acima_media <- indices_campo$influencia >= mean(campo)

  list(
    multiplicadores = multiplicadores,
    ligacoes = ligacoes,
    indices_puros = indices_puros_tabela,
    coeficientes_variacao = coeficientes_variacao,
    campo = indices_campo
  )
}

simular_choque <- function(modelo, setor, tipo, magnitude) {
  indice_setor <- match(setor, modelo$setores)

  if (is.na(indice_setor)) {
    stop("Selecione um setor válido.", call. = FALSE)
  }

  if (length(magnitude) != 1 || !is.finite(magnitude)) {
    stop("Informe uma magnitude numérica válida.", call. = FALSE)
  }

  if (tipo == "valor") {
    valor_choque <- magnitude
  } else if (tipo == "percentual") {
    valor_choque <- modelo$y[indice_setor] * magnitude / 100
  } else {
    stop("Selecione um tipo de choque válido.", call. = FALSE)
  }

  demanda_final_pos_choque <- modelo$y[indice_setor] + valor_choque

  if (demanda_final_pos_choque < -TOL) {
    stop(
      "O choque reduziria a demanda final do setor para um valor negativo. ",
      "A redução máxima permitida é R$ ",
      format(round(modelo$y[indice_setor], 4), decimal.mark = ",", big.mark = "."),
      " milhões.",
      call. = FALSE
    )
  }

  delta_y <- numeric(modelo$n)
  delta_y[indice_setor] <- valor_choque
  delta_x <- as.vector(modelo$B %*% delta_y)
  delta_vab <- modelo$coef_vab * delta_x
  delta_emprego <- modelo$coef_emprego * delta_x
  impacto_producao_total <- sum(delta_x)

  participacao <- if (abs(impacto_producao_total) <= TOL) {
    rep(NA_real_, modelo$n)
  } else {
    delta_x / impacto_producao_total
  }

  tabela <- data.frame(
    setor = modelo$setores,
    choque_direto = delta_y,
    impacto_producao = delta_x,
    impacto_vab = delta_vab,
    impacto_emprego = delta_emprego,
    participacao_impacto = participacao,
    impacto_producao_pct_vbp = delta_x / modelo$x,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(abs(impacto_producao)))

  list(
    setor = setor,
    indice_setor = indice_setor,
    valor_choque = valor_choque,
    demanda_final_original = modelo$y[indice_setor],
    demanda_final_pos_choque = demanda_final_pos_choque,
    impacto_producao_total = impacto_producao_total,
    efeito_indireto = impacto_producao_total - valor_choque,
    multiplicador = if (abs(valor_choque) <= TOL) NA_real_ else impacto_producao_total / valor_choque,
    impacto_vab_total = sum(delta_vab),
    impacto_emprego_total = sum(delta_emprego),
    residuo_identidade = max(abs(as.vector((diag(modelo$n) - modelo$A) %*% delta_x) - delta_y)),
    tabela = tabela
  )
}

formatar_numero <- function(valor, casas = 2) {
  if (length(valor) == 0 || is.na(valor)) {
    return("—")
  }

  number(
    valor,
    accuracy = 10^(-casas),
    big.mark = ".",
    decimal.mark = ","
  )
}

grafico_barras <- function(dados, variavel, titulo, eixo_y) {
  dados_plot <- dados %>%
    mutate(setor = reorder(setor, .data[[variavel]]))

  ggplot(dados_plot, aes(x = setor, y = .data[[variavel]])) +
    geom_col(fill = "#176B87") +
    coord_flip() +
    labs(title = titulo, x = NULL, y = eixo_y) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major.y = element_blank())
}

ARQ_DADOS <- file.path("dados", "nota_6.xlsx")
modelo <- carregar_modelo(ARQ_DADOS)
indicadores <- calcular_indicadores(modelo)

tema_app <- bs_theme(
  version = 5,
  bg = "#F7F9FB",
  fg = "#1F2933",
  primary = "#176B87",
  secondary = "#5C6F7B"
)

ui <- navbarPage(
  title = "MIP do Piauí — Nota 6",
  theme = tema_app,
  header = tags$head(
    tags$style(HTML("\n      body { background-color: #f7f9fb; }\n      .navbar { margin-bottom: 22px; }\n      .metric-card {\n        background: white; border: 1px solid #dfe5e8; border-radius: 10px;\n        padding: 16px; margin-bottom: 16px; min-height: 116px;\n        box-shadow: 0 2px 8px rgba(31, 41, 51, 0.06);\n      }\n      .metric-label { color: #52616b; font-size: 0.92rem; }\n      .metric-value { color: #164B60; font-size: 1.65rem; font-weight: 650; }\n      .method-note {\n        background: #eef6f8; border-left: 4px solid #176B87;\n        padding: 12px 16px; margin: 12px 0 20px;\n      }\n      .control-label { font-weight: 600; }\n    "))
  ),

  tabPanel(
    "Simulação de choque",
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("Definição do cenário"),
          selectInput(
            "setor_choque",
            "Setor",
            choices = modelo$setores,
            selected = modelo$setores[1]
          ),
          radioButtons(
            "tipo_choque",
            "Forma de entrada",
            choices = c(
              "Valor em R$ milhões" = "valor",
              "Percentual da demanda final" = "percentual"
            ),
            selected = "valor"
          ),
          uiOutput("entrada_magnitude"),
          hr(),
          uiOutput("demanda_setor"),
          helpText("Valores positivos ampliam e valores negativos reduzem a demanda final.")
        )
      ),
      column(
        width = 9,
        uiOutput("mensagem_erro"),
        fluidRow(
          column(4, uiOutput("card_producao")),
          column(4, uiOutput("card_indireto")),
          column(4, uiOutput("card_multiplicador"))
        ),
        fluidRow(
          column(6, uiOutput("card_vab")),
          column(6, uiOutput("card_emprego"))
        ),
        plotOutput("grafico_impacto_producao", height = "470px")
      )
    ),
    div(
      class = "method-note",
      strong("Interpretação: "),
      "os resultados são efeitos contábeis do modelo aberto de Leontief, com coeficientes técnicos fixos. Não representam previsão nem efeito causal."
    )
  ),

  tabPanel(
    "Resultados setoriais",
    h3("Detalhamento do choque por setor"),
    p("A tabela é ordenada pelo módulo do impacto sobre a produção."),
    DTOutput("tabela_impactos")
  ),

  tabPanel(
    "Indicadores estruturais",
    tabsetPanel(
      tabPanel(
        "Produção",
        br(),
        plotOutput("grafico_multiplicador_producao", height = "480px"),
        DTOutput("tabela_multiplicador_producao")
      ),
      tabPanel(
        "Emprego",
        br(),
        radioButtons(
          "tipo_mult_emprego",
          "Indicador",
          choices = c("Multiplicador simples" = "emprego_simples", "Multiplicador Tipo I" = "emprego_tipo_I"),
          selected = "emprego_simples",
          inline = TRUE
        ),
        plotOutput("grafico_multiplicador_emprego", height = "480px"),
        DTOutput("tabela_multiplicador_emprego")
      ),
      tabPanel(
        "Ligações",
        br(),
        plotOutput("grafico_ligacoes", height = "560px"),
        DTOutput("tabela_ligacoes")
      ),
      tabPanel(
        "Índices puros",
        br(),
        plotOutput("grafico_indices_puros", height = "480px"),
        DTOutput("tabela_indices_puros")
      ),
      tabPanel(
        "Dispersão",
        br(),
        plotOutput("grafico_coeficientes_variacao", height = "480px"),
        DTOutput("tabela_coeficientes_variacao")
      ),
      tabPanel(
        "Campo de influência",
        br(),
        radioButtons(
          "tipo_campo",
          "Visualização",
          choices = c("Valores contínuos" = "continuo", "Acima ou abaixo da média" = "media"),
          selected = "continuo",
          inline = TRUE
        ),
        plotOutput("grafico_campo", height = "620px")
      )
    )
  ),

  tabPanel(
    "Diagnóstico",
    h3("Validações da matriz"),
    tableOutput("tabela_diagnostico"),
    div(
      class = "method-note",
      "A folga da identidade X = Z1 + y é herdada do balanceamento da TRUR e já está documentada na aba 19_Diagnostico da planilha-fonte."
    )
  )
)

server <- function(input, output, session) {
  output$entrada_magnitude <- renderUI({
    if (identical(input$tipo_choque, "percentual")) {
      numericInput(
        "magnitude_choque",
        "Variação da demanda final (%)",
        value = 10,
        step = 1
      )
    } else {
      numericInput(
        "magnitude_choque",
        "Variação da demanda final (R$ milhões)",
        value = 100,
        step = 10
      )
    }
  })

  output$demanda_setor <- renderUI({
    indice <- match(input$setor_choque, modelo$setores)
    tagList(
      strong("Demanda final original:"),
      br(),
      paste0("R$ ", formatar_numero(modelo$y[indice]), " milhões")
    )
  })

  resultado_choque <- reactive({
    req(input$setor_choque, input$tipo_choque, input$magnitude_choque)

    tryCatch(
      simular_choque(
        modelo = modelo,
        setor = input$setor_choque,
        tipo = input$tipo_choque,
        magnitude = input$magnitude_choque
      ),
      error = function(e) {
        structure(list(mensagem = conditionMessage(e)), class = "erro_choque")
      }
    )
  })

  resultado_valido <- reactive({
    resultado <- resultado_choque()
    validate(need(!inherits(resultado, "erro_choque"), resultado$mensagem))
    resultado
  })

  output$mensagem_erro <- renderUI({
    resultado <- resultado_choque()

    if (inherits(resultado, "erro_choque")) {
      div(class = "alert alert-danger", resultado$mensagem)
    }
  })

  criar_card <- function(rotulo, valor, complemento = NULL) {
    div(
      class = "metric-card",
      div(class = "metric-label", rotulo),
      div(class = "metric-value", valor),
      if (!is.null(complemento)) tags$small(complemento)
    )
  }

  output$card_producao <- renderUI({
    resultado <- resultado_valido()
    criar_card(
      "Impacto total na produção",
      paste0("R$ ", formatar_numero(resultado$impacto_producao_total), " mi")
    )
  })

  output$card_indireto <- renderUI({
    resultado <- resultado_valido()
    criar_card(
      "Efeito indireto na produção",
      paste0("R$ ", formatar_numero(resultado$efeito_indireto), " mi")
    )
  })

  output$card_multiplicador <- renderUI({
    resultado <- resultado_valido()
    criar_card(
      "Multiplicador de produção",
      formatar_numero(resultado$multiplicador, 3),
      "R$ de produção por R$ de demanda"
    )
  })

  output$card_vab <- renderUI({
    resultado <- resultado_valido()
    criar_card(
      "Impacto total no VAB",
      paste0("R$ ", formatar_numero(resultado$impacto_vab_total), " mi")
    )
  })

  output$card_emprego <- renderUI({
    resultado <- resultado_valido()
    criar_card(
      "Impacto total no emprego",
      paste0(formatar_numero(resultado$impacto_emprego_total), " pessoas")
    )
  })

  output$grafico_impacto_producao <- renderPlot({
    resultado <- resultado_valido()

    dados_plot <- resultado$tabela %>%
      mutate(setor = reorder(setor, impacto_producao))

    ggplot(dados_plot, aes(x = setor, y = impacto_producao, fill = impacto_producao >= 0)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#176B87", "FALSE" = "#B23A48")) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(
        title = "Impacto do choque sobre a produção setorial",
        subtitle = paste0(
          resultado$setor,
          ": R$ ", formatar_numero(resultado$valor_choque), " milhões"
        ),
        x = NULL,
        y = "R$ milhões"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank())
  })

  output$tabela_impactos <- renderDT({
    tabela <- resultado_valido()$tabela %>%
      rename(
        Setor = setor,
        `Choque direto (R$ mi)` = choque_direto,
        `Impacto na produção (R$ mi)` = impacto_producao,
        `Impacto no VAB (R$ mi)` = impacto_vab,
        `Impacto no emprego (pessoas)` = impacto_emprego,
        `Participação no impacto` = participacao_impacto,
        `Impacto / VBP inicial` = impacto_producao_pct_vbp
      )

    datatable(
      tabela,
      rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE, dom = "tip"),
      class = "stripe hover compact"
    ) %>%
      formatRound(2:5, digits = 2, dec.mark = ",", mark = ".") %>%
      formatPercentage(6:7, digits = 2, dec.mark = ",", mark = ".")
  })

  output$grafico_multiplicador_producao <- renderPlot({
    grafico_barras(
      indicadores$multiplicadores,
      "producao",
      "Multiplicadores de produção",
      "R$ de produção por R$ de demanda final"
    )
  })

  output$tabela_multiplicador_producao <- renderDT({
    indicadores$multiplicadores %>%
      select(Setor = setor, `Multiplicador de produção` = producao) %>%
      datatable(rownames = FALSE, options = list(dom = "t", paging = FALSE)) %>%
      formatRound(2, 4, dec.mark = ",", mark = ".")
  })

  output$grafico_multiplicador_emprego <- renderPlot({
    variavel <- input$tipo_mult_emprego
    titulo <- if (variavel == "emprego_simples") {
      "Multiplicadores simples de emprego"
    } else {
      "Multiplicadores de emprego — Tipo I"
    }
    eixo <- if (variavel == "emprego_simples") {
      "Pessoas por R$ 1 milhão de demanda final"
    } else {
      "Empregos totais por emprego direto"
    }

    grafico_barras(indicadores$multiplicadores, variavel, titulo, eixo)
  })

  output$tabela_multiplicador_emprego <- renderDT({
    indicadores$multiplicadores %>%
      select(
        Setor = setor,
        `Multiplicador simples` = emprego_simples,
        `Multiplicador Tipo I` = emprego_tipo_I
      ) %>%
      datatable(rownames = FALSE, options = list(dom = "t", paging = FALSE)) %>%
      formatRound(2:3, 4, dec.mark = ",", mark = ".")
  })

  output$grafico_ligacoes <- renderPlot({
    ggplot(indicadores$ligacoes, aes(x = para_frente, y = para_tras, label = setor)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "#5C6F7B") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#5C6F7B") +
      geom_point(aes(color = setor_chave), size = 3) +
      geom_text_repel(size = 4, max.overlaps = Inf) +
      scale_color_manual(values = c("Setor-chave" = "#B23A48", "Não" = "#176B87")) +
      labs(
        title = "Índices de ligação de Rasmussen-Hirschman",
        x = "Ligação para frente",
        y = "Ligação para trás",
        color = NULL
      ) +
      theme_minimal(base_size = 13)
  })

  output$tabela_ligacoes <- renderDT({
    indicadores$ligacoes %>%
      rename(
        Setor = setor,
        `Ligação para trás` = para_tras,
        `Ligação para frente` = para_frente,
        Classificação = setor_chave
      ) %>%
      datatable(rownames = FALSE, options = list(dom = "t", paging = FALSE)) %>%
      formatRound(2:3, 4, dec.mark = ",", mark = ".")
  })

  output$grafico_indices_puros <- renderPlot({
    grafico_barras(
      indicadores$indices_puros,
      "PTL",
      "Índice puro total de ligação normalizado",
      "Índice normalizado"
    ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "#B23A48")
  })

  output$tabela_indices_puros <- renderDT({
    indicadores$indices_puros %>%
      rename(Setor = setor) %>%
      datatable(rownames = FALSE, options = list(dom = "t", paging = FALSE)) %>%
      formatRound(2:4, 4, dec.mark = ",", mark = ".")
  })

  output$grafico_coeficientes_variacao <- renderPlot({
    dados_longos <- bind_rows(
      indicadores$coeficientes_variacao %>%
        transmute(setor, indicador = "Dispersão nas colunas", valor = dispersao_coluna),
      indicadores$coeficientes_variacao %>%
        transmute(setor, indicador = "Dispersão nas linhas", valor = dispersao_linha)
    )

    ggplot(dados_longos, aes(x = setor, y = valor, fill = indicador)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("#176B87", "#F28E2B")) +
      labs(
        title = "Coeficientes de variação da inversa de Leontief",
        x = NULL,
        y = "Coeficiente de variação",
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank())
  })

  output$tabela_coeficientes_variacao <- renderDT({
    indicadores$coeficientes_variacao %>%
      rename(
        Setor = setor,
        `Dispersão nas colunas` = dispersao_coluna,
        `Dispersão nas linhas` = dispersao_linha
      ) %>%
      datatable(rownames = FALSE, options = list(dom = "t", paging = FALSE)) %>%
      formatRound(2:3, 4, dec.mark = ",", mark = ".")
  })

  output$grafico_campo <- renderPlot({
    dados <- indicadores$campo

    if (identical(input$tipo_campo, "media")) {
      grafico <- ggplot(dados, aes(x = consumidor, y = fornecedor, fill = acima_media)) +
        geom_tile(color = "white", linewidth = 0.2) +
        scale_fill_manual(
          values = c("TRUE" = "#164B60", "FALSE" = "#E5E7EB"),
          labels = c("FALSE" = "Abaixo da média", "TRUE" = "Acima da média")
        ) +
        labs(fill = NULL)
    } else {
      grafico <- ggplot(dados, aes(x = consumidor, y = fornecedor, fill = influencia)) +
        geom_tile(color = "white", linewidth = 0.2) +
        scale_fill_gradient(low = "#EEF6F8", high = "#164B60") +
        labs(fill = "Influência")
    }

    grafico +
      labs(
        title = "Campo de influência",
        x = "Setor consumidor",
        y = "Setor fornecedor"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
  })

  output$tabela_diagnostico <- renderTable({
    data.frame(
      Verificação = c(
        "Número de setores",
        "Raio espectral de A",
        "Número de condição de I - A",
        "Máxima folga em X = Z1 + y"
      ),
      Resultado = c(
        modelo$n,
        formatar_numero(modelo$raio_espectral, 6),
        formatar_numero(modelo$condicao_sistema, 6),
        paste0("R$ ", formatar_numero(modelo$erro_balanco, 6), " milhões")
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")
}

app <- shinyApp(ui = ui, server = server)
