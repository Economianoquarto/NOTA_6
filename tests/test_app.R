# Testes simples, executáveis sem dependências adicionais.
source("app.R", local = FALSE)

parar_se_falso <- function(condicao, mensagem) {
  if (!isTRUE(condicao)) {
    stop(mensagem, call. = FALSE)
  }
}

parar_se_falso(modelo$n == 12L, "O modelo não contém 12 setores.")
parar_se_falso(modelo$raio_espectral < 1, "O raio espectral não é menor que 1.")
parar_se_falso(is.finite(modelo$condicao_sistema), "I - A não possui condição finita.")
parar_se_falso(
  abs(modelo$erro_balanco - 0.23306901379464762) < 1e-8,
  "A folga de balanceamento difere da documentada."
)

choque_agro <- simular_choque(modelo, "Agro", "valor", 100)

parar_se_falso(
  abs(choque_agro$impacto_producao_total - 128.0236) < 1e-4,
  "O impacto total na produção do cenário de referência divergiu."
)
parar_se_falso(
  abs(choque_agro$impacto_vab_total - 71.83459) < 1e-4,
  "O impacto total no VAB do cenário de referência divergiu."
)
parar_se_falso(
  abs(choque_agro$impacto_emprego_total - 3702.219) < 1e-3,
  "O impacto total no emprego do cenário de referência divergiu."
)
parar_se_falso(
  choque_agro$residuo_identidade < 1e-12,
  "A identidade (I - A) delta_X = delta_y não foi satisfeita."
)

choque_percentual <- simular_choque(modelo, "Agro", "percentual", 10)
parar_se_falso(
  abs(choque_percentual$valor_choque - 0.10 * modelo$y[1]) < 1e-10,
  "A conversão do choque percentual está incorreta."
)

reducao_valida <- simular_choque(modelo, "Agro", "percentual", -100)
parar_se_falso(
  abs(reducao_valida$demanda_final_pos_choque) < TOL,
  "Uma redução de 100% deveria zerar a demanda final."
)

erro_reducao <- tryCatch(
  {
    simular_choque(modelo, "Agro", "percentual", -100.01)
    NULL
  },
  error = function(e) e
)
parar_se_falso(inherits(erro_reducao, "error"), "Uma redução inválida não foi bloqueada.")

parar_se_falso(
  all(vapply(
    indicadores,
    function(x) {
      colunas_numericas <- vapply(x, is.numeric, logical(1))
      all(is.finite(unlist(x[colunas_numericas], use.names = FALSE)))
    },
    logical(1)
  )),
  "Há valores não finitos nos indicadores estruturais."
)

shiny::testServer(server, {
  session$setInputs(
    setor_choque = "Agro",
    tipo_choque = "valor",
    magnitude_choque = 100,
    tipo_mult_emprego = "emprego_simples",
    tipo_campo = "continuo"
  )
  session$flushReact()
  parar_se_falso(
    abs(resultado_valido()$impacto_producao_total - 128.0236) < 1e-4,
    "O servidor não atualizou corretamente o choque em valor."
  )

  session$setInputs(tipo_choque = "percentual", magnitude_choque = 10)
  session$flushReact()
  parar_se_falso(
    abs(resultado_valido()$valor_choque - 0.10 * modelo$y[1]) < 1e-10,
    "O servidor não atualizou corretamente o choque percentual."
  )

  # Força a renderização de todos os componentes visuais e tabelas.
  invisible(output$card_producao)
  invisible(output$card_indireto)
  invisible(output$card_multiplicador)
  invisible(output$card_vab)
  invisible(output$card_emprego)
  invisible(output$grafico_impacto_producao)
  invisible(output$tabela_impactos)
  invisible(output$grafico_multiplicador_producao)
  invisible(output$tabela_multiplicador_producao)
  invisible(output$grafico_multiplicador_emprego)
  invisible(output$tabela_multiplicador_emprego)
  invisible(output$grafico_ligacoes)
  invisible(output$tabela_ligacoes)
  invisible(output$grafico_indices_puros)
  invisible(output$tabela_indices_puros)
  invisible(output$grafico_coeficientes_variacao)
  invisible(output$tabela_coeficientes_variacao)
  invisible(output$grafico_campo)
  invisible(output$tabela_diagnostico)

  session$setInputs(tipo_choque = "percentual", magnitude_choque = -100.01)
  session$flushReact()
  parar_se_falso(
    inherits(resultado_choque(), "erro_choque"),
    "O servidor não apresentou erro para uma redução inválida."
  )
})

cat("Todos os testes do aplicativo foram aprovados.\n")
