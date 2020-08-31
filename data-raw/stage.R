ames <- readr::read_rds("data-raw/ames.rds")

ames %>%
   dplyr::select(venda_valor) %>%
   ggplot2::ggplot() +
   ggplot2::geom_histogram(ggplot2::aes(x = venda_valor), color = "gray") +
   ggplot2::labs(
      title = "Distribuição do Valor de Venda",
      x = "Valor de Venda",
      y = "Contagem"
   )

ames %>%
   dplyr::mutate(log_venda_valor = log(venda_valor)) %>%
   dplyr::select(-venda_valor) %>%
   ggplot2::ggplot() +
   ggplot2::geom_histogram(ggplot2::aes(x = log_venda_valor), color = "gray") +
   ggplot2::labs(
      title = "Distribuição do Logaritmo do Valor de Venda",
      x = "Logaritmo do Valor de Venda",
      y = "Contagem"
   )

modelo_glm <- ames %>%
   dplyr::select(where(is.numeric)) %>%
   glm(formula = as.vector(ames$venda_valor) ~ .,
       family = gaussian(link = "log"))

ggplot2::ggplot() +
   ggplot2::geom_point(ggplot2::aes(x = modelo_glm$y,
                                    y = modelo_glm$fitted.values),
                       size = 0.9,
                       color = "blue",
                       shape = 1,
                       show.legend = F) +
   ggplot2::geom_abline(slope = 1, color = "red") +
   ggplot2::labs(
      title = "Ajuste do Modelo do Valor de Venda",
      x = "Valor Real de Venda",
      y = "Valor Previsto de Venda",
      caption = "Fonte: Base de Dados AMES"
   )
