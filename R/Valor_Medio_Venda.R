#' Funcao de Analise da Base de Dados AMES
#'
#' Esta funcao calcula o valor medio de venda por quebras de uma determinada variavel..
#'
#' @param tab Tabela ou base de dados que contem a variavel a ser analisada.
#' @param nome_coluna Variavel a ser analisada.
#'
#' @return Retorna o valor medio da venda para cada valor da variavel escolhida.
#'
#' @export
sumarizar_venda_media <- function(tab, nome_coluna) {
   tab %>%
      dplyr::group_by(across(nome_coluna)) %>%
      dplyr::summarise(
         valor_medio_venda = mean(venda_valor,
                                  na.rm = TRUE)
      ) %>%
      dplyr::arrange(desc(valor_medio_venda), nome_coluna) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
         valor_medio_venda = scales::comma(x = valor_medio_venda,
                                           accuracy = 0.01,
                                           prefix = "$ ",
                                           big.mark = ".",
                                           decimal.mark = ",",
                                           trim = FALSE)
      )
}
