#' Funcao de Analise da Base de Dados AMES
#'
#' Esta funcao conta o numero de dados nao disponiveis em uma determinada variavel de uma determinada base de dados. Tambem calcula a proporcao de dados nao disponiveis desta variavel.
#'
#' @param tab Tabela ou base de dados que contem a variavel a ser analisada.
#' @param nome_coluna Variavel a ser analisada.
#' @param prop Opcao de fazer o calculo da proporcao de dados nao disponiveis na variavel.
#'
#' @return Se prop = FALSE, retorna o numero de observacoes com dados nao disponiveis. Caso prop = TRUE, retorna a proporcao de dados nao disponiveis na variavel.
#'
#' @export
pegar_num_NAs <- function(tab, nome_coluna, prop = FALSE) {
   com_na <- tab %>%
      dplyr::filter_at(dplyr::vars(nome_coluna), is.na) %>%
      dplyr::count() %>%
      as.integer()

   total <- tab %>%
      dplyr::select(nome_coluna) %>%
      dplyr::count() %>%
      as.integer()

   if (prop) {
      return(com_na / total)
   } else {
      return(com_na)
   }
}
