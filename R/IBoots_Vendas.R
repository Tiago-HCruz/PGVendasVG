#' @title Intervalo Bootstrap para vendas
#'
#' @description
#' Função recebe um vetor com vendas por categorias e retorna o intervalo de bootstrap, para a média
#' de vendas para cada uma das regiões de vendas.

#' @param n  numero de reamostragem bootstrap.
#' @param amostra amostra de vendas de uma região, argumento deve ser um vetor.
#'
#' @returns Um vetor contendo um intervalo superior e inferior de 95% e a mediana.
#'
#' @importFrom purrr map list_rbind
#' @export
#'
#' @examples
#' # O Banco de dados precisa estar classificado como 'data.frame',
#' # isso é, esse é o motivo de usar a função "as.data.frame()"
#' IBoots_Vendas(100, as.data.frame(Dados)$NA_Sales)
#'
IBoots_Vendas <- function(n, amostra){

  ic <- function(n, amostra){
    tam_amostra <-length(amostra)
  matrix_boot <- matrix(sample(amostra, size = n*tam_amostra, replace = T) ,
                        ncol = n , nrow = tam_amostra)

  esta_boot <- cbind(quantile(colMeans(matrix_boot) , prob = 0.025),
                    quantile(colMeans(matrix_boot) , prob = 0.50),
                    quantile(colMeans(matrix_boot) , prob = 0.975))
  return(esta_boot)
  }

  Dados <- as.data.frame(Dados) |>
    dplyr::select("Platform", "Genre", "Rating", "NA_Sales", "EU_Sales",
                  "JP_Sales", "Other_Sales", "Global_Sales") |>
    na.omit(Dados)

  ic_list <-map(c( "NA_Sales", "EU_Sales","JP_Sales", "Other_Sales", "Global_Sales")
                , function(x) as.data.frame(ic(n, Dados[,x])))
  ic_df <- list_rbind(ic_list)
  ic_df<-cbind(ic_df , c( "América do Norte", "Estados Unidos", "Japão",
                          "Outros", "Global"))

  names(ic_df) <-c("Limite inferior ","Mediana", "Limite superior",  "País")
  row.names(ic_df) <-c()

  #print(ic_df)
  return(ic_df)

}

