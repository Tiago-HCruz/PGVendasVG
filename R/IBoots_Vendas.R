#' @title Intervalo Bootstrap para vendas
#'
#' @description
#' Função que recebe um quantidade de numero de reamostragem bootsrap e com as
#' seguintes regiões do banco de dados, demostra os seus resultados em
#' um data frame.

#' @param n  numero de reamostragem bootstrap.
#'
#' @returns Um data.frame contendo um intervalo superior e inferior de 95% e a mediana para
#' cada região.
#'
#' @importFrom purrr map list_rbind
#' @export
#'
#' @examples
#' IBoots_Vendas(100)
#'
IBoots_Vendas <- function(n){

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

