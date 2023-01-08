#' Indice de diferenciacion de tamanos
#'
#' La función calcula el índice de diferenciacion de tamano para los arboles que se encuentran en el sitio
#'
#' @param id vector del identificador numerico del arbol
#' @param sp vector de las especies de los arboles
#' @param size vector con las dimensiones del diametro de los arboles
#' @param x1 vector de las coordenadas x de los arboles
#' @param y1 vector de las coordenadas y de los arboles
#' @param k valor numerico que identifica el numero de vecinos cercanos
#'
#' @return Regresa un data.frame con los id's, especies, el tamano y el indice de diferenciacion de tamano de los arboles
#' @export tamano_indice_arbol
#'
tamano_indice_arbol <- function(id, sp, size, x1, y1, k){

  n = length(id)
  d = rep(0, n*n)
  vsp = rep("", n*n)
  vnsp = as.integer(rep(0, n*n))
  vsize = rep(0, n*n)
  d1 = rep(0, n*n)
  vsp1 = rep("", n*n)
  vnsp1 = as.integer(rep(0, n*n))
  vsize1 = rep(0, n*n)
  indice = rep(as.double(0), n)

  out <- .C("tamano_indice_arbol", as.integer(n), as.integer(k), id, sp, size, x1, y1,
            d, vsp, vnsp, vsize, d1=d1, vsp1=vsp1, vnsp1=vnsp1, vsize1=vsize1,
            indice=indice)

  return(df = data.frame(id, sp, size, indice = out$indice))
}
