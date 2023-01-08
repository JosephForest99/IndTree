#' Distancias entre arboles
#'
#' La función calcula la distancias euclidianas entre arboles
#'
#' @param id vector del identificador numerico del arbol
#' @param sp vector de las especies de los arboles
#' @param size vector con las dimensiones del diametro de los arboles
#' @param x1 vector de las coordenadas x de los arboles
#' @param y1 vector de las coordenadas y de los arboles
#'
#' @return Regresa cuatro objetos en forma matricial de las distancias, especies, id's y el tamano de los arboles
#' @export NN_distances
#'
NN_distances <- function(id, sp, size, x1, y1){

  n = length(id)
  d = rep(0, n*n)
  vsp = rep("", n*n)
  vnsp = as.integer(rep(0, n*n))
  vsize = rep(0, n*n)

  out <- .C("NN_distances", as.integer(n), id, sp, size, x1, y1,
            d=d, vsp=vsp, vnsp=vnsp, vsize=vsize)

  return(list(dist = matrix(round(out$d,3), ncol = n),
              spp = matrix(out$vsp, ncol = n),
              id_arb = matrix(out$vnsp, ncol = n),
              size_arb = matrix(round(out$vsize,3), ncol = n)))

}
