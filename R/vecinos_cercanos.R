#' Vecinos mas cercanos
#'
#' La función calcula los k-vecinos mas cercanos
#'
#' @param id vector del identificador numerico del arbol
#' @param sp vector de las especies de los arboles
#' @param size vector con las dimensiones del diametro de los arboles
#' @param x1 vector de las coordenadas x de los arboles
#' @param y1 vector de las coordenadas y de los arboles
#' @param k valor numerico que identifica el numero de vecinos cercanos
#'
#' @return Regresa cuatro objetos en forma matricial de las distancias, especies, id's y el tamano de los arboles
#' @export vecinos_cercanos
#'
vecinos_cercanos <- function(id, sp, size, x1, y1, k){

  n = length(id)
  d = rep(0, n*n)
  vsp = rep("", n*n)
  vnsp = as.integer(rep(0, n*n))
  vsize = rep(0, n*n)
  d1 = rep(0, n*n)
  vsp1 = rep("", n*n)
  vnsp1 = as.integer(rep(0, n*n))
  vsize1 = rep(0, n*n)

  out <- .C("vecinos_cercanos", as.integer(n), as.integer(k), id, sp, size, x1, y1,
            d, vsp, vnsp, vsize, d1=d1, vsp1=vsp1, vnsp1=vnsp1, vsize1=vsize1)

  return(list(dist = matrix(round(out$d1,3), ncol = n)[2:(k+1),],
              spp = matrix(out$vsp1, ncol = n)[2:(k+1),],
              id_arb = matrix(out$vnsp1, ncol = n)[2:(k+1),],
              size_arb = matrix(round(out$vsize1,3), ncol = n)[1:(k+1),]))

}
