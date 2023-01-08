#' Indice de mezcla de especies (Mingling index of Gadow)
#'
#' La función calcula el índice de mezcla de especies para los arboles que se encuentran en el sitio
#'
#' @param id vector del identificador numerico del arbol
#' @param sp vector de las especies de los arboles
#' @param size vector con las dimensiones del diametro de los arboles
#' @param x1 vector de las coordenadas x de los arboles
#' @param y1 vector de las coordenadas y de los arboles
#' @param k valor numerico que identifica el numero de vecinos cercanos
#'
#' @return Regresa un data.frame con los id's, especies, el tamano y el indice de mezcla de especies de los arboles
#' @export mezcla_indice_arbol
#'
mezcla_indice_arbol <- function(id, sp, size, x1, y1, k){

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

  out <- .C("mezcla_indice_arbol", as.integer(n), as.integer(k), id, sp, size, x1, y1,
            d, vsp, vnsp, vsize, d1=d1, vsp1=vsp1, vnsp1=vnsp1, vsize1=vsize1,
            indice=indice)

  r <- function(x){

    for (j in 1:n) {

      suma = 0

      for (i in 2:(k+1)) {

        if (x[(j-1)*n+1] != x[(j-1)*n+i]) {

          suma = suma + 1;

        }

      }

      indice[j] = suma / k

    }

    return(indice)

  }

  return(df = data.frame(id, sp, size, indice = r(out$vsp1)))

}
