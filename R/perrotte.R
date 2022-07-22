#' formule de perotte
#'
#' @param diam vecteur des diametres des arbres en mètre
#' 
#' @param tarif numéro dutarif algan
#'
#' @return vecteurs volume des arbres en m^3
#' @export 
#'
perrotte <- function(diam, tarif) {
  
  ((0.312384-4.51338*(diam/100)+23.8866*(diam/100)^2-2.89769*(diam/100)^3)*(8+tarif)/28)
}