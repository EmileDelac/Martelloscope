#' Nature d eclaircie
#'
#' @param diam vecteur des diametre des arbres du peuplement en metre
#' 
#' @param mart vecteur factoriel des arbres marteles
#' 
#' @param tarif numéro dutarif algan
#' 
#'
#' @return coefficient K de la nature de l eclaircie
#' K = ve/vav où ve est le volume de l'arbre moyen enlevé à l'éclaircie et vav est le volume de l'arbre moyen avant éclaircie.
#'
#'    Si K < 0,7 l'éclaircie est qualifiée de « par le bas »
#'    si K > 1, de « par le haut », et
#'    si 0,7 < K < 1, de « mixte »
#' @export 
#'
keclaircie <- function(diam,mart,tarif) {
  
 diampp <-   ((0.312384-4.51338*(diam/100)+23.8866*(diam/100)^2-2.89769*(diam/100)^3)*(8+tarif)/28)
 
 tab <- data.frame(diampp,mart)
 tab2 <- na.omit(tab[!tab$mart=="NA",])
 
 K <- mean(tab2$diampp)/mean(diampp)
 K
}
