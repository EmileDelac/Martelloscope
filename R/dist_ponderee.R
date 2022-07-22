#' Distance moyenne ponderee
#'
#' @param geom Geometries des arbres = longitude et latitudes
#' 
#' @param diam vecteur des diametres des arbres en mètre
#' 
#' @param tarif numéro dutarif algan
#'
#' @return vecteurs distance moyenne des arbres ponderee par leur volume
#' @export 
#'
dist.pond <- function(geometry, diam, tarif) {
  
lat = unlist(purrr::map(geometry,1))
long = unlist(purrr::map(geometry,2))
dist.mean <- rowMeans(as.matrix(dist(cbind(lat,long),diag = TRUE,upper = TRUE)))
volume <- perrotte(diam,tarif)
dist.mean*volume
}