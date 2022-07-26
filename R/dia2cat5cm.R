#' Title diamètre vers categories de diamètre de 5 en 5
#'
#' @param diam vecteur de diametre
#'
#' @return tableau de la surface terrière selon categories de diamètre 5cm en 5cm
#' @export 
#' @import dplyr
#' @import magrittr
#' 
dia2cat5cm <- function(diam) {
  
  cut <- cut(as.numeric(diam), breaks = seq(from = 15, to = 100, by = 5), labels = c("15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","95")) 
G <-  (as.numeric(diam) / 100 )^2 /4 * pi  
data.frame(cut,G)

}
