#' Title diametre vers categorie de diametre
#'
#' @encoding UTF-8
#' @param diam vecteur de diametre
#'
#' @return vecteurs des categories
#' @export 
#'
dia2cat <- function(diam) {

cut(as.numeric(diam), breaks = c(17.5, 27.5, 42.5, 62.5, 2000), labels = c("PB", "BM", "GB", "TGB"))
       }
