#' Title diamètre vers cat\u00e9gories de diamètre
#'
#' @encoding UTF-8
#' @param diam vecter de diametre
#'
#' @return vecteurs des cat\u00e9gories
#' @export 
#'
dia2cat <- function(diam) {

cut(as.numeric(diam), breaks = c(17.5, 27.5, 42.5, 62.5, 2000), labels = c("PB", "BM", "GB", "TGB"))
       }
