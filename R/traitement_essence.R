#' traitement essence
#'
#' @param tab fichier import\u00e9
#' @param duree duree de rotation
#' @param surface surface de la parcelle
#'
#' @return table des accroissement et des passage à la futaie par essence
#' @export
#' @importFrom dplyr mutate summarise group_by left_join
#' 
traitement_essence <- function(tab, duree, surface){
  if (is.numeric(req(tab$dia1)) == TRUE ) {
  tabaccdiam <- tab[!(is.na(tab$dia2) | tab$dia2 == 0),] 
    mutate(tabaccdiam, cat1 = dia2cat(dia1)) %>% 
    group_by(ess, cat1) %>%
    summarise(accdiam = mean(dia2-dia1))
  
  
  tab %>% mutate(cat1 = dia2cat(dia1)) %>% 
    left_join(tabaccdiam) %>% 
    mutate(dia1 = ifelse(is.na(dia1), 0, dia1),
           dia2 = ifelse(is.na(dia2), dia1+accdiam/2, dia2),
           ispf = ifelse(dia1<17.5 & dia2>=17.5, 1, 0)) %>%  
    group_by(ess) %>%
    summarise(
      pf = sum(ispf, na.rm = T),
      acc = sum(((dia2/100)^2 - (dia1/100)^2) * pi/4/surface/duree, na.rm = T) 
    )
  
  }
  else {print("dia1 manquant")}
}

