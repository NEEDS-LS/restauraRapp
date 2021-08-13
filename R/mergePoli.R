#' Dissolve os poligonos intersectados
#'
#' Essa função dissolve os poligonos que por acaso estejam intersectados depois dos recortes com o CAR e os buffers
#' @param buff ????
#' @return buffdis Os poligonos com as intersecções dissolvidas
#' @export
#' @examples
#' mergePoli<-function(buff)


mergePoli<-function(buff){
  
  gt <- gIntersects(buff, byid = TRUE, returnDense = FALSE)
  ut <- unique(gt)
  nth <- 1:length(ut)
  buff$n <- 1:nrow(buff)
  buff$nth <- NA
  for(i in 1:length(ut)){
    x <- ut[[i]]
    buff$nth[x] <- i
  }
  buffdis <- gUnaryUnion(buff, buff$nth)
  return(buffdis)
}