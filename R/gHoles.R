#' Função para identificar os holes nos fragmentos
#'
#' Essa função identifica os buracos (donut shape polygons) nos fragmentos
#' @param X Spdf
#' @return Y Spdf
#' @export
#' @examples
#' holes.shp<-gHoles(shp)
#'

gHoles <- function(X, Y) {
  lss <-  gDifference(X, gUnionCascaded(Y), byid = TRUE)
  SpatialPolygons(c(if(is.null(lss)) NULL else lss@polygons))
}
