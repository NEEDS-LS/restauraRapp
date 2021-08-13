#' Organiza os IDs dos poligonos 2
#'
#' Essa função evita que os poligonos tenham o mesmo ID (Corrigir)
#' @param spdf O spatialpolygonsdataframe original
#' @return A area com os IDs corrigidos newspdf
#' @export
#' @examples
#' makeUniformID<-function(spdf)


makeUniformID<-function(spdf){
  pref<-substitute(spdf)  #just putting the file name in front.
  newspdf<-spChFIDs(spdf,as.character(paste(pref,sapply(slot(spdf, "polygons"), function(x) slot(x, "ID")),sep="_")))
  return(newspdf)
}

