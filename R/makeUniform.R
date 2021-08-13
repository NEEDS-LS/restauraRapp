#' Organiza os IDs dos poligonos
#'
#' Essa função evita que os poligonos tenham o mesmo ID
#' @param spdf O spatialpolygonsdataframe original
#' @return A area com os IDs corrigidos newspdf
#' @export
#' @examples
#' makeUniform<-function(spdf)



makeUniform<-function(spdf){
  pref<-substitute(spdf)  #just putting the file name in front.
  newspdf<-spChFIDs(spdf,as.character(paste(pref,rownames(as(spdf,"data.frame")),sep="_")))
  return(newspdf)
}