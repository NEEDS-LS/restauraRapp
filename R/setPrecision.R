#' Função para correção do erro de topologia após a criação e recorte do buffers
#'
#' Essa função ????
#' @param sp ????
#' @return sp_exploded ????
#' @export
#' @examples
#' setPrecision<-function(sp)




#função para correção do erro de topologia após a criação e recorte do buffers.
setPrecision<-function(sp){
  sp_sf<-as(sp, "sf")
  sp_sf_cast<-st_cast(sp_sf,"POLYGON")
  
  sp_sf_precision<-sp_sf_cast %>% st_set_precision(100) %>% st_make_valid()
  sf_colect_poly<-st_collection_extract(sp_sf_precision, "POLYGON")
  
  #sp_exploded<-as(sf_colect_poly,"Spatial") #caso seja um SPDF
  sp_exploded<-as_Spatial(sf_colect_poly$geom) #caso o objeto a ser corrigido seja um SP
  return(sp_exploded)
}