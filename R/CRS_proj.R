#' Função que reprojeta o ShapeFile usando o wkt fornecido de acordo com a atualização para o PROJ6 e GDAL3.
#'
#' Essa função reprojeta o ShapeFile usando o wkt fornecido de acordo com a atualização para o PROJ6 e GDAL3.
#' @param obj2Reproj Objeto que precisa ser reprojetado
#' @param crs_code Código CRS destino.
#' @return Objeto SpatialPolygonsDataFrames reprojetado.
#' @export
#' @examples
#' meu_shp<-CRS_proj(meu_shp, 31982) #reprojetando para SIRGAS2000/UTM 22S
#' 

CRS_proj<-function(obj2Reproj, crs_code){
  setcrs<-st_crs(crs_code)
  wkt_crs<-st_crs(setcrs$wkt)
  sp_to_sf<-st_as_sf(obj2Reproj)
  sp_to_sf<-st_transform(sp_to_sf, wkt_crs)
  sf_to_sp<-as(sp_to_sf, "Spatial")
  return(sf_to_sp)
}