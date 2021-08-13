#' Função unifica a hidrográfia, cria os buffers e corta com o uso do solo.
#'
#' Essa função unifica a hidrografia e cria os buffer referentes as áreas a serem 
#' restauradas em micro propriedades (15 metros em nascentes e 5 metros nas demais 
#' hidrográfias), retornando a intersecção deste buffer com o uso do solo
#' @param mapa_MDA Mapa da hidrográfia referente as Massas d'água.
#' @param mapa_RMS Mapa da hidrográfia referente aos Rios de margem simples.
#' @param mapa_RMD Mapa da hidrográfia referente aos Rios de margem dupla.
#' @param mapa_NAS Mapa da hidrográfia referente as Nascentes.
#' @param micro Conjunto de propriedades caracterizadas como micro, resultantes da função 
#' @param uso Mapa do uso do solo do local
#' @return Objeto SpatialPolygonsDataFrames referente ao uso do solo dentro do buffer criado
#' @export
#' @examples
#' gHidro<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,micro,uso)
#' 

gMicro<-function(mapa_MDA,mapa_RMS,mapa_RMD = NULL,mapa_NAS,micro,uso){
  
  if(!is.null(mapa_RMD)){
    mapa_RMD<-gBuffer(mapa_RMD, byid=TRUE, width=0)
  
    mapa_MDA<-mapa_MDA[mapa_MDA@data$AREA_HA > 1,]
  
    if(length(mapa_MDA@polygons)==0){ 
      mapa_hidro_pol<-mapa_RMD
      mapa_hidro<-gUnion(mapa_hidro_pol, mapa_RMS)
    }else{
      mapa_MDA<-gBuffer(mapa_MDA, byid=TRUE, width=0)
      mapa_hidro_pol<-gUnion(mapa_MDA, mapa_RMD)
      mapa_hidro<-gUnion(mapa_hidro_pol, mapa_RMS)
  }}else{
    mapa_MDA<-mapa_MDA[mapa_MDA@data$AREA_HA > 1,]
    if(length(mapa_MDA@polygons)==0){ 
      mapa_hidro<-mapa_RMS
      mapa_hidro_pol<-NULL
    }else{
      mapa_MDA<-gBuffer(mapa_MDA, byid=TRUE, width=0)
      mapa_hidro_pol<-mapa_MDA
      mapa_hidro<-gUnion(mapa_hidro_pol, mapa_RMS)
    }
  }

  mapa_NAS<-gBuffer(mapa_NAS, byid=TRUE, width = 15)
  
  micro_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=5)
  micro_app_original<-gUnion(micro_app_original, mapa_NAS)
  if(!is.null(mapa_hidro_pol)){
  micro_app_original<-gDifference(micro_app_original, mapa_hidro_pol)
  }
  micro_app<-gIntersection(micro_app_original, micro)
  micro_app<-gBuffer(micro_app, byid=TRUE, width=0)
  micro_app<-raster::intersect(uso, micro_app)
  
  return(micro_app)
}