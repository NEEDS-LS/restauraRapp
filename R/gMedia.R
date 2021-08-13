#' Função unifica a hidrográfia, cria os buffers e corta com o uso do solo.
#'
#' Essa função unifica a hidrografia e cria os buffer referentes as áreas a serem 
#' restauradas em propriedades de tamanho médio, entre quatro e dez módulos fiscais, 
#'(15 metros em nascentes, 20 metros nos rios de margem simples e 30 nas demais
#'hidrográfias), retornando a intersecção deste buffer com o uso do solo
#' @param mapa_MDA Mapa da hidrográfia referente as Massas d'água.
#' @param mapa_RMS Mapa da hidrográfia referente aos Rios de margem simples.
#' @param mapa_RMD Mapa da hidrográfia referente aos Rios de margem dupla.
#' @param mapa_NAS Mapa da hidrográfia referente as Nascentes.
#' @param media Conjunto de propriedades caracterizadas como média, resultantes da função separaTamanho.
#' @param uso Mapa do uso do solo do local
#' @return Objeto SpatialPolygonsDataFrames referente ao uso do solo dentro do buffer criado
#' @export
#' @examples
#' gMedia<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,media,uso)
#' 

gMedia<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,media,uso){
  
  if(!is.null(mapa_RMD)){
    mapa_RMD<-gBuffer(mapa_RMD, byid=TRUE, width=0)
    
    mapa_MDA<-mapa_MDA[mapa_MDA@data$AREA_HA > 1,]
    
    if(length(mapa_MDA@polygons)==0){ 
      mapa_hidro_pol<-mapa_RMD
      media_poli<-gBuffer(mapa_hidro_pol, byid=TRUE, width=30)
    }else{
      mapa_MDA<-gBuffer(mapa_MDA, byid=TRUE, width=0)
      mapa_hidro_pol<-gUnion(mapa_MDA, mapa_RMD)
      media_poli<-gBuffer(mapa_hidro_pol, byid=TRUE, width=30)
    }}else{
      mapa_MDA<-mapa_MDA[mapa_MDA@data$AREA_HA > 1,]
      if(length(mapa_MDA@polygons)==0){ 
        mapa_hidro_pol<-NULL
      }else{
        mapa_MDA<-gBuffer(mapa_MDA, byid=TRUE, width=0)
        mapa_hidro_pol<-mapa_MDA
        media_poli<-gBuffer(mapa_hidro_pol, byid=TRUE, width=30)
      }
    }
  
  mapa_NAS<-gBuffer(mapa_NAS, byid=TRUE, width = 15)
  
  media_rios<-gBuffer(mapa_RMS, byid=TRUE, width=20)
  if(!is.null(mapa_hidro_pol)){
    media_app_original<-gUnion(media_poli, media_rios)
    media_app_original<-gDifference(media_app_original, mapa_hidro_pol)
  }else{ media_app_original<-media_rios }
  
  media_app_original<-gUnion(media_app_original, mapa_NAS)
  
  media_app<-gIntersection(media_app_original, media)
  media_app<-gBuffer(media_app, byid=TRUE, width=0)
  media_app<-raster::intersect(mapa_USO, media_app)
  
  return(media_app)
}