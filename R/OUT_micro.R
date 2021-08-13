#' Função unifica a hidrográfia, cria os buffers e corta com o uso do solo.
#'
#' Essa função unifica a hidrografia e cria os buffer referentes as áreas a serem 
#' restauradas fora das propriedades registradas no CAR, de forma que seja possível 
#' estimar a quantidade que precisa ser restaurada, retornando a intersecção deste
#' buffer com o uso do solo, considerando toda a área como micro propriedade.
#' @param mapa_MDA Mapa da hidrográfia referente as Massas d'água.
#' @param mapa_RMS Mapa da hidrográfia referente aos Rios de margem simples.
#' @param mapa_RMD Mapa da hidrográfia referente aos Rios de margem dupla.
#' @param mapa_NAS Mapa da hidrográfia referente as Nascentes.
#' @param mapa_CAR Conjunto de propriedades registradas no SICAR 
#' @param uso Mapa do uso do solo do local
#' @param lim Limite do município em análise.
#' @return Objeto SpatialPolygonsDataFrames referente ao uso do solo dentro do buffer criado
#' @export
#' @examples
#' 
#' exemplo<-OUT_micro(mapa_MDA, mapa_RMS, mapa_RMD, mapa_NAS, mapa_CAR, uso, lim)
#' exemplo_restaurar<-areaRestaurar(exemplo)
#' exemplo_vegetação<-areaVegetacao(exemplo)

OUT_micro<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,mapa_CAR,uso,lim){
  
  mapa_MDA<-mapa_MDA[mapa_MDA@data$AREA_HA > 1,]
  mapa_MDA<-gBuffer(mapa_MDA, byid=TRUE, width=0)
  mapa_RMD<-gBuffer(mapa_RMD, byid=TRUE, width=0)
  mapa_NAS<-gBuffer(mapa_NAS, byid=TRUE, width = 15)

  mapa_hidro_pol<-gUnion(mapa_MDA, mapa_RMD)
  mapa_hidro<-gUnion(mapa_hidro_pol, mapa_RMS)

  micro_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=5)
  micro_app_original<-gUnion(micro_app_original, mapa_NAS)
  micro_app_original<-gDifference(micro_app_original, mapa_hidro_pol)
  micro_app_original<-gDifference(micro_app_original, lim)

  out_allmicro<-gDifference(micro_app_original, mapa_CAR)
  out_allmicro<-gBuffer(out_allmicro, byid=TRUE, width=0)
  out_allmicro<-raster::intersect(mapa_USO, out_allmicro)
  out_allmicro<-makeUniform(out_allmicro)
  
  return(out_allmicro)

  }