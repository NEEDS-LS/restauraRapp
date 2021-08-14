#' Função unifica a hidrográfia, cria os buffers e corta com o uso do solo para propriedades pequenas de 1 a 2.
#'
#' Essa função unifica a hidrografia e cria os buffer referentes as áreas a serem
#' restauradas em propriedades pequenas entre 1 e 2 módulos fiscais (15 metros em nascentes e 8 metros nas demais
#' hidrográfias), retornando a intersecção deste buffer com o uso do solo
#' @param mapa_MDA Mapa da hidrográfia referente as Massas d'água.
#' @param mapa_RMS Mapa da hidrográfia referente aos Rios de margem simples.
#' @param mapa_RMD Mapa da hidrográfia referente aos Rios de margem dupla.
#' @param mapa_NAS Mapa da hidrográfia referente as Nascentes.
#' @param pequeno_1_2 Conjunto de propriedades caracterizadas como entre um e dois módulos fiscais, resultantes da função separaTamanho.
#' @param uso Mapa do uso do solo do local
#' @return Objeto SpatialPolygonsDataFrames referente ao uso do solo dentro do buffer criado
#' @export
#' @examples
#' gPeq_1_2<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,Pequena_1_2,uso)
#'

gPeq_1_2<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,pequeno_1_2,uso){

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

  pequeno12_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=8)
  pequeno12_app_original<-gUnion(pequeno12_app_original, mapa_NAS)
  if(!is.null(mapa_hidro_pol)){
  pequeno12_app_original<-gDifference(pequeno12_app_original, mapa_hidro_pol)
  }
  pequeno12_app<-gIntersection(pequeno12_app_original, pequeno_1_2)
  pequeno12_app<-gBuffer(pequeno12_app, byid=TRUE, width=0)
  pequeno12_app<-raster::intersect(uso, pequeno12_app)

  return(pequeno12_app)
}
