#' Função unifica a hidrográfia, cria os buffers e corta com o uso do solo para as propriedades
#'
#' Essa função unifica a hidrografia e cria os buffer referentes as áreas a serem
#' restauradas nas propriedades (com o tamanho da área de acordo com o tipo de propriedade selecionaldo),
#' retornando a intersecção deste buffer com o uso do solo
#' @param mapa_MDA Mapa da hidrográfia referente as Massas d'água.
#' @param mapa_RMS Mapa da hidrográfia referente aos Rios de margem simples.
#' @param mapa_RMD Mapa da hidrográfia referente aos Rios de margem dupla.
#' @param mapa_NAS Mapa da hidrográfia referente as Nascentes.
#' @param CAR Conjunto de propriedades do CAR
#' @param uso Mapa do uso do solo do local
#' @param tipo Qual tamanho de propriedade deve ser usado, sendo "micro" para menores que 1 módulo, "Peq1" entre 1 e 2 módulos, "peq2" para 2 a 4 módulos, "media" entre 4 a 10 módulos e "grande" para maiores que 10 módulos
#' @return Objeto SpatialPolygonsDataFrames referente ao uso do solo dentro do buffer criado
#' @export
#' @examples
#' gCARapp<-function(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,micro,uso)
#'

gCARapp<-function(mapa_MDA,mapa_RMS,mapa_RMD = NULL,mapa_NAS,CAR,uso,tipo){

  propriedades<-separaTamanho(CAR)
  if(tipo != "media"){
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
  }else{
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
    }

  mapa_NAS<-gBuffer(mapa_NAS, byid=TRUE, width = 15)

  if(tipo == "micro"){

    micro<-propriedades[[1]]

    micro_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=5)
    micro_app_original<-gUnion(micro_app_original, mapa_NAS)
    if(!is.null(mapa_hidro_pol)){
      micro_app_original<-gDifference(micro_app_original, mapa_hidro_pol)
    }
    micro_app<-gIntersection(micro_app_original, micro)
    micro_app<-gBuffer(micro_app, byid=TRUE, width=0)
    micro_app<-raster::intersect(uso, micro_app)

    return(micro_app)

  }else if(tipo == "peq1"){

    pequeno_1_2<-propriedades[[2]]

    pequeno12_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=8)
    pequeno12_app_original<-gUnion(pequeno12_app_original, mapa_NAS)
    if(!is.null(mapa_hidro_pol)){
      pequeno12_app_original<-gDifference(pequeno12_app_original, mapa_hidro_pol)
    }
    pequeno12_app<-gIntersection(pequeno12_app_original, pequeno_1_2)
    pequeno12_app<-gBuffer(pequeno12_app, byid=TRUE, width=0)
    pequeno12_app<-raster::intersect(uso, pequeno12_app)

    return(pequeno12_app)

  }else if(tipo == "peq2"){

    pequeno_2_4<-propriedades[[3]]

    pequeno24_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=15)
    pequeno24_app_original<-gUnion(pequeno24_app_original, mapa_NAS)
    if(!is.null(mapa_hidro_pol)){
      pequeno24_app_original<-gDifference(pequeno24_app_original, mapa_hidro_pol)
    }
    pequeno24_app<-gIntersection(pequeno24_app_original, pequeno_2_4)
    pequeno24_app<-gBuffer(pequeno24_app, byid=TRUE, width=0)
    pequeno24_app<-raster::intersect(mapa_USO, pequeno24_app)

    return(pequeno24_app)

  }else if(tipo == "media"){

    media<-propriedades[[4]]

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
  }else if(tipo == "grande"){

    grande<-propriedades[[5]]

    grande_app_original<-gBuffer(mapa_hidro, byid=TRUE, width=30)
    grande_app_original<-gUnion(grande_app_original, mapa_NAS)
    if(!is.null(mapa_hidro_pol)){
      grande_app_original<-gDifference(grande_app_original, mapa_hidro_pol)
    }
    grande_app<-gIntersection(grande_app_original, grande)
    grande_app<-gBuffer(grande_app, byid=TRUE, width=0)
    grande_app<-raster::intersect(mapa_USO, grande_app)


    return(grande_app)

  }


}
