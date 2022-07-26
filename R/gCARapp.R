#' Função unifica a hidrográfia, cria os buffers e corta com o uso do solo para as propriedades
#'
#' Essa função unifica a hidrografia e cria os buffer referentes as áreas a serem
#' restauradas nas propriedades (com o tamanho da área de acordo com o tipo de propriedade
#' selecionaldo), retornando a intersecção deste buffer com o uso do solo
#' @param mapa_MDA Mapa da hidrográfia referente as Massas d'água, pode ser NULL.
#' @param mapa_RMS Mapa da hidrográfia referente aos Rios de margem simples.
#' @param mapa_RMD Mapa da hidrográfia referente aos Rios de margem dupla, pode ser NULL.
#' @param mapa_NAS Mapa da hidrográfia referente as Nascentes.
#' @param CAR Shapefile com poligonos das propriedades do CAR.
#' @param uso Mapa do uso do solo do local
#' @param tipo Qual tamanho de propriedade deve ser usado, sendo "micro" para menores que 1 módulo, "Peq1" entre 1 e 2 módulos, "peq2" para 2 a 4 módulos, "media" entre 4 a 10 módulos, "grande" para maiores que 10 módulos, "out1" para modelar as áreas sem CAR como micro propriedades e "out2" para modelar as áreas sem car como grandes propriedades.
#' @return Objeto sf referente ao uso do solo dentro do buffer criado.
#' @export
#' @examples
#' APP_micro<-gCARapp(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,CAR,uso,tipo="micro")
#'
#' #caso não exista Massas d'água e/ou Rios de margem dupla
#' APP_micro<-gCARapp(mapa_MDA = NULL,mapa_RMS,mapa_RMD = NULL,mapa_NAS,CAR,uso,tipo="micro")
#'

gCARapp<-function(mapa_MDA = NULL,mapa_RMS,mapa_RMD = NULL,mapa_NAS,CAR,uso,tipo){
# mapa_MDA<-mun_MDA
# mapa_RMS<-mun_RMS
# mapa_RMD<-mun_RMD
# mapa_NAS<-mun_NAS
# CAR<-mun_CAR
# uso<-mun_USO
# tipo<-"out1"

  propriedades<-separaTamanho(CAR)
  if(tipo != "media"){
  if(!is.null(mapa_RMD)){
    mapa_RMD<-st_buffer(mapa_RMD, 0)

    if(!is.null(mapa_MDA)){
    mapa_MDA<-mapa_MDA[mapa_MDA$AREA_HA > 1,]

    if(length(mapa_MDA$geometry)==0){
      mapa_hidro_pol<-mapa_RMD %>% select((1:5))
      mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% select((1:5)))
    }else{
      mapa_MDA<-st_buffer(mapa_MDA, 0)
      mapa_hidro_pol<-rbind(mapa_MDA %>% select((1:5)), mapa_RMD %>% select((1:5)))
      mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% select((1:5)))
    }
    }else{
      mapa_hidro_pol<-mapa_RMD %>% select((1:5))
      mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% select((1:5)))
    }
    }else{
      if(!is.null(mapa_MDA)){
      mapa_MDA<-mapa_MDA[mapa_MDA$AREA_HA > 1,]
      if(length(mapa_MDA$geometry)==0){
        mapa_hidro<-mapa_RMS %>% select((1:5))
        mapa_hidro_pol<-NULL
      }else{
        mapa_MDA<-st_buffer(mapa_MDA, 0)
        mapa_hidro_pol<-mapa_MDA %>% select((1:5))
        mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% select((1:5)))
      }
      }else{
        mapa_hidro<-mapa_RMS %>% select((1:5))
        mapa_hidro_pol<-NULL
      }
    }
  }else{
    if(!is.null(mapa_RMD)){
      mapa_RMD<-st_buffer(mapa_RMD, 0)

      if(!is.null(mapa_MDA)){
      mapa_MDA<-mapa_MDA[mapa_MDA$AREA_HA > 1,]

        if(length(mapa_MDA$geometry)==0){
          mapa_hidro_pol<-mapa_RMD %>% select((1:5))
          media_poli<-st_buffer(mapa_hidro_pol, 30)
        }else{
          mapa_MDA<-st_buffer(mapa_MDA, 0)
          mapa_hidro_pol<-rbind(mapa_MDA %>% select((1:5)), mapa_RMD %>% select((1:5)))
          media_poli<-st_buffer(mapa_hidro_pol, 30)
        }
      }else{
        mapa_hidro_pol<-mapa_RMD %>% select((1:5))
        media_poli<-st_buffer(mapa_hidro_pol, 30)
      }
    }else{
      if(!is.null(mapa_MDA)){
        mapa_MDA<-mapa_MDA[mapa_MDA$AREA_HA > 1,]
        if(length(mapa_MDA$geometry)==0){
          mapa_hidro_pol<-NULL
        }else{
          mapa_MDA<-st_buffer(mapa_MDA, 0)
          mapa_hidro_pol<-mapa_MDA %>% select((1:5))
          media_poli<-st_buffer(mapa_hidro_pol, 30)
        }
      }else{
        mapa_hidro_pol<-NULL
      }
    }
  }

  mapa_NAS<-st_buffer(mapa_NAS %>% select((1:5)), 15)

  if(tipo == "micro"){

    cars<-propriedades[[1]]
    appm<-5
    prop<-"Micro"

  }else if(tipo == "peq1"){

    cars<-propriedades[[2]]
    appm<-8
    prop<-"Pequenas 1 a 2 modulos"

  }else if(tipo == "peq2"){

    cars<-propriedades[[3]]
    appm<-15
    prop<-"Pequenas 2 a 4 modulos"

  }else if(tipo == "media"){

    cars<-propriedades[[4]]
    appm<-20
    prop<-"Media"

  }else if(tipo == "grande"){

    cars<-propriedades[[5]]
    appm<-30
    prop<-"Grande"

  }else if(tipo == "out1"){

    cars<-CAR[CAR$SITUACAO!="CA",]
    appm<-5
    prop<-"Sem CAR (Micro)"

  }else if(tipo == "out2"){

    cars<-CAR[CAR$SITUACAO!="CA",]
    appm<-30
    prop<-"Sem CAR (Grande)"

  }

  if(tipo != "media"){
    app_original<-st_buffer(mapa_hidro, appm)
  }else{
    rios<-st_buffer(mapa_RMS %>% select((1:5)), 20)
    if(!is.null(mapa_hidro_pol)){
    app_original<-rbind(media_poli, rios)
    }else{app_original<-rios}
  }

  app_original<-rbind(app_original, mapa_NAS)
  app_original<-app_original %>% group_by(MUNICIPIO) %>% summarize()

  if(!is.null(mapa_hidro_pol)){
    app_original<-st_difference(app_original, mapa_hidro_pol)
  }

#as diferentes geometria aparecem a partir do recorte entre app_original e cars.
  if(tipo != "out1" & tipo != "out2"){
    rec_app<-st_intersection(app_original, cars)
    rec_app<-st_buffer(rec_app, 0)
  }else{
    cars<-cars %>% group_by(NOM_MUNICI) %>% summarise()
    rec_app<-st_difference(app_original, cars)
    rec_app<-st_buffer(rec_app, 0)
  }

  uso_app<-st_intersection(uso, rec_app)
  #uso_app<-uso_app %>% group_by(CLASSE_USO) %>% summarize()
  uso_app<-st_collection_extract(uso_app, "POLYGON") %>% group_by(CLASSE_USO) %>% summarise()
  uso_app$C_PROP<-prop
  uso_app$MUN<-unique(CAR$NOM_MUNICI)
  uso_app$AREA_HA <- round(as.numeric(st_area(uso_app, by_feature = TRUE)/10000),2)
  uso_app<-st_as_sf(as.data.frame(uso_app))
  uso_app<-uso_app %>% rename ("C_USO" = "CLASSE_USO")

return(uso_app)
}
