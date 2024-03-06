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
#' @param tipo Qual tamanho de propriedade deve ser usado, sendo "micro" para menores que 1 módulo, "Peq1" entre 1 e 2 módulos, "peq2" para 2 a 4 módulos, "grande" para maiores que 4 módulos, "out1" para modelar as áreas sem CAR como micro propriedades e "out2" para modelar as áreas sem car como grandes propriedades.
#' @return Objeto sf referente ao uso do solo dentro do buffer criado.
#' @export
#' @examples
#' APP_micro<-resapp_app_buffer(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,CAR,uso,tipo="micro")
#'
#' #caso não exista Massas d'água e/ou Rios de margem dupla
#' APP_micro<-resapp_app_buffer(mapa_MDA = NULL,mapa_RMS,mapa_RMD = NULL,mapa_NAS,CAR,uso,tipo="micro")
#'

resapp_app_buffer<-function(mapa_MDA = NULL,mapa_RMS,mapa_RMD = NULL,mapa_NAS,CAR,uso,tipo){

  CAR<-res_app_correctpoly(CAR)
  uso<-res_app_correctpoly(uso)

  propriedades<-resapp_car_class(CAR)

  if(!is.null(mapa_RMD)){ #verifica se existe os rios de margem dupla
    mapa_RMD<-res_app_correctpoly(mapa_RMD) #corrige os poligonos
    if(!is.null(mapa_MDA)){ #verifica se existe as massas d'água
      mapa_MDA<-res_app_correctpoly(mapa_MDA) #corrige os poligonos
      mapa_MDA<-mapa_MDA[mapa_MDA$NATUREZA != "artificial",] #separa apenas os de origem natural
        if(length(mapa_MDA$geometry)==0){ #verifica se existe massas d'água de origem natural
          #caso não tenha usa apenas os rios de margem dupla
          mapa_hidro_pol<-mapa_RMD %>% dplyr::select((1:5))
          mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% dplyr::select((1:5)))
        }else{
          #caso tenha junta todos os poligonos pra formar a hidrografia para o buffer
          mapa_hidro_pol<-rbind(mapa_MDA %>% dplyr::select((1:5)), mapa_RMD %>% dplyr::select((1:5)))
          mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% dplyr::select((1:5)))
        }
      }else{
        #Caso as massas d'água sejam NULL, cria a hidrografia apenas com os rios de margem dupla
        mapa_hidro_pol<-mapa_RMD %>% dplyr::select((1:5))
        mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% dplyr::select((1:5)))
      }
    }else{ #aqui é caso não exista rios de margem dupla
      if(!is.null(mapa_MDA)){ #verifica novamente a existencia de massas d'água e corrige
        mapa_MDA<-res_app_correctpoly(mapa_MDA)
        mapa_MDA<-mapa_MDA[mapa_MDA$NATUREZA != "artificial",]
        if(length(mapa_MDA$geometry)==0){
          mapa_hidro<-mapa_RMS %>% dplyr::select((1:5)) #caso não exista MDA natural usa apenas RMS
          mapa_hidro_pol<-NULL
        }else{
          #caso exista cria apenas com as MDA
          mapa_hidro_pol<-mapa_MDA %>% dplyr::select((1:5))
          mapa_hidro<-rbind(mapa_hidro_pol, mapa_RMS %>% dplyr::select((1:5)))
        }
      }else{ #caso tudo seja NULL usa apenas os RMS
        mapa_hidro<-mapa_RMS %>% dplyr::select((1:5))
        mapa_hidro_pol<-NULL
      }
    }


  mapa_NAS<-st_buffer(mapa_NAS %>% dplyr::select((1:5)), 15)

  lista.classe<-NULL

  if(tipo == "tudo"){
    lista.classe<-c("micro","peq1","peq2","grande")
    mapa.geral<-NULL
    tipo<-lista.classe[1]
    i<-1
    prop<-NA
  }

  repeat{
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

    }else if(tipo == "grande"){

      cars<-propriedades[[4]]
      appm<-20
      prop<-"Grande"

    }else if(tipo == "out1"){

      cars<-CAR[CAR$ind_status!="CA",]
      appm<-5
      prop<-"Sem CAR (Micro)"

    }else if(tipo == "out2"){

      cars<-CAR[CAR$ind_status!="CA",]
      appm<-30
      prop<-"Sem CAR (Grande)"

    }

    app_original<-st_buffer(mapa_hidro, appm, endCapStyle = "FLAT")

    app_original<-rbind(app_original, mapa_NAS)
    app_original<-app_original %>% group_by(MUNICIPIO) %>% summarize()

    if(!is.null(mapa_hidro_pol)){
      app_original<-st_difference(app_original, mapa_hidro_pol)
    }

    if(tipo != "out1" & tipo != "out2"){
      rec_app<-st_intersection(app_original, cars)
      rec_app<-st_buffer(rec_app, 0)
    }else{
      cars<-cars %>% group_by(municipio) %>% summarise()
      rec_app<-st_difference(app_original, cars)
      rec_app<-st_buffer(rec_app, 0)
    }

    uso_app<-st_intersection(uso, rec_app)
    #uso_app<-uso_app %>% group_by(CLASSE_USO) %>% summarize()
    uso_app<-st_collection_extract(uso_app, "POLYGON") %>% group_by(CLASSE_USO) %>% summarise()
    uso_app$C_PROP<-prop
    uso_app<-uso_app %>% rename ("C_USO" = "CLASSE_USO")
    uso_app$MUN<-unique(CAR$municipio)
    uso_app$AREA_HA <- round(as.numeric(st_area(uso_app)/10000),2)
    uso_app$C_USO<-rm_accent(uso_app$C_USO)
    uso_app<-st_as_sf(as.data.frame(uso_app))

    if(is.null(lista.classe)){
      return(uso_app)
      break
    }else if(i < 4){
      if(is.null(mapa.geral)){
        mapa.geral<-uso_app
      }else{
        mapa.geral<-rbind(mapa.geral,uso_app)
      }
      i<-i+1
      tipo<-lista.classe[i]
    }else if( i == 4){
      mapa.geral<-rbind(mapa.geral,uso_app)
      return(mapa.geral)
      break
    }

  }
}


res_app_correctpoly<-function(poly){
  if(!is.null(poly)){
    poly<-st_buffer(poly, 0)
  }
  return(poly)
}

