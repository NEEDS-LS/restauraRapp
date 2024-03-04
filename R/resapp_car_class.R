#' Função separa as propriedades por tamanho baseado no numero de modulos fiscais
#'
#' Essa função separa as propriedades por tamanho baseado no número de módulos fiscais
#' @param mapa_CAR Conjunto de propriedades registradas que serão utilizadas na definição das APPS
#' @return lista referente as propriedades separadas por tamanho.
#' @export
#' @examples
#'
#' props<-resapp_car_class(mapa_CAR)
#' micro<-props[[1]]
#' peq12<-props[[2]]
#' peq24<-props[[3]]
#' grand<-props[[4]]

resapp_car_class<-function(mapa_CAR){

  mapa_CAR<-st_buffer(mapa_CAR, 0)

  mapa_CAR<-mapa_CAR[mapa_CAR$SITUACAO != "CA",]

  micro<-mapa_CAR[mapa_CAR$NUM_MODULO<1,]
  pequena_1_2<-mapa_CAR[mapa_CAR$NUM_MODULO>=1 & mapa_CAR$NUM_MODULO<2,]
  pequena_2_4<-mapa_CAR[mapa_CAR$NUM_MODULO>=2 & mapa_CAR$NUM_MODULO<4,]
  grande<-mapa_CAR[mapa_CAR$NUM_MODULO>=4,]

  nm<-c("Micro", "Pequenas 1 a 2 modulos", "Pequenas 2 a 4 modulos", "Grande")
  lista<-list(micro, pequena_1_2, pequena_2_4, grande, "names" = nm)
  return(lista)
}
