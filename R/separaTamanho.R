#' Função separa as propriedades por tamanho baseado no numero de modulos fiscais
#'
#' Essa função separa as propriedades por tamanho baseado no número de módulos fiscais
#' @param mapa_CAR Conjunto de propriedades registradas que serão utilizadas na definição das APPS
#' @return lista Uma lista de SpatialPolygonsDataFrames referentes as propriedades separados por tamanho.
#' @export
#' @examples
#' 
#' props<-separaTamanho(mapa_CAR)
#' micro<-props[[1]]
#' peq12<-props[[2]]
#' peq24<-props[[3]]
#' media<-props[[4]]
#' grand<-props[[5]]
 
separaTamanho<-function(mapa_CAR){
  
  mapa_CAR<-gBuffer(mapa_CAR, byid=TRUE, width=0)
  
  micro<-mapa_CAR[mapa_CAR@data$NUM_MODULO<1,]
  pequena_1_2<-mapa_CAR[mapa_CAR@data$NUM_MODULO>=1 & mapa_CAR@data$NUM_MODULO<2,]
  pequena_2_4<-mapa_CAR[mapa_CAR@data$NUM_MODULO>=2 & mapa_CAR@data$NUM_MODULO<4,]
  media<-mapa_CAR[mapa_CAR@data$NUM_MODULO>=4 & mapa_CAR@data$NUM_MODULO<10,]
  grande<-mapa_CAR[mapa_CAR@data$NUM_MODULO>=10,]
  
  nm<-c("micro", "pequena_1_2", "pequena_2_4", "media", "grande")
  lista<-list(micro, pequena_1_2, pequena_2_4, media, grande, "names" = nm)
  return(lista)
}