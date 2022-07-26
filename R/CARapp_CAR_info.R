#' Função que calcula e retorna algumas informações referentes ao CAR por classe de propriedade.
#'
#' Essa função tem como objetivo retornar de forma rápida informações do CAR, como número de propriedades
#' e área ocupada por classe de tamanho, tamanho médio e desvio padrão.
#'
#' @param CAR Objeto contendo as informações do CAR obtidas através do SICAR.
#' @return Objeto data frame referente às propriedades cadastradas no SICAR
#' @export
#' @examples
#' df.CAR<-CARapp_CAR_info(CAR)
#'
#'

CARapp_CAR_info<-function(CAR){
  CAR<-CAR[CAR$SITUACAO != "CA",]

  CAR<-CAR %>% mutate(classe = case_when(NUM_MODULO < 1 ~ "Micro",
                                          NUM_MODULO >= 1 & NUM_MODULO < 2 ~
                                            "Pequena (1 a 2 módulos)",
                                          NUM_MODULO >= 2 & NUM_MODULO < 4 ~
                                            "Pequena (2 a 4 módulos)",
                                          NUM_MODULO >= 4 & NUM_MODULO < 10 ~ "Média",
                                          NUM_MODULO >= 10 ~ "Grande"))
  car.map<-CAR %>% group_by(classe) %>% summarise(num_prop = n(),
                                                  tam_medio_ha = mean(NUM_AREA),
                                                  desvio_ha = sd(NUM_AREA))
  car.map<-st_make_valid(car.map)
  car.map$area_ha<-as.numeric(units::set_units(st_area(car.map), value = ha))

  car.map<-st_drop_geometry(car.map)

  return(car.map)
}
