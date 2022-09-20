#' Função que calcula e retorna algumas informações referentes ao CAR por classe de propriedade.
#'
#' Essa função tem como objetivo retornar de forma rápida informações do CAR, como número de propriedades
#' e área ocupada por classe de tamanho, tamanho médio e desvio padrão.
#'
#' @param CAR Objeto contendo as informações do CAR obtidas através do SICAR.
#' @param mun Nome do município foco da análise que será utilizado como limite para o recorte do CAR.
#' @param tipo parâmetro para retornar o resultado, "df" retorna apenas a tabela de dados. Tem como
#' padrão o retorno de um objeto do tipo sf.
#' @return Objeto data frame referente às propriedades cadastradas no SICAR
#' @export
#' @examples
#' df.CAR<-resapp_car_info(CAR)
#'
#'

resapp_car_info<-function(CAR, mun = NULL, tipo = NULL){

  CAR<-CAR[CAR$SITUACAO != "CA",]

  CAR<-CAR %>% mutate(classe = case_when(NUM_MODULO < 1 ~ "Micro",
                                          NUM_MODULO >= 1 & NUM_MODULO < 2 ~
                                            "Pequena (1 a 2 modulos)",
                                          NUM_MODULO >= 2 & NUM_MODULO < 4 ~
                                            "Pequena (2 a 4 modulos)",
                                          NUM_MODULO >= 4 & NUM_MODULO < 10 ~ "Media",
                                          NUM_MODULO >= 10 ~ "Grande"))
  if(!is.null(mun)){
    if(file.exists("./data_use")==FALSE){dir.create("./data_use")}
    if(file.exists("./data_use/gadm36_BRA_2_pk.rds")==FALSE){
      lim<-st_as_sf(gadm(country = "BRA", level = 2, path = "./data_use"))
    }else{
      lim<-st_as_sf(vect(readRDS("./data_use/gadm36_BRA_2_pk.rds")))
    }

    lim<-st_transform(lim,st_crs(CAR)$wkt)

    lim$mun<-rm_accent(lim$NAME_2) %>% tolower()
    nome<-rm_accent(mun) %>% tolower()

    lim.mun<-lim[lim$mun == nome,]
    CAR<-st_intersection(CAR, lim.mun)

  }

  car.map<-CAR %>% group_by(classe) %>% summarise(num_prop = n(),
                                                  tam_medio_ha = round(mean(NUM_AREA),2),
                                                  desvio_ha = round(sd(NUM_AREA),2))
  car.map<-st_make_valid(car.map)
  car.map$area_ha<-as.numeric(round(units::set_units(st_area(car.map), value = ha),2))
  total.car<-car.map %>% summarise(t=n())
  car.map$total_ha<-as.numeric(round(st_area(total.car)/10000,2))
  ordem<-c("Micro","Pequena (1 a 2 modulos)","Pequena (2 a 4 modulos)","Media","Grande")
  car.map<- car.map %>% arrange(factor(classe, levels = ordem))
  car.map<-st_as_sf(as.data.frame(car.map))

  if(is.null(tipo)){
    return(car.map)
  }else if(tipo == "df"){
    car.map<-st_drop_geometry(car.map)
    return(car.map)
  }
}
