#' Função salva os buffers criados em KML
#'
#' Por padrão, essa função salva dentro do diretório criado para o projeto, com o nome do município acompanhado de "KML"
#' @param app_buffer Mapa do buffer com uso do solo proveniente da função resapp_app_buffer.
#' @param municipio Nome do município para compor o nome do arquivo. Pode ser Null.
#' @param estado UF ou nome do estado para compor o nome do arquivo. Pode ser Null.
#' @return Cria uma pasta no mesmo diretório com os arquivos KML separados por tamanho de propriedade
#' e por app completa e apenas áreas a serem restauradas.
#' @export
#' @examples
#'


resapp_app_kml<-function(app_buffer, municipio = NULL, estado = NULL){

  sep_app<-resapp_app_info(app_buffer, tipo = "tudo")

  sep_app<-sep_app[sep_app$SIT != "Agua",]

  if(is.null(municipio)){
    municipio<-unique(sep_app$MUN)
  }
  if(is.null(estado)){
    estado<-"resapp"
  }

  if(!dir.exists("./KML")){
    dir.create("./KML")
    dir.create(paste("./KML/",municipio,"_",estado,"_KML", sep = ""))
  }else{
    dir.create(paste("./KML/",municipio,"_",estado,"_KML", sep = ""))
  }

  st_write(sep_app[sep_app$C_PROP == "Micro",],
           paste("./KML/",municipio,"_",estado,"_KML/micro_5m_app_",municipio,"_",estado,".kml",sep = ""))

  st_write(sep_app[sep_app$C_PROP == "Pequenas 1 a 2 modulos",],
           paste("./KML/",municipio,"_",estado,"_KML/peq12_8m_app_",municipio,"_",estado,".kml",sep = ""))

  st_write(sep_app[sep_app$C_PROP == "Pequenas 2 a 4 modulos",],
           paste("./KML/",municipio,"_",estado,"_KML/peq24_15m_app_",municipio,"_",estado,".kml",sep = ""))

  st_write(sep_app[sep_app$C_PROP == "Grande",],
           paste("./KML/",municipio,"_",estado,"_KML/grande_20m_app_",municipio,"_",estado,".kml",sep = ""))

  res_app<-sep_app[sep_app$SIT=="Restaurar",]

  st_write(res_app[res_app$C_PROP == "Micro",],
           paste("./KML/",municipio,"_",estado,"_KML/micro_5m_restaurar_",municipio,"_",estado,".kml", sep = ""))

  st_write(res_app[res_app$C_PROP == "Pequenas 1 a 2 modulos",],
           paste("./KML/",municipio,"_",estado,"_KML/peq12_8m_restaurar_",municipio,"_",estado,".kml", sep = ""))

  st_write(res_app[res_app$C_PROP == "Pequenas 2 a 4 modulos",],
           paste("./KML/",municipio,"_",estado,"_KML/peq24_15m_restaurar_",municipio,"_",estado,".kml", sep = ""))

  st_write(res_app[res_app$C_PROP == "Grande",],
           paste("./KML/",municipio,"_",estado,"_KML/grande_20m_restaurar_",municipio,"_",estado,".kml", sep = ""))
}
