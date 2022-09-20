#' Baixa os dados da FBDS
#'
#' Essa função baixa os dados de uso do solo e de APP da FBDS
#' @param estado O estado do município (SIGLA)
#' @param municipio o municipio escolhido (caixa alta separado por "_")
#' @return os dados de uso do solo e de Hidrografia da FBDS
#' @export
#' @examples
#' dado.municipio<-resapp_fbds_dados("SP", "CAMPINA_DO_MONTE_ALEGRE")

resapp_fbds_dados<-function(estado, municipio){

  lista.files.uso = readLines(
    paste('http://geo.fbds.org.br/',estado,'/',municipio,'/USO/', sep = ""))
  lista.files.app = readLines(
    paste('http://geo.fbds.org.br/',estado,'/',municipio,'/HIDROGRAFIA/', sep = ""))

  lks.lista<-c(lista.files.uso,lista.files.app)
  lks<-getHTMLLinks(lks.lista, xpQuery =
                      paste("//a/@href[contains(., '",estado,"')]", sep = ""))
  if(file.exists("./data")==FALSE){
  dir.create("./data")
  }
  dir.create(paste("./data/FBDS_",municipio, sep = ""))
  for (i in 1:(length(lks))){
    destf<-paste(paste("./data/FBDS_",municipio, sep = ""),
                 strsplit(lks[i], "/")[[1]][5], sep="/") #Aqui precisa ver se eh data_use mesmo, tvz nao
    curl_download((paste("http://geo.fbds.org.br", lks[i], sep="")),destfile = destf)
  }
}



