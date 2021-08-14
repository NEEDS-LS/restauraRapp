#' Baixa os dados da FBDS
#'
#' Essa função baixa os dados de uso do solo e de APP da FBDS
#' @param estado O estado do município
#' @param municipio o municipio escolhido
#' @return os dados
#' @export
#' @examples
#' dado.municipio<-baixaFBDS(estado, municipio)

baixaFBDS<-function(estado, municipio){

  lista.files.uso = readLines(paste('http://geo.fbds.org.br/', estado, "/", municipio, "/USO'"))
  lista.files.app = readLines(paste('http://geo.fbds.org.br/', estado, "/", municipio, "/APP'"))

  lks.lista<-c(lista.files.uso,lista.files.app)
  lks<-getHTMLLinks(lks.lista, xpQuery = "//a/@href[contains(., estado)]")
  for (i in 1:(length(lks))){
    destf<-paste("./data_use", strsplit(lks[i], "/")[[1]][5], sep="/") #Aqui precisa ver se eh data_use mesmo, tvz nao
    curl_download((paste("http://geo.fbds.org.br", lks[i], sep="")),destfile = destf)
  }
}



