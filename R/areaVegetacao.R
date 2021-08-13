#' Delimita a área de APP com vegetação nativa
#'
#' Essa função delimita a área a ser restaurada a partir do objeto resultante da aplicação das funções para recorte do uso dentro das APPs, esta função é especifica para os dados de uso do solo disponiveis na FBDS
#' @param clipped_use Área delimitada pelo buffer referente ao tamanho da APP com os usos do solo.
#' @return a.veg.area Retorna o valor, em hectares, das áreas com vegetação dentro do buffer de entrada.
#' @export
#' @examples
#' 
#' exemplo<-gMicro(mapa_MDA,mapa_RMS,mapa_RMD,mapa_NAS,micro,uso)
#' 
#' area<-areaVegetacao(exemplo)
#' 
#' #area = Valor em hectares da área coberta por vegetação nativa.


areaVegetacao<-function(clipped_use){
 
  clipped_use$CLASSE_USO<-rm_accent(clipped_use$CLASSE_USO)
  
  clipped_use<-clipped_use[clipped_use$CLASSE_USO=="formacao florestal" | 
                             clipped_use$CLASSE_USO=="formacao nao florestal",]
  
  clipped_use<-mergePoli(clipped_use)
  
  clipped_use<-gArea(clipped_use)/10000
  return(clipped_use)
}