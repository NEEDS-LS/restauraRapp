#' Função que calcula e retorna uma tabela (data frame) da área preservada e que precisa ser
#' restaurada por classe de propriedade.
#'
#' Essa função unifica as áreas com diferentes usos do solo em "Preservado (ha)", com os usos
#' Formação Florestal e Formação não Florestal, e "Restaurar (ha)" com os demais usos. Pode
#' retornar os polígonos destas áreas, um data frame com os valores ou o que existe existe
#' de cada categoria dentro das propriedades cadastradas no CAR.
#'
#' @param CARapp Objeto resultante da execução da função principal resapp_app_buffer() para as propriedades
#'  que possuem CAR registrado, é recomendavel que todos os tamanhos, exceto os cenários disponíveis,
#'  etejam em um unico objeto.
#' @param CARapp_out1 Objeto resultante da execução da função principal resapp_app_buffer(), com tipo "out1",
#'  ou seja, considerando as áreas sem CAR como micro propriedades. Pode ser NULL.
#' @param CARapp_out2 Objeto resultante da execução da função principal resapp_app_buffer(), com tipo "out2",
#'  ou seja, considerando as áreas sem CAR como grande propriedades. Pode ser NULL.
#' @param CAR Objeto contendo os polígonos com as áreas das propriedades registradas no CAR.
#'  Pode ser NULL.
#' @param tipo Quais informações serão retornadas. "df" para data frame, "tudo" para retornar
#'  as informações dos polígonos para as propriedades cadastradas e "prop" para o recorte das áreas
#'  por propriedade cadastrada.
#' @return Objeto data frame ou sf referente à preservação dentro do buffer criado.
#' "all" Retorna os polígonos dos buffers criados classificados como "Preservado" e "Restaurar"
#' "df" Retorna a tabela de dados com as informações sobre o quando está preservado e o que é necessário
#' restaurar por classe de tamanho das propriedades.
#' "prop" Retorna a tabela de dados com os valores a serem restaurados e qu ese encontram preservados
#' por propriedade do CAR
#' @export
#' @examples
#' #Para executar o tipo "df" é necessário todos os resultados da função resapp_app_buffer()
#' dados.CARapp<-resapp_app_info(CARapp,CARapp_out1,CARapp_out2,tipo="df")
#'
#' # A opção "tudo" é executada apenas para os resultados das análises das áreas que possuem CAR.
#' poligonos.CARapp<-resapp_app_info(CARapp, tipo="all")
#'
#' # Por fim, a opção "prop" leva em consideração apenas os resultados das análises das áreas que
#' #possuem CAR e os polígonos do CAR que não estão cancelados por decisão administrativa.
#' propriedade.CARapp<-resapp_app_info(CARapp, CAR, tipo="prop")
#'


resapp_app_info<-function(CARapp, CARapp_out1 = NULL, CARapp_out2 = NULL, CAR = NULL, tipo){
  if(tipo == "tudo"){
    CARapp$C_USO<-rm_accent(CARapp$C_USO)
    CARapp<-CARapp %>% mutate(SIT =
                                case_when(C_USO == "agua" ~ "Agua",
                                          C_USO == "area antropizada" ~ "Restaurar",
                                          C_USO == "area edificada" ~ "Restaurar",
                                          C_USO == "silvicultura" ~ "Restaurar",
                                          C_USO == "formacao florestal" ~ "Preservado",
                                          C_USO == "formacao nao florestal" ~ "Preservado"))
    CARapp<-CARapp %>% group_by(C_PROP, MUN, SIT) %>% summarise(AREA_HA = sum(AREA_HA))
    CARapp<-st_as_sf(as.data.frame(CARapp))

    return(CARapp)

  }else if(tipo == "df"){

    clipped_app<-CARapp

    if(!is.null(CARapp_out1)){
      clipped_app<-rbind(clipped_app, CARapp_out1)
    }
    if(!is.null(CARapp_out2)){
      clipped_app<-rbind(clipped_app, CARapp_out2)
    }
    clipped_app$C_USO<-rm_accent(clipped_app$C_USO)
    clipped_app<-clipped_app %>% mutate(SIT =
                                case_when(C_USO == "agua" ~ "Agua",
                                          C_USO == "area antropizada" ~ "Restaurar",
                                          C_USO == "area edificada" ~ "Restaurar",
                                          C_USO == "silvicultura" ~ "Restaurar",
                                          C_USO == "formacao florestal" ~ "Preservado",
                                          C_USO == "formacao nao florestal" ~ "Preservado"))
    clipped_app<-clipped_app %>% group_by(C_PROP, MUN, SIT) %>% summarise(AREA_HA = sum(AREA_HA))
    clipped_app<-st_drop_geometry(clipped_app)

    rest<-clipped_app[clipped_app$SIT=="Restaurar",]
    pres<-clipped_app[clipped_app$SIT=="Preservado",]

    l.col.data<-c("Propriedade", "Restaurar (ha)", "Preservado (ha)")

    l.micro<-c("Micro",
               rest$AREA_HA[rest$C_PROP=="Micro"],
               pres$AREA_HA[pres$C_PROP=="Micro"])
    l.peq12<-c("Pequenas (> 1 e < 2 módulos)",
               rest$AREA_HA[rest$C_PROP=="Pequenas 1 a 2 modulos"],
               pres$AREA_HA[pres$C_PROP=="Pequenas 1 a 2 modulos"])
    l.peq23<-c("Pequenas (> 2 e < 4 módulos)",
               rest$AREA_HA[rest$C_PROP=="Pequenas 2 a 4 modulos"],
               pres$AREA_HA[pres$C_PROP=="Pequenas 2 a 4 modulos"])
    l.media<-c("Média",
               rest$AREA_HA[rest$C_PROP=="Media"],
               pres$AREA_HA[pres$C_PROP=="Media"])
    l.grand<-c("Grande",
               rest$AREA_HA[rest$C_PROP=="Grande"],
               pres$AREA_HA[pres$C_PROP=="Grande"])
    l.total<-c("Cenário 1 (Total)",
               colSums(rest[1:5,4]),
               colSums(pres[1:5,4]))
    l.out1<-c("Cenário 2 (Micro)",
              rest$AREA_HA[rest$C_PROP=="Sem CAR (Micro)"],
              pres$AREA_HA[pres$C_PROP=="Sem CAR (Micro)"])
    l.totalC2<-c("Cenário 2 (Total)",
               colSums(rest[1:5,4])+rest$AREA_HA[rest$C_PROP=="Sem CAR (Micro)"],
               colSums(pres[1:5,4])+pres$AREA_HA[pres$C_PROP=="Sem CAR (Micro)"])
    l.out2<-c("Cenário 3 (Grande)",
              rest$AREA_HA[rest$C_PROP=="Sem CAR (Grande)"],
              pres$AREA_HA[pres$C_PROP=="Sem CAR (Grande)"])
    l.totalC3<-c("Cenário 3 (Total)",
                 colSums(rest[1:5,4])+rest$AREA_HA[rest$C_PROP=="Sem CAR (Grande)"],
                 colSums(pres[1:5,4])+pres$AREA_HA[pres$C_PROP=="Sem CAR (Grande)"])

    names(l.micro)<-l.col.data
    names(l.peq12)<-l.col.data
    names(l.peq23)<-l.col.data
    names(l.media)<-l.col.data
    names(l.grand)<-l.col.data
    names(l.total)<-l.col.data
    names(l.out1)<-l.col.data
    names(l.totalC2)<-l.col.data
    names(l.out2)<-l.col.data
    names(l.totalC3)<-l.col.data

    l.all.data<-list(l.micro,l.peq12,l.peq23,l.media,l.grand,l.total,l.out1,l.totalC2,
                     l.out2,l.totalC3)

    t.data<-as.data.frame(do.call("rbind", l.all.data))

    return(t.data)

  }else if(tipo == "prop"){
    CARapp$C_USO<-rm_accent(CARapp$C_USO)
    CARapp<-CARapp %>% mutate(SIT =
                                case_when(C_USO == "agua" ~ "Agua",
                                          C_USO == "area antropizada" ~ "Restaurar",
                                          C_USO == "area edificada" ~ "Restaurar",
                                          C_USO == "silvicultura" ~ "Restaurar",
                                          C_USO == "formacao florestal" ~ "Preservado",
                                          C_USO == "formacao nao florestal" ~ "Preservado"))
    CARapp<-CARapp %>% group_by(C_PROP, MUN, SIT) %>% summarise()

    CAR.df<-CAR[CAR$SITUACAO!="CA",]
    CAR.df<-CAR.df %>% select((1:5))

    CAR<-CARapp_CAR_class(CAR)
    prop.app<-NULL
    for(i in 1:5){
      app.uso<-CARapp[CARapp$C_PROP == CAR[[6]][i],]
      prop<-st_intersection(app.uso, CAR[[i]])
    if(is.null(prop.app)){
      prop.app<-prop
    }else{
      prop.app<-rbind(prop.app, prop)
    }
    }
    prop.app$area_ha<-as.numeric(round(st_area(prop.app)/10000,2))
    rest.prop.app<-prop.app[prop.app$SIT == "Restaurar",]
    pres.prop.app<-prop.app[prop.app$SIT == "Preservado",]

    CAR.df$REST_ha<-rest.prop.app$area_ha[match(CAR.df$COD_IMOVEL, rest.prop.app$COD_IMOVEL)]
    CAR.df$PRES_ha<-pres.prop.app$area_ha[match(CAR.df$COD_IMOVEL, pres.prop.app$COD_IMOVEL)]

    CAR.df$REST_ha[is.na(CAR.df$REST_ha)]<-0
    CAR.df$PRES_ha[is.na(CAR.df$PRES_ha)]<-0

    CAR.df<-st_drop_geometry(CAR.df)

    return(CAR.df)
    }


}

