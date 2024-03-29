% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separaTamanho.R
\name{separaTamanho}
\alias{separaTamanho}
\title{Função separa as propriedades por tamanho baseado no numero de modulos fiscais}
\usage{
separaTamanho(mapa_CAR)
}
\arguments{
\item{mapa_CAR}{Conjunto de propriedades registradas que serão utilizadas na definição das APPS}
}
\value{
lista Uma lista de SpatialPolygonsDataFrames referentes as propriedades separados por tamanho.
}
\description{
Essa função separa as propriedades por tamanho baseado no número de módulos fiscais
}
\examples{

props<-separaTamanho(mapa_CAR)
micro<-props[[1]]
peq12<-props[[2]]
peq24<-props[[3]]
media<-props[[4]]
grand<-props[[5]]
}
