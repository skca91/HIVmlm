#' Archivo Global
#' 
#' @description 
#' Archivo global donde permite cargar las libreras necesarias para la aplicacion
#' @details 
#' Tambien permite la carga del mapa
#' @author Stephanie Correa

library(ggplot2)#graficos
library(lattice)
library(maptools)#mapas
library(rgdal)#leer archivos shp
library(sp) 
library(RColorBrewer)#paletas de colores
library(classInt)
library(plotly)#graficos interactivos
library(gridExtra)#para mapear
library(grid)#para mapear
library(shiny)#el Shiny
library(leaflet)
library(lme4)
library(influence.ME)
library(nlme)
library(lmerTest)
library(car)
library(Matrix)
library(influence.SEM)
library(longitudinal)
library(MASS)
library(sqldf)



#' Carga de la informacion del mapa
#' El \code{read.csv} Permite cargar el archivo .csv  para cargar la informacion del mapa
#' @param ../Data/VEN_adm2.csv Fichero donde esta ubicado el archivo
#' @param header Es la cabecera del archivo, es decir, los nombres de las variables
#' @param sep Es el metodo para separar los datos en el archivo
info <- read.csv("VEN_adm2.csv",header = TRUE,sep = ",")

#' Carga del archivo .shp del mapa de venezuela
#' El \code{readOGR} Permite cargar el archivo .shp  para cargar el mapa
#' @param VEN_adm2.shp Fichero donde esta ubicado el archivo
#' @param layer Es la capa del mapa
#' @return Me devuelve el mapa
map<-readOGR("VEN_adm2.shp", layer="VEN_adm2")




