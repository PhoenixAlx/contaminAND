library("entropy")
library("ggplot2")
library("ggthemes")
library("readr")
library("svglite")
library("lubridate")
library("plyr")
library("RColorBrewer")
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(scales)
library(tidyr)
library(grid)
library(gridExtra)
datos.CO<-read_csv("contaminAND-gr-conjunto.csv", 
                   col_types = cols(date= col_datetime(),CO.congresos = col_number(), 
                                    CO.norte = col_number()), na = "NA")
number_data.datos.CO<- nrow(datos.CO)
datos.CO.filtros<-datos.CO
datos.CO.filtros[is.na(datos.CO.filtros)] <- -1000000
datos.CO.filtros$ano <- as.numeric(format(datos.CO.filtros$date, "%Y"))
datos.CO.filtros$mes<- as.numeric(format(datos.CO.filtros$date, "%m"))
datos.CO.filtros$dia<- as.numeric(format(datos.CO.filtros$date, "%d"))
datos.CO.filtros$hora<- as.numeric(format(datos.CO.filtros$date, "%H"))
datos.CO.filtros$minuto<- as.numeric(format(datos.CO.filtros$date, "%M"))
datos.CO.filtros$date <- as.numeric(datos.CO.filtros$date)
number_data.datos.CO.filtros<- nrow(datos.CO.filtros)
datos.CO.filtros2<- datos.CO.filtros[1:(number_data.datos.CO.filtros-1),]
datos.CO.filtros3<- datos.CO.filtros[-1,]
#datos.gradiente.CO <- data.frame(congresos= numeric(0), norte= numeric(0), percert.congresos=numeric(0),percent.norte=numeric(0),original.congresos.ant=numeric(0),original.norte.ant=numeric(0),original.congresos=numeric(0),original.norte=numeric(0))
datos.gradiente.CO<-data.frame(diff(as.matrix(datos.CO.filtros)),datos.CO.filtros2,datos.CO.filtros3)
datos.gradiente.CO$percent.congresos<-datos.gradiente.CO$CO.congresos*100/datos.gradiente.CO$CO.congresos.1;
datos.gradiente.CO$percent.norte<-datos.gradiente.CO$CO.norte*100/datos.gradiente.CO$CO.norte.1;
datos.gradiente.CO$magnitud.congresos<-trunc(log10(abs(datos.gradiente.CO$CO.congresos)))
datos.gradiente.CO$magnitud.congresos[datos.gradiente.CO$CO.congresos==0]<-0
datos.gradiente.CO.exageraos<-datos.gradiente.CO[datos.gradiente.CO$percent.congresos>-100 ,]
datos.gradiente.CO$magnitud.norte<-trunc(log10(abs(datos.gradiente.CO$CO.norte)))
datos.gradiente.CO$magnitud.norte[datos.gradiente.CO$CO.norte==0]<-0


#ggsave("gradientes.png")
#ggsave("gradientes.svg")
gg.mapas.calor<-function(datos,campo1,campo2,title) {
  mapa.calor<-count(datos, c(tiempo=campo1, magnitud=campo2));
  mapa.calor.completo<-mapa.calor %>% complete(magnitud=full_seq(magnitud, period = 1),tiempo,fill = list(freq = 0))
  gg <- ggplot(mapa.calor.completo, aes(x=tiempo, y=magnitud, fill=freq))
  gg <- gg + geom_tile(color="white", size=0.1)
  gg <- gg + scale_fill_viridis(name="# Frecuency", label=comma)
  gg <- gg + coord_equal()
  gg <- gg + labs(x=NULL, y=NULL, title=title)
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(plot.title=element_text(hjust=0))
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_text(size=7))
  gg <- gg + theme(legend.title=element_text(size=8))
  gg <- gg + theme(legend.text=element_text(size=6))
  return(gg)
}

gg.congresos.minuto<-gg.mapas.calor(datos.gradiente.CO,"minuto.2","magnitud.congresos","Frecuencies by minuto & Magnitude of gradients of Congresos")
gg.norte.minuto<-gg.mapas.calor(datos.gradiente.CO,"minuto.2","magnitud.norte","Frecuencies by minuto & Magnitude of gradients of Norte")


gg.congresos.hora<-gg.mapas.calor(datos.gradiente.CO,"hora.2","magnitud.congresos","Frecuencies by hour & Magnitude of gradients of Congresos")
gg.norte.hora<-gg.mapas.calor(datos.gradiente.CO,"hora.2","magnitud.norte","Frecuencies by hour & Magnitude of gradients of Norte")

gg.congresos.dia<-gg.mapas.calor(datos.gradiente.CO,"dia.2","magnitud.congresos","Frecuencies by month's day & Magnitude of gradients of Congresos")
gg.norte.dia<-gg.mapas.calor(datos.gradiente.CO,"dia.2","magnitud.norte","Frecuencies by month's day & Magnitude of gradients of Norte")

gg.congresos.mes<-gg.mapas.calor(datos.gradiente.CO,"mes.2","magnitud.congresos","Frecuencies by month & Magnitude of gradients of Congresos")
gg.norte.mes<-gg.mapas.calor(datos.gradiente.CO,"mes.2","magnitud.norte","Frecuencies by month & Magnitude of gradients of Norte")

gg.congresos.ano<-gg.mapas.calor(datos.gradiente.CO,"ano.2","magnitud.congresos","Frecuencies by year & Magnitude of gradients of Congresos")
gg.norte.ano<-gg.mapas.calor(datos.gradiente.CO,"ano.2","magnitud.norte","Frecuencies by year & Magnitude of gradients of Norte")


ml<-grid.arrange(gg.congresos.minuto, gg.norte.minuto,gg.congresos.hora, gg.norte.hora,gg.congresos.dia,gg.norte.dia,gg.congresos.mes,gg.norte.mes,gg.congresos.ano,gg.norte.ano,ncol=2)
ggsave("gradientes.png",ml, width = 50, height = 50,units = "cm",limitsize = FALSE)