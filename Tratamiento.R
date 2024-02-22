
### En el siguiente codigo se unen y agrupan las bases de Emision y siniestros del ramo autos en México en 
### los años 2020, 2021 y 2022. Para efectos práctivos se agrupan bajo las variables Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`



library(tidyverse)          
library(readr)   
library(reshape2)
library(lubridate)
library(readxl)
library(dplyr)

Emision2022<- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//Emision_2022.txt",col_names = TRUE)
Emision2022_1 <- Emision2022 %>% 
  filter(Entidad != "ND") %>% 
  select(-Marca) %>% 
  group_by(Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`) %>% 
  summarise(Numero_de_vehiculos=sum(`Numero de Vehiculos`),
            Prima_Emitida =sum(`Prima Emitida`),
            Prima_Cedida = sum(`Prima Cedida`),
            Prima_Devengada = sum(`Prima Devengada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`))
AÑO<-rep(2022,dim(Emision2022_1)[1])
Emision2022<-cbind(AÑO,Emision2022_1) 
names(Emision2022)<-c("AÑO",names(Emision2022_1))


Siniestros2022 <- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//Siniestros_2022.txt",col_names = TRUE,)
Siniestros2022_1 <- Siniestros2022 %>% 
  filter(Entidad != "ND") %>%
  select(-Marca) %>% 
  group_by(Entidad,`Tipo de Vehiculo`,Cobertura, `Subtipo de Seguro`,`Tipo de Perdida`,`Causa del siniestro`) %>% 
  summarise(Numero_de_vehiculo= sum(`Numero de Vehiculos`),
            Monto_del_Siniestro = sum(`Monto de Siniestros`),
            Monto_Pagado = sum(`Monto Pagado`),
            Salvamentos =sum(`Salvamentos`),
            Gastos_de_Ajuste=sum(`Gastos de Ajuste`),
            Moonto_Deducible=sum(`Monto de Deducible`),
            Monto_Reaseguro=sum(`Monto Reaseguro`),
            Monto_Recuperado=sum(`Monto Recuperado`))

AÑO<-rep(2022,dim(Siniestros2022_1)[1])
Siniestros2022<-cbind(AÑO,Siniestros2022_1) 
names(Siniestros2022)<-c("AÑO",names(Siniestros2022_1))


EmisionFlotilla2022<- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//EmisionFlotilla_2022.txt",col_names = TRUE)
EmisionFlotilla2022_1 <- EmisionFlotilla2022 %>% 
  filter(Entidad != "ND") %>% 
  select(-Marca) %>% 
  group_by(Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`) %>% 
  summarise(Numero_de_vehiculos=sum(`Numero de Vehiculos`),
            Prima_Emitida =sum(`Prima Emitida`),
            Prima_Cedida = sum(`Prima Cedida`),
            Prima_Devengada = sum(`Prima Devengada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`))
AÑO<-rep(2022,dim(EmisionFlotilla2022_1)[1])
EmisionFlotilla2022<-cbind(AÑO,EmisionFlotilla2022_1) 
names(EmisionFlotilla2022)<-c("AÑO",names(EmisionFlotilla2022_1))


SiniestrosFlotilla2022 <- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//SiniestrosFlotilla_2022.txt",col_names = TRUE,)
SiniestrosFlotilla2022_1 <- SiniestrosFlotilla2022 %>% 
  filter(Entidad != "ND") %>%
  select(-Marca) %>% 
  group_by(Entidad,`Tipo de vehiculo`,Cobertura, `Subtipo de Seguro`,`Tipo de perdida`,`Causa del Siniestro`) %>% 
  summarise(Numero_de_vehiculo= sum(`Numero de vehiculos`),
            Monto_del_Siniestro = sum(`Monto de siniestro`),
            Monto_Pagado = sum(`Monto pagado`),
            Salvamentos =sum(`Salvamento`),
            Gastos_de_Ajuste=sum(`Gasto de ajuste`),
            Moonto_Deducible=sum(`Monto de deducible`),
            Monto_Reaseguro=sum(`Monto de reaseguro`),
            Monto_Recuperado=sum(`Monto recuperado`))

AÑO<-rep(2022,dim(SiniestrosFlotilla2022_1)[1])
SiniestrosFlotilla2022<-cbind(AÑO,SiniestrosFlotilla2022_1) 
names(SiniestrosFlotilla2022)<-c("AÑO",names(SiniestrosFlotilla2022_1))



Emision2021<- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//Emision_2021.txt",col_names = TRUE)
Emision2021_1 <- Emision2021 %>% 
  filter(Entidad != "ND") %>% 
  select(-Marca) %>% 
  group_by(Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`) %>% 
  summarise(Numero_de_vehiculos=sum(`Numero de Vehiculos`),
            Prima_Emitida =sum(`Prima Emitida`),
            Prima_Cedida = sum(`Prima Cedida`),
            Prima_Devengada = sum(`Prima Devengada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`))
AÑO<-rep(2021,dim(Emision2021_1)[1])
Emision2021<-cbind(AÑO,Emision2021_1) 
names(Emision2021)<-c("AÑO",names(Emision2021_1))


Siniestros2021 <- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//Siniestros_2021.txt",col_names = TRUE,)
Siniestros2021_1 <- Siniestros2021 %>% 
  filter(Entidad != "ND") %>%
  select(-Marca) %>% 
  group_by(Entidad,`Tipo de Vehiculo`,Cobertura, `Subtipo de Seguro`,`Tipo de Perdida`,`Causa del siniestro`) %>% 
  summarise(Numero_de_vehiculo= sum(`Numero de Vehiculos`),
            Monto_del_Siniestro = sum(`Monto de Siniestros`),
            Monto_Pagado = sum(`Monto Pagado`),
            Salvamentos =sum(`Salvamentos`),
            Gastos_de_Ajuste=sum(`Gastos de Ajuste`),
            Moonto_Deducible=sum(`Monto de Deducible`),
            Monto_Reaseguro=sum(`Monto Reaseguro`),
            Monto_Recuperado=sum(`Monto Recuperado`))

AÑO<-rep(2021,dim(Siniestros2021_1)[1])
Siniestros2021<-cbind(AÑO,Siniestros2021_1) 
names(Siniestros2021)<-c("AÑO",names(Siniestros2021_1))


EmisionFlotilla2021<- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//EmisionFlotilla_2021.txt",col_names = TRUE)
EmisionFlotilla2021_1 <- EmisionFlotilla2021 %>% 
  filter(Entidad != "ND") %>% 
  select(-Marca) %>% 
  group_by(Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`) %>% 
  summarise(Numero_de_vehiculos=sum(`Numero de Vehiculos`),
            Prima_Emitida =sum(`Prima Emitida`),
            Prima_Cedida = sum(`Prima Cedida`),
            Prima_Devengada = sum(`Prima Devengada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`))
AÑO<-rep(2021,dim(EmisionFlotilla2021_1)[1])
EmisionFlotilla2021<-cbind(AÑO,EmisionFlotilla2021_1) 
names(EmisionFlotilla2021)<-c("AÑO",names(EmisionFlotilla2021_1))


SiniestrosFlotilla2021 <- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//SiniestrosFlotilla_2021.txt",col_names = TRUE,)
SiniestrosFlotilla2021_1 <- SiniestrosFlotilla2021 %>% 
  filter(Entidad != "ND") %>%
  select(-Marca) %>% 
  group_by(Entidad,`Tipo de vehiculo`,Cobertura, `Subtipo de Seguro`,`Tipo de perdida`,`Causa del Siniestro`) %>% 
  summarise(Numero_de_vehiculo= sum(`Numero de vehiculos`),
            Monto_del_Siniestro = sum(`Monto de siniestro`),
            Monto_Pagado = sum(`Monto pagado`),
            Salvamentos =sum(`Salvamento`),
            Gastos_de_Ajuste=sum(`Gasto de ajuste`),
            Moonto_Deducible=sum(`Monto de deducible`),
            Monto_Reaseguro=sum(`Monto de reaseguro`),
            Monto_Recuperado=sum(`Monto recuperado`))

AÑO<-rep(2021,dim(SiniestrosFlotilla2021_1)[1])
SiniestrosFlotilla2021<-cbind(AÑO,SiniestrosFlotilla2021_1) 
names(SiniestrosFlotilla2021)<-c("AÑO",names(SiniestrosFlotilla2021_1))


Emision2020<- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//Emision_2020.txt",col_names = TRUE)
Emision2020_1 <- Emision2020 %>% 
  filter(Entidad != "ND") %>% 
  select(-Marca) %>% 
  group_by(Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`) %>% 
  summarise(Numero_de_vehiculos=sum(`Numero de Vehiculos`),
            Prima_Emitida =sum(`Prima Emitida`),
            Prima_Cedida = sum(`Prima Cedida`),
            Prima_Devengada = sum(`Prima Devengada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`))
AÑO<-rep(2020,dim(Emision2020_1)[1])
Emision2020<-cbind(AÑO,Emision2020_1) 
names(Emision2020)<-c("AÑO",names(Emision2020_1))


Siniestros2020 <- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//Siniestros_2020.txt",col_names = TRUE,)
Siniestros2020_1 <- Siniestros2020 %>% 
  filter(Entidad != "ND") %>%
  select(-Marca) %>% 
  group_by(Entidad,`Tipo de Vehiculo`,Cobertura, `Subtipo de Seguro`,`Tipo de Perdida`,`Causa del siniestro`) %>% 
  summarise(Numero_de_vehiculo= sum(`Numero de Vehiculos`),
            Monto_del_Siniestro = sum(`Monto de Siniestros`),
            Monto_Pagado = sum(`Monto Pagado`),
            Salvamentos =sum(`Salvamentos`),
            Gastos_de_Ajuste=sum(`Gastos de Ajuste`),
            Moonto_Deducible=sum(`Monto de Deducible`),
            Comision_Directa =0,
            Suma_Asegurada =0)

AÑO<-rep(2020,dim(Siniestros2020_1)[1])
Siniestros2020<-cbind(AÑO,Siniestros2020_1) 
names(Siniestros2020)<-c("AÑO",names(Siniestros2020_1))


EmisionFlotilla2020<- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//EmisionFlotilla_2020.txt",col_names = TRUE)
EmisionFlotilla2020_1 <- EmisionFlotilla2020 %>% 
  filter(Entidad != "ND") %>% 
  select(-Marca) %>% 
  group_by(Entidad, `Tipo de Vehiculo`, Cobertura, `Subtipo de Seguro`) %>% 
  summarise(Numero_de_vehiculos=sum(`Numero de Vehiculos`),
            Prima_Emitida =sum(`Prima Emitida`),
            Prima_Cedida = sum(`Prima Cedida`),
            Prima_Devengada = sum(`Prima Devengada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`),
            Comision_Directa =sum(`Comision Directa`),
            Suma_Asegurada =sum(`Suma Asegurada`))
AÑO<-rep(2020,dim(EmisionFlotilla2020_1)[1])
EmisionFlotilla2020<-cbind(AÑO,EmisionFlotilla2020_1) 
names(EmisionFlotilla2020)<-c("AÑO",names(EmisionFlotilla2020_1))


SiniestrosFlotilla2020 <- read_tsv("C://Users//Eliezer Garcia Cruz//Documents//SiniestrosFlotilla_2020.txt",col_names = TRUE,)
SiniestrosFlotilla2020_1 <- SiniestrosFlotilla2020 %>% 
  filter(Entidad != "ND") %>%
  select(-Marca) %>% 
  group_by(Entidad,`Tipo de vehiculo`,Cobertura, `Subtipo de Seguro`,`Tipo de perdida`,`Causa del Siniestro`) %>% 
  summarise(Numero_de_vehiculo= sum(`Numero de vehiculos`),
            Monto_del_Siniestro = sum(`Monto de siniestro`),
            Monto_Pagado = sum(`Monto pagado`),
            Salvamentos =sum(`Salvamento`),
            Gastos_de_Ajuste=sum(`Gasto de ajuste`),
            Moonto_Deducible=sum(`Monto de deducible`),
            Comision_Directa =0,
            Suma_Asegurada =0)

AÑO<-rep(2020,dim(SiniestrosFlotilla2020_1)[1])
SiniestrosFlotilla2020<-cbind(AÑO,SiniestrosFlotilla2020_1) 
names(SiniestrosFlotilla2020)<-c("AÑO",names(SiniestrosFlotilla2020_1))

View(Emision2020)  

EmisionAutos <- rbind(Emision2022,EmisionFlotilla2022,Emision2021,EmisionFlotilla2021,Emision2020,EmisionFlotilla2020)

SiniestrosAutos <- rbind(Siniestros2022,SiniestrosFlotilla2022,Siniestros2021,SiniestrosFlotilla2021,Siniestros2020,SiniestrosFlotilla2020)

write.csv(EmisionAutos,"C:/Users/Eliezer Garcia Cruz/Documents/AUTOS/EmisionAutos.csv")

