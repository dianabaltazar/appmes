##### Dev. Subdirector MSc Erick René Hernández Cervantes #####
##### Secretaría de Gobernación ###################
##### Project:  MES############
##### Script: indicreator#####
##### Notes: From drive always % Keep it real b
##### Packages {uncoment as required}

library(tidyverse)
# library(lubridate)
# library(docxtractr)
# library(rvest)
# library(maptools)
# library(gtools)
library(rlist)
library(readxl)
library(googledrive)

##########################
sis_ind <- function(download=T, upload=F, anio.trimestre, user, secrets.path, gargle_token.path){
  
drive_auth(email = user,
           path = secrets.path,
           scopes = "https://www.googleapis.com/auth/drive",
           cache = gargle_token.path)
  
  if(download){
di <- googledrive::drive_ls(as_id("1d51wj4Mq2MJiIntS0a65thB1k0YUZyar"))
path_csv <- di$id[grep(paste0("database_mes_",anio.trimestre,".csv"), di$name)]
googledrive::drive_download(file=as_id(path_csv),
                            path="db/tempo.csv",
                            overwrite = T) # set temporary file properly
df <- read.csv("db/tempo.csv", stringsAsFactors = F)
  } else{
  df <- read.csv(paste0("db/database_mes_",anio.trimestre,".csv"), stringsAsFactors = F)
  }
df[df=="no aplica"] <- NA
df[df=="o"] <- 0
year <- as.numeric(strsplit(anio.trimestre,"_")[[1]][1])
df <- df %>%  mutate_at(2:(2+(year-2017)), function(x){as.integer(x)})

##### moment for tidyng the data
df_t <- df %>% pivot_longer(cols=2:(2+(year-2017)), names_to = "year", values_to = "número")
df_t$year <- as.numeric(gsub(".*?([0-9]+).*$", "\\1", df_t$year))
df_t <- df_t %>% 
  pivot_wider(id_cols = c(2, "year"), names_from = 1, values_from = "número") 

#### creating indices
df_i <- df_t %>% mutate(
  `Carpetas de investigación abiertas`=`CII por DQO Año Vigente`/
    (`Querellas u Otros Requisitos`+`Denuncias`)*100,
  `Indice de carpetas determinadas por el MP`=(`PCII Archivo Temporal`+
                                                 `PCII Abstención de Investigar`+
                                                 `PCII No Ejercicio Acción Penal`+
                                                 `PCII Criterio de Oportunidad`+
                                                 `PCII por Incompetencia`+
                                                 `PCII por  Acumulación`+
                                                 `PCII por Otra Causa de Extinción Penal`+
                                                 `PCII por Otra Decisión/Terminación Código Penal Estatal`)
  /(`PCII Archivo Temporal`+
      `PCII Abstención de Investigar`+
      `PCII No Ejercicio Acción Penal`+
      `PCII Criterio de Oportunidad`+
      `PCII por Incompetencia`+
      `PCII por  Acumulación`+
      `PCII por Sobreseimiento Ord. Juez Control`+ 
      `PCII por Otra Causa de Extinción Penal`+ 
      `PCII por Otra Decisión/Terminación Código Penal Estatal`+
      `PCII en Trámite Etapa de Investigación`+
      `PCII Vinculadas a Proceso`+
      `PCII en Trámite OEMASC sin Acuerdo`+ 
      `PCII en Trámite OEMASC con Acuerdo`+
      `PCII Resueltos OEMASC por Mediación`+
      `PCII Resueltos OEMASC por Conciliación`+
      `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
  `Resolución de carpetas por acuerdos reparatorios`=(`PCII en Trámite OEMASC sin Acuerdo`+ 
                                                        `PCII en Trámite OEMASC con Acuerdo`+
                                                        `PCII Resueltos OEMASC por Mediación`+
                                                        `PCII Resueltos OEMASC por Conciliación`+
                                                        `PCII Resueltos OEMASC por Junta Restaurativa`)/(`PCII Archivo Temporal`+
                                                                                                           `PCII Abstención de Investigar`+
                                                                                                           `PCII No Ejercicio Acción Penal`+
                                                                                                           `PCII Criterio de Oportunidad`+
                                                                                                           `PCII por Incompetencia`+
                                                                                                           `PCII por  Acumulación`+
                                                                                                           `PCII por Sobreseimiento Ord. Juez Control`+ 
                                                                                                           `PCII por Otra Causa de Extinción Penal`+ 
                                                                                                           `PCII por Otra Decisión/Terminación Código Penal Estatal`+
                                                                                                           `PCII en Trámite Etapa de Investigación`+
                                                                                                           `PCII Vinculadas a Proceso`+
                                                                                                           `PCII en Trámite OEMASC sin Acuerdo`+ 
                                                                                                           `PCII en Trámite OEMASC con Acuerdo`+
                                                                                                           `PCII Resueltos OEMASC por Mediación`+
                                                                                                           `PCII Resueltos OEMASC por Conciliación`+
                                                                                                           `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
  `Carpetas sin determinar en su fase inicial`=(`PCII en Trámite Etapa de Investigación`)/
    (`PCII Archivo Temporal`+
       `PCII Abstención de Investigar`+
       `PCII No Ejercicio Acción Penal`+
       `PCII Criterio de Oportunidad`+
       `PCII por Incompetencia`+
       `PCII por  Acumulación`+
       `PCII por Sobreseimiento Ord. Juez Control`+ 
       `PCII por Otra Causa de Extinción Penal`+ 
       `PCII por Otra Decisión/Terminación Código Penal Estatal`+
       `PCII en Trámite Etapa de Investigación`+
       `PCII Vinculadas a Proceso`+
       `PCII en Trámite OEMASC sin Acuerdo`+ 
       `PCII en Trámite OEMASC con Acuerdo`+
       `PCII Resueltos OEMASC por Mediación`+
       `PCII Resueltos OEMASC por Conciliación`+
       `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
  `Carpetas de investigación vinculadas a proceso`=(`PCII Vinculadas a Proceso`)/
    (`PCII Archivo Temporal`+
       `PCII Abstención de Investigar`+
       `PCII No Ejercicio Acción Penal`+
       `PCII Criterio de Oportunidad`+
       `PCII por Incompetencia`+
       `PCII por  Acumulación`+
       `PCII por Sobreseimiento Ord. Juez Control`+ 
       `PCII por Otra Causa de Extinción Penal`+ 
       `PCII por Otra Decisión/Terminación Código Penal Estatal`+
       `PCII en Trámite Etapa de Investigación`+
       `PCII Vinculadas a Proceso`+
       `PCII en Trámite OEMASC sin Acuerdo`+ 
       `PCII en Trámite OEMASC con Acuerdo`+
       `PCII Resueltos OEMASC por Mediación`+
       `PCII Resueltos OEMASC por Conciliación`+
       `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
  `Resolucion de carpetas por órgano jurisdiccional`=(`PVP Cumplida Suspensión Condicional Proc.`+
                                                        `PVP Resueltos OEMASC por Mediación`+
                                                        `PVP Resueltos OEMASC por Conciliación`+
                                                        `PVP Resueltos OEMASC por Junta Restaurativa`+
                                                        `PVP Resueltos por Otros Sobreseimientos`+
                                                        `PVP por Criterio de Oportunidad`+
                                                        `PVP Resueltos  Procedimiento Abreviado`+
                                                        `PVP Resueltos por Juicio Oral`+
                                                        `PVP en Trámite ante el Tribunal Enjuiciamiento`)/
    (`PVP Cumplida Suspensión Condicional Proc.`+              
       `PVP en Trámite ante el Tribunal Enjuiciamiento`+         
       `PVP en Trámite Juez de Control`+                         
       `PVP en Trámite OEMASC con Acuerdo`+                      
       `PVP en Trámite OEMASC sin Acuerdo`+                      
       `PVP en Trámite Procedimiento Abreviado`+                 
       `PVP en Trámite Suspensión Condicional Proc.`+            
       `PVP por Criterio de Oportunidad`+                        
       `PVP Resueltos  Procedimiento Abreviado`+                 
       `PVP Resueltos OEMASC por Conciliación`+                  
       `PVP Resueltos OEMASC por Junta Restaurativa`+            
       `PVP Resueltos OEMASC por Mediación`+                     
       `PVP Resueltos por Juicio Oral`+                          
       `PVP Resueltos por Otros Sobreseimientos`)*100,
  `Carpetas de investigación vinculadas a proceso en trámite`=(`PVP en Trámite Suspensión Condicional Proc.`+
                                                                 `PVP en Trámite OEMASC sin Acuerdo`+
                                                                 `PVP en Trámite OEMASC con Acuerdo`+
                                                                 `PVP en Trámite Juez de Control`+
                                                                 `PVP en Trámite Procedimiento Abreviado`)/
    (`PVP Cumplida Suspensión Condicional Proc.`+              
       `PVP en Trámite ante el Tribunal Enjuiciamiento`+         
       `PVP en Trámite Juez de Control`+                         
       `PVP en Trámite OEMASC con Acuerdo`+                      
       `PVP en Trámite OEMASC sin Acuerdo`+                      
       `PVP en Trámite Procedimiento Abreviado`+                 
       `PVP en Trámite Suspensión Condicional Proc.`+            
       `PVP por Criterio de Oportunidad`+                        
       `PVP Resueltos  Procedimiento Abreviado`+                 
       `PVP Resueltos OEMASC por Conciliación`+                  
       `PVP Resueltos OEMASC por Junta Restaurativa`+            
       `PVP Resueltos OEMASC por Mediación`+                     
       `PVP Resueltos por Juicio Oral`+                          
       `PVP Resueltos por Otros Sobreseimientos`)*100,#Asuntos vinculados a proceso
  `Indice de sentencias condenatorias`=(`Imputados Sentencia Condenatoria Proced. Abreviado`+
                                          `Imputados con Sentencia Condenatoria Juicio Oral`)/
    (`Imputados Sentencia Condenatoria Proced. Abreviado`+
       `Imputados con Sentencia Condenatoria Juicio Oral`+
       `Imputados con Sentencia Absolutoria Juicio Oral`+
       `Imputados Sentencia Absolutoria Proced. Abreviado`)*100,
  `Indice de medidas cautelares impuestas`=(`Imputados con Prisión Preventiva Oficiosa`+
                                              `Imputados con Prisión Preventiva No Oficiosa`+
                                              `Imputados con Otra Medida Cautelar`)/
    (`Imputados con Prisión Preventiva Oficiosa`+
       `Imputados con Prisión Preventiva No Oficiosa`+
       `Imputados con Otra Medida Cautelar`+
       `Imputados Sin Medida Cautelar`)*100#,
#  `Tasa de internamiento de imputados en prisión preventiva`=(`Población procesada del fuero común`)/(`Población sentenciada del fuero común`+
 #                                                                                                       `Población procesada del fuero común`)*100
 ) %>%
  select(1, year, 
              `Carpetas de investigación abiertas`:`Indice de medidas cautelares impuestas`)#`Tasa de internamiento de imputados en prisión preventiva`)#`Tasa de internamiento de imputados en prisión preventiva`)
######## creating nacional indicator
#### creating indices
df_i_nacional <- df_t %>% group_by(year) %>% summarise_if(is.numeric,function(x){sum(x, na.rm=T)}) %>% 
  mutate(
    `Carpetas de investigación abiertas`=`CII por DQO Año Vigente`/
      (`Querellas u Otros Requisitos`+`Denuncias`)*100,
    `Indice de carpetas determinadas por el MP`=(`PCII Archivo Temporal`+
                                                   `PCII Abstención de Investigar`+
                                                   `PCII No Ejercicio Acción Penal`+
                                                   `PCII Criterio de Oportunidad`+
                                                   `PCII por Incompetencia`+
                                                   `PCII por  Acumulación`+
                                                   `PCII por Otra Causa de Extinción Penal`+
                                                   `PCII por Otra Decisión/Terminación Código Penal Estatal`)
    /(`PCII Archivo Temporal`+
        `PCII Abstención de Investigar`+
        `PCII No Ejercicio Acción Penal`+
        `PCII Criterio de Oportunidad`+
        `PCII por Incompetencia`+
        `PCII por  Acumulación`+
        `PCII por Sobreseimiento Ord. Juez Control`+ 
        `PCII por Otra Causa de Extinción Penal`+ 
        `PCII por Otra Decisión/Terminación Código Penal Estatal`+
        `PCII en Trámite Etapa de Investigación`+
        `PCII Vinculadas a Proceso`+
        `PCII en Trámite OEMASC sin Acuerdo`+ 
        `PCII en Trámite OEMASC con Acuerdo`+
        `PCII Resueltos OEMASC por Mediación`+
        `PCII Resueltos OEMASC por Conciliación`+
        `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
    `Resolución de carpetas por acuerdos reparatorios`=(`PCII en Trámite OEMASC sin Acuerdo`+ 
                                                          `PCII en Trámite OEMASC con Acuerdo`+
                                                          `PCII Resueltos OEMASC por Mediación`+
                                                          `PCII Resueltos OEMASC por Conciliación`+
                                                          `PCII Resueltos OEMASC por Junta Restaurativa`)/(`PCII Archivo Temporal`+
                                                                                                             `PCII Abstención de Investigar`+
                                                                                                             `PCII No Ejercicio Acción Penal`+
                                                                                                             `PCII Criterio de Oportunidad`+
                                                                                                             `PCII por Incompetencia`+
                                                                                                             `PCII por  Acumulación`+
                                                                                                             `PCII por Sobreseimiento Ord. Juez Control`+ 
                                                                                                             `PCII por Otra Causa de Extinción Penal`+ 
                                                                                                             `PCII por Otra Decisión/Terminación Código Penal Estatal`+
                                                                                                             `PCII en Trámite Etapa de Investigación`+
                                                                                                             `PCII Vinculadas a Proceso`+
                                                                                                             `PCII en Trámite OEMASC sin Acuerdo`+ 
                                                                                                             `PCII en Trámite OEMASC con Acuerdo`+
                                                                                                             `PCII Resueltos OEMASC por Mediación`+
                                                                                                             `PCII Resueltos OEMASC por Conciliación`+
                                                                                                             `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
    `Carpetas sin determinar en su fase inicial`=(`PCII en Trámite Etapa de Investigación`)/
      (`PCII Archivo Temporal`+
         `PCII Abstención de Investigar`+
         `PCII No Ejercicio Acción Penal`+
         `PCII Criterio de Oportunidad`+
         `PCII por Incompetencia`+
         `PCII por  Acumulación`+
         `PCII por Sobreseimiento Ord. Juez Control`+ 
         `PCII por Otra Causa de Extinción Penal`+ 
         `PCII por Otra Decisión/Terminación Código Penal Estatal`+
         `PCII en Trámite Etapa de Investigación`+
         `PCII Vinculadas a Proceso`+
         `PCII en Trámite OEMASC sin Acuerdo`+ 
         `PCII en Trámite OEMASC con Acuerdo`+
         `PCII Resueltos OEMASC por Mediación`+
         `PCII Resueltos OEMASC por Conciliación`+
         `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
    `Carpetas de investigación vinculadas a proceso`=(`PCII Vinculadas a Proceso`)/
      (`PCII Archivo Temporal`+
         `PCII Abstención de Investigar`+
         `PCII No Ejercicio Acción Penal`+
         `PCII Criterio de Oportunidad`+
         `PCII por Incompetencia`+
         `PCII por  Acumulación`+
         `PCII por Sobreseimiento Ord. Juez Control`+ 
         `PCII por Otra Causa de Extinción Penal`+ 
         `PCII por Otra Decisión/Terminación Código Penal Estatal`+
         `PCII en Trámite Etapa de Investigación`+
         `PCII Vinculadas a Proceso`+
         `PCII en Trámite OEMASC sin Acuerdo`+ 
         `PCII en Trámite OEMASC con Acuerdo`+
         `PCII Resueltos OEMASC por Mediación`+
         `PCII Resueltos OEMASC por Conciliación`+
         `PCII Resueltos OEMASC por Junta Restaurativa`)*100,
    `Resolucion de carpetas por órgano jurisdiccional`=(`PVP Cumplida Suspensión Condicional Proc.`+
                                                          `PVP Resueltos OEMASC por Mediación`+
                                                          `PVP Resueltos OEMASC por Conciliación`+
                                                          `PVP Resueltos OEMASC por Junta Restaurativa`+
                                                          `PVP Resueltos por Otros Sobreseimientos`+
                                                          `PVP por Criterio de Oportunidad`+
                                                          `PVP Resueltos  Procedimiento Abreviado`+
                                                          `PVP Resueltos por Juicio Oral`+
                                                          `PVP en Trámite ante el Tribunal Enjuiciamiento`)/
      (`PVP Cumplida Suspensión Condicional Proc.`+              
         `PVP en Trámite ante el Tribunal Enjuiciamiento`+         
         `PVP en Trámite Juez de Control`+                         
         `PVP en Trámite OEMASC con Acuerdo`+                      
         `PVP en Trámite OEMASC sin Acuerdo`+                      
         `PVP en Trámite Procedimiento Abreviado`+                 
         `PVP en Trámite Suspensión Condicional Proc.`+            
         `PVP por Criterio de Oportunidad`+                        
         `PVP Resueltos  Procedimiento Abreviado`+                 
         `PVP Resueltos OEMASC por Conciliación`+                  
         `PVP Resueltos OEMASC por Junta Restaurativa`+            
         `PVP Resueltos OEMASC por Mediación`+                     
         `PVP Resueltos por Juicio Oral`+                          
         `PVP Resueltos por Otros Sobreseimientos`)*100,
    `Carpetas de investigación vinculadas a proceso en trámite`=(`PVP en Trámite Suspensión Condicional Proc.`+
                                                                   `PVP en Trámite OEMASC sin Acuerdo`+
                                                                   `PVP en Trámite OEMASC con Acuerdo`+
                                                                   `PVP en Trámite Juez de Control`+
                                                                   `PVP en Trámite Procedimiento Abreviado`)/
      (`PVP Cumplida Suspensión Condicional Proc.`+              
         `PVP en Trámite ante el Tribunal Enjuiciamiento`+         
         `PVP en Trámite Juez de Control`+                         
         `PVP en Trámite OEMASC con Acuerdo`+                      
         `PVP en Trámite OEMASC sin Acuerdo`+                      
         `PVP en Trámite Procedimiento Abreviado`+                 
         `PVP en Trámite Suspensión Condicional Proc.`+            
         `PVP por Criterio de Oportunidad`+                        
         `PVP Resueltos  Procedimiento Abreviado`+                 
         `PVP Resueltos OEMASC por Conciliación`+                  
         `PVP Resueltos OEMASC por Junta Restaurativa`+            
         `PVP Resueltos OEMASC por Mediación`+                     
         `PVP Resueltos por Juicio Oral`+                          
         `PVP Resueltos por Otros Sobreseimientos`)*100,#Asuntos vinculados a proceso
    `Indice de sentencias condenatorias`=(`Imputados Sentencia Condenatoria Proced. Abreviado`+
                                            `Imputados con Sentencia Condenatoria Juicio Oral`)/
      (`Imputados Sentencia Condenatoria Proced. Abreviado`+
         `Imputados con Sentencia Condenatoria Juicio Oral`+
         `Imputados con Sentencia Absolutoria Juicio Oral`+
         `Imputados Sentencia Absolutoria Proced. Abreviado`)*100,
    `Indice de medidas cautelares impuestas`=(`Imputados con Prisión Preventiva Oficiosa`+
                                                `Imputados con Prisión Preventiva No Oficiosa`+
                                                `Imputados con Otra Medida Cautelar`)/
      (`Imputados con Prisión Preventiva Oficiosa`+
         `Imputados con Prisión Preventiva No Oficiosa`+
         `Imputados con Otra Medida Cautelar`+
         `Imputados Sin Medida Cautelar`)*100#,
  #  `Tasa de internamiento de imputados en prisión preventiva`=(`Población procesada del fuero común`)/(`Población sentenciada del fuero común`+
   #                                                                                                       `Población procesada del fuero común`)*100
    #  (1)*100##Total de población penitenciaria (esta info la podríamos sacar de los cuadernos del OADPRS o de nuestra base de amnistía)
  ) %>% mutate(X="Nacional") %>% 
  select(X, year, 
        `Carpetas de investigación abiertas`:`Indice de medidas cautelares impuestas`)#`Tasa de internamiento de imputados en prisión preventiva`)#`Tasa de internamiento de imputados en prisión preventiva`)
colnames(df_i_nacional) <- colnames(df_i)
df_i <- rbind(df_i, df_i_nacional)
df_i[do.call(cbind, lapply(df_i, is.nan))] <- 0 # controla la división con 0

write.csv(df_i, file=paste0("db/db_indicadores_",anio.trimestre,".csv"), row.names=F)

if(upload){
drive_upload(media=paste0("db/db_indicadores_",anio.trimestre,".csv"),
             path = as_id("1d51wj4Mq2MJiIntS0a65thB1k0YUZyar"),
             overwrite = T)
  print(paste("indicadores creados y actualizados en drive", anio.trimestre))
} else{print(paste("indicadores creados pero no actualizados en drive", anio.trimestre))}
}


sis_ind(download=F, upload = T, 
        anio.trimestre = "2020_II",
        user="**",
        secrets.path = "credentials.json",
        gargle_token.path = "***"
)

