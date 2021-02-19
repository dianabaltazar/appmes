######################################################
######################################################
# Dev: Subdirector MSc Erick René Hernández Cervantes
# Secretaría de Gobernación
# Unidad de Apoyo al Sistema de justicia
# Dirección de evaluación
# Project:  MES ######
# Script:   Unificacion ######
# Notes:  Vamos a hacer una unificación de tres tablas de datos:
#         Entregada por el SENSP
#         Resultado de los cuestionarios gestionados por la unidad:
#         4to trimestre 2019; 1er trimestre de 2020
#         A partir de aquí la tabla será gestionada enteramente por nosotros
#         Es importante mencionar que tomamos como referencia la base de SENSP
#         que aunque tiene muchas columnas no utilizadas, la tomamos porque la
#         App fue desarrollada siguiendo este esquema, pero podemos modificarlo
#         con posterioridad
#####################################################
#####################################################
#----------Packages (uncoment as required) ---------#
options(java.parameters = "-Xmx8000m")
library(tidyverse)
library(readxl)
library(XLConnect)
# library(RDCOMClient)
# library(lubridate)
# library(stringi)
# library(rvest)
# library(docxtractr)
# library(plotly)

# ftf nuestros dataframes de partida
db_mes_sensp <- read_xlsx("datos/BD Meaestra.xlsx",
                          "Concentrado Primera Etapa")
# db_4_2019 <- read_xlsx("datos/database_mes_2019_IV.xlsx",
#                   "database_mes_2019_IV")
# db_1_2020 <- read_xlsx("datos/database_mes_2020_I.xlsx",
#                   "database_mes_2020_I")
db_3_2020 <- read_xlsx("datos/database_mes_2020_III.xlsx",
                       "database_mes_2020_III")


# Modificaciones para db_4_2019 y db_1_2020
# db_4_2019_t <- db_4_2019 %>% 
#   mutate_at(vars(`2017-IV`:`2019-IV`), function(x){as.integer(x)}) %>% 
#   pivot_longer(cols=`2017-IV`:`2019-IV`, names_to = "proto") %>% 
#   separate(proto, c("año", "trim"), "-") %>% select(-trim) %>% 
#   mutate(trim="31-diciembre-2019",
#          `Entidad federativa`=case_when(`Entidad federativa`=="CDMX"~"Ciudad de México",
#                                         `Entidad federativa`=="EdoMex"~"México",
#                                         !`Entidad federativa`%in%c("CDMX","EdoMex")~`Entidad federativa`))

db_3_2020_t <- db_3_2020 %>% 
  mutate_at(vars(`2017-III`:`2020-III`), function(x){as.integer(x)}) %>% 
  pivot_longer(cols=`2017-III`:`2020-III`, names_to = "proto") %>% 
  separate(proto, c("año", "trim"), "-") %>% select(-trim) %>% 
  mutate(trim="30-septiembre-2020",
         `Entidad federativa`=case_when(`Entidad federativa`=="CDMX"~"Ciudad de México",
                                        `Entidad federativa`=="EdoMex"~"México",
                                        !`Entidad federativa`%in%c("CDMX","EdoMex")~`Entidad federativa`))

# Ahora necesitamos hacer una coordinación entre los nombre de las variables
# tanto para la tabla legada como para las nuevas
# write.csv(unique(db_1_2020_t$Variable), "nombres_nuevos.csv")
# write.csv(colnames(db_mes_sensp), "nombres_viejos.csv")

# ahora vamos a introducir los nuevos nombres
new_names <- read.csv("nombres_nuevos_2join.csv")
# los unimos

# db_4_2019_t <- left_join(db_4_2019_t, new_names, 
#                          by=c("Variable"="x"))
# db_4_w <- db_4_2019_t %>%  select(-Variable, -X) %>% 
#   pivot_wider(names_from = New_name) %>% 
#   rename(`08. Entidad Federativa`=`Entidad federativa`,
#          `02. Año`=año,
#          `06. Fecha de Corte`=trim
#          ) %>% 
#   mutate(`00. PK_Consecutivo`=NA,
#          `01. FK_Año`=NA,
#          `03. FK_Fuero`=1,
#          `04. Fuero`="Común",
#          `05. FK_Fecha de Corte`=NA,
#          `07. FK_Entidad Federativa`=NA,
#          `64. PPL Procesada (Prisión Preventiva)`=0,
#          `65. PPL Cumpliendo Condena`=0
#          ) %>% 
#   select(colnames(db_mes_sensp))
### ahora en 2020
db_3_2020_t <- left_join(db_3_2020_t, new_names, 
                         by=c("Variable"="x"))
db_3_w <- db_3_2020_t %>%  select(-Variable, -X) %>% 
  pivot_wider(names_from = New_name) %>% 
  rename(`08. Entidad Federativa`=`Entidad federativa`,
         `02. Año`=año,
         `06. Fecha de Corte`=trim
  ) %>% 
  mutate(`00. PK_Consecutivo`=NA,
         `01. FK_Año`=NA,
         `03. FK_Fuero`=1,
         `04. Fuero`="Común",
         `05. FK_Fecha de Corte`=NA,
         `07. FK_Entidad Federativa`=NA,
         `64. PPL Procesada (Prisión Preventiva)`=0,
         `65. PPL Cumpliendo Condena`=0 # porque si esta esta info
      
  ) %>% 
  select(colnames(db_mes_sensp))

## ahora unimos y listo
db_new <- rbind(db_mes_sensp, db_3_w)

### ahora escribimos en excel y calculamos los indicadores
x <- XLConnect::loadWorkbook("datos/BD Maestra actualizacion 3 2020.xlsx", create=T)
XLConnect::createSheet(x, "Concentrado Primera Etapa")
XLConnect::writeWorksheet(x, db_new, "Concentrado Primera Etapa")
XLConnect::saveWorkbook(x, "datos/BD Meaestra actualizacion 3 2020.xlsx")

unity <- function(cuestionaire, data_base, output_name){
  
}
### indicadores
db_new <- readxl::read_xlsx("datos/BD Maestra actualizacion 3 2020_d.xlsx")
db_new$`50. PVP Resueltos por Juicio Oral` <- as.integer(db_new$`50. PVP Resueltos por Juicio Oral`)
db_ind <- db_new %>% mutate(
  #indicador 1
  `66. Indicador 1: Carpetas de investigación iniciadas`=
    `11. CII por DQO Año Vigente`/(`09. Denuncias`+`10. Querellas u Otros Requisitos`)*100
    
    ,
  #indicador 2
  `67. Indicador 2: Carpetas de investigación determinadas por el Ministerio Público`=
    (`26. PCII Archivo Temporal`+`27. PCII Abstención de Investigar`+
       `28. PCII No Ejercicio Acción Penal`+`29. PCII Criterio de Oportunidad`+
       `30. PCII por Incompetencia`+`31. PCII por  Acumulación`+
       `33. PCII por Otra Causa de Extinción Penal`+`34. PCII por Otra Decisión/Terminación Código Penal Estatal`)/
    (`26. PCII Archivo Temporal`+`27. PCII Abstención de Investigar`+
       `28. PCII No Ejercicio Acción Penal`+`29. PCII Criterio de Oportunidad`+
       `30. PCII por Incompetencia`+`31. PCII por  Acumulación`+
       `32. PCII por Sobreseimiento Ord. Juez Control`+`33. PCII por Otra Causa de Extinción Penal`+
       `34. PCII por Otra Decisión/Terminación Código Penal Estatal`+`35. PCII en Trámite Etapa de Investigación`+
       `36. PCII Vinculadas a Proceso`+`37. PCII en Trámite OEMASC sin Acuerdo`+
       `38. PCII en Trámite OEMASC con Acuerdo`+`39. PCII Resueltos OEMASC por Mediación`+
       `40. PCII Resueltos OEMASC por Conciliación`+`41. PCII Resueltos OEMASC por Junta Restaurativa`)*100
    ,
  #indicador3
  `68. Indicador 3: Resolución de carpetas de investigación por acuerdos reparatorios (sede ministerial)`=
    (`37. PCII en Trámite OEMASC sin Acuerdo`+`38. PCII en Trámite OEMASC con Acuerdo`+
       `39. PCII Resueltos OEMASC por Mediación`+`40. PCII Resueltos OEMASC por Conciliación`+
       `41. PCII Resueltos OEMASC por Junta Restaurativa`)/
    (`26. PCII Archivo Temporal`+`27. PCII Abstención de Investigar`+
       `28. PCII No Ejercicio Acción Penal`+`29. PCII Criterio de Oportunidad`+
       `30. PCII por Incompetencia`+`31. PCII por  Acumulación`+
       `32. PCII por Sobreseimiento Ord. Juez Control`+`33. PCII por Otra Causa de Extinción Penal`+
       `34. PCII por Otra Decisión/Terminación Código Penal Estatal`+`35. PCII en Trámite Etapa de Investigación`+
       `36. PCII Vinculadas a Proceso`+`37. PCII en Trámite OEMASC sin Acuerdo`+
       `38. PCII en Trámite OEMASC con Acuerdo`+`39. PCII Resueltos OEMASC por Mediación`+
       `40. PCII Resueltos OEMASC por Conciliación`+`41. PCII Resueltos OEMASC por Junta Restaurativa`)*100
  ,
  #indicador4
  `69. Indicador 4: Carpetas de investigación sin determinar en fase inicial`=
    (`35. PCII en Trámite Etapa de Investigación`)/
    (`26. PCII Archivo Temporal`+`27. PCII Abstención de Investigar`+
       `28. PCII No Ejercicio Acción Penal`+`29. PCII Criterio de Oportunidad`+
       `30. PCII por Incompetencia`+`31. PCII por  Acumulación`+
       `32. PCII por Sobreseimiento Ord. Juez Control`+`33. PCII por Otra Causa de Extinción Penal`+
       `34. PCII por Otra Decisión/Terminación Código Penal Estatal`+`35. PCII en Trámite Etapa de Investigación`+
       `36. PCII Vinculadas a Proceso`+`37. PCII en Trámite OEMASC sin Acuerdo`+
       `38. PCII en Trámite OEMASC con Acuerdo`+`39. PCII Resueltos OEMASC por Mediación`+
       `40. PCII Resueltos OEMASC por Conciliación`+`41. PCII Resueltos OEMASC por Junta Restaurativa`)*100
  ,
  #indi 5
  `70. Indicador 5: Carpetas de investigación vinculadas a proceso`=
    (`36. PCII Vinculadas a Proceso`)/
    (`26. PCII Archivo Temporal`+`27. PCII Abstención de Investigar`+
       `28. PCII No Ejercicio Acción Penal`+`29. PCII Criterio de Oportunidad`+
       `30. PCII por Incompetencia`+`31. PCII por  Acumulación`+
       `32. PCII por Sobreseimiento Ord. Juez Control`+`33. PCII por Otra Causa de Extinción Penal`+
       `34. PCII por Otra Decisión/Terminación Código Penal Estatal`+`35. PCII en Trámite Etapa de Investigación`+
       `36. PCII Vinculadas a Proceso`+`37. PCII en Trámite OEMASC sin Acuerdo`+
       `38. PCII en Trámite OEMASC con Acuerdo`+`39. PCII Resueltos OEMASC por Mediación`+
       `40. PCII Resueltos OEMASC por Conciliación`+`41. PCII Resueltos OEMASC por Junta Restaurativa`)*100
    ,
  #indi6
  `71. Indicador 6: Resolución de carpetas de investigación por Órgano Jurisdiccional`=
    (`45. PVP Cumplida Suspensión Condicional Proc.`+`53. PVP Resueltos OEMASC por Mediación`+
       `54. PVP Resueltos OEMASC por Conciliación`+`55. PVP Resueltos OEMASC por Junta Restaurativa`+
       `46. PVP Resueltos por Otros Sobreseimientos`+`43. PVP por Criterio de Oportunidad`+
       `48. PVP Resueltos  Procedimiento Abreviado`+`50. PVP Resueltos por Juicio Oral`+
       `49. PVP en Trámite ante el Tribunal Enjuiciamiento`)/
    (`42. PVP en Trámite Juez de Control`+`43. PVP por Criterio de Oportunidad`+
       `44. PVP en Trámite Suspensión Condicional Proc.`+`45. PVP Cumplida Suspensión Condicional Proc.`+
       `46. PVP Resueltos por Otros Sobreseimientos`+`47. PVP en Trámite Procedimiento Abreviado`+
       `48. PVP Resueltos  Procedimiento Abreviado`+`49. PVP en Trámite ante el Tribunal Enjuiciamiento`+
       `50. PVP Resueltos por Juicio Oral`+`51. PVP en Trámite OEMASC sin Acuerdo`+
       `52. PVP en Trámite OEMASC con Acuerdo`+`53. PVP Resueltos OEMASC por Mediación`+
       `54. PVP Resueltos OEMASC por Conciliación`+`55. PVP Resueltos OEMASC por Junta Restaurativa`)*100
    ,
  #indi7
  `72. Indicador 7: Carpetas de investigación vinculadas a proceso en trámite`=
    (`45. PVP Cumplida Suspensión Condicional Proc.`+`51. PVP en Trámite OEMASC sin Acuerdo`+
       `52. PVP en Trámite OEMASC con Acuerdo`+`42. PVP en Trámite Juez de Control`+
       `47. PVP en Trámite Procedimiento Abreviado`)/
    (`42. PVP en Trámite Juez de Control`+`43. PVP por Criterio de Oportunidad`+
       `44. PVP en Trámite Suspensión Condicional Proc.`+`45. PVP Cumplida Suspensión Condicional Proc.`+
       `46. PVP Resueltos por Otros Sobreseimientos`+`47. PVP en Trámite Procedimiento Abreviado`+
       `48. PVP Resueltos  Procedimiento Abreviado`+`49. PVP en Trámite ante el Tribunal Enjuiciamiento`+
       `50. PVP Resueltos por Juicio Oral`+`51. PVP en Trámite OEMASC sin Acuerdo`+
       `52. PVP en Trámite OEMASC con Acuerdo`+`53. PVP Resueltos OEMASC por Mediación`+
       `54. PVP Resueltos OEMASC por Conciliación`+`55. PVP Resueltos OEMASC por Junta Restaurativa`)*100
    ,
  #indi8
  `73. Indicador 8: Sentencias condenatorias`=
    (`60. Imputados Sentencia Condenatoria Proced. Abreviado`+`62. Imputados con Sentencia Condenatoria Juicio Oral`)/
    (`60. Imputados Sentencia Condenatoria Proced. Abreviado`+`62. Imputados con Sentencia Condenatoria Juicio Oral`+
       `61. Imputados Sentencia Absolutoria Proced. Abreviado`+`63. Imputados con Sentencia Absolutoria Juicio Oral`)*100
    ,
  #indi9
  `74. Indicador 9: Medidas cautelares impuestas`=
    (`56. Imputados con Prisión Preventiva Oficiosa`+`57. Imputados con Prisión Preventiva No Oficiosa`+
       `58. Imputados con Otra Medida Cautelar`)/
    (`56. Imputados con Prisión Preventiva Oficiosa`+`57. Imputados con Prisión Preventiva No Oficiosa`+
       `58. Imputados con Otra Medida Cautelar`+`59. Imputados Sin Medida Cautelar`)*100
    ,
  #indi10
  `75. Indicador 10: Porcentaje de internamiento de imputados en prisión preventiva`=
    (`64. PPL Procesada (Prisión Preventiva)`)/
    (`64. PPL Procesada (Prisión Preventiva)`+`65. PPL Cumpliendo Condena`)*100
)

z <- XLConnect::loadWorkbook("datos/indicadores 3 2020_dian.xlsx", create=T)
XLConnect::createSheet(z, "indicadores")
XLConnect::writeWorksheet(z, db_ind, "indicadores")
XLConnect::saveWorkbook(z, "datos/indicadores 3 2020_dian.xlsx")


