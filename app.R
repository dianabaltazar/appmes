##### Dev. Subdirector MSc Erick René Hernández Cervantes #####
##### Secretaría de Gobernación ###################
##### Project: MES-App ############
##### Script: App#####
##### Notes: Whole server and ui
##### Packages {uncoment as required}

library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(htmltools)


library(shiny)
library(rintrojs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(waiter)


#########################################
#########################################
#
# Seccion de funciones y constantes
#
#########################################
#########################################

esquema_treemap <- function(df, colores, etapa, anio_carpetas, fecha_corte, nacional = T, entidad = NULL) {
  if (nacional) {
    df_aux_etapa <- df %>%
      filter(`Fecha de Corte` == fecha_corte & .[[3]] == anio_carpetas) %>%
      select(`Entidad Federativa`, all_of(etapa)) %>%
      pivot_longer(cols = all_of(etapa), names_to = "var")
    aux_row <- df_aux_etapa %>%
      group_by(var) %>%
      summarize(value = sum(value, na.rm=T)) %>%
      mutate(`Entidad Federativa` = var, var = "") %>%
      select(`Entidad Federativa`, var, value)
    
    n <- length(etapa)
    unique.names <- c()
    for (i in 1:n) {
      space <- paste(rep("", i), collapse = " ")
      unique.names <- append(unique.names, space)
    }
    
    df_aux_etapa <- df_aux_etapa %>%
      mutate(`Entidad Federativa` = paste0(`Entidad Federativa`, c(rep(unique.names, 32))))
    # unique names in branches for a correct selection
    db_plot <- rbind(df_aux_etapa, aux_row)
    
    db_plot[is.na(db_plot)] <- 0
    
    p <- plot_ly(db_plot,
                 type = "treemap",
                 textinfo = "label+value+percent parent",
                 labels = ~`Entidad Federativa`,
                 text = ~`Entidad Federativa`,
                 values = ~value,
                 parents = ~var,
                 branchvalues = "total",
                 domain = list(column = 0, row = 0),
                 # maxdepth=3,
                 marker = list(colors = c(sort(rep(colores, n))))
    ) %>% layout(
      title = paste0("Nivel Nacional; apertura de carpetas : ", anio_carpetas, "; fecha de corte: ", fecha_corte),
      height = 700
    )
    return(p)
  } else if (!is.null(entidad)) {
    df_aux_etapa <- df %>%
      filter(`Fecha de Corte` == fecha_corte & .[[3]] == anio_carpetas &
               `Entidad Federativa` == entidad) %>%
      select(`Entidad Federativa`, all_of(etapa)) %>%
      pivot_longer(cols = all_of(etapa), names_to = "var")
    aux_row <- df_aux_etapa %>%
      group_by(`Entidad Federativa`) %>%
      summarize(value = sum(value, na.rm=T)) %>%
      mutate(var = "") %>%
      select(`Entidad Federativa`, var, value)
    db_plot <- rbind(df_aux_etapa, aux_row)
    db_plot[is.na(db_plot)] <- 0
    n <- length(etapa)
    p <- plot_ly(db_plot,
                 type = "treemap",
                 textinfo = "label+value+percent parent",
                 labels = ~var,
                 text = ~`Entidad Federativa`,
                 values = ~value,
                 parents = ~NA,
                 branchvalues = "total",
                 domain = list(column = 0, row = 0),
                 # maxdepth=3,
                 marker = list(colors = c(sample(colores, n)))
    ) %>% layout(
      title = paste0(entidad, "; apertura de carpetas: ", anio_carpetas, "; fecha de corte: ", fecha_corte),
      height = 700
    )
    return(p)
  } else {
    return(NULL)
  }
}
resumen_spiderchart <- function(df, anio_carpetas, fecha_corte) {
  db_nacional <- df %>%
    filter(`Fecha de Corte` == fecha_corte & .[[3]] == anio_carpetas) %>%
    select(
      `Entidad Federativa`,
      67:76
    ) %>%
    pivot_longer(cols = 2:11, names_to = "var")
  db_nacional_1 <- db_nacional %>%
    filter(`Entidad Federativa` == "Aguascalientes")
  states <- unique(db_nacional$`Entidad Federativa`)
  
  q <- plot_ly(db_nacional_1,
               type = "scatterpolar",
               mode = "closest",
               fill = "toself"
               
  ) %>%
    add_trace(
      r = ~value,
      theta = paste("indicador", seq(1, 10, 1)),
      showlegend = TRUE,
      mode = "markers",
      name = "Aguascalientes"
      ,
      hovertemplate="Porcentaje %{r:.2f}<br> <b>%{theta}</b>"
    ) %>%
    layout(
      polar = list(
        bgcolor = "white",
        angularaxis = list(
          tickwidth = 1,
          linewidth = 3,
          layer = "below traces"
        )
      ),
      radialaxis = list(
        side = "counterclockwise",
        showline = T,
        linewidth = 2,
        tickwidth = 2,
        gridcolor = "#FFF",
        gridwidth = 2
      ),
      height = 700 # controla el alto de figura
    )
  for (i in states) {
    if (!i == "Aguascalientes") {
      db_aux <- db_nacional %>%
        filter(`Entidad Federativa` == i)
      q <- add_trace(q,
                     type = "scatterpolar",
                     mode = "closest",
                     fill = "toself",
                     r = db_aux$value,
                     theta = paste("indicador", seq(1, 10, 1)),
                     showlegend = TRUE,
                     mode = "markers",
                     name = i,
                     inherit = F,
                     hovertemplate="Porcentaje %{r:.2f}<br> <b>%{theta}</b>"
      )
    }
  }
  return(q)
}
mapa_indicadores <- function(df, map_inegi, anio_carpetas, fecha_corte, indicador) {
  db_nacional <- db_ind %>%
    filter(`Fecha de Corte` == fecha_corte & .[[3]] == anio_carpetas) %>%
    select(
      region,
      67:76
    ) %>%
    pivot_longer(cols = 2:11, names_to = "var") %>%
    filter(var == indicador)
  edos <- map_inegi %>%
    left_join(db_nacional, by = c("cve_edo" = "region"))
  edos_small <- edos %>%
    mutate(as.numeric(value))
  
  bins <- seq(0, 100, 10)
  pal <- colorBin("YlOrRd", domain = edos_small$value, bins = bins)
  labels <- paste(edos_small$nombre, "<br><b>", round(edos_small$value, 2), "%</b>") %>% lapply(htmltools::HTML)
  
  mapita <- edos_small %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~ pal(as.integer(value)),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels
    ) %>%
    addLegend(pal = pal, value = ~value, title = htmltools::HTML(paste0(strwrap(indicador, 21, prefix = "<br>"))))
  return(mapita)
}
bar_variables <- function(df, variables, fecha_corte, anio_carpetas, colores) {
  y <- switch(variables,
              "Victimas" = Victimas,
              "Detenidos" = Detenidos,
              "Órdenes de aprehensión" = `Ordenes de aprehension`,
              "Órdenes de detención" = `Ordenes de detencion`
  )
  
  if (is.null(y)) {
    db_df_bar <- df %>%
      filter(`Fecha de Corte` == fecha_corte & .[[3]] == anio_carpetas) %>%
      select(`Entidad Federativa`, all_of(variables))
  } else if (!is.null(y)) {
    db_df_bar <- df %>%
      filter(`Fecha de Corte` == fecha_corte & .[[3]] == anio_carpetas) %>%
      select(`Entidad Federativa`, all_of(y))
  }
  
  tracs <- ncol(db_df_bar)
  #  db_df_bar <- db_df_bar %>% pivot_longer(-`Entidad Federativa`, n)
  if(tracs>2){ # fucking 2
    pp <- plot_ly(db_df_bar,
                  x = ~`Entidad Federativa`,
                  y = ~ get(colnames(db_df_bar)[2]), type = "bar", name = colnames(db_df_bar)[2],
                  marker = list(color = sample(colores, 1))
    )%>% add_trace(
      y = ~ get(colnames(db_df_bar)[3]), name = colnames(db_df_bar)[3],
      type = "bar",
      marker = list(color = sample(colores, 1))
    )
    if (tracs > 3) {
      for (i in 4:tracs) {
        pp <- pp %>% add_trace(
          y = ~ get(colnames(db_df_bar)[i]), name = colnames(db_df_bar)[i],
          type = "bar",
          marker = list(color = sample(colores, 1))
        )
      }
    }
  }else{
    pp <- plot_ly(db_df_bar,
                  x = ~`Entidad Federativa`,
                  y = ~ get(colnames(db_df_bar)[2]), type = "bar", name = colnames(db_df_bar)[2],
                  marker = list(color = sample(colores, 1))
    )}
  
  pp <- pp %>% layout(
    xaxis = list(title = "", tickangle = -90),
    yaxis = list(title = "", tickformat = ",d"),
    barmode = "dodge",
    legend = list(
      orientation = "h", # show entries horizontally
      xanchor = "center", # use center of legend as anchor
      x = 0.5,
      y = 1
    ),
    height = 700
  )
  return(pp)
}

jbalvin <- c(
  "#94F1BB", # Celadon (verde pistache)
  "#D6BEF7", # Mauve (lavanda pastel casi)
  "#A0B8B0", # Opal (gris verdoso muy pastel)
  "#D64D76", # Mystic (rosa mexicano apastelado)
  "#DDE8DA", # Honeydew (blanco ostion)
  "#70BCFF", # Maya Blue (azul cielo)
  "#DB8030", # Bronze (naranja pastel)
  "#AD6442", # Brown sugar (cafe clarito)
  "#ECF5DF", # Beige (beige)
  "#FCDBA2", # Navajo White (entre amarillo y cafe claro)
  "#99F0D0", # Magic Mint (verde menta)
  "#4F5A66", # Black coral (Gris)
  "#F5BC7F", # Mellow Apricot (Durazno)
  "#ADAC49", # Olive Green (verde seco)
  "#5874B0", # Blue Yonder (azul acero)
  "#6B8E23", # Mantis (verde bonito)
  "#D4AF28", # Gold Metallic (amarillo mango)
  "#7A2235", # Claret (Vino Morena)
  "#6C8745", # Maximum Green (Verde militar)
  "#507A80", # ming (azul grisaceo)
  "#871A2E", # Antique Ruby (vino deslabado)
  "#D7F296", # Yellow green crayola (amarillo verdoso)
  "#DBAA91", # Tumbleweed (Rosa cafesoso)
  "#5C394E", # Eggplant (Morado no brillante)
  "#C0B8E0", # Lavander Blue (Moradito pastel)
  "#2F9680", # Illuminating Emerald (Verde esmeralda pastel)
  "#159EB0", # Blue Munsell (Azul bonito)
  "#944432", # Chestnut (Cafe nueces)
  "#F29D35", # Carrot Orange (Naranja clarito)
  "#B3F1FC", # Blizzard Blue (Azul celeste)
  "#6C8C7F", # Xanadu (Verde acero)
  "#6B2A54" # Byzantium (Morado de mi cama)
)
`Etapa 1` <- substring(c("09. Denuncias", "10. Querellas u Otros Requisitos"),5)
`Etapa 2` <- substring(c("16. CII con Detenido en Flagrancia", "17. CII sin Detenido"),5)
`Etapa 3` <- substring(c(
  "26. PCII Archivo Temporal",
  "27. PCII Abstención de Investigar",
  "28. PCII No Ejercicio Acción Penal",
  "29. PCII Criterio de Oportunidad",
  "30. PCII por Incompetencia",
  "31. PCII por  Acumulación",
  "33. PCII por Otra Causa de Extinción Penal",
  "34. PCII por Otra Decisión/Terminación Código Penal Estatal"
),5)
`Etapa 4` <- substring(c(
  "37. PCII en Trámite OEMASC sin Acuerdo",
  "38. PCII en Trámite OEMASC con Acuerdo",
  "39. PCII Resueltos OEMASC por Mediación",
  "40. PCII Resueltos OEMASC por Conciliación",
  "41. PCII Resueltos OEMASC por Junta Restaurativa"
),5)
`Etapa 5` <- substring(c(
  "35. PCII en Trámite Etapa de Investigación",
  "36. PCII Vinculadas a Proceso",
  "42. PVP en Trámite Juez de Control",
  "49. PVP en Trámite ante el Tribunal Enjuiciamiento",
  "32. PCII por Sobreseimiento Ord. Juez Control",
  "50. PVP Resueltos por Juicio Oral"
),5)
`Etapa 6` <- substring(c(
  "44. PVP en Trámite Suspensión Condicional Proc.",
  "48. PVP Resueltos  Procedimiento Abreviado",
  "45. PVP Cumplida Suspensión Condicional Proc.",
  "47. PVP en Trámite Procedimiento Abreviado",
  "43. PVP por Criterio de Oportunidad",
  "46. PVP Resueltos por Otros Sobreseimientos"
),5)
`Etapa 7` <- substring(c(
  "51. PVP en Trámite OEMASC sin Acuerdo",
  "52. PVP en Trámite OEMASC con Acuerdo",
  "53. PVP Resueltos OEMASC por Mediación",
  "54. PVP Resueltos OEMASC por Conciliación",
  "55. PVP Resueltos OEMASC por Junta Restaurativa"
),5)

Victimas <- substring(c("14. Víctimas u Ofendidos Hombres", "13. Víctimas u Ofendidos Mujeres", "15. Otros Tipos Víctimas u Ofendidos"),5)
Detenidos <- substring(c("23. Detenidos en Flagrancia", "25. Detenidos por Caso Urgente", "24. Detenidos por Orden de Aprehensión"),5)
`Ordenes de aprehension` <- substring(c(
  "20. Órdenes de Aprehensión Cumplimentadas",
  "19. Órdenes de Aprehensión Ordenadas",
  "18. Órdenes de Aprehensión Solicitadas"
),5)
`Ordenes de detencion` <- substring(c(
  "21. Órdenes Detención Caso Urgente Emitidas",
  "22. Órdenes Detención Caso Urgente Cumplimentadas"
),5)
#########################################
#########################################
#
# Section estática de carga de información y opciones
#
#########################################
#########################################

def_df_var <- readxl::read_xlsx("data/definiciones_c.xlsx", sheet = "Variables")
def_df_ind <- readxl::read_xlsx("data/definiciones_c.xlsx", sheet = "Indicadores")
db_df <- readxl::read_xlsx("data/BD Maestra.xlsx",
                           sheet = "Concentrado Primera Etapa"
)
# quitar los na por cero que causan problema al desplegar info
db_df$`08. Entidad Federativa`[db_df$`08. Entidad Federativa`=="México"] <- "Estado de México"

db_df$`09. Denuncias`[is.na(db_df$`09. Denuncias`)] <- 0
db_df$`10. Querellas u Otros Requisitos`[is.na(db_df$`10. Querellas u Otros Requisitos`)] <- 0
db_df[, 12][is.na(db_df[, 12])] <- 0
db_df[, 13][is.na(db_df[, 13])] <- 0
db_df$`16. CII con Detenido en Flagrancia`[is.na(db_df$`16. CII con Detenido en Flagrancia`)] <- 0
db_df$`17. CII sin Detenido`[is.na(db_df$`17. CII sin Detenido`)] <- 0
# esto en algun momento debe ir por fuera
db_df$`50. PVP Resueltos por Juicio Oral` <- as.numeric(db_df$`50. PVP Resueltos por Juicio Oral`)
# db_df$`35. PCII en Trámite Etapa de Investigación` <- as.numeric(db_df$`50. PVP Resueltos por Juicio Oral`)
db_df <- db_df %>% magrittr::set_colnames(substring(colnames(db_df),5))

varlist <- colnames(db_df)[10:ncol(db_df)]



db_ind <- readxl::read_xlsx("data/indicadores.xlsx",
                            sheet = "indicadores")
db_ind <- db_ind %>% magrittr::set_colnames(substring(colnames(db_ind),5))

codigos <- read_csv("data/Codigos_INEGI.csv")
db_ind <- left_join(db_ind, codigos, by = c("Entidad Federativa"="Entidad Federativa"))
edos <-
  st_read("data/mapa/shp/edos_ine_2018/edos_ine_2018.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(cve_edo = str_pad(entidad, 2, pad = "0")) %>%
  mutate(cve_edo = as.numeric(cve_edo))



entities <- unique(db_df$`Entidad Federativa`)
trims <- rev(unique(db_df$`Fecha de Corte`))
indicadores <- colnames(db_ind)[grepl("Indicador", colnames(db_ind))]



##
#########################################
#########################################
#
# Inicia el user interface con un navbarpage que se compondra de n tab Panels
#
#########################################
#########################################
ui <- navbarPage(
  id = "intabset", # needed for landing page
  
  title = div(tags$a(img(src = "https://upload.wikimedia.org/wikipedia/commons/a/ad/SEGOB.jpg", height = 40), href = "https://justicia.segob.gob.mx/"),
              style = "position: relative; top: -5px;"
  ), # Navigation bar
  windowTitle = "MES-UASJ", # title for browser tab
  # includeCSS("C:/Users/ehernandez/Documents/Subdireccion/Proyectos propios/Desarrollo/App-Mes/gb/css/main.min.css"),
  #  includeCSS("https://framework-gb.cdn.gob.mx/assets/styles/main.css"),
  
  theme = "https://framework-gb.cdn.gob.mx/assets/styles/main.css",
  # theme=shinytheme(theme="united"),#
  collapsible = TRUE, # tab panels collapse into menu in small screens
  
  header = # oficial html code for header
    tags$head(HTML('<header>
        <nav class="navbar navbar-inverse navbar-fixed-top navbar">
            <div class="container">
                <div class="navbar-header">
                    <button type="button" class="navbar-toggle collapsed" data-target="#navbarMainCollapse" data-toggle="collapse">
                        <span class="sr-only">Interruptor de Navegación</span>
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                        <span class="icon-bar"></span>
                    </button>
                    <a href="/" style="padding: 1px 3px 3px 3px;" class="navbar-brand " title="Ir a la página inicial">
                   
                    <div class="logo-main"></div>
                   </a>
                </div>
                <div id="navbarMainCollapse" class="collapse navbar-collapse">
                    <ul class="nav navbar-nav navbar-right">
                      <li><a href="https://coronavirus.gob.mx/" title="Trámites">Información importante Coronavirus COVID-19</a></li>
                        <li><a href="https://www.gob.mx/tramites" title="Trámites">Trámites </a></li>
                        <li><a href="https://www.gob.mx/gobierno" title="Gobierno">Gobierno</a></li>
                        <li>
                            <form accept-charset="UTF-8" action="/busqueda" method="get"><div style="display:none"><input name="utf8" type="hidden" value="✓"></div>
                                <input type="hidden" name="site" value="segob">
                                 <button data-v-6394873e="" id="botonbuscar" type="button" class="btn btn-search"><a data-v-6394873e="" href="https://www.gob.mx/busqueda?utf8=✓" target="_blank" id="botbusca"><img data-v-6394873e="" alt="Buscar" src="https://framework-gb.cdn.gob.mx/landing/img/lupa.png"></a></button>
                                
</form>                        </li>
                    </ul>
                </div>
            </div>
        </nav>
        


            



    </header>')),
  #########################################
  #########################################
  #
  # Tab Panel inicio: Descripción de los indicadores, texto y nada mas
  #
  #########################################
  #########################################
  tabPanel("Inicio",
           icon = icon("university"), value = "inicio",
           
           wellPanel(fluidRow(
             mainPanel(
               width = 15,
               h4("Modelo de Evaluación y Seguimiento de la Consolidación del Sistema de Justicia Penal", style = "color:black;"),
               p("De conformidad con el Acuerdo 06/XLV/19 del Consejo Nacional de Seguridad Pública, en su Cuadragésima Quinta Sesión Ordinaria, celebrada el 18 de diciembre de 2019, la Unidad de Apoyo al Sistema de Justicia (UASJ) funge como enlace operativo del poder ejecutivo, e integrante de la Instancia Tripartita Coordinadora de la Consolidación del Sistema de Justicia Penal y enlace de coordinación entre las autoridades locales y federales para el fortalecimiento y consolidación del Sistema de Justicia Penal."),
               br(),
               p("De igual forma, se estableció que la Unidad de Apoyo al Sistema de Justicia de la Secretaría de Gobernación estaría a cargo del Modelo de Evaluación y Seguimiento de la Consolidación del Sistema de Justicia Penal (MES)."),
               br(),
               p(HTML("Los <b>diez indicadores estratégicos</b> del Modelo de Evaluación y Seguimiento de la Consolidación del
Sistema de Justicia Penal aprobados por el Consejo Nacional de Seguridad Pública son:")),
               tags$ol(
                 tags$li("Carpetas de investigación iniciadas"),
                 tags$li("Carpetas de investigación determinadas por el Ministerio Público"),
                 tags$li("Resolución de carpetas de investigación por acuerdos reparatorios (sede ministerial)"),
                 tags$li("Carpetas de investigación sin determinar en fase inicial"),
                 tags$li("Carpetas de investigación vinculadas a proceso"),
                 tags$li("Resolución de carpetas de investigación por Órgano Jurisdiccional"),
                 tags$li("Carpetas de investigación vinculadas a proceso en trámite"),
                 tags$li("Sentencias condenatorias"),
                 tags$li("Medidas cautelares impuestas"),
                 tags$li("Porcentaje de internamiento de imputados en prisión preventiva")
               )
             ),
             br(),
             actionButton("ayuda", label = "Guía de Usuario")
           ))
  ), # Tab panel bracket
  #########################################
  #########################################
  #
  # Tab Panel: Esquema, seccion de treemaps para los stocks de corte trimestral del MES proceso penal
  #
  #########################################
  #########################################
  tabPanel("Esquema",
           icon = icon("cubes"), value = "esq",
           selected = NA,
           wellPanel(
             fluidRow(
               div(
                 title = "Seleccionar una etapa",
                 radioGroupButtons("esquema",
                                   status = "primary", ##### identificador del boton
                                   choices = c(
                                     "Denuncias y <br>Querellas"="Etapa 1",
                                     "Carpetas <br>de investigación<br> "="Etapa 2",
                                     "Determinaciones del<br> Ministerio Público"="Etapa 3",
                                     "Mecanismos Alternativos <br>Sede Ministerial"="Etapa 4",
                                     "Estatus de <br>los procedimientos"="Etapa 5",
                                     "Determinaciones de <br>los Tribunales"="Etapa 6",
                                     "Mecanismos Alternativos <br>Sede Judicial"="Etapa 7"
                                   ),
                                   selected = "Etapa 1",
                                   label = "1. Seleccione la etapa del proceso penal:"
                 )
               ),
               div(
                 title = "Desplegar información a nivel nacional o por entidad federativa",
                 radioGroupButtons("nac_o_ef_etapa",
                                   status = "primary", ##### identificador del boton
                                   choices = c("A Nivel Nacional", "Por Entidad Federativa"),
                                   selected = "A Nivel Nacional",
                                   label = "2. Seleccione la opción que prefiera para visualizar la información:"
                 )
               ),
               
               selectInput("trims_etapa", "3. Seleccione una fecha de corte de la información:", ### input de etapa para
                           trims,
                           multiple = F,
                           selected = NA
               ),
               uiOutput("ui_cond_carpeta_denuncia_etapa"),
               conditionalPanel(
                 condition = 'input.nac_o_ef_etapa == "Por Entidad Federativa"',
                 div(
                   style = "display:inline-block",
                   title = "Seleccione las opciones por entidad federativa",
                   br(),
                   selectInput("Entidad", "5. Seleccione una Entidad Federativa:", ### input de entidad para
                               entities,
                               multiple = F,
                               selected = NA
                   )
                 )
               ),
               # actionButton("calcular_esquema", "Graficar"),
               ########## Condicional panel for E1
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 1"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 1: Denuncias, querellas y otros requisitos equivalentes"),
                   br(),
                   HTML("La investigación de un delito puede darse por dos causas:
<ol>
<li>por una denuncia, querella o requisito equivalente</li>
<li>por una detención de un delito cometido, ya sea en flagrancia o por caso urgente. En los casos de detención en flagrancia o caso urgente es indispensable que medie una denuncia, querella o acto equivalente</li></ol>") #### area para renderear el plot treemap
                 ) # div bracket
               ), # bracket conditional panel Et
               ######## Conditional panel for E2
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 2"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 2: Apertura de carpetas de investigación"),
                   br(),
                   HTML("Al cometerse un delito, se apertura una carpeta para iniciar su investigación.")
                   #### area para renderear el plot treemap
                 ) # div bracket
               ), # bracket conditional panel Et
               ######## Conditional panel for E3
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 3"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 3: Decisiones y determinaciones del Ministerio Público"),
                   br(),
                   HTML("El Ministerio Público puede decretar el no ejercicio de la acción penal cuando los antecedentes del caso le permitan concluir que en el caso concreto se actualiza alguna de las causales de sobreseimiento. De igual forma, puede abstenerse de investigar, archivar temporalmente, aplicar un criterio de oportunidad o realizar alguna otra determinación.")
                   #### area para renderear el plot treemap
                 ) # div bracket
               ), # bracket conditional panel Et
               ######## Conditional panel for E4
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 4"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 4: Procedimientos derivados al OEMASC"),
                   br(),
                   HTML("Procedimientos derivados de las carpetas de investigación que fueron dirigidos a los Órganos Especializados en Mecanismos Alternativos de Solución de Controversias en Materia Penal (OEMASC) dependientes de las Procuradurías Generales de Justicia o Fiscalías Generales de las entidades federativas (desde su inicio hasta el auto de vinculación a proceso).")
                   #### area para renderear el plot treemap
                 ) # div bracket
               ), # bracket conditional panel Et
               ######## Conditional panel for E5
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 5"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 5: Estatus general de los procedimientos"),
                   br(),
                   HTML("Procedimientos generados a partir de las vinculaciones a proceso derivadas de las carpetas de investigación")
                   #### area para renderear el plot treemap
                 ) # div bracket
               ), # bracket conditional panel Et
               ######## Conditional panel for E6
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 6"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 6: Otras determinaciones de los jueces"),
                   br(),
                   HTML("Los jueces de control pueden realizar ciertas determinaciones a lo largo del Proceso Penal. En la siguiente gráfica se muestra el estatus de dichas determinaciones, correspondientes a los procedimientos generados de las vinculaciones a proceso derivadas de las carpetas de investigación")
                   #### area para renderear el plot treemap
                 ) # div bracket
               ), # bracket conditional panel Et
               ######## Conditional panel for E7
               conditionalPanel(
                 condition = 'input.esquema == "Etapa 7"',
                 div(
                   style = "display:inline-block",
                   title = NA,
                   br(),
                   h2("Etapa 7: OEMASC sede judicial"),
                   br(),
                   HTML("Procedimientos generados de las vinculaciones a proceso que fueron derivados al Órgano Especializado en Mecanismos Alternativos de Solución de Controversias en materia Penal (OEMASC) dependiente del Poder Judicial (en caso de ausencia de éste los estados presentaron el OEMASC dependiente de la Procuraduría General de Justicia o Fiscalía General de la entidad federativa).")
                   #### area para renderear el plot treemap
                 ) # div bracket
               ),
               br(),
               plotlyOutput("prueba2"),
               br(),
               hr(),
               hr(),
               hr(),
               hr(),
               hr()
             )
           )
  ), # Tab panel bracket
  #########################################
  #########################################
  #
  # Indicadores, sección para revisar los indicadores por trimestre, desplegados con leaflet y plotly
  #
  #########################################
  #########################################
  tabPanel("Indicadores",
           icon = icon("check-square-o"), value = "ind",
           introBox(
             wellPanel(fluidRow( # Filter options
               column(
                 3,
                 selectInput("indicador_mapa", "1. Seleccione un indicador a consultar:",
                             indicadores,
                             multiple = F,
                             selected = NA
                 ),
                 selectInput("trims_indicadores", "2. Seleccione una fecha de corte de la información:", ### input de etapa para
                             trims,
                             multiple = F,
                             selected = NA
                 ),
                 uiOutput("ui_indicadores"),
                 htmlOutput("definiciones_indicadores")
                 
               ),
               column(
                 7,
                 leafletOutput("mapa", height = 700)
               )
             ))
           )
  ), # Tab panel bracket
  #########################################
  #########################################
  #
  # TabPanel: Variables Seccion para la consulta de variables externas al proceso penal, por corte trimestral
  #
  #########################################
  #########################################
  tabPanel("Variables",
           icon = icon("database"), value = "vars",
           introBox(
             wellPanel(fluidRow( # Filter options
               column(
                 7,
                 
                 plotlyOutput("variables"),
                 br(),
                 hr(),
                 hr(),
                 hr(),
                 hr(),
                 hr()
               ),
               column(
                 3,
                 div(
                   title = "Seleccionar un grupo",
                   radioGroupButtons("var_group",
                                     status = "primary", ##### identificador del boton
                                     choices = c(
                                       "Agrupadas",
                                       "Desagrupadas"
                                     ),
                                     selected = "Agrupadas",
                                     label = "1. Seleccione el tipo de variables a mostrar:"
                   )
                 ),
                 uiOutput("ui_vars"),
                 selectInput("trims_var", "3. Seleccione una fecha de corte de la información:", ### input de etapa para
                             trims,
                             multiple = F,
                             selected = NA
                 ),
                 uiOutput("ui_vars_year"),
                 htmlOutput("definiciones_variables")
               )
             ))
           )
  ), # Tab panel bracket

  #########################################
  #########################################
  #
  # Spider Charts para los 10 indicadores
  #
  #########################################
  #########################################
  tabPanel("Resumen",
           icon = icon("list-ul"), value = "res",
           introBox(
             wellPanel(fluidRow( # Filter options
               column(
                 3,
                 selectInput("trims_resumen", "1. Seleccione una fecha de corte de la información:", ### input de etapa para
                             trims,
                             multiple = F,
                             selected = NA
                 ),
                 uiOutput("ui_resumen"),
                 p(HTML("Los <b>diez indicadores estratégicos</b> del Modelo de Evaluación y Seguimiento de la Consolidación del
Sistema de Justicia Penal aprobados por el Consejo Nacional de Seguridad Pública son:")),
                 tags$ol(
                   tags$li("Carpetas de investigación iniciadas"),
                   tags$li("Carpetas de investigación determinadas por el Ministerio Público"),
                   tags$li("Resolución de carpetas de investigación por acuerdos reparatorios (sede ministerial)"),
                   tags$li("Carpetas de investigación sin determinar en fase inicial"),
                   tags$li("Carpetas de investigación vinculadas a proceso"),
                   tags$li("Resolución de carpetas de investigación por Órgano Jurisdiccional"),
                   tags$li("Carpetas de investigación vinculadas a proceso en trámite"),
                   tags$li("Sentencias condenatorias"),
                   tags$li("Medidas cautelares impuestas"),
                   tags$li("Porcentaje de internamiento de imputados en prisión preventiva")
                 ) # aqui hay que seleccionar AÑO de carpeta y fecha de corte
               ),
               column(
                 8,
                 plotlyOutput("resumen"),
                 br(),
                 hr(),
                 hr(),
                 hr(),
                 hr(),
                 hr()
               )
             ))
           )
  ), # Tab panel bracket
  #########################################
  #########################################
  #
  # Una tabla de las definiciones por variable en lista y para todas
  #
  #########################################
  #########################################
  # tabPanel("Definiciones",
  #          icon = icon("book"), value = "def",
  #          fluidRow(
  #            style = "width:80%; margin-left: 2%; min-width: 350px",
  #            h4("Definiciones de indicadores y variables", style = "color:black;"),
  #            h5("Los indicadores del MES están construidos como cocientes de variables de operación a través de las distintas etapas del Proceso Penal.
  #                                   A su vez, las variables recogen las actividades de los operadores y tienen su fundamento en el Código Nacional de Procedimientos Penales."),
  #            h5("En esta sección podrá conocer las definiciones de las variables e indicadores utilizadas."),
  #            
  #            br(),
  #            div(
  #              title = "Desplegar información sobre variables o indicadores",
  #              radioGroupButtons("Var_o_ind",
  #                                status = "primary", ##### identificador del boton
  #                                choices = c("Definiciones para indicadores", "Definiciones para variables"),
  #                                selected = NA,
  #                                label = "1. Seleccione las definiciones a consultar:"
  #              )
  #            ),
  #            br(),
  #            # conditional panel for profile summary
  #            conditionalPanel(
  #              condition = 'input.Var_o_ind == "Definiciones para indicadores"',
  #              br(),
  #              dataTableOutput("def_ind")
  #            ),
  #            # conditional panel for single indicator
  #            conditionalPanel(
  #              condition = 'input.Var_o_ind == "Definiciones para variables"',
  #              div(
  #                style = "display:inline-block",
  #                title = "Realice una copia del archivo de definiciones",
  #                br(),
  #                dataTableOutput("def_var")
  #              )
  #            )
  #          )
  # ), # Tab panel bracket
  #########################################
  #########################################
  #
  # Descargas: Una seccion para renderear tablas y reportes por entidad federativa y generales
  #
  #########################################
  #########################################
  tabPanel("Descargas",
           icon = icon("file"), value = "desc",
           fluidRow(
             style = "width:80%; margin-left: 2%; min-width: 350px",
             h4("Descarga de reportes y datos", style = "color:black;"),
             h5("En esta sección podrá descargar los reportes estáticos y datos que los integran a nivel nacional y por entidad federativa"),
             
             br(),
             div(
               title = "Desplegar información a nivel nacional o por entidad federativa",
               radioGroupButtons("nac_o_ef",
                                 status = "primary", ##### identificador del boton
                                 choices = c("A Nivel Nacional", "Por Entidad Federativa"),
                                 selected = NA,
                                 label = "1. Seleccione la opción que prefiera para visualizar la información:"
               )
             ),
             br(),
             # conditional panel for profile summary
             conditionalPanel(
               condition = 'input.nac_o_ef == "A Nivel Nacional"',
               div(
                 style = "display:inline-block",
                 title = "Seleccione las opciones a nivel nacional",
                 br(),
                 selectInput("trims_nacional", "2. Seleccione una fecha de corte de la información:", ### input de entidad para
                             trims,
                             multiple = F,
                             selected = NA
                 ),
                 br(),
                 uiOutput("ui_cond_carpeta_denuncia_nacional"),
                 br(),
                 wellPanel(
                   #     downloadButton("reporte_nacional", "Descarga el reporte(.pdf)",
                   #      class = "btn-danger"
                   #   ),
                   downloadButton("datos_nacional", "Descarga los datos (.csv)",
                                  class = "btn-danger"
                   )
                 )
               )
             ),
             # conditional panel for single indicator
             conditionalPanel(
               condition = 'input.nac_o_ef == "Por Entidad Federativa"',
               div(
                 style = "display:inline-block",
                 title = "Seleccione las opciones por entidad federativa",
                 br(),
                 selectInput("Entidad_desc", "2. Seleccione una Entidad Federativa:", ### input de entidad para
                             entities,
                             multiple = F,
                             selected = NA
                 ),
                 selectInput("trims_entidad", "3. Seleccione una fecha de corte de la información:", ### input de entidad para
                             trims,
                             multiple = F,
                             selected = NA
                 ),
                 br(),
                 uiOutput("ui_cond_carpeta_denuncia_entidad"),
                 br(),
                 wellPanel(
                   # downloadButton("reporte_estatal", "Descarga el reporte (.pdf)",
                   #   class = "btn-danger"
                   # ),
                   downloadButton("datos_estatales", "Descarga los datos (.csv)",
                                  class = "btn-danger"
                   )
                 )
               )
             )
           )
  ),
  hr(),
  hr(),
  footer =
    tags$footer( # Oficial footer html code
      HTML('<footer class="main-footer"> 
	<div class="list-info">
		<div class="container">

				<div class="col-sm-3">
						<img src="https://framework-gb.cdn.gob.mx/landing/img/logoheader.svg" href="/" alt="logo gobierno de méxico" class="logo_footer" style="max-width: 90%; margin-left: -27px;   margin-top: 12px;">
						</div>
				<div class="col-sm-3">
					<h2>Enlaces</h2>
					<ul>
					<li><a href="http://www.participa.gob.mx">Participa</a></li>
					<li><a href="https://datos.gob.mx/">Datos</a></li>
						<li><a href="https://www.gob.mx/publicaciones" target="_blank" rel="noopener" title="Enlace abre en ventana nueva">Publicaciones Oficiales</a></li>
								<li><a class="footer" href="https://www.gob.mx/segob" id="Transparencia" target="blank" title="Enlace abre en ventana nueva">Portal de Obligaciones de Transparencia</a></li>
						<li><a class="footer" id="Infomex" href="https://www.infomex.org.mx/gobiernofederal/home.action" target="_blank" rel="noopener" title="Enlace abre en ventana nueva">Sistema Infomex</a></li>
						<li><a class="footer" id="INAI" href="http://www.inai.org.mx" target="_blank" rel="noopener" title="Enlace abre en ventana nueva">INAI</a></li>
					</ul>
				</div>
				<div class="col-sm-3">
					<h2>¿Qué es gob.mx?</h2>
					<p>Es el portal único de trámites, información y participación ciudadana. <a href="/que-es-gobmx" style="text-decoration: none;">Leer más</a></p>
					<ul>

						<li><a href="https://www.gob.mx/temas">Temas</a></li>
							
						<li><a href="https://www.gob.mx/accesibilidad">Declaración de Accesibilidad</a></li>
						<li><a href="https://www.gob.mx/aviso_de_privacidad">Aviso de privacidad integral</a></li>
            			<li><a href="https://www.gob.mx/privacidadsimplificado">Aviso de privacidad simplificado</a></li>
						<li><a href="https://www.gob.mx/terminos">Términos y Condiciones</a></li>
						<li><a href="https://www.gob.mx/terminos#medidas-seguridad-informacion">Política de seguridad</a></li>
						<li><a class="footer" id="Marco Juridico" href="http://www.ordenjuridico.gob.mx" target="_blank" rel="noopener" title="Enlace abre en ventana nueva">Marco Jurídico</a></li>
						<li><a href="https://www.gob.mx/sitemap">Mapa de sitio</a></li>
					</ul>
				</div>
				<div class="col-sm-3">

				    <!------------------------------------------------------------------------------------------>
                                    <!-- <a href="mailto:gobmx@funcionpublica.gob.mx"> gobmx@funcionpublica.gob.mx</a>        -->
                                    <!------------------------------------------------------------------------------------------>

				  <p></p>
						<h2>Síguenos en</h2>
					<ul class="list-inline">
						<li>
			              <a id="Facebook" href="https://www.facebook.com/gobmexico" target="_blank" rel="noopener" class="sendEst share-info footer" title="enlace a facebook abre en una nueva ventana">
			                <img alt="Facebook" src="https://framework-gb.cdn.gob.mx/landing/img/facebook.png">
			              </a>
			            </li>
						<li>
			              <a id="Twitter" href="https://twitter.com/GobiernoMX" target="_blank" rel="noopener" class="sendEst share-info footer" title="Enlace a twitter abre en una nueva ventana">
			                <img alt="Twitter" src="https://framework-gb.cdn.gob.mx/landing/img/twitter.png">
			              </a>
			            </li>
					</ul>
				</div>
			</div>

		</div>
	
	
	<div class="container-fluid footer-pleca">
		<div class="row">
			<div class="col">
				<br><br>
			</div>
		</div>
	</div>
</footer>')
    )
  
  ################################################ .
) # bracket tagList
### END

# Define server logic
server <- function(input, output) {
  #####################################
  ###################################
  ################################
  ############################## Modal de bienvenida
  showModal(modalDialog(
    title = "Bienvenida/o",
    HTML("Información actualizada al <b>31 de marzo de 2021</b>"),
    
    easyClose = T,
    footer = modalButton("Ok") # actionButton("ayuda", label="Guia de Usuario")
  ))
  observeEvent(input$ayuda, {
    showModal(modalDialog(
      title = "Guía de usuario",
      easyClose = TRUE,
      footer = uiOutput("pdfview"),
      size = "l"
    ))
  })
  
  observeEvent(input$ayuda, {
    output$pdfview <- renderUI({
      tags$iframe(style = "height:600px; width:100%", src = "Manual_Usuario.pdf")
    })
  })
  
  #########################################
  #########################################
  #
  # Modales de ESQUEMA ademas de treemaps por seccion
  #
  #########################################
  #########################################
  observeEvent(input$toggleSide, {
    shinyjs::toggle(id = "Sidebar")
  }) # toggle
  ## UI CONDICIONAL
  output$ui_cond_carpeta_denuncia_etapa <- renderUI({
    if (strsplit(input$trims_etapa, "-")[[1]][3] == 2017) {
      tagList(
        selectInput("year_report_etapa", "4. Seleccione el año de apertura de las carpetas de investigación a consultar:", choices = c("2017")) ### elements in report
      )
    }
    else if (strsplit(input$trims_etapa, "-")[[1]][3] > 2017) {
      tagList(
        selectInput("year_report_etapa", "4. Seleccione el año de apertura de las carpetas de investigación a consultar:",
                    choices = c(seq(2017, strsplit(input$trims_etapa, "-")[[1]][3], 1)),
                    selected = rev(c(seq(2017, strsplit(input$trims_etapa, "-")[[1]][3], 1)))[1]
        )
      )
    }
  })
  #### in an observe event button render conditional uis with many plots
  
  x <- eventReactive(input$calcular_esquema, {
    db_plot <- rbind(xx, aux_row)
    db_plot <- db_plot %>% mutate(labelx = paste(`Entidad Federativa`, var))
  })
  
  
  output$prueba2 <- renderPlotly({
    validate(
      need(input$year_report_etapa != "", "Please wait")
    )
    if (input$nac_o_ef_etapa == "A Nivel Nacional") {
      esquema_treemap(
        df = db_df,
        colores = jbalvin,
        etapa = get(input$esquema),
        anio_carpetas = input$year_report_etapa,
        fecha_corte = input$trims_etapa,
        nacional = T,
        entidad = NULL
      )
    } else {
      esquema_treemap(
        df = db_df,
        colores = jbalvin,
        etapa = get(input$esquema),
        anio_carpetas = input$year_report_etapa,
        fecha_corte = input$trims_etapa,
        nacional = F,
        entidad = input$Entidad
      )
    }
  })
  #########################################
  #########################################
  #
  # Outputs de la seccion indicadores: Mapas
  #
  #########################################
  #########################################
  output$definiciones_indicadores <- renderText({
    paste("<b>Definición:</b>", def_df_ind$Definicion[def_df_ind$Indicador==input$indicador_mapa])
  })
  output$ui_indicadores <- renderUI({
    if (strsplit(input$trims_indicadores, "-")[[1]][3] == 2017) {
      tagList(
        selectInput("year_indicadores", "3. Seleccione el año de las carpetas y denuncias a consultar:", choices = c("2017")) ### elements in report
      )
    }
    else if (strsplit(input$trims_indicadores, "-")[[1]][3] > 2017) {
      tagList(
        selectInput("year_indicadores", "3. Seleccione el año de las carpetas y denuncias a consultar:",
                    choices = c(seq(2017, strsplit(input$trims_indicadores, "-")[[1]][3], 1)),
                    selected = rev(c(seq(2017, strsplit(input$trims_indicadores, "-")[[1]][3], 1)))[1]
        )
      )
    }
  })
  output$mapa <- renderLeaflet({
    validate(
      need(input$year_indicadores != "", "Please wait")
    )
    mapa_indicadores(
      df = df_ind,
      map_inegi = edos,
      anio_carpetas = input$year_indicadores,
      fecha_corte = input$trims_indicadores,
      indicador = input$indicador_mapa
    )
  })
  #########################################
  #########################################
  #
  # Outputs de la seccion variables: Barplots
  #
  #########################################
  #########################################
  output$definiciones_variables <- renderText({
    ifelse(length(def_df_var$Definicion[def_df_var$Variable==input$var_var])>0,
      paste("<b>Definición:</b>", def_df_var$Definicion[def_df_var$Variable==input$var_var]),"")
  })
  output$ui_vars <- renderUI({
    if (input$var_group == "Agrupadas") {
      tagList(
        selectInput("var_var", "2. Seleccione el grupo de variables a consultar:", choices = c(
          "Víctimas" = "Victimas",
          "Detenidos",
          "Órdenes de aprehensión",
          "Órdenes de detención"
        )) ### elements in report
      )
    }
    else if (input$var_group != "Agrupadas") {
      tagList(
        selectInput("var_var", "2. Seleccione la variable a consultar:",
                    choices = varlist
        )
      )
    }
  })
  output$ui_vars_year <- renderUI({
    if (strsplit(input$trims_var, "-")[[1]][3] == 2017) {
      tagList(
        selectInput("year_var", "4. Seleccione el año de las carpetas y denuncias a consultar:", choices = c("2017")) ### elements in report
      )
    }
    else if (strsplit(input$trims_var, "-")[[1]][3] > 2017) {
      tagList(
        selectInput("year_var", "4. Seleccione el año de las carpetas y denuncias a consultar:",
                    choices = c(seq(2017, strsplit(input$trims_var, "-")[[1]][3], 1)),
                    selected = rev(c(seq(2017, strsplit(input$trims_var, "-")[[1]][3], 1)))[1]
        )
      )
    }
  })
  output$variables <- renderPlotly({
    validate(
      need(input$year_var != "", "Please wait")
    )
    bar_variables(
      df = db_df,
      variables = input$var_var,
      fecha_corte = input$trims_var,
      anio_carpetas = input$year_var,
      colores = jbalvin
    )
  })
  #########################################
  #########################################
  #
  # Outputs de la seccion resumen: spidercharts
  #
  #########################################
  #########################################
  output$ui_resumen <- renderUI({
    if (strsplit(input$trims_resumen, "-")[[1]][3] == 2017) {
      tagList(
        selectInput("year_resumen", "2. Seleccione el año de las carpetas y denuncias a consultar:", choices = c("2017")) ### elements in report
      )
    }
    else if (strsplit(input$trims_resumen, "-")[[1]][3] > 2017) {
      tagList(
        selectInput("year_resumen", "2. Seleccione el año de las carpetas y denuncias a consultar:",
                    choices = c(seq(2017, strsplit(input$trims_resumen, "-")[[1]][3], 1))
        )
      )
    }
  })
  output$resumen <- renderPlotly({
    validate(
      need(input$year_resumen != "", "Please wait")
    )
    resumen_spiderchart(
      df = db_ind,
      anio_carpetas = input$year_resumen,
      fecha_corte = input$trims_resumen
    )
  })
  #########################################
  #########################################
  #
  # Outputs de la seccion Definiciones: dos tablas con definiciones
  #
  #########################################
  #########################################
  output$def_var <- renderDataTable(def_df_var,
                                    extensions = "Buttons",
                                    server = FALSE,
                                    options = list(
                                      pageLength = 5,
                                      columnDefs = list(list(width = "300px", targets = c("Definición"))),
                                      buttons = list(
                                        list(
                                          extend = "excel", text = "Descargar esta tabla", filename = "Variables_page_MES",
                                          exportOptions = list(
                                            modifier = list(page = "current")
                                          )
                                        ),
                                        list(
                                          extend = "excel", text = "Descargar toda la tabla", filename = "Variables_all_MES",
                                          exportOptions = list(
                                            modifier = list(page = "all")
                                          )
                                        )
                                      ),
                                      dom = "Bfrtip"
                                    ),
                                    rownames = FALSE
  )
  output$def_ind <- renderDataTable(def_df_ind,
                                    extensions = "Buttons",
                                    server = FALSE,
                                    options = list(
                                      pageLength = 5,
                                      columnDefs = list(list(width = "300px", targets = c("Definición"))),
                                      buttons = list(
                                        list(
                                          extend = "excel", text = "Descargar esta tabla", filename = "Variables_page_MES",
                                          exportOptions = list(
                                            modifier = list(page = "current")
                                          )
                                        ),
                                        list(
                                          extend = "excel", text = "Descargar toda la tabla", filename = "Variables_all_MES",
                                          exportOptions = list(
                                            modifier = list(page = "all")
                                          )
                                        )
                                      ),
                                      dom = "Bfrtip"
                                    ),
                                    rownames = FALSE
  )
  #########################################
  #########################################
  #
  # Outputs de descarga de reportes e información
  #
  #########################################
  #########################################
  ## UI CONDICIONAL
  output$ui_cond_carpeta_denuncia_entidad <- renderUI({
    if (strsplit(input$trims_entidad, "-")[[1]][3] == 2017) {
      tagList(
        checkboxGroupInput("year_report_enti", "4. Seleccione el año de las carpetas y denuncias a consultar:", choices = c("2017")) ### elements in report
      )
    }
    else if (strsplit(input$trims_entidad, "-")[[1]][3] > 2017) {
      tagList(
        checkboxGroupInput("year_report_enti", "4. Seleccione el año de las carpetas y denuncias a consultar:",
                           choices = c(seq(2017, strsplit(input$trims_entidad, "-")[[1]][3], 1))
        )
      )
    }
  })
  ## UI CONDICIONAL
  output$ui_cond_carpeta_denuncia_nacional <- renderUI({
    if (strsplit(input$trims_nacional, "-")[[1]][3] == 2017) {
      tagList(
        checkboxGroupInput("year_report_nac", "3. Seleccione el año de las carpetas y denuncias a consultar:", choices = c("2017")) ### elements in report
      )
    }
    else if (strsplit(input$trims_nacional, "-")[[1]][3] > 2017) {
      tagList(
        checkboxGroupInput("year_report_nac", "3. Seleccione el año de las carpetas y denuncias a consultar:",
                           choices = c(seq(2017, strsplit(input$trims_nacional, "-")[[1]][3], 1))
        )
      )
    }
  })
  # funcion para knitear el reporte y descargarlo, lleva su tiempo
  ## funcion para descargar los datos nacionales y por entidad federativa
  
  output$datos_nacional <- downloadHandler(
    filename = function() {
      paste("db-nacional", ".csv", sep = "")
    },
    
    content = function(file) {
      data <-
        db_df %>%
        filter(
          .[[3]] %in% input$year_report_nac,
          `Fecha de Corte` == input$trims_nacional
        )
      
      write.csv(data, file, row.names = F, fileEncoding = "latin1")
    }
  )
  
  output$datos_estatales <- downloadHandler(
    filename = function() {
      paste("db-", input$Entidad_desc, ".csv", sep = "")
    },
    
    content = function(file) {
      data <-
        db_df %>%
        filter(
          .[[3]] %in% input$year_report_enti,
          `Fecha de Corte` == input$trims_entidad,
          `Entidad Federativa` == input$Entidad_desc
        )
      
      write.csv(data, file, row.names = F, fileEncoding = "latin1")
    }
  )
} # server function key

# Run the application
shinyApp(ui = ui, server = server)
