#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### 1 LOAD PACKAGES ###########################################################

library(dplyr)                                                                    # data wrangling work horse
library(tidyr)                                                                    # additional data wrangling
library(tidytidbits)                                                              # for conditional piping
library(stringr)                                                                  # to do some operations with strings
library(shiny)                                                                    # for shiny app functions
library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)                                                              # to apply a theme to the shiny app
library(sf)                                                                       # to read/manipulate shapefiles
library(leaflet)                                                                  # to display maps
library(leaflet.extras)                                                           # additional options for leaflet
library(highcharter)                                                              # to build plots
library(DT)                                                                       # for datatable in data explorer
library(kableExtra)                                                               # to make tables
library(scales)                                                                   # to define percentages
library(readxl)
library(fmsb)
library(leafpop)
library(ggradar)

#### 2 LOAD DATA ###############################################################

data <- read_excel("data/PUNTAJE_FINAL_condata_JM.xlsx", "Calculo_puntaje")
data$Indice <- as.character(data$Indice)

# Cargar shps
barrio <- st_read("gis/Barrios_Ejemplo.shp")
departamento <- st_read("gis/Admin1_UnodcOcha_01012009.shp")
country     <- st_read("gis/World_admin0_countries_py_WFP_nd.shp")
encuestas <- st_read("gis/Encuestas_ejemplo.shp")
encuestas["uuid"] <- rownames(encuestas)

encuestas <- encuestas |> left_join(data, by = c("uuid" = "Indice"))

# Join para los barrios
encuestas <- st_join(encuestas, barrio, left = TRUE)

# Calcular media del puntaje por hogar de los barrios
barrio <- encuestas |> as.data.frame() |> group_by(Barrio) |> 
  summarize(puntaje = median(`PUNTAJE POR HOGAR`, na.rm =T)) |> 
  dplyr::right_join(barrio, by = "Barrio")

barrio <- sf::st_as_sf(barrio)

#set.seed(1)
#df <- data.frame(rbind(rep(10, 8), rep(0, 8),
#                       matrix(sample(0:10, 8),
#                              nrow = 1)))
#colnames(df) <- paste("Var", 1:8)


set.seed(4)
df <- data.frame(matrix(runif(30), ncol = 10))
df[, 1] <- paste0("G", 1:3)
colnames(df) <- c("Group", paste("Var", 1:9))


# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Plataforma Regional",
               theme = shinytheme("simplex"),
               
               # 1ra página ####################################################
               tabPanel("Mapa",
                        icon = icon("map"),
                        div(class = "Mapa",
                            tags$head(includeCSS("styles.css")),
                            
                            leafletOutput("mapa", width = "100%", height = "100%")
                            )
                        
                        )
               )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map_text <- renderText({""})
  
  output$mapa <- renderLeaflet({
    
    # Coordenadas
    bounds <- barrio %>% 
      st_bbox() %>% 
      as.character()
    
    # Etiquetas
    labels <- sprintf("<strong>%s</strong><br/>%.2f %s", barrio$Barrio, (barrio$puntaje*100), "%") %>% lapply(htmltools::HTML)
    # Paleta de colores
    pal <- colorNumeric(palette = c("#F8696B", "#F98370", "#FA9D75", "#FCB77A", "#FDD17F", "#FFEB84", "#E0E383", "#C1DA81", "#A2D07F", "#83C77D", "#63BE7B"),
                        domain = c(0,1), na.color = "transparent")
    
    # Segunda etiqueta
    lab <- sprintf("<strong>%s</strong><br/>%.2f %s", "P. alojamiento:" , encuestas$`PUNTAJE POR HOGAR`, "%") %>% lapply(htmltools::HTML)
    
    ## Generar los graficos de radar
    # Dejar los datos a graficar
    prueba <- encuestas |> select(uuid, `Puntaje ponderado seguridad de la tenencia`, 
                                  `Puntaje ponderado asequibilidad`,
                                  `Puntaje ponderado habitabilidad`,
                                  `Puntaje ponderado servicios e infraestructuras`,
                                  `Puntaje ponderado accesibilidad`,
                                  `Puntaje ponderado ubicación`,
                                  `Puntaje ponderado adaptación cultural`) |> 
      as.data.frame() |> select(-geometry)
    
    #lab <-  recordPlot(ggradar(prueba))
    # Almacenar los graficos en una lista
    my_list <- list()
    for (i in prueba$uuid) {
      ojo <- prueba %>% filter(uuid == i)
      plot <- ggradar(ojo, grid.min = 0, grid.mid = 1, grid.max = 2, values.radar = c("0", "1", "2"), plot.extent.x.sf = 2)
      plot
      my_list[[i]] <- plot
    }

    
    mapa <- leaflet(options = leafletOptions(attributionControl=FALSE))%>%
      fitBounds((as.numeric(bounds[1])+0.05), (as.numeric(bounds[2])-0.05), (as.numeric(bounds[3])-0.05), (as.numeric(bounds[4])+0.05)) %>%
      addMapPane(name = "base", zIndex = 410) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "label", zIndex = 430)%>%
      addMapPane(name = "points", zIndex = 425)%>%
      addCircles(data = encuestas, group = "Encuestas", fill = TRUE, fillOpacity = 0.6 ,fillColor = "red",
                 stroke = TRUE, color = "#58585A", weight = 0.5, opacity = 1,
                 highlight = highlightOptions(
                   weight = 5,
                   color = "#666666",
                   fillOpacity = 0.75,
                   bringToFront = TRUE
                 ),
                 popup = popupGraph(my_list, width = 720, height = 400),
                 label = lab,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 options = leafletOptions(pane = "points")) %>%
      addPolygons(data = barrio, group = "Barrios", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(barrio$puntaje),
                  stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.75,
                    bringToFront = TRUE
                  ),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  options = leafletOptions(pane = "polygons")
      )%>%
      addPolygons(data = country, group = "Country", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) %>%
      addLegend("bottomright", pal = pal, values = barrio$puntaje,
                title = "Puntaje de alojamiento </br> adecuado (mediana):",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1
      )%>%
      setMapWidgetStyle(style = list(background = "transparent")) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                       options = c(providerTileOptions(opacity = 0.6),
                                   leafletOptions(pane = "base"))) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                       options = c(providerTileOptions(opacity = 1),
                                   leafletOptions(pane = "label")))  %>%
      addLayersControl(overlayGroups = c("Labels", "Country", "Barrios", "Base map"))
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)




