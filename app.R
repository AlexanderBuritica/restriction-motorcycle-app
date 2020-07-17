
# Load packages
library('shiny')
library('shinythemes')
library('tidyverse')
library('maptools')
library("broom")
library('leaflet')
library('htmltools')
library('rsconnect')
library('rgdal')
library('magrittr')
library('sp')
library('rgeos')
library("ggpubr")

# Load maps and data
maps = readRDS("data/maps.rds")
df_results = readRDS("data/df_results.rds")
 
#-----------------------#
# Define user interface #
#-----------------------# 
                              
ui <- navbarPage(title = "Restricciones a motocicletas y sus efectos sobre el crimen en Colombia", id="nav", theme = "http://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
                 
                 # Panel Results
                 tabPanel("Resultados",withMathJax(),
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("style/style.css"),
                                includeScript("style/gomap.js")
                              ),
                              
                              
                          #tags$style(type="text/css", "html, body {width:100%;height:100%}"),
                          #div(class="outer",
                          #tags$head(includeCSS("style/styles.css"), 
                          #tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                          #tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')),
                         
                          
                          # Map
                          leafletOutput("mymap", width="100%", height="100%"),
                              
                          # Coefplot
                          absolutePanel(id = "coefplot", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = 50, right = "auto", bottom = "auto", width = 330, height = "auto", 
                                        h3("Restricciones a motocicletas y sus efectos sobre el crimen en Colombia"),
                                        selectInput(inputId = "zonerestric", label = "Seleccione una restricción:", choices = c("",as.character(maps[[2]]$city_restric)), selected = ""),
                                        plotOutput("DD_plot", width = 280, height = 240)
                          ),

                        
                          # Event Study
                          absolutePanel(id = "event", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",width = 330, height = "auto",
                                        conditionalPanel(
                                                        condition = "input.zonerestric != '' ",
                                                        h4('Efecto mensual promedio', align = "center"),
                                                        selectInput(inputId = "type_crime", label = "Seleccione un tipo de delito:", 
                                                                    choices = c(unique(df_results[[1]]$type_crime)),
                                                                    selected = "Total crimes"),
                                                        plotOutput("eventstudy", width = 300, height = 250),
                                                        htmlOutput("avoided_event")
                                        )
                          )
                      )
                 ),
                 
                 # Panel Description
                 tabPanel("Descripción",
                          p("NULL")
                          #includeHTML("text/description.html")
                          
                 ),
                 
                 # Panel Description
                 tabPanel("Acerca de",
                          p("This proyect is an extension of the working paper"),
                          br(),
                          br(),
                          br(),
                          br(),
                          "This proyect is under development.",
                          br(),
                          br(),
                          # HTML('<center><img src="banrep_logo.png" height="72" width="72"/></center>')
                 )
)


# Create a pop-up:
popup <- paste0("<center><b>", maps[[2]]@data$name_restric, "</b></center>",
                "<b><br />Since: </b> ", maps[[2]]@data$STATUS_YR,
                "<b><br />Until: </b> ", maps[[2]]@data$STATUS_YR,
                "<b><br />Annual deforestation: </b>", maps[[2]]@data$area_restric, " ha.",
                "<b><br />Annual deforestation rate: </b>", maps[[2]]@data$percent_restric, " ha/km$^2$")

pal <- colorFactor( palette = c("#31a354", "#e5f5f9","pink2"), domain = maps[[2]]$name_restric)


#---------------#
# Define server #
#---------------#
server <- function(input, output,session) {

          ### Create the map
          output$mymap <- renderLeaflet({  
          leaflet() %>% addTiles() %>% addPolygons(data = maps[[1]],color='red',fillColor=NA,fillOpacity = 0) %>% 
          addPolygons(data = maps[[2]], popup = popup,
                      layerId = as.character(maps[[2]]$city_restric),
                      color = '#ffffb2', weight = 2, opacity = 0.8) %>% 
          addProviderTiles("CartoDB.Positron") %>% 
          addLegend(position="bottomleft", pal = pal, values = maps[[2]]$name_restric, labels=c(unique(maps[[2]]$name_restric)), title = "Zone") 
          })
          observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
          p <- input$mymap_shape_click
          if(!is.null(p$id)){if(is.null(input$zonerestric) || input$zonerestric != p$id) updateSelectInput(session, "zonerestric", selected = p$id)
          }
          })
          observeEvent(input$mymap_shape_click, { # update the map view on map clicks
                       p <- input$mymap_shape_click
                       proxy <- leafletProxy("mymap")
                       if(is.null(p$id)){proxy} 
                       else {proxy %>% setView(lng=p$lng, lat=p$lat, input$mymap_zoom) %>% removeMarker(layerId="city_restric")}
          })
          observeEvent(input$zonerestric, { # update the map view on location selectInput changes
                       if(input$zonerestric != ""){
                       p <- input$mymap_shape_click
                       p2 <- gCentroid(maps[[2]][maps[[2]]$city_restric == input$zonerestric, ]) %>% coordinates()
                       proxy <- leafletProxy("mymap")
                       if(length(p$id) && input$zonerestric != p$id){
                          proxy %>% clearPopups() %>% setView(lng = p2[1], lat = p2[2] , input$mymap_zoom) %>% addMarkers(lng = p2[1], lat = p2[2], layerId = "city_restric")
                       }
                       } 
                       else {proxy <- leafletProxy("mymap")
                             proxy
                       }
          })
   
          
          ### Coefplot
          data_1 <- reactive({
                    if(input$zonerestric == ""){ return(NULL)} 
                    else {df_results[[1]] %>% subset(city_restric == input$zonerestric) %>%
                          subset(type_crime == "Total crimes")
                    }
          })
          data_2 <- reactive({
                    if(input$zonerestric == ""){ return(NULL)} 
                    else {df_results[[1]] %>% subset(city_restric == input$zonerestric) %>%
                          subset(type_crime == input$type_crime)
                    }
          })
          output$DD_plot <- renderPlot({
                            if(input$zonerestric == ""){return(NULL)} 
                            else {g2 <- ggplot(data_2(), aes(y = coef, x = zone, color=zone,fill = zone)) + 
                                        scale_color_manual(values=c("darkblue","brown4")) + scale_fill_manual(values=c("darkblue","brown4")) +
                                        geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper),show.legend = F) + 
                                        geom_point(shape = 21, size = 3,show.legend = F) + 
                                        geom_hline(aes(yintercept = 0),linetype="solid",colour = "black") +
                                        theme_bw()  +  theme(plot.title = element_text(hjust = 0.5,size = 18)) + xlab("Zona") + ylab("Coeficiente") + 
                                        ggtitle(as.character(input$type_crime)) 
                                  g2
                           }
          })
          
          
          ### Event Study
          data_3 <- reactive({
                    if(input$zonerestric == ""){  return(NULL)} 
                    else { df_results[[2]] %>% subset(city_restric == input$zonerestric) %>%
                           subset(type_crime == input$type_crime)
                    } 
          })
          output$eventstudy <- renderPlot({
                               if(input$zonerestric == ""){ return(NULL) } 
                               else{ g3 <- ggplot(data_3(), aes(x = months, y = coef, color = zone,fill = zone)) + labs(" ") +
                                           scale_color_manual(values=c("darkblue","brown4")) + scale_fill_manual(values=c("darkblue","brown4")) +
                                           geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper)) + scale_x_discrete(limits=c(-6:5)) +
                                           geom_point(shape = 21, size = 3) + ylab("Coeficiente") + xlab("Meses al inicio de la restricción") +
                                           geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = -1),linetype="dashed",colour = "black") +
                                           theme_bw() + theme(plot.title = element_text(hjust = 0.5,size = 20),
                                                              legend.title = element_blank(),legend.position="bottom",
                                                              legend.direction = "horizontal",legend.text = element_text(hjust = 0.5,size = 10) )
                                    g3
                               }
           })
          
           ### avoided_event          
           output$avoided_event <- renderUI({
                                            if(input$zonerestric == ""){
                                              return(NULL)
                                            } else { withMathJax(HTML(paste0("<b>LATE: </b>", "Hola", " ha/km$^2$",
                                                            "<b><br /> Avoided deforestation: </b>", "Hola", " ha.",
                                                             "<b><br /> Percent change: </b>", "Hola", " %")))
                              }
               
            })

}

#---------------------#
# Run the application #
#---------------------#
shinyApp(ui = ui, server = server)
