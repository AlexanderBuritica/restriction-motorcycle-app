
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
#-----------------------# shinytheme("spacelab")
                              
ui <- navbarPage(title = "Motorcycle restrictions and their effect on crime", theme = "http://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
                 
                 # Panel Results
                 tabPanel("Results",
                          withMathJax(),
                          tags$style(type="text/css", "html, body {width:100%;height:100%}"),
                          div(class="outer",
                              tags$head(includeCSS("www/style.css"), 
                                                                  tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                                                                  tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')),
                              #leafletOutput("mymap", width="100%", height="100%"),
                              
                            # Coefplot
                            absolutePanel(id = "coefplot", fixed = TRUE,
                                          draggable = TRUE, top = 50, left = 20, right = "auto", bottom = "auto", width = 330, height = "auto", 
                                          h2("Motorcycle restrictions and their effect on crime: Evidence from Colombia"),
                                          selectInput(inputId = "map_zone", label = "Select one restriction:", choices = c("",as.character(maps[[2]]$city_restric)), selected = ""),
                                          plotOutput("DD_plot", width = 300, height = 250),
                                          h6("This ...")
                            ),
                            
                            # Event Study
                            absolutePanel(id = "event", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 30, bottom = "auto",
                                          width = 280, height = "auto",
                                          conditionalPanel(
                                                          condition = "input.map_zone != '' ",
                                                          h4("Monthly average effect", align = "center"),
                                                          selectInput(inputId = "type_crime", label = "Select a type of crime:", 
                                                                      choices = c(unique(df_results[[1]]$type_crime)),
                                                                      selected = ""),
                                                          plotOutput("eventstudy", width = 300, height = 200),
                                                          htmlOutput("avoided_event")
                                          )
                            )
                        )
                 ),
                 
                 # Panel Description
                 tabPanel("Description",
                          includeHTML("text/description.html")
                          
                 ),
                 
                 # Panel Description
                 tabPanel("About",
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
          
          ### Map in leaflet
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
                            if(!is.null(p$id)){ 
                            if(is.null(input$map_zone) || input$map_zone != p$id) 
                            updateSelectInput(session, "map_zone", selected = p$id)
                            }
          })
    
          observeEvent(input$mymap_shape_click, { # update the map view on map clicks
                       p <- input$mymap_shape_click
                       proxy <- leafletProxy("mymap")
                       if(is.null(p$id)){proxy
                       } 
                       else {proxy %>% setView(lng=p$lng, lat=p$lat, input$mymap_zoom) %>% removeMarker(layerId="city_restric")
                       }
          })
    
          observeEvent(input$map_zone, { # update the map view on location selectInput changes
                       if(input$map_zone != ""){
                       p <- input$mymap_shape_click
                       p2 <- gCentroid(maps[[2]][maps[[2]]$city_restric == input$map_zone, ]) %>% coordinates()
                       proxy <- leafletProxy("mymap")
                       if(length(p$id) && input$map_zone != p$id){
                          proxy %>% clearPopups() %>% setView(lng = p2[1], lat = p2[2] , input$mymap_zoom) %>% addMarkers(lng = p2[1], lat = p2[2], layerId = "city_restric")
                       }
                       } 
                       else {
                       proxy <- leafletProxy("mymap")
                       proxy
                       }
          })
          
          ### Coefplot
          data_1 <- reactive({
                    if(input$map_zone == ""){ return(NULL)} 
                    else {df_results[[1]] %>% subset(city_restric == input$map_zone) %>%
                          subset(type_crime == "Total crimes")
                    }
          })
          data_2 <- reactive({
                    if(input$map_zone == ""){ return(NULL)} 
                    else {df_results[[1]] %>% subset(city_restric == input$map_zone) %>%
                          subset(type_crime == input$type_crime)
                    }
          })
          
          output$DD_plot <- renderPlot({
                            if(input$map_zone == ""){return(NULL)} 
                            else {#g1 <- ggplot(data_1, aes(x = coef, y = zone, color=zone,fill = zone)) + 
                                  #     geom_errorbarh(width=.1, aes(xmin = ci_lower, xmax = ci_upper),show.legend = F) + 
                                  #     geom_point(shape = 21, size = 3,show.legend = F) +
                                  #     geom_vline(aes(xintercept = 0),linetype="solid",colour = "black") +
                                  #     theme_bw()  +  theme(plot.title = element_text(hjust = 0.5,size = 20)) + xlab("Coefficient") + ylab("Zone") +
                                  #     ggtitle(as.character("Total crimes")) 
                                  g2 <- ggplot(data_2(), aes(y = coef, x = zone, color=zone,fill = zone)) + 
                                        geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper),show.legend = F) + 
                                        geom_point(shape = 21, size = 3,show.legend = F) + 
                                        geom_hline(aes(yintercept = 0),linetype="solid",colour = "black") +
                                        theme_bw()  +  theme(plot.title = element_text(hjust = 0.5,size = 20)) + xlab("Coefficient") + ylab("Zone") + 
                                        ggtitle(as.character(input$type_crime)) 
                                  g2
                                  #ggpubr::ggarrange(g1, g2, ncol = 1, nrow = 2)
                           }
          })
          
          ### Event Study
          data_3 <- reactive({
                    if(input$map_zone == ""){  return(NULL)} 
                    else { df_results[[2]] %>% subset(city_restric == input$map_zone) %>%
                           subset(type_crime == input$type_crime)
                    } 
          })
          output$eventstudy <- renderPlot({
                               if(input$map_zone == ""){ return(NULL) } 
                               else{ g3 <- ggplot(data_3(), aes(x = months, y = coef, color = zone,fill = zone)) + labs(" ") +
                                           geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper)) + scale_x_discrete(limits=c(-6:5)) +
                                           geom_point(shape = 21, size = 3) + ylab("Coefficient") + xlab("Months at the start of the restriction") +
                                           geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = -1),linetype="dashed",colour = "black") +
                                           theme_bw() + theme(plot.title = element_text(hjust = 0.5,size = 20),
                                                              legend.justification = "center" , legend.position =c(.5,.97),
                                                              legend.direction = "horizontal",legend.text = element_text(hjust = 0.5,size = 10) )
                                    g3
              }
            })
          
          output$avoided_event <-
            renderUI({
              if(input$map_zone == ""){
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
