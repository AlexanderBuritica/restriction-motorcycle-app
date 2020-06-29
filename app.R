
# Load packages
library('shiny')
library('tidyverse')
library('maptools')
library('leaflet')
library('htmltools')
library('rsconnect')
library('rgdal')
library('magrittr')
library('sp')
library('rgeos')

# Load maps and data
maps = readRDS("data/maps.rds")
df_results = readRDS("data/df_results.rds")

#-----------------------#
# Define user interface #
#-----------------------#
ui <- navbarPage(title = "Motorcycle restrictions and their effect on crime", theme = "https://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
                 
                 # Panel Results
                 tabPanel("Results",
                        withMathJax(),
                        tags$style(type="text/css", "html, body {width:100%;height:100%}"),
                        div(class="outer",
                            tags$head(includeCSS("www/style.css"), tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", 
                                                                               type = 'text/javascript'),
                                      tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            # Coefplot
                            absolutePanel(id = "coefplot", fixed = TRUE,
                                          draggable = TRUE, top = 50, left = 40, right = "auto", bottom = "auto",
                                          width = 330, height = "auto", 
                                          h2("Motorcycle restrictions and their effect on crime"),
                                          #h4("Select one restriction:"),
                                          selectInput(inputId = "map_zone", label = "Select one restriction:", choices = c("",as.character(maps[[2]]$city_restric)), selected = ""),
                                          plotOutput("DD_plot", width = 300, height = 300)
                            ),
                            
                            # Event Study
                            absolutePanel(id = "event", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 280, height = "auto",
                                          conditionalPanel(
                                                          condition = "input.map_zone != '' ",
                                                          #h5("Event Study", align = "center"),
                                                          h4("Monthly average effect", align = "center"),
                                                          selectInput(inputId = "type_crime", label = "Select a type of crime:", 
                                                                      choices = c("Total crimes","Property crimes","Homicide and personal injury","Theft"),
                                                                      selected = ""),
                                                          plotOutput("eventstudy", width = 260, height = 260),
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
                          # img(src = "banrep_logo.png", height = 72, width = 72)
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
                          leaflet() %>% addTiles() #%>% 
                          addPolygons(data = maps[[2]], popup = popup,
                                      layerId = as.character(maps[[2]]$city_restric),
                                      color = '#ffffb2', weight = 2, opacity = 0.8) %>% 
                           addPolygons(data = maps[[1]],color = "red") %>%
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
                    else {defo_dist %>% subset(defo_dist$buffer_name == input$park) %>%
                          mutate(., bin = cut(.$dist_disc, breaks = c(-50:50), include.lowest = T)) %>%
                          group_by(bin) %>%
                          summarize(meanbin = mean(loss_sum), sdbin = sd(loss_sum), n = length(ID)) %>%
                          .[complete.cases(.),] %>%
                          as.data.frame() %>%
                          mutate(treatment = ifelse(as.numeric(row.names(.)) > 50, 1, 0), bins = row.names(.)) %>%
                          mutate(bins = mapvalues(.$bins, from = c(1:100), to = c(-50:49)))
                    }
          })
          
          output$coefplot <- renderPlot({
                           if(input$map_zone == ""){return(NULL)} 
                           else {
                              g <- ggplot(data(), aes(y = (meanbin), x = as.numeric(bins), colour = as.factor(treatment)))
                              g <- g + stat_smooth(method = "auto")
                              g <- g + geom_point(colour = "black", size = 1)
                              g <- g + labs(x = "Distance (km)", y = expression(paste("Deforestation", " (ha/",km^{2}, ")")))
                              # g <- g + scale_x_continuous(limits = c(-20, 20))
                              # g <- g + scale_y_continuous(limits = c(0, 0.3))
                              # # g <- g + ggtitle(str_c("Discontinuidad\n", "para", type, sep = " "))
                              g <- g + guides(colour = FALSE)
                              # g <- g + theme_bw()
                              g
                           }
          })
          
          ### Event Study
          data_2 <- reactive({
                    if(input$map_zone == ""){  return(NULL)} 
                    else { all_rd_df %>% mutate(buffer_name = as.factor(buffer_name)) %>% 
                           subset(buffer_name == input$park) %>%
                           rbind(., rd_agg[c(2:3), ])
                    } 
          })
          output$event <- renderPlot({
                          if(input$map_zone == ""){ return(NULL) } 
                          else{
                g2 <- ggplot(data_2, aes(x = months, y = coef, color = var)) + 
                      geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper)) + 
                      geom_point(shape = 21, size = 3, fill = "white") +
                      geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = -1),linetype="dashed",colour = "black") +
                      theme_bw()
                g2
              }
            })
          
          
          data_3 <- reactive({
            if(input$map_zone == ""){
              return(NULL)
            } else {
              all_rd_df %>% mutate(buffer_name == as.factor(buffer_name)) %>%
                subset(buffer_name == input$park) %>%
                mutate(change = (LATE/defo_mean) * 100) %>%
                mutate(valid = ifelse(p_value > 0.05, 0, 1)) %>%
                mutate(avoided = LATE * N_r)
            }
          }) 
          
          output$avoided_event <-
            renderUI({
              if(input$map_zone == ""){
                return(NULL)
              } else if(data_3()$valid == 1 & data_3()$LATE < 0){
                withMathJax(HTML(paste0("<b>LATE: </b>", data_3()$LATE, " ha/km$^2$",
                                        "<b><br /> Avoided deforestation: </b>", round(data_3()$avoided, 3), " ha.",
                                        "<b><br /> Percent change: </b>", round(data_3()$change, 2), " %")))
              } else if (data_3()$valid == 1 & data_3()$LATE > 0){
                withMathJax(HTML(paste0("<b>LATE: </b>", data_3()$LATE, " ha/km$^2$",
                                        "<b><br /> Excess deforestation: </b>", round(data_3()$avoided, 3), " ha.",
                                        "<b><br /> Percent change: </b>", round(data_3()$change, 2), "%")))
              } else if(data_3()$valid == 0){
                withMathJax(HTML(paste0("<center>The selected park does not have a significant effect</center>")))
              }
            })
          

}

#---------------------#
# Run the application #
#---------------------#
shinyApp(ui = ui, server = server)
