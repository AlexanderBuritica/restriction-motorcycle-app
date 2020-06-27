
# Load packages
library("shiny")
library("tidyverse")
library("maptools")
library("leaflet")
library("htmltools")

#---------------------------------#
# Definiendo interfaz del usuario #
#---------------------------------#
ui <- navbarPage(title = "Motorcycle restrictions and their effect on crime",
                 theme="http://bootswatch.com/spacelab/bootstrap.css", 
                 inverse=TRUE,
                 
                 # Panel Results
                 tabPanel("Results",
                        #  p("This proyect ...")
                        withMathJax(),
                        tags$style(type="text/css", "html, body {width:100%;height:100%}"),
                        div(class="outer",
                            tags$head(includeCSS("www/style.css"), tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", 
                                                                               type = 'text/javascript'),
                                      tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')),
                            leafletOutput("mymap", width="100%", height="100%"),
                            absolutePanel(id = "controls", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h2("Motorcycle restrictions and their effect on crime"),
                                          h4("Select one restriction:"),
                                          selectInput(inputId = "zone", label = "", choices = c("","Male passenger - Bogotá",
                                                                                                   "Male passenger - Barranquilla",
                                                                                                   "No passengers - Neiva",
                                                                                                   "No passengers - Cartagena",
                                                                                                   "No motorcycles - Barranquilla",
                                                                                                   "No motorcycles - Soledad"), selected = ""),
                                          plotOutput("rdplot", width = 300, height = 300)
                            ),
                            absolutePanel(id = "estimate", fixed = TRUE,
                                          draggable = TRUE, top = 100, left = 40, right = "auto", bottom = "auto",
                                          width = 280, height = "auto",
                                          conditionalPanel(
                                                          condition = "input.zone != '' ",
                                                          h5("Event Study", align = "center"),
                                                          h6("Monthly average effect", align = "center"),
                                                          plotOutput("eventstudy", width = 260, height = 260),
                                                          htmlOutput("avoided1")
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

#---------------#
# Define server #
#---------------#
server <- function(input, output) {

    output$grafico <- renderPlot({

    })
}

#--------------------------------#
# Definiendo el mapa del leaflet #
#--------------------------------#



#---------------------#
# Run the application #
#---------------------#
shinyApp(ui = ui, server = server)
