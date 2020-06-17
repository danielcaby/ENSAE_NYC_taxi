#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(
  fluidPage(
    # Chargement du thème css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    ),
    div(img(src = "img/nyct_img.png",width = 800), style="text-align: center;"),
    br(),
    # navbarPage
    navbarPage("NYC Taxis",
               tabPanel("Exploration des données",
                                    fluidPage(
                                       column(width = 3, selectInput("yearInput", "Année : ", choices = yearSelectListChoices),
                                              selectInput("monthInput", "Mois : ", choices = monthSelectListChoices),
                                              selectInput("dayInput", "Jour : ", choices = daySelectListChoices)

                                       ),
                                       column(width = 8,
                                              plotlyOutput('myPlot',height = 350))
                                     )
                            ),
               # Onglet stats desc
               tabPanel("Statistiques descriptives", 
                        navlistPanel(
                          widths = c(3, 9), 
                          tabPanel("Durée des trajets",
                                   tabsetPanel(
                                     tabPanel("Histogramme",plotlyOutput("hist_duration")),
                                     tabPanel("Boxplot",plotlyOutput("bp_duration"))
                                   )),
                          tabPanel("Lien distance - durée",plotlyOutput("link_dist_dur")),
                          tabPanel("Distance des trajets",
                                   tabsetPanel(
                                     tabPanel("Histogramme",plotlyOutput("hist_dist")),
                                     tabPanel("Boxplot",plotlyOutput("bp_dist"))
                                   )),
                          tabPanel("Influence de l'heure",
                                   tabsetPanel(
                                     tabPanel("Sur la durée",plotlyOutput("hours_dur")),
                                     tabPanel("Sur la distance",plotlyOutput("hours_dist")),
                                     tabPanel("Sur le coef. de regression",plotlyOutput("hours_reg"))
                                     )),
                          tabPanel("Influence du jour de la semaine",
                                   tabsetPanel(
                                     tabPanel("Sur la durée",plotOutput("wd_dur")),
                                     tabPanel("Sur la distance",plotOutput("wd_dist")),
                                     tabPanel("Sur le coef. de regression",plotlyOutput("wd_reg"))
                                   )),
                          #tabPanel("Carte de la densité des taxis _ to delete",leafletOutput("myMap")),
                          tabPanel("Carte de la densité des taxis",leafletOutput("map_dens"))
                        )
               ), 
               
               # Onglet simulation à partir du modèle entrainé. 
               tabPanel("Résultats de la modélisation", 
                        
                        fluidRow(
                          # premier colonne
                          column(width = 3, 
                                 # wellPanel pour griser
                                 wellPanel(
                                   h1("to do")
                                 )
                          ), 
                          # deuxieme colonne
                          column(width = 9, 
                                 h1("Feed me!")
                                 )
                          )
                        )
               )
    )
)
