# devtools::install_github("sylvainschmitt/ecoevosimulator")
# need to install from github to push on shinyapps

library(ecoevosimulator)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())

ui <- dashboardPage(
    dashboardHeader(title = "Eco-evo simulator",
                    tags$li(class = "dropdown",
                            actionButton("simulate", "Simulate", icon = icon("recycle"),
                                         width = "300px"),
                            actionButton("compgif", "GIF", icon = icon("recycle"),
                                         width = "300px"),
                            actionButton("quit", "Quit", icon = icon("sign-out-alt"),
                                         width = "300px")
                            )
                    ),
    dashboardSidebar(
        tags$head(tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }"))), 
        sliderInput("grid", "Number of cells per side:",
                    min = 1, max = 100, value = 20),
        sliderInput("Nt", "Number of time steps:",
                    min = 1, max = 500, value = 50),
        selectInput("topography", "Topography generator", 
                    choices = c('sinusoidal', 'squarediamond', 'paracou'), selected = 'sinusoidal'),
        conditionalPanel(
            condition = "input.topography == 'paracou'",
            sliderInput("plot", "Paracou plot:",
                        min = 1, max = 15, value = 1)
        ),
        conditionalPanel(
            condition = "input.topography != 'squarediamond'",
            sliderInput("Elim", "Environmental gradient size:",
                        min = 0, max = 100, value = 5)  
        ),
        conditionalPanel(
            condition = "input.topography == 'sinusoidal'",
            sliderInput("amplitude", "Environmental gradient amplitude:",
                        min = 0, max = 10, value = 1, step = 1)  
        ),
        conditionalPanel(
            condition = "input.topography == 'squarediamond'",
            sliderInput("rudgeness", "Environmental gradient rudgeness:",
                        min = 0, max = 10, value = 1, step = 1)
        ),
        numericInput("sigmaG", "Genetic variance:",
                     min = 0, max = 10, value = 1, step = 1),
        numericInput("sigmaE", "Environmental variance:",
                     min = 0, max = 10, value = 1, step = 1),
        numericInput("Pfall", "Treefall probability:",
                    min = 0, max = 1, value = 0.01, step = 0.05),
        sliderInput("Rgaps", "Gap radius:",
                    min = 0, max = 50, value = 2),
        numericInput("Pdeath", "Background mortality probability:",
                    min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("Ns", "Number of seedlings:",
                    min = 0, max = 50, value = 4),
        sliderInput("Rdispersal", "Dispersal radius:",
                    min = 0, max = 50, value = 1),
        checkboxInput("determinist", "Determinist", value = TRUE)
    ),
    dashboardBody(
        fluidRow(
            tabBox(
                title = NULL, width = 12,
                tabPanel("Simulation", plotOutput("simulator", height = "800px")),
                tabPanel("GIF", "Press the button to compute the GIF, be patient GIF computing can be long.", 
                         plotOutput("gif", height = "800px"))
            )
        )
    )
)