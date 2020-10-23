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
        sliderInput("muNCI", "NCI mean:",
                     min = 0, max = 300, value = 124, step = 1),
        numericInput("sigmaNCI", "Variance of NCI:",
                     min = 0, max = 100, value = 2.651, step = 1),
        numericInput("p", "Probability of negative deltaNCI:",
                     min = 0, max = 1, value = 0.271, step = 0.1),
        numericInput("mu", "Positive deltaNCI mean:",
                     min = 0, max = 10, value = 0.749, step = 0.1),
        numericInput("sigma", "Variance of positive delta NCI:",
                     min = 0, max = 10, value = 2.651, step = 0.5),
        numericInput("lambda", "Lamnda for exponential negative deltaNCI:",
                     min = 0, max = 5, value = 0.31, step = 0.1),
        sliderInput("d", "Spatial aggregation of forest gap dynamics:",
                    min = 0, max = 50, value = 3),
        numericInput("sigmaGtopo", "Variance of topography genotypes:",
                     min = 0, max = 100, value = 1, step = 1),
        numericInput("sigmaZtopo", "Variance of topography phenotypes:",
                     min = 0, max = 100, value = 1, step = 1),
        numericInput("sigmaGnci", "Variance of NCI genotypes:",
                     min = 0, max = 100, value = 26, step = 1),
        numericInput("sigmaZnci", "Variance of NCI phenotypes:",
                     min = 0, max = 100, value = 26, step = 1),
        numericInput("Pdeath", "Mortality probability:",
                    min = 0, max = 1, value = 1, step = 0.1),
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