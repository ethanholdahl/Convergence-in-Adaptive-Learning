


## Load and install the packages
library("tidyverse")
library("shiny")
theme_set(theme_minimal())


# Define UI for the application
navbarPage(
  title = HTML("Convergence to a Convention in Adaptive Play by <a href='https://ethanholdahl.com'>Ethan Holdahl</a>"),
  tabPanel(
    "Unperturbed 2x2 Coordination Game",
    tags$body(
      withMathJax(),
      h2("Some Title"),
      helpText("Some Text."),
      # Sidebar with a slider input for number of bins
      verticalLayout(
        plotOutput("gametable"),
        
        wellPanel(
          sliderInput(
            "alpha",
            label = "\\(\\alpha\\): The smallest probability that Player 2 plays action 1 such that playing action 1 is a best response for Player 1",
            min = .01,
            max = .99,
            step = .01,
            value = .01
          ),
          sliderInput(
            "beta",
            label = "\\(\\beta\\): The smallest probability that Player 1 plays action 1 such that playing action 1 is a best response for Player 2",
            min = .01,
            max = .99,
            step = .01,
            value = .99
          ),
          sliderInput(
            "m",
            label = div(HTML("<em>m</em>: The size of the memory")),
            min = 2,
            max = 20,
            step = 1,
            value = 13
          ),
          sliderInput(
            "s",
            label = div(HTML("<em>s</em>: The sample size")),
            min = 1,
            max = 19,
            step = 1,
            value = 11
          ),
          radioButtons(
            "scenario",
            "Scenario:",
            choices = c("Random", "Match", "Switch Different", "Switch Same"),
            selected = "Switch Same"
          ),
          actionButton("regenerate",
                       "Regenerate"),
          sliderInput(
            "animation",
            "Animation:",
            min = 1,
            max = 30,
            step = 1,
            value = 1,
            animate = animationOptions(interval = 500, loop = TRUE)
          )
          
        ),
        
        
        # Show a plot of the generated distribution
        plotOutput("gifconverge"),
      )
    )
  )
)
