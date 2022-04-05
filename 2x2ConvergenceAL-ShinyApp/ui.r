


## Load and install the packages
library("tidyverse")
#library(shinythemes)
library("shiny")


# Define UI for the application
fluidPage(
  #theme = shinytheme("sandstone"),
  tags$head(HTML("<title> Convergence to a Convention in Adaptive Play by Ethan Holdahl </title>")),
navbarPage(
  title = HTML("Convergence to a Convention in Adaptive Play by <a href='https://ethanholdahl.com'>Ethan Holdahl</a>"),
  tabPanel(
    "Unperturbed 2x2 Coordination Game",
    tags$body(
      withMathJax(),
      h2("Convergence to a Convention in Adaptive Play in an Unperturbed 2x2 Coordination Game: A Relaxation of the Bounds of s"),
      helpText("Young 1993\\(^1\\), established that in 2x2 coordination games a convention can be reached under unperturbed adaptive play dynamics given that the sample size (s) was sufficiently small compared to the memory size (m). 
               The limit of this bound was \\(k \\leq m / (L\\_r+2)\\) where \\(L\\_r\\) is the maximum of the set of lengths of the shortest directed  path in the best reply graph from \\(s\\) to a strict Nash equilibrium. 
               The bounds of this limit was expanded in Young 2020 \\(^2\\) to simply requiring \\(s/m \\leq 1/2\\). In both cases, Young did not claim that the bound on \\(s/m\\) was the best possible.
               Here, I prove that any degree of incompleteness, \\(s<m\\) is a sufficient amount of incompleteness for conventions arise in the unperturbed proccess."),
      br(),
      helpText("The sampling algorithm that makes the proof work is essentially the following:"),
      br(),
      helpText("1. both players sample the most recent \\(s\\) records available"),
      helpText("2. If players have different best responses continue to sample the most recent \\(s\\) records until atleast one player's best response changes."),
      helpText("3. If at any point both players have the same best response when sampling the most recent \\(s\\) records then if they continue to sample the most recent \\(s\\) records for \\(m\\) periods a convention will be reached and locked into."),         
      helpText("4. If instead, there is a period where both players best responses flip simultaneously call this period \\(t\\)"),
      helpText("5. For one player, have them sample the most recent \\(s\\) records from period \\(t+1\\) through period \\(t+m\\)"),
      helpText("5. For the other player, have them sample the most recent \\(s+1\\) records excluding the record created in period \\(t\\) from period \\(t+1\\) through period \\(t+m\\)"),
      br(),
      helpText("The above algorithm works as long as \\(s < m\\). For an interactive and visual representation of the dynamics in action, please input any variables you like and use the application below.
               The full rigorous proof is available to read below the application, or the PDF of the proof can be downloaded using the link below."),
      a(href="Convergence_to_a_Convention_in_a_2x2_Coordination_Game.pdf", "Download PDF of Proof", download=NA, target="_blank"),
      verticalLayout(
        wellPanel(
          sliderInput(
            "alpha",
            label = "\\(\\alpha\\): The smallest probability that Player B plays action 1 such that playing action 1 is a best response for Player A",
            min = .01,
            max = .99,
            step = .01,
            value = .01
          ),
          sliderInput(
            "beta",
            label = "\\(\\beta\\): The smallest probability that Player A plays action 1 such that playing action 1 is a best response for Player B",
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
            choices = c("Random" = "Random", "Case 1: Best Responses Match at Period t+1" = "Match", "Case 2a: j \\(\\neq\\) k" = "Switch Different", "Case 2b: j = k" = "Switch Same"),
            selected = "Switch Same"
          ),
          actionButton("regenerate",
                       "Regenerate"),
          sliderInput(
            "animation",
            "Possible evolution of play leading to a convention (click play):",
            min = 1,
            max = 30,
            step = 1,
            value = 1,
            animate =  animationOptions(interval = 1000, loop = TRUE)
          )
          
        ),
        
        
        # Show a plot of the generated distribution
        plotOutput("gifconverge")
      ),
      wellPanel(
      tags$iframe(style = "height:800px; width:100%; scrolling=yes", 
                  src = "Convergence_to_a_Convention_in_a_2x2_Coordination_Game.pdf"),
      ),
      h3("References"),
      helpText('1. Young, H. Peyton. "The evolution of conventions." Econometrica: Journal of the Econometric Society (1993): 57-84.'),
      helpText("2. Young, H. Peyton. Individual strategy and social structure. Princeton University Press, 2020.")
       )
  )
)
)

