library(shiny)
library(ggplot2)
library(binom)

shinyUI(pageWithSidebar(
  headerPanel("Funnel plots"),
  sidebarPanel(
    sliderInput('pop.mort', 'Population procedure mortality', min=0.01, max=0.5,
      value=0.1, step=0.01),
    sliderInput('surg.mort', 'Individual surgeon procedure mortality', min=0.01, max=0.5,
      value=0.1, step=0.01),
    sliderInput('n_sim', 'Number of simulations', min=10, max=500,
      value=100)	 	  
  ),
 
  mainPanel(
    plotOutput('plot'),
	tableOutput('view')
  )
))