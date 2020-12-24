ui <- fluidPage(
  
  # Application title
  titlePanel("tombala-draweR"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "draw_number",
                   label = "Draw number",
                   icon=icon("arrow-right")
      ),
      actionButton(inputId = "go_back",
                   label = "Go back",
                   icon=icon("arrow-left")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("currentNumber"),
      plotOutput("heatmap"),
      tableOutput("lastNumber")
    )
  )
)
