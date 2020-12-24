# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat <- reactive({
    pool <- 1:90
    pool_scramble <- sample(pool)
    dat <- data.frame(Draw = pool, 
                      Number = pool_scramble)
    return(dat)
  })
  
  
  currentID <- reactive({
    currentID <- input$draw_number - input$go_back
  })
  
  heatdat <- reactive({
    mat <- matrix(as.numeric(dat()$Draw), nrow = 10, ncol = 9, byrow = T)
    hmMat <- mat %in% dat()[1:currentID(),"Number"] 
    hmMat <- matrix(as.numeric(hmMat), nrow = 10, ncol = 9, byrow = F)
    
    return(list(mat = mat, hmMat = hmMat))
  })
  
  output$currentNumber <- renderText({
    x <- dat()[currentID(),"Number"]
    if(currentID() <= 0){
      paste('<p style="font-size:30px">', "Press 'Draw number' to start game!", '</p>')
    }else if(currentID() > 0 & currentID() <= 90){
      paste('<p style="font-size:120px">', x, '</p>')    
    }else if(currentID() > 90){
      paste('<p style="font-size:30px">', "Game over!", '</p>')
    }
    
  })
  
  heatmapPlot <- reactive({
    hdat <- melt(heatdat()$mat)
    hdat$col <- 0
    if(currentID() != 0){
      hdat$col <- hdat$value %in% dat()[1:currentID(),"Number"]
      hdat$col <- as.numeric(hdat$col)
    }
    
    heatmap <- ggplot(hdat, aes((Var2), rev(Var1))) +
      geom_tile(aes(fill = col)) + 
      geom_text(aes(label = value)) +
      scale_fill_gradient(low = "white", high = "red") + 
      theme_void() + theme(legend.position = "none")
    
    return(heatmap)
  })
  
  output$heatmap <- renderPlot({
    heatmapPlot()
  })
  
  output$lastNumber <- renderTable({
    lastnumber <- tail(dat()[1:currentID(),], 5)
  })
  
}