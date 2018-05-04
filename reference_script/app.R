library(shiny)

ui <- fluidPage(
  title = "Diamonds Data",
  sidebarLayout(
    
    sidebarPanel(
      helpText("This app is to visualize diamonds dataset")
               ),
    mainPanel()  #three paramiters in the sidebar
  ),
  plotOutput(outputId = "bar")
)

server<- function(input,output){
  output$bar = renderPlot({
    ggplot(data, aex = (x = cut)) +
      geom_bar() +
      ggtitle("I am a barplot")
  }) 
  }

shinyApp(ui,server)


data = diamonds
