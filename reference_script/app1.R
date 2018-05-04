library(shiny)
library(ggplot2)

# define the user interface (ui)

ui <- fluidPage()

ui <- fluidPage(title = "App1",
                "Hellor World",
                sliderInput(inputId = "num",
                            min = 0, 
                            max = 100, 
                            value = 75, 
                            label = "choose a value:"),
                plotOutput(outputId = "hist")
                )

# define the server

server <- function(input, output){
  
  # define the dataset
data = reactive({
  data.frame(x = rnorm(input$num))
})


output$hist = renderPlot({
  #plot a histogram of n random number
  ggplot(data(), aes(x)) +  # add data() because it is reactive
  geom_histogram()
})
}




#run the shiny app

shinyApp(ui = ui, server = server)


