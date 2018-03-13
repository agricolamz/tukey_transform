library(shiny)
library(tidyverse)
library(latex2exp)

ui <- fluidPage(
   titlePanel("Tukey transformation"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("power",
                     "bandwidth adjustment:",
                     min = -5,
                     max = 5,
                     value = 1,
                     step = 0.01)
      ),
      mainPanel(
         plotOutput("distPlot")
      )
   )
)
server <- function(input, output) {
  ldt <- read_csv("https://goo.gl/ToxfU6")
   output$distPlot <- renderPlot({
     if(input$power > 0){
       ldt %>% 
         ggplot(aes(Mean_RT, (Freq+1)^input$power))+
         geom_point()+
         theme_bw()+
         labs(x = "average reaction time",
              y = TeX(paste0("(word frequencies)^{", input$power, "}")),
              title = paste("correlation", cor(ldt$Mean_RT, (ldt$Freq+1)^input$power)))+
         theme(text = element_text(size = rel(5)),
               plot.title = element_text(size = rel(5)))
     } else if(input$power < 0){
       ldt %>% 
         ggplot(aes(Mean_RT, -(Freq+1)^input$power))+
         geom_point()+
         theme_bw()+
         labs(x = "average reaction time",
              y = TeX(paste0("-(word frequencies)^{", input$power, "}")),
              title = paste("correlation", cor(ldt$Mean_RT, (ldt$Freq+1)^input$power)))+
         theme(text = element_text(size = rel(5)),
               plot.title = element_text(size = rel(5)))
     } else {
       ldt %>% 
         ggplot(aes(Mean_RT, log(Freq+1)))+
         geom_point() +
         theme_bw()+
         labs(x = "average reaction time",
              y = "log(word frequencies)",
              title = paste("correlation", cor(ldt$Mean_RT, log(ldt$Freq+1))))+
         theme(text = element_text(size = rel(5)),
               plot.title = element_text(size = rel(5)))
     }
   })
}
shinyApp(ui = ui, server = server)

