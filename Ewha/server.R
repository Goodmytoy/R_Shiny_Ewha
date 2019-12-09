#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

no <- 1:length(letters)
name <- letters
height <- rnorm(length(letters), 170, 10)
weight <- rnorm(length(letters), 70, 10)

data <- data.frame(no,name,height, weight)


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$Test_Score_Plot1 <- renderPlot({

    stripchart(data["no"], pch = 20)
    
    })

  output$Test_Score_Plot2 <- renderPlot({

    stripchart(data["no"], pch = 20)
    
    })

  output$Test_Score_Plot3 <- renderPlot({

    stripchart(data["no"], pch = 20)
    
    })

  output$Test_Score_Plot4 <- renderPlot({

    stripchart(data["no"], pch = 20)
    
    })

  output$Test_Score_Plot5 <- renderPlot({

    stripchart(data["no"], pch = 20)
    
    })

  output$Test_Score_Plot6 <- renderPlot({

    stripchart(data["no"], pch = 20)
    
    })


})
