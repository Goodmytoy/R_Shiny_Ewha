#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# 필요한 패키지만 설치
required_packages = c("readxl", "data.table", "plotly", "ggplot2", "shiny")

for(pkg in required_packages){
  if(!pkg %in% required_packages){
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readxl)
library(data.table)
library(plotly)
library(ggplot2)
library(shiny)

# 1. Load Data
last_test_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 2))
last_weekly_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 3))
last_total_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 4))
this_test_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 5))
this_weekly_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 6))
# this_total_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 7))

# 2. Merge Data
last_test_score = last_test_score[last_total_score[,c("수강생", "실험집단", "성적등급"), with = FALSE], 
                                  on = c("수강생", "실험집단")]
last_year_score = last_test_score[last_weekly_score, on = c("수강생", "실험집단")][last_total_score,on = c("수강생", "실험집단")]
this_year_score = this_test_score[this_weekly_score, on = c("수강생", "실험집단")]

# 3. Plotting

melted_last_test_score = melt(last_test_score, 
                         id.vars = c("수강생", "실험집단", "성적등급"), 
                         measure.vars = c("중간점수", "기말점수"), 
                         variable.name = "type", 
                         value.name = "score")




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   

rgb_group_A = rgb(102,176,226, maxColorValue = 255)
rgb_group_B = rgb(255,189,55, maxColorValue = 255)
rgb_group_C = rgb(127,108,171, maxColorValue = 255)
rgb_group_D = rgb(158,200,110, maxColorValue = 255)
rgb_gray = rgb(192, 192, 192, maxColorValue = 255)
pal <- c(rgb_group_A, rgb_group_B, rgb_group_C, rgb_group_D)
pal <- c(rgb_gray, rgb_gray, rgb_gray, rgb_gray)

output$Test_Score_Plot1 = renderPlotly({
  g = ggplot(melted_last_test_score, 
            aes(x = type, y = score, color = 성적등급)) + 
            scale_colour_manual(values=pal) +
            geom_jitter(position = position_jitter(0.1), cex = 1.3) +
            ylim(0, 100) +
            labs(title="시험점수",x="구분", y = "점수") + 
            theme(plot.title = element_text(hjust = 0.5, face="bold"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text = element_text(face="bold"),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.ticks.x = element_blank())
  ggplotly(g)
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
