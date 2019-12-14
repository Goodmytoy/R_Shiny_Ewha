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
## 2.1 Last Year Data
last_test_score = last_test_score[last_total_score[,c("수강생", "실험집단", "성적등급"), with = FALSE], 
                                  on = c("수강생", "실험집단")]
last_year_score = last_test_score[last_weekly_score, on = c("수강생", "실험집단")][last_total_score,on = c("수강생", "실험집단")]

## 2.2 This Year Data
this_year_score = this_test_score[this_weekly_score, on = c("수강생", "실험집단")]

# 3. Melt Data
# 3.1 Last Year Data
long_last_test_score = melt(last_test_score, 
                         id.vars = c("수강생", "실험집단", "성적등급"), 
                         measure.vars = c("중간점수", "기말점수"), 
                         variable.name = "type", 
                         value.name = "score")

long_last_weekly_score = melt(last_year_score,
                              id.vars = c("수강생", "실험집단", "성적등급"),
                              measure.vars = patterns("^y[0-9]{1,2}1$", "^y[0-9]{1,2}2$", "^y[0-9]{1,2}3$", "^y[0-9]{1,2}4$"),
                              variable.name = "week",
                              value.name = c("Q&A 게시글 수", "Q&A 댓글 수", "팀플 게시글 수", "팀플 댓글 수"))

long_last_year_qna = melt(long_last_weekly_score,
                          id.vars = c("수강생", "실험집단", "성적등급", "week"),
                          measure.vars = c("Q&A 게시글 수", "Q&A 댓글 수"),
                          variable.name = "type",
                          value.name = "count")

long_last_year_team = melt(long_last_weekly_score,
                          id.vars = c("수강생", "실험집단", "성적등급", "week"),
                          measure.vars = c("팀플 게시글 수", "팀플 댓글 수"),
                          variable.name = "type",
                          value.name = "count")

# 3.2 This Year Data
long_this_test_score = melt(this_test_score, 
                         id.vars = c("수강생", "실험집단"), 
                         measure.vars = c("중간점수"), 
                         variable.name = "type", 
                         value.name = "score")
long_this_test_score[,"성적등급" := "NA"]

long_this_weekly_score = melt(this_year_score,
                            id.vars = c("수강생", "실험집단"),
                            measure.vars = patterns("^y[0-9]{1,2}1$", "^y[0-9]{1,2}2$", "^y[0-9]{1,2}3$", "^y[0-9]{1,2}4$"),
                            variable.name = "week",
                            value.name = c("Q&A 게시글 수", "Q&A 댓글 수", "팀플 게시글 수", "팀플 댓글 수"))


long_this_year_qna = melt(long_this_weekly_score,
                          id.vars = c("수강생", "실험집단", "week"),
                          measure.vars = c("Q&A 게시글 수", "Q&A 댓글 수"),
                          variable.name = "type",
                          value.name = "count")
long_this_year_qna[,"성적등급" := "NA"]


long_this_year_team = melt(long_this_weekly_score,
                           id.vars = c("수강생", "실험집단", "week"),
                           measure.vars = c("팀플 게시글 수", "팀플 댓글 수"),
                           variable.name = "type",
                           value.name = "count")
long_this_year_team[,"성적등급" := "NA"]
  # rgb_group_A = rgb(102,176,226, maxColorValue = 255)
  # rgb_group_B = rgb(255,189,55, maxColorValue = 255)
  # rgb_group_C = rgb(127,108,171, maxColorValue = 255)
  # rgb_group_D = rgb(158,200,110, maxColorValue = 255)
  # rgb_gray = rgb(192, 192, 192, maxColorValue = 255)
  # pal <- c(rgb_group_A, rgb_group_B, rgb_group_C, rgb_group_D)
  # pal <- c(rgb_gray, rgb_gray, rgb_gray, rgb_gray)


fn_change_color = function(choices){
  # default setting
  rgb_group_A = rgb(102,176,226, maxColorValue = 255)
  rgb_group_B = rgb(255,189,55, maxColorValue = 255)
  rgb_group_C = rgb(127,108,171, maxColorValue = 255)
  rgb_group_D = rgb(158,200,110, maxColorValue = 255)
  rgb_gray = rgb(192, 192, 192, maxColorValue = 255)

  pal = rep(rgb_gray, 4)

  if("A" %in% choices){
    pal[1] = rgb_group_A
  }
  if("B" %in% choices){
    pal[2] = rgb_group_B
  }
  if("C" %in% choices){
    pal[3] = rgb_group_C
  }
  if("D" %in% choices){
    pal[4] = rgb_group_D
  }
  return(pal)
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$Test_Score_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group)

    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }

    g = ggplot(test_score_data, 
              aes(x = type, y = score, color = 성적등급)) + 
              # scale_colour_manual(values=pal) +
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
                    axis.ticks.x = element_blank(),
                    legend.position = "none")
    ggplotly(g) %>% 
      config(displayModeBar = F) %>%
       highlight(on = "plotly_click")
  })



  output$Online_QNA_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group)

    if(input$Mode == "지난 학기 수강생"){
      online_qna_data = long_last_year_qna
    }else if(input$Mode == "현재 학기 수강생"){
      online_qna_data = long_this_year_qna
    }

    g = ggplot(online_qna_data[week == as.numeric(input$Select_Week)], 
               aes(x=type, y=count, color = 성적등급)) + 
               scale_colour_manual(values=pal) +
               geom_jitter(position = position_jitter(0.1), cex = 1.3) +
               labs(title="온라인 Q&A 참여",x="구분", y = "점수") + 
               theme(plot.title = element_text(hjust = 0.5, face="bold"),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.text = element_text(face="bold"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.ticks.x = element_blank(),
                     legend.position = "none")
  ggplotly(g,showlegend = F)
  })


  output$Online_Team_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group)

    if(input$Mode == "지난 학기 수강생"){
        online_team_data = long_last_year_team
    }else if(input$Mode == "현재 학기 수강생"){
        online_team_data = long_this_year_team
    }
    
      g = ggplot(online_team_data[week == as.numeric(input$Select_Week)], 
                 aes(x=type, y=count, color = 성적등급)) + 
                 scale_colour_manual(values=pal) +
                 geom_jitter(position = position_jitter(0.1), cex = 1.3) +
                 labs(title="온라인 토론 참여",x="구분", y = "점수") + 
                 theme(plot.title = element_text(hjust = 0.5, face="bold"),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text = element_text(face="bold"),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       axis.ticks.x = element_blank(),
                       legend.position = "none")
    ggplotly(g,showlegend = F)
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
