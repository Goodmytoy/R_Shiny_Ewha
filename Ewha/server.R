#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# 필요한 패키지만 설치
required_packages = c("readxl", "data.table", "plotly", "ggplot2", "shiny", "htmlwidgets")

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
library(htmlwidgets)

# 0. Set My Data
my_data = "x11"

# default setting
rgb_group_A = rgb(102,176,226, maxColorValue = 255)
rgb_group_B = rgb(255,189,55, maxColorValue = 255)
rgb_group_C = rgb(127,108,171, maxColorValue = 255)
rgb_group_D = rgb(158,200,110, maxColorValue = 255)
rgb_gray = rgb(192, 192, 192, maxColorValue = 255)
rgb_green = rgb(41, 76, 43, maxColorValue = 255)

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

# 4. Group by Weekly Data
long_last_weekly_score[, `Q&A 게시글 수 평균` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]
long_last_weekly_score[, `Q&A 댓글 수 평균` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]
long_last_weekly_score[, `팀플 게시글 수 평균` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]
long_last_weekly_score[, `팀플 댓글 수 평균` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]

long_this_weekly_score[, `Q&A 게시글 수 평균` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균` := mean(`Q&A 게시글 수`), by = list(week)]

fn_change_color = function(choices, type = "지난 학기 수강생"){

  pal = rep(rgb_gray, 4)

  if(type == "지난 학기 수강생"){
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
  }
  
  return(pal)
}


javascript = "function(el, x){
                el.on('plotly_click', function(data) {
                  var point = document.getElementsByClassName('scatterlayer')[0].getElementsByClassName('scatter')[data.points[0].curveNumber].getElementsByClassName('point')[data.points[0].pointNumber];
                  var plotly_div = document.getElementsByClassName('plotly')[0];
                  if (plotly_div.backup !== undefined) {
                    var old_point = document.getElementsByClassName('scatterlayer')[0].getElementsByClassName('scatter')[plotly_div.backup.curveNumber].getElementsByClassName('point')[plotly_div.backup.pointNumber]
                    if (old_point !== undefined) {
                      old_point.setAttribute('d', plotly_div.backup.d);
                    }
                  }
                  plotly_div.backup = {curveNumber: data.points[0].curveNumber,
                                      pointNumber: data.points[0].pointNumber,
                                      d: point.attributes['d'].value,
                                      style: point.attributes['style'].value
                                      }

                  point.setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
                });
              }"



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #Dynamic Slider Input (max_week)
  output$max_week_slider_input = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      max_week = max(as.numeric(long_last_year_qna$week))
    }else if(input$Mode == "현재 학기 수강생"){
      max_week = max(as.numeric(long_this_year_qna$week))
    }

    sliderInput(
      inputId = "Select_Week", 
      label = h4(p(strong("주차 선택"))),
      min = 1,
      max = max_week,
      value = 1,
      post = "주차",
      width = "90%"
    )
  })


  output$Mid_Test_Score_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }

    test_score_data = test_score_data[type == "중간점수",]  

    cat(paste0("<중간점수> \n",
               "평균 = ", round(mean(test_score_data$score),2), "점\n", 
               "표준편차 = ", round(sd(test_score_data$score),2)))
  })

  output$Final_Test_Score_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }

    test_score_data = test_score_data[type == "기말점수",]

    cat(paste0("<기말점수> \n",
               "평균 = ", round(mean(test_score_data$score),2), "점\n", 
               "표준편차 = ", round(sd(test_score_data$score),2)))
  })

  output$Online_QNA_Post_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      online_qna_data = long_last_year_qna
    }else if(input$Mode == "현재 학기 수강생"){
      online_qna_data = long_this_year_qna
    }

    online_qna_data = online_qna_data[week %in% as.numeric(input$Select_Week) & type == "Q&A 게시글 수",]

    cat(paste0("<Q&A 게시글 수> \n",
               "평균 = ", round(mean(online_qna_data$count),2), "개\n", 
               "표준편차 = ", round(sd(online_qna_data$count),2)))
  })

  output$Online_QNA_Reply_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      online_qna_data = long_last_year_qna
    }else if(input$Mode == "현재 학기 수강생"){
      online_qna_data = long_this_year_qna
    }

    online_qna_data = online_qna_data[week %in% as.numeric(input$Select_Week) & type == "Q&A 댓글 수",]

    cat(paste0("<Q&A 댓글 수> \n",
               "평균 = ", round(mean(online_qna_data$count),2), "개\n", 
               "표준편차 = ", round(sd(online_qna_data$count),2)))
  })

  output$Online_Team_Post_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      online_team_data = long_last_year_team
    }else if(input$Mode == "현재 학기 수강생"){
      online_team_data = long_this_year_team
    }

    online_team_data = online_team_data[week %in% as.numeric(input$Select_Week) & type == "팀플 게시글 수",]

    cat(paste0("<팀플 게시글 수> \n",
               "평균 = ", round(mean(online_team_data$count),2), "개\n", 
               "표준편차 = ", round(sd(online_team_data$count),2)))
  })

   output$Online_Team_Reply_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      online_team_data = long_last_year_team
    }else if(input$Mode == "현재 학기 수강생"){
      online_team_data = long_this_year_team
    }

    online_team_data = online_team_data[week %in% as.numeric(input$Select_Week) & type == "팀플 댓글 수",]

    cat(paste0("<팀플 댓글 수> \n",
               "평균 = ", round(mean(online_team_data$count),2), "개\n", 
               "표준편차 = ", round(sd(online_team_data$count),2)))
  })
  
   

  output$Test_Score_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }
    set.seed(100)
    g = ggplot(test_score_data, 
              aes(x = type, y = score, color = 성적등급)) + 
              scale_colour_manual(values = c(pal, rgb_green)) +
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
    g = g + geom_point(data = long_this_test_score[수강생 == my_data,], 
                position = position_jitter(0.1), cex = 1.7, , color = rgb_green)              
    ggplotly(g) %>% 
      config(displayModeBar = F) %>%
       onRender(javascript)
  })



  output$Online_QNA_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      online_qna_data = long_last_year_qna
    }else if(input$Mode == "현재 학기 수강생"){
      online_qna_data = long_this_year_qna
    }
    set.seed(100)
    g = ggplot(online_qna_data[week %in% as.numeric(input$Select_Week)], 
               aes(x=type, y=count, color = 성적등급)) + 
               scale_colour_manual(values=c(pal, rgb_green)) +
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

    g = g + geom_point(data = long_this_year_qna[week %in% as.numeric(input$Select_Week) & 수강생 == my_data,], 
                position = position_jitter(0.1), cex = 1.7, , color = rgb_green)

    ggplotly(g) %>% 
      config(displayModeBar = F) %>%
       onRender(javascript)
  })


  output$Online_Team_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)
    
    if(input$Mode == "지난 학기 수강생"){
      online_team_data = long_last_year_team
    }else if(input$Mode == "현재 학기 수강생"){
      online_team_data = long_this_year_team
    }

    set.seed(100)
    g = ggplot(online_team_data[week %in% as.numeric(input$Select_Week)], 
                aes(x=type, y=count, color = 성적등급)) + 
                scale_colour_manual(values=c(pal, rgb_green)) +
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

    g = g + geom_point(data = long_this_year_team[week %in% as.numeric(input$Select_Week) & 수강생 == my_data,], 
                position = position_jitter(0.1), cex = 1.7, , color = rgb_green)

    ggplotly(g) %>% 
      config(displayModeBar = F) %>%
       onRender(javascript)
    })


  
  output$Weekly_Mean_QNA_Post_Plot = renderPlotly({
    if(input$Mode == "지난 학기 수강생"){
      max_week = max(as.numeric(long_last_year_qna$week))
    }else if(input$Mode == "현재 학기 수강생"){
      max_week = max(as.numeric(long_this_year_qna$week))
    }

    g1 = ggplot(long_last_weekly_score, aes(x = week, y = `Q&A 게시글 수 평균`, group = 수강생, color = `성적등급`)) +
      geom_line() +
      geom_point() + 
      scale_x_discrete(position = "top", labels = paste0(rep(1:max_week), " 주차")) +
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.text = element_text(face="bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_rect(fill = NA, color = "black"),
            panel.grid.major = element_line(color = "grey"),
            panel.grid.minor = element_line(color = "grey"),
            panel.background = element_blank(),
            legend.position = "none")
    
    ggplotly(g1)%>% 
      config(displayModeBar = F)
  })

  output$Weekly_Mean_QNA_Reply_Plot = renderPlotly({
    if(input$Mode == "지난 학기 수강생"){
      max_week = max(as.numeric(long_last_year_qna$week))
    }else if(input$Mode == "현재 학기 수강생"){
      max_week = max(as.numeric(long_this_year_qna$week))
    }

    g1 = ggplot(long_last_weekly_score, aes(x = week, y = `Q&A 댓글 수 평균`, group = 수강생, color = `성적등급`)) +
      geom_line() +
      geom_point() + 
      scale_x_discrete(position = "top", labels = paste0(rep(1:max_week), " 주차")) +
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.text = element_text(face="bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_rect(fill = NA, color = "black"),
            panel.grid.major = element_line(color = "grey"),
            panel.grid.minor = element_line(color = "grey"),
            panel.background = element_blank(),
            legend.position = "none")
    
    ggplotly(g1)%>% 
      config(displayModeBar = F)
  })

  output$Weekly_Mean_Team_Post_Plot = renderPlotly({
    if(input$Mode == "지난 학기 수강생"){
      max_week = max(as.numeric(long_last_year_qna$week))
    }else if(input$Mode == "현재 학기 수강생"){
      max_week = max(as.numeric(long_this_year_qna$week))
    }

    g1 = ggplot(long_last_weekly_score, aes(x = week, y = `팀플 댓글 수 평균`, group = 수강생, color = `성적등급`)) +
      geom_line() +
      geom_point() + 
      scale_x_discrete(position = "top", labels = paste0(rep(1:max_week), " 주차")) +
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.text = element_text(face="bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_rect(fill = NA, color = "black"),
            panel.grid.major = element_line(color = "grey"),
            panel.grid.minor = element_line(color = "grey"),
            panel.background = element_blank(),
            legend.position = "none")
    
    ggplotly(g1)%>% 
      config(displayModeBar = F)
  })


  output$Weekly_Mean_Team_Reply_Plot = renderPlotly({
    if(input$Mode == "지난 학기 수강생"){
      max_week = max(as.numeric(long_last_year_qna$week))
    }else if(input$Mode == "현재 학기 수강생"){
      max_week = max(as.numeric(long_this_year_qna$week))
    }

    g1 = ggplot(long_last_weekly_score, aes(x = week, y = `팀플 댓글 수 평균`, group = 수강생, color = `성적등급`)) +
      geom_line() +
      geom_point() + 
      scale_x_discrete(position = "top", labels = paste0(rep(1:max_week), " 주차")) +
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.text = element_text(face="bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_rect(fill = NA, color = "black"),
            panel.grid.major = element_line(color = "grey"),
            panel.grid.minor = element_line(color = "grey"),
            panel.background = element_blank(),
            legend.position = "none")
    
    ggplotly(g1)%>% 
      config(displayModeBar = F)
  })
})