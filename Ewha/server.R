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
rgb_red = rgb(255, 0, 0, maxColorValue = 255)

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
long_this_weekly_score[,"성적등급" := "NA"]

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
# 4.1 지난 학기 데이터
# 성적 그룹별
long_last_weekly_score[, `Q&A 게시글 수 평균 그룹별` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 그룹별` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]
long_last_weekly_score[, `팀플 게시글 수 평균 그룹별` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]
long_last_weekly_score[, `팀플 댓글 수 평균 그룹별` := mean(`Q&A 게시글 수`), by = list(성적등급, week)]

# 전체 학습자
long_last_weekly_score[, `Q&A 게시글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 게시글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 댓글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]

# 나와 유사한 학습자
my_score = this_test_score[수강생 == my_data, 중간점수]
similar_student = last_test_score[중간점수 >= (my_score - (100 * 0.05)) & 중간점수 <= (my_score + (100 * 0.05)), 수강생]
long_last_weekly_score[수강생 %in% similar_student, `Q&A 게시글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[수강생 %in% similar_student, `Q&A 댓글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[수강생 %in% similar_student, `팀플 게시글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[수강생 %in% similar_student, `팀플 댓글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]

# 최고점 학습자
long_last_weekly_score[, `Q&A 게시글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 게시글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 댓글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]

# 최저점 학습자
long_last_weekly_score[, `Q&A 게시글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 게시글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 댓글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]

# 4.2 현재 학기 데이터

#전체 학습자
long_this_weekly_score[, `Q&A 게시글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균 전체` := mean(`Q&A 게시글 수`), by = list(week)]

# 나와 유사한 학습자
my_score = this_test_score[수강생 == my_data, 중간점수]
similar_student = this_test_score[중간점수 >= (my_score - (100 * 0.05)) & 중간점수 <= (my_score + (100 * 0.05)), 수강생]
long_this_weekly_score[수강생 %in% similar_student, `Q&A 게시글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[수강생 %in% similar_student, `Q&A 댓글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[수강생 %in% similar_student, `팀플 게시글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[수강생 %in% similar_student, `팀플 댓글 수 평균 유사` := mean(`Q&A 게시글 수`), by = list(week)]
# 최고점 학습자
long_this_weekly_score[, `Q&A 게시글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]

# 최저점 학습자
long_this_weekly_score[, `Q&A 게시글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]

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
    if("ALL" %in% choices){
      pal[1] = rgb_group_A
      pal[2] = rgb_group_B
      pal[3] = rgb_group_C
      pal[4] = rgb_group_D
    }
  }
  
  return(pal)
}

fn_draw_bar_plot = function(data, this_data, y, pal, my_data = NA, my_data_y = NA){
  max_week = max(as.integer(data[,week]))
  temp_data = unique(data, by = c("성적등급", "week"))
  
  g = ggplot(temp_data, aes_string(x = "week", y = paste0("`", y, "`"), group = "수강생", color = "성적등급")) +
             scale_colour_manual(values = pal) +
             geom_line() +
             geom_point() + 
             scale_x_discrete(position = "top", labels = paste0(rep(1:max_week), " 주차")) +
             theme(plot.title = element_text(hjust = 0.5, face="bold"),
                   axis.text = element_text(face="bold"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.border = element_rect(fill = NA, color = "black"),
                   panel.grid.major = element_line(color = "gray"),
                   panel.grid.minor = element_line(color = "gray"),
                   panel.background = element_blank(),
                   legend.position = "none")
  if(is.na(my_data) == FALSE){
    g = g + geom_line(data = this_data[수강생 == my_data,], aes_string(x = "week", y = paste0("`", my_data_y, "`")), color = "red") 
  }
  
  return(ggplotly(g, tooltip = c("x", "y")))
}


fn_draw_strip_plot = function(data, this_data, y, type_vec, pal, my_data = NA){
  plot_list = vector("list", length = 2)
  
  for(i in 1:length(type_vec)){
    temp_data = data[type == type_vec[i]]
    if(nrow(temp_data) == 0){
      next
    }
    plot_list[[i]] = ggplot(temp_data, aes_string(x = "type", y = paste0("`", y, "`"), color = "성적등급")) + 
                          scale_colour_manual(values = c(pal, rgb_red)) +
                          geom_jitter(position = position_jitter(0.1), cex = 1.3) +
                          labs(title = "시험점수", x = "구분", y = "점수") + 
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
    if(y == "score"){
      plot_list[[i]] = plot_list[[i]] + ylim(0, 100)
    }
    if(is.na(my_data) == FALSE){
      if(type_vec[i] %in% this_data[,type]){
        plot_list[[i]] = plot_list[[i]] + geom_point(data = this_data[type == type_vec[i] & 수강생 == my_data,], position = position_jitter(0.1), cex = 3, shape = 18)   
      }
    } 
  }
  
  if(sum(sapply(plot_list, is.null)) == 0){
    result_plot = subplot(plot_list[[1]], plot_list[[2]], shareY = TRUE)
  }else{
    result_plot = ggplotly(plot_list[[1]])
  }
  
  return(result_plot)
}



# document.getElementsByClassName('scatterlayer')[4].getElementsByClassName('scatter')[data.points[0].curveNumber].getElementsByClassName('point')[data.points[0].pointNumber];
javascript = "function(el, x){
                el.on('plotly_click', function(data) {
                  var plot_len = document.getElementsByClassName('scatterlayer').length
                  var point_arr = new Array(plot_len);
                  var old_point_arr = new Array(plot_len);
                  var plotly_div_arr = new Array(plot_len);


                  for(i=0; i<plot_len; i++) {
                    point_arr[i] = document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('scatter')[data.points[0].curveNumber].getElementsByClassName('point')[data.points[0].pointNumber];
                    plotly_div_arr[i] =  document.getElementsByClassName('plotly')[i];
                  }

                  for(i=0; i<plot_len; i++) {
                    if (plotly_div_arr[i].backup !== undefined) {
                      old_point_arr[0] = document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('scatter')[plotly_div_arr[i].backup.curveNumber].getElementsByClassName('point')[plotly_div_arr[i].backup.pointNumber]
                      if (old_point_arr[0] !== undefined) {
                        old_point_arr[0].setAttribute('d', plotly_div_arr[i].backup.d);
                      }
                    } 
                  }
                  for(i=0; i<plot_len; i++) {
                    plotly_div_arr[i].backup = {curveNumber: data.points[0].curveNumber,
                                            pointNumber: data.points[0].pointNumber,
                                            d: point_arr[i].attributes['d'].value,
                                            style: point_arr[i].attributes['style'].value
                                            };
                  }

                  for(i=0; i<plot_len; i++) {
                    point_arr[i].setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
                  }
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


  ### Text Output (Mean, Standard Deviance)
  
  ## 시험점수 (Test Score)
  # 중간점수 (Mid Test Score)
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

  # 기말점수 (Mid Test Score)
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

  ## 온라인 Q&A 참여 (Online Q&A)
  # Q&A 게시글 수 (Q&A Post)
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

  # Q&A 댓글 수 (Q&A Reply)
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

  ## 온라인 토론 참여 (Online Team)
  # 팀플 게시글 수 (Team Post)
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

  # 팀플 댓글 수 (Team Reply)
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
  

  ### Plot Output (ggplotly)
  output$Test_Score_Plot = renderPlotly({
    pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }
    
    set.seed(100)
    test_plot = fn_draw_strip_plot(data = test_score_data,
                           this_data = long_this_test_score,
                           y = "score",
                           type_vec = c("중간점수", "기말점수"),
                           pal = pal,
                           my_data = my_data)


    test_plot %>%
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
    qna_plot = fn_draw_strip_plot(data = online_qna_data[week %in% as.numeric(input$Select_Week)],
                           this_data = long_this_year_qna[week %in% as.numeric(input$Select_Week)],
                           y = "count",
                           type_vec = c("Q&A 게시글 수", "Q&A 댓글 수"),
                           pal = pal,
                           my_data = my_data)

    qna_plot %>% 
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
    qna_plot = fn_draw_strip_plot(data = online_team_data[week %in% as.numeric(input$Select_Week)],
                                  this_data = long_this_year_team[week %in% as.numeric(input$Select_Week)],
                                  y = "count",
                                  type_vec = c("팀플 게시글 수", "팀플 댓글 수"),
                                  pal = pal,
                                  my_data = my_data)

    qna_plot %>% 
      config(displayModeBar = F) %>%
       onRender(javascript)
    })


  
  
  #######################################################
  # Pop Up
  #######################################################
  output$Weekly_Mean_QNA_Post_Plot = renderPlotly({
    pal = fn_change_color(choices = "ALL", type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }
    
    
    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "Q&A 게시글 수 평균 그룹별"
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 전체"
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 유사"
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 최고"
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 최저"
    }
    
    g = fn_draw_bar_plot(data = weekly_score, 
                         this_data = long_this_weekly_score, 
                         y = y_val, 
                         pal = pal,
                         my_data = my_data,
                         my_data_y = "Q&A 게시글 수")
    
    ggplotly(g)%>% 
      config(displayModeBar = F)
  })

  
  output$Weekly_Mean_QNA_Reply_Plot = renderPlotly({
    pal = fn_change_color(choices = "ALL", type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }

    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "Q&A 댓글 수 평균 그룹별"
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 전체"
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 유사"
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 최고"
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 최저"
    }
    
    g = fn_draw_bar_plot(data = weekly_score, 
                         this_data = long_this_weekly_score, 
                         y = y_val, 
                         pal = pal,
                         my_data = my_data,
                         my_data_y = "Q&A 댓글 수")
    
    ggplotly(g)%>% 
      config(displayModeBar = F)
  })

  output$Weekly_Mean_Team_Post_Plot = renderPlotly({
    pal = fn_change_color(choices = "ALL", type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }

    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "팀플 게시글 수 평균 그룹별"
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 전체"
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 유사"
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 최고"
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 최저"
    }
    
    g = fn_draw_bar_plot(data = weekly_score, 
                         this_data = long_this_weekly_score, 
                         y = y_val, 
                         pal = pal,
                         my_data = my_data,
                         my_data_y = "팀플 게시글 수")

    ggplotly(g)%>% 
      config(displayModeBar = F)
  })


  output$Weekly_Mean_Team_Reply_Plot = renderPlotly({
    pal = fn_change_color(choices = "ALL", type = input$Mode)

    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }
    
    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "팀플 댓글 수 평균 그룹별"
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 전체"
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 유사"
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 최고"
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 최저"
    }
    
    g = fn_draw_bar_plot(data = weekly_score, 
                         this_data = long_this_weekly_score, 
                         y = y_val, 
                         pal = pal,
                         my_data = my_data,
                         my_data_y = "팀플 댓글 수")
    
    ggplotly(g)%>% 
      config(displayModeBar = F)
  })
  
  output$Pop_Up_title = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      title_text = "지난 학기 수강생 학습활동 요약보기"  
    }else if(input$Mode == "현재 학기 수강생"){
      title_text = "현재 학기 수강생 학습활동 요약보기"  
    }
      
    titlePanel(
      div(style = "margin-left: 5%;", 
          h3(p(strong(title_text))))
    )
  })
  
  output$Pop_Up_Description = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      description = "성적 그룹별 추이, 나와 유사한 학습자 추이, 최고점/최저점 학습자 추이는 학기 종료시점의 총점을 기준으로 제공됩니다."  
    }else if(input$Mode == "현재 학기 수강생"){
      description = "나와 유사한 학습자 추이, 최고점/최저점 학습자 추이는 중간고사 성적을 기준으로 제공됩니다."  
    }
    
    div(style = "margin-left: 5%;",
        h5(description))
  })
  
  output$Pop_Up_SelectInput = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      choice_list = c("성적 그룹별 추이 보기", 
                     "전체 학습자 추이 보기", 
                     "나와 유사한 학습자 추이 보기",
                     "최고점 학습자 추이 보기",
                     "최저점 학습자 추이 보기") 
    }else if(input$Mode == "현재 학기 수강생"){
      choice_list = c("전체 학습자 추이 보기", 
                     "나와 유사한 학습자 추이 보기",
                     "최고점 학습자 추이 보기",
                     "최저점 학습자 추이 보기")
    }
    
    fluidRow(
      column(width = 12, align = "center",
             div(
               style = "width: 250px; margin-left: 60%;",
               selectInput( 
                 inputId = "Description_Mode", 
                 label = div(style = "margin-left: 60%;", h4(p(strong("Mode")))),
                 choice = choice_list
               )
             )
      )
    )
  })
  
  output$Pop_Up_Plot = renderUI({
    
    fluidRow(
      column(width = 2, align = "center",
             verticalLayout(
               #
               div(style = "width: 35%; margin-left: 30px; margin-top: 70px; text-align: center;", 
                   h5(p(strong("Q&A")))
               ),
               div(style = "width: 60%; margin-left: 30px; margin-top: -20px; text-align: center;", 
                   h5(p(strong("게시글 수")))
               ),
               
               #
               div(style = "width: 35%; margin-left: 30px; margin-top: 125px; text-align: center;", 
                   h5(p(strong("Q&A")))
               ),
               div(style = "width: 60%; margin-left: 30px; margin-top: -20px; text-align: center;", 
                   h5(p(strong("댓글 수")))
               ),
               
               #
               div(style = "width: 35%; margin-left: 30px; margin-top: 125px; text-align: center;", 
                   h5(p(strong("팀플")))
               ),
               div(style = "width: 60%; margin-left: 30px; margin-top: -20px; text-align: center;", 
                   h5(p(strong("게시글 수")))
               ),
               
               #
               div(style = "width: 35%; margin-left: 30px; margin-top: 125px; text-align: center;", 
                   h5(p(strong("팀플")))
               ),
               div(style = "width: 60%; margin-left: 30px; margin-top: -20px; text-align: center;", 
                   h5(p(strong("댓글 수")))
               )
             )
      ),
      column(width = 10,
             verticalLayout(
               plotlyOutput(outputId = "Weekly_Mean_QNA_Post_Plot", height = "170px"),
               plotlyOutput(outputId = "Weekly_Mean_QNA_Reply_Plot", height = "170px"),
               plotlyOutput(outputId = "Weekly_Mean_Team_Post_Plot", height = "170px"),
               plotlyOutput(outputId = "Weekly_Mean_Team_Reply_Plot", height = "170px")
             )
             # plotlyOutput(outputId = "Weekly_Summary_Plot")
      )
    )    
  })
})
