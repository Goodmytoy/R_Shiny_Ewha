#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# options(encoding = "UTF-8")

# 0. Packages ----------------------------------------------
required_packages = c("readxl", "data.table", "plotly", "ggplot2", "shiny", "htmlwidgets", "shinyBS", "shinyjs")

# 설치되어 있지 않으면 해당 패키지를 설치한다.
for(pkg in required_packages){
  if(!pkg %in% installed.packages()){
    install.packages(pkg, dependencies = TRUE)
  }
}

# 설치가 완료된 이후에 library  실행
library(readxl)
library(data.table)
library(plotly)
library(ggplot2)
library(shiny)
library(htmlwidgets)
library(showtext)
library(shinyBS)
library(shinyjs)



# 1. 경로 설정 ----------------------------------------------
# 데이터 및 functions_script 경로 지정
base_path = "C:/Users/seho1/Documents/R_Shiny_Ewha/Ewha/"
# base_path = "."
source(paste0(base_path,"/functions_script.R"), encoding = "UTF-8")




# 2. 폰트 지정 ----------------------------------------------
font_add_google(name = "Nanum Gothic", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 112)




# 3. 데이터 Loading ----------------------------------------------
# Set My Data(x11, x34, x43)
my_data = "x25"
jitter_value = 0

# Load Data
last_test_score = as.data.table(read_excel(paste0(base_path,"/data/data.xlsx"), sheet = 2))
last_weekly_score = as.data.table(read_excel(paste0(base_path,"/data/data.xlsx"), sheet = 3))
last_total_score = as.data.table(read_excel(paste0(base_path,"/data/data.xlsx"), sheet = 4))
this_test_score = as.data.table(read_excel(paste0(base_path,"/data/data.xlsx"), sheet = 5))
this_weekly_score = as.data.table(read_excel(paste0(base_path,"/data/data.xlsx"), sheet = 6))


# 데이터의 순서를 맞춰준다.
# 추후, Plotly click event 발생 시 point_number가 동일하게 지정될 수 있도록
last_test_score[order(수강생)]
last_weekly_score[order(수강생)]
last_total_score[order(수강생)]
this_test_score[order(수강생)]
this_weekly_score[order(수강생)]







# 4. 데이터 전처리 ----------------------------------------------
## 4.1 This Year Data ----------------------------------------------
## 4.1.1. Data Join ----------------------------------------------
# 성적등급 데이터를 Join하여 등급별 데이터를 추출할 수 있도록 한다.
this_year_score = this_test_score[this_weekly_score, on = c("수강생", "실험집단")]

# 4.1.2. Melt Data ----------------------------------------------
#  --> Longitudinal Data로 변환
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


# 4.1.3. Group by Weekly Data ----------------------------------------------
#전체 학습자
long_this_weekly_score[, `Q&A 게시글 수 평균 전체` := round(mean(`Q&A 게시글 수`),1), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균 전체` := round(mean(`Q&A 댓글 수`),1), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균 전체` := round(mean(`팀플 게시글 수`),1), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균 전체` := round(mean(`Q&A 댓글 수`),1), by = list(week)]

# 나와 유사한 학습자
my_score = this_test_score[수강생 == my_data, 중간점수]
similar_student = this_test_score[중간점수 >= (my_score - (100 * 0.05)) & 중간점수 <= (my_score + (100 * 0.05)), 수강생]
long_this_weekly_score[수강생 %in% similar_student, `Q&A 게시글 수 평균 유사` := round(mean(`Q&A 게시글 수`),1), by = list(week)]
long_this_weekly_score[수강생 %in% similar_student, `Q&A 댓글 수 평균 유사` := round(mean(`Q&A 댓글 수`),1), by = list(week)]
long_this_weekly_score[수강생 %in% similar_student, `팀플 게시글 수 평균 유사` := round(mean(`팀플 게시글 수`),1), by = list(week)]
long_this_weekly_score[수강생 %in% similar_student, `팀플 댓글 수 평균 유사` := round(mean(`팀플 댓글 수`),1), by = list(week)]
# 최고점 학습자
long_this_weekly_score[, `Q&A 게시글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균 최고` := max(`Q&A 댓글 수`), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균 최고` := max(`팀플 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균 최고` := max(`팀플 댓글 수`), by = list(week)]

# 최저점 학습자
long_this_weekly_score[, `Q&A 게시글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_this_weekly_score[, `Q&A 댓글 수 평균 최저` := min(`Q&A 댓글 수`), by = list(week)]
long_this_weekly_score[, `팀플 게시글 수 평균 최저` := min(`팀플 게시글 수`), by = list(week)]
long_this_weekly_score[, `팀플 댓글 수 평균 최저` := min(`팀플 댓글 수`), by = list(week)]



## 4.2 Last Year Data ----------------------------------------------
## 4.2.1. Data Join ----------------------------------------------
# 성적등급 데이터를 Join하여 등급별 데이터를 추출할 수 있도록 한다.
last_test_score = last_test_score[last_total_score[,c("수강생", "실험집단", "성적등급"), with = FALSE], 
                                  on = c("수강생", "실험집단")]

# 주차별 성적데이터와 시험 점수 데이터를 합쳐서 전체 데이터 생성
last_year_score = last_test_score[last_weekly_score, on = c("수강생", "실험집단")][last_total_score, on = c("수강생", "실험집단")]

# 4.2.2. Melt Data ----------------------------------------------
#  --> Longitudinal Data로 변환
# 시험점수 데이터 Melt
long_last_test_score = melt(last_test_score, 
                            id.vars = c("수강생", "실험집단", "성적등급"), 
                            measure.vars = c("중간점수", "기말점수"), 
                            variable.name = "type", 
                            value.name = "score")
# 주차별 전체 성적 데이터 Melt
long_last_weekly_score = melt(last_year_score,
                              id.vars = c("수강생", "실험집단", "성적등급"),
                              measure.vars = patterns("^y[0-9]{1,2}1$", "^y[0-9]{1,2}2$", "^y[0-9]{1,2}3$", "^y[0-9]{1,2}4$"),
                              variable.name = "week",
                              value.name = c("Q&A 게시글 수", "Q&A 댓글 수", "팀플 게시글 수", "팀플 댓글 수"))
# 주차별 Q&A 성적 데이터 Melt
long_last_year_qna = melt(long_last_weekly_score,
                          id.vars = c("수강생", "실험집단", "성적등급", "week"),
                          measure.vars = c("Q&A 게시글 수", "Q&A 댓글 수"),
                          variable.name = "type",
                          value.name = "count")
# 주차별 팀플 성적 데이터 Melt
long_last_year_team = melt(long_last_weekly_score,
                           id.vars = c("수강생", "실험집단", "성적등급", "week"),
                           measure.vars = c("팀플 게시글 수", "팀플 댓글 수"),
                           variable.name = "type",
                           value.name = "count")

# Y축 최대값을 정하기 위해 max_count를 구한다.
max_count = max(c(long_last_year_qna[,count], long_last_year_team[,count], long_this_year_qna[,count], long_this_year_team[,count]))

# 4.2.3. Group by Weekly Data ----------------------------------------------
# 성적 그룹별
long_last_weekly_score[, `Q&A 게시글 수 평균 그룹별` := round(mean(`Q&A 게시글 수`),1), by = list(성적등급, week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 그룹별` := round(mean(`Q&A 댓글 수`),1), by = list(성적등급, week)]
long_last_weekly_score[, `팀플 게시글 수 평균 그룹별` := round(mean(`팀플 게시글 수`),1), by = list(성적등급, week)]
long_last_weekly_score[, `팀플 댓글 수 평균 그룹별` := round(mean(`팀플 댓글 수`),1), by = list(성적등급, week)]

# 전체 학습자
long_last_weekly_score[, `Q&A 게시글 수 평균 전체` := round(mean(`Q&A 게시글 수`),1), by = list(week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 전체` := round(mean(`Q&A 댓글 수`),1), by = list(week)]
long_last_weekly_score[, `팀플 게시글 수 평균 전체` := round(mean(`팀플 게시글 수`),1), by = list(week)]
long_last_weekly_score[, `팀플 댓글 수 평균 전체` := round(mean(`팀플 댓글 수`),1), by = list(week)]

# 나와 유사한 학습자
my_score = this_test_score[수강생 == my_data, 중간점수]
similar_student = last_test_score[중간점수 >= (my_score - (100 * 0.05)) & 중간점수 <= (my_score + (100 * 0.05)), 수강생]
long_last_weekly_score[수강생 %in% similar_student, `Q&A 게시글 수 평균 유사` := round(mean(`Q&A 게시글 수`),1), by = list(week)]
long_last_weekly_score[수강생 %in% similar_student, `Q&A 댓글 수 평균 유사` := round(mean(`Q&A 댓글 수`),1), by = list(week)]
long_last_weekly_score[수강생 %in% similar_student, `팀플 게시글 수 평균 유사` := round(mean(`팀플 게시글 수`),1), by = list(week)]
long_last_weekly_score[수강생 %in% similar_student, `팀플 댓글 수 평균 유사` := round(mean(`팀플 댓글 수`),1), by = list(week)]

# 최고점 학습자
long_last_weekly_score[, `Q&A 게시글 수 평균 최고` := max(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 최고` := max(`Q&A 댓글 수`), by = list(week)]
long_last_weekly_score[, `팀플 게시글 수 평균 최고` := max(`팀플 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 댓글 수 평균 최고` := max(`팀플 댓글 수`), by = list(week)]

# 최저점 학습자
long_last_weekly_score[, `Q&A 게시글 수 평균 최저` := min(`Q&A 게시글 수`), by = list(week)]
long_last_weekly_score[, `Q&A 댓글 수 평균 최저` := min(`Q&A 댓글 수`), by = list(week)]
long_last_weekly_score[, `팀플 게시글 수 평균 최저` := min(`팀플 게시글 수`), by = list(week)]
long_last_weekly_score[, `팀플 댓글 수 평균 최저` := min(`팀플 댓글 수`), by = list(week)]




# 5. Shiny Server ----------------------------------------------
shinyServer(function(input, output) {
  
  # shinyjs::runjs(HTML("
  #   var plot_len = document.getElementsByClassName('scatterlayer').length
  #   
  #   if(plot_len > 6){
  #       plot_len = plot_len - 4
  #   }
  #     var checkboxes = document.getElementsByName('Grade_Compare_Group')
  #   var chk = false
  #   var color_arr = new Array(checkboxes.length); 
  #   color_arr[0] = 'rgb(102,176,226)'
  #   color_arr[1] = 'rgb(255,189,55)'
  #   color_arr[2] = 'rgb(127,108,171)'
  #   color_arr[3] = 'rgb(158,200,110)'
  #   
  #   console.log('checkbox length : ', String(checkboxes.length))
  #   
  #   for(i=0; i<checkboxes.length; i++){
  #   
  #       if(checkboxes[i].checked){
  #           for(j=0; j<plot_len; j++) {
  #               document.getElementsByClassName('scatterlayer')[j].getElementsByClassName('scatter')[i].getElementsByClassName('point')[0].style['fill'] = color_arr[i]
  #           }
  #       }
  #   }
  #                "))
  # 5.1 Main Page ----------------------------------------------
  # 5.0 Mydata Image
  output$My_Data_Img = renderImage({
    list(src = paste0(base_path, "/data/My_Data_Img.png"),
         contentType = "image/png",
         width = 100,
         height = 100)
    
  })
  # 5.1.1. Text Output ----------------------------------------------
  # -->  (Mean, Standard Deviance)
  ## 시험점수 (Test Score)
  # 중간점수 (Mid Test Score)
  output$Mid_Test_Score_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }
    
    test_score_data = test_score_data[type == "중간점수",]  
    score_mean = ifelse(is.nan(round(mean(test_score_data$score),2))," ", round(mean(test_score_data$score),2))
    score_sd = ifelse(is.na(round(sd(test_score_data$score),2))," ", round(sd(test_score_data$score),2))
    
    cat(paste0("<중간점수> \n",
               "평균: ", score_mean, "점\n", 
               "표준편차: ", score_sd))
  })
  
  # 기말점수 (Mid Test Score)
  output$Final_Test_Score_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
    }
    
    test_score_data = test_score_data[type == "기말점수",]
    score_mean = ifelse(is.nan(round(mean(test_score_data$score),2)),"", round(mean(test_score_data$score),2))
    score_sd = ifelse(is.na(round(sd(test_score_data$score),2)),"", round(sd(test_score_data$score),2))
    
    cat(paste0("<기말점수> \n",
               "평균: ", score_mean, "점\n", 
               "표준편차: ", score_sd))
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
    count_mean = ifelse(is.nan(round(mean(online_qna_data$count),2)),"", round(mean(online_qna_data$count),2))
    count_sd = ifelse(is.na(round(sd(online_qna_data$count),2)),"", round(sd(online_qna_data$count),2))
    
    cat(paste0("<Q&A 게시글 수> \n",
               "평균: ", count_mean, "개\n", 
               "표준편차: ", count_sd))
  })
  
  # Q&A 댓글 수 (Q&A Reply)
  output$Online_QNA_Reply_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      online_qna_data = long_last_year_qna
    }else if(input$Mode == "현재 학기 수강생"){
      online_qna_data = long_this_year_qna
    }
    
    online_qna_data = online_qna_data[week %in% as.numeric(input$Select_Week) & type == "Q&A 댓글 수",]
    count_mean = ifelse(is.nan(round(mean(online_qna_data$count),2)),"", round(mean(online_qna_data$count),2))
    count_sd = ifelse(is.na(round(sd(online_qna_data$count),2)),"", round(sd(online_qna_data$count),2))
    
    cat(paste0("<Q&A 댓글 수> \n",
               "평균: ", count_mean, "개\n", 
               "표준편차: ", count_sd))
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
    count_mean = ifelse(is.nan(round(mean(online_team_data$count),2)),"", round(mean(online_team_data$count),2))
    count_sd = ifelse(is.na(round(sd(online_team_data$count),2)),"", round(sd(online_team_data$count),2))
    
    cat(paste0("<팀플 게시글 수> \n",
               "평균: ", count_mean, "개\n", 
               "표준편차: ", count_sd))
  })
  
  # 팀플 댓글 수 (Team Reply)
  output$Online_Team_Reply_Summary = renderPrint({
    if(input$Mode == "지난 학기 수강생"){
      online_team_data = long_last_year_team
    }else if(input$Mode == "현재 학기 수강생"){
      online_team_data = long_this_year_team
    }
    
    online_team_data = online_team_data[week %in% as.numeric(input$Select_Week) & type == "팀플 댓글 수",]
    
    count_mean = ifelse(is.nan(round(mean(online_team_data$count),2)),"", round(mean(online_team_data$count),2))
    count_sd = ifelse(is.na(round(sd(online_team_data$count),2)),"", round(sd(online_team_data$count),2))    
    
    cat(paste0("<팀플 댓글 수> \n",
               "평균: ", count_mean, "개\n", 
               "표준편차: ", count_sd))
  })
  
  
  # 5.1.2. Strip Plot ----------------------------------------------
  output$Test_Score_Plot = renderPlotly({
    # pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)
    pal = rep(rgb_gray , 4)
    if(input$Mode == "지난 학기 수강생"){
      test_score_data = long_last_test_score
    }else if(input$Mode == "현재 학기 수강생"){
      test_score_data = long_this_test_score
      test_score_data = test_score_data[수강생 != my_data,]
      test_score_data = rbind(test_score_data, test_score_data[1,][,`:=` (type = "기말점수", score = NA)])
    }
    
    set.seed(100)
    test_plot = fn_draw_strip_plot(data = test_score_data,
                                   this_data = long_this_test_score,
                                   y = "score",
                                   type_vec = c("중간점수", "기말점수"),
                                   pal = pal,
                                   my_data = my_data, 
                                   y_max = 100,
                                   jitter = jitter_value)
    
    test_plot %>% layout(autosize = T) %>%
      config(displayModeBar = F) %>%
      onRender(click_event_js) %>%
      onRender(click_event_maintain_js) %>%
      onRender(color_change_js)
    
  })
  
  
  
  output$Online_QNA_Plot = renderPlotly({
    # pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)
    pal = rep(rgb_gray , 4)
    select_week = as.numeric(input$Select_Week)
    # select_week = 10
    if(input$Mode == "지난 학기 수강생"){
      online_qna_data = long_last_year_qna
    }else if(input$Mode == "현재 학기 수강생"){
      online_qna_data = long_this_year_qna
      online_qna_data = online_qna_data[수강생 != my_data,]
    }
    set.seed(100)
    input_data = online_qna_data[week %in% select_week]
    if(nrow(online_qna_data[week %in% select_week]) == 0){
      input_data = online_qna_data[week %in% 1][, `:=` (week = rep(select_week, nrow(online_qna_data[week %in% 1])), count = NA)]
    }
    
    qna_plot = fn_draw_strip_plot(data = input_data,
                                  this_data = long_this_year_qna[week %in% select_week],
                                  y = "count",
                                  type_vec = c("Q&A 게시글 수", "Q&A 댓글 수"),
                                  pal = pal,
                                  my_data = my_data,
                                  y_max = max_count,
                                  jitter = jitter_value)
    
    qna_plot %>% layout(autosize = T) %>%
      config(displayModeBar = F) %>%
      onRender(click_event_js) %>%
      onRender(click_event_maintain_js)  %>%
      onRender(color_change_js)
    
  })
  
  
  output$Online_Team_Plot = renderPlotly({
    # pal = fn_change_color(input$Grade_Compare_Group, type = input$Mode)
    pal = rep(rgb_gray , 4)
    select_week = as.numeric(input$Select_Week)
    
    if(input$Mode == "지난 학기 수강생"){
      online_team_data = long_last_year_team
    }else if(input$Mode == "현재 학기 수강생"){
      online_team_data = long_this_year_team
      online_team_data = online_team_data[수강생 != my_data,]
    }
    
    set.seed(100)
    input_data = online_team_data[week %in% select_week]
    if(nrow(online_team_data[week %in% select_week]) == 0){
      input_data = online_team_data[week %in% 1][, `:=` (week = rep(select_week, nrow(online_team_data[week %in% 1])), count = NA)]
    }
    
    team_plot = fn_draw_strip_plot(data = input_data,
                                   this_data = long_this_year_team[week %in% select_week],
                                   y = "count",
                                   type_vec = c("팀플 게시글 수", "팀플 댓글 수"),
                                   pal = pal,
                                   my_data = my_data,
                                   y_max = max_count,
                                   jitter = jitter_value,
                                   browser = F)
    
    team_plot %>% layout(autosize = T) %>%
      config(displayModeBar = F) %>%
      onRender(click_event_js) %>%
      onRender(click_event_maintain_js)  %>%
      onRender(color_change_js)
    
  })
  
  observe({
    shinyjs::runjs(cursor_disable_js)
  })
  
  #
  observe({
    if(input$Mode == "지난 학기 수강생"){
      if('A' %in% input$Grade_Compare_Group){
        shinyjs::runjs(fn_change_color_js("rgb(102,176,226)", 0))
      } else if(!'A' %in% input$Grade_Compare_Group) {
        shinyjs::runjs(fn_change_color_js("rgb(192,192,192)", 0))
      }
      
      if('B' %in% input$Grade_Compare_Group){
        shinyjs::runjs(fn_change_color_js("rgb(255,189,55)", 1))
      } else if(!'B' %in% input$Grade_Compare_Group) {
        shinyjs::runjs(fn_change_color_js("rgb(192,192,192)", 1))
      }
      
      if('C' %in% input$Grade_Compare_Group){
        shinyjs::runjs(fn_change_color_js("rgb(127,108,171)", 2))
      } else if(!'C' %in% input$Grade_Compare_Group) {
        shinyjs::runjs(fn_change_color_js("rgb(192,192,192)", 2))
      }
      
      if('D' %in% input$Grade_Compare_Group){
        shinyjs::runjs(fn_change_color_js("rgb(158,200,110)", 3))
      } else if(!'D' %in% input$Grade_Compare_Group) {
        shinyjs::runjs(fn_change_color_js("rgb(192,192,192)", 3))
      }
    }
  })
  
  
  
  # 5.1.3. Slider Input UI ----------------------------------------------
  output$max_week_slider_input = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      max_week = max(as.numeric(long_last_year_qna$week))
    }else if(input$Mode == "현재 학기 수강생"){
      max_week = max(as.numeric(long_this_year_qna$week))
    }
    
    sliderInput(
      inputId = "Select_Week", 
      # label = h4(p(strong("주차 선택"))),
      label = NULL,
      step = 1,
      min = 1,
      # max = max_week,
      max = 15,
      value = 1,
      post = "주차",
      width = "100%"
    )
  })
  
  # 5.1.4. Checkbox Input UI ----------------------------------------------
  output$group_checkbox_input = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      fixedRow(
        column(width = 2, offset = 1, align = "left",
               div(style = "display:inline-block; padding-top: 12px; margin-left: 0px;", 
                   h4(p(strong("성적 비교 집단")))
               )     
        ),
        column(width = 5, offset = 0, align = "left",
               br(),
               div(style = "display:inline-block; margin-left: 0px;",
                   checkboxGroupInput(
                     inputId = "Grade_Compare_Group", 
                     label = NULL,
                     choiceNames = list(
                       tags$span("A", style = "color: rgb(102,176,226); font-weight: bold; font-size: large; margin-right : 50%;"),
                       tags$span("B", style = "color: rgb(255,189,55); font-weight: bold; font-size: large; margin-right : 50%;"),
                       tags$span("C", style = "color: rgb(127,108,171); font-weight: bold; font-size: large; margin-right : 50%;"),
                       tags$span("D이하", style = "color: rgb(158,200,110); font-weight: bold; font-size: large; margin-right : 20px;")
                     ),
                     choiceValues = c("A", "B", "C", "D"),
                     inline = TRUE
                   )   
               )
        )  
      )
    } else {
      br()
      br()
    }
  })
  
  
  # 5.2. Pop-Up Page ----------------------------------------------
  # 5.2.1. Line Plot ----------------------------------------------
  # Q&A 게시글 수
  output$Weekly_Mean_QNA_Post_Plot = renderPlotly({
    
    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }
    
    
    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "Q&A 게시글 수 평균 그룹별"
      pal = fn_change_color(choices = "ALL", type = input$Mode)
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 전체"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 유사"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 최고"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "Q&A 게시글 수 평균 최저"
      pal = rep(rgb_green, 4)
    }
    
    g = fn_draw_line_plot(data = weekly_score, 
                          this_data = long_this_weekly_score, 
                          y = y_val, 
                          pal = pal,
                          legend = TRUE,
                          x_axis = FALSE,
                          my_data = my_data,
                          my_data_y = "Q&A 게시글 수",
                          browser = F)
    
    
    ggplotly(g)%>% 
      config(displayModeBar = F) %>%
      layout(showlegend = TRUE, 
             legend = list(orientation = "h", y = 1.4),
             yaxis = list(hoverformat = ".1f")) %>%
      onRender(legend_disable_js)
  })
  
  # Q&A 댓글 수
  output$Weekly_Mean_QNA_Reply_Plot = renderPlotly({
    
    
    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }
    
    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "Q&A 댓글 수 평균 그룹별"
      pal = fn_change_color(choices = "ALL", type = input$Mode)
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 전체"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 유사"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 최고"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "Q&A 댓글 수 평균 최저"
      pal = rep(rgb_green, 4)
    }
    
    g = fn_draw_line_plot(data = weekly_score, 
                          this_data = long_this_weekly_score, 
                          y = y_val, 
                          pal = pal,
                          legend = FALSE,
                          x_axis = FALSE,
                          my_data = my_data,
                          my_data_y = "Q&A 댓글 수",
                          browser = F)
    
    ggplotly(g)%>% 
      config(displayModeBar = F)
  })
  
  # 팀플 게시글 수
  output$Weekly_Mean_Team_Post_Plot = renderPlotly({
    pal = fn_change_color(choices = "ALL", type = input$Mode)
    
    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }
    
    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "팀플 게시글 수 평균 그룹별"
      pal = fn_change_color(choices = "ALL", type = input$Mode)
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 전체"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 유사"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 최고"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "팀플 게시글 수 평균 최저"
      pal = rep(rgb_green, 4)
    }
    
    g = fn_draw_line_plot(data = weekly_score, 
                          this_data = long_this_weekly_score, 
                          y = y_val, 
                          pal = pal,
                          legend = FALSE,
                          x_axis = FALSE,
                          my_data = my_data,
                          my_data_y = "팀플 게시글 수")
    
    ggplotly(g)%>% 
      config(displayModeBar = F)
  })
  
  # 팀플 댓글 수
  output$Weekly_Mean_Team_Reply_Plot = renderPlotly({
    pal = fn_change_color(choices = "ALL", type = input$Mode)
    
    if(input$Mode == "지난 학기 수강생"){
      weekly_score = long_last_weekly_score
    }else if(input$Mode == "현재 학기 수강생"){
      weekly_score = long_this_weekly_score
    }
    
    if(input$Description_Mode == "성적 그룹별 추이 보기"){
      y_val = "팀플 댓글 수 평균 그룹별"
      pal = fn_change_color(choices = "ALL", type = input$Mode)
    }else if(input$Description_Mode == "전체 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 전체"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "나와 유사한 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 유사"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최고점 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 최고"
      pal = rep(rgb_green, 4)
    }else if(input$Description_Mode == "최저점 학습자 추이 보기"){
      y_val = "팀플 댓글 수 평균 최저"
      pal = rep(rgb_green, 4)
    }
    
    g = fn_draw_line_plot(data = weekly_score, 
                          this_data = long_this_weekly_score, 
                          y = y_val, 
                          pal = pal,
                          legend = FALSE,
                          x_axis = TRUE,
                          my_data = my_data,
                          my_data_y = "팀플 댓글 수")
    
    ggplotly(g)%>% 
      config(displayModeBar = F)
  })
  
  
  # 5.2.2. Pop-Up UI ----------------------------------------------
  # Pop-Up Title
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
  
  # Pop-Up 설명
  output$Pop_Up_Description = renderUI({
    if(input$Mode == "지난 학기 수강생"){
      description = "성적 그룹별 추이, 나와 유사한 학습자 추이, 최고점/최저점 학습자 추이는 학기 종료시점의 총점을 기준으로 제공됩니다."  
    }else if(input$Mode == "현재 학기 수강생"){
      description = "나와 유사한 학습자 추이, 최고점/최저점 학습자 추이는 중간고사 성적을 기준으로 제공됩니다."  
    }
    
    div(style = "margin-left: 5%;",
        h5(description))
  })
  
  # Pop-Up SelectInput
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
      column(width = 2, offset = 7, align = "center",
             div(style = "display:inline-block; padding-top: 18px; margin-left: -20%;", 
                 h4(p(strong("Mode : ")))
             )       
      ),
      column(width = 3, align = "center",
             br(),
             div(
               style = "width: 250px; margin-left: -40%;",
               selectInput( 
                 inputId = "Description_Mode", 
                 label = NULL,
                 choice = choice_list
               )
             )
      )
    )
  })
  
  # Pop-Up Plot
  output$Pop_Up_Plot = renderUI({
    
    fluidRow(
      column(width = 2, align = "center",
             verticalLayout(
               #
               div(style = "width: 35%; margin-left: 30px; margin-top: 85px; text-align: center;", 
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
               div(style = "margin-top:0px;", plotlyOutput(outputId = "Weekly_Mean_QNA_Post_Plot", height = "190px")),
               div(style = "margin-top:-5px;", plotlyOutput(outputId = "Weekly_Mean_QNA_Reply_Plot", height = "170px")),
               div(style = "margin-top:-5px;", plotlyOutput(outputId = "Weekly_Mean_Team_Post_Plot", height = "170px")),
               div(style = "margin-top:-5px;", plotlyOutput(outputId = "Weekly_Mean_Team_Reply_Plot", height = "170px"))
             )
      )
    )    
  })
})