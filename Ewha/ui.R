#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# options(encoding = "UTF-8")

# 0. Packages ----------------------------------------------
library(data.table)
library(plotly)
library(ggplot2)
library(shiny)
library(htmlwidgets)
library(showtext)
library(shinyBS)
library(shinyjs)

# 1. 경로 설정 ----------------------------------------------
font_add_google(name = "Nanum Gothic", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 112)


# 2. Shiny UI ----------------------------------------------
shinyUI(fixedPage(
  # Javascript 코드를 실행하여 UI에 적용한다.
  shinyjs::useShinyjs(),

  # 2.1 Application title ----------------------------------------------
  titlePanel(h1(p(strong("Find me Find you")), align = "center")),
  
  # 2.2 Description ----------------------------------------------
  fixedRow(column(width = 10, offset = 1,
           br(),
           p("본 화면은 나와 다른 학습자들이 어떻게 학습하는지를 확인할 수 있는 화면입니다."),
           p("나의 활동 및 성적을 지난학기 수강생들의 활동 및 성적, 또는 이번학기 수강생들의 활동 및 성적과 비교해 볼 수 있습니다. "),
           p("특정한 수강생의 활동 및 성적을 클릭하거나, 성적 비교집단을 클릭하여 나의 활동 및 성적과 비교해보기 바랍니다."),
           p("특정한 주차에 대해 자세한 정보를 얻고자 할 경우, 하단의 슬라이더를 이동해 보십시오."),
           p("15주차 전체의 정보를 얻고자 할 경우, 슬라이더 우측의 '요약보기'를 클릭하면 됩니다."),
          p("나의 활동 및 성적은", span(strong("[빨간색 마름모(◆)]"), style = "color:red"), "로 표시됩니다.")
           
  )),
  
  
  # 2.3 SelectInput (학기 선택) ----------------------------------------------
  fixedRow(
        column(width = 1, offset = 1, align = "left",
          div(style = "display:inline-block; padding-top: 18px; margin-left: 0px;", 
              h4(p(strong("Mode")))
          )       
        ),
        column(width = 2, offset = 1, align = "center",
          br(),
          div(
            style = "height: 30px; width: 170px; margin-left: 0px;",
            selectInput(
              inputId = "Mode",
              label = NULL,
              choice = c("지난 학기 수강생", "현재 학기 수강생"),
              selected = "지난 학기 수강생"
            ),
          )
        )
  ),
  
  # 2.4 group_checkbox_input (성적 그룹 선택) ----------------------------------------------
  uiOutput(outputId = "group_checkbox_input"),
  
  # 2.5 시험 점수 Well Panel ----------------------------------------------
  # Plot & 평균/표준편차
  fixedRow(height = 300,
    column(width = 3, offset = 1,
      tags$style(type = "text/css", "#Mid_Test_Score_Summary { font-size: 10.7px; }
                                     #Final_Test_Score_Summary { font-size: 10.7px; }"),
      wellPanel(
        verticalLayout(
          plotlyOutput(outputId = "Test_Score_Plot"),
          splitLayout(
            div(style = "text-align: center;", verbatimTextOutput(outputId = "Mid_Test_Score_Summary")),
            div(style = "text-align: center;", verbatimTextOutput(outputId = "Final_Test_Score_Summary"))
          )
        )
      )
    ),
    
    # 2.6 온라인 주차별 성적 Well Panel ----------------------------------------------
    column(width = 7, offset = 0, 
      tags$style(type = "text/css", "#Online_QNA_Post_Summary { font-size: 10.7px; }
                                     #Online_QNA_Reply_Summary { font-size: 10.7px; }
                                     #Online_Team_Post_Summary { font-size: 10.7px; }
                                     #Online_Team_Reply_Summary { font-size: 10.7px; }"),
      

      wellPanel(
        fluidRow(
          # Online_QNA_Plot
          # Plot & 평균/표준편차
          column(width = 5, offset = 1,
            verticalLayout(
              plotlyOutput(outputId = "Online_QNA_Plot"),
              splitLayout(
                div(style = "text-align: center;", verbatimTextOutput(outputId = "Online_QNA_Post_Summary")),
                div(style = "text-align: center;", verbatimTextOutput(outputId = "Online_QNA_Reply_Summary"))
              )
            ),
          ),
          # Online_Team_Plot
          # Plot & 평균/표준편차
          column(width = 5,
            verticalLayout(
              plotlyOutput(outputId = "Online_Team_Plot"),
              splitLayout(
                div(style = "text-align: center;", verbatimTextOutput(outputId = "Online_Team_Post_Summary")),
                div(style = "text-align: center;", verbatimTextOutput(outputId = "Online_Team_Reply_Summary"))            
              )
            )          
          )
        ),
        
        # 2.7 SliderInput (주차 선택) ----------------------------------------------
        fixedRow(
          column(width = 2, align = "center",
            div(style = "display:inline-block; padding-top: 30px; margin-left : -10px",
                h4(p(strong("주차 선택")))
            )
          ),
          column(width = 8, align = "center",
            tags$style(type = "text/css", ".irs-grid-text { font-size: 12px; }
                                           .irs-min { font-size: 12px; }
                                           .irs-max { font-size: 12px; }
                                           .irs-from { font-size: 12px; }
                                           .irs-to { font-size: 12px; }
                                           .irs-single { font-size: 12px; }"),
            br(),
            uiOutput("max_week_slider_input")

          ),
          
          # 2.7 actionButton ( Pop Up ) ----------------------------------------------
          column(width = 1, offset = 1, align = "center",
            div(style="display:inline-block; height: 50%; width:32%; padding-top: 30px; margin-left : -150px",
               actionButton(
                 inputId = "Pop-Up",
                 label = "요약 보기"
               )
            )
          )
        )
      )
    )
  ),
  # 2.8 bsModal - Pop-Up ----------------------------------------------
  mainPanel(
    bsModal(
      id = "Description", 
      title = h2(p(strong("요약보기"))), 
      trigger = "Pop-Up",
      size = "large",
      uiOutput(outputId = "Pop_Up_title"),
      # br(),
      uiOutput(outputId = "Pop_Up_Description"),
      uiOutput(outputId = "Pop_Up_SelectInput"),
      uiOutput(outputId = "Pop_Up_Plot"),
      tags$head(tags$style("#Description .modal-footer{ display:none}"))
    )
  )       
))
