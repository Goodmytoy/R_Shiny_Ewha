#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)
library(shinyBS)
library(showtext)


# configure font
font_add_google(name = "Nanum Gothic", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 112)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel(h1(p(strong("Find me Find you")), align = "center")),
  fluidRow(column(width = 12,
    mainPanel(
      h6("본 화면은 나와 다른 학습자들이 어떻게 학습하는지를 확인할 수 있는 화면입니다"),
      #br(),
      h6("먼저 [모드(Mode)]를 선택합니다."),
      h6("‘지난 학기 수강생’ 모드를 선택한 경우에는 지난 학기에 이 과목을 수강했던 학습자들의 정보와 나의 정보를 비교하게 됩니다."), 
      h6("‘현재 학기 수강생’ 모드를 선택한 경우에는 이번 학기에 이 과목을 나와 함께 수강하고 있는 학습자들의 정보와 나의 정보를 비교하게 됩니다."),
      #br(),
      h6("다음으로, 화면 상단에서 [성적 비교 집단]을 선택할 수 있습니다. 궁금한 성적대를 체크하면 해당 성적을 받았던 학습자들의 정보가 다른 색으로 변합니다. 중복체크도 가능하며, 체크해제도 가능합니다."),
      h6("단, 이 기능은 ‘지난 학기 수강생’ 모드에서만 사용할 수 있으며, ‘현재 학기 수강생’ 모드에서는 사용할 수 없습니다."),
      #br(),
      h6("화면 왼쪽에는 [시험 점수]가 표시됩니다. 화면 오른쪽에는 [온라인 학습참여] 에 대한 정보가 표시됩니다. [온라인 학습참여]의 경우 하단의 ‘주차 선택 슬라이더’를 움직이면 해당 주차의 학습활동을 확인할 수 있습니다."),
      #br(),
      h6("예를 들어, [성적 비교 집단] A와 8주차를 선택했다면 오른쪽 [온라인 학습참여] 박스에서는 A를 받은 학습자들이 8주차에 어떤 학습 활동을 했는지 확인할 수 있습니다. 이때 왼쪽의 [시험 점수] 는 주차별로 나오지 않고, A를 받은 학습자들의 중간고사와 기말고사 점수만 확인할 수 있습니다."),
      #br(),
      h6("만약 특정 학습자의 전체 학습 활동과 성적이 궁금하다면 각 축에서 해당 점(즉, 학습자)을 클릭해 보십시오. 그러면 그 학습자의 학습 활동이나 성적 정보가 모든 축에서 큰 점으로 나타날 것입니다."),
      # br(),
      h6("* 참고: 학습 기간 전체의 추이를 확인하려면 슬라이더 옆의 [요약 보기]를 클릭하십시오.")
    )
  )),
  
  
  fluidRow(
    # column(width = 8,
    #   fluidRow(
        column(width = 2, offset = 1,
          tags$style(type = "text/css", ".selectize-label { font-size : large }"),
          selectInput(
            inputId = "Mode",
            label = h4(p(strong("Mode"))),
            choice = c("지난 학기 수강생", "현재 학기 수강생"),
            selected = "지난 학기 수강생"
          )
        ),
        column(width = 4, offset = 0, 
          align = "center",
          checkboxGroupInput(
            inputId = "Grade_Compare_Group", 
            label = h4(p(strong("성적 비교 집단"))), 
            # choices = c("A", "B", "C", "D이하"),
            choiceNames = list(
              tags$span("A", style = "color: rgb(102,176,226); font-weight: bold; font-size: large; margin-right : 10px;"),
              tags$span("B", style = "color: rgb(255,189,55); font-weight: bold; font-size: large; margin-right : 10px;"),
              tags$span("C", style = "color: rgb(127,108,171); font-weight: bold; font-size: large; margin-right : 10px;"),
              tags$span("D", style = "color: rgb(158,200,110); font-weight: bold; font-size: large; margin-right : 10px;")
            ),
            choiceValues = c("A", "B", "C", "D"),
            # colors = c("orange", "blue", "purple", "green"),
            inline = TRUE
          )
        )  
      # )
    # )
  ),
  fluidRow(height = 300,
    column(width = 2, offset = 1,
      wellPanel(        
        plotlyOutput(outputId = "Test_Score_Plot1")
        # style = "padding-bottom: 300px"
      )
    )
    ,
    column(width = 4, offset = 0,
      wellPanel(
        verticalLayout(
          splitLayout(
            plotOutput(outputId = "Test_Score_Plot2"),
            plotOutput(outputId = "Test_Score_Plot3")
          ),
          sliderInput(
            inputId = "Select_Week", 
            label = "주차 선택",
            min = 0,
            max = 16,
            value = 1,
            post = "주차")
        ),
        actionButton(
          inputId = "Pop-Up",
          label = "요약 보기"
          )
      ),
      mainPanel(
        bsModal(
          id = "Description", 
          title = h2(p(strong("요약보기"))), 
          trigger = "Pop-Up",
          size = "large",
          titlePanel(h3(p(strong("지난 학기 수강생 학습활동 요약보기")))),
          h5("성적 그룹별 추이, 나와 유사한 학습자 추이, 최고점/최저점 학습자 추이는 학기 종료시점의 총점을 기준으로 제공됩니다."),
          fluidRow(
            column(width = 12,
              selectInput( 
                inputId = "Description_Mode", 
                label = h4(p(strong("Mode"))),
                choice = c("성적 그룹별 추이 보기", 
                          "전체 학습자 추이 보기", 
                          "나와 유사한 학습자 추이 보기",
                          "최고점 학습자 추이 보기",
                          "최저점 학습자 추이 보기")
              )
            )
          )
          ,
          verticalLayout(
            plotOutput("Test_Score_Plot5"),
            plotOutput("Test_Score_Plot6")
          )
        )
      )       
    )
  )

))
