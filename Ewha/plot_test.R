# 필요한 패키지만 설치
required_packages = c("readxl", "data.table", "plotly", "ggplot2")

for(pkg in required_packages){
  if(!pkg %in% required_packages){
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readxl)
library(data.table)
library(plotly)
library(ggplot2)


# 1. Load Data
last_test_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 2))
last_weekly_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 3))
last_total_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 4))
this_test_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 5))
this_weekly_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 6))
this_total_score = as.data.table(read_excel("C:/Users/seho1/Documents/R_Shiny_Ewha/data.xlsx", sheet = 7))

# 2. Merge Data
last_year_score = last_test_score[last_weekly_score, on = c("수강생", "실험집단")][last_total_score,on = c("수강생", "실험집단")]
this_year_score = this_test_score[this_weekly_score, on = c("수강생", "실험집단")]

# 3. Plotting

melted_last_test_score = melt(last_test_score, 
                         id.vars = c("수강생", "실험집단"), 
                         measure.vars = c("중간점수", "기말점수"), 
                         variable.name = "type", 
                         value.name = "score")

ggplotly(ggplot(melted_last_score, aes(x = type, y = score)) + 
           geom_jitter(position = position_jitter(0.1), cex = 1.3) +
           labs(title="시험점수",x="", y = "점수"))

rgb_group_A = "rgb(102,176,226)"
rgb_group_B = "rgb(255,189,55)"
rgb_group_C = "rgb(127,108,171)"
rgb_group_D = "rgb(158,200,110)"
pal <- c(rgb_group_A, rgb_group_B, rgb_group_C, rgb_group_D)

plot_ly(data = melted_last_test_score, 
        x = ~type, 
        y = ~score, 
        color = ~실험집단, 
        colors = pal,
        type = 'scatter',
        mode = 'markers'
)


