library(plotly)
library(shiny)
library(readxl)
library(tibble)
last_score = read_excel("data.xlsx", sheet = 2)
last_score



install.packages("readxl")
install.packages("reshape")
install.packages("data.table")
install.packages("plotly")
install.packages("ggplot2")
library(readxl)
library(reshape)
library(data.table)
library(plotly)
library(ggplot2)

library(plotly)
library(shiny)
library(readxl)
library(tibble)

last_score = read_excel("C:/Users/seho1/Documents/Ewha/R_Shiny_Ewha/data.xlsx", sheet = 2)
last_score
melted_last_score = melt(last_score, id.vars = c("수강생", "실험집단"), measure.vars = c("중간점수", "기말점수"), variable.name = "type", value.name = "score")
# melted_last_score["type"] = ifelse(melted_last_score["type"] == "중간점수", "middle", "final")
melted_last_score

ggplotly(ggplot(melted_last_score, aes(x = type, y = score)) + 
  geom_jitter(position = position_jitter(0.1), cex = 1.3) +
  labs(title="시험점수",x="", y = "점수"))

group_A = c(102,176,226)/255
group_B = c(102,176,226)/255
group_C = c(102,176,226)/255
group_D = c(102,176,226)/255
pal <- c(rgb())

plot_ly(data = melted_last_score, x = ~type, y = ~score, color = ~실험집단, type = 'scatter')

        