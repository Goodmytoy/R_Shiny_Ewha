# options(encoding = "UTF-8")

library(data.table)
library(plotly)
library(ggplot2)
library(shiny)
library(htmlwidgets)
library(showtext)
library(shinyBS)


options(shiny.usecairo = FALSE)
# configure font
font_add_google(name = "Nanum Gothic", regular.wt = 400, bold.wt = 700)
# font_add_google("Nanum Gothic", "nanumgothic")
showtext_auto()
# showtext_opts(dpi = 112)
# default setting
rgb_group_A = rgb(102,176,226, maxColorValue = 255)
rgb_group_B = rgb(255,189,55, maxColorValue = 255)
rgb_group_C = rgb(127,108,171, maxColorValue = 255)
rgb_group_D = rgb(158,200,110, maxColorValue = 255)
rgb_gray = rgb(192, 192, 192, maxColorValue = 255)
rgb_green = rgb(41, 76, 43, maxColorValue = 255)
rgb_red = rgb(255, 0, 0, maxColorValue = 255)


fn_change_color = function(choices, type = "지난 학기 수강생"){
  # Description:
  #   Plotting을 위한 Pallete vector를 만드는 함수
  #
  # Args:
  #   choices: ("A", "B", "C", "D") => 등급 선택에 따라 다른 색의 Pallete를 주도록 되어 있음
  #   type: ("지난 학기 수강생", "현재 학기 수강생") 
  #         => 지난 학기 수강생의 경우 Group에 따라 색상을 다르게 적용
  #         => 현재 학기 수강생의 경우 전부 Gray 색상으로 적용
  #
  # Returns:
  #   pallete vector
  
  # 기본적으로 Gray 색상으로 길이 4의 벡터를 생성한다.
  # 현재 학기 데이터의 경우 이 벡터가 그대로 적용된다.
  pal = rep(rgb_gray, 4)
  
  # 지난 학기 수강생으로 선택된 경우에는
  # [성적 비교 집단] Checkbox에 따라서 색상이 다르게 적용될 수 있도록 한다.
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


fn_draw_line_plot = function(data, 
                             this_data, 
                             y, 
                             pal, 
                             legend = TRUE, 
                             my_data = NA, 
                             my_data_y = NA, 
                             browser = FALSE){
  # Description:
  #   line plot를 그리는 함수
  #
  # Args:
  #   data: plot을 그리는데 사용되는 데이터
  #   this_data: 나의 점수를 표시하기 위해 사용되는 현재 학기 데이터
  #   y: 활용할 Y 변수 이름
  #   pal: 색상 팔레트
  #   my_data: 나의 이름(수강생 이름)
  #            ex) x11, x14       
  #   my_data_y: 나의 점수를 표시할 때 사용할 Y 변수 이름
  #
  #
  # Returns:
  #   Plotly 형태의 그래프 object 반환
  
  
  
  # if(browser == TRUE){
  #   browser()
  # }
  # 해당 데이터가 가지고 있는 max week을 구한다.
  max_week = max(as.integer(data[,week]))
  # 성적등급별, 주차별 Unique한 데이터만 사용한다.
  # (여러 학생이 있는 경우 그 중 첫번쨰 학생만 적용)
  temp_data = unique(data, by = c("성적등급", "week"))
  if(grepl("그룹별", y) == FALSE){
    temp_data[,성적등급 := strsplit(y, " ")[[1]][length(strsplit(y, " ")[[1]])]]
    temp_data = unique(temp_data, by = c(y, "성적등급", "week"))
    # temp_data[, 성적등급 := afactor(temp_data$성적등급, levels = c(y, "나의 점수"))]
    pal = c("red", pal[1])
  }
  pal = c(pal, "red")
  
  # Plotting (ggplot)
  g = ggplot(temp_data, aes_string(x = "week", y = paste0("`", y, "`"), group = "수강생", color = "성적등급")) +
    scale_colour_manual(values = pal) +
    geom_line() +
    geom_point() + 
    scale_x_discrete(labels = paste0(rep(1:max_week), "주차")) +
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.text = element_text(face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.grid.major = element_line(color = "gray"),
          panel.grid.minor = element_line(color = "gray"),
          panel.background = element_blank())
  
  
  # my_data가 NA가 아닌 경우에 빨간색으로 나의 점수를 표시한다.
  this_data[,성적등급:= "나의 점수"]
  if(is.na(my_data) == FALSE){
    g = g + geom_line(data = this_data[수강생 == my_data,], aes_string(y = paste0("`", my_data_y, "`"), color = "성적등급")) +
      geom_point(data = this_data[수강생 == my_data,], aes_string(y = paste0("`", my_data_y, "`"), color = "성적등급"))
  }
  
  if(legend == FALSE){
    g = g + theme(legend.position = "none")
  }
  
  
  return(ggplotly(g, tooltip = c("week", y)))
}



fn_draw_strip_plot = function(data, this_data, y, type_vec, pal, my_data = NA, jitter = 0.1, browser = FALSE){
  
  # Description:
  #   strip plot를 그리는 함수
  #
  # Args:
  #   data: plot을 그리는데 사용되는 데이터
  #   this_data: 나의 점수를 표시하기 위해 사용되는 현재 학기 데이터
  #   y: 활용할 Y 변수 이름
  #   type_vec: 적용할 type들을 벡터 형태로 전달 (길이는 2)
  #             ex) c("중간점수", "기말점수"), c("Q&A 게시글 수", "Q&A 댓글 수"), c("팀플 게시글 수", "팀플 댓글 수")
  #   pal: 색상 팔레트
  #   my_data: 나의 이름(수강생 이름)
  #            ex) x11, x14       
  #
  #
  # Returns:
  #   Plotly 형태의 그래프 object 반환
  
  # if(browser == TRUE){
  #   browser()
  # }
  # 2개의 빈 리스트를 생성(type 길이에 맞추지만 여기서는 길이가 2로 제한되어 있으므로)
  plot_list = vector("list", length = 2)
  
  for(i in 1:length(type_vec)){
    # 해당 type에 맞는 데이터만 추출하여 temp_data로 저장
    temp_data = data[type == type_vec[i]]
    # 해당 type에 맞는 데이터가 없는 경우에는 Plot을 그리지 않음
    # -> 현재 학기 데이터에서 기말고사 점수를 그리는 경우
    if(nrow(temp_data) == 0){
      next
    }
    # Plot Title 지정
    if(grepl("점수", type_vec[i])){
      title = "시험 점수"
      source_text = "Test_Score_Plot"
    }else if(grepl("Q&A", type_vec[i])){
      title = "온라인 Q&A 참여"
      source_text = "Online_QNA_Plot"
    }else if(grepl("팀플", type_vec[i])){
      title = "온라인 토론 참여"
      source_text = "Online_Team_Plot"
    }
    
    pal = c(pal, rgb_red)
    # Plotting (ggplot)
    plot_list[[i]] = ggplot(temp_data, aes_string(x = "type", y = paste0("`", y, "`"), color = "성적등급")) + 
      scale_colour_manual(values = pal) +
      geom_jitter(position = position_jitter(jitter), cex = 1.3) +
      labs(title = title, x = "구분", y = "점수") +
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
    
    # 시험 점수를 그리는 경우에는 Y 범위를 0부터 100으로 제한
    if(y == "score"){
      plot_list[[i]] = plot_list[[i]] + ylim(-1, 101)
    } else {
      
      plot_list[[i]] = plot_list[[i]] + ylim(min(temp_data[, y, with = FALSE]) - 1, max(temp_data[,y, with = FALSE]) + 1)
    }
    
    
    
    # my_data가 NA가 아닌 경우에 빨간색으로 나의 점수를 표시
    if(is.na(my_data) == FALSE){
      # 해당 type이 데이터에 존재할 때만 표시
      if(type_vec[i] %in% this_data[,type]){
        plot_list[[i]] = plot_list[[i]] + geom_point(data = this_data[type == type_vec[i] & 수강생 == my_data,], 
                                                     position = position_jitter(jitter), 
                                                     cex = 3, 
                                                     shape = 18,
                                                     color = "red")   
      }
    } 
  }
  
  if(sum(sapply(plot_list, is.null)) == 0){
    g1 = ggplotly(plot_list[[1]], tooltip = c("x", "y", "color"))
    g2 = ggplotly(plot_list[[2]], tooltip = c("x", "y", "color"))
    result_plot = subplot(g1, g2, shareY = TRUE)
  }else{
    result_plot = ggplotly(plot_list[[1]], tooltip = c("x", "y"))
  }
  
  return(result_plot)
}


# Javascript Functions


legend_disable_js = "function(el, x){
                el.on('plotly_legendclick', function() { return false; })
             }"

# 
click_event_js = "
function(el, x){
    el.on('plotly_click', function(data) {
        var plot_len = document.getElementsByClassName('scatterlayer').length
        if(plot_len > 6){
            plot_len = plot_len - 4
        }

        var clicked_plot_curve_len = el.getElementsByClassName('scatter').length;
        var point_arr = new Array(plot_len);
        var old_point_arr = new Array(plot_len);
        var plotly_div_arr = new Array(plot_len);
        var curve_num_arr = new Array(plot_len);
        
        var old_curve_num = data.points[0].curveNumber;
        var curve_num = data.points[0].curveNumber;
        var point_num = data.points[0].pointNumber;
        
        console.log('data: ', String(data));
        console.log('curve_num: ', String(curve_num));
        console.log('point_num: ', String(point_num));

        if(curve_num >= Math.ceil(clicked_plot_curve_len / 2)){
            curve_num = curve_num - Math.ceil(clicked_plot_curve_len / 2);
        }
        
        if(document.getElementsByClassName('scatter')[curve_num].getElementsByClassName('point').length == 1){
            return;
        }
          
          console.log('curve_num: ', String(curve_num));
        
        for(i=0; i<plot_len; i++) {
            console.log('curve_num: ', String(curve_num));
            point_arr[i] = document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('scatter')[curve_num].getElementsByClassName('point')[point_num];
            plotly_div_arr[i] =  document.getElementsByClassName('plotly')[i];
        }
        
        for(i=0; i<plot_len; i++) {
            if (plotly_div_arr[i].backup !== undefined) {
                if ( plotly_div_arr[i].backup.curveNumber < document.getElementsByClassName('scatterlayer')[0].getElementsByClassName('scatter').length){
                    console.log('plotly_div_arr[', i, '].backup curveNumber : ', plotly_div_arr[i].backup.curveNumber);
                    console.log('plotly_div_arr[', i, '].backup point_num : ', plotly_div_arr[i].backup.pointNumber);    

                    old_point_arr[0] = document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('scatter')[plotly_div_arr[i].backup.curveNumber].getElementsByClassName('point')[plotly_div_arr[i].backup.pointNumber]
                    if (old_point_arr[0] !== undefined) {
                        old_point_arr[0].setAttribute('d', plotly_div_arr[i].backup.d);
                    } 
                }
            } 
        }
        
        for(i=0; i<plot_len; i++) {
            plotly_div_arr[i].backup = {curveNumber: curve_num,
                                        pointNumber: point_num,
                                        d: point_arr[i].attributes['d'].value,
                                        style: point_arr[i].attributes['style'].value
                                        };
        }
        for(i=0; i<plot_len; i++) {
            point_arr[i].setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
        }
    });
    }
"