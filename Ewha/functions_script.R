# 0. Packages ----------------------------------------------
library(data.table)
library(plotly)
library(ggplot2)
library(shiny)
library(htmlwidgets)
library(showtext)
library(shinyBS)


# 1. 경로 설정 ----------------------------------------------
font_add_google(name = "Nanum Gothic", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 112)

# 2. RGB 설정 ----------------------------------------------
rgb_group_A = rgb(102,176,226, maxColorValue = 255)
rgb_group_B = rgb(255,189,55, maxColorValue = 255)
rgb_group_C = rgb(127,108,171, maxColorValue = 255)
rgb_group_D = rgb(158,200,110, maxColorValue = 255)
rgb_gray = rgb(192, 192, 192, maxColorValue = 255)
rgb_green = rgb(41, 76, 43, maxColorValue = 255)
rgb_red = rgb(255, 0, 0, maxColorValue = 255)


# 3. Functions ----------------------------------------------
fn_change_color_js = function(rgb_col, curve){
  js_code = sprintf("
      var curve_num = %d
      for(j=0; j<6; j++) {
        if(document.getElementsByClassName('scatterlayer')[j].getElementsByClassName('point').length == 0){
            continue;
        }
        for(pnt=0; pnt<document.getElementsByClassName('scatterlayer')[j].getElementsByClassName('scatter')[curve_num].getElementsByClassName('point').length;pnt++){
          document.getElementsByClassName('scatterlayer')[j].getElementsByClassName('scatter')[curve_num].getElementsByClassName('point')[pnt].style['fill'] = '%s'
          document.getElementsByClassName('scatterlayer')[j].getElementsByClassName('scatter')[curve_num].getElementsByClassName('point')[pnt].style['stroke'] = '%s'
        }
        
      }", curve, rgb_col, rgb_col)
  
  return(js_code)
}


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
                             x_axis = TRUE,
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
  
  
  
  if(browser == TRUE){
    # browser()
  }
  # 해당 데이터가 가지고 있는 max week을 구한다.
  max_week = max(as.integer(data[,week]))
  # 성적등급별, 주차별 Unique한 데이터만 사용한다.
  # (여러 학생이 있는 경우 그 중 첫번쨰 학생만 적용)
  temp_data = unique(data, by = c("성적등급", "week"))
  
  pal = c(pal, "red")
  
  if(grepl("그룹별", y) == FALSE){
    temp_data = unique(data, by = c(y, "week"))[,c("수강생", y, "성적등급", "실험집단", "week"), with = F]
    temp_data[,성적등급 := strsplit(y, " ")[[1]][length(strsplit(y, " ")[[1]])]]
    temp_data = na.omit(temp_data)
    pal = c("red", pal[1])
  }
  
  if(max_week < 15){
    temp = data.table(week = 1:15)
    temp$week = factor(temp$week)
    
    temp_data = merge(temp, temp_data, by = "week", all.x = TRUE)
    temp_data[, `:=` (`수강생` = rep(temp_data[1, `수강생`], 15), 
                      `실험집단` = rep(temp_data[1, `실험집단`], 15), 
                      `성적등급` = rep(temp_data[1, `성적등급`], 15))]
  }

  
  # Plotting (ggplot)
  g = ggplot(temp_data, aes_string(x = "week", y = paste0("`", y, "`"), group = "수강생", color = "성적등급"), na.rm = F) +
    scale_colour_manual(values = pal) +
    geom_line() +
    geom_point() + 
    ylim(-1, 21) + 
    # scale_x_discrete(labels = paste0(rep(1:max_week), "주차")) +
    scale_x_discrete(labels = paste0(rep(1:15), "주차")) +
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
  
  if(x_axis == FALSE){
    g = g + theme(axis.ticks = element_blank(),
                  axis.text.x = element_blank())
  }
  
  
  return(ggplotly(g, tooltip = c("week", y)))
}



fn_draw_strip_plot = function(data, 
                              this_data, 
                              y, 
                              type_vec, 
                              pal,
                              week_num = NULL,
                              y_max = 21,
                              my_data = NA, 
                              jitter = 0.1,
                              browser = FALSE){
  
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
  
  if(browser == TRUE){
    # browser()
  }
  # 2개의 빈 리스트를 생성(type 길이에 맞추지만 여기서는 길이가 2로 제한되어 있으므로)
  plot_list = vector("list", length = 2)

  # data = data[week %in% week_num]
  # if(nrow(data[week %in% week_num]) == 0){
  #   data = data[week %in% 1][, `:=` (week = rep(week_num, nrow(data[week %in% 1])), count = NA)]
  # }
  
  
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
      # geom_point(cex = 1.3) +
      labs(title = title, x = "구분", y = "점수") +
      ylim(-1, y_max+1) +
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
    
    
    
    
    # my_data가 NA가 아닌 경우에 빨간색으로 나의 점수를 표시
    if((is.na(my_data) == FALSE)){
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
    result_plot = subplot(g1, g2, shareY = TRUE, which_layout = 1)
  }else{
    result_plot = ggplotly(plot_list[[1]], tooltip = c("x", "y"))
  }
  
  return(result_plot)
}


# 4. Javascript Functions ----------------------------------------------
# legend 클릭 비활성화
legend_disable_js = "
function(el, x){
    el.on('plotly_legendclick', function() { return false; })
}
"

# Click EVent
# 클릭했을 때, 연계된 데이터 포인트들의 크기를 확대
click_event_js = "
function(el, x){
    el.on('plotly_click', function(data) {
        // 적용 대상이 되는 Strip Plot들의 개수를 담는 변수 : plot_len
        var plot_len = document.getElementsByClassName('scatterlayer').length
        // Pop-up을 하면 Plot의 개수가 4개 추가 되므로, 최대 Plot의 개수가 6을 초과하게 되면 4를 뺴서 strip_plot들만의 개수를 추출한다.
        if(plot_len > 6){
            plot_len = plot_len - 4
        }
        
        // CLick한 Plot의 curve 개수
        var clicked_plot_curve_len = el.getElementsByClassName('scatter').length;

        // 변경할 point들을 담을 Array 생성
        var point_arr = new Array(plot_len);
        
        // CLick한 데이터들의 curveNumber, pointNumber
        var curve_num = data.points[0].curveNumber;
        var point_num = data.points[0].pointNumber;
        
        // 추후 임시로 사용할 변수 생성
        var temp_point = ''
        
        console.log('data: ', String(data));
        console.log('curve_num: ', String(curve_num));
        console.log('point_num: ', String(point_num));
        
        // Click한 Point가 있는 Plot의 Curve 개수의 절반의 올림값보다, 값이 크게 나오는 경우는 Subplot의 2번째 Plot을 클릭한 경우 이므로,
        // curve_num을 그만큼 줄여 조정해준다.
        if(curve_num >= Math.ceil(clicked_plot_curve_len / 2)){
            curve_num = curve_num - Math.ceil(clicked_plot_curve_len / 2);
        }

        //나의 점수는 확대가 되지 않도록 설정
        // 길이가 1인(나의 점수는 1개의 점이므로) 점을 클릭한 경우 함수를 종료
        if(document.getElementsByClassName('scatter')[curve_num].getElementsByClassName('point').length == 1){
            return;
        }
          
        console.log('curve_num: ', String(curve_num));
        
        // 크기 변경을 하는 파트 
        // 1) Click한 point과 동일한 순서에 있는 point를 찾아서 저장 -> point_arr
        // (추가 1) 이미 확대된 점을 클릭한 경우 다시 축소 시킨다.
        // 2) 나의 점수를 제외하고는 전체 point를 작게 조정 (다른 점을 클릭할 떄 기존의 점을 작게 만들기 위해)
        // 3) 1)에서 저장한 point의 크기를 확대 시킨다.
        
        for(plt=0; plt<plot_len; plt++) {
            // point의 개수가 0개인 Plot은 pass한다.
            // 현재 학기 수강생의 기말고사 점수 Strip Plot
            if(document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point').length == 0){
                continue;
            }
            
            console.log('curve_num: ', String(curve_num));
            // 각 Plot들에서 Click한 점이 가지고 있는 curveNumber, pointNumber에 해당하는 point 값을 추출하여 point_arr에 저장
            point_arr[plt] = document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('scatter')[curve_num].getElementsByClassName('point')[point_num];
            


            // 확대된 점: 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z'
            // 작은 점(기본): 'M2.46,0A2.46,2.46 0 1,1 0,-2.46A2.46,2.46 0 0,1 2.46,0Z'
            // 나의점수 점(마름모): 'M7.37,0L0,7.37L-7.37,0L0,-7.37Z'

            // 이미 확대된 점을 클릭한 경우 다시 축소 시킨다.
            // 축소하는 작업만 하고 이후의 작업들을 수행하지 않기 위해 continue를 사용
            if((point_arr[plt].attributes['d'].value == 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z') ||
               (point_arr[plt].attributes['d'].value == 'M 10 0 A 10 10 0 1 1 0 -10 A 10 10 0 0 1 10 0 Z')){
                point_arr[plt].setAttribute('d', 'M2.46,0A2.46,2.46 0 1,1 0,-2.46A2.46,2.46 0 0,1 2.46,0Z');
                continue;
            } else {
                // 전체 point들의 크기를 작게 조절한다.
                // 나의 점수에 해당하는 point만을 제외하고 적용
                for(pnt=0; pnt<document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point').length; pnt++){
                    temp_point = document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point')[pnt];
                    // 나의 점수 제외
                    
                    if((temp_point.attributes['d'].value != 'M7.37,0L0,7.37L-7.37,0L0,-7.37Z') &&
                       (temp_point.attributes['d'].value != 'M 7.37 0 L 0 7.37 L -7.37 0 L 0 -7.37 Z')){
                        temp_point.setAttribute('d', 'M2.46,0A2.46,2.46 0 1,1 0,-2.46A2.46,2.46 0 0,1 2.46,0Z')
                    }
                }
                // 점을 확대 한다.
                point_arr[plt].setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
            }
        }
    });
 }
"

# Plot이 재생성될 떄 checkbox의 선택에 따라 점의 색상을 변경
color_change_js = "
function(el, x){
    console.log('Mode : ', document.getElementById('Mode').value)
    // 기본적으로 '지난 학기 수강생'일 때만 Group 지정을 할 수 있기 때문에 
    // Mode 가 '지난 학기 수강생'인 경우에만 함수 실행
    if(document.getElementById('Mode').value == '지난 학기 수강생'){
        // 적용 대상이 되는 Strip Plot들의 개수를 담는 변수 : plot_len
        var plot_len = document.getElementsByClassName('scatterlayer').length
        // Pop-up을 하면 Plot의 개수가 4개 추가 되므로, 최대 Plot의 개수가 6을 초과하게 되면 4를 뺴서 strip_plot들만의 개수를 추출한다.
        if(plot_len > 6){
            plot_len = plot_len - 4
        }
  
        console.log('color_change')
        
        // 성적 그룹 선택 CheckBox의 class를 선택
        var checkboxed = document.getElementsByName('Grade_Compare_Group')
        console.log('checkbox length: ', String(checkboxed.length))
        
        // color들을 저장할 배열
        var color_arr = new Array(checkboxed.length)

        color_arr[0] = 'rgb(102,176,226)'
        color_arr[1] = 'rgb(255,189,55)'
        color_arr[2] = 'rgb(127,108,171)'
        color_arr[3] = 'rgb(158,200,110)'
        
        

        // 색상을 변경을 하는 파트 
        for(plt=0; plt<plot_len; plt++){
            // point의 개수가 0개인 Plot은 pass한다.
            // 현재 학기 수강생의 기말고사 점수 Strip Plot
            if(document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point').length == 0){
                continue;
            }

            // Plot을 저장하는 변수 생성
            var scatter = document.getElementsByClassName('scatterlayer')[plt]
            
            // checkbox 가 클릭되면 순서대로 Group에 해당하는 Point들의 색을 변경해준다.
            // checkbox[0] = A 그룹 = 0 curveNumber
            // checkbox[1] = B 그룹 = 1 curveNumber 
            // checkbox[2] = C 그룹 = 2 curveNumber 
            // checkbox[3] = D 그룹 = 3 curveNumber 

            for(ckbx=0; ckbx<checkboxed.length; ckbx++){
                console.log('checkbox[', ckbx, ']')

                if(checkboxed[ckbx].checked){
                    console.log('checked')
                    for(pnt=0; pnt<scatter.getElementsByClassName('scatter')[ckbx].getElementsByClassName('point').length;pnt++){
                        console.log(String(scatter.getElementsByClassName('scatter')[ckbx].getElementsByClassName('point')[pnt].style['fill']))
                        // fill: 점의 색상
                        // stroke: 점의 테두리
                        scatter.getElementsByClassName('scatter')[ckbx].getElementsByClassName('point')[pnt].style['fill'] = color_arr[ckbx]
                        scatter.getElementsByClassName('scatter')[ckbx].getElementsByClassName('point')[pnt].style['stroke'] = color_arr[ckbx]
                    } 
                }   
            }  
        }
    }
}
"

cursor_disable_js = "
console.log('cursor default');
for(i=0; i<3; i++){
  document.getElementsByClassName('cursor-crosshair')[i].style['cursor'] = 'default';
}
"



click_event_maintain_js = "
function(el, x){
    // 적용 대상이 되는 Strip Plot들의 개수를 담는 변수 : plot_len
    var plot_len = document.getElementsByClassName('scatterlayer').length;
    // Pop-up을 하면 Plot의 개수가 4개 추가 되므로, 최대 Plot의 개수가 6을 초과하게 되면 4를 뺴서 strip_plot들만의 개수를 추출한다.
    if(plot_len > 6){
        plot_len = plot_len - 4;
    }
    
    console.log('plot_len : ', String(plot_len))

    // 첫번째 Plot은 SliderInput이 변경되어도 변하지 않기 떄문에 기준이 되는 Plot으로 설정
    var first_plot = document.getElementsByClassName('scatterlayer')[0];

    // curve_num, point_num, temp_point는 기본값을 -1로 설정
    // => 조건문에 사용
    var curve_num_mt = -1;
    var point_num_mt = -1;
    var temp_point = -1;
    
    // 확대된 Point의 curveNumber, pointNumber를 찾아서 저장
    for(crv=0; crv<first_plot.getElementsByClassName('scatter').length; crv++){
        for(pnt=0; pnt<first_plot.getElementsByClassName('scatter')[crv].getElementsByClassName('point').length; pnt++){
            var temp_point = first_plot.getElementsByClassName('scatter')[crv].getElementsByClassName('point')[pnt];
            if((temp_point.attributes['d'].value == 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z') || 
               (temp_point.attributes['d'].value == 'M 10 0 A 10 10 0 1 1 0 -10 A 10 10 0 0 1 10 0 Z')){
                curve_num_mt = crv;
                point_num_mt = pnt;

            }
        }
    }

    console.log('curve_num_mt : ', String(curve_num_mt));
    console.log('point_num_mt : ', String(point_num_mt));
    console.log('point_num_mt : ', String(curve_num_mt != -1));
    
    // curve_num_mt가 -1이 아니면 => 1번 Plot에서 확대된 점이 있는 경우
    // 다른 plot들에 대해서 확대된 점의 curveNumber와 pointNumber에 해당하는 point을 찾아서 같이 확대된 점으로 만든다.
    if(curve_num_mt != -1){
        console.log('curve_num_mt is not empty')
        for(plt=0; plt<plot_len; plt++){

            console.log('plot : ', String(plt))

            // point의 개수가 0개인 Plot은 pass한다.
            // 현재 학기 수강생의 기말고사 점수 Strip Plot
            if(document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point').length == 0){
                continue;
            }

            console.log('plot_apply : ', String(plt))
            // 다른 plot들에 대해서 확대된 점의 curveNumber와 pointNumber에 해당하는 point
            temp_point = document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('scatter')[curve_num_mt].getElementsByClassName('point')[point_num_mt];
            // 점을 확대 한다.
            temp_point.setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
        }
    }
}
"
