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