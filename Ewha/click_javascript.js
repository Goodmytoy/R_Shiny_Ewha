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
            if(point_arr[plt].attributes['d'].value == 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z'){
                point_arr[plt].setAttribute('d', 'M2.46,0A2.46,2.46 0 1,1 0,-2.46A2.46,2.46 0 0,1 2.46,0Z');
                continue;
            } else {
                // 전체 point들의 크기를 작게 조절한다.
                // 나의 점수에 해당하는 point만을 제외하고 적용
                for(pnt=0; pnt<document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point').length; pnt++){
                    temp_point = document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('point')[pnt];
                    // 나의 점수 제외
                    if(temp_point.attributes['d'].value != 'M7.37,0L0,7.37L-7.37,0L0,-7.37Z'){
                        temp_point.setAttribute('d', 'M2.46,0A2.46,2.46 0 1,1 0,-2.46A2.46,2.46 0 0,1 2.46,0Z')
                    }
                }
                // 점을 확대 한다.
                point_arr[plt].setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
            }
        }
    });
 }