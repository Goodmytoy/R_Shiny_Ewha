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
            if(temp_point.attributes['d'].value == 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z'){
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