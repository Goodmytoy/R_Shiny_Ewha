function(el, x){
    var plot_len = document.getElementsByClassName('scatterlayer').length
    if(plot_len > 6){
        plot_len = plot_len - 4
    }

    var clicked_plot_curve_len = el.getElementsByClassName('scatter').length;
    var point_arr = new Array(plot_len);
    var old_point_arr = new Array(plot_len);
    var plotly_div_arr = new Array(plot_len);
    var curve_num_arr = new Array(plot_len);

    el.on('plotly_click', function(data) {

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
            if(document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('point').length == 0){
                continue;
            }
            console.log('curve_num: ', String(curve_num));
            point_arr[i] = document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('scatter')[curve_num].getElementsByClassName('point')[point_num];
            plotly_div_arr[i] =  document.getElementsByClassName('plotly')[i];
        }
        
        for(i=0; i<plot_len; i++) {
            if(document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('point').length == 0){
                continue;
            }
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
            if(document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('point').length == 0){
                continue;
            }
            plotly_div_arr[i].backup = {curveNumber: curve_num,
                                        pointNumber: point_num,
                                        d: point_arr[i].attributes['d'].value,
                                        style: point_arr[i].attributes['style'].value
                                        };
        }
        for(i=0; i<plot_len; i++) {
            if(document.getElementsByClassName('scatterlayer')[i].getElementsByClassName('point').length == 0){
                continue;
            }
            point_arr[i].setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
        }
    });
}


var plot_len = document.getElementsByClassName('scatterlayer').length
var plotly_div_arr = new Array(plot_len);

if(plot_len > 6){
    plot_len = plot_len - 4
}
var checkboxes = document.getElementsByName('Grade_Compare_Group')
var chk = false

for(i=0; i<checkboxes.length; i++){
    if(checkboxes[i].checked){
        for(j=0; j<plot_len; j++) {
            plotly_div_arr[j] = document.getElementsByClassName('plotly')[j];
        }
    }
}

function(el, x){
    var checkboxed = document.getElementsByName("Grade_Compare_Group")
    var chk = false

    for(i=0; i<checkboxed.length; i++){
        if(checkboxed[i].checked){
            for(pnt=0; pnt<el.getElementsByClassName('scatter')[i].getElementsByClassName('point').length;pnt++){
                el.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['fill'] = '%s'
                el.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['stroke'] = '%s'
            }     
        }
    }
}





function(el, x){

    var plot_len = document.getElementsByClassName('scatterlayer').length
    if(plot_len > 6){
        plot_len = plot_len - 4
    }

    console.log('color_change')
    var checkboxed = document.getElementsByName('Grade_Compare_Group')
    var color_arr = new Array(checkboxed.length)
    color_arr[0] = 'rgb(102,176,226)'
    color_arr[1] = 'rgb(255,189,55)'
    color_arr[2] = 'rgb(127,108,171)'
    color_arr[3] = 'rgb(158,200,110)'
    
    for(j=0; j<plot_len; j++){
        scatter = document.getElementsByClassName('scatterlayer')[j]
        for(i=0; i<checkboxed.length; i++){
            console.log('checkbox[', i, ']')
            if(checkboxed[i].checked){
                console.log('checked')
                for(pnt=0; pnt<scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point').length;pnt++){
                    console.log(String(scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['fill']))
                    scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['fill'] = color_arr[i]
                    scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['stroke'] = color_arr[i]
                }     
            } else {
                for(pnt=0; pnt<scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point').length;pnt++){
                    scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['fill'] = 'rgb(192,192,192)'
                    scatter.getElementsByClassName('scatter')[i].getElementsByClassName('point')[pnt].style['stroke'] = 'rgb(192,192,192)'
                }   
            }
        }
    }
}


function(el, x){
    var plot_len = document.getElementsByClassName('scatterlayer').length;
    if(plot_len > 6){
        plot_len = plot_len - 4;
    }
    var first_plot = document.getElementsByClassName('scatterlayer')[0];
    var curve_num = null;
    var point_num = null;
    for(crv=0; crv<first_plot.getElementsByClassName('scatter').length; crv++){
        for(pnt=0; pnt<first_plot.getElementsByClassName('scatter')[crv].getElementsByClassName('point').length; pnt++){
            var temp_point = first_plot.getElementsByClassName('scatter')[crv].getElementsByClassName('point')[pnt];
            if(temp_point.attributes['d'].value == 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z'){
                curve_num = crv;
                point_num = pnt;
                console.log('curve_num : ', String(crv))
                console.log('point_num : ', String(point_num))
            }
        }
    }
    
    /*
    if(!curve_num){
        for(plt=0; plt<plot_len; plt++){
            console.log('plot : ', String(plt))
            temp_point = document.getElementsByClassName('scatterlayer')[plt].getElementsByClassName('scatter')[crv].getElementsByClassName('point')[pnt];
            temp_point.setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
        }
    }  
    */
    if(!curve_num){
        temp_point = el.getElementsByClassName('scatter')[crv].getElementsByClassName('point')[pnt];
        temp_point.setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
    }
}