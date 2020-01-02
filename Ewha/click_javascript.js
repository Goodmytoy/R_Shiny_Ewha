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