// dot plots and scatter plots
//initialize DataTable:
var table = $('#table').DataTable({
    "colReorder": true,
    "columnDefs": [
      { //hide rowID column
        "targets": [0],
        "visible": false
      }
    ]
  });

// create form for variable selection:
function createVariableSelectionForm() {
  //create form
  var form = document.createElement('form');
  form.setAttribute('class', 'form-inline');
  form.setAttribute('id', 'form');
  document.getElementById('control').appendChild(form);

  //create label for form
  var formLabel = document.createElement('label');
  formLabel.setAttribute('for', 'selectVar');
  formLabel.innerHTML = "Variables to display";
  form.appendChild(formLabel);

  //create selection options:
  var selectVar = document.createElement('select');
  selectVar.setAttribute('class', 'form-control');
  selectVar.setAttribute('id', 'selectVar');
  selectVar.setAttribute('multiple', 'multiple');
  form.appendChild(selectVar);

  // get column names:
  var ncol = $("#table thead tr th").length;
  var th = document.getElementsByTagName('th');
  for (var i = 0; i <= ncol; i++){
      var opt = document.createElement('option');
      if (i == 0) {
        opt.value = 0;
        opt.classList.add('select');
        opt.innerHTML = "Display all";
        opt.selected = "selected";
      } else {
      opt.value = i;
      opt.innerHTML = th[i-1].innerHTML;
    }
    selectVar.appendChild(opt);
  };

}

createVariableSelectionForm();

//drive the viewTable button:
showTable = function() {
  var tableWrapper = $('#table_wrapper');
  tableWrapper.toggleClass('hidden');
};

//find out whether it's a dot or scatter:
// if a box plot exists:
var boxElements = document.getElementById('inz-box.1.1.1.1');
if (boxElements !== null) {
  // for dotplots only...
  var Grob = "inz-DOTPOINTS.1.1.1.1",
      lastLine = "inz-box-line.1.1.1.1";
  boxMe(lastLine, boxElements);
} else {
  var Grob = "inz-SCATTERPOINTS.1.1.1";
}

var panel = document.getElementById(Grob);
var count = panel.childElementCount;
var ind = [];

// TOOLTIPS:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .style('width', '100');

d3.select(panel).selectAll('use')
  .data(data)
  .attr('class', 'point')
  .on('mouseover', function (d, i) {
      var g = ' ';
      var names = chart.varNames;
      var selectVar = document.getElementsByTagName('select')[0];
      var sOpt = selectVar.selectedOptions;
      var s = [];
      for (j = 0; j < sOpt.length; j++) {
          s.push(Number(sOpt[j].value));
        }

      for (var j = 0; j < s.length; j++) {
        if (s[j] !== undefined) {
          var w = names[s[j]-1] + ": <span>"
          var t =  data[i][names[s[j]-1]] + "</span> <br>"
       }

       var g = w + t + g;
       var p = s.length;

       if (s.includes(0)) {
         var g = ' ';
         for (var k = 0; k < names.length; k++) {
           var w = names[k] + ": <span>"
           var t =  data[i][names[k]] + "</span> <br>"
         var g = w + t + g;
         var p = names.length;
       }
     }
   }

   return tooltip.style('visibility', 'visible')
                 .style("left", d3.event.pageX - 50 + "px")
                 .style("top", d3.event.pageY - 30 - 15*(p-1) + "px")
                 .html(g);
    })
    .on('mouseout', function () { return tooltip.style("visibility", 'hidden'); })
    .on('click', function (d, i) {
      var selected = this;
        var ind = [];

        d3.select(panel).selectAll('.point')
            .attr("class", function() {
              if (this.getAttribute('class') === "point selected") {
                return "point selected";
              } else if (this === selected) {
                  return "point selected";
              } else {
                return "point none";
              }
            })

        //filter table:
        //search for all those that are selected, then extract id num:
        var selected = document.getElementsByClassName('selected');
            for (var k = 0; k < selected.length; k++) {
                var id = selected[k].id;
                idNum = id.substring(id.lastIndexOf('.') + 1);
                ind.push("^" + idNum + "$");
            }
        table.search('').columns().search('').draw();
        table.columns(0).search(ind.join("|"), true).draw();

        //hide box
        d3.selectAll('.boxData')
          .classed('hidden', true);
    })

    .on('dblclick', function (d, i) { // deselect
      var selected = this;
        var ind = [];
       selected.setAttribute('class', 'point none');

        // update table:
        var selected = document.getElementsByClassName('selected');
            for (var k = 0; k < selected.length; k++) {
                var id = selected[k].id;
                idNum = id.substring(id.lastIndexOf('.') + 1);
                ind.push("^" + idNum + "$");
            }
       table.search('').columns().search('').draw();
       table.columns(0).search(ind.join("|"), true).draw();
    });

//link TABLE TO PLOT:
$('#table tbody').on('click', 'tr', function() {
    $(this).toggleClass('active');
    var ind = table.rows('.active')[0];
    d3.select(panel).selectAll('.point')
      .attr("class", function(d, i) {
        return (ind.includes(i) ? "point selected" : "point none");
      })
    })

//LEGEND INTERACTION:
var legendLayout = document.getElementById('inz-leg-layout.1');
if (legendLayout) {
  //grabbing keys and text from the legend:
  var colGroupNo = chart.colGroupNo;
  //assigning mouse events:
  for (i = 1; i <= colGroupNo; i++) { //colGroupNo -> colby levels from R (nlevels)
      var keyText = document.getElementById('inz-leg-txt-' + i + '.1.1.tspan.1');
      var key = document.getElementById('inz-leg-pt-' + i + '.1.1');

      (function (i) {
          key.addEventListener("mouseover", function () { show(i) }, false);
          key.addEventListener("mouseout", function () { out(i) }, false);
          key.addEventListener("click", function () { subset(i) }, false);

          keyText.addEventListener("mouseover", function () { show(i) }, false);
          keyText.addEventListener("mouseout", function () { out(i) }, false);
          keyText.addEventListener("click", function () { subset(i) }, false);
      })(i)
    }

    //on click, subsetting occurs:
    subset = function (i) {

      //get the title variable:
      var titleVar = document.getElementById('inz-leg-title.1.1.tspan.1').innerHTML;
      var key = document.getElementById('inz-leg-pt-' + i + '.1.1');
      var keyText = document.getElementById('inz-leg-txt-' + i + '.1.1.tspan.1').innerHTML;
      var names = chart.varNames;

      for (j = 1; j <= count; j++) {
          var point = document.getElementById(Grob + '.' + j);
          if (key.getAttribute('fill') == point.getAttribute('stroke')) {
              point.setAttribute('class', 'point selected');
          } else {
              point.setAttribute('class', 'point none');
          }
      }

      // find column index + filter: (add one for hidden column)
      var colInd = names.indexOf(titleVar) + 1;
      table.search('').columns().search('').draw();
      table.columns(colInd).search("^" + keyText + "$", true).draw();
    }
};

// BRUSH EFFECTS:
var brush = d3.brush()
              .on("brush", brushmove)
              .on("end", brushend);

// attaching brush to correct positions:
if (chart.type == "dot") {
  var pp = panel.parentNode.parentNode,
      insertBrush = ":first-child";
} else {
  var pp = panel.parentNode,
      insertBrush = "g:nth-child(4)";
}

d3.select(pp)
  .insert("g", insertBrush)
  .attr("class", "brush")
  .call(brush);

// make handles invisible:
d3.selectAll('.handle')
  .style('opacity', 0);
d3.select('.overlay')
  .style('stroke', 'none');

function brushmove() {
  var s = d3.event.selection;
  var x1 = s[0][0];
  var x2 = s[1][0];
  var y1 = s[0][1];
  var y2 = s[1][1];
  var ind = [];

  for (i =1; i <= count; i++) {
    var point = document.getElementById(Grob + '.' + i);
    var x = point.x.baseVal.value;
    var y = point.y.baseVal.value;
    var dataRow = document.getElementById('tr' + i);

    //points that lie within the  boundary box drawn:
    if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
      point.setAttribute('class', 'point selected');
      ind.push("^" + i + "$");
    } else {
      point.setAttribute('class', 'point none');
    }
  }
  //reset and filter table:
  table.columns('').search().columns('').draw();
  table.columns(0).search(ind.join("|"), true).draw();
};

//Reset Button:
reset = function() {
    d3.selectAll('.point')
      .classed("none selected", false);

    // restore table to original state
    table.search('').columns().search('').draw();
    table.rows().nodes().to$().removeClass('active');

    d3.selectAll('.boxData')
      .classed("hidden", true);

    d3.selectAll('.selection')
      .style("display", "none");
};

// deselection/reset using plotregion double-click:
var plotRegion = document.getElementsByClassName('overlay')[0];
plotRegion.addEventListener("dblclick", reset, false);
