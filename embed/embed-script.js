document.head.innerHTML += '<link rel="stylesheet" href="script.css">';

var container = document.querySelector("#immerse")
container.innerHTML = '<div class="chart1"></div><div class="chart2"></div>'

var spec = {
  "$schema": "https://vega.github.io/schema/vega-lite/v2.json",
  "description": "A scatterplot showing horsepower and miles per gallons.",
  "data": {"url": "https://vega.github.io/vega-lite/data/cars.json"},
  "mark": "point",
  "encoding": {
    "x": {"field": "Horsepower", "type": "quantitative"},
    "y": {"field": "Miles_per_Gallon", "type": "quantitative"},
    "color": {"field": "Origin", "type": "nominal"},
    "shape": {"field": "Origin", "type": "nominal"}
  }
}

vega.embed(".chart1", spec);
var spec = "https://raw.githubusercontent.com/vega/vega/master/docs/examples/bar-chart.vg.json";
vega.embed('.chart2', spec);