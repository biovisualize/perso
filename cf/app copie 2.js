const ChartManager = function() {
  let spec = {
    "$schema": "https://vega.github.io/schema/vega-lite/v2.0.json",
    "signals": [
      {
        "name": "custom",
        "value": {},
        "on": [
          {"events": "rect:mouseover", "update": "datum"},
          {"events": "rect:mouseout",  "update": "{}"}
        ]
      }
    ],
    "data": {
      "values": [
      ]
    },
    "mark": "bar",
    "encoding": {
      "x": {"field": "key", "type": "ordinal"},
      "y": {"field": "value", "type": "quantitative"}
    }
  }
  let rootSelector = null
  function init(selector) {
    rootSelector = selector
    return this
  }

  function config() {
    return this
  }

  function setData(data) {
    spec.data.values = data
    render()
    return this
  }

  function render() {
    vegaEmbed(rootSelector, spec, {actions: false})
      .then(function(result) {
        console.dir(result)

        result.view.addSignalListener('custom', function(name, value) {
          console.log('WIDTH: ' + value)
        })
        result.view.width(500).run()

        // result.view.addSignalListener('custom', (name, value) => {
        //   console.log(name, value)
        // })
      }).catch(console.error);
    return this
  }

  function onBrush() {
    return this
  }
  return {
    init,
    config,
    setData,
    render,
    onBrush
  }
}

const DataManager = function() {
  function getData() {
    return [
      {"key": "A","value": 28}, {"key": "B","value": 55}, {"key": "C","value": 43},
      {"key": "D","value": 91}, {"key": "E","value": 81}, {"key": "F","value": 53},
      {"key": "G","value": 19}, {"key": "H","value": 87}, {"key": "I","value": 52}
    ]
  }

  function getFilteredData(key) {
    return getData().filter(d => d.key === key)
  }
  return {
    getData,
    getFilteredData
  }
}

const EventManager = function(_ = {}) {
  function publish(a, b, c, d) {
    for (d = -1, c = [].concat(_[a]); c[++d];) {
      c[d](b)
    }
    return this
  }

  function subscribe(a, b) {
    (_[a] || (_[a] = [])).push(b)
    return this
  }

  function unsubscribe() {
    return this
  }

  return {
    publish,
    subscribe,
    unsubscribe
  }
}

const chartManager1 = ChartManager()
const chartManager2 = ChartManager()
const dataManager = DataManager()
const eventManager = EventManager()

const chart1 = chartManager1.init("#chart1")
  .config()
  .setData(dataManager.getData())
  .onBrush(d => eventManager.publish("onBrush1", {selection: d}))

const chart2 = chartManager2.init("#chart2")
  .config()
  .setData(dataManager.getData())

eventManager.subscribe("onBrush1", d => chart2.setData(dataManager.getFilteredData(d)))

