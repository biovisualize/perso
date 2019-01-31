const ChartManager = function() {
  function init() {
    return this
  }

  function config() {
    return this
  }

  function setData() {
    return this
  }

  function render() {
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
    return this
  }

  function getFilteredData() {
    return this
  }
  return {
    getData,
    getFilteredData
  }
}

const eventManager = (function(_) {
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
})({})

const chartManager = ChartManager()
const dataManager = DataManager()

const chart1 = chartManager.init()
  .config()
  .setData(dataManager.getData())
  .render()
  .onBrush(d => eventManager.publish("onBrush1", {range: d}))

const chart2 = chartManager.init()
  .config()
  .setData()
  .render()

eventManager.subscribe("onBrush1", d => chart2.setData(dataManager.getFilteredData()))

