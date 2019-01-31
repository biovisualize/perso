// this makes all methods chainable
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
  return {
    init,
    config,
    setData,
    render
  }
}

const chart1 = chartManager.init()
  .config()
  .setData()
  .render()