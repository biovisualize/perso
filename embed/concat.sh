{ curl https://cdnjs.cloudflare.com/ajax/libs/vega/3.0.0-rc2/vega.js \
  && curl https://cdnjs.cloudflare.com/ajax/libs/vega-lite/2.0.0-beta.10/vega-lite.js \
  && curl https://cdnjs.cloudflare.com/ajax/libs/vega-embed/3.0.0-beta.19/vega-embed.js \
  && cat embed-script.js; } > exportfile