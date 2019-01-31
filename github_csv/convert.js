fs = require('fs');
d3 = require('d3v4');
fs.readFile('./issues.json', 'utf8', function (_err, _data) {
  const data = JSON.parse(_data)
  // const filtered = data.map(d => {
  //   return d.id
  // })
  // console.log(filtered)

  data.forEach(d => {
    d.author = d.user.login
    d.labels = d.labels.map(dB => dB.name)
    d.assignee = d.assignee && d.assignee.login
  })
  const keys = [
    "title",
    "html_url",
    "id",
    "number",
    "milestone",
    "created_at",
    "updated_at",
    "body",
    "author",
    "labels",
    "comments",
    "assignee"
  ]
  const csv = d3.csvFormat(data, keys)
  fs.writeFile("issues.csv", csv, function(err) {}); 
});