const request = require("request");
fs = require('fs');
d3 = require('d3v4');

const USERNAME = "biovisualize";
const PASSWORD = "autruchongris1";

function query (page) {
  const options = {  
      url: `https://${USERNAME}:${PASSWORD}@api.github.com/repos/mapd/mapd-immerse/issues?per_page=500&page=${page}`,
      method: "GET",
      headers: {
          "Accept": "application/json",
          "Accept-Charset": "utf-8",
          "User-Agent": "agent"
      }
  };
  request(options, (err, res, body) => {
    if (err) { return console.log(err); }
    transform(body, page)
  });
}

function transform (body, page) {
    const data = JSON.parse(body)
    // console.log(data);

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
    write(csv, page)
}

function write (csv, page) {
  fs.writeFile(`issues${page}.csv`, csv, function(err) {}); 
}

d3.range(0, 1).forEach(query)
