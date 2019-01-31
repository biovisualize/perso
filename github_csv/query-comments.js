const request = require("request");
const fs = require('fs');
const d3 = require('d3v4');
const async = require("async")

const USERNAME = "biovisualize";
const PASSWORD = "autruchongris1";

function queryComments (issueNumber, cb) {
  console.log("Fetching comments for", issueNumber)
  const options = {  
      url: `https://${USERNAME}:${PASSWORD}@api.github.com/repos/mapd/mapd-immerse/issues/${issueNumber}/comments`,
      method: "GET",
      headers: {
          "Accept": "application/json",
          "Accept-Charset": "utf-8",
          "User-Agent": "agent"
      }
  };
  request(options, (err, res, body) => {
    const data = JSON.parse(body)
    if (err) { return console.log(err); }
    cb(data.map(d => [d.body, d.user.login]).join("\r\n"))
  });
}

function write (err, _data) {
  const csv = d3.csvFormat(_data)
  fs.writeFile(`issues-comments.csv`, csv, function(err) {}); 
}

fs.readFile('./issues-clean3.csv', 'utf8', function (_err, _data) {
  const data = d3.csvParse(_data)

  let count = 0
  async.doUntil(
    cb => {
      queryComments(data[count]["LEGACY GH issue ID"], comments => {
        data[count]["Comment Body"] = comments
        count++
        cb(null, data, count)
      })
    },
    (d, count) => count >= d.length,
    write
  )
})