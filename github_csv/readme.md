node_modules/d3-dsv/bin/csv2json < issues.json > issues.csv
curl -i "https://biovisualize:autruchongris1@api.github.com/repos/mapd/mapd-immerse/issues?per_page=500&page=1" >> issues.txt
node index.js >> index.csv

"Issue name",html_url,id,"Issue ID","Fix Version/s","Created timestamp","Updated timestamp","Description","Reporter","Labels",comments,"Assignee"

"Summary","LEGACY GH issue ID","Fix Version/s","LEGACY GH created","LEGACY GH updated","Description","Reporter","Labels","Assignee"

{
title: "Issue name",
html_url: null,
id: null,
number: "Issue ID",
milestone: "Fix Version/s",
created_at: "Created timestamp",
updated_at: "Updated timestamp",
body: "Description",
author: "Reporter",
labels: "Labels",
comments: null
assignee: "Assignee"
}

Status    
Project
Component/s
Affects Version/s
Issue type
Priority
Resolution    
Votes    
Remaining Estimate
Time Spent
Original Estimate

make sure the dates are in the right format

Jira import doc https://confluence.atlassian.com/adminjiracloud/importing-data-from-csv-776636762.html

http://downloads.kano.me/public/4G/img/en_US/Kanux-Beta-v3.14.0-hotfix-jessie-release.img.gz


624
4705
4706
4701
4731
4733
4735