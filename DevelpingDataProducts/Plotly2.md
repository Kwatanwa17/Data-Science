Peer-graded Assignment: R Markdown Presentation & Plotly
========================================================
author: "kwatanwa17"
date: "2018-9-24"
autosize: true

Load plotly
========================================================


```r
library(plotly)
```

Clean the data set
========================================================


```r
mtcars$am <- factor(mtcars$am, levels = c(0,1), labels = c("Automatic", "Manual"))
```

plotly 
========================================================

Weight (wt) vs Miles per galon (mpg) by transmission type (am) using Number of gears (gear)


```
Error in file(con, "rb") : cannot open the connection
```
