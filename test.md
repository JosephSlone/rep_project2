---
title: "Untitled"
author: "Joseph B. Slone"
date: "October 23, 2015"
output: html_document
---

Tables
======
 

```r
library(xtable)
data(iris)
print(xtable(head(iris, 10)), type = "html", include.rownames = F)
```

<!-- html table generated in R 3.2.1 by xtable 1.7-4 package -->
<!-- Fri Oct 23 11:33:17 2015 -->
<table border=1>
<tr> <th> Sepal.Length </th> <th> Sepal.Width </th> <th> Petal.Length </th> <th> Petal.Width </th> <th> Species </th>  </tr>
  <tr> <td align="right"> 5.10 </td> <td align="right"> 3.50 </td> <td align="right"> 1.40 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 4.90 </td> <td align="right"> 3.00 </td> <td align="right"> 1.40 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 4.70 </td> <td align="right"> 3.20 </td> <td align="right"> 1.30 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 4.60 </td> <td align="right"> 3.10 </td> <td align="right"> 1.50 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 5.00 </td> <td align="right"> 3.60 </td> <td align="right"> 1.40 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 5.40 </td> <td align="right"> 3.90 </td> <td align="right"> 1.70 </td> <td align="right"> 0.40 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 4.60 </td> <td align="right"> 3.40 </td> <td align="right"> 1.40 </td> <td align="right"> 0.30 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 5.00 </td> <td align="right"> 3.40 </td> <td align="right"> 1.50 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 4.40 </td> <td align="right"> 2.90 </td> <td align="right"> 1.40 </td> <td align="right"> 0.20 </td> <td> setosa </td> </tr>
  <tr> <td align="right"> 4.90 </td> <td align="right"> 3.10 </td> <td align="right"> 1.50 </td> <td align="right"> 0.10 </td> <td> setosa </td> </tr>
   </table>


