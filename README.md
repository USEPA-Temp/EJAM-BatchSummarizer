**batch.summarizer R package**

This is a shiny app that will be turned into an R package.

The app displays summary stats and graphics for an uploaded file of raw results of EJ buffer analysis (from batch tool or from several manually saved csv buffer reports). The only relevant public tool or dataset is the example built into this demo. Uses R's shinyapps tools to demonstrate an interactive web-based app.
  
This package is one of a series of [R packages related to environmental justice (EJ) analysis](http://ejanalysis.github.io/), as part of [ejanalysis.com](http://www.ejanalysis.com).  

Related packages, once each is made available as a public repository on GitHub, until available on cran, can be installed using the devtools package: 

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github("ejanalysis/analyze.stuff")  
devtools::install_github("ejanalysis/countyhealthrankings")  
devtools::install_github("ejanalysis/UScensus2010blocks")  
devtools::install_github("ejanalysis/ACSdownload")  
devtools::install_github(c("ejanalysis/proxistat", "ejanalysis/ejanalysis"))
devtools::install_github("ejanalysis/ejscreen")
```
