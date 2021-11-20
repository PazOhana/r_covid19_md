Gtrends & Covid19 // Predicting a pandemic with google trends
================

Below is a showcase meant to present the amount of hits over specific
Google search keywords in israel  
and covid19 actual stats:

``` r
##First we run a quick setup:

  ##Check if anything needs to be installed
    if(!"COVID19" %in% rownames(installed.packages())){
      install.packages("COVID19")}
    if(!"devtools" %in% rownames(installed.packages())){
      install.packages("devtools")}
    if(!"gtrendsR" %in% rownames(installed.packages())){
      install_github("PMassicotte/gtrendsR")}
    if(!"tidyverse" %in% rownames(installed.packages())){
      install.packages("tidyverse")}
  
  ##Load required libs
    library(COVID19)
    library(devtools)
    library(gtrendsR)
    library(tidyverse)
  
  ##Format Today's date to match the data set 
    today <- Sys.Date()
    invisible(format(today, format="%Y-%m-%d"))
  
  ##Define what trends we'd like to follow (examining Israel only)
    var1 <- "בידוד"
    var2 <- "תסמינים"
    var3 <- "בדיקת קורונה"
    
  ##Define what countries we'd like to follow
    c1 <- "germany"
    c2 <- "israel"
    c3 <- "italy"
    c4 <- "portugal"

##Then we load the data into two main datasets, gtData & coviData:  

  ##Get Google trends data
    gtData <- gtrends(c(var1, var2, var3), geo = c("IL", "IL", "IL"), time = paste("2020-01-01", today))
  ##Clean everything but interest over time and arrange it by date
    gtData <- gtData$interest_over_time %>% arrange(date)
  
  ##Bind 4 calls of Covid19 data into one long DF
    coviData <- do.call("rbind", list((covid19(country=c1, start="2020-01-01", end=Sys.Date(), verbose=FALSE))
          , (covid19(country=c2, start = "2020-01-01", end = Sys.Date()))
          , (covid19(country=c3, start = "2020-01-01", end = Sys.Date()))
          , (covid19(country=c4, start = "2020-01-01", end = Sys.Date()))))
    colnames(coviData)[32] <- "countryName"
    
##Plotting a comparison between the selected countries, showing confirmed cases / population:
      
  ##Plot confirmed over time
    ggplot(data = coviData,
          aes(x = date, y = confirmed/population,  color = countryName)) +
          geom_point()
```

![](r_covid19_md_files/figure-gfm/Showcase%20and%20Data%20analysis-1.png)<!-- -->

``` r
##A comparison between the selected trends, showing their trending over time:
    
  ##Plot gtrends
    ggplot(data = gtData,
          aes(x = date, y = as.numeric(as.character(hits)), color = keyword)) +
          geom_line()+
          ylab("Google Trend")
```

![](r_covid19_md_files/figure-gfm/Showcase%20and%20Data%20analysis-2.png)<!-- -->

``` r
##Pivoting the dataset so we could check for correlations:
  gtData <- gtData %>% spread(key = keyword, value = hits)

  ##While editing this showcase, best correlation were between 'בידוד' and 'בדיקת קורונה': About 72.13 percent.
  ##as for today its:
    
  cor(as.numeric(as.character(gtData$`בדיקת קורונה`)), as.numeric(as.character(gtData$בידוד)),
      method = "pearson", use = "complete.obs")
```

    ## [1] 0.7175226

``` r
##Creating a second DF which scales the countries data according to their population:
##While representing "Immuned" as a summary for whom are twice vaccinated + whom are recovered.
  covidCorr <- data.frame("date" = coviData$date, "country" = coviData$countryName , "confirmed" =
    as.numeric(as.character(coviData$confirmed))/as.numeric(as.character(coviData$population)), sumimmuned =
    coviData$vaccines/coviData$population/2+coviData$recovered/coviData$population, "vaccines" =
    coviData$vaccines/coviData$population/2, "recovered" = coviData$recovered/coviData$population)

## Plotting all trending data:
  ggplot(data = covidCorr,
        aes(x = date, y = sumimmuned,  color = "sumImmuned")) +
        geom_line()+
        geom_line(aes(y=confirmed, color = "confirmed"))+
        geom_line(aes(y=vaccines, color = "vaccinated"))+
        geom_line(aes(y=recovered, color = "recovered"))+
        facet_grid(. ~ country)+
        ylab("Covid - since started")+
        theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
```

![](r_covid19_md_files/figure-gfm/Showcase%20and%20Data%20analysis-3.png)<!-- -->

``` r
##Plotting only confirmed vs. recovered
  ggplot(data = covidCorr,
        aes(x = date, y = recovered,  color = "recovered")) +
        geom_line()+
        geom_line(aes(y=confirmed, color = "confirmed"))+
        facet_grid(. ~ country)+
        ylab("Covid - since started (conf-reco)")+
        theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
```

![](r_covid19_md_files/figure-gfm/Showcase%20and%20Data%20analysis-4.png)<!-- -->

This md report made possible thanks to the hard work of R’s Covid19 lib
crew:  
Guidotti, E., Ardia, D., (2020), “COVID-19 Data Hub”, Journal of Open  
Source Software 5(51):2376, doi: 10.21105/joss.02376.

I Hope you read zero growth of confirmed cases by now.

Stay safe!  
Paz.
