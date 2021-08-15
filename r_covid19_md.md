Gtrends & Covid19 // Google’s Capstone project
================

Lets collect some libs and data: \* Check for installed libs \* Refresh
data sources

``` r
##setting libs

#if(!"COVID19" %in% rownames(installed.packages())){
#  install.packages("COVID19")}
library(COVID19)
#if(!"devtools" %in% rownames(installed.packages())){
#  install.packages("devtools")}
library(devtools)
```

    ## Loading required package: usethis

``` r
#if(!"gtrendsR" %in% rownames(installed.packages())){
#  install_github("PMassicotte/gtrendsR")}
library(gtrendsR)
#if(!"tidyverse" %in% rownames(installed.packages())){
#  install.packages(tidyverse)}
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.2     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
##Loading recent data

###Format Today's date to match the data set 
  today <- Sys.Date()
  format(today, format="%Y-%m-%d")
```

    ## [1] "2021-08-15"

``` r
##Define what trends we'd like to follow
  var1 <- "בידוד"
  var2 <- "תסמינים"
  var3 <- "בדיקת קורונה"
  gtData <- gtrends(c(var1, var2, var3), geo = c("IL", "IL", "IL"), time = paste("2020-01-01", today))

##Clean everything but interest over time and arrange it
  gtData <- gtData$interest_over_time %>% arrange(date)

###Get Covid19 data for the below countries
##  update_dataset()
  coviData <-  do.call("rbind", list((covid19(country="israel", start = "2020-01-01", end = Sys.Date()))
        , (covid19(country="germany", start = "2020-01-01", end = Sys.Date()))
        , (covid19(country="russia", start = "2020-01-01", end = Sys.Date()))
        , (covid19(country="italy", start = "2020-01-01", end = Sys.Date()))))
```

    ## We have invested a lot of time and effort in creating COVID-19 Data Hub, please cite the following when using it:
    ## 
    ##   Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open
    ##   Source Software 5(51):2376, doi: 10.21105/joss.02376.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {COVID-19 Data Hub},
    ##     year = {2020},
    ##     doi = {10.21105/joss.02376},
    ##     author = {Emanuele Guidotti and David Ardia},
    ##     journal = {Journal of Open Source Software},
    ##     volume = {5},
    ##     number = {51},
    ##     pages = {2376},
    ##   }
    ## 
    ## To retrieve citation and metadata of the data sources see ?covid19cite. To hide this message use 'verbose = FALSE'.

``` r
  glimpse(coviData)
```

    ## Rows: 2,285
    ## Columns: 36
    ## Groups: id [4]
    ## $ id                                  <chr> "ISR", "ISR", "ISR", "ISR", "ISR",~
    ## $ date                                <date> 2020-01-22, 2020-01-23, 2020-01-2~
    ## $ vaccines                            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ tests                               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ confirmed                           <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ recovered                           <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ deaths                              <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ hosp                                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ vent                                <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ icu                                 <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ population                          <int> 8882800, 8882800, 8882800, 8882800~
    ## $ school_closing                      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ workplace_closing                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ cancel_events                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ gatherings_restrictions             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ transport_closing                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ stay_home_restrictions              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ internal_movement_restrictions      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ international_movement_restrictions <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3~
    ## $ information_campaigns               <int> 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1~
    ## $ testing_policy                      <int> 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ contact_tracing                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ stringency_index                    <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 2.78~
    ## $ iso_alpha_3                         <chr> "ISR", "ISR", "ISR", "ISR", "ISR",~
    ## $ iso_alpha_2                         <chr> "IL", "IL", "IL", "IL", "IL", "IL"~
    ## $ iso_numeric                         <int> 376, 376, 376, 376, 376, 376, 376,~
    ## $ currency                            <chr> "ILS", "ILS", "ILS", "ILS", "ILS",~
    ## $ administrative_area_level           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ administrative_area_level_1         <chr> "Israel", "Israel", "Israel", "Isr~
    ## $ administrative_area_level_2         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ administrative_area_level_3         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ latitude                            <dbl> 31, 31, 31, 31, 31, 31, 31, 31, 31~
    ## $ longitude                           <dbl> 35, 35, 35, 35, 35, 35, 35, 35, 35~
    ## $ key                                 <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
    ## $ key_apple_mobility                  <chr> "Israel", "Israel", "Israel", "Isr~
    ## $ key_google_mobility                 <chr> "IL", "IL", "IL", "IL", "IL", "IL"~

``` r
##Plot confirmed over time
  ggplot(data = coviData,
        aes(x = date, y = confirmed/population,  color = id)) +
        geom_point()
```

    ## Warning: Removed 92 rows containing missing values (geom_point).

![](r_covid19_md_files/figure-gfm/Refresh%20Libs%20&%20Data-1.png)<!-- -->

``` r
##Plot gtrends
  ggplot(data = gtData,
        aes(x = date, y = as.numeric(as.character(hits)), color = keyword)) +
        geom_line()
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

![](r_covid19_md_files/figure-gfm/Refresh%20Libs%20&%20Data-2.png)<!-- -->

``` r
## Transforming the data set so we could check for correlations
        gtData <- gtData %>% spread(key = keyword, value = hits)
        cor(as.numeric(as.character(gtData$`בדיקת קורונה`)), as.numeric(as.character(gtData$בידוד)),  method = "pearson", use = "complete.obs")
```

    ## Warning in is.data.frame(x): NAs introduced by coercion

    ## [1] 0.7212944

``` r
        cor(as.numeric(as.character(gtData$תסמינים)), as.numeric(as.character(gtData$בידוד)),  method = "pearson", use = "complete.obs")
```

    ## [1] 0.5376518

``` r
        cor(as.numeric(as.character(gtData$`בדיקת קורונה`)), as.numeric(as.character(gtData$תסמינים)),  method = "pearson", use = "complete.obs")
```

    ## Warning in is.data.frame(x): NAs introduced by coercion

    ## [1] 0.07013621

``` r
##coviData[c("vaccines", "recovered")][is.na(coviData[c("vaccines", "recovered")])] <- 0
covidCorr <- data.frame("date" = coviData$date, "country" = coviData$id, "confirmed" = as.numeric(as.character(coviData$confirmed))/as.numeric(as.character(coviData$population)), sumimmuned = coviData$vaccines/coviData$population/2+coviData$recovered/coviData$population, "vaccines" = coviData$vaccines/coviData$population/2, "recovered" = coviData$recovered/coviData$population)

glimpse(covidCorr)
```

    ## Rows: 2,285
    ## Columns: 6
    ## $ date       <date> 2020-01-22, 2020-01-23, 2020-01-24, 2020-01-25, 2020-01-26~
    ## $ country    <chr> "ISR", "ISR", "ISR", "ISR", "ISR", "ISR", "ISR", "ISR", "IS~
    ## $ confirmed  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ sumimmuned <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ vaccines   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ recovered  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~

``` r
  ggplot(data = covidCorr,
        aes(x = date, y = sumimmuned,  color = "sumImmuned")) +
        geom_line()+
        geom_line(aes(y=confirmed, color = "confirmed"))+
        geom_line(aes(y=vaccines, color = "vaccinated"))+
        geom_line(aes(y=recovered, color = "recovered"))+
        facet_grid(. ~ country)
```

    ## Warning: Removed 351 row(s) containing missing values (geom_path).

    ## Warning: Removed 342 row(s) containing missing values (geom_path).

    ## Warning: Removed 10 row(s) containing missing values (geom_path).

![](r_covid19_md_files/figure-gfm/Refresh%20Libs%20&%20Data-3.png)<!-- -->

``` r
  ggplot(data = covidCorr,
        aes(x = date, y = recovered,  color = "recovered")) +
        geom_line()+
        geom_line(aes(y=confirmed, color = "confirmed"))+
        facet_grid(. ~ country)       
```

    ## Warning: Removed 10 row(s) containing missing values (geom_path).

![](r_covid19_md_files/figure-gfm/Refresh%20Libs%20&%20Data-4.png)<!-- -->

``` r
##covidCorr <- covidCorr %>% spread(key = country, value = confirmed, immuned)
##covidCorr <- covidCorr %>% group_by(week = cut(date, "week")) %>% summarise_if(is.numeric, mean, na.rm = FALSE)
```
