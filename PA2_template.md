Reproducible Research, Peer Assessment 2
========================================

## Questions

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## Data


```r
require(data.table)
```

```
## Loading required package: data.table
```

```r
setInternet2(TRUE)
if(!file.exists('StormData.RData')) {
    here <- tempdir()
    data <- paste(here,'StormData.csv.bz2',sep='/')
    download.file('https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2',data)
    data <- data.table(read.csv(data))
    save(data,file='StormData.RData')
} else {
    load('StormData.RData')
}
```

## Some summary statistics


```r
myn <- 10
peoplesum <- data[,list(fatalities=sum(FATALITIES), 
                        injuries=sum(INJURIES)),
                  by=list(EVTYPE)]
```

1. Top 10 types of weather events that kill most people:
    
    ```r
    ftop <- head(peoplesum[order(-fatalities),],n=myn)
    ftop
    ```
    
    ```
    ##             EVTYPE fatalities injuries
    ##  1:        TORNADO       5633    91346
    ##  2: EXCESSIVE HEAT       1903     6525
    ##  3:    FLASH FLOOD        978     1777
    ##  4:           HEAT        937     2100
    ##  5:      LIGHTNING        816     5230
    ##  6:      TSTM WIND        504     6957
    ##  7:          FLOOD        470     6789
    ##  8:    RIP CURRENT        368      232
    ##  9:      HIGH WIND        248     1137
    ## 10:      AVALANCHE        224      170
    ```
    
2. Top 10 types of weather events that injure most people:
    
    ```r
    itop <- head(peoplesum[order(-injuries),],n=myn)
    itop
    ```
    
    ```
    ##                EVTYPE fatalities injuries
    ##  1:           TORNADO       5633    91346
    ##  2:         TSTM WIND        504     6957
    ##  3:             FLOOD        470     6789
    ##  4:    EXCESSIVE HEAT       1903     6525
    ##  5:         LIGHTNING        816     5230
    ##  6:              HEAT        937     2100
    ##  7:         ICE STORM         89     1975
    ##  8:       FLASH FLOOD        978     1777
    ##  9: THUNDERSTORM WIND        133     1488
    ## 10:              HAIL         15     1361
    ```

3. Overlap between the two: 
    
    ```r
    fsum <- as.character(ftop[,EVTYPE])
    psum <- as.character(itop[,EVTYPE])
    olap <- sort(intersect(fsum,psum))
    ftopshare <- sum(ftop[,injuries][ftop[,EVTYPE] %in% olap])/sum(ftop[,injuries])
    itopshare <- sum(itop[,fatalities][itop[,EVTYPE] %in% olap])/sum(itop[,fatalities])
    fallshare <- sum(ftop[,injuries][ftop[,EVTYPE] %in% olap])/sum(peoplesum[,injuries])
    iallshare <- sum(itop[,fatalities][itop[,EVTYPE] %in% olap])/sum(peoplesum[,fatalities])
    t(t(olap))
    ```
    
    ```
    ##      [,1]            
    ## [1,] "EXCESSIVE HEAT"
    ## [2,] "FLASH FLOOD"   
    ## [3,] "FLOOD"         
    ## [4,] "HEAT"          
    ## [5,] "LIGHTNING"     
    ## [6,] "TORNADO"       
    ## [7,] "TSTM WIND"
    ```

The 7 types of events that show up in both top 10 lists are responsible for 99% of fatalities and 98% of injuries in each top 10 list. This amounts to 74.22% of all recorded injuries and 85.91% of all recorded fatalities in the StormData file.


