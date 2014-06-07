Reproducible Research, Peer Assessment 2
========================================

## Questions

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## Data


```r
require(data.table) # for faster summary stats
```

```
## Loading required package: data.table
```

```r
setInternet2(TRUE)  # for https downloads.
# Download is slow, and read.csv() from large .bz2 file is even slower.
# So, to speed things up, do it only once and save an .RData version.
# I know this is not 100% OK (Peng exhorts you to save no intermediary
# data files) but if .RData is not saved to GitHub, all is well: customer
# who forks the code will have no choice but do download and read.csv().
if(!file.exists('StormData.RData')) {
    here <- tempdir()
    data <- paste(here,'StormData.csv.bz2',sep='/')
    download.file('https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2',data)
    data <- data.table(read.csv(data, stringsAsFactors=FALSE))
    save(data,file='StormData.RData')
} else {
    load('StormData.RData')
}
# Transform the date of interest from string to true date
data$BGN_DATE <- as.Date(sapply(data$BGN_DATE,
                                function(x) {
                                    unlist(strsplit(x," "))[1]
                                }),"%m/%d/%Y")
```

## Ranking of event types by injuries and fatalities


```r
myn <- 10 # Top myn causes of injuries, fatalities. Set to suit.
myy <- 10 # Most recent years of data to consider separately. Set to suit.

oldest <- min(year(data$BGN_DATE))
newest <- max(year(data$BGN_DATE))
oldesty <- subset(data,year(BGN_DATE) %in% c(oldest:oldest+(myy-1)))
newesty <- subset(data,year(BGN_DATE) %in% c(newest-(myy-1):newest))

# summarize data: add losses of interest by event type.
# returns a data table.
getLossSum <- function(dt) {
    out <- dt[,list(fatalities=sum(FATALITIES), 
                    injuries=sum(INJURIES),
                    cropdmg=sum(CROPDMG),
                    propdmg=sum(PROPDMG)),
              by=list(EVTYPE)]
    out[,econdmg:=cropdmg+propdmg]
    return(subset(out,select=-c(cropdmg,propdmg)))
}

# report damage. 2 arguments:
# dt -- a data table summarized by getLossSum()
# thisn -- how many observations in the head() call
# returns a list with as many elements as there are
# metrics of interest. In this case, 3:
# fatalities, injuries, econdmg.
# each element is a data table.
getLoss <- function(dt,thisn=myn) {
    myx <- setdiff(names(dt),'EVTYPE')
    getTopset <- function(x) {
        return(head(subset(dt,select=c(EVTYPE,get(x)))[order(-get(x)),],n=thisn))
    }
    out <- lapply(myx,FUN=getTopset)
    names(out) <- myx
    return(out)
}

# get share of event types listed
# in string vector e for loss x
# in data table dt. returns a number 
# with a value between 0 and 1.
getShareOfLoss <- function(e,x,dt) {
    num <- sum(dt[,get(x)][dt[,EVTYPE] %in% e],na.rm=TRUE)
    den <- sum(dt[,get(x)],na.rm=TRUE)
    return(num/den)
}

# get data frame of top losses of all times and
# from newest myn years, from lists produced
# by getLoss()
getLossTable <- function(x,all,new) {
    out <- merge(all[[x]],new[[x]],by=c('EVTYPE'),all=TRUE)
    oldnames <- c('EVTYPE',paste(x,'x',sep='.'),paste(x,'y',sep='.'))
    newnames <- c('EVTYPE',paste('all',x,sep='.'),paste('last',myn,'years',sep='.'))
    setnames(out,oldnames,newnames)
    return(out[order(-get(paste('all',x,sep='.'))),])
}

# Get summarized data tables.
# This is the analytical data.
peoplesum <- getLossSum(data)
peoplenew <- getLossSum(newesty)
```

There are 985 event types in the StormData data set, tracked over 61 years, between 1950 and 2011. These events were responsible for 15,145 fatalities and 140,528 injuries. 

Over time, the number of event types tracked grew from 3 over the first 10 years data to 973 over the last 10.


```r
allLoss <- getLoss(peoplesum)
newLoss <- getLoss(peoplenew)
fatalities <- getLossTable('fatalities',allLoss,newLoss)
injuries   <- getLossTable('injuries',allLoss,newLoss)
overlap    <- intersect(fatalities$EVTYPE,injuries$EVTYPE)
econdmg    <- getLossTable('econdmg',allLoss,newLoss)
```

1. Top 10 types of weather events that kill most people:
    
    ```r
    fatalities
    ```
    
    ```
    ##             EVTYPE all.fatalities last.10.years
    ##  1:        TORNADO           5633          4576
    ##  2: EXCESSIVE HEAT           1903          1379
    ##  3:    FLASH FLOOD            978           471
    ##  4:           HEAT            937           708
    ##  5:      LIGHTNING            816           497
    ##  6:      TSTM WIND            504           443
    ##  7:          FLOOD            470           233
    ##  8:    RIP CURRENT            368            NA
    ##  9:      HIGH WIND            248           170
    ## 10:      AVALANCHE            224            NA
    ## 11:      HEAT WAVE             NA           172
    ## 12:   RIP CURRENTS             NA           203
    ```
    
2. Top 10 types of weather events that injure most people:
    
    ```r
    injuries
    ```
    
    ```
    ##                 EVTYPE all.injuries last.10.years
    ##  1:            TORNADO        91346         78726
    ##  2:          TSTM WIND         6957          6073
    ##  3:              FLOOD         6789          6493
    ##  4:     EXCESSIVE HEAT         6525          4106
    ##  5:          LIGHTNING         5230          3236
    ##  6:               HEAT         2100            NA
    ##  7:          ICE STORM         1975          1918
    ##  8:        FLASH FLOOD         1777          1331
    ##  9:  THUNDERSTORM WIND         1488            NA
    ## 10:               HAIL         1361          1030
    ## 11: THUNDERSTORM WINDS           NA           908
    ## 12:       WINTER STORM           NA          1176
    ```

3. Overlap between the two: 
    
    ```r
    t(t(overlap))
    ```
    
    ```
    ##      [,1]            
    ## [1,] "TORNADO"       
    ## [2,] "EXCESSIVE HEAT"
    ## [3,] "FLASH FLOOD"   
    ## [4,] "HEAT"          
    ## [5,] "LIGHTNING"     
    ## [6,] "TSTM WIND"     
    ## [7,] "FLOOD"
    ```

The 7 types of events that show up in both top 10 lists, shown above, are responsible for 93% of fatalities and 96% of injuries in each top 10 list. This amounts to 74% of all recorded fatalities and 86% of all recorded injuries in the StormData file. 

The list of top 10 most harmful types of events contains both "HEAT" and "EXCESSIVE HEAT". They are explained in sections 7.20 and 7.12 respectively of the [Storm Data documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf), so there must be some good reason why they are treated separately.

The rankings above include a column for only the last 10 years of data because it might be of interest to see how they change over time. If they do not, then maybe that is an indication that it is hopeless to try and tackle the worst kinds of event types, and disaster preparedness and relief resources might be better used on less severe but more tractable ones. Then again, if the rankings stay the same but the relative share of the damage done by the worst 10 types of events is decreasing, that is an indication of progress.

The 7 types of events that show up in both top 10 lists above are responsible for 94% of fatalities and 95% of injuries in each top 10 list for data collected over the last 10 years only. This amounts to 78% of all recorded fatalities and 88% of all recorded injuries in the last 10 years of the StormData file.

So, the worst events for human health are about the same over time, and their share of the total damage does not seem to change much. The counts for the past 10 years seem to be disproportionately high. That may be accounted for either by population growth (more people in harm's way, so more people truly harmed) or improved data collection (fewer of the people truly harmed are missed by the counts).

Any way you count them, tornadoes are the top threat to human health among all natural events according to StormData, by a wide margin.

## Ranking of event types by economic damage

I added crop and property damage together into a single measure of economic damage, because they are both dollar values rounded to thousands. By this measure, the top 10 most costly types of weather events are (in thousands of dollars):

```r
overlap <- econdmg$EVTYPE
econdmg
```

```
##                 EVTYPE all.econdmg last.10.years
##  1:            TORNADO     3312277       2503623
##  2:        FLASH FLOOD     1599325        659452
##  3:          TSTM WIND     1445168        966963
##  4:               HAIL     1268290        649867
##  5:              FLOOD     1067976        364635
##  6:  THUNDERSTORM WIND      943636            NA
##  7:          LIGHTNING      606932        299102
##  8: THUNDERSTORM WINDS      464978        464978
##  9:          HIGH WIND      342015        142096
## 10:       WINTER STORM      134700            NA
## 11:         HEAVY SNOW          NA         81810
## 12:         HIGH WINDS          NA         57385
```

The types of events ranked above are responsible for 93% of all recorded economic damage in the StormData file. That share is 93% if measured over the last 10 years only. Again the shares are similar because the last 10 years' worth of estimates are disproportionately large, reflecting either higher-value property in harm's way (accounted for by both inflation and true wealth creation) or better data collection. Tornadoes top the list again.
