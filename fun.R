fun <- function(){
    library(reshape2)
    library(plyr)
    Sys.setlocale("LC_ALL", "C")
    
    stormdata<-read.csv(bzfile("repdata-data-StormData.csv.bz2"), sep=",", stringsAsFactors=FALSE)
    
    ## clean EVTYPE        
    stormdata$cate <- toupper(stormdata$EVTYPE)
    stormdata$cate[grepl("^( *)", stormdata$cate)] <- (gsub("^( *)", "", stormdata$cate[grepl("^( *)", stormdata$cate)]))
    stormdata$cate[grepl(" +", stormdata$cate)] <- (gsub(" +", " ", stormdata$cate[grepl(" +", stormdata$cate)]))
    stormdata$cate[grepl("S+\\b", stormdata$cate)] <- gsub("S+\\b", "", stormdata$cate[grepl("S+\\b", stormdata$cate)])
    stormdata$cate[grepl(" AND$", stormdata$cate)] <- (gsub(" AND$", "", stormdata$cate[grepl(" AND$", stormdata$cate)]))
        
    stormdata$cate[grepl("^(SUMMARY|Summary)", stormdata$cate)] <- "SUMMARY"
    
    stormdata$cate[grepl("(SMALL|DEEP|NON SEVERE|LATE SEASON) HAIL", stormdata$cate)] <- "HAIL"
    stormdata$cate[grepl("HAIL (DAMAGE|ALOFT|[0-9])", stormdata$cate)] <- "HAIL"
    
    stormdata$cate[grepl("^TORNADO(E)?( DEBRI)?( F[0-9])?$", stormdata$cate)] <- "TORNADO"
            
    stormdata$cate[grepl("TSTM", stormdata$cate)] <- (gsub("TSTM", "THUNDERSTORM", stormdata$cate[grepl("TSTM", stormdata$cate)]))
    stormdata$cate[grepl("^THUNDERSTORM(W)? (WIND)?(S)? ?\\(?G?[0-9]", stormdata$cate)] <- "THUNDERSTORM WIND"
    stormdata$cate[grepl("^(SEVERE |GUSTY )?THUNDERSTORM(W)? ?(W)? ?(I)?(N)?(D)? ?(.)?(G)?( DAMAGE| TO| LE CEN)?$", stormdata$cate)] <- "THUNDERSTORM WIND"
    stormdata$cate[grepl("^THUNDERSTORM WIND(/)? ?(TREE)?$", stormdata$cate)] <- "THUNDERSTORM WIND"
    
    stormdata$cate[grepl("^HIGH WIND [0-9]", stormdata$cate)] <- "HIGH WIND"
    stormdata$cate[grepl("^HIGH WIND(/)?$", stormdata$cate)] <- "HIGH WIND"
    stormdata$cate[grepl("^HIGH WIND \\(.*\\)", stormdata$cate)] <- "HIGH WIND"
    
    stormdata$cate[grepl("FLOODING|FLOODIN", stormdata$cate)] <- (gsub("FLOODING|FLOODIN", "FLOOD", stormdata$cate[grepl("FLOODING|FLOODIN", stormdata$cate)]))
    stormdata$cate[grepl("^(RURAL|MINOR|MAJOR|SNOWMELT|HIGHWAY|STREET|RIVER|LOCAL) FLOOD$", stormdata$cate)] <- "FLOOD"
    stormdata$cate[grepl("^(URBAN|SMALL STREAM|RIVER)?.?( AND )?(SMALL)? ?(URBAN|STREAM|STREET)? ?FLOOD$", stormdata$cate)] <- "FLOOD"
    stormdata$cate[grepl("^FLOOD( WATCH)?/(RIVER FLOOD)?$", stormdata$cate)] <- "FLOOD"
    
    stormdata$cate[grepl("^LAKE FLOOD$", stormdata$cate)] <- "LAKESHORE FLOOD"
    
    stormdata$cate[grepl("^COASTAL( *)FLOOD?$", stormdata$cate)] <- "COASTAL FLOOD"
    stormdata$cate[grepl("^BEACH FLOOD?$", stormdata$cate)] <- "COASTAL FLOOD"
    
    stormdata$cate[grepl("^(LOCAL )?FLASH FLOOD(/)( STREET)?$", stormdata$cate)] <- "FLASH FLOOD"
    stormdata$cate[grepl("^FLOOD( |/)FLASH$", stormdata$cate)] <- "FLASH FLOOD"
    stormdata$cate[grepl("^ICE JAM FLOOD (\\(MINOR)?$", stormdata$cate)] <- "FLASH FLOOD"
    stormdata$cate[grepl("^FLASH FLOOD FROM ICE JAM$", stormdata$cate)] <- "FLASH FLOOD"
    
    stormdata$cate[grepl("^LIGHT(N)?(ING)?(\\.)? ?(DAMAGE|INJURY|FIRE)?$", stormdata$cate)] <- "LIGHTNING"
    
    stormdata$cate[grepl("HURRICANE|TYPHOON", stormdata$cate)] <- "HURRICANE(TYPHOON)"
    
    stormdata$cate[grepl("^LANDSLIDE|LANDSLUMP", stormdata$cate)] <- "DEBRIS FLOW"
    
    stormdata$cate[grepl("EXTREME COLD", stormdata$cate)] <- "EXTREME COLD/WIND CHILL"
    
    stormdata$cate[grepl("^(PATCHY )?(DENSE )?FOG$", stormdata$cate)] <- "DENSE FOG"
    
    stormdata$cate[grepl("^ICE FOG$", stormdata$cate)] <- "FREEZING FOG"
    
    events <- unique(stormdata$cate)
    
    events
    
    #ASSESSMENT FOR Q1    
    stormdata$H_INDEX <- stormdata$FATALITIES+0.5*stormdata$INJURIES
        
    melt.data<-melt(stormdata, id=c("REFNUM", "cate"), measure.vars=c("FATALITIES", "INJURIES", "H_INDEX"))
    melt.data$value<-as.integer(melt.data$value)
    data.fatal<-dcast(melt.data, cate ~ variable, sum)
    
    head(arrange(data.fatal, H_INDEX, decreasing=TRUE),20)
    
    #ASSESSMENT FOR Q2
    if (stormdata$PROPDMDEXP == K)
}