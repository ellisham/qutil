library(stringr)
library(Hmisc)
library(colorspace)
library(dygraphs)
library(xts)


argv <- commandArgs(TRUE)
if (length(argv) == 2){
  start.month <-argv[1]
  end.month <-argv[2]
}else if (length(argv) == 1){
  start.month <-argv[1]
  end.month <-paste(substring(Sys.Date(),3,4), 
                    substring(Sys.Date(),6,7),
                    sep="")
}else{
  start.month <-as.character(1404)  # first hosts files in Mike's directory are for 1404
  #start.month <-as.character(1501)
  end.month <-paste(substring(Sys.Date(),3,4), 
                    substring(Sys.Date(),6,7),
                    sep="")
}

#start.month <-as.character(2109)
#end.month <-as.character(1903)

start.year <- as.numeric(substring(start.month,1,2))
end.year <- as.numeric(substring(end.month,1,2))

start.month <- as.numeric(substring(start.month,3,4))
end.month <- as.numeric(substring(end.month,3,4))

time.str <- NULL

queuetable <- read.csv("/projectnb/scv/utilization/katia/queuetable.csv", stringsAsFactors=FALSE)

public <- queuetable[queuetable$class_user =="shared" & queuetable$class_own=="buyin",]
time.str <- NULL

db.daily.list <- NULL
sh.db.daily.list <- NULL
rec.db.daily.list <- NULL
i.month <- 0
dates.days <- numeric(5000 ) # This should be total number of days
i.dates <- 0

buyin.util.i = 0
shared.util.i = 0
all.util.i = 0

buyin.util.accumulate = 0
shared.util.accumulate = 0
all.util.accumulate = 0

rec.monthly.usage <- numeric((end.year - start.year+1) * 12)


for (i in (start.year:end.year)){
  if(i == start.year) month1<-start.month else month1<-1
  if(i == end.year) month2<-end.month else month2<-12
  for(j in (month1:month2)){
    
    
    year.str <- as.character(i)
    month.str <- as.character(j)
    if(nchar(month.str) ==1)month.str=paste("0",month.str,sep="")
    year.month.str <- paste(year.str,month.str,sep="")
    message(year.month.str)
    i.month=i.month + 1
    
    
    # file name in Mike's folder
    df <- paste("/project/scv/dugan/sge/data/",year.month.str,".q",sep="")
    # Open data file and read the data in
    dt <- readLines(df, n=-1)
    
    #each record must have 92 characters
    dt <- dt[nchar(dt)==92]
    
    
    # Process each record
    num<-0
    db.num<-0
    maxnum<-length(dt)
    
    # Create a dataframe to hold the data
    na.vector <- vector(mode="numeric", length=maxnum)
    ina.vector <- vector(mode="integer", length=maxnum)
    cna.vector <- vector(mode="character", length=maxnum)
    db<-data.frame(time=na.vector, 
                   queue=cna.vector, 
                   used=ina.vector, 
                   avail=ina.vector, 
                   total=ina.vector, 
                   stringsAsFactors = FALSE)
    
    
    db$time <-as.numeric(substr(dt,1,10))
    db$queue <- gsub("^\\s+|\\s+$", "", as.character(substr(dt,12,44)))
    db$used <-as.integer(substr(dt,51,57))
    db$avail <-as.integer(substr(dt,65,71))
    db$total <-as.integer(substr(dt,72,78))
    
    
    #remove erronious rows
    db <- db[db$time>1350000000 & db$time<1900000000,]
    
    # Read qhost files (if file does not exist the previous month will be used):
    if (file.exists(paste0("/project/scv/dugan/sge/hosts/",year.month.str,".h")))
      dh <- readLines(paste0("/project/scv/dugan/sge/hosts/",year.month.str,".h"))
    dht <- str_split(dh," ", simplify=TRUE)
    
    # exclude interactive and other weird cases
    dht <- dht[!(dht[,1] %in% c("geo", "scc1","scc2","scc3","scc4","scc5","scc-globus", "tanto", "scc-globus","scc-hadoop")),]
    
    # recalculate the hyperthreaded cores
    hadoop <- grep("scc-q", dht[,1])
    if (length(hadoop) > 0) dht[hadoop,2] <- 8
    ib <- grep("scc-ib", dht[,1])
    if (length(ib) > 0) dht[ib,2] <- 68
    
    dht.shared <- dht[dht[,1] %in% c("scc-aa1","scc-aa2","scc-aa3","scc-aa4","scc-aa5","scc-aa6","scc-aa7","scc-aa8",
                                     "scc-ab1","scc-ab2","scc-ab3","scc-ab4","scc-ab5","scc-ab6","scc-ab7","scc-ab8",
                                     "scc-ac1","scc-ac2","scc-ac3","scc-ac4","scc-ac5","scc-ac6","scc-ac7","scc-ac8",
                                     "scc-ad1","scc-ad2","scc-ad3","scc-ad4","scc-ad5","scc-ad6","scc-ad7","scc-ad8",
                                     "scc-ae1","scc-ae2","scc-ae3","scc-ae4",
                                     "scc-ba1","scc-ba2","scc-ba3","scc-ba4","scc-ba5","scc-ba6","scc-ba7","scc-ba8",
                                     "scc-bb1","scc-bb2","scc-bb3","scc-bb4","scc-bb5","scc-bb6","scc-bb7","scc-bb8",
                                     "scc-bc1","scc-bc2","scc-bc3","scc-bc4",
                                     "scc-bd3","scc-bd4","scc-bd5","scc-bd6","scc-bd7","scc-bd8",
                                     "scc-c01","scc-c02","scc-c06","scc-c07","scc-c08","scc-c09","scc-c10","scc-c11",
                                     "scc-ca1","scc-ca2","scc-ca3","scc-ca4","scc-ca5","scc-ca6","scc-ca7","scc-ca8",
                                     "scc-ga01","scc-ga02","scc-ga03","scc-ga04","scc-ga05","scc-ga06","scc-ga07","scc-ga08","scc-ga09","scc-ga10","scc-ga11",
                                     "scc-ha1","scc-ha2","scc-hb1","scc-hb2","scc-hc1","scc-hc2","scc-hd1","scc-hd2","scc-he1","scc-he2",
                                     "scc-ib1","scc-ib2","scc-ic1","scc-ic2", 
                                     "scc-ja1","scc-ja2","scc-jb1","scc-jb2","scc-jc1","scc-jc2","scc-jd1","scc-jd2","scc-je1","scc-je2",
                                     "scc-pa1","scc-pa2","scc-pa3","scc-pa4","scc-pa5","scc-pa6","scc-pa7","scc-pa8",
                                     "scc-pb1","scc-pb2","scc-pb3","scc-pb4","scc-pb5","scc-pb6","scc-pb7","scc-pb8",
                                     "scc-pc1","scc-pc2","scc-pc3","scc-pc4","scc-pc5","scc-pc6","scc-pc7","scc-pc8",
                                     "scc-pd1","scc-pd2","scc-pd3","scc-pd4","scc-pd5","scc-pd6","scc-pd7","scc-pd8",
                                     "scc-pe1","scc-pe2","scc-pe3","scc-pe4","scc-pe5","scc-pe6","scc-pe7","scc-pe8",
                                     "scc-pf1","scc-pf2","scc-pf3","scc-pf4","scc-pf5","scc-pf6","scc-pf7","scc-pf8",
                                     "scc-pg1","scc-pg2","scc-pg3","scc-pg4","scc-pg5","scc-pg6","scc-pg7","scc-pg8",
                                     "scc-ph1","scc-ph2","scc-ph3","scc-ph4","scc-ph5","scc-ph6","scc-ph7","scc-ph8",
                                     "scc-pi1","scc-pi2","scc-pi3","scc-pi4",
                                     "scc-q01","scc-q02","scc-q03","scc-q04","scc-q05","scc-q06","scc-q07","scc-q08", # hadoop - only 8 of them shared
                                     "scc-ua1","scc-ua2","scc-ua3","scc-ua4","scc-ub1","scc-ub2","scc-ub3","scc-ub4",
                                     "scc-uc1","scc-uc2","scc-uc3","scc-uc4","scc-ud1","scc-ud2","scc-ud3","scc-ud4",
                                     "scc-ue1","scc-ue2","scc-ue3","scc-ue4","scc-uf1","scc-uf2","scc-uf3","scc-uf4",
                                     "scc-ug1","scc-ug2","scc-ug3","scc-ug4","scc-uh1","scc-uh2","scc-uh3","scc-uh4",
                                     "scc-ui1","scc-ui2","scc-ui3","scc-ui4",
                                     "scc-v01","scc-v02","scc-v03", "scc-v04","scc-v05","scc-v06","scc-v07",                                                    #virtualGL nodes
                                     "scc-wa1","scc-wa2","scc-wa3","scc-wa4","scc-wb1","scc-wb2","scc-wb3","scc-wb4",
                                     "scc-wc1","scc-wc2","scc-wc3","scc-wc4","scc-wd1","scc-wd2","scc-wd3","scc-wd4",
                                     "scc-we1","scc-we2","scc-we3","scc-we4","scc-wf1","scc-wf2","scc-wf3","scc-wf4",
                                     "scc-wg1","scc-wg2","scc-wg3","scc-wg4","scc-wh1","scc-wh2","scc-wh3","scc-wh4",
                                     "scc-wi1","scc-wi2","scc-wi3","scc-wi4","scc-wj1","scc-wj2","scc-wj3","scc-wj4",
                                     "scc-wk1","scc-wk2","scc-wk3","scc-wk4","scc-wl1","scc-wl2","scc-wl3","scc-wl4",
                                     "scc-wm1","scc-wm2","scc-wm3","scc-wm4","scc-wn1","scc-wn2","scc-wn3","scc-wn4",
                                     "scc-x05","scc-x06",
                                     "scc-ya1","scc-ya2","scc-ya3","scc-ya4","scc-yb1","scc-yb2","scc-yb3","scc-yb4",
                                     "scc-yc1","scc-yc2","scc-yc3","scc-yc4","scc-yd1","scc-yd2","scc-yd3","scc-yd4",
                                     "scc-ye1","scc-ye2","scc-ye3","scc-ye4","scc-yf1","scc-yf2","scc-yf3","scc-yf4",
                                     "scc-yg1","scc-yg2","scc-yg3","scc-yg4","scc-yh1","scc-yh2","scc-yh3","scc-yh4",
                                     "scc-yi1","scc-yi2","scc-yi3","scc-yi4","scc-yj1","scc-yj2","scc-yj3","scc-yj4",
                                     "scc-yk1","scc-yk2","scc-yk3","scc-yk4", "scc-ym4",
                                     "scc-yp3","scc-yp4","scc-yq1","scc-yq2", "scc-yq3", "scc-yr4",
                                     "scc-za1","scc-za2","scc-za3","scc-za4","scc-zb1","scc-zb2","scc-zb3","scc-zb4",
                                     "scc-zc1","scc-zc2","scc-zc3","scc-zc4","scc-zd1","scc-zd2","scc-zd3","scc-zd4",
                                     "scc-ze1","scc-ze2","scc-ze3","scc-ze4","scc-zf1","scc-zf2","scc-zf3","scc-zf4",
                                     "scc-zg1","scc-zg2","scc-zg3","scc-zg4","scc-zh1","scc-zh2","scc-zh3","scc-zh4",
                                     "scc-zi1","scc-zi2","scc-zi3","scc-zi4","scc-zk3"),]
    
    
    
    # Find total number of slots used on shared nodes
    sh.compute.names      <- unique(na.omit(queuetable$queuetotal[(!is.na(queuetable$class_util) & queuetable$class_util=="compute")]))
    sh.compute.names.used <- unique(na.omit(queuetable$queuename[(!is.na(queuetable$class_util) & queuetable$class_util=="compute")]))
    
    #Mike change b-long, p-long and q-long sometime in October 2017. Before then they were part of b,p and w queue. So we need to exlude them
    if (i < 17 ) sh.compute.names <- sh.compute.names[!sh.compute.names %in% c("b-long","p-long","w-long")]
    if (i == 17 & j < 10 ) sh.compute.names <- sh.compute.names[!sh.compute.names %in% c("b-long","p-long","w-long")]
    
    
    db.sh.total <- subset(db, db$queue %in% sh.compute.names)
    db.sh.used <- subset(db, db$queue %in% sh.compute.names.used)
    
    used <-  tapply(db.sh.used$used,db.sh.used$time,sum)
    total <- tapply(db.sh.total$total,db.sh.total$time,sum)
    utilization <- used/total
    
    
    shared.util.accumulate <- shared.util.accumulate + sum(utilization)
    shared.util.i <- shared.util.i + length(unique(db.sh.total$time))
    
    
    t.time.start <- db$time[1]
    t.time.end <- t.time.start  + 60*60*24
    db.daily <- matrix(nrow=31,ncol=6)
    t.count <- 0
    
    
    # these commands superseed the following loop
    db$date <- as.Date(as.POSIXct(db$time, origin="1970-01-01"))
    db.sh.total$date <- as.Date(as.POSIXct(db.sh.total$time, origin="1970-01-01"))
    db.sh.used$date <- as.Date(as.POSIXct(db.sh.used$time, origin="1970-01-01"))
    
    sh.util.df <- data.frame(utilization=utilization, time = unique(db.sh.total$time), date = as.Date(as.POSIXct(unique(db.sh.total$time), origin="1970-01-01")), 
                             stringsAsFactors=FALSE)
    sh.util.df$week <- strftime(sh.util.df$date, format="%Y-%V")
    
    # daily averages
    sh.daily.util.df <- data.frame(average = tapply(sh.util.df$utilization, sh.util.df$date, mean), 
                                   min = tapply(sh.util.df$utilization, sh.util.df$date, min),
                                   max = tapply(sh.util.df$utilization, sh.util.df$date, max))
    
    # weekly averages
    sh.weekly.util.df <- data.frame(average = tapply(sh.util.df$utilization, sh.util.df$week, mean), 
                                    min = tapply(sh.util.df$utilization, sh.util.df$week, min),
                                    max = tapply(sh.util.df$utilization, sh.util.df$week, max))
    
    
    
    
    while (t.time.start <= max(db$time)){
      
      i.dates<- i.dates+1
      dates.days[i.dates] <- t.time.start + 60*60*12
      
      # get utilization per day
      t.count<-t.count+1
      db.daily[t.count,] <- c(min(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              mean(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              max(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              sum(as.numeric(dht.shared[,2])),
                              #max(total[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              t.time.start,as.numeric(year.month.str))
      
      t.time.start <- t.time.end
      t.time.end <- t.time.end  + 60*60*24
    }
    db.daily <- db.daily[1:t.count,]
    sh.db.daily.list <- rbind(sh.db.daily.list,db.daily)
    
    #============ all: compute, buy-in, special, no interactive ============
    
    all.names <- unique(na.omit(queuetable$queuetotal[(!is.na(queuetable$class_util)) & (queuetable$class_util=="compute" | queuetable$class_util=="buyin") ]))
    all.names.used <- unique(na.omit(queuetable$queuename[(!is.na(queuetable$class_util)) & (queuetable$class_util=="compute" | queuetable$class_util=="buyin")]))
    
    all.total <- subset(db, db$queue %in% all.names)
    all.used <- subset(db, db$queue %in% all.names.used)
    
    used <-  tapply(all.used$used,all.used$time,sum)
    total <- tapply(all.total$total,all.total$time,sum)
    utilization <- used/total
    
    
    all.util.accumulate <- all.util.accumulate + sum(utilization)
    all.util.i <- all.util.i + length(unique(all.total$time))
    
    t.time.start <- db$time[1]
    t.time.end <- t.time.start  + 60*60*24
    db.daily <- matrix(nrow=31,ncol=6)
    t.count <- 0
    while (t.time.start <= max(db$time)){
      
      # get utilization per day
      t.count<-t.count+1
      db.daily[t.count,] <- c(min(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              mean(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              max(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              sum(as.numeric(dht[,2])),
                              #max(total[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              t.time.start,as.numeric(year.month.str))
      
      t.time.start <- t.time.end
      t.time.end <- t.time.end  + 60*60*24
    }
    db.daily <- db.daily[1:t.count,]
    db.daily.list <- rbind(db.daily.list,db.daily)
    
    
    #============ buy-in only ============
    
    buyin.names <- unique(na.omit(queuetable$queuetotal[(!is.na(queuetable$class_util)) & (queuetable$class_util=="buyin") ]))
    buyin.names.used <- unique(na.omit(queuetable$queuename[(!is.na(queuetable$class_util)) & (queuetable$class_util=="buyin")]))
    
    buyin.total <- subset(db, db$queue %in% buyin.names)
    buyin.used <- subset(db, db$queue %in% buyin.names.used)
    
    used <-  tapply(buyin.used$used,buyin.used$time,sum)
    total <- tapply(buyin.total$total,buyin.total$time,sum)
    utilization <- used/total
    
    buyin.util.accumulate <- buyin.util.accumulate + sum(utilization)
    buyin.util.i <- buyin.util.i + length(unique(buyin.total$time))
    
    #------ time stamp ---------------
    
    #create a string with a month name and year
    time.str <- c(time.str,paste(j,paste("20",i,sep=""),sep="/"))    
    
    
    
    #============ reclaimed resource utilization ============
    
    rec.names <- unique(na.omit(queuetable$queuetotal[(!is.na(queuetable$class_util)) & (queuetable$class_user=="shared" & queuetable$class_own=="buyin") ]))
    rec.names.used <- unique(na.omit(queuetable$queuename[(!is.na(queuetable$class_util)) & (queuetable$class_user=="shared" & queuetable$class_own=="buyin")]))
    
    rec.total <- subset(db, db$queue %in% rec.names)
    rec.used <- subset(db, db$queue %in% rec.names.used)
    
    used <-  tapply(rec.used$used,rec.used$time,sum)
    total <- tapply(rec.total$total,rec.total$time,sum)
    utilization <- used/total
    rec.monthly.usage[i.month] = sum(used)/3600*5
    
    
    t.time.start <- db$time[1]
    t.time.end <- t.time.start  + 60*60*24
    db.daily <- matrix(nrow=31,ncol=6)
    t.count <- 0
    while (t.time.start <= max(db$time)){
      
      # get utilization per day
      t.count<-t.count+1
      db.daily[t.count,] <- c(min(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              mean(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              max(utilization[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              sum(total),
                              #max(total[unique(db$time)>=t.time.start & unique(db$time)<t.time.end]),
                              t.time.start,
                              as.numeric(year.month.str))
      
      t.time.start <- t.time.end
      t.time.end <- t.time.end  + 60*60*24
    }
    db.daily <- db.daily[1:t.count,]
    rec.db.daily.list <- rbind(rec.db.daily.list,db.daily)
    
    
  }
  
}

#output overall utilization
message(paste0("Shared compute nodes utilization: ", format(round(shared.util.accumulate/shared.util.i*100,2),nsmall=2),"%"))
message(paste0("Buy-in compute nodes utilization: ", format(round(buyin.util.accumulate/buyin.util.i*100,2),nsmall=2),"%"))
message(paste0("All compute nodes utilization: ", format(round(all.util.accumulate/all.util.i*100,2),nsmall=2),"%"))

for (i in (1+3): (nrow(db.daily.list)-3) ) {
  if (db.daily.list[i,4] > db.daily.list[i-3,4] &&
      db.daily.list[i,4] > db.daily.list[i+3,4] ) db.daily.list[i,4] = db.daily.list[i-1,4]
}

dates.days <- dates.days[1:nrow(sh.db.daily.list)]
dates.days <- as.Date(as.POSIXct(dates.days, origin="1970-01-01"))
shared.curve <- xts(sh.db.daily.list[,2]*100, dates.days)
lwr.curve <- xts(sh.db.daily.list[,1]*100, dates.days)
upr.curve <- xts(sh.db.daily.list[,3]*100, dates.days)
all.curve <- xts(db.daily.list[,2]*100, dates.days)
cores.curve <- xts(db.daily.list[,4], dates.days)
sh.cores.curve <- xts(sh.db.daily.list[,4], dates.days)
time.curve <- cbind( shared.curve, lwr.curve, upr.curve, all.curve, cores.curve,sh.cores.curve )
names(time.curve) <- c("shared","lwr","upr","all","ncores","shncores")

dygraph(time.curve, main="SCC Compute Nodes Utilization (daily averages)")%>%
  dyAxis("y", label="Utilization (%)", valueRange=c(0,120)) %>%
  dyAxis("y2", label="Number of cores", valueRange=c(0,27000)) %>%
  dySeries(c("lwr","shared","upr"), label = "Shared compute utilization ", col='green', strokeWidth=NULL, strokePattern=NULL) %>%
  dySeries("all", label = "All  compute utilization", col='cornflowerblue', strokeWidth=NULL, strokePattern=NULL) %>%
  dySeries("ncores", label = "Total compute cores ", axis = 'y2', col='red', strokeWidth=2, strokePattern="dashed") %>%
  dySeries("shncores", label = "Shared Compute cores", axis = 'y2', col='black', strokeWidth=2, strokePattern="dashed") %>%
  dyEvent("2015-05-18", "Downtime 2015", labelLoc="bottom") %>%
  dyEvent("2016-05-16", "Downtime 2016", labelLoc="bottom") %>%
  dyEvent("2017-05-22", "Downtime 2017", labelLoc="bottom") %>%
  dyEvent("2018-05-22", "Downtime 2018", labelLoc="bottom") %>%
  dyEvent("2019-06-11", "Downtime 2019", labelLoc="bottom") %>%
  dyOptions(stackedGraph = FALSE, fillGraph=FALSE, stepPlot=FALSE,fillAlpha=0.3) %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE, width = 250, labelsSeparateLines = TRUE) %>%
  dyLimit(100, color="black", strokePattern = "dotdash") %>%
  dyRangeSelector() 


# All resources
lwr.curve <- xts(db.daily.list[,1]*100, dates.days)
upr.curve <- xts(db.daily.list[,3]*100, dates.days)
all.curve <- xts(db.daily.list[,2]*100, dates.days)
cores.curve <- xts(db.daily.list[,4], dates.days)
time.curve <- cbind(lwr.curve, upr.curve, all.curve, cores.curve )
names(time.curve) <- c("lwr","upr","all","ncores")

dygraph(time.curve, main="SCC Compute (shared and buyin) Nodes Utilization (daily averages)")%>%
  dyAxis("y", label="Utilization (%)", valueRange=c(0,120)) %>%
  dyAxis("y2", label="Number of cores", valueRange=c(0,30000)) %>%
  dySeries(c("lwr","all","upr"), label = "All compute utilization ", col='cornflowerblue', strokeWidth=NULL, strokePattern=NULL) %>%
  dySeries("ncores", label = "Total compute cores ", axis = 'y2', col='red', strokeWidth=2, strokePattern="dashed") %>%
  dyEvent("2015-05-18", "Downtime 2015", labelLoc="bottom") %>%
  dyEvent("2016-05-16", "Downtime 2016", labelLoc="bottom") %>%
  dyEvent("2017-05-22", "Downtime 2017", labelLoc="bottom") %>%
  dyOptions(stackedGraph = FALSE, fillGraph=FALSE, stepPlot=FALSE,fillAlpha=0.4) %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE, width = 250, labelsSeparateLines = TRUE) %>%
  dyLimit(100, color="black", strokePattern = "dotdash") %>%
  dyRangeSelector() 


# Shared resources
dates.days <- dates.days[1:nrow(sh.db.daily.list)]
dates.days <- as.Date(as.POSIXct(dates.days, origin="1970-01-01"))
shared.curve <- xts(sh.db.daily.list[,2]*100, dates.days)
lwr.curve <- xts(sh.db.daily.list[,1]*100, dates.days)
upr.curve <- xts(sh.db.daily.list[,3]*100, dates.days)
sh.cores.curve <- xts(sh.db.daily.list[,4], dates.days)
time.curve <- cbind( shared.curve, lwr.curve, upr.curve, sh.cores.curve )
names(time.curve) <- c("shared","lwr","upr","shncores")

dygraph(time.curve, main="SCC Shared Compute Nodes Utilization (daily averages)")%>%
  dyAxis("y", label="Utilization (%)", valueRange=c(0,120)) %>%
  dyAxis("y2", label="Number of cores", valueRange=c(0,24000)) %>%
  dySeries(c("lwr","shared","upr"), label = "Shared compute utilization ", col='green', strokeWidth=NULL, strokePattern=NULL) %>%
  dySeries("shncores", label = "Shared Compute cores", axis = 'y2', col='black', strokeWidth=2, strokePattern="dashed") %>%
  dyEvent("2015-05-18", "Downtime 2015", labelLoc="bottom") %>%
  dyEvent("2016-05-16", "Downtime 2016", labelLoc="bottom") %>%
  dyEvent("2017-05-22", "Downtime 2017", labelLoc="bottom") %>%
  dyOptions(stackedGraph = FALSE, fillGraph=FALSE, stepPlot=FALSE,fillAlpha=0.3) %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE, width = 250, labelsSeparateLines = TRUE) %>%
  dyLimit(100, color="black", strokePattern = "dotdash") %>%
  dyRangeSelector() 


# # full version with 2 bands (does not look good)
# dates.days <- dates.days[1:nrow(sh.db.daily.list)]
# dates.days <- as.Date(as.POSIXct(dates.days, origin="1970-01-01"))
# shared.curve <- xts(sh.db.daily.list[,2]*100, dates.days)
# lwr.curve <- xts(sh.db.daily.list[,1]*100, dates.days)
# upr.curve <- xts(sh.db.daily.list[,3]*100, dates.days)
# all.curve <- xts(db.daily.list[,2]*100, dates.days)
# all.lwr.curve <- xts(db.daily.list[,1]*100, dates.days)
# all.upr.curve <- xts(db.daily.list[,3]*100, dates.days)
# cores.curve <- xts(db.daily.list[,4], dates.days)
# sh.cores.curve <- xts(sh.db.daily.list[,4], dates.days)
# time.curve <- cbind( shared.curve, lwr.curve, upr.curve, all.curve, all.lwr.curve, all.upr.curve, cores.curve,sh.cores.curve )
# names(time.curve) <- c("shared","lwr","upr","all","alwr","aupr","ncores","shncores")
# 
# dygraph(time.curve, main="SCC Compute Nodes Utilization (daily averages)")%>%
#   dyAxis("y", label="Utilization (%)", valueRange=c(0,120)) %>%
#   dyAxis("y2", label="Number of cores", valueRange=c(0,24000)) %>%
#   dySeries(c("lwr","shared","upr"), label = "Shared compute utilization ", col='green', strokeWidth=NULL, strokePattern=NULL) %>%
#   dySeries(c("alwr","all","aupr"), label = "All  compute utilization", col='cornflowerblue', strokeWidth=NULL, strokePattern=NULL) %>%
#   dySeries("ncores", label = "Total compute cores ", axis = 'y2', col='red', strokeWidth=2, strokePattern="dashed") %>%
#   dySeries("shncores", label = "Shared Compute cores", axis = 'y2', col='black', strokeWidth=2, strokePattern="dashed") %>%
#   dyEvent("2015-05-18", "Downtime 2015", labelLoc="bottom") %>%
#   dyEvent("2016-05-16", "Downtime 2016", labelLoc="bottom") %>%
#   dyEvent("2017-05-22", "Downtime 2017", labelLoc="bottom") %>%
#   dyOptions(stackedGraph = FALSE, fillGraph=FALSE, stepPlot=FALSE,fillAlpha=0.3) %>%
#   dyLegend(show = "always", hideOnMouseOut = FALSE, width = 250, labelsSeparateLines = TRUE) %>%
#   dyLimit(100, color="black", strokePattern = "dotdash") %>%
#   dyRangeSelector() 

rec.monthly.usage <- rec.monthly.usage[1:49]
recl.ts <- ts(rec.monthly.usage, frequency=12, start=c(2014,4))
plot(recl.ts, type="h", at="pretty")


library(lubridate)
graph_data <- xts(x = rec.monthly.usage, 
                  order.by = seq(ymd("2014-04-01"),ymd("2018-04-02"), by='months'))
names(graph_data) <- "value"

dygraph(graph_data) %>%
  dyOptions(useDataTimezone = TRUE, plotter = 
              "function barChartPlotter(e) {
            var ctx = e.drawingContext;
            var points = e.points;
            var y_bottom = e.dygraph.toDomYCoord(0);  // see     http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord
            
            // This should really be based on the minimum gap
            var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx);
            ctx.fillStyle = e.color;
            
            // Do the actual plotting.
            for (var i = 0; i < points.length; i++) {
            var p = points[i];
            var center_x = p.canvasx;  // center of the bar
            
            ctx.fillRect(center_x - bar_width / 2, p.canvasy,
            bar_width, y_bottom - p.canvasy);
            ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
            bar_width, y_bottom - p.canvasy);
            }
            }")