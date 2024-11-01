
# Q Type Analysis - Ellis Hamilton 

library(lubridate)
library(tidyverse)
library(stringr)
library(base)
library(scales)
library(plotly)
library(readr)
library(zoo)

source ("/projectnb/rcsmetrics/_functions/get_helper_function.R")
get_helper_function("get_acct")
get_helper_function("get_nodes")
nodes<- get_nodes()

acct2223<- get_acct(2022,2023) %>% filter(year(start_date)>=2022) 
acct2324<- get_acct(2023,2024) %>% filter(
  year(start_date)>=2023
)

acctfull<- get_acct(2000,2024)

#-------------------------------------------------------------------------------

# Identify MPI Queues

# Identify MPI nodes 

identify_mpi <- function() {
  mpi <- nodes %>% filter(ib_speed != 0, type == "MPI")
  
  mpiqs <- acctfull %>% select(qname, hostname) %>% unique()
  
  mpi <- mpi %>% left_join(mpiqs, by = c("host" = "hostname"))
  
  mpi <- mpi %>% select(qname) %>% unique() %>% na.omit()
  
  return(mpi)
  
}

#-------------------------------------------------------------------------------


mpi_utilization <- function(mpi, start="", end="") {
  startYear<- as.numeric(substring(start,6,7))
  startMonth<-as.numeric(substring(start,1,2))
  endYear<-as.numeric(substring(end,6,7))
  endMonth<- as.numeric(substring(end,1,2))
  i.month<-0
  for (i in (startYear:endYear)) {
    if (i == startYear)
      month1 <- startMonth
    else
      month1 <- 1
    if (i == endYear)
      month2 <- endMonth
    else
      month2 <- 12
    for (j in (month1:month2)) {
      year.str <- as.character(i)
      month.str <- as.character(j)
      if (nchar(month.str) == 1)
        month.str = paste("0", month.str, sep = "")
      year.month.str <- paste(year.str, month.str, sep = "")
      message(year.month.str)
      i.month = i.month + 1
      
      
      # file name in Mike's folder
      line <-
        paste("/project/scv/dugan/sge/data/",
              year.month.str,
              ".q",
              sep = "")
      # Open data file and read the data in
      dt <- readLines(line, n = -1)
      
      maxnum <- length(dt)
      na.vector <- vector(mode = "numeric", length = maxnum)
      ina.vector <- vector(mode = "integer", length = maxnum)
      cna.vector <- vector(mode = "character", length = maxnum)
      db <- data.frame(
        time = na.vector,
        queue = cna.vector,
        used = ina.vector,
        avail = ina.vector,
        total = ina.vector,
        stringsAsFactors = FALSE
      )
      
      
      db$time <- as.numeric(substr(dt, 1, 10))
      db$queue <-
        gsub("^\\s+|\\s+$", "", as.character(substr(dt, 12, 44)))
      db$used <- as.integer(substr(dt, 51, 57))
      db$avail <- as.integer(substr(dt, 65, 71))
      db$total <- as.integer(substr(dt, 72, 78))
      
    }
  }
  
  mpicu <- mpi %>% inner_join(db, by = c("qname" = "queue"))
  
  mpicu <- mpicu %>% mutate(CU = used / total)
  
  mpicu <- mpicu %>% group_by(qname) %>% summarise(AvgCU = mean(CU))
  
  return(mpicu)
  
  
}
mpi<-identify_mpi()
mpicu<-mpi_utilization(mpi, "02/2024", "05/2024")
#--------------------------------------------------------------------------------

# Loop through Mike's files and calculate CU for only the MPI's 
exchar<-read_lines("/project/scv/dugan/sge/data/2402.q")
ex <- readLines("/project/scv/dugan/sge/data/2402.q")
maxnum<-length(exchar)
na.vector <- vector(mode="numeric", length=maxnum)
ina.vector <- vector(mode="integer", length=maxnum)
cna.vector <- vector(mode="character", length=maxnum)
db<-data.frame(time=na.vector, 
               queue=cna.vector, 
               used=ina.vector, 
               avail=ina.vector, 
               total=ina.vector, 
               stringsAsFactors = FALSE)


db$time <-as.numeric(substr(exchar,1,10))
db$queue<- trimws(substr(exchar, 12, 44))
# db$queue <- gsub("^\\s+|\\s+$", "", as.character(substr(exchar,12,44)))
db$used <-as.integer(substr(exchar,51,57))
db$avail <-as.integer(substr(exchar,65,71))
db$total <-as.integer(substr(exchar,72,78))

full<- db

ex2 <- readLines("/project/scv/dugan/sge/data/2403.q")
maxnum<-length(ex2)
na.vector <- vector(mode="numeric", length=maxnum)
ina.vector <- vector(mode="integer", length=maxnum)
cna.vector <- vector(mode="character", length=maxnum)
db<-data.frame(time=na.vector, 
               queue=cna.vector, 
               used=ina.vector, 
               avail=ina.vector, 
               total=ina.vector, 
               stringsAsFactors = FALSE)
full<- db %>% full_join(full)

db$time <-as.numeric(substr(ex2,1,10))
db$queue <- gsub("^\\s+|\\s+$", "", as.character(substr(ex2,12,44)))
db$used <-as.integer(substr(ex2,51,57))
db$avail <-as.integer(substr(ex2,65,71))
db$total <-as.integer(substr(ex2,72,78))


db<- db %>% mutate(Date = as_datetime(time)) %>% mutate(Date = floor_date(Date, unit = "day"))

db<- db %>% filter(queue %in% c("a","a128","u","z"))

mpicu<- mpi %>% inner_join(db, by= c("qname" = "queue"))

mpicu<- mpicu %>% mutate(CU = used/total)

mpicu<- mpicu %>% group_by(qname) %>% summarise(AvgCU = mean(CU))

ggplot(mpicu, aes(x=qname, y=AvgCU)) + geom_bar(stat = "identity")+
  labs(title = "Average Capacity Utilization for 02/2024", 
       x = "Queue", y = "Average Capacity Utilization ") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------

#Daily Utilization Function 

daily_utilization<- function(queues=c("a","a128","4","u","z"),start,end){
  startYear<- as.numeric(substring(start,6,7))
  startMonth<-as.numeric(substring(start,1,2))
  endYear<-as.numeric(substring(end,6,7))
  endMonth<- as.numeric(substring(end,1,2))
  i.month<-0
  
  full<- data.frame(time = as.numeric(),
                    queue = as.character(),
                    used = as.numeric(),
                    avail = as.numeric(),
                    total = as.numeric())
  
  # Create skeleton of DB 
  
  # loop to add data to db 
  for (i in (startYear:endYear)) {
    if (i == startYear)
      month1 <- startMonth
    else
      month1 <- 1
    if (i == endYear)
      month2 <- endMonth
    else
      month2 <- 12
    for (j in (month1:month2)) {
      year.str <- as.character(i)
      month.str <- as.character(j)
      if (nchar(month.str) == 1)
        month.str = paste("0", month.str, sep = "")
      year.month.str <- paste(year.str, month.str, sep = "")
      message(year.month.str)
      i.month = i.month + 1
      
      
      # file name in Mike's folder
      line <-
        paste("/project/scv/dugan/sge/data/",
              year.month.str,
              ".q",
              sep = "")
      # Open data file and read the data in
      dt <- readLines(line, n = -1)
      maxnum <- length(dt)
      na.vector <- vector(mode = "numeric", length = maxnum)
      ina.vector <- vector(mode = "integer", length = maxnum)
      cna.vector <- vector(mode = "character", length = maxnum)
      db <- data.frame(
        time = na.vector,
        queue = cna.vector,
        used = ina.vector,
        avail = ina.vector,
        total = ina.vector,
        stringsAsFactors = FALSE
      )
      
      db$time <- as.numeric(substr(dt, 1, 10))
      db$queue <-
        gsub("^\\s+|\\s+$", "", as.character(substr(dt, 12, 44)))
      db$used <- as.integer(substr(dt, 51, 57))
      db$avail <- as.integer(substr(dt, 65, 71))
      db$total <- as.integer(substr(dt, 72, 78))
      
      db<- db %>% filter(queue %in% queues)
      
      full<-db %>% full_join(full)
    }}
  
  
      # filter for just necessary queues 

      full<- full %>% mutate(CU = used/total)

      # Get Date 
      full<- full %>% mutate(Date = as_datetime(time)) %>% mutate(Date = floor_date(Date, unit = "day"))
      
      
      #group by day and average 
      full<- full %>% mutate(queue = case_when (
        (queue == "a" | queue == "a128") ~ "a",
        (!queue == "a" & !queue == "a128" )~ queue
      ))
      full<- full %>% group_by(queue,Date) %>% summarise(DailyUtilization = mean(CU), .groups = "keep")
      full <- full %>% mutate(DailyUtilization = case_when(
        queue == "a" ~ DailyUtilization*2,
        !queue == "a"~ DailyUtilization
      ))
      # then graph by day 
      
      return(ggplot(full, aes(x = Date , y=DailyUtilization, fill = queue)) + geom_line() + geom_point())
  
  
    }
ggplotly(daily_utilization(start="01/2023", end = "03/2023"))


#-------------------------------------------------------------------------------
# Sliding Means 

util_sliding_means<- function(window, startdate, enddate){
  data<- daily_utilization(start = startdate, end = enddate)
  
  data<- data %>% select()
  sliding_means<- rollmean(data$DailyUtilization, window, fill = NA)
  
  return(sliding_means)
}

rolling_mean <- rollmean(daily$DailyUtilization, 3, fill = NA, align = 'right')

daily<-daily %>% filter(queue =="a") %>% select(Date,DailyUtilization)
rollingmean<- rollapply(daily$DailyUtilization, width = 3, by =1, FUN = mean, align = "center", fill = NA, partial = TRUE)
daily<- daily %>% mutate(
  RollingMean = rollapply(DailyUtilization, width = 5, by =1, FUN = mean, align = "center", fill = NA, partial = TRUE))
util_sliding_means(5, "01/2023","03/2023")

mpiutil<-mpiutil %>% mutate(
  RollingMean =rollapply(DailyUtilization, width = 3, by =1, FUN = mean, align = "center", fill = NA, partial = TRUE)
)
