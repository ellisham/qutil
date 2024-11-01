# MPI Queue Analysis - Final 

library(lubridate)
library(tidyverse)
library(stringr)
library(base)
library(scales)
library(plotly)
library(profvis)
library(zoo)
library(htmlwidgets)

daily_utilization<- function(queues=c("a","a128","4","u","z"),start,end){
  # initialize parameters 
  startYear<- as.numeric(substring(start,6,7))
  startMonth<-as.numeric(substring(start,1,2))
  endYear<-as.numeric(substring(end,6,7))
  endMonth<- as.numeric(substring(end,1,2))
  i.month<-0
  # create empty dataframes for all to be added to 
  full<- data.frame(time = as.numeric(),
                    queue = as.character(),
                    used = as.numeric(),
                    avail = as.numeric(),
                    total = as.numeric())
  na.vector <- vector(mode = "numeric", length = 1)
  ina.vector <- vector(mode = "integer", length = 1)
  cna.vector <- vector(mode = "character", length = 1)
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
      dt <- read_lines(line, n_max = -1)
      maxnum <- length(dt)
      length(na.vector) <- maxnum
      length(ina.vector) <- maxnum
      length(cna.vector) <- maxnum
      db <- data.frame(
        time = na.vector,
        queue = cna.vector,
        used = ina.vector,
        avail = ina.vector,
        total = ina.vector,
        stringsAsFactors = FALSE
      )
      # add data to empty dataframe db
      db$time <- as.numeric(substr(dt, 1, 10))
      db$queue<- trimws(substr(dt, 12, 44))
      db$used <- as.integer(substr(dt, 51, 57))
      db$avail <- as.integer(substr(dt, 65, 71))
      db$total <- as.integer(substr(dt, 72, 78))
      
      # filter for necessary queues 
      db<- db %>% filter(queue %in% queues)
      # add this month to the full dataframe of months 
      full<-db %>% full_join(full)
    }}
  
  # calcualte utilization 
  full<- full %>% mutate(CU = used/total)
  
  # Get Date 
  full<- full %>% mutate(Date = as_datetime(time)) %>% mutate(Date = floor_date(Date, unit = "day"))
  
  #group by day and average 
  # also, correct for a queues 
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
  
  return(full)
  
}


daily<- daily_utilization(start = "01/2023", end = "03/2023")

daily<-daily %>% select(queue,DailyUtilization, Date)
ggdaily <-
  ggplot(daily, aes(x = Date , y = DailyUtilization, color = queue)) +
  geom_line(
    aes(y = rollingmean),
    color = "blue",
    linewidth = 1.2,
    linetype = "dashed",
    na.rm = TRUE
  ) +
  geom_line(aes(y = DailyUtilization), color = "red") + geom_point() +
  ylim(0,1)

profvis(daily_utilization(start="01/2023",end="02/2023"))
ggdaily<- ggplotly(ggdaily) %>% 
  layout( list(
    y = 0.8,
    buttons = list(
      list(
        args = list("type", "scatter"),
        label = "Scatter",
        method = "restyle"
      ))))
      
# Save at HTML 
saveWidget(ggdaily, file = "dailyutilizationfig.html", selfcontained = TRUE)



adebug<- daily_utilization(queues = c("a", "a128"), start= "01/2024", end = "08/2024")
adebugroll<- rollapply(adebug$DailyUtilization, width = 3, by =1, FUN = mean, align = "center", fill = NA, partial = TRUE)
adebug$roll<-adebugroll






