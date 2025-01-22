# H Analysis
library(readr)
library(tidyverse)
library(lubridate)
library(plotly)
setwd("/projectnb/rcs-intern/ellis/qutil")
hdata <- read_csv("hfiles.csv")

# H Analysis ------------
colnames(hdata) <- c("Time",
                     "Node",
                     "TotalCores",
                     "MemTotal",
                     "MemUsed",
                     "Queue",
                     "CoresinUse",
                     "Drained")
hdata$Time <- as_datetime(hdata$Time)
hdata$Drained <-
  case_when(!is.na(hdata$Drained) ~ 1, is.na(hdata$Drained) ~ 0)

#combine a and a128
hdata <- hdata %>% 
  mutate(
    Queue = case_when(
      Queue == "a128" ~ "a",
      TRUE ~ Queue
    )
  )
databyQueues <-
  hdata %>% group_by(Queue) %>% summarise(TotalCores = sum(TotalCores),
                                          TotalinUse = sum(CoresinUse))


# MPI Analysis -----------------------------------------------------------------

mpi <- hdata %>% filter(Queue %in% c("a", "a128", "4", "u", "z"))
mpi <- subset(mpi, is.na(mpi$Drained))
mpi <-
  mpi %>% group_by(Time, Queue) %>% summarise(
    TotalCores = sum(TotalCores),
    TotalinUse = sum(CoresinUse),
    TotalNodes = n(),
    .groups = "keep"
  )
mpi$Time <- as_datetime(mpi$Time)
mpi <- mpi %>% mutate(Usage = TotalinUse / TotalCores)

ggplot(mpi, aes(x = Time, color = Queue)) +
  geom_line(aes(y = Usage)) +
  geom_line(aes(y = TotalNodes))
scale_y_continuous(limits = c(0, 1),
                   sec.axis = sec_axis( ~ . * 36,
                                        name = "Y2 (0 to 36)",
                                        breaks = seq(0, 36, by = 6))) +
  scale_color_manual(name = "Queue",
                     values = c("blue", "red")) +
  theme_minimal()

ggplot(mpi, aes(x = Time, y = Usage, color = Queue)) +
  geom_line() +
  facet_grid(rows = vars(Queue))

#Function-----------------------------------------------------------------------

# Function to Call Perl Script and Then Create Merged DataFrame 

merged<-function(begMonth, endMonth){
  command<- "perl /projectnb/rcs-intern/ellis/qutil/hanalysis.pl"
  begDate<- as.Date(paste("01/", begMonth), "%d/%m/%Y")
  endDate<-as.Date(paste("01/",endMonth), "%d/%m/%Y")
  monthSeq <- seq(begDate, endDate, by = "month")
  monthSeq<-as.Date(monthSeq)
  df<-data.frame()
  for (i in 1:length(monthSeq)) {
  date<- monthSeq[i]
  date<-format(date, "%y%m")
  commandFull<- paste(command," ", date,".h", sep="")
  print(commandFull)
  system(commandFull)
  output_file <- paste0(date, ".h.csv")
  
  # Check if the file exists before trying to read it
  if (file.exists(output_file)) {
    # Read the output CSV file
    exported_df <- read.csv(output_file, header = TRUE)
    colnames(exported_df)<- c("Time",
                              "Node",
                              "TotalCores",
                              "MemTotal",
                              "MemUsed",
                              "Queue",
                              "CoresinUse",
                              "Drained")
    df <- df %>% bind_rows(exported_df)
  } else {
    warning(paste("File", output_file, "not found. Skipping."))
  }
  }
  return(df)
}

funcdata<-merged("01/2024","02/2024")


# Write a function that takes data and queue name as input and generates utilization 

hfileUtil<-function(data, queueName){
  # Check if 'a' queue and adjust dataframe if so 
  if (queueName %in% c("a", "a128")){
    df<-data %>% filter(Queue %in% c("a","a128"))
    df<- df %>% mutate(Queue = case_when (
      (Queue == "a" | Queue == "a128") ~ "a",
      (!Queue == "a" & !Queue == "a128" )~ Queue
    ))
  } else {
  df<-data %>% filter(Queue == queueName)
  }
  #Calculate Utilization
  df<- df %>% group_by(Queue) %>% summarise(totalTotal = sum(TotalCores),
                                                totalInUse=sum(CoresinUse))
  utilization<-df$totalInUse/df$totalTotal
  # Adjust A Queue Utilization
  if (queueName %in% c("a","a128")){
    utilization<- utilization*2
  }
  return(utilization)
}

# write a function that takes data and queue name as input and generates utilization graph 

graphQueueUtil<-function(data, queueName){
  #Check if A Queue and adjust dataframe/ calculate utilization appropriately
  if (queueName %in% c("a", "a128")){
    df<-data %>% filter(Queue %in% c("a","a128"))
    df<- df %>% mutate(Queue = case_when (
      (Queue == "a" | Queue == "a128") ~ "a",
      (!Queue == "a" & !Queue == "a128" )~ Queue
    ))
  df <- df %>% group_by(Time) %>% summarise(totalTotal = sum(TotalCores),
                                              totalInUse = sum(CoresinUse))
  df<-df %>% mutate(CU = (totalInUse/totalTotal)*2)
  } else {
    df<-data %>% filter(Queue == queueName)
    df<-df %>% group_by(Time) %>% summarise(totalTotal = sum(TotalCores),
                                              totalInUse = sum(CoresinUse))
    df<-df %>% mutate(CU = totalInUse/totalTotal)
  }
  # Group by Day 
  df<- df %>% mutate(Date = as_datetime(Time)) %>% mutate(Date = floor_date(Date, unit = "day"))
  df<- df %>% group_by(Date) %>% summarise(DailyUtilization = mean(CU))
  
  # Graph 
  ggplotly(ggplot(df, aes(x = Date, y = DailyUtilization)) +
  geom_line(aes(y = DailyUtilization), color = "red") + geom_point() +
  ylim(0,1))
  }

