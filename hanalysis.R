# H Analysis 
library(readr)
library(tidyverse)
library(lubridate)
setwd("~/Perl")
hdata<- read_csv("hfiles.csv")
colnames(hdata)<- c("Time", "Node", "TotalCores",
                    "MemTotal", "MemUsed","Queue", "CoresinUse", "Drained")
hdata$Time<- as_datetime(hdata$Time)
hdata$Drained<- case_when(!is.na(hdata$Drained) ~ 1, is.na(hdata$Drained) ~ 0)

databyQueues<- hdata %>% group_by(Queue) %>% summarise(TotalCores = sum(TotalCores),
                                                       TotalinUse = sum(CoresinUse))


# MPI Analysis -----------------------------------------------------------------

mpi<-hdata %>% filter(Queue %in% c("a","a128", "4", "u", "z"))
mpi <- subset(mpi, is.na(mpi$Drained))
mpi<- mpi %>% group_by(Time, Queue) %>% summarise(TotalCores = sum(TotalCores),
                                                  TotalinUse = sum(CoresinUse), 
                                                  TotalNodes= n(),
                                                  .groups="keep")
mpi$Time<- as_datetime(mpi$Time)
mpi<-mpi %>% mutate(Usage = TotalinUse/TotalCores)

ggplot(mpi, aes(x = Time, color = Queue)) + 
  geom_line(aes(y = Usage)) +
  geom_line(aes(y=TotalNodes))
  scale_y_continuous(
    limits = c(0, 1),                           
    sec.axis = sec_axis(~ . * 36,
                        name = "Y2 (0 to 36)", 
                        breaks = seq(0, 36, by = 6))
  ) + 
  scale_color_manual(
    name = "Queue",                           
    values = c("blue", "red")
  ) +
  theme_minimal()
