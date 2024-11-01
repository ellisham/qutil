#---------------------------------------------------------------------

# Set up 

#---------------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(stringr)

source ("/projectnb/rcsmetrics/_functions/get_helper_function.R")

get_helper_function("get_acct")

get_helper_function("get_nodes")

get_helper_function("get_pidb")

nodes<- get_nodes()

pi_db<-get_pidb()

acct2324<- get_acct(2023,2024) %>% 
  filter(
    year(start_date)>=2023
  )

acct2324<- acct2324 %>% 
  mutate(
    qname = case_when(
      grepl("-pub", qname) == TRUE ~ str_sub(qname, start=1, end = -5), 
      grepl("-pub", qname)==FALSE ~ qname
    )
  )


acct2324<- acct2324 %>%  mutate(
    jobHours=wallclock/3600,
    coreHours= jobHours * slots
  ) 

byqnamebymonth<-acct2324 %>% 
  group_by(
    month = floor_date(end_date, unit="month"),
    qname
  ) %>% 
  summarise(monthCoreHours=sum(coreHours),
            .groups="keep")

#------------------------------------------------------------------

# Information on Queues via Nodes

#------------------------------------------------------------------

qinfo<- acct2324 %>% 
  select(
    qname,
    hostname, 
    end_date
  ) %>% 
  distinct() %>% 
  rename(host=hostname)

qinfo <- left_join(qinfo, nodes %>% select(host, cores))

qinfo <- qinfo %>% 
  group_by(
    qname, 
    month = floor_date(end_date, unit="month"),
  ) %>% 
  summarise(cores = sum(cores), 
            .groups = "keep")

byqnamebymonth<-left_join(qinfo, byqnamebymonth)

byqnamebymonth<-byqnamebymonth %>% 
  mutate(
    workload=monthCoreHours/cores
  )

                                                                                                                          
#----------------------------------------------------------------------------
# Departmental Analysis 
#----------------------------------------------------------------------------
acct2324<-acct2324 %>% 
  left_join(
      select(pi_db, group,dept,college), 
      by = c("project" = "group")
  )

