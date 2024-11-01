#---------------------------------------------------------

# Functions 


#----------------------------------------------------------

# Set Up: 

library(lubridate)
library(tidyverse)
library(stringr)
library(base)
library(scales)

source ("/projectnb/rcsmetrics/_functions/get_helper_function.R")
get_helper_function("get_acct")
get_helper_function("get_nodes")
get_helper_function("get_pidb")

nodes<- get_nodes()
pi_db<-get_pidb()

acct2324<- get_acct(2023,2024) %>% 
  filter(
  year(start_date)>=2023
  ) %>%  
  mutate(
    qname = case_when(
      grepl("-pub", qname) == TRUE ~ str_sub(qname, start=1, end = -5), 
      grepl("-pub", qname)==FALSE ~ qname
    )
  )

acct2223<- get_acct(2022,2023) %>% 
  filter(
    year(start_date)>=2022
  ) %>%  
  mutate(
    qname = case_when(
      grepl("-pub", qname) == TRUE ~ str_sub(qname, start=1, end = -5), 
      grepl("-pub", qname)==FALSE ~ qname
    )
  )

#-------------------------------------------------------------------------------

# Compute Nodes Utilization - Run this function, passing in the accounting file and node 
#  file you are analyzing, and the output is a table with the node (a bit of extra info)
#  and the capacity utilization. 

computeNodeUtilization <- function(acctdf, nodedf){
  nodesCU <- nodedf %>%
    select(host,
           cores) %>%
    mutate(YearlyCoreHours = cores * 8742)
  
  bynode <- acctdf %>%
    select(hostname,
           wallclock) %>%
    group_by(hostname) %>%
    summarise(CoreHours = sum(wallclock)) %>% rename(host = hostname)
  
  nodesCU <- nodesCU %>% left_join(bynode, by = "host") %>% na.omit()
  
  nodesCU <- nodesCU %>% mutate(CU = CoreHours/YearlyCoreHours)
  return(as.data.table(nodesCU))
}

computeNodeUtilization(acct2324, nodes)


#-------------------------------------------------------------------------------

# Total Core Hours by Project Graph - input is the accounting file, output is the total 
#  core hours by project in bar chart form. 

project<- function(df) {
  core<- df %>%  mutate(
    jobHours=wallclock/3600,
    coreHours= jobHours * slots
  )
  core<- core %>%
    group_by(project) %>% 
    summarise(
      totalCoreHours = sum(coreHours)
    )
  ggplot(core, aes(x = project , y = totalCoreHours)) + 
    geom_bar(stat= 'identity', position = 'dodge') + 
    labs( title = "Total Core Hours by Project")
  
  
}

project(acct2324)
#-------------------------------------------------------------------------------

#  Core Hours by Month for Individual Project - input is the project and accounting file,
#  output is a bar chart of the number of core hours per month for this project. 

bymonthProject<- function(proj, df){
  core<- df %>%  mutate(
    jobHours=wallclock/3600,
    coreHours= jobHours * slots
  )
  
  individ<- core %>% filter(project == proj)
  
  individ<- individ %>% group_by(
    month = floor_date(end_date, unit="month")
  )
  
  ggplot(individ, aes(x=month, y = coreHours)) + 
    geom_bar(stat="identity", position = "dodge") + 
    labs(title = "Core Hours per Month for Selected Project", 
         x = "Month", y = "Core Hours")
  
}

bymonthProject("scv", acct2324)

#-------------------------------------------------------------------------------

# Percent of Total Core Hours by Each Project - input the accounting file and output is
#  a table (in descending order) of the number of core hours for each project, as a percentage
#  of the total core hours

percentCoreHours<- function (df){
  df2<- df %>%  mutate(
    jobHours=wallclock/3600,
    coreHours= jobHours * slots
  )
  totalCH<- df2 %>% summarise(total = sum(coreHours))
  
  df2<- df2 %>% group_by(
    project
  ) %>% summarise(
    totalCoreHours = sum(coreHours)
  )
  
  df2<- df2 %>% mutate(
    percentage = totalCoreHours/totalCH$total
  )
  
  return(df2[order(df2$percentage, decreasing = TRUE),])
  
}

percentCoreHours(acct2324)

#-------------------------------------------------------------------------------

# Department specific 

department_percentage <- function(df, department) {
  depart <- df %>%
    left_join(select(pi_db, group, dept, college),
              by = c("project" = "group")) %>% na.omit()
  
  depart<- depart %>% filter(dept == department)
  
  new <- percentCoreHours (depart)
  
  return(new)
}

biostat<- department_percentage(acct2324, "Biostatistics")


#-------------------------------------------------------------------------------

# Job Type Distribution - input is the accounting file and node file, output is an adjusted 
# node dataframe that includes a column describing the type of node 
#-------------------------------------------------------------------------------

# Type of Queue

qlist<- acct2324 %>% select(qname, hostname) %>% unique()

#add information which helps with

qlist<- qlist %>% left_join(select(nodes, host, ib_speed, gpu_type),
                            by = c("hostname" = "host")) %>% unique()

qlist<- qlist %>% select(qname, ib_speed,gpu_type) %>% unique()

qlist<- qlist %>% mutate(
  Type = case_when(
    !is.na(gpu_type)  ~ "GPU",
    ib_speed != 0 ~ "MPI"
  )
) %>% select(qname, Type)

mpiq<- qlist %>% filter(Type == "MPI")

#-------------------------------------------------------------------------------

# Memory Distribution 

#default bins: 0-192,192-384,384 +

memory_distribution <-
  function(acctdf,
           nodedf,
           bins =  c(0, 192, 384, 10000000),
           titles = c("0-192 GB", "192-384 GB", "384 + GB")) {
    nodesMemory <- nodedf %>% select(host, memory)
    jobsMemory <-
      acctdf %>% select(hostname, wallclock) %>% rename(host = hostname) %>%
      group_by(host) %>% summarise(Time = sum(wallclock))
    nodesMemory <-
      nodesMemory %>% left_join(jobsMemory, by = "host") %>% na.omit()
    nodesMemory$category <- cut(nodesMemory$memory,
                                breaks =
                                  bins,
                                labels=
                                  titles,
                                include.lowest = TRUE)
    nodesMemory <-
      nodesMemory %>% group_by(category) %>% summarise(TotalTime = sum(Time))
    nodesMemory <-
      nodesMemory %>% mutate(Percentage = percent((TotalTime / sum(TotalTime)), accuracy = .01))
    ggplot(nodesMemory, aes(x = category, y = TotalTime)) +
      geom_bar(stat = "identity") +
      labs(title = "Memory Distribution", x = "Memory", y = "Total Job Hours") +
      geom_text(aes(label = Percentage), position=position_dodge(width=0.9), vjust=-0.25)
                
  }

memory_distribution(acct2324,nodes)

#-------------------------------------------------------------------------------

# Number of Cores Requested (with adjustable bins) 

#default : 1, 2-7, 8-12, 13-16, 17-28, 29-39, 40-49,50-59,60-69, 70-79, 80-100


cores_requested<- function(df, bins = c(0,1,7,12,16,28,39,99), 
                           titles = c("1","2-8", "9-12","13-16","17-28","29-39", "40-99")){
  slots <- df %>% select(slots, wallclock)
  slots<- slots %>% group_by(slots) %>% 
    summarise(Time = sum(wallclock)) %>% 
    mutate(
      TimeHours = Time/3600
  )
  slots$category<- cut(slots$slots,
                       breaks = 
                         bins,
                       labels = titles, 
                       include.lowest = TRUE)
  slots<- slots %>% group_by (category) %>% summarise(TotalTime = sum(TimeHours))
  slots<- slots %>% mutate(Percentage = percent((TotalTime / sum(TotalTime)), accuracy = .01))
  
  ggplot(slots, aes(x= category, y= TotalTime)) + geom_bar(stat="identity") + 
    labs( title = "Distribution of Number of Cores Requested", 
          x = "Number of Cores Requested",
          y = "Total Hours of Jobs") + 
    geom_text(aes(label = Percentage), position=position_dodge(width=0.9), vjust=-0.25)
}

cores_requested(acct2324)

#-------------------------------------------------------------------------------

# Percent of Time Each Amt of Cores is Requested by Project 

projects <- acct2324 %>% select(owner, project, slots, wallclock)

projects <-
  projects %>% group_by(owner, project, slots) %>% summarise(Time = sum(wallclock),
                                                             .groups = "keep")

projects$category<- cut(projects$slots, c(0,1,8,12,16,28,36,99))

projects<- projects %>% group_by(owner,project,category) %>% summarise(Time=sum(Time), .groups = "keep")


byproject <-
  projects %>% group_by(owner, project) %>% summarise(TotalTime = sum(Time), .groups =
                                                        "keep")
projects<- projects %>% left_join(byproject, by = c("project", "owner"))

projects<- projects %>% mutate(
  Percentage = percent((Time/TotalTime), accuracy = .01)
)

#-------------------------------------------------------------------------------



