# Exporting of Queues 


rollingQueueGraphs<- function(selected,startMonth = "01/2024",endMonth = "08/2024"){
  db<-daily_utilization(queues = c(selected), start = startMonth, end = endMonth)
  
  rolling<- rollapply(db$DailyUtilization, width = 3, by =1, FUN = mean, align = "center", fill = NA, partial = TRUE)
  
  gg<- ggplot(db, aes(x = as.Date(Date) , y = DailyUtilization, color = queue)) +
    geom_line(
      aes(y = rolling),
      color = "blue",
      linewidth = 1,
      na.rm = TRUE
    ) +
    ylim(0,1) + 
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(title = "Daily Queue Utilization", x = "Date", y = "Daily Utilization") + theme_light()
  
  return(gg)
}


#-------------------------------------------------------------------------------
# MPI queues: 4, a/a128, u, z

util4<-rollingQueueGraphs(selected = c("4")) + ggtitle("Daily Utilization for 4 Queue")

utila<-rollingQueueGraphs(selected=c("a","a128")) + ggtitle("Daily Utilization for a Queue")

utilu<- rollingQueueGraphs(selected=c("u")) + ggtitle("Daily Utilization for u Queue")

utilz<-rollingQueueGraphs(selected=c('z')) + ggtitle("Daily Utilization for z Queue")


#--------------------------------------------------------------------------------
# “old” 1p queues: b, b-long, p

ggplotly(utila)

old1p <- daily_utilization(queues = c("b","b-long","p"), start = "01/2024", end = "08/2024")

ggplot(old1p, aes(x=as.Date(Date) , y = DailyUtilization , color = queue)) + 
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  ylim(0,1) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Daily Queue Utilization of Old 1p Queues", x = "Date", y = "Daily Utilization") + theme_light()


# “new” 1p queues: f, w, w-long, y

new1p <- daily_utilization(queues = c("f","w","w-long", "y"), start = "01/2024", end = "08/2024")

ggplot(new1p, aes(x=as.Date(Date) , y = DailyUtilization , color = queue)) + 
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  ylim(0,1) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Daily Queue Utilization of New 1p Queues", x = "Date", y = "Daily Utilization") + theme_light()


# “1TB node”: mem1024

tb1 <- daily_utilization(queues = c("mem1024"), start = "01/2024", end = "08/2024")

ggplot(tb1, aes(x=as.Date(Date) , y = DailyUtilization , color = queue)) + 
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  ylim(0,1) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Daily Queue Utilization of 1TB Queues", x = "Date", y = "Daily Utilization") + theme_light()


# “old interactive queue” p-int

oldint<- daily_utilization(queues = c("p-int"), start = "01/2024", end = "08/2024")

ggplot(oldint, aes(x=as.Date(Date) , y = DailyUtilization , color = queue)) + 
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  ylim(0,1) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Daily Queue Utilization of Old Interactive Queue", x = "Date", y = "Daily Utilization") + theme_light()

# “28 core queues” queues: mem384, mem512, w28,

twentyeightcore<- daily_utilization(queues = c("mem384", "mem512","w28"), start = "01/2024", end = "08/2024")

ggplot(twentyeightcore, aes(x=as.Date(Date), y=DailyUtilization, color = queue)) +
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  ylim(0,1) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Daily Queue Utilization of 28 Core Queues", x = "Date", y = "Daily Utilization") + theme_light()
# “8 and 16 core queues” queues: c, p8, p16, e8

eightsixteen<- daily_utilization(queues = c("c","p8", "p16", "e8"), start = "01/2024", end = "08/2024")

ggplot(eightsixteen, aes(x=as.Date(Date), y= DailyUtilization, color = queue)) +
  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +
  ylim(0,1) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Daily Queue Utilization of 8 and 16 Core Queues", x = "Date", y = "Daily Utilization") + theme_light()

#-------------------------------------------------------------------------------

# Data Exportation 

df <- get_acct(2023, 2024, datatype="feather")
df<- df %>% mutate(Year = year(end_date))
aqueue <- df %>% filter (qname %in% c("a", "a128"))
aqueue<- aqueue %>% group_by(qname, Year, project,owner) %>% summarise(N=n(), .groups = "keep")
write.csv(aqueue, "aqueueutil.csv")
