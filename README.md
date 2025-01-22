# qutil - Queue Utilization App
Research Computing Services, IS&T, Boston University  
**Authors**: Ellis Hamilton (ellisham@bu.edu)

This app provides information about Utilization of Share Computing Cluster at Boston University. 

## Usage 
``` 
qutil -h 
```

### Important Files: 

* hanalysis.pl : This file accepts a ".h" file as input (say 2402.h) and outputs a csv file in proper format 
* hanalysis.R : This file contains an R script, including multiple functions :
   * ```merged()``` : a function that accepts a beginning and ending month as input, and then calls the Perl script mentioned above, and returns a merged dataframe of the data from the months in the range
  * ```hfileUtil()``` : a function that accepts a dataframe (as created by merged()) and a queue, and outputs the utilization
  * ```graphQueueUtil``` : a function that also accepts a dataframe and a queue, but outputs the daily utilization graph of the time included

### Action Items/Updates Necessary: 

* These functions, as written above, take data as input, as created by the merged dataframe function. However, with a bit of extra lines, you could call that function within so that only a time range needs to be inputted/no extra step is needed
* We were wanting to turn this into an RShiny dashboard, where the user had options of queues and could specify dates, and be shown the resulting utilization or utilization graph 
