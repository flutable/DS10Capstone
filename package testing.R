library(tidyverse)
library(data.table)
library(sqldf)
library(quanteda)
# data.table experiments. Enhanced data frames.
flights <- fread(".\\data.table\\flights14.csv")  # can fread(http/s URIs too)
DT <- data.table(ID=c("b", "b", "b", "a", "a", "c"), a=1:6, b=7:12, c=13:18) #as.data.table to convert
# DT[i, j, by]

##   R:      i                 j        by
## SQL:  where   select | update  group by
#subset rows using i, calculate j, group by.
ans <- flights[origin=="JFK" & month==6L]  #use colnames as variables. calculation & grouping are optional
ans <-flights[1:2]
#Sort flights first by column origin in ascending order, and then by dest in descending order:
#We can use the base R function order() to accomplish this.
ans <- flights[order(origin, -dest)]  # order returns a vector whose indices would put the initial vector in order.
 #- on a character column in a data.table to sort in decreasing order. Data.table's order is faster than base.

#Since columns can be referred to as if they are variables within the frame of data.tables, we directly 
#refer to the variable we want to subset. Since we want all the rows, we simply skip i
ans <- flights[, arr_delay]        #selects as vector
ans <- flights[, list(arr_delay)]  #selects as data.table .() is the same as .list()
#As long as j-expression returns a list, each element of the list will be converted to a column in the 
#resulting data.table. This makes j quite powerful, as we will see shortly.
# Select both arr_delay and dep_delay columns.

ans <- flights[, .(arr_delay, dep_delay)]
head(ans)
#    arr_delay dep_delay
# 1:        13        14
# 2:        13        -3

#Select both arr_delay and dep_delay columns and rename them to delay_arr and delay_dep.
#Since .() is just an alias for list(), we can name columns as we would while creating a list.
ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]

#Compute or do in j
#– How many trips have had total delay < 0?
ans <- flights[, sum((arr_delay + dep_delay) < 0)] #colnames as variables, so calc on them
ans
# [1] 141814

#Subset in i and do in j
# Calculate the average arrival and departure delay for all flights with “JFK” as the origin airport in the month of June.
ans <- flights[origin == "JFK" & month == 6L,                       #data.table doesn't subset here
               .(m_arr = mean(arr_delay), m_dep = mean(dep_delay))] # .but gets cols of interest as data.table, then subsets rows. optimised.
ans
#       m_arr    m_dep
# 1: 5.839349 9.807884
#How many trips have been made in 2014 from “JFK” airport in the month of June?
  
ans <- flights[origin == "JFK" & month == 6L, length(dest)]
ans
# [1] 8422
# The function length() requires an input argument. We just needed to compute the number of rows in the subset. 
#We could have used any other column as input argument to length()
#.N is a special in-built variable that holds the number of observations in the current group. 
#useful when combined with by 
#In the absence of group by operations, it simply returns the number of rows in the subset.
ans <- flights[origin == "JFK" & month == 6L, .N]  #subset by origin & month, .N is the calc. 
#No grouping or .() or list() so return vector.
ans
#8422.

#refer to columns by names in j (like in a data.frame)?
# You can refer to column names the data.frame way using with = FALSE.
# Select both arr_delay and dep_delay columns the data.frame way.
ans <- flights[, c("arr_delay", "dep_delay"), with = FALSE]
head(ans)
#    arr_delay dep_delay
# 1:        13        14
# with() is similar to R with()
DF = data.frame(x = c(1,1,1,2,2,3,3,3), y = 1:8)
# find all rows where x > 1
DF[DF$x >1, ]           #usual way. Needs , to select all cols
DF[with(DF, x > 1), ]   # using with() allows using DF$x as if it was a variable. with=FALSE in data.table restores "data frame" mode
# deselect cols
ans <- flights[ ,!c("arr_delay", "dep_delay"), with=FALSE]  #-c works as well
ans <- flights[ , -(year:day), with =FALSE] # can select start/end cols.

# Aggregations
#no. of trips by origin airport
ans <- flights[ , .(.N), by=origin] #all rows, group_by, then .(.N) gives a d.t with the total in each group. by=ori
# group by several columns. Need to use quotes around character vector
ans <- flights[ , .(.N), by=c("origin","day")]
# trips for each origin airport for carrier code "AA
ans <- flights[carrier=="AA", .N, by=origin]    
#total number of trips for each origin, dest pair for carrier code “AA”
ans <- flights[carrier=="AA", .N, by=.(origin,dest)]  #need to use .(colname1, colname2) to use DT's colname as variable feature
#average arrival and departure delay for each orig,dest pair for each month for carrier code “AA”
ans <- flights[carrier=="AA", .(mean(arr_delay), mean(dep_delay)), by=.(origin, dest, month)] #by=c("origin", "dest", "month")
# ans
# origin dest month         V1         V2
# 1:    JFK  LAX     1   6.590361 14.2289157
# 2:    LGA  PBI     1  -7.758621  0.3103448
#How to order the result by those grouping columns origin, dest and month? data.table retains the original order of groups by design. There are cases when preserving the original order is essential. But at times we would like to automatically sort by the variables we grouped by.
