# Library needed
library(readr)
library(dplyr)
library(moments)



#import data
nflix <- read.csv("netflix_titles.csv",
                  header = TRUE)



#Analysis of the data
View(nflix)
sum(nflix$director == "")
sum(nflix$type == "")
sum(nflix$title == "")
sum(nflix$cast == "")
sum(nflix$country == "")
sum(nflix$date_added == "")
sum(nflix$release_year == "")
sum(nflix$rating == "")
sum(nflix$duration == "")
sum(nflix$listed_in == "")
sum(nflix$description == "")
summary(nflix)
dim(nflix)
str(nflix)


#Required functions
empty_func<-function(n)
{
  count<-0
  for (i in 1: length(n)) {
    
    if(n[i]==""){count=count+1}
  }
  return(count)
  
}


mean_fun <- function(n)
{
  mean_c = numeric(0)
  for (i in 1:ncol(n)) {
    s = sum(n[, i], na.rm = TRUE)
    l = length(n[, i][is.na(n[, i]) == FALSE])
    m[i] = s / l
  }
  return (m)
}


var_fun <- function(n)
{
  m = mean_fun(n)
  for (i in 1:ncol(n))
  {
    v = (i - mean(i)) ^ 2 / length(n[, i][is.na(n[, i]) == FALSE])
  }
  return(v)
}



Sd_func <- function(n)
{
  s = sqrt(var_fun(n))
  return(s)
}


mode<-function(n){
  uniqvalue<-unique(n)
  uniqvalue[which.max(tabulate(match(n,uniqvalue)))]
}




#datframe
table = data.frame()
for (i in 1:ncol(nflix)) {
  new_table <- data.frame(
    coloumn_name = colnames(nflix)[i],
    data_type = typeof(nflix[, i]),
    unique_value = length(unique(nflix[, i])),
    Null_value = sum(is.null(nflix[, i])),
    NA_element = sum(is.na(nflix[, i])),
    NaN_element = sum(is.nan(nflix[, i])),
    empty_element=empty_func(nflix[,i]),
    Mode= mode(nflix[,i]),
    Mean_VALUE=ifelse( typeof(nflix[,i])!="CHARACTER",mean(nflix[,i]),"Not Numeric"),
    MEDIAN_=ifelse(typeof(nflix[,i])!="CHARACTER",median(nflix[,i]),"Not Numeric"),
    VARIANCE_=ifelse(typeof(nflix[,i])!="CHARACTER",var(nflix[,i]),"Not Numeric"),
    STANDARD_DEVIATION_=ifelse(typeof(nflix[,i])!="CHARACTER",sqrt(var(nflix[,i])),"Not Numeric"),
    SKEWNESS_=ifelse( is.character(nflix[,i])!="TRUE",skewness(nflix[,i]),"Not Numeric"),
    KURTOSIS_=ifelse( is.character(nflix[,i])!="TRUE",kurtosis(nflix[,i]),"Not Numeric")
    )    
  table <- rbind.data.frame(table, new_table)
    
}
