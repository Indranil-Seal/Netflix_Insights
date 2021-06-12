
    #library included
library(dplyr)
library(moments)


    #import data
movie_data<-read.csv("netflix_titles.csv")

   


     
   # function for empty element
empty_func<-function(g)
{
  c<-0
  for (j in 1: length(g)) {
    
  if(g[j]==""){c=c+1}
  }
    return(c)
  
}





    #function for mode 
 getmode<-function(m){
  uniqvaue<-unique(m)
  uniqvaue[which.max(tabulate(match(m,uniqvaue)))]
 }
 
 
mode_func<-function(m){
  if(getmode(m)==""){
    return(print("more than one mode"))
  }
  else{
    return(getmode(m))
  }
}


   # primary analysis in single window view

  table=data.frame()
for(i in 1:ncol(movie_data)){
analysis_table<-data.frame(column_name=colnames(movie_data)[i],
                           Entered_Data_type=typeof(movie_data[,i]),
                           unique_entries=length(unique(movie_data[,i])),
                          na_element=sum(is.na(movie_data[,i])),
                          null_element=sum(is.null(movie_data[,i])),
                          nan_element=sum(is.nan(movie_data[,i])),
                          empty_element=empty_func(movie_data[,i]),
                          Mean_VALUE=ifelse( is.character(movie_data[,i])!="TRUE",mean(movie_data[,i]),"Not Numeric"),
                          MEDIAN_=ifelse(is.character(movie_data[,i])!="TRUE",median(movie_data[,i]),"Not Numeric"),
                          MODE_=mode_func(movie_data[,i]),
                          VARIANCE_=ifelse(is.character(movie_data[,i])!="TRUE",var(movie_data[,i]),"Not Numeric"),
                          STANDARD_DEVIATION_=ifelse(is.character(movie_data[,i])!="TRUE",sqrt(var(movie_data[,i])),"Not Numeric"),
                          
                          SKEWNESS_=ifelse( is.character(movie_data[,i])!="TRUE",skewness(movie_data[,i]),"Not Numeric"),
                          KURTOSIS_=ifelse( is.character(movie_data[,i])!="TRUE",kurtosis(movie_data[,i]),"Not Numeric")
                          )
                           
 table<-rbind.data.frame(table,analysis_table)

}


