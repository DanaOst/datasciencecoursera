corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
all_cor <-c()  
for(i in 1:332){
  if(i<10) {
    monitoring_stations <- paste("00",i,".csv", sep="")}
  else {
    if(i<100 & i>=10) {
      monitoring_stations <- paste("0",i,".csv", sep="")}
  else {monitoring_stations <- paste(i,".csv", sep="")}}
  station <- read.csv(monitoring_stations,header=TRUE)
  stations_no_na<-station[complete.cases(station),]
  row_count <- nrow(stations_no_na)
  if(row_count>threshold) {
    x<- stations_no_na[,2]
    y<- stations_no_na[,3]
    all_cor <- append(all_cor, cor(x,y))
  } 
}
return(all_cor)
}