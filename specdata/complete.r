complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
final2 <- data.frame("id"=NA, "nobs"=NA)[numeric(0),]
for(i in id){
  if(i<10) {
    monitoring_stations <- paste("00",i,".csv", sep="")}
  else {
       if(i<100 & i>=10) {
      monitoring_stations <- paste("0",i,".csv", sep="")}
  else {monitoring_stations <- paste(i,".csv", sep="")}}
station <- read.csv(monitoring_stations,header=TRUE)
stations_no_na<-station[complete.cases(station),]
final<- c(stations_no_na[1,4],nrow(stations_no_na))
final2<- rbind(final2, final)
}
names(final2) <- c("id","nobs")
print(final2)
}