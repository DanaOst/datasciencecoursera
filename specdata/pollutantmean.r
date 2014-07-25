
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values) 
counter=0
row_nums = 0
station_sums<- 0
for (i in id) {
  if(i<10) {
    monitoring_stations <- paste("00",i,".csv", sep="")}
  else {
    if(i<100 & i>=10) {
      monitoring_stations <- paste("0",i,".csv", sep="")}
    else {monitoring_stations <- paste(i,".csv", sep="")}}
  station <- read.csv(monitoring_stations,header=TRUE)
  newstation<-station[complete.cases(station[,pollutant]),]
  row_nums <- NROW(newstation)
  stationsum <- (colSums(newstation[pollutant]))
  station_sums <- station_sums+stationsum
  counter=counter+row_nums
}
stations_mean=station_sums/counter
stations_mean
}
