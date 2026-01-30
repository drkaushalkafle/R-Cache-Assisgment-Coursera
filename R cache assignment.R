makeCacheVector<- function (x=numeric()){
    cached_mean<-NULL
    
    set <-function (y){
      x<<-y
      cached_mean<<-NULL #
    }

get<-function ()x
setmean <- function (mean_value,cached_mean <<-mean_value
  getmean <- function () cached_mean 

list (set=set, get=get, setmean=setmean, getmean=getmean)
}

