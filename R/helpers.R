
getDatedMarkers <- function(formattedcpcht) {
  dmarkers <- formattedcpcht %>%
    filter(CLADE=='[dated' | CLADE=='AGE' )%>%
    rename ( DATLEV=FAD)%>%
    separate( col=EVENT,sep = '=',into=c('EVENT','AGE'))%>% #разделяем колонку EVENT на две используя в качестве разделителя '='
    mutate( AGE=as.numeric(AGE))%>%   #преобразуем тип колонки AGE в числовой тип
    arrange( desc(AGE)) %>% #сортируем
    select(ID, EVENT, DATLEV, AGE)
  return (dmarkers)
}

get_intervals_by_params<- function (start,finish, step, age=FALSE){
  # start=14
  # finish=200
  # step=8
  if (age==FALSE) {
    st <- seq(start, finish-step, by=step) #заполнение начальных координат отрезков
    en <- seq(start+step, finish, by=step) #заполнение конечных координат отрезков
  }
  if (age==TRUE) {
    en <- seq(start, finish-step, by=step*-1) #заполнение начальных координат отрезков
    st <- seq(start+step, finish, by=step*-1) #заполнение конечных координат отрезков

  }
  return ( data.frame(num=c(1:numrow),st=st,en=en,mid=(st+en)/2)  )  #data: num,st,en,mid
}
splitRangesToBins_by_int <- function(intervals, dat) {
  divdindat<-dat[0,]%>%mutate(int=NULL) #create empty tibble
  for (i in 1:nrow(intervals)) { #interate intervals
    int<-intervals[i,] #select first intervals
    st<-int$st
    en<-int$en
    dat.tt<-dat%>%filter( (max_ma<=st & max_ma>=en) |  (min_ma<=st & min_ma>=en) |  (max_ma>=st & min_ma<=en) )#filter dat by params
    dat.tt<-mutate(dat.tt,int=int$num) #set interval nuber
    divdindat<- bind_rows(divdindat, dat.tt) #connect filtered data together
  }
  return (divdindat)
}




