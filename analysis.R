listofpackages<-c("readxl","imputeTS","zoo","plyr","ggplot2","forecast") 
new.pack<-listofpackages[!(listofpackages %in% installed.packages()[,"Package"])]
if(length(new.pack)) 
  install.packages(new.pack,repos='http://cran.us.r-project.org',dependencies = T)
lapply(listofpackages, require, character.only = TRUE)

# read_excel reads both xls and xlsx files
t1<-"./data/cats/"
t2<-"./data/pets/"

dir_list1<-list.files(t1)
file_list1<-paste0(t1,dir_list)

dir_list2<-list.files(t2)
file_list2<-paste0(t2,dir_list2)


extract_date<-function(filename){
  
  subfilename<-gsub(".*100K ","",filename)
  subfilename<-gsub(".xlsx*","",subfilename)
  chk<-substr(subfilename,1,2)
  year<-substr(subfilename,3,6)
  if(chk == "Q1"){
    mnt<-"1jan"
  } else  if(chk == "Q2"){
    mnt<-"1apr"
  }else  if(chk == "Q3"){
    mnt<-"1jul"
  }else  if(chk == "Q4"){
    mnt<-"1oct"
  }
  date<-paste0(mnt,year)
  z <- as.Date(date, "%d%b%Y")
  name<-as.yearmon((z))
  name
}

readfile_updated_file<-function(filename){
  temp<-read_excel(filename)
  Timestamp<-extract_date(filename)
  # print(Timestamp)
  temp$Date<-as.Date(Timestamp)
  temp
}


iterate<-function(file_list){
  for (file in file_list){
    if (!exists("xvf")){
      xvf <- readfile_updated_file(file)
    }
    if (exists("xvf")){
      temp_dataset <-readfile_updated_file(file)
      xvf<-rbind.fill(xvf, temp_dataset)
      rm(temp_dataset)
    }
  }
 xvf
}

ts_fillData<-function(){
  dataset1<-iterate(file_list1)
 # dataset2<-iterate(file_list2)
  
  tmpf<-function(dataset,arg2){
    tmp<-dataset[order(dataset$Queries,decreasing = T),]
    t2<-tmp$Query[!duplicated(tmp$Query)]
    t2<-t2[!is.na(t2)]
    gglist1<-list()
    gglist2<-list()
    
    sd_of_key<-function(keyword){
      temp<-subset(dataset,dataset$Query==keyword)
      deviation<-sd(temp$Queries)
      deviation
    }
    
    newdat<-lapply(t2,sd_of_key)
    neccomp<-data.frame(key=t2,sd=unlist(newdat))
    chk<-neccomp[order(neccomp$sd,decreasing = T),]
    
    for(t in chk$key){
      newtmp<-subset(dataset,dataset$Query==t ,select =c(Queries,Date))
      
      newtmp<-newtmp[!duplicated(newtmp),]
      newtmp<-newtmp[complete.cases(newtmp), ]
      nats<-fill_missing(newtmp)
      dev<-subset(chk,chk$key==t,select="sd")
      if(nrow(nats)>=2){
        nats$Queries<-na.ma(nats$Queries)
       if(arg2=="cat") 
         ggsave(filename = paste0("figs/Cat/",t,"-",round(dev),"-ts.png"),timeseries_Queries(nats,t))
        else if(arg2=="pet")
          ggsave(filename = paste0("figs/Cat/",t,"-",round(dev),"-ts.png"),timeseries_Queries(nats,t))
        
        #ggsave(filename = paste0(t,"_",dev,"-fs.png"),timeseries_calc(nats,t))
      } else print("not enough data")
      }
  }
  
  tmpf(dataset1,"cat")
  tmpf(dataset2,"pet")

}






timeseries_Queries<-function(doc,arg1,pm=2){
  f<-doc$Date[order(doc$Date)]
  date1 <- as.yearmon(f, "%Y-%m-%d")
  y<-(format(date1[1],"%Y"))
  m<-(format(date1[1],"%m"))
  c<-ts(doc$Queries,frequency=4,start = as.numeric(c(y,m)))
  arima.model<-auto.arima(c) #ARIMA model
  forec.20<-forecast.Arima(arima.model,h = pm,bootstrap = TRUE)
  ps<-plot_timeseries(forec.20,format.date = T)+
    scale_x_date(date_labels = "%b-%Y")+ylab("Queries Distribution")+
    xlab("TimeSeries")+
    ggtitle(bquote(atop(.(paste0("Keyword :- ",arg1))))) +
    theme(axis.text.x=element_text(angle=90, hjust=1))
  ps
}


plot_timeseries<-function(forec.obj, data.color = 'blue', fit.color = 'red', forec.color = 'black',
                          lower.fill = 'darkgrey', upper.fill = 'grey', format.date = F)
{
  serie.orig = forec.obj$x
  serie.fit = forec.obj$fitted
  pi.strings = paste(forec.obj$level, '%', sep = '')
  
  if(format.date)
    dates = as.Date(time(serie.orig))
  else
    dates = time(serie.orig)
  
  serie.df = data.frame(date = dates, serie.orig = serie.orig, serie.fit = serie.fit)
  
  forec.M = cbind(forec.obj$mean, forec.obj$lower[, 1:2], forec.obj$upper[, 1:2])
  forec.df = as.data.frame(forec.M)
  colnames(forec.df) = c('forec.val', 'l0', 'l1', 'u0', 'u1')
  
  if(format.date)
    forec.df$date = as.Date(time(forec.obj$mean))
  else
    forec.df$date = time(forec.obj$mean)
  
  p = ggplot() + 
    geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) + 
    geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df) + 
    scale_y_continuous() +
    geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) + 
    geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) + 
    geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df) + 
    scale_color_manual('Series', values=c('data' = data.color, 'fit' = fit.color, 'forecast' = forec.color)) + 
    scale_fill_manual('P.I.', values=c('lower' = lower.fill, 'upper' = upper.fill))
  
  if (format.date)
    p = p + scale_x_date()
  
  p
}

fill_missing<-function(raw.data){
  raw.data$Date <- as.Date(raw.data$Date)
  sorted.data <- raw.data[order(raw.data$Date),]
  data.length <- length(sorted.data$Date)
  Date.min <- sorted.data$Date[1]
  Date.max <- sorted.data$Date[data.length]
  all.dates <- seq(Date.min, Date.max, by="3 month")
  all.dates.frame <- data.frame(list(Date=all.dates))
  merged.data <- merge(all.dates.frame, sorted.data, all=T)
  merged.data
}

ts_fillData()

