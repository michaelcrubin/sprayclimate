

library(tidyverse)
library(data.table)

#*****************************************************************************************
#******THESE ARE THE GENERAL FUNCTIONS WHICH ARE CALLED FROM OTHER SCRIPTS ***************
#*****************************************************************************************

#IMPORT DATA FRAME
import_fun<-function(filename){
  
  #getting current working directory
  data_path<-here("DATA BASE")
  
  #Importing the File
  pathname<-paste(data_path,filename, sep = "/")
  df1 <- read.table(pathname, header = FALSE, sep = ";", dec=',', stringsAsFactors = FALSE)
  #returning
  df1
}

#CLEANING THE DATA FRAME
clean_dfs<-function(df_station, station){
  #checking the file lat and long
  if (df_station[1,1] != 'LAT'){print('Incorrect LAT label')}
  if (df_station[2,1] != 'LONG'){print('Incorrect LONG label')}
  if (df_station[3,1] != ''){print('Incorrect LINE ERROR')}
  #getting coordinates
  lat<-as.numeric(gsub(",", ".", df_station[1,2]))
  long<-as.numeric(gsub(",", ".", df_station[2,2]))
  
  #getting lengh and with of df
  n<-nrow(df_station)-4
  k<-ncol(df_station)-1
  
  #storing headers incl ET0 and Timestamp value
  heanders<-df_station[3,]
  heanders[1]<-'Timestamp'
  #ficuting out of has ETo
  eto<-FALSE
  if (heanders[(k+1)] == ''){
    heanders[(k+1)]<-'ET0'
    eto<-TRUE
    }
  #cleaning the header texts
  heanders<-apply(heanders, 2,function(y) (gsub(" ", "_", y)))
  
  #deleting unneeded rows and writing the correct headers
  df_station<-df_station[5:(5+n),]
  colnames(df_station)<-heanders
  
  df_station<-df_station %>% drop_na()
  #redefine n after clipping na
  n<-nrow(df_station)
  
  #storing time stamps
  Timestamp<-as.character(df_station[,1])
  #taking all data columns, replacing , by . and turning to numeric
  df_station<-as.data.frame(apply(df_station[2:(k+1)], 2,function(y) as.numeric(gsub(",", ".", y))))
  
  #if I have a ETO column, i slice that last column from the rest and store it in a sep df
  if (eto==TRUE){
    #subsetting into the eto df and the normal one
    df_eto<-as.data.frame(df_station[1:(n-1),(k)])
    df_station<-df_station[,1:(k-1)]
    #putting timestamps as rownames, clipping all that are na and deleting the hours
    #attention, the value is for the day before, therefore offset the timestamp for 1 unit
    rownames(df_eto)<-Timestamp[2:n]
    colnames(df_eto)<-'ET0_daily'
    df_eto<-df_eto %>% drop_na()
    b<-as.character(lapply(rownames(df_eto), function(y) substring(y,first=1, last = 10)))
    rownames(df_eto)<-b
  }
  #defining rownames at the main df
  rownames(df_station)<-Timestamp
  df_station<-df_station %>% drop_na()
  #storing the metadata
  meta_data<-as.data.frame(list(StationID=station$Station_ID, Name=station$Name,Type=station$Type , LAT=as.numeric(lat), LONG=as.numeric(long), N=n, K=k, ETO=eto))
  #creating the return vector
  if (eto==TRUE){
    return<-list(Data=df_station,ETO=df_eto, Meta=meta_data)
  }else{
    return<-list(Data=df_station, Meta=meta_data)
  }
  return
}


#FUNCTION WHICH IS CALLED ALSO FROM OTHER SCRIPTS
extract_data<-function(pathname){
  #importing the crude station data

  station_list <- read.csv(pathname,header = TRUE,sep = ";", dec=",", stringsAsFactors = FALSE)
  
  
  
  #start data set
  data_set<-list()
  eto_set<-list()
  meta_set<-list()
  all_names<-list()
  eto_names<-list()
  all_ids<-list()
  eto_ids<-list()
  
  #loop through all stations in station list, importint their data and cleaning them and store in list of DF
  for (i in 1:nrow(station_list)){
    #slicing out current station
    #i=1
    station<-station_list[i,]
    #creating the file name
    filename<-paste(station$Station_ID, '.csv', sep='')
    #filename<-'00000160.csv'
    df_station<-import_fun(filename)
    #send it to function to clean it
    return<-clean_dfs(df_station, station)
    
    #if the return contains 3 elements i have also ETO set, otherwise only data and meta
    if (length(return)==3){
      #write df in a list
      data_set<-c(data_set, list(return$Data))
      eto_set<-c(eto_set, list(return$ETO))
      meta_set<-c(meta_set, list(return$Meta))
      #define lists with names of stations
      all_names<-append(all_names, station$Name)
      eto_names<-append(eto_names, station$Name)
      all_ids<-append(all_ids, station$Station_ID)
      eto_ids<-append(eto_ids, station$Station_ID)
    }else{
      #write df in a list
      data_set<-c(data_set, list(return$Data))
      meta_set<-c(meta_set, list(return$Meta))
      #define lists with names of stations
      all_names<-append(all_names, station$Name)
      all_ids<-append(all_ids, station$Station_ID)
    }
  }
  
  #tibble together the lists with dfs
  data_sets <- tibble(Stations = all_names, Station_Data = data_set)
  meta_sets <- tibble(Stations = all_names, Meta_Data = meta_set)
  eto_sets <- tibble(Stations = eto_names, ETO_Data = eto_set)
  #defining rownames
  rownames(data_sets)<-all_ids
  rownames(meta_sets)<-all_ids
  rownames(eto_sets)<-eto_ids
  #finally deleting all unneeded variables
  rm(i, df_station, data_set, meta_set, eto_set, all_names, eto_names, filename,pathname, all_ids, eto_ids, return, station)
  #TIBBLE THE WHOLE STUFF TOGETHER TO FEED IT BACK
  resultdata<-list()
  resultdata<-c(resultdata, list(data_sets, meta_sets,eto_sets))
  result <- tibble(RESULT=resultdata)
  result
}


# #THIS PART IS USED ONLY THEN CALLING THE EXTRACTION DIRECTLY INSIDE THIS SCRIPT
# #defining the filename with the list of all stations i want to import
# filename='Station_list.csv'
# mydir<-"~/OneDrive/DRAWDOWN LABS/400_RUNNING PROJECTS/OWN DEVELOPMENTS/DATA BASE"
# pathname<-paste(mydir,filename, sep = "/")
# 
# #Calling the function to extract
# result<-extract_data(pathname)
# 
# #DE-TIBBLE THE WHOLE STUFF
# data_sets<-result$RESULT[[1]]
# meta_sets<-result$RESULT[[2]]
# eto_sets<-result$RESULT[[3]]
# 
# #LIKE THIS I EXTRACT A SINGLE DATA SET
# case1<-data_sets[[2]][[1]]
# case2<-data_sets[[2]][[4]]
