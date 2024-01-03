################################################
#Functions
################################################
#format start/end time as "%Y-%m-%d %H:%M:%S"-----
#especially include 00:00:00
format_time = function(actr){
  for (i in 1:nrow(actr)) {
    if(is.na(strptime(actr$starttime[i],format = "%Y-%m-%d %H:%M:%S"))){
      actr$starttime[i] = format(strptime(
        paste(strptime(actr$starttime[i],format = "%Y-%m-%d"),
              '00:00:00',sep=' '),
        format = "%Y-%m-%d %H:%M:%S"),
        "%Y-%m-%d %H:%M:%S")
    }
    
    if(is.na(strptime(actr$endtime[i],format = "%Y-%m-%d %H:%M:%S"))){
      actr$endtime[i] = format(strptime(paste(strptime(
        actr$endtime[i],format = "%Y-%m-%d"),'00:00:00',sep=' '),
        format = "%Y-%m-%d %H:%M:%S"),
        "%Y-%m-%d %H:%M:%S")
    }
  }
  return(actr)
}


#split data if it across multiple days-----
split_day = function(actr_person){
  actr_person_split = actr_person
  for (i in 1:length(actr_person)) {
    actr_person[[i]] = actr_person[[i]][order(actr_person[[i]]$starttime),]
    actr_person_split[[i]] = actr_person[[i]][1,]
    firstdate = actr_person[[i]][1,'startdate']
    lastdate = actr_person[[i]][nrow(actr_person[[i]]),'enddate']
    num_day = as.numeric(
      as.Date(as.character(lastdate), format="%Y-%m-%d") - 
        as.Date(as.character(firstdate), format="%Y-%m-%d")) + 1
    days = seq(as.Date(as.character(firstdate), format="%Y-%m-%d"), 
               as.Date(as.character(lastdate), format="%Y-%m-%d"), 
               by="days")
    
    for (j in 1:num_day) {
      today_ac = actr_person[[i]][which(
        actr_person[[i]]$startdate==days[j]|
          (days[j]<=actr_person[[i]]$enddate &
             days[j]>=actr_person[[i]]$startdate)),]
      #look at pervious day overnight activities
      yesterday = strptime(days[j]-1,format = "%Y-%m-%d") 
      yesterday_ac = actr_person[[i]][which(
        actr_person[[i]]$startdate==yesterday),]
      yesterday_ac = yesterday_ac[order(yesterday_ac$starttime),]
      if(nrow(yesterday_ac)>0){
        if(yesterday_ac[nrow(yesterday_ac),'startdate']!=
           yesterday_ac[nrow(yesterday_ac),'enddate']){
          #yesterday last actr activity is overnight
          today_ac = rbind(yesterday_ac[nrow(yesterday_ac),],today_ac)
        }
      }
      today_ac = unique(today_ac)
      today_ac = today_ac[order(today_ac$starttime),]
      #look at if there is activity in this day
      if(nrow(today_ac)>0){
        if(today_ac[nrow(today_ac),'enddate']>days[j]){
          today_ac$endtime[nrow(today_ac)]= format(strptime(
            paste(days[j],'24:00:00',sep=' '),
            format = "%Y-%m-%d %H:%M:%S"),
            "%Y-%m-%d %H:%M:%S")
        }
        if(today_ac$startdate[1] < days[j]){
          today_ac$starttime[1]= format(strptime(
            paste(days[j],'00:00:00',sep=' '),
            format = "%Y-%m-%d %H:%M:%S"),
            "%Y-%m-%d %H:%M:%S")
        }
        actr_person_split[[i]] = rbind(actr_person_split[[i]],today_ac)
      }
    }
    actr_person_split[[i]] = actr_person_split[[i]][-1,]
    actr_person_split[[i]]$startdate = strptime(
      actr_person_split[[i]]$starttime,format = "%Y-%m-%d")
  }
  return(actr_person_split)
}


#remove the invalid data----
get_valid = function(actr_person_split){
  for (i in 1:length(actr_person_split)) {
    for (j in 1:length(actr_date)) {
      if(actr_person_split[[i]][1,'UserId']==names(actr_date)[j]){
        actr_person_split[[i]] = actr_person_split[[i]][
          order(actr_person_split[[i]]$starttime),]
        actr_person_split[[i]]$startdate = strptime(
          actr_person_split[[i]]$starttime,format = "%Y-%m-%d")
        actr_person_split[[i]]$enddate = strptime((strptime(
          actr_person_split[[i]]$endtime,format = "%Y-%m-%d %H:%M:%S")-1),
          format = "%Y-%m-%d")
        actr_person_split[[i]]$valid = FALSE
        valid_date = actr_date[[j]][which(
          actr_date[[j]]$label!='invalid'),'date']
        for (k in 1:nrow(actr_person_split[[i]])) {
          if(any(valid_date==actr_person_split[[i]]$startdate[k])){
            actr_person_split[[i]]$valid[k] = TRUE
          }
        }
      }
    }
    actr_person_split[[i]] = actr_person_split[[i]][
      which(actr_person_split[[i]]$valid),]
    actr_person_split[[i]] = actr_person_split[[i]][
      ,-length(actr_person_split[[i]])]
  }
  return(actr_person_split)
}
################################################
#Read and Clean data
################################################
#read data----
actr = read.csv("data/activity_travel_data.csv",sep=",",
                header = T,stringsAsFactors=FALSE)

#format data
actr = format_time(actr)
actr$startdate <- strptime(actr$starttime,format = "%Y-%m-%d")   
actr$enddate <- strptime((strptime(
  actr$endtime,format = "%Y-%m-%d %H:%M:%S")-1),format = "%Y-%m-%d")       
actr$UserId = as.character(actr$UserId)

activity <- actr[actr$type=='ACTIVITY']

#filter invalid data----
actr_person = split(actr,actr$UserId)  #get list by person
actr_person_summary = data.frame(UserId=NULL,num_day=NULL)
actr_date = actr_person

actr_person_split = split_day(actr_person)

#label whether the person-day is valid/incomplete
for(i in 1:length(actr_person)){
  actr_person[[i]] = actr_person[[i]][
    order(actr_person[[i]]$starttime),]
  
  #construct actr_date
  firstdate = actr_person[[i]][1,'startdate']
  lastdate = actr_person[[i]][nrow(actr_person[[i]]),'enddate']
  
  
  actr_date[[i]] = data.frame(date = seq(as.Date(firstdate), 
                                             as.Date(lastdate), by="days"), 
                                  label = as.character('valid'), 
                                  stringsAsFactors = FALSE)
  
  #if it is first/last day -> incomplete
  if(actr_person[[i]][1,'starttime']>format(strptime(
    paste(firstdate,'00:00:00',sep=' '),
    format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")){  #first day
    actr_date[[i]][1,'label'] = 'incomplete'  
  }
  else{
    actr_date[[i]][1,'label'] = 'valid'
  }
  
  if(actr_person[[i]][nrow(actr_person[[i]]),'endtime']<
     format(strptime(paste(lastdate,'24:00:00',sep=' '),
                     format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")){ 
    #last day
    actr_date[[i]][nrow(actr_date[[i]]),'label'] = 'incomplete' 
  }
  else{
    actr_date[[i]][nrow(actr_date[[i]]),'label'] = 'valid'
  }  
  
  #days with missing data
  #all-day missing -> invalid
  #others -> incomplete
  if('MI'%in%actr_person[[i]]$subtype){
    for (j in 1:nrow(actr_person[[i]])) {
      if(actr_person[[i]][j,'starttime']>=format(strptime(
        paste(actr_person[[i]][j,'startdate'],'00:00:00',sep=' '),
        format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S") &
        actr_person[[i]][j,'endtime']<=format(strptime(
          paste(actr_person[[i]][j,'enddate'],'24:00:00',sep=' '),
          format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")&
        actr_person[[i]][j,'subtype']=='MI'){
        if(as.numeric(as.Date(actr_person[[i]][j,'enddate'])-
                      as.Date(actr_person[[i]][j,'startdate']))>1){
          date = seq(as.Date(actr_person[[i]][j,'startdate']), 
                     as.Date(actr_person[[i]][j,'enddate']), by="days")
          if(actr_person[[i]][j,'starttime']==format(strptime(
            paste(date[1],'00:00:00',sep=' '),format = "%Y-%m-%d %H:%M:%S"),
            "%Y-%m-%d %H:%M:%S")){
            actr_date[[i]][which(
              actr_date[[i]]$date==date[1]),'label']='invalid'
          }
          else{
            actr_date[[i]][which(
              actr_date[[i]]$date==date[1]),'label']='incomplete'
          }
          
          if(actr_person[[i]][j,'enddate']==format(strptime(
            paste(date[length(date)],'24:00:00',sep=' '),
            format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")){
            actr_date[[i]][which(
              actr_date[[i]]$date==date[length(date)]),'label']='invalid'
          }
          else{
            actr_date[[i]][which(
              actr_date[[i]]$date==date[length(date)]),'label']='incomplete'
          }
          actr_date[[i]][which(
            actr_date[[i]]$date%in%date[2:(length(date)-1)]),'label']='invalid'
        }
        else if(as.numeric(as.Date(actr_person[[i]][j,'enddate'])-
                           as.Date(actr_person[[i]][j,'startdate']))==1){
          if(actr_person[[i]][j,'starttime']==format(strptime(
            paste(actr_person[[i]][j,'startdate'],'00:00:00',sep=' '),
            format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")){
            actr_date[[i]][which(
              actr_date[[i]]$date==
                as.Date(actr_person[[i]][j,'startdate'])),'label']='invalid'
          }
          else{
            actr_date[[i]][which(
              actr_date[[i]]$date==as.Date(
                actr_person[[i]][j,'startdate'])),'label']='incomplete'
          }
          
          if(actr_person[[i]][j,'endtime']==format(strptime(
            paste(actr_person[[i]][j,'enddate'],'24:00:00',sep=' '),
            format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")){
            actr_date[[i]][which(actr_date[[i]]$date==as.Date(
              actr_person[[i]][j,'enddate'])),'label']='invalid'
          }
          else{
            actr_date[[i]][which(actr_date[[i]]$date==as.Date(
              actr_person[[i]][j,'enddate'])),'label']='incomplete'
          }
        }
        else if(as.numeric(as.Date(actr_person[[i]][j,'enddate'])-
                           as.Date(actr_person[[i]][j,'startdate']))==0){
          if(actr_person[[i]][j,'starttime']==format(strptime(
            paste(actr_person[[i]][j,'startdate'],'00:00:00',sep=' '),
            format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S") &
            actr_person[[i]][j,'endtime']==format(strptime(
              paste(actr_person[[i]][j,'enddate'],'24:00:00',sep=' '),
              format = "%Y-%m-%d %H:%M:%S"),"%Y-%m-%d %H:%M:%S")){
            actr_date[[i]][which(
              actr_date[[i]]$date==as.Date(
                actr_person[[i]][j,'startdate'])),'label']='invalid'
          }
          else{
            actr_date[[i]][which(
              actr_date[[i]]$date==as.Date(
                actr_person[[i]][j,'startdate'])),'label']='incomplete'
          }
        }
      }
    }
  }
  
  #summary
  num_day = as.numeric(as.Date(as.character(lastdate), format="%Y-%m-%d")- 
                         as.Date(as.character(firstdate), format="%Y-%m-%d"))+1
  actr_person_summary  = rbind(
    actr_person_summary,
    data.frame(UserId = actr_person[[i]][1,'UserId'],
               num_day=num_day))
}

for (i in 1:length(actr_date)) {
  actr_person_summary[i,'Valid_num'] = nrow(
    actr_date[[i]][which(actr_date[[i]]$label!='invalid'),])
}

#summary
table(actr_person_summary$Valid_num)  #number of valid day
#remove invalid person
#a valid person should have at least 2 valid days 
actr_person_summary = actr_person_summary[which(
  actr_person_summary$Valid_num>1),] 
actr_date = actr_date[names(actr_date)%in%
                                actr_person_summary$UserId]
actr_person = actr_person[names(actr_person)%in%
                                    actr_person_summary$UserId]
actr_person_split = get_valid(actr_person_split)