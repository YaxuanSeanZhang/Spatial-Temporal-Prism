#Temporal Indicator Calculation----
###examine temporal fixity (Repetitive Ratio)----
temporal_list = function(activity_person,person_fixity_summary){
  person_temporal = activity_person
  for (i in 1:length(activity_person)) {
    person_temporal[[i]] = data.table(
      UserId=NULL,date=NULL,weekday=NULL,
      early_starttime=NULL,late_endtime=NULL,actual_duration=NULL, 
      num_activity=NULL,TripIdList=NULL,StartList=NULL,EndList=NULL)
    for (j in 1:length(actr_date)) {
      if(activity_person[[i]][1,'UserId']==names(actr_date)[j]){
        person_fixity_summary$ValidDay[i] = activity_person_summary$Valid_num[j]
        
        num_day = nrow(actr_date[[j]][which(actr_date[[j]]$label!='invalid'),])
        days = actr_date[[j]][which(actr_date[[j]]$label!='invalid'),'date']
        for (k in 1:num_day) {
          valid = activity_person[[i]][which(activity_person[[i]]$startdate==days[k]),]
          # without valid days
          if(nrow(valid)==0){
            person_temporal[[i]] = rbind(
              person_temporal[[i]],
              data.table(UserId=names(activity_person)[i], date=days[k], 
                         weekday = weekdays(days[k]),
                         early_starttime=0,late_endtime=0,actual_duration=0, 
                         num_activity=0,TripIdList=-999,
                         StartList=-999,EndList=-999))
          }
          else{ 
            #with valid days
            actual_duration = as.numeric(sum(difftime(valid$endtime,
                                                      valid$starttime,
                                                      units = 'mins')))
            num_activity = nrow(valid)
            starttime = as.numeric(difftime(valid$starttime[1],
                                            days[k],units='mins'))
            endtime = as.numeric(difftime(valid$endtime[nrow(valid)],
                                          days[k],units='mins'))
            StartList = as.numeric(difftime(valid$starttime,
                                            days[k],units='mins'))
            EndList = as.numeric(difftime(valid$endtime,days[k],units='mins'))
            
            person_temporal[[i]] = rbind(
              person_temporal[[i]],
              data.table(UserId=names(activity_person)[i], date=days[k], 
                         weekday = weekdays(days[k]),
                         early_starttime=starttime,late_endtime=endtime,
                         actual_duration=actual_duration, 
                         num_activity=num_activity,
                         TripIdList=list(valid$TripId),
                         StartList=list(StartList),EndList=list(EndList)))
          }
        }
      }
    }
  }
  
  for (i in 1:nrow(person_fixity_summary)) {
    #total number of activities
    person_fixity_summary$num_activity[i] = 
      sum(person_temporal[[i]]$num_activity)  
    #total activity/ total day
    person_fixity_summary$activity_perday[i] = 
      person_fixity_summary$num_activity[i]/nrow(person_temporal[[i]])  
    #number of days including activities
    person_fixity_summary$activity_days[i] = 
      nrow(person_temporal[[i]][which(person_temporal[[i]]$num_activity>0),]) 
  }
  
  mylist = list(summary = person_fixity_summary,
                temporal_list = person_temporal)
  return(mylist)
}

###spatio-temporal fixity (temporal fixity for each location)----
space_time_list = function(activity_person,person_fixity_summary){
  person_spacetime = activity_person
  person_fixity_summary = data.table(person_fixity_summary)
  for (i in 1:length(activity_person)) {
    person_spacetime[[i]] = data.table(
      UserId=NULL,date=NULL,weekday=NULL,
      early_starttime=NULL,late_endtime=NULL,actual_duration=NULL, 
      num_activity=NULL,TripIdList=NULL,StartList=NULL,EndList=NULL,Clust=NULL)
    
    for (j in 1:length(actr_date)) {
      if(activity_person[[i]][1,'UserId']==names(actr_date)[j]){
        Clust = data.frame(table(activity_person[[i]]$Clust))
        Clust = Clust[which(Clust$Var1!=-999),]
        if(nrow(Clust)==0){
          person_spacetime[[i]] = data.table(
            UserId=NULL,date=NULL,weekday=NULL,
            early_starttime=NULL,late_endtime=NULL,actual_duration=NULL, 
            num_activity=NULL,TripIdList=NULL,StartList=NULL,EndList=NULL,Clust=NULL)
        }
        else{
          for (k in 1:nrow(Clust)) {
            num_day = nrow(actr_date[[j]][which(actr_date[[j]]$label!='invalid'),])
            days = actr_date[[j]][which(actr_date[[j]]$label!='invalid'),'date']
            for (h in 1:num_day) {
              today_ac = activity_person[[i]][which(
                activity_person[[i]]$startdate==days[h] &
                  activity_person[[i]]$Clust==Clust$Var1[k]),]
              #look at if there is activity in this day
              if(nrow(today_ac)==0){
                person_spacetime[[i]] = rbind(
                  person_spacetime[[i]],
                  data.table(UserId=names(activity_person)[i], date=days[h], 
                             weekday = weekdays(days[h]),
                             early_starttime=0,late_endtime=0,actual_duration=0, 
                             num_activity=0,TripIdList=-999,
                             StartList=-999,EndList=-999,Clust=Clust$Var1[k]))
              }
              else{
                if(today_ac[nrow(today_ac),'enddate']>days[h]){
                  today_ac$endtime[nrow(today_ac)]= strptime(
                    paste(days[h],'24:00:00',sep=' '),
                    format = "%Y-%m-%d %H:%M:%S")
                }
                if(today_ac$startdate[1] < days[h]){
                  today_ac$starttime[1]= strptime(
                    paste(days[h],'00:00:00',sep=' '),
                    format = "%Y-%m-%d %H:%M:%S")
                }
                
                actual_duration = as.numeric(sum(difftime(today_ac$endtime,
                                                          today_ac$starttime,
                                                          units = 'mins')))
                num_activity = nrow(today_ac)
                starttime = as.numeric(difftime(today_ac$starttime[1],
                                                days[h], units='mins'))
                endtime = as.numeric(difftime(today_ac$endtime[nrow(today_ac)],
                                              days[h], units='mins'))
                StartList = as.numeric(difftime(today_ac$starttime,
                                                days[h], units='mins'))
                EndList = as.numeric(difftime(today_ac$endtime,
                                              days[h],units='mins'))
                
                person_spacetime[[i]] = rbind(
                  person_spacetime[[i]],
                  data.table(UserId=names(activity_person)[i], date=days[h], 
                             weekday = weekdays(days[h]),
                             early_starttime=starttime,late_endtime=endtime, 
                             actual_duration=actual_duration,
                             num_activity=num_activity, 
                             TripIdList=list(today_ac$TripId),
                             StartList=list(StartList),EndList=list(EndList),
                             Clust=Clust$Var1[k]))
              }
            }
          }
        }
        
        if(is.null(person_spacetime[[i]])==FALSE){
          person_spacetime[[i]] = split(
            person_spacetime[[i]],
            as.character(person_spacetime[[i]]$Clust))
        }
      }
    }
  }
  
  for(i in 1:length(person_spacetime)){
    if(person_fixity_summary$spatial_location[i]==0){
      person_fixity_summary$num_activity_byloc[i] = 0
      person_fixity_summary$activity_perday_byloc[i] = 0
      person_fixity_summary$activity_days_byloc[i] = 0
      person_fixity_summary$RR_byloc[i] = 0
    }
    else{
      num_activity = NULL
      activity_days = NULL
      for (j in 1:length(person_spacetime[[i]])) {
        num_activity = c(num_activity,
                         sum(person_spacetime[[i]][[j]]$num_activity))
        activity_days = c(activity_days,
                          nrow(person_spacetime[[i]][[j]][
                            which(person_spacetime[[i]][[j]]$num_activity>0),]))
      }
      
      person_fixity_summary$num_activity_byloc[i] = 
        list(num_activity)
      person_fixity_summary$activity_perday_byloc[i] = 
        list(num_activity/person_fixity_summary$ValidDay[i])
      person_fixity_summary$activity_days_byloc[i] = 
        list(activity_days)
      person_fixity_summary$RR_byloc[i] = 
        list(activity_days/person_fixity_summary$ValidDay[i])
    }
  }
  
  mylist = list(summary = person_fixity_summary,
                spacetime_list = person_spacetime)
  return(mylist)
}

###construct activity sequence at 1-min sampling interval----
get_sequence_count_byloc = function(person_spacetime,person_fixity_summary){
  person_fixity_summary_byloc = split(person_fixity_summary,
                                      as.character(person_fixity_summary$UserId))
  for (i in 1:length(person_spacetime)) {
    if(person_fixity_summary$spatial_location[i]==0){
      time = rep(0, 1440)
      person_fixity_summary_byloc[[i]]$time_count_byloc = list(time)
    }
    
    else{
      for(j in 1:length(person_spacetime[[i]])){
        person_spacetime[[i]][[j]] = person_spacetime[[i]][[j]][
          which(person_spacetime[[i]][[j]]$num_activity>0),]
        time = rep(0, 1440)
        for (k in 1:nrow(person_spacetime[[i]][[j]])) {
          interval = data.frame(Intervals(
            data.frame(start=unlist(person_spacetime[[i]][[j]]$StartList[k]), 
                       end=unlist(person_spacetime[[i]][[j]]$EndList[k]))))
          
          for (h in 1:nrow(interval)) {
            time[(interval$start[h]+1):interval$end[h]] = 
              time[(interval$start[h]+1):interval$end[h]]+1
          }
        }
        
        if(j==1){
          person_fixity_summary_byloc[[i]]$times_count_byloc = list(time)
        }
        else{
          person_fixity_summary_byloc[[i]] = rbind(
            person_fixity_summary_byloc[[i]],
            person_fixity_summary_byloc[[i]][1,])
          person_fixity_summary_byloc[[i]]$times_count_byloc[j] = list(time)
        }
      }
    }
  }
  return(person_fixity_summary_byloc)
}

###get repetitive interval for each person----
get_repetitive_interval_byloc = function(person_summary_byloc,
                                         # threshold for percent of overlapped intervals
                                         percent,
                                         # threshold for number of repeated days
                                         threshold,
                                         window = NA){
  for(i in 1:length(person_summary_byloc)){
    if(person_summary_byloc[[i]]$spatial_location[1]>0){
      for (j in 1:nrow(person_summary_byloc[[i]])) {
        time = unlist(person_summary_byloc[[i]]$times_count_byloc[j])
        if(!is.na(window)){
          time = movavg(time, window, "s")
        }
        num_percent = max(time) * percent
        num_threshold = person_summary_byloc[[i]]$ValidDay[1] * threshold
        num = min(num_percent,num_threshold)
        num = ifelse(num>=2,num,2)
        time = ifelse(time>=num,1,0)
        interval = NULL
        
        if(time[1]==1){ #stay at home in the morning
          for (k in 1:1439) {
            if(time[k]==1&time[k+1]==0){
              end=k
              if(is.null(interval)){
                interval = data.frame(start=0,end=end)
              } 
              else{
                interval = rbind(interval,data.frame(start=start,end=end))
              }
            }
            else if(time[k]==0 & time[k+1]==1){
              start = k
            }
          }
          if(time[1440]==1){
            if(is.null(interval)){
              interval = data.frame(start=0,end=1440)
            } 
            else{
              interval = rbind(interval,data.frame(start=start,end=1440))
            }
          }
        }
        
        else{ # not stay at home in the morning
          for (k in 1:1439) {
            if(time[k]==0&time[k+1]==1){
              start = k
            }
            else if(time[k]==1&time[k+1]==0){
              end=k
              interval = rbind(interval,data.frame(start=start,end=end))
            }
          }
          if(time[1440]==1){
            interval = rbind(interval,data.frame(start=start,end=1440))
          }
        }
        
        if(!is.null(interval)){
          interval = Intervals(interval)
        }
        else{
          interval = Intervals(data.frame(start=-999,end=-999))
        }
        person_summary_byloc[[i]]$interval_byloc[j] = list(interval)
      }
    }
    else{
      interval = Intervals(data.frame(start=-999,end=-999))
      person_summary_byloc[[i]]$interval_byloc[1] = list(interval)
    }
  }
  return(person_summary_byloc)
}

###visualize the repetitive interval result----
show_intersection_byloc = function(person_summary_byloc,
                                   person_spacetime,
                                   # threshold for percent of overlapped intervals
                                   percent,
                                   # threshold for number of repeated days
                                   threshold,
                                   # random pick a person to visualize
                                   i,
                                   #activity type
                                   type,window=NA){
  if(mode(i)=='character'){
    i = which(names(person_summary_byloc)==i)
  }
  
  cat('User',as.character(person_summary_byloc[[i]]$UserId[1]),'has',
      person_summary_byloc[[i]]$spatial_location[1],type,'locations(s)\n',
      'This person has',person_summary_byloc[[i]]$ValidDay[1],'valid days in total\n')
  
  for (j in 1:nrow(person_summary_byloc[[i]])) {
    time = unlist(person_summary_byloc[[i]]$times_count_byloc[j])
    if(!is.na(window)){
      time = movavg(time, window, "s")
    }
    num_percent = max(time) * percent
    num_threshold = person_summary_byloc[[i]]$ValidDay[1] * threshold
    num = min(num_percent,num_threshold)
    num = ifelse(num>=2,num,2)
    time = ifelse(time>=num,1,0)
    interval = NULL
    
    if(time[1]==1){ #stay at home in the morning
      for (k in 1:1439) {
        if(time[k]==1&time[k+1]==0){
          end=k
          if(is.null(interval)){
            interval = data.frame(start=0,end=end)
          } 
          else{
            interval = rbind(interval,data.frame(start=start,end=end))
          }
        }
        else if(time[k]==0 & time[k+1]==1){
          start = k
        }
      }
      if(time[1440]==1){
        if(is.null(interval)){
          interval = data.frame(start=0,end=1440)
        } 
        else{
          interval = rbind(interval,data.frame(start=start,end=1440))
        }
      }
    } 
    
    else{ # not stay at home in the morning
      for (k in 1:1439) {
        if(time[k]==0&time[k+1]==1){
          start = k
        }
        else if(time[k]==1&time[k+1]==0){
          end=k
          interval = rbind(interval,data.frame(start=start,end=end))
        }
      }
      if(time[1440]==1){
        interval = rbind(interval,data.frame(start=start,end=1440))
      }
    }
    
    date = data.frame(date=NULL,weekday=NULL)
    if(!is.null(interval)){
      interval = Intervals(interval)
      for (k in 1:nrow(person_spacetime[[i]][[j]])) {
        interval_today = Intervals(
          data.frame(start=unlist(person_spacetime[[i]][[j]]$StartList[k]), 
                     end=unlist(person_spacetime[[i]][[j]]$EndList[k])))
        if(nrow(data.frame(interval_intersection(interval_today,interval)))>0){
          date = rbind(date,
                       data.frame(date=person_spacetime[[i]][[j]]$date[k],
                                  weekday=person_spacetime[[i]][[j]]$weekday[k]))
        }
      }
    }
    
    cat('For',type,'location',j,':\n',
        type,'activities accross',
        unlist(person_summary_byloc[[i]]$activity_days_byloc[j])[j],'days\n',
        type,'activities are fixed in',length(date$date),'days:\n',
        'these days are:',as.character(date$date),'\n',
        'and are in:',as.character(date$weekday),'\n',
        'the repetitive interval is:\n')
    print(interval)
  }
}

###get the anchor number for each type of activity for each person----
anchor_num = function(person_fixity_summary,person_summary_byloc,RR){
  #person anchor
  person_fixity_summary$interval_num = 0
  person_fixity_summary$anchor_num = 0
  person_fixity_summary$UserId = as.character(person_fixity_summary$UserId)
  for (i in 1:nrow(person_fixity_summary)) {
    for (j in 1:length(person_summary_byloc)) {
      if(person_fixity_summary$UserId[i]==names(person_summary_byloc)[j]){
        num = 0
        anchor_num = 0
        if(person_fixity_summary$Repetitive_Ratio[i]>=RR){
          for (k in 1:nrow(person_summary_byloc[[j]])) {
            if(-999%in%data.frame(person_summary_byloc[[j]]$interval_byloc[k])==FALSE){
              num = c(num,nrow(data.frame(person_summary_byloc[[j]]$interval_byloc[k])))
              anchor_num = anchor_num +1
            }
          }
          person_fixity_summary$interval_num[i] = max(num)
          person_fixity_summary$anchor_num[i] = anchor_num
        }
      }
    }
  }
  return(person_fixity_summary)
}

