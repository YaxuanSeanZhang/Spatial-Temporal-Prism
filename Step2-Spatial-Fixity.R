#Spatial Indicator Calculation----
###Spatial region/Activity Space (minimum convex polygon)----
for (i in 1:length(activity$UserId)) {
  if(is.na(activity$TripId[i])==FALSE){
    record = search_route(activity[i,'UserId'],
                          activity[i,'TripId'],route_update)
    record <- cbind(record, activity[i,'UserId'])
    record <- cbind(record, activity[i,'TripId'])
    colnames(record)[3:4]=c('UserId','TripId')
    if(record$lat[1]!=-999){
      if(length(unique(record)$lat)<3){
        activity[i,'area']=activity[i,'distance']*0.001
        activity[i,'perimeter'] = activity[i,'distance']*2 + 0.002
        activity[i,'line'] = TRUE
      }
      else if(length(record$lat)<5){
        record <- rbind(record,unique(record)[2:3,])
        record <- cbind(record,LongLatToUTM(record$lon,record$lat,15))
        coordinates(record) <- record[, c('X', 'Y')]
        activity[i,'area'] = sum(as.data.frame(mcp(
          record[,4], unin = 'm', unout = 'km2', percent=100))[,2])
        activity[i,'perimeter'] = polyPerimeter(mcp(
          record[,4], unin = 'm',unout = 'km2',percent=100))/1000
        activity[i,'line'] = FALSE
      }
      else{
        record <- cbind(record,LongLatToUTM(record$lon,record$lat,15))
        coordinates(record) <- record[, c('X', 'Y')]
        activity[i,'area'] = sum(as.data.frame(mcp(
          record[,4], unin = 'm',unout = 'km2',percent=100))[,2])
        activity[i,'perimeter'] = polyPerimeter(mcp(
          record[,4], unin = 'm',unout = 'km2',percent=100))/1000
        activity[i,'line'] = FALSE
      }
    }
  }
}

#get SRArea (squre root of spatial region)
for(i in 1:length(activity$TripId)){
  if(activity$area[i]>0){
    activity[i,'SRArea']=sqrt(activity[i,'area'])
  }
  else{
    activity[i,'SRArea']=0
  }
}

###get Maximum Distance to Mean center-----
#get mean center
for (i in 1:length(activity$UserId)) {
  record = search_route(activity[i,'UserId'],activity[i,'TripId'],route_update)
  if(record$lat[1]!=-999){
    activity[i,'mean_lat'] = mean_center(record)[1]
    activity[i,'mean_lon'] = mean_center(record)[2]
  }
  else{
    print(i)
  }
}

#get distance to the mean center
dist_to_mean_list = split(activity[which(is.na(activity$TripId)==FALSE),],
                          activity$TripId[which(is.na(activity$TripId)==FALSE)])

for (i in 1:length(activity$UserId)) {
  distance = 0
  if(is.na(activity$TripId[i])==FALSE){
    #names(dist_to_mean[[i]]) = actr[i,'TripId']
    name = as.character(activity[i,'TripId'])
    record = search_route(activity[i,'UserId'],activity[i,'TripId'],route_update)
    dist_to_mean_list[[name]] = data.frame(NA,NA,NA)
    colnames(dist_to_mean_list[[name]]) = c('lat','lon','dist')
    if(record$lat[1]!=-999){
      for (j in 1:length(record$lat)) {
        dist = distance84(activity[i,'mean_lat'],activity[i,'mean_lon'],
                          record[j,1],record[j,2])
        distance = c(distance,dist)
        dist_to_mean_list[[name]] = rbind(dist_to_mean_list[[name]],
                                          c(record[j,1],record[j,2],dist))
      }
    }
    else{
      dist = distance84(activity[i,'mean_lat'],activity[i,'mean_lon'],
                        activity[i,'startlat'],activity[i,'startlon'])
      distance = c(distance,dist)
      dist_to_mean_list[[name]] = rbind(dist_to_mean_list[[name]],
                                        c(activity[i,'startlat'],
                                          activity[i,'startlon'],dist))
      dist = distance84(activity[i,'mean_lat'],activity[i,'mean_lon'],
                        activity[i,'endlat'],activity[i,'endlon'])
      distance = c(distance,dist)
      dist_to_mean_list[[name]] = rbind(dist_to_mean_list[[name]],
                                        c(activity[i,'endlat'],
                                          activity[i,'endlon'],dist))
    }
    distance = distance[-1]
    dist_to_mean_list[[name]] = dist_to_mean_list[[name]][-1,]
    activity[i,'AvgDMean'] = mean(distance)
    activity[i,'MinDMean'] = min(distance)
    activity[i,'MaxDMean'] = max(distance)
    activity[i,'SdDMean'] = sd(distance)    
  }
}

#Classify stationary and non-stationary activity----
#get valid one type of activity data
activity_onetype = activity[which(
  activity$subtype==activity_type & 
    activity$UserId %in% actrperson_summary$UserId),]

#data for classification (exclude predefined stationary activity)
activity_sub = activity_onetype[which(activity_onetype$distance>0.005 | 
                                 activity_onetype$SRArea>sqrt(pi)*0.005 | 
                                 activity_onetype$MaxDMean > 0.005),]
activity_cluster = activity_sub[,c('distance','SRArea','MaxDMean')]

##fit Gaussian finite mixture model
gmm_optimal <- Mclust(activity_cluster)
plot(gmm_optimal, what = 'BIC')  #cluster number: 3
gmm <- Mclust(activity_cluster,3)
gmm = cbind(activity_sub,group = as.character(gmm$classification))
table(gmm$group)
gmm$group = as.character(gmm$group)

#clustering result visualization 
cols    <- c( "1" = "Coral2", "2" = "goldenrod2", "3" = "springgreen3", 
              "4" = "deepskyblue3","5" = "mediumpurple2", "6" = 'bisque4')

ggplot(gmm, aes(x = distance, y = SRArea, col = group)) + 
  geom_point(size = 1) + coord_equal(xlim=c(0,0.35),ylim=c(0,0.35)) +
  scale_color_manual(name = "group", breaks = c('1','2','3'), 
                     values = cols,labels = c('1','2','3'))

#add predefined stationary group 
activity_stationary = rbind(gmm,
            cbind(activity_onetype[which(
              activity_onetype$distance<=0.005 & 
                activity_onetype$SRArea<=sqrt(pi)*0.005 & 
                activity_onetype$MaxDMean <= 0.005),],group=0))

#Representative Activity Location Extract----------------------
#get representative activity location (spatial fixity)
get_location = function(activity){
  group = length(table(activity$group)) - 1
  
  #construct list and summary
  activity_person = split(activity,activity$UserId)
  
  person_summary = data.frame(UserId=NULL,stationary=NULL,nonstationary=NULL)
  for(i in 1:length(activity_person)){
    person_summary = rbind(
      person_summary,
      data.frame(UserId = activity_person[[i]][1,'UserId'],
                 stationary= nrow(activity_person[[i]][which(
                   activity_person[[i]]$group!=3),]),
                 nonstationary= nrow(activity_person[[i]][which(
                   activity_person[[i]]$group==3),])))
  }
  
  #get threshold
  threshold = max(c(Rfast::nth(c(Hmisc::summarize(
    activity$distance,
    activity$group,max)[
      ,'activity$distance']),2, descending = T),
    Rfast::nth(c(Hmisc::summarize(
      activity$SRArea,
      activity$group,max)[
        ,'activity$SRArea']),2, descending = T),
    Rfast::nth(c(Hmisc::summarize(
      activity$MaxDMean,
      activity$group,max)[
        ,'activity$MaxDMean']),2, descending = T)))
  
  print(threshold)
  
  person_location = activity_person  #cluster center
  person_Clust = activity_person #points & cluster group
  
  for (i in 1:length(activity_person)) {
    person_location[[i]] = cbind(person_summary[i,],
                                    data.frame(Lat = NA, Lon = NA,Clust = NA))
    points = activity_person[[i]][which(
      activity_person[[i]]$group!=group),
                         c('mean_lon','mean_lat')]
    if(nrow(points)>1){
      xy <- SpatialPointsDataFrame(
        points, 
        data.frame(ID=seq(1:nrow(points))),
        proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
      xy <- spTransform(xy, CRS("+init=epsg:26915"))
      chc <- hclust(dist(data.frame(rownames=rownames(xy@data), 
                                    x=coordinates(xy)[,1],
                                    y=coordinates(xy)[,2])), 
                    method="complete")
      chc.d <- cutree(chc, h=2000 * threshold) 
      person_Clust[[i]] <- data.frame(person_Clust[[i]][which(
        person_Clust[[i]]$group!=group),], Clust=chc.d) #only stationary
      Clust = data.frame(table(person_Clust[[i]]$Clust)) 
      
      for (j in 1:nrow(Clust)) {
        points = person_Clust[[i]][which(
          person_Clust[[i]]$Clust==Clust$Var1[j]),c('mean_lat','mean_lon')]
        if(Clust$Freq[j]<2){  #label clusters only show 1 time
          points = person_Clust[[i]][which(
            person_Clust[[i]]$Clust==Clust$Var1[j]),c('mean_lat','mean_lon')]
          person_location[[i]] = rbind(
            person_location[[i]],
            cbind(person_summary[i,],
                  data.frame(Lat = points$mean_lat, 
                             Lon = points$mean_lon, 
                             Clust = Clust$Var1[j])))
        }
        else{
          person_location[[i]] = rbind(
            person_location[[i]],
            cbind(person_summary[i,],
                  data.frame(Lat = mean_center(points)[1], 
                             Lon = mean_center(points)[2],
                             Clust = Clust$Var1[j])))
        } 
      }
    }
    else if(nrow(points)==1){
      person_location[[i]] = rbind(
        person_location[[i]],
        cbind(person_summary[i,],
              data.frame(Lat = points$mean_lat, 
                         Lon = points$mean_lon,Clust=1)))
      person_Clust[[i]] <- data.frame(person_Clust[[i]][
        which(person_Clust[[i]]$group!=group),], Clust=1)  #only stationary
    }
    else{
      person_Clust[[i]] <- data.frame(person_Clust[[i]][
        which(person_Clust[[i]]$group!=group),], Clust=NULL)
    }
    person_location[[i]] = person_location[[i]][-1,]
    if(nrow(activity_person[[i]][which(
      activity_person[[i]]$group==group),])>0){
      activity_person[[i]] = rbind(
        person_Clust[[i]],
        data.frame(activity_person[[i]][
          which(activity_person[[i]]$group==group),], Clust=-999))
      activity_person[[i]] = activity_person[[i]][order(
        activity_person[[i]]$starttime),]
    }
    else{
      activity_person[[i]] = person_Clust[[i]]
    }
  }
  
  for(i in 1:nrow(person_summary)){
    person_summary$spatial_location[i] = nrow(person_location[[i]])
  }
  
  my_list = list(summary = person_summary,
                 activity_list = activity_person,
                 location_list = person_location)
  return(my_list)
}

my_list_spatial = get_location(activity_stationary)
#number of representative activity locations for each person
person_fixity_summary = my_list_spatial[[1]]
#detailed activity information for each person
activity_person = my_list_spatial[[2]]
#representative activity location(s) for each person
representative_location = my_list_spatial[[3]]  
#number of representative activity locations
table(person_fixity_summary$spatial_location)  