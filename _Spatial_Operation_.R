#read route trajectory-----
search_route = function(UserId,TripId,route){
  for (i in 1:length(route)) {
    if(names(route)[i] == as.character(UserId)){
      a = i
      for (j in 1:length(route[[i]])) {
        if(names(route[[i]])[j]==as.character(TripId)){
          b = j
        }
      }
    }
  }
  record = data.frame(route[[a]][b])
  colnames(record)=c('lat','lon')
  return(record)
}

#calculate Euclidean distance in km----
distance84 = function(lat1,long1,lat2,long2){  
  #r = 6378.137
  r = 6371.001
  x1 = lat1 * pi / 180
  x2 = lat2 * pi / 180
  y1 = long1 * pi / 180
  y2 = long2 * pi / 180
  dx = abs(x1 - x2)
  dy = abs(y1 - y2)
  p = 2 * asin(sqrt((sin(dx/2)**2) + cos(x1)*cos(x2)*(sin(dy/2)**2)))
  return (round(r *p *10000)/10000)
}

#calculate Euclidean distance between two points----
distance2pnts = function(p1,p2){
  #r = 6378.137
  r = 6371.001
  x1 = p1[2] * pi / 180
  x2 = p2[2] * pi / 180
  y1 = p1[1] * pi / 180
  y2 = p2[1] * pi / 180
  dx = abs(x1 - x2)
  dy = abs(y1 - y2)
  p = 2 * asin(sqrt((sin(dx/2)**2) + cos(x1)*cos(x2)*(sin(dy/2)**2)))
  return (round(r *p *10000)/10000)
}

#project to UTM zone----
LongLatToUTM = function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
  res <- spTransform(xy, CRS("+init=epsg:26915"))
  return(as.data.frame(res))
}

#get the midpoint of two locations----
midpoint = function(lat1,lon1,lat2,lon2){
  point1 = st_point(c(lon1,lat1))
  point2 = st_point(c(lon2,lat2))
  new_point1 = st_sfc(point1, point2, 
                      crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_point2 = st_transform(new_point1,26915)
  point = data.frame(c(unlist(new_point2)[1],unlist(new_point2)[3]),
                     c(unlist(new_point2)[2],unlist(new_point2)[4]))
  names(point) = c('lon','lat')
  midpoint = st_point(c((point$lon[1] + point$lon[2])/2,
                        (point$lat[1] + point$lat[2])/2))
  midpoint = st_sfc(midpoint, 
                    crs = "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs")
  midpoint = st_transform(midpoint,4326)
  return(c(unlist(midpoint)[2],unlist(midpoint)[1]))
}

#get the mean center of one route-----
mean_center = function(route){
  n = length(route[,1])
  if(n>1){
    route = route[,c(2,1)]
    point = st_multipoint(as.matrix(route))
    new_point1 = st_sfc(point, crs = "+proj=longlat +datum=WGS84 +no_defs")
    new_point2 = st_transform(new_point1,26915)
    new_point = data.frame(new_point2[[1]][1:n,])
    names(new_point) = c('lon','lat')
    mean_center = st_point(c(mean(new_point$lon),mean(new_point$lat)))
    mean_center = st_sfc(mean_center, 
                         crs = "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs")
    mean_center = st_transform(mean_center,4326)
    return(c(unlist(mean_center)[2],unlist(mean_center)[1]))
  }
  else{
    return(route)
  }
}











