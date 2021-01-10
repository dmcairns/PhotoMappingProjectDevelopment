###############################################
#                Map Creation                 #
###############################################


sample.photos.random <- function(photo.coordinates, num.photos) {
  ##########################################
  # Randomly sample the photos from the    #
  # coordinates provided.                  #
  ##########################################
  
  t.sample <- sample(c(1:dim(photo.coordinates)[1]), num.photos)
  sampled.photo.coordinates <- photo.coordinates[t.sample,]
  return(sampled.photo.coordinates)
}

sample.photos.select <- function(photo.coordinates, photo.names){
  ##########################################
  # Samples the photos based off of a list #
  # of selected photos.                    #
  ##########################################
  
  sampled.photo.coordinates <- photo.coordinates%>%
    filter(Name %in% photo.names)
  return(sampled.photo.coordinates)
}

create.bounding.box <- function(photo.coordinates, percentMargin){
  ##########################################
  # Creates a bounding box around the      #
  # selected photos and adds a % margin on #
  # each side. (Then prevents it from      #
  # being out of scope)                    #
  ##########################################
  
  sampled.photo.coordinates.m <- as.matrix(photo.coordinates[,c(2:1)])
  b <- bbox(sampled.photo.coordinates.m)
  xmin <- b[1]
  ymin <- b[2]
  xmax <- b[3]
  ymax <- b[4]
  b[1] <- xmin - (xmax - xmin) / (100 / percentMargin)
  b[2] <- ymin - (ymax - ymin) / (100 / percentMargin)
  b[3] <- xmax + (xmax - xmin) / (100 / percentMargin)
  b[4] <- ymax + (ymax - ymin) / (100 / percentMargin)
  if(b[1] < -180){
    b[1] <- -180
  }
  if(b[2] < -89){
    b[2] <- -89
  }
  if(b[3] > 180){
    b[3] <- 180
  }
  if(b[4] > 89){
    b[4] <- 89
  }
  globalValues$b.box <- b
  return(b)
}

create.map <- function(photo.coordinates, bounding.box, fovDist, fovDraw, dfDrawOut){
  #define dfDrawOut as pass by reference, see DataManagement.R for method
  byRef(dfDrawOut)
  
  ##########################################
  #   Create a basemap using the photo     #
  #   coordinates.                         #
  ##########################################
  
  photo.coordinates <- photo.coordinates[,-4]
  selectedPhoto.coordinates <- NULL
  if(!is.null(globalValues$selectedPhoto)){
    selectedPhoto.coordinates <- photo.coordinates %>%
      filter(Name == globalValues$selectedPhoto)
  }
  photo.coordinates$fovLeft <- (photo.coordinates$GPSImgDirection - (photo.coordinates$FOV / 2)) %% 360
  photo.coordinates$fovRight <- (photo.coordinates$GPSImgDirection + (photo.coordinates$FOV / 2)) %% 360
  fovPosLeft <- calc.pointAtDistance(photo.coordinates, photo.coordinates$fovLeft, fovDist)
  fovPosRight <- calc.pointAtDistance(photo.coordinates, photo.coordinates$fovRight, fovDist)
  fovPosCenter <- calc.pointAtDistance(photo.coordinates, photo.coordinates$GPSImgDirection, fovDist)
  dfDraw <- data.frame("Name" = photo.coordinates$Name,
                       "Lon" = photo.coordinates$GPSLongitude,
                       "Lat" = photo.coordinates$GPSLatitude,
                       "FOVLeftLon" = fovPosLeft$lon,
                       "FOVLeftLat" = fovPosLeft$lat,
                       "FOVRightLon" = fovPosRight$lon,
                       "FOVRightLat" = fovPosRight$lat,
                       "FOVCenterLon" = fovPosCenter$lon,
                       "FOVCenterLat" = fovPosCenter$lat,
                       "FOV" = photo.coordinates$FOV)
  dfDrawOut <- dfDraw
  if(fovDraw){
    if(!is.null(selectedPhoto.coordinates)){
      hs.map <- ggmap(get_map(location = bounding.box, zoom=3)) +
        geom_point(shape = 19, color = "#FF0000", data = dfDraw, aes(x = Lon, y = Lat)) +
        geom_segment(data = dfDraw, aes(x = FOVRightLon, y = FOVRightLat, xend = FOVCenterLon, yend = FOVCenterLat, color = "FF0000")) +
        geom_segment(data = dfDraw, aes(x = FOVLeftLon, y = FOVLeftLat, xend = FOVCenterLon, yend = FOVCenterLat, color = "FF0000")) +
        geom_point(shape = 19, color =  "#4080FF", data = selectedPhoto.coordinates, aes(x = GPSLongitude, y = GPSLatitude))
    } else {
      hs.map <- ggmap(get_map(location = bounding.box, zoom=3)) +
        geom_point(shape = 19, color = "#FF0000", data = dfDraw, aes(x = Lon, y = Lat)) +
        geom_segment(data = dfDraw, aes(x = FOVRightLon, y = FOVRightLat, xend = FOVCenterLon, yend = FOVCenterLat, color = "FF0000")) +
        geom_segment(data = dfDraw, aes(x = FOVLeftLon, y = FOVLeftLat, xend = FOVCenterLon, yend = FOVCenterLat, color = "FF0000"))
    }
  } else {
    if(!is.null(selectedPhoto.coordinates)){
      hs.map <- ggmap(get_map(location = bounding.box, zoom=3)) +
        geom_point(shape = 19, color = "#FF0000", data = dfDraw, aes(x = Lon, y = Lat)) +
        geom_point(shape = 19, color = "#4080FF", data = selectedPhoto.coordinates, aes(x = GPSLongitude, y = GPSLatitude))
    } else {
      hs.map <- ggmap(get_map(location = bounding.box, zoom=3)) +
        geom_point(shape = 19, color = "#FF0000", data = dfDraw, aes(x = Lon, y = Lat))
    }
  }
  return(hs.map)
}

#calculation methods derived from formulas given here: https://www.movable-type.co.uk/scripts/latlong.html

calc.distance <- function(coordinatesdblclick, coordinatesphoto){
  ##########################################
  # Uses the haversine formula to compute  #
  # distances between points.              #
  ##########################################
  
  R <- 6371 #earth's average radius in kilometers
  lon1 <- coordinatesdblclick$x * pi / 180
  lat1 <- coordinatesdblclick$y * pi / 180
  lon2 <- coordinatesphoto$GPSLongitude * pi / 180
  lat2 <- coordinatesphoto$GPSLatitude * pi / 180
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- (sin(dlat / 2))^2 + cos(lat1) * cos(lat2) * (sin(dlon / 2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}

calc.pointAtDistance <- function(coordinatesPoint, angle, distance){
  ##########################################
  # Calculates a second point a given      #
  # distance and heading away from a first #
  # point.                                 #
  ##########################################
  
  R <- 6371 #earth's average radius in kilometers
  lon1 <- coordinatesPoint$GPSLongitude * pi / 180
  lat1 <- coordinatesPoint$GPSLatitude * pi / 180
  heading <- angle * pi / 180
  angularDistance <- distance / R
  lat2 <- asin(sin(lat1) * cos(angularDistance) + cos(lat1) * sin(angularDistance) * cos(heading))
  lon2 <- lon1 + atan2(sin(heading) * sin(angularDistance) * cos(lat1), cos(angularDistance) - sin(lat1) * sin(lat2))
  output <- NULL
  output$name <- coordinatesPoint$Name
  output$lat <- lat2 * 180 / pi
  output$lon <- lon2 * 180 / pi
  return(output)
}
