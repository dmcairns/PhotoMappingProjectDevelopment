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
  sampled.photo.coordinates
}

sample.photos.select <- function(photo.coordinates, photo.names){
  ##########################################
  # Samples the photos based off of a list #
  # of selected photos.                    #
  ##########################################
  
  sampled.photo.coordinates <- photo.coordinates%>%
    filter(Name %in% photo.names)
  sampled.photo.coordinates
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
  b
}

create.map <- function(photo.coordinates, bounding.box){
  ##########################################
  #   Create a basemap using the photo     #
  #   coordinates.                         #
  ##########################################
  
  photo.coordinates <- photo.coordinates[,-4]
  selectedPhoto.coordinates <- NULL
  if(!is.null(globalValues$selectedPhoto)){
    selectedPhoto.coordinates <- photo.coordinates %>%
      filter(Name == globalValues$selectedPhoto)
    print(selectedPhoto.coordinates)
    photo.coordinates <- photo.coordinates %>%
      filter(Name != globalValues$selectedPhoto)
  }
  hs.map <- ggmap(get_map(location = bounding.box, zoom=3)) +
    geom_point(shape = 19, color = '#FF0000', data = photo.coordinates, aes(x = GPSLongitude, y = GPSLatitude)) +
    if(!is.null(selectedPhoto.coordinates)){
      geom_point(shape = 19, color = "#4080FF", data = selectedPhoto.coordinates, aes(x = GPSLongitude, y = GPSLatitude))
    }
  hs.map
}

calc.distance <- function(coordinatesdblclick, coordinatesphoto){
  ##########################################
  # Uses the haversine formula to compute  #
  # distances between points.              #
  ##########################################
  
  R <- 6371 #earth's radius in kilometers
  lon1 <- coordinatesdblclick$x * pi / 180
  lat1 <- coordinatesdblclick$y * pi / 180
  lon2 <- coordinatesphoto$GPSLongitude * pi / 180
  lat2 <- coordinatesphoto$GPSLatitude * pi / 180
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- (sin(dlat / 2))^2 + cos(lat1) * cos(lat2) * (sin(dlon / 2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  d
}
