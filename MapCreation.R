###############################################
#                Map Creation                 #
###############################################

sample.photos <- function(photo.coordinates, num.photos) {
  ##########################################
  # Randomly sample the photos from the    #
  # coordinates provided.                  #
  ##########################################
  
  t.sample <- sample(c(1:dim(photo.coordinates)[1]), num.photos)
  sampled.photo.coordinates <- photo.coordinates[t.sample,]
  sampled.photo.coordinates
}

create.bounding.box <- function(photo.coordinates){
  sampled.photo.coordinates.m <- as.matrix(photo.coordinates[,c(2:1)])
  b <- bbox(sampled.photo.coordinates.m)
  globalValues$b.box <- b
  b
}

create.map <- function(photo.coordinates, bounding.box){
  ##########################################
  #   Create a basemap using the photo     #
  #   coordinates.                         #
  ##########################################
  print("In create.map")
  photo.coordinates <- photo.coordinates[,-4]
  hs.map <- ggmap(get_map(location = bounding.box, zoom=3)) +
    geom_point(data=photo.coordinates, aes(x=GPSLongitude, y=GPSLatitude)) 
  hs.map
}

