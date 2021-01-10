#############################
# Perform GIS functions     #
#############################

require(tidyr)
require(dplyr)
require(sf)
require(sp)

#############################
# Convert the FOV dataframe #
# into a table that can be  #
# used to create polygons   #
#############################

createPolysSF <- function(df){
  modifiedVertices <- na.omit(df) %>%
    select("Name", "Lon", "Lat", "FOVLeftLon", "FOVLeftLat", "FOVCenterLon", "FOVCenterLat", "FOVRightLon", "FOVRightLat")
  ID <- modifiedVertices$Name
  modifiedVertices <- modifiedVertices %>%
    select("Lon", "Lat", "FOVLeftLon", "FOVLeftLat", "FOVCenterLon", "FOVCenterLat", "FOVRightLon", "FOVRightLat") %>%
    as.matrix()
  
  fovPolys <- SpatialPolygons(mapply(function(poly, id) {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, split(modifiedVertices, row(modifiedVertices)), ID))
  
  # Create SpatialPolygonsDataFrame (SPDF)
  fovPolysSPDF <- SpatialPolygonsDataFrame(fovPolys, data.frame(id=ID, row.names=ID))
  
  # Convert to sf data format
  fovPolysSF <- st_as_sf(fovPolysSPDF)
  return(fovPolysSF)
}

determineOverlaps <- function(photoOfInterest, allPhotos){
  singlePhoto <- allPhotos %>%
    filter(id == photoOfInterest)
  if(nrow(singlePhoto) > 0){
    fovSpatialJoin <- st_join(singlePhoto, allPhotos)
    
    ###############################
    # calculate the % overlap for #
    # each polygon                #
    ###############################
    
    indices <- as.numeric(fovSpatialJoin$id.y)
    fileNames <- fovSpatialJoin$id.y
    refArea <- st_area(singlePhoto)
    thePercentages <- c(NULL)
    for(i in 1:length(fovSpatialJoin$id.y)){
      # how much intersected area is there between polygons
      targetPhoto <- allPhotos %>%
        filter(id==fovSpatialJoin$id.y[i])
      theIntersection <- st_intersection(singlePhoto, targetPhoto)
      theArea <- st_area(theIntersection)
      thePercentage <- theArea/refArea
      thePercentages <- c(thePercentages, thePercentage)
    }
    return(data.frame(image=fovSpatialJoin$id.y, pct.overlap=thePercentages))
  } else {
    print("NA Value For FOV of Selected Photo")
    return(NULL)
  }
}