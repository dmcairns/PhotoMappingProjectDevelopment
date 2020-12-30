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

modifiedVertices <- na.omit(t.dfDraw) %>%
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

# Plot using base plotting tools
plot(fovPolysSPDF, col=rainbow(50, alpha=0.5))

# Want to identify which photos have overlap.
# Also determine the amount of overlap (% of FOV) that  use st_area?
# overlaps between the two images.  Create a table for
# this and display it in the interface.
# Should be able to pick two photos and display them side-by-side
# to view the overlap.

# fovPolysSF.1 <- fovPolysSF %>%
#   filter(id=="IMG_3335.heic")
# fovPolysSF.2 <- fovPolysSF %>%
#   filter((id=="IMG_3336.heic"))
# fovPolysSF.3 <- fovPolysSF %>%
#   filter((id=="IMG_3336.heic") | (id=="IMG_3337.heic"))
# fovPolysSF.4 <- fovPolysSF %>%
#   filter((id=="IMG_6145.JPG"))


##################################
# Simple 2 polygon intersection  #
##################################
# theIntersection <- st_intersection(fovPolysSF.1, fovPolysSF.2)
# 
# ggplot() +
#   geom_sf(data=fovPolysSF.1, colour="blue", aes(fill=id, alpha=0.1)) +
#   geom_sf(data=fovPolysSF.2, colour="red", aes(fill=id, alpha=0.1)) +
#   geom_sf(data=theIntersection, colour="green", aes(fill=id, alpha=0.1))

############################################
# More complicated 3 polygon intersection  #
############################################
# theIntersection2 <- st_intersection(fovPolysSF.1, fovPolysSF.2)
# ggplot() +
#   geom_sf(data=fovPolysSF.1, colour="blue", aes(fill=id, alpha=0.1)) +
#   geom_sf(data=fovPolysSF.3, colour="red", aes(fill=id, alpha=0.1)) +
#   geom_sf(data=fovPolysSF.4, colour="pink", aes(fill=id, alpha=0.1))
  
############################################
# Spatial join between 1 polygon and many  #
# others.                                  #
#    Produces a list of the polygons with  #
#    intersections.  Stored as id.y        #
#
############################################

# fovSpatialJoin <- st_join(fovPolysSF.4, fovPolysSF.3)

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
  }
}

# t.1 <- determineOverlaps(photoOfInterest="IMG_3335.heic", allPhotos=fovPolysSF)
# 
# plot2Segments <- function(seg1="IMG_3335.heic", seg2="IMG_3335.heic", allPolys=fovPolysSF){
#   fovPolysSF.1 <- fovPolysSF %>%
#     filter(id==seg1)
#   fovPolysSF.2 <- fovPolysSF %>%
#     filter((id==seg2))
#   
#   pOut <- ggplot() +
#     geom_sf(data=fovPolysSF.1, colour="blue", aes(fill=id, alpha=0.1)) +
#     geom_sf(data=fovPolysSF.2, colour="red", aes(fill=id, alpha=0.1)) 
#   pOut
# }
# 
# plot2Segments("IMG_3335.heic", "IMG_3356.heic")

##################################
# Plotting the data with ggplot2 #
##################################
# ggplot(fovPolysSF.3) +
#   geom_sf(colour="black", aes(fill=id, alpha=0.1)) +
#   geom_sf(data=fovPolysSF.2, aes(fill=id, alpha=0.1)) +
#   geom_sf(data=theIntersection2, fill="yellow", colour="blue")




# b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
# b = st_buffer(b, dist = 1) # convert points to circles
# plot(b)
# text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text
# x = b[1]
# y = b[2]
# x_and_y = st_intersection(x, y)
# plot(b)
# plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area