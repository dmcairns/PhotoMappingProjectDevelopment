###############################################
#  Photo Manipulation Tools                   #
###############################################


check.photo.format <- function(filename){
  ##########################################
  # Verify that the file is an image file  #
  ##########################################
  
  t.strsplit <- strsplit(filename, ".", fixed=T)
  suffix <- t.strsplit[[1]][2]
  
  if (!is.na(suffix) & ((suffix=="JPG") | (suffix =="jpg") | (suffix == "HEIC"))) {
    output <- TRUE
  } else output <- FALSE
  ##########################################
  # Eventually could do virus checks here  #
  ##########################################
  output
}
extract.exif.info <- function(filename){
  ##########################################
  # Extract the pieces of information from #
  # EXIF header.                           #
  ##########################################
  t.exif <- read_exif(filename)

  ##########################################
  # Check to see if geotagged              #
  ##########################################  
  t.exif.names <- names(t.exif)
  t.match <- match(c("GPSLatitude", "GPSLongitude"), t.exif.names)
  if(length(t.match)==2){
    if(!is.na(t.match[1]) & !is.na(t.match[2]))
      geotagged.flag <- TRUE
    else geotagged.flag <- FALSE
  } else geotagged.flag <- FALSE
  
  ##########################################
  # Process geotagged data                 #
  ##########################################    
  if(geotagged.flag){
    output <- t.exif %>%
      select("GPSLatitude", "GPSLongitude", "GPSAltitude", "GPSDateTime")
  } else output <- NULL
  output
}
readPhotoCoordinates <- function(path){
  t.dir <- dir(path)
  t.dir.list <- as.list(t.dir)
  t.checked <- unlist(lapply(t.dir, FUN=check.photo.format))
  t.dir <- t.dir[t.checked]
  t.dir <- paste0(path, t.dir)
  t.photo.coordinates <- lapply(t.dir, FUN=extract.exif.info)
  ###############################################
  # Remove any elements that were not geocoded  #
  ###############################################
  t.null <- unlist(lapply(t.photo.coordinates, FUN=is.null))
  t.photo.coordinates <- t.photo.coordinates[!t.null]
  for(i in 1:length(t.photo.coordinates)){
    if(i ==1){
      t.matrix <- matrix(nrow=length(t.photo.coordinates), 
                         ncol=length(t.photo.coordinates[[1]]))
      t.matrix[i,] <- unlist(t.photo.coordinates[[i]])
      t.names <- names(t.photo.coordinates[[i]])
    } else {
      t.matrix[i,] <- unlist(t.photo.coordinates[[i]])
    }
  }
  t.matrix <- data.frame(t.matrix, stringsAsFactors = FALSE)
  names(t.matrix) <- t.names
  for(i in 1:dim(t.matrix)[[2]]){
    if(t.names[i]=="GPSLatitude") t.matrix[,i] <- as.numeric(t.matrix[,i])
    if(t.names[i]=="GPSLongitude") t.matrix[,i] <- as.numeric(t.matrix[,i])
    if(t.names[i]=="GPSAltitude") t.matrix[,i] <- as.numeric(t.matrix[,i])
  }
  t.matrix
}


