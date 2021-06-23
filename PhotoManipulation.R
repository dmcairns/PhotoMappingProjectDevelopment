###############################################
#  Photo Manipulation Tools                   #
###############################################

check.photo.format <- function(filename){
  ##########################################
  # Verify that the file is an image file  #
  ##########################################
  
  t.strsplit <- strsplit(filename, ".", fixed=T)
  suffix <- t.strsplit[[1]][2]
  return((!is.na(suffix) & ((suffix=="JPG") | (suffix =="jpg") | (suffix == "HEIC") | (suffix == "heic") | (suffix == "PNG") | (suffix == "png"))))
}

extract.exif.info <- function(filename){
  ##########################################
  # Extract the pieces of information from #
  # EXIF header.                           #
  ##########################################
  
  t.exif <- read_exif(filename)
  t.exif.names <- names(t.exif)
  t.matchCoords <- match(c("GPSLatitude", "GPSLongitude"), t.exif.names)
  t.matchAltitude <- match("GPSAltitude", t.exif.names)
  t.matchDateTime <- match("GPSDateTime", t.exif.names)
  t.matchHeading <- match("GPSImgDirection", t.exif.names)
  t.matchFOV <- match("FOV", t.exif.names)
  
  if(!is.na(t.matchCoords[1]) && !is.na(t.matchCoords[2])){
    output <- t.exif %>%
      select("GPSLatitude", "GPSLongitude")
  } else output <- data.frame(GPSLatitude = c(NA), GPSLongitude = c(NA))
  
  if(!is.na(t.matchAltitude[1])){
    o <- t.exif %>%
      select("GPSAltitude")
  } else o <- NA
  output$GPSAltitude <- o
  
  if(!is.na(t.matchDateTime[1])){
    o <- t.exif %>%
      select("GPSDateTime")
  } else o <- NA
  output$GPSDateTime <- o
  
  if(!is.na(t.matchHeading[1])){
    o <- t.exif %>%
      select("GPSImgDirection")
  } else o <- NA
  output$GPSImgDirection <- o
  
  if(!is.na(t.matchFOV[1])){
    o <- t.exif %>%
      select("FOV")
  } else o <- NA
  output$FOV <- o
  
  return(output)
}

readPhotoCoordinates <- function(path){
  ##########################################
  # Creates a matrix with containing exif  #
  # data of all the image files in the     #
  # folder.                                #
  ##########################################
  
  t.dir <- dir(path)
  t.dir.list <- as.list(t.dir)
  t.checked <- unlist(lapply(t.dir, FUN=check.photo.format))
  t.dir <- t.dir[t.checked]
  t.dir <- paste0(path, t.dir)
  t.photo.coordinates <- lapply(t.dir, FUN=extract.exif.info)
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
    if(t.names[i]=="GPSImgDirection") t.matrix[,i] <- as.numeric(t.matrix[,i])
    if(t.names[i]=="FOV") t.matrix[,i] <- as.numeric(t.matrix[,i])
  }
  t.matrix$Name <- readPhotoNames(path)
  return(t.matrix)
}

readPhotoNames <- function(path){
  ##########################################
  # Produces a list of file names ending   #
  # in '.jpg', '.JPG', or '.HEIC'.         #
  ##########################################
  
  return(list.files(path, pattern = '.*(\\.(?i)(jpg|heic|png))$'))
}

photosInsideBoundingBox <- function(photos){
  ##########################################
  # Produces a list of all photos          #
  # contained within the confines of the   #
  # boudning box.                          #
  ##########################################
  
  photosIn <- photos %>%
    filter(GPSLongitude > globalValues$b.box[1] & GPSLongitude < globalValues$b.box[3])
  photosIn <- photosIn %>%
    filter(GPSLatitude > globalValues$b.box[2] & GPSLatitude < globalValues$b.box[4])
  names <- photosIn$Name
  return(names)
}

getNewDims <- function(h, w, mh, mw){
  byRef(h, w)
  if((h * mw) > (w * mh)){
    w <- "auto"
    h <- "100%"
  } else {
    h <- "auto"
    w <- "100%"
  }
}

rotateImage <- function(path){
  im <- readImage(path)
  orientation <- read_exif(path, tags="Orientation")
  switch(orientation[[2]] - 1,
         two = {
           im <- flipImage(im, mode = 'horizontal')
         },
         three = {
           im <- rotateFixed(im, 180)
         },
         four = {
           im <- rotateFixed(im, 180)
           im <- flipImage(im, mode = 'horizontal')
         },
         five = {
           im <- rotateFixed(im, 90)
           im <- flipImage(im, mode = 'horizontal')
         },
         six = {
           im <- rotateFixed(im, 90)
         },
         seven = {
           im <- rotateFixed(im, 270)
           im <- flipImage(im, mode = 'horizontal')
         },
         eight = {
           im <- rotateFixed(im, 270)
         })
  writeImage(im, path)
  return(dim(im))
}

convertJPG <- function(filename){
  ###############################
  # Converts an image to JPG    #
  # to then be used by          #
  # OpenImageR; returns the     #
  # filepath.                   #
  ###############################
  
  extension <- regmatches(filename, regexpr("[^.]*$", filename))
  filenameWithJPGExt <- paste(substr(filename, 1, nchar(filename) - nchar(extension) - 1), "jpg.", "jpg", sep = "")
  image_write(image_read(filename), path = filenameWithJPGExt, format = "jpg")
  return(filenameWithJPGExt)
}

getStitchPath <- function(jpgFilepaths){
  for(i in 1:2){
    jpgFilepaths[i] <- paste(substr(jpgFilepaths[i], 1, nchar(jpgFilepaths[i]) - 7))
    jpgFilepaths[i] <- regmatches(jpgFilepaths[i], regexpr("[^\\]+$", jpgFilepaths[i]))
  }
  return(paste(normalizePath(file.path("Data", "Panoramas")), "\\", jpgFilepaths[1], "-", jpgFilepaths[2], "jpg.jpg", sep=""))
}

stitch <- function(jpgFilepaths, outputJPGPath){
  source_python("stitch.py")
  return(stitchImagesBasic(jpgFilepaths, outputJPGPath))
}