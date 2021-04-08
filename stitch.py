import cv2 as cv

def stitchImages(filepaths, outputpath, silent = True):
    imgs = []
    for filepath in filepaths:
        img = cv.imread(cv.samples.findFile(filepath))
        if img is None:
            if(not silent):
                print("Error reading image contained at:", filepath)
            return(-1)
        imgs.append(img)
    stitcher = cv.Stitcher_create(mode = 1)
    status, panorama = stitcher.stitch(imgs)
    if(status != 0):
        if(not silent):
            print("Error in stitching. OpenCV stitcher error code:", status)
        return(status)
    cv.imwrite(outputpath, panorama)
    return(0)