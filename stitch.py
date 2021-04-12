import cv2 as cv

def stitchImages(filepaths, outputpath):
    imgs = []
    for filepath in filepaths:
        img = cv.imread(cv.samples.findFile(filepath))
        if img is None:
            return(-1)
        imgs.append(img)
    stitcher = cv.Stitcher_create(mode = 1)
    status, panorama = stitcher.stitch(imgs)
    if(status == 0):
        cv.imwrite(outputpath, panorama)
    return(status)