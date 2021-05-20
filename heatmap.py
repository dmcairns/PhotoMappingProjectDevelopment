import cv2 as cv
import functools
import math
from multiprocessing.dummy import Pool
from numba import njit
from numba.typed import List
import numpy as np
import os
from PIL import Image
import re

def matchKPs(filepaths):
    #filepaths: [img1path, img2path, imgoverlappath]

    orb = cv.ORB_create()
    bf = cv.BFMatcher()

    imgs = []
    kps = []
    dess = []
    for filepath in filepaths:
        img = cv.imread(filepath)
        imgs.append(img)
        gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
        kp, des = orb.compute(gray, orb.detect(gray, None))
        kps.append(kp)
        dess.append(des)

    matches = []
    for i in range(2):
        matches.append(bf.knnMatch(dess[i], dess[2], k = 2))

    matchesMask = [[[0, 0] for i in range(len(matches[j]))] for j in range(2)]
    for j in range(2):
        for i, (m, n) in enumerate(matches[j]):
            if m.distance < 0.7 * n.distance:
                matchesMask[j][i][0] = 1

    kpMatches = [[[[], []] for i in range(2)] for j in range(2)]
    for j in range(2):
        for i in range(len(matches[j])):
            kpMatches[0][j][matchesMask[j][i][0]].append(kps[j][matches[j][i][0].queryIdx])
            kpMatches[1][j][matchesMask[j][i][0]].append(kps[2][matches[j][i][0].trainIdx])

    #why does opencv use BGR instead of RGB!
    draw_params = [dict(color = (0, 255, 0),
                        flags = cv.DrawMatchesFlags_DRAW_RICH_KEYPOINTS),
                   dict(color = (255, 0, 0),
                        flags = cv.DrawMatchesFlags_DRAW_RICH_KEYPOINTS)]

    for i in range(2):
        imgs.append(cv.cvtColor(imgs[2], cv.COLOR_BGR2GRAY))
        imgs[i] = cv.drawKeypoints(imgs[i], kpMatches[0][i][1], None, **draw_params[i])
        imgs[2] = cv.drawKeypoints(imgs[2], kpMatches[1][i][1], None, **draw_params[i])
        imgs[3] = cv.drawKeypoints(imgs[3], kpMatches[1][i][1], None, **draw_params[i])

    wd = os.getcwd()
    fileOutNames = []
    fileOutNames.append(wd + "\Data\Photos\kp" + re.split("\\\\", filepaths[0])[-1])
    fileOutNames.append(wd + "\Data\Photos\kp" + re.split("\\\\", filepaths[1])[-1])
    fileOutNames.append(wd + "\Data\Panoramas\kp" + re.split("\\\\", filepaths[2])[-1])
    fileOutNames.append(wd + "\Data\Panoramas\g" + re.split("\\\\", fileOutNames[2])[-1])
    fileOutNames.append(wd + "\Data\Panoramas\g" + re.split("\\\\", filepaths[2])[-1])
    for i in range(5):
        cv.imwrite(fileOutNames[i], imgs[i])

    return(kpMatches, fileOutNames)

def createHeatMap(kpSetLeft, kpSetRight, imgPath):
    img = Image.open(imgPath)
    width, height = img.size
    maxDist = math.dist([0, 0], [width, height])
    npHeatMap = np.zeros((height, width))
    for r in range(height):
        print(r, "/", height)
        for c in range(width):
            sum = 0
            for kp in kpSetLeft:
                sum += 1 - math.dist([c, r], [kp.pt[0], kp.pt[1]]) / maxDist
            for kp in kpSetRight:
                sum -= 1 - math.dist([c, r], [kp.pt[0], kp.pt[1]]) / maxDist
            npHeatMap[r, c] = sum
    return(npHeatMap)

def createHeatMapMT(kpSetLeft, kpSetRight, imgPath):
    img = Image.open(imgPath)
    width, height = img.size

    kpL = List()
    kpR = List()
    [kpL.append(List([kp.pt[0], kp.pt[1]])) for kp in kpSetLeft]
    [kpR.append(List([kp.pt[0], kp.pt[1]])) for kp in kpSetRight]

    maxDist = math.dist([0, 0], [width, height])
    npHeatMap = [[[x, y] for x in range(width)] for y in range(height)]
    for r in range(height):
        print(r, "/", height)
        typedHeatMapRow = List()
        for c in npHeatMap[r]:
            typedHeatMapRowC = List()
            [typedHeatMapRowC.append(element) for element in c]
            typedHeatMapRow.append(typedHeatMapRowC)
        with Pool(32) as pool:
            npHeatMap[r] = pool.map(functools.partial(njit(createHeatMapMTHelper, nogil = True), kpL = kpL, kpR = kpR, mD = maxDist), typedHeatMapRow)
    return (np.asarray(npHeatMap))

def createHeatMapMTHelper(coords, kpL, kpR, mD):
    sum = 0
    for kp in kpL:
        sum += 1 - math.sqrt((coords[0] - kp[0]) ** 2 + (coords[1] - kp[1]) ** 2) / mD
    for kp in kpR:
        sum -= 1 - math.sqrt((coords[0] - kp[0]) ** 2 + (coords[1] - kp[1]) ** 2) / mD
    return(sum)

def addHeatMapOverlay(inPath, outPath, heatmap, intensity):
    img = cv.imread(inPath)
    heatmapOverlay = None
    heatmapOverlay = cv.normalize(heatmap, heatmapOverlay, alpha = 0, beta = 255, norm_type = cv.NORM_MINMAX, dtype = cv.CV_8U)
    heatmapOverlay = cv.applyColorMap(heatmapOverlay, cv.COLORMAP_WINTER)
    superimposed = img * (1 - intensity) + heatmapOverlay * intensity
    cv.imwrite(outPath, superimposed)

#test driver code
paths = [r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Photos\DSC_0870.JPG',
         r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Photos\DSC_0871.JPG',
         r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Panoramas\DSC_0871-DSC_0870jpg.jpg']

kpMatch, kpImgNames = matchKPs(paths)
heatmap = createHeatMapMT(kpMatch[1][0][1], kpMatch[1][1][1], kpImgNames[3])
addHeatMapOverlay(kpImgNames[3], "superimposedGKP.jpg", heatmap, 0.5)
addHeatMapOverlay(kpImgNames[4], "superimposedG.jpg", heatmap, 0.5) # bad practice, heatmap was created for kpImgNames[3], but works because kpImgNames[2, 3, and 4] all have the same size