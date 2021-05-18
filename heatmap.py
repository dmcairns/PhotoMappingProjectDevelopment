import cv2 as cv
import math
import matplotlib.pyplot as plt
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
        imgs[i] = cv.drawKeypoints(imgs[i], kpMatches[0][i][1], None, **draw_params[i])
        imgs[2] = cv.drawKeypoints(imgs[2], kpMatches[1][i][1], None, **draw_params[i])

    filenames = []
    wd = os.getcwd()
    for i in range(3):
        filenames.append(wd + "\Data\Photos\kp" + re.split("\\\\", filepaths[i])[-1])
        cv.imwrite(filenames[i], imgs[i])

    return(kpMatches, filenames)

def createHeatmap(kpSetLeft, kpSetRight, img):
    img = Image.open(img)
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
    plt.imshow(npHeatMap, cmap='winter')
    plt.colorbar()
    plt.show()

#test driver code
paths = [r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Photos\DSC_0870.JPG',
         r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Photos\DSC_0871.JPG',
         r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Panoramas\DSC_0871-DSC_0870jpg.jpg']

kpMatch, kpImgNames = matchKPs(paths)
createHeatmap(kpMatch[1][0][1], kpMatch[1][1][1], kpImgNames[-1])