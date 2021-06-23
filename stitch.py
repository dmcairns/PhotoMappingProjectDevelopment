import numpy as np
import cv2 as cv

def stitchImagesBasic(filepaths, outputpath):
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

class customStitcher:
    def __init__(self):
        pass

    def stitchImages(self, inPaths, outPath, createVisualization = False, ratio = 0.7, reprojThresh = 4.0):
        imgs = []
        kps = []
        dess = []
        for filepath in inPaths:
            img = cv.imread(filepath)
            imgs.append(img)
            self.detectDescribe(img)
            kp, des = self.detectDescribe(img)
            kps.append(kp)
            dess.append(des)

        match = self.matchKeypoints(kps[0], kps[1], dess[0], dess[1], ratio, reprojThresh)

        if match is None:
            return -1

        (matches, homography, status) = match

        result = cv.warpPerspective(imgs[0], homography, (imgs[0].shape[1] + imgs[1].shape[1], imgs[0].shape[0]))
        result[0:imgs[1].shape[0], 0:imgs[1].shape[1]] = imgs[1]

        cv.imwrite(outPath, result)

        if createVisualization:
            vis = self.drawMatches(imgs[0], imgs[1], kps[0], kps[1], matches, status)
            cv.imwrite(outPath[:-4] + "vis.jpg", vis)
        return 1

    def detectDescribe(self, img):
        #akaze = cv.AKAZE_create()
        #gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
        #kps = np.float32([kp.pt for kp in kps])

        mser = cv.MSER_create()
        brisk = cv.BRISK_create()
        gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
        kps, dess = brisk.compute(gray, mser.detect(gray, None))

        kps = np.float32([kp.pt for kp in kps])

        return kps, dess

    def matchKeypoints(self, kpsA, kpsB, desA, desB, ratio, reprojThresh):
        bf = cv.BFMatcher()

        rMatches = bf.knnMatch(desA, desB, k = 2)

        matches = []
        for match in rMatches:
            if len(match) == 2 and match[0].distance < match[1].distance * ratio:
                matches.append([match[0].trainIdx, match[0].queryIdx])

        if len(matches) > 4:
            ptsA = np.float32([kpsA[i] for (_, i) in matches])
            ptsB = np.float32([kpsB[i] for (i, _) in matches])

            (homography, status) = cv.findHomography(ptsA, ptsB, cv.RANSAC, reprojThresh)

            return (matches, homography, status)
        return None

    def drawMatches(self, imageA, imageB, kpsA, kpsB, matches, status):
        # initialize the output visualization image
        (hA, wA) = imageA.shape[:2]
        (hB, wB) = imageB.shape[:2]
        vis = np.zeros((max(hA, hB), wA + wB, 3), dtype="uint8")
        vis[0:hA, 0:wA] = imageA
        vis[0:hB, wA:] = imageB
        # loop over the matches
        for ((trainIdx, queryIdx), s) in zip(matches, status):
            # only process the match if the keypoint was successfully
            # matched
            if s == 1:
                # draw the match
                ptA = (int(kpsA[queryIdx][0]), int(kpsA[queryIdx][1]))
                ptB = (int(kpsB[trainIdx][0]) + wA, int(kpsB[trainIdx][1]))
                cv.line(vis, ptA, ptB, (0, 255, 0), 1)
        # return the visualization
        return vis