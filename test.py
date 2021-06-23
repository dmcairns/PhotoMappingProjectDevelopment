import stitch

inPaths = [r'C:\Users\Preston\Documents\RPrograms\PhotoMapProject\Data\Photos\unnamed.jpg',
         r'C:\Users\Preston\Documents\RPrograms\PhotoMapProject\Data\Photos\unnamed 2.jpg']

outPath = r'C:\Users\Preston\Documents\RPrograms\PhotoMapProject\Data\Panoramas\test.jpg'

stitcher = stitch.Stitcher()
status = stitcher.stitch(inPaths, outPath, True, 0.7)
print(status)