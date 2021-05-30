import stitch

inPaths = [r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Photos\unnamed.jpg',
         r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Photos\unnamed 2.jpg']

outPath = r'C:\Users\Preston\Documents\R\RProjects\PhotoMappingProjectDevelopment\Data\Panoramas\test.jpg'

status = stitch.stitchImages(inPaths, outPath)
print(status)