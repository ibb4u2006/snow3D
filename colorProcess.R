#load the jpeg into an RGB image object
snow3D = readJPEG("selected.jpg")

dim(snow3D)

#View RGB Channels of the image directly from R
library("grid")
library("gridExtra")

#Show the RGB Image
grid.raster(snow3D)

#To represent pixel intensity, show the blue channel in gray scale
grid.raster(snow3D[,,3])

#Show the 3 channels in separate images
snow3D.R = snow3D
snow3D.G = snow3D
snow3D.B = snow3D

#zero out the non-contributing channels for each image
snow3D.R[,,2:3] = 0
snow3D.G[,,1] = 0
snow3D.G[,,3] = 0
snow3D.B[,,1:2] = 0

#Build the image grid
redImage = rasterGrob(snow3D.R)
greenImage = rasterGrob(snow3D.G)
blueImage = rasterGrob(snow3D.B)
grid.arrange(redImage, greenImage, blueImage, nrow = 1)

#To segment this image we will need to reshape the array into a data frame with one row for each pixel and three columns for the RGB channels
#reshape image into data frame
df = data.frame(
  red = matrix(snow3D[,,1], ncol = 1),
  green = matrix(snow3D[,,2], ncol = 1),
  blue = matrix(snow3D[,,3], ncol = 1)
)

#We will apply k-means = 4 to break the image into 4 where k-means = K
#compute the k-means clustering
K = kmeans(df,4)
df$label = K$cluster

#replace the colour of each pixel in the image with the mean
#R, G, B values of the cluster in which the pixel resides.
#get the coloring
colours = data.frame(
  label = 1:nrow(K$centers),
  R = K$centers[,"red"],
  G = K$centers[,"green"],
  B = K$centers[,"blue"]
)

#merge colour codes on to data frame and we must maintain the original order of dataframe after the merge
df$order = 1:nrow(df)
df = merge(df, colours)
df = df[order(df$order),]
df$order = NULL

#Finally we have to reshape our data frame back into an image
#Get mean colour channel values for each row of the df
R = matrix(df$R, nrow = dim(snow3D)[1])
G = matrix(df$G, nrow = dim(snow3D)[1])
B = matrix(df$B, nrow = dim(snow3D)[1])

#reconstitute the segmented image in the same shape as the input image
snow3D.segmented = array(dim = dim(snow3D))
snow3D.segmented[,,1] = R
snow3D.segmented[,,2] = G
snow3D.segmented[,,3] = B

#View the result
grid.raster(snow3D.segmented)

#Colour Space plots in 2 and 3-D
library("rgl")

#colour space plot of snow3D
open3d()
plot3d(df$red, df$green, df$blue,
       col = rgb(df$red, df$green, df$blue),
       xlab = "R", ylab = "G", zlab = "B",
      size = 3, box = FALSE, axes = TRUE)
play3d( spin3d(axis = c(1,1,1), rpm = 3), duration = 10)

#colour space plot of segmented snow3D
open3d()
plot3d(df$red, df$green, df$blue,
       col = rgb(df$red, df$green, df$blue),
       xlab = "R", ylab = "G", zlab = "B",
       size = 3, box = FALSE)
play3d(spin3d(axis = c(1,1,1), rpm = 3), duration = 10)

#use 

movie3d ( spin3d(axis = c(1,1,1), rpm = 3), duration = 10)

#Instead of play3d to generate gifs (requires imagemagick)

#To generate the projections

require("ggplot2")

#perform PCA ont he snow3D data and add the UV coordinates to the dataframe
PCA = prcomp(df[,c("red", "green", "blue")],
             center = TRUE, scale. = TRUE)
df$u = PCA$x[,1]
df$v = PCA$x[,2]

#snow3D
ggplot(df, aes(x=u, y=v, col=rgb(red, green, blue))) + geom_point(size=2) + scale_colour_identity()

#segmented snow3D
ggplot(df, aes(x=u, y=v, col = rgb(R, G, B))) + geom_point(size=2) + scale_colour_identity()
