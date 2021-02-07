#CLASS READ IN RASTER DATA IN R

#Change to the folder where the documents are contained
library(raster)
band1=raster("band1.tif")
band1

plot(band1)

band2=raster("band2.img")
band2

b3=raster("CA2015_new.rst")
writeRaster(b3, "ConvertToTif.tif", overwrite=TRUE)

###THIS FOLLOWING TECHQNIUE ONLY WORKS IF ALL THE RASTER BANDS HAVE THE EXACT SAME SPATIAL EXTENT, PIXEL RESOLUTION AND FILE EXTENSION
##Change to the folder bands
#READ ALL THE FILES THAT HAVE TIF FORMAT ON THE FOLDER AND STORE THEM ON A RLIST
rlist=list.files(pattern="tif$", full.names=TRUE)

#Once all files are stored, we can create a raster stack
rasters=stack(rlist)
rasters

#Examine your raster stack
names(rasters)
rasters
plot(rasters)


### LEcture 6: Examine and Modify CRS of a raster band
band1

bill1<-projectRaster(band1, crs='+proj=longlat')
bill1
plot(bill1)


#Change from Lat Long to UTM
x="+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
b1u=projectRaster(bill1, crs=x)
b1u
plot(b1u)

#Obs.: it is not possible to change the coordinates of entire raster stacks. We can only modify individual raster bands in R


####Modify Raster Stack in R
band1=raster("band1.tif")
band1

band3=raster("band3.tif")
band3

band4=raster("band4.tif")
band4

#Stacks different bands together
stackR=stack(band1, band3, band4) #this is useful when reading raster files one by one
names(stackR)
plot(stackR)

#Puts them together using brick function
br=brick(band1, band3, band4)
br

#Plot individual bands
plot(br$band1)
plot(stackR$band1)

#Going back to another function that has been previously run above
rasters=stack(rlist)
rasters

#How to drop a layer from the stack
rastersD1=dropLayer(rasters, 6)
names(rastersD1)
  
rastersD2=dropLayer(rasters, c(1,6)) #specify index 1 and 6
rastersD2
names(rastersD2)

#How to add a new band to the Raster Stack ***Obs: all the bands that are going to be stacked need
#to have the same extension, extent, pixel size, etc
rastAdd=addLayer(rastersD2, band1) #add b1 to the stack
names(rastAdd)

####Plot multiple bands as false color composites
rasters #please read them in all 6 bands

plotRGB(rasters, r=3, g=2, b=1, stretch="hist" )
plotRGB(rasters, r=3, g=2, b=1, scale=800, stretch="lin" )
plotRGB(rasters, r=4, g=3, b=2, stretch="hist" ) #this is how standard remote software tools are desgined for the colours bands to bs used


###Band arithmetic in R
rasters #make sure to include all original 6 bands on the object to be used from now on

s=calc(rasters,sum) #will sum up all raster bands

func<-function(x) ##apply  custom function to your stack
{ 
  x[x<300]<-NA
  return(x)
}
rc2<-calc(s, func)
s

#(NIR-Red) / (NIR+Red)
ndvi=(rasters$band4-rasters$band3)/(rasters$band3-rasters$band4) #Formula to calculate NDVI, which is: near infreared - red band divided by red band - near infrared
plot(ndvi) #0 to 1

#####Reclassify: build a categorical map
#Convert a continuos rasters to a categorical one
m<-c(-0.2,0.2,1,  0.2,0.6,2,  0.6,1,3)
mat <-matrix(m, ncol=3, byrow=TRUE) #matrix of categorical value clases
mat

#Reclassify the matrix
rc<-reclassify(ndvi, mat)
rc
plot(rc)

#####Categorical Rasters Statistics in R

install.packages("SDMTools")
library(SDMTools)

install.packages("remotes")
remotes::install_version("SDMTools", "1.1-221")

#our reclassified raster is in UTM, for SDMTools, we need it in lat long
#Converts a raster file to lat long
rc2<-projectRaster(rc, crs='+proj=longlat')

c=ClassStat(rc, cellsize= 30, latlon=FALSE)
head(c)


#### Resampling raster data in R => Resampling means to change the raster pixel size according to another baseline raster
#the basic format for resampling a raster in R is resample(raster_to_be_resampled, baselineraster, "interpolation technique")
#the raster whose pixel size has to be modified has to be modified is the first argument
#A number of interpolation techniques exists nearest-neighbors, bilinear 
library(raster)
h=raster("tamd_hill.tif")
n=raster("tamd_ndvi.tif")

plot(h)
plot(n)

#Change the coordinate system to latitude-longitude
hLL<-projectRaster(n, crs='+proj=longlat')

#Examine the rasters to check that pixel resolution lies on top of each other
hLL
h

#Resampling the pixel resolution
n2=resample(hLL, h, "ngb")

#Check the new pixel values
n2

#Obs.: if the pixel values are quite far apart, then it is better to 
#resample from a bigger pixel to a smaller pixel


####Clipping rasters in R
library(raster)
hill=raster("1hillshade.tif")
plot(hill)

#Sets what are the range extensions to be cropped
e<-extent(105.6, 105.7, 20.5, 21.5) #longitude, latitude

#Crops the raster to certain dimensions and locations of the map that have been previously specified
abs=crop(hill, e)
plot(abs)

#Enables to manually crop the extent that you need in the map
e2=drawExtent()
abs2=crop(hill, e2)

#Clipping a raster to the extent of a defined polygon
library(rgdal)
tdao=readOGR(".", "VietnamPA_name__Tam Dao")
plot(tdao, add=T)

#Clip out the raster corresponding to tdao from hillshade. Obs: this is much quicker to do in QGIS
A=mask(hill,tdao)
plot(A)

#Extracts data from the raster into the overlaying polygons
pal=readOGR(".", "VietnamPA")
plot(pal, add=T)
dat<-extract(hill, pal, fun=mean, na.rm=TRUE)
head(dat)


### Topographic calculations in R
#1)Read in raster of digital elevation model (DEM) ready stored on 
#hard drive
dem=raster("vie_dem_srtm.tif") #elevation in m
plot(dem)

#2)Download elevation data from internet
#country codes can be found on this website: http://max2.ese.u-psud.fr/epc/conservation/Girondot/Publications/Blog_r/Entrees/2014/11/2_Other_geographic_databases.html 
dem2=getData("alt", country= "VNM") #you can calculate the slope and etc from here  
plot(dem2)

#PLot areas of a certain values of elevation
plot(dem, zlim= c(1000,3000), main="Elevation 1000m-3000m") #areas where elevation is between 1000 m and 3000 m

slope=terrain(dem, opt="slope", unit="degrees")#calculate slope in degrees
aspect=terrain(dem, opt="aspect")

x<-terrain(dem, opt=c("slope", "aspect"), unit="degrees") #plot slope and aspect in degrees
plot(x)

hill<-hillShade(slope, aspect, 40, 270)#hillshade (40 and 270 stand for the angle and direction of the sun. These are default values that can be used for other kind of data if we do not know the values for it)
plot(hill)