
#Flujo de trabajo para el análisis de imágenes:

# Antes de empezar a trabajar con Crustcover IMPORTANTE Ordenar carpetas y directorios.

# Para trabajar con CrustCover necesitaremos imágenes e formato tif correspondientes a fotografías del espectro visible (vis) 
# y del infrarojo cercano (nir) 

# Estas imágenes han sufrido un proceso de transformación antes de llegar a las condiciones de trabajo de crustCover
# 1º han sido tansformadas desde formato rawde las cámaras de mapir (transfotrmación hecha con Darktable) en linux
# 2º posteriormente sometidas a un proceso de alineamiento y corrección de histogramas con ImageJ

# Las imágenes obtenidas quedan guardadas en sendas carpetas "vis" (imágenes en visible) y "nir" (imágenes en infrarojo cercano) que estarán
# localizadas en el directorio de trabajo de crustCover. 

# Por último antes de comenzar a trabajar con crustCover debemos renombrar las imágenes de tal manera que queden así:

# observation 1 VIS: IMG_100.jpeg
# observation 1 NIR: IMG_101.jpeg
# observation 2 VIS: IMG_102.jpeg
# observation 2 NIR: IMG_103.jpeg

# The crustCover functions will automatically name the output files obs_1, obs_2, obs_3, etc., 
# but the user may optionally specify a list of names to label the function output files
# by adding a .csv titled names.csv to the working directory. 
# There should be one name per observation in the following format:
# names:
# alternate id 1
# alternate id 2
# alternate id 3

# Para empezar:

#renombramos los archivos tiff para poder trabajar con crustCover

# Rename nir files
nir_fileNames <- Sys.glob("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example/nir/*tif")
nir_newnames <- paste0("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example/nir/IMG_",
                       seq(101, by=2, len= length(seq(nir_fileNames))), ".tif")

file.rename(nir_fileNames, nir_newnames)


# Rename vis files
vis_fileNames <- Sys.glob("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example/vis/*tif")
vis_newnames <- paste0("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example/vis/IMG_",
                       seq(100, by=2, len= length(seq(nir_fileNames))), ".tif")

file.rename(vis_fileNames, vis_newnames)


#Necesitaremos la función "check.packages" que tendremos guardada en nuestro directorio de funciones 
#(se trata de una función con base en el repositorio de github https://gist.github.com/smithdanielle/9913897) 
#aunque existen maneras de instalarla desde github con las funciones install-github (paquete "devtools") o githubinstal(paquete "githubinstall")
# preferimos radicar la función en nuestro directorio local pues es una función corta y sencilla de copiar
# Fuente de la función  "check.packages": 
# Check to see if packages are installed. 
# Install them if they are not, then load them into the R session
source("/home/manu/Desktop/Ndvi_prueba/R_ndvi/Herramientas_R/check.packages.R")

packages <- c("tiff")
check.packages(packages)

#Para empezar:

#renombramos los archivos tiff para poder trabajar con crustCover


#Establecemos el directorio de trabajo de crustCover
setwd("~/Desktop/Ndvi_prueba/R_ndvi/crustcover.example")

# Establecemos la imagen de referencia para hacer losw siguietes cálculos
RGB_stack_DEM <-stack("vis/IMG_100.tif")
bandred <- raster("vis/IMG_100.tif", band=1)



#Ahora vamos a proceder a la importacion de las areas de interés o ventanas de observación de las imágenes. 

#crustCover nos permite dibujar el área de interés pero nosotros queremos importar áreas de interés de tamaño conocido
#estas áreas de interés corresponden a celdas de 5 x 5 cm que coinciden con los alveolos en la fotografía.
# Estas celdas las hemos creado manualmente con el  programa Image J y están guardadas originalmente en formato .roi
# para poder trabajar con estos archivos en crustCover, necesitamos transformar desde el formato .roi (objeto de clase ijroi)
# a objetos SpatialPolygons

# Para importar los archivos punto .roi instalamos el paquete "RImageJROI" 
packages <- c("RImageJROI")

check.packages(packages)

# Importamos el área de interes que creamos con Image J
# usamos como EJEMPLO la número 5 (la numeración de las celdillas las sabemos gracias  a la referencia 
# y orden conocido de los alveolos en la fotografía, algo que fuimos 
# apuntando según creabamos las celdillas en el Image J)
x_roi5<- read.ijroi("~/Desktop/Ndvi_prueba/R_ndvi/obs_area/RoiSet2/2146-1492.roi", verbose = FALSE)

# una vez importada el área de interés a nuestro Global Environment vamos a necesitar hacer una serie de cambios de formato
# hasta llegar a tener objetos SpatialPolygons legibles por crusCover 

# para ello, instalamos el paquete "spatstat"
packages <- c("spatstat")
check.packages(packages)

# primero lo transformamos a formato owin (ijroi ->  owin)
x_owin5<- ij2spatstat(x_roi5)

# Ahora hacemos dos operaciones que son necesarias para que las ventanas coincidan sobre la imagen que proyecta crustCover:
# 1) es necesario invertir las coordenadas del eje Y de la ventana, pues imageJ proyecta el eje Y
# invertido (y=0 -> borde superior de la imagen) con respecto a cómo lo proyecta crustCover (y=0 -> borde inferior de la imagen) 
# 2) es necesario reescalar las coordenadas de la ventana tal manera que correspondan al rango de X e Y que oscila entre 0 y 1 en la
# imagen proyectada por crustCover

# En el vector de coordenadas y de la ventana hcemos la operación 1 y 2
w5_y_corr <- (nrow(as.matrix(bandred))-(as.data.frame(x_owin5))$y)/nrow(RGB_stack_DEM)

# En el vector de coordenadas x de la ventana hacemos solo la operación 2
w5_x<-(as.data.frame(x_owin5))$x/ncol(RGB_stack_DEM)


#Unimos los vectores
xym5<-  cbind(x = w5_x, y = w5_y_corr)
#creamos el polígono
p5 <-  Polygon(xym5)
# Creamos la lista de poígonos con un solo poligono, en este ejemplo el pocillo 5
ps5 <- Polygons(list(p5), "pocillo 5")
# creamos el objeto SpatialPolygons
sps <- SpatialPolygons(list(ps5))

# Instalamos el paquete "crustCover" dede github utilizando e paquete "githubinstall"
packages <- c("githubinstall")
check.packages(packages)
install.packages("githubinstall")

githubinstall("crustCover")
#Do you want to install the package (Y/n)?  
# Y
library(crustCover)

#fijamos la medida de la carta de color para hacer la calibración
# chart <- chart()
source("/home/manu/Desktop/Ndvi_prueba/R_ndvi/Herramientas_R/crustcover.from.tif/chart.from.tif.r")

chart.from.tif()

source("/home/manu/Desktop/Ndvi_prueba/R_ndvi/Herramientas_R/function.20180412.my.draw.observation.area.from.tif.r")
# revisar la llamada a la función
obs.area.5 <- my.draw.obs.from.tif(sps)


head(obs.area.5)






# nosotros aun no sabemos el umbral para la guata (probablemente no sea 5, como sugiere el mosscover), nosotros probamos con 0.3
ccSpectral(chart = chart, obs.area = obs.area.5, rasters = T, ml = T, thresholds = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3))

#Load the processed spectral data, divide by 1000 to restore to original scale
spec.stack <- stack("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example/output 2018-03-14 12:30:10/obs_1_spectral_stack.tif")/1000

#Load the binary cutoff data
cover.stack <- stack("/home/manu/Desktop/Ndvi_prueba/R_ndvi/crustcover.example/output 2018-03-14 12:28:18/obs_1_spectral_stack.tif")

#Select the NDVI layer from the spectral stack
obs1.ndvi <- spec.stack[[1]]

export(ndvi)
# to be able to open in imageJ, TRY IT! :)