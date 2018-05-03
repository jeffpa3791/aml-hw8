
homedir <- "/Users/Jeff/UIUC/Applied Machine Learning/hw8"
setwd(homedir)

datadir <- paste(homedir, "SupplementaryAndSampleData", sep="/")

# define the images we are going to look at
start_image <- 1
end_image <- 20
num_images <- end_image - start_image + 1

train_file_name <- paste(homedir,"train-images.idx3-ubyte",sep='/')
train_file <- file(train_file_name,"rb")

magic <- readBin(train_file, integer(), n=1, endian="big")
num_images_in_file <- readBin(train_file, integer(), n=1, endian="big")
size1 <- readBin(train_file, integer(), n=1, endian="big")
size2 <- readBin(train_file, integer(), n=1, endian="big")

# display first 100
#par(mfrow=c(10,10))    # put multiple plots in a 5x5 display
#par(mar=c(0,0,0,0))  # set 4 margins
#for(i in 1:100) { m = matrix(readBin(train_file,integer(), size=1, n=28*28, endian="big",signed=FALSE),28,28);image(m[,28:1])}

train_images <- list()
train_images_bin <- list()
train_bounded_images <- list()
train_stretched_images <- list()
for(i in 1:end_image) { 
  file_image <- matrix(readBin(train_file,integer(), size=1, n=28*28, endian="big",signed=FALSE),28,28)
  
  # image imported by this method must be transposed to match form used in assignment
  train_images[[i]] <- t(file_image)
  
  train_images_bin[[i]] <- train_images[[i]]
  for (x in 1:28) {
    for (y in 1:28) {
      if (train_images_bin[[i]][x,y] < 128) {
        train_images_bin[[i]][x,y] <- -1
      } else {
        train_images_bin[[i]][x,y] <- 1
      }
    }
  }
}
close(train_file)

# copy original images to noisy
train_images_noisy <- train_images_bin

# read noise data
noise_df <- read.csv(file=paste(datadir,"NoiseCoordinates.csv",sep="/"))

# apply noise data:
#   indexing on my list starts with 1, while example starts with 0
#   my images have (1,1) in the top left column -- need to add 1 from each coordinate of file
#   so first coordinate in sample for image 0 (21,17) is translated to (22,18) in image 1 in my program
for (image in start_image:end_image) {
  for (c in 2:ncol(noise_df)) {
    x_coord <- noise_df[2*image-1,c] + 1
    y_coord <- noise_df[2*image,c] + 1
    # print(paste(image,x_coord,y_coord,sep=","))
    # reverse the pixel at the indicated position
    train_images_noisy[[image]][x_coord,y_coord] <- -1 * train_images_noisy[[image]][x_coord,y_coord]
  }
}

# display first 20 noisy images
par(mfrow=c(4,5))    # put multiple plots in a 5x5 display
par(mar=c(0,0,0,0))  # set 4 margins
for(i in start_image:end_image) {image(t(train_images_noisy[[i]])[,28:1])} # must transpose to display using this method


# read in the Update Order Coordinates and translate to be consistent with my format
raw_UpdateOrder_df <- read.csv(file=paste(datadir,"UpdateOrderCoordinates.csv",sep="/"))
UpdateOrder_x <- matrix(0,20,ncol(raw_UpdateOrder_df)-1)
UpdateOrder_y <- matrix(0,20,ncol(raw_UpdateOrder_df)-1)
for (image in 1:20) {
  for (c in 2:ncol(raw_UpdateOrder_df)) {
    UpdateOrder_x[image,c-1] <- raw_UpdateOrder_df[2*image-1,c] + 1
    UpdateOrder_y[image,c-1] <- raw_UpdateOrder_df[2*image,c] + 1
  }
}

# read the InitialParameterModel - set of probabilities that each pixel = 1
raw_InitialParameters_df <- read.csv(file=paste(datadir,"InitialParametersModel.csv",sep="/"),header=FALSE)

# create a list of lists to store the parameters at each iteration for each image
Parameters_list <- list()
for (image in start_image:end_image) {
  iteration_parameters <- list()
  iteration_parameters[[1]] <- raw_InitialParameters_df
  Parameters_list[[image]] <- iteration_parameters
}

# Define the Theta's
theta_HH <- 0.8   # between the Hidden values in the grid
theta_HX <- 2.0   # between each Observed value and its corresponding Hidden value 

