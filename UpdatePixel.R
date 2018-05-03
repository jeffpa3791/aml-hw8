# iterate through all pixels for an image and update the probability parameters

image <- 1
iteration <- 1

# obtain the starting parameters for the iteration from the list of lists structure
# actually this is done for the entire image, and then each probability is updated in turn, so the order matters
parameters <- Parameters_list[[image]][[iteration]]

noisy_image <- train_images_noisy[[image]]

pixel_count <- 0

process_neighbor_1 <- function(x,y) {
  if ((x > 0) & (x < 29) & (y > 0) & (y < 29)) {
    bef <- neighbor_sum
    val <- (theta_HH * ((2 * parameters[x,y]) - 1))
    neighbor_sum <<- neighbor_sum + (theta_HH * ((2 * parameters[x,y]) - 1))
    print(paste(x,y,bef,theta_HH,parameters[x,y],val,neighbor_sum))
  } else {
    # print("not a valid point")
  }
}


for (pixel in 1:ncol(UpdateOrder_x)) {
  pixel_x <- UpdateOrder_x[image,pixel]
  pixel_y <- UpdateOrder_y[image,pixel]
  
  neighbor_sum <- 0      # = sum of (theta * (2 * pi - 1) ) for each hidden neighbor of the pixel
  process_neighbor_1(pixel_x-1, pixel_y)
  process_neighbor_1(pixel_x+1, pixel_y)
  process_neighbor_1(pixel_x, pixel_y-1)
  process_neighbor_1(pixel_x, pixel_y+1)
  
  # term for the edge for the observed value 
  observed_value <- theta_HX * train_images_noisy[[image]][x,y] 

  #calculate the probaility as per the formula  
  numerator <- exp(neighbor_sum + observed_value)
  new_pi <- numerator / (numerator + 1/numerator)
  
 # new_q <- (new_pi) * log(new_pi) + (1-new_pi) * log(1-new_pi)
  
  print(paste(pixel, pixel_x, pixel_y, parameters[pixel_x,pixel_y] , 
              neighbor_sum, observed_value, numerator, 
              new_pi))
  
  # update the parameters so this value is used by its neighbors
  parameters[pixel_x,pixel_y] <- new_pi
  
  # set the pixel
  old_pixel <- noisy_image[pixel_x,pixel_y]
  if (new_pi > 0.5) {
    new_pixel <- 1
  } else {
    new_pixel <- -1
  }
  
  if (old_pixel != new_pixel) {
    noisy_image[pixel_x,pixel_y] <- new_pixel
    pixel_count <- pixel_count + 1
  }
  
}

Parameters_list[[image]][[iteration + 1]] <- parameters





# display first 20 noisy images
par(mfrow=c(1,1))    # put multiple plots in a 5x5 display
par(mar=c(0,0,0,0))  # set 4 margins
{image(t(noisy_image)[,28:1])} # must transpose to display using this method




