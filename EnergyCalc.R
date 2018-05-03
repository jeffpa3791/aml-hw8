

image <- 1
iteration <- 2


epsilon <- 10 ^ -10

Q <- Parameters_list[[image]][[iteration]]


addNeighborToLL <- function(x,y,xN,yN) {
  if ((xN > 0) & (xN < 29) & (yN > 0) & (yN < 29)) {
    log_likelihood <<- log_likelihood + ( theta_HH * (2 * Q[x,y] - 1) * (2 * Q[xN,yN] - 1)) 
  }
}

entropy <- 0
log_likelihood <- 0

for (x in 1:28) {
  for (y in 1:28) {
    entropy <<- entropy + (Q[x,y] * log(Q[x,y] + epsilon)) + ( (1 - Q[x,y]) * log( (1 - Q[x,y]) + epsilon))
    addNeighborToLL(x,y,x+1,y)
    addNeighborToLL(x,y,x-1,y)
    addNeighborToLL(x,y,x,y+1)
    addNeighborToLL(x,y,x,y-1)
   #  log_likelihood <<- log_likelihood + ( theta_HX * (2 * Q[x,y] - 1) * train_images_noisy[[image]][x,y] )
      log_likelihood <<- log_likelihood + ( theta_HX * (2 * Q[x,y] - 1) * noisy_image[x,y] )
  }
}

print(entropy)
print(log_likelihood)

print(entropy - log_likelihood)