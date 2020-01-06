### IMPLEMENTATION OF HALFSPACEMASS 
# according to the paper of Chen et al. 

## args 
# data: consists of n observations of d-dimensional data points, everyone of which is an element of R^d 
# n_halfspace: Number of half-spaces sampled for estimation.  The larger n_halfspace is, the more accurate the estimation is
# subsample: size of subsample that should be used for calculating each halfspace
# scope:
# seed: integer >= 0 


############ TRAINING ##########################################################

train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed = 1337) {
  
  # INPUT CHECKS AND HOMOGENIZATION 
  
  ### for data 
  # inputhomogenization
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  #input checking
  checkmate::assert_data_frame(data)
  
  ### for n_halfspace
  checkmate::assert_integerish(n_halfspace, lower = 1, max.len = 1)
  
  ### for subsample
  checkmate::assert_integerish(subsample, upper = 1, lower = 0, max.len = 1)
  
  ### for scope 
  checkmate::assert_integerish(scope, lower = 0, max.len = 1)
  
  ### for seed
  checkmate::assert_integerish(seed, lower = 0, max.len = 1)
  
  # IMPLEMENTATION
  
  # create vars for output 
  halfspaces <- list()
  directions <- matrix(nrow = n_halfspace, ncol = ncol(data))
  splitpoints <- c()
  means <- matrix(nrow = n_halfspace, ncol = 2)
  
  
  for (i in 1:n_halfspace) {
    
    # Generate a random direction 
    random_direction <- random_direction(dimensions = ncol(data))
    
    # Generate a subsample
    sub_sample <- sub_sample(data,subsample)
    
    # Project datapoints onto direction, as matrix is neccessary as dot product and norm demands numeric args 
    # as norm the euclidean norm is used
    projection <- (as.matrix(sub_sample) %*% random_direction) / norm(as.matrix(sub_sample), type = "F")

    # select maximum, minimum and mid values 
    max <- max(projection)
    min <- min(projection)
    mid <- (max + min) / 2 
    
    # select splitpoint
    splitpoint <- select_splitpoint(max,min,mid,scope)
    
    # create mean left  
    mean_left <- create_mean_left(projection,splitpoint,sub_sample)
    
    # create mean_right
    mean_right <- create_mean_right(projection,splitpoint,sub_sample)
    
    # add output
    directions[i, ] <- random_direction
    splitpoints[i] <- splitpoint 
    means[i, ] <- c(mean_left,mean_right)
  }
  halfspaces[["data"]] <- data
  halfspaces[["directions"]] <- directions
  halfspaces[["splitpoints"]] <- splitpoints
  halfspaces[["means"]] <- means
  
  halfspaces
}

############# TESTING ##########################################################


evaluate_depth <- function(data, halfspaces) {
  
  #input checks 
  
  # HIER MUSS ICH DAS WAHRSCHEINLICH ALS DATA FRAME CASTEN WEIL MEINE CALCULATE SPACE MASS NUR FÜR DFS FUNKTIONIERT 

  # create vector to store halfspacesmasses
  halfspace_mass <- c()
  
  # calculate halfspacemass for every row of data. At first I wanted to do this with apply(data,1,calculate_half_space_masses, halfspaces),
  # however I got an error for unfitting inputs for calculate_halfspace_masses that I couldnt fix :(
  for (i in 1:nrow(data)) {
    data_point <- data[i,]
    halfspace_mass[i] <- calculate_halfspace_mass(data_point, halfspaces)
  }
  # add halfspacemass vector to the data 
  data$mass <- halfspace_mass
  data
}
  
############# DEFINING SUBFUNCTIONS ############################################

##################### function for sampling ####################################

sub_sample <- function(data, subsample) {
  sample_size = round(subsample * nrow(data))
  # Select a random subset of the original data
  sub_sample <- data[sample(nrow(data), sample_size), ]
}

##################### function for random direction ############################

# this function generates a random unit direction vector (I'm not sure if this could also be done more easily by just sample 
# a vector with length ncol from a uniform distribution, however I read [https://stackoverflow.com/questions/6283080/random-unit-vector-in-multi-dimensional-space] 
# that in highdimensional spaces the result  might not really be random, so I went with normalized gauss samples, which [hopefully] provide sufficient randomness..)

random_direction <- function(dimensions) {
  # d-dimensional gaussian random vector
  r_gauss_vector <- rnorm(dimensions)
  # normalized according to ~Pythagorean Formula ~
  normalization_const <- sqrt(sum(r_gauss_vector^2))
  # each element of the vector is normalized 
  random_direction <- r_gauss_vector / normalization_const
}

##################### function for selecting splitpoint ########################

select_splitpoint <- function(max,min,mid,scope) {
  # lower bound for intervall
  lower_bound <- mid - (scope/2 * (max - min))
  # upper bound for intervall
  upper_bound <- mid + (scope/2 * (max - min))
  # select random number in between 
  splitpoint <- runif(n = 1, min = lower_bound, max = upper_bound)
}

##################### function for creating mean left ##########################

create_mean_left <- function(projection,splitpoint,sub_sample) {
  # subset projection by s
  mean_left <- as.data.frame(projection)
  mean_left <- mean_left[mean_left[,1] < splitpoint,]
  # divide power of ml by power of subsample
  mean_left <- length(mean_left) / nrow(sub_sample)
}

##################### function for creating mean right #########################

create_mean_right <- function(projection,splitpoint,sub_sample) {
  # subset projection by s
  mean_right <- as.data.frame(projection)
  mean_right <- mean_right[mean_right[,1] >= splitpoint,]
  # divide power of ml by power of subsample
  mean_right <- length(mean_right) / nrow(sub_sample)
}

##################### function for calcualting halfspacemass for one point #####

calculate_halfspace_mass <- function(x, halfspaces){
  
    # projection of x on every direction 
    projections <- as.vector((as.matrix(x) %*% t(halfspaces$directions)))
    
    # create matrix with neccessary data 
    mass_matrix <- data.frame(projections = projections,  
                              splitpoints = halfspaces$splitpoints,
                              mean_left = halfspaces$means[,1],
                              mean_right = halfspaces$means[,2])
    
    # filter the matrix that was created above for every entry where prjection is smaller than the splitpoit
    half_space_mass_left <- sum(subset(mass_matrix, projections < splitpoints)$mean_left)
    
    # filter the matrix that was created above for every entry where prjection is bigger than the splitpoit
    half_space_mass_right <- sum(subset(mass_matrix, projections >= splitpoints)$mean_right)
    
    # calculate total halfspace mass
    half_space_mass <- half_space_mass_left + half_space_mass_right 
    
    half_space_mass
}
  

