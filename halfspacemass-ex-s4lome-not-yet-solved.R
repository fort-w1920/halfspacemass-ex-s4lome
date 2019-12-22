### IMPLEMENTATION OF HALFSPACEMASS 
# according to the paper of Chen et al. 

## args 
# data: consists of n observations of d-dimensional data points, everyone of which is an element of R^d 
# n_halfspace: Number of half-spaces sampled for estimation.  The larger n_halfspace is, the more accurate the estimation is
# subsample: size of subsample that should be used for calculating each halfspace
# scope:
# seed: integer >= 0 



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
  checkmate::assert_integerish(subsample, lower = 1, max.len = 1)
  
  ### for scope 
  checkmate::assert_integerish(scope, lower = 1, max.len = 1)
  
  ### for seed
  checkmate::assert_integerish(seed, lower = 0, max.len = 1)
  
  
  # DEFENSIVE STUFF
  
  
  # IMPLEMENTATION
  
  for (i in 1:n_halfspace) {
    
    # Generate a random direction 
    random_direction <- random_direction(dimensions = ncol(data))
    
    # Generate a subsample FUNKTIONIERT NICHT RICHTIG 
    sub_sample <- sub_sample(data,subsample)
    
    # Project Di onto i , denoted by Di
    # dot product von jedem daten vektor und unit vektor 
    
    # maxi ← max(Di i ), mini ← min(Dii ), midi ← maxi +mini 2 5 
    
    # Randomly select si in (midi − λ2 (maxi − mini), midi + λ2 (maxi − mini 6 ))
    
    # step 7 
    
    # step 8

  }
}


############# DEFINING SUBFUNCTIONS ############################################

##################### function for sampling ####################################

sub_sample <- function(data, subsample) {
  # Select a random subset of the original data
  subsample <- data[sample(nrow(data), subsample), ]
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