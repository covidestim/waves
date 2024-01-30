conv_edge <- function(m, k, sample_size, return.df = TRUE){
  
  ## Subsampling
  if(!missing(sample_size)){
    ## Filtering the m to the sample size
    m_sample <- m |> 
      dplyr::filter(i %in% 1:sample_size, 
                    j %in% 1:sample_size)
    
    m <- as.matrix(Matrix::sparseMatrix(i = m_sample$i,
                                        j = m_sample$j,
                                        x = m_sample$alpha))
  }
  
  ## Convolution
  conv <- OpenImageR::convolution(m, k, mode = "same")
  
  ## Normalize
  # conv <- NormalizeObject(conv)
  
  if(return.df){
    ## Convolution data.frame
    conv_df <- data.frame(i = c(t(row(conv))),
                          j = c(t(col(conv))),
                          alpha = c(t(conv)))
    ## Returning
    return(conv_df) 
  }else{
    return(conv)
  }
}

magnitude <- function(Ix2, Iy2){
  G <- sqrt(Ix2 + Iy2)
  return(G/max(G)*255)
}

# Function for non-maximum suppression
non_maximum_suppression <- function(magnitude, direction) {
  ## The function will suppress pixel value in positive and negative directions of the gradient if,
  ## the next pixel in that direction has a higher value
  
  suppressed_magnitude <- magnitude
  for (i in 2:(nrow(magnitude)-1)) {
    for (j in 2:(ncol(magnitude)-1)) {
      angle = direction[i, j]
      if ((angle >= -22.5 && angle < 22.5) || (angle >= 157.5 || angle < -157.5)) {
        if (!(magnitude[i, j] > magnitude[i, j+1] && magnitude[i, j] > magnitude[i, j-1])) {
          suppressed_magnitude[i, j] <- 0
        }
      } else if ((angle >= 22.5 && angle < 67.5) || (angle >= -157.5 && angle < -112.5)) {
        if (!(magnitude[i, j] > magnitude[i-1, j+1] && magnitude[i, j] > magnitude[i+1, j-1])) {
          suppressed_magnitude[i, j] <- 0
        }
      } else if ((angle >= 67.5 && angle < 112.5) || (angle >= -112.5 && angle < -67.5)) {
        if (!(magnitude[i, j] > magnitude[i-1, j] && magnitude[i, j] > magnitude[i+1, j])) {
          suppressed_magnitude[i, j] <- 0
        }
      } else if ((angle >= 112.5 && angle < 157.5) || (angle >= -67.5 && angle < -22.5)) {
        if (!(magnitude[i, j] > magnitude[i-1, j-1] && magnitude[i, j] > magnitude[i+1, j+1])) {
          suppressed_magnitude[i, j] <- 0
        }
      }
    }
  }
  return(suppressed_magnitude)
}

# Function for edge tracking by hysteresis
edge_tracking_by_hysteresis <- function(magnitude, low_threshold, high_threshold) {
  edges <- magnitude >= high_threshold
  candidates <- magnitude >= low_threshold
  
  while (any(candidates)) {
    idx <- which(candidates, arr.ind=TRUE, useNames=FALSE)
    i <- idx[1, 1]
    j <- idx[1, 2]
    candidates[i, j] <- FALSE
    
    # Define neighborhood indices
    neighbors_i <- max(1, i-1):min(nrow(magnitude), i+1)
    neighbors_j <- max(1, j-1):min(ncol(magnitude), j+1)
    
    # Check neighbors within the matrix bounds
    neighbors_i <- neighbors_i[neighbors_i <= nrow(magnitude)]
    neighbors_j <- neighbors_j[neighbors_j <= ncol(magnitude)]
    
    # Extract neighborhood values
    neighbors <- magnitude[neighbors_i, neighbors_j]
    
    # Update edges based on neighbors and set candidates to FALSE
    if (any(neighbors >= low_threshold)) {
      edges[i, j] <- TRUE
    }
  }
  
  return(edges)
}


canny_edge_detector <- function(m, low_threshold, high_threshold){
  require(OpenImageR)
  
  if(!is.matrix(m)){
    m <- m <- as.matrix(Matrix::sparseMatrix(i = m$i,
                                             j = m$j,
                                             x = m$alpha))
  }
  
  #Step.1 Denoiesing with Gaussian kernel
  gaussian_kernel <- matrix(nrow = 3,
                            c(1,2,1,2,4,2,1,2,1)/16)
  
  
  gauss <- convolution(m, kernel = gaussian_kernel, mode = "same")
  
  ## Step.2 Magnitude and Direction over the gaussian denoised matrix
  sobel_x <- matrix(nrow = 3,
                    c(-1,-2,-1,0,0,0,1,2,1))
  sobel_y <- t(sobel_x)
  
  Ix <- convolution(gauss, kernel = sobel_x, mode = "same")
  Ix2 <- Ix*Ix
  Iy <- convolution(gauss, kernel = sobel_y, mode = "same")
  Iy2 <- Iy*Iy
  
  magnitude <- function(Ix2, Iy2){
    G <- sqrt(Ix2 + Iy2)
    return(G/max(G)*255)
  }
  
  ## Magnitude = √Ix2 + Iy2
  mag <- magnitude(Ix2, Iy2)
  
  ## Direction = arctan(Iy/Ix)
  dir <- atan2(Iy, Ix)
  
  ## Step.3 Non-maxima suppression
  # Function for non-maximum suppression
  non_maximum_suppression <- function(magnitude, direction) {
    ## The function will suppress pixel value in positive and negative directions of the gradient if,
    ## the next pixel in that direction has a higher value
    
    suppressed_magnitude <- magnitude
    for (i in 2:(nrow(magnitude)-1)) {
      for (j in 2:(ncol(magnitude)-1)) {
        angle = direction[i, j]
        if ((angle >= -22.5 && angle < 22.5) || (angle >= 157.5 && angle < -157.5)) {
          if (!(magnitude[i, j] > magnitude[i, j+1] && magnitude[i, j] > magnitude[i, j-1])) {
            suppressed_magnitude[i, j] <- 0
          }
        } else if ((angle >= 22.5 && angle < 67.5) || (angle >= -157.5 && angle < -112.5)) {
          if (!(magnitude[i, j] > magnitude[i-1, j+1] && magnitude[i, j] > magnitude[i+1, j-1])) {
            suppressed_magnitude[i, j] <- 0
          }
        } else if ((angle >= 67.5 && angle < 112.5) || (angle >= -112.5 && angle < -67.5)) {
          if (!(magnitude[i, j] > magnitude[i-1, j] && magnitude[i, j] > magnitude[i+1, j])) {
            suppressed_magnitude[i, j] <- 0
          }
        } else if ((angle >= 112.5 && angle < 157.5) || (angle >= -67.5 && angle < -22.5)) {
          if (!(magnitude[i, j] > magnitude[i-1, j-1] && magnitude[i, j] > magnitude[i+1, j+1])) {
            suppressed_magnitude[i, j] <- 0
          }
        }
      }
    }
    return(suppressed_magnitude)
  }
  
  non_maxima_sup <- non_maximum_suppression(magnitude = mag, 
                                            direction = dir)
  
  ## Step.4 Double Thresholding
  non_maxima_sup[non_maxima_sup < low_threshold] <- 0
  non_maxima_sup[non_maxima_sup > high_threshold] <- 1
  
  ## Step.5 Edge Tracking by Hyteresis
  edge_tracking_by_hysteresis <- function(magnitude, low_threshold, high_threshold) {
    edges <- magnitude >= high_threshold
    candidates <- magnitude >= low_threshold
    
    while (any(candidates)) {
      idx <- which(candidates, arr.ind=TRUE, useNames=FALSE)
      i <- idx[1, 1]
      j <- idx[1, 2]
      candidates[i, j] <- FALSE
      
      # Define neighborhood indices
      neighbors_i <- max(1, i-1):min(nrow(magnitude), i+1)
      neighbors_j <- max(1, j-1):min(ncol(magnitude), j+1)
      
      # Check neighbors within the matrix bounds
      neighbors_i <- neighbors_i[neighbors_i <= nrow(magnitude)]
      neighbors_j <- neighbors_j[neighbors_j <= ncol(magnitude)]
      
      # Extract neighborhood values
      neighbors <- magnitude[neighbors_i, neighbors_j]
      
      # Update edges based on neighbors and set candidates to FALSE
      if (any(neighbors >= low_threshold)) {
        edges[i, j] <- TRUE
      }
    }
    
    return(edges)
  }
  
  edged <- edge_tracking_by_hysteresis(non_maxima_sup, 
                                       low_threshold = low_threshold, 
                                       high_threshold = high_threshold)
  
  return(edged)
  
}


canny_edge_track <- canny_edge_detector(m = samples_alphas, 
                                        low_threshold = 0.3, 
                                        high_threshold = 0.7)
