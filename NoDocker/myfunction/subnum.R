subnum <- function(x, digits = 2, eql = TRUE) {
  
  # Special thanks to BingChat
  
  # Check if 'digits' argument is valid
  if (!digits %in% c(1,2,3)) {
    stop("Invalid value for 'digits' argument. Must be 1, 2, or 3.")
  }
  

# define main process -----------------------------------------------------

  mainprocess <- function(inside_x, inside_digits = digits, inside_eql = eql) {
    
  # When argument 'x' is numeric --------------------------------------------
    
    if (is.numeric(inside_x)) {
      
      # Check if 'x' is greater than or equal to 1
      if (inside_x > 1) {
        stop("Invalid value for 'x' argument. Must be smaller than 1.")
      }
      
      
    # When 'x' is positive ----------------------------------------------------
      
      # Check if 'x' is less than 0.001
      if (0 <= inside_x & inside_x < 0.001) {
        if (inside_eql == TRUE) {
          return(paste("< .001"))
        } else {
          return(paste(".001"))
        }
      }
      
      # Check if 'x' is between 0.001 and 0.01
      else if (0.001 <= inside_x & inside_x < 0.01) {
        if (inside_digits == 3) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^[0-9]*\\.", ".", sprintf('%.3f',inside_x))))
          } else {
            return(paste(sub("^[0-9]*\\.", ".", sprintf('%.3f',inside_x))))
          }
        }
        
        if (inside_digits == 2) {
          if (inside_eql == TRUE) {
            return(paste("< .01"))
          } else {
            return(paste(".01"))
          }
        }
        
        if (inside_digits == 1) {
          if (inside_eql == TRUE) {
            return(paste("< .1"))
          } else {
            return(paste(".1"))
          }
        }
        
      }
      
      # Check if 'x' is between 0.01 and 0.1
      else if (0.01 <= inside_x & inside_x < 0.1) {
        if (inside_digits == 3) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^[0-9]*\\.", ".", sprintf('%.3f',inside_x))))
          } else {
            return(paste(sub("^[0-9]*\\.", ".", sprintf('%.3f',inside_x))))
          }
        }
        
        if (inside_digits == 2) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^[0-9]*\\.", ".", sprintf('%.2f',inside_x))))
          } else {
            return(paste(sub("^[0-9]*\\.", ".", sprintf('%.2f',inside_x))))
          }
        }
        
        if (inside_digits == 1) {
          if (inside_eql == TRUE) {
            return(paste("< .1"))
          } else {
            return(paste(".1"))
          }
        }
        
      }
      
      # Check if 'x' is greater than or equal to 0.1
      else if (0.1 <= inside_x & inside_x < 1) {
        if (inside_digits == 3) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^[0-9]*\\.", ".", sprintf('%.3f',inside_x))))
          } else {
            return(paste(sub("^[0-9]*\\.", ".", sprintf('%.3f',inside_x))))
          }
        }
        
        if (inside_digits == 2) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^[0-9]*\\.", ".", sprintf('%.2f',inside_x))))
          } else {
            return(paste(sub("^[0-9]*\\.", ".", sprintf('%.2f',inside_x))))
          }
        }
        
        if (inside_digits == 1) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^[0-9]*\\.", ".", sprintf('%.1f',inside_x))))
          } else {
            return(paste(sub("^[0-9]*\\.", ".", sprintf('%.1f',inside_x))))
          }
        }
        
      }  
      
      # When 'x' is negative ----------------------------------------------------
      
      # Check if 'x' is more than -0.001
      else if (-0.001 <= inside_x & inside_x < 0) {
        if (inside_eql == TRUE) {
          return(paste("> -.001"))
        } else {
          return(paste("-.001"))
        }
      }
      
      # Check if 'x' is between 0.001 and 0.01
      else if (-0.01 < inside_x & inside_x <= -0.001) {
        if (inside_digits == 3) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^-[0-9]*\\.", "-.", sprintf('%.3f',inside_x))))
          } else {
            return(paste(sub("^-[0-9]*\\.", "-.", sprintf('%.3f',inside_x))))
          }
        }
        
        if (inside_digits == 2) {
          if (inside_eql == TRUE) {
            return(paste("> -.01"))
          } else {
            return(paste("-.01"))
          }
        }
        
        if (inside_digits == 1) {
          if (inside_eql == TRUE) {
            return(paste("> -.1"))
          } else {
            return(paste("-.1"))
          }
        }
        
      }
      
      # Check if 'x' is between 0.01 and 0.1
      else if (-0.1 < inside_x & inside_x <= -0.01) {
        if (inside_digits == 3) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^-[0-9]*\\.", "-.", sprintf('%.3f',inside_x))))
          } else {
            return(paste(sub("^-[0-9]*\\.", "-.", sprintf('%.3f',inside_x))))
          }
        }
        
        if (inside_digits == 2) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^-[0-9]*\\.", "-.", sprintf('%.2f',inside_x))))
          } else {
            return(paste(sub("^-[0-9]*\\.", "-.", sprintf('%.2f',inside_x))))
          }
        }
        
        if (inside_digits == 1) {
          if (inside_eql == TRUE) {
            return(paste("> -.1"))
          } else {
            return(paste("-.1"))
          }
        }
        
      }
      
      # Check if 'x' is greater than or equal to 0.1
      else if (-1 < inside_x & inside_x <= -0.1) {
        if (inside_digits == 3) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^-[0-9]*\\.", "-.", sprintf('%.3f',inside_x))))
          } else {
            return(paste(sub("^-[0-9]*\\.", "-.", sprintf('%.3f',inside_x))))
          }
        }
        
        if (inside_digits == 2) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^-[0-9]*\\.", "-.", sprintf('%.2f',inside_x))))
          } else {
            return(paste(sub("^-[0-9]*\\.", "-.", sprintf('%.2f',inside_x))))
          }
        }
        
        if (inside_digits == 1) {
          if (inside_eql == TRUE) {
            return(paste("=", sub("^-[0-9]*\\.", "-.", sprintf('%.1f',inside_x))))
          } else {
            return(paste(sub("^-[0-9]*\\.", "-.", sprintf('%.1f',inside_x))))
          }
        }
        
      }
    } 
    
    else {
      stop("Invalid value for 'x' argument. Must be numeric.")
      # stop("Invalid value for 'x' argument. Must be numeric or a matrix.")
    }
  
  }



# When argument 'x' is data frame -----------------------------------------
  
  if (is.data.frame(x)) {
    
    if (all(-1 < x & x < 1)) {
      
      if (nrow(x) == 1 & ncol(x) == 1) {
        x_replaced <- NULL
        x_replaced <- as.numeric(x)
        return(mainprocess(inside_x = x_replaced))
      } 
      
      else {
        n_rows <- nrow(x)
        n_cols <- ncol(x)
        
        x_replaced <- data.frame()
        for (column in 1:n_cols) {
          for (row in 1:n_rows) {
            x_replaced[row, column] <- mainprocess(inside_x = x[row, column])
          }
        }
        
        return(x_replaced)
      }
      
    } else {
      stop("Invalid value for 'x' argument. 'x' must inclued no absolute values larger than 1.")
    }
    
  }
  
  
  
  # When argument 'x' is matrix ---------------------------------------------

  else if (is.matrix(x)) {
    
    if (all(-1 < x & x < 1)) {
      
      n_rows <- nrow(x)
      n_length <- length(x)
      
      x_replaced <- matrix()
      for (cell in 1:n_length) {
        x_replaced[cell] <- mainprocess(inside_x = x[cell])
      }
      
      return(matrix(x_replaced, nrow = n_rows))
    }
    
    else {
      stop("Invalid value for 'x' argument. 'x' must inclued no absolute values larger than 1.")
    }
    
  }

  

# When argument 'x' is vector ---------------------------------------------

  else if (is.vector(x)) {
    
    if (all(-1 < x & x < 1)) {
      
      n_length <- length(x)
      
      x_replaced <- vector()
      for (cell in 1:n_length) {
        x_replaced[cell] <- mainprocess(inside_x = x[cell])
      }
      
      return(x_replaced)
    }
    
    else {
      stop("Invalid value for 'x' argument. 'x' must inclued no absolute values larger than 1.")
    }
    
  }  
  
  
  
  # When argument 'x' is numeric --------------------------------------------
  
  else if (is.numeric(x)) {
    
    if (-1 < x & x < 1) {
      return(mainprocess(inside_x = x))
    } 
    
    else {
      stop("Invalid value for 'x' argument. 'x' must inclued no absolute values larger than 1.")
      # stop("Invalid value for 'x' argument. Must be numeric or a matrix.")
    }
    
  }
  
  
  
  # Handle other types of invalid input values for 'x' ----------------------
  else {
    stop("Invalid value for 'x' argument")
    # stop("Invalid value for 'x' argument. Must be numeric or a matrix.")
  }
  
}


