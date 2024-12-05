
# 1 -----------------------------------------------------------------------

sort_loadings <- function(fa_mtrx, va = TRUE, nointgr = FALSE) {
  # Special thanks to ChatGPT, BingChat, & http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R
  
  if (!inherits(fa_mtrx, "fa") & 
      !is.null(fa_mtrx$loadings) & 
      !is.null(fa_mtrx$communality)) {
    stop("Input object must be created by psych::fa()")
  }
  
  if (class(va) != "logical") {
    stop("'va' must be class logical")
  }
  
  loading_mtrx <- fa_mtrx$loadings
  communality <- fa_mtrx$communality
  items <- rownames(fa_mtrx$loadings)
  
  # Check if the number of factors is greater than the number of columns in loading_mtrx
  ncol_loading_mtrx <- ncol(loading_mtrx) # get number of columns in loading_mtrx
  if (ncol(loading_mtrx) > ncol_loading_mtrx) {
    stop("The number of factors is greater than the number of columns in loading_mtrx.")
  }
  
  
  abs_mtrx <- abs(loading_mtrx)
  loc_max <- apply(abs_mtrx, 1, which.max)
  
  if (nointgr == TRUE) {
    if (!exists("subnum")) {
      stop("function 'subnum' not defined")
      
    } 
    else {
      result_mtrx <- cbind(
        items,
        subnum(x = cbind(loading_mtrx, communality), digits = 2, eql = FALSE))
    } 
  }  
  else {
    result_mtrx <- cbind(items, loading_mtrx, communality)
  }
  
  
  sorted <- NULL
  
  for (cnt in 1:ncol(abs_mtrx)) {
    
    container_max <- result_mtrx[loc_max == cnt, , drop = FALSE]
    
    if (nrow(container_max)) {
      
      ordered <- order(container_max[, 1 + cnt], decreasing = TRUE)
      # The reason of "1 + cnt" is that the 1st column is the vector of items names
      
      sorted <- rbind(sorted, container_max[ordered, , drop = FALSE])
      
    }
  }
  
  sorted <- as.matrix(sorted)
  
  if (va == TRUE) {
    rownames_vaccoounted <- rownames(fa_mtrx[["Vaccounted"]])
    vaccounted <- as.data.frame(matrix(sprintf(fmt = '%.2f', fa_mtrx[["Vaccounted"]]),
                                       ncol = ncol_loading_mtrx))
    vaccounted <- as.matrix(cbind(rownames_vaccoounted,vaccounted, rep(NA,5)))
    sorted_va <- rbind(sorted,vaccounted)
    return(data.frame(sorted_va))
  } else {
    return(data.frame(sorted))
  }
  
}


# 2 -----------------------------------------------------------------------

sort_loadings2 <- function(fa_mtrx, nfac, va = TRUE) {
  # Special thanks, ChatGPT
  
  if (!inherits(fa_mtrx, "fa") & 
      !is.null(fa_mtrx$loadings) & 
      !is.null(fa_mtrx$communality)) {
    stop("Input object must be created by psych::fa()")
  }
  
  if (class(va) != "logical") {
    stop("va must be class logical")
  }
  
  loading_mtrx <- fa_mtrx$loadings
  communality <- fa_mtrx$communality
  container_num <- vector("list", nfac)
  container_vss <- vector("list", nfac)
  
  ncol_loading_mtrx <- ncol(loading_mtrx) # get number of columns in loading_mtrx
  
  # Check if the number of factors is greater than the number of columns in loading_mtrx
  if (nfac > ncol_loading_mtrx) {
    stop("The number of factors is greater than the number of columns in loading_mtrx.")
  }
  
  result_mtrx <- cbind(loading_mtrx,communality)
  abs_mtrx <- abs(loading_mtrx)
  result_vss <- t(apply(abs_mtrx, 1, function(x) ifelse(x == max(x),1,0)))
  
  for (cnt in 1:nfac) {
    container_vss <- result_vss[result_vss[,cnt] == 1, , drop = FALSE]
    container_num[[cnt]] <- result_mtrx[rownames(container_vss),]
    container_num[[cnt]] <- container_num[[cnt]][order(-abs(container_num[[cnt]][,cnt])),]
    result_vss <- result_vss[result_vss[,cnt] == 0, , drop = FALSE]
  }
  
  sorted <- do.call(rbind, container_num)

  if (va == TRUE) {
    vaccounted <- fa_mtrx[["Vaccounted"]]
    vaccounted <- cbind(vaccounted, rep(NA,5))
    sorted_va <- rbind(sorted,vaccounted)
    return(sorted_va)
  } else {
    return(sorted)
  }
  
}

