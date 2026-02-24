combinations <- function(input){
  #browser()
  if(input != "" & input != 0){
    mtx <- as.numeric(unlist(strsplit(as.character(input),split = ",")))
    if(length(mtx) > 3){
      pattern <- 0
      ###For loop from 2 till no of unique elements in input
      for(n in c(2:floor(length(mtx)/2))){
        if(all(pattern == 0)){
          comb <- combn(mtx,n) #Creating different combinations
          dup_col <- duplicated(t(comb)) 
          comb_new <- comb[,!dup_col] #Getting unique combinations
          try_final <- 0
          match_pat <- numeric()
          if(is.matrix(comb_new)){
            ###Checking each combination if it exist in the pattern
            for(i in c(1:ncol(comb_new))){
              pat <- comb_new[,i]
              try <- sapply(1:(length(mtx)-length(pat)+1),function(x) all(mtx[x:(x+length(pat)-1)] == pat))
              #print(any(try == TRUE))
              true_count <- length(try[try == TRUE])
              match_pat <- append(match_pat, true_count)
            }
            
            match_df <- data.frame("No" = c(1: (length(match_pat))),true_count = match_pat)
            match_comb <- comb_new[,match_df[match_df$true_count == max(match_df$true_count),]$No] #Getting only matching combinations
          }
          if(is.matrix(match_comb)){
            for(i in c(1:ncol(match_comb))){
              if(floor(length(mtx)/length(match_comb[,i])) > 1){
                if(all(rep(match_comb[,i],floor(length(mtx)/length(match_comb[,i]))) == mtx[1:length(rep(match_comb[,i],floor(length(mtx)/length(match_comb[,i]))))]) == TRUE){
                  pattern <- match_comb[,i]
                  break
                }
              }
            }
            
          }else{
            if(floor(length(mtx)/length(match_comb)) > 1){
              if(all(rep(match_comb,floor(length(mtx)/length(match_comb))) == mtx[1:length(rep(match_comb,floor(length(mtx)/length(match_comb))))]) == TRUE){
                pattern <- match_comb
                break
              }
            }
          }
        }
      }
      
      if(all(pattern == 0)){
        pattern <- "No Pattern"
      }
    }else{
      pattern <- "No Pattern"
    }
  }else{
    pattern <- "No Pattern"
  }
  
  return(pattern)
}