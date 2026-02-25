rounding_lsm = function(x,col1,col2){

  # x = lsm_new
  # col1 = "Max_Shelf_Weeks"
  # col2 = "Max_Display_Weeks"
  
  x[,Shelf_Remainder := get(col1)%%3]
  x[,Display_Remainder := get(col2)%%3]
  
  df_names = names(x)
  
  x = data.frame(x)
  
  for(i in 1:nrow(x)){ #for
    
    #i = 1
    vec = c(x$Shelf_Remainder[i], x$Display_Remainder[i])
    
    if( sum(vec == c(0,0)) == 2 ){
      x[i,col1] = x[i,col1]
      x[i,col2] = x[i,col2]
    }
    
    if( sum(vec == c(0,1)) == 2 ){
      x[i,col1] = x[i,col1]
      x[i,col2] = x[i,col2] + 2
    }
    
    if( sum(vec == c(1,0)) == 2 ){
      x[i,col1] = x[i,col1] + 2
      x[i,col2] = x[i,col2]
    }
    
    if( sum(vec == c(1,1)) == 2 ){
      x[i,col1] = x[i,col1] - 1
      x[i,col2] = x[i,col2] + 2
    }
    
    if( sum(vec == c(1,2)) == 2 ){
      x[i,col1] = x[i,col1] - 1
      x[i,col2] = x[i,col2] + 1
    }
    
    if( sum(vec == c(2,1)) == 2 ){
      x[i,col1] = x[i,col1] + 1
      x[i,col2] = x[i,col2] - 1
    }
    
    if( sum(vec == c(2,2)) == 2 ){
      x[i,col1] = x[i,col1] + 1
      x[i,col2] = x[i,col2] + 1
    }
    
    if( sum(vec == c(0,2)) == 2 ){
      x[i,col1] = x[i,col1]
      x[i,col2] = x[i,col2] + 1
    }
    
    if( sum(vec == c(2,0)) == 2 ){
      x[i,col1] = x[i,col1] + 1
      x[i,col2] = x[i,col2]
    }
    
} #for

  names(x) = df_names
  
return(data.table(x))
  
}