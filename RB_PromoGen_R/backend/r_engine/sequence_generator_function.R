sequence_generator = function(slots) {
  # RED browser() for performance
  l=expand.grid(rep(list(0:1), slots)) #2^slots number of combinations
  
  lag(l$Var1,n = 1,default=0)[1]
  
  l_t <- t(l)
  
  res.rows <- apply(l, MARGIN = 1, function(x){1 %in% unique(x*dplyr::lag(x,n = 1))})
  
  #  l$Var1_lagged <- lag(l$Var1, n = 1, default = 0)  # Use 'default' to replace NA with 0
  
  # Transpose (if needed, depends on your data structure)
  # l_t <- t(l$Var1_lagged)
  
  # Apply function to check if there's a 1 in the product of x and lagged x
  #res.rows <- apply(l_t, MARGIN = 1, function(x) {
  # 1 %in% unique(x * lag(x, n = 1, default = 0))  # Replace lag NA with 0 here as well
  #})
  
  
  df.result <- l[!res.rows,]
  df.result<- as.data.frame(df.result)
  length(unique(apply(df.result,1,paste, collapse="")))
  
  df.result[,"Sum"] = rowSums(df.result)
  df.result[,"Variable_Name"] = paste0("V",c(1:nrow(df.result)),"_",df.result$Sum)
  
  row.names(df.result) = df.result$Variable_Name
  df.result$Sum = NULL
  df.result$Variable_Name = NULL
  
  final_data = as.matrix(df.result)
  final_data = as.data.frame(t(df.result))
  
  return(final_data)
}
