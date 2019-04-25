makerandcall = function(numfloors) {
  
  call       = vector()
  
  call[1]    = sample(1:numfloors, size = 1, replace = T)
  
  call[2]    = sample(1:numfloors, size = 1, replace = T)
  
  while (call[1] == call[2]) {
    
    call[2]  = sample(1:numfloors, size = 1, replace = T)
  }
  
  call[3]    = sign(call[2] - call[1])
  
  return(call)
  
}