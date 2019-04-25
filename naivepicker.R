naivepicker  = function(t, config, cars, call) {
  
  
  distance   = Inf
  
  for (car in 1:length(config$numcars)) {
    
    fromY      = call[2] * config$floorheight
    
    difference = abs(cars[car]$y - fromY)
    
    if (difference < distance) {
      
      distance = difference
      
      carIndex = car
    }
  }
  
  return(carIndex)
  
}