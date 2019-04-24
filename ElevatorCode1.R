# At a random time lapse (30s) generate 'h' hall calls

# Assign a number of passengers and a number of destinations to each hall call

# Go to destinations in an order (from closest to most distant)

# If car is full: don't take people until it's empty

# The destination is different than the position

# Compute waiting time for each hall call

hlist = vector()
plist = vector()
dlist = as.list(rep(NA, 10000))
tlist = vector()

wlist = vector()


for (i in 1:10000) {
  
  # The Hall Call
  
  h = sample(1:4, size = 1, replace = T, prob = c(0.4, 0.3, 0.2, 0.1))
  
  hlist[i] = h
  
  # The number of passengers
  
  p = sample(1:3, size = 1, replace = T)
  
  plist[i] = p
  
  # The destinations of the passengers

  x = 0:4
  
  d = sample(x[-h], size = p, replace = T)
  
  dlist[[i]] = d
  
  if (sum(d>h) < sum(d<h)) {
    
    t = 0
    
    d = sort(d , decreasing = T)
    
    for (u in length(d)) {
      
      if (u == 1) {
        
        t = t + abs(d[u]-h)*2 + 6
        
      } else {
        
        t = t + abs(d[u]-d[u-1])*2 + 6
      }
      
    }
    
    tlist[i] = t
  }
  
  else if (sum(d>h) > sum(d<h) || sum(d>h) == sum(d<h)) {
    
    t = 0
    
    d = sort(d, decreasing = F)
    
    for (u in length(d)) {
      
      if (u == 1) {
        
        t = t + abs(d[u]-h)*2 + 6
      } else {
        
        t = t + abs(d[u]-d[u-1])*2 + 6
      }
      
    }
    
    tlist[i] = t
  }
  
  else  {
    
    tlist[i] = max(abs(h-d))*2 + length(unique(d))*6
    
  }
  
  if (i > 1) {
    
    c = d[length(d)]
    
    w = 0.8*(abs(h-c)*2 + tlist[i-1])
    
    wlist[i] = w
  }
  
  
}