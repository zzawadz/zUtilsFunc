getEqualNumBreaks = function(x, k = 50)
{
  n = length(x)
  
  x = sort(x)
  rest = n-(n%/%k)*k
  minmax = rep(rest%/%2  ,2)
  minmax[1] = minmax[1] + rest%%2
  
  br = c(1, cumsum(c(minmax[1],rep(k,n%/%k),minmax[2])))
  breaks = x[br]
  
  nb = length(breaks)
  if(breaks[nb] == breaks[nb-1]) breaks = breaks[-nb]
  
  breaks[1] = breaks[1]-1e-6
  breaks[length(breaks)] = breaks[length(breaks)]+1e-6
  breaks
}


