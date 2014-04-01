plotDensityExample = function(distr,q = c(0.001,0.999),a,b, col = "green",...)
{
  qfun = get(paste0("q",distr))
  pfun = get(paste0("p",distr))
  dfun = get(paste0("d",distr))

  q = qfun(q,...)
  
  prob = paste0(round(diff(pfun(c(a,b),...))*100,3),"%")
  desc = sprintf("Szansa na trafienie w zaznaczony obszar wynosi %s",prob)
  
  curve(dfun(x,...), from = q[1], to= q[2], main = desc, ylab = "")
  
  xx = seq(a,b,length.out=200)
  den = dfun(xx, ...)
  xxx = c(xx,rev(xx))
  den = c(den,rep(0,length(den)))
  
  polygon(xxx,den, col = addAlpha(col,alpha=0.5))
  
}
