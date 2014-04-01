# Ilosc  prawdopodobienstwa pomiedzy nilosc sigma
plotSigmaExample = function(sigma, distr, df = 3)
{
  xx = seq(-5,5,length.out=500)
  if(distr == "norm")
  {
    zz = (paste(paste("Rozkład normalny\nZaznaczony obszar to około\n", round(100*(pnorm(sigma,0,1) - pnorm(-sigma,0,1)),2)),"% prawdopodobieństwa",sep = ""))
    curve(dnorm(x),xlim = c(-4,4), ylab = "", main = zz, cex.main = 1)
    yy = dnorm(xx)
  }
  if(distr == "t")
  {
    sigma2 = sigma*sqrt(df/(df-2))
    zz = (paste(paste("Rozkład t\nZaznaczony obszar to około\n", round(100*(pt(sigma2,df=df) - pt(-sigma2,df=df)),2)),"% prawdopodobieństwa",sep = ""))
    curve(dt(x,df=df),xlim = c(-4,4), ylab = "", main = zz)
    yy = dt(xx,df=df)
  }
  
  name = paste0(sigma,"* sigma")
  xxx = c(xx[abs(xx)<=sigma])
  xxx = c(xxx,rev(xxx))
  yyy = c(yy[abs(xx)<=sigma],rep(0,sum(abs(xx)<=sigma)))
  if(distr == "norm") polygon(xxx,yyy, col = rgb(1,0,0,0.5))
  if(distr == "t") polygon(xxx,yyy, col = rgb(0,1,0,0.5))
  segments(x0=0,x1=0,y0=0,y1 = dnorm(0))
  t = 0.05
  if(sigma == 3) t = 0.001
  text(x=c(-sigma/2,sigma/2), y = 0.07, labels=parse(text=name))
  #arrows(x0=c(0,0),x1=c(-sigma,sigma), y1 = t,y0 = t)
}
