plotHistogram = function(x, freq = TRUE, colors = 1:length(x), alpha = 1, nl = 20, breaks = 30, ...)
{
  names = names(x)
  colors = addAlpha(colors, alpha)
  
  hists = sapply(x, hist, plot = FALSE, simplify=FALSE, breaks = breaks)
  
  ylim = sapply(hists, function(h) if(freq == TRUE) return(h$counts) else return(h$density))
  ylim = extendrange(unlist(ylim)); ylim[1] = 0
  
  xlim = sapply(hists, function(h) return(h$breaks) )
  xlim = extendrange(unlist(xlim))
  
  plot(1,type = "n", xlim = xlim, ylim = ylim, ...)
  legend("topleft", names, col = colors, fill = colors)
  
  for(i in 1:length(hists))
  {
    hist = hists[[i]]
    if(freq == TRUE) count =  hist$counts else count = hist$density
    breaks = hist$breaks
    
    # Tworzenie wektora z liniami
    lines = seq(0,max(count), length.out = nl)
    
    # Rysowanie kolejnych slupkow
    sapply(1:length(count), function(k)
    {
      rect(xleft=breaks[k],xright=breaks[k+1],ybottom=0,ytop=count[k], border = colors[i], ...)
      lines = lines[lines<count[k]]
      
      # Ustalanie kierunku lini na histogramie
      dir = c(-1,-length(lines)) 
      if(i%%2 == 0) dir = rev(dir)
      
      #Rysowanie lini - jezeli bylyby ponizej 2 - wtedy na slupku nie ma lini
      if(length(lines)<2) return(0)
      segments(x0=breaks[k],x1=breaks[k+1],y0=lines[dir[1]],y1=lines[dir[2]], col = colors[i], ...)
    })
  }

}






