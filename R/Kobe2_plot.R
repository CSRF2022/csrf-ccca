#' param PB the data and model fit coming from applying the PB model (PB.f)
#' param E the time series to overlay and colour code Kobe years
#' param Bref.multiplier a multiplier of the mean B in the reference period to adjust it to a specific reference point value
#' param col1 the colour at lowest value
#' param col2 the colour at the highest value
#' param ... par options for plot
#' keywords Kobe, reference point, Bmsy, environment
#' export
#' examples
#' Kobe.f(PB=PB,E=PB$E)
#'
#' SAME AS THE CCCA Kobe.f plot but without the legend

Kobe2.f= function(PB, E, Bref.multiplier=1, col1="blue", col2="red", offset=0.1, ...){

  Year= PB$Year
  F.rel= PB$F.rel
  Index.q= PB$Index.q
  base= PB$refererence.years==1
  E.kobe= E/mean(E[base])
  F.kobe= F.rel/mean(F.rel[base])
  B.kobe= Index.q*Bref.multiplier/mean(Index.q[base])

  plot(B.kobe,F.kobe,type="b",pch="    ",xlab=expression("B/B"["base"]),ylab=expression("F/F"["base"]),...)
  E.categ= floor(E*4)/4 #quarter degree C categories
  tempcol=colorRampPalette(c(col1, col2))(length(E.kobe))
  temperaturecolours= tempcol[order(E.categ)]
  last.year= length(F.kobe)
  points(B.kobe,F.kobe,pch=21,bg=temperaturecolours,col=temperaturecolours,cex=1)
  points(B.kobe[1],F.kobe[1],pch=21,bg=temperaturecolours[1],col=temperaturecolours[1],cex=3)
  text(B.kobe[1],F.kobe[1],PB$Year[1],col="white",cex=0.55,font=2)
  points(B.kobe[last.year],F.kobe[last.year],pch=21,bg=temperaturecolours[last.year],col=temperaturecolours[last.year],cex=3)
  text(B.kobe[last.year],F.kobe[last.year],PB$Year[last.year],col="white",cex=0.55,font=2)
  text(B.kobe[2:(last.year-1)]+offset,F.kobe[2:(last.year-1)]+offset,PB$Year[2:(last.year-1)],col=temperaturecolours[2:(last.year-1)],cex=0.55,font=2)
  abline(h=1,col="grey")
  abline(v=1,col="grey")
  #legend("topright",bty="n",cex=0.7,legend=c(paste0("Bbase=",round(B.kobe),
  #                                                  paste0("Fbase=",round(F.kobe,3)))))
}

Kobe3.f= function(PB, E, Bref.multiplier=1, col1="blue", col2="red", offset=0.1, ...){

  Year= PB$Year
  F.rel= PB$F.rel
  Index.q= PB$Index.q
  base= PB$refererence.years==1
  E.kobe= E/mean(E[base])
  F.kobe= F.rel/mean(F.rel[base])
  B.kobe= Index.q*Bref.multiplier/mean(Index.q[base])
  PB.kobe= PB$PB

  plot(B.kobe,PB.kobe,type="b",pch="    ",
       xlab=expression("B/B"["base"]),ylab=expression("P/B"),
       ylim=c(-0.75,1.1*max(PB.kobe, na.rm=T)),
       xlim=c(0,1.1*max(B.kobe, na.rm=T)),cex.lab=1.5)
  E.categ= floor(E*4)/4 #quarter degree C categories
  tempcol=colorRampPalette(c(col1, col2))(length(E.kobe))
  temperaturecolours= tempcol[order(E.categ)]
  last.year= length(F.kobe)
  points(B.kobe,PB.kobe,pch=21,bg=temperaturecolours,col=temperaturecolours,cex=1)
  points(B.kobe[1],PB.kobe[1],pch=21,bg=temperaturecolours[1],col=temperaturecolours[1],cex=3.5)
  text(B.kobe[1],PB.kobe[1],PB$Year[1],col="white",cex=0.65,font=2)
  points(B.kobe[last.year-1],PB.kobe[last.year-1],pch=21,bg=temperaturecolours[last.year-1],col=temperaturecolours[last.year-1],cex=3.5)
  text(B.kobe[last.year-1],PB.kobe[last.year-1],PB$Year[last.year-1],col="white",cex=.65,font=2)
  text(B.kobe[2:(last.year-2)]+offset,PB.kobe[2:(last.year-2)]+offset,PB$Year[2:(last.year-2)],col=temperaturecolours[2:(last.year-2)],cex=1,font=2)
  abline(h=0,col="grey")
  abline(v=1,col="grey")
}

colramp.legend2 <- function (col1 = "red", col2 = "blue", ncol, xleft, ybottom,
          xright, ytop, ...)
{
  tempcol = colorRampPalette(c(col1, col2))(ncol)
  legend_image <- as.raster(matrix(tempcol, ncol = 1))
  rasterImage(legend_image, xleft = xleft, ybottom = ybottom,
              xright = xright, ytop = ytop)
  rasterImage(legend_image, 12, 2, 13, 8)
  text(xright * 1.02, ytop, labels = round(max(PB$E, na.rm = T),
                                           1), cex = 1.5)
  text(xright * 1.02, ybottom, labels = round(min(PB$E, na.rm = T),
                                              1), cex = 1.5)
}
