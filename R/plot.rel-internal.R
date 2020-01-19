"plot.rel" <-
  function(x, ...)
  { 
    if (!is.na(class(x)[2]) && (class(x)[2] == "icc" || class(x)[2] == "sem")){ 
      value <- "num"
    } else{
      value <-"kat"
    }
    switch(value,
           kat = {
             layout(rbind(1,2), heights=c(6,1)) 
             par(mar=c(4, 4, 4, 2))
             plot(as.vector(t(x$data))~rep(1:x$sample,each=x$obs), 
                  pch=26-1:x$obs, col=1+1:x$obs, bg=1, yaxt="n",
                  main="Observations per subject", xlab="Subject", ylab="Data value")
             axis(2, las=1, at=min(x$data,na.rm=T):max(x$data,na.rm=T))
             abline(v=1:x$sample,col=rgb(0,0,1,0.4),lty=3)
             par(mar=c(0, 0, 0, 0))
             plot.new()
             legend("center",legend=paste("Obs",1:x$obs),
                    pch=26-1:x$obs,col=1+1:x$obs, bty="n", ncol=x$obs)
             layout(cbind(1,1))
             par(mar = c(5,4,4,2))
           },
           num = {
             layout(rbind(1,2), heights=c(6,1)) 
             par(mar=c(4, 4, 4, 2))
             plot(as.vector(t(x$data))~rep(1:x$sample,each=x$obs), 
                  pch=26-1:x$obs, col=1+1:x$obs, bg=1,
                  main="Observations per subject", xlab="Subject", ylab="Data value")
             abline(v=1:x$sample,col=rgb(0,0,1,0.4),lty=3)
             par(mar=c(0, 0, 0, 0))
             plot.new()
             legend("center",legend=paste("Obs",1:x$obs),
                    pch=26-1:x$obs,col=1+1:x$obs, bty="n", ncol=x$obs)

             readline(prompt = "Hit <Enter> to see next plot:")
             
             layout(cbind(1,2), widths=c(3,1))  
             par(mar=c(4, 4, 4, 0))
             plot(as.vector(t(x$data-rowMeans(x$data)))~rep(1:x$sample,each=x$obs), 
                  lwd=1, pch=26-1:x$obs, col=1+1:x$obs, bg=1,
                  main="Observations normalized per subject", xlab="Subject", ylab="Data value")
             abline(v=1:x$sample,col=rgb(0,0,1,0.4),lty=3)
             abline(h=0,lty=2)
             par(mar=c(0, 0, 0, 0))
             plot.new()
             legend("left",legend=c(paste("Obs",1:x$obs),"Subject mean"),
                    lty=c(rep(NA,x$obs),2),pch=c(26-1:x$obs,NA),
                    col=c(1+1:x$obs,1), bty="n")

             readline(prompt = "Hit <Enter> to see next plot:")
             
             layout(cbind(1,2), widths=c(3,1)) 
             par(mar=c(4, 4, 4, 0))
             plot(as.vector(t(x$data))~rep(1:x$sample,each=x$obs), 
                  pch=16, col=1, bg=1, cex=0.6,
                  main="Average ratings", xlab="Subject", ylab="Data value")
             abline(v=1:x$sample,col=rgb(0,0,1,0.4),lty=3)
             abline(h=mean(x$data,na.rm=T),lty=3,col=1) #Grand mean
             abline(h=colMeans(x$data,na.rm=T),lty=3,col=1+1:x$obs) #Observation mean
             points(rowMeans(x$data,na.rm=T),pch=22,col=1,bg=18) #Subject mean
             par(mar=c(0, 0, 0, 0))
             plot.new()
             legend("left",legend=c("Observation","Subject mean",paste("Obs",1:x$obs,"mean"),"Grand mean"),
                    lty=c(NA,NA,rep(3,x$obs+1)),pch=c(16,15,rep(NA,x$obs),NA),
                    col=c(1,18,1+1:x$obs,1), bty="n")
             
             readline(prompt = "Hit <Enter> to see next plot:")
             
             layout(rbind(1,2), heights=c(6,1)) 
             par(mar=c(4, 4, 4, 2))
             plot(apply(x$data,1,sd), lwd=1, pch=15, col=10,
                  main="Error per subject", xlab="Subject", ylab="Data value")
             segments(x0=1:dim(x$data)[1], y0=apply(x$data,1,sd), y1=0, col=rgb(0,0,0,0.2))
             abline(h=x$est,lty=2)
             par(mar=c(0, 0, 0, 0))
             plot.new()
             legend("center",legend=c("Subject error   ","Mean error"), lty=c(NA,2),
                    pch=c(15,NA),col=c(10,1), bty="n", ncol=2)
             
             readline(prompt = "Hit <Enter> to see next plot:")
             
             layout(cbind(1,1))
             par(mar = c(5,4,4,2))
             }
    )
  }