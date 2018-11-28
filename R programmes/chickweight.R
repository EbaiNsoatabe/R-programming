ChickWeight$Diet <- as.numeric(ChickWeight$Diet)
ndiets <- max(ChickWeight$Diet[ChickWeight$Chick == 1])
xrange <- range(ChickWeight$Time[ChickWeight$Chick == 1])
yrange <- range(ChickWeight$weight[ChickWeight$Chick == 1])

plot(xrange, yrange, type="n", xlab="Time",
     ylab = "Weight (g)")
colours <- rainbow(ndiets)
linetype <- c(1:ndiets)
plotchar <- seq(30, 30+ndiets, 1)

for(i in 1:ndiets){
  diet <- subset(ChickWeight, Diet == i)
  lines(diet$Time, diet$Weight, type="b", lwd=1.5,
        lty=linetype[i], col=colours[i], pch=plotchar[i])
}

title("Chick Weight Loss")
legend(xrange[1], yrange[2], 1:ndiets, cex=0.8, col=colours, 
       pch=plotchar, lty=linetype, title="Diet")