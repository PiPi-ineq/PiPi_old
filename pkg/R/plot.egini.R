## Plot function for egini function (supplementary)

plot.egini <- function (x, value=NULL, lwd = 2, xlab = "p", ylab = "L(p)", main = "Egini - Lorenz curve") {
#	x <- x[order(x$p),]
	L <- c(0,cumsum(x$p))
	K <- c(0,cumsum(x$k))*1/max(cumsum(kis$k))
	if (!is.null(value))
		main <- paste0(main, "\n", "egini=", round(value,4)*100, "%")
    plot(K, L, type = "l", main = main, lwd = lwd, xlab = xlab, 
        ylab = ylab, xaxs = "i", yaxs = "i")
    abline(0, max(L))
	points(K,L,pch=19, cex=2, col="blue")
}

