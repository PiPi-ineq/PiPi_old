egini <- function(df=NULL, p=NULL, k=NULL, normalize = TRUE, plot=FALSE) {
#check assumptions for 'k' and 'p'
	if (!is.null(df)) {
		if (!all(colnames(df) %in% c("p", "k")))
			stop("'df' cannot contain 'p' and 'k' columns")
		p <- df$p
		k <- df$k
	}	
	if (any(p<0)) 
        stop("'p' cannot contain negative values!")
	if (any(k<0)) 
        stop("'k' cannot contain negative values!")
	if (all(p==0)) 
        stop("'p' cannot contain only 0 values!")
	if (all(k==0)) 
        stop("'k' cannot contain only 0 values!")
	if (length(p) != length(k))
		stop("The lengths of the vectors 'p' and 'k' must be the same!")
	if (is.null(p) | is.null(k)) 
		stop("'p' and 'k' must contain valid values!")
	if (!is.numeric(p) | !is.numeric(p))
		stop("'p' and 'k' must contain numeric values!")
	if (sum(p) != 1) {
		warning("The sum of p is not equal to 100%!\n", call. = FALSE)
		if (normalize) {
			warning("'p' vector is normalized\n")
			p <- p/sum(p)  
		} else {
			stop("Please correct the p values, or set 'normalize' parameter to 'TRUE'!")
		}
	}
	if (sum(k) != 1) {
		warning("The sum of k is not equal to 100%!\n", call. = FALSE)
		if (normalize) {
			warning("'k' vector is normalized\n")
			k <- k/sum(k)  
		} else {
			stop("Please correct the k values, or set 'normalize' parameter to 'TRUE'!")
		}
	}
	x		<- data.frame(p, k, stringsAsFactors=F)
	x$slope	<- x$p/x$k   
	x		<- x[order(x$slope),]
	x$c		<- cumsum(x$k)
	k		<- (x$k*x$p)/2
	kk		<- x$p*(1-x$c)
	egini	<- 2*(0.5-(sum(k)+sum(kk)))
#	print(x)
	class(egini) <- 'egini'
	if (plot) {
		plot.egini(x, value=egini)
	}
	return(egini)
}
