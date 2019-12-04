eginiMin<- function(np=NULL, sh=NULL, sim=1000, type="empirical", prob=0.95) { 
	ng	<- length(sh)
	k1	<- do.call(cbind, rlply(sim,eginiCalc(np=np, sh=sh), .progress = "text"))
	mu	<- mean(k1)
	sds	<- sd(k1)
	medianv <- median(k1)
	if (type=="classic") {
		q	<- qnorm((1-(1-prob)/2), 0, 1)
		k_f	<- mu+q*sds/sqrt(nrow(k1))
		k_a	<- mu-q*sds/sqrt(nrow(k1))	  
	}
	if (type=="empirical") {
		q	<- c((1-prob)/2, (1-(1-prob)/2))
		k_f	<- sort(k1)[round(length(k1)*q[2])]
		k_a <- sort(k1)[round(length(k1)*q[1])]
	}
	simData	<- list(egini.minimum.mean=mu, egini.minimum.median=medianv, egini.minimum.sd=sds, CI_low=k_a, CI_high=k_f, np=np, ng=ng, sim=sim, prob=prob)
	class(simData) <- "eginiMinSim"
	return(simData)
}
