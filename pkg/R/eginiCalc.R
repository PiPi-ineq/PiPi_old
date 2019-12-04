eginiCalc	<- function(np=NULL, sh=NULL){ 
	ng		<- length(sh)
	kk		<- sample(1:ng, np, replace=TRUE, prob=sh)
	tafel	<- table(factor(kk, levels = 1:ng)) 
	kis		<- data.frame(k=sh, p=as.numeric(prop.table(tafel)))
	eginiCalc	<- egini(kis)
    return(eginiCalc)
}

