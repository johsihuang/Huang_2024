#####
saemix_r110 <- function(psi, id, xidep){

	####
	###
	t_min <- xidep[, 1] 
	rtot <- xidep[, 2] 
	ctot <- xidep[, 3]
	r0 <- xidep[, 4]
	kr <- xidep[, 5] 

	###
	c0 <- psi[id, 1] 
	kc <- psi[id, 2]
	kb <- psi[id, 3]

	####
	r <- rtot - (rtot - r0) * ((c0 / ctot) * (exp(kc * ctot * t_min) - 1) + 1)^(-kr / kc) * exp(-kb * t_min)
	
	####
	return(r)
	
}
