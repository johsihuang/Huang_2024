#####
scale_intensity <- function(imglist = imglist){

	####
	outlist <- list()

	####
	for(i in 1:length(imglist)){

		####
		img_i <- imglist[[i]]

		####
		rtot <- unique(img_i$NOMINAL_rtot)

		####
		set.seed(123)
		mix <- normalmixEM(x = img_i$INTENSITY, k = 2)

		####
		bigger <- which.max(mix$mu)

		####
		max_i <- mix$mu[bigger]
		min_i <- 605 # estimated background in tube

		####
		img_i$SCALED_INTENSITY <- (img_i$INTENSITY - min_i) / (max_i - min_i) * rtot

		####
		img_i$OR_FullyApoptotic <- mix$posterior[, bigger] / mix$posterior[, -bigger]

		####
		outlist[[i]] <- img_i

	}

	####
	names(outlist) <- names(imglist)

	####
	return(outlist)

}
