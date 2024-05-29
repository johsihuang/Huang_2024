#####
align_traces <- function(imglist = imglist){

	####
	outlist <- list()

	####
	for(i in 1:length(imglist)){

		####
		img_i <- imglist[[i]]
		pos_i <- unique(img_i$POSITION_X)

		####
		for(j in 1:length(pos_i)){

			####
			pos_ij <- pos_i[j]
			row_ij <- which(img_i$POSITION_X == pos_ij)
			img_ij <- img_i[row_ij, ]

			####
			apop_ij <- which(img_ij$OR_FullyApoptotic >= 1e3) # choice of 1e3 is rather arbitrary but seems to work

			####
			if(length(apop_ij) > 0){

				####
				apop_ij <- min(apop_ij, na.rm = TRUE)

				####
				img_i[row_ij, ]$FULLAPOP_t_min <- img_ij$t_min - img_ij[apop_ij, ]$t_min

			}

			####
			if(length(apop_ij) == 0){
				next}			

		}

		outlist[[i]] <- img_i

	}

	####
	names(outlist) <- names(imglist)

	####
	return(outlist)

}
