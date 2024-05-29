#####
find_r0 <- function(imglist = lmglist){

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
			tmp_r0 <- img_ij[which(img_ij$t_min == 0), ]$SCALED_INTENSITY

			if(length(tmp_r0) == 1){
				img_i[row_ij, ]$MEASURED_r0 <- tmp_r0}

			####
			tmp_r0_a <- img_ij[which(img_ij$ALIGNED_t_min == 0), ]$SCALED_INTENSITY

			if(length(tmp_r0_a) == 1){
				img_i[row_ij, ]$ALIGNED_MEASURED_r0 <- tmp_r0_a}

		}

		####
		outlist[[i]] <- img_i

	}

	####
	names(outlist) <- names(imglist)

	####
	return(outlist)

}