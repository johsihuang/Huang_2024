#####
mark_alignedTime <- function(imglist = imglist, start_time = -30){

	####
	outlist <- list()

	####
	for(i in 1:length(imglist)){

		####
		img_i <- imglist[[i]]
		pos_i <- unique(img_i$POSITION_X)

		####
		img_i$ALIGNED_t_min <- img_i$FULLAPOP_t_min - start_time

		####
		outlist[[i]] <- img_i

	}

	####
	names(outlist) <- names(imglist)

	####
	return(outlist)

}

######
#select_timeWindow <- function(imglist = imglist, end_time = 60){
#
#	####
#	outlist <- list()
#
#	####
#	for(i in 1:length(imglist)){
#
#		####
#		img_i <- imglist[[i]]
#
#		####
#		img_i <- img_i[which(img_i$t_min <= end_time), ] 
#
#		####
#		outlist[[i]] <- img_i
#
#	}
#
#	####
#	return(outlist)
#
#}
#
######
#select_timeWindow_aligned <- function(imglist = imglist, start_time = -30, end_time = 30){
#
#	####
#	outlist <- list()
#
#	####
#	for(i in 1:length(imglist)){
#
#		####
#		img_i <- imglist[[i]]
#		pos_i <- unique(img_i$POSITION_X)
#
#		####
#		img_i$ALIGNED_t_min <- img_i$FULLAPOP_t_min - start_time
#
#		####
#		pos_t0 <- img_i[which(img_i$ALIGNED_t_min == 0), ]$POSITION_X
#		img_i <- img_i[which(img_i$POSITION_X %in% pos_t0), ]
#
#		####
#		duration <- end_time - start_time
#		img_i <- img_i[which(img_i$ALIGNED_t_min <= duration), ]
#
#		####
#		outlist[[i]] <- img_i
#
#	}
#
#	####
#	return(outlist)
#
#}
