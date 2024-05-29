#####
select_timeWindow <- function(imglist = imglist, duration = 60, type = c('original', 'aligned')){

	####
	outlist <- list()

	####
	if(type == 'original'){

		####
		for(i in 1:length(imglist)){

			####
			img_i <- imglist[[i]]

			####
			img_i <- img_i[which(img_i$t_min <= duration), ]

			####
			outlist[[i]] <- img_i

		}

	}

	####
	if(type == 'aligned'){

		####
		for(i in 1:length(imglist)){

			####
			img_i <- imglist[[i]]

			####
			keep <- img_i[which(img_i$ALIGNED_t_min == 0), ]$POSITION_X
			img_i <- img_i[which(img_i$POSITION_X %in% keep), ]

			####
			img_i <- img_i[which(img_i$ALIGNED_t_min %in% seq(0, duration, 2)), ]

			####
			outlist[[i]] <- img_i

		}

	}

	####
	names(outlist) <- names(imglist)

	####
	return(outlist)

}