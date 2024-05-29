#####
read_img <- function(meta = meta){

	####
	imgnamelist <- list.files() 
	imgnamelist <- imgnamelist[str_which(string = imgnamelist, pattern = '.tif', negate = FALSE)] # select .tif files 
	print(imgnamelist)

	####
	imglist <- list()

	####
	for(i in 1:length(imgnamelist)){

		####
		img_i <- as_tibble(t(as.array(readImage(imgnamelist[i], type = 'TIFF')))) 
		# reformat into a tibble for easier handle
		# EBImage transposes images so transpose back
		# as.array coerce image into array 

		####
		length_x <- ncol(img_i)
		colnames(img_i) <- 1:length_x

		####
		img_i$POSITION_T <- as.numeric(rownames(img_i)) - 1
		img_i$t_min <- img_i$POSITION_T * 2 # for tubling experiments, frame rate always 1 frame per 2 mins

		####
		img_i <- pivot_longer(
			data = img_i, 
			cols = 1:length_x, 
			names_to = 'POSITION_X', 
			values_to = 'INTENSITY')

		img_i$POSITION_X <- as.numeric(img_i$POSITION_X)
		img_i <- arrange(img_i, POSITION_X)

		####
		img_i$POSITION_X_um <- 2.58 * (img_i$POSITION_X - min(img_i$POSITION_X)) # 1 px = 2.58 um

		####
		img_i$SCALED_INTENSITY <- NA 
		img_i$MODEL_INTENSITY <- NA
		img_i$ALIGNED_MODEL_INTENSITY <- NA 
		img_i$OR_FullyApoptotic <- NA 
		img_i$FULLAPOP_t_min <- NA
		img_i$ALIGNED_t_min <- NA

		####
		img_i$Extract <- meta$Extract
		img_i$Cytoplasm <- meta$Cytoplasm
		img_i$Status <- meta$Status 

		#### 
		img_i$NOMINAL_rtot <- meta$NOMINAL_rtot
		img_i$NOMINAL_ctot <- meta$NOMINAL_ctot
		img_i$MEASURED_kr <- meta$MEASURED_kr
		img_i$MEASURED_r0 <- NA
		img_i$ALIGNED_MEASURED_r0 <- NA

		####
		imglist[[i]] <- img_i

	}

	####
	names(imglist) <- imgnamelist

	####
	return(imglist)

}