#####
caspSensorModel <- function(t_min, rtot, ctot, r0, c0, kr, kc, kb){

	####
	r <- rtot - (rtot - r0) * ((c0 / ctot) * (exp(kc * ctot * t_min) - 1) + 1)^(-kr / kc) * exp(-kb * t_min)

	####
	return(r)

}

#####
simulate_skeleton <- function(data, params, type = c('original', 'aligned')){
	
	####
	outlist <- list()

	####
	if(type == 'original'){

		####
		for(i in 1:length(data)){

			####
			img_name_i <- names(data)[i]
			par_rows_i <- which(params$Image_name == img_name_i)
			par_i <- params[par_rows_i, ]

			####
			img_i <- data[[i]]
			pos_i <- unique(img_i$POSITION_X)

			####
			for(j in 1:length(pos_i)){

				####
				par_ij <- par_i[j, ]

				####
				pos_ij <- pos_i[j]
				row_ij <- which(img_i$POSITION_X == pos_ij)

				####
				img_i[row_ij, ]$MODEL_INTENSITY <- caspSensorModel(
					t_min = img_i[row_ij, ]$t_min, 
					rtot = par_ij$NOMINAL_rtot, 
					ctot = par_ij$NOMINAL_ctot, 
					r0 = unique(img_i[row_ij, ]$MEASURED_r0), 
					c0 = par_ij$SAEMIX_c0, 
					kr = par_ij$MEASURED_kr, 
					kc = par_ij$SAEMIX_kc, 
					kb = par_ij$SAEMIX_kb)

			}

			outlist[[i]] <- img_i

		}


	}

	####
	else if(type == 'aligned'){

		####
		for(i in 1:length(data)){

			####
			img_name_i <- names(data)[i]
			par_rows_i <- which(params$Image_name == img_name_i)
			par_i <- params[par_rows_i, ]

			####
			img_i <- data[[i]]
			pos_i <- unique(img_i$POSITION_X)

			####
			for(j in 1:length(pos_i)){

				####
				par_ij <- par_i[j, ]

				####
				pos_ij <- pos_i[j]
				row_ij <- which(img_i$POSITION_X == pos_ij)

				####
				img_i[row_ij, ]$ALIGNED_MODEL_INTENSITY <- caspSensorModel(
					t_min = img_i[row_ij, ]$ALIGNED_t_min, 
					rtot = par_ij$NOMINAL_rtot, 
					ctot = par_ij$NOMINAL_ctot, 
					r0 = unique(img_i[row_ij, ]$ALIGNED_MEASURED_r0), 
					c0 = par_ij$SAEMIX_c0, 
					kr = par_ij$MEASURED_kr, 
					kc = par_ij$SAEMIX_kc, 
					kb = par_ij$SAEMIX_kb)

			}

			outlist[[i]] <- img_i

		}


	}

	####
	names(outlist) <- names(data)

	####
	return(outlist)

}