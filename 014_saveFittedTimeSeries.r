#####
save_fittedTimeSeries <- function(data, type = c('original', 'aligned')){

	####
	out <- data[[1]][Inf, ] # [Inf, ] returns one row with all cols being NA
	out$Image_name <- NA 

	####
	for(i in 1:length(data)){

		####
		data_i <- data[[i]]
		data_i$Image_name <- names(data)[i]

		####
		out <- bind_rows(out, data_i)
	}

	out <- out[-1, ]

	####
	if(type == 'original'){

		####
		write_delim(
			file = 'SAEMIX_individualFittedTimeSeries_originalTime.txt', 
			x = out, 
			delim = "\t")

	}

	else if(type == 'aligned'){

		####
		write_delim(
			file = 'SAEMIX_individualFittedTimeSeries_alignedTime.txt', 
			x = out, 
			delim = "\t")

	}

}