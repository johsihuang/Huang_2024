#####
run_saemix <- function(saemix_data, saemix_model = saemix_model, saemix_control = saemix_control){

	####
	outlist <- list()

	####
	for(i in 1:length(saemix_data)){

		####
		msg_start <- paste("Starting image #", i, "of", length(saemix_data), sep = " ")
		print(msg_start)

		####
		saemix_out_i <- saemix(model = saemix_model, data = saemix_data[[i]], control = saemix_control)

		####
		outlist[[i]] <- saemix_out_i

		####
		msg_end <- paste("Finishing image #", i, "of", length(saemix_data), ep = " ")
		print(msg_end)

	}

	####
	return(outlist)

} 