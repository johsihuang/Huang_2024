#####
calculate_kr <- function(Status, Cytoplasm){

	####
	if(Status == 'Original / diluted'){

		###
		kr <- 0.0057 - 0.0023 * Cytoplasm
		# from experimentally determined scaling (linear model) for original / diluted extracts

	}

	else if(Status == 'Concentrated / reconstituted'){

		###
		kr <- 0.0044 - 0.0015 * Cytoplasm
		# from experimentally determined scaling (linear model) for concentrated / reconstituted extracts

	}

	####
	return(kr)

}
