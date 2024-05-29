#####
save_saemixOut <- function(saemix_out, image_names = 1:100, meta = meta, type = c('original', 'aligned')){

	####
	pop_tab <- data.frame(
		Parameter = NA,
		Fixed_effect_mean = NA,
		Fixed_effect_sem = NA, 
		Extract = NA,
		Cytoplasm = NA,
		Status = NA, 
		Image_name = NA, 
		NOMINAL_rtot = NA,
		NOMINAL_ctot = NA, 
		MEASURED_kr = NA)

	####
	indiv_tab <- data.frame(
		SAEMIX_c0 = NA, 
		SAEMIX_kc = NA, 
		SAEMIX_kb = NA, 
		Extract = NA, 
		Cytoplasm = NA, 
		Status = NA,
		Image_name = NA, 
		NOMINAL_rtot = NA,
		NOMINAL_ctot = NA, 
		MEASURED_kr = NA)

	####
	for(i in 1:length(saemix_out)){

		####
		saemix_out_i <- saemix_out[[i]]

		#### Population effect 
		###
		pop_tab_i <- data.frame(
			Parameter = saemix_out_i@results@name.fixed,
			Fixed_effect_mean = saemix_out_i@results@fixed.effects,
			Fixed_effect_sem = saemix_out_i@results@se.fixed, 
			Extract = meta$Extract,
			Cytoplasm = meta$Cytoplasm,
			Status = meta$Status, 
			Image_name = image_names[i], 
			NOMINAL_rtot = meta$NOMINAL_rtot,
			NOMINAL_ctot = meta$NOMINAL_ctot, 
			MEASURED_kr = meta$MEASURED_kr)

		###
		pop_tab <- bind_rows(pop_tab, pop_tab_i)

		#### Individual fits
		###
		saemix_indiv_i <- saemix_out_i@results@map.psi

		###
		indiv_tab_i <- data.frame(
			SAEMIX_c0 = saemix_indiv_i$c0, 
			SAEMIX_kc = saemix_indiv_i$kc, 
			SAEMIX_kb = saemix_indiv_i$kb, 
			Extract = meta$Extract, 
			Cytoplasm = meta$Cytoplasm, 
			Status = meta$Status, 
			Image_name = image_names[i], 
			NOMINAL_rtot = meta$NOMINAL_rtot,
			NOMINAL_ctot = meta$NOMINAL_ctot, 
			MEASURED_kr = meta$MEASURED_kr)

		###
		indiv_tab <- bind_rows(indiv_tab, indiv_tab_i)

	}

	####
	if(type == 'original'){

		####
		pop_tab <- pop_tab[-1, ]

		write_delim(
			file = 'SAEMIX_populationLevelFit_originalTime.txt', 
			x = pop_tab, 
			delim = "\t")

		####
		indiv_tab <- indiv_tab[-1, ]

		write_delim(
			file = 'SAEMIX_individualLevelFit_originalTime.txt', 
			x = indiv_tab, 
			delim = "\t")

	}

	else if(type == 'aligned'){

		####
		pop_tab <- pop_tab[-1, ]

		write_delim(
			file = 'SAEMIX_populationLevelFit_alignedTime.txt', 
			x = pop_tab, 
			delim = "\t")

		####
		indiv_tab <- indiv_tab[-1, ]

		write_delim(
			file = 'SAEMIX_individualLevelFit_alignedTime.txt', 
			x = indiv_tab, 
			delim = "\t")
	
	}

}
