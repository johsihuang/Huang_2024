#####
make_meta <- function(Extract, Cytoplasm, Status, NOMINAL_rtot){

	####
	out <- as_tibble(data.frame(
		Extract = Extract, 
		Cytoplasm = Cytoplasm, 
		Status = Status, 
		NOMINAL_rtot = NOMINAL_rtot, 
		NOMINAL_ctot = 200 * Cytoplasm, 
		MEASURED_kr = calculate_kr(Status = Status, Cytoplasm = Cytoplasm)))

	####
	return(out)
}