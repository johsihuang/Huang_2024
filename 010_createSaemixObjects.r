#####
make_saemixDataList <- function(imglist, type = c('original', 'aligned')){

	####
	outlist <- list()

	####
	if(type == 'original'){

		####
		for(i in 1:length(imglist)){

			####
			img_i <- imglist[[i]]

			####
			saemix_data_i <- saemixData(
				name.data = img_i, 
				name.group = 'POSITION_X', 
				name.predictors = c('t_min', 'NOMINAL_rtot', 'NOMINAL_ctot', 'MEASURED_r0', 'MEASURED_kr'), 
				name.X = 't_min', 
				name.response = 'SCALED_INTENSITY',
				units = list(x = 'min', y = 'nM'))
			# treat ctot, rtot, and kr not as parameters but independent variables to explain the signal value
			# assume no covariate given a single-condition dataset; otherwise consider $Cytoplasm and $Status as covariates when appropriate 

			####
			outlist[[i]] <- saemix_data_i

		}

	}

	####
	if(type == 'aligned'){

		####
		for(i in 1:length(imglist)){

			####
			img_i <- imglist[[i]]

			####
			saemix_data_i <- saemixData(
				name.data = img_i, 
				name.group = 'POSITION_X', 
				name.predictors = c('ALIGNED_t_min', 'NOMINAL_rtot', 'NOMINAL_ctot', 'ALIGNED_MEASURED_r0', 'MEASURED_kr'), 
				name.X = 'ALIGNED_t_min', 
				name.response = 'SCALED_INTENSITY',
				units = list(x = 'min', y = 'nM'))
			# treat ctot, rtot, and kr not as parameters but independent variables to explain the signal value
			# assume no covariate given a single-condition dataset; otherwise consider $Cytoplasm and $Status as covariates when appropriate 

			####
			outlist[[i]] <- saemix_data_i

		}

	}

	####
	return(outlist)

}

#####
#### Create saemix model object
### Put in numbers
INITIAL_c0 <- 10^-3
INITIAL_kc <- 10^-2.5
INITIAL_kb <- 10^-2
# numbers are general guesses informed by previous NLS fitting experiences 

###
psi0 <- matrix(
	data = c(INITIAL_c0, INITIAL_kc, INITIAL_kb), 
	ncol = 3, 
	nrow = 1,
	byrow = TRUE, 
	dimnames = list(
		NULL, 
		c('c0', 'kc', 'kb')
))

###
fixed.estim <- c(1, 1, 1) 
# esitmate = 1, fixed value = 0; follow the order in psi/psi0
# only estimate for c0, kc, and kb

###
transform.par <- c(1, 1, 1) 
# parameter distrubution: 0 = normal, 1 = log-normal... etc.
# assume all parameters to be normally distributed
# log-normal distribution restricts parameter space to positive numbers
# assuming normal distribution for small-valued risks having parameters coverging on negative numbers... 

###
#covariate.model <- NULL
# assume no covariate and thus no covariate model

###
covariance.model <- matrix(data = 0, nrow = 3, ncol = 3)
diag(covariance.model) <- 1 
# only estimate variance for c0, kc, and kb; i.e., 1s on the diagon
# assume no covariance; i.e., all 0s off diagonal

### make saemix model object 
saemix_model <- saemixModel(
	model = saemix_r110, 
	modeltype = 'structural', 
	psi0 = psi0, 
	fixed.estim = fixed.estim, 
	transform.par = transform.par, 
	covariance.model = covariance.model, 
	error.model = 'constant', 
	description = 'Fit for c0, kc, kb; assume no covariance; assume log-normal para. dist.')

#### Create saemix control object 
saemix_control <- list(
	seed = 123, 
	save = FALSE, 
	save.graphs = FALSE)
