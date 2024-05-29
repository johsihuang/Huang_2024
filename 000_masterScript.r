##### Set up environment
####
setwd('~/Desktop/Wave in tube/')

####
library('tidyverse')
library('mixtools')
library('viridis')
library('cowplot')
library('EBImage')
library('saemix')
library('lemon')

####
source('001_makeMetaData.r')
source('002_readImage.r')
source('003_krCalculator.r')
source('004_scaleImage.r')
source('005_alignTraces.r')
source('006_markAlignedTime.r')
source('007_findR0.r')
source('008_selectTimeWindow.r')
source('009_saemixCaspaseSensorModel.r')
source('010_createSaemixObjects.r')
source('011_runSaemix.r')
source('012_saveSaemixResults.r')
source('013_simulateSkeleton.r')
source('014_saveFittedTimeSeries.r')
source('015_makePlots.r')

####
meta <- make_meta(
	Extract = 'Interphase #1', 
	Cytoplasm = 1, 
	Status = 'Original / diluted', 
	NOMINAL_rtot = 2000)

####
gtheme <- theme(
	axis.line = element_line(color = "black"),
	axis.ticks = element_line(color = "black"),
	axis.text = element_text(color = "black", size = 6),
	axis.title = element_text(color = "black", size = 6),
	legend.key = element_rect(fill = "white"),
	legend.text = element_text(color = "black", size = 6),
	legend.title = element_text(color = "black", size = 6),
	panel.grid.major = element_line(color = NA),
	panel.grid.minor = element_line(color = NA),
	panel.background = element_rect(fill = "white"),
	panel.border = element_rect(fill = NA, color = NA),
	plot.title = element_text(color = "black", size = 6),
	strip.background = element_rect(colour = "black", fill = "white"),
	strip.text.x = element_text(color = "black", size = 6),
	strip.text.y = element_text(color = "black", size = 6))

####
###
setwd('~/Desktop/Wave in tube/TubeRiseTimeVignetteData/')
imglist <- read_img(meta = meta)

###
setwd('~/Desktop/Wave in tube/')

##### Process images
#### 
imglist <- scale_intensity(imglist = imglist)

####
imglist <- align_traces(imglist = imglist)

####
imglist <- mark_alignedTime(imglist = imglist, start_time = -30)

####
imglist <- find_r0(imglist = imglist)

##### Prepare for and run SAEMIX
####
img_og <- select_timeWindow(imglist = imglist, duration = 60, type = 'original')
img_al <- select_timeWindow(imglist = imglist, duration = 60, type = 'aligned')

####
saemix_data_og <- make_saemixDataList(imglist = img_og, type = 'original')
saemix_data_al <- make_saemixDataList(imglist = img_al, type = 'aligned')

####
saemix_out_og <- run_saemix(saemix_data = saemix_data_og, saemix_model = saemix_model, saemix_control = saemix_control)
saemix_out_al <- run_saemix(saemix_data = saemix_data_al, saemix_model = saemix_model, saemix_control = saemix_control)

#### Save fitting output
save_saemixOut(saemix_out = saemix_out_og, image_names = names(img_og), meta = meta, type = 'original')
save_saemixOut(saemix_out = saemix_out_al, image_names = names(img_al), meta = meta, type = 'aligned')

#### Read back
###
indiv_og <- read_delim('SAEMIX_individualLevelFit_originalTime.txt') 
indiv_al <- read_delim('SAEMIX_individualLevelFit_alignedTime.txt')

###
pop_og <- read_delim('SAEMIX_populationLevelFit_originalTime.txt') 
pop_al <- read_delim('SAEMIX_populationLevelFit_alignedTime.txt')

##### Realize individual fits for comparison
####
img_og <- simulate_skeleton(data = img_og, params = indiv_og, type = 'original')
img_al <- simulate_skeleton(data = img_al, params = indiv_al, type = 'aligned')

####
save_fittedTimeSeries(data = img_og, type = 'original')
save_fittedTimeSeries(data = img_al, type = 'aligned')

##### Plot to see results!
p1_og <- make_plot_1(imglist = img_og, type = 'original', save = TRUE)
p1_al <- make_plot_1(imglist = img_al, type = 'aligned', save = TRUE)
p2 <- make_plot_2(pop_og = pop_og, pop_al = pop_al, save = TRUE)

