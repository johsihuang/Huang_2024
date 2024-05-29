#####
make_plot_1 <- function(imglist, type = c('original', 'aligned'), save = TRUE){

	####
	plotlist <- list()

	####
	if(type == 'original'){

		####
		for(i in 1:length(imglist)){

			####
			img_i <- imglist[[i]]

			####
			p0a <- ggplot(data = img_i, aes(x = t_min, y = SCALED_INTENSITY, group = POSITION_X))
			p0b <- p0a + geom_path(alpha = 0.1)
			p0c <- p0b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Cleaved DEVD-R110 (nM)', expand = c(0, 0), limits = c(0, 2500))
			p0d <- p0c + gtheme

			####
			p1a <- ggplot(data = img_i, aes(x = t_min, y = MODEL_INTENSITY, group = POSITION_X))
			p1b <- p1a + geom_path(alpha = 0.1)
			p1c <- p1b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Cleaved DEVD-R110 (nM)', expand = c(0, 0), limits = c(0, 2500))
			p1d <- p1c + gtheme

			####
			p2a <- ggplot(data = img_i, aes(x = t_min, y = SCALED_INTENSITY - MODEL_INTENSITY, group = POSITION_X))
			p2b <- p2a + geom_path(alpha = 0.1)
			p2c <- p2b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Residual (nM)', expand = c(0, 0), limits = c(-300, 300))
			p2d <- p2c + gtheme

			####
			p3a <- ggplot(data = img_i, aes(x = t_min, y = POSITION_X_um, fill = SCALED_INTENSITY))
			p3b <- p3a + geom_raster()
			p3c <- p3b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Distance (um)', expand = c(0, 0)) + scale_fill_gradient(name = 'Cleaved DEVD-R110 (nM)', high = 'white', low = 'black', limits = c(0, 2500))
			p3d <- p3c + gtheme
			p3e <- p3d + theme(legend.position = 'none')

			####
			p4a <- ggplot(data = img_i, aes(x = t_min, y = POSITION_X_um, fill = MODEL_INTENSITY))
			p4b <- p4a + geom_raster()
			p4c <- p4b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Distance (um)', expand = c(0, 0)) + scale_fill_gradient(name = 'Cleaved DEVD-R110 (nM)', high = 'white', low = 'black', limits = c(0, 2500))
			p4d <- p4c + gtheme 
			p4e <- p4d + theme(legend.position = 'none')

			####
			p5a <- ggplot(data = img_i, aes(x = t_min, y = POSITION_X_um, fill = SCALED_INTENSITY - MODEL_INTENSITY))
			p5b <- p5a + geom_raster()
			p5c <- p5b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Distance (um)', expand = c(0, 0)) + scale_fill_gradient2(name = 'Residual (nM)', high = 'magenta', low = 'green', mid = 'black', limits = c(-300, 300))
			p5d <- p5c + gtheme
			p5e <- p5d + theme(legend.position = 'none')

			####
			leg1 <- get_legend(p3d)
			leg2 <- get_legend(p5d)

			####
			ap1 <- plot_grid(p0d, p1d, p2d, p3e, p4e, p5e, align = 'hv', axis = 'tblr', nrow = 2, rel_heights = c(1, 2), rel_widths = 1)
			ap2 <- plot_grid(leg1, leg2, align = 'hv', axis = 'tblr', ncol = 1)

			####
			ap3 <- plot_grid(ap1, ap2, align = 'hv', axis = 'tblr', ncol = 2, rel_widths = c(6, 2))

			####
			if(save){

				####
				plot_name <- paste("originalTime", "Plot", "1", i, ".pdf", sep = "_")

				####
				ggsave(filename = plot_name, plot = ap3, width = 8, height = 3, units = "in", dpi = 600, limitsize = FALSE)

			}

			####
			plotlist[[i]] <- ap3

		}

	}

	####
	else if(type == 'aligned'){

		####
		for(i in 1:length(imglist)){

			####
			img_i <- imglist[[i]]

			####
			p0a <- ggplot(data = img_i, aes(x = ALIGNED_t_min, y = SCALED_INTENSITY, group = POSITION_X))
			p0b <- p0a + geom_path(alpha = 0.1)
			p0c <- p0b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Cleaved DEVD-R110 (nM)', expand = c(0, 0), limits = c(0, 2500))
			p0d <- p0c + gtheme

			####
			p1a <- ggplot(data = img_i, aes(x = ALIGNED_t_min, y = ALIGNED_MODEL_INTENSITY, group = POSITION_X))
			p1b <- p1a + geom_path(alpha = 0.1)
			p1c <- p1b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Cleaved DEVD-R110 (nM)', expand = c(0, 0), limits = c(0, 2500))
			p1d <- p1c + gtheme

			####
			p2a <- ggplot(data = img_i, aes(x = ALIGNED_t_min, y = SCALED_INTENSITY - ALIGNED_MODEL_INTENSITY, group = POSITION_X))
			p2b <- p2a + geom_path(alpha = 0.1)
			p2c <- p2b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Residual (nM)', expand = c(0, 0), limits = c(-300, 300))
			p2d <- p2c + gtheme

			####
			p3a <- ggplot(data = img_i, aes(x = ALIGNED_t_min, y = POSITION_X_um, fill = SCALED_INTENSITY))
			p3b <- p3a + geom_raster()
			p3c <- p3b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Distance (um)', expand = c(0, 0)) + scale_fill_gradient(name = 'Cleaved DEVD-R110 (nM)', high = 'white', low = 'black', limits = c(0, 2500))
			p3d <- p3c + gtheme
			p3e <- p3d + theme(legend.position = 'none')

			####
			p4a <- ggplot(data = img_i, aes(x = ALIGNED_t_min, y = POSITION_X_um, fill = ALIGNED_MODEL_INTENSITY))
			p4b <- p4a + geom_raster()
			p4c <- p4b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Distance (um)', expand = c(0, 0)) + scale_fill_gradient(name = 'Cleaved DEVD-R110 (nM)', high = 'white', low = 'black', limits = c(0, 2500))
			p4d <- p4c + gtheme 
			p4e <- p4d + theme(legend.position = 'none')

			####
			p5a <- ggplot(data = img_i, aes(x = ALIGNED_t_min, y = POSITION_X_um, fill = SCALED_INTENSITY - ALIGNED_MODEL_INTENSITY))
			p5b <- p5a + geom_raster()
			p5c <- p5b + scale_x_continuous(name = 'Time (min)', expand = c(0, 0)) + scale_y_continuous(name = 'Distance (um)', expand = c(0, 0)) + scale_fill_gradient2(name = 'Residual (nM)', high = 'magenta', low = 'green', mid = 'black', limits = c(-300, 300))
			p5d <- p5c + gtheme
			p5e <- p5d + theme(legend.position = 'none')

			####
			leg1 <- get_legend(p3d)
			leg2 <- get_legend(p5d)

			####
			ap1 <- plot_grid(p0d, p1d, p2d, p3e, p4e, p5e, align = 'hv', axis = 'tblr', nrow = 2, rel_heights = c(1, 2), rel_widths = 1)
			ap2 <- plot_grid(leg1, leg2, align = 'hv', axis = 'tblr', ncol = 1)

			####
			ap3 <- plot_grid(ap1, ap2, align = 'hv', axis = 'tblr', ncol = 2, rel_widths = c(6, 2))

			####
			if(save){

				####
				plot_name <- paste("alignedTime", "Plot", "1", i, ".pdf", sep = "_")

				####
				ggsave(filename = plot_name, plot = ap3, width = 8, height = 3, units = "in", dpi = 600, limitsize = FALSE)

			}

			####
			plotlist[[i]] <- ap3

		}

	}

	####
	return(plotlist)

}

#####
make_plot_2 <- function(pop_og = pop_og, pop_al = pop_al, save = TRUE){

	####
	pop_og$type <- 'original'
	pop_al$type <- 'aligned'
	pop <- bind_rows(pop_og, pop_al)

	####
	pop_w <- pivot_wider(pop, names_from = type, values_from = c(Fixed_effect_mean, Fixed_effect_sem))

	####
	p0a <- ggplot(data = pop_w, aes(x = Fixed_effect_mean_original, y = Fixed_effect_mean_aligned))
	p0b <- p0a + geom_linerange(aes(xmin = Fixed_effect_mean_original - Fixed_effect_sem_original, xmax = Fixed_effect_mean_original + Fixed_effect_sem_original)) + geom_linerange(aes(ymin = Fixed_effect_mean_aligned - Fixed_effect_sem_aligned, ymax = Fixed_effect_mean_aligned + Fixed_effect_sem_aligned)) + geom_point(shape = 1)
	p0c <- p0b + scale_x_continuous(name = 'Fitted values (original time)') + scale_y_continuous(name = 'Fitted values (aligned time)')
	p0d <- p0c + gtheme + facet_rep_wrap(. ~ Parameter, scales = 'free', repeat.tick.labels = TRUE)

	####
	if(save){

		####
		plot_name <- paste("Compare", "Plot", "2", ".pdf", sep = "_")

		####
		ggsave(filename = plot_name, plot = p0d, width = 6, height = 2, units = "in", dpi = 600, limitsize = FALSE)

	}

	####
	return(p0d)

}