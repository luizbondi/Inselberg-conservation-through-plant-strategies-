maxent_dtvp <- function (occ, env, taxa=''){

## where 'occ' is a data.frame containg at least 2 columns, one with longitude
	## and other with latitude information, named 'lon' and 'lat' respectively.
	## env is a RasterStack containg the environmental conditions to be 
	## used in the model. 'taxa' is the species identification reference.
	## The function will perform MaxEnt model, with 10000 background points
	## within an extent exceding in 5 degrees the extreme occurence points
	## in all directions, evaluating the predictability of the model using
	## the k-fold cross-validation technique (k=5). Species with less than 5
	## observation points with minimum distance of 5 km from each other, won't 
	## be assessed. This will return a list with (1) RasterLayer with predicted
	## distribution (labelled 'pred.distr'), (2) area under the receiver
	## operating characteristic curve (labelled 'auc'), the optimum
	## threshold for binary predicted distribution (labelled 'threshold'),
	## (4) the relative contribution of the use environmentalvariables
	## for the model (labelled 'env.contr'), and the html with MaxEnt
	## results analyses (labelled 'html').

require(dismo)
require(humboldt)
require(maptools)
require(SDMtune)
require(usdm)
## Now we are going to select only georeferenced records and then, exclude
## close occurence point in order to reduce 'sampling biases' and
## spacial autocorrelations if it is intended.
x <- occ
x_georef <- data.frame(x$lon,x$lat)
names(x_georef) <- c('lon','lat')
x_georef <- x_georef[complete.cases(x_georef[ , 1:2]),]
x_occ <- humboldt.occ.rarefy(in.pts = x_georef, colxy=1:2, rarefy.dist = 1,
rarefy.units = "km")

## the model will only be constructed if the observations are higher than 5
if(dim(x_occ)[1] < 5){print('less than 5')} else {

	## Converting the geographical coordinates from occurence localities into
	## SpatialPoints object and projecting them to the same projection of
	## worldclim Raster files
	coordinates(x_occ) <- ~lon+lat
	proj4string(x_occ) <- CRS("+proj=longlat +datum=WGS84")
	## defining extents
	ext <- extent(x_occ) + c(-5,5,-5,5)
	spatial_cut <- crop(env,ext)
	ext@xmin <- if (ext@xmin < -180) {-180} else {ext@xmin}
	ext@xmax <- if (ext@xmax > 180) {180} else {ext@xmax}
	ext@ymin <- if (ext@ymin < -90) {-90} else {ext@ymin}
	ext@ymax <- if (ext@ymax > 90) {90} else {ext@ymax}

	## split data for training and testing
	pres_group <- kfold(x_occ, k=5)

	## creating background points
	backg <- randomPoints(spatial_cut[[1]], n=10000)
	colnames(backg) = c('lon', 'lat')
	bg_group <- kfold(backg, 5) #

	## selecting relevant variables
	vif_res <- vifstep(spatial_cut) 
	pred_nac <- dropLayer(spatial_cut, vif_res@excluded)

	SWDdata <- prepareSWD(species = 'whatever',
		p = as.data.frame(x_occ),
		a = backg,	env = pred_nac)
	default_model <- train(method = "Maxent", data = SWDdata)
	VarImp <- maxentVarImp(default_model)
	sel_variables <- subset(VarImp, Percent_contribution < 5 )
	final_pred <- dropLayer(pred_nac, sel_variables$Variable)

	## Model 1
	## defining training and test groups
	pres_train1 <- x_occ[pres_group != 1,]
	pres_test1 <- x_occ[pres_group == 1,]
	backg_train1 <- backg[bg_group != 1, ]
	backg_test1 <- backg[bg_group == 1, ]

	## running Maxent 1
	model1 <- maxent(final_pred, pres_train1)         
	pred_model1 <- predict(model1, final_pred)
	auc_test1 <- evaluate(pres_test1, backg_test1, model1, final_pred)
	tr1 <- threshold(auc_test1, 'spec_sens')
	
	## variable importance model 1
	SWDdata1 <- prepareSWD(species = taxa, p = as.data.frame(pres_train1),
		a = backg_train1,	env = env)
	default_model1 <- train(method = "Maxent", data = SWDdata1)
	VarImp1 <- maxentVarImp(default_model1)

	## Model 2
	## defining training and test groups
	pres_train2 <- x_occ[pres_group != 2,]
	pres_test2 <- x_occ[pres_group == 2,]
	backg_train2 <- backg[bg_group != 2, ]
	backg_test2 <- backg[bg_group == 2, ]

	## running Maxent 2
	model2 <- maxent(final_pred, pres_train2)         
	pred_model2 <- predict(model2, final_pred)
	auc_test2 <- evaluate(pres_test2, backg_test2, model2, final_pred)
	tr2 <- threshold(auc_test2, 'spec_sens')

	## variable importance model 2
	SWDdata2 <- prepareSWD(species = taxa, p = as.data.frame(pres_train2),
		a = backg_train2,	env = env)
	default_model2 <- train(method = "Maxent", data = SWDdata2)
	VarImp2 <- maxentVarImp(default_model2)

	## Model 3
	## defining training and test groups
	pres_train3 <- x_occ[pres_group != 3,]
	pres_test3 <- x_occ[pres_group == 3,]
	backg_train3 <- backg[bg_group != 3, ]
	backg_test3 <- backg[bg_group == 3, ]

	## running Maxent 3
	model3 <- maxent(final_pred, pres_train3)         
	pred_model3 <- predict(model3, final_pred)
	auc_test3 <- evaluate(pres_test3, backg_test3, model3, final_pred)
	tr3 <- threshold(auc_test3, 'spec_sens')

	## variable importance model 3
	SWDdata3 <- prepareSWD(species = taxa, p = as.data.frame(pres_train3),
		a = backg_train3,	env = env)
	default_model3 <- train(method = "Maxent", data = SWDdata3)
	VarImp3 <- maxentVarImp(default_model3)

	## Model 4
	## defining training and test groups
	pres_train4 <- x_occ[pres_group != 4,]
	pres_test4 <- x_occ[pres_group == 4,]
	backg_train4 <- backg[bg_group != 4, ]
	backg_test4 <- backg[bg_group == 4, ]

	## running Maxent 4
	model4 <- maxent(final_pred, pres_train4)         
	pred_model4 <- predict(model4, final_pred)
	auc_test4 <- evaluate(pres_test4, backg_test4, model4, final_pred)
	tr4 <- threshold(auc_test4, 'spec_sens')

	## variable importance model 4
	SWDdata4 <- prepareSWD(species = taxa, p = as.data.frame(pres_train4),
		a = backg_train4,	env = env)
	default_model4 <- train(method = "Maxent", data = SWDdata4)
	VarImp4 <- maxentVarImp(default_model4)

	## Model 5
	## defining training and test groups
	pres_train5 <- x_occ[pres_group != 5,]
	pres_test5 <- x_occ[pres_group == 5,]
	backg_train5 <- backg[bg_group != 5, ]
	backg_test5 <- backg[bg_group == 5, ]

	## running Maxent 5
	model5 <- maxent(final_pred, pres_train5)         
	pred_model5 <- predict(model5, final_pred)
	auc_test5 <- evaluate(pres_test5, backg_test5, model5, final_pred)
	tr5 <- threshold(auc_test5, 'spec_sens')

	## variable importance model 5
	SWDdata5 <- prepareSWD(species = taxa, p = as.data.frame(pres_train5),
		a = backg_train5,	env = env)
	default_model5 <- train(method = "Maxent", data = SWDdata5)
	VarImp5 <- maxentVarImp(default_model5)

	pred_modelnull <- pred_model1 * 0

	pred_model <- sum(
		if (auc_test1@auc > 0.7) {pred_model1} else {pred_modelnull},
		if (auc_test2@auc > 0.7) {pred_model2} else {pred_modelnull},
		if (auc_test3@auc > 0.7) {pred_model3} else {pred_modelnull},
		if (auc_test4@auc > 0.7) {pred_model4} else {pred_modelnull},
		if (auc_test5@auc > 0.7) {pred_model5} else {pred_modelnull})/
		sum(
		if (auc_test1@auc > 0.7) {1} else {0},
		if (auc_test2@auc > 0.7) {1} else {0},
		if (auc_test3@auc > 0.7) {1} else {0},
		if (auc_test4@auc > 0.7) {1} else {0},
		if (auc_test5@auc > 0.7) {1} else {0})

	pred_model1 <- pred_model1 > tr1
	pred_model2 <- pred_model2 > tr2
	pred_model3 <- pred_model3 > tr3
	pred_model4 <- pred_model4 > tr4
	pred_model5 <- pred_model5 > tr5

	pred_model_tr <- sum(
		if (auc_test1@auc > 0.7) {pred_model1} else {pred_modelnull},
		if (auc_test2@auc > 0.7) {pred_model2} else {pred_modelnull},
		if (auc_test3@auc > 0.7) {pred_model3} else {pred_modelnull},
		if (auc_test4@auc > 0.7) {pred_model4} else {pred_modelnull},
		if (auc_test5@auc > 0.7) {pred_model5} else {pred_modelnull}) > 
		(sum(
		if (auc_test1@auc > 0.7) {1} else {0},
		if (auc_test2@auc > 0.7) {1} else {0},
		if (auc_test3@auc > 0.7) {1} else {0},
		if (auc_test4@auc > 0.7) {1} else {0},
		if (auc_test5@auc > 0.7) {1} else {0}) * 0.5)

	VarImp <- rbind(VarImp1, VarImp2, VarImp3, VarImp4, VarImp5)
	VarImp_mean <- aggregate(VarImp, by=list(VarImp$Variable), FUN = "mean")
	VarImp_mean <- VarImp_mean[,-2]

	## export the resulting raster file	with model means
	writeRaster(pred_model, paste(format(Sys.Date(), format= "%Y%m%d"), '_',
		 taxa, "_MaxEnt_raster.tif", sep=''))

	## export the resulting raster file	with average of models cut by the 
		#respective thresholds
	writeRaster(pred_model_tr, paste(format(Sys.Date(), format= "%Y%m%d"), '_',
		 taxa, "_MaxEnt_threshold_raster.tif", sep=''))

	tiff(paste(format(Sys.Date(), format= "%Y%m%d"), '_',taxa,
		"_MaxEnt","_map.tiff", sep=''),width = 1000, height = 1000, 
		units = "px", type = "cairo", compression = "lzw")
		data(wrld_simpl)
		r <- raster(final_pred, 1)
		plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE,
			main= paste(taxa, "AUC = ", round(sum(
		if (auc_test1@auc > 0.7) {auc_test1@auc} else {0},
		if (auc_test2@auc > 0.7) {auc_test2@auc} else {0},
		if (auc_test3@auc > 0.7) {auc_test3@auc} else {0},
		if (auc_test4@auc > 0.7) {auc_test4@auc} else {0},
		if (auc_test5@auc > 0.7) {auc_test5@auc} else {0})/
		sum(
		if (auc_test1@auc > 0.7) {1} else {0},
		if (auc_test2@auc > 0.7) {1} else {0},
		if (auc_test3@auc > 0.7) {1} else {0},
		if (auc_test4@auc > 0.7) {1} else {0},
		if (auc_test5@auc > 0.7) {1} else {0}), 3), "models =",
		sum(
		if (auc_test1@auc > 0.7) {1} else {0},
		if (auc_test2@auc > 0.7) {1} else {0},
		if (auc_test3@auc > 0.7) {1} else {0},
		if (auc_test4@auc > 0.7) {1} else {0},
		if (auc_test5@auc > 0.7) {1} else {0})))
		plot(pred_model_tr, add=TRUE)
		plot(wrld_simpl, border='dark grey', add=TRUE)
	dev.off()

	write.table(data.frame(models_AUC = c(auc_test1@auc,
		auc_test2@auc, auc_test3@auc, auc_test4@auc, auc_test5@auc)),
		paste(format(Sys.Date(), format= "%Y%m%d"),
		'_',taxa,'_MaxEnt_auc.txt',sep='')) 
	write.table(data.frame(models_threshold = c(tr1, tr2, tr3, tr4, tr5)),
		paste(format(Sys.Date(), format= "%Y%m%d"), 
		'_',taxa,'_MaxEnt_threshold.txt',sep=''))
	write.table(VarImp_mean, 
		paste(format(Sys.Date(), format= "%Y%m%d"),
		 '_',taxa,'_MaxEnt_env.contr.txt',sep=''))
		
		print(paste(taxa,'AUC:',round(sum(
		if (auc_test1@auc > 0.7) {auc_test1@auc} else {0},
		if (auc_test2@auc > 0.7) {auc_test2@auc} else {0},
		if (auc_test3@auc > 0.7) {auc_test3@auc} else {0},
		if (auc_test4@auc > 0.7) {auc_test4@auc} else {0},
		if (auc_test5@auc > 0.7) {auc_test5@auc} else {0})/
		sum(
		if (auc_test1@auc > 0.7) {1} else {0},
		if (auc_test2@auc > 0.7) {1} else {0},
		if (auc_test3@auc > 0.7) {1} else {0},
		if (auc_test4@auc > 0.7) {1} else {0},
		if (auc_test5@auc > 0.7) {1} else {0}), 3)))
}}


setwd("C:/Users/Administrator/Desktop/Distribution/Occurrence_PDA")
ctl149 <- read.csv("ctl149.finalissima.csv", header = T)
setwd("C:/Users/Administrator/Desktop/Distribution/Env_variables")

envirem <- stack(
"current_30arcsec_annualPET.tif",
"current_30arcsec_aridityIndexThornthwaite.tif",
"current_30arcsec_climaticMoistureIndex.tif",
"current_30arcsec_PETColdestQuarter.tif",
"current_30arcsec_PETDriestQuarter.tif",
"current_30arcsec_PETseasonality.tif",
"current_30arcsec_PETWarmestQuarter.tif",
"current_30arcsec_PETWettestQuarter.tif"
)

## Environmental conditions
setwd("H:/wc2.1_30s_bio")
worldclim <- stack(
"wc2.1_30s_bio_1.tif",
"wc2.1_30s_bio_2.tif",
"wc2.1_30s_bio_3.tif",
"wc2.1_30s_bio_4.tif",
"wc2.1_30s_bio_5.tif",
"wc2.1_30s_bio_6.tif",
"wc2.1_30s_bio_7.tif",
"wc2.1_30s_bio_8.tif",
"wc2.1_30s_bio_9.tif",
"wc2.1_30s_bio_10.tif",
"wc2.1_30s_bio_11.tif",
"wc2.1_30s_bio_12.tif",
"wc2.1_30s_bio_13.tif",
"wc2.1_30s_bio_14.tif",
"wc2.1_30s_bio_15.tif",
"wc2.1_30s_bio_16.tif",
"wc2.1_30s_bio_17.tif",
"wc2.1_30s_bio_18.tif",
"wc2.1_30s_bio_19.tif"
)


worldclim <- crop(worldclim, envirem)

env <- stack(worldclim, envirem)


writeRaster(env, 'env.tif')


maxent_dtvp(occ =	aaa9	, env = env, taxa = 'aaa9')

