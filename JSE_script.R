setwd("path")
traits <- read.csv('20230214_suplementary_information.csv', header=TRUE)

#creating colors
library(RColorBrewer)
cor.ds <- c(brewer.pal(n=9,"RdBu")[7], brewer.pal(n=9,"PRGn")[2], brewer.pal(n=9,"Set1")[1])
cor.gf <- c(brewer.pal(n=9,"YlGnBu")[4], brewer.pal(n=9,"RdPu")[5],
	brewer.pal(n=9,"OrRd")[6], brewer.pal(n=9,"Greens")[7],
	brewer.pal(n=9,"Blues")[8], brewer.pal(n=9,"BuPu")[9])

#creating legend
library(ggplot2)
library(ggpubr)
library(grid)
library(RColorBrewer)
	g_legend<-function(x){
	require(ggplot2)
	  tmp <- ggplot_gtable(ggplot_build(x))
	  leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box")
	  legend <- tmp$grobs[[leg]]
	  legend
	}

paired.wanova<- function(x=x, y=y, data=data){
df <- data.frame(data[, c(x,y)])
names(df)<- c("x1","x2")
allPairs <- expand.grid(levels(df$x2), levels(df$x2))
## http://stackoverflow.com/questions/28574006/unique-combination-of-two-columns-in-r/28574136#28574136
allPairs <- unique(t(apply(allPairs, 1, sort)))
allPairs <- allPairs[ allPairs[,1] != allPairs[,2], ]
allPairs

allResults <- apply(allPairs, 1, function(p) {
    dat <- df[ df$x2 %in% p, ]
    ret <- oneway.test(x1 ~ x2, data = dat, na.action = na.omit, var.equal = FALSE)
    ret$groups <- p
    ret
})

length(allResults)
allResults[[2]]

mm <- diag(length(levels(df$x2)))
dimnames(mm) <- list(levels(df$x2), levels(df$x2))
pMatrix <- lapply(allResults, function(res) {
    ## not fond of out-of-scope assignment ...
    mm[res$groups[1], res$groups[2]] <<- mm[res$groups[2], res$groups[1]] <<- res$p.value
})

return(round(mm,7))
}

paired.fstatistic<- function(x=x, y=y, data=data){
df <- data.frame(data[, c(x,y)])
names(df)<- c("x1","x2")
allPairs <- expand.grid(levels(df$x2), levels(df$x2))
## http://stackoverflow.com/questions/28574006/unique-combination-of-two-columns-in-r/28574136#28574136
allPairs <- unique(t(apply(allPairs, 1, sort)))
allPairs <- allPairs[ allPairs[,1] != allPairs[,2], ]
allPairs

allResults <- apply(allPairs, 1, function(p) {
    dat <- df[ df$x2 %in% p, ]
    ret <- oneway.test(x1 ~ x2, data = dat, na.action = na.omit, var.equal = FALSE)
    ret$groups <- p
    ret
})

length(allResults)
allResults[[2]]

mm <- diag(length(levels(df$x2)))
dimnames(mm) <- list(levels(df$x2), levels(df$x2))
pMatrix <- lapply(allResults, function(res) {
    ## not fond of out-of-scope assignment ...
    mm[res$groups[1], res$groups[2]] <<- mm[res$groups[2], res$groups[1]] <<- res$statistic
})

return(round(mm,4))
}


############################################
###########Descriptive analyses#############
############################################
	str(traits)
	#'data.frame':   220 obs. of  18 variables:
	# $ SIGLA             : chr  "aaa9" "aac10" "aac5" "aag108" ...
	# $ FAMILY            : chr  "Arecaceae" "Aristolochiaceae" "Araceae" "Apocynaceae" ...
	# $ SPECIES           : chr  "Astrocaryum aculeatissimum (Schott) Burret" "Aristolochia cymbifera Mart. & Zucc." "Anthurium coriaceum G. Don" "Aspidosperma gomezianum A.DC." ...
	# $ Dispersal.syndrome: chr  "Zoochory" "Anemochory" "Zoochory" "Anemochory" ...
	# $ Growth.form       : chr  "Semi-woody" "Climber" "Epiphyte" "Woody" ...
	# $ RS1               : num  -0.515 -1.034 -0.138 -0.461 -0.637 ...
	# $ RS2               : num  0.237 -0.122 -0.617 -1.844 -0.706 ...
	# $ inertia           : num  0.744 0.588 0.549 1.437 0.575 ...
	# $ OMI               : num  0.0808 0.2896 0.0724 0.6578 0.1968 ...
	# $ Tol               : num  0.347 0.137 0.182 0.623 0.166 ...
	# $ Rtol              : num  0.316 0.162 0.295 0.156 0.212 ...
	# $ omi               : num  10.9 49.2 13.2 45.8 34.2 25.7 84.7 16.3 38.4 96.5 ...
	# $ tol               : num  46.7 23.3 33.1 43.4 28.9 38.9 4.3 20.8 38.1 2.1 ...
	# $ rtol              : num  42.4 27.5 53.7 10.8 36.9 35.3 11 62.9 23.5 1.4 ...
	# $ DL                : num  41.4 69.68 14.65 1.4 5.33 ...
	# $ AREA              : num  104063 308255 47505 4060 5564 ...
	# $ EN                : int  0 0 0 1 0 0 1 0 0 0 ...
	# $ VUL               : num  1.74 2.01 1.78 2.38 2.05 ...

	traits$SIGLA <- as.factor(traits$SIGLA)
	traits$FAMILY <- as.factor(traits$FAMILY)
	traits$SPECIES <- as.factor(traits$SPECIES)
	traits$Dispersal.syndrome <- as.factor(traits$Dispersal.syndrome)
	traits$Growth.form <- as.factor(traits$Growth.form)

	nlevels(traits$SIGLA) # 220 sps
	nlevels(traits$FAMILY) # 53 families
	gen <- data.frame(do.call('rbind', strsplit(as.character(traits$SPECIES)," ",fixed=TRUE)))
	nlevels(gen[,1]) #142 genera

	traits$Dispersal.syndrome <- factor(traits$Dispersal.syndrome, levels = c("Autochory", "Anemochory", "Zoochory"))
	table(traits$Dispersal.syndrome)
	# Autochory Anemochory  Zoochory 
	#    12     92    116 
	(chisqds<- chisq.test(table(traits$Dispersal.syndrome)))
	#	Chi-squared test for given probabilities
	#data: table(traits$Dispersal.syndrome)
	#X-squared = 80.873, df = 2, p-value < 0.00000000000000022
	#null hypothesis of equal probability among levels. This hyphothesis was rejected.
	chisqds$expected
	# Autochory Anemochory  Zoochory 
	# 73.33333  73.33333  73.33333 

	traits$Growth.form <- factor(traits$Growth.form, levels = c("Lithophyte", "Semi-woody", "Herbaceous", "Climber", "Epiphyte", "Woody"))
	table(traits$Growth.form)
	#Lithophyte Semi-woody Herbaceous  Climber  Epiphyte   Woody 
	#    19     8     24     61     38     70
	(chisqgf<- chisq.test(table(traits$Growth.form)))
	#	Chi-squared test for given probabilities
	#data: table(traits$Growth.form)
	#X-squared = 81.8, df = 5, p-value = 0.0000000000000003525
	#null hypothesis of equal probability among levels. This hyphothesis was rejected.
	chisqgf$expected
	#Lithophyte Semi-woody Herbaceous  Climber  Epiphyte   Woody 
	# 36.66667  36.66667  36.66667  36.66667  36.66667  36.66667

	for (fcrescimento in c("Herbaceous", "Semi-woody", "Woody", "Climber", "Epiphyte", "Lithophyte")){
	print(fcrescimento)
	print(sort(table(subset(traits, Growth.form == fcrescimento)$FAMILY), decreasing = TRUE))
	}
	#[1] "Herbaceous"
	#
	#   ORCHIDACEAE   BROMELIACEAE    CYPERACEAE    ASTERACEAE 
	#        8        4        3        2 
	#   BEGONIACEAE     MORACEAE   COMELINACEAE  EUPHORBIACEAE 
	#        2        2        1        1 
	# PHYLLANTHACEAE 
	#        1
	#[1] "Semi-woody"
	#
	#    CACTACEAE    ARECACEAE     POACEAE 
	#        4        2        2 
	#[1] "Woody"
	#
	#  EUPHORBIACEAE    MYRTACEAE    MALVACEAE     MORACEAE 
	#        9        8        5        4 
	#    FABACEAE MELASTOMATACEAE    PIPERACEAE    SOLANACEAE 
	#        3        3        3        3
	#[1] "Epiphyte"
	#
	#   ORCHIDACEAE   BROMELIACEAE     ARACEAE    CACTACEAE 
	#        8        7        5        5 
	#    MORACEAE    PIPERACEAE    CLUSIACEAE    SOLANACEAE 
	#        5        4        2        1 
	#   URTICACEAE 
	#        1 
	#[1] "Climber"
	#
	#  DIOSCOREACEAE  MALPIGHIACEAE     FABACEAE  PASSIFLORACEAE 
	#        7        7        6        6 
	#  EUPHORBIACEAE   APOCYNACEAE   BIGNONIACEAE   SAPINDACEAE 
	#        4        3        3        3 
	#   SMILACACEAE ARISTOLOCHIACEAE    ASTERACEAE   BORAGINACEAE 
	#        3        2        2        2 
	#    CACTACEAE  CONVOLVULACEAE 
	#        2        2 
	#[1] "Lithophyte"
	#
	#  BROMELIACEAE    CACTACEAE   GESNERIACEAE   ORCHIDACEAE 
	#        9        2        2        2 
	#  VELLOZIACEAE   APOCYNACEAE    CYPERACEAE 
	#        2        1        1 
	for (sdispersao in c("Autochory", "Anemochory", "Zoochory")){
	print(sdispersao)
	print(sort(table(subset(traits, Dispersal.syndrome == sdispersao)$FAMILY), decreasing = TRUE))
	}
	#[1] "Autochory"
	#
	#  EUPHORBIACEAE     FABACEAE     RUTACEAE    MALVACEAE 
	#        5        3        2        1 
	# PHYLLANTHACEAE  
	#        1 
	#[1] "Anemochory"
	#
	#   ORCHIDACEAE   BROMELIACEAE  DIOSCOREACEAE   APOCYNACEAE 
	#       18        14        7        6 
	#   ASTERACEAE  MALPIGHIACEAE   BIGNONIACEAE    MALVACEAE 
	#        6        6        4        4
	#[1] "Zoochory"
	#
	#    CACTACEAE     MORACEAE  EUPHORBIACEAE    MYRTACEAE 
	#       15        11        9        8 
	#   PIPERACEAE     ARACEAE   BROMELIACEAE  PASSIFLORACEAE 
	#        7        6        6        6

	for (fcrescimento in c("Herbaceous", "Semi-woody", "Woody", "Climber", "Epiphyte", "Lithophyte")){
	print(fcrescimento)
	print(table(subset(traits, Growth.form == fcrescimento)$Dispersal.syndrome))
	}
	#[1] "Herbaceous"
	# Autochory Anemochory  Zoochory 
	#     1     15     8 
	#[1] "Semi-woody"
	# Autochory Anemochory  Zoochory 
	#     0     2     6 
	#[1] "Woody"
	# Autochory Anemochory  Zoochory 
	#     6     14     50 
	#[1] "Epiphyte"
	# Autochory Anemochory  Zoochory 
	#     0     12     26 
	#[1] "Climber"
	# Autochory Anemochory  Zoochory 
	#     5     32     24 
	#[1] "Lithophyte"
	# Autochory Anemochory  Zoochory 
	#     0     17     2 
	for (sdispersao in c("Autochory", "Anemochory", "Zoochory")){
	print(sdispersao)
	print(table(subset(traits, Dispersal.syndrome == sdispersao)$Growth.form))
	}
	#[1] "Autochory"
	#Herbaceous Semi-woody   Woody  Epiphyte  Climber Lithophyte 
	#     1     0     6     0     5     0 
	#[1] "Anemochory"
	#Herbaceous Semi-woody   Woody  Epiphyte  Climber Lithophyte 
	#    15     2     14     12     32     17 
	#[1] "Zoochory"
	#Herbaceous Semi-woody   Woody  Epiphyte  Climber Lithophyte 
	#     8     6     50     26     24     2

	summary(traits$Max_PHeight) 
	#  Min. 1st Qu. Median  Mean 3rd Qu.  Max.  NA's 
	# 0.030  1.215  4.000  6.395 10.000 30.000   16
	sd(na.omit(traits$Max_PHeight))
	summary(traits$Circular.area)
	#  Min. 1st Qu. Median  Mean 3rd Qu.  Max. 
	#  1.00  11.67  37.13  71.18  74.54 676.50

#Long-distance capacity
	summary(traits$DL)
	#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
	#   1.40   12.19   41.40   97.45   99.93  983.13       1
 	
 	(nbxds <- data.frame("Mean"=with(traits, tapply(DL, Dispersal.syndrome, mean, na.rm=TRUE)),
		"SD" = with(traits, tapply(DL, Dispersal.syndrome, sd, na.rm=TRUE))))
	#                Mean        SD
	#Autochory   70.17967  84.65373
	#Anemochory  71.47954  97.06489
	#Zoochory   120.64966 193.59539

	(nbxgf <- data.frame("Mean"=with(traits, tapply(DL, Growth.form, mean, na.rm=TRUE)),
		"SD" = with(traits, tapply(DL, Growth.form, sd, na.rm=TRUE))))
	#                Mean        SD
	#Lithophyte  10.25522  16.15098
	#Semi-woody  50.98675  48.68330
	#Herbaceous  66.87301  82.41771
	#Climber     83.63009 112.49371
	#Epiphyte    98.52812 144.65110
	#Woody      148.39262 218.75878
 	
	nb.low <- traits[order(traits$DL),]
	for (menor in (1:10)){
	print(paste(nb.low[menor,3]," (",nb.low[menor,15],")", sep=""))
	}
	#[1] "Aspidosperma gomezianum A.DC. (1.4)"
	#[1] "Tillandsia brachyphylla Baker (1.5)"
	#[1] "Stigmatodon brassicoides (Baker) Leme, G.K.Br. & Barfuss (1.5)"
	#[1] "Cattleya lobata Lindl. (1.5)"
	#[1] "Epidendrum ammophilum Barb.Rodr. (1.5)"
	#[1] "Cryptanthus bromelioides Otto & A.Dietr. (1.666666667)"
	#[1] "Rhipsalis cereoides (Backeb. & Voll) Backeb. (2)"
	#[1] "Trigonia rotundifolia Lleras (2)"
	#[1] "Pitcairnia albiflos Herb. (2.333333333)"
	#[1] "Vriesea botafogensis Mez (2.333333333)"
	nb.top <- traits[order(-traits$DL),]
	for (maior in (1:10)){
	print(paste(nb.top[maior,3]," (",nb.top[maior,15],")", sep=""))
	}
	#[1] "Piper hispidum Sw. (983.1281796)"
	#[1] "Piper amalago L. (789.3404389)"
	#[1] "Guarea guidonia (L.) Sleumer (787.2595797)"
	#[1] "Randia armata (Sw.) DC. (761.1662338)"
	#[1] "Ficus pertusa L.f. (717.5582656)"
	#[1] "Myrciaria floribunda (H.West ex Willd.) O.Berg (653.7632167)"
	#[1] "Guapira opposita (Vell.) Reitz (588.7230246)"
	#[1] "Chamissoa altissima (Jacq.) Kunth (579.3039384)"
	#[1] "Dalbergia frutescens (Vell.) Britton (474.4598985)"
	#[1] "Schinus terebinthifolius Raddi (453.1256713)"

#Niche breadth
	summary(traits$Tol)
	#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 	#0.0000  0.1576  0.3317  0.3788  0.5418  2.5290
 	
 	(nbxds <- data.frame("Mean"=with(traits, tapply(Tol, Dispersal.syndrome, mean)),
		"SD" = with(traits, tapply(Tol, Dispersal.syndrome, sd))))
	#               Mean        SD
	#Autochory  0.3984680 0.2079936
	#Anemochory 0.3568685 0.3520462
	#Zoochory   0.3941376 0.2882332

	(nbxgf <- data.frame("Mean"=with(traits, tapply(Tol, Growth.form, mean)),
		"SD" = with(traits, tapply(Tol, Growth.form, sd))))
	#                Mean        SD
	#Lithophyte 0.0763403 0.1284746
	#Semi-woody 0.2843317 0.2330481
	#Herbaceous 0.4443906 0.5110263
	#Climber    0.4224983 0.2856606
	#Epiphyte   0.3580505 0.2709766
	#Woody      0.4223524 0.2667362
 	
	nb.low <- traits[order(traits$Tol),]
	for (menor in (1:10)){
	print(paste(nb.low[menor,3]," (",nb.low[menor,16],")", sep=""))
	}
	#[1] "Epidendrum harrisoniae Hook. (0)"
	#[1] "Stigmatodon brassicoides (Baker) Leme, G.K.Br. & Barfuss (0)"
	#[1] "Prescottia spiranthophylla Barb.Rodr. (0.000280657454)"
	#[1] "Pitcairnia albiflos Herb. (0.0007839720691)"
	#[1] "Vriesea botafogensis Mez (0.00088229979)"
	#[1] "Rhipsalis cereoides (Backeb. & Voll) Backeb. (0.001308629846)"
	#[1] "Dioscorea pseudomacrocapsa G.M.Barroso et al. (0.001577723524)"
	#[1] "Alcantarea glaziouana (Leme) J.R.Grant (0.002370908352)"
	#[1] "Brasilaelia lobata Lindl. (0.004725449445)"
	#[1] "Aristolochia raja Mart. (0.006677443053)"
	nb.top <- traits[order(-traits$Tol),]
	for (maior in (1:10)){
	print(paste(nb.top[maior,3]," (",nb.top[maior,16],")", sep=""))
	}
	#[1] "Bulbostylis capillaris Clarke (2.528634797)"
	#[1] "Ficus pertusa L.f. (1.220910515)"
	#[1] "Mascagnia sepium (A.Juss.) Griseb. (1.209478063)"
	#[1] "Dioscorea campestris Griseb. (1.155697788)"
	#[1] "Peperomia pereskiaefolia (Jacq.) Kunth (1.155144486)"
	#[1] "Randia armata (Sw.) DC. (1.151834339)"
	#[1] "Chamissoa altissima (Jacq.) Kunth (1.081011439)"
	#[1] "Euphorbia insulana Vell. (1.064677253)"
	#[1] "Tragia volubilis L. (1.030817735)"
	#[1] "Piper amalago L. (1.012106914)"
	
	nbxN<-hist(traits$Tol, breaks =75)
	str(nbxN)
	H2<- data.frame(traits$Tol)
	names(H2)<- "NB"
	H2$NB<- H2[order(H2$NB),]
	Hg2<-data.frame(nbxN$counts,nbxN$mids)
	names(Hg2) <- c("freq","am")

	ggplot(Hg2, aes(am,freq)) + 
		geom_point((aes(size = -am, colour = log(freq)))) +
		theme_bw()+ 
		scale_colour_gradient(na.value = "royalblue4", low="royalblue3",high="red3")+
	 	labs (x="Niche breadth", y="Count of species") + 
		theme(legend.position="none", axis.title=element_text(size=11,face="bold"))

## Geographical range
	ca.low <- traits[order(traits$AREA),]
	for (menor in (1:10)){
	print(paste(ca.low[menor,3]," (",ca.low[menor,16],")", sep=""))
	}
	#[1] "Alcantarea glaziouana (Leme) J.R.Grant (315.0360528)"
	#[1] "Dioscorea pseudomacrocapsa G.M.Barroso et al. (315.0818697)"
	#[1] "Cryptanthus acaulis (Lindl.) Beer (658.7200346)"
	#[1] "Rhipsalis mesembryanthemoides Haw. (740.6924446)"
	#[1] "Anthurium microphyllum (Raf.) G.Don (835.0113118)"
	#[1] "Passiflora ovalis Vell. ex M.Roem. (875.3317694)"
	#[1] "Alcantarea regina (Vell.) Harms (984.2121751)"
	#[1] "Pitcairnia staminea Lodd. (1957.410458)"
	#[1] "Tillandsia araujei Mez (2015.255799)"
	#[1] "Plinia ilhensis G.M.Barroso (2184.344988)"

	ca.top <- traits[order(-traits$AREA),]
	for (maior in (1:10)){
	print(paste(ca.top[maior,3]," (",ca.top[maior,16],")", sep=""))
	}
	#[1] "Guarea guidonia (L.) Sleumer (2607310.909)"
	#[1] "Ficus pertusa L.f. (2574022.417)"
	#[1] "Randia armata (Sw.) DC. (2466446.289)"
	#[1] "Piper hispidum Sw. (2451927.425)"
	#[1] "Piper amalago L. (2322126.643)"
	#[1] "Brosimum guianense (Aubl.) Huber (1657153.585)"
	#[1] "Cordia trichotoma (Vell.) Arrb. ex Steud. (1648609.002)"
	#[1] "Chamissoa altissima (Jacq.) Kunth (1608469.921)"
	#[1] "Myrciaria floribunda (H.West ex Willd.) O.Berg (1508896.002)"
	#[1] "Bulbostylis capillaris Clarke (1447504.745)"

	ACxN<-hist(traits$AREA, breaks = 75)
	str(ACxN)
	H<- data.frame(traits$AREA)
	names(H)<- "AC"
	H$AC<- H[order(H$AC),]
	Hg<-data.frame(ACxN$counts,ACxN$mids)
	names(Hg) <- c("freq","am")
	
	ggplot(Hg, aes(am,freq)) + 
		geom_point((aes(size = -am, colour = log(freq)))) +
		theme_bw()+ 
		scale_colour_gradient(na.value = "royalblue4", low="royalblue3",high="red3")+
 		labs (x="Geographic range", y="Count of species") + 
		theme(legend.position="none", axis.title=element_text(size=11,face="bold"))

	(caxds <- data.frame("Mean"=with(traits, tapply(AREA, Dispersal.syndrome, mean)),
		"SD" = with(traits, tapply(AREA, Dispersal.syndrome, sd))))
	#               Mean       SD
	#Autochory  307581.8 364701.2
	#Anemochory 266983.0 341517.9
	#Zoochory   371860.4 575026.4
	
	(caxgf <- data.frame("Mean"=with(traits, tapply(AREA, Growth.form, mean)),
		"SD" = with(traits, tapply(AREA, Growth.form, sd))))
	#                Mean        SD
	#Lithophyte  30849.24  63429.23
	#Semi-woody 197772.51 237071.48
	#Herbaceous 249872.36 315379.04
	#Climber    318496.19 382392.84
	#Epiphyte   319240.79 473965.72
	#Woody      452350.75 634553.43

#Vulnerability to climate changes
	summary(traits$VUL)
	#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
	#  1.087   1.439   1.613   1.743   1.840   3.763       1
 	
 	(nbxds <- data.frame("Mean"=with(traits, tapply(VUL, Dispersal.syndrome, mean, na.rm=TRUE)),
		"SD" = with(traits, tapply(VUL, Dispersal.syndrome, sd, na.rm=TRUE))))
	#               Mean         SD
	#Autochory  1.507473 0.09970765
	#Anemochory 1.751257 0.47235866
	#Zoochory   1.760489 0.51502354

	(nbxgf <- data.frame("Mean"=with(traits, tapply(VUL, Growth.form, mean, na.rm=TRUE)),
		"SD" = with(traits, tapply(VUL, Growth.form, sd, na.rm=TRUE))))
	#               Mean        SD
	#Lithophyte 2.105369 0.6717147
	#Semi-woody 1.746586 0.3223397
	#Herbaceous 1.683269 0.4967940
	#Climber    1.705157 0.3919029
	#Epiphyte   1.769054 0.5715948
	#Woody      1.682014 0.4308274
 	
	nb.low <- traits[order(traits$VUL),]
	for (menor in (1:10)){
	print(paste(nb.low[menor,3]," (",nb.low[menor,18],")", sep=""))
	}
	#[1] "Stigmaphyllon auriculatum (Cav.) A.Juss (1.087398532)"
	#[1] "Austroeupatorium inulaefolium (Kunth) R.M.King & H.Rob. (1.152367614)"
	#[1] "Ficus enormis Mart. Ex Miq. (1.163083179)"
	#[1] "Chamissoa altissima (Jacq.) Kunth (1.215964784)"
	#[1] "Ficus pertusa L.f. (1.223399031)"
	#[1] "Randia armata (Sw.) DC. (1.228530616)"
	#[1] "Piper hispidum Sw. (1.248052931)"
	#[1] "Psidium cattleianum Sabine (1.253198337)"
	#[1] "Epidendrum secundum Jacq. (1.253397429)"
	#[1] "Anthurium solitarium Schott (1.263084019)"
	nb.top <- traits[order(-traits$VUL),]
	for (maior in (1:10)){
	print(paste(nb.top[maior,3]," (",nb.top[maior,18],")", sep=""))
	}
	#[1] "Alcantarea glaziouana (Leme) J.R.Grant (3.762776522)"
	#[1] "Dioscorea pseudomacrocapsa G.M.Barroso et al. (3.74681274)"
	#[1] "Ficus nevesiae Carauta (3.738247677)"
	#[1] "Rhipsalis mesembryanthemoides Haw. (3.658083829)"
	#[1] "Cryptanthus acaulis (Lindl.) Beer (3.454604705)"
	#[1] "Alcantarea regina (Vell.) Harms (3.369913351)"
	#[1] "Peperomia incana (Haw.) Hook. (3.297836031)"
	#[1] "Pitcairnia staminea Lodd. (3.212187766)"
	#[1] "Anthurium microphyllum (Raf.) G.Don (3.195917959)"
	#[1] "Ouratea oliviformis (A.St.-Hil.) Engl. (2.900161485)"
	
#####################################
#####################################
#####################################
##### Testing ANOVA assumptions #####
#####################################
#####################################
#####################################

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95, labels = names(x)){
	
	require(ggplot2)
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(Sample.Quantiles = x[ord], Theoretical.Quantiles = q.function(P, ...))

  if(is.null(line.estimate)){
    Q.x <- quantile(df$Sample.Quantiles, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(Sample.Quantiles ~ Theoretical.Quantiles))
  }

  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$Theoretical.Quantiles)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$Theoretical.Quantiles
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE

  if(!is.null(labels)){ 
    df$label <- ifelse(df$Sample.Quantiles > df$upper | df$Sample.Quantiles < df$lower, labels[ord],"")
    }
 
  p <- ggplot(df, aes(x=Theoretical.Quantiles, y=Sample.Quantiles)) +
    geom_point() + ggtitle ("Normal Q-Q Plot") +
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) 
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
   
  print(p)
  coef
}

#Geographic range
	gg_qq(traits$AREA)
	shapiro.test(traits$AREA) # h0 = distribuição normal
	#        Shapiro-Wilk normality test
	#data:  traits$AREA
	#W = 0.66271, p-value < 0.00000000000000022
	shapiro.test(resid(aov(lm(AREA~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(AREA ~ Growth.form, data = traits)))
	#W = 0.72822, p-value < 0.00000000000000022
	shapiro.test(resid(aov(lm(AREA~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(AREA ~ Dispersal.syndrome, data = traits)))
	#W = 0.6892, p-value < 0.00000000000000022

#Niche breadth
	gg_qq(traits$Tol)
	shapiro.test(traits$Tol) # h0 = distribuição normal
	#        Shapiro-Wilk normality test
	#data:  traits$Tol
	#W = 0.86388, p-value = 0.0000000000004236
	shapiro.test(resid(aov(lm(Tol~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(Tol ~ Growth.form, data = traits)))
	#W = 0.8675, p-value = 0.0000000000006654
	shapiro.test(resid(aov(lm(Tol~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(Tol ~ Dispersal.syndrome, data = traits)))
	#W = 0.85982, p-value = 0.0000000000002576
	
#Long-dispersal dispersal
	gg_qq(traits$DL)
	shapiro.test(traits$DL)
	#        Shapiro-Wilk normality test
	#data:  traits$DL
	#W = 0.608, p-value < 0.00000000000000022
	shapiro.test(resid(aov(lm(DL~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(DL ~ Growth.form, data = traits)))
	#W = 0.69883, p-value < 0.00000000000000022
	shapiro.test(resid(aov(lm(DL~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(DL ~ Dispersal.syndrome, data = traits)))
	#W = 0.65944, p-value < 0.00000000000000022

#Vulnerability
	gg_qq(traits$VUL)
	shapiro.test(traits$VUL)
	#        Shapiro-Wilk normality test
	#data:  traits$VUL
	#W = 0.77213, p-value < 0.00000000000000022
	shapiro.test(resid(aov(lm(VUL~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(VUL ~ Growth.form, data = traits)))
	#W = 0.79176, p-value = 0.0000000000000002232
	shapiro.test(resid(aov(lm(VUL~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(VUL ~ Dispersal.syndrome, data = traits)))
	#W = 0.78087, p-value < 0.00000000000000022
	
#box-cox tansformation
library(MASS)
#Geographic range
	dsbc <- boxcox(AREA~Dispersal.syndrome, data=traits)
	dsboxc<-data.frame(dsbc$x,dsbc$y)
	dsboxc[order(dsboxc$dsbc.y),]
	gfbc <- boxcox(AREA~Growth.form, data=traits)
	gfboxc<-data.frame(gfbc$x,gfbc$y)
	gfboxc[order(gfboxc$gfbc.y),]

	traits$area.bc <- log(traits$AREA)##^ 0.14

#Niche bradth
	traits$Tol <- traits$Tol +0.000000001
	gfbc2 <- boxcox(Tol~Growth.form, data=traits)
	gfboxc2<-data.frame(gfbc2$x,gfbc2$y)
	gfboxc2[order(gfboxc2$gfbc2.y),]
	dsbc2 <- boxcox(Tol~Dispersal.syndrome, data=traits)
	dsboxc2<-data.frame(dsbc2$x,dsbc2$y)
	dsboxc2[order(dsboxc2$dsbc2.y),]
	
	traits$niche.bc <- log(traits$Tol)##^ 0.38383838
	
#Long-distance dispersal
	PHbc1 <- boxcox(traits$DL~traits$niche.bc)
	PHboxc1<-data.frame(PHbc1$x,PHbc1$y)
	PHboxc1[order(PHboxc1$PHbc1.y),]
	PHbc2 <- boxcox(traits$DL~traits$area.bc)
	PHboxc2<-data.frame(PHbc2$x,PHbc2$y)
	PHboxc2[order(PHboxc2$PHbc2.y),]
	
	traits$LD.bc <-log(traits$DL)##^ 0.06

#Vulnerability
	PHbc1 <- boxcox(traits$VUL~traits$niche.bc)
	PHboxc1<-data.frame(PHbc1$x,PHbc1$y)
	PHboxc1[order(PHboxc1$PHbc1.y),]
	PHbc2 <- boxcox(traits$VUL~traits$EN)
	PHboxc2<-data.frame(PHbc2$x,PHbc2$y)
	PHboxc2[order(PHboxc2$PHbc2.y),]
	
	traits$VUL.bc <- log(traits$VUL)##^ (-1.75)

#Normality
#Geographic range
	gg_qq(traits$area.bc)
	shapiro.test(traits$area.bc) # h0 = distribuição normal
	#        Shapiro-Wilk normality test
	#data:  traits$area.bc
	#W = 0.98055, p-value = 0.003962
	shapiro.test(resid(aov(lm(area.bc~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(area.bc ~ Growth.form, data = traits)))
	#W = 0.98712, p-value = 0.0444
	shapiro.test(resid(aov(lm(area.bc~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(area.bc ~ Dispersal.syndrome, data = traits)))
	#W = 0.98193, p-value = 0.006474

#Niche breadth
	gg_qq(traits$niche.bc)
	shapiro.test(traits$niche.bc) # h0 = distribuição normal
	#        Shapiro-Wilk normality test
	#data:  traits$niche.bc
	#W = 0.9808, p-value = 0.004331
	shapiro.test(resid(aov(lm(niche.bc~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(niche.bc ~ Growth.form, data = traits)))
	#W = 0.99063, p-value = 0.1671
	shapiro.test(resid(aov(lm(niche.bc~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(niche.bc ~ Dispersal.syndrome, data = traits)))
	#W = 0.98429, p-value = 0.01535
	
#Long-dispersal dispersal
	gg_qq(traits$LD.bc)
	shapiro.test(traits$LD.bc)
	#        Shapiro-Wilk normality test
	#data:  traits$LD.bc
	#W = 0.98517, p-value = 0.02179
	shapiro.test(resid(aov(lm(LD.bc~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(LD.bc ~ Growth.form, data = traits)))
	#W = 0.99087, p-value = 0.1845
	shapiro.test(resid(aov(lm(LD.bc~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(LD.bc ~ Dispersal.syndrome, data = traits)))
	#W = 0.98114, p-value = 0.005024

#Vulnerability
	gg_qq(traits$VUL.bc)
	shapiro.test(traits$VUL.bc)
	#        Shapiro-Wilk normality test
	#data:  traits$VUL.bc
	#W = 0.98743, p-value = 0.05095
	shapiro.test(resid(aov(lm(VUL.bc~Growth.form,data=traits))))
	#        Shapiro-Wilk normality test
	#data:  resid(aov(lm(VUL.bc ~ Growth.form, data = traits)))
	#W = 0.99005, p-value = 0.1367
	shapiro.test(resid(aov(lm(VUL.bc~Dispersal.syndrome,data=traits))))
	#        Shapiro-Wilk normality test

data:  resid(aov(lm(VUL.bc ~ Dispersal.syndrome, data = traits)))
W = 0.98787, p-value = 0.06011
	
#Homoscedasticity
library(car)
library(ggplot2)

#Geographic range
	qplot(.fitted, .resid, data = lm(area.bc ~ Growth.form, data = traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	qplot(.fitted, .resid, data = lm(area.bc~Dispersal.syndrome,data=traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	leveneTest(lm(area.bc ~ Growth.form, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#       Df F value Pr(>F)
	#group   5  1.7206 0.1311
	#      214 
	leveneTest(lm(area.bc ~ Dispersal.syndrome, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value Pr(>F) 
	#group   2  2.2822 0.1045
	#      217 

#Niche brasdth
	qplot(.fitted, .resid, data = lm(niche.bc ~ Growth.form, data = traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	qplot(.fitted, .resid, data = lm(niche.bc~Dispersal.syndrome,data=traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	leveneTest(lm(niche.bc ~ Growth.form, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value Pr(>F)
	#group   5  0.0642 0.9972
	#      214 
	leveneTest(lm(niche.bc ~ Dispersal.syndrome, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value  Pr(>F)  
	#group   2  4.3694 0.01379 *
	#      217                  
	#---
	#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Long-distance dispersal
	qplot(.fitted, .resid, data = lm(LD.bc ~ Growth.form, data = traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	qplot(.fitted, .resid, data = lm(LD.bc ~ Dispersal.syndrome,data=traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	leveneTest(lm(LD.bc ~ Growth.form, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value Pr(>F)
	#group   5   2.249 0.05069 .
	#      213                  
	#---
	#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
	leveneTest(lm(LD.bc ~ Dispersal.syndrome, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value  Pr(>F)  
	#group   2  1.7074 0.1838
	#      216 

#Vulnerability
	qplot(.fitted, .resid, data = lm(VUL.bc ~ Growth.form, data = traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	qplot(.fitted, .resid, data = lm(VUL.bc~Dispersal.syndrome,data=traits)) +
		 geom_hline(yintercept = 0, color = "red") +
		 geom_smooth(se = FALSE, linetype = "dashed", color = "black") + 
		 ggtitle("Residuals vs Fitted") +
		 labs(x="Fitted values",y="Residuals")

	leveneTest(lm(VUL.bc ~ Growth.form, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value Pr(>F)
	#group   5  0.7767 0.5675
      213 
	leveneTest(lm(VUL.bc ~ Dispersal.syndrome, data = traits))
	#Levene's Test for Homogeneity of Variance (center = median)
	#    Df F value  Pr(>F)  
	#group   2  5.5592 0.004423 **
	#      216                    
	#---
	#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#####################################
#####################################
#####################################
############ Correlations ###########
#####################################
#####################################
#####################################

#Growth form x Dispersal syndrome

	#Proportion
	bysd <- qplot(factor(Dispersal.syndrome), data = traits, geom = "bar", fill = factor(Growth.form))
		bysd + geom_bar(colour="grey70") +
		scale_fill_manual("Growth form", values = cor.gf) +
		xlab("Dispersal syndrome") + ylab("Count of species")+
		theme_bw() + theme(axis.title=element_text(face="bold"))
		

	bygf <- qplot(factor(Growth.form), data = traits, geom = "bar", 
		fill = factor(Dispersal.syndrome))
	figs1a <- bygf + geom_bar(colour="grey70") +
		scale_fill_manual("Dispersal syndrome", values = cor.ds) +
		xlab("Growth form") + ylab("Count of species") +
		theme_bw() +
		theme(axis.title=element_text(size=12,face="bold"),
		legend.title = element_blank(),
		axis.text.x = element_text(face="bold", angle=30, hjust = 1),
		legend.position = "none", axis.title.x = element_blank(),
		axis.text.y = element_text(face="bold"))


	#Associations
	library(corrplot)
	(chisq <- chisq.test(table(traits$Dispersal.syndrome,traits$Growth.form),simulate.p.value = TRUE))
	#	Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
	#data: table(traits$Dispersal.syndrome, traits$Growth.form)
	#X-squared = 46.966, df = NA, p-value = 0.0004998
	### Association between atributes through chi-square residuals/Pearson's residuals (r)
	#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
	chisq$residuals
	#             Lithophyte Semi-woody Herbaceous    Climber   Epiphyte      Woody
	#  Autochory  -1.0180195 -0.6605783 -0.2701477  0.9170245 -1.4396969  1.1165811
	#  Anemochory  3.2122348 -0.7355996  1.5667935  1.2851625 -0.9760604 -2.8228318
	#  Zoochory   -2.5332698  0.8675624 -1.3084406 -1.4394649  1.3322990  2.1547810
	
	correlacao<- corrplot(chisq$residuals, is.cor = FALSE, tl.srt = 30, method = "number", cl.pos = "n", 
		number.cex = 1.5)
	corrplot(chisq$residuals, is.cor = FALSE, col = cm.colors(100), tl.srt = 45)
	#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
	
###############################################################
## ANOVAs

	sheetanova <- traits %>%
		dplyr::select(c(Dispersal.syndrome, Growth.form, niche.bc, LD.bc, area.bc, EN, VUL.bc))
	sheetanova$Dispersal.syndrome <- factor(sheetanova$Dispersal.syndrome, 
		levels = c("Autochory", "Anemochory", "Zoochory"))
	sheetanova <- sheetanova[-c(8,52,168),]

# Welch's ANOVA for unbalanced data
# Outlying Mean Index analysis
	library(dunn.test)
	library(ggplot2)

#Species range-size distribution
#Geographic range x Growth form (Welch's ANOVA)
	oneway.test(area.bc~Growth.form, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  area.bc and Growth.form
	#F = 8.2702, num df = 5.000, denom df = 49.507, p-value = 0.00000974
	
	wanova.gfgr<-paired.wanova("area.bc","Growth.form",sheetanova)
	wanova.gfgr[,1] <- p.adjust(wanova.gfgr[,1], "bonferroni")
	wanova.gfgr[,2] <- p.adjust(wanova.gfgr[,2], "bonferroni")
	wanova.gfgr[,3] <- p.adjust(wanova.gfgr[,3], "bonferroni")
	wanova.gfgr[,4] <- p.adjust(wanova.gfgr[,4], "bonferroni")
	wanova.gfgr[,5] <- p.adjust(wanova.gfgr[,5], "bonferroni")
	wanova.gfgr[,6] <- p.adjust(wanova.gfgr[,6], "bonferroni")
	wanova.gfgr
	#           Lithophyte Semi-woody Herbaceous   Climber  Epiphyte     Woody
	#Lithophyte  1.0000000  0.0005178   0.000249 0.0000144 0.0000564 0.0000036
	#Semi-woody  0.0005178  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Herbaceous  0.0002490  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Climber     0.0000144  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Epiphyte    0.0000564  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Woody       0.0000036  1.0000000   1.000000 1.0000000 1.0000000 1.0000000

	fstatistic.gfgr <- paired.fstatistic("area.bc","Growth.form",sheetanova)
	fstatistic.gfgr
	#           Lithophyte Semi-woody Herbaceous Climber Epiphyte   Woody
	#Lithophyte     1.0000    23.8721    21.4549 33.6207  25.9928 40.1782
	#Semi-woody    23.8721     1.0000     0.1370  0.0028   0.0460  0.1673
	#Herbaceous    21.4549     0.1370     1.0000  0.1570   0.0340  0.7775
	#Climber       33.6207     0.0028     0.1570  1.0000   0.0442  0.3995
	#Epiphyte      25.9928     0.0460     0.0340  0.0442   1.0000  0.5524
	#Woody         40.1782     0.1673     0.7775  0.3995   0.5524  1.0000

#Geographic range x Dispersal syndrome
	oneway.test(area.bc~Dispersal.syndrome, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  area.bc and Dispersal.syndrome
	#F = 1.7972, num df = 2.000, denom df = 35.064, p-value = 0.1807

	wanova.dsgr<-paired.wanova("area.bc","Dispersal.syndrome",sheetanova)
	wanova.dsgr[,1] <- p.adjust(wanova.dsgr[,1], "bonferroni")
	wanova.dsgr[,2] <- p.adjust(wanova.dsgr[,2], "bonferroni")
	wanova.dsgr[,3] <- p.adjust(wanova.dsgr[,3], "bonferroni")
	wanova.dsgr
	#           Autochory Anemochory  Zoochory
	#Autochory  1.0000000  0.2306940 0.7851378
	#Anemochory 0.2306940  1.0000000 0.7778238
	#Zoochory   0.7851378  0.7778238 1.0000000

	fstatistic.dsgr <- paired.fstatistic("area.bc","Dispersal.syndrome",sheetanova)
	fstatistic.dsgr
	#           Autochory Anemochory Zoochory
	#Autochory     1.0000     3.4794   1.3481
	#Anemochory    3.4794     1.0000   1.2805
	#Zoochory      1.3481     1.2805   1.0000

#Niche breadth x Growth form (Welch's ANOVA)
	oneway.test(niche.bc~Growth.form, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  niche.bc and Growth.form
	#F = 5.5608, num df = 5.000, denom df = 46.759, p-value = 0.0004235
	
	wanova.gfnb<-paired.wanova("niche.bc","Growth.form",sheetanova)
	wanova.gfnb[,1] <- p.adjust(wanova.gfnb[,1], "bonferroni")
	wanova.gfnb[,2] <- p.adjust(wanova.gfnb[,2], "bonferroni")
	wanova.gfnb[,3] <- p.adjust(wanova.gfnb[,3], "bonferroni")
	wanova.gfnb[,4] <- p.adjust(wanova.gfnb[,4], "bonferroni")
	wanova.gfnb[,5] <- p.adjust(wanova.gfnb[,5], "bonferroni")
	wanova.gfnb[,6] <- p.adjust(wanova.gfnb[,6], "bonferroni")
	wanova.gfnb
	#           Lithophyte Semi-woody Herbaceous Climber Epiphyte Woody
	#Lithophyte  1.0000000  0.0023004  0.0003066 0.0002802 0.0004872 0.0002562
	#Semi-woody  0.0023004  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Herbaceous  0.0003066  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Climber     0.0002802  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Epiphyte    0.0004872  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Woody       0.0002562  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000

	fstatistic.gfnb <- paired.fstatistic("niche.bc","Growth.form",sheetanova)
	fstatistic.gfnb
	#           Lithophyte Semi-woody Herbaceous Climber Epiphyte   Woody
	#Lithophyte     1.0000    17.0887    25.4240 26.9937  23.9405 28.0185
	#Semi-woody    17.0887     1.0000     0.8510  1.0525   0.3736  1.2773
	#Herbaceous    25.4240     0.8510     1.0000  0.0030   0.2077  0.0269
	#Climber       26.9937     1.0525     0.0030  1.0000   0.3379  0.0161
	#Epiphyte      23.9405     0.3736     0.2077  0.3379   1.0000  0.5400
	#Woody         28.0185     1.2773     0.0269  0.0161   0.5400  1.0000

	nichegf <- ggplot(traits, aes(RS1,Growth.form))
	nichegf1 <- nichegf + geom_point(aes(size=Tol, col=Tol)) + theme_bw() +
		labs (x="", y="", col ="Tolerance", size= "Tolerance") + 
		scale_colour_viridis_c(guide = "legend",  direction = -1, end = 0.8)+
		geom_segment(aes(x = -2.7, y = .65, xend = 1.7, yend = .65), 
		arrow = arrow(length = unit(.2, "cm")))+
		theme(axis.text=element_text(face="bold"))+
		annotate("text", label = "warmer and wetter", x = .5, y = .8, size = 3,
		fontface = 2) + geom_rug(sides="b") 
	
#Niche breadth x Dispersal syndrome (Welch's ANOVA)
	oneway.test(niche.bc~Dispersal.syndrome, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  niche.bc and Dispersal.syndrome
	#F = 3.2833, num df = 2.000, denom df = 38.216, p-value = 0.04832
	
	wanova.dsnb<-paired.wanova("niche.bc","Dispersal.syndrome",sheetanova)
	wanova.dsnb[,1] <- p.adjust(wanova.dsnb[,1], "bonferroni")
	wanova.dsnb[,2] <- p.adjust(wanova.dsnb[,2], "bonferroni")
	wanova.dsnb[,3] <- p.adjust(wanova.dsnb[,3], "bonferroni")
	wanova.dsnb
	#Autochory  1.0000000  0.0517932 0.9485172
	#Anemochory 0.0517932  1.0000000 0.1189023
	#Zoochory   0.9485172  0.1189023 1.0000000

	fstatistic.dsnb <- paired.fstatistic("niche.bc","Dispersal.syndrome",sheetanova)
	fstatistic.dsnb
	#           Autochory Anemochory Zoochory
	#Autochory     1.0000     6.2014   1.0630
	#Anemochory    6.2014     1.0000   4.3145
	#Zoochory      1.0630     4.3145   1.0000

#Niche position x Dispersal.syndrome
	RS1.ds <- data.frame(RS1 = c(
		mean(subset(traits, Dispersal.syndrome == "Autochory")$RS1),
		mean(subset(traits, Dispersal.syndrome == "Anemochory")$RS1),
		mean(subset(traits, Dispersal.syndrome == "Zoochory")$RS1)),
		Var= c(
		var(subset(traits, Dispersal.syndrome == "Autochory")$RS1),
		var(subset(traits, Dispersal.syndrome == "Anemochory")$RS1),
		var(subset(traits, Dispersal.syndrome == "Zoochory")$RS1)),
		SD = c(
		sd(subset(traits, Dispersal.syndrome == "Autochory")$RS1),
		sd(subset(traits, Dispersal.syndrome == "Anemochory")$RS1),
		sd(subset(traits, Dispersal.syndrome == "Zoochory")$RS1)))
	row.names(RS1.ds) <- c("Autochory","Anemochory","Zoochory")
	#                RS1       Var       SD
	#Autochory  -0.2241525 0.8950022 0.9460456
	#Anemochory -0.4115400 0.4723379 0.6872684
	#Zoochory   -0.3655447 0.6155342 0.7845599
	
	nicheds <- ggplot(traits, aes(RS1,Dispersal.syndrome))
	nicheds1 <- nicheds + geom_point(aes(size=Tol, col=Tol)) + theme_bw() +
		labs (x="", y="", col ="Tolerance", size= "Tolerance") +
		scale_colour_viridis_c(guide = "legend",  direction = -1, end = 0.8)+
  		geom_segment(aes(x = -2.7, y = .55, xend = 1.7, yend = .55), 
		arrow = arrow(length = unit(.2, "cm")))+
		theme(axis.text=element_text(face="bold"))+
		annotate("text", label = "warmer and wetter", x = .5, y = .63, size = 3,
		fontface = 2) + geom_rug(sides="b") 

	tiff("20230215_Fig_S5.tiff", width = 240, height = 120, units = "mm", res = 600)
	figs5 <- ggarrange(nichegf1, nicheds1, ncol = 2,
	common.legend = TRUE, legend = 'right', labels = c('A','B'))
	annotate_figure(figs5, bottom = 'Niche position')
	dev.off()

#Long-dispersal x Growth form (Welch's ANOVA)
	oneway.test(LD.bc~Growth.form, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  LD.bc and Growth.form
	#F = 11.047, num df = 5.000, denom df = 50.747, p-value = 0.0000003141

	wanova.gfld<-paired.wanova("LD.bc","Growth.form",sheetanova)
	wanova.gfld[,1] <- p.adjust(wanova.gfld[,1], "bonferroni")
	wanova.gfld[,2] <- p.adjust(wanova.gfld[,2], "bonferroni")
	wanova.gfld[,3] <- p.adjust(wanova.gfld[,3], "bonferroni")
	wanova.gfld[,4] <- p.adjust(wanova.gfld[,4], "bonferroni")
	wanova.gfld[,5] <- p.adjust(wanova.gfld[,5], "bonferroni")
	wanova.gfld[,6] <- p.adjust(wanova.gfld[,6], "bonferroni")
	wanova.gfld
	#           Lithophyte Semi-woody Herbaceous   Climber  Epiphyte     Woody
	#Lithophyte  1.0000000  0.0003054  0.0003258 0.0000018 0.0000036 0.0000000
	#Semi-woody  0.0003054  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Herbaceous  0.0003258  1.0000000  1.0000000 1.0000000 1.0000000 0.9085494
	#Climber     0.0000018  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Epiphyte    0.0000036  1.0000000  1.0000000 1.0000000 1.0000000 1.0000000
	#Woody       0.0000000  1.0000000  0.9085494 1.0000000 1.0000000 1.0000000

	fstatistic.gfld <- paired.fstatistic("LD.bc","Growth.form",sheetanova)
	fstatistic.gfld
	#           Lithophyte Semi-woody Herbaceous Climber Epiphyte   Woody
	#Lithophyte     1.0000    27.2786    20.4080 40.8910  34.2962 50.9489
	#Semi-woody    27.2786     1.0000     0.2104  0.0035   0.0380  0.9833
	#Herbaceous    20.4080     0.2104     1.0000  0.3534   0.4574  2.1314
	#Climber       40.8910     0.0035     0.3534  1.0000   0.0308  1.4550
	#Epiphyte      34.2962     0.0380     0.4574  0.0308   1.0000  0.7496
	#Woody         50.9489     0.9833     2.1314  1.4550   0.7496  1.0000

	ldistancegf <- ggplot(traits, aes(RS1,Growth.form))
	ldistancegf1 <- ldistancegf + geom_point(aes(size=LD.bc, col=LD.bc)) + theme_bw() +
		labs (x="", y="", col ="Long-dispersal\ncapacity", size= "Long-dispersal\ncapacity") + 
		scale_colour_viridis_c(guide = "legend",  direction = -1, end = 0.8)+
		geom_segment(aes(x = -2.7, y = .65, xend = 1.7, yend = .65), 
		arrow = arrow(length = unit(.2, "cm")))+
		theme(axis.text=element_text(face="bold"))+
		annotate("text", label = "warmer and wetter", x = .5, y = .8, size = 3,
		fontface = 2) + geom_rug(sides="b") 
	
#Long-dispersal x Dispersal syndrome (Welch's ANOVA)
	oneway.test(LD.bc~Dispersal.syndrome, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  LD.bc and Dispersal.syndrome
	#F = 2.3761, num df = 2.000, denom df = 32.572, p-value = 0.1088
	
	wanova.dsld<-paired.wanova("LD.bc","Dispersal.syndrome",sheetanova)
	wanova.dsld[,1] <- p.adjust(wanova.dsld[,1], "bonferroni")
	wanova.dsld[,2] <- p.adjust(wanova.dsld[,2], "bonferroni")
	wanova.dsld[,3] <- p.adjust(wanova.dsld[,3], "bonferroni")
	wanova.dsld
	#           Autochory Anemochory  Zoochory
	#Autochory  1.0000000  0.9457461 1.0000000
	#Anemochory 0.9457461  1.0000000 0.0914988
	#Zoochory   1.0000000  0.0914988 1.0000000

	fstatistic.dsld <- paired.fstatistic("LD.bc","Dispersal.syndrome",sheetanova)
	fstatistic.dsld
	#           Autochory Anemochory Zoochory
	#Autochory     1.0000     1.0714   0.0471
	#Anemochory    1.0714     1.0000   4.7545
	#Zoochory      0.0471     4.7545   1.0000

#Long-dispersal x Dispersal.syndrome
	RS1.ds <- data.frame(RS1 = c(
		mean(subset(traits, Dispersal.syndrome == "Autochory")$RS1),
		mean(subset(traits, Dispersal.syndrome == "Anemochory")$RS1),
		mean(subset(traits, Dispersal.syndrome == "Zoochory")$RS1)),
		Var= c(
		var(subset(traits, Dispersal.syndrome == "Autochory")$RS1),
		var(subset(traits, Dispersal.syndrome == "Anemochory")$RS1),
		var(subset(traits, Dispersal.syndrome == "Zoochory")$RS1)),
		SD = c(
		sd(subset(traits, Dispersal.syndrome == "Autochory")$RS1),
		sd(subset(traits, Dispersal.syndrome == "Anemochory")$RS1),
		sd(subset(traits, Dispersal.syndrome == "Zoochory")$RS1)))
	row.names(RS1.ds) <- c("Autochory","Anemochory","Zoochory")
	#                  RS1       Var        SD
	#Autochory  -0.2241525 0.8950022 0.9460456
	#Anemochory -0.4115400 0.4723379 0.6872684
	#Zoochory   -0.3655447 0.6155342 0.7845599

	ldistanceds <- ggplot(traits, aes(RS1,Dispersal.syndrome))
	ldistanceds1 <- ldistanceds + geom_point(aes(size=LD.bc, col=LD.bc)) + theme_bw() +
		labs (x="", y="", col ="Long-dispersal\ncapacity", size= "Long-dispersal\ncapacity") +
		scale_colour_viridis_c(guide = "legend",  direction = -1, end = 0.8)+
  		geom_segment(aes(x = -2.7, y = .55, xend = 1.7, yend = .55), 
		arrow = arrow(length = unit(.2, "cm")))+
		theme(axis.text=element_text(face="bold"))+
		annotate("text", label = "warmer and wetter", x = .5, y = .63, size = 3,
		fontface = 2) + geom_rug(sides="b") 

	tiff("20230215_Fig_S6.tiff", width = 240, height = 120, units = "mm", res = 600)
	figs5 <- ggarrange(ldistancegf1, ldistanceds1, ncol = 2,
	common.legend = TRUE, legend = 'right', labels = c('A','B'))
	annotate_figure(figs5, bottom = 'Niche position')
	dev.off()

write.table(wanova.dsgr,"20230215_dsxgr.txt")
write.table(wanova.dsnb,"20230215_dsxnb.txt")
write.table(wanova.dsld,"20230215_dsxld.txt")
write.table(wanova.gfnb,"20230215_gfxnb.txt")
write.table(wanova.gfgr,"20230215_gfxgr.txt")
write.table(wanova.gfld,"20230215_gfxld.txt")

write.table(fstatistic.gfgr,"20230215_gfgr.txt")
write.table(fstatistic.dsgr,"20230215_dsgr.txt")
write.table(fstatistic.gfnb,"20230215_gfnb.txt")
write.table(fstatistic.dsnb,"20230215_dsnb.txt")
write.table(fstatistic.gfld,"20230215_gfld.txt")
write.table(fstatistic.dsld,"20230215_dsld.txt")
	
################################
############ FAMD ##############
################################
	
	library(FactoMineR)
	library(MASS)
	library(dplyr)
	library(gridExtra)

	traits.facto <- traits %>%
		dplyr::select(c(Dispersal.syndrome, Growth.form, niche.bc, LD.bc, area.bc))

	names(traits.facto) <- c("Dispersal syndrome", "Growth form", "Niche breadth",
		"Long-dispersal capacity", "Geographic range")
	row.names(traits.facto) <- traits[,1]
	
	facto <- FAMD(na.omit(traits.facto))
	summary(facto)
	#Call:
	#FAMD(base = na.omit(traits.facto)) 
	#
	#Eigenvalues
	#                      Dim.1  Dim.2  Dim.3  Dim.4  Dim.5
	#Variance              2.993  1.322  1.132  1.001  1.000
	#% of var.            29.926 13.219 11.324 10.006 10.000
	#Cumulative % of var. 29.926 43.145 54.469 64.474 74.474
	#
	#Individuals (the 10 first)
	#                            Dist    Dim.1    ctr   cos2    Dim.2    ctr   cos2    Dim.3    ctr   cos2  
	#aaa9                    |  5.225 |  0.173  0.005  0.001 | -1.627  0.915  0.097 |  1.016  0.416  0.038 |
	#aac10                   |  2.221 |  0.036  0.000  0.000 |  1.483  0.760  0.446 |  0.019  0.000  0.000 |
	#aac5                    |  2.575 | -0.638  0.062  0.061 | -1.470  0.746  0.326 |  1.131  0.516  0.193 |
	#aag108                  |  3.230 | -1.385  0.293  0.184 | -0.059  0.001  0.000 | -0.624  0.157  0.037 |
	#aah6                    |  3.087 | -1.489  0.339  0.233 | -1.648  0.938  0.285 |  1.018  0.418  0.109 |
	#aai115                  |  2.795 |  1.928  0.567  0.475 |  0.582  0.117  0.043 | -0.173  0.012  0.004 |
	#aam111                  |  3.952 | -2.644  1.066  0.448 | -1.945  1.307  0.242 |  0.896  0.324  0.051 |
	#aap109                  |  1.936 |  0.432  0.028  0.050 |  0.254  0.022  0.017 | -0.356  0.051  0.034 |
	#aap7                    |  2.423 | -0.030  0.000  0.000 | -1.346  0.626  0.309 |  1.208  0.589  0.249 |
	#aar114                  |  3.515 | -2.724  1.132  0.600 |  0.895  0.277  0.065 | -0.328  0.043  0.009 |
	#
	#Continuous variables
	#                           Dim.1    ctr   cos2    Dim.2    ctr   cos2    Dim.3    ctr   cos2  
	#Niche breadth           |  0.892 26.586  0.796 |  0.162  1.979  0.026 |  0.050  0.224  0.003 |
	#Long-dispersal capacity |  0.929 28.853  0.863 |  0.080  0.485  0.006 |  0.086  0.656  0.007 |
	#Geographic range        |  0.933 29.076  0.870 |  0.159  1.924  0.025 |  0.071  0.445  0.005 |
	#
	#Categories
	#                            Dim.1     ctr    cos2  v.test     Dim.2     ctr    cos2  v.test     Dim.3     ctr    cos2  v.test  
	#Autochory               |   0.627   0.240   0.022   1.288 |   0.559   0.981   0.018   1.729 |  -3.319  47.060   0.618 -11.086 |
	#Anemochory              |  -0.574   1.527   0.189  -4.129 |   1.019  24.693   0.596  11.034 |   0.323   3.386   0.060   3.782 |
	#Zoochory                |   0.385   0.878   0.138   3.489 |  -0.857  22.277   0.686 -11.683 |   0.090   0.333   0.008   1.321 |
	#Lithophyte              |  -3.382  11.081   0.683  -8.897 |   0.635   2.001   0.024   2.513 |   0.103   0.072   0.001   0.440 |
	#Semi-woody              |  -0.064   0.002   0.000  -0.106 |  -1.313   3.602   0.065  -3.282 |   1.054   3.164   0.042   2.847 |
	#Herbaceous              |  -0.059   0.004   0.000  -0.177 |   1.221   9.356   0.179   5.503 |   0.706   4.259   0.060   3.436 |
	#Climber                 |   0.214   0.142   0.017   1.134 |   0.933  13.870   0.320   7.443 |  -0.343   2.553   0.043  -2.956 |
	#Epiphyte                |   0.194   0.071   0.007   0.746 |  -0.852   7.021   0.143  -4.934 |   1.309  22.559   0.337   8.186 |
	#Woody                   |   0.657   1.540   0.174   3.842 |  -0.804  11.810   0.260  -7.073 |  -0.783  15.292   0.247  -7.449 |
	#
	#cos2 indica o quão bem representado a variável ou categoria está pelos eixos.
	#v.test >|2| indica que a categoria está significativamente diferente de zero no eixo

	circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
		r = diameter / 2
		tt <- seq(0,2*pi,length.out = npoints)
		xx <- center[1] + r * cos(tt)
		yy <- center[2] + r * sin(tt)
		return(data.frame(x = xx, y = yy))
	}
	dat <- circleFun(c(0,0),2,npoints = 100)
		#geom_path will do open circles, geom_polygon will do filled circles

	quant.famd <- ggplot(dat,aes(x,y)) + geom_path() + theme_bw()+
		ylim(-1,1) + xlim(-1,1)+
		geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0),linetype=2) +
		geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1),linetype=2) +
		labs (x=paste("Dim 1",paste("(",round(facto$eig[1,2], 2),
		 "%", ")", sep=""), sep=" "), y=paste("Dim 2",
		paste("(",round(facto$eig[2,2], 2), "%", ")", sep=""), sep=" ")) +
		geom_segment(aes(x = 0, y = 0, xend = facto$quanti.var$coord[1,1],
		yend = facto$quanti.var$coord[1,2]),size = 1.01, 
		arrow = arrow(length = unit(.2, "cm"))) +
		annotate("text", label = "NB",
		x = facto$quanti.var$coord[1,1]+0.1, y = facto$quanti.var$coord[1,2]-0.02,
		size = 4) + theme (axis.title=element_text(face="bold"),
		axis.text=element_text(face="bold")) +
		geom_segment(aes(x = 0, y = 0, xend = facto$quanti.var$coord[2,1],
		yend = facto$quanti.var$coord[2,2]),size = 1.01, 
		arrow = arrow(length = unit(.2, "cm"))) +
		annotate("text", label = "LD",
		x = facto$quanti.var$coord[2,1]-.08, y = facto$quanti.var$coord[2,2]-.1,
		size = 4) +
		geom_segment(aes(x = 0, y = 0, xend = facto$quanti.var$coord[3,1],
		yend = facto$quanti.var$coord[3,2]),size = 1.01, 
		arrow = arrow(length = unit(.2, "cm"))) +
		annotate("text", label = "GR",
		x = facto$quanti.var$coord[3,1]-.2, y = facto$quanti.var$coord[3,2]+.03,
		size = 4) + theme (axis.title=element_text(face="bold"))

	famd.ind<-data.frame(facto$ind$coord)
	famd.ind2<-cbind(famd.ind,traits[-168,c(4:5)])
	
	#GF
	famd.gf <- ggplot(famd.ind2,aes(x = Dim.1, y = Dim.2, linetype = Growth.form,
		color = Growth.form)) + geom_segment(aes(x = -3, y = 0, xend = 6, yend = 0),
		linetype=2,	colour = "black") + geom_segment(aes(x = 0, y = -3, xend = 0,
		yend = 4.5), linetype=2, colour = "black") + 
		geom_point(size=5, alpha = .9) + theme_bw() +
		scale_colour_manual(values=cor.gf) +
		labs (x=paste("Dim 1",paste("(",round(facto$eig[1,2], 2),
		 "%", ")", sep=""), sep=" "), y=paste("Dim 2",
		paste("(",round(facto$eig[2,2], 2), "%", ")", sep=""), sep=" "),
		color="") +
		theme(legend.position= c(.16,.81), axis.title=element_text(face="bold"),
		axis.text=element_text(face="bold"),legend.text = element_text(face="bold"),
		legend.background = element_rect(fill=alpha(0.1))) 
		#stat_ellipse() para fazer as elipses

	#DS
	famd.ds <- ggplot(famd.ind2,aes(x = Dim.1, y = Dim.2, linetype = Dispersal.syndrome,
		color = Dispersal.syndrome)) + geom_segment(aes(x = -3, y = 0, xend = 6, yend = 0),
		linetype=2,	colour = "black") + geom_segment(aes(x = 0, y = -3, xend = 0,
		yend = 4.5), linetype=2, colour = "black") + 
		geom_point(size=5, alpha = .9) + theme_bw() +
		scale_colour_manual(values=cor.ds) +
		labs (x=paste("Dim 1",paste("(",round(facto$eig[1,2], 2),
		 "%", ")", sep=""), sep=" "), y=paste("Dim 2",
		paste("(",round(facto$eig[2,2], 2), "%", ")", sep=""), sep=" "),
		color="") +
		theme(legend.position= c(.16,.92), axis.title=element_text(face="bold"),
		axis.text=element_text(face="bold"), legend.text = element_text(face="bold"),
		legend.background = element_rect(fill=alpha(0.1)))

	grid.arrange(quant.famd, famd.gf, famd.ds, ncol = 3, widths = c(3/9,3/9,3/9))
	grid.arrange(quant.famd, famd.gf, famd.ds, ncol = 1, heights = c(3/9,3/9,3/9))
	
	tiff("20230215_Bondi_et_al_Fig 2.tiff", width = 360, height = 120, units = "mm", res = 600)
		ggarrange(quant.famd, famd.gf, famd.ds, ncol = 3, labels = c('A','B','C'))	
	dev.off()

#########################################################################################################
## boxplot
	
#Niche breadth x Growth form
	NBxGF <- ggplot(traits, aes(factor(Growth.form),niche.bc, fill=Growth.form)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Growth forms", y="Niche breadth", 
	fill = 'Growth form')+
	scale_fill_manual(values = cor.gf) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		legend.position="none")
	
#Niche breadth x Dispersal syndrome
	NBxDS <- ggplot(traits, aes(factor(Dispersal.syndrome),niche.bc, fill=Dispersal.syndrome)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Dispersal syndromes", y="", 
	fill = 'Dispersal\nsyndrome')+
	scale_fill_manual(values = cor.ds) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")

#Long-dispersal capacity x Growth form
	LDxGF <- ggplot(traits, aes(factor(Growth.form),LD.bc, fill=Growth.form)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Growth forms", y="Long-dispersal\ncapacity", 
	fill = 'Growth form')+
	scale_fill_manual(values = cor.gf) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		legend.position="none")
	
#Long-dispersal capacity x Dispersal syndrome
	LDxDS <- ggplot(traits, aes(factor(Dispersal.syndrome),LD.bc, fill=Dispersal.syndrome)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Dispersal syndromes", y="", 
	fill = 'Dispersal\nsyndrome')+
	scale_fill_manual(values = cor.ds) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")

#Geographic range x Growth form
	RWxGF <- ggplot(traits, aes(factor(Growth.form),area.bc, fill=Growth.form)) +
	geom_boxplot(colour="grey70") + labs (x="Growth forms", y="Geographic range",
	fill = 'Growth form')+
	scale_fill_manual(values = cor.gf) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		legend.position="none")

#Geographic range x Dispersal syndrome
	RWxDS <- ggplot(traits, aes(factor(Dispersal.syndrome),area.bc, fill=Dispersal.syndrome)) +
	geom_boxplot(colour="grey70") + labs (x="Dispersal syndromes", y="", 
	fill = 'Dispersal\nsyndrome')+ 
	scale_fill_manual(values = cor.ds) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")

# Vulnerabiltiy to climate change
#VUL x Growth form
	VULxGF <- ggplot(traits, aes(factor(Growth.form),VUL, fill=Growth.form)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Growth forms", y="Vulnerability to\nclimate change", 
	fill = 'Growth form')+
	scale_fill_manual(values = cor.gf) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		legend.position="none")

#VUL x Dispersal syndrome
	VULxDS <- ggplot(traits, aes(factor(Dispersal.syndrome),VUL.bc, fill=Dispersal.syndrome)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Dispersal syndromes", y="", 
	fill = 'Dispersal\nsyndrome')+
	scale_fill_manual(values = cor.ds) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"),
		axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")

# legendaGF
	legendaGF <- ggplot(traits, aes(factor(Growth.form),VUL, fill=Growth.form)) +
	geom_boxplot(colour="grey70") + 
	labs (x="", y="", 
	fill = 'Growth form')+
	scale_fill_manual(values = cor.gf) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"))

# legendaDS
	legendaDS <- ggplot(traits, aes(factor(Dispersal.syndrome),VUL.bc, fill=Dispersal.syndrome)) +
	geom_boxplot(colour="grey70") + 
	labs (x="Dispersal syndromes", y="", 
	fill = 'Dispersal\nsyndrome')+
	scale_fill_manual(values = cor.ds) + theme_bw() +
	theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
		axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
		legend.text = element_text(face="bold"), legend.title = element_text(face="bold"))


png("Figure 3.png", width = 28, height = 24, units = "cm", res = 600)	
ggarrange(
	ggarrange(
		ggarrange(NBxGF, NBxDS, ncol = 2, labels = c('A',''), widths = c(6,3.2)),
		ggarrange(LDxGF, LDxDS, ncol = 2, labels = c('B',''), widths = c(6,3.2)),
	ncol = 2),
	ggarrange(
		ggarrange(RWxGF, RWxDS, ncol = 2, labels = c('C',''), widths = c(6,3.2)),
		ggarrange(VULxGF, VULxDS, ncol = 2, labels = c('D',''), widths = c(6,3.2)),	
	ncol = 2),
	ggarrange(legendaGF, legendaGF, ncol = 2, common.legend = TRUE, legend = 'top'),
	ggarrange(legendaDS, legendaDS, ncol = 2, common.legend = TRUE, legend = 'top'),
nrow = 4)

dev.off()


#VUL x Growth form (Welch's ANOVA)
	oneway.test(VUL.bc~Growth.form, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  VUL.bc and Growth.form
	#F = 2.8454, num df = 5.00, denom df = 48.12, p-value = 0.02493
	
	wanova.gfvul<-paired.wanova("VUL.bc","Growth.form",sheetanova)
	wanova.gfvul[,1] <- p.adjust(wanova.gfvul[,1], "bonferroni")
	wanova.gfvul[,2] <- p.adjust(wanova.gfvul[,2], "bonferroni")
	wanova.gfvul[,3] <- p.adjust(wanova.gfvul[,3], "bonferroni")
	wanova.gfvul[,4] <- p.adjust(wanova.gfvul[,4], "bonferroni")
	wanova.gfvul[,5] <- p.adjust(wanova.gfvul[,5], "bonferroni")
	wanova.gfvul[,6] <- p.adjust(wanova.gfvul[,6], "bonferroni")
	wanova.gfvul
	#           Lithophyte Semi-woody Herbaceous   Climber  Epiphyte     Woody
	#Lithophyte  1.0000000  0.5048568   0.071382 0.0656634 0.1973352 0.0359676
	#Semi-woody  0.5048568  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Herbaceous  0.0713820  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Climber     0.0656634  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Epiphyte    0.1973352  1.0000000   1.000000 1.0000000 1.0000000 1.0000000
	#Woody       0.0359676  1.0000000   1.000000 1.0000000 1.0000000 1.0000000

	fstatistic.gfvul<- paired.fstatistic("VUL.bc","Growth.form",sheetanova)
	fstatistic.gfvul
	#           Lithophyte Semi-woody Herbaceous Climber Epiphyte  Woody
	#Lithophyte     1.0000     3.3600     6.9874  7.4540   4.9761 8.8329
	#Semi-woody     3.3600     1.0000     0.5048  0.2343   0.0410 0.5814
	#Herbaceous     6.9874     0.5048     1.0000  0.1825   0.3619 0.0085
	#Climber        7.4540     0.2343     0.1825  1.0000   0.1016 0.2573
	#Epiphyte       4.9761     0.0410     0.3619  0.1016   1.0000 0.4509
	#Woody          8.8329     0.5814     0.0085  0.2573   0.4509 1.0000

#VUL x Dispersal syndrome (Welch's ANOVA)
	oneway.test(VUL.bc~Dispersal.syndrome, data=sheetanova, var.equal=FALSE)
	#	One-way analysis of means (not assuming equal variances)
	#data:  VUL.bc and Dispersal.syndrome
	#F = 8.0492, num df = 2.000, denom df = 53.767, p-value = 0.0008752
	
	wanova.dsvul<-paired.wanova("VUL.bc","Dispersal.syndrome",sheetanova)
	wanova.dsvul[,1] <- p.adjust(wanova.dsvul[,1], "bonferroni")
	wanova.dsvul[,2] <- p.adjust(wanova.dsvul[,2], "bonferroni")
	wanova.dsvul[,3] <- p.adjust(wanova.dsvul[,3], "bonferroni")
	wanova.dsvul
	#          Autochory Anemochory  Zoochory
	#Autochory  1.0000000  0.0003078 0.0002865
	#Anemochory 0.0003078  1.0000000 1.0000000
	#Zoochory   0.0002865  1.0000000 1.0000000

	fstatistic.dsvul<- paired.fstatistic("VUL.bc","Dispersal.syndrome",sheetanova)
	fstatistic.dsvul
	#           Autochory Anemochory Zoochory
	#Autochory     1.0000    16.5745  17.5154
	#Anemochory   16.5745     1.0000   0.0002
	#Zoochory     17.5154     0.0002   1.0000


write.table(wanova.dsvul,"20230215_dsxvul.txt")
write.table(wanova.gfvul,"20230215_gfxvul.txt")

write.table(fstatistic.gfvul,"20230215_gfvul.txt")
write.table(fstatistic.dsvul,"20230215_dsvul.txt")


##############################################
##############################################


	modelVU <- glm(EN ~ VUL.bc, family=binomial(link='logit'),data = sheetanova)
	summary(modelVUds)
	#Call:
	#glm(formula = EN ~ VUL.bc, family = binomial(link = "logit"), 
	#    data = sheetanova)
	#
	#Deviance Residuals: 
	#    Min       1Q   Median       3Q      Max  
	#-1.3478  -0.6119  -0.5132  -0.4284   2.2004  
	#
	#Coefficients:
	#            Estimate Std. Error z value        Pr(>|z|)    
	#(Intercept)  -3.0820     0.4695  -6.564 0.0000000000524 ***
	#VUL.bc        2.6346     0.7102   3.710        0.000207 ***
	#---
	#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
	#
	#(Dispersion parameter for binomial family taken to be 1)
	#
	#    Null deviance: 198.20  on 216  degrees of freedom
	#Residual deviance: 184.24  on 215  degrees of freedom
	#AIC: 188.24
	#
	#Number of Fisher Scoring iterations: 4

	exp(cbind(OR = coef(modelVU), confint(modelVU)))# Odds ratio and confidence interval
	#Waiting for profiling to be done...
	#                    OR      2.5 %     97.5 %
	#(Intercept)  0.0458661 0.01747689  0.1111722
	#VUL.bc      13.9375351 3.52700673 58.6272727

	#Hosmer-Lemeshow Test
	library("MKmisc")
	HLgof.test(fit = fitted(modelVU), obs = sheetanova$EN)
	#$C
	#        Hosmer-Lemeshow C statistic
	#data:  fitted(modelVU) and sheetanova$EN
	#X-squared = 3.487, df = 8, p-value = 0.9002
	#
	#$H
	#        Hosmer-Lemeshow H statistic
	#data:  fitted(modelVU) and sheetanova$EN
	#X-squared = 10.572, df = 8, p-value = 0.2271

	## A significant test indicates that the model is not a good fit and a
	## non-significant test indicates a good fit.
	
	#Data frame with hp in ascending order
	Predicted_data <- data.frame(VUL.bc = seq(
		min(sheetanova$VUL.bc, na.rm =T), max(sheetanova$VUL.bc, na.rm =T),len=1000))
	 
	# Fill predicted values using regression model
	Predicted_data$EN = predict(modelVU, Predicted_data, type="response")
	 
	# Plot Predicted data and original data points
	logregr.gf <- ggplot(sheetanova, aes(x = VUL.bc, y = EN, colour = Growth.form)) + 
		geom_point(size = 4, alpha = .8) + stat_smooth(method="glm", se=FALSE, colour = "black", 
			method.args = list(family=binomial)) +
		theme_bw() + labs (x="Vulnerability to climate change",
			y="Red Lists", colour = 'Growth form') +
		scale_colour_manual(values = cor.gf)  +
		scale_y_continuous(breaks=c(0,1), labels=c("Not included","Included")) +
			theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
			axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
			legend.text = element_text(size = 10), legend.title = element_text(size = 10),
			legend.key.size = unit(.5, 'line'), legend.position = c(0.15, 0.55))

	# Plot Predicted data and original data points
	logregr.ds <- ggplot(sheetanova, aes(x = VUL.bc, y = EN, colour = Dispersal.syndrome)) + 
		geom_point(size = 4, alpha = .8) + stat_smooth(method="glm", se=FALSE, colour = "black", 
			method.args = list(family=binomial)) +
		theme_bw() + labs (x="Vulnerability to climate change",
			y="Red Lists", colour = 'Dispersal\nsyndrome') +
		scale_colour_manual(values = cor.ds) +
		scale_y_continuous(breaks=c(0,1), labels=c("Not included","Included")) +
		theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
			axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
			legend.text = element_text(size = 10), legend.title = element_text(size = 10),
			legend.key.size = unit(.5, 'line'), legend.position = c(0.15, 0.65))

	#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
	#https://www.r-bloggers.com/evaluating-logistic-regression-models/
	#http://www2.unil.ch/biomapper/Download/Lobo-GloEcoBioGeo-2007.pdf
	#https://stackoverflow.com/questions/55680449/ggplot-filled-barplot-with-percentage-labels
	
	library(ggplot2)
	library(scales)
	library(dplyr)
	
	gf_en_raw <- traits %>% dplyr::select(Growth.form, EN) 
	gf_en <- gf_en_raw %>% group_by(Growth.form, EN) %>% summarise(n=n())
	
	pcnt_gf_en <- do.call(rbind,
	  lapply(split(gf_en, gf_en$Growth.form), function(x){x[x$EN=='1', 'n']/sum(x$n)})
	  )
	names(pcnt_gf_en) <- 'pcnt'
	pcnt_gf_en$Growth.form <- rownames(pcnt_gf_en)
	pcnt_gf_en$EN='1'
	
	pcnt_ds_nen <- do.call(rbind,
	  lapply(split(gf_en, gf_en$Growth.form), function(x){x[x$EN=='0', 'n']/sum(x$n)})
	  )
	names(pcnt_ds_nen) <- 'pcnt'
	pcnt_ds_nen$Growth.form <- rownames(pcnt_ds_nen)
	pcnt_ds_nen$EN='0'
	
	gf_en_pcnt <- merge(gf_en_raw , rbind(pcnt_gf_en, pcnt_ds_nen))
	
	
	gf_en_pcnt$labelpos <- ifelse(gf_en_pcnt$EN=='1',
	                      gf_en_pcnt$pcnt/2, 1 - gf_en_pcnt$pcnt/2)
	
	gf_en_pcnt$pcnt <- round(gf_en_pcnt$pcnt,2)
	
	gg_gfen <- ggplot(gf_en_pcnt, aes(x = factor(Growth.form), fill = factor(EN))) +
		geom_bar(position="fill")+ geom_text(aes(label = paste0(100*pcnt,"%"), y = labelpos), size = 3, color = 'white') +
			scale_y_continuous(labels = scales::percent) + labs(x = "", 
			y = "Species number (%)", fill = "Red Lists") +
		scale_fill_manual(values = c("#0d0887ff","#b12a90ff"), labels = c("Not included", "Included")) +
		theme_bw() + coord_flip() + theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
			axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
			legend.text = element_text(size = 10), legend.title = element_text(size = 10))

	ds_en_raw <- traits %>% dplyr::select(Dispersal.syndrome, EN) 
	ds_en <- ds_en_raw %>% group_by(Dispersal.syndrome, EN) %>% summarise(n=n())
	ds_en <- rbind(ds_en, data.frame(Dispersal.syndrome = "Autochory", EN = 1, n = 0))
	
	pcnt_ds_en <- do.call(rbind,
	  lapply(split(ds_en, ds_en$Dispersal.syndrome), function(x){x[x$EN=='1', 'n']/sum(x$n)})
	  )
	names(pcnt_ds_en) <- 'pcnt'
		pcnt_ds_en$Dispersal.syndrome <- rownames(pcnt_ds_en)
	pcnt_ds_en$EN='1'
	
	pcnt_ds_nen <- do.call(rbind,	
	  lapply(split(ds_en, ds_en$Dispersal.syndrome), function(x){x[x$EN=='0', 'n']/sum(x$n)})
	  )
	names(pcnt_ds_nen) <- 'pcnt'
	pcnt_ds_nen$Dispersal.syndrome <- rownames(pcnt_ds_nen)
	pcnt_ds_nen$EN='0'
	
	ds_en_pcnt <- merge(ds_en_raw , rbind(pcnt_ds_en, pcnt_ds_nen))
	
	ds_en_pcnt$labelpos <- ifelse(ds_en_pcnt$EN=='1',
	                      ds_en_pcnt$pcnt/2, 1 - ds_en_pcnt$pcnt/2)

	ds_en_pcnt$pcnt <- round(ds_en_pcnt$pcnt,4)
	
	gg_dsen <- ggplot(ds_en_pcnt, aes(x = factor(Dispersal.syndrome), fill = factor(EN))) +
		geom_bar(position="fill")+ geom_text(aes(label = paste0(100*pcnt,"%"), y = labelpos), size = 3, color = 'white') +
		scale_y_continuous(labels = scales::percent) + labs(x = "", 
			y = "Species number (%)", fill = "Red Lists") +
		scale_fill_manual(values = c("#0d0887ff","#b12a90ff"), labels = c("Not included", "Included")) +
		theme_bw() + coord_flip() + theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
			axis.title=element_text(face="bold"), axis.text=element_text(face="bold"),
			legend.text = element_text(size = 10), legend.title = element_text(size = 10))
	


tiff("Figure 4.tiff", width = 26, height = 14, units = "cm", res = 600)	
	ggarrange(ggarrange(gg_gfen, gg_dsen, ncol = 2, labels = c('A','B'), common.legend = TRUE, legend = 'top'),
	ggarrange(logregr.gf, logregr.ds, ncol = 2, labels = c('C','D')),nrow = 2)
dev.off()

png("Figure 4.png", width = 26, height = 14, units = "cm", res = 600)	
	ggarrange(ggarrange(gg_gfen, gg_dsen, ncol = 2, labels = c('A','B'), common.legend = TRUE, legend = 'top'),
	ggarrange(logregr.gf, logregr.ds, ncol = 2, labels = c('C','D')),nrow = 2)
dev.off()








