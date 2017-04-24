############# R Script to predict species probability of presence #########################
## Hierarchical model, observavions ⊂ small_cells ⊂ big_cells ⊂ NUTS3 polygons
## Input = GBIF data; in the code below input data are simulated
## Last update 03 December 2016
## Code related to the manuscript:
## Rocchini, D.*, Garzon-Lopez, C.X., Marcantonio, M.*,  Amici, V., Bacaro, G., Bastin, L., Brummitt, N., Chiarucci, A., Foody, G.M., Hauffe, H.C., He, K.S., Ricotta, C., Rizzoli, A., Rosà, R. (2016). Anticipating species distributions: handling sampling effort bias under a Bayesian framework. Submitted to Science of the Total Environment.
#######################################################################################

# Load required packages
library(R2jags)
library(coda)
library(MASS) # For mvrnorm()

# Geneate synthetic predictors through multivariate normal distribution
#set.seed(111)
N <- 10000 # number of observations
M <- matrix(c(22.26,2.8,0.01899,0.0317,317.2,4591,776.2),7,7,byrow=T) # covariance matrix
diag(M) <- 1; M[lower.tri(M)] <- t(M)[lower.tri(M)] # make the matrix simmetrical
X <- mvrnorm(N,c(7.85,8.79,0.57,0.85,45.60,146.10,9.00),M,empirical=TRUE,tol=1) # generate data

# The response variable (binomial) is a linear combinantion of this three variables
z = 1 + 2*scale(X[,1]) + 2*scale(X[,1])^2 + 2*scale(X[,2]) + 2*scale(X[,3]) + scale(X[,4]) + scale(X[,5]) + scale(X[,6]) + scale(X[,7]) # linear combination
pr = 1/(1+exp(-z)) # pass through an inv-logit function
y <- rbinom(N,1,pr) # Bernoulli distr. response variable
y[sample(1:N,100)] <- NA

# Input dataset
dataex <- data.frame( 
	NUTS_ID = as.factor(rep(seq(1:10),each=1000)),
	Cell35km = as.factor(rep(paste(35,seq(1:100),sep=""),each=100)),
	Cell6km = as.factor(rep(paste(6,seq(1:1000),sep=""),each=10)),
	obs = y, bio01 = X[,1], bio02 = X[,2], bio23 = X[,3], bio28 = X[,4], wetdays = X[,5], frostdays = X[,6], sampeffort = abs(X[,7])
	)

# Transform in lower cases
colnames(dataex) <- tolower(colnames(dataex))

# Prepare predictors for all the parcels
n_obs <- length(dataex[which(dataex$obs>=0),1])
status <- dataex$obs[which(dataex$obs>=0)]
obs_cells <- dataex$cell6km[which(dataex$obs>=0)]
n_cell_small <- length(levels(dataex$cell6km))
all_cell_small <- dataex$cell6km
weight <- as.numeric(tapply(dataex$sampeffort,list(dataex$cell6km), FUN = max))
all_bio1 <- as.numeric(tapply(dataex$bio01,list(dataex$cell6km), FUN = mean))
all_bio2 <- as.numeric(tapply(dataex$bio02,list(dataex$cell6km), FUN = mean))
n_cell_big <- length(unique(dataex$cell35km))
all_cell_big <- as.numeric(tapply(as.numeric(dataex$cell35km),list(dataex$cell6km), FUN = max))
all_bio23 <- as.numeric(tapply(dataex$bio23,list(dataex$cell35km), FUN = mean))
all_bio28 <- as.numeric(tapply(dataex$bio28,list(dataex$cell35km), FUN = mean))
all_frostdays <- as.numeric(tapply(dataex$frostdays,list(dataex$cell35km), FUN = mean))
all_wetdays <- as.numeric(tapply(dataex$wetdays,list(dataex$cell35km), FUN = mean))
n_district <- length(unique(dataex$nuts_id))
all_district <- as.numeric(tapply(as.numeric(dataex$nuts_id),list(dataex$cell6km), FUN = max))
all_sampeffort <- as.numeric(tapply(dataex$sampeffort,list(dataex$nuts_id), FUN = mean))

# Set prior distributions average 
mu.bio01 <- 0
mu.bio01p <- 0
mu.bio02 <- 0
mu.bio23 <- 0
mu.bio28 <- 0
mu.frostdays <- 0
mu.frostdaysp <- 0
mu.wetdays <- 0
mu.sampeffort <- 5 # Set strong prior
mu.b0 <- 0

# Set prior distributions standard deviation 
sd.bio01 <- 10^2
sd.bio01p <- 10^2
sd.bio02 <- 10^2
sd.bio23 <- 10^2
sd.bio28 <- 10^2
sd.frostdays <- 10^2
sd.wetdays <- 10^2
sd.sampeffort <- 1 # Set strong prior
sd.b0 <- 10^2

# Model structure in JAGS language
cat("model{

	for (i in 1:n_obs) {
		status[i] ~ dbern((guess * (1/2)
		+ (1.0-guess) * ilogit(b.cell_small[obs_cells[i]])))
	}

	for (i in 1:n_cell_small) {
		b.cell_small[i] ~ dnorm(b.cell_small.hat[i],tau.cells_small*weight[i])
		b.cell_small.hat[i] <- b.cell_big[cell_big[i]] + b.bio01*bio01[i] + b.bio01p*bio01[i]^2 + b.bio02*bio02[i]
	}

	b.bio01 ~ dnorm(mu.bio01,1/sd.bio01)
	b.bio01p ~ dnorm(mu.bio01p,1/sd.bio01p)
	b.bio02 ~ dnorm(mu.bio02,1/sd.bio02)
	guess ~ dbeta(1,9)

	for (i in 1:n_cell_big) {
		b.cell_big[i] ~ dnorm(b.cell_big.hat[i],tau.cells_big)
		b.cell_big.hat[i] <- b.district[district[i]] + b.bio23*bio23[i] + b.bio28*bio28[i] + b.frostdays*frostdays[i] + b.wetdays*wetdays[i]
	}

	b.bio23 ~ dnorm(mu.bio23,1/sd.bio23)
	b.bio28 ~ dnorm(mu.bio28,1/sd.bio28)
	b.frostdays ~ dnorm(mu.frostdays,1/sd.frostdays)
	b.wetdays ~ dnorm(mu.wetdays,1/sd.wetdays)

	for (i in 1:n_district) {
		b.district[i] ~ dnorm(b.district.hat[i],tau.district)
		b.district.hat[i] <- b.0 + b.sampeffort*sampeffort[i]
	}

	b.sampeffort~dnorm(mu.sampeffort,1/sd.sampeffort)
	b.0~dnorm(mu.b0,sd.b0)
	tau.cells_small<-pow(sigma.cells_small,-2)
	sigma.cells_small~dlnorm(0,1)

	tau.cells_big<-pow(sigma.cells_big,-2)
	sigma.cells_big~dlnorm(0,1)

	tau.district<-pow(sigma.district,-2)
	sigma.district~dlnorm(0,1)

	for (i in 1:n_cell_small){
		cell_small.probs[i]<-exp(b.cell_small[i])/(1+exp(b.cell_small[i]))
	}
}", fill=TRUE, file="lme_model_abies.txt")

# Bundle data for JAGS
jags_data_pred <- list(
	status = status,
	n_obs = n_obs,
	obs_cells = obs_cells,
	weight = weight+1/mean(weight+1),
	n_cell_small = n_cell_small,
	bio01 = as.numeric(scale(all_bio1)),
	bio02 = as.numeric(scale(all_bio2)),
	cell_big = factor(all_cell_big,levels=unique(all_cell_big),ordered=TRUE),
	n_cell_big = n_cell_big,
	bio23 = as.numeric(scale(all_bio23)),
	bio28 = as.numeric(scale(all_bio28)),
	frostdays = as.numeric(scale(all_frostdays)),
	wetdays = as.numeric(scale(all_wetdays)),
	district = factor(all_district,levels=unique(all_district),ordered=TRUE),
	n_district = n_district,
	sampeffort = as.numeric(scale(all_sampeffort)),
	mu.bio01 = mu.bio01,
	mu.bio01p = mu.bio01p,
	mu.bio02 = mu.bio02,
	mu.bio23 = mu.bio23,
	mu.bio28 = mu.bio28,
	mu.frostdays = mu.frostdays,
	mu.wetdays = mu.wetdays,
	mu.sampeffort = mu.sampeffort,
	mu.b0=mu.b0,
	sd.bio01 = sd.bio01,
	sd.bio02 = sd.bio02,
	sd.bio01p = sd.bio01p,
	sd.bio23 = sd.bio23,
	sd.bio28 = sd.bio28,
	sd.frostdays = sd.frostdays,
	sd.wetdays = sd.wetdays,
	sd.sampeffort = sd.sampeffort,
	sd.b0=sd.b0
	)

# Drop levels (JAGS requires continous numeration of levels)
jags_data_pred$obs_cells <- droplevels(jags_data_pred$obs_cells)
jags_data_pred$obs_cells <- as.integer(jags_data_pred$obs_cells)
jags_data_pred$cell_big <- droplevels(jags_data_pred$cell_big)
jags_data_pred$cell_big <- as.integer(jags_data_pred$cell_big)
jags_data_pred$district <- droplevels(jags_data_pred$district)
jags_data_pred$district <- as.integer(jags_data_pred$district)

# Parameter distributions to track
params_track <- c("b.0","b.bio01","b.bio01p","b.bio01","b.bio02","b.bio23","b.bio28","b.frostdays","b.wetdays","b.sampeffort","tau.cells_small","tau.cells_big","tau.district")

# Run the model
samples_param<-jags.parallel(data=jags_data_pred, parameters.to.save=params_track, model.file="lme_model_abies.txt", n.chains=4, n.iter=10000, DIC=TRUE, digits=3, n.burnin=1000, n.thin=20)

# Run the model for prediction
samples_pred<-jags.parallel(data=jags_data_pred, parameters.to.save=c("cell_small.probs"), model.file="lme_model_abies.txt", n.chains=2, n.iter=100, n.burnin=100, digits=3)

# Check the mcmc
# Traceplot
pdf("output_folder/traceplots_abies.pdf")
traceplot(samples_param[[2]],ask=F)
dev.off()

# Densityplot
pdf("output_folder/densityplot_abies.pdf")
for (i in 1:length(params_track)) {
	temp <- cbind.data.frame(c(samples_param[[2]]$sims.array[,1,i],
		samples_param[[2]]$sims.array[,2,i]),
	as.factor(c(rep("a",length(samples_param[[2]]$sims.array[,1,i])),rep("b",length(samples_param[[2]]$sims.array[,1,i])))))
	names(temp)<-c("value","group")
	print(densityplot(~temp$value,group=temp$group))
}
dev.off()

# Gelman diagram
pdf("output_folder/gelmandiag_abies.pdf")
gelman.plot(samples_param[[2]],ask=F)
dev.off()