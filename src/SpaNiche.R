################################################################################
# 
# SpaNiche.R
# Version 1.0
# 07/03/2015
#
# Takes a fine scale probability map and coarse-scale atlas data and downscales 
# the atlas data to predict area of occupancy at the fine scale. It then
# determines the probability threshold that will give the closest fine-scale
# area of occupancy as the prediction.
#
# Args:
#   poo: raster of the predicted probability of occurrence 
#   pa_data: data frame of presence-absence data with columns "X", "Y" and 
#            "Presence" (1 for presence, 0 for absence)
#   atlas.scale: the cell area at which to make the atlas data (must be multiple
#                of the cell area of the probability map)
#   upgrain.scales: the number of scales to fit the downscale models to
#   divergence.scales: scales to calculate divergence for
#   thresholds: the thresholds to apply to the probability map
#   masking: whether to apply a mask using the atlas data
# Returns:
#   acc_div: Data frame of accuracy (TSS), divergence and distance to the top-
#            left corner of the accuracy-divergence plot for each threshold
#   pa_map: Raster of the presence-absence map applying the Top X threshold
#
################################################################################


SpaNiche <- function(poo, 
                     pa_data, 
                     atlas.scale, 
                     upgrain.scales = 2,
                     divergence.scales,
                     thresholds = seq(0, 1, 0.01),
                     masking = FALSE) {
  require(downscale)
  require(raster)
  require(SDMTools)
  
  fine.scale <- res(poo)[1] ^ 2
  presences <- pa_data[pa_data$Presence == 1, ]
  
  ##############################################################################
  ### Create atlas data
  ##############################################################################
  
  atlas <- aggregate(poo, sqrt(atlas.scale / fine.scale))
  atlas[!is.na(atlas@data@values)] <- 0
  atlas[cellFromXY(atlas, presences[, c("X", "Y")])] <- 1
  
  ##############################################################################
  ###     Downscaling     ######################################################
  ##############################################################################
  
  upgrained <- upgrain(atlas, scales = upgrain.scales, 
                       method = "All_Sampled", plot = FALSE)
  
  ### data storage
  aoo <- matrix(ncol = 2, nrow =length(divergence.scales))
  colnames(aoo) <- c("Area", "AOO")
  aoo[, "Area"] <- divergence.scales
  
  pred <- ensemble.downscale(occupancies = upgrained,
                             new.areas = divergence.scales,
                             models = c("Nachman", "PL", "Logis", "Poisson", 
                                        "NB", "GNB", "INB"),
                             starting_params = list(GNB = list(C = 0.001,
                                                               z = 0.1,
                                                               k = 0.01),
                                                    INB = list(C = 1,
                                                               r = 1,
                                                               b = 2)),
                             plot = FALSE, verbose = FALSE)
  aoo[, "AOO"] <- pred$AOO[, "Means"]
  
  if(masking == TRUE) {
    ############################################################################
    ###     Masking    #########################################################
    ############################################################################
    
    ### make atlas 1km resolution and fit to wallonia
    atlas_mask <- disaggregate(atlas, sqrt(atlas.scale / fine.scale))
    poo_boundary <- poo
    poo_boundary[!is.na(poo_boundary)] <- 1
    atlas_mask <- crop(atlas_mask, poo_boundary)
    atlas_mask <- overlay(atlas_mask, poo_boundary, fun = sum)
    
    ### mask poo map
    poo[atlas_mask == 1] <- 0
  }
  
  ##############################################################################
  ###     Upgraining thresholded maps    #######################################
  ##############################################################################
  
  ### data storage
  aoo.sdm <- matrix(NA, ncol = length(thresholds) + 1,
                    nrow = length(divergence.scales))
  colnames(aoo.sdm) <- c("Area", paste("t", thresholds, sep = "."))
  aoo.sdm[, "Area"] <- divergence.scales
  
  ### loop through thresholds and calculate AOO at all grain sizes
  for(threshs in 1:length(thresholds)) {
    thresh <- thresholds[threshs]
    pred.thresh <- poo
    pred.thresh[pred.thresh >= thresh]  <- 1
    pred.thresh[pred.thresh < thresh]  <- 0
    
    ### convert presences to points
    pred.points <- rasterToPoints(pred.thresh)
    pred.points <- pred.points[pred.points[, 3] == 1, ]
    
    if(length(pred.points) == 0) {next}
    if(length(pred.points) == 3) {
      aoo.sdm[, (threshs + 1)] <- divergence.scales
    }
    if(length(pred.points) > 3) {
      
      ### calculate AOO for all grain sizes
      aoo.sdm[1, (threshs + 1)] <- sum(pred.thresh@data@values > 0, 
                                       na.rm = TRUE) * (res(pred.thresh)[1] ^ 2)
      for(grain in 2:length(divergence.scales)) {  
        r <- raster(ext = extent(atlas),
                    resolution = sqrt(divergence.scales[grain]))
        pred.scale <- rasterize(pred.points[, 1:2], r, fun = "count")
        
        aoo.sdm[grain, (threshs + 1)] <- sum(pred.scale@data@values > 0, 
                                             na.rm = TRUE) *
          (res(pred.scale)[1] ^ 2)
      }
    }
  }
  
  ##############################################################################
  ###     Calculate divergence     #############################################
  ##############################################################################
  
  ### define weights
  pj <- (divergence.scales) / sum((divergence.scales))
  k <- numeric()
  for(i in 2:length(divergence.scales)) {
    k[i - 1] <- divergence.scales[i] /  divergence.scales[i - 1]
  }
  k <- mean(k)
  weight <- pj ^ k / sum(pj ^ k)
  
  ### calculate divergence across thresholds
  div <- data.frame(Thresh = thresholds, Div = NA, Div.stand = NA)
  for(threshs in 1:length(thresholds)) {
    thresh <- thresholds[threshs]
    diff <- log(aoo[1:9, "AOO"]) - log(aoo.sdm[, paste("t", thresh, sep = ".")])
    diff <- diff ^ 2
    div[threshs, "Div"] <- sum(diff * weight)
  }
  div[, "Div"] <- log(div[, "Div"])
  
  ### standardise divergence (best = 1)
  div[, "Div.stand"] <- (div[, "Div"] - min(div[, "Div"], na.rm = TRUE)) / 
    (max(div[, "Div"], na.rm = T) - min(div[, "Div"], na.rm = TRUE))
  thresh.div <- div[which.min(div$Div), "Thresh"] 
  
  ##############################################################################
  ###     Calculate accuracy     ###############################################
  ##############################################################################
  
  accuracy <- data.frame(Thresh = thresholds, TSS = NA, TSS.stand = NA) 
  for(threshs in 1:length(thresholds)) {
    thresh <- thresholds[threshs]
    pred.thresh <- poo
    pred.thresh[pred.thresh >= thresh]  <- 1
    pred.thresh[pred.thresh < thresh]  <- 0
    pred.thresh <- pred.thresh[cellFromXY(pred.thresh, pa_data[, c("X", "Y")])]
    acc <- accuracy(obs = pa_data$Presence,
                    pred = pred.thresh)
    accuracy[threshs, "TSS"] <- acc$sensitivity + acc$specificity - 1
  }
  
  ### standardise TSS (best = 1)
  accuracy[, "TSS.stand"] <- accuracy[, "TSS"] / max(accuracy[, "TSS"])
  thresh.tss <- accuracy[which.max(accuracy$TSS), "Thresh"]
  
  ##############################################################################
  ###     SpaNiche threshold     ###############################################
  ##############################################################################
  
  dists <- data.frame(Thresh = thresholds,
                      Dist = apply(data.frame(Div = div[, "Div.stand", ], 
                                              Acc = accuracy[, "TSS.stand"]),
                                   1, function(x) dist(rbind(c(0, 1), x))))
  thresh.span <- dists[which.min(dists$Dist), "Thresh"]
  
  final.pa <- poo
  final.pa[final.pa >= thresh.span]  <- 1
  final.pa[final.pa < thresh.span]  <- 0
  
  ##############################################################################
  ###       Plotting and outputs      ##########################################
  ##############################################################################
  
  par.original <- par()
  par.original <- list(mfrow = par.original$mfrow, mar = par.original$mar)
  par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
  
  ### Plot OAR for subset of thresholds
  aoo.up <- data.frame(Area = upgrained$occupancy.stand[, "Cell.area"], 
                       AOO = upgrained$occupancy.stand[, "Occupancy"] * 
                         upgrained$extent.stand)
  thresh.plot <- seq(0, 1, 0.05)
  aoo.sdm.plot <- aoo.sdm[, c(1, seq(2, length(thresholds), 5))]
  
  plot(aoo[, "Area"], aoo[, "AOO"], log = "xy", type = "n",
       ylim = c(min(aoo.sdm.plot[, -1]), max(aoo.up[, "AOO"])), 
       xlim = c(min(aoo.sdm.plot[, 1]), max(aoo.up[, "Area"])),
       ylab = "Cell area", xlab = "AOO", main = "Threshold OARs")
  
  apply(aoo.sdm.plot[, -1], 2, function(x) points(aoo.sdm.plot[, "Area"], x))
  apply(aoo.sdm.plot[, -1], 2, 
        function(x) lines(aoo.sdm.plot[, "Area"], x, lwd = 1.5))
  points(aoo[, "Area"], aoo[, "AOO"], col = "red", cex = 1.5, pch = 16)
  lines(aoo[, "Area"], aoo[, "AOO"], lwd = 3, col = "red")
  points(aoo.up[, "Area"], aoo.up[, "AOO"], col = "blue", cex = 1.5, pch = 16)
  lines(aoo.up[, "Area"], aoo.up[, "AOO"], lwd = 3, col = "blue")
  text(rep(1e6, length(thresh.plot)), aoo.sdm.plot[1, -1], 
       labels = thresh.plot, cex = 0.7, pos = 2)
  
  ### Plot accuracy-threshold plot
  plot(accuracy[, "TSS"] ~ accuracy[, "Thresh"], type = "b", lwd = 1, 
       xlab = "Threshold", ylab = "Accuracy", col = "blue",
       main = "Niche consistency")
  points(accuracy[accuracy$Thresh == thresh.div, "Thresh"], 
         accuracy[accuracy$Thresh == thresh.div, "TSS"], 
         col = "dark green", pch = 16, cex = 1.5)
  points(accuracy[accuracy$Thresh == thresh.tss, "Thresh"], 
         accuracy[accuracy$Thresh == thresh.tss, "TSS"], 
         col = "blue", pch = 16, cex = 1.5)
  points(accuracy[accuracy$Thresh == thresh.span, "Thresh"], 
         accuracy[accuracy$Thresh == thresh.span, "TSS"], 
         col = "red", pch = 16, cex = 1.5)
  
  ### Plot divergence-threshold plot
  plot(div[, "Div"] ~ div[, "Thresh"], type = "b", lwd = 1,
       ylab = "Divergence", xlab = "Threshold", col = "dark green",
       main = "Spatial consistency")
  points(div[div$Thresh == thresh.div, "Thresh"], 
         div[div$Thresh == thresh.div, "Div"], 
         col = "dark green", pch = 16, cex = 1.5)
  points(div[div$Thresh == thresh.tss, "Thresh"], 
         div[div$Thresh == thresh.tss, "Div"], 
         col = "blue", pch = 16, cex = 1.5)
  points(div[div$Thresh == thresh.span, "Thresh"], 
         div[div$Thresh == thresh.span, "Div"], 
         col = "red", pch = 16, cex = 1.5)
  
  ### Plot accuracy-divergence plot
  plot(div[, "Div.stand"], accuracy[, "TSS.stand"], 
       xlab = "Divergence", ylab = "Accuracy", main= "Consistency trade-off plot",
       asp = 1, type = "b", col = "red", lwd = 1)
  points(div[div$Thresh == thresh.div, "Div.stand"], 
         accuracy[div$Thresh == thresh.div, "TSS.stand"], 
         col = "dark green", pch = 16, cex = 1.5)
  points(div[div$Thresh == thresh.tss, "Div.stand"], 
         accuracy[div$Thresh == thresh.tss, "TSS.stand"], 
         col = "blue", pch = 16, cex = 1.5)
  points(div[div$Thresh == thresh.span, "Div.stand"], 
         accuracy[div$Thresh == thresh.span, "TSS.stand"], 
         col = "red", pch = 16, cex = 1.5)
  
  ### maps
  plot(poo, zlim = c(0, 1), colNA = "dark grey", main = "PoO map")
  plot(final.pa, colNA = "dark grey", legend = FALSE,
       main = "PA map")
  
  par(mfrow = par.original$mfrow, mar = par.original$mar)
  
  ###
  output <- list(acc_div = data.frame(Threshold = thresholds,
                                      Accuracy = accuracy[, "TSS"],
                                      Accuracy.stand = accuracy[, "TSS.stand"],
                                      Divergence = div[, "Div.stand"],
                                      Dists = dists[, "Dist"]),
                 pa_map = final.pa)
  return(output)
}

