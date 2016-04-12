################################################################################
# 
# topX.R
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
#   thresholds: the thresholds to apply to the probability map
# Returns:
#   poo_mask: Raster of the original probability of occurrence map after masking
#   pa_map: Raster of the presence-absence map applying the Top X threshold
#   pa_map_mask: Raster of the presence-absence map applying the Top X threshold
#                after masking
#
################################################################################


TopX <- function(poo, 
                 pa_data, 
                 atlas.scale, 
                 upgrain.scales = 2, 
                 thresholds = seq(0, 1, 0.01)) {
  require(downscale)
  require(raster)
  
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
  pred <- ensemble.downscale(occupancies = upgrained,
                             new.areas = fine.scale,
                             models = c("Nachman", "PL", "Logis", "Poisson", 
                                        "NB", "GNB", "INB"),
                             starting_params = list(GNB = list(C = 0.001,
                                                               z = 0.1,
                                                               k = 0.01),
                                                    INB = list(C = 1,
                                                               r = 1,
                                                               b = 2)),
                             plot = FALSE, verbose = FALSE)
  aoo <- pred$AOO[, "Means"]
  
  ##############################################################################
  ###     Top X     ############################################################
  ##############################################################################
  
  ### Loop through threshold and calculate aoo
  cells <- data.frame(Thresh = thresholds, AOO = NA)
  for(threshs in 1:length(thresholds)) {
    thresh <- thresholds[threshs]
    pred.thresh <- poo
    pred.thresh[pred.thresh >= thresh]  <- 1
    pred.thresh[pred.thresh < thresh]  <- 0
    cells[threshs, "No.cells"] <- sum(pred.thresh@data@values == 1, na.rm =TRUE)
    cells[threshs, "AOO"] <- sum(pred.thresh@data@values== 1, na.rm = TRUE) * 
      (res(poo)[1] ^ 2)
  }
  
  ### difference between downscale prediction and thresholded aoo
  diff.cell <- abs(round(aoo - cells[, "AOO"]))
  thresh.topx <- cells[which.min(diff.cell), "Thresh"]
  
  final.pa <- poo
  final.pa[final.pa >= thresh.topx]  <- 1
  final.pa[final.pa < thresh.topx]  <- 0
  
  ##############################################################################
  ###     Masking    ###########################################################
  ##############################################################################
  
  ### make atlas 1km resolution and fit to wallonia
  atlas_mask <- disaggregate(atlas, sqrt(atlas.scale / fine.scale))
  poo_boundary <- poo
  poo_boundary[!is.na(poo_boundary)] <- 1
  atlas_mask <- crop(atlas_mask, poo_boundary)
  atlas_mask <- overlay(atlas_mask, poo_boundary, fun = sum)
  
  ### mask poo map
  poo_mask <- poo
  poo_mask[atlas_mask == 1] <- 0
  
  ##############################################################################
  ###     Top X masked    ######################################################
  ##############################################################################
  
  ### Loop through threshold and calculate aoo
  cells <- data.frame(Thresh = thresholds, AOO = NA)
  for(threshs in 1:length(thresholds)) {
    thresh <- thresholds[threshs]
    pred.thresh <- poo_mask
    pred.thresh[pred.thresh >= thresh]  <- 1
    pred.thresh[pred.thresh < thresh]  <- 0
    cells[threshs, "No.cells"] <- sum(pred.thresh@data@values == 1, na.rm =TRUE)
    cells[threshs, "AOO"] <- sum(pred.thresh@data@values== 1, na.rm = TRUE) * 
      (res(poo)[1] ^ 2)
  }
  
  ### difference between downscale prediction and thresholded aoo
  diff.cell <- abs(round(aoo - cells[, "AOO"]))
  thresh.topx.mask <- cells[which.min(diff.cell), "Thresh"]
  
  final.pa.mask <- poo
  final.pa.mask[final.pa.mask >= thresh.topx.mask]  <- 1
  final.pa.mask[final.pa.mask < thresh.topx.mask]  <- 0
  
  ##############################################################################
  ###       Plotting and outputs      ##########################################
  ##############################################################################
  
  par.original <- par()
  par.original <- list(mfrow = par.original$mfrow, mar = par.original$mar)
  
  par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
  plot(poo, zlim = c(0, 1), colNA = "dark grey", main = "PoO map")
  plot(poo_mask, zlim = c(0, 1), colNA = "dark grey", main = "PoO map - masked")
  plot(final.pa, colNA = "dark grey", legend = FALSE,
       main = "PA map")
  plot(final.pa.mask, colNA = "dark grey", legend = FALSE,
       main = "PA map - masked")
  
  par(mfrow = par.original$mfrow, mar = par.original$mar)
  
  ###
  output <- list(poo_mask = poo_mask,
                 pa_map = final.pa,
                 pa_map_mask = final.pa.mask)
  return(output)
}


