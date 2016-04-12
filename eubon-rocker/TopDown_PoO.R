

###########################################################################
#           Name:                                                         #                                             
# TopDown_PoO.R                                                           #
#                                                                         #
#           Aim:                                                          #
# to select sites in which a species occurs based on both probabilities   #
# ofoccurence and proportion of occupied cells at various scales          #     
#                                                                         # 
#           Input:                                                        # 
# Stack           - rasterstack with 3 raster layers in the following     # 
#                   order:                                                #
#                   1. IDs for each cell                                  #
#                   2. presence data in some cells, that will be used     #
#                      to create atlas data. For this layer, values of    #
#                      1 are asumed to be presences, and all other        #
#                      values areabsence. NAs are allowd.                 #      
#                   3. the Probability of occurence (PoO) in all cells.   # 
#                      NAs for all other cells.                           #
# scales          - Integer, the number of scales to upgrain in the       # 
#                   downscale::upgrain function                           #
# FUN             - charachther, the function by which to aggreage the    #
#                   PoO to larger grain sizes. either:                    # 
#                   c("mean", "median", "Quan_75"). For Quan_75, the 75   # 
#                    quantelie is used.                                   #
# Atlas           - Integer, the number of scales to use in the downscale #
#                   function. minimum is 3.                               #
# Mask            - Logical, if TRUE, all P/A will be within the observed # 
#                   atlas distribution.                                   #     
# Plot            - Logical, if TRUE, varioues plots will be generated.   #
# models          - vector of chosen downscaling models. see package      # 
#                   downscale for a list of availble models.If set to     #
#                   "all" or if the vector is longer than 1,              #
# method          - the upgraining method to use. See                     #
#                   downscale::upgrain for details                        #
#                   ensemble.downscale is used.                           #
# tolerance_mod   - Numeric, see downscale::ensemble.downscale            #
# tolerance_pred  - Numeric, see downscale::ensemble.downscale            #
# tolerance_hui   - Numeric, see downscale::ensemble.downscale            #
# starting_params - List, see downscale::ensemble.downscale or            # 
#                   downscale::downscale for details.                     # 
# verbose         - Logical, if TRUE, progress is printed in the console. #                                         
#                                                                         #
#           Main algorithm:                                               #
# 1. use the upgrain function of the package 'downscale' to create atlas  #
#    data at various scales.                                              #              
# 2. fit several downscaling models to the atlas data and save the        # 
#    predicted area of occurence.                                         #
# 3. Start with the largest grain size of the atlas data:                 #
#     3.a- calcualte the mean PoO in each large grain size.               #
#     3.b- select the top n cells, n being the number of occupied cells   # 
#          at this scale based on the downscaling models.                 # 
#     3.c- In each selected cells, identify the fine grain cell with the  # 
#          highest PoO and mark it as occupied.                           #
#     3.d- Update the P/A status of all larger grain sizes.               #
# 4. Continue with the second largest grain:                              #
#     4.a- Calcualte the number of currently occupied cells.              #
#     4.b- get the predicted number of occupied cells from the            # 
#          downscaling model.                                             #
#     4.c- the difference between b and a is the number of new ocupied    #
#          cells that should be added at this scale.                      #
#     4.d- identify the list of potential new cells at this scale - not   #
#          already occupied at this scale + occupied at the scale above.  #
#     4.e- for each potential new occupied cell, calucalte the mean PoO.  #
#     4.f- select the top n cells with n being the output of 4.c          #
#     4.6- witin each of the n cells, identify the unoccpupied fine grain #
#          cell with the highest PoO and mark it as occupied.             #
#     4.7- Update the P/A status of all larger grain sizes.               #
# 5. repeat step 4 for all scales from larger to smaller. the result is a # 
#    fully spatially consistent model, in most cases, with the exact      #
#    predicted prevelence in all scales. In some cases, their are not     #
#    enough suitable scales at certain scales, such that the number of    #
#    occupied cells at this scales are lower than predicted by the        # 
#    downscale models.                                                    #
#                                                                         #
#           Output:                                                       #
# a list containing:                                                      #
# Stack          - Rasterstack, the two original rasterlayer + the mean   #
#                  PoO and the predicted P/A at all scales - all have     # 
#                  grain size as in the original rasterstack.             #
# Data           - Dataframe,  info on all Cells. contains the following: #
#         ID                - The Id from the first input raster layer    #
#         X, Y              - x,y coordinate of each cell                 #
#         ID_0, ID_1, ...   - The new IDs assign for each cell in each    # 
#                             scale following the upgraining procedure    #
#         PoO_0, PoO_1, ... - The mean PoO at each cell in each scale     #
#         PA_0, PA_1, ...   - The predicted occupancy for each cell at    # 
#                             each scale                                  #
# DownHyb - Dataframe, the extended downscale dataframe.                  #
# UpGrained      - Object of class upgrain, the output of the             # 
#                  downscale::upgrain function.                           #  
# DownScaled     - Object of class downscale, the output of the           #
#                  downscale::downscale function.                         #
#                                                                         #
#          Additional information:                                        #
# Author:  Yoni Gavish <gavishyoni@gmail.com>                             #
# Date:    23 Feb. 2016                                                   #
###########################################################################

TopDown_PoO = function(Stack,              
                       scales,  
                       FUN             = "mean",
                       Atlas           = 3,      
                       Mask            = TRUE,  
                       Plot            = TRUE,  
                       models          = "all", 
                       method          = "All_Sampled",
                       tolerance_mod   = 1e-2,   
                       tolerance_pred  = 1e-2,   
                       tolerance_hui   = 1e-6,   
                       starting_params = NULL,   
                       verbose         = TRUE)   
{
  require(downscale)
  require(raster)
  require(SDMTools)

  ###-------------------------------------------------------------------###
  ### checks -----------------------------------------------------------###
  ###-------------------------------------------------------------------###
  
  if(!is.logical(verbose)){
    cat(paste("verbose should be logical --> default=T --> "))
    verbose = TRUE}
  
  if(verbose){
    cat(paste("\n##---------------------------------------------##\n"))}
  if(verbose){
    cat(paste("Performing checks       --> "))}
  
  if(as.integer(scales) != scales){
    stop(paste("scales should be an integer >= 3\n"))}
  
  if(as.integer(Atlas) != Atlas){
    stop(paste("Atlas should be an integer >= 3\n"))}
  
  if(!is.logical(Plot)){
    cat(paste("Plot should be logical --> setting to default=T --> "))
    Plot = TRUE}
  
  if(!is.logical(Mask)){
    cat(paste("Mask should be logical --> setting to default=T --> "))
    Use_Atlas = TRUE}
  
  # decide if ensemble or single model is to be used    
  if (length(models) == 1){
    if (models == "all") {Ensemble= TRUE}
    if (models != "all") {Ensemble= FALSE}}
  if (length(models) > 1) {Ensemble = TRUE}
  if(verbose){
    cat(paste("done\n"))}
  
  ###-------------------------------------------------------------------###
  ### preparing raster -------------------------------------------------###
  ###-------------------------------------------------------------------###
  
  if(verbose){
    cat(paste("Preparing extent raster --> "))}
  # subst the input raster
  ID_Orig_Raster  <- subset(Stack, 1)
  Presence_Raster <- subset(Stack, 2)
  PoO_Raster      <- subset(Stack, 3)
  
  # change NaN to NA if needed
  ID_Orig_Raster[is.nan(ID_Orig_Raster)]   <- NA
  Presence_Raster[is.nan(Presence_Raster)] <- NA
  PoO_Raster[is.nan(PoO_Raster)]           <- NA
  
  # get the values
  New_values <- values(PoO_Raster)
  New_values[!is.na(New_values)]           <- 0
  New_values[values(Presence_Raster) == 1] <- 1
  Upgrain_Raster <- setValues(x= PoO_Raster, values = New_values)
  rm(New_values)
  
  if(verbose){
    cat(paste("done\n"))}
  
  ###-------------------------------------------------------------------###
  ### upgraining -------------------------------------------------------###
  ###-------------------------------------------------------------------###
  
  if(verbose){cat(paste("Upgraining              --> "))}
  UpGrained <- upgrain(Upgrain_Raster, 
                       cell.width = NULL, 
                       scales = scales, 
                       threshold = NULL,
                       method =  method,
                       plot = Plot)
  
  DownHyb <- merge(UpGrained$occupancy.stand, 
                          UpGrained$occupancy.orig, 
                          by="Cell.area")
  names(DownHyb)[2:5] <- c("Extent.stand", 
                           "Occ.stand", 
                           "Extent.orig", 
                           "Occ.orig")
  DownHyb$ncells_Obs <- (DownHyb$Occ.stand * 
                         DownHyb$Extent.stand) /  
                         DownHyb$Cell.area
  DownHyb$Scale <- c(0:scales)
  
  Extent_0           <- UpGrained$atlas.raster.stand
  Vals               <- values(Extent_0)
  Vals[!is.na(Vals)] <- 1:sum(!is.na(Vals))
  Extent_0_ID        <- setValues(Extent_0, Vals)
  rm(Vals)
  
  for (i in 1:scales){
    New_raster <-aggregate(get(paste("Extent_",i-1,sep="")), 
                           2, 
                           fun = max)
    assign(paste("Extent_",i,sep=""), New_raster)
    Vals               <- values(New_raster)
    Vals[!is.na(Vals)] <- 1:sum(!is.na(Vals))
    New_raster_ID      <- setValues(New_raster, Vals) 
    assign(paste("Extent_",i,"_ID",sep=""), New_raster_ID)
    rm(New_raster, New_raster_ID, Vals)
  }
  rm(i)
  if(verbose){
    cat(paste("done\n"))}
  
  ###-------------------------------------------------------------------###
  ### downscaling ------------------------------------------------------###
  ###-------------------------------------------------------------------###
  
  if(verbose){
    cat(paste("downscaling             --> "))}
  
  
  # the upgrain object with the atlas scale as the smallest grain
  UpGrained2 <- upgrain(get(paste("Extent_", 
                                  scales - Atlas + 1, sep = "")), 
                        cell.width = NULL, 
                        scales = Atlas -1, 
                        threshold = NULL,
                        method = method,
                        plot = FALSE) 
  
  occupancy.stand <- UpGrained$occupancy.stand
  
  if(Ensemble){
    if(verbose){cat(paste("calling ensemble.downscale function:\n\n"))}
    DownScaled <- ensemble.downscale(
      occupancies     = UpGrained2, 
      new.areas       = as.vector(occupancy.stand[, 1]), 
      extent          = UpGrained$extent.stand, 
      cell.width      = sqrt(UpGrained2$occupancy.stand[1, 1]), 
      models          = models, 
      tolerance_mod   = tolerance_mod, 
      tolerance_pred  = tolerance_pred,
      tolerance_hui   = tolerance_hui, 
      starting_params = starting_params, 
      plot            = Plot, 
      verbose         = verbose) 
    # arrange Downscale_Out
    DownScaled_Occ <- DownScaled$Occupancy
    DownScaled_AOO <- DownScaled$AOO
    # set names
    names(DownScaled_Occ)[2:ncol(DownScaled_Occ)] <- 
         paste(names(DownScaled_Occ)[2:ncol(DownScaled_Occ)], 
               "_Occ", sep="")
    names(DownScaled_AOO)[2:ncol(DownScaled_AOO)] <- 
         paste(names(DownScaled_AOO)[2:ncol(DownScaled_AOO)], 
               "_AOO", sep="")
    # merge
    DownHyb <- merge(DownHyb, 
                            DownScaled_Occ, 
                            by = names(DownScaled_Occ)[1])
    DownHyb <- merge(DownHyb, 
                            DownScaled_AOO, 
                            by = names(DownScaled_Occ)[1])
    
    # number of cells, rounded and ensure monotonically increasing 
    # with decreased grain
    DownHyb[, "ncells_Down"] <- DownHyb[, "Means_AOO"] / 
                                       DownHyb[, "Cell.area"]
    rm(DownScaled_Occ, DownScaled_AOO)
  } # end ensemble if
  
  if(!Ensemble){
    if(models!="Hui"){
      # arrange the starting params
      if(!is.null(starting_params)){
        if(models %in% names(starting_params)){ 
          starting_params <- starting_params[[models]]}
        if(!(models %in% names(starting_params))){ 
          model_list <- c("Nachman", "PL", "Logis", "Poisson", 
                          "NB", "GNB", "INB", "FNB", "Thomas", "Hui")
          if(any(model_list%in% names(starting_params))){ 
            starting_params <- NULL} 
          rm(model_list)
        }
      }
      
      if(verbose){
        cat(paste("calling downscale function:\n\n"))}
      if(verbose){
        cat(paste(models, " model is running...  "))}
    
      DownScaled <- downscale(
        occupancies     = UpGrained2, 
        model           = models, 
        extent          = UpGrained$extent.stand, 
        tolerance       = tolerance_mod,
        starting_params = starting_params)
      # predicted values
      DownScaled_pred <- predict(
        DownScaled,
        new.areas  = as.vector(occupancy.stand[, 1]),
        tolerence  = tolerance_pred,
        plot       = Plot)
      DownScaled_pred <- DownScaled_pred$predicted   
      # set names
      names(DownScaled_pred)[2] <- paste(models, "_Occ", sep="")
      names(DownScaled_pred)[3] <- paste(models, "_AOO", sep="")
      # arrange Downscale_Out
      DownHyb <- merge(DownHyb, 
                              DownScaled_pred, 
                              by = names(DownHyb)[1])
      # number of cells, rounded and ensure monotonically increasing 
      # with decreased grain
      DownHyb[, "ncells_Down"] <- 
            DownHyb[, paste(models, "_AOO", sep="")] / 
            DownHyb[, "Cell.area"]
      rm(DownScaled_pred)
      if(verbose){cat(paste("complete\n"))}
    } # end !Hui if
    
    if(models=="Hui"){
      if(verbose){cat(paste("calling hui.downscale function:\n\n"))}
      if(verbose){cat(paste(models, " model is running...  "))}
      DownScaled <- hui.downscale(
            atlas.data = UpGrained2, 
            cell.width = sqrt(UpGrained2$occupancy.stand[1, 1]), 
            new.areas  = as.vector(head(occupancy.stand, 
                                        scales-Atlas +1)[,1]), 
            extent     = UpGrained$extent.stand,
            tolerance  = tolerance_hui, 
            plot       = Plot) 
      
      DownScaled_pred <- DownScaled$predicted
      DownHyb  <- DownHyb
      DownHyb[1:nrow(DownScaled_pred), c("Hui_Occ")] <- 
            DownScaled_pred[, 2]  
      DownHyb[1:nrow(DownScaled_pred), c("Hui_AOO")] <- 
            DownScaled_pred[, 3]  
      if(!Mask){
        Down2          <- DownHyb
        DownHyb <- DownHyb[1:nrow(DownScaled_pred), ]
      }
      
      # number of cells, rounded and ensure monotonically increasing 
      # with decreased grain
      DownHyb[, "ncells_Down"] <- 
             DownHyb[, paste(models, "_AOO", sep="")] / 
             DownHyb[, "Cell.area"]
      if(verbose){cat(paste("complete\n"))}  
    } # end Hui if
  } # end !ensemble if
  
  if(!Mask){
    DownHyb[, "ncells_Aim"]  <- 
             rev(cummax(rev(round(DownHyb[, "ncells_Down"]))))}  
  if(Mask){
    if(Ensemble){
      NCells <- c(round(DownHyb[1:(scales-Atlas+1), 
                                       "ncells_Down"]),
                  round(DownHyb[(scales-Atlas+2):(scales+1), 
                                       "ncells_Obs"]))
      DownHyb[, "ncells_Aim"] <- rev(cummax(rev(NCells)))}
    if(!Ensemble){
      if(models!="Hui"){
        NCells <- c(round(DownHyb[1:(scales-Atlas+1), 
                                         "ncells_Down"]),
                    round(DownHyb[(scales-Atlas+2):(scales+1), 
                                         "ncells_Obs"]))
        DownHyb[, "ncells_Aim"] <- rev(cummax(rev(NCells)))
      }
      if(models=="Hui"){
        NCells <- round(DownHyb[1:(scales-Atlas+1), 
                                       "ncells_Down"])
        DownHyb[1:nrow(DownScaled_pred), "ncells_Aim"] <-
               rev(cummax(rev(NCells)))
        rm(DownScaled_pred)
      }
    }
    rm(NCells)
  }
  
  ###-------------------------------------------------------------------###
  ### Data -------------------------------------------------------------###
  ###-------------------------------------------------------------------###
  
  
  if(verbose){
    cat(paste("\nArranging the Data dataframe  --> "))}
  #if(verbose){
  #  cat(paste("extracting data from PoO rasrer --> "))}
  
  XY_Data <- xyFromCell(PoO_Raster, 1:ncell(PoO_Raster))
  Data <- data.frame(ID    = values(ID_Orig_Raster),
                     X     = XY_Data[,1],
                     Y     = XY_Data[,2],
                     PoO_0 = values(PoO_Raster),
                     PA_0  = NA,
                     ID_0  = extract(Extent_0_ID, XY_Data))
  
  Data[!is.na(Data$PoO_0), "PA_0"] = 0
  #if(verbose){
  #  cat(paste("extracting new IDs from upgrained rasters --> "))}
  
  for (i in 1:(nrow(DownHyb)-1)){
    ID_ras <- get(paste("Extent_",i,"_ID",sep=""))
    Data[, paste("ID_", i, sep="")] <- extract(ID_ras, XY_Data)
    rm(ID_ras)
  }
  
  if(FUN =="mean"){
    # Adding the mean PoO at each grain size
    for (i in 1:(nrow(DownHyb)-1)){
      Mean_PoO <- aggregate(x  = Data$PoO_0,
                            by = list(Data[, paste("ID_", i, sep="")]),
                            FUN=mean,
                            na.rm= TRUE)
      names(Mean_PoO)[1] = "ID"
      names(Mean_PoO)[2] = "PoO"
      for(k in 1: nrow(Mean_PoO)){
        CELLS <- which(Data[, paste("ID_", i, sep="")] == 
                         Mean_PoO[k, "ID"])
        Data[CELLS, paste("PoO_", i, sep="")] <- Mean_PoO[k, "PoO"]
        rm(CELLS)}
      rm(Mean_PoO)
    }}
  
  if(FUN =="median"){
    # Adding the median PoO at each grain size
    for ( i in 1:(nrow(DownHyb)-1)){
      Mean_PoO <- aggregate(x  = Data$PoO_0,
                            by = list(Data[, paste("ID_", i, sep="")]),
                            FUN=median,
                            na.rm= TRUE)
      names(Mean_PoO)[1] = "ID"
      names(Mean_PoO)[2] = "PoO"
      for(k in 1: nrow(Mean_PoO)){
        CELLS <- which(Data[, paste("ID_", i, sep="")] == 
                         Mean_PoO[k, "ID"])
        Data[CELLS, paste("PoO_", i, sep="")] <- Mean_PoO[k, "PoO"]
        rm(CELLS)}
      rm(Mean_PoO)
    }}
  
  if(FUN =="Quan_75"){
    #Adding the 75 quantile PoO at each grain size
    Quan_75 = function(x){quantile(x, probs = 0.75, na.rm=TRUE)}
    for ( i in 1:(nrow(DownHyb)-1)){
      Mean_PoO <- aggregate(x  = Data$PoO_0,
                            by = list(Data[, paste("ID_", i, sep="")]),
                            FUN=Quan_75)
      names(Mean_PoO)[1] = "ID"
      names(Mean_PoO)[2] = "PoO"
      for(k in 1: nrow(Mean_PoO)){
        CELLS <- which(Data[, paste("ID_", i, sep="")] == 
                         Mean_PoO[k, "ID"])
        Data[CELLS, paste("PoO_", i, sep="")] <- Mean_PoO[k, "PoO"]
        rm(CELLS)}
      rm(Mean_PoO)
    }}
  
  ## add all the PA columns 
  for (i in 1:(nrow(DownHyb)-1)){
    Data[, paste("PA_", i, sep="")] <- Data$PA_0}
  rm(i)
  
  # reorder
  Ordered <- c("ID", "X", "Y")
  for (i in 0:(nrow(DownHyb)-1)){
    Ordered <- c(Ordered, paste("ID_", i, sep=""))}
  for (i in 0:(nrow(DownHyb)-1)){
    Ordered <- c(Ordered, paste("PoO_", i, sep=""))}
  for (i in 0:(nrow(DownHyb)-1)){
    Ordered <- c(Ordered, paste("PA_", i, sep=""))}
  Data <- Data[, Ordered]
  rm(Ordered, i)
  
  if(Mask){
    # row number in DownHyb that the hybrid model will start from 
    START <- scales - Atlas
    for (N in (START+1):scales){
      if(N>nrow(DownHyb)){break}
      # PA and ID
      Atlas_PA <- values(get(paste("Extent_", N , sep="")))       
      Atlas_ID <- values(get(paste("Extent_", N, "_ID", sep=""))) 
      Atlas1 <- data.frame(ID = Atlas_ID, PA= Atlas_PA) 
      # remove all NAs and keep only occupied cells
      Atlas1 <- Atlas1[!is.na(Atlas1$ID), ] 
      Atlas1 <- Atlas1[Atlas1$PA==1, ]
      DownHyb[N+1, "Presences"] <- 0
      DownHyb[N+1, "Available"] <- 0
      DownHyb[N+1, "Needed"]    <- NA
      DownHyb[N+1, "Added"]     <- 0
      
      for (k in 1:nrow(Atlas1)){
        Foc_ID <- Atlas1[k, "ID"]    # the focal ID
        # column number in Data with the IDs at scale N
        Col_ID <- match(paste("ID_", N, sep=""), names(Data)) 
        # column number in Data with the PAs at scale N
        Col_PA <- match(paste("PA_", N, sep=""), names(Data))
        # update occurence
        Data[which(Data[, Col_ID] == Foc_ID), Col_PA] <- 1 
        rm(Foc_ID, Col_ID, Col_PA)            
      }
      rm(Atlas_PA, Atlas_ID, Atlas1)
    }
    rm(N, k) 
  }
  
  if(verbose){
    cat(paste("done \n"))}
  
  ###-------------------------------------------------------------------###
  ### start coarse scale for no mask -----------------------------------###
  ###-------------------------------------------------------------------###
  
  if(!Mask){
    # start at the largest grain size
    if(verbose){
      cat(paste("\nNo mask: intializing coarse scale distribution:\n"))}
    
    i <- nrow(DownHyb)-1
   
    N_Cells <- DownHyb[i+1, "ncells_Aim"]
    Foc_PA  <- paste("PA_", i, sep="")
    Foc_ID  <- paste("ID_", i, sep="")
    Foc_PoO <- paste("PoO_", i, sep="")
    
    if(verbose){cat(paste("     Scale= ", i, 
                         sep=""))}
    # the number of cells at the grain size already occupied
    P_Cells   <- 0
    DownHyb[i+1, "Presences"] <- P_Cells
    DownHyb[i+1, "Needed"]    <- N_Cells
    
    # remove NAs
    All_ID        <- Data[!is.na(Data[, Foc_ID]), ] 
    # remove NAs at local scale
    All_ID        <- All_ID[!is.na(All_ID$PA_0),]   
    # get the unique ID in Scale i in which new presences can be added
    All_ID        <- unique(All_ID[, Foc_ID]) 
    # remove NA from unique
    All_ID        <- as.data.frame(All_ID[!is.na(All_ID)]) 
    # match the mean PoO
    All_ID$PoO    <- Data[match(All_ID[,1], Data[, Foc_ID]), Foc_PoO] 
    names(All_ID) <- c("ID", "PoO")
    # Order in decreasing PoO
    All_ID        <- All_ID[order(-All_ID$PoO),]                                
    All_ID$PA_ID  <- NA
    # number of available cells
    DownHyb[i + 1, "Available"]   <- nrow(All_ID)                      
    
    ###-----------------------------------------------------------------###
    # within each coarse scale cell, the local cells with the highest PoO #
    
    k <- 1
    j <- 1
    while(j<= N_Cells){
      # are there additional cells to consider?
      if (k> nrow(All_ID)){break}
      # subset all potential cells
      Temp1 <- Data[which(Data[, Foc_ID] == All_ID[k, "ID"]), ]
      # are there potantial cells left in cell k?
      if(sum(Temp1$PA_0, na.rm=TRUE) == sum(!is.na(Temp1$PA_0))){
        k = k+1 
        next
      }
      # remove NAs
      Temp1 <- Temp1[!is.na( Temp1[,"PA_0"]), ]
      # remove occupied cells
      Temp1 <- Temp1[Temp1[, "PA_0"]!= 1,]
      # identify the cell with the highest local PoO
      PA0_ID <- Temp1[which(Temp1[, "PoO_0"]== 
                              max(Temp1[, "PoO_0"])), "ID_0"]
      if(length( PA0_ID)>1){PA0_ID <- sample(PA0_ID, 1)  }
      All_ID[k, "PA_ID"] <- PA0_ID
      
      Data[which(Data[, "ID_0"]==PA0_ID), "PA_0"] <- 1
      
      k <- k + 1
      j <- j + 1
      rm(Temp1, PA0_ID)
    }
    rm(j, k) 
    All_ID <- All_ID[!is.na(All_ID$PA_ID),]
    
    ###-----------------------------------------------------------------###
    # Update occupancy at all scales                                      #
    
    # update all other scales to present
    for (j in 1:nrow(All_ID)){
      for (k in 1:i){  
        Loc_k <- Data[which(Data[, "ID_0"] == All_ID[j, "PA_ID"]), 
                      paste("ID_", k, sep="")]
        Data[which(Data[,  paste("ID_", k, sep="")] == Loc_k), 
             paste("PA_", k, sep="")] <- 1
        Data[is.na(Data$PA_0), 
             paste("PA_", k, sep="")] <- NA
        rm(Loc_k) 
      } 
      rm(k)
    }
    
    ###-----------------------------------------------------------------###
    # update DownHyb                                                      #
    DownHyb[i + 1, "Added"]   <- nrow(All_ID)
    START <- i-1
    rm(i, N_Cells, Foc_PA, Foc_ID, Foc_PoO, P_Cells, All_ID, j)
    if(verbose){cat(paste(" --> done \n"))}  
  }
  
  ###-------------------------------------------------------------------###
  ### The main hybrid loop ---------------------------------------------###
  ###-------------------------------------------------------------------###
  
  if(verbose){
    cat(paste("Assigning presences:\n"))}
  
  for (i in START:0){
    ###-----------------------------------------------------------------###
    # identify needs at the scale                                         #
    
    # number of occupied cells in the scale
    N_Cells <- DownHyb[i + 1, "ncells_Aim"]     
    Foc_PA  <- paste("PA_", i, sep="")         # focal PA in Data
    Foc_ID  <- paste("ID_", i, sep="")         # Focal ID in Data
    if(verbose){cat(paste("     Scale= ", i, 
                          sep=""))}
    
    # the number of cells at the grain size already occupied
    P_Cells   <- length(unique(Data[which(Data[, Foc_PA] == 1), Foc_ID]))
    # number of cells that need to be added
    Add_Cells <- N_Cells - P_Cells                                                 
    DownHyb[i + 1, "Presences"] <- P_Cells
    DownHyb[i + 1, "Needed"]   <- Add_Cells

    if(Add_Cells == 0){
      DownHyb[i + 1, "Available"] <- NA
      DownHyb[i + 1, "Added"]     <- 0
    }
    
    if(Add_Cells >0){
      Foc_PoO   <- paste("PoO_", i, sep="")
      Foc_PA_Up <- paste("PA_", i + 1, sep="")
      Foc_ID_Up <- paste("ID_", i + 1, sep="")
      New_add   <- New_add2 <- data.frame(ID_0= rep(NA,0))
      New_Cells <- Add_Cells
      # remove NA at the larger scales
      All_ID    <- Data[!is.na(Data[, Foc_PA_Up]), ] 
      # remove absences at the larger scales
      All_ID    <- All_ID[All_ID[, Foc_PA_Up] == 1, ]          
      
      ###---------------------------------------------------------------###
      # for each occupied cells one scale above the focal scale           #
      # 1. check if there is at least one fine scale cell occupied        #
      # 2. if not, select the one with the highest PoO                    #
      
      # unique IDs one scale up
      Top_ID <- unique(All_ID[, Foc_ID_Up])
      Count <- 1
      
      ###---------------------------------------------------------------###
      # Add cells if needed                                               #
      for(M in 1:length(Top_ID)){
        ID      <- Top_ID[M]
        Temp_ID <- All_ID[which(All_ID[,Foc_ID_Up] == ID), ]
        Temp_ID <- Temp_ID[!is.na(Temp_ID$PA_0),]
        if(sum(Temp_ID$PA_0) == 0){
          TOP <- which(Temp_ID$PoO_0 == max(Temp_ID$PoO_0))
          if(length(TOP)>1){TOP <- sample(TOP, 1)}
          # row in Cell_ID
          Data[which(Data$ID_0 == Temp_ID[TOP, "ID_0" ]), "PA_0"] <- 1
          New_add[Count, 1] <- Data[which(Data$ID_0 == 
                                            Temp_ID[TOP, "ID_0" ]), 
                                    "ID_0"]
          Count <- Count + 1 
          rm(TOP)
        }
        rm(ID, Temp_ID)
      }
      
      ###---------------------------------------------------------------###
      # Update at all scales                                              #
      
      if(nrow(New_add)!=0){ 
        for (j in 1:nrow(New_add)){
          for (k in 1:(i+1)){  
            Loc_k <- Data[which(Data[, "ID_0"] == New_add[j, 1]), 
                          paste("ID_", k, sep="")]
            Data[which(Data[,  paste("ID_", k, sep="")]==Loc_k), 
                 paste("PA_", k, sep="")] <- 1
            Data[is.na(Data$PA_0), 
                 paste("PA_", k, sep="")] <- NA
            rm(Loc_k) 
          } # end loop 1:scales
          rm(k)
        }  
      }
      rm(Count)
      # update the number of cells to add
      New_Cells <- Add_Cells - nrow(New_add)
      rm(All_ID, Top_ID, M)
      
      ###---------------------------------------------------------------###
      # add additional cells if needed                                    #
      
      if(New_Cells >0){   
        # remove absences at the larger scales
        All_ID <- Data[!is.na(Data[, Foc_PA_Up]), ]  
        # remove absences at the larger scales 
        All_ID <- All_ID[All_ID[, Foc_PA_Up] == 1, ] 
        # remove ones at current scale
        All_ID <- All_ID[All_ID[, Foc_PA] != 1, ] 
        # remove NAs at local scale
        All_ID <- All_ID[!is.na(All_ID$PA_0),]  
        # remove 1 at local scale
        All_ID <- All_ID[All_ID[, "PA_0"]!= 1,]              
        
        ###-------------------------------------------------------------###
        # by now- All_ID contains cells that are not NA in any scale,     # 
        # that are occupied at the scale up and are empty at the current  # 
        # scale.                                                          #
        
        # unique ID in Scale i
        All_ID        <- unique(All_ID[, Foc_ID]) 
        # remove NA from unique
        All_ID        <- as.data.frame(All_ID[!is.na(All_ID)])  
        # match the mean PoO
        All_ID$PoO    <- Data[match(All_ID[,1], Data[, Foc_ID]), Foc_PoO] 
        names(All_ID) <- c("ID", "PoO")
        # Order in decreasing PoO
        All_ID        <- All_ID[order(-All_ID$PoO),]                               
        All_ID$PA_ID  <- NA
        # number of available cells
        DownHyb[i + 1, "Available"]   <- nrow(All_ID)

        ###-------------------------------------------------------------###
        # select within each coarse scale cell,                           #
        # the local cells with the highest PoO                            #
        
        k <- 1
        j <- 1
        while(j<=New_Cells){
          # are there additional cells to consider?
          if (k> nrow(All_ID)){break} 
          Temp1 <- Data[which(Data[, Foc_ID] == All_ID[k, "ID"]), ] 
          # are there potantial cells left in cell k?
          if(sum(Temp1$PA_0, na.rm=TRUE) == sum(!is.na(Temp1$PA_0))){
            k = k+1 
            next}
          Temp1  <- Temp1[!is.na( Temp1[,"PA_0"]), ]                                
          Temp1  <- Temp1[Temp1[, "PA_0"] != 1,]                                    
          # highest local PoO
          PA0_ID <- Temp1[which(Temp1[, "PoO_0"] == 
                                  max(Temp1[, "PoO_0"])), 
                          "ID_0"]
          if(length( PA0_ID)>1){PA0_ID <- sample(PA0_ID, 1)  }                      
          All_ID[k, "PA_ID"] <- PA0_ID
          Data[which(Data[, "ID_0"]==PA0_ID), "PA_0"] <- 1
          k <- k + 1
          j <- j + 1
          rm(Temp1, PA0_ID)
        }
        rm(j, k)      
        New_add2 <- data.frame(ID_0 =
                                 All_ID[!is.na(All_ID$PA_ID), "PA_ID"]) 
        
        ###-------------------------------------------------------------###
        # Update P/A in all scales                                        #      
        
        # update all other scales to present
        for (j in 1:nrow(New_add2)){
          for (k in 1:(i+1)){  
            Loc_k <- Data[which(Data[, "ID_0"] == New_add2[j, 1]), 
                          paste("ID_", k, sep="")]
            Data[which(Data[,  paste("ID_", k, sep="")] == Loc_k), 
                 paste("PA_", k, sep="")] <- 1
            Data[is.na(Data$PA_0), 
                 paste("PA_", k, sep="")] <- NA
            rm(Loc_k) 
          } 
          rm(k)
        }
        rm(j, Foc_PoO, Foc_PA_Up, All_ID) 
      }
      
      ###---------------------------------------------------------------###
      # Update the DownsScaled_Hyb dataframe                              #
      
      DownHyb[i + 1, "Added"]   <- nrow(New_add)+ nrow(New_add2)
      

    } # end if(Add_Cells >0)
    if(verbose){
      cat(paste(" --> done \n"))}  
    rm(N_Cells, P_Cells, Add_Cells, Foc_ID, Foc_PA)
    
  }# end loop nrow(DownHyb)  
  
  
  ###-------------------------------------------------------------------###
  ### Update Data and DownHyb for a special case: ----------------------###
  ### Hui is the only model and no masking -----------------------------###
  ###-------------------------------------------------------------------###
  
  if(models== "Hui" && !Mask){
    if(verbose){
      cat(paste("Special case: Hui + no masking --> "))
    }
    XY_Data <- xyFromCell(PoO_Raster, 1:ncell(PoO_Raster))
    for ( i in nrow(DownHyb):(nrow(Down2)-1)){
      ID_ras <- get(paste("Extent_",i,"_ID",sep=""))
      Data[, paste("ID_", i, sep="")] <- extract(ID_ras, XY_Data)
      rm(ID_ras)
    }
    
    if(FUN =="mean"){
      for (i in nrow(DownHyb):(nrow(Down2)-1)){
        Mean_PoO <- aggregate(x  = Data$PoO_0,
                              by = list(Data[, paste("ID_", i, sep="")]),
                              FUN=mean,
                              na.rm= TRUE)
        names(Mean_PoO)[1] = "ID"
        names(Mean_PoO)[2] = "PoO"
        for(k in 1: nrow(Mean_PoO)){
          CELLS <- which(Data[, paste("ID_", i, sep="")] == 
                           Mean_PoO[k, "ID"])
          Data[CELLS, paste("PoO_", i, sep="")] <- Mean_PoO[k, "PoO"]
          rm(CELLS)}
        rm(Mean_PoO)
      }}
    
    if(FUN =="median"){
      for ( i in nrow(DownHyb):(nrow(Down2)-1)){
        Mean_PoO <- aggregate(x  = Data$PoO_0,
                              by = list(Data[, paste("ID_", i, sep="")]),
                              FUN=median,
                              na.rm= TRUE)
        names(Mean_PoO)[1] = "ID"
        names(Mean_PoO)[2] = "PoO"
        for(k in 1: nrow(Mean_PoO)){
          CELLS <- which(Data[, paste("ID_", i, sep="")] == 
                           Mean_PoO[k, "ID"])
          Data[CELLS, paste("PoO_", i, sep="")] <- Mean_PoO[k, "PoO"]
          rm(CELLS)}
        rm(Mean_PoO)
      }}
    
    if(FUN =="Quan_75"){
      Quan_75 = function(x){quantile(x, probs = 0.75, na.rm=TRUE)}
      for ( i in nrow(DownHyb):(nrow(Down2)-1)){
        Mean_PoO <- aggregate(x  = Data$PoO_0,
                              by = list(Data[, paste("ID_", i, sep="")]),
                              FUN=Quan_75)
        names(Mean_PoO)[1] = "ID"
        names(Mean_PoO)[2] = "PoO"
        for(k in 1: nrow(Mean_PoO)){
          CELLS <- which(Data[, paste("ID_", i, sep="")] == 
                           Mean_PoO[k, "ID"])
          Data[CELLS, paste("PoO_", i, sep="")] <- Mean_PoO[k, "PoO"]
          rm(CELLS)}
        rm(Mean_PoO)
      }}
    
    
    ## add all the PA columns 
    for ( i in nrow(DownHyb):(nrow(Down2)-1)){
      Data[, paste("PA_", i, sep="")] <- Data$PA_0  
    }
    rm(i)
    
    # reorder
    Ordered <- c("ID", "X", "Y")
    for (i in 0:(nrow(Down2)-1)){Ordered <- c(Ordered, 
                                              paste("ID_", i, sep=""))}
    for (i in 0:(nrow(Down2)-1)){Ordered <- c(Ordered, 
                                              paste("PoO_", i, sep=""))}
    for (i in 0:(nrow(Down2)-1)){Ordered <- c(Ordered, 
                                              paste("PA_", i, sep=""))}
    Data <- Data[, Ordered]
    rm(Ordered, i)
    
    ## update all occurences for creating rasters
    All_ID <- Data[which(Data[, "PA_0"]==1 ), "ID_0" ]
    
    for (j in 1:length(All_ID)){
      for (k in 1:(scales)){  
        Loc_k <- Data[which(Data[, "ID_0"] == All_ID[j]), 
                      paste("ID_", k, sep="")]
        Data[which(Data[,  paste("ID_", k, sep="")] == Loc_k),
             paste("PA_", k, sep="")] <- 1
        Data[is.na(Data$PA_0), 
             paste("PA_", k, sep="")] <- NA
        rm(Loc_k) 
      } 
      rm(k)
    }
    rm(j, XY_Data)
    
    ### treat the DownHyb
    DownHyb[(nrow(DownHyb)+1): nrow(Down2), 1:ncol(Down2)] <-
          Down2[(nrow(DownHyb)+1): nrow(Down2),]
    if(verbose){
      cat(paste(" --> done \n"))} 
  }
  
  ###-------------------------------------------------------------------###
  # verify P/A at each scale according to the hybrid model                #
  if(verbose){
    cat(paste("Veryifying occupancies nestedness --> "))}
  Temp <- upgrain(setValues(x = PoO_Raster, values = Data$PA_0), 
                  cell.width = NULL, 
                  scales     = scales, 
                  threshold  = NULL,
                  method     = method, 
                  plot       = Plot)
  DownHyb[, "ncells_Hybrid"] <- 
      (Temp$occupancy.stand[1:nrow(DownHyb), "Extent"] * 
       Temp$occupancy.stand[1:nrow(DownHyb), "Occupancy"]) / 
      Temp$occupancy.stand[1:nrow(DownHyb), "Cell.area"]
  rm(Temp)
  if(verbose){
    cat(paste("done\n"))}
  
  ###-------------------------------------------------------------------###
  ### Rasterizing PA at all scales -------------------------------------###
  
  
  if(verbose){
    cat(paste("Rasterizing predicted P/A + aggregated PoO at all scales:\n"))}
  
  #  a temporary raster, needed for the rasterize function
  r_temp <- raster(ext = extent(PoO_Raster),
                   res = res(PoO_Raster), vals = 1)
  
  for(i in 1:nrow(DownHyb)){
    if(verbose){
      cat(paste("     Scale= ",
                i-1,
                " --> ",
                sep = ""))}
    
    if(i!=1){
      PoO.run <- SpatialPointsDataFrame(
            coords      = cbind(Data$X, Data$Y),
            data        = data.frame(Data[, paste("PoO_", i-1, sep="")]),
            proj4string = CRS(projection(PoO_Raster)))
      PoO.run <- rasterize(PoO.run, r_temp, fun = mean)
      PoO.run <- subset(PoO.run, 2, drop=TRUE)
      names(PoO.run) <-  paste("PoO_", i-1, sep="")
      Stack <- addLayer(Stack, PoO.run)
      rm(PoO.run)
    }
    
    PA.run <- SpatialPointsDataFrame(
            coords      = cbind(Data$X, Data$Y),
            data        = data.frame(Data[, paste("PA_", i-1, sep="")]),
            proj4string = CRS(projection(PoO_Raster)))
    PA.run <- rasterize(PA.run, r_temp, fun = mean)
    PA.run <- subset(PA.run, 2, drop=TRUE)
    names(PA.run) <-  paste("PA_", i-1, sep="")
    Stack <- addLayer(Stack, PA.run)
    rm(PA.run)
    if(verbose){cat(paste("done \n"))}
    
  }
  
  if(Plot){
    if(verbose){
      cat(paste("Plotting  --> "))}
    plot(Stack, colNA = "darkgray")
    if(verbose){cat(paste("done \n"))}
  }
  
  ###-------------------------------------------------------------------###
  ### the return list --------------------------------------------------###
 
  if(verbose){
    cat(paste("Arranging the return list --> "))}
  Return_List <- list(Stack          = Stack,    
                      Data           = Data,      
                      DownHyb        = DownHyb, 
                      UpGrained      = UpGrained,     
                      DownScaled     = DownScaled)     
  if(verbose){
    cat(paste("done \n"))}
  if(verbose){
    cat(paste("\n##---------------------------------------------##\n"))}
  return(Return_List)
}# End function

###########################################################################











