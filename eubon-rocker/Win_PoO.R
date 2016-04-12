##############################################################################
# Name:                                                                      #
#    Win_PoO                                                                 #                                
# Aim:                                                                       #
#    for each cell in a raster, return the mean probability of occurrence    #
#    (PoO) in user-defined windows sizes around the cell                     #
# Input:                                                                     #
#    Stack    - object of class "RasterStack", with IDs in the first raster  #
#               and PoO in the second.                                       #
#    Win_Size - vector of odd integer numbers with the sizes of the windows. #
#               Mean PoO is calcualted for each window size.                 #
#               See raster::focal for details.                               #
#    Out      - character, the type of output three options:                 #
#               1. "Stacked" - the mean PoO in each window size is added as  #
#               another raster to Stack, and the RasterStack is returned.    #
#               2. "DataFrame" - All the info from Stack and the mean Poo in #
#               each window size are returned as a data frame.               #
#               3. "Both" - A list with the RasterStack and the DataFrame    #
#    Plot     - Logical, if TRUE, the mean PoO is plotted                    #
#    verbose  - Logical, if TRUE, progress is printed in the consule         #
# Return:                                                                    #
#    See details in "Out" above                                              #
# Author:                                                                    #
#    Yoni Gavish <gavishyoni@gmail.com>                                      #
# Date:                                                                      #
#    29 Feb. 2016                                                            #
##############################################################################

Win_PoO = function(Stack, 
                   Win_Size,    
                   Out        = "Both",      
                   Plot        = TRUE,        
                   verbose     = TRUE,
                   Title       = "Species")
{
  
  ###----------------------------------------------------------------------###
  ## perform checks -------------------------------------------------------###
  ###----------------------------------------------------------------------###
  
  require(raster)
  if(!is.logical(verbose)){
    cat(paste("verbose should be logical --> default=T --> "))
    verbose = TRUE}
  
  if(verbose){cat(paste("\n##---##---##---##---##\n"))}
  if(verbose){cat(paste("Performing checks           --> "))}
  
  if(!is.logical(Plot)){
    cat(paste("Plot should be logical --> setting to default=T --> "))
    Plot = TRUE}
  
  if(class(Stack)!= "RasterStack"){
    stop(paste("'Stack' should be of class 'RasterStack' - \n
               ID of cells in the first raster and the Probabilites of 
               occurrence in the second."))}
  if(!is.vector(Win_Size)){
    stop(paste("'Win_Size' should be a vector of odd integers - \n
               See raster::factor for details"))}
  
  if(!all(Win_Size %% 2 != 0)){
    stop(paste("'Win_Size' should be a vector of odd integers - \n
               See raster::factor for details"))}
  
  if(length(Out) > 1){
    stop(paste("'Out' should be one of: `Both`, `DataFrame`, or `Stacked`"))}
  
  if(is.na(match(Out, c("Both", "DataFrame", "Stacked")))){
    stop(paste("Out' should be one of: `Both`, `DataFrame`, or `Stacked`"))}
  
  if(verbose){cat(paste("done\n"))}
  
  ###----------------------------------------------------------------------###
  ### preparing stuff -----------------------------------------------------###
  ###----------------------------------------------------------------------###
  if(Plot){
    if(length(Win_Size) <= 2){par(mfrow = c(1,2))}
    if(length(Win_Size) <= 4){par(mfrow = c(2,2))}
    if(length(Win_Size) <= 6){par(mfrow = c(3,2))}
    if(length(Win_Size) >  6){par(mfrow = c(3,3))}
  }
  # Extract raster
  PoO_Ras <- subset(Stack, 2, drop=TRUE)
  if(Plot){plot(PoO_Ras, main = paste(Title, " : Win_1", sep=""))}
  
  # preparing fun to deal with NAs
  fun = function(x){mean(x, na.rm=TRUE)}
  
  # starting Win_Val
  PoO_Means <- as.data.frame(getValues(Stack))
  
  ###----------------------------------------------------------------------###
  ### deploy moving windows -----------------------------------------------###
  ###----------------------------------------------------------------------###
  
  if(verbose){
    cat(paste("Deploying moving windows    --> "))}
  for(i in 1:length(Win_Size)){
    NAME    <-paste("Win_", Win_Size[i], sep="") 
    PoO_New <- focal(PoO_Ras, 
                     w     = matrix(1 ,nrow=Win_Size[i],ncol=Win_Size[i]), 
                     fun   = fun,
                     pad   = TRUE,
                     padValue = NA) 
    
    Temp1 <- getValues(PoO_New)
    Temp2 <- getValues(PoO_Ras)
    Temp1[is.na(Temp2)] <- NA
    PoO_New <- setValues(PoO_New, Temp1)
    
    if(Plot){
      plot(PoO_New, main = paste(Title, " : ", NAME, sep=""))}
    
    if(Out== "Both" || Out== "Stack"){
      names(PoO_New) <- NAME
      Stack <- addLayer(Stack, PoO_New)}
    
    if(Out== "Both" || Out== "DataFrame"){
      PoO_Means[, NAME] <- getValues(PoO_New)}
    rm(PoO_New, NAME, Temp1, Temp2)
  }
  if(verbose){cat(paste("done\n"))}
  
  ###----------------------------------------------------------------------###
  ### the return object ---------------------------------------------------###
  ###----------------------------------------------------------------------###
  
  if(verbose){cat(paste("Arranging the return object --> done"))}
  if(verbose){cat(paste("\n##---##---##---##---##\n"))}
  if(Out == "Both"){
    Return_list <- list(Stack  = Stack,
                        PoO_Means    = PoO_Means)
    return(Return_list)}
  if(Out == "DataFrame"){return(PoO_Means)}
  if(Out == "Stack")    {return(Stack)}
  
} # end main function







