#'Calculate kendrick mass
#'
#'using use data
#'@param data:fticrdata, base compound: repeating unit
#'@return add_kendrick mass and KMD
#'@export
add_kendrick <- function(ftmsObj, base_compounds = 'CH2'){
  # check that ftmsObj is of the correct class #
  exmass=ftmsObj
  # check that ftmsObj doesn't already have cnames specified for ratios in e_meta #
  if(!all(base_compounds %in% c('CH2', 'CO2', 'H2', 'H2O', 'CHO'))) stop("Base compounds must be one of 'CH2', 'CO2', 'H2', 'H2O', or 'CHO'")

  # pull e_meta out of ftmsObj #
  # determine which ratio to use: CH2, CO2, H2, H20, CHO
  coefs = sapply(base_compounds, function(base){
    switch(base,
           'CH2' = 14/14.01565,
           'CO2' = 44/43.98983,
           'H2' = 2/2.015650,
           'H2O' = 18/18.010565,
           'CHO' = 29/29.00274
    )}
  )

  # make new column names for e_meta
  mass_cols = sapply(base_compounds, function(base) paste0('kmass.', base))
  defect_cols = sapply(base_compounds, function(base) paste0('kdefect.', base))
  nominal_cols=sapply(base_compounds, function(base) paste0('Nominal_K.', base))
  # calculate kendrick masses for each base...
  masses = as.numeric(as.character(exmass[,4])) %*% t(coefs)
  # ... workaround for a quick when storing a single column matrix in a dataframe column ...
  if(dim(masses)[2] == 1) masses <- as.vector(masses)
  # ... and store in new columns
  exmass[mass_cols] = masses

  # calculate kendrick defect #
  exmass[defect_cols] = ceiling(exmass[mass_cols]) - exmass[mass_cols]
  exmass[nominal_cols] = round(masses)
  return(exmass)
}
