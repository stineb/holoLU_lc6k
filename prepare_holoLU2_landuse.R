library(ncdf)
source('/alphadata01/bstocker/utilities/cdf.write.R')

## End product: field for every hyde time step defining "land suitability" 

## Read cropland/pasture fields (HYDE 3.1)
lu <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/hyde3_1/hyde31_final/landuse_hyde31_final_halfdeg.cdf" )
time_hyde <- lu$dim$TIME$vals
nlon <- lu$dim$LONGITUDE$len
nlat <- lu$dim$LATITUDE$len
crop_hyde <- get.var.ncdf(lu, "crop")
past_hyde <- get.var.ncdf(lu, "past")
close.ncdf(lu)

## Read land suitability field (SAGE)
suit <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/sage/land_suit_sage_halfdeg.nc" )
suit_sage <- get.var.ncdf(suit, "DATA")
close.ncdf(suit)

## Read natural grassland field (SAGE)
# The following legend is used for the 15 vegetation types:
# 1       Tropical Evergreen Forest/Woodland
# 2       Tropical Deciduous Forest/Woodland
# 3       Temperate Broadleaf Evergreen Forest/Woodland
# 4       Temperate Needleleaf Evergreen Forest/Woodland
# 5       Temperate Deciduous Forest/Woodland
# 6       Boreal Evergreen Forest/Woodland
# 7       Boreal Deciduous Forest/Woodland
# 8       Evergreen/Deciduous Mixed Forest/Woodland
# 9       Savanna
# 10      Grassland/Steppe
# 11      Dense Shrubland
# 12      Open Shrubland
# 13      Tundra
# 14      Desert
# 15      Polar Desert/Rock/Ice

vegt <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/sage/potveg_nc/vegtype_0.5.nc" )
vegt_sage <- get.var.ncdf(vegt, "vegtype")
close.ncdf(vegt)

## create field that contains TRUE if gridcell is covered by "open" vegetation (pasture does not require conversion)
open_sage <- vegt_sage * NA
open_sage[ vegt_sage < 9 ] <- FALSE
open_sage[ vegt_sage > 8 ] <- TRUE

## Read civilisation fields (Olofsson & Hickler)
civi <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/olofsson/popcat_olhick_cru_sherratt_59191.nc" )
civi_olof <- get.var.ncdf(civi, "category")
time_olof <- civi$dim$TIME$vals
close.ncdf(civi)

## Read LUH shifting cultivation field
scul <- open.ncdf( "/alphadata01/bstocker/data/landuse_data/luh_rcp_hurtt/historical/halfdeg/shiftcult_map_halfdeg.cdf" )
scul_hurt <- get.var.ncdf(scul, "SHIFTCULT")
close.ncdf(scul)

## create field that contains string "perm" for each time step in HYDE data where O&H suggest permanent agriculture
len <- length(time_hyde)
index_in_olof <- rep(NA, len)
jdx <- 1
for (idx in 1:length(time_hyde)){
  if (time_hyde[idx] >= time_olof[jdx]){
    index_in_olof[idx:len] <- jdx
    jdx <- jdx + 1
    if (jdx > length(time_olof)){ break }
  }
}
for (idx in 1:length(time_hyde)){
  if (time_hyde[idx]>=1700) {
    index_in_olof[idx:len] <- 5
    break
  }
}


calc_crop_suit <- function( crop, suit_sage, perm ){

  if ( perm ){
    ## permanent agriculture: fallow-rotation, 2 : 1
    # crop_suit <- min( 1.0, 3/2 * crop)
    crop_suit <- 2
  } else {
    ## non-permanent agriculture: shifting cultivation in all suitable area
    # crop_suit <- suit_sage
    crop_suit <- 1
  }

  return( crop_suit )
}


calc_perm <- function( perm ){

  if ( perm ){
    ## permanent agriculture: fallow-rotation, 2 : 1
    crop_suit <- 2
  } else {
    ## non-permanent agriculture: shifting cultivation in all suitable area
    crop_suit <- 1
  }

  return( crop_suit )
}


perm_field<- crop_hyde * NA

## Determine whether gridcell features permanent agriculture at each time step

for (ilon in 1:nlon){

  for (ilat in 1:nlat){

    if ( !is.na(crop_hyde[ilon,ilat,1])){
      
      for (itim in 1:len){

        # print(paste("year",time_hyde[itim]))

        perm <- FALSE

        if ( is.na( index_in_olof[itim] ) ){

          # print("no permanent agriculture")

        } else {

          if ( time_hyde[itim]<1700 ){

            ## take O&H civilisation
            # print( paste( "using O&H info for their year", time_olof[index_in_olof[itim]]))

            if (!is.na(civi_olof[ilon,ilat,index_in_olof[itim]])){

              if (civi_olof[ilon,ilat,index_in_olof[itim]]==2){ 

                perm <- TRUE 
                # print("permanent agriculture!")

              }            

            }

          } else {

            ## take LUH shifting cultivation map, add all permanent cells from O&H data at 1000 AD
            # print( paste( "using LUH shift. cult. map for year", time_hyde[itim]))

            if (!is.na(crop_hyde[ilon,ilat,itim])&&!is.na(scul_hurt[ilon,ilat])){

              if ( crop_hyde[ilon,ilat,itim]>0.0 && scul_hurt[ilon,ilat]!=1 || !is.na(civi_olof[ilon,ilat,4]) && civi_olof[ilon,ilat,4]==2) {

                perm <- TRUE
                # print("permanent agriculture!")

              } 

            }

          }

        }

        # print( paste( "crop:      ", crop_hyde[ilon,ilat,itim] ))
        # print( paste( "suit sage: ", suit_sage[ilon,ilat]))
        # print( paste( "perm       ", perm))

        if (is.na(suit_sage[ilon,ilat]) && !is.na(crop_hyde[ilon,ilat,itim]) ){
          suit_sage[ilon,ilat] <- crop_hyde[ilon,ilat,itim]
        }

        # crop_suit[ilon,ilat,itim] <- calc_crop_suit( crop_hyde[ilon,ilat,itim], suit_sage[ilon,ilat], perm )
        perm_field[ilon,ilat,itim] <- calc_perm( perm )

        # print( paste( "mysuit     ", calc_crop_suit( crop_hyde[ilon,ilat,itim], suit_sage[ilon,ilat], perm ) ))

      }      
    }
  }
}

## scale back fallow-rotation factor from 3/2 to 1 (with population density) between 1850 and 1960
fallow_factor <- rep(NA,len)
for (itim in 1:len){
  fallow_factor[itim] <- max( 1.0, min( 3.0/2.0, 3.0/2.0 - 0.5/110.0 * (time_hyde[itim]-1850.0) ) )
}

## On permanent-agriculture land, "suitable" (=allowable) cropland area is proportional to HYDE-cropland area
## but scaled accounting for the fallow-factor
## On non-permanent-agriculture land, "suitable" is all suitable areas from the SAGE data.
crop_suit <- crop_hyde * NA
totime    <- crop_hyde * NA

for (ilon in 1:nlon){

  for (ilat in 1:nlat){

    if ( !is.na(crop_hyde[ilon,ilat,1])){
      
      for (itim in 1:len){

        if (perm_field[ilon,ilat,itim]==2){

          ## permanent areas:

          ## "suitable fraction" is cropland area scaled by the "fallow factor" to account for fallow-rotation
          crop_suit[ilon,ilat,itim] <- min( 1.0, fallow_factor[itim] * crop_hyde[ilon,ilat,itim] )

          ## land turnover is calculated so that the length of a fallow-rotation period is constant (3 years)
          totime[ilon,ilat,itim] <- 3.0 / fallow_factor[itim]


        } else if (perm_field[ilon,ilat,itim]==1){

          ## non-permanent areas:

          ## suitable fraction taken from SAGE data
          crop_suit[ilon,ilat,itim] <- suit_sage[ilon,ilat]

          ## land turnover time in shifting cultivation: 4 years
          totime[ilon,ilat,itim] <- 4.0

        }
      }
    }
  }
}


## Write to NetCDF file
cdf.write( perm_field,"permanent",
          lu$dim$LONGITUDE$vals,lu$dim$LATITUDE$vals,
          "perm_holoLU2.nc",
          z_dim=NA,time=time_hyde,
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )

cdf.write( crop_suit,"crop_suit",
          lu$dim$LONGITUDE$vals,lu$dim$LATITUDE$vals,
          "crop_suit_holoLU2.nc",
          z_dim=NA,time=time_hyde,
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )

cdf.write( totime,"turnovertime_cropland",
          lu$dim$LONGITUDE$vals,lu$dim$LATITUDE$vals,
          "turnovertime_cropland_holoLU2.nc",
          z_dim=NA,time=time_hyde,
          make.zdim=FALSE,make.tdim=TRUE,
          nvars=1,
          vartype="NC_FLOAT",
          verbose=FALSE
          )

