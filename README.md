# PAGES Landcover6K protocols

This repository contains protocols for the following purposes:

- Defining simulation protocols and implementations of the land use forcing
- Preparing land use model forcing files
- Analysing model outputs

## Simulation protocols

### Implementation of the land use forcing

#### Summary

This is a description of how the land use forcing was implemented for simulations presented in (Stocker et al., 2017)[https://www.pnas.org/content/114/7/1492]. This may serve as a guide for implementations in other models targeted for the Landcover6K simulations.

Four different time-varying input variables have to be provided, covering all time steps (12 ka BP - present) and are given as maps (gridded field for longitude and latitude). All preparation steps are done at a spatial resolution of 1 degree.

1. Harvested area (gridcell fraction), defines the (forest) area that is cleared per year. Variable `aharv` in file `data/harvest_hurtt_byarea_v2_lpjgr_backby_hyde32_baseline.nc`
2. Cropland area (gridcell fraction). Variable `crop` in file `data/landuse_hyde32_baseline_pastcorr_lpjgr.nc`
3. Pasture area (gridcell fraction), defines only the pasture area of what would be forested otherwise (potential natural vegetation). Variable `past` in file `data/landuse_hyde32_baseline_pastcorr_lpjgr.nc`
4. Extent of permanent agriculture (logical), defines wether croplands in the respective gridcell is under permanent management or under a shifting cultivation regime. Variable `PERM` in file `data/perm_lpjgr_holoLU2.nc`

<!-- 4. Cropland turnover rate (fraction/year), defines the extent of cropland abandoned and re-claimed from non-agricultural land each year.
5. crop_suit ???
 -->

One additional, temporally constant input variables, also given as maps, has to be provided:

1. Land suitability (gridcell fraction), defines the fraction of the gridcell that is suitable for agriculture. Remainder remains primary land. This is used to constrain the maximum extent of secondary land. Provided by variable `SUIT` in file `land_suit_sage_lpjgr.nc`.



## Preparing the land use forcing for BGC simulations

This describes how the LPX land use forcing files are created - from original HYDE and KK10 files to final forcing files.

In summary, the following steps have to be repeated for each added scenario (given original files in NetCDF at any resolution)

1. `r source('prepare_harvest_holoLU2.R’)` done only for `hyde32_upper`
2. `r source('regrid_landuse_holoLU2.R')`
3. `r source(‘regrid_shiftcult_holoLU2.R')`
4. `r source(‘remove_fopen_holoLU2.R')` 
To be completed: I will add a brief description of what went into creating the forcing files and how exactly they were created.



<!-- ### Harvested area

Preparation of harvest data (‘prepare_harvest_holoLU2.R'):
* ‘extract_hydeslices_harvest.sh’ on Bern server: extract HYDE slices from 'harvest_hurtt_byarea_v2_historical_1500-2004_halfdeg.nc ‘ =>  harvest_hurtt_byarea_v2_halfdeg_hydeslices_tmp.nc
* ‘prepare_harvest_holoLU2.R’: project backwards in time using cropland area evolution over time from HYDE final and KK10 for each continent separately => harvest_hurtt_byarea_v2_halfdeg_backby_hyde.nc harvest_hurtt_byarea_v2_halfdeg_backby_kk10.nc

Preparation of land turnover data (‘prepare_perm_turnover_cropsuit_holoLU2.R'):
* file ‘perm_holoLU.nc’:
    * Before 1700 AD, permanent agriculture where O&H (from file 'popcat_olhick_cru_sherratt_59191.nc') suggest civilisations. 
    * After 1700 AD, permanent agriculture where LUH ('shiftcult_map_halfdeg.cdf’) does not suggest shifting cultivation and cropland>0
* file "crop_suit_holoLU2.nc”:
    * in permanent agriculture areas: "suitable fraction" is cropland area scaled by the "fallow factor" to account for fallow-rotation.
        * fallow factor: 3/2 before 1850, globally uniform, reducing to 1 by 1960.
    * in non-permanent areas: suitable fraction taken from SAGE data
* file "turnovertime_cropland_holoLU2.nc”:
    * in permanent agriculture areas: turnover time tau = 3 / ff
    * in non-permanent agriculture areas: tau = 4 yr

Preparation of KK10 landuse data:
* prepare_kk10.sh: extracted time slices using on Bern server. for each HYDE-year, extract from ‘ K11_30m_8k_merge.nc' => 'landuse_KK11_halfdeg_<YEAR>.nc'
* download slices to my mac
* prepare_fpast_fcrop.R: pasture fraction for all HYDE years => "fpast_STAGE1_hyde31_final_halfdeg.nc"
* prepare_fpast.jnl: extension in space using Ferret => "fpast_STAGE2_hyde31_final_halfdeg.nc"
* prepare_kk10.R: stack slices together, interpolate remaining slices, separate fcrop/fpast => "landuse_KK11_halfdeg_hydeslices.nc”

Preparation of fopen file:
* extracted variable fpc_grid from LPX output r0_holoLU2.cdf
* downloaded this to my machine
* ‘prepare_fopen.jnl’: average over years and sum over grasses => fopen_r0_holoLU2.nc

Regridding all files:
* regrid_landuse_holoLU2.R:
    * regridding cropland/pasture files (landuse_hyde31_final_halfdeg.cdf, landuse_KK11_halfdeg_hydeslices.nc) from halfdeg to lpjgr => (landuse_hyde31_final_lpjgr.cdf, landuse_KK11_lpjgr.nc)
    * regridding harvested area files (harvest_hurtt_byarea_v2_halfdeg_backby_hyde.nc,  harvest_hurtt_byarea_v2_halfdeg_backby_kk10.nc) => (harvest_hurtt_byarea_v2_lpjgr_backby_hyde.nc, harvest_hurtt_byarea_v2_lpjgr_backby_kk10.nc)
* regridding files for defining permanen/non-permanent, shifting cultivation, land turnover time
    * 1. regrid_suit_holoLU2.jnl: regrid SAGE suitable land fraction file to lpjgr
    * 2. regrid_perm_holoLU2.jnl: regrid permanent/non-permanent info using Ferret’s nice transformation function @nrst
    * 3. regrid_shiftcult_holoLU2.R: uses regridded permanent/non-permanent mask (perm_lpjgr_holoLU2.nc) and suitable fraction file (land_suit_sage_lpjgr.nc) to calculate “suitable fraction” and land turnover time. => shiftcultinfo_*_lpjgr_holoLU2.nc, inaccess_lpjgr_holoLU2.nc
* remove_fopen_holoLU2.R: remove open vegetation fraction from pasture => landuse_hyde31_final_pastcorr_lpjgr.cdf -->