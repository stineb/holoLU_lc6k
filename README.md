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
2. Cropland area (gridcell fraction). Variable `crop` in file `landuse_hyde32_baseline_pastcorr_lpjgr.nc`
3. Pasture area (gridcell fraction), defines only the pasture area of what would be forested otherwise (potential natural vegetation). Variable `past` in file `landuse_hyde32_baseline_pastcorr_lpjgr.nc`
4. Extent of permanent agriculture (logical), defines wether croplands in the respective gridcell is under permanent management or under a shifting cultivation regime. Variable `PERM` in file `perm_lpjgr_holoLU2.nc`

<!-- 4. Cropland turnover rate (fraction/year), defines the extent of cropland abandoned and re-claimed from non-agricultural land each year.
5. crop_suit ???
 -->

One additional, temporally constant input variables, also given as maps, has to be provided:

1. Land suitability (gridcell fraction), defines the fraction of the gridcell that is suitable for agriculture. Remainder remains primary land. This is used to constrain the maximum extent of secondary land. Provided by variable `SUIT` in file `land_suit_sage_lpjgr.nc`.


#### Procedure in LPX as an example

Fortran code used within LPX to read and interpret the land use forcing is given in the file `read_landuse_map.F`. The following steps (code extracted fromt that file) are crucial:

1. Define the gridcell area fraction that remains inaccessible and therefore primary land (not affected by land conversion), based on the file `land_suit_sage_lpjgr.nc`:
```Fortran
inaccess(jpngr)   = max( 0.d0, land_fraction(jpngr) - fsuit )
```

2. Define wether land is under permanent of non-permanent (shifting cultivation-type) agriculture, based on the file `perm_lpjgr_holoLU2.nc`. Under non-permanent agriculture, a constant land turnover rate of 0.25 is assumed (a quarter of cropland area abandoned each year, variable `ltor`), and the fraction of land suitable for cropland cultivation (`cropsuit`) is set to be equal to the (temporally constant) accessible area fraction. Under permanent agriculture, a fallow rotation regime is assumed, corresponding to a three-field rotation for all years before 1850 CE, gradually shifting to no fallow for all years after 1960 CE. This is implemented using the variable `fallow_factor`. In this case (pre-1850), the fraction of land suitable (used) for cropland cultivation is set to 1.5 times (`3.0d0/2.0d0`) the cropland area (`lu_area(lucrop,jpngr)`). Note the difference: The prescribed cropland area (`lu_area(lucrop,jpngr)`) is the area *currently* (in a given year) under cultivation and fallow areas don't count toward that value. This implies that an additional 50% is fallow at each given point in time. The land turnover rate (`ltor`) is `fallow_factor / 3.d0`. Hence, for pre-1850 years, 50% of the land is "abandoned", i.e. converted from actually cultivated cropland to fallow, which is treated as secondary land in LPX.  
```Fortran
!     -------------------------------------------------------------------------
!     CROPSUIT and LAND TURNOVER RATE
!     Land fraction suitable from cropland agriculture. 
!     -------------------------------------------------------------------------
          fallow_factor = max(
     $         1.0d0,
     $         min(
     $         3.0d0/2.0d0,
     $         3.0d0/2.0d0 - 0.5d0/110.0d0 * (realyear-1850.0d0)
     $         ) )
          
          if (shifting_cultivation(jpngr)) then
            cropsuit(jpngr) = accessible(jpngr)
            ltor(jpngr)     = 0.25d0
          else
            cropsuit(jpngr) = max( lu_area(lucrop,jpngr),
     $           min(
     $           accessible(jpngr),
     $           fallow_factor*lu_area(lucrop,jpngr)
     $           ))
            ltor(jpngr)    = fallow_factor / 3.d0
          endif

```

3. The land use transition matrix is constructed. This contains $i \times j$ values, where $i$ and $j$ are the land use categories (primary, secondary, cropland, pasture), and the martrix $M_{i,j}$ (variable `DF_tr` below) defines the gridcell area fraction transiting from land use category $i$ to $j$. 
```Fortran
!     ADD LAND TURNOVER (shifting cultivation)
!     -------------------------------------------------------------------------
          
!     CLAIM CROPLAND/PASTURE
!     only cropland turnover, priority for primary land,
!     accessible primary land constrained.
          lu=lucrop
          
!     first priority, claim from lunat
          req=min(
     $         lu_area_tr(lu,jpngr)*ltor(jpngr),
     $         luold(lu)*ltor(jpngr)                     !required for conversion to crop
     $         )
          avl=max(
     $         max( 0.d0, luold(lunat)-inaccess(jpngr)),
     $         0.d0
     $         )
          con=min(req,avl)                                  !actually converted primary land
          DF_tr(lunat,lu,jpngr)=DF_tr(lunat,lu,jpngr)+con
          req=req-con
          luold(lunat)=luold(lunat)-con
          lu_area_tr(lunat,jpngr)=lu_area_tr(lunat,jpngr)-con
          lu_area_tr(lu,jpngr)=lu_area_tr(lu,jpngr)+con
          if (req.gt.0.0d0) then
!     second priority, claim from lusecd
            avl=luold(lusecd)
            con=min(req,avl)
            DF_tr(lusecd,lu,jpngr)=DF_tr(lusecd,lu,jpngr)+con
            req=req-con
            luold(lusecd)=luold(lusecd)-con
            lu_area_tr(lusecd,jpngr)=lu_area_tr(lusecd,jpngr)-con
            lu_area_tr(lu,jpngr)=lu_area_tr(lu,jpngr)+con
            if (req.gt.0.0d0) then
!     not enough primary and secondary land available to satisfy cropland/pasture abandonment
              uns=req                                       !un-satisfied requirement
            endif
          endif
            
            
!     ABANDON CROPLAND (limited by available primary and secondary)
!     crop/pasture -> secondary (all goes into secondary)
          lu=lucrop
          DF_tr(lu,lusecd,jpngr)=sum(DF_tr(:,lu,jpngr))
          luold(lu)=luold(lu)-DF_tr(lu,lusecd,jpngr)
          lu_area_tr(lu,jpngr)=lu_area_tr(lu,jpngr)-DF_tr(lu,lusecd,jpngr)
          lu_area_tr(lusecd,jpngr)=lu_area_tr(lusecd,jpngr)+DF_tr(lu,lusecd,jpngr)
          
        endif       
```


## Preparing the land use forcing for BGC simulations

This describes how the LPX land use forcing files are created - from original HYDE and KK10 files to final forcing files.

In summary, the following steps have to be repeated for each added scenario (given original files in NetCDF at any resolution)

1. `r source('prepare_harvest_holoLU2.R’)` done only for `hyde32_upper`
2. `r source('regrid_landuse_holoLU2.R')`
3. `r source(‘regrid_shiftcult_holoLU2.R')`
4. `r source(‘remove_fopen_holoLU2.R')`

### Harvested area




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
* remove_fopen_holoLU2.R: remove open vegetation fraction from pasture => landuse_hyde31_final_pastcorr_lpjgr.cdf