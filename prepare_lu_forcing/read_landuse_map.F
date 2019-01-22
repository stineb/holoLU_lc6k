c     Reads in land use map annually

      subroutine read_landuse_map(newcell,year,realyear)

      implicit none
#include "para.inc"
#include "modelpara.inc"
#include "soil.inc"
#include "pftpar.inc"
#include "output.inc"
#include "products.inc"
      include 'coordinates.inc'
      include 'landuse.inc'
      include 'netcdf.inc'
      include 'steer.inc'
#include "pools.inc"
#include "peatland.inc"
#if _parallel_mpi
      include 'parallel_mpi.inc'
#endif

!     subroutine arguments
      LOGICAL newcell(maxgrid)
      INTEGER year
      REAL*8 realyear

!     local variables
      logical :: start=.true.
      integer error
      integer jpngr,lu,from,to
      integer lufile,varid(0:3),cdf_index(3),cdf_index4(4),cdf_spinup_index(2)
      integer, dimension(:), allocatable :: time
      integer time_length,itime,i,j,imax,ibracket(2)
      real*8 primary(2),secondary(2),cropland(2), pasture(2), builtup(2), sumlu
      real*8 arealeft(maxgrid)
!     Relative cumulative peat initianlisation
!     per grid cell, s-shape function
      real*8 growth_init        
!     Rate of area change of peatlands is calculated from C densities
      REAL*8 areachangerate,scdiffp,scdiffpo
      REAL*8 dlu
      real*8 min_precision
      parameter(min_precision=1.0d-12)

      character*256 message

      save lufile,varid,time,time_length,start

#if _LU_transitions
!     #########################################################################
!     GROSS LAND USE CHANGE INCLUDING TRANSITIONS
      
!     /////////////////////////////////////////////////////////////////////////
!     NEW IMPLEMENTATION OF READ_LANDUSE_MAP.F FOR LU-TRANSITION SIMULATIONS
!     - states are always read in
!     - transitions are always read in
!     - transitions are adjusted to match next year's states
!     - transitions during spinup are based on first transient year's trans.
!       and are corrected to not lead to any net area changes
!     - generated transitions make use of one pre-defined annual transition map
!     Beni Stocker, 8.7.2013; based on code by Fabian Feissli.
!     -------------------------------------------------------------------------
      
!     local variables
      logical readlumap,                          !true when states are read in
     $     readluscenmap,                         !true when states are read in from scenario file
!     $     readscultmap,                          !true when shift. cult. map is read in
     $     readlutrmap,
     $     calclutr,                              !true if harvest-transitions are to be calculated
     $     make_correct,                          !true if any element does not match lu_area from data
     $     correct
      integer nlutr,llu,pft,ndistlon,ndistlat,
     $     jpngr_search,ilon_search,ilat_search,
     $     readlutrmapyear                                  !year AD for which transitions are read
      parameter (nlutr=5)
      character*1 lustate(nlutr)
      parameter (lustate=(/'v','s','c','p','u'/))
      character*2 lutrans_name(nlutr,nlutr)
      integer varid_state(5),ludimid(3),scultdimid(2)
      integer lutrans_varid(nlutr,nlutr),lutrans_harv_varid
      character*16 dimname(3)
      real*8 DF_tr(nlutr,nlutr,maxgrid),                    !transition matrix from data
     $     DF_corr(nlutr,nlutr,maxgrid),                    !correction matrix to match lu_area from data
     $     DF_harv(nlutr,nlutr,maxgrid),                    !transition matrix derived from harvest data
     $     DF_tmp(nlutr,nlutr), luold(0:nlu)                !temporary variables
      save DF_tr,DF_corr,DF_harv
      real*8 lu_area_tr(0:nlu,maxgrid)
      real*8 sum_DF
      integer lustatesfile,lutransfile,dimlength
      save lustatesfile,lutransfile,dimlength
#if _wood_harvest
      character*256 harvest_map_path
      integer harvestfile
      logical found
#if _harvest_byarea
      real*8 harv_a_orig(2),                                !read from input [area fraction]
     $     harv_a(maxgrid),                                 !harvested area fraction in and to primary/secondary
     $     req,avl,con                                      !temporary variables (required,available,converted)
      save harv_a
#else
      real*8 harv_m_orig(5),                                !read from input [MgC]
     $     harv_m(maxgrid,nlunat+nlusecd),                  !harvest in and to primary/secondary [gC]
     $     biomass(nlu,maxgrid),                            !total heartwood biomass in grid cell [gC]
     $     required(maxgrid,nlunat+nlusecd),                !mass still required to satisfy harv. in jpngr [gC]
     $     available(nlu,maxgrid),                          !biomass still available grid cell [gC]
     $     removed                                          !total harvested biomass in gridcell, LU category
      save harv_m
#endif
#if _harvest_mortality
      integer prod
      real*8 hm_store_lu(nlunat+nlusecd)                    !hm_store in this lu,jpngr, [gC/gridcell]
     $     ,scale_hm_store(nlunat+nlusecd)
#endif
#if _harvest_cpexpansion
      real*8 hm_felled(nlunat+nlusecd)
#endif
#endif
#if _LU_generated_transitions
      character*256 fsuit_map_path
      integer fsuit_fileid, fsuitdimid(2), fsuit_varid
      integer control
      logical shifting_cultivation(maxgrid)                 !mask where shift. cult. takes place
      real*8 fsuit
      real*8 perm
      real*8 fallow_factor
      real*8 ltor(maxgrid),                                 !land turnover rate (field)
     $     accessible(maxgrid),                             !accessible land area fraction (field)
     $     inaccess(maxgrid),
     $     cropsuit(maxgrid)
      save shifting_cultivation,ltor,accessible
      
      integer ilu,
     $     shifting_cult_file,                              !NetCDF file ID
     $     shifting_cult_varid                              !NetCDF varible ID
      real*8
     $     req,                                             !required land fraction for shifting cult.
     $     avl,                                             !available land fraction for shifitng cult.
     $     con,                                             !converted land fraction for shifting cult.
     $     uns,                                             !unsatisfied land fraction requirement
     $     scult
      integer, dimension(2) :: luclaim                      !order from which LU shift. cult. requirement is satisfied
#endif

!     Store last year's lu_area into 'lu_area_old'. lu_area will only be
!     updated when readlumap or readluscenmap is true.
!     -------------------------------------------------------------------------
      do jpngr=minline_task,maxline_task
        lu_area_old(:,jpngr) = lu_area(:,jpngr)
      enddo
      
!     /////////////////////////////////////////////////////////////////////////
!     READ IN LAND USE STATES FIELD
!     -------------------------------------------------------------------------
!     During spinup, read states file only in the first simulation year. During
!     transient run, read states file every year.
!     -------------------------------------------------------------------------
      readlumap = .false.
      readluscenmap = .false.
      if (rcp.and.realyear.ge.2005.d0) then
        if (spinup) then
          if (init) then
            readluscenmap = .true.
          else
            readluscenmap = .false.
          endif
        else
          readluscenmap = .true.
        endif
      else
        if (spinup) then
          if (init) then
            readlumap = .true.
          else
            readlumap = .false.
          endif
        else
          readlumap = .true.
        endif
      endif
      
      if (readlumap) then
        lu_states_map_path =
     $       '../input_data/anthropogenic2d/landuse/'//trim(lu_map_filename)
      else if (readluscenmap) then
        lu_states_map_path =
     $       '../input_data/anthropogenic2d/landuse/'//trim(luscen_map_filename)
      endif
      !print*,'landuse states file: ', lu_states_map_path
        
c      print*,'readlumap',readlumap
      
      if (readlumap.or.readluscenmap) then
!     -------------------------------------------------------------------------
!     Update 'lu_area' with this years data from input.
!     Read fractions for each category. Assure consistency with total available
!     land by only using information of fraction of total read in lu_area.
!     -------------------------------------------------------------------------

!     initialise
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          lu_area(:,jpngr)     = 0.0d0
          arealeft(jpngr)      = min( 1.0d0, land_fraction(jpngr) )
          prim(jpngr)          = 0.0d0
          secd(jpngr)          = 0.0d0 
          crop(jpngr)          = 0.0d0
          past(jpngr)          = 0.0d0
          built(jpngr)         = 0.0d0
        enddo
                  
!     open cdf file
!     -------------------------------------------------------------------------
#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
#endif
!          print*,'landuse file: ',trim(lu_states_map_path)
#if _parallel_mpi
        endif
#endif
        error=NF_OPEN(trim(lu_states_map_path),NF_NOWRITE,lustatesfile)
        message=lu_states_map_path
        if (error.ne.0) goto 400

!     get dimension IDs
!     -------------------------------------------------------------------------
        dimname(1)='LONGITUDE' 
        dimname(2)='LATITUDE'
        dimname(3)='TIME'
        
        do i=1,3
          error = NF_INQ_DIMID(lustatesfile,dimname(i),ludimid(i))
          message = 'lustatesfile'
          if (error.ne.0) goto 500
        enddo

!     get data IDs 
!     -------------------------------------------------------------------------
#if _LU_generated_transitions==0
        error=NF_INQ_VARID(lustatesfile,trim(lu_map_natvar),
     $       varid_state(lunat))
        if (error.ne.0) goto 999
        error=NF_INQ_VARID(lustatesfile,trim(lu_map_secdvar),
     $       varid_state(lusecd))
        if (error.ne.0) goto 999
#endif
        error=NF_INQ_VARID(lustatesfile,trim(lu_map_cropvar),
     $       varid_state(lucrop))
        if (error.ne.0) goto 999
        error=NF_INQ_VARID(lustatesfile,trim(lu_map_pastvar),
     $       varid_state(lupasture))
        if (error.ne.0) goto 999
        if (trim(lu_map_builtvar).ne."") then
          error=NF_INQ_VARID(lustatesfile,trim(lu_map_builtvar),
     $         varid_state(lubuilt))
          if (error.ne.0) goto 999
        endif
        
!     get time domain
!     -------------------------------------------------------------------------
        error = NF_INQ_DIMLEN(lustatesfile,ludimid(3),time_length)
        if (error.ne.0) goto 999
        !print*,'time length successfully read from landuse file'
        allocate(time(time_length))
        error = NF_GET_VAR_INT(lustatesfile,ludimid(3),time)
        if (error.ne.0) goto 999
        !print*,'time successfully read from landuse file'

!     get bracket
!     -------------------------------------------------------------------------
        ibracket(:)=0
        call getbracket(realyear,time,time_length,ibracket,imax)

!     read states for bracketing years
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          cdf_index(1)=ilon(jpngr)
          cdf_index(2)=ilat(jpngr)
          do i=1,imax
            cdf_index(3)=ibracket(i)
!            print*,'cdf_index',cdf_index
#if _LU_generated_transitions==0
            error = NF_GET_VAR1_DOUBLE
     $           (lustatesfile,varid_state(1),cdf_index,primary(i))
            if (error.ne.0) goto 999
!            print*,'primary successfully read from landuse file'
            error = NF_GET_VAR1_DOUBLE
     $           (lustatesfile,varid_state(2),cdf_index,secondary(i))
            if (error.ne.0) goto 999
!            print*,'secondary successfully read from landuse file'
#endif
            error = NF_GET_VAR1_DOUBLE
     $           (lustatesfile,varid_state(3),cdf_index,cropland(i))
            if (error.ne.0) goto 999
!            print*,'crop successfully read from landuse file'
            error = NF_GET_VAR1_DOUBLE
     $           (lustatesfile,varid_state(4),cdf_index,pasture(i))
            if (error.ne.0) goto 999
!            print*,'past successfully read from landuse file'
            if (trim(lu_map_builtvar).ne."") then
              error = NF_GET_VAR1_DOUBLE
     $             (lustatesfile,varid_state(5),cdf_index,builtup(i))
              if (error.ne.0) goto 999
!              print*,'built successfully read from landuse file'
            endif
          enddo

          if (imax.eq.2) then
!     interpolate and save the result in first slice
!     -------------------------------------------------------------------------
#if _LU_generated_transitions==0
            primary(1)= primary(1)
     $           +(primary(2)-primary(1))
     $           /(time(ibracket(2))-time(ibracket(1)))
     $           *(realyear-time(ibracket(1)))
            secondary(1)= secondary(1)
     $           +(secondary(2)-secondary(1))
     $           /(time(ibracket(2))-time(ibracket(1)))
     $           *(realyear-time(ibracket(1)))
#endif
            cropland(1)= cropland(1)
     $           +(cropland(2)-cropland(1))
     $           /(time(ibracket(2))-time(ibracket(1)))
     $           *(realyear-time(ibracket(1)))
            pasture(1)= pasture(1)
     $           +(pasture(2)-pasture(1))
     $           /(time(ibracket(2))-time(ibracket(1)))
     $           *(realyear-time(ibracket(1)))
            if (trim(lu_map_builtvar).ne."") then
              builtup(1)= builtup(1)
     $             +(builtup(2)-builtup(1))
     $             /(time(ibracket(2))-time(ibracket(1)))
     $             *(realyear-time(ibracket(1)))
            endif
          endif

#if _LU_generated_transitions==0
          prim(jpngr)=primary(1)
          secd(jpngr)=secondary(1)
#endif
          past(jpngr)=pasture(1)
          crop(jpngr)=cropland(1)
          if (trim(lu_map_builtvar).ne."") then
            built(jpngr)=builtup(1)
          endif

        enddo

        error=NF_CLOSE(lustatesfile)
        
#if _LU_generated_transitions==0
!     correct sum of landuse categories to guarantee that they equal available land area
        do jpngr=minline_task,maxline_task
          sumlu = (prim(jpngr)+secd(jpngr)+crop(jpngr)+past(jpngr)+built(jpngr))
          if (sumlu.gt.0.0d0) then
            sumlu = 1.d0/sumlu
            prim(jpngr) = land_fraction(jpngr)*prim(jpngr)*sumlu
            secd(jpngr) = land_fraction(jpngr)*secd(jpngr)*sumlu
            crop(jpngr) = land_fraction(jpngr)*crop(jpngr)*sumlu
            past(jpngr) = land_fraction(jpngr)*past(jpngr)*sumlu
            built(jpngr) = land_fraction(jpngr)*built(jpngr)*sumlu
          endif
        enddo
#endif
        
!     assign all the landuse categories to 'lu_area'
!     -------------------------------------------------------------------------
        
        do jpngr=minline_task,maxline_task
          if (lucrop.ne.0) then
            lu_area(lucrop,jpngr) = min(arealeft(jpngr),crop(jpngr))
            arealeft(jpngr) = arealeft(jpngr)-lu_area(lucrop,jpngr)
          end if
          if (lupasture.ne.0) then
            lu_area(lupasture,jpngr) = min(arealeft(jpngr),past(jpngr))
            arealeft(jpngr) = arealeft(jpngr)-lu_area(lupasture,jpngr)
          end if
          if (lubuilt.ne.0) then
            lu_area(lubuilt,jpngr) = min(arealeft(jpngr),built(jpngr))
            arealeft(jpngr) = arealeft(jpngr)-lu_area(lubuilt,jpngr)
          end if

#if _LU_generated_transitions
!     Only cropland, pasture, and builtup are relevant as targets in 'lu_area'.
!     Primary and secondary follow from land turnover and expansion/contraction
!     in cropland, pasture and builtup.
!     Area leftover is allocated to primary and secondary. But at this point,
!     lunat and lusecd are not yet defined.
c$$$  if (abs(arealeft(jpngr)-(prim(jpngr)+secd(jpngr))).gt.min_precision) then
c$$$  print*,'jpngr',jpngr
c$$$  print*,'arealeft,prim+secd',arealeft(jpngr),prim(jpngr)+secd(jpngr)
c$$$  print*,'diff',arealeft(jpngr)-(prim(jpngr)+secd(jpngr))
c$$$  write(0,*) 'area allocation problem'
c$$$  stop
c$$$  endif
          
!     With generated trans., start in first year (of spinup) without
!     any secondary and all non-agricultural area is primary.
          if (spinup.and.year.eq.1) then
            
            if (lusecd.ne.0) then
              lu_area(lusecd,jpngr) = 0.0d0
              arealeft(jpngr) = arealeft(jpngr)-lu_area(lusecd,jpngr)
            endif

            lu_area(lunat,jpngr) = max(arealeft(jpngr),0.0d0)

          else
!     Use primary and secondary from previous year. In combination with
!     updated crop, past, and builtup, this will violate area conservation.
!     shuffle2target will then be used to correct this.
            lu_area(lusecd,jpngr) = lu_area_old(lusecd,jpngr)
            lu_area(lunat,jpngr)  = lu_area_old(lunat,jpngr)
            
          endif
#else
          if (lusecd.ne.0) then
            lu_area(lusecd,jpngr) = min(arealeft(jpngr),secd(jpngr))
            arealeft(jpngr) = arealeft(jpngr)-lu_area(lusecd,jpngr)
          endif
          
!     use arealeft land primarily for peatland
!     XXX adjustment for TOPMODEL peatland implementation XXX
          if (lupeat.ne.0) then
            lu_area(lupeat,jpngr)=min(arealeft(jpngr),peatlandfrac(jpngr))
            arealeft(jpngr) = arealeft(jpngr)-lu_area(lupeat,jpngr)
          endif

          lu_area(lunat,jpngr)=max(arealeft(jpngr),0.0d0)
#endif
          
        enddo
          
#if _LU_generated_transitions==0
        do jpngr=minline_task,maxline_task
          if (abs(lu_area(lunat,jpngr)-prim(jpngr)).gt.1.2d-6) then
            write(0,*) '*******************************'
            write(0,*) 'WARNING:'/
     $           /'remaining primary land significantly '/
     $           /'different from input data in ',jpngr,
     $           prim(jpngr),lu_area(:,jpngr)
            write(0,*) '*******************************'    
            stop
          endif
        enddo
#endif

#if _fixedlandmask==0
!     spahni 2011-02-7: added total grid cell are scaling due to ice/sea cover
        do jpngr=minline_task,maxline_task
          lascale(jpngr)=lascale(jpngr)+dla(jpngr)
          sumlu = 0.0d0
          do lu=1,nlu
            if (lu.ne.luburied) then
              lu_area(lu,jpngr) = lascale(jpngr)*lu_area(lu,jpngr)
              sumlu = sumlu+lu_area(lu,jpngr)
            end if
          end do
          lu_area(luburied,jpngr) = max(1.0d0-sumlu,0.0d0)
        end do
#endif

        do jpngr=minline_task,maxline_task
          do lu=1,nlu
            if(lu_area(lu,jpngr).lt.0.d0)then
              print*,'jpngr',jpngr
              stop 'A: error in lu-area assignment, area negative'
            endif
          end do
        end do

!     In first year of spinup, lu_area_old is not available -> copy from lu_area
        if (spinup.and.init) then
          do jpngr=minline_task,maxline_task
            lu_area_old(:,jpngr) = lu_area(:,jpngr)
          enddo
        endif

!     xxx debug
        do jpngr=minline_task,maxline_task
          if (abs(land_fraction(jpngr)-sum(lu_area_old(:,jpngr))).gt.min_precision) then
            print*,'jpngr',jpngr
            print*,'land_fraction',land_fraction(jpngr)
            print*,'lu_area_old()',lu_area_old(:,jpngr)
            print*,'diff',land_fraction(jpngr)-sum(lu_area_old(:,jpngr))
            stop 'sum of lu_area_old not equal land_fraction'
          endif
        enddo
          
      endif                                                 !readlumap

!     /////////////////////////////////////////////////////////////////////////
!     DEFINE TRANSITION MATRIX
!     is read at the first year of the spinup and in all years of transient run
!     -------------------------------------------------------------------------
!     During spinup with transitions, read transition file of the first
!     transientsimulation year. During transient run, read trans. file every year.
!     -------------------------------------------------------------------------
      do jpngr=minline_task,maxline_task
        DF_tr(:,:,jpngr)      = 0.0d0
        DF_corr(:,:,jpngr)    = 0.0d0
        DF_harv(:,:,jpngr)    = 0.0d0
      enddo
      
!     xxx debug: must not be re-initialised, carries information from last call!
c$$$#if _wood_harvest
c$$$#if _harvest_byarea
c$$$      harv_a(:,:)       = 0.0d0
c$$$#else
c$$$      harv_m(:,:)       = 0.0d0
c$$$#endif
c$$$#endif

      readlutrmap = .false.
      calclutr = .false.

      if (spinup) then
        if (year.eq.spinupyears-spinup_trans_years) then
          readlutrmap = .true.
          readlutrmapyear=int(realyear)
        endif
        if (year.ge.spinupyears-spinup_trans_years) then
          calclutr = .true.
        endif
      else
        readlutrmap = .true.
        calclutr = .true.
        if (year.eq.spinupyears+1) then
          readlutrmapyear=int(realyear)
        else
          readlutrmapyear=int(realyear-1.d0)
        endif
      endif

c      print*,'readlutrmap, readlutrmapyear',readlutrmap,readlutrmapyear
      
      if (readlutrmap) then
        
#if _LU_generated_transitions == 0
!     /////////////////////////////////////////////////////////////////////////
!     READ TRANSITION FILES
!     -------------------------------------------------------------------------
!     Open land use transition map
!     -------------------------------------------------------------------------
        write(lu_trans_map_year,'(i4)') readlutrmapyear
        if (rcp.and.readlutrmapyear.ge.2005.0d0) then
          lu_trans_map_path =
     $         '../input_data/anthropogenic2d/landuse/transitions/'//
     $         trim(luscen_tr_map_filename_begin)//trim(lu_trans_map_year)//
     $         trim(lu_tr_map_filename_end)
        else
          lu_trans_map_path =
     $         '../input_data/anthropogenic2d/landuse/transitions/'//
     $         trim(lu_tr_map_filename_begin)//trim(lu_trans_map_year)//
     $         trim(lu_tr_map_filename_end)
        endif

        print*,'reading landuse transition file',trim(lu_trans_map_path)
        error=NF_OPEN((trim(lu_trans_map_path)),NF_NOWRITE,lutransfile)
        message=lu_trans_map_path
        if (error.ne.0) goto 400
        

!     Construct variable names as given in the input file
!     -------------------------------------------------------------------------
        do from=1,nlutr
          do to=1,nlutr
            lutrans_name(from,to) = trim(lustate(from))//
     $           trim(lustate(to))
          enddo
        enddo              
        
!     get data IDs
!     -------------------------------------------------------------------------
        do from=1,nlutr
          do to=1,nlutr
            error=NF_INQ_VARID(lutransfile,lutrans_name(from,to),
     $           lutrans_varid(from,to))
!     some transitions are not inlcuded in lutransfile
            if (error.ne.0) lutrans_varid(from,to)=-999
          enddo
        enddo

!     read fields for land use land use transitions
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          cdf_index(1)=ilon(jpngr)
          cdf_index(2)=ilat(jpngr)
!          print*,'cdf_index',cdf_index
          do from=1,nlutr
            do to=1,nlutr
!     avoid reading in transition categories that are not included in lutransfile
              if (lutrans_varid(from,to).ne.(-999)) then
                error = NF_GET_VAR1_DOUBLE
     $               (lutransfile,lutrans_varid(from,to),cdf_index,
     $               DF_tr(from,to,jpngr))
                if (error.ne.0) goto 999
!                print*,'transition successfully read from landuse file'
              endif
            enddo
          enddo
        enddo

#endif

#if _wood_harvest
!     /////////////////////////////////////////////////////////////////////////
!     READ HARVEST FILE
!     -------------------------------------------------------------------------
!     Open harvest file (all harvest-related variables in a separate file)
!     -------------------------------------------------------------------------
        if (.not.rcp.or.(rcp.and.int(realyear).le.2005)) then
          harvest_map_path ='../input_data/anthropogenic2d/landuse/'//trim(harvest_filename)
        else
          harvest_map_path ='../input_data/anthropogenic2d/landuse/'//trim(harvestscen_filename)
        endif

!        print*,'reading harvest file',trim(harvest_map_path)
        error=NF_OPEN(trim(harvest_map_path),NF_NOWRITE,harvestfile)
        message=lu_trans_map_path
        if (error.ne.0) goto 400

c     get dimension IDs
c----------------------------------------------------------------------
        dimname(1)='LONGITUDE' 
        dimname(2)='LATITUDE'
        dimname(3)='TIME'
        
        do i=1,3
          error = NF_INQ_DIMID(harvestfile,dimname(i),ludimid(i))
          message = 'harvestfile'
          if (error.ne.0) goto 500
        enddo

c     check coordinate information in harvestfile
c----------------------------------------------------------------------
        error = NF_INQ_DIMLEN(harvestfile,ludimid(1),dimlength)
        if (error.ne.0) goto 100
        if (dimlength.ne.nlon) goto 300
        
        error = NF_INQ_DIMLEN(harvestfile,ludimid(2),dimlength)
        if (error.ne.0) goto 100
        if (dimlength.ne.nlat) goto 300
        
c     get time domain
c----------------------------------------------------------------------
        error = NF_INQ_DIMLEN(harvestfile,ludimid(3),time_length)
        if (error.ne.0) goto 999
        allocate(time(time_length))
        error = NF_GET_VAR_INT(harvestfile,ludimid(3),time)
        if (error.ne.0) goto 999

c     get data IDs
c----------------------------------------------------------------------
#if _harvest_byarea
c     landuse transition associated with biomass harvested
        error=NF_INQ_VARID(harvestfile,'aharv',lutrans_harv_varid)
        if (error.ne.0) goto 999        
#else
c     landuse transition associated with biomass harvested from mature secondary forested land
        error=NF_INQ_VARID(harvestfile,'SBH1_EXT',lutrans_harv_varid(1))
        !error=NF_INQ_VARID(harvestfile,'sbh',lutrans_harv_varid(1))
        if (error.ne.0) goto 999        

c     landuse transition associated with biomass harvested from primary forested land
        !error=NF_INQ_VARID(harvestfile,'vbh',lutrans_harv_varid(2))
        error=NF_INQ_VARID(harvestfile,'VBH1_EXT',lutrans_harv_varid(2))
        if (error.ne.0) goto 999        

c     landuse transition associated with biomass harvested from young secondary forested land
        !error=NF_INQ_VARID(harvestfile,'sbh2',lutrans_harv_varid(3))
        error=NF_INQ_VARID(harvestfile,'SBH2_EXT',lutrans_harv_varid(3))
        if (error.ne.0) goto 999        

c     landuse transition associated with biomass harvested from primary non-forested land
        !error=NF_INQ_VARID(harvestfile,'vbh2',lutrans_harv_varid(4))
        error=NF_INQ_VARID(harvestfile,'VBH2_EXT',lutrans_harv_varid(4))
        if (error.ne.0) goto 999        

c     landuse transition associated with biomass harvested from secondary non-forested land
        !error=NF_INQ_VARID(harvestfile,'sbh3',lutrans_harv_varid(5))
        error=NF_INQ_VARID(harvestfile,'SBH3_EXT',lutrans_harv_varid(5))
        if (error.ne.0) goto 999        
#endif

        
!     get bracket
!     -------------------------------------------------------------------------
        ibracket(:)=0
        call getbracket(realyear,time,time_length,ibracket,imax)

!     read harvest data for bracketing years
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          cdf_index(1)=ilon(jpngr)
          cdf_index(2)=ilat(jpngr)
          do i=1,imax
            cdf_index(3)=ibracket(i)
!     read in harvest categories
#if _harvest_byarea
            error = NF_GET_VAR1_DOUBLE
     $           (harvestfile,lutrans_harv_varid,cdf_index,
     $           harv_a_orig(i))
            if (error.ne.0) goto 999
#else
            stop 'not implemented for harvest by mass'
#endif                                                      
          enddo

          if (imax.eq.2) then
!     interpolate and save the result in first slice
!     -------------------------------------------------------------------------
            harv_a_orig(1) = harv_a_orig(1)
     $           +(harv_a_orig(2)-harv_a_orig(1))
     $           /(time(ibracket(2))-time(ibracket(1)))
     $           *(realyear-time(ibracket(1)))
          endif

          harv_a(jpngr) = harv_a_orig(1)
        
        enddo

        error=NF_CLOSE(harvestfile)
#endif                                                      ! harvest

        
!     Check whether total area is conserved
        do jpngr=minline_task,maxline_task
          sum_DF = 0.0d0
          do lu=1,nlutr
            sum_DF = sum_DF + sum(DF_tr(:,lu,jpngr)) 
     $           - sum(DF_tr(lu,:,jpngr))
          enddo
          if (sum_DF.ge.1.d-7) then
            write(0,*)'Transition is not allowed.',jpngr
            write(0,*) sum_DF
            write(0,*) DF_tr(:,:,jpngr)
            stop
          endif
        enddo
        
      endif                                                 !readlutrmap

#if _LU_generated_transitions
      if (init) then
!     -------------------------------------------------------------------------
!     SUITABLE LAND MAP
!     -------------------------------------------------------------------------
        fsuit_map_path =
     $       '../input_data/anthropogenic2d/landuse/'//
     $       'land_suit_sage_lpjgr.nc'

#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
#endif
          print*,'suitable land file: ',fsuit_map_path
#if _parallel_mpi
        endif
#endif        
        error=NF_OPEN(trim(fsuit_map_path),NF_NOWRITE,
     $       fsuit_fileid)
        message=fsuit_map_path
        if (error.ne.0) goto 400

!     get dimension IDs
!     -------------------------------------------------------------------------
        dimname(1)='LONGITUDE' 
        dimname(2)='LATITUDE'
        
        do i=1,2
          error = NF_INQ_DIMID(fsuit_fileid,dimname(i),fsuitdimid(i))
          if (error.ne.0) goto 500
        enddo
        
!     check coordinate information in shifting cultivation file
!     -------------------------------------------------------------------------
        error = NF_INQ_DIMLEN(fsuit_fileid,fsuitdimid(1),dimlength)
        if (error.ne.0) goto 100
        if (dimlength.ne.nlon) goto 300
        
        error = NF_INQ_DIMLEN(fsuit_fileid,fsuitdimid(2),dimlength)
        if (error.ne.0) goto 100
        if (dimlength.ne.nlat) goto 300

!     get data IDs
!     -------------------------------------------------------------------------
        error=NF_INQ_VARID(fsuit_fileid,'SUIT',
     $       fsuit_varid)
        if (error.ne.0) goto 999        

!     read in shifting cultivation areas
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          cdf_index4(1)=ilon(jpngr)
          cdf_index4(2)=ilat(jpngr)
          cdf_index4(3)=1
          cdf_index4(4)=1
    
          error = NF_GET_VAR1_DOUBLE
     $         (fsuit_fileid,fsuit_varid,cdf_index4,fsuit)
          if (error.ne.0) goto 999

!     -------------------------------------------------------------------------
!     ACCESSIBILITY MAP
!     -------------------------------------------------------------------------
          accessible(jpngr) = min( land_fraction(jpngr), fsuit )
          inaccess(jpngr)   = max( 0.d0, land_fraction(jpngr) - fsuit )
          
        enddo        
        error=NF_CLOSE(fsuit_varid)
      endif                                                 !init

      if (calclutr) then
!     -------------------------------------------------------------------------
!     SHIFTING CULTIVATION MAP
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          shifting_cultivation(jpngr)=.false.
        enddo
        
        shifting_cult_map_path =
     $  '../input_data/anthropogenic2d/landuse/perm_lpjgr_holoLU2.nc'

#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
#endif
!          print*,'shifting cultivation file: ',shifting_cult_map_path
#if _parallel_mpi
        endif
#endif        
        error=NF_OPEN(trim(shifting_cult_map_path),NF_NOWRITE,
     $       shifting_cult_file)
        message=shifting_cult_map_path
        if (error.ne.0) goto 400

!     get dimension IDs
!     -------------------------------------------------------------------------
        dimname(1)='LONGITUDE' 
        dimname(2)='LATITUDE'
        dimname(3)='TIME'
        
        do i=1,3
          error = NF_INQ_DIMID(shifting_cult_file,dimname(i),scultdimid(i))
          message = 'shifting_cult_file'
          if (error.ne.0) goto 500
        enddo
        
!     check coordinate information in shifting cultivation file
!     -------------------------------------------------------------------------
        error = NF_INQ_DIMLEN(shifting_cult_file,scultdimid(1),dimlength)
        if (error.ne.0) goto 100
        if (dimlength.ne.nlon) goto 300
        
        error = NF_INQ_DIMLEN(shifting_cult_file,scultdimid(2),dimlength)
        if (error.ne.0) goto 100
        if (dimlength.ne.nlat) goto 300

!     get data IDs
!     -------------------------------------------------------------------------
        error=NF_INQ_VARID(shifting_cult_file,'PERM',
     $       shifting_cult_varid)
        if (error.ne.0) goto 999        
        
c     get time domain
c----------------------------------------------------------------------
        error = NF_INQ_DIMLEN(shifting_cult_file,scultdimid(3),time_length)
        if (error.ne.0) goto 999
        allocate(time(time_length))
        error = NF_GET_VAR_INT(shifting_cult_file,scultdimid(3),time)
        if (error.ne.0) goto 999

!     get time index
        itime=1
        do while (int(time(itime+1)).lt.int(realyear))
          itime=itime+1
        end do
        cdf_index(3)=itime
        
!     read in shifting cultivation areas
!     -------------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          cdf_index(1)=ilon(jpngr)
          cdf_index(2)=ilat(jpngr)
          error = NF_GET_VAR1_DOUBLE
     $         (shifting_cult_file,shifting_cult_varid,cdf_index,
     $         perm)
          if (error.ne.0) goto 999
          if (perm.eq.1.0d0) shifting_cultivation(jpngr) = .true.
                    
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

        enddo        
        error=NF_CLOSE(shifting_cult_file)
      endif                                                 !calclutr

!     -------------------------------------------------------------------------
!     GENERATE SHIFTING CULTIVATION TRANSITION MATRIX
!     Additional transitions to meet net area changes are added by shuffle2target
!     -------------------------------------------------------------------------

      do jpngr=minline_task,maxline_task
        lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
      enddo
      
      do jpngr=minline_task,maxline_task

        luold(:) = lu_area_old(:,jpngr)
        
!     xxx debug
        if (abs(land_fraction(jpngr)-sum(lu_area_tr(:,jpngr))).gt.min_precision) then
          print*,'jpngr',jpngr
          stop 'sum of lu_area_tr not equal land_fraction, 0'
        endif

        if (.not.spinup) then        
!     ADD TRANSITIONS TO SATISFY NET CHANGES
!     no net changes during spinup
!     -------------------------------------------------------------------------

c          print*,'deriving matrix for net changes'
          
          error=0
          control=1
 444      DF_tmp(:,:) = DF_tr(:,:,jpngr)
          call shuffle2target(
     $         lu_area_tr(1:nlutr,jpngr),
     $         lu_area(1:nlutr,jpngr),
     $         DF_corr(:,:,jpngr),
     $         DF_tmp(:,:),
     $         luold(1:nlutr),
     $         nlutr,
     $         jpngr,
     $         error
     $         )
          if (error.eq.1) then
            print*,'jpngr',jpngr
            stop "ABORTED. area conserv. in shuffle2target."
          else if (error.eq.2) then
            control=control+1
            !write(0,*) '1st approach failed. Apply 2nd approach.'
            !write(0,*) 'jpngr,control',jpngr,control
            DF_corr(:,:,jpngr)  = 0.0d0
            DF_tr(:,:,jpngr)    = 0.0d0
            lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
            luold(:)            = lu_area_old(:,jpngr)
            if (control.gt.3) then
              stop 'pioblem'
            else
              goto 444
            endif
          endif
        endif                                               !.not.spinup
        
        do lu=lucrop,lubuilt
          if (abs(lu_area_tr(lu,jpngr)-lu_area(lu,jpngr)).gt.min_precision) then
            print*,'jpngr,lu',jpngr,lu
            print*,'target',lu_area(lu,jpngr)
            print*,'init',lu_area_tr(lu,jpngr)
            stop 'OUTSIDE A: target not achieved'
          endif
        enddo

!     xxx debug
        if (abs(land_fraction(jpngr)-sum(lu_area_tr(:,jpngr))).gt.min_precision) then
          print*,'jpngr',jpngr
          stop 'sum of lu_area_tr not equal land_fraction, A'
        endif

!     xxx debug
        do from=1,nlu
          do to=1,nlu
            if (DF_tr(from,to,jpngr).ne.0.0d0) then
              print*,'DF_tr is not 0:',DF_tr(from,to,jpngr)
            endif
          enddo
        enddo

        
        if (calclutr) then
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
          
        endif                                               !calclutr
        
!     xxx debug
        do lu=lucrop,lubuilt
          if (abs(lu_area_tr(lu,jpngr)-lu_area(lu,jpngr)).gt.min_precision) then
            print*,'jpngr,lu',jpngr,lu
            print*,'target',lu_area(lu,jpngr)
            print*,'init',lu_area_tr(lu,jpngr)
            stop 'OUTSIDE B: target not achieved'
          endif
        enddo

        do lu=1,nlu
          if (lu_area_tr(lu,jpngr).lt.0.0d0) then
            print*,'lu, jpngr, lu_area_tr',lu,jpngr,lu_area_tr(lu,jpngr)
            stop 'negative lu_area_tr'
          endif
        enddo

        do from=1,nlu
          do to=1,nlu
            if ((DF_tr(from,to,jpngr)+DF_corr(from,to,jpngr)).lt.0.0d0) then
              print*,'from,to,jpngr,DF_tr,DF_corr',from,to,jpngr,DF_tr(from,to,jpngr),DF_corr(from,to,jpngr)
            endif
          enddo
        enddo

!     xxx debug
        if (abs(land_fraction(jpngr)-sum(lu_area_tr(:,jpngr))).gt.min_precision) then
          print*,'jpngr',jpngr
          print*,'land_fraction,sum(lu_area)',land_fraction(jpngr),sum(lu_area(:,jpngr))
          stop 'sum of lu_area not equal land_fraction, B'
        endif

        
      enddo                                                 !jpngr

#else                                                       ! LU_generated_transitions == 0

      if (readlutrmap) then
!     /////////////////////////////////////////////////////////////////////////
!     CORRECT TRANSITION MATRIX TO BE CONSISTENT WITH STATES
!     -------------------------------------------------------------------------
!     FIRST CORRECTION
!     During spinup, do not involve 'secondary'. Replace all transitions affecting
!     'secd' by transitions from/to primary. (secondary remains zero throughout)
!     spinup. First year of transient is handled the same as spinup (no net changes).
        if (spinup.or.year.eq.1) then
          do jpngr=minline_task,maxline_task          

            DF_tr(lunat,:,jpngr) = DF_tr(lunat,:,jpngr) + DF_tr(lusecd,:,jpngr)
            DF_tr(lusecd,:,jpngr) = 0.0d0

            DF_tr(:,lunat,jpngr) = DF_tr(:,lunat,jpngr) + DF_tr(:,lusecd,jpngr)
            DF_tr(:,lusecd,jpngr) = 0.0d0
            
          enddo
        endif
        
!     SECOND CORRECTION
!     Make sure that no transition can occur where no land is available. Problem in
!     original data: An area of land can transition between land use classes several
!     times in one year. E.g., there can be a transition into and out of land use class
!     'lu', even when 'lu_area_old(lu)' is zero.
        do jpngr=minline_task,maxline_task
          lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
        enddo
        
        do jpngr=minline_task,maxline_task          
          do from=1,nlutr
            do to=1,nlutr
!     xxx debug
              if (DF_tr(from,to,jpngr).lt.0.0d0) then
                print*,'jpngr,from,to',jpngr,from,to
                print*,'A, negative DF_tr',DF_tr(from,to,jpngr)
                print*,'lu_area_tr',lu_area_tr(from,jpngr)
              endif
              DF_tr(from,to,jpngr) = max(
     $             min(lu_area_tr(from,jpngr),
     $             DF_tr(from,to,jpngr)),
     $             0.0d0
     $             )
              lu_area_tr(from,jpngr) = lu_area_tr(from,jpngr) - DF_tr(from,to,jpngr)
            enddo
          enddo
        enddo
          
!     calculate lu_area_tr based on lu_area_old and the read-in/first corrected transition matrix DF_tr
        do jpngr=minline_task,maxline_task        
          lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
        enddo
        
        do jpngr=minline_task,maxline_task
          luold(:) = lu_area_old(:,jpngr)
          do from=1,nlutr
            do to=1,nlutr
              lu_area_tr(to,jpngr) = lu_area_tr(to,jpngr) + DF_tr(from,to,jpngr)
              lu_area_tr(from,jpngr) = lu_area_tr(from,jpngr) - DF_tr(from,to,jpngr)
              luold(from) = luold(from) - DF_tr(from,to,jpngr)          
            enddo
          enddo
          
!     Find additional/reduce transitions necessary to fulfill next year's states after transtion from data
          make_correct = .false.
          do lu=1,nlu
            if (abs(lu_area_tr(lu,jpngr)-lu_area(lu,jpngr)).gt.min_precision) then
              make_correct = .true.
              exit
            endif
          enddo
          if (make_correct) then

c            print*,'correcting transition matrix'
                  
!     THIRD CORRECTION
!     Determine correction transition matrix 'DF_corr' to satisfy
!     lu_area = lu_area_old + DF_tr + DF_corr
 444        error=0
            DF_tmp(:,:) = DF_tr(:,:,jpngr)
            call shuffle2target(
     $           lu_area_tr(1:nlutr,jpngr),
     $           lu_area(1:nlutr,jpngr),
     $           DF_corr(:,:,jpngr),
     $           DF_tmp(:,:),
     $           luold(1:nlutr),
     $           nlutr,
     $           jpngr,
     $           error
     $           )
            if (error.eq.1) then
              print*,'jpngr',jpngr
              stop "ABORTED. area conserv. in shuffle2target."
            else if (error.eq.2) then
              write(0,*) 'uncorrectable matrix -> using only net trans.'
              DF_corr(:,:,jpngr)  = 0.0d0
              DF_tr(:,:,jpngr)    = 0.0d0
              lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
              luold(:)            = lu_area_old(:,jpngr)
              goto 444
            endif

!     xxx debug
            do from=1,nlutr
              do to=1,nlutr
                if (DF_tr(from,to,jpngr)+DF_corr(from,to,jpngr).lt.0.0d0) then
                  print*,'jpngr,from,to',jpngr,from,to
                  print*,'B, negative DF_tr',DF_tr(from,to,jpngr)
                endif
              enddo
            enddo
            
            llu=1
            do while (llu.le.nlu)
              if (abs(lu_area_tr(llu,jpngr)-lu_area(llu,jpngr)).gt.min_precision) then
                print*,'ilon,ilat,jpngr',ilon(jpngr),ilat(jpngr),jpngr
                print*,'lu_area',lu_area(:,jpngr)
                print*,'lu_area_old',lu_area_old(:,jpngr)
                print*,'lu_area_tr',lu_area_tr(:,jpngr)
                print*,'DF and lu_area are still not consistent. A'
                goto 666
              else
                llu=llu+1
              endif
            enddo
            
!     xxx this should work !!!
!     lu_area_old + DF_tr + DF_corr should yield lu_area
            lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
            do from=1,nlu
              do to=1,nlu
                lu_area_tr(to,jpngr) = lu_area_tr(to,jpngr)
     $               + DF_tr(from,to,jpngr) + DF_corr(from,to,jpngr)
                lu_area_tr(from,jpngr) = lu_area_tr(from,jpngr)
     $               - (DF_tr(from,to,jpngr) + DF_corr(from,to,jpngr))
              enddo
            enddo
            do lu=1,nlu
              if (abs(lu_area_tr(lu,jpngr)-lu_area(lu,jpngr)).gt.min_precision) then
                print*,'DF and lu_area are still not consistent. B'
                print*,'ilon,ilat,jpngr',ilon(jpngr),ilat(jpngr),jpngr
                print*,'lu_area_old',lu_area_old(:,jpngr)
                print*,'DF_tr'
                do llu=1,nlu
                  print*,DF_tr(llu,:,jpngr)
                enddo
                print*,'DF_corr'
                do llu=1,nlu
                  print*,DF_corr(llu,:,jpngr)
                enddo
                print*,'lu_area_tr',lu_area_tr(:,jpngr)
                print*,'lu_area',lu_area(:,jpngr)
              endif
            enddo

          endif                                             !make_correct
          
        enddo                                               !minline_task,maxline_task
c       print*,'finished correcting'
        
#if _fixedlandmask==0
        write(0,*) 'transition in combination with'
        write(0,*) 'changing land mask not yet implemented'
        stop
#endif

      endif                                                 !readlutrmap/calclutr

#endif
      
#if _wood_harvest
      if (calclutr) then
!     /////////////////////////////////////////////////////////////////////////
!     DETERMINE AREA AFFECTED BY WOOD HARVEST
!     -------------------------------------------------------------------------
!     Transitions associated with harvest depend on the dynamical C storage in
!     affected cells, and are thus not stationary even during spinup. Therefore,
!     DF_harv has to be re-calculated each year during spinup where transitions
!     are turned on. 
!     -------------------------------------------------------------------------
!     Remove all "away-"transitions to guarantee that harvest does not cause
!     negative areas
        do jpngr=minline_task,maxline_task
          lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
        enddo
        
        do jpngr=minline_task,maxline_task          
          do from=1,nlutr
            do to=1,nlutr

              lu_area_tr(from,jpngr) = lu_area_tr(from,jpngr) -
     $             (DF_tr(from,to,jpngr)+DF_corr(from,to,jpngr))
              
!     Correct negative lu_area_tr (necessary only when using generated transitions.
!     Setting lu_area_tr to zero violates area conservation, but not withing
!     pre-defined allowed uncertainty (min_precision).
              if (lu_area_tr(from,jpngr).lt.0.0d0) then
                if (-1.d0*lu_area_tr(from,jpngr).gt.min_precision) then
                  stop 'negative lu_area_tr'
                else
                  lu_area_tr(from,jpngr) = 0.0d0
                endif
              endif
              
            enddo
          enddo
        enddo

#if _harvest_byarea        
!     CORRECTION limit harvested area to actually available area after transitions (lu_area_tr)
!     and determine harvested biomass in gC per gridcell
        do jpngr=minline_task,maxline_task
          aharvest(jpngr,:)=0.0d0
          mharvest(jpngr,:)=0.0d0
        enddo
        
        do jpngr=minline_task,maxline_task          
          if (lu_area_tr(lunat,jpngr).lt.0.0d0) then
            if (abs(lu_area_tr(lunat,jpngr)).gt.min_precision) then
              print*,'jpngr,lu',jpngr,lu
              print*,'lu_area_tr:',lu_area_tr(lunat,jpngr)
              stop
            else
              lu_area_tr(lunat,jpngr) = 0.0d0
            endif
          endif
          req = harv_a(jpngr)
          avl = max( 0.d0, lu_area_tr(lunat,jpngr) - inaccess(jpngr))
          con = min(req,avl)
          req = req-con
          aharvest(jpngr,lunat) = aharvest(jpngr,lunat) + con
          DF_harv(lunat,lusecd,jpngr) = DF_harv(lunat,lusecd,jpngr) + con
          lu_area_tr(lunat,jpngr) = lu_area_tr(lunat,jpngr) - con
          if (aharvest(jpngr,lunat).lt.0.0d0) then
            print*,'jpngr,lu',jpngr,lunat
            print*,'aharvest:',aharvest(jpngr,lunat)
            stop
          endif
!     If not enough land available in this lu category, get land from other (primary/secondary).
          if (req.gt.min_precision) then
            if (lu_area_tr(lusecd,jpngr).lt.0.0d0) then
              if (abs(lu_area_tr(lusecd,jpngr)).gt.min_precision) then
                print*,'jpngr,lusecd',jpngr,lusecd
                print*,'lu_area_tr:',lu_area_tr(lusecd,jpngr)
                stop
              else
                lu_area_tr(lusecd,jpngr) = 0.0d0
              endif
            endif
            avl = lu_area_tr(lusecd,jpngr)
            con = min(req,avl)
            req = req-con
            aharvest(jpngr,lusecd) = aharvest(jpngr,lusecd) + con
            DF_harv(lusecd,lusecd,jpngr) = DF_harv(lusecd,lusecd,jpngr) + con
            lu_area_tr(lusecd,jpngr) = lu_area_tr(lusecd,jpngr) - con
            if (aharvest(jpngr,lusecd).lt.0.0d0) then
              print*,'jpngr,lusecd',jpngr,lusecd
              print*,'aharvest:',aharvest(jpngr,lusecd)
              stop
            endif
          endif
      
!     Sum up harvested mass on harvested area
          do pft=1,npft
            if (lu_category(pft).eq.lunat) then
#if _harvest_allbiomass
              mharvest(jpngr,lunat) = mharvest(jpngr,lunat)
     $             + (hm_ind(pft,jpngr,1) + sm_ind(pft,jpngr,1)
     $             + lm_ind(pft,jpngr,1) + rm_ind(pft,jpngr,1))
     $             *nind(pft,jpngr)
     $             *cell_area(jpngr)*aharvest(jpngr,lunat)
#else
              mharvest(jpngr,lunat) = mharvest(jpngr,lunat)
     $             + hm_ind(pft,jpngr,1)*nind(pft,jpngr)
     $             *cell_area(jpngr)*aharvest(jpngr,lunat)
#endif
c$$$              if (mharvest(jpngr,lunat).gt.0.0d0) then
c$$$                print*,'jpngr,mharvest',jpngr,mharvest(jpngr,lunat)
c$$$              end if 
            endif
            if (lu_category(pft).eq.lusecd) then
#if _harvest_allbiomass
              mharvest(jpngr,lusecd) = mharvest(jpngr,lusecd)
     $             + (hm_ind(pft,jpngr,1) + sm_ind(pft,jpngr,1)
     $             + lm_ind(pft,jpngr,1) + rm_ind(pft,jpngr,1))
     $             *nind(pft,jpngr)
     $             *cell_area(jpngr)*aharvest(jpngr,lusecd)
#else
              mharvest(jpngr,lusecd) = mharvest(jpngr,lusecd)
     $             + hm_ind(pft,jpngr,1)*nind(pft,jpngr)
     $             *cell_area(jpngr)*aharvest(jpngr,lusecd)
#endif
            endif

c$$$!     Add harvested mass to product pools
c$$$!     -----------------------------------
c$$$            lu=lu_category(pft)
c$$$            
c$$$c     (cf. code below to distribute litter)
c$$$!     what if no pft is present?...
c$$$            hmfrac=0.d0
c$$$            smfrac=0.d0
c$$$            lmfrac=0.d0
c$$$            
c$$$            if(aharvest(jpngr,lu).gt.0.d0) then                        !lu shrinking
c$$$              if(present(pft,jpngr)) then                   !vegetation
c$$$c     Convert vegetation to litter
c$$$c     products: divert some fraction into products here
c$$$c     NOTE: products are in units of gC/cell (not gC/lu-area)
c$$$                do prod=1,nproduct
c$$$                  if((lu_product(prod).eq.lu).and.          !product assigned to this lu-category
c$$$     $                 (pfraction(prod).gt.0.d0)) then      !product gets input...
c$$$                    if(porigin(prod).eq.'hm') then          !             ...from this plant pool
c$$$                      call addc(jpngr,
c$$$     $                     products(prod,jpngr,:),
c$$$     $                     aharvest(jpngr,lunat)*nind(pft,jpngr)
c$$$     $                     *hm_ind(pft,jpngr,1)*pfraction(prod),
c$$$     $                     hm_ind(pft,jpngr,2:ncvar)
c$$$     $                     )
c$$$                      hmfrac=hmfrac+pfraction(prod)
c$$$#if _DyN
c$$$                      call addn(jpngr,
c$$$     $                     products(prod,jpngr,:),
c$$$     $                     aharvest(jpngr,lunat)*nind(pft,jpngr)
c$$$     $                     *hm_ind(pft,jpngr,n_id)*pfraction(prod)
c$$$     $                     )
c$$$#endif
c$$$                    elseif(porigin(prod).eq.'sm')then       ! ...or from this...
c$$$                      call addc(jpngr,
c$$$     $                     products(prod,jpngr,:),
c$$$     $                     aharvest(jpngr,lunat)*nind(pft,jpngr)
c$$$     $                     *sm_ind(pft,jpngr,1)*pfraction(prod),
c$$$     $                     sm_ind(pft,jpngr,2:ncvar)
c$$$     $                     )
c$$$                      smfrac=smfrac+pfraction(prod)
c$$$#if _DyN
c$$$                      call addn(jpngr,
c$$$     $                     products(prod,jpngr,:),
c$$$     $                     aharvest(jpngr,lunat)*nind(pft,jpngr)
c$$$     $                     *sm_ind(pft,jpngr,n_id)*pfraction(prod)
c$$$     $                     )
c$$$#endif
c$$$                    elseif(porigin(prod).eq.'lm')then       !... or from this
c$$$                      call addc(jpngr,
c$$$     $                     products(prod,jpngr,:),
c$$$     $                     aharvest(jpngr,lunat)*nind(pft,jpngr)
c$$$     $                     *lm_ind(pft,jpngr,1)*pfraction(prod),
c$$$     $                     lm_ind(pft,jpngr,2:ncvar)
c$$$     $                     )
c$$$#if _DyN
c$$$                      call addn(jpngr,
c$$$     $                     products(prod,jpngr,:),
c$$$     $                     aharvest(jpngr,lunat)*nind(pft,jpngr)
c$$$     $                     *lm_ind(pft,jpngr,n_id)*pfraction(prod)
c$$$     $                     )xxx
c$$$#endif
c$$$                      lmfrac=lmfrac+pfraction(prod)
c$$$                    endif
c$$$                  endif
c$$$                enddo
c$$$              endif
c$$$            endif

          enddo  
        enddo
#else
!     Fulfill harvest (restricted to the gridcell where harvest is required)
!     Sum up heartwood mass (in g C) in each gridcell. Biomass available is de-
!     termined based on 'lu_area_tr', landuse area fractions *after* transition.
        do jpngr=minline_task,maxline_task
          biomass(:,jpngr)=0.0d0
        enddo
        
        do jpngr=minline_task,maxline_task
          do pft=1,npft
            if (present(pft,jpngr).and. 
     $           (lu_category(pft).eq.lunat.or.
     $           lu_category(pft).eq.lusecd)) then
#if _harvest_oldgrowth
              if (tree(pft)) then
#endif
#if _harvest_allbiomass
                biomass(lu_category(pft),jpngr)             ! [biomass] = g
     $               =biomass(lu_category(pft),jpngr)
     $               +(hm_ind(pft,jpngr,1)+rm_ind(pft,jpngr,1)
     $               +sm_ind(pft,jpngr,1)+lm_ind(pft,jpngr,1)) ! [hm_ind] = g/m2/ind.
     $               *nind(pft,jpngr)*cell_area(jpngr)      ! [nind] = ind., [cell_area] = m2
     $               *lu_area_tr(lu_category(pft),jpngr)    ! [lu_area_tr] = 1
#else
                biomass(lu_category(pft),jpngr)             ! [biomass] = g
     $               =biomass(lu_category(pft),jpngr)
     $               +hm_ind(pft,jpngr,1)                   ! [hm_ind] = g/m2/ind.
     $               *nind(pft,jpngr)*cell_area(jpngr)      ! [nind] = ind., [cell_area] = m2
     $               *lu_area_tr(lu_category(pft),jpngr)    ! [lu_area_tr] = 1
#endif
#if _harvest_oldgrowth
              endif
#endif
            endif
          enddo
        enddo

!     ---------------------------------------------------------------------

!     Determine area harvested in order to fulfill harvested mass
        do jpngr=minline_task,maxline_task
          required(jpngr,:)=harv_m(jpngr,:)
          available(:,jpngr)=biomass(:,jpngr)
#if _harvest_mortality
          harv_mort(jpngr,:)=0.0d0
#endif
#if _harvest_cpexpansion
          harv_felled(jpngr,:)=0.0d0
#endif
        enddo

        do jpngr=minline_task,maxline_task
          do lu=1,nlunat+nlusecd                            !only prim and secd
#if _harvest_mortality
            if (required(jpngr,lu).gt.0.0d0) then
              ! print*,'.......'
              ! print*,'get mortality for jpngr,lu',jpngr,lu
              ! print*,'required',required(jpngr,lu)/1.d12
!     Satisfy as much of the required harvest mass with "natural" hm-turnover
!     as possible. Add remaining hm in hm_store to litter (would otherwise have
!     been added to litter in mortality.F)
              if (lu.eq.1) then
                hm_store_lu(:) = 0.0d0
                do pft=1,npft
!     Sum up hm_stored in primary lu-category and convert to gC/gridcell
                  if (lu_category(pft).eq.lunat) then
                    hm_store_lu(lunat) = hm_store_lu(lunat)
     $                   + hm_store(pft,jpngr,1)
     $                   *cell_area(jpngr)*lu_area_old(lunat,jpngr)
                  endif
                  
!     Sum up hm_stored in secondary lu-category and convert to gC/gridcell
                  if (lu_category(pft).eq.lusecd) then
                    hm_store_lu(lusecd) = hm_store_lu(lusecd)
     $                   + hm_store(pft,jpngr,1)
     $                   *cell_area(jpngr)*lu_area_old(lusecd,jpngr)
                  endif
                enddo
              endif

              ! print*,'recalculated hm_store_lu',hm_store_lu/1.d12
              
              ! print*,'in hm_store_lu(lu)    ',hm_store_lu(lu)/1.d12
              if (hm_store_lu(lu).gt.0.0d0) then
                harv_mort(jpngr,lu) = min(required(jpngr,lu),hm_store_lu(lu))
                scale_hm_store(lu) = max(0.0d0,
     $               (hm_store_lu(lu)-harv_mort(jpngr,lu))/hm_store_lu(lu))
                ! print*,'scale store by        ',scale_hm_store(lu)
                hm_store_lu(lu) = hm_store_lu(lu)-harv_mort(jpngr,lu)
                required(jpngr,lu) = required(jpngr,lu)-harv_mort(jpngr,lu)
                do pft=1,npft
                  if (lu_category(pft).eq.lu) then
                    do prod=1,nproduct
                      if ((lu_product(prod).eq.lu).and.
     $                     (pfraction(prod).gt.0.0d0)) then
                        if (porigin(prod).eq.'hm') then
                          call addc(jpngr,
     $                         products(prod,jpngr,:),
     $                         hm_store(pft,jpngr,1)
     $                         *lu_area_old(lu,jpngr)
     $                         *pfraction(prod)
     $                         *(1.d0-scale_hm_store(lu)),
     $                         hm_store(pft,jpngr,2:ncvar)
     $                         )
#if _DyN
                          call addn(jpngr,
     $                         products(prod,jpngr,:),
     $                         hm_store(pft,jpngr,n_id)
     $                         *lu_area_old(lu,jpngr)
     $                         *pfraction(prod)
     $                         *(1.d0-scale_hm_store(lu))
     $                         )
#endif
                        endif
                      endif
                    enddo
                    hm_store(pft,jpngr,1) = hm_store(pft,jpngr,1)*scale_hm_store(lu)
#if _DyN
                    hm_store(pft,jpngr,n_id) = hm_store(pft,jpngr,n_id)*scale_hm_store(lu)
#endif
                  endif
                enddo
              endif
              
!     If not all harvest requirement is satisfied, get it from other lu category
              if (required(jpngr,lu).gt.min_precision) then
                if (lu.eq.lunat) llu=lusecd
                if (lu.eq.lusecd) llu=lunat
                ! print*,'in hm_store_lu(llu),llu     ',hm_store_lu(llu)/1.d12,llu
                if (hm_store_lu(llu).gt.min_precision) then
                  ! print*,'still required(jpngr,lu), lu',required(jpngr,lu)/1.d12,lu
                  harv_mort(jpngr,llu) = min(required(jpngr,lu),hm_store_lu(llu))
                  scale_hm_store(llu) = max(0.0d0,
     $                 (hm_store_lu(llu)-harv_mort(jpngr,llu))/hm_store_lu(llu))
                  ! print*,'scale store by              ',scale_hm_store(llu)
                  hm_store_lu(llu) = hm_store_lu(llu)-harv_mort(jpngr,llu)
                  required(jpngr,lu) = required(jpngr,lu)-harv_mort(jpngr,llu)
                  do pft=1,npft
                    if (lu_category(pft).eq.llu) then
                      do prod=1,nproduct
                        if ((lu_product(prod).eq.llu).and.
     $                       (pfraction(prod).gt.0.0d0)) then
                          if (porigin(prod).eq.'hm') then
                            call addc(jpngr,
     $                           products(prod,jpngr,:),
     $                           hm_store(pft,jpngr,1)
     $                           *lu_area_old(llu,jpngr)
     $                           *pfraction(prod)
     $                           *(1.d0-scale_hm_store(llu)),
     $                           hm_store(pft,jpngr,2:ncvar)
     $                           )
#if _DyN
                            call addn(jpngr,
     $                           products(prod,jpngr,:),
     $                           hm_store(pft,jpngr,n_id)
     $                           *lu_area_old(llu,jpngr)
     $                           *pfraction(prod)
     $                           *(1.d0-scale_hm_store(llu))
     $                           )
#endif
                          endif
                        endif
                      enddo
                      hm_store(pft,jpngr,1) = hm_store(pft,jpngr,1)*scale_hm_store(llu)
#if _DyN
                      hm_store(pft,jpngr,n_id) = hm_store(pft,jpngr,n_id)*scale_hm_store(llu)
#endif
                    endif
                  enddo
                endif
              endif

#if _harvest_onlynat
!     Harvest only natural heartwood (biomass) turnover.
              required(jpngr,lu) = 0.0d0
#endif              
              ! print*,'now in hm_store_lu',hm_store_lu/1.d12
              
            endif                                           !required.gt.0
#endif     
#if _harvest_cpexpansion
            if (required(jpngr,lu).gt.0.0d0) then
              ! print*,'.......'
              ! print*,'get felled hm for jpngr,lu',jpngr,lu
              ! print*,'required',required(jpngr,lu)/1.d12
!     Satisfy as much of the required harvest mass with hm felled due to cropland/
!     pasture expansion as possilble.
              if (lu.eq.1) then
                hm_felled(:) = 0.0d0
                do pft=1,npft
!     Sum up hm felled due to cropland and pasture expansion                              
                  if (lu_category(pft).eq.lunat) then
#if _harvest_allbiomass
                    hm_felled(lunat) = hm_felled(lunat)
     $                   + (hm_ind(pft,jpngr,1)+sm_ind(pft,jpngr,1)
     $                   +  rm_ind(pft,jpngr,1)+lm_ind(pft,jpngr,1))
     $                   *nind(pft,jpngr)*cell_area(jpngr)
     $                   *(DF_tr(lunat,lucrop,jpngr)+DF_corr(lunat,lucrop,jpngr)
     $                   + DF_tr(lunat,lupasture,jpngr)+DF_corr(lunat,lupasture,jpngr)) ! [hm_felled]=g/gridcell
#else
                    hm_felled(lunat) = hm_felled(lunat) + hm_ind(pft,jpngr,1)
     $                   *nind(pft,jpngr)*cell_area(jpngr)
     $                   *(DF_tr(lunat,lucrop,jpngr)+DF_corr(lunat,lucrop,jpngr)
     $                   + DF_tr(lunat,lupasture,jpngr)+DF_corr(lunat,lupasture,jpngr)) ! [hm_felled]=g/gridcell
#endif
                  endif                  
                  if (lu_category(pft).eq.lusecd) then
#if _harvest_allbiomass
                    hm_felled(lusecd) = hm_felled(lusecd)
     $                   + (hm_ind(pft,jpngr,1)+sm_ind(pft,jpngr,1)
     $                   +  rm_ind(pft,jpngr,1)+lm_ind(pft,jpngr,1))
     $                   *nind(pft,jpngr)*cell_area(jpngr)
     $                   *(DF_tr(lusecd,lucrop,jpngr)+DF_corr(lusecd,lucrop,jpngr)
     $                   + DF_tr(lusecd,lupasture,jpngr)+DF_corr(lusecd,lupasture,jpngr)) ! [hm_felled]=g/gridcell
#else
                    hm_felled(lusecd) = hm_felled(lusecd) + hm_ind(pft,jpngr,1)
     $                   *nind(pft,jpngr)*cell_area(jpngr)
     $                   *(DF_tr(lusecd,lucrop,jpngr)+DF_corr(lusecd,lucrop,jpngr)
     $                   + DF_tr(lusecd,lupasture,jpngr)+DF_corr(lusecd,lupasture,jpngr)) ! [hm_felled]=g/gridcell
#endif
                  endif
                enddo
              endif

              if (hm_felled(lu).gt.0.0d0) then
                harv_felled(jpngr,lu) = min(required(jpngr,lu),hm_felled(lu))
                ! print*,'felled hm, lu',hm_felled(lu)/1.d12
                required(jpngr,lu) = required(jpngr,lu)-harv_felled(jpngr,lu)
                hm_felled(lu) = hm_felled(lu)-harv_felled(jpngr,lu)
              endif     
!     If not all harvest requirement is satisfied, get it from other lu category
              if (required(jpngr,lu).gt.min_precision) then
                if (lu.eq.lunat) llu=lusecd
                if (lu.eq.lusecd) llu=lunat
                !print*,'in hm_felled(llu),llu     ',hm_felled(llu)/1.d12,llu
                if (hm_felled(llu).gt.min_precision) then
                  ! print*,'still required(jpngr,lu), lu',required(jpngr,lu)/1.d12,lu
                  harv_felled(jpngr,llu) = min(required(jpngr,lu),hm_felled(llu))
                  ! print*,'felled hm, llu',hm_felled(llu)/1.d12
                  required(jpngr,lu) = required(jpngr,lu)-harv_felled(jpngr,llu)
                  hm_felled(llu) = hm_felled(llu)-harv_felled(jpngr,llu)
                endif
              endif

              ! print*,'now in hm_felled',hm_felled/1.d12

            endif
#endif
!     Close lu-loop. Important, e.g. if all dead wood is in secondary.
          enddo                                             !lu=1,nlunat+nlusecd

          do lu=1,nlunat+nlusecd
            
!     xxx debug
c            print*,'D: lu, required  ', lu, required(jpngr,lu)
c            print*,'D: lu, available ', lu, available(lu,jpngr)

            if (required(jpngr,lu).gt.0.0d0) then
              removed=min(required(jpngr,lu),available(lu,jpngr))
!     update what is still neede and what is still there
              required(jpngr,lu)=required(jpngr,lu)-removed
              available(lu,jpngr)=available(lu,jpngr)-removed

              if (required(jpngr,lu).gt.min_precision) then
                if (lu.eq.lunat) llu=lusecd
                if (lu.eq.lusecd) llu=lunat
                removed=min(required(jpngr,lu),available(llu,jpngr))
!     update what is still neede and what is still there
                required(jpngr,lu)=required(jpngr,lu)-removed
                available(llu,jpngr)=available(llu,jpngr)-removed                
              endif              
            endif                                           ! required.gt.0.0d0            
          enddo                                             ! lu=lunat,lusecd
          
#if _harvest_mortality
!     Dump remaining hm_store into litter (can't be used next year again)
          do pft=1,npft
            call addc(jpngr,
     $           litter_ag_slow(pft,jpngr,:),
     $           hm_store(pft,jpngr,1),
     $           hm_store(pft,jpngr,2:ncvar)
     $           )
#if _DyN
            call addn(jpngr,
     $           litter_ag_slow(pft,jpngr,:),
     $           hm_store(pft,jpngr,n_id)
     $           )
#endif
            hm_store(pft,jpngr,:) = 0.0d0
          enddo
#endif

        enddo                                               !jpngr
        
!     convert removed mass into harvested area and define additional transitions due to harvest
        do jpngr=minline_task,maxline_task
          aharvest(jpngr,:)=0.0d0
          mharvest(jpngr,:)=0.0d0
        enddo
        
        do jpngr=minline_task,maxline_task
          do lu=1,nlunat+nlusecd                            !only prim and secd

!     xxx debug
            if (lu_area_tr(lu,jpngr).lt.0.0d0) then
              stop 'negative lu_area_tr'
            endif
            if (biomass(lu,jpngr).gt.0.d0) then
              if ((1.d0-available(lu,jpngr)/biomass(lu,jpngr)).lt.0.0d0) then
                stop 'negative DF_harv will result'
              endif
            endif
            
            if (biomass(lu,jpngr).gt.0.d0) then
              aharvest(jpngr,lu)=max(0.0d0,lu_area_tr(lu,jpngr)
     $             *(1.d0-available(lu,jpngr)/biomass(lu,jpngr)))
              mharvest(jpngr,lu)=biomass(lu,jpngr)-available(lu,jpngr)
              DF_harv(lu,lu,jpngr) = aharvest(jpngr,lu)
              if (aharvest(jpngr,lu).gt.lu_area_tr(lu,jpngr)) then
                print*,'harvested area not valid',
     $               aharvest(jpngr,lu),
     $               lu_area_tr(lu,jpngr),
     $               jpngr
                stop
              endif
            else if (harv_m(jpngr,lu).gt.0.d0) then
              aharvest(jpngr,lu)=lu_area_tr(lu,jpngr)
              mharvest(jpngr,lu)=0.d0
              DF_harv(lu,lu,jpngr) = aharvest(jpngr,lu)
            else if (harv_m(jpngr,lu).eq.0.d0) then
              aharvest(jpngr,lu)=0.d0
              mharvest(jpngr,lu)=0.d0
              DF_harv(lu,lu,jpngr) = aharvest(jpngr,lu)
            endif
          enddo
        enddo

!     harvest_byarea
#endif

      endif                                                 !calclutr

!     wood_harvest
#endif

!     xxx debug
c$$$      print*,'DF_tr  ',DF_tr(:,:,5)
c$$$      print*,'DF_corr',DF_corr(:,:,5)
c$$$      print*,'DF_harv',DF_harv(:,:,5)
c$$$      print*,'lu_area    ', lu_area(:,5)
c$$$      print*,'lu_area_old', lu_area_old(:,5)
      
!     -------------------------------------------------------------------------
!     The final corrected transition matrix DF (global variable) is
!     the sum of the matrix read from input, the correction, and
!     the harvest matrix
!     -------------------------------------------------------------------------
      do jpngr=minline_task,maxline_task
        DF(:,:,jpngr) = DF_tr(:,:,jpngr) + DF_corr(:,:,jpngr) + DF_harv(:,:,jpngr)
      enddo
!     -------------------------------------------------------------------------

!     Check, if negative areas will result...
      do jpngr=minline_task,maxline_task
        lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
      enddo
      
      do jpngr=minline_task,maxline_task
        error=.false.
        do from=1,nlutr
          if (sum(DF(from,:,jpngr))-lu_area_old(from,jpngr).gt.min_precision) then
            print*,'mismatch:',sum(DF(from,:,jpngr))-lu_area_old(from,jpngr)
            error=.true.
          endif
          if (error) then
            print*,'transition not allowed: not enough land in from cat'
 666        print*,'jpngr,from',jpngr,from
            print*,'lu_area_old',lu_area_old(:,jpngr)
            print*,'DF'
            do llu=1,nlu
              print*,DF(llu,:,jpngr)
            enddo
            print*,'lu_area',lu_area(:,jpngr)
            stop
          endif
        enddo
      enddo
      
!     lu_area_old + DF_tr + DF_corr + DF_harv should yield lu_area
      do jpngr=minline_task,maxline_task
        lu_area_tr(:,jpngr) = lu_area_old(:,jpngr)
        do from=1,nlu
          do to=1,nlu

            if ((DF_tr(from,to,jpngr)+DF_corr(from,to,jpngr)+DF_harv(from,to,jpngr)).lt.0.0d0) then
              print*,'from,to,jpngr,DF_tr,DF_corr,DF_harv',from,to,jpngr,
     $             DF_tr(from,to,jpngr),DF_corr(from,to,jpngr),DF_harv(from,to,jpngr)
            endif

            lu_area_tr(to,jpngr) = lu_area_tr(to,jpngr)
     $           + DF_tr(from,to,jpngr) + DF_corr(from,to,jpngr) + DF_harv(from,to,jpngr)
            lu_area_tr(from,jpngr) = lu_area_tr(from,jpngr)
     $           - (DF_tr(from,to,jpngr) + DF_corr(from,to,jpngr) + DF_harv(from,to,jpngr) )
          enddo
        enddo
#if _LU_generated_transitions
        do lu=lucrop,lubuilt
#else
        do lu=1,nlu
#endif
          if (abs(lu_area_tr(lu,jpngr)-lu_area(lu,jpngr)).gt.min_precision) then
            print*,'DF and lu_area are still not consistent. C'
            print*,'ilon,ilat,jpngr',ilon(jpngr),ilat(jpngr),jpngr
            print*,'lu_area_old',lu_area_old(:,jpngr)
            print*,'DF_tr'
            do llu=1,nlu
              print*,DF_tr(llu,:,jpngr)
            enddo
            print*,'DF_corr'
            do llu=1,nlu
              print*,DF_corr(llu,:,jpngr)
            enddo
            print*,'DF_harv'
            do llu=1,nlu
              print*,DF_harv(llu,:,jpngr)
            enddo
            print*,'lu_area_tr',lu_area_tr(:,jpngr)
            print*,'lu_area',lu_area(:,jpngr)
          endif
        enddo
      enddo

#if _LU_generated_transitions
!     should not make a difference, also when using with LU_transitions. Un-
!     fortunately it does: area can become negative within precision limits.
!     -------------------------------------------------------------------------
!     lu_area_tr now meets targets (lu_area) for crop, past, and builtup
!     while its primary and secondary consistently follow from land turnover
!     and additional transitions necessary to meet target.
!     Use 'lu_area_tr' to define 'lu_area' to be perfectly consistent with
!     transitions (there might still be a minimum numerical difference between
!     read-in 'lu_area' and derived 'lu_area_tr' (='lu_area_old'+'DF').
!     -------------------------------------------------------------------------
      do jpngr=minline_task,maxline_task
        lu_area(:,jpngr) = lu_area_tr(:,jpngr)
      enddo
!     -------------------------------------------------------------------------
#endif
      
#else                                                       ! _LU_transitions == 0
!     ################################################################
!     ONLY NET LAND USE CHANGE (NO GROSS TRANSITIONS)
!     _LU_transitions==0 && _LU_generated_transitions==0
!     ----------------------------------------------------------------
!     initialise
      lu_area(:,:)=0.d0         !man
      lu_area(lunat,:)=land_fraction(:) !nature
      arealeft(:)=land_fraction(:) !nature
      past(:)=0.d0
      crop(:)=0.d0
      built(:)=0.d0

#if _peatlands_on
!     ///////////////////////////////////////////////////////////////////////////
!     DEFINE GROWTH AND INITIATION OF PEATLAND AREA (without topmodel)
!     ///////////////////////////////////////////////////////////////////////////

c#if _DOsim || _hi_res
      growth_init = 1.d0
c#else
c      growth_init=1.0d0/(1.0d0+exp(-1.0d0*(
c     $     s_peat+realyear-1950.0d0)/1000.0d0))
c#endif
      
c     spahni 2011-08-8: Implementation of peatland area growth with time
      do jpngr=minline_task,maxline_task

        if ((init.and.notrestart).or.newcell(jpngr)) then        
#if _topmodel
          peat_initiation(jpngr)=9999.0d0 ! year AD
#else
          if (lat(jpngr).ge.30.0d0) then
            if (lon(jpngr).ge.-10.0d0) then
              peat_initiation(jpngr)=max(realyear,-1.0d0*peat_init_EA+1950.0d0)
            else
              peat_initiation(jpngr)=max(realyear,-1.0d0*peat_init_NA+1950.0d0)
            endif
          else
            peat_initiation(jpngr)=realyear
          endif
#endif
        endif
        
!     ///////////////////////////////////////////////////////////////////////////
!     CALCULATE PEATLAND AREA
!     ///////////////////////////////////////////////////////////////////////////
#if _topmodel
!     ///////////////////////////////////////////////////////////////////////////
!     Dynamical peatland based on TOPMODEL
!     ----------------------------------------------------------------------------
!     Multi-step spinup:
!     1. (years 1 through recycle year) : all cells are set to minimum
!        peatland fraction (min_peat_fraction)
!     2. In recycle year (31 yr) soil carbon is partially filled: 25% of thaw 
!        layer C for mineral soils and 100% of acrotelm C in peatlands
!     3. In soil_equil_year: analytical soil equilibration. This causes
!        acro-to-cato transfer rate to make jumps because analytical equil.
!        may cause acrothelm C storage to exceed its prescribed maximum.
!        All cells area still set to min_peat_fraction
!     4. In soil_equil_year+10:  acro-to-cato transfer is now (more or
!        less) equilibrated. Balance (corresponds to this acro-to-cato-transfer)
!        is diagnosed and peatlandfrac is dynamically set. This corresponds
!        to an immediate scaling of the peatland area without dilution of C
!        stocks, thus C balance is not conserved. Additionally, peatland area is
!        now changing every year and C pools are exchanging between land units.
!        In order to prevent a dilution peatland C into mineral soils, an 
!        additional lu area is used: lupeatold (old peatland). This area upholds
!        the large C pools, which can be reclaimed by lupeat if peatland area 
!        should increase again.
!     5. In soil_equil_year_two : equilibrium is again calculated analytically for
!        lunat only. This will make sure mineral soil pools are in equilibrium also
!        including the rather constant thaw layer to permafrost C flux.
!
!     During spinup, conversion SR is not executed before soil_equil_year+10.
!     ----------------------------------------------------------------------------
        if (spinup.and.year.lt.soil_equil_year+conversion_wait) then
          lu_area(lupeat,jpngr)=min_peat_fraction
        else
c     Set peatland area immediately to simulated inundation fraction
          if (spinup.and.year.eq.soil_equil_year+conversion_wait) then
            lu_area(lupeat,jpngr)=peatlandfrac(jpngr)
            lu_area_old(lupeat,jpngr)=peatlandfrac(jpngr)

          endif
!     ----------------------------------------------------------------------------
!     Transient peatland area change:
!     peatlandfrac is set dynamically (see fflooded.F) based on inundated areas
!     over the previous 20 yr, and is updated annually. Inundated areas are
!     independent of peatland area. lu_area varies in response to changes in
!     peatlandfrac, but rate of areal change is limited to max_peat_areachangerate.
!     ----------------------------------------------------------------------------

!     calculate area rate of change for expansion
          scdiffp = (cpool_fast(jpngr,lupeat,1)+cpool_slow(jpngr,lupeat,1)
     $         -cpool_fast(jpngr,lunat,1)-cpool_slow(jpngr,lunat,1))
          scdiffpo = (cpool_fast(jpngr,lupeatold,1)+cpool_slow(jpngr,lupeatold,1)
     $         -cpool_fast(jpngr,lunat,1)-cpool_slow(jpngr,lunat,1))
          if (scdiffp.gt.0.0d0.and.scdiffpo.gt.0.0d0) then
            areachangerate = min_peat_areachangerate + (max_peat_areachangerate
     $           -min_peat_areachangerate)/scdiffp*scdiffpo
          else
            areachangerate = min_peat_areachangerate
          endif
          
          if (lu_area_old(lupeat,jpngr).gt.peatlandfrac(jpngr)) then
!     shrinking peatland
            lu_area(lupeat,jpngr) = max(lu_area_old(lupeat,jpngr)
     $           *(1.0d0-max_peat_areachangerate)
     $           ,peatlandfrac(jpngr))
          else if (lu_area_old(lupeat,jpngr).lt.peatlandfrac(jpngr)) then
!     expanding peatland
            lu_area(lupeat,jpngr) = min(lu_area_old(lupeat,jpngr)
     $           *(1.0d0+areachangerate)
     $           ,peatlandfrac(jpngr))
!     check if peat is already initiated and set date
            if (peat_initiation(jpngr).eq.9999.0d0
     $           .or.lu_area_old(lupeat,jpngr).le.init_areaf*min_peat_fraction) then
              peat_initiation(jpngr) = realyear ! date in years AD
            endif
          else 
            lu_area(lupeat,jpngr) = peatlandfrac(jpngr)
          endif
        endif
#else

        lu_area(lupeat,jpngr)=peatlandfrac(jpngr)
        
        if (ptgridcell(jpngr).and.
     $       (realyear.ge.peat_initiation(jpngr))) then
          lu_area(lupeat,jpngr)=peatlandfrac(jpngr)*growth_init 
        else
          lu_area(lupeat,jpngr)=min_peat_fraction
        endif
#endif

!     set absoulte grid cell boundaries
        lu_area(lupeat,jpngr)=min(lu_area(lupeat,jpngr),land_fraction(jpngr))
        lu_area(lupeat,jpngr)=max(lu_area(lupeat,jpngr),min_peat_fraction)
        
#if _topmodel
!     Change in peatland area 
        dlu = lu_area(lupeat,jpngr)-lu_area_old(lupeat,jpngr)
!     Expanding peatland -> reclaim former peatland
        if (dlu.gt.0.0d0) then
          lu_area(lupeatold,jpngr)=
     $         max(lu_area_old(lupeatold,jpngr)-dlu,0.0d0)
!     Contracting peatland -> reallocate to peatold
        else if (dlu.lt.0.0d0) then
          lu_area(lupeatold,jpngr)=
     $         min(lu_area_old(lupeatold,jpngr)-dlu,
     $         lu_area(lunat,jpngr)-lu_area(lupeat,jpngr))
        else
          lu_area(lupeatold,jpngr)=lu_area_old(lupeatold,jpngr)
        endif
!     Transform former peatland to mineral soils a a rate of 0.1 per mill per year.
c        lu_area(lupeatold,jpngr)=0.9999d0*lu_area(lupeatold,jpngr)
!     Update natural land
        lu_area(lunat,jpngr)=max(lu_area(lunat,jpngr)
     $       -lu_area(lupeat,jpngr)-lu_area(lupeatold,jpngr),0.0d0) !natural
#else
        lu_area(lunat,jpngr)=max(lu_area(lunat,jpngr)
     $       -lu_area(lupeat,jpngr),0.0d0) !natural
#endif
      enddo
#endif   !  _peatlands_on   
      
    
#if _LU  && (_LU_transitions == 0)   
      ! No landuse flag set -> Take filename from parameter file
      if (.not.rcp.or.(rcp.and.int(realyear).le.2005)) then
        lu_map_path = '../input_data/anthropogenic2d/landuse/'//trim(lu_map_filename)
      else
        lu_map_path = '../input_data/anthropogenic2d/landuse/'//trim(luscen_map_filename)
        if (realyear.eq.2006.d0) start=.true.
      endif
      
      if(start.or.rcp.and.realyear.eq.2005.0d0)then
c     open cdf file

#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
#endif
!          print*,'landuse file: ',trim(lu_map_path)
#if _parallel_mpi
        endif
#endif
         error=NF_OPEN(trim(lu_map_path),NF_NOWRITE,lufile)

         if (error.ne.0) goto 999
         

#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
#endif         
          print *,"Read dynamic landuse map from: "//lu_map_path
#if _parallel_mpi
        endif
#endif
        varid(:) = -1

        ! Time
        error=NF_INQ_VARID(lufile,trim(lu_map_timevar),varid(0))
        if (error.ne.0) goto 999

        ! Cropland
        error=NF_INQ_VARID(lufile,trim(lu_map_cropvar),varid(1))
        if (error.ne.0) goto 999
         
#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
! Only master does output
#endif
           print*,'read in variable for cropland ',trim(lu_map_cropvar)
#if _parallel_mpi
        endif
#endif

        ! Pasture (skip if varname is empty)
        if (trim(lu_map_pastvar).ne.'') then
          error=NF_INQ_VARID(lufile,trim(lu_map_pastvar),varid(2))
          if (error.ne.0) goto 999
        else
#if _parallel_mpi
          if (mpi_rank_lpj.eq.0) then
#endif
            print *," - Pasture landuse is not included."
#if _parallel_mpi
          endif
#endif
        endif
         
#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
! Only master does output
#endif
           print*,'read in variable for pasture ',trim(lu_map_pastvar)
#if _parallel_mpi
        endif
#endif

        ! Builtup (skip if varname is empty)
        if (trim(lu_map_builtvar).ne.'') then
          error=NF_INQ_VARID(lufile,trim(lu_map_builtvar),varid(3))
          if (error.ne.0) goto 999
        else
#if _parallel_mpi
          if (mpi_rank_lpj.eq.0) then
#endif
            print *," - Builtup landuse is not included."
#if _parallel_mpi
          endif
#endif
        endif
         
#if _parallel_mpi
        if (mpi_rank_lpj.eq.0) then
!     Only master does output
#endif
           print*,'read in variable for builtup ',trim(lu_map_builtvar)
#if _parallel_mpi
        endif
#endif

c     get time domain
c     debug: -------------------------------------------------------
c     error=NF_OPEN('output_netcdf/sp1000_test.cdf',NF_NOWRITE,lufile) !
c     error=NF_INQ_VARID(lufile,'TIME',varid(0)) !
c     if (error.ne.0) goto 999  !
c     --------------------------------------------------------------
        error = NF_INQ_DIMLEN(lufile,varid(0),time_length)
        if (error.ne.0) goto 999
        allocate(time(time_length))
        error = NF_GET_VAR_INT(lufile,varid(0),time)
        if (error.ne.0) goto 999
        
        start=.false.
      else
c     reopen cdf file
        error=NF_OPEN(trim(lu_map_path),NF_NOWRITE,lufile)
        if (error.ne.0) goto 999
      endif
      
      
c     get bracket
      ibracket(:)=0
      do itime=1,time_length
        if(time(itime).le.realyear)then
          ibracket(1)=itime
        else
          ibracket(2)=itime
          exit
        endif
      enddo
      if(ibracket(1).eq.0.or.ibracket(2).eq.0)then
        imax=1
        if(ibracket(1).eq.ibracket(2))then !both are zero
          stop 'failed to get landuse map for this year'
        elseif(ibracket(1).ne.0)then
#if _parallel_mpi
          if (mpi_rank_lpj.eq.0) then
#endif
            if (time_length.gt.1) then

            ! Only print the warning if there are multiple timesteps in the forcing file,
            ! otherwise it is assumed that the constant forcing is intended.
            ! (this gives a spurious warning when the year matches the last year in the file)
#if _parallel_mpi
            if (mpi_rank_lpj.eq.0) then
! Only master does output
#endif
              write(0,*)'----------------------------------------------'
              write(0,*)'warning: landuse map extended forward in time'
              write(0,*)'----------------------------------------------'
#if _parallel_mpi
            end if
#endif
           endif
#if _parallel_mpi
          endif 
#endif
          ibracket(2)=ibracket(1)
        elseif(ibracket(2).ne.0)then
#if _parallel_mpi
          if (mpi_rank_lpj.eq.0) then
#endif
           if (time_length.gt.1) then
            ! Only print the warning if there are multiple timesteps in the forcing file,
            ! otherwise it is assumed that the constant forcing is intended.
#if _parallel_mpi
            if (mpi_rank_lpj.eq.0) then
! Only master does output
#endif
              write(0,*)'--------------------------------------------'
              write(0,*)'warning landuse map extended backwards in time'
              write(0,*)'--------------------------------------------'
#if _parallel_mpi
            end if
#endif
           endif
#if _parallel_mpi
          endif 
#endif
          ibracket(1)=ibracket(2)
        endif
      else
        imax=2
      endif
c     print*,realyear
c     print*,time(ibracket(:))

c     read data and interpolate...
c     for the time being, interpolation is done linearly. this may
c     not be the best choice. possibly better to use stepfunction or even
c     exponential interpolation (linear on log-transformed data).

      if (time(ibracket(1)).eq.realyear) then 
        ibracket(2)=ibracket(1)
        imax=1
      endif
      if (time(ibracket(2)).eq.realyear) then
        ibracket(1)=ibracket(2)
        imax=1
      endif

c/////////////////////////////////////////////////////////////////////////////
      do jpngr=minline_task,maxline_task
        cdf_index(1)=int(lon(jpngr)/dlon-(minlon-dlon)/dlon)
        cdf_index(2)=int(lat(jpngr)/dlat-(minlat-dlat)/dlat)
        do i=1,imax
          cdf_index(3)=ibracket(i)
          error = NF_GET_VAR1_DOUBLE
     $         (lufile,varid(1),cdf_index,cropland(i))
          if (error.ne.0) goto 999
          if (varid(2).ge.0) then
            error = NF_GET_VAR1_DOUBLE
     $           (lufile,varid(2),cdf_index,pasture(i))
          else 
            pasture(i)=0.d0
          endif
          if (varid(3).ge.0) then
            error = NF_GET_VAR1_DOUBLE
     $           (lufile,varid(3),cdf_index,builtup(i))
          else 
            builtup(i)=0.d0
          endif
          
        enddo                   !do i=1,imax
        
        if (imax.eq.2) then
c     interpolate and save the result in first slice
          cropland(1)= cropland(1)
     $         +(cropland(2)-cropland(1))
     $         /(time(ibracket(2))-time(ibracket(1)))
     $         *(realyear-time(ibracket(1)))
          pasture(1)= pasture(1)
     $         +(pasture(2)-pasture(1))
     $         /(time(ibracket(2))-time(ibracket(1)))
     $         *(realyear-time(ibracket(1)))
          builtup(1)= builtup(1)
     $         +(builtup(2)-builtup(1))
     $         /(time(ibracket(2))-time(ibracket(1)))
     $         *(realyear-time(ibracket(1)))
        endif
        
        if(lupasture.ne.0)then
          past(jpngr)=pasture(1)
        else
          past(jpngr)=0.d0
        endif
        if(lucrop.ne.0)then
          crop(jpngr)=cropland(1)
        else
          crop(jpngr)=0.d0
        endif
        built(jpngr)=builtup(1)

      enddo            !jpngr
c//////////////////////////////////////////////////////////////////////
      
      error=NF_CLOSE(lufile)
      
      
c     how do we assure that several landuse categories together do not exceed 1?
c     a little error would be acceptable, so maybe we just run a check and stop if it's too much
      do jpngr=minline_task,maxline_task
        sumlu = crop(jpngr)+past(jpngr)+built(jpngr)
        if (sumlu.gt.1.d0) then
          ! Rescale land use sum to 1.0 and print a warning if the
          ! correction is significant
          crop(jpngr) = crop(jpngr)/sumlu
          past(jpngr) = past(jpngr)/sumlu
          built(jpngr) = built(jpngr)/sumlu
          if (sumlu-1.d0.gt.1.d-4) then
            write(0,*) '*******************************'
            write(0,*) 'WARNING: sum of landuse areas significantly '/
     $           /'greater than 1. in cell ',jpngr
            write(0,*) 'crop(jpngr)+past(jpngr)+built(jpngr)',
     $           sumlu
            write(0,*) '=> rescaling to 1.0'
            write(0,*) '*******************************'
          endif
        endif
      enddo

c     assign all the landuse categories
c     (all non-natural areas are still zero)

      if (lucrop.ne.0)
     $     lu_area(lucrop,:)=min(max(crop(:),0.d0),land_fraction(:))
      if (lupasture.ne.0)
     $     lu_area(lupasture,:)=min(max(past(:),0.d0),land_fraction(:))
      if (lubuilt.ne.0)
     $     lu_area(lubuilt,:)=min(max(built(:),0.d0),land_fraction(:))
      

c     This is quite limited, it should be
c     possible e.g. to assign cropland to different categories in different
c     areas (e.g. explicitly c3 or c4) or even to split it.     
c     Anyhow, the input map always has one figure each for pasture and cropland,
c     so the conversion routine can use these.

c     kuno 2005-07-12: it would be better, if this assignment wasn't
c     hardwired: should be somehow inherited from the definitions of the
c     different lu-categories in landuse.inc

c     Roth: Landuse-areas can exeed 1, since already non-natural landuse
c     reach 1 for certrain cells. Priority is set as follows:
c     a) non-natural as given by e.g. hyde3.x data
c     b) if "space" left, assign peatland
c     c) rest (if any) assign to natural category


      if (lucrop.ne.0) arealeft(:)=arealeft(:)-lu_area(lucrop,:)
      if (lupasture.ne.0) arealeft(:)=arealeft(:)-lu_area(lupasture,:)
      if (lubuilt.ne.0) arealeft(:)=arealeft(:)-lu_area(lubuilt,:)

      ! Make sure arealeft is >= 0.0
      arealeft(:) = max(0.d0,arealeft(:))

      if (lupeat.eq.0) then      ! no peatlands
        lu_area(lunat,:)=arealeft(:)
      else                      ! peatlands: 
        lu_area(lupeat,:)=min(arealeft(:),lu_area(lupeat,:)) ! lupeat cannot be > space left
        do jpngr=minline_task,maxline_task
#if _topmodel
          lu_area(lupeatold,jpngr)=min(lu_area(lupeatold,jpngr),
     $         arealeft(jpngr)-lu_area(lupeat,jpngr)) ! lupeat cannot be > space left 
          lu_area(lunat,jpngr)=max(arealeft(jpngr)-lu_area(lupeat,jpngr)
     $         -lu_area(lupeatold,jpngr),0.0d0) ! lunat gets rest
#else
          lu_area(lunat,jpngr)=max(arealeft(jpngr)-lu_area(lupeat,jpngr),0.0d0) ! lunat gets rest
#endif
        enddo
      endif
#endif                          ! LU

!     ################################################################
!     this is executed irrespective of _LU flags

#if _fixedlandmask==0
c     spahni 2011-02-7: added total grid cell are scaling due to ice/sea cover
      do jpngr=minline_task,maxline_task
        lascale(jpngr)=max(lascale(jpngr)+dla(jpngr),0.0d0) ! prevent numerical problem: negative values
c#if _parallel_mpi
c        if (mpi_rank_lpj.eq.0) then
c#endif
c          if (lascale(jpngr).ne.1.0d0) then
c            print*,'land mask changes now',jpngr,lascale(jpngr)
c          endif
c#if _parallel_mpi
c        endif
c#endif
        sumlu = 0.0d0
        do lu=1,nlu
          if (lu.ne.luburied) then
            lu_area(lu,jpngr) = lascale(jpngr)*lu_area(lu,jpngr)
            sumlu = sumlu+lu_area(lu,jpngr)
          end if
        end do
        lu_area(luburied,jpngr) = max(1.0d0-sumlu,0.0d0)
c     spahni 2013-01-17: rounding error correction for areas smaller
c     than 10m² ~ 10^-10 fraction on 2.5x3.75 deg grids
        do lu=1,nlu
          if (lu_area(lu,jpngr).lt.1.0d-10) lu_area(lu,jpngr)=0.0d0
        enddo
        lu_area(lunat,jpngr) = max(lu_area(lunat,jpngr),0.0d0) ! cannot be < 0
#if _peatlands_on
        lu_area(lupeat,jpngr) = max(lu_area(lupeat,jpngr),0.0d0) ! cannot be < 0
#endif
      end do
#endif                          ! _fixedlandmask==0

      do jpngr=minline_task,maxline_task
        do lu=1,nlu
          if(lu_area(lu,jpngr).lt.0.0d0)then
            print*, 'lu-area negative', lu, jpngr, lu_area(lu,jpngr)
            stop 'B: error in lu-area assignment, area negative'
          endif
        enddo
      enddo

#endif

      do jpngr=minline_task,maxline_task
        do lu=1,nlu
          if(lu_area(lu,jpngr).lt.0.d0)then
            print*,'lu,jpngr',lu,jpngr
            print*,'lu_area:',lu_area(lu,jpngr)
            print*,'aborting supressed' ! xxx
            if (abs(lu_area(lu,jpngr)).gt.min_precision) then
              stop 'C: error in lu-area assignment, area negative'
            else
              ! set to zero for negligible numbers
              lu_area(lu,jpngr) = 0.0d0
            endif
          endif
        end do
      enddo
      return
      
 999  write(0,*) 'error trying to read in landuse map:',
     $     NF_STRERROR(error)
      stop

 998  write(0,*) 'negative area in landuse change: jpngr, from, to',
     $     jpngr, from, to
      stop
      
 100  write(0,*)  'cannot read dimensions of input file'
      stop 
      
 300  write(0,*)  'dimensions of input files differ'
      stop    
      
 400  write(0,*)  'error trying to read in input file:',
     $     trim(message),
     $     NF_STRERROR(error)
      stop 
      
 500  write(0,*)  'error opening'
      write(0,*)  trim(message)
      stop
      
      end

#if _methane_mod
#if _LU
!//////////////////////////////////////////////////////////////////////////////
!     SUBROUTINE READ FRICE MAP     
!     Map for precribing frice per grid cell (fraction of cropland used for
!     rice cultivation with seasonal man-made flooding).

      subroutine read_frice_map

      IMPLICIT NONE
#include "para.inc"
#include "modelpara.inc"
      include 'coordinates.inc'
      include 'landuse.inc'
      include 'iodata.inc'
      include 'netcdf.inc'

!     ARGUMENTS
      REAL*8 realyear
      
!     LOCAL PARAMETERS
      INTEGER start(3),count(3), error
      DATA start /1, 1, 1/
      DATA count /nlon, nlat, 1/

!     LOCAL VARIABLES
      INTEGER jpngr
      REAL*8 datarice(nlon,nlat,1) ! Temporally constant rice fraction

!     //////////////////////////////////////////////////////////////////////
!     Read rice fraction data from NetCDF file
!     ----------------------------------------------------------------------
      error = NF_GET_VARA_DOUBLE(inffil,ricefvarid,start,count,datarice) 
      if (error.ne.0) goto 100

!     loop over land cells only and assign values to global variable 'frice'
!     ----------------------------------------------------------------------
      do jpngr=minline_task,maxline_task
        rice(jpngr) = max( 0.0d0, datarice(ilon(jpngr),ilat(jpngr),1) )
      enddo
            
      return
      
 100  write(0,*) 'in read_frice_map: ' // NF_STRERROR(error)
      write(0,*) 'realyear',realyear
      stop
      
      end subroutine read_frice_map
      
#endif
      
#if _topmodel==0
c//////////////////////////////////////////////////////////////////////////////
c
c     SUBROUTINE READ INUNDATION MAP
c     
c     Map for precribing inundation per grid cell for methane
c 
      subroutine read_inundation_map(realyear)

      IMPLICIT NONE
#include "para.inc"
#include "modelpara.inc"
      include 'coordinates.inc'
      include 'landuse.inc'
      include 'iodata.inc'
      include 'netcdf.inc'

c     ARGUMENTS
      REAL*8 realyear
      
c     LOCAL PARAMETERS
      LOGICAL monthly_average   ! Calculate monthly average inundation from input map
      PARAMETER (monthly_average=.true.)
      INTEGER nmonthall
      PARAMETER (nmonthall=12*nmonth)
      INTEGER start(3),count(3),countall(3)
      INTEGER m,y
      DATA start /1, 1, 1/
      DATA count /nlon, nlat, nmonth/
      DATA countall /nlon, nlat, nmonthall/

c     LOCAL VARIABLES
      INTEGER jpngr,inundcycleyear,error
      REAL*8 datainund(nlon,nlat,nmonth) ! 1 year of data between 1993-2004 Prigent-Papa
      REAL*8 inundall(nlon,nlat,nmonth*12) ! 12 years of data 1993-2004 Prigent-Papa

c     When using TOPMODEL:
c     Inundated fraction is calculated as a function of water table position
c     in SR fflooded.F. This subroutine is called after gpp SR and
c     waterbalance SR because it needs water table position first. 

c     //////////////////////////////////////////////////////////////////////
c     Use average over entire data period (1993-2004) for each month
c     ----------------------------------------------------------------------
      if (monthly_average) then
       
        error = NF_GET_VARA_DOUBLE(inffil,inundvarid,start,countall,inundall) 
        if (error.ne.0) goto 100
        
c     loop over land cells only and assign values
c     ----------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          do m=1,nmonth
            inund(jpngr,m) = 0.0d0
            do y=1,12           ! 12 years of data
              inund(jpngr,m)=inund(jpngr,m)+inundall(ilon(jpngr),
     $             ilat(jpngr),m+nmonth*(y-1))/12.0d0
            enddo
          enddo
        enddo
        
c     //////////////////////////////////////////////////////////////////////
c     Use actual data for each month of data period (1993-2004)
c     ----------------------------------------------------------------------
      else
     
        inundcycleyear = modulo(int(realyear)-1993,12)+1 ! modulo, not mod
        start(3) = (inundcycleyear-1)*nmonth+1

        error = NF_GET_VARA_DOUBLE(inffil,inundvarid,start,count,datainund) 
        if (error.ne.0) goto 100
        
c     loop over land cells only and assign values
c     ----------------------------------------------------------------------
        do jpngr=minline_task,maxline_task
          do m=1,nmonth
!     XXX combine with landuse map: if inund and lucrop then rice else inund

c     Natural inundated peatlands
            inund(jpngr,m)=datainund(ilon(jpngr),ilat(jpngr),m)
            
          enddo
        enddo
        
      endif
          
      return
      
 100  write(0,*) 'in read_inundation_map: ' // NF_STRERROR(error)
      write(0,*) 'realyear',realyear
      stop
      
      end subroutine read_inundation_map

#endif
#endif


#if _DyN && _ndep
!//////////////////////////////////////////////////////////////////////////////
!
!     SUBROUTINE GETNDEP
!     Read map for reactive N-deposition (NHx and NOy) for this year.
!     Global (common) arrays 'noydep' and 'noxdep' are provided in 'soil.inc'

      subroutine getndep(realyear)

      IMPLICIT NONE
#include "para.inc"
#include "modelpara.inc"
#include "soil.inc"
      include 'coordinates.inc'
      include 'iodata.inc'
      include 'netcdf.inc'

!     ARGUMENTS
      REAL*8 realyear
      
!     LOCAL PARAMETERS
      INTEGER index(3)

!     LOCAL VARIABLES
      INTEGER error,jpngr

!     Set index corresponding to the year to be read in. For all years beyond
!     2099 AD use data of 2099 AD. For all years before 1850, use data of 1850 AD.
!     -------------------------------------------------------------------------
      if (.not.rcp.or.(rcp.and.int(realyear).le.2005)) then
!     Reading historical file
        index(3) = min(max(int(realyear-1849.d0),1),160)
      else
!     Reading RCP file
        index(3) = min(int(realyear-1999.d0),100)
      endif

!     Loop over spatial domain and get single values from NetCDF file
!     -------------------------------------------------------------------------        
      do jpngr=minline_task,maxline_task

        index(1)=ilon(jpngr)
        index(2)=ilat(jpngr)

        if (.not.rcp.or.(rcp.and.int(realyear).le.2005)) then
          error =
     $         NF_GET_VAR1_DOUBLE(ndepfil,ndepvarid(1),index,noydep(jpngr)) 
          if (error.ne.0) goto 100
          error =
     $         NF_GET_VAR1_DOUBLE(ndepfil,ndepvarid(2),index,nhxdep(jpngr)) 
          if (error.ne.0) goto 100
        else
          error =
     $         NF_GET_VAR1_DOUBLE(ndepscenfil,ndepscenvarid(1),index,
     $         noydep(jpngr)) 
          if (error.ne.0) goto 100
          error =
     $         NF_GET_VAR1_DOUBLE(ndepscenfil,ndepscenvarid(2),index,
     $         nhxdep(jpngr)) 
          if (error.ne.0) goto 100
        endif
        
      enddo
      
      return
      
 100  write(0,*) 'in getndep: ' // NF_STRERROR(error)
      write(0,*) 'realyear',realyear
      write(0,*) 'index(:)',index(:)
      stop
      
      end subroutine getndep
#endif

#if _DyN && _nfert
!//////////////////////////////////////////////////////////////////////////////
!
!     SUBROUTINE GETNFERT
!     Read map for N-fertilisation for this year.
!     Global (common) array 'nfertilisation' is provided in 'soil.inc'

      subroutine getnfert(realyear)

      IMPLICIT NONE
#include "para.inc"
#include "modelpara.inc"
#include "soil.inc"
      include 'coordinates.inc'
      include 'iodata.inc'
      include 'netcdf.inc'

!     ARGUMENTS
      REAL*8 realyear
      
!     LOCAL PARAMETERS
      INTEGER index(3),index2(3)

!     LOCAL VARIABLES
      INTEGER error,jpngr,k
      
!     Set index corresponding to the year to be read in the direct
!     climate input files (CRU extends from 1901 to 2009; CMIP5 RCP from 2006 to 2100).
!     For all years beyond 2100 AD use data of 2100 AD. For all years before
!     1850, use data of 1850 AD.
!     -------------------------------------------------------------------------
c$$$!     xxx provisional: hold nfert field constant at 2005 AD level
c$$$      index(3) = min(max(int(realyear-1699.d0),1),306)
      
      if (.not.rcp.or.(rcp.and.int(realyear).le.2005)) then
!     Reading historical file
c        index(3) = min(max(int(realyear-1699.d0),1),306)
        index(3) = min(max(int(realyear-1700.d0),1),306)
      else
!     Reading RCP file
        index(3) = min(int(realyear-2004.d0),96)
      endif
      
!     Loop over spatial domain and get single values from NetCDF file
!     -------------------------------------------------------------------------        
      do jpngr=minline_task,maxline_task

        index(1)=ilon(jpngr)
        index(2)=ilat(jpngr)
        index2(1:2)=index(1:2)
        index2(3)=1
        
        do k=2,5
          error =
     $         NF_GET_VAR1_DOUBLE(napplfil,nfertvarid(k),index2,nappl(k-1,jpngr)) 
          if (error.ne.0) goto 100
        enddo
        
        if (.not.rcp.or.(rcp.and.int(realyear).le.2005)) then
          error =
     $         NF_GET_VAR1_DOUBLE(nfertfil,nfertvarid(1),index,nfert(jpngr)) 
          if (error.ne.0) goto 100
        else
          error =
     $         NF_GET_VAR1_DOUBLE(nfertscenfil,nfertscenvarid,index,nfert(jpngr)) 
          if (error.ne.0) goto 100
        endif

      enddo
      
      return
      
 100  write(0,*) 'in getnfert: ' // NF_STRERROR(error)
      write(0,*) 'realyear',realyear
      write(0,*) 'index(:)',index(:)
      stop
      
      end subroutine getnfert
#endif

!//////////////////////////////////////////////////////////////////////////////
!
!     SUBROUTINE GETBRACKET
      subroutine getbracket(realyear,time,time_length,ibracket,imax)

      IMPLICIT NONE
#if _parallel_mpi
      include 'parallel_mpi.inc'
#endif

!     ARGUMENTS
      REAL*8 realyear
      INTEGER ibracket(2),imax,time_length,
     $     time(time_length)

#if _LU_transitions
!     LOCAL VARIABLES
      INTRINSIC size
      INTEGER itime

      do itime=1,time_length
        if(time(itime).le.realyear)then
          ibracket(1)=itime
        else
          ibracket(2)=itime
          exit
        endif
      enddo
      
      if(ibracket(1).eq.0.or.ibracket(2).eq.0)then
        imax=1
        if(ibracket(1).eq.ibracket(2))then                  !both are zero
          stop 'failed to get landuse map for this year'
        elseif(ibracket(1).ne.0)then
#if _parallel_mpi
          if (mpi_rank_lpj.eq.0) then
#endif
            if (time_length.gt.1) then

!     Only print the warning if there are multiple timesteps in the forcing file,
!     otherwise it is assumed that the constant forcing is intended.
#if _parallel_mpi
!     Only master does output
              if (mpi_rank_lpj.eq.0) then
#endif
                write(0,*)'------------------------------------'
                write(0,*)'landuse map extended forward in time'
                write(0,*)'------------------------------------'
#if _parallel_mpi
              end if
#endif
            endif
#if _parallel_mpi
          endif 
#endif
          ibracket(2)=ibracket(1)
        elseif(ibracket(2).ne.0)then
#if _parallel_mpi
          if (mpi_rank_lpj.eq.0) then
#endif
            if (time_length.gt.1) then
!     Only print the warning if there are multiple timesteps in the forcing file,
!     otherwise it is assumed that the constant forcing is intended.
#if _parallel_mpi
              if (mpi_rank_lpj.eq.0) then
#endif
                write(0,*)'--------------------------------------'
                write(0,*)'landuse map extended backwards in time'
                write(0,*)'--------------------------------------'
#if _parallel_mpi
              end if
#endif
            endif
#if _parallel_mpi
          endif 
#endif
          ibracket(1)=ibracket(2)
        endif
      else
        imax=2
      endif
      if (time(ibracket(1)).eq.realyear) then 
        ibracket(2)=ibracket(1)
        imax=1
      endif
      if (time(ibracket(2)).eq.realyear) then
        ibracket(1)=ibracket(2)
        imax=1
      endif

#endif
      
      return
      end subroutine getbracket


!//////////////////////////////////////////////////////////////////////////////
!
!     SUBROUTINE SHUFFLE2TARGET
!     Re-distributes values in vector 'init' between elements so that ...
!     1. resulting vector is equal to 'target'
!     2. sum over values in 'init' is conserved (equals sum over elements in 'target')
!     Re-distribution matrix (from element 'from' -> to element 'to') is returned ('DF_corr')
!     bs, 8.7.2013

      subroutine shuffle2target(init,trgt,DF_corr,DF_tmp,luold,nlutr,jpngr,error)

      IMPLICIT NONE
#include "para.inc"

!     ARGUMENTS
      INTEGER error,nlutr,jpngr
      REAL*8 init(nlutr),
     $     trgt(nlutr),
     $     DF_corr(nlutr,nlutr),
     $     DF_tmp(nlutr,nlutr),
     $     luold(nlutr)

!     LOCAL VARIABLES
      INTEGER lu,from,to,i
      REAL*8 min_precision
      PARAMETER(min_precision=1.d-12)
      INTEGER :: NA=9999

#if _LU_transitions
#if _LU_generated_transitions == 0
!     ////////////////////////////////////////////////////////
!     VERSION FOR LUH DATA 
!     --------------------------------------------------------
      INTEGER j,lutmp,control,ffrom
      LOGICAL addback_options(nlutr),
     $     above(nlutr),                                    !elements in init greater than trgt
     $     below(nlutr)                                       !elements in init smaller than trgt
      REAL*8 distribute,
     $     space,
     $     add,
     $     addtrans,
     $     reducetrans,
     $     addback
      INTEGER, DIMENSION(nlutr,nlutr-1) :: prio

!     define priority array: row number = from-category,
!     column number: to-category by priority (col.1 ==> 1st prio., ...)
      prio = reshape((/                                     
     $     lusecd,lupasture,lupasture,lucrop,lucrop,        !1st prio 
     $     lucrop,lucrop,lusecd,lusecd,lupasture,           !2nd prio
     $     lupasture,lubuilt,lubuilt,lubuilt,lusecd,        !3rd prio
     $     lubuilt,lunat,lunat,lunat,lunat /),              !4th prio
     $     shape(prio))

c$$$      print*,'prio'
c$$$      do lu=1,nlu
c$$$        print*,prio(lu,:)
c$$$      enddo
      
      control=0
      addback_options(:)=.true.
 222  control=control+1
      if (control.gt.12) then
        print*,'jpngr,distribute',jpngr,distribute
        error=2
        goto 333        
c        stop 'shuffled more than 12 times...'
      endif
      
!     conservation constraint: sum over elements must be equal in init and trgt
      if (abs(sum(init)-sum(trgt)).gt.min_precision) then
        error=1
        print*,'diff',abs(sum(init)-sum(trgt))
        goto 333
      endif

!     find elements in init that are greater than respective elements in trgt
      above(:) = .false.
      do lu=1,nlutr
        if (init(lu).gt.trgt(lu)) above(lu) = .true.
      enddo

      ! print*,'shuffle2target for jpngr,control',jpngr,control

!     Loop over elements above trgt and distribute excendence (diff to trgt)
!     over elements below trgt. Use pre-defined priority of transitions.
      do from=1,nlutr
        if (above(from)) then
         
!     determine how much of the element above trgt is to be distributed
          distribute=init(from)-trgt(from)
          ! print*,'taking amount,from',distribute,from
!     distribute this over elements below trgt
            do i=1,nlutr-1
              to=prio(from,i)
              if (init(to).lt.trgt(to)) then
                ! print*,'trying to put amount, to',distribute, to        
                space=trgt(to)-init(to)
                add=min(space,distribute)

                if (add.gt.0.0d0) then
                  ! print*,'adding amount, to',add,to,'by means ...'
!     --- found a pair (from/to) with (too high/too low) resulting area ---
!     1. option: if there exists a transition in original data DF_tr that
!     can be reduced to match target, reduce it
                  reducetrans=min(DF_tmp(to,from),add)
                  if (reducetrans.gt.0.0d0) then
                    ! print*,'1st option: reducing transition from,to,by',to,from,reducetrans
                    DF_corr(to,from)=DF_corr(to,from)-reducetrans
                    DF_tmp(to,from)=DF_tmp(to,from)-reducetrans
!                    luold(to)=luold(to)+reducetrans
                    init(from)=init(from)-reducetrans
                    init(to)=init(to)+reducetrans
                    add=add-reducetrans
                    distribute=distribute-reducetrans
                    ! print*,'still leftover after 1st opt:',add
                  else
                    ! print*,'1st option not possible'
                  endif
                endif
                
                if (add.gt.0.0d0) then
!     2. option: if there is enough transitioning area available, create
!     an additional transition to cover mismatch
                  addtrans=min(luold(from),add)
                  if (addtrans.gt.0.0d0) then
                    ! print*,'2nd option: creating add. tr. from,to,by',from,to,addtrans
                    ! print*,'luold(from)',luold(from)
                    DF_corr(from,to)=DF_corr(from,to)+addtrans
                    DF_tmp(from,to)=DF_tmp(from,to)+addtrans
                    luold(from)=luold(from)-addtrans
                    init(from)=init(from)-addtrans
                    init(to)=init(to)+addtrans
                    add=add-addtrans
                    distribute=distribute-addtrans
                    ! print*,'still leftover after 2nd opt:',add
                  else
                    ! print*,'2nd option not possible'  
                  endif
                endif

              endif
            enddo
            
        endif
      enddo

!     conservation constraint: sum over elements must be equal in init and trgt
      if (abs(sum(init)-sum(trgt)).gt.min_precision) then
        error=1
        print*,'diff',abs(sum(init)-sum(trgt))
        goto 333
      endif

!     Loop over elements above trgt and distribute excendence (diff to trgt)
!     over elements below trgt. Use pre-defined priority of transitions.
!     find elements in init that are greater than respective elements in trgt
      above(:) = .false.
      do lu=1,nlutr
        if (init(lu).gt.trgt(lu)) above(lu) = .true.
      enddo

      do from=1,nlutr
        if (above(from)) then

!     determine how much of the element above trgt is to be distributed
          distribute=init(from)-trgt(from)
          ! print*,'ULTIMA RATIO: taking amount,from,control',distribute,from,control

!     ultima ratio: find any trans 'lutmp->from' into 'from' and reduce it.
!     This causes lutmp ending up too high. => start over shuffle2target 
!     with this "forced" mismatch.
c     if (distribute.gt.sum(DF_tmp(:,from)).and.distribute.gt.luold(from)) then
          ! print*,'distribute,from',distribute,from
          ! print*,'DF_tmp'
          do ffrom=1,nlutr
            ! print*,DF_tmp(:,ffrom)
          enddo
          ! print*,'luold(from)',luold(from)
c     endif
          do lutmp=1,nlutr
c            ! print*,'lutmp,DF_tmp(lutmp,from)',lutmp,DF_tmp(lutmp,from)
            ! print*,'addback_options',addback_options

            if (distribute.gt.0.0d0.and.lutmp.ne.from
     $           .and.addback_options(lutmp)
     $           ) then
              addback=min(distribute,DF_tmp(lutmp,from))
              ! print*,'put amount,from,to,DF_tmp(lutmp,from)',addback,from,lutmp,DF_tmp(lutmp,from)
              if (addback.gt.0.0d0) then
                DF_corr(lutmp,from)=DF_corr(lutmp,from)-addback
                DF_tmp(lutmp,from)=DF_tmp(lutmp,from)-addback
                init(from)=init(from)-addback
                init(lutmp)=init(lutmp)+addback
                distribute=distribute-addback
                addback_options(from)=.false.
                ! print*,'distribute',distribute
                if (distribute.eq.0.0d0) exit
              endif
            endif
          enddo
          
          if (distribute.gt.0.0d0) then
            ! print*,'still leftover, distribute',distribute
            do i=1,nlutr-1
              to=prio(from,i)
              if (init(to).lt.trgt(to)) then
                space=trgt(to)-init(to)
                add=min(space,distribute)
                if (add.gt.0.0d0) then
!     2. option: if there is enough transitioning area available, create
!     an additional transition to cover mismatch
                  addtrans=min(luold(from),distribute)
                  if (addtrans.gt.0.0d0) then
                    ! print*,'*** Creating add. tr. from,to,by',from,to,addtrans
                    ! print*,'luold(from)',luold(from)
                    DF_corr(from,to)=DF_corr(from,to)+addtrans
                    DF_tmp(from,to)=DF_tmp(from,to)+addtrans
                    luold(from)=luold(from)-addtrans
                    init(from)=init(from)-addtrans
                    init(to)=init(to)+addtrans
                    add=add-addtrans
                    distribute=distribute-addtrans
                    ! print*,'*** still leftover after 2nd opt:',add
                  else
                    ! print*,'*** not possible'  
                  endif
                endif
              endif
            enddo
          endif

c          if (distribute.gt.min_precision)
          
        endif
      enddo

!     conservation constraint: sum over elements must be equal in init and trgt
      if (abs(sum(init)-sum(trgt)).gt.min_precision) then
        error=1
        print*,'diff',abs(sum(init)-sum(trgt))
        goto 333
      endif

      do lu=1,nlu
        if (abs(init(lu)-trgt(lu)).gt.min_precision) then
          goto 222
        endif
      enddo

#else
!     ////////////////////////////////////////////////////////
!     VERSION FOR GENERATED TRANSITIONS
!     --------------------------------------------------------
!     'init' is a copy of 'lu_area_tr'. 'trgt' is a copy of 'lu_area'.
!     'lu_area_tr' represents states after land turnover (shifting cultivation).
!     'lu_area' simply holds relevant information for cropland, pasture and builtup.
!     primary and secondary in 'lu_area' can be ignored.
      
!     LOCAL VARIABLES
      INTEGER, DIMENSION(nlutr,nlunat+nlusecd) :: prio_claim
      INTEGER, DIMENSION(3,nlutr-1) :: prio_put
      LOGICAL :: revisit=.false.
      LOGICAL above(nlutr)
c      INTEGER :: control=1
c      REAL*8 :: stillrequired=0.0d0
c      INTEGER gaplu
      REAL*8 required,
     $     available,
     $     trans,
     $     distribute,
     $     space,
     $     addtrans,
     $     add
c     $     reducetrans

!     define priority array: row number = to-category,
!     column number: from-category by priority (col.1 ==> 1st prio., ...)
      prio_claim = reshape((/                                     
     $     NA,NA,lunat,lusecd,lusecd, !1st prio
     $     NA,NA,lusecd,lunat,lunat /), !2nd prio
     $     shape(prio_claim))

!     define priority array: row number = from-category,
!     column number: to-category by priority (col.1 ==> 1st prio., ...)
      prio_put = reshape((/                                     
     $     NA,NA,lupasture,lucrop,lucrop,                   !1st prio 
     $     NA,NA,lubuilt,lubuilt,lupasture,                   !2nd prio
     $     NA,NA,lubuilt,lubuilt,lubuilt,                    !3rd prio
     $     NA,NA,lunat,lunat,lunat /),                      !4th prio
     $     shape(prio_put))

! print*,'shuffle2target for jpngr',jpngr
      
!     Conservation constraint is not applicable: In target (lu_area), sum of
!     primary and secondary (lu_area_tr) is unchanged from previous year
!     (lu_area_old), while cropland, pasture, and builtup (lu_area) have
!     been updated. Test, if this is the case:
      if (abs((
     $     sum(trgt)-sum(init)
     $     )-(
     $     (trgt(lucrop)+trgt(lupasture)+trgt(lubuilt))-(init(lucrop)+init(lupasture)+init(lubuilt))
     $     )
     $     ).gt.min_precision) then
        error=1
        goto 333
      endif


!     Below loops area programmed so that required order is ...,lucrop,lupasture,lubuilt.
      if (lupasture.ne.lucrop+1.or.lubuilt.ne.lucrop+2.or.lubuilt.ne.lupasture+1) then
        stop 'lucrop,lupasture,lubuilt should be in that order'
      endif
      
 222  if (error.eq.0) then
!     --------------------------------------------------------
!     1st approach: All expanding LU categories get land from
!     primary or secondary, according to priority list. And
!     conctracting LU categories' land is added to secondary.
!     --------------------------------------------------------      
        
!     Loop over cropland, pasture, and builtup and find additional transitions
!     to match their target.
        do lu=lucrop,lubuilt

!     expanding land use category
          if (trgt(lu).gt.init(lu)) then
            to=lu
            required=trgt(to)-init(to)
            do i=1,nlunat+nlusecd
              
!     claim land according to priority list
              from=prio_claim(to,i)
              available=luold(from)
              trans=min(required,available)
              ! print*,'to, required',to, required
              ! print*,'from, available, trans',from,available,trans
              DF_corr(from,to)=DF_corr(from,to)+trans
              !trgt(from)=trgt(from)-trans
              luold(from)=luold(from)-trans
              init(from)=init(from)-trans
              init(to)=init(to)+trans
              required=required-trans
              if (required.eq.0.0d0) exit
            enddo
            if (required.gt.min_precision) then
!     land requirement for expanding categories cannot be met by
!     land available in primary and secondary. => apply 2nd approach
              error=2
              ! print*,'trying 2nd approach'
              goto 333
            endif
            
!     contracting land use category
          else if (trgt(lu).lt.init(lu)) then
            from=lu
            distribute=init(from)-trgt(from)
            
!     add transition, put all abandoned land to secondary
            ! print*,'init,trgt,luold,from',init(from),trgt(from),luold(from),from
            to=lusecd
            addtrans=min(luold(from),distribute)
            if (addtrans.gt.0.0d0) then
              DF_corr(from,to)=DF_corr(from,to)+addtrans
              DF_tmp(from,to)=DF_tmp(from,to)+addtrans
              luold(from)=luold(from)-addtrans
              init(from)=init(from)-addtrans
              init(to)=init(to)+addtrans
              !trgt(to)=trgt(to)+addtrans
              distribute=distribute-addtrans
            endif
            ! print*,'distribute',distribute
            if (distribute.gt.min_precision) then
              print*,'jpngr,distribute',jpngr,distribute
              stop 'could not distribute all' 
            endif
          endif
          
        enddo

      else if (error.eq.2) then
!     --------------------------------------------------------
!     2nd approach: Expanding LU categories get land first
!     from contracting LU categories (instead of only from
!     primary or secondary).
!     --------------------------------------------------------
        ! print*,'doing second approach'
        error=0
        
!     find elements in init that are greater than respective elements in trgt
        above(:) = .false.
        do lu=lucrop,lubuilt
          if (init(lu).gt.trgt(lu)) above(lu) = .true.
        enddo

        ! print*,'init',init
        ! print*,'trgt',trgt
        ! print*,'above',above

!     Loop over elements above trgt and distribute excendence (diff to trgt)
!     over elements below trgt. Use pre-defined priority of transitions.
        do from=lucrop,lubuilt
          if (above(from)) then
            
!     determine how much of the element above trgt is to be distributed
            distribute=init(from)-trgt(from)
            ! print*,'distribute,from',distribute,from

!     print*,'taking amount,from',distribute,from
!     distribute this over elements below trgt
            do to=lucrop,lubuilt
              if (to.ne.from) then
c     to=prio_put(from,i)
                
                if (init(to).lt.trgt(to)) then
                  ! print*,'trying to put amount, to',distribute, to        
                  space=trgt(to)-init(to)
                  add=min(space,distribute)
                  
                  if (add.gt.0.0d0) then
!     If there is enough transitioning area available, create
!     an additional transition to cover mismatch
                    addtrans=min(luold(from),add)
                    if (addtrans.gt.0.0d0) then
                      ! print*,'creating add. tr. from,to,by',from,to,addtrans
                      ! print*,'luold(from)',luold(from)
                      DF_corr(from,to)=DF_corr(from,to)+addtrans
                      DF_tmp(from,to)=DF_tmp(from,to)+addtrans
                      luold(from)=luold(from)-addtrans
                      init(from)=init(from)-addtrans
                      init(to)=init(to)+addtrans
                      add=add-addtrans
                      distribute=distribute-addtrans
                      ! print*,'still leftover after 2nd opt:',add
                    else
                      ! print*,'2nd option not possible'  
                    endif
                  endif

                endif
              endif
            enddo
            
          endif
        enddo

!     Re-shuffle remaining areas (involving primary and secondary)
        goto 222
        
      endif

      do lu=lucrop,lubuilt
        if (abs(init(lu)-trgt(lu)).gt.min_precision) then
          print*,'jpngr,lu',jpngr,lu
          print*,'target',trgt
          print*,'init',init
          stop 'INSIDE: target not achieved'
        endif
      enddo
      
#endif            
#endif
      
 333  return
      end subroutine shuffle2target


#if _water_isotopes
#if _echam
#if _fixoxygen18
!//////////////////////////////////////////////////////////////////////////////
!
!     SUBROUTINE GETOXYGEN18
!     Read map for d18O of soilwater and water vapor for this year.

      subroutine getoxygen18(realyear)

      IMPLICIT NONE
#include "para.inc"
#include "modelpara.inc"
#include "waterisotopes.inc"
      include 'coordinates.inc'
      include 'iodata.inc'
      include 'netcdf.inc'

!     ARGUMENTS
      REAL*8 realyear
      
!     LOCAL PARAMETERS
      INTEGER index(3)

!     LOCAL VARIABLES
      INTEGER error,jpngr
       
!     Set index corresponding to the year to be read in.
!     -------------------------------------------------------------------------
!     For all years before 1960 hold oxygen18 fields constant at 1960 AD. For all years 
!     beyond 2012 hold oxygen18 fields constant at 2012 AD.

      if (int(realyear).le.1960) then
         index(3) = max(min(int(realyear-1901.d0),1),60)
      else
         index(3) = min(int(realyear-1900.d0),112)
      endif   
          
!     Loop over spatial domain and get single values from NetCDF file
!     -------------------------------------------------------------------------        
      do jpngr=minline_task,maxline_task

        index(1)=ilon(jpngr)
        index(2)=ilat(jpngr)
        
          error =
     $         NF_GET_VAR1_DOUBLE(wifil,wivarid(1),index,soilw18(jpngr)) 
          if (error.ne.0) goto 100
          error =
     $         NF_GET_VAR1_DOUBLE(wifil,wivarid(2),index,vapor18(jpngr)) 
          if (error.ne.0) goto 100

      enddo
      
      return
      
 100  write(0,*) 'in getoxygen18: ' // NF_STRERROR(error)
      write(0,*) 'realyear',realyear
      write(0,*) 'index(:)',index(:)
      stop
      
      end subroutine getoxygen18
#endif
#endif      
#endif

