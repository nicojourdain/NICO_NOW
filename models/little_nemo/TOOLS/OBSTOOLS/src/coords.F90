MODULE coords

   IMPLICIT NONE

   INTEGER, PARAMETER     :: nsech = 167
   CHARACTER(len=20), DIMENSION(nsech)  :: cl_sech = (/ &
      & 'global              ',  &
      & 'nstrpac             ',  &
      & 'sstrpac             ',  &
      & 'npac                ',  &
      & 'spac                ',  &
      & 'trpac               ',  &
      & 'natl                ',  &
      & 'satl                ',  &
      & 'tratl               ',  &
      & 'nstratl             ',  &
      & 'sstratl             ',  &
      & 'neatl               ',  &
      & 'nwatl               ',  &
      & 'equa                ',  &
      & 'nino1               ',  &
      & 'nino2               ',  &
      & 'nino12              ',  &
      & 'nino3               ',  &
      & 'nino4               ',  &
      & 'nino34              ',  &
      & 'ind1                ',  &
      & 'ind2                ',  &
      & 'ind3                ',  &
      & 'eq1                 ',  &
      & 'eq2                 ',  &
      & 'eq3                 ',  &
      & 'eq4                 ',  &
      & 'neq1                ',  &
      & 'neq2                ',  &
      & 'neq3                ',  &
      & 'neq4                ',  &
      & 'eqpac               ',  &
      & 'eqind               ',  &
      & 'atl1                ',  &
      & 'atl2                ',  &
      & 'atl3                ',  &
      & 'eqatl               ',  &
      & 'trop                ',  &
      & 'nxtrp               ',  &
      & 'sxtrp               ',  &
      & 'trind               ',  &
      & 'sind                ',  &
      & 'nepac               ',  &
      & 'nwpac               ',  &
      & 'trepac              ',  &
      & 'trwpac              ',  &
      & 'p15n38w             ',  &
      & 'p12n38w             ',  &
      & 'p8n38w              ',  &
      & 'p4n38w              ',  &
      & 'p0n35w              ',  &
      & 'p21n23w             ',  &
      & 'p12n23w             ',  &
      & 'p4n23w              ',  &
      & 'p0n23w              ',  &
      & 'p0n10w              ',  &
      & 'p0n0w               ',  &
      & 'p5s10w              ',  &
      & 'p10s10w             ',  &
      & 't0n156e             ',  &
      & 't0n165e             ',  &
      & 't0n180e             ',  &
      & 't0n170w             ',  &
      & 't0n155w             ',  &
      & 't0n140w             ',  &
      & 't0n125w             ',  &
      & 't0n110w             ',  &
      & 't0n95w              ',  &
      & 't5n156e             ',  &
      & 't5s156e             ',  &
      & 't5n165e             ',  &
      & 't5n180e             ',  &
      & 't5n170w             ',  &
      & 't5n155w             ',  &
      & 't5n140w             ',  &
      & 't5n125w             ',  &
      & 't5n110w             ',  &
      & 't5n95w              ',  &
      & 't5s165e             ',  &
      & 't5s180e             ',  &
      & 't5s170w             ',  &
      & 't5s155w             ',  &
      & 't5s140w             ',  &
      & 't5s125w             ',  &
      & 't5s110w             ',  &
      & 't5s95w              ',  &
      & 'r8s55e              ',  &
      & 'r12s55e             ',  &
      & 'r4s67e              ',  &
      & 'r8s67e              ',  &
      & 'r12s67e             ',  &
      & 'r0n80e              ',  &
      & 'r4s80e              ',  &
      & 'r12s80e             ',  &
      & 'r12n90e             ',  &
      & 'r8n90e              ',  &
      & 'r4n90e              ',  &
      & 'r0n90e              ',  &
      & 'r5s95e              ',  &
      & 'r8s95e              ',  &
      & 'r8s100e             ',  &
      & 'NE_subtrop_pac      ',  &
      & 'NW_subtrop_pac      ',  &
      & 'NE_extratrop_pac    ',  &
      & 'NW_extratrop_pac    ',  &
      & 'SE_subtrop_pac      ',  &
      & 'SW_subtrop_pac      ',  &
      & 'NE_subtrop_atl      ',  &
      & 'NW_subtrop_atl      ',  &
      & 'NE_extratrop_atl    ',  &
      & 'NW_extratrop_atl    ',  &
      & 'SE_subtrop_atl      ',  &
      & 'SW_subtrop_atl      ',  &
      & 'SE_subtrop_ind      ',  &
      & 'SW_subtrop_ind      ',  &
      & 'Southern_ocean_pac  ',  &
      & 'Southern_ocean_atl  ',  &
      & 'Southern_ocean_ind  ',  &
      & 'GLOBAL05            ',  &
      & 'GLOBAL10            ',  &
      & 'GLOBAL15            ',  &
      & 'GLOBAL20            ',  &
      & 'GLOBAL25            ',  &
      & 'GLOBAL30            ',  &
      & 'GLOBAL40            ',  &
      & 'GLOBAL50            ',  &
      & 'GLOBAL60            ',  &
      & 'ARCTIC              ',  &
      & 'ATL60NA             ',  &
      & 'ATL50NA             ',  &
      & 'ATL40NA             ',  &
      & 'ATL35NA             ',  &
      & 'ATL30NA             ',  &
      & 'ATL26NA             ',  &
      & 'ATL10NA             ',  &
      & 'ATLEQA              ',  &
      & 'ATL10SA             ',  &
      & 'ATL20SA             ',  &
      & 'ATL30SA             ',  &
      & 'PAC60NA             ',  &
      & 'PAC50NA             ',  &
      & 'PAC40NA             ',  &
      & 'PAC35NA             ',  &
      & 'PAC30NA             ',  &
      & 'PAC20NA             ',  &
      & 'PAC10NA             ',  &
      & 'PACEQA              ',  &
      & 'INP10SA             ',  &
      & 'PAC20SA             ',  &
      & 'PAC30SA             ',  &
      & 'INDEQA              ',  &
      & 'IND20SA             ',  &
      & 'IND30SA             ',  &
      & 'GLB60NA             ',  &
      & 'GLB50NA             ',  &
      & 'GLB40NA             ',  &
      & 'GLB30NA             ',  &
      & 'GLB20NA             ',  &
      & 'GLB10NA             ',  &
      & 'GLBEQA              ',  &
      & 'GLB10SA             ',  &
      & 'GLB20SA             ',  &
      & 'GLB30SA             ',  &
      & 'GLB40SA             ',  &
      & 'GLB50SA             ',  &
      & 'GLB60SA             ',  &
      & 'npac25              '  &
      & /)

   ! User defined areas
   INTEGER :: nboxuser
   CHARACTER(len=20), DIMENSION(:), ALLOCATABLE  :: cl_boxes_user
   REAL, DIMENSION(:,:), ALLOCATABLE :: areas

   ! zonal sections
   INTEGER, PARAMETER     :: nsecz = 50
   CHARACTER(len=20), DIMENSION(nsecz)  :: cl_secz = (/ &
      & 'LOMBOK              ',  &
      & 'BANDA               ',  &
      & 'MAKASSAR            ',  &
      & 'SAVU                ',  &
      & 'MALACCAS            ',  &
      & 'PHILIPINES          ',  &
      & 'YUCATAN             ',  &
      & 'GIN                 ',  &
      & 'LABRADOR            ',  &
      & 'ATL60N              ',  &
      & 'ATL50N              ',  &
      & 'ATL40N              ',  &
      & 'ATL35N              ',  &
      & 'ATL30N              ',  &
      & 'ATL27N              ',  &
      & 'ATL26N              ',  &
      & 'ATL10N              ',  &
      & 'ATLEQ               ',  &
      & 'ATL10S              ',  &
      & 'ATL20S              ',  &
      & 'ATL30S              ',  &
      & 'PAC60N              ',  &
      & 'PAC50N              ',  &
      & 'PAC40N              ',  &
      & 'PAC35N              ',  &
      & 'PAC30N              ',  &
      & 'PAC25N              ',  &
      & 'PAC20N              ',  &
      & 'PAC10N              ',  &
      & 'PACEQ               ',  &
      & 'INP10S              ',  &
      & 'PAC20S              ',  &
      & 'PAC30S              ',  &
      & 'INDEQ               ',  &
      & 'IND20S              ',  &
      & 'IND30S              ',  &
      & 'GLB60N              ',  &
      & 'GLB50N              ',  &
      & 'GLB40N              ',  &
      & 'GLB30N              ',  &
      & 'GLB20N              ',  &
      & 'GLB10N              ',  &
      & 'GLBEQ               ',  &
      & 'GLB10S              ',  &
      & 'GLB20S              ',  &
      & 'GLB30S              ',  &
      & 'GLB40S              ',  &
      & 'GLB50S              ',  &
      & 'GLB60S              ',  &
      & 'SUM-DARWIN          '   &
      & /)

   ! meridional sections
   INTEGER, PARAMETER     :: nsecm = 10
   CHARACTER(len=20), DIMENSION(nsecm)  :: cl_secm = (/ &
      & 'IT                  ',  &
      & 'ITA                 ',  &
      & 'TIMOR               ',  &
!      & 'OMBAI               ',  &
!      & 'SUMBA               ',  &
!      & 'LUZON               ',  &
      & 'DRAKE               ',  &
      & 'TORRES              ',  &
      & 'MED                 ',  &
      & 'FLORIDA             ',  &
      & 'ANTILLAS            ',  &
      & 'GOODHOPE            ', &
      & 'SOUTHAUS            '   &
      & /)

CONTAINS

   SUBROUTINE coord_area( reg, area )
      !-----------------------------------------------------------------------
      !
      !                       ROUTINE coord_area
      !                     **********************
      !
      !  Purpose :
      !  -------
      !    Define coordinates of different regions 
      !
      !   Modifications :
      !   -------------
      !
      !      SEE: /home/rd/ocx/postp/NEWGRIB/regions.txt 
      !      and  /home/rd/nep/sms/verify/automat/include/regions.h
      !
      !      modification     : 04-09 (N. Daget)
      !      modification     : 04-09 (N. Daget) add new regions
      IMPLICIT NONE
      !----------------------------------------------------------------------
      ! local declarations
      !----------------------------------------------------------------------
      !
      CHARACTER(len=20), INTENT(inout) :: reg
      REAL, DIMENSION(4), INTENT(out) :: area
      !
      reg=TRIM(reg)
      ! 
      SELECT CASE (reg)
      CASE ('global')
         area = (/0.,360.,-90.,90./)
      CASE ('nstrpac')
         area = (/105.,270.,10.,30./)
      CASE ('sstrpac')
         area = (/105.,270.,-30.,-10./)
      CASE ('npac')    
         area = (/100.,260.,30.,70./)
      CASE ('spac')
         area = (/150.,290.,-70.,-30./)
      CASE ('trpac')   
         area = (/125.,280.,-30.,30./)
      CASE ('natl')
         area = (/290.,15.,30.,70./)
      CASE ('satl')
         area = (/290.,20.,-70.,-30./)
      CASE ('tratl')
         area = (/280.,20.,-20.,30./)
      CASE ('nstratl')
         area = (/280.,20.,5.,28./)
      CASE ('sstratl')
         area = (/300.,20.,-20.,5./)
      CASE ('neatl')
         area = (/320.,15.,30.,70./)
      CASE ('nwatl')
         area = (/260.,320.,30.,70./)
      CASE ('equa')
         area = (/0.,360.,-2.,2./)
      CASE ('nino1')
         area = (/270.,280.,-10.,-5./)
      CASE ('nino2')
         area = (/270.,280.,-5.,0./)
      CASE ('nino12')
         area = (/270.,280.,-10.,0./)
      CASE ('nino3')
         area = (/210.,270.,-5.,5./)
      CASE ('nino4')
         area = (/160.,210.,-5.,5./)
      CASE ('nino34')
         area = (/190.,240.,-5.,5./)
      CASE ('ind1')
         area = (/50.,70.,-10.,10./)
      CASE ('ind2')
         area = (/90.,110.,-10.,0./)
      CASE ('ind3')
         area = (/50.,90.,-10.,0./)
      CASE ('eq1')
         area = (/230.,270.,-5.,5./)
      CASE ('eq2')
         area = (/190.,230.,-5.,5./)
      CASE ('eq3')
         area = (/150.,190.,-5.,5./)
      CASE ('eq4')
         area = (/120.,150.,-5.,5./)
      CASE ('neq1')
         area = (/230.,270.,5.,15./)
      CASE ('neq2')
         area = (/190.,230.,5.,15./)
      CASE ('neq3')
         area = (/150.,190.,5.,15./)
      CASE ('neq4')
         area = (/120.,150.,5.,15./)
      CASE ('eqpac')
         area = (/130.,280.,-5.,5./)
      CASE ('eqind')
         area = (/40.,120.,-5.,5./)
      CASE ('atl1')
         area = (/315.,340.,0.,10./)
      CASE ('atl2')
         area = (/0.,10.,-3.,3./)
      CASE ('atl3')
         area = (/340.,360.,-3.,3./)
      CASE ('eqatl')
         area = (/290.,30.,-5.,5./)
      CASE ('trop')
         area = (/0.,360.,-30.,30./)      ! Tropics (second definition)
      CASE ('nxtrp')
         area = (/0.,360.,30.,70./)     ! Northern Extratropics
      CASE ('sxtrp')
         area = (/0.,360.,-70.,-30./)   ! Southern Extratropics
      CASE ('trind')
         area = (/40.,120.,-30.,30./)
      CASE ('sind')
         area = (/20.,150.,-70.,-30./)
      CASE ('nepac')
         area = (/210.,260.,30.,70./)
      CASE ('nwpac')
         area = (/100.,210.,30.,70./)
      CASE ('trepac')
         area = (/210.,270.,-30.,30./)
      CASE ('trwpac')
         area = (/100.,210.,-30.,30./)
     ! PIRATA
      CASE ('p20n38w') 
         area = (/321.,323.,19.,21./)
      CASE ('p15n38w') 
         area = (/321.,323.,14.,16./)
      CASE ('p12n38w') 
         area = (/321.,323.,11.,13./)
      CASE ('p8n38w') 
         area = (/321.,323.,7.,9./)
      CASE ('p4n38w') 
         area = (/321.,323.,3.,5./)
      CASE ('p0n35w') 
         area = (/324.,326.,-0.5,0.5/)
      CASE ('p21n23w') 
         area = (/336.,338.,20.,22./)
      CASE ('p12n23w') 
         area = (/336.,338.,11.,13./)
      CASE ('p4n23w') 
         area = (/336.,338.,3.,5./)
      CASE ('p0n23w') 
         area = (/336.,338.,-0.5,0.5/)
      CASE ('p0n10w') 
         area = (/349.,351.,-0.5,0.5/)
      CASE ('p0n0w') 
         area = (/359.,1.,-0.5,0.5/)
      CASE ('p5s10w') 
         area = (/349.,351.,-6.,-4./)
      CASE ('p10s10w') 
         area = (/349.,351.,-11.,-9./)

      ! TAO
      CASE ('t0n156e') 
         area = (/155.,157.,-0.5,0.5/)
      CASE ('t0n165e') 
         area = (/164.,166.,-0.5,0.5/)
      CASE ('t0n180e') 
         area = (/179.,181.,-0.5,0.5/)
      CASE ('t0n170w') 
         area = (/189.,191.,-0.5,0.5/)
      CASE ('t0n155w') 
         area = (/204.,206.,-0.5,0.5/)
      CASE ('t0n140w')
         area = (/219.,221.,-0.5,0.5/)
      CASE ('t0n125w')
         area = (/234.,236.,-0.5,0.5/)
      CASE ('t0n110w')
         area = (/249.,251.,-0.5,0.5/)
      CASE ('t0n95w')
         area = (/264.,266.,-0.5,0.5/)
      CASE ('t5n156e')
         area = (/155.,157.,4.5,5.5/)
      CASE ('t5n165e')
         area = (/164.,166.,4.5,5.5/)
      CASE ('t5n180e')
         area = (/179.,181.,4.5,5.5/)
      CASE ('t5n170w')
         area = (/189.,191.,4.5,5.5/)
      CASE ('t5n155w') 
         area = (/204.,206.,4.5,5.5/)
      CASE ('t5n140w')
         area = (/219.,221.,4.5,5.5/)
      CASE ('t5n125w')
         area = (/234.,236.,4.5,5.5/)
      CASE ('t5n110w')
         area = (/249.,251.,4.5,5.5/)
      CASE ('t5n95w')
         area = (/264.,266.,4.5,5.5/)
      CASE ('t5s156e')
         area = (/155.,157.,-5.5,-5.5/)
      CASE ('t5s165e')
         area = (/164.,166.,-5.5,4.5/)
      CASE ('t5s180e')
         area = (/179.,181.,-5.5,-4.5/)
      CASE ('t5s170w')
         area = (/189.,191.,-5.5,-4.5/)
      CASE ('t5s155w') 
         area = (/204.,206.,-5.5,-4.5/)
      CASE ('t5s140w')
         area = (/219.,221.,-5.5,-4.5/)
      CASE ('t5s125w')
         area = (/234.,236.,-5.5,-4.5/)
      CASE ('t5s110w')
         area = (/249.,251.,-5.5,-4.5/)
      CASE ('t5s95w')
         area = (/264.,266.,-5.5,-4.5/)
      !RAMA
      CASE ('r8s55e')
         area = (/54.,56.,-8.,-7./)
      CASE ('r12s55e')
         area = (/54.,56.,-13.,-11./)
      CASE ('r4s67e')
         area = (/66.,68.,-4.5,-3.5/)
      CASE ('r8s67e')
         area = (/66.,68.,-9.,-7./)
      CASE ('r12s67e')
         area = (/66.,68.,-13.,-11./)
      CASE ('r0n80e')
         area = (/79.,81.,-0.5,0.5/)
      CASE ('r4s80e')
         area = (/79.,81.,-4.5,-3.5/)
      CASE ('r8s80e')
         area = (/79.,81.,-9.,-7./)
      CASE ('r12s80e')
         area = (/79.,81.,-13.,-11./)
      CASE ('r12n90e')
         area = (/89.,91.,11.,13./)
      CASE ('r8n90e')
         area = (/89.,91.,7.,9./)
      CASE ('r4n90e')
         area = (/89.,91.,3.5,4.5/)
      CASE ('r0n90e')
         area = (/89.,91.,-0.5,0.5/)
      CASE ('r5s95e')
         area = (/94.,96.,-5.5,-4.5/)
      CASE ('r8s95e')
         area = (/94.,96.,-9.,-7./)
      CASE ('r8s100e')
         area = (/99.,101.,-9.,-7./)


      ! ENACT
      CASE ('NE_subtrop_pac')
         area = (/190.,260.,10.,30./)
      CASE ('NW_subtrop_pac')
         area = (/120.,190.,10.,30./)
      CASE ('NE_extratrop_pac')
         area = (/190.,250.,30.,60./)
      CASE ('NW_extratrop_pac')
         area = (/120.,190.,30.,60./)
      CASE ('SE_subtrop_pac')
         area = (/200.,300.,-30.,-10./)
      CASE ('SW_subtrop_pac')
         area = (/143.,200.,-30.,-10./)
      CASE ('NE_subtrop_atl')
         area = (/320.,355.,10.,30./)
      CASE ('NW_subtrop_atl')
         area = (/283.,320.,10.,30./)
      CASE ('NE_extratrop_atl')
         area = (/320.,360.,30.,60./)
      CASE ('NW_extratrop_atl')
         area = (/285.,320.,30.,60./)
      CASE ('SE_subtrop_atl')
         area = (/350.,20.,-30.,-10./)
      CASE ('SW_subtrop_atl')
         area = (/300.,350.,-30.,-10./)
      CASE ('SE_subtrop_ind')
         area = (/80.,120.,-30.,-10./)
      CASE ('SW_subtrop_ind')
         area = (/30.,80.,-30.,-10./)
      CASE ('Southern_ocean_pac')
         area = (/130.,290.,-80.,-30./)
      CASE ('Southern_ocean_atl')
         area = (/290.,20.,-80.,-30./)
      CASE ('Southern_ocean_ind')
         area = (/20.,130.,-80.,-30./)
      ! Global areas different latitudes
      CASE ('GLOBAL05')
         area = (/0.,360.,-5.,5./)
      CASE ('GLOBAL10')
         area = (/0.,360.,-10.,10./)
      CASE ('GLOBAL15')
         area = (/0.,360.,-15.,15./)
      CASE ('GLOBAL20')
         area = (/0.,360.,-20.,20./)
      CASE ('GLOBAL25')
         area = (/0.,360.,-25.,25./)
      CASE ('GLOBAL30')
         area = (/0.,360.,-30.,30./)
      CASE ('GLOBAL40')
         area = (/0.,360.,-40.,40./)
      CASE ('GLOBAL50')
         area = (/0.,360.,-50.,50./)
      CASE ('GLOBAL60')
         area = (/0.,360.,-60.,60./)
      CASE ('ARCTIC')
         area = (/0.,360.,65.,90./)
      CASE  ('ATL60NA')
         area=(/260.,9.13,59.,61./) 
      CASE  ('ATL50NA')
         area=(/260.,5.,49.,51./) 
      CASE  ('ATL40NA')
         area=(/260.,358.,39.,41./) 
      CASE  ('ATL35NA')
         area=(/260.,360.,34.,36./) 
      CASE  ('ATL30NA')
         area=(/260.,360.,29.,31./) 
      CASE  ('ATL26NA')
         area=(/260.,360.,25.,27./) 
      CASE  ('ATL20NA')
         area=(/260.,360.,19.,21./) 
      CASE  ('ATL10NA')
         area=(/290.,360.,9.,11./) 
      CASE  ('ATLEQA')
         area=(/289.,11.,-1.,1./) 
      CASE  ('ATL10SA')
         area=(/320.,15.,-11.,-9./) 
      CASE  ('ATL20SA')
         area=(/318.,15.,-21.,-19./) 
      CASE  ('ATL30SA')
         area=(/310.,20.,-31.,-29./) 
      CASE  ('PAC60NA')
         area=(/140.,250.,59.,61./) 
      CASE  ('PAC50NA')
         area=(/130.,240.,49.,51./) 
      CASE  ('PAC40NA')
         area=(/125.,240.,39.,41./) 
      CASE  ('PAC35NA')
         area=(/115.,242.,34.,36./) 
      CASE  ('PAC30NA')
         area=(/115.,250.,29.,31./) 
      CASE  ('PAC20NA')
         area=(/100.,260.,19.,21./) 
      CASE  ('PAC10NA')
         area=(/105.,275.,9.,11./) 
      CASE  ('PACEQA')
         area=(/115.,282.,-1.,1./) 
      CASE  ('INP10SA')
         area=(/35.,290.,-11.,-9./) 
      CASE  ('PAC20SA')
         area=(/140.,292.,-21.,-19./) 
      CASE  ('PAC30SA')
         area=(/150.,292.,-31.,-29./) 
      CASE  ('INDEQA')
         area=(/40.,115.,-1.,1./) 
      CASE  ('IND20SA')
         area=(/30.,130.,-21.,-19./) 
      CASE  ('IND30SA')
         area=(/30.,120.,-31.,-29./) 
      CASE  ('GLB60NA')
         area=(/166.,9.13,59.,61./) 
      CASE  ('GLB50NA')
         area=(/0.,360.,49.,51./) 
      CASE  ('GLB40NA')
         area=(/0.,360.,39.,41./) 
      CASE  ('GLB30NA')
         area=(/0.,360.,29.,31./) 
      CASE  ('GLB20NA')
         area=(/0.,360.,19.,21./) 
      CASE  ('GLB10NA')
         area=(/0.,360.,9.,11./) 
      CASE  ('GLBEQA')
         area=(/0.,360.,-1.,1./) 
      CASE  ('GLB10SA')
         area=(/0.,360.,-11.,-9./) 
      CASE  ('GLB20SA')
         area=(/0.,360.,-21.,-19./) 
      CASE  ('GLB30SA')
         area=(/0.,360.,-31.,-29./) 
      CASE  ('GLB40SA')
         area=(/0.,360.,-41.,-39./) 
      CASE  ('GLB50SA')
         area=(/0.,360.,-51.,-49./) 
      CASE  ('GLB60SA')
         area=(/0.,360.,-61.,-59./) 
      CASE ('npac25')    
         area = (/100.,260.,25.,70./)
      !Zonal sections
      ! Measurements of Indonesian Throughflow at 
      ! http://www.ocean.washington.edu/people/faculty/susanh/spga/spga.htm
      ! INSTANT obserational program

      CASE ('LOMBOK')
!         area=(/114.,118.,-8.,-8./)  
         area=(/114.,120.,-8.,-9./) ! first/last point rather than min,max
      CASE ('MAKASSAR')
!         area=(/114.,120.,-3.,-3./) 
         area=(/114.,121.,-3.,-3./) 
      CASE ('MALACCAS')
!         area=(/99.,102.,3.,3./) 
         area=(/103.,112.,-2.8,-2.8/) 
      CASE ('BANDA')
         area=(/122.,140.,-4.,-4./)
      CASE  ('SAVU')
!         area=(/122.,124.,-8.8,-8.8/) 
         area=(/120.,125.,-8.8,-9.4/) 
      CASE  ('PHILIPINES')
         area=(/106.,120.,10.985,10.985/) 
      CASE  ('YUCATAN')
!         area=(/273.,285.,20.,20./) 
         area=(/271.,283.,20.,21./) 
      CASE  ('GIN')
!         area=(/315.,7.,63.,63./) 
         area=(/315.,9.8,63.,63./) 
      CASE  ('LABRADOR')
!         area=(/290.,315.,61.,61./) 
         area=(/289.,310.,60.6,63.5/) 
      CASE  ('ATL60N')
!         area=(/260.,10.,57.,57./) 
!         area=(/260.,10.87,57.,57./) 
!         area=(/260.,11.2,57.,57./) 
         area=(/260.,9.13,60.,59.925/) 
      CASE  ('ATL50N')
         area=(/260.,5.,50.,50./) 
      CASE  ('ATL40N')
         area=(/260.,358.,40.,40./) 
      CASE  ('ATL35N')
         area=(/260.,360.,35.,35./) 
      CASE  ('ATL30N')
         area=(/260.,360.,30.,30./) 
      CASE  ('ATL27N')
         area=(/260.,360.,27.,27./) 
      CASE  ('ATL26N')
         area=(/260.,360.,26.,26./) 
      CASE  ('ATL20N')
         area=(/260.,360.,20.,20./) 
      CASE  ('ATL10N')
!         area=(/300.,360.,10.,10./) 
         area=(/290.,360.,10.,10./) 
      CASE  ('ATLEQ')
!         area=(/300.,10.,0.,0./) 
         area=(/289.,11.,0.,0./) 
      CASE  ('ATL10S')
         area=(/320.,15.,-10.,-10./) 
      CASE  ('ATL20S')
         area=(/318.,15.,-30.,-30./) 
      CASE  ('ATL30S')
         area=(/310.,20.,-30.,-30./) 
      CASE  ('PAC60N')
         area=(/140.,250.,60.,60./) 
      CASE  ('PAC50N')
         area=(/130.,240.,50.,50./) 
      CASE  ('PAC40N')
         area=(/125.,240.,40.,40./) 
      CASE  ('PAC35N')
!         area=(/115.,240.,35.,35./) 
         area=(/115.,242.,35.,35./) 
      CASE  ('PAC30N')
         area=(/115.,250.,30.,30./) 
      CASE  ('PAC25N')
         area=(/100.,260.,25.,25./) 
      CASE  ('PAC20N')
         area=(/100.,260.,20.,20./) 
      CASE  ('PAC10N')
!         area=(/98.,275.,10.,10./) 
         area=(/105.,275.,10.,10./) 
      CASE  ('PACEQ')
         area=(/115.,282.,0.,0./) 
      CASE  ('INP10S')
         area=(/35.,290.,-10.,-10./) 
      CASE  ('PAC20S')
         area=(/140.,292.,-20.,-20./) 
      CASE  ('PAC30S')
         area=(/150.,292.,-30.,-30./) 
      CASE  ('INDEQ')
         area=(/40.,115.,-0.,-0./) 
      CASE  ('IND20S')
         area=(/30.,130.,-20.,-20./) 
      CASE  ('IND30S')
         area=(/30.,120.,-30.,-30./) 
      CASE  ('GLB60N')
!         area=(/0.,360.,60.,60./) 
!         area=(/166.,10.,60.5,60./) 
!         area=(/166.,6.6,60.5,59.7/) 
         area=(/166.,9.13,60.5,59.925/) 
      CASE  ('GLB50N')
         area=(/0.,360.,50.,50./) 
      CASE  ('GLB40N')
         area=(/0.,360.,40.,40./) 
      CASE  ('GLB30N')
         area=(/0.,360.,30.,30./) 
      CASE  ('GLB20N')
         area=(/0.,360.,20.,20./) 
      CASE  ('GLB10N')
         area=(/0.,360.,10.,10./) 
      CASE  ('GLBEQ')
         area=(/0.,360.,0.,0./) 
      CASE  ('GLB10S')
         area=(/0.,360.,-10.,-10./) 
      CASE  ('GLB20S')
         area=(/0.,360.,-20.,-20./) 
      CASE  ('GLB30S')
         area=(/0.,360.,-30.,-30./) 
      CASE  ('GLB40S')
         area=(/0.,360.,-40.,-40./) 
      CASE  ('GLB50S')
         area=(/0.,360.,-50.,-50./) 
      CASE  ('GLB60S')
         area=(/0.,360.,-60.,-60./) 
      CASE  ('SUM-DARWIN')
         area=(/104.,131.,-4.9,-15.3/) 

      !Meridonal sections (for zonal transports)
      CASE ('IT'   )                    !From Flores to Australia
!         area=(/114.,114.,-22.,-8.5/)
         area=(/126.,126.,-8.8,-16./)
      CASE ('ITA'   )                   !From Sumatra to Australia
!         area=(/115.,114.,-22.,-3./)
         area=(/104.,115.,-4.9,-24.7/)
      CASE ('TIMOR')
         area=(/124.,124.,-17.,-9./) 
!      CASE  ('OMBAI')
!         area=(/124.5,124.5,-9.2,-8.2/) 
!      CASE  ('SUMBA') 
!         area=(/120.,120.,-9.3,-8.3/) 
!      CASE ('LUZON')
!         area=(/120.5,120.5,17.,23./) 
      CASE ('DRAKE')
!         area=(/290.,290.,-75.,-52./) 
!         area=(/-69.,-64.,-55.2,-65.9/) 
         area=(/291.,296.,-54.6,-65.9/) 
      CASE ('TORRES')
         area=(/143.,143.,-15.,-8./) 
      CASE ('MED')
         area=(/356.,356.,32.,40./) 
      CASE ('FLORIDA')
!         area=(/279.5,279.5,22.,28./) 
!         area=(/-81.,-79,26.5,21.9/) 
         area=(/-82.,-79.,28.2,22./) 
      CASE ('ANTILLAS')
!         area=(/290.,290.,10.,18./) 
         area=(/-72.,-72.,19.1,8.2/) 
      CASE ('GOODHOPE')
!         area=(/340.,340.,-80.,-30./) 
         area=(/23.,44.,-31.7,-68.2/) 
      CASE ('SOUTHAUS')
!         area=(/140.,140.,-80.,-30./) 
         area=(/133.,133.,-30.,-67.5/) 
      CASE default
         PRINT*,'area: ', reg, 'is not defined'
         CALL abort
      END SELECT

   END SUBROUTINE coord_area

   SUBROUTINE coord_user_init (sec)
      CHARACTER(len=1), INTENT(IN) :: sec
      CHARACTER(len=20), DIMENSION(:), ALLOCATABLE  :: cl_boxes
      INTEGER                            :: nbox
      CHARACTER(len=32) :: cdnamelist = 'coords.nml'
      LOGICAL :: lexists, lnodefaults
      CHARACTER(len=20) :: carea
      REAL :: lat1,lat2,lon1,lon2,dlat,dlon
      LOGICAL :: lreg, lstd
      INTEGER :: nlat,nlon
      INTEGER :: i,j,k
      NAMELIST/area/lstd,lreg,carea,lat1,lat2,lon1,lon2,dlat,dlon

      lnodefaults=.TRUE.
      nboxuser=0
      SELECT CASE (sec)
      CASE ('u')
         nbox=nsecm
         ALLOCATE(cl_boxes(nbox))
         cl_boxes(:)=cl_secm(:)
      CASE ('v')
         nbox=nsecz
         ALLOCATE(cl_boxes(nbox))
         cl_boxes(:)=cl_secz(:)
      CASE default
         nbox=nsech
         ALLOCATE(cl_boxes(nbox))
         cl_boxes(:)=cl_sech(:)
      END SELECT
      INQUIRE(file=cdnamelist,exist=lexists) 
      IF (lexists) THEN
         nboxuser=0
         OPEN(20,file=cdnamelist)
         DO
            carea='undefined'
            lat1=-90
            lat2=90
            lon1=0
            lon2=360
            dlat=10
            dlon=10
            lreg=.FALSE.
            lstd=.FALSE.
            READ(20,area,end=100)
            DO
               IF (lon1<0) lon1=lon1+360
               IF (lon1>360) lon1=lon1-360
               IF ((lon1>=0).AND.(lon1<=360)) EXIT
            ENDDO
            DO
               IF (lon2<0) lon2=lon2+360
               IF (lon2>360) lon2=lon2-360
               IF ((lon2>=0).AND.(lon2<=360)) EXIT
            ENDDO
            WRITE(*,area)
            IF (lreg.AND.(TRIM(carea)/='undefined')) THEN
               WRITE(*,*)'coord_init: please specify either lreg=true '//&
                  &      'or carea/=undefined'
               CALL abort
            ENDIF
            IF (TRIM(carea)/='undefined') THEN
               nboxuser=nboxuser+1
            ENDIF
            IF (lreg) THEN
               nlat=NINT((MAX(lat1,lat2)-MIN(lat1,lat2))/dlat)
               nlon=NINT((MAX(lon1,lon2)-MIN(lon1,lon2))/dlon)
               nboxuser=nboxuser+nlat*nlon
            ENDIF
            IF (lstd) THEN
               IF (lnodefaults) THEN
                  nboxuser=nboxuser+nbox
                  lnodefaults=.FALSE.
               ENDIF
            ENDIF
         END DO
100      CONTINUE
         WRITE(*,*)'Total areas = ',nboxuser
         IF (nboxuser==0) THEN
            CLOSE(20)
            WRITE(*,*)'coord_init: no boxes defined!!'
            CALL abort
         ENDIF
         ALLOCATE(cl_boxes_user(nboxuser))
         ALLOCATE(areas(4,nboxuser))
         nboxuser=0
         IF (.NOT.lnodefaults) THEN
            cl_boxes_user(1:nbox)=cl_boxes(1:nbox)
            DO i=1,nbox
               CALL coord_area( cl_boxes_user(i), areas(:,i) )
            ENDDO
            nboxuser=nboxuser+nbox
         ENDIF
         REWIND(20)
         WRITE(*,*)'Reading areas'
         DO
            carea='undefined'
            lat1=-90
            lat2=90
            lon1=0
            lon2=360
            dlat=10
            dlon=10
            lreg=.FALSE.
            lstd=.FALSE.
            READ(20,area,end=200)
            DO
               IF (lon1<0) lon1=lon1+360
               IF (lon1>360) lon1=lon1-360
               IF ((lon1>=0).AND.(lon1<=360)) EXIT
            ENDDO
            DO
               IF (lon2<0) lon2=lon2+360
               IF (lon2>360) lon2=lon2-360
               IF ((lon2>=0).AND.(lon2<=360)) EXIT
            ENDDO
            IF (TRIM(carea)/='undefined') THEN
               nboxuser=nboxuser+1
               cl_boxes_user(nboxuser)=carea
               areas(1,nboxuser)=MIN(lon1,lon2)
               areas(2,nboxuser)=MAX(lon1,lon2)
               areas(3,nboxuser)=MIN(lat1,lat2)
               areas(4,nboxuser)=MAX(lat1,lat2)
            ENDIF
            IF (lreg) THEN
               nlat=NINT((MAX(lat1,lat2)-MIN(lat1,lat2))/dlat)
               nlon=NINT((MAX(lon1,lon2)-MIN(lon1,lon2))/dlon)
               k=0
               DO j=1,nlat
                  DO i=1,nlon
                     k=k+1
                     areas(1,k+nboxuser)=MIN(lon1,lon2)+(i-1)*dlon
                     areas(2,k+nboxuser)=MIN(lon1,lon2)+i*dlon
                     areas(3,k+nboxuser)=MIN(lat1,lat2)+(j-1)*dlat
                     areas(4,k+nboxuser)=MIN(lat1,lat2)+j*dlat
                     WRITE(cl_boxes_user(k+nboxuser)(1:5),'(I4.4,A1)') &
                        & NINT(areas(1,k+nboxuser)*10),'e'
                     WRITE(cl_boxes_user(k+nboxuser)(6:10),'(I4.4,A1)') &
                        & NINT(areas(2,k+nboxuser)*10),'e'
                     IF (areas(3,k+nboxuser)<0) THEN
                        WRITE(cl_boxes_user(k+nboxuser)(11:15),'(I4.4,A1)') &
                           & -NINT(areas(3,k+nboxuser)*10),'s'
                     ELSE
                        WRITE(cl_boxes_user(k+nboxuser)(11:15),'(I4.4,A1)') &
                           & NINT(areas(3,k+nboxuser)*10),'n'
                     ENDIF
                     IF (areas(4,k+nboxuser)<0) THEN
                        WRITE(cl_boxes_user(k+nboxuser)(16:20),'(I4.4,A1)') &
                           & -NINT(areas(4,k+nboxuser)*10),'s'
                     ELSE
                        WRITE(cl_boxes_user(k+nboxuser)(16:20),'(I4.4,A1)') &
                           & NINT(areas(4,k+nboxuser)*10),'n'
                     ENDIF
                  ENDDO
               ENDDO
               nboxuser=nboxuser+nlat*nlon
            ENDIF
         END DO
200      CONTINUE
         CLOSE(20)
      ELSE
         nboxuser=nbox
         ALLOCATE(cl_boxes_user(nboxuser))
         ALLOCATE(areas(4,nboxuser))
         cl_boxes_user(:)=cl_boxes(:)
         DO i=1,nbox
            CALL coord_area( cl_boxes_user(i), areas(:,i) )
         ENDDO
      ENDIF
      DO i=1,nboxuser
         WRITE(*,'(A,4F12.2)')cl_boxes_user(i),areas(:,i)
         DO j=i+1,nboxuser
            IF (TRIM(cl_boxes_user(i))==TRIM(cl_boxes_user(j))) THEN
               WRITE(*,*)'coord_user_init: dublicate boxes'
               CALL abort
            ENDIF
         ENDDO
      ENDDO

   END SUBROUTINE coord_user_init

   SUBROUTINE coord_area_user( reg, area, ldfail )
      !-----------------------------------------------------------------------
      !
      !                       ROUTINE coord_area_user
      !                     ****************************
      !
      !  Purpose :
      !  -------
      !    Get coordinate of different regions
      !
      !   Modifications :
      !   -------------
      IMPLICIT NONE
      !----------------------------------------------------------------------
      ! local declarations
      !----------------------------------------------------------------------
      !
      CHARACTER(len=20), INTENT(inout) :: reg
      REAL, DIMENSION(4), INTENT(out) :: area
      LOGICAL, OPTIONAL, INTENT(out) :: ldfail
      INTEGER :: i
      LOGICAL :: lnotfound
      !
      reg=TRIM(reg)

      lnotfound=.TRUE.
      DO i=1,nboxuser
         IF (reg==TRIM(cl_boxes_user(i))) THEN
            area(:)=areas(:,i)
            lnotfound=.FALSE.
            EXIT
         ENDIF
      ENDDO
      IF (PRESENT(ldfail)) THEN
         ldfail=lnotfound
      ELSE
         IF (lnotfound) THEN
            WRITE(*,*)'coord_area_user: area not found'
            CALL abort
         ENDIF
      ENDIF

   END SUBROUTINE coord_area_user

END MODULE coords
