MODULE sedsfc
   !!======================================================================
   !!              ***  MODULE  sedsfc  ***
   !!    Sediment : Data at sediment surface
   !!=====================================================================
#if defined key_sed && ! defined key_sed_off
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedarr
   USE seddta

   PUBLIC sed_sfc

CONTAINS

   SUBROUTINE sed_sfc( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_sfc ***
      !!
      !! ** Purpose :  Give data from sediment model to tracer model
      !!
      !!
      !!   History :
      !!        !  06-04 (C. Ethe)  Orginal code
      !!----------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  kt              ! time step

      ! * local variables
      INTEGER :: ji, jj, ikt     ! dummy loop indices

      !------------------------------------------------------------------------

      IF( kt == nitsed000 ) THEN
         WRITE(numsed,*) ' sed_sfc : Give data from sediment model to tracer model  '
         WRITE(numsed,*) ' '
      ENDIF

      ! reading variables

      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,1), iarroce(1:jpoce), pwcp(1:jpoce,1,jwalk) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,2), iarroce(1:jpoce), pwcp(1:jpoce,1,jwdic) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,3), iarroce(1:jpoce), pwcp(1:jpoce,1,jwno3) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,4), iarroce(1:jpoce), pwcp(1:jpoce,1,jwpo4) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,5), iarroce(1:jpoce), pwcp(1:jpoce,1,jwoxy) )
      CALL unpack_arr ( jpoce, trc_data(1:jpi,1:jpj,6), iarroce(1:jpoce), pwcp(1:jpoce,1,jwsil) )


      DO jj = 1,jpj
         DO ji = 1, jpi
            ikt = mbkt(ji,jj)
            IF ( tmask(ji,jj,ikt) == 1 ) THEN
               trn(ji,jj,ikt,jptal) = trc_data(ji,jj,1)
               trn(ji,jj,ikt,jpdic) = trc_data(ji,jj,2)
               trn(ji,jj,ikt,jpno3) = trc_data(ji,jj,3) * 7.6
               trn(ji,jj,ikt,jppo4) = trc_data(ji,jj,4) * 122.
               trn(ji,jj,ikt,jpoxy) = trc_data(ji,jj,5)
               trn(ji,jj,ikt,jpsil) = trc_data(ji,jj,6)
            ENDIF
         ENDDO
      ENDDO

   END SUBROUTINE sed_sfc

#else
   !!======================================================================
   !! MODULE sedsfc  :   Dummy module 
   !!======================================================================
CONTAINS
   SUBROUTINE sed_sfc ( kt )
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'sed_sfc: You should not have seen this print! error?', kt
   END SUBROUTINE sed_sfc
#endif

END MODULE sedsfc
