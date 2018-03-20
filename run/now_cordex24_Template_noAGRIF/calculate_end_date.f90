program end_date

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain
!
! input : year, month, day, duration
!
! output : yearf, monthf, dayf (final if run length = duration)
!          durcorr (corrected duration to finish at the end of current year)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE

INTEGER :: an0, mois0, jour0, duration ! duration in nb days

INTEGER :: an, mois, jour, durleft, durcorr

INTEGER, DIMENSION(12) :: ndays_month

LOGICAL :: tocken

ndays_month = (/31,28,31,30,31,30,31,31,30,31,30,31/)

open(11,file='start_date_duration')

read(11,*) an0, mois0, jour0, duration

tocken = .true.

durleft = duration
durcorr = duration
an      = an0
mois    = mois0
jour    = jour0

do while ( durleft .gt. 0 )

  if ( MOD(an,4) .eq. 0 ) then
    ndays_month(2) = 29
  else
    ndays_month(2) = 28
  endif

  if ( (jour+durleft) .gt. ndays_month(mois) ) then

    if ( mois .eq. 12 ) then

      durleft = durleft - ( ndays_month(mois) - jour + 1 )
      if ( tocken ) then
        durcorr = duration - durleft
        tocken = .false.
      endif
      an      = an + 1
      mois    = 1
      jour    = 1

    else

      durleft = durleft - ( ndays_month(mois) - jour + 1 )
      mois    = mois + 1
      jour    = 1

    endif

  else

    jour    = jour + durleft
    durleft = 0

  endif

enddo

write(*,*) an, mois, jour, durcorr

close(11)

end program end_date
