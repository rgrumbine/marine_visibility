C                                                                       
      SUBROUTINE WRITEF(C84S,IDX,IGRIB,IERR)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    WRITEF       WRITES OUTPUT TO DISK AND ARCHIVE         
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 88-07-29             
C                                                                       
C ABSTRACT: WRITES OUTPUT TO DISK AND ARCHIVE IN GRIB FORMAT.           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   88-07-29  L. D. BURROUGHS                                           
C   88-08-31  L. D. BURROUGHS - REMOVED SOME UNNECESSARY CODE           
C   96-02-15  L. D. BURROUGHS - CHANGED TO PUT OUTPUT FIELDS IN GRIB
C   96-06-19  L. D. BURROUGHS - CHANGED FOR USE IN ICE ACCRETION PGM
C                                                                       
C USAGE:    CALL WRITEI(C84S,IDX,IGRIB,IERR)
C                                                                       
C   INPUT ARGUMENT LIST:
C     IFT06    - FILE NAME FOR PRINT OUT
C     C84      - VISUAL RANGE MATRIX FOR 1X1 N HEMISPHERIC GRID
C     IDX      - INDEX PASSED TO WRITE TO DETERMINE FILE BEING       
C                WRITTEN.
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     IERR     - ERROR CODE PASSED FROM WRITE TO DETERMINE WHAT IS      
C                WRITTEN TO THE PRINTER. I*4                            
C                = 9 - FIELDS UNABLE TO BE WRITTEN TO OUTPUT FILE       
C                                                                       
C   INPUT FILES:                                                        
C     fort.06  - PRINT OUT FILE
C                                                                       
C   OUTPUT FILES:                                                       
C     fort.68  - GRIBBED NH 1X1 DEG VISUAL RANGE DATA AT 3-H INTERVALS
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE     - GRBFOG
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - NONE                                                 
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE                                                 
C                                                                       
C REMARKS: NONE                                                         
C                                                                       
C     VARIABLES:                                                        
C       INPUT    - C84( , )   = N HEMIS 1X1 VISUAL RANGE MATRIX M
C                  IFT06      = FILE NAME FOR PRINT OUT
C                  IDATE      = DATE TIME GROUP (YYMMDDCC): YEAR, MONTH,
C                               DAY AND CYCLE TIME - I*4                
C                                                                       
C       INTERNAL - NONE
C                                                                       
C       OUTPUT   - GRIB( )    = N HEMIS 1X1 GRIBBED VISUAL RANGE
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: CF90
C   MACHINE:  IBM SP
C
C$$$
      IMPLICIT none !RG

      SAVE

      INTEGER IFT68, JS, IGBS, IPDS, IMXBIT, IMS, JMS
      PARAMETER (IFT68=68,JS=360*181,IGBS=260640,IPDS=28,IMXBIT=0,
     1           IMS=360,JMS=181)
C
      INTEGER*4 IDATE,ICY,IFC,IY,IM,ID
      INTEGER*4 IGRIB
C
      REAL*4 C84S(361,181),TMP(360,181)
      REAL*4 FGRB(JS)
C
      CHARACTER ZERO*1,GBSI(IGBS)*4
C
      COMMON/FDT/IDATE,ICY,IFC,IY,IM,ID
C
      EQUIVALENCE (FGRB(1),TMP(1,1))
C
      INTEGER i, j, ierr, lgrib, idx
      REAL xlat1, xlon1
C                                                                       
C          PREPARE VISUAL RANGE IN M TO BE CONVERTED TO GRIB
C 
      IERR=0
      XLAT1=0.
      XLON1=0.
      DO J=1,181
         DO I=1,360
            TMP(I,J)=C84S(I,J)
         END DO
      END DO
C                                                                       
C          GRIB 1X1 N HEMISPHERE VISUAL RANGE
C
      WRITE(98,5000)IY,IM,ID,ICY,IFC
 5000 FORMAT(/1X,5I5/)
      CALL GRBFOG(IFT68,FGRB,IDX,IMS,JMS,IMXBIT,IPDS,IY,IM,ID,ICY,
     1            IFC,JS,IGBS,GBSI,LGRIB,IGRIB,IERR)
C
      WRITE(98,1010)IERR,LGRIB,IGRIB,IDX
 1010 FORMAT(1H0,4I8/)
      WRITE(98,1500)IDATE,IERR,IDX
 1500 FORMAT(1H0,3I8/)
      RETURN
      END
