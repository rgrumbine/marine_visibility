C
      SUBROUTINE FINDNH(NOFLD,IDENT,IFST,OUTF,INO)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    FINDNH      FIND GFS AND ANL FIELDS                    
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 2003-03-13
C                                                                       
C ABSTRACT: GIVEN THE FIELD NUMBER, THIS SUBROUTINE WILL RETURN THE     
C   DESIRED 361X181 NORTHERN HEMISPHERIC GFS OR GDAS FIELD.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C 2003-03-13  L. D. BURROUGHS
C                                                                       
C USAGE:    CALL FINDNH(NOFLD,IDENT,IFCST,OUTF,INO)
C   INPUT ARGUMENT LIST:                                                
C     NOFLD    - NUMBER OF FIELD TO BE RETRIEVED.  NUMBERS RANGE FROM   
C                01 - 10  (I*4)  CODE IS GIVEN BELOW:                  
C                01 = 1000 MB TEMPERATURE DEG K - R*4
C                02 = 1000 MB RELATIVE HUMIDITY % - R*4
C                03 = 1000 MB CLOUD WATER MIXING RATIO KG/KG - R*4
C                04 = PRECIPITATION RATE KG/M^2/S - R*4
C                05 = CONVECTIVE PRECIPITATION RATE KG/M^2/S - R*4
C                06 = PRECIPITABLE WATER KG/M^2 - R*4
C                07 = Categorical rain (y=1/n=0) - I*4
C                08 = Categorical freezing rain (y=1/n=0) - I*4
C                09 = Categorical ice pellets (y=1/n=0) - I*4
C                10 = Categorical snow (y=1/n=0) - I*4
C                11 = SURFACE PRESSURE PA - I*4
C                12 = SURFACE TEMPERATURE - I*4
C     IDENT    - POSITION OF FIELD IN FIELD IDENTIFICATION MATRIX I*4
C     IFST     - THE FORECAST TAU AT 3-H INTERVALS FROM 00 - 168 HOURS  
C                (I*4)                                                  
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     OUTF(,)  - OUTPUT MATRIX FOR GIVEN FIELD RETRIEVED (R*4)          
C     INO      - ERROR CODE = 0 FOR SUCCESSFUL RETRIEVE                 
C                           = 9 FOR UNSUCCESSFUL RETRIEVE               
C                                                                       
C   INPUT FILES:
C      SEE MAIN PROGRAM DOC BLOCK
C                                                                       
C   OUTPUT FILES:                                                       
C      NONE
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - NONE
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - GETGB   GETENV  BAOPENR
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE                                                 
C                                                                       
C REMARKS: IF ANYTHING GOES WRONG, A DIAGNOSTIC MESSAGE IS WRITTEN TO   
C   FT28F001 AND THE OUTPUT FIELDS ARE GIVEN VALUES OF -9999. AT EVERY     
C   GRID POINT.                                                         
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - IGRD( )    = GRID IDENTIFICATION MATRIX I*4
C                                                                       
C       INTERNAL - OUT( , )   = WORK MATRIX R*4
C                  FLD( )     = OUTPUT MATRIX FROM GETGB R*4
C                  PDS( )     = MATRIX FOR GRIB DESCRIPTORS I*4
C                  GDS( )     = MATRIX FOR GRID DESCRIPTORS I*4
C                  JPDS       = NUMBER OF GRIB DESCRIPTORS I*4
C                  JGDS       = NUMBER OR GRID DESCRIPTORS I*4
C                  IDX        = NUMBER USED TO DETERMINE GRIB FILE NUMBER I*4
C                  LUGB       = FILE NUMBER FOR GRIB FILES
C                  LUGI       = FILE NUMBER FOR GRIB INDEX FILES
C                  JF         = NUMBER OF GRIDS
C                  ENVVAR     = CHARACTER STRING IDENTIFIER FOR OUTPUT FILE -
C                               C*11
C                  IRET1      = RETURN CODE FOR BAOPENR - I*4
C                               0 - GOOD RETURN FOR FILEB
C                               >0- BAD RETURN FOR FILEB
C                  IRET2      = RETURN CODE FOR BAOPENR - I*4
C                               0 - GOOD RETURN FOR FILEI
C                               >0- BAD RETURN FOR FILEI
C                  FILEB      = DESCRIPTOR STRING FOR GRIB FILE - C*80
C                  FILEI      = DESCRIPTOR STRING FOR GRIB INDEX FILE - C*80
C                  IRET       = RETURN CODE FOR GETGB - I*4
C                               0    - GOOD RETURN
C                               NE 0 - BAD RETURN 
C                                                                       
C       OUTPUT   - NONE                                                 
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$
      IMPLICIT none
      SAVE
! Arguments
      INTEGER NOFLD, IDENT, IFST, INO
      REAL*4 OUTF(361,181)

! Local
      INTEGER jf
      PARAMETER(JF=360*181)
      INTEGER*4 PDS,GDS
      INTEGER*4 JPDS(25),JGDS(22)
      INTEGER*4 KPDS(25),KGDS(22)
      REAL*4 FLD(JF),OUT(360,181)
      CHARACTER*80 FILEB,FILEI
      CHARACTER*11 ENVVAR
      LOGICAL LB(JF)
C                                                                       
      INTEGER IGRD
      COMMON/IDNT/IGRD(42)
C
      EQUIVALENCE(OUT(1,1),FLD(1))
      INTEGER iret, k, kf, j, kk
      INTEGER idx, lugb, lugi, jb, iret1, iret2
C
      INO = 0                                                           
      DO PDS=1,25
         JPDS(PDS)=-1
      ENDDO
      DO GDS=1,22
         JGDS(GDS)=-1
      ENDDO
C                                                                       
      WRITE(98,1020)IFST                                               
 1020 FORMAT(1H0,'IFST',I4)                                            
C                                                                       
C          DETERMINE ANALYSIS OR MODEL FIELD ID                         
C
      IDX=IFST/3                                                  
      LUGB=IDX+11
      LUGI=IDX+111
      JB=0
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)')LUGB
      CALL GETENV(ENVVAR,FILEB)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:11),FMT='(I3)')LUGI
      CALL GETENV(ENVVAR,FILEI)
      CALL BAOPENR(LUGB,FILEB,IRET1)
      CALL BAOPENR(LUGI,FILEI,IRET2)
      IF((IRET1+IRET2).NE.0)THEN
         WRITE(98,FMT='("Error opening GRIB data file or index file.")
     1         ')
         INO=9
         GO TO 1100
      ENDIF
C                                                                       
C          SET IDENT FOR CORRECT FORECAST                               
C          GET SELECT FIELDS
CXXXXNS----------------------------------------------------------------E
      JPDS(5)=IGRD(IDENT)
      JPDS(6)=IGRD(IDENT+1)
      JPDS(7)=IGRD(IDENT+2)
      PRINT *,'IDENT, JPDS, IGRD ',IDENT,JPDS(5),IGRD(IDENT),JPDS(6),
     1        IGRD(IDENT+1),JPDS(7),IGRD(IDENT+2)
!      PRINT *,'CALL getgb'
      CALL GETGB(LUGB,LUGI,JF,JB,JPDS,JGDS,KF,K,KPDS,KGDS,LB,FLD,IRET)
      IF(IRET.NE.0)THEN
         INO=9
         WRITE(98,1065)IRET,INO
 1065    FORMAT(1X,' IRET =',I5,' INO =',I2)
         GO TO 1100
      END IF
      WRITE(98,1066)KPDS,KF,KGDS
 1066 FORMAT(2(/,2X,'PDS =',13I7),2(/,2X,' GDS =',11I7))
!      IF (IDENT .EQ. 4 .OR. IDENT .EQ. 5 .OR. IDENT .EQ. 7 .OR.
!     1    IDENT .EQ. 8 .OR. IDENT .EQ. 9 .OR. IDENT .EQ. 10) THEN
!        PRINT *,' valid parm? max min ',IDENT, MAXVAL(FLD),
!     1                                                MINVAL(FLD)
!      ENDIF
C
      DO J=1,181
         DO KK=1,360
C           OUTF(KK,182-J)=OUT(KK,J)
            OUTF(KK,J)=OUT(KK,J)
         ENDDO
C
C          DUPLICATE GREENWICH
C
         OUTF(361,J)=OUTF(1,J)
      ENDDO
C
 1100 CONTINUE
      CALL BACLOSE(LUGB,IRET)
      CALL BACLOSE(LUGI,IRET)
      RETURN                                                            
      END                                                               
