      PROGRAM GBLVSBY
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: GBLVSBY       GFS PRODUCTION GLOBAL VISIBILITY PROGRAM
C   PRGMMR: BURROUGHS        ORG: NP21        DATE: 2003-03-13
C                                                                       
C ABSTRACT: THIS PROGRAM TAKES DATA FROM THE GFS, COMPUTES SEVERAL
C   PARAMETERS FROM BASIC MODEL OUTPUT, COMPUTES FORECASTS OF VISIBILITY
C   FOR THE GLOBE, AND OUTPUTS THE FORECAST FIELD IN GRIB FORMAT FOR
C   ARCHIVING AND FURTHER USE IN THE JOB STREAM.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C 2003-03-13  L. D. BURROUGHS                                           
C                                                                       
C USAGE:                                                                
C   INPUT FILES:                                                        
C      fort.12 - AVN 03-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.13 - AVN 06-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.14 - AVN 09-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.15 - AVN 12-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.16 - AVN 15-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.17 - AVN 18-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.18 - AVN 21-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.19 - AVN 24-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.20 - AVN 27-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.21 - AVN 30-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.22 - AVN 33-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.23 - AVN 36-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.24 - AVN 39-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.25 - AVN 42-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.26 - AVN 45-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.27 - AVN 48-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.28 - AVN 51-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.29 - AVN 54-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.30 - AVN 57-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.31 - AVN 60-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.32 - AVN 63-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.33 - AVN 66-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.34 - AVN 69-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.35 - AVN 72-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.36 - AVN 75-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.37 - AVN 78-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.38 - AVN 81-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.39 - AVN 84-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.40 - AVN 87-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.41 - AVN 90-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.42 - AVN 93-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.43 - AVN 96-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.44 - AVN 99-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.45 - AVN 102-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.46 - AVN 105-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.47 - AVN 108-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.48 - AVN 111-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.49 - AVN 114-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.50 - AVN 117-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.51 - AVN 120-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.52 - AVN 123-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.53 - AVN 126-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.54 - AVN 129-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.55 - AVN 132-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.56 - AVN 135-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.57 - AVN 138-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.58 - AVN 141-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.59 - AVN 144-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.60 - AVN 147-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.61 - AVN 150-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.62 - AVN 153-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.63 - AVN 156-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.64 - AVN 159-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.65 - AVN 162-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.66 - AVN 165-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.67 - AVN 168-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.112 - AVN INDEX 03-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.113 - AVN INDEX 06-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.114 - AVN INDEX 09-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.115 - AVN INDEX 12-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.116 - AVN INDEX 15-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.117 - AVN INDEX 18-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.118 - AVN INDEX 21-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.119 - AVN INDEX 24-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.120 - AVN INDEX 27-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.121 - AVN INDEX 30-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.122 - AVN INDEX 33-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.123 - AVN INDEX 36-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.124 - AVN INDEX 39-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.125 - AVN INDEX 42-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.126 - AVN INDEX 45-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.127 - AVN INDEX 48-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.128 - AVN INDEX 51-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.129 - AVN INDEX 54-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.130 - AVN INDEX 57-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.131 - AVN INDEX 60-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.132 - AVN INDEX 63-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.133 - AVN INDEX 66-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.134 - AVN INDEX 69-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.135 - AVN INDEX 72-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.136 - AVN INDEX 75-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.137 - AVN INDEX 78-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.138 - AVN INDEX 81-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.139 - AVN INDEX 84-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.140 - AVN INDEX 87-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.141 - AVN INDEX 90-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.142 - AVN INDEX 93-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.143 - AVN INDEX 96-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.144 - AVN INDEX 99-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.145 - AVN INDEX 102-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.146 - AVN INDEX 105-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.147 - AVN INDEX 108-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.148 - AVN INDEX 111-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.149 - AVN INDEX 114-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.150 - AVN INDEX 117-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.151 - AVN INDEX 120-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.152 - AVN INDEX 123-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.153 - AVN INDEX 126-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.154 - AVN INDEX 129-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.155 - AVN INDEX 132-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.156 - AVN INDEX 135-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.157 - AVN INDEX 138-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.158 - AVN INDEX 141-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.159 - AVN INDEX 144-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.160 - AVN INDEX 147-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.161 - AVN INDEX 150-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.162 - AVN INDEX 153-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.163 - AVN INDEX 156-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.164 - AVN INDEX 159-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.165 - AVN INDEX 162-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.166 - AVN INDEX 165-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.167 - AVN INDEX 168-H FORECASTS 00/06/12/18 UTC CYCLES
C      fort.90 - DATE FILE - YYYYMMDD
C      fort.91 - CYCLE FILE - CC: 00/06/12/18
C      fort.92 - SMOOTHER ID NUMBER (CURRENTLY DISABLED BECAUSE SMOOTHER
C                HARDWIRED INTERNALLY)
C      fort.93 - RTG_SST 1.0X1.0 DEG LON/LAT GRID (360 X 180 GRID PTS)
C                                                                       
C   OUTPUT FILES:
C      fort.98 - DIAGNOSTIC MESSAGE FILE
C      fort.06 - PRINTOUT                                               
C      fort.68 - GRIBBED OUTPUT FILE
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - FIND   transl EPOTTM CALVIS GRBFOG WRITEF BLOCKDATA
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - W3TAGB  W3TAGE  GETENV  BAOPENW GETGB  BAWRITE
C                                                                       
C       IMSL     - NONE
C                                                                       
C       SPECIAL  - LOG
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C     COND =   8 - ERROR: ERROR in subroutine getgb                     
C     COND =   9 - ERROR: ERROR in subroutine grbfog                    
C                                                                       
C REMARKS: IF ANYTHING GOES WRONG, A DIAGNOSTIC MESSAGE IS WRITTEN TO   
C   FT98F001 AND THE OUTPUT FIELDS ARE GIVEN VALUES OF -9999. AT EVERY  
C   GRID POINT AND SENT TO THE OUTPUT FILE IN GRIB FORMAT.
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - NONE                                                 
C                                                                       
C       INTERNAL - NONE                                                 
C                                                                       
C       OUTPUT   - NONE
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90
C   MACHINE:  IBM SP
C
C$$$
      SAVE
      CALL W3TAGB('GBLVSBY',2003,0272,0290,'NP21   ') 
C
      WRITE(98,1000)
 1000 FORMAT(1H ,'START MAIN PROGRAM')
C
      CALL FIND(IERR,KERR)
C
      IF(IERR.EQ.9) THEN
         ICODE=9
         WRITE(98,1100)IERR,ICODE
 1100    FORMAT(1H0,'JOB ENDED ABNORMALLY. ERROR IN WRITEF',2I3)
         CALL W3TAGE('GBLVSBY') 
         STOP 9
      ELSE IF(KERR.EQ.9) THEN
         ICODE=8
         WRITE(98,1105)KERR,ICODE
 1105    FORMAT(1H0,'JOB ENDED ABNORMALLY. ERROR IN GETGB',2I3)
         CALL W3TAGE('GBLVSBY') 
         STOP 8
      ELSE
         ICODE=0
         WRITE(98,1120)IERR,KERR,ICODE
 1120    FORMAT(1H0,'JOB ENDED NORMALLY',3I3)
         CALL W3TAGE('GBLVSBY')
         STOP
      END IF
      END PROGRAM GBLVSBY
C
      SUBROUTINE FIND(IERR,KERR)                                        
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    FIND        FIND AVN AND ANL FIELDS                    
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 2003-03-13
C                                                                       
C ABSTRACT: THIS SUBROUTINE FINDS ALL THE INPUT FILES USED TO COMPUTE   
C   THE Stoelinga and Warner visual range algoritm as modified for the  
C   GFS system.  IT CALLS THE SUBROUTINES NECESSARY TO MAKE THE     
C   VARIOUS FORECAST COMPUTATIONS AND INITIALIZE THE OUTPUT FIELDS.     
C   IT ALSO CALLS THE WRITE ROUTINE.                                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C 2003-03-13  L. D. BURROUGHS
C                                                                       
C USAGE:    CALL FIND(IERR,KERR)                                        
C   INPUT ARGUMENT LIST:                                                
C     NONE                                                              
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     IERR     - ERROR CODE IN WRITEF                                   
C                0 - GOOD WRITE
C                9 - ERROR IN GRBFOG
C     KERR     - ERROR CODE - I*4                                       
C                0 - NO FIELDS MISSING.                                 
C                9 - FIELDS MISSING.                                    
C                                                                       
C   INPUT FILES:                                                        
C     SEE MAIN DOC BLOCK
C                                                                       
C   OUTPUT FILES:                                                       
C     SEE MAIN DOC BLOCK
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - transl  MIXR    CALVIS  WRITEF  SMTH25
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - GETENV  BAOPENW
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - NONE                                                 
C                                                                       
C REMARKS: IF ANYTHING GOES WRONG, A DIAGNOSTIC MESSAGE IS WRITTEN TO   
C   FT92F001 AND THE OUTPUT FIELDS ARE GIVEN VALUES OF -9999. AT EVERY     
C   GRID POINT AND SENT TO THE OUTPUT FILE IN GRIB FORMAT.
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - IYYYY      = 4 DIGIT YEAR - I*4
C                  IM         = MONTH - I*4
C                  ID         = DAY - I*4
C                  IH         = MODEL CYCLE TIME 00/06/12/18 UTC - I*4
C                  SF         = SMOOTHER NUMBER - R*4
C                               0. - NO SMOOTHING
C                               1. -  5 PT SMOOTHER ALONG I AND J AXES
C                               2. -  9 PT SMOOTHER ALONG I AND J AXES
C                               3. -  9 PT SMOOTHER WITHIN 1.4 PTS OF I,J
C                               4. - 13 PT SMOOTHER ALONG I AND J AXES
C                               5. - 17 PT SMOOTHER ALONG I AND J AXES
C                               6. - 25 PT SMOOTHER WITHIN 2.4 PTS OF I,J
C                               7. -  9 PT WEIGHTED SMOOTHER WITHIN 1.4 PTS
C                                          OF I,J
C                  C01( , )   = 2 M TEMPERATURE DEG K - R*4
C                  C02( , )   = 2 M RELATIVE HUMIDITY % - R*4
C                  C03( , )   = 1000 MB CLOUD WATER MIXING RATIO KG/KG - R*4
C                  C04( , )   = PRECIPITATION RATE KG/M^2/S - R*4
C                  C05( , )   = CONVECTIVE PRECIPITATION RATE KG/M^2/S - R*4
C                  C06( , )   = PRECIPITABLE WATER KG/M^2 - R*4
C                  C07( , )   = Categorical rain (y=1/n=0) - R*4
C                  C08( , )   = Categorical freezing rain (y=1/n=0) - R*4
C                  C09( , )   = Categorical ice pellets (y=1/n=0) - R*4
C                  C10( , )   = Categorical snow (y=1/n=0) - R*4
C                  C11( , )   = SURFACE PRESSURE PA - R*4
C                  C12( , )   = SURFACE temperature - R*4
C                  C13( , )   = SURFACE ice - R*4
C                  C14( , )   = LAND/SEA TAG - R*4
C                  C15( , )   = RTG_SST - R*4
C                                                                       
C       INTERNAL - LUG1       = OUTPUT FILE NUMBER - I*4
C                  ENVVAR     = CHARACTER STRING IDENTIFIER FOR OUTPUT FILE -
C                               C*11
C                  FILE1      = DESCRIPTOR STRING FOR GRIB FILE - C*80
C                  IRET1      = RETURN CODE FOR BAOPENW - I*4
C                               0 - GOOD RETURN
C                               >0- BAD RETURN
C                  IDATE      = NCEP 2 DIGIT DATE WORD - I*4
C                  PP         = PRESSURE = 1000 MB - R*4
C                  IFCST      = FCST TAU AT 3-H INTRVLS FM 00-168 - I*4 
C                  IONA       = 0 - IF FIELD IS FOUND BY FINDNH         
C                               9 - IF FIELD IS NOT FOUND OR SOME OTHER 
C                                   ERROR OCCURS - I*4
C                  NERR       = 0 - IF ALL FIELDS ARE FOUND - I*4
C                               9 - IF FIELDS ARE MISSING
C                  QC         = CLOUD WATER MIXING RATION (KG/KG) - R*4
C                  TA         = AIR TEMPERATURE (DEG K) - R*4
C                  RH         = RELATIVE HUMIDITY (%) - R*4
C                  PRATE      = SYNOPTIC PRECIPITATION RATE (KG/M**2/S) R*4
C                  CPRAT      = CONVECTIVE PRECIP RATE (KG/M**2/S) R*4
C                  PW         = LOCAL PRECIPITABLE WATER (KG/M**2) R*4
C                  CRAIN      = Categorical rain (y=1/n=0) - I*4
C                  CFRZR      = Categorical freezing rain (y=1/n=0) - I*4
C                  CICEP      = Categorical ice pellets (y=1/n=0) - I*4
C                  CSNOW      = Categorical snow (y=1/n=0) - I*4
C                  W          = WATER VAPOR MIXING RATIO (KG/KG) - R*4
C                  QV         = WATER VAPOR MIXING RATIO (KG/KG) - R*4
C                  QR         = PRECIPITATION MIXING RATIO (KG/KG) - R*4
C                  VIS        = VISIBILITY (M) - R*4
C                  C84( , )   = UNSMOOTHED VISIBILITY MATRIX (M) - R*4
C
C       OUTPUT   - IY         = YEAR - 2000 - I*4                       
C                  IM         = MONTH - I*4                             
C                  ID         = DAY - I*4                               
C                  ICY        = CYCLE (00/06/12/18 UTC) - I*4
C                  IFC        = FORECAST CENTURY I*4
C                  C84S( , )  = SMOOTHED VISIBILITY MATRIX (M) - R*4
C                                                                       
C REMARKS: THE SMOOTHER IS CURRENTLY HARDWIRED TO 7., BUT CAN BE CHANGED
C          TO BE READ IN AS AN INPUT PARAMETER.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90
C   MACHINE:  IBM SP
CNNNNXB----------------------------------------------------------------ENNNN
C$$$
      IMPLICIT none

      SAVE
      INTEGER imm, jm, lupb
      PARAMETER(IMM=360,JM=180,LUPB=93)

      REAL*4 C01(361,181),C02(361,181),C03(361,181),C04(361,181),
     1       C05(361,181),C06(361,181),C07(361,181),C08(361,181),
     2       C09(361,181),C10(361,181),C11(361,181),C12(361,181),
     3       C13(361,181),C14(361,181),C15(361,181),ANAL(IMM,JM)
      REAL*4 C84(361,181),C84S(361,181)
      REAL*4 CRAIN,CFRZR,CICEP,CSNOW
C                                                                       
      INTEGER*4 IDATE,ICY,IFC,IY,IM,ID
C                                                                       
      CHARACTER*80 FILE1
      CHARACTER*11 ENVVAR
C                                                                       
      COMMON/FDT/IDATE,ICY,IFC,IY,IM,ID
! Local
      INTEGER i, j, ii, jj, kk
      INTEGER iyyyy, ih, isf, itm
      INTEGER lug1, iret1, ierr, nerr, kerr, igrib, ifcst, iona
      INTEGER iyrstmp, imstmp, idstmp
      REAL sf, tm, qc, ta, rh, prate, cprat, pwat, pp, ts, w
      REAL qc2, qv, pw, qr, vis 
C
      READ(90,1000)IYYYY,IM,ID
 1000 FORMAT(I4,2I2)
      READ(91,1005)IH
 1005 FORMAT(I2)
C
      READ(92,1010)ISF,ITM
 1010 FORMAT(I2,1X,I3)
      WRITE(98,5500)ISF,ITM
 5500 FORMAT(' ',I2,1X,I3)

      SF=ISF
      TM=ITM
      LUG1=68
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)')LUG1
      
      CALL GETENV(ENVVAR,FILE1)

      CALL BAOPENW(LUG1,FILE1,IRET1)
      IF((IRET1).NE.0)THEN
         WRITE(98,FMT='("Error opening GRIB data file for 1x1 03-h."
     1)')
      ENDIF
      ICY=IH
      IFC=(IYYYY/100)*100
      IY=IYYYY-IFC
      IDATE=1000000*IY+10000*IM+100*ID+ICY
C
      NERR=0
      KERR=0
      IGRIB=0

      IFCST=10
      OPEN(UNIT=IFCST,FILE="fort.10", FORM="UNFORMATTED",STATUS="OLD")
      CALL transl(13,37,IFCST,C13,IONA)
      IF(IONA.NE.0)NERR=9                                            
      CALL transl(14,40,IFCST,C14,IONA)
      IF(IONA.NE.0)NERR=9
      CLOSE(IFCST)

      CALL RDGRBSST(LUPB,IMM,JM,ANAL,IYRSTMP,IMSTMP,IDSTMP,IONA)

      IF(IONA.NE.0)NERR=9
      DO J=1,JM
         IF(J.EQ.JM)CYCLE
         DO I=1,IMM
            IF(I.EQ.IMM)CYCLE
            C15(I+1,180-J+1)=(ANAL(I+1,J+1)+ANAL(I+1,J)+ANAL(I,J+1)+
     1                        ANAL(I,J))/4.
            C15(I+1,1)=0.
            C15(I+1,181)=0.
         ENDDO
         C15(1,180-J+1)=(ANAL(1,J+1)+ANAL(1,J)+ANAL(IMM,J+1)+
     1                   ANAL(IMM,J))/4.
         C15(361,180-J+1)=C15(1,180-J+1)
      ENDDO
C                                                                       
      DO 1250 KK=1,56                                                    
C
C          INITIALIZE OUTPUT FIELD C84
C
         DO J=1,181
            DO I=1,361
               C84(I,J)=20000.
               C84S(I,J)=20000.
            ENDDO
         ENDDO
         IFCST=KK+11
         OPEN(IFCST,FORM="UNFORMATTED",STATUS="OLD")
C                                                                       
C          FIND GFS FIELDS                                      
C                                                                       
         CALL transl(01,01,IFCST,C01,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(02,04,IFCST,C02,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(03,07,IFCST,C03,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(04,10,IFCST,C04,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(05,13,IFCST,C05,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(06,16,IFCST,C06,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(07,19,IFCST,C07,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(08,22,IFCST,C08,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(09,25,IFCST,C09,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(10,28,IFCST,C10,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(11,31,IFCST,C11,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CALL transl(12,34,IFCST,C12,IONA)
         IF(IONA.NE.0)NERR=9                                            
         CLOSE(IFCST)
C
         IF(NERR.EQ.9)GO TO 1030
         WRITE(98,1020) IFCST                                           
 1020    FORMAT(1H ,'END OF FIND FOR IFCST ',I3)
         GO TO 1050                                                     
C                                                                       
 1030    WRITE(98,1040)                                                 
 1040    FORMAT(1H ,' FIELDS ARE MISSING. -9999. WRITTEN')
         KERR=NERR                                                      
         CALL WRITEF(C84S,KK,IGRIB,IERR)
         GO TO 1240                                                     
C                                                                       
C          COMPUTE VISUAL RANGE FORECASTS                
C
 1050    DO JJ=1,181
            DO II=1,361
               QC=C03(II,JJ)
               TA=C01(II,JJ)
               RH=C02(II,JJ)
               PRATE=C04(II,JJ)
               CPRAT=C05(II,JJ)
               PWAT=C06(II,JJ)
               CRAIN=C07(II,JJ)
               CFRZR=C08(II,JJ)
               CICEP=C09(II,JJ)
               CSNOW=C10(II,JJ)
               PP=C11(II,JJ)/100.
               IF(C13(II,JJ).GE.0.5.OR.C14(II,JJ).GE.0.5)THEN
                  TS=C12(II,JJ)
               ELSE
                  TS=C15(II,JJ)
               ENDIF
C
C          FIND WATER VAPOR MIXING RATIO (W IS WRT WATER >= 273.15 AND WRT
C            ICE < 273.15)
C
               CALL MIXR(PP,TS,TA,RH,W,QC2)
               QV=W
               IF(QC2.GT.QC.AND.(C13(II,JJ).LT.0.5.AND.C14(II,JJ).LT.
     1            0.5))THEN
                  QC=QC2
                  RH=100.
               ENDIF
C
C          FIND PRECIPITATION MIXING RATIO; PW IS COMPUTED BY USING 1 MB AS
C            THE THICKNESS OF THE LAYER IN QUESTION. QR IS COMPUTED FOR one
C            SECOND.
C
C              PW=1013.53063*QV
               PW=10.19767*QV
C              PW=101.9767*QV
C              PW=254.94175*QV
C              PW=PWAT
               QR=TM*(PRATE+CPRAT)/PW
               IF(C13(II,JJ).EQ.1.OR.RH.EQ.0.)THEN
                  QV=0.
                  QC=0.
                  QR=0.
               ENDIF
C
C          FIND VISUAL RANGE
C
               CALL CALVIS(QV,QC,QR,CRAIN,CFRZR,CICEP,CSNOW,TA,PP,VIS)
               !DEBUG PRINT *,'mixr w calvis vis = ',w, vis
               IF(C14(II,JJ).EQ.0..AND.RH.LE.85.)THEN
                  IF(VIS.LT.13000.)THEN
                     VIS=13000. + VIS
                     VIS=MIN(20000.,VIS)
                  ENDIF
               ELSE IF(C14(II,JJ).EQ.0..AND.RH.GT.85..AND.RH.LE.98.)
     1            THEN
                  IF(VIS.LT.5600.)THEN
                     VIS=5600. + VIS
                     VIS=MIN(20000.,VIS)
                  ENDIF
               ELSE IF(C14(II,JJ).EQ.1..AND.RH.LE.95.)THEN
                  IF(VIS.LT.13000.)THEN
                     VIS=13000. + VIS
                     VIS=MIN(20000.,VIS)
                  ENDIF 
               ELSE IF(C14(II,JJ).EQ.1..AND.RH.GT.95..AND.RH.LE.99.)
     1            THEN
                  IF(VIS.LT.5600.)THEN
                     VIS=5600. + VIS
                     VIS=MIN(20000.,VIS)
                  ENDIF
               ENDIF
               VIS=MIN(20000.,VIS)
               C84(II,JJ)=VIS
CNNNNXB----------------------------------------------------------------ENNNN
           ENDDO
         ENDDO
         CALL SMTH25(C84,SF,C84S)
         WRITE(98,2000)C01(100,20),C02(100,20),C03(100,20),
     1                 C04(100,20),C05(100,20),C06(100,20),
     2                 C07(100,20),C08(100,20),C09(100,20),
     3                 C10(100,20),C11(100,20),C12(100,20),
     3                 C13(100,20)
 2000    FORMAT(' ',F9.5,1X,F9.5,1X,F9.7,1X,F9.7,1X,F9.7,1X,
     1          F9.5,1X,F3.0,1X,F3.0,1X,F3.0,1X,F3.0,1X,F9.2,1X,F9.2,
     2          1X,F3.0)
         WRITE(98,2010)C84(100,20),C84S(100,20)
 2010    FORMAT(' ',F9.2,1X,F9.2)
C                                                                       
C          WRITE FORECAST FIELD TO DISK AND ARCHIVE AND RE-INITIALIZE   
C                                                                       
         CALL WRITEF(C84S,KK,IGRIB,IERR)
 1240    NERR=0                                                         
 1250 CONTINUE
      CALL BACLOSE(LUG1)
 1400 RETURN                                                            
      END                                                               
      SUBROUTINE transl(arg1,arg2,IFCST,fld,IONA)
      IMPLICIT none
      INTEGER arg1, arg2, ifcst, iona
      INTEGER nx, ny
      PARAMETER (nx = 361)
      PARAMETER (ny = 181)
      REAL x(nx-1,ny), fld(nx,ny)
      INTEGER i,j

      PRINT *,'transl reading from IFCST ',ifcst
      READ (IFCST) x
      PRINT *,'transl back ',MAXVAL(x), MINVAL(x)
      DO j = 1, ny
        DO i = 1, nx-1
          fld(i,j) = x(i,j)
        ENDDO
        fld(nx,j) = fld(1,j)
      ENDDO
      IONA = 0

      RETURN
      END
