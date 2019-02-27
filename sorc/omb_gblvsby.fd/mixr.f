C                                                                       
      SUBROUTINE MIXR(PR,TS,T,RH,W,QC2)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    MIXR         COMPUTES MIXING RATIO
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 03-03-06             
C                                                                       
C ABSTRACT: COMPUTES THE MIXING RATIO FROM PRES, TEMP, AND REL HUM
C           BY USING THE TETONS FORMULATION IN THE SMITHSONIAN
C           METEOROLOGICAL TABLES.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   03-03-06  L. D. BURROUGHS                                           
C                                                                       
C USAGE:    CALL MIXR(PR,TS,T,RH,W,QC2)
C                                                                       
C   INPUT ARGUMENT LIST:                                                
C     PR       - PRESSURE - R*4 (MB) (USED WHEN EQ POT TEMP (EPT) IS    
C                COMPUTED FOR CONSTANT PRESSURE SURFACES)               
C     T        - AIR TEMPERATURE AT THE GIVEN PRESSURE - R*4 (DEG K)    
C     RH       - RELATIVE HUMIDITY AT GIVEN PRESSURE AND TEMPERATURE -  
C                R*4 (PERCENT) (USED WHEN EPT IS COMPUTED FOR CONSTANT  
C                PRESSURE SURFACES OR AT SFC WHEN 1000 MB RH IS ASSUMED 
C                TO BE THE SAME AS THE SURFACE RH)                      
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     W        - MIXING RATIO (KG/KG) R*4
C                                                                       
C   INPUT FILES:                                                        
C     NONE                                                              
C                                                                       
C   OUTPUT FILES:                                                       
C     NONE                                                              
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C                                                                       
C     UNIQUE:    - NONE                                                 
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - NONE                                                 
C                                                                       
C       IMSL     - NONE                                                 
C                                                                       
C       SPECIAL  - EXP     ALOG
C                                                                       
C REMARKS: NONE
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C       INPUT    - NONE                                                 
C                                                                       
C       INTERNAL - CP         = SPECIFIC HEAT OF DRY AIR AT CNST PRES - 
C                               R*4 (CAL/((GM OF DRY AIR)*DEG K))       
C                  ES         = SATURATION VAPOR PRESSURE - R*4 (MB)    
C                  WS         = SATURATION MIXING RATIO - R*4 (G WATER  
C                               VAPOR/G DRY AIR)                        
C                  W          = MIXING RATIO FOR A GIVEN RH - R*4       
C                                                                       
C       OUTPUT   - NONE                                                 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90
C   MACHINE:  IBM SP
C                                                                       
C$$$                                                                    
      IMPLICIT none

      SAVE
! Arguments
      REAL PR, TS, T, RH, W, QC2
! Local
      REAL CP, D, ES1, ES, ESS1, ESS
      REAL a, b, c, e
      REAL a1, b1, c1, d1, e1
      REAL ws, wss
C                                                                       
C          SET CONSTANTS
C                                                                       
      CP=.240
      D=LOG10(6.1071)
      IF(RH.LE.0.)GO TO 1050                                            
C
C          COMPUTE SATURATION VAPOR PRESSURE
C
      IF(TS.GE.273.16)THEN
         ES1=(17.2694*T-4717.31)/(T-35.86)
         IF(ES1.GE.174.673)GO TO 1050
         ES=6.1078*EXP(ES1)
         ESS1=(17.2694*TS-4717.31)/(TS-35.86)
         IF(ESS1.GE.174.673)GO TO 1050
         ESS=6.1078*EXP(ESS1)
      ELSE
         IF(T.LE.0.)GO TO 1050
         A=-9.09718*((273.16/T)-1.)
         B=-3.56654*LOG10(273.16/T)
         C= 0.876793*(1.-(T/273.16))
         D= LOG10(6.1071)
         E= A+B+C+D
         ! Error, found by lizo@oceanweather.com ES=ALOG10(E)
         ES = 10**E
         A1=-9.09718*((273.16/TS)-1.)
         B1=-3.56654*LOG10(273.16/TS)
         C1= 0.876793*(1.-(TS/273.16))
         D1= LOG10(6.1071)
         E1= A1+B1+C1+D1
         ! Error, found by lizo@oceanweather.com ESS=ALOG10(E1)
         ESS = 10**E1
      ENDIF
C
C          COMPUTE SATURATION MIXING RATIO, MIXING RATIO, AND VAPOR
C          PRESSURE
C
      WS=0.622*ES/(PR-ES)
      WSS=0.622*ESS/(PR-ESS)
      W=RH*WS/100.
      QC2=W-WSS
      E=PR*W/(W+0.622)
      GO TO 1060

 1050 CONTINUE
      W=0.
C                                                                       
 1060 RETURN                                                            
      END                                                               
