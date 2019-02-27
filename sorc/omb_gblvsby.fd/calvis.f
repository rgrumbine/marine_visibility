CNNNNXB----------------------------------------------------------------E
      SUBROUTINE CALVIS(QV,QC,QR,CRAIN,CFRZR,CICEP,CSNOW,TA,PP,VIS)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK                                   
C                .      .    .                                       .  
C SUBPROGRAM:    CALVIS      COMPUTES VISUAL RANGE IN KM FROM GFS
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 03-03-09             
C                                                                       
C ABSTRACT: COMPUTES HORIZONTAL VISIBILITY AT THE 1000 HPA LAYER FROM
c   QV, QC, QR, CRAIN, CFRZR, CICEP,CSNOW, AIR TEMP AND PRESSURE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   03-02-03  L. D. BURROUGHS                                           
C                                                                       
C USAGE:    CALL CALVIS(QV,QC,QR,CRAIN,CFRZR,CICEP,CSNOW,TA,PP,VIS)
C                                                                       
C   INPUT ARGUMENT LIST:                                                
c      qv      - water vapor mixing ratio (kg/kg)
c      qc      - cloud water mixing ratio (kg/kg)
c      qr      - rain water mixing ratio  (kg/kg)
c      ta      - air temperature          (k)
c      pp      - pressure                 (hPa)
c      crain   - categorical rain         (y=1,n=0)
c      cfrzr   - categorical freezing rain(y=1,n=0)
c      cicep   - categorical ice pellets  (y=1.n=0)
c      csnow   - categorical snow         (y=1,n=0) 
c
C                                                                       
C   OUTPUT ARGUMENT LIST:                                               
C     vis      - visual range in km
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
C       SPECIAL  - LOG
C                                                                       
C REMARKS:
c   If crain and cfrzr = 0 and cicep and csnow = 0; then qc determines
c                  vis alone;
c   if crain or cfrzr =1 and cicep and csnow = 0; then qc and qr deter-
c           mine   vis;
c   if crain or cfrzr =1 and cicep or csnow =1; then qc and a combo of
c           50 percent rain and snow is used to determine vis;
c   if crain or cfrzr =0 and cicep or csnow =1; then qc and qs deter-
c           nine   vis;
C                                                                       
c   The routine uses the following
c   expressions for extinction coefficient, beta (in km**-1),
c   with C being the mass concentration (in g/m**3):
c
c      cloud water:  beta = 144.7 * C ** (0.8800)
c      rain water:   beta =  2.24 * C ** (0.7500)
c      cloud ice:    beta = 327.8 * C ** (1.0000)
c      snow:         beta = 10.36 * C ** (0.7776)
c
c   These expressions were obtained from the following sources:
c
c      for cloud water: from Kunkel (1984)
c      for rainwater: from M-P dist'n, with No=8e6 m**-4 and
c         rho_w=1000 kg/m**3
c      for cloud ice: assume randomly oriented plates which follow
c         mass-diameter relationship from Rutledge and Hobbs (1983)
c      for snow: from Stallabrass (1985), assuming beta = -ln(.02)/vis
c
c   The extinction coefficient for each water species present is
c   calculated, and then all applicable betas are summed to yield
c   a single beta. Then the following relationship is used to
c   determine visibility (in km), where epsilon is the threshhold
c   of contrast, usually taken to be .02:
c
c      vis = -ln(epsilon)/beta      [found in Kunkel (1984)]
c
c   This procedure was first developed by Stoelinga and Warner, adapted to the
c   eta model postprocessor by Geoff Manikin, and adapted by Larry Burroughs for
c   use with the GFS to replace the statistical open ocean fog and visibility	c   system.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90
C   MACHINE:  IBM SP
C                                                                       
C$$$                                                                    
C                                                                       
      IMPLICIT none
CNNNNX------------------------------------------------------------------
      SAVE
      REAL rdc, coeflc, coeflp, coeffc, coeffp
      REAL exponlc, exponlp, exponfc, exponfp, rhoice, rhowat
     
      PARAMETER(RDC=0.34838,COEFLC=144.7,COEFLP=2.24,COEFFC=327.8,
     1          COEFFP=10.36,EXPONLC=0.8800,EXPONLP=0.7500,
     2          EXPONFC=1.0000,EXPONFP=0.7776,RHOICE=916.8,
     3          RHOWAT=997.37)

! Arguments
      REAL qv, qc, qr, crain, cfrzr, cicep, csnow, ta, pp, vis
! Local
      REAL const1, sh, tv, rhoair, qcld, vovermd, concfp
      REAL conclc, conclp, concfc, qprr, qprs, qcldi
      REAL betav
CNNNNX------------------------------------------------------------------
      CONST1=-LOG(0.02)
      SH=QV/(1+QV)
      TV=TA*(1.0+0.608*SH)
      RHOAIR=RDC*PP/TV
C
C        Determine precipitation type and mixing ratio values
C
      IF(CRAIN.EQ.0..AND.CFRZR.EQ.0..AND.CICEP.EQ.0..AND.CSNOW.EQ.0.)
     1   THEN
         QCLD=QC
         VOVERMD=(1.+QV)/RHOAIR+QCLD/RHOWAT
         CONCLC = QCLD/VOVERMD*1000.
         CONCLP = 0.
         CONCFC = 0.
         CONCFP = 0.
      ELSEIF((CRAIN.EQ.1..OR.CFRZR.EQ.1.).AND.CICEP.EQ.0..AND.
     1   CSNOW.EQ.0.)THEN
         QPRR=QR
         QCLD=QC
         VOVERMD=(1.+QV)/RHOAIR+(QPRR+QCLD)/RHOWAT
         CONCLC = QCLD/VOVERMD*1000.
         CONCLP = QPRR/VOVERMD*1000.
         CONCFC = 0.
         CONCFP = 0.
      ELSEIF((CRAIN.EQ.1..OR.CFRZR.EQ.1.).AND.(CICEP.EQ.1..OR.
     1   CSNOW.EQ.1.))THEN
         QPRR=0.5*QR
         QPRS=0.5*QR
         QCLD=0.5*QC
         QCLDI=0.5*QC
         VOVERMD=(1.+QV)/RHOAIR+(QPRR+QCLD)/RHOWAT+(QPRS+QCLDI)/RHOICE
         CONCLC = QCLD/VOVERMD*1000.
         CONCLP = QPRR/VOVERMD*1000.
         CONCFC = QCLDI/VOVERMD*1000.
         CONCFP = QPRS/VOVERMD*1000.
      ELSEIF(CRAIN.EQ.0..AND.CFRZR.EQ.0..AND.(CICEP.EQ.1..OR.
     1   CSNOW.EQ.1.))THEN
         QPRS=QR
         QCLDI=QC
         IF(TA.LE.263.)THEN
            VOVERMD=(1.+QV)/RHOAIR+(QPRS+QCLDI)/RHOICE
            CONCLC = 0.
            CONCLP = 0.
            CONCFC = QCLDI/VOVERMD*1000.
            CONCFP = QPRS/VOVERMD*1000.
         ELSE
            VOVERMD=(1.+QV)/RHOAIR+QCLDI/RHOWAT+QPRS/RHOICE
            CONCLC = QCLDI/VOVERMD*1000.
            CONCLP = 0.
            CONCFC = 0.
            CONCFP = QPRS/VOVERMD*1000.
         ENDIF
      ENDIF
      BETAV=COEFFC*CONCFC**EXPONFC+COEFFP*CONCFP**EXPONFP
     1     +COEFLC*CONCLC**EXPONLC+COEFLP*CONCLP**EXPONLP
     2     +1.E-10
c CHANGED GSM 3-10-00 -->  no point in distinguishing values
c       above 20 km, so make that value the max (prev max was 80)
        VIS=1.E3*MIN(20.,CONST1/BETAV)
C
      RETURN
      END
