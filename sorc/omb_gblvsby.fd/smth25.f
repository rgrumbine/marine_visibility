C
CXXXXNB----------------------------------------------------------------ENNNNNNNN
      SUBROUTINE SMTH25(UNSM,SF,SMTH)
C
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SMTH25                  SMOOTHES DATA
C   PRGMMR: L. D. BURROUGHS  ORG: W/NMC21    DATE: 03-03-09
C
C ABSTRACT: SMOOTHES DATA AT A LOCATION (I,J) BY TAKING THE WEIGHTED    
C   AVERAGE OF THE DATA AT THE 16 SURROUNDING POINTS AND (I,J).       
C
C PROGRAM HISTORY LOG:
C   03-03-09  L. D. BURROUGHS
C
C USAGE:    CALL SMTH25(UNSM,I,J,SF,SMTH)
C
C   INPUT ARGUMENT LIST:
C     UNSM(,)  - FIELD OF DATA TO BE SMOOTHED - R*4
C     SF       - SMOOTHER NUMBER - R*4
C                0 NO SMOOTHING DONE. -1 0 1  -2-1 0 1 2
C                1  5 POINT SMOOTHER>1   .   2     1
C                                    0 . . . 1     2
C                                   -1   .   0 1 2 3 2 1
C                2  9 POINT SMOOTHER ======>-1     2        -1 0 1
C                                           -2     1       1 . . .
C                3  9 POINT SMOOTHER =====================>0 . . .
C                                        -3-2-1 0 1 2 3   -1 . . .
C                                       3       .         -4-3-2-1 0 1 2 3 4
C                4 13 POINT SMOOTHER ==>2       .        4         .
C                                       1       .        3         .
C                                       0 . . . . . . .  2         .
C                                      -1       .        1         .
C                                      -2       .        0 . . . . . . . . .
C                                      -3       .       -1         .
C                5 17 POINT SMOOTHER ==================>-2         .
C                                        -2-1 0 1 2     -3         .
C                                       2 . . . . .     -4         .
C                6 25 POINT SMOOTHER    1 . . . . .
C                                       0 . . . . . 
C                                      -1 . . . . .  -1 0 1
C                                      -2 . . . . . 1 1 2 1
C                7  9 POINT WEIGHTED SMOOTHER =====>0 2 3 2
C                                                  -1 1 2 1
C
C   OUTPUT ARGUMENT LIST:
C     SMTH     - THE SMOOTHED VALUE MATRIX R*4
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
C       SPECIAL  - NONE
C
C REMARKS: NONE
C
C     VARIABLES:
C       INPUT    - NONE
C
C       INTERNAL - SUMIP2     = SUM OF VALUES FROM JM2 TO JP2 ON ROW IP2 FOR
C                               LOCATION (I,J)
C                  SUMIP1     = SUM OF VALUES FROM JM2 TO JP2 ON ROW IP1 FOR
C                               LOCATION (I,J)
C                  SUMI       = SUM OF VALUES FROM JM2 TO JP2 ON ROW I FOR
C                               LOCATION (I,J)
C                  SUMIM1     = SUM OF VALUES FROM JM2 TO JP2 ON ROW IM1 FOR
C                               LOCATION (I,J)
C                  SUMIM2     = SUM OF VALUES FROM JM2 TO JP2 ON ROW IM2 FOR
C                               LOCATION (I,J)
C                  AVG        = AVERAGE VALUE OF THE SUMS ADDED TOGETHER
C                               DIVIDED BY THE NUMBER OF POINTS (25)
C
C       OUTPUT   - NONE
C
C ATTRIBUTES:
C   LANGUAGE: VS FORTRAN
C   MACHINE:  NAS
C
C$$$
CNNNNXB----------------------------------------------------------------ENNNNNNNN
      IMPLICIT none
      SAVE

! Arguments
      REAL SF, UNSM(361,181),SMTH(361,181)
C
! Local
      REAL*4 ISURSM,JSURSM
      INTEGER i, j
      INTEGER ip1, ip2, ip3, ip4
      INTEGER im1, im2, im3, im4
      INTEGER jp1, jp2, jp3, jp4
      INTEGER jm1, jm2, jm3, jm4
      REAL sumip1, sumi, sumim1
      REAL sumip2, sumim2
      
C
      DO J=1,181
         DO I=1,361
            IF(SF.LT.0..OR.SF.GT.7.)THEN
               SMTH(I,J)=UNSM(I,J)
            ENDIF
            IP1=I+1
            IP2=I+2
            IP3=I+3
            IP4=I+4
            IM1=I-1
            IM2=I-2
            IM3=I-3
            IM4=I-4
            JP1=J+1
            JP2=J+2
            JP3=J+3
!Original -- typo noticed by Liz Orelup, lizo@oceanweather.com            JP2=4+4
            JP4=J+4
            JM1=J-1
            JM2=J-2
            JM3=J-3
            JM4=J-4
            IF(SF.EQ.0.)THEN
               SMTH(I,J)=UNSM(I,J)
            ENDIF
CNNNNXB----------------------------------------------------------------ENNNNNNNN
            IF(SF.EQ.1.)THEN
               IF((I.LT.2.OR.I.GT.360).OR.(J.LT.2.OR.J.GT.180))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  ISURSM=UNSM(IP1,J)+UNSM(IM1,J)
                  JSURSM=UNSM(I,JP1)+UNSM(I,JM1)
                  SMTH(I,J)=(ISURSM+JSURSM+UNSM(I,J))/5.
               ENDIF
            ENDIF
            IF(SF.EQ.2.)THEN
               IF((I.LT.3.OR.I.GT.359).OR.(J.LT.3.OR.J.GT.179))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  ISURSM=UNSM(IP2,J)+2*UNSM(IP1,J)+2*UNSM(IM1,J)+
     1                   UNSM(IM2,J)
                  JSURSM=UNSM(I,JP2)+2*UNSM(I,JP1)+2*UNSM(I,JM1)+
     1                   UNSM(I,JM2)
                  SMTH(I,J)=(ISURSM+JSURSM+3*UNSM(I,J))/15.
               ENDIF
            ENDIF
            IF(SF.EQ.3.)THEN
               IF((I.LT.3.OR.I.GT.359).OR.(J.LT.3.OR.J.GT.179))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  SUMIP1=UNSM(IP1,JP1)+UNSM(IP1,J)+UNSM(IP1,JM1)
                  SUMI=UNSM(I,JP1)+UNSM(I,J)+UNSM(I,JM1)
                  SUMIM1=UNSM(IM1,JP1)+UNSM(IM1,J)+UNSM(IM1,JM1)
                  SMTH(I,J)=(SUMIP1+SUMI+SUMIM1)/9.
               ENDIF
            ENDIF
CNNNNXB----------------------------------------------------------------ENNNNNNNN
            IF(SF.EQ.4.)THEN
               IF((I.LT.4.OR.I.GT.358).OR.(J.LT.4.OR.J.GT.178))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  ISURSM=UNSM(IP3,J)+UNSM(IP2,J)+UNSM(IP1,J)+
     1                   UNSM(IM1,J)+UNSM(IM2,J)+UNSM(IM3,J)
                  JSURSM=UNSM(I,JP3)+UNSM(I,JP2)+UNSM(I,JP1)+
     1                   UNSM(I,JM1)+UNSM(I,JM2)+UNSM(I,JM3)
                  SMTH(I,J)=(ISURSM+JSURSM+UNSM(I,J))/13.
               ENDIF
            ENDIF
            IF(SF.EQ.5.)THEN
               IF((I.LT.5.OR.I.GT.357).OR.(J.LT.5.OR.J.GT.177))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  ISURSM=UNSM(IP4,J)+UNSM(IP3,J)+UNSM(IP2,J)+
     1                   UNSM(IP1,J)+UNSM(IM1,J)+UNSM(IM2,J)+
     2                   UNSM(IM3,J)+UNSM(IM4,J)
                  JSURSM=UNSM(I,JP4)+UNSM(I,JP3)+UNSM(I,JP2)+
     1                   UNSM(I,JP1)+UNSM(I,JM1)+UNSM(I,JM2)+
     2                   UNSM(I,JM3)+UNSM(I,JM4)
                  SMTH(I,J)=(ISURSM+JSURSM+UNSM(I,J))/17.
               ENDIF
            ENDIF
CNNNNXB----------------------------------------------------------------ENNNNNNNN
            IF(SF.EQ.6.)THEN
               IF((I.LT.6.OR.I.GT.356).OR.(J.LT.6.OR.J.GT.176))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  SUMIP2=UNSM(IP2,JP2)+UNSM(IP2,JP1)+UNSM(IP2,J)+
     1                   UNSM(IP2,JM1)+UNSM(IP2,JM2)
                  SUMIP1=UNSM(IP1,JP2)+UNSM(IP1,JP1)+UNSM(IP1,J)+
     1                   UNSM(IP1,JM1)+UNSM(IP1,JM2)
                  SUMI=UNSM(I,JP2)+UNSM(I,JP1)+UNSM(I,J)+UNSM(I,JM1)+
     1                 UNSM(I,JM2)
                  SUMIM1=UNSM(IM1,JP2)+UNSM(IM1,JP1)+UNSM(IM1,J)+
     1                   UNSM(IM1,JM1)+UNSM(IM1,JM2)
                  SUMIM2=UNSM(IM2,JP2)+UNSM(IM2,JP1)+UNSM(IM2,J)+
     1                   UNSM(IM2,JM1)+UNSM(IM2,JP2)
                  SMTH(I,J)=(SUMIP2+SUMIP1+SUMI+SUMIM1+SUMIM2)/25.
               ENDIF
            ENDIF
            IF(SF.EQ.7.)THEN
               IF((I.LT.3.OR.I.GT.359).OR.(J.LT.3.OR.J.GT.179))THEN
                  SMTH(I,J)=UNSM(I,J)
               ELSE
                  SUMIP1=UNSM(IP1,JP1)+2*UNSM(IP1,J)+UNSM(IP1,JM1)
                  SUMI=2*UNSM(I,JP1)+2*UNSM(I,J)+2*UNSM(I,JM1)
                  SUMIM1=UNSM(IM1,JP1)+2*UNSM(IM1,J)+UNSM(IM1,JM1)
                  SMTH(I,J)=(SUMIP1+SUMI+SUMIM1)/14.
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
