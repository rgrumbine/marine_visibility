#!/bin/sh
#-------- 09/29/2003 ------ BEGIN EXFOG SCRIPT -------------------------
 
set -xe
cd $DATA

# ######################################################################
# start GBLVSBY processing
# modified: 20041027
# changes: 1) re-arranged script
#          2) added step to create northern hemispheric GRIB2 bulletins
#             for AWIPS
# ######################################################################

# **************************************************
#   PRODUCE GLOBAL VISIBILITY GUIDANCE
# **************************************************

for fhr in 00  03  06  09  12  15  18  21  24  27  30  33  36  39  42  45  48 \
           51  54  57  60  63  66  69  72  75  78  81  84  87  90  93  96  99 \
          102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 \
          153 156 159 162 165 168
do
#for grib2:
  if [ $fhr -lt 100 ] ; then
    fhr=0$fhr
  fi
#grib2
  cp $COMINgfs/gfs.${cycle}.pgrb2.1p00.f${fhr} pgrb2f${fhr}
#  if [ -f $COMINgfs/gfs.${cycle}.pgrb2.f${fhr}.idx ] ; then
#    cp $COMINgfs/gfs.${cycle}.pgrb2.f${fhr}.idx pgrb2if${fhr}
#  else
    ${utilexec}/wgrib2 pgrb2f${fhr} > pgrb2if${fhr}
    export err=$?; err_chk
#  fi
done

cp $COMINsst/sst2dvar_grb sst2dvar_grb

echo $PDY
echo $cyc
echo $ypdy
  
echo $PDY > dtg.ft90
echo $cyc > dtg.ft91

echo " 1   1" > sf.ft92
 
export pgm="omb_gblvsby"; . prep_step

iunit=10
junit=110
fhr=003
for field in ICEC LAND
do
  grep "$field" pgrb2if$fhr | ${utilexec}/wgrib2 -i pgrb2f$fhr -append -order we:ns -bin static
done
export XLFUNIT_${iunit}=static
ln -sf static fort.$iunit
if [ ! -s static ] ; then
  echo "the static fields file is empty!"
  exit
fi

for fhr in 00  03  06  09  12  15  18  21  24  27  30  33  36  39  42  45  48 \
           51  54  57  60  63  66  69  72  75  78  81  84  87  90  93  96  99 \
          102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 \
          153 156 159 162 165 168
do
#for grib2:
  if [ $fhr -lt 100 ] ; then
    fhr=0$fhr
  fi
#
  iunit=`expr $iunit + 1 `
  junit=`expr $junit + 1 `
  for field in ':TMP:2 m above' 'RH:2 m above' 'CLWMR:1000 mb' PRATE CPRAT \
                PWAT CRAIN CFRZR CICEP CSNOW PRES:surface TMP:surface ICEC LAND
  do
    grep "$field" pgrb2if$fhr | ${utilexec}/wgrib2 -i pgrb2f$fhr -append -order we:ns -bin fort.$iunit
  done

#   export XLFUNIT_${iunit}="pgrb2f${fhr}"
#   ln -s pgrb2f${fhr} fort.${iunit}
   export XLFUNIT_${junit}="pgrb2if${fhr}"
   ln -s pgrb2fi${fhr} fort.${junit}
done

export XLFRTEOPTS="unit_vars=yes"
export XLFUNIT_68="gvisg.dat"
export XLFUNIT_98="gvis.ft98"
export XLFUNIT_90="dtg.ft90"
export XLFUNIT_91="dtg.ft91"
export XLFUNIT_92="sf.ft92"
export XLFUNIT_93="sst2dvar_grb"
ln -s gvisg.dat fort.68
ln -s gvis.ft98 fort.98
ln -s dtg.ft90  fort.90
ln -s dtg.ft91  fort.91
ln -s sf.ft92   fort.92
ln -s sst2dvar_grb fort.93

startmsg
$EXECglobal_fog/omb_gblvsby >> $pgmout 2> errfile
export err=$?; err_chk
echo "RG Done with model itself"

# ********************************************************************
#   Create GRIB index file
# ********************************************************************

${utilexec}/grbindex gvisg.dat gvisgi.dat
export err=$?; err_chk
head -1 gvisgi.dat

# *********************************************************************
#   Create grid 232 northern hemispheric 1x1 deg lon/lat grid GRIB file
# *********************************************************************

${utilexec}/copygb -g 232 -x gvisg.dat fvnhg.dat #please note change
export err=$?; err_chk
${utilexec}/grbindex fvnhg.dat fvnhgi.dat
export err=$?; err_chk

echo "RG Done with copygb 232"

# *************************************************************
# Build GRIB bulletins
# *************************************************************

# *************************************************************
# Make grib2 conversion
# *************************************************************

${utilexec}/cnvgrib -g12 -p40 fvnhg.dat grib2
export err=$?; err_chk

# *************************************************************
# Make grib2 index
# *************************************************************

${utilexec}/grb2index grib2 gribi2
export err=$?; err_chk

echo "RG done with cnvgrib grb2index"

# *************************************************************
# Create data set to send to TOC/AWIPS
# *************************************************************

export XLFRTEOPTS="unit_vars=yes"
export FORT11="grib2"
export FORT31="gribi2"
export FORT51="xtrn.awpfog.t${cyc}"

ln -fs grib2                 fort.11
ln -fs gribi2                fort.31
ln -fs xtrn.awpfog.t${cyc}   fort.51

${utilexec}/tocgrib2 <${utilparm}/grib2_awpnhvis.232 1>> $pgmout 2>> errfile
export err=$?; err_chk

echo "RG done with tocgrib2"

# *************************************************************
# Copy files to COM; note rearrangement of script
# *************************************************************

if test $SENDCOM = 'YES'
then
  echo "ncosp" > $COMOUT/where_gblvsby_ran.$cycle
  cp gvisg.dat $COMOUT/fog.${cycle}.gvisg
  cp gvisgi.dat $COMOUT/fog.${cycle}.gvisgi
  cp fvnhg.dat $COMOUT/fog.${cycle}.fvnhg
  cp fvnhgi.dat $COMOUT/fog.${cycle}.fvnhgi
  cp xtrn.awpfog.t${cyc} ${PCOM}/xtrn.awpfog.t${cyc}
fi

################################
# Convert to grib2 format
################################
for fil in gvisg fvnhg
do
  ${utilexec}/cnvgrib -g12 -p40 $COMOUT/fog.${cycle}.${fil} $COMOUT/fog.${cycle}.${fil}.grib2
  export err=$?; err_chk
  ${utilexec}/wgrib2 $COMOUT/fog.${cycle}.${fil}.grib2 -s >$COMOUT/fog.${cycle}.${fil}.grib2.idx
  export err=$?; err_chk
done

# ******************************************
# Send files to ftpprd and pcom with alerts
# ******************************************

if [ "$SENDDBN" = 'YES' ]
then
# add line to send xtrn.awpfog.t${cyc} to pcom and alert the TOC
# JY - turn off the alert -05/11/2016
  #if [ "$SENDDBN_NTC" = 'YES' ]; then
  #    $DBNROOT/bin/dbn_alert GRIB_LOW OMBFOG $job $PCOM/xtrn.awpfog.t${cyc}
  #fi
  $DBNROOT/bin/dbn_alert MODEL OMBFOG $job $COMOUT/fog.${cycle}.gvisg
  $DBNROOT/bin/dbn_alert MODEL OMBFOG $job $COMOUT/fog.${cycle}.fvnhg

  $DBNROOT/bin/dbn_alert MODEL OMBFOG_GB2 $job $COMOUT/fog.${cycle}.gvisg.grib2
  $DBNROOT/bin/dbn_alert MODEL OMBFOG_GB2 $job $COMOUT/fog.${cycle}.fvnhg.grib2
 
  $DBNROOT/bin/dbn_alert MODEL OMBFOG_GB2_WIDX $job $COMOUT/fog.${cycle}.gvisg.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL OMBFOG_GB2_WIDX $job $COMOUT/fog.${cycle}.fvnhg.grib2.idx
fi

set +x
echo " ***** PROCESSING COMPLETED NORMALLY *****"
set -x

# ---------------END OF EXFOG SCRIPT ----------------------------
