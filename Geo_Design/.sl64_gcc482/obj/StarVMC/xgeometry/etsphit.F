C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
******************************************************************************
* $Id: etsphit.g,v 1.1 2004/01/19 21:16:53 potekhin Exp $
* $Log: etsphit.g,v $
* Revision 1.1  2004/01/19 21:16:53  potekhin
* Need to make this into a separate compilation unit
* due to necessary code changes in the calbgeo
*
      SUBROUTINE ETSPHIT(J,HIT)                                           8
*
      IMPLICIT NONE                                                       10
C - Dynamic core for the GEANT data (v3.15)                               10
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   10
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        10
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  10
     *LQ,JCG                                                              10
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       10
      PARAMETER (KWBANK=69000)                                            10
      PARAMETER (KWWORK=5200)                                             10
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           10
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      10
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      10
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    10
     *JGPAR,JGPAR2,JSKLT                                                  10
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                10
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   10
     *JGSTAT)                                                             10
C - Multi-level current volume description                                10
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,NLDEV,      10
     *LINMX                                                               10
      REAL GTRAN,GRMAT,GONLY,GLX                                          10
      COMMON /GCVOLU/ NLEVEL,NAMES(15),NUMBER(15),LVOLUM(15),LINDEX(15),  10
     *INFROM, NLEVMX,NLDEV(15),LINMX(15),GTRAN(3,15),GRMAT(10,15),        10
     *GONLY(15),GLX(3)                                                    10
C - Kinematics of current track                                           10
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           10
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            10
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       10
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               10
C - track parameters at current point                                     10
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    10
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               10
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  10
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           10
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           10
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      10
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       10
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     10
C Connection between AgGSTEP, AgGHIT and a user step routine              10
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   10
      INTEGER IHAVEMORE                                                   10
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    10
      COMMON /AGCSTEPH/ HITS(15)                                          10
      COMMON /AGCSTEPI/ IHAVEMORE                                         10
*
      INTEGER J,IPLANE,ISHAPE                                             12
      INTEGER NLEV_R,NLEV_M,MYCELL,JMYCELL,JMYPAR                         13
      CHARACTER CNAME*4,MNAME*4                                           14
      DATA MNAME/'CSDA'/                                                  15
      REAL HIT,XYZHM(3),XYZH(3),DXHW,DYHW,DZHW                            16
      SAVE NLEV_R,NLEV_M,DXHW,DYHW,DZHW                                   17
*
      NLEV_R=NLEVEL-2                                                     19
      MYCELL=LVOLUM(NLEV_R)                                               20
      JMYCELL=LQ(JVOLUM-MYCELL)                                           21
      CALL UHTOC(IQ(JMYCELL-4),4,CNAME,4)                                 22
      IF (CNAME.EQ.MNAME) THEN                                            23
      ISHAPE=Q(JMYCELL+2)                                                 24
      IF (ISHAPE.EQ.1) THEN                                               25
      JMYPAR=LQ(JGPAR-NLEV_R)                                             26
      DXHW=Q(JMYPAR+1)                                                    27
      DYHW=Q(JMYPAR+2)                                                    28
      DZHW=Q(JMYPAR+3)                                                    29
      ENDIF                                                               30
      ELSE                                                                31
      PRINT *,'There are some problem with level volume in the CSDA'      32
      ENDIF                                                               33
      NLEV_M=NLEVEL                                                       34
      CALL GDTOM(XLOC(1),XYZHM(1),1)                                      35
      HITS(1)=HIT                                                         36
      IPLANE=HIT                                                          37
      NLEVEL=NLEV_R                                                       38
      CALL GMTOD(XYZHM(1),XYZH(1),1)                                      39
      NLEVEL=NLEV_M                                                       40
      IF (IPLANE.LE.2) THEN                                               41
      HIT=XYZH(3)+DZHW                                                    42
      ELSE                                                                43
      HIT=XYZH(2)+DYHW                                                    44
      ENDIF                                                               45
*
      END                                                                 47
