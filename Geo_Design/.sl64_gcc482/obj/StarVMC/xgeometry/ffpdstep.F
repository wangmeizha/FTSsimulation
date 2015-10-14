C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: ffpdstep.g,v 1.2 2004/03/31 16:34:10 potekhin Exp $
* $Log: ffpdstep.g,v $
* Revision 1.2  2004/03/31 16:34:10  potekhin
* Factored out two methods due to code
* reorganization.
*
      SUBROUTINE FFPDSTEP                                                 8
* discard cerenkov photon absorption hits everywhere except for
* photocathode
      IMPLICIT NONE                                                       11
C - Dynamic core for the GEANT data (v3.15)                               11
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   11
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        11
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  11
     *LQ,JCG                                                              11
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       11
      PARAMETER (KWBANK=69000)                                            11
      PARAMETER (KWWORK=5200)                                             11
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           11
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      11
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      11
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    11
     *JGPAR,JGPAR2,JSKLT                                                  11
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                11
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   11
     *JGSTAT)                                                             11
C - Natural constants (or considered as such)                             11
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   11
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        11
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       11
C - GEANT description of logical units                                    11
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            11
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      11
      COMMON/GCMAIL/ CHMAIL                                               11
      CHARACTER*132 CHMAIL                                                11
C - parameters of current tracking medium                                 11
      INTEGER NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD                11
      REAL FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC            11
      COMMON/GCTMED/ NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX,   11
     *DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD                   11
      REAL THRIND,PMIN,DP,DNDL                                            11
      INTEGER JMIN,ITCKOV,IMCKOV,NPCKOV                                   11
      COMMON/GCTLIT/ THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV        11
C - track parameters at current point                                     11
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    11
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               11
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  11
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           11
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           11
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      11
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       11
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     11
C - Kinematics of current track                                           11
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           11
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            11
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       11
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               11
C - Identification of the current sensitive detector                      11
      INTEGER IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV                   11
      COMMON /GCSETS/ IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)       11
C Connection between AgGSTEP, AgGHIT and a user step routine              11
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   11
      INTEGER IHAVEMORE                                                   11
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    11
      COMMON /AGCSTEPH/ HITS(15)                                          11
      COMMON /AGCSTEPI/ IHAVEMORE                                         11
      CHARACTER CMED*13                                                   11
*      check ISVOL>0
*      CALL UHTOC(NATMED,4,Cmed,8)
*     if (istop==2) write (*,*) '***',Cmed,isvol,Ipart,AdEstep,istop,destep
*      if(Cmed=='FWOL_PhotCath') then
*       if(Ipart!=50) then
*          {Step,dEstep,aStep,AdEstep} = 0
*         endif
*      else if(Cmed=='FWOL_FLGR') then
*      endif
      RETURN                                                              23
      END                                                                 24
      SUBROUTINE FPCTSTEP(JJ,HIT)                                         26
* define hit element USER as # of cherenkov photon detected
      IMPLICIT NONE                                                       28
C - Dynamic core for the GEANT data (v3.15)                               28
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   28
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        28
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  28
     *LQ,JCG                                                              28
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       28
      PARAMETER (KWBANK=69000)                                            28
      PARAMETER (KWWORK=5200)                                             28
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           28
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      28
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      28
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    28
     *JGPAR,JGPAR2,JSKLT                                                  28
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                28
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   28
     *JGSTAT)                                                             28
C - Natural constants (or considered as such)                             28
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   28
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        28
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       28
C - GEANT description of logical units                                    28
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            28
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      28
      COMMON/GCMAIL/ CHMAIL                                               28
      CHARACTER*132 CHMAIL                                                28
C - track parameters at current point                                     28
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    28
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               28
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  28
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           28
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           28
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      28
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       28
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     28
C - Kinematics of current track                                           28
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           28
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            28
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       28
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               28
C - Identification of the current sensitive detector                      28
      INTEGER IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV                   28
      COMMON /GCSETS/ IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)       28
C Connection between AgGSTEP, AgGHIT and a user step routine              28
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   28
      INTEGER IHAVEMORE                                                   28
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    28
      COMMON /AGCSTEPH/ HITS(15)                                          28
      COMMON /AGCSTEPI/ IHAVEMORE                                         28
      INTEGER JJ                                                          28
      REAL HIT                                                            30
      IF (IPART .EQ. 50 .AND. ISTOP .EQ.2 .AND. DESTEP.GT.0.0) THEN       31
      HIT=1.0                                                             32
      RETURN                                                              33
      ENDIF                                                               34
      HIT = 0.0                                                           35
      RETURN                                                              36
      END                                                                 37
