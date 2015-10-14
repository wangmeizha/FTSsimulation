C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: fpdmgeo.g,v 1.12 2011/02/28 16:20:01 jwebb Exp $
* $Name:  $
* $Log: fpdmgeo.g,v $
* Revision 1.12  2011/02/28 16:20:01  jwebb
* Switched from fortran-style to mortran-style continuation lines.
*
* Revision 1.11  2010/02/05 21:26:59  jwebb
* All materials are now defined locally, so that changes to medium properties
* do not affect other detectors.
*
* Revision 1.10  2009/11/10 02:14:30  perev
* Where GSTPAR, set local material avoid bug in gphysi
*
* Revision 1.9  2009/08/27 03:20:42  perev
* Stra_Air added
*
* Revision 1.8  2009/08/19 22:11:05  perev
* Jason. Remove redundant Isvol=0
*
* Revision 1.7  2009/08/18 17:28:02  perev
* J.Webb Air is not an Iron fix
*
* Revision 1.6  2009/08/14 22:40:26  perev
* Fix BIRK3 constant
*
* Revision 1.5  2008/11/19 04:08:21  perev
*  updates to the corrected(vp) starsim
*
* Revision 1.4  2004/03/31 16:34:56  potekhin
* Placed fppdstep and another subroutine
* into a separate file due to code reorganization.
*
* Revision 1.3  2002/11/03 02:15:41  nevski
* clean-up parameters
*
* Revision 1.2  2002/11/02 19:37:01  nevski
* reduce mother, loop over fpd assembly
*
*
* Revision 1.14  2001/09/13 18:15:13  yqwang
*
* Endcap: West ON, Fpd: East ON, Lgd: East ON
*
* Revision 1.13  2001/08/29 22:44:54  yqwang
* Rotate Top and South Lgd, so their numberings agree with the actual
* labels.
*
* Revision 1.12  2001/08/23 12:15:41  yqwang
* Each (West (1) , East (2)) Endcap (Fpd) has possible
*                            XOffset, YOffset, and ZOffset.
* Each Lgd has its own ZPoz and XDis, YDis.
* Update Elgg_DGap value
*
* Revision 1.11  2001/08/20 21:17:41  yqwang
* South Lgd is tilted, but not the top and bottom
*
* Revision 1.10  2001/04/09 14:20:24  akio
* set default to de, not cherenkov, for fpd
*
* Revision 1.9  2001/04/06 18:12:20  akio
* Include the first version of FPD
*
******************************************************************************
      SUBROUTINE FPDMGEO                                                  64
C FPDMGEO IS THE FORWARD PION DETECTOR MODULES GEOMETRY                   64
      IMPLICIT NONE                                                       64
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       64
      DATA MODULE/'FPDMGEO'/, BANK_TITLE/'*'/                             64
      DATA MCOMMENT/'IS THE FORWARD PION DETECTOR MODULES GEOMETRY'/      64
      LOGICAL FIRST                                                       64
      DATA FIRST/.TRUE./                                                  64
      SAVE FIRST                                                          64
      INTEGER JUMPAD                                                      64
      EXTERNAL JUMPAD                                                     64
      INTEGER OK                                                          64
      PARAMETER (OK=0)                                                    64
      INTEGER AG_I,AG_J                                                   64
      SAVE AG_I,AG_J                                                      64
C CREATED 06 JAN 2002                                                     64
      DATA AG_I/0/,AG_J/0/                                                65
      CHARACTER*40 CREATED /'06 JAN 2002'/                                65
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  65
C AUTHOR YIQUN WANG                                                       65
      CHARACTER*40 AUTHOR /'YIQUN WANG'/                                  66
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  66
*
* Version 4.1, O. Akio                                        23 Jan 01
*               - Include forward pion detectors
*       - FPDG_ChkvSim to do either cherenkov photon simulation
*                 or just take de above thresholds.
* Version 4.2, Yiqun Wang                                  31 July 01
*               - Update FPD Pb-glass geometry
*
* Version 5.0, Yiqun Wang
*       - Put FPD (with LGD) in one volume FPDM, as suggested by Pavel Nevski
*
* Version 5.1, Yiqun Wang
*               - Put FPD (with LGD) in FPDM or FPDE, depending on FPDG_Onoff switch
*
******************************************************************************
C - Advanced Geant interface                                              82
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      82
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       82
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         82
     *AG_OPTION,AG_ORT,AG_MARK                                            82
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         82
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     82
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     82
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    82
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          82
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   82
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     82
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        82
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   82
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 82
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     82
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       82
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        82
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       82
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       82
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       82
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         82
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     82
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       82
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   82
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       82
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     82
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          82
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         82
     *-696969,AG_LDETU=250)                                               82
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    82
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  82
     *AG_IMFLD,AG_ISIMU                                                   82
C Inherited variables a: saved during internal calls                      82
C b: reset at the entry in a block and then saved                         82
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        82
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       82
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         82
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     82
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       82
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         82
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           82
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        82
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        82
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            82
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           82
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    82
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      82
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           82
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          82
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                82
C local variables valid inside same block                                 82
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       82
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  82
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         82
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    82
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          82
     *AG_OPTION                                                           82
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        82
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             82
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              82
C temporarly                                                              82
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           82
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    82
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       82
     *AG_HY)                                                              82
      REAL MIL_P,INCH_P                                                   82
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               82
C - Natural constants (or considered as such)                             82
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   82
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        82
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       82
C - GEANT description of logical units                                    82
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            82
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      82
      COMMON/GCMAIL/ CHMAIL                                               82
      CHARACTER*132 CHMAIL                                                82
*
      INTEGER BLOCK_FPDM                                                  86
      INTEGER BLOCK_FPDE                                                  86
      INTEGER BLOCK_FWOL                                                  86
      INTEGER BLOCK_FCVO                                                  86
      INTEGER BLOCK_FMDI                                                  86
      INTEGER BLOCK_FMSS                                                  86
      INTEGER BLOCK_FAGA                                                  86
      INTEGER BLOCK_FTOW                                                  86
      INTEGER BLOCK_FPSE                                                  86
      INTEGER BLOCK_FPER                                                  86
      INTEGER BLOCK_FFLP                                                  86
      INTEGER BLOCK_FXFP                                                  86
      INTEGER BLOCK_FSHM                                                  86
      INTEGER BLOCK_FSSP                                                  86
      INTEGER BLOCK_FTAR                                                  86
      INTEGER BLOCK_FGTN                                                  86
      INTEGER BLOCK_FSCI                                                  86
      INTEGER BLOCK_FLED                                                  86
      INTEGER BLOCK_FMOD                                                  86
      INTEGER BLOCK_FXGT                                                  86
      INTEGER BLOCK_FXSG                                                  86
      INTEGER BLOCK_FALP                                                  86
      INTEGER BLOCK_FHMS                                                  86
      INTEGER BLOCK_FLGD                                                  86
      INTEGER BLOCK_FLGT                                                  86
      INTEGER BLOCK_FWAL                                                  86
      INTEGER BLOCK_FLGR                                                  86
      INTEGER BLOCK_FPRB                                                  86
      INTEGER BLOCK_FPCT                                                  86
      INTEGER BLOCK_FUMT                                                  86
*
C -                                                                       88
      REAL FPDG_VERSION                                                   88
      EQUIVALENCE (BANK_FPDG(3),FPDG_VERSION,RE_FPDG_VERSION),            88
     *(NAMES_FPDG(1),NFPDG_VERSION)                                       88
      EQUIVALENCE (COMMEN_FPDG(1),CFPDG_VERSION),(MAP_FPDG(1,1),          88
     *MFPDG_VERSION)                                                      88
      CHARACTER CFPDG_VERSION*40/' '/                                     88
      CHARACTER NFPDG_VERSION*12/'R.VERSION'/                             88
      INTEGER MFPDG_VERSION(2)/1,1/                                       88
      REAL RE_FPDG_VERSION                                                88
C -                                                                       88
      REAL FPDG_ONOFF                                                     88
      EQUIVALENCE (BANK_FPDG(4),FPDG_ONOFF,RE_FPDG_ONOFF),                88
     *(NAMES_FPDG(2),NFPDG_ONOFF)                                         88
      EQUIVALENCE (COMMEN_FPDG(2),CFPDG_ONOFF),(MAP_FPDG(1,2),            88
     *MFPDG_ONOFF)                                                        88
      CHARACTER CFPDG_ONOFF*40/' '/                                       88
      CHARACTER NFPDG_ONOFF*12/'R.ONOFF'/                                 88
      INTEGER MFPDG_ONOFF(2)/1,1/                                         88
      REAL RE_FPDG_ONOFF                                                  88
C -                                                                       88
      REAL FPDG_CHKVSIM                                                   88
      EQUIVALENCE (BANK_FPDG(5),FPDG_CHKVSIM,RE_FPDG_CHKVSIM),            88
     *(NAMES_FPDG(3),NFPDG_CHKVSIM)                                       88
      EQUIVALENCE (COMMEN_FPDG(3),CFPDG_CHKVSIM),(MAP_FPDG(1,3),          88
     *MFPDG_CHKVSIM)                                                      88
      CHARACTER CFPDG_CHKVSIM*40/' '/                                     88
      CHARACTER NFPDG_CHKVSIM*12/'R.CHKVSIM'/                             88
      INTEGER MFPDG_CHKVSIM(2)/1,1/                                       88
      REAL RE_FPDG_CHKVSIM                                                88
C -                                                                       88
      REAL FPDG_ZDIS                                                      88
      EQUIVALENCE (BANK_FPDG(6),FPDG_ZDIS,RE_FPDG_ZDIS),(NAMES_FPDG(4),   88
     *NFPDG_ZDIS)                                                         88
      EQUIVALENCE (COMMEN_FPDG(4),CFPDG_ZDIS),(MAP_FPDG(1,4),             88
     *MFPDG_ZDIS)                                                         88
      CHARACTER CFPDG_ZDIS*40/' '/                                        88
      CHARACTER NFPDG_ZDIS*12/'R.ZDIS'/                                   88
      INTEGER MFPDG_ZDIS(2)/1,1/                                          88
      REAL RE_FPDG_ZDIS                                                   88
C -                                                                       88
      REAL FPDG_ZLEN                                                      88
      EQUIVALENCE (BANK_FPDG(7),FPDG_ZLEN,RE_FPDG_ZLEN),(NAMES_FPDG(5),   88
     *NFPDG_ZLEN)                                                         88
      EQUIVALENCE (COMMEN_FPDG(5),CFPDG_ZLEN),(MAP_FPDG(1,5),             88
     *MFPDG_ZLEN)                                                         88
      CHARACTER CFPDG_ZLEN*40/' '/                                        88
      CHARACTER NFPDG_ZLEN*12/'R.ZLEN'/                                   88
      INTEGER MFPDG_ZLEN(2)/1,1/                                          88
      REAL RE_FPDG_ZLEN                                                   88
C -                                                                       88
      REAL FPDG_RADIUS                                                    88
      EQUIVALENCE (BANK_FPDG(8),FPDG_RADIUS,RE_FPDG_RADIUS),              88
     *(NAMES_FPDG(6),NFPDG_RADIUS)                                        88
      EQUIVALENCE (COMMEN_FPDG(6),CFPDG_RADIUS),(MAP_FPDG(1,6),           88
     *MFPDG_RADIUS)                                                       88
      CHARACTER CFPDG_RADIUS*40/' '/                                      88
      CHARACTER NFPDG_RADIUS*12/'R.RADIUS'/                               88
      INTEGER MFPDG_RADIUS(2)/1,1/                                        88
      REAL RE_FPDG_RADIUS                                                 88
C -                                                                       88
      REAL FPDG_RMIN                                                      88
      EQUIVALENCE (BANK_FPDG(9),FPDG_RMIN,RE_FPDG_RMIN),(NAMES_FPDG(7),   88
     *NFPDG_RMIN)                                                         88
      EQUIVALENCE (COMMEN_FPDG(7),CFPDG_RMIN),(MAP_FPDG(1,7),             88
     *MFPDG_RMIN)                                                         88
      CHARACTER CFPDG_RMIN*40/' '/                                        88
      CHARACTER NFPDG_RMIN*12/'R.RMIN'/                                   88
      INTEGER MFPDG_RMIN(2)/1,1/                                          88
      REAL RE_FPDG_RMIN                                                   88
C -                                                                       88
      INTEGER LEN_FPDG,LLEN_FPDG,LINK_FPDG                                88
      PARAMETER (LEN_FPDG=9)                                              88
      PARAMETER (LLEN_FPDG=7)                                             88
      INTEGER FLAG_FPDG/0/,MAP_FPDG(2,LLEN_FPDG),IDNUM_FPDG(10)           88
      CHARACTER NAMES_FPDG(LLEN_FPDG)*12,COMMEN_FPDG(LLEN_FPDG)*40        88
      DATA IDNUM_FPDG/10*0/                                               88
      REAL BANK_FPDG(LEN_FPDG)                                            88
      CHARACTER*60 BPATH_FPDG/' '/                                        88
      SAVE FLAG_FPDG,MAP_FPDG,IDNUM_FPDG,NAMES_FPDG,COMMEN_FPDG,          88
     *BPATH_FPDG                                                          88
      DATA LINK_FPDG/0/,BANK_FPDG/LEN_FPDG*0/                             88
C *                                                                       88
C -                                                                       92
      INTEGER FMCS_TYPE                                                   92
      EQUIVALENCE (BANK_FMCS(3),FMCS_TYPE,RE_FMCS_TYPE),(NAMES_FMCS(1),   92
     *NFMCS_TYPE)                                                         92
      EQUIVALENCE (COMMEN_FMCS(1),CFMCS_TYPE),(MAP_FMCS(1,1),             92
     *MFMCS_TYPE)                                                         92
      CHARACTER CFMCS_TYPE*40/' '/                                        92
      CHARACTER NFMCS_TYPE*12/'I.TYPE'/                                   92
      INTEGER MFMCS_TYPE(2)/1,1/                                          92
      REAL RE_FMCS_TYPE                                                   92
C -                                                                       92
      REAL FMCS_ZORIG                                                     92
      EQUIVALENCE (BANK_FMCS(4),FMCS_ZORIG,RE_FMCS_ZORIG),                92
     *(NAMES_FMCS(2),NFMCS_ZORIG)                                         92
      EQUIVALENCE (COMMEN_FMCS(2),CFMCS_ZORIG),(MAP_FMCS(1,2),            92
     *MFMCS_ZORIG)                                                        92
      CHARACTER CFMCS_ZORIG*40/' '/                                       92
      CHARACTER NFMCS_ZORIG*12/'R.ZORIG'/                                 92
      INTEGER MFMCS_ZORIG(2)/1,1/                                         92
      REAL RE_FMCS_ZORIG                                                  92
C -                                                                       92
      REAL FMCS_ZEND                                                      92
      EQUIVALENCE (BANK_FMCS(5),FMCS_ZEND,RE_FMCS_ZEND),(NAMES_FMCS(3),   92
     *NFMCS_ZEND)                                                         92
      EQUIVALENCE (COMMEN_FMCS(3),CFMCS_ZEND),(MAP_FMCS(1,3),             92
     *MFMCS_ZEND)                                                         92
      CHARACTER CFMCS_ZEND*40/' '/                                        92
      CHARACTER NFMCS_ZEND*12/'R.ZEND'/                                   92
      INTEGER MFMCS_ZEND(2)/1,1/                                          92
      REAL RE_FMCS_ZEND                                                   92
C -                                                                       92
      REAL FMCS_ETAMIN                                                    92
      EQUIVALENCE (BANK_FMCS(6),FMCS_ETAMIN,RE_FMCS_ETAMIN),              92
     *(NAMES_FMCS(4),NFMCS_ETAMIN)                                        92
      EQUIVALENCE (COMMEN_FMCS(4),CFMCS_ETAMIN),(MAP_FMCS(1,4),           92
     *MFMCS_ETAMIN)                                                       92
      CHARACTER CFMCS_ETAMIN*40/' '/                                      92
      CHARACTER NFMCS_ETAMIN*12/'R.ETAMIN'/                               92
      INTEGER MFMCS_ETAMIN(2)/1,1/                                        92
      REAL RE_FMCS_ETAMIN                                                 92
C -                                                                       92
      REAL FMCS_ETAMAX                                                    92
      EQUIVALENCE (BANK_FMCS(7),FMCS_ETAMAX,RE_FMCS_ETAMAX),              92
     *(NAMES_FMCS(5),NFMCS_ETAMAX)                                        92
      EQUIVALENCE (COMMEN_FMCS(5),CFMCS_ETAMAX),(MAP_FMCS(1,5),           92
     *MFMCS_ETAMAX)                                                       92
      CHARACTER CFMCS_ETAMAX*40/' '/                                      92
      CHARACTER NFMCS_ETAMAX*12/'R.ETAMAX'/                               92
      INTEGER MFMCS_ETAMAX(2)/1,1/                                        92
      REAL RE_FMCS_ETAMAX                                                 92
C -                                                                       92
      REAL FMCS_PHIMIN                                                    92
      EQUIVALENCE (BANK_FMCS(8),FMCS_PHIMIN,RE_FMCS_PHIMIN),              92
     *(NAMES_FMCS(6),NFMCS_PHIMIN)                                        92
      EQUIVALENCE (COMMEN_FMCS(6),CFMCS_PHIMIN),(MAP_FMCS(1,6),           92
     *MFMCS_PHIMIN)                                                       92
      CHARACTER CFMCS_PHIMIN*40/' '/                                      92
      CHARACTER NFMCS_PHIMIN*12/'R.PHIMIN'/                               92
      INTEGER MFMCS_PHIMIN(2)/1,1/                                        92
      REAL RE_FMCS_PHIMIN                                                 92
C -                                                                       92
      REAL FMCS_PHIMAX                                                    92
      EQUIVALENCE (BANK_FMCS(9),FMCS_PHIMAX,RE_FMCS_PHIMAX),              92
     *(NAMES_FMCS(7),NFMCS_PHIMAX)                                        92
      EQUIVALENCE (COMMEN_FMCS(7),CFMCS_PHIMAX),(MAP_FMCS(1,7),           92
     *MFMCS_PHIMAX)                                                       92
      CHARACTER CFMCS_PHIMAX*40/' '/                                      92
      CHARACTER NFMCS_PHIMAX*12/'R.PHIMAX'/                               92
      INTEGER MFMCS_PHIMAX(2)/1,1/                                        92
      REAL RE_FMCS_PHIMAX                                                 92
C -                                                                       92
      REAL FMCS_NSUPSEC                                                   92
      EQUIVALENCE (BANK_FMCS(10),FMCS_NSUPSEC,RE_FMCS_NSUPSEC),           92
     *(NAMES_FMCS(8),NFMCS_NSUPSEC)                                       92
      EQUIVALENCE (COMMEN_FMCS(8),CFMCS_NSUPSEC),(MAP_FMCS(1,8),          92
     *MFMCS_NSUPSEC)                                                      92
      CHARACTER CFMCS_NSUPSEC*40/' '/                                     92
      CHARACTER NFMCS_NSUPSEC*12/'R.NSUPSEC'/                             92
      INTEGER MFMCS_NSUPSEC(2)/1,1/                                       92
      REAL RE_FMCS_NSUPSEC                                                92
C -                                                                       92
      REAL FMCS_NSECTOR                                                   92
      EQUIVALENCE (BANK_FMCS(11),FMCS_NSECTOR,RE_FMCS_NSECTOR),           92
     *(NAMES_FMCS(9),NFMCS_NSECTOR)                                       92
      EQUIVALENCE (COMMEN_FMCS(9),CFMCS_NSECTOR),(MAP_FMCS(1,9),          92
     *MFMCS_NSECTOR)                                                      92
      CHARACTER CFMCS_NSECTOR*40/' '/                                     92
      CHARACTER NFMCS_NSECTOR*12/'R.NSECTOR'/                             92
      INTEGER MFMCS_NSECTOR(2)/1,1/                                       92
      REAL RE_FMCS_NSECTOR                                                92
C -                                                                       92
      REAL FMCS_NSECTION                                                  92
      EQUIVALENCE (BANK_FMCS(12),FMCS_NSECTION,RE_FMCS_NSECTION),         92
     *(NAMES_FMCS(10),NFMCS_NSECTION)                                     92
      EQUIVALENCE (COMMEN_FMCS(10),CFMCS_NSECTION),(MAP_FMCS(1,10),       92
     *MFMCS_NSECTION)                                                     92
      CHARACTER CFMCS_NSECTION*40/' '/                                    92
      CHARACTER NFMCS_NSECTION*12/'R.NSECTION'/                           92
      INTEGER MFMCS_NSECTION(2)/1,1/                                      92
      REAL RE_FMCS_NSECTION                                               92
C -                                                                       92
      REAL FMCS_NSLICES                                                   92
      EQUIVALENCE (BANK_FMCS(13),FMCS_NSLICES,RE_FMCS_NSLICES),           92
     *(NAMES_FMCS(11),NFMCS_NSLICES)                                      92
      EQUIVALENCE (COMMEN_FMCS(11),CFMCS_NSLICES),(MAP_FMCS(1,11),        92
     *MFMCS_NSLICES)                                                      92
      CHARACTER CFMCS_NSLICES*40/' '/                                     92
      CHARACTER NFMCS_NSLICES*12/'R.NSLICES'/                             92
      INTEGER MFMCS_NSLICES(2)/1,1/                                       92
      REAL RE_FMCS_NSLICES                                                92
C -                                                                       92
      REAL FMCS_FRONT                                                     92
      EQUIVALENCE (BANK_FMCS(14),FMCS_FRONT,RE_FMCS_FRONT),               92
     *(NAMES_FMCS(12),NFMCS_FRONT)                                        92
      EQUIVALENCE (COMMEN_FMCS(12),CFMCS_FRONT),(MAP_FMCS(1,12),          92
     *MFMCS_FRONT)                                                        92
      CHARACTER CFMCS_FRONT*40/' '/                                       92
      CHARACTER NFMCS_FRONT*12/'R.FRONT'/                                 92
      INTEGER MFMCS_FRONT(2)/1,1/                                         92
      REAL RE_FMCS_FRONT                                                  92
C -                                                                       92
      REAL FMCS_GTEN                                                      92
      EQUIVALENCE (BANK_FMCS(15),FMCS_GTEN,RE_FMCS_GTEN),                 92
     *(NAMES_FMCS(13),NFMCS_GTEN)                                         92
      EQUIVALENCE (COMMEN_FMCS(13),CFMCS_GTEN),(MAP_FMCS(1,13),           92
     *MFMCS_GTEN)                                                         92
      CHARACTER CFMCS_GTEN*40/' '/                                        92
      CHARACTER NFMCS_GTEN*12/'R.GTEN'/                                   92
      INTEGER MFMCS_GTEN(2)/1,1/                                          92
      REAL RE_FMCS_GTEN                                                   92
C -                                                                       92
      REAL FMCS_PLATE                                                     92
      EQUIVALENCE (BANK_FMCS(16),FMCS_PLATE,RE_FMCS_PLATE),               92
     *(NAMES_FMCS(14),NFMCS_PLATE)                                        92
      EQUIVALENCE (COMMEN_FMCS(14),CFMCS_PLATE),(MAP_FMCS(1,14),          92
     *MFMCS_PLATE)                                                        92
      CHARACTER CFMCS_PLATE*40/' '/                                       92
      CHARACTER NFMCS_PLATE*12/'R.PLATE'/                                 92
      INTEGER MFMCS_PLATE(2)/1,1/                                         92
      REAL RE_FMCS_PLATE                                                  92
C -                                                                       92
      REAL FMCS_PLATES                                                    92
      EQUIVALENCE (BANK_FMCS(17),FMCS_PLATES,RE_FMCS_PLATES),             92
     *(NAMES_FMCS(15),NFMCS_PLATES)                                       92
      EQUIVALENCE (COMMEN_FMCS(15),CFMCS_PLATES),(MAP_FMCS(1,15),         92
     *MFMCS_PLATES)                                                       92
      CHARACTER CFMCS_PLATES*40/' '/                                      92
      CHARACTER NFMCS_PLATES*12/'R.PLATES'/                               92
      INTEGER MFMCS_PLATES(2)/1,1/                                        92
      REAL RE_FMCS_PLATES                                                 92
C -                                                                       92
      REAL FMCS_PLATEB                                                    92
      EQUIVALENCE (BANK_FMCS(18),FMCS_PLATEB,RE_FMCS_PLATEB),             92
     *(NAMES_FMCS(16),NFMCS_PLATEB)                                       92
      EQUIVALENCE (COMMEN_FMCS(16),CFMCS_PLATEB),(MAP_FMCS(1,16),         92
     *MFMCS_PLATEB)                                                       92
      CHARACTER CFMCS_PLATEB*40/' '/                                      92
      CHARACTER NFMCS_PLATEB*12/'R.PLATEB'/                               92
      INTEGER MFMCS_PLATEB(2)/1,1/                                        92
      REAL RE_FMCS_PLATEB                                                 92
C -                                                                       92
      REAL FMCS_HUB                                                       92
      EQUIVALENCE (BANK_FMCS(19),FMCS_HUB,RE_FMCS_HUB),(NAMES_FMCS(17),   92
     *NFMCS_HUB)                                                          92
      EQUIVALENCE (COMMEN_FMCS(17),CFMCS_HUB),(MAP_FMCS(1,17),            92
     *MFMCS_HUB)                                                          92
      CHARACTER CFMCS_HUB*40/' '/                                         92
      CHARACTER NFMCS_HUB*12/'R.HUB'/                                     92
      INTEGER MFMCS_HUB(2)/1,1/                                           92
      REAL RE_FMCS_HUB                                                    92
C -                                                                       92
      REAL FMCS_RMSHIFT                                                   92
      EQUIVALENCE (BANK_FMCS(20),FMCS_RMSHIFT,RE_FMCS_RMSHIFT),           92
     *(NAMES_FMCS(18),NFMCS_RMSHIFT)                                      92
      EQUIVALENCE (COMMEN_FMCS(18),CFMCS_RMSHIFT),(MAP_FMCS(1,18),        92
     *MFMCS_RMSHIFT)                                                      92
      CHARACTER CFMCS_RMSHIFT*40/' '/                                     92
      CHARACTER NFMCS_RMSHIFT*12/'R.RMSHIFT'/                             92
      INTEGER MFMCS_RMSHIFT(2)/1,1/                                       92
      REAL RE_FMCS_RMSHIFT                                                92
C -                                                                       92
      REAL FMCS_SMSHIFT                                                   92
      EQUIVALENCE (BANK_FMCS(21),FMCS_SMSHIFT,RE_FMCS_SMSHIFT),           92
     *(NAMES_FMCS(19),NFMCS_SMSHIFT)                                      92
      EQUIVALENCE (COMMEN_FMCS(19),CFMCS_SMSHIFT),(MAP_FMCS(1,19),        92
     *MFMCS_SMSHIFT)                                                      92
      CHARACTER CFMCS_SMSHIFT*40/' '/                                     92
      CHARACTER NFMCS_SMSHIFT*12/'R.SMSHIFT'/                             92
      INTEGER MFMCS_SMSHIFT(2)/1,1/                                       92
      REAL RE_FMCS_SMSHIFT                                                92
C -                                                                       92
      REAL FMCS_GAPPLT                                                    92
      EQUIVALENCE (BANK_FMCS(22),FMCS_GAPPLT,RE_FMCS_GAPPLT),             92
     *(NAMES_FMCS(20),NFMCS_GAPPLT)                                       92
      EQUIVALENCE (COMMEN_FMCS(20),CFMCS_GAPPLT),(MAP_FMCS(1,20),         92
     *MFMCS_GAPPLT)                                                       92
      CHARACTER CFMCS_GAPPLT*40/' '/                                      92
      CHARACTER NFMCS_GAPPLT*12/'R.GAPPLT'/                               92
      INTEGER MFMCS_GAPPLT(2)/1,1/                                        92
      REAL RE_FMCS_GAPPLT                                                 92
C -                                                                       92
      REAL FMCS_GAPCEL                                                    92
      EQUIVALENCE (BANK_FMCS(23),FMCS_GAPCEL,RE_FMCS_GAPCEL),             92
     *(NAMES_FMCS(21),NFMCS_GAPCEL)                                       92
      EQUIVALENCE (COMMEN_FMCS(21),CFMCS_GAPCEL),(MAP_FMCS(1,21),         92
     *MFMCS_GAPCEL)                                                       92
      CHARACTER CFMCS_GAPCEL*40/' '/                                      92
      CHARACTER NFMCS_GAPCEL*12/'R.GAPCEL'/                               92
      INTEGER MFMCS_GAPCEL(2)/1,1/                                        92
      REAL RE_FMCS_GAPCEL                                                 92
C -                                                                       92
      REAL FMCS_GAPSMD                                                    92
      EQUIVALENCE (BANK_FMCS(24),FMCS_GAPSMD,RE_FMCS_GAPSMD),             92
     *(NAMES_FMCS(22),NFMCS_GAPSMD)                                       92
      EQUIVALENCE (COMMEN_FMCS(22),CFMCS_GAPSMD),(MAP_FMCS(1,22),         92
     *MFMCS_GAPSMD)                                                       92
      CHARACTER CFMCS_GAPSMD*40/' '/                                      92
      CHARACTER NFMCS_GAPSMD*12/'R.GAPSMD'/                               92
      INTEGER MFMCS_GAPSMD(2)/1,1/                                        92
      REAL RE_FMCS_GAPSMD                                                 92
C -                                                                       92
      INTEGER LEN_FMCS,LLEN_FMCS,LINK_FMCS                                92
      PARAMETER (LEN_FMCS=24)                                             92
      PARAMETER (LLEN_FMCS=22)                                            92
      INTEGER FLAG_FMCS/0/,MAP_FMCS(2,LLEN_FMCS),IDNUM_FMCS(10)           92
      CHARACTER NAMES_FMCS(LLEN_FMCS)*12,COMMEN_FMCS(LLEN_FMCS)*40        92
      DATA IDNUM_FMCS/10*0/                                               92
      REAL BANK_FMCS(LEN_FMCS)                                            92
      CHARACTER*60 BPATH_FMCS/' '/                                        92
      SAVE FLAG_FMCS,MAP_FMCS,IDNUM_FMCS,NAMES_FMCS,COMMEN_FMCS,          92
     *BPATH_FMCS                                                          92
      DATA LINK_FMCS/0/,BANK_FMCS/LEN_FMCS*0/                             92
C *                                                                       92
*
C -                                                                       94
      INTEGER FMCC_IPART                                                  94
      EQUIVALENCE (BANK_FMCC(3),FMCC_IPART,RE_FMCC_IPART),                94
     *(NAMES_FMCC(1),NFMCC_IPART)                                         94
      EQUIVALENCE (COMMEN_FMCC(1),CFMCC_IPART),(MAP_FMCC(1,1),            94
     *MFMCC_IPART)                                                        94
      CHARACTER CFMCC_IPART*40/' '/                                       94
      CHARACTER NFMCC_IPART*12/'I.IPART'/                                 94
      INTEGER MFMCC_IPART(2)/1,1/                                         94
      REAL RE_FMCC_IPART                                                  94
C -                                                                       94
      REAL FMCC_ZORIG                                                     94
      EQUIVALENCE (BANK_FMCC(4),FMCC_ZORIG,RE_FMCC_ZORIG),                94
     *(NAMES_FMCC(2),NFMCC_ZORIG)                                         94
      EQUIVALENCE (COMMEN_FMCC(2),CFMCC_ZORIG),(MAP_FMCC(1,2),            94
     *MFMCC_ZORIG)                                                        94
      CHARACTER CFMCC_ZORIG*40/' '/                                       94
      CHARACTER NFMCC_ZORIG*12/'R.ZORIG'/                                 94
      INTEGER MFMCC_ZORIG(2)/1,1/                                         94
      REAL RE_FMCC_ZORIG                                                  94
C -                                                                       94
      REAL FMCC_ZEND                                                      94
      EQUIVALENCE (BANK_FMCC(5),FMCC_ZEND,RE_FMCC_ZEND),(NAMES_FMCC(3),   94
     *NFMCC_ZEND)                                                         94
      EQUIVALENCE (COMMEN_FMCC(3),CFMCC_ZEND),(MAP_FMCC(1,3),             94
     *MFMCC_ZEND)                                                         94
      CHARACTER CFMCC_ZEND*40/' '/                                        94
      CHARACTER NFMCC_ZEND*12/'R.ZEND'/                                   94
      INTEGER MFMCC_ZEND(2)/1,1/                                          94
      REAL RE_FMCC_ZEND                                                   94
C -                                                                       94
      REAL FMCC_XOFFSET                                                   94
      EQUIVALENCE (BANK_FMCC(6),FMCC_XOFFSET,RE_FMCC_XOFFSET),            94
     *(NAMES_FMCC(4),NFMCC_XOFFSET)                                       94
      EQUIVALENCE (COMMEN_FMCC(4),CFMCC_XOFFSET),(MAP_FMCC(1,4),          94
     *MFMCC_XOFFSET)                                                      94
      CHARACTER CFMCC_XOFFSET*40/' '/                                     94
      CHARACTER NFMCC_XOFFSET*12/'R.XOFFSET'/                             94
      INTEGER MFMCC_XOFFSET(2)/1,1/                                       94
      REAL RE_FMCC_XOFFSET                                                94
C -                                                                       94
      REAL FMCC_YOFFSET                                                   94
      EQUIVALENCE (BANK_FMCC(7),FMCC_YOFFSET,RE_FMCC_YOFFSET),            94
     *(NAMES_FMCC(5),NFMCC_YOFFSET)                                       94
      EQUIVALENCE (COMMEN_FMCC(5),CFMCC_YOFFSET),(MAP_FMCC(1,5),          94
     *MFMCC_YOFFSET)                                                      94
      CHARACTER CFMCC_YOFFSET*40/' '/                                     94
      CHARACTER NFMCC_YOFFSET*12/'R.YOFFSET'/                             94
      INTEGER MFMCC_YOFFSET(2)/1,1/                                       94
      REAL RE_FMCC_YOFFSET                                                94
C -                                                                       94
      REAL FMCC_ZOFFSET                                                   94
      EQUIVALENCE (BANK_FMCC(8),FMCC_ZOFFSET,RE_FMCC_ZOFFSET),            94
     *(NAMES_FMCC(6),NFMCC_ZOFFSET)                                       94
      EQUIVALENCE (COMMEN_FMCC(6),CFMCC_ZOFFSET),(MAP_FMCC(1,6),          94
     *MFMCC_ZOFFSET)                                                      94
      CHARACTER CFMCC_ZOFFSET*40/' '/                                     94
      CHARACTER NFMCC_ZOFFSET*12/'R.ZOFFSET'/                             94
      INTEGER MFMCC_ZOFFSET(2)/1,1/                                       94
      REAL RE_FMCC_ZOFFSET                                                94
C -                                                                       94
      INTEGER LEN_FMCC,LLEN_FMCC,LINK_FMCC                                94
      PARAMETER (LEN_FMCC=8)                                              94
      PARAMETER (LLEN_FMCC=6)                                             94
      INTEGER FLAG_FMCC/0/,MAP_FMCC(2,LLEN_FMCC),IDNUM_FMCC(10)           94
      CHARACTER NAMES_FMCC(LLEN_FMCC)*12,COMMEN_FMCC(LLEN_FMCC)*40        94
      DATA IDNUM_FMCC/10*0/                                               94
      REAL BANK_FMCC(LEN_FMCC)                                            94
      CHARACTER*60 BPATH_FMCC/' '/                                        94
      SAVE FLAG_FMCC,MAP_FMCC,IDNUM_FMCC,NAMES_FMCC,COMMEN_FMCC,          94
     *BPATH_FMCC                                                          94
      DATA LINK_FMCC/0/,BANK_FMCC/LEN_FMCC*0/                             94
C *                                                                       94
*
C -                                                                       96
      INTEGER FETR_TYPE                                                   96
      EQUIVALENCE (BANK_FETR(3),FETR_TYPE,RE_FETR_TYPE),(NAMES_FETR(1),   96
     *NFETR_TYPE)                                                         96
      EQUIVALENCE (COMMEN_FETR(1),CFETR_TYPE),(MAP_FETR(1,1),             96
     *MFETR_TYPE)                                                         96
      CHARACTER CFETR_TYPE*40/' '/                                        96
      CHARACTER NFETR_TYPE*12/'I.TYPE'/                                   96
      INTEGER MFETR_TYPE(2)/1,1/                                          96
      REAL RE_FETR_TYPE                                                   96
C -                                                                       96
      REAL FETR_ETAGR                                                     96
      EQUIVALENCE (BANK_FETR(4),FETR_ETAGR,RE_FETR_ETAGR),                96
     *(NAMES_FETR(2),NFETR_ETAGR)                                         96
      EQUIVALENCE (COMMEN_FETR(2),CFETR_ETAGR),(MAP_FETR(1,2),            96
     *MFETR_ETAGR)                                                        96
      CHARACTER CFETR_ETAGR*40/' '/                                       96
      CHARACTER NFETR_ETAGR*12/'R.ETAGR'/                                 96
      INTEGER MFETR_ETAGR(2)/1,1/                                         96
      REAL RE_FETR_ETAGR                                                  96
C -                                                                       96
      REAL FETR_PHIGR                                                     96
      EQUIVALENCE (BANK_FETR(5),FETR_PHIGR,RE_FETR_PHIGR),                96
     *(NAMES_FETR(3),NFETR_PHIGR)                                         96
      EQUIVALENCE (COMMEN_FETR(3),CFETR_PHIGR),(MAP_FETR(1,3),            96
     *MFETR_PHIGR)                                                        96
      CHARACTER CFETR_PHIGR*40/' '/                                       96
      CHARACTER NFETR_PHIGR*12/'R.PHIGR'/                                 96
      INTEGER MFETR_PHIGR(2)/1,1/                                         96
      REAL RE_FETR_PHIGR                                                  96
C -                                                                       96
      REAL FETR_NETA                                                      96
      EQUIVALENCE (BANK_FETR(6),FETR_NETA,RE_FETR_NETA),(NAMES_FETR(4),   96
     *NFETR_NETA)                                                         96
      EQUIVALENCE (COMMEN_FETR(4),CFETR_NETA),(MAP_FETR(1,4),             96
     *MFETR_NETA)                                                         96
      CHARACTER CFETR_NETA*40/' '/                                        96
      CHARACTER NFETR_NETA*12/'R.NETA'/                                   96
      INTEGER MFETR_NETA(2)/1,1/                                          96
      REAL RE_FETR_NETA                                                   96
C -                                                                       96
      REAL FETR_ETABIN(13)                                                96
      EQUIVALENCE (BANK_FETR(7),FETR_ETABIN,RE_FETR_ETABIN),              96
     *(NAMES_FETR(5),NFETR_ETABIN)                                        96
      EQUIVALENCE (COMMEN_FETR(5),CFETR_ETABIN),(MAP_FETR(1,5),           96
     *MFETR_ETABIN)                                                       96
      CHARACTER CFETR_ETABIN*40/' '/                                      96
      CHARACTER NFETR_ETABIN*12/'R.ETABIN'/                               96
      INTEGER MFETR_ETABIN(2)/13,1/                                       96
      REAL RE_FETR_ETABIN                                                 96
C -                                                                       96
      INTEGER LEN_FETR,LLEN_FETR,LINK_FETR                                96
      PARAMETER (LEN_FETR=19)                                             96
      PARAMETER (LLEN_FETR=5)                                             96
      INTEGER FLAG_FETR/0/,MAP_FETR(2,LLEN_FETR),IDNUM_FETR(10)           96
      CHARACTER NAMES_FETR(LLEN_FETR)*12,COMMEN_FETR(LLEN_FETR)*40        96
      DATA IDNUM_FETR/10*0/                                               96
      REAL BANK_FETR(LEN_FETR)                                            96
      CHARACTER*60 BPATH_FETR/' '/                                        96
      SAVE FLAG_FETR,MAP_FETR,IDNUM_FETR,NAMES_FETR,COMMEN_FETR,          96
     *BPATH_FETR                                                          96
      DATA LINK_FETR/0/,BANK_FETR/LEN_FETR*0/                             96
C *                                                                       96
*
C -                                                                       98
      REAL FSEC_ISECT                                                     98
      EQUIVALENCE (BANK_FSEC(3),FSEC_ISECT,RE_FSEC_ISECT),                98
     *(NAMES_FSEC(1),NFSEC_ISECT)                                         98
      EQUIVALENCE (COMMEN_FSEC(1),CFSEC_ISECT),(MAP_FSEC(1,1),            98
     *MFSEC_ISECT)                                                        98
      CHARACTER CFSEC_ISECT*40/' '/                                       98
      CHARACTER NFSEC_ISECT*12/'R.ISECT'/                                 98
      INTEGER MFSEC_ISECT(2)/1,1/                                         98
      REAL RE_FSEC_ISECT                                                  98
C -                                                                       98
      REAL FSEC_FPLMAT                                                    98
      EQUIVALENCE (BANK_FSEC(4),FSEC_FPLMAT,RE_FSEC_FPLMAT),              98
     *(NAMES_FSEC(2),NFSEC_FPLMAT)                                        98
      EQUIVALENCE (COMMEN_FSEC(2),CFSEC_FPLMAT),(MAP_FSEC(1,2),           98
     *MFSEC_FPLMAT)                                                       98
      CHARACTER CFSEC_FPLMAT*40/' '/                                      98
      CHARACTER NFSEC_FPLMAT*12/'R.FPLMAT'/                               98
      INTEGER MFSEC_FPLMAT(2)/1,1/                                        98
      REAL RE_FSEC_FPLMAT                                                 98
C -                                                                       98
      REAL FSEC_CELL                                                      98
      EQUIVALENCE (BANK_FSEC(5),FSEC_CELL,RE_FSEC_CELL),(NAMES_FSEC(3),   98
     *NFSEC_CELL)                                                         98
      EQUIVALENCE (COMMEN_FSEC(3),CFSEC_CELL),(MAP_FSEC(1,3),             98
     *MFSEC_CELL)                                                         98
      CHARACTER CFSEC_CELL*40/' '/                                        98
      CHARACTER NFSEC_CELL*12/'R.CELL'/                                   98
      INTEGER MFSEC_CELL(2)/1,1/                                          98
      REAL RE_FSEC_CELL                                                   98
C -                                                                       98
      REAL FSEC_SCINT(2)                                                  98
      EQUIVALENCE (BANK_FSEC(6),FSEC_SCINT,RE_FSEC_SCINT),                98
     *(NAMES_FSEC(4),NFSEC_SCINT)                                         98
      EQUIVALENCE (COMMEN_FSEC(4),CFSEC_SCINT),(MAP_FSEC(1,4),            98
     *MFSEC_SCINT)                                                        98
      CHARACTER CFSEC_SCINT*40/' '/                                       98
      CHARACTER NFSEC_SCINT*12/'R.SCINT'/                                 98
      INTEGER MFSEC_SCINT(2)/2,1/                                         98
      REAL RE_FSEC_SCINT                                                  98
C -                                                                       98
      REAL FSEC_NLAYER                                                    98
      EQUIVALENCE (BANK_FSEC(8),FSEC_NLAYER,RE_FSEC_NLAYER),              98
     *(NAMES_FSEC(5),NFSEC_NLAYER)                                        98
      EQUIVALENCE (COMMEN_FSEC(5),CFSEC_NLAYER),(MAP_FSEC(1,5),           98
     *MFSEC_NLAYER)                                                       98
      CHARACTER CFSEC_NLAYER*40/' '/                                      98
      CHARACTER NFSEC_NLAYER*12/'R.NLAYER'/                               98
      INTEGER MFSEC_NLAYER(2)/1,1/                                        98
      REAL RE_FSEC_NLAYER                                                 98
C -                                                                       98
      INTEGER LEN_FSEC,LLEN_FSEC,LINK_FSEC                                98
      PARAMETER (LEN_FSEC=8)                                              98
      PARAMETER (LLEN_FSEC=5)                                             98
      INTEGER FLAG_FSEC/0/,MAP_FSEC(2,LLEN_FSEC),IDNUM_FSEC(10)           98
      CHARACTER NAMES_FSEC(LLEN_FSEC)*12,COMMEN_FSEC(LLEN_FSEC)*40        98
      DATA IDNUM_FSEC/10*0/                                               98
      REAL BANK_FSEC(LEN_FSEC)                                            98
      CHARACTER*60 BPATH_FSEC/' '/                                        98
      SAVE FLAG_FSEC,MAP_FSEC,IDNUM_FSEC,NAMES_FSEC,COMMEN_FSEC,          98
     *BPATH_FSEC                                                          98
      DATA LINK_FSEC/0/,BANK_FSEC/LEN_FSEC*0/                             98
C *                                                                       98
*
C -                                                                       100
      REAL FMXG_VERSION                                                   100
      EQUIVALENCE (BANK_FMXG(3),FMXG_VERSION,RE_FMXG_VERSION),            100
     *(NAMES_FMXG(1),NFMXG_VERSION)                                       100
      EQUIVALENCE (COMMEN_FMXG(1),CFMXG_VERSION),(MAP_FMXG(1,1),          100
     *MFMXG_VERSION)                                                      100
      CHARACTER CFMXG_VERSION*40/' '/                                     100
      CHARACTER NFMXG_VERSION*12/'R.VERSION'/                             100
      INTEGER MFMXG_VERSION(2)/1,1/                                       100
      REAL RE_FMXG_VERSION                                                100
C -                                                                       100
      REAL FMXG_SAPEX                                                     100
      EQUIVALENCE (BANK_FMXG(4),FMXG_SAPEX,RE_FMXG_SAPEX),                100
     *(NAMES_FMXG(2),NFMXG_SAPEX)                                         100
      EQUIVALENCE (COMMEN_FMXG(2),CFMXG_SAPEX),(MAP_FMXG(1,2),            100
     *MFMXG_SAPEX)                                                        100
      CHARACTER CFMXG_SAPEX*40/' '/                                       100
      CHARACTER NFMXG_SAPEX*12/'R.SAPEX'/                                 100
      INTEGER MFMXG_SAPEX(2)/1,1/                                         100
      REAL RE_FMXG_SAPEX                                                  100
C -                                                                       100
      REAL FMXG_SBASE                                                     100
      EQUIVALENCE (BANK_FMXG(5),FMXG_SBASE,RE_FMXG_SBASE),                100
     *(NAMES_FMXG(3),NFMXG_SBASE)                                         100
      EQUIVALENCE (COMMEN_FMXG(3),CFMXG_SBASE),(MAP_FMXG(1,3),            100
     *MFMXG_SBASE)                                                        100
      CHARACTER CFMXG_SBASE*40/' '/                                       100
      CHARACTER NFMXG_SBASE*12/'R.SBASE'/                                 100
      INTEGER MFMXG_SBASE(2)/1,1/                                         100
      REAL RE_FMXG_SBASE                                                  100
C -                                                                       100
      INTEGER LEN_FMXG,LLEN_FMXG,LINK_FMXG                                100
      PARAMETER (LEN_FMXG=5)                                              100
      PARAMETER (LLEN_FMXG=3)                                             100
      INTEGER FLAG_FMXG/0/,MAP_FMXG(2,LLEN_FMXG),IDNUM_FMXG(10)           100
      CHARACTER NAMES_FMXG(LLEN_FMXG)*12,COMMEN_FMXG(LLEN_FMXG)*40        100
      DATA IDNUM_FMXG/10*0/                                               100
      REAL BANK_FMXG(LEN_FMXG)                                            100
      CHARACTER*60 BPATH_FMXG/' '/                                        100
      SAVE FLAG_FMXG,MAP_FMXG,IDNUM_FMXG,NAMES_FMXG,COMMEN_FMXG,          100
     *BPATH_FMXG                                                          100
      DATA LINK_FMXG/0/,BANK_FMXG/LEN_FMXG*0/                             100
C *                                                                       100
*
C -                                                                       102
      REAL FXSE_JSECT                                                     102
      EQUIVALENCE (BANK_FXSE(3),FXSE_JSECT,RE_FXSE_JSECT),                102
     *(NAMES_FXSE(1),NFXSE_JSECT)                                         102
      EQUIVALENCE (COMMEN_FXSE(1),CFXSE_JSECT),(MAP_FXSE(1,1),            102
     *MFXSE_JSECT)                                                        102
      CHARACTER CFXSE_JSECT*40/' '/                                       102
      CHARACTER NFXSE_JSECT*12/'R.JSECT'/                                 102
      INTEGER MFXSE_JSECT(2)/1,1/                                         102
      REAL RE_FXSE_JSECT                                                  102
C -                                                                       102
      REAL FXSE_SWIDTH                                                    102
      EQUIVALENCE (BANK_FXSE(4),FXSE_SWIDTH,RE_FXSE_SWIDTH),              102
     *(NAMES_FXSE(2),NFXSE_SWIDTH)                                        102
      EQUIVALENCE (COMMEN_FXSE(2),CFXSE_SWIDTH),(MAP_FXSE(1,2),           102
     *MFXSE_SWIDTH)                                                       102
      CHARACTER CFXSE_SWIDTH*40/' '/                                      102
      CHARACTER NFXSE_SWIDTH*12/'R.SWIDTH'/                               102
      INTEGER MFXSE_SWIDTH(2)/1,1/                                        102
      REAL RE_FXSE_SWIDTH                                                 102
C -                                                                       102
      REAL FXSE_APLATE                                                    102
      EQUIVALENCE (BANK_FXSE(5),FXSE_APLATE,RE_FXSE_APLATE),              102
     *(NAMES_FXSE(3),NFXSE_APLATE)                                        102
      EQUIVALENCE (COMMEN_FXSE(3),CFXSE_APLATE),(MAP_FXSE(1,3),           102
     *MFXSE_APLATE)                                                       102
      CHARACTER CFXSE_APLATE*40/' '/                                      102
      CHARACTER NFXSE_APLATE*12/'R.APLATE'/                               102
      INTEGER MFXSE_APLATE(2)/1,1/                                        102
      REAL RE_FXSE_APLATE                                                 102
C -                                                                       102
      INTEGER LEN_FXSE,LLEN_FXSE,LINK_FXSE                                102
      PARAMETER (LEN_FXSE=5)                                              102
      PARAMETER (LLEN_FXSE=3)                                             102
      INTEGER FLAG_FXSE/0/,MAP_FXSE(2,LLEN_FXSE),IDNUM_FXSE(10)           102
      CHARACTER NAMES_FXSE(LLEN_FXSE)*12,COMMEN_FXSE(LLEN_FXSE)*40        102
      DATA IDNUM_FXSE/10*0/                                               102
      REAL BANK_FXSE(LEN_FXSE)                                            102
      CHARACTER*60 BPATH_FXSE/' '/                                        102
      SAVE FLAG_FXSE,MAP_FXSE,IDNUM_FXSE,NAMES_FXSE,COMMEN_FXSE,          102
     *BPATH_FXSE                                                          102
      DATA LINK_FXSE/0/,BANK_FXSE/LEN_FXSE*0/                             102
C *                                                                       102
*
C -                                                                       105
      INTEGER FLGG_VERSION                                                105
      EQUIVALENCE (BANK_FLGG(3),FLGG_VERSION,RE_FLGG_VERSION),            105
     *(NAMES_FLGG(1),NFLGG_VERSION)                                       105
      EQUIVALENCE (COMMEN_FLGG(1),CFLGG_VERSION),(MAP_FLGG(1,1),          105
     *MFLGG_VERSION)                                                      105
      CHARACTER CFLGG_VERSION*40/' '/                                     105
      CHARACTER NFLGG_VERSION*12/'I.VERSION'/                             105
      INTEGER MFLGG_VERSION(2)/1,1/                                       105
      REAL RE_FLGG_VERSION                                                105
C -                                                                       105
      REAL FLGG_WIDTH                                                     105
      EQUIVALENCE (BANK_FLGG(4),FLGG_WIDTH,RE_FLGG_WIDTH),                105
     *(NAMES_FLGG(2),NFLGG_WIDTH)                                         105
      EQUIVALENCE (COMMEN_FLGG(2),CFLGG_WIDTH),(MAP_FLGG(1,2),            105
     *MFLGG_WIDTH)                                                        105
      CHARACTER CFLGG_WIDTH*40/' '/                                       105
      CHARACTER NFLGG_WIDTH*12/'R.WIDTH'/                                 105
      INTEGER MFLGG_WIDTH(2)/1,1/                                         105
      REAL RE_FLGG_WIDTH                                                  105
C -                                                                       105
      REAL FLGG_DEPTH                                                     105
      EQUIVALENCE (BANK_FLGG(5),FLGG_DEPTH,RE_FLGG_DEPTH),                105
     *(NAMES_FLGG(3),NFLGG_DEPTH)                                         105
      EQUIVALENCE (COMMEN_FLGG(3),CFLGG_DEPTH),(MAP_FLGG(1,3),            105
     *MFLGG_DEPTH)                                                        105
      CHARACTER CFLGG_DEPTH*40/' '/                                       105
      CHARACTER NFLGG_DEPTH*12/'R.DEPTH'/                                 105
      INTEGER MFLGG_DEPTH(2)/1,1/                                         105
      REAL RE_FLGG_DEPTH                                                  105
C -                                                                       105
      REAL FLGG_DGAP                                                      105
      EQUIVALENCE (BANK_FLGG(6),FLGG_DGAP,RE_FLGG_DGAP),(NAMES_FLGG(4),   105
     *NFLGG_DGAP)                                                         105
      EQUIVALENCE (COMMEN_FLGG(4),CFLGG_DGAP),(MAP_FLGG(1,4),             105
     *MFLGG_DGAP)                                                         105
      CHARACTER CFLGG_DGAP*40/' '/                                        105
      CHARACTER NFLGG_DGAP*12/'R.DGAP'/                                   105
      INTEGER MFLGG_DGAP(2)/1,1/                                          105
      REAL RE_FLGG_DGAP                                                   105
C -                                                                       105
      REAL FLGG_NPHI                                                      105
      EQUIVALENCE (BANK_FLGG(7),FLGG_NPHI,RE_FLGG_NPHI),(NAMES_FLGG(5),   105
     *NFLGG_NPHI)                                                         105
      EQUIVALENCE (COMMEN_FLGG(5),CFLGG_NPHI),(MAP_FLGG(1,5),             105
     *MFLGG_NPHI)                                                         105
      CHARACTER CFLGG_NPHI*40/' '/                                        105
      CHARACTER NFLGG_NPHI*12/'R.NPHI'/                                   105
      INTEGER MFLGG_NPHI(2)/1,1/                                          105
      REAL RE_FLGG_NPHI                                                   105
C -                                                                       105
      REAL FLGG_NETA                                                      105
      EQUIVALENCE (BANK_FLGG(8),FLGG_NETA,RE_FLGG_NETA),(NAMES_FLGG(6),   105
     *NFLGG_NETA)                                                         105
      EQUIVALENCE (COMMEN_FLGG(6),CFLGG_NETA),(MAP_FLGG(1,6),             105
     *MFLGG_NETA)                                                         105
      CHARACTER CFLGG_NETA*40/' '/                                        105
      CHARACTER NFLGG_NETA*12/'R.NETA'/                                   105
      INTEGER MFLGG_NETA(2)/1,1/                                          105
      REAL RE_FLGG_NETA                                                   105
C -                                                                       105
      REAL FLGG_ALTHICK                                                   105
      EQUIVALENCE (BANK_FLGG(9),FLGG_ALTHICK,RE_FLGG_ALTHICK),            105
     *(NAMES_FLGG(7),NFLGG_ALTHICK)                                       105
      EQUIVALENCE (COMMEN_FLGG(7),CFLGG_ALTHICK),(MAP_FLGG(1,7),          105
     *MFLGG_ALTHICK)                                                      105
      CHARACTER CFLGG_ALTHICK*40/' '/                                     105
      CHARACTER NFLGG_ALTHICK*12/'R.ALTHICK'/                             105
      INTEGER MFLGG_ALTHICK(2)/1,1/                                       105
      REAL RE_FLGG_ALTHICK                                                105
C -                                                                       105
      REAL FLGG_SIRUBDZ                                                   105
      EQUIVALENCE (BANK_FLGG(10),FLGG_SIRUBDZ,RE_FLGG_SIRUBDZ),           105
     *(NAMES_FLGG(8),NFLGG_SIRUBDZ)                                       105
      EQUIVALENCE (COMMEN_FLGG(8),CFLGG_SIRUBDZ),(MAP_FLGG(1,8),          105
     *MFLGG_SIRUBDZ)                                                      105
      CHARACTER CFLGG_SIRUBDZ*40/' '/                                     105
      CHARACTER NFLGG_SIRUBDZ*12/'R.SIRUBDZ'/                             105
      INTEGER MFLGG_SIRUBDZ(2)/1,1/                                       105
      REAL RE_FLGG_SIRUBDZ                                                105
C -                                                                       105
      REAL FLGG_PHCATHDZ                                                  105
      EQUIVALENCE (BANK_FLGG(11),FLGG_PHCATHDZ,RE_FLGG_PHCATHDZ),         105
     *(NAMES_FLGG(9),NFLGG_PHCATHDZ)                                      105
      EQUIVALENCE (COMMEN_FLGG(9),CFLGG_PHCATHDZ),(MAP_FLGG(1,9),         105
     *MFLGG_PHCATHDZ)                                                     105
      CHARACTER CFLGG_PHCATHDZ*40/' '/                                    105
      CHARACTER NFLGG_PHCATHDZ*12/'R.PHCATHDZ'/                           105
      INTEGER MFLGG_PHCATHDZ(2)/1,1/                                      105
      REAL RE_FLGG_PHCATHDZ                                               105
C -                                                                       105
      REAL FLGG_PHCATHR                                                   105
      EQUIVALENCE (BANK_FLGG(12),FLGG_PHCATHR,RE_FLGG_PHCATHR),           105
     *(NAMES_FLGG(10),NFLGG_PHCATHR)                                      105
      EQUIVALENCE (COMMEN_FLGG(10),CFLGG_PHCATHR),(MAP_FLGG(1,10),        105
     *MFLGG_PHCATHR)                                                      105
      CHARACTER CFLGG_PHCATHR*40/' '/                                     105
      CHARACTER NFLGG_PHCATHR*12/'R.PHCATHR'/                             105
      INTEGER MFLGG_PHCATHR(2)/1,1/                                       105
      REAL RE_FLGG_PHCATHR                                                105
C -                                                                       105
      REAL FLGG_MUMETDZ                                                   105
      EQUIVALENCE (BANK_FLGG(13),FLGG_MUMETDZ,RE_FLGG_MUMETDZ),           105
     *(NAMES_FLGG(11),NFLGG_MUMETDZ)                                      105
      EQUIVALENCE (COMMEN_FLGG(11),CFLGG_MUMETDZ),(MAP_FLGG(1,11),        105
     *MFLGG_MUMETDZ)                                                      105
      CHARACTER CFLGG_MUMETDZ*40/' '/                                     105
      CHARACTER NFLGG_MUMETDZ*12/'R.MUMETDZ'/                             105
      INTEGER MFLGG_MUMETDZ(2)/1,1/                                       105
      REAL RE_FLGG_MUMETDZ                                                105
C -                                                                       105
      REAL FLGG_MUMETR                                                    105
      EQUIVALENCE (BANK_FLGG(14),FLGG_MUMETR,RE_FLGG_MUMETR),             105
     *(NAMES_FLGG(12),NFLGG_MUMETR)                                       105
      EQUIVALENCE (COMMEN_FLGG(12),CFLGG_MUMETR),(MAP_FLGG(1,12),         105
     *MFLGG_MUMETR)                                                       105
      CHARACTER CFLGG_MUMETR*40/' '/                                      105
      CHARACTER NFLGG_MUMETR*12/'R.MUMETR'/                               105
      INTEGER MFLGG_MUMETR(2)/1,1/                                        105
      REAL RE_FLGG_MUMETR                                                 105
C -                                                                       105
      INTEGER LEN_FLGG,LLEN_FLGG,LINK_FLGG                                105
      PARAMETER (LEN_FLGG=14)                                             105
      PARAMETER (LLEN_FLGG=12)                                            105
      INTEGER FLAG_FLGG/0/,MAP_FLGG(2,LLEN_FLGG),IDNUM_FLGG(10)           105
      CHARACTER NAMES_FLGG(LLEN_FLGG)*12,COMMEN_FLGG(LLEN_FLGG)*40        105
      DATA IDNUM_FLGG/10*0/                                               105
      REAL BANK_FLGG(LEN_FLGG)                                            105
      CHARACTER*60 BPATH_FLGG/' '/                                        105
      SAVE FLAG_FLGG,MAP_FLGG,IDNUM_FLGG,NAMES_FLGG,COMMEN_FLGG,          105
     *BPATH_FLGG                                                          105
      DATA LINK_FLGG/0/,BANK_FLGG/LEN_FLGG*0/                             105
C *                                                                       105
*
C -                                                                       107
      INTEGER FLGC_ICOPY                                                  107
      EQUIVALENCE (BANK_FLGC(3),FLGC_ICOPY,RE_FLGC_ICOPY),                107
     *(NAMES_FLGC(1),NFLGC_ICOPY)                                         107
      EQUIVALENCE (COMMEN_FLGC(1),CFLGC_ICOPY),(MAP_FLGC(1,1),            107
     *MFLGC_ICOPY)                                                        107
      CHARACTER CFLGC_ICOPY*40/' '/                                       107
      CHARACTER NFLGC_ICOPY*12/'I.ICOPY'/                                 107
      INTEGER MFLGC_ICOPY(2)/1,1/                                         107
      REAL RE_FLGC_ICOPY                                                  107
C -                                                                       107
      REAL FLGC_ZPOS                                                      107
      EQUIVALENCE (BANK_FLGC(4),FLGC_ZPOS,RE_FLGC_ZPOS),(NAMES_FLGC(2),   107
     *NFLGC_ZPOS)                                                         107
      EQUIVALENCE (COMMEN_FLGC(2),CFLGC_ZPOS),(MAP_FLGC(1,2),             107
     *MFLGC_ZPOS)                                                         107
      CHARACTER CFLGC_ZPOS*40/' '/                                        107
      CHARACTER NFLGC_ZPOS*12/'R.ZPOS'/                                   107
      INTEGER MFLGC_ZPOS(2)/1,1/                                          107
      REAL RE_FLGC_ZPOS                                                   107
C -                                                                       107
      REAL FLGC_RDIS                                                      107
      EQUIVALENCE (BANK_FLGC(5),FLGC_RDIS,RE_FLGC_RDIS),(NAMES_FLGC(3),   107
     *NFLGC_RDIS)                                                         107
      EQUIVALENCE (COMMEN_FLGC(3),CFLGC_RDIS),(MAP_FLGC(1,3),             107
     *MFLGC_RDIS)                                                         107
      CHARACTER CFLGC_RDIS*40/' '/                                        107
      CHARACTER NFLGC_RDIS*12/'R.RDIS'/                                   107
      INTEGER MFLGC_RDIS(2)/1,1/                                          107
      REAL RE_FLGC_RDIS                                                   107
C -                                                                       107
      REAL FLGC_DIPANG                                                    107
      EQUIVALENCE (BANK_FLGC(6),FLGC_DIPANG,RE_FLGC_DIPANG),              107
     *(NAMES_FLGC(4),NFLGC_DIPANG)                                        107
      EQUIVALENCE (COMMEN_FLGC(4),CFLGC_DIPANG),(MAP_FLGC(1,4),           107
     *MFLGC_DIPANG)                                                       107
      CHARACTER CFLGC_DIPANG*40/' '/                                      107
      CHARACTER NFLGC_DIPANG*12/'R.DIPANG'/                               107
      INTEGER MFLGC_DIPANG(2)/1,1/                                        107
      REAL RE_FLGC_DIPANG                                                 107
C -                                                                       107
      INTEGER LEN_FLGC,LLEN_FLGC,LINK_FLGC                                107
      PARAMETER (LEN_FLGC=6)                                              107
      PARAMETER (LLEN_FLGC=4)                                             107
      INTEGER FLAG_FLGC/0/,MAP_FLGC(2,LLEN_FLGC),IDNUM_FLGC(10)           107
      CHARACTER NAMES_FLGC(LLEN_FLGC)*12,COMMEN_FLGC(LLEN_FLGC)*40        107
      DATA IDNUM_FLGC/10*0/                                               107
      REAL BANK_FLGC(LEN_FLGC)                                            107
      CHARACTER*60 BPATH_FLGC/' '/                                        107
      SAVE FLAG_FLGC,MAP_FLGC,IDNUM_FLGC,NAMES_FLGC,COMMEN_FLGC,          107
     *BPATH_FLGC                                                          107
      DATA LINK_FLGC/0/,BANK_FLGC/LEN_FLGC*0/                             107
C *                                                                       107
*
C -                                                                       109
      REAL FLGM_DENSITY                                                   109
      EQUIVALENCE (BANK_FLGM(3),FLGM_DENSITY,RE_FLGM_DENSITY),            109
     *(NAMES_FLGM(1),NFLGM_DENSITY)                                       109
      EQUIVALENCE (COMMEN_FLGM(1),CFLGM_DENSITY),(MAP_FLGM(1,1),          109
     *MFLGM_DENSITY)                                                      109
      CHARACTER CFLGM_DENSITY*40/' '/                                     109
      CHARACTER NFLGM_DENSITY*12/'R.DENSITY'/                             109
      INTEGER MFLGM_DENSITY(2)/1,1/                                       109
      REAL RE_FLGM_DENSITY                                                109
C -                                                                       109
      REAL FLGM_RADLEN                                                    109
      EQUIVALENCE (BANK_FLGM(4),FLGM_RADLEN,RE_FLGM_RADLEN),              109
     *(NAMES_FLGM(2),NFLGM_RADLEN)                                        109
      EQUIVALENCE (COMMEN_FLGM(2),CFLGM_RADLEN),(MAP_FLGM(1,2),           109
     *MFLGM_RADLEN)                                                       109
      CHARACTER CFLGM_RADLEN*40/' '/                                      109
      CHARACTER NFLGM_RADLEN*12/'R.RADLEN'/                               109
      INTEGER MFLGM_RADLEN(2)/1,1/                                        109
      REAL RE_FLGM_RADLEN                                                 109
C -                                                                       109
      REAL FLGM_INDEX                                                     109
      EQUIVALENCE (BANK_FLGM(5),FLGM_INDEX,RE_FLGM_INDEX),                109
     *(NAMES_FLGM(3),NFLGM_INDEX)                                         109
      EQUIVALENCE (COMMEN_FLGM(3),CFLGM_INDEX),(MAP_FLGM(1,3),            109
     *MFLGM_INDEX)                                                        109
      CHARACTER CFLGM_INDEX*40/' '/                                       109
      CHARACTER NFLGM_INDEX*12/'R.INDEX'/                                 109
      INTEGER MFLGM_INDEX(2)/1,1/                                         109
      REAL RE_FLGM_INDEX                                                  109
C -                                                                       109
      REAL FLGM_PBCONTENT                                                 109
      EQUIVALENCE (BANK_FLGM(6),FLGM_PBCONTENT,RE_FLGM_PBCONTENT),        109
     *(NAMES_FLGM(4),NFLGM_PBCONTENT)                                     109
      EQUIVALENCE (COMMEN_FLGM(4),CFLGM_PBCONTENT),(MAP_FLGM(1,4),        109
     *MFLGM_PBCONTENT)                                                    109
      CHARACTER CFLGM_PBCONTENT*40/' '/                                   109
      CHARACTER NFLGM_PBCONTENT*12/'R.PBCONTENT'/                         109
      INTEGER MFLGM_PBCONTENT(2)/1,1/                                     109
      REAL RE_FLGM_PBCONTENT                                              109
C -                                                                       109
      REAL FLGM_CRITENE                                                   109
      EQUIVALENCE (BANK_FLGM(7),FLGM_CRITENE,RE_FLGM_CRITENE),            109
     *(NAMES_FLGM(5),NFLGM_CRITENE)                                       109
      EQUIVALENCE (COMMEN_FLGM(5),CFLGM_CRITENE),(MAP_FLGM(1,5),          109
     *MFLGM_CRITENE)                                                      109
      CHARACTER CFLGM_CRITENE*40/' '/                                     109
      CHARACTER NFLGM_CRITENE*12/'R.CRITENE'/                             109
      INTEGER MFLGM_CRITENE(2)/1,1/                                       109
      REAL RE_FLGM_CRITENE                                                109
C -                                                                       109
      REAL FLGM_MOLIERER                                                  109
      EQUIVALENCE (BANK_FLGM(8),FLGM_MOLIERER,RE_FLGM_MOLIERER),          109
     *(NAMES_FLGM(6),NFLGM_MOLIERER)                                      109
      EQUIVALENCE (COMMEN_FLGM(6),CFLGM_MOLIERER),(MAP_FLGM(1,6),         109
     *MFLGM_MOLIERER)                                                     109
      CHARACTER CFLGM_MOLIERER*40/' '/                                    109
      CHARACTER NFLGM_MOLIERER*12/'R.MOLIERER'/                           109
      INTEGER MFLGM_MOLIERER(2)/1,1/                                      109
      REAL RE_FLGM_MOLIERER                                               109
C -                                                                       109
      INTEGER LEN_FLGM,LLEN_FLGM,LINK_FLGM                                109
      PARAMETER (LEN_FLGM=8)                                              109
      PARAMETER (LLEN_FLGM=6)                                             109
      INTEGER FLAG_FLGM/0/,MAP_FLGM(2,LLEN_FLGM),IDNUM_FLGM(10)           109
      CHARACTER NAMES_FLGM(LLEN_FLGM)*12,COMMEN_FLGM(LLEN_FLGM)*40        109
      DATA IDNUM_FLGM/10*0/                                               109
      REAL BANK_FLGM(LEN_FLGM)                                            109
      CHARACTER*60 BPATH_FLGM/' '/                                        109
      SAVE FLAG_FLGM,MAP_FLGM,IDNUM_FLGM,NAMES_FLGM,COMMEN_FLGM,          109
     *BPATH_FLGM                                                          109
      DATA LINK_FLGM/0/,BANK_FLGM/LEN_FLGM*0/                             109
C *                                                                       109
*
      INTEGER I_SECTION,J_SECTION,IE,IS,ISEC,I_STR,NSTR,TYPE,II,JJ,       111
     *ISTAT/0/,IPOS                                                       111
*
      REAL SECWID,SECTION,CENTER,CURRENT,PLATE,GAP,CELL,G10, TAN_LOW,     120
     *TAN_UPP,TANF,RBOT,RTOP,DETA,ETAX, DUP,DD,D2,D3,RSHIFT,DPHI,         120
     *ORGKEEP,ENDKEEP, MAXCNT,MSECWD,MXGTEN,CURR, CURCL,ETATOP,ETABOT,    120
     *ZPOS,ZDIS, XLEFT,XRIGHT,YLEFT,YRIGHT, SQ2,SQ3,RTH,TNG,LEN,P,XC,YC,  120
     *DIFF, XX,YY,RTOT,ZTOT,FWOLDZ,ALP                                    120
      INTEGER N                                                           122
      PARAMETER (N=12)                                                    123
* --- Enegry bins
      REAL E(N) /1.776E-9, 1.912E-9, 2.072E-9, 2.26E-9, 2.486E-9,         126
     *2.702E-9, 2.825E-9, 2.96E-9, 3.108E-9, 3.271E-9, 3.551E-9,          126
     *3.767E-9/                                                           126
* --- Refraction indexes
      REAL RINDEX_PBG(N) /N*1.67/                                         129
      REAL RINDEX_SIRUB(N) /N*1.67/                                       130
      REAL RINDEX_PHCATH(N) /N*1.67/                                      131
      REAL RINDEX_ALM(N) /N*1.0/                                          132
      REAL RINDEX_MUMET(N) /N*1.0/                                        133
* ---- Absorbtion lenghts (in cm)
      REAL ABSCO_PBG(N) /N*38.0/                                          135
      REAL ABSCO_SIRUB(N) /N*38.0/                                        136
      REAL ABSCO_PHCATH(N) /N*0.0001/                                     137
      REAL ABSCO_ALM(N) /N*0.0001/                                        138
      REAL ABSCO_MUMET(N) /N*0.0001/                                      139
* ---- Detection efficiencies (quantum efficiency for Photo Cathode)
      REAL EFFIC_PHCATH(N) / N*1.0/                                       141
*    real effic_PhCath(N)  / 0.06,  0.08,   0.105,  0.14,   0.155,  0.17,
*                            0.18,  0.2,    0.19,   0.175,  0.135,  0.09/
      REAL EFFIC_ALL(N) /N*0.0/                                           145
      EXTERNAL FFPDSTEP,FPCTSTEP                                          146
*
      TANF(ETAX) = TAN(2*ATAN(EXP(-ETAX)))                                148
*
* ----------------------------------------------------------------------------
*
C *                                                                       152
      CALL AGSBEGM(MODULE,IPRIN)                                          152
      IF (IPRIN.LT.0) RETURN                                              152
C Filling bank FPDG - FPD ENDCAP CALORIMETER BASIC DATA                   152
      BANK_TITLE='FPD ENDCAP CALORIMETER BASIC DATA'                      152
      BPATH_FPDG='FPDG*'                                                  152
      IDNUM_FPDG(1)=0                                                     153
C GEOMETRY VERSION                                                        153
      FPDG_VERSION=5.0                                                    153
      CFPDG_VERSION='GEOMETRY VERSION'                                    153
C =0 NO, =1 WEST, =2 EAST, =3 BOTH FOR FPDM, FPD AND PBG                  154
      FPDG_ONOFF=2                                                        154
      CFPDG_ONOFF='=0 NO, =1 WEST, =2 EAST, =3 BOTH FOR FPDM, FPD AND PB  154
     *G'                                                                  154
C = 0 DE, = 1 CHERENKOV SIMULATION FOR PBG                                155
      FPDG_CHKVSIM=1                                                      155
      CFPDG_CHKVSIM='= 0 DE, = 1 CHERENKOV SIMULATION FOR PBG'            155
C Z-DISTANCE OF CENTER OF FPD MOTHER                                      156
      FPDG_ZDIS=600.0                                                     156
      CFPDG_ZDIS='Z-DISTANCE OF CENTER OF FPD MOTHER'                     156
C Z-LENGTH OF FPD MOTHER                                                  157
      FPDG_ZLEN=210.0                                                     157
      CFPDG_ZLEN='Z-LENGTH OF FPD MOTHER'                                 157
C RADIUS OF FPDM TUBE                                                     158
      FPDG_RADIUS=70.0                                                    158
      CFPDG_RADIUS='RADIUS OF FPDM TUBE'                                  158
C FPDM INNER RADIUS                                                       159
      FPDG_RMIN=10.0                                                      159
      CFPDG_RMIN='FPDM INNER RADIUS'                                      159
* ----------------------------------------------------------------------------
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPDG',    161
     *BPATH_FPDG,IDNUM_FPDG,1, MAP_FPDG,NAMES_FPDG,COMMEN_FPDG,           161
     *BANK_FPDG,LEN_FPDG,LLEN_FPDG,LINK_FPDG,FLAG_FPDG)                   161
C *                                                                       161
C *                                                                       161
      CALL AGSBEGM(MODULE,IPRIN)                                          161
      IF (IPRIN.LT.0) RETURN                                              161
C Filling bank FMCC - DETAILS OF EM SECTOR POSITION                       161
      BANK_TITLE='DETAILS OF EM SECTOR POSITION'                          161
      BPATH_FMCC='FMCC*'                                                  161
      IDNUM_FMCC(1)=0                                                     162
C CALORIMETER SECTOR ORIGIN IN Z IN THIS SETUP                            162
      FMCC_ZORIG=750                                                      162
      CFMCC_ZORIG='CALORIMETER SECTOR ORIGIN IN Z IN THIS SETUP'          162
C CALORIMETER SECTOR END IN Z IN THIS SETUP                               163
      FMCC_ZEND=786.507                                                   163
      CFMCC_ZEND='CALORIMETER SECTOR END IN Z IN THIS SETUP'              163
C OFFSETS IN X: SIGNED ACCORDING TO GLOBAL COORDS(60 FOR NEAR, 20 - FAR)  164
      FMCC_XOFFSET=60.0                                                   164
      CFMCC_XOFFSET='OFFSETS IN X: SIGNED ACCORDING TO GLOBAL COORDS(60   164
     *FOR NEAR, 20 - FAR)'                                                164
C OFFSETS IN Y: SIGNED ACCORDING TO GLOBAL COORDS                         165
      FMCC_YOFFSET=0.0                                                    165
      CFMCC_YOFFSET='OFFSETS IN Y: SIGNED ACCORDING TO GLOBAL COORDS'     165
C OFFSETS IN Z: SIGNED ACCORDING TO GLOBAL COORDS                         166
      FMCC_ZOFFSET=0.0                                                    166
      CFMCC_ZOFFSET='OFFSETS IN Z: SIGNED ACCORDING TO GLOBAL COORDS'     166
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCC',    169
     *BPATH_FMCC,IDNUM_FMCC,1, MAP_FMCC,NAMES_FMCC,COMMEN_FMCC,           169
     *BANK_FMCC,LEN_FMCC,LLEN_FMCC,LINK_FMCC,FLAG_FMCC)                   169
C *                                                                       169
C *                                                                       169
      CALL AGSBEGM(MODULE,IPRIN)                                          169
      IF (IPRIN.LT.0) RETURN                                              169
C Filling bank FMCS - SECTOR OF EM ENDCAP CALORIMETER GEOMETRY            169
      BANK_TITLE='SECTOR OF EM ENDCAP CALORIMETER GEOMETRY'               169
      BPATH_FMCS='FMCS*'                                                  169
      IDNUM_FMCS(1)=0                                                     170
C =1 ENDCAP, =2 FPD ENDCAP PROTOTYPE                                      170
      FMCS_TYPE=2                                                         170
      CFMCS_TYPE='=1 ENDCAP, =2 FPD ENDCAP PROTOTYPE'                     170
C NOMINAL CALORIMETER ORIGIN IN Z (IE FOCUS)                              171
      FMCS_ZORIG=273.5                                                    171
      CFMCS_ZORIG='NOMINAL CALORIMETER ORIGIN IN Z (IE FOCUS)'            171
C NOMINAL CALORIMETER END IN Z                                            172
      FMCS_ZEND=310.007                                                   172
      CFMCS_ZEND='NOMINAL CALORIMETER END IN Z'                           172
C UPPER FEDUCIAL ETA CUT                                                  173
      FMCS_ETAMIN=1.6317                                                  173
      CFMCS_ETAMIN='UPPER FEDUCIAL ETA CUT'                               173
C LOWER FEDUCIAL ETA CUT                                                  174
      FMCS_ETAMAX=2.0                                                     174
      CFMCS_ETAMAX='LOWER FEDUCIAL ETA CUT'                               174
C MIN PHI                                                                 175
      FMCS_PHIMIN=-9                                                      175
      CFMCS_PHIMIN='MIN PHI'                                              175
C MAX PHI                                                                 176
      FMCS_PHIMAX=9                                                       176
      CFMCS_PHIMAX='MAX PHI'                                              176
C NUMBER OF AZIMUTHAL SUPERSECTORS                                        177
      FMCS_NSUPSEC=1                                                      177
      CFMCS_NSUPSEC='NUMBER OF AZIMUTHAL SUPERSECTORS'                    177
C NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)                            178
      FMCS_NSECTOR=3                                                      178
      CFMCS_NSECTOR='NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)'        178
C NUMBER OF PHI SLICES IN SUPERSECTOR                                     179
      FMCS_NSLICES=3                                                      179
      CFMCS_NSLICES='NUMBER OF PHI SLICES IN SUPERSECTOR'                 179
C NUMBER OF READOUT SECTIONS                                              180
      FMCS_NSECTION=8                                                     180
      CFMCS_NSECTION='NUMBER OF READOUT SECTIONS'                         180
C THICKNESS OF THE FRONT AL PLATES                                        181
      FMCS_FRONT=0.953                                                    181
      CFMCS_FRONT='THICKNESS OF THE FRONT AL PLATES'                      181
C FIBER ROUTING GUIDES                                                    182
      FMCS_GTEN=0.16                                                      182
      CFMCS_GTEN='FIBER ROUTING GUIDES'                                   182
C LEAD RADIATOR THICKNESS                                                 183
      FMCS_PLATE=0.5                                                      183
      CFMCS_PLATE='LEAD RADIATOR THICKNESS'                               183
C LAMINATED SS PLATE THICKNESS                                            184
      FMCS_PLATES=0.05                                                    184
      CFMCS_PLATES='LAMINATED SS PLATE THICKNESS'                         184
C BACK PLATE THICKNESS SS                                                 185
      FMCS_PLATEB=3.175                                                   185
      CFMCS_PLATEB='BACK PLATE THICKNESS SS'                              185
C THICKNESS OF ENDCAP HUB                                                 186
      FMCS_HUB=2.5                                                        186
      CFMCS_HUB='THICKNESS OF ENDCAP HUB'                                 186
C RADIAL SHIFT OF MODULE                                                  187
      FMCS_RMSHIFT=1.228                                                  187
      CFMCS_RMSHIFT='RADIAL SHIFT OF MODULE'                              187
C RADIAL SHIFT OF STEEL SUPPORT WALLS                                     188
      FMCS_SMSHIFT=0.12                                                   188
      CFMCS_SMSHIFT='RADIAL SHIFT OF STEEL SUPPORT WALLS'                 188
C HALF OF THE INTER-PLATE GAP IN PHI                                      189
      FMCS_GAPPLT=0.3/2                                                   189
      CFMCS_GAPPLT='HALF OF THE INTER-PLATE GAP IN PHI'                   189
C HALF OF THE RADIAL INTER-CELL GAP                                       190
      FMCS_GAPCEL=0.03/2                                                  190
      CFMCS_GAPCEL='HALF OF THE RADIAL INTER-CELL GAP'                    190
C SPACE FOR SMD DETECTOR                                                  191
      FMCS_GAPSMD=3.2                                                     191
      CFMCS_GAPSMD='SPACE FOR SMD DETECTOR'                               191
* --------------------------------------------------------------------------
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCS',    193
     *BPATH_FMCS,IDNUM_FMCS,1, MAP_FMCS,NAMES_FMCS,COMMEN_FMCS,           193
     *BANK_FMCS,LEN_FMCS,LLEN_FMCS,LINK_FMCS,FLAG_FMCS)                   193
C *                                                                       193
C *                                                                       193
      CALL AGSBEGM(MODULE,IPRIN)                                          193
      IF (IPRIN.LT.0) RETURN                                              193
C Filling bank FETR - ETA AND PHI GRID VALUES                             193
      BANK_TITLE='ETA AND PHI GRID VALUES'                                193
      BPATH_FETR='FETR*'                                                  193
      IDNUM_FETR(1)=0                                                     194
C =1 ENDCAP, =2 FPD                                                       194
      FETR_TYPE=2                                                         194
      CFETR_TYPE='=1 ENDCAP, =2 FPD'                                      194
C ETA_TOP/ETA_BOT TOWER GRANULARITY                                       195
      FETR_ETAGR=1.0536                                                   195
      CFETR_ETAGR='ETA_TOP/ETA_BOT TOWER GRANULARITY'                     195
C PHI GRANULARITY (RADIANS)                                               196
      FETR_PHIGR=0.0981747                                                196
      CFETR_PHIGR='PHI GRANULARITY (RADIANS)'                             196
C ETA GRANULARITY                                                         197
      FETR_NETA=4                                                         197
      CFETR_NETA='ETA GRANULARITY'                                        197
C ETA RAPIDITIES                                                          198
      FETR_ETABIN(1)=2.0                                                  198
      FETR_ETABIN(2)=1.9                                                  198
      FETR_ETABIN(3)=1.8                                                  198
      FETR_ETABIN(4)=1.7                                                  198
      FETR_ETABIN(5)=1.6                                                  198
      FETR_ETABIN(6)=1.6                                                  198
      FETR_ETABIN(7)=1.6                                                  198
      FETR_ETABIN(8)=1.6                                                  198
      FETR_ETABIN(9)=1.6                                                  198
      FETR_ETABIN(10)=1.6                                                 198
      FETR_ETABIN(11)=1.6                                                 198
      FETR_ETABIN(12)=1.6                                                 198
      FETR_ETABIN(13)=1.6                                                 198
      CFETR_ETABIN='ETA RAPIDITIES'                                       198
*---------------------------------------------------------------------------
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FETR',    200
     *BPATH_FETR,IDNUM_FETR,1, MAP_FETR,NAMES_FETR,COMMEN_FETR,           200
     *BANK_FETR,LEN_FETR,LLEN_FETR,LINK_FETR,FLAG_FETR)                   200
C *                                                                       200
C *                                                                       200
      CALL AGSBEGM(MODULE,IPRIN)                                          200
      IF (IPRIN.LT.0) RETURN                                              200
C Filling bank FSEC - FIRST EM SECTION                                    200
      BANK_TITLE='FIRST EM SECTION'                                       200
      BPATH_FSEC='FSEC*'                                                  200
      IDNUM_FSEC(1)=0                                                     201
C SECTION NUMBER                                                          201
      FSEC_ISECT=1                                                        201
      CFSEC_ISECT='SECTION NUMBER'                                        201
C NUMBER OF SCI LAYERS ALONG Z                                            202
      FSEC_NLAYER=2                                                       202
      CFSEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         202
C CELL FULL WIDTH IN Z                                                    203
      FSEC_CELL=1.328                                                     203
      CFSEC_CELL='CELL FULL WIDTH IN Z'                                   203
C SCI LAYER THICKNESS                                                     204
      FSEC_SCINT(1)=0.5                                                   204
      FSEC_SCINT(2)=0.5                                                   204
      CFSEC_SCINT='SCI LAYER THICKNESS'                                   204
*
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSEC',    206
     *BPATH_FSEC,IDNUM_FSEC,1, MAP_FSEC,NAMES_FSEC,COMMEN_FSEC,           206
     *BANK_FSEC,LEN_FSEC,LLEN_FSEC,LINK_FSEC,FLAG_FSEC)                   206
C *                                                                       206
C *                                                                       206
      CALL AGSBEGM(MODULE,IPRIN)                                          206
      IF (IPRIN.LT.0) RETURN                                              206
C Filling bank FSEC - SECOND EM SECTION                                   206
      BANK_TITLE='SECOND EM SECTION'                                      206
      BPATH_FSEC='FSEC*'                                                  206
      IDNUM_FSEC(1)=0                                                     207
C SECTION NUMBER                                                          207
      FSEC_ISECT=2                                                        207
      CFSEC_ISECT='SECTION NUMBER'                                        207
C NUMBER OF SCI LAYERS ALONG Z                                            208
      FSEC_NLAYER=4                                                       208
      CFSEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         208
C CELL FULL WIDTH IN Z                                                    209
      FSEC_CELL=1.228                                                     209
      CFSEC_CELL='CELL FULL WIDTH IN Z'                                   209
C SCI LAYER THICKNESS                                                     210
      FSEC_SCINT(1)=0.4                                                   210
      FSEC_SCINT(2)=0.5                                                   210
      CFSEC_SCINT='SCI LAYER THICKNESS'                                   210
*
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSEC',    212
     *BPATH_FSEC,IDNUM_FSEC,1, MAP_FSEC,NAMES_FSEC,COMMEN_FSEC,           212
     *BANK_FSEC,LEN_FSEC,LLEN_FSEC,LINK_FSEC,FLAG_FSEC)                   212
C *                                                                       212
C *                                                                       212
      CALL AGSBEGM(MODULE,IPRIN)                                          212
      IF (IPRIN.LT.0) RETURN                                              212
C Filling bank FSEC - THIRD AND FURTHER EM SECTION                        212
      BANK_TITLE='THIRD AND FURTHER EM SECTION'                           212
      BPATH_FSEC='FSEC*'                                                  212
      IDNUM_FSEC(1)=0                                                     213
C SECTION                                                                 213
      FSEC_ISECT=3                                                        213
      CFSEC_ISECT='SECTION'                                               213
C NUMBER OF SCI LAYERS ALONG Z                                            214
      FSEC_NLAYER=3                                                       214
      CFSEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         214
C CELL FULL WIDTH IN Z                                                    215
      FSEC_CELL=1.228                                                     215
      CFSEC_CELL='CELL FULL WIDTH IN Z'                                   215
C SCI LAYER THICKNESS                                                     216
      FSEC_SCINT(1)=0.4                                                   216
      FSEC_SCINT(2)=0.5                                                   216
      CFSEC_SCINT='SCI LAYER THICKNESS'                                   216
*----------------------------------------------------------------------------
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSEC',    218
     *BPATH_FSEC,IDNUM_FSEC,1, MAP_FSEC,NAMES_FSEC,COMMEN_FSEC,           218
     *BANK_FSEC,LEN_FSEC,LLEN_FSEC,LINK_FSEC,FLAG_FSEC)                   218
C *                                                                       218
C *                                                                       218
      CALL AGSBEGM(MODULE,IPRIN)                                          218
      IF (IPRIN.LT.0) RETURN                                              218
C Filling bank FMXG - EM ENDCAP SMD BASIC DATA                            218
      BANK_TITLE='EM ENDCAP SMD BASIC DATA'                               218
      BPATH_FMXG='FMXG*'                                                  218
      IDNUM_FMXG(1)=0                                                     219
C GEOMETRY VERSION                                                        219
      FMXG_VERSION=1                                                      219
      CFMXG_VERSION='GEOMETRY VERSION'                                    219
C SCINTILLATOR STRIP APEX                                                 220
      FMXG_SAPEX=0.5                                                      220
      CFMXG_SAPEX='SCINTILLATOR STRIP APEX'                               220
C SCINTILLATOR STRIP BASE                                                 221
      FMXG_SBASE=1.0                                                      221
      CFMXG_SBASE='SCINTILLATOR STRIP BASE'                               221
*
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    223
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           223
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   223
C *                                                                       223
C *                                                                       223
      CALL AGSBEGM(MODULE,IPRIN)                                          223
      IF (IPRIN.LT.0) RETURN                                              223
C Filling bank FXSE - FIRST SMD SECTION                                   223
      BANK_TITLE='FIRST SMD SECTION'                                      223
      BPATH_FXSE='FXSE*'                                                  223
      IDNUM_FXSE(1)=0                                                     224
C SECTION NUMBER                                                          224
      FXSE_JSECT=1                                                        224
      CFXSE_JSECT='SECTION NUMBER'                                        224
C SECTION WIDTH                                                           225
      FXSE_SWIDTH=1.6                                                     225
      CFXSE_SWIDTH='SECTION WIDTH'                                        225
C WIDTH OF ALUMINIUM PLATE                                                226
      FXSE_APLATE=0.15                                                    226
      CFXSE_APLATE='WIDTH OF ALUMINIUM PLATE'                             226
*
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FXSE',    228
     *BPATH_FXSE,IDNUM_FXSE,1, MAP_FXSE,NAMES_FXSE,COMMEN_FXSE,           228
     *BANK_FXSE,LEN_FXSE,LLEN_FXSE,LINK_FXSE,FLAG_FXSE)                   228
C *                                                                       228
C *                                                                       228
      CALL AGSBEGM(MODULE,IPRIN)                                          228
      IF (IPRIN.LT.0) RETURN                                              228
C Filling bank FXSE - SECOND SMD SECTION                                  228
      BANK_TITLE='SECOND SMD SECTION'                                     228
      BPATH_FXSE='FXSE*'                                                  228
      IDNUM_FXSE(1)=0                                                     229
C SECTION NUMBER                                                          229
      FXSE_JSECT=2                                                        229
      CFXSE_JSECT='SECTION NUMBER'                                        229
C SECTION WIDTH                                                           230
      FXSE_SWIDTH=1.6                                                     230
      CFXSE_SWIDTH='SECTION WIDTH'                                        230
C WIDTH OF ALUMINIUM PLATE                                                231
      FXSE_APLATE=0.15                                                    231
      CFXSE_APLATE='WIDTH OF ALUMINIUM PLATE'                             231
*----------------------------------------------------------------------------
*----------------------------------------------------------------------------
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FXSE',    234
     *BPATH_FXSE,IDNUM_FXSE,1, MAP_FXSE,NAMES_FXSE,COMMEN_FXSE,           234
     *BANK_FXSE,LEN_FXSE,LLEN_FXSE,LINK_FXSE,FLAG_FXSE)                   234
C *                                                                       234
C *                                                                       234
      CALL AGSBEGM(MODULE,IPRIN)                                          234
      IF (IPRIN.LT.0) RETURN                                              234
C Filling bank FLGG - PBG DETECTOR GENERAL GEOMETRY                       234
      BANK_TITLE='PBG DETECTOR GENERAL GEOMETRY'                          234
      BPATH_FLGG='FLGG*'                                                  234
      IDNUM_FLGG(1)=0                                                     235
C PBG WIDTH                                                               235
      FLGG_WIDTH=3.8                                                      235
      CFLGG_WIDTH='PBG WIDTH'                                             235
C PBG DEPTH                                                               236
      FLGG_DEPTH=45.0                                                     236
      CFLGG_DEPTH='PBG DEPTH'                                             236
C GAP BETWEEN PBG                                                         237
      FLGG_DGAP=0.40                                                      237
      CFLGG_DGAP='GAP BETWEEN PBG'                                        237
C # OF TOWER IN PHI (TEST)                                                238
      FLGG_NPHI=4                                                         238
      CFLGG_NPHI='# OF TOWER IN PHI (TEST)'                               238
C # OF TOWER IN ETA (TEST)                                                239
      FLGG_NETA=4                                                         239
      CFLGG_NETA='# OF TOWER IN ETA (TEST)'                               239
C ALMUNIM WRAP THINKNESS (REAL)                                           240
      FLGG_ALTHICK=0.002                                                  240
      CFLGG_ALTHICK='ALMUNIM WRAP THINKNESS (REAL)'                       240
C SILICON LUBBER THINKNESS (TEST)                                         241
      FLGG_SIRUBDZ=0.0                                                    241
      CFLGG_SIRUBDZ='SILICON LUBBER THINKNESS (TEST)'                     241
C PHOTO CATHODE THINKNESS                                                 242
      FLGG_PHCATHDZ=2.0                                                   242
      CFLGG_PHCATHDZ='PHOTO CATHODE THINKNESS'                            242
C PHOTO CATHODE RADIUS (TEST)                                             243
      FLGG_PHCATHR=1.9                                                    243
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (TEST)'                         243
C MU METAL LENGTH                                                         244
      FLGG_MUMETDZ=11.0                                                   244
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     244
C MU METAL OUTER RADIUS (REAL)                                            245
      FLGG_MUMETR=1.9                                                     245
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         245
C MU METAL OUTER RADIUS (TEST)                                            246
      FLGG_MUMETR=2.0                                                     246
      CFLGG_MUMETR='MU METAL OUTER RADIUS (TEST)'                         246
*
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    248
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           248
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   248
C *                                                                       248
C *                                                                       248
      CALL AGSBEGM(MODULE,IPRIN)                                          248
      IF (IPRIN.LT.0) RETURN                                              248
C Filling bank FLGM - PBG DETECTOR MATERIALS                              248
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 248
      BPATH_FLGM='FLGM*'                                                  248
      IDNUM_FLGM(1)=0                                                     249
C GDENSITY [/CM^3]                                                        249
      FLGM_DENSITY=3.86                                                   249
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    249
C RADIATION LENGTH [CM]                                                   250
      FLGM_RADLEN=2.5                                                     250
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                250
C PBO CONTENT [%]                                                         251
      FLGM_PBCONTENT=65.4                                                 251
      CFLGM_PBCONTENT='PBO CONTENT [%]'                                   251
C CRITICAL ENERGY [GEV]                                                   252
      FLGM_CRITENE=0.0158                                                 252
      CFLGM_CRITENE='CRITICAL ENERGY [GEV]'                               252
C MOLIERE RADIUS [CM]                                                     253
      FLGM_MOLIERER=3.32                                                  253
      CFLGM_MOLIERER='MOLIERE RADIUS [CM]'                                253
*
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    255
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           255
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   255
C *                                                                       255
C *                                                                       255
      CALL AGSBEGM(MODULE,IPRIN)                                          255
      IF (IPRIN.LT.0) RETURN                                              255
C Filling bank FLGC - ONE COPY OF FPD                                     255
      BANK_TITLE='ONE COPY OF FPD'                                        255
      BPATH_FLGC='FLGC*'                                                  255
      IDNUM_FLGC(1)=0                                                     256
C 1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH                                       256
      FLGC_ICOPY=1                                                        256
      CFLGC_ICOPY='1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH'                     256
C Z POSITIONS                                                             257
      FLGC_ZPOS=600.0                                                     257
      CFLGC_ZPOS='Z POSITIONS'                                            257
C R-DISTANCES FROM BEAM                                                   258
      FLGC_RDIS=25.6                                                      258
      CFLGC_RDIS='R-DISTANCES FROM BEAM'                                  258
C DIP ANGLE                                                               259
      FLGC_DIPANG=0.0                                                     259
      CFLGC_DIPANG='DIP ANGLE'                                            259
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGC',    260
     *BPATH_FLGC,IDNUM_FLGC,1, MAP_FLGC,NAMES_FLGC,COMMEN_FLGC,           260
     *BANK_FLGC,LEN_FLGC,LLEN_FLGC,LINK_FLGC,FLAG_FLGC)                   260
C *                                                                       260
C *                                                                       260
      CALL AGSBEGM(MODULE,IPRIN)                                          260
      IF (IPRIN.LT.0) RETURN                                              260
C Filling bank FLGC - ONE COPY OF FPD                                     260
      BANK_TITLE='ONE COPY OF FPD'                                        260
      BPATH_FLGC='FLGC*'                                                  260
      IDNUM_FLGC(1)=0                                                     261
C 1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH                                       261
      FLGC_ICOPY=2                                                        261
      CFLGC_ICOPY='1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH'                     261
C Z POSITIONS                                                             262
      FLGC_ZPOS=750.0                                                     262
      CFLGC_ZPOS='Z POSITIONS'                                            262
C X-DISTANCES FROM BEAM                                                   263
      FLGC_RDIS=31.0                                                      263
      CFLGC_RDIS='X-DISTANCES FROM BEAM'                                  263
C DIP ANGLE                                                               264
      FLGC_DIPANG=15.0                                                    264
      CFLGC_DIPANG='DIP ANGLE'                                            264
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGC',    265
     *BPATH_FLGC,IDNUM_FLGC,1, MAP_FLGC,NAMES_FLGC,COMMEN_FLGC,           265
     *BANK_FLGC,LEN_FLGC,LLEN_FLGC,LINK_FLGC,FLAG_FLGC)                   265
C *                                                                       265
C *                                                                       265
      CALL AGSBEGM(MODULE,IPRIN)                                          265
      IF (IPRIN.LT.0) RETURN                                              265
C Filling bank FLGC - ONE COPY OF FPD                                     265
      BANK_TITLE='ONE COPY OF FPD'                                        265
      BPATH_FLGC='FLGC*'                                                  265
      IDNUM_FLGC(1)=0                                                     266
C 1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH                                       266
      FLGC_ICOPY=3                                                        266
      CFLGC_ICOPY='1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH'                     266
C Z POSITIONS                                                             267
      FLGC_ZPOS=600.0                                                     267
      CFLGC_ZPOS='Z POSITIONS'                                            267
C Y-DISTANCES FROM BEAM                                                   268
      FLGC_RDIS=25.6                                                      268
      CFLGC_RDIS='Y-DISTANCES FROM BEAM'                                  268
C DIP ANGLE                                                               269
      FLGC_DIPANG=0.0                                                     269
      CFLGC_DIPANG='DIP ANGLE'                                            269
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGC',    270
     *BPATH_FLGC,IDNUM_FLGC,1, MAP_FLGC,NAMES_FLGC,COMMEN_FLGC,           270
     *BANK_FLGC,LEN_FLGC,LLEN_FLGC,LINK_FLGC,FLAG_FLGC)                   270
C *                                                                       270
C *                                                                       270
      CALL AGSBEGM(MODULE,IPRIN)                                          270
      IF (IPRIN.LT.0) RETURN                                              270
C Filling bank FLGC - ONE COPY OF FPD                                     270
      BANK_TITLE='ONE COPY OF FPD'                                        270
      BPATH_FLGC='FLGC*'                                                  270
      IDNUM_FLGC(1)=0                                                     271
C 1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH                                       271
      FLGC_ICOPY=4                                                        271
      CFLGC_ICOPY='1- TOP, 2-SOUTH,3-BOTTOM, 4-NORTH'                     271
C Z POSITIONS                                                             272
      FLGC_ZPOS=750.0                                                     272
      CFLGC_ZPOS='Z POSITIONS'                                            272
C Y-DISTANCES FROM BEAM                                                   273
      FLGC_RDIS=31.0                                                      273
      CFLGC_RDIS='Y-DISTANCES FROM BEAM'                                  273
C DIP ANGLE                                                               274
      FLGC_DIPANG=-15.0                                                   274
      CFLGC_DIPANG='DIP ANGLE'                                            274
*
*----------------------------------------------------------------------------
*
* PbG and general:
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGC',    279
     *BPATH_FLGC,IDNUM_FLGC,1, MAP_FLGC,NAMES_FLGC,COMMEN_FLGC,           279
     *BANK_FLGC,LEN_FLGC,LLEN_FLGC,LINK_FLGC,FLAG_FLGC)                   279
C *                                                                       279
C use default FPDG bank                                                   279
      BPATH_FPDG='FPDG*'                                                  279
      IDNUM_FPDG(1)=0                                                     279
      CALL AGDGETP('FPDMGEO','*',-999,'FPDG',' ',BANK_FPDG, BPATH_FPDG,   279
     *IDNUM_FPDG,1,MAP_FPDG,NAMES_FPDG,BANK_FPDG,LEN_FPDG,LLEN_FPDG,      279
     *LINK_FPDG,FLAG_FPDG)                                                279
C use default FLGG bank                                                   280
      BPATH_FLGG='FLGG*'                                                  280
      IDNUM_FLGG(1)=0                                                     280
      CALL AGDGETP('FPDMGEO','*',-999,'FLGG',' ',BANK_FLGG, BPATH_FLGG,   280
     *IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,BANK_FLGG,LEN_FLGG,LLEN_FLGG,      280
     *LINK_FLGG,FLAG_FLGG)                                                280
C use default FLGM bank                                                   281
      BPATH_FLGM='FLGM*'                                                  281
      IDNUM_FLGM(1)=0                                                     281
      CALL AGDGETP('FPDMGEO','*',-999,'FLGM',' ',BANK_FLGM, BPATH_FLGM,   281
     *IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,BANK_FLGM,LEN_FLGM,LLEN_FLGM,      281
     *LINK_FLGM,FLAG_FLGM)                                                281
* Endcap calorimeter sector:
C use default FMCC bank                                                   283
      BPATH_FMCC='FMCC*'                                                  283
      IDNUM_FMCC(1)=0                                                     283
      CALL AGDGETP('FPDMGEO','*',-999,'FMCC',' ',BANK_FMCC, BPATH_FMCC,   283
     *IDNUM_FMCC,1,MAP_FMCC,NAMES_FMCC,BANK_FMCC,LEN_FMCC,LLEN_FMCC,      283
     *LINK_FMCC,FLAG_FMCC)                                                283
C use default FMCS bank                                                   284
      BPATH_FMCS='FMCS*'                                                  284
      IDNUM_FMCS(1)=0                                                     284
      CALL AGDGETP('FPDMGEO','*',-999,'FMCS',' ',BANK_FMCS, BPATH_FMCS,   284
     *IDNUM_FMCS,1,MAP_FMCS,NAMES_FMCS,BANK_FMCS,LEN_FMCS,LLEN_FMCS,      284
     *LINK_FMCS,FLAG_FMCS)                                                284
C use default FETR bank                                                   285
      BPATH_FETR='FETR*'                                                  285
      IDNUM_FETR(1)=0                                                     285
      CALL AGDGETP('FPDMGEO','*',-999,'FETR',' ',BANK_FETR, BPATH_FETR,   285
     *IDNUM_FETR,1,MAP_FETR,NAMES_FETR,BANK_FETR,LEN_FETR,LLEN_FETR,      285
     *LINK_FETR,FLAG_FETR)                                                285
C Check IPRIN>=1                                                          286
      IF (IPRIN.GE.1) WRITE(LOUT,5010)FPDG_VERSION                        286
5010  FORMAT(' FPDMGEO version ', F4.2)                                   287
*     call  AgSSTEP(FFPDSTEP)
* FPD Module
      IF (FPDG_ONOFF.GT.0) THEN                                           292
      CALL AGSBEGM(MODULE,IPRIN)                                          294
C Check IPRIN<0                                                           294
      IF (IPRIN.LT.0) RETURN                                              294
C Create FPDM :                                                           294
C Call To Local Procedure FPDM                                            294
      ASSIGN 5030 TO BLOCK_FPDM                                           294
      GO TO  5020                                                         294
5030  CONTINUE                                                            294
C *                                                                       294
C Check FPDG_ONOFF==1 | FPDG_ONOFF==3                                     295
      IF (FPDG_ONOFF.EQ.1 .OR. FPDG_ONOFF.EQ.3) THEN                      295
C Geant POSITION : FPDM ===> MOTHER='CAVE' Z=FPDG_ZDIS+FPDG_ZLEN/2 KONLY  296
C ='MANY' <===                                                            296
      CALL AGSRESET                                                       296
      AG_MOTHER = 'CAVE'                                                  296
      AG_Z = FPDG_ZDIS+FPDG_ZLEN/2                                        296
      AG_KONLY = 'MANY'                                                   296
      AG_TITLE = 'FPDM'                                                   296
      AG_EXNAME = 'POSITION'                                              296
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       296
      CALL AXPOSITION                                                     296
C *                                                                       296
      ENDIF                                                               296
* East FPD Module
C Check FPDG_ONOFF==2 | FPDG_ONOFF==3                                     299
      IF (FPDG_ONOFF.EQ.2 .OR. FPDG_ONOFF.EQ.3) THEN                      299
C Geant POSITION : FPDM ===> MOTHER='CAVE' Z=-FPDG_ZDIS-FPDG_ZLEN/2 THET  300
C AZ=180 KONLY='MANY' <===                                                300
      CALL AGSRESET                                                       300
      AG_MOTHER = 'CAVE'                                                  300
      AG_Z = -FPDG_ZDIS-FPDG_ZLEN/2                                       300
      AG_THETAZ = 180                                                     300
      AG_KONLY = 'MANY'                                                   300
      AG_TITLE = 'FPDM'                                                   300
      AG_EXNAME = 'POSITION'                                              300
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                300
      CALL AXPOSITION                                                     300
C *                                                                       300
      ENDIF                                                               300
C Check IPRIN>=1                                                          301
      IF (IPRIN.GE.1) WRITE(LOUT,5040)                                    301
5040  FORMAT('FPDMGEO done')                                              302
      ENDIF                                                               304
*
* ----------------------------------------------------------------------------
      CALL AGSENDM                                                        307
      RETURN                                                              307
C Begin Local Procedure FPDM                                              307
C FPDM IS ONE FPD WEST VOLUME                                             307
5020  CONTINUE                                                            307
         AG_TITLE='FPDM'                                                  307
         AG_MODULE=MODULE                                                 307
         AG_MARK='V00'                                                    307
         CALL AGSPUSH                                                     307
         AG_MODULE=MODULE                                                 307
C    *                                                                    307
C    Geant MATERIAL : AIR ===> inherited arguments used <===              308
         CALL AGSRESET                                                    308
         AG_TITLE = 'AIR'                                                 308
         AG_EXNAME = 'MATERIAL'                                           308
         AG_PARLIST = 'NONE'                                              308
         CALL AXMATERIAL                                                  308
C    *                                                                    308
C    Geant MATERIAL : FPDM_AIR ===> ISVOL=0 <===                          308
         CALL AGSRESET                                                    309
         AG_ISVOL = 0                                                     309
         AG_TITLE = 'FPDM_AIR'                                            309
         AG_EXNAME = 'MATERIAL'                                           309
         AG_PARLIST = 'ISVOL'                                             309
         CALL AXMATERIAL                                                  309
C    *                                                                    309
c$$$  Medium    standard
C    Geant ATTRIBUTE : PCON ===> SEEN=1 COLO=7 ! LIGHTBLUE <===           311
         CALL AGSRESET                                                    311
         AG_SEEN = 1                                                      311
C    LIGHTBLUE                                                            311
         AG_COLO = 7                                                      311
         AG_TITLE = 'PCON'                                                311
         AG_EXNAME = 'ATTRIBUTE'                                          311
         AG_PARLIST = 'SEEN_COLO'                                         311
         CALL AXATTRIBUTE                                                 311
C    *                                                                    311
C    Geant SHAPE : TUBE ===> DZ=FPDG_ZLEN/2 RMIN=FPDG_RMIN RMAX=FPDG_RAD  311
C    IUS <===                                                             311
         CALL AGSRESET                                                    312
         AG_DZ = FPDG_ZLEN/2                                              312
         AG_RMIN = FPDG_RMIN                                              312
         AG_RMAX = FPDG_RADIUS                                            312
         AG_TITLE = 'TUBE'                                                312
         AG_EXNAME = 'SHAPE'                                              312
         AG_PARLIST = 'DZ_RMIN_RMAX'                                      312
         CALL AXSHAPE                                                     312
C    *                                                                    312
* ECAL sector: dimensions are taken from original Endcap emcs_zorig and emcs_zend!!!
         DIFF = FMCC_ZORIG - FMCS_ZORIG                                   315
         CENTER = (FMCC_ZORIG+FMCC_ZEND)/2                                316
         TAN_UPP = TANF(FMCS_ETAMIN)                                      317
         TAN_LOW = TANF(FMCS_ETAMAX)                                      318
         RSHIFT = FMCS_HUB * SQRT(1. + TAN_LOW*TAN_LOW)                   319
         DUP = FMCS_RMSHIFT*TAN_UPP                                       320
         DD = FMCS_RMSHIFT*TAN_LOW                                        321
         D2 = RSHIFT + DD                                                 322
         ZDIS = FMCC_ZORIG-FPDG_ZDIS                                      323
         DPHI = (FMCS_PHIMAX-FMCS_PHIMIN)/FMCS_NSECTOR                    324
         FWOLDZ = (FMCC_ZEND-FMCC_ZORIG)                                  325
C    Create FMSS :                                                        329
C    Call To Local Procedure FMSS                                         329
         ASSIGN 5060 TO BLOCK_FMSS                                        329
         GO TO  5050                                                      329
5060     CONTINUE                                                         329
C    *                                                                    329
C    Geant POSITION : FMSS ===> Z=-FPDG_ZLEN/2+FWOLDZ/2+ZDIS, X=FMCC_XOF  329
C    FSET, ALPHAZ=180 <===                                                329
         CALL AGSRESET                                                    329
         AG_Z = -FPDG_ZLEN/2+FWOLDZ/2+ZDIS                                329
         AG_X = FMCC_XOFFSET                                              329
         AG_ALPHAN=AG_ALPHAN+1                                            329
         AG_ALPHAK(AG_ALPHAN)=3                                           329
         AG_ALPHAL(AG_ALPHAN)=180                                         329
         AG_TITLE = 'FMSS'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'Z_X_ALPHAZ'                                        329
         CALL AXPOSITION                                                  329
C    *                                                                    329
         IF (SECTION .GT. FMCC_ZEND) THEN                                 331
         WRITE(LOUT,5070)SECTION,FMCC_ZEND                                332
5070     FORMAT(' FPDMGEO error: sum of sections exceeds maximum ',       333
     *   2F12.4)                                                          333
         ENDIF                                                            334
* PbG detectors
* full half dimensions of FPD assembly
         ZTOT = (FLGG_DEPTH+FLGG_ALTHICK+FLGG_MUMETDZ)/2.0                338
         RTOT = (FLGG_NETA*FLGG_WIDTH + (FLGG_NETA+1)*FLGG_DGAP)/2.0      339
C    *                                                                    341
         IPOS=1                                                           341
            GO TO 5083                                                    341
5081        IPOS=IPOS+1                                                   341
5083        IF(IPOS-(3).GT.0) GO TO 5082                                  341
C       use FLGC bank                                                     342
            BPATH_FLGC='FLGC*'                                            342
            IDNUM_FLGC(1)=0                                               342
            FLGC_ICOPY=IPOS                                               342
            CALL AGDGETP('FPDMGEO','*',-999,'FLGC',NFLGC_ICOPY,           342
     *      RE_FLGC_ICOPY,BPATH_FLGC,IDNUM_FLGC,1,MAP_FLGC,NAMES_FLGC,    342
     *      BANK_FLGC,LEN_FLGC,LLEN_FLGC,LINK_FLGC,FLAG_FLGC)             342
            ALP = 180-IPOS*90                                             343
* in FPDM mother z positions start from its center, its face is at -zlen/2
            ZPOS = -FPDG_ZLEN/2+ZTOT                                      345
            ZDIS = FLGC_ZPOS-FPDG_ZDIS                                    346
C       Check IPOS==2                                                     347
            IF (IPOS.EQ.2) THEN                                           347
C       Create FLGD :                                                     351
C       Call To Local Procedure FLGD                                      351
            ASSIGN 5100 TO BLOCK_FLGD                                     351
            GO TO  5090                                                   351
5100        CONTINUE                                                      351
C       *                                                                 351
C       Geant POSITION : FLGD ===> Z=ZPOS+ZDIS, X=(FLGC_RDIS+RTOT)*COS(A  351
C       LP*DEGRAD), Y=(FLGC_RDIS+RTOT)*SIN(ALP*DEGRAD), ALPHAY=FLGC_DIPA  351
C       NG <===                                                           351
            CALL AGSRESET                                                 351
            AG_Z = ZPOS+ZDIS                                              351
            AG_X = (FLGC_RDIS+RTOT)*COS(ALP*DEGRAD)                       351
            AG_Y = (FLGC_RDIS+RTOT)*SIN(ALP*DEGRAD)                       351
            AG_ALPHAN=AG_ALPHAN+1                                         351
            AG_ALPHAK(AG_ALPHAN)=2                                        351
            AG_ALPHAL(AG_ALPHAN)=FLGC_DIPANG                              351
            AG_TITLE = 'FLGD'                                             351
            AG_EXNAME = 'POSITION'                                        351
            AG_PARLIST = 'Z_X_Y_ALPHAY'                                   351
            CALL AXPOSITION                                               351
C       *                                                                 351
            ELSE                                                          351
C       Create FLGD :                                                     356
C       Call To Local Procedure FLGD                                      356
            ASSIGN 5110 TO BLOCK_FLGD                                     356
            GO TO  5090                                                   356
5110        CONTINUE                                                      356
C       *                                                                 356
C       Geant POSITION : FLGD ===> Z=ZPOS+ZDIS, X=(FLGC_RDIS+RTOT)*COS(A  356
C       LP*DEGRAD), Y=(FLGC_RDIS+RTOT)*SIN(ALP*DEGRAD), ALPHAZ=180-IPOS*  356
C       90 <===                                                           356
            CALL AGSRESET                                                 356
            AG_Z = ZPOS+ZDIS                                              356
            AG_X = (FLGC_RDIS+RTOT)*COS(ALP*DEGRAD)                       356
            AG_Y = (FLGC_RDIS+RTOT)*SIN(ALP*DEGRAD)                       356
            AG_ALPHAN=AG_ALPHAN+1                                         356
            AG_ALPHAK(AG_ALPHAN)=3                                        356
            AG_ALPHAL(AG_ALPHAN)=180-IPOS*90                              356
            AG_TITLE = 'FLGD'                                             356
            AG_EXNAME = 'POSITION'                                        356
            AG_PARLIST = 'Z_X_Y_ALPHAZ'                                   356
            CALL AXPOSITION                                               356
C       *                                                                 356
            ENDIF                                                         356
         GO TO 5081                                                       358
5082     CONTINUE                                                         358
5021  CALL AGSPOPB                                                        359
      GO TO BLOCK_FPDM, (5020 ,5030)                                      359
C End of Local Procedure                                                  359
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
C Begin Local Procedure FMSS                                              362
C FMSS IS STEEL SUPPORT OF THE ENDCAP MODULE                              362
5050  CONTINUE                                                            362
         AG_TITLE='FMSS'                                                  362
         AG_MODULE=MODULE                                                 362
         AG_MARK='V00'                                                    362
         CALL AGSPUSH                                                     362
         AG_MODULE=MODULE                                                 362
C    *                                                                    362
C    Geant ATTRIBUTE : FMSS ===> SEEN=1 COLO=1 ! BLACK <===               363
         CALL AGSRESET                                                    363
         AG_SEEN = 1                                                      363
C    BLACK                                                                363
         AG_COLO = 1                                                      363
         AG_TITLE = 'FMSS'                                                363
         AG_EXNAME = 'ATTRIBUTE'                                          363
         AG_PARLIST = 'SEEN_COLO'                                         363
         CALL AXATTRIBUTE                                                 363
C    *                                                                    363
C    Geant MATERIAL : IRON ===> inherited arguments used <===             363
         CALL AGSRESET                                                    364
         AG_TITLE = 'IRON'                                                364
         AG_EXNAME = 'MATERIAL'                                           364
         AG_PARLIST = 'NONE'                                              364
         CALL AXMATERIAL                                                  364
C    *                                                                    364
C    Geant MATERIAL : FPDM_IRON ===> ISVOL=0 <===                         364
         CALL AGSRESET                                                    365
         AG_ISVOL = 0                                                     365
         AG_TITLE = 'FPDM_IRON'                                           365
         AG_EXNAME = 'MATERIAL'                                           365
         AG_PARLIST = 'ISVOL'                                             365
         CALL AXMATERIAL                                                  365
C    *                                                                    365
C    Geant SHAPE : CONS ===> DZ=(FMCC_ZEND-FMCC_ZORIG)/2, RMN1=FMCS_ZORI  365
C    G*TAN_LOW-D2 RMN2=FMCS_ZEND*TAN_LOW-D2, RMX1=FMCS_ZORIG*TAN_UPP+DUP  365
C     RMX2=FMCS_ZEND*TAN_UPP+DUP, PHI1=FMCS_PHIMIN/FMCS_NSUPSEC, PHI2=+F  365
C    MCS_PHIMAX/FMCS_NSUPSEC <===                                         365
         CALL AGSRESET                                                    370
         AG_DZ = (FMCC_ZEND-FMCC_ZORIG)/2                                 370
         AG_RMN1 = FMCS_ZORIG*TAN_LOW-D2                                  370
         AG_RMN2 = FMCS_ZEND*TAN_LOW-D2                                   370
         AG_RMX1 = FMCS_ZORIG*TAN_UPP+DUP                                 370
         AG_RMX2 = FMCS_ZEND*TAN_UPP+DUP                                  370
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               370
         AG_PHI2 = +FMCS_PHIMAX/FMCS_NSUPSEC                              370
         AG_TITLE = 'CONS'                                                370
         AG_EXNAME = 'SHAPE'                                              370
         AG_PARLIST = 'DZ_RMN1_RMN2_RMX1_RMX2_PHI1_PHI2'                  370
         CALL AXSHAPE                                                     370
C    *                                                                    370
*
C    Create FMOD :                                                        372
C    Call To Local Procedure FMOD                                         372
         ASSIGN 5130 TO BLOCK_FMOD                                        372
         GO TO  5120                                                      372
5130     CONTINUE                                                         372
C    *                                                                    372
C    Geant POSITION : FMOD ===> inherited arguments used <===             372
         CALL AGSRESET                                                    372
         AG_TITLE = 'FMOD'                                                372
         AG_EXNAME = 'POSITION'                                           372
         AG_PARLIST = 'NONE'                                              372
         CALL AXPOSITION                                                  372
C    *                                                                    372
5051  CALL AGSPOPB                                                        374
      GO TO BLOCK_FMSS, (5050 ,5060)                                      374
C End of Local Procedure                                                  374
* ----------------------------------------------------------------------------
C Begin Local Procedure FMOD                                              376
C FMOD IS ONE MODULE OF THE EM ENDCAP                                     376
5120  CONTINUE                                                            376
         AG_TITLE='FMOD'                                                  376
         AG_MODULE=MODULE                                                 376
         AG_MARK='V00'                                                    376
         CALL AGSPUSH                                                     376
         AG_MODULE=MODULE                                                 376
C    *                                                                    376
C    Geant ATTRIBUTE : FMOD ===> SEEN=1 COLO=3 ! GREEN <===               377
         CALL AGSRESET                                                    377
         AG_SEEN = 1                                                      377
C    GREEN                                                                377
         AG_COLO = 3                                                      377
         AG_TITLE = 'FMOD'                                                377
         AG_EXNAME = 'ATTRIBUTE'                                          377
         AG_PARLIST = 'SEEN_COLO'                                         377
         CALL AXATTRIBUTE                                                 377
C    *                                                                    377
C    Geant MATERIAL : FPDM_AIR ===> inherited arguments used <===         377
         CALL AGSRESET                                                    378
         AG_TITLE = 'FPDM_AIR'                                            378
         AG_EXNAME = 'MATERIAL'                                           378
         AG_PARLIST = 'NONE'                                              378
         CALL AXMATERIAL                                                  378
C    *                                                                    378
C    Geant SHAPE : CONS ===> DZ=(FMCC_ZEND-FMCC_ZORIG)/2, RMN1=FMCS_ZORI  378
C    G*TAN_LOW-DD RMN2=FMCS_ZEND*TAN_LOW-DD, RMX1=FMCS_ZORIG*TAN_UPP+DUP  378
C     RMX2=FMCS_ZEND*TAN_UPP+DUP <===                                     378
         CALL AGSRESET                                                    381
         AG_DZ = (FMCC_ZEND-FMCC_ZORIG)/2                                 381
         AG_RMN1 = FMCS_ZORIG*TAN_LOW-DD                                  381
         AG_RMN2 = FMCS_ZEND*TAN_LOW-DD                                   381
         AG_RMX1 = FMCS_ZORIG*TAN_UPP+DUP                                 381
         AG_RMX2 = FMCS_ZEND*TAN_UPP+DUP                                  381
         AG_TITLE = 'CONS'                                                381
         AG_EXNAME = 'SHAPE'                                              381
         AG_PARLIST = 'DZ_RMN1_RMN2_RMX1_RMX2'                            381
         CALL AXSHAPE                                                     381
C    *                                                                    381
*
*    Running parameter 'section' contains the position of the current section
*     It should not be modified in daughters, use 'current' variable instead.
*     SecWid is used in all 'CONS' daughters to define dimensions.
*
         SECTION = FMCC_ZORIG                                             387
         SECWID = FMCS_FRONT                                              388
C    Create FFLP :                                                        389
C    Call To Local Procedure FFLP                                         389
         ASSIGN 5150 TO BLOCK_FFLP                                        389
         GO TO  5140                                                      389
5150     CONTINUE                                                         389
C    *                                                                    389
C    Geant POSITION : FFLP ===> Z=SECTION-CENTER+SECWID/2 <===            389
         CALL AGSRESET                                                    389
         AG_Z = SECTION-CENTER+SECWID/2                                   389
         AG_TITLE = 'FFLP'                                                389
         AG_EXNAME = 'POSITION'                                           389
         AG_PARLIST = 'Z'                                                 389
         CALL AXPOSITION                                                  389
C    *                                                                    389
         SECTION = SECTION + SECWID                                       389
*
C    *                                                                    392
         I_SECTION=1                                                      392
            GO TO 5163                                                    392
5161        I_SECTION=I_SECTION+1                                         392
5163        IF(I_SECTION-(FMCS_NSECTION).GT.0) GO TO 5162                 392
*        number of section may be less then Fmcs_Nsection, keep the last available
C       use FSEC bank                                                     394
            BPATH_FSEC='FSEC*'                                            394
            IDNUM_FSEC(1)=0                                               394
            FSEC_ISECT=I_SECTION                                          394
            CALL AGDGETP('FPDMGEO','*',ISTAT,'FSEC',NFSEC_ISECT,          394
     *      RE_FSEC_ISECT,BPATH_FSEC,IDNUM_FSEC,1,MAP_FSEC,NAMES_FSEC,    394
     *      BANK_FSEC,LEN_FSEC,LLEN_FSEC,LINK_FSEC,FLAG_FSEC)             394
*
            SECWID = FSEC_CELL*FSEC_NLAYER                                396
C       Check I_SECTION == NINT(FMCS_NSECTION)                            397
C       LAST SECTION                                                      397
            IF (I_SECTION .EQ. NINT(FMCS_NSECTION)) THEN                  397
            SECWID = SECWID - FMCS_PLATE - 2*FMCS_PLATES                  398
            ENDIF                                                         399
C       Create FPSE :                                                     400
C       Call To Local Procedure FPSE                                      400
            ASSIGN 5180 TO BLOCK_FPSE                                     400
            GO TO  5170                                                   400
5180        CONTINUE                                                      400
C       *                                                                 400
C       Geant POSITION : FPSE ===> Z=SECTION-CENTER+SECWID/2 <===         400
            CALL AGSRESET                                                 400
            AG_Z = SECTION-CENTER+SECWID/2                                400
            AG_TITLE = 'FPSE'                                             400
            AG_EXNAME = 'POSITION'                                        400
            AG_PARLIST = 'Z'                                              400
            CALL AXPOSITION                                               400
C       *                                                                 400
            SECTION = SECTION + SECWID                                    400
*
C       Check I_SECTION == 2                                              403
C       SHOWER MAX SECTION                                                403
            IF (I_SECTION .EQ. 2) THEN                                    403
            SECWID = FMCS_GAPSMD                                          404
C       Create FSHM :                                                     405
C       Call To Local Procedure FSHM                                      405
            ASSIGN 5200 TO BLOCK_FSHM                                     405
            GO TO  5190                                                   405
5200        CONTINUE                                                      405
C       *                                                                 405
C       Geant POSITION : FSHM ===> Z=SECTION-CENTER+SECWID/2 <===         405
            CALL AGSRESET                                                 405
            AG_Z = SECTION-CENTER+SECWID/2                                405
            AG_TITLE = 'FSHM'                                             405
            AG_EXNAME = 'POSITION'                                        405
            AG_PARLIST = 'Z'                                              405
            CALL AXPOSITION                                               405
C       *                                                                 405
            SECTION = SECTION + SECWID                                    405
            ENDIF                                                         407
         GO TO 5161                                                       408
5162     CONTINUE                                                         408
         SECWID = FMCS_PLATEB                                             410
C    Create FSSP :                                                        411
C    Call To Local Procedure FSSP                                         411
         ASSIGN 5220 TO BLOCK_FSSP                                        411
         GO TO  5210                                                      411
5220     CONTINUE                                                         411
C    *                                                                    411
C    Geant POSITION : FSSP ===> Z=SECTION-CENTER+SECWID/2 <===            411
         CALL AGSRESET                                                    411
         AG_Z = SECTION-CENTER+SECWID/2                                   411
         AG_TITLE = 'FSSP'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'Z'                                                 411
         CALL AXPOSITION                                                  411
C    *                                                                    411
         SECTION = SECTION + SECWID                                       411
5121  CALL AGSPOPB                                                        413
      GO TO BLOCK_FMOD, (5120 ,5130)                                      413
C End of Local Procedure                                                  413
* ----------------------------------------------------------------------------
C Begin Local Procedure FPSE                                              415
C FPSE IS A SINGLE EM SECTION                                             415
5170  CONTINUE                                                            415
         AG_TITLE='FPSE'                                                  415
         AG_MODULE=MODULE                                                 415
         AG_MARK='V00'                                                    415
         CALL AGSPUSH                                                     415
         AG_MODULE=MODULE                                                 415
C    *                                                                    415
C    Geant ATTRIBUTE : FPSE ===> SEEN=1 COLO=1 <===                       416
         CALL AGSRESET                                                    416
         AG_SEEN = 1                                                      416
         AG_COLO = 1                                                      416
         AG_TITLE = 'FPSE'                                                416
         AG_EXNAME = 'ATTRIBUTE'                                          416
         AG_PARLIST = 'SEEN_COLO'                                         416
         CALL AXATTRIBUTE                                                 416
C    *                                                                    416
C    Geant MATERIAL : AIR ===> inherited arguments used <===              416
         CALL AGSRESET                                                    417
         AG_TITLE = 'AIR'                                                 417
         AG_EXNAME = 'MATERIAL'                                           417
         AG_PARLIST = 'NONE'                                              417
         CALL AXMATERIAL                                                  417
C    *                                                                    417
C    Geant MATERIAL : FPDM_AIR_FPSE ===> ISVOL=0 <===                     417
         CALL AGSRESET                                                    418
         AG_ISVOL = 0                                                     418
         AG_TITLE = 'FPDM_AIR_FPSE'                                       418
         AG_EXNAME = 'MATERIAL'                                           418
         AG_PARLIST = 'ISVOL'                                             418
         CALL AXMATERIAL                                                  418
C    *                                                                    418
c$$$      Material Air_FPSE isVol=0
c$$$      Medium standard
*
C    Geant SHAPE : CONS ===> DZ=SECWID/2, RMN1=(SECTION-DIFF)*TAN_LOW-DD  424
C     RMN2=(SECTION+SECWID-DIFF)*TAN_LOW-DD, RMX1=(SECTION-DIFF)*TAN_UPP  424
C    +DUP RMX2=(SECTION+SECWID-DIFF)*TAN_UPP+DUP <===                     424
         CALL AGSRESET                                                    424
         AG_DZ = SECWID/2                                                 424
         AG_RMN1 = (SECTION-DIFF)*TAN_LOW-DD                              424
         AG_RMN2 = (SECTION+SECWID-DIFF)*TAN_LOW-DD                       424
         AG_RMX1 = (SECTION-DIFF)*TAN_UPP+DUP                             424
         AG_RMX2 = (SECTION+SECWID-DIFF)*TAN_UPP+DUP                      424
         AG_TITLE = 'CONS'                                                424
         AG_EXNAME = 'SHAPE'                                              424
         AG_PARLIST = 'DZ_RMN1_RMN2_RMX1_RMX2'                            424
         CALL AXSHAPE                                                     424
C    *                                                                    424
C    Check I_SECTION==1                                                   424
         IF (I_SECTION.EQ.1) THEN                                         425
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00001)                           426
         CALL GSTPAR (AG_IMED,'CUTELE',0.00001)                           427
         ELSE                                                             428
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00008)                           429
         CALL GSTPAR (AG_IMED,'CUTELE',0.001)                             430
         CALL GSTPAR (AG_IMED,'BCUTE' ,0.0001)                            431
         END IF                                                           432
*
C    *                                                                    434
         ISEC=1                                                           434
            GO TO 5233                                                    434
5231        ISEC=ISEC+1                                                   434
5233        IF(ISEC-(NINT(FMCS_NSLICES)).GT.0) GO TO 5232                 434
C       Create FTOW :                                                     435
C       Call To Local Procedure FTOW                                      435
            ASSIGN 5250 TO BLOCK_FTOW                                     435
            GO TO  5240                                                   435
5250        CONTINUE                                                      435
C       *                                                                 435
C       Geant POSITION : FTOW ===> ALPHAZ=(ISEC-FMCS_NSLICES/2.0-0.5)*DP  435
C       HI <===                                                           435
            CALL AGSRESET                                                 435
            AG_ALPHAN=AG_ALPHAN+1                                         435
            AG_ALPHAK(AG_ALPHAN)=3                                        435
            AG_ALPHAL(AG_ALPHAN)=(ISEC-FMCS_NSLICES/2.0-0.5)*DPHI         435
            AG_TITLE = 'FTOW'                                             435
            AG_EXNAME = 'POSITION'                                        435
            AG_PARLIST = 'ALPHAZ'                                         435
            CALL AXPOSITION                                               435
C       *                                                                 435
         GO TO 5231                                                       436
5232     CONTINUE                                                         436
5171  CALL AGSPOPB                                                        437
      GO TO BLOCK_FPSE, (5170 ,5180)                                      437
C End of Local Procedure                                                  437
*---------------------------------------------------------------------------
C Begin Local Procedure FTOW                                              439
C FTOW IS AN INDIVIDUAL 1/60 PHI EM TOWER (SECTION IN FACT)               439
5240  CONTINUE                                                            439
         AG_TITLE='FTOW'                                                  439
         AG_MODULE=MODULE                                                 439
         AG_MARK='V00'                                                    439
         CALL AGSPUSH                                                     439
         AG_MODULE=MODULE                                                 439
C    *                                                                    439
C    Geant ATTRIBUTE : FTOW ===> SEEN=1 COLO=1 <===                       440
         CALL AGSRESET                                                    440
         AG_SEEN = 1                                                      440
         AG_COLO = 1                                                      440
         AG_TITLE = 'FTOW'                                                440
         AG_EXNAME = 'ATTRIBUTE'                                          440
         AG_PARLIST = 'SEEN_COLO'                                         440
         CALL AXATTRIBUTE                                                 440
C    *                                                                    440
C    Geant SHAPE : CONS ===> PHI1=FMCS_PHIMIN/FMCS_NSECTOR, PHI2=FMCS_PH  440
C    IMAX/FMCS_NSECTOR <===                                               440
         CALL AGSRESET                                                    442
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSECTOR                               442
         AG_PHI2 = FMCS_PHIMAX/FMCS_NSECTOR                               442
         AG_TITLE = 'CONS'                                                442
         AG_EXNAME = 'SHAPE'                                              442
         AG_PARLIST = 'PHI1_PHI2'                                         442
         CALL AXSHAPE                                                     442
C    *                                                                    442
         CURRENT = SECTION                                                442
C    *                                                                    444
         IS=1                                                             444
            GO TO 5263                                                    444
5261        IS=IS+1                                                       444
5263        IF(IS-(FSEC_NLAYER).GT.0) GO TO 5262                          444
*        define actual Plate and cell thickness:
            PLATE = FMCS_PLATE + 2*FMCS_PLATES                            446
            GAP = FSEC_CELL - PLATE - FSEC_SCINT(FMCS_TYPE)               447
            CELL = FSEC_CELL                                              448
* below: change from 3 to nint(Fmcs_Nsection)=8: 08/03/2001
C       Check IS==NINT(FSEC_NLAYER) & I_SECTION == NINT(FMCS_NSECTION)    451
            IF (IS.EQ.NINT(FSEC_NLAYER) .AND. I_SECTION .EQ.              451
     *      NINT(FMCS_NSECTION)) THEN                                     451
            CELL = FSEC_CELL - PLATE                                      452
            PLATE=0                                                       453
            ENDIF                                                         454
*
C       Create FPER :                                                     456
C       Call To Local Procedure FPER                                      456
            ASSIGN 5280 TO BLOCK_FPER                                     456
            GO TO  5270                                                   456
5280        CONTINUE                                                      456
C       *                                                                 456
C       Geant POSITION : FPER ===> Z=-SECWID/2+(IS-1)*FSEC_CELL+CELL/2 <  457
C       ===                                                               457
            CALL AGSRESET                                                 457
            AG_Z = -SECWID/2+(IS-1)*FSEC_CELL+CELL/2                      457
            AG_TITLE = 'FPER'                                             457
            AG_EXNAME = 'POSITION'                                        457
            AG_PARLIST = 'Z'                                              457
            CALL AXPOSITION                                               457
C       *                                                                 457
            CURRENT = CURRENT + CELL                                      457
         GO TO 5261                                                       459
5262     CONTINUE                                                         459
*
5241  CALL AGSPOPB                                                        461
      GO TO BLOCK_FTOW, (5240 ,5250)                                      461
C End of Local Procedure                                                  461
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FPER                                              463
C FPER IS A EM SESECTION PERIOD (SUPER LAYER)                             463
5270  CONTINUE                                                            463
         AG_TITLE='FPER'                                                  463
         AG_MODULE=MODULE                                                 463
         AG_MARK='V00'                                                    463
         CALL AGSPUSH                                                     463
         AG_MODULE=MODULE                                                 463
C    *                                                                    463
*
C    Geant MATERIAL : FPDM_AIR ===> inherited arguments used <===         465
         CALL AGSRESET                                                    465
         AG_TITLE = 'FPDM_AIR'                                            465
         AG_EXNAME = 'MATERIAL'                                           465
         AG_PARLIST = 'NONE'                                              465
         CALL AXMATERIAL                                                  465
C    *                                                                    465
C    Geant ATTRIBUTE : FPER ===> SEEN=1 COLO=1 <===                       465
         CALL AGSRESET                                                    466
         AG_SEEN = 1                                                      466
         AG_COLO = 1                                                      466
         AG_TITLE = 'FPER'                                                466
         AG_EXNAME = 'ATTRIBUTE'                                          466
         AG_PARLIST = 'SEEN_COLO'                                         466
         CALL AXATTRIBUTE                                                 466
C    *                                                                    466
C    Geant SHAPE : CONS ===> DZ=CELL/2, PHI1=FMCS_PHIMIN/FMCS_NSECTOR, P  466
C    HI2=+FMCS_PHIMAX/FMCS_NSECTOR, RMN1=(CURRENT-DIFF)*TAN_LOW-DD RMN2=  466
C    (CURRENT+CELL-DIFF)*TAN_LOW-DD, RMX1=(CURRENT-DIFF)*TAN_UPP+DUP RMX  466
C    2=(CURRENT+CELL-DIFF)*TAN_UPP+DUP <===                               466
         CALL AGSRESET                                                    471
         AG_DZ = CELL/2                                                   471
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSECTOR                               471
         AG_PHI2 = +FMCS_PHIMAX/FMCS_NSECTOR                              471
         AG_RMN1 = (CURRENT-DIFF)*TAN_LOW-DD                              471
         AG_RMN2 = (CURRENT+CELL-DIFF)*TAN_LOW-DD                         471
         AG_RMX1 = (CURRENT-DIFF)*TAN_UPP+DUP                             471
         AG_RMX2 = (CURRENT+CELL-DIFF)*TAN_UPP+DUP                        471
         AG_TITLE = 'CONS'                                                471
         AG_EXNAME = 'SHAPE'                                              471
         AG_PARLIST = 'DZ_PHI1_PHI2_RMN1_RMN2_RMX1_RMX2'                  471
         CALL AXSHAPE                                                     471
C    *                                                                    471
*
* --- Divide module (section) into radial blocks
*
         CURCL = CURRENT+CELL/2                                           475
C    *                                                                    476
         IE=1                                                             476
            GO TO 5293                                                    476
5291        IE=IE+1                                                       476
5293        IF(IE-(FETR_NETA).GT.0) GO TO 5292                            476
            ETABOT = FETR_ETABIN(IE)                                      477
            ETATOP = FETR_ETABIN(IE+1)                                    478
C       LOWER SLICE                                                       480
            IF (IE .EQ. 1) THEN                                           480
            RBOT=(CURRENT-DIFF)*TAN_LOW                                   481
            ELSE                                                          482
            RBOT=(CURCL-DIFF)*TANF(ETABOT)                                483
            ENDIF                                                         484
*
C       ORDINARY SCI LAYER                                                486
            IF (PLATE .GT. 0) THEN                                        486
            RTOP=MIN((CURCL-DIFF)*TANF(ETATOP),                           487
     *      ((CURRENT-DIFF)*TAN_UPP+DUP))                                 487
C       LAST SCI LAYER IN SECTION                                         488
            ELSE                                                          488
            RTOP=MIN((CURCL-DIFF)*TANF(ETATOP),                           489
     *      ((CURRENT-DIFF)*TAN_UPP+DUP))                                 489
            ENDIF                                                         490
C       Skip Unless RBOT<RTOP                                             491
            IF (RBOT.GE.RTOP)GO TO 5291                                   491
*
            XX=TAN(PI*FMCS_PHIMAX/180.0/FMCS_NSECTOR)                     493
            YY=COS(PI*FMCS_PHIMAX/180.0/FMCS_NSECTOR)                     494
C       Create FTAR :                                                     495
C       Call To Local Procedure FTAR                                      495
            ASSIGN 5310 TO BLOCK_FTAR                                     495
            GO TO  5300                                                   495
5310        CONTINUE                                                      495
C       *                                                                 495
C       Geant POSITION : FTAR ===> X=(RBOT+RTOP)/2 ORT=YZX <===           495
            CALL AGSRESET                                                 495
            AG_X = (RBOT+RTOP)/2                                          495
            AG_ALPHAN=AG_ALPHAN+1                                         495
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              495
            AG_TITLE = 'FTAR'                                             495
            AG_EXNAME = 'POSITION'                                        495
            AG_PARLIST = 'X_ORT'                                          495
            CALL AXPOSITION                                               495
C       *                                                                 495
*         prin0 ie,EtaTop,EtaBot,rbot,rtop
*         (' EPER : ie,EtaTop,EtaBot,rbot,rtop ',i3,4F12.4)
         GO TO 5291                                                       498
5292     CONTINUE                                                         498
*
5271  CALL AGSPOPB                                                        500
      GO TO BLOCK_FPER, (5270 ,5280)                                      500
C End of Local Procedure                                                  500
*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FTAR                                              502
C FTAR IS ONE CELL OF SCINTILLATOR, FIBER AND LAMINATED LEAD              502
5300  CONTINUE                                                            502
         AG_TITLE='FTAR'                                                  502
         AG_MODULE=MODULE                                                 502
         AG_MARK='V00'                                                    502
         CALL AGSPUSH                                                     502
         AG_MODULE=MODULE                                                 502
C    *                                                                    502
*
C    Geant ATTRIBUTE : FTAR ===> SEEN=1 COLO=2 ! VIOLET <===              504
         CALL AGSRESET                                                    504
         AG_SEEN = 1                                                      504
C    VIOLET                                                               504
         AG_COLO = 2                                                      504
         AG_TITLE = 'FTAR'                                                504
         AG_EXNAME = 'ATTRIBUTE'                                          504
         AG_PARLIST = 'SEEN_COLO'                                         504
         CALL AXATTRIBUTE                                                 504
C    *                                                                    504
*     local z goes along the radius, y is the thickness
C    Geant SHAPE : TRD1 ===> DY=CELL/2 DZ=(RTOP-RBOT)/2, DX1=RBOT*XX-FMC  508
C    S_GAPCEL/YY, DX2=RTOP*XX-FMCS_GAPCEL/YY <===                         508
         CALL AGSRESET                                                    508
         AG_DY = CELL/2                                                   508
         AG_DZ = (RTOP-RBOT)/2                                            508
         AG_DX1 = RBOT*XX-FMCS_GAPCEL/YY                                  508
         AG_DX2 = RTOP*XX-FMCS_GAPCEL/YY                                  508
         AG_TITLE = 'TRD1'                                                508
         AG_EXNAME = 'SHAPE'                                              508
         AG_PARLIST = 'DY_DZ_DX1_DX2'                                     508
         CALL AXSHAPE                                                     508
C    *                                                                    508
*
         G10 = FMCS_GTEN                                                  510
C    Create FSCI :                                                        511
C    Call To Local Procedure FSCI                                         511
         ASSIGN 5330 TO BLOCK_FSCI                                        511
         GO TO  5320                                                      511
5330     CONTINUE                                                         511
C    *                                                                    511
C    Geant POSITION : FSCI ===> Y=(-CELL + FSEC_SCINT(FMCS_TYPE))/2 <===  511
         CALL AGSRESET                                                    511
         AG_Y = (-CELL + FSEC_SCINT(FMCS_TYPE))/2                         511
         AG_TITLE = 'FSCI'                                                511
         AG_EXNAME = 'POSITION'                                           511
         AG_PARLIST = 'Y'                                                 511
         CALL AXPOSITION                                                  511
C    *                                                                    511
C    Create FGTN :                                                        512
C    Call To Local Procedure FGTN                                         512
         ASSIGN 5350 TO BLOCK_FGTN                                        512
         GO TO  5340                                                      512
5350     CONTINUE                                                         512
C    *                                                                    512
C    Geant POSITION : FGTN ===> Y=(-CELL+G10)/2+FSEC_SCINT(FMCS_TYPE) <=  512
C    ==                                                                   512
         CALL AGSRESET                                                    512
         AG_Y = (-CELL+G10)/2+FSEC_SCINT(FMCS_TYPE)                       512
         AG_TITLE = 'FGTN'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'Y'                                                 512
         CALL AXPOSITION                                                  512
C    *                                                                    512
C    Check PLATE>0                                                        512
         IF (PLATE.GT.0) THEN                                             513
C    Create FXFP :                                                        514
C    Call To Local Procedure FXFP                                         514
         ASSIGN 5370 TO BLOCK_FXFP                                        514
         GO TO  5360                                                      514
5370     CONTINUE                                                         514
C    *                                                                    514
C    Geant POSITION : FXFP ===> Y=(CELL + FMCS_PLATES)/2 - PLATE <===     514
         CALL AGSRESET                                                    514
         AG_Y = (CELL + FMCS_PLATES)/2 - PLATE                            514
         AG_TITLE = 'FXFP'                                                514
         AG_EXNAME = 'POSITION'                                           514
         AG_PARLIST = 'Y'                                                 514
         CALL AXPOSITION                                                  514
C    *                                                                    514
C    Create FLED :                                                        515
C    Call To Local Procedure FLED                                         515
         ASSIGN 5390 TO BLOCK_FLED                                        515
         GO TO  5380                                                      515
5390     CONTINUE                                                         515
C    *                                                                    515
C    Geant POSITION : FLED ===> Y=(CELL - PLATE)/2 <===                   515
         CALL AGSRESET                                                    515
         AG_Y = (CELL - PLATE)/2                                          515
         AG_TITLE = 'FLED'                                                515
         AG_EXNAME = 'POSITION'                                           515
         AG_PARLIST = 'Y'                                                 515
         CALL AXPOSITION                                                  515
C    *                                                                    515
C    Create FXFP :                                                        516
C    Call To Local Procedure FXFP                                         516
         ASSIGN 5400 TO BLOCK_FXFP                                        516
         GO TO  5360                                                      516
5400     CONTINUE                                                         516
C    *                                                                    516
C    Geant POSITION : FXFP ===> Y=(CELL - FMCS_PLATES)/2 <===             516
         CALL AGSRESET                                                    516
         AG_Y = (CELL - FMCS_PLATES)/2                                    516
         AG_TITLE = 'FXFP'                                                516
         AG_EXNAME = 'POSITION'                                           516
         AG_PARLIST = 'Y'                                                 516
         CALL AXPOSITION                                                  516
C    *                                                                    516
         END IF                                                           516
5301  CALL AGSPOPB                                                        518
      GO TO BLOCK_FTAR, (5300 ,5310)                                      518
C End of Local Procedure                                                  518
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FGTN                                              520
C FGTN IS THE G10 LAYER                                                   520
5340  CONTINUE                                                            520
         AG_TITLE='FGTN'                                                  520
         AG_MODULE=MODULE                                                 520
         AG_MARK='V00'                                                    520
         CALL AGSPUSH                                                     520
         AG_MODULE=MODULE                                                 520
C    *                                                                    520
*
*     G10 is about 60% SiO2 and 40% epoxy
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          523
         CALL AGSRESET                                                    523
         AG_A = 28.08                                                     523
         AG_Z = 14                                                        523
         AG_W = 0.6*1*28./60.                                             523
         AG_TITLE = 'SI'                                                  523
         AG_EXNAME = 'COMPONENT'                                          523
         AG_PARLIST = 'A_Z_W'                                             523
         CALL AXCOMPONENT                                                 523
C    *                                                                    523
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               523
         CALL AGSRESET                                                    524
         AG_A = 16                                                        524
         AG_Z = 8                                                         524
         AG_W = 0.6*2*16./60.                                             524
         AG_TITLE = 'O'                                                   524
         AG_EXNAME = 'COMPONENT'                                          524
         AG_PARLIST = 'A_Z_W'                                             524
         CALL AXCOMPONENT                                                 524
C    *                                                                    524
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              524
         CALL AGSRESET                                                    525
         AG_A = 12                                                        525
         AG_Z = 6                                                         525
         AG_W = 0.4*8*12./174.                                            525
         AG_TITLE = 'C'                                                   525
         AG_EXNAME = 'COMPONENT'                                          525
         AG_PARLIST = 'A_Z_W'                                             525
         CALL AXCOMPONENT                                                 525
C    *                                                                    525
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               525
         CALL AGSRESET                                                    526
         AG_A = 1                                                         526
         AG_Z = 1                                                         526
         AG_W = 0.4*14*1./174.                                            526
         AG_TITLE = 'H'                                                   526
         AG_EXNAME = 'COMPONENT'                                          526
         AG_PARLIST = 'A_Z_W'                                             526
         CALL AXCOMPONENT                                                 526
C    *                                                                    526
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              526
         CALL AGSRESET                                                    527
         AG_A = 16                                                        527
         AG_Z = 8                                                         527
         AG_W = 0.4*4*16./174.                                            527
         AG_TITLE = 'O'                                                   527
         AG_EXNAME = 'COMPONENT'                                          527
         AG_PARLIST = 'A_Z_W'                                             527
         CALL AXCOMPONENT                                                 527
C    *                                                                    527
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               527
         CALL AGSRESET                                                    528
         AG_DENS = 1.7                                                    528
         AG_TITLE = 'G10'                                                 528
         AG_EXNAME = 'MIXTURE'                                            528
         AG_PARLIST = 'DENS'                                              528
         CALL AXMIXTURE                                                   528
C    *                                                                    528
C    Geant ATTRIBUTE : FGTN ===> SEEN=1 COLO=4 ! BLUE <===                528
         CALL AGSRESET                                                    529
         AG_SEEN = 1                                                      529
C    BLUE                                                                 529
         AG_COLO = 4                                                      529
         AG_TITLE = 'FGTN'                                                529
         AG_EXNAME = 'ATTRIBUTE'                                          529
         AG_PARLIST = 'SEEN_COLO'                                         529
         CALL AXATTRIBUTE                                                 529
C    *                                                                    529
*     local z goes along the radius, y is the thickness
C    Geant SHAPE : TRD1 ===> DY=FMCS_GTEN/2 DZ=(RTOP-RBOT)/2 <===         531
         CALL AGSRESET                                                    531
         AG_DY = FMCS_GTEN/2                                              531
         AG_DZ = (RTOP-RBOT)/2                                            531
         AG_TITLE = 'TRD1'                                                531
         AG_EXNAME = 'SHAPE'                                              531
         AG_PARLIST = 'DY_DZ'                                             531
         CALL AXSHAPE                                                     531
C    *                                                                    531
5341  CALL AGSPOPB                                                        531
      GO TO BLOCK_FGTN, (5340 ,5350)                                      532
C End of Local Procedure                                                  532
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FSCI                                              534
C FSCI IS THE ACTIVE SCINTILLATOR (POLYSTYREN) LAYER                      534
5320  CONTINUE                                                            534
         AG_TITLE='FSCI'                                                  534
         AG_MODULE=MODULE                                                 534
         AG_MARK='V00'                                                    534
         CALL AGSPUSH                                                     534
         AG_MODULE=MODULE                                                 534
C    *                                                                    534
*
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       536
         CALL AGSRESET                                                    536
         AG_TITLE = 'POLYSTYREN'                                          536
         AG_EXNAME = 'MATERIAL'                                           536
         AG_PARLIST = 'NONE'                                              536
         CALL AXMATERIAL                                                  536
C    *                                                                    536
C    Geant MATERIAL : FPDM_POLYSTYREN ===> ISVOL=1 <===                   536
         CALL AGSRESET                                                    537
         AG_ISVOL = 1                                                     537
         AG_TITLE = 'FPDM_POLYSTYREN'                                     537
         AG_EXNAME = 'MATERIAL'                                           537
         AG_PARLIST = 'ISVOL'                                             537
         CALL AXMATERIAL                                                  537
C    *                                                                    537
C    Geant ATTRIBUTE : FSCI ===> SEEN=1 COLO=7 FILL=0 ! LIGHTBLUE <===    537
         CALL AGSRESET                                                    538
         AG_SEEN = 1                                                      538
         AG_COLO = 7                                                      538
C    LIGHTBLUE                                                            538
         AG_FILL = 0                                                      538
         AG_TITLE = 'FSCI'                                                538
         AG_EXNAME = 'ATTRIBUTE'                                          538
         AG_PARLIST = 'SEEN_COLO_FILL'                                    538
         CALL AXATTRIBUTE                                                 538
C    *                                                                    538
*     local z goes along the radius, y is the thickness
C    Geant SHAPE : TRD1 ===> DY=FSEC_SCINT(FMCS_TYPE)/2 DZ=(RTOP-RBOT)/2  540
C    -FMCS_GAPCEL <===                                                    540
         CALL AGSRESET                                                    540
         AG_DY = FSEC_SCINT(FMCS_TYPE)/2                                  540
         AG_DZ = (RTOP-RBOT)/2-FMCS_GAPCEL                                540
         AG_TITLE = 'TRD1'                                                540
         AG_EXNAME = 'SHAPE'                                              540
         AG_PARLIST = 'DY_DZ'                                             540
         CALL AXSHAPE                                                     540
C    *                                                                    540
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00008)                           540
         CALL GSTPAR (AG_IMED,'CUTELE',0.001)                             542
         CALL GSTPAR (AG_IMED,'BCUTE' ,0.0001)                            543
         CALL GSTPAR (AG_IMED,'CUTNEU',0.001)                             544
         CALL GSTPAR (AG_IMED,'CUTHAD',0.001)                             545
         CALL GSTPAR (AG_IMED,'CUTMUO',0.001)                             546
* define Birks law parameters
         CALL GSTPAR (AG_IMED,'BIRK1',1.)                                 548
         CALL GSTPAR (AG_IMED,'BIRK2',0.013)                              549
         CALL GSTPAR (AG_IMED,'BIRK3',9.6E-6)                             550
*
C    Defining Hits: BIRK:0:(0,10)                                         552
         AG_TITLE = 'FSCI'                                                552
         AG_EXNAME = 'HITS'                                               552
         AG_TYPE=AG_MAGIC                                                 552
         CALL AGSDETV('FSCI')                                             552
C    *                                                                    552
         AG_PARLIST='BIRK'                                                552
         AG_TITLE = 'DETH'                                                552
         AG_NBITS = 0                                                     552
         AG_BIN=AG_MAGIC                                                  552
         AG_OPTION='*'                                                    552
         AG_FHMIN = 0                                                     552
         AG_FHMAX = 10                                                    552
         CALL AGSDETH (0)                                                 552
C    *                                                                    552
         CALL AGSDETU('FSCI')                                             552
C    *                                                                    552
5321  CALL AGSPOPB                                                        553
      GO TO BLOCK_FSCI, (5320 ,5330)                                      553
C End of Local Procedure                                                  553
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FLED                                              555
C FLED IS LEAD ABSORBER PLATE                                             555
5380  CONTINUE                                                            555
         AG_TITLE='FLED'                                                  555
         AG_MODULE=MODULE                                                 555
         AG_MARK='V00'                                                    555
         CALL AGSPUSH                                                     555
         AG_MODULE=MODULE                                                 555
C    *                                                                    555
*
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             557
         CALL AGSRESET                                                    557
         AG_TITLE = 'LEAD'                                                557
         AG_EXNAME = 'MATERIAL'                                           557
         AG_PARLIST = 'NONE'                                              557
         CALL AXMATERIAL                                                  557
C    *                                                                    557
C    Geant MATERIAL : FPDM_LEAD ===> ISVOL=0 <===                         557
         CALL AGSRESET                                                    558
         AG_ISVOL = 0                                                     558
         AG_TITLE = 'FPDM_LEAD'                                           558
         AG_EXNAME = 'MATERIAL'                                           558
         AG_PARLIST = 'ISVOL'                                             558
         CALL AXMATERIAL                                                  558
C    *                                                                    558
C    Geant ATTRIBUTE : FLED ===> SEEN=1 COLO=3 FILL=1 ! GREEN <===        558
         CALL AGSRESET                                                    559
         AG_SEEN = 1                                                      559
         AG_COLO = 3                                                      559
C    GREEN                                                                559
         AG_FILL = 1                                                      559
         AG_TITLE = 'FLED'                                                559
         AG_EXNAME = 'ATTRIBUTE'                                          559
         AG_PARLIST = 'SEEN_COLO_FILL'                                    559
         CALL AXATTRIBUTE                                                 559
C    *                                                                    559
C    Geant SHAPE : TRD1 ===> DY=FMCS_PLATE/2 DZ=(RTOP-RBOT)/2 <===        559
         CALL AGSRESET                                                    560
         AG_DY = FMCS_PLATE/2                                             560
         AG_DZ = (RTOP-RBOT)/2                                            560
         AG_TITLE = 'TRD1'                                                560
         AG_EXNAME = 'SHAPE'                                              560
         AG_PARLIST = 'DY_DZ'                                             560
         CALL AXSHAPE                                                     560
C    *                                                                    560
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00008)                           560
         CALL GSTPAR (AG_IMED,'CUTELE',0.001)                             562
         CALL GSTPAR (AG_IMED,'BCUTE' ,0.0001)                            563
         CALL GSTPAR (AG_IMED,'CUTNEU',0.001)                             564
         CALL GSTPAR (AG_IMED,'CUTHAD',0.001)                             565
         CALL GSTPAR (AG_IMED,'CUTMUO',0.001)                             566
5381  CALL AGSPOPB                                                        568
      GO TO BLOCK_FLED, (5380 ,5390)                                      568
C End of Local Procedure                                                  568
* ----------------------------------------------------------------------------
C Begin Local Procedure FFLP                                              570
C FFLP IS FIRST ALUMINIUM PLATE                                           570
5140  CONTINUE                                                            570
         AG_TITLE='FFLP'                                                  570
         AG_MODULE=MODULE                                                 570
         AG_MARK='V00'                                                    570
         CALL AGSPUSH                                                     570
         AG_MODULE=MODULE                                                 570
C    *                                                                    570
*
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        572
         CALL AGSRESET                                                    572
         AG_TITLE = 'ALUMINIUM'                                           572
         AG_EXNAME = 'MATERIAL'                                           572
         AG_PARLIST = 'NONE'                                              572
         CALL AXMATERIAL                                                  572
C    *                                                                    572
C    Geant MATERIAL : FPDM_ALUMINIUM ===> ISVOL=0 <===                    572
         CALL AGSRESET                                                    573
         AG_ISVOL = 0                                                     573
         AG_TITLE = 'FPDM_ALUMINIUM'                                      573
         AG_EXNAME = 'MATERIAL'                                           573
         AG_PARLIST = 'ISVOL'                                             573
         CALL AXMATERIAL                                                  573
C    *                                                                    573
C    Geant ATTRIBUTE : FFLP ===> SEEN=1 COLO=3 FILL=1 ! GREEN <===        573
         CALL AGSRESET                                                    574
         AG_SEEN = 1                                                      574
         AG_COLO = 3                                                      574
C    GREEN                                                                574
         AG_FILL = 1                                                      574
         AG_TITLE = 'FFLP'                                                574
         AG_EXNAME = 'ATTRIBUTE'                                          574
         AG_PARLIST = 'SEEN_COLO_FILL'                                    574
         CALL AXATTRIBUTE                                                 574
C    *                                                                    574
C    Geant SHAPE : TUBS ===> DZ=SECWID/2, RMIN=(SECTION-DIFF)*TAN_LOW, R  574
C    MAX=(SECTION-DIFF)*TAN_UPP+DUP, PHI1=FMCS_PHIMIN/FMCS_NSUPSEC, PHI2  574
C    =FMCS_PHIMAX/FMCS_NSUPSEC <===                                       574
         CALL AGSRESET                                                    579
         AG_DZ = SECWID/2                                                 579
         AG_RMIN = (SECTION-DIFF)*TAN_LOW                                 579
         AG_RMAX = (SECTION-DIFF)*TAN_UPP+DUP                             579
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               579
         AG_PHI2 = FMCS_PHIMAX/FMCS_NSUPSEC                               579
         AG_TITLE = 'TUBS'                                                579
         AG_EXNAME = 'SHAPE'                                              579
         AG_PARLIST = 'DZ_RMIN_RMAX_PHI1_PHI2'                            579
         CALL AXSHAPE                                                     579
C    *                                                                    579
*                rmn1=(section-diff)*Tan_Low-dd rmn2=(section+secwid-diff)*Tan_Low-dd,
*                rmx1=(section-diff)*Tan_Upp-dd rmx2=(section+secwid-diff)*Tan_Upp-dd
5141  CALL AGSPOPB                                                        582
      GO TO BLOCK_FFLP, (5140 ,5150)                                      582
C End of Local Procedure                                                  582
* ----------------------------------------------------------------------------
C Begin Local Procedure FXFP                                              584
C FXFP IS SS LAMINATED PLATE                                              584
5360  CONTINUE                                                            584
         AG_TITLE='FXFP'                                                  584
         AG_MODULE=MODULE                                                 584
         AG_MARK='V00'                                                    584
         CALL AGSPUSH                                                     584
         AG_MODULE=MODULE                                                 584
C    *                                                                    584
*
C    Geant MATERIAL : FPDM_IRON ===> inherited arguments used <===        586
         CALL AGSRESET                                                    586
         AG_TITLE = 'FPDM_IRON'                                           586
         AG_EXNAME = 'MATERIAL'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXMATERIAL                                                  586
C    *                                                                    586
C    Geant ATTRIBUTE : FXFP ===> SEEN=1 COLO=6 FILL=1 ! VIOLET <===       586
         CALL AGSRESET                                                    587
         AG_SEEN = 1                                                      587
         AG_COLO = 6                                                      587
C    VIOLET                                                               587
         AG_FILL = 1                                                      587
         AG_TITLE = 'FXFP'                                                587
         AG_EXNAME = 'ATTRIBUTE'                                          587
         AG_PARLIST = 'SEEN_COLO_FILL'                                    587
         CALL AXATTRIBUTE                                                 587
C    *                                                                    587
C    Geant SHAPE : TRD1 ===> DY=FMCS_PLATES/2 DZ=(RTOP-RBOT)/2 <===       587
         CALL AGSRESET                                                    588
         AG_DY = FMCS_PLATES/2                                            588
         AG_DZ = (RTOP-RBOT)/2                                            588
         AG_TITLE = 'TRD1'                                                588
         AG_EXNAME = 'SHAPE'                                              588
         AG_PARLIST = 'DY_DZ'                                             588
         CALL AXSHAPE                                                     588
C    *                                                                    588
5361  CALL AGSPOPB                                                        588
      GO TO BLOCK_FXFP, (5360 ,5370,5400)                                 589
C End of Local Procedure                                                  589
* ----------------------------------------------------------------------------
C Begin Local Procedure FSHM                                              591
C FSHM IS THE SHOWER MAX SECTION                                          591
5190  CONTINUE                                                            591
         AG_TITLE='FSHM'                                                  591
         AG_MODULE=MODULE                                                 591
         AG_MARK='V00'                                                    591
         CALL AGSPUSH                                                     591
         AG_MODULE=MODULE                                                 591
C    *                                                                    591
*
C    Geant MATERIAL : FPDM_AIR ===> inherited arguments used <===         593
         CALL AGSRESET                                                    593
         AG_TITLE = 'FPDM_AIR'                                            593
         AG_EXNAME = 'MATERIAL'                                           593
         AG_PARLIST = 'NONE'                                              593
         CALL AXMATERIAL                                                  593
C    *                                                                    593
c$$$      Material  Stra_Air isvol=0
C    Geant ATTRIBUTE : FSHM ===> SEEN=1 COLO=4 ! BLUE <===                595
         CALL AGSRESET                                                    595
         AG_SEEN = 1                                                      595
C    BLUE                                                                 595
         AG_COLO = 4                                                      595
         AG_TITLE = 'FSHM'                                                595
         AG_EXNAME = 'ATTRIBUTE'                                          595
         AG_PARLIST = 'SEEN_COLO'                                         595
         CALL AXATTRIBUTE                                                 595
C    *                                                                    595
C    Geant SHAPE : CONS ===> DZ=SECWID/2, PHI1=FMCS_PHIMIN/FMCS_NSUPSEC,  595
C     PHI2=+FMCS_PHIMAX/FMCS_NSUPSEC, RMN1=(SECTION-DIFF)*TAN_LOW-DD RMN  595
C    2=(SECTION+SECWID-DIFF)*TAN_LOW-DD, RMX1=(SECTION-DIFF)*TAN_UPP+DUP  595
C     RMX2=(SECTION+SECWID-DIFF)*TAN_UPP+DUP <===                         595
         CALL AGSRESET                                                    600
         AG_DZ = SECWID/2                                                 600
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               600
         AG_PHI2 = +FMCS_PHIMAX/FMCS_NSUPSEC                              600
         AG_RMN1 = (SECTION-DIFF)*TAN_LOW-DD                              600
         AG_RMN2 = (SECTION+SECWID-DIFF)*TAN_LOW-DD                       600
         AG_RMX1 = (SECTION-DIFF)*TAN_UPP+DUP                             600
         AG_RMX2 = (SECTION+SECWID-DIFF)*TAN_UPP+DUP                      600
         AG_TITLE = 'CONS'                                                600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'DZ_PHI1_PHI2_RMN1_RMN2_RMX1_RMX2'                  600
         CALL AXSHAPE                                                     600
C    *                                                                    600
*      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
*      Call GSTPAR (ag_imed,'CUTELE',0.00001)
*      Call GSTPAR (ag_imed,'LOSS',1.)
*      Call GSTPAR (ag_imed,'STRA',1.)
*
C    use FMXG bank                                                        606
         BPATH_FMXG='FMXG*'                                               606
         IDNUM_FMXG(1)=0                                                  606
         FMXG_VERSION=1                                                   606
         CALL AGDGETP('FPDMGEO','*',-999,'FMXG',NFMXG_VERSION,            606
     *   RE_FMXG_VERSION,BPATH_FMXG,IDNUM_FMXG,1,MAP_FMXG,NAMES_FMXG,     606
     *   BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                606
         CURR = SECTION                                                   607
         MAXCNT = CURR+FMCS_GAPSMD/2                                      608
*
C    *                                                                    610
         J_SECTION=1                                                      610
            GO TO 5413                                                    610
5411        J_SECTION=J_SECTION+1                                         610
5413        IF(J_SECTION-(2).GT.0) GO TO 5412                             610
*
C       use FXSE bank                                                     612
            BPATH_FXSE='FXSE*'                                            612
            IDNUM_FXSE(1)=0                                               612
            FXSE_JSECT=J_SECTION                                          612
            CALL AGDGETP('FPDMGEO','*',-999,'FXSE',NFXSE_JSECT,           612
     *      RE_FXSE_JSECT,BPATH_FXSE,IDNUM_FXSE,1,MAP_FXSE,NAMES_FXSE,    612
     *      BANK_FXSE,LEN_FXSE,LLEN_FXSE,LINK_FXSE,FLAG_FXSE)             612
*
            MSECWD=FXSE_SWIDTH - FXSE_APLATE - FMXG_SAPEX                 614
C       Create FXGT :                                                     615
C       Call To Local Procedure FXGT                                      615
            ASSIGN 5430 TO BLOCK_FXGT                                     615
            GO TO  5420                                                   615
5430        CONTINUE                                                      615
C       *                                                                 615
C       Geant POSITION : FXGT ===> Z=CURR-MAXCNT+MSECWD/2 <===            615
            CALL AGSRESET                                                 615
            AG_Z = CURR-MAXCNT+MSECWD/2                                   615
            AG_TITLE = 'FXGT'                                             615
            AG_EXNAME = 'POSITION'                                        615
            AG_PARLIST = 'Z'                                              615
            CALL AXPOSITION                                               615
C       *                                                                 615
            CURR = CURR + MSECWD                                          615
*
            MSECWD = FMXG_SAPEX                                           618
C       Check J_SECTION =1                                                619
            IF (J_SECTION .EQ.1) THEN                                     619
            RTOP = CURR+MSECWD/2                                          620
*          prin0 rtop
*          (' Z for u:  ',F12.4)
C       Create FXSG :                                                     623
C       Call To Local Procedure FXSG                                      623
            ASSIGN 5450 TO BLOCK_FXSG                                     623
            GO TO  5440                                                   623
5450        CONTINUE                                                      623
C       *                                                                 623
C       Geant POSITION : FXSG ===> Z=CURR-MAXCNT+MSECWD/2 <===            623
            CALL AGSRESET                                                 623
            AG_Z = CURR-MAXCNT+MSECWD/2                                   623
            AG_TITLE = 'FXSG'                                             623
            AG_EXNAME = 'POSITION'                                        623
            AG_PARLIST = 'Z'                                              623
            CALL AXPOSITION                                               623
C       *                                                                 623
            ELSE                                                          623
            RTOP = CURR+MSECWD/2                                          625
*          prin0 rtop
*          (' Z for v:  ',F12.4)
C       Create FXSG :                                                     628
C       Call To Local Procedure FXSG                                      628
            ASSIGN 5460 TO BLOCK_FXSG                                     628
            GO TO  5440                                                   628
5460        CONTINUE                                                      628
C       *                                                                 628
C       Geant POSITION : FXSG ===> Z=CURR-MAXCNT+MSECWD/2 ALPHAX=180 <==  628
C       =                                                                 628
            CALL AGSRESET                                                 628
            AG_Z = CURR-MAXCNT+MSECWD/2                                   628
            AG_ALPHAN=AG_ALPHAN+1                                         628
            AG_ALPHAK(AG_ALPHAN)=1                                        628
            AG_ALPHAL(AG_ALPHAN)=180                                      628
            AG_TITLE = 'FXSG'                                             628
            AG_EXNAME = 'POSITION'                                        628
            AG_PARLIST = 'Z_ALPHAX'                                       628
            CALL AXPOSITION                                               628
C       *                                                                 628
            ENDIF                                                         630
            CURR = CURR + MSECWD                                          631
*
            MSECWD = FXSE_APLATE                                          633
C       Create FALP :                                                     634
C       Call To Local Procedure FALP                                      634
            ASSIGN 5480 TO BLOCK_FALP                                     634
            GO TO  5470                                                   634
5480        CONTINUE                                                      634
C       *                                                                 634
C       Geant POSITION : FALP ===> Z=CURR-MAXCNT+MSECWD/2 <===            634
            CALL AGSRESET                                                 634
            AG_Z = CURR-MAXCNT+MSECWD/2                                   634
            AG_TITLE = 'FALP'                                             634
            AG_EXNAME = 'POSITION'                                        634
            AG_PARLIST = 'Z'                                              634
            CALL AXPOSITION                                               634
C       *                                                                 634
            CURR = CURR + MSECWD                                          634
         GO TO 5411                                                       636
5412     CONTINUE                                                         636
5191  CALL AGSPOPB                                                        638
      GO TO BLOCK_FSHM, (5190 ,5200)                                      638
C End of Local Procedure                                                  638
* ----------------------------------------------------------------------------
C Begin Local Procedure FXGT                                              640
C FXGT IS THE G10 LAYER IN THE SMAX                                       640
5420  CONTINUE                                                            640
         AG_TITLE='FXGT'                                                  640
         AG_MODULE=MODULE                                                 640
         AG_MARK='V00'                                                    640
         CALL AGSPUSH                                                     640
         AG_MODULE=MODULE                                                 640
C    *                                                                    640
*
*     G10 is about 60% SiO2 and 40% epoxy
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          643
         CALL AGSRESET                                                    643
         AG_A = 28.08                                                     643
         AG_Z = 14                                                        643
         AG_W = 0.6*1*28./60.                                             643
         AG_TITLE = 'SI'                                                  643
         AG_EXNAME = 'COMPONENT'                                          643
         AG_PARLIST = 'A_Z_W'                                             643
         CALL AXCOMPONENT                                                 643
C    *                                                                    643
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               643
         CALL AGSRESET                                                    644
         AG_A = 16                                                        644
         AG_Z = 8                                                         644
         AG_W = 0.6*2*16./60.                                             644
         AG_TITLE = 'O'                                                   644
         AG_EXNAME = 'COMPONENT'                                          644
         AG_PARLIST = 'A_Z_W'                                             644
         CALL AXCOMPONENT                                                 644
C    *                                                                    644
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              644
         CALL AGSRESET                                                    645
         AG_A = 12                                                        645
         AG_Z = 6                                                         645
         AG_W = 0.4*8*12./174.                                            645
         AG_TITLE = 'C'                                                   645
         AG_EXNAME = 'COMPONENT'                                          645
         AG_PARLIST = 'A_Z_W'                                             645
         CALL AXCOMPONENT                                                 645
C    *                                                                    645
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               645
         CALL AGSRESET                                                    646
         AG_A = 1                                                         646
         AG_Z = 1                                                         646
         AG_W = 0.4*14*1./174.                                            646
         AG_TITLE = 'H'                                                   646
         AG_EXNAME = 'COMPONENT'                                          646
         AG_PARLIST = 'A_Z_W'                                             646
         CALL AXCOMPONENT                                                 646
C    *                                                                    646
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              646
         CALL AGSRESET                                                    647
         AG_A = 16                                                        647
         AG_Z = 8                                                         647
         AG_W = 0.4*4*16./174.                                            647
         AG_TITLE = 'O'                                                   647
         AG_EXNAME = 'COMPONENT'                                          647
         AG_PARLIST = 'A_Z_W'                                             647
         CALL AXCOMPONENT                                                 647
C    *                                                                    647
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               647
         CALL AGSRESET                                                    648
         AG_DENS = 1.7                                                    648
         AG_TITLE = 'G10'                                                 648
         AG_EXNAME = 'MIXTURE'                                            648
         AG_PARLIST = 'DENS'                                              648
         CALL AXMIXTURE                                                   648
C    *                                                                    648
C    Geant ATTRIBUTE : FXGT ===> SEEN=1 COLO=7 <===                       650
         CALL AGSRESET                                                    650
         AG_SEEN = 1                                                      650
         AG_COLO = 7                                                      650
         AG_TITLE = 'FXGT'                                                650
         AG_EXNAME = 'ATTRIBUTE'                                          650
         AG_PARLIST = 'SEEN_COLO'                                         650
         CALL AXATTRIBUTE                                                 650
C    *                                                                    650
C    Geant SHAPE : CONS ===> DZ=MSECWD/2, PHI1=FMCS_PHIMIN/FMCS_NSUPSEC,  650
C     PHI2=FMCS_PHIMAX/FMCS_NSUPSEC, RMN1=(CURR-DIFF)*TAN_LOW-DD RMN2=(C  650
C    URR+MSECWD-DIFF)*TAN_LOW-DD, RMX1=(CURR-DIFF)*TAN_UPP-DD RMX2=(CURR  650
C    +MSECWD-DIFF)*TAN_UPP-DD <===                                        650
         CALL AGSRESET                                                    655
         AG_DZ = MSECWD/2                                                 655
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               655
         AG_PHI2 = FMCS_PHIMAX/FMCS_NSUPSEC                               655
         AG_RMN1 = (CURR-DIFF)*TAN_LOW-DD                                 655
         AG_RMN2 = (CURR+MSECWD-DIFF)*TAN_LOW-DD                          655
         AG_RMX1 = (CURR-DIFF)*TAN_UPP-DD                                 655
         AG_RMX2 = (CURR+MSECWD-DIFF)*TAN_UPP-DD                          655
         AG_TITLE = 'CONS'                                                655
         AG_EXNAME = 'SHAPE'                                              655
         AG_PARLIST = 'DZ_PHI1_PHI2_RMN1_RMN2_RMX1_RMX2'                  655
         CALL AXSHAPE                                                     655
C    *                                                                    655
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00001)                           655
         CALL GSTPAR (AG_IMED,'CUTELE',0.00001)                           657
5421  CALL AGSPOPB                                                        658
      GO TO BLOCK_FXGT, (5420 ,5430)                                      658
C End of Local Procedure                                                  658
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FXSG                                              660
C FXSG IS THE SHOWER MAX GAP FOR SCINTILLATOR STRIPS                      660
5440  CONTINUE                                                            660
         AG_TITLE='FXSG'                                                  660
         AG_MODULE=MODULE                                                 660
         AG_MARK='V00'                                                    660
         CALL AGSPUSH                                                     660
         AG_MODULE=MODULE                                                 660
C    *                                                                    660
*
C    Geant ATTRIBUTE : FXSG ===> SEEN=1 COLO=7 ! BLACK <===               662
         CALL AGSRESET                                                    662
         AG_SEEN = 1                                                      662
C    BLACK                                                                662
         AG_COLO = 7                                                      662
         AG_TITLE = 'FXSG'                                                662
         AG_EXNAME = 'ATTRIBUTE'                                          662
         AG_PARLIST = 'SEEN_COLO'                                         662
         CALL AXATTRIBUTE                                                 662
C    *                                                                    662
C    Geant MATERIAL : FPDM_AIR ===> inherited arguments used <===         662
         CALL AGSRESET                                                    663
         AG_TITLE = 'FPDM_AIR'                                            663
         AG_EXNAME = 'MATERIAL'                                           663
         AG_PARLIST = 'NONE'                                              663
         CALL AXMATERIAL                                                  663
C    *                                                                    663
C    Geant SHAPE : CONS ===> DZ=MSECWD/2, PHI1=FMCS_PHIMIN/FMCS_NSUPSEC,  663
C     PHI2=FMCS_PHIMAX/FMCS_NSUPSEC, RMN1=(CURR-DIFF)*TAN_LOW-DD RMN2=(C  663
C    URR+MSECWD-DIFF)*TAN_LOW-DD, RMX1=(CURR-DIFF)*TAN_UPP+DUP RMX2=(CUR  663
C    R+MSECWD-DIFF)*TAN_UPP+DUP <===                                      663
         CALL AGSRESET                                                    668
         AG_DZ = MSECWD/2                                                 668
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               668
         AG_PHI2 = FMCS_PHIMAX/FMCS_NSUPSEC                               668
         AG_RMN1 = (CURR-DIFF)*TAN_LOW-DD                                 668
         AG_RMN2 = (CURR+MSECWD-DIFF)*TAN_LOW-DD                          668
         AG_RMX1 = (CURR-DIFF)*TAN_UPP+DUP                                668
         AG_RMX2 = (CURR+MSECWD-DIFF)*TAN_UPP+DUP                         668
         AG_TITLE = 'CONS'                                                668
         AG_EXNAME = 'SHAPE'                                              668
         AG_PARLIST = 'DZ_PHI1_PHI2_RMN1_RMN2_RMX1_RMX2'                  668
         CALL AXSHAPE                                                     668
C    *                                                                    668
*
         SQ3 = SQRT(3.)                                                   670
         SQ2 = SQRT(2.)                                                   671
         RBOT = (CURR-DIFF+MSECWD/2.)*TAN_LOW-DD                          672
         RTOP = (CURR-DIFF+MSECWD/2.)*TAN_UPP+DUP                         673
*
C    Check FMCS_TYPE==1                                                   675
         IF (FMCS_TYPE.EQ.1) THEN                                         675
         RTH = PI/FMCS_NSUPSEC                                            676
         TNG = TAN(RTH)                                                   677
         NSTR = NINT((RTOP*SQ3 - RBOT)/FMXG_SBASE)                        678
         IF (NSTR .GT. 300) NSTR = 300                                    679
C    *                                                                    680
         I_STR=1                                                          680
            GO TO 5493                                                    680
5491        I_STR=I_STR+1                                                 680
5493        IF(I_STR-(NSTR).GT.0) GO TO 5492                              680
            P = .5*(I_STR*FMXG_SBASE + RBOT)                              681
*
C       Check P <= .5*RBOT*SQ3                                            683
            IF (P .LE. .5*RBOT*SQ3) THEN                                  683
            YLEFT = .5*(SQ2*P - SQRT(2.*RBOT*RBOT - 2.*P*P))              684
            XLEFT = SQ2*P - YLEFT                                         685
            XRIGHT = SQ2*P/(1. - TNG )                                    686
            YRIGHT = -TNG*XRIGHT                                          687
            ELSEIF (.5*RBOT .LT.P.AND.P.LE. .5*RTOP) THEN                 688
            XLEFT = SQ2*P/(1. + TNG )                                     689
            YLEFT = TNG*XLEFT                                             690
            XRIGHT = SQ2*P/(1. - TNG )                                    691
            YRIGHT = -TNG*XRIGHT                                          692
            ELSEIF (.5*RTOP .LT.P.AND.P.LE. .5*RTOP*SQ3) THEN             693
            XLEFT = SQ2*P/(1. + TNG )                                     694
            YLEFT = TNG*XLEFT                                             695
            YRIGHT = .5*(SQ2*P - SQRT(2.*RTOP*RTOP - 2.*P*P))             696
            XRIGHT = SQ2*P - YRIGHT                                       697
            ENDIF                                                         698
            LEN = SQRT((XLEFT-XRIGHT)**2 + (YLEFT-YRIGHT)**2 )            700
            XC = .5*(XRIGHT+XLEFT)                                        701
            YC = .5*(YRIGHT+YLEFT)                                        702
*          prin0 i_str,xleft,yleft,xright,yright,len
*             (' FXSG: i,xleft,yleft,xright,yright,len',i3,5F12.4)
C       Check MOD(I_STR,2) != 0                                           706
            IF (MOD(I_STR,2) .NE. 0) THEN                                 706
C       Create FHMS :                                                     707
C       Call To Local Procedure FHMS                                      707
            ASSIGN 5510 TO BLOCK_FHMS                                     707
            GO TO  5500                                                   707
5510        CONTINUE                                                      707
C       *                                                                 707
C       Geant POSITION : FHMS ===> X=XC Y=YC ALPHAZ=45 <===               707
            CALL AGSRESET                                                 707
            AG_X = XC                                                     707
            AG_Y = YC                                                     707
            AG_ALPHAN=AG_ALPHAN+1                                         707
            AG_ALPHAK(AG_ALPHAN)=3                                        707
            AG_ALPHAL(AG_ALPHAN)=45                                       707
            AG_TITLE = 'FHMS'                                             707
            AG_EXNAME = 'POSITION'                                        707
            AG_PARLIST = 'X_Y_ALPHAZ'                                     707
            CALL AXPOSITION                                               707
C       *                                                                 707
            ELSE                                                          707
C       Create FHMS :                                                     709
C       Call To Local Procedure FHMS                                      709
            ASSIGN 5520 TO BLOCK_FHMS                                     709
            GO TO  5500                                                   709
5520        CONTINUE                                                      709
C       *                                                                 709
C       Geant POSITION : FHMS ===> X=XC Y=YC ALPHAZ=45 ORT=X-Y-Z <===     709
            CALL AGSRESET                                                 709
            AG_X = XC                                                     709
            AG_Y = YC                                                     709
            AG_ALPHAN=AG_ALPHAN+1                                         709
            AG_ALPHAK(AG_ALPHAN)=3                                        709
            AG_ALPHAL(AG_ALPHAN)=45                                       709
            AG_ALPHAN=AG_ALPHAN+1                                         709
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            709
            AG_TITLE = 'FHMS'                                             709
            AG_EXNAME = 'POSITION'                                        709
            AG_PARLIST = 'X_Y_ALPHAZ_ORT'                                 709
            CALL AXPOSITION                                               709
C       *                                                                 709
            ENDIF                                                         709
         GO TO 5491                                                       711
5492     CONTINUE                                                         711
         ELSE                                                             712
C    Check J_SECTION=1                                                    713
         IF (J_SECTION.EQ.1) THEN                                         713
*        Nstr = 60
         NSTR = 100                                                       715
         LEN = 36                                                         716
C    *                                                                    717
         I_STR=1                                                          717
            GO TO 5533                                                    717
5531        I_STR=I_STR+1                                                 717
5533        IF(I_STR-(NSTR).GT.0) GO TO 5532                              717
            XC = (RBOT+RTOP)/2 + (I_STR-NSTR/2.0)*FMXG_SBASE/2.0          718
            YC = 0.0                                                      719
C       Check MOD(I_STR,2) != 0                                           720
            IF (MOD(I_STR,2) .NE. 0) THEN                                 720
C       Create FHMS :                                                     721
C       Call To Local Procedure FHMS                                      721
            ASSIGN 5540 TO BLOCK_FHMS                                     721
            GO TO  5500                                                   721
5540        CONTINUE                                                      721
C       *                                                                 721
C       Geant POSITION : FHMS ===> X=XC Y=YC <===                         721
            CALL AGSRESET                                                 721
            AG_X = XC                                                     721
            AG_Y = YC                                                     721
            AG_TITLE = 'FHMS'                                             721
            AG_EXNAME = 'POSITION'                                        721
            AG_PARLIST = 'X_Y'                                            721
            CALL AXPOSITION                                               721
C       *                                                                 721
            ELSE                                                          721
C       Create FHMS :                                                     723
C       Call To Local Procedure FHMS                                      723
            ASSIGN 5550 TO BLOCK_FHMS                                     723
            GO TO  5500                                                   723
5550        CONTINUE                                                      723
C       *                                                                 723
C       Geant POSITION : FHMS ===> X=XC Y=YC ALPHAX=180 <===              723
            CALL AGSRESET                                                 723
            AG_X = XC                                                     723
            AG_Y = YC                                                     723
            AG_ALPHAN=AG_ALPHAN+1                                         723
            AG_ALPHAK(AG_ALPHAN)=1                                        723
            AG_ALPHAL(AG_ALPHAN)=180                                      723
            AG_TITLE = 'FHMS'                                             723
            AG_EXNAME = 'POSITION'                                        723
            AG_PARLIST = 'X_Y_ALPHAX'                                     723
            CALL AXPOSITION                                               723
C       *                                                                 723
            ENDIF                                                         723
         GO TO 5531                                                       725
5532     CONTINUE                                                         725
         ELSE                                                             726
*        Nstr = 100
         NSTR = 60                                                        728
         LEN = 36                                                         729
C    *                                                                    730
         I_STR=1                                                          730
            GO TO 5563                                                    730
5561        I_STR=I_STR+1                                                 730
5563        IF(I_STR-(NSTR).GT.0) GO TO 5562                              730
            XC = (RBOT+RTOP)/2.0                                          731
            YC = (I_STR-NSTR/2.0)*FMXG_SBASE/2.0                          732
C       Check MOD(I_STR,2) != 0                                           733
            IF (MOD(I_STR,2) .NE. 0) THEN                                 733
C       Create FHMS :                                                     734
C       Call To Local Procedure FHMS                                      734
            ASSIGN 5570 TO BLOCK_FHMS                                     734
            GO TO  5500                                                   734
5570        CONTINUE                                                      734
C       *                                                                 734
C       Geant POSITION : FHMS ===> X=XC Y=YC ALPHAZ=90 <===               734
            CALL AGSRESET                                                 734
            AG_X = XC                                                     734
            AG_Y = YC                                                     734
            AG_ALPHAN=AG_ALPHAN+1                                         734
            AG_ALPHAK(AG_ALPHAN)=3                                        734
            AG_ALPHAL(AG_ALPHAN)=90                                       734
            AG_TITLE = 'FHMS'                                             734
            AG_EXNAME = 'POSITION'                                        734
            AG_PARLIST = 'X_Y_ALPHAZ'                                     734
            CALL AXPOSITION                                               734
C       *                                                                 734
            ELSE                                                          734
C       Create FHMS :                                                     736
C       Call To Local Procedure FHMS                                      736
            ASSIGN 5580 TO BLOCK_FHMS                                     736
            GO TO  5500                                                   736
5580        CONTINUE                                                      736
C       *                                                                 736
C       Geant POSITION : FHMS ===> X=XC Y=YC ORT=YX-Z <===                736
            CALL AGSRESET                                                 736
            AG_X = XC                                                     736
            AG_Y = YC                                                     736
            AG_ALPHAN=AG_ALPHAN+1                                         736
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             736
            AG_TITLE = 'FHMS'                                             736
            AG_EXNAME = 'POSITION'                                        736
            AG_PARLIST = 'X_Y_ORT'                                        736
            CALL AXPOSITION                                               736
C       *                                                                 736
            ENDIF                                                         736
         GO TO 5561                                                       738
5562     CONTINUE                                                         738
         ENDIF                                                            739
         ENDIF                                                            740
*     dcut exsg z 0 0 10 0.1 0.1
*     dcut exsg y 0 10 -50 0.7 0.7
5441  CALL AGSPOPB                                                        745
      GO TO BLOCK_FXSG, (5440 ,5450,5460)                                 745
C End of Local Procedure                                                  745
* ----------------------------------------------------------------------------
C Begin Local Procedure FHMS                                              747
C FHMS IS SHOWER MAX STRIP                                                747
5500  CONTINUE                                                            747
         AG_TITLE='FHMS'                                                  747
         AG_MODULE=MODULE                                                 747
         AG_MARK='V00'                                                    747
         CALL AGSPUSH                                                     747
         AG_MODULE=MODULE                                                 747
C    *                                                                    747
*
C    Geant MATERIAL : FPDM_POLYSTYREN ===> inherited arguments used <===  749
         CALL AGSRESET                                                    749
         AG_TITLE = 'FPDM_POLYSTYREN'                                     749
         AG_EXNAME = 'MATERIAL'                                           749
         AG_PARLIST = 'NONE'                                              749
         CALL AXMATERIAL                                                  749
C    *                                                                    749
c      Material  Cpolystyren   Isvol=1
C    Geant ATTRIBUTE : FHMS ===> SEEN=1 COLO=2 ! RED <===                 751
         CALL AGSRESET                                                    751
         AG_SEEN = 1                                                      751
C    RED                                                                  751
         AG_COLO = 2                                                      751
         AG_TITLE = 'FHMS'                                                751
         AG_EXNAME = 'ATTRIBUTE'                                          751
         AG_PARLIST = 'SEEN_COLO'                                         751
         CALL AXATTRIBUTE                                                 751
C    *                                                                    751
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=FMXG_SBASE/2 DY=LEN/2 DZ=FMXG_SAP  751
C    EX/2 <===                                                            751
         CALL AGSRESET                                                    752
         AG_DX1 = 0                                                       752
         AG_DX2 = FMXG_SBASE/2                                            752
         AG_DY = LEN/2                                                    752
         AG_DZ = FMXG_SAPEX/2                                             752
         AG_TITLE = 'TRD1'                                                752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     752
         CALL AXSHAPE                                                     752
C    *                                                                    752
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00008)                           752
         CALL GSTPAR (AG_IMED,'CUTELE',0.001)                             754
         CALL GSTPAR (AG_IMED,'BCUTE', 0.0001)                            755
* define Birks law parameters
         CALL GSTPAR (AG_IMED,'BIRK1',1.)                                 757
         CALL GSTPAR (AG_IMED,'BIRK2',0.0130)                             758
         CALL GSTPAR (AG_IMED,'BIRK3',9.6E-6)                             759
*
C    Defining Hits: BIRK:0:(0,10)                                         761
         AG_TITLE = 'FHMS'                                                761
         AG_EXNAME = 'HITS'                                               761
         AG_TYPE=AG_MAGIC                                                 761
         CALL AGSDETV('FHMS')                                             761
C    *                                                                    761
         AG_PARLIST='BIRK'                                                761
         AG_TITLE = 'DETH'                                                761
         AG_NBITS = 0                                                     761
         AG_BIN=AG_MAGIC                                                  761
         AG_OPTION='*'                                                    761
         AG_FHMIN = 0                                                     761
         AG_FHMAX = 10                                                    761
         CALL AGSDETH (0)                                                 761
C    *                                                                    761
         CALL AGSDETU('FHMS')                                             761
C    *                                                                    761
5501  CALL AGSPOPB                                                        762
      GO TO BLOCK_FHMS, (5500 ,5510,5520,5540,5550,5570,5580)             762
C End of Local Procedure                                                  762
* ----------------------------------------------------------------------------
C Begin Local Procedure FALP                                              764
C FALP IS ALUMINIUM PLATE IN SHOWER MAX                                   764
5470  CONTINUE                                                            764
         AG_TITLE='FALP'                                                  764
         AG_MODULE=MODULE                                                 764
         AG_MARK='V00'                                                    764
         CALL AGSPUSH                                                     764
         AG_MODULE=MODULE                                                 764
C    *                                                                    764
*
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        766
         CALL AGSRESET                                                    766
         AG_TITLE = 'ALUMINIUM'                                           766
         AG_EXNAME = 'MATERIAL'                                           766
         AG_PARLIST = 'NONE'                                              766
         CALL AXMATERIAL                                                  766
C    *                                                                    766
C    Geant MATERIAL : FPDM_ALUMINIUM_FALP ===> ISVOL=0 <===               766
         CALL AGSRESET                                                    767
         AG_ISVOL = 0                                                     767
         AG_TITLE = 'FPDM_ALUMINIUM_FALP'                                 767
         AG_EXNAME = 'MATERIAL'                                           767
         AG_PARLIST = 'ISVOL'                                             767
         CALL AXMATERIAL                                                  767
C    *                                                                    767
C    Geant ATTRIBUTE : FALP ===> SEEN=1 COLO=1 <===                       767
         CALL AGSRESET                                                    768
         AG_SEEN = 1                                                      768
         AG_COLO = 1                                                      768
         AG_TITLE = 'FALP'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO'                                         768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant SHAPE : CONS ===> DZ=MSECWD/2, PHI1=FMCS_PHIMIN/FMCS_NSUPSEC,  768
C     PHI2=+FMCS_PHIMAX/FMCS_NSUPSEC, RMN1=(CURR-DIFF)*TAN_LOW-DD RMN2=(  768
C    CURR+MSECWD/2-DIFF)*TAN_LOW-DD, RMX1=(CURR-DIFF)*TAN_UPP-DD RMX2=(C  768
C    URR+MSECWD/2-DIFF)*TAN_UPP-DD <===                                   768
         CALL AGSRESET                                                    773
         AG_DZ = MSECWD/2                                                 773
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               773
         AG_PHI2 = +FMCS_PHIMAX/FMCS_NSUPSEC                              773
         AG_RMN1 = (CURR-DIFF)*TAN_LOW-DD                                 773
         AG_RMN2 = (CURR+MSECWD/2-DIFF)*TAN_LOW-DD                        773
         AG_RMX1 = (CURR-DIFF)*TAN_UPP-DD                                 773
         AG_RMX2 = (CURR+MSECWD/2-DIFF)*TAN_UPP-DD                        773
         AG_TITLE = 'CONS'                                                773
         AG_EXNAME = 'SHAPE'                                              773
         AG_PARLIST = 'DZ_PHI1_PHI2_RMN1_RMN2_RMX1_RMX2'                  773
         CALL AXSHAPE                                                     773
C    *                                                                    773
         CALL GSTPAR (AG_IMED,'CUTGAM',0.00001)                           773
         CALL GSTPAR (AG_IMED,'CUTELE',0.00001)                           775
         CALL GSTPAR (AG_IMED,'LOSS',1.)                                  776
         CALL GSTPAR (AG_IMED,'STRA',1.)                                  777
5471  CALL AGSPOPB                                                        778
      GO TO BLOCK_FALP, (5470 ,5480)                                      778
C End of Local Procedure                                                  778
* ----------------------------------------------------------------------------
C Begin Local Procedure FSSP                                              780
C FSSP IS STAINLESS STEEL PLATE                                           780
5210  CONTINUE                                                            780
         AG_TITLE='FSSP'                                                  780
         AG_MODULE=MODULE                                                 780
         AG_MARK='V00'                                                    780
         CALL AGSPUSH                                                     780
         AG_MODULE=MODULE                                                 780
C    *                                                                    780
*
C    Geant MATERIAL : FPDM_IRON ===> inherited arguments used <===        782
         CALL AGSRESET                                                    782
         AG_TITLE = 'FPDM_IRON'                                           782
         AG_EXNAME = 'MATERIAL'                                           782
         AG_PARLIST = 'NONE'                                              782
         CALL AXMATERIAL                                                  782
C    *                                                                    782
C    Geant ATTRIBUTE : FSSP ===> SEEN=1 COLO=6 FILL=1 <===                782
         CALL AGSRESET                                                    783
         AG_SEEN = 1                                                      783
         AG_COLO = 6                                                      783
         AG_FILL = 1                                                      783
         AG_TITLE = 'FSSP'                                                783
         AG_EXNAME = 'ATTRIBUTE'                                          783
         AG_PARLIST = 'SEEN_COLO_FILL'                                    783
         CALL AXATTRIBUTE                                                 783
C    *                                                                    783
C    Geant SHAPE : CONS ===> DZ=FMCS_PLATEB/2, PHI1=FMCS_PHIMIN/FMCS_NSU  783
C    PSEC, PHI2=+FMCS_PHIMAX/FMCS_NSUPSEC, RMN1=(SECTION-DIFF)*TAN_LOW-D  783
C    D RMN2=(SECTION+SECWID-DIFF)*TAN_LOW-DD, RMX1=(SECTION-DIFF)*TAN_UP  783
C    P+DUP RMX2=(SECTION+SECWID-DIFF)*TAN_UPP+DUP <===                    783
         CALL AGSRESET                                                    788
         AG_DZ = FMCS_PLATEB/2                                            788
         AG_PHI1 = FMCS_PHIMIN/FMCS_NSUPSEC                               788
         AG_PHI2 = +FMCS_PHIMAX/FMCS_NSUPSEC                              788
         AG_RMN1 = (SECTION-DIFF)*TAN_LOW-DD                              788
         AG_RMN2 = (SECTION+SECWID-DIFF)*TAN_LOW-DD                       788
         AG_RMX1 = (SECTION-DIFF)*TAN_UPP+DUP                             788
         AG_RMX2 = (SECTION+SECWID-DIFF)*TAN_UPP+DUP                      788
         AG_TITLE = 'CONS'                                                788
         AG_EXNAME = 'SHAPE'                                              788
         AG_PARLIST = 'DZ_PHI1_PHI2_RMN1_RMN2_RMX1_RMX2'                  788
         CALL AXSHAPE                                                     788
C    *                                                                    788
5211  CALL AGSPOPB                                                        788
      GO TO BLOCK_FSSP, (5210 ,5220)                                      789
C End of Local Procedure                                                  789
* ----------------------------------------------------------------------------
* FWOL nice views: dcut ecvo x 1       10 -5  .5 .1
*                  draw emdi 105 0 160  2 13  .2 .1
*                  draw emdi 120 180 150  1 14  .12 .12
* ---------------------------------------------------------------------------
C Begin Local Procedure FLGD                                              795
C FLGD IS ONE PB-GLASS FPD DETECTOR                                       795
5090  CONTINUE                                                            795
         AG_TITLE='FLGD'                                                  795
         AG_MODULE=MODULE                                                 795
         AG_MARK='V00'                                                    795
         CALL AGSPUSH                                                     795
         AG_MODULE=MODULE                                                 795
C    *                                                                    795
C    Geant MATERIAL : FPDM_AIR ===> inherited arguments used <===         796
         CALL AGSRESET                                                    796
         AG_TITLE = 'FPDM_AIR'                                            796
         AG_EXNAME = 'MATERIAL'                                           796
         AG_PARLIST = 'NONE'                                              796
         CALL AXMATERIAL                                                  796
C    *                                                                    796
c$$$      Medium    standard
C    Geant ATTRIBUTE : FLGD ===> SEEN=1 COLO=1 <===                       798
         CALL AGSRESET                                                    798
         AG_SEEN = 1                                                      798
         AG_COLO = 1                                                      798
         AG_TITLE = 'FLGD'                                                798
         AG_EXNAME = 'ATTRIBUTE'                                          798
         AG_PARLIST = 'SEEN_COLO'                                         798
         CALL AXATTRIBUTE                                                 798
C    *                                                                    798
C    Geant SHAPE : BOX ===> DZ=ZTOT, DY=(FLGG_NETA*FLGG_WIDTH + (FLGG_NE  798
C    TA+1.0)*FLGG_DGAP)/2.0, DX=(FLGG_NPHI*FLGG_WIDTH + (FLGG_NPHI+1.0)*  798
C    FLGG_DGAP)/2.0 <===                                                  798
         CALL AGSRESET                                                    801
         AG_DZ = ZTOT                                                     801
         AG_DY = (FLGG_NETA*FLGG_WIDTH + (FLGG_NETA+1.0)*FLGG_DGAP)/2.0   801
         AG_DX = (FLGG_NPHI*FLGG_WIDTH + (FLGG_NPHI+1.0)*FLGG_DGAP)/2.0   801
         AG_TITLE = 'BOX'                                                 801
         AG_EXNAME = 'SHAPE'                                              801
         AG_PARLIST = 'DZ_DY_DX'                                          801
         CALL AXSHAPE                                                     801
C    *                                                                    801
C    *                                                                    803
         II=1                                                             803
            GO TO 5593                                                    803
5591        II=II+1                                                       803
5593        IF(II-(FLGG_NETA).GT.0) GO TO 5592                            803
            YY = (II-FLGG_NETA/2.0-0.5)*(FLGG_WIDTH+FLGG_DGAP)            804
C       *                                                                 805
            JJ=1                                                          805
               GO TO 5603                                                 805
5601           JJ=JJ+1                                                    805
5603           IF(JJ-(FLGG_NPHI).GT.0) GO TO 5602                         805
               XX = (JJ-FLGG_NPHI/2.0-0.5)*(FLGG_WIDTH+FLGG_DGAP)         806
C          Create FLGT :                                                  807
C          Call To Local Procedure FLGT                                   807
               ASSIGN 5620 TO BLOCK_FLGT                                  807
               GO TO  5610                                                807
5620           CONTINUE                                                   807
C          *                                                              807
C          Geant POSITION : FLGT ===> X=XX Y=YY <===                      807
               CALL AGSRESET                                              807
               AG_X = XX                                                  807
               AG_Y = YY                                                  807
               AG_TITLE = 'FLGT'                                          807
               AG_EXNAME = 'POSITION'                                     807
               AG_PARLIST = 'X_Y'                                         807
               CALL AXPOSITION                                            807
C          *                                                              807
            GO TO 5601                                                    808
5602        CONTINUE                                                      808
         GO TO 5591                                                       809
5592     CONTINUE                                                         809
5091  CALL AGSPOPB                                                        810
      GO TO BLOCK_FLGD, (5090 ,5100,5110)                                 810
C End of Local Procedure                                                  810
* ----------------------------------------------------------------------------
C Begin Local Procedure FLGT                                              812
C FLGT IS ONE PBG TOWER                                                   812
5610  CONTINUE                                                            812
         AG_TITLE='FLGT'                                                  812
         AG_MODULE=MODULE                                                 812
         AG_MARK='V00'                                                    812
         CALL AGSPUSH                                                     812
         AG_MODULE=MODULE                                                 812
C    *                                                                    812
C    Geant MATERIAL : FPDM_AIR ===> inherited arguments used <===         813
         CALL AGSRESET                                                    813
         AG_TITLE = 'FPDM_AIR'                                            813
         AG_EXNAME = 'MATERIAL'                                           813
         AG_PARLIST = 'NONE'                                              813
         CALL AXMATERIAL                                                  813
C    *                                                                    813
C    Geant ATTRIBUTE : FLGT ===> SEEN=1 COLO=2 <===                       813
         CALL AGSRESET                                                    814
         AG_SEEN = 1                                                      814
         AG_COLO = 2                                                      814
         AG_TITLE = 'FLGT'                                                814
         AG_EXNAME = 'ATTRIBUTE'                                          814
         AG_PARLIST = 'SEEN_COLO'                                         814
         CALL AXATTRIBUTE                                                 814
C    *                                                                    814
C    Geant SHAPE : BOX ===> DZ=ZTOT, DX=FLGG_WIDTH/2.0+FLGG_ALTHICK, DY=  814
C    FLGG_WIDTH/2.0+FLGG_ALTHICK <===                                     814
         CALL AGSRESET                                                    817
         AG_DZ = ZTOT                                                     817
         AG_DX = FLGG_WIDTH/2.0+FLGG_ALTHICK                              817
         AG_DY = FLGG_WIDTH/2.0+FLGG_ALTHICK                              817
         AG_TITLE = 'BOX'                                                 817
         AG_EXNAME = 'SHAPE'                                              817
         AG_PARLIST = 'DZ_DX_DY'                                          817
         CALL AXSHAPE                                                     817
C    *                                                                    817
C    Create FWAL :                                                        819
C    Call To Local Procedure FWAL                                         819
         ASSIGN 5640 TO BLOCK_FWAL                                        819
         GO TO  5630                                                      819
5640     CONTINUE                                                         819
C    *                                                                    819
C    Geant POSITION : FWAL ===> Z=-ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0 <=  819
C    ==                                                                   819
         CALL AGSRESET                                                    819
         AG_Z = -ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0                       819
         AG_TITLE = 'FWAL'                                                819
         AG_EXNAME = 'POSITION'                                           819
         AG_PARLIST = 'Z'                                                 819
         CALL AXPOSITION                                                  819
C    *                                                                    819
C    Create FUMT :                                                        820
C    Call To Local Procedure FUMT                                         820
         ASSIGN 5660 TO BLOCK_FUMT                                        820
         GO TO  5650                                                      820
5660     CONTINUE                                                         820
C    *                                                                    820
C    Geant POSITION : FUMT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUM  820
C    ETDZ/2.0 <===                                                        820
         CALL AGSRESET                                                    820
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUMETDZ/2.0            820
         AG_TITLE = 'FUMT'                                                820
         AG_EXNAME = 'POSITION'                                           820
         AG_PARLIST = 'Z'                                                 820
         CALL AXPOSITION                                                  820
C    *                                                                    820
C    Create FPRB :                                                        821
C    Call To Local Procedure FPRB                                         821
         ASSIGN 5680 TO BLOCK_FPRB                                        821
         GO TO  5670                                                      821
5680     CONTINUE                                                         821
C    *                                                                    821
C    Geant POSITION : FPRB ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_SIR  821
C    UBDZ/2.0 <===                                                        821
         CALL AGSRESET                                                    821
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_SIRUBDZ/2.0            821
         AG_TITLE = 'FPRB'                                                821
         AG_EXNAME = 'POSITION'                                           821
         AG_PARLIST = 'Z'                                                 821
         CALL AXPOSITION                                                  821
C    *                                                                    821
C    Create FPCT :                                                        822
C    Call To Local Procedure FPCT                                         822
         ASSIGN 5700 TO BLOCK_FPCT                                        822
         GO TO  5690                                                      822
5700     CONTINUE                                                         822
C    *                                                                    822
C    Geant POSITION : FPCT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_SIR  822
C    UBDZ+FLGG_PHCATHDZ/2.0 <===                                          822
         CALL AGSRESET                                                    822
         AG_Z =                                                           822
     *   -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_SIRUBDZ+FLGG_PHCATHDZ/2.0     822
         AG_TITLE = 'FPCT'                                                822
         AG_EXNAME = 'POSITION'                                           822
         AG_PARLIST = 'Z'                                                 822
         CALL AXPOSITION                                                  822
C    *                                                                    822
5611  CALL AGSPOPB                                                        824
      GO TO BLOCK_FLGT, (5610 ,5620)                                      824
C End of Local Procedure                                                  824
* ----------------------------------------------------------------------------
C Begin Local Procedure FWAL                                              826
C FWAL IS ALMUNUM WRAPPER                                                 826
5630  CONTINUE                                                            826
         AG_TITLE='FWAL'                                                  826
         AG_MODULE=MODULE                                                 826
         AG_MARK='V00'                                                    826
         CALL AGSPUSH                                                     826
         AG_MODULE=MODULE                                                 826
C    *                                                                    826
C    Geant MATERIAL : FPDM_ALUMINIUM ===> inherited arguments used <===   827
         CALL AGSRESET                                                    827
         AG_TITLE = 'FPDM_ALUMINIUM'                                      827
         AG_EXNAME = 'MATERIAL'                                           827
         AG_PARLIST = 'NONE'                                              827
         CALL AXMATERIAL                                                  827
C    *                                                                    827
C    Geant ATTRIBUTE : FWAL ===> SEEN=1 COLO=3 <===                       827
         CALL AGSRESET                                                    828
         AG_SEEN = 1                                                      828
         AG_COLO = 3                                                      828
         AG_TITLE = 'FWAL'                                                828
         AG_EXNAME = 'ATTRIBUTE'                                          828
         AG_PARLIST = 'SEEN_COLO'                                         828
         CALL AXATTRIBUTE                                                 828
C    *                                                                    828
C    Geant SHAPE : BOX ===> DZ=FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0, DX=FLGG_  828
C    WIDTH/2.0+FLGG_ALTHICK, DY=FLGG_WIDTH/2.0+FLGG_ALTHICK <===          828
         CALL AGSRESET                                                    831
         AG_DZ = FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0                          831
         AG_DX = FLGG_WIDTH/2.0+FLGG_ALTHICK                              831
         AG_DY = FLGG_WIDTH/2.0+FLGG_ALTHICK                              831
         AG_TITLE = 'BOX'                                                 831
         AG_EXNAME = 'SHAPE'                                              831
         AG_PARLIST = 'DZ_DX_DY'                                          831
         CALL AXSHAPE                                                     831
C    *                                                                    831
         IF (FPDG_CHKVSIM.EQ.1) CALL GSCKOV(AG_IMED,N,E,ABSCO_ALM,        831
     *   EFFIC_ALL,RINDEX_ALM)                                            831
C    Create FLGR :                                                        834
C    Call To Local Procedure FLGR                                         834
         ASSIGN 5720 TO BLOCK_FLGR                                        834
         GO TO  5710                                                      834
5720     CONTINUE                                                         834
C    *                                                                    834
C    Geant POSITION : FLGR ===> Z=+FLGG_ALTHICK/2.0 <===                  834
         CALL AGSRESET                                                    834
         AG_Z = +FLGG_ALTHICK/2.0                                         834
         AG_TITLE = 'FLGR'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'Z'                                                 834
         CALL AXPOSITION                                                  834
C    *                                                                    834
5631  CALL AGSPOPB                                                        836
      GO TO BLOCK_FWAL, (5630 ,5640)                                      836
C End of Local Procedure                                                  836
* ----------------------------------------------------------------------------
C Begin Local Procedure FLGR                                              838
C FLGR IS LEAD GLASS DETECTOR                                             838
5710  CONTINUE                                                            838
         AG_TITLE='FLGR'                                                  838
         AG_MODULE=MODULE                                                 838
         AG_MARK='V00'                                                    838
         CALL AGSPUSH                                                     838
         AG_MODULE=MODULE                                                 838
C    *                                                                    838
*     PbG is about 65% Pb
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.60712 <===                840
         CALL AGSRESET                                                    840
         AG_A = 207.19                                                    840
         AG_Z = 82                                                        840
         AG_W = .60712                                                    840
         AG_TITLE = 'PB'                                                  840
         AG_EXNAME = 'COMPONENT'                                          840
         AG_PARLIST = 'A_Z_W'                                             840
         CALL AXCOMPONENT                                                 840
C    *                                                                    840
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.02324 <===                 840
         CALL AGSRESET                                                    841
         AG_A = 39.102                                                    841
         AG_Z = 19                                                        841
         AG_W = .02324                                                    841
         AG_TITLE = 'K'                                                   841
         AG_EXNAME = 'COMPONENT'                                          841
         AG_PARLIST = 'A_Z_W'                                             841
         CALL AXCOMPONENT                                                 841
C    *                                                                    841
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.14771 <===                841
         CALL AGSRESET                                                    842
         AG_A = 28.088                                                    842
         AG_Z = 14                                                        842
         AG_W = .14771                                                    842
         AG_TITLE = 'SI'                                                  842
         AG_EXNAME = 'COMPONENT'                                          842
         AG_PARLIST = 'A_Z_W'                                             842
         CALL AXCOMPONENT                                                 842
C    *                                                                    842
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.22041 <===                  842
         CALL AGSRESET                                                    843
         AG_A = 15.999                                                    843
         AG_Z = 8                                                         843
         AG_W = .22041                                                    843
         AG_TITLE = 'O'                                                   843
         AG_EXNAME = 'COMPONENT'                                          843
         AG_PARLIST = 'A_Z_W'                                             843
         CALL AXCOMPONENT                                                 843
C    *                                                                    843
C    Geant COMPONENT : AS ===> A=74.922 Z=33 W=.00152 <===                843
         CALL AGSRESET                                                    844
         AG_A = 74.922                                                    844
         AG_Z = 33                                                        844
         AG_W = .00152                                                    844
         AG_TITLE = 'AS'                                                  844
         AG_EXNAME = 'COMPONENT'                                          844
         AG_PARLIST = 'A_Z_W'                                             844
         CALL AXCOMPONENT                                                 844
C    *                                                                    844
C    Geant MIXTURE : PBG ===> DENS=FLGM_DENSITY RADL=FLGM_RADLEN <===     844
         CALL AGSRESET                                                    845
         AG_DENS = FLGM_DENSITY                                           845
         AG_RADL = FLGM_RADLEN                                            845
         AG_TITLE = 'PBG'                                                 845
         AG_EXNAME = 'MIXTURE'                                            845
         AG_PARLIST = 'DENS_RADL'                                         845
         CALL AXMIXTURE                                                   845
C    *                                                                    845
c$$$      Medium leadglass ISVOL=1
C    Geant MATERIAL : FPDM_PBGLASS ===> ISVOL=1 <===                      848
         CALL AGSRESET                                                    848
         AG_ISVOL = 1                                                     848
         AG_TITLE = 'FPDM_PBGLASS'                                        848
         AG_EXNAME = 'MATERIAL'                                           848
         AG_PARLIST = 'ISVOL'                                             848
         CALL AXMATERIAL                                                  848
C    *                                                                    848
C    Geant ATTRIBUTE : FLGR ===> SEEN=1 COLO=4 ! RED <===                 850
         CALL AGSRESET                                                    850
         AG_SEEN = 1                                                      850
C    RED                                                                  850
         AG_COLO = 4                                                      850
         AG_TITLE = 'FLGR'                                                850
         AG_EXNAME = 'ATTRIBUTE'                                          850
         AG_PARLIST = 'SEEN_COLO'                                         850
         CALL AXATTRIBUTE                                                 850
C    *                                                                    850
C    Geant SHAPE : BOX ===> DZ=FLGG_DEPTH/2 DX=FLGG_WIDTH/2 DY=FLGG_WIDT  850
C    H/2 <===                                                             850
         CALL AGSRESET                                                    851
         AG_DZ = FLGG_DEPTH/2                                             851
         AG_DX = FLGG_WIDTH/2                                             851
         AG_DY = FLGG_WIDTH/2                                             851
         AG_TITLE = 'BOX'                                                 851
         AG_EXNAME = 'SHAPE'                                              851
         AG_PARLIST = 'DZ_DX_DY'                                          851
         CALL AXSHAPE                                                     851
C    *                                                                    851
         IF (FPDG_CHKVSIM.EQ.1) THEN                                      853
         CALL GSCKOV(AG_IMED,N,E,ABSCO_PBG,EFFIC_ALL,RINDEX_PBG)          854
         ELSE                                                             855
         CALL GSTPAR (AG_IMED,'CUTELE', FLGM_CRITENE)                     856
C    Defining Hits: ELOSS:0:(0,10)                                        857
         AG_TITLE = 'FLGR'                                                857
         AG_EXNAME = 'HITS'                                               857
         AG_TYPE=AG_MAGIC                                                 857
         CALL AGSDETV('FLGR')                                             857
C    *                                                                    857
         AG_PARLIST='ELOSS'                                               857
         AG_TITLE = 'DETH'                                                857
         AG_NBITS = 0                                                     857
         AG_BIN=AG_MAGIC                                                  857
         AG_OPTION='*'                                                    857
         AG_FHMIN = 0                                                     857
         AG_FHMAX = 10                                                    857
         CALL AGSDETH (0)                                                 857
C    *                                                                    857
         CALL AGSDETU('FLGR')                                             857
C    *                                                                    857
         ENDIF                                                            858
5711  CALL AGSPOPB                                                        859
      GO TO BLOCK_FLGR, (5710 ,5720)                                      859
C End of Local Procedure                                                  859
* ----------------------------------------------------------------------------
C Begin Local Procedure FPRB                                              861
C FPRB IS SILICON RUBBER                                                  861
5670  CONTINUE                                                            861
         AG_TITLE='FPRB'                                                  861
         AG_MODULE=MODULE                                                 861
         AG_MARK='V00'                                                    861
         CALL AGSPUSH                                                     861
         AG_MODULE=MODULE                                                 861
C    *                                                                    861
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          862
         CALL AGSRESET                                                    862
         AG_TITLE = 'SILICON'                                             862
         AG_EXNAME = 'MATERIAL'                                           862
         AG_PARLIST = 'NONE'                                              862
         CALL AXMATERIAL                                                  862
C    *                                                                    862
C    Geant MATERIAL : FPDM_SILICON ===> ISVOL=1 <===                      862
         CALL AGSRESET                                                    863
         AG_ISVOL = 1                                                     863
         AG_TITLE = 'FPDM_SILICON'                                        863
         AG_EXNAME = 'MATERIAL'                                           863
         AG_PARLIST = 'ISVOL'                                             863
         CALL AXMATERIAL                                                  863
C    *                                                                    863
C    Geant ATTRIBUTE : FPRB ===> SEEN=1 COLO=5 <===                       863
         CALL AGSRESET                                                    864
         AG_SEEN = 1                                                      864
         AG_COLO = 5                                                      864
         AG_TITLE = 'FPRB'                                                864
         AG_EXNAME = 'ATTRIBUTE'                                          864
         AG_PARLIST = 'SEEN_COLO'                                         864
         CALL AXATTRIBUTE                                                 864
C    *                                                                    864
C    Geant SHAPE : TUBE ===> DZ=FLGG_SIRUBDZ/2.0 RMIN=0 RMAX=FLGG_PHCATH  864
C    R <===                                                               864
         CALL AGSRESET                                                    865
         AG_DZ = FLGG_SIRUBDZ/2.0                                         865
         AG_RMIN = 0                                                      865
         AG_RMAX = FLGG_PHCATHR                                           865
         AG_TITLE = 'TUBE'                                                865
         AG_EXNAME = 'SHAPE'                                              865
         AG_PARLIST = 'DZ_RMIN_RMAX'                                      865
         CALL AXSHAPE                                                     865
C    *                                                                    865
         IF (FPDG_CHKVSIM.EQ.1) CALL GSCKOV(AG_IMED,N,E,ABSCO_SIRUB,      865
     *   EFFIC_ALL,RINDEX_SIRUB)                                          865
5671  CALL AGSPOPB                                                        867
      GO TO BLOCK_FPRB, (5670 ,5680)                                      867
C End of Local Procedure                                                  867
* ----------------------------------------------------------------------------
C Begin Local Procedure FPCT                                              869
C FPCT IS PHOTO CATHODE                                                   869
5690  CONTINUE                                                            869
         AG_TITLE='FPCT'                                                  869
         AG_MODULE=MODULE                                                 869
         AG_MARK='V00'                                                    869
         CALL AGSPUSH                                                     869
         AG_MODULE=MODULE                                                 869
C    *                                                                    869
C    Geant MATERIAL : AIR ===> inherited arguments used <===              870
         CALL AGSRESET                                                    870
         AG_TITLE = 'AIR'                                                 870
         AG_EXNAME = 'MATERIAL'                                           870
         AG_PARLIST = 'NONE'                                              870
         CALL AXMATERIAL                                                  870
C    *                                                                    870
c$$$      Medium PhotCath ISVOL=1
C    Geant MATERIAL : FPDM_AIR_FPCT ===> ISVOL=1 <===                     872
         CALL AGSRESET                                                    872
         AG_ISVOL = 1                                                     872
         AG_TITLE = 'FPDM_AIR_FPCT'                                       872
         AG_EXNAME = 'MATERIAL'                                           872
         AG_PARLIST = 'ISVOL'                                             872
         CALL AXMATERIAL                                                  872
C    *                                                                    872
C    Geant ATTRIBUTE : LPCT ===> SEEN=1 COLO=6 <===                       872
         CALL AGSRESET                                                    873
         AG_SEEN = 1                                                      873
         AG_COLO = 6                                                      873
         AG_TITLE = 'LPCT'                                                873
         AG_EXNAME = 'ATTRIBUTE'                                          873
         AG_PARLIST = 'SEEN_COLO'                                         873
         CALL AXATTRIBUTE                                                 873
C    *                                                                    873
C    Geant SHAPE : TUBE ===> DZ=FLGG_PHCATHDZ/2.0 RMIN=0 RMAX=FLGG_PHCAT  873
C    HR <===                                                              873
         CALL AGSRESET                                                    874
         AG_DZ = FLGG_PHCATHDZ/2.0                                        874
         AG_RMIN = 0                                                      874
         AG_RMAX = FLGG_PHCATHR                                           874
         AG_TITLE = 'TUBE'                                                874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DZ_RMIN_RMAX'                                      874
         CALL AXSHAPE                                                     874
C    *                                                                    874
         IF (FPDG_CHKVSIM.EQ.1) THEN                                      874
         CALL GSCKOV(AG_IMED,N,E,ABSCO_PHCATH,EFFIC_PHCATH,               876
     *   RINDEX_PHCATH)                                                   876
C    Defining Hits: USER:0:(0,10000)                                      877
         AG_TITLE = 'FPCT'                                                877
         AG_EXNAME = 'HITS'                                               877
         AG_TYPE=AG_MAGIC                                                 877
         CALL AGSDETV('FPCT')                                             877
C    *                                                                    877
         AG_PARLIST='USER'                                                877
         AG_TITLE = 'DETH'                                                877
         AG_NBITS = 0                                                     877
         AG_BIN=AG_MAGIC                                                  877
         AG_OPTION='*'                                                    877
         AG_FHMIN = 0                                                     877
         AG_FHMAX = 10000                                                 877
         CALL AGSDETH (JUMPAD(FPCTSTEP))                                  877
C    *                                                                    877
         CALL AGSDETU('FPCT')                                             877
C    *                                                                    877
*      HITS FPCT USER:0:(0,100000)
         ENDIF                                                            879
5691  CALL AGSPOPB                                                        880
      GO TO BLOCK_FPCT, (5690 ,5700)                                      880
C End of Local Procedure                                                  880
* ----------------------------------------------------------------------------
C Begin Local Procedure FUMT                                              882
C FUMT IS MU METAL                                                        882
5650  CONTINUE                                                            882
         AG_TITLE='FUMT'                                                  882
         AG_MODULE=MODULE                                                 882
         AG_MARK='V00'                                                    882
         CALL AGSPUSH                                                     882
         AG_MODULE=MODULE                                                 882
C    *                                                                    882
C    Geant MATERIAL : IRON ===> inherited arguments used <===             883
         CALL AGSRESET                                                    883
         AG_TITLE = 'IRON'                                                883
         AG_EXNAME = 'MATERIAL'                                           883
         AG_PARLIST = 'NONE'                                              883
         CALL AXMATERIAL                                                  883
C    *                                                                    883
C    Geant MATERIAL : FPDM_IRON_FUMT ===> ISVOL=0 <===                    883
         CALL AGSRESET                                                    884
         AG_ISVOL = 0                                                     884
         AG_TITLE = 'FPDM_IRON_FUMT'                                      884
         AG_EXNAME = 'MATERIAL'                                           884
         AG_PARLIST = 'ISVOL'                                             884
         CALL AXMATERIAL                                                  884
C    *                                                                    884
C    Geant ATTRIBUTE : LUMT ===> SEEN=1 COLO=1 <===                       884
         CALL AGSRESET                                                    885
         AG_SEEN = 1                                                      885
         AG_COLO = 1                                                      885
         AG_TITLE = 'LUMT'                                                885
         AG_EXNAME = 'ATTRIBUTE'                                          885
         AG_PARLIST = 'SEEN_COLO'                                         885
         CALL AXATTRIBUTE                                                 885
C    *                                                                    885
C    Geant SHAPE : TUBE ===> DZ=FLGG_MUMETDZ/2.0 RMIN=FLGG_PHCATHR RMAX=  885
C    FLGG_MUMETR <===                                                     885
         CALL AGSRESET                                                    886
         AG_DZ = FLGG_MUMETDZ/2.0                                         886
         AG_RMIN = FLGG_PHCATHR                                           886
         AG_RMAX = FLGG_MUMETR                                            886
         AG_TITLE = 'TUBE'                                                886
         AG_EXNAME = 'SHAPE'                                              886
         AG_PARLIST = 'DZ_RMIN_RMAX'                                      886
         CALL AXSHAPE                                                     886
C    *                                                                    886
         IF (FPDG_CHKVSIM.EQ.1) CALL GSCKOV(AG_IMED,N,E,ABSCO_MUMET,      886
     *   EFFIC_ALL,RINDEX_MUMET)                                          886
5651  CALL AGSPOPB                                                        888
      GO TO BLOCK_FUMT, (5650 ,5660)                                      888
C End of Local Procedure                                                  888
* ----------------------------------------------------------------------------
1     CONTINUE                                                            890
      END                                                                 890
