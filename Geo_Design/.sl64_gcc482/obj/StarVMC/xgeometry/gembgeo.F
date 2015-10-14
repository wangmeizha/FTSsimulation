C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: gembgeo.g,v 1.1 2005/06/28 16:16:24 potekhin Exp $
*
* $Log: gembgeo.g,v $
* Revision 1.1  2005/06/28 16:16:24  potekhin
* Gem barrel tracker code created by Nikolai and Gerrit.
* Only active in the tag IST1 for now. Tested by Maxim.
*
******************************************************************
      SUBROUTINE GEMBGEO                                                  9
C GEMBGEO IS THE GEOMETRY OF GEMS IN FRONT OF THE TPC                     9
      IMPLICIT NONE                                                       9
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       9
      DATA MODULE/'GEMBGEO'/, BANK_TITLE/'*'/                             9
      DATA MCOMMENT/'IS THE GEOMETRY OF GEMS IN FRONT OF THE TPC'/        9
      LOGICAL FIRST                                                       9
      DATA FIRST/.TRUE./                                                  9
      SAVE FIRST                                                          9
      INTEGER JUMPAD                                                      9
      EXTERNAL JUMPAD                                                     9
      INTEGER OK                                                          9
      PARAMETER (OK=0)                                                    9
      INTEGER AG_I,AG_J                                                   9
      SAVE AG_I,AG_J                                                      9
C AUTHOR NNS                                                              9
      DATA AG_I/0/,AG_J/0/                                                10
      CHARACTER*40 AUTHOR /'NNS'/                                         10
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  10
C CREATED SEPT 25, 2004                                                   10
      CHARACTER*40 CREATED /'SEPT 25, 2004'/                              11
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  11
*     Modified by GvN, May 18, 2005
*     Added mothervolumes etc.
*     Code review by Maxim Potekhin, June 28, 2005, CVS tags added
*
*    Notes:
*    Two layers of GEM strip detectors (check gem_pattern)
*    First (or Inner)  -- R ~ 34 cm
*    Second (or Outer) -- R ~ 40 cm
*
* -----------------------------------------------------------
C - Advanced Geant interface                                              23
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      23
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       23
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         23
     *AG_OPTION,AG_ORT,AG_MARK                                            23
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         23
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     23
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     23
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    23
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          23
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   23
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     23
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        23
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   23
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 23
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     23
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       23
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        23
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       23
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       23
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       23
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         23
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     23
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       23
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   23
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       23
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     23
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          23
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         23
     *-696969,AG_LDETU=250)                                               23
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    23
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  23
     *AG_IMFLD,AG_ISIMU                                                   23
C Inherited variables a: saved during internal calls                      23
C b: reset at the entry in a block and then saved                         23
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        23
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       23
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         23
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     23
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       23
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         23
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           23
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        23
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        23
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            23
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           23
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    23
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      23
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           23
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          23
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                23
C local variables valid inside same block                                 23
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       23
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  23
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         23
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    23
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          23
     *AG_OPTION                                                           23
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        23
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             23
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              23
C temporarly                                                              23
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           23
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    23
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       23
     *AG_HY)                                                              23
      REAL MIL_P,INCH_P                                                   23
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               23
C - GEANT description of logical units                                    23
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            23
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      23
      COMMON/GCMAIL/ CHMAIL                                               23
      CHARACTER*132 CHMAIL                                                23
C - Natural constants (or considered as such)                             23
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   23
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        23
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       23
      INTEGER N, K                                                        25
      REAL UNGL, ANGL                                                     26
      INTEGER BLOCK_GMBO                                                  28
      INTEGER BLOCK_GMVI                                                  28
      INTEGER BLOCK_GMDI                                                  28
      INTEGER BLOCK_GMVO                                                  28
      INTEGER BLOCK_GMDO                                                  28
      INTEGER BLOCK_GMMI                                                  28
      INTEGER BLOCK_GMGI                                                  28
      INTEGER BLOCK_GMMO                                                  28
      INTEGER BLOCK_GMGO                                                  28
C -                                                                       30
      REAL GMMG_VERSION                                                   30
      EQUIVALENCE (BANK_GMMG(3),GMMG_VERSION,RE_GMMG_VERSION),            30
     *(NAMES_GMMG(1),NGMMG_VERSION)                                       30
      EQUIVALENCE (COMMEN_GMMG(1),CGMMG_VERSION),(MAP_GMMG(1,1),          30
     *MGMMG_VERSION)                                                      30
      CHARACTER CGMMG_VERSION*40/' '/                                     30
      CHARACTER NGMMG_VERSION*12/'R.VERSION'/                             30
      INTEGER MGMMG_VERSION(2)/1,1/                                       30
      REAL RE_GMMG_VERSION                                                30
C -                                                                       30
      REAL GMMG_RIN                                                       30
      EQUIVALENCE (BANK_GMMG(4),GMMG_RIN,RE_GMMG_RIN),(NAMES_GMMG(2),     30
     *NGMMG_RIN)                                                          30
      EQUIVALENCE (COMMEN_GMMG(2),CGMMG_RIN),(MAP_GMMG(1,2),MGMMG_RIN)    30
      CHARACTER CGMMG_RIN*40/' '/                                         30
      CHARACTER NGMMG_RIN*12/'R.RIN'/                                     30
      INTEGER MGMMG_RIN(2)/1,1/                                           30
      REAL RE_GMMG_RIN                                                    30
C -                                                                       30
      REAL GMMG_ROUT                                                      30
      EQUIVALENCE (BANK_GMMG(5),GMMG_ROUT,RE_GMMG_ROUT),(NAMES_GMMG(3),   30
     *NGMMG_ROUT)                                                         30
      EQUIVALENCE (COMMEN_GMMG(3),CGMMG_ROUT),(MAP_GMMG(1,3),             30
     *MGMMG_ROUT)                                                         30
      CHARACTER CGMMG_ROUT*40/' '/                                        30
      CHARACTER NGMMG_ROUT*12/'R.ROUT'/                                   30
      INTEGER MGMMG_ROUT(2)/1,1/                                          30
      REAL RE_GMMG_ROUT                                                   30
C -                                                                       30
      REAL GMMG_TOTALLENGTH                                               30
      EQUIVALENCE (BANK_GMMG(6),GMMG_TOTALLENGTH,RE_GMMG_TOTALLENGTH),    30
     *(NAMES_GMMG(4),NGMMG_TOTALLENGTH)                                   30
      EQUIVALENCE (COMMEN_GMMG(4),CGMMG_TOTALLENGTH),(MAP_GMMG(1,4),      30
     *MGMMG_TOTALLENGTH)                                                  30
      CHARACTER CGMMG_TOTALLENGTH*40/' '/                                 30
      CHARACTER NGMMG_TOTALLENGTH*12/'R.TOTALLENGTH'/                     30
      INTEGER MGMMG_TOTALLENGTH(2)/1,1/                                   30
      REAL RE_GMMG_TOTALLENGTH                                            30
C -                                                                       30
      INTEGER LEN_GMMG,LLEN_GMMG,LINK_GMMG                                30
      PARAMETER (LEN_GMMG=6)                                              30
      PARAMETER (LLEN_GMMG=4)                                             30
      INTEGER FLAG_GMMG/0/,MAP_GMMG(2,LLEN_GMMG),IDNUM_GMMG(10)           30
      CHARACTER NAMES_GMMG(LLEN_GMMG)*12,COMMEN_GMMG(LLEN_GMMG)*40        30
      DATA IDNUM_GMMG/10*0/                                               30
      REAL BANK_GMMG(LEN_GMMG)                                            30
      CHARACTER*60 BPATH_GMMG/' '/                                        30
      SAVE FLAG_GMMG,MAP_GMMG,IDNUM_GMMG,NAMES_GMMG,COMMEN_GMMG,          30
     *BPATH_GMMG                                                          30
      DATA LINK_GMMG/0/,BANK_GMMG/LEN_GMMG*0/                             30
C *                                                                       30
C -                                                                       33
      REAL GEMV_LAYER                                                     33
      EQUIVALENCE (BANK_GEMV(3),GEMV_LAYER,RE_GEMV_LAYER),                33
     *(NAMES_GEMV(1),NGEMV_LAYER)                                         33
      EQUIVALENCE (COMMEN_GEMV(1),CGEMV_LAYER),(MAP_GEMV(1,1),            33
     *MGEMV_LAYER)                                                        33
      CHARACTER CGEMV_LAYER*40/' '/                                       33
      CHARACTER NGEMV_LAYER*12/'R.LAYER'/                                 33
      INTEGER MGEMV_LAYER(2)/1,1/                                         33
      REAL RE_GEMV_LAYER                                                  33
C -                                                                       33
      REAL GEMV_DX                                                        33
      EQUIVALENCE (BANK_GEMV(4),GEMV_DX,RE_GEMV_DX),(NAMES_GEMV(2),       33
     *NGEMV_DX)                                                           33
      EQUIVALENCE (COMMEN_GEMV(2),CGEMV_DX),(MAP_GEMV(1,2),MGEMV_DX)      33
      CHARACTER CGEMV_DX*40/' '/                                          33
      CHARACTER NGEMV_DX*12/'R.DX'/                                       33
      INTEGER MGEMV_DX(2)/1,1/                                            33
      REAL RE_GEMV_DX                                                     33
C -                                                                       33
      REAL GEMV_DY                                                        33
      EQUIVALENCE (BANK_GEMV(5),GEMV_DY,RE_GEMV_DY),(NAMES_GEMV(3),       33
     *NGEMV_DY)                                                           33
      EQUIVALENCE (COMMEN_GEMV(3),CGEMV_DY),(MAP_GEMV(1,3),MGEMV_DY)      33
      CHARACTER CGEMV_DY*40/' '/                                          33
      CHARACTER NGEMV_DY*12/'R.DY'/                                       33
      INTEGER MGEMV_DY(2)/1,1/                                            33
      REAL RE_GEMV_DY                                                     33
C -                                                                       33
      REAL GEMV_DZ                                                        33
      EQUIVALENCE (BANK_GEMV(6),GEMV_DZ,RE_GEMV_DZ),(NAMES_GEMV(4),       33
     *NGEMV_DZ)                                                           33
      EQUIVALENCE (COMMEN_GEMV(4),CGEMV_DZ),(MAP_GEMV(1,4),MGEMV_DZ)      33
      CHARACTER CGEMV_DZ*40/' '/                                          33
      CHARACTER NGEMV_DZ*12/'R.DZ'/                                       33
      INTEGER MGEMV_DZ(2)/1,1/                                            33
      REAL RE_GEMV_DZ                                                     33
C -                                                                       33
      REAL GEMV_RPOS                                                      33
      EQUIVALENCE (BANK_GEMV(7),GEMV_RPOS,RE_GEMV_RPOS),(NAMES_GEMV(5),   33
     *NGEMV_RPOS)                                                         33
      EQUIVALENCE (COMMEN_GEMV(5),CGEMV_RPOS),(MAP_GEMV(1,5),             33
     *MGEMV_RPOS)                                                         33
      CHARACTER CGEMV_RPOS*40/' '/                                        33
      CHARACTER NGEMV_RPOS*12/'R.RPOS'/                                   33
      INTEGER MGEMV_RPOS(2)/1,1/                                          33
      REAL RE_GEMV_RPOS                                                   33
C -                                                                       33
      REAL GEMV_PHI                                                       33
      EQUIVALENCE (BANK_GEMV(8),GEMV_PHI,RE_GEMV_PHI),(NAMES_GEMV(6),     33
     *NGEMV_PHI)                                                          33
      EQUIVALENCE (COMMEN_GEMV(6),CGEMV_PHI),(MAP_GEMV(1,6),MGEMV_PHI)    33
      CHARACTER CGEMV_PHI*40/' '/                                         33
      CHARACTER NGEMV_PHI*12/'R.PHI'/                                     33
      INTEGER MGEMV_PHI(2)/1,1/                                           33
      REAL RE_GEMV_PHI                                                    33
C -                                                                       33
      REAL GEMV_NSD                                                       33
      EQUIVALENCE (BANK_GEMV(9),GEMV_NSD,RE_GEMV_NSD),(NAMES_GEMV(7),     33
     *NGEMV_NSD)                                                          33
      EQUIVALENCE (COMMEN_GEMV(7),CGEMV_NSD),(MAP_GEMV(1,7),MGEMV_NSD)    33
      CHARACTER CGEMV_NSD*40/' '/                                         33
      CHARACTER NGEMV_NSD*12/'R.NSD'/                                     33
      INTEGER MGEMV_NSD(2)/1,1/                                           33
      REAL RE_GEMV_NSD                                                    33
C -                                                                       33
      REAL GEMV_NZ                                                        33
      EQUIVALENCE (BANK_GEMV(10),GEMV_NZ,RE_GEMV_NZ),(NAMES_GEMV(8),      33
     *NGEMV_NZ)                                                           33
      EQUIVALENCE (COMMEN_GEMV(8),CGEMV_NZ),(MAP_GEMV(1,8),MGEMV_NZ)      33
      CHARACTER CGEMV_NZ*40/' '/                                          33
      CHARACTER NGEMV_NZ*12/'R.NZ'/                                       33
      INTEGER MGEMV_NZ(2)/1,1/                                            33
      REAL RE_GEMV_NZ                                                     33
C -                                                                       33
      REAL GEMV_ALPHA                                                     33
      EQUIVALENCE (BANK_GEMV(11),GEMV_ALPHA,RE_GEMV_ALPHA),               33
     *(NAMES_GEMV(9),NGEMV_ALPHA)                                         33
      EQUIVALENCE (COMMEN_GEMV(9),CGEMV_ALPHA),(MAP_GEMV(1,9),            33
     *MGEMV_ALPHA)                                                        33
      CHARACTER CGEMV_ALPHA*40/' '/                                       33
      CHARACTER NGEMV_ALPHA*12/'R.ALPHA'/                                 33
      INTEGER MGEMV_ALPHA(2)/1,1/                                         33
      REAL RE_GEMV_ALPHA                                                  33
C -                                                                       33
      REAL GEMV_FRTH                                                      33
      EQUIVALENCE (BANK_GEMV(12),GEMV_FRTH,RE_GEMV_FRTH),                 33
     *(NAMES_GEMV(10),NGEMV_FRTH)                                         33
      EQUIVALENCE (COMMEN_GEMV(10),CGEMV_FRTH),(MAP_GEMV(1,10),           33
     *MGEMV_FRTH)                                                         33
      CHARACTER CGEMV_FRTH*40/' '/                                        33
      CHARACTER NGEMV_FRTH*12/'R.FRTH'/                                   33
      INTEGER MGEMV_FRTH(2)/1,1/                                          33
      REAL RE_GEMV_FRTH                                                   33
C -                                                                       33
      REAL GEMV_RDTH                                                      33
      EQUIVALENCE (BANK_GEMV(13),GEMV_RDTH,RE_GEMV_RDTH),                 33
     *(NAMES_GEMV(11),NGEMV_RDTH)                                         33
      EQUIVALENCE (COMMEN_GEMV(11),CGEMV_RDTH),(MAP_GEMV(1,11),           33
     *MGEMV_RDTH)                                                         33
      CHARACTER CGEMV_RDTH*40/' '/                                        33
      CHARACTER NGEMV_RDTH*12/'R.RDTH'/                                   33
      INTEGER MGEMV_RDTH(2)/1,1/                                          33
      REAL RE_GEMV_RDTH                                                   33
C -                                                                       33
      REAL GEMV_MLRTH                                                     33
      EQUIVALENCE (BANK_GEMV(14),GEMV_MLRTH,RE_GEMV_MLRTH),               33
     *(NAMES_GEMV(12),NGEMV_MLRTH)                                        33
      EQUIVALENCE (COMMEN_GEMV(12),CGEMV_MLRTH),(MAP_GEMV(1,12),          33
     *MGEMV_MLRTH)                                                        33
      CHARACTER CGEMV_MLRTH*40/' '/                                       33
      CHARACTER NGEMV_MLRTH*12/'R.MLRTH'/                                 33
      INTEGER MGEMV_MLRTH(2)/1,1/                                         33
      REAL RE_GEMV_MLRTH                                                  33
C -                                                                       33
      REAL GEMV_SGASTH                                                    33
      EQUIVALENCE (BANK_GEMV(15),GEMV_SGASTH,RE_GEMV_SGASTH),             33
     *(NAMES_GEMV(13),NGEMV_SGASTH)                                       33
      EQUIVALENCE (COMMEN_GEMV(13),CGEMV_SGASTH),(MAP_GEMV(1,13),         33
     *MGEMV_SGASTH)                                                       33
      CHARACTER CGEMV_SGASTH*40/' '/                                      33
      CHARACTER NGEMV_SGASTH*12/'R.SGASTH'/                               33
      INTEGER MGEMV_SGASTH(2)/1,1/                                        33
      REAL RE_GEMV_SGASTH                                                 33
C -                                                                       33
      INTEGER LEN_GEMV,LLEN_GEMV,LINK_GEMV                                33
      PARAMETER (LEN_GEMV=15)                                             33
      PARAMETER (LLEN_GEMV=13)                                            33
      INTEGER FLAG_GEMV/0/,MAP_GEMV(2,LLEN_GEMV),IDNUM_GEMV(10)           33
      CHARACTER NAMES_GEMV(LLEN_GEMV)*12,COMMEN_GEMV(LLEN_GEMV)*40        33
      DATA IDNUM_GEMV/10*0/                                               33
      REAL BANK_GEMV(LEN_GEMV)                                            33
      CHARACTER*60 BPATH_GEMV/' '/                                        33
      SAVE FLAG_GEMV,MAP_GEMV,IDNUM_GEMV,NAMES_GEMV,COMMEN_GEMV,          33
     *BPATH_GEMV                                                          33
      DATA LINK_GEMV/0/,BANK_GEMV/LEN_GEMV*0/                             33
C *                                                                       33
*Begin
*-----------------------------------------------------------------
C *                                                                       38
      CALL AGSBEGM(MODULE,IPRIN)                                          38
      IF (IPRIN.LT.0) RETURN                                              38
C Filling bank GMMG - MOTHER VOLUME DATA                                  38
      BANK_TITLE='MOTHER VOLUME DATA'                                     38
      BPATH_GMMG='GMMG*'                                                  38
      IDNUM_GMMG(1)=0                                                     39
C VERSION                                                                 39
      GMMG_VERSION=1                                                      39
      CGMMG_VERSION='VERSION'                                             39
C INNER RADIUS                                                            40
      GMMG_RIN=28.0                                                       40
      CGMMG_RIN='INNER RADIUS'                                            40
C OUTER RADIUS                                                            41
      GMMG_ROUT=42.0                                                      41
      CGMMG_ROUT='OUTER RADIUS'                                           41
C MAXIMUM LENGTH OF THE DETECTOR                                          42
      GMMG_TOTALLENGTH=260.0                                              42
      CGMMG_TOTALLENGTH='MAXIMUM LENGTH OF THE DETECTOR'                  42
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GMMG',    43
     *BPATH_GMMG,IDNUM_GMMG,1, MAP_GMMG,NAMES_GMMG,COMMEN_GMMG,           43
     *BANK_GMMG,LEN_GMMG,LLEN_GMMG,LINK_GMMG,FLAG_GMMG)                   43
C *                                                                       43
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GMMG',    43
     *BPATH_GMMG,IDNUM_GMMG,1, MAP_GMMG,NAMES_GMMG,COMMEN_GMMG,           43
     *BANK_GMMG,LEN_GMMG,LLEN_GMMG,LINK_GMMG,FLAG_GMMG)                   43
*-----------------------------------------------------------------
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank GEMV - GEM STRIP DET SYSTEM                                45
      BANK_TITLE='GEM STRIP DET SYSTEM'                                   45
      BPATH_GEMV='GEMV*'                                                  45
      IDNUM_GEMV(1)=0                                                     46
C INNER LAYER                                                             46
      GEMV_LAYER=1                                                        46
      CGEMV_LAYER='INNER LAYER'                                           46
C ONE DETECTOR DX ( I , FOR O - X2) ORG=5.0CM ORG2=5.5                    47
      GEMV_DX=5.5                                                         47
      CGEMV_DX='ONE DETECTOR DX ( I , FOR O - X2) ORG=5.0CM ORG2=5.5'     47
C ONE DETECTOR DY (THICKNESS)                                             48
      GEMV_DY=0.5                                                         48
      CGEMV_DY='ONE DETECTOR DY (THICKNESS)'                              48
C ONE DETECTOR DZ ( I , FOR O - X2)                                       49
      GEMV_DZ=5.0                                                         49
      CGEMV_DZ='ONE DETECTOR DZ ( I , FOR O - X2)'                        49
C R POSITION I LAYER ORG=30.0                                             50
      GEMV_RPOS=33.0                                                      50
      CGEMV_RPOS='R POSITION I LAYER ORG=30.0'                            50
C ANGLE IN PHI                                                            51
      GEMV_PHI=18.94737                                                   51
      CGEMV_PHI='ANGLE IN PHI'                                            51
C N OF LADENS IN PHI                                                      52
      GEMV_NSD=19                                                         52
      CGEMV_NSD='N OF LADENS IN PHI'                                      52
C N OF LADENS IN Z                                                        53
      GEMV_NZ=16                                                          53
      CGEMV_NZ='N OF LADENS IN Z'                                         53
C INNER LAYER SHIFT ANGLE ORG=8.0                                         54
      GEMV_ALPHA=10.0                                                     54
      CGEMV_ALPHA='INNER LAYER SHIFT ANGLE ORG=8.0'                       54
C FRAME THICKNESS                                                         55
      GEMV_FRTH=0.35                                                      55
      CGEMV_FRTH='FRAME THICKNESS'                                        55
C READ-OUT PLATE THICKNESS                                                56
      GEMV_RDTH=0.25                                                      56
      CGEMV_RDTH='READ-OUT PLATE THICKNESS'                               56
C MYLAR WINDOW THICKNESS                                                  57
      GEMV_MLRTH=0.04                                                     57
      CGEMV_MLRTH='MYLAR WINDOW THICKNESS'                                57
C SENSIVE GAS THICKNESS                                                   58
      GEMV_SGASTH=0.3                                                     58
      CGEMV_SGASTH='SENSIVE GAS THICKNESS'                                58
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GEMV',    59
     *BPATH_GEMV,IDNUM_GEMV,1, MAP_GEMV,NAMES_GEMV,COMMEN_GEMV,           59
     *BANK_GEMV,LEN_GEMV,LLEN_GEMV,LINK_GEMV,FLAG_GEMV)                   59
C *                                                                       59
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GEMV',    59
     *BPATH_GEMV,IDNUM_GEMV,1, MAP_GEMV,NAMES_GEMV,COMMEN_GEMV,           59
     *BANK_GEMV,LEN_GEMV,LLEN_GEMV,LINK_GEMV,FLAG_GEMV)                   59
C *                                                                       61
      CALL AGSBEGM(MODULE,IPRIN)                                          61
      IF (IPRIN.LT.0) RETURN                                              61
C Filling bank GEMV - GEM STRIP DET SYSTEM                                61
      BANK_TITLE='GEM STRIP DET SYSTEM'                                   61
      BPATH_GEMV='GEMV*'                                                  61
      IDNUM_GEMV(1)=0                                                     62
C OUTER LAYER                                                             62
      GEMV_LAYER=2                                                        62
      CGEMV_LAYER='OUTER LAYER'                                           62
C R POSITION O LAYER ORG=38.0                                             63
      GEMV_RPOS=40.0                                                      63
      CGEMV_RPOS='R POSITION O LAYER ORG=38.0'                            63
C ANGLE STEP IN PHI                                                       64
      GEMV_PHI=13.84615                                                   64
      CGEMV_PHI='ANGLE STEP IN PHI'                                       64
C N OF LADENS IN PHI                                                      65
      GEMV_NSD=26                                                         65
      CGEMV_NSD='N OF LADENS IN PHI'                                      65
C N OF LADENS IN Z                                                        66
      GEMV_NZ=24                                                          66
      CGEMV_NZ='N OF LADENS IN Z'                                         66
C OUTER LAYER ROTATION ANGLE ORG=5.0                                      67
      GEMV_ALPHA=10.0                                                     67
      CGEMV_ALPHA='OUTER LAYER ROTATION ANGLE ORG=5.0'                    67
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GEMV',    68
     *BPATH_GEMV,IDNUM_GEMV,1, MAP_GEMV,NAMES_GEMV,COMMEN_GEMV,           68
     *BANK_GEMV,LEN_GEMV,LLEN_GEMV,LINK_GEMV,FLAG_GEMV)                   68
C *                                                                       68
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GEMV',    68
     *BPATH_GEMV,IDNUM_GEMV,1, MAP_GEMV,NAMES_GEMV,COMMEN_GEMV,           68
     *BANK_GEMV,LEN_GEMV,LLEN_GEMV,LINK_GEMV,FLAG_GEMV)                   68
*-----------------------------------------------------------------
*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             72
      CALL AGSRESET                                                       72
      AG_A = 28.08                                                        72
      AG_Z = 14                                                           72
      AG_W = 0.6*1*28./60.                                                72
      AG_TITLE = 'SI'                                                     72
      AG_EXNAME = 'COMPONENT'                                             72
      AG_PARLIST = 'A_Z_W'                                                72
      CALL AXCOMPONENT                                                    72
C *                                                                       72
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  72
      CALL AGSRESET                                                       73
      AG_A = 16                                                           73
      AG_Z = 8                                                            73
      AG_W = 0.6*2*16./60.                                                73
      AG_TITLE = 'O'                                                      73
      AG_EXNAME = 'COMPONENT'                                             73
      AG_PARLIST = 'A_Z_W'                                                73
      CALL AXCOMPONENT                                                    73
C *                                                                       73
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 73
      CALL AGSRESET                                                       74
      AG_A = 12                                                           74
      AG_Z = 6                                                            74
      AG_W = 0.4*8*12./174.                                               74
      AG_TITLE = 'C'                                                      74
      AG_EXNAME = 'COMPONENT'                                             74
      AG_PARLIST = 'A_Z_W'                                                74
      CALL AXCOMPONENT                                                    74
C *                                                                       74
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  74
      CALL AGSRESET                                                       75
      AG_A = 1                                                            75
      AG_Z = 1                                                            75
      AG_W = 0.4*14*1./174.                                               75
      AG_TITLE = 'H'                                                      75
      AG_EXNAME = 'COMPONENT'                                             75
      AG_PARLIST = 'A_Z_W'                                                75
      CALL AXCOMPONENT                                                    75
C *                                                                       75
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 75
      CALL AGSRESET                                                       76
      AG_A = 16                                                           76
      AG_Z = 8                                                            76
      AG_W = 0.4*4*16./174.                                               76
      AG_TITLE = 'O'                                                      76
      AG_EXNAME = 'COMPONENT'                                             76
      AG_PARLIST = 'A_Z_W'                                                76
      CALL AXCOMPONENT                                                    76
C *                                                                       76
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  76
      CALL AGSRESET                                                       77
      AG_DENS = 1.7                                                       77
      AG_TITLE = 'G10'                                                    77
      AG_EXNAME = 'MIXTURE'                                               77
      AG_PARLIST = 'DENS'                                                 77
      CALL AXMIXTURE                                                      77
C *                                                                       77
*     mixture gas C20: Ar/CO2 80:20 by volume
C Geant COMPONENT : AR ===> A=40 Z=18 W=8 <===                            81
      CALL AGSRESET                                                       81
      AG_A = 40                                                           81
      AG_Z = 18                                                           81
      AG_W = 8                                                            81
      AG_TITLE = 'AR'                                                     81
      AG_EXNAME = 'COMPONENT'                                             81
      AG_PARLIST = 'A_Z_W'                                                81
      CALL AXCOMPONENT                                                    81
C *                                                                       81
C Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                              81
      CALL AGSRESET                                                       82
      AG_A = 12                                                           82
      AG_Z = 6                                                            82
      AG_W = 1                                                            82
      AG_TITLE = 'C'                                                      82
      AG_EXNAME = 'COMPONENT'                                             82
      AG_PARLIST = 'A_Z_W'                                                82
      CALL AXCOMPONENT                                                    82
C *                                                                       82
C Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                              82
      CALL AGSRESET                                                       83
      AG_A = 16                                                           83
      AG_Z = 8                                                            83
      AG_W = 2                                                            83
      AG_TITLE = 'O'                                                      83
      AG_EXNAME = 'COMPONENT'                                             83
      AG_PARLIST = 'A_Z_W'                                                83
      CALL AXCOMPONENT                                                    83
C *                                                                       83
C Geant MIXTURE : C20 ===> DENS=0.8*0.00166+0.2*0.001977 <===             83
      CALL AGSRESET                                                       84
      AG_DENS = 0.8*0.00166+0.2*0.001977                                  84
      AG_TITLE = 'C20'                                                    84
      AG_EXNAME = 'MIXTURE'                                               84
      AG_PARLIST = 'DENS'                                                 84
      CALL AXMIXTURE                                                      84
C *                                                                       84
*-----------------------------------------------------------------
C use default GMMG bank                                                   87
      BPATH_GMMG='GMMG*'                                                  87
      IDNUM_GMMG(1)=0                                                     87
      CALL AGDGETP('GEMBGEO','*',-999,'GMMG',' ',BANK_GMMG, BPATH_GMMG,   87
     *IDNUM_GMMG,1,MAP_GMMG,NAMES_GMMG,BANK_GMMG,LEN_GMMG,LLEN_GMMG,      87
     *LINK_GMMG,FLAG_GMMG)                                                87
************************* MOTHER ************************
      CALL AGSBEGM(MODULE,IPRIN)                                          91
C Check IPRIN<0                                                           91
      IF (IPRIN.LT.0) RETURN                                              91
C Create GMBO :                                                           91
C Call To Local Procedure GMBO                                            91
      ASSIGN 5020 TO BLOCK_GMBO                                           91
      GO TO  5010                                                         91
5020  CONTINUE                                                            91
C *                                                                       91
C Geant POSITION : GMBO ===> MOTHER='CAVE' <===                           92
      CALL AGSRESET                                                       92
      AG_MOTHER = 'CAVE'                                                  92
      AG_TITLE = 'GMBO'                                                   92
      AG_EXNAME = 'POSITION'                                              92
      AG_PARLIST = 'MOTHER'                                               92
      CALL AXPOSITION                                                     92
C *                                                                       92
*-----------------------------------------------------------------
      CALL AGSENDM                                                        96
      RETURN                                                              96
C Begin Local Procedure GMBO                                              96
C GMBO IS THE MOTHER OF THE GEMB DETECTOR                                 96
5010  CONTINUE                                                            96
         AG_TITLE='GMBO'                                                  96
         AG_MODULE=MODULE                                                 96
         AG_MARK='V00'                                                    96
         CALL AGSPUSH                                                     96
         AG_MODULE=MODULE                                                 96
C    *                                                                    96
C    Geant MATERIAL : AIR ===> inherited arguments used <===              97
         CALL AGSRESET                                                    97
         AG_TITLE = 'AIR'                                                 97
         AG_EXNAME = 'MATERIAL'                                           97
         AG_PARLIST = 'NONE'                                              97
         CALL AXMATERIAL                                                  97
C    *                                                                    97
C    Geant ATTRIBUTE : GMBO ===> SEEN=0 COLO=6 <===                       97
         CALL AGSRESET                                                    98
         AG_SEEN = 0                                                      98
         AG_COLO = 6                                                      98
         AG_TITLE = 'GMBO'                                                98
         AG_EXNAME = 'ATTRIBUTE'                                          98
         AG_PARLIST = 'SEEN_COLO'                                         98
         CALL AXATTRIBUTE                                                 98
C    *                                                                    98
C    Geant SHAPE : TUBE ===> RMIN=GMMG_RIN RMAX=GMMG_ROUT DZ = GMMG_TOTA  102
C    LLENGTH/2.0 <===                                                     102
         CALL AGSRESET                                                    102
         AG_RMIN = GMMG_RIN                                               102
         AG_RMAX = GMMG_ROUT                                              102
         AG_DZ = GMMG_TOTALLENGTH/2.0                                     102
         AG_TITLE = 'TUBE'                                                102
         AG_EXNAME = 'SHAPE'                                              102
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      102
         CALL AXSHAPE                                                     102
C    *                                                                    102
C    use GEMV bank                                                        104
         BPATH_GEMV='GEMV*'                                               104
         IDNUM_GEMV(1)=0                                                  104
         GEMV_LAYER=1                                                     104
         CALL AGDGETP('GEMBGEO','*',-999,'GEMV',NGEMV_LAYER,              104
     *   RE_GEMV_LAYER,BPATH_GEMV,IDNUM_GEMV,1,MAP_GEMV,NAMES_GEMV,       104
     *   BANK_GEMV,LEN_GEMV,LLEN_GEMV,LINK_GEMV,FLAG_GEMV)                104
C    *                                                                    105
         N=1                                                              105
            GO TO 5033                                                    105
5031        N=N+1                                                         105
5033        IF(N-(GEMV_NZ).GT.0) GO TO 5032                               105
C       *                                                                 106
            K=1                                                           106
               GO TO 5043                                                 106
5041           K=K+1                                                      106
5043           IF(K-(GEMV_NSD).GT.0) GO TO 5042                           106
               UNGL = (K-1)*GEMV_PHI                                      108
               ANGL = (90+UNGL)*DEGRAD                                    109
C          Create GMVI :                                                  115
C          Call To Local Procedure GMVI                                   115
               ASSIGN 5060 TO BLOCK_GMVI                                  115
               GO TO  5050                                                115
5060           CONTINUE                                                   115
C          *                                                              115
C          Geant POSITION : GMVI ===> MOTHER='GMBO' X=GEMV_RPOS*COS(ANGL  115
C          ), Y=GEMV_RPOS*SIN(ANGL), Z=(-(GEMV_NZ-1)+2.*(N-1))*(GEMV_DZ+  115
C          2.*GEMV_FRTH), THETAX=90, THETAY=90, THETAZ=0, PHIX=UNGL+GEMV  115
C          _ALPHA, PHIY=90+UNGL+GEMV_ALPHA, PHIZ=0 <===                   115
               CALL AGSRESET                                              115
               AG_MOTHER = 'GMBO'                                         115
               AG_X = GEMV_RPOS*COS(ANGL)                                 115
               AG_Y = GEMV_RPOS*SIN(ANGL)                                 115
               AG_Z = (-(GEMV_NZ-1)+2.*(N-1))*(GEMV_DZ+2.*GEMV_FRTH)      115
               AG_THETAX = 90                                             115
               AG_THETAY = 90                                             115
               AG_THETAZ = 0                                              115
               AG_PHIX = UNGL+GEMV_ALPHA                                  115
               AG_PHIY = 90+UNGL+GEMV_ALPHA                               115
               AG_PHIZ = 0                                                115
               AG_TITLE = 'GMVI'                                          115
               AG_EXNAME = 'POSITION'                                     115
               AG_PARLIST = 'MOTHER_X_Y_Z_THETAX_THETAY_THETAZ_PHIX_PHIY  115
     *_PHIZ'                                                              115
               CALL AXPOSITION                                            115
C          *                                                              115
            GO TO 5041                                                    116
5042        CONTINUE                                                      116
         GO TO 5031                                                       117
5032     CONTINUE                                                         117
C    use GEMV bank                                                        120
         BPATH_GEMV='GEMV*'                                               120
         IDNUM_GEMV(1)=0                                                  120
         GEMV_LAYER=2                                                     120
         CALL AGDGETP('GEMBGEO','*',-999,'GEMV',NGEMV_LAYER,              120
     *   RE_GEMV_LAYER,BPATH_GEMV,IDNUM_GEMV,1,MAP_GEMV,NAMES_GEMV,       120
     *   BANK_GEMV,LEN_GEMV,LLEN_GEMV,LINK_GEMV,FLAG_GEMV)                120
C    *                                                                    121
         N=1                                                              121
            GO TO 5073                                                    121
5071        N=N+1                                                         121
5073        IF(N-(GEMV_NZ).GT.0) GO TO 5072                               121
C       *                                                                 122
            K=1                                                           122
               GO TO 5083                                                 122
5081           K=K+1                                                      122
5083           IF(K-(GEMV_NSD).GT.0) GO TO 5082                           122
               UNGL = (K-1)*GEMV_PHI                                      124
               ANGL = (90+UNGL)*DEGRAD                                    125
C          Create GMVI :                                                  131
C          Call To Local Procedure GMVI                                   131
               ASSIGN 5090 TO BLOCK_GMVI                                  131
               GO TO  5050                                                131
5090           CONTINUE                                                   131
C          *                                                              131
C          Geant POSITION : GMVI ===> MOTHER='GMBO' X=GEMV_RPOS*COS(ANGL  131
C          ), Y=GEMV_RPOS*SIN(ANGL), Z=(-(GEMV_NZ-1)+2.*(N-1))*(GEMV_DZ+  131
C          2.*GEMV_FRTH), THETAX=90, THETAY=90, THETAZ=0, PHIX=UNGL+GEMV  131
C          _ALPHA, PHIY=90+UNGL+GEMV_ALPHA, PHIZ=0 <===                   131
               CALL AGSRESET                                              131
               AG_MOTHER = 'GMBO'                                         131
               AG_X = GEMV_RPOS*COS(ANGL)                                 131
               AG_Y = GEMV_RPOS*SIN(ANGL)                                 131
               AG_Z = (-(GEMV_NZ-1)+2.*(N-1))*(GEMV_DZ+2.*GEMV_FRTH)      131
               AG_THETAX = 90                                             131
               AG_THETAY = 90                                             131
               AG_THETAZ = 0                                              131
               AG_PHIX = UNGL+GEMV_ALPHA                                  131
               AG_PHIY = 90+UNGL+GEMV_ALPHA                               131
               AG_PHIZ = 0                                                131
               AG_TITLE = 'GMVI'                                          131
               AG_EXNAME = 'POSITION'                                     131
               AG_PARLIST = 'MOTHER_X_Y_Z_THETAX_THETAY_THETAZ_PHIX_PHIY  131
     *_PHIZ'                                                              131
               CALL AXPOSITION                                            131
C          *                                                              131
            GO TO 5081                                                    132
5082        CONTINUE                                                      132
         GO TO 5071                                                       133
5072     CONTINUE                                                         133
5011  CALL AGSPOPB                                                        135
      GO TO BLOCK_GMBO, (5010 ,5020)                                      135
C End of Local Procedure                                                  135
*-----------------------------------------------------------------
C Begin Local Procedure GMVI                                              137
C GMVI IS A GEM STRIP DETECTOR, LAYER 1                                   137
5050  CONTINUE                                                            137
         AG_TITLE='GMVI'                                                  137
         AG_MODULE=MODULE                                                 137
         AG_MARK='V00'                                                    137
         CALL AGSPUSH                                                     137
         AG_MODULE=MODULE                                                 137
C    *                                                                    137
C    Geant MATERIAL : G10 ===> inherited arguments used <===              139
         CALL AGSRESET                                                    139
         AG_TITLE = 'G10'                                                 139
         AG_EXNAME = 'MATERIAL'                                           139
         AG_PARLIST = 'NONE'                                              139
         CALL AXMATERIAL                                                  139
C    *                                                                    139
C    Geant ATTRIBUTE : GMVI ===> SEEN=1 COLO=2 <===                       139
         CALL AGSRESET                                                    140
         AG_SEEN = 1                                                      140
         AG_COLO = 2                                                      140
         AG_TITLE = 'GMVI'                                                140
         AG_EXNAME = 'ATTRIBUTE'                                          140
         AG_PARLIST = 'SEEN_COLO'                                         140
         CALL AXATTRIBUTE                                                 140
C    *                                                                    140
C    Geant SHAPE : BOX ===> DX=GEMV_DX+GEMV_FRTH DY=GEMV_DY DZ=GEMV_DZ+G  142
C    EMV_FRTH <===                                                        142
         CALL AGSRESET                                                    142
         AG_DX = GEMV_DX+GEMV_FRTH                                        142
         AG_DY = GEMV_DY                                                  142
         AG_DZ = GEMV_DZ+GEMV_FRTH                                        142
         AG_TITLE = 'BOX'                                                 142
         AG_EXNAME = 'SHAPE'                                              142
         AG_PARLIST = 'DX_DY_DZ'                                          142
         CALL AXSHAPE                                                     142
C    *                                                                    142
C    Create GMMI :                                                        144
C    Call To Local Procedure GMMI                                         144
         ASSIGN 5110 TO BLOCK_GMMI                                        144
         GO TO  5100                                                      144
5110     CONTINUE                                                         144
C    *                                                                    144
C    Geant POSITION : GMMI ===> X=0. Y=-GEMV_RDTH/2. Z=0. <===            144
         CALL AGSRESET                                                    144
         AG_X = 0.                                                        144
         AG_Y = -GEMV_RDTH/2.                                             144
         AG_Z = 0.                                                        144
         AG_TITLE = 'GMMI'                                                144
         AG_EXNAME = 'POSITION'                                           144
         AG_PARLIST = 'X_Y_Z'                                             144
         CALL AXPOSITION                                                  144
C    *                                                                    144
5051  CALL AGSPOPB                                                        146
      GO TO BLOCK_GMVI, (5050 ,5060,5090)                                 146
C End of Local Procedure                                                  146
*-----------------------------------------------------------------
C Begin Local Procedure GMMI                                              148
C GMMI IS A GEM STRIP DETECTOR, MYLAR STAFF, LAYER 1                      148
5100  CONTINUE                                                            148
         AG_TITLE='GMMI'                                                  148
         AG_MODULE=MODULE                                                 148
         AG_MARK='V00'                                                    148
         CALL AGSPUSH                                                     148
         AG_MODULE=MODULE                                                 148
C    *                                                                    148
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            150
         CALL AGSRESET                                                    150
         AG_TITLE = 'MYLAR'                                               150
         AG_EXNAME = 'MATERIAL'                                           150
         AG_PARLIST = 'NONE'                                              150
         CALL AXMATERIAL                                                  150
C    *                                                                    150
C    Geant ATTRIBUTE : GMMI ===> SEEN=1 COLO=3 <===                       150
         CALL AGSRESET                                                    151
         AG_SEEN = 1                                                      151
         AG_COLO = 3                                                      151
         AG_TITLE = 'GMMI'                                                151
         AG_EXNAME = 'ATTRIBUTE'                                          151
         AG_PARLIST = 'SEEN_COLO'                                         151
         CALL AXATTRIBUTE                                                 151
C    *                                                                    151
C    Geant SHAPE : BOX ===> DX=GEMV_DX DY=GEMV_DY-GEMV_RDTH/2. DZ=GEMV_D  153
C    Z <===                                                               153
         CALL AGSRESET                                                    153
         AG_DX = GEMV_DX                                                  153
         AG_DY = GEMV_DY-GEMV_RDTH/2.                                     153
         AG_DZ = GEMV_DZ                                                  153
         AG_TITLE = 'BOX'                                                 153
         AG_EXNAME = 'SHAPE'                                              153
         AG_PARLIST = 'DX_DY_DZ'                                          153
         CALL AXSHAPE                                                     153
C    *                                                                    153
C    Create GMGI :                                                        155
C    Call To Local Procedure GMGI                                         155
         ASSIGN 5130 TO BLOCK_GMGI                                        155
         GO TO  5120                                                      155
5130     CONTINUE                                                         155
C    *                                                                    155
C    Geant POSITION : GMGI ===> X=0. Y=0. Z=0. <===                       155
         CALL AGSRESET                                                    155
         AG_X = 0.                                                        155
         AG_Y = 0.                                                        155
         AG_Z = 0.                                                        155
         AG_TITLE = 'GMGI'                                                155
         AG_EXNAME = 'POSITION'                                           155
         AG_PARLIST = 'X_Y_Z'                                             155
         CALL AXPOSITION                                                  155
C    *                                                                    155
5101  CALL AGSPOPB                                                        157
      GO TO BLOCK_GMMI, (5100 ,5110)                                      157
C End of Local Procedure                                                  157
*-----------------------------------------------------------------
C Begin Local Procedure GMGI                                              159
C GMGI IS A GEM STRIP DETECTOR FILLED WITH GAS, LAYER 1                   159
5120  CONTINUE                                                            159
         AG_TITLE='GMGI'                                                  159
         AG_MODULE=MODULE                                                 159
         AG_MARK='V00'                                                    159
         CALL AGSPUSH                                                     159
         AG_MODULE=MODULE                                                 159
C    *                                                                    159
C    Geant MATERIAL : C20 ===> inherited arguments used <===              161
         CALL AGSRESET                                                    161
         AG_TITLE = 'C20'                                                 161
         AG_EXNAME = 'MATERIAL'                                           161
         AG_PARLIST = 'NONE'                                              161
         CALL AXMATERIAL                                                  161
C    *                                                                    161
C    Geant ATTRIBUTE : GMGI ===> SEEN=1 COLO=3 <===                       161
         CALL AGSRESET                                                    162
         AG_SEEN = 1                                                      162
         AG_COLO = 3                                                      162
         AG_TITLE = 'GMGI'                                                162
         AG_EXNAME = 'ATTRIBUTE'                                          162
         AG_PARLIST = 'SEEN_COLO'                                         162
         CALL AXATTRIBUTE                                                 162
C    *                                                                    162
C    Geant SHAPE : BOX ===> DX=GEMV_DX DY=GEMV_DY-GEMV_RDTH/2.-GEMV_MLRT  164
C    H DZ=GEMV_DZ <===                                                    164
         CALL AGSRESET                                                    164
         AG_DX = GEMV_DX                                                  164
         AG_DY = GEMV_DY-GEMV_RDTH/2.-GEMV_MLRTH                          164
         AG_DZ = GEMV_DZ                                                  164
         AG_TITLE = 'BOX'                                                 164
         AG_EXNAME = 'SHAPE'                                              164
         AG_PARLIST = 'DX_DY_DZ'                                          164
         CALL AXSHAPE                                                     164
C    *                                                                    164
C    Create GMDI :                                                        166
C    Call To Local Procedure GMDI                                         166
         ASSIGN 5150 TO BLOCK_GMDI                                        166
         GO TO  5140                                                      166
5150     CONTINUE                                                         166
C    *                                                                    166
C    Geant POSITION : GMDI ===> X=0. Y=0. Z=0. <===                       166
         CALL AGSRESET                                                    166
         AG_X = 0.                                                        166
         AG_Y = 0.                                                        166
         AG_Z = 0.                                                        166
         AG_TITLE = 'GMDI'                                                166
         AG_EXNAME = 'POSITION'                                           166
         AG_PARLIST = 'X_Y_Z'                                             166
         CALL AXPOSITION                                                  166
C    *                                                                    166
5121  CALL AGSPOPB                                                        168
      GO TO BLOCK_GMGI, (5120 ,5130)                                      168
C End of Local Procedure                                                  168
*-----------------------------------------------------------------
C Begin Local Procedure GMDI                                              170
C GMDI IS THE SENSITIVE VOLUME OF THE GEM STRIP DETECTOR, LAYER 1         170
5140  CONTINUE                                                            170
         AG_TITLE='GMDI'                                                  170
         AG_MODULE=MODULE                                                 170
         AG_MARK='V00'                                                    170
         CALL AGSPUSH                                                     170
         AG_MODULE=MODULE                                                 170
C    *                                                                    170
C    Geant MATERIAL : C20 ===> inherited arguments used <===              172
         CALL AGSRESET                                                    172
         AG_TITLE = 'C20'                                                 172
         AG_EXNAME = 'MATERIAL'                                           172
         AG_PARLIST = 'NONE'                                              172
         CALL AXMATERIAL                                                  172
C    *                                                                    172
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         172
         CALL AGSRESET                                                    173
         AG_ISVOL = 1                                                     173
         AG_TITLE = 'SENSITIVE'                                           173
         AG_EXNAME = 'MATERIAL'                                           173
         AG_PARLIST = 'ISVOL'                                             173
         CALL AXMATERIAL                                                  173
C    *                                                                    173
C    Geant ATTRIBUTE : GMDI ===> SEEN=1 COLO=4 <===                       173
         CALL AGSRESET                                                    174
         AG_SEEN = 1                                                      174
         AG_COLO = 4                                                      174
         AG_TITLE = 'GMDI'                                                174
         AG_EXNAME = 'ATTRIBUTE'                                          174
         AG_PARLIST = 'SEEN_COLO'                                         174
         CALL AXATTRIBUTE                                                 174
C    *                                                                    174
C    Geant SHAPE : BOX ===> DX=GEMV_DX DY=GEMV_SGASTH/2.0 DZ = GEMV_DZ <  178
C    ===                                                                  178
         CALL AGSRESET                                                    178
         AG_DX = GEMV_DX                                                  178
         AG_DY = GEMV_SGASTH/2.0                                          178
         AG_DZ = GEMV_DZ                                                  178
         AG_TITLE = 'BOX'                                                 178
         AG_EXNAME = 'SHAPE'                                              178
         AG_PARLIST = 'DX_DY_DZ'                                          178
         CALL AXSHAPE                                                     178
C    *                                                                    178
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  183
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  183
C    ,0.001)                                                              183
         AG_TITLE = 'GMDI'                                                183
         AG_EXNAME = 'HITS'                                               183
         AG_TYPE=AG_MAGIC                                                 183
         CALL AGSDETV('GMDI')                                             183
C    *                                                                    183
         AG_PARLIST='Z'                                                   183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = AG_MAGIC                                              183
         AG_BIN=.001                                                      183
         AG_OPTION='S'                                                    183
         AG_FHMIN = AG_MAGIC                                              183
         AG_FHMAX = AG_MAGIC                                              183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='Y'                                                   183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = AG_MAGIC                                              183
         AG_BIN=.001                                                      183
         AG_FHMIN=AG_MAGIC                                                183
         AG_FHMAX=AG_MAGIC                                                183
         AG_OPTION='*'                                                    183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='X'                                                   183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = AG_MAGIC                                              183
         AG_BIN=.001                                                      183
         AG_FHMIN=AG_MAGIC                                                183
         AG_FHMAX=AG_MAGIC                                                183
         AG_OPTION='*'                                                    183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='PTOT'                                                183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 16                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_OPTION='*'                                                    183
         AG_FHMIN = 0                                                     183
         AG_FHMAX = 100                                                   183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='CX'                                                  183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 10                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_FHMIN=AG_MAGIC                                                183
         AG_FHMAX=AG_MAGIC                                                183
         AG_OPTION='*'                                                    183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='CY'                                                  183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 10                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_FHMIN=AG_MAGIC                                                183
         AG_FHMAX=AG_MAGIC                                                183
         AG_OPTION='*'                                                    183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='CZ'                                                  183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 10                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_FHMIN=AG_MAGIC                                                183
         AG_FHMAX=AG_MAGIC                                                183
         AG_OPTION='*'                                                    183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='SLENG'                                               183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 16                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_OPTION='*'                                                    183
         AG_FHMIN = 0                                                     183
         AG_FHMAX = 500                                                   183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='TOF'                                                 183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 16                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_OPTION='*'                                                    183
         AG_FHMIN = 0                                                     183
         AG_FHMAX = 1.E-6                                                 183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='STEP'                                                183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = AG_MAGIC                                              183
         AG_BIN=.01                                                       183
         AG_FHMIN=AG_MAGIC                                                183
         AG_FHMAX=AG_MAGIC                                                183
         AG_OPTION='*'                                                    183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         AG_PARLIST='ELOSS'                                               183
         AG_TITLE = 'DETH'                                                183
         AG_NBITS = 16                                                    183
         AG_BIN=AG_MAGIC                                                  183
         AG_OPTION='*'                                                    183
         AG_FHMIN = 0                                                     183
         AG_FHMAX = 0.001                                                 183
         CALL AGSDETH (0)                                                 183
C    *                                                                    183
         CALL AGSDETU('GMDI')                                             183
C    *                                                                    183
C    HITS GMDI ZZ:.00001:S YY:.00001: XX:.00001: ETOT:16:(0,100), !PX:16  188
C    :(-60,60) PY:16:(-60,60) PZ:16:(-60,60), !Z:.00001: Y:.00001: X:.00  188
C    001:, !ELOSS:32:(0,0.01) STEP:16:(0,1.)                              188
5141  CALL AGSPOPB                                                        190
      GO TO BLOCK_GMDI, (5140 ,5150)                                      190
C End of Local Procedure                                                  190
*------------------------------------------------------------------------------
      END                                                                 192
*------------------------------------------------------------------------------
