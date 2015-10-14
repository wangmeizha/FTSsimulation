C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: richgeo.g,v 1.12 2001/04/09 15:31:37 nevski Exp $
*
* $Log: richgeo.g,v $
* Revision 1.12  2001/04/09 15:31:37  nevski
* second version of cerenkov light properties introduced
*
* Revision 1.11  2001/03/13 23:33:42  nevski
* quartz absco corrected back
*
* Revision 1.10  2001/03/13 20:56:33  nevski
* variable rich position taken from DB
*
* Revision 1.9  2000/02/22 20:34:14  nevski
* LGAM added to HITs definition
*
* Revision 1.8  2000/01/28 21:53:37  nevsky
* secondaries in Rich can be discarded
*
* Revision 2.0  2000/10/12 kunde
*
***************************************************************************
      SUBROUTINE RICHGEO                                                  22
C RICHGEO DEFINES RING IMAGE CERENKOV GEOMETRY                            22
      IMPLICIT NONE                                                       22
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       22
      DATA MODULE/'RICHGEO'/, BANK_TITLE/'*'/                             22
      DATA MCOMMENT/'DEFINES RING IMAGE CERENKOV GEOMETRY'/               22
      LOGICAL FIRST                                                       22
      DATA FIRST/.TRUE./                                                  22
      SAVE FIRST                                                          22
      INTEGER JUMPAD                                                      22
      EXTERNAL JUMPAD                                                     22
      INTEGER OK                                                          22
      PARAMETER (OK=0)                                                    22
      INTEGER AG_I,AG_J                                                   22
      SAVE AG_I,AG_J                                                      22
C AUTHOR GERD KUNDE                                                       22
      DATA AG_I/0/,AG_J/0/                                                23
      CHARACTER*40 AUTHOR /'GERD KUNDE'/                                  23
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  23
C CREATED JUNE 1, 1998 SUPPOSELY                                          23
      CHARACTER*40 CREATED /'JUNE 1, 1998 SUPPOSELY'/                     24
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  24
***************************************************************************
*  medium vs material table at my understanding:
*       air   honey  quarz  freon methan   csi    gri   opaco   Gap    Al
*  med: 101    102    103    104    105    106    107    108    109    110
*  mat: 101    106    120    130    140    106    111    121    140    150
*       air     C    SiO2   C6F14   CH4     C     Cu    SiO2    CH4    Al
*             Radl?                       Radl?
C - Advanced Geant interface                                              33
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      33
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       33
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         33
     *AG_OPTION,AG_ORT,AG_MARK                                            33
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         33
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     33
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     33
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    33
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          33
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   33
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     33
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        33
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   33
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 33
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     33
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       33
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        33
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       33
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       33
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       33
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         33
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     33
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       33
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   33
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       33
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     33
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          33
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         33
     *-696969,AG_LDETU=250)                                               33
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    33
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  33
     *AG_IMFLD,AG_ISIMU                                                   33
C Inherited variables a: saved during internal calls                      33
C b: reset at the entry in a block and then saved                         33
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        33
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       33
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         33
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     33
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       33
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         33
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           33
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        33
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        33
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            33
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           33
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    33
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      33
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           33
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          33
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                33
C local variables valid inside same block                                 33
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       33
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  33
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         33
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    33
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          33
     *AG_OPTION                                                           33
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        33
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             33
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              33
C temporarly                                                              33
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           33
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    33
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       33
     *AG_HY)                                                              33
      REAL MIL_P,INCH_P                                                   33
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               33
C - GEANT description of logical units                                    33
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            33
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      33
      COMMON/GCMAIL/ CHMAIL                                               33
      CHARACTER*132 CHMAIL                                                33
C - Natural constants (or considered as such)                             33
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   33
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        33
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       33
      INTEGER BLOCK_RICH                                                  36
      INTEGER BLOCK_SRIC                                                  36
      INTEGER BLOCK_ALUM                                                  36
      INTEGER BLOCK_HONE                                                  36
      INTEGER BLOCK_OQUA                                                  36
      INTEGER BLOCK_OQUF                                                  36
      INTEGER BLOCK_QUAR                                                  36
      INTEGER BLOCK_RGAP                                                  36
      INTEGER BLOCK_RCSI                                                  36
      INTEGER BLOCK_BARR                                                  36
      INTEGER BLOCK_SPAC                                                  36
      INTEGER BLOCK_FREO                                                  36
      INTEGER BLOCK_ALMF                                                  36
      INTEGER BLOCK_BORD                                                  36
      INTEGER BLOCK_HOLE                                                  36
C -                                                                       38
      INTEGER RICH_VERSION                                                38
      EQUIVALENCE (BANK_RICH(3),RICH_VERSION,RE_RICH_VERSION),            38
     *(NAMES_RICH(1),NRICH_VERSION)                                       38
      EQUIVALENCE (COMMEN_RICH(1),CRICH_VERSION),(MAP_RICH(1,1),          38
     *MRICH_VERSION)                                                      38
      CHARACTER CRICH_VERSION*40/' '/                                     38
      CHARACTER NRICH_VERSION*12/'I.VERSION'/                             38
      INTEGER MRICH_VERSION(2)/1,1/                                       38
      REAL RE_RICH_VERSION                                                38
C -                                                                       38
      INTEGER RICH_POSITION                                               38
      EQUIVALENCE (BANK_RICH(4),RICH_POSITION,RE_RICH_POSITION),          38
     *(NAMES_RICH(2),NRICH_POSITION)                                      38
      EQUIVALENCE (COMMEN_RICH(2),CRICH_POSITION),(MAP_RICH(1,2),         38
     *MRICH_POSITION)                                                     38
      CHARACTER CRICH_POSITION*40/' '/                                    38
      CHARACTER NRICH_POSITION*12/'I.POSITION'/                           38
      INTEGER MRICH_POSITION(2)/1,1/                                      38
      REAL RE_RICH_POSITION                                               38
C -                                                                       38
      INTEGER RICH_CVERSION                                               38
      EQUIVALENCE (BANK_RICH(5),RICH_CVERSION,RE_RICH_CVERSION),          38
     *(NAMES_RICH(3),NRICH_CVERSION)                                      38
      EQUIVALENCE (COMMEN_RICH(3),CRICH_CVERSION),(MAP_RICH(1,3),         38
     *MRICH_CVERSION)                                                     38
      CHARACTER CRICH_CVERSION*40/' '/                                    38
      CHARACTER NRICH_CVERSION*12/'I.CVERSION'/                           38
      INTEGER MRICH_CVERSION(2)/1,1/                                      38
      REAL RE_RICH_CVERSION                                               38
C -                                                                       38
      REAL RICH_DX                                                        38
      EQUIVALENCE (BANK_RICH(6),RICH_DX,RE_RICH_DX),(NAMES_RICH(4),       38
     *NRICH_DX)                                                           38
      EQUIVALENCE (COMMEN_RICH(4),CRICH_DX),(MAP_RICH(1,4),MRICH_DX)      38
      CHARACTER CRICH_DX*40/' '/                                          38
      CHARACTER NRICH_DX*12/'R.DX'/                                       38
      INTEGER MRICH_DX(2)/1,1/                                            38
      REAL RE_RICH_DX                                                     38
C -                                                                       38
      REAL RICH_DY                                                        38
      EQUIVALENCE (BANK_RICH(7),RICH_DY,RE_RICH_DY),(NAMES_RICH(5),       38
     *NRICH_DY)                                                           38
      EQUIVALENCE (COMMEN_RICH(5),CRICH_DY),(MAP_RICH(1,5),MRICH_DY)      38
      CHARACTER CRICH_DY*40/' '/                                          38
      CHARACTER NRICH_DY*12/'R.DY'/                                       38
      INTEGER MRICH_DY(2)/1,1/                                            38
      REAL RE_RICH_DY                                                     38
C -                                                                       38
      REAL RICH_DZ                                                        38
      EQUIVALENCE (BANK_RICH(8),RICH_DZ,RE_RICH_DZ),(NAMES_RICH(6),       38
     *NRICH_DZ)                                                           38
      EQUIVALENCE (COMMEN_RICH(6),CRICH_DZ),(MAP_RICH(1,6),MRICH_DZ)      38
      CHARACTER CRICH_DZ*40/' '/                                          38
      CHARACTER NRICH_DZ*12/'R.DZ'/                                       38
      INTEGER MRICH_DZ(2)/1,1/                                            38
      REAL RE_RICH_DZ                                                     38
C -                                                                       38
      INTEGER LEN_RICH,LLEN_RICH,LINK_RICH                                38
      PARAMETER (LEN_RICH=8)                                              38
      PARAMETER (LLEN_RICH=6)                                             38
      INTEGER FLAG_RICH/0/,MAP_RICH(2,LLEN_RICH),IDNUM_RICH(10)           38
      CHARACTER NAMES_RICH(LLEN_RICH)*12,COMMEN_RICH(LLEN_RICH)*40        38
      DATA IDNUM_RICH/10*0/                                               38
      REAL BANK_RICH(LEN_RICH)                                            38
      CHARACTER*60 BPATH_RICH/' '/                                        38
      SAVE FLAG_RICH,MAP_RICH,IDNUM_RICH,NAMES_RICH,COMMEN_RICH,          38
     *BPATH_RICH                                                          38
      DATA LINK_RICH/0/,BANK_RICH/LEN_RICH*0/                             38
C *                                                                       38
C -                                                                       39
      INTEGER RPOS_POSITION                                               39
      EQUIVALENCE (BANK_RPOS(3),RPOS_POSITION,RE_RPOS_POSITION),          39
     *(NAMES_RPOS(1),NRPOS_POSITION)                                      39
      EQUIVALENCE (COMMEN_RPOS(1),CRPOS_POSITION),(MAP_RPOS(1,1),         39
     *MRPOS_POSITION)                                                     39
      CHARACTER CRPOS_POSITION*40/' '/                                    39
      CHARACTER NRPOS_POSITION*12/'I.POSITION'/                           39
      INTEGER MRPOS_POSITION(2)/1,1/                                      39
      REAL RE_RPOS_POSITION                                               39
C -                                                                       39
      REAL RPOS_PHI                                                       39
      EQUIVALENCE (BANK_RPOS(4),RPOS_PHI,RE_RPOS_PHI),(NAMES_RPOS(2),     39
     *NRPOS_PHI)                                                          39
      EQUIVALENCE (COMMEN_RPOS(2),CRPOS_PHI),(MAP_RPOS(1,2),MRPOS_PHI)    39
      CHARACTER CRPOS_PHI*40/' '/                                         39
      CHARACTER NRPOS_PHI*12/'R.PHI'/                                     39
      INTEGER MRPOS_PHI(2)/1,1/                                           39
      REAL RE_RPOS_PHI                                                    39
C -                                                                       39
      REAL RPOS_RPOS                                                      39
      EQUIVALENCE (BANK_RPOS(5),RPOS_RPOS,RE_RPOS_RPOS),(NAMES_RPOS(3),   39
     *NRPOS_RPOS)                                                         39
      EQUIVALENCE (COMMEN_RPOS(3),CRPOS_RPOS),(MAP_RPOS(1,3),             39
     *MRPOS_RPOS)                                                         39
      CHARACTER CRPOS_RPOS*40/' '/                                        39
      CHARACTER NRPOS_RPOS*12/'R.RPOS'/                                   39
      INTEGER MRPOS_RPOS(2)/1,1/                                          39
      REAL RE_RPOS_RPOS                                                   39
C -                                                                       39
      REAL RPOS_YPOS                                                      39
      EQUIVALENCE (BANK_RPOS(6),RPOS_YPOS,RE_RPOS_YPOS),(NAMES_RPOS(4),   39
     *NRPOS_YPOS)                                                         39
      EQUIVALENCE (COMMEN_RPOS(4),CRPOS_YPOS),(MAP_RPOS(1,4),             39
     *MRPOS_YPOS)                                                         39
      CHARACTER CRPOS_YPOS*40/' '/                                        39
      CHARACTER NRPOS_YPOS*12/'R.YPOS'/                                   39
      INTEGER MRPOS_YPOS(2)/1,1/                                          39
      REAL RE_RPOS_YPOS                                                   39
C -                                                                       39
      REAL RPOS_ZPOS                                                      39
      EQUIVALENCE (BANK_RPOS(7),RPOS_ZPOS,RE_RPOS_ZPOS),(NAMES_RPOS(5),   39
     *NRPOS_ZPOS)                                                         39
      EQUIVALENCE (COMMEN_RPOS(5),CRPOS_ZPOS),(MAP_RPOS(1,5),             39
     *MRPOS_ZPOS)                                                         39
      CHARACTER CRPOS_ZPOS*40/' '/                                        39
      CHARACTER NRPOS_ZPOS*12/'R.ZPOS'/                                   39
      INTEGER MRPOS_ZPOS(2)/1,1/                                          39
      REAL RE_RPOS_ZPOS                                                   39
C -                                                                       39
      REAL RPOS_TX                                                        39
      EQUIVALENCE (BANK_RPOS(8),RPOS_TX,RE_RPOS_TX),(NAMES_RPOS(6),       39
     *NRPOS_TX)                                                           39
      EQUIVALENCE (COMMEN_RPOS(6),CRPOS_TX),(MAP_RPOS(1,6),MRPOS_TX)      39
      CHARACTER CRPOS_TX*40/' '/                                          39
      CHARACTER NRPOS_TX*12/'R.TX'/                                       39
      INTEGER MRPOS_TX(2)/1,1/                                            39
      REAL RE_RPOS_TX                                                     39
C -                                                                       39
      REAL RPOS_TY                                                        39
      EQUIVALENCE (BANK_RPOS(9),RPOS_TY,RE_RPOS_TY),(NAMES_RPOS(7),       39
     *NRPOS_TY)                                                           39
      EQUIVALENCE (COMMEN_RPOS(7),CRPOS_TY),(MAP_RPOS(1,7),MRPOS_TY)      39
      CHARACTER CRPOS_TY*40/' '/                                          39
      CHARACTER NRPOS_TY*12/'R.TY'/                                       39
      INTEGER MRPOS_TY(2)/1,1/                                            39
      REAL RE_RPOS_TY                                                     39
C -                                                                       39
      REAL RPOS_TZ                                                        39
      EQUIVALENCE (BANK_RPOS(10),RPOS_TZ,RE_RPOS_TZ),(NAMES_RPOS(8),      39
     *NRPOS_TZ)                                                           39
      EQUIVALENCE (COMMEN_RPOS(8),CRPOS_TZ),(MAP_RPOS(1,8),MRPOS_TZ)      39
      CHARACTER CRPOS_TZ*40/' '/                                          39
      CHARACTER NRPOS_TZ*12/'R.TZ'/                                       39
      INTEGER MRPOS_TZ(2)/1,1/                                            39
      REAL RE_RPOS_TZ                                                     39
C -                                                                       39
      INTEGER LEN_RPOS,LLEN_RPOS,LINK_RPOS                                39
      PARAMETER (LEN_RPOS=10)                                             39
      PARAMETER (LLEN_RPOS=8)                                             39
      INTEGER FLAG_RPOS/0/,MAP_RPOS(2,LLEN_RPOS),IDNUM_RPOS(10)           39
      CHARACTER NAMES_RPOS(LLEN_RPOS)*12,COMMEN_RPOS(LLEN_RPOS)*40        39
      DATA IDNUM_RPOS/10*0/                                               39
      REAL BANK_RPOS(LEN_RPOS)                                            39
      CHARACTER*60 BPATH_RPOS/' '/                                        39
      SAVE FLAG_RPOS,MAP_RPOS,IDNUM_RPOS,NAMES_RPOS,COMMEN_RPOS,          39
     *BPATH_RPOS                                                          39
      DATA LINK_RPOS/0/,BANK_RPOS/LEN_RPOS*0/                             39
C *                                                                       39
C -                                                                       40
      INTEGER RANG_VERSION                                                40
      EQUIVALENCE (BANK_RANG(3),RANG_VERSION,RE_RANG_VERSION),            40
     *(NAMES_RANG(1),NRANG_VERSION)                                       40
      EQUIVALENCE (COMMEN_RANG(1),CRANG_VERSION),(MAP_RANG(1,1),          40
     *MRANG_VERSION)                                                      40
      CHARACTER CRANG_VERSION*40/' '/                                     40
      CHARACTER NRANG_VERSION*12/'I.VERSION'/                             40
      INTEGER MRANG_VERSION(2)/1,1/                                       40
      REAL RE_RANG_VERSION                                                40
C -                                                                       40
      INTEGER RANG_LENGTH                                                 40
      EQUIVALENCE (BANK_RANG(4),RANG_LENGTH,RE_RANG_LENGTH),              40
     *(NAMES_RANG(2),NRANG_LENGTH)                                        40
      EQUIVALENCE (COMMEN_RANG(2),CRANG_LENGTH),(MAP_RANG(1,2),           40
     *MRANG_LENGTH)                                                       40
      CHARACTER CRANG_LENGTH*40/' '/                                      40
      CHARACTER NRANG_LENGTH*12/'I.LENGTH'/                               40
      INTEGER MRANG_LENGTH(2)/1,1/                                        40
      REAL RE_RANG_LENGTH                                                 40
C -                                                                       40
      REAL RANG_PPCKOV(30)                                                40
      EQUIVALENCE (BANK_RANG(5),RANG_PPCKOV,RE_RANG_PPCKOV),              40
     *(NAMES_RANG(3),NRANG_PPCKOV)                                        40
      EQUIVALENCE (COMMEN_RANG(3),CRANG_PPCKOV),(MAP_RANG(1,3),           40
     *MRANG_PPCKOV)                                                       40
      CHARACTER CRANG_PPCKOV*40/' '/                                      40
      CHARACTER NRANG_PPCKOV*12/'R.PPCKOV'/                               40
      INTEGER MRANG_PPCKOV(2)/30,1/                                       40
      REAL RE_RANG_PPCKOV                                                 40
C -                                                                       40
      INTEGER LEN_RANG,LLEN_RANG,LINK_RANG                                40
      PARAMETER (LEN_RANG=34)                                             40
      PARAMETER (LLEN_RANG=3)                                             40
      INTEGER FLAG_RANG/0/,MAP_RANG(2,LLEN_RANG),IDNUM_RANG(10)           40
      CHARACTER NAMES_RANG(LLEN_RANG)*12,COMMEN_RANG(LLEN_RANG)*40        40
      DATA IDNUM_RANG/10*0/                                               40
      REAL BANK_RANG(LEN_RANG)                                            40
      CHARACTER*60 BPATH_RANG/' '/                                        40
      SAVE FLAG_RANG,MAP_RANG,IDNUM_RANG,NAMES_RANG,COMMEN_RANG,          40
     *BPATH_RANG                                                          40
      DATA LINK_RANG/0/,BANK_RANG/LEN_RANG*0/                             40
C *                                                                       40
C -                                                                       41
      CHARACTER*4 RMAT_MATE                                               41
      EQUIVALENCE (BANK_RMAT(3),RMAT_MATE,RE_RMAT_MATE),(NAMES_RMAT(1),   41
     *NRMAT_MATE)                                                         41
      EQUIVALENCE (COMMEN_RMAT(1),CRMAT_MATE),(MAP_RMAT(1,1),             41
     *MRMAT_MATE)                                                         41
      CHARACTER CRMAT_MATE*40/' '/                                        41
      CHARACTER NRMAT_MATE*12/'H.MATE'/                                   41
      INTEGER MRMAT_MATE(2)/1,1/                                          41
      REAL RE_RMAT_MATE                                                   41
C -                                                                       41
      REAL RMAT_RINDEX(30)                                                41
      EQUIVALENCE (BANK_RMAT(4),RMAT_RINDEX,RE_RMAT_RINDEX),              41
     *(NAMES_RMAT(2),NRMAT_RINDEX)                                        41
      EQUIVALENCE (COMMEN_RMAT(2),CRMAT_RINDEX),(MAP_RMAT(1,2),           41
     *MRMAT_RINDEX)                                                       41
      CHARACTER CRMAT_RINDEX*40/' '/                                      41
      CHARACTER NRMAT_RINDEX*12/'R.RINDEX'/                               41
      INTEGER MRMAT_RINDEX(2)/30,1/                                       41
      REAL RE_RMAT_RINDEX                                                 41
C -                                                                       41
      REAL RMAT_ABSCO(30)                                                 41
      EQUIVALENCE (BANK_RMAT(34),RMAT_ABSCO,RE_RMAT_ABSCO),               41
     *(NAMES_RMAT(3),NRMAT_ABSCO)                                         41
      EQUIVALENCE (COMMEN_RMAT(3),CRMAT_ABSCO),(MAP_RMAT(1,3),            41
     *MRMAT_ABSCO)                                                        41
      CHARACTER CRMAT_ABSCO*40/' '/                                       41
      CHARACTER NRMAT_ABSCO*12/'R.ABSCO'/                                 41
      INTEGER MRMAT_ABSCO(2)/30,1/                                        41
      REAL RE_RMAT_ABSCO                                                  41
C -                                                                       41
      REAL RMAT_EFFIC(30)                                                 41
      EQUIVALENCE (BANK_RMAT(64),RMAT_EFFIC,RE_RMAT_EFFIC),               41
     *(NAMES_RMAT(4),NRMAT_EFFIC)                                         41
      EQUIVALENCE (COMMEN_RMAT(4),CRMAT_EFFIC),(MAP_RMAT(1,4),            41
     *MRMAT_EFFIC)                                                        41
      CHARACTER CRMAT_EFFIC*40/' '/                                       41
      CHARACTER NRMAT_EFFIC*12/'R.EFFIC'/                                 41
      INTEGER MRMAT_EFFIC(2)/30,1/                                        41
      REAL RE_RMAT_EFFIC                                                  41
C -                                                                       41
      INTEGER LEN_RMAT,LLEN_RMAT,LINK_RMAT                                41
      PARAMETER (LEN_RMAT=93)                                             41
      PARAMETER (LLEN_RMAT=4)                                             41
      INTEGER FLAG_RMAT/0/,MAP_RMAT(2,LLEN_RMAT),IDNUM_RMAT(10)           41
      CHARACTER NAMES_RMAT(LLEN_RMAT)*12,COMMEN_RMAT(LLEN_RMAT)*40        41
      DATA IDNUM_RMAT/10*0/                                               41
      REAL BANK_RMAT(LEN_RMAT)                                            41
      CHARACTER*60 BPATH_RMAT/' '/                                        41
      SAVE FLAG_RMAT,MAP_RMAT,IDNUM_RMAT,NAMES_RMAT,COMMEN_RMAT,          41
     *BPATH_RMAT                                                          41
      DATA LINK_RMAT/0/,BANK_RMAT/LEN_RMAT*0/                             41
C *                                                                       41
      INTEGER N,I,ISOPT                                                   43
* double precision is needed to keep angle<->cosinus conversion correct
      REAL*8 ROT(3,3), TX, TY, TZ                                         46
      EXTERNAL RICHSTEP,RCSISTEP                                          47
*
C *                                                                       49
      CALL AGSBEGM(MODULE,IPRIN)                                          49
      IF (IPRIN.LT.0) RETURN                                              49
C *                                                                       51
      CALL AGSBEGM(MODULE,IPRIN)                                          51
      IF (IPRIN.LT.0) RETURN                                              51
C Filling bank RICH - CERENKOV DETECTOR PARAMETERS                        51
      BANK_TITLE='CERENKOV DETECTOR PARAMETERS'                           51
      BPATH_RICH='RICH*'                                                  51
      IDNUM_RICH(1)=0                                                     52
C COMPLEXITY VERSION                                                      52
      RICH_VERSION=1                                                      52
      CRICH_VERSION='COMPLEXITY VERSION'                                  52
C ACTUAL POSITION IN STAR                                                 53
      RICH_POSITION=1                                                     53
      CRICH_POSITION='ACTUAL POSITION IN STAR'                            53
C VERSION OF CERENKOV LIGHT TABLES                                        54
      RICH_CVERSION=1                                                     54
      CRICH_CVERSION='VERSION OF CERENKOV LIGHT TABLES'                   54
C HALF WIDTH (LONG DIMENSION)                                             55
      RICH_DX=49.45                                                       55
      CRICH_DX='HALF WIDTH (LONG DIMENSION)'                              55
C RICH HALF THICKNESS                                                     56
      RICH_DY=11.325                                                      56
      CRICH_DY='RICH HALF THICKNESS'                                      56
C HALF LENGTH (SHORT DIMENSION)                                           57
      RICH_DZ=73.15                                                       57
      CRICH_DZ='HALF LENGTH (SHORT DIMENSION)'                            57
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RICH',    59
     *BPATH_RICH,IDNUM_RICH,1, MAP_RICH,NAMES_RICH,COMMEN_RICH,           59
     *BANK_RICH,LEN_RICH,LLEN_RICH,LINK_RICH,FLAG_RICH)                   59
C *                                                                       59
C *                                                                       59
      CALL AGSBEGM(MODULE,IPRIN)                                          59
      IF (IPRIN.LT.0) RETURN                                              59
C Filling bank RPOS - CERENKOV DETECTOR POSITION                          59
      BANK_TITLE='CERENKOV DETECTOR POSITION'                             59
      BPATH_RPOS='RPOS*'                                                  59
      IDNUM_RPOS(1)=0                                                     60
C NOMINAL POSITION IN STAR                                                60
      RPOS_POSITION=1                                                     60
      CRPOS_POSITION='NOMINAL POSITION IN STAR'                           60
C PAD PLANE CENTER PHI POSITION                                           61
      RPOS_PHI=300                                                        61
      CRPOS_PHI='PAD PLANE CENTER PHI POSITION'                           61
C DISTANCE TO CENTER                                                      62
      RPOS_RPOS=235.0                                                     62
      CRPOS_RPOS='DISTANCE TO CENTER'                                     62
C PAD PLANE CENTER OFFSET                                                 63
      RPOS_YPOS=0                                                         63
      CRPOS_YPOS='PAD PLANE CENTER OFFSET'                                63
C Z POSITION OF THE PAD PLANE CENTER                                      64
      RPOS_ZPOS=0                                                         64
      CRPOS_ZPOS='Z POSITION OF THE PAD PLANE CENTER'                     64
C RICH ORIENTATION X-ANGLE (DEG)                                          65
      RPOS_TX=0                                                           65
      CRPOS_TX='RICH ORIENTATION X-ANGLE (DEG)'                           65
C RICH ORIENTATION Y-ANGLE (DEG)                                          66
      RPOS_TY=0                                                           66
      CRPOS_TY='RICH ORIENTATION Y-ANGLE (DEG)'                           66
C RICH ORIENTATION Z-ANGLE (DEG)                                          67
      RPOS_TZ=30                                                          67
      CRPOS_TZ='RICH ORIENTATION Z-ANGLE (DEG)'                           67
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RPOS',    69
     *BPATH_RPOS,IDNUM_RPOS,1, MAP_RPOS,NAMES_RPOS,COMMEN_RPOS,           69
     *BANK_RPOS,LEN_RPOS,LLEN_RPOS,LINK_RPOS,FLAG_RPOS)                   69
C *                                                                       69
C *                                                                       69
      CALL AGSBEGM(MODULE,IPRIN)                                          69
      IF (IPRIN.LT.0) RETURN                                              69
C Filling bank RPOS - CERENKOV DETECTOR POSITION                          69
      BANK_TITLE='CERENKOV DETECTOR POSITION'                             69
      BPATH_RPOS='RPOS*'                                                  69
      IDNUM_RPOS(1)=0                                                     70
C ACTUAL POSITION IN STAR                                                 70
      RPOS_POSITION=2                                                     70
      CRPOS_POSITION='ACTUAL POSITION IN STAR'                            70
C PAD PLANE CENTER PHI POSITION                                           71
      RPOS_PHI=-59.95062                                                  71
      CRPOS_PHI='PAD PLANE CENTER PHI POSITION'                           71
C DISTANCE TO THE PAD PANE CENTER                                         72
      RPOS_RPOS=240.22755                                                 72
      CRPOS_RPOS='DISTANCE TO THE PAD PANE CENTER'                        72
C PAD PLANE CENTER OFFSET                                                 73
      RPOS_YPOS=8.125                                                     73
      CRPOS_YPOS='PAD PLANE CENTER OFFSET'                                73
C Z POSITION OF THE PAD PLANE CENTER                                      74
      RPOS_ZPOS=-.3089772                                                 74
      CRPOS_ZPOS='Z POSITION OF THE PAD PLANE CENTER'                     74
C RICH ORIENTATION X-ANGLE (DEG)                                          75
      RPOS_TX=-.02003531                                                  75
      CRPOS_TX='RICH ORIENTATION X-ANGLE (DEG)'                           75
C RICH ORIENTATION Y-ANGLE (DEG)                                          76
      RPOS_TY=.03788445                                                   76
      CRPOS_TY='RICH ORIENTATION Y-ANGLE (DEG)'                           76
C RICH ORIENTATION Z-ANGLE (DEG)                                          77
      RPOS_TZ=29.81715                                                    77
      CRPOS_TZ='RICH ORIENTATION Z-ANGLE (DEG)'                           77
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RPOS',    78
     *BPATH_RPOS,IDNUM_RPOS,1, MAP_RPOS,NAMES_RPOS,COMMEN_RPOS,           78
     *BANK_RPOS,LEN_RPOS,LLEN_RPOS,LINK_RPOS,FLAG_RPOS)                   78
C *                                                                       78
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RPOS',    78
     *BPATH_RPOS,IDNUM_RPOS,1, MAP_RPOS,NAMES_RPOS,COMMEN_RPOS,           78
     *BANK_RPOS,LEN_RPOS,LLEN_RPOS,LINK_RPOS,FLAG_RPOS)                   78
*
C *                                                                       81
      CALL AGSBEGM(MODULE,IPRIN)                                          81
      IF (IPRIN.LT.0) RETURN                                              81
C Filling bank RANG(1) - CERENKOV LIGHT ENERGY RANGE                      81
      BANK_TITLE='CERENKOV LIGHT ENERGY RANGE'                            81
      BPATH_RANG='RANG*'                                                  81
      IDNUM_RANG(1)=1                                                     82
C TABLE VERSION                                                           82
      RANG_VERSION=1                                                      82
      CRANG_VERSION='TABLE VERSION'                                       82
C TABLE LENGTH                                                            83
      RANG_LENGTH=13                                                      83
      CRANG_LENGTH='TABLE LENGTH'                                         83
C PHOTON ENERGIES                                                         86
      RANG_PPCKOV(1)=5.63E-9                                              86
      RANG_PPCKOV(2)=5.77E-9                                              86
      RANG_PPCKOV(3)=5.9E-9                                               86
      RANG_PPCKOV(4)=6.05E-9                                              86
      RANG_PPCKOV(5)=6.2E-9                                               86
      RANG_PPCKOV(6)=6.36E-9                                              86
      RANG_PPCKOV(7)=6.52E-9                                              86
      RANG_PPCKOV(8)=6.7E-9                                               86
      RANG_PPCKOV(9)=6.88E-9                                              86
      RANG_PPCKOV(10)=7.08E-9                                             86
      RANG_PPCKOV(11)=7.3E-9                                              86
      RANG_PPCKOV(12)=7.52E-9                                             86
      RANG_PPCKOV(13)=7.75E-9                                             86
      CRANG_PPCKOV='PHOTON ENERGIES'                                      86
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RANG',    88
     *BPATH_RANG,IDNUM_RANG,1, MAP_RANG,NAMES_RANG,COMMEN_RANG,           88
     *BANK_RANG,LEN_RANG,LLEN_RANG,LINK_RANG,FLAG_RANG)                   88
C *                                                                       88
C *                                                                       88
      CALL AGSBEGM(MODULE,IPRIN)                                          88
      IF (IPRIN.LT.0) RETURN                                              88
C Filling bank RANG(1)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        88
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   88
      BPATH_RMAT='RANG/RMAT*'                                             88
      IDNUM_RMAT(1)=1                                                     89
      IDNUM_RMAT(2)=0                                                     89
C DEFAULT MATERIALS                                                       89
      RMAT_MATE='GRI'                                                     89
      CRMAT_MATE='DEFAULT MATERIALS'                                      89
C DETECTION EFFICIENCIES                                                  90
      RMAT_EFFIC(1)=1                                                     90
      RMAT_EFFIC(2)=1                                                     90
      RMAT_EFFIC(3)=1                                                     90
      RMAT_EFFIC(4)=1                                                     90
      RMAT_EFFIC(5)=1                                                     90
      RMAT_EFFIC(6)=1                                                     90
      RMAT_EFFIC(7)=1                                                     90
      RMAT_EFFIC(8)=1                                                     90
      RMAT_EFFIC(9)=1                                                     90
      RMAT_EFFIC(10)=1                                                    90
      RMAT_EFFIC(11)=1                                                    90
      RMAT_EFFIC(12)=1                                                    90
      RMAT_EFFIC(13)=1                                                    90
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                90
C REFRACTION INDEX                                                        91
      RMAT_RINDEX(1)=1                                                    91
      RMAT_RINDEX(2)=1                                                    91
      RMAT_RINDEX(3)=1                                                    91
      RMAT_RINDEX(4)=1                                                    91
      RMAT_RINDEX(5)=1                                                    91
      RMAT_RINDEX(6)=1                                                    91
      RMAT_RINDEX(7)=1                                                    91
      RMAT_RINDEX(8)=1                                                    91
      RMAT_RINDEX(9)=1                                                    91
      RMAT_RINDEX(10)=1                                                   91
      RMAT_RINDEX(11)=1                                                   91
      RMAT_RINDEX(12)=1                                                   91
      RMAT_RINDEX(13)=1                                                   91
      CRMAT_RINDEX='REFRACTION INDEX'                                     91
C ABSORBTION LENGHTS (IN CM)                                              93
      RMAT_ABSCO(1)=1.E-4                                                 93
      RMAT_ABSCO(2)=1.E-4                                                 93
      RMAT_ABSCO(3)=1.E-4                                                 93
      RMAT_ABSCO(4)=1.E-4                                                 93
      RMAT_ABSCO(5)=1.E-4                                                 93
      RMAT_ABSCO(6)=1.E-4                                                 93
      RMAT_ABSCO(7)=1.E-4                                                 93
      RMAT_ABSCO(8)=1.E-4                                                 93
      RMAT_ABSCO(9)=1.E-4                                                 93
      RMAT_ABSCO(10)=1.E-4                                                93
      RMAT_ABSCO(11)=1.E-4                                                93
      RMAT_ABSCO(12)=1.E-4                                                93
      RMAT_ABSCO(13)=1.E-4                                                93
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            93
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    95
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           95
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   95
C *                                                                       95
C *                                                                       95
      CALL AGSBEGM(MODULE,IPRIN)                                          95
      IF (IPRIN.LT.0) RETURN                                              95
C Filling bank RANG(1)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        95
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   95
      BPATH_RMAT='RANG/RMAT*'                                             95
      IDNUM_RMAT(1)=1                                                     96
      IDNUM_RMAT(2)=0                                                     96
C OPAQUE QUARZ                                                            96
      RMAT_MATE='QRZO'                                                    96
      CRMAT_MATE='OPAQUE QUARZ'                                           96
C DETECTION EFFICIENCIES                                                  97
      RMAT_EFFIC(1)=1                                                     97
      RMAT_EFFIC(2)=1                                                     97
      RMAT_EFFIC(3)=1                                                     97
      RMAT_EFFIC(4)=1                                                     97
      RMAT_EFFIC(5)=1                                                     97
      RMAT_EFFIC(6)=1                                                     97
      RMAT_EFFIC(7)=1                                                     97
      RMAT_EFFIC(8)=1                                                     97
      RMAT_EFFIC(9)=1                                                     97
      RMAT_EFFIC(10)=1                                                    97
      RMAT_EFFIC(11)=1                                                    97
      RMAT_EFFIC(12)=1                                                    97
      RMAT_EFFIC(13)=1                                                    97
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                97
C REFRACTION INDEX                                                        98
      RMAT_RINDEX(1)=1                                                    98
      RMAT_RINDEX(2)=1                                                    98
      RMAT_RINDEX(3)=1                                                    98
      RMAT_RINDEX(4)=1                                                    98
      RMAT_RINDEX(5)=1                                                    98
      RMAT_RINDEX(6)=1                                                    98
      RMAT_RINDEX(7)=1                                                    98
      RMAT_RINDEX(8)=1                                                    98
      RMAT_RINDEX(9)=1                                                    98
      RMAT_RINDEX(10)=1                                                   98
      RMAT_RINDEX(11)=1                                                   98
      RMAT_RINDEX(12)=1                                                   98
      RMAT_RINDEX(13)=1                                                   98
      CRMAT_RINDEX='REFRACTION INDEX'                                     98
C ABSORBTION LENGHTS (IN CM)                                              100
      RMAT_ABSCO(1)=1.E-5                                                 100
      RMAT_ABSCO(2)=1.E-5                                                 100
      RMAT_ABSCO(3)=1.E-5                                                 100
      RMAT_ABSCO(4)=1.E-5                                                 100
      RMAT_ABSCO(5)=1.E-5                                                 100
      RMAT_ABSCO(6)=1.E-5                                                 100
      RMAT_ABSCO(7)=1.E-5                                                 100
      RMAT_ABSCO(8)=1.E-5                                                 100
      RMAT_ABSCO(9)=1.E-5                                                 100
      RMAT_ABSCO(10)=1.E-5                                                100
      RMAT_ABSCO(11)=1.E-5                                                100
      RMAT_ABSCO(12)=1.E-5                                                100
      RMAT_ABSCO(13)=1.E-5                                                100
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            100
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    102
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           102
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   102
C *                                                                       102
C *                                                                       102
      CALL AGSBEGM(MODULE,IPRIN)                                          102
      IF (IPRIN.LT.0) RETURN                                              102
C Filling bank RANG(1)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        102
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   102
      BPATH_RMAT='RANG/RMAT*'                                             102
      IDNUM_RMAT(1)=1                                                     103
      IDNUM_RMAT(2)=0                                                     103
C MATERIAL - METHANE                                                      103
      RMAT_MATE='META'                                                    103
      CRMAT_MATE='MATERIAL - METHANE'                                     103
C DETECTION EFFICIENCIES                                                  104
      RMAT_EFFIC(1)=1                                                     104
      RMAT_EFFIC(2)=1                                                     104
      RMAT_EFFIC(3)=1                                                     104
      RMAT_EFFIC(4)=1                                                     104
      RMAT_EFFIC(5)=1                                                     104
      RMAT_EFFIC(6)=1                                                     104
      RMAT_EFFIC(7)=1                                                     104
      RMAT_EFFIC(8)=1                                                     104
      RMAT_EFFIC(9)=1                                                     104
      RMAT_EFFIC(10)=1                                                    104
      RMAT_EFFIC(11)=1                                                    104
      RMAT_EFFIC(12)=1                                                    104
      RMAT_EFFIC(13)=1                                                    104
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                104
C REFRACTION INDEX                                                        107
      RMAT_RINDEX(1)=1.000444                                             107
      RMAT_RINDEX(2)=1.000444                                             107
      RMAT_RINDEX(3)=1.000444                                             107
      RMAT_RINDEX(4)=1.000444                                             107
      RMAT_RINDEX(5)=1.000444                                             107
      RMAT_RINDEX(6)=1.000444                                             107
      RMAT_RINDEX(7)=1.000444                                             107
      RMAT_RINDEX(8)=1.000444                                             107
      RMAT_RINDEX(9)=1.000444                                             107
      RMAT_RINDEX(10)=1.000444                                            107
      RMAT_RINDEX(11)=1.000444                                            107
      RMAT_RINDEX(12)=1.000444                                            107
      RMAT_RINDEX(13)=1.000444                                            107
      CRMAT_RINDEX='REFRACTION INDEX'                                     107
C ABSORBTION LENGHTS (IN CM)                                              109
      RMAT_ABSCO(1)=1.E6                                                  109
      RMAT_ABSCO(2)=1.E6                                                  109
      RMAT_ABSCO(3)=1.E6                                                  109
      RMAT_ABSCO(4)=1.E6                                                  109
      RMAT_ABSCO(5)=1.E6                                                  109
      RMAT_ABSCO(6)=1.E6                                                  109
      RMAT_ABSCO(7)=1.E6                                                  109
      RMAT_ABSCO(8)=1.E6                                                  109
      RMAT_ABSCO(9)=1.E6                                                  109
      RMAT_ABSCO(10)=1.E6                                                 109
      RMAT_ABSCO(11)=1.E6                                                 109
      RMAT_ABSCO(12)=1.E6                                                 109
      RMAT_ABSCO(13)=1.E6                                                 109
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            109
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    111
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           111
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   111
C *                                                                       111
C *                                                                       111
      CALL AGSBEGM(MODULE,IPRIN)                                          111
      IF (IPRIN.LT.0) RETURN                                              111
C Filling bank RANG(1)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        111
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   111
      BPATH_RMAT='RANG/RMAT*'                                             111
      IDNUM_RMAT(1)=1                                                     112
      IDNUM_RMAT(2)=0                                                     112
C MATERIAL - QUARTZ                                                       112
      RMAT_MATE='QARZ'                                                    112
      CRMAT_MATE='MATERIAL - QUARTZ'                                      112
C DETECTION EFFICIENCIES                                                  113
      RMAT_EFFIC(1)=1                                                     113
      RMAT_EFFIC(2)=1                                                     113
      RMAT_EFFIC(3)=1                                                     113
      RMAT_EFFIC(4)=1                                                     113
      RMAT_EFFIC(5)=1                                                     113
      RMAT_EFFIC(6)=1                                                     113
      RMAT_EFFIC(7)=1                                                     113
      RMAT_EFFIC(8)=1                                                     113
      RMAT_EFFIC(9)=1                                                     113
      RMAT_EFFIC(10)=1                                                    113
      RMAT_EFFIC(11)=1                                                    113
      RMAT_EFFIC(12)=1                                                    113
      RMAT_EFFIC(13)=1                                                    113
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                113
C REFRACTION INDEX                                                        116
      RMAT_RINDEX(1)=1.528309                                             116
      RMAT_RINDEX(2)=1.533333                                             116
      RMAT_RINDEX(3)=1.538243                                             116
      RMAT_RINDEX(4)=1.544223                                             116
      RMAT_RINDEX(5)=1.550568                                             116
      RMAT_RINDEX(6)=1.557770                                             116
      RMAT_RINDEX(7)=1.565463                                             116
      RMAT_RINDEX(8)=1.574765                                             116
      RMAT_RINDEX(9)=1.584831                                             116
      RMAT_RINDEX(10)=1.597027                                            116
      RMAT_RINDEX(11)=1.611858                                            116
      RMAT_RINDEX(12)=1.62366                                             116
      RMAT_RINDEX(13)=1.63815                                             116
      CRMAT_RINDEX='REFRACTION INDEX'                                     116
C ABSORBTION LENGHTS (IN CM)                                              118
      RMAT_ABSCO(1)=1.E6                                                  118
      RMAT_ABSCO(2)=1.E6                                                  118
      RMAT_ABSCO(3)=1.E6                                                  118
      RMAT_ABSCO(4)=1.E6                                                  118
      RMAT_ABSCO(5)=1.E6                                                  118
      RMAT_ABSCO(6)=29.85                                                 118
      RMAT_ABSCO(7)=7.34                                                  118
      RMAT_ABSCO(8)=4.134                                                 118
      RMAT_ABSCO(9)=1.273                                                 118
      RMAT_ABSCO(10)=.722                                                 118
      RMAT_ABSCO(11)=.365                                                 118
      RMAT_ABSCO(12)=.10051                                               118
      RMAT_ABSCO(13)=0.001                                                118
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            118
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    120
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           120
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   120
C *                                                                       120
C *                                                                       120
      CALL AGSBEGM(MODULE,IPRIN)                                          120
      IF (IPRIN.LT.0) RETURN                                              120
C Filling bank RANG(1)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        120
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   120
      BPATH_RMAT='RANG/RMAT*'                                             120
      IDNUM_RMAT(1)=1                                                     121
      IDNUM_RMAT(2)=0                                                     121
C MATERIAL                                                                121
      RMAT_MATE='FREO'                                                    121
      CRMAT_MATE='MATERIAL'                                               121
C DETECTION EFFICIENCIES                                                  122
      RMAT_EFFIC(1)=1                                                     122
      RMAT_EFFIC(2)=1                                                     122
      RMAT_EFFIC(3)=1                                                     122
      RMAT_EFFIC(4)=1                                                     122
      RMAT_EFFIC(5)=1                                                     122
      RMAT_EFFIC(6)=1                                                     122
      RMAT_EFFIC(7)=1                                                     122
      RMAT_EFFIC(8)=1                                                     122
      RMAT_EFFIC(9)=1                                                     122
      RMAT_EFFIC(10)=1                                                    122
      RMAT_EFFIC(11)=1                                                    122
      RMAT_EFFIC(12)=1                                                    122
      RMAT_EFFIC(13)=1                                                    122
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                122
C REFRACTION INDEX                                                        125
      RMAT_RINDEX(1)=1.269946                                             125
      RMAT_RINDEX(2)=1.271246                                             125
      RMAT_RINDEX(3)=1.272452                                             125
      RMAT_RINDEX(4)=1.273844                                             125
      RMAT_RINDEX(5)=1.275236                                             125
      RMAT_RINDEX(6)=1.276721                                             125
      RMAT_RINDEX(7)=1.278206                                             125
      RMAT_RINDEX(8)=1.279876                                             125
      RMAT_RINDEX(9)=1.281546                                             125
      RMAT_RINDEX(10)=1.283402                                            125
      RMAT_RINDEX(11)=1.285444                                            125
      RMAT_RINDEX(12)=1.28731                                             125
      RMAT_RINDEX(13)=1.28936                                             125
      CRMAT_RINDEX='REFRACTION INDEX'                                     125
C ABSORBTION LENGHTS (IN CM)                                              128
      RMAT_ABSCO(1)=179.0987                                              128
      RMAT_ABSCO(2)=179.0987                                              128
      RMAT_ABSCO(3)=179.0987                                              128
      RMAT_ABSCO(4)=179.0987                                              128
      RMAT_ABSCO(5)=179.0987                                              128
      RMAT_ABSCO(6)=121.9547                                              128
      RMAT_ABSCO(7)=43.40067                                              128
      RMAT_ABSCO(8)=15.7394                                               128
      RMAT_ABSCO(9)=9.417928                                              128
      RMAT_ABSCO(10)=5.195241                                             128
      RMAT_ABSCO(11)=1.415808                                             128
      RMAT_ABSCO(12)=0.36728                                              128
      RMAT_ABSCO(13)=0.001                                                128
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            128
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    130
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           130
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   130
C *                                                                       130
C *                                                                       130
      CALL AGSBEGM(MODULE,IPRIN)                                          130
      IF (IPRIN.LT.0) RETURN                                              130
C Filling bank RANG(1)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        130
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   130
      BPATH_RMAT='RANG/RMAT*'                                             130
      IDNUM_RMAT(1)=1                                                     131
      IDNUM_RMAT(2)=0                                                     131
C MATERIAL - CESIUM IODIDE                                                131
      RMAT_MATE='CSI'                                                     131
      CRMAT_MATE='MATERIAL - CESIUM IODIDE'                               131
C QUANTUM EFFICIENCIES                                                    134
      RMAT_EFFIC(1)=3.15E-4                                               134
      RMAT_EFFIC(2)=4.50E-4                                               134
      RMAT_EFFIC(3)=6.75E-3                                               134
      RMAT_EFFIC(4)=1.125E-2                                              134
      RMAT_EFFIC(5)=2.115E-2                                              134
      RMAT_EFFIC(6)=3.60E-2                                               134
      RMAT_EFFIC(7)=8.46E-2                                               134
      RMAT_EFFIC(8)=.15533                                                134
      RMAT_EFFIC(9)=.20286                                                134
      RMAT_EFFIC(10)=.24745                                               134
      RMAT_EFFIC(11)=.27881                                               134
      RMAT_EFFIC(12)=0.3060                                               134
      RMAT_EFFIC(13)=0.3250                                               134
      CRMAT_EFFIC='QUANTUM EFFICIENCIES'                                  134
C REFRACTION INDEX                                                        137
      RMAT_RINDEX(1)=1.000444                                             137
      RMAT_RINDEX(2)=1.000444                                             137
      RMAT_RINDEX(3)=1.000444                                             137
      RMAT_RINDEX(4)=1.000444                                             137
      RMAT_RINDEX(5)=1.000444                                             137
      RMAT_RINDEX(6)=1.000444                                             137
      RMAT_RINDEX(7)=1.000444                                             137
      RMAT_RINDEX(8)=1.000444                                             137
      RMAT_RINDEX(9)=1.000444                                             137
      RMAT_RINDEX(10)=1.000444                                            137
      RMAT_RINDEX(11)=1.000444                                            137
      RMAT_RINDEX(12)=1.000444                                            137
      RMAT_RINDEX(13)=1.000444                                            137
      CRMAT_RINDEX='REFRACTION INDEX'                                     137
C ABSORBTION LENGHTS (IN CM)                                              139
      RMAT_ABSCO(1)=1.E-4                                                 139
      RMAT_ABSCO(2)=1.E-4                                                 139
      RMAT_ABSCO(3)=1.E-4                                                 139
      RMAT_ABSCO(4)=1.E-4                                                 139
      RMAT_ABSCO(5)=1.E-4                                                 139
      RMAT_ABSCO(6)=1.E-4                                                 139
      RMAT_ABSCO(7)=1.E-4                                                 139
      RMAT_ABSCO(8)=1.E-4                                                 139
      RMAT_ABSCO(9)=1.E-4                                                 139
      RMAT_ABSCO(10)=1.E-4                                                139
      RMAT_ABSCO(11)=1.E-4                                                139
      RMAT_ABSCO(12)=1.E-4                                                139
      RMAT_ABSCO(13)=1.E-4                                                139
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            139
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    140
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           140
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   140
C *                                                                       140
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    140
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           140
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   140
*
C *                                                                       143
      CALL AGSBEGM(MODULE,IPRIN)                                          143
      IF (IPRIN.LT.0) RETURN                                              143
C Filling bank RANG(2) - CERENKOV LIGHT ENERGY RANGE                      143
      BANK_TITLE='CERENKOV LIGHT ENERGY RANGE'                            143
      BPATH_RANG='RANG*'                                                  143
      IDNUM_RANG(1)=2                                                     144
C TABLE VERSION                                                           144
      RANG_VERSION=2                                                      144
      CRANG_VERSION='TABLE VERSION'                                       144
C TABLE LENGTH                                                            145
      RANG_LENGTH=26                                                      145
      CRANG_LENGTH='TABLE LENGTH'                                         145
C PHOTON ENERGIES                                                         150
      RANG_PPCKOV(1)=5.5E-9                                               150
      RANG_PPCKOV(2)=5.6E-9                                               150
      RANG_PPCKOV(3)=5.7E-9                                               150
      RANG_PPCKOV(4)=5.8E-9                                               150
      RANG_PPCKOV(5)=5.9E-9                                               150
      RANG_PPCKOV(6)=6.0E-9                                               150
      RANG_PPCKOV(7)=6.1E-9                                               150
      RANG_PPCKOV(8)=6.2E-9                                               150
      RANG_PPCKOV(9)=6.3E-9                                               150
      RANG_PPCKOV(10)=6.4E-9                                              150
      RANG_PPCKOV(11)=6.5E-9                                              150
      RANG_PPCKOV(12)=6.6E-9                                              150
      RANG_PPCKOV(13)=6.7E-9                                              150
      RANG_PPCKOV(14)=6.8E-9                                              150
      RANG_PPCKOV(15)=6.9E-9                                              150
      RANG_PPCKOV(16)=7.0E-9                                              150
      RANG_PPCKOV(17)=7.1E-9                                              150
      RANG_PPCKOV(18)=7.2E-9                                              150
      RANG_PPCKOV(19)=7.3E-9                                              150
      RANG_PPCKOV(20)=7.4E-9                                              150
      RANG_PPCKOV(21)=7.5E-9                                              150
      RANG_PPCKOV(22)=7.6E-9                                              150
      RANG_PPCKOV(23)=7.7E-9                                              150
      RANG_PPCKOV(24)=7.8E-9                                              150
      RANG_PPCKOV(25)=7.9E-9                                              150
      RANG_PPCKOV(26)=8.0E-9                                              150
      CRANG_PPCKOV='PHOTON ENERGIES'                                      150
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RANG',    152
     *BPATH_RANG,IDNUM_RANG,1, MAP_RANG,NAMES_RANG,COMMEN_RANG,           152
     *BANK_RANG,LEN_RANG,LLEN_RANG,LINK_RANG,FLAG_RANG)                   152
C *                                                                       152
C *                                                                       152
      CALL AGSBEGM(MODULE,IPRIN)                                          152
      IF (IPRIN.LT.0) RETURN                                              152
C Filling bank RANG(2)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        152
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   152
      BPATH_RMAT='RANG/RMAT*'                                             152
      IDNUM_RMAT(1)=2                                                     153
      IDNUM_RMAT(2)=0                                                     153
C DEFAULT MATERIALS                                                       153
      RMAT_MATE='GRI'                                                     153
      CRMAT_MATE='DEFAULT MATERIALS'                                      153
C DETECTION EFFICIENCIES                                                  155
      RMAT_EFFIC(1)=1                                                     155
      RMAT_EFFIC(2)=1                                                     155
      RMAT_EFFIC(3)=1                                                     155
      RMAT_EFFIC(4)=1                                                     155
      RMAT_EFFIC(5)=1                                                     155
      RMAT_EFFIC(6)=1                                                     155
      RMAT_EFFIC(7)=1                                                     155
      RMAT_EFFIC(8)=1                                                     155
      RMAT_EFFIC(9)=1                                                     155
      RMAT_EFFIC(10)=1                                                    155
      RMAT_EFFIC(11)=1                                                    155
      RMAT_EFFIC(12)=1                                                    155
      RMAT_EFFIC(13)=1                                                    155
      RMAT_EFFIC(14)=1                                                    155
      RMAT_EFFIC(15)=1                                                    155
      RMAT_EFFIC(16)=1                                                    155
      RMAT_EFFIC(17)=1                                                    155
      RMAT_EFFIC(18)=1                                                    155
      RMAT_EFFIC(19)=1                                                    155
      RMAT_EFFIC(20)=1                                                    155
      RMAT_EFFIC(21)=1                                                    155
      RMAT_EFFIC(22)=1                                                    155
      RMAT_EFFIC(23)=1                                                    155
      RMAT_EFFIC(24)=1                                                    155
      RMAT_EFFIC(25)=1                                                    155
      RMAT_EFFIC(26)=1                                                    155
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                155
C REFRACTION INDEX                                                        157
      RMAT_RINDEX(1)=1                                                    157
      RMAT_RINDEX(2)=1                                                    157
      RMAT_RINDEX(3)=1                                                    157
      RMAT_RINDEX(4)=1                                                    157
      RMAT_RINDEX(5)=1                                                    157
      RMAT_RINDEX(6)=1                                                    157
      RMAT_RINDEX(7)=1                                                    157
      RMAT_RINDEX(8)=1                                                    157
      RMAT_RINDEX(9)=1                                                    157
      RMAT_RINDEX(10)=1                                                   157
      RMAT_RINDEX(11)=1                                                   157
      RMAT_RINDEX(12)=1                                                   157
      RMAT_RINDEX(13)=1                                                   157
      RMAT_RINDEX(14)=1                                                   157
      RMAT_RINDEX(15)=1                                                   157
      RMAT_RINDEX(16)=1                                                   157
      RMAT_RINDEX(17)=1                                                   157
      RMAT_RINDEX(18)=1                                                   157
      RMAT_RINDEX(19)=1                                                   157
      RMAT_RINDEX(20)=1                                                   157
      RMAT_RINDEX(21)=1                                                   157
      RMAT_RINDEX(22)=1                                                   157
      RMAT_RINDEX(23)=1                                                   157
      RMAT_RINDEX(24)=1                                                   157
      RMAT_RINDEX(25)=1                                                   157
      RMAT_RINDEX(26)=1                                                   157
      CRMAT_RINDEX='REFRACTION INDEX'                                     157
C ABSORBTION LENGHTS (IN CM)                                              161
      RMAT_ABSCO(1)=1.E-4                                                 161
      RMAT_ABSCO(2)=1.E-4                                                 161
      RMAT_ABSCO(3)=1.E-4                                                 161
      RMAT_ABSCO(4)=1.E-4                                                 161
      RMAT_ABSCO(5)=1.E-4                                                 161
      RMAT_ABSCO(6)=1.E-4                                                 161
      RMAT_ABSCO(7)=1.E-4                                                 161
      RMAT_ABSCO(8)=1.E-4                                                 161
      RMAT_ABSCO(9)=1.E-4                                                 161
      RMAT_ABSCO(10)=1.E-4                                                161
      RMAT_ABSCO(11)=1.E-4                                                161
      RMAT_ABSCO(12)=1.E-4                                                161
      RMAT_ABSCO(13)=1.E-4                                                161
      RMAT_ABSCO(14)=1.E-4                                                161
      RMAT_ABSCO(15)=1.E-4                                                161
      RMAT_ABSCO(16)=1.E-4                                                161
      RMAT_ABSCO(17)=1.E-4                                                161
      RMAT_ABSCO(18)=1.E-4                                                161
      RMAT_ABSCO(19)=1.E-4                                                161
      RMAT_ABSCO(20)=1.E-4                                                161
      RMAT_ABSCO(21)=1.E-4                                                161
      RMAT_ABSCO(22)=1.E-4                                                161
      RMAT_ABSCO(23)=1.E-4                                                161
      RMAT_ABSCO(24)=1.E-4                                                161
      RMAT_ABSCO(25)=1.E-4                                                161
      RMAT_ABSCO(26)=1.E-4                                                161
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            161
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    163
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           163
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   163
C *                                                                       163
C *                                                                       163
      CALL AGSBEGM(MODULE,IPRIN)                                          163
      IF (IPRIN.LT.0) RETURN                                              163
C Filling bank RANG(2)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        163
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   163
      BPATH_RMAT='RANG/RMAT*'                                             163
      IDNUM_RMAT(1)=2                                                     164
      IDNUM_RMAT(2)=0                                                     164
C OPAQUE QUARZ                                                            164
      RMAT_MATE='QRZO'                                                    164
      CRMAT_MATE='OPAQUE QUARZ'                                           164
C DETECTION EFFICIENCIES                                                  166
      RMAT_EFFIC(1)=1                                                     166
      RMAT_EFFIC(2)=1                                                     166
      RMAT_EFFIC(3)=1                                                     166
      RMAT_EFFIC(4)=1                                                     166
      RMAT_EFFIC(5)=1                                                     166
      RMAT_EFFIC(6)=1                                                     166
      RMAT_EFFIC(7)=1                                                     166
      RMAT_EFFIC(8)=1                                                     166
      RMAT_EFFIC(9)=1                                                     166
      RMAT_EFFIC(10)=1                                                    166
      RMAT_EFFIC(11)=1                                                    166
      RMAT_EFFIC(12)=1                                                    166
      RMAT_EFFIC(13)=1                                                    166
      RMAT_EFFIC(14)=1                                                    166
      RMAT_EFFIC(15)=1                                                    166
      RMAT_EFFIC(16)=1                                                    166
      RMAT_EFFIC(17)=1                                                    166
      RMAT_EFFIC(18)=1                                                    166
      RMAT_EFFIC(19)=1                                                    166
      RMAT_EFFIC(20)=1                                                    166
      RMAT_EFFIC(21)=1                                                    166
      RMAT_EFFIC(22)=1                                                    166
      RMAT_EFFIC(23)=1                                                    166
      RMAT_EFFIC(24)=1                                                    166
      RMAT_EFFIC(25)=1                                                    166
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                166
C REFRACTION INDEX                                                        168
      RMAT_RINDEX(1)=1                                                    168
      RMAT_RINDEX(2)=1                                                    168
      RMAT_RINDEX(3)=1                                                    168
      RMAT_RINDEX(4)=1                                                    168
      RMAT_RINDEX(5)=1                                                    168
      RMAT_RINDEX(6)=1                                                    168
      RMAT_RINDEX(7)=1                                                    168
      RMAT_RINDEX(8)=1                                                    168
      RMAT_RINDEX(9)=1                                                    168
      RMAT_RINDEX(10)=1                                                   168
      RMAT_RINDEX(11)=1                                                   168
      RMAT_RINDEX(12)=1                                                   168
      RMAT_RINDEX(13)=1                                                   168
      RMAT_RINDEX(14)=1                                                   168
      RMAT_RINDEX(15)=1                                                   168
      RMAT_RINDEX(16)=1                                                   168
      RMAT_RINDEX(17)=1                                                   168
      RMAT_RINDEX(18)=1                                                   168
      RMAT_RINDEX(19)=1                                                   168
      RMAT_RINDEX(20)=1                                                   168
      RMAT_RINDEX(21)=1                                                   168
      RMAT_RINDEX(22)=1                                                   168
      RMAT_RINDEX(23)=1                                                   168
      RMAT_RINDEX(24)=1                                                   168
      RMAT_RINDEX(25)=1                                                   168
      CRMAT_RINDEX='REFRACTION INDEX'                                     168
C ABSORBTION LENGHTS (IN CM)                                              172
      RMAT_ABSCO(1)=1.E-5                                                 172
      RMAT_ABSCO(2)=1.E-5                                                 172
      RMAT_ABSCO(3)=1.E-5                                                 172
      RMAT_ABSCO(4)=1.E-5                                                 172
      RMAT_ABSCO(5)=1.E-5                                                 172
      RMAT_ABSCO(6)=1.E-5                                                 172
      RMAT_ABSCO(7)=1.E-5                                                 172
      RMAT_ABSCO(8)=1.E-5                                                 172
      RMAT_ABSCO(9)=1.E-5                                                 172
      RMAT_ABSCO(10)=1.E-5                                                172
      RMAT_ABSCO(11)=1.E-5                                                172
      RMAT_ABSCO(12)=1.E-5                                                172
      RMAT_ABSCO(13)=1.E-5                                                172
      RMAT_ABSCO(14)=1.E-5                                                172
      RMAT_ABSCO(15)=1.E-5                                                172
      RMAT_ABSCO(16)=1.E-5                                                172
      RMAT_ABSCO(17)=1.E-5                                                172
      RMAT_ABSCO(18)=1.E-5                                                172
      RMAT_ABSCO(19)=1.E-5                                                172
      RMAT_ABSCO(20)=1.E-5                                                172
      RMAT_ABSCO(21)=1.E-5                                                172
      RMAT_ABSCO(22)=1.E-5                                                172
      RMAT_ABSCO(23)=1.E-5                                                172
      RMAT_ABSCO(24)=1.E-5                                                172
      RMAT_ABSCO(25)=1.E-5                                                172
      RMAT_ABSCO(26)=1.E-5                                                172
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            172
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    174
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           174
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   174
C *                                                                       174
C *                                                                       174
      CALL AGSBEGM(MODULE,IPRIN)                                          174
      IF (IPRIN.LT.0) RETURN                                              174
C Filling bank RANG(2)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        174
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   174
      BPATH_RMAT='RANG/RMAT*'                                             174
      IDNUM_RMAT(1)=2                                                     175
      IDNUM_RMAT(2)=0                                                     175
C MATERIAL - METHANE                                                      175
      RMAT_MATE='META'                                                    175
      CRMAT_MATE='MATERIAL - METHANE'                                     175
C DETECTION EFFICIENCIES                                                  177
      RMAT_EFFIC(1)=1                                                     177
      RMAT_EFFIC(2)=1                                                     177
      RMAT_EFFIC(3)=1                                                     177
      RMAT_EFFIC(4)=1                                                     177
      RMAT_EFFIC(5)=1                                                     177
      RMAT_EFFIC(6)=1                                                     177
      RMAT_EFFIC(7)=1                                                     177
      RMAT_EFFIC(8)=1                                                     177
      RMAT_EFFIC(9)=1                                                     177
      RMAT_EFFIC(10)=1                                                    177
      RMAT_EFFIC(11)=1                                                    177
      RMAT_EFFIC(12)=1                                                    177
      RMAT_EFFIC(13)=1                                                    177
      RMAT_EFFIC(14)=1                                                    177
      RMAT_EFFIC(15)=1                                                    177
      RMAT_EFFIC(16)=1                                                    177
      RMAT_EFFIC(17)=1                                                    177
      RMAT_EFFIC(18)=1                                                    177
      RMAT_EFFIC(19)=1                                                    177
      RMAT_EFFIC(20)=1                                                    177
      RMAT_EFFIC(21)=1                                                    177
      RMAT_EFFIC(22)=1                                                    177
      RMAT_EFFIC(23)=1                                                    177
      RMAT_EFFIC(24)=1                                                    177
      RMAT_EFFIC(25)=1                                                    177
      RMAT_EFFIC(26)=1                                                    177
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                177
C REFRACTION INDEX                                                        182
      RMAT_RINDEX(1)=1.000444                                             182
      RMAT_RINDEX(2)=1.000444                                             182
      RMAT_RINDEX(3)=1.000444                                             182
      RMAT_RINDEX(4)=1.000444                                             182
      RMAT_RINDEX(5)=1.000444                                             182
      RMAT_RINDEX(6)=1.000444                                             182
      RMAT_RINDEX(7)=1.000444                                             182
      RMAT_RINDEX(8)=1.000444                                             182
      RMAT_RINDEX(9)=1.000444                                             182
      RMAT_RINDEX(10)=1.000444                                            182
      RMAT_RINDEX(11)=1.000444                                            182
      RMAT_RINDEX(12)=1.000444                                            182
      RMAT_RINDEX(13)=1.000444                                            182
      RMAT_RINDEX(14)=1.000444                                            182
      RMAT_RINDEX(15)=1.000444                                            182
      RMAT_RINDEX(16)=1.000444                                            182
      RMAT_RINDEX(17)=1.000444                                            182
      RMAT_RINDEX(18)=1.000444                                            182
      RMAT_RINDEX(19)=1.000444                                            182
      RMAT_RINDEX(20)=1.000444                                            182
      RMAT_RINDEX(21)=1.000444                                            182
      RMAT_RINDEX(22)=1.000444                                            182
      RMAT_RINDEX(23)=1.000444                                            182
      RMAT_RINDEX(24)=1.000444                                            182
      RMAT_RINDEX(25)=1.000444                                            182
      RMAT_RINDEX(26)=1.000444                                            182
      CRMAT_RINDEX='REFRACTION INDEX'                                     182
C ABSORBTION LENGHTS (IN CM)                                              186
      RMAT_ABSCO(1)=1.E6                                                  186
      RMAT_ABSCO(2)=1.E6                                                  186
      RMAT_ABSCO(3)=1.E6                                                  186
      RMAT_ABSCO(4)=1.E6                                                  186
      RMAT_ABSCO(5)=1.E6                                                  186
      RMAT_ABSCO(6)=1.E6                                                  186
      RMAT_ABSCO(7)=1.E6                                                  186
      RMAT_ABSCO(8)=1.E6                                                  186
      RMAT_ABSCO(9)=1.E6                                                  186
      RMAT_ABSCO(10)=1.E6                                                 186
      RMAT_ABSCO(11)=1.E6                                                 186
      RMAT_ABSCO(12)=1.E6                                                 186
      RMAT_ABSCO(13)=1.E6                                                 186
      RMAT_ABSCO(14)=1.E6                                                 186
      RMAT_ABSCO(15)=1.E6                                                 186
      RMAT_ABSCO(16)=1.E6                                                 186
      RMAT_ABSCO(17)=1.E6                                                 186
      RMAT_ABSCO(18)=1.E6                                                 186
      RMAT_ABSCO(19)=1.E6                                                 186
      RMAT_ABSCO(20)=1.E6                                                 186
      RMAT_ABSCO(21)=1.E6                                                 186
      RMAT_ABSCO(22)=1.E6                                                 186
      RMAT_ABSCO(23)=1.E6                                                 186
      RMAT_ABSCO(24)=1.E6                                                 186
      RMAT_ABSCO(25)=1.E6                                                 186
      RMAT_ABSCO(26)=1.E6                                                 186
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            186
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    188
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           188
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   188
C *                                                                       188
C *                                                                       188
      CALL AGSBEGM(MODULE,IPRIN)                                          188
      IF (IPRIN.LT.0) RETURN                                              188
C Filling bank RANG(2)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        188
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   188
      BPATH_RMAT='RANG/RMAT*'                                             188
      IDNUM_RMAT(1)=2                                                     189
      IDNUM_RMAT(2)=0                                                     189
C MATERIAL - QUARTZ                                                       189
      RMAT_MATE='QARZ'                                                    189
      CRMAT_MATE='MATERIAL - QUARTZ'                                      189
C DETECTION EFFICIENCIES                                                  191
      RMAT_EFFIC(1)=1                                                     191
      RMAT_EFFIC(2)=1                                                     191
      RMAT_EFFIC(3)=1                                                     191
      RMAT_EFFIC(4)=1                                                     191
      RMAT_EFFIC(5)=1                                                     191
      RMAT_EFFIC(6)=1                                                     191
      RMAT_EFFIC(7)=1                                                     191
      RMAT_EFFIC(8)=1                                                     191
      RMAT_EFFIC(9)=1                                                     191
      RMAT_EFFIC(10)=1                                                    191
      RMAT_EFFIC(11)=1                                                    191
      RMAT_EFFIC(12)=1                                                    191
      RMAT_EFFIC(13)=1                                                    191
      RMAT_EFFIC(14)=1                                                    191
      RMAT_EFFIC(15)=1                                                    191
      RMAT_EFFIC(16)=1                                                    191
      RMAT_EFFIC(17)=1                                                    191
      RMAT_EFFIC(18)=1                                                    191
      RMAT_EFFIC(19)=1                                                    191
      RMAT_EFFIC(20)=1                                                    191
      RMAT_EFFIC(21)=1                                                    191
      RMAT_EFFIC(22)=1                                                    191
      RMAT_EFFIC(23)=1                                                    191
      RMAT_EFFIC(24)=1                                                    191
      RMAT_EFFIC(25)=1                                                    191
      RMAT_EFFIC(26)=1                                                    191
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                191
C REFRACTION INDEX                                                        197
      RMAT_RINDEX(1)=1.52398                                              197
      RMAT_RINDEX(2)=1.52738                                              197
      RMAT_RINDEX(3)=1.5309                                               197
      RMAT_RINDEX(4)=1.5346                                               197
      RMAT_RINDEX(5)=1.53835                                              197
      RMAT_RINDEX(6)=1.542302                                             197
      RMAT_RINDEX(7)=1.54641                                              197
      RMAT_RINDEX(8)=1.55067                                              197
      RMAT_RINDEX(9)=1.55513                                              197
      RMAT_RINDEX(10)=1.55976                                             197
      RMAT_RINDEX(11)=1.56458                                             197
      RMAT_RINDEX(12)=1.56962                                             197
      RMAT_RINDEX(13)=1.57488                                             197
      RMAT_RINDEX(14)=1.58037                                             197
      RMAT_RINDEX(15)=1.58611                                             197
      RMAT_RINDEX(16)=1.59212                                             197
      RMAT_RINDEX(17)=1.59842                                             197
      RMAT_RINDEX(18)=1.60503                                             197
      RMAT_RINDEX(19)=1.61197                                             197
      RMAT_RINDEX(20)=1.61927                                             197
      RMAT_RINDEX(21)=1.62696                                             197
      RMAT_RINDEX(22)=1.63506                                             197
      RMAT_RINDEX(23)=1.64362                                             197
      RMAT_RINDEX(24)=1.65269                                             197
      RMAT_RINDEX(25)=1.662295                                            197
      RMAT_RINDEX(26)=1.6725                                              197
      CRMAT_RINDEX='REFRACTION INDEX'                                     197
C ABSORBTION LENGHTS (IN CM)                                              203
      RMAT_ABSCO(1)=105.8                                                 203
      RMAT_ABSCO(2)=65.52                                                 203
      RMAT_ABSCO(3)=48.58                                                 203
      RMAT_ABSCO(4)=42.85                                                 203
      RMAT_ABSCO(5)=35.79                                                 203
      RMAT_ABSCO(6)=31.262                                                203
      RMAT_ABSCO(7)=28.598                                                203
      RMAT_ABSCO(8)=27.527                                                203
      RMAT_ABSCO(9)=25.007                                                203
      RMAT_ABSCO(10)=22.815                                               203
      RMAT_ABSCO(11)=21.04                                                203
      RMAT_ABSCO(12)=19.266                                               203
      RMAT_ABSCO(13)=17.525                                               203
      RMAT_ABSCO(14)=15.878                                               203
      RMAT_ABSCO(15)=14.177                                               203
      RMAT_ABSCO(16)=11.719                                               203
      RMAT_ABSCO(17)=9.282                                                203
      RMAT_ABSCO(18)=6.62                                                 203
      RMAT_ABSCO(19)=4.0925                                               203
      RMAT_ABSCO(20)=2.601                                                203
      RMAT_ABSCO(21)=1.149                                                203
      RMAT_ABSCO(22)=0.667                                                203
      RMAT_ABSCO(23)=0.3627                                               203
      RMAT_ABSCO(24)=0.192                                                203
      RMAT_ABSCO(25)=0.1497                                               203
      RMAT_ABSCO(26)=0.10857                                              203
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            203
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    205
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           205
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   205
C *                                                                       205
C *                                                                       205
      CALL AGSBEGM(MODULE,IPRIN)                                          205
      IF (IPRIN.LT.0) RETURN                                              205
C Filling bank RANG(2)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        205
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   205
      BPATH_RMAT='RANG/RMAT*'                                             205
      IDNUM_RMAT(1)=2                                                     206
      IDNUM_RMAT(2)=0                                                     206
C MATERIAL                                                                206
      RMAT_MATE='FREO'                                                    206
      CRMAT_MATE='MATERIAL'                                               206
C DETECTION EFFICIENCIES                                                  208
      RMAT_EFFIC(1)=1                                                     208
      RMAT_EFFIC(2)=1                                                     208
      RMAT_EFFIC(3)=1                                                     208
      RMAT_EFFIC(4)=1                                                     208
      RMAT_EFFIC(5)=1                                                     208
      RMAT_EFFIC(6)=1                                                     208
      RMAT_EFFIC(7)=1                                                     208
      RMAT_EFFIC(8)=1                                                     208
      RMAT_EFFIC(9)=1                                                     208
      RMAT_EFFIC(10)=1                                                    208
      RMAT_EFFIC(11)=1                                                    208
      RMAT_EFFIC(12)=1                                                    208
      RMAT_EFFIC(13)=1                                                    208
      RMAT_EFFIC(14)=1                                                    208
      RMAT_EFFIC(15)=1                                                    208
      RMAT_EFFIC(16)=1                                                    208
      RMAT_EFFIC(17)=1                                                    208
      RMAT_EFFIC(18)=1                                                    208
      RMAT_EFFIC(19)=1                                                    208
      RMAT_EFFIC(20)=1                                                    208
      RMAT_EFFIC(21)=1                                                    208
      RMAT_EFFIC(22)=1                                                    208
      RMAT_EFFIC(23)=1                                                    208
      RMAT_EFFIC(24)=1                                                    208
      RMAT_EFFIC(25)=1                                                    208
      RMAT_EFFIC(26)=1                                                    208
      CRMAT_EFFIC='DETECTION EFFICIENCIES'                                208
C REFRACTION INDEX                                                        214
      RMAT_RINDEX(1)=1.2716                                               214
      RMAT_RINDEX(2)=1.27332                                              214
      RMAT_RINDEX(3)=1.27504                                              214
      RMAT_RINDEX(4)=1.27676                                              214
      RMAT_RINDEX(5)=1.27848                                              214
      RMAT_RINDEX(6)=1.2802                                               214
      RMAT_RINDEX(7)=1.28192                                              214
      RMAT_RINDEX(8)=1.28364                                              214
      RMAT_RINDEX(9)=1.28536                                              214
      RMAT_RINDEX(10)=1.28708                                             214
      RMAT_RINDEX(11)=1.2888                                              214
      RMAT_RINDEX(12)=1.29052                                             214
      RMAT_RINDEX(13)=1.29224                                             214
      RMAT_RINDEX(14)=1.29396                                             214
      RMAT_RINDEX(15)=1.29568                                             214
      RMAT_RINDEX(16)=1.2974                                              214
      RMAT_RINDEX(17)=1.29912                                             214
      RMAT_RINDEX(18)=1.30084                                             214
      RMAT_RINDEX(19)=1.30256                                             214
      RMAT_RINDEX(20)=1.30428                                             214
      RMAT_RINDEX(21)=1.306                                               214
      RMAT_RINDEX(22)=1.30772                                             214
      RMAT_RINDEX(23)=1.30944                                             214
      RMAT_RINDEX(24)=1.31116                                             214
      RMAT_RINDEX(25)=1.31288                                             214
      RMAT_RINDEX(26)=1.3146                                              214
      CRMAT_RINDEX='REFRACTION INDEX'                                     214
C ABSORBTION LENGHTS (IN CM)                                              220
      RMAT_ABSCO(1)=176.4219                                              220
      RMAT_ABSCO(2)=176.4219                                              220
      RMAT_ABSCO(3)=176.4219                                              220
      RMAT_ABSCO(4)=176.4219                                              220
      RMAT_ABSCO(5)=176.4219                                              220
      RMAT_ABSCO(6)=176.4219                                              220
      RMAT_ABSCO(7)=176.4219                                              220
      RMAT_ABSCO(8)=176.4219                                              220
      RMAT_ABSCO(9)=176.4219                                              220
      RMAT_ABSCO(10)=176.4219                                             220
      RMAT_ABSCO(11)=176.4219                                             220
      RMAT_ABSCO(12)=176.4219                                             220
      RMAT_ABSCO(13)=176.4219                                             220
      RMAT_ABSCO(14)=176.4219                                             220
      RMAT_ABSCO(15)=58.322                                               220
      RMAT_ABSCO(16)=41.6371                                              220
      RMAT_ABSCO(17)=22.5158                                              220
      RMAT_ABSCO(18)=10.9767                                              220
      RMAT_ABSCO(19)=4.01237                                              220
      RMAT_ABSCO(20)=1.65053                                              220
      RMAT_ABSCO(21)=0.783613                                             220
      RMAT_ABSCO(22)=0.452046                                             220
      RMAT_ABSCO(23)=0.312553                                             220
      RMAT_ABSCO(24)=0.15627                                              220
      RMAT_ABSCO(25)=0.0                                                  220
      RMAT_ABSCO(26)=0.0                                                  220
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            220
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    222
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           222
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   222
C *                                                                       222
C *                                                                       222
      CALL AGSBEGM(MODULE,IPRIN)                                          222
      IF (IPRIN.LT.0) RETURN                                              222
C Filling bank RANG(2)/RMAT - MATERIAL PROPERTY FOR CERENKOV LIGHT        222
      BANK_TITLE='MATERIAL PROPERTY FOR CERENKOV LIGHT'                   222
      BPATH_RMAT='RANG/RMAT*'                                             222
      IDNUM_RMAT(1)=2                                                     223
      IDNUM_RMAT(2)=0                                                     223
C MATERIAL - CESIUM IODIDE                                                223
      RMAT_MATE='CSI'                                                     223
      CRMAT_MATE='MATERIAL - CESIUM IODIDE'                               223
C QUANTUM EFFICIENCIES                                                    229
      RMAT_EFFIC(1)=0.0002                                                229
      RMAT_EFFIC(2)=0.0006                                                229
      RMAT_EFFIC(3)=0.0007                                                229
      RMAT_EFFIC(4)=0.005                                                 229
      RMAT_EFFIC(5)=0.0075                                                229
      RMAT_EFFIC(6)=0.010125                                              229
      RMAT_EFFIC(7)=0.0243                                                229
      RMAT_EFFIC(8)=0.0405                                                229
      RMAT_EFFIC(9)=0.06885                                               229
      RMAT_EFFIC(10)=0.1053                                               229
      RMAT_EFFIC(11)=0.1215                                               229
      RMAT_EFFIC(12)=0.1435                                               229
      RMAT_EFFIC(13)=0.16                                                 229
      RMAT_EFFIC(14)=0.164                                                229
      RMAT_EFFIC(15)=0.1681                                               229
      RMAT_EFFIC(16)=0.17                                                 229
      RMAT_EFFIC(17)=0.1785                                               229
      RMAT_EFFIC(18)=0.1811                                               229
      RMAT_EFFIC(19)=0.1836                                               229
      RMAT_EFFIC(20)=0.187                                                229
      RMAT_EFFIC(21)=0.1904                                               229
      RMAT_EFFIC(22)=0.1921                                               229
      RMAT_EFFIC(23)=0.1938                                               229
      RMAT_EFFIC(24)=0.1955                                               229
      RMAT_EFFIC(25)=0.2125                                               229
      RMAT_EFFIC(26)=0.221                                                229
      CRMAT_EFFIC='QUANTUM EFFICIENCIES'                                  229
C REFRACTION INDEX                                                        234
      RMAT_RINDEX(1)=1.000444                                             234
      RMAT_RINDEX(2)=1.000444                                             234
      RMAT_RINDEX(3)=1.000444                                             234
      RMAT_RINDEX(4)=1.000444                                             234
      RMAT_RINDEX(5)=1.000444                                             234
      RMAT_RINDEX(6)=1.000444                                             234
      RMAT_RINDEX(7)=1.000444                                             234
      RMAT_RINDEX(8)=1.000444                                             234
      RMAT_RINDEX(9)=1.000444                                             234
      RMAT_RINDEX(10)=1.000444                                            234
      RMAT_RINDEX(11)=1.000444                                            234
      RMAT_RINDEX(12)=1.000444                                            234
      RMAT_RINDEX(13)=1.000444                                            234
      RMAT_RINDEX(14)=1.000444                                            234
      RMAT_RINDEX(15)=1.000444                                            234
      RMAT_RINDEX(16)=1.000444                                            234
      RMAT_RINDEX(17)=1.000444                                            234
      RMAT_RINDEX(18)=1.000444                                            234
      RMAT_RINDEX(19)=1.000444                                            234
      RMAT_RINDEX(20)=1.000444                                            234
      RMAT_RINDEX(21)=1.000444                                            234
      RMAT_RINDEX(22)=1.000444                                            234
      RMAT_RINDEX(23)=1.000444                                            234
      RMAT_RINDEX(24)=1.000444                                            234
      RMAT_RINDEX(25)=1.000444                                            234
      RMAT_RINDEX(26)=1.000444                                            234
      CRMAT_RINDEX='REFRACTION INDEX'                                     234
C ABSORBTION LENGHTS (IN CM)                                              238
      RMAT_ABSCO(1)=1.E-4                                                 238
      RMAT_ABSCO(2)=1.E-4                                                 238
      RMAT_ABSCO(3)=1.E-4                                                 238
      RMAT_ABSCO(4)=1.E-4                                                 238
      RMAT_ABSCO(5)=1.E-4                                                 238
      RMAT_ABSCO(6)=1.E-4                                                 238
      RMAT_ABSCO(7)=1.E-4                                                 238
      RMAT_ABSCO(8)=1.E-4                                                 238
      RMAT_ABSCO(9)=1.E-4                                                 238
      RMAT_ABSCO(10)=1.E-4                                                238
      RMAT_ABSCO(11)=1.E-4                                                238
      RMAT_ABSCO(12)=1.E-4                                                238
      RMAT_ABSCO(13)=1.E-4                                                238
      RMAT_ABSCO(14)=1.E-4                                                238
      RMAT_ABSCO(15)=1.E-4                                                238
      RMAT_ABSCO(16)=1.E-4                                                238
      RMAT_ABSCO(17)=1.E-4                                                238
      RMAT_ABSCO(18)=1.E-4                                                238
      RMAT_ABSCO(19)=1.E-4                                                238
      RMAT_ABSCO(20)=1.E-4                                                238
      RMAT_ABSCO(21)=1.E-4                                                238
      RMAT_ABSCO(22)=1.E-4                                                238
      RMAT_ABSCO(23)=1.E-4                                                238
      RMAT_ABSCO(24)=1.E-4                                                238
      RMAT_ABSCO(25)=1.E-4                                                238
      RMAT_ABSCO(26)=1.E-4                                                238
      CRMAT_ABSCO='ABSORBTION LENGHTS (IN CM)'                            238
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    239
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           239
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   239
C *                                                                       239
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RMAT',    239
     *BPATH_RMAT,IDNUM_RMAT,2, MAP_RMAT,NAMES_RMAT,COMMEN_RMAT,           239
     *BANK_RMAT,LEN_RMAT,LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                   239
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL AGSSTEP(RICHSTEP)                                              243
C use default RICH bank                                                   245
      BPATH_RICH='RICH*'                                                  245
      IDNUM_RICH(1)=0                                                     245
      CALL AGDGETP('RICHGEO','*',-999,'RICH',' ',BANK_RICH, BPATH_RICH,   245
     *IDNUM_RICH,1,MAP_RICH,NAMES_RICH,BANK_RICH,LEN_RICH,LLEN_RICH,      245
     *LINK_RICH,FLAG_RICH)                                                245
      ISOPT=0                                                             246
C Check RICH_VERSION>1                                                    247
      IF (RICH_VERSION.GT.1) ISOPT=1                                      247
* select a DB parameter version and make this bank unique
C use RPOS bank                                                           251
      BPATH_RPOS='RPOS*'                                                  251
      IDNUM_RPOS(1)=0                                                     251
      RPOS_POSITION=RICH_POSITION                                         251
      CALL AGDGETP('RICHGEO','U',-999,'RPOS',NRPOS_POSITION,              251
     *RE_RPOS_POSITION,BPATH_RPOS,IDNUM_RPOS,1,MAP_RPOS,NAMES_RPOS,       251
     *BANK_RPOS,LEN_RPOS,LLEN_RPOS,LINK_RPOS,FLAG_RPOS)                   251
C use RANG bank                                                           252
      BPATH_RANG='RANG*'                                                  252
      IDNUM_RANG(1)=0                                                     252
      RANG_VERSION=RICH_CVERSION                                          252
      CALL AGDGETP('RICHGEO','U',-999,'RANG',NRANG_VERSION,               252
     *RE_RANG_VERSION,BPATH_RANG,IDNUM_RANG,1,MAP_RANG,NAMES_RANG,        252
     *BANK_RANG,LEN_RANG,LLEN_RANG,LINK_RANG,FLAG_RANG)                   252
* make selected RANG Current Working Directory
C use default /DETM/RICH/RICH/RANG bank                                   256
      BPATH_RANG='/DETM/RICH/RICH/RANG*'                                  256
      IDNUM_RANG(1)=1                                                     256
      IDNUM_RANG(2)=1                                                     256
      IDNUM_RANG(3)=1                                                     256
      IDNUM_RANG(4)=0                                                     256
      CALL AGDGETP('RICHGEO','*',-999,'RANG',' ',BANK_RANG, BPATH_RANG,   256
     *IDNUM_RANG,4,MAP_RANG,NAMES_RANG,BANK_RANG,LEN_RANG,LLEN_RANG,      256
     *LINK_RANG,FLAG_RANG)                                                256
      N = RANG_LENGTH                                                     257
*   special material definition here:
C Geant COMPONENT : O ===> A=16.00 Z=8 W=2 <===                           260
      CALL AGSRESET                                                       260
      AG_A = 16.00                                                        260
      AG_Z = 8                                                            260
      AG_W = 2                                                            260
      AG_TITLE = 'O'                                                      260
      AG_EXNAME = 'COMPONENT'                                             260
      AG_PARLIST = 'A_Z_W'                                                260
      CALL AXCOMPONENT                                                    260
C *                                                                       260
C Geant COMPONENT : SI ===> A=28.09 Z=14 W=1 <===                         260
      CALL AGSRESET                                                       261
      AG_A = 28.09                                                        261
      AG_Z = 14                                                           261
      AG_W = 1                                                            261
      AG_TITLE = 'SI'                                                     261
      AG_EXNAME = 'COMPONENT'                                             261
      AG_PARLIST = 'A_Z_W'                                                261
      CALL AXCOMPONENT                                                    261
C *                                                                       261
C Geant MIXTURE : QUARZ ===> DENS=2.64 <===                               261
      CALL AGSRESET                                                       262
      AG_DENS = 2.64                                                      262
      AG_TITLE = 'QUARZ'                                                  262
      AG_EXNAME = 'MIXTURE'                                               262
      AG_PARLIST = 'DENS'                                                 262
      CALL AXMIXTURE                                                      262
C *                                                                       262
C Geant COMPONENT : O ===> A=16.00 Z=8 W=2 <===                           264
      CALL AGSRESET                                                       264
      AG_A = 16.00                                                        264
      AG_Z = 8                                                            264
      AG_W = 2                                                            264
      AG_TITLE = 'O'                                                      264
      AG_EXNAME = 'COMPONENT'                                             264
      AG_PARLIST = 'A_Z_W'                                                264
      CALL AXCOMPONENT                                                    264
C *                                                                       264
C Geant COMPONENT : SI ===> A=28.09 Z=14 W=1 <===                         264
      CALL AGSRESET                                                       265
      AG_A = 28.09                                                        265
      AG_Z = 14                                                           265
      AG_W = 1                                                            265
      AG_TITLE = 'SI'                                                     265
      AG_EXNAME = 'COMPONENT'                                             265
      AG_PARLIST = 'A_Z_W'                                                265
      CALL AXCOMPONENT                                                    265
C *                                                                       265
C Geant MIXTURE : OPACO ===> DENS=2.64 <===                               265
      CALL AGSRESET                                                       266
      AG_DENS = 2.64                                                      266
      AG_TITLE = 'OPACO'                                                  266
      AG_EXNAME = 'MIXTURE'                                               266
      AG_PARLIST = 'DENS'                                                 266
      CALL AXMIXTURE                                                      266
C *                                                                       266
*
C Geant COMPONENT : C ===> A=12.01 Z=6 W=1 <===                           268
      CALL AGSRESET                                                       268
      AG_A = 12.01                                                        268
      AG_Z = 6                                                            268
      AG_W = 1                                                            268
      AG_TITLE = 'C'                                                      268
      AG_EXNAME = 'COMPONENT'                                             268
      AG_PARLIST = 'A_Z_W'                                                268
      CALL AXCOMPONENT                                                    268
C *                                                                       268
C Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                               268
      CALL AGSRESET                                                       269
      AG_A = 1                                                            269
      AG_Z = 1                                                            269
      AG_W = 4                                                            269
      AG_TITLE = 'H'                                                      269
      AG_EXNAME = 'COMPONENT'                                             269
      AG_PARLIST = 'A_Z_W'                                                269
      CALL AXCOMPONENT                                                    269
C *                                                                       269
C Geant MIXTURE : METHANE ===> DENS=0.000717 <===                         269
      CALL AGSRESET                                                       270
      AG_DENS = 0.000717                                                  270
      AG_TITLE = 'METHANE'                                                270
      AG_EXNAME = 'MIXTURE'                                               270
      AG_PARLIST = 'DENS'                                                 270
      CALL AXMIXTURE                                                      270
C *                                                                       270
*
C Geant MATERIAL : CARBON ===> inherited arguments used <===              272
      CALL AGSRESET                                                       272
      AG_TITLE = 'CARBON'                                                 272
      AG_EXNAME = 'MATERIAL'                                              272
      AG_PARLIST = 'NONE'                                                 272
      CALL AXMATERIAL                                                     272
C *                                                                       272
C Geant MATERIAL : CARBONIO ===> RADL=0.376 DENS=2.265 <===               272
      CALL AGSRESET                                                       273
      AG_RADL = 0.376                                                     273
      AG_DENS = 2.265                                                     273
      AG_TITLE = 'CARBONIO'                                               273
      AG_EXNAME = 'MATERIAL'                                              273
      AG_PARLIST = 'RADL_DENS'                                            273
      CALL AXMATERIAL                                                     273
C *                                                                       273
*   Material  Carbonio      Radl=18.8    Dens=2.265
*
C Geant COMPONENT : CU ===> A=63.540 Z=29 W=1 <===                        276
      CALL AGSRESET                                                       276
      AG_A = 63.540                                                       276
      AG_Z = 29                                                           276
      AG_W = 1                                                            276
      AG_TITLE = 'CU'                                                     276
      AG_EXNAME = 'COMPONENT'                                             276
      AG_PARLIST = 'A_Z_W'                                                276
      CALL AXCOMPONENT                                                    276
C *                                                                       276
C Geant MIXTURE : ANODE ===> RADL=1.43 DENS=8.96 <===                     276
      CALL AGSRESET                                                       277
      AG_RADL = 1.43                                                      277
      AG_DENS = 8.96                                                      277
      AG_TITLE = 'ANODE'                                                  277
      AG_EXNAME = 'MIXTURE'                                               277
      AG_PARLIST = 'RADL_DENS'                                            277
      CALL AXMIXTURE                                                      277
C *                                                                       277
*
C Geant MATERIAL : AIR ===> inherited arguments used <===                 279
      CALL AGSRESET                                                       279
      AG_TITLE = 'AIR'                                                    279
      AG_EXNAME = 'MATERIAL'                                              279
      AG_PARLIST = 'NONE'                                                 279
      CALL AXMATERIAL                                                     279
C *                                                                       279
C Geant MEDIUM : STANDARD ===> inherited arguments used <===              279
      CALL AGSRESET                                                       280
      AG_TITLE = 'STANDARD'                                               280
      AG_EXNAME = 'MEDIUM'                                                280
      AG_PARLIST = 'NONE'                                                 280
      CALL AXMEDIUM                                                       280
C *                                                                       280
* define RICH position using reconstructed Euler angles
* thanks to James Dunlop
      TX = DEGRAD*RPOS_TX                                                 285
      TY = DEGRAD*RPOS_TY                                                 286
      TZ = DEGRAD*RPOS_TZ                                                 287
      ROT(1,1) = -COS(TZ)*COS(TY)                                         289
      ROT(2,1) = -SIN(TZ)*COS(TY)                                         290
      ROT(3,1) = SIN(TY)                                                  291
      ROT(1,2) = -COS(TZ)*SIN(TY)*SIN(TX) + SIN(TZ)*COS(TX)               293
      ROT(2,2) = -SIN(TZ)*SIN(TY)*SIN(TX) - COS(TZ)*COS(TX)               294
      ROT(3,2) = -COS(TY)*SIN(TX)                                         295
      ROT(1,3) = COS(TZ)*SIN(TY)*COS(TX) + SIN(TZ)*SIN(TX)                297
      ROT(2,3) = SIN(TZ)*SIN(TY)*COS(TX) - COS(TZ)*SIN(TX)                298
      ROT(3,3) = COS(TY)*COS(TX)                                          299
      CALL AGSBEGM(MODULE,IPRIN)                                          307
C Check IPRIN<0                                                           307
      IF (IPRIN.LT.0) RETURN                                              307
C Create RICH :                                                           307
C Call To Local Procedure RICH                                            307
      ASSIGN 5020 TO BLOCK_RICH                                           307
      GO TO  5010                                                         307
5020  CONTINUE                                                            307
C *                                                                       307
C Geant POSITION : RICH ===> MOTHER='CALB' X=RPOS_RPOS*COS(RPOS_PHI*DEGR  307
C AD)-RPOS_YPOS*ROT(1,2), Y = RPOS_RPOS*SIN(RPOS_PHI*DEGRAD)-RPOS_YPOS*R  307
C OT(2,2), Z = RPOS_ZPOS-RPOS_YPOS*ROT(3,2), THETAX=ACOS(ROT(3,1))*RADDE  307
C G PHIX=ATAN2(ROT(2,1),ROT(1,1))*RADDEG, THETAY=ACOS(ROT(3,2))*RADDEG P  307
C HIY=ATAN2(ROT(2,2),ROT(1,2))*RADDEG, THETAZ=ACOS(ROT(3,3))*RADDEG PHIZ  307
C =ATAN2(ROT(2,3),ROT(1,3))*RADDEG <===                                   307
      CALL AGSRESET                                                       307
      AG_MOTHER = 'CALB'                                                  307
      AG_X = RPOS_RPOS*COS(RPOS_PHI*DEGRAD)-RPOS_YPOS*ROT(1,2)            307
      AG_Y = RPOS_RPOS*SIN(RPOS_PHI*DEGRAD)-RPOS_YPOS*ROT(2,2)            307
      AG_Z = RPOS_ZPOS-RPOS_YPOS*ROT(3,2)                                 307
      AG_THETAX = ACOS(ROT(3,1))*RADDEG                                   307
      AG_PHIX = ATAN2(ROT(2,1),ROT(1,1))*RADDEG                           307
      AG_THETAY = ACOS(ROT(3,2))*RADDEG                                   307
      AG_PHIY = ATAN2(ROT(2,2),ROT(1,2))*RADDEG                           307
      AG_THETAZ = ACOS(ROT(3,3))*RADDEG                                   307
      AG_PHIZ = ATAN2(ROT(2,3),ROT(1,3))*RADDEG                           307
      AG_TITLE = 'RICH'                                                   307
      AG_EXNAME = 'POSITION'                                              307
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'     307
      CALL AXPOSITION                                                     307
C *                                                                       307
*     AlphaZ=rpos_phi-90
C Check IPRIN>=1                                                          310
      IF (IPRIN.GE.1) WRITE(LOUT,5030)AG_X,AG_Y,AG_Z, (ROT(I,2),I=1,3)    310
5030  FORMAT(' Rich position =',3F10.4,',  orientation =',3F10.6)         312
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL AGSENDM                                                        315
      RETURN                                                              315
C Begin Local Procedure RICH                                              315
C RICH IS JUST AN ALUMINUM BOX                                            315
5010  CONTINUE                                                            315
         AG_TITLE='RICH'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
*   outside dimensions                         mm       cm
*   tickness        104+25+41+22.5+2+32 =  226.5 /2 = 11.325
*   long dimension  146.3 /2 = 73.15  cm
*   short dimension 989 /2   = 494.5  mm
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        322
         CALL AGSRESET                                                    322
         AG_TITLE = 'ALUMINIUM'                                           322
         AG_EXNAME = 'MATERIAL'                                           322
         AG_PARLIST = 'NONE'                                              322
         CALL AXMATERIAL                                                  322
C    *                                                                    322
C    Geant ATTRIBUTE : RICH ===> SEEN=1 COLO=5 <===                       322
         CALL AGSRESET                                                    323
         AG_SEEN = 1                                                      323
         AG_COLO = 5                                                      323
         AG_TITLE = 'RICH'                                                323
         AG_EXNAME = 'ATTRIBUTE'                                          323
         AG_PARLIST = 'SEEN_COLO'                                         323
         CALL AXATTRIBUTE                                                 323
C    *                                                                    323
C    Geant SHAPE : BOX ===> DX=RICH_DX DY=RICH_DY DZ=RICH_DZ <===         323
         CALL AGSRESET                                                    324
         AG_DX = RICH_DX                                                  324
         AG_DY = RICH_DY                                                  324
         AG_DZ = RICH_DZ                                                  324
         AG_TITLE = 'BOX'                                                 324
         AG_EXNAME = 'SHAPE'                                              324
         AG_PARLIST = 'DX_DY_DZ'                                          324
         CALL AXSHAPE                                                     324
C    *                                                                    324
C    use RMAT bank                                                        324
         BPATH_RMAT='RMAT*'                                               325
         IDNUM_RMAT(1)=0                                                  325
         RMAT_MATE='GRI'                                                  325
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  325
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  325
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   325
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         326
     *   RMAT_RINDEX)                                                     326
C    Create SRIC :                                                        328
C    Call To Local Procedure SRIC                                         328
         ASSIGN 5050 TO BLOCK_SRIC                                        328
         GO TO  5040                                                      328
5050     CONTINUE                                                         328
C    *                                                                    328
C    Geant POSITION : SRIC ===> inherited arguments used <===             328
         CALL AGSRESET                                                    328
         AG_TITLE = 'SRIC'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'NONE'                                              328
         CALL AXPOSITION                                                  328
C    *                                                                    328
5011  CALL AGSPOPB                                                        328
      GO TO BLOCK_RICH, (5010 ,5020)                                      329
C End of Local Procedure                                                  329
*-----------------------------------------------------------------
C Begin Local Procedure SRIC                                              331
C SRIC IS SENSITIVE PART OF THE WHOLE RICH ... FULL OF METHANE            331
5040  CONTINUE                                                            331
         AG_TITLE='SRIC'                                                  331
         AG_MODULE=MODULE                                                 331
         AG_MARK='V00'                                                    331
         CALL AGSPUSH                                                     331
         AG_MODULE=MODULE                                                 331
C    *                                                                    331
* inside dimensions
* long dimension 146.3 - 2*70 = 132.3 / 2 = 66.15
* short dimension  989 - 2*70 =  84.9 / 2 = 42.45
*   +194.5+16    Aluminum frame                9.725
*   +194.5+16    Hole in Frame                 9.725
*   +194.5+2.25  board                         8.350
*   +194.5+0.25  csi center                    8.150
*   +192.5  4 millimeter volume gap center     7.925
*    (190.5 - 98 )/2=46.25+ 98 = 144.25 methane center  3.1
*   +95.5   center quartz                     -1.775 +1.275
*   +88     center freon                      -2.525 +1.275
*   +81     center neoceram                   -3.225 +1.275
*   +71     honeycomb ends   with alu cover   -4.225
*   +38     center honeycomb                  -7.525
*   +5      honeycomb starts with alu cover   -10.825
*   -11.325 is the bottom of the detector
*
*    position of the radiator is 433/2 = 21.65
*
*    four csi cathodes 640*393 with a frame of 21
*    xpos  393/2 + 21/2 + 9.5 = 21.65
*    zpos  640/2 + 21/2 + 4.5 = 33.5
C    Geant MATERIAL : METHANE ===> inherited arguments used <===          356
         CALL AGSRESET                                                    356
         AG_TITLE = 'METHANE'                                             356
         AG_EXNAME = 'MATERIAL'                                           356
         AG_PARLIST = 'NONE'                                              356
         CALL AXMATERIAL                                                  356
C    *                                                                    356
C    Geant ATTRIBUTE : SRIC ===> SEEN=1 COLO=1 <===                       356
         CALL AGSRESET                                                    357
         AG_SEEN = 1                                                      357
         AG_COLO = 1                                                      357
         AG_TITLE = 'SRIC'                                                357
         AG_EXNAME = 'ATTRIBUTE'                                          357
         AG_PARLIST = 'SEEN_COLO'                                         357
         CALL AXATTRIBUTE                                                 357
C    *                                                                    357
C    Geant SHAPE : BOX ===> DX=42.45 DY=11.325 DZ=66.15 <===              357
         CALL AGSRESET                                                    358
         AG_DX = 42.45                                                    358
         AG_DY = 11.325                                                   358
         AG_DZ = 66.15                                                    358
         AG_TITLE = 'BOX'                                                 358
         AG_EXNAME = 'SHAPE'                                              358
         AG_PARLIST = 'DX_DY_DZ'                                          358
         CALL AXSHAPE                                                     358
C    *                                                                    358
C    use RMAT bank                                                        358
         BPATH_RMAT='RMAT*'                                               359
         IDNUM_RMAT(1)=0                                                  359
         RMAT_MATE='META'                                                 359
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  359
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  359
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   359
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         360
     *   RMAT_RINDEX)                                                     360
C    Create ALUM :                                                        362
C    Call To Local Procedure ALUM                                         362
         ASSIGN 5070 TO BLOCK_ALUM                                        362
         GO TO  5060                                                      362
5070     CONTINUE                                                         362
C    *                                                                    362
C    Geant POSITION : ALUM ===> Y=-10.825 <===                            362
         CALL AGSRESET                                                    362
         AG_Y = -10.825                                                   362
         AG_TITLE = 'ALUM'                                                362
         AG_EXNAME = 'POSITION'                                           362
         AG_PARLIST = 'Y'                                                 362
         CALL AXPOSITION                                                  362
C    *                                                                    362
C    Create HONE :                                                        363
C    Call To Local Procedure HONE                                         363
         ASSIGN 5090 TO BLOCK_HONE                                        363
         GO TO  5080                                                      363
5090     CONTINUE                                                         363
C    *                                                                    363
C    Geant POSITION : HONE ===> Y=-7.525 <===                             363
         CALL AGSRESET                                                    363
         AG_Y = -7.525                                                    363
         AG_TITLE = 'HONE'                                                363
         AG_EXNAME = 'POSITION'                                           363
         AG_PARLIST = 'Y'                                                 363
         CALL AXPOSITION                                                  363
C    *                                                                    363
C    Create ALUM :                                                        364
C    Call To Local Procedure ALUM                                         364
         ASSIGN 5100 TO BLOCK_ALUM                                        364
         GO TO  5060                                                      364
5100     CONTINUE                                                         364
C    *                                                                    364
C    Geant POSITION : ALUM ===> Y=-4.225 <===                             364
         CALL AGSRESET                                                    364
         AG_Y = -4.225                                                    364
         AG_TITLE = 'ALUM'                                                364
         AG_EXNAME = 'POSITION'                                           364
         AG_PARLIST = 'Y'                                                 364
         CALL AXPOSITION                                                  364
C    *                                                                    364
C    Create OQUA :                                                        365
C    Call To Local Procedure OQUA                                         365
         ASSIGN 5120 TO BLOCK_OQUA                                        365
         GO TO  5110                                                      365
5120     CONTINUE                                                         365
C    *                                                                    365
C    Geant POSITION : OQUA ===> X=-21.65 Y=-3.225+1.275 <===              365
         CALL AGSRESET                                                    365
         AG_X = -21.65                                                    365
         AG_Y = -3.225+1.275                                              365
         AG_TITLE = 'OQUA'                                                365
         AG_EXNAME = 'POSITION'                                           365
         AG_PARLIST = 'X_Y'                                               365
         CALL AXPOSITION                                                  365
C    *                                                                    365
C    Create OQUA :                                                        366
C    Call To Local Procedure OQUA                                         366
         ASSIGN 5130 TO BLOCK_OQUA                                        366
         GO TO  5110                                                      366
5130     CONTINUE                                                         366
C    *                                                                    366
C    Geant POSITION : OQUA ===> X=21.65 Y=-3.225+1.275 <===               366
         CALL AGSRESET                                                    366
         AG_X = 21.65                                                     366
         AG_Y = -3.225+1.275                                              366
         AG_TITLE = 'OQUA'                                                366
         AG_EXNAME = 'POSITION'                                           366
         AG_PARLIST = 'X_Y'                                               366
         CALL AXPOSITION                                                  366
C    *                                                                    366
C    Create OQUF :                                                        367
C    Call To Local Procedure OQUF                                         367
         ASSIGN 5150 TO BLOCK_OQUF                                        367
         GO TO  5140                                                      367
5150     CONTINUE                                                         367
C    *                                                                    367
C    Geant POSITION : OQUF ===> X=-21.65 Y=-2.525+1.275 <===              367
         CALL AGSRESET                                                    367
         AG_X = -21.65                                                    367
         AG_Y = -2.525+1.275                                              367
         AG_TITLE = 'OQUF'                                                367
         AG_EXNAME = 'POSITION'                                           367
         AG_PARLIST = 'X_Y'                                               367
         CALL AXPOSITION                                                  367
C    *                                                                    367
C    Create OQUF :                                                        368
C    Call To Local Procedure OQUF                                         368
         ASSIGN 5160 TO BLOCK_OQUF                                        368
         GO TO  5140                                                      368
5160     CONTINUE                                                         368
C    *                                                                    368
C    Geant POSITION : OQUF ===> X=21.65 Y=-2.525+1.275 <===               368
         CALL AGSRESET                                                    368
         AG_X = 21.65                                                     368
         AG_Y = -2.525+1.275                                              368
         AG_TITLE = 'OQUF'                                                368
         AG_EXNAME = 'POSITION'                                           368
         AG_PARLIST = 'X_Y'                                               368
         CALL AXPOSITION                                                  368
C    *                                                                    368
C    Create QUAR :                                                        369
C    Call To Local Procedure QUAR                                         369
         ASSIGN 5180 TO BLOCK_QUAR                                        369
         GO TO  5170                                                      369
5180     CONTINUE                                                         369
C    *                                                                    369
C    Geant POSITION : QUAR ===> X=-21.6 Y=-1.775+1.275 <===               369
         CALL AGSRESET                                                    369
         AG_X = -21.6                                                     369
         AG_Y = -1.775+1.275                                              369
         AG_TITLE = 'QUAR'                                                369
         AG_EXNAME = 'POSITION'                                           369
         AG_PARLIST = 'X_Y'                                               369
         CALL AXPOSITION                                                  369
C    *                                                                    369
C    Create QUAR :                                                        370
C    Call To Local Procedure QUAR                                         370
         ASSIGN 5190 TO BLOCK_QUAR                                        370
         GO TO  5170                                                      370
5190     CONTINUE                                                         370
C    *                                                                    370
C    Geant POSITION : QUAR ===> X=21.6 Y=-1.775+1.275 <===                370
         CALL AGSRESET                                                    370
         AG_X = 21.6                                                      370
         AG_Y = -1.775+1.275                                              370
         AG_TITLE = 'QUAR'                                                370
         AG_EXNAME = 'POSITION'                                           370
         AG_PARLIST = 'X_Y'                                               370
         CALL AXPOSITION                                                  370
C    *                                                                    370
*    It turns out that we are working in a methane volume in the
*    first place so no Methane here ..
*    Create and Position META              y= 3.100
C    Create RGAP :                                                        376
C    Call To Local Procedure RGAP                                         376
         ASSIGN 5210 TO BLOCK_RGAP                                        376
         GO TO  5200                                                      376
5210     CONTINUE                                                         376
C    *                                                                    376
C    Geant POSITION : RGAP ===> Y=7.550 <===                              376
         CALL AGSRESET                                                    376
         AG_Y = 7.550                                                     376
         AG_TITLE = 'RGAP'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'Y'                                                 376
         CALL AXPOSITION                                                  376
C    *                                                                    376
C    Create ALMF :                                                        377
C    Call To Local Procedure ALMF                                         377
         ASSIGN 5230 TO BLOCK_ALMF                                        377
         GO TO  5220                                                      377
5230     CONTINUE                                                         377
C    *                                                                    377
C    Geant POSITION : ALMF ===> Y=9.350 <===                              377
         CALL AGSRESET                                                    377
         AG_Y = 9.350                                                     377
         AG_TITLE = 'ALMF'                                                377
         AG_EXNAME = 'POSITION'                                           377
         AG_PARLIST = 'Y'                                                 377
         CALL AXPOSITION                                                  377
C    *                                                                    377
5041  CALL AGSPOPB                                                        377
      GO TO BLOCK_SRIC, (5040 ,5050)                                      378
C End of Local Procedure                                                  378
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure ALUM                                              380
C ALUM IS AN ALUMINUM SHEET                                               380
5060  CONTINUE                                                            380
         AG_TITLE='ALUM'                                                  380
         AG_MODULE=MODULE                                                 380
         AG_MARK='V00'                                                    380
         CALL AGSPUSH                                                     380
         AG_MODULE=MODULE                                                 380
C    *                                                                    380
* flush in sric
* thickness 0.5 mm ???
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        385
         CALL AGSRESET                                                    385
         AG_TITLE = 'ALUMINIUM'                                           385
         AG_EXNAME = 'MATERIAL'                                           385
         AG_PARLIST = 'NONE'                                              385
         CALL AXMATERIAL                                                  385
C    *                                                                    385
C    Geant ATTRIBUTE : ALUM ===> SEEN=1 COLO=5 <===                       385
         CALL AGSRESET                                                    386
         AG_SEEN = 1                                                      386
         AG_COLO = 5                                                      386
         AG_TITLE = 'ALUM'                                                386
         AG_EXNAME = 'ATTRIBUTE'                                          386
         AG_PARLIST = 'SEEN_COLO'                                         386
         CALL AXATTRIBUTE                                                 386
C    *                                                                    386
C    Geant SHAPE : BOX ===> DX=42.45 DY=0.025 DZ=66.15 <===               386
         CALL AGSRESET                                                    387
         AG_DX = 42.45                                                    387
         AG_DY = 0.025                                                    387
         AG_DZ = 66.15                                                    387
         AG_TITLE = 'BOX'                                                 387
         AG_EXNAME = 'SHAPE'                                              387
         AG_PARLIST = 'DX_DY_DZ'                                          387
         CALL AXSHAPE                                                     387
C    *                                                                    387
C    use RMAT bank                                                        387
         BPATH_RMAT='RMAT*'                                               388
         IDNUM_RMAT(1)=0                                                  388
         RMAT_MATE='GRI'                                                  388
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  388
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  388
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   388
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         389
     *   RMAT_RINDEX)                                                     389
5061  CALL AGSPOPB                                                        390
      GO TO BLOCK_ALUM, (5060 ,5070,5100)                                 390
C End of Local Procedure                                                  390
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure ALMF                                              392
C ALMF IS AN ALUMINUM SHEET                                               392
5220  CONTINUE                                                            392
         AG_TITLE='ALMF'                                                  392
         AG_MODULE=MODULE                                                 392
         AG_MARK='V00'                                                    392
         CALL AGSPUSH                                                     392
         AG_MODULE=MODULE                                                 392
C    *                                                                    392
* flush in sric a four hole aluminum frame
* thickness 32mm
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        397
         CALL AGSRESET                                                    397
         AG_TITLE = 'ALUMINIUM'                                           397
         AG_EXNAME = 'MATERIAL'                                           397
         AG_PARLIST = 'NONE'                                              397
         CALL AXMATERIAL                                                  397
C    *                                                                    397
C    Geant ATTRIBUTE : ALMF ===> SEEN=1 COLO=5 <===                       397
         CALL AGSRESET                                                    398
         AG_SEEN = 1                                                      398
         AG_COLO = 5                                                      398
         AG_TITLE = 'ALMF'                                                398
         AG_EXNAME = 'ATTRIBUTE'                                          398
         AG_PARLIST = 'SEEN_COLO'                                         398
         CALL AXATTRIBUTE                                                 398
C    *                                                                    398
C    Geant SHAPE : BOX ===> DX=42.45 DY=1.6 DZ=66.15 <===                 398
         CALL AGSRESET                                                    399
         AG_DX = 42.45                                                    399
         AG_DY = 1.6                                                      399
         AG_DZ = 66.15                                                    399
         AG_TITLE = 'BOX'                                                 399
         AG_EXNAME = 'SHAPE'                                              399
         AG_PARLIST = 'DX_DY_DZ'                                          399
         CALL AXSHAPE                                                     399
C    *                                                                    399
C    use RMAT bank                                                        399
         BPATH_RMAT='RMAT*'                                               400
         IDNUM_RMAT(1)=0                                                  400
         RMAT_MATE='GRI'                                                  400
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  400
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  400
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   400
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         401
     *   RMAT_RINDEX)                                                     401
C    Create HOLE :                                                        403
C    Call To Local Procedure HOLE                                         403
         ASSIGN 5250 TO BLOCK_HOLE                                        403
         GO TO  5240                                                      403
5250     CONTINUE                                                         403
C    *                                                                    403
C    Geant POSITION : HOLE ===> X=-21.65 Y=0.0 Z=-33.5 <===               403
         CALL AGSRESET                                                    403
         AG_X = -21.65                                                    403
         AG_Y = 0.0                                                       403
         AG_Z = -33.5                                                     403
         AG_TITLE = 'HOLE'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'X_Y_Z'                                             403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Create HOLE :                                                        404
C    Call To Local Procedure HOLE                                         404
         ASSIGN 5260 TO BLOCK_HOLE                                        404
         GO TO  5240                                                      404
5260     CONTINUE                                                         404
C    *                                                                    404
C    Geant POSITION : HOLE ===> X=21.65 Y=0.0 Z=-33.5 <===                404
         CALL AGSRESET                                                    404
         AG_X = 21.65                                                     404
         AG_Y = 0.0                                                       404
         AG_Z = -33.5                                                     404
         AG_TITLE = 'HOLE'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'X_Y_Z'                                             404
         CALL AXPOSITION                                                  404
C    *                                                                    404
C    Create HOLE :                                                        405
C    Call To Local Procedure HOLE                                         405
         ASSIGN 5270 TO BLOCK_HOLE                                        405
         GO TO  5240                                                      405
5270     CONTINUE                                                         405
C    *                                                                    405
C    Geant POSITION : HOLE ===> X=-21.65 Y=0.0 Z=33.5 <===                405
         CALL AGSRESET                                                    405
         AG_X = -21.65                                                    405
         AG_Y = 0.0                                                       405
         AG_Z = 33.5                                                      405
         AG_TITLE = 'HOLE'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'X_Y_Z'                                             405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Create HOLE :                                                        406
C    Call To Local Procedure HOLE                                         406
         ASSIGN 5280 TO BLOCK_HOLE                                        406
         GO TO  5240                                                      406
5280     CONTINUE                                                         406
C    *                                                                    406
C    Geant POSITION : HOLE ===> X=21.65 Y=0.0 Z=33.5 <===                 406
         CALL AGSRESET                                                    406
         AG_X = 21.65                                                     406
         AG_Y = 0.0                                                       406
         AG_Z = 33.5                                                      406
         AG_TITLE = 'HOLE'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'X_Y_Z'                                             406
         CALL AXPOSITION                                                  406
C    *                                                                    406
5221  CALL AGSPOPB                                                        406
      GO TO BLOCK_ALMF, (5220 ,5230)                                      407
C End of Local Procedure                                                  407
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure HONE                                              409
C HONE IS A CARBONIO                                                      409
5080  CONTINUE                                                            409
         AG_TITLE='HONE'                                                  409
         AG_MODULE=MODULE                                                 409
         AG_MARK='V00'                                                    409
         CALL AGSPUSH                                                     409
         AG_MODULE=MODULE                                                 409
C    *                                                                    409
*  flush in sric
*  thickness  104-33-5 = 66 / 2 = 3.3 but 0.188 is the equivalent to carbon
C    Geant MATERIAL : CARBONIO ===> inherited arguments used <===         414
         CALL AGSRESET                                                    414
         AG_TITLE = 'CARBONIO'                                            414
         AG_EXNAME = 'MATERIAL'                                           414
         AG_PARLIST = 'NONE'                                              414
         CALL AXMATERIAL                                                  414
C    *                                                                    414
C    Geant ATTRIBUTE : HONE ===> SEEN=1 COLO=6 <===                       414
         CALL AGSRESET                                                    415
         AG_SEEN = 1                                                      415
         AG_COLO = 6                                                      415
         AG_TITLE = 'HONE'                                                415
         AG_EXNAME = 'ATTRIBUTE'                                          415
         AG_PARLIST = 'SEEN_COLO'                                         415
         CALL AXATTRIBUTE                                                 415
C    *                                                                    415
C    Geant SHAPE : BOX ===> DX=42.45 DY=0.188 DZ=66.15 <===               415
         CALL AGSRESET                                                    416
         AG_DX = 42.45                                                    416
         AG_DY = 0.188                                                    416
         AG_DZ = 66.15                                                    416
         AG_TITLE = 'BOX'                                                 416
         AG_EXNAME = 'SHAPE'                                              416
         AG_PARLIST = 'DX_DY_DZ'                                          416
         CALL AXSHAPE                                                     416
C    *                                                                    416
*   use methane properties intead of Carbon ?
C    use RMAT bank                                                        418
         BPATH_RMAT='RMAT*'                                               418
         IDNUM_RMAT(1)=0                                                  418
         RMAT_MATE='META'                                                 418
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  418
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  418
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   418
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         419
     *   RMAT_RINDEX)                                                     419
5081  CALL AGSPOPB                                                        420
      GO TO BLOCK_HONE, (5080 ,5090)                                      420
C End of Local Procedure                                                  420
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure OQUA                                              422
C OQUA E ME SCELTO PER LABELLARE IL QUARZO OPACO                          422
5110  CONTINUE                                                            422
         AG_TITLE='OQUA'                                                  422
         AG_MODULE=MODULE                                                 422
         AG_MARK='V00'                                                    422
         CALL AGSPUSH                                                     422
         AG_MODULE=MODULE                                                 422
C    *                                                                    422
* 1330 /2 = 66.5 bigger than inside box
* 413  /2 = 20.65
* thickness 4 / 2 = 0.2
* position width 433
C    Geant MATERIAL : OPACO ===> inherited arguments used <===            429
         CALL AGSRESET                                                    429
         AG_TITLE = 'OPACO'                                               429
         AG_EXNAME = 'MATERIAL'                                           429
         AG_PARLIST = 'NONE'                                              429
         CALL AXMATERIAL                                                  429
C    *                                                                    429
C    Geant MEDIUM : OPACO_QUARTZ ===> ISVOL=ISOPT <===                    429
         CALL AGSRESET                                                    430
         AG_ISVOL = ISOPT                                                 430
         AG_TITLE = 'OPACO_QUARTZ'                                        430
         AG_EXNAME = 'MEDIUM'                                             430
         AG_PARLIST = 'ISVOL'                                             430
         CALL AXMEDIUM                                                    430
C    *                                                                    430
C    Geant ATTRIBUTE : OQUA ===> SEEN=1 COLO=2 <===                       430
         CALL AGSRESET                                                    431
         AG_SEEN = 1                                                      431
         AG_COLO = 2                                                      431
         AG_TITLE = 'OQUA'                                                431
         AG_EXNAME = 'ATTRIBUTE'                                          431
         AG_PARLIST = 'SEEN_COLO'                                         431
         CALL AXATTRIBUTE                                                 431
C    *                                                                    431
C    Geant SHAPE : BOX ===> DX=20.65 DY=0.2 DZ=66.5 <===                  431
         CALL AGSRESET                                                    432
         AG_DX = 20.65                                                    432
         AG_DY = 0.2                                                      432
         AG_DZ = 66.5                                                     432
         AG_TITLE = 'BOX'                                                 432
         AG_EXNAME = 'SHAPE'                                              432
         AG_PARLIST = 'DX_DY_DZ'                                          432
         CALL AXSHAPE                                                     432
C    *                                                                    432
C    use RMAT bank                                                        432
         BPATH_RMAT='RMAT*'                                               433
         IDNUM_RMAT(1)=0                                                  433
         RMAT_MATE='QRZO'                                                 433
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  433
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  433
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   433
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         434
     *   RMAT_RINDEX)                                                     434
C    Check ISOPT>0                                                        436
         IF (ISOPT.GT.0) THEN                                             436
C    Defining Hits: X:.01: Y:.01: Z:.01:, CX:10: CY:10: CZ:10:, SLEN:.1:  441
C    (0,500) LPTOT:18:(-3,2), TOF:16:(0,1.E-6) LGAM:16:(-2,2), STEP:16:(  441
C    0,10) ELOSS:16:(0,0.001)                                             441
         AG_TITLE = 'OQUA'                                                441
         AG_EXNAME = 'HITS'                                               441
         AG_TYPE=AG_MAGIC                                                 441
         CALL AGSDETV('OQUA')                                             441
C    *                                                                    441
         AG_PARLIST='X'                                                   441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = AG_MAGIC                                              441
         AG_BIN=.01                                                       441
         AG_FHMIN=AG_MAGIC                                                441
         AG_FHMAX=AG_MAGIC                                                441
         AG_OPTION='*'                                                    441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='Y'                                                   441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = AG_MAGIC                                              441
         AG_BIN=.01                                                       441
         AG_FHMIN=AG_MAGIC                                                441
         AG_FHMAX=AG_MAGIC                                                441
         AG_OPTION='*'                                                    441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='Z'                                                   441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = AG_MAGIC                                              441
         AG_BIN=.01                                                       441
         AG_OPTION='*'                                                    441
         AG_FHMIN = AG_MAGIC                                              441
         AG_FHMAX = AG_MAGIC                                              441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='CX'                                                  441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 10                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_FHMIN=AG_MAGIC                                                441
         AG_FHMAX=AG_MAGIC                                                441
         AG_OPTION='*'                                                    441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='CY'                                                  441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 10                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_FHMIN=AG_MAGIC                                                441
         AG_FHMAX=AG_MAGIC                                                441
         AG_OPTION='*'                                                    441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='CZ'                                                  441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 10                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_OPTION='*'                                                    441
         AG_FHMIN = AG_MAGIC                                              441
         AG_FHMAX = AG_MAGIC                                              441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='SLEN'                                                441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = AG_MAGIC                                              441
         AG_BIN=.1                                                        441
         AG_OPTION='*'                                                    441
         AG_FHMIN = 0                                                     441
         AG_FHMAX = 500                                                   441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='LPTOT'                                               441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 18                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_OPTION='*'                                                    441
         AG_FHMIN = -3                                                    441
         AG_FHMAX = 2                                                     441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='TOF'                                                 441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 16                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_OPTION='*'                                                    441
         AG_FHMIN = 0                                                     441
         AG_FHMAX = 1.E-6                                                 441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='LGAM'                                                441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 16                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_OPTION='*'                                                    441
         AG_FHMIN = -2                                                    441
         AG_FHMAX = 2                                                     441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='STEP'                                                441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 16                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_OPTION='*'                                                    441
         AG_FHMIN = 0                                                     441
         AG_FHMAX = 10                                                    441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         AG_PARLIST='ELOSS'                                               441
         AG_TITLE = 'DETH'                                                441
         AG_NBITS = 16                                                    441
         AG_BIN=AG_MAGIC                                                  441
         AG_OPTION='*'                                                    441
         AG_FHMIN = 0                                                     441
         AG_FHMAX = 0.001                                                 441
         CALL AGSDETH (0)                                                 441
C    *                                                                    441
         CALL AGSDETU('OQUA')                                             441
C    *                                                                    441
         ENDIF                                                            442
5111  CALL AGSPOPB                                                        443
      GO TO BLOCK_OQUA, (5110 ,5120,5130)                                 443
C End of Local Procedure                                                  443
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure QUAR                                              445
C QUAR DA ME SCELTO PER LABELLARE IL QUARZO                               445
5170  CONTINUE                                                            445
         AG_TITLE='QUAR'                                                  445
         AG_MODULE=MODULE                                                 445
         AG_MARK='V00'                                                    445
         CALL AGSPUSH                                                     445
         AG_MODULE=MODULE                                                 445
C    *                                                                    445
*   A big piece consisting out of 3 tiles connected with opaco glue
*   actual tile size is
*   443.2 * 3 = 1329.6 that's 0.4 less than the 133, 2 gaps of 0.2/2=0.1
*     gaps are barr, positioned at 443.2/2+0.1 away from center 221.7
*   413   /2 =  20.65
C    Geant MATERIAL : QUARZ ===> inherited arguments used <===            453
         CALL AGSRESET                                                    453
         AG_TITLE = 'QUARZ'                                               453
         AG_EXNAME = 'MATERIAL'                                           453
         AG_PARLIST = 'NONE'                                              453
         CALL AXMATERIAL                                                  453
C    *                                                                    453
C    Geant MEDIUM : QUARTZ ===> ISVOL=ISOPT <===                          453
         CALL AGSRESET                                                    454
         AG_ISVOL = ISOPT                                                 454
         AG_TITLE = 'QUARTZ'                                              454
         AG_EXNAME = 'MEDIUM'                                             454
         AG_PARLIST = 'ISVOL'                                             454
         CALL AXMEDIUM                                                    454
C    *                                                                    454
C    Geant ATTRIBUTE : QUAR ===> SEEN=1 COLO=2 <===                       454
         CALL AGSRESET                                                    455
         AG_SEEN = 1                                                      455
         AG_COLO = 2                                                      455
         AG_TITLE = 'QUAR'                                                455
         AG_EXNAME = 'ATTRIBUTE'                                          455
         AG_PARLIST = 'SEEN_COLO'                                         455
         CALL AXATTRIBUTE                                                 455
C    *                                                                    455
C    Geant SHAPE : BOX ===> DX=20.65 DY=0.25 DZ=66.5 <===                 455
         CALL AGSRESET                                                    456
         AG_DX = 20.65                                                    456
         AG_DY = 0.25                                                     456
         AG_DZ = 66.5                                                     456
         AG_TITLE = 'BOX'                                                 456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'DX_DY_DZ'                                          456
         CALL AXSHAPE                                                     456
C    *                                                                    456
C    use RMAT bank                                                        456
         BPATH_RMAT='RMAT*'                                               457
         IDNUM_RMAT(1)=0                                                  457
         RMAT_MATE='QARZ'                                                 457
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  457
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  457
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   457
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         458
     *   RMAT_RINDEX)                                                     458
C    Create BARR :                                                        460
C    Call To Local Procedure BARR                                         460
         ASSIGN 5300 TO BLOCK_BARR                                        460
         GO TO  5290                                                      460
5300     CONTINUE                                                         460
C    *                                                                    460
C    Geant POSITION : BARR ===> Z=-21.7 <===                              460
         CALL AGSRESET                                                    460
         AG_Z = -21.7                                                     460
         AG_TITLE = 'BARR'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'Z'                                                 460
         CALL AXPOSITION                                                  460
C    *                                                                    460
C    Create BARR :                                                        461
C    Call To Local Procedure BARR                                         461
         ASSIGN 5310 TO BLOCK_BARR                                        461
         GO TO  5290                                                      461
5310     CONTINUE                                                         461
C    *                                                                    461
C    Geant POSITION : BARR ===> Z=21.7 <===                               461
         CALL AGSRESET                                                    461
         AG_Z = 21.7                                                      461
         AG_TITLE = 'BARR'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'Z'                                                 461
         CALL AXPOSITION                                                  461
C    *                                                                    461
C    Check ISOPT>0                                                        461
         IF (ISOPT.GT.0) THEN                                             462
C    Defining Hits: X:.01: Y:.01: Z:.01:, CX:10: CY:10: CZ:10:, SLEN:.1:  467
C    (0,500) LPTOT:18:(-3,2), TOF:16:(0,1.E-6) LGAM:16:(-2,2), STEP:16:(  467
C    0,10) ELOSS:16:(0,0.001)                                             467
         AG_TITLE = 'QUAR'                                                467
         AG_EXNAME = 'HITS'                                               467
         AG_TYPE=AG_MAGIC                                                 467
         CALL AGSDETV('QUAR')                                             467
C    *                                                                    467
         AG_PARLIST='X'                                                   467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = AG_MAGIC                                              467
         AG_BIN=.01                                                       467
         AG_FHMIN=AG_MAGIC                                                467
         AG_FHMAX=AG_MAGIC                                                467
         AG_OPTION='*'                                                    467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='Y'                                                   467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = AG_MAGIC                                              467
         AG_BIN=.01                                                       467
         AG_FHMIN=AG_MAGIC                                                467
         AG_FHMAX=AG_MAGIC                                                467
         AG_OPTION='*'                                                    467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='Z'                                                   467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = AG_MAGIC                                              467
         AG_BIN=.01                                                       467
         AG_OPTION='*'                                                    467
         AG_FHMIN = AG_MAGIC                                              467
         AG_FHMAX = AG_MAGIC                                              467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='CX'                                                  467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 10                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_FHMIN=AG_MAGIC                                                467
         AG_FHMAX=AG_MAGIC                                                467
         AG_OPTION='*'                                                    467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='CY'                                                  467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 10                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_FHMIN=AG_MAGIC                                                467
         AG_FHMAX=AG_MAGIC                                                467
         AG_OPTION='*'                                                    467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='CZ'                                                  467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 10                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_OPTION='*'                                                    467
         AG_FHMIN = AG_MAGIC                                              467
         AG_FHMAX = AG_MAGIC                                              467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='SLEN'                                                467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = AG_MAGIC                                              467
         AG_BIN=.1                                                        467
         AG_OPTION='*'                                                    467
         AG_FHMIN = 0                                                     467
         AG_FHMAX = 500                                                   467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='LPTOT'                                               467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 18                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_OPTION='*'                                                    467
         AG_FHMIN = -3                                                    467
         AG_FHMAX = 2                                                     467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='TOF'                                                 467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 16                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_OPTION='*'                                                    467
         AG_FHMIN = 0                                                     467
         AG_FHMAX = 1.E-6                                                 467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='LGAM'                                                467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 16                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_OPTION='*'                                                    467
         AG_FHMIN = -2                                                    467
         AG_FHMAX = 2                                                     467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='STEP'                                                467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 16                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_OPTION='*'                                                    467
         AG_FHMIN = 0                                                     467
         AG_FHMAX = 10                                                    467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         AG_PARLIST='ELOSS'                                               467
         AG_TITLE = 'DETH'                                                467
         AG_NBITS = 16                                                    467
         AG_BIN=AG_MAGIC                                                  467
         AG_OPTION='*'                                                    467
         AG_FHMIN = 0                                                     467
         AG_FHMAX = 0.001                                                 467
         CALL AGSDETH (0)                                                 467
C    *                                                                    467
         CALL AGSDETU('QUAR')                                             467
C    *                                                                    467
         ENDIF                                                            468
5171  CALL AGSPOPB                                                        469
      GO TO BLOCK_QUAR, (5170 ,5180,5190)                                 469
C End of Local Procedure                                                  469
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure BARR                                              471
C BARR E BARRETTE QUARZO OPACO                                            471
5290  CONTINUE                                                            471
         AG_TITLE='BARR'                                                  471
         AG_MODULE=MODULE                                                 471
         AG_MARK='V00'                                                    471
         CALL AGSPUSH                                                     471
         AG_MODULE=MODULE                                                 471
C    *                                                                    471
* width in z  0.2/2=0.1
* 413   /2 =  20.65
C    Geant MATERIAL : OPACO ===> inherited arguments used <===            476
         CALL AGSRESET                                                    476
         AG_TITLE = 'OPACO'                                               476
         AG_EXNAME = 'MATERIAL'                                           476
         AG_PARLIST = 'NONE'                                              476
         CALL AXMATERIAL                                                  476
C    *                                                                    476
C    Geant ATTRIBUTE : BARR ===> SEEN=1 COLO=2 <===                       476
         CALL AGSRESET                                                    477
         AG_SEEN = 1                                                      477
         AG_COLO = 2                                                      477
         AG_TITLE = 'BARR'                                                477
         AG_EXNAME = 'ATTRIBUTE'                                          477
         AG_PARLIST = 'SEEN_COLO'                                         477
         CALL AXATTRIBUTE                                                 477
C    *                                                                    477
C    Geant SHAPE : BOX ===> DX=20.65 DY=0.25 DZ=0.1 <===                  477
         CALL AGSRESET                                                    478
         AG_DX = 20.65                                                    478
         AG_DY = 0.25                                                     478
         AG_DZ = 0.1                                                      478
         AG_TITLE = 'BOX'                                                 478
         AG_EXNAME = 'SHAPE'                                              478
         AG_PARLIST = 'DX_DY_DZ'                                          478
         CALL AXSHAPE                                                     478
C    *                                                                    478
C    use RMAT bank                                                        478
         BPATH_RMAT='RMAT*'                                               479
         IDNUM_RMAT(1)=0                                                  479
         RMAT_MATE='QRZO'                                                 479
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  479
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  479
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   479
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         480
     *   RMAT_RINDEX)                                                     480
5291  CALL AGSPOPB                                                        481
      GO TO BLOCK_BARR, (5290 ,5300,5310)                                 481
C End of Local Procedure                                                  481
* - - - - - - - - - - - - - - - - - - - - - - - - - - -
*block META is METANO
* long dimension 146.3 - 2*70 = 132.3 / 2 = 66.15
* short dimension  989 - 2*70 =  84.9 / 2 = 42.45
* thickness is 92.5/2 =46.25
*
*    material  Methane
*    Attribute META  seen=1   colo=4
*    shape     BOX   dx=42.45  dy=4.625   dz=66.15
*    USE       RMAT  Mate='META'
*    CALL GSCKOV(%Imed,N,rang_PPCKOV,rmat_ABSCO,rmat_EFFIC,rmat_RINDEX)
*endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure RGAP                                              495
C RGAP IS METANOL GAP                                                     495
5200  CONTINUE                                                            495
         AG_TITLE='RGAP'                                                  495
         AG_MODULE=MODULE                                                 495
         AG_MARK='V00'                                                    495
         CALL AGSPUSH                                                     495
         AG_MODULE=MODULE                                                 495
C    *                                                                    495
* long dimension 146.3 - 2*70 = 132.3 / 2 = 66.15
* short dimension  989 - 2*70 =  84.9 / 2 = 42.45
C    Geant MATERIAL : METHANE ===> inherited arguments used <===          500
         CALL AGSRESET                                                    500
         AG_TITLE = 'METHANE'                                             500
         AG_EXNAME = 'MATERIAL'                                           500
         AG_PARLIST = 'NONE'                                              500
         CALL AXMATERIAL                                                  500
C    *                                                                    500
C    Geant MEDIUM : METHANE_GAP ===> ISVOL=1 <===                         500
         CALL AGSRESET                                                    501
         AG_ISVOL = 1                                                     501
         AG_TITLE = 'METHANE_GAP'                                         501
         AG_EXNAME = 'MEDIUM'                                             501
         AG_PARLIST = 'ISVOL'                                             501
         CALL AXMEDIUM                                                    501
C    *                                                                    501
C    Geant ATTRIBUTE : RGAP ===> SEEN=1 COLO=4 <===                       501
         CALL AGSRESET                                                    502
         AG_SEEN = 1                                                      502
         AG_COLO = 4                                                      502
         AG_TITLE = 'RGAP'                                                502
         AG_EXNAME = 'ATTRIBUTE'                                          502
         AG_PARLIST = 'SEEN_COLO'                                         502
         CALL AXATTRIBUTE                                                 502
C    *                                                                    502
C    Geant SHAPE : BOX ===> DX=42.45 DY=0.2 DZ=66.15 <===                 502
         CALL AGSRESET                                                    503
         AG_DX = 42.45                                                    503
         AG_DY = 0.2                                                      503
         AG_DZ = 66.15                                                    503
         AG_TITLE = 'BOX'                                                 503
         AG_EXNAME = 'SHAPE'                                              503
         AG_PARLIST = 'DX_DY_DZ'                                          503
         CALL AXSHAPE                                                     503
C    *                                                                    503
*   use methane properties instead of methanol ?
C    use RMAT bank                                                        505
         BPATH_RMAT='RMAT*'                                               505
         IDNUM_RMAT(1)=0                                                  505
         RMAT_MATE='META'                                                 505
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  505
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  505
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   505
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         506
     *   RMAT_RINDEX)                                                     506
C    Defining Hits: X:.01: Y:.01: Z:.01:, CX:10: CY:10: CZ:10:, SLEN:.1:  512
C    (0,500) LPTOT:18:(-3,2), TOF:16:(0,1.E-6) LGAM:16:(-2,2), STEP:16:(  512
C    0,10) ELOSS:16:(0,0.001)                                             512
         AG_TITLE = 'RGAP'                                                512
         AG_EXNAME = 'HITS'                                               512
         AG_TYPE=AG_MAGIC                                                 512
         CALL AGSDETV('RGAP')                                             512
C    *                                                                    512
         AG_PARLIST='X'                                                   512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = AG_MAGIC                                              512
         AG_BIN=.01                                                       512
         AG_FHMIN=AG_MAGIC                                                512
         AG_FHMAX=AG_MAGIC                                                512
         AG_OPTION='*'                                                    512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='Y'                                                   512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = AG_MAGIC                                              512
         AG_BIN=.01                                                       512
         AG_FHMIN=AG_MAGIC                                                512
         AG_FHMAX=AG_MAGIC                                                512
         AG_OPTION='*'                                                    512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='Z'                                                   512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = AG_MAGIC                                              512
         AG_BIN=.01                                                       512
         AG_OPTION='*'                                                    512
         AG_FHMIN = AG_MAGIC                                              512
         AG_FHMAX = AG_MAGIC                                              512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='CX'                                                  512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 10                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_FHMIN=AG_MAGIC                                                512
         AG_FHMAX=AG_MAGIC                                                512
         AG_OPTION='*'                                                    512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='CY'                                                  512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 10                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_FHMIN=AG_MAGIC                                                512
         AG_FHMAX=AG_MAGIC                                                512
         AG_OPTION='*'                                                    512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='CZ'                                                  512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 10                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_OPTION='*'                                                    512
         AG_FHMIN = AG_MAGIC                                              512
         AG_FHMAX = AG_MAGIC                                              512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='SLEN'                                                512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = AG_MAGIC                                              512
         AG_BIN=.1                                                        512
         AG_OPTION='*'                                                    512
         AG_FHMIN = 0                                                     512
         AG_FHMAX = 500                                                   512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='LPTOT'                                               512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 18                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_OPTION='*'                                                    512
         AG_FHMIN = -3                                                    512
         AG_FHMAX = 2                                                     512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='TOF'                                                 512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 16                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_OPTION='*'                                                    512
         AG_FHMIN = 0                                                     512
         AG_FHMAX = 1.E-6                                                 512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='LGAM'                                                512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 16                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_OPTION='*'                                                    512
         AG_FHMIN = -2                                                    512
         AG_FHMAX = 2                                                     512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='STEP'                                                512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 16                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_OPTION='*'                                                    512
         AG_FHMIN = 0                                                     512
         AG_FHMAX = 10                                                    512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         AG_PARLIST='ELOSS'                                               512
         AG_TITLE = 'DETH'                                                512
         AG_NBITS = 16                                                    512
         AG_BIN=AG_MAGIC                                                  512
         AG_OPTION='*'                                                    512
         AG_FHMIN = 0                                                     512
         AG_FHMAX = 0.001                                                 512
         CALL AGSDETH (0)                                                 512
C    *                                                                    512
         CALL AGSDETU('RGAP')                                             512
C    *                                                                    512
5201  CALL AGSPOPB                                                        513
      GO TO BLOCK_RGAP, (5200 ,5210)                                      513
C End of Local Procedure                                                  513
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure HOLE                                              515
C HOLE IS AN OPPENING IN THE ALUMINUM FRAME                               515
5240  CONTINUE                                                            515
         AG_TITLE='HOLE'                                                  515
         AG_MODULE=MODULE                                                 515
         AG_MARK='V00'                                                    515
         CALL AGSPUSH                                                     515
         AG_MODULE=MODULE                                                 515
C    *                                                                    515
C    Geant MATERIAL : AIR ===> inherited arguments used <===              516
         CALL AGSRESET                                                    516
         AG_TITLE = 'AIR'                                                 516
         AG_EXNAME = 'MATERIAL'                                           516
         AG_PARLIST = 'NONE'                                              516
         CALL AXMATERIAL                                                  516
C    *                                                                    516
C    Geant ATTRIBUTE : HOLE ===> SEEN=1 COLO=4 <===                       516
         CALL AGSRESET                                                    517
         AG_SEEN = 1                                                      517
         AG_COLO = 4                                                      517
         AG_TITLE = 'HOLE'                                                517
         AG_EXNAME = 'ATTRIBUTE'                                          517
         AG_PARLIST = 'SEEN_COLO'                                         517
         CALL AXATTRIBUTE                                                 517
C    *                                                                    517
C    Geant SHAPE : BOX ===> DX=19.65 DY=1.6 DZ=32.00 <===                 517
         CALL AGSRESET                                                    518
         AG_DX = 19.65                                                    518
         AG_DY = 1.6                                                      518
         AG_DZ = 32.00                                                    518
         AG_TITLE = 'BOX'                                                 518
         AG_EXNAME = 'SHAPE'                                              518
         AG_PARLIST = 'DX_DY_DZ'                                          518
         CALL AXSHAPE                                                     518
C    *                                                                    518
*   use methane properties instead of air ?
C    use RMAT bank                                                        520
         BPATH_RMAT='RMAT*'                                               520
         IDNUM_RMAT(1)=0                                                  520
         RMAT_MATE='META'                                                 520
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  520
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  520
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   520
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         521
     *   RMAT_RINDEX)                                                     521
C    Create BORD :                                                        523
C    Call To Local Procedure BORD                                         523
         ASSIGN 5330 TO BLOCK_BORD                                        523
         GO TO  5320                                                      523
5330     CONTINUE                                                         523
C    *                                                                    523
C    Geant POSITION : BORD ===> X=0 Y=-1.35 Z=0 <===                      523
         CALL AGSRESET                                                    523
         AG_X = 0                                                         523
         AG_Y = -1.35                                                     523
         AG_Z = 0                                                         523
         AG_TITLE = 'BORD'                                                523
         AG_EXNAME = 'POSITION'                                           523
         AG_PARLIST = 'X_Y_Z'                                             523
         CALL AXPOSITION                                                  523
C    *                                                                    523
C    Create RCSI :                                                        524
C    Call To Local Procedure RCSI                                         524
         ASSIGN 5350 TO BLOCK_RCSI                                        524
         GO TO  5340                                                      524
5350     CONTINUE                                                         524
C    *                                                                    524
C    Geant POSITION : RCSI ===> X=0 Y=-1.575 Z=0 <===                     524
         CALL AGSRESET                                                    524
         AG_X = 0                                                         524
         AG_Y = -1.575                                                    524
         AG_Z = 0                                                         524
         AG_TITLE = 'RCSI'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'X_Y_Z'                                             524
         CALL AXPOSITION                                                  524
C    *                                                                    524
5241  CALL AGSPOPB                                                        524
      GO TO BLOCK_HOLE, (5240 ,5250,5260,5270,5280)                       525
C End of Local Procedure                                                  525
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure BORD                                              527
C BORD IS CARBON                                                          527
5320  CONTINUE                                                            527
         AG_TITLE='BORD'                                                  527
         AG_MODULE=MODULE                                                 527
         AG_MARK='V00'                                                    527
         CALL AGSPUSH                                                     527
         AG_MODULE=MODULE                                                 527
C    *                                                                    527
C    Geant MATERIAL : CARBONIO ===> inherited arguments used <===         528
         CALL AGSRESET                                                    528
         AG_TITLE = 'CARBONIO'                                            528
         AG_EXNAME = 'MATERIAL'                                           528
         AG_PARLIST = 'NONE'                                              528
         CALL AXMATERIAL                                                  528
C    *                                                                    528
C    Geant ATTRIBUTE : BORD ===> SEEN=1 COLO=6 <===                       528
         CALL AGSRESET                                                    529
         AG_SEEN = 1                                                      529
         AG_COLO = 6                                                      529
         AG_TITLE = 'BORD'                                                529
         AG_EXNAME = 'ATTRIBUTE'                                          529
         AG_PARLIST = 'SEEN_COLO'                                         529
         CALL AXATTRIBUTE                                                 529
C    *                                                                    529
C    Geant SHAPE : BOX ===> DX=19.65 DY=0.2 DZ=32.0 <===                  529
         CALL AGSRESET                                                    530
         AG_DX = 19.65                                                    530
         AG_DY = 0.2                                                      530
         AG_DZ = 32.0                                                     530
         AG_TITLE = 'BOX'                                                 530
         AG_EXNAME = 'SHAPE'                                              530
         AG_PARLIST = 'DX_DY_DZ'                                          530
         CALL AXSHAPE                                                     530
C    *                                                                    530
C    use RMAT bank                                                        530
         BPATH_RMAT='RMAT*'                                               531
         IDNUM_RMAT(1)=0                                                  531
         RMAT_MATE='GRI'                                                  531
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  531
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  531
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   531
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         532
     *   RMAT_RINDEX)                                                     532
5321  CALL AGSPOPB                                                        533
      GO TO BLOCK_BORD, (5320 ,5330)                                      533
C End of Local Procedure                                                  533
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure RCSI                                              535
C RCSI IS CESIUM IODIDE                                                   535
5340  CONTINUE                                                            535
         AG_TITLE='RCSI'                                                  535
         AG_MODULE=MODULE                                                 535
         AG_MARK='V00'                                                    535
         CALL AGSPUSH                                                     535
         AG_MODULE=MODULE                                                 535
C    *                                                                    535
* changed to four blocks
* frame size of holder is 72 each side
* frame along long dimension is 4.5
* frame along short dimension is 9.5
* 1463 - 72 - 72 - 21 = 1298 / 2 = 649 - 2*4.5 = 640
* long dimension  640 /2 = 32.0 this is 80 pads
* 1320 mm is 165 pads 5 empty in the center offset in z = 66.0
*
*  989  - 72 - 72 - 21 = 824 / 2 = 412 - 2*9.5 = 393
* short dimension (393-2*0.5) /2 = 19.60 this is 49 pads
*  392*2+30 = 814 /2 =  (40.7 + 0.1) * 2 = 816 is 102 pads 3 empty offest 40.8
*   call  AgSSTEP(RGJKSTEP)
C    Geant MATERIAL : CARBONIO ===> inherited arguments used <===         550
         CALL AGSRESET                                                    550
         AG_TITLE = 'CARBONIO'                                            550
         AG_EXNAME = 'MATERIAL'                                           550
         AG_PARLIST = 'NONE'                                              550
         CALL AXMATERIAL                                                  550
C    *                                                                    550
C    Geant MEDIUM : CSI ===> ISVOL=1 <===                                 550
         CALL AGSRESET                                                    551
         AG_ISVOL = 1                                                     551
         AG_TITLE = 'CSI'                                                 551
         AG_EXNAME = 'MEDIUM'                                             551
         AG_PARLIST = 'ISVOL'                                             551
         CALL AXMEDIUM                                                    551
C    *                                                                    551
C    Geant ATTRIBUTE : RCSI ===> SEEN=1 COLO=6 <===                       551
         CALL AGSRESET                                                    552
         AG_SEEN = 1                                                      552
         AG_COLO = 6                                                      552
         AG_TITLE = 'RCSI'                                                552
         AG_EXNAME = 'ATTRIBUTE'                                          552
         AG_PARLIST = 'SEEN_COLO'                                         552
         CALL AXATTRIBUTE                                                 552
C    *                                                                    552
C    Geant SHAPE : BOX ===> DX=19.60 DY=0.025 DZ=32.0 <===                552
         CALL AGSRESET                                                    553
         AG_DX = 19.60                                                    553
         AG_DY = 0.025                                                    553
         AG_DZ = 32.0                                                     553
         AG_TITLE = 'BOX'                                                 553
         AG_EXNAME = 'SHAPE'                                              553
         AG_PARLIST = 'DX_DY_DZ'                                          553
         CALL AXSHAPE                                                     553
C    *                                                                    553
*   refraction index same as in methane ?
C    use RMAT bank                                                        555
         BPATH_RMAT='RMAT*'                                               555
         IDNUM_RMAT(1)=0                                                  555
         RMAT_MATE='CSI'                                                  555
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  555
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  555
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   555
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         556
     *   RMAT_RINDEX)                                                     556
C    Defining Hits: X:.005: Y:.01: Z:.005:, CX:10: CY:10: CZ:10:, SLEN:.  562
C    1:(0,500) LPTOT:18:(-3,2), TOF:16:(0,1.E-6) LGAM:16:(-2,2), STEP:16  562
C    :(0,10) USER:32:(-0.01,0.01)                                         562
         AG_TITLE = 'RCSI'                                                562
         AG_EXNAME = 'HITS'                                               562
         AG_TYPE=AG_MAGIC                                                 562
         CALL AGSDETV('RCSI')                                             562
C    *                                                                    562
         AG_PARLIST='X'                                                   562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = AG_MAGIC                                              562
         AG_BIN=.005                                                      562
         AG_FHMIN=AG_MAGIC                                                562
         AG_FHMAX=AG_MAGIC                                                562
         AG_OPTION='*'                                                    562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='Y'                                                   562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = AG_MAGIC                                              562
         AG_BIN=.01                                                       562
         AG_FHMIN=AG_MAGIC                                                562
         AG_FHMAX=AG_MAGIC                                                562
         AG_OPTION='*'                                                    562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='Z'                                                   562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = AG_MAGIC                                              562
         AG_BIN=.005                                                      562
         AG_OPTION='*'                                                    562
         AG_FHMIN = AG_MAGIC                                              562
         AG_FHMAX = AG_MAGIC                                              562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='CX'                                                  562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 10                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_FHMIN=AG_MAGIC                                                562
         AG_FHMAX=AG_MAGIC                                                562
         AG_OPTION='*'                                                    562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='CY'                                                  562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 10                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_FHMIN=AG_MAGIC                                                562
         AG_FHMAX=AG_MAGIC                                                562
         AG_OPTION='*'                                                    562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='CZ'                                                  562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 10                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_OPTION='*'                                                    562
         AG_FHMIN = AG_MAGIC                                              562
         AG_FHMAX = AG_MAGIC                                              562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='SLEN'                                                562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = AG_MAGIC                                              562
         AG_BIN=.1                                                        562
         AG_OPTION='*'                                                    562
         AG_FHMIN = 0                                                     562
         AG_FHMAX = 500                                                   562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='LPTOT'                                               562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 18                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_OPTION='*'                                                    562
         AG_FHMIN = -3                                                    562
         AG_FHMAX = 2                                                     562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='TOF'                                                 562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 16                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_OPTION='*'                                                    562
         AG_FHMIN = 0                                                     562
         AG_FHMAX = 1.E-6                                                 562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='LGAM'                                                562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 16                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_OPTION='*'                                                    562
         AG_FHMIN = -2                                                    562
         AG_FHMAX = 2                                                     562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='STEP'                                                562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 16                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_OPTION='*'                                                    562
         AG_FHMIN = 0                                                     562
         AG_FHMAX = 10                                                    562
         CALL AGSDETH (0)                                                 562
C    *                                                                    562
         AG_PARLIST='USER'                                                562
         AG_TITLE = 'DETH'                                                562
         AG_NBITS = 32                                                    562
         AG_BIN=AG_MAGIC                                                  562
         AG_OPTION='*'                                                    562
         AG_FHMIN = -0.01                                                 562
         AG_FHMAX = 0.01                                                  562
         CALL AGSDETH (JUMPAD(RCSISTEP))                                  562
C    *                                                                    562
         CALL AGSDETU('RCSI')                                             562
C    *                                                                    562
5341  CALL AGSPOPB                                                        563
      GO TO BLOCK_RCSI, (5340 ,5350)                                      563
C End of Local Procedure                                                  563
*- - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure OQUF                                              565
C OQUF IS FRAME QUARZO OPACO                                              565
5140  CONTINUE                                                            565
         AG_TITLE='OQUF'                                                  565
         AG_MODULE=MODULE                                                 565
         AG_MARK='V00'                                                    565
         CALL AGSPUSH                                                     565
         AG_MODULE=MODULE                                                 565
C    *                                                                    565
* 1330 /2 = 66.5 bigger than inside box
* 413  /2 = 20.65
* thickness 10 / 2 = 0.5
* positioning width 433
C    Geant MATERIAL : OPACO ===> inherited arguments used <===            572
         CALL AGSRESET                                                    572
         AG_TITLE = 'OPACO'                                               572
         AG_EXNAME = 'MATERIAL'                                           572
         AG_PARLIST = 'NONE'                                              572
         CALL AXMATERIAL                                                  572
C    *                                                                    572
C    Geant ATTRIBUTE : OQUF ===> SEEN=1 COLO=2 <===                       572
         CALL AGSRESET                                                    573
         AG_SEEN = 1                                                      573
         AG_COLO = 2                                                      573
         AG_TITLE = 'OQUF'                                                573
         AG_EXNAME = 'ATTRIBUTE'                                          573
         AG_PARLIST = 'SEEN_COLO'                                         573
         CALL AXATTRIBUTE                                                 573
C    *                                                                    573
C    Geant SHAPE : BOX ===> DX=20.65 DY=0.5 DZ=66.5 <===                  573
         CALL AGSRESET                                                    574
         AG_DX = 20.65                                                    574
         AG_DY = 0.5                                                      574
         AG_DZ = 66.5                                                     574
         AG_TITLE = 'BOX'                                                 574
         AG_EXNAME = 'SHAPE'                                              574
         AG_PARLIST = 'DX_DY_DZ'                                          574
         CALL AXSHAPE                                                     574
C    *                                                                    574
C    use RMAT bank                                                        574
         BPATH_RMAT='RMAT*'                                               575
         IDNUM_RMAT(1)=0                                                  575
         RMAT_MATE='QRZO'                                                 575
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  575
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  575
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   575
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         576
     *   RMAT_RINDEX)                                                     576
C    Create FREO :                                                        578
C    Call To Local Procedure FREO                                         578
         ASSIGN 5370 TO BLOCK_FREO                                        578
         GO TO  5360                                                      578
5370     CONTINUE                                                         578
C    *                                                                    578
C    Geant POSITION : FREO ===> inherited arguments used <===             578
         CALL AGSRESET                                                    578
         AG_TITLE = 'FREO'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'NONE'                                              578
         CALL AXPOSITION                                                  578
C    *                                                                    578
5141  CALL AGSPOPB                                                        578
      GO TO BLOCK_OQUF, (5140 ,5150,5160)                                 579
C End of Local Procedure                                                  579
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure FREO                                              581
C FREO IS FREON                                                           581
5360  CONTINUE                                                            581
         AG_TITLE='FREO'                                                  581
         AG_MODULE=MODULE                                                 581
         AG_MARK='V00'                                                    581
         CALL AGSPUSH                                                     581
         AG_MODULE=MODULE                                                 581
C    *                                                                    581
* inside dimensions
* 1310 /2 = 65.5
* 413 of the frame - 2*0.5 = 403  /2 = 20.15
C    Geant COMPONENT : C ===> A=12 Z=6 W=6 <===                           587
         CALL AGSRESET                                                    587
         AG_A = 12                                                        587
         AG_Z = 6                                                         587
         AG_W = 6                                                         587
         AG_TITLE = 'C'                                                   587
         AG_EXNAME = 'COMPONENT'                                          587
         AG_PARLIST = 'A_Z_W'                                             587
         CALL AXCOMPONENT                                                 587
C    *                                                                    587
C    Geant COMPONENT : F ===> A=19 Z=9 W=14 <===                          587
         CALL AGSRESET                                                    588
         AG_A = 19                                                        588
         AG_Z = 9                                                         588
         AG_W = 14                                                        588
         AG_TITLE = 'F'                                                   588
         AG_EXNAME = 'COMPONENT'                                          588
         AG_PARLIST = 'A_Z_W'                                             588
         CALL AXCOMPONENT                                                 588
C    *                                                                    588
C    Geant MIXTURE : FREON ===> DENS=1.7 ISVOL=ISOPT <===                 588
         CALL AGSRESET                                                    589
         AG_DENS = 1.7                                                    589
         AG_ISVOL = ISOPT                                                 589
         AG_TITLE = 'FREON'                                               589
         AG_EXNAME = 'MIXTURE'                                            589
         AG_PARLIST = 'DENS_ISVOL'                                        589
         CALL AXMIXTURE                                                   589
C    *                                                                    589
C    Geant ATTRIBUTE : FREO ===> SEEN=1 COLO=3 <===                       591
         CALL AGSRESET                                                    591
         AG_SEEN = 1                                                      591
         AG_COLO = 3                                                      591
         AG_TITLE = 'FREO'                                                591
         AG_EXNAME = 'ATTRIBUTE'                                          591
         AG_PARLIST = 'SEEN_COLO'                                         591
         CALL AXATTRIBUTE                                                 591
C    *                                                                    591
C    Geant SHAPE : BOX ===> DX=20.15 DY=0.5 DZ=65.5 <===                  591
         CALL AGSRESET                                                    592
         AG_DX = 20.15                                                    592
         AG_DY = 0.5                                                      592
         AG_DZ = 65.5                                                     592
         AG_TITLE = 'BOX'                                                 592
         AG_EXNAME = 'SHAPE'                                              592
         AG_PARLIST = 'DX_DY_DZ'                                          592
         CALL AXSHAPE                                                     592
C    *                                                                    592
C    use RMAT bank                                                        592
         BPATH_RMAT='RMAT*'                                               593
         IDNUM_RMAT(1)=0                                                  593
         RMAT_MATE='FREO'                                                 593
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  593
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  593
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   593
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         594
     *   RMAT_RINDEX)                                                     594
C    *                                                                    596
         I=1                                                              596
            GO TO 5383                                                    596
5381        I=I+1                                                         596
5383        IF(I-(9).GT.0) GO TO 5382                                     596
C       Create SPAC :                                                     597
C       Call To Local Procedure SPAC                                      597
            ASSIGN 5400 TO BLOCK_SPAC                                     597
            GO TO  5390                                                   597
5400        CONTINUE                                                      597
C       *                                                                 597
C       Geant POSITION : SPAC ===> X=+6.7 Z=(5-I)*14.4 ORT=ZXY <===       597
            CALL AGSRESET                                                 597
            AG_X = +6.7                                                   597
            AG_Z = (5-I)*14.4                                             597
            AG_ALPHAN=AG_ALPHAN+1                                         597
            AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                              597
            AG_TITLE = 'SPAC'                                             597
            AG_EXNAME = 'POSITION'                                        597
            AG_PARLIST = 'X_Z_ORT'                                        597
            CALL AXPOSITION                                               597
C       *                                                                 597
C       Create SPAC :                                                     598
C       Call To Local Procedure SPAC                                      598
            ASSIGN 5410 TO BLOCK_SPAC                                     598
            GO TO  5390                                                   598
5410        CONTINUE                                                      598
C       *                                                                 598
C       Geant POSITION : SPAC ===> X=-6.7 Z=(5-I)*14.4 ORT=ZXY <===       598
            CALL AGSRESET                                                 598
            AG_X = -6.7                                                   598
            AG_Z = (5-I)*14.4                                             598
            AG_ALPHAN=AG_ALPHAN+1                                         598
            AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                              598
            AG_TITLE = 'SPAC'                                             598
            AG_EXNAME = 'POSITION'                                        598
            AG_PARLIST = 'X_Z_ORT'                                        598
            CALL AXPOSITION                                               598
C       *                                                                 598
         GO TO 5381                                                       599
5382     CONTINUE                                                         599
C    Check ISOPT>0                                                        601
         IF (ISOPT.GT.0) THEN                                             601
C    Defining Hits: X:.01: Y:.01: Z:.01:, CX:10: CY:10: CZ:10:, SLEN:.1:  606
C    (0,500) LPTOT:18:(-3,2), TOF:16:(0,1.E-6) LGAM:16:(-2,2), STEP:16:(  606
C    0,10) ELOSS:16:(0,0.001)                                             606
         AG_TITLE = 'FREO'                                                606
         AG_EXNAME = 'HITS'                                               606
         AG_TYPE=AG_MAGIC                                                 606
         CALL AGSDETV('FREO')                                             606
C    *                                                                    606
         AG_PARLIST='X'                                                   606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = AG_MAGIC                                              606
         AG_BIN=.01                                                       606
         AG_FHMIN=AG_MAGIC                                                606
         AG_FHMAX=AG_MAGIC                                                606
         AG_OPTION='*'                                                    606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='Y'                                                   606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = AG_MAGIC                                              606
         AG_BIN=.01                                                       606
         AG_FHMIN=AG_MAGIC                                                606
         AG_FHMAX=AG_MAGIC                                                606
         AG_OPTION='*'                                                    606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='Z'                                                   606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = AG_MAGIC                                              606
         AG_BIN=.01                                                       606
         AG_OPTION='*'                                                    606
         AG_FHMIN = AG_MAGIC                                              606
         AG_FHMAX = AG_MAGIC                                              606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='CX'                                                  606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 10                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_FHMIN=AG_MAGIC                                                606
         AG_FHMAX=AG_MAGIC                                                606
         AG_OPTION='*'                                                    606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='CY'                                                  606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 10                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_FHMIN=AG_MAGIC                                                606
         AG_FHMAX=AG_MAGIC                                                606
         AG_OPTION='*'                                                    606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='CZ'                                                  606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 10                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_OPTION='*'                                                    606
         AG_FHMIN = AG_MAGIC                                              606
         AG_FHMAX = AG_MAGIC                                              606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='SLEN'                                                606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = AG_MAGIC                                              606
         AG_BIN=.1                                                        606
         AG_OPTION='*'                                                    606
         AG_FHMIN = 0                                                     606
         AG_FHMAX = 500                                                   606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='LPTOT'                                               606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 18                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_OPTION='*'                                                    606
         AG_FHMIN = -3                                                    606
         AG_FHMAX = 2                                                     606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='TOF'                                                 606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 16                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_OPTION='*'                                                    606
         AG_FHMIN = 0                                                     606
         AG_FHMAX = 1.E-6                                                 606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='LGAM'                                                606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 16                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_OPTION='*'                                                    606
         AG_FHMIN = -2                                                    606
         AG_FHMAX = 2                                                     606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='STEP'                                                606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 16                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_OPTION='*'                                                    606
         AG_FHMIN = 0                                                     606
         AG_FHMAX = 10                                                    606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         AG_PARLIST='ELOSS'                                               606
         AG_TITLE = 'DETH'                                                606
         AG_NBITS = 16                                                    606
         AG_BIN=AG_MAGIC                                                  606
         AG_OPTION='*'                                                    606
         AG_FHMIN = 0                                                     606
         AG_FHMAX = 0.001                                                 606
         CALL AGSDETH (0)                                                 606
C    *                                                                    606
         CALL AGSDETU('FREO')                                             606
C    *                                                                    606
         ENDIF                                                            607
5361  CALL AGSPOPB                                                        608
      GO TO BLOCK_FREO, (5360 ,5370)                                      608
C End of Local Procedure                                                  608
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure SPAC                                              610
C SPAC E SPACERS (QUARZ CYLINDERS)                                        610
5390  CONTINUE                                                            610
         AG_TITLE='SPAC'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
* 9 spacers center is z=0
* spaced 144
* distance from the middle 134.0
* 1 cm cylinders
C    Geant MATERIAL : QUARZ ===> inherited arguments used <===            617
         CALL AGSRESET                                                    617
         AG_TITLE = 'QUARZ'                                               617
         AG_EXNAME = 'MATERIAL'                                           617
         AG_PARLIST = 'NONE'                                              617
         CALL AXMATERIAL                                                  617
C    *                                                                    617
C    Geant ATTRIBUTE : SPAC ===> SEEN=1 COLO=2 <===                       617
         CALL AGSRESET                                                    618
         AG_SEEN = 1                                                      618
         AG_COLO = 2                                                      618
         AG_TITLE = 'SPAC'                                                618
         AG_EXNAME = 'ATTRIBUTE'                                          618
         AG_PARLIST = 'SEEN_COLO'                                         618
         CALL AXATTRIBUTE                                                 618
C    *                                                                    618
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=.5 DZ=0.5 <===                   618
         CALL AGSRESET                                                    619
         AG_RMIN = 0                                                      619
         AG_RMAX = .5                                                     619
         AG_DZ = 0.5                                                      619
         AG_TITLE = 'TUBE'                                                619
         AG_EXNAME = 'SHAPE'                                              619
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      619
         CALL AXSHAPE                                                     619
C    *                                                                    619
C    use RMAT bank                                                        619
         BPATH_RMAT='RMAT*'                                               620
         IDNUM_RMAT(1)=0                                                  620
         RMAT_MATE='QARZ'                                                 620
         CALL AGDGETP('RICHGEO','*',-999,'RMAT',NRMAT_MATE,RE_RMAT_MATE,  620
     *   BPATH_RMAT,IDNUM_RMAT,1,MAP_RMAT,NAMES_RMAT,BANK_RMAT,LEN_RMAT,  620
     *   LLEN_RMAT,LINK_RMAT,FLAG_RMAT)                                   620
         CALL GSCKOV(AG_IMED,N,RANG_PPCKOV,RMAT_ABSCO,RMAT_EFFIC,         621
     *   RMAT_RINDEX)                                                     621
5391  CALL AGSPOPB                                                        622
      GO TO BLOCK_SPAC, (5390 ,5400,5410)                                 622
C End of Local Procedure                                                  622
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*Block awire
* inside dimensions
* long dimension 146.3 - 2*70 = 132.3 / 2 = 66.15
* anode wires 20 microns 4 millimeter pitch
*
*    material  anode
*    attribute SPAC seen=1 colo=2
*    shape TUBE rmin=0 Rmax=0.0020 dy= 66.15
*endblock
      END                                                                 634
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE RICHSTEP                                                 638
* discard cerenkov photon absorption hits everywhere except for CSI
      IMPLICIT NONE                                                       640
C - Dynamic core for the GEANT data (v3.15)                               640
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   640
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        640
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  640
     *LQ,JCG                                                              640
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       640
      PARAMETER (KWBANK=69000)                                            640
      PARAMETER (KWWORK=5200)                                             640
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           640
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      640
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      640
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    640
     *JGPAR,JGPAR2,JSKLT                                                  640
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                640
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   640
     *JGSTAT)                                                             640
C - Natural constants (or considered as such)                             640
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   640
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        640
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       640
C - GEANT description of logical units                                    640
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            640
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      640
      COMMON/GCMAIL/ CHMAIL                                               640
      CHARACTER*132 CHMAIL                                                640
C - parameters of current tracking medium                                 640
      INTEGER NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD                640
      REAL FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC            640
      COMMON/GCTMED/ NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX,   640
     *DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD                   640
      REAL THRIND,PMIN,DP,DNDL                                            640
      INTEGER JMIN,ITCKOV,IMCKOV,NPCKOV                                   640
      COMMON/GCTLIT/ THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV        640
C - track parameters at current point                                     640
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    640
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               640
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  640
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           640
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           640
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      640
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       640
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     640
C - Kinematics of current track                                           640
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           640
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            640
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       640
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               640
C - Identification of the current sensitive detector                      640
      INTEGER IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV                   640
      COMMON /GCSETS/ IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)       640
C Connection between AgGSTEP, AgGHIT and a user step routine              640
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   640
      INTEGER IHAVEMORE                                                   640
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    640
      COMMON /AGCSTEPH/ HITS(15)                                          640
      COMMON /AGCSTEPI/ IHAVEMORE                                         640
      CHARACTER CMED*8                                                    640
C Skip Unless ISVOL>0                                                     643
      IF (ISVOL.LE.0)GO TO 1                                              643
      CALL UHTOC(NATMED,4,CMED,8)                                         644
C Skip Unless CMED!='RICH_CSI'                                            645
      IF (CMED.EQ.'RICH_CSI')GO TO 1                                      645
* cerenkov photons are seen in CSI only
* secondaries are not interesting ???
C Skip Unless IPART==50                                                   649
      IF (IPART.NE.50)GO TO 1                                             649
* discard hit:
      STEP=0                                                              651
      DESTEP=0                                                            651
      ASTEP=0                                                             651
      ADESTEP=0                                                           651
*
1     CONTINUE                                                            654
      END                                                                 654
      SUBROUTINE RCSISTEP(JJ,HIT)                                         657
      IMPLICIT NONE                                                       658
C - Dynamic core for the GEANT data (v3.15)                               658
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   658
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        658
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  658
     *LQ,JCG                                                              658
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       658
      PARAMETER (KWBANK=69000)                                            658
      PARAMETER (KWWORK=5200)                                             658
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           658
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      658
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      658
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    658
     *JGPAR,JGPAR2,JSKLT                                                  658
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                658
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   658
     *JGSTAT)                                                             658
C - Natural constants (or considered as such)                             658
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   658
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        658
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       658
C - GEANT description of logical units                                    658
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            658
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      658
      COMMON/GCMAIL/ CHMAIL                                               658
      CHARACTER*132 CHMAIL                                                658
C - track parameters at current point                                     658
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    658
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               658
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  658
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           658
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           658
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      658
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       658
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     658
C - Kinematics of current track                                           658
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           658
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            658
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       658
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               658
C - Identification of the current sensitive detector                      658
      INTEGER IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV                   658
      COMMON /GCSETS/ IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)       658
C Connection between AgGSTEP, AgGHIT and a user step routine              658
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   658
      INTEGER IHAVEMORE                                                   658
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    658
      COMMON /AGCSTEPH/ HITS(15)                                          658
      COMMON /AGCSTEPI/ IHAVEMORE                                         658
      INTEGER JJ                                                          658
      REAL HIT                                                            660
C *                                                                       661
C    Check IPART != 50                                                    661
         IF (IPART .NE. 50) THEN                                          661
         HIT=ADESTEP                                                      661
         RETURN                                                           662
      END IF                                                              662
      HIT = -ADESTEP                                                      663
      END                                                                 664
