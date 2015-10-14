C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FPDMGEO2                                                 1
C FPDMGEO2 IS THE FORWARD PION DETECTOR MODULES GEOMETRY                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FPDMGEO2'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE FORWARD PION DETECTOR MODULES GEOMETRY'/      1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 12 JUN 2006                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'12 JUN 2006'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR AKIO OGAWA                                                       2
      CHARACTER*40 AUTHOR /'AKIO OGAWA'/                                  3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C - Advanced Geant interface                                              4
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      4
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       4
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         4
     *AG_OPTION,AG_ORT,AG_MARK                                            4
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         4
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     4
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     4
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    4
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          4
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   4
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     4
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        4
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   4
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 4
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     4
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       4
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        4
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       4
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       4
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       4
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         4
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     4
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       4
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   4
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       4
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     4
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          4
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         4
     *-696969,AG_LDETU=250)                                               4
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    4
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  4
     *AG_IMFLD,AG_ISIMU                                                   4
C Inherited variables a: saved during internal calls                      4
C b: reset at the entry in a block and then saved                         4
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        4
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       4
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         4
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     4
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       4
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         4
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           4
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        4
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        4
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            4
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           4
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    4
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      4
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           4
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          4
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                4
C local variables valid inside same block                                 4
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       4
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  4
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         4
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    4
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          4
     *AG_OPTION                                                           4
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        4
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             4
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              4
C temporarly                                                              4
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           4
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    4
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       4
     *AG_HY)                                                              4
      REAL MIL_P,INCH_P                                                   4
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               4
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            6
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      6
      COMMON/GCMAIL/ CHMAIL                                               6
      CHARACTER*132 CHMAIL                                                6
      INTEGER BLOCK_FBOX                                                  6
      INTEGER BLOCK_FLGT                                                  7
      INTEGER BLOCK_FLGF                                                  7
      INTEGER BLOCK_FWAL                                                  7
      INTEGER BLOCK_FLGR                                                  7
      INTEGER BLOCK_FPCT                                                  7
      INTEGER BLOCK_FUMT                                                  7
      INTEGER BLOCK_PBPT                                                  7
      INTEGER BLOCK_FSHM                                                  7
      INTEGER BLOCK_FHMS                                                  7
      INTEGER BLOCK_FXGT                                                  7
C -                                                                       8
      REAL FMCG_VERSION                                                   8
      EQUIVALENCE (BANK_FMCG(3),FMCG_VERSION,RE_FMCG_VERSION),            8
     *(NAMES_FMCG(1),NFMCG_VERSION)                                       8
      EQUIVALENCE (COMMEN_FMCG(1),CFMCG_VERSION),(MAP_FMCG(1,1),          8
     *MFMCG_VERSION)                                                      8
      CHARACTER CFMCG_VERSION*40/' '/                                     8
      CHARACTER NFMCG_VERSION*12/'R.VERSION'/                             8
      INTEGER MFMCG_VERSION(2)/1,1/                                       8
      REAL RE_FMCG_VERSION                                                8
C -                                                                       8
      REAL FMCG_CHKVSIM                                                   8
      EQUIVALENCE (BANK_FMCG(4),FMCG_CHKVSIM,RE_FMCG_CHKVSIM),            8
     *(NAMES_FMCG(2),NFMCG_CHKVSIM)                                       8
      EQUIVALENCE (COMMEN_FMCG(2),CFMCG_CHKVSIM),(MAP_FMCG(1,2),          8
     *MFMCG_CHKVSIM)                                                      8
      CHARACTER CFMCG_CHKVSIM*40/' '/                                     8
      CHARACTER NFMCG_CHKVSIM*12/'R.CHKVSIM'/                             8
      INTEGER MFMCG_CHKVSIM(2)/1,1/                                       8
      REAL RE_FMCG_CHKVSIM                                                8
C -                                                                       8
      REAL FMCG_PBPLATE                                                   8
      EQUIVALENCE (BANK_FMCG(5),FMCG_PBPLATE,RE_FMCG_PBPLATE),            8
     *(NAMES_FMCG(3),NFMCG_PBPLATE)                                       8
      EQUIVALENCE (COMMEN_FMCG(3),CFMCG_PBPLATE),(MAP_FMCG(1,3),          8
     *MFMCG_PBPLATE)                                                      8
      CHARACTER CFMCG_PBPLATE*40/' '/                                     8
      CHARACTER NFMCG_PBPLATE*12/'R.PBPLATE'/                             8
      INTEGER MFMCG_PBPLATE(2)/1,1/                                       8
      REAL RE_FMCG_PBPLATE                                                8
C -                                                                       8
      INTEGER LEN_FMCG,LLEN_FMCG,LINK_FMCG                                8
      PARAMETER (LEN_FMCG=5)                                              8
      PARAMETER (LLEN_FMCG=3)                                             8
      INTEGER FLAG_FMCG/0/,MAP_FMCG(2,LLEN_FMCG),IDNUM_FMCG(10)           8
      CHARACTER NAMES_FMCG(LLEN_FMCG)*12,COMMEN_FMCG(LLEN_FMCG)*40        8
      DATA IDNUM_FMCG/10*0/                                               8
      REAL BANK_FMCG(LEN_FMCG)                                            8
      CHARACTER*60 BPATH_FMCG/' '/                                        8
      SAVE FLAG_FMCG,MAP_FMCG,IDNUM_FMCG,NAMES_FMCG,COMMEN_FMCG,          8
     *BPATH_FMCG                                                          8
      DATA LINK_FMCG/0/,BANK_FMCG/LEN_FMCG*0/                             8
C *                                                                       8
C -                                                                       9
      REAL FPOS_IMOD                                                      9
      EQUIVALENCE (BANK_FPOS(3),FPOS_IMOD,RE_FPOS_IMOD),(NAMES_FPOS(1),   9
     *NFPOS_IMOD)                                                         9
      EQUIVALENCE (COMMEN_FPOS(1),CFPOS_IMOD),(MAP_FPOS(1,1),             9
     *MFPOS_IMOD)                                                         9
      CHARACTER CFPOS_IMOD*40/' '/                                        9
      CHARACTER NFPOS_IMOD*12/'R.IMOD'/                                   9
      INTEGER MFPOS_IMOD(2)/1,1/                                          9
      REAL RE_FPOS_IMOD                                                   9
C -                                                                       9
      REAL FPOS_ITYPE                                                     9
      EQUIVALENCE (BANK_FPOS(4),FPOS_ITYPE,RE_FPOS_ITYPE),                9
     *(NAMES_FPOS(2),NFPOS_ITYPE)                                         9
      EQUIVALENCE (COMMEN_FPOS(2),CFPOS_ITYPE),(MAP_FPOS(1,2),            9
     *MFPOS_ITYPE)                                                        9
      CHARACTER CFPOS_ITYPE*40/' '/                                       9
      CHARACTER NFPOS_ITYPE*12/'R.ITYPE'/                                 9
      INTEGER MFPOS_ITYPE(2)/1,1/                                         9
      REAL RE_FPOS_ITYPE                                                  9
C -                                                                       9
      REAL FPOS_X                                                         9
      EQUIVALENCE (BANK_FPOS(5),FPOS_X,RE_FPOS_X),(NAMES_FPOS(3),         9
     *NFPOS_X)                                                            9
      EQUIVALENCE (COMMEN_FPOS(3),CFPOS_X),(MAP_FPOS(1,3),MFPOS_X)        9
      CHARACTER CFPOS_X*40/' '/                                           9
      CHARACTER NFPOS_X*12/'R.X'/                                         9
      INTEGER MFPOS_X(2)/1,1/                                             9
      REAL RE_FPOS_X                                                      9
C -                                                                       9
      REAL FPOS_Y                                                         9
      EQUIVALENCE (BANK_FPOS(6),FPOS_Y,RE_FPOS_Y),(NAMES_FPOS(4),         9
     *NFPOS_Y)                                                            9
      EQUIVALENCE (COMMEN_FPOS(4),CFPOS_Y),(MAP_FPOS(1,4),MFPOS_Y)        9
      CHARACTER CFPOS_Y*40/' '/                                           9
      CHARACTER NFPOS_Y*12/'R.Y'/                                         9
      INTEGER MFPOS_Y(2)/1,1/                                             9
      REAL RE_FPOS_Y                                                      9
C -                                                                       9
      REAL FPOS_Z                                                         9
      EQUIVALENCE (BANK_FPOS(7),FPOS_Z,RE_FPOS_Z),(NAMES_FPOS(5),         9
     *NFPOS_Z)                                                            9
      EQUIVALENCE (COMMEN_FPOS(5),CFPOS_Z),(MAP_FPOS(1,5),MFPOS_Z)        9
      CHARACTER CFPOS_Z*40/' '/                                           9
      CHARACTER NFPOS_Z*12/'R.Z'/                                         9
      INTEGER MFPOS_Z(2)/1,1/                                             9
      REAL RE_FPOS_Z                                                      9
C -                                                                       9
      REAL FPOS_AY                                                        9
      EQUIVALENCE (BANK_FPOS(8),FPOS_AY,RE_FPOS_AY),(NAMES_FPOS(6),       9
     *NFPOS_AY)                                                           9
      EQUIVALENCE (COMMEN_FPOS(6),CFPOS_AY),(MAP_FPOS(1,6),MFPOS_AY)      9
      CHARACTER CFPOS_AY*40/' '/                                          9
      CHARACTER NFPOS_AY*12/'R.AY'/                                       9
      INTEGER MFPOS_AY(2)/1,1/                                            9
      REAL RE_FPOS_AY                                                     9
C -                                                                       9
      INTEGER LEN_FPOS,LLEN_FPOS,LINK_FPOS                                9
      PARAMETER (LEN_FPOS=8)                                              9
      PARAMETER (LLEN_FPOS=6)                                             9
      INTEGER FLAG_FPOS/0/,MAP_FPOS(2,LLEN_FPOS),IDNUM_FPOS(10)           9
      CHARACTER NAMES_FPOS(LLEN_FPOS)*12,COMMEN_FPOS(LLEN_FPOS)*40        9
      DATA IDNUM_FPOS/10*0/                                               9
      REAL BANK_FPOS(LEN_FPOS)                                            9
      CHARACTER*60 BPATH_FPOS/' '/                                        9
      SAVE FLAG_FPOS,MAP_FPOS,IDNUM_FPOS,NAMES_FPOS,COMMEN_FPOS,          9
     *BPATH_FPOS                                                          9
      DATA LINK_FPOS/0/,BANK_FPOS/LEN_FPOS*0/                             9
C *                                                                       9
C -                                                                       10
      REAL FBXD_TYPE                                                      10
      EQUIVALENCE (BANK_FBXD(3),FBXD_TYPE,RE_FBXD_TYPE),(NAMES_FBXD(1),   10
     *NFBXD_TYPE)                                                         10
      EQUIVALENCE (COMMEN_FBXD(1),CFBXD_TYPE),(MAP_FBXD(1,1),             10
     *MFBXD_TYPE)                                                         10
      CHARACTER CFBXD_TYPE*40/' '/                                        10
      CHARACTER NFBXD_TYPE*12/'R.TYPE'/                                   10
      INTEGER MFBXD_TYPE(2)/1,1/                                          10
      REAL RE_FBXD_TYPE                                                   10
C -                                                                       10
      REAL FBXD_HEIGHT                                                    10
      EQUIVALENCE (BANK_FBXD(4),FBXD_HEIGHT,RE_FBXD_HEIGHT),              10
     *(NAMES_FBXD(2),NFBXD_HEIGHT)                                        10
      EQUIVALENCE (COMMEN_FBXD(2),CFBXD_HEIGHT),(MAP_FBXD(1,2),           10
     *MFBXD_HEIGHT)                                                       10
      CHARACTER CFBXD_HEIGHT*40/' '/                                      10
      CHARACTER NFBXD_HEIGHT*12/'R.HEIGHT'/                               10
      INTEGER MFBXD_HEIGHT(2)/1,1/                                        10
      REAL RE_FBXD_HEIGHT                                                 10
C -                                                                       10
      REAL FBXD_DEPTH                                                     10
      EQUIVALENCE (BANK_FBXD(5),FBXD_DEPTH,RE_FBXD_DEPTH),                10
     *(NAMES_FBXD(3),NFBXD_DEPTH)                                         10
      EQUIVALENCE (COMMEN_FBXD(3),CFBXD_DEPTH),(MAP_FBXD(1,3),            10
     *MFBXD_DEPTH)                                                        10
      CHARACTER CFBXD_DEPTH*40/' '/                                       10
      CHARACTER NFBXD_DEPTH*12/'R.DEPTH'/                                 10
      INTEGER MFBXD_DEPTH(2)/1,1/                                         10
      REAL RE_FBXD_DEPTH                                                  10
C -                                                                       10
      REAL FBXD_NX                                                        10
      EQUIVALENCE (BANK_FBXD(6),FBXD_NX,RE_FBXD_NX),(NAMES_FBXD(4),       10
     *NFBXD_NX)                                                           10
      EQUIVALENCE (COMMEN_FBXD(4),CFBXD_NX),(MAP_FBXD(1,4),MFBXD_NX)      10
      CHARACTER CFBXD_NX*40/' '/                                          10
      CHARACTER NFBXD_NX*12/'R.NX'/                                       10
      INTEGER MFBXD_NX(2)/1,1/                                            10
      REAL RE_FBXD_NX                                                     10
C -                                                                       10
      REAL FBXD_NY                                                        10
      EQUIVALENCE (BANK_FBXD(7),FBXD_NY,RE_FBXD_NY),(NAMES_FBXD(5),       10
     *NFBXD_NY)                                                           10
      EQUIVALENCE (COMMEN_FBXD(5),CFBXD_NY),(MAP_FBXD(1,5),MFBXD_NY)      10
      CHARACTER CFBXD_NY*40/' '/                                          10
      CHARACTER NFBXD_NY*12/'R.NY'/                                       10
      INTEGER MFBXD_NY(2)/1,1/                                            10
      REAL RE_FBXD_NY                                                     10
C -                                                                       10
      REAL FBXD_XOFFSET                                                   10
      EQUIVALENCE (BANK_FBXD(8),FBXD_XOFFSET,RE_FBXD_XOFFSET),            10
     *(NAMES_FBXD(6),NFBXD_XOFFSET)                                       10
      EQUIVALENCE (COMMEN_FBXD(6),CFBXD_XOFFSET),(MAP_FBXD(1,6),          10
     *MFBXD_XOFFSET)                                                      10
      CHARACTER CFBXD_XOFFSET*40/' '/                                     10
      CHARACTER NFBXD_XOFFSET*12/'R.XOFFSET'/                             10
      INTEGER MFBXD_XOFFSET(2)/1,1/                                       10
      REAL RE_FBXD_XOFFSET                                                10
C -                                                                       10
      REAL FBXD_ZOFFSET                                                   10
      EQUIVALENCE (BANK_FBXD(9),FBXD_ZOFFSET,RE_FBXD_ZOFFSET),            10
     *(NAMES_FBXD(7),NFBXD_ZOFFSET)                                       10
      EQUIVALENCE (COMMEN_FBXD(7),CFBXD_ZOFFSET),(MAP_FBXD(1,7),          10
     *MFBXD_ZOFFSET)                                                      10
      CHARACTER CFBXD_ZOFFSET*40/' '/                                     10
      CHARACTER NFBXD_ZOFFSET*12/'R.ZOFFSET'/                             10
      INTEGER MFBXD_ZOFFSET(2)/1,1/                                       10
      REAL RE_FBXD_ZOFFSET                                                10
C -                                                                       10
      REAL FBXD_PSOFFSET                                                  10
      EQUIVALENCE (BANK_FBXD(10),FBXD_PSOFFSET,RE_FBXD_PSOFFSET),         10
     *(NAMES_FBXD(8),NFBXD_PSOFFSET)                                      10
      EQUIVALENCE (COMMEN_FBXD(8),CFBXD_PSOFFSET),(MAP_FBXD(1,8),         10
     *MFBXD_PSOFFSET)                                                     10
      CHARACTER CFBXD_PSOFFSET*40/' '/                                    10
      CHARACTER NFBXD_PSOFFSET*12/'R.PSOFFSET'/                           10
      INTEGER MFBXD_PSOFFSET(2)/1,1/                                      10
      REAL RE_FBXD_PSOFFSET                                               10
C -                                                                       10
      REAL FBXD_SMDOFF                                                    10
      EQUIVALENCE (BANK_FBXD(11),FBXD_SMDOFF,RE_FBXD_SMDOFF),             10
     *(NAMES_FBXD(9),NFBXD_SMDOFF)                                        10
      EQUIVALENCE (COMMEN_FBXD(9),CFBXD_SMDOFF),(MAP_FBXD(1,9),           10
     *MFBXD_SMDOFF)                                                       10
      CHARACTER CFBXD_SMDOFF*40/' '/                                      10
      CHARACTER NFBXD_SMDOFF*12/'R.SMDOFF'/                               10
      INTEGER MFBXD_SMDOFF(2)/1,1/                                        10
      REAL RE_FBXD_SMDOFF                                                 10
C -                                                                       10
      INTEGER LEN_FBXD,LLEN_FBXD,LINK_FBXD                                10
      PARAMETER (LEN_FBXD=11)                                             10
      PARAMETER (LLEN_FBXD=9)                                             10
      INTEGER FLAG_FBXD/0/,MAP_FBXD(2,LLEN_FBXD),IDNUM_FBXD(10)           10
      CHARACTER NAMES_FBXD(LLEN_FBXD)*12,COMMEN_FBXD(LLEN_FBXD)*40        10
      DATA IDNUM_FBXD/10*0/                                               10
      REAL BANK_FBXD(LEN_FBXD)                                            10
      CHARACTER*60 BPATH_FBXD/' '/                                        10
      SAVE FLAG_FBXD,MAP_FBXD,IDNUM_FBXD,NAMES_FBXD,COMMEN_FBXD,          10
     *BPATH_FBXD                                                          10
      DATA LINK_FBXD/0/,BANK_FBXD/LEN_FBXD*0/                             10
C *                                                                       10
C -                                                                       11
      REAL FLGG_TYPE                                                      11
      EQUIVALENCE (BANK_FLGG(3),FLGG_TYPE,RE_FLGG_TYPE),(NAMES_FLGG(1),   11
     *NFLGG_TYPE)                                                         11
      EQUIVALENCE (COMMEN_FLGG(1),CFLGG_TYPE),(MAP_FLGG(1,1),             11
     *MFLGG_TYPE)                                                         11
      CHARACTER CFLGG_TYPE*40/' '/                                        11
      CHARACTER NFLGG_TYPE*12/'R.TYPE'/                                   11
      INTEGER MFLGG_TYPE(2)/1,1/                                          11
      REAL RE_FLGG_TYPE                                                   11
C -                                                                       11
      REAL FLGG_WIDTH                                                     11
      EQUIVALENCE (BANK_FLGG(4),FLGG_WIDTH,RE_FLGG_WIDTH),                11
     *(NAMES_FLGG(2),NFLGG_WIDTH)                                         11
      EQUIVALENCE (COMMEN_FLGG(2),CFLGG_WIDTH),(MAP_FLGG(1,2),            11
     *MFLGG_WIDTH)                                                        11
      CHARACTER CFLGG_WIDTH*40/' '/                                       11
      CHARACTER NFLGG_WIDTH*12/'R.WIDTH'/                                 11
      INTEGER MFLGG_WIDTH(2)/1,1/                                         11
      REAL RE_FLGG_WIDTH                                                  11
C -                                                                       11
      REAL FLGG_DEPTH                                                     11
      EQUIVALENCE (BANK_FLGG(5),FLGG_DEPTH,RE_FLGG_DEPTH),                11
     *(NAMES_FLGG(3),NFLGG_DEPTH)                                         11
      EQUIVALENCE (COMMEN_FLGG(3),CFLGG_DEPTH),(MAP_FLGG(1,3),            11
     *MFLGG_DEPTH)                                                        11
      CHARACTER CFLGG_DEPTH*40/' '/                                       11
      CHARACTER NFLGG_DEPTH*12/'R.DEPTH'/                                 11
      INTEGER MFLGG_DEPTH(2)/1,1/                                         11
      REAL RE_FLGG_DEPTH                                                  11
C -                                                                       11
      REAL FLGG_DGAP                                                      11
      EQUIVALENCE (BANK_FLGG(6),FLGG_DGAP,RE_FLGG_DGAP),(NAMES_FLGG(4),   11
     *NFLGG_DGAP)                                                         11
      EQUIVALENCE (COMMEN_FLGG(4),CFLGG_DGAP),(MAP_FLGG(1,4),             11
     *MFLGG_DGAP)                                                         11
      CHARACTER CFLGG_DGAP*40/' '/                                        11
      CHARACTER NFLGG_DGAP*12/'R.DGAP'/                                   11
      INTEGER MFLGG_DGAP(2)/1,1/                                          11
      REAL RE_FLGG_DGAP                                                   11
C -                                                                       11
      REAL FLGG_ALTHICK                                                   11
      EQUIVALENCE (BANK_FLGG(7),FLGG_ALTHICK,RE_FLGG_ALTHICK),            11
     *(NAMES_FLGG(5),NFLGG_ALTHICK)                                       11
      EQUIVALENCE (COMMEN_FLGG(5),CFLGG_ALTHICK),(MAP_FLGG(1,5),          11
     *MFLGG_ALTHICK)                                                      11
      CHARACTER CFLGG_ALTHICK*40/' '/                                     11
      CHARACTER NFLGG_ALTHICK*12/'R.ALTHICK'/                             11
      INTEGER MFLGG_ALTHICK(2)/1,1/                                       11
      REAL RE_FLGG_ALTHICK                                                11
C -                                                                       11
      REAL FLGG_PHCATHDZ                                                  11
      EQUIVALENCE (BANK_FLGG(8),FLGG_PHCATHDZ,RE_FLGG_PHCATHDZ),          11
     *(NAMES_FLGG(6),NFLGG_PHCATHDZ)                                      11
      EQUIVALENCE (COMMEN_FLGG(6),CFLGG_PHCATHDZ),(MAP_FLGG(1,6),         11
     *MFLGG_PHCATHDZ)                                                     11
      CHARACTER CFLGG_PHCATHDZ*40/' '/                                    11
      CHARACTER NFLGG_PHCATHDZ*12/'R.PHCATHDZ'/                           11
      INTEGER MFLGG_PHCATHDZ(2)/1,1/                                      11
      REAL RE_FLGG_PHCATHDZ                                               11
C -                                                                       11
      REAL FLGG_PHCATHR                                                   11
      EQUIVALENCE (BANK_FLGG(9),FLGG_PHCATHR,RE_FLGG_PHCATHR),            11
     *(NAMES_FLGG(7),NFLGG_PHCATHR)                                       11
      EQUIVALENCE (COMMEN_FLGG(7),CFLGG_PHCATHR),(MAP_FLGG(1,7),          11
     *MFLGG_PHCATHR)                                                      11
      CHARACTER CFLGG_PHCATHR*40/' '/                                     11
      CHARACTER NFLGG_PHCATHR*12/'R.PHCATHR'/                             11
      INTEGER MFLGG_PHCATHR(2)/1,1/                                       11
      REAL RE_FLGG_PHCATHR                                                11
C -                                                                       11
      REAL FLGG_MUMETDZ                                                   11
      EQUIVALENCE (BANK_FLGG(10),FLGG_MUMETDZ,RE_FLGG_MUMETDZ),           11
     *(NAMES_FLGG(8),NFLGG_MUMETDZ)                                       11
      EQUIVALENCE (COMMEN_FLGG(8),CFLGG_MUMETDZ),(MAP_FLGG(1,8),          11
     *MFLGG_MUMETDZ)                                                      11
      CHARACTER CFLGG_MUMETDZ*40/' '/                                     11
      CHARACTER NFLGG_MUMETDZ*12/'R.MUMETDZ'/                             11
      INTEGER MFLGG_MUMETDZ(2)/1,1/                                       11
      REAL RE_FLGG_MUMETDZ                                                11
C -                                                                       11
      REAL FLGG_MUMETR                                                    11
      EQUIVALENCE (BANK_FLGG(11),FLGG_MUMETR,RE_FLGG_MUMETR),             11
     *(NAMES_FLGG(9),NFLGG_MUMETR)                                        11
      EQUIVALENCE (COMMEN_FLGG(9),CFLGG_MUMETR),(MAP_FLGG(1,9),           11
     *MFLGG_MUMETR)                                                       11
      CHARACTER CFLGG_MUMETR*40/' '/                                      11
      CHARACTER NFLGG_MUMETR*12/'R.MUMETR'/                               11
      INTEGER MFLGG_MUMETR(2)/1,1/                                        11
      REAL RE_FLGG_MUMETR                                                 11
C -                                                                       11
      INTEGER LEN_FLGG,LLEN_FLGG,LINK_FLGG                                11
      PARAMETER (LEN_FLGG=11)                                             11
      PARAMETER (LLEN_FLGG=9)                                             11
      INTEGER FLAG_FLGG/0/,MAP_FLGG(2,LLEN_FLGG),IDNUM_FLGG(10)           11
      CHARACTER NAMES_FLGG(LLEN_FLGG)*12,COMMEN_FLGG(LLEN_FLGG)*40        11
      DATA IDNUM_FLGG/10*0/                                               11
      REAL BANK_FLGG(LEN_FLGG)                                            11
      CHARACTER*60 BPATH_FLGG/' '/                                        11
      SAVE FLAG_FLGG,MAP_FLGG,IDNUM_FLGG,NAMES_FLGG,COMMEN_FLGG,          11
     *BPATH_FLGG                                                          11
      DATA LINK_FLGG/0/,BANK_FLGG/LEN_FLGG*0/                             11
C *                                                                       11
C -                                                                       12
      INTEGER FLGM_TYPE                                                   12
      EQUIVALENCE (BANK_FLGM(3),FLGM_TYPE,RE_FLGM_TYPE),(NAMES_FLGM(1),   12
     *NFLGM_TYPE)                                                         12
      EQUIVALENCE (COMMEN_FLGM(1),CFLGM_TYPE),(MAP_FLGM(1,1),             12
     *MFLGM_TYPE)                                                         12
      CHARACTER CFLGM_TYPE*40/' '/                                        12
      CHARACTER NFLGM_TYPE*12/'I.TYPE'/                                   12
      INTEGER MFLGM_TYPE(2)/1,1/                                          12
      REAL RE_FLGM_TYPE                                                   12
C -                                                                       12
      REAL FLGM_DENSITY                                                   12
      EQUIVALENCE (BANK_FLGM(4),FLGM_DENSITY,RE_FLGM_DENSITY),            12
     *(NAMES_FLGM(2),NFLGM_DENSITY)                                       12
      EQUIVALENCE (COMMEN_FLGM(2),CFLGM_DENSITY),(MAP_FLGM(1,2),          12
     *MFLGM_DENSITY)                                                      12
      CHARACTER CFLGM_DENSITY*40/' '/                                     12
      CHARACTER NFLGM_DENSITY*12/'R.DENSITY'/                             12
      INTEGER MFLGM_DENSITY(2)/1,1/                                       12
      REAL RE_FLGM_DENSITY                                                12
C -                                                                       12
      REAL FLGM_RADLEN                                                    12
      EQUIVALENCE (BANK_FLGM(5),FLGM_RADLEN,RE_FLGM_RADLEN),              12
     *(NAMES_FLGM(3),NFLGM_RADLEN)                                        12
      EQUIVALENCE (COMMEN_FLGM(3),CFLGM_RADLEN),(MAP_FLGM(1,3),           12
     *MFLGM_RADLEN)                                                       12
      CHARACTER CFLGM_RADLEN*40/' '/                                      12
      CHARACTER NFLGM_RADLEN*12/'R.RADLEN'/                               12
      INTEGER MFLGM_RADLEN(2)/1,1/                                        12
      REAL RE_FLGM_RADLEN                                                 12
C -                                                                       12
      REAL FLGM_PBCONT                                                    12
      EQUIVALENCE (BANK_FLGM(6),FLGM_PBCONT,RE_FLGM_PBCONT),              12
     *(NAMES_FLGM(4),NFLGM_PBCONT)                                        12
      EQUIVALENCE (COMMEN_FLGM(4),CFLGM_PBCONT),(MAP_FLGM(1,4),           12
     *MFLGM_PBCONT)                                                       12
      CHARACTER CFLGM_PBCONT*40/' '/                                      12
      CHARACTER NFLGM_PBCONT*12/'R.PBCONT'/                               12
      INTEGER MFLGM_PBCONT(2)/1,1/                                        12
      REAL RE_FLGM_PBCONT                                                 12
C -                                                                       12
      REAL FLGM_CRITENE                                                   12
      EQUIVALENCE (BANK_FLGM(7),FLGM_CRITENE,RE_FLGM_CRITENE),            12
     *(NAMES_FLGM(5),NFLGM_CRITENE)                                       12
      EQUIVALENCE (COMMEN_FLGM(5),CFLGM_CRITENE),(MAP_FLGM(1,5),          12
     *MFLGM_CRITENE)                                                      12
      CHARACTER CFLGM_CRITENE*40/' '/                                     12
      CHARACTER NFLGM_CRITENE*12/'R.CRITENE'/                             12
      INTEGER MFLGM_CRITENE(2)/1,1/                                       12
      REAL RE_FLGM_CRITENE                                                12
C -                                                                       12
      REAL FLGM_MOLIERER                                                  12
      EQUIVALENCE (BANK_FLGM(8),FLGM_MOLIERER,RE_FLGM_MOLIERER),          12
     *(NAMES_FLGM(6),NFLGM_MOLIERER)                                      12
      EQUIVALENCE (COMMEN_FLGM(6),CFLGM_MOLIERER),(MAP_FLGM(1,6),         12
     *MFLGM_MOLIERER)                                                     12
      CHARACTER CFLGM_MOLIERER*40/' '/                                    12
      CHARACTER NFLGM_MOLIERER*12/'R.MOLIERER'/                           12
      INTEGER MFLGM_MOLIERER(2)/1,1/                                      12
      REAL RE_FLGM_MOLIERER                                               12
C -                                                                       12
      INTEGER LEN_FLGM,LLEN_FLGM,LINK_FLGM                                12
      PARAMETER (LEN_FLGM=8)                                              12
      PARAMETER (LLEN_FLGM=6)                                             12
      INTEGER FLAG_FLGM/0/,MAP_FLGM(2,LLEN_FLGM),IDNUM_FLGM(10)           12
      CHARACTER NAMES_FLGM(LLEN_FLGM)*12,COMMEN_FLGM(LLEN_FLGM)*40        12
      DATA IDNUM_FLGM/10*0/                                               12
      REAL BANK_FLGM(LEN_FLGM)                                            12
      CHARACTER*60 BPATH_FLGM/' '/                                        12
      SAVE FLAG_FLGM,MAP_FLGM,IDNUM_FLGM,NAMES_FLGM,COMMEN_FLGM,          12
     *BPATH_FLGM                                                          12
      DATA LINK_FLGM/0/,BANK_FLGM/LEN_FLGM*0/                             12
C *                                                                       12
C -                                                                       13
      REAL PBPD_Z                                                         13
      EQUIVALENCE (BANK_PBPD(3),PBPD_Z,RE_PBPD_Z),(NAMES_PBPD(1),         13
     *NPBPD_Z)                                                            13
      EQUIVALENCE (COMMEN_PBPD(1),CPBPD_Z),(MAP_PBPD(1,1),MPBPD_Z)        13
      CHARACTER CPBPD_Z*40/' '/                                           13
      CHARACTER NPBPD_Z*12/'R.Z'/                                         13
      INTEGER MPBPD_Z(2)/1,1/                                             13
      REAL RE_PBPD_Z                                                      13
C -                                                                       13
      REAL PBPD_WIDTH                                                     13
      EQUIVALENCE (BANK_PBPD(4),PBPD_WIDTH,RE_PBPD_WIDTH),                13
     *(NAMES_PBPD(2),NPBPD_WIDTH)                                         13
      EQUIVALENCE (COMMEN_PBPD(2),CPBPD_WIDTH),(MAP_PBPD(1,2),            13
     *MPBPD_WIDTH)                                                        13
      CHARACTER CPBPD_WIDTH*40/' '/                                       13
      CHARACTER NPBPD_WIDTH*12/'R.WIDTH'/                                 13
      INTEGER MPBPD_WIDTH(2)/1,1/                                         13
      REAL RE_PBPD_WIDTH                                                  13
C -                                                                       13
      REAL PBPD_HEIGHT                                                    13
      EQUIVALENCE (BANK_PBPD(5),PBPD_HEIGHT,RE_PBPD_HEIGHT),              13
     *(NAMES_PBPD(3),NPBPD_HEIGHT)                                        13
      EQUIVALENCE (COMMEN_PBPD(3),CPBPD_HEIGHT),(MAP_PBPD(1,3),           13
     *MPBPD_HEIGHT)                                                       13
      CHARACTER CPBPD_HEIGHT*40/' '/                                      13
      CHARACTER NPBPD_HEIGHT*12/'R.HEIGHT'/                               13
      INTEGER MPBPD_HEIGHT(2)/1,1/                                        13
      REAL RE_PBPD_HEIGHT                                                 13
C -                                                                       13
      REAL PBPD_THICK                                                     13
      EQUIVALENCE (BANK_PBPD(6),PBPD_THICK,RE_PBPD_THICK),                13
     *(NAMES_PBPD(4),NPBPD_THICK)                                         13
      EQUIVALENCE (COMMEN_PBPD(4),CPBPD_THICK),(MAP_PBPD(1,4),            13
     *MPBPD_THICK)                                                        13
      CHARACTER CPBPD_THICK*40/' '/                                       13
      CHARACTER NPBPD_THICK*12/'R.THICK'/                                 13
      INTEGER MPBPD_THICK(2)/1,1/                                         13
      REAL RE_PBPD_THICK                                                  13
C -                                                                       13
      INTEGER LEN_PBPD,LLEN_PBPD,LINK_PBPD                                13
      PARAMETER (LEN_PBPD=6)                                              13
      PARAMETER (LLEN_PBPD=4)                                             13
      INTEGER FLAG_PBPD/0/,MAP_PBPD(2,LLEN_PBPD),IDNUM_PBPD(10)           13
      CHARACTER NAMES_PBPD(LLEN_PBPD)*12,COMMEN_PBPD(LLEN_PBPD)*40        13
      DATA IDNUM_PBPD/10*0/                                               13
      REAL BANK_PBPD(LEN_PBPD)                                            13
      CHARACTER*60 BPATH_PBPD/' '/                                        13
      SAVE FLAG_PBPD,MAP_PBPD,IDNUM_PBPD,NAMES_PBPD,COMMEN_PBPD,          13
     *BPATH_PBPD                                                          13
      DATA LINK_PBPD/0/,BANK_PBPD/LEN_PBPD*0/                             13
C *                                                                       13
C -                                                                       14
      REAL FMXG_VERSION                                                   14
      EQUIVALENCE (BANK_FMXG(3),FMXG_VERSION,RE_FMXG_VERSION),            14
     *(NAMES_FMXG(1),NFMXG_VERSION)                                       14
      EQUIVALENCE (COMMEN_FMXG(1),CFMXG_VERSION),(MAP_FMXG(1,1),          14
     *MFMXG_VERSION)                                                      14
      CHARACTER CFMXG_VERSION*40/' '/                                     14
      CHARACTER NFMXG_VERSION*12/'R.VERSION'/                             14
      INTEGER MFMXG_VERSION(2)/1,1/                                       14
      REAL RE_FMXG_VERSION                                                14
C -                                                                       14
      REAL FMXG_SAPEX                                                     14
      EQUIVALENCE (BANK_FMXG(4),FMXG_SAPEX,RE_FMXG_SAPEX),                14
     *(NAMES_FMXG(2),NFMXG_SAPEX)                                         14
      EQUIVALENCE (COMMEN_FMXG(2),CFMXG_SAPEX),(MAP_FMXG(1,2),            14
     *MFMXG_SAPEX)                                                        14
      CHARACTER CFMXG_SAPEX*40/' '/                                       14
      CHARACTER NFMXG_SAPEX*12/'R.SAPEX'/                                 14
      INTEGER MFMXG_SAPEX(2)/1,1/                                         14
      REAL RE_FMXG_SAPEX                                                  14
C -                                                                       14
      REAL FMXG_SBASE                                                     14
      EQUIVALENCE (BANK_FMXG(5),FMXG_SBASE,RE_FMXG_SBASE),                14
     *(NAMES_FMXG(3),NFMXG_SBASE)                                         14
      EQUIVALENCE (COMMEN_FMXG(3),CFMXG_SBASE),(MAP_FMXG(1,3),            14
     *MFMXG_SBASE)                                                        14
      CHARACTER CFMXG_SBASE*40/' '/                                       14
      CHARACTER NFMXG_SBASE*12/'R.SBASE'/                                 14
      INTEGER MFMXG_SBASE(2)/1,1/                                         14
      REAL RE_FMXG_SBASE                                                  14
C -                                                                       14
      REAL FMXG_SGAP                                                      14
      EQUIVALENCE (BANK_FMXG(6),FMXG_SGAP,RE_FMXG_SGAP),(NAMES_FMXG(4),   14
     *NFMXG_SGAP)                                                         14
      EQUIVALENCE (COMMEN_FMXG(4),CFMXG_SGAP),(MAP_FMXG(1,4),             14
     *MFMXG_SGAP)                                                         14
      CHARACTER CFMXG_SGAP*40/' '/                                        14
      CHARACTER NFMXG_SGAP*12/'R.SGAP'/                                   14
      INTEGER MFMXG_SGAP(2)/1,1/                                          14
      REAL RE_FMXG_SGAP                                                   14
C -                                                                       14
      REAL FMXG_NSTRIP                                                    14
      EQUIVALENCE (BANK_FMXG(7),FMXG_NSTRIP,RE_FMXG_NSTRIP),              14
     *(NAMES_FMXG(5),NFMXG_NSTRIP)                                        14
      EQUIVALENCE (COMMEN_FMXG(5),CFMXG_NSTRIP),(MAP_FMXG(1,5),           14
     *MFMXG_NSTRIP)                                                       14
      CHARACTER CFMXG_NSTRIP*40/' '/                                      14
      CHARACTER NFMXG_NSTRIP*12/'R.NSTRIP'/                               14
      INTEGER MFMXG_NSTRIP(2)/1,1/                                        14
      REAL RE_FMXG_NSTRIP                                                 14
C -                                                                       14
      REAL FMXG_G10WIDTH                                                  14
      EQUIVALENCE (BANK_FMXG(8),FMXG_G10WIDTH,RE_FMXG_G10WIDTH),          14
     *(NAMES_FMXG(6),NFMXG_G10WIDTH)                                      14
      EQUIVALENCE (COMMEN_FMXG(6),CFMXG_G10WIDTH),(MAP_FMXG(1,6),         14
     *MFMXG_G10WIDTH)                                                     14
      CHARACTER CFMXG_G10WIDTH*40/' '/                                    14
      CHARACTER NFMXG_G10WIDTH*12/'R.G10WIDTH'/                           14
      INTEGER MFMXG_G10WIDTH(2)/1,1/                                      14
      REAL RE_FMXG_G10WIDTH                                               14
C -                                                                       14
      REAL FMXG_G10HGT                                                    14
      EQUIVALENCE (BANK_FMXG(9),FMXG_G10HGT,RE_FMXG_G10HGT),              14
     *(NAMES_FMXG(7),NFMXG_G10HGT)                                        14
      EQUIVALENCE (COMMEN_FMXG(7),CFMXG_G10HGT),(MAP_FMXG(1,7),           14
     *MFMXG_G10HGT)                                                       14
      CHARACTER CFMXG_G10HGT*40/' '/                                      14
      CHARACTER NFMXG_G10HGT*12/'R.G10HGT'/                               14
      INTEGER MFMXG_G10HGT(2)/1,1/                                        14
      REAL RE_FMXG_G10HGT                                                 14
C -                                                                       14
      REAL FMXG_G10THICK                                                  14
      EQUIVALENCE (BANK_FMXG(10),FMXG_G10THICK,RE_FMXG_G10THICK),         14
     *(NAMES_FMXG(8),NFMXG_G10THICK)                                      14
      EQUIVALENCE (COMMEN_FMXG(8),CFMXG_G10THICK),(MAP_FMXG(1,8),         14
     *MFMXG_G10THICK)                                                     14
      CHARACTER CFMXG_G10THICK*40/' '/                                    14
      CHARACTER NFMXG_G10THICK*12/'R.G10THICK'/                           14
      INTEGER MFMXG_G10THICK(2)/1,1/                                      14
      REAL RE_FMXG_G10THICK                                               14
C -                                                                       14
      INTEGER LEN_FMXG,LLEN_FMXG,LINK_FMXG                                14
      PARAMETER (LEN_FMXG=10)                                             14
      PARAMETER (LLEN_FMXG=8)                                             14
      INTEGER FLAG_FMXG/0/,MAP_FMXG(2,LLEN_FMXG),IDNUM_FMXG(10)           14
      CHARACTER NAMES_FMXG(LLEN_FMXG)*12,COMMEN_FMXG(LLEN_FMXG)*40        14
      DATA IDNUM_FMXG/10*0/                                               14
      REAL BANK_FMXG(LEN_FMXG)                                            14
      CHARACTER*60 BPATH_FMXG/' '/                                        14
      SAVE FLAG_FMXG,MAP_FMXG,IDNUM_FMXG,NAMES_FMXG,COMMEN_FMXG,          14
     *BPATH_FMXG                                                          14
      DATA LINK_FMXG/0/,BANK_FMXG/LEN_FMXG*0/                             14
C *                                                                       14
      INTEGER CHKVSIM,IMOD,ITYPE,TYPE,PBPLATE                             15
      INTEGER I,J,M                                                       17
      REAL XX,YY,ZZ,X1,Y1,Z1,ZTOT,RTOT,WID,BWID,X0                        19
      REAL ZTOTSMD,WTOTSMD,XXX,YYY,ZZZ,WSMD                               21
      INTEGER N                                                           23
      PARAMETER (N=12)                                                    25
      REAL E(N)                                                           26
      REAL RINDEX_PBG(N)                                                  28
      REAL RINDEX_SIRUB(N)                                                30
      REAL RINDEX_PHCATH(N)                                               32
      REAL RINDEX_ALM(N)                                                  34
      REAL RINDEX_MUMET(N)                                                36
      REAL ABSCO_PBG(N)                                                   38
      REAL ABSCO_SIRUB(N)                                                 40
      REAL ABSCO_PHCATH(N)                                                42
      REAL ABSCO_ALM(N)                                                   44
      REAL ABSCO_MUMET(N)                                                 46
      REAL EFFIC_PHCATH(N)                                                48
      REAL EFFIC_ALL(N)                                                   50
      EXTERNAL FFPDSTEP                                                   52
      EXTERNAL FPCTSTEP                                                   53
C *                                                                       54
      CALL AGSBEGM(MODULE,IPRIN)                                          54
      IF (IPRIN.LT.0) RETURN                                              54
C Filling bank FMCG - FPD CALORIMETER BASIC DATA                          54
      BANK_TITLE='FPD CALORIMETER BASIC DATA'                             54
      BPATH_FMCG='FMCG*'                                                  54
      IDNUM_FMCG(1)=0                                                     55
C GEOMETRY VERSION                                                        55
      FMCG_VERSION=7.0                                                    55
      CFMCG_VERSION='GEOMETRY VERSION'                                    55
C = 0 DE, = 1 CHERENKOV SIMULATION FOR PBG                                56
      FMCG_CHKVSIM=0                                                      56
      CFMCG_CHKVSIM='= 0 DE, = 1 CHERENKOV SIMULATION FOR PBG'            56
C =0 NO PLATE, =1 WITH PLATE                                              57
      FMCG_PBPLATE=0                                                      57
      CFMCG_PBPLATE='=0 NO PLATE, =1 WITH PLATE'                          57
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    58
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           58
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   58
C *                                                                       58
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    58
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           58
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   58
C *                                                                       59
      CALL AGSBEGM(MODULE,IPRIN)                                          59
      IF (IPRIN.LT.0) RETURN                                              59
C Filling bank FPOS - FPD EN POSITIONING                                  59
      BANK_TITLE='FPD EN POSITIONING'                                     59
      BPATH_FPOS='FPOS*'                                                  59
      IDNUM_FPOS(1)=0                                                     60
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               60
      FPOS_IMOD=1                                                         60
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              60
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          61
      FPOS_ITYPE=1                                                        61
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        61
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                62
      FPOS_X=-48.19                                                       62
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  62
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                63
      FPOS_Y=0.0                                                          63
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  63
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               64
      FPOS_Z=-779.0                                                       64
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 64
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              65
      FPOS_AY=180                                                         65
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               65
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    66
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           66
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   66
C *                                                                       66
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    66
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           66
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   66
C *                                                                       67
      CALL AGSBEGM(MODULE,IPRIN)                                          67
      IF (IPRIN.LT.0) RETURN                                              67
C Filling bank FPOS - FPD ES POSITIONING                                  67
      BANK_TITLE='FPD ES POSITIONING'                                     67
      BPATH_FPOS='FPOS*'                                                  67
      IDNUM_FPOS(1)=0                                                     68
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               68
      FPOS_IMOD=2                                                         68
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              68
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          69
      FPOS_ITYPE=1                                                        69
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        69
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                70
      FPOS_X=48.19                                                        70
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  70
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                71
      FPOS_Y=0.0                                                          71
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  71
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               72
      FPOS_Z=-779.0                                                       72
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 72
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              73
      FPOS_AY=180                                                         73
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               73
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    74
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           74
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   74
C *                                                                       74
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    74
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           74
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   74
C *                                                                       75
      CALL AGSBEGM(MODULE,IPRIN)                                          75
      IF (IPRIN.LT.0) RETURN                                              75
C Filling bank FPOS - FPD ET POSITIONING                                  75
      BANK_TITLE='FPD ET POSITIONING'                                     75
      BPATH_FPOS='FPOS*'                                                  75
      IDNUM_FPOS(1)=0                                                     76
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               76
      FPOS_IMOD=3                                                         76
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              76
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          77
      FPOS_ITYPE=2                                                        77
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        77
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                78
      FPOS_X=0.0                                                          78
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  78
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                79
      FPOS_Y=30.8                                                         79
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  79
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               80
      FPOS_Z=-590.2                                                       80
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 80
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              81
      FPOS_AY=180                                                         81
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               81
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    82
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           82
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   82
C *                                                                       82
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    82
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           82
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   82
C *                                                                       83
      CALL AGSBEGM(MODULE,IPRIN)                                          83
      IF (IPRIN.LT.0) RETURN                                              83
C Filling bank FPOS - FPD EB POSITIONING                                  83
      BANK_TITLE='FPD EB POSITIONING'                                     83
      BPATH_FPOS='FPOS*'                                                  83
      IDNUM_FPOS(1)=0                                                     84
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               84
      FPOS_IMOD=4                                                         84
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              84
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          85
      FPOS_ITYPE=2                                                        85
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        85
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                86
      FPOS_X=0.0                                                          86
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  86
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                87
      FPOS_Y=-30.2                                                        87
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  87
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               88
      FPOS_Z=-590.2                                                       88
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 88
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              89
      FPOS_AY=180                                                         89
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               89
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    90
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           90
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   90
C *                                                                       90
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    90
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           90
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   90
C *                                                                       91
      CALL AGSBEGM(MODULE,IPRIN)                                          91
      IF (IPRIN.LT.0) RETURN                                              91
C Filling bank FPOS - FPD WN POSITIONING                                  91
      BANK_TITLE='FPD WN POSITIONING'                                     91
      BPATH_FPOS='FPOS*'                                                  91
      IDNUM_FPOS(1)=0                                                     92
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               92
      FPOS_IMOD=5                                                         92
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              92
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          93
      FPOS_ITYPE=3                                                        93
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        93
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                94
      FPOS_X=-20.00                                                       94
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  94
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                95
      FPOS_Y=0.0                                                          95
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  95
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               96
      FPOS_Z=700.0                                                        96
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 96
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              97
      FPOS_AY=0                                                           97
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               97
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    98
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           98
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   98
C *                                                                       98
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    98
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           98
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   98
C *                                                                       99
      CALL AGSBEGM(MODULE,IPRIN)                                          99
      IF (IPRIN.LT.0) RETURN                                              99
C Filling bank FPOS - FPD WS POSITIONING                                  99
      BANK_TITLE='FPD WS POSITIONING'                                     99
      BPATH_FPOS='FPOS*'                                                  99
      IDNUM_FPOS(1)=0                                                     100
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               100
      FPOS_IMOD=6                                                         100
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              100
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          101
      FPOS_ITYPE=3                                                        101
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        101
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                102
      FPOS_X=20.00                                                        102
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  102
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                103
      FPOS_Y=0.0                                                          103
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  103
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               104
      FPOS_Z=700.0                                                        104
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 104
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              105
      FPOS_AY=0                                                           105
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               105
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    106
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           106
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   106
C *                                                                       106
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    106
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           106
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   106
C *                                                                       107
      CALL AGSBEGM(MODULE,IPRIN)                                          107
      IF (IPRIN.LT.0) RETURN                                              107
C Filling bank FPOS - FPD WT POSITIONING                                  107
      BANK_TITLE='FPD WT POSITIONING'                                     107
      BPATH_FPOS='FPOS*'                                                  107
      IDNUM_FPOS(1)=0                                                     108
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               108
      FPOS_IMOD=7                                                         108
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              108
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          109
      FPOS_ITYPE=2                                                        109
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        109
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                110
      FPOS_X=0.0                                                          110
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  110
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                111
      FPOS_Y=30.8                                                         111
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  111
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               112
      FPOS_Z=590.2                                                        112
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 112
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              113
      FPOS_AY=0                                                           113
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               113
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    114
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           114
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   114
C *                                                                       114
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    114
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           114
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   114
C *                                                                       115
      CALL AGSBEGM(MODULE,IPRIN)                                          115
      IF (IPRIN.LT.0) RETURN                                              115
C Filling bank FPOS - FPD WB POSITIONING                                  115
      BANK_TITLE='FPD WB POSITIONING'                                     115
      BPATH_FPOS='FPOS*'                                                  115
      IDNUM_FPOS(1)=0                                                     116
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               116
      FPOS_IMOD=8                                                         116
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              116
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          117
      FPOS_ITYPE=2                                                        117
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'        117
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                118
      FPOS_X=0.0                                                          118
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  118
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                119
      FPOS_Y=-30.2                                                        119
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  119
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               120
      FPOS_Z=590.2                                                        120
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 120
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              121
      FPOS_AY=0                                                           121
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               121
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    122
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           122
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   122
C *                                                                       122
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    122
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           122
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   122
C *                                                                       123
      CALL AGSBEGM(MODULE,IPRIN)                                          123
      IF (IPRIN.LT.0) RETURN                                              123
C Filling bank FBXD - FPD BOX GEOMETRY                                    123
      BANK_TITLE='FPD BOX GEOMETRY'                                       123
      BPATH_FBXD='FBXD*'                                                  123
      IDNUM_FBXD(1)=0                                                     124
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          124
      FBXD_TYPE=1                                                         124
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'         124
C BOX HEIGHT                                                              125
      FBXD_HEIGHT=100                                                     125
      CFBXD_HEIGHT='BOX HEIGHT'                                           125
C BOX DEPTH                                                               126
      FBXD_DEPTH=96                                                       126
      CFBXD_DEPTH='BOX DEPTH'                                             126
C NUMBER OF PBG IN X                                                      127
      FBXD_NX=7                                                           127
      CFBXD_NX='NUMBER OF PBG IN X'                                       127
C NUMBER OF PBG IN Y                                                      128
      FBXD_NY=7                                                           128
      CFBXD_NY='NUMBER OF PBG IN Y'                                       128
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                129
      FBXD_XOFFSET=2.54                                                   129
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            129
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                130
      FBXD_ZOFFSET=19                                                     130
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            130
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            131
      FBXD_PSOFFSET=2.0                                                   131
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       131
C SMD V-PLANE Z OFFSET FROM BOX EDGE                                      132
      FBXD_SMDOFF=8.0                                                     132
      CFBXD_SMDOFF='SMD V-PLANE Z OFFSET FROM BOX EDGE'                   132
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    133
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           133
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   133
C *                                                                       133
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    133
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           133
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   133
C *                                                                       134
      CALL AGSBEGM(MODULE,IPRIN)                                          134
      IF (IPRIN.LT.0) RETURN                                              134
C Filling bank FBXD - FPD BOX GEOMETRY                                    134
      BANK_TITLE='FPD BOX GEOMETRY'                                       134
      BPATH_FBXD='FBXD*'                                                  134
      IDNUM_FBXD(1)=0                                                     135
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          135
      FBXD_TYPE=2                                                         135
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'         135
C BOX HEIGHT                                                              136
      FBXD_HEIGHT=20                                                      136
      CFBXD_HEIGHT='BOX HEIGHT'                                           136
C BOX DEPTH                                                               137
      FBXD_DEPTH=65                                                       137
      CFBXD_DEPTH='BOX DEPTH'                                             137
C NUMBER OF PBG IN X                                                      138
      FBXD_NX=5                                                           138
      CFBXD_NX='NUMBER OF PBG IN X'                                       138
C NUMBER OF PBG IN Y                                                      139
      FBXD_NY=5                                                           139
      CFBXD_NY='NUMBER OF PBG IN Y'                                       139
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                140
      FBXD_XOFFSET=0.0                                                    140
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            140
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                141
      FBXD_ZOFFSET=1                                                      141
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            141
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            142
      FBXD_PSOFFSET=0                                                     142
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       142
C SMD Z OFFSET FROM BOX EDGE                                              143
      FBXD_SMDOFF=0.0                                                     143
      CFBXD_SMDOFF='SMD Z OFFSET FROM BOX EDGE'                           143
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    144
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           144
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   144
C *                                                                       144
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    144
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           144
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   144
C *                                                                       145
      CALL AGSBEGM(MODULE,IPRIN)                                          145
      IF (IPRIN.LT.0) RETURN                                              145
C Filling bank FBXD - FPD BOX GEOMETRY                                    145
      BANK_TITLE='FPD BOX GEOMETRY'                                       145
      BPATH_FBXD='FBXD*'                                                  145
      IDNUM_FBXD(1)=0                                                     146
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)                          146
      FBXD_TYPE=3                                                         146
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5, 3=14*14+6*6)'         146
C BOX HEIGHT                                                              147
      FBXD_HEIGHT=82                                                      147
      CFBXD_HEIGHT='BOX HEIGHT'                                           147
C BOX DEPTH                                                               148
      FBXD_DEPTH=82                                                       148
      CFBXD_DEPTH='BOX DEPTH'                                             148
C NUMBER OF PBG IN X                                                      149
      FBXD_NX=6                                                           149
      CFBXD_NX='NUMBER OF PBG IN X'                                       149
C NUMBER OF PBG IN Y                                                      150
      FBXD_NY=6                                                           150
      CFBXD_NY='NUMBER OF PBG IN Y'                                       150
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                151
      FBXD_XOFFSET=29.0                                                   151
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            151
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                152
      FBXD_ZOFFSET=1                                                      152
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            152
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            153
      FBXD_PSOFFSET=0                                                     153
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       153
C SMD Z OFFSET FROM BOX EDGE                                              154
      FBXD_SMDOFF=0.0                                                     154
      CFBXD_SMDOFF='SMD Z OFFSET FROM BOX EDGE'                           154
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    155
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           155
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   155
C *                                                                       155
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    155
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           155
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   155
C *                                                                       156
      CALL AGSBEGM(MODULE,IPRIN)                                          156
      IF (IPRIN.LT.0) RETURN                                              156
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               156
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  156
      BPATH_FLGG='FLGG*'                                                  156
      IDNUM_FLGG(1)=0                                                     157
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    157
      FLGG_TYPE=1                                                         157
      CFLGG_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   157
C PBG WIDTH                                                               158
      FLGG_WIDTH=3.81                                                     158
      CFLGG_WIDTH='PBG WIDTH'                                             158
C PBG DEPTH                                                               159
      FLGG_DEPTH=45.0                                                     159
      CFLGG_DEPTH='PBG DEPTH'                                             159
C GAP BETWEEN PBG                                                         160
      FLGG_DGAP=0.01                                                      160
      CFLGG_DGAP='GAP BETWEEN PBG'                                        160
C ALMUNIM WRAP THICKNESS (REAL)                                           161
      FLGG_ALTHICK=0.001                                                  161
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       161
C PHOTO CATHODE THICKNESS                                                 162
      FLGG_PHCATHDZ=2.0                                                   162
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            162
C PHOTO CATHODE RADIUS (REAL)                                             163
      FLGG_PHCATHR=1.8                                                    163
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         163
C MU METAL LENGTH                                                         164
      FLGG_MUMETDZ=11.0                                                   164
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     164
C MU METAL OUTER RADIUS (REAL)                                            165
      FLGG_MUMETR=1.9                                                     165
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         165
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    166
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           166
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   166
C *                                                                       166
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    166
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           166
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   166
C *                                                                       167
      CALL AGSBEGM(MODULE,IPRIN)                                          167
      IF (IPRIN.LT.0) RETURN                                              167
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               167
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  167
      BPATH_FLGG='FLGG*'                                                  167
      IDNUM_FLGG(1)=0                                                     168
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    168
      FLGG_TYPE=2                                                         168
      CFLGG_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   168
C PBG WIDTH                                                               169
      FLGG_WIDTH=5.8                                                      169
      CFLGG_WIDTH='PBG WIDTH'                                             169
C PBG DEPTH                                                               170
      FLGG_DEPTH=60.2                                                     170
      CFLGG_DEPTH='PBG DEPTH'                                             170
C GAP BETWEEN PBG                                                         171
      FLGG_DGAP=0.01                                                      171
      CFLGG_DGAP='GAP BETWEEN PBG'                                        171
C ALMUNIM WRAP THICKNESS (REAL)                                           172
      FLGG_ALTHICK=0.001                                                  172
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       172
C PHOTO CATHODE THICKNESS                                                 173
      FLGG_PHCATHDZ=2.0                                                   173
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            173
C PHOTO CATHODE RADIUS (REAL)                                             174
      FLGG_PHCATHR=1.8                                                    174
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         174
C MU METAL LENGTH                                                         175
      FLGG_MUMETDZ=11.0                                                   175
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     175
C MU METAL OUTER RADIUS (REAL)                                            176
      FLGG_MUMETR=1.9                                                     176
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         176
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    177
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           177
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   177
C *                                                                       177
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    177
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           177
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   177
C *                                                                       178
      CALL AGSBEGM(MODULE,IPRIN)                                          178
      IF (IPRIN.LT.0) RETURN                                              178
C Filling bank FLGM - PBG DETECTOR MATERIALS                              178
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 178
      BPATH_FLGM='FLGM*'                                                  178
      IDNUM_FLGM(1)=0                                                     179
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    179
      FLGM_TYPE=1                                                         179
      CFLGM_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   179
C GDENSITY [/CM^3]                                                        180
      FLGM_DENSITY=3.86                                                   180
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    180
C RADIATION LENGTH [CM]                                                   181
      FLGM_RADLEN=2.5                                                     181
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                181
C PBO CONTENT [%]                                                         182
      FLGM_PBCONT=65.4                                                    182
      CFLGM_PBCONT='PBO CONTENT [%]'                                      182
C CRITICAL ENERGY [GEV]                                                   183
      FLGM_CRITENE=0.0158                                                 183
      CFLGM_CRITENE='CRITICAL ENERGY [GEV]'                               183
C MOLIERE RADIUS [CM]                                                     184
      FLGM_MOLIERER=3.32                                                  184
      CFLGM_MOLIERER='MOLIERE RADIUS [CM]'                                184
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    185
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           185
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   185
C *                                                                       185
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    185
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           185
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   185
C *                                                                       186
      CALL AGSBEGM(MODULE,IPRIN)                                          186
      IF (IPRIN.LT.0) RETURN                                              186
C Filling bank FLGM - PBG DETECTOR MATERIALS                              186
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 186
      BPATH_FLGM='FLGM*'                                                  186
      IDNUM_FLGM(1)=0                                                     187
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    187
      FLGM_TYPE=2                                                         187
      CFLGM_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   187
C GDENSITY [/CM^3]                                                        188
      FLGM_DENSITY=3.61                                                   188
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    188
C RADIATION LENGTH [CM]                                                   189
      FLGM_RADLEN=2.5                                                     189
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                189
C PBO CONTENT [%]                                                         190
      FLGM_PBCONT=65.4                                                    190
      CFLGM_PBCONT='PBO CONTENT [%]'                                      190
C CRITICAL ENERGY [GEV]                                                   191
      FLGM_CRITENE=0.0158                                                 191
      CFLGM_CRITENE='CRITICAL ENERGY [GEV]'                               191
C MOLIERE RADIUS [CM]                                                     192
      FLGM_MOLIERER=3.32                                                  192
      CFLGM_MOLIERER='MOLIERE RADIUS [CM]'                                192
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    193
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           193
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   193
C *                                                                       193
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    193
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           193
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   193
C *                                                                       194
      CALL AGSBEGM(MODULE,IPRIN)                                          194
      IF (IPRIN.LT.0) RETURN                                              194
C Filling bank PBPD - PB PLATE DIMENSIONS                                 194
      BANK_TITLE='PB PLATE DIMENSIONS'                                    194
      BPATH_PBPD='PBPD*'                                                  194
      IDNUM_PBPD(1)=0                                                     195
C WIDTH                                                                   195
      PBPD_WIDTH=33.02                                                    195
      CPBPD_WIDTH='WIDTH'                                                 195
C HEIGHT                                                                  196
      PBPD_HEIGHT=33.02                                                   196
      CPBPD_HEIGHT='HEIGHT'                                               196
C THICKNESS                                                               197
      PBPD_THICK=1.27                                                     197
      CPBPD_THICK='THICKNESS'                                             197
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    198
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           198
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   198
C *                                                                       198
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    198
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           198
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   198
C *                                                                       199
      CALL AGSBEGM(MODULE,IPRIN)                                          199
      IF (IPRIN.LT.0) RETURN                                              199
C Filling bank FMXG - SMD GEOMETRY                                        199
      BANK_TITLE='SMD GEOMETRY'                                           199
      BPATH_FMXG='FMXG*'                                                  199
      IDNUM_FMXG(1)=0                                                     200
C GEOMETRY VERSION                                                        200
      FMXG_VERSION=2                                                      200
      CFMXG_VERSION='GEOMETRY VERSION'                                    200
C SCINTILLATOR STRIP APEX                                                 201
      FMXG_SAPEX=0.7                                                      201
      CFMXG_SAPEX='SCINTILLATOR STRIP APEX'                               201
C SCINTILLATOR STRIP BASE                                                 202
      FMXG_SBASE=1.0                                                      202
      CFMXG_SBASE='SCINTILLATOR STRIP BASE'                               202
C GAP BETWEEN STRIPS                                                      203
      FMXG_SGAP=0.0064                                                    203
      CFMXG_SGAP='GAP BETWEEN STRIPS'                                     203
C # OF STRIPS                                                             204
      FMXG_NSTRIP=50                                                      204
      CFMXG_NSTRIP='# OF STRIPS'                                          204
C G10 PLATE WIDTH                                                         205
      FMXG_G10WIDTH=27.0                                                  205
      CFMXG_G10WIDTH='G10 PLATE WIDTH'                                    205
C G10 PLATE HEIGHT                                                        206
      FMXG_G10HGT=27.0                                                    206
      CFMXG_G10HGT='G10 PLATE HEIGHT'                                     206
C G10 PLATE THICKNESS                                                     207
      FMXG_G10THICK=0.15                                                  207
      CFMXG_G10THICK='G10 PLATE THICKNESS'                                207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    208
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           208
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    208
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           208
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   208
C use default FMCG bank                                                   209
      BPATH_FMCG='FMCG*'                                                  209
      IDNUM_FMCG(1)=0                                                     209
      CALL AGDGETP('FPDMGEO2','*',-999,'FMCG',' ',BANK_FMCG, BPATH_FMCG,  209
     *IDNUM_FMCG,1,MAP_FMCG,NAMES_FMCG,BANK_FMCG,LEN_FMCG,LLEN_FMCG,      209
     *LINK_FMCG,FLAG_FMCG)                                                209
C *                                                                       210
      DO 5011 M=1,8                                                       210
C    use FPOS bank                                                        211
         BPATH_FPOS='FPOS*'                                               211
         IDNUM_FPOS(1)=0                                                  211
         FPOS_IMOD=M                                                      211
         CALL AGDGETP('FPDMGEO2','*',-999,'FPOS',NFPOS_IMOD,              211
     *   RE_FPOS_IMOD,BPATH_FPOS,IDNUM_FPOS,1,MAP_FPOS,NAMES_FPOS,        211
     *   BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                211
C    use FBXD bank                                                        212
         BPATH_FBXD='FBXD*'                                               212
         IDNUM_FBXD(1)=0                                                  212
         FBXD_TYPE=FPOS_ITYPE                                             212
         CALL AGDGETP('FPDMGEO2','*',-999,'FBXD',NFBXD_TYPE,              212
     *   RE_FBXD_TYPE,BPATH_FBXD,IDNUM_FBXD,1,MAP_FBXD,NAMES_FBXD,        212
     *   BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                212
C    Check FBXD_TYPE.LT.3                                                 213
         IF (FBXD_TYPE.LT.3) THEN                                         213
C    use FLGG bank                                                        214
         BPATH_FLGG='FLGG*'                                               214
         IDNUM_FLGG(1)=0                                                  214
         FLGG_TYPE=1                                                      214
         CALL AGDGETP('FPDMGEO2','*',-999,'FLGG',NFLGG_TYPE,              214
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        214
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                214
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2                    215
         ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0            216
         RTOT = FBXD_NX*WID/2.0                                           217
         BWID = RTOT+FBXD_XOFFSET                                         218
         ELSE                                                             219
C    use FLGG bank                                                        220
         BPATH_FLGG='FLGG*'                                               220
         IDNUM_FLGG(1)=0                                                  220
         FLGG_TYPE=2                                                      220
         CALL AGDGETP('FPDMGEO2','*',-999,'FLGG',NFLGG_TYPE,              220
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        220
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                220
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2                    221
         ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0            222
         RTOT = 14*WID/2.0                                                223
         BWID = RTOT                                                      224
         ENDIF                                                            225
C    Check FPOS_X.GT.0.0                                                  226
         IF (FPOS_X.GT.0.0) THEN                                          226
         XX=FPOS_X+BWID                                                   227
         ELSEIF (FPOS_X.EQ.0.0) THEN                                      228
         XX=0.0                                                           229
         ELSE                                                             230
         XX=FPOS_X-BWID                                                   231
         ENDIF                                                            232
C    Check FPOS_Y.GT.0.0                                                  233
         IF (FPOS_Y.GT.0.0) THEN                                          233
         YY=FPOS_Y+FBXD_HEIGHT/2.0                                        234
         ELSEIF (FPOS_Y.EQ.0.0) THEN                                      235
         YY=0.0                                                           236
         ELSE                                                             237
         YY=FPOS_Y-FBXD_HEIGHT/2.0                                        238
         ENDIF                                                            239
C    Check FPOS_Z.GT.0.0                                                  240
         IF (FPOS_Z.GT.0.0) THEN                                          240
         ZZ=FPOS_Z+FBXD_DEPTH/2.0                                         241
         ELSE                                                             242
         ZZ=FPOS_Z-FBXD_DEPTH/2.0                                         243
         ENDIF                                                            244
         CALL AGSBEGM(MODULE,IPRIN)                                       245
C    Check IPRIN<0                                                        245
         IF (IPRIN.LT.0) RETURN                                           245
C    Create FBOX :                                                        245
C    Call To Local Procedure FBOX                                         245
         ASSIGN 5030 TO BLOCK_FBOX                                        245
         GO TO  5020                                                      245
5030     CONTINUE                                                         245
C    *                                                                    245
C    Geant POSITION : FBOX ===> MOTHER='CAVE' X=XX Y=YY Z=ZZ ALPHAY=FPOS  246
C    _AY <===                                                             246
         CALL AGSRESET                                                    246
         AG_MOTHER = 'CAVE'                                               246
         AG_X = XX                                                        246
         AG_Y = YY                                                        246
         AG_Z = ZZ                                                        246
         AG_ALPHAN=AG_ALPHAN+1                                            246
         AG_ALPHAK(AG_ALPHAN)=2                                           246
         AG_ALPHAL(AG_ALPHAN)=FPOS_AY                                     246
         AG_TITLE = 'FBOX'                                                246
         AG_EXNAME = 'POSITION'                                           246
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                               246
         CALL AXPOSITION                                                  246
C    *                                                                    246
5011  CONTINUE                                                            247
5012  CONTINUE                                                            247
C ----------------------------------------------------------------------  248
C ------------ FBOX                                                       248
      CALL AGSENDM                                                        249
      RETURN                                                              249
C Begin Local Procedure FBOX                                              249
C FBOX IS ONE PB-GLASS FPD DETECTOR                                       249
5020  CONTINUE                                                            249
         AG_TITLE='FBOX'                                                  249
         AG_MODULE=MODULE                                                 249
         AG_MARK='V00'                                                    249
         CALL AGSPUSH                                                     249
         AG_MODULE=MODULE                                                 249
C    *                                                                    249
C    Geant MATERIAL : AIR ===> inherited arguments used <===              250
         CALL AGSRESET                                                    250
         AG_TITLE = 'AIR'                                                 250
         AG_EXNAME = 'MATERIAL'                                           250
         AG_PARLIST = 'NONE'                                              250
         CALL AXMATERIAL                                                  250
C    *                                                                    250
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           250
         CALL AGSRESET                                                    251
         AG_TITLE = 'STANDARD'                                            251
         AG_EXNAME = 'MEDIUM'                                             251
         AG_PARLIST = 'NONE'                                              251
         CALL AXMEDIUM                                                    251
C    *                                                                    251
C    Geant ATTRIBUTE : FBOX ===> SEEN=1 COLO=2 <===                       251
         CALL AGSRESET                                                    252
         AG_SEEN = 1                                                      252
         AG_COLO = 2                                                      252
         AG_TITLE = 'FBOX'                                                252
         AG_EXNAME = 'ATTRIBUTE'                                          252
         AG_PARLIST = 'SEEN_COLO'                                         252
         CALL AXATTRIBUTE                                                 252
C    *                                                                    252
C    Geant SHAPE : BOX ===> DX=BWID DY=FBXD_HEIGHT/2 DZ=FBXD_DEPTH/2 <==  252
C    =                                                                    252
         CALL AGSRESET                                                    253
         AG_DX = BWID                                                     253
         AG_DY = FBXD_HEIGHT/2                                            253
         AG_DZ = FBXD_DEPTH/2                                             253
         AG_TITLE = 'BOX'                                                 253
         AG_EXNAME = 'SHAPE'                                              253
         AG_PARLIST = 'DX_DY_DZ'                                          253
         CALL AXSHAPE                                                     253
C    *                                                                    253
C    use FLGG bank                                                        253
         BPATH_FLGG='FLGG*'                                               254
         IDNUM_FLGG(1)=0                                                  254
         FLGG_TYPE=1                                                      254
         CALL AGDGETP('FPDMGEO2','*',-999,'FLGG',NFLGG_TYPE,              254
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        254
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                254
C    use FLGM bank                                                        255
         BPATH_FLGM='FLGM*'                                               255
         IDNUM_FLGM(1)=0                                                  255
         FLGM_TYPE=1                                                      255
         CALL AGDGETP('FPDMGEO2','*',-999,'FLGM',NFLGM_TYPE,              255
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        255
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                255
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2                    256
         ZTOT =(FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0             257
         RTOT = FBXD_NX*WID/2.0                                           258
         BWID = RTOT + FBXD_XOFFSET                                       259
         X0 = BWID - FBXD_XOFFSET - WID/2                                 260
         Y1 = RTOT - WID/2                                                261
         Z1 = - FBXD_DEPTH/2 + FBXD_ZOFFSET + ZTOT                        262
C    *                                                                    263
         I=1                                                              263
            GO TO 5043                                                    263
5041        I=I+1                                                         263
5043        IF(I-(FBXD_NY).GT.0) GO TO 5042                               263
            X1=X0                                                         264
C       *                                                                 265
            J=1                                                           265
               GO TO 5053                                                 265
5051           J=J+1                                                      265
5053           IF(J-(FBXD_NX).GT.0) GO TO 5052                            265
C          Create FLGT :                                                  266
C          Call To Local Procedure FLGT                                   266
               ASSIGN 5070 TO BLOCK_FLGT                                  266
               GO TO  5060                                                266
5070           CONTINUE                                                   266
C          *                                                              266
C          Geant POSITION : FLGT ===> X=X1 Y=Y1 Z=Z1 <===                 267
               CALL AGSRESET                                              267
               AG_X = X1                                                  267
               AG_Y = Y1                                                  267
               AG_Z = Z1                                                  267
               AG_TITLE = 'FLGT'                                          267
               AG_EXNAME = 'POSITION'                                     267
               AG_PARLIST = 'X_Y_Z'                                       267
               CALL AXPOSITION                                            267
C          *                                                              267
               X1=X1-WID                                                  267
            GO TO 5051                                                    269
5052        CONTINUE                                                      269
            Y1=Y1-WID                                                     270
         GO TO 5041                                                       271
5042     CONTINUE                                                         271
C    Check FBXD_TYPE.EQ.1                                                 272
         IF (FBXD_TYPE.EQ.1) THEN                                         272
         X1=X0                                                            273
         Y1= -RTOT + ZTOT                                                 274
         Z1=-FBXD_DEPTH/2 + FBXD_PSOFFSET + WID/2                         275
C    *                                                                    276
         J=1                                                              276
            GO TO 5083                                                    276
5081        J=J+1                                                         276
5083        IF(J-(FBXD_NX).GT.0) GO TO 5082                               276
C       Create FLGT :                                                     277
C       Call To Local Procedure FLGT                                      277
            ASSIGN 5090 TO BLOCK_FLGT                                     277
            GO TO  5060                                                   277
5090        CONTINUE                                                      277
C       *                                                                 277
C       Geant POSITION : FLGT ===> X=X1 Y=Y1 Z=Z1 ALPHAX=90 <===          278
            CALL AGSRESET                                                 278
            AG_X = X1                                                     278
            AG_Y = Y1                                                     278
            AG_Z = Z1                                                     278
            AG_ALPHAN=AG_ALPHAN+1                                         278
            AG_ALPHAK(AG_ALPHAN)=1                                        278
            AG_ALPHAL(AG_ALPHAN)=90                                       278
            AG_TITLE = 'FLGT'                                             278
            AG_EXNAME = 'POSITION'                                        278
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   278
            CALL AXPOSITION                                               278
C       *                                                                 278
            X1=X1-WID                                                     278
         GO TO 5081                                                       280
5082     CONTINUE                                                         280
C    Check FMCG_PBPLATE==1                                                281
         IF (FMCG_PBPLATE.EQ.1) THEN                                      281
C    Create PBPT :                                                        282
C    Call To Local Procedure PBPT                                         282
         ASSIGN 5110 TO BLOCK_PBPT                                        282
         GO TO  5100                                                      282
5110     CONTINUE                                                         282
C    *                                                                    282
C    Geant POSITION : PBPT ===> X=0 Y=0 Z=PBPD_THICK/2.0-FBXD_DEPTH/2 <=  283
C    ==                                                                   283
         CALL AGSRESET                                                    283
         AG_X = 0                                                         283
         AG_Y = 0                                                         283
         AG_Z = PBPD_THICK/2.0-FBXD_DEPTH/2                               283
         AG_TITLE = 'PBPT'                                                283
         AG_EXNAME = 'POSITION'                                           283
         AG_PARLIST = 'X_Y_Z'                                             283
         CALL AXPOSITION                                                  283
C    *                                                                    283
         ENDIF                                                            283
         ZTOTSMD=FMXG_G10THICK+FMXG_SAPEX                                 285
C    Create FSHM :                                                        286
C    Call To Local Procedure FSHM                                         286
         ASSIGN 5130 TO BLOCK_FSHM                                        286
         GO TO  5120                                                      286
5130     CONTINUE                                                         286
C    *                                                                    286
C    Geant POSITION : FSHM ===> X=0 Y=0 Z=FBXD_SMDOFF+ZTOTSMD-FBXD_DEPTH  287
C    /2 <===                                                              287
         CALL AGSRESET                                                    287
         AG_X = 0                                                         287
         AG_Y = 0                                                         287
         AG_Z = FBXD_SMDOFF+ZTOTSMD-FBXD_DEPTH/2                          287
         AG_TITLE = 'FSHM'                                                287
         AG_EXNAME = 'POSITION'                                           287
         AG_PARLIST = 'X_Y_Z'                                             287
         CALL AXPOSITION                                                  287
C    *                                                                    287
         ENDIF                                                            287
C    Check FBXD_TYPE.EQ.3                                                 289
         IF (FBXD_TYPE.EQ.3) THEN                                         289
C    use FLGG bank                                                        290
         BPATH_FLGG='FLGG*'                                               290
         IDNUM_FLGG(1)=0                                                  290
         FLGG_TYPE=2                                                      290
         CALL AGDGETP('FPDMGEO2','*',-999,'FLGG',NFLGG_TYPE,              290
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        290
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                290
C    use FLGM bank                                                        291
         BPATH_FLGM='FLGM*'                                               291
         IDNUM_FLGM(1)=0                                                  291
         FLGM_TYPE=2                                                      291
         CALL AGDGETP('FPDMGEO2','*',-999,'FLGM',NFLGM_TYPE,              291
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        291
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                291
         WID = FLGG_WIDTH + FLGG_DGAP                                     292
         ZTOT = FLGG_DEPTH/2.0                                            293
         RTOT = 14*WID/2.0                                                294
         BWID = RTOT                                                      295
         X0 = BWID - WID/2                                                296
         Y1 = RTOT - WID/2                                                297
         Z1 = -FBXD_DEPTH/2 + FBXD_ZOFFSET + ZTOT                         298
C    *                                                                    299
         I=1                                                              299
            GO TO 5143                                                    299
5141        I=I+1                                                         299
5143        IF(I-(14).GT.0) GO TO 5142                                    299
            X1=X0                                                         300
C       *                                                                 301
            J=1                                                           301
               GO TO 5153                                                 301
5151           J=J+1                                                      301
5153           IF(J-(14).GT.0) GO TO 5152                                 301
C          Check I.GT.5.AND.I.LT.10.AND.J.GT.5.AND.J.LT.10                302
               IF (I.GT.5.AND.I.LT.10.AND.J.GT.5.AND.J.LT.10) THEN        302
               X1=X1-WID                                                  303
               ELSE                                                       304
C          Create FLGF :                                                  305
C          Call To Local Procedure FLGF                                   305
               ASSIGN 5170 TO BLOCK_FLGF                                  305
               GO TO  5160                                                305
5170           CONTINUE                                                   305
C          *                                                              305
C          Geant POSITION : FLGF ===> X=X1 Y=Y1 Z=Z1 <===                 306
               CALL AGSRESET                                              306
               AG_X = X1                                                  306
               AG_Y = Y1                                                  306
               AG_Z = Z1                                                  306
               AG_TITLE = 'FLGF'                                          306
               AG_EXNAME = 'POSITION'                                     306
               AG_PARLIST = 'X_Y_Z'                                       306
               CALL AXPOSITION                                            306
C          *                                                              306
               X1=X1-WID                                                  306
               ENDIF                                                      308
            GO TO 5151                                                    309
5152        CONTINUE                                                      309
            Y1=Y1-WID                                                     310
         GO TO 5141                                                       311
5142     CONTINUE                                                         311
         ENDIF                                                            312
5021  CALL AGSPOPB                                                        313
      GO TO BLOCK_FBOX, (5020 ,5030)                                      313
C End of Local Procedure                                                  313
C ----------------------------------------------------------------------  314
C ------------ FLGT                                                       314
C Begin Local Procedure FLGT                                              315
C FLGT IS ONE PBG TOWER                                                   315
5060  CONTINUE                                                            315
         AG_TITLE='FLGT'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
C    Geant MATERIAL : AIR ===> inherited arguments used <===              316
         CALL AGSRESET                                                    316
         AG_TITLE = 'AIR'                                                 316
         AG_EXNAME = 'MATERIAL'                                           316
         AG_PARLIST = 'NONE'                                              316
         CALL AXMATERIAL                                                  316
C    *                                                                    316
C    Geant ATTRIBUTE : FLGT ===> SEEN=1 COLO=2 <===                       316
         CALL AGSRESET                                                    317
         AG_SEEN = 1                                                      317
         AG_COLO = 2                                                      317
         AG_TITLE = 'FLGT'                                                317
         AG_EXNAME = 'ATTRIBUTE'                                          317
         AG_PARLIST = 'SEEN_COLO'                                         317
         CALL AXATTRIBUTE                                                 317
C    *                                                                    317
C    Geant SHAPE : BOX ===> DX=WID/2 DY=WID/2 DZ=ZTOT <===                317
         CALL AGSRESET                                                    318
         AG_DX = WID/2                                                    318
         AG_DY = WID/2                                                    318
         AG_DZ = ZTOT                                                     318
         AG_TITLE = 'BOX'                                                 318
         AG_EXNAME = 'SHAPE'                                              318
         AG_PARLIST = 'DX_DY_DZ'                                          318
         CALL AXSHAPE                                                     318
C    *                                                                    318
C    Create FWAL :                                                        319
C    Call To Local Procedure FWAL                                         319
         ASSIGN 5190 TO BLOCK_FWAL                                        319
         GO TO  5180                                                      319
5190     CONTINUE                                                         319
C    *                                                                    319
C    Geant POSITION : FWAL ===> Z=-ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0 <=  320
C    ==                                                                   320
         CALL AGSRESET                                                    320
         AG_Z = -ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0                       320
         AG_TITLE = 'FWAL'                                                320
         AG_EXNAME = 'POSITION'                                           320
         AG_PARLIST = 'Z'                                                 320
         CALL AXPOSITION                                                  320
C    *                                                                    320
C    Create FUMT :                                                        321
C    Call To Local Procedure FUMT                                         321
         ASSIGN 5210 TO BLOCK_FUMT                                        321
         GO TO  5200                                                      321
5210     CONTINUE                                                         321
C    *                                                                    321
C    Geant POSITION : FUMT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUM  322
C    ETDZ/2.0 <===                                                        322
         CALL AGSRESET                                                    322
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUMETDZ/2.0            322
         AG_TITLE = 'FUMT'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'Z'                                                 322
         CALL AXPOSITION                                                  322
C    *                                                                    322
C    Create FPCT :                                                        323
C    Call To Local Procedure FPCT                                         323
         ASSIGN 5230 TO BLOCK_FPCT                                        323
         GO TO  5220                                                      323
5230     CONTINUE                                                         323
C    *                                                                    323
C    Geant POSITION : FPCT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHC  324
C    ATHDZ/2.0 <===                                                       324
         CALL AGSRESET                                                    324
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHCATHDZ/2.0           324
         AG_TITLE = 'FPCT'                                                324
         AG_EXNAME = 'POSITION'                                           324
         AG_PARLIST = 'Z'                                                 324
         CALL AXPOSITION                                                  324
C    *                                                                    324
5061  CALL AGSPOPB                                                        325
      GO TO BLOCK_FLGT, (5060 ,5070,5090)                                 325
C End of Local Procedure                                                  325
C ----------------------------------------------------------------------  326
C ------------ FWAL                                                       326
C Begin Local Procedure FWAL                                              327
C FWAL IS ALMUNUM WRAPPER                                                 327
5180  CONTINUE                                                            327
         AG_TITLE='FWAL'                                                  327
         AG_MODULE=MODULE                                                 327
         AG_MARK='V00'                                                    327
         CALL AGSPUSH                                                     327
         AG_MODULE=MODULE                                                 327
C    *                                                                    327
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        328
         CALL AGSRESET                                                    328
         AG_TITLE = 'ALUMINIUM'                                           328
         AG_EXNAME = 'MATERIAL'                                           328
         AG_PARLIST = 'NONE'                                              328
         CALL AXMATERIAL                                                  328
C    *                                                                    328
C    Geant ATTRIBUTE : FWAL ===> SEEN=1 COLO=3 <===                       328
         CALL AGSRESET                                                    329
         AG_SEEN = 1                                                      329
         AG_COLO = 3                                                      329
         AG_TITLE = 'FWAL'                                                329
         AG_EXNAME = 'ATTRIBUTE'                                          329
         AG_PARLIST = 'SEEN_COLO'                                         329
         CALL AXATTRIBUTE                                                 329
C    *                                                                    329
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0+FLGG_ALTHICK, DY=FLGG_WIDT  329
C    H/2.0+FLGG_ALTHICK DZ=FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0 <===           329
         CALL AGSRESET                                                    331
         AG_DX = FLGG_WIDTH/2.0+FLGG_ALTHICK                              331
         AG_DY = FLGG_WIDTH/2.0+FLGG_ALTHICK                              331
         AG_DZ = FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0                          331
         AG_TITLE = 'BOX'                                                 331
         AG_EXNAME = 'SHAPE'                                              331
         AG_PARLIST = 'DX_DY_DZ'                                          331
         CALL AXSHAPE                                                     331
C    *                                                                    331
C    Check FMCG_CHKVSIM==1                                                331
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      332
         CALL GSCKOV( AG_IMED,N,E,ABSCO_ALM,EFFIC_ALL,RINDEX_ALM )        333
         ENDIF                                                            334
C    Create FLGR :                                                        335
C    Call To Local Procedure FLGR                                         335
         ASSIGN 5250 TO BLOCK_FLGR                                        335
         GO TO  5240                                                      335
5250     CONTINUE                                                         335
C    *                                                                    335
C    Geant POSITION : FLGR ===> Z=+FLGG_ALTHICK/2.0 <===                  336
         CALL AGSRESET                                                    336
         AG_Z = +FLGG_ALTHICK/2.0                                         336
         AG_TITLE = 'FLGR'                                                336
         AG_EXNAME = 'POSITION'                                           336
         AG_PARLIST = 'Z'                                                 336
         CALL AXPOSITION                                                  336
C    *                                                                    336
5181  CALL AGSPOPB                                                        337
      GO TO BLOCK_FWAL, (5180 ,5190)                                      337
C End of Local Procedure                                                  337
C ----------------------------------------------------------------------  338
C ------------ FLGR                                                       338
C Begin Local Procedure FLGR                                              339
C FLGR IS LEAD GLASS DETECTOR                                             339
5240  CONTINUE                                                            339
         AG_TITLE='FLGR'                                                  339
         AG_MODULE=MODULE                                                 339
         AG_MARK='V00'                                                    339
         CALL AGSPUSH                                                     339
         AG_MODULE=MODULE                                                 339
C    *                                                                    339
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.60712 <===                340
         CALL AGSRESET                                                    340
         AG_A = 207.19                                                    340
         AG_Z = 82                                                        340
         AG_W = .60712                                                    340
         AG_TITLE = 'PB'                                                  340
         AG_EXNAME = 'COMPONENT'                                          340
         AG_PARLIST = 'A_Z_W'                                             340
         CALL AXCOMPONENT                                                 340
C    *                                                                    340
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.02324 <===                 340
         CALL AGSRESET                                                    341
         AG_A = 39.102                                                    341
         AG_Z = 19                                                        341
         AG_W = .02324                                                    341
         AG_TITLE = 'K'                                                   341
         AG_EXNAME = 'COMPONENT'                                          341
         AG_PARLIST = 'A_Z_W'                                             341
         CALL AXCOMPONENT                                                 341
C    *                                                                    341
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.14771 <===                341
         CALL AGSRESET                                                    342
         AG_A = 28.088                                                    342
         AG_Z = 14                                                        342
         AG_W = .14771                                                    342
         AG_TITLE = 'SI'                                                  342
         AG_EXNAME = 'COMPONENT'                                          342
         AG_PARLIST = 'A_Z_W'                                             342
         CALL AXCOMPONENT                                                 342
C    *                                                                    342
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.22041 <===                  342
         CALL AGSRESET                                                    343
         AG_A = 15.999                                                    343
         AG_Z = 8                                                         343
         AG_W = .22041                                                    343
         AG_TITLE = 'O'                                                   343
         AG_EXNAME = 'COMPONENT'                                          343
         AG_PARLIST = 'A_Z_W'                                             343
         CALL AXCOMPONENT                                                 343
C    *                                                                    343
C    Geant COMPONENT : AS ===> A=74.922 Z=33 W=.00152 <===                343
         CALL AGSRESET                                                    344
         AG_A = 74.922                                                    344
         AG_Z = 33                                                        344
         AG_W = .00152                                                    344
         AG_TITLE = 'AS'                                                  344
         AG_EXNAME = 'COMPONENT'                                          344
         AG_PARLIST = 'A_Z_W'                                             344
         CALL AXCOMPONENT                                                 344
C    *                                                                    344
C    Geant MIXTURE : PBG ===> DENS=FLGM_DENSITY <===                      344
         CALL AGSRESET                                                    345
         AG_DENS = FLGM_DENSITY                                           345
         AG_TITLE = 'PBG'                                                 345
         AG_EXNAME = 'MIXTURE'                                            345
         AG_PARLIST = 'DENS'                                              345
         CALL AXMIXTURE                                                   345
C    *                                                                    345
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           345
         CALL AGSRESET                                                    346
         AG_ISVOL = 1                                                     346
         AG_TITLE = 'LEADGLASS'                                           346
         AG_EXNAME = 'MEDIUM'                                             346
         AG_PARLIST = 'ISVOL'                                             346
         CALL AXMEDIUM                                                    346
C    *                                                                    346
C    Geant ATTRIBUTE : FLGR ===> SEEN=1 COLO=4 <===                       346
         CALL AGSRESET                                                    347
         AG_SEEN = 1                                                      347
         AG_COLO = 4                                                      347
         AG_TITLE = 'FLGR'                                                347
         AG_EXNAME = 'ATTRIBUTE'                                          347
         AG_PARLIST = 'SEEN_COLO'                                         347
         CALL AXATTRIBUTE                                                 347
C    *                                                                    347
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2 DY=FLGG_WIDTH/2, DZ=FLGG_DEP  347
C    TH/2 <===                                                            347
         CALL AGSRESET                                                    349
         AG_DX = FLGG_WIDTH/2                                             349
         AG_DY = FLGG_WIDTH/2                                             349
         AG_DZ = FLGG_DEPTH/2                                             349
         AG_TITLE = 'BOX'                                                 349
         AG_EXNAME = 'SHAPE'                                              349
         AG_PARLIST = 'DX_DY_DZ'                                          349
         CALL AXSHAPE                                                     349
C    *                                                                    349
C    Defining Hits: ELOS:0:(0,50)                                         349
         AG_TITLE = 'FLGR'                                                350
         AG_EXNAME = 'HITS'                                               350
         AG_TYPE=AG_MAGIC                                                 350
         CALL AGSDETV('FLGR')                                             350
C    *                                                                    350
         AG_PARLIST='ELOS'                                                350
         AG_TITLE = 'DETH'                                                350
         AG_NBITS = 0                                                     350
         AG_BIN=AG_MAGIC                                                  350
         AG_OPTION='*'                                                    350
         AG_FHMIN = 0                                                     350
         AG_FHMAX = 50                                                    350
         CALL AGSDETH (0)                                                 350
C    *                                                                    350
         CALL AGSDETU('FLGR')                                             350
C    *                                                                    350
C    Check FMCG_CHKVSIM==1                                                351
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      351
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PBG,EFFIC_ALL,RINDEX_PBG )        352
         ENDIF                                                            353
5241  CALL AGSPOPB                                                        354
      GO TO BLOCK_FLGR, (5240 ,5250)                                      354
C End of Local Procedure                                                  354
C ----------------------------------------------------------------------  355
C ------------ FLGF                                                       355
C Begin Local Procedure FLGF                                              356
C FLGF IS LEAD GLASS DETECTOR                                             356
5160  CONTINUE                                                            356
         AG_TITLE='FLGF'                                                  356
         AG_MODULE=MODULE                                                 356
         AG_MARK='V00'                                                    356
         CALL AGSPUSH                                                     356
         AG_MODULE=MODULE                                                 356
C    *                                                                    356
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.60712 <===                357
         CALL AGSRESET                                                    357
         AG_A = 207.19                                                    357
         AG_Z = 82                                                        357
         AG_W = .60712                                                    357
         AG_TITLE = 'PB'                                                  357
         AG_EXNAME = 'COMPONENT'                                          357
         AG_PARLIST = 'A_Z_W'                                             357
         CALL AXCOMPONENT                                                 357
C    *                                                                    357
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.02324 <===                 357
         CALL AGSRESET                                                    358
         AG_A = 39.102                                                    358
         AG_Z = 19                                                        358
         AG_W = .02324                                                    358
         AG_TITLE = 'K'                                                   358
         AG_EXNAME = 'COMPONENT'                                          358
         AG_PARLIST = 'A_Z_W'                                             358
         CALL AXCOMPONENT                                                 358
C    *                                                                    358
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.14771 <===                358
         CALL AGSRESET                                                    359
         AG_A = 28.088                                                    359
         AG_Z = 14                                                        359
         AG_W = .14771                                                    359
         AG_TITLE = 'SI'                                                  359
         AG_EXNAME = 'COMPONENT'                                          359
         AG_PARLIST = 'A_Z_W'                                             359
         CALL AXCOMPONENT                                                 359
C    *                                                                    359
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.22041 <===                  359
         CALL AGSRESET                                                    360
         AG_A = 15.999                                                    360
         AG_Z = 8                                                         360
         AG_W = .22041                                                    360
         AG_TITLE = 'O'                                                   360
         AG_EXNAME = 'COMPONENT'                                          360
         AG_PARLIST = 'A_Z_W'                                             360
         CALL AXCOMPONENT                                                 360
C    *                                                                    360
C    Geant COMPONENT : AS ===> A=74.922 Z=33 W=.00152 <===                360
         CALL AGSRESET                                                    361
         AG_A = 74.922                                                    361
         AG_Z = 33                                                        361
         AG_W = .00152                                                    361
         AG_TITLE = 'AS'                                                  361
         AG_EXNAME = 'COMPONENT'                                          361
         AG_PARLIST = 'A_Z_W'                                             361
         CALL AXCOMPONENT                                                 361
C    *                                                                    361
C    Geant MIXTURE : PBG ===> DENS=FLGM_DENSITY <===                      361
         CALL AGSRESET                                                    362
         AG_DENS = FLGM_DENSITY                                           362
         AG_TITLE = 'PBG'                                                 362
         AG_EXNAME = 'MIXTURE'                                            362
         AG_PARLIST = 'DENS'                                              362
         CALL AXMIXTURE                                                   362
C    *                                                                    362
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           362
         CALL AGSRESET                                                    363
         AG_ISVOL = 1                                                     363
         AG_TITLE = 'LEADGLASS'                                           363
         AG_EXNAME = 'MEDIUM'                                             363
         AG_PARLIST = 'ISVOL'                                             363
         CALL AXMEDIUM                                                    363
C    *                                                                    363
C    Geant ATTRIBUTE : FLGF ===> SEEN=1 COLO=4 <===                       363
         CALL AGSRESET                                                    364
         AG_SEEN = 1                                                      364
         AG_COLO = 4                                                      364
         AG_TITLE = 'FLGF'                                                364
         AG_EXNAME = 'ATTRIBUTE'                                          364
         AG_PARLIST = 'SEEN_COLO'                                         364
         CALL AXATTRIBUTE                                                 364
C    *                                                                    364
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2 DY=FLGG_WIDTH/2, DZ=FLGG_DEP  364
C    TH/2 <===                                                            364
         CALL AGSRESET                                                    366
         AG_DX = FLGG_WIDTH/2                                             366
         AG_DY = FLGG_WIDTH/2                                             366
         AG_DZ = FLGG_DEPTH/2                                             366
         AG_TITLE = 'BOX'                                                 366
         AG_EXNAME = 'SHAPE'                                              366
         AG_PARLIST = 'DX_DY_DZ'                                          366
         CALL AXSHAPE                                                     366
C    *                                                                    366
         CALL GSTPAR( AG_IMED,'CUTELE',FLGM_CRITENE )                     366
C    Defining Hits: ELOS:0:(0,50)                                         368
         AG_TITLE = 'FLGF'                                                368
         AG_EXNAME = 'HITS'                                               368
         AG_TYPE=AG_MAGIC                                                 368
         CALL AGSDETV('FLGF')                                             368
C    *                                                                    368
         AG_PARLIST='ELOS'                                                368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 0                                                     368
         AG_BIN=AG_MAGIC                                                  368
         AG_OPTION='*'                                                    368
         AG_FHMIN = 0                                                     368
         AG_FHMAX = 50                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         CALL AGSDETU('FLGF')                                             368
C    *                                                                    368
5161  CALL AGSPOPB                                                        369
      GO TO BLOCK_FLGF, (5160 ,5170)                                      369
C End of Local Procedure                                                  369
C ----------------------------------------------------------------------  370
C ------------ FPCT                                                       370
C Begin Local Procedure FPCT                                              371
C FPCT IS PHOTO CATHODE                                                   371
5220  CONTINUE                                                            371
         AG_TITLE='FPCT'                                                  371
         AG_MODULE=MODULE                                                 371
         AG_MARK='V00'                                                    371
         CALL AGSPUSH                                                     371
         AG_MODULE=MODULE                                                 371
C    *                                                                    371
C    Geant MATERIAL : AIR ===> inherited arguments used <===              372
         CALL AGSRESET                                                    372
         AG_TITLE = 'AIR'                                                 372
         AG_EXNAME = 'MATERIAL'                                           372
         AG_PARLIST = 'NONE'                                              372
         CALL AXMATERIAL                                                  372
C    *                                                                    372
C    Geant MEDIUM : PHOTCATH ===> ISVOL=1 <===                            372
         CALL AGSRESET                                                    373
         AG_ISVOL = 1                                                     373
         AG_TITLE = 'PHOTCATH'                                            373
         AG_EXNAME = 'MEDIUM'                                             373
         AG_PARLIST = 'ISVOL'                                             373
         CALL AXMEDIUM                                                    373
C    *                                                                    373
C    Geant ATTRIBUTE : LPCT ===> SEEN=1 COLO=6 <===                       373
         CALL AGSRESET                                                    374
         AG_SEEN = 1                                                      374
         AG_COLO = 6                                                      374
         AG_TITLE = 'LPCT'                                                374
         AG_EXNAME = 'ATTRIBUTE'                                          374
         AG_PARLIST = 'SEEN_COLO'                                         374
         CALL AXATTRIBUTE                                                 374
C    *                                                                    374
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=FLGG_PHCATHR DZ=FLGG_PHCATHDZ/2  374
C    .0 <===                                                              374
         CALL AGSRESET                                                    375
         AG_RMIN = 0                                                      375
         AG_RMAX = FLGG_PHCATHR                                           375
         AG_DZ = FLGG_PHCATHDZ/2.0                                        375
         AG_TITLE = 'TUBE'                                                375
         AG_EXNAME = 'SHAPE'                                              375
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      375
         CALL AXSHAPE                                                     375
C    *                                                                    375
C    Check FMCG_CHKVSIM==1                                                375
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      376
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PHCATH,EFFIC_PHCATH,              377
     *   RINDEX_PHCATH )                                                  377
C    Defining Hits: USER:0:(0,100000)                                     378
         AG_TITLE = 'FPCT'                                                378
         AG_EXNAME = 'HITS'                                               378
         AG_TYPE=AG_MAGIC                                                 378
         CALL AGSDETV('FPCT')                                             378
C    *                                                                    378
         AG_PARLIST='USER'                                                378
         AG_TITLE = 'DETH'                                                378
         AG_NBITS = 0                                                     378
         AG_BIN=AG_MAGIC                                                  378
         AG_OPTION='*'                                                    378
         AG_FHMIN = 0                                                     378
         AG_FHMAX = 100000                                                378
         CALL AGSDETH (JUMPAD(FPCTSTEP))                                  378
C    *                                                                    378
         CALL AGSDETU('FPCT')                                             378
C    *                                                                    378
         ENDIF                                                            379
5221  CALL AGSPOPB                                                        380
      GO TO BLOCK_FPCT, (5220 ,5230)                                      380
C End of Local Procedure                                                  380
C ----------------------------------------------------------------------  381
C ------------ FUMT                                                       381
C Begin Local Procedure FUMT                                              382
C FUMT IS MU METAL                                                        382
5200  CONTINUE                                                            382
         AG_TITLE='FUMT'                                                  382
         AG_MODULE=MODULE                                                 382
         AG_MARK='V00'                                                    382
         CALL AGSPUSH                                                     382
         AG_MODULE=MODULE                                                 382
C    *                                                                    382
C    Geant MATERIAL : IRON ===> inherited arguments used <===             383
         CALL AGSRESET                                                    383
         AG_TITLE = 'IRON'                                                383
         AG_EXNAME = 'MATERIAL'                                           383
         AG_PARLIST = 'NONE'                                              383
         CALL AXMATERIAL                                                  383
C    *                                                                    383
C    Geant ATTRIBUTE : LUMT ===> SEEN=1 COLO=5 <===                       383
         CALL AGSRESET                                                    384
         AG_SEEN = 1                                                      384
         AG_COLO = 5                                                      384
         AG_TITLE = 'LUMT'                                                384
         AG_EXNAME = 'ATTRIBUTE'                                          384
         AG_PARLIST = 'SEEN_COLO'                                         384
         CALL AXATTRIBUTE                                                 384
C    *                                                                    384
C    Geant SHAPE : TUBE ===> RMIN=FLGG_PHCATHR RMAX=FLGG_MUMETR, DZ=FLGG  384
C    _MUMETDZ/2.0 <===                                                    384
         CALL AGSRESET                                                    386
         AG_RMIN = FLGG_PHCATHR                                           386
         AG_RMAX = FLGG_MUMETR                                            386
         AG_DZ = FLGG_MUMETDZ/2.0                                         386
         AG_TITLE = 'TUBE'                                                386
         AG_EXNAME = 'SHAPE'                                              386
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      386
         CALL AXSHAPE                                                     386
C    *                                                                    386
C    Check FMCG_CHKVSIM==1                                                386
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      387
         CALL GSCKOV( AG_IMED,N,E,ABSCO_MUMET,EFFIC_ALL,RINDEX_MUMET )    388
         ENDIF                                                            389
5201  CALL AGSPOPB                                                        390
      GO TO BLOCK_FUMT, (5200 ,5210)                                      390
C End of Local Procedure                                                  390
C ----------------------------------------------------------------------  391
C ------------ PBPT                                                       391
C Begin Local Procedure PBPT                                              392
C PBPT IS PB PLATE                                                        392
5100  CONTINUE                                                            392
         AG_TITLE='PBPT'                                                  392
         AG_MODULE=MODULE                                                 392
         AG_MARK='V00'                                                    392
         CALL AGSPUSH                                                     392
         AG_MODULE=MODULE                                                 392
C    *                                                                    392
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             393
         CALL AGSRESET                                                    393
         AG_TITLE = 'LEAD'                                                393
         AG_EXNAME = 'MATERIAL'                                           393
         AG_PARLIST = 'NONE'                                              393
         CALL AXMATERIAL                                                  393
C    *                                                                    393
C    Geant ATTRIBUTE : PBPT ===> SEEN=1 COLO=7 <===                       393
         CALL AGSRESET                                                    394
         AG_SEEN = 1                                                      394
         AG_COLO = 7                                                      394
         AG_TITLE = 'PBPT'                                                394
         AG_EXNAME = 'ATTRIBUTE'                                          394
         AG_PARLIST = 'SEEN_COLO'                                         394
         CALL AXATTRIBUTE                                                 394
C    *                                                                    394
C    Geant SHAPE : BOX ===> DX=PBPD_WIDTH/2.0 DY=PBPD_HEIGHT/2.0, DZ=PBP  394
C    D_THICK/2.0 <===                                                     394
         CALL AGSRESET                                                    396
         AG_DX = PBPD_WIDTH/2.0                                           396
         AG_DY = PBPD_HEIGHT/2.0                                          396
         AG_DZ = PBPD_THICK/2.0                                           396
         AG_TITLE = 'BOX'                                                 396
         AG_EXNAME = 'SHAPE'                                              396
         AG_PARLIST = 'DX_DY_DZ'                                          396
         CALL AXSHAPE                                                     396
C    *                                                                    396
5101  CALL AGSPOPB                                                        397
      GO TO BLOCK_PBPT, (5100 ,5110)                                      397
C End of Local Procedure                                                  397
C ----------------------------------------------------------------------  398
C ------------ FSHM                                                       398
C Begin Local Procedure FSHM                                              399
C FSHM IS THE SHOWER MAX SECTION                                          399
5120  CONTINUE                                                            399
         AG_TITLE='FSHM'                                                  399
         AG_MODULE=MODULE                                                 399
         AG_MARK='V00'                                                    399
         CALL AGSPUSH                                                     399
         AG_MODULE=MODULE                                                 399
C    *                                                                    399
C    Geant MATERIAL : AIR ===> inherited arguments used <===              400
         CALL AGSRESET                                                    400
         AG_TITLE = 'AIR'                                                 400
         AG_EXNAME = 'MATERIAL'                                           400
         AG_PARLIST = 'NONE'                                              400
         CALL AXMATERIAL                                                  400
C    *                                                                    400
C    Geant ATTRIBUTE : FSHM ===> SEEN=1 COLO=4 <===                       400
         CALL AGSRESET                                                    401
         AG_SEEN = 1                                                      401
         AG_COLO = 4                                                      401
         AG_TITLE = 'FSHM'                                                401
         AG_EXNAME = 'ATTRIBUTE'                                          401
         AG_PARLIST = 'SEEN_COLO'                                         401
         CALL AXATTRIBUTE                                                 401
C    *                                                                    401
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2 DY=FMXG_G10HGT/2, DZ=ZTOT  401
C    SMD <===                                                             401
         CALL AGSRESET                                                    403
         AG_DX = FMXG_G10WIDTH/2                                          403
         AG_DY = FMXG_G10HGT/2                                            403
         AG_DZ = ZTOTSMD                                                  403
         AG_TITLE = 'BOX'                                                 403
         AG_EXNAME = 'SHAPE'                                              403
         AG_PARLIST = 'DX_DY_DZ'                                          403
         CALL AXSHAPE                                                     403
C    *                                                                    403
         WSMD=FMXG_SBASE/2 + FMXG_SGAP                                    403
         WTOTSMD=(FMXG_NSTRIP + 1)*WSMD                                   405
         ZZZ= - ZTOTSMD + FMXG_G10THICK/2                                 406
C    Create FXGT :                                                        407
C    Call To Local Procedure FXGT                                         407
         ASSIGN 5270 TO BLOCK_FXGT                                        407
         GO TO  5260                                                      407
5270     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZZZ <===                        408
         CALL AGSRESET                                                    408
         AG_X = 0                                                         408
         AG_Y = 0                                                         408
         AG_Z = ZZZ                                                       408
         AG_TITLE = 'FXGT'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z'                                             408
         CALL AXPOSITION                                                  408
C    *                                                                    408
         XXX= - WTOTSMD/2 - FMXG_SGAP/2 + WSMD                            408
         YYY=0.0                                                          410
         ZZZ=ZZZ + FMXG_G10THICK/2 + FMXG_SAPEX/2                         411
C    *                                                                    412
         I=1                                                              412
            GO TO 5283                                                    412
5281        I=I+1                                                         412
5283        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5282                           412
C       Check MOD(I,2)!=0                                                 413
            IF (MOD(I,2).NE.0) THEN                                       413
C       Create FHMS :                                                     414
C       Call To Local Procedure FHMS                                      414
            ASSIGN 5300 TO BLOCK_FHMS                                     414
            GO TO  5290                                                   414
5300        CONTINUE                                                      414
C       *                                                                 414
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ <===                 415
            CALL AGSRESET                                                 415
            AG_X = XXX                                                    415
            AG_Y = YYY                                                    415
            AG_Z = ZZZ                                                    415
            AG_TITLE = 'FHMS'                                             415
            AG_EXNAME = 'POSITION'                                        415
            AG_PARLIST = 'X_Y_Z'                                          415
            CALL AXPOSITION                                               415
C       *                                                                 415
            ELSE                                                          415
C       Create FHMS :                                                     417
C       Call To Local Procedure FHMS                                      417
            ASSIGN 5310 TO BLOCK_FHMS                                     417
            GO TO  5290                                                   417
5310        CONTINUE                                                      417
C       *                                                                 417
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ ALPHAX=180 <===      418
            CALL AGSRESET                                                 418
            AG_X = XXX                                                    418
            AG_Y = YYY                                                    418
            AG_Z = ZZZ                                                    418
            AG_ALPHAN=AG_ALPHAN+1                                         418
            AG_ALPHAK(AG_ALPHAN)=1                                        418
            AG_ALPHAL(AG_ALPHAN)=180                                      418
            AG_TITLE = 'FHMS'                                             418
            AG_EXNAME = 'POSITION'                                        418
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   418
            CALL AXPOSITION                                               418
C       *                                                                 418
            ENDIF                                                         418
            XXX=XXX+WSMD                                                  420
         GO TO 5281                                                       421
5282     CONTINUE                                                         421
         ZZZ=ZZZ + FMXG_G10THICK/2 + FMXG_SAPEX/2                         422
C    Create FXGT :                                                        423
C    Call To Local Procedure FXGT                                         423
         ASSIGN 5320 TO BLOCK_FXGT                                        423
         GO TO  5260                                                      423
5320     CONTINUE                                                         423
C    *                                                                    423
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZZZ <===                        424
         CALL AGSRESET                                                    424
         AG_X = 0                                                         424
         AG_Y = 0                                                         424
         AG_Z = ZZZ                                                       424
         AG_TITLE = 'FXGT'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'X_Y_Z'                                             424
         CALL AXPOSITION                                                  424
C    *                                                                    424
         XXX=0.0                                                          424
         YYY= - WTOTSMD/2 - FMXG_SGAP/2 + WSMD                            426
         ZZZ=ZZZ + FMXG_G10THICK/2 + FMXG_SAPEX/2                         427
C    *                                                                    428
         I=1                                                              428
            GO TO 5333                                                    428
5331        I=I+1                                                         428
5333        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5332                           428
C       Check MOD(I,2)!=0                                                 429
            IF (MOD(I,2).NE.0) THEN                                       429
C       Create FHMS :                                                     430
C       Call To Local Procedure FHMS                                      430
            ASSIGN 5340 TO BLOCK_FHMS                                     430
            GO TO  5290                                                   430
5340        CONTINUE                                                      430
C       *                                                                 430
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ ALPHAZ=90 <===       431
            CALL AGSRESET                                                 431
            AG_X = XXX                                                    431
            AG_Y = YYY                                                    431
            AG_Z = ZZZ                                                    431
            AG_ALPHAN=AG_ALPHAN+1                                         431
            AG_ALPHAK(AG_ALPHAN)=3                                        431
            AG_ALPHAL(AG_ALPHAN)=90                                       431
            AG_TITLE = 'FHMS'                                             431
            AG_EXNAME = 'POSITION'                                        431
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   431
            CALL AXPOSITION                                               431
C       *                                                                 431
            ELSE                                                          431
C       Create FHMS :                                                     433
C       Call To Local Procedure FHMS                                      433
            ASSIGN 5350 TO BLOCK_FHMS                                     433
            GO TO  5290                                                   433
5350        CONTINUE                                                      433
C       *                                                                 433
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ ORT=YX-Z <===        434
            CALL AGSRESET                                                 434
            AG_X = XXX                                                    434
            AG_Y = YYY                                                    434
            AG_Z = ZZZ                                                    434
            AG_ALPHAN=AG_ALPHAN+1                                         434
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             434
            AG_TITLE = 'FHMS'                                             434
            AG_EXNAME = 'POSITION'                                        434
            AG_PARLIST = 'X_Y_Z_ORT'                                      434
            CALL AXPOSITION                                               434
C       *                                                                 434
            ENDIF                                                         434
            YYY=YYY+WSMD                                                  436
         GO TO 5331                                                       437
5332     CONTINUE                                                         437
5121  CALL AGSPOPB                                                        438
      GO TO BLOCK_FSHM, (5120 ,5130)                                      438
C End of Local Procedure                                                  438
C ----------------------------------------------------------------------  439
C ------------ FXGT                                                       439
C Begin Local Procedure FXGT                                              440
C FXGT IS THE G10 LAYER IN THE SMAX                                       440
5260  CONTINUE                                                            440
         AG_TITLE='FXGT'                                                  440
         AG_MODULE=MODULE                                                 440
         AG_MARK='V00'                                                    440
         CALL AGSPUSH                                                     440
         AG_MODULE=MODULE                                                 440
C    *                                                                    440
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          441
         CALL AGSRESET                                                    441
         AG_A = 28.08                                                     441
         AG_Z = 14                                                        441
         AG_W = 0.6*1*28./60.                                             441
         AG_TITLE = 'SI'                                                  441
         AG_EXNAME = 'COMPONENT'                                          441
         AG_PARLIST = 'A_Z_W'                                             441
         CALL AXCOMPONENT                                                 441
C    *                                                                    441
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               441
         CALL AGSRESET                                                    442
         AG_A = 16                                                        442
         AG_Z = 8                                                         442
         AG_W = 0.6*2*16./60.                                             442
         AG_TITLE = 'O'                                                   442
         AG_EXNAME = 'COMPONENT'                                          442
         AG_PARLIST = 'A_Z_W'                                             442
         CALL AXCOMPONENT                                                 442
C    *                                                                    442
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              442
         CALL AGSRESET                                                    443
         AG_A = 12                                                        443
         AG_Z = 6                                                         443
         AG_W = 0.4*8*12./174.                                            443
         AG_TITLE = 'C'                                                   443
         AG_EXNAME = 'COMPONENT'                                          443
         AG_PARLIST = 'A_Z_W'                                             443
         CALL AXCOMPONENT                                                 443
C    *                                                                    443
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               443
         CALL AGSRESET                                                    444
         AG_A = 1                                                         444
         AG_Z = 1                                                         444
         AG_W = 0.4*14*1./174.                                            444
         AG_TITLE = 'H'                                                   444
         AG_EXNAME = 'COMPONENT'                                          444
         AG_PARLIST = 'A_Z_W'                                             444
         CALL AXCOMPONENT                                                 444
C    *                                                                    444
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              444
         CALL AGSRESET                                                    445
         AG_A = 16                                                        445
         AG_Z = 8                                                         445
         AG_W = 0.4*4*16./174.                                            445
         AG_TITLE = 'O'                                                   445
         AG_EXNAME = 'COMPONENT'                                          445
         AG_PARLIST = 'A_Z_W'                                             445
         CALL AXCOMPONENT                                                 445
C    *                                                                    445
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               445
         CALL AGSRESET                                                    446
         AG_DENS = 1.7                                                    446
         AG_TITLE = 'G10'                                                 446
         AG_EXNAME = 'MIXTURE'                                            446
         AG_PARLIST = 'DENS'                                              446
         CALL AXMIXTURE                                                   446
C    *                                                                    446
C    Geant ATTRIBUTE : FXGT ===> SEEN=1 COLO=7 <===                       446
         CALL AGSRESET                                                    447
         AG_SEEN = 1                                                      447
         AG_COLO = 7                                                      447
         AG_TITLE = 'FXGT'                                                447
         AG_EXNAME = 'ATTRIBUTE'                                          447
         AG_PARLIST = 'SEEN_COLO'                                         447
         CALL AXATTRIBUTE                                                 447
C    *                                                                    447
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2 DY=FMXG_G10HGT/2, DZ=FMXG  447
C    _G10THICK/2 <===                                                     447
         CALL AGSRESET                                                    449
         AG_DX = FMXG_G10WIDTH/2                                          449
         AG_DY = FMXG_G10HGT/2                                            449
         AG_DZ = FMXG_G10THICK/2                                          449
         AG_TITLE = 'BOX'                                                 449
         AG_EXNAME = 'SHAPE'                                              449
         AG_PARLIST = 'DX_DY_DZ'                                          449
         CALL AXSHAPE                                                     449
C    *                                                                    449
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           449
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           451
5261  CALL AGSPOPB                                                        452
      GO TO BLOCK_FXGT, (5260 ,5270,5320)                                 452
C End of Local Procedure                                                  452
C ----------------------------------------------------------------------  453
C ------------ FHMS                                                       453
C Begin Local Procedure FHMS                                              454
C FHMS IS SHOWER MAX STRIP                                                454
5290  CONTINUE                                                            454
         AG_TITLE='FHMS'                                                  454
         AG_MODULE=MODULE                                                 454
         AG_MARK='V00'                                                    454
         CALL AGSPUSH                                                     454
         AG_MODULE=MODULE                                                 454
C    *                                                                    454
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       455
         CALL AGSRESET                                                    455
         AG_TITLE = 'POLYSTYREN'                                          455
         AG_EXNAME = 'MATERIAL'                                           455
         AG_PARLIST = 'NONE'                                              455
         CALL AXMATERIAL                                                  455
C    *                                                                    455
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       455
         CALL AGSRESET                                                    456
         AG_ISVOL = 1                                                     456
         AG_TITLE = 'CPOLYSTYREN'                                         456
         AG_EXNAME = 'MATERIAL'                                           456
         AG_PARLIST = 'ISVOL'                                             456
         CALL AXMATERIAL                                                  456
C    *                                                                    456
C    Geant ATTRIBUTE : FHMS ===> SEEN=1 COLO=2 <===                       456
         CALL AGSRESET                                                    457
         AG_SEEN = 1                                                      457
         AG_COLO = 2                                                      457
         AG_TITLE = 'FHMS'                                                457
         AG_EXNAME = 'ATTRIBUTE'                                          457
         AG_PARLIST = 'SEEN_COLO'                                         457
         CALL AXATTRIBUTE                                                 457
C    *                                                                    457
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=FMXG_SBASE/2 DY=FMXG_G10HGT/2, DZ  457
C    =FMXG_SAPEX/2 <===                                                   457
         CALL AGSRESET                                                    459
         AG_DX1 = 0                                                       459
         AG_DX2 = FMXG_SBASE/2                                            459
         AG_DY = FMXG_G10HGT/2                                            459
         AG_DZ = FMXG_SAPEX/2                                             459
         AG_TITLE = 'TRD1'                                                459
         AG_EXNAME = 'SHAPE'                                              459
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     459
         CALL AXSHAPE                                                     459
C    *                                                                    459
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           459
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             461
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             462
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                463
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.0130)                            464
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            465
C    Defining Hits: BIRK:0:(0,10)                                         466
         AG_TITLE = 'FHMS'                                                466
         AG_EXNAME = 'HITS'                                               466
         AG_TYPE=AG_MAGIC                                                 466
         CALL AGSDETV('FHMS')                                             466
C    *                                                                    466
         AG_PARLIST='BIRK'                                                466
         AG_TITLE = 'DETH'                                                466
         AG_NBITS = 0                                                     466
         AG_BIN=AG_MAGIC                                                  466
         AG_OPTION='*'                                                    466
         AG_FHMIN = 0                                                     466
         AG_FHMAX = 10                                                    466
         CALL AGSDETH (0)                                                 466
C    *                                                                    466
         CALL AGSDETU('FHMS')                                             466
C    *                                                                    466
5291  CALL AGSPOPB                                                        467
      GO TO BLOCK_FHMS, (5290 ,5300,5310,5340,5350)                       467
C End of Local Procedure                                                  467
      WRITE(LOUT,5360)'===>>  [End AgML Module FPDMGEO2]  <<==='          467
5360  FORMAT(A32,/,/)                                                     467
C MODULE FPDMGEO2                                                         469
      END                                                                 469
