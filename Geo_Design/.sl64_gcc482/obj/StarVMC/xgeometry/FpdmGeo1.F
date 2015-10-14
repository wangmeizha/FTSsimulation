C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FPDMGEO1                                                 1
C FPDMGEO1 IS THE FORWARD PION DETECTOR MODULES GEOMETRY                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FPDMGEO1'/, BANK_TITLE/'*'/                            1
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
C CREATED 12 NOV 2003                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'12 NOV 2003'/                                2
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
      INTEGER LEN_FMCG,LLEN_FMCG,LINK_FMCG                                8
      PARAMETER (LEN_FMCG=4)                                              8
      PARAMETER (LLEN_FMCG=2)                                             8
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
      REAL FBXD_SMDOFFSET                                                 10
      EQUIVALENCE (BANK_FBXD(11),FBXD_SMDOFFSET,RE_FBXD_SMDOFFSET),       10
     *(NAMES_FBXD(9),NFBXD_SMDOFFSET)                                     10
      EQUIVALENCE (COMMEN_FBXD(9),CFBXD_SMDOFFSET),(MAP_FBXD(1,9),        10
     *MFBXD_SMDOFFSET)                                                    10
      CHARACTER CFBXD_SMDOFFSET*40/' '/                                   10
      CHARACTER NFBXD_SMDOFFSET*12/'R.SMDOFFSET'/                         10
      INTEGER MFBXD_SMDOFFSET(2)/1,1/                                     10
      REAL RE_FBXD_SMDOFFSET                                              10
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
      REAL FLGG_WIDTH                                                     11
      EQUIVALENCE (BANK_FLGG(3),FLGG_WIDTH,RE_FLGG_WIDTH),                11
     *(NAMES_FLGG(1),NFLGG_WIDTH)                                         11
      EQUIVALENCE (COMMEN_FLGG(1),CFLGG_WIDTH),(MAP_FLGG(1,1),            11
     *MFLGG_WIDTH)                                                        11
      CHARACTER CFLGG_WIDTH*40/' '/                                       11
      CHARACTER NFLGG_WIDTH*12/'R.WIDTH'/                                 11
      INTEGER MFLGG_WIDTH(2)/1,1/                                         11
      REAL RE_FLGG_WIDTH                                                  11
C -                                                                       11
      REAL FLGG_DEPTH                                                     11
      EQUIVALENCE (BANK_FLGG(4),FLGG_DEPTH,RE_FLGG_DEPTH),                11
     *(NAMES_FLGG(2),NFLGG_DEPTH)                                         11
      EQUIVALENCE (COMMEN_FLGG(2),CFLGG_DEPTH),(MAP_FLGG(1,2),            11
     *MFLGG_DEPTH)                                                        11
      CHARACTER CFLGG_DEPTH*40/' '/                                       11
      CHARACTER NFLGG_DEPTH*12/'R.DEPTH'/                                 11
      INTEGER MFLGG_DEPTH(2)/1,1/                                         11
      REAL RE_FLGG_DEPTH                                                  11
C -                                                                       11
      REAL FLGG_DGAP                                                      11
      EQUIVALENCE (BANK_FLGG(5),FLGG_DGAP,RE_FLGG_DGAP),(NAMES_FLGG(3),   11
     *NFLGG_DGAP)                                                         11
      EQUIVALENCE (COMMEN_FLGG(3),CFLGG_DGAP),(MAP_FLGG(1,3),             11
     *MFLGG_DGAP)                                                         11
      CHARACTER CFLGG_DGAP*40/' '/                                        11
      CHARACTER NFLGG_DGAP*12/'R.DGAP'/                                   11
      INTEGER MFLGG_DGAP(2)/1,1/                                          11
      REAL RE_FLGG_DGAP                                                   11
C -                                                                       11
      REAL FLGG_ALTHICK                                                   11
      EQUIVALENCE (BANK_FLGG(6),FLGG_ALTHICK,RE_FLGG_ALTHICK),            11
     *(NAMES_FLGG(4),NFLGG_ALTHICK)                                       11
      EQUIVALENCE (COMMEN_FLGG(4),CFLGG_ALTHICK),(MAP_FLGG(1,4),          11
     *MFLGG_ALTHICK)                                                      11
      CHARACTER CFLGG_ALTHICK*40/' '/                                     11
      CHARACTER NFLGG_ALTHICK*12/'R.ALTHICK'/                             11
      INTEGER MFLGG_ALTHICK(2)/1,1/                                       11
      REAL RE_FLGG_ALTHICK                                                11
C -                                                                       11
      REAL FLGG_PHCATHDZ                                                  11
      EQUIVALENCE (BANK_FLGG(7),FLGG_PHCATHDZ,RE_FLGG_PHCATHDZ),          11
     *(NAMES_FLGG(5),NFLGG_PHCATHDZ)                                      11
      EQUIVALENCE (COMMEN_FLGG(5),CFLGG_PHCATHDZ),(MAP_FLGG(1,5),         11
     *MFLGG_PHCATHDZ)                                                     11
      CHARACTER CFLGG_PHCATHDZ*40/' '/                                    11
      CHARACTER NFLGG_PHCATHDZ*12/'R.PHCATHDZ'/                           11
      INTEGER MFLGG_PHCATHDZ(2)/1,1/                                      11
      REAL RE_FLGG_PHCATHDZ                                               11
C -                                                                       11
      REAL FLGG_PHCATHR                                                   11
      EQUIVALENCE (BANK_FLGG(8),FLGG_PHCATHR,RE_FLGG_PHCATHR),            11
     *(NAMES_FLGG(6),NFLGG_PHCATHR)                                       11
      EQUIVALENCE (COMMEN_FLGG(6),CFLGG_PHCATHR),(MAP_FLGG(1,6),          11
     *MFLGG_PHCATHR)                                                      11
      CHARACTER CFLGG_PHCATHR*40/' '/                                     11
      CHARACTER NFLGG_PHCATHR*12/'R.PHCATHR'/                             11
      INTEGER MFLGG_PHCATHR(2)/1,1/                                       11
      REAL RE_FLGG_PHCATHR                                                11
C -                                                                       11
      REAL FLGG_MUMETDZ                                                   11
      EQUIVALENCE (BANK_FLGG(9),FLGG_MUMETDZ,RE_FLGG_MUMETDZ),            11
     *(NAMES_FLGG(7),NFLGG_MUMETDZ)                                       11
      EQUIVALENCE (COMMEN_FLGG(7),CFLGG_MUMETDZ),(MAP_FLGG(1,7),          11
     *MFLGG_MUMETDZ)                                                      11
      CHARACTER CFLGG_MUMETDZ*40/' '/                                     11
      CHARACTER NFLGG_MUMETDZ*12/'R.MUMETDZ'/                             11
      INTEGER MFLGG_MUMETDZ(2)/1,1/                                       11
      REAL RE_FLGG_MUMETDZ                                                11
C -                                                                       11
      REAL FLGG_MUMETR                                                    11
      EQUIVALENCE (BANK_FLGG(10),FLGG_MUMETR,RE_FLGG_MUMETR),             11
     *(NAMES_FLGG(8),NFLGG_MUMETR)                                        11
      EQUIVALENCE (COMMEN_FLGG(8),CFLGG_MUMETR),(MAP_FLGG(1,8),           11
     *MFLGG_MUMETR)                                                       11
      CHARACTER CFLGG_MUMETR*40/' '/                                      11
      CHARACTER NFLGG_MUMETR*12/'R.MUMETR'/                               11
      INTEGER MFLGG_MUMETR(2)/1,1/                                        11
      REAL RE_FLGG_MUMETR                                                 11
C -                                                                       11
      INTEGER LEN_FLGG,LLEN_FLGG,LINK_FLGG                                11
      PARAMETER (LEN_FLGG=10)                                             11
      PARAMETER (LLEN_FLGG=8)                                             11
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
      REAL FLGM_DENSITY                                                   12
      EQUIVALENCE (BANK_FLGM(3),FLGM_DENSITY,RE_FLGM_DENSITY),            12
     *(NAMES_FLGM(1),NFLGM_DENSITY)                                       12
      EQUIVALENCE (COMMEN_FLGM(1),CFLGM_DENSITY),(MAP_FLGM(1,1),          12
     *MFLGM_DENSITY)                                                      12
      CHARACTER CFLGM_DENSITY*40/' '/                                     12
      CHARACTER NFLGM_DENSITY*12/'R.DENSITY'/                             12
      INTEGER MFLGM_DENSITY(2)/1,1/                                       12
      REAL RE_FLGM_DENSITY                                                12
C -                                                                       12
      REAL FLGM_RADLEN                                                    12
      EQUIVALENCE (BANK_FLGM(4),FLGM_RADLEN,RE_FLGM_RADLEN),              12
     *(NAMES_FLGM(2),NFLGM_RADLEN)                                        12
      EQUIVALENCE (COMMEN_FLGM(2),CFLGM_RADLEN),(MAP_FLGM(1,2),           12
     *MFLGM_RADLEN)                                                       12
      CHARACTER CFLGM_RADLEN*40/' '/                                      12
      CHARACTER NFLGM_RADLEN*12/'R.RADLEN'/                               12
      INTEGER MFLGM_RADLEN(2)/1,1/                                        12
      REAL RE_FLGM_RADLEN                                                 12
C -                                                                       12
      REAL FLGM_PBCONT                                                    12
      EQUIVALENCE (BANK_FLGM(5),FLGM_PBCONT,RE_FLGM_PBCONT),              12
     *(NAMES_FLGM(3),NFLGM_PBCONT)                                        12
      EQUIVALENCE (COMMEN_FLGM(3),CFLGM_PBCONT),(MAP_FLGM(1,3),           12
     *MFLGM_PBCONT)                                                       12
      CHARACTER CFLGM_PBCONT*40/' '/                                      12
      CHARACTER NFLGM_PBCONT*12/'R.PBCONT'/                               12
      INTEGER MFLGM_PBCONT(2)/1,1/                                        12
      REAL RE_FLGM_PBCONT                                                 12
C -                                                                       12
      REAL FLGM_CRITENE                                                   12
      EQUIVALENCE (BANK_FLGM(6),FLGM_CRITENE,RE_FLGM_CRITENE),            12
     *(NAMES_FLGM(4),NFLGM_CRITENE)                                       12
      EQUIVALENCE (COMMEN_FLGM(4),CFLGM_CRITENE),(MAP_FLGM(1,4),          12
     *MFLGM_CRITENE)                                                      12
      CHARACTER CFLGM_CRITENE*40/' '/                                     12
      CHARACTER NFLGM_CRITENE*12/'R.CRITENE'/                             12
      INTEGER MFLGM_CRITENE(2)/1,1/                                       12
      REAL RE_FLGM_CRITENE                                                12
C -                                                                       12
      REAL FLGM_MOLIERER                                                  12
      EQUIVALENCE (BANK_FLGM(7),FLGM_MOLIERER,RE_FLGM_MOLIERER),          12
     *(NAMES_FLGM(5),NFLGM_MOLIERER)                                      12
      EQUIVALENCE (COMMEN_FLGM(5),CFLGM_MOLIERER),(MAP_FLGM(1,5),         12
     *MFLGM_MOLIERER)                                                     12
      CHARACTER CFLGM_MOLIERER*40/' '/                                    12
      CHARACTER NFLGM_MOLIERER*12/'R.MOLIERER'/                           12
      INTEGER MFLGM_MOLIERER(2)/1,1/                                      12
      REAL RE_FLGM_MOLIERER                                               12
C -                                                                       12
      INTEGER LEN_FLGM,LLEN_FLGM,LINK_FLGM                                12
      PARAMETER (LEN_FLGM=7)                                              12
      PARAMETER (LLEN_FLGM=5)                                             12
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
      REAL FMXG_G10HEIGHT                                                 14
      EQUIVALENCE (BANK_FMXG(9),FMXG_G10HEIGHT,RE_FMXG_G10HEIGHT),        14
     *(NAMES_FMXG(7),NFMXG_G10HEIGHT)                                     14
      EQUIVALENCE (COMMEN_FMXG(7),CFMXG_G10HEIGHT),(MAP_FMXG(1,7),        14
     *MFMXG_G10HEIGHT)                                                    14
      CHARACTER CFMXG_G10HEIGHT*40/' '/                                   14
      CHARACTER NFMXG_G10HEIGHT*12/'R.G10HEIGHT'/                         14
      INTEGER MFMXG_G10HEIGHT(2)/1,1/                                     14
      REAL RE_FMXG_G10HEIGHT                                              14
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
      INTEGER CHKVSIM,IMOD,ITYPE,TYPE                                     15
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
      FMCG_VERSION=6.0                                                    55
      CFMCG_VERSION='GEOMETRY VERSION'                                    55
C = 0 DE, = 1 CHERENKOV SIMULATION FOR PBG                                56
      FMCG_CHKVSIM=0                                                      56
      CFMCG_CHKVSIM='= 0 DE, = 1 CHERENKOV SIMULATION FOR PBG'            56
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    57
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           57
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   57
C *                                                                       57
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    57
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           57
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   57
C *                                                                       58
      CALL AGSBEGM(MODULE,IPRIN)                                          58
      IF (IPRIN.LT.0) RETURN                                              58
C Filling bank FPOS - FPD EN POSITIONING                                  58
      BANK_TITLE='FPD EN POSITIONING'                                     58
      BPATH_FPOS='FPOS*'                                                  58
      IDNUM_FPOS(1)=0                                                     59
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               59
      FPOS_IMOD=1                                                         59
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              59
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       60
      FPOS_ITYPE=1                                                        60
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     60
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                61
      FPOS_X=-28.1                                                        61
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  61
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                62
      FPOS_Y=0.0                                                          62
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  62
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               63
      FPOS_Z=-751.8                                                       63
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 63
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              64
      FPOS_AY=180                                                         64
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               64
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    65
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           65
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   65
C *                                                                       65
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    65
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           65
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   65
C *                                                                       66
      CALL AGSBEGM(MODULE,IPRIN)                                          66
      IF (IPRIN.LT.0) RETURN                                              66
C Filling bank FPOS - FPD ES POSITIONING                                  66
      BANK_TITLE='FPD ES POSITIONING'                                     66
      BPATH_FPOS='FPOS*'                                                  66
      IDNUM_FPOS(1)=0                                                     67
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               67
      FPOS_IMOD=2                                                         67
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              67
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       68
      FPOS_ITYPE=1                                                        68
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     68
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                69
      FPOS_X=28.1                                                         69
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  69
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                70
      FPOS_Y=0.0                                                          70
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  70
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               71
      FPOS_Z=-752.4                                                       71
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 71
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              72
      FPOS_AY=180                                                         72
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               72
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    73
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           73
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   73
C *                                                                       73
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    73
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           73
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   73
C *                                                                       74
      CALL AGSBEGM(MODULE,IPRIN)                                          74
      IF (IPRIN.LT.0) RETURN                                              74
C Filling bank FPOS - FPD ET POSITIONING                                  74
      BANK_TITLE='FPD ET POSITIONING'                                     74
      BPATH_FPOS='FPOS*'                                                  74
      IDNUM_FPOS(1)=0                                                     75
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               75
      FPOS_IMOD=3                                                         75
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              75
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       76
      FPOS_ITYPE=2                                                        76
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     76
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                77
      FPOS_X=0.0                                                          77
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  77
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                78
      FPOS_Y=30.8                                                         78
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  78
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               79
      FPOS_Z=-590.2                                                       79
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 79
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              80
      FPOS_AY=180                                                         80
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               80
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    81
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           81
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   81
C *                                                                       81
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    81
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           81
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   81
C *                                                                       82
      CALL AGSBEGM(MODULE,IPRIN)                                          82
      IF (IPRIN.LT.0) RETURN                                              82
C Filling bank FPOS - FPD EB POSITIONING                                  82
      BANK_TITLE='FPD EB POSITIONING'                                     82
      BPATH_FPOS='FPOS*'                                                  82
      IDNUM_FPOS(1)=0                                                     83
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               83
      FPOS_IMOD=4                                                         83
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              83
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       84
      FPOS_ITYPE=2                                                        84
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     84
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                85
      FPOS_X=0.0                                                          85
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  85
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                86
      FPOS_Y=-30.2                                                        86
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  86
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               87
      FPOS_Z=-590.2                                                       87
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 87
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              88
      FPOS_AY=180                                                         88
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               88
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    89
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           89
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   89
C *                                                                       89
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    89
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           89
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   89
C *                                                                       90
      CALL AGSBEGM(MODULE,IPRIN)                                          90
      IF (IPRIN.LT.0) RETURN                                              90
C Filling bank FPOS - FPD EN POSITIONING                                  90
      BANK_TITLE='FPD EN POSITIONING'                                     90
      BPATH_FPOS='FPOS*'                                                  90
      IDNUM_FPOS(1)=0                                                     91
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               91
      FPOS_IMOD=5                                                         91
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              91
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       92
      FPOS_ITYPE=1                                                        92
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     92
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                93
      FPOS_X=-28.1                                                        93
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  93
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                94
      FPOS_Y=0.0                                                          94
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  94
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               95
      FPOS_Z=751.8                                                        95
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 95
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              96
      FPOS_AY=0                                                           96
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               96
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    97
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           97
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   97
C *                                                                       97
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    97
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           97
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   97
C *                                                                       98
      CALL AGSBEGM(MODULE,IPRIN)                                          98
      IF (IPRIN.LT.0) RETURN                                              98
C Filling bank FPOS - FPD ES POSITIONING                                  98
      BANK_TITLE='FPD ES POSITIONING'                                     98
      BPATH_FPOS='FPOS*'                                                  98
      IDNUM_FPOS(1)=0                                                     99
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               99
      FPOS_IMOD=6                                                         99
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              99
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       100
      FPOS_ITYPE=1                                                        100
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     100
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                101
      FPOS_X=28.1                                                         101
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  101
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                102
      FPOS_Y=0.0                                                          102
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  102
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               103
      FPOS_Z=752.4                                                        103
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 103
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              104
      FPOS_AY=0                                                           104
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               104
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    105
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           105
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   105
C *                                                                       105
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    105
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           105
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   105
C *                                                                       106
      CALL AGSBEGM(MODULE,IPRIN)                                          106
      IF (IPRIN.LT.0) RETURN                                              106
C Filling bank FPOS - FPD ET POSITIONING                                  106
      BANK_TITLE='FPD ET POSITIONING'                                     106
      BPATH_FPOS='FPOS*'                                                  106
      IDNUM_FPOS(1)=0                                                     107
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               107
      FPOS_IMOD=7                                                         107
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              107
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       108
      FPOS_ITYPE=2                                                        108
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     108
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                109
      FPOS_X=0.0                                                          109
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  109
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                110
      FPOS_Y=30.8                                                         110
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  110
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               111
      FPOS_Z=590.2                                                        111
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 111
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              112
      FPOS_AY=0                                                           112
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               112
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    113
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           113
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   113
C *                                                                       113
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    113
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           113
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   113
C *                                                                       114
      CALL AGSBEGM(MODULE,IPRIN)                                          114
      IF (IPRIN.LT.0) RETURN                                              114
C Filling bank FPOS - FPD EB POSITIONING                                  114
      BANK_TITLE='FPD EB POSITIONING'                                     114
      BPATH_FPOS='FPOS*'                                                  114
      IDNUM_FPOS(1)=0                                                     115
C MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)                               115
      FPOS_IMOD=8                                                         115
      CFPOS_IMOD='MODULE# (EN=1, ES=2, ET=3, EB=4, WN=5...)'              115
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       116
      FPOS_ITYPE=2                                                        116
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                     116
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                117
      FPOS_X=0.0                                                          117
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  117
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                118
      FPOS_Y=-30.2                                                        118
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  118
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               119
      FPOS_Z=590.2                                                        119
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 119
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              120
      FPOS_AY=0                                                           120
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               120
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    121
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           121
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   121
C *                                                                       121
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    121
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           121
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   121
C *                                                                       122
      CALL AGSBEGM(MODULE,IPRIN)                                          122
      IF (IPRIN.LT.0) RETURN                                              122
C Filling bank FBXD - FPD BOX GEOMETRY                                    122
      BANK_TITLE='FPD BOX GEOMETRY'                                       122
      BPATH_FBXD='FBXD*'                                                  122
      IDNUM_FBXD(1)=0                                                     123
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       123
      FBXD_TYPE=1                                                         123
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                      123
C BOX HEIGHT                                                              124
      FBXD_HEIGHT=100                                                     124
      CFBXD_HEIGHT='BOX HEIGHT'                                           124
C BOX DEPTH                                                               125
      FBXD_DEPTH=96                                                       125
      CFBXD_DEPTH='BOX DEPTH'                                             125
C NUMBER OF PBG IN X                                                      126
      FBXD_NX=7                                                           126
      CFBXD_NX='NUMBER OF PBG IN X'                                       126
C NUMBER OF PBG IN Y                                                      127
      FBXD_NY=7                                                           127
      CFBXD_NY='NUMBER OF PBG IN Y'                                       127
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                128
      FBXD_XOFFSET=2.54                                                   128
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            128
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                129
      FBXD_ZOFFSET=19                                                     129
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            129
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            130
      FBXD_PSOFFSET=2.0                                                   130
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       130
C SMD V-PLANE Z OFFSET FROM BOX EDGE                                      131
      FBXD_SMDOFFSET=8.0                                                  131
      CFBXD_SMDOFFSET='SMD V-PLANE Z OFFSET FROM BOX EDGE'                131
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    132
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           132
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   132
C *                                                                       132
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    132
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           132
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   132
C *                                                                       133
      CALL AGSBEGM(MODULE,IPRIN)                                          133
      IF (IPRIN.LT.0) RETURN                                              133
C Filling bank FBXD - FPD BOX GEOMETRY                                    133
      BANK_TITLE='FPD BOX GEOMETRY'                                       133
      BPATH_FBXD='FBXD*'                                                  133
      IDNUM_FBXD(1)=0                                                     134
C TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)                                       134
      FBXD_TYPE=2                                                         134
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=5*5)'                      134
C BOX HEIGHT                                                              135
      FBXD_HEIGHT=20                                                      135
      CFBXD_HEIGHT='BOX HEIGHT'                                           135
C BOX DEPTH                                                               136
      FBXD_DEPTH=65                                                       136
      CFBXD_DEPTH='BOX DEPTH'                                             136
C NUMBER OF PBG IN X                                                      137
      FBXD_NX=5                                                           137
      CFBXD_NX='NUMBER OF PBG IN X'                                       137
C NUMBER OF PBG IN Y                                                      138
      FBXD_NY=5                                                           138
      CFBXD_NY='NUMBER OF PBG IN Y'                                       138
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                139
      FBXD_XOFFSET=0.0                                                    139
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            139
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                140
      FBXD_ZOFFSET=1                                                      140
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            140
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            141
      FBXD_PSOFFSET=0                                                     141
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       141
C SMD Z OFFSET FROM BOX EDG                                               142
      FBXD_SMDOFFSET=0.0                                                  142
      CFBXD_SMDOFFSET='SMD Z OFFSET FROM BOX EDG'                         142
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    143
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           143
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   143
C *                                                                       143
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    143
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           143
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   143
C *                                                                       144
      CALL AGSBEGM(MODULE,IPRIN)                                          144
      IF (IPRIN.LT.0) RETURN                                              144
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               144
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  144
      BPATH_FLGG='FLGG*'                                                  144
      IDNUM_FLGG(1)=0                                                     145
C PBG WIDTH                                                               145
      FLGG_WIDTH=3.81                                                     145
      CFLGG_WIDTH='PBG WIDTH'                                             145
C PBG DEPTH                                                               146
      FLGG_DEPTH=45.0                                                     146
      CFLGG_DEPTH='PBG DEPTH'                                             146
C GAP BETWEEN PBG                                                         147
      FLGG_DGAP=0.01                                                      147
      CFLGG_DGAP='GAP BETWEEN PBG'                                        147
C ALMUNIM WRAP THICKNESS (REAL)                                           148
      FLGG_ALTHICK=0.001                                                  148
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       148
C PHOTO CATHODE THICKNESS                                                 149
      FLGG_PHCATHDZ=2.0                                                   149
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            149
C PHOTO CATHODE RADIUS (REAL)                                             150
      FLGG_PHCATHR=1.8                                                    150
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         150
C MU METAL LENGTH                                                         151
      FLGG_MUMETDZ=11.0                                                   151
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     151
C MU METAL OUTER RADIUS (REAL)                                            152
      FLGG_MUMETR=1.9                                                     152
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         152
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    153
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           153
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   153
C *                                                                       153
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    153
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           153
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   153
C *                                                                       154
      CALL AGSBEGM(MODULE,IPRIN)                                          154
      IF (IPRIN.LT.0) RETURN                                              154
C Filling bank FLGM - PBG DETECTOR MATERIALS                              154
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 154
      BPATH_FLGM='FLGM*'                                                  154
      IDNUM_FLGM(1)=0                                                     155
C GDENSITY [/CM^3]                                                        155
      FLGM_DENSITY=3.86                                                   155
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    155
C RADIATION LENGTH [CM]                                                   156
      FLGM_RADLEN=2.5                                                     156
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                156
C PBO CONTENT [%]                                                         157
      FLGM_PBCONT=65.4                                                    157
      CFLGM_PBCONT='PBO CONTENT [%]'                                      157
C CRITICAL ENERGY [GEV]                                                   158
      FLGM_CRITENE=0.0158                                                 158
      CFLGM_CRITENE='CRITICAL ENERGY [GEV]'                               158
C MOLIERE RADIUS [CM]                                                     159
      FLGM_MOLIERER=3.32                                                  159
      CFLGM_MOLIERER='MOLIERE RADIUS [CM]'                                159
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    160
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           160
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   160
C *                                                                       160
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    160
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           160
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   160
C *                                                                       161
      CALL AGSBEGM(MODULE,IPRIN)                                          161
      IF (IPRIN.LT.0) RETURN                                              161
C Filling bank PBPD - PB PLATE DIMENSIONS                                 161
      BANK_TITLE='PB PLATE DIMENSIONS'                                    161
      BPATH_PBPD='PBPD*'                                                  161
      IDNUM_PBPD(1)=0                                                     162
C WIDTH                                                                   162
      PBPD_WIDTH=33.02                                                    162
      CPBPD_WIDTH='WIDTH'                                                 162
C HEIGHT                                                                  163
      PBPD_HEIGHT=33.02                                                   163
      CPBPD_HEIGHT='HEIGHT'                                               163
C THICKNESS                                                               164
      PBPD_THICK=1.27                                                     164
      CPBPD_THICK='THICKNESS'                                             164
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    165
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           165
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   165
C *                                                                       165
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    165
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           165
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   165
C *                                                                       166
      CALL AGSBEGM(MODULE,IPRIN)                                          166
      IF (IPRIN.LT.0) RETURN                                              166
C Filling bank FMXG - SMD GEOMETRY                                        166
      BANK_TITLE='SMD GEOMETRY'                                           166
      BPATH_FMXG='FMXG*'                                                  166
      IDNUM_FMXG(1)=0                                                     167
C GEOMETRY VERSION                                                        167
      FMXG_VERSION=2                                                      167
      CFMXG_VERSION='GEOMETRY VERSION'                                    167
C SCINTILLATOR STRIP APEX                                                 168
      FMXG_SAPEX=0.7                                                      168
      CFMXG_SAPEX='SCINTILLATOR STRIP APEX'                               168
C SCINTILLATOR STRIP BASE                                                 169
      FMXG_SBASE=1.0                                                      169
      CFMXG_SBASE='SCINTILLATOR STRIP BASE'                               169
C GAP BETWEEN STRIPS                                                      170
      FMXG_SGAP=0.0064                                                    170
      CFMXG_SGAP='GAP BETWEEN STRIPS'                                     170
C # OF STRIPS                                                             171
      FMXG_NSTRIP=50                                                      171
      CFMXG_NSTRIP='# OF STRIPS'                                          171
C G10 PLATE WIDTH                                                         172
      FMXG_G10WIDTH=27.0                                                  172
      CFMXG_G10WIDTH='G10 PLATE WIDTH'                                    172
C G10 PLATE HEIGHT                                                        173
      FMXG_G10HEIGHT=27.0                                                 173
      CFMXG_G10HEIGHT='G10 PLATE HEIGHT'                                  173
C G10 PLATE THICKNESS                                                     174
      FMXG_G10THICK=0.15                                                  174
      CFMXG_G10THICK='G10 PLATE THICKNESS'                                174
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    175
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           175
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   175
C *                                                                       175
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    175
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           175
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   175
C use default FMCG bank                                                   176
      BPATH_FMCG='FMCG*'                                                  176
      IDNUM_FMCG(1)=0                                                     176
      CALL AGDGETP('FPDMGEO1','*',-999,'FMCG',' ',BANK_FMCG, BPATH_FMCG,  176
     *IDNUM_FMCG,1,MAP_FMCG,NAMES_FMCG,BANK_FMCG,LEN_FMCG,LLEN_FMCG,      176
     *LINK_FMCG,FLAG_FMCG)                                                176
C Check IPRIN>=1                                                          176
      IF (IPRIN.GE.1) WRITE(LOUT,5010)FMCG_VERSION                        176
5010  FORMAT('****************** NEW FPDMGEO version ', F4.2)             178
      WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2                       179
      ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0               180
C *                                                                       181
      DO 5021 M=1,8                                                       181
C    use FPOS bank                                                        182
         BPATH_FPOS='FPOS*'                                               182
         IDNUM_FPOS(1)=0                                                  182
         FPOS_IMOD=M                                                      182
         CALL AGDGETP('FPDMGEO1','*',-999,'FPOS',NFPOS_IMOD,              182
     *   RE_FPOS_IMOD,BPATH_FPOS,IDNUM_FPOS,1,MAP_FPOS,NAMES_FPOS,        182
     *   BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                182
C    use FBXD bank                                                        183
         BPATH_FBXD='FBXD*'                                               183
         IDNUM_FBXD(1)=0                                                  183
         FBXD_TYPE=FPOS_ITYPE                                             183
         CALL AGDGETP('FPDMGEO1','*',-999,'FBXD',NFBXD_TYPE,              183
     *   RE_FBXD_TYPE,BPATH_FBXD,IDNUM_FBXD,1,MAP_FBXD,NAMES_FBXD,        183
     *   BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                183
         RTOT = FBXD_NX*WID/2.0                                           184
         BWID = RTOT+FBXD_XOFFSET                                         185
C    Check FPOS_X.GT.0.0                                                  186
         IF (FPOS_X.GT.0.0) THEN                                          186
         XX=FPOS_X+BWID                                                   187
         ELSEIF (FPOS_X.EQ.0.0) THEN                                      188
         XX=0.0                                                           189
         ELSE                                                             190
         XX=FPOS_X-BWID                                                   191
         ENDIF                                                            192
C    Check FPOS_Y.GT.0.0                                                  193
         IF (FPOS_Y.GT.0.0) THEN                                          193
         YY=FPOS_Y+FBXD_HEIGHT/2.0                                        194
         ELSEIF (FPOS_Y.EQ.0.0) THEN                                      195
         YY=0.0                                                           196
         ELSE                                                             197
         YY=FPOS_Y-FBXD_HEIGHT/2.0                                        198
         ENDIF                                                            199
C    Check FPOS_Z.GT.0.0                                                  200
         IF (FPOS_Z.GT.0.0) THEN                                          200
         ZZ=FPOS_Z+FBXD_DEPTH/2.0                                         201
         ELSE                                                             202
         ZZ=FPOS_Z-FBXD_DEPTH/2.0                                         203
         ENDIF                                                            204
         CALL AGSBEGM(MODULE,IPRIN)                                       205
C    Check IPRIN<0                                                        205
         IF (IPRIN.LT.0) RETURN                                           205
C    Create FBOX :                                                        205
C    Call To Local Procedure FBOX                                         205
         ASSIGN 5040 TO BLOCK_FBOX                                        205
         GO TO  5030                                                      205
5040     CONTINUE                                                         205
C    *                                                                    205
C    Geant POSITION : FBOX ===> MOTHER='CAVE' X=XX Y=YY Z=ZZ ALPHAY=FPOS  206
C    _AY <===                                                             206
         CALL AGSRESET                                                    206
         AG_MOTHER = 'CAVE'                                               206
         AG_X = XX                                                        206
         AG_Y = YY                                                        206
         AG_Z = ZZ                                                        206
         AG_ALPHAN=AG_ALPHAN+1                                            206
         AG_ALPHAK(AG_ALPHAN)=2                                           206
         AG_ALPHAL(AG_ALPHAN)=FPOS_AY                                     206
         AG_TITLE = 'FBOX'                                                206
         AG_EXNAME = 'POSITION'                                           206
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                               206
         CALL AXPOSITION                                                  206
C    *                                                                    206
5021  CONTINUE                                                            207
5022  CONTINUE                                                            207
C ----------------------------------------------------------------------  208
C ------------ FBOX                                                       208
      CALL AGSENDM                                                        209
      RETURN                                                              209
C Begin Local Procedure FBOX                                              209
C FBOX IS ONE PB-GLASS FPD DETECTOR                                       209
5030  CONTINUE                                                            209
         AG_TITLE='FBOX'                                                  209
         AG_MODULE=MODULE                                                 209
         AG_MARK='V00'                                                    209
         CALL AGSPUSH                                                     209
         AG_MODULE=MODULE                                                 209
C    *                                                                    209
C    Geant MATERIAL : AIR ===> inherited arguments used <===              210
         CALL AGSRESET                                                    210
         AG_TITLE = 'AIR'                                                 210
         AG_EXNAME = 'MATERIAL'                                           210
         AG_PARLIST = 'NONE'                                              210
         CALL AXMATERIAL                                                  210
C    *                                                                    210
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           210
         CALL AGSRESET                                                    211
         AG_TITLE = 'STANDARD'                                            211
         AG_EXNAME = 'MEDIUM'                                             211
         AG_PARLIST = 'NONE'                                              211
         CALL AXMEDIUM                                                    211
C    *                                                                    211
C    Geant ATTRIBUTE : FBOX ===> SEEN=1 COLO=2 <===                       211
         CALL AGSRESET                                                    212
         AG_SEEN = 1                                                      212
         AG_COLO = 2                                                      212
         AG_TITLE = 'FBOX'                                                212
         AG_EXNAME = 'ATTRIBUTE'                                          212
         AG_PARLIST = 'SEEN_COLO'                                         212
         CALL AXATTRIBUTE                                                 212
C    *                                                                    212
C    Geant SHAPE : BOX ===> DX=BWID DY=FBXD_HEIGHT/2 DZ=FBXD_DEPTH/2 <==  212
C    =                                                                    212
         CALL AGSRESET                                                    213
         AG_DX = BWID                                                     213
         AG_DY = FBXD_HEIGHT/2                                            213
         AG_DZ = FBXD_DEPTH/2                                             213
         AG_TITLE = 'BOX'                                                 213
         AG_EXNAME = 'SHAPE'                                              213
         AG_PARLIST = 'DX_DY_DZ'                                          213
         CALL AXSHAPE                                                     213
C    *                                                                    213
         X0 = BWID - FBXD_XOFFSET - WID/2                                 213
         Y1 = RTOT - WID/2                                                215
         Z1 = - FBXD_DEPTH/2 + FBXD_ZOFFSET + ZTOT                        216
C    *                                                                    217
         I=1                                                              217
            GO TO 5053                                                    217
5051        I=I+1                                                         217
5053        IF(I-(FBXD_NY).GT.0) GO TO 5052                               217
            X1=X0                                                         218
C       *                                                                 219
            J=1                                                           219
               GO TO 5063                                                 219
5061           J=J+1                                                      219
5063           IF(J-(FBXD_NX).GT.0) GO TO 5062                            219
C          Create FLGT :                                                  220
C          Call To Local Procedure FLGT                                   220
               ASSIGN 5080 TO BLOCK_FLGT                                  220
               GO TO  5070                                                220
5080           CONTINUE                                                   220
C          *                                                              220
C          Geant POSITION : FLGT ===> X=X1 Y=Y1 Z=Z1 <===                 221
               CALL AGSRESET                                              221
               AG_X = X1                                                  221
               AG_Y = Y1                                                  221
               AG_Z = Z1                                                  221
               AG_TITLE = 'FLGT'                                          221
               AG_EXNAME = 'POSITION'                                     221
               AG_PARLIST = 'X_Y_Z'                                       221
               CALL AXPOSITION                                            221
C          *                                                              221
               X1=X1-WID                                                  221
            GO TO 5061                                                    223
5062        CONTINUE                                                      223
            Y1=Y1-WID                                                     224
         GO TO 5051                                                       225
5052     CONTINUE                                                         225
C    Check FBXD_TYPE.EQ.1                                                 226
         IF (FBXD_TYPE.EQ.1) THEN                                         226
         X1=X0                                                            227
         Y1= -RTOT + ZTOT                                                 228
         Z1=-FBXD_DEPTH/2 + FBXD_PSOFFSET + WID/2                         229
C    *                                                                    230
         J=1                                                              230
            GO TO 5093                                                    230
5091        J=J+1                                                         230
5093        IF(J-(FBXD_NX).GT.0) GO TO 5092                               230
C       Create FLGT :                                                     231
C       Call To Local Procedure FLGT                                      231
            ASSIGN 5100 TO BLOCK_FLGT                                     231
            GO TO  5070                                                   231
5100        CONTINUE                                                      231
C       *                                                                 231
C       Geant POSITION : FLGT ===> X=X1 Y=Y1 Z=Z1 ALPHAX=90 <===          232
            CALL AGSRESET                                                 232
            AG_X = X1                                                     232
            AG_Y = Y1                                                     232
            AG_Z = Z1                                                     232
            AG_ALPHAN=AG_ALPHAN+1                                         232
            AG_ALPHAK(AG_ALPHAN)=1                                        232
            AG_ALPHAL(AG_ALPHAN)=90                                       232
            AG_TITLE = 'FLGT'                                             232
            AG_EXNAME = 'POSITION'                                        232
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   232
            CALL AXPOSITION                                               232
C       *                                                                 232
            X1=X1-WID                                                     232
         GO TO 5091                                                       234
5092     CONTINUE                                                         234
C    Create PBPT :                                                        235
C    Call To Local Procedure PBPT                                         235
         ASSIGN 5120 TO BLOCK_PBPT                                        235
         GO TO  5110                                                      235
5120     CONTINUE                                                         235
C    *                                                                    235
C    Geant POSITION : PBPT ===> X=0 Y=0 Z=PBPD_THICK/2.0-FBXD_DEPTH/2 <=  236
C    ==                                                                   236
         CALL AGSRESET                                                    236
         AG_X = 0                                                         236
         AG_Y = 0                                                         236
         AG_Z = PBPD_THICK/2.0-FBXD_DEPTH/2                               236
         AG_TITLE = 'PBPT'                                                236
         AG_EXNAME = 'POSITION'                                           236
         AG_PARLIST = 'X_Y_Z'                                             236
         CALL AXPOSITION                                                  236
C    *                                                                    236
         ZTOTSMD=FMXG_G10THICK+FMXG_SAPEX                                 236
C    Create FSHM :                                                        238
C    Call To Local Procedure FSHM                                         238
         ASSIGN 5140 TO BLOCK_FSHM                                        238
         GO TO  5130                                                      238
5140     CONTINUE                                                         238
C    *                                                                    238
C    Geant POSITION : FSHM ===> X=0 Y=0 Z=FBXD_SMDOFFSET+ZTOTSMD-FBXD_DE  239
C    PTH/2 <===                                                           239
         CALL AGSRESET                                                    239
         AG_X = 0                                                         239
         AG_Y = 0                                                         239
         AG_Z = FBXD_SMDOFFSET+ZTOTSMD-FBXD_DEPTH/2                       239
         AG_TITLE = 'FSHM'                                                239
         AG_EXNAME = 'POSITION'                                           239
         AG_PARLIST = 'X_Y_Z'                                             239
         CALL AXPOSITION                                                  239
C    *                                                                    239
         ENDIF                                                            239
5031  CALL AGSPOPB                                                        241
      GO TO BLOCK_FBOX, (5030 ,5040)                                      241
C End of Local Procedure                                                  241
C ----------------------------------------------------------------------  242
C ------------ FLGT                                                       242
C Begin Local Procedure FLGT                                              243
C FLGT IS ONE PBG TOWER                                                   243
5070  CONTINUE                                                            243
         AG_TITLE='FLGT'                                                  243
         AG_MODULE=MODULE                                                 243
         AG_MARK='V00'                                                    243
         CALL AGSPUSH                                                     243
         AG_MODULE=MODULE                                                 243
C    *                                                                    243
C    Geant MATERIAL : AIR ===> inherited arguments used <===              244
         CALL AGSRESET                                                    244
         AG_TITLE = 'AIR'                                                 244
         AG_EXNAME = 'MATERIAL'                                           244
         AG_PARLIST = 'NONE'                                              244
         CALL AXMATERIAL                                                  244
C    *                                                                    244
C    Geant ATTRIBUTE : FLGT ===> SEEN=1 COLO=2 <===                       244
         CALL AGSRESET                                                    245
         AG_SEEN = 1                                                      245
         AG_COLO = 2                                                      245
         AG_TITLE = 'FLGT'                                                245
         AG_EXNAME = 'ATTRIBUTE'                                          245
         AG_PARLIST = 'SEEN_COLO'                                         245
         CALL AXATTRIBUTE                                                 245
C    *                                                                    245
C    Geant SHAPE : BOX ===> DX=WID/2 DY=WID/2 DZ=ZTOT <===                245
         CALL AGSRESET                                                    246
         AG_DX = WID/2                                                    246
         AG_DY = WID/2                                                    246
         AG_DZ = ZTOT                                                     246
         AG_TITLE = 'BOX'                                                 246
         AG_EXNAME = 'SHAPE'                                              246
         AG_PARLIST = 'DX_DY_DZ'                                          246
         CALL AXSHAPE                                                     246
C    *                                                                    246
C    Create FWAL :                                                        247
C    Call To Local Procedure FWAL                                         247
         ASSIGN 5160 TO BLOCK_FWAL                                        247
         GO TO  5150                                                      247
5160     CONTINUE                                                         247
C    *                                                                    247
C    Geant POSITION : FWAL ===> Z=-ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0 <=  248
C    ==                                                                   248
         CALL AGSRESET                                                    248
         AG_Z = -ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0                       248
         AG_TITLE = 'FWAL'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'Z'                                                 248
         CALL AXPOSITION                                                  248
C    *                                                                    248
C    Create FUMT :                                                        249
C    Call To Local Procedure FUMT                                         249
         ASSIGN 5180 TO BLOCK_FUMT                                        249
         GO TO  5170                                                      249
5180     CONTINUE                                                         249
C    *                                                                    249
C    Geant POSITION : FUMT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUM  250
C    ETDZ/2.0 <===                                                        250
         CALL AGSRESET                                                    250
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUMETDZ/2.0            250
         AG_TITLE = 'FUMT'                                                250
         AG_EXNAME = 'POSITION'                                           250
         AG_PARLIST = 'Z'                                                 250
         CALL AXPOSITION                                                  250
C    *                                                                    250
C    Create FPCT :                                                        251
C    Call To Local Procedure FPCT                                         251
         ASSIGN 5200 TO BLOCK_FPCT                                        251
         GO TO  5190                                                      251
5200     CONTINUE                                                         251
C    *                                                                    251
C    Geant POSITION : FPCT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHC  252
C    ATHDZ/2.0 <===                                                       252
         CALL AGSRESET                                                    252
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHCATHDZ/2.0           252
         AG_TITLE = 'FPCT'                                                252
         AG_EXNAME = 'POSITION'                                           252
         AG_PARLIST = 'Z'                                                 252
         CALL AXPOSITION                                                  252
C    *                                                                    252
5071  CALL AGSPOPB                                                        253
      GO TO BLOCK_FLGT, (5070 ,5080,5100)                                 253
C End of Local Procedure                                                  253
C ----------------------------------------------------------------------  254
C ------------ FWAL                                                       254
C Begin Local Procedure FWAL                                              255
C FWAL IS ALMUNUM WRAPPER                                                 255
5150  CONTINUE                                                            255
         AG_TITLE='FWAL'                                                  255
         AG_MODULE=MODULE                                                 255
         AG_MARK='V00'                                                    255
         CALL AGSPUSH                                                     255
         AG_MODULE=MODULE                                                 255
C    *                                                                    255
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        256
         CALL AGSRESET                                                    256
         AG_TITLE = 'ALUMINIUM'                                           256
         AG_EXNAME = 'MATERIAL'                                           256
         AG_PARLIST = 'NONE'                                              256
         CALL AXMATERIAL                                                  256
C    *                                                                    256
C    Geant ATTRIBUTE : FWAL ===> SEEN=1 COLO=3 <===                       256
         CALL AGSRESET                                                    257
         AG_SEEN = 1                                                      257
         AG_COLO = 3                                                      257
         AG_TITLE = 'FWAL'                                                257
         AG_EXNAME = 'ATTRIBUTE'                                          257
         AG_PARLIST = 'SEEN_COLO'                                         257
         CALL AXATTRIBUTE                                                 257
C    *                                                                    257
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0+FLGG_ALTHICK, DY=FLGG_WIDT  257
C    H/2.0+FLGG_ALTHICK DZ=FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0 <===           257
         CALL AGSRESET                                                    259
         AG_DX = FLGG_WIDTH/2.0+FLGG_ALTHICK                              259
         AG_DY = FLGG_WIDTH/2.0+FLGG_ALTHICK                              259
         AG_DZ = FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0                          259
         AG_TITLE = 'BOX'                                                 259
         AG_EXNAME = 'SHAPE'                                              259
         AG_PARLIST = 'DX_DY_DZ'                                          259
         CALL AXSHAPE                                                     259
C    *                                                                    259
C    Check FMCG_CHKVSIM==1                                                259
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      260
         CALL GSCKOV( AG_IMED,N,E,ABSCO_ALM,EFFIC_ALL,RINDEX_ALM )        261
         ENDIF                                                            262
C    Create FLGR :                                                        263
C    Call To Local Procedure FLGR                                         263
         ASSIGN 5220 TO BLOCK_FLGR                                        263
         GO TO  5210                                                      263
5220     CONTINUE                                                         263
C    *                                                                    263
C    Geant POSITION : FLGR ===> Z=+FLGG_ALTHICK/2.0 <===                  264
         CALL AGSRESET                                                    264
         AG_Z = +FLGG_ALTHICK/2.0                                         264
         AG_TITLE = 'FLGR'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'Z'                                                 264
         CALL AXPOSITION                                                  264
C    *                                                                    264
5151  CALL AGSPOPB                                                        265
      GO TO BLOCK_FWAL, (5150 ,5160)                                      265
C End of Local Procedure                                                  265
C ----------------------------------------------------------------------  266
C ------------ FLGR                                                       266
C Begin Local Procedure FLGR                                              267
C FLGR IS LEAD GLASS DETECTOR                                             267
5210  CONTINUE                                                            267
         AG_TITLE='FLGR'                                                  267
         AG_MODULE=MODULE                                                 267
         AG_MARK='V00'                                                    267
         CALL AGSPUSH                                                     267
         AG_MODULE=MODULE                                                 267
C    *                                                                    267
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.60712 <===                268
         CALL AGSRESET                                                    268
         AG_A = 207.19                                                    268
         AG_Z = 82                                                        268
         AG_W = .60712                                                    268
         AG_TITLE = 'PB'                                                  268
         AG_EXNAME = 'COMPONENT'                                          268
         AG_PARLIST = 'A_Z_W'                                             268
         CALL AXCOMPONENT                                                 268
C    *                                                                    268
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.02324 <===                 268
         CALL AGSRESET                                                    269
         AG_A = 39.102                                                    269
         AG_Z = 19                                                        269
         AG_W = .02324                                                    269
         AG_TITLE = 'K'                                                   269
         AG_EXNAME = 'COMPONENT'                                          269
         AG_PARLIST = 'A_Z_W'                                             269
         CALL AXCOMPONENT                                                 269
C    *                                                                    269
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.14771 <===                269
         CALL AGSRESET                                                    270
         AG_A = 28.088                                                    270
         AG_Z = 14                                                        270
         AG_W = .14771                                                    270
         AG_TITLE = 'SI'                                                  270
         AG_EXNAME = 'COMPONENT'                                          270
         AG_PARLIST = 'A_Z_W'                                             270
         CALL AXCOMPONENT                                                 270
C    *                                                                    270
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.22041 <===                  270
         CALL AGSRESET                                                    271
         AG_A = 15.999                                                    271
         AG_Z = 8                                                         271
         AG_W = .22041                                                    271
         AG_TITLE = 'O'                                                   271
         AG_EXNAME = 'COMPONENT'                                          271
         AG_PARLIST = 'A_Z_W'                                             271
         CALL AXCOMPONENT                                                 271
C    *                                                                    271
C    Geant COMPONENT : AS ===> A=74.922 Z=33 W=.00152 <===                271
         CALL AGSRESET                                                    272
         AG_A = 74.922                                                    272
         AG_Z = 33                                                        272
         AG_W = .00152                                                    272
         AG_TITLE = 'AS'                                                  272
         AG_EXNAME = 'COMPONENT'                                          272
         AG_PARLIST = 'A_Z_W'                                             272
         CALL AXCOMPONENT                                                 272
C    *                                                                    272
C    Geant MIXTURE : PBG ===> DENS=FLGM_DENSITY <===                      272
         CALL AGSRESET                                                    273
         AG_DENS = FLGM_DENSITY                                           273
         AG_TITLE = 'PBG'                                                 273
         AG_EXNAME = 'MIXTURE'                                            273
         AG_PARLIST = 'DENS'                                              273
         CALL AXMIXTURE                                                   273
C    *                                                                    273
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           273
         CALL AGSRESET                                                    274
         AG_ISVOL = 1                                                     274
         AG_TITLE = 'LEADGLASS'                                           274
         AG_EXNAME = 'MEDIUM'                                             274
         AG_PARLIST = 'ISVOL'                                             274
         CALL AXMEDIUM                                                    274
C    *                                                                    274
C    Geant ATTRIBUTE : FLGR ===> SEEN=1 COLO=4 <===                       274
         CALL AGSRESET                                                    275
         AG_SEEN = 1                                                      275
         AG_COLO = 4                                                      275
         AG_TITLE = 'FLGR'                                                275
         AG_EXNAME = 'ATTRIBUTE'                                          275
         AG_PARLIST = 'SEEN_COLO'                                         275
         CALL AXATTRIBUTE                                                 275
C    *                                                                    275
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2 DY=FLGG_WIDTH/2, DZ=FLGG_DEP  275
C    TH/2 <===                                                            275
         CALL AGSRESET                                                    277
         AG_DX = FLGG_WIDTH/2                                             277
         AG_DY = FLGG_WIDTH/2                                             277
         AG_DZ = FLGG_DEPTH/2                                             277
         AG_TITLE = 'BOX'                                                 277
         AG_EXNAME = 'SHAPE'                                              277
         AG_PARLIST = 'DX_DY_DZ'                                          277
         CALL AXSHAPE                                                     277
C    *                                                                    277
C    Defining Hits: ELOS:0:(0,50)                                         277
         AG_TITLE = 'FLGR'                                                278
         AG_EXNAME = 'HITS'                                               278
         AG_TYPE=AG_MAGIC                                                 278
         CALL AGSDETV('FLGR')                                             278
C    *                                                                    278
         AG_PARLIST='ELOS'                                                278
         AG_TITLE = 'DETH'                                                278
         AG_NBITS = 0                                                     278
         AG_BIN=AG_MAGIC                                                  278
         AG_OPTION='*'                                                    278
         AG_FHMIN = 0                                                     278
         AG_FHMAX = 50                                                    278
         CALL AGSDETH (0)                                                 278
C    *                                                                    278
         CALL AGSDETU('FLGR')                                             278
C    *                                                                    278
C    Check FMCG_CHKVSIM==1                                                279
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      279
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PBG,EFFIC_ALL,RINDEX_PBG )        280
         ENDIF                                                            281
5211  CALL AGSPOPB                                                        282
      GO TO BLOCK_FLGR, (5210 ,5220)                                      282
C End of Local Procedure                                                  282
C ----------------------------------------------------------------------  283
C ------------ FPCT                                                       283
C Begin Local Procedure FPCT                                              284
C FPCT IS PHOTO CATHODE                                                   284
5190  CONTINUE                                                            284
         AG_TITLE='FPCT'                                                  284
         AG_MODULE=MODULE                                                 284
         AG_MARK='V00'                                                    284
         CALL AGSPUSH                                                     284
         AG_MODULE=MODULE                                                 284
C    *                                                                    284
C    Geant MATERIAL : AIR ===> inherited arguments used <===              285
         CALL AGSRESET                                                    285
         AG_TITLE = 'AIR'                                                 285
         AG_EXNAME = 'MATERIAL'                                           285
         AG_PARLIST = 'NONE'                                              285
         CALL AXMATERIAL                                                  285
C    *                                                                    285
C    Geant MEDIUM : PHOTCATH ===> ISVOL=1 <===                            285
         CALL AGSRESET                                                    286
         AG_ISVOL = 1                                                     286
         AG_TITLE = 'PHOTCATH'                                            286
         AG_EXNAME = 'MEDIUM'                                             286
         AG_PARLIST = 'ISVOL'                                             286
         CALL AXMEDIUM                                                    286
C    *                                                                    286
C    Geant ATTRIBUTE : LPCT ===> SEEN=1 COLO=6 <===                       286
         CALL AGSRESET                                                    287
         AG_SEEN = 1                                                      287
         AG_COLO = 6                                                      287
         AG_TITLE = 'LPCT'                                                287
         AG_EXNAME = 'ATTRIBUTE'                                          287
         AG_PARLIST = 'SEEN_COLO'                                         287
         CALL AXATTRIBUTE                                                 287
C    *                                                                    287
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=FLGG_PHCATHR DZ=FLGG_PHCATHDZ/2  287
C    .0 <===                                                              287
         CALL AGSRESET                                                    288
         AG_RMIN = 0                                                      288
         AG_RMAX = FLGG_PHCATHR                                           288
         AG_DZ = FLGG_PHCATHDZ/2.0                                        288
         AG_TITLE = 'TUBE'                                                288
         AG_EXNAME = 'SHAPE'                                              288
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      288
         CALL AXSHAPE                                                     288
C    *                                                                    288
C    Check FMCG_CHKVSIM==1                                                288
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      289
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PHCATH,EFFIC_PHCATH,              290
     *   RINDEX_PHCATH )                                                  290
C    Defining Hits: USER:0:(0,100000)                                     291
         AG_TITLE = 'FPCT'                                                291
         AG_EXNAME = 'HITS'                                               291
         AG_TYPE=AG_MAGIC                                                 291
         CALL AGSDETV('FPCT')                                             291
C    *                                                                    291
         AG_PARLIST='USER'                                                291
         AG_TITLE = 'DETH'                                                291
         AG_NBITS = 0                                                     291
         AG_BIN=AG_MAGIC                                                  291
         AG_OPTION='*'                                                    291
         AG_FHMIN = 0                                                     291
         AG_FHMAX = 100000                                                291
         CALL AGSDETH (JUMPAD(FPCTSTEP))                                  291
C    *                                                                    291
         CALL AGSDETU('FPCT')                                             291
C    *                                                                    291
         ENDIF                                                            292
5191  CALL AGSPOPB                                                        293
      GO TO BLOCK_FPCT, (5190 ,5200)                                      293
C End of Local Procedure                                                  293
C ----------------------------------------------------------------------  294
C ------------ FUMT                                                       294
C Begin Local Procedure FUMT                                              295
C FUMT IS MU METAL                                                        295
5170  CONTINUE                                                            295
         AG_TITLE='FUMT'                                                  295
         AG_MODULE=MODULE                                                 295
         AG_MARK='V00'                                                    295
         CALL AGSPUSH                                                     295
         AG_MODULE=MODULE                                                 295
C    *                                                                    295
C    Geant MATERIAL : IRON ===> inherited arguments used <===             296
         CALL AGSRESET                                                    296
         AG_TITLE = 'IRON'                                                296
         AG_EXNAME = 'MATERIAL'                                           296
         AG_PARLIST = 'NONE'                                              296
         CALL AXMATERIAL                                                  296
C    *                                                                    296
C    Geant ATTRIBUTE : LUMT ===> SEEN=1 COLO=5 <===                       296
         CALL AGSRESET                                                    297
         AG_SEEN = 1                                                      297
         AG_COLO = 5                                                      297
         AG_TITLE = 'LUMT'                                                297
         AG_EXNAME = 'ATTRIBUTE'                                          297
         AG_PARLIST = 'SEEN_COLO'                                         297
         CALL AXATTRIBUTE                                                 297
C    *                                                                    297
C    Geant SHAPE : TUBE ===> RMIN=FLGG_PHCATHR RMAX=FLGG_MUMETR, DZ=FLGG  297
C    _MUMETDZ/2.0 <===                                                    297
         CALL AGSRESET                                                    299
         AG_RMIN = FLGG_PHCATHR                                           299
         AG_RMAX = FLGG_MUMETR                                            299
         AG_DZ = FLGG_MUMETDZ/2.0                                         299
         AG_TITLE = 'TUBE'                                                299
         AG_EXNAME = 'SHAPE'                                              299
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      299
         CALL AXSHAPE                                                     299
C    *                                                                    299
C    Check FMCG_CHKVSIM==1                                                299
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      300
         CALL GSCKOV( AG_IMED,N,E,ABSCO_MUMET,EFFIC_ALL,RINDEX_MUMET )    301
         ENDIF                                                            302
5171  CALL AGSPOPB                                                        303
      GO TO BLOCK_FUMT, (5170 ,5180)                                      303
C End of Local Procedure                                                  303
C ----------------------------------------------------------------------  304
C ------------ PBPT                                                       304
C Begin Local Procedure PBPT                                              305
C PBPT IS PB PLATE                                                        305
5110  CONTINUE                                                            305
         AG_TITLE='PBPT'                                                  305
         AG_MODULE=MODULE                                                 305
         AG_MARK='V00'                                                    305
         CALL AGSPUSH                                                     305
         AG_MODULE=MODULE                                                 305
C    *                                                                    305
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             306
         CALL AGSRESET                                                    306
         AG_TITLE = 'LEAD'                                                306
         AG_EXNAME = 'MATERIAL'                                           306
         AG_PARLIST = 'NONE'                                              306
         CALL AXMATERIAL                                                  306
C    *                                                                    306
C    Geant ATTRIBUTE : PBPT ===> SEEN=1 COLO=7 <===                       306
         CALL AGSRESET                                                    307
         AG_SEEN = 1                                                      307
         AG_COLO = 7                                                      307
         AG_TITLE = 'PBPT'                                                307
         AG_EXNAME = 'ATTRIBUTE'                                          307
         AG_PARLIST = 'SEEN_COLO'                                         307
         CALL AXATTRIBUTE                                                 307
C    *                                                                    307
C    Geant SHAPE : BOX ===> DX=PBPD_WIDTH/2.0 DY=PBPD_HEIGHT/2.0, DZ=PBP  307
C    D_THICK/2.0 <===                                                     307
         CALL AGSRESET                                                    309
         AG_DX = PBPD_WIDTH/2.0                                           309
         AG_DY = PBPD_HEIGHT/2.0                                          309
         AG_DZ = PBPD_THICK/2.0                                           309
         AG_TITLE = 'BOX'                                                 309
         AG_EXNAME = 'SHAPE'                                              309
         AG_PARLIST = 'DX_DY_DZ'                                          309
         CALL AXSHAPE                                                     309
C    *                                                                    309
5111  CALL AGSPOPB                                                        310
      GO TO BLOCK_PBPT, (5110 ,5120)                                      310
C End of Local Procedure                                                  310
C ----------------------------------------------------------------------  311
C ------------ FSHM                                                       311
C Begin Local Procedure FSHM                                              312
C FSHM IS THE SHOWER MAX SECTION                                          312
5130  CONTINUE                                                            312
         AG_TITLE='FSHM'                                                  312
         AG_MODULE=MODULE                                                 312
         AG_MARK='V00'                                                    312
         CALL AGSPUSH                                                     312
         AG_MODULE=MODULE                                                 312
C    *                                                                    312
C    Geant MATERIAL : AIR ===> inherited arguments used <===              313
         CALL AGSRESET                                                    313
         AG_TITLE = 'AIR'                                                 313
         AG_EXNAME = 'MATERIAL'                                           313
         AG_PARLIST = 'NONE'                                              313
         CALL AXMATERIAL                                                  313
C    *                                                                    313
C    Geant ATTRIBUTE : FSHM ===> SEEN=1 COLO=4 <===                       313
         CALL AGSRESET                                                    314
         AG_SEEN = 1                                                      314
         AG_COLO = 4                                                      314
         AG_TITLE = 'FSHM'                                                314
         AG_EXNAME = 'ATTRIBUTE'                                          314
         AG_PARLIST = 'SEEN_COLO'                                         314
         CALL AXATTRIBUTE                                                 314
C    *                                                                    314
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2 DY=FMXG_G10HEIGHT/2, DZ=Z  314
C    TOTSMD <===                                                          314
         CALL AGSRESET                                                    316
         AG_DX = FMXG_G10WIDTH/2                                          316
         AG_DY = FMXG_G10HEIGHT/2                                         316
         AG_DZ = ZTOTSMD                                                  316
         AG_TITLE = 'BOX'                                                 316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'DX_DY_DZ'                                          316
         CALL AXSHAPE                                                     316
C    *                                                                    316
         WSMD=FMXG_SBASE/2 + FMXG_SGAP                                    316
         WTOTSMD=(FMXG_NSTRIP + 1)*WSMD                                   318
         ZZZ= - ZTOTSMD + FMXG_G10THICK/2                                 319
C    Create FXGT :                                                        320
C    Call To Local Procedure FXGT                                         320
         ASSIGN 5240 TO BLOCK_FXGT                                        320
         GO TO  5230                                                      320
5240     CONTINUE                                                         320
C    *                                                                    320
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZZZ <===                        321
         CALL AGSRESET                                                    321
         AG_X = 0                                                         321
         AG_Y = 0                                                         321
         AG_Z = ZZZ                                                       321
         AG_TITLE = 'FXGT'                                                321
         AG_EXNAME = 'POSITION'                                           321
         AG_PARLIST = 'X_Y_Z'                                             321
         CALL AXPOSITION                                                  321
C    *                                                                    321
         XXX= - WTOTSMD/2 - FMXG_SGAP/2 + WSMD                            321
         YYY=0.0                                                          323
         ZZZ=ZZZ + FMXG_G10THICK/2 + FMXG_SAPEX/2                         324
C    *                                                                    325
         I=1                                                              325
            GO TO 5253                                                    325
5251        I=I+1                                                         325
5253        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5252                           325
C       Check MOD(I,2)!=0                                                 326
            IF (MOD(I,2).NE.0) THEN                                       326
C       Create FHMS :                                                     327
C       Call To Local Procedure FHMS                                      327
            ASSIGN 5270 TO BLOCK_FHMS                                     327
            GO TO  5260                                                   327
5270        CONTINUE                                                      327
C       *                                                                 327
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ <===                 328
            CALL AGSRESET                                                 328
            AG_X = XXX                                                    328
            AG_Y = YYY                                                    328
            AG_Z = ZZZ                                                    328
            AG_TITLE = 'FHMS'                                             328
            AG_EXNAME = 'POSITION'                                        328
            AG_PARLIST = 'X_Y_Z'                                          328
            CALL AXPOSITION                                               328
C       *                                                                 328
            ELSE                                                          328
C       Create FHMS :                                                     330
C       Call To Local Procedure FHMS                                      330
            ASSIGN 5280 TO BLOCK_FHMS                                     330
            GO TO  5260                                                   330
5280        CONTINUE                                                      330
C       *                                                                 330
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ ALPHAX=180 <===      331
            CALL AGSRESET                                                 331
            AG_X = XXX                                                    331
            AG_Y = YYY                                                    331
            AG_Z = ZZZ                                                    331
            AG_ALPHAN=AG_ALPHAN+1                                         331
            AG_ALPHAK(AG_ALPHAN)=1                                        331
            AG_ALPHAL(AG_ALPHAN)=180                                      331
            AG_TITLE = 'FHMS'                                             331
            AG_EXNAME = 'POSITION'                                        331
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   331
            CALL AXPOSITION                                               331
C       *                                                                 331
            ENDIF                                                         331
            XXX=XXX+WSMD                                                  333
         GO TO 5251                                                       334
5252     CONTINUE                                                         334
         ZZZ=ZZZ + FMXG_G10THICK/2 + FMXG_SAPEX/2                         335
C    Create FXGT :                                                        336
C    Call To Local Procedure FXGT                                         336
         ASSIGN 5290 TO BLOCK_FXGT                                        336
         GO TO  5230                                                      336
5290     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZZZ <===                        337
         CALL AGSRESET                                                    337
         AG_X = 0                                                         337
         AG_Y = 0                                                         337
         AG_Z = ZZZ                                                       337
         AG_TITLE = 'FXGT'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'X_Y_Z'                                             337
         CALL AXPOSITION                                                  337
C    *                                                                    337
         XXX=0.0                                                          337
         YYY= - WTOTSMD/2 - FMXG_SGAP/2 + WSMD                            339
         ZZZ=ZZZ + FMXG_G10THICK/2 + FMXG_SAPEX/2                         340
C    *                                                                    341
         I=1                                                              341
            GO TO 5303                                                    341
5301        I=I+1                                                         341
5303        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5302                           341
C       Check MOD(I,2)!=0                                                 342
            IF (MOD(I,2).NE.0) THEN                                       342
C       Create FHMS :                                                     343
C       Call To Local Procedure FHMS                                      343
            ASSIGN 5310 TO BLOCK_FHMS                                     343
            GO TO  5260                                                   343
5310        CONTINUE                                                      343
C       *                                                                 343
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ ALPHAZ=90 <===       344
            CALL AGSRESET                                                 344
            AG_X = XXX                                                    344
            AG_Y = YYY                                                    344
            AG_Z = ZZZ                                                    344
            AG_ALPHAN=AG_ALPHAN+1                                         344
            AG_ALPHAK(AG_ALPHAN)=3                                        344
            AG_ALPHAL(AG_ALPHAN)=90                                       344
            AG_TITLE = 'FHMS'                                             344
            AG_EXNAME = 'POSITION'                                        344
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   344
            CALL AXPOSITION                                               344
C       *                                                                 344
            ELSE                                                          344
C       Create FHMS :                                                     346
C       Call To Local Procedure FHMS                                      346
            ASSIGN 5320 TO BLOCK_FHMS                                     346
            GO TO  5260                                                   346
5320        CONTINUE                                                      346
C       *                                                                 346
C       Geant POSITION : FHMS ===> X=XXX Y=YYY Z=ZZZ ORT=YX-Z <===        347
            CALL AGSRESET                                                 347
            AG_X = XXX                                                    347
            AG_Y = YYY                                                    347
            AG_Z = ZZZ                                                    347
            AG_ALPHAN=AG_ALPHAN+1                                         347
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             347
            AG_TITLE = 'FHMS'                                             347
            AG_EXNAME = 'POSITION'                                        347
            AG_PARLIST = 'X_Y_Z_ORT'                                      347
            CALL AXPOSITION                                               347
C       *                                                                 347
            ENDIF                                                         347
            YYY=YYY+WSMD                                                  349
         GO TO 5301                                                       350
5302     CONTINUE                                                         350
5131  CALL AGSPOPB                                                        351
      GO TO BLOCK_FSHM, (5130 ,5140)                                      351
C End of Local Procedure                                                  351
C ----------------------------------------------------------------------  352
C ------------ FXGT                                                       352
C Begin Local Procedure FXGT                                              353
C FXGT IS THE G10 LAYER IN THE SMAX                                       353
5230  CONTINUE                                                            353
         AG_TITLE='FXGT'                                                  353
         AG_MODULE=MODULE                                                 353
         AG_MARK='V00'                                                    353
         CALL AGSPUSH                                                     353
         AG_MODULE=MODULE                                                 353
C    *                                                                    353
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          354
         CALL AGSRESET                                                    354
         AG_A = 28.08                                                     354
         AG_Z = 14                                                        354
         AG_W = 0.6*1*28./60.                                             354
         AG_TITLE = 'SI'                                                  354
         AG_EXNAME = 'COMPONENT'                                          354
         AG_PARLIST = 'A_Z_W'                                             354
         CALL AXCOMPONENT                                                 354
C    *                                                                    354
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               354
         CALL AGSRESET                                                    355
         AG_A = 16                                                        355
         AG_Z = 8                                                         355
         AG_W = 0.6*2*16./60.                                             355
         AG_TITLE = 'O'                                                   355
         AG_EXNAME = 'COMPONENT'                                          355
         AG_PARLIST = 'A_Z_W'                                             355
         CALL AXCOMPONENT                                                 355
C    *                                                                    355
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              355
         CALL AGSRESET                                                    356
         AG_A = 12                                                        356
         AG_Z = 6                                                         356
         AG_W = 0.4*8*12./174.                                            356
         AG_TITLE = 'C'                                                   356
         AG_EXNAME = 'COMPONENT'                                          356
         AG_PARLIST = 'A_Z_W'                                             356
         CALL AXCOMPONENT                                                 356
C    *                                                                    356
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               356
         CALL AGSRESET                                                    357
         AG_A = 1                                                         357
         AG_Z = 1                                                         357
         AG_W = 0.4*14*1./174.                                            357
         AG_TITLE = 'H'                                                   357
         AG_EXNAME = 'COMPONENT'                                          357
         AG_PARLIST = 'A_Z_W'                                             357
         CALL AXCOMPONENT                                                 357
C    *                                                                    357
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              357
         CALL AGSRESET                                                    358
         AG_A = 16                                                        358
         AG_Z = 8                                                         358
         AG_W = 0.4*4*16./174.                                            358
         AG_TITLE = 'O'                                                   358
         AG_EXNAME = 'COMPONENT'                                          358
         AG_PARLIST = 'A_Z_W'                                             358
         CALL AXCOMPONENT                                                 358
C    *                                                                    358
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               358
         CALL AGSRESET                                                    359
         AG_DENS = 1.7                                                    359
         AG_TITLE = 'G10'                                                 359
         AG_EXNAME = 'MIXTURE'                                            359
         AG_PARLIST = 'DENS'                                              359
         CALL AXMIXTURE                                                   359
C    *                                                                    359
C    Geant ATTRIBUTE : FXGT ===> SEEN=1 COLO=7 <===                       359
         CALL AGSRESET                                                    360
         AG_SEEN = 1                                                      360
         AG_COLO = 7                                                      360
         AG_TITLE = 'FXGT'                                                360
         AG_EXNAME = 'ATTRIBUTE'                                          360
         AG_PARLIST = 'SEEN_COLO'                                         360
         CALL AXATTRIBUTE                                                 360
C    *                                                                    360
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2 DY=FMXG_G10HEIGHT/2, DZ=F  360
C    MXG_G10THICK/2 <===                                                  360
         CALL AGSRESET                                                    362
         AG_DX = FMXG_G10WIDTH/2                                          362
         AG_DY = FMXG_G10HEIGHT/2                                         362
         AG_DZ = FMXG_G10THICK/2                                          362
         AG_TITLE = 'BOX'                                                 362
         AG_EXNAME = 'SHAPE'                                              362
         AG_PARLIST = 'DX_DY_DZ'                                          362
         CALL AXSHAPE                                                     362
C    *                                                                    362
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           362
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           364
5231  CALL AGSPOPB                                                        365
      GO TO BLOCK_FXGT, (5230 ,5240,5290)                                 365
C End of Local Procedure                                                  365
C ----------------------------------------------------------------------  366
C ------------ FHMS                                                       366
C Begin Local Procedure FHMS                                              367
C FHMS IS SHOWER MAX STRIP                                                367
5260  CONTINUE                                                            367
         AG_TITLE='FHMS'                                                  367
         AG_MODULE=MODULE                                                 367
         AG_MARK='V00'                                                    367
         CALL AGSPUSH                                                     367
         AG_MODULE=MODULE                                                 367
C    *                                                                    367
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       368
         CALL AGSRESET                                                    368
         AG_TITLE = 'POLYSTYREN'                                          368
         AG_EXNAME = 'MATERIAL'                                           368
         AG_PARLIST = 'NONE'                                              368
         CALL AXMATERIAL                                                  368
C    *                                                                    368
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       368
         CALL AGSRESET                                                    369
         AG_ISVOL = 1                                                     369
         AG_TITLE = 'CPOLYSTYREN'                                         369
         AG_EXNAME = 'MATERIAL'                                           369
         AG_PARLIST = 'ISVOL'                                             369
         CALL AXMATERIAL                                                  369
C    *                                                                    369
C    Geant ATTRIBUTE : FHMS ===> SEEN=1 COLO=2 <===                       369
         CALL AGSRESET                                                    370
         AG_SEEN = 1                                                      370
         AG_COLO = 2                                                      370
         AG_TITLE = 'FHMS'                                                370
         AG_EXNAME = 'ATTRIBUTE'                                          370
         AG_PARLIST = 'SEEN_COLO'                                         370
         CALL AXATTRIBUTE                                                 370
C    *                                                                    370
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=FMXG_SBASE/2 DY=FMXG_G10HEIGHT/2,  370
C     DZ=FMXG_SAPEX/2 <===                                                370
         CALL AGSRESET                                                    372
         AG_DX1 = 0                                                       372
         AG_DX2 = FMXG_SBASE/2                                            372
         AG_DY = FMXG_G10HEIGHT/2                                         372
         AG_DZ = FMXG_SAPEX/2                                             372
         AG_TITLE = 'TRD1'                                                372
         AG_EXNAME = 'SHAPE'                                              372
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     372
         CALL AXSHAPE                                                     372
C    *                                                                    372
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           372
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             374
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             375
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                376
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.0130)                            377
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            378
C    Defining Hits: BIRK:0:(0,10)                                         379
         AG_TITLE = 'FHMS'                                                379
         AG_EXNAME = 'HITS'                                               379
         AG_TYPE=AG_MAGIC                                                 379
         CALL AGSDETV('FHMS')                                             379
C    *                                                                    379
         AG_PARLIST='BIRK'                                                379
         AG_TITLE = 'DETH'                                                379
         AG_NBITS = 0                                                     379
         AG_BIN=AG_MAGIC                                                  379
         AG_OPTION='*'                                                    379
         AG_FHMIN = 0                                                     379
         AG_FHMAX = 10                                                    379
         CALL AGSDETH (0)                                                 379
C    *                                                                    379
         CALL AGSDETU('FHMS')                                             379
C    *                                                                    379
5261  CALL AGSPOPB                                                        380
      GO TO BLOCK_FHMS, (5260 ,5270,5280,5310,5320)                       380
C End of Local Procedure                                                  380
      WRITE(LOUT,5330)'===>>  [End AgML Module FPDMGEO1]  <<==='          380
5330  FORMAT(A32,/,/)                                                     380
C MODULE FPDMGEO1                                                         382
      END                                                                 382
