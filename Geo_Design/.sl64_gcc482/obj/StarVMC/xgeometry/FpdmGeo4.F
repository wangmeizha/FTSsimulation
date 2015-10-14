C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FPDMGEO4                                                 1
C FPDMGEO4 IS THE FORWARD PION DETECTOR MODULES GEOMETRY                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FPDMGEO4'/, BANK_TITLE/'*'/                            1
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
C CREATED 27 NOV 2006                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'27 NOV 2006'/                                2
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
      INTEGER BLOCK_FTOW                                                  13
      INTEGER BLOCK_FLXF                                                  13
      INTEGER BLOCK_FWAL                                                  13
      INTEGER BLOCK_FLGR                                                  13
      INTEGER BLOCK_FPCT                                                  13
      INTEGER BLOCK_FUMT                                                  13
      INTEGER BLOCK_PBPT                                                  13
      INTEGER BLOCK_FSHM                                                  13
      INTEGER BLOCK_FHMS                                                  13
      INTEGER BLOCK_FXGT                                                  13
      INTEGER BLOCK_FALU                                                  13
      INTEGER BLOCK_FBAS                                                  13
      INTEGER BLOCK_FENC                                                  13
      INTEGER BLOCK_FEAC                                                  13
      INTEGER BLOCK_FEBC                                                  13
      INTEGER BLOCK_FECC                                                  13
      INTEGER BLOCK_FEDC                                                  13
      INTEGER BLOCK_FETC                                                  13
      INTEGER BLOCK_FERC                                                  13
      INTEGER BLOCK_FESC                                                  13
      INTEGER BLOCK_FEEC                                                  13
      INTEGER BLOCK_FPRS                                                  13
      INTEGER BLOCK_FPLY                                                  13
      INTEGER BLOCK_FPPB                                                  13
      INTEGER BLOCK_FPSC                                                  13
      INTEGER BLOCK_FPBB                                                  13
      INTEGER BLOCK_FLIG                                                  13
      INTEGER BLOCK_FLBA                                                  13
      INTEGER BLOCK_FLCO                                                  13
      INTEGER BLOCK_FPMT                                                  13
      INTEGER BLOCK_FFEE                                                  13
      INTEGER BLOCK_FLLG                                                  13
      INTEGER BLOCK_FBLL                                                  13
      INTEGER BLOCK_FCLL                                                  13
      INTEGER BLOCK_WPPL                                                  13
      INTEGER BLOCK_WPIS                                                  13
      INTEGER BLOCK_WPUF                                                  13
      INTEGER BLOCK_WPUN                                                  13
      INTEGER BLOCK_WPUS                                                  13
      INTEGER BLOCK_WPAR                                                  13
C -                                                                       14
      REAL FMCG_VERSION                                                   14
      EQUIVALENCE (BANK_FMCG(3),FMCG_VERSION,RE_FMCG_VERSION),            14
     *(NAMES_FMCG(1),NFMCG_VERSION)                                       14
      EQUIVALENCE (COMMEN_FMCG(1),CFMCG_VERSION),(MAP_FMCG(1,1),          14
     *MFMCG_VERSION)                                                      14
      CHARACTER CFMCG_VERSION*40/' '/                                     14
      CHARACTER NFMCG_VERSION*12/'R.VERSION'/                             14
      INTEGER MFMCG_VERSION(2)/1,1/                                       14
      REAL RE_FMCG_VERSION                                                14
C -                                                                       14
      REAL FMCG_CHKVSIM                                                   14
      EQUIVALENCE (BANK_FMCG(4),FMCG_CHKVSIM,RE_FMCG_CHKVSIM),            14
     *(NAMES_FMCG(2),NFMCG_CHKVSIM)                                       14
      EQUIVALENCE (COMMEN_FMCG(2),CFMCG_CHKVSIM),(MAP_FMCG(1,2),          14
     *MFMCG_CHKVSIM)                                                      14
      CHARACTER CFMCG_CHKVSIM*40/' '/                                     14
      CHARACTER NFMCG_CHKVSIM*12/'R.CHKVSIM'/                             14
      INTEGER MFMCG_CHKVSIM(2)/1,1/                                       14
      REAL RE_FMCG_CHKVSIM                                                14
C -                                                                       14
      REAL FMCG_PBPLATE                                                   14
      EQUIVALENCE (BANK_FMCG(5),FMCG_PBPLATE,RE_FMCG_PBPLATE),            14
     *(NAMES_FMCG(3),NFMCG_PBPLATE)                                       14
      EQUIVALENCE (COMMEN_FMCG(3),CFMCG_PBPLATE),(MAP_FMCG(1,3),          14
     *MFMCG_PBPLATE)                                                      14
      CHARACTER CFMCG_PBPLATE*40/' '/                                     14
      CHARACTER NFMCG_PBPLATE*12/'R.PBPLATE'/                             14
      INTEGER MFMCG_PBPLATE(2)/1,1/                                       14
      REAL RE_FMCG_PBPLATE                                                14
C -                                                                       14
      REAL FMCG_FMSNORTHX                                                 14
      EQUIVALENCE (BANK_FMCG(6),FMCG_FMSNORTHX,RE_FMCG_FMSNORTHX),        14
     *(NAMES_FMCG(4),NFMCG_FMSNORTHX)                                     14
      EQUIVALENCE (COMMEN_FMCG(4),CFMCG_FMSNORTHX),(MAP_FMCG(1,4),        14
     *MFMCG_FMSNORTHX)                                                    14
      CHARACTER CFMCG_FMSNORTHX*40/' '/                                   14
      CHARACTER NFMCG_FMSNORTHX*12/'R.FMSNORTHX'/                         14
      INTEGER MFMCG_FMSNORTHX(2)/1,1/                                     14
      REAL RE_FMCG_FMSNORTHX                                              14
C -                                                                       14
      REAL FMCG_FMSSOUTHX                                                 14
      EQUIVALENCE (BANK_FMCG(7),FMCG_FMSSOUTHX,RE_FMCG_FMSSOUTHX),        14
     *(NAMES_FMCG(5),NFMCG_FMSSOUTHX)                                     14
      EQUIVALENCE (COMMEN_FMCG(5),CFMCG_FMSSOUTHX),(MAP_FMCG(1,5),        14
     *MFMCG_FMSSOUTHX)                                                    14
      CHARACTER CFMCG_FMSSOUTHX*40/' '/                                   14
      CHARACTER NFMCG_FMSSOUTHX*12/'R.FMSSOUTHX'/                         14
      INTEGER MFMCG_FMSSOUTHX(2)/1,1/                                     14
      REAL RE_FMCG_FMSSOUTHX                                              14
C -                                                                       14
      INTEGER LEN_FMCG,LLEN_FMCG,LINK_FMCG                                14
      PARAMETER (LEN_FMCG=7)                                              14
      PARAMETER (LLEN_FMCG=5)                                             14
      INTEGER FLAG_FMCG/0/,MAP_FMCG(2,LLEN_FMCG),IDNUM_FMCG(10)           14
      CHARACTER NAMES_FMCG(LLEN_FMCG)*12,COMMEN_FMCG(LLEN_FMCG)*40        14
      DATA IDNUM_FMCG/10*0/                                               14
      REAL BANK_FMCG(LEN_FMCG)                                            14
      CHARACTER*60 BPATH_FMCG/' '/                                        14
      SAVE FLAG_FMCG,MAP_FMCG,IDNUM_FMCG,NAMES_FMCG,COMMEN_FMCG,          14
     *BPATH_FMCG                                                          14
      DATA LINK_FMCG/0/,BANK_FMCG/LEN_FMCG*0/                             14
C *                                                                       14
C -                                                                       15
      REAL FPOS_IMOD                                                      15
      EQUIVALENCE (BANK_FPOS(3),FPOS_IMOD,RE_FPOS_IMOD),(NAMES_FPOS(1),   15
     *NFPOS_IMOD)                                                         15
      EQUIVALENCE (COMMEN_FPOS(1),CFPOS_IMOD),(MAP_FPOS(1,1),             15
     *MFPOS_IMOD)                                                         15
      CHARACTER CFPOS_IMOD*40/' '/                                        15
      CHARACTER NFPOS_IMOD*12/'R.IMOD'/                                   15
      INTEGER MFPOS_IMOD(2)/1,1/                                          15
      REAL RE_FPOS_IMOD                                                   15
C -                                                                       15
      REAL FPOS_ITYPE                                                     15
      EQUIVALENCE (BANK_FPOS(4),FPOS_ITYPE,RE_FPOS_ITYPE),                15
     *(NAMES_FPOS(2),NFPOS_ITYPE)                                         15
      EQUIVALENCE (COMMEN_FPOS(2),CFPOS_ITYPE),(MAP_FPOS(1,2),            15
     *MFPOS_ITYPE)                                                        15
      CHARACTER CFPOS_ITYPE*40/' '/                                       15
      CHARACTER NFPOS_ITYPE*12/'R.ITYPE'/                                 15
      INTEGER MFPOS_ITYPE(2)/1,1/                                         15
      REAL RE_FPOS_ITYPE                                                  15
C -                                                                       15
      REAL FPOS_X                                                         15
      EQUIVALENCE (BANK_FPOS(5),FPOS_X,RE_FPOS_X),(NAMES_FPOS(3),         15
     *NFPOS_X)                                                            15
      EQUIVALENCE (COMMEN_FPOS(3),CFPOS_X),(MAP_FPOS(1,3),MFPOS_X)        15
      CHARACTER CFPOS_X*40/' '/                                           15
      CHARACTER NFPOS_X*12/'R.X'/                                         15
      INTEGER MFPOS_X(2)/1,1/                                             15
      REAL RE_FPOS_X                                                      15
C -                                                                       15
      REAL FPOS_Y                                                         15
      EQUIVALENCE (BANK_FPOS(6),FPOS_Y,RE_FPOS_Y),(NAMES_FPOS(4),         15
     *NFPOS_Y)                                                            15
      EQUIVALENCE (COMMEN_FPOS(4),CFPOS_Y),(MAP_FPOS(1,4),MFPOS_Y)        15
      CHARACTER CFPOS_Y*40/' '/                                           15
      CHARACTER NFPOS_Y*12/'R.Y'/                                         15
      INTEGER MFPOS_Y(2)/1,1/                                             15
      REAL RE_FPOS_Y                                                      15
C -                                                                       15
      REAL FPOS_Z                                                         15
      EQUIVALENCE (BANK_FPOS(7),FPOS_Z,RE_FPOS_Z),(NAMES_FPOS(5),         15
     *NFPOS_Z)                                                            15
      EQUIVALENCE (COMMEN_FPOS(5),CFPOS_Z),(MAP_FPOS(1,5),MFPOS_Z)        15
      CHARACTER CFPOS_Z*40/' '/                                           15
      CHARACTER NFPOS_Z*12/'R.Z'/                                         15
      INTEGER MFPOS_Z(2)/1,1/                                             15
      REAL RE_FPOS_Z                                                      15
C -                                                                       15
      REAL FPOS_AY                                                        15
      EQUIVALENCE (BANK_FPOS(8),FPOS_AY,RE_FPOS_AY),(NAMES_FPOS(6),       15
     *NFPOS_AY)                                                           15
      EQUIVALENCE (COMMEN_FPOS(6),CFPOS_AY),(MAP_FPOS(1,6),MFPOS_AY)      15
      CHARACTER CFPOS_AY*40/' '/                                          15
      CHARACTER NFPOS_AY*12/'R.AY'/                                       15
      INTEGER MFPOS_AY(2)/1,1/                                            15
      REAL RE_FPOS_AY                                                     15
C -                                                                       15
      REAL FPOS_AZ                                                        15
      EQUIVALENCE (BANK_FPOS(9),FPOS_AZ,RE_FPOS_AZ),(NAMES_FPOS(7),       15
     *NFPOS_AZ)                                                           15
      EQUIVALENCE (COMMEN_FPOS(7),CFPOS_AZ),(MAP_FPOS(1,7),MFPOS_AZ)      15
      CHARACTER CFPOS_AZ*40/' '/                                          15
      CHARACTER NFPOS_AZ*12/'R.AZ'/                                       15
      INTEGER MFPOS_AZ(2)/1,1/                                            15
      REAL RE_FPOS_AZ                                                     15
C -                                                                       15
      INTEGER LEN_FPOS,LLEN_FPOS,LINK_FPOS                                15
      PARAMETER (LEN_FPOS=9)                                              15
      PARAMETER (LLEN_FPOS=7)                                             15
      INTEGER FLAG_FPOS/0/,MAP_FPOS(2,LLEN_FPOS),IDNUM_FPOS(10)           15
      CHARACTER NAMES_FPOS(LLEN_FPOS)*12,COMMEN_FPOS(LLEN_FPOS)*40        15
      DATA IDNUM_FPOS/10*0/                                               15
      REAL BANK_FPOS(LEN_FPOS)                                            15
      CHARACTER*60 BPATH_FPOS/' '/                                        15
      SAVE FLAG_FPOS,MAP_FPOS,IDNUM_FPOS,NAMES_FPOS,COMMEN_FPOS,          15
     *BPATH_FPOS                                                          15
      DATA LINK_FPOS/0/,BANK_FPOS/LEN_FPOS*0/                             15
C *                                                                       15
C -                                                                       17
      REAL FBXD_TYPE                                                      17
      EQUIVALENCE (BANK_FBXD(3),FBXD_TYPE,RE_FBXD_TYPE),(NAMES_FBXD(1),   17
     *NFBXD_TYPE)                                                         17
      EQUIVALENCE (COMMEN_FBXD(1),CFBXD_TYPE),(MAP_FBXD(1,1),             17
     *MFBXD_TYPE)                                                         17
      CHARACTER CFBXD_TYPE*40/' '/                                        17
      CHARACTER NFBXD_TYPE*12/'R.TYPE'/                                   17
      INTEGER MFBXD_TYPE(2)/1,1/                                          17
      REAL RE_FBXD_TYPE                                                   17
C -                                                                       17
      REAL FBXD_HEIGHT                                                    17
      EQUIVALENCE (BANK_FBXD(4),FBXD_HEIGHT,RE_FBXD_HEIGHT),              17
     *(NAMES_FBXD(2),NFBXD_HEIGHT)                                        17
      EQUIVALENCE (COMMEN_FBXD(2),CFBXD_HEIGHT),(MAP_FBXD(1,2),           17
     *MFBXD_HEIGHT)                                                       17
      CHARACTER CFBXD_HEIGHT*40/' '/                                      17
      CHARACTER NFBXD_HEIGHT*12/'R.HEIGHT'/                               17
      INTEGER MFBXD_HEIGHT(2)/1,1/                                        17
      REAL RE_FBXD_HEIGHT                                                 17
C -                                                                       17
      REAL FBXD_DEPTH                                                     17
      EQUIVALENCE (BANK_FBXD(5),FBXD_DEPTH,RE_FBXD_DEPTH),                17
     *(NAMES_FBXD(3),NFBXD_DEPTH)                                         17
      EQUIVALENCE (COMMEN_FBXD(3),CFBXD_DEPTH),(MAP_FBXD(1,3),            17
     *MFBXD_DEPTH)                                                        17
      CHARACTER CFBXD_DEPTH*40/' '/                                       17
      CHARACTER NFBXD_DEPTH*12/'R.DEPTH'/                                 17
      INTEGER MFBXD_DEPTH(2)/1,1/                                         17
      REAL RE_FBXD_DEPTH                                                  17
C -                                                                       17
      REAL FBXD_WIDTH                                                     17
      EQUIVALENCE (BANK_FBXD(6),FBXD_WIDTH,RE_FBXD_WIDTH),                17
     *(NAMES_FBXD(4),NFBXD_WIDTH)                                         17
      EQUIVALENCE (COMMEN_FBXD(4),CFBXD_WIDTH),(MAP_FBXD(1,4),            17
     *MFBXD_WIDTH)                                                        17
      CHARACTER CFBXD_WIDTH*40/' '/                                       17
      CHARACTER NFBXD_WIDTH*12/'R.WIDTH'/                                 17
      INTEGER MFBXD_WIDTH(2)/1,1/                                         17
      REAL RE_FBXD_WIDTH                                                  17
C -                                                                       17
      REAL FBXD_NX                                                        17
      EQUIVALENCE (BANK_FBXD(7),FBXD_NX,RE_FBXD_NX),(NAMES_FBXD(5),       17
     *NFBXD_NX)                                                           17
      EQUIVALENCE (COMMEN_FBXD(5),CFBXD_NX),(MAP_FBXD(1,5),MFBXD_NX)      17
      CHARACTER CFBXD_NX*40/' '/                                          17
      CHARACTER NFBXD_NX*12/'R.NX'/                                       17
      INTEGER MFBXD_NX(2)/1,1/                                            17
      REAL RE_FBXD_NX                                                     17
C -                                                                       17
      REAL FBXD_NY                                                        17
      EQUIVALENCE (BANK_FBXD(8),FBXD_NY,RE_FBXD_NY),(NAMES_FBXD(6),       17
     *NFBXD_NY)                                                           17
      EQUIVALENCE (COMMEN_FBXD(6),CFBXD_NY),(MAP_FBXD(1,6),MFBXD_NY)      17
      CHARACTER CFBXD_NY*40/' '/                                          17
      CHARACTER NFBXD_NY*12/'R.NY'/                                       17
      INTEGER MFBXD_NY(2)/1,1/                                            17
      REAL RE_FBXD_NY                                                     17
C -                                                                       17
      REAL FBXD_NXL                                                       17
      EQUIVALENCE (BANK_FBXD(9),FBXD_NXL,RE_FBXD_NXL),(NAMES_FBXD(7),     17
     *NFBXD_NXL)                                                          17
      EQUIVALENCE (COMMEN_FBXD(7),CFBXD_NXL),(MAP_FBXD(1,7),MFBXD_NXL)    17
      CHARACTER CFBXD_NXL*40/' '/                                         17
      CHARACTER NFBXD_NXL*12/'R.NXL'/                                     17
      INTEGER MFBXD_NXL(2)/1,1/                                           17
      REAL RE_FBXD_NXL                                                    17
C -                                                                       17
      REAL FBXD_NYL                                                       17
      EQUIVALENCE (BANK_FBXD(10),FBXD_NYL,RE_FBXD_NYL),(NAMES_FBXD(8),    17
     *NFBXD_NYL)                                                          17
      EQUIVALENCE (COMMEN_FBXD(8),CFBXD_NYL),(MAP_FBXD(1,8),MFBXD_NYL)    17
      CHARACTER CFBXD_NYL*40/' '/                                         17
      CHARACTER NFBXD_NYL*12/'R.NYL'/                                     17
      INTEGER MFBXD_NYL(2)/1,1/                                           17
      REAL RE_FBXD_NYL                                                    17
C -                                                                       17
      REAL FBXD_XOFFSET                                                   17
      EQUIVALENCE (BANK_FBXD(11),FBXD_XOFFSET,RE_FBXD_XOFFSET),           17
     *(NAMES_FBXD(9),NFBXD_XOFFSET)                                       17
      EQUIVALENCE (COMMEN_FBXD(9),CFBXD_XOFFSET),(MAP_FBXD(1,9),          17
     *MFBXD_XOFFSET)                                                      17
      CHARACTER CFBXD_XOFFSET*40/' '/                                     17
      CHARACTER NFBXD_XOFFSET*12/'R.XOFFSET'/                             17
      INTEGER MFBXD_XOFFSET(2)/1,1/                                       17
      REAL RE_FBXD_XOFFSET                                                17
C -                                                                       17
      REAL FBXD_ZOFFSET                                                   17
      EQUIVALENCE (BANK_FBXD(12),FBXD_ZOFFSET,RE_FBXD_ZOFFSET),           17
     *(NAMES_FBXD(10),NFBXD_ZOFFSET)                                      17
      EQUIVALENCE (COMMEN_FBXD(10),CFBXD_ZOFFSET),(MAP_FBXD(1,10),        17
     *MFBXD_ZOFFSET)                                                      17
      CHARACTER CFBXD_ZOFFSET*40/' '/                                     17
      CHARACTER NFBXD_ZOFFSET*12/'R.ZOFFSET'/                             17
      INTEGER MFBXD_ZOFFSET(2)/1,1/                                       17
      REAL RE_FBXD_ZOFFSET                                                17
C -                                                                       17
      REAL FBXD_PSOFFSET                                                  17
      EQUIVALENCE (BANK_FBXD(13),FBXD_PSOFFSET,RE_FBXD_PSOFFSET),         17
     *(NAMES_FBXD(11),NFBXD_PSOFFSET)                                     17
      EQUIVALENCE (COMMEN_FBXD(11),CFBXD_PSOFFSET),(MAP_FBXD(1,11),       17
     *MFBXD_PSOFFSET)                                                     17
      CHARACTER CFBXD_PSOFFSET*40/' '/                                    17
      CHARACTER NFBXD_PSOFFSET*12/'R.PSOFFSET'/                           17
      INTEGER MFBXD_PSOFFSET(2)/1,1/                                      17
      REAL RE_FBXD_PSOFFSET                                               17
C -                                                                       17
      REAL FBXD_SMDOFF                                                    17
      EQUIVALENCE (BANK_FBXD(14),FBXD_SMDOFF,RE_FBXD_SMDOFF),             17
     *(NAMES_FBXD(12),NFBXD_SMDOFF)                                       17
      EQUIVALENCE (COMMEN_FBXD(12),CFBXD_SMDOFF),(MAP_FBXD(1,12),         17
     *MFBXD_SMDOFF)                                                       17
      CHARACTER CFBXD_SMDOFF*40/' '/                                      17
      CHARACTER NFBXD_SMDOFF*12/'R.SMDOFF'/                               17
      INTEGER MFBXD_SMDOFF(2)/1,1/                                        17
      REAL RE_FBXD_SMDOFF                                                 17
C -                                                                       17
      INTEGER LEN_FBXD,LLEN_FBXD,LINK_FBXD                                17
      PARAMETER (LEN_FBXD=14)                                             17
      PARAMETER (LLEN_FBXD=12)                                            17
      INTEGER FLAG_FBXD/0/,MAP_FBXD(2,LLEN_FBXD),IDNUM_FBXD(10)           17
      CHARACTER NAMES_FBXD(LLEN_FBXD)*12,COMMEN_FBXD(LLEN_FBXD)*40        17
      DATA IDNUM_FBXD/10*0/                                               17
      REAL BANK_FBXD(LEN_FBXD)                                            17
      CHARACTER*60 BPATH_FBXD/' '/                                        17
      SAVE FLAG_FBXD,MAP_FBXD,IDNUM_FBXD,NAMES_FBXD,COMMEN_FBXD,          17
     *BPATH_FBXD                                                          17
      DATA LINK_FBXD/0/,BANK_FBXD/LEN_FBXD*0/                             17
C *                                                                       17
C -                                                                       18
      REAL FLGG_TYPE                                                      18
      EQUIVALENCE (BANK_FLGG(3),FLGG_TYPE,RE_FLGG_TYPE),(NAMES_FLGG(1),   18
     *NFLGG_TYPE)                                                         18
      EQUIVALENCE (COMMEN_FLGG(1),CFLGG_TYPE),(MAP_FLGG(1,1),             18
     *MFLGG_TYPE)                                                         18
      CHARACTER CFLGG_TYPE*40/' '/                                        18
      CHARACTER NFLGG_TYPE*12/'R.TYPE'/                                   18
      INTEGER MFLGG_TYPE(2)/1,1/                                          18
      REAL RE_FLGG_TYPE                                                   18
C -                                                                       18
      REAL FLGG_WIDTH                                                     18
      EQUIVALENCE (BANK_FLGG(4),FLGG_WIDTH,RE_FLGG_WIDTH),                18
     *(NAMES_FLGG(2),NFLGG_WIDTH)                                         18
      EQUIVALENCE (COMMEN_FLGG(2),CFLGG_WIDTH),(MAP_FLGG(1,2),            18
     *MFLGG_WIDTH)                                                        18
      CHARACTER CFLGG_WIDTH*40/' '/                                       18
      CHARACTER NFLGG_WIDTH*12/'R.WIDTH'/                                 18
      INTEGER MFLGG_WIDTH(2)/1,1/                                         18
      REAL RE_FLGG_WIDTH                                                  18
C -                                                                       18
      REAL FLGG_DEPTH                                                     18
      EQUIVALENCE (BANK_FLGG(5),FLGG_DEPTH,RE_FLGG_DEPTH),                18
     *(NAMES_FLGG(3),NFLGG_DEPTH)                                         18
      EQUIVALENCE (COMMEN_FLGG(3),CFLGG_DEPTH),(MAP_FLGG(1,3),            18
     *MFLGG_DEPTH)                                                        18
      CHARACTER CFLGG_DEPTH*40/' '/                                       18
      CHARACTER NFLGG_DEPTH*12/'R.DEPTH'/                                 18
      INTEGER MFLGG_DEPTH(2)/1,1/                                         18
      REAL RE_FLGG_DEPTH                                                  18
C -                                                                       18
      REAL FLGG_DGAP                                                      18
      EQUIVALENCE (BANK_FLGG(6),FLGG_DGAP,RE_FLGG_DGAP),(NAMES_FLGG(4),   18
     *NFLGG_DGAP)                                                         18
      EQUIVALENCE (COMMEN_FLGG(4),CFLGG_DGAP),(MAP_FLGG(1,4),             18
     *MFLGG_DGAP)                                                         18
      CHARACTER CFLGG_DGAP*40/' '/                                        18
      CHARACTER NFLGG_DGAP*12/'R.DGAP'/                                   18
      INTEGER MFLGG_DGAP(2)/1,1/                                          18
      REAL RE_FLGG_DGAP                                                   18
C -                                                                       18
      REAL FLGG_ALTHICK                                                   18
      EQUIVALENCE (BANK_FLGG(7),FLGG_ALTHICK,RE_FLGG_ALTHICK),            18
     *(NAMES_FLGG(5),NFLGG_ALTHICK)                                       18
      EQUIVALENCE (COMMEN_FLGG(5),CFLGG_ALTHICK),(MAP_FLGG(1,5),          18
     *MFLGG_ALTHICK)                                                      18
      CHARACTER CFLGG_ALTHICK*40/' '/                                     18
      CHARACTER NFLGG_ALTHICK*12/'R.ALTHICK'/                             18
      INTEGER MFLGG_ALTHICK(2)/1,1/                                       18
      REAL RE_FLGG_ALTHICK                                                18
C -                                                                       18
      REAL FLGG_PHCATHDZ                                                  18
      EQUIVALENCE (BANK_FLGG(8),FLGG_PHCATHDZ,RE_FLGG_PHCATHDZ),          18
     *(NAMES_FLGG(6),NFLGG_PHCATHDZ)                                      18
      EQUIVALENCE (COMMEN_FLGG(6),CFLGG_PHCATHDZ),(MAP_FLGG(1,6),         18
     *MFLGG_PHCATHDZ)                                                     18
      CHARACTER CFLGG_PHCATHDZ*40/' '/                                    18
      CHARACTER NFLGG_PHCATHDZ*12/'R.PHCATHDZ'/                           18
      INTEGER MFLGG_PHCATHDZ(2)/1,1/                                      18
      REAL RE_FLGG_PHCATHDZ                                               18
C -                                                                       18
      REAL FLGG_PHCATHR                                                   18
      EQUIVALENCE (BANK_FLGG(9),FLGG_PHCATHR,RE_FLGG_PHCATHR),            18
     *(NAMES_FLGG(7),NFLGG_PHCATHR)                                       18
      EQUIVALENCE (COMMEN_FLGG(7),CFLGG_PHCATHR),(MAP_FLGG(1,7),          18
     *MFLGG_PHCATHR)                                                      18
      CHARACTER CFLGG_PHCATHR*40/' '/                                     18
      CHARACTER NFLGG_PHCATHR*12/'R.PHCATHR'/                             18
      INTEGER MFLGG_PHCATHR(2)/1,1/                                       18
      REAL RE_FLGG_PHCATHR                                                18
C -                                                                       18
      REAL FLGG_MUMETDZ                                                   18
      EQUIVALENCE (BANK_FLGG(10),FLGG_MUMETDZ,RE_FLGG_MUMETDZ),           18
     *(NAMES_FLGG(8),NFLGG_MUMETDZ)                                       18
      EQUIVALENCE (COMMEN_FLGG(8),CFLGG_MUMETDZ),(MAP_FLGG(1,8),          18
     *MFLGG_MUMETDZ)                                                      18
      CHARACTER CFLGG_MUMETDZ*40/' '/                                     18
      CHARACTER NFLGG_MUMETDZ*12/'R.MUMETDZ'/                             18
      INTEGER MFLGG_MUMETDZ(2)/1,1/                                       18
      REAL RE_FLGG_MUMETDZ                                                18
C -                                                                       18
      REAL FLGG_MUMETR                                                    18
      EQUIVALENCE (BANK_FLGG(11),FLGG_MUMETR,RE_FLGG_MUMETR),             18
     *(NAMES_FLGG(9),NFLGG_MUMETR)                                        18
      EQUIVALENCE (COMMEN_FLGG(9),CFLGG_MUMETR),(MAP_FLGG(1,9),           18
     *MFLGG_MUMETR)                                                       18
      CHARACTER CFLGG_MUMETR*40/' '/                                      18
      CHARACTER NFLGG_MUMETR*12/'R.MUMETR'/                               18
      INTEGER MFLGG_MUMETR(2)/1,1/                                        18
      REAL RE_FLGG_MUMETR                                                 18
C -                                                                       18
      INTEGER LEN_FLGG,LLEN_FLGG,LINK_FLGG                                18
      PARAMETER (LEN_FLGG=11)                                             18
      PARAMETER (LLEN_FLGG=9)                                             18
      INTEGER FLAG_FLGG/0/,MAP_FLGG(2,LLEN_FLGG),IDNUM_FLGG(10)           18
      CHARACTER NAMES_FLGG(LLEN_FLGG)*12,COMMEN_FLGG(LLEN_FLGG)*40        18
      DATA IDNUM_FLGG/10*0/                                               18
      REAL BANK_FLGG(LEN_FLGG)                                            18
      CHARACTER*60 BPATH_FLGG/' '/                                        18
      SAVE FLAG_FLGG,MAP_FLGG,IDNUM_FLGG,NAMES_FLGG,COMMEN_FLGG,          18
     *BPATH_FLGG                                                          18
      DATA LINK_FLGG/0/,BANK_FLGG/LEN_FLGG*0/                             18
C *                                                                       18
C -                                                                       19
      REAL FLGM_TYPE                                                      19
      EQUIVALENCE (BANK_FLGM(3),FLGM_TYPE,RE_FLGM_TYPE),(NAMES_FLGM(1),   19
     *NFLGM_TYPE)                                                         19
      EQUIVALENCE (COMMEN_FLGM(1),CFLGM_TYPE),(MAP_FLGM(1,1),             19
     *MFLGM_TYPE)                                                         19
      CHARACTER CFLGM_TYPE*40/' '/                                        19
      CHARACTER NFLGM_TYPE*12/'R.TYPE'/                                   19
      INTEGER MFLGM_TYPE(2)/1,1/                                          19
      REAL RE_FLGM_TYPE                                                   19
C -                                                                       19
      REAL FLGM_DENSITY                                                   19
      EQUIVALENCE (BANK_FLGM(4),FLGM_DENSITY,RE_FLGM_DENSITY),            19
     *(NAMES_FLGM(2),NFLGM_DENSITY)                                       19
      EQUIVALENCE (COMMEN_FLGM(2),CFLGM_DENSITY),(MAP_FLGM(1,2),          19
     *MFLGM_DENSITY)                                                      19
      CHARACTER CFLGM_DENSITY*40/' '/                                     19
      CHARACTER NFLGM_DENSITY*12/'R.DENSITY'/                             19
      INTEGER MFLGM_DENSITY(2)/1,1/                                       19
      REAL RE_FLGM_DENSITY                                                19
C -                                                                       19
      REAL FLGM_RADLEN                                                    19
      EQUIVALENCE (BANK_FLGM(5),FLGM_RADLEN,RE_FLGM_RADLEN),              19
     *(NAMES_FLGM(3),NFLGM_RADLEN)                                        19
      EQUIVALENCE (COMMEN_FLGM(3),CFLGM_RADLEN),(MAP_FLGM(1,3),           19
     *MFLGM_RADLEN)                                                       19
      CHARACTER CFLGM_RADLEN*40/' '/                                      19
      CHARACTER NFLGM_RADLEN*12/'R.RADLEN'/                               19
      INTEGER MFLGM_RADLEN(2)/1,1/                                        19
      REAL RE_FLGM_RADLEN                                                 19
C -                                                                       19
      REAL FLGM_PBCONT                                                    19
      EQUIVALENCE (BANK_FLGM(6),FLGM_PBCONT,RE_FLGM_PBCONT),              19
     *(NAMES_FLGM(4),NFLGM_PBCONT)                                        19
      EQUIVALENCE (COMMEN_FLGM(4),CFLGM_PBCONT),(MAP_FLGM(1,4),           19
     *MFLGM_PBCONT)                                                       19
      CHARACTER CFLGM_PBCONT*40/' '/                                      19
      CHARACTER NFLGM_PBCONT*12/'R.PBCONT'/                               19
      INTEGER MFLGM_PBCONT(2)/1,1/                                        19
      REAL RE_FLGM_PBCONT                                                 19
C -                                                                       19
      REAL FLGM_CRITENE                                                   19
      EQUIVALENCE (BANK_FLGM(7),FLGM_CRITENE,RE_FLGM_CRITENE),            19
     *(NAMES_FLGM(5),NFLGM_CRITENE)                                       19
      EQUIVALENCE (COMMEN_FLGM(5),CFLGM_CRITENE),(MAP_FLGM(1,5),          19
     *MFLGM_CRITENE)                                                      19
      CHARACTER CFLGM_CRITENE*40/' '/                                     19
      CHARACTER NFLGM_CRITENE*12/'R.CRITENE'/                             19
      INTEGER MFLGM_CRITENE(2)/1,1/                                       19
      REAL RE_FLGM_CRITENE                                                19
C -                                                                       19
      REAL FLGM_MOLIERER                                                  19
      EQUIVALENCE (BANK_FLGM(8),FLGM_MOLIERER,RE_FLGM_MOLIERER),          19
     *(NAMES_FLGM(6),NFLGM_MOLIERER)                                      19
      EQUIVALENCE (COMMEN_FLGM(6),CFLGM_MOLIERER),(MAP_FLGM(1,6),         19
     *MFLGM_MOLIERER)                                                     19
      CHARACTER CFLGM_MOLIERER*40/' '/                                    19
      CHARACTER NFLGM_MOLIERER*12/'R.MOLIERER'/                           19
      INTEGER MFLGM_MOLIERER(2)/1,1/                                      19
      REAL RE_FLGM_MOLIERER                                               19
C -                                                                       19
      INTEGER LEN_FLGM,LLEN_FLGM,LINK_FLGM                                19
      PARAMETER (LEN_FLGM=8)                                              19
      PARAMETER (LLEN_FLGM=6)                                             19
      INTEGER FLAG_FLGM/0/,MAP_FLGM(2,LLEN_FLGM),IDNUM_FLGM(10)           19
      CHARACTER NAMES_FLGM(LLEN_FLGM)*12,COMMEN_FLGM(LLEN_FLGM)*40        19
      DATA IDNUM_FLGM/10*0/                                               19
      REAL BANK_FLGM(LEN_FLGM)                                            19
      CHARACTER*60 BPATH_FLGM/' '/                                        19
      SAVE FLAG_FLGM,MAP_FLGM,IDNUM_FLGM,NAMES_FLGM,COMMEN_FLGM,          19
     *BPATH_FLGM                                                          19
      DATA LINK_FLGM/0/,BANK_FLGM/LEN_FLGM*0/                             19
C *                                                                       19
C -                                                                       20
      REAL PBPD_Z                                                         20
      EQUIVALENCE (BANK_PBPD(3),PBPD_Z,RE_PBPD_Z),(NAMES_PBPD(1),         20
     *NPBPD_Z)                                                            20
      EQUIVALENCE (COMMEN_PBPD(1),CPBPD_Z),(MAP_PBPD(1,1),MPBPD_Z)        20
      CHARACTER CPBPD_Z*40/' '/                                           20
      CHARACTER NPBPD_Z*12/'R.Z'/                                         20
      INTEGER MPBPD_Z(2)/1,1/                                             20
      REAL RE_PBPD_Z                                                      20
C -                                                                       20
      REAL PBPD_WIDTH                                                     20
      EQUIVALENCE (BANK_PBPD(4),PBPD_WIDTH,RE_PBPD_WIDTH),                20
     *(NAMES_PBPD(2),NPBPD_WIDTH)                                         20
      EQUIVALENCE (COMMEN_PBPD(2),CPBPD_WIDTH),(MAP_PBPD(1,2),            20
     *MPBPD_WIDTH)                                                        20
      CHARACTER CPBPD_WIDTH*40/' '/                                       20
      CHARACTER NPBPD_WIDTH*12/'R.WIDTH'/                                 20
      INTEGER MPBPD_WIDTH(2)/1,1/                                         20
      REAL RE_PBPD_WIDTH                                                  20
C -                                                                       20
      REAL PBPD_HEIGHT                                                    20
      EQUIVALENCE (BANK_PBPD(5),PBPD_HEIGHT,RE_PBPD_HEIGHT),              20
     *(NAMES_PBPD(3),NPBPD_HEIGHT)                                        20
      EQUIVALENCE (COMMEN_PBPD(3),CPBPD_HEIGHT),(MAP_PBPD(1,3),           20
     *MPBPD_HEIGHT)                                                       20
      CHARACTER CPBPD_HEIGHT*40/' '/                                      20
      CHARACTER NPBPD_HEIGHT*12/'R.HEIGHT'/                               20
      INTEGER MPBPD_HEIGHT(2)/1,1/                                        20
      REAL RE_PBPD_HEIGHT                                                 20
C -                                                                       20
      REAL PBPD_THICK                                                     20
      EQUIVALENCE (BANK_PBPD(6),PBPD_THICK,RE_PBPD_THICK),                20
     *(NAMES_PBPD(4),NPBPD_THICK)                                         20
      EQUIVALENCE (COMMEN_PBPD(4),CPBPD_THICK),(MAP_PBPD(1,4),            20
     *MPBPD_THICK)                                                        20
      CHARACTER CPBPD_THICK*40/' '/                                       20
      CHARACTER NPBPD_THICK*12/'R.THICK'/                                 20
      INTEGER MPBPD_THICK(2)/1,1/                                         20
      REAL RE_PBPD_THICK                                                  20
C -                                                                       20
      INTEGER LEN_PBPD,LLEN_PBPD,LINK_PBPD                                20
      PARAMETER (LEN_PBPD=6)                                              20
      PARAMETER (LLEN_PBPD=4)                                             20
      INTEGER FLAG_PBPD/0/,MAP_PBPD(2,LLEN_PBPD),IDNUM_PBPD(10)           20
      CHARACTER NAMES_PBPD(LLEN_PBPD)*12,COMMEN_PBPD(LLEN_PBPD)*40        20
      DATA IDNUM_PBPD/10*0/                                               20
      REAL BANK_PBPD(LEN_PBPD)                                            20
      CHARACTER*60 BPATH_PBPD/' '/                                        20
      SAVE FLAG_PBPD,MAP_PBPD,IDNUM_PBPD,NAMES_PBPD,COMMEN_PBPD,          20
     *BPATH_PBPD                                                          20
      DATA LINK_PBPD/0/,BANK_PBPD/LEN_PBPD*0/                             20
C *                                                                       20
C -                                                                       21
      REAL FMXG_VERSION                                                   21
      EQUIVALENCE (BANK_FMXG(3),FMXG_VERSION,RE_FMXG_VERSION),            21
     *(NAMES_FMXG(1),NFMXG_VERSION)                                       21
      EQUIVALENCE (COMMEN_FMXG(1),CFMXG_VERSION),(MAP_FMXG(1,1),          21
     *MFMXG_VERSION)                                                      21
      CHARACTER CFMXG_VERSION*40/' '/                                     21
      CHARACTER NFMXG_VERSION*12/'R.VERSION'/                             21
      INTEGER MFMXG_VERSION(2)/1,1/                                       21
      REAL RE_FMXG_VERSION                                                21
C -                                                                       21
      REAL FMXG_SAPEX                                                     21
      EQUIVALENCE (BANK_FMXG(4),FMXG_SAPEX,RE_FMXG_SAPEX),                21
     *(NAMES_FMXG(2),NFMXG_SAPEX)                                         21
      EQUIVALENCE (COMMEN_FMXG(2),CFMXG_SAPEX),(MAP_FMXG(1,2),            21
     *MFMXG_SAPEX)                                                        21
      CHARACTER CFMXG_SAPEX*40/' '/                                       21
      CHARACTER NFMXG_SAPEX*12/'R.SAPEX'/                                 21
      INTEGER MFMXG_SAPEX(2)/1,1/                                         21
      REAL RE_FMXG_SAPEX                                                  21
C -                                                                       21
      REAL FMXG_SBASE                                                     21
      EQUIVALENCE (BANK_FMXG(5),FMXG_SBASE,RE_FMXG_SBASE),                21
     *(NAMES_FMXG(3),NFMXG_SBASE)                                         21
      EQUIVALENCE (COMMEN_FMXG(3),CFMXG_SBASE),(MAP_FMXG(1,3),            21
     *MFMXG_SBASE)                                                        21
      CHARACTER CFMXG_SBASE*40/' '/                                       21
      CHARACTER NFMXG_SBASE*12/'R.SBASE'/                                 21
      INTEGER MFMXG_SBASE(2)/1,1/                                         21
      REAL RE_FMXG_SBASE                                                  21
C -                                                                       21
      REAL FMXG_SGAP                                                      21
      EQUIVALENCE (BANK_FMXG(6),FMXG_SGAP,RE_FMXG_SGAP),(NAMES_FMXG(4),   21
     *NFMXG_SGAP)                                                         21
      EQUIVALENCE (COMMEN_FMXG(4),CFMXG_SGAP),(MAP_FMXG(1,4),             21
     *MFMXG_SGAP)                                                         21
      CHARACTER CFMXG_SGAP*40/' '/                                        21
      CHARACTER NFMXG_SGAP*12/'R.SGAP'/                                   21
      INTEGER MFMXG_SGAP(2)/1,1/                                          21
      REAL RE_FMXG_SGAP                                                   21
C -                                                                       21
      REAL FMXG_NSTRIP                                                    21
      EQUIVALENCE (BANK_FMXG(7),FMXG_NSTRIP,RE_FMXG_NSTRIP),              21
     *(NAMES_FMXG(5),NFMXG_NSTRIP)                                        21
      EQUIVALENCE (COMMEN_FMXG(5),CFMXG_NSTRIP),(MAP_FMXG(1,5),           21
     *MFMXG_NSTRIP)                                                       21
      CHARACTER CFMXG_NSTRIP*40/' '/                                      21
      CHARACTER NFMXG_NSTRIP*12/'R.NSTRIP'/                               21
      INTEGER MFMXG_NSTRIP(2)/1,1/                                        21
      REAL RE_FMXG_NSTRIP                                                 21
C -                                                                       21
      REAL FMXG_G10WIDTH                                                  21
      EQUIVALENCE (BANK_FMXG(8),FMXG_G10WIDTH,RE_FMXG_G10WIDTH),          21
     *(NAMES_FMXG(6),NFMXG_G10WIDTH)                                      21
      EQUIVALENCE (COMMEN_FMXG(6),CFMXG_G10WIDTH),(MAP_FMXG(1,6),         21
     *MFMXG_G10WIDTH)                                                     21
      CHARACTER CFMXG_G10WIDTH*40/' '/                                    21
      CHARACTER NFMXG_G10WIDTH*12/'R.G10WIDTH'/                           21
      INTEGER MFMXG_G10WIDTH(2)/1,1/                                      21
      REAL RE_FMXG_G10WIDTH                                               21
C -                                                                       21
      REAL FMXG_G10HGT                                                    21
      EQUIVALENCE (BANK_FMXG(9),FMXG_G10HGT,RE_FMXG_G10HGT),              21
     *(NAMES_FMXG(7),NFMXG_G10HGT)                                        21
      EQUIVALENCE (COMMEN_FMXG(7),CFMXG_G10HGT),(MAP_FMXG(1,7),           21
     *MFMXG_G10HGT)                                                       21
      CHARACTER CFMXG_G10HGT*40/' '/                                      21
      CHARACTER NFMXG_G10HGT*12/'R.G10HGT'/                               21
      INTEGER MFMXG_G10HGT(2)/1,1/                                        21
      REAL RE_FMXG_G10HGT                                                 21
C -                                                                       21
      REAL FMXG_G10THICK                                                  21
      EQUIVALENCE (BANK_FMXG(10),FMXG_G10THICK,RE_FMXG_G10THICK),         21
     *(NAMES_FMXG(8),NFMXG_G10THICK)                                      21
      EQUIVALENCE (COMMEN_FMXG(8),CFMXG_G10THICK),(MAP_FMXG(1,8),         21
     *MFMXG_G10THICK)                                                     21
      CHARACTER CFMXG_G10THICK*40/' '/                                    21
      CHARACTER NFMXG_G10THICK*12/'R.G10THICK'/                           21
      INTEGER MFMXG_G10THICK(2)/1,1/                                      21
      REAL RE_FMXG_G10THICK                                               21
C -                                                                       21
      INTEGER LEN_FMXG,LLEN_FMXG,LINK_FMXG                                21
      PARAMETER (LEN_FMXG=10)                                             21
      PARAMETER (LLEN_FMXG=8)                                             21
      INTEGER FLAG_FMXG/0/,MAP_FMXG(2,LLEN_FMXG),IDNUM_FMXG(10)           21
      CHARACTER NAMES_FMXG(LLEN_FMXG)*12,COMMEN_FMXG(LLEN_FMXG)*40        21
      DATA IDNUM_FMXG/10*0/                                               21
      REAL BANK_FMXG(LEN_FMXG)                                            21
      CHARACTER*60 BPATH_FMXG/' '/                                        21
      SAVE FLAG_FMXG,MAP_FMXG,IDNUM_FMXG,NAMES_FMXG,COMMEN_FMXG,          21
     *BPATH_FMXG                                                          21
      DATA LINK_FMXG/0/,BANK_FMXG/LEN_FMXG*0/                             21
C *                                                                       21
C -                                                                       23
      REAL INSE_WIDTH                                                     23
      EQUIVALENCE (BANK_INSE(3),INSE_WIDTH,RE_INSE_WIDTH),                23
     *(NAMES_INSE(1),NINSE_WIDTH)                                         23
      EQUIVALENCE (COMMEN_INSE(1),CINSE_WIDTH),(MAP_INSE(1,1),            23
     *MINSE_WIDTH)                                                        23
      CHARACTER CINSE_WIDTH*40/' '/                                       23
      CHARACTER NINSE_WIDTH*12/'R.WIDTH'/                                 23
      INTEGER MINSE_WIDTH(2)/1,1/                                         23
      REAL RE_INSE_WIDTH                                                  23
C -                                                                       23
      REAL INSE_DEPTH                                                     23
      EQUIVALENCE (BANK_INSE(4),INSE_DEPTH,RE_INSE_DEPTH),                23
     *(NAMES_INSE(2),NINSE_DEPTH)                                         23
      EQUIVALENCE (COMMEN_INSE(2),CINSE_DEPTH),(MAP_INSE(1,2),            23
     *MINSE_DEPTH)                                                        23
      CHARACTER CINSE_DEPTH*40/' '/                                       23
      CHARACTER NINSE_DEPTH*12/'R.DEPTH'/                                 23
      INTEGER MINSE_DEPTH(2)/1,1/                                         23
      REAL RE_INSE_DEPTH                                                  23
C -                                                                       23
      REAL INSE_HEIGHT                                                    23
      EQUIVALENCE (BANK_INSE(5),INSE_HEIGHT,RE_INSE_HEIGHT),              23
     *(NAMES_INSE(3),NINSE_HEIGHT)                                        23
      EQUIVALENCE (COMMEN_INSE(3),CINSE_HEIGHT),(MAP_INSE(1,3),           23
     *MINSE_HEIGHT)                                                       23
      CHARACTER CINSE_HEIGHT*40/' '/                                      23
      CHARACTER NINSE_HEIGHT*12/'R.HEIGHT'/                               23
      INTEGER MINSE_HEIGHT(2)/1,1/                                        23
      REAL RE_INSE_HEIGHT                                                 23
C -                                                                       23
      REAL INSE_SHEETDPT                                                  23
      EQUIVALENCE (BANK_INSE(6),INSE_SHEETDPT,RE_INSE_SHEETDPT),          23
     *(NAMES_INSE(4),NINSE_SHEETDPT)                                      23
      EQUIVALENCE (COMMEN_INSE(4),CINSE_SHEETDPT),(MAP_INSE(1,4),         23
     *MINSE_SHEETDPT)                                                     23
      CHARACTER CINSE_SHEETDPT*40/' '/                                    23
      CHARACTER NINSE_SHEETDPT*12/'R.SHEETDPT'/                           23
      INTEGER MINSE_SHEETDPT(2)/1,1/                                      23
      REAL RE_INSE_SHEETDPT                                               23
C -                                                                       23
      REAL INSE_HOLEGAP                                                   23
      EQUIVALENCE (BANK_INSE(7),INSE_HOLEGAP,RE_INSE_HOLEGAP),            23
     *(NAMES_INSE(5),NINSE_HOLEGAP)                                       23
      EQUIVALENCE (COMMEN_INSE(5),CINSE_HOLEGAP),(MAP_INSE(1,5),          23
     *MINSE_HOLEGAP)                                                      23
      CHARACTER CINSE_HOLEGAP*40/' '/                                     23
      CHARACTER NINSE_HOLEGAP*12/'R.HOLEGAP'/                             23
      INTEGER MINSE_HOLEGAP(2)/1,1/                                       23
      REAL RE_INSE_HOLEGAP                                                23
C -                                                                       23
      REAL INSE_HOLEDEPTH                                                 23
      EQUIVALENCE (BANK_INSE(8),INSE_HOLEDEPTH,RE_INSE_HOLEDEPTH),        23
     *(NAMES_INSE(6),NINSE_HOLEDEPTH)                                     23
      EQUIVALENCE (COMMEN_INSE(6),CINSE_HOLEDEPTH),(MAP_INSE(1,6),        23
     *MINSE_HOLEDEPTH)                                                    23
      CHARACTER CINSE_HOLEDEPTH*40/' '/                                   23
      CHARACTER NINSE_HOLEDEPTH*12/'R.HOLEDEPTH'/                         23
      INTEGER MINSE_HOLEDEPTH(2)/1,1/                                     23
      REAL RE_INSE_HOLEDEPTH                                              23
C -                                                                       23
      REAL INSE_HOLEHEIGHT                                                23
      EQUIVALENCE (BANK_INSE(9),INSE_HOLEHEIGHT,RE_INSE_HOLEHEIGHT),      23
     *(NAMES_INSE(7),NINSE_HOLEHEIGHT)                                    23
      EQUIVALENCE (COMMEN_INSE(7),CINSE_HOLEHEIGHT),(MAP_INSE(1,7),       23
     *MINSE_HOLEHEIGHT)                                                   23
      CHARACTER CINSE_HOLEHEIGHT*40/' '/                                  23
      CHARACTER NINSE_HOLEHEIGHT*12/'R.HOLEHEIGHT'/                       23
      INTEGER MINSE_HOLEHEIGHT(2)/1,1/                                    23
      REAL RE_INSE_HOLEHEIGHT                                             23
C -                                                                       23
      REAL INSE_GAPDEPTH                                                  23
      EQUIVALENCE (BANK_INSE(10),INSE_GAPDEPTH,RE_INSE_GAPDEPTH),         23
     *(NAMES_INSE(8),NINSE_GAPDEPTH)                                      23
      EQUIVALENCE (COMMEN_INSE(8),CINSE_GAPDEPTH),(MAP_INSE(1,8),         23
     *MINSE_GAPDEPTH)                                                     23
      CHARACTER CINSE_GAPDEPTH*40/' '/                                    23
      CHARACTER NINSE_GAPDEPTH*12/'R.GAPDEPTH'/                           23
      INTEGER MINSE_GAPDEPTH(2)/1,1/                                      23
      REAL RE_INSE_GAPDEPTH                                               23
C -                                                                       23
      REAL INSE_GAPHEIGHT                                                 23
      EQUIVALENCE (BANK_INSE(11),INSE_GAPHEIGHT,RE_INSE_GAPHEIGHT),       23
     *(NAMES_INSE(9),NINSE_GAPHEIGHT)                                     23
      EQUIVALENCE (COMMEN_INSE(9),CINSE_GAPHEIGHT),(MAP_INSE(1,9),        23
     *MINSE_GAPHEIGHT)                                                    23
      CHARACTER CINSE_GAPHEIGHT*40/' '/                                   23
      CHARACTER NINSE_GAPHEIGHT*12/'R.GAPHEIGHT'/                         23
      INTEGER MINSE_GAPHEIGHT(2)/1,1/                                     23
      REAL RE_INSE_GAPHEIGHT                                              23
C -                                                                       23
      REAL INSE_GATEDEPTH                                                 23
      EQUIVALENCE (BANK_INSE(12),INSE_GATEDEPTH,RE_INSE_GATEDEPTH),       23
     *(NAMES_INSE(10),NINSE_GATEDEPTH)                                    23
      EQUIVALENCE (COMMEN_INSE(10),CINSE_GATEDEPTH),(MAP_INSE(1,10),      23
     *MINSE_GATEDEPTH)                                                    23
      CHARACTER CINSE_GATEDEPTH*40/' '/                                   23
      CHARACTER NINSE_GATEDEPTH*12/'R.GATEDEPTH'/                         23
      INTEGER MINSE_GATEDEPTH(2)/1,1/                                     23
      REAL RE_INSE_GATEDEPTH                                              23
C -                                                                       23
      REAL INSE_RA                                                        23
      EQUIVALENCE (BANK_INSE(13),INSE_RA,RE_INSE_RA),(NAMES_INSE(11),     23
     *NINSE_RA)                                                           23
      EQUIVALENCE (COMMEN_INSE(11),CINSE_RA),(MAP_INSE(1,11),MINSE_RA)    23
      CHARACTER CINSE_RA*40/' '/                                          23
      CHARACTER NINSE_RA*12/'R.RA'/                                       23
      INTEGER MINSE_RA(2)/1,1/                                            23
      REAL RE_INSE_RA                                                     23
C -                                                                       23
      REAL INSE_RB                                                        23
      EQUIVALENCE (BANK_INSE(14),INSE_RB,RE_INSE_RB),(NAMES_INSE(12),     23
     *NINSE_RB)                                                           23
      EQUIVALENCE (COMMEN_INSE(12),CINSE_RB),(MAP_INSE(1,12),MINSE_RB)    23
      CHARACTER CINSE_RB*40/' '/                                          23
      CHARACTER NINSE_RB*12/'R.RB'/                                       23
      INTEGER MINSE_RB(2)/1,1/                                            23
      REAL RE_INSE_RB                                                     23
C -                                                                       23
      REAL INSE_DIAM                                                      23
      EQUIVALENCE (BANK_INSE(15),INSE_DIAM,RE_INSE_DIAM),                 23
     *(NAMES_INSE(13),NINSE_DIAM)                                         23
      EQUIVALENCE (COMMEN_INSE(13),CINSE_DIAM),(MAP_INSE(1,13),           23
     *MINSE_DIAM)                                                         23
      CHARACTER CINSE_DIAM*40/' '/                                        23
      CHARACTER NINSE_DIAM*12/'R.DIAM'/                                   23
      INTEGER MINSE_DIAM(2)/1,1/                                          23
      REAL RE_INSE_DIAM                                                   23
C -                                                                       23
      REAL INSE_RMAX                                                      23
      EQUIVALENCE (BANK_INSE(16),INSE_RMAX,RE_INSE_RMAX),                 23
     *(NAMES_INSE(14),NINSE_RMAX)                                         23
      EQUIVALENCE (COMMEN_INSE(14),CINSE_RMAX),(MAP_INSE(1,14),           23
     *MINSE_RMAX)                                                         23
      CHARACTER CINSE_RMAX*40/' '/                                        23
      CHARACTER NINSE_RMAX*12/'R.RMAX'/                                   23
      INTEGER MINSE_RMAX(2)/1,1/                                          23
      REAL RE_INSE_RMAX                                                   23
C -                                                                       23
      REAL INSE_GATEGAP                                                   23
      EQUIVALENCE (BANK_INSE(17),INSE_GATEGAP,RE_INSE_GATEGAP),           23
     *(NAMES_INSE(15),NINSE_GATEGAP)                                      23
      EQUIVALENCE (COMMEN_INSE(15),CINSE_GATEGAP),(MAP_INSE(1,15),        23
     *MINSE_GATEGAP)                                                      23
      CHARACTER CINSE_GATEGAP*40/' '/                                     23
      CHARACTER NINSE_GATEGAP*12/'R.GATEGAP'/                             23
      INTEGER MINSE_GATEGAP(2)/1,1/                                       23
      REAL RE_INSE_GATEGAP                                                23
C -                                                                       23
      INTEGER LEN_INSE,LLEN_INSE,LINK_INSE                                23
      PARAMETER (LEN_INSE=17)                                             23
      PARAMETER (LLEN_INSE=15)                                            23
      INTEGER FLAG_INSE/0/,MAP_INSE(2,LLEN_INSE),IDNUM_INSE(10)           23
      CHARACTER NAMES_INSE(LLEN_INSE)*12,COMMEN_INSE(LLEN_INSE)*40        23
      DATA IDNUM_INSE/10*0/                                               23
      REAL BANK_INSE(LEN_INSE)                                            23
      CHARACTER*60 BPATH_INSE/' '/                                        23
      SAVE FLAG_INSE,MAP_INSE,IDNUM_INSE,NAMES_INSE,COMMEN_INSE,          23
     *BPATH_INSE                                                          23
      DATA LINK_INSE/0/,BANK_INSE/LEN_INSE*0/                             23
C *                                                                       23
C -                                                                       25
      REAL PRSW_DBOX(3)                                                   25
      EQUIVALENCE (BANK_PRSW(3),PRSW_DBOX,RE_PRSW_DBOX),(NAMES_PRSW(1),   25
     *NPRSW_DBOX)                                                         25
      EQUIVALENCE (COMMEN_PRSW(1),CPRSW_DBOX),(MAP_PRSW(1,1),             25
     *MPRSW_DBOX)                                                         25
      CHARACTER CPRSW_DBOX*40/' '/                                        25
      CHARACTER NPRSW_DBOX*12/'R.DBOX'/                                   25
      INTEGER MPRSW_DBOX(2)/3,1/                                          25
      REAL RE_PRSW_DBOX                                                   25
C -                                                                       25
      REAL PRSW_XOFF(4)                                                   25
      EQUIVALENCE (BANK_PRSW(6),PRSW_XOFF,RE_PRSW_XOFF),(NAMES_PRSW(2),   25
     *NPRSW_XOFF)                                                         25
      EQUIVALENCE (COMMEN_PRSW(2),CPRSW_XOFF),(MAP_PRSW(1,2),             25
     *MPRSW_XOFF)                                                         25
      CHARACTER CPRSW_XOFF*40/' '/                                        25
      CHARACTER NPRSW_XOFF*12/'R.XOFF'/                                   25
      INTEGER MPRSW_XOFF(2)/4,1/                                          25
      REAL RE_PRSW_XOFF                                                   25
C -                                                                       25
      REAL PRSW_YOFF(4)                                                   25
      EQUIVALENCE (BANK_PRSW(10),PRSW_YOFF,RE_PRSW_YOFF),(NAMES_PRSW(3),  25
     *NPRSW_YOFF)                                                         25
      EQUIVALENCE (COMMEN_PRSW(3),CPRSW_YOFF),(MAP_PRSW(1,3),             25
     *MPRSW_YOFF)                                                         25
      CHARACTER CPRSW_YOFF*40/' '/                                        25
      CHARACTER NPRSW_YOFF*12/'R.YOFF'/                                   25
      INTEGER MPRSW_YOFF(2)/4,1/                                          25
      REAL RE_PRSW_YOFF                                                   25
C -                                                                       25
      REAL PRSW_ZOFF(4)                                                   25
      EQUIVALENCE (BANK_PRSW(14),PRSW_ZOFF,RE_PRSW_ZOFF),(NAMES_PRSW(4),  25
     *NPRSW_ZOFF)                                                         25
      EQUIVALENCE (COMMEN_PRSW(4),CPRSW_ZOFF),(MAP_PRSW(1,4),             25
     *MPRSW_ZOFF)                                                         25
      CHARACTER CPRSW_ZOFF*40/' '/                                        25
      CHARACTER NPRSW_ZOFF*12/'R.ZOFF'/                                   25
      INTEGER MPRSW_ZOFF(2)/4,1/                                          25
      REAL RE_PRSW_ZOFF                                                   25
C -                                                                       25
      REAL PRSW_DZ(4)                                                     25
      EQUIVALENCE (BANK_PRSW(18),PRSW_DZ,RE_PRSW_DZ),(NAMES_PRSW(5),      25
     *NPRSW_DZ)                                                           25
      EQUIVALENCE (COMMEN_PRSW(5),CPRSW_DZ),(MAP_PRSW(1,5),MPRSW_DZ)      25
      CHARACTER CPRSW_DZ*40/' '/                                          25
      CHARACTER NPRSW_DZ*12/'R.DZ'/                                       25
      INTEGER MPRSW_DZ(2)/4,1/                                            25
      REAL RE_PRSW_DZ                                                     25
C -                                                                       25
      REAL PRSW_DHOLE                                                     25
      EQUIVALENCE (BANK_PRSW(22),PRSW_DHOLE,RE_PRSW_DHOLE),               25
     *(NAMES_PRSW(6),NPRSW_DHOLE)                                         25
      EQUIVALENCE (COMMEN_PRSW(6),CPRSW_DHOLE),(MAP_PRSW(1,6),            25
     *MPRSW_DHOLE)                                                        25
      CHARACTER CPRSW_DHOLE*40/' '/                                       25
      CHARACTER NPRSW_DHOLE*12/'R.DHOLE'/                                 25
      INTEGER MPRSW_DHOLE(2)/1,1/                                         25
      REAL RE_PRSW_DHOLE                                                  25
C -                                                                       25
      REAL PRSW_NTYPE                                                     25
      EQUIVALENCE (BANK_PRSW(23),PRSW_NTYPE,RE_PRSW_NTYPE),               25
     *(NAMES_PRSW(7),NPRSW_NTYPE)                                         25
      EQUIVALENCE (COMMEN_PRSW(7),CPRSW_NTYPE),(MAP_PRSW(1,7),            25
     *MPRSW_NTYPE)                                                        25
      CHARACTER CPRSW_NTYPE*40/' '/                                       25
      CHARACTER NPRSW_NTYPE*12/'R.NTYPE'/                                 25
      INTEGER MPRSW_NTYPE(2)/1,1/                                         25
      REAL RE_PRSW_NTYPE                                                  25
C -                                                                       25
      REAL PRSW_NPRS(2)                                                   25
      EQUIVALENCE (BANK_PRSW(24),PRSW_NPRS,RE_PRSW_NPRS),(NAMES_PRSW(8),  25
     *NPRSW_NPRS)                                                         25
      EQUIVALENCE (COMMEN_PRSW(8),CPRSW_NPRS),(MAP_PRSW(1,8),             25
     *MPRSW_NPRS)                                                         25
      CHARACTER CPRSW_NPRS*40/' '/                                        25
      CHARACTER NPRSW_NPRS*12/'R.NPRS'/                                   25
      INTEGER MPRSW_NPRS(2)/2,1/                                          25
      REAL RE_PRSW_NPRS                                                   25
C -                                                                       25
      REAL PRSW_DPRS(3)                                                   25
      EQUIVALENCE (BANK_PRSW(26),PRSW_DPRS,RE_PRSW_DPRS),(NAMES_PRSW(9),  25
     *NPRSW_DPRS)                                                         25
      EQUIVALENCE (COMMEN_PRSW(9),CPRSW_DPRS),(MAP_PRSW(1,9),             25
     *MPRSW_DPRS)                                                         25
      CHARACTER CPRSW_DPRS*40/' '/                                        25
      CHARACTER NPRSW_DPRS*12/'R.DPRS'/                                   25
      INTEGER MPRSW_DPRS(2)/3,1/                                          25
      REAL RE_PRSW_DPRS                                                   25
C -                                                                       25
      REAL PRSW_NSKIPV                                                    25
      EQUIVALENCE (BANK_PRSW(29),PRSW_NSKIPV,RE_PRSW_NSKIPV),             25
     *(NAMES_PRSW(10),NPRSW_NSKIPV)                                       25
      EQUIVALENCE (COMMEN_PRSW(10),CPRSW_NSKIPV),(MAP_PRSW(1,10),         25
     *MPRSW_NSKIPV)                                                       25
      CHARACTER CPRSW_NSKIPV*40/' '/                                      25
      CHARACTER NPRSW_NSKIPV*12/'R.NSKIPV'/                               25
      INTEGER MPRSW_NSKIPV(2)/1,1/                                        25
      REAL RE_PRSW_NSKIPV                                                 25
C -                                                                       25
      REAL PRSW_NSKIPH                                                    25
      EQUIVALENCE (BANK_PRSW(30),PRSW_NSKIPH,RE_PRSW_NSKIPH),             25
     *(NAMES_PRSW(11),NPRSW_NSKIPH)                                       25
      EQUIVALENCE (COMMEN_PRSW(11),CPRSW_NSKIPH),(MAP_PRSW(1,11),         25
     *MPRSW_NSKIPH)                                                       25
      CHARACTER CPRSW_NSKIPH*40/' '/                                      25
      CHARACTER NPRSW_NSKIPH*12/'R.NSKIPH'/                               25
      INTEGER MPRSW_NSKIPH(2)/1,1/                                        25
      REAL RE_PRSW_NSKIPH                                                 25
C -                                                                       25
      REAL PRSW_DYCUT                                                     25
      EQUIVALENCE (BANK_PRSW(31),PRSW_DYCUT,RE_PRSW_DYCUT),               25
     *(NAMES_PRSW(12),NPRSW_DYCUT)                                        25
      EQUIVALENCE (COMMEN_PRSW(12),CPRSW_DYCUT),(MAP_PRSW(1,12),          25
     *MPRSW_DYCUT)                                                        25
      CHARACTER CPRSW_DYCUT*40/' '/                                       25
      CHARACTER NPRSW_DYCUT*12/'R.DYCUT'/                                 25
      INTEGER MPRSW_DYCUT(2)/1,1/                                         25
      REAL RE_PRSW_DYCUT                                                  25
C -                                                                       25
      REAL PRSW_DLG(3)                                                    25
      EQUIVALENCE (BANK_PRSW(32),PRSW_DLG,RE_PRSW_DLG),(NAMES_PRSW(13),   25
     *NPRSW_DLG)                                                          25
      EQUIVALENCE (COMMEN_PRSW(13),CPRSW_DLG),(MAP_PRSW(1,13),            25
     *MPRSW_DLG)                                                          25
      CHARACTER CPRSW_DLG*40/' '/                                         25
      CHARACTER NPRSW_DLG*12/'R.DLG'/                                     25
      INTEGER MPRSW_DLG(2)/3,1/                                           25
      REAL RE_PRSW_DLG                                                    25
C -                                                                       25
      REAL PRSW_DSIPM(4)                                                  25
      EQUIVALENCE (BANK_PRSW(35),PRSW_DSIPM,RE_PRSW_DSIPM),               25
     *(NAMES_PRSW(14),NPRSW_DSIPM)                                        25
      EQUIVALENCE (COMMEN_PRSW(14),CPRSW_DSIPM),(MAP_PRSW(1,14),          25
     *MPRSW_DSIPM)                                                        25
      CHARACTER CPRSW_DSIPM*40/' '/                                       25
      CHARACTER NPRSW_DSIPM*12/'R.DSIPM'/                                 25
      INTEGER MPRSW_DSIPM(2)/4,1/                                         25
      REAL RE_PRSW_DSIPM                                                  25
C -                                                                       25
      REAL PRSW_DZBBD(4)                                                  25
      EQUIVALENCE (BANK_PRSW(39),PRSW_DZBBD,RE_PRSW_DZBBD),               25
     *(NAMES_PRSW(15),NPRSW_DZBBD)                                        25
      EQUIVALENCE (COMMEN_PRSW(15),CPRSW_DZBBD),(MAP_PRSW(1,15),          25
     *MPRSW_DZBBD)                                                        25
      CHARACTER CPRSW_DZBBD*40/' '/                                       25
      CHARACTER NPRSW_DZBBD*12/'R.DZBBD'/                                 25
      INTEGER MPRSW_DZBBD(2)/4,1/                                         25
      REAL RE_PRSW_DZBBD                                                  25
C -                                                                       25
      REAL PRSW_DXBBD(2)                                                  25
      EQUIVALENCE (BANK_PRSW(43),PRSW_DXBBD,RE_PRSW_DXBBD),               25
     *(NAMES_PRSW(16),NPRSW_DXBBD)                                        25
      EQUIVALENCE (COMMEN_PRSW(16),CPRSW_DXBBD),(MAP_PRSW(1,16),          25
     *MPRSW_DXBBD)                                                        25
      CHARACTER CPRSW_DXBBD*40/' '/                                       25
      CHARACTER NPRSW_DXBBD*12/'R.DXBBD'/                                 25
      INTEGER MPRSW_DXBBD(2)/2,1/                                         25
      REAL RE_PRSW_DXBBD                                                  25
C -                                                                       25
      INTEGER LEN_PRSW,LLEN_PRSW,LINK_PRSW                                25
      PARAMETER (LEN_PRSW=44)                                             25
      PARAMETER (LLEN_PRSW=16)                                            25
      INTEGER FLAG_PRSW/0/,MAP_PRSW(2,LLEN_PRSW),IDNUM_PRSW(10)           25
      CHARACTER NAMES_PRSW(LLEN_PRSW)*12,COMMEN_PRSW(LLEN_PRSW)*40        25
      DATA IDNUM_PRSW/10*0/                                               25
      REAL BANK_PRSW(LEN_PRSW)                                            25
      CHARACTER*60 BPATH_PRSW/' '/                                        25
      SAVE FLAG_PRSW,MAP_PRSW,IDNUM_PRSW,NAMES_PRSW,COMMEN_PRSW,          25
     *BPATH_PRSW                                                          25
      DATA LINK_PRSW/0/,BANK_PRSW/LEN_PRSW*0/                             25
C *                                                                       25
C -                                                                       27
      REAL WPFM_POLED(4)                                                  27
      EQUIVALENCE (BANK_WPFM(3),WPFM_POLED,RE_WPFM_POLED),                27
     *(NAMES_WPFM(1),NWPFM_POLED)                                         27
      EQUIVALENCE (COMMEN_WPFM(1),CWPFM_POLED),(MAP_WPFM(1,1),            27
     *MWPFM_POLED)                                                        27
      CHARACTER CWPFM_POLED*40/' '/                                       27
      CHARACTER NWPFM_POLED*12/'R.POLED'/                                 27
      INTEGER MWPFM_POLED(2)/4,1/                                         27
      REAL RE_WPFM_POLED                                                  27
C -                                                                       27
      REAL WPFM_POLEP(3)                                                  27
      EQUIVALENCE (BANK_WPFM(7),WPFM_POLEP,RE_WPFM_POLEP),                27
     *(NAMES_WPFM(2),NWPFM_POLEP)                                         27
      EQUIVALENCE (COMMEN_WPFM(2),CWPFM_POLEP),(MAP_WPFM(1,2),            27
     *MWPFM_POLEP)                                                        27
      CHARACTER CWPFM_POLEP*40/' '/                                       27
      CHARACTER NWPFM_POLEP*12/'R.POLEP'/                                 27
      INTEGER MWPFM_POLEP(2)/3,1/                                         27
      REAL RE_WPFM_POLEP                                                  27
C -                                                                       27
      REAL WPFM_ISTRD(4)                                                  27
      EQUIVALENCE (BANK_WPFM(10),WPFM_ISTRD,RE_WPFM_ISTRD),               27
     *(NAMES_WPFM(3),NWPFM_ISTRD)                                         27
      EQUIVALENCE (COMMEN_WPFM(3),CWPFM_ISTRD),(MAP_WPFM(1,3),            27
     *MWPFM_ISTRD)                                                        27
      CHARACTER CWPFM_ISTRD*40/' '/                                       27
      CHARACTER NWPFM_ISTRD*12/'R.ISTRD'/                                 27
      INTEGER MWPFM_ISTRD(2)/4,1/                                         27
      REAL RE_WPFM_ISTRD                                                  27
C -                                                                       27
      REAL WPFM_IST1P(3)                                                  27
      EQUIVALENCE (BANK_WPFM(14),WPFM_IST1P,RE_WPFM_IST1P),               27
     *(NAMES_WPFM(4),NWPFM_IST1P)                                         27
      EQUIVALENCE (COMMEN_WPFM(4),CWPFM_IST1P),(MAP_WPFM(1,4),            27
     *MWPFM_IST1P)                                                        27
      CHARACTER CWPFM_IST1P*40/' '/                                       27
      CHARACTER NWPFM_IST1P*12/'R.IST1P'/                                 27
      INTEGER MWPFM_IST1P(2)/3,1/                                         27
      REAL RE_WPFM_IST1P                                                  27
C -                                                                       27
      REAL WPFM_IST2P(3)                                                  27
      EQUIVALENCE (BANK_WPFM(17),WPFM_IST2P,RE_WPFM_IST2P),               27
     *(NAMES_WPFM(5),NWPFM_IST2P)                                         27
      EQUIVALENCE (COMMEN_WPFM(5),CWPFM_IST2P),(MAP_WPFM(1,5),            27
     *MWPFM_IST2P)                                                        27
      CHARACTER CWPFM_IST2P*40/' '/                                       27
      CHARACTER NWPFM_IST2P*12/'R.IST2P'/                                 27
      INTEGER MWPFM_IST2P(2)/3,1/                                         27
      REAL RE_WPFM_IST2P                                                  27
C -                                                                       27
      REAL WPFM_USTFD(4)                                                  27
      EQUIVALENCE (BANK_WPFM(20),WPFM_USTFD,RE_WPFM_USTFD),               27
     *(NAMES_WPFM(6),NWPFM_USTFD)                                         27
      EQUIVALENCE (COMMEN_WPFM(6),CWPFM_USTFD),(MAP_WPFM(1,6),            27
     *MWPFM_USTFD)                                                        27
      CHARACTER CWPFM_USTFD*40/' '/                                       27
      CHARACTER NWPFM_USTFD*12/'R.USTFD'/                                 27
      INTEGER MWPFM_USTFD(2)/4,1/                                         27
      REAL RE_WPFM_USTFD                                                  27
C -                                                                       27
      REAL WPFM_USTFP(3)                                                  27
      EQUIVALENCE (BANK_WPFM(24),WPFM_USTFP,RE_WPFM_USTFP),               27
     *(NAMES_WPFM(7),NWPFM_USTFP)                                         27
      EQUIVALENCE (COMMEN_WPFM(7),CWPFM_USTFP),(MAP_WPFM(1,7),            27
     *MWPFM_USTFP)                                                        27
      CHARACTER CWPFM_USTFP*40/' '/                                       27
      CHARACTER NWPFM_USTFP*12/'R.USTFP'/                                 27
      INTEGER MWPFM_USTFP(2)/3,1/                                         27
      REAL RE_WPFM_USTFP                                                  27
C -                                                                       27
      REAL WPFM_USTND(4)                                                  27
      EQUIVALENCE (BANK_WPFM(27),WPFM_USTND,RE_WPFM_USTND),               27
     *(NAMES_WPFM(8),NWPFM_USTND)                                         27
      EQUIVALENCE (COMMEN_WPFM(8),CWPFM_USTND),(MAP_WPFM(1,8),            27
     *MWPFM_USTND)                                                        27
      CHARACTER CWPFM_USTND*40/' '/                                       27
      CHARACTER NWPFM_USTND*12/'R.USTND'/                                 27
      INTEGER MWPFM_USTND(2)/4,1/                                         27
      REAL RE_WPFM_USTND                                                  27
C -                                                                       27
      REAL WPFM_USTNP(3)                                                  27
      EQUIVALENCE (BANK_WPFM(31),WPFM_USTNP,RE_WPFM_USTNP),               27
     *(NAMES_WPFM(9),NWPFM_USTNP)                                         27
      EQUIVALENCE (COMMEN_WPFM(9),CWPFM_USTNP),(MAP_WPFM(1,9),            27
     *MWPFM_USTNP)                                                        27
      CHARACTER CWPFM_USTNP*40/' '/                                       27
      CHARACTER NWPFM_USTNP*12/'R.USTNP'/                                 27
      INTEGER MWPFM_USTNP(2)/3,1/                                         27
      REAL RE_WPFM_USTNP                                                  27
C -                                                                       27
      REAL WPFM_USTSD(4)                                                  27
      EQUIVALENCE (BANK_WPFM(34),WPFM_USTSD,RE_WPFM_USTSD),               27
     *(NAMES_WPFM(10),NWPFM_USTSD)                                        27
      EQUIVALENCE (COMMEN_WPFM(10),CWPFM_USTSD),(MAP_WPFM(1,10),          27
     *MWPFM_USTSD)                                                        27
      CHARACTER CWPFM_USTSD*40/' '/                                       27
      CHARACTER NWPFM_USTSD*12/'R.USTSD'/                                 27
      INTEGER MWPFM_USTSD(2)/4,1/                                         27
      REAL RE_WPFM_USTSD                                                  27
C -                                                                       27
      REAL WPFM_USTSP(3)                                                  27
      EQUIVALENCE (BANK_WPFM(38),WPFM_USTSP,RE_WPFM_USTSP),               27
     *(NAMES_WPFM(11),NWPFM_USTSP)                                        27
      EQUIVALENCE (COMMEN_WPFM(11),CWPFM_USTSP),(MAP_WPFM(1,11),          27
     *MWPFM_USTSP)                                                        27
      CHARACTER CWPFM_USTSP*40/' '/                                       27
      CHARACTER NWPFM_USTSP*12/'R.USTSP'/                                 27
      INTEGER MWPFM_USTSP(2)/3,1/                                         27
      REAL RE_WPFM_USTSP                                                  27
C -                                                                       27
      INTEGER LEN_WPFM,LLEN_WPFM,LINK_WPFM                                27
      PARAMETER (LEN_WPFM=40)                                             27
      PARAMETER (LLEN_WPFM=11)                                            27
      INTEGER FLAG_WPFM/0/,MAP_WPFM(2,LLEN_WPFM),IDNUM_WPFM(10)           27
      CHARACTER NAMES_WPFM(LLEN_WPFM)*12,COMMEN_WPFM(LLEN_WPFM)*40        27
      DATA IDNUM_WPFM/10*0/                                               27
      REAL BANK_WPFM(LEN_WPFM)                                            27
      CHARACTER*60 BPATH_WPFM/' '/                                        27
      SAVE FLAG_WPFM,MAP_WPFM,IDNUM_WPFM,NAMES_WPFM,COMMEN_WPFM,          27
     *BPATH_WPFM                                                          27
      DATA LINK_WPFM/0/,BANK_WPFM/LEN_WPFM*0/                             27
C *                                                                       27
      INTEGER CHKVSIM,IMOD,ITYPE,TYPE,PBPLATE                             28
      INTEGER I,J,K,KK,M,SERN,LR,TB,COL,KMOD,ROT,ID,LAYR,QUAD,SLAT(4,3),  31
     * DEBUG, NPSTYPE,PSTYPE(20),CPSTYPE(20),FLAG                         31
      REAL XX,YY,ZZ,X1,Y1,Z1,ZTOT,RTOT,WID,WIDX,WIDY,BWID,X0,WIDL,DX,DY,  35
     *DZ,DXX,DYY, DZZ,XXX,YYY,DXXX,DYYY,XPMT,YPMT, DX1,DY1,DZ1,DX2,DY2,   35
     *DZ2,X2,Y2,Z2,DZ3,DX1A, DX1B,DZSB,PSDIM(20,2)                        35
      REAL ZTOTSMD,WTOTSMD,ZSMD,ZSMD2,WSMD                                37
      REAL XSMDH,YSMDH,ZSMDH,XSMDV,YSMDV,ZSMDV                            39
      REAL XLCOFFSET,BZOFFSET                                             41
      REAL BASEWIDTH,DISTANCER,XOFFFECC,XOFFFEDC,XSHIFT                   43
      REAL XOFFFENC,YOFFFENC,ZOFFFENC,ZOFFFECC                            45
      REAL TMP(7)                                                         47
      INTEGER N                                                           49
      PARAMETER (N=12)                                                    51
      REAL E(N)                                                           52
      REAL RINDEX_PBG(N)                                                  54
      REAL RINDEX_SIRUB(N)                                                56
      REAL RINDEX_PHCATH(N)                                               58
      REAL RINDEX_ALM(N)                                                  60
      REAL RINDEX_MUMET(N)                                                62
      REAL ABSCO_PBG(N)                                                   64
      REAL ABSCO_SIRUB(N)                                                 66
      REAL ABSCO_PHCATH(N)                                                68
      REAL ABSCO_ALM(N)                                                   70
      REAL ABSCO_MUMET(N)                                                 72
      REAL EFFIC_PHCATH(N)                                                74
      REAL EFFIC_ALL(N)                                                   76
      REAL LPRS1,LPRS2,LPRS3,LPRS4,PWID,X,Y,Z,DLG                         78
      EXTERNAL FFPDSTEP                                                   80
      EXTERNAL FPCTSTEP                                                   81
C *                                                                       82
      CALL AGSBEGM(MODULE,IPRIN)                                          82
      IF (IPRIN.LT.0) RETURN                                              82
C Filling bank FMCG - FPD CALORIMETER BASIC DATA                          82
      BANK_TITLE='FPD CALORIMETER BASIC DATA'                             82
      BPATH_FMCG='FMCG*'                                                  82
      IDNUM_FMCG(1)=0                                                     83
C GEOMETRY VERSION                                                        83
      FMCG_VERSION=8.0                                                    83
      CFMCG_VERSION='GEOMETRY VERSION'                                    83
C = 0 DE, = 1 CHERENKOV SIMULATION FOR PBG                                84
      FMCG_CHKVSIM=0                                                      84
      CFMCG_CHKVSIM='= 0 DE, = 1 CHERENKOV SIMULATION FOR PBG'            84
C =0 NO PLATE, =1 WITH PLATE                                              85
      FMCG_PBPLATE=0                                                      85
      CFMCG_PBPLATE='=0 NO PLATE, =1 WITH PLATE'                          85
C DEFAULT X-POSITION                                                      86
      FMCG_FMSNORTHX=-0.3                                                 86
      CFMCG_FMSNORTHX='DEFAULT X-POSITION'                                86
C DEFAULT X-POSITION                                                      87
      FMCG_FMSSOUTHX=+0.3                                                 87
      CFMCG_FMSSOUTHX='DEFAULT X-POSITION'                                87
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    88
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           88
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   88
C *                                                                       88
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    88
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           88
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   88
C use FMCG bank                                                           89
      BPATH_FMCG='FMCG*'                                                  89
      IDNUM_FMCG(1)=0                                                     89
      FMCG_VERSION=8.0                                                    89
      CALL AGDGETP('FPDMGEO4','*',-999,'FMCG',NFMCG_VERSION,              89
     *RE_FMCG_VERSION,BPATH_FMCG,IDNUM_FMCG,1,MAP_FMCG,NAMES_FMCG,        89
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   89
C *                                                                       90
      CALL AGSBEGM(MODULE,IPRIN)                                          90
      IF (IPRIN.LT.0) RETURN                                              90
C Filling bank FPOS - FPD EN POSITIONING                                  90
      BANK_TITLE='FPD EN POSITIONING'                                     90
      BPATH_FPOS='FPOS*'                                                  90
      IDNUM_FPOS(1)=0                                                     91
C MODULE# (EN=1, ES=2, WN=3, WS=4, ...)                                   91
      FPOS_IMOD=1                                                         91
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WN=3, WS=4, ...)'                  91
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               92
      FPOS_ITYPE=1                                                        92
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             92
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                93
      FPOS_X=-48.19                                                       93
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  93
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                94
      FPOS_Y=0.0                                                          94
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  94
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               95
      FPOS_Z=-779.0                                                       95
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 95
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              96
      FPOS_AY=180                                                         96
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               96
C ANGLE AROUND Z                                                          97
      FPOS_AZ=0                                                           97
      CFPOS_AZ='ANGLE AROUND Z'                                           97
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
C Filling bank FPOS - FPD ES POSITIONING                                  99
      BANK_TITLE='FPD ES POSITIONING'                                     99
      BPATH_FPOS='FPOS*'                                                  99
      IDNUM_FPOS(1)=0                                                     100
C MODULE# (EN=1, ES=2, WS=3, WS=4, ...)                                   100
      FPOS_IMOD=2                                                         100
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WS=3, WS=4, ...)'                  100
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               101
      FPOS_ITYPE=1                                                        101
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             101
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                102
      FPOS_X=48.19                                                        102
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  102
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                103
      FPOS_Y=0.0                                                          103
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  103
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               104
      FPOS_Z=-779.0                                                       104
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 104
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              105
      FPOS_AY=180                                                         105
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               105
C ANGLE AROUND Z                                                          106
      FPOS_AZ=0                                                           106
      CFPOS_AZ='ANGLE AROUND Z'                                           106
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    107
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           107
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   107
C *                                                                       107
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    107
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           107
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   107
C *                                                                       108
      CALL AGSBEGM(MODULE,IPRIN)                                          108
      IF (IPRIN.LT.0) RETURN                                              108
C Filling bank FPOS - FMS WN POSITIONING                                  108
      BANK_TITLE='FMS WN POSITIONING'                                     108
      BPATH_FPOS='FPOS*'                                                  108
      IDNUM_FPOS(1)=0                                                     109
C MODULE# (EN=1, ES=2, WN=3, WS=4, ...)                                   109
      FPOS_IMOD=3                                                         109
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WN=3, WS=4, ...)'                  109
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               110
      FPOS_ITYPE=2                                                        110
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             110
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                111
      FPOS_X=FMCG_FMSNORTHX                                               111
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  111
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                112
      FPOS_Y=0.0                                                          112
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  112
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               113
      FPOS_Z=706.3                                                        113
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 113
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              114
      FPOS_AY=0                                                           114
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               114
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    115
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           115
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   115
C *                                                                       115
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    115
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           115
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   115
C *                                                                       116
      CALL AGSBEGM(MODULE,IPRIN)                                          116
      IF (IPRIN.LT.0) RETURN                                              116
C Filling bank FPOS - FMS WS POSITIONING                                  116
      BANK_TITLE='FMS WS POSITIONING'                                     116
      BPATH_FPOS='FPOS*'                                                  116
      IDNUM_FPOS(1)=0                                                     117
C MODULE# (EN=1, ES=2, WN=3, WS=4, ...)                                   117
      FPOS_IMOD=4                                                         117
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WN=3, WS=4, ...)'                  117
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               118
      FPOS_ITYPE=2                                                        118
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             118
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                119
      FPOS_X=FMCG_FMSSOUTHX                                               119
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  119
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                120
      FPOS_Y=0.0                                                          120
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  120
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               121
      FPOS_Z=706.3                                                        121
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 121
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              122
      FPOS_AY=0                                                           122
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               122
C ANGLE AROUND Z                                                          123
      FPOS_AZ=0                                                           123
      CFPOS_AZ='ANGLE AROUND Z'                                           123
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    124
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           124
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   124
C *                                                                       124
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    124
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           124
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   124
C *                                                                       125
      CALL AGSBEGM(MODULE,IPRIN)                                          125
      IF (IPRIN.LT.0) RETURN                                              125
C Filling bank FBXD - FPD BOX GEOMETRY                                    125
      BANK_TITLE='FPD BOX GEOMETRY'                                       125
      BPATH_FBXD='FBXD*'                                                  125
      IDNUM_FBXD(1)=0                                                     126
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               126
      FBXD_TYPE=1                                                         126
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'              126
C BOX HEIGHT                                                              127
      FBXD_HEIGHT=100                                                     127
      CFBXD_HEIGHT='BOX HEIGHT'                                           127
C BOX DEPTH                                                               128
      FBXD_DEPTH=96                                                       128
      CFBXD_DEPTH='BOX DEPTH'                                             128
C BOX WIDTH (ONLY FOR FMS)                                                129
      FBXD_WIDTH=0.0                                                      129
      CFBXD_WIDTH='BOX WIDTH (ONLY FOR FMS)'                              129
C NUMBER OF PBG IN X                                                      130
      FBXD_NX=7                                                           130
      CFBXD_NX='NUMBER OF PBG IN X'                                       130
C NUMBER OF PBG IN Y                                                      131
      FBXD_NY=7                                                           131
      CFBXD_NY='NUMBER OF PBG IN Y'                                       131
C NUMBER OF LARGE PBG IN X                                                132
      FBXD_NXL=0                                                          132
      CFBXD_NXL='NUMBER OF LARGE PBG IN X'                                132
C NUMBER OF LARGE PBG IN Y                                                133
      FBXD_NYL=0                                                          133
      CFBXD_NYL='NUMBER OF LARGE PBG IN Y'                                133
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                134
      FBXD_XOFFSET=2.54                                                   134
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            134
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                135
      FBXD_ZOFFSET=19                                                     135
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            135
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            136
      FBXD_PSOFFSET=2.0                                                   136
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       136
C SMD V-PLANE Z OFFSET FROM BOX EDGE                                      137
      FBXD_SMDOFF=8.0                                                     137
      CFBXD_SMDOFF='SMD V-PLANE Z OFFSET FROM BOX EDGE'                   137
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    138
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           138
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   138
C *                                                                       138
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    138
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           138
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   138
C *                                                                       139
      CALL AGSBEGM(MODULE,IPRIN)                                          139
      IF (IPRIN.LT.0) RETURN                                              139
C Filling bank FBXD - FPD BOX GEOMETRY                                    139
      BANK_TITLE='FPD BOX GEOMETRY'                                       139
      BPATH_FBXD='FBXD*'                                                  139
      IDNUM_FBXD(1)=0                                                     140
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               140
      FBXD_TYPE=2                                                         140
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'              140
C BOX HEIGHT                                                              141
      FBXD_HEIGHT=212                                                     141
      CFBXD_HEIGHT='BOX HEIGHT'                                           141
C BOX DEPTH                                                               142
      FBXD_DEPTH=98.425                                                   142
      CFBXD_DEPTH='BOX DEPTH'                                             142
C BOX WIDTH (ONLY FOR FMS)                                                143
      FBXD_WIDTH=127.0                                                    143
      CFBXD_WIDTH='BOX WIDTH (ONLY FOR FMS)'                              143
C NUMBER OF PBG IN X                                                      144
      FBXD_NX=12                                                          144
      CFBXD_NX='NUMBER OF PBG IN X'                                       144
C NUMBER OF PBG IN Y                                                      145
      FBXD_NY=24                                                          145
      CFBXD_NY='NUMBER OF PBG IN Y'                                       145
C NUMBER OF LARGE PBG IN X                                                146
      FBXD_NXL=17                                                         146
      CFBXD_NXL='NUMBER OF LARGE PBG IN X'                                146
C NUMBER OF LARGE PBG IN Y                                                147
      FBXD_NYL=34                                                         147
      CFBXD_NYL='NUMBER OF LARGE PBG IN Y'                                147
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                148
      FBXD_XOFFSET=(6*3.822+0.5*5.812)+(127.0-17*5.812)/2.0               148
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            148
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                149
      FBXD_ZOFFSET=10.4                                                   149
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            149
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            150
      FBXD_PSOFFSET=0                                                     150
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       150
C SMD Z OFFSET FROM BOX EDGE                                              151
      FBXD_SMDOFF=0.0                                                     151
      CFBXD_SMDOFF='SMD Z OFFSET FROM BOX EDGE'                           151
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    152
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           152
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   152
C *                                                                       152
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    152
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           152
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   152
C *                                                                       153
      CALL AGSBEGM(MODULE,IPRIN)                                          153
      IF (IPRIN.LT.0) RETURN                                              153
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               153
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  153
      BPATH_FLGG='FLGG*'                                                  153
      IDNUM_FLGG(1)=0                                                     154
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    154
      FLGG_TYPE=1                                                         154
      CFLGG_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   154
C PBG WIDTH                                                               155
      FLGG_WIDTH=3.81                                                     155
      CFLGG_WIDTH='PBG WIDTH'                                             155
C PBG DEPTH                                                               156
      FLGG_DEPTH=45.0                                                     156
      CFLGG_DEPTH='PBG DEPTH'                                             156
C GAP BETWEEN PBG                                                         157
      FLGG_DGAP=0.01                                                      157
      CFLGG_DGAP='GAP BETWEEN PBG'                                        157
C ALMUNIM WRAP THICKNESS (REAL)                                           158
      FLGG_ALTHICK=0.001                                                  158
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       158
C PHOTO CATHODE THICKNESS                                                 159
      FLGG_PHCATHDZ=2.0                                                   159
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            159
C PHOTO CATHODE RADIUS (REAL)                                             160
      FLGG_PHCATHR=1.8                                                    160
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         160
C MU METAL LENGTH                                                         161
      FLGG_MUMETDZ=11.0                                                   161
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     161
C MU METAL OUTER RADIUS (REAL)                                            162
      FLGG_MUMETR=1.9                                                     162
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         162
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    163
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           163
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   163
C *                                                                       163
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    163
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           163
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   163
C *                                                                       164
      CALL AGSBEGM(MODULE,IPRIN)                                          164
      IF (IPRIN.LT.0) RETURN                                              164
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               164
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  164
      BPATH_FLGG='FLGG*'                                                  164
      IDNUM_FLGG(1)=0                                                     165
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    165
      FLGG_TYPE=2                                                         165
      CFLGG_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   165
C PBG WIDTH                                                               166
      FLGG_WIDTH=5.8                                                      166
      CFLGG_WIDTH='PBG WIDTH'                                             166
C PBG DEPTH                                                               167
      FLGG_DEPTH=60.2                                                     167
      CFLGG_DEPTH='PBG DEPTH'                                             167
C GAP BETWEEN PBG                                                         168
      FLGG_DGAP=0.01                                                      168
      CFLGG_DGAP='GAP BETWEEN PBG'                                        168
C ALMUNIM WRAP THICKNESS (REAL)                                           169
      FLGG_ALTHICK=0.001                                                  169
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       169
C PHOTO CATHODE THICKNESS                                                 170
      FLGG_PHCATHDZ=2.0                                                   170
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            170
C PHOTO CATHODE RADIUS (REAL)                                             171
      FLGG_PHCATHR=1.8                                                    171
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         171
C MU METAL LENGTH                                                         172
      FLGG_MUMETDZ=11.0                                                   172
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     172
C MU METAL OUTER RADIUS (REAL)                                            173
      FLGG_MUMETR=1.9                                                     173
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         173
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    174
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           174
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   174
C *                                                                       174
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    174
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           174
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   174
C *                                                                       175
      CALL AGSBEGM(MODULE,IPRIN)                                          175
      IF (IPRIN.LT.0) RETURN                                              175
C Filling bank FLGM - PBG DETECTOR MATERIALS                              175
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 175
      BPATH_FLGM='FLGM*'                                                  175
      IDNUM_FLGM(1)=0                                                     176
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    176
      FLGM_TYPE=1                                                         176
      CFLGM_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   176
C GDENSITY [/CM^3]                                                        177
      FLGM_DENSITY=3.86                                                   177
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    177
C RADIATION LENGTH [CM]                                                   178
      FLGM_RADLEN=2.5                                                     178
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                178
C PBO CONTENT [%]                                                         179
      FLGM_PBCONT=65.4                                                    179
      CFLGM_PBCONT='PBO CONTENT [%]'                                      179
C CRITICAL ENERGY [GEV]                                                   180
      FLGM_CRITENE=0.0158                                                 180
      CFLGM_CRITENE='CRITICAL ENERGY [GEV]'                               180
C MOLIERE RADIUS [CM]                                                     181
      FLGM_MOLIERER=3.32                                                  181
      CFLGM_MOLIERER='MOLIERE RADIUS [CM]'                                181
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    182
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           182
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   182
C *                                                                       182
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    182
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           182
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   182
C *                                                                       183
      CALL AGSBEGM(MODULE,IPRIN)                                          183
      IF (IPRIN.LT.0) RETURN                                              183
C Filling bank FLGM - PBG DETECTOR MATERIALS                              183
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 183
      BPATH_FLGM='FLGM*'                                                  183
      IDNUM_FLGM(1)=0                                                     184
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    184
      FLGM_TYPE=2                                                         184
      CFLGM_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   184
C GDENSITY [/CM^3]                                                        185
      FLGM_DENSITY=3.61                                                   185
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    185
C RADIATION LENGTH [CM]                                                   186
      FLGM_RADLEN=3.21                                                    186
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                186
C PBO CONTENT [%]                                                         187
      FLGM_PBCONT=45.0                                                    187
      CFLGM_PBCONT='PBO CONTENT [%]'                                      187
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    188
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           188
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   188
C *                                                                       188
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    188
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           188
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   188
C *                                                                       189
      CALL AGSBEGM(MODULE,IPRIN)                                          189
      IF (IPRIN.LT.0) RETURN                                              189
C Filling bank PBPD - PB PLATE DIMENSIONS                                 189
      BANK_TITLE='PB PLATE DIMENSIONS'                                    189
      BPATH_PBPD='PBPD*'                                                  189
      IDNUM_PBPD(1)=0                                                     190
C WIDTH                                                                   190
      PBPD_WIDTH=33.02                                                    190
      CPBPD_WIDTH='WIDTH'                                                 190
C HEIGHT                                                                  191
      PBPD_HEIGHT=33.02                                                   191
      CPBPD_HEIGHT='HEIGHT'                                               191
C THICKNESS                                                               192
      PBPD_THICK=1.27                                                     192
      CPBPD_THICK='THICKNESS'                                             192
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    193
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           193
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   193
C *                                                                       193
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    193
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           193
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   193
C *                                                                       194
      CALL AGSBEGM(MODULE,IPRIN)                                          194
      IF (IPRIN.LT.0) RETURN                                              194
C Filling bank FMXG - SMD GEOMETRY                                        194
      BANK_TITLE='SMD GEOMETRY'                                           194
      BPATH_FMXG='FMXG*'                                                  194
      IDNUM_FMXG(1)=0                                                     195
C GEOMETRY VERSION                                                        195
      FMXG_VERSION=2                                                      195
      CFMXG_VERSION='GEOMETRY VERSION'                                    195
C SCINTILLATOR STRIP APEX                                                 196
      FMXG_SAPEX=0.7                                                      196
      CFMXG_SAPEX='SCINTILLATOR STRIP APEX'                               196
C SCINTILLATOR STRIP BASE                                                 197
      FMXG_SBASE=1.0                                                      197
      CFMXG_SBASE='SCINTILLATOR STRIP BASE'                               197
C GAP BETWEEN STRIPS                                                      198
      FMXG_SGAP=0.0064                                                    198
      CFMXG_SGAP='GAP BETWEEN STRIPS'                                     198
C # OF STRIPS                                                             199
      FMXG_NSTRIP=50                                                      199
      CFMXG_NSTRIP='# OF STRIPS'                                          199
C G10 PLATE WIDTH                                                         200
      FMXG_G10WIDTH=27.0                                                  200
      CFMXG_G10WIDTH='G10 PLATE WIDTH'                                    200
C G10 PLATE HEIGHT                                                        201
      FMXG_G10HGT=27.0                                                    201
      CFMXG_G10HGT='G10 PLATE HEIGHT'                                     201
C G10 PLATE THICKNESS                                                     202
      FMXG_G10THICK=0.15                                                  202
      CFMXG_G10THICK='G10 PLATE THICKNESS'                                202
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    203
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           203
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   203
C *                                                                       203
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    203
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           203
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   203
C *                                                                       204
      CALL AGSBEGM(MODULE,IPRIN)                                          204
      IF (IPRIN.LT.0) RETURN                                              204
C Filling bank INSE - INSERT GEOMETRY                                     204
      BANK_TITLE='INSERT GEOMETRY'                                        204
      BPATH_INSE='INSE*'                                                  204
      IDNUM_INSE(1)=0                                                     205
C WIDTH OF THE INSERT (X)                                                 205
      INSE_WIDTH=19.30908                                                 205
      CINSE_WIDTH='WIDTH OF THE INSERT (X)'                               205
C DEPTH OF THE INSERT (Z)                                                 206
      INSE_DEPTH=98.425                                                   206
      CINSE_DEPTH='DEPTH OF THE INSERT (Z)'                               206
C HEIGHT OF THE INSERT (Y)                                                207
      INSE_HEIGHT=38.608                                                  207
      CINSE_HEIGHT='HEIGHT OF THE INSERT (Y)'                             207
C DEPTH OF THE STEEL PARTS (X,Y)                                          208
      INSE_SHEETDPT=1.27                                                  208
      CINSE_SHEETDPT='DEPTH OF THE STEEL PARTS (X,Y)'                     208
C DISTANCE BETWEEN EDGE OF INSERT AND SQUARE HOLE                         209
      INSE_HOLEGAP=5.08                                                   209
      CINSE_HOLEGAP='DISTANCE BETWEEN EDGE OF INSERT AND SQUARE HOLE'     209
C DEPTH OF THE SQUARE HOLE (Z)                                            210
      INSE_HOLEDEPTH=25.4                                                 210
      CINSE_HOLEDEPTH='DEPTH OF THE SQUARE HOLE (Z)'                      210
C HEIGHT OF THE SQUARE HOLE (Y)                                           211
      INSE_HOLEHEIGHT=30.48                                               211
      CINSE_HOLEHEIGHT='HEIGHT OF THE SQUARE HOLE (Y)'                    211
C DEPTH OF THE IRON DISTANCER (Z)                                         212
      INSE_GAPDEPTH=19.685                                                212
      CINSE_GAPDEPTH='DEPTH OF THE IRON DISTANCER (Z)'                    212
C HEIGHT OF THE IRON DISTANCER (Y)                                        213
      INSE_GAPHEIGHT=7.874                                                213
      CINSE_GAPHEIGHT='HEIGHT OF THE IRON DISTANCER (Y)'                  213
C DEPTH OF ONE OF THE THREE GATES (Z)                                     214
      INSE_GATEDEPTH=1.905                                                214
      CINSE_GATEDEPTH='DEPTH OF ONE OF THE THREE GATES (Z)'               214
C DISTANCE BETWEEN THE BACK EDGE OF THE BOX AND LAST GATE                 215
      INSE_GATEGAP=12.7                                                   215
      CINSE_GATEGAP='DISTANCE BETWEEN THE BACK EDGE OF THE BOX AND LAST   215
     *GATE'                                                               215
C RADIUS OF THE INNER CIRCLE OF TUBES                                     216
      INSE_RA=13.97                                                       216
      CINSE_RA='RADIUS OF THE INNER CIRCLE OF TUBES'                      216
C RADIUS OF THE OUTER CIRCLE OF TUBES                                     217
      INSE_RB=20.6375                                                     217
      CINSE_RB='RADIUS OF THE OUTER CIRCLE OF TUBES'                      217
C DIAMETER OF THE TUBES                                                   218
      INSE_DIAM=6.0325                                                    218
      CINSE_DIAM='DIAMETER OF THE TUBES'                                  218
C RADIUS OF THE INNER TUBE FOR BEAMPIPE                                   219
      INSE_RMAX=10.16                                                     219
      CINSE_RMAX='RADIUS OF THE INNER TUBE FOR BEAMPIPE'                  219
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'INSE',    220
     *BPATH_INSE,IDNUM_INSE,1, MAP_INSE,NAMES_INSE,COMMEN_INSE,           220
     *BANK_INSE,LEN_INSE,LLEN_INSE,LINK_INSE,FLAG_INSE)                   220
C *                                                                       220
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'INSE',    220
     *BPATH_INSE,IDNUM_INSE,1, MAP_INSE,NAMES_INSE,COMMEN_INSE,           220
     *BANK_INSE,LEN_INSE,LLEN_INSE,LINK_INSE,FLAG_INSE)                   220
C *                                                                       221
      CALL AGSBEGM(MODULE,IPRIN)                                          221
      IF (IPRIN.LT.0) RETURN                                              221
C Filling bank PRSW - PRE SHOWER GEOMETRY                                 221
      BANK_TITLE='PRE SHOWER GEOMETRY'                                    221
      BPATH_PRSW='PRSW*'                                                  221
      IDNUM_PRSW(1)=0                                                     222
C NONE                                                                    222
      PRSW_DBOX(1)=225.0                                                  222
      PRSW_DBOX(2)=250                                                    222
      PRSW_DBOX(3)=22.0                                                   222
      CPRSW_DBOX='NONE'                                                   222
C NONE                                                                    223
      PRSW_XOFF(1)=1.0                                                    223
      PRSW_XOFF(2)=1.0                                                    223
      PRSW_XOFF(3)=1.0                                                    223
      PRSW_XOFF(4)=1.0                                                    223
      CPRSW_XOFF='NONE'                                                   223
C NONE                                                                    224
      PRSW_YOFF(1)=0.0                                                    224
      PRSW_YOFF(2)=0.0                                                    224
      PRSW_YOFF(3)=0.0                                                    224
      PRSW_YOFF(4)=0.0                                                    224
      CPRSW_YOFF='NONE'                                                   224
C NONE                                                                    225
      PRSW_ZOFF(1)=675.64                                                 225
      PRSW_ZOFF(2)=678.52                                                 225
      PRSW_ZOFF(3)=690.29                                                 225
      PRSW_ZOFF(4)=692.22                                                 225
      CPRSW_ZOFF='NONE'                                                   225
C NONE                                                                    226
      PRSW_DZ(1)=1.0                                                      226
      PRSW_DZ(2)=1.0                                                      226
      PRSW_DZ(3)=0.635                                                    226
      PRSW_DZ(4)=1.0                                                      226
      CPRSW_DZ='NONE'                                                     226
C NONE                                                                    227
      PRSW_DHOLE=20.25                                                    227
      CPRSW_DHOLE='NONE'                                                  227
C NONE                                                                    228
      PRSW_NTYPE=2                                                        228
      CPRSW_NTYPE='NONE'                                                  228
C NONE                                                                    229
      PRSW_NPRS(1)=12                                                     229
      PRSW_NPRS(2)=9                                                      229
      CPRSW_NPRS='NONE'                                                   229
C NONE                                                                    230
      PRSW_DPRS(1)=4.0                                                    230
      PRSW_DPRS(2)=5.8                                                    230
      PRSW_DPRS(3)=0.05                                                   230
      CPRSW_DPRS='NONE'                                                   230
C NONE                                                                    231
      PRSW_NSKIPV=2                                                       231
      CPRSW_NSKIPV='NONE'                                                 231
C NONE                                                                    232
      PRSW_NSKIPH=2                                                       232
      CPRSW_NSKIPH='NONE'                                                 232
C NONE                                                                    233
      PRSW_DYCUT=3.0+5.8                                                  233
      CPRSW_DYCUT='NONE'                                                  233
C NONE                                                                    234
      PRSW_DLG(1)=0.3                                                     234
      PRSW_DLG(2)=6.0                                                     234
      PRSW_DLG(3)=1.0                                                     234
      CPRSW_DLG='NONE'                                                    234
C NONE                                                                    235
      PRSW_DSIPM(1)=0.5                                                   235
      PRSW_DSIPM(2)=3.5                                                   235
      PRSW_DSIPM(3)=1.0                                                   235
      PRSW_DSIPM(4)=7.0                                                   235
      CPRSW_DSIPM='NONE'                                                  235
C NONE                                                                    237
      PRSW_DZBBD(1)=2.54/8.0                                              237
      PRSW_DZBBD(2)=2.54/4.0                                              237
      PRSW_DZBBD(3)=2*2.54/4.0                                            237
      PRSW_DZBBD(4)=2.54/4.0                                              237
      CPRSW_DZBBD='NONE'                                                  237
C NONE                                                                    238
      PRSW_DXBBD(1)=2.54                                                  238
      PRSW_DXBBD(2)=2.54+7.0                                              238
      CPRSW_DXBBD='NONE'                                                  238
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PRSW',    239
     *BPATH_PRSW,IDNUM_PRSW,1, MAP_PRSW,NAMES_PRSW,COMMEN_PRSW,           239
     *BANK_PRSW,LEN_PRSW,LLEN_PRSW,LINK_PRSW,FLAG_PRSW)                   239
C *                                                                       239
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PRSW',    239
     *BPATH_PRSW,IDNUM_PRSW,1, MAP_PRSW,NAMES_PRSW,COMMEN_PRSW,           239
     *BANK_PRSW,LEN_PRSW,LLEN_PRSW,LINK_PRSW,FLAG_PRSW)                   239
C *                                                                       240
      CALL AGSBEGM(MODULE,IPRIN)                                          240
      IF (IPRIN.LT.0) RETURN                                              240
C Filling bank WPFM - WEST PLATFORM GEOMETRY                              240
      BANK_TITLE='WEST PLATFORM GEOMETRY'                                 240
      BPATH_WPFM='WPFM*'                                                  240
      IDNUM_WPFM(1)=0                                                     242
C NONE                                                                    242
      WPFM_POLED(1)=12.0*2.54                                             242
      WPFM_POLED(2)=155.5 * 2.54                                          242
      WPFM_POLED(3)=12.0*2.54                                             242
      WPFM_POLED(4)=1.0                                                   242
      CWPFM_POLED='NONE'                                                  242
C NONE                                                                    245
      WPFM_POLEP(1)=72.0*2.54                                             245
      WPFM_POLEP(2)=(-44.36-10-155.5/2)*2.54                              245
      WPFM_POLEP(3)=807.72-27*2.54                                        245
      CWPFM_POLEP='NONE'                                                  245
C NONE                                                                    247
      WPFM_ISTRD(1)=231.0*2.54                                            247
      WPFM_ISTRD(2)=8.0*2.54                                              247
      WPFM_ISTRD(3)=8.0*2.54                                              247
      WPFM_ISTRD(4)=1.0                                                   247
      CWPFM_ISTRD='NONE'                                                  247
C NONE                                                                    249
      WPFM_IST1P(1)=-8.0*2.54                                             249
      WPFM_IST1P(2)=(-44.36-5.0)*2.54                                     249
      WPFM_IST1P(3)=807.72-37*2.54                                        249
      CWPFM_IST1P='NONE'                                                  249
C NONE                                                                    251
      WPFM_IST2P(1)=-8.0*2.54                                             251
      WPFM_IST2P(2)=(-44.36-5.0)*2.54                                     251
      WPFM_IST2P(3)=807.72-17*2.54                                        251
      CWPFM_IST2P='NONE'                                                  251
C NONE                                                                    253
      WPFM_USTFD(1)=236.0*2.54                                            253
      WPFM_USTFD(2)=12*2.54                                               253
      WPFM_USTFD(3)=4*2.54                                                253
      WPFM_USTFD(4)=1.0                                                   253
      CWPFM_USTFD='NONE'                                                  253
C NONE                                                                    255
      WPFM_USTFP(1)=-8.0*2.54                                             255
      WPFM_USTFP(2)=-44.36*2.54                                           255
      WPFM_USTFP(3)=807.72-(46-2)*2.54                                    255
      CWPFM_USTFP='NONE'                                                  255
C NONE                                                                    257
      WPFM_USTND(1)=4*2.54                                                257
      WPFM_USTND(2)=12.0*2.54                                             257
      WPFM_USTND(3)=(46-4)*2.54                                           257
      WPFM_USTND(4)=1.0                                                   257
      CWPFM_USTND='NONE'                                                  257
C NONE                                                                    259
      WPFM_USTNP(1)=-(118-2+8)*2.54                                       259
      WPFM_USTNP(2)=-44.36*2.54                                           259
      WPFM_USTNP(3)=807.72-(46-4)*2.54/2                                  259
      CWPFM_USTNP='NONE'                                                  259
C NONE                                                                    261
      WPFM_USTSD(1)=4*2.54                                                261
      WPFM_USTSD(2)=12.0*2.54                                             261
      WPFM_USTSD(3)=(46-4)*2.54                                           261
      WPFM_USTSD(4)=1.0                                                   261
      CWPFM_USTSD='NONE'                                                  261
C NONE                                                                    263
      WPFM_USTSP(1)=+(118-2-8)*2.54                                       263
      WPFM_USTSP(2)=-44.36*2.54                                           263
      WPFM_USTSP(3)=807.72-(46-4)*2.54/2                                  263
      CWPFM_USTSP='NONE'                                                  263
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'WPFM',    264
     *BPATH_WPFM,IDNUM_WPFM,1, MAP_WPFM,NAMES_WPFM,COMMEN_WPFM,           264
     *BANK_WPFM,LEN_WPFM,LLEN_WPFM,LINK_WPFM,FLAG_WPFM)                   264
C *                                                                       264
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'WPFM',    264
     *BPATH_WPFM,IDNUM_WPFM,1, MAP_WPFM,NAMES_WPFM,COMMEN_WPFM,           264
     *BANK_WPFM,LEN_WPFM,LLEN_WPFM,LINK_WPFM,FLAG_WPFM)                   264
C use default FMCG bank                                                   265
      BPATH_FMCG='FMCG*'                                                  265
      IDNUM_FMCG(1)=0                                                     265
      CALL AGDGETP('FPDMGEO4','*',-999,'FMCG',' ',BANK_FMCG, BPATH_FMCG,  265
     *IDNUM_FMCG,1,MAP_FMCG,NAMES_FMCG,BANK_FMCG,LEN_FMCG,LLEN_FMCG,      265
     *LINK_FMCG,FLAG_FMCG)                                                265
C Check IPRIN>=1                                                          265
      IF (IPRIN.GE.1) WRITE(LOUT,5010)FMCG_VERSION                        265
5010  FORMAT('****************** FPDMGEO version ', F4.2)                 267
C *                                                                       268
      DO 5021 M=1,4                                                       268
C    use FPOS bank                                                        269
         BPATH_FPOS='FPOS*'                                               269
         IDNUM_FPOS(1)=0                                                  269
         FPOS_IMOD=M                                                      269
         CALL AGDGETP('FPDMGEO4','*',-999,'FPOS',NFPOS_IMOD,              269
     *   RE_FPOS_IMOD,BPATH_FPOS,IDNUM_FPOS,1,MAP_FPOS,NAMES_FPOS,        269
     *   BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                269
C    use FBXD bank                                                        270
         BPATH_FBXD='FBXD*'                                               270
         IDNUM_FBXD(1)=0                                                  270
         FBXD_TYPE=FPOS_ITYPE                                             270
         CALL AGDGETP('FPDMGEO4','*',-999,'FBXD',NFBXD_TYPE,              270
     *   RE_FBXD_TYPE,BPATH_FBXD,IDNUM_FBXD,1,MAP_FBXD,NAMES_FBXD,        270
     *   BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                270
C    Check FBXD_TYPE.EQ.1                                                 271
         IF (FBXD_TYPE.EQ.1) THEN                                         271
C    use FLGG bank                                                        272
         BPATH_FLGG='FLGG*'                                               272
         IDNUM_FLGG(1)=0                                                  272
         FLGG_TYPE=1                                                      272
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGG',NFLGG_TYPE,              272
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        272
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                272
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  273
         ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0            274
         RTOT = FBXD_NX*WID/2.0                                           275
         BWID = RTOT+FBXD_XOFFSET                                         276
         ELSE                                                             277
C    use FLGG bank                                                        278
         BPATH_FLGG='FLGG*'                                               278
         IDNUM_FLGG(1)=0                                                  278
         FLGG_TYPE=2                                                      278
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGG',NFLGG_TYPE,              278
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        278
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                278
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  279
         ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0            280
         RTOT = FBXD_NXL*WID/2.0                                          281
         BWID = RTOT                                                      282
         ENDIF                                                            283
C    Check M.GE.3                                                         284
         IF (M.GE.3) THEN                                                 284
         BWID=FBXD_WIDTH/2.0                                              285
C    Check FPOS_X.GT.0.0                                                  286
         IF (FPOS_X.GT.0.0) THEN                                          286
         XX=FPOS_X+BWID                                                   287
         ELSEIF (FPOS_X.EQ.0.0) THEN                                      288
         XX=0.0                                                           289
         ELSE                                                             290
         XX=FPOS_X-BWID                                                   291
         ENDIF                                                            292
         ELSE                                                             293
C    Check FPOS_X.GT.0.0                                                  294
         IF (FPOS_X.GT.0.0) THEN                                          294
         XX=FPOS_X+BWID                                                   295
         ELSEIF (FPOS_X.EQ.0.0) THEN                                      296
         XX=0.0                                                           297
         ELSE                                                             298
         XX=FPOS_X-BWID                                                   299
         ENDIF                                                            300
         ENDIF                                                            301
C    Check FPOS_Y.GT.0.0                                                  302
         IF (FPOS_Y.GT.0.0) THEN                                          302
         YY=FPOS_Y+FBXD_HEIGHT/2.0                                        303
         ELSEIF (FPOS_Y.EQ.0.0) THEN                                      304
         YY=0.0                                                           305
         ELSE                                                             306
         YY=FPOS_Y-FBXD_HEIGHT/2.0                                        307
         ENDIF                                                            308
C    Check FPOS_Z.GT.0.0                                                  309
         IF (FPOS_Z.GT.0.0) THEN                                          309
         ZZ=FPOS_Z+FBXD_DEPTH/2.0                                         310
         ELSE                                                             311
         ZZ=FPOS_Z-FBXD_DEPTH/2.0                                         312
         ENDIF                                                            313
         SERN=0                                                           314
C    Check M.EQ.4                                                         315
         IF (M.EQ.4) THEN                                                 315
         SERN=1                                                           316
         ENDIF                                                            317
C    Check M.NE.7                                                         318
         IF (M.NE.7) THEN                                                 318
         CALL AGSBEGM(MODULE,IPRIN)                                       319
C    Check IPRIN<0                                                        319
         IF (IPRIN.LT.0) RETURN                                           319
C    Create FBOX :                                                        319
C    Call To Local Procedure FBOX                                         319
         ASSIGN 5040 TO BLOCK_FBOX                                        319
         GO TO  5030                                                      319
5040     CONTINUE                                                         319
C    *                                                                    319
C    *                                                                    320
C       Check ZZ>0                                                        320
            IF (ZZ.GT.0) THEN                                             320
C       Geant POSITION : FBOX ===> MOTHER='CAVE' X=XX Y=YY Z=ZZ ALPHAY=F  321
C       POS_AY KONLY='MANY' <===                                          321
            CALL AGSRESET                                                 321
            AG_MOTHER = 'CAVE'                                            321
            AG_X = XX                                                     321
            AG_Y = YY                                                     321
            AG_Z = ZZ                                                     321
            AG_ALPHAN=AG_ALPHAN+1                                         321
            AG_ALPHAK(AG_ALPHAN)=2                                        321
            AG_ALPHAL(AG_ALPHAN)=FPOS_AY                                  321
            AG_KONLY = 'MANY'                                             321
            AG_TITLE = 'FBOX'                                             321
            AG_EXNAME = 'POSITION'                                        321
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_KONLY'                      321
            CALL AXPOSITION                                               321
C       *                                                                 321
         END IF                                                           322
C    *                                                                    323
C       Check ZZ<0                                                        323
            IF (ZZ.LT.0) THEN                                             323
C       Geant POSITION : FBOX ===> MOTHER='CAVE' X=XX Y=YY Z=ZZ ALPHAY=F  324
C       POS_AY KONLY='MANY' <===                                          324
            CALL AGSRESET                                                 324
            AG_MOTHER = 'CAVE'                                            324
            AG_X = XX                                                     324
            AG_Y = YY                                                     324
            AG_Z = ZZ                                                     324
            AG_ALPHAN=AG_ALPHAN+1                                         324
            AG_ALPHAK(AG_ALPHAN)=2                                        324
            AG_ALPHAL(AG_ALPHAN)=FPOS_AY                                  324
            AG_KONLY = 'MANY'                                             324
            AG_TITLE = 'FBOX'                                             324
            AG_EXNAME = 'POSITION'                                        324
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_KONLY'                      324
            CALL AXPOSITION                                               324
C       *                                                                 324
         END IF                                                           325
         ENDIF                                                            326
5021  CONTINUE                                                            327
5022  CONTINUE                                                            327
      CALL AGSBEGM(MODULE,IPRIN)                                          328
C Check IPRIN<0                                                           328
      IF (IPRIN.LT.0) RETURN                                              328
C Create FPRS :                                                           328
C Call To Local Procedure FPRS                                            328
      ASSIGN 5060 TO BLOCK_FPRS                                           328
      GO TO  5050                                                         328
5060  CONTINUE                                                            328
C *                                                                       328
C Geant POSITION : FPRS ===> MOTHER='CAVE' X=0.0 Y=0.0 Z=PRSW_ZOFF(1)-PR  330
C SW_DSIPM(1)+PRSW_DBOX(3)/2.0 KONLY='MANY' <===                          330
      CALL AGSRESET                                                       330
      AG_MOTHER = 'CAVE'                                                  330
      AG_X = 0.0                                                          330
      AG_Y = 0.0                                                          330
      AG_Z = PRSW_ZOFF(1)-PRSW_DSIPM(1)+PRSW_DBOX(3)/2.0                  330
      AG_KONLY = 'MANY'                                                   330
      AG_TITLE = 'FPRS'                                                   330
      AG_EXNAME = 'POSITION'                                              330
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   330
      CALL AXPOSITION                                                     330
C *                                                                       330
      CALL AGSBEGM(MODULE,IPRIN)                                          330
C Check IPRIN<0                                                           331
      IF (IPRIN.LT.0) RETURN                                              331
C Create WPPL :                                                           331
C Call To Local Procedure WPPL                                            331
      ASSIGN 5080 TO BLOCK_WPPL                                           331
      GO TO  5070                                                         331
5080  CONTINUE                                                            331
C *                                                                       331
C Geant POSITION : WPPL ===> MOTHER='CAVE' X=+WPFM_POLEP(1) Y=WPFM_POLEP  332
C (2) Z=WPFM_POLEP(3) <===                                                332
      CALL AGSRESET                                                       332
      AG_MOTHER = 'CAVE'                                                  332
      AG_X = +WPFM_POLEP(1)                                               332
      AG_Y = WPFM_POLEP(2)                                                332
      AG_Z = WPFM_POLEP(3)                                                332
      AG_TITLE = 'WPPL'                                                   332
      AG_EXNAME = 'POSITION'                                              332
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         332
      CALL AXPOSITION                                                     332
C *                                                                       332
C Geant POSITION : WPPL ===> MOTHER='CAVE' X=-WPFM_POLEP(1) Y=WPFM_POLEP  332
C (2) Z=WPFM_POLEP(3) <===                                                332
      CALL AGSRESET                                                       333
      AG_MOTHER = 'CAVE'                                                  333
      AG_X = -WPFM_POLEP(1)                                               333
      AG_Y = WPFM_POLEP(2)                                                333
      AG_Z = WPFM_POLEP(3)                                                333
      AG_TITLE = 'WPPL'                                                   333
      AG_EXNAME = 'POSITION'                                              333
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         333
      CALL AXPOSITION                                                     333
C *                                                                       333
      CALL AGSBEGM(MODULE,IPRIN)                                          333
C Check IPRIN<0                                                           334
      IF (IPRIN.LT.0) RETURN                                              334
C Create WPIS :                                                           334
C Call To Local Procedure WPIS                                            334
      ASSIGN 5100 TO BLOCK_WPIS                                           334
      GO TO  5090                                                         334
5100  CONTINUE                                                            334
C *                                                                       334
C Geant POSITION : WPIS ===> MOTHER='CAVE' X=WPFM_IST1P(1) Y=WPFM_IST1P(  335
C 2) Z=WPFM_IST1P(3) <===                                                 335
      CALL AGSRESET                                                       335
      AG_MOTHER = 'CAVE'                                                  335
      AG_X = WPFM_IST1P(1)                                                335
      AG_Y = WPFM_IST1P(2)                                                335
      AG_Z = WPFM_IST1P(3)                                                335
      AG_TITLE = 'WPIS'                                                   335
      AG_EXNAME = 'POSITION'                                              335
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         335
      CALL AXPOSITION                                                     335
C *                                                                       335
C Geant POSITION : WPIS ===> MOTHER='CAVE' X=WPFM_IST2P(1) Y=WPFM_IST2P(  335
C 2) Z=WPFM_IST2P(3) <===                                                 335
      CALL AGSRESET                                                       336
      AG_MOTHER = 'CAVE'                                                  336
      AG_X = WPFM_IST2P(1)                                                336
      AG_Y = WPFM_IST2P(2)                                                336
      AG_Z = WPFM_IST2P(3)                                                336
      AG_TITLE = 'WPIS'                                                   336
      AG_EXNAME = 'POSITION'                                              336
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         336
      CALL AXPOSITION                                                     336
C *                                                                       336
      CALL AGSBEGM(MODULE,IPRIN)                                          336
C Check IPRIN<0                                                           337
      IF (IPRIN.LT.0) RETURN                                              337
C Create WPUF :                                                           337
C Call To Local Procedure WPUF                                            337
      ASSIGN 5120 TO BLOCK_WPUF                                           337
      GO TO  5110                                                         337
5120  CONTINUE                                                            337
C *                                                                       337
C Geant POSITION : WPUF ===> MOTHER='CAVE' X=+WPFM_USTFP(1) Y=WPFM_USTFP  338
C (2) Z=WPFM_USTFP(3) <===                                                338
      CALL AGSRESET                                                       338
      AG_MOTHER = 'CAVE'                                                  338
      AG_X = +WPFM_USTFP(1)                                               338
      AG_Y = WPFM_USTFP(2)                                                338
      AG_Z = WPFM_USTFP(3)                                                338
      AG_TITLE = 'WPUF'                                                   338
      AG_EXNAME = 'POSITION'                                              338
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         338
      CALL AXPOSITION                                                     338
C *                                                                       338
      CALL AGSBEGM(MODULE,IPRIN)                                          338
C Check IPRIN<0                                                           339
      IF (IPRIN.LT.0) RETURN                                              339
C Create WPUN :                                                           339
C Call To Local Procedure WPUN                                            339
      ASSIGN 5140 TO BLOCK_WPUN                                           339
      GO TO  5130                                                         339
5140  CONTINUE                                                            339
C *                                                                       339
C Geant POSITION : WPUN ===> MOTHER='CAVE' X=+WPFM_USTNP(1) Y=WPFM_USTNP  340
C (2) Z=WPFM_USTNP(3) <===                                                340
      CALL AGSRESET                                                       340
      AG_MOTHER = 'CAVE'                                                  340
      AG_X = +WPFM_USTNP(1)                                               340
      AG_Y = WPFM_USTNP(2)                                                340
      AG_Z = WPFM_USTNP(3)                                                340
      AG_TITLE = 'WPUN'                                                   340
      AG_EXNAME = 'POSITION'                                              340
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         340
      CALL AXPOSITION                                                     340
C *                                                                       340
      CALL AGSBEGM(MODULE,IPRIN)                                          340
C Check IPRIN<0                                                           341
      IF (IPRIN.LT.0) RETURN                                              341
C Create WPUS :                                                           341
C Call To Local Procedure WPUS                                            341
      ASSIGN 5160 TO BLOCK_WPUS                                           341
      GO TO  5150                                                         341
5160  CONTINUE                                                            341
C *                                                                       341
C Geant POSITION : WPUS ===> MOTHER='CAVE' X=+WPFM_USTSP(1) Y=WPFM_USTSP  342
C (2) Z=WPFM_USTSP(3) <===                                                342
      CALL AGSRESET                                                       342
      AG_MOTHER = 'CAVE'                                                  342
      AG_X = +WPFM_USTSP(1)                                               342
      AG_Y = WPFM_USTSP(2)                                                342
      AG_Z = WPFM_USTSP(3)                                                342
      AG_TITLE = 'WPUS'                                                   342
      AG_EXNAME = 'POSITION'                                              342
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         342
      CALL AXPOSITION                                                     342
C *                                                                       342
C ----------------------------------------------------------------------  342
C ------------ FBOX                                                       342
      CALL AGSENDM                                                        344
      RETURN                                                              344
C Begin Local Procedure FBOX                                              344
C FBOX IS ONE PB-GLASS FPD DETECTOR                                       344
5030  CONTINUE                                                            344
         AG_TITLE='FBOX'                                                  344
         AG_MODULE=MODULE                                                 344
         AG_MARK='V00'                                                    344
         CALL AGSPUSH                                                     344
         AG_MODULE=MODULE                                                 344
C    *                                                                    344
C    Geant MATERIAL : AIR ===> inherited arguments used <===              345
         CALL AGSRESET                                                    345
         AG_TITLE = 'AIR'                                                 345
         AG_EXNAME = 'MATERIAL'                                           345
         AG_PARLIST = 'NONE'                                              345
         CALL AXMATERIAL                                                  345
C    *                                                                    345
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           345
         CALL AGSRESET                                                    346
         AG_TITLE = 'STANDARD'                                            346
         AG_EXNAME = 'MEDIUM'                                             346
         AG_PARLIST = 'NONE'                                              346
         CALL AXMEDIUM                                                    346
C    *                                                                    346
C    Geant ATTRIBUTE : FBOX ===> SEEN=1 COLO=2 SERIAL=SERN <===           346
         CALL AGSRESET                                                    347
         AG_SEEN = 1                                                      347
         AG_COLO = 2                                                      347
         AG_SERIAL = SERN                                                 347
         AG_TITLE = 'FBOX'                                                347
         AG_EXNAME = 'ATTRIBUTE'                                          347
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  347
         CALL AXATTRIBUTE                                                 347
C    *                                                                    347
C    Check FBXD_TYPE.EQ.2                                                 347
         IF (FBXD_TYPE.EQ.2) THEN                                         348
C    Geant SHAPE : BOX ===> DX=FBXD_WIDTH/2.0 DY=FBXD_HEIGHT/2.0, DZ=FBX  350
C    D_DEPTH/2.0 <===                                                     350
         CALL AGSRESET                                                    350
         AG_DX = FBXD_WIDTH/2.0                                           350
         AG_DY = FBXD_HEIGHT/2.0                                          350
         AG_DZ = FBXD_DEPTH/2.0                                           350
         AG_TITLE = 'BOX'                                                 350
         AG_EXNAME = 'SHAPE'                                              350
         AG_PARLIST = 'DX_DY_DZ'                                          350
         CALL AXSHAPE                                                     350
C    *                                                                    350
         ELSE                                                             350
C    Geant SHAPE : BOX ===> DX=BWID DY=FBXD_HEIGHT/2.0 DZ=FBXD_DEPTH/2.0  352
C     <===                                                                352
         CALL AGSRESET                                                    352
         AG_DX = BWID                                                     352
         AG_DY = FBXD_HEIGHT/2.0                                          352
         AG_DZ = FBXD_DEPTH/2.0                                           352
         AG_TITLE = 'BOX'                                                 352
         AG_EXNAME = 'SHAPE'                                              352
         AG_PARLIST = 'DX_DY_DZ'                                          352
         CALL AXSHAPE                                                     352
C    *                                                                    352
         ENDIF                                                            352
C    use FLGG bank                                                        354
         BPATH_FLGG='FLGG*'                                               354
         IDNUM_FLGG(1)=0                                                  354
         FLGG_TYPE=2                                                      354
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGG',NFLGG_TYPE,              354
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        354
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                354
         WIDL = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                 355
C    use FLGG bank                                                        356
         BPATH_FLGG='FLGG*'                                               356
         IDNUM_FLGG(1)=0                                                  356
         FLGG_TYPE=1                                                      356
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGG',NFLGG_TYPE,              356
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        356
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                356
C    use FLGM bank                                                        357
         BPATH_FLGM='FLGM*'                                               357
         IDNUM_FLGM(1)=0                                                  357
         FLGM_TYPE=1                                                      357
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGM',NFLGM_TYPE,              357
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        357
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                357
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  358
         ZTOT =(FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0             359
         RTOT = FBXD_NX*WID/2.0                                           360
         BWID = RTOT - FBXD_XOFFSET                                       361
C    Create FTOW :                                                        362
C    Call To Local Procedure FTOW                                         362
         ASSIGN 5180 TO BLOCK_FTOW                                        362
         GO TO  5170                                                      362
5180     CONTINUE                                                         362
C    *                                                                    362
C    Create PBPT :                                                        363
C    Call To Local Procedure PBPT                                         363
         ASSIGN 5200 TO BLOCK_PBPT                                        363
         GO TO  5190                                                      363
5200     CONTINUE                                                         363
C    *                                                                    363
C    Create FSHM :                                                        364
C    Call To Local Procedure FSHM                                         364
         ASSIGN 5220 TO BLOCK_FSHM                                        364
         GO TO  5210                                                      364
5220     CONTINUE                                                         364
C    *                                                                    364
C    Check FBXD_TYPE.EQ.2.AND.FPOS_IMOD.EQ.4                              365
         IF (FBXD_TYPE.EQ.2.AND.FPOS_IMOD.EQ.4) THEN                      365
         X0 = - RTOT - FBXD_XOFFSET + WID/2.0                             366
         WIDX = WID                                                       367
         ELSE                                                             368
         X0 = RTOT + FBXD_XOFFSET - WID/2.0                               369
         WIDX = -WID                                                      370
         ENDIF                                                            371
C    Check FBXD_TYPE.EQ.2                                                 372
         IF (FBXD_TYPE.EQ.2) THEN                                         372
         Y1 = FBXD_NY*WID/2.0 - WID/2.0 + (16*WIDL-FBXD_NY*WID)/2.0       373
         WIDY = WID+(16.0*WIDL-FBXD_NY*WID)/23.0                          374
         ELSE                                                             375
         Y1 = FBXD_NY*WID/2.0 - WID/2.0                                   376
         WIDY = WID                                                       377
         ENDIF                                                            378
         Z1 = - FBXD_DEPTH/2.0 + FBXD_ZOFFSET + ZTOT                      379
C    *                                                                    380
         I=1                                                              380
            GO TO 5233                                                    380
5231        I=I+1                                                         380
5233        IF(I-(FBXD_NY).GT.0) GO TO 5232                               380
            X1=X0                                                         381
C       *                                                                 382
            J=1                                                           382
               GO TO 5243                                                 382
5241           J=J+1                                                      382
5243           IF(J-(FBXD_NX).GT.0) GO TO 5242                            382
C          Check FBXD_TYPE.EQ.2.AND.J.LT.6.AND.I.GT.7.AND.I.LT.18         383
               IF (FBXD_TYPE.EQ.2.AND.J.LT.6.AND.I.GT.7.AND.I.LT.18)      383
     *         THEN                                                       383
               X1=X1+WIDX                                                 384
               ELSE                                                       385
C          Create FTOW :                                                  386
C          Call To Local Procedure FTOW                                   386
               ASSIGN 5250 TO BLOCK_FTOW                                  386
               GO TO  5170                                                386
5250           CONTINUE                                                   386
C          *                                                              386
C          Geant POSITION : FTOW ===> X=X1 Y=Y1 Z=Z1 <===                 387
               CALL AGSRESET                                              387
               AG_X = X1                                                  387
               AG_Y = Y1                                                  387
               AG_Z = Z1                                                  387
               AG_TITLE = 'FTOW'                                          387
               AG_EXNAME = 'POSITION'                                     387
               AG_PARLIST = 'X_Y_Z'                                       387
               CALL AXPOSITION                                            387
C          *                                                              387
               X1=X1+WIDX                                                 387
               ENDIF                                                      389
            GO TO 5241                                                    390
5242        CONTINUE                                                      390
            Y1 = Y1-WIDY                                                  391
         GO TO 5231                                                       392
5232     CONTINUE                                                         392
C    Check FBXD_TYPE.EQ.1                                                 393
         IF (FBXD_TYPE.EQ.1) THEN                                         393
         X1=X0                                                            394
         Y1= -RTOT + ZTOT                                                 395
         Z1=-FBXD_DEPTH/2.0 + FBXD_PSOFFSET + WID/2.0                     396
C    *                                                                    397
         J=1                                                              397
            GO TO 5263                                                    397
5261        J=J+1                                                         397
5263        IF(J-(FBXD_NX).GT.0) GO TO 5262                               397
C       Create FTOW :                                                     398
C       Call To Local Procedure FTOW                                      398
            ASSIGN 5270 TO BLOCK_FTOW                                     398
            GO TO  5170                                                   398
5270        CONTINUE                                                      398
C       *                                                                 398
C       Geant POSITION : FTOW ===> X=X1 Y=Y1 Z=Z1 ALPHAX=90 <===          399
            CALL AGSRESET                                                 399
            AG_X = X1                                                     399
            AG_Y = Y1                                                     399
            AG_Z = Z1                                                     399
            AG_ALPHAN=AG_ALPHAN+1                                         399
            AG_ALPHAK(AG_ALPHAN)=1                                        399
            AG_ALPHAL(AG_ALPHAN)=90                                       399
            AG_TITLE = 'FTOW'                                             399
            AG_EXNAME = 'POSITION'                                        399
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   399
            CALL AXPOSITION                                               399
C       *                                                                 399
            X1=X1-WID                                                     399
         GO TO 5261                                                       401
5262     CONTINUE                                                         401
C    Check FMCG_PBPLATE==1                                                402
         IF (FMCG_PBPLATE.EQ.1) THEN                                      402
C    Create PBPT :                                                        403
C    Call To Local Procedure PBPT                                         403
         ASSIGN 5280 TO BLOCK_PBPT                                        403
         GO TO  5190                                                      403
5280     CONTINUE                                                         403
C    *                                                                    403
C    Geant POSITION : PBPT ===> X=0 Y=0 Z=PBPD_THICK/2.0-FBXD_DEPTH/2.0   404
C    <===                                                                 404
         CALL AGSRESET                                                    404
         AG_X = 0                                                         404
         AG_Y = 0                                                         404
         AG_Z = PBPD_THICK/2.0-FBXD_DEPTH/2.0                             404
         AG_TITLE = 'PBPT'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'X_Y_Z'                                             404
         CALL AXPOSITION                                                  404
C    *                                                                    404
         ENDIF                                                            404
         ZTOTSMD=FMXG_G10THICK+FMXG_SAPEX                                 406
C    Create FSHM :                                                        407
C    Call To Local Procedure FSHM                                         407
         ASSIGN 5290 TO BLOCK_FSHM                                        407
         GO TO  5210                                                      407
5290     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : FSHM ===> X=0 Y=0 Z=FBXD_SMDOFF+ZTOTSMD-FBXD_DEPTH  408
C    /2.0 <===                                                            408
         CALL AGSRESET                                                    408
         AG_X = 0                                                         408
         AG_Y = 0                                                         408
         AG_Z = FBXD_SMDOFF+ZTOTSMD-FBXD_DEPTH/2.0                        408
         AG_TITLE = 'FSHM'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z'                                             408
         CALL AXPOSITION                                                  408
C    *                                                                    408
         ENDIF                                                            408
C    Check FBXD_TYPE.GE.2                                                 410
         IF (FBXD_TYPE.GE.2) THEN                                         410
C    use FLGG bank                                                        411
         BPATH_FLGG='FLGG*'                                               411
         IDNUM_FLGG(1)=0                                                  411
         FLGG_TYPE=2                                                      411
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGG',NFLGG_TYPE,              411
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        411
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                411
C    use FLGM bank                                                        412
         BPATH_FLGM='FLGM*'                                               412
         IDNUM_FLGM(1)=0                                                  412
         FLGM_TYPE=2                                                      412
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGM',NFLGM_TYPE,              412
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        412
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                412
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  413
         ZTOT = FLGG_DEPTH/2.0                                            414
         RTOT = FBXD_NXL*WID/2.0                                          415
         BWID = RTOT                                                      416
C    LARGE CELLS OFFSET IN X                                              417
         XLCOFFSET = (FBXD_WIDTH-FBXD_NXL*WID)/2.0                        417
C    Check FPOS_IMOD.EQ.4                                                 418
         IF (FPOS_IMOD.EQ.4) THEN                                         418
         X0 = -BWID + WID/2.0 - XLCOFFSET                                 419
         WIDX = WID                                                       420
         ELSEIF (FPOS_IMOD.EQ.3) THEN                                     421
         X0 = +BWID - WID/2.0 + XLCOFFSET                                 422
         WIDX = -WID                                                      423
         ENDIF                                                            424
         Y1 = FBXD_NYL*WID/2.0 - WID/2.0                                  425
         Z1 = -FBXD_DEPTH/2.0 + FBXD_ZOFFSET + ZTOT                       426
C    *                                                                    427
         I=1                                                              427
            GO TO 5303                                                    427
5301        I=I+1                                                         427
5303        IF(I-(FBXD_NYL).GT.0) GO TO 5302                              427
            X1=X0                                                         428
C       *                                                                 429
            J=1                                                           429
               GO TO 5313                                                 429
5311           J=J+1                                                      429
5313           IF(J-(FBXD_NXL).GT.0) GO TO 5312                           429
C          Check J.LT.9.AND.I.GT.9.AND.I.LT.26                            430
               IF (J.LT.9.AND.I.GT.9.AND.I.LT.26) THEN                    430
               X1=X1+WIDX                                                 431
               ELSEIF ((I+J).GE.45) THEN                                  432
C          Create FALU :                                                  433
C          Call To Local Procedure FALU                                   433
               ASSIGN 5330 TO BLOCK_FALU                                  433
               GO TO  5320                                                433
5330           CONTINUE                                                   433
C          *                                                              433
C          Geant POSITION : FALU ===> X=X1 Y=Y1 Z=Z1 <===                 434
               CALL AGSRESET                                              434
               AG_X = X1                                                  434
               AG_Y = Y1                                                  434
               AG_Z = Z1                                                  434
               AG_TITLE = 'FALU'                                          434
               AG_EXNAME = 'POSITION'                                     434
               AG_PARLIST = 'X_Y_Z'                                       434
               CALL AXPOSITION                                            434
C          *                                                              434
               X1=X1+WIDX                                                 434
               ELSEIF ((J-I).GE.10) THEN                                  436
               X1=X1+WIDX                                                 437
               ELSE                                                       438
C          Create FLXF :                                                  439
C          Call To Local Procedure FLXF                                   439
               ASSIGN 5350 TO BLOCK_FLXF                                  439
               GO TO  5340                                                439
5350           CONTINUE                                                   439
C          *                                                              439
C          Geant POSITION : FLXF ===> X=X1 Y=Y1 Z=Z1 <===                 440
               CALL AGSRESET                                              440
               AG_X = X1                                                  440
               AG_Y = Y1                                                  440
               AG_Z = Z1                                                  440
               AG_TITLE = 'FLXF'                                          440
               AG_EXNAME = 'POSITION'                                     440
               AG_PARLIST = 'X_Y_Z'                                       440
               CALL AXPOSITION                                            440
C          *                                                              440
               X1=X1+WIDX                                                 440
               ENDIF                                                      442
            GO TO 5311                                                    443
5312        CONTINUE                                                      443
            Y1=Y1-WID                                                     444
         GO TO 5301                                                       445
5302     CONTINUE                                                         445
         ENDIF                                                            446
C    Check FBXD_TYPE.EQ.2                                                 447
         IF (FBXD_TYPE.EQ.2) THEN                                         447
C    use FLGG bank                                                        448
         BPATH_FLGG='FLGG*'                                               448
         IDNUM_FLGG(1)=0                                                  448
         FLGG_TYPE=2                                                      448
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGG',NFLGG_TYPE,              448
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        448
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                448
C    use FLGM bank                                                        449
         BPATH_FLGM='FLGM*'                                               449
         IDNUM_FLGM(1)=0                                                  449
         FLGM_TYPE=2                                                      449
         CALL AGDGETP('FPDMGEO4','*',-999,'FLGM',NFLGM_TYPE,              449
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        449
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                449
         BZOFFSET=0.0                                                     450
         WID = FLGG_WIDTH + FLGG_DGAP                                     451
         BASEWIDTH=1.0                                                    452
C    Create FBAS :                                                        453
C    Call To Local Procedure FBAS                                         453
         ASSIGN 5370 TO BLOCK_FBAS                                        453
         GO TO  5360                                                      453
5370     CONTINUE                                                         453
C    *                                                                    453
C    Geant POSITION : FBAS ===> X=FPOS_X Y=-(FBXD_NXL*WID+BASEWIDTH/2.0)  455
C     Z=-FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0 <===                      455
         CALL AGSRESET                                                    455
         AG_X = FPOS_X                                                    455
         AG_Y = -(FBXD_NXL*WID+BASEWIDTH/2.0)                             455
         AG_Z = -FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0                   455
         AG_TITLE = 'FBAS'                                                455
         AG_EXNAME = 'POSITION'                                           455
         AG_PARLIST = 'X_Y_Z'                                             455
         CALL AXPOSITION                                                  455
C    *                                                                    455
         ENDIF                                                            455
C    Create FENC :                                                        457
C    Call To Local Procedure FENC                                         457
         ASSIGN 5390 TO BLOCK_FENC                                        457
         GO TO  5380                                                      457
5390     CONTINUE                                                         457
C    *                                                                    457
C    Create FEAC :                                                        458
C    Call To Local Procedure FEAC                                         458
         ASSIGN 5410 TO BLOCK_FEAC                                        458
         GO TO  5400                                                      458
5410     CONTINUE                                                         458
C    *                                                                    458
C    Create FECC :                                                        459
C    Call To Local Procedure FECC                                         459
         ASSIGN 5430 TO BLOCK_FECC                                        459
         GO TO  5420                                                      459
5430     CONTINUE                                                         459
C    *                                                                    459
C    Create FEDC :                                                        460
C    Call To Local Procedure FEDC                                         460
         ASSIGN 5450 TO BLOCK_FEDC                                        460
         GO TO  5440                                                      460
5450     CONTINUE                                                         460
C    *                                                                    460
C    Create FEEC :                                                        461
C    Call To Local Procedure FEEC                                         461
         ASSIGN 5470 TO BLOCK_FEEC                                        461
         GO TO  5460                                                      461
5470     CONTINUE                                                         461
C    *                                                                    461
         DISTANCER=INSE_GAPHEIGHT - (INSE_HEIGHT - 2.0*INSE_SHEETDPT)     462
         XOFFFECC=(INSE_SHEETDPT - FBXD_WIDTH)/2.0                        463
         ZOFFFECC= - FBXD_DEPTH/2.0 + BZOFFSET + INSE_DEPTH -             464
     *   INSE_GATEGAP                                                     464
         XOFFFEDC=INSE_WIDTH - INSE_SHEETDPT                              465
         XOFFFENC=(INSE_WIDTH - FBXD_WIDTH)/2.0                           466
         YOFFFENC=(INSE_HEIGHT - INSE_SHEETDPT)/2.0                       467
         ZOFFFENC= - FBXD_DEPTH/2.0 + BZOFFSET + INSE_DEPTH/2.0           468
C    THIS IS TO MOVE INSERT TO SMALL CELL EDGE                            469
         XSHIFT=8*5.812 - 12*3.822 + 5*3.822 - INSE_WIDTH                 469
C    Check FBXD_TYPE.EQ.2                                                 470
         IF (FBXD_TYPE.EQ.2) THEN                                         470
C    Check FPOS_IMOD.EQ.4                                                 471
         IF (FPOS_IMOD.EQ.4) THEN                                         471
C    Geant POSITION : FENC ===> X=XOFFFENC+XSHIFT Y=-YOFFFENC Z=ZOFFFENC  472
C     <===                                                                472
         CALL AGSRESET                                                    472
         AG_X = XOFFFENC+XSHIFT                                           472
         AG_Y = -YOFFFENC                                                 472
         AG_Z = ZOFFFENC                                                  472
         AG_TITLE = 'FENC'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'X_Y_Z'                                             472
         CALL AXPOSITION                                                  472
C    *                                                                    472
C    Geant POSITION : FENC ===> X=XOFFFENC+XSHIFT Y=YOFFFENC Z=ZOFFFENC   472
C    <===                                                                 472
         CALL AGSRESET                                                    473
         AG_X = XOFFFENC+XSHIFT                                           473
         AG_Y = YOFFFENC                                                  473
         AG_Z = ZOFFFENC                                                  473
         AG_TITLE = 'FENC'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'X_Y_Z'                                             473
         CALL AXPOSITION                                                  473
C    *                                                                    473
C    Geant POSITION : FEAC ===> X=INSE_WIDTH+XSHIFT-(FBXD_WIDTH+INSE_SHE  473
C    ETDPT)/2.0 Y=0 Z=-FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0 <===        473
         CALL AGSRESET                                                    475
         AG_X = INSE_WIDTH+XSHIFT-(FBXD_WIDTH+INSE_SHEETDPT)/2.0          475
         AG_Y = 0                                                         475
         AG_Z = -FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0                   475
         AG_TITLE = 'FEAC'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'X_Y_Z'                                             475
         CALL AXPOSITION                                                  475
C    *                                                                    475
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=DISTANCER/2.0 Z=ZOFF  475
C    FECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                           475
         CALL AGSRESET                                                    477
         AG_X = XOFFFECC+XSHIFT                                           477
         AG_Y = DISTANCER/2.0                                             477
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 477
         AG_TITLE = 'FECC'                                                477
         AG_EXNAME = 'POSITION'                                           477
         AG_PARLIST = 'X_Y_Z'                                             477
         CALL AXPOSITION                                                  477
C    *                                                                    477
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=DISTANCER/2.0 Z=ZOFF  477
C    FECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                   477
         CALL AGSRESET                                                    479
         AG_X = XOFFFECC+XSHIFT                                           479
         AG_Y = DISTANCER/2.0                                             479
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         479
         AG_TITLE = 'FECC'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z'                                             479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=-DISTANCER/2.0 Z=ZOF  479
C    FFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                          479
         CALL AGSRESET                                                    481
         AG_X = XOFFFECC+XSHIFT                                           481
         AG_Y = -DISTANCER/2.0                                            481
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 481
         AG_TITLE = 'FECC'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X_Y_Z'                                             481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=-DISTANCER/2.0 Z=ZOF  481
C    FFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                  481
         CALL AGSRESET                                                    483
         AG_X = XOFFFECC+XSHIFT                                           483
         AG_Y = -DISTANCER/2.0                                            483
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         483
         AG_TITLE = 'FECC'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Y_Z'                                             483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Geant POSITION : FEDC ===> X=XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0 Z  483
C    =ZOFFFECC-INSE_GATEDEPTH/2.0 <===                                    483
         CALL AGSRESET                                                    485
         AG_X = XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0                          485
         AG_Y = 0                                                         485
         AG_Z = ZOFFFECC-INSE_GATEDEPTH/2.0                               485
         AG_TITLE = 'FEDC'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'X_Y_Z'                                             485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Geant POSITION : FEDC ===> X=XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0 Z  485
C    =ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/2.0 <===       485
         CALL AGSRESET                                                    487
         AG_X = XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0                          487
         AG_Y = 0                                                         487
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/     487
     *   2.0                                                              487
         AG_TITLE = 'FEDC'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'X_Y_Z'                                             487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Geant POSITION : FEDC ===> X=XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0 Z  487
C    =ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0 <==  487
C    =                                                                    487
         CALL AGSRESET                                                    489
         AG_X = XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0                          489
         AG_Y = 0                                                         489
         AG_Z =                                                           489
     *   ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0   489
         AG_TITLE = 'FEDC'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'X_Y_Z'                                             489
         CALL AXPOSITION                                                  489
C    *                                                                    489
         ELSEIF (FPOS_IMOD.EQ.3) THEN                                     489
C    Geant POSITION : FENC ===> X=-XOFFFENC-XSHIFT Y=-YOFFFENC Z=ZOFFFEN  491
C    C <===                                                               491
         CALL AGSRESET                                                    491
         AG_X = -XOFFFENC-XSHIFT                                          491
         AG_Y = -YOFFFENC                                                 491
         AG_Z = ZOFFFENC                                                  491
         AG_TITLE = 'FENC'                                                491
         AG_EXNAME = 'POSITION'                                           491
         AG_PARLIST = 'X_Y_Z'                                             491
         CALL AXPOSITION                                                  491
C    *                                                                    491
C    Geant POSITION : FENC ===> X=-XOFFFENC-XSHIFT Y=YOFFFENC Z=ZOFFFENC  491
C     <===                                                                491
         CALL AGSRESET                                                    492
         AG_X = -XOFFFENC-XSHIFT                                          492
         AG_Y = YOFFFENC                                                  492
         AG_Z = ZOFFFENC                                                  492
         AG_TITLE = 'FENC'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Y_Z'                                             492
         CALL AXPOSITION                                                  492
C    *                                                                    492
C    Geant POSITION : FEAC ===> X=-XSHIFT-INSE_WIDTH+(FBXD_WIDTH+INSE_SH  492
C    EETDPT)/2.0 Y=0 Z=-FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0 <===       492
         CALL AGSRESET                                                    494
         AG_X = -XSHIFT-INSE_WIDTH+(FBXD_WIDTH+INSE_SHEETDPT)/2.0         494
         AG_Y = 0                                                         494
         AG_Z = -FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0                   494
         AG_TITLE = 'FEAC'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'X_Y_Z'                                             494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=DISTANCER/2.0 Z=ZOF  494
C    FFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                          494
         CALL AGSRESET                                                    496
         AG_X = -XOFFFECC-XSHIFT                                          496
         AG_Y = DISTANCER/2.0                                             496
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 496
         AG_TITLE = 'FECC'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'X_Y_Z'                                             496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=DISTANCER/2.0 Z=ZOF  496
C    FFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                  496
         CALL AGSRESET                                                    498
         AG_X = -XOFFFECC-XSHIFT                                          498
         AG_Y = DISTANCER/2.0                                             498
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         498
         AG_TITLE = 'FECC'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'X_Y_Z'                                             498
         CALL AXPOSITION                                                  498
C    *                                                                    498
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=-DISTANCER/2.0 Z=ZO  498
C    FFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                         498
         CALL AGSRESET                                                    500
         AG_X = -XOFFFECC-XSHIFT                                          500
         AG_Y = -DISTANCER/2.0                                            500
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 500
         AG_TITLE = 'FECC'                                                500
         AG_EXNAME = 'POSITION'                                           500
         AG_PARLIST = 'X_Y_Z'                                             500
         CALL AXPOSITION                                                  500
C    *                                                                    500
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=-DISTANCER/2.0 Z=ZO  500
C    FFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                 500
         CALL AGSRESET                                                    502
         AG_X = -XOFFFECC-XSHIFT                                          502
         AG_Y = -DISTANCER/2.0                                            502
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         502
         AG_TITLE = 'FECC'                                                502
         AG_EXNAME = 'POSITION'                                           502
         AG_PARLIST = 'X_Y_Z'                                             502
         CALL AXPOSITION                                                  502
C    *                                                                    502
C    Geant POSITION : FEEC ===> X=-XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0   502
C    Z=ZOFFFECC-INSE_GATEDEPTH/2.0 <===                                   502
         CALL AGSRESET                                                    504
         AG_X = -XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0                         504
         AG_Y = 0                                                         504
         AG_Z = ZOFFFECC-INSE_GATEDEPTH/2.0                               504
         AG_TITLE = 'FEEC'                                                504
         AG_EXNAME = 'POSITION'                                           504
         AG_PARLIST = 'X_Y_Z'                                             504
         CALL AXPOSITION                                                  504
C    *                                                                    504
C    Geant POSITION : FEEC ===> X=-XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0   504
C    Z=ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/2.0 <===      504
         CALL AGSRESET                                                    506
         AG_X = -XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0                         506
         AG_Y = 0                                                         506
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/     506
     *   2.0                                                              506
         AG_TITLE = 'FEEC'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_Y_Z'                                             506
         CALL AXPOSITION                                                  506
C    *                                                                    506
C    Geant POSITION : FEEC ===> X=-XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0   506
C    Z=ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0 <=  506
C    ==                                                                   506
         CALL AGSRESET                                                    508
         AG_X = -XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0                         508
         AG_Y = 0                                                         508
         AG_Z =                                                           508
     *   ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0   508
         AG_TITLE = 'FEEC'                                                508
         AG_EXNAME = 'POSITION'                                           508
         AG_PARLIST = 'X_Y_Z'                                             508
         CALL AXPOSITION                                                  508
C    *                                                                    508
         ENDIF                                                            508
         ENDIF                                                            510
5031  CALL AGSPOPB                                                        511
      GO TO BLOCK_FBOX, (5030 ,5040)                                      511
C End of Local Procedure                                                  511
C ----------------------------------------------------------------------  512
C ------------ FTOW                                                       512
C Begin Local Procedure FTOW                                              513
C FTOW IS ONE PBG TOWER                                                   513
5170  CONTINUE                                                            513
         AG_TITLE='FTOW'                                                  513
         AG_MODULE=MODULE                                                 513
         AG_MARK='V00'                                                    513
         CALL AGSPUSH                                                     513
         AG_MODULE=MODULE                                                 513
C    *                                                                    513
C    Geant MATERIAL : AIR ===> inherited arguments used <===              514
         CALL AGSRESET                                                    514
         AG_TITLE = 'AIR'                                                 514
         AG_EXNAME = 'MATERIAL'                                           514
         AG_PARLIST = 'NONE'                                              514
         CALL AXMATERIAL                                                  514
C    *                                                                    514
C    Geant ATTRIBUTE : FTOW ===> SEEN=1 COLO=2 <===                       514
         CALL AGSRESET                                                    515
         AG_SEEN = 1                                                      515
         AG_COLO = 2                                                      515
         AG_TITLE = 'FTOW'                                                515
         AG_EXNAME = 'ATTRIBUTE'                                          515
         AG_PARLIST = 'SEEN_COLO'                                         515
         CALL AXATTRIBUTE                                                 515
C    *                                                                    515
C    Geant SHAPE : BOX ===> DX=WID/2.0 DY=WID/2.0 DZ=ZTOT <===            515
         CALL AGSRESET                                                    516
         AG_DX = WID/2.0                                                  516
         AG_DY = WID/2.0                                                  516
         AG_DZ = ZTOT                                                     516
         AG_TITLE = 'BOX'                                                 516
         AG_EXNAME = 'SHAPE'                                              516
         AG_PARLIST = 'DX_DY_DZ'                                          516
         CALL AXSHAPE                                                     516
C    *                                                                    516
C    Create FWAL :                                                        517
C    Call To Local Procedure FWAL                                         517
         ASSIGN 5490 TO BLOCK_FWAL                                        517
         GO TO  5480                                                      517
5490     CONTINUE                                                         517
C    *                                                                    517
C    Geant POSITION : FWAL ===> Z=-ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0 <=  518
C    ==                                                                   518
         CALL AGSRESET                                                    518
         AG_Z = -ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0                       518
         AG_TITLE = 'FWAL'                                                518
         AG_EXNAME = 'POSITION'                                           518
         AG_PARLIST = 'Z'                                                 518
         CALL AXPOSITION                                                  518
C    *                                                                    518
C    Create FUMT :                                                        519
C    Call To Local Procedure FUMT                                         519
         ASSIGN 5510 TO BLOCK_FUMT                                        519
         GO TO  5500                                                      519
5510     CONTINUE                                                         519
C    *                                                                    519
C    Geant POSITION : FUMT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUM  520
C    ETDZ/2.0 <===                                                        520
         CALL AGSRESET                                                    520
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUMETDZ/2.0            520
         AG_TITLE = 'FUMT'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'Z'                                                 520
         CALL AXPOSITION                                                  520
C    *                                                                    520
C    Create FPCT :                                                        521
C    Call To Local Procedure FPCT                                         521
         ASSIGN 5530 TO BLOCK_FPCT                                        521
         GO TO  5520                                                      521
5530     CONTINUE                                                         521
C    *                                                                    521
C    Geant POSITION : FPCT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHC  522
C    ATHDZ/2.0 <===                                                       522
         CALL AGSRESET                                                    522
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHCATHDZ/2.0           522
         AG_TITLE = 'FPCT'                                                522
         AG_EXNAME = 'POSITION'                                           522
         AG_PARLIST = 'Z'                                                 522
         CALL AXPOSITION                                                  522
C    *                                                                    522
5171  CALL AGSPOPB                                                        523
      GO TO BLOCK_FTOW, (5170 ,5180,5250,5270)                            523
C End of Local Procedure                                                  523
C ----------------------------------------------------------------------  524
C ------------ FWAL                                                       524
C Begin Local Procedure FWAL                                              525
C FWAL IS ALMUNUM WRAPPER                                                 525
5480  CONTINUE                                                            525
         AG_TITLE='FWAL'                                                  525
         AG_MODULE=MODULE                                                 525
         AG_MARK='V00'                                                    525
         CALL AGSPUSH                                                     525
         AG_MODULE=MODULE                                                 525
C    *                                                                    525
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        526
         CALL AGSRESET                                                    526
         AG_TITLE = 'ALUMINIUM'                                           526
         AG_EXNAME = 'MATERIAL'                                           526
         AG_PARLIST = 'NONE'                                              526
         CALL AXMATERIAL                                                  526
C    *                                                                    526
C    Geant ATTRIBUTE : FWAL ===> SEEN=1 COLO=3 <===                       526
         CALL AGSRESET                                                    527
         AG_SEEN = 1                                                      527
         AG_COLO = 3                                                      527
         AG_TITLE = 'FWAL'                                                527
         AG_EXNAME = 'ATTRIBUTE'                                          527
         AG_PARLIST = 'SEEN_COLO'                                         527
         CALL AXATTRIBUTE                                                 527
C    *                                                                    527
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0+FLGG_ALTHICK, DY=FLGG_WIDT  527
C    H/2.0+FLGG_ALTHICK DZ=FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0 <===           527
         CALL AGSRESET                                                    529
         AG_DX = FLGG_WIDTH/2.0+FLGG_ALTHICK                              529
         AG_DY = FLGG_WIDTH/2.0+FLGG_ALTHICK                              529
         AG_DZ = FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0                          529
         AG_TITLE = 'BOX'                                                 529
         AG_EXNAME = 'SHAPE'                                              529
         AG_PARLIST = 'DX_DY_DZ'                                          529
         CALL AXSHAPE                                                     529
C    *                                                                    529
C    Check FMCG_CHKVSIM==1                                                529
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      530
         CALL GSCKOV( AG_IMED,N,E,ABSCO_ALM,EFFIC_ALL,RINDEX_ALM )        531
         ENDIF                                                            532
C    Create FLGR :                                                        533
C    Call To Local Procedure FLGR                                         533
         ASSIGN 5550 TO BLOCK_FLGR                                        533
         GO TO  5540                                                      533
5550     CONTINUE                                                         533
C    *                                                                    533
C    Geant POSITION : FLGR ===> Z=+FLGG_ALTHICK/2.0 <===                  534
         CALL AGSRESET                                                    534
         AG_Z = +FLGG_ALTHICK/2.0                                         534
         AG_TITLE = 'FLGR'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'Z'                                                 534
         CALL AXPOSITION                                                  534
C    *                                                                    534
5481  CALL AGSPOPB                                                        535
      GO TO BLOCK_FWAL, (5480 ,5490)                                      535
C End of Local Procedure                                                  535
C ----------------------------------------------------------------------  536
C ------------ FLGR                                                       536
C Begin Local Procedure FLGR                                              537
C FLGR IS LEAD GLASS DETECTOR                                             537
5540  CONTINUE                                                            537
         AG_TITLE='FLGR'                                                  537
         AG_MODULE=MODULE                                                 537
         AG_MARK='V00'                                                    537
         CALL AGSPUSH                                                     537
         AG_MODULE=MODULE                                                 537
C    *                                                                    537
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.60712 <===                538
         CALL AGSRESET                                                    538
         AG_A = 207.19                                                    538
         AG_Z = 82                                                        538
         AG_W = .60712                                                    538
         AG_TITLE = 'PB'                                                  538
         AG_EXNAME = 'COMPONENT'                                          538
         AG_PARLIST = 'A_Z_W'                                             538
         CALL AXCOMPONENT                                                 538
C    *                                                                    538
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.02324 <===                 538
         CALL AGSRESET                                                    539
         AG_A = 39.102                                                    539
         AG_Z = 19                                                        539
         AG_W = .02324                                                    539
         AG_TITLE = 'K'                                                   539
         AG_EXNAME = 'COMPONENT'                                          539
         AG_PARLIST = 'A_Z_W'                                             539
         CALL AXCOMPONENT                                                 539
C    *                                                                    539
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.14771 <===                539
         CALL AGSRESET                                                    540
         AG_A = 28.088                                                    540
         AG_Z = 14                                                        540
         AG_W = .14771                                                    540
         AG_TITLE = 'SI'                                                  540
         AG_EXNAME = 'COMPONENT'                                          540
         AG_PARLIST = 'A_Z_W'                                             540
         CALL AXCOMPONENT                                                 540
C    *                                                                    540
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.22041 <===                  540
         CALL AGSRESET                                                    541
         AG_A = 15.999                                                    541
         AG_Z = 8                                                         541
         AG_W = .22041                                                    541
         AG_TITLE = 'O'                                                   541
         AG_EXNAME = 'COMPONENT'                                          541
         AG_PARLIST = 'A_Z_W'                                             541
         CALL AXCOMPONENT                                                 541
C    *                                                                    541
C    Geant COMPONENT : AS ===> A=74.922 Z=33 W=.00152 <===                541
         CALL AGSRESET                                                    542
         AG_A = 74.922                                                    542
         AG_Z = 33                                                        542
         AG_W = .00152                                                    542
         AG_TITLE = 'AS'                                                  542
         AG_EXNAME = 'COMPONENT'                                          542
         AG_PARLIST = 'A_Z_W'                                             542
         CALL AXCOMPONENT                                                 542
C    *                                                                    542
C    Geant MIXTURE : PBG ===> DENS=FLGM_DENSITY <===                      542
         CALL AGSRESET                                                    543
         AG_DENS = FLGM_DENSITY                                           543
         AG_TITLE = 'PBG'                                                 543
         AG_EXNAME = 'MIXTURE'                                            543
         AG_PARLIST = 'DENS'                                              543
         CALL AXMIXTURE                                                   543
C    *                                                                    543
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           543
         CALL AGSRESET                                                    544
         AG_ISVOL = 1                                                     544
         AG_TITLE = 'LEADGLASS'                                           544
         AG_EXNAME = 'MEDIUM'                                             544
         AG_PARLIST = 'ISVOL'                                             544
         CALL AXMEDIUM                                                    544
C    *                                                                    544
C    Geant ATTRIBUTE : FLGR ===> SEEN=1 COLO=4 <===                       544
         CALL AGSRESET                                                    545
         AG_SEEN = 1                                                      545
         AG_COLO = 4                                                      545
         AG_TITLE = 'FLGR'                                                545
         AG_EXNAME = 'ATTRIBUTE'                                          545
         AG_PARLIST = 'SEEN_COLO'                                         545
         CALL AXATTRIBUTE                                                 545
C    *                                                                    545
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0 DY=FLGG_WIDTH/2.0, DZ=FLGG  545
C    _DEPTH/2.0 <===                                                      545
         CALL AGSRESET                                                    547
         AG_DX = FLGG_WIDTH/2.0                                           547
         AG_DY = FLGG_WIDTH/2.0                                           547
         AG_DZ = FLGG_DEPTH/2.0                                           547
         AG_TITLE = 'BOX'                                                 547
         AG_EXNAME = 'SHAPE'                                              547
         AG_PARLIST = 'DX_DY_DZ'                                          547
         CALL AXSHAPE                                                     547
C    *                                                                    547
C    Defining Hits: ELOS:0:(0,250)                                        547
         AG_TITLE = 'FLGR'                                                548
         AG_EXNAME = 'HITS'                                               548
         AG_TYPE=AG_MAGIC                                                 548
         CALL AGSDETV('FLGR')                                             548
C    *                                                                    548
         AG_PARLIST='ELOS'                                                548
         AG_TITLE = 'DETH'                                                548
         AG_NBITS = 0                                                     548
         AG_BIN=AG_MAGIC                                                  548
         AG_OPTION='*'                                                    548
         AG_FHMIN = 0                                                     548
         AG_FHMAX = 250                                                   548
         CALL AGSDETH (0)                                                 548
C    *                                                                    548
         CALL AGSDETU('FLGR')                                             548
C    *                                                                    548
C    Check FMCG_CHKVSIM==1                                                549
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      549
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PBG,EFFIC_ALL,RINDEX_PBG )        550
         ENDIF                                                            551
5541  CALL AGSPOPB                                                        552
      GO TO BLOCK_FLGR, (5540 ,5550)                                      552
C End of Local Procedure                                                  552
C ----------------------------------------------------------------------  553
C ------------ FLXF                                                       553
C Begin Local Procedure FLXF                                              554
C FLXF IS LEAD GLASS DETECTOR                                             554
5340  CONTINUE                                                            554
         AG_TITLE='FLXF'                                                  554
         AG_MODULE=MODULE                                                 554
         AG_MARK='V00'                                                    554
         CALL AGSPUSH                                                     554
         AG_MODULE=MODULE                                                 554
C    *                                                                    554
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.41774 <===                555
         CALL AGSRESET                                                    555
         AG_A = 207.19                                                    555
         AG_Z = 82                                                        555
         AG_W = .41774                                                    555
         AG_TITLE = 'PB'                                                  555
         AG_EXNAME = 'COMPONENT'                                          555
         AG_PARLIST = 'A_Z_W'                                             555
         CALL AXCOMPONENT                                                 555
C    *                                                                    555
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.04151 <===                 555
         CALL AGSRESET                                                    556
         AG_A = 39.102                                                    556
         AG_Z = 19                                                        556
         AG_W = .04151                                                    556
         AG_TITLE = 'K'                                                   556
         AG_EXNAME = 'COMPONENT'                                          556
         AG_PARLIST = 'A_Z_W'                                             556
         CALL AXCOMPONENT                                                 556
C    *                                                                    556
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.21047 <===                556
         CALL AGSRESET                                                    557
         AG_A = 28.088                                                    557
         AG_Z = 14                                                        557
         AG_W = .21047                                                    557
         AG_TITLE = 'SI'                                                  557
         AG_EXNAME = 'COMPONENT'                                          557
         AG_PARLIST = 'A_Z_W'                                             557
         CALL AXCOMPONENT                                                 557
C    *                                                                    557
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.29330 <===                  557
         CALL AGSRESET                                                    558
         AG_A = 15.999                                                    558
         AG_Z = 8                                                         558
         AG_W = .29330                                                    558
         AG_TITLE = 'O'                                                   558
         AG_EXNAME = 'COMPONENT'                                          558
         AG_PARLIST = 'A_Z_W'                                             558
         CALL AXCOMPONENT                                                 558
C    *                                                                    558
C    Geant COMPONENT : NA ===> A=22.990 Z=11 W=.03710 <===                558
         CALL AGSRESET                                                    559
         AG_A = 22.990                                                    559
         AG_Z = 11                                                        559
         AG_W = .03710                                                    559
         AG_TITLE = 'NA'                                                  559
         AG_EXNAME = 'COMPONENT'                                          559
         AG_PARLIST = 'A_Z_W'                                             559
         CALL AXCOMPONENT                                                 559
C    *                                                                    559
C    Geant MIXTURE : F2 ===> DENS=FLGM_DENSITY <===                       559
         CALL AGSRESET                                                    560
         AG_DENS = FLGM_DENSITY                                           560
         AG_TITLE = 'F2'                                                  560
         AG_EXNAME = 'MIXTURE'                                            560
         AG_PARLIST = 'DENS'                                              560
         CALL AXMIXTURE                                                   560
C    *                                                                    560
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           560
         CALL AGSRESET                                                    561
         AG_ISVOL = 1                                                     561
         AG_TITLE = 'LEADGLASS'                                           561
         AG_EXNAME = 'MEDIUM'                                             561
         AG_PARLIST = 'ISVOL'                                             561
         CALL AXMEDIUM                                                    561
C    *                                                                    561
C    Geant ATTRIBUTE : FLXF ===> SEEN=1 COLO=4 <===                       561
         CALL AGSRESET                                                    562
         AG_SEEN = 1                                                      562
         AG_COLO = 4                                                      562
         AG_TITLE = 'FLXF'                                                562
         AG_EXNAME = 'ATTRIBUTE'                                          562
         AG_PARLIST = 'SEEN_COLO'                                         562
         CALL AXATTRIBUTE                                                 562
C    *                                                                    562
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0 DY=FLGG_WIDTH/2.0, DZ=FLGG  562
C    _DEPTH/2.0 <===                                                      562
         CALL AGSRESET                                                    564
         AG_DX = FLGG_WIDTH/2.0                                           564
         AG_DY = FLGG_WIDTH/2.0                                           564
         AG_DZ = FLGG_DEPTH/2.0                                           564
         AG_TITLE = 'BOX'                                                 564
         AG_EXNAME = 'SHAPE'                                              564
         AG_PARLIST = 'DX_DY_DZ'                                          564
         CALL AXSHAPE                                                     564
C    *                                                                    564
C    Defining Hits: ELOS:0:(0,250)                                        564
         AG_TITLE = 'FLXF'                                                565
         AG_EXNAME = 'HITS'                                               565
         AG_TYPE=AG_MAGIC                                                 565
         CALL AGSDETV('FLXF')                                             565
C    *                                                                    565
         AG_PARLIST='ELOS'                                                565
         AG_TITLE = 'DETH'                                                565
         AG_NBITS = 0                                                     565
         AG_BIN=AG_MAGIC                                                  565
         AG_OPTION='*'                                                    565
         AG_FHMIN = 0                                                     565
         AG_FHMAX = 250                                                   565
         CALL AGSDETH (0)                                                 565
C    *                                                                    565
         CALL AGSDETU('FLXF')                                             565
C    *                                                                    565
5341  CALL AGSPOPB                                                        566
      GO TO BLOCK_FLXF, (5340 ,5350)                                      566
C End of Local Procedure                                                  566
C ----------------------------------------------------------------------  567
C ------------ FALU                                                       567
C Begin Local Procedure FALU                                              568
C FALU IS ALUMINIUM BASE CELL                                             568
5320  CONTINUE                                                            568
         AG_TITLE='FALU'                                                  568
         AG_MODULE=MODULE                                                 568
         AG_MARK='V00'                                                    568
         CALL AGSPUSH                                                     568
         AG_MODULE=MODULE                                                 568
C    *                                                                    568
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        569
         CALL AGSRESET                                                    569
         AG_TITLE = 'ALUMINIUM'                                           569
         AG_EXNAME = 'MATERIAL'                                           569
         AG_PARLIST = 'NONE'                                              569
         CALL AXMATERIAL                                                  569
C    *                                                                    569
C    Geant ATTRIBUTE : FALU ===> SEEN=1 COLO=1 <===                       569
         CALL AGSRESET                                                    570
         AG_SEEN = 1                                                      570
         AG_COLO = 1                                                      570
         AG_TITLE = 'FALU'                                                570
         AG_EXNAME = 'ATTRIBUTE'                                          570
         AG_PARLIST = 'SEEN_COLO'                                         570
         CALL AXATTRIBUTE                                                 570
C    *                                                                    570
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0 DY=FLGG_WIDTH/2.0, DZ=FLGG  570
C    _DEPTH/2.0 <===                                                      570
         CALL AGSRESET                                                    572
         AG_DX = FLGG_WIDTH/2.0                                           572
         AG_DY = FLGG_WIDTH/2.0                                           572
         AG_DZ = FLGG_DEPTH/2.0                                           572
         AG_TITLE = 'BOX'                                                 572
         AG_EXNAME = 'SHAPE'                                              572
         AG_PARLIST = 'DX_DY_DZ'                                          572
         CALL AXSHAPE                                                     572
C    *                                                                    572
5321  CALL AGSPOPB                                                        573
      GO TO BLOCK_FALU, (5320 ,5330)                                      573
C End of Local Procedure                                                  573
C ----------------------------------------------------------------------  574
C ------------ FBAS                                                       574
C Begin Local Procedure FBAS                                              575
C FBAS IS STEEL BASE PLATE                                                575
5360  CONTINUE                                                            575
         AG_TITLE='FBAS'                                                  575
         AG_MODULE=MODULE                                                 575
         AG_MARK='V00'                                                    575
         CALL AGSPUSH                                                     575
         AG_MODULE=MODULE                                                 575
C    *                                                                    575
C    Geant MATERIAL : IRON ===> inherited arguments used <===             576
         CALL AGSRESET                                                    576
         AG_TITLE = 'IRON'                                                576
         AG_EXNAME = 'MATERIAL'                                           576
         AG_PARLIST = 'NONE'                                              576
         CALL AXMATERIAL                                                  576
C    *                                                                    576
C    Geant ATTRIBUTE : FBAS ===> SEEN=1 COLO=1 <===                       576
         CALL AGSRESET                                                    577
         AG_SEEN = 1                                                      577
         AG_COLO = 1                                                      577
         AG_TITLE = 'FBAS'                                                577
         AG_EXNAME = 'ATTRIBUTE'                                          577
         AG_PARLIST = 'SEEN_COLO'                                         577
         CALL AXATTRIBUTE                                                 577
C    *                                                                    577
C    Geant SHAPE : BOX ===> DX=FBXD_WIDTH/2.0 DY=BASEWIDTH/2.0, DZ=INSE_  577
C    DEPTH/2.0 <===                                                       577
         CALL AGSRESET                                                    579
         AG_DX = FBXD_WIDTH/2.0                                           579
         AG_DY = BASEWIDTH/2.0                                            579
         AG_DZ = INSE_DEPTH/2.0                                           579
         AG_TITLE = 'BOX'                                                 579
         AG_EXNAME = 'SHAPE'                                              579
         AG_PARLIST = 'DX_DY_DZ'                                          579
         CALL AXSHAPE                                                     579
C    *                                                                    579
5361  CALL AGSPOPB                                                        580
      GO TO BLOCK_FBAS, (5360 ,5370)                                      580
C End of Local Procedure                                                  580
C ----------------------------------------------------------------------  581
C ------------ FENC                                                       581
C Begin Local Procedure FENC                                              582
C FENC IS STEEL ENCLOSURE                                                 582
5380  CONTINUE                                                            582
         AG_TITLE='FENC'                                                  582
         AG_MODULE=MODULE                                                 582
         AG_MARK='V00'                                                    582
         CALL AGSPUSH                                                     582
         AG_MODULE=MODULE                                                 582
C    *                                                                    582
C    Geant MATERIAL : IRON ===> inherited arguments used <===             583
         CALL AGSRESET                                                    583
         AG_TITLE = 'IRON'                                                583
         AG_EXNAME = 'MATERIAL'                                           583
         AG_PARLIST = 'NONE'                                              583
         CALL AXMATERIAL                                                  583
C    *                                                                    583
C    Geant ATTRIBUTE : FENC ===> SEEN=1 COLO=5 <===                       583
         CALL AGSRESET                                                    584
         AG_SEEN = 1                                                      584
         AG_COLO = 5                                                      584
         AG_TITLE = 'FENC'                                                584
         AG_EXNAME = 'ATTRIBUTE'                                          584
         AG_PARLIST = 'SEEN_COLO'                                         584
         CALL AXATTRIBUTE                                                 584
C    *                                                                    584
C    Geant SHAPE : BOX ===> DX=INSE_WIDTH/2.0 DY=INSE_SHEETDPT/2.0, DZ=I  584
C    NSE_DEPTH/2.0 <===                                                   584
         CALL AGSRESET                                                    586
         AG_DX = INSE_WIDTH/2.0                                           586
         AG_DY = INSE_SHEETDPT/2.0                                        586
         AG_DZ = INSE_DEPTH/2.0                                           586
         AG_TITLE = 'BOX'                                                 586
         AG_EXNAME = 'SHAPE'                                              586
         AG_PARLIST = 'DX_DY_DZ'                                          586
         CALL AXSHAPE                                                     586
C    *                                                                    586
5381  CALL AGSPOPB                                                        587
      GO TO BLOCK_FENC, (5380 ,5390)                                      587
C End of Local Procedure                                                  587
C ----------------------------------------------------------------------  588
C ------------ FEAC                                                       588
C Begin Local Procedure FEAC                                              589
C FEAC IS STEEL ENCLOSURE                                                 589
5400  CONTINUE                                                            589
         AG_TITLE='FEAC'                                                  589
         AG_MODULE=MODULE                                                 589
         AG_MARK='V00'                                                    589
         CALL AGSPUSH                                                     589
         AG_MODULE=MODULE                                                 589
C    *                                                                    589
C    Geant MATERIAL : IRON ===> inherited arguments used <===             590
         CALL AGSRESET                                                    590
         AG_TITLE = 'IRON'                                                590
         AG_EXNAME = 'MATERIAL'                                           590
         AG_PARLIST = 'NONE'                                              590
         CALL AXMATERIAL                                                  590
C    *                                                                    590
C    Geant ATTRIBUTE : FEAC ===> SEEN=1 COLO=5 <===                       590
         CALL AGSRESET                                                    591
         AG_SEEN = 1                                                      591
         AG_COLO = 5                                                      591
         AG_TITLE = 'FEAC'                                                591
         AG_EXNAME = 'ATTRIBUTE'                                          591
         AG_PARLIST = 'SEEN_COLO'                                         591
         CALL AXATTRIBUTE                                                 591
C    *                                                                    591
C    Geant SHAPE : BOX ===> DX=INSE_SHEETDPT/2.0 DY=(INSE_HEIGHT-2.0*INS  591
C    E_SHEETDPT)/2.0, DZ=INSE_DEPTH/2.0 <===                              591
         CALL AGSRESET                                                    593
         AG_DX = INSE_SHEETDPT/2.0                                        593
         AG_DY = (INSE_HEIGHT-2.0*INSE_SHEETDPT)/2.0                      593
         AG_DZ = INSE_DEPTH/2.0                                           593
         AG_TITLE = 'BOX'                                                 593
         AG_EXNAME = 'SHAPE'                                              593
         AG_PARLIST = 'DX_DY_DZ'                                          593
         CALL AXSHAPE                                                     593
C    *                                                                    593
C    Create FEBC :                                                        594
C    Call To Local Procedure FEBC                                         594
         ASSIGN 5570 TO BLOCK_FEBC                                        594
         GO TO  5560                                                      594
5570     CONTINUE                                                         594
C    *                                                                    594
C    Geant POSITION : FEBC ===> X=0.0 Y=0 Z=-INSE_DEPTH/2.0+INSE_HOLEDEP  595
C    TH/2.0+INSE_HOLEGAP <===                                             595
         CALL AGSRESET                                                    595
         AG_X = 0.0                                                       595
         AG_Y = 0                                                         595
         AG_Z = -INSE_DEPTH/2.0+INSE_HOLEDEPTH/2.0+INSE_HOLEGAP           595
         AG_TITLE = 'FEBC'                                                595
         AG_EXNAME = 'POSITION'                                           595
         AG_PARLIST = 'X_Y_Z'                                             595
         CALL AXPOSITION                                                  595
C    *                                                                    595
5401  CALL AGSPOPB                                                        596
      GO TO BLOCK_FEAC, (5400 ,5410)                                      596
C End of Local Procedure                                                  596
C ----------------------------------------------------------------------  597
C ------------ FEBC                                                       597
C Begin Local Procedure FEBC                                              598
C FEBC IS AIR SQUARE HOLE                                                 598
5560  CONTINUE                                                            598
         AG_TITLE='FEBC'                                                  598
         AG_MODULE=MODULE                                                 598
         AG_MARK='V00'                                                    598
         CALL AGSPUSH                                                     598
         AG_MODULE=MODULE                                                 598
C    *                                                                    598
C    Geant MATERIAL : AIR ===> inherited arguments used <===              599
         CALL AGSRESET                                                    599
         AG_TITLE = 'AIR'                                                 599
         AG_EXNAME = 'MATERIAL'                                           599
         AG_PARLIST = 'NONE'                                              599
         CALL AXMATERIAL                                                  599
C    *                                                                    599
C    Geant ATTRIBUTE : FEBC ===> SEEN=1 COLO=5 <===                       599
         CALL AGSRESET                                                    600
         AG_SEEN = 1                                                      600
         AG_COLO = 5                                                      600
         AG_TITLE = 'FEBC'                                                600
         AG_EXNAME = 'ATTRIBUTE'                                          600
         AG_PARLIST = 'SEEN_COLO'                                         600
         CALL AXATTRIBUTE                                                 600
C    *                                                                    600
C    Geant SHAPE : BOX ===> DX=INSE_SHEETDPT/2.0 DY=INSE_HOLEHEIGHT/2.0,  600
C     DZ=INSE_HOLEDEPTH/2.0 <===                                          600
         CALL AGSRESET                                                    602
         AG_DX = INSE_SHEETDPT/2.0                                        602
         AG_DY = INSE_HOLEHEIGHT/2.0                                      602
         AG_DZ = INSE_HOLEDEPTH/2.0                                       602
         AG_TITLE = 'BOX'                                                 602
         AG_EXNAME = 'SHAPE'                                              602
         AG_PARLIST = 'DX_DY_DZ'                                          602
         CALL AXSHAPE                                                     602
C    *                                                                    602
5561  CALL AGSPOPB                                                        603
      GO TO BLOCK_FEBC, (5560 ,5570)                                      603
C End of Local Procedure                                                  603
C ----------------------------------------------------------------------  604
C ------------ FECC                                                       604
C Begin Local Procedure FECC                                              605
C FECC IS STEEL DISTANCER                                                 605
5420  CONTINUE                                                            605
         AG_TITLE='FECC'                                                  605
         AG_MODULE=MODULE                                                 605
         AG_MARK='V00'                                                    605
         CALL AGSPUSH                                                     605
         AG_MODULE=MODULE                                                 605
C    *                                                                    605
C    Geant MATERIAL : IRON ===> inherited arguments used <===             606
         CALL AGSRESET                                                    606
         AG_TITLE = 'IRON'                                                606
         AG_EXNAME = 'MATERIAL'                                           606
         AG_PARLIST = 'NONE'                                              606
         CALL AXMATERIAL                                                  606
C    *                                                                    606
C    Geant ATTRIBUTE : FECC ===> SEEN=1 COLO=5 <===                       606
         CALL AGSRESET                                                    607
         AG_SEEN = 1                                                      607
         AG_COLO = 5                                                      607
         AG_TITLE = 'FECC'                                                607
         AG_EXNAME = 'ATTRIBUTE'                                          607
         AG_PARLIST = 'SEEN_COLO'                                         607
         CALL AXATTRIBUTE                                                 607
C    *                                                                    607
C    Geant SHAPE : BOX ===> DX=INSE_SHEETDPT/2.0 DY=INSE_GAPHEIGHT/2.0,   607
C    DZ=INSE_GAPDEPTH/2.0 <===                                            607
         CALL AGSRESET                                                    609
         AG_DX = INSE_SHEETDPT/2.0                                        609
         AG_DY = INSE_GAPHEIGHT/2.0                                       609
         AG_DZ = INSE_GAPDEPTH/2.0                                        609
         AG_TITLE = 'BOX'                                                 609
         AG_EXNAME = 'SHAPE'                                              609
         AG_PARLIST = 'DX_DY_DZ'                                          609
         CALL AXSHAPE                                                     609
C    *                                                                    609
5421  CALL AGSPOPB                                                        610
      GO TO BLOCK_FECC, (5420 ,5430)                                      610
C End of Local Procedure                                                  610
C ----------------------------------------------------------------------  611
C ------------ FEDC                                                       611
C Begin Local Procedure FEDC                                              612
C FEDC IS STEEL ENCLOSURE PART ON SOUTH                                   612
5440  CONTINUE                                                            612
         AG_TITLE='FEDC'                                                  612
         AG_MODULE=MODULE                                                 612
         AG_MARK='V00'                                                    612
         CALL AGSPUSH                                                     612
         AG_MODULE=MODULE                                                 612
C    *                                                                    612
C    Geant MATERIAL : IRON ===> inherited arguments used <===             613
         CALL AGSRESET                                                    613
         AG_TITLE = 'IRON'                                                613
         AG_EXNAME = 'MATERIAL'                                           613
         AG_PARLIST = 'NONE'                                              613
         CALL AXMATERIAL                                                  613
C    *                                                                    613
C    Geant ATTRIBUTE : FEDC ===> SEEN=1 COLO=5 <===                       613
         CALL AGSRESET                                                    614
         AG_SEEN = 1                                                      614
         AG_COLO = 5                                                      614
         AG_TITLE = 'FEDC'                                                614
         AG_EXNAME = 'ATTRIBUTE'                                          614
         AG_PARLIST = 'SEEN_COLO'                                         614
         CALL AXATTRIBUTE                                                 614
C    *                                                                    614
C    Geant SHAPE : BOX ===> DX=(XOFFFEDC)/2.0 DY=(INSE_HEIGHT-2.0*INSE_S  614
C    HEETDPT)/2.0, DZ=INSE_GATEDEPTH/2.0 <===                             614
         CALL AGSRESET                                                    616
         AG_DX = (XOFFFEDC)/2.0                                           616
         AG_DY = (INSE_HEIGHT-2.0*INSE_SHEETDPT)/2.0                      616
         AG_DZ = INSE_GATEDEPTH/2.0                                       616
         AG_TITLE = 'BOX'                                                 616
         AG_EXNAME = 'SHAPE'                                              616
         AG_PARLIST = 'DX_DY_DZ'                                          616
         CALL AXSHAPE                                                     616
C    *                                                                    616
C    Create FERC :                                                        617
C    Call To Local Procedure FERC                                         617
         ASSIGN 5590 TO BLOCK_FERC                                        617
         GO TO  5580                                                      617
5590     CONTINUE                                                         617
C    *                                                                    617
C    Geant POSITION : FERC ===> X=-(XOFFFEDC)/2.0 Y=0 Z=0.0 <===          618
         CALL AGSRESET                                                    618
         AG_X = -(XOFFFEDC)/2.0                                           618
         AG_Y = 0                                                         618
         AG_Z = 0.0                                                       618
         AG_TITLE = 'FERC'                                                618
         AG_EXNAME = 'POSITION'                                           618
         AG_PARLIST = 'X_Y_Z'                                             618
         CALL AXPOSITION                                                  618
C    *                                                                    618
C    Create FESC :                                                        619
C    Call To Local Procedure FESC                                         619
         ASSIGN 5610 TO BLOCK_FESC                                        619
         GO TO  5600                                                      619
5610     CONTINUE                                                         619
C    *                                                                    619
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.  621
C    0) Y=INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                             621
         CALL AGSRESET                                                    621
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.0)                  621
         AG_Y = INSE_RA*SIN(PI*5.0/12.0)                                  621
         AG_Z = 0.0                                                       621
         AG_TITLE = 'FESC'                                                621
         AG_EXNAME = 'POSITION'                                           621
         AG_PARLIST = 'X_Y_Z'                                             621
         CALL AXPOSITION                                                  621
C    *                                                                    621
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0) Y=  621
C    INSE_RA*SIN(PI/4.0) Z=0.0 <===                                       621
         CALL AGSRESET                                                    622
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0)                       622
         AG_Y = INSE_RA*SIN(PI/4.0)                                       622
         AG_Z = 0.0                                                       622
         AG_TITLE = 'FESC'                                                622
         AG_EXNAME = 'POSITION'                                           622
         AG_PARLIST = 'X_Y_Z'                                             622
         CALL AXPOSITION                                                  622
C    *                                                                    622
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0) Y  622
C    =INSE_RA*SIN(PI/12.0) Z=0.0 <===                                     622
         CALL AGSRESET                                                    624
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0)                      624
         AG_Y = INSE_RA*SIN(PI/12.0)                                      624
         AG_Z = 0.0                                                       624
         AG_TITLE = 'FESC'                                                624
         AG_EXNAME = 'POSITION'                                           624
         AG_PARLIST = 'X_Y_Z'                                             624
         CALL AXPOSITION                                                  624
C    *                                                                    624
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0) Y  624
C    =-INSE_RA*SIN(PI/12.0) Z=0.0 <===                                    624
         CALL AGSRESET                                                    626
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0)                      626
         AG_Y = -INSE_RA*SIN(PI/12.0)                                     626
         AG_Z = 0.0                                                       626
         AG_TITLE = 'FESC'                                                626
         AG_EXNAME = 'POSITION'                                           626
         AG_PARLIST = 'X_Y_Z'                                             626
         CALL AXPOSITION                                                  626
C    *                                                                    626
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0) Y=  626
C    -INSE_RA*SIN(PI/4.0) Z=0.0 <===                                      626
         CALL AGSRESET                                                    628
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0)                       628
         AG_Y = -INSE_RA*SIN(PI/4.0)                                      628
         AG_Z = 0.0                                                       628
         AG_TITLE = 'FESC'                                                628
         AG_EXNAME = 'POSITION'                                           628
         AG_PARLIST = 'X_Y_Z'                                             628
         CALL AXPOSITION                                                  628
C    *                                                                    628
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.  628
C    0) Y=-INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                            628
         CALL AGSRESET                                                    630
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.0)                  630
         AG_Y = -INSE_RA*SIN(PI*5.0/12.0)                                 630
         AG_Z = 0.0                                                       630
         AG_TITLE = 'FESC'                                                630
         AG_EXNAME = 'POSITION'                                           630
         AG_PARLIST = 'X_Y_Z'                                             630
         CALL AXPOSITION                                                  630
C    *                                                                    630
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0) Y=  630
C    INSE_RB*SIN(PI/4.0) Z=0.0 <===                                       630
         CALL AGSRESET                                                    631
         AG_X = -(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0)                       631
         AG_Y = INSE_RB*SIN(PI/4.0)                                       631
         AG_Z = 0.0                                                       631
         AG_TITLE = 'FESC'                                                631
         AG_EXNAME = 'POSITION'                                           631
         AG_PARLIST = 'X_Y_Z'                                             631
         CALL AXPOSITION                                                  631
C    *                                                                    631
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0) Y=  631
C    -INSE_RB*SIN(PI/4.0) Z=0.0 <===                                      631
         CALL AGSRESET                                                    633
         AG_X = -(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0)                       633
         AG_Y = -INSE_RB*SIN(PI/4.0)                                      633
         AG_Z = 0.0                                                       633
         AG_TITLE = 'FESC'                                                633
         AG_EXNAME = 'POSITION'                                           633
         AG_PARLIST = 'X_Y_Z'                                             633
         CALL AXPOSITION                                                  633
C    *                                                                    633
5441  CALL AGSPOPB                                                        634
      GO TO BLOCK_FEDC, (5440 ,5450)                                      634
C End of Local Procedure                                                  634
C ----------------------------------------------------------------------  635
C ------------ FEEC                                                       635
C Begin Local Procedure FEEC                                              636
C FEEC IS STEEL ENCLOSURE PART ON NORTH                                   636
5460  CONTINUE                                                            636
         AG_TITLE='FEEC'                                                  636
         AG_MODULE=MODULE                                                 636
         AG_MARK='V00'                                                    636
         CALL AGSPUSH                                                     636
         AG_MODULE=MODULE                                                 636
C    *                                                                    636
C    Geant MATERIAL : IRON ===> inherited arguments used <===             637
         CALL AGSRESET                                                    637
         AG_TITLE = 'IRON'                                                637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant ATTRIBUTE : FEEC ===> SEEN=1 COLO=5 <===                       637
         CALL AGSRESET                                                    638
         AG_SEEN = 1                                                      638
         AG_COLO = 5                                                      638
         AG_TITLE = 'FEEC'                                                638
         AG_EXNAME = 'ATTRIBUTE'                                          638
         AG_PARLIST = 'SEEN_COLO'                                         638
         CALL AXATTRIBUTE                                                 638
C    *                                                                    638
C    Geant SHAPE : BOX ===> DX=(XOFFFEDC)/2.0 DY=(INSE_HEIGHT-2.0*INSE_S  638
C    HEETDPT)/2.0, DZ=INSE_GATEDEPTH/2.0 <===                             638
         CALL AGSRESET                                                    640
         AG_DX = (XOFFFEDC)/2.0                                           640
         AG_DY = (INSE_HEIGHT-2.0*INSE_SHEETDPT)/2.0                      640
         AG_DZ = INSE_GATEDEPTH/2.0                                       640
         AG_TITLE = 'BOX'                                                 640
         AG_EXNAME = 'SHAPE'                                              640
         AG_PARLIST = 'DX_DY_DZ'                                          640
         CALL AXSHAPE                                                     640
C    *                                                                    640
C    Create FETC :                                                        641
C    Call To Local Procedure FETC                                         641
         ASSIGN 5630 TO BLOCK_FETC                                        641
         GO TO  5620                                                      641
5630     CONTINUE                                                         641
C    *                                                                    641
C    Geant POSITION : FETC ===> X=(XOFFFEDC)/2.0 Y=0 Z=0.0 <===           642
         CALL AGSRESET                                                    642
         AG_X = (XOFFFEDC)/2.0                                            642
         AG_Y = 0                                                         642
         AG_Z = 0.0                                                       642
         AG_TITLE = 'FETC'                                                642
         AG_EXNAME = 'POSITION'                                           642
         AG_PARLIST = 'X_Y_Z'                                             642
         CALL AXPOSITION                                                  642
C    *                                                                    642
C    Create FESC :                                                        643
C    Call To Local Procedure FESC                                         643
         ASSIGN 5640 TO BLOCK_FESC                                        643
         GO TO  5600                                                      643
5640     CONTINUE                                                         643
C    *                                                                    643
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0  645
C    ) Y=INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                              645
         CALL AGSRESET                                                    645
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0)                   645
         AG_Y = INSE_RA*SIN(PI*5.0/12.0)                                  645
         AG_Z = 0.0                                                       645
         AG_TITLE = 'FESC'                                                645
         AG_EXNAME = 'POSITION'                                           645
         AG_PARLIST = 'X_Y_Z'                                             645
         CALL AXPOSITION                                                  645
C    *                                                                    645
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0) Y=I  645
C    NSE_RA*SIN(PI/4.0) Z=0.0 <===                                        645
         CALL AGSRESET                                                    646
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0)                        646
         AG_Y = INSE_RA*SIN(PI/4.0)                                       646
         AG_Z = 0.0                                                       646
         AG_TITLE = 'FESC'                                                646
         AG_EXNAME = 'POSITION'                                           646
         AG_PARLIST = 'X_Y_Z'                                             646
         CALL AXPOSITION                                                  646
C    *                                                                    646
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0) Y=  646
C    INSE_RA*SIN(PI/12.0) Z=0.0 <===                                      646
         CALL AGSRESET                                                    648
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0)                       648
         AG_Y = INSE_RA*SIN(PI/12.0)                                      648
         AG_Z = 0.0                                                       648
         AG_TITLE = 'FESC'                                                648
         AG_EXNAME = 'POSITION'                                           648
         AG_PARLIST = 'X_Y_Z'                                             648
         CALL AXPOSITION                                                  648
C    *                                                                    648
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0) Y=  648
C    -INSE_RA*SIN(PI/12.0) Z=0.0 <===                                     648
         CALL AGSRESET                                                    650
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0)                       650
         AG_Y = -INSE_RA*SIN(PI/12.0)                                     650
         AG_Z = 0.0                                                       650
         AG_TITLE = 'FESC'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'X_Y_Z'                                             650
         CALL AXPOSITION                                                  650
C    *                                                                    650
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0) Y=-  650
C    INSE_RA*SIN(PI/4.0) Z=0.0 <===                                       650
         CALL AGSRESET                                                    651
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0)                        651
         AG_Y = -INSE_RA*SIN(PI/4.0)                                      651
         AG_Z = 0.0                                                       651
         AG_TITLE = 'FESC'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'X_Y_Z'                                             651
         CALL AXPOSITION                                                  651
C    *                                                                    651
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0  651
C    ) Y=-INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                             651
         CALL AGSRESET                                                    653
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0)                   653
         AG_Y = -INSE_RA*SIN(PI*5.0/12.0)                                 653
         AG_Z = 0.0                                                       653
         AG_TITLE = 'FESC'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'X_Y_Z'                                             653
         CALL AXPOSITION                                                  653
C    *                                                                    653
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0) Y=I  653
C    NSE_RB*SIN(PI/4.0) Z=0.0 <===                                        653
         CALL AGSRESET                                                    654
         AG_X = (XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0)                        654
         AG_Y = INSE_RB*SIN(PI/4.0)                                       654
         AG_Z = 0.0                                                       654
         AG_TITLE = 'FESC'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'X_Y_Z'                                             654
         CALL AXPOSITION                                                  654
C    *                                                                    654
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0) Y=-  654
C    INSE_RB*SIN(PI/4.0) Z=0.0 <===                                       654
         CALL AGSRESET                                                    655
         AG_X = (XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0)                        655
         AG_Y = -INSE_RB*SIN(PI/4.0)                                      655
         AG_Z = 0.0                                                       655
         AG_TITLE = 'FESC'                                                655
         AG_EXNAME = 'POSITION'                                           655
         AG_PARLIST = 'X_Y_Z'                                             655
         CALL AXPOSITION                                                  655
C    *                                                                    655
5461  CALL AGSPOPB                                                        656
      GO TO BLOCK_FEEC, (5460 ,5470)                                      656
C End of Local Procedure                                                  656
C ----------------------------------------------------------------------  657
C ------------ FETC                                                       657
C Begin Local Procedure FETC                                              658
C FETC IS AIR ENCLOSURE PART                                              658
5620  CONTINUE                                                            658
         AG_TITLE='FETC'                                                  658
         AG_MODULE=MODULE                                                 658
         AG_MARK='V00'                                                    658
         CALL AGSPUSH                                                     658
         AG_MODULE=MODULE                                                 658
C    *                                                                    658
C    Geant MATERIAL : AIR ===> inherited arguments used <===              659
         CALL AGSRESET                                                    659
         AG_TITLE = 'AIR'                                                 659
         AG_EXNAME = 'MATERIAL'                                           659
         AG_PARLIST = 'NONE'                                              659
         CALL AXMATERIAL                                                  659
C    *                                                                    659
C    Geant ATTRIBUTE : FETC ===> SEEN=1 COLO=6 <===                       659
         CALL AGSRESET                                                    660
         AG_SEEN = 1                                                      660
         AG_COLO = 6                                                      660
         AG_TITLE = 'FETC'                                                660
         AG_EXNAME = 'ATTRIBUTE'                                          660
         AG_PARLIST = 'SEEN_COLO'                                         660
         CALL AXATTRIBUTE                                                 660
C    *                                                                    660
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=INSE_RMAX PHI1=90, PHI2=-90 D  660
C    Z=INSE_GATEDEPTH/2.0 <===                                            660
         CALL AGSRESET                                                    662
         AG_RMIN = 0.0                                                    662
         AG_RMAX = INSE_RMAX                                              662
         AG_PHI1 = 90                                                     662
         AG_PHI2 = -90                                                    662
         AG_DZ = INSE_GATEDEPTH/2.0                                       662
         AG_TITLE = 'TUBS'                                                662
         AG_EXNAME = 'SHAPE'                                              662
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            662
         CALL AXSHAPE                                                     662
C    *                                                                    662
5621  CALL AGSPOPB                                                        663
      GO TO BLOCK_FETC, (5620 ,5630)                                      663
C End of Local Procedure                                                  663
C ----------------------------------------------------------------------  664
C ------------ FERC                                                       664
C Begin Local Procedure FERC                                              665
C FERC IS AIR ENCLOSURE PART                                              665
5580  CONTINUE                                                            665
         AG_TITLE='FERC'                                                  665
         AG_MODULE=MODULE                                                 665
         AG_MARK='V00'                                                    665
         CALL AGSPUSH                                                     665
         AG_MODULE=MODULE                                                 665
C    *                                                                    665
C    Geant MATERIAL : AIR ===> inherited arguments used <===              666
         CALL AGSRESET                                                    666
         AG_TITLE = 'AIR'                                                 666
         AG_EXNAME = 'MATERIAL'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXMATERIAL                                                  666
C    *                                                                    666
C    Geant ATTRIBUTE : FERC ===> SEEN=1 COLO=6 <===                       666
         CALL AGSRESET                                                    667
         AG_SEEN = 1                                                      667
         AG_COLO = 6                                                      667
         AG_TITLE = 'FERC'                                                667
         AG_EXNAME = 'ATTRIBUTE'                                          667
         AG_PARLIST = 'SEEN_COLO'                                         667
         CALL AXATTRIBUTE                                                 667
C    *                                                                    667
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=INSE_RMAX PHI1=-90, PHI2=90 D  667
C    Z=INSE_GATEDEPTH/2.0 <===                                            667
         CALL AGSRESET                                                    669
         AG_RMIN = 0.0                                                    669
         AG_RMAX = INSE_RMAX                                              669
         AG_PHI1 = -90                                                    669
         AG_PHI2 = 90                                                     669
         AG_DZ = INSE_GATEDEPTH/2.0                                       669
         AG_TITLE = 'TUBS'                                                669
         AG_EXNAME = 'SHAPE'                                              669
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            669
         CALL AXSHAPE                                                     669
C    *                                                                    669
5581  CALL AGSPOPB                                                        670
      GO TO BLOCK_FERC, (5580 ,5590)                                      670
C End of Local Procedure                                                  670
C ----------------------------------------------------------------------  671
C ------------ FESC                                                       671
C Begin Local Procedure FESC                                              672
C FESC IS AIR ENCLOSURE PART                                              672
5600  CONTINUE                                                            672
         AG_TITLE='FESC'                                                  672
         AG_MODULE=MODULE                                                 672
         AG_MARK='V00'                                                    672
         CALL AGSPUSH                                                     672
         AG_MODULE=MODULE                                                 672
C    *                                                                    672
C    Geant MATERIAL : AIR ===> inherited arguments used <===              673
         CALL AGSRESET                                                    673
         AG_TITLE = 'AIR'                                                 673
         AG_EXNAME = 'MATERIAL'                                           673
         AG_PARLIST = 'NONE'                                              673
         CALL AXMATERIAL                                                  673
C    *                                                                    673
C    Geant ATTRIBUTE : FESC ===> SEEN=1 COLO=6 <===                       673
         CALL AGSRESET                                                    674
         AG_SEEN = 1                                                      674
         AG_COLO = 6                                                      674
         AG_TITLE = 'FESC'                                                674
         AG_EXNAME = 'ATTRIBUTE'                                          674
         AG_PARLIST = 'SEEN_COLO'                                         674
         CALL AXATTRIBUTE                                                 674
C    *                                                                    674
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=INSE_DIAM/2.0 DZ=INSE_GATEDEP  674
C    TH/2.0 <===                                                          674
         CALL AGSRESET                                                    675
         AG_RMIN = 0.0                                                    675
         AG_RMAX = INSE_DIAM/2.0                                          675
         AG_DZ = INSE_GATEDEPTH/2.0                                       675
         AG_TITLE = 'TUBE'                                                675
         AG_EXNAME = 'SHAPE'                                              675
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      675
         CALL AXSHAPE                                                     675
C    *                                                                    675
5601  CALL AGSPOPB                                                        676
      GO TO BLOCK_FESC, (5600 ,5610,5640)                                 676
C End of Local Procedure                                                  676
C ----------------------------------------------------------------------  677
C ------------ FPCT                                                       677
C Begin Local Procedure FPCT                                              678
C FPCT IS PHOTO CATHODE                                                   678
5520  CONTINUE                                                            678
         AG_TITLE='FPCT'                                                  678
         AG_MODULE=MODULE                                                 678
         AG_MARK='V00'                                                    678
         CALL AGSPUSH                                                     678
         AG_MODULE=MODULE                                                 678
C    *                                                                    678
C    Geant MATERIAL : AIR ===> inherited arguments used <===              679
         CALL AGSRESET                                                    679
         AG_TITLE = 'AIR'                                                 679
         AG_EXNAME = 'MATERIAL'                                           679
         AG_PARLIST = 'NONE'                                              679
         CALL AXMATERIAL                                                  679
C    *                                                                    679
C    Geant MEDIUM : PHOTCATH ===> ISVOL=1 <===                            679
         CALL AGSRESET                                                    680
         AG_ISVOL = 1                                                     680
         AG_TITLE = 'PHOTCATH'                                            680
         AG_EXNAME = 'MEDIUM'                                             680
         AG_PARLIST = 'ISVOL'                                             680
         CALL AXMEDIUM                                                    680
C    *                                                                    680
C    Geant ATTRIBUTE : LPCT ===> SEEN=1 COLO=6 <===                       680
         CALL AGSRESET                                                    681
         AG_SEEN = 1                                                      681
         AG_COLO = 6                                                      681
         AG_TITLE = 'LPCT'                                                681
         AG_EXNAME = 'ATTRIBUTE'                                          681
         AG_PARLIST = 'SEEN_COLO'                                         681
         CALL AXATTRIBUTE                                                 681
C    *                                                                    681
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=FLGG_PHCATHR DZ=FLGG_PHCATHDZ/2  681
C    .0 <===                                                              681
         CALL AGSRESET                                                    682
         AG_RMIN = 0                                                      682
         AG_RMAX = FLGG_PHCATHR                                           682
         AG_DZ = FLGG_PHCATHDZ/2.0                                        682
         AG_TITLE = 'TUBE'                                                682
         AG_EXNAME = 'SHAPE'                                              682
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      682
         CALL AXSHAPE                                                     682
C    *                                                                    682
C    Check FMCG_CHKVSIM==1                                                682
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      683
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PHCATH,EFFIC_PHCATH,              684
     *   RINDEX_PHCATH )                                                  684
C    Defining Hits: USER:0:(0,100000)                                     685
         AG_TITLE = 'FPCT'                                                685
         AG_EXNAME = 'HITS'                                               685
         AG_TYPE=AG_MAGIC                                                 685
         CALL AGSDETV('FPCT')                                             685
C    *                                                                    685
         AG_PARLIST='USER'                                                685
         AG_TITLE = 'DETH'                                                685
         AG_NBITS = 0                                                     685
         AG_BIN=AG_MAGIC                                                  685
         AG_OPTION='*'                                                    685
         AG_FHMIN = 0                                                     685
         AG_FHMAX = 100000                                                685
         CALL AGSDETH (JUMPAD(FPCTSTEP))                                  685
C    *                                                                    685
         CALL AGSDETU('FPCT')                                             685
C    *                                                                    685
         ENDIF                                                            686
5521  CALL AGSPOPB                                                        687
      GO TO BLOCK_FPCT, (5520 ,5530)                                      687
C End of Local Procedure                                                  687
C ----------------------------------------------------------------------  688
C ------------ FUMT                                                       688
C Begin Local Procedure FUMT                                              689
C FUMT IS MU METAL                                                        689
5500  CONTINUE                                                            689
         AG_TITLE='FUMT'                                                  689
         AG_MODULE=MODULE                                                 689
         AG_MARK='V00'                                                    689
         CALL AGSPUSH                                                     689
         AG_MODULE=MODULE                                                 689
C    *                                                                    689
C    Geant MATERIAL : IRON ===> inherited arguments used <===             690
         CALL AGSRESET                                                    690
         AG_TITLE = 'IRON'                                                690
         AG_EXNAME = 'MATERIAL'                                           690
         AG_PARLIST = 'NONE'                                              690
         CALL AXMATERIAL                                                  690
C    *                                                                    690
C    Geant ATTRIBUTE : LUMT ===> SEEN=1 COLO=5 <===                       690
         CALL AGSRESET                                                    691
         AG_SEEN = 1                                                      691
         AG_COLO = 5                                                      691
         AG_TITLE = 'LUMT'                                                691
         AG_EXNAME = 'ATTRIBUTE'                                          691
         AG_PARLIST = 'SEEN_COLO'                                         691
         CALL AXATTRIBUTE                                                 691
C    *                                                                    691
C    Geant SHAPE : TUBE ===> RMIN=FLGG_PHCATHR RMAX=FLGG_MUMETR, DZ=FLGG  691
C    _MUMETDZ/2.0 <===                                                    691
         CALL AGSRESET                                                    693
         AG_RMIN = FLGG_PHCATHR                                           693
         AG_RMAX = FLGG_MUMETR                                            693
         AG_DZ = FLGG_MUMETDZ/2.0                                         693
         AG_TITLE = 'TUBE'                                                693
         AG_EXNAME = 'SHAPE'                                              693
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      693
         CALL AXSHAPE                                                     693
C    *                                                                    693
C    Check FMCG_CHKVSIM==1                                                693
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      694
         CALL GSCKOV( AG_IMED,N,E,ABSCO_MUMET,EFFIC_ALL,RINDEX_MUMET )    695
         ENDIF                                                            696
5501  CALL AGSPOPB                                                        697
      GO TO BLOCK_FUMT, (5500 ,5510)                                      697
C End of Local Procedure                                                  697
C ----------------------------------------------------------------------  698
C ------------ PBPT                                                       698
C Begin Local Procedure PBPT                                              699
C PBPT IS PB PLATE                                                        699
5190  CONTINUE                                                            699
         AG_TITLE='PBPT'                                                  699
         AG_MODULE=MODULE                                                 699
         AG_MARK='V00'                                                    699
         CALL AGSPUSH                                                     699
         AG_MODULE=MODULE                                                 699
C    *                                                                    699
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             700
         CALL AGSRESET                                                    700
         AG_TITLE = 'LEAD'                                                700
         AG_EXNAME = 'MATERIAL'                                           700
         AG_PARLIST = 'NONE'                                              700
         CALL AXMATERIAL                                                  700
C    *                                                                    700
C    Geant ATTRIBUTE : PBPT ===> SEEN=1 COLO=7 <===                       700
         CALL AGSRESET                                                    701
         AG_SEEN = 1                                                      701
         AG_COLO = 7                                                      701
         AG_TITLE = 'PBPT'                                                701
         AG_EXNAME = 'ATTRIBUTE'                                          701
         AG_PARLIST = 'SEEN_COLO'                                         701
         CALL AXATTRIBUTE                                                 701
C    *                                                                    701
C    Geant SHAPE : BOX ===> DX=PBPD_WIDTH/2.0 DY=PBPD_HEIGHT/2.0, DZ=PBP  701
C    D_THICK/2.0 <===                                                     701
         CALL AGSRESET                                                    703
         AG_DX = PBPD_WIDTH/2.0                                           703
         AG_DY = PBPD_HEIGHT/2.0                                          703
         AG_DZ = PBPD_THICK/2.0                                           703
         AG_TITLE = 'BOX'                                                 703
         AG_EXNAME = 'SHAPE'                                              703
         AG_PARLIST = 'DX_DY_DZ'                                          703
         CALL AXSHAPE                                                     703
C    *                                                                    703
5191  CALL AGSPOPB                                                        704
      GO TO BLOCK_PBPT, (5190 ,5200,5280)                                 704
C End of Local Procedure                                                  704
C ----------------------------------------------------------------------  705
C ------------ FSHM                                                       705
C Begin Local Procedure FSHM                                              706
C FSHM IS THE SHOWER MAX SECTION                                          706
5210  CONTINUE                                                            706
         AG_TITLE='FSHM'                                                  706
         AG_MODULE=MODULE                                                 706
         AG_MARK='V00'                                                    706
         CALL AGSPUSH                                                     706
         AG_MODULE=MODULE                                                 706
C    *                                                                    706
C    Geant MATERIAL : AIR ===> inherited arguments used <===              707
         CALL AGSRESET                                                    707
         AG_TITLE = 'AIR'                                                 707
         AG_EXNAME = 'MATERIAL'                                           707
         AG_PARLIST = 'NONE'                                              707
         CALL AXMATERIAL                                                  707
C    *                                                                    707
C    Geant ATTRIBUTE : FSHM ===> SEEN=1 COLO=4 <===                       707
         CALL AGSRESET                                                    708
         AG_SEEN = 1                                                      708
         AG_COLO = 4                                                      708
         AG_TITLE = 'FSHM'                                                708
         AG_EXNAME = 'ATTRIBUTE'                                          708
         AG_PARLIST = 'SEEN_COLO'                                         708
         CALL AXATTRIBUTE                                                 708
C    *                                                                    708
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2.0 DY=FMXG_G10HGT/2.0, DZ=  708
C    ZTOTSMD <===                                                         708
         CALL AGSRESET                                                    710
         AG_DX = FMXG_G10WIDTH/2.0                                        710
         AG_DY = FMXG_G10HGT/2.0                                          710
         AG_DZ = ZTOTSMD                                                  710
         AG_TITLE = 'BOX'                                                 710
         AG_EXNAME = 'SHAPE'                                              710
         AG_PARLIST = 'DX_DY_DZ'                                          710
         CALL AXSHAPE                                                     710
C    *                                                                    710
         WSMD=FMXG_SBASE/2.0 + FMXG_SGAP                                  710
         WTOTSMD=(FMXG_NSTRIP + 1)*WSMD                                   712
         ZSMD= - ZTOTSMD + FMXG_G10THICK/2.0                              713
C    Create FXGT :                                                        714
C    Call To Local Procedure FXGT                                         714
         ASSIGN 5660 TO BLOCK_FXGT                                        714
         GO TO  5650                                                      714
5660     CONTINUE                                                         714
C    *                                                                    714
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZSMD <===                       715
         CALL AGSRESET                                                    715
         AG_X = 0                                                         715
         AG_Y = 0                                                         715
         AG_Z = ZSMD                                                      715
         AG_TITLE = 'FXGT'                                                715
         AG_EXNAME = 'POSITION'                                           715
         AG_PARLIST = 'X_Y_Z'                                             715
         CALL AXPOSITION                                                  715
C    *                                                                    715
         XSMDV= - WTOTSMD/2.0 - FMXG_SGAP/2.0 + WSMD                      715
         YSMDV=0.0                                                        717
         ZSMDV=ZSMD + FMXG_G10THICK/2.0 + FMXG_SAPEX/2.0                  718
C    *                                                                    719
         I=1                                                              719
            GO TO 5673                                                    719
5671        I=I+1                                                         719
5673        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5672                           719
C       Check MOD(I,2)!=0                                                 720
            IF (MOD(I,2).NE.0) THEN                                       720
C       Create FHMS :                                                     721
C       Call To Local Procedure FHMS                                      721
            ASSIGN 5690 TO BLOCK_FHMS                                     721
            GO TO  5680                                                   721
5690        CONTINUE                                                      721
C       *                                                                 721
C       Geant POSITION : FHMS ===> X=XSMDV Y=YSMDV Z=ZSMDV <===           722
            CALL AGSRESET                                                 722
            AG_X = XSMDV                                                  722
            AG_Y = YSMDV                                                  722
            AG_Z = ZSMDV                                                  722
            AG_TITLE = 'FHMS'                                             722
            AG_EXNAME = 'POSITION'                                        722
            AG_PARLIST = 'X_Y_Z'                                          722
            CALL AXPOSITION                                               722
C       *                                                                 722
            ELSE                                                          722
C       Create FHMS :                                                     724
C       Call To Local Procedure FHMS                                      724
            ASSIGN 5700 TO BLOCK_FHMS                                     724
            GO TO  5680                                                   724
5700        CONTINUE                                                      724
C       *                                                                 724
C       Geant POSITION : FHMS ===> X=XSMDV Y=YSMDV Z=ZSMDV ALPHAX=180 <=  725
C       ==                                                                725
            CALL AGSRESET                                                 725
            AG_X = XSMDV                                                  725
            AG_Y = YSMDV                                                  725
            AG_Z = ZSMDV                                                  725
            AG_ALPHAN=AG_ALPHAN+1                                         725
            AG_ALPHAK(AG_ALPHAN)=1                                        725
            AG_ALPHAL(AG_ALPHAN)=180                                      725
            AG_TITLE = 'FHMS'                                             725
            AG_EXNAME = 'POSITION'                                        725
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   725
            CALL AXPOSITION                                               725
C       *                                                                 725
            ENDIF                                                         725
            XSMDV=XSMDV+WSMD                                              727
         GO TO 5671                                                       728
5672     CONTINUE                                                         728
         ZSMD2=ZSMDV + FMXG_G10THICK/2.0 + FMXG_SAPEX/2.0                 729
C    Create FXGT :                                                        730
C    Call To Local Procedure FXGT                                         730
         ASSIGN 5710 TO BLOCK_FXGT                                        730
         GO TO  5650                                                      730
5710     CONTINUE                                                         730
C    *                                                                    730
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZSMD2 <===                      731
         CALL AGSRESET                                                    731
         AG_X = 0                                                         731
         AG_Y = 0                                                         731
         AG_Z = ZSMD2                                                     731
         AG_TITLE = 'FXGT'                                                731
         AG_EXNAME = 'POSITION'                                           731
         AG_PARLIST = 'X_Y_Z'                                             731
         CALL AXPOSITION                                                  731
C    *                                                                    731
         XSMDH=0.0                                                        731
         YSMDH= - WTOTSMD/2.0 - FMXG_SGAP/2.0 + WSMD                      733
         ZSMDH=ZSMD2 + FMXG_G10THICK/2.0 + FMXG_SAPEX/2.0                 734
C    Create FHMS :                                                        735
C    Call To Local Procedure FHMS                                         735
         ASSIGN 5720 TO BLOCK_FHMS                                        735
         GO TO  5680                                                      735
5720     CONTINUE                                                         735
C    *                                                                    735
C    *                                                                    736
         I=1                                                              736
            GO TO 5733                                                    736
5731        I=I+1                                                         736
5733        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5732                           736
C       Check MOD(I,2)!=0                                                 737
            IF (MOD(I,2).NE.0) THEN                                       737
C       Geant POSITION : FHMS ===> X=XSMDH Y=YSMDH Z=ZSMDH ORT=Y-XZ <===  738
            CALL AGSRESET                                                 738
            AG_X = XSMDH                                                  738
            AG_Y = YSMDH                                                  738
            AG_Z = ZSMDH                                                  738
            AG_ALPHAN=AG_ALPHAN+1                                         738
            AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                             738
            AG_TITLE = 'FHMS'                                             738
            AG_EXNAME = 'POSITION'                                        738
            AG_PARLIST = 'X_Y_Z_ORT'                                      738
            CALL AXPOSITION                                               738
C       *                                                                 738
            ELSE                                                          738
C       Geant POSITION : FHMS ===> X=XSMDH Y=YSMDH Z=ZSMDH ORT=YX-Z <===  740
            CALL AGSRESET                                                 740
            AG_X = XSMDH                                                  740
            AG_Y = YSMDH                                                  740
            AG_Z = ZSMDH                                                  740
            AG_ALPHAN=AG_ALPHAN+1                                         740
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             740
            AG_TITLE = 'FHMS'                                             740
            AG_EXNAME = 'POSITION'                                        740
            AG_PARLIST = 'X_Y_Z_ORT'                                      740
            CALL AXPOSITION                                               740
C       *                                                                 740
            ENDIF                                                         740
            YSMDH=YSMDH+WSMD                                              742
         GO TO 5731                                                       743
5732     CONTINUE                                                         743
5211  CALL AGSPOPB                                                        744
      GO TO BLOCK_FSHM, (5210 ,5220,5290)                                 744
C End of Local Procedure                                                  744
C ----------------------------------------------------------------------  745
C ------------ FXGT                                                       745
C Begin Local Procedure FXGT                                              746
C FXGT IS THE G10 LAYER IN THE SMAX                                       746
5650  CONTINUE                                                            746
         AG_TITLE='FXGT'                                                  746
         AG_MODULE=MODULE                                                 746
         AG_MARK='V00'                                                    746
         CALL AGSPUSH                                                     746
         AG_MODULE=MODULE                                                 746
C    *                                                                    746
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          747
         CALL AGSRESET                                                    747
         AG_A = 28.08                                                     747
         AG_Z = 14                                                        747
         AG_W = 0.6*1*28./60.                                             747
         AG_TITLE = 'SI'                                                  747
         AG_EXNAME = 'COMPONENT'                                          747
         AG_PARLIST = 'A_Z_W'                                             747
         CALL AXCOMPONENT                                                 747
C    *                                                                    747
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               747
         CALL AGSRESET                                                    748
         AG_A = 16                                                        748
         AG_Z = 8                                                         748
         AG_W = 0.6*2*16./60.                                             748
         AG_TITLE = 'O'                                                   748
         AG_EXNAME = 'COMPONENT'                                          748
         AG_PARLIST = 'A_Z_W'                                             748
         CALL AXCOMPONENT                                                 748
C    *                                                                    748
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              748
         CALL AGSRESET                                                    749
         AG_A = 12                                                        749
         AG_Z = 6                                                         749
         AG_W = 0.4*8*12./174.                                            749
         AG_TITLE = 'C'                                                   749
         AG_EXNAME = 'COMPONENT'                                          749
         AG_PARLIST = 'A_Z_W'                                             749
         CALL AXCOMPONENT                                                 749
C    *                                                                    749
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               749
         CALL AGSRESET                                                    750
         AG_A = 1                                                         750
         AG_Z = 1                                                         750
         AG_W = 0.4*14*1./174.                                            750
         AG_TITLE = 'H'                                                   750
         AG_EXNAME = 'COMPONENT'                                          750
         AG_PARLIST = 'A_Z_W'                                             750
         CALL AXCOMPONENT                                                 750
C    *                                                                    750
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              750
         CALL AGSRESET                                                    751
         AG_A = 16                                                        751
         AG_Z = 8                                                         751
         AG_W = 0.4*4*16./174.                                            751
         AG_TITLE = 'O'                                                   751
         AG_EXNAME = 'COMPONENT'                                          751
         AG_PARLIST = 'A_Z_W'                                             751
         CALL AXCOMPONENT                                                 751
C    *                                                                    751
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               751
         CALL AGSRESET                                                    752
         AG_DENS = 1.7                                                    752
         AG_TITLE = 'G10'                                                 752
         AG_EXNAME = 'MIXTURE'                                            752
         AG_PARLIST = 'DENS'                                              752
         CALL AXMIXTURE                                                   752
C    *                                                                    752
C    Geant ATTRIBUTE : FXGT ===> SEEN=1 COLO=7 <===                       752
         CALL AGSRESET                                                    753
         AG_SEEN = 1                                                      753
         AG_COLO = 7                                                      753
         AG_TITLE = 'FXGT'                                                753
         AG_EXNAME = 'ATTRIBUTE'                                          753
         AG_PARLIST = 'SEEN_COLO'                                         753
         CALL AXATTRIBUTE                                                 753
C    *                                                                    753
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2 DY=FMXG_G10HGT/2, DZ=FMXG  753
C    _G10THICK/2 <===                                                     753
         CALL AGSRESET                                                    755
         AG_DX = FMXG_G10WIDTH/2                                          755
         AG_DY = FMXG_G10HGT/2                                            755
         AG_DZ = FMXG_G10THICK/2                                          755
         AG_TITLE = 'BOX'                                                 755
         AG_EXNAME = 'SHAPE'                                              755
         AG_PARLIST = 'DX_DY_DZ'                                          755
         CALL AXSHAPE                                                     755
C    *                                                                    755
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           755
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           757
5651  CALL AGSPOPB                                                        758
      GO TO BLOCK_FXGT, (5650 ,5660,5710)                                 758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ FHMS                                                       759
C Begin Local Procedure FHMS                                              760
C FHMS IS SHOWER MAX STRIP                                                760
5680  CONTINUE                                                            760
         AG_TITLE='FHMS'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       761
         CALL AGSRESET                                                    761
         AG_TITLE = 'POLYSTYREN'                                          761
         AG_EXNAME = 'MATERIAL'                                           761
         AG_PARLIST = 'NONE'                                              761
         CALL AXMATERIAL                                                  761
C    *                                                                    761
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       761
         CALL AGSRESET                                                    762
         AG_ISVOL = 1                                                     762
         AG_TITLE = 'CPOLYSTYREN'                                         762
         AG_EXNAME = 'MATERIAL'                                           762
         AG_PARLIST = 'ISVOL'                                             762
         CALL AXMATERIAL                                                  762
C    *                                                                    762
C    Geant ATTRIBUTE : FHMS ===> SEEN=1 COLO=2 <===                       762
         CALL AGSRESET                                                    763
         AG_SEEN = 1                                                      763
         AG_COLO = 2                                                      763
         AG_TITLE = 'FHMS'                                                763
         AG_EXNAME = 'ATTRIBUTE'                                          763
         AG_PARLIST = 'SEEN_COLO'                                         763
         CALL AXATTRIBUTE                                                 763
C    *                                                                    763
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=FMXG_SBASE/2.0 DY=FMXG_G10HGT/2.0  763
C    , DZ=FMXG_SAPEX/2.0 <===                                             763
         CALL AGSRESET                                                    765
         AG_DX1 = 0                                                       765
         AG_DX2 = FMXG_SBASE/2.0                                          765
         AG_DY = FMXG_G10HGT/2.0                                          765
         AG_DZ = FMXG_SAPEX/2.0                                           765
         AG_TITLE = 'TRD1'                                                765
         AG_EXNAME = 'SHAPE'                                              765
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     765
         CALL AXSHAPE                                                     765
C    *                                                                    765
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           765
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             767
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             768
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                769
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.0130)                            770
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            771
C    Defining Hits: BIRK:0:(0,10)                                         772
         AG_TITLE = 'FHMS'                                                772
         AG_EXNAME = 'HITS'                                               772
         AG_TYPE=AG_MAGIC                                                 772
         CALL AGSDETV('FHMS')                                             772
C    *                                                                    772
         AG_PARLIST='BIRK'                                                772
         AG_TITLE = 'DETH'                                                772
         AG_NBITS = 0                                                     772
         AG_BIN=AG_MAGIC                                                  772
         AG_OPTION='*'                                                    772
         AG_FHMIN = 0                                                     772
         AG_FHMAX = 10                                                    772
         CALL AGSDETH (0)                                                 772
C    *                                                                    772
         CALL AGSDETU('FHMS')                                             772
C    *                                                                    772
5681  CALL AGSPOPB                                                        773
      GO TO BLOCK_FHMS, (5680 ,5690,5700,5720)                            773
C End of Local Procedure                                                  773
C ----------------------------------------------------------------------  774
C ------------ FPRS                                                       774
C Begin Local Procedure FPRS                                              775
C FPRS IS BOX FOR PLASTIC SCHINTI FMS PRE-SHOWER DETECTORS                775
5050  CONTINUE                                                            775
         AG_TITLE='FPRS'                                                  775
         AG_MODULE=MODULE                                                 775
         AG_MARK='V00'                                                    775
         CALL AGSPUSH                                                     775
         AG_MODULE=MODULE                                                 775
C    *                                                                    775
C    Geant MATERIAL : AIR ===> inherited arguments used <===              776
         CALL AGSRESET                                                    776
         AG_TITLE = 'AIR'                                                 776
         AG_EXNAME = 'MATERIAL'                                           776
         AG_PARLIST = 'NONE'                                              776
         CALL AXMATERIAL                                                  776
C    *                                                                    776
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           776
         CALL AGSRESET                                                    777
         AG_TITLE = 'STANDARD'                                            777
         AG_EXNAME = 'MEDIUM'                                             777
         AG_PARLIST = 'NONE'                                              777
         CALL AXMEDIUM                                                    777
C    *                                                                    777
C    Geant ATTRIBUTE : FPRS ===> SEEN=0 COLO=2 <===                       777
         CALL AGSRESET                                                    778
         AG_SEEN = 0                                                      778
         AG_COLO = 2                                                      778
         AG_TITLE = 'FPRS'                                                778
         AG_EXNAME = 'ATTRIBUTE'                                          778
         AG_PARLIST = 'SEEN_COLO'                                         778
         CALL AXATTRIBUTE                                                 778
C    *                                                                    778
C    Geant SHAPE : PGON ===> PHI1=45 DPHI=360 NPDIV=4 NZ=2, ZI={-PRSW_DB  778
C    OX(3)/2.0,PRSW_DBOX(3)/2.0} RMN={PRSW_DHOLE-PRSW_DXBBD(1), PRSW_DHO  778
C    LE-PRSW_DXBBD(1)}, RMX={PRSW_DBOX(1)/2.0,PRSW_DBOX(1)/2.0} <===      778
         CALL AGSRESET                                                    782
         AG_PHI1 = 45                                                     782
         AG_DPHI = 360                                                    782
         AG_NPDIV = 4                                                     782
         AG_NZ = 2                                                        782
         AG_ZI(1)=-PRSW_DBOX(3)/2.0                                       782
         AG_ZI(2)=PRSW_DBOX(3)/2.0                                        782
         AG_RMN(1)=PRSW_DHOLE-PRSW_DXBBD(1)                               782
         AG_RMN(2)=PRSW_DHOLE-PRSW_DXBBD(1)                               782
         AG_RMX(1)=PRSW_DBOX(1)/2.0                                       782
         AG_RMX(2)=PRSW_DBOX(1)/2.0                                       782
         AG_TITLE = 'PGON'                                                782
         AG_EXNAME = 'SHAPE'                                              782
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     782
         CALL AXSHAPE                                                     782
C    *                                                                    782
         DEBUG=0                                                          782
C    Check DEBUG.GE.1                                                     784
         IF (DEBUG.GE.1) THEN                                             784
         NPSTYPE=0                                                        785
         CPSTYPE(1)=1                                                     786
         CPSTYPE(2)=2                                                     787
         CPSTYPE(3)=6                                                     788
         CPSTYPE(4)=3                                                     789
         CPSTYPE(5)=4                                                     790
         CPSTYPE(6)=7                                                     791
         CPSTYPE(7)=5                                                     792
         CPSTYPE(8)=8                                                     793
         OPEN(11,FILE='fpsgeom.txt',STATUS='REPLACE')                     794
         ENDIF                                                            795
         SERN=1                                                           796
         DX=PRSW_DBOX(1)/2.0                                              797
         DY=PRSW_DBOX(2)/2.0                                              798
         DZ=PRSW_DZ(1)/2.0 + PRSW_DZBBD(1)/2.0 + PRSW_DSIPM(1)/2.0        799
         ZZ= - PRSW_DBOX(3)/2.0 + DZ                                      800
C    Create FPLY :                                                        801
C    Call To Local Procedure FPLY                                         801
         ASSIGN 5750 TO BLOCK_FPLY                                        801
         GO TO  5740                                                      801
5750     CONTINUE                                                         801
C    *                                                                    801
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      802
         CALL AGSRESET                                                    802
         AG_MOTHER = 'FPRS'                                               802
         AG_Z = ZZ                                                        802
         AG_KONLY = 'MANY'                                                802
         AG_TITLE = 'FPLY'                                                802
         AG_EXNAME = 'POSITION'                                           802
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    802
         CALL AXPOSITION                                                  802
C    *                                                                    802
         SERN=2                                                           802
         DX=PRSW_DBOX(1)/2.0                                              804
         DY=PRSW_DBOX(2)/2.0                                              805
         DZ=PRSW_DZ(2)/2.0 + PRSW_DZBBD(2)/2.0 + PRSW_DSIPM(1)/2.0        806
         ZZ= - PRSW_DBOX(3)/2.0 + DZ + PRSW_ZOFF(2) - PRSW_ZOFF(1)        807
C    Create FPLY :                                                        808
C    Call To Local Procedure FPLY                                         808
         ASSIGN 5760 TO BLOCK_FPLY                                        808
         GO TO  5740                                                      808
5760     CONTINUE                                                         808
C    *                                                                    808
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      809
         CALL AGSRESET                                                    809
         AG_MOTHER = 'FPRS'                                               809
         AG_Z = ZZ                                                        809
         AG_KONLY = 'MANY'                                                809
         AG_TITLE = 'FPLY'                                                809
         AG_EXNAME = 'POSITION'                                           809
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    809
         CALL AXPOSITION                                                  809
C    *                                                                    809
         SERN=3                                                           809
         DX=PRSW_DBOX(1)/2.0                                              811
         DY=PRSW_DBOX(2)/2.0                                              812
         DZ=PRSW_DZ(3)/2.0 + PRSW_DZBBD(3)/2.0                            813
         ZZ= - PRSW_DBOX(3)/2.0 + PRSW_DSIPM(1) + DZ - PRSW_DZBBD(3)/     815
     *   2.0 + PRSW_ZOFF(3) - PRSW_ZOFF(1)                                815
         DZSB = PRSW_DZ(3)/2.0 + PRSW_DZBBD(3)/4.0                        816
C    Create FPLY :                                                        817
C    Call To Local Procedure FPLY                                         817
         ASSIGN 5770 TO BLOCK_FPLY                                        817
         GO TO  5740                                                      817
5770     CONTINUE                                                         817
C    *                                                                    817
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      818
         CALL AGSRESET                                                    818
         AG_MOTHER = 'FPRS'                                               818
         AG_Z = ZZ                                                        818
         AG_KONLY = 'MANY'                                                818
         AG_TITLE = 'FPLY'                                                818
         AG_EXNAME = 'POSITION'                                           818
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    818
         CALL AXPOSITION                                                  818
C    *                                                                    818
         SERN=4                                                           818
         DX=PRSW_DBOX(1)/2.0                                              820
         DY=PRSW_DBOX(2)/2.0                                              821
         DZ=PRSW_DZ(4)/2.0 + PRSW_DZBBD(4)/2.0 + PRSW_DSIPM(1)/2.0        822
         ZZ= - PRSW_DBOX(3)/2.0 + DZ + PRSW_DSIPM(1) + PRSW_ZOFF(4) -     824
     *   PRSW_ZOFF(1) - PRSW_DZBBD(4)                                     824
C    Create FPLY :                                                        825
C    Call To Local Procedure FPLY                                         825
         ASSIGN 5780 TO BLOCK_FPLY                                        825
         GO TO  5740                                                      825
5780     CONTINUE                                                         825
C    *                                                                    825
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      826
         CALL AGSRESET                                                    826
         AG_MOTHER = 'FPRS'                                               826
         AG_Z = ZZ                                                        826
         AG_KONLY = 'MANY'                                                826
         AG_TITLE = 'FPLY'                                                826
         AG_EXNAME = 'POSITION'                                           826
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    826
         CALL AXPOSITION                                                  826
C    *                                                                    826
C    Check DEBUG.GE.1                                                     826
         IF (DEBUG.GE.1) THEN                                             827
         OPEN(10,FILE='prs.txt',STATUS='REPLACE')                         828
         K=0                                                              829
C    *                                                                    830
         I=1                                                              830
            GO TO 5793                                                    830
5791        I=I+1                                                         830
5793        IF(I-(NPSTYPE).GT.0) GO TO 5792                               830
C       *                                                                 831
            J=1                                                           831
               GO TO 5803                                                 831
5801           J=J+1                                                      831
5803           IF(J-(NPSTYPE).GT.0) GO TO 5802                            831
C          Check I.EQ.CPSTYPE(J)                                          832
               IF (I.EQ.CPSTYPE(J)) THEN                                  832
               WRITE(*,'(A5,A1,I5,F8.2,F8.2,F8.2)') 'Type=',CHAR(ICHAR('  834
     *A')+CPSTYPE(J)-1), PSTYPE(J),PSDIM(J,1)*2,PSDIM(J,2)*2,1.0          834
               WRITE(10,'(A5,A1,I5,F8.2,F8.2,F8.2)') 'Type=',             836
     *         CHAR(ICHAR('A')+CPSTYPE(J)-1), PSTYPE(J),PSDIM(J,1)*2,     836
     *         PSDIM(J,2)*2,1.0                                           836
               K=K+PSTYPE(J)                                              837
               ENDIF                                                      838
            GO TO 5801                                                    839
5802        CONTINUE                                                      839
         GO TO 5791                                                       840
5792     CONTINUE                                                         840
         WRITE(10,*) 'Total=',K                                           841
         CLOSE(10)                                                        842
         CLOSE(11)                                                        843
         ENDIF                                                            844
5051  CALL AGSPOPB                                                        845
      GO TO BLOCK_FPRS, (5050 ,5060)                                      845
C End of Local Procedure                                                  845
C ----------------------------------------------------------------------  846
C ------------ FPLY                                                       846
C Begin Local Procedure FPLY                                              847
C FPLY IS A LAYER OF FMS PRESHOWER                                        847
5740  CONTINUE                                                            847
         AG_TITLE='FPLY'                                                  847
         AG_MODULE=MODULE                                                 847
         AG_MARK='V00'                                                    847
         CALL AGSPUSH                                                     847
         AG_MODULE=MODULE                                                 847
C    *                                                                    847
C    Geant MATERIAL : AIR ===> inherited arguments used <===              848
         CALL AGSRESET                                                    848
         AG_TITLE = 'AIR'                                                 848
         AG_EXNAME = 'MATERIAL'                                           848
         AG_PARLIST = 'NONE'                                              848
         CALL AXMATERIAL                                                  848
C    *                                                                    848
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           848
         CALL AGSRESET                                                    849
         AG_TITLE = 'STANDARD'                                            849
         AG_EXNAME = 'MEDIUM'                                             849
         AG_PARLIST = 'NONE'                                              849
         CALL AXMEDIUM                                                    849
C    *                                                                    849
C    Geant ATTRIBUTE : FPLY ===> SEEN=0 COLO=3 SERIAL=SERN <===           849
         CALL AGSRESET                                                    850
         AG_SEEN = 0                                                      850
         AG_COLO = 3                                                      850
         AG_SERIAL = SERN                                                 850
         AG_TITLE = 'FPLY'                                                850
         AG_EXNAME = 'ATTRIBUTE'                                          850
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  850
         CALL AXATTRIBUTE                                                 850
C    *                                                                    850
C    Geant SHAPE : PGON ===> PHI1=45 DPHI=360 NPDIV=4 NZ=2, ZI={-DZ,DZ}   850
C    RMN={PRSW_DHOLE-PRSW_DXBBD(1),PRSW_DHOLE-PRSW_DXBBD(1)}, RMX={PRSW_  850
C    DBOX(1)/2.0, PRSW_DBOX(1)/2.0} <===                                  850
         CALL AGSRESET                                                    854
         AG_PHI1 = 45                                                     854
         AG_DPHI = 360                                                    854
         AG_NPDIV = 4                                                     854
         AG_NZ = 2                                                        854
         AG_ZI(1)=-DZ                                                     854
         AG_ZI(2)=DZ                                                      854
         AG_RMN(1)=PRSW_DHOLE-PRSW_DXBBD(1)                               854
         AG_RMN(2)=PRSW_DHOLE-PRSW_DXBBD(1)                               854
         AG_RMX(1)=PRSW_DBOX(1)/2.0                                       854
         AG_RMX(2)=PRSW_DBOX(1)/2.0                                       854
         AG_TITLE = 'PGON'                                                854
         AG_EXNAME = 'SHAPE'                                              854
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     854
         CALL AXSHAPE                                                     854
C    *                                                                    854
         LPRS1=PRSW_NPRS(1)*(PRSW_DPRS(1) + PRSW_DPRS(3))                 854
         LPRS2=PRSW_NPRS(2)*(PRSW_DPRS(2) + PRSW_DPRS(3)) + LPRS1         856
         LPRS3=(PRSW_NPRS(2) - PRSW_NSKIPH)*(PRSW_DPRS(2) +               857
     *   PRSW_DPRS(3)) + LPRS1                                            857
         LPRS4=(PRSW_NPRS(1) - PRSW_NSKIPV)*(PRSW_DPRS(1) +               859
     *   PRSW_DPRS(3)) + PRSW_NPRS(2)*(PRSW_DPRS(2) + PRSW_DPRS(3))       859
         DLG=(PRSW_DLG(2) + PRSW_DLG(3) + PRSW_DSIPM(1))/2.0              860
         K = 0                                                            861
         DZZ = PRSW_DZ(SERN)/2.0                                          862
C    Check SERN.EQ.3                                                      863
         IF (SERN.EQ.3) THEN                                              863
C    PB IN MIDDLE, BACK PLATE IN FRONT AND BACK                           864
         Z1=0                                                             865
         Z2=0                                                             866
         ELSEIF (SERN.EQ.4) THEN                                          867
C    BACK BOARD FIRST FOR LAYER3                                          868
C    Z FOR SCHINTI                                                        869
         Z1=-DZ+PRSW_DZBBD(SERN)+DZZ                                      869
C    Z FOR BACK BOARD                                                     870
         Z2=-DZ+PRSW_DZBBD(SERN)/2.0                                      870
         ELSE                                                             871
C    BACK BOARD 2ND FOR LAYER1,2                                          872
C    Z FOR SCHINTI                                                        873
         Z1=-DZ+PRSW_DSIPM(1)+DZZ                                         873
C    Z FOR BACK BOARD                                                     874
         Z2=-DZ+PRSW_DSIPM(1)+DZZ*2+PRSW_DZBBD(SERN)/2.0                  874
         ENDIF                                                            875
         Z = PRSW_ZOFF(SERN)                                              876
C    Create FPSC :                                                        877
C    Call To Local Procedure FPSC                                         877
         ASSIGN 5820 TO BLOCK_FPSC                                        877
         GO TO  5810                                                      877
5820     CONTINUE                                                         877
C    *                                                                    877
C    Create FLIG :                                                        878
C    Call To Local Procedure FLIG                                         878
         ASSIGN 5840 TO BLOCK_FLIG                                        878
         GO TO  5830                                                      878
5840     CONTINUE                                                         878
C    *                                                                    878
C    Create FLLG :                                                        879
C    Call To Local Procedure FLLG                                         879
         ASSIGN 5860 TO BLOCK_FLLG                                        879
         GO TO  5850                                                      879
5860     CONTINUE                                                         879
C    *                                                                    879
C    Create WPAR :                                                        880
C    Call To Local Procedure WPAR                                         880
         ASSIGN 5880 TO BLOCK_WPAR                                        880
         GO TO  5870                                                      880
5880     CONTINUE                                                         880
C    *                                                                    880
C    Create FPBB :                                                        881
C    Call To Local Procedure FPBB                                         881
         ASSIGN 5900 TO BLOCK_FPBB                                        881
         GO TO  5890                                                      881
5900     CONTINUE                                                         881
C    *                                                                    881
C    Create FPPB :                                                        882
C    Call To Local Procedure FPPB                                         882
         ASSIGN 5920 TO BLOCK_FPPB                                        882
         GO TO  5910                                                      882
5920     CONTINUE                                                         882
C    *                                                                    882
C    Check DEBUG.GE.1 .AND. SERN.EQ.1                                     883
         IF (DEBUG.GE.1 .AND. SERN.EQ.1) THEN                             883
         OPEN(1,FILE='prs1map.txt',STATUS='REPLACE')                      884
         ENDIF                                                            885
C    Check DEBUG.GE.1 .AND. SERN.EQ.2                                     886
         IF (DEBUG.GE.1 .AND. SERN.EQ.2) THEN                             886
         OPEN(2,FILE='prs2map.txt',STATUS='REPLACE')                      887
         ENDIF                                                            888
C    Check DEBUG.GE.1 .AND. SERN.EQ.3                                     889
         IF (DEBUG.GE.1 .AND. SERN.EQ.3) THEN                             889
         OPEN(3,FILE='prsCmap.txt',STATUS='REPLACE')                      890
         ENDIF                                                            891
C    Check DEBUG.GE.1 .AND. SERN.EQ.4                                     892
         IF (DEBUG.GE.1 .AND. SERN.EQ.4) THEN                             892
         OPEN(4,FILE='prs3map.txt',STATUS='REPLACE')                      893
         ENDIF                                                            894
C    Check SERN.LE.2                                                      895
         IF (SERN.LE.2) THEN                                              895
         LAYR=SERN                                                        896
         ELSE                                                             897
         LAYR=SERN-1                                                      898
         ENDIF                                                            899
C    Check SERN.EQ.1                                                      900
         IF (SERN.EQ.1) THEN                                              900
C    *                                                                    901
         LR=1                                                             901
            GO TO 5933                                                    901
5931        LR=LR+1                                                       901
5933        IF(LR-(2).GT.0) GO TO 5932                                    901
C       *                                                                 902
            TB=1                                                          902
               GO TO 5943                                                 902
5941           TB=TB+1                                                    902
5943           IF(TB-(2).GT.0) GO TO 5942                                 902
               X=0.0                                                      903
               QUAD=(LR-1)*2+TB                                           904
               SLAT(QUAD,LAYR)=0                                          905
C          *                                                              906
               J=1                                                        906
                  GO TO 5953                                              906
5951              J=J+1                                                   906
5953              IF(J-(2).GT.0) GO TO 5952                               906
C             *                                                           907
                  I=1                                                     907
                     GO TO 5963                                           907
5961                 I=I+1                                                907
5963                 IF(I-(PRSW_NPRS(J)).GT.0) GO TO 5962                 907
                     DXX = PRSW_DPRS(J)/2.0                               908
                     X = X + DXX + PRSW_DPRS(3)/2.0                       909
C                Check TB.EQ.1                                            910
                     IF (TB.EQ.1) THEN                                    910
                     PWID=LPRS2                                           911
                     ELSE                                                 912
                     PWID=LPRS3-PRSW_DYCUT                                913
                     ENDIF                                                914
C                Check X.LE.PRSW_DHOLE                                    915
                     IF (X.LE.PRSW_DHOLE) THEN                            915
                     DYY = (PWID-PRSW_DHOLE)/2.0                          916
                     Y = DYY+PRSW_DHOLE                                   917
                     ELSE                                                 918
                     DYY = PWID/2.0                                       919
                     Y = DYY                                              920
                     ENDIF                                                921
                     YY=Y+DYY+DLG                                         922
C                Check TB.EQ.1                                            923
                     IF (TB.EQ.1) THEN                                    923
                     ROT=0                                                924
                     ENDIF                                                925
C                Check TB.EQ.2                                            926
                     IF (TB.EQ.2) THEN                                    926
                     Y=-Y                                                 926
                     YY=-YY                                               926
                     ROT=180                                              927
                     ENDIF                                                928
C                Check LR.EQ.1                                            929
                     IF (LR.EQ.1) THEN                                    929
                     XX=+X+PRSW_XOFF(SERN)                                930
                     ENDIF                                                931
C                Check LR.EQ.2                                            932
                     IF (LR.EQ.2) THEN                                    932
                     XX=-X-PRSW_XOFF(SERN)                                933
                     ENDIF                                                934
                     Y=Y+PRSW_YOFF(SERN)                                  934
                     YY=YY+PRSW_YOFF(SERN)                                934
C                Check TB.EQ.1 .OR. J.EQ.2 .OR. I.GT.PRSW_NSKIPV          936
                     IF (TB.EQ.1 .OR. J.EQ.2 .OR. I.GT.PRSW_NSKIPV)       936
     *               THEN                                                 936
C                Check DEBUG.GE.1                                         937
                     IF (DEBUG.GE.1) THEN                                 937
                     K = K+1                                              938
                     SLAT(QUAD,LAYR)=SLAT(QUAD,LAYR)+1                    939
                     ID=(QUAD-1)*3*21+(LAYR-1)*21+(SLAT(QUAD,LAYR)-1)     940
                     FLAG=0                                               941
C                *                                                        942
                     KK=1                                                 942
                        GO TO 5973                                        942
5971                    KK=KK+1                                           942
5973                    IF(KK-(NPSTYPE).GT.0) GO TO 5972                  942
C                   Check (PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,2).EQ.DYY).OR  944
C                   .(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK, 2).EQ.DXX)         944
                        IF ((PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,             944
     *                  2).EQ.DYY).OR.(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK,   944
     *                  2).EQ.DXX)) THEN                                  944
                        FLAG=KK                                           945
                        PSTYPE(KK)=PSTYPE(KK)+1                           946
                        ENDIF                                             947
                     GO TO 5971                                           948
5972                 CONTINUE                                             948
C                Check FLAG.EQ.0                                          949
                     IF (FLAG.EQ.0) THEN                                  949
                     NPSTYPE=NPSTYPE+1                                    950
                     FLAG=NPSTYPE                                         951
                     PSTYPE(FLAG)=1                                       952
C                Check DXX.GT.DYY                                         953
                     IF (DXX.GT.DYY) THEN                                 953
                     PSDIM(NPSTYPE,1)=DYY                                 954
                     PSDIM(NPSTYPE,2)=DXX                                 955
                     ELSE                                                 956
                     PSDIM(NPSTYPE,1)=DXX                                 957
                     PSDIM(NPSTYPE,2)=DYY                                 958
                     ENDIF                                                959
                     ENDIF                                                960
                     WRITE(SERN,'(I2,I3,F8.2,F8.2,F8.2,F8.2,F8.2,F8.2,2X  962
     *,A1)') SERN,K,DXX*2,DYY*2, DZZ*2,XX,Y,Z,                            962
     *               CHAR(ICHAR('A')+CPSTYPE(FLAG)-1)                     962
                     WRITE(11,'(I3,I2,I2,I3,F8.2,F8.2,F8.2,F8.2,F8.2,F8.  964
     *2)') ID,QUAD,LAYR,SLAT(QUAD, LAYR),DXX*2,DYY*2,DZZ*2,XX,Y,Z         964
                     ENDIF                                                965
C                Geant POSITION : FPSC ===> X=XX Y=Y Z=Z1 DZ=DZZ DX=DXX   966
C                DY=DYY <===                                              966
                     CALL AGSRESET                                        966
                     AG_X = XX                                            966
                     AG_Y = Y                                             966
                     AG_Z = Z1                                            966
                     AG_DZ = DZZ                                          966
                     AG_DX = DXX                                          966
                     AG_DY = DYY                                          966
                     AG_TITLE = 'FPSC'                                    966
                     AG_EXNAME = 'POSITION'                               966
                     AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                        966
                     CALL AXPOSITION                                      966
C                *                                                        966
C                *                                                        967
C                   Check J.EQ.1                                          967
                        IF (J.EQ.1) THEN                                  967
C                   Geant POSITION : FLIG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  968
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    968
                        CALL AGSRESET                                     968
                        AG_X = XX                                         968
                        AG_Y = YY                                         968
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       968
                        AG_ALPHAN=AG_ALPHAN+1                             968
                        AG_ALPHAK(AG_ALPHAN)=1                            968
                        AG_ALPHAL(AG_ALPHAN)=-90                          968
                        AG_ALPHAN=AG_ALPHAN+1                             968
                        AG_ALPHAK(AG_ALPHAN)=3                            968
                        AG_ALPHAL(AG_ALPHAN)=ROT                          968
                        AG_TITLE = 'FLIG'                                 968
                        AG_EXNAME = 'POSITION'                            968
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                968
                        CALL AXPOSITION                                   968
C                   *                                                     968
                     END IF                                               969
C                *                                                        970
C                   Check J.EQ.2                                          970
                        IF (J.EQ.2) THEN                                  970
C                   Geant POSITION : FLLG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  971
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    971
                        CALL AGSRESET                                     971
                        AG_X = XX                                         971
                        AG_Y = YY                                         971
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       971
                        AG_ALPHAN=AG_ALPHAN+1                             971
                        AG_ALPHAK(AG_ALPHAN)=1                            971
                        AG_ALPHAL(AG_ALPHAN)=-90                          971
                        AG_ALPHAN=AG_ALPHAN+1                             971
                        AG_ALPHAK(AG_ALPHAN)=3                            971
                        AG_ALPHAL(AG_ALPHAN)=ROT                          971
                        AG_TITLE = 'FLLG'                                 971
                        AG_EXNAME = 'POSITION'                            971
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                971
                        CALL AXPOSITION                                   971
C                   *                                                     971
                     END IF                                               972
                     ENDIF                                                973
                     X = X + DXX                                          974
                  GO TO 5961                                              975
5962              CONTINUE                                                975
               GO TO 5951                                                 976
5952           CONTINUE                                                   976
            GO TO 5941                                                    977
5942        CONTINUE                                                      977
         GO TO 5931                                                       978
5932     CONTINUE                                                         978
         ELSEIF (SERN.EQ.2 .OR. SERN.EQ.4) THEN                           979
C    *                                                                    980
         LR=1                                                             980
            GO TO 5983                                                    980
5981        LR=LR+1                                                       980
5983        IF(LR-(2).GT.0) GO TO 5982                                    980
C       *                                                                 981
            TB=1                                                          981
               GO TO 5993                                                 981
5991           TB=TB+1                                                    981
5993           IF(TB-(2).GT.0) GO TO 5992                                 981
               Y=0.0                                                      982
               QUAD=(LR-1)*2+TB                                           983
               SLAT(QUAD,LAYR)=0                                          984
C          *                                                              985
               J=1                                                        985
                  GO TO 6003                                              985
6001              J=J+1                                                   985
6003              IF(J-(2).GT.0) GO TO 6002                               985
C             *                                                           986
                  I=1                                                     986
                     GO TO 6013                                           986
6011                 I=I+1                                                986
6013                 IF(I-(PRSW_NPRS(J)).GT.0) GO TO 6012                 986
                     DYY = PRSW_DPRS(J)/2.0                               987
                     Y = Y + DYY + PRSW_DPRS(3)/2.0                       988
                     PWID=LPRS2                                           989
C                Check Y.LE.PRSW_DHOLE                                    990
                     IF (Y.LE.PRSW_DHOLE) THEN                            990
                     DXX = (PWID-PRSW_DHOLE)/2.0                          991
                     X = DXX+PRSW_DHOLE                                   992
                     ELSE                                                 993
C                Check TB.EQ.1                                            994
                     IF (TB.EQ.1) THEN                                    994
                     DXX = PWID/2.0                                       995
                     X = DXX                                              996
                     ELSE                                                 997
                     DXX = LPRS4/2.0                                      998
                     X = DXX+2*PRSW_DPRS(1)                               999
                     ENDIF                                                1000
                     ENDIF                                                1001
                     XX=X+DXX+DLG                                         1002
C                Check TB.EQ.1                                            1003
                     IF (TB.EQ.1) THEN                                    1003
                     YY=+Y                                                1003
                     ENDIF                                                1005
C                Check TB.EQ.2                                            1006
                     IF (TB.EQ.2) THEN                                    1006
                     YY=-Y                                                1006
                     ENDIF                                                1008
C                Check LR.EQ.1                                            1009
                     IF (LR.EQ.1) THEN                                    1009
                     X=+X+PRSW_XOFF(SERN)                                 1009
                     XX=+XX+PRSW_XOFF(SERN)                               1009
                     ROT=-90                                              1010
                     ENDIF                                                1011
C                Check LR.EQ.2                                            1012
                     IF (LR.EQ.2) THEN                                    1012
                     X=-X-PRSW_XOFF(SERN)                                 1012
                     XX=-XX-PRSW_XOFF(SERN)                               1012
                     ROT=90                                               1013
                     ENDIF                                                1014
                     YY=YY+PRSW_YOFF(SERN)                                1014
C                Check TB.EQ.1 .OR. J.EQ.1 .OR. I.LE.PRSW_NPRS(J)-PRSW_N  1016
C                SKIPH                                                    1016
                     IF (TB.EQ.1 .OR. J.EQ.1 .OR.                         1016
     *               I.LE.PRSW_NPRS(J)-PRSW_NSKIPH) THEN                  1016
C                Check DEBUG.GE.1                                         1017
                     IF (DEBUG.GE.1) THEN                                 1017
                     K = K+1                                              1018
                     SLAT(QUAD,LAYR)=SLAT(QUAD,LAYR)+1                    1019
                     ID=(QUAD-1)*3*21+(LAYR-1)*21+(SLAT(QUAD,LAYR)-1)     1020
                     FLAG=0                                               1021
C                *                                                        1022
                     KK=1                                                 1022
                        GO TO 6023                                        1022
6021                    KK=KK+1                                           1022
6023                    IF(KK-(NPSTYPE).GT.0) GO TO 6022                  1022
C                   Check (PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,2).EQ.DYY).OR  1024
C                   .(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK, 2).EQ.DXX)         1024
                        IF ((PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,             1024
     *                  2).EQ.DYY).OR.(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK,   1024
     *                  2).EQ.DXX)) THEN                                  1024
                        FLAG=KK                                           1025
                        PSTYPE(KK)=PSTYPE(KK)+1                           1026
                        ENDIF                                             1027
                     GO TO 6021                                           1028
6022                 CONTINUE                                             1028
C                Check FLAG.EQ.0                                          1029
                     IF (FLAG.EQ.0) THEN                                  1029
                     NPSTYPE=NPSTYPE+1                                    1030
                     FLAG=NPSTYPE                                         1031
                     PSTYPE(FLAG)=1                                       1032
C                Check DXX.GT.DYY                                         1033
                     IF (DXX.GT.DYY) THEN                                 1033
                     PSDIM(NPSTYPE,1)=DYY                                 1034
                     PSDIM(NPSTYPE,2)=DXX                                 1035
                     ELSE                                                 1036
                     PSDIM(NPSTYPE,1)=DXX                                 1037
                     PSDIM(NPSTYPE,2)=DYY                                 1038
                     ENDIF                                                1039
                     ENDIF                                                1040
                     WRITE(SERN,'(I2,I3,F8.2,F8.2,F8.2,F8.2,F8.2,F8.2,2X  1042
     *,A1)') SERN,K,DXX*2,DYY*2, DZZ*2,X,YY,Z,                            1042
     *               CHAR(ICHAR('A')+CPSTYPE(FLAG)-1)                     1042
                     WRITE(11,'(I3,I2,I2,I3,F8.2,F8.2,F8.2,F8.2,F8.2,F8.  1044
     *2)') ID,QUAD,LAYR,SLAT(QUAD, LAYR),DXX*2,DYY*2,DZZ*2,XX,Y,Z         1044
                     ENDIF                                                1045
C                Geant POSITION : FPSC ===> X=X Y=YY Z=Z1 DZ=DZZ DX=DXX   1046
C                DY=DYY <===                                              1046
                     CALL AGSRESET                                        1046
                     AG_X = X                                             1046
                     AG_Y = YY                                            1046
                     AG_Z = Z1                                            1046
                     AG_DZ = DZZ                                          1046
                     AG_DX = DXX                                          1046
                     AG_DY = DYY                                          1046
                     AG_TITLE = 'FPSC'                                    1046
                     AG_EXNAME = 'POSITION'                               1046
                     AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                        1046
                     CALL AXPOSITION                                      1046
C                *                                                        1046
C                *                                                        1047
C                   Check J.EQ.1.AND.SERN.EQ.2                            1047
                        IF (J.EQ.1.AND.SERN.EQ.2) THEN                    1047
C                   Geant POSITION : FLIG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  1048
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    1048
                        CALL AGSRESET                                     1048
                        AG_X = XX                                         1048
                        AG_Y = YY                                         1048
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       1048
                        AG_ALPHAN=AG_ALPHAN+1                             1048
                        AG_ALPHAK(AG_ALPHAN)=1                            1048
                        AG_ALPHAL(AG_ALPHAN)=-90                          1048
                        AG_ALPHAN=AG_ALPHAN+1                             1048
                        AG_ALPHAK(AG_ALPHAN)=3                            1048
                        AG_ALPHAL(AG_ALPHAN)=ROT                          1048
                        AG_TITLE = 'FLIG'                                 1048
                        AG_EXNAME = 'POSITION'                            1048
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                1048
                        CALL AXPOSITION                                   1048
C                   *                                                     1048
                     END IF                                               1049
C                *                                                        1050
C                   Check J.EQ.2.AND.SERN.EQ.2                            1050
                        IF (J.EQ.2.AND.SERN.EQ.2) THEN                    1050
C                   Geant POSITION : FLLG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  1051
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    1051
                        CALL AGSRESET                                     1051
                        AG_X = XX                                         1051
                        AG_Y = YY                                         1051
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       1051
                        AG_ALPHAN=AG_ALPHAN+1                             1051
                        AG_ALPHAK(AG_ALPHAN)=1                            1051
                        AG_ALPHAL(AG_ALPHAN)=-90                          1051
                        AG_ALPHAN=AG_ALPHAN+1                             1051
                        AG_ALPHAK(AG_ALPHAN)=3                            1051
                        AG_ALPHAL(AG_ALPHAN)=ROT                          1051
                        AG_TITLE = 'FLLG'                                 1051
                        AG_EXNAME = 'POSITION'                            1051
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                1051
                        CALL AXPOSITION                                   1051
C                   *                                                     1051
                     END IF                                               1052
C                *                                                        1053
C                   Check J.EQ.1.AND.SERN.EQ.4                            1053
                        IF (J.EQ.1.AND.SERN.EQ.4) THEN                    1053
C                   Geant POSITION : FLIG ===> X=XX Y=YY Z=Z1+PRSW_DSIPM  1054
C                   (1)/2.0 ALPHAX=90 ALPHAZ=-ROT <===                    1054
                        CALL AGSRESET                                     1054
                        AG_X = XX                                         1054
                        AG_Y = YY                                         1054
                        AG_Z = Z1+PRSW_DSIPM(1)/2.0                       1054
                        AG_ALPHAN=AG_ALPHAN+1                             1054
                        AG_ALPHAK(AG_ALPHAN)=1                            1054
                        AG_ALPHAL(AG_ALPHAN)=90                           1054
                        AG_ALPHAN=AG_ALPHAN+1                             1054
                        AG_ALPHAK(AG_ALPHAN)=3                            1054
                        AG_ALPHAL(AG_ALPHAN)=-ROT                         1054
                        AG_TITLE = 'FLIG'                                 1054
                        AG_EXNAME = 'POSITION'                            1054
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                1054
                        CALL AXPOSITION                                   1054
C                   *                                                     1054
                     END IF                                               1055
C                *                                                        1056
C                   Check J.EQ.2.AND.SERN.EQ.4                            1056
                        IF (J.EQ.2.AND.SERN.EQ.4) THEN                    1056
C                   Geant POSITION : FLLG ===> X=XX Y=YY Z=Z1+PRSW_DSIPM  1057
C                   (1)/2.0 ALPHAX=90 ALPHAZ=-ROT <===                    1057
                        CALL AGSRESET                                     1057
                        AG_X = XX                                         1057
                        AG_Y = YY                                         1057
                        AG_Z = Z1+PRSW_DSIPM(1)/2.0                       1057
                        AG_ALPHAN=AG_ALPHAN+1                             1057
                        AG_ALPHAK(AG_ALPHAN)=1                            1057
                        AG_ALPHAL(AG_ALPHAN)=90                           1057
                        AG_ALPHAN=AG_ALPHAN+1                             1057
                        AG_ALPHAK(AG_ALPHAN)=3                            1057
                        AG_ALPHAL(AG_ALPHAN)=-ROT                         1057
                        AG_TITLE = 'FLLG'                                 1057
                        AG_EXNAME = 'POSITION'                            1057
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                1057
                        CALL AXPOSITION                                   1057
C                   *                                                     1057
                     END IF                                               1058
                     ENDIF                                                1059
                     Y = Y + DYY                                          1060
                  GO TO 6011                                              1061
6012              CONTINUE                                                1061
               GO TO 6001                                                 1062
6002           CONTINUE                                                   1062
            GO TO 5991                                                    1063
5992        CONTINUE                                                      1063
         GO TO 5981                                                       1064
5982     CONTINUE                                                         1064
         ENDIF                                                            1065
C    Check DEBUG.GE.1                                                     1066
         IF (DEBUG.GE.1) THEN                                             1066
         CLOSE(SERN)                                                      1067
         ENDIF                                                            1068
         DZ1=PRSW_DZBBD(SERN)/2.0                                         1069
         DZ3=PRSW_DZ(3) + PRSW_DZBBD(3)                                   1070
         DX1=(PRSW_DHOLE - PRSW_DXBBD(1))/2.0                             1071
         DY1=(LPRS2 - PRSW_DHOLE + PRSW_DXBBD(1) + PRSW_DXBBD(2))/2.0     1072
         X2=PRSW_XOFF(SERN) + DX1                                         1073
         Y2=PRSW_DHOLE - PRSW_DXBBD(1) + DY1                              1074
         DXX=DX1                                                          1074
         DYY=DY1                                                          1074
C    *                                                                    1076
C       Check SERN.NE.3                                                   1076
            IF (SERN.NE.3) THEN                                           1076
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1 <  1077
C       ===                                                               1077
            CALL AGSRESET                                                 1077
            AG_X = X2                                                     1077
            AG_Y = Y2                                                     1077
            AG_Z = Z2                                                     1077
            AG_DZ = DZ1                                                   1077
            AG_DX = DX1                                                   1077
            AG_DY = DY1                                                   1077
            AG_TITLE = 'FPBB'                                             1077
            AG_EXNAME = 'POSITION'                                        1077
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1077
            CALL AXPOSITION                                               1077
C       *                                                                 1077
         END IF                                                           1078
C    *                                                                    1079
C       Check SERN.NE.3                                                   1079
            IF (SERN.NE.3) THEN                                           1079
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1   1080
C       <===                                                              1080
            CALL AGSRESET                                                 1080
            AG_X = -X2                                                    1080
            AG_Y = Y2                                                     1080
            AG_Z = Z2                                                     1080
            AG_DZ = DZ1                                                   1080
            AG_DX = DX1                                                   1080
            AG_DY = DY1                                                   1080
            AG_TITLE = 'FPBB'                                             1080
            AG_EXNAME = 'POSITION'                                        1080
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1080
            CALL AXPOSITION                                               1080
C       *                                                                 1080
         END IF                                                           1081
C    *                                                                    1082
C       Check SERN.EQ.3                                                   1082
            IF (SERN.EQ.3) THEN                                           1082
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)/  1083
C       4.0 DX=DX1 DY=DY1 <===                                            1083
            CALL AGSRESET                                                 1083
            AG_X = X2                                                     1083
            AG_Y = Y2                                                     1083
            AG_Z = Z2-DZSB                                                1083
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1083
            AG_DX = DX1                                                   1083
            AG_DY = DY1                                                   1083
            AG_TITLE = 'FPBB'                                             1083
            AG_EXNAME = 'POSITION'                                        1083
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1083
            CALL AXPOSITION                                               1083
C       *                                                                 1083
         END IF                                                           1084
C    *                                                                    1085
C       Check SERN.EQ.3                                                   1085
            IF (SERN.EQ.3) THEN                                           1085
C       Geant POSITION : FPPB ===> X=X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=D  1086
C       X1 DY=DY1 <===                                                    1086
            CALL AGSRESET                                                 1086
            AG_X = X2                                                     1086
            AG_Y = Y2                                                     1086
            AG_Z = Z2                                                     1086
            AG_DZ = PRSW_DZ(3)/2.0                                        1086
            AG_DX = DX1                                                   1086
            AG_DY = DY1                                                   1086
            AG_TITLE = 'FPPB'                                             1086
            AG_EXNAME = 'POSITION'                                        1086
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1086
            CALL AXPOSITION                                               1086
C       *                                                                 1086
         END IF                                                           1087
C    *                                                                    1088
C       Check SERN.EQ.3                                                   1088
            IF (SERN.EQ.3) THEN                                           1088
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)/  1089
C       4.0 DX=DX1 DY=DY1 <===                                            1089
            CALL AGSRESET                                                 1089
            AG_X = X2                                                     1089
            AG_Y = Y2                                                     1089
            AG_Z = Z2+DZSB                                                1089
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1089
            AG_DX = DX1                                                   1089
            AG_DY = DY1                                                   1089
            AG_TITLE = 'FPBB'                                             1089
            AG_EXNAME = 'POSITION'                                        1089
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1089
            CALL AXPOSITION                                               1089
C       *                                                                 1089
         END IF                                                           1090
C    *                                                                    1091
C       Check SERN.EQ.3                                                   1091
            IF (SERN.EQ.3) THEN                                           1091
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  1092
C       /4.0 DX=DX1 DY=DY1 <===                                           1092
            CALL AGSRESET                                                 1092
            AG_X = -X2                                                    1092
            AG_Y = Y2                                                     1092
            AG_Z = Z2-DZSB                                                1092
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1092
            AG_DX = DX1                                                   1092
            AG_DY = DY1                                                   1092
            AG_TITLE = 'FPBB'                                             1092
            AG_EXNAME = 'POSITION'                                        1092
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1092
            CALL AXPOSITION                                               1092
C       *                                                                 1092
         END IF                                                           1093
C    *                                                                    1094
C       Check SERN.EQ.3                                                   1094
            IF (SERN.EQ.3) THEN                                           1094
C       Geant POSITION : FPPB ===> X=-X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  1095
C       DX1 DY=DY1 <===                                                   1095
            CALL AGSRESET                                                 1095
            AG_X = -X2                                                    1095
            AG_Y = Y2                                                     1095
            AG_Z = Z2                                                     1095
            AG_DZ = PRSW_DZ(3)/2.0                                        1095
            AG_DX = DX1                                                   1095
            AG_DY = DY1                                                   1095
            AG_TITLE = 'FPPB'                                             1095
            AG_EXNAME = 'POSITION'                                        1095
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1095
            CALL AXPOSITION                                               1095
C       *                                                                 1095
         END IF                                                           1096
C    *                                                                    1097
C       Check SERN.EQ.3                                                   1097
            IF (SERN.EQ.3) THEN                                           1097
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  1098
C       /4.0 DX=DX1 DY=DY1 <===                                           1098
            CALL AGSRESET                                                 1098
            AG_X = -X2                                                    1098
            AG_Y = Y2                                                     1098
            AG_Z = Z2+DZSB                                                1098
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1098
            AG_DX = DX1                                                   1098
            AG_DY = DY1                                                   1098
            AG_TITLE = 'FPBB'                                             1098
            AG_EXNAME = 'POSITION'                                        1098
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1098
            CALL AXPOSITION                                               1098
C       *                                                                 1098
         END IF                                                           1099
         DX2=(LPRS2 - PRSW_DHOLE + PRSW_DXBBD(1) + PRSW_DXBBD(2))/2.0     1100
         DY2=(LPRS2 + PRSW_DXBBD(2))/2.0                                  1101
         X2=PRSW_XOFF(SERN) + DX1*2 + DX2                                 1102
         Y2=DY2                                                           1103
         DXX=DX2                                                          1103
         DYY=DY2                                                          1103
C    *                                                                    1105
C       Check SERN.NE.3                                                   1105
            IF (SERN.NE.3) THEN                                           1105
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2 <  1106
C       ===                                                               1106
            CALL AGSRESET                                                 1106
            AG_X = X2                                                     1106
            AG_Y = Y2                                                     1106
            AG_Z = Z2                                                     1106
            AG_DZ = DZ1                                                   1106
            AG_DX = DX2                                                   1106
            AG_DY = DY2                                                   1106
            AG_TITLE = 'FPBB'                                             1106
            AG_EXNAME = 'POSITION'                                        1106
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1106
            CALL AXPOSITION                                               1106
C       *                                                                 1106
         END IF                                                           1107
C    *                                                                    1108
C       Check SERN.NE.3                                                   1108
            IF (SERN.NE.3) THEN                                           1108
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2   1109
C       <===                                                              1109
            CALL AGSRESET                                                 1109
            AG_X = -X2                                                    1109
            AG_Y = Y2                                                     1109
            AG_Z = Z2                                                     1109
            AG_DZ = DZ1                                                   1109
            AG_DX = DX2                                                   1109
            AG_DY = DY2                                                   1109
            AG_TITLE = 'FPBB'                                             1109
            AG_EXNAME = 'POSITION'                                        1109
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1109
            CALL AXPOSITION                                               1109
C       *                                                                 1109
         END IF                                                           1110
C    *                                                                    1111
C       Check SERN.EQ.3                                                   1111
            IF (SERN.EQ.3) THEN                                           1111
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)/  1112
C       4.0 DX=DX2 DY=DY2 <===                                            1112
            CALL AGSRESET                                                 1112
            AG_X = X2                                                     1112
            AG_Y = Y2                                                     1112
            AG_Z = Z2-DZSB                                                1112
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1112
            AG_DX = DX2                                                   1112
            AG_DY = DY2                                                   1112
            AG_TITLE = 'FPBB'                                             1112
            AG_EXNAME = 'POSITION'                                        1112
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1112
            CALL AXPOSITION                                               1112
C       *                                                                 1112
         END IF                                                           1113
C    *                                                                    1114
C       Check SERN.EQ.3                                                   1114
            IF (SERN.EQ.3) THEN                                           1114
C       Geant POSITION : FPPB ===> X=X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=D  1115
C       X2 DY=DY2 <===                                                    1115
            CALL AGSRESET                                                 1115
            AG_X = X2                                                     1115
            AG_Y = Y2                                                     1115
            AG_Z = Z2                                                     1115
            AG_DZ = PRSW_DZ(3)/2.0                                        1115
            AG_DX = DX2                                                   1115
            AG_DY = DY2                                                   1115
            AG_TITLE = 'FPPB'                                             1115
            AG_EXNAME = 'POSITION'                                        1115
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1115
            CALL AXPOSITION                                               1115
C       *                                                                 1115
         END IF                                                           1116
C    *                                                                    1117
C       Check SERN.EQ.3                                                   1117
            IF (SERN.EQ.3) THEN                                           1117
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)/  1118
C       4.0 DX=DX2 DY=DY2 <===                                            1118
            CALL AGSRESET                                                 1118
            AG_X = X2                                                     1118
            AG_Y = Y2                                                     1118
            AG_Z = Z2+DZSB                                                1118
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1118
            AG_DX = DX2                                                   1118
            AG_DY = DY2                                                   1118
            AG_TITLE = 'FPBB'                                             1118
            AG_EXNAME = 'POSITION'                                        1118
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1118
            CALL AXPOSITION                                               1118
C       *                                                                 1118
         END IF                                                           1119
C    *                                                                    1120
C       Check SERN.EQ.3                                                   1120
            IF (SERN.EQ.3) THEN                                           1120
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  1121
C       /4.0 DX=DX2 DY=DY2 <===                                           1121
            CALL AGSRESET                                                 1121
            AG_X = -X2                                                    1121
            AG_Y = Y2                                                     1121
            AG_Z = Z2-DZSB                                                1121
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1121
            AG_DX = DX2                                                   1121
            AG_DY = DY2                                                   1121
            AG_TITLE = 'FPBB'                                             1121
            AG_EXNAME = 'POSITION'                                        1121
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1121
            CALL AXPOSITION                                               1121
C       *                                                                 1121
         END IF                                                           1122
C    *                                                                    1123
C       Check SERN.EQ.3                                                   1123
            IF (SERN.EQ.3) THEN                                           1123
C       Geant POSITION : FPPB ===> X=-X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  1124
C       DX2 DY=DY2 <===                                                   1124
            CALL AGSRESET                                                 1124
            AG_X = -X2                                                    1124
            AG_Y = Y2                                                     1124
            AG_Z = Z2                                                     1124
            AG_DZ = PRSW_DZ(3)/2.0                                        1124
            AG_DX = DX2                                                   1124
            AG_DY = DY2                                                   1124
            AG_TITLE = 'FPPB'                                             1124
            AG_EXNAME = 'POSITION'                                        1124
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1124
            CALL AXPOSITION                                               1124
C       *                                                                 1124
         END IF                                                           1125
C    *                                                                    1126
C       Check SERN.EQ.3                                                   1126
            IF (SERN.EQ.3) THEN                                           1126
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  1127
C       /4.0 DX=DX2 DY=DY2 <===                                           1127
            CALL AGSRESET                                                 1127
            AG_X = -X2                                                    1127
            AG_Y = Y2                                                     1127
            AG_Z = Z2+DZSB                                                1127
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1127
            AG_DX = DX2                                                   1127
            AG_DY = DY2                                                   1127
            AG_TITLE = 'FPBB'                                             1127
            AG_EXNAME = 'POSITION'                                        1127
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1127
            CALL AXPOSITION                                               1127
C       *                                                                 1127
         END IF                                                           1128
         DX1A=(PRSW_DHOLE - PRSW_DXBBD(1) - PRSW_NSKIPV*PRSW_DPRS(1))/    1129
     *   2.0                                                              1129
         DX1B=(PRSW_DHOLE - PRSW_DXBBD(1))/2.0                            1130
C    Check SERN.LE.4                                                      1131
         IF (SERN.LE.4) THEN                                              1131
         DX1=DX1A                                                         1132
         X2=PRSW_XOFF(SERN)+PRSW_NSKIPV*PRSW_DPRS(1)+DX1                  1133
         ELSE                                                             1134
         DX1=DX1B                                                         1135
         X2=PRSW_XOFF(SERN)+DX1                                           1136
         ENDIF                                                            1137
         DY1=(LPRS3 - PRSW_DHOLE - PRSW_DYCUT + PRSW_DXBBD(1) +           1138
     *   PRSW_DXBBD(2))/2.0                                               1138
         Y2=PRSW_DHOLE - PRSW_DXBBD(1) + DY1                              1139
         DXX=DX1                                                          1139
         DYY=DY1                                                          1139
C    *                                                                    1141
C       Check SERN.NE.3                                                   1141
            IF (SERN.NE.3) THEN                                           1141
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1   1142
C       <===                                                              1142
            CALL AGSRESET                                                 1142
            AG_X = X2                                                     1142
            AG_Y = -Y2                                                    1142
            AG_Z = Z2                                                     1142
            AG_DZ = DZ1                                                   1142
            AG_DX = DX1                                                   1142
            AG_DY = DY1                                                   1142
            AG_TITLE = 'FPBB'                                             1142
            AG_EXNAME = 'POSITION'                                        1142
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1142
            CALL AXPOSITION                                               1142
C       *                                                                 1142
         END IF                                                           1143
C    *                                                                    1144
C       Check SERN.NE.3                                                   1144
            IF (SERN.NE.3) THEN                                           1144
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1  1145
C        <===                                                             1145
            CALL AGSRESET                                                 1145
            AG_X = -X2                                                    1145
            AG_Y = -Y2                                                    1145
            AG_Z = Z2                                                     1145
            AG_DZ = DZ1                                                   1145
            AG_DX = DX1                                                   1145
            AG_DY = DY1                                                   1145
            AG_TITLE = 'FPBB'                                             1145
            AG_EXNAME = 'POSITION'                                        1145
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1145
            CALL AXPOSITION                                               1145
C       *                                                                 1145
         END IF                                                           1146
C    *                                                                    1147
C       Check SERN.EQ.3                                                   1147
            IF (SERN.EQ.3) THEN                                           1147
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  1148
C       /4.0 DX=DX1 DY=DY1 <===                                           1148
            CALL AGSRESET                                                 1148
            AG_X = X2                                                     1148
            AG_Y = -Y2                                                    1148
            AG_Z = Z2-DZSB                                                1148
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1148
            AG_DX = DX1                                                   1148
            AG_DY = DY1                                                   1148
            AG_TITLE = 'FPBB'                                             1148
            AG_EXNAME = 'POSITION'                                        1148
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1148
            CALL AXPOSITION                                               1148
C       *                                                                 1148
         END IF                                                           1149
C    *                                                                    1150
C       Check SERN.EQ.3                                                   1150
            IF (SERN.EQ.3) THEN                                           1150
C       Geant POSITION : FPPB ===> X=X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  1151
C       DX1 DY=DY1 <===                                                   1151
            CALL AGSRESET                                                 1151
            AG_X = X2                                                     1151
            AG_Y = -Y2                                                    1151
            AG_Z = Z2                                                     1151
            AG_DZ = PRSW_DZ(3)/2.0                                        1151
            AG_DX = DX1                                                   1151
            AG_DY = DY1                                                   1151
            AG_TITLE = 'FPPB'                                             1151
            AG_EXNAME = 'POSITION'                                        1151
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1151
            CALL AXPOSITION                                               1151
C       *                                                                 1151
         END IF                                                           1152
C    *                                                                    1153
C       Check SERN.EQ.3                                                   1153
            IF (SERN.EQ.3) THEN                                           1153
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  1154
C       /4.0 DX=DX1 DY=DY1 <===                                           1154
            CALL AGSRESET                                                 1154
            AG_X = X2                                                     1154
            AG_Y = -Y2                                                    1154
            AG_Z = Z2+DZSB                                                1154
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1154
            AG_DX = DX1                                                   1154
            AG_DY = DY1                                                   1154
            AG_TITLE = 'FPBB'                                             1154
            AG_EXNAME = 'POSITION'                                        1154
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1154
            CALL AXPOSITION                                               1154
C       *                                                                 1154
         END IF                                                           1155
C    *                                                                    1156
C       Check SERN.EQ.3                                                   1156
            IF (SERN.EQ.3) THEN                                           1156
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3  1157
C       )/4.0 DX=DX1 DY=DY1 <===                                          1157
            CALL AGSRESET                                                 1157
            AG_X = -X2                                                    1157
            AG_Y = -Y2                                                    1157
            AG_Z = Z2-DZSB                                                1157
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1157
            AG_DX = DX1                                                   1157
            AG_DY = DY1                                                   1157
            AG_TITLE = 'FPBB'                                             1157
            AG_EXNAME = 'POSITION'                                        1157
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1157
            CALL AXPOSITION                                               1157
C       *                                                                 1157
         END IF                                                           1158
C    *                                                                    1159
C       Check SERN.EQ.3                                                   1159
            IF (SERN.EQ.3) THEN                                           1159
C       Geant POSITION : FPPB ===> X=-X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX  1160
C       =DX1 DY=DY1 <===                                                  1160
            CALL AGSRESET                                                 1160
            AG_X = -X2                                                    1160
            AG_Y = -Y2                                                    1160
            AG_Z = Z2                                                     1160
            AG_DZ = PRSW_DZ(3)/2.0                                        1160
            AG_DX = DX1                                                   1160
            AG_DY = DY1                                                   1160
            AG_TITLE = 'FPPB'                                             1160
            AG_EXNAME = 'POSITION'                                        1160
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1160
            CALL AXPOSITION                                               1160
C       *                                                                 1160
         END IF                                                           1161
C    *                                                                    1162
C       Check SERN.EQ.3                                                   1162
            IF (SERN.EQ.3) THEN                                           1162
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3  1163
C       )/4.0 DX=DX1 DY=DY1 <===                                          1163
            CALL AGSRESET                                                 1163
            AG_X = -X2                                                    1163
            AG_Y = -Y2                                                    1163
            AG_Z = Z2+DZSB                                                1163
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1163
            AG_DX = DX1                                                   1163
            AG_DY = DY1                                                   1163
            AG_TITLE = 'FPBB'                                             1163
            AG_EXNAME = 'POSITION'                                        1163
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1163
            CALL AXPOSITION                                               1163
C       *                                                                 1163
         END IF                                                           1164
C    Check SERN.LE.2                                                      1165
         IF (SERN.LE.2) THEN                                              1165
         DX2=(LPRS2-PRSW_DHOLE+PRSW_DXBBD(1)+PRSW_DXBBD(2))/2.0           1166
         ELSE                                                             1167
         DX2=(LPRS2-PRSW_DHOLE+PRSW_DXBBD(1)+PRSW_DXBBD(2))/2.0           1168
         ENDIF                                                            1169
         DY2=(LPRS3 - PRSW_DYCUT + PRSW_DXBBD(2))/2.0                     1170
         X2=PRSW_XOFF(SERN) + DX1B*2 + DX2                                1171
         Y2=DY2                                                           1172
         DXX=DX2                                                          1172
         DYY=DY2                                                          1172
C    *                                                                    1174
C       Check SERN.NE.3                                                   1174
            IF (SERN.NE.3) THEN                                           1174
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2   1175
C       <===                                                              1175
            CALL AGSRESET                                                 1175
            AG_X = X2                                                     1175
            AG_Y = -Y2                                                    1175
            AG_Z = Z2                                                     1175
            AG_DZ = DZ1                                                   1175
            AG_DX = DX2                                                   1175
            AG_DY = DY2                                                   1175
            AG_TITLE = 'FPBB'                                             1175
            AG_EXNAME = 'POSITION'                                        1175
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1175
            CALL AXPOSITION                                               1175
C       *                                                                 1175
         END IF                                                           1176
C    *                                                                    1177
C       Check SERN.NE.3                                                   1177
            IF (SERN.NE.3) THEN                                           1177
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2  1178
C        <===                                                             1178
            CALL AGSRESET                                                 1178
            AG_X = -X2                                                    1178
            AG_Y = -Y2                                                    1178
            AG_Z = Z2                                                     1178
            AG_DZ = DZ1                                                   1178
            AG_DX = DX2                                                   1178
            AG_DY = DY2                                                   1178
            AG_TITLE = 'FPBB'                                             1178
            AG_EXNAME = 'POSITION'                                        1178
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1178
            CALL AXPOSITION                                               1178
C       *                                                                 1178
         END IF                                                           1179
C    *                                                                    1180
C       Check SERN.EQ.3                                                   1180
            IF (SERN.EQ.3) THEN                                           1180
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  1181
C       /4.0 DX=DX2 DY=DY2 <===                                           1181
            CALL AGSRESET                                                 1181
            AG_X = X2                                                     1181
            AG_Y = -Y2                                                    1181
            AG_Z = Z2-DZSB                                                1181
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1181
            AG_DX = DX2                                                   1181
            AG_DY = DY2                                                   1181
            AG_TITLE = 'FPBB'                                             1181
            AG_EXNAME = 'POSITION'                                        1181
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1181
            CALL AXPOSITION                                               1181
C       *                                                                 1181
         END IF                                                           1182
C    *                                                                    1183
C       Check SERN.EQ.3                                                   1183
            IF (SERN.EQ.3) THEN                                           1183
C       Geant POSITION : FPPB ===> X=X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  1184
C       DX2 DY=DY2 <===                                                   1184
            CALL AGSRESET                                                 1184
            AG_X = X2                                                     1184
            AG_Y = -Y2                                                    1184
            AG_Z = Z2                                                     1184
            AG_DZ = PRSW_DZ(3)/2.0                                        1184
            AG_DX = DX2                                                   1184
            AG_DY = DY2                                                   1184
            AG_TITLE = 'FPPB'                                             1184
            AG_EXNAME = 'POSITION'                                        1184
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1184
            CALL AXPOSITION                                               1184
C       *                                                                 1184
         END IF                                                           1185
C    *                                                                    1186
C       Check SERN.EQ.3                                                   1186
            IF (SERN.EQ.3) THEN                                           1186
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  1187
C       /4.0 DX=DX2 DY=DY2 <===                                           1187
            CALL AGSRESET                                                 1187
            AG_X = X2                                                     1187
            AG_Y = -Y2                                                    1187
            AG_Z = Z2+DZSB                                                1187
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1187
            AG_DX = DX2                                                   1187
            AG_DY = DY2                                                   1187
            AG_TITLE = 'FPBB'                                             1187
            AG_EXNAME = 'POSITION'                                        1187
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1187
            CALL AXPOSITION                                               1187
C       *                                                                 1187
         END IF                                                           1188
C    *                                                                    1189
C       Check SERN.EQ.3                                                   1189
            IF (SERN.EQ.3) THEN                                           1189
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3  1190
C       )/4.0 DX=DX2 DY=DY2 <===                                          1190
            CALL AGSRESET                                                 1190
            AG_X = -X2                                                    1190
            AG_Y = -Y2                                                    1190
            AG_Z = Z2-DZSB                                                1190
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1190
            AG_DX = DX2                                                   1190
            AG_DY = DY2                                                   1190
            AG_TITLE = 'FPBB'                                             1190
            AG_EXNAME = 'POSITION'                                        1190
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1190
            CALL AXPOSITION                                               1190
C       *                                                                 1190
         END IF                                                           1191
C    *                                                                    1192
C       Check SERN.EQ.3                                                   1192
            IF (SERN.EQ.3) THEN                                           1192
C       Geant POSITION : FPPB ===> X=-X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX  1193
C       =DX2 DY=DY2 <===                                                  1193
            CALL AGSRESET                                                 1193
            AG_X = -X2                                                    1193
            AG_Y = -Y2                                                    1193
            AG_Z = Z2                                                     1193
            AG_DZ = PRSW_DZ(3)/2.0                                        1193
            AG_DX = DX2                                                   1193
            AG_DY = DY2                                                   1193
            AG_TITLE = 'FPPB'                                             1193
            AG_EXNAME = 'POSITION'                                        1193
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1193
            CALL AXPOSITION                                               1193
C       *                                                                 1193
         END IF                                                           1194
C    *                                                                    1195
C       Check SERN.EQ.3                                                   1195
            IF (SERN.EQ.3) THEN                                           1195
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3  1196
C       )/4.0 DX=DX2 DY=DY2 <===                                          1196
            CALL AGSRESET                                                 1196
            AG_X = -X2                                                    1196
            AG_Y = -Y2                                                    1196
            AG_Z = Z2+DZSB                                                1196
            AG_DZ = PRSW_DZBBD(3)/4.0                                     1196
            AG_DX = DX2                                                   1196
            AG_DY = DY2                                                   1196
            AG_TITLE = 'FPBB'                                             1196
            AG_EXNAME = 'POSITION'                                        1196
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 1196
            CALL AXPOSITION                                               1196
C       *                                                                 1196
         END IF                                                           1197
5741  CALL AGSPOPB                                                        1198
      GO TO BLOCK_FPLY, (5740 ,5750,5760,5770,5780)                       1198
C End of Local Procedure                                                  1198
C ----------------------------------------------------------------------  1199
C ------------ FPBB                                                       1199
C Begin Local Procedure FPBB                                              1200
C FPBB IS THE G10 BACK BOARD                                              1200
5890  CONTINUE                                                            1200
         AG_TITLE='FPBB'                                                  1200
         AG_MODULE=MODULE                                                 1200
         AG_MARK='V00'                                                    1200
         CALL AGSPUSH                                                     1200
         AG_MODULE=MODULE                                                 1200
C    *                                                                    1200
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          1201
         CALL AGSRESET                                                    1201
         AG_A = 28.08                                                     1201
         AG_Z = 14                                                        1201
         AG_W = 0.6*1*28./60.                                             1201
         AG_TITLE = 'SI'                                                  1201
         AG_EXNAME = 'COMPONENT'                                          1201
         AG_PARLIST = 'A_Z_W'                                             1201
         CALL AXCOMPONENT                                                 1201
C    *                                                                    1201
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               1201
         CALL AGSRESET                                                    1202
         AG_A = 16                                                        1202
         AG_Z = 8                                                         1202
         AG_W = 0.6*2*16./60.                                             1202
         AG_TITLE = 'O'                                                   1202
         AG_EXNAME = 'COMPONENT'                                          1202
         AG_PARLIST = 'A_Z_W'                                             1202
         CALL AXCOMPONENT                                                 1202
C    *                                                                    1202
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              1202
         CALL AGSRESET                                                    1203
         AG_A = 12                                                        1203
         AG_Z = 6                                                         1203
         AG_W = 0.4*8*12./174.                                            1203
         AG_TITLE = 'C'                                                   1203
         AG_EXNAME = 'COMPONENT'                                          1203
         AG_PARLIST = 'A_Z_W'                                             1203
         CALL AXCOMPONENT                                                 1203
C    *                                                                    1203
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               1203
         CALL AGSRESET                                                    1204
         AG_A = 1                                                         1204
         AG_Z = 1                                                         1204
         AG_W = 0.4*14*1./174.                                            1204
         AG_TITLE = 'H'                                                   1204
         AG_EXNAME = 'COMPONENT'                                          1204
         AG_PARLIST = 'A_Z_W'                                             1204
         CALL AXCOMPONENT                                                 1204
C    *                                                                    1204
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              1204
         CALL AGSRESET                                                    1205
         AG_A = 16                                                        1205
         AG_Z = 8                                                         1205
         AG_W = 0.4*4*16./174.                                            1205
         AG_TITLE = 'O'                                                   1205
         AG_EXNAME = 'COMPONENT'                                          1205
         AG_PARLIST = 'A_Z_W'                                             1205
         CALL AXCOMPONENT                                                 1205
C    *                                                                    1205
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               1205
         CALL AGSRESET                                                    1206
         AG_DENS = 1.7                                                    1206
         AG_TITLE = 'G10'                                                 1206
         AG_EXNAME = 'MIXTURE'                                            1206
         AG_PARLIST = 'DENS'                                              1206
         CALL AXMIXTURE                                                   1206
C    *                                                                    1206
C    Geant ATTRIBUTE : FPBB ===> SEEN=1 COLO=7 <===                       1206
         CALL AGSRESET                                                    1207
         AG_SEEN = 1                                                      1207
         AG_COLO = 7                                                      1207
         AG_TITLE = 'FPBB'                                                1207
         AG_EXNAME = 'ATTRIBUTE'                                          1207
         AG_PARLIST = 'SEEN_COLO'                                         1207
         CALL AXATTRIBUTE                                                 1207
C    *                                                                    1207
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1207
         CALL AGSRESET                                                    1208
         AG_DX = 0.0                                                      1208
         AG_DY = 0.0                                                      1208
         AG_DZ = 0.0                                                      1208
         AG_TITLE = 'BOX'                                                 1208
         AG_EXNAME = 'SHAPE'                                              1208
         AG_PARLIST = 'DX_DY_DZ'                                          1208
         CALL AXSHAPE                                                     1208
C    *                                                                    1208
5891  CALL AGSPOPB                                                        1209
      GO TO BLOCK_FPBB, (5890 ,5900)                                      1209
C End of Local Procedure                                                  1209
C ----------------------------------------------------------------------  1210
C ------------ FPPB                                                       1210
C Begin Local Procedure FPPB                                              1211
C FPPB IS FMS PS PB CONVERTER                                             1211
5910  CONTINUE                                                            1211
         AG_TITLE='FPPB'                                                  1211
         AG_MODULE=MODULE                                                 1211
         AG_MARK='V00'                                                    1211
         CALL AGSPUSH                                                     1211
         AG_MODULE=MODULE                                                 1211
C    *                                                                    1211
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             1212
         CALL AGSRESET                                                    1212
         AG_TITLE = 'LEAD'                                                1212
         AG_EXNAME = 'MATERIAL'                                           1212
         AG_PARLIST = 'NONE'                                              1212
         CALL AXMATERIAL                                                  1212
C    *                                                                    1212
C    Geant ATTRIBUTE : FPPB ===> SEEN=1 COLO=6 <===                       1212
         CALL AGSRESET                                                    1213
         AG_SEEN = 1                                                      1213
         AG_COLO = 6                                                      1213
         AG_TITLE = 'FPPB'                                                1213
         AG_EXNAME = 'ATTRIBUTE'                                          1213
         AG_PARLIST = 'SEEN_COLO'                                         1213
         CALL AXATTRIBUTE                                                 1213
C    *                                                                    1213
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1213
         CALL AGSRESET                                                    1214
         AG_DX = 0.0                                                      1214
         AG_DY = 0.0                                                      1214
         AG_DZ = 0.0                                                      1214
         AG_TITLE = 'BOX'                                                 1214
         AG_EXNAME = 'SHAPE'                                              1214
         AG_PARLIST = 'DX_DY_DZ'                                          1214
         CALL AXSHAPE                                                     1214
C    *                                                                    1214
5911  CALL AGSPOPB                                                        1215
      GO TO BLOCK_FPPB, (5910 ,5920)                                      1215
C End of Local Procedure                                                  1215
C ----------------------------------------------------------------------  1216
C ------------ FPSC                                                       1216
C Begin Local Procedure FPSC                                              1217
C FPSC IS A PIECE OF SCINTILLATOR IN FMS PRESHOWER                        1217
5810  CONTINUE                                                            1217
         AG_TITLE='FPSC'                                                  1217
         AG_MODULE=MODULE                                                 1217
         AG_MARK='V00'                                                    1217
         CALL AGSPUSH                                                     1217
         AG_MODULE=MODULE                                                 1217
C    *                                                                    1217
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1218
         CALL AGSRESET                                                    1218
         AG_TITLE = 'POLYSTYREN'                                          1218
         AG_EXNAME = 'MATERIAL'                                           1218
         AG_PARLIST = 'NONE'                                              1218
         CALL AXMATERIAL                                                  1218
C    *                                                                    1218
C    Geant MATERIAL : FMSPS_SCINT ===> ISVOL=1 <===                       1218
         CALL AGSRESET                                                    1219
         AG_ISVOL = 1                                                     1219
         AG_TITLE = 'FMSPS_SCINT'                                         1219
         AG_EXNAME = 'MATERIAL'                                           1219
         AG_PARLIST = 'ISVOL'                                             1219
         CALL AXMATERIAL                                                  1219
C    *                                                                    1219
C    Geant ATTRIBUTE : FPSC ===> SEEN=1 COLO=4 <===                       1219
         CALL AGSRESET                                                    1220
         AG_SEEN = 1                                                      1220
         AG_COLO = 4                                                      1220
         AG_TITLE = 'FPSC'                                                1220
         AG_EXNAME = 'ATTRIBUTE'                                          1220
         AG_PARLIST = 'SEEN_COLO'                                         1220
         CALL AXATTRIBUTE                                                 1220
C    *                                                                    1220
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1220
         CALL AGSRESET                                                    1221
         AG_DX = 0.0                                                      1221
         AG_DY = 0.0                                                      1221
         AG_DZ = 0.0                                                      1221
         AG_TITLE = 'BOX'                                                 1221
         AG_EXNAME = 'SHAPE'                                              1221
         AG_PARLIST = 'DX_DY_DZ'                                          1221
         CALL AXSHAPE                                                     1221
C    *                                                                    1221
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           1221
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             1223
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             1224
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                1225
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.013)                             1226
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            1227
C    Defining Hits: BIRK:0:(0,10)                                         1228
         AG_TITLE = 'FPSC'                                                1228
         AG_EXNAME = 'HITS'                                               1228
         AG_TYPE=AG_MAGIC                                                 1228
         CALL AGSDETV('FPSC')                                             1228
C    *                                                                    1228
         AG_PARLIST='BIRK'                                                1228
         AG_TITLE = 'DETH'                                                1228
         AG_NBITS = 0                                                     1228
         AG_BIN=AG_MAGIC                                                  1228
         AG_OPTION='*'                                                    1228
         AG_FHMIN = 0                                                     1228
         AG_FHMAX = 10                                                    1228
         CALL AGSDETH (0)                                                 1228
C    *                                                                    1228
         CALL AGSDETU('FPSC')                                             1228
C    *                                                                    1228
5811  CALL AGSPOPB                                                        1229
      GO TO BLOCK_FPSC, (5810 ,5820)                                      1229
C End of Local Procedure                                                  1229
C ----------------------------------------------------------------------  1230
C ------------ FLIG                                                       1230
C Begin Local Procedure FLIG                                              1231
C FLIG IS A LIGHT GUIDE + SIPM BOARD FOR PRESHOWER                        1231
5830  CONTINUE                                                            1231
         AG_TITLE='FLIG'                                                  1231
         AG_MODULE=MODULE                                                 1231
         AG_MARK='V00'                                                    1231
         CALL AGSPUSH                                                     1231
         AG_MODULE=MODULE                                                 1231
C    *                                                                    1231
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1232
         CALL AGSRESET                                                    1232
         AG_TITLE = 'AIR'                                                 1232
         AG_EXNAME = 'MATERIAL'                                           1232
         AG_PARLIST = 'NONE'                                              1232
         CALL AXMATERIAL                                                  1232
C    *                                                                    1232
C    Geant ATTRIBUTE : FLIG ===> SEEN=1 COLO=6 SERIAL=1 <===              1232
         CALL AGSRESET                                                    1233
         AG_SEEN = 1                                                      1233
         AG_COLO = 6                                                      1233
         AG_SERIAL = 1                                                    1233
         AG_TITLE = 'FLIG'                                                1233
         AG_EXNAME = 'ATTRIBUTE'                                          1233
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1233
         CALL AXATTRIBUTE                                                 1233
C    *                                                                    1233
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(1)/2.0 DY=PRSW_DZ(1)/2.0+PRSW_D  1233
C    SIPM(1)/2.0, DZ=DLG <===                                             1233
         CALL AGSRESET                                                    1235
         AG_DX = PRSW_DPRS(1)/2.0                                         1235
         AG_DY = PRSW_DZ(1)/2.0+PRSW_DSIPM(1)/2.0                         1235
         AG_DZ = DLG                                                      1235
         AG_TITLE = 'BOX'                                                 1235
         AG_EXNAME = 'SHAPE'                                              1235
         AG_PARLIST = 'DX_DY_DZ'                                          1235
         CALL AXSHAPE                                                     1235
C    *                                                                    1235
C    Create FLBA :                                                        1236
C    Call To Local Procedure FLBA                                         1236
         ASSIGN 6040 TO BLOCK_FLBA                                        1236
         GO TO  6030                                                      1236
6040     CONTINUE                                                         1236
C    *                                                                    1236
C    Create FLCO :                                                        1237
C    Call To Local Procedure FLCO                                         1237
         ASSIGN 6060 TO BLOCK_FLCO                                        1237
         GO TO  6050                                                      1237
6060     CONTINUE                                                         1237
C    *                                                                    1237
C    Create FPMT :                                                        1238
C    Call To Local Procedure FPMT                                         1238
         ASSIGN 6080 TO BLOCK_FPMT                                        1238
         GO TO  6070                                                      1238
6080     CONTINUE                                                         1238
C    *                                                                    1238
C    Create FFEE :                                                        1239
C    Call To Local Procedure FFEE                                         1239
         ASSIGN 6100 TO BLOCK_FFEE                                        1239
         GO TO  6090                                                      1239
6100     CONTINUE                                                         1239
C    *                                                                    1239
C    Geant POSITION : FLBA ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=PRSW_DLG(3)/2  1240
C    .0-DLG <===                                                          1240
         CALL AGSRESET                                                    1240
         AG_X = 0                                                         1240
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1240
         AG_Z = PRSW_DLG(3)/2.0-DLG                                       1240
         AG_TITLE = 'FLBA'                                                1240
         AG_EXNAME = 'POSITION'                                           1240
         AG_PARLIST = 'X_Y_Z'                                             1240
         CALL AXPOSITION                                                  1240
C    *                                                                    1240
C    Geant POSITION : FLCO ===> X=-PRSW_DPRS(1)/4.0 Y=-PRSW_DSIPM(1)/2.0  1240
C     Z=PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG <===                              1240
         CALL AGSRESET                                                    1242
         AG_X = -PRSW_DPRS(1)/4.0                                         1242
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1242
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG                           1242
         AG_TITLE = 'FLCO'                                                1242
         AG_EXNAME = 'POSITION'                                           1242
         AG_PARLIST = 'X_Y_Z'                                             1242
         CALL AXPOSITION                                                  1242
C    *                                                                    1242
C    Geant POSITION : FLCO ===> X=+PRSW_DPRS(1)/4.0 Y=-PRSW_DSIPM(1)/2.0  1242
C     Z=PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG <===                              1242
         CALL AGSRESET                                                    1244
         AG_X = +PRSW_DPRS(1)/4.0                                         1244
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1244
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG                           1244
         AG_TITLE = 'FLCO'                                                1244
         AG_EXNAME = 'POSITION'                                           1244
         AG_PARLIST = 'X_Y_Z'                                             1244
         CALL AXPOSITION                                                  1244
C    *                                                                    1244
C    Geant POSITION : FPMT ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=DLG-PRSW_DSIP  1244
C    M(1)/2.0 <===                                                        1244
         CALL AGSRESET                                                    1245
         AG_X = 0                                                         1245
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1245
         AG_Z = DLG-PRSW_DSIPM(1)/2.0                                     1245
         AG_TITLE = 'FPMT'                                                1245
         AG_EXNAME = 'POSITION'                                           1245
         AG_PARLIST = 'X_Y_Z'                                             1245
         CALL AXPOSITION                                                  1245
C    *                                                                    1245
C    Geant POSITION : FFEE ===> X=0 Y=PRSW_DZ(1)/2.0 Z=DLG-PRSW_DSIPM(4)  1245
C    /2.0 <===                                                            1245
         CALL AGSRESET                                                    1246
         AG_X = 0                                                         1246
         AG_Y = PRSW_DZ(1)/2.0                                            1246
         AG_Z = DLG-PRSW_DSIPM(4)/2.0                                     1246
         AG_TITLE = 'FFEE'                                                1246
         AG_EXNAME = 'POSITION'                                           1246
         AG_PARLIST = 'X_Y_Z'                                             1246
         CALL AXPOSITION                                                  1246
C    *                                                                    1246
5831  CALL AGSPOPB                                                        1247
      GO TO BLOCK_FLIG, (5830 ,5840)                                      1247
C End of Local Procedure                                                  1247
C ----------------------------------------------------------------------  1248
C ------------ FLLG                                                       1248
C Begin Local Procedure FLLG                                              1249
C FLLG IS A LIGHT GUIDE + SIPM BOARD FOR PRESHOWER FOR LARGE              1249
5850  CONTINUE                                                            1249
         AG_TITLE='FLLG'                                                  1249
         AG_MODULE=MODULE                                                 1249
         AG_MARK='V00'                                                    1249
         CALL AGSPUSH                                                     1249
         AG_MODULE=MODULE                                                 1249
C    *                                                                    1249
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1250
         CALL AGSRESET                                                    1250
         AG_TITLE = 'AIR'                                                 1250
         AG_EXNAME = 'MATERIAL'                                           1250
         AG_PARLIST = 'NONE'                                              1250
         CALL AXMATERIAL                                                  1250
C    *                                                                    1250
C    Geant ATTRIBUTE : FLLG ===> SEEN=1 COLO=6 SERIAL=1 <===              1250
         CALL AGSRESET                                                    1251
         AG_SEEN = 1                                                      1251
         AG_COLO = 6                                                      1251
         AG_SERIAL = 1                                                    1251
         AG_TITLE = 'FLLG'                                                1251
         AG_EXNAME = 'ATTRIBUTE'                                          1251
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1251
         CALL AXATTRIBUTE                                                 1251
C    *                                                                    1251
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(2)/2.0 DY=PRSW_DZ(1)/2.0+PRSW_D  1251
C    SIPM(1)/2.0, DZ=DLG <===                                             1251
         CALL AGSRESET                                                    1253
         AG_DX = PRSW_DPRS(2)/2.0                                         1253
         AG_DY = PRSW_DZ(1)/2.0+PRSW_DSIPM(1)/2.0                         1253
         AG_DZ = DLG                                                      1253
         AG_TITLE = 'BOX'                                                 1253
         AG_EXNAME = 'SHAPE'                                              1253
         AG_PARLIST = 'DX_DY_DZ'                                          1253
         CALL AXSHAPE                                                     1253
C    *                                                                    1253
C    Create FBLL :                                                        1254
C    Call To Local Procedure FBLL                                         1254
         ASSIGN 6120 TO BLOCK_FBLL                                        1254
         GO TO  6110                                                      1254
6120     CONTINUE                                                         1254
C    *                                                                    1254
C    Create FCLL :                                                        1255
C    Call To Local Procedure FCLL                                         1255
         ASSIGN 6140 TO BLOCK_FCLL                                        1255
         GO TO  6130                                                      1255
6140     CONTINUE                                                         1255
C    *                                                                    1255
C    Create FPMT :                                                        1256
C    Call To Local Procedure FPMT                                         1256
         ASSIGN 6150 TO BLOCK_FPMT                                        1256
         GO TO  6070                                                      1256
6150     CONTINUE                                                         1256
C    *                                                                    1256
C    Create FFEE :                                                        1257
C    Call To Local Procedure FFEE                                         1257
         ASSIGN 6160 TO BLOCK_FFEE                                        1257
         GO TO  6090                                                      1257
6160     CONTINUE                                                         1257
C    *                                                                    1257
C    Geant POSITION : FBLL ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=PRSW_DLG(3)/2  1258
C    .0-DLG <===                                                          1258
         CALL AGSRESET                                                    1258
         AG_X = 0                                                         1258
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1258
         AG_Z = PRSW_DLG(3)/2.0-DLG                                       1258
         AG_TITLE = 'FBLL'                                                1258
         AG_EXNAME = 'POSITION'                                           1258
         AG_PARLIST = 'X_Y_Z'                                             1258
         CALL AXPOSITION                                                  1258
C    *                                                                    1258
C    Geant POSITION : FCLL ===> X=-PRSW_DPRS(2)/4.0 Y=-PRSW_DSIPM(1)/2.0  1258
C     Z=PRSW_DLG(3)+PRSW_DLG(2)/2-DLG <===                                1258
         CALL AGSRESET                                                    1260
         AG_X = -PRSW_DPRS(2)/4.0                                         1260
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1260
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2-DLG                             1260
         AG_TITLE = 'FCLL'                                                1260
         AG_EXNAME = 'POSITION'                                           1260
         AG_PARLIST = 'X_Y_Z'                                             1260
         CALL AXPOSITION                                                  1260
C    *                                                                    1260
C    Geant POSITION : FCLL ===> X=PRSW_DPRS(2)/4.0 Y=-PRSW_DSIPM(1)/2.0   1260
C    Z=PRSW_DLG(3)+PRSW_DLG(2)/2-DLG <===                                 1260
         CALL AGSRESET                                                    1262
         AG_X = PRSW_DPRS(2)/4.0                                          1262
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1262
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2-DLG                             1262
         AG_TITLE = 'FCLL'                                                1262
         AG_EXNAME = 'POSITION'                                           1262
         AG_PARLIST = 'X_Y_Z'                                             1262
         CALL AXPOSITION                                                  1262
C    *                                                                    1262
C    Geant POSITION : FPMT ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=DLG-PRSW_DSIP  1262
C    M(1)/2.0 <===                                                        1262
         CALL AGSRESET                                                    1263
         AG_X = 0                                                         1263
         AG_Y = -PRSW_DSIPM(1)/2.0                                        1263
         AG_Z = DLG-PRSW_DSIPM(1)/2.0                                     1263
         AG_TITLE = 'FPMT'                                                1263
         AG_EXNAME = 'POSITION'                                           1263
         AG_PARLIST = 'X_Y_Z'                                             1263
         CALL AXPOSITION                                                  1263
C    *                                                                    1263
C    Geant POSITION : FFEE ===> X=0 Y=PRSW_DZ(1)/2.0 Z=DLG-PRSW_DSIPM(4)  1263
C    /2.0 <===                                                            1263
         CALL AGSRESET                                                    1264
         AG_X = 0                                                         1264
         AG_Y = PRSW_DZ(1)/2.0                                            1264
         AG_Z = DLG-PRSW_DSIPM(4)/2.0                                     1264
         AG_TITLE = 'FFEE'                                                1264
         AG_EXNAME = 'POSITION'                                           1264
         AG_PARLIST = 'X_Y_Z'                                             1264
         CALL AXPOSITION                                                  1264
C    *                                                                    1264
5851  CALL AGSPOPB                                                        1265
      GO TO BLOCK_FLLG, (5850 ,5860)                                      1265
C End of Local Procedure                                                  1265
C ----------------------------------------------------------------------  1266
C ------------ FLBA                                                       1266
C Begin Local Procedure FLBA                                              1267
C FLBA IS A LIGHT GUIDE BASE FOR PRESHOWER                                1267
6030  CONTINUE                                                            1267
         AG_TITLE='FLBA'                                                  1267
         AG_MODULE=MODULE                                                 1267
         AG_MARK='V00'                                                    1267
         CALL AGSPUSH                                                     1267
         AG_MODULE=MODULE                                                 1267
C    *                                                                    1267
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1268
         CALL AGSRESET                                                    1268
         AG_TITLE = 'POLYSTYREN'                                          1268
         AG_EXNAME = 'MATERIAL'                                           1268
         AG_PARLIST = 'NONE'                                              1268
         CALL AXMATERIAL                                                  1268
C    *                                                                    1268
C    Geant ATTRIBUTE : FLBA ===> SEEN=1 COLO=4 SERIAL=1 <===              1268
         CALL AGSRESET                                                    1269
         AG_SEEN = 1                                                      1269
         AG_COLO = 4                                                      1269
         AG_SERIAL = 1                                                    1269
         AG_TITLE = 'FLBA'                                                1269
         AG_EXNAME = 'ATTRIBUTE'                                          1269
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1269
         CALL AXATTRIBUTE                                                 1269
C    *                                                                    1269
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(1)/2.0 DY=PRSW_DZ(SERN)/2.0, DZ  1269
C    =PRSW_DLG(3)/2.0 <===                                                1269
         CALL AGSRESET                                                    1271
         AG_DX = PRSW_DPRS(1)/2.0                                         1271
         AG_DY = PRSW_DZ(SERN)/2.0                                        1271
         AG_DZ = PRSW_DLG(3)/2.0                                          1271
         AG_TITLE = 'BOX'                                                 1271
         AG_EXNAME = 'SHAPE'                                              1271
         AG_PARLIST = 'DX_DY_DZ'                                          1271
         CALL AXSHAPE                                                     1271
C    *                                                                    1271
6031  CALL AGSPOPB                                                        1272
      GO TO BLOCK_FLBA, (6030 ,6040)                                      1272
C End of Local Procedure                                                  1272
C ----------------------------------------------------------------------  1273
C ------------ FBLL                                                       1273
C Begin Local Procedure FBLL                                              1274
C FBLL IS A LIGHT GUIDE BASE FOR PRESHOWER LARGE                          1274
6110  CONTINUE                                                            1274
         AG_TITLE='FBLL'                                                  1274
         AG_MODULE=MODULE                                                 1274
         AG_MARK='V00'                                                    1274
         CALL AGSPUSH                                                     1274
         AG_MODULE=MODULE                                                 1274
C    *                                                                    1274
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1275
         CALL AGSRESET                                                    1275
         AG_TITLE = 'POLYSTYREN'                                          1275
         AG_EXNAME = 'MATERIAL'                                           1275
         AG_PARLIST = 'NONE'                                              1275
         CALL AXMATERIAL                                                  1275
C    *                                                                    1275
C    Geant ATTRIBUTE : FBLL ===> SEEN=1 COLO=4 SERIAL=1 <===              1275
         CALL AGSRESET                                                    1276
         AG_SEEN = 1                                                      1276
         AG_COLO = 4                                                      1276
         AG_SERIAL = 1                                                    1276
         AG_TITLE = 'FBLL'                                                1276
         AG_EXNAME = 'ATTRIBUTE'                                          1276
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1276
         CALL AXATTRIBUTE                                                 1276
C    *                                                                    1276
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(2)/2.0 DY=PRSW_DZ(SERN)/2.0, DZ  1276
C    =PRSW_DLG(3)/2.0 <===                                                1276
         CALL AGSRESET                                                    1278
         AG_DX = PRSW_DPRS(2)/2.0                                         1278
         AG_DY = PRSW_DZ(SERN)/2.0                                        1278
         AG_DZ = PRSW_DLG(3)/2.0                                          1278
         AG_TITLE = 'BOX'                                                 1278
         AG_EXNAME = 'SHAPE'                                              1278
         AG_PARLIST = 'DX_DY_DZ'                                          1278
         CALL AXSHAPE                                                     1278
C    *                                                                    1278
6111  CALL AGSPOPB                                                        1279
      GO TO BLOCK_FBLL, (6110 ,6120)                                      1279
C End of Local Procedure                                                  1279
C ----------------------------------------------------------------------  1280
C ------------ FLCO                                                       1280
C Begin Local Procedure FLCO                                              1281
C FLCO IS A LIGHT GUIDE CONE FOR PRESHOWER                                1281
6050  CONTINUE                                                            1281
         AG_TITLE='FLCO'                                                  1281
         AG_MODULE=MODULE                                                 1281
         AG_MARK='V00'                                                    1281
         CALL AGSPUSH                                                     1281
         AG_MODULE=MODULE                                                 1281
C    *                                                                    1281
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1282
         CALL AGSRESET                                                    1282
         AG_TITLE = 'POLYSTYREN'                                          1282
         AG_EXNAME = 'MATERIAL'                                           1282
         AG_PARLIST = 'NONE'                                              1282
         CALL AXMATERIAL                                                  1282
C    *                                                                    1282
C    Geant ATTRIBUTE : FLCO ===> SEEN=1 COLO=4 SERIAL=1 <===              1282
         CALL AGSRESET                                                    1283
         AG_SEEN = 1                                                      1283
         AG_COLO = 4                                                      1283
         AG_SERIAL = 1                                                    1283
         AG_TITLE = 'FLCO'                                                1283
         AG_EXNAME = 'ATTRIBUTE'                                          1283
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1283
         CALL AXATTRIBUTE                                                 1283
C    *                                                                    1283
C    Geant SHAPE : TRD2 ===> DX1=PRSW_DPRS(1)/4.0 DX2=PRSW_DLG(1)/2.0, D  1283
C    Y1=PRSW_DZ(SERN)/2.0 DY2=PRSW_DLG(1)/2.0, DZ=PRSW_DLG(2)/2.0 <===    1283
         CALL AGSRESET                                                    1286
         AG_DX1 = PRSW_DPRS(1)/4.0                                        1286
         AG_DX2 = PRSW_DLG(1)/2.0                                         1286
         AG_DY1 = PRSW_DZ(SERN)/2.0                                       1286
         AG_DY2 = PRSW_DLG(1)/2.0                                         1286
         AG_DZ = PRSW_DLG(2)/2.0                                          1286
         AG_TITLE = 'TRD2'                                                1286
         AG_EXNAME = 'SHAPE'                                              1286
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                1286
         CALL AXSHAPE                                                     1286
C    *                                                                    1286
6051  CALL AGSPOPB                                                        1287
      GO TO BLOCK_FLCO, (6050 ,6060)                                      1287
C End of Local Procedure                                                  1287
C ----------------------------------------------------------------------  1288
C ------------ FCLL                                                       1288
C Begin Local Procedure FCLL                                              1289
C FCLL IS A LIGHT GUIDE CONE FOR PRESHOWER LARGE                          1289
6130  CONTINUE                                                            1289
         AG_TITLE='FCLL'                                                  1289
         AG_MODULE=MODULE                                                 1289
         AG_MARK='V00'                                                    1289
         CALL AGSPUSH                                                     1289
         AG_MODULE=MODULE                                                 1289
C    *                                                                    1289
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1290
         CALL AGSRESET                                                    1290
         AG_TITLE = 'POLYSTYREN'                                          1290
         AG_EXNAME = 'MATERIAL'                                           1290
         AG_PARLIST = 'NONE'                                              1290
         CALL AXMATERIAL                                                  1290
C    *                                                                    1290
C    Geant ATTRIBUTE : FCLL ===> SEEN=1 COLO=4 SERIAL=1 <===              1290
         CALL AGSRESET                                                    1291
         AG_SEEN = 1                                                      1291
         AG_COLO = 4                                                      1291
         AG_SERIAL = 1                                                    1291
         AG_TITLE = 'FCLL'                                                1291
         AG_EXNAME = 'ATTRIBUTE'                                          1291
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1291
         CALL AXATTRIBUTE                                                 1291
C    *                                                                    1291
C    Geant SHAPE : TRD2 ===> DX1=PRSW_DPRS(2)/4.0 DX2=PRSW_DLG(1)/2.0, D  1291
C    Y1=PRSW_DZ(SERN)/2.0 DY2=PRSW_DLG(1)/2.0, DZ=PRSW_DLG(2)/2.0 <===    1291
         CALL AGSRESET                                                    1294
         AG_DX1 = PRSW_DPRS(2)/4.0                                        1294
         AG_DX2 = PRSW_DLG(1)/2.0                                         1294
         AG_DY1 = PRSW_DZ(SERN)/2.0                                       1294
         AG_DY2 = PRSW_DLG(1)/2.0                                         1294
         AG_DZ = PRSW_DLG(2)/2.0                                          1294
         AG_TITLE = 'TRD2'                                                1294
         AG_EXNAME = 'SHAPE'                                              1294
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                1294
         CALL AXSHAPE                                                     1294
C    *                                                                    1294
6131  CALL AGSPOPB                                                        1295
      GO TO BLOCK_FCLL, (6130 ,6140)                                      1295
C End of Local Procedure                                                  1295
C ----------------------------------------------------------------------  1296
C ------------ FPMT                                                       1296
C Begin Local Procedure FPMT                                              1297
C FPMT IS A SIPM BOARD FOR PRESHOWER                                      1297
6070  CONTINUE                                                            1297
         AG_TITLE='FPMT'                                                  1297
         AG_MODULE=MODULE                                                 1297
         AG_MARK='V00'                                                    1297
         CALL AGSPUSH                                                     1297
         AG_MODULE=MODULE                                                 1297
C    *                                                                    1297
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1298
         CALL AGSRESET                                                    1298
         AG_TITLE = 'POLYSTYREN'                                          1298
         AG_EXNAME = 'MATERIAL'                                           1298
         AG_PARLIST = 'NONE'                                              1298
         CALL AXMATERIAL                                                  1298
C    *                                                                    1298
C    Geant ATTRIBUTE : FPMT ===> SEEN=1 COLO=3 SERIAL=1 <===              1298
         CALL AGSRESET                                                    1299
         AG_SEEN = 1                                                      1299
         AG_COLO = 3                                                      1299
         AG_SERIAL = 1                                                    1299
         AG_TITLE = 'FPMT'                                                1299
         AG_EXNAME = 'ATTRIBUTE'                                          1299
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1299
         CALL AXATTRIBUTE                                                 1299
C    *                                                                    1299
C    Geant SHAPE : BOX ===> DX=PRSW_DSIPM(2)/2.0 DY=PRSW_DSIPM(3)/2.0, D  1299
C    Z=PRSW_DSIPM(1)/2.0 <===                                             1299
         CALL AGSRESET                                                    1301
         AG_DX = PRSW_DSIPM(2)/2.0                                        1301
         AG_DY = PRSW_DSIPM(3)/2.0                                        1301
         AG_DZ = PRSW_DSIPM(1)/2.0                                        1301
         AG_TITLE = 'BOX'                                                 1301
         AG_EXNAME = 'SHAPE'                                              1301
         AG_PARLIST = 'DX_DY_DZ'                                          1301
         CALL AXSHAPE                                                     1301
C    *                                                                    1301
6071  CALL AGSPOPB                                                        1302
      GO TO BLOCK_FPMT, (6070 ,6080,6150)                                 1302
C End of Local Procedure                                                  1302
C ----------------------------------------------------------------------  1303
C ------------ FFEE                                                       1303
C Begin Local Procedure FFEE                                              1304
C FFEE IS A FEE BOARD FOR PRESHOWER                                       1304
6090  CONTINUE                                                            1304
         AG_TITLE='FFEE'                                                  1304
         AG_MODULE=MODULE                                                 1304
         AG_MARK='V00'                                                    1304
         CALL AGSPUSH                                                     1304
         AG_MODULE=MODULE                                                 1304
C    *                                                                    1304
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1305
         CALL AGSRESET                                                    1305
         AG_TITLE = 'POLYSTYREN'                                          1305
         AG_EXNAME = 'MATERIAL'                                           1305
         AG_PARLIST = 'NONE'                                              1305
         CALL AXMATERIAL                                                  1305
C    *                                                                    1305
C    Geant ATTRIBUTE : FEEE ===> SEEN=1 COLO=3 SERIAL=1 <===              1305
         CALL AGSRESET                                                    1306
         AG_SEEN = 1                                                      1306
         AG_COLO = 3                                                      1306
         AG_SERIAL = 1                                                    1306
         AG_TITLE = 'FEEE'                                                1306
         AG_EXNAME = 'ATTRIBUTE'                                          1306
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1306
         CALL AXATTRIBUTE                                                 1306
C    *                                                                    1306
C    Geant SHAPE : BOX ===> DX=PRSW_DSIPM(2)/2.0 DY=PRSW_DSIPM(1)/2.0, D  1306
C    Z=PRSW_DSIPM(4)/2.0 <===                                             1306
         CALL AGSRESET                                                    1308
         AG_DX = PRSW_DSIPM(2)/2.0                                        1308
         AG_DY = PRSW_DSIPM(1)/2.0                                        1308
         AG_DZ = PRSW_DSIPM(4)/2.0                                        1308
         AG_TITLE = 'BOX'                                                 1308
         AG_EXNAME = 'SHAPE'                                              1308
         AG_PARLIST = 'DX_DY_DZ'                                          1308
         CALL AXSHAPE                                                     1308
C    *                                                                    1308
6091  CALL AGSPOPB                                                        1309
      GO TO BLOCK_FFEE, (6090 ,6100,6160)                                 1309
C End of Local Procedure                                                  1309
C ----------------------------------------------------------------------  1310
C ------------ WPPL                                                       1310
C Begin Local Procedure WPPL                                              1311
C WPPL IS A POLE FOR WEST PLATFORM                                        1311
5070  CONTINUE                                                            1311
         AG_TITLE='WPPL'                                                  1311
         AG_MODULE=MODULE                                                 1311
         AG_MARK='V00'                                                    1311
         CALL AGSPUSH                                                     1311
         AG_MODULE=MODULE                                                 1311
C    *                                                                    1311
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1312
         CALL AGSRESET                                                    1312
         AG_TITLE = 'IRON'                                                1312
         AG_EXNAME = 'MATERIAL'                                           1312
         AG_PARLIST = 'NONE'                                              1312
         CALL AXMATERIAL                                                  1312
C    *                                                                    1312
C    Geant ATTRIBUTE : WPPL ===> SEEN=1 COLO=4 <===                       1312
         CALL AGSRESET                                                    1313
         AG_SEEN = 1                                                      1313
         AG_COLO = 4                                                      1313
         AG_TITLE = 'WPPL'                                                1313
         AG_EXNAME = 'ATTRIBUTE'                                          1313
         AG_PARLIST = 'SEEN_COLO'                                         1313
         CALL AXATTRIBUTE                                                 1313
C    *                                                                    1313
C    Geant SHAPE : BOX ===> DX=WPFM_POLED(1)/2.0 DY=WPFM_POLED(2)/2.0, D  1313
C    Z=WPFM_POLED(3)/2.0 <===                                             1313
         CALL AGSRESET                                                    1315
         AG_DX = WPFM_POLED(1)/2.0                                        1315
         AG_DY = WPFM_POLED(2)/2.0                                        1315
         AG_DZ = WPFM_POLED(3)/2.0                                        1315
         AG_TITLE = 'BOX'                                                 1315
         AG_EXNAME = 'SHAPE'                                              1315
         AG_PARLIST = 'DX_DY_DZ'                                          1315
         CALL AXSHAPE                                                     1315
C    *                                                                    1315
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=0.0 DZ=WPFM_POLED(3)/2.0-W  1315
C    PFM_POLED(4) DX=WPFM_POLED(1)/2.0-WPFM_POLED(4) DY=WPFM_POLED(2)/2.  1315
C    0 <===                                                               1315
         CALL AGSRESET                                                    1317
         AG_X = 0.0                                                       1317
         AG_Y = 0.0                                                       1317
         AG_Z = 0.0                                                       1317
         AG_DZ = WPFM_POLED(3)/2.0-WPFM_POLED(4)                          1317
         AG_DX = WPFM_POLED(1)/2.0-WPFM_POLED(4)                          1317
         AG_DY = WPFM_POLED(2)/2.0                                        1317
         AG_TITLE = 'WPAR'                                                1317
         AG_EXNAME = 'POSITION'                                           1317
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    1317
         CALL AXPOSITION                                                  1317
C    *                                                                    1317
5071  CALL AGSPOPB                                                        1318
      GO TO BLOCK_WPPL, (5070 ,5080)                                      1318
C End of Local Procedure                                                  1318
C ----------------------------------------------------------------------  1319
C ------------ WPIS                                                       1319
C Begin Local Procedure WPIS                                              1320
C WPIS IS A I-STRUCT FOR WEST PLATFORM                                    1320
5090  CONTINUE                                                            1320
         AG_TITLE='WPIS'                                                  1320
         AG_MODULE=MODULE                                                 1320
         AG_MARK='V00'                                                    1320
         CALL AGSPUSH                                                     1320
         AG_MODULE=MODULE                                                 1320
C    *                                                                    1320
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1321
         CALL AGSRESET                                                    1321
         AG_TITLE = 'IRON'                                                1321
         AG_EXNAME = 'MATERIAL'                                           1321
         AG_PARLIST = 'NONE'                                              1321
         CALL AXMATERIAL                                                  1321
C    *                                                                    1321
C    Geant ATTRIBUTE : WPIS ===> SEEN=1 COLO=4 <===                       1321
         CALL AGSRESET                                                    1322
         AG_SEEN = 1                                                      1322
         AG_COLO = 4                                                      1322
         AG_TITLE = 'WPIS'                                                1322
         AG_EXNAME = 'ATTRIBUTE'                                          1322
         AG_PARLIST = 'SEEN_COLO'                                         1322
         CALL AXATTRIBUTE                                                 1322
C    *                                                                    1322
C    Geant SHAPE : BOX ===> DX=WPFM_ISTRD(1)/2.0 DY=WPFM_ISTRD(2)/2.0, D  1322
C    Z=WPFM_ISTRD(3)/2.0 <===                                             1322
         CALL AGSRESET                                                    1324
         AG_DX = WPFM_ISTRD(1)/2.0                                        1324
         AG_DY = WPFM_ISTRD(2)/2.0                                        1324
         AG_DZ = WPFM_ISTRD(3)/2.0                                        1324
         AG_TITLE = 'BOX'                                                 1324
         AG_EXNAME = 'SHAPE'                                              1324
         AG_PARLIST = 'DX_DY_DZ'                                          1324
         CALL AXSHAPE                                                     1324
C    *                                                                    1324
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=+(WPFM_ISTRD(3)/4.0-WPFM_I  1324
C    STRD(4)/2.0) DZ=(WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0 DX=WPFM_I  1324
C    STRD(1)/2.0 DY=WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4) <===                  1324
         CALL AGSRESET                                                    1327
         AG_X = 0.0                                                       1327
         AG_Y = 0.0                                                       1327
         AG_Z = +(WPFM_ISTRD(3)/4.0-WPFM_ISTRD(4)/2.0)                    1327
         AG_DZ = (WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0                1327
         AG_DX = WPFM_ISTRD(1)/2.0                                        1327
         AG_DY = WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4)                          1327
         AG_TITLE = 'WPAR'                                                1327
         AG_EXNAME = 'POSITION'                                           1327
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    1327
         CALL AXPOSITION                                                  1327
C    *                                                                    1327
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=-(WPFM_ISTRD(3)/4.0-WPFM_I  1327
C    STRD(4)/2.0) DZ=(WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0 DX=WPFM_I  1327
C    STRD(1)/2.0 DY=WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4) <===                  1327
         CALL AGSRESET                                                    1330
         AG_X = 0.0                                                       1330
         AG_Y = 0.0                                                       1330
         AG_Z = -(WPFM_ISTRD(3)/4.0-WPFM_ISTRD(4)/2.0)                    1330
         AG_DZ = (WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0                1330
         AG_DX = WPFM_ISTRD(1)/2.0                                        1330
         AG_DY = WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4)                          1330
         AG_TITLE = 'WPAR'                                                1330
         AG_EXNAME = 'POSITION'                                           1330
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    1330
         CALL AXPOSITION                                                  1330
C    *                                                                    1330
5091  CALL AGSPOPB                                                        1331
      GO TO BLOCK_WPIS, (5090 ,5100)                                      1331
C End of Local Procedure                                                  1331
C ----------------------------------------------------------------------  1332
C ------------ WPUF                                                       1332
C Begin Local Procedure WPUF                                              1333
C WPUF IS A FRONT U-STRUCT FOR WEST PLATFORM                              1333
5110  CONTINUE                                                            1333
         AG_TITLE='WPUF'                                                  1333
         AG_MODULE=MODULE                                                 1333
         AG_MARK='V00'                                                    1333
         CALL AGSPUSH                                                     1333
         AG_MODULE=MODULE                                                 1333
C    *                                                                    1333
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1334
         CALL AGSRESET                                                    1334
         AG_TITLE = 'IRON'                                                1334
         AG_EXNAME = 'MATERIAL'                                           1334
         AG_PARLIST = 'NONE'                                              1334
         CALL AXMATERIAL                                                  1334
C    *                                                                    1334
C    Geant ATTRIBUTE : WPUF ===> SEEN=1 COLO=4 <===                       1334
         CALL AGSRESET                                                    1335
         AG_SEEN = 1                                                      1335
         AG_COLO = 4                                                      1335
         AG_TITLE = 'WPUF'                                                1335
         AG_EXNAME = 'ATTRIBUTE'                                          1335
         AG_PARLIST = 'SEEN_COLO'                                         1335
         CALL AXATTRIBUTE                                                 1335
C    *                                                                    1335
C    Geant SHAPE : BOX ===> DX=WPFM_USTFD(1)/2.0 DY=WPFM_USTFD(2)/2.0, D  1335
C    Z=WPFM_USTFD(3)/2.0 <===                                             1335
         CALL AGSRESET                                                    1337
         AG_DX = WPFM_USTFD(1)/2.0                                        1337
         AG_DY = WPFM_USTFD(2)/2.0                                        1337
         AG_DZ = WPFM_USTFD(3)/2.0                                        1337
         AG_TITLE = 'BOX'                                                 1337
         AG_EXNAME = 'SHAPE'                                              1337
         AG_PARLIST = 'DX_DY_DZ'                                          1337
         CALL AXSHAPE                                                     1337
C    *                                                                    1337
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=WPFM_USTFD(4)/2.0 DZ=WPFM_  1337
C    USTFD(3)/2.0-WPFM_USTFD(4)/2.0 DX=WPFM_USTFD(1)/2.0 DY=WPFM_USTFD(2  1337
C    )/2.0-WPFM_USTFD(4) <===                                             1337
         CALL AGSRESET                                                    1340
         AG_X = 0.0                                                       1340
         AG_Y = 0.0                                                       1340
         AG_Z = WPFM_USTFD(4)/2.0                                         1340
         AG_DZ = WPFM_USTFD(3)/2.0-WPFM_USTFD(4)/2.0                      1340
         AG_DX = WPFM_USTFD(1)/2.0                                        1340
         AG_DY = WPFM_USTFD(2)/2.0-WPFM_USTFD(4)                          1340
         AG_TITLE = 'WPAR'                                                1340
         AG_EXNAME = 'POSITION'                                           1340
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    1340
         CALL AXPOSITION                                                  1340
C    *                                                                    1340
5111  CALL AGSPOPB                                                        1341
      GO TO BLOCK_WPUF, (5110 ,5120)                                      1341
C End of Local Procedure                                                  1341
C ----------------------------------------------------------------------  1342
C ------------ WPUN                                                       1342
C Begin Local Procedure WPUN                                              1343
C WPUN IS A NORTH U-STRUCT FOR WEST PLATFORM                              1343
5130  CONTINUE                                                            1343
         AG_TITLE='WPUN'                                                  1343
         AG_MODULE=MODULE                                                 1343
         AG_MARK='V00'                                                    1343
         CALL AGSPUSH                                                     1343
         AG_MODULE=MODULE                                                 1343
C    *                                                                    1343
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1344
         CALL AGSRESET                                                    1344
         AG_TITLE = 'IRON'                                                1344
         AG_EXNAME = 'MATERIAL'                                           1344
         AG_PARLIST = 'NONE'                                              1344
         CALL AXMATERIAL                                                  1344
C    *                                                                    1344
C    Geant ATTRIBUTE : WPUN ===> SEEN=1 COLO=4 <===                       1344
         CALL AGSRESET                                                    1345
         AG_SEEN = 1                                                      1345
         AG_COLO = 4                                                      1345
         AG_TITLE = 'WPUN'                                                1345
         AG_EXNAME = 'ATTRIBUTE'                                          1345
         AG_PARLIST = 'SEEN_COLO'                                         1345
         CALL AXATTRIBUTE                                                 1345
C    *                                                                    1345
C    Geant SHAPE : BOX ===> DX=WPFM_USTND(1)/2.0 DY=WPFM_USTND(2)/2.0, D  1345
C    Z=WPFM_USTND(3)/2.0 <===                                             1345
         CALL AGSRESET                                                    1347
         AG_DX = WPFM_USTND(1)/2.0                                        1347
         AG_DY = WPFM_USTND(2)/2.0                                        1347
         AG_DZ = WPFM_USTND(3)/2.0                                        1347
         AG_TITLE = 'BOX'                                                 1347
         AG_EXNAME = 'SHAPE'                                              1347
         AG_PARLIST = 'DX_DY_DZ'                                          1347
         CALL AXSHAPE                                                     1347
C    *                                                                    1347
C    Geant POSITION : WPAR ===> X=+WPFM_USTND(4)/2.0 Y=0.0 Z=0.0 DZ=WPFM  1347
C    _USTND(3)/2.0 DX=WPFM_USTND(1)/2.0-WPFM_USTND(4)/2.0 DY=WPFM_USTND(  1347
C    2)/2.0-WPFM_USTND(4) <===                                            1347
         CALL AGSRESET                                                    1349
         AG_X = +WPFM_USTND(4)/2.0                                        1349
         AG_Y = 0.0                                                       1349
         AG_Z = 0.0                                                       1349
         AG_DZ = WPFM_USTND(3)/2.0                                        1349
         AG_DX = WPFM_USTND(1)/2.0-WPFM_USTND(4)/2.0                      1349
         AG_DY = WPFM_USTND(2)/2.0-WPFM_USTND(4)                          1349
         AG_TITLE = 'WPAR'                                                1349
         AG_EXNAME = 'POSITION'                                           1349
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    1349
         CALL AXPOSITION                                                  1349
C    *                                                                    1349
5131  CALL AGSPOPB                                                        1350
      GO TO BLOCK_WPUN, (5130 ,5140)                                      1350
C End of Local Procedure                                                  1350
C ----------------------------------------------------------------------  1351
C ------------ WPUS                                                       1351
C Begin Local Procedure WPUS                                              1352
C WPUS IS A SOUTH U-STRUCT FOR WEST PLATFORM                              1352
5150  CONTINUE                                                            1352
         AG_TITLE='WPUS'                                                  1352
         AG_MODULE=MODULE                                                 1352
         AG_MARK='V00'                                                    1352
         CALL AGSPUSH                                                     1352
         AG_MODULE=MODULE                                                 1352
C    *                                                                    1352
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1353
         CALL AGSRESET                                                    1353
         AG_TITLE = 'IRON'                                                1353
         AG_EXNAME = 'MATERIAL'                                           1353
         AG_PARLIST = 'NONE'                                              1353
         CALL AXMATERIAL                                                  1353
C    *                                                                    1353
C    Geant ATTRIBUTE : WPUS ===> SEEN=1 COLO=4 <===                       1353
         CALL AGSRESET                                                    1354
         AG_SEEN = 1                                                      1354
         AG_COLO = 4                                                      1354
         AG_TITLE = 'WPUS'                                                1354
         AG_EXNAME = 'ATTRIBUTE'                                          1354
         AG_PARLIST = 'SEEN_COLO'                                         1354
         CALL AXATTRIBUTE                                                 1354
C    *                                                                    1354
C    Geant SHAPE : BOX ===> DX=WPFM_USTSD(1)/2.0 DY=WPFM_USTSD(2)/2.0, D  1354
C    Z=WPFM_USTSD(3)/2.0 <===                                             1354
         CALL AGSRESET                                                    1356
         AG_DX = WPFM_USTSD(1)/2.0                                        1356
         AG_DY = WPFM_USTSD(2)/2.0                                        1356
         AG_DZ = WPFM_USTSD(3)/2.0                                        1356
         AG_TITLE = 'BOX'                                                 1356
         AG_EXNAME = 'SHAPE'                                              1356
         AG_PARLIST = 'DX_DY_DZ'                                          1356
         CALL AXSHAPE                                                     1356
C    *                                                                    1356
C    Geant POSITION : WPAR ===> X=-WPFM_USTSD(4)/2.0 Y=0.0 Z=0.0 DZ=WPFM  1356
C    _USTSD(3)/2.0 DX=WPFM_USTSD(1)/2.0-WPFM_USTSD(4)/2.0 DY=WPFM_USTSD(  1356
C    2)/2.0-WPFM_USTSD(4) <===                                            1356
         CALL AGSRESET                                                    1358
         AG_X = -WPFM_USTSD(4)/2.0                                        1358
         AG_Y = 0.0                                                       1358
         AG_Z = 0.0                                                       1358
         AG_DZ = WPFM_USTSD(3)/2.0                                        1358
         AG_DX = WPFM_USTSD(1)/2.0-WPFM_USTSD(4)/2.0                      1358
         AG_DY = WPFM_USTSD(2)/2.0-WPFM_USTSD(4)                          1358
         AG_TITLE = 'WPAR'                                                1358
         AG_EXNAME = 'POSITION'                                           1358
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    1358
         CALL AXPOSITION                                                  1358
C    *                                                                    1358
5151  CALL AGSPOPB                                                        1359
      GO TO BLOCK_WPUS, (5150 ,5160)                                      1359
C End of Local Procedure                                                  1359
C ----------------------------------------------------------------------  1360
C ------------ WPAR                                                       1360
C Begin Local Procedure WPAR                                              1361
C WPAR IS A PLATFORM STRUCTS AIR                                          1361
5870  CONTINUE                                                            1361
         AG_TITLE='WPAR'                                                  1361
         AG_MODULE=MODULE                                                 1361
         AG_MARK='V00'                                                    1361
         CALL AGSPUSH                                                     1361
         AG_MODULE=MODULE                                                 1361
C    *                                                                    1361
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1362
         CALL AGSRESET                                                    1362
         AG_TITLE = 'AIR'                                                 1362
         AG_EXNAME = 'MATERIAL'                                           1362
         AG_PARLIST = 'NONE'                                              1362
         CALL AXMATERIAL                                                  1362
C    *                                                                    1362
C    Geant ATTRIBUTE : WPAR ===> SEEN=1 COLO=0 <===                       1362
         CALL AGSRESET                                                    1363
         AG_SEEN = 1                                                      1363
         AG_COLO = 0                                                      1363
         AG_TITLE = 'WPAR'                                                1363
         AG_EXNAME = 'ATTRIBUTE'                                          1363
         AG_PARLIST = 'SEEN_COLO'                                         1363
         CALL AXATTRIBUTE                                                 1363
C    *                                                                    1363
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1363
         CALL AGSRESET                                                    1364
         AG_DX = 0.0                                                      1364
         AG_DY = 0.0                                                      1364
         AG_DZ = 0.0                                                      1364
         AG_TITLE = 'BOX'                                                 1364
         AG_EXNAME = 'SHAPE'                                              1364
         AG_PARLIST = 'DX_DY_DZ'                                          1364
         CALL AXSHAPE                                                     1364
C    *                                                                    1364
5871  CALL AGSPOPB                                                        1365
      GO TO BLOCK_WPAR, (5870 ,5880)                                      1365
C End of Local Procedure                                                  1365
      WRITE(LOUT,6170)'===>>  [End AgML Module FPDMGEO4]  <<==='          1365
6170  FORMAT(A32,/,/)                                                     1365
C MODULE FPDMGEO4                                                         1367
      END                                                                 1367
