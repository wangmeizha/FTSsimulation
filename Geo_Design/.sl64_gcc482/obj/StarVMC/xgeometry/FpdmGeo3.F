C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FPDMGEO3                                                 1
C FPDMGEO3 IS THE FORWARD PION DETECTOR MODULES GEOMETRY                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FPDMGEO3'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_FTOW                                                  10
      INTEGER BLOCK_FLXF                                                  10
      INTEGER BLOCK_FWAL                                                  10
      INTEGER BLOCK_FLGR                                                  10
      INTEGER BLOCK_FPCT                                                  10
      INTEGER BLOCK_FUMT                                                  10
      INTEGER BLOCK_PBPT                                                  10
      INTEGER BLOCK_FSHM                                                  10
      INTEGER BLOCK_FHMS                                                  10
      INTEGER BLOCK_FXGT                                                  10
      INTEGER BLOCK_FALU                                                  10
      INTEGER BLOCK_FBAS                                                  10
      INTEGER BLOCK_FENC                                                  10
      INTEGER BLOCK_FEAC                                                  10
      INTEGER BLOCK_FEBC                                                  10
      INTEGER BLOCK_FECC                                                  10
      INTEGER BLOCK_FEDC                                                  10
      INTEGER BLOCK_FETC                                                  10
      INTEGER BLOCK_FERC                                                  10
      INTEGER BLOCK_FESC                                                  10
      INTEGER BLOCK_FEEC                                                  10
C -                                                                       11
      REAL FMCG_VERSION                                                   11
      EQUIVALENCE (BANK_FMCG(3),FMCG_VERSION,RE_FMCG_VERSION),            11
     *(NAMES_FMCG(1),NFMCG_VERSION)                                       11
      EQUIVALENCE (COMMEN_FMCG(1),CFMCG_VERSION),(MAP_FMCG(1,1),          11
     *MFMCG_VERSION)                                                      11
      CHARACTER CFMCG_VERSION*40/' '/                                     11
      CHARACTER NFMCG_VERSION*12/'R.VERSION'/                             11
      INTEGER MFMCG_VERSION(2)/1,1/                                       11
      REAL RE_FMCG_VERSION                                                11
C -                                                                       11
      REAL FMCG_CHKVSIM                                                   11
      EQUIVALENCE (BANK_FMCG(4),FMCG_CHKVSIM,RE_FMCG_CHKVSIM),            11
     *(NAMES_FMCG(2),NFMCG_CHKVSIM)                                       11
      EQUIVALENCE (COMMEN_FMCG(2),CFMCG_CHKVSIM),(MAP_FMCG(1,2),          11
     *MFMCG_CHKVSIM)                                                      11
      CHARACTER CFMCG_CHKVSIM*40/' '/                                     11
      CHARACTER NFMCG_CHKVSIM*12/'R.CHKVSIM'/                             11
      INTEGER MFMCG_CHKVSIM(2)/1,1/                                       11
      REAL RE_FMCG_CHKVSIM                                                11
C -                                                                       11
      REAL FMCG_PBPLATE                                                   11
      EQUIVALENCE (BANK_FMCG(5),FMCG_PBPLATE,RE_FMCG_PBPLATE),            11
     *(NAMES_FMCG(3),NFMCG_PBPLATE)                                       11
      EQUIVALENCE (COMMEN_FMCG(3),CFMCG_PBPLATE),(MAP_FMCG(1,3),          11
     *MFMCG_PBPLATE)                                                      11
      CHARACTER CFMCG_PBPLATE*40/' '/                                     11
      CHARACTER NFMCG_PBPLATE*12/'R.PBPLATE'/                             11
      INTEGER MFMCG_PBPLATE(2)/1,1/                                       11
      REAL RE_FMCG_PBPLATE                                                11
C -                                                                       11
      REAL FMCG_FMSNORTHX                                                 11
      EQUIVALENCE (BANK_FMCG(6),FMCG_FMSNORTHX,RE_FMCG_FMSNORTHX),        11
     *(NAMES_FMCG(4),NFMCG_FMSNORTHX)                                     11
      EQUIVALENCE (COMMEN_FMCG(4),CFMCG_FMSNORTHX),(MAP_FMCG(1,4),        11
     *MFMCG_FMSNORTHX)                                                    11
      CHARACTER CFMCG_FMSNORTHX*40/' '/                                   11
      CHARACTER NFMCG_FMSNORTHX*12/'R.FMSNORTHX'/                         11
      INTEGER MFMCG_FMSNORTHX(2)/1,1/                                     11
      REAL RE_FMCG_FMSNORTHX                                              11
C -                                                                       11
      REAL FMCG_FMSSOUTHX                                                 11
      EQUIVALENCE (BANK_FMCG(7),FMCG_FMSSOUTHX,RE_FMCG_FMSSOUTHX),        11
     *(NAMES_FMCG(5),NFMCG_FMSSOUTHX)                                     11
      EQUIVALENCE (COMMEN_FMCG(5),CFMCG_FMSSOUTHX),(MAP_FMCG(1,5),        11
     *MFMCG_FMSSOUTHX)                                                    11
      CHARACTER CFMCG_FMSSOUTHX*40/' '/                                   11
      CHARACTER NFMCG_FMSSOUTHX*12/'R.FMSSOUTHX'/                         11
      INTEGER MFMCG_FMSSOUTHX(2)/1,1/                                     11
      REAL RE_FMCG_FMSSOUTHX                                              11
C -                                                                       11
      INTEGER LEN_FMCG,LLEN_FMCG,LINK_FMCG                                11
      PARAMETER (LEN_FMCG=7)                                              11
      PARAMETER (LLEN_FMCG=5)                                             11
      INTEGER FLAG_FMCG/0/,MAP_FMCG(2,LLEN_FMCG),IDNUM_FMCG(10)           11
      CHARACTER NAMES_FMCG(LLEN_FMCG)*12,COMMEN_FMCG(LLEN_FMCG)*40        11
      DATA IDNUM_FMCG/10*0/                                               11
      REAL BANK_FMCG(LEN_FMCG)                                            11
      CHARACTER*60 BPATH_FMCG/' '/                                        11
      SAVE FLAG_FMCG,MAP_FMCG,IDNUM_FMCG,NAMES_FMCG,COMMEN_FMCG,          11
     *BPATH_FMCG                                                          11
      DATA LINK_FMCG/0/,BANK_FMCG/LEN_FMCG*0/                             11
C *                                                                       11
C -                                                                       12
      REAL FPOS_IMOD                                                      12
      EQUIVALENCE (BANK_FPOS(3),FPOS_IMOD,RE_FPOS_IMOD),(NAMES_FPOS(1),   12
     *NFPOS_IMOD)                                                         12
      EQUIVALENCE (COMMEN_FPOS(1),CFPOS_IMOD),(MAP_FPOS(1,1),             12
     *MFPOS_IMOD)                                                         12
      CHARACTER CFPOS_IMOD*40/' '/                                        12
      CHARACTER NFPOS_IMOD*12/'R.IMOD'/                                   12
      INTEGER MFPOS_IMOD(2)/1,1/                                          12
      REAL RE_FPOS_IMOD                                                   12
C -                                                                       12
      REAL FPOS_ITYPE                                                     12
      EQUIVALENCE (BANK_FPOS(4),FPOS_ITYPE,RE_FPOS_ITYPE),                12
     *(NAMES_FPOS(2),NFPOS_ITYPE)                                         12
      EQUIVALENCE (COMMEN_FPOS(2),CFPOS_ITYPE),(MAP_FPOS(1,2),            12
     *MFPOS_ITYPE)                                                        12
      CHARACTER CFPOS_ITYPE*40/' '/                                       12
      CHARACTER NFPOS_ITYPE*12/'R.ITYPE'/                                 12
      INTEGER MFPOS_ITYPE(2)/1,1/                                         12
      REAL RE_FPOS_ITYPE                                                  12
C -                                                                       12
      REAL FPOS_X                                                         12
      EQUIVALENCE (BANK_FPOS(5),FPOS_X,RE_FPOS_X),(NAMES_FPOS(3),         12
     *NFPOS_X)                                                            12
      EQUIVALENCE (COMMEN_FPOS(3),CFPOS_X),(MAP_FPOS(1,3),MFPOS_X)        12
      CHARACTER CFPOS_X*40/' '/                                           12
      CHARACTER NFPOS_X*12/'R.X'/                                         12
      INTEGER MFPOS_X(2)/1,1/                                             12
      REAL RE_FPOS_X                                                      12
C -                                                                       12
      REAL FPOS_Y                                                         12
      EQUIVALENCE (BANK_FPOS(6),FPOS_Y,RE_FPOS_Y),(NAMES_FPOS(4),         12
     *NFPOS_Y)                                                            12
      EQUIVALENCE (COMMEN_FPOS(4),CFPOS_Y),(MAP_FPOS(1,4),MFPOS_Y)        12
      CHARACTER CFPOS_Y*40/' '/                                           12
      CHARACTER NFPOS_Y*12/'R.Y'/                                         12
      INTEGER MFPOS_Y(2)/1,1/                                             12
      REAL RE_FPOS_Y                                                      12
C -                                                                       12
      REAL FPOS_Z                                                         12
      EQUIVALENCE (BANK_FPOS(7),FPOS_Z,RE_FPOS_Z),(NAMES_FPOS(5),         12
     *NFPOS_Z)                                                            12
      EQUIVALENCE (COMMEN_FPOS(5),CFPOS_Z),(MAP_FPOS(1,5),MFPOS_Z)        12
      CHARACTER CFPOS_Z*40/' '/                                           12
      CHARACTER NFPOS_Z*12/'R.Z'/                                         12
      INTEGER MFPOS_Z(2)/1,1/                                             12
      REAL RE_FPOS_Z                                                      12
C -                                                                       12
      REAL FPOS_AY                                                        12
      EQUIVALENCE (BANK_FPOS(8),FPOS_AY,RE_FPOS_AY),(NAMES_FPOS(6),       12
     *NFPOS_AY)                                                           12
      EQUIVALENCE (COMMEN_FPOS(6),CFPOS_AY),(MAP_FPOS(1,6),MFPOS_AY)      12
      CHARACTER CFPOS_AY*40/' '/                                          12
      CHARACTER NFPOS_AY*12/'R.AY'/                                       12
      INTEGER MFPOS_AY(2)/1,1/                                            12
      REAL RE_FPOS_AY                                                     12
C -                                                                       12
      REAL FPOS_AZ                                                        12
      EQUIVALENCE (BANK_FPOS(9),FPOS_AZ,RE_FPOS_AZ),(NAMES_FPOS(7),       12
     *NFPOS_AZ)                                                           12
      EQUIVALENCE (COMMEN_FPOS(7),CFPOS_AZ),(MAP_FPOS(1,7),MFPOS_AZ)      12
      CHARACTER CFPOS_AZ*40/' '/                                          12
      CHARACTER NFPOS_AZ*12/'R.AZ'/                                       12
      INTEGER MFPOS_AZ(2)/1,1/                                            12
      REAL RE_FPOS_AZ                                                     12
C -                                                                       12
      INTEGER LEN_FPOS,LLEN_FPOS,LINK_FPOS                                12
      PARAMETER (LEN_FPOS=9)                                              12
      PARAMETER (LLEN_FPOS=7)                                             12
      INTEGER FLAG_FPOS/0/,MAP_FPOS(2,LLEN_FPOS),IDNUM_FPOS(10)           12
      CHARACTER NAMES_FPOS(LLEN_FPOS)*12,COMMEN_FPOS(LLEN_FPOS)*40        12
      DATA IDNUM_FPOS/10*0/                                               12
      REAL BANK_FPOS(LEN_FPOS)                                            12
      CHARACTER*60 BPATH_FPOS/' '/                                        12
      SAVE FLAG_FPOS,MAP_FPOS,IDNUM_FPOS,NAMES_FPOS,COMMEN_FPOS,          12
     *BPATH_FPOS                                                          12
      DATA LINK_FPOS/0/,BANK_FPOS/LEN_FPOS*0/                             12
C *                                                                       12
C -                                                                       14
      REAL FBXD_TYPE                                                      14
      EQUIVALENCE (BANK_FBXD(3),FBXD_TYPE,RE_FBXD_TYPE),(NAMES_FBXD(1),   14
     *NFBXD_TYPE)                                                         14
      EQUIVALENCE (COMMEN_FBXD(1),CFBXD_TYPE),(MAP_FBXD(1,1),             14
     *MFBXD_TYPE)                                                         14
      CHARACTER CFBXD_TYPE*40/' '/                                        14
      CHARACTER NFBXD_TYPE*12/'R.TYPE'/                                   14
      INTEGER MFBXD_TYPE(2)/1,1/                                          14
      REAL RE_FBXD_TYPE                                                   14
C -                                                                       14
      REAL FBXD_HEIGHT                                                    14
      EQUIVALENCE (BANK_FBXD(4),FBXD_HEIGHT,RE_FBXD_HEIGHT),              14
     *(NAMES_FBXD(2),NFBXD_HEIGHT)                                        14
      EQUIVALENCE (COMMEN_FBXD(2),CFBXD_HEIGHT),(MAP_FBXD(1,2),           14
     *MFBXD_HEIGHT)                                                       14
      CHARACTER CFBXD_HEIGHT*40/' '/                                      14
      CHARACTER NFBXD_HEIGHT*12/'R.HEIGHT'/                               14
      INTEGER MFBXD_HEIGHT(2)/1,1/                                        14
      REAL RE_FBXD_HEIGHT                                                 14
C -                                                                       14
      REAL FBXD_DEPTH                                                     14
      EQUIVALENCE (BANK_FBXD(5),FBXD_DEPTH,RE_FBXD_DEPTH),                14
     *(NAMES_FBXD(3),NFBXD_DEPTH)                                         14
      EQUIVALENCE (COMMEN_FBXD(3),CFBXD_DEPTH),(MAP_FBXD(1,3),            14
     *MFBXD_DEPTH)                                                        14
      CHARACTER CFBXD_DEPTH*40/' '/                                       14
      CHARACTER NFBXD_DEPTH*12/'R.DEPTH'/                                 14
      INTEGER MFBXD_DEPTH(2)/1,1/                                         14
      REAL RE_FBXD_DEPTH                                                  14
C -                                                                       14
      REAL FBXD_WIDTH                                                     14
      EQUIVALENCE (BANK_FBXD(6),FBXD_WIDTH,RE_FBXD_WIDTH),                14
     *(NAMES_FBXD(4),NFBXD_WIDTH)                                         14
      EQUIVALENCE (COMMEN_FBXD(4),CFBXD_WIDTH),(MAP_FBXD(1,4),            14
     *MFBXD_WIDTH)                                                        14
      CHARACTER CFBXD_WIDTH*40/' '/                                       14
      CHARACTER NFBXD_WIDTH*12/'R.WIDTH'/                                 14
      INTEGER MFBXD_WIDTH(2)/1,1/                                         14
      REAL RE_FBXD_WIDTH                                                  14
C -                                                                       14
      REAL FBXD_NX                                                        14
      EQUIVALENCE (BANK_FBXD(7),FBXD_NX,RE_FBXD_NX),(NAMES_FBXD(5),       14
     *NFBXD_NX)                                                           14
      EQUIVALENCE (COMMEN_FBXD(5),CFBXD_NX),(MAP_FBXD(1,5),MFBXD_NX)      14
      CHARACTER CFBXD_NX*40/' '/                                          14
      CHARACTER NFBXD_NX*12/'R.NX'/                                       14
      INTEGER MFBXD_NX(2)/1,1/                                            14
      REAL RE_FBXD_NX                                                     14
C -                                                                       14
      REAL FBXD_NY                                                        14
      EQUIVALENCE (BANK_FBXD(8),FBXD_NY,RE_FBXD_NY),(NAMES_FBXD(6),       14
     *NFBXD_NY)                                                           14
      EQUIVALENCE (COMMEN_FBXD(6),CFBXD_NY),(MAP_FBXD(1,6),MFBXD_NY)      14
      CHARACTER CFBXD_NY*40/' '/                                          14
      CHARACTER NFBXD_NY*12/'R.NY'/                                       14
      INTEGER MFBXD_NY(2)/1,1/                                            14
      REAL RE_FBXD_NY                                                     14
C -                                                                       14
      REAL FBXD_NXL                                                       14
      EQUIVALENCE (BANK_FBXD(9),FBXD_NXL,RE_FBXD_NXL),(NAMES_FBXD(7),     14
     *NFBXD_NXL)                                                          14
      EQUIVALENCE (COMMEN_FBXD(7),CFBXD_NXL),(MAP_FBXD(1,7),MFBXD_NXL)    14
      CHARACTER CFBXD_NXL*40/' '/                                         14
      CHARACTER NFBXD_NXL*12/'R.NXL'/                                     14
      INTEGER MFBXD_NXL(2)/1,1/                                           14
      REAL RE_FBXD_NXL                                                    14
C -                                                                       14
      REAL FBXD_NYL                                                       14
      EQUIVALENCE (BANK_FBXD(10),FBXD_NYL,RE_FBXD_NYL),(NAMES_FBXD(8),    14
     *NFBXD_NYL)                                                          14
      EQUIVALENCE (COMMEN_FBXD(8),CFBXD_NYL),(MAP_FBXD(1,8),MFBXD_NYL)    14
      CHARACTER CFBXD_NYL*40/' '/                                         14
      CHARACTER NFBXD_NYL*12/'R.NYL'/                                     14
      INTEGER MFBXD_NYL(2)/1,1/                                           14
      REAL RE_FBXD_NYL                                                    14
C -                                                                       14
      REAL FBXD_XOFFSET                                                   14
      EQUIVALENCE (BANK_FBXD(11),FBXD_XOFFSET,RE_FBXD_XOFFSET),           14
     *(NAMES_FBXD(9),NFBXD_XOFFSET)                                       14
      EQUIVALENCE (COMMEN_FBXD(9),CFBXD_XOFFSET),(MAP_FBXD(1,9),          14
     *MFBXD_XOFFSET)                                                      14
      CHARACTER CFBXD_XOFFSET*40/' '/                                     14
      CHARACTER NFBXD_XOFFSET*12/'R.XOFFSET'/                             14
      INTEGER MFBXD_XOFFSET(2)/1,1/                                       14
      REAL RE_FBXD_XOFFSET                                                14
C -                                                                       14
      REAL FBXD_ZOFFSET                                                   14
      EQUIVALENCE (BANK_FBXD(12),FBXD_ZOFFSET,RE_FBXD_ZOFFSET),           14
     *(NAMES_FBXD(10),NFBXD_ZOFFSET)                                      14
      EQUIVALENCE (COMMEN_FBXD(10),CFBXD_ZOFFSET),(MAP_FBXD(1,10),        14
     *MFBXD_ZOFFSET)                                                      14
      CHARACTER CFBXD_ZOFFSET*40/' '/                                     14
      CHARACTER NFBXD_ZOFFSET*12/'R.ZOFFSET'/                             14
      INTEGER MFBXD_ZOFFSET(2)/1,1/                                       14
      REAL RE_FBXD_ZOFFSET                                                14
C -                                                                       14
      REAL FBXD_PSOFFSET                                                  14
      EQUIVALENCE (BANK_FBXD(13),FBXD_PSOFFSET,RE_FBXD_PSOFFSET),         14
     *(NAMES_FBXD(11),NFBXD_PSOFFSET)                                     14
      EQUIVALENCE (COMMEN_FBXD(11),CFBXD_PSOFFSET),(MAP_FBXD(1,11),       14
     *MFBXD_PSOFFSET)                                                     14
      CHARACTER CFBXD_PSOFFSET*40/' '/                                    14
      CHARACTER NFBXD_PSOFFSET*12/'R.PSOFFSET'/                           14
      INTEGER MFBXD_PSOFFSET(2)/1,1/                                      14
      REAL RE_FBXD_PSOFFSET                                               14
C -                                                                       14
      REAL FBXD_SMDOFF                                                    14
      EQUIVALENCE (BANK_FBXD(14),FBXD_SMDOFF,RE_FBXD_SMDOFF),             14
     *(NAMES_FBXD(12),NFBXD_SMDOFF)                                       14
      EQUIVALENCE (COMMEN_FBXD(12),CFBXD_SMDOFF),(MAP_FBXD(1,12),         14
     *MFBXD_SMDOFF)                                                       14
      CHARACTER CFBXD_SMDOFF*40/' '/                                      14
      CHARACTER NFBXD_SMDOFF*12/'R.SMDOFF'/                               14
      INTEGER MFBXD_SMDOFF(2)/1,1/                                        14
      REAL RE_FBXD_SMDOFF                                                 14
C -                                                                       14
      INTEGER LEN_FBXD,LLEN_FBXD,LINK_FBXD                                14
      PARAMETER (LEN_FBXD=14)                                             14
      PARAMETER (LLEN_FBXD=12)                                            14
      INTEGER FLAG_FBXD/0/,MAP_FBXD(2,LLEN_FBXD),IDNUM_FBXD(10)           14
      CHARACTER NAMES_FBXD(LLEN_FBXD)*12,COMMEN_FBXD(LLEN_FBXD)*40        14
      DATA IDNUM_FBXD/10*0/                                               14
      REAL BANK_FBXD(LEN_FBXD)                                            14
      CHARACTER*60 BPATH_FBXD/' '/                                        14
      SAVE FLAG_FBXD,MAP_FBXD,IDNUM_FBXD,NAMES_FBXD,COMMEN_FBXD,          14
     *BPATH_FBXD                                                          14
      DATA LINK_FBXD/0/,BANK_FBXD/LEN_FBXD*0/                             14
C *                                                                       14
C -                                                                       15
      REAL FLGG_TYPE                                                      15
      EQUIVALENCE (BANK_FLGG(3),FLGG_TYPE,RE_FLGG_TYPE),(NAMES_FLGG(1),   15
     *NFLGG_TYPE)                                                         15
      EQUIVALENCE (COMMEN_FLGG(1),CFLGG_TYPE),(MAP_FLGG(1,1),             15
     *MFLGG_TYPE)                                                         15
      CHARACTER CFLGG_TYPE*40/' '/                                        15
      CHARACTER NFLGG_TYPE*12/'R.TYPE'/                                   15
      INTEGER MFLGG_TYPE(2)/1,1/                                          15
      REAL RE_FLGG_TYPE                                                   15
C -                                                                       15
      REAL FLGG_WIDTH                                                     15
      EQUIVALENCE (BANK_FLGG(4),FLGG_WIDTH,RE_FLGG_WIDTH),                15
     *(NAMES_FLGG(2),NFLGG_WIDTH)                                         15
      EQUIVALENCE (COMMEN_FLGG(2),CFLGG_WIDTH),(MAP_FLGG(1,2),            15
     *MFLGG_WIDTH)                                                        15
      CHARACTER CFLGG_WIDTH*40/' '/                                       15
      CHARACTER NFLGG_WIDTH*12/'R.WIDTH'/                                 15
      INTEGER MFLGG_WIDTH(2)/1,1/                                         15
      REAL RE_FLGG_WIDTH                                                  15
C -                                                                       15
      REAL FLGG_DEPTH                                                     15
      EQUIVALENCE (BANK_FLGG(5),FLGG_DEPTH,RE_FLGG_DEPTH),                15
     *(NAMES_FLGG(3),NFLGG_DEPTH)                                         15
      EQUIVALENCE (COMMEN_FLGG(3),CFLGG_DEPTH),(MAP_FLGG(1,3),            15
     *MFLGG_DEPTH)                                                        15
      CHARACTER CFLGG_DEPTH*40/' '/                                       15
      CHARACTER NFLGG_DEPTH*12/'R.DEPTH'/                                 15
      INTEGER MFLGG_DEPTH(2)/1,1/                                         15
      REAL RE_FLGG_DEPTH                                                  15
C -                                                                       15
      REAL FLGG_DGAP                                                      15
      EQUIVALENCE (BANK_FLGG(6),FLGG_DGAP,RE_FLGG_DGAP),(NAMES_FLGG(4),   15
     *NFLGG_DGAP)                                                         15
      EQUIVALENCE (COMMEN_FLGG(4),CFLGG_DGAP),(MAP_FLGG(1,4),             15
     *MFLGG_DGAP)                                                         15
      CHARACTER CFLGG_DGAP*40/' '/                                        15
      CHARACTER NFLGG_DGAP*12/'R.DGAP'/                                   15
      INTEGER MFLGG_DGAP(2)/1,1/                                          15
      REAL RE_FLGG_DGAP                                                   15
C -                                                                       15
      REAL FLGG_ALTHICK                                                   15
      EQUIVALENCE (BANK_FLGG(7),FLGG_ALTHICK,RE_FLGG_ALTHICK),            15
     *(NAMES_FLGG(5),NFLGG_ALTHICK)                                       15
      EQUIVALENCE (COMMEN_FLGG(5),CFLGG_ALTHICK),(MAP_FLGG(1,5),          15
     *MFLGG_ALTHICK)                                                      15
      CHARACTER CFLGG_ALTHICK*40/' '/                                     15
      CHARACTER NFLGG_ALTHICK*12/'R.ALTHICK'/                             15
      INTEGER MFLGG_ALTHICK(2)/1,1/                                       15
      REAL RE_FLGG_ALTHICK                                                15
C -                                                                       15
      REAL FLGG_PHCATHDZ                                                  15
      EQUIVALENCE (BANK_FLGG(8),FLGG_PHCATHDZ,RE_FLGG_PHCATHDZ),          15
     *(NAMES_FLGG(6),NFLGG_PHCATHDZ)                                      15
      EQUIVALENCE (COMMEN_FLGG(6),CFLGG_PHCATHDZ),(MAP_FLGG(1,6),         15
     *MFLGG_PHCATHDZ)                                                     15
      CHARACTER CFLGG_PHCATHDZ*40/' '/                                    15
      CHARACTER NFLGG_PHCATHDZ*12/'R.PHCATHDZ'/                           15
      INTEGER MFLGG_PHCATHDZ(2)/1,1/                                      15
      REAL RE_FLGG_PHCATHDZ                                               15
C -                                                                       15
      REAL FLGG_PHCATHR                                                   15
      EQUIVALENCE (BANK_FLGG(9),FLGG_PHCATHR,RE_FLGG_PHCATHR),            15
     *(NAMES_FLGG(7),NFLGG_PHCATHR)                                       15
      EQUIVALENCE (COMMEN_FLGG(7),CFLGG_PHCATHR),(MAP_FLGG(1,7),          15
     *MFLGG_PHCATHR)                                                      15
      CHARACTER CFLGG_PHCATHR*40/' '/                                     15
      CHARACTER NFLGG_PHCATHR*12/'R.PHCATHR'/                             15
      INTEGER MFLGG_PHCATHR(2)/1,1/                                       15
      REAL RE_FLGG_PHCATHR                                                15
C -                                                                       15
      REAL FLGG_MUMETDZ                                                   15
      EQUIVALENCE (BANK_FLGG(10),FLGG_MUMETDZ,RE_FLGG_MUMETDZ),           15
     *(NAMES_FLGG(8),NFLGG_MUMETDZ)                                       15
      EQUIVALENCE (COMMEN_FLGG(8),CFLGG_MUMETDZ),(MAP_FLGG(1,8),          15
     *MFLGG_MUMETDZ)                                                      15
      CHARACTER CFLGG_MUMETDZ*40/' '/                                     15
      CHARACTER NFLGG_MUMETDZ*12/'R.MUMETDZ'/                             15
      INTEGER MFLGG_MUMETDZ(2)/1,1/                                       15
      REAL RE_FLGG_MUMETDZ                                                15
C -                                                                       15
      REAL FLGG_MUMETR                                                    15
      EQUIVALENCE (BANK_FLGG(11),FLGG_MUMETR,RE_FLGG_MUMETR),             15
     *(NAMES_FLGG(9),NFLGG_MUMETR)                                        15
      EQUIVALENCE (COMMEN_FLGG(9),CFLGG_MUMETR),(MAP_FLGG(1,9),           15
     *MFLGG_MUMETR)                                                       15
      CHARACTER CFLGG_MUMETR*40/' '/                                      15
      CHARACTER NFLGG_MUMETR*12/'R.MUMETR'/                               15
      INTEGER MFLGG_MUMETR(2)/1,1/                                        15
      REAL RE_FLGG_MUMETR                                                 15
C -                                                                       15
      INTEGER LEN_FLGG,LLEN_FLGG,LINK_FLGG                                15
      PARAMETER (LEN_FLGG=11)                                             15
      PARAMETER (LLEN_FLGG=9)                                             15
      INTEGER FLAG_FLGG/0/,MAP_FLGG(2,LLEN_FLGG),IDNUM_FLGG(10)           15
      CHARACTER NAMES_FLGG(LLEN_FLGG)*12,COMMEN_FLGG(LLEN_FLGG)*40        15
      DATA IDNUM_FLGG/10*0/                                               15
      REAL BANK_FLGG(LEN_FLGG)                                            15
      CHARACTER*60 BPATH_FLGG/' '/                                        15
      SAVE FLAG_FLGG,MAP_FLGG,IDNUM_FLGG,NAMES_FLGG,COMMEN_FLGG,          15
     *BPATH_FLGG                                                          15
      DATA LINK_FLGG/0/,BANK_FLGG/LEN_FLGG*0/                             15
C *                                                                       15
C -                                                                       16
      REAL FLGM_TYPE                                                      16
      EQUIVALENCE (BANK_FLGM(3),FLGM_TYPE,RE_FLGM_TYPE),(NAMES_FLGM(1),   16
     *NFLGM_TYPE)                                                         16
      EQUIVALENCE (COMMEN_FLGM(1),CFLGM_TYPE),(MAP_FLGM(1,1),             16
     *MFLGM_TYPE)                                                         16
      CHARACTER CFLGM_TYPE*40/' '/                                        16
      CHARACTER NFLGM_TYPE*12/'R.TYPE'/                                   16
      INTEGER MFLGM_TYPE(2)/1,1/                                          16
      REAL RE_FLGM_TYPE                                                   16
C -                                                                       16
      REAL FLGM_DENSITY                                                   16
      EQUIVALENCE (BANK_FLGM(4),FLGM_DENSITY,RE_FLGM_DENSITY),            16
     *(NAMES_FLGM(2),NFLGM_DENSITY)                                       16
      EQUIVALENCE (COMMEN_FLGM(2),CFLGM_DENSITY),(MAP_FLGM(1,2),          16
     *MFLGM_DENSITY)                                                      16
      CHARACTER CFLGM_DENSITY*40/' '/                                     16
      CHARACTER NFLGM_DENSITY*12/'R.DENSITY'/                             16
      INTEGER MFLGM_DENSITY(2)/1,1/                                       16
      REAL RE_FLGM_DENSITY                                                16
C -                                                                       16
      REAL FLGM_RADLEN                                                    16
      EQUIVALENCE (BANK_FLGM(5),FLGM_RADLEN,RE_FLGM_RADLEN),              16
     *(NAMES_FLGM(3),NFLGM_RADLEN)                                        16
      EQUIVALENCE (COMMEN_FLGM(3),CFLGM_RADLEN),(MAP_FLGM(1,3),           16
     *MFLGM_RADLEN)                                                       16
      CHARACTER CFLGM_RADLEN*40/' '/                                      16
      CHARACTER NFLGM_RADLEN*12/'R.RADLEN'/                               16
      INTEGER MFLGM_RADLEN(2)/1,1/                                        16
      REAL RE_FLGM_RADLEN                                                 16
C -                                                                       16
      REAL FLGM_PBCONT                                                    16
      EQUIVALENCE (BANK_FLGM(6),FLGM_PBCONT,RE_FLGM_PBCONT),              16
     *(NAMES_FLGM(4),NFLGM_PBCONT)                                        16
      EQUIVALENCE (COMMEN_FLGM(4),CFLGM_PBCONT),(MAP_FLGM(1,4),           16
     *MFLGM_PBCONT)                                                       16
      CHARACTER CFLGM_PBCONT*40/' '/                                      16
      CHARACTER NFLGM_PBCONT*12/'R.PBCONT'/                               16
      INTEGER MFLGM_PBCONT(2)/1,1/                                        16
      REAL RE_FLGM_PBCONT                                                 16
C -                                                                       16
      REAL FLGM_CRITENE                                                   16
      EQUIVALENCE (BANK_FLGM(7),FLGM_CRITENE,RE_FLGM_CRITENE),            16
     *(NAMES_FLGM(5),NFLGM_CRITENE)                                       16
      EQUIVALENCE (COMMEN_FLGM(5),CFLGM_CRITENE),(MAP_FLGM(1,5),          16
     *MFLGM_CRITENE)                                                      16
      CHARACTER CFLGM_CRITENE*40/' '/                                     16
      CHARACTER NFLGM_CRITENE*12/'R.CRITENE'/                             16
      INTEGER MFLGM_CRITENE(2)/1,1/                                       16
      REAL RE_FLGM_CRITENE                                                16
C -                                                                       16
      REAL FLGM_MOLIERER                                                  16
      EQUIVALENCE (BANK_FLGM(8),FLGM_MOLIERER,RE_FLGM_MOLIERER),          16
     *(NAMES_FLGM(6),NFLGM_MOLIERER)                                      16
      EQUIVALENCE (COMMEN_FLGM(6),CFLGM_MOLIERER),(MAP_FLGM(1,6),         16
     *MFLGM_MOLIERER)                                                     16
      CHARACTER CFLGM_MOLIERER*40/' '/                                    16
      CHARACTER NFLGM_MOLIERER*12/'R.MOLIERER'/                           16
      INTEGER MFLGM_MOLIERER(2)/1,1/                                      16
      REAL RE_FLGM_MOLIERER                                               16
C -                                                                       16
      INTEGER LEN_FLGM,LLEN_FLGM,LINK_FLGM                                16
      PARAMETER (LEN_FLGM=8)                                              16
      PARAMETER (LLEN_FLGM=6)                                             16
      INTEGER FLAG_FLGM/0/,MAP_FLGM(2,LLEN_FLGM),IDNUM_FLGM(10)           16
      CHARACTER NAMES_FLGM(LLEN_FLGM)*12,COMMEN_FLGM(LLEN_FLGM)*40        16
      DATA IDNUM_FLGM/10*0/                                               16
      REAL BANK_FLGM(LEN_FLGM)                                            16
      CHARACTER*60 BPATH_FLGM/' '/                                        16
      SAVE FLAG_FLGM,MAP_FLGM,IDNUM_FLGM,NAMES_FLGM,COMMEN_FLGM,          16
     *BPATH_FLGM                                                          16
      DATA LINK_FLGM/0/,BANK_FLGM/LEN_FLGM*0/                             16
C *                                                                       16
C -                                                                       17
      REAL PBPD_Z                                                         17
      EQUIVALENCE (BANK_PBPD(3),PBPD_Z,RE_PBPD_Z),(NAMES_PBPD(1),         17
     *NPBPD_Z)                                                            17
      EQUIVALENCE (COMMEN_PBPD(1),CPBPD_Z),(MAP_PBPD(1,1),MPBPD_Z)        17
      CHARACTER CPBPD_Z*40/' '/                                           17
      CHARACTER NPBPD_Z*12/'R.Z'/                                         17
      INTEGER MPBPD_Z(2)/1,1/                                             17
      REAL RE_PBPD_Z                                                      17
C -                                                                       17
      REAL PBPD_WIDTH                                                     17
      EQUIVALENCE (BANK_PBPD(4),PBPD_WIDTH,RE_PBPD_WIDTH),                17
     *(NAMES_PBPD(2),NPBPD_WIDTH)                                         17
      EQUIVALENCE (COMMEN_PBPD(2),CPBPD_WIDTH),(MAP_PBPD(1,2),            17
     *MPBPD_WIDTH)                                                        17
      CHARACTER CPBPD_WIDTH*40/' '/                                       17
      CHARACTER NPBPD_WIDTH*12/'R.WIDTH'/                                 17
      INTEGER MPBPD_WIDTH(2)/1,1/                                         17
      REAL RE_PBPD_WIDTH                                                  17
C -                                                                       17
      REAL PBPD_HEIGHT                                                    17
      EQUIVALENCE (BANK_PBPD(5),PBPD_HEIGHT,RE_PBPD_HEIGHT),              17
     *(NAMES_PBPD(3),NPBPD_HEIGHT)                                        17
      EQUIVALENCE (COMMEN_PBPD(3),CPBPD_HEIGHT),(MAP_PBPD(1,3),           17
     *MPBPD_HEIGHT)                                                       17
      CHARACTER CPBPD_HEIGHT*40/' '/                                      17
      CHARACTER NPBPD_HEIGHT*12/'R.HEIGHT'/                               17
      INTEGER MPBPD_HEIGHT(2)/1,1/                                        17
      REAL RE_PBPD_HEIGHT                                                 17
C -                                                                       17
      REAL PBPD_THICK                                                     17
      EQUIVALENCE (BANK_PBPD(6),PBPD_THICK,RE_PBPD_THICK),                17
     *(NAMES_PBPD(4),NPBPD_THICK)                                         17
      EQUIVALENCE (COMMEN_PBPD(4),CPBPD_THICK),(MAP_PBPD(1,4),            17
     *MPBPD_THICK)                                                        17
      CHARACTER CPBPD_THICK*40/' '/                                       17
      CHARACTER NPBPD_THICK*12/'R.THICK'/                                 17
      INTEGER MPBPD_THICK(2)/1,1/                                         17
      REAL RE_PBPD_THICK                                                  17
C -                                                                       17
      INTEGER LEN_PBPD,LLEN_PBPD,LINK_PBPD                                17
      PARAMETER (LEN_PBPD=6)                                              17
      PARAMETER (LLEN_PBPD=4)                                             17
      INTEGER FLAG_PBPD/0/,MAP_PBPD(2,LLEN_PBPD),IDNUM_PBPD(10)           17
      CHARACTER NAMES_PBPD(LLEN_PBPD)*12,COMMEN_PBPD(LLEN_PBPD)*40        17
      DATA IDNUM_PBPD/10*0/                                               17
      REAL BANK_PBPD(LEN_PBPD)                                            17
      CHARACTER*60 BPATH_PBPD/' '/                                        17
      SAVE FLAG_PBPD,MAP_PBPD,IDNUM_PBPD,NAMES_PBPD,COMMEN_PBPD,          17
     *BPATH_PBPD                                                          17
      DATA LINK_PBPD/0/,BANK_PBPD/LEN_PBPD*0/                             17
C *                                                                       17
C -                                                                       18
      REAL FMXG_VERSION                                                   18
      EQUIVALENCE (BANK_FMXG(3),FMXG_VERSION,RE_FMXG_VERSION),            18
     *(NAMES_FMXG(1),NFMXG_VERSION)                                       18
      EQUIVALENCE (COMMEN_FMXG(1),CFMXG_VERSION),(MAP_FMXG(1,1),          18
     *MFMXG_VERSION)                                                      18
      CHARACTER CFMXG_VERSION*40/' '/                                     18
      CHARACTER NFMXG_VERSION*12/'R.VERSION'/                             18
      INTEGER MFMXG_VERSION(2)/1,1/                                       18
      REAL RE_FMXG_VERSION                                                18
C -                                                                       18
      REAL FMXG_SAPEX                                                     18
      EQUIVALENCE (BANK_FMXG(4),FMXG_SAPEX,RE_FMXG_SAPEX),                18
     *(NAMES_FMXG(2),NFMXG_SAPEX)                                         18
      EQUIVALENCE (COMMEN_FMXG(2),CFMXG_SAPEX),(MAP_FMXG(1,2),            18
     *MFMXG_SAPEX)                                                        18
      CHARACTER CFMXG_SAPEX*40/' '/                                       18
      CHARACTER NFMXG_SAPEX*12/'R.SAPEX'/                                 18
      INTEGER MFMXG_SAPEX(2)/1,1/                                         18
      REAL RE_FMXG_SAPEX                                                  18
C -                                                                       18
      REAL FMXG_SBASE                                                     18
      EQUIVALENCE (BANK_FMXG(5),FMXG_SBASE,RE_FMXG_SBASE),                18
     *(NAMES_FMXG(3),NFMXG_SBASE)                                         18
      EQUIVALENCE (COMMEN_FMXG(3),CFMXG_SBASE),(MAP_FMXG(1,3),            18
     *MFMXG_SBASE)                                                        18
      CHARACTER CFMXG_SBASE*40/' '/                                       18
      CHARACTER NFMXG_SBASE*12/'R.SBASE'/                                 18
      INTEGER MFMXG_SBASE(2)/1,1/                                         18
      REAL RE_FMXG_SBASE                                                  18
C -                                                                       18
      REAL FMXG_SGAP                                                      18
      EQUIVALENCE (BANK_FMXG(6),FMXG_SGAP,RE_FMXG_SGAP),(NAMES_FMXG(4),   18
     *NFMXG_SGAP)                                                         18
      EQUIVALENCE (COMMEN_FMXG(4),CFMXG_SGAP),(MAP_FMXG(1,4),             18
     *MFMXG_SGAP)                                                         18
      CHARACTER CFMXG_SGAP*40/' '/                                        18
      CHARACTER NFMXG_SGAP*12/'R.SGAP'/                                   18
      INTEGER MFMXG_SGAP(2)/1,1/                                          18
      REAL RE_FMXG_SGAP                                                   18
C -                                                                       18
      REAL FMXG_NSTRIP                                                    18
      EQUIVALENCE (BANK_FMXG(7),FMXG_NSTRIP,RE_FMXG_NSTRIP),              18
     *(NAMES_FMXG(5),NFMXG_NSTRIP)                                        18
      EQUIVALENCE (COMMEN_FMXG(5),CFMXG_NSTRIP),(MAP_FMXG(1,5),           18
     *MFMXG_NSTRIP)                                                       18
      CHARACTER CFMXG_NSTRIP*40/' '/                                      18
      CHARACTER NFMXG_NSTRIP*12/'R.NSTRIP'/                               18
      INTEGER MFMXG_NSTRIP(2)/1,1/                                        18
      REAL RE_FMXG_NSTRIP                                                 18
C -                                                                       18
      REAL FMXG_G10WIDTH                                                  18
      EQUIVALENCE (BANK_FMXG(8),FMXG_G10WIDTH,RE_FMXG_G10WIDTH),          18
     *(NAMES_FMXG(6),NFMXG_G10WIDTH)                                      18
      EQUIVALENCE (COMMEN_FMXG(6),CFMXG_G10WIDTH),(MAP_FMXG(1,6),         18
     *MFMXG_G10WIDTH)                                                     18
      CHARACTER CFMXG_G10WIDTH*40/' '/                                    18
      CHARACTER NFMXG_G10WIDTH*12/'R.G10WIDTH'/                           18
      INTEGER MFMXG_G10WIDTH(2)/1,1/                                      18
      REAL RE_FMXG_G10WIDTH                                               18
C -                                                                       18
      REAL FMXG_G10HGT                                                    18
      EQUIVALENCE (BANK_FMXG(9),FMXG_G10HGT,RE_FMXG_G10HGT),              18
     *(NAMES_FMXG(7),NFMXG_G10HGT)                                        18
      EQUIVALENCE (COMMEN_FMXG(7),CFMXG_G10HGT),(MAP_FMXG(1,7),           18
     *MFMXG_G10HGT)                                                       18
      CHARACTER CFMXG_G10HGT*40/' '/                                      18
      CHARACTER NFMXG_G10HGT*12/'R.G10HGT'/                               18
      INTEGER MFMXG_G10HGT(2)/1,1/                                        18
      REAL RE_FMXG_G10HGT                                                 18
C -                                                                       18
      REAL FMXG_G10THICK                                                  18
      EQUIVALENCE (BANK_FMXG(10),FMXG_G10THICK,RE_FMXG_G10THICK),         18
     *(NAMES_FMXG(8),NFMXG_G10THICK)                                      18
      EQUIVALENCE (COMMEN_FMXG(8),CFMXG_G10THICK),(MAP_FMXG(1,8),         18
     *MFMXG_G10THICK)                                                     18
      CHARACTER CFMXG_G10THICK*40/' '/                                    18
      CHARACTER NFMXG_G10THICK*12/'R.G10THICK'/                           18
      INTEGER MFMXG_G10THICK(2)/1,1/                                      18
      REAL RE_FMXG_G10THICK                                               18
C -                                                                       18
      INTEGER LEN_FMXG,LLEN_FMXG,LINK_FMXG                                18
      PARAMETER (LEN_FMXG=10)                                             18
      PARAMETER (LLEN_FMXG=8)                                             18
      INTEGER FLAG_FMXG/0/,MAP_FMXG(2,LLEN_FMXG),IDNUM_FMXG(10)           18
      CHARACTER NAMES_FMXG(LLEN_FMXG)*12,COMMEN_FMXG(LLEN_FMXG)*40        18
      DATA IDNUM_FMXG/10*0/                                               18
      REAL BANK_FMXG(LEN_FMXG)                                            18
      CHARACTER*60 BPATH_FMXG/' '/                                        18
      SAVE FLAG_FMXG,MAP_FMXG,IDNUM_FMXG,NAMES_FMXG,COMMEN_FMXG,          18
     *BPATH_FMXG                                                          18
      DATA LINK_FMXG/0/,BANK_FMXG/LEN_FMXG*0/                             18
C *                                                                       18
C -                                                                       20
      REAL INSE_WIDTH                                                     20
      EQUIVALENCE (BANK_INSE(3),INSE_WIDTH,RE_INSE_WIDTH),                20
     *(NAMES_INSE(1),NINSE_WIDTH)                                         20
      EQUIVALENCE (COMMEN_INSE(1),CINSE_WIDTH),(MAP_INSE(1,1),            20
     *MINSE_WIDTH)                                                        20
      CHARACTER CINSE_WIDTH*40/' '/                                       20
      CHARACTER NINSE_WIDTH*12/'R.WIDTH'/                                 20
      INTEGER MINSE_WIDTH(2)/1,1/                                         20
      REAL RE_INSE_WIDTH                                                  20
C -                                                                       20
      REAL INSE_DEPTH                                                     20
      EQUIVALENCE (BANK_INSE(4),INSE_DEPTH,RE_INSE_DEPTH),                20
     *(NAMES_INSE(2),NINSE_DEPTH)                                         20
      EQUIVALENCE (COMMEN_INSE(2),CINSE_DEPTH),(MAP_INSE(1,2),            20
     *MINSE_DEPTH)                                                        20
      CHARACTER CINSE_DEPTH*40/' '/                                       20
      CHARACTER NINSE_DEPTH*12/'R.DEPTH'/                                 20
      INTEGER MINSE_DEPTH(2)/1,1/                                         20
      REAL RE_INSE_DEPTH                                                  20
C -                                                                       20
      REAL INSE_HEIGHT                                                    20
      EQUIVALENCE (BANK_INSE(5),INSE_HEIGHT,RE_INSE_HEIGHT),              20
     *(NAMES_INSE(3),NINSE_HEIGHT)                                        20
      EQUIVALENCE (COMMEN_INSE(3),CINSE_HEIGHT),(MAP_INSE(1,3),           20
     *MINSE_HEIGHT)                                                       20
      CHARACTER CINSE_HEIGHT*40/' '/                                      20
      CHARACTER NINSE_HEIGHT*12/'R.HEIGHT'/                               20
      INTEGER MINSE_HEIGHT(2)/1,1/                                        20
      REAL RE_INSE_HEIGHT                                                 20
C -                                                                       20
      REAL INSE_SHEETDPT                                                  20
      EQUIVALENCE (BANK_INSE(6),INSE_SHEETDPT,RE_INSE_SHEETDPT),          20
     *(NAMES_INSE(4),NINSE_SHEETDPT)                                      20
      EQUIVALENCE (COMMEN_INSE(4),CINSE_SHEETDPT),(MAP_INSE(1,4),         20
     *MINSE_SHEETDPT)                                                     20
      CHARACTER CINSE_SHEETDPT*40/' '/                                    20
      CHARACTER NINSE_SHEETDPT*12/'R.SHEETDPT'/                           20
      INTEGER MINSE_SHEETDPT(2)/1,1/                                      20
      REAL RE_INSE_SHEETDPT                                               20
C -                                                                       20
      REAL INSE_HOLEGAP                                                   20
      EQUIVALENCE (BANK_INSE(7),INSE_HOLEGAP,RE_INSE_HOLEGAP),            20
     *(NAMES_INSE(5),NINSE_HOLEGAP)                                       20
      EQUIVALENCE (COMMEN_INSE(5),CINSE_HOLEGAP),(MAP_INSE(1,5),          20
     *MINSE_HOLEGAP)                                                      20
      CHARACTER CINSE_HOLEGAP*40/' '/                                     20
      CHARACTER NINSE_HOLEGAP*12/'R.HOLEGAP'/                             20
      INTEGER MINSE_HOLEGAP(2)/1,1/                                       20
      REAL RE_INSE_HOLEGAP                                                20
C -                                                                       20
      REAL INSE_HOLEDEPTH                                                 20
      EQUIVALENCE (BANK_INSE(8),INSE_HOLEDEPTH,RE_INSE_HOLEDEPTH),        20
     *(NAMES_INSE(6),NINSE_HOLEDEPTH)                                     20
      EQUIVALENCE (COMMEN_INSE(6),CINSE_HOLEDEPTH),(MAP_INSE(1,6),        20
     *MINSE_HOLEDEPTH)                                                    20
      CHARACTER CINSE_HOLEDEPTH*40/' '/                                   20
      CHARACTER NINSE_HOLEDEPTH*12/'R.HOLEDEPTH'/                         20
      INTEGER MINSE_HOLEDEPTH(2)/1,1/                                     20
      REAL RE_INSE_HOLEDEPTH                                              20
C -                                                                       20
      REAL INSE_HOLEHEIGHT                                                20
      EQUIVALENCE (BANK_INSE(9),INSE_HOLEHEIGHT,RE_INSE_HOLEHEIGHT),      20
     *(NAMES_INSE(7),NINSE_HOLEHEIGHT)                                    20
      EQUIVALENCE (COMMEN_INSE(7),CINSE_HOLEHEIGHT),(MAP_INSE(1,7),       20
     *MINSE_HOLEHEIGHT)                                                   20
      CHARACTER CINSE_HOLEHEIGHT*40/' '/                                  20
      CHARACTER NINSE_HOLEHEIGHT*12/'R.HOLEHEIGHT'/                       20
      INTEGER MINSE_HOLEHEIGHT(2)/1,1/                                    20
      REAL RE_INSE_HOLEHEIGHT                                             20
C -                                                                       20
      REAL INSE_GAPDEPTH                                                  20
      EQUIVALENCE (BANK_INSE(10),INSE_GAPDEPTH,RE_INSE_GAPDEPTH),         20
     *(NAMES_INSE(8),NINSE_GAPDEPTH)                                      20
      EQUIVALENCE (COMMEN_INSE(8),CINSE_GAPDEPTH),(MAP_INSE(1,8),         20
     *MINSE_GAPDEPTH)                                                     20
      CHARACTER CINSE_GAPDEPTH*40/' '/                                    20
      CHARACTER NINSE_GAPDEPTH*12/'R.GAPDEPTH'/                           20
      INTEGER MINSE_GAPDEPTH(2)/1,1/                                      20
      REAL RE_INSE_GAPDEPTH                                               20
C -                                                                       20
      REAL INSE_GAPHEIGHT                                                 20
      EQUIVALENCE (BANK_INSE(11),INSE_GAPHEIGHT,RE_INSE_GAPHEIGHT),       20
     *(NAMES_INSE(9),NINSE_GAPHEIGHT)                                     20
      EQUIVALENCE (COMMEN_INSE(9),CINSE_GAPHEIGHT),(MAP_INSE(1,9),        20
     *MINSE_GAPHEIGHT)                                                    20
      CHARACTER CINSE_GAPHEIGHT*40/' '/                                   20
      CHARACTER NINSE_GAPHEIGHT*12/'R.GAPHEIGHT'/                         20
      INTEGER MINSE_GAPHEIGHT(2)/1,1/                                     20
      REAL RE_INSE_GAPHEIGHT                                              20
C -                                                                       20
      REAL INSE_GATEDEPTH                                                 20
      EQUIVALENCE (BANK_INSE(12),INSE_GATEDEPTH,RE_INSE_GATEDEPTH),       20
     *(NAMES_INSE(10),NINSE_GATEDEPTH)                                    20
      EQUIVALENCE (COMMEN_INSE(10),CINSE_GATEDEPTH),(MAP_INSE(1,10),      20
     *MINSE_GATEDEPTH)                                                    20
      CHARACTER CINSE_GATEDEPTH*40/' '/                                   20
      CHARACTER NINSE_GATEDEPTH*12/'R.GATEDEPTH'/                         20
      INTEGER MINSE_GATEDEPTH(2)/1,1/                                     20
      REAL RE_INSE_GATEDEPTH                                              20
C -                                                                       20
      REAL INSE_RA                                                        20
      EQUIVALENCE (BANK_INSE(13),INSE_RA,RE_INSE_RA),(NAMES_INSE(11),     20
     *NINSE_RA)                                                           20
      EQUIVALENCE (COMMEN_INSE(11),CINSE_RA),(MAP_INSE(1,11),MINSE_RA)    20
      CHARACTER CINSE_RA*40/' '/                                          20
      CHARACTER NINSE_RA*12/'R.RA'/                                       20
      INTEGER MINSE_RA(2)/1,1/                                            20
      REAL RE_INSE_RA                                                     20
C -                                                                       20
      REAL INSE_RB                                                        20
      EQUIVALENCE (BANK_INSE(14),INSE_RB,RE_INSE_RB),(NAMES_INSE(12),     20
     *NINSE_RB)                                                           20
      EQUIVALENCE (COMMEN_INSE(12),CINSE_RB),(MAP_INSE(1,12),MINSE_RB)    20
      CHARACTER CINSE_RB*40/' '/                                          20
      CHARACTER NINSE_RB*12/'R.RB'/                                       20
      INTEGER MINSE_RB(2)/1,1/                                            20
      REAL RE_INSE_RB                                                     20
C -                                                                       20
      REAL INSE_DIAM                                                      20
      EQUIVALENCE (BANK_INSE(15),INSE_DIAM,RE_INSE_DIAM),                 20
     *(NAMES_INSE(13),NINSE_DIAM)                                         20
      EQUIVALENCE (COMMEN_INSE(13),CINSE_DIAM),(MAP_INSE(1,13),           20
     *MINSE_DIAM)                                                         20
      CHARACTER CINSE_DIAM*40/' '/                                        20
      CHARACTER NINSE_DIAM*12/'R.DIAM'/                                   20
      INTEGER MINSE_DIAM(2)/1,1/                                          20
      REAL RE_INSE_DIAM                                                   20
C -                                                                       20
      REAL INSE_RMAX                                                      20
      EQUIVALENCE (BANK_INSE(16),INSE_RMAX,RE_INSE_RMAX),                 20
     *(NAMES_INSE(14),NINSE_RMAX)                                         20
      EQUIVALENCE (COMMEN_INSE(14),CINSE_RMAX),(MAP_INSE(1,14),           20
     *MINSE_RMAX)                                                         20
      CHARACTER CINSE_RMAX*40/' '/                                        20
      CHARACTER NINSE_RMAX*12/'R.RMAX'/                                   20
      INTEGER MINSE_RMAX(2)/1,1/                                          20
      REAL RE_INSE_RMAX                                                   20
C -                                                                       20
      REAL INSE_GATEGAP                                                   20
      EQUIVALENCE (BANK_INSE(17),INSE_GATEGAP,RE_INSE_GATEGAP),           20
     *(NAMES_INSE(15),NINSE_GATEGAP)                                      20
      EQUIVALENCE (COMMEN_INSE(15),CINSE_GATEGAP),(MAP_INSE(1,15),        20
     *MINSE_GATEGAP)                                                      20
      CHARACTER CINSE_GATEGAP*40/' '/                                     20
      CHARACTER NINSE_GATEGAP*12/'R.GATEGAP'/                             20
      INTEGER MINSE_GATEGAP(2)/1,1/                                       20
      REAL RE_INSE_GATEGAP                                                20
C -                                                                       20
      INTEGER LEN_INSE,LLEN_INSE,LINK_INSE                                20
      PARAMETER (LEN_INSE=17)                                             20
      PARAMETER (LLEN_INSE=15)                                            20
      INTEGER FLAG_INSE/0/,MAP_INSE(2,LLEN_INSE),IDNUM_INSE(10)           20
      CHARACTER NAMES_INSE(LLEN_INSE)*12,COMMEN_INSE(LLEN_INSE)*40        20
      DATA IDNUM_INSE/10*0/                                               20
      REAL BANK_INSE(LEN_INSE)                                            20
      CHARACTER*60 BPATH_INSE/' '/                                        20
      SAVE FLAG_INSE,MAP_INSE,IDNUM_INSE,NAMES_INSE,COMMEN_INSE,          20
     *BPATH_INSE                                                          20
      DATA LINK_INSE/0/,BANK_INSE/LEN_INSE*0/                             20
C *                                                                       20
      INTEGER CHKVSIM,IMOD,ITYPE,TYPE,PBPLATE                             21
      INTEGER I,J,M,SERN                                                  23
      REAL XX,YY,ZZ,X1,Y1,Z1,ZTOT,RTOT,WID,WIDX,WIDY,BWID,X0,WIDL         25
      REAL ZTOTSMD,WTOTSMD,ZSMD,ZSMD2,WSMD                                27
      REAL XSMDH,YSMDH,ZSMDH,XSMDV,YSMDV,ZSMDV                            29
      REAL XLCOFFSET,BZOFFSET                                             31
      REAL BASEWIDTH,DISTANCER,XOFFFECC,XOFFFEDC,XSHIFT                   33
      REAL XOFFFENC,YOFFFENC,ZOFFFENC,ZOFFFECC                            35
      REAL TMP(7)                                                         37
      INTEGER N                                                           39
      PARAMETER (N=12)                                                    41
      REAL E(N)                                                           42
      REAL RINDEX_PBG(N)                                                  44
      REAL RINDEX_SIRUB(N)                                                46
      REAL RINDEX_PHCATH(N)                                               48
      REAL RINDEX_ALM(N)                                                  50
      REAL RINDEX_MUMET(N)                                                52
      REAL ABSCO_PBG(N)                                                   54
      REAL ABSCO_SIRUB(N)                                                 56
      REAL ABSCO_PHCATH(N)                                                58
      REAL ABSCO_ALM(N)                                                   60
      REAL ABSCO_MUMET(N)                                                 62
      REAL EFFIC_PHCATH(N)                                                64
      REAL EFFIC_ALL(N)                                                   66
      EXTERNAL FFPDSTEP                                                   68
      EXTERNAL FPCTSTEP                                                   69
C *                                                                       70
      CALL AGSBEGM(MODULE,IPRIN)                                          70
      IF (IPRIN.LT.0) RETURN                                              70
C Filling bank FMCG - FPD CALORIMETER BASIC DATA                          70
      BANK_TITLE='FPD CALORIMETER BASIC DATA'                             70
      BPATH_FMCG='FMCG*'                                                  70
      IDNUM_FMCG(1)=0                                                     71
C GEOMETRY VERSION                                                        71
      FMCG_VERSION=8.0                                                    71
      CFMCG_VERSION='GEOMETRY VERSION'                                    71
C = 0 DE, = 1 CHERENKOV SIMULATION FOR PBG                                72
      FMCG_CHKVSIM=0                                                      72
      CFMCG_CHKVSIM='= 0 DE, = 1 CHERENKOV SIMULATION FOR PBG'            72
C =0 NO PLATE, =1 WITH PLATE                                              73
      FMCG_PBPLATE=0                                                      73
      CFMCG_PBPLATE='=0 NO PLATE, =1 WITH PLATE'                          73
C DEFAULT X-POSITION                                                      74
      FMCG_FMSNORTHX=-0.3                                                 74
      CFMCG_FMSNORTHX='DEFAULT X-POSITION'                                74
C DEFAULT X-POSITION                                                      75
      FMCG_FMSSOUTHX=+0.3                                                 75
      CFMCG_FMSSOUTHX='DEFAULT X-POSITION'                                75
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    76
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           76
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   76
C *                                                                       76
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMCG',    76
     *BPATH_FMCG,IDNUM_FMCG,1, MAP_FMCG,NAMES_FMCG,COMMEN_FMCG,           76
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   76
C use FMCG bank                                                           77
      BPATH_FMCG='FMCG*'                                                  77
      IDNUM_FMCG(1)=0                                                     77
      FMCG_VERSION=8.0                                                    77
      CALL AGDGETP('FPDMGEO3','*',-999,'FMCG',NFMCG_VERSION,              77
     *RE_FMCG_VERSION,BPATH_FMCG,IDNUM_FMCG,1,MAP_FMCG,NAMES_FMCG,        77
     *BANK_FMCG,LEN_FMCG,LLEN_FMCG,LINK_FMCG,FLAG_FMCG)                   77
C *                                                                       78
      CALL AGSBEGM(MODULE,IPRIN)                                          78
      IF (IPRIN.LT.0) RETURN                                              78
C Filling bank FPOS - FPD EN POSITIONING                                  78
      BANK_TITLE='FPD EN POSITIONING'                                     78
      BPATH_FPOS='FPOS*'                                                  78
      IDNUM_FPOS(1)=0                                                     79
C MODULE# (EN=1, ES=2, WN=3, WS=4, ...)                                   79
      FPOS_IMOD=1                                                         79
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WN=3, WS=4, ...)'                  79
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               80
      FPOS_ITYPE=1                                                        80
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             80
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                81
      FPOS_X=-48.19                                                       81
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  81
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                82
      FPOS_Y=0.0                                                          82
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  82
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               83
      FPOS_Z=-779.0                                                       83
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 83
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              84
      FPOS_AY=180                                                         84
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               84
C ANGLE AROUND Z                                                          85
      FPOS_AZ=0                                                           85
      CFPOS_AZ='ANGLE AROUND Z'                                           85
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    86
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           86
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   86
C *                                                                       86
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    86
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           86
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   86
C *                                                                       87
      CALL AGSBEGM(MODULE,IPRIN)                                          87
      IF (IPRIN.LT.0) RETURN                                              87
C Filling bank FPOS - FPD ES POSITIONING                                  87
      BANK_TITLE='FPD ES POSITIONING'                                     87
      BPATH_FPOS='FPOS*'                                                  87
      IDNUM_FPOS(1)=0                                                     88
C MODULE# (EN=1, ES=2, WS=3, WS=4, ...)                                   88
      FPOS_IMOD=2                                                         88
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WS=3, WS=4, ...)'                  88
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               89
      FPOS_ITYPE=1                                                        89
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             89
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                90
      FPOS_X=48.19                                                        90
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  90
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                91
      FPOS_Y=0.0                                                          91
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  91
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               92
      FPOS_Z=-779.0                                                       92
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 92
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              93
      FPOS_AY=180                                                         93
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               93
C ANGLE AROUND Z                                                          94
      FPOS_AZ=0                                                           94
      CFPOS_AZ='ANGLE AROUND Z'                                           94
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    95
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           95
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   95
C *                                                                       95
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    95
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           95
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   95
C *                                                                       96
      CALL AGSBEGM(MODULE,IPRIN)                                          96
      IF (IPRIN.LT.0) RETURN                                              96
C Filling bank FPOS - FMS WN POSITIONING                                  96
      BANK_TITLE='FMS WN POSITIONING'                                     96
      BPATH_FPOS='FPOS*'                                                  96
      IDNUM_FPOS(1)=0                                                     97
C MODULE# (EN=1, ES=2, WN=3, WS=4, ...)                                   97
      FPOS_IMOD=3                                                         97
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WN=3, WS=4, ...)'                  97
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               98
      FPOS_ITYPE=2                                                        98
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             98
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                99
      FPOS_X=FMCG_FMSNORTHX                                               99
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  99
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                100
      FPOS_Y=0.0                                                          100
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  100
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               101
      FPOS_Z=706.3                                                        101
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 101
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              102
      FPOS_AY=0                                                           102
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               102
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    103
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           103
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   103
C *                                                                       103
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    103
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           103
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   103
C *                                                                       104
      CALL AGSBEGM(MODULE,IPRIN)                                          104
      IF (IPRIN.LT.0) RETURN                                              104
C Filling bank FPOS - FMS WS POSITIONING                                  104
      BANK_TITLE='FMS WS POSITIONING'                                     104
      BPATH_FPOS='FPOS*'                                                  104
      IDNUM_FPOS(1)=0                                                     105
C MODULE# (EN=1, ES=2, WN=3, WS=4, ...)                                   105
      FPOS_IMOD=4                                                         105
      CFPOS_IMOD='MODULE# (EN=1, ES=2, WN=3, WS=4, ...)'                  105
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               106
      FPOS_ITYPE=2                                                        106
      CFPOS_ITYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'             106
C X DISTANCE FROM BEAM TO EDGE OF DETECTOR                                107
      FPOS_X=FMCG_FMSSOUTHX                                               107
      CFPOS_X='X DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  107
C Y DISTANCE FROM BEAM TO EDGE OF DETECTOR                                108
      FPOS_Y=0.0                                                          108
      CFPOS_Y='Y DISTANCE FROM BEAM TO EDGE OF DETECTOR'                  108
C Z DISTANCE FROM IP TO SURFACE OF DETECTOR                               109
      FPOS_Z=706.3                                                        109
      CFPOS_Z='Z DISTANCE FROM IP TO SURFACE OF DETECTOR'                 109
C ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)                              110
      FPOS_AY=0                                                           110
      CFPOS_AY='ANGLE AROOUND Y (0 FOR WEST, 180 FOR EAST)'               110
C ANGLE AROUND Z                                                          111
      FPOS_AZ=0                                                           111
      CFPOS_AZ='ANGLE AROUND Z'                                           111
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    112
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           112
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   112
C *                                                                       112
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FPOS',    112
     *BPATH_FPOS,IDNUM_FPOS,1, MAP_FPOS,NAMES_FPOS,COMMEN_FPOS,           112
     *BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                   112
C *                                                                       113
      CALL AGSBEGM(MODULE,IPRIN)                                          113
      IF (IPRIN.LT.0) RETURN                                              113
C Filling bank FBXD - FPD BOX GEOMETRY                                    113
      BANK_TITLE='FPD BOX GEOMETRY'                                       113
      BPATH_FBXD='FBXD*'                                                  113
      IDNUM_FBXD(1)=0                                                     114
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               114
      FBXD_TYPE=1                                                         114
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'              114
C BOX HEIGHT                                                              115
      FBXD_HEIGHT=100                                                     115
      CFBXD_HEIGHT='BOX HEIGHT'                                           115
C BOX DEPTH                                                               116
      FBXD_DEPTH=96                                                       116
      CFBXD_DEPTH='BOX DEPTH'                                             116
C BOX WIDTH (ONLY FOR FMS)                                                117
      FBXD_WIDTH=0.0                                                      117
      CFBXD_WIDTH='BOX WIDTH (ONLY FOR FMS)'                              117
C NUMBER OF PBG IN X                                                      118
      FBXD_NX=7                                                           118
      CFBXD_NX='NUMBER OF PBG IN X'                                       118
C NUMBER OF PBG IN Y                                                      119
      FBXD_NY=7                                                           119
      CFBXD_NY='NUMBER OF PBG IN Y'                                       119
C NUMBER OF LARGE PBG IN X                                                120
      FBXD_NXL=0                                                          120
      CFBXD_NXL='NUMBER OF LARGE PBG IN X'                                120
C NUMBER OF LARGE PBG IN Y                                                121
      FBXD_NYL=0                                                          121
      CFBXD_NYL='NUMBER OF LARGE PBG IN Y'                                121
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                122
      FBXD_XOFFSET=2.54                                                   122
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            122
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                123
      FBXD_ZOFFSET=19                                                     123
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            123
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            124
      FBXD_PSOFFSET=2.0                                                   124
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       124
C SMD V-PLANE Z OFFSET FROM BOX EDGE                                      125
      FBXD_SMDOFF=8.0                                                     125
      CFBXD_SMDOFF='SMD V-PLANE Z OFFSET FROM BOX EDGE'                   125
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    126
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           126
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   126
C *                                                                       126
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    126
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           126
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   126
C *                                                                       127
      CALL AGSBEGM(MODULE,IPRIN)                                          127
      IF (IPRIN.LT.0) RETURN                                              127
C Filling bank FBXD - FPD BOX GEOMETRY                                    127
      BANK_TITLE='FPD BOX GEOMETRY'                                       127
      BPATH_FBXD='FBXD*'                                                  127
      IDNUM_FBXD(1)=0                                                     128
C TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)                               128
      FBXD_TYPE=2                                                         128
      CFBXD_TYPE='TYPE (1=7*7+SMD+PRESHOWER, 2=17*34+14*28)'              128
C BOX HEIGHT                                                              129
      FBXD_HEIGHT=210                                                     129
      CFBXD_HEIGHT='BOX HEIGHT'                                           129
C BOX DEPTH                                                               130
      FBXD_DEPTH=98.425                                                   130
      CFBXD_DEPTH='BOX DEPTH'                                             130
C BOX WIDTH (ONLY FOR FMS)                                                131
      FBXD_WIDTH=127.0                                                    131
      CFBXD_WIDTH='BOX WIDTH (ONLY FOR FMS)'                              131
C NUMBER OF PBG IN X                                                      132
      FBXD_NX=12                                                          132
      CFBXD_NX='NUMBER OF PBG IN X'                                       132
C NUMBER OF PBG IN Y                                                      133
      FBXD_NY=24                                                          133
      CFBXD_NY='NUMBER OF PBG IN Y'                                       133
C NUMBER OF LARGE PBG IN X                                                134
      FBXD_NXL=17                                                         134
      CFBXD_NXL='NUMBER OF LARGE PBG IN X'                                134
C NUMBER OF LARGE PBG IN Y                                                135
      FBXD_NYL=34                                                         135
      CFBXD_NYL='NUMBER OF LARGE PBG IN Y'                                135
C TOWER X OFFSET FROM BOX EDGE TO PBG EDGE                                136
      FBXD_XOFFSET=(6*3.822+0.5*5.812)+(127.0-17*5.812)/2.0               136
      CFBXD_XOFFSET='TOWER X OFFSET FROM BOX EDGE TO PBG EDGE'            136
C TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                                137
      FBXD_ZOFFSET=10.4                                                   137
      CFBXD_ZOFFSET='TOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'            137
C PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE                            138
      FBXD_PSOFFSET=0                                                     138
      CFBXD_PSOFFSET='PRESHOWER Z OFFSET FROM BOX EDGE TO PBG EDGE'       138
C SMD Z OFFSET FROM BOX EDGE                                              139
      FBXD_SMDOFF=0.0                                                     139
      CFBXD_SMDOFF='SMD Z OFFSET FROM BOX EDGE'                           139
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    140
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           140
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   140
C *                                                                       140
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FBXD',    140
     *BPATH_FBXD,IDNUM_FBXD,1, MAP_FBXD,NAMES_FBXD,COMMEN_FBXD,           140
     *BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                   140
C *                                                                       141
      CALL AGSBEGM(MODULE,IPRIN)                                          141
      IF (IPRIN.LT.0) RETURN                                              141
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               141
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  141
      BPATH_FLGG='FLGG*'                                                  141
      IDNUM_FLGG(1)=0                                                     142
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    142
      FLGG_TYPE=1                                                         142
      CFLGG_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   142
C PBG WIDTH                                                               143
      FLGG_WIDTH=3.81                                                     143
      CFLGG_WIDTH='PBG WIDTH'                                             143
C PBG DEPTH                                                               144
      FLGG_DEPTH=45.0                                                     144
      CFLGG_DEPTH='PBG DEPTH'                                             144
C GAP BETWEEN PBG                                                         145
      FLGG_DGAP=0.01                                                      145
      CFLGG_DGAP='GAP BETWEEN PBG'                                        145
C ALMUNIM WRAP THICKNESS (REAL)                                           146
      FLGG_ALTHICK=0.001                                                  146
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       146
C PHOTO CATHODE THICKNESS                                                 147
      FLGG_PHCATHDZ=2.0                                                   147
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            147
C PHOTO CATHODE RADIUS (REAL)                                             148
      FLGG_PHCATHR=1.8                                                    148
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         148
C MU METAL LENGTH                                                         149
      FLGG_MUMETDZ=11.0                                                   149
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     149
C MU METAL OUTER RADIUS (REAL)                                            150
      FLGG_MUMETR=1.9                                                     150
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         150
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    151
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           151
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   151
C *                                                                       151
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    151
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           151
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   151
C *                                                                       152
      CALL AGSBEGM(MODULE,IPRIN)                                          152
      IF (IPRIN.LT.0) RETURN                                              152
C Filling bank FLGG - PBG DETECTOR GEOMETRY                               152
      BANK_TITLE='PBG DETECTOR GEOMETRY'                                  152
      BPATH_FLGG='FLGG*'                                                  152
      IDNUM_FLGG(1)=0                                                     153
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    153
      FLGG_TYPE=2                                                         153
      CFLGG_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   153
C PBG WIDTH                                                               154
      FLGG_WIDTH=5.8                                                      154
      CFLGG_WIDTH='PBG WIDTH'                                             154
C PBG DEPTH                                                               155
      FLGG_DEPTH=60.2                                                     155
      CFLGG_DEPTH='PBG DEPTH'                                             155
C GAP BETWEEN PBG                                                         156
      FLGG_DGAP=0.01                                                      156
      CFLGG_DGAP='GAP BETWEEN PBG'                                        156
C ALMUNIM WRAP THICKNESS (REAL)                                           157
      FLGG_ALTHICK=0.001                                                  157
      CFLGG_ALTHICK='ALMUNIM WRAP THICKNESS (REAL)'                       157
C PHOTO CATHODE THICKNESS                                                 158
      FLGG_PHCATHDZ=2.0                                                   158
      CFLGG_PHCATHDZ='PHOTO CATHODE THICKNESS'                            158
C PHOTO CATHODE RADIUS (REAL)                                             159
      FLGG_PHCATHR=1.8                                                    159
      CFLGG_PHCATHR='PHOTO CATHODE RADIUS (REAL)'                         159
C MU METAL LENGTH                                                         160
      FLGG_MUMETDZ=11.0                                                   160
      CFLGG_MUMETDZ='MU METAL LENGTH'                                     160
C MU METAL OUTER RADIUS (REAL)                                            161
      FLGG_MUMETR=1.9                                                     161
      CFLGG_MUMETR='MU METAL OUTER RADIUS (REAL)'                         161
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    162
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           162
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   162
C *                                                                       162
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGG',    162
     *BPATH_FLGG,IDNUM_FLGG,1, MAP_FLGG,NAMES_FLGG,COMMEN_FLGG,           162
     *BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                   162
C *                                                                       163
      CALL AGSBEGM(MODULE,IPRIN)                                          163
      IF (IPRIN.LT.0) RETURN                                              163
C Filling bank FLGM - PBG DETECTOR MATERIALS                              163
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 163
      BPATH_FLGM='FLGM*'                                                  163
      IDNUM_FLGM(1)=0                                                     164
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    164
      FLGM_TYPE=1                                                         164
      CFLGM_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   164
C GDENSITY [/CM^3]                                                        165
      FLGM_DENSITY=3.86                                                   165
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    165
C RADIATION LENGTH [CM]                                                   166
      FLGM_RADLEN=2.5                                                     166
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                166
C PBO CONTENT [%]                                                         167
      FLGM_PBCONT=65.4                                                    167
      CFLGM_PBCONT='PBO CONTENT [%]'                                      167
C CRITICAL ENERGY [GEV]                                                   168
      FLGM_CRITENE=0.0158                                                 168
      CFLGM_CRITENE='CRITICAL ENERGY [GEV]'                               168
C MOLIERE RADIUS [CM]                                                     169
      FLGM_MOLIERER=3.32                                                  169
      CFLGM_MOLIERER='MOLIERE RADIUS [CM]'                                169
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    170
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           170
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   170
C *                                                                       170
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    170
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           170
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   170
C *                                                                       171
      CALL AGSBEGM(MODULE,IPRIN)                                          171
      IF (IPRIN.LT.0) RETURN                                              171
C Filling bank FLGM - PBG DETECTOR MATERIALS                              171
      BANK_TITLE='PBG DETECTOR MATERIALS'                                 171
      BPATH_FLGM='FLGM*'                                                  171
      IDNUM_FLGM(1)=0                                                     172
C TYPE (1=PROTOVINO CELL, 2=FLAB CELL)                                    172
      FLGM_TYPE=2                                                         172
      CFLGM_TYPE='TYPE (1=PROTOVINO CELL, 2=FLAB CELL)'                   172
C GDENSITY [/CM^3]                                                        173
      FLGM_DENSITY=3.61                                                   173
      CFLGM_DENSITY='GDENSITY [/CM^3]'                                    173
C RADIATION LENGTH [CM]                                                   174
      FLGM_RADLEN=3.21                                                    174
      CFLGM_RADLEN='RADIATION LENGTH [CM]'                                174
C PBO CONTENT [%]                                                         175
      FLGM_PBCONT=45.0                                                    175
      CFLGM_PBCONT='PBO CONTENT [%]'                                      175
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    176
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           176
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   176
C *                                                                       176
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FLGM',    176
     *BPATH_FLGM,IDNUM_FLGM,1, MAP_FLGM,NAMES_FLGM,COMMEN_FLGM,           176
     *BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                   176
C *                                                                       177
      CALL AGSBEGM(MODULE,IPRIN)                                          177
      IF (IPRIN.LT.0) RETURN                                              177
C Filling bank PBPD - PB PLATE DIMENSIONS                                 177
      BANK_TITLE='PB PLATE DIMENSIONS'                                    177
      BPATH_PBPD='PBPD*'                                                  177
      IDNUM_PBPD(1)=0                                                     178
C WIDTH                                                                   178
      PBPD_WIDTH=33.02                                                    178
      CPBPD_WIDTH='WIDTH'                                                 178
C HEIGHT                                                                  179
      PBPD_HEIGHT=33.02                                                   179
      CPBPD_HEIGHT='HEIGHT'                                               179
C THICKNESS                                                               180
      PBPD_THICK=1.27                                                     180
      CPBPD_THICK='THICKNESS'                                             180
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    181
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           181
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   181
C *                                                                       181
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PBPD',    181
     *BPATH_PBPD,IDNUM_PBPD,1, MAP_PBPD,NAMES_PBPD,COMMEN_PBPD,           181
     *BANK_PBPD,LEN_PBPD,LLEN_PBPD,LINK_PBPD,FLAG_PBPD)                   181
C *                                                                       182
      CALL AGSBEGM(MODULE,IPRIN)                                          182
      IF (IPRIN.LT.0) RETURN                                              182
C Filling bank FMXG - SMD GEOMETRY                                        182
      BANK_TITLE='SMD GEOMETRY'                                           182
      BPATH_FMXG='FMXG*'                                                  182
      IDNUM_FMXG(1)=0                                                     183
C GEOMETRY VERSION                                                        183
      FMXG_VERSION=2                                                      183
      CFMXG_VERSION='GEOMETRY VERSION'                                    183
C SCINTILLATOR STRIP APEX                                                 184
      FMXG_SAPEX=0.7                                                      184
      CFMXG_SAPEX='SCINTILLATOR STRIP APEX'                               184
C SCINTILLATOR STRIP BASE                                                 185
      FMXG_SBASE=1.0                                                      185
      CFMXG_SBASE='SCINTILLATOR STRIP BASE'                               185
C GAP BETWEEN STRIPS                                                      186
      FMXG_SGAP=0.0064                                                    186
      CFMXG_SGAP='GAP BETWEEN STRIPS'                                     186
C # OF STRIPS                                                             187
      FMXG_NSTRIP=50                                                      187
      CFMXG_NSTRIP='# OF STRIPS'                                          187
C G10 PLATE WIDTH                                                         188
      FMXG_G10WIDTH=27.0                                                  188
      CFMXG_G10WIDTH='G10 PLATE WIDTH'                                    188
C G10 PLATE HEIGHT                                                        189
      FMXG_G10HGT=27.0                                                    189
      CFMXG_G10HGT='G10 PLATE HEIGHT'                                     189
C G10 PLATE THICKNESS                                                     190
      FMXG_G10THICK=0.15                                                  190
      CFMXG_G10THICK='G10 PLATE THICKNESS'                                190
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    191
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           191
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   191
C *                                                                       191
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FMXG',    191
     *BPATH_FMXG,IDNUM_FMXG,1, MAP_FMXG,NAMES_FMXG,COMMEN_FMXG,           191
     *BANK_FMXG,LEN_FMXG,LLEN_FMXG,LINK_FMXG,FLAG_FMXG)                   191
C *                                                                       192
      CALL AGSBEGM(MODULE,IPRIN)                                          192
      IF (IPRIN.LT.0) RETURN                                              192
C Filling bank INSE - INSERT GEOMETRY                                     192
      BANK_TITLE='INSERT GEOMETRY'                                        192
      BPATH_INSE='INSE*'                                                  192
      IDNUM_INSE(1)=0                                                     193
C WIDTH OF THE INSERT (X)                                                 193
      INSE_WIDTH=19.30908                                                 193
      CINSE_WIDTH='WIDTH OF THE INSERT (X)'                               193
C DEPTH OF THE INSERT (Z)                                                 194
      INSE_DEPTH=98.425                                                   194
      CINSE_DEPTH='DEPTH OF THE INSERT (Z)'                               194
C HEIGHT OF THE INSERT (Y)                                                195
      INSE_HEIGHT=38.608                                                  195
      CINSE_HEIGHT='HEIGHT OF THE INSERT (Y)'                             195
C DEPTH OF THE STEEL PARTS (X,Y)                                          196
      INSE_SHEETDPT=1.27                                                  196
      CINSE_SHEETDPT='DEPTH OF THE STEEL PARTS (X,Y)'                     196
C DISTANCE BETWEEN EDGE OF INSERT AND SQUARE HOLE                         197
      INSE_HOLEGAP=5.08                                                   197
      CINSE_HOLEGAP='DISTANCE BETWEEN EDGE OF INSERT AND SQUARE HOLE'     197
C DEPTH OF THE SQUARE HOLE (Z)                                            198
      INSE_HOLEDEPTH=25.4                                                 198
      CINSE_HOLEDEPTH='DEPTH OF THE SQUARE HOLE (Z)'                      198
C HEIGHT OF THE SQUARE HOLE (Y)                                           199
      INSE_HOLEHEIGHT=30.48                                               199
      CINSE_HOLEHEIGHT='HEIGHT OF THE SQUARE HOLE (Y)'                    199
C DEPTH OF THE IRON DISTANCER (Z)                                         200
      INSE_GAPDEPTH=19.685                                                200
      CINSE_GAPDEPTH='DEPTH OF THE IRON DISTANCER (Z)'                    200
C HEIGHT OF THE IRON DISTANCER (Y)                                        201
      INSE_GAPHEIGHT=7.874                                                201
      CINSE_GAPHEIGHT='HEIGHT OF THE IRON DISTANCER (Y)'                  201
C DEPTH OF ONE OF THE THREE GATES (Z)                                     202
      INSE_GATEDEPTH=1.905                                                202
      CINSE_GATEDEPTH='DEPTH OF ONE OF THE THREE GATES (Z)'               202
C DISTANCE BETWEEN THE BACK EDGE OF THE BOX AND LAST GATE                 203
      INSE_GATEGAP=12.7                                                   203
      CINSE_GATEGAP='DISTANCE BETWEEN THE BACK EDGE OF THE BOX AND LAST   203
     *GATE'                                                               203
C RADIUS OF THE INNER CIRCLE OF TUBES                                     204
      INSE_RA=13.97                                                       204
      CINSE_RA='RADIUS OF THE INNER CIRCLE OF TUBES'                      204
C RADIUS OF THE OUTER CIRCLE OF TUBES                                     205
      INSE_RB=20.6375                                                     205
      CINSE_RB='RADIUS OF THE OUTER CIRCLE OF TUBES'                      205
C DIAMETER OF THE TUBES                                                   206
      INSE_DIAM=6.0325                                                    206
      CINSE_DIAM='DIAMETER OF THE TUBES'                                  206
C RADIUS OF THE INNER TUBE FOR BEAMPIPE                                   207
      INSE_RMAX=10.16                                                     207
      CINSE_RMAX='RADIUS OF THE INNER TUBE FOR BEAMPIPE'                  207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'INSE',    208
     *BPATH_INSE,IDNUM_INSE,1, MAP_INSE,NAMES_INSE,COMMEN_INSE,           208
     *BANK_INSE,LEN_INSE,LLEN_INSE,LINK_INSE,FLAG_INSE)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'INSE',    208
     *BPATH_INSE,IDNUM_INSE,1, MAP_INSE,NAMES_INSE,COMMEN_INSE,           208
     *BANK_INSE,LEN_INSE,LLEN_INSE,LINK_INSE,FLAG_INSE)                   208
C use default FMCG bank                                                   209
      BPATH_FMCG='FMCG*'                                                  209
      IDNUM_FMCG(1)=0                                                     209
      CALL AGDGETP('FPDMGEO3','*',-999,'FMCG',' ',BANK_FMCG, BPATH_FMCG,  209
     *IDNUM_FMCG,1,MAP_FMCG,NAMES_FMCG,BANK_FMCG,LEN_FMCG,LLEN_FMCG,      209
     *LINK_FMCG,FLAG_FMCG)                                                209
C Check IPRIN>=1                                                          209
      IF (IPRIN.GE.1) WRITE(LOUT,5010)FMCG_VERSION                        209
5010  FORMAT('****************** FPDMGEO version ', F4.2)                 211
C *                                                                       212
      DO 5021 M=1,4                                                       212
C    use FPOS bank                                                        213
         BPATH_FPOS='FPOS*'                                               213
         IDNUM_FPOS(1)=0                                                  213
         FPOS_IMOD=M                                                      213
         CALL AGDGETP('FPDMGEO3','*',-999,'FPOS',NFPOS_IMOD,              213
     *   RE_FPOS_IMOD,BPATH_FPOS,IDNUM_FPOS,1,MAP_FPOS,NAMES_FPOS,        213
     *   BANK_FPOS,LEN_FPOS,LLEN_FPOS,LINK_FPOS,FLAG_FPOS)                213
C    use FBXD bank                                                        214
         BPATH_FBXD='FBXD*'                                               214
         IDNUM_FBXD(1)=0                                                  214
         FBXD_TYPE=FPOS_ITYPE                                             214
         CALL AGDGETP('FPDMGEO3','*',-999,'FBXD',NFBXD_TYPE,              214
     *   RE_FBXD_TYPE,BPATH_FBXD,IDNUM_FBXD,1,MAP_FBXD,NAMES_FBXD,        214
     *   BANK_FBXD,LEN_FBXD,LLEN_FBXD,LINK_FBXD,FLAG_FBXD)                214
C    Check FBXD_TYPE.EQ.1                                                 215
         IF (FBXD_TYPE.EQ.1) THEN                                         215
C    use FLGG bank                                                        216
         BPATH_FLGG='FLGG*'                                               216
         IDNUM_FLGG(1)=0                                                  216
         FLGG_TYPE=1                                                      216
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGG',NFLGG_TYPE,              216
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        216
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                216
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  217
         ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0            218
         RTOT = FBXD_NX*WID/2.0                                           219
         BWID = RTOT+FBXD_XOFFSET                                         220
         ELSE                                                             221
C    use FLGG bank                                                        222
         BPATH_FLGG='FLGG*'                                               222
         IDNUM_FLGG(1)=0                                                  222
         FLGG_TYPE=2                                                      222
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGG',NFLGG_TYPE,              222
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        222
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                222
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  223
         ZTOT = (FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0            224
         RTOT = FBXD_NXL*WID/2.0                                          225
         BWID = RTOT                                                      226
         ENDIF                                                            227
C    Check M.GE.3                                                         228
         IF (M.GE.3) THEN                                                 228
         BWID=FBXD_WIDTH/2.0                                              229
C    Check FPOS_X.GT.0.0                                                  230
         IF (FPOS_X.GT.0.0) THEN                                          230
         XX=FPOS_X+BWID                                                   231
         ELSEIF (FPOS_X.EQ.0.0) THEN                                      232
         XX=0.0                                                           233
         ELSE                                                             234
         XX=FPOS_X-BWID                                                   235
         ENDIF                                                            236
         ELSE                                                             237
C    Check FPOS_X.GT.0.0                                                  238
         IF (FPOS_X.GT.0.0) THEN                                          238
         XX=FPOS_X+BWID                                                   239
         ELSEIF (FPOS_X.EQ.0.0) THEN                                      240
         XX=0.0                                                           241
         ELSE                                                             242
         XX=FPOS_X-BWID                                                   243
         ENDIF                                                            244
         ENDIF                                                            245
C    Check FPOS_Y.GT.0.0                                                  246
         IF (FPOS_Y.GT.0.0) THEN                                          246
         YY=FPOS_Y+FBXD_HEIGHT/2.0                                        247
         ELSEIF (FPOS_Y.EQ.0.0) THEN                                      248
         YY=0.0                                                           249
         ELSE                                                             250
         YY=FPOS_Y-FBXD_HEIGHT/2.0                                        251
         ENDIF                                                            252
C    Check FPOS_Z.GT.0.0                                                  253
         IF (FPOS_Z.GT.0.0) THEN                                          253
         ZZ=FPOS_Z+FBXD_DEPTH/2.0                                         254
         ELSE                                                             255
         ZZ=FPOS_Z-FBXD_DEPTH/2.0                                         256
         ENDIF                                                            257
         SERN=0                                                           258
C    Check M.EQ.4                                                         259
         IF (M.EQ.4) THEN                                                 259
         SERN=1                                                           260
         ENDIF                                                            261
C    Check M.NE.7                                                         262
         IF (M.NE.7) THEN                                                 262
         CALL AGSBEGM(MODULE,IPRIN)                                       263
C    Check IPRIN<0                                                        263
         IF (IPRIN.LT.0) RETURN                                           263
C    Create FBOX :                                                        263
C    Call To Local Procedure FBOX                                         263
         ASSIGN 5040 TO BLOCK_FBOX                                        263
         GO TO  5030                                                      263
5040     CONTINUE                                                         263
C    *                                                                    263
C    Geant POSITION : FBOX ===> MOTHER='CAVE' X=XX Y=YY Z=ZZ ALPHAY=FPOS  264
C    _AY KONLY='MANY' <===                                                264
         CALL AGSRESET                                                    264
         AG_MOTHER = 'CAVE'                                               264
         AG_X = XX                                                        264
         AG_Y = YY                                                        264
         AG_Z = ZZ                                                        264
         AG_ALPHAN=AG_ALPHAN+1                                            264
         AG_ALPHAK(AG_ALPHAN)=2                                           264
         AG_ALPHAL(AG_ALPHAN)=FPOS_AY                                     264
         AG_KONLY = 'MANY'                                                264
         AG_TITLE = 'FBOX'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_KONLY'                         264
         CALL AXPOSITION                                                  264
C    *                                                                    264
         ENDIF                                                            264
5021  CONTINUE                                                            266
5022  CONTINUE                                                            266
C ----------------------------------------------------------------------  267
C ------------ FBOX                                                       267
      CALL AGSENDM                                                        268
      RETURN                                                              268
C Begin Local Procedure FBOX                                              268
C FBOX IS ONE PB-GLASS FPD DETECTOR                                       268
5030  CONTINUE                                                            268
         AG_TITLE='FBOX'                                                  268
         AG_MODULE=MODULE                                                 268
         AG_MARK='V00'                                                    268
         CALL AGSPUSH                                                     268
         AG_MODULE=MODULE                                                 268
C    *                                                                    268
C    Geant MATERIAL : AIR ===> inherited arguments used <===              269
         CALL AGSRESET                                                    269
         AG_TITLE = 'AIR'                                                 269
         AG_EXNAME = 'MATERIAL'                                           269
         AG_PARLIST = 'NONE'                                              269
         CALL AXMATERIAL                                                  269
C    *                                                                    269
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           269
         CALL AGSRESET                                                    270
         AG_TITLE = 'STANDARD'                                            270
         AG_EXNAME = 'MEDIUM'                                             270
         AG_PARLIST = 'NONE'                                              270
         CALL AXMEDIUM                                                    270
C    *                                                                    270
C    Geant ATTRIBUTE : FBOX ===> SEEN=1 COLO=2 SERIAL=SERN <===           270
         CALL AGSRESET                                                    271
         AG_SEEN = 1                                                      271
         AG_COLO = 2                                                      271
         AG_SERIAL = SERN                                                 271
         AG_TITLE = 'FBOX'                                                271
         AG_EXNAME = 'ATTRIBUTE'                                          271
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  271
         CALL AXATTRIBUTE                                                 271
C    *                                                                    271
C    Check FBXD_TYPE.EQ.2                                                 271
         IF (FBXD_TYPE.EQ.2) THEN                                         272
C    Geant SHAPE : BOX ===> DX=FBXD_WIDTH/2.0 DY=FBXD_HEIGHT/2.0, DZ=FBX  274
C    D_DEPTH/2.0 <===                                                     274
         CALL AGSRESET                                                    274
         AG_DX = FBXD_WIDTH/2.0                                           274
         AG_DY = FBXD_HEIGHT/2.0                                          274
         AG_DZ = FBXD_DEPTH/2.0                                           274
         AG_TITLE = 'BOX'                                                 274
         AG_EXNAME = 'SHAPE'                                              274
         AG_PARLIST = 'DX_DY_DZ'                                          274
         CALL AXSHAPE                                                     274
C    *                                                                    274
         ELSE                                                             274
C    Geant SHAPE : BOX ===> DX=BWID DY=FBXD_HEIGHT/2.0 DZ=FBXD_DEPTH/2.0  276
C     <===                                                                276
         CALL AGSRESET                                                    276
         AG_DX = BWID                                                     276
         AG_DY = FBXD_HEIGHT/2.0                                          276
         AG_DZ = FBXD_DEPTH/2.0                                           276
         AG_TITLE = 'BOX'                                                 276
         AG_EXNAME = 'SHAPE'                                              276
         AG_PARLIST = 'DX_DY_DZ'                                          276
         CALL AXSHAPE                                                     276
C    *                                                                    276
         ENDIF                                                            276
C    use FLGG bank                                                        278
         BPATH_FLGG='FLGG*'                                               278
         IDNUM_FLGG(1)=0                                                  278
         FLGG_TYPE=2                                                      278
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGG',NFLGG_TYPE,              278
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        278
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                278
C    LARGE CELL WIDTH                                                     279
         WIDL = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                 279
C    use FLGG bank                                                        280
         BPATH_FLGG='FLGG*'                                               280
         IDNUM_FLGG(1)=0                                                  280
         FLGG_TYPE=1                                                      280
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGG',NFLGG_TYPE,              280
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        280
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                280
C    use FLGM bank                                                        281
         BPATH_FLGM='FLGM*'                                               281
         IDNUM_FLGM(1)=0                                                  281
         FLGM_TYPE=1                                                      281
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGM',NFLGM_TYPE,              281
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        281
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                281
C    SMALL CELL WIDTH                                                     282
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  282
         ZTOT =(FLGG_DEPTH + FLGG_ALTHICK + FLGG_MUMETDZ)/2.0             283
         RTOT = FBXD_NX*WID/2.0                                           284
         BWID = RTOT - FBXD_XOFFSET                                       285
C    Create FTOW :                                                        286
C    Call To Local Procedure FTOW                                         286
         ASSIGN 5060 TO BLOCK_FTOW                                        286
         GO TO  5050                                                      286
5060     CONTINUE                                                         286
C    *                                                                    286
C    Create PBPT :                                                        287
C    Call To Local Procedure PBPT                                         287
         ASSIGN 5080 TO BLOCK_PBPT                                        287
         GO TO  5070                                                      287
5080     CONTINUE                                                         287
C    *                                                                    287
C    Create FSHM :                                                        288
C    Call To Local Procedure FSHM                                         288
         ASSIGN 5100 TO BLOCK_FSHM                                        288
         GO TO  5090                                                      288
5100     CONTINUE                                                         288
C    *                                                                    288
C    Check FBXD_TYPE.EQ.2.AND.FPOS_IMOD.EQ.4                              289
         IF (FBXD_TYPE.EQ.2.AND.FPOS_IMOD.EQ.4) THEN                      289
C    X0 START FROM NORTH (NEAR BEAM) FOR                                  290
         X0 = - RTOT - FBXD_XOFFSET + WID/2.0                             290
C    WS (FMS-SOUTH) MODULE                                                291
         WIDX = WID                                                       291
         ELSE                                                             292
C    X0 START FROM SOUTH FOR NORTH/TOP/BOTTOM MODULE                      293
         X0 = RTOT + FBXD_XOFFSET - WID/2.0                               293
C    SINCE FPD IS SYMMETRIC, TRUE FOR ES MODULE TOO                       294
         WIDX = -WID                                                      294
         ENDIF                                                            295
C    Check FBXD_TYPE.EQ.2                                                 296
         IF (FBXD_TYPE.EQ.2) THEN                                         296
C    IN ORDER TO CORRECT THE DIFFERENCES BETWEEN                          297
         Y1 = FBXD_NY*WID/2.0 - WID/2.0 + (16*WIDL-FBXD_NY*WID)/2.0       297
C    3 SMALL CELLS AND 2 LARGE ONESSEE AGAIN IN 17 LIN                    298
         WIDY = WID+(16.0*WIDL-FBXD_NY*WID)/23.0                          298
         ELSE                                                             299
         Y1 = FBXD_NY*WID/2.0 - WID/2.0                                   300
         WIDY = WID                                                       301
         ENDIF                                                            302
         Z1 = - FBXD_DEPTH/2.0 + FBXD_ZOFFSET + ZTOT                      303
C    *                                                                    304
         I=1                                                              304
            GO TO 5113                                                    304
5111        I=I+1                                                         304
5113        IF(I-(FBXD_NY).GT.0) GO TO 5112                               304
            X1=X0                                                         305
C       *                                                                 306
            J=1                                                           306
               GO TO 5123                                                 306
5121           J=J+1                                                      306
5123           IF(J-(FBXD_NX).GT.0) GO TO 5122                            306
C          Check FBXD_TYPE.EQ.2.AND.J.LT.6.AND.I.GT.7.AND.I.LT.18         307
               IF (FBXD_TYPE.EQ.2.AND.J.LT.6.AND.I.GT.7.AND.I.LT.18)      307
     *         THEN                                                       307
               X1=X1+WIDX                                                 308
               ELSE                                                       309
C          Create FTOW :                                                  310
C          Call To Local Procedure FTOW                                   310
               ASSIGN 5130 TO BLOCK_FTOW                                  310
               GO TO  5050                                                310
5130           CONTINUE                                                   310
C          *                                                              310
C          Geant POSITION : FTOW ===> X=X1 Y=Y1 Z=Z1 <===                 311
               CALL AGSRESET                                              311
               AG_X = X1                                                  311
               AG_Y = Y1                                                  311
               AG_Z = Z1                                                  311
               AG_TITLE = 'FTOW'                                          311
               AG_EXNAME = 'POSITION'                                     311
               AG_PARLIST = 'X_Y_Z'                                       311
               CALL AXPOSITION                                            311
C          *                                                              311
               X1=X1+WIDX                                                 311
               ENDIF                                                      313
            GO TO 5121                                                    314
5122        CONTINUE                                                      314
            Y1 = Y1-WIDY                                                  315
         GO TO 5111                                                       316
5112     CONTINUE                                                         316
C    Check FBXD_TYPE.EQ.1                                                 317
         IF (FBXD_TYPE.EQ.1) THEN                                         317
         X1=X0                                                            318
         Y1= -RTOT + ZTOT                                                 319
         Z1=-FBXD_DEPTH/2.0 + FBXD_PSOFFSET + WID/2.0                     320
C    *                                                                    321
         J=1                                                              321
            GO TO 5143                                                    321
5141        J=J+1                                                         321
5143        IF(J-(FBXD_NX).GT.0) GO TO 5142                               321
C       Create FTOW :                                                     322
C       Call To Local Procedure FTOW                                      322
            ASSIGN 5150 TO BLOCK_FTOW                                     322
            GO TO  5050                                                   322
5150        CONTINUE                                                      322
C       *                                                                 322
C       Geant POSITION : FTOW ===> X=X1 Y=Y1 Z=Z1 ALPHAX=90 <===          323
            CALL AGSRESET                                                 323
            AG_X = X1                                                     323
            AG_Y = Y1                                                     323
            AG_Z = Z1                                                     323
            AG_ALPHAN=AG_ALPHAN+1                                         323
            AG_ALPHAK(AG_ALPHAN)=1                                        323
            AG_ALPHAL(AG_ALPHAN)=90                                       323
            AG_TITLE = 'FTOW'                                             323
            AG_EXNAME = 'POSITION'                                        323
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   323
            CALL AXPOSITION                                               323
C       *                                                                 323
            X1=X1-WID                                                     323
         GO TO 5141                                                       325
5142     CONTINUE                                                         325
C    Check FMCG_PBPLATE==1                                                326
         IF (FMCG_PBPLATE.EQ.1) THEN                                      326
C    Create PBPT :                                                        327
C    Call To Local Procedure PBPT                                         327
         ASSIGN 5160 TO BLOCK_PBPT                                        327
         GO TO  5070                                                      327
5160     CONTINUE                                                         327
C    *                                                                    327
C    Geant POSITION : PBPT ===> X=0 Y=0 Z=PBPD_THICK/2.0-FBXD_DEPTH/2.0   328
C    <===                                                                 328
         CALL AGSRESET                                                    328
         AG_X = 0                                                         328
         AG_Y = 0                                                         328
         AG_Z = PBPD_THICK/2.0-FBXD_DEPTH/2.0                             328
         AG_TITLE = 'PBPT'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'X_Y_Z'                                             328
         CALL AXPOSITION                                                  328
C    *                                                                    328
         ENDIF                                                            328
         ZTOTSMD=FMXG_G10THICK+FMXG_SAPEX                                 330
C    Create FSHM :                                                        331
C    Call To Local Procedure FSHM                                         331
         ASSIGN 5170 TO BLOCK_FSHM                                        331
         GO TO  5090                                                      331
5170     CONTINUE                                                         331
C    *                                                                    331
C    Geant POSITION : FSHM ===> X=0 Y=0 Z=FBXD_SMDOFF+ZTOTSMD-FBXD_DEPTH  332
C    /2.0 <===                                                            332
         CALL AGSRESET                                                    332
         AG_X = 0                                                         332
         AG_Y = 0                                                         332
         AG_Z = FBXD_SMDOFF+ZTOTSMD-FBXD_DEPTH/2.0                        332
         AG_TITLE = 'FSHM'                                                332
         AG_EXNAME = 'POSITION'                                           332
         AG_PARLIST = 'X_Y_Z'                                             332
         CALL AXPOSITION                                                  332
C    *                                                                    332
         ENDIF                                                            332
C    Check FBXD_TYPE.GE.2                                                 334
         IF (FBXD_TYPE.GE.2) THEN                                         334
C    use FLGG bank                                                        335
         BPATH_FLGG='FLGG*'                                               335
         IDNUM_FLGG(1)=0                                                  335
         FLGG_TYPE=2                                                      335
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGG',NFLGG_TYPE,              335
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        335
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                335
C    use FLGM bank                                                        336
         BPATH_FLGM='FLGM*'                                               336
         IDNUM_FLGM(1)=0                                                  336
         FLGM_TYPE=2                                                      336
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGM',NFLGM_TYPE,              336
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        336
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                336
         WID = FLGG_WIDTH + FLGG_DGAP + FLGG_ALTHICK*2.0                  337
         ZTOT = FLGG_DEPTH/2.0                                            338
         RTOT = FBXD_NXL*WID/2.0                                          339
         BWID = RTOT                                                      340
C    LARGE CELLS OFFSET IN X                                              341
         XLCOFFSET = (FBXD_WIDTH-FBXD_NXL*WID)/2.0                        341
C    Check FPOS_IMOD.EQ.4                                                 342
         IF (FPOS_IMOD.EQ.4) THEN                                         342
         X0 = -BWID + WID/2.0 - XLCOFFSET                                 343
         WIDX = WID                                                       344
         ELSEIF (FPOS_IMOD.EQ.3) THEN                                     345
         X0 = +BWID - WID/2.0 + XLCOFFSET                                 346
         WIDX = -WID                                                      347
         ENDIF                                                            348
         Y1 = FBXD_NYL*WID/2.0 - WID/2.0                                  349
         Z1 = -FBXD_DEPTH/2.0 + FBXD_ZOFFSET + ZTOT                       350
C    *                                                                    351
         I=1                                                              351
            GO TO 5183                                                    351
5181        I=I+1                                                         351
5183        IF(I-(FBXD_NYL).GT.0) GO TO 5182                              351
            X1=X0                                                         352
C       *                                                                 353
            J=1                                                           353
               GO TO 5193                                                 353
5191           J=J+1                                                      353
5193           IF(J-(FBXD_NXL).GT.0) GO TO 5192                           353
C          Check J.LT.9.AND.I.GT.9.AND.I.LT.26                            354
               IF (J.LT.9.AND.I.GT.9.AND.I.LT.26) THEN                    354
               X1=X1+WIDX                                                 355
               ELSEIF ((I+J).GE.45) THEN                                  356
C          Create FALU :                                                  357
C          Call To Local Procedure FALU                                   357
               ASSIGN 5210 TO BLOCK_FALU                                  357
               GO TO  5200                                                357
5210           CONTINUE                                                   357
C          *                                                              357
C          Geant POSITION : FALU ===> X=X1 Y=Y1 Z=Z1 <===                 358
               CALL AGSRESET                                              358
               AG_X = X1                                                  358
               AG_Y = Y1                                                  358
               AG_Z = Z1                                                  358
               AG_TITLE = 'FALU'                                          358
               AG_EXNAME = 'POSITION'                                     358
               AG_PARLIST = 'X_Y_Z'                                       358
               CALL AXPOSITION                                            358
C          *                                                              358
               X1=X1+WIDX                                                 358
               ELSEIF ((J-I).GE.10) THEN                                  360
               X1=X1+WIDX                                                 361
               ELSE                                                       362
C          Create FLXF :                                                  363
C          Call To Local Procedure FLXF                                   363
               ASSIGN 5230 TO BLOCK_FLXF                                  363
               GO TO  5220                                                363
5230           CONTINUE                                                   363
C          *                                                              363
C          Geant POSITION : FLXF ===> X=X1 Y=Y1 Z=Z1 <===                 364
               CALL AGSRESET                                              364
               AG_X = X1                                                  364
               AG_Y = Y1                                                  364
               AG_Z = Z1                                                  364
               AG_TITLE = 'FLXF'                                          364
               AG_EXNAME = 'POSITION'                                     364
               AG_PARLIST = 'X_Y_Z'                                       364
               CALL AXPOSITION                                            364
C          *                                                              364
               X1=X1+WIDX                                                 364
               ENDIF                                                      366
            GO TO 5191                                                    367
5192        CONTINUE                                                      367
            Y1=Y1-WID                                                     368
         GO TO 5181                                                       369
5182     CONTINUE                                                         369
         ENDIF                                                            370
C    Check FBXD_TYPE.EQ.2                                                 371
         IF (FBXD_TYPE.EQ.2) THEN                                         371
C    use FLGG bank                                                        372
         BPATH_FLGG='FLGG*'                                               372
         IDNUM_FLGG(1)=0                                                  372
         FLGG_TYPE=2                                                      372
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGG',NFLGG_TYPE,              372
     *   RE_FLGG_TYPE,BPATH_FLGG,IDNUM_FLGG,1,MAP_FLGG,NAMES_FLGG,        372
     *   BANK_FLGG,LEN_FLGG,LLEN_FLGG,LINK_FLGG,FLAG_FLGG)                372
C    use FLGM bank                                                        373
         BPATH_FLGM='FLGM*'                                               373
         IDNUM_FLGM(1)=0                                                  373
         FLGM_TYPE=2                                                      373
         CALL AGDGETP('FPDMGEO3','*',-999,'FLGM',NFLGM_TYPE,              373
     *   RE_FLGM_TYPE,BPATH_FLGM,IDNUM_FLGM,1,MAP_FLGM,NAMES_FLGM,        373
     *   BANK_FLGM,LEN_FLGM,LLEN_FLGM,LINK_FLGM,FLAG_FLGM)                373
         BZOFFSET=0.0                                                     374
         WID = FLGG_WIDTH + FLGG_DGAP                                     375
C    Create FBAS :                                                        376
C    Call To Local Procedure FBAS                                         376
         ASSIGN 5250 TO BLOCK_FBAS                                        376
         GO TO  5240                                                      376
5250     CONTINUE                                                         376
C    *                                                                    376
C    Geant POSITION : FBAS ===> X=FPOS_X Y=-(FBXD_NXL*WID+BASEWIDTH/2.0)  378
C     Z=-FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0 <===                      378
         CALL AGSRESET                                                    378
         AG_X = FPOS_X                                                    378
         AG_Y = -(FBXD_NXL*WID+BASEWIDTH/2.0)                             378
         AG_Z = -FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0                   378
         AG_TITLE = 'FBAS'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'X_Y_Z'                                             378
         CALL AXPOSITION                                                  378
C    *                                                                    378
         ENDIF                                                            378
C    Create FENC :                                                        380
C    Call To Local Procedure FENC                                         380
         ASSIGN 5270 TO BLOCK_FENC                                        380
         GO TO  5260                                                      380
5270     CONTINUE                                                         380
C    *                                                                    380
C    Create FEAC :                                                        381
C    Call To Local Procedure FEAC                                         381
         ASSIGN 5290 TO BLOCK_FEAC                                        381
         GO TO  5280                                                      381
5290     CONTINUE                                                         381
C    *                                                                    381
C    Create FECC :                                                        382
C    Call To Local Procedure FECC                                         382
         ASSIGN 5310 TO BLOCK_FECC                                        382
         GO TO  5300                                                      382
5310     CONTINUE                                                         382
C    *                                                                    382
C    Create FEDC :                                                        383
C    Call To Local Procedure FEDC                                         383
         ASSIGN 5330 TO BLOCK_FEDC                                        383
         GO TO  5320                                                      383
5330     CONTINUE                                                         383
C    *                                                                    383
C    Create FEEC :                                                        384
C    Call To Local Procedure FEEC                                         384
         ASSIGN 5350 TO BLOCK_FEEC                                        384
         GO TO  5340                                                      384
5350     CONTINUE                                                         384
C    *                                                                    384
         DISTANCER=INSE_GAPHEIGHT - (INSE_HEIGHT - 2.0*INSE_SHEETDPT)     385
         XOFFFECC=(INSE_SHEETDPT - FBXD_WIDTH)/2.0                        386
         ZOFFFECC= - FBXD_DEPTH/2.0 + BZOFFSET + INSE_DEPTH -             387
     *   INSE_GATEGAP                                                     387
         XOFFFEDC=INSE_WIDTH - INSE_SHEETDPT                              388
         XOFFFENC=(INSE_WIDTH - FBXD_WIDTH)/2.0                           389
         YOFFFENC=(INSE_HEIGHT - INSE_SHEETDPT)/2.0                       390
         ZOFFFENC= - FBXD_DEPTH/2.0 + BZOFFSET + INSE_DEPTH/2.0           391
C    THIS IS TO MOVE INSERT TO SMALL CELL EDGE                            392
         XSHIFT=8*5.812 - 12*3.822 + 5*3.822 - INSE_WIDTH                 392
C    Check FBXD_TYPE.EQ.2                                                 393
         IF (FBXD_TYPE.EQ.2) THEN                                         393
C    Check FPOS_IMOD.EQ.4                                                 394
         IF (FPOS_IMOD.EQ.4) THEN                                         394
C    Geant POSITION : FENC ===> X=XOFFFENC+XSHIFT Y=-YOFFFENC Z=ZOFFFENC  395
C     <===                                                                395
         CALL AGSRESET                                                    395
         AG_X = XOFFFENC+XSHIFT                                           395
         AG_Y = -YOFFFENC                                                 395
         AG_Z = ZOFFFENC                                                  395
         AG_TITLE = 'FENC'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'X_Y_Z'                                             395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Geant POSITION : FENC ===> X=XOFFFENC+XSHIFT Y=YOFFFENC Z=ZOFFFENC   395
C    <===                                                                 395
         CALL AGSRESET                                                    396
         AG_X = XOFFFENC+XSHIFT                                           396
         AG_Y = YOFFFENC                                                  396
         AG_Z = ZOFFFENC                                                  396
         AG_TITLE = 'FENC'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y_Z'                                             396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Geant POSITION : FEAC ===> X=INSE_WIDTH+XSHIFT-(FBXD_WIDTH+INSE_SHE  396
C    ETDPT)/2.0 Y=0 Z=-FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0 <===        396
         CALL AGSRESET                                                    398
         AG_X = INSE_WIDTH+XSHIFT-(FBXD_WIDTH+INSE_SHEETDPT)/2.0          398
         AG_Y = 0                                                         398
         AG_Z = -FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0                   398
         AG_TITLE = 'FEAC'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'X_Y_Z'                                             398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=DISTANCER/2.0 Z=ZOFF  398
C    FECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                           398
         CALL AGSRESET                                                    400
         AG_X = XOFFFECC+XSHIFT                                           400
         AG_Y = DISTANCER/2.0                                             400
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 400
         AG_TITLE = 'FECC'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'X_Y_Z'                                             400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=DISTANCER/2.0 Z=ZOFF  400
C    FECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                   400
         CALL AGSRESET                                                    402
         AG_X = XOFFFECC+XSHIFT                                           402
         AG_Y = DISTANCER/2.0                                             402
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         402
         AG_TITLE = 'FECC'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Y_Z'                                             402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=-DISTANCER/2.0 Z=ZOF  402
C    FFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                          402
         CALL AGSRESET                                                    404
         AG_X = XOFFFECC+XSHIFT                                           404
         AG_Y = -DISTANCER/2.0                                            404
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 404
         AG_TITLE = 'FECC'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'X_Y_Z'                                             404
         CALL AXPOSITION                                                  404
C    *                                                                    404
C    Geant POSITION : FECC ===> X=XOFFFECC+XSHIFT Y=-DISTANCER/2.0 Z=ZOF  404
C    FFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                  404
         CALL AGSRESET                                                    406
         AG_X = XOFFFECC+XSHIFT                                           406
         AG_Y = -DISTANCER/2.0                                            406
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         406
         AG_TITLE = 'FECC'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'X_Y_Z'                                             406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Geant POSITION : FEDC ===> X=XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0 Z  406
C    =ZOFFFECC-INSE_GATEDEPTH/2.0 <===                                    406
         CALL AGSRESET                                                    408
         AG_X = XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0                          408
         AG_Y = 0                                                         408
         AG_Z = ZOFFFECC-INSE_GATEDEPTH/2.0                               408
         AG_TITLE = 'FEDC'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z'                                             408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Geant POSITION : FEDC ===> X=XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0 Z  408
C    =ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/2.0 <===       408
         CALL AGSRESET                                                    410
         AG_X = XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0                          410
         AG_Y = 0                                                         410
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/     410
     *   2.0                                                              410
         AG_TITLE = 'FEDC'                                                410
         AG_EXNAME = 'POSITION'                                           410
         AG_PARLIST = 'X_Y_Z'                                             410
         CALL AXPOSITION                                                  410
C    *                                                                    410
C    Geant POSITION : FEDC ===> X=XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0 Z  410
C    =ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0 <==  410
C    =                                                                    410
         CALL AGSRESET                                                    412
         AG_X = XSHIFT+(XOFFFEDC-FBXD_WIDTH)/2.0                          412
         AG_Y = 0                                                         412
         AG_Z =                                                           412
     *   ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0   412
         AG_TITLE = 'FEDC'                                                412
         AG_EXNAME = 'POSITION'                                           412
         AG_PARLIST = 'X_Y_Z'                                             412
         CALL AXPOSITION                                                  412
C    *                                                                    412
         ELSEIF (FPOS_IMOD.EQ.3) THEN                                     412
C    Geant POSITION : FENC ===> X=-XOFFFENC-XSHIFT Y=-YOFFFENC Z=ZOFFFEN  414
C    C <===                                                               414
         CALL AGSRESET                                                    414
         AG_X = -XOFFFENC-XSHIFT                                          414
         AG_Y = -YOFFFENC                                                 414
         AG_Z = ZOFFFENC                                                  414
         AG_TITLE = 'FENC'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'X_Y_Z'                                             414
         CALL AXPOSITION                                                  414
C    *                                                                    414
C    Geant POSITION : FENC ===> X=-XOFFFENC-XSHIFT Y=YOFFFENC Z=ZOFFFENC  414
C     <===                                                                414
         CALL AGSRESET                                                    415
         AG_X = -XOFFFENC-XSHIFT                                          415
         AG_Y = YOFFFENC                                                  415
         AG_Z = ZOFFFENC                                                  415
         AG_TITLE = 'FENC'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'X_Y_Z'                                             415
         CALL AXPOSITION                                                  415
C    *                                                                    415
C    Geant POSITION : FEAC ===> X=-XSHIFT-INSE_WIDTH+(FBXD_WIDTH+INSE_SH  415
C    EETDPT)/2.0 Y=0 Z=-FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0 <===       415
         CALL AGSRESET                                                    417
         AG_X = -XSHIFT-INSE_WIDTH+(FBXD_WIDTH+INSE_SHEETDPT)/2.0         417
         AG_Y = 0                                                         417
         AG_Z = -FBXD_DEPTH/2.0+BZOFFSET+INSE_DEPTH/2.0                   417
         AG_TITLE = 'FEAC'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'X_Y_Z'                                             417
         CALL AXPOSITION                                                  417
C    *                                                                    417
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=DISTANCER/2.0 Z=ZOF  417
C    FFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                          417
         CALL AGSRESET                                                    419
         AG_X = -XOFFFECC-XSHIFT                                          419
         AG_Y = DISTANCER/2.0                                             419
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 419
         AG_TITLE = 'FECC'                                                419
         AG_EXNAME = 'POSITION'                                           419
         AG_PARLIST = 'X_Y_Z'                                             419
         CALL AXPOSITION                                                  419
C    *                                                                    419
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=DISTANCER/2.0 Z=ZOF  419
C    FFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                  419
         CALL AGSRESET                                                    421
         AG_X = -XOFFFECC-XSHIFT                                          421
         AG_Y = DISTANCER/2.0                                             421
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         421
         AG_TITLE = 'FECC'                                                421
         AG_EXNAME = 'POSITION'                                           421
         AG_PARLIST = 'X_Y_Z'                                             421
         CALL AXPOSITION                                                  421
C    *                                                                    421
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=-DISTANCER/2.0 Z=ZO  421
C    FFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0 <===                         421
         CALL AGSRESET                                                    423
         AG_X = -XOFFFECC-XSHIFT                                          423
         AG_Y = -DISTANCER/2.0                                            423
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH/2.0                 423
         AG_TITLE = 'FECC'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'X_Y_Z'                                             423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Geant POSITION : FECC ===> X=-XOFFFECC-XSHIFT Y=-DISTANCER/2.0 Z=ZO  423
C    FFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0 <===                 423
         CALL AGSRESET                                                    425
         AG_X = -XOFFFECC-XSHIFT                                          425
         AG_Y = -DISTANCER/2.0                                            425
         AG_Z = ZOFFFECC-2.0*INSE_GATEDEPTH-3.0*INSE_GAPDEPTH/2.0         425
         AG_TITLE = 'FECC'                                                425
         AG_EXNAME = 'POSITION'                                           425
         AG_PARLIST = 'X_Y_Z'                                             425
         CALL AXPOSITION                                                  425
C    *                                                                    425
C    Geant POSITION : FEEC ===> X=-XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0   425
C    Z=ZOFFFECC-INSE_GATEDEPTH/2.0 <===                                   425
         CALL AGSRESET                                                    427
         AG_X = -XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0                         427
         AG_Y = 0                                                         427
         AG_Z = ZOFFFECC-INSE_GATEDEPTH/2.0                               427
         AG_TITLE = 'FEEC'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'X_Y_Z'                                             427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Geant POSITION : FEEC ===> X=-XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0   427
C    Z=ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/2.0 <===      427
         CALL AGSRESET                                                    429
         AG_X = -XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0                         429
         AG_Y = 0                                                         429
         AG_Z = ZOFFFECC-INSE_GATEDEPTH-INSE_GAPDEPTH-INSE_GATEDEPTH/     429
     *   2.0                                                              429
         AG_TITLE = 'FEEC'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'X_Y_Z'                                             429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Geant POSITION : FEEC ===> X=-XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0 Y=0   429
C    Z=ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0 <=  429
C    ==                                                                   429
         CALL AGSRESET                                                    431
         AG_X = -XSHIFT-(XOFFFEDC-FBXD_WIDTH)/2.0                         431
         AG_Y = 0                                                         431
         AG_Z =                                                           431
     *   ZOFFFECC-2.0*(INSE_GATEDEPTH+INSE_GAPDEPTH)-INSE_GATEDEPTH/2.0   431
         AG_TITLE = 'FEEC'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'X_Y_Z'                                             431
         CALL AXPOSITION                                                  431
C    *                                                                    431
         ENDIF                                                            431
         ENDIF                                                            433
5031  CALL AGSPOPB                                                        434
      GO TO BLOCK_FBOX, (5030 ,5040)                                      434
C End of Local Procedure                                                  434
C ----------------------------------------------------------------------  435
C ------------ FTOW                                                       435
C Begin Local Procedure FTOW                                              436
C FTOW IS ONE PBG TOWER                                                   436
5050  CONTINUE                                                            436
         AG_TITLE='FTOW'                                                  436
         AG_MODULE=MODULE                                                 436
         AG_MARK='V00'                                                    436
         CALL AGSPUSH                                                     436
         AG_MODULE=MODULE                                                 436
C    *                                                                    436
C    Geant MATERIAL : AIR ===> inherited arguments used <===              437
         CALL AGSRESET                                                    437
         AG_TITLE = 'AIR'                                                 437
         AG_EXNAME = 'MATERIAL'                                           437
         AG_PARLIST = 'NONE'                                              437
         CALL AXMATERIAL                                                  437
C    *                                                                    437
C    Geant ATTRIBUTE : FTOW ===> SEEN=1 COLO=2 <===                       437
         CALL AGSRESET                                                    438
         AG_SEEN = 1                                                      438
         AG_COLO = 2                                                      438
         AG_TITLE = 'FTOW'                                                438
         AG_EXNAME = 'ATTRIBUTE'                                          438
         AG_PARLIST = 'SEEN_COLO'                                         438
         CALL AXATTRIBUTE                                                 438
C    *                                                                    438
C    Geant SHAPE : BOX ===> DX=WID/2.0 DY=WID/2.0 DZ=ZTOT <===            438
         CALL AGSRESET                                                    439
         AG_DX = WID/2.0                                                  439
         AG_DY = WID/2.0                                                  439
         AG_DZ = ZTOT                                                     439
         AG_TITLE = 'BOX'                                                 439
         AG_EXNAME = 'SHAPE'                                              439
         AG_PARLIST = 'DX_DY_DZ'                                          439
         CALL AXSHAPE                                                     439
C    *                                                                    439
C    Create FWAL :                                                        440
C    Call To Local Procedure FWAL                                         440
         ASSIGN 5370 TO BLOCK_FWAL                                        440
         GO TO  5360                                                      440
5370     CONTINUE                                                         440
C    *                                                                    440
C    Geant POSITION : FWAL ===> Z=-ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0 <=  441
C    ==                                                                   441
         CALL AGSRESET                                                    441
         AG_Z = -ZTOT+(FLGG_ALTHICK+FLGG_DEPTH)/2.0                       441
         AG_TITLE = 'FWAL'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'Z'                                                 441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Create FUMT :                                                        442
C    Call To Local Procedure FUMT                                         442
         ASSIGN 5390 TO BLOCK_FUMT                                        442
         GO TO  5380                                                      442
5390     CONTINUE                                                         442
C    *                                                                    442
C    Geant POSITION : FUMT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUM  443
C    ETDZ/2.0 <===                                                        443
         CALL AGSRESET                                                    443
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_MUMETDZ/2.0            443
         AG_TITLE = 'FUMT'                                                443
         AG_EXNAME = 'POSITION'                                           443
         AG_PARLIST = 'Z'                                                 443
         CALL AXPOSITION                                                  443
C    *                                                                    443
C    Create FPCT :                                                        444
C    Call To Local Procedure FPCT                                         444
         ASSIGN 5410 TO BLOCK_FPCT                                        444
         GO TO  5400                                                      444
5410     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : FPCT ===> Z=-ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHC  445
C    ATHDZ/2.0 <===                                                       445
         CALL AGSRESET                                                    445
         AG_Z = -ZTOT+FLGG_ALTHICK+FLGG_DEPTH+FLGG_PHCATHDZ/2.0           445
         AG_TITLE = 'FPCT'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'Z'                                                 445
         CALL AXPOSITION                                                  445
C    *                                                                    445
5051  CALL AGSPOPB                                                        446
      GO TO BLOCK_FTOW, (5050 ,5060,5130,5150)                            446
C End of Local Procedure                                                  446
C ----------------------------------------------------------------------  447
C ------------ FWAL                                                       447
C Begin Local Procedure FWAL                                              448
C FWAL IS ALMUNUM WRAPPER                                                 448
5360  CONTINUE                                                            448
         AG_TITLE='FWAL'                                                  448
         AG_MODULE=MODULE                                                 448
         AG_MARK='V00'                                                    448
         CALL AGSPUSH                                                     448
         AG_MODULE=MODULE                                                 448
C    *                                                                    448
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        449
         CALL AGSRESET                                                    449
         AG_TITLE = 'ALUMINIUM'                                           449
         AG_EXNAME = 'MATERIAL'                                           449
         AG_PARLIST = 'NONE'                                              449
         CALL AXMATERIAL                                                  449
C    *                                                                    449
C    Geant ATTRIBUTE : FWAL ===> SEEN=1 COLO=3 <===                       449
         CALL AGSRESET                                                    450
         AG_SEEN = 1                                                      450
         AG_COLO = 3                                                      450
         AG_TITLE = 'FWAL'                                                450
         AG_EXNAME = 'ATTRIBUTE'                                          450
         AG_PARLIST = 'SEEN_COLO'                                         450
         CALL AXATTRIBUTE                                                 450
C    *                                                                    450
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0+FLGG_ALTHICK, DY=FLGG_WIDT  450
C    H/2.0+FLGG_ALTHICK DZ=FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0 <===           450
         CALL AGSRESET                                                    452
         AG_DX = FLGG_WIDTH/2.0+FLGG_ALTHICK                              452
         AG_DY = FLGG_WIDTH/2.0+FLGG_ALTHICK                              452
         AG_DZ = FLGG_DEPTH/2.0+FLGG_ALTHICK/2.0                          452
         AG_TITLE = 'BOX'                                                 452
         AG_EXNAME = 'SHAPE'                                              452
         AG_PARLIST = 'DX_DY_DZ'                                          452
         CALL AXSHAPE                                                     452
C    *                                                                    452
C    Check FMCG_CHKVSIM==1                                                452
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      453
         CALL GSCKOV( AG_IMED,N,E,ABSCO_ALM,EFFIC_ALL,RINDEX_ALM )        454
         ENDIF                                                            455
C    Create FLGR :                                                        456
C    Call To Local Procedure FLGR                                         456
         ASSIGN 5430 TO BLOCK_FLGR                                        456
         GO TO  5420                                                      456
5430     CONTINUE                                                         456
C    *                                                                    456
C    Geant POSITION : FLGR ===> Z=+FLGG_ALTHICK/2.0 <===                  457
         CALL AGSRESET                                                    457
         AG_Z = +FLGG_ALTHICK/2.0                                         457
         AG_TITLE = 'FLGR'                                                457
         AG_EXNAME = 'POSITION'                                           457
         AG_PARLIST = 'Z'                                                 457
         CALL AXPOSITION                                                  457
C    *                                                                    457
5361  CALL AGSPOPB                                                        458
      GO TO BLOCK_FWAL, (5360 ,5370)                                      458
C End of Local Procedure                                                  458
C ----------------------------------------------------------------------  459
C ------------ FLGR                                                       459
C Begin Local Procedure FLGR                                              460
C FLGR IS LEAD GLASS DETECTOR                                             460
5420  CONTINUE                                                            460
         AG_TITLE='FLGR'                                                  460
         AG_MODULE=MODULE                                                 460
         AG_MARK='V00'                                                    460
         CALL AGSPUSH                                                     460
         AG_MODULE=MODULE                                                 460
C    *                                                                    460
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.60712 <===                461
         CALL AGSRESET                                                    461
         AG_A = 207.19                                                    461
         AG_Z = 82                                                        461
         AG_W = .60712                                                    461
         AG_TITLE = 'PB'                                                  461
         AG_EXNAME = 'COMPONENT'                                          461
         AG_PARLIST = 'A_Z_W'                                             461
         CALL AXCOMPONENT                                                 461
C    *                                                                    461
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.02324 <===                 461
         CALL AGSRESET                                                    462
         AG_A = 39.102                                                    462
         AG_Z = 19                                                        462
         AG_W = .02324                                                    462
         AG_TITLE = 'K'                                                   462
         AG_EXNAME = 'COMPONENT'                                          462
         AG_PARLIST = 'A_Z_W'                                             462
         CALL AXCOMPONENT                                                 462
C    *                                                                    462
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.14771 <===                462
         CALL AGSRESET                                                    463
         AG_A = 28.088                                                    463
         AG_Z = 14                                                        463
         AG_W = .14771                                                    463
         AG_TITLE = 'SI'                                                  463
         AG_EXNAME = 'COMPONENT'                                          463
         AG_PARLIST = 'A_Z_W'                                             463
         CALL AXCOMPONENT                                                 463
C    *                                                                    463
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.22041 <===                  463
         CALL AGSRESET                                                    464
         AG_A = 15.999                                                    464
         AG_Z = 8                                                         464
         AG_W = .22041                                                    464
         AG_TITLE = 'O'                                                   464
         AG_EXNAME = 'COMPONENT'                                          464
         AG_PARLIST = 'A_Z_W'                                             464
         CALL AXCOMPONENT                                                 464
C    *                                                                    464
C    Geant COMPONENT : AS ===> A=74.922 Z=33 W=.00152 <===                464
         CALL AGSRESET                                                    465
         AG_A = 74.922                                                    465
         AG_Z = 33                                                        465
         AG_W = .00152                                                    465
         AG_TITLE = 'AS'                                                  465
         AG_EXNAME = 'COMPONENT'                                          465
         AG_PARLIST = 'A_Z_W'                                             465
         CALL AXCOMPONENT                                                 465
C    *                                                                    465
C    Geant MIXTURE : PBG ===> DENS=FLGM_DENSITY <===                      465
         CALL AGSRESET                                                    466
         AG_DENS = FLGM_DENSITY                                           466
         AG_TITLE = 'PBG'                                                 466
         AG_EXNAME = 'MIXTURE'                                            466
         AG_PARLIST = 'DENS'                                              466
         CALL AXMIXTURE                                                   466
C    *                                                                    466
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           466
         CALL AGSRESET                                                    467
         AG_ISVOL = 1                                                     467
         AG_TITLE = 'LEADGLASS'                                           467
         AG_EXNAME = 'MEDIUM'                                             467
         AG_PARLIST = 'ISVOL'                                             467
         CALL AXMEDIUM                                                    467
C    *                                                                    467
C    Geant ATTRIBUTE : FLGR ===> SEEN=1 COLO=4 <===                       467
         CALL AGSRESET                                                    468
         AG_SEEN = 1                                                      468
         AG_COLO = 4                                                      468
         AG_TITLE = 'FLGR'                                                468
         AG_EXNAME = 'ATTRIBUTE'                                          468
         AG_PARLIST = 'SEEN_COLO'                                         468
         CALL AXATTRIBUTE                                                 468
C    *                                                                    468
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0 DY=FLGG_WIDTH/2.0, DZ=FLGG  468
C    _DEPTH/2.0 <===                                                      468
         CALL AGSRESET                                                    470
         AG_DX = FLGG_WIDTH/2.0                                           470
         AG_DY = FLGG_WIDTH/2.0                                           470
         AG_DZ = FLGG_DEPTH/2.0                                           470
         AG_TITLE = 'BOX'                                                 470
         AG_EXNAME = 'SHAPE'                                              470
         AG_PARLIST = 'DX_DY_DZ'                                          470
         CALL AXSHAPE                                                     470
C    *                                                                    470
C    Defining Hits: ELOS:0:(0,50)                                         470
         AG_TITLE = 'FLGR'                                                471
         AG_EXNAME = 'HITS'                                               471
         AG_TYPE=AG_MAGIC                                                 471
         CALL AGSDETV('FLGR')                                             471
C    *                                                                    471
         AG_PARLIST='ELOS'                                                471
         AG_TITLE = 'DETH'                                                471
         AG_NBITS = 0                                                     471
         AG_BIN=AG_MAGIC                                                  471
         AG_OPTION='*'                                                    471
         AG_FHMIN = 0                                                     471
         AG_FHMAX = 50                                                    471
         CALL AGSDETH (0)                                                 471
C    *                                                                    471
         CALL AGSDETU('FLGR')                                             471
C    *                                                                    471
C    Check FMCG_CHKVSIM==1                                                472
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      472
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PBG,EFFIC_ALL,RINDEX_PBG )        473
         ENDIF                                                            474
5421  CALL AGSPOPB                                                        475
      GO TO BLOCK_FLGR, (5420 ,5430)                                      475
C End of Local Procedure                                                  475
C ----------------------------------------------------------------------  476
C ------------ FLXF                                                       476
C Begin Local Procedure FLXF                                              477
C FLXF IS LEAD GLASS DETECTOR                                             477
5220  CONTINUE                                                            477
         AG_TITLE='FLXF'                                                  477
         AG_MODULE=MODULE                                                 477
         AG_MARK='V00'                                                    477
         CALL AGSPUSH                                                     477
         AG_MODULE=MODULE                                                 477
C    *                                                                    477
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.41774 <===                478
         CALL AGSRESET                                                    478
         AG_A = 207.19                                                    478
         AG_Z = 82                                                        478
         AG_W = .41774                                                    478
         AG_TITLE = 'PB'                                                  478
         AG_EXNAME = 'COMPONENT'                                          478
         AG_PARLIST = 'A_Z_W'                                             478
         CALL AXCOMPONENT                                                 478
C    *                                                                    478
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.04151 <===                 478
         CALL AGSRESET                                                    479
         AG_A = 39.102                                                    479
         AG_Z = 19                                                        479
         AG_W = .04151                                                    479
         AG_TITLE = 'K'                                                   479
         AG_EXNAME = 'COMPONENT'                                          479
         AG_PARLIST = 'A_Z_W'                                             479
         CALL AXCOMPONENT                                                 479
C    *                                                                    479
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.21047 <===                479
         CALL AGSRESET                                                    480
         AG_A = 28.088                                                    480
         AG_Z = 14                                                        480
         AG_W = .21047                                                    480
         AG_TITLE = 'SI'                                                  480
         AG_EXNAME = 'COMPONENT'                                          480
         AG_PARLIST = 'A_Z_W'                                             480
         CALL AXCOMPONENT                                                 480
C    *                                                                    480
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.29330 <===                  480
         CALL AGSRESET                                                    481
         AG_A = 15.999                                                    481
         AG_Z = 8                                                         481
         AG_W = .29330                                                    481
         AG_TITLE = 'O'                                                   481
         AG_EXNAME = 'COMPONENT'                                          481
         AG_PARLIST = 'A_Z_W'                                             481
         CALL AXCOMPONENT                                                 481
C    *                                                                    481
C    Geant COMPONENT : NA ===> A=22.990 Z=11 W=.03710 <===                481
         CALL AGSRESET                                                    482
         AG_A = 22.990                                                    482
         AG_Z = 11                                                        482
         AG_W = .03710                                                    482
         AG_TITLE = 'NA'                                                  482
         AG_EXNAME = 'COMPONENT'                                          482
         AG_PARLIST = 'A_Z_W'                                             482
         CALL AXCOMPONENT                                                 482
C    *                                                                    482
C    Geant MIXTURE : F2 ===> DENS=FLGM_DENSITY <===                       482
         CALL AGSRESET                                                    483
         AG_DENS = FLGM_DENSITY                                           483
         AG_TITLE = 'F2'                                                  483
         AG_EXNAME = 'MIXTURE'                                            483
         AG_PARLIST = 'DENS'                                              483
         CALL AXMIXTURE                                                   483
C    *                                                                    483
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           483
         CALL AGSRESET                                                    484
         AG_ISVOL = 1                                                     484
         AG_TITLE = 'LEADGLASS'                                           484
         AG_EXNAME = 'MEDIUM'                                             484
         AG_PARLIST = 'ISVOL'                                             484
         CALL AXMEDIUM                                                    484
C    *                                                                    484
C    Geant ATTRIBUTE : FLXF ===> SEEN=1 COLO=4 <===                       484
         CALL AGSRESET                                                    485
         AG_SEEN = 1                                                      485
         AG_COLO = 4                                                      485
         AG_TITLE = 'FLXF'                                                485
         AG_EXNAME = 'ATTRIBUTE'                                          485
         AG_PARLIST = 'SEEN_COLO'                                         485
         CALL AXATTRIBUTE                                                 485
C    *                                                                    485
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0 DY=FLGG_WIDTH/2.0, DZ=FLGG  485
C    _DEPTH/2.0 <===                                                      485
         CALL AGSRESET                                                    487
         AG_DX = FLGG_WIDTH/2.0                                           487
         AG_DY = FLGG_WIDTH/2.0                                           487
         AG_DZ = FLGG_DEPTH/2.0                                           487
         AG_TITLE = 'BOX'                                                 487
         AG_EXNAME = 'SHAPE'                                              487
         AG_PARLIST = 'DX_DY_DZ'                                          487
         CALL AXSHAPE                                                     487
C    *                                                                    487
C    Defining Hits: ELOS:0:(0,50)                                         487
         AG_TITLE = 'FLXF'                                                488
         AG_EXNAME = 'HITS'                                               488
         AG_TYPE=AG_MAGIC                                                 488
         CALL AGSDETV('FLXF')                                             488
C    *                                                                    488
         AG_PARLIST='ELOS'                                                488
         AG_TITLE = 'DETH'                                                488
         AG_NBITS = 0                                                     488
         AG_BIN=AG_MAGIC                                                  488
         AG_OPTION='*'                                                    488
         AG_FHMIN = 0                                                     488
         AG_FHMAX = 50                                                    488
         CALL AGSDETH (0)                                                 488
C    *                                                                    488
         CALL AGSDETU('FLXF')                                             488
C    *                                                                    488
5221  CALL AGSPOPB                                                        489
      GO TO BLOCK_FLXF, (5220 ,5230)                                      489
C End of Local Procedure                                                  489
C ----------------------------------------------------------------------  490
C ------------ FALU                                                       490
C Begin Local Procedure FALU                                              491
C FALU IS ALUMINIUM BASE CELL                                             491
5200  CONTINUE                                                            491
         AG_TITLE='FALU'                                                  491
         AG_MODULE=MODULE                                                 491
         AG_MARK='V00'                                                    491
         CALL AGSPUSH                                                     491
         AG_MODULE=MODULE                                                 491
C    *                                                                    491
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        492
         CALL AGSRESET                                                    492
         AG_TITLE = 'ALUMINIUM'                                           492
         AG_EXNAME = 'MATERIAL'                                           492
         AG_PARLIST = 'NONE'                                              492
         CALL AXMATERIAL                                                  492
C    *                                                                    492
C    Geant ATTRIBUTE : FALU ===> SEEN=1 COLO=1 <===                       492
         CALL AGSRESET                                                    493
         AG_SEEN = 1                                                      493
         AG_COLO = 1                                                      493
         AG_TITLE = 'FALU'                                                493
         AG_EXNAME = 'ATTRIBUTE'                                          493
         AG_PARLIST = 'SEEN_COLO'                                         493
         CALL AXATTRIBUTE                                                 493
C    *                                                                    493
C    Geant SHAPE : BOX ===> DX=FLGG_WIDTH/2.0 DY=FLGG_WIDTH/2.0, DZ=FLGG  493
C    _DEPTH/2.0 <===                                                      493
         CALL AGSRESET                                                    495
         AG_DX = FLGG_WIDTH/2.0                                           495
         AG_DY = FLGG_WIDTH/2.0                                           495
         AG_DZ = FLGG_DEPTH/2.0                                           495
         AG_TITLE = 'BOX'                                                 495
         AG_EXNAME = 'SHAPE'                                              495
         AG_PARLIST = 'DX_DY_DZ'                                          495
         CALL AXSHAPE                                                     495
C    *                                                                    495
5201  CALL AGSPOPB                                                        496
      GO TO BLOCK_FALU, (5200 ,5210)                                      496
C End of Local Procedure                                                  496
C ----------------------------------------------------------------------  497
C ------------ FBAS                                                       497
C Begin Local Procedure FBAS                                              498
C FBAS IS STEEL BASE PLATE                                                498
5240  CONTINUE                                                            498
         AG_TITLE='FBAS'                                                  498
         AG_MODULE=MODULE                                                 498
         AG_MARK='V00'                                                    498
         CALL AGSPUSH                                                     498
         AG_MODULE=MODULE                                                 498
C    *                                                                    498
C    Geant MATERIAL : IRON ===> inherited arguments used <===             499
         CALL AGSRESET                                                    499
         AG_TITLE = 'IRON'                                                499
         AG_EXNAME = 'MATERIAL'                                           499
         AG_PARLIST = 'NONE'                                              499
         CALL AXMATERIAL                                                  499
C    *                                                                    499
C    Geant ATTRIBUTE : FBAS ===> SEEN=1 COLO=1 <===                       499
         CALL AGSRESET                                                    500
         AG_SEEN = 1                                                      500
         AG_COLO = 1                                                      500
         AG_TITLE = 'FBAS'                                                500
         AG_EXNAME = 'ATTRIBUTE'                                          500
         AG_PARLIST = 'SEEN_COLO'                                         500
         CALL AXATTRIBUTE                                                 500
C    *                                                                    500
C    Geant SHAPE : BOX ===> DX=FBXD_WIDTH/2.0 DY=BASEWIDTH/2.0, DZ=INSE_  500
C    DEPTH/2.0 <===                                                       500
         CALL AGSRESET                                                    502
         AG_DX = FBXD_WIDTH/2.0                                           502
         AG_DY = BASEWIDTH/2.0                                            502
         AG_DZ = INSE_DEPTH/2.0                                           502
         AG_TITLE = 'BOX'                                                 502
         AG_EXNAME = 'SHAPE'                                              502
         AG_PARLIST = 'DX_DY_DZ'                                          502
         CALL AXSHAPE                                                     502
C    *                                                                    502
5241  CALL AGSPOPB                                                        503
      GO TO BLOCK_FBAS, (5240 ,5250)                                      503
C End of Local Procedure                                                  503
C ----------------------------------------------------------------------  504
C ------------ FENC                                                       504
C Begin Local Procedure FENC                                              505
C FENC IS STEEL ENCLOSURE                                                 505
5260  CONTINUE                                                            505
         AG_TITLE='FENC'                                                  505
         AG_MODULE=MODULE                                                 505
         AG_MARK='V00'                                                    505
         CALL AGSPUSH                                                     505
         AG_MODULE=MODULE                                                 505
C    *                                                                    505
C    Geant MATERIAL : IRON ===> inherited arguments used <===             506
         CALL AGSRESET                                                    506
         AG_TITLE = 'IRON'                                                506
         AG_EXNAME = 'MATERIAL'                                           506
         AG_PARLIST = 'NONE'                                              506
         CALL AXMATERIAL                                                  506
C    *                                                                    506
C    Geant ATTRIBUTE : FENC ===> SEEN=1 COLO=5 <===                       506
         CALL AGSRESET                                                    507
         AG_SEEN = 1                                                      507
         AG_COLO = 5                                                      507
         AG_TITLE = 'FENC'                                                507
         AG_EXNAME = 'ATTRIBUTE'                                          507
         AG_PARLIST = 'SEEN_COLO'                                         507
         CALL AXATTRIBUTE                                                 507
C    *                                                                    507
C    Geant SHAPE : BOX ===> DX=INSE_WIDTH/2.0 DY=INSE_SHEETDPT/2.0, DZ=I  507
C    NSE_DEPTH/2.0 <===                                                   507
         CALL AGSRESET                                                    509
         AG_DX = INSE_WIDTH/2.0                                           509
         AG_DY = INSE_SHEETDPT/2.0                                        509
         AG_DZ = INSE_DEPTH/2.0                                           509
         AG_TITLE = 'BOX'                                                 509
         AG_EXNAME = 'SHAPE'                                              509
         AG_PARLIST = 'DX_DY_DZ'                                          509
         CALL AXSHAPE                                                     509
C    *                                                                    509
5261  CALL AGSPOPB                                                        510
      GO TO BLOCK_FENC, (5260 ,5270)                                      510
C End of Local Procedure                                                  510
C ----------------------------------------------------------------------  511
C ------------ FEAC                                                       511
C Begin Local Procedure FEAC                                              512
C FEAC IS STEEL ENCLOSURE                                                 512
5280  CONTINUE                                                            512
         AG_TITLE='FEAC'                                                  512
         AG_MODULE=MODULE                                                 512
         AG_MARK='V00'                                                    512
         CALL AGSPUSH                                                     512
         AG_MODULE=MODULE                                                 512
C    *                                                                    512
C    Geant MATERIAL : IRON ===> inherited arguments used <===             513
         CALL AGSRESET                                                    513
         AG_TITLE = 'IRON'                                                513
         AG_EXNAME = 'MATERIAL'                                           513
         AG_PARLIST = 'NONE'                                              513
         CALL AXMATERIAL                                                  513
C    *                                                                    513
C    Geant ATTRIBUTE : FEAC ===> SEEN=1 COLO=5 <===                       513
         CALL AGSRESET                                                    514
         AG_SEEN = 1                                                      514
         AG_COLO = 5                                                      514
         AG_TITLE = 'FEAC'                                                514
         AG_EXNAME = 'ATTRIBUTE'                                          514
         AG_PARLIST = 'SEEN_COLO'                                         514
         CALL AXATTRIBUTE                                                 514
C    *                                                                    514
C    Geant SHAPE : BOX ===> DX=INSE_SHEETDPT/2.0 DY=(INSE_HEIGHT-2.0*INS  514
C    E_SHEETDPT)/2.0, DZ=INSE_DEPTH/2.0 <===                              514
         CALL AGSRESET                                                    516
         AG_DX = INSE_SHEETDPT/2.0                                        516
         AG_DY = (INSE_HEIGHT-2.0*INSE_SHEETDPT)/2.0                      516
         AG_DZ = INSE_DEPTH/2.0                                           516
         AG_TITLE = 'BOX'                                                 516
         AG_EXNAME = 'SHAPE'                                              516
         AG_PARLIST = 'DX_DY_DZ'                                          516
         CALL AXSHAPE                                                     516
C    *                                                                    516
C    Create FEBC :                                                        517
C    Call To Local Procedure FEBC                                         517
         ASSIGN 5450 TO BLOCK_FEBC                                        517
         GO TO  5440                                                      517
5450     CONTINUE                                                         517
C    *                                                                    517
C    Geant POSITION : FEBC ===> X=0.0 Y=0 Z=-INSE_DEPTH/2.0+INSE_HOLEDEP  518
C    TH/2.0+INSE_HOLEGAP <===                                             518
         CALL AGSRESET                                                    518
         AG_X = 0.0                                                       518
         AG_Y = 0                                                         518
         AG_Z = -INSE_DEPTH/2.0+INSE_HOLEDEPTH/2.0+INSE_HOLEGAP           518
         AG_TITLE = 'FEBC'                                                518
         AG_EXNAME = 'POSITION'                                           518
         AG_PARLIST = 'X_Y_Z'                                             518
         CALL AXPOSITION                                                  518
C    *                                                                    518
5281  CALL AGSPOPB                                                        519
      GO TO BLOCK_FEAC, (5280 ,5290)                                      519
C End of Local Procedure                                                  519
C ----------------------------------------------------------------------  520
C ------------ FEBC                                                       520
C Begin Local Procedure FEBC                                              521
C FEBC IS AIR SQUARE HOLE                                                 521
5440  CONTINUE                                                            521
         AG_TITLE='FEBC'                                                  521
         AG_MODULE=MODULE                                                 521
         AG_MARK='V00'                                                    521
         CALL AGSPUSH                                                     521
         AG_MODULE=MODULE                                                 521
C    *                                                                    521
C    Geant MATERIAL : AIR ===> inherited arguments used <===              522
         CALL AGSRESET                                                    522
         AG_TITLE = 'AIR'                                                 522
         AG_EXNAME = 'MATERIAL'                                           522
         AG_PARLIST = 'NONE'                                              522
         CALL AXMATERIAL                                                  522
C    *                                                                    522
C    Geant ATTRIBUTE : FEBC ===> SEEN=1 COLO=5 <===                       522
         CALL AGSRESET                                                    523
         AG_SEEN = 1                                                      523
         AG_COLO = 5                                                      523
         AG_TITLE = 'FEBC'                                                523
         AG_EXNAME = 'ATTRIBUTE'                                          523
         AG_PARLIST = 'SEEN_COLO'                                         523
         CALL AXATTRIBUTE                                                 523
C    *                                                                    523
C    Geant SHAPE : BOX ===> DX=INSE_SHEETDPT/2.0 DY=INSE_HOLEHEIGHT/2.0,  523
C     DZ=INSE_HOLEDEPTH/2.0 <===                                          523
         CALL AGSRESET                                                    525
         AG_DX = INSE_SHEETDPT/2.0                                        525
         AG_DY = INSE_HOLEHEIGHT/2.0                                      525
         AG_DZ = INSE_HOLEDEPTH/2.0                                       525
         AG_TITLE = 'BOX'                                                 525
         AG_EXNAME = 'SHAPE'                                              525
         AG_PARLIST = 'DX_DY_DZ'                                          525
         CALL AXSHAPE                                                     525
C    *                                                                    525
5441  CALL AGSPOPB                                                        526
      GO TO BLOCK_FEBC, (5440 ,5450)                                      526
C End of Local Procedure                                                  526
C ----------------------------------------------------------------------  527
C ------------ FECC                                                       527
C Begin Local Procedure FECC                                              528
C FECC IS STEEL DISTANCER                                                 528
5300  CONTINUE                                                            528
         AG_TITLE='FECC'                                                  528
         AG_MODULE=MODULE                                                 528
         AG_MARK='V00'                                                    528
         CALL AGSPUSH                                                     528
         AG_MODULE=MODULE                                                 528
C    *                                                                    528
C    Geant MATERIAL : IRON ===> inherited arguments used <===             529
         CALL AGSRESET                                                    529
         AG_TITLE = 'IRON'                                                529
         AG_EXNAME = 'MATERIAL'                                           529
         AG_PARLIST = 'NONE'                                              529
         CALL AXMATERIAL                                                  529
C    *                                                                    529
C    Geant ATTRIBUTE : FECC ===> SEEN=1 COLO=5 <===                       529
         CALL AGSRESET                                                    530
         AG_SEEN = 1                                                      530
         AG_COLO = 5                                                      530
         AG_TITLE = 'FECC'                                                530
         AG_EXNAME = 'ATTRIBUTE'                                          530
         AG_PARLIST = 'SEEN_COLO'                                         530
         CALL AXATTRIBUTE                                                 530
C    *                                                                    530
C    Geant SHAPE : BOX ===> DX=INSE_SHEETDPT/2.0 DY=INSE_GAPHEIGHT/2.0,   530
C    DZ=INSE_GAPDEPTH/2.0 <===                                            530
         CALL AGSRESET                                                    532
         AG_DX = INSE_SHEETDPT/2.0                                        532
         AG_DY = INSE_GAPHEIGHT/2.0                                       532
         AG_DZ = INSE_GAPDEPTH/2.0                                        532
         AG_TITLE = 'BOX'                                                 532
         AG_EXNAME = 'SHAPE'                                              532
         AG_PARLIST = 'DX_DY_DZ'                                          532
         CALL AXSHAPE                                                     532
C    *                                                                    532
5301  CALL AGSPOPB                                                        533
      GO TO BLOCK_FECC, (5300 ,5310)                                      533
C End of Local Procedure                                                  533
C ----------------------------------------------------------------------  534
C ------------ FEDC                                                       534
C Begin Local Procedure FEDC                                              535
C FEDC IS STEEL ENCLOSURE PART ON SOUTH                                   535
5320  CONTINUE                                                            535
         AG_TITLE='FEDC'                                                  535
         AG_MODULE=MODULE                                                 535
         AG_MARK='V00'                                                    535
         CALL AGSPUSH                                                     535
         AG_MODULE=MODULE                                                 535
C    *                                                                    535
C    Geant MATERIAL : IRON ===> inherited arguments used <===             536
         CALL AGSRESET                                                    536
         AG_TITLE = 'IRON'                                                536
         AG_EXNAME = 'MATERIAL'                                           536
         AG_PARLIST = 'NONE'                                              536
         CALL AXMATERIAL                                                  536
C    *                                                                    536
C    Geant ATTRIBUTE : FEDC ===> SEEN=1 COLO=5 <===                       536
         CALL AGSRESET                                                    537
         AG_SEEN = 1                                                      537
         AG_COLO = 5                                                      537
         AG_TITLE = 'FEDC'                                                537
         AG_EXNAME = 'ATTRIBUTE'                                          537
         AG_PARLIST = 'SEEN_COLO'                                         537
         CALL AXATTRIBUTE                                                 537
C    *                                                                    537
C    Geant SHAPE : BOX ===> DX=(XOFFFEDC)/2.0 DY=(INSE_HEIGHT-2.0*INSE_S  537
C    HEETDPT)/2.0, DZ=INSE_GATEDEPTH/2.0 <===                             537
         CALL AGSRESET                                                    539
         AG_DX = (XOFFFEDC)/2.0                                           539
         AG_DY = (INSE_HEIGHT-2.0*INSE_SHEETDPT)/2.0                      539
         AG_DZ = INSE_GATEDEPTH/2.0                                       539
         AG_TITLE = 'BOX'                                                 539
         AG_EXNAME = 'SHAPE'                                              539
         AG_PARLIST = 'DX_DY_DZ'                                          539
         CALL AXSHAPE                                                     539
C    *                                                                    539
C    Create FERC :                                                        540
C    Call To Local Procedure FERC                                         540
         ASSIGN 5470 TO BLOCK_FERC                                        540
         GO TO  5460                                                      540
5470     CONTINUE                                                         540
C    *                                                                    540
C    Geant POSITION : FERC ===> X=-(XOFFFEDC)/2.0 Y=0 Z=0.0 <===          541
         CALL AGSRESET                                                    541
         AG_X = -(XOFFFEDC)/2.0                                           541
         AG_Y = 0                                                         541
         AG_Z = 0.0                                                       541
         AG_TITLE = 'FERC'                                                541
         AG_EXNAME = 'POSITION'                                           541
         AG_PARLIST = 'X_Y_Z'                                             541
         CALL AXPOSITION                                                  541
C    *                                                                    541
C    Create FESC :                                                        542
C    Call To Local Procedure FESC                                         542
         ASSIGN 5490 TO BLOCK_FESC                                        542
         GO TO  5480                                                      542
5490     CONTINUE                                                         542
C    *                                                                    542
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.  544
C    0) Y=INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                             544
         CALL AGSRESET                                                    544
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.0)                  544
         AG_Y = INSE_RA*SIN(PI*5.0/12.0)                                  544
         AG_Z = 0.0                                                       544
         AG_TITLE = 'FESC'                                                544
         AG_EXNAME = 'POSITION'                                           544
         AG_PARLIST = 'X_Y_Z'                                             544
         CALL AXPOSITION                                                  544
C    *                                                                    544
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0) Y=  544
C    INSE_RA*SIN(PI/4.0) Z=0.0 <===                                       544
         CALL AGSRESET                                                    545
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0)                       545
         AG_Y = INSE_RA*SIN(PI/4.0)                                       545
         AG_Z = 0.0                                                       545
         AG_TITLE = 'FESC'                                                545
         AG_EXNAME = 'POSITION'                                           545
         AG_PARLIST = 'X_Y_Z'                                             545
         CALL AXPOSITION                                                  545
C    *                                                                    545
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0) Y  545
C    =INSE_RA*SIN(PI/12.0) Z=0.0 <===                                     545
         CALL AGSRESET                                                    547
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0)                      547
         AG_Y = INSE_RA*SIN(PI/12.0)                                      547
         AG_Z = 0.0                                                       547
         AG_TITLE = 'FESC'                                                547
         AG_EXNAME = 'POSITION'                                           547
         AG_PARLIST = 'X_Y_Z'                                             547
         CALL AXPOSITION                                                  547
C    *                                                                    547
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0) Y  547
C    =-INSE_RA*SIN(PI/12.0) Z=0.0 <===                                    547
         CALL AGSRESET                                                    549
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/12.0)                      549
         AG_Y = -INSE_RA*SIN(PI/12.0)                                     549
         AG_Z = 0.0                                                       549
         AG_TITLE = 'FESC'                                                549
         AG_EXNAME = 'POSITION'                                           549
         AG_PARLIST = 'X_Y_Z'                                             549
         CALL AXPOSITION                                                  549
C    *                                                                    549
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0) Y=  549
C    -INSE_RA*SIN(PI/4.0) Z=0.0 <===                                      549
         CALL AGSRESET                                                    551
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI/4.0)                       551
         AG_Y = -INSE_RA*SIN(PI/4.0)                                      551
         AG_Z = 0.0                                                       551
         AG_TITLE = 'FESC'                                                551
         AG_EXNAME = 'POSITION'                                           551
         AG_PARLIST = 'X_Y_Z'                                             551
         CALL AXPOSITION                                                  551
C    *                                                                    551
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.  551
C    0) Y=-INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                            551
         CALL AGSRESET                                                    553
         AG_X = -(XOFFFEDC)/2.0+INSE_RA*COS(PI*5.0/12.0)                  553
         AG_Y = -INSE_RA*SIN(PI*5.0/12.0)                                 553
         AG_Z = 0.0                                                       553
         AG_TITLE = 'FESC'                                                553
         AG_EXNAME = 'POSITION'                                           553
         AG_PARLIST = 'X_Y_Z'                                             553
         CALL AXPOSITION                                                  553
C    *                                                                    553
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0) Y=  553
C    INSE_RB*SIN(PI/4.0) Z=0.0 <===                                       553
         CALL AGSRESET                                                    554
         AG_X = -(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0)                       554
         AG_Y = INSE_RB*SIN(PI/4.0)                                       554
         AG_Z = 0.0                                                       554
         AG_TITLE = 'FESC'                                                554
         AG_EXNAME = 'POSITION'                                           554
         AG_PARLIST = 'X_Y_Z'                                             554
         CALL AXPOSITION                                                  554
C    *                                                                    554
C    Geant POSITION : FESC ===> X=-(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0) Y=  554
C    -INSE_RB*SIN(PI/4.0) Z=0.0 <===                                      554
         CALL AGSRESET                                                    556
         AG_X = -(XOFFFEDC)/2.0+INSE_RB*COS(PI/4.0)                       556
         AG_Y = -INSE_RB*SIN(PI/4.0)                                      556
         AG_Z = 0.0                                                       556
         AG_TITLE = 'FESC'                                                556
         AG_EXNAME = 'POSITION'                                           556
         AG_PARLIST = 'X_Y_Z'                                             556
         CALL AXPOSITION                                                  556
C    *                                                                    556
5321  CALL AGSPOPB                                                        557
      GO TO BLOCK_FEDC, (5320 ,5330)                                      557
C End of Local Procedure                                                  557
C ----------------------------------------------------------------------  558
C ------------ FEEC                                                       558
C Begin Local Procedure FEEC                                              559
C FEEC IS STEEL ENCLOSURE PART ON NORTH                                   559
5340  CONTINUE                                                            559
         AG_TITLE='FEEC'                                                  559
         AG_MODULE=MODULE                                                 559
         AG_MARK='V00'                                                    559
         CALL AGSPUSH                                                     559
         AG_MODULE=MODULE                                                 559
C    *                                                                    559
C    Geant MATERIAL : IRON ===> inherited arguments used <===             560
         CALL AGSRESET                                                    560
         AG_TITLE = 'IRON'                                                560
         AG_EXNAME = 'MATERIAL'                                           560
         AG_PARLIST = 'NONE'                                              560
         CALL AXMATERIAL                                                  560
C    *                                                                    560
C    Geant ATTRIBUTE : FEEC ===> SEEN=1 COLO=5 <===                       560
         CALL AGSRESET                                                    561
         AG_SEEN = 1                                                      561
         AG_COLO = 5                                                      561
         AG_TITLE = 'FEEC'                                                561
         AG_EXNAME = 'ATTRIBUTE'                                          561
         AG_PARLIST = 'SEEN_COLO'                                         561
         CALL AXATTRIBUTE                                                 561
C    *                                                                    561
C    Geant SHAPE : BOX ===> DX=(XOFFFEDC)/2.0 DY=(INSE_HEIGHT-2.0*INSE_S  561
C    HEETDPT)/2.0, DZ=INSE_GATEDEPTH/2.0 <===                             561
         CALL AGSRESET                                                    563
         AG_DX = (XOFFFEDC)/2.0                                           563
         AG_DY = (INSE_HEIGHT-2.0*INSE_SHEETDPT)/2.0                      563
         AG_DZ = INSE_GATEDEPTH/2.0                                       563
         AG_TITLE = 'BOX'                                                 563
         AG_EXNAME = 'SHAPE'                                              563
         AG_PARLIST = 'DX_DY_DZ'                                          563
         CALL AXSHAPE                                                     563
C    *                                                                    563
C    Create FETC :                                                        564
C    Call To Local Procedure FETC                                         564
         ASSIGN 5510 TO BLOCK_FETC                                        564
         GO TO  5500                                                      564
5510     CONTINUE                                                         564
C    *                                                                    564
C    Geant POSITION : FETC ===> X=(XOFFFEDC)/2.0 Y=0 Z=0.0 <===           565
         CALL AGSRESET                                                    565
         AG_X = (XOFFFEDC)/2.0                                            565
         AG_Y = 0                                                         565
         AG_Z = 0.0                                                       565
         AG_TITLE = 'FETC'                                                565
         AG_EXNAME = 'POSITION'                                           565
         AG_PARLIST = 'X_Y_Z'                                             565
         CALL AXPOSITION                                                  565
C    *                                                                    565
C    Create FESC :                                                        566
C    Call To Local Procedure FESC                                         566
         ASSIGN 5520 TO BLOCK_FESC                                        566
         GO TO  5480                                                      566
5520     CONTINUE                                                         566
C    *                                                                    566
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0  568
C    ) Y=INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                              568
         CALL AGSRESET                                                    568
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0)                   568
         AG_Y = INSE_RA*SIN(PI*5.0/12.0)                                  568
         AG_Z = 0.0                                                       568
         AG_TITLE = 'FESC'                                                568
         AG_EXNAME = 'POSITION'                                           568
         AG_PARLIST = 'X_Y_Z'                                             568
         CALL AXPOSITION                                                  568
C    *                                                                    568
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0) Y=I  568
C    NSE_RA*SIN(PI/4.0) Z=0.0 <===                                        568
         CALL AGSRESET                                                    569
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0)                        569
         AG_Y = INSE_RA*SIN(PI/4.0)                                       569
         AG_Z = 0.0                                                       569
         AG_TITLE = 'FESC'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'X_Y_Z'                                             569
         CALL AXPOSITION                                                  569
C    *                                                                    569
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0) Y=  569
C    INSE_RA*SIN(PI/12.0) Z=0.0 <===                                      569
         CALL AGSRESET                                                    571
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0)                       571
         AG_Y = INSE_RA*SIN(PI/12.0)                                      571
         AG_Z = 0.0                                                       571
         AG_TITLE = 'FESC'                                                571
         AG_EXNAME = 'POSITION'                                           571
         AG_PARLIST = 'X_Y_Z'                                             571
         CALL AXPOSITION                                                  571
C    *                                                                    571
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0) Y=  571
C    -INSE_RA*SIN(PI/12.0) Z=0.0 <===                                     571
         CALL AGSRESET                                                    573
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/12.0)                       573
         AG_Y = -INSE_RA*SIN(PI/12.0)                                     573
         AG_Z = 0.0                                                       573
         AG_TITLE = 'FESC'                                                573
         AG_EXNAME = 'POSITION'                                           573
         AG_PARLIST = 'X_Y_Z'                                             573
         CALL AXPOSITION                                                  573
C    *                                                                    573
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0) Y=-  573
C    INSE_RA*SIN(PI/4.0) Z=0.0 <===                                       573
         CALL AGSRESET                                                    574
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI/4.0)                        574
         AG_Y = -INSE_RA*SIN(PI/4.0)                                      574
         AG_Z = 0.0                                                       574
         AG_TITLE = 'FESC'                                                574
         AG_EXNAME = 'POSITION'                                           574
         AG_PARLIST = 'X_Y_Z'                                             574
         CALL AXPOSITION                                                  574
C    *                                                                    574
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0  574
C    ) Y=-INSE_RA*SIN(PI*5.0/12.0) Z=0.0 <===                             574
         CALL AGSRESET                                                    576
         AG_X = (XOFFFEDC)/2.0-INSE_RA*COS(PI*5.0/12.0)                   576
         AG_Y = -INSE_RA*SIN(PI*5.0/12.0)                                 576
         AG_Z = 0.0                                                       576
         AG_TITLE = 'FESC'                                                576
         AG_EXNAME = 'POSITION'                                           576
         AG_PARLIST = 'X_Y_Z'                                             576
         CALL AXPOSITION                                                  576
C    *                                                                    576
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0) Y=I  576
C    NSE_RB*SIN(PI/4.0) Z=0.0 <===                                        576
         CALL AGSRESET                                                    577
         AG_X = (XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0)                        577
         AG_Y = INSE_RB*SIN(PI/4.0)                                       577
         AG_Z = 0.0                                                       577
         AG_TITLE = 'FESC'                                                577
         AG_EXNAME = 'POSITION'                                           577
         AG_PARLIST = 'X_Y_Z'                                             577
         CALL AXPOSITION                                                  577
C    *                                                                    577
C    Geant POSITION : FESC ===> X=(XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0) Y=-  577
C    INSE_RB*SIN(PI/4.0) Z=0.0 <===                                       577
         CALL AGSRESET                                                    578
         AG_X = (XOFFFEDC)/2.0-INSE_RB*COS(PI/4.0)                        578
         AG_Y = -INSE_RB*SIN(PI/4.0)                                      578
         AG_Z = 0.0                                                       578
         AG_TITLE = 'FESC'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'X_Y_Z'                                             578
         CALL AXPOSITION                                                  578
C    *                                                                    578
5341  CALL AGSPOPB                                                        579
      GO TO BLOCK_FEEC, (5340 ,5350)                                      579
C End of Local Procedure                                                  579
C ----------------------------------------------------------------------  580
C ------------ FETC                                                       580
C Begin Local Procedure FETC                                              581
C FETC IS AIR ENCLOSURE PART                                              581
5500  CONTINUE                                                            581
         AG_TITLE='FETC'                                                  581
         AG_MODULE=MODULE                                                 581
         AG_MARK='V00'                                                    581
         CALL AGSPUSH                                                     581
         AG_MODULE=MODULE                                                 581
C    *                                                                    581
C    Geant MATERIAL : AIR ===> inherited arguments used <===              582
         CALL AGSRESET                                                    582
         AG_TITLE = 'AIR'                                                 582
         AG_EXNAME = 'MATERIAL'                                           582
         AG_PARLIST = 'NONE'                                              582
         CALL AXMATERIAL                                                  582
C    *                                                                    582
C    Geant ATTRIBUTE : FETC ===> SEEN=1 COLO=6 <===                       582
         CALL AGSRESET                                                    583
         AG_SEEN = 1                                                      583
         AG_COLO = 6                                                      583
         AG_TITLE = 'FETC'                                                583
         AG_EXNAME = 'ATTRIBUTE'                                          583
         AG_PARLIST = 'SEEN_COLO'                                         583
         CALL AXATTRIBUTE                                                 583
C    *                                                                    583
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=INSE_RMAX PHI1=90, PHI2=-90 D  583
C    Z=INSE_GATEDEPTH/2.0 <===                                            583
         CALL AGSRESET                                                    585
         AG_RMIN = 0.0                                                    585
         AG_RMAX = INSE_RMAX                                              585
         AG_PHI1 = 90                                                     585
         AG_PHI2 = -90                                                    585
         AG_DZ = INSE_GATEDEPTH/2.0                                       585
         AG_TITLE = 'TUBS'                                                585
         AG_EXNAME = 'SHAPE'                                              585
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            585
         CALL AXSHAPE                                                     585
C    *                                                                    585
5501  CALL AGSPOPB                                                        586
      GO TO BLOCK_FETC, (5500 ,5510)                                      586
C End of Local Procedure                                                  586
C ----------------------------------------------------------------------  587
C ------------ FERC                                                       587
C Begin Local Procedure FERC                                              588
C FERC IS AIR ENCLOSURE PART                                              588
5460  CONTINUE                                                            588
         AG_TITLE='FERC'                                                  588
         AG_MODULE=MODULE                                                 588
         AG_MARK='V00'                                                    588
         CALL AGSPUSH                                                     588
         AG_MODULE=MODULE                                                 588
C    *                                                                    588
C    Geant MATERIAL : AIR ===> inherited arguments used <===              589
         CALL AGSRESET                                                    589
         AG_TITLE = 'AIR'                                                 589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant ATTRIBUTE : FERC ===> SEEN=1 COLO=6 <===                       589
         CALL AGSRESET                                                    590
         AG_SEEN = 1                                                      590
         AG_COLO = 6                                                      590
         AG_TITLE = 'FERC'                                                590
         AG_EXNAME = 'ATTRIBUTE'                                          590
         AG_PARLIST = 'SEEN_COLO'                                         590
         CALL AXATTRIBUTE                                                 590
C    *                                                                    590
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=INSE_RMAX PHI1=-90, PHI2=90 D  590
C    Z=INSE_GATEDEPTH/2.0 <===                                            590
         CALL AGSRESET                                                    592
         AG_RMIN = 0.0                                                    592
         AG_RMAX = INSE_RMAX                                              592
         AG_PHI1 = -90                                                    592
         AG_PHI2 = 90                                                     592
         AG_DZ = INSE_GATEDEPTH/2.0                                       592
         AG_TITLE = 'TUBS'                                                592
         AG_EXNAME = 'SHAPE'                                              592
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            592
         CALL AXSHAPE                                                     592
C    *                                                                    592
5461  CALL AGSPOPB                                                        593
      GO TO BLOCK_FERC, (5460 ,5470)                                      593
C End of Local Procedure                                                  593
C ----------------------------------------------------------------------  594
C ------------ FESC                                                       594
C Begin Local Procedure FESC                                              595
C FESC IS AIR ENCLOSURE PART                                              595
5480  CONTINUE                                                            595
         AG_TITLE='FESC'                                                  595
         AG_MODULE=MODULE                                                 595
         AG_MARK='V00'                                                    595
         CALL AGSPUSH                                                     595
         AG_MODULE=MODULE                                                 595
C    *                                                                    595
C    Geant MATERIAL : AIR ===> inherited arguments used <===              596
         CALL AGSRESET                                                    596
         AG_TITLE = 'AIR'                                                 596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant ATTRIBUTE : FESC ===> SEEN=1 COLO=6 <===                       596
         CALL AGSRESET                                                    597
         AG_SEEN = 1                                                      597
         AG_COLO = 6                                                      597
         AG_TITLE = 'FESC'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO'                                         597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=INSE_DIAM/2.0 DZ=INSE_GATEDEP  597
C    TH/2.0 <===                                                          597
         CALL AGSRESET                                                    598
         AG_RMIN = 0.0                                                    598
         AG_RMAX = INSE_DIAM/2.0                                          598
         AG_DZ = INSE_GATEDEPTH/2.0                                       598
         AG_TITLE = 'TUBE'                                                598
         AG_EXNAME = 'SHAPE'                                              598
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      598
         CALL AXSHAPE                                                     598
C    *                                                                    598
5481  CALL AGSPOPB                                                        599
      GO TO BLOCK_FESC, (5480 ,5490,5520)                                 599
C End of Local Procedure                                                  599
C ----------------------------------------------------------------------  600
C ------------ FPCT                                                       600
C Begin Local Procedure FPCT                                              601
C FPCT IS PHOTO CATHODE                                                   601
5400  CONTINUE                                                            601
         AG_TITLE='FPCT'                                                  601
         AG_MODULE=MODULE                                                 601
         AG_MARK='V00'                                                    601
         CALL AGSPUSH                                                     601
         AG_MODULE=MODULE                                                 601
C    *                                                                    601
C    Geant MATERIAL : AIR ===> inherited arguments used <===              602
         CALL AGSRESET                                                    602
         AG_TITLE = 'AIR'                                                 602
         AG_EXNAME = 'MATERIAL'                                           602
         AG_PARLIST = 'NONE'                                              602
         CALL AXMATERIAL                                                  602
C    *                                                                    602
C    Geant MEDIUM : PHOTCATH ===> ISVOL=1 <===                            602
         CALL AGSRESET                                                    603
         AG_ISVOL = 1                                                     603
         AG_TITLE = 'PHOTCATH'                                            603
         AG_EXNAME = 'MEDIUM'                                             603
         AG_PARLIST = 'ISVOL'                                             603
         CALL AXMEDIUM                                                    603
C    *                                                                    603
C    Geant ATTRIBUTE : LPCT ===> SEEN=1 COLO=6 <===                       603
         CALL AGSRESET                                                    604
         AG_SEEN = 1                                                      604
         AG_COLO = 6                                                      604
         AG_TITLE = 'LPCT'                                                604
         AG_EXNAME = 'ATTRIBUTE'                                          604
         AG_PARLIST = 'SEEN_COLO'                                         604
         CALL AXATTRIBUTE                                                 604
C    *                                                                    604
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=FLGG_PHCATHR DZ=FLGG_PHCATHDZ/2  604
C    .0 <===                                                              604
         CALL AGSRESET                                                    605
         AG_RMIN = 0                                                      605
         AG_RMAX = FLGG_PHCATHR                                           605
         AG_DZ = FLGG_PHCATHDZ/2.0                                        605
         AG_TITLE = 'TUBE'                                                605
         AG_EXNAME = 'SHAPE'                                              605
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      605
         CALL AXSHAPE                                                     605
C    *                                                                    605
C    Check FMCG_CHKVSIM==1                                                605
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      606
         CALL GSCKOV( AG_IMED,N,E,ABSCO_PHCATH,EFFIC_PHCATH,              607
     *   RINDEX_PHCATH )                                                  607
C    Defining Hits: USER:0:(0,100000)                                     608
         AG_TITLE = 'FPCT'                                                608
         AG_EXNAME = 'HITS'                                               608
         AG_TYPE=AG_MAGIC                                                 608
         CALL AGSDETV('FPCT')                                             608
C    *                                                                    608
         AG_PARLIST='USER'                                                608
         AG_TITLE = 'DETH'                                                608
         AG_NBITS = 0                                                     608
         AG_BIN=AG_MAGIC                                                  608
         AG_OPTION='*'                                                    608
         AG_FHMIN = 0                                                     608
         AG_FHMAX = 100000                                                608
         CALL AGSDETH (JUMPAD(FPCTSTEP))                                  608
C    *                                                                    608
         CALL AGSDETU('FPCT')                                             608
C    *                                                                    608
         ENDIF                                                            609
5401  CALL AGSPOPB                                                        610
      GO TO BLOCK_FPCT, (5400 ,5410)                                      610
C End of Local Procedure                                                  610
C ----------------------------------------------------------------------  611
C ------------ FUMT                                                       611
C Begin Local Procedure FUMT                                              612
C FUMT IS MU METAL                                                        612
5380  CONTINUE                                                            612
         AG_TITLE='FUMT'                                                  612
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
C    Geant ATTRIBUTE : LUMT ===> SEEN=1 COLO=5 <===                       613
         CALL AGSRESET                                                    614
         AG_SEEN = 1                                                      614
         AG_COLO = 5                                                      614
         AG_TITLE = 'LUMT'                                                614
         AG_EXNAME = 'ATTRIBUTE'                                          614
         AG_PARLIST = 'SEEN_COLO'                                         614
         CALL AXATTRIBUTE                                                 614
C    *                                                                    614
C    Geant SHAPE : TUBE ===> RMIN=FLGG_PHCATHR RMAX=FLGG_MUMETR, DZ=FLGG  614
C    _MUMETDZ/2.0 <===                                                    614
         CALL AGSRESET                                                    616
         AG_RMIN = FLGG_PHCATHR                                           616
         AG_RMAX = FLGG_MUMETR                                            616
         AG_DZ = FLGG_MUMETDZ/2.0                                         616
         AG_TITLE = 'TUBE'                                                616
         AG_EXNAME = 'SHAPE'                                              616
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      616
         CALL AXSHAPE                                                     616
C    *                                                                    616
C    Check FMCG_CHKVSIM==1                                                616
         IF (FMCG_CHKVSIM.EQ.1) THEN                                      617
         CALL GSCKOV( AG_IMED,N,E,ABSCO_MUMET,EFFIC_ALL,RINDEX_MUMET )    618
         ENDIF                                                            619
5381  CALL AGSPOPB                                                        620
      GO TO BLOCK_FUMT, (5380 ,5390)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ PBPT                                                       621
C Begin Local Procedure PBPT                                              622
C PBPT IS PB PLATE                                                        622
5070  CONTINUE                                                            622
         AG_TITLE='PBPT'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             623
         CALL AGSRESET                                                    623
         AG_TITLE = 'LEAD'                                                623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant ATTRIBUTE : PBPT ===> SEEN=1 COLO=7 <===                       623
         CALL AGSRESET                                                    624
         AG_SEEN = 1                                                      624
         AG_COLO = 7                                                      624
         AG_TITLE = 'PBPT'                                                624
         AG_EXNAME = 'ATTRIBUTE'                                          624
         AG_PARLIST = 'SEEN_COLO'                                         624
         CALL AXATTRIBUTE                                                 624
C    *                                                                    624
C    Geant SHAPE : BOX ===> DX=PBPD_WIDTH/2.0 DY=PBPD_HEIGHT/2.0, DZ=PBP  624
C    D_THICK/2.0 <===                                                     624
         CALL AGSRESET                                                    626
         AG_DX = PBPD_WIDTH/2.0                                           626
         AG_DY = PBPD_HEIGHT/2.0                                          626
         AG_DZ = PBPD_THICK/2.0                                           626
         AG_TITLE = 'BOX'                                                 626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'DX_DY_DZ'                                          626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5071  CALL AGSPOPB                                                        627
      GO TO BLOCK_PBPT, (5070 ,5080,5160)                                 627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ FSHM                                                       628
C Begin Local Procedure FSHM                                              629
C FSHM IS THE SHOWER MAX SECTION                                          629
5090  CONTINUE                                                            629
         AG_TITLE='FSHM'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant MATERIAL : AIR ===> inherited arguments used <===              630
         CALL AGSRESET                                                    630
         AG_TITLE = 'AIR'                                                 630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant ATTRIBUTE : FSHM ===> SEEN=1 COLO=4 <===                       630
         CALL AGSRESET                                                    631
         AG_SEEN = 1                                                      631
         AG_COLO = 4                                                      631
         AG_TITLE = 'FSHM'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO'                                         631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2.0 DY=FMXG_G10HGT/2.0, DZ=  631
C    ZTOTSMD <===                                                         631
         CALL AGSRESET                                                    633
         AG_DX = FMXG_G10WIDTH/2.0                                        633
         AG_DY = FMXG_G10HGT/2.0                                          633
         AG_DZ = ZTOTSMD                                                  633
         AG_TITLE = 'BOX'                                                 633
         AG_EXNAME = 'SHAPE'                                              633
         AG_PARLIST = 'DX_DY_DZ'                                          633
         CALL AXSHAPE                                                     633
C    *                                                                    633
         WSMD=FMXG_SBASE/2.0 + FMXG_SGAP                                  633
         WTOTSMD=(FMXG_NSTRIP + 1)*WSMD                                   635
         ZSMD= - ZTOTSMD + FMXG_G10THICK/2.0                              636
C    Create FXGT :                                                        637
C    Call To Local Procedure FXGT                                         637
         ASSIGN 5540 TO BLOCK_FXGT                                        637
         GO TO  5530                                                      637
5540     CONTINUE                                                         637
C    *                                                                    637
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZSMD <===                       638
         CALL AGSRESET                                                    638
         AG_X = 0                                                         638
         AG_Y = 0                                                         638
         AG_Z = ZSMD                                                      638
         AG_TITLE = 'FXGT'                                                638
         AG_EXNAME = 'POSITION'                                           638
         AG_PARLIST = 'X_Y_Z'                                             638
         CALL AXPOSITION                                                  638
C    *                                                                    638
         XSMDV= - WTOTSMD/2.0 - FMXG_SGAP/2.0 + WSMD                      638
         YSMDV=0.0                                                        640
         ZSMDV=ZSMD + FMXG_G10THICK/2.0 + FMXG_SAPEX/2.0                  641
C    *                                                                    642
         I=1                                                              642
            GO TO 5553                                                    642
5551        I=I+1                                                         642
5553        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5552                           642
C       Check MOD(I,2)!=0                                                 643
            IF (MOD(I,2).NE.0) THEN                                       643
C       Create FHMS :                                                     644
C       Call To Local Procedure FHMS                                      644
            ASSIGN 5570 TO BLOCK_FHMS                                     644
            GO TO  5560                                                   644
5570        CONTINUE                                                      644
C       *                                                                 644
C       Geant POSITION : FHMS ===> X=XSMDV Y=YSMDV Z=ZSMDV <===           645
            CALL AGSRESET                                                 645
            AG_X = XSMDV                                                  645
            AG_Y = YSMDV                                                  645
            AG_Z = ZSMDV                                                  645
            AG_TITLE = 'FHMS'                                             645
            AG_EXNAME = 'POSITION'                                        645
            AG_PARLIST = 'X_Y_Z'                                          645
            CALL AXPOSITION                                               645
C       *                                                                 645
            ELSE                                                          645
C       Create FHMS :                                                     647
C       Call To Local Procedure FHMS                                      647
            ASSIGN 5580 TO BLOCK_FHMS                                     647
            GO TO  5560                                                   647
5580        CONTINUE                                                      647
C       *                                                                 647
C       Geant POSITION : FHMS ===> X=XSMDV Y=YSMDV Z=ZSMDV ALPHAX=180 <=  648
C       ==                                                                648
            CALL AGSRESET                                                 648
            AG_X = XSMDV                                                  648
            AG_Y = YSMDV                                                  648
            AG_Z = ZSMDV                                                  648
            AG_ALPHAN=AG_ALPHAN+1                                         648
            AG_ALPHAK(AG_ALPHAN)=1                                        648
            AG_ALPHAL(AG_ALPHAN)=180                                      648
            AG_TITLE = 'FHMS'                                             648
            AG_EXNAME = 'POSITION'                                        648
            AG_PARLIST = 'X_Y_Z_ALPHAX'                                   648
            CALL AXPOSITION                                               648
C       *                                                                 648
            ENDIF                                                         648
            XSMDV=XSMDV+WSMD                                              650
         GO TO 5551                                                       651
5552     CONTINUE                                                         651
         ZSMD2=ZSMDV + FMXG_G10THICK/2.0 + FMXG_SAPEX/2.0                 652
C    Create FXGT :                                                        653
C    Call To Local Procedure FXGT                                         653
         ASSIGN 5590 TO BLOCK_FXGT                                        653
         GO TO  5530                                                      653
5590     CONTINUE                                                         653
C    *                                                                    653
C    Geant POSITION : FXGT ===> X=0 Y=0 Z=ZSMD2 <===                      654
         CALL AGSRESET                                                    654
         AG_X = 0                                                         654
         AG_Y = 0                                                         654
         AG_Z = ZSMD2                                                     654
         AG_TITLE = 'FXGT'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'X_Y_Z'                                             654
         CALL AXPOSITION                                                  654
C    *                                                                    654
         XSMDH=0.0                                                        654
         YSMDH= - WTOTSMD/2.0 - FMXG_SGAP/2.0 + WSMD                      656
         ZSMDH=ZSMD2 + FMXG_G10THICK/2.0 + FMXG_SAPEX/2.0                 657
C    Create FHMS :                                                        658
C    Call To Local Procedure FHMS                                         658
         ASSIGN 5600 TO BLOCK_FHMS                                        658
         GO TO  5560                                                      658
5600     CONTINUE                                                         658
C    *                                                                    658
C    *                                                                    659
         I=1                                                              659
            GO TO 5613                                                    659
5611        I=I+1                                                         659
5613        IF(I-(FMXG_NSTRIP).GT.0) GO TO 5612                           659
C       Check MOD(I,2)!=0                                                 660
            IF (MOD(I,2).NE.0) THEN                                       660
C       Geant POSITION : FHMS ===> X=XSMDH Y=YSMDH Z=ZSMDH ORT=Y-XZ <===  661
            CALL AGSRESET                                                 661
            AG_X = XSMDH                                                  661
            AG_Y = YSMDH                                                  661
            AG_Z = ZSMDH                                                  661
            AG_ALPHAN=AG_ALPHAN+1                                         661
            AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                             661
            AG_TITLE = 'FHMS'                                             661
            AG_EXNAME = 'POSITION'                                        661
            AG_PARLIST = 'X_Y_Z_ORT'                                      661
            CALL AXPOSITION                                               661
C       *                                                                 661
            ELSE                                                          661
C       Geant POSITION : FHMS ===> X=XSMDH Y=YSMDH Z=ZSMDH ORT=YX-Z <===  663
            CALL AGSRESET                                                 663
            AG_X = XSMDH                                                  663
            AG_Y = YSMDH                                                  663
            AG_Z = ZSMDH                                                  663
            AG_ALPHAN=AG_ALPHAN+1                                         663
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             663
            AG_TITLE = 'FHMS'                                             663
            AG_EXNAME = 'POSITION'                                        663
            AG_PARLIST = 'X_Y_Z_ORT'                                      663
            CALL AXPOSITION                                               663
C       *                                                                 663
            ENDIF                                                         663
            YSMDH=YSMDH+WSMD                                              665
         GO TO 5611                                                       666
5612     CONTINUE                                                         666
5091  CALL AGSPOPB                                                        667
      GO TO BLOCK_FSHM, (5090 ,5100,5170)                                 667
C End of Local Procedure                                                  667
C ----------------------------------------------------------------------  668
C ------------ FXGT                                                       668
C Begin Local Procedure FXGT                                              669
C FXGT IS THE G10 LAYER IN THE SMAX                                       669
5530  CONTINUE                                                            669
         AG_TITLE='FXGT'                                                  669
         AG_MODULE=MODULE                                                 669
         AG_MARK='V00'                                                    669
         CALL AGSPUSH                                                     669
         AG_MODULE=MODULE                                                 669
C    *                                                                    669
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          670
         CALL AGSRESET                                                    670
         AG_A = 28.08                                                     670
         AG_Z = 14                                                        670
         AG_W = 0.6*1*28./60.                                             670
         AG_TITLE = 'SI'                                                  670
         AG_EXNAME = 'COMPONENT'                                          670
         AG_PARLIST = 'A_Z_W'                                             670
         CALL AXCOMPONENT                                                 670
C    *                                                                    670
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               670
         CALL AGSRESET                                                    671
         AG_A = 16                                                        671
         AG_Z = 8                                                         671
         AG_W = 0.6*2*16./60.                                             671
         AG_TITLE = 'O'                                                   671
         AG_EXNAME = 'COMPONENT'                                          671
         AG_PARLIST = 'A_Z_W'                                             671
         CALL AXCOMPONENT                                                 671
C    *                                                                    671
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              671
         CALL AGSRESET                                                    672
         AG_A = 12                                                        672
         AG_Z = 6                                                         672
         AG_W = 0.4*8*12./174.                                            672
         AG_TITLE = 'C'                                                   672
         AG_EXNAME = 'COMPONENT'                                          672
         AG_PARLIST = 'A_Z_W'                                             672
         CALL AXCOMPONENT                                                 672
C    *                                                                    672
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               672
         CALL AGSRESET                                                    673
         AG_A = 1                                                         673
         AG_Z = 1                                                         673
         AG_W = 0.4*14*1./174.                                            673
         AG_TITLE = 'H'                                                   673
         AG_EXNAME = 'COMPONENT'                                          673
         AG_PARLIST = 'A_Z_W'                                             673
         CALL AXCOMPONENT                                                 673
C    *                                                                    673
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              673
         CALL AGSRESET                                                    674
         AG_A = 16                                                        674
         AG_Z = 8                                                         674
         AG_W = 0.4*4*16./174.                                            674
         AG_TITLE = 'O'                                                   674
         AG_EXNAME = 'COMPONENT'                                          674
         AG_PARLIST = 'A_Z_W'                                             674
         CALL AXCOMPONENT                                                 674
C    *                                                                    674
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               674
         CALL AGSRESET                                                    675
         AG_DENS = 1.7                                                    675
         AG_TITLE = 'G10'                                                 675
         AG_EXNAME = 'MIXTURE'                                            675
         AG_PARLIST = 'DENS'                                              675
         CALL AXMIXTURE                                                   675
C    *                                                                    675
C    Geant ATTRIBUTE : FXGT ===> SEEN=1 COLO=7 <===                       675
         CALL AGSRESET                                                    676
         AG_SEEN = 1                                                      676
         AG_COLO = 7                                                      676
         AG_TITLE = 'FXGT'                                                676
         AG_EXNAME = 'ATTRIBUTE'                                          676
         AG_PARLIST = 'SEEN_COLO'                                         676
         CALL AXATTRIBUTE                                                 676
C    *                                                                    676
C    Geant SHAPE : BOX ===> DX=FMXG_G10WIDTH/2 DY=FMXG_G10HGT/2, DZ=FMXG  676
C    _G10THICK/2 <===                                                     676
         CALL AGSRESET                                                    678
         AG_DX = FMXG_G10WIDTH/2                                          678
         AG_DY = FMXG_G10HGT/2                                            678
         AG_DZ = FMXG_G10THICK/2                                          678
         AG_TITLE = 'BOX'                                                 678
         AG_EXNAME = 'SHAPE'                                              678
         AG_PARLIST = 'DX_DY_DZ'                                          678
         CALL AXSHAPE                                                     678
C    *                                                                    678
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           678
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           680
5531  CALL AGSPOPB                                                        681
      GO TO BLOCK_FXGT, (5530 ,5540,5590)                                 681
C End of Local Procedure                                                  681
C ----------------------------------------------------------------------  682
C ------------ FHMS                                                       682
C Begin Local Procedure FHMS                                              683
C FHMS IS SHOWER MAX STRIP                                                683
5560  CONTINUE                                                            683
         AG_TITLE='FHMS'                                                  683
         AG_MODULE=MODULE                                                 683
         AG_MARK='V00'                                                    683
         CALL AGSPUSH                                                     683
         AG_MODULE=MODULE                                                 683
C    *                                                                    683
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       684
         CALL AGSRESET                                                    684
         AG_TITLE = 'POLYSTYREN'                                          684
         AG_EXNAME = 'MATERIAL'                                           684
         AG_PARLIST = 'NONE'                                              684
         CALL AXMATERIAL                                                  684
C    *                                                                    684
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       684
         CALL AGSRESET                                                    685
         AG_ISVOL = 1                                                     685
         AG_TITLE = 'CPOLYSTYREN'                                         685
         AG_EXNAME = 'MATERIAL'                                           685
         AG_PARLIST = 'ISVOL'                                             685
         CALL AXMATERIAL                                                  685
C    *                                                                    685
C    Geant ATTRIBUTE : FHMS ===> SEEN=1 COLO=2 <===                       685
         CALL AGSRESET                                                    686
         AG_SEEN = 1                                                      686
         AG_COLO = 2                                                      686
         AG_TITLE = 'FHMS'                                                686
         AG_EXNAME = 'ATTRIBUTE'                                          686
         AG_PARLIST = 'SEEN_COLO'                                         686
         CALL AXATTRIBUTE                                                 686
C    *                                                                    686
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=FMXG_SBASE/2.0 DY=FMXG_G10HGT/2.0  686
C    , DZ=FMXG_SAPEX/2.0 <===                                             686
         CALL AGSRESET                                                    688
         AG_DX1 = 0                                                       688
         AG_DX2 = FMXG_SBASE/2.0                                          688
         AG_DY = FMXG_G10HGT/2.0                                          688
         AG_DZ = FMXG_SAPEX/2.0                                           688
         AG_TITLE = 'TRD1'                                                688
         AG_EXNAME = 'SHAPE'                                              688
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     688
         CALL AXSHAPE                                                     688
C    *                                                                    688
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           688
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             690
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             691
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                692
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.0130)                            693
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            694
C    Defining Hits: BIRK:0:(0,10)                                         695
         AG_TITLE = 'FHMS'                                                695
         AG_EXNAME = 'HITS'                                               695
         AG_TYPE=AG_MAGIC                                                 695
         CALL AGSDETV('FHMS')                                             695
C    *                                                                    695
         AG_PARLIST='BIRK'                                                695
         AG_TITLE = 'DETH'                                                695
         AG_NBITS = 0                                                     695
         AG_BIN=AG_MAGIC                                                  695
         AG_OPTION='*'                                                    695
         AG_FHMIN = 0                                                     695
         AG_FHMAX = 10                                                    695
         CALL AGSDETH (0)                                                 695
C    *                                                                    695
         CALL AGSDETU('FHMS')                                             695
C    *                                                                    695
5561  CALL AGSPOPB                                                        696
      GO TO BLOCK_FHMS, (5560 ,5570,5580,5600)                            696
C End of Local Procedure                                                  696
      WRITE(LOUT,5620)'===>>  [End AgML Module FPDMGEO3]  <<==='          696
5620  FORMAT(A32,/,/)                                                     696
C MODULE FPDMGEO3                                                         698
      END                                                                 698
