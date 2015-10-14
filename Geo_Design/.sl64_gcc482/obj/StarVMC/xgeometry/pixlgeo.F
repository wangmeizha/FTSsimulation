C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
******************************************************************************
      SUBROUTINE PIXLGEO00                                                2
C PIXLGEO00 IS THE SIMPLIFIED PIXEL DETECTOR                              2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'PIXLGEO00'/, BANK_TITLE/'*'/                           2
      DATA MCOMMENT/'IS THE SIMPLIFIED PIXEL DETECTOR'/                   2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C CREATED 03/13/08                                                        2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 CREATED /'03/13/08'/                                   3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C AUTHOR GERRIT VAN NIEUWENHUIZEN                                         3
      CHARACTER*40 AUTHOR /'GERRIT VAN NIEUWENHUIZEN'/                    4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
******************************************************************************
C - Advanced Geant interface                                              6
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      6
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       6
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         6
     *AG_OPTION,AG_ORT,AG_MARK                                            6
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         6
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     6
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     6
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    6
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          6
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   6
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     6
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        6
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   6
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 6
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     6
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       6
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        6
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       6
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       6
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       6
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         6
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     6
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       6
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   6
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       6
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     6
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          6
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         6
     *-696969,AG_LDETU=250)                                               6
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    6
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  6
     *AG_IMFLD,AG_ISIMU                                                   6
C Inherited variables a: saved during internal calls                      6
C b: reset at the entry in a block and then saved                         6
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        6
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       6
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         6
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     6
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       6
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         6
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           6
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        6
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        6
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            6
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           6
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    6
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      6
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           6
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          6
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                6
C local variables valid inside same block                                 6
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       6
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  6
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         6
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    6
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          6
     *AG_OPTION                                                           6
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        6
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             6
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              6
C temporarly                                                              6
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           6
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    6
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       6
     *AG_HY)                                                              6
      REAL MIL_P,INCH_P                                                   6
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               6
C - GEANT description of logical units                                    6
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            6
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      6
      COMMON/GCMAIL/ CHMAIL                                               6
      CHARACTER*132 CHMAIL                                                6
*
      REAL ANGLE,ANGLEPOS,ANGLECORR,RADDEG                                8
      INTEGER NLADDER,NSECTOR, NEXTRALADDER                               9
      REAL LADDERRADIUS, LADDERTHK, LADDERWIDTH                           10
      INTEGER BLOCK_PXMO                                                  12
      INTEGER BLOCK_PXLA                                                  12
      INTEGER BLOCK_PLMI                                                  12
      INTEGER BLOCK_PLAC                                                  12
      INTEGER BLOCK_PLPS                                                  12
      INTEGER BLOCK_PXME                                                  12
*
C -                                                                       17
      REAL PXLD_VERSION                                                   17
      EQUIVALENCE (BANK_PXLD(3),PXLD_VERSION,RE_PXLD_VERSION),            17
     *(NAMES_PXLD(1),NPXLD_VERSION)                                       17
      EQUIVALENCE (COMMEN_PXLD(1),CPXLD_VERSION),(MAP_PXLD(1,1),          17
     *MPXLD_VERSION)                                                      17
      CHARACTER CPXLD_VERSION*40/' '/                                     17
      CHARACTER NPXLD_VERSION*12/'R.VERSION'/                             17
      INTEGER MPXLD_VERSION(2)/1,1/                                       17
      REAL RE_PXLD_VERSION                                                17
C -                                                                       17
      REAL PXLD_SUBVERSION                                                17
      EQUIVALENCE (BANK_PXLD(4),PXLD_SUBVERSION,RE_PXLD_SUBVERSION),      17
     *(NAMES_PXLD(2),NPXLD_SUBVERSION)                                    17
      EQUIVALENCE (COMMEN_PXLD(2),CPXLD_SUBVERSION),(MAP_PXLD(1,2),       17
     *MPXLD_SUBVERSION)                                                   17
      CHARACTER CPXLD_SUBVERSION*40/' '/                                  17
      CHARACTER NPXLD_SUBVERSION*12/'R.SUBVERSION'/                       17
      INTEGER MPXLD_SUBVERSION(2)/1,1/                                    17
      REAL RE_PXLD_SUBVERSION                                             17
C -                                                                       17
      REAL PXLD_TOTALLENGTH                                               17
      EQUIVALENCE (BANK_PXLD(5),PXLD_TOTALLENGTH,RE_PXLD_TOTALLENGTH),    17
     *(NAMES_PXLD(3),NPXLD_TOTALLENGTH)                                   17
      EQUIVALENCE (COMMEN_PXLD(3),CPXLD_TOTALLENGTH),(MAP_PXLD(1,3),      17
     *MPXLD_TOTALLENGTH)                                                  17
      CHARACTER CPXLD_TOTALLENGTH*40/' '/                                 17
      CHARACTER NPXLD_TOTALLENGTH*12/'R.TOTALLENGTH'/                     17
      INTEGER MPXLD_TOTALLENGTH(2)/1,1/                                   17
      REAL RE_PXLD_TOTALLENGTH                                            17
C -                                                                       17
      REAL PXLD_PASSIVETHK                                                17
      EQUIVALENCE (BANK_PXLD(6),PXLD_PASSIVETHK,RE_PXLD_PASSIVETHK),      17
     *(NAMES_PXLD(4),NPXLD_PASSIVETHK)                                    17
      EQUIVALENCE (COMMEN_PXLD(4),CPXLD_PASSIVETHK),(MAP_PXLD(1,4),       17
     *MPXLD_PASSIVETHK)                                                   17
      CHARACTER CPXLD_PASSIVETHK*40/' '/                                  17
      CHARACTER NPXLD_PASSIVETHK*12/'R.PASSIVETHK'/                       17
      INTEGER MPXLD_PASSIVETHK(2)/1,1/                                    17
      REAL RE_PXLD_PASSIVETHK                                             17
C -                                                                       17
      REAL PXLD_ACTIVETHK                                                 17
      EQUIVALENCE (BANK_PXLD(7),PXLD_ACTIVETHK,RE_PXLD_ACTIVETHK),        17
     *(NAMES_PXLD(5),NPXLD_ACTIVETHK)                                     17
      EQUIVALENCE (COMMEN_PXLD(5),CPXLD_ACTIVETHK),(MAP_PXLD(1,5),        17
     *MPXLD_ACTIVETHK)                                                    17
      CHARACTER CPXLD_ACTIVETHK*40/' '/                                   17
      CHARACTER NPXLD_ACTIVETHK*12/'R.ACTIVETHK'/                         17
      INTEGER MPXLD_ACTIVETHK(2)/1,1/                                     17
      REAL RE_PXLD_ACTIVETHK                                              17
C -                                                                       17
      REAL PXLD_LAYERTHK                                                  17
      EQUIVALENCE (BANK_PXLD(8),PXLD_LAYERTHK,RE_PXLD_LAYERTHK),          17
     *(NAMES_PXLD(6),NPXLD_LAYERTHK)                                      17
      EQUIVALENCE (COMMEN_PXLD(6),CPXLD_LAYERTHK),(MAP_PXLD(1,6),         17
     *MPXLD_LAYERTHK)                                                     17
      CHARACTER CPXLD_LAYERTHK*40/' '/                                    17
      CHARACTER NPXLD_LAYERTHK*12/'R.LAYERTHK'/                           17
      INTEGER MPXLD_LAYERTHK(2)/1,1/                                      17
      REAL RE_PXLD_LAYERTHK                                               17
C -                                                                       17
      REAL PXLD_RIN                                                       17
      EQUIVALENCE (BANK_PXLD(9),PXLD_RIN,RE_PXLD_RIN),(NAMES_PXLD(7),     17
     *NPXLD_RIN)                                                          17
      EQUIVALENCE (COMMEN_PXLD(7),CPXLD_RIN),(MAP_PXLD(1,7),MPXLD_RIN)    17
      CHARACTER CPXLD_RIN*40/' '/                                         17
      CHARACTER NPXLD_RIN*12/'R.RIN'/                                     17
      INTEGER MPXLD_RIN(2)/1,1/                                           17
      REAL RE_PXLD_RIN                                                    17
C -                                                                       17
      REAL PXLD_ROUT                                                      17
      EQUIVALENCE (BANK_PXLD(10),PXLD_ROUT,RE_PXLD_ROUT),(NAMES_PXLD(8),  17
     *NPXLD_ROUT)                                                         17
      EQUIVALENCE (COMMEN_PXLD(8),CPXLD_ROUT),(MAP_PXLD(1,8),             17
     *MPXLD_ROUT)                                                         17
      CHARACTER CPXLD_ROUT*40/' '/                                        17
      CHARACTER NPXLD_ROUT*12/'R.ROUT'/                                   17
      INTEGER MPXLD_ROUT(2)/1,1/                                          17
      REAL RE_PXLD_ROUT                                                   17
C -                                                                       17
      INTEGER LEN_PXLD,LLEN_PXLD,LINK_PXLD                                17
      PARAMETER (LEN_PXLD=10)                                             17
      PARAMETER (LLEN_PXLD=8)                                             17
      INTEGER FLAG_PXLD/0/,MAP_PXLD(2,LLEN_PXLD),IDNUM_PXLD(10)           17
      CHARACTER NAMES_PXLD(LLEN_PXLD)*12,COMMEN_PXLD(LLEN_PXLD)*40        17
      DATA IDNUM_PXLD/10*0/                                               17
      REAL BANK_PXLD(LEN_PXLD)                                            17
      CHARACTER*60 BPATH_PXLD/' '/                                        17
      SAVE FLAG_PXLD,MAP_PXLD,IDNUM_PXLD,NAMES_PXLD,COMMEN_PXLD,          17
     *BPATH_PXLD                                                          17
      DATA LINK_PXLD/0/,BANK_PXLD/LEN_PXLD*0/                             17
C *                                                                       17
C -                                                                       19
      REAL PIXG_LAYER                                                     19
      EQUIVALENCE (BANK_PIXG(3),PIXG_LAYER,RE_PIXG_LAYER),                19
     *(NAMES_PIXG(1),NPIXG_LAYER)                                         19
      EQUIVALENCE (COMMEN_PIXG(1),CPIXG_LAYER),(MAP_PIXG(1,1),            19
     *MPIXG_LAYER)                                                        19
      CHARACTER CPIXG_LAYER*40/' '/                                       19
      CHARACTER NPIXG_LAYER*12/'R.LAYER'/                                 19
      INTEGER MPIXG_LAYER(2)/1,1/                                         19
      REAL RE_PIXG_LAYER                                                  19
C -                                                                       19
      REAL PIXG_NOLADDERS                                                 19
      EQUIVALENCE (BANK_PIXG(4),PIXG_NOLADDERS,RE_PIXG_NOLADDERS),        19
     *(NAMES_PIXG(2),NPIXG_NOLADDERS)                                     19
      EQUIVALENCE (COMMEN_PIXG(2),CPIXG_NOLADDERS),(MAP_PIXG(1,2),        19
     *MPIXG_NOLADDERS)                                                    19
      CHARACTER CPIXG_NOLADDERS*40/' '/                                   19
      CHARACTER NPIXG_NOLADDERS*12/'R.NOLADDERS'/                         19
      INTEGER MPIXG_NOLADDERS(2)/1,1/                                     19
      REAL RE_PIXG_NOLADDERS                                              19
C -                                                                       19
      REAL PIXG_R                                                         19
      EQUIVALENCE (BANK_PIXG(5),PIXG_R,RE_PIXG_R),(NAMES_PIXG(3),         19
     *NPIXG_R)                                                            19
      EQUIVALENCE (COMMEN_PIXG(3),CPIXG_R),(MAP_PIXG(1,3),MPIXG_R)        19
      CHARACTER CPIXG_R*40/' '/                                           19
      CHARACTER NPIXG_R*12/'R.R'/                                         19
      INTEGER MPIXG_R(2)/1,1/                                             19
      REAL RE_PIXG_R                                                      19
C -                                                                       19
      REAL PIXG_A                                                         19
      EQUIVALENCE (BANK_PIXG(6),PIXG_A,RE_PIXG_A),(NAMES_PIXG(4),         19
     *NPIXG_A)                                                            19
      EQUIVALENCE (COMMEN_PIXG(4),CPIXG_A),(MAP_PIXG(1,4),MPIXG_A)        19
      CHARACTER CPIXG_A*40/' '/                                           19
      CHARACTER NPIXG_A*12/'R.A'/                                         19
      INTEGER MPIXG_A(2)/1,1/                                             19
      REAL RE_PIXG_A                                                      19
C -                                                                       19
      REAL PIXG_POFFSET                                                   19
      EQUIVALENCE (BANK_PIXG(7),PIXG_POFFSET,RE_PIXG_POFFSET),            19
     *(NAMES_PIXG(5),NPIXG_POFFSET)                                       19
      EQUIVALENCE (COMMEN_PIXG(5),CPIXG_POFFSET),(MAP_PIXG(1,5),          19
     *MPIXG_POFFSET)                                                      19
      CHARACTER CPIXG_POFFSET*40/' '/                                     19
      CHARACTER NPIXG_POFFSET*12/'R.POFFSET'/                             19
      INTEGER MPIXG_POFFSET(2)/1,1/                                       19
      REAL RE_PIXG_POFFSET                                                19
C -                                                                       19
      REAL PIXG_AOFFSET                                                   19
      EQUIVALENCE (BANK_PIXG(8),PIXG_AOFFSET,RE_PIXG_AOFFSET),            19
     *(NAMES_PIXG(6),NPIXG_AOFFSET)                                       19
      EQUIVALENCE (COMMEN_PIXG(6),CPIXG_AOFFSET),(MAP_PIXG(1,6),          19
     *MPIXG_AOFFSET)                                                      19
      CHARACTER CPIXG_AOFFSET*40/' '/                                     19
      CHARACTER NPIXG_AOFFSET*12/'R.AOFFSET'/                             19
      INTEGER MPIXG_AOFFSET(2)/1,1/                                       19
      REAL RE_PIXG_AOFFSET                                                19
C -                                                                       19
      INTEGER LEN_PIXG,LLEN_PIXG,LINK_PIXG                                19
      PARAMETER (LEN_PIXG=8)                                              19
      PARAMETER (LLEN_PIXG=6)                                             19
      INTEGER FLAG_PIXG/0/,MAP_PIXG(2,LLEN_PIXG),IDNUM_PIXG(10)           19
      CHARACTER NAMES_PIXG(LLEN_PIXG)*12,COMMEN_PIXG(LLEN_PIXG)*40        19
      DATA IDNUM_PIXG/10*0/                                               19
      REAL BANK_PIXG(LEN_PIXG)                                            19
      CHARACTER*60 BPATH_PIXG/' '/                                        19
      SAVE FLAG_PIXG,MAP_PIXG,IDNUM_PIXG,NAMES_PIXG,COMMEN_PIXG,          19
     *BPATH_PIXG                                                          19
      DATA LINK_PIXG/0/,BANK_PIXG/LEN_PIXG*0/                             19
C *                                                                       19
C -                                                                       20
      REAL PXBG_VERSION                                                   20
      EQUIVALENCE (BANK_PXBG(3),PXBG_VERSION,RE_PXBG_VERSION),            20
     *(NAMES_PXBG(1),NPXBG_VERSION)                                       20
      EQUIVALENCE (COMMEN_PXBG(1),CPXBG_VERSION),(MAP_PXBG(1,1),          20
     *MPXBG_VERSION)                                                      20
      CHARACTER CPXBG_VERSION*40/' '/                                     20
      CHARACTER NPXBG_VERSION*12/'R.VERSION'/                             20
      INTEGER MPXBG_VERSION(2)/1,1/                                       20
      REAL RE_PXBG_VERSION                                                20
C -                                                                       20
      REAL PXBG_LENGTH                                                    20
      EQUIVALENCE (BANK_PXBG(4),PXBG_LENGTH,RE_PXBG_LENGTH),              20
     *(NAMES_PXBG(2),NPXBG_LENGTH)                                        20
      EQUIVALENCE (COMMEN_PXBG(2),CPXBG_LENGTH),(MAP_PXBG(1,2),           20
     *MPXBG_LENGTH)                                                       20
      CHARACTER CPXBG_LENGTH*40/' '/                                      20
      CHARACTER NPXBG_LENGTH*12/'R.LENGTH'/                               20
      INTEGER MPXBG_LENGTH(2)/1,1/                                        20
      REAL RE_PXBG_LENGTH                                                 20
C -                                                                       20
      REAL PXBG_RIN                                                       20
      EQUIVALENCE (BANK_PXBG(5),PXBG_RIN,RE_PXBG_RIN),(NAMES_PXBG(3),     20
     *NPXBG_RIN)                                                          20
      EQUIVALENCE (COMMEN_PXBG(3),CPXBG_RIN),(MAP_PXBG(1,3),MPXBG_RIN)    20
      CHARACTER CPXBG_RIN*40/' '/                                         20
      CHARACTER NPXBG_RIN*12/'R.RIN'/                                     20
      INTEGER MPXBG_RIN(2)/1,1/                                           20
      REAL RE_PXBG_RIN                                                    20
C -                                                                       20
      REAL PXBG_THK                                                       20
      EQUIVALENCE (BANK_PXBG(6),PXBG_THK,RE_PXBG_THK),(NAMES_PXBG(4),     20
     *NPXBG_THK)                                                          20
      EQUIVALENCE (COMMEN_PXBG(4),CPXBG_THK),(MAP_PXBG(1,4),MPXBG_THK)    20
      CHARACTER CPXBG_THK*40/' '/                                         20
      CHARACTER NPXBG_THK*12/'R.THK'/                                     20
      INTEGER MPXBG_THK(2)/1,1/                                           20
      REAL RE_PXBG_THK                                                    20
C -                                                                       20
      INTEGER LEN_PXBG,LLEN_PXBG,LINK_PXBG                                20
      PARAMETER (LEN_PXBG=6)                                              20
      PARAMETER (LLEN_PXBG=4)                                             20
      INTEGER FLAG_PXBG/0/,MAP_PXBG(2,LLEN_PXBG),IDNUM_PXBG(10)           20
      CHARACTER NAMES_PXBG(LLEN_PXBG)*12,COMMEN_PXBG(LLEN_PXBG)*40        20
      DATA IDNUM_PXBG/10*0/                                               20
      REAL BANK_PXBG(LEN_PXBG)                                            20
      CHARACTER*60 BPATH_PXBG/' '/                                        20
      SAVE FLAG_PXBG,MAP_PXBG,IDNUM_PXBG,NAMES_PXBG,COMMEN_PXBG,          20
     *BPATH_PXBG                                                          20
      DATA LINK_PXBG/0/,BANK_PXBG/LEN_PXBG*0/                             20
C *                                                                       20
*
* -----------------------------------------------------------------------------
*
*
C *                                                                       25
      CALL AGSBEGM(MODULE,IPRIN)                                          25
      IF (IPRIN.LT.0) RETURN                                              25
C Filling bank PXLD - GENERAL PIXEL PARAMETERS                            25
      BANK_TITLE='GENERAL PIXEL PARAMETERS'                               25
      BPATH_PXLD='PXLD*'                                                  25
      IDNUM_PXLD(1)=0                                                     26
C VERSION                                                                 26
      PXLD_VERSION=1.0                                                    26
      CPXLD_VERSION='VERSION'                                             26
C SUB VERSION                                                             27
      PXLD_SUBVERSION=0.0                                                 27
      CPXLD_SUBVERSION='SUB VERSION'                                      27
C OVERAL LENGTH OF THE DETECTOR                                           28
      PXLD_TOTALLENGTH=20.0                                               28
      CPXLD_TOTALLENGTH='OVERAL LENGTH OF THE DETECTOR'                   28
C PASSIVE SILICON THICKNESS                                               29
      PXLD_PASSIVETHK=0.0280                                              29
      CPXLD_PASSIVETHK='PASSIVE SILICON THICKNESS'                        29
C ACTIVE SILICON THICKNESS                                                30
      PXLD_ACTIVETHK=0.0020                                               30
      CPXLD_ACTIVETHK='ACTIVE SILICON THICKNESS'                          30
C INNER RADIUS                                                            31
      PXLD_RIN=2.5                                                        31
      CPXLD_RIN='INNER RADIUS'                                            31
C OUTER RADIUS                                                            32
      PXLD_ROUT=8.0                                                       32
      CPXLD_ROUT='OUTER RADIUS'                                           32
C THICKNESS OF THE LAYER MOTHER VOLUME                                    33
      PXLD_LAYERTHK=0.5                                                   33
      CPXLD_LAYERTHK='THICKNESS OF THE LAYER MOTHER VOLUME'               33
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLD',    34
     *BPATH_PXLD,IDNUM_PXLD,1, MAP_PXLD,NAMES_PXLD,COMMEN_PXLD,           34
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   34
C *                                                                       34
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLD',    34
     *BPATH_PXLD,IDNUM_PXLD,1, MAP_PXLD,NAMES_PXLD,COMMEN_PXLD,           34
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   34
C *                                                                       36
      CALL AGSBEGM(MODULE,IPRIN)                                          36
      IF (IPRIN.LT.0) RETURN                                              36
C Filling bank PIXG - INNER LAYER PARAMETERS                              36
      BANK_TITLE='INNER LAYER PARAMETERS'                                 36
      BPATH_PIXG='PIXG*'                                                  36
      IDNUM_PIXG(1)=0                                                     37
C LAYER INDEX                                                             37
      PIXG_LAYER=1                                                        37
      CPIXG_LAYER='LAYER INDEX'                                           37
C NUMBER OF LADDERS                                                       38
      PIXG_NOLADDERS=10                                                   38
      CPIXG_NOLADDERS='NUMBER OF LADDERS'                                 38
C 1ST LADDER NOMINAL RADIUS                                               39
      PIXG_R=2.5                                                          39
      CPIXG_R='1ST LADDER NOMINAL RADIUS'                                 39
C ANGULAR OFFSET                                                          40
      PIXG_AOFFSET=-90.0                                                  40
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      40
C POSITION OFFSET (SHIFT)                                                 41
      PIXG_POFFSET=0.0                                                    41
      CPIXG_POFFSET='POSITION OFFSET (SHIFT)'                             41
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    42
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           42
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   42
C *                                                                       42
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    42
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           42
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   42
C *                                                                       44
      CALL AGSBEGM(MODULE,IPRIN)                                          44
      IF (IPRIN.LT.0) RETURN                                              44
C Filling bank PIXG - OUTER LAYER PARAMETERS                              44
      BANK_TITLE='OUTER LAYER PARAMETERS'                                 44
      BPATH_PIXG='PIXG*'                                                  44
      IDNUM_PIXG(1)=0                                                     45
C LAYER INDEX                                                             45
      PIXG_LAYER=2                                                        45
      CPIXG_LAYER='LAYER INDEX'                                           45
C NUMBER OF LADDERS                                                       46
      PIXG_NOLADDERS=30                                                   46
      CPIXG_NOLADDERS='NUMBER OF LADDERS'                                 46
C LADDER RADIUS                                                           47
      PIXG_R=8.0                                                          47
      CPIXG_R='LADDER RADIUS'                                             47
C ANGULAR OFFSET                                                          48
      PIXG_AOFFSET=-90.0                                                  48
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      48
C POSITION OFFSET (SHIFT)                                                 49
      PIXG_POFFSET=0.0                                                    49
      CPIXG_POFFSET='POSITION OFFSET (SHIFT)'                             49
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    50
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           50
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   50
C *                                                                       50
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    50
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           50
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   50
*   Fill PXBG                   ! Beam Pipe Exoskeleton Data
*      version    =  2          ! Version
*      Length     =  48.0       ! Total Length
*      Rin        =   8.5       ! Inner Radius
*      Thk        =   0.0800    ! Thickness
*   EndFill
* -----------------------------------------------------------------------------
      RADDEG=3.14159265/180.0                                             61
* -----------------------------------------------------------------------------
C use PXLD bank                                                           63
      BPATH_PXLD='PXLD*'                                                  63
      IDNUM_PXLD(1)=0                                                     63
      PXLD_VERSION=1                                                      63
      CALL AGDGETP('PIXLGEO00','*',-999,'PXLD',NPXLD_VERSION,             63
     *RE_PXLD_VERSION,BPATH_PXLD,IDNUM_PXLD,1,MAP_PXLD,NAMES_PXLD,        63
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   63
      LADDERTHK = PXLD_ACTIVETHK + PXLD_PASSIVETHK                        65
      WRITE(*,*) '===>GEOINFO/pixlgeo00 SIMPLE VERSION of PIXEL!!! - PXM  67
     *O - created'                                                        67
      CALL AGSBEGM(MODULE,IPRIN)                                          69
C Check IPRIN<0                                                           69
      IF (IPRIN.LT.0) RETURN                                              69
C Create PXMO :                                                           69
C Call To Local Procedure PXMO                                            69
      ASSIGN 5020 TO BLOCK_PXMO                                           69
      GO TO  5010                                                         69
5020  CONTINUE                                                            69
C *                                                                       69
C Geant POSITION : PXMO ===> MOTHER='CAVE' KONLY='ONLY' <===              70
      CALL AGSRESET                                                       70
      AG_MOTHER = 'CAVE'                                                  70
      AG_KONLY = 'ONLY'                                                   70
      AG_TITLE = 'PXMO'                                                   70
      AG_EXNAME = 'POSITION'                                              70
      AG_PARLIST = 'MOTHER_KONLY'                                         70
      CALL AXPOSITION                                                     70
C *                                                                       70
*      Create   PXME
*      Position PXME in CAVE
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        75
      RETURN                                                              75
C Begin Local Procedure PXMO                                              75
C PXMO IS THE MOTHER OF THE PIXEL DETECTOR VOLUMES                        75
5010  CONTINUE                                                            75
         AG_TITLE='PXMO'                                                  75
         AG_MODULE=MODULE                                                 75
         AG_MARK='V00'                                                    75
         CALL AGSPUSH                                                     75
         AG_MODULE=MODULE                                                 75
C    *                                                                    75
C    Geant MATERIAL : AIR ===> inherited arguments used <===              76
         CALL AGSRESET                                                    76
         AG_TITLE = 'AIR'                                                 76
         AG_EXNAME = 'MATERIAL'                                           76
         AG_PARLIST = 'NONE'                                              76
         CALL AXMATERIAL                                                  76
C    *                                                                    76
C    Geant ATTRIBUTE : PXMO ===> SEEN=0 COLO=6 <===                       76
         CALL AGSRESET                                                    77
         AG_SEEN = 0                                                      77
         AG_COLO = 6                                                      77
         AG_TITLE = 'PXMO'                                                77
         AG_EXNAME = 'ATTRIBUTE'                                          77
         AG_PARLIST = 'SEEN_COLO'                                         77
         CALL AXATTRIBUTE                                                 77
C    *                                                                    77
C    Geant SHAPE : TUBE ===> RMIN=PXLD_RIN-PXLD_LAYERTHK/2.0 RMAX=PXLD_R  81
C    OUT+PXLD_LAYERTHK/2.0 DZ=PXLD_TOTALLENGTH/2.0 <===                   81
         CALL AGSRESET                                                    81
         AG_RMIN = PXLD_RIN-PXLD_LAYERTHK/2.0                             81
         AG_RMAX = PXLD_ROUT+PXLD_LAYERTHK/2.0                            81
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     81
         AG_TITLE = 'TUBE'                                                81
         AG_EXNAME = 'SHAPE'                                              81
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      81
         CALL AXSHAPE                                                     81
C    *                                                                    81
*      Make inner layer
C    use PIXG bank                                                        85
         BPATH_PIXG='PIXG*'                                               85
         IDNUM_PIXG(1)=0                                                  85
         PIXG_LAYER=1                                                     85
         CALL AGDGETP('PIXLGEO00','*',-999,'PIXG',NPIXG_LAYER,            85
     *   RE_PIXG_LAYER,BPATH_PIXG,IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,       85
     *   BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                85
C    Create PXLA :                                                        86
C    Call To Local Procedure PXLA                                         86
         ASSIGN 5040 TO BLOCK_PXLA                                        86
         GO TO  5030                                                      86
5040     CONTINUE                                                         86
C    *                                                                    86
C    Geant POSITION : PXLA ===> inherited arguments used <===             87
         CALL AGSRESET                                                    87
         AG_TITLE = 'PXLA'                                                87
         AG_EXNAME = 'POSITION'                                           87
         AG_PARLIST = 'NONE'                                              87
         CALL AXPOSITION                                                  87
C    *                                                                    87
*      Make outer layer
C    use PIXG bank                                                        89
         BPATH_PIXG='PIXG*'                                               89
         IDNUM_PIXG(1)=0                                                  89
         PIXG_LAYER=2                                                     89
         CALL AGDGETP('PIXLGEO00','*',-999,'PIXG',NPIXG_LAYER,            89
     *   RE_PIXG_LAYER,BPATH_PIXG,IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,       89
     *   BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                89
C    Create PXLA :                                                        90
C    Call To Local Procedure PXLA                                         90
         ASSIGN 5050 TO BLOCK_PXLA                                        90
         GO TO  5030                                                      90
5050     CONTINUE                                                         90
C    *                                                                    90
C    Geant POSITION : PXLA ===> inherited arguments used <===             91
         CALL AGSRESET                                                    91
         AG_TITLE = 'PXLA'                                                91
         AG_EXNAME = 'POSITION'                                           91
         AG_PARLIST = 'NONE'                                              91
         CALL AXPOSITION                                                  91
C    *                                                                    91
5011  CALL AGSPOPB                                                        91
      GO TO BLOCK_PXMO, (5010 ,5020)                                      92
C End of Local Procedure                                                  92
* -----------------------------------------------------------------------------
C Begin Local Procedure PXLA                                              94
C PXLA IS THE MOTHER OF A LAYER                                           94
5030  CONTINUE                                                            94
         AG_TITLE='PXLA'                                                  94
         AG_MODULE=MODULE                                                 94
         AG_MARK='V00'                                                    94
         CALL AGSPUSH                                                     94
         AG_MODULE=MODULE                                                 94
C    *                                                                    94
C    Geant MATERIAL : AIR ===> inherited arguments used <===              95
         CALL AGSRESET                                                    95
         AG_TITLE = 'AIR'                                                 95
         AG_EXNAME = 'MATERIAL'                                           95
         AG_PARLIST = 'NONE'                                              95
         CALL AXMATERIAL                                                  95
C    *                                                                    95
C    Geant ATTRIBUTE : PXLA ===> SEEN=0 COLO=1 <===                       95
         CALL AGSRESET                                                    96
         AG_SEEN = 0                                                      96
         AG_COLO = 1                                                      96
         AG_TITLE = 'PXLA'                                                96
         AG_EXNAME = 'ATTRIBUTE'                                          96
         AG_PARLIST = 'SEEN_COLO'                                         96
         CALL AXATTRIBUTE                                                 96
C    *                                                                    96
C    Geant SHAPE : TUBE ===> RMIN=PIXG_R-PXLD_LAYERTHK/2.0 RMAX=PIXG_R+P  96
C    XLD_LAYERTHK/2.0 DZ=PXLD_TOTALLENGTH/2.0 <===                        96
         CALL AGSRESET                                                    99
         AG_RMIN = PIXG_R-PXLD_LAYERTHK/2.0                               99
         AG_RMAX = PIXG_R+PXLD_LAYERTHK/2.0                               99
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     99
         AG_TITLE = 'TUBE'                                                99
         AG_EXNAME = 'SHAPE'                                              99
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      99
         CALL AXSHAPE                                                     99
C    *                                                                    99
*     Determine the radius of the ladder mother so that the active layer
*     ends up at the right radius
         LADDERRADIUS = PIXG_R+LADDERTHK/2.0-PXLD_ACTIVETHK/2.0           103
*     Determine the width of the ladder so that the layer
*     will be hermetic
         LADDERWIDTH = 2.0 * (PIXG_R-PXLD_ACTIVETHK/2.0) *                108
     *   TAN(RADDEG*360.0/PIXG_NOLADDERS/2.0)                             108
C    *                                                                    110
         NLADDER=1                                                        110
            GO TO 5063                                                    110
5061        NLADDER=NLADDER+1                                             110
5063        IF(NLADDER-(PIXG_NOLADDERS).GT.0) GO TO 5062                  110
            ANGLE = (360.0/PIXG_NOLADDERS)*NLADDER                        111
            ANGLEPOS = ANGLE*RADDEG                                       112
C       Create PLMI :                                                     114
C       Call To Local Procedure PLMI                                      114
            ASSIGN 5080 TO BLOCK_PLMI                                     114
            GO TO  5070                                                   114
5080        CONTINUE                                                      114
C       *                                                                 114
C       Geant POSITION : PLMI ===> X=LADDERRADIUS*COS(ANGLEPOS) Y=LADDER  118
C       RADIUS*SIN(ANGLEPOS) Z=0.0 ALPHAZ=ANGLE+PIXG_AOFFSET <===         118
            CALL AGSRESET                                                 118
            AG_X = LADDERRADIUS*COS(ANGLEPOS)                             118
            AG_Y = LADDERRADIUS*SIN(ANGLEPOS)                             118
            AG_Z = 0.0                                                    118
            AG_ALPHAN=AG_ALPHAN+1                                         118
            AG_ALPHAK(AG_ALPHAN)=3                                        118
            AG_ALPHAL(AG_ALPHAN)=ANGLE+PIXG_AOFFSET                       118
            AG_TITLE = 'PLMI'                                             118
            AG_EXNAME = 'POSITION'                                        118
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   118
            CALL AXPOSITION                                               118
C       *                                                                 118
         GO TO 5061                                                       119
5062     CONTINUE                                                         119
5031  CALL AGSPOPB                                                        120
      GO TO BLOCK_PXLA, (5030 ,5040,5050)                                 120
C End of Local Procedure                                                  120
* -----------------------------------------------------------------------------
C Begin Local Procedure PLMI                                              122
C PLMI IS THE MOTHER OF A SILICON LADDER                                  122
5070  CONTINUE                                                            122
         AG_TITLE='PLMI'                                                  122
         AG_MODULE=MODULE                                                 122
         AG_MARK='V00'                                                    122
         CALL AGSPUSH                                                     122
         AG_MODULE=MODULE                                                 122
C    *                                                                    122
C    Geant MATERIAL : AIR ===> inherited arguments used <===              123
         CALL AGSRESET                                                    123
         AG_TITLE = 'AIR'                                                 123
         AG_EXNAME = 'MATERIAL'                                           123
         AG_PARLIST = 'NONE'                                              123
         CALL AXMATERIAL                                                  123
C    *                                                                    123
C    Geant ATTRIBUTE : PLMI ===> SEEN=0 COLO=6 <===                       123
         CALL AGSRESET                                                    124
         AG_SEEN = 0                                                      124
         AG_COLO = 6                                                      124
         AG_TITLE = 'PLMI'                                                124
         AG_EXNAME = 'ATTRIBUTE'                                          124
         AG_PARLIST = 'SEEN_COLO'                                         124
         CALL AXATTRIBUTE                                                 124
C    *                                                                    124
C    Geant SHAPE : BOX ===> DX=LADDERWIDTH/2.0 DY=LADDERTHK/2.0 DZ=PXLD_  124
C    TOTALLENGTH/2.0 <===                                                 124
         CALL AGSRESET                                                    127
         AG_DX = LADDERWIDTH/2.0                                          127
         AG_DY = LADDERTHK/2.0                                            127
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     127
         AG_TITLE = 'BOX'                                                 127
         AG_EXNAME = 'SHAPE'                                              127
         AG_PARLIST = 'DX_DY_DZ'                                          127
         CALL AXSHAPE                                                     127
C    *                                                                    127
C    Create PLAC :                                                        129
C    Call To Local Procedure PLAC                                         129
         ASSIGN 5100 TO BLOCK_PLAC                                        129
         GO TO  5090                                                      129
5100     CONTINUE                                                         129
C    *                                                                    129
C    Geant POSITION : PLAC ===> X=0.0 Y=-1.0*LADDERTHK/2.0+PXLD_ACTIVETH  132
C    K/2.0 Z = 0.0 <===                                                   132
         CALL AGSRESET                                                    132
         AG_X = 0.0                                                       132
         AG_Y = -1.0*LADDERTHK/2.0+PXLD_ACTIVETHK/2.0                     132
         AG_Z = 0.0                                                       132
         AG_TITLE = 'PLAC'                                                132
         AG_EXNAME = 'POSITION'                                           132
         AG_PARLIST = 'X_Y_Z'                                             132
         CALL AXPOSITION                                                  132
C    *                                                                    132
C    Create PLPS :                                                        133
C    Call To Local Procedure PLPS                                         133
         ASSIGN 5120 TO BLOCK_PLPS                                        133
         GO TO  5110                                                      133
5120     CONTINUE                                                         133
C    *                                                                    133
C    Geant POSITION : PLPS ===> X=0.0 Y=+1.0*LADDERTHK/2.0-PXLD_PASSIVET  136
C    HK/2.0 Z = 0.0 <===                                                  136
         CALL AGSRESET                                                    136
         AG_X = 0.0                                                       136
         AG_Y = +1.0*LADDERTHK/2.0-PXLD_PASSIVETHK/2.0                    136
         AG_Z = 0.0                                                       136
         AG_TITLE = 'PLPS'                                                136
         AG_EXNAME = 'POSITION'                                           136
         AG_PARLIST = 'X_Y_Z'                                             136
         CALL AXPOSITION                                                  136
C    *                                                                    136
5071  CALL AGSPOPB                                                        136
      GO TO BLOCK_PLMI, (5070 ,5080)                                      137
C End of Local Procedure                                                  137
* -----------------------------------------------------------------------------
C Begin Local Procedure PLPS                                              139
C PLPS IS THE PASSIVE LAYER OF THE LADDER                                 139
5110  CONTINUE                                                            139
         AG_TITLE='PLPS'                                                  139
         AG_MODULE=MODULE                                                 139
         AG_MARK='V00'                                                    139
         CALL AGSPUSH                                                     139
         AG_MODULE=MODULE                                                 139
C    *                                                                    139
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          140
         CALL AGSRESET                                                    140
         AG_TITLE = 'SILICON'                                             140
         AG_EXNAME = 'MATERIAL'                                           140
         AG_PARLIST = 'NONE'                                              140
         CALL AXMATERIAL                                                  140
C    *                                                                    140
C    Geant ATTRIBUTE : PLPS ===> SEEN=1 COLO=4 <===                       140
         CALL AGSRESET                                                    141
         AG_SEEN = 1                                                      141
         AG_COLO = 4                                                      141
         AG_TITLE = 'PLPS'                                                141
         AG_EXNAME = 'ATTRIBUTE'                                          141
         AG_PARLIST = 'SEEN_COLO'                                         141
         CALL AXATTRIBUTE                                                 141
C    *                                                                    141
C    Geant SHAPE : BOX ===> DX=LADDERWIDTH/2.0 DY=PXLD_PASSIVETHK/2.0 DZ  145
C    =PXLD_TOTALLENGTH/2.0 <===                                           145
         CALL AGSRESET                                                    145
         AG_DX = LADDERWIDTH/2.0                                          145
         AG_DY = PXLD_PASSIVETHK/2.0                                      145
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     145
         AG_TITLE = 'BOX'                                                 145
         AG_EXNAME = 'SHAPE'                                              145
         AG_PARLIST = 'DX_DY_DZ'                                          145
         CALL AXSHAPE                                                     145
C    *                                                                    145
5111  CALL AGSPOPB                                                        145
      GO TO BLOCK_PLPS, (5110 ,5120)                                      146
C End of Local Procedure                                                  146
* -----------------------------------------------------------------------------
C Begin Local Procedure PLAC                                              148
C PLAC IS THE ACTIVE LAYER OF THE LADDER                                  148
5090  CONTINUE                                                            148
         AG_TITLE='PLAC'                                                  148
         AG_MODULE=MODULE                                                 148
         AG_MARK='V00'                                                    148
         CALL AGSPUSH                                                     148
         AG_MODULE=MODULE                                                 148
C    *                                                                    148
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          149
         CALL AGSRESET                                                    149
         AG_TITLE = 'SILICON'                                             149
         AG_EXNAME = 'MATERIAL'                                           149
         AG_PARLIST = 'NONE'                                              149
         CALL AXMATERIAL                                                  149
C    *                                                                    149
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         149
         CALL AGSRESET                                                    150
         AG_ISVOL = 1                                                     150
         AG_TITLE = 'SENSITIVE'                                           150
         AG_EXNAME = 'MATERIAL'                                           150
         AG_PARLIST = 'ISVOL'                                             150
         CALL AXMATERIAL                                                  150
C    *                                                                    150
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=2 <===                       150
         CALL AGSRESET                                                    151
         AG_SEEN = 1                                                      151
         AG_COLO = 2                                                      151
         AG_TITLE = 'PLAC'                                                151
         AG_EXNAME = 'ATTRIBUTE'                                          151
         AG_PARLIST = 'SEEN_COLO'                                         151
         CALL AXATTRIBUTE                                                 151
C    *                                                                    151
C    Geant SHAPE : BOX ===> DX=LADDERWIDTH/2.0 DY=PXLD_ACTIVETHK/2.0 DZ=  155
C    PXLD_TOTALLENGTH/2.0 <===                                            155
         CALL AGSRESET                                                    155
         AG_DX = LADDERWIDTH/2.0                                          155
         AG_DY = PXLD_ACTIVETHK/2.0                                       155
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     155
         AG_TITLE = 'BOX'                                                 155
         AG_EXNAME = 'SHAPE'                                              155
         AG_PARLIST = 'DX_DY_DZ'                                          155
         CALL AXSHAPE                                                     155
C    *                                                                    155
*      call      GSTPAR (%Imed,'STRA',1.)
C    Defining Hits: Z:.00001:S Y:.00001: X:.00001: PTOT:16:(0,100), CX:1  162
C    0: CY:10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS  162
C    :16:(0,0.001)                                                        162
         AG_TITLE = 'PLAC'                                                162
         AG_EXNAME = 'HITS'                                               162
         AG_TYPE=AG_MAGIC                                                 162
         CALL AGSDETV('PLAC')                                             162
C    *                                                                    162
         AG_PARLIST='Z'                                                   162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = AG_MAGIC                                              162
         AG_BIN=.00001                                                    162
         AG_OPTION='S'                                                    162
         AG_FHMIN = AG_MAGIC                                              162
         AG_FHMAX = AG_MAGIC                                              162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='Y'                                                   162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = AG_MAGIC                                              162
         AG_BIN=.00001                                                    162
         AG_FHMIN=AG_MAGIC                                                162
         AG_FHMAX=AG_MAGIC                                                162
         AG_OPTION='*'                                                    162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='X'                                                   162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = AG_MAGIC                                              162
         AG_BIN=.00001                                                    162
         AG_FHMIN=AG_MAGIC                                                162
         AG_FHMAX=AG_MAGIC                                                162
         AG_OPTION='*'                                                    162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='PTOT'                                                162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 16                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_OPTION='*'                                                    162
         AG_FHMIN = 0                                                     162
         AG_FHMAX = 100                                                   162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='CX'                                                  162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 10                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_FHMIN=AG_MAGIC                                                162
         AG_FHMAX=AG_MAGIC                                                162
         AG_OPTION='*'                                                    162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='CY'                                                  162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 10                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_FHMIN=AG_MAGIC                                                162
         AG_FHMAX=AG_MAGIC                                                162
         AG_OPTION='*'                                                    162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='CZ'                                                  162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 10                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_FHMIN=AG_MAGIC                                                162
         AG_FHMAX=AG_MAGIC                                                162
         AG_OPTION='*'                                                    162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='SLENG'                                               162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 16                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_OPTION='*'                                                    162
         AG_FHMIN = 0                                                     162
         AG_FHMAX = 500                                                   162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='TOF'                                                 162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 16                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_OPTION='*'                                                    162
         AG_FHMIN = 0                                                     162
         AG_FHMAX = 1.E-6                                                 162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='STEP'                                                162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = AG_MAGIC                                              162
         AG_BIN=.01                                                       162
         AG_FHMIN=AG_MAGIC                                                162
         AG_FHMAX=AG_MAGIC                                                162
         AG_OPTION='*'                                                    162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         AG_PARLIST='ELOSS'                                               162
         AG_TITLE = 'DETH'                                                162
         AG_NBITS = 16                                                    162
         AG_BIN=AG_MAGIC                                                  162
         AG_OPTION='*'                                                    162
         AG_FHMIN = 0                                                     162
         AG_FHMAX = 0.001                                                 162
         CALL AGSDETH (0)                                                 162
C    *                                                                    162
         CALL AGSDETU('PLAC')                                             162
C    *                                                                    162
5091  CALL AGSPOPB                                                        164
      GO TO BLOCK_PLAC, (5090 ,5100)                                      164
C End of Local Procedure                                                  164
* -----------------------------------------------------------------------------
*Block PXME is the exoskeleton tube
*      Component C A=12 Z=6 W=1
*      Mixture   CFiber Dens=1.713
*      Attribute PXME  Seen=1  colo=3
*
*      Shape TUBE Rmin=PXBG_Rin          _
*                 Rmax=PXBG_Rin+PXBG_Thk _
*                 Dz=PXBG_Length/2.0
*EndBlock
* -----------------------------------------------------------------------------
      END                                                                 177
* $Id: pixlgeo.g,v 1.10 2004/04/21 22:45:57 potekhin Exp $
* $Log: pixlgeo.g,v $
* Revision 1.10  2004/04/21 22:45:57  potekhin
* Killed the extraneous line with angular offset of 90
* for the first ladder, which was a typo and the actual
* angle should be slightly different,  89.28
*
* Revision 1.9  2004/01/29 19:58:12  potekhin
* Corrected a typo in the ladder mother volume
* material -- it's not silicon but air
*
* Revision 1.8  2004/01/19 22:52:09  potekhin
* Some new dimensions
*
* Revision 1.7  2003/10/27 23:59:58  potekhin
* Added the sensitivity and hits definition to the
* wafers' active layers -- just a step to establish the
* hit propagation in the chain  -- subject to change
*
* Revision 1.6  2003/10/22 15:09:01  potekhin
* Finished the changes needed to organize the detector into
* 6 symmetrical sectors, removed a small bug on the tilt calculation,
* and improved the comments.
*
* Revision 1.5  2003/10/16 21:06:01  potekhin
* Notice that this check-in and previous
* both contain only two sectors for brevity.
*
* I changed the notation slightly in the
* angle offset area, and adjusted the sector
* opening angle.
*
* Revision 1.4  2003/10/16 20:47:55  potekhin
* Substantial move forward:
* 1) Introduced the mother volume for each ladder
* which contains both passive and active silicon
* layers (also introduced)
* 2) Introduced mother volume for sectors
* (groups of 4 wafers) which is really the right
* way to code this
* 2) Put in numbers from Kai, but those need
* verification as the inner wafers don't seem to fit
*
* Revision 1.3  2003/10/13 17:15:53  potekhin
* Added more tuning paremters for a more
* precise geometry definition, and a lot
* of documentation to facilitate the
* development by the pixel group members,
* if necessary
*
* Revision 1.2  2003/10/10 23:09:56  potekhin
* A more functional version with one sixth of all
* ladders properly populated. Still a template for
* more development. Data is structured fro easier
* editing by Kai and others.
*
******************************************************************************
      SUBROUTINE PIXLGEO                                                  236
C PIXLGEO IS THE GEOMETRY OF THE STAR PIXEL DETECTOR                      236
      IMPLICIT NONE                                                       236
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       236
      DATA MODULE/'PIXLGEO'/, BANK_TITLE/'*'/                             236
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR PIXEL DETECTOR'/         236
      LOGICAL FIRST                                                       236
      DATA FIRST/.TRUE./                                                  236
      SAVE FIRST                                                          236
      INTEGER JUMPAD                                                      236
      EXTERNAL JUMPAD                                                     236
      INTEGER OK                                                          236
      PARAMETER (OK=0)                                                    236
      INTEGER AG_I,AG_J                                                   236
      SAVE AG_I,AG_J                                                      236
C CREATED 10/09/03                                                        236
      DATA AG_I/0/,AG_J/0/                                                237
      CHARACTER*40 CREATED /'10/09/03'/                                   237
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  237
C AUTHOR MAXIM POTEKHIN                                                   237
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              238
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  238
******************************************************************************
C - Advanced Geant interface                                              240
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      240
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       240
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         240
     *AG_OPTION,AG_ORT,AG_MARK                                            240
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         240
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     240
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     240
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    240
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          240
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   240
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     240
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        240
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   240
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 240
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     240
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       240
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        240
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       240
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       240
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       240
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         240
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     240
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       240
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   240
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       240
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     240
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          240
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         240
     *-696969,AG_LDETU=250)                                               240
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    240
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  240
     *AG_IMFLD,AG_ISIMU                                                   240
C Inherited variables a: saved during internal calls                      240
C b: reset at the entry in a block and then saved                         240
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        240
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       240
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         240
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     240
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       240
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         240
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           240
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        240
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        240
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            240
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           240
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    240
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      240
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           240
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          240
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                240
C local variables valid inside same block                                 240
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       240
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  240
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         240
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    240
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          240
     *AG_OPTION                                                           240
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        240
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             240
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              240
C temporarly                                                              240
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           240
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    240
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       240
     *AG_HY)                                                              240
      REAL MIL_P,INCH_P                                                   240
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               240
C - GEANT description of logical units                                    240
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            240
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      240
      COMMON/GCMAIL/ CHMAIL                                               240
      CHARACTER*132 CHMAIL                                                240
*
      REAL ANGLE,ANGLEPOS,ANGLECORR,RADDEG                                242
      INTEGER NLADDER,NSECTOR                                             243
      INTEGER BLOCK_PXMO                                                  245
      INTEGER BLOCK_PSEC                                                  245
      INTEGER BLOCK_PLMO                                                  245
      INTEGER BLOCK_PLAC                                                  245
      INTEGER BLOCK_PLPS                                                  245
*
C -                                                                       249
      REAL PIXG_LADDER                                                    249
      EQUIVALENCE (BANK_PIXG(3),PIXG_LADDER,RE_PIXG_LADDER),              249
     *(NAMES_PIXG(1),NPIXG_LADDER)                                        249
      EQUIVALENCE (COMMEN_PIXG(1),CPIXG_LADDER),(MAP_PIXG(1,1),           249
     *MPIXG_LADDER)                                                       249
      CHARACTER CPIXG_LADDER*40/' '/                                      249
      CHARACTER NPIXG_LADDER*12/'R.LADDER'/                               249
      INTEGER MPIXG_LADDER(2)/1,1/                                        249
      REAL RE_PIXG_LADDER                                                 249
C -                                                                       249
      REAL PIXG_RIN                                                       249
      EQUIVALENCE (BANK_PIXG(4),PIXG_RIN,RE_PIXG_RIN),(NAMES_PIXG(2),     249
     *NPIXG_RIN)                                                          249
      EQUIVALENCE (COMMEN_PIXG(2),CPIXG_RIN),(MAP_PIXG(1,2),MPIXG_RIN)    249
      CHARACTER CPIXG_RIN*40/' '/                                         249
      CHARACTER NPIXG_RIN*12/'R.RIN'/                                     249
      INTEGER MPIXG_RIN(2)/1,1/                                           249
      REAL RE_PIXG_RIN                                                    249
C -                                                                       249
      REAL PIXG_ROUT                                                      249
      EQUIVALENCE (BANK_PIXG(5),PIXG_ROUT,RE_PIXG_ROUT),(NAMES_PIXG(3),   249
     *NPIXG_ROUT)                                                         249
      EQUIVALENCE (COMMEN_PIXG(3),CPIXG_ROUT),(MAP_PIXG(1,3),             249
     *MPIXG_ROUT)                                                         249
      CHARACTER CPIXG_ROUT*40/' '/                                        249
      CHARACTER NPIXG_ROUT*12/'R.ROUT'/                                   249
      INTEGER MPIXG_ROUT(2)/1,1/                                          249
      REAL RE_PIXG_ROUT                                                   249
C -                                                                       249
      REAL PIXG_TOTALLENGTH                                               249
      EQUIVALENCE (BANK_PIXG(6),PIXG_TOTALLENGTH,RE_PIXG_TOTALLENGTH),    249
     *(NAMES_PIXG(4),NPIXG_TOTALLENGTH)                                   249
      EQUIVALENCE (COMMEN_PIXG(4),CPIXG_TOTALLENGTH),(MAP_PIXG(1,4),      249
     *MPIXG_TOTALLENGTH)                                                  249
      CHARACTER CPIXG_TOTALLENGTH*40/' '/                                 249
      CHARACTER NPIXG_TOTALLENGTH*12/'R.TOTALLENGTH'/                     249
      INTEGER MPIXG_TOTALLENGTH(2)/1,1/                                   249
      REAL RE_PIXG_TOTALLENGTH                                            249
C -                                                                       249
      REAL PIXG_LADDERWIDTH                                               249
      EQUIVALENCE (BANK_PIXG(7),PIXG_LADDERWIDTH,RE_PIXG_LADDERWIDTH),    249
     *(NAMES_PIXG(5),NPIXG_LADDERWIDTH)                                   249
      EQUIVALENCE (COMMEN_PIXG(5),CPIXG_LADDERWIDTH),(MAP_PIXG(1,5),      249
     *MPIXG_LADDERWIDTH)                                                  249
      CHARACTER CPIXG_LADDERWIDTH*40/' '/                                 249
      CHARACTER NPIXG_LADDERWIDTH*12/'R.LADDERWIDTH'/                     249
      INTEGER MPIXG_LADDERWIDTH(2)/1,1/                                   249
      REAL RE_PIXG_LADDERWIDTH                                            249
C -                                                                       249
      REAL PIXG_LADDERTHK                                                 249
      EQUIVALENCE (BANK_PIXG(8),PIXG_LADDERTHK,RE_PIXG_LADDERTHK),        249
     *(NAMES_PIXG(6),NPIXG_LADDERTHK)                                     249
      EQUIVALENCE (COMMEN_PIXG(6),CPIXG_LADDERTHK),(MAP_PIXG(1,6),        249
     *MPIXG_LADDERTHK)                                                    249
      CHARACTER CPIXG_LADDERTHK*40/' '/                                   249
      CHARACTER NPIXG_LADDERTHK*12/'R.LADDERTHK'/                         249
      INTEGER MPIXG_LADDERTHK(2)/1,1/                                     249
      REAL RE_PIXG_LADDERTHK                                              249
C -                                                                       249
      REAL PIXG_PASSIVETHK                                                249
      EQUIVALENCE (BANK_PIXG(9),PIXG_PASSIVETHK,RE_PIXG_PASSIVETHK),      249
     *(NAMES_PIXG(7),NPIXG_PASSIVETHK)                                    249
      EQUIVALENCE (COMMEN_PIXG(7),CPIXG_PASSIVETHK),(MAP_PIXG(1,7),       249
     *MPIXG_PASSIVETHK)                                                   249
      CHARACTER CPIXG_PASSIVETHK*40/' '/                                  249
      CHARACTER NPIXG_PASSIVETHK*12/'R.PASSIVETHK'/                       249
      INTEGER MPIXG_PASSIVETHK(2)/1,1/                                    249
      REAL RE_PIXG_PASSIVETHK                                             249
C -                                                                       249
      REAL PIXG_ACTIVETHK                                                 249
      EQUIVALENCE (BANK_PIXG(10),PIXG_ACTIVETHK,RE_PIXG_ACTIVETHK),       249
     *(NAMES_PIXG(8),NPIXG_ACTIVETHK)                                     249
      EQUIVALENCE (COMMEN_PIXG(8),CPIXG_ACTIVETHK),(MAP_PIXG(1,8),        249
     *MPIXG_ACTIVETHK)                                                    249
      CHARACTER CPIXG_ACTIVETHK*40/' '/                                   249
      CHARACTER NPIXG_ACTIVETHK*12/'R.ACTIVETHK'/                         249
      INTEGER MPIXG_ACTIVETHK(2)/1,1/                                     249
      REAL RE_PIXG_ACTIVETHK                                              249
C -                                                                       249
      REAL PIXG_R                                                         249
      EQUIVALENCE (BANK_PIXG(11),PIXG_R,RE_PIXG_R),(NAMES_PIXG(9),        249
     *NPIXG_R)                                                            249
      EQUIVALENCE (COMMEN_PIXG(9),CPIXG_R),(MAP_PIXG(1,9),MPIXG_R)        249
      CHARACTER CPIXG_R*40/' '/                                           249
      CHARACTER NPIXG_R*12/'R.R'/                                         249
      INTEGER MPIXG_R(2)/1,1/                                             249
      REAL RE_PIXG_R                                                      249
C -                                                                       249
      REAL PIXG_A                                                         249
      EQUIVALENCE (BANK_PIXG(12),PIXG_A,RE_PIXG_A),(NAMES_PIXG(10),       249
     *NPIXG_A)                                                            249
      EQUIVALENCE (COMMEN_PIXG(10),CPIXG_A),(MAP_PIXG(1,10),MPIXG_A)      249
      CHARACTER CPIXG_A*40/' '/                                           249
      CHARACTER NPIXG_A*12/'R.A'/                                         249
      INTEGER MPIXG_A(2)/1,1/                                             249
      REAL RE_PIXG_A                                                      249
C -                                                                       249
      REAL PIXG_POFFSET                                                   249
      EQUIVALENCE (BANK_PIXG(13),PIXG_POFFSET,RE_PIXG_POFFSET),           249
     *(NAMES_PIXG(11),NPIXG_POFFSET)                                      249
      EQUIVALENCE (COMMEN_PIXG(11),CPIXG_POFFSET),(MAP_PIXG(1,11),        249
     *MPIXG_POFFSET)                                                      249
      CHARACTER CPIXG_POFFSET*40/' '/                                     249
      CHARACTER NPIXG_POFFSET*12/'R.POFFSET'/                             249
      INTEGER MPIXG_POFFSET(2)/1,1/                                       249
      REAL RE_PIXG_POFFSET                                                249
C -                                                                       249
      REAL PIXG_AOFFSET                                                   249
      EQUIVALENCE (BANK_PIXG(14),PIXG_AOFFSET,RE_PIXG_AOFFSET),           249
     *(NAMES_PIXG(12),NPIXG_AOFFSET)                                      249
      EQUIVALENCE (COMMEN_PIXG(12),CPIXG_AOFFSET),(MAP_PIXG(1,12),        249
     *MPIXG_AOFFSET)                                                      249
      CHARACTER CPIXG_AOFFSET*40/' '/                                     249
      CHARACTER NPIXG_AOFFSET*12/'R.AOFFSET'/                             249
      INTEGER MPIXG_AOFFSET(2)/1,1/                                       249
      REAL RE_PIXG_AOFFSET                                                249
C -                                                                       249
      INTEGER LEN_PIXG,LLEN_PIXG,LINK_PIXG                                249
      PARAMETER (LEN_PIXG=14)                                             249
      PARAMETER (LLEN_PIXG=12)                                            249
      INTEGER FLAG_PIXG/0/,MAP_PIXG(2,LLEN_PIXG),IDNUM_PIXG(10)           249
      CHARACTER NAMES_PIXG(LLEN_PIXG)*12,COMMEN_PIXG(LLEN_PIXG)*40        249
      DATA IDNUM_PIXG/10*0/                                               249
      REAL BANK_PIXG(LEN_PIXG)                                            249
      CHARACTER*60 BPATH_PIXG/' '/                                        249
      SAVE FLAG_PIXG,MAP_PIXG,IDNUM_PIXG,NAMES_PIXG,COMMEN_PIXG,          249
     *BPATH_PIXG                                                          249
      DATA LINK_PIXG/0/,BANK_PIXG/LEN_PIXG*0/                             249
C *                                                                       249
*
* -----------------------------------------------------------------------------
*
C *                                                                       253
      CALL AGSBEGM(MODULE,IPRIN)                                          253
      IF (IPRIN.LT.0) RETURN                                              253
C Filling bank PIXG - PIXEL DETECTOR DATA                                 253
      BANK_TITLE='PIXEL DETECTOR DATA'                                    253
      BPATH_PIXG='PIXG*'                                                  253
      IDNUM_PIXG(1)=0                                                     254
C LADDER INDEX                                                            254
      PIXG_LADDER=1                                                       254
      CPIXG_LADDER='LADDER INDEX'                                         254
C INNER RADIUS                                                            255
      PIXG_RIN=1.45                                                       255
      CPIXG_RIN='INNER RADIUS'                                            255
C OUTER RADIUS                                                            256
      PIXG_ROUT=5.65                                                      256
      CPIXG_ROUT='OUTER RADIUS'                                           256
C OVERAL LENGTH OF THE DETECTOR                                           257
      PIXG_TOTALLENGTH=16.0                                               257
      CPIXG_TOTALLENGTH='OVERAL LENGTH OF THE DETECTOR'                   257
*
C LADDER WIDTH                                                            259
      PIXG_LADDERWIDTH=2.00                                               259
      CPIXG_LADDERWIDTH='LADDER WIDTH'                                    259
C TOTAL LADDER THICKNESS                                                  260
      PIXG_LADDERTHK=0.0120                                               260
      CPIXG_LADDERTHK='TOTAL LADDER THICKNESS'                            260
C PASSIVE SILICON THICKNESS                                               261
      PIXG_PASSIVETHK=0.0100                                              261
      CPIXG_PASSIVETHK='PASSIVE SILICON THICKNESS'                        261
C ACTIVE SILICON THICKNESS                                                262
      PIXG_ACTIVETHK=0.0020                                               262
      CPIXG_ACTIVETHK='ACTIVE SILICON THICKNESS'                          262
*
C 1ST LADDER NOMINAL RADIUS                                               264
      PIXG_R=5.294                                                        264
      CPIXG_R='1ST LADDER NOMINAL RADIUS'                                 264
C 1ST LADDER NOMINAL POSITION ANGLE                                       265
      PIXG_A=0.0                                                          265
      CPIXG_A='1ST LADDER NOMINAL POSITION ANGLE'                         265
C ANGULAR OFFSET                                                          266
      PIXG_AOFFSET=89.28                                                  266
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      266
*
C POSITION OFFSET (SHIFT)                                                 268
      PIXG_POFFSET=0.0                                                    268
      CPIXG_POFFSET='POSITION OFFSET (SHIFT)'                             268
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    269
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           269
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   269
C *                                                                       269
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    269
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           269
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   269
C *                                                                       271
      CALL AGSBEGM(MODULE,IPRIN)                                          271
      IF (IPRIN.LT.0) RETURN                                              271
C Filling bank PIXG - PIXEL DETECTOR DATA                                 271
      BANK_TITLE='PIXEL DETECTOR DATA'                                    271
      BPATH_PIXG='PIXG*'                                                  271
      IDNUM_PIXG(1)=0                                                     272
C LADDER INDEX                                                            272
      PIXG_LADDER=2                                                       272
      CPIXG_LADDER='LADDER INDEX'                                         272
C 2ND LADDER NOMINAL RADIUS                                               273
      PIXG_R=4.862                                                        273
      CPIXG_R='2ND LADDER NOMINAL RADIUS'                                 273
C 2ND LADDER NOMINAL POSITION ANGLE                                       274
      PIXG_A=20.27                                                        274
      CPIXG_A='2ND LADDER NOMINAL POSITION ANGLE'                         274
C ANGULAR OFFSET                                                          275
      PIXG_AOFFSET=88.31                                                  275
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      275
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    276
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           276
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   276
C *                                                                       276
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    276
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           276
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   276
C *                                                                       278
      CALL AGSBEGM(MODULE,IPRIN)                                          278
      IF (IPRIN.LT.0) RETURN                                              278
C Filling bank PIXG - PIXEL DETECTOR DATA                                 278
      BANK_TITLE='PIXEL DETECTOR DATA'                                    278
      BPATH_PIXG='PIXG*'                                                  278
      IDNUM_PIXG(1)=0                                                     279
C LADDER INDEX                                                            279
      PIXG_LADDER=3                                                       279
      CPIXG_LADDER='LADDER INDEX'                                         279
C 3RD LADDER RADIUS                                                       280
      PIXG_R=4.391                                                        280
      CPIXG_R='3RD LADDER RADIUS'                                         280
C 3RD LADDER NOMINAL POSITION ANGLE                                       281
      PIXG_A=42.62                                                        281
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         281
C ANGULAR OFFSET                                                          282
      PIXG_AOFFSET=87.01                                                  282
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      282
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    283
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           283
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   283
C *                                                                       283
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    283
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           283
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   283
C *                                                                       285
      CALL AGSBEGM(MODULE,IPRIN)                                          285
      IF (IPRIN.LT.0) RETURN                                              285
C Filling bank PIXG - PIXEL DETECTOR DATA                                 285
      BANK_TITLE='PIXEL DETECTOR DATA'                                    285
      BPATH_PIXG='PIXG*'                                                  285
      IDNUM_PIXG(1)=0                                                     286
C LADDER INDEX                                                            286
      PIXG_LADDER=4                                                       286
      CPIXG_LADDER='LADDER INDEX'                                         286
C 4TH LADDER NOMINAL RADIUS                                               287
      PIXG_R=1.595                                                        287
      CPIXG_R='4TH LADDER NOMINAL RADIUS'                                 287
C 4TH LADDER NOMINAL POSITION ANGLE                                       288
      PIXG_A=79.51                                                        288
      CPIXG_A='4TH LADDER NOMINAL POSITION ANGLE'                         288
C ANGULAR OFFSET                                                          289
      PIXG_AOFFSET=70.15                                                  289
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      289
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    290
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           290
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   290
C *                                                                       290
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    290
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           290
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   290
******************************************************
C use default PIXG bank                                                   293
      BPATH_PIXG='PIXG*'                                                  293
      IDNUM_PIXG(1)=0                                                     293
      CALL AGDGETP('PIXLGEO','*',-999,'PIXG',' ',BANK_PIXG, BPATH_PIXG,   293
     *IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,BANK_PIXG,LEN_PIXG,LLEN_PIXG,      293
     *LINK_PIXG,FLAG_PIXG)                                                293
*
      RADDEG=3.14159265/180.0                                             295
      CALL AGSBEGM(MODULE,IPRIN)                                          297
C Check IPRIN<0                                                           297
      IF (IPRIN.LT.0) RETURN                                              297
C Create PXMO :                                                           297
C Call To Local Procedure PXMO                                            297
      ASSIGN 5020 TO BLOCK_PXMO                                           297
      GO TO  5010                                                         297
5020  CONTINUE                                                            297
C *                                                                       297
C Geant POSITION : PXMO ===> MOTHER='SVTT' <===                           298
      CALL AGSRESET                                                       298
      AG_MOTHER = 'SVTT'                                                  298
      AG_TITLE = 'PXMO'                                                   298
      AG_EXNAME = 'POSITION'                                              298
      AG_PARLIST = 'MOTHER'                                               298
      CALL AXPOSITION                                                     298
C *                                                                       298
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        300
      RETURN                                                              300
C Begin Local Procedure PXMO                                              300
C PXMO IS THE MOTHER OF THE PIXEL DETECTOR VOLUMES                        300
5010  CONTINUE                                                            300
         AG_TITLE='PXMO'                                                  300
         AG_MODULE=MODULE                                                 300
         AG_MARK='V00'                                                    300
         CALL AGSPUSH                                                     300
         AG_MODULE=MODULE                                                 300
C    *                                                                    300
C    Geant MATERIAL : AIR ===> inherited arguments used <===              301
         CALL AGSRESET                                                    301
         AG_TITLE = 'AIR'                                                 301
         AG_EXNAME = 'MATERIAL'                                           301
         AG_PARLIST = 'NONE'                                              301
         CALL AXMATERIAL                                                  301
C    *                                                                    301
C    Geant ATTRIBUTE : PXMO ===> SEEN=1 COLO=6 <===                       301
         CALL AGSRESET                                                    302
         AG_SEEN = 1                                                      302
         AG_COLO = 6                                                      302
         AG_TITLE = 'PXMO'                                                302
         AG_EXNAME = 'ATTRIBUTE'                                          302
         AG_PARLIST = 'SEEN_COLO'                                         302
         CALL AXATTRIBUTE                                                 302
C    *                                                                    302
C    Geant SHAPE : TUBE ===> RMIN=PIXG_RIN RMAX=PIXG_ROUT DZ=PIXG_TOTALL  302
C    ENGTH/2.0 <===                                                       302
         CALL AGSRESET                                                    303
         AG_RMIN = PIXG_RIN                                               303
         AG_RMAX = PIXG_ROUT                                              303
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     303
         AG_TITLE = 'TUBE'                                                303
         AG_EXNAME = 'SHAPE'                                              303
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      303
         CALL AXSHAPE                                                     303
C    *                                                                    303
* The "sector" is defined as a group of 4 ladders, we
* have a total of 6 overlapping sectors placed with rotational symmetry
C    Create PSEC :                                                        308
C    Call To Local Procedure PSEC                                         308
         ASSIGN 5040 TO BLOCK_PSEC                                        308
         GO TO  5030                                                      308
5040     CONTINUE                                                         308
C    *                                                                    308
C    *                                                                    309
         NSECTOR=1                                                        309
            GO TO 5053                                                    309
5051        NSECTOR=NSECTOR+1                                             309
5053        IF(NSECTOR-(6).GT.0) GO TO 5052                               309
C       NEED THE 04MANY0 OPTION AS THEY DO OVERLAP                        309
C       Geant POSITION : PSEC ===> ALPHAZ=60.0*(NSECTOR-1) KONLY='MANY'   310
C       <===                                                              310
            CALL AGSRESET                                                 310
            AG_ALPHAN=AG_ALPHAN+1                                         310
            AG_ALPHAK(AG_ALPHAN)=3                                        310
            AG_ALPHAL(AG_ALPHAN)=60.0*(NSECTOR-1)                         310
            AG_KONLY = 'MANY'                                             310
            AG_TITLE = 'PSEC'                                             310
            AG_EXNAME = 'POSITION'                                        310
            AG_PARLIST = 'ALPHAZ_KONLY'                                   310
            CALL AXPOSITION                                               310
C       *                                                                 310
         GO TO 5051                                                       311
5052     CONTINUE                                                         311
5011  CALL AGSPOPB                                                        314
      GO TO BLOCK_PXMO, (5010 ,5020)                                      314
C End of Local Procedure                                                  314
* -----------------------------------------------------------------------------
C Begin Local Procedure PSEC                                              316
C PSEC IS A GROUP OF LADDERS                                              316
5030  CONTINUE                                                            316
         AG_TITLE='PSEC'                                                  316
         AG_MODULE=MODULE                                                 316
         AG_MARK='V00'                                                    316
         CALL AGSPUSH                                                     316
         AG_MODULE=MODULE                                                 316
C    *                                                                    316
C    Geant MATERIAL : AIR ===> inherited arguments used <===              317
         CALL AGSRESET                                                    317
         AG_TITLE = 'AIR'                                                 317
         AG_EXNAME = 'MATERIAL'                                           317
         AG_PARLIST = 'NONE'                                              317
         CALL AXMATERIAL                                                  317
C    *                                                                    317
C    Geant ATTRIBUTE : PSEC ===> SEEN=1 COLO=5 <===                       317
         CALL AGSRESET                                                    318
         AG_SEEN = 1                                                      318
         AG_COLO = 5                                                      318
         AG_TITLE = 'PSEC'                                                318
         AG_EXNAME = 'ATTRIBUTE'                                          318
         AG_PARLIST = 'SEEN_COLO'                                         318
         CALL AXATTRIBUTE                                                 318
C    *                                                                    318
C    Geant SHAPE : TUBS ===> RMIN=PIXG_RIN RMAX=PIXG_ROUT DZ=PIXG_TOTALL  318
C    ENGTH/2.0 PHI1=-11.0 PHI2=118.0 <===                                 318
         CALL AGSRESET                                                    319
         AG_RMIN = PIXG_RIN                                               319
         AG_RMAX = PIXG_ROUT                                              319
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     319
         AG_PHI1 = -11.0                                                  319
         AG_PHI2 = 118.0                                                  319
         AG_TITLE = 'TUBS'                                                319
         AG_EXNAME = 'SHAPE'                                              319
         AG_PARLIST = 'RMIN_RMAX_DZ_PHI1_PHI2'                            319
         CALL AXSHAPE                                                     319
C    *                                                                    319
C    *                                                                    322
         NLADDER=1                                                        322
            GO TO 5063                                                    322
5061        NLADDER=NLADDER+1                                             322
5063        IF(NLADDER-(4).GT.0) GO TO 5062                               322
C       INNER LOOP, CREATE LADDERS INSIDE THE SECTOR                      322
C       use PIXG bank                                                     324
            BPATH_PIXG='PIXG*'                                            324
            IDNUM_PIXG(1)=0                                               324
            PIXG_LADDER=NLADDER                                           324
C       INDEX THE LADDER DATA STRUCTURES                                  324
            CALL AGDGETP('PIXLGEO','*',-999,'PIXG',NPIXG_LADDER,          324
     *      RE_PIXG_LADDER,BPATH_PIXG,IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,   324
     *      BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)             324
            ANGLE = PIXG_A                                                325
* Individual ladders can be individually tilted by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement. (Optional and maybe obsoleted soon: angleCorr= atan(PIXG_pOffset/PIXG_r))
* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)
* have to correct and convert to radians:
C       +ANGLECORR SEE ABOVE CPMMENT                                      336
            ANGLEPOS = ANGLE*RADDEG                                       336
C       Create PLMO :                                                     339
C       Call To Local Procedure PLMO                                      339
            ASSIGN 5080 TO BLOCK_PLMO                                     339
            GO TO  5070                                                   339
5080        CONTINUE                                                      339
C       *                                                                 339
C       Geant POSITION : PLMO ===> X=PIXG_R*COS(ANGLEPOS) Y=PIXG_R*SIN(A  339
C       NGLEPOS) Z=0.0 ALPHAZ=-PIXG_AOFFSET+ANGLE <===                    339
            CALL AGSRESET                                                 339
            AG_X = PIXG_R*COS(ANGLEPOS)                                   339
            AG_Y = PIXG_R*SIN(ANGLEPOS)                                   339
            AG_Z = 0.0                                                    339
            AG_ALPHAN=AG_ALPHAN+1                                         339
            AG_ALPHAK(AG_ALPHAN)=3                                        339
            AG_ALPHAL(AG_ALPHAN)=-PIXG_AOFFSET+ANGLE                      339
            AG_TITLE = 'PLMO'                                             339
            AG_EXNAME = 'POSITION'                                        339
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   339
            CALL AXPOSITION                                               339
C       *                                                                 339
         GO TO 5061                                                       341
5062     CONTINUE                                                         341
5031  CALL AGSPOPB                                                        343
      GO TO BLOCK_PSEC, (5030 ,5040)                                      343
C End of Local Procedure                                                  343
*
* -----------------------------------------------------------------------------
C Begin Local Procedure PLMO                                              346
C PLMO IS THE MOTHER OF THE SILICON LADDER                                346
5070  CONTINUE                                                            346
         AG_TITLE='PLMO'                                                  346
         AG_MODULE=MODULE                                                 346
         AG_MARK='V00'                                                    346
         CALL AGSPUSH                                                     346
         AG_MODULE=MODULE                                                 346
C    *                                                                    346
C    Geant MATERIAL : AIR ===> inherited arguments used <===              347
         CALL AGSRESET                                                    347
         AG_TITLE = 'AIR'                                                 347
         AG_EXNAME = 'MATERIAL'                                           347
         AG_PARLIST = 'NONE'                                              347
         CALL AXMATERIAL                                                  347
C    *                                                                    347
C    Geant ATTRIBUTE : PLMO ===> SEEN=1 COLO=4 <===                       347
         CALL AGSRESET                                                    348
         AG_SEEN = 1                                                      348
         AG_COLO = 4                                                      348
         AG_TITLE = 'PLMO'                                                348
         AG_EXNAME = 'ATTRIBUTE'                                          348
         AG_PARLIST = 'SEEN_COLO'                                         348
         CALL AXATTRIBUTE                                                 348
C    *                                                                    348
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_LADDERTHK/2.  348
C    0 DZ=PIXG_TOTALLENGTH/2.0 <===                                       348
         CALL AGSRESET                                                    349
         AG_DX = PIXG_LADDERWIDTH/2.0                                     349
         AG_DY = PIXG_LADDERTHK/2.0                                       349
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     349
         AG_TITLE = 'BOX'                                                 349
         AG_EXNAME = 'SHAPE'                                              349
         AG_PARLIST = 'DX_DY_DZ'                                          349
         CALL AXSHAPE                                                     349
C    *                                                                    349
C    Create PLAC :                                                        351
C    Call To Local Procedure PLAC                                         351
         ASSIGN 5100 TO BLOCK_PLAC                                        351
         GO TO  5090                                                      351
5100     CONTINUE                                                         351
C    *                                                                    351
C    Geant POSITION : PLAC ===> Y=-PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK/2.0  351
C     <===                                                                351
         CALL AGSRESET                                                    351
         AG_Y = -PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK/2.0                    351
         AG_TITLE = 'PLAC'                                                351
         AG_EXNAME = 'POSITION'                                           351
         AG_PARLIST = 'Y'                                                 351
         CALL AXPOSITION                                                  351
C    *                                                                    351
C    Create PLPS :                                                        352
C    Call To Local Procedure PLPS                                         352
         ASSIGN 5120 TO BLOCK_PLPS                                        352
         GO TO  5110                                                      352
5120     CONTINUE                                                         352
C    *                                                                    352
C    Geant POSITION : PLPS ===> Y=-PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK+PIX  352
C    G_PASSIVETHK/2.0 <===                                                352
         CALL AGSRESET                                                    352
         AG_Y = -PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK+PIXG_PASSIVETHK/2.0    352
         AG_TITLE = 'PLPS'                                                352
         AG_EXNAME = 'POSITION'                                           352
         AG_PARLIST = 'Y'                                                 352
         CALL AXPOSITION                                                  352
C    *                                                                    352
5071  CALL AGSPOPB                                                        354
      GO TO BLOCK_PLMO, (5070 ,5080)                                      354
C End of Local Procedure                                                  354
*
* -----------------------------------------------------------------------------
C Begin Local Procedure PLAC                                              357
C PLAC IS THE ACTIVE LAYER OF THE LADDER                                  357
5090  CONTINUE                                                            357
         AG_TITLE='PLAC'                                                  357
         AG_MODULE=MODULE                                                 357
         AG_MARK='V00'                                                    357
         CALL AGSPUSH                                                     357
         AG_MODULE=MODULE                                                 357
C    *                                                                    357
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          358
         CALL AGSRESET                                                    358
         AG_TITLE = 'SILICON'                                             358
         AG_EXNAME = 'MATERIAL'                                           358
         AG_PARLIST = 'NONE'                                              358
         CALL AXMATERIAL                                                  358
C    *                                                                    358
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         358
         CALL AGSRESET                                                    359
         AG_ISVOL = 1                                                     359
         AG_TITLE = 'SENSITIVE'                                           359
         AG_EXNAME = 'MATERIAL'                                           359
         AG_PARLIST = 'ISVOL'                                             359
         CALL AXMATERIAL                                                  359
C    *                                                                    359
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=4 <===                       359
         CALL AGSRESET                                                    360
         AG_SEEN = 1                                                      360
         AG_COLO = 4                                                      360
         AG_TITLE = 'PLAC'                                                360
         AG_EXNAME = 'ATTRIBUTE'                                          360
         AG_PARLIST = 'SEEN_COLO'                                         360
         CALL AXATTRIBUTE                                                 360
C    *                                                                    360
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_ACTIVETHK/2.  362
C    0 DZ=PIXG_TOTALLENGTH/2.0 <===                                       362
         CALL AGSRESET                                                    362
         AG_DX = PIXG_LADDERWIDTH/2.0                                     362
         AG_DY = PIXG_ACTIVETHK/2.0                                       362
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     362
         AG_TITLE = 'BOX'                                                 362
         AG_EXNAME = 'SHAPE'                                              362
         AG_PARLIST = 'DX_DY_DZ'                                          362
         CALL AXSHAPE                                                     362
C    *                                                                    362
         CALL GSTPAR (AG_IMED,'STRA',1.)                                  364
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  368
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  368
C    ,0.001)                                                              368
         AG_TITLE = 'PLAC'                                                368
         AG_EXNAME = 'HITS'                                               368
         AG_TYPE=AG_MAGIC                                                 368
         CALL AGSDETV('PLAC')                                             368
C    *                                                                    368
         AG_PARLIST='Z'                                                   368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = AG_MAGIC                                              368
         AG_BIN=.001                                                      368
         AG_OPTION='S'                                                    368
         AG_FHMIN = AG_MAGIC                                              368
         AG_FHMAX = AG_MAGIC                                              368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='Y'                                                   368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = AG_MAGIC                                              368
         AG_BIN=.001                                                      368
         AG_FHMIN=AG_MAGIC                                                368
         AG_FHMAX=AG_MAGIC                                                368
         AG_OPTION='*'                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='X'                                                   368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = AG_MAGIC                                              368
         AG_BIN=.001                                                      368
         AG_FHMIN=AG_MAGIC                                                368
         AG_FHMAX=AG_MAGIC                                                368
         AG_OPTION='*'                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='PTOT'                                                368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 16                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_OPTION='*'                                                    368
         AG_FHMIN = 0                                                     368
         AG_FHMAX = 100                                                   368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='CX'                                                  368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 10                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_FHMIN=AG_MAGIC                                                368
         AG_FHMAX=AG_MAGIC                                                368
         AG_OPTION='*'                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='CY'                                                  368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 10                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_FHMIN=AG_MAGIC                                                368
         AG_FHMAX=AG_MAGIC                                                368
         AG_OPTION='*'                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='CZ'                                                  368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 10                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_FHMIN=AG_MAGIC                                                368
         AG_FHMAX=AG_MAGIC                                                368
         AG_OPTION='*'                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='SLENG'                                               368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 16                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_OPTION='*'                                                    368
         AG_FHMIN = 0                                                     368
         AG_FHMAX = 500                                                   368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='TOF'                                                 368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 16                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_OPTION='*'                                                    368
         AG_FHMIN = 0                                                     368
         AG_FHMAX = 1.E-6                                                 368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='STEP'                                                368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = AG_MAGIC                                              368
         AG_BIN=.01                                                       368
         AG_FHMIN=AG_MAGIC                                                368
         AG_FHMAX=AG_MAGIC                                                368
         AG_OPTION='*'                                                    368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         AG_PARLIST='ELOSS'                                               368
         AG_TITLE = 'DETH'                                                368
         AG_NBITS = 16                                                    368
         AG_BIN=AG_MAGIC                                                  368
         AG_OPTION='*'                                                    368
         AG_FHMIN = 0                                                     368
         AG_FHMAX = 0.001                                                 368
         CALL AGSDETH (0)                                                 368
C    *                                                                    368
         CALL AGSDETU('PLAC')                                             368
C    *                                                                    368
5091  CALL AGSPOPB                                                        369
      GO TO BLOCK_PLAC, (5090 ,5100)                                      369
C End of Local Procedure                                                  369
* -----------------------------------------------------------------------------
C Begin Local Procedure PLPS                                              371
C PLPS IS THE PASSIVE LAYER OF THE LADDER                                 371
5110  CONTINUE                                                            371
         AG_TITLE='PLPS'                                                  371
         AG_MODULE=MODULE                                                 371
         AG_MARK='V00'                                                    371
         CALL AGSPUSH                                                     371
         AG_MODULE=MODULE                                                 371
C    *                                                                    371
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          372
         CALL AGSRESET                                                    372
         AG_TITLE = 'SILICON'                                             372
         AG_EXNAME = 'MATERIAL'                                           372
         AG_PARLIST = 'NONE'                                              372
         CALL AXMATERIAL                                                  372
C    *                                                                    372
C    Geant ATTRIBUTE : PLPS ===> SEEN=1 COLO=2 <===                       372
         CALL AGSRESET                                                    373
         AG_SEEN = 1                                                      373
         AG_COLO = 2                                                      373
         AG_TITLE = 'PLPS'                                                373
         AG_EXNAME = 'ATTRIBUTE'                                          373
         AG_PARLIST = 'SEEN_COLO'                                         373
         CALL AXATTRIBUTE                                                 373
C    *                                                                    373
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_PASSIVETHK/2  373
C    .0 DZ=PIXG_TOTALLENGTH/2.0 <===                                      373
         CALL AGSRESET                                                    374
         AG_DX = PIXG_LADDERWIDTH/2.0                                     374
         AG_DY = PIXG_PASSIVETHK/2.0                                      374
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     374
         AG_TITLE = 'BOX'                                                 374
         AG_EXNAME = 'SHAPE'                                              374
         AG_PARLIST = 'DX_DY_DZ'                                          374
         CALL AXSHAPE                                                     374
C    *                                                                    374
5111  CALL AGSPOPB                                                        374
      GO TO BLOCK_PLPS, (5110 ,5120)                                      375
C End of Local Procedure                                                  375
*
      END                                                                 377
* $Id: pixlgeo1.g,v 1.4 2005/01/06 01:59:40 potekhin Exp $
* $Log: pixlgeo1.g,v $
* Revision 1.4  2005/01/06 01:59:40  potekhin
* Added an important comment, corrected a typo in another
*
* Revision 1.3  2004/07/15 16:28:40  potekhin
* Changes the outer radius of the mother volume to a more
* reasonable value
*
* Revision 1.2  2004/06/28 22:51:02  potekhin
* Cleaned out the extra ladders which were moved to
* the MIT detector. Based on the original pixel but
* positioned directly in cave, not inside svt
*
* Revision 1.1  2004/06/28 20:46:33  potekhin
* R&D work with the pixel-based inner tracker (replacing
* SVT) will require different configurations of the "inner"
* pixel detector itself, hence we factor this out into a
* separate piece of code
*
******************************************************************************
      SUBROUTINE PIXLGEO1                                                 400
C PIXLGEO1 IS THE GEOMETRY OF THE STAR PIXEL DETECTOR                     400
      IMPLICIT NONE                                                       400
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       400
      DATA MODULE/'PIXLGEO1'/, BANK_TITLE/'*'/                            400
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR PIXEL DETECTOR'/         400
      LOGICAL FIRST                                                       400
      DATA FIRST/.TRUE./                                                  400
      SAVE FIRST                                                          400
      INTEGER JUMPAD                                                      400
      EXTERNAL JUMPAD                                                     400
      INTEGER OK                                                          400
      PARAMETER (OK=0)                                                    400
      INTEGER AG_I,AG_J                                                   400
      SAVE AG_I,AG_J                                                      400
C CREATED 10/09/03                                                        400
      DATA AG_I/0/,AG_J/0/                                                401
      CHARACTER*40 CREATED /'10/09/03'/                                   401
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  401
C AUTHOR MAXIM POTEKHIN                                                   401
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              402
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  402
******************************************************************************
C - Advanced Geant interface                                              404
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      404
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       404
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         404
     *AG_OPTION,AG_ORT,AG_MARK                                            404
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         404
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     404
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     404
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    404
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          404
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   404
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     404
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        404
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   404
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 404
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     404
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       404
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        404
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       404
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       404
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       404
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         404
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     404
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       404
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   404
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       404
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     404
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          404
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         404
     *-696969,AG_LDETU=250)                                               404
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    404
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  404
     *AG_IMFLD,AG_ISIMU                                                   404
C Inherited variables a: saved during internal calls                      404
C b: reset at the entry in a block and then saved                         404
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        404
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       404
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         404
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     404
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       404
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         404
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           404
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        404
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        404
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            404
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           404
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    404
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      404
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           404
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          404
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                404
C local variables valid inside same block                                 404
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       404
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  404
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         404
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    404
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          404
     *AG_OPTION                                                           404
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        404
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             404
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              404
C temporarly                                                              404
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           404
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    404
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       404
     *AG_HY)                                                              404
      REAL MIL_P,INCH_P                                                   404
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               404
C - GEANT description of logical units                                    404
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            404
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      404
      COMMON/GCMAIL/ CHMAIL                                               404
      CHARACTER*132 CHMAIL                                                404
*
      REAL ANGLE,ANGLEPOS,ANGLECORR,RADDEG                                406
      INTEGER NLADDER,NSECTOR, NEXTRALADDER                               407
      INTEGER BLOCK_PXMO                                                  409
      INTEGER BLOCK_PSEC                                                  409
      INTEGER BLOCK_PLMO                                                  409
      INTEGER BLOCK_PLAC                                                  409
      INTEGER BLOCK_PLPS                                                  409
*
C -                                                                       413
      REAL PIXG_LADDER                                                    413
      EQUIVALENCE (BANK_PIXG(3),PIXG_LADDER,RE_PIXG_LADDER),              413
     *(NAMES_PIXG(1),NPIXG_LADDER)                                        413
      EQUIVALENCE (COMMEN_PIXG(1),CPIXG_LADDER),(MAP_PIXG(1,1),           413
     *MPIXG_LADDER)                                                       413
      CHARACTER CPIXG_LADDER*40/' '/                                      413
      CHARACTER NPIXG_LADDER*12/'R.LADDER'/                               413
      INTEGER MPIXG_LADDER(2)/1,1/                                        413
      REAL RE_PIXG_LADDER                                                 413
C -                                                                       413
      REAL PIXG_RIN                                                       413
      EQUIVALENCE (BANK_PIXG(4),PIXG_RIN,RE_PIXG_RIN),(NAMES_PIXG(2),     413
     *NPIXG_RIN)                                                          413
      EQUIVALENCE (COMMEN_PIXG(2),CPIXG_RIN),(MAP_PIXG(1,2),MPIXG_RIN)    413
      CHARACTER CPIXG_RIN*40/' '/                                         413
      CHARACTER NPIXG_RIN*12/'R.RIN'/                                     413
      INTEGER MPIXG_RIN(2)/1,1/                                           413
      REAL RE_PIXG_RIN                                                    413
C -                                                                       413
      REAL PIXG_ROUT                                                      413
      EQUIVALENCE (BANK_PIXG(5),PIXG_ROUT,RE_PIXG_ROUT),(NAMES_PIXG(3),   413
     *NPIXG_ROUT)                                                         413
      EQUIVALENCE (COMMEN_PIXG(3),CPIXG_ROUT),(MAP_PIXG(1,3),             413
     *MPIXG_ROUT)                                                         413
      CHARACTER CPIXG_ROUT*40/' '/                                        413
      CHARACTER NPIXG_ROUT*12/'R.ROUT'/                                   413
      INTEGER MPIXG_ROUT(2)/1,1/                                          413
      REAL RE_PIXG_ROUT                                                   413
C -                                                                       413
      REAL PIXG_TOTALLENGTH                                               413
      EQUIVALENCE (BANK_PIXG(6),PIXG_TOTALLENGTH,RE_PIXG_TOTALLENGTH),    413
     *(NAMES_PIXG(4),NPIXG_TOTALLENGTH)                                   413
      EQUIVALENCE (COMMEN_PIXG(4),CPIXG_TOTALLENGTH),(MAP_PIXG(1,4),      413
     *MPIXG_TOTALLENGTH)                                                  413
      CHARACTER CPIXG_TOTALLENGTH*40/' '/                                 413
      CHARACTER NPIXG_TOTALLENGTH*12/'R.TOTALLENGTH'/                     413
      INTEGER MPIXG_TOTALLENGTH(2)/1,1/                                   413
      REAL RE_PIXG_TOTALLENGTH                                            413
C -                                                                       413
      REAL PIXG_LADDERWIDTH                                               413
      EQUIVALENCE (BANK_PIXG(7),PIXG_LADDERWIDTH,RE_PIXG_LADDERWIDTH),    413
     *(NAMES_PIXG(5),NPIXG_LADDERWIDTH)                                   413
      EQUIVALENCE (COMMEN_PIXG(5),CPIXG_LADDERWIDTH),(MAP_PIXG(1,5),      413
     *MPIXG_LADDERWIDTH)                                                  413
      CHARACTER CPIXG_LADDERWIDTH*40/' '/                                 413
      CHARACTER NPIXG_LADDERWIDTH*12/'R.LADDERWIDTH'/                     413
      INTEGER MPIXG_LADDERWIDTH(2)/1,1/                                   413
      REAL RE_PIXG_LADDERWIDTH                                            413
C -                                                                       413
      REAL PIXG_LADDERTHK                                                 413
      EQUIVALENCE (BANK_PIXG(8),PIXG_LADDERTHK,RE_PIXG_LADDERTHK),        413
     *(NAMES_PIXG(6),NPIXG_LADDERTHK)                                     413
      EQUIVALENCE (COMMEN_PIXG(6),CPIXG_LADDERTHK),(MAP_PIXG(1,6),        413
     *MPIXG_LADDERTHK)                                                    413
      CHARACTER CPIXG_LADDERTHK*40/' '/                                   413
      CHARACTER NPIXG_LADDERTHK*12/'R.LADDERTHK'/                         413
      INTEGER MPIXG_LADDERTHK(2)/1,1/                                     413
      REAL RE_PIXG_LADDERTHK                                              413
C -                                                                       413
      REAL PIXG_PASSIVETHK                                                413
      EQUIVALENCE (BANK_PIXG(9),PIXG_PASSIVETHK,RE_PIXG_PASSIVETHK),      413
     *(NAMES_PIXG(7),NPIXG_PASSIVETHK)                                    413
      EQUIVALENCE (COMMEN_PIXG(7),CPIXG_PASSIVETHK),(MAP_PIXG(1,7),       413
     *MPIXG_PASSIVETHK)                                                   413
      CHARACTER CPIXG_PASSIVETHK*40/' '/                                  413
      CHARACTER NPIXG_PASSIVETHK*12/'R.PASSIVETHK'/                       413
      INTEGER MPIXG_PASSIVETHK(2)/1,1/                                    413
      REAL RE_PIXG_PASSIVETHK                                             413
C -                                                                       413
      REAL PIXG_ACTIVETHK                                                 413
      EQUIVALENCE (BANK_PIXG(10),PIXG_ACTIVETHK,RE_PIXG_ACTIVETHK),       413
     *(NAMES_PIXG(8),NPIXG_ACTIVETHK)                                     413
      EQUIVALENCE (COMMEN_PIXG(8),CPIXG_ACTIVETHK),(MAP_PIXG(1,8),        413
     *MPIXG_ACTIVETHK)                                                    413
      CHARACTER CPIXG_ACTIVETHK*40/' '/                                   413
      CHARACTER NPIXG_ACTIVETHK*12/'R.ACTIVETHK'/                         413
      INTEGER MPIXG_ACTIVETHK(2)/1,1/                                     413
      REAL RE_PIXG_ACTIVETHK                                              413
C -                                                                       413
      REAL PIXG_R                                                         413
      EQUIVALENCE (BANK_PIXG(11),PIXG_R,RE_PIXG_R),(NAMES_PIXG(9),        413
     *NPIXG_R)                                                            413
      EQUIVALENCE (COMMEN_PIXG(9),CPIXG_R),(MAP_PIXG(1,9),MPIXG_R)        413
      CHARACTER CPIXG_R*40/' '/                                           413
      CHARACTER NPIXG_R*12/'R.R'/                                         413
      INTEGER MPIXG_R(2)/1,1/                                             413
      REAL RE_PIXG_R                                                      413
C -                                                                       413
      REAL PIXG_A                                                         413
      EQUIVALENCE (BANK_PIXG(12),PIXG_A,RE_PIXG_A),(NAMES_PIXG(10),       413
     *NPIXG_A)                                                            413
      EQUIVALENCE (COMMEN_PIXG(10),CPIXG_A),(MAP_PIXG(1,10),MPIXG_A)      413
      CHARACTER CPIXG_A*40/' '/                                           413
      CHARACTER NPIXG_A*12/'R.A'/                                         413
      INTEGER MPIXG_A(2)/1,1/                                             413
      REAL RE_PIXG_A                                                      413
C -                                                                       413
      REAL PIXG_POFFSET                                                   413
      EQUIVALENCE (BANK_PIXG(13),PIXG_POFFSET,RE_PIXG_POFFSET),           413
     *(NAMES_PIXG(11),NPIXG_POFFSET)                                      413
      EQUIVALENCE (COMMEN_PIXG(11),CPIXG_POFFSET),(MAP_PIXG(1,11),        413
     *MPIXG_POFFSET)                                                      413
      CHARACTER CPIXG_POFFSET*40/' '/                                     413
      CHARACTER NPIXG_POFFSET*12/'R.POFFSET'/                             413
      INTEGER MPIXG_POFFSET(2)/1,1/                                       413
      REAL RE_PIXG_POFFSET                                                413
C -                                                                       413
      REAL PIXG_AOFFSET                                                   413
      EQUIVALENCE (BANK_PIXG(14),PIXG_AOFFSET,RE_PIXG_AOFFSET),           413
     *(NAMES_PIXG(12),NPIXG_AOFFSET)                                      413
      EQUIVALENCE (COMMEN_PIXG(12),CPIXG_AOFFSET),(MAP_PIXG(1,12),        413
     *MPIXG_AOFFSET)                                                      413
      CHARACTER CPIXG_AOFFSET*40/' '/                                     413
      CHARACTER NPIXG_AOFFSET*12/'R.AOFFSET'/                             413
      INTEGER MPIXG_AOFFSET(2)/1,1/                                       413
      REAL RE_PIXG_AOFFSET                                                413
C -                                                                       413
      INTEGER LEN_PIXG,LLEN_PIXG,LINK_PIXG                                413
      PARAMETER (LEN_PIXG=14)                                             413
      PARAMETER (LLEN_PIXG=12)                                            413
      INTEGER FLAG_PIXG/0/,MAP_PIXG(2,LLEN_PIXG),IDNUM_PIXG(10)           413
      CHARACTER NAMES_PIXG(LLEN_PIXG)*12,COMMEN_PIXG(LLEN_PIXG)*40        413
      DATA IDNUM_PIXG/10*0/                                               413
      REAL BANK_PIXG(LEN_PIXG)                                            413
      CHARACTER*60 BPATH_PIXG/' '/                                        413
      SAVE FLAG_PIXG,MAP_PIXG,IDNUM_PIXG,NAMES_PIXG,COMMEN_PIXG,          413
     *BPATH_PIXG                                                          413
      DATA LINK_PIXG/0/,BANK_PIXG/LEN_PIXG*0/                             413
C *                                                                       413
*
* -----------------------------------------------------------------------------
*
C *                                                                       417
      CALL AGSBEGM(MODULE,IPRIN)                                          417
      IF (IPRIN.LT.0) RETURN                                              417
C Filling bank PIXG - PIXEL DETECTOR DATA                                 417
      BANK_TITLE='PIXEL DETECTOR DATA'                                    417
      BPATH_PIXG='PIXG*'                                                  417
      IDNUM_PIXG(1)=0                                                     418
C LADDER INDEX                                                            418
      PIXG_LADDER=1                                                       418
      CPIXG_LADDER='LADDER INDEX'                                         418
C INNER RADIUS                                                            419
      PIXG_RIN=1.45                                                       419
      CPIXG_RIN='INNER RADIUS'                                            419
C OUTER RADIUS                                                            420
      PIXG_ROUT=5.65                                                      420
      CPIXG_ROUT='OUTER RADIUS'                                           420
C OVERAL LENGTH OF THE DETECTOR                                           421
      PIXG_TOTALLENGTH=16.0                                               421
      CPIXG_TOTALLENGTH='OVERAL LENGTH OF THE DETECTOR'                   421
*
C LADDER WIDTH                                                            423
      PIXG_LADDERWIDTH=2.00                                               423
      CPIXG_LADDERWIDTH='LADDER WIDTH'                                    423
C TOTAL LADDER THICKNESS                                                  424
      PIXG_LADDERTHK=0.0120                                               424
      CPIXG_LADDERTHK='TOTAL LADDER THICKNESS'                            424
C PASSIVE SILICON THICKNESS                                               425
      PIXG_PASSIVETHK=0.0100                                              425
      CPIXG_PASSIVETHK='PASSIVE SILICON THICKNESS'                        425
C ACTIVE SILICON THICKNESS                                                426
      PIXG_ACTIVETHK=0.0020                                               426
      CPIXG_ACTIVETHK='ACTIVE SILICON THICKNESS'                          426
*
C 1ST LADDER NOMINAL RADIUS                                               428
      PIXG_R=5.294                                                        428
      CPIXG_R='1ST LADDER NOMINAL RADIUS'                                 428
C 1ST LADDER NOMINAL POSITION ANGLE                                       429
      PIXG_A=0.0                                                          429
      CPIXG_A='1ST LADDER NOMINAL POSITION ANGLE'                         429
C ANGULAR OFFSET                                                          430
      PIXG_AOFFSET=89.28                                                  430
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      430
*
C POSITION OFFSET (SHIFT)                                                 432
      PIXG_POFFSET=0.0                                                    432
      CPIXG_POFFSET='POSITION OFFSET (SHIFT)'                             432
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    433
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           433
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   433
C *                                                                       433
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    433
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           433
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   433
C *                                                                       435
      CALL AGSBEGM(MODULE,IPRIN)                                          435
      IF (IPRIN.LT.0) RETURN                                              435
C Filling bank PIXG - PIXEL DETECTOR DATA                                 435
      BANK_TITLE='PIXEL DETECTOR DATA'                                    435
      BPATH_PIXG='PIXG*'                                                  435
      IDNUM_PIXG(1)=0                                                     436
C LADDER INDEX                                                            436
      PIXG_LADDER=2                                                       436
      CPIXG_LADDER='LADDER INDEX'                                         436
C 2ND LADDER NOMINAL RADIUS                                               437
      PIXG_R=4.862                                                        437
      CPIXG_R='2ND LADDER NOMINAL RADIUS'                                 437
C 2ND LADDER NOMINAL POSITION ANGLE                                       438
      PIXG_A=20.27                                                        438
      CPIXG_A='2ND LADDER NOMINAL POSITION ANGLE'                         438
C ANGULAR OFFSET                                                          439
      PIXG_AOFFSET=88.31                                                  439
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      439
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    440
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           440
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   440
C *                                                                       440
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    440
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           440
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   440
C *                                                                       442
      CALL AGSBEGM(MODULE,IPRIN)                                          442
      IF (IPRIN.LT.0) RETURN                                              442
C Filling bank PIXG - PIXEL DETECTOR DATA                                 442
      BANK_TITLE='PIXEL DETECTOR DATA'                                    442
      BPATH_PIXG='PIXG*'                                                  442
      IDNUM_PIXG(1)=0                                                     443
C LADDER INDEX                                                            443
      PIXG_LADDER=3                                                       443
      CPIXG_LADDER='LADDER INDEX'                                         443
C 3RD LADDER RADIUS                                                       444
      PIXG_R=4.391                                                        444
      CPIXG_R='3RD LADDER RADIUS'                                         444
C 3RD LADDER NOMINAL POSITION ANGLE                                       445
      PIXG_A=42.62                                                        445
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         445
C ANGULAR OFFSET                                                          446
      PIXG_AOFFSET=87.01                                                  446
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      446
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    447
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           447
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   447
C *                                                                       447
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    447
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           447
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   447
C *                                                                       449
      CALL AGSBEGM(MODULE,IPRIN)                                          449
      IF (IPRIN.LT.0) RETURN                                              449
C Filling bank PIXG - PIXEL DETECTOR DATA                                 449
      BANK_TITLE='PIXEL DETECTOR DATA'                                    449
      BPATH_PIXG='PIXG*'                                                  449
      IDNUM_PIXG(1)=0                                                     450
C LADDER INDEX                                                            450
      PIXG_LADDER=4                                                       450
      CPIXG_LADDER='LADDER INDEX'                                         450
C 4TH LADDER NOMINAL RADIUS                                               451
      PIXG_R=1.595                                                        451
      CPIXG_R='4TH LADDER NOMINAL RADIUS'                                 451
C 4TH LADDER NOMINAL POSITION ANGLE                                       452
      PIXG_A=79.51                                                        452
      CPIXG_A='4TH LADDER NOMINAL POSITION ANGLE'                         452
C ANGULAR OFFSET                                                          453
      PIXG_AOFFSET=70.15                                                  453
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      453
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    454
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           454
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   454
C *                                                                       454
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    454
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           454
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   454
******************************************************
C use default PIXG bank                                                   457
      BPATH_PIXG='PIXG*'                                                  457
      IDNUM_PIXG(1)=0                                                     457
      CALL AGDGETP('PIXLGEO1','*',-999,'PIXG',' ',BANK_PIXG, BPATH_PIXG,  457
     *IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,BANK_PIXG,LEN_PIXG,LLEN_PIXG,      457
     *LINK_PIXG,FLAG_PIXG)                                                457
*
      RADDEG=3.14159265/180.0                                             459
      CALL AGSBEGM(MODULE,IPRIN)                                          461
C Check IPRIN<0                                                           461
      IF (IPRIN.LT.0) RETURN                                              461
C Create PXMO :                                                           461
C Call To Local Procedure PXMO                                            461
      ASSIGN 5020 TO BLOCK_PXMO                                           461
      GO TO  5010                                                         461
5020  CONTINUE                                                            461
C *                                                                       461
C Geant POSITION : PXMO ===> MOTHER='CAVE' <===                           462
      CALL AGSRESET                                                       462
      AG_MOTHER = 'CAVE'                                                  462
      AG_TITLE = 'PXMO'                                                   462
      AG_EXNAME = 'POSITION'                                              462
      AG_PARLIST = 'MOTHER'                                               462
      CALL AXPOSITION                                                     462
C *                                                                       462
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        464
      RETURN                                                              464
C Begin Local Procedure PXMO                                              464
C PXMO IS THE MOTHER OF THE PIXEL DETECTOR VOLUMES                        464
5010  CONTINUE                                                            464
         AG_TITLE='PXMO'                                                  464
         AG_MODULE=MODULE                                                 464
         AG_MARK='V00'                                                    464
         CALL AGSPUSH                                                     464
         AG_MODULE=MODULE                                                 464
C    *                                                                    464
C    Geant MATERIAL : AIR ===> inherited arguments used <===              465
         CALL AGSRESET                                                    465
         AG_TITLE = 'AIR'                                                 465
         AG_EXNAME = 'MATERIAL'                                           465
         AG_PARLIST = 'NONE'                                              465
         CALL AXMATERIAL                                                  465
C    *                                                                    465
C    Geant ATTRIBUTE : PXMO ===> SEEN=1 COLO=6 <===                       465
         CALL AGSRESET                                                    466
         AG_SEEN = 1                                                      466
         AG_COLO = 6                                                      466
         AG_TITLE = 'PXMO'                                                466
         AG_EXNAME = 'ATTRIBUTE'                                          466
         AG_PARLIST = 'SEEN_COLO'                                         466
         CALL AXATTRIBUTE                                                 466
C    *                                                                    466
C    Geant SHAPE : TUBE ===> RMIN=PIXG_RIN RMAX=PIXG_ROUT DZ=PIXG_TOTALL  468
C    ENGTH/2.0 <===                                                       468
         CALL AGSRESET                                                    468
         AG_RMIN = PIXG_RIN                                               468
         AG_RMAX = PIXG_ROUT                                              468
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     468
         AG_TITLE = 'TUBE'                                                468
         AG_EXNAME = 'SHAPE'                                              468
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      468
         CALL AXSHAPE                                                     468
C    *                                                                    468
* The "sector" is defined as a group of 4 ladders, we
* have a total of 6 overlapping sectors placed with rotational symmetry
C    Create PSEC :                                                        473
C    Call To Local Procedure PSEC                                         473
         ASSIGN 5040 TO BLOCK_PSEC                                        473
         GO TO  5030                                                      473
5040     CONTINUE                                                         473
C    *                                                                    473
C    *                                                                    474
         NSECTOR=1                                                        474
            GO TO 5053                                                    474
5051        NSECTOR=NSECTOR+1                                             474
5053        IF(NSECTOR-(6).GT.0) GO TO 5052                               474
C       NEED THE 04MANY0 OPTION AS THEY DO OVERLAP                        474
C       Geant POSITION : PSEC ===> ALPHAZ=60.0*(NSECTOR-1) KONLY='MANY'   475
C       <===                                                              475
            CALL AGSRESET                                                 475
            AG_ALPHAN=AG_ALPHAN+1                                         475
            AG_ALPHAK(AG_ALPHAN)=3                                        475
            AG_ALPHAL(AG_ALPHAN)=60.0*(NSECTOR-1)                         475
            AG_KONLY = 'MANY'                                             475
            AG_TITLE = 'PSEC'                                             475
            AG_EXNAME = 'POSITION'                                        475
            AG_PARLIST = 'ALPHAZ_KONLY'                                   475
            CALL AXPOSITION                                               475
C       *                                                                 475
         GO TO 5051                                                       476
5052     CONTINUE                                                         476
5011  CALL AGSPOPB                                                        479
      GO TO BLOCK_PXMO, (5010 ,5020)                                      479
C End of Local Procedure                                                  479
* -----------------------------------------------------------------------------
C Begin Local Procedure PSEC                                              481
C PSEC IS A GROUP OF LADDERS                                              481
5030  CONTINUE                                                            481
         AG_TITLE='PSEC'                                                  481
         AG_MODULE=MODULE                                                 481
         AG_MARK='V00'                                                    481
         CALL AGSPUSH                                                     481
         AG_MODULE=MODULE                                                 481
C    *                                                                    481
C    Geant MATERIAL : AIR ===> inherited arguments used <===              482
         CALL AGSRESET                                                    482
         AG_TITLE = 'AIR'                                                 482
         AG_EXNAME = 'MATERIAL'                                           482
         AG_PARLIST = 'NONE'                                              482
         CALL AXMATERIAL                                                  482
C    *                                                                    482
C    Geant ATTRIBUTE : PSEC ===> SEEN=1 COLO=5 <===                       482
         CALL AGSRESET                                                    483
         AG_SEEN = 1                                                      483
         AG_COLO = 5                                                      483
         AG_TITLE = 'PSEC'                                                483
         AG_EXNAME = 'ATTRIBUTE'                                          483
         AG_PARLIST = 'SEEN_COLO'                                         483
         CALL AXATTRIBUTE                                                 483
C    *                                                                    483
C    Geant SHAPE : TUBS ===> RMIN=PIXG_RIN RMAX=PIXG_ROUT DZ=PIXG_TOTALL  483
C    ENGTH/2.0 PHI1=-11.0 PHI2=118.0 <===                                 483
         CALL AGSRESET                                                    484
         AG_RMIN = PIXG_RIN                                               484
         AG_RMAX = PIXG_ROUT                                              484
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     484
         AG_PHI1 = -11.0                                                  484
         AG_PHI2 = 118.0                                                  484
         AG_TITLE = 'TUBS'                                                484
         AG_EXNAME = 'SHAPE'                                              484
         AG_PARLIST = 'RMIN_RMAX_DZ_PHI1_PHI2'                            484
         CALL AXSHAPE                                                     484
C    *                                                                    484
C    *                                                                    487
         NLADDER=1                                                        487
            GO TO 5063                                                    487
5061        NLADDER=NLADDER+1                                             487
5063        IF(NLADDER-(4).GT.0) GO TO 5062                               487
C       INNER LOOP, CREATE LADDERS INSIDE THE SECTOR                      487
C       use PIXG bank                                                     489
            BPATH_PIXG='PIXG*'                                            489
            IDNUM_PIXG(1)=0                                               489
            PIXG_LADDER=NLADDER                                           489
C       INDEX THE LADDER DATA STRUCTURES                                  489
            CALL AGDGETP('PIXLGEO1','*',-999,'PIXG',NPIXG_LADDER,         489
     *      RE_PIXG_LADDER,BPATH_PIXG,IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,   489
     *      BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)             489
            ANGLE = PIXG_A                                                490
* Individual ladders can be individually tilted by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement. (Optional and maybe obsoleted soon: angleCorr= atan(PIXG_pOffset/PIXG_r))
* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)
* have to correct and convert to radians:
C       +ANGLECORR SEE ABOVE COMMENT                                      501
            ANGLEPOS = ANGLE*RADDEG                                       501
* In case we do go with the pOffset, don't forget to correct the radius,
* to keep the surfaces at the nominal DCA to the beam
C       Create PLMO :                                                     507
C       Call To Local Procedure PLMO                                      507
            ASSIGN 5080 TO BLOCK_PLMO                                     507
            GO TO  5070                                                   507
5080        CONTINUE                                                      507
C       *                                                                 507
C       Geant POSITION : PLMO ===> X=PIXG_R*COS(ANGLEPOS) Y=PIXG_R*SIN(A  507
C       NGLEPOS) Z=0.0 ALPHAZ=-PIXG_AOFFSET+ANGLE <===                    507
            CALL AGSRESET                                                 507
            AG_X = PIXG_R*COS(ANGLEPOS)                                   507
            AG_Y = PIXG_R*SIN(ANGLEPOS)                                   507
            AG_Z = 0.0                                                    507
            AG_ALPHAN=AG_ALPHAN+1                                         507
            AG_ALPHAK(AG_ALPHAN)=3                                        507
            AG_ALPHAL(AG_ALPHAN)=-PIXG_AOFFSET+ANGLE                      507
            AG_TITLE = 'PLMO'                                             507
            AG_EXNAME = 'POSITION'                                        507
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   507
            CALL AXPOSITION                                               507
C       *                                                                 507
         GO TO 5061                                                       509
5062     CONTINUE                                                         509
5031  CALL AGSPOPB                                                        511
      GO TO BLOCK_PSEC, (5030 ,5040)                                      511
C End of Local Procedure                                                  511
*
* -----------------------------------------------------------------------------
C Begin Local Procedure PLMO                                              514
C PLMO IS THE MOTHER OF THE SILICON LADDER                                514
5070  CONTINUE                                                            514
         AG_TITLE='PLMO'                                                  514
         AG_MODULE=MODULE                                                 514
         AG_MARK='V00'                                                    514
         CALL AGSPUSH                                                     514
         AG_MODULE=MODULE                                                 514
C    *                                                                    514
C    Geant MATERIAL : AIR ===> inherited arguments used <===              515
         CALL AGSRESET                                                    515
         AG_TITLE = 'AIR'                                                 515
         AG_EXNAME = 'MATERIAL'                                           515
         AG_PARLIST = 'NONE'                                              515
         CALL AXMATERIAL                                                  515
C    *                                                                    515
C    Geant ATTRIBUTE : PLMO ===> SEEN=1 COLO=4 <===                       515
         CALL AGSRESET                                                    516
         AG_SEEN = 1                                                      516
         AG_COLO = 4                                                      516
         AG_TITLE = 'PLMO'                                                516
         AG_EXNAME = 'ATTRIBUTE'                                          516
         AG_PARLIST = 'SEEN_COLO'                                         516
         CALL AXATTRIBUTE                                                 516
C    *                                                                    516
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_LADDERTHK/2.  516
C    0 DZ=PIXG_TOTALLENGTH/2.0 <===                                       516
         CALL AGSRESET                                                    517
         AG_DX = PIXG_LADDERWIDTH/2.0                                     517
         AG_DY = PIXG_LADDERTHK/2.0                                       517
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     517
         AG_TITLE = 'BOX'                                                 517
         AG_EXNAME = 'SHAPE'                                              517
         AG_PARLIST = 'DX_DY_DZ'                                          517
         CALL AXSHAPE                                                     517
C    *                                                                    517
C    Create PLAC :                                                        519
C    Call To Local Procedure PLAC                                         519
         ASSIGN 5100 TO BLOCK_PLAC                                        519
         GO TO  5090                                                      519
5100     CONTINUE                                                         519
C    *                                                                    519
C    Geant POSITION : PLAC ===> Y=-PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK/2.0  519
C     <===                                                                519
         CALL AGSRESET                                                    519
         AG_Y = -PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK/2.0                    519
         AG_TITLE = 'PLAC'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'Y'                                                 519
         CALL AXPOSITION                                                  519
C    *                                                                    519
C    Create PLPS :                                                        520
C    Call To Local Procedure PLPS                                         520
         ASSIGN 5120 TO BLOCK_PLPS                                        520
         GO TO  5110                                                      520
5120     CONTINUE                                                         520
C    *                                                                    520
C    Geant POSITION : PLPS ===> Y=-PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK+PIX  520
C    G_PASSIVETHK/2.0 <===                                                520
         CALL AGSRESET                                                    520
         AG_Y = -PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK+PIXG_PASSIVETHK/2.0    520
         AG_TITLE = 'PLPS'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'Y'                                                 520
         CALL AXPOSITION                                                  520
C    *                                                                    520
5071  CALL AGSPOPB                                                        522
      GO TO BLOCK_PLMO, (5070 ,5080)                                      522
C End of Local Procedure                                                  522
*
* -----------------------------------------------------------------------------
C Begin Local Procedure PLAC                                              525
C PLAC IS THE ACTIVE LAYER OF THE LADDER                                  525
5090  CONTINUE                                                            525
         AG_TITLE='PLAC'                                                  525
         AG_MODULE=MODULE                                                 525
         AG_MARK='V00'                                                    525
         CALL AGSPUSH                                                     525
         AG_MODULE=MODULE                                                 525
C    *                                                                    525
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          526
         CALL AGSRESET                                                    526
         AG_TITLE = 'SILICON'                                             526
         AG_EXNAME = 'MATERIAL'                                           526
         AG_PARLIST = 'NONE'                                              526
         CALL AXMATERIAL                                                  526
C    *                                                                    526
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         526
         CALL AGSRESET                                                    527
         AG_ISVOL = 1                                                     527
         AG_TITLE = 'SENSITIVE'                                           527
         AG_EXNAME = 'MATERIAL'                                           527
         AG_PARLIST = 'ISVOL'                                             527
         CALL AXMATERIAL                                                  527
C    *                                                                    527
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=4 <===                       527
         CALL AGSRESET                                                    528
         AG_SEEN = 1                                                      528
         AG_COLO = 4                                                      528
         AG_TITLE = 'PLAC'                                                528
         AG_EXNAME = 'ATTRIBUTE'                                          528
         AG_PARLIST = 'SEEN_COLO'                                         528
         CALL AXATTRIBUTE                                                 528
C    *                                                                    528
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_ACTIVETHK/2.  530
C    0 DZ=PIXG_TOTALLENGTH/2.0 <===                                       530
         CALL AGSRESET                                                    530
         AG_DX = PIXG_LADDERWIDTH/2.0                                     530
         AG_DY = PIXG_ACTIVETHK/2.0                                       530
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     530
         AG_TITLE = 'BOX'                                                 530
         AG_EXNAME = 'SHAPE'                                              530
         AG_PARLIST = 'DX_DY_DZ'                                          530
         CALL AXSHAPE                                                     530
C    *                                                                    530
         CALL GSTPAR (AG_IMED,'STRA',1.)                                  532
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  536
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  536
C    ,0.001)                                                              536
         AG_TITLE = 'PLAC'                                                536
         AG_EXNAME = 'HITS'                                               536
         AG_TYPE=AG_MAGIC                                                 536
         CALL AGSDETV('PLAC')                                             536
C    *                                                                    536
         AG_PARLIST='Z'                                                   536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = AG_MAGIC                                              536
         AG_BIN=.001                                                      536
         AG_OPTION='S'                                                    536
         AG_FHMIN = AG_MAGIC                                              536
         AG_FHMAX = AG_MAGIC                                              536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='Y'                                                   536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = AG_MAGIC                                              536
         AG_BIN=.001                                                      536
         AG_FHMIN=AG_MAGIC                                                536
         AG_FHMAX=AG_MAGIC                                                536
         AG_OPTION='*'                                                    536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='X'                                                   536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = AG_MAGIC                                              536
         AG_BIN=.001                                                      536
         AG_FHMIN=AG_MAGIC                                                536
         AG_FHMAX=AG_MAGIC                                                536
         AG_OPTION='*'                                                    536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='PTOT'                                                536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 16                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_OPTION='*'                                                    536
         AG_FHMIN = 0                                                     536
         AG_FHMAX = 100                                                   536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='CX'                                                  536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 10                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_FHMIN=AG_MAGIC                                                536
         AG_FHMAX=AG_MAGIC                                                536
         AG_OPTION='*'                                                    536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='CY'                                                  536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 10                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_FHMIN=AG_MAGIC                                                536
         AG_FHMAX=AG_MAGIC                                                536
         AG_OPTION='*'                                                    536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='CZ'                                                  536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 10                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_FHMIN=AG_MAGIC                                                536
         AG_FHMAX=AG_MAGIC                                                536
         AG_OPTION='*'                                                    536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='SLENG'                                               536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 16                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_OPTION='*'                                                    536
         AG_FHMIN = 0                                                     536
         AG_FHMAX = 500                                                   536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='TOF'                                                 536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 16                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_OPTION='*'                                                    536
         AG_FHMIN = 0                                                     536
         AG_FHMAX = 1.E-6                                                 536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='STEP'                                                536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = AG_MAGIC                                              536
         AG_BIN=.01                                                       536
         AG_FHMIN=AG_MAGIC                                                536
         AG_FHMAX=AG_MAGIC                                                536
         AG_OPTION='*'                                                    536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         AG_PARLIST='ELOSS'                                               536
         AG_TITLE = 'DETH'                                                536
         AG_NBITS = 16                                                    536
         AG_BIN=AG_MAGIC                                                  536
         AG_OPTION='*'                                                    536
         AG_FHMIN = 0                                                     536
         AG_FHMAX = 0.001                                                 536
         CALL AGSDETH (0)                                                 536
C    *                                                                    536
         CALL AGSDETU('PLAC')                                             536
C    *                                                                    536
5091  CALL AGSPOPB                                                        537
      GO TO BLOCK_PLAC, (5090 ,5100)                                      537
C End of Local Procedure                                                  537
* -----------------------------------------------------------------------------
C Begin Local Procedure PLPS                                              539
C PLPS IS THE PASSIVE LAYER OF THE LADDER                                 539
5110  CONTINUE                                                            539
         AG_TITLE='PLPS'                                                  539
         AG_MODULE=MODULE                                                 539
         AG_MARK='V00'                                                    539
         CALL AGSPUSH                                                     539
         AG_MODULE=MODULE                                                 539
C    *                                                                    539
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          540
         CALL AGSRESET                                                    540
         AG_TITLE = 'SILICON'                                             540
         AG_EXNAME = 'MATERIAL'                                           540
         AG_PARLIST = 'NONE'                                              540
         CALL AXMATERIAL                                                  540
C    *                                                                    540
C    Geant ATTRIBUTE : PLPS ===> SEEN=1 COLO=2 <===                       540
         CALL AGSRESET                                                    541
         AG_SEEN = 1                                                      541
         AG_COLO = 2                                                      541
         AG_TITLE = 'PLPS'                                                541
         AG_EXNAME = 'ATTRIBUTE'                                          541
         AG_PARLIST = 'SEEN_COLO'                                         541
         CALL AXATTRIBUTE                                                 541
C    *                                                                    541
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_PASSIVETHK/2  541
C    .0 DZ=PIXG_TOTALLENGTH/2.0 <===                                      541
         CALL AGSRESET                                                    542
         AG_DX = PIXG_LADDERWIDTH/2.0                                     542
         AG_DY = PIXG_PASSIVETHK/2.0                                      542
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     542
         AG_TITLE = 'BOX'                                                 542
         AG_EXNAME = 'SHAPE'                                              542
         AG_PARLIST = 'DX_DY_DZ'                                          542
         CALL AXSHAPE                                                     542
C    *                                                                    542
5111  CALL AGSPOPB                                                        542
      GO TO BLOCK_PLPS, (5110 ,5120)                                      543
C End of Local Procedure                                                  543
      END                                                                 545
* $Id: pixlgeo2.g,v 1.2 2005/07/14 23:03:54 potekhin Exp $
* $Log: pixlgeo2.g,v $
* Revision 1.2  2005/07/14 23:03:54  potekhin
* Removed obsolete CVS rlog comments, changed the color
* of the Be exoskeleton cylinder
*
* Revision 1.1  2005/07/14 22:59:08  potekhin
* An updated version of the pixel detector geometry,
* which shall inslude the exoskeleton for the beampipe
*
*
******************************************************************************
      SUBROUTINE PIXLGEO2                                                 559
C PIXLGEO2 IS THE THE STAR PIXEL DETECTOR AND BEAM PIPE SUPPORT           559
      IMPLICIT NONE                                                       559
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       559
      DATA MODULE/'PIXLGEO2'/, BANK_TITLE/'*'/                            559
      DATA MCOMMENT/'IS THE THE STAR PIXEL DETECTOR AND BEAM PIPE SUPPOR  559
     *T'/                                                                 559
      LOGICAL FIRST                                                       559
      DATA FIRST/.TRUE./                                                  559
      SAVE FIRST                                                          559
      INTEGER JUMPAD                                                      559
      EXTERNAL JUMPAD                                                     559
      INTEGER OK                                                          559
      PARAMETER (OK=0)                                                    559
      INTEGER AG_I,AG_J                                                   559
      SAVE AG_I,AG_J                                                      559
C CREATED 07/14/05                                                        559
      DATA AG_I/0/,AG_J/0/                                                560
      CHARACTER*40 CREATED /'07/14/05'/                                   560
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  560
C AUTHOR MAXIM POTEKHIN                                                   560
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              561
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  561
******************************************************************************
C - Advanced Geant interface                                              563
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      563
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       563
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         563
     *AG_OPTION,AG_ORT,AG_MARK                                            563
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         563
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     563
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     563
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    563
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          563
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   563
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     563
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        563
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   563
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 563
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     563
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       563
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        563
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       563
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       563
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       563
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         563
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     563
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       563
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   563
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       563
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     563
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          563
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         563
     *-696969,AG_LDETU=250)                                               563
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    563
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  563
     *AG_IMFLD,AG_ISIMU                                                   563
C Inherited variables a: saved during internal calls                      563
C b: reset at the entry in a block and then saved                         563
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        563
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       563
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         563
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     563
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       563
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         563
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           563
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        563
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        563
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            563
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           563
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    563
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      563
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           563
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          563
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                563
C local variables valid inside same block                                 563
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       563
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  563
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         563
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    563
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          563
     *AG_OPTION                                                           563
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        563
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             563
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              563
C temporarly                                                              563
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           563
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    563
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       563
     *AG_HY)                                                              563
      REAL MIL_P,INCH_P                                                   563
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               563
C - GEANT description of logical units                                    563
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            563
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      563
      COMMON/GCMAIL/ CHMAIL                                               563
      CHARACTER*132 CHMAIL                                                563
*
      REAL ANGLE,ANGLEPOS,ANGLECORR,RADDEG                                565
      INTEGER NLADDER,NSECTOR, NEXTRALADDER                               566
      INTEGER BLOCK_PXMO                                                  568
      INTEGER BLOCK_PSEC                                                  568
      INTEGER BLOCK_PLMO                                                  568
      INTEGER BLOCK_PLAC                                                  568
      INTEGER BLOCK_PLPS                                                  568
      INTEGER BLOCK_PXBX                                                  568
*
C -                                                                       572
      REAL PIXG_LADDER                                                    572
      EQUIVALENCE (BANK_PIXG(3),PIXG_LADDER,RE_PIXG_LADDER),              572
     *(NAMES_PIXG(1),NPIXG_LADDER)                                        572
      EQUIVALENCE (COMMEN_PIXG(1),CPIXG_LADDER),(MAP_PIXG(1,1),           572
     *MPIXG_LADDER)                                                       572
      CHARACTER CPIXG_LADDER*40/' '/                                      572
      CHARACTER NPIXG_LADDER*12/'R.LADDER'/                               572
      INTEGER MPIXG_LADDER(2)/1,1/                                        572
      REAL RE_PIXG_LADDER                                                 572
C -                                                                       572
      REAL PIXG_RIN                                                       572
      EQUIVALENCE (BANK_PIXG(4),PIXG_RIN,RE_PIXG_RIN),(NAMES_PIXG(2),     572
     *NPIXG_RIN)                                                          572
      EQUIVALENCE (COMMEN_PIXG(2),CPIXG_RIN),(MAP_PIXG(1,2),MPIXG_RIN)    572
      CHARACTER CPIXG_RIN*40/' '/                                         572
      CHARACTER NPIXG_RIN*12/'R.RIN'/                                     572
      INTEGER MPIXG_RIN(2)/1,1/                                           572
      REAL RE_PIXG_RIN                                                    572
C -                                                                       572
      REAL PIXG_ROUT                                                      572
      EQUIVALENCE (BANK_PIXG(5),PIXG_ROUT,RE_PIXG_ROUT),(NAMES_PIXG(3),   572
     *NPIXG_ROUT)                                                         572
      EQUIVALENCE (COMMEN_PIXG(3),CPIXG_ROUT),(MAP_PIXG(1,3),             572
     *MPIXG_ROUT)                                                         572
      CHARACTER CPIXG_ROUT*40/' '/                                        572
      CHARACTER NPIXG_ROUT*12/'R.ROUT'/                                   572
      INTEGER MPIXG_ROUT(2)/1,1/                                          572
      REAL RE_PIXG_ROUT                                                   572
C -                                                                       572
      REAL PIXG_TOTALLENGTH                                               572
      EQUIVALENCE (BANK_PIXG(6),PIXG_TOTALLENGTH,RE_PIXG_TOTALLENGTH),    572
     *(NAMES_PIXG(4),NPIXG_TOTALLENGTH)                                   572
      EQUIVALENCE (COMMEN_PIXG(4),CPIXG_TOTALLENGTH),(MAP_PIXG(1,4),      572
     *MPIXG_TOTALLENGTH)                                                  572
      CHARACTER CPIXG_TOTALLENGTH*40/' '/                                 572
      CHARACTER NPIXG_TOTALLENGTH*12/'R.TOTALLENGTH'/                     572
      INTEGER MPIXG_TOTALLENGTH(2)/1,1/                                   572
      REAL RE_PIXG_TOTALLENGTH                                            572
C -                                                                       572
      REAL PIXG_LADDERWIDTH                                               572
      EQUIVALENCE (BANK_PIXG(7),PIXG_LADDERWIDTH,RE_PIXG_LADDERWIDTH),    572
     *(NAMES_PIXG(5),NPIXG_LADDERWIDTH)                                   572
      EQUIVALENCE (COMMEN_PIXG(5),CPIXG_LADDERWIDTH),(MAP_PIXG(1,5),      572
     *MPIXG_LADDERWIDTH)                                                  572
      CHARACTER CPIXG_LADDERWIDTH*40/' '/                                 572
      CHARACTER NPIXG_LADDERWIDTH*12/'R.LADDERWIDTH'/                     572
      INTEGER MPIXG_LADDERWIDTH(2)/1,1/                                   572
      REAL RE_PIXG_LADDERWIDTH                                            572
C -                                                                       572
      REAL PIXG_LADDERTHK                                                 572
      EQUIVALENCE (BANK_PIXG(8),PIXG_LADDERTHK,RE_PIXG_LADDERTHK),        572
     *(NAMES_PIXG(6),NPIXG_LADDERTHK)                                     572
      EQUIVALENCE (COMMEN_PIXG(6),CPIXG_LADDERTHK),(MAP_PIXG(1,6),        572
     *MPIXG_LADDERTHK)                                                    572
      CHARACTER CPIXG_LADDERTHK*40/' '/                                   572
      CHARACTER NPIXG_LADDERTHK*12/'R.LADDERTHK'/                         572
      INTEGER MPIXG_LADDERTHK(2)/1,1/                                     572
      REAL RE_PIXG_LADDERTHK                                              572
C -                                                                       572
      REAL PIXG_PASSIVETHK                                                572
      EQUIVALENCE (BANK_PIXG(9),PIXG_PASSIVETHK,RE_PIXG_PASSIVETHK),      572
     *(NAMES_PIXG(7),NPIXG_PASSIVETHK)                                    572
      EQUIVALENCE (COMMEN_PIXG(7),CPIXG_PASSIVETHK),(MAP_PIXG(1,7),       572
     *MPIXG_PASSIVETHK)                                                   572
      CHARACTER CPIXG_PASSIVETHK*40/' '/                                  572
      CHARACTER NPIXG_PASSIVETHK*12/'R.PASSIVETHK'/                       572
      INTEGER MPIXG_PASSIVETHK(2)/1,1/                                    572
      REAL RE_PIXG_PASSIVETHK                                             572
C -                                                                       572
      REAL PIXG_ACTIVETHK                                                 572
      EQUIVALENCE (BANK_PIXG(10),PIXG_ACTIVETHK,RE_PIXG_ACTIVETHK),       572
     *(NAMES_PIXG(8),NPIXG_ACTIVETHK)                                     572
      EQUIVALENCE (COMMEN_PIXG(8),CPIXG_ACTIVETHK),(MAP_PIXG(1,8),        572
     *MPIXG_ACTIVETHK)                                                    572
      CHARACTER CPIXG_ACTIVETHK*40/' '/                                   572
      CHARACTER NPIXG_ACTIVETHK*12/'R.ACTIVETHK'/                         572
      INTEGER MPIXG_ACTIVETHK(2)/1,1/                                     572
      REAL RE_PIXG_ACTIVETHK                                              572
C -                                                                       572
      REAL PIXG_R                                                         572
      EQUIVALENCE (BANK_PIXG(11),PIXG_R,RE_PIXG_R),(NAMES_PIXG(9),        572
     *NPIXG_R)                                                            572
      EQUIVALENCE (COMMEN_PIXG(9),CPIXG_R),(MAP_PIXG(1,9),MPIXG_R)        572
      CHARACTER CPIXG_R*40/' '/                                           572
      CHARACTER NPIXG_R*12/'R.R'/                                         572
      INTEGER MPIXG_R(2)/1,1/                                             572
      REAL RE_PIXG_R                                                      572
C -                                                                       572
      REAL PIXG_A                                                         572
      EQUIVALENCE (BANK_PIXG(12),PIXG_A,RE_PIXG_A),(NAMES_PIXG(10),       572
     *NPIXG_A)                                                            572
      EQUIVALENCE (COMMEN_PIXG(10),CPIXG_A),(MAP_PIXG(1,10),MPIXG_A)      572
      CHARACTER CPIXG_A*40/' '/                                           572
      CHARACTER NPIXG_A*12/'R.A'/                                         572
      INTEGER MPIXG_A(2)/1,1/                                             572
      REAL RE_PIXG_A                                                      572
C -                                                                       572
      REAL PIXG_POFFSET                                                   572
      EQUIVALENCE (BANK_PIXG(13),PIXG_POFFSET,RE_PIXG_POFFSET),           572
     *(NAMES_PIXG(11),NPIXG_POFFSET)                                      572
      EQUIVALENCE (COMMEN_PIXG(11),CPIXG_POFFSET),(MAP_PIXG(1,11),        572
     *MPIXG_POFFSET)                                                      572
      CHARACTER CPIXG_POFFSET*40/' '/                                     572
      CHARACTER NPIXG_POFFSET*12/'R.POFFSET'/                             572
      INTEGER MPIXG_POFFSET(2)/1,1/                                       572
      REAL RE_PIXG_POFFSET                                                572
C -                                                                       572
      REAL PIXG_AOFFSET                                                   572
      EQUIVALENCE (BANK_PIXG(14),PIXG_AOFFSET,RE_PIXG_AOFFSET),           572
     *(NAMES_PIXG(12),NPIXG_AOFFSET)                                      572
      EQUIVALENCE (COMMEN_PIXG(12),CPIXG_AOFFSET),(MAP_PIXG(1,12),        572
     *MPIXG_AOFFSET)                                                      572
      CHARACTER CPIXG_AOFFSET*40/' '/                                     572
      CHARACTER NPIXG_AOFFSET*12/'R.AOFFSET'/                             572
      INTEGER MPIXG_AOFFSET(2)/1,1/                                       572
      REAL RE_PIXG_AOFFSET                                                572
C -                                                                       572
      INTEGER LEN_PIXG,LLEN_PIXG,LINK_PIXG                                572
      PARAMETER (LEN_PIXG=14)                                             572
      PARAMETER (LLEN_PIXG=12)                                            572
      INTEGER FLAG_PIXG/0/,MAP_PIXG(2,LLEN_PIXG),IDNUM_PIXG(10)           572
      CHARACTER NAMES_PIXG(LLEN_PIXG)*12,COMMEN_PIXG(LLEN_PIXG)*40        572
      DATA IDNUM_PIXG/10*0/                                               572
      REAL BANK_PIXG(LEN_PIXG)                                            572
      CHARACTER*60 BPATH_PIXG/' '/                                        572
      SAVE FLAG_PIXG,MAP_PIXG,IDNUM_PIXG,NAMES_PIXG,COMMEN_PIXG,          572
     *BPATH_PIXG                                                          572
      DATA LINK_PIXG/0/,BANK_PIXG/LEN_PIXG*0/                             572
C *                                                                       572
C -                                                                       573
      REAL PXBG_VERSION                                                   573
      EQUIVALENCE (BANK_PXBG(3),PXBG_VERSION,RE_PXBG_VERSION),            573
     *(NAMES_PXBG(1),NPXBG_VERSION)                                       573
      EQUIVALENCE (COMMEN_PXBG(1),CPXBG_VERSION),(MAP_PXBG(1,1),          573
     *MPXBG_VERSION)                                                      573
      CHARACTER CPXBG_VERSION*40/' '/                                     573
      CHARACTER NPXBG_VERSION*12/'R.VERSION'/                             573
      INTEGER MPXBG_VERSION(2)/1,1/                                       573
      REAL RE_PXBG_VERSION                                                573
C -                                                                       573
      REAL PXBG_LENGTH                                                    573
      EQUIVALENCE (BANK_PXBG(4),PXBG_LENGTH,RE_PXBG_LENGTH),              573
     *(NAMES_PXBG(2),NPXBG_LENGTH)                                        573
      EQUIVALENCE (COMMEN_PXBG(2),CPXBG_LENGTH),(MAP_PXBG(1,2),           573
     *MPXBG_LENGTH)                                                       573
      CHARACTER CPXBG_LENGTH*40/' '/                                      573
      CHARACTER NPXBG_LENGTH*12/'R.LENGTH'/                               573
      INTEGER MPXBG_LENGTH(2)/1,1/                                        573
      REAL RE_PXBG_LENGTH                                                 573
C -                                                                       573
      REAL PXBG_RIN                                                       573
      EQUIVALENCE (BANK_PXBG(5),PXBG_RIN,RE_PXBG_RIN),(NAMES_PXBG(3),     573
     *NPXBG_RIN)                                                          573
      EQUIVALENCE (COMMEN_PXBG(3),CPXBG_RIN),(MAP_PXBG(1,3),MPXBG_RIN)    573
      CHARACTER CPXBG_RIN*40/' '/                                         573
      CHARACTER NPXBG_RIN*12/'R.RIN'/                                     573
      INTEGER MPXBG_RIN(2)/1,1/                                           573
      REAL RE_PXBG_RIN                                                    573
C -                                                                       573
      REAL PXBG_THK                                                       573
      EQUIVALENCE (BANK_PXBG(6),PXBG_THK,RE_PXBG_THK),(NAMES_PXBG(4),     573
     *NPXBG_THK)                                                          573
      EQUIVALENCE (COMMEN_PXBG(4),CPXBG_THK),(MAP_PXBG(1,4),MPXBG_THK)    573
      CHARACTER CPXBG_THK*40/' '/                                         573
      CHARACTER NPXBG_THK*12/'R.THK'/                                     573
      INTEGER MPXBG_THK(2)/1,1/                                           573
      REAL RE_PXBG_THK                                                    573
C -                                                                       573
      INTEGER LEN_PXBG,LLEN_PXBG,LINK_PXBG                                573
      PARAMETER (LEN_PXBG=6)                                              573
      PARAMETER (LLEN_PXBG=4)                                             573
      INTEGER FLAG_PXBG/0/,MAP_PXBG(2,LLEN_PXBG),IDNUM_PXBG(10)           573
      CHARACTER NAMES_PXBG(LLEN_PXBG)*12,COMMEN_PXBG(LLEN_PXBG)*40        573
      DATA IDNUM_PXBG/10*0/                                               573
      REAL BANK_PXBG(LEN_PXBG)                                            573
      CHARACTER*60 BPATH_PXBG/' '/                                        573
      SAVE FLAG_PXBG,MAP_PXBG,IDNUM_PXBG,NAMES_PXBG,COMMEN_PXBG,          573
     *BPATH_PXBG                                                          573
      DATA LINK_PXBG/0/,BANK_PXBG/LEN_PXBG*0/                             573
C *                                                                       573
*
* -----------------------------------------------------------------------------
*
C *                                                                       577
      CALL AGSBEGM(MODULE,IPRIN)                                          577
      IF (IPRIN.LT.0) RETURN                                              577
C Filling bank PIXG - PIXEL DETECTOR DATA                                 577
      BANK_TITLE='PIXEL DETECTOR DATA'                                    577
      BPATH_PIXG='PIXG*'                                                  577
      IDNUM_PIXG(1)=0                                                     578
C LADDER INDEX                                                            578
      PIXG_LADDER=1                                                       578
      CPIXG_LADDER='LADDER INDEX'                                         578
C INNER RADIUS                                                            579
      PIXG_RIN=1.45                                                       579
      CPIXG_RIN='INNER RADIUS'                                            579
C OUTER RADIUS                                                            580
      PIXG_ROUT=5.65                                                      580
      CPIXG_ROUT='OUTER RADIUS'                                           580
C OVERAL LENGTH OF THE DETECTOR                                           581
      PIXG_TOTALLENGTH=16.0                                               581
      CPIXG_TOTALLENGTH='OVERAL LENGTH OF THE DETECTOR'                   581
*
C LADDER WIDTH                                                            583
      PIXG_LADDERWIDTH=2.00                                               583
      CPIXG_LADDERWIDTH='LADDER WIDTH'                                    583
C TOTAL LADDER THICKNESS                                                  584
      PIXG_LADDERTHK=0.0120                                               584
      CPIXG_LADDERTHK='TOTAL LADDER THICKNESS'                            584
C PASSIVE SILICON THICKNESS                                               585
      PIXG_PASSIVETHK=0.0100                                              585
      CPIXG_PASSIVETHK='PASSIVE SILICON THICKNESS'                        585
C ACTIVE SILICON THICKNESS                                                586
      PIXG_ACTIVETHK=0.0020                                               586
      CPIXG_ACTIVETHK='ACTIVE SILICON THICKNESS'                          586
*
C 1ST LADDER NOMINAL RADIUS                                               588
      PIXG_R=5.294                                                        588
      CPIXG_R='1ST LADDER NOMINAL RADIUS'                                 588
C 1ST LADDER NOMINAL POSITION ANGLE                                       589
      PIXG_A=0.0                                                          589
      CPIXG_A='1ST LADDER NOMINAL POSITION ANGLE'                         589
C ANGULAR OFFSET                                                          590
      PIXG_AOFFSET=89.28                                                  590
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      590
*
C POSITION OFFSET (SHIFT)                                                 592
      PIXG_POFFSET=0.0                                                    592
      CPIXG_POFFSET='POSITION OFFSET (SHIFT)'                             592
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    593
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           593
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   593
C *                                                                       593
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    593
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           593
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   593
C *                                                                       595
      CALL AGSBEGM(MODULE,IPRIN)                                          595
      IF (IPRIN.LT.0) RETURN                                              595
C Filling bank PIXG - PIXEL DETECTOR DATA                                 595
      BANK_TITLE='PIXEL DETECTOR DATA'                                    595
      BPATH_PIXG='PIXG*'                                                  595
      IDNUM_PIXG(1)=0                                                     596
C LADDER INDEX                                                            596
      PIXG_LADDER=2                                                       596
      CPIXG_LADDER='LADDER INDEX'                                         596
C 2ND LADDER NOMINAL RADIUS                                               597
      PIXG_R=4.862                                                        597
      CPIXG_R='2ND LADDER NOMINAL RADIUS'                                 597
C 2ND LADDER NOMINAL POSITION ANGLE                                       598
      PIXG_A=20.27                                                        598
      CPIXG_A='2ND LADDER NOMINAL POSITION ANGLE'                         598
C ANGULAR OFFSET                                                          599
      PIXG_AOFFSET=88.31                                                  599
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      599
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    600
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           600
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   600
C *                                                                       600
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    600
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           600
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   600
C *                                                                       602
      CALL AGSBEGM(MODULE,IPRIN)                                          602
      IF (IPRIN.LT.0) RETURN                                              602
C Filling bank PIXG - PIXEL DETECTOR DATA                                 602
      BANK_TITLE='PIXEL DETECTOR DATA'                                    602
      BPATH_PIXG='PIXG*'                                                  602
      IDNUM_PIXG(1)=0                                                     603
C LADDER INDEX                                                            603
      PIXG_LADDER=3                                                       603
      CPIXG_LADDER='LADDER INDEX'                                         603
C 3RD LADDER RADIUS                                                       604
      PIXG_R=4.391                                                        604
      CPIXG_R='3RD LADDER RADIUS'                                         604
C 3RD LADDER NOMINAL POSITION ANGLE                                       605
      PIXG_A=42.62                                                        605
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         605
C ANGULAR OFFSET                                                          606
      PIXG_AOFFSET=87.01                                                  606
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      606
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    607
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           607
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   607
C *                                                                       607
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    607
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           607
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   607
C *                                                                       609
      CALL AGSBEGM(MODULE,IPRIN)                                          609
      IF (IPRIN.LT.0) RETURN                                              609
C Filling bank PIXG - PIXEL DETECTOR DATA                                 609
      BANK_TITLE='PIXEL DETECTOR DATA'                                    609
      BPATH_PIXG='PIXG*'                                                  609
      IDNUM_PIXG(1)=0                                                     610
C LADDER INDEX                                                            610
      PIXG_LADDER=4                                                       610
      CPIXG_LADDER='LADDER INDEX'                                         610
C 4TH LADDER NOMINAL RADIUS                                               611
      PIXG_R=1.595                                                        611
      CPIXG_R='4TH LADDER NOMINAL RADIUS'                                 611
C 4TH LADDER NOMINAL POSITION ANGLE                                       612
      PIXG_A=79.51                                                        612
      CPIXG_A='4TH LADDER NOMINAL POSITION ANGLE'                         612
C ANGULAR OFFSET                                                          613
      PIXG_AOFFSET=70.15                                                  613
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      613
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    614
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           614
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   614
C *                                                                       614
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    614
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           614
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   614
C *                                                                       616
      CALL AGSBEGM(MODULE,IPRIN)                                          616
      IF (IPRIN.LT.0) RETURN                                              616
C Filling bank PXBG - BEAM PIPE EXOSKELETON DATA                          616
      BANK_TITLE='BEAM PIPE EXOSKELETON DATA'                             616
      BPATH_PXBG='PXBG*'                                                  616
      IDNUM_PXBG(1)=0                                                     617
C VERSION                                                                 617
      PXBG_VERSION=1                                                      617
      CPXBG_VERSION='VERSION'                                             617
C TOTAL LENGTH                                                            618
      PXBG_LENGTH=48.0                                                    618
      CPXBG_LENGTH='TOTAL LENGTH'                                         618
C INNER RADIUS                                                            619
      PXBG_RIN=5.9                                                        619
      CPXBG_RIN='INNER RADIUS'                                            619
C THICKNESS                                                               620
      PXBG_THK=0.1                                                        620
      CPXBG_THK='THICKNESS'                                               620
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXBG',    621
     *BPATH_PXBG,IDNUM_PXBG,1, MAP_PXBG,NAMES_PXBG,COMMEN_PXBG,           621
     *BANK_PXBG,LEN_PXBG,LLEN_PXBG,LINK_PXBG,FLAG_PXBG)                   621
C *                                                                       621
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXBG',    621
     *BPATH_PXBG,IDNUM_PXBG,1, MAP_PXBG,NAMES_PXBG,COMMEN_PXBG,           621
     *BANK_PXBG,LEN_PXBG,LLEN_PXBG,LINK_PXBG,FLAG_PXBG)                   621
******************************************************
C use default PIXG bank                                                   624
      BPATH_PIXG='PIXG*'                                                  624
      IDNUM_PIXG(1)=0                                                     624
      CALL AGDGETP('PIXLGEO2','*',-999,'PIXG',' ',BANK_PIXG, BPATH_PIXG,  624
     *IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,BANK_PIXG,LEN_PIXG,LLEN_PIXG,      624
     *LINK_PIXG,FLAG_PIXG)                                                624
*
      RADDEG=3.14159265/180.0                                             626
      WRITE(*,*) '=======  Constructing the Pixel Detector with Beam Pip  628
     *e Support ========'                                                 628
      CALL AGSBEGM(MODULE,IPRIN)                                          629
C Check IPRIN<0                                                           629
      IF (IPRIN.LT.0) RETURN                                              629
C Create PXMO :                                                           629
C Call To Local Procedure PXMO                                            629
      ASSIGN 5020 TO BLOCK_PXMO                                           629
      GO TO  5010                                                         629
5020  CONTINUE                                                            629
C *                                                                       629
C Geant POSITION : PXMO ===> MOTHER='CAVE' KONLY='MANY' <===              630
      CALL AGSRESET                                                       630
      AG_MOTHER = 'CAVE'                                                  630
      AG_KONLY = 'MANY'                                                   630
      AG_TITLE = 'PXMO'                                                   630
      AG_EXNAME = 'POSITION'                                              630
      AG_PARLIST = 'MOTHER_KONLY'                                         630
      CALL AXPOSITION                                                     630
C *                                                                       630
      CALL AGSBEGM(MODULE,IPRIN)                                          632
C Check IPRIN<0                                                           632
      IF (IPRIN.LT.0) RETURN                                              632
C Create PXBX :                                                           632
C Call To Local Procedure PXBX                                            632
      ASSIGN 5040 TO BLOCK_PXBX                                           632
      GO TO  5030                                                         632
5040  CONTINUE                                                            632
C *                                                                       632
C Geant POSITION : PXBX ===> MOTHER='CAVE' <===                           633
      CALL AGSRESET                                                       633
      AG_MOTHER = 'CAVE'                                                  633
      AG_TITLE = 'PXBX'                                                   633
      AG_EXNAME = 'POSITION'                                              633
      AG_PARLIST = 'MOTHER'                                               633
      CALL AXPOSITION                                                     633
C *                                                                       633
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        635
      RETURN                                                              635
C Begin Local Procedure PXMO                                              635
C PXMO IS THE MOTHER OF THE PIXEL DETECTOR VOLUMES                        635
5010  CONTINUE                                                            635
         AG_TITLE='PXMO'                                                  635
         AG_MODULE=MODULE                                                 635
         AG_MARK='V00'                                                    635
         CALL AGSPUSH                                                     635
         AG_MODULE=MODULE                                                 635
C    *                                                                    635
C    Geant MATERIAL : AIR ===> inherited arguments used <===              636
         CALL AGSRESET                                                    636
         AG_TITLE = 'AIR'                                                 636
         AG_EXNAME = 'MATERIAL'                                           636
         AG_PARLIST = 'NONE'                                              636
         CALL AXMATERIAL                                                  636
C    *                                                                    636
C    Geant ATTRIBUTE : PXMO ===> SEEN=1 COLO=6 <===                       636
         CALL AGSRESET                                                    637
         AG_SEEN = 1                                                      637
         AG_COLO = 6                                                      637
         AG_TITLE = 'PXMO'                                                637
         AG_EXNAME = 'ATTRIBUTE'                                          637
         AG_PARLIST = 'SEEN_COLO'                                         637
         CALL AXATTRIBUTE                                                 637
C    *                                                                    637
C    Geant SHAPE : TUBE ===> RMIN=PIXG_RIN RMAX=PIXG_ROUT DZ=PIXG_TOTALL  639
C    ENGTH/2.0 <===                                                       639
         CALL AGSRESET                                                    639
         AG_RMIN = PIXG_RIN                                               639
         AG_RMAX = PIXG_ROUT                                              639
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     639
         AG_TITLE = 'TUBE'                                                639
         AG_EXNAME = 'SHAPE'                                              639
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      639
         CALL AXSHAPE                                                     639
C    *                                                                    639
* The "sector" is defined as a group of 4 ladders, we
* have a total of 6 overlapping sectors placed with rotational symmetry
C    Create PSEC :                                                        644
C    Call To Local Procedure PSEC                                         644
         ASSIGN 5060 TO BLOCK_PSEC                                        644
         GO TO  5050                                                      644
5060     CONTINUE                                                         644
C    *                                                                    644
C    *                                                                    645
         NSECTOR=1                                                        645
            GO TO 5073                                                    645
5071        NSECTOR=NSECTOR+1                                             645
5073        IF(NSECTOR-(6).GT.0) GO TO 5072                               645
C       NEED THE 04MANY0 OPTION AS THEY DO OVERLAP                        645
C       Geant POSITION : PSEC ===> ALPHAZ=60.0*(NSECTOR-1) KONLY='MANY'   646
C       <===                                                              646
            CALL AGSRESET                                                 646
            AG_ALPHAN=AG_ALPHAN+1                                         646
            AG_ALPHAK(AG_ALPHAN)=3                                        646
            AG_ALPHAL(AG_ALPHAN)=60.0*(NSECTOR-1)                         646
            AG_KONLY = 'MANY'                                             646
            AG_TITLE = 'PSEC'                                             646
            AG_EXNAME = 'POSITION'                                        646
            AG_PARLIST = 'ALPHAZ_KONLY'                                   646
            CALL AXPOSITION                                               646
C       *                                                                 646
         GO TO 5071                                                       647
5072     CONTINUE                                                         647
5011  CALL AGSPOPB                                                        649
      GO TO BLOCK_PXMO, (5010 ,5020)                                      649
C End of Local Procedure                                                  649
* -----------------------------------------------------------------------------
C Begin Local Procedure PXBX                                              651
C PXBX IS THE EXOSKELETON OF THE BEAMPIPE                                 651
5030  CONTINUE                                                            651
         AG_TITLE='PXBX'                                                  651
         AG_MODULE=MODULE                                                 651
         AG_MARK='V00'                                                    651
         CALL AGSPUSH                                                     651
         AG_MODULE=MODULE                                                 651
C    *                                                                    651
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        652
         CALL AGSRESET                                                    652
         AG_TITLE = 'BERILLIUM'                                           652
         AG_EXNAME = 'MATERIAL'                                           652
         AG_PARLIST = 'NONE'                                              652
         CALL AXMATERIAL                                                  652
C    *                                                                    652
C    Geant ATTRIBUTE : PXBX ===> SEEN=1 COLO=1 <===                       652
         CALL AGSRESET                                                    653
         AG_SEEN = 1                                                      653
         AG_COLO = 1                                                      653
         AG_TITLE = 'PXBX'                                                653
         AG_EXNAME = 'ATTRIBUTE'                                          653
         AG_PARLIST = 'SEEN_COLO'                                         653
         CALL AXATTRIBUTE                                                 653
C    *                                                                    653
C    Geant SHAPE : TUBE ===> RMIN=PXBG_RIN RMAX=PXBG_RIN+PXBG_THK DZ=PXB  655
C    G_LENGTH/2.0 <===                                                    655
         CALL AGSRESET                                                    655
         AG_RMIN = PXBG_RIN                                               655
         AG_RMAX = PXBG_RIN+PXBG_THK                                      655
         AG_DZ = PXBG_LENGTH/2.0                                          655
         AG_TITLE = 'TUBE'                                                655
         AG_EXNAME = 'SHAPE'                                              655
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      655
         CALL AXSHAPE                                                     655
C    *                                                                    655
5031  CALL AGSPOPB                                                        655
      GO TO BLOCK_PXBX, (5030 ,5040)                                      656
C End of Local Procedure                                                  656
* -----------------------------------------------------------------------------
C Begin Local Procedure PSEC                                              658
C PSEC IS A GROUP OF LADDERS                                              658
5050  CONTINUE                                                            658
         AG_TITLE='PSEC'                                                  658
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
C    Geant ATTRIBUTE : PSEC ===> SEEN=1 COLO=5 <===                       659
         CALL AGSRESET                                                    660
         AG_SEEN = 1                                                      660
         AG_COLO = 5                                                      660
         AG_TITLE = 'PSEC'                                                660
         AG_EXNAME = 'ATTRIBUTE'                                          660
         AG_PARLIST = 'SEEN_COLO'                                         660
         CALL AXATTRIBUTE                                                 660
C    *                                                                    660
C    Geant SHAPE : TUBS ===> RMIN=PIXG_RIN RMAX=PIXG_ROUT DZ=PIXG_TOTALL  660
C    ENGTH/2.0 PHI1=-11.0 PHI2=118.0 <===                                 660
         CALL AGSRESET                                                    661
         AG_RMIN = PIXG_RIN                                               661
         AG_RMAX = PIXG_ROUT                                              661
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     661
         AG_PHI1 = -11.0                                                  661
         AG_PHI2 = 118.0                                                  661
         AG_TITLE = 'TUBS'                                                661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'RMIN_RMAX_DZ_PHI1_PHI2'                            661
         CALL AXSHAPE                                                     661
C    *                                                                    661
C    *                                                                    664
         NLADDER=1                                                        664
            GO TO 5083                                                    664
5081        NLADDER=NLADDER+1                                             664
5083        IF(NLADDER-(4).GT.0) GO TO 5082                               664
C       INNER LOOP, CREATE LADDERS INSIDE THE SECTOR                      664
C       use PIXG bank                                                     666
            BPATH_PIXG='PIXG*'                                            666
            IDNUM_PIXG(1)=0                                               666
            PIXG_LADDER=NLADDER                                           666
C       INDEX THE LADDER DATA STRUCTURES                                  666
            CALL AGDGETP('PIXLGEO2','*',-999,'PIXG',NPIXG_LADDER,         666
     *      RE_PIXG_LADDER,BPATH_PIXG,IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,   666
     *      BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)             666
            ANGLE = PIXG_A                                                667
* Individual ladders can be individually tilted by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement. (Optional and maybe obsoleted soon: angleCorr= atan(PIXG_pOffset/PIXG_r))
* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)
* have to correct and convert to radians:
C       +ANGLECORR SEE ABOVE COMMENT                                      678
            ANGLEPOS = ANGLE*RADDEG                                       678
* In case we do go with the pOffset, don't forget to correct the radius,
* to keep the surfaces at the nominal DCA to the beam
C       Create PLMO :                                                     684
C       Call To Local Procedure PLMO                                      684
            ASSIGN 5100 TO BLOCK_PLMO                                     684
            GO TO  5090                                                   684
5100        CONTINUE                                                      684
C       *                                                                 684
C       Geant POSITION : PLMO ===> X=PIXG_R*COS(ANGLEPOS) Y=PIXG_R*SIN(A  684
C       NGLEPOS) Z=0.0 ALPHAZ=-PIXG_AOFFSET+ANGLE <===                    684
            CALL AGSRESET                                                 684
            AG_X = PIXG_R*COS(ANGLEPOS)                                   684
            AG_Y = PIXG_R*SIN(ANGLEPOS)                                   684
            AG_Z = 0.0                                                    684
            AG_ALPHAN=AG_ALPHAN+1                                         684
            AG_ALPHAK(AG_ALPHAN)=3                                        684
            AG_ALPHAL(AG_ALPHAN)=-PIXG_AOFFSET+ANGLE                      684
            AG_TITLE = 'PLMO'                                             684
            AG_EXNAME = 'POSITION'                                        684
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   684
            CALL AXPOSITION                                               684
C       *                                                                 684
         GO TO 5081                                                       686
5082     CONTINUE                                                         686
5051  CALL AGSPOPB                                                        688
      GO TO BLOCK_PSEC, (5050 ,5060)                                      688
C End of Local Procedure                                                  688
*
* -----------------------------------------------------------------------------
C Begin Local Procedure PLMO                                              691
C PLMO IS THE MOTHER OF THE SILICON LADDER                                691
5090  CONTINUE                                                            691
         AG_TITLE='PLMO'                                                  691
         AG_MODULE=MODULE                                                 691
         AG_MARK='V00'                                                    691
         CALL AGSPUSH                                                     691
         AG_MODULE=MODULE                                                 691
C    *                                                                    691
C    Geant MATERIAL : AIR ===> inherited arguments used <===              692
         CALL AGSRESET                                                    692
         AG_TITLE = 'AIR'                                                 692
         AG_EXNAME = 'MATERIAL'                                           692
         AG_PARLIST = 'NONE'                                              692
         CALL AXMATERIAL                                                  692
C    *                                                                    692
C    Geant ATTRIBUTE : PLMO ===> SEEN=1 COLO=4 <===                       692
         CALL AGSRESET                                                    693
         AG_SEEN = 1                                                      693
         AG_COLO = 4                                                      693
         AG_TITLE = 'PLMO'                                                693
         AG_EXNAME = 'ATTRIBUTE'                                          693
         AG_PARLIST = 'SEEN_COLO'                                         693
         CALL AXATTRIBUTE                                                 693
C    *                                                                    693
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_LADDERTHK/2.  693
C    0 DZ=PIXG_TOTALLENGTH/2.0 <===                                       693
         CALL AGSRESET                                                    694
         AG_DX = PIXG_LADDERWIDTH/2.0                                     694
         AG_DY = PIXG_LADDERTHK/2.0                                       694
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     694
         AG_TITLE = 'BOX'                                                 694
         AG_EXNAME = 'SHAPE'                                              694
         AG_PARLIST = 'DX_DY_DZ'                                          694
         CALL AXSHAPE                                                     694
C    *                                                                    694
C    Create PLAC :                                                        696
C    Call To Local Procedure PLAC                                         696
         ASSIGN 5120 TO BLOCK_PLAC                                        696
         GO TO  5110                                                      696
5120     CONTINUE                                                         696
C    *                                                                    696
C    Geant POSITION : PLAC ===> Y=-PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK/2.0  696
C     <===                                                                696
         CALL AGSRESET                                                    696
         AG_Y = -PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK/2.0                    696
         AG_TITLE = 'PLAC'                                                696
         AG_EXNAME = 'POSITION'                                           696
         AG_PARLIST = 'Y'                                                 696
         CALL AXPOSITION                                                  696
C    *                                                                    696
C    Create PLPS :                                                        697
C    Call To Local Procedure PLPS                                         697
         ASSIGN 5140 TO BLOCK_PLPS                                        697
         GO TO  5130                                                      697
5140     CONTINUE                                                         697
C    *                                                                    697
C    Geant POSITION : PLPS ===> Y=-PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK+PIX  697
C    G_PASSIVETHK/2.0 <===                                                697
         CALL AGSRESET                                                    697
         AG_Y = -PIXG_LADDERTHK/2.0+PIXG_ACTIVETHK+PIXG_PASSIVETHK/2.0    697
         AG_TITLE = 'PLPS'                                                697
         AG_EXNAME = 'POSITION'                                           697
         AG_PARLIST = 'Y'                                                 697
         CALL AXPOSITION                                                  697
C    *                                                                    697
5091  CALL AGSPOPB                                                        699
      GO TO BLOCK_PLMO, (5090 ,5100)                                      699
C End of Local Procedure                                                  699
*
* -----------------------------------------------------------------------------
C Begin Local Procedure PLAC                                              702
C PLAC IS THE ACTIVE LAYER OF THE LADDER                                  702
5110  CONTINUE                                                            702
         AG_TITLE='PLAC'                                                  702
         AG_MODULE=MODULE                                                 702
         AG_MARK='V00'                                                    702
         CALL AGSPUSH                                                     702
         AG_MODULE=MODULE                                                 702
C    *                                                                    702
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          703
         CALL AGSRESET                                                    703
         AG_TITLE = 'SILICON'                                             703
         AG_EXNAME = 'MATERIAL'                                           703
         AG_PARLIST = 'NONE'                                              703
         CALL AXMATERIAL                                                  703
C    *                                                                    703
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         703
         CALL AGSRESET                                                    704
         AG_ISVOL = 1                                                     704
         AG_TITLE = 'SENSITIVE'                                           704
         AG_EXNAME = 'MATERIAL'                                           704
         AG_PARLIST = 'ISVOL'                                             704
         CALL AXMATERIAL                                                  704
C    *                                                                    704
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=4 <===                       704
         CALL AGSRESET                                                    705
         AG_SEEN = 1                                                      705
         AG_COLO = 4                                                      705
         AG_TITLE = 'PLAC'                                                705
         AG_EXNAME = 'ATTRIBUTE'                                          705
         AG_PARLIST = 'SEEN_COLO'                                         705
         CALL AXATTRIBUTE                                                 705
C    *                                                                    705
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_ACTIVETHK/2.  707
C    0 DZ=PIXG_TOTALLENGTH/2.0 <===                                       707
         CALL AGSRESET                                                    707
         AG_DX = PIXG_LADDERWIDTH/2.0                                     707
         AG_DY = PIXG_ACTIVETHK/2.0                                       707
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     707
         AG_TITLE = 'BOX'                                                 707
         AG_EXNAME = 'SHAPE'                                              707
         AG_PARLIST = 'DX_DY_DZ'                                          707
         CALL AXSHAPE                                                     707
C    *                                                                    707
         CALL GSTPAR (AG_IMED,'STRA',1.)                                  709
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  713
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  713
C    ,0.001)                                                              713
         AG_TITLE = 'PLAC'                                                713
         AG_EXNAME = 'HITS'                                               713
         AG_TYPE=AG_MAGIC                                                 713
         CALL AGSDETV('PLAC')                                             713
C    *                                                                    713
         AG_PARLIST='Z'                                                   713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.001                                                      713
         AG_OPTION='S'                                                    713
         AG_FHMIN = AG_MAGIC                                              713
         AG_FHMAX = AG_MAGIC                                              713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='Y'                                                   713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.001                                                      713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='X'                                                   713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.001                                                      713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='PTOT'                                                713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 16                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 100                                                   713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='CX'                                                  713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 10                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='CY'                                                  713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 10                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='CZ'                                                  713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 10                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='SLENG'                                               713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 16                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 500                                                   713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='TOF'                                                 713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 16                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 1.E-6                                                 713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='STEP'                                                713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = AG_MAGIC                                              713
         AG_BIN=.01                                                       713
         AG_FHMIN=AG_MAGIC                                                713
         AG_FHMAX=AG_MAGIC                                                713
         AG_OPTION='*'                                                    713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         AG_PARLIST='ELOSS'                                               713
         AG_TITLE = 'DETH'                                                713
         AG_NBITS = 16                                                    713
         AG_BIN=AG_MAGIC                                                  713
         AG_OPTION='*'                                                    713
         AG_FHMIN = 0                                                     713
         AG_FHMAX = 0.001                                                 713
         CALL AGSDETH (0)                                                 713
C    *                                                                    713
         CALL AGSDETU('PLAC')                                             713
C    *                                                                    713
5111  CALL AGSPOPB                                                        714
      GO TO BLOCK_PLAC, (5110 ,5120)                                      714
C End of Local Procedure                                                  714
* -----------------------------------------------------------------------------
C Begin Local Procedure PLPS                                              716
C PLPS IS THE PASSIVE LAYER OF THE LADDER                                 716
5130  CONTINUE                                                            716
         AG_TITLE='PLPS'                                                  716
         AG_MODULE=MODULE                                                 716
         AG_MARK='V00'                                                    716
         CALL AGSPUSH                                                     716
         AG_MODULE=MODULE                                                 716
C    *                                                                    716
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          717
         CALL AGSRESET                                                    717
         AG_TITLE = 'SILICON'                                             717
         AG_EXNAME = 'MATERIAL'                                           717
         AG_PARLIST = 'NONE'                                              717
         CALL AXMATERIAL                                                  717
C    *                                                                    717
C    Geant ATTRIBUTE : PLPS ===> SEEN=1 COLO=2 <===                       717
         CALL AGSRESET                                                    718
         AG_SEEN = 1                                                      718
         AG_COLO = 2                                                      718
         AG_TITLE = 'PLPS'                                                718
         AG_EXNAME = 'ATTRIBUTE'                                          718
         AG_PARLIST = 'SEEN_COLO'                                         718
         CALL AXATTRIBUTE                                                 718
C    *                                                                    718
C    Geant SHAPE : BOX ===> DX=PIXG_LADDERWIDTH/2.0 DY=PIXG_PASSIVETHK/2  718
C    .0 DZ=PIXG_TOTALLENGTH/2.0 <===                                      718
         CALL AGSRESET                                                    719
         AG_DX = PIXG_LADDERWIDTH/2.0                                     719
         AG_DY = PIXG_PASSIVETHK/2.0                                      719
         AG_DZ = PIXG_TOTALLENGTH/2.0                                     719
         AG_TITLE = 'BOX'                                                 719
         AG_EXNAME = 'SHAPE'                                              719
         AG_PARLIST = 'DX_DY_DZ'                                          719
         CALL AXSHAPE                                                     719
C    *                                                                    719
5131  CALL AGSPOPB                                                        719
      GO TO BLOCK_PLPS, (5130 ,5140)                                      720
C End of Local Procedure                                                  720
      END                                                                 722
