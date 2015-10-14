C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIXLGEO3                                                 1
C PIXLGEO3 IS THE THE STAR PIXEL DETECTOR AND BEAM PIPE SUPPORT           1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIXLGEO3'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE THE STAR PIXEL DETECTOR AND BEAM PIPE SUPPOR  1
     *T'/                                                                 1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 10/02/06                                                        1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'10/02/06'/                                   2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR ANDREW ROSE                                                      2
      CHARACTER*40 AUTHOR /'ANDREW ROSE'/                                 3
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
C - GEANT description of logical units                                    4
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
      REAL *4 ANGLE,ANGLEPOS,ANGLECORR,RADDEG                             5
      INTEGER *4 NLADDER,NSECTOR,NEXTRALADDER                             8
      INTEGER BLOCK_PXMO                                                  10
      INTEGER BLOCK_PSEC                                                  10
      INTEGER BLOCK_PLMO                                                  10
      INTEGER BLOCK_PLAC                                                  10
      INTEGER BLOCK_PLPS                                                  10
      INTEGER BLOCK_PXBX                                                  10
C -                                                                       11
      INTEGER PXLV_VERSION                                                11
      EQUIVALENCE (BANK_PXLV(3),PXLV_VERSION,RE_PXLV_VERSION),            11
     *(NAMES_PXLV(1),NPXLV_VERSION)                                       11
      EQUIVALENCE (COMMEN_PXLV(1),CPXLV_VERSION),(MAP_PXLV(1,1),          11
     *MPXLV_VERSION)                                                      11
      CHARACTER CPXLV_VERSION*40/' '/                                     11
      CHARACTER NPXLV_VERSION*12/'I.VERSION'/                             11
      INTEGER MPXLV_VERSION(2)/1,1/                                       11
      REAL RE_PXLV_VERSION                                                11
C -                                                                       11
      REAL PXLV_LADVER                                                    11
      EQUIVALENCE (BANK_PXLV(4),PXLV_LADVER,RE_PXLV_LADVER),              11
     *(NAMES_PXLV(2),NPXLV_LADVER)                                        11
      EQUIVALENCE (COMMEN_PXLV(2),CPXLV_LADVER),(MAP_PXLV(1,2),           11
     *MPXLV_LADVER)                                                       11
      CHARACTER CPXLV_LADVER*40/' '/                                      11
      CHARACTER NPXLV_LADVER*12/'R.LADVER'/                               11
      INTEGER MPXLV_LADVER(2)/1,1/                                        11
      REAL RE_PXLV_LADVER                                                 11
C -                                                                       11
      REAL PXLV_LOCATION                                                  11
      EQUIVALENCE (BANK_PXLV(5),PXLV_LOCATION,RE_PXLV_LOCATION),          11
     *(NAMES_PXLV(3),NPXLV_LOCATION)                                      11
      EQUIVALENCE (COMMEN_PXLV(3),CPXLV_LOCATION),(MAP_PXLV(1,3),         11
     *MPXLV_LOCATION)                                                     11
      CHARACTER CPXLV_LOCATION*40/' '/                                    11
      CHARACTER NPXLV_LOCATION*12/'R.LOCATION'/                           11
      INTEGER MPXLV_LOCATION(2)/1,1/                                      11
      REAL RE_PXLV_LOCATION                                               11
C -                                                                       11
      INTEGER LEN_PXLV,LLEN_PXLV,LINK_PXLV                                11
      PARAMETER (LEN_PXLV=5)                                              11
      PARAMETER (LLEN_PXLV=3)                                             11
      INTEGER FLAG_PXLV/0/,MAP_PXLV(2,LLEN_PXLV),IDNUM_PXLV(10)           11
      CHARACTER NAMES_PXLV(LLEN_PXLV)*12,COMMEN_PXLV(LLEN_PXLV)*40        11
      DATA IDNUM_PXLV/10*0/                                               11
      REAL BANK_PXLV(LEN_PXLV)                                            11
      CHARACTER*60 BPATH_PXLV/' '/                                        11
      SAVE FLAG_PXLV,MAP_PXLV,IDNUM_PXLV,NAMES_PXLV,COMMEN_PXLV,          11
     *BPATH_PXLV                                                          11
      DATA LINK_PXLV/0/,BANK_PXLV/LEN_PXLV*0/                             11
C *                                                                       11
C -                                                                       13
      REAL PXLD_VERSION                                                   13
      EQUIVALENCE (BANK_PXLD(3),PXLD_VERSION,RE_PXLD_VERSION),            13
     *(NAMES_PXLD(1),NPXLD_VERSION)                                       13
      EQUIVALENCE (COMMEN_PXLD(1),CPXLD_VERSION),(MAP_PXLD(1,1),          13
     *MPXLD_VERSION)                                                      13
      CHARACTER CPXLD_VERSION*40/' '/                                     13
      CHARACTER NPXLD_VERSION*12/'R.VERSION'/                             13
      INTEGER MPXLD_VERSION(2)/1,1/                                       13
      REAL RE_PXLD_VERSION                                                13
C -                                                                       13
      REAL PXLD_TOTALLENGTH                                               13
      EQUIVALENCE (BANK_PXLD(4),PXLD_TOTALLENGTH,RE_PXLD_TOTALLENGTH),    13
     *(NAMES_PXLD(2),NPXLD_TOTALLENGTH)                                   13
      EQUIVALENCE (COMMEN_PXLD(2),CPXLD_TOTALLENGTH),(MAP_PXLD(1,2),      13
     *MPXLD_TOTALLENGTH)                                                  13
      CHARACTER CPXLD_TOTALLENGTH*40/' '/                                 13
      CHARACTER NPXLD_TOTALLENGTH*12/'R.TOTALLENGTH'/                     13
      INTEGER MPXLD_TOTALLENGTH(2)/1,1/                                   13
      REAL RE_PXLD_TOTALLENGTH                                            13
C -                                                                       13
      REAL PXLD_LADDERWIDTH                                               13
      EQUIVALENCE (BANK_PXLD(5),PXLD_LADDERWIDTH,RE_PXLD_LADDERWIDTH),    13
     *(NAMES_PXLD(3),NPXLD_LADDERWIDTH)                                   13
      EQUIVALENCE (COMMEN_PXLD(3),CPXLD_LADDERWIDTH),(MAP_PXLD(1,3),      13
     *MPXLD_LADDERWIDTH)                                                  13
      CHARACTER CPXLD_LADDERWIDTH*40/' '/                                 13
      CHARACTER NPXLD_LADDERWIDTH*12/'R.LADDERWIDTH'/                     13
      INTEGER MPXLD_LADDERWIDTH(2)/1,1/                                   13
      REAL RE_PXLD_LADDERWIDTH                                            13
C -                                                                       13
      REAL PXLD_LADDERTHK                                                 13
      EQUIVALENCE (BANK_PXLD(6),PXLD_LADDERTHK,RE_PXLD_LADDERTHK),        13
     *(NAMES_PXLD(4),NPXLD_LADDERTHK)                                     13
      EQUIVALENCE (COMMEN_PXLD(4),CPXLD_LADDERTHK),(MAP_PXLD(1,4),        13
     *MPXLD_LADDERTHK)                                                    13
      CHARACTER CPXLD_LADDERTHK*40/' '/                                   13
      CHARACTER NPXLD_LADDERTHK*12/'R.LADDERTHK'/                         13
      INTEGER MPXLD_LADDERTHK(2)/1,1/                                     13
      REAL RE_PXLD_LADDERTHK                                              13
C -                                                                       13
      REAL PXLD_PASSIVETHK                                                13
      EQUIVALENCE (BANK_PXLD(7),PXLD_PASSIVETHK,RE_PXLD_PASSIVETHK),      13
     *(NAMES_PXLD(5),NPXLD_PASSIVETHK)                                    13
      EQUIVALENCE (COMMEN_PXLD(5),CPXLD_PASSIVETHK),(MAP_PXLD(1,5),       13
     *MPXLD_PASSIVETHK)                                                   13
      CHARACTER CPXLD_PASSIVETHK*40/' '/                                  13
      CHARACTER NPXLD_PASSIVETHK*12/'R.PASSIVETHK'/                       13
      INTEGER MPXLD_PASSIVETHK(2)/1,1/                                    13
      REAL RE_PXLD_PASSIVETHK                                             13
C -                                                                       13
      REAL PXLD_ACTIVETHK                                                 13
      EQUIVALENCE (BANK_PXLD(8),PXLD_ACTIVETHK,RE_PXLD_ACTIVETHK),        13
     *(NAMES_PXLD(6),NPXLD_ACTIVETHK)                                     13
      EQUIVALENCE (COMMEN_PXLD(6),CPXLD_ACTIVETHK),(MAP_PXLD(1,6),        13
     *MPXLD_ACTIVETHK)                                                    13
      CHARACTER CPXLD_ACTIVETHK*40/' '/                                   13
      CHARACTER NPXLD_ACTIVETHK*12/'R.ACTIVETHK'/                         13
      INTEGER MPXLD_ACTIVETHK(2)/1,1/                                     13
      REAL RE_PXLD_ACTIVETHK                                              13
C -                                                                       13
      REAL PXLD_RIN                                                       13
      EQUIVALENCE (BANK_PXLD(9),PXLD_RIN,RE_PXLD_RIN),(NAMES_PXLD(7),     13
     *NPXLD_RIN)                                                          13
      EQUIVALENCE (COMMEN_PXLD(7),CPXLD_RIN),(MAP_PXLD(1,7),MPXLD_RIN)    13
      CHARACTER CPXLD_RIN*40/' '/                                         13
      CHARACTER NPXLD_RIN*12/'R.RIN'/                                     13
      INTEGER MPXLD_RIN(2)/1,1/                                           13
      REAL RE_PXLD_RIN                                                    13
C -                                                                       13
      REAL PXLD_ROUT                                                      13
      EQUIVALENCE (BANK_PXLD(10),PXLD_ROUT,RE_PXLD_ROUT),(NAMES_PXLD(8),  13
     *NPXLD_ROUT)                                                         13
      EQUIVALENCE (COMMEN_PXLD(8),CPXLD_ROUT),(MAP_PXLD(1,8),             13
     *MPXLD_ROUT)                                                         13
      CHARACTER CPXLD_ROUT*40/' '/                                        13
      CHARACTER NPXLD_ROUT*12/'R.ROUT'/                                   13
      INTEGER MPXLD_ROUT(2)/1,1/                                          13
      REAL RE_PXLD_ROUT                                                   13
C -                                                                       13
      INTEGER LEN_PXLD,LLEN_PXLD,LINK_PXLD                                13
      PARAMETER (LEN_PXLD=10)                                             13
      PARAMETER (LLEN_PXLD=8)                                             13
      INTEGER FLAG_PXLD/0/,MAP_PXLD(2,LLEN_PXLD),IDNUM_PXLD(10)           13
      CHARACTER NAMES_PXLD(LLEN_PXLD)*12,COMMEN_PXLD(LLEN_PXLD)*40        13
      DATA IDNUM_PXLD/10*0/                                               13
      REAL BANK_PXLD(LEN_PXLD)                                            13
      CHARACTER*60 BPATH_PXLD/' '/                                        13
      SAVE FLAG_PXLD,MAP_PXLD,IDNUM_PXLD,NAMES_PXLD,COMMEN_PXLD,          13
     *BPATH_PXLD                                                          13
      DATA LINK_PXLD/0/,BANK_PXLD/LEN_PXLD*0/                             13
C *                                                                       13
C -                                                                       14
      REAL PIXG_LADDER                                                    14
      EQUIVALENCE (BANK_PIXG(3),PIXG_LADDER,RE_PIXG_LADDER),              14
     *(NAMES_PIXG(1),NPIXG_LADDER)                                        14
      EQUIVALENCE (COMMEN_PIXG(1),CPIXG_LADDER),(MAP_PIXG(1,1),           14
     *MPIXG_LADDER)                                                       14
      CHARACTER CPIXG_LADDER*40/' '/                                      14
      CHARACTER NPIXG_LADDER*12/'R.LADDER'/                               14
      INTEGER MPIXG_LADDER(2)/1,1/                                        14
      REAL RE_PIXG_LADDER                                                 14
C -                                                                       14
      REAL PIXG_R                                                         14
      EQUIVALENCE (BANK_PIXG(4),PIXG_R,RE_PIXG_R),(NAMES_PIXG(2),         14
     *NPIXG_R)                                                            14
      EQUIVALENCE (COMMEN_PIXG(2),CPIXG_R),(MAP_PIXG(1,2),MPIXG_R)        14
      CHARACTER CPIXG_R*40/' '/                                           14
      CHARACTER NPIXG_R*12/'R.R'/                                         14
      INTEGER MPIXG_R(2)/1,1/                                             14
      REAL RE_PIXG_R                                                      14
C -                                                                       14
      REAL PIXG_A                                                         14
      EQUIVALENCE (BANK_PIXG(5),PIXG_A,RE_PIXG_A),(NAMES_PIXG(3),         14
     *NPIXG_A)                                                            14
      EQUIVALENCE (COMMEN_PIXG(3),CPIXG_A),(MAP_PIXG(1,3),MPIXG_A)        14
      CHARACTER CPIXG_A*40/' '/                                           14
      CHARACTER NPIXG_A*12/'R.A'/                                         14
      INTEGER MPIXG_A(2)/1,1/                                             14
      REAL RE_PIXG_A                                                      14
C -                                                                       14
      REAL PIXG_POFFSET                                                   14
      EQUIVALENCE (BANK_PIXG(6),PIXG_POFFSET,RE_PIXG_POFFSET),            14
     *(NAMES_PIXG(4),NPIXG_POFFSET)                                       14
      EQUIVALENCE (COMMEN_PIXG(4),CPIXG_POFFSET),(MAP_PIXG(1,4),          14
     *MPIXG_POFFSET)                                                      14
      CHARACTER CPIXG_POFFSET*40/' '/                                     14
      CHARACTER NPIXG_POFFSET*12/'R.POFFSET'/                             14
      INTEGER MPIXG_POFFSET(2)/1,1/                                       14
      REAL RE_PIXG_POFFSET                                                14
C -                                                                       14
      REAL PIXG_AOFFSET                                                   14
      EQUIVALENCE (BANK_PIXG(7),PIXG_AOFFSET,RE_PIXG_AOFFSET),            14
     *(NAMES_PIXG(5),NPIXG_AOFFSET)                                       14
      EQUIVALENCE (COMMEN_PIXG(5),CPIXG_AOFFSET),(MAP_PIXG(1,5),          14
     *MPIXG_AOFFSET)                                                      14
      CHARACTER CPIXG_AOFFSET*40/' '/                                     14
      CHARACTER NPIXG_AOFFSET*12/'R.AOFFSET'/                             14
      INTEGER MPIXG_AOFFSET(2)/1,1/                                       14
      REAL RE_PIXG_AOFFSET                                                14
C -                                                                       14
      INTEGER LEN_PIXG,LLEN_PIXG,LINK_PIXG                                14
      PARAMETER (LEN_PIXG=7)                                              14
      PARAMETER (LLEN_PIXG=5)                                             14
      INTEGER FLAG_PIXG/0/,MAP_PIXG(2,LLEN_PIXG),IDNUM_PIXG(10)           14
      CHARACTER NAMES_PIXG(LLEN_PIXG)*12,COMMEN_PIXG(LLEN_PIXG)*40        14
      DATA IDNUM_PIXG/10*0/                                               14
      REAL BANK_PIXG(LEN_PIXG)                                            14
      CHARACTER*60 BPATH_PIXG/' '/                                        14
      SAVE FLAG_PIXG,MAP_PIXG,IDNUM_PIXG,NAMES_PIXG,COMMEN_PIXG,          14
     *BPATH_PIXG                                                          14
      DATA LINK_PIXG/0/,BANK_PIXG/LEN_PIXG*0/                             14
C *                                                                       14
C -                                                                       15
      REAL PXBG_VERSION                                                   15
      EQUIVALENCE (BANK_PXBG(3),PXBG_VERSION,RE_PXBG_VERSION),            15
     *(NAMES_PXBG(1),NPXBG_VERSION)                                       15
      EQUIVALENCE (COMMEN_PXBG(1),CPXBG_VERSION),(MAP_PXBG(1,1),          15
     *MPXBG_VERSION)                                                      15
      CHARACTER CPXBG_VERSION*40/' '/                                     15
      CHARACTER NPXBG_VERSION*12/'R.VERSION'/                             15
      INTEGER MPXBG_VERSION(2)/1,1/                                       15
      REAL RE_PXBG_VERSION                                                15
C -                                                                       15
      REAL PXBG_LENGTH                                                    15
      EQUIVALENCE (BANK_PXBG(4),PXBG_LENGTH,RE_PXBG_LENGTH),              15
     *(NAMES_PXBG(2),NPXBG_LENGTH)                                        15
      EQUIVALENCE (COMMEN_PXBG(2),CPXBG_LENGTH),(MAP_PXBG(1,2),           15
     *MPXBG_LENGTH)                                                       15
      CHARACTER CPXBG_LENGTH*40/' '/                                      15
      CHARACTER NPXBG_LENGTH*12/'R.LENGTH'/                               15
      INTEGER MPXBG_LENGTH(2)/1,1/                                        15
      REAL RE_PXBG_LENGTH                                                 15
C -                                                                       15
      REAL PXBG_RIN                                                       15
      EQUIVALENCE (BANK_PXBG(5),PXBG_RIN,RE_PXBG_RIN),(NAMES_PXBG(3),     15
     *NPXBG_RIN)                                                          15
      EQUIVALENCE (COMMEN_PXBG(3),CPXBG_RIN),(MAP_PXBG(1,3),MPXBG_RIN)    15
      CHARACTER CPXBG_RIN*40/' '/                                         15
      CHARACTER NPXBG_RIN*12/'R.RIN'/                                     15
      INTEGER MPXBG_RIN(2)/1,1/                                           15
      REAL RE_PXBG_RIN                                                    15
C -                                                                       15
      REAL PXBG_THK                                                       15
      EQUIVALENCE (BANK_PXBG(6),PXBG_THK,RE_PXBG_THK),(NAMES_PXBG(4),     15
     *NPXBG_THK)                                                          15
      EQUIVALENCE (COMMEN_PXBG(4),CPXBG_THK),(MAP_PXBG(1,4),MPXBG_THK)    15
      CHARACTER CPXBG_THK*40/' '/                                         15
      CHARACTER NPXBG_THK*12/'R.THK'/                                     15
      INTEGER MPXBG_THK(2)/1,1/                                           15
      REAL RE_PXBG_THK                                                    15
C -                                                                       15
      INTEGER LEN_PXBG,LLEN_PXBG,LINK_PXBG                                15
      PARAMETER (LEN_PXBG=6)                                              15
      PARAMETER (LLEN_PXBG=4)                                             15
      INTEGER FLAG_PXBG/0/,MAP_PXBG(2,LLEN_PXBG),IDNUM_PXBG(10)           15
      CHARACTER NAMES_PXBG(LLEN_PXBG)*12,COMMEN_PXBG(LLEN_PXBG)*40        15
      DATA IDNUM_PXBG/10*0/                                               15
      REAL BANK_PXBG(LEN_PXBG)                                            15
      CHARACTER*60 BPATH_PXBG/' '/                                        15
      SAVE FLAG_PXBG,MAP_PXBG,IDNUM_PXBG,NAMES_PXBG,COMMEN_PXBG,          15
     *BPATH_PXBG                                                          15
      DATA LINK_PXBG/0/,BANK_PXBG/LEN_PXBG*0/                             15
C *                                                                       15
C *                                                                       16
      CALL AGSBEGM(MODULE,IPRIN)                                          16
      IF (IPRIN.LT.0) RETURN                                              16
C Filling bank PXLV - PIXEL LADDER DATA                                   16
      BANK_TITLE='PIXEL LADDER DATA'                                      16
      BPATH_PXLV='PXLV*'                                                  16
      IDNUM_PXLV(1)=0                                                     17
C CONFIG VERSION                                                          17
      PXLV_VERSION=1.0                                                    17
      CPXLV_VERSION='CONFIG VERSION'                                      17
C LADDER VERSION                                                          18
      PXLV_LADVER=1.0                                                     18
      CPXLV_LADVER='LADDER VERSION'                                       18
C LOCATION: 1=CAVE, 2=IDSM                                                19
      PXLV_LOCATION=1.0                                                   19
      CPXLV_LOCATION='LOCATION: 1=CAVE, 2=IDSM'                           19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLV',    20
     *BPATH_PXLV,IDNUM_PXLV,1, MAP_PXLV,NAMES_PXLV,COMMEN_PXLV,           20
     *BANK_PXLV,LEN_PXLV,LLEN_PXLV,LINK_PXLV,FLAG_PXLV)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLV',    20
     *BPATH_PXLV,IDNUM_PXLV,1, MAP_PXLV,NAMES_PXLV,COMMEN_PXLV,           20
     *BANK_PXLV,LEN_PXLV,LLEN_PXLV,LINK_PXLV,FLAG_PXLV)                   20
C *                                                                       21
      CALL AGSBEGM(MODULE,IPRIN)                                          21
      IF (IPRIN.LT.0) RETURN                                              21
C Filling bank PXLD - PIXEL LADDER DATA                                   21
      BANK_TITLE='PIXEL LADDER DATA'                                      21
      BPATH_PXLD='PXLD*'                                                  21
      IDNUM_PXLD(1)=0                                                     22
C VERSION                                                                 22
      PXLD_VERSION=1.0                                                    22
      CPXLD_VERSION='VERSION'                                             22
C OVERAL LENGTH OF THE DETECTOR                                           23
      PXLD_TOTALLENGTH=20.0                                               23
      CPXLD_TOTALLENGTH='OVERAL LENGTH OF THE DETECTOR'                   23
C LADDER WIDTH                                                            24
      PXLD_LADDERWIDTH=2.00                                               24
      CPXLD_LADDERWIDTH='LADDER WIDTH'                                    24
C TOTAL LADDER THICKNESS                                                  25
      PXLD_LADDERTHK=0.0240                                               25
      CPXLD_LADDERTHK='TOTAL LADDER THICKNESS'                            25
C PASSIVE SILICON THICKNESS                                               26
      PXLD_PASSIVETHK=0.0220                                              26
      CPXLD_PASSIVETHK='PASSIVE SILICON THICKNESS'                        26
C ACTIVE SILICON THICKNESS                                                27
      PXLD_ACTIVETHK=0.0020                                               27
      CPXLD_ACTIVETHK='ACTIVE SILICON THICKNESS'                          27
C INNER RADIUS                                                            28
      PXLD_RIN=2.4                                                        28
      CPXLD_RIN='INNER RADIUS'                                            28
C OUTER RADIUS                                                            29
      PXLD_ROUT=8.3                                                       29
      CPXLD_ROUT='OUTER RADIUS'                                           29
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLD',    30
     *BPATH_PXLD,IDNUM_PXLD,1, MAP_PXLD,NAMES_PXLD,COMMEN_PXLD,           30
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   30
C *                                                                       30
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLD',    30
     *BPATH_PXLD,IDNUM_PXLD,1, MAP_PXLD,NAMES_PXLD,COMMEN_PXLD,           30
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   30
C *                                                                       31
      CALL AGSBEGM(MODULE,IPRIN)                                          31
      IF (IPRIN.LT.0) RETURN                                              31
C Filling bank PXLD - PIXEL LADDER DATA                                   31
      BANK_TITLE='PIXEL LADDER DATA'                                      31
      BPATH_PXLD='PXLD*'                                                  31
      IDNUM_PXLD(1)=0                                                     32
C VERSION                                                                 32
      PXLD_VERSION=2.0                                                    32
      CPXLD_VERSION='VERSION'                                             32
C LADDER WIDTH                                                            33
      PXLD_LADDERWIDTH=2.00                                               33
      CPXLD_LADDERWIDTH='LADDER WIDTH'                                    33
C TOTAL LADDER THICKNESS                                                  34
      PXLD_LADDERTHK=0.0240                                               34
      CPXLD_LADDERTHK='TOTAL LADDER THICKNESS'                            34
C PASSIVE SILICON THICKNESS                                               35
      PXLD_PASSIVETHK=0.0120                                              35
      CPXLD_PASSIVETHK='PASSIVE SILICON THICKNESS'                        35
C ACTIVE SILICON THICKNESS                                                36
      PXLD_ACTIVETHK=0.0120                                               36
      CPXLD_ACTIVETHK='ACTIVE SILICON THICKNESS'                          36
C INNER RADIUS                                                            37
      PXLD_RIN=2.4                                                        37
      CPXLD_RIN='INNER RADIUS'                                            37
C OUTER RADIUS                                                            38
      PXLD_ROUT=8.3                                                       38
      CPXLD_ROUT='OUTER RADIUS'                                           38
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLD',    39
     *BPATH_PXLD,IDNUM_PXLD,1, MAP_PXLD,NAMES_PXLD,COMMEN_PXLD,           39
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   39
C *                                                                       39
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLD',    39
     *BPATH_PXLD,IDNUM_PXLD,1, MAP_PXLD,NAMES_PXLD,COMMEN_PXLD,           39
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   39
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank PIXG - PIXEL DETECTOR DATA                                 40
      BANK_TITLE='PIXEL DETECTOR DATA'                                    40
      BPATH_PIXG='PIXG*'                                                  40
      IDNUM_PIXG(1)=0                                                     41
C LADDER INDEX                                                            41
      PIXG_LADDER=1                                                       41
      CPIXG_LADDER='LADDER INDEX'                                         41
C 1ST LADDER NOMINAL RADIUS                                               42
      PIXG_R=2.5                                                          42
      CPIXG_R='1ST LADDER NOMINAL RADIUS'                                 42
C 1ST LADDER NOMINAL POSITION ANGLE                                       43
      PIXG_A=100.                                                         43
      CPIXG_A='1ST LADDER NOMINAL POSITION ANGLE'                         43
C ANGULAR OFFSET                                                          44
      PIXG_AOFFSET=103.                                                   44
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      44
C POSITION OFFSET (SHIFT)                                                 45
      PIXG_POFFSET=0.0                                                    45
      CPIXG_POFFSET='POSITION OFFSET (SHIFT)'                             45
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    46
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           46
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   46
C *                                                                       46
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    46
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           46
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   46
C *                                                                       47
      CALL AGSBEGM(MODULE,IPRIN)                                          47
      IF (IPRIN.LT.0) RETURN                                              47
C Filling bank PIXG - PIXEL DETECTOR DATA                                 47
      BANK_TITLE='PIXEL DETECTOR DATA'                                    47
      BPATH_PIXG='PIXG*'                                                  47
      IDNUM_PIXG(1)=0                                                     48
C LADDER INDEX                                                            48
      PIXG_LADDER=2                                                       48
      CPIXG_LADDER='LADDER INDEX'                                         48
C 2ND LADDER NOMINAL RADIUS                                               49
      PIXG_R=2.5                                                          49
      CPIXG_R='2ND LADDER NOMINAL RADIUS'                                 49
C 2ND LADDER NOMINAL POSITION ANGLE                                       50
      PIXG_A=60.                                                          50
      CPIXG_A='2ND LADDER NOMINAL POSITION ANGLE'                         50
C ANGULAR OFFSET                                                          51
      PIXG_AOFFSET=103.                                                   51
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      51
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    52
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           52
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   52
C *                                                                       52
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    52
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           52
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   52
C *                                                                       53
      CALL AGSBEGM(MODULE,IPRIN)                                          53
      IF (IPRIN.LT.0) RETURN                                              53
C Filling bank PIXG - PIXEL DETECTOR DATA                                 53
      BANK_TITLE='PIXEL DETECTOR DATA'                                    53
      BPATH_PIXG='PIXG*'                                                  53
      IDNUM_PIXG(1)=0                                                     54
C LADDER INDEX                                                            54
      PIXG_LADDER=3                                                       54
      CPIXG_LADDER='LADDER INDEX'                                         54
C 2ND LADDER NOMINAL RADIUS                                               55
      PIXG_R=2.5                                                          55
      CPIXG_R='2ND LADDER NOMINAL RADIUS'                                 55
C 2ND LADDER NOMINAL POSITION ANGLE                                       56
      PIXG_A=20.                                                          56
      CPIXG_A='2ND LADDER NOMINAL POSITION ANGLE'                         56
C ANGULAR OFFSET                                                          57
      PIXG_AOFFSET=103.                                                   57
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      57
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    58
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           58
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   58
C *                                                                       58
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    58
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           58
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   58
C *                                                                       59
      CALL AGSBEGM(MODULE,IPRIN)                                          59
      IF (IPRIN.LT.0) RETURN                                              59
C Filling bank PIXG - PIXEL DETECTOR DATA                                 59
      BANK_TITLE='PIXEL DETECTOR DATA'                                    59
      BPATH_PIXG='PIXG*'                                                  59
      IDNUM_PIXG(1)=0                                                     60
C LADDER INDEX                                                            60
      PIXG_LADDER=4                                                       60
      CPIXG_LADDER='LADDER INDEX'                                         60
C 2ND LADDER NOMINAL RADIUS                                               61
      PIXG_R=6.5                                                          61
      CPIXG_R='2ND LADDER NOMINAL RADIUS'                                 61
C 2ND LADDER NOMINAL POSITION ANGLE                                       62
      PIXG_A=105.                                                         62
      CPIXG_A='2ND LADDER NOMINAL POSITION ANGLE'                         62
C ANGULAR OFFSET                                                          63
      PIXG_AOFFSET=90.                                                    63
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      63
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    64
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           64
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   64
C *                                                                       64
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    64
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           64
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   64
C *                                                                       65
      CALL AGSBEGM(MODULE,IPRIN)                                          65
      IF (IPRIN.LT.0) RETURN                                              65
C Filling bank PIXG - PIXEL DETECTOR DATA                                 65
      BANK_TITLE='PIXEL DETECTOR DATA'                                    65
      BPATH_PIXG='PIXG*'                                                  65
      IDNUM_PIXG(1)=0                                                     66
C LADDER INDEX                                                            66
      PIXG_LADDER=5                                                       66
      CPIXG_LADDER='LADDER INDEX'                                         66
C 3RD LADDER RADIUS                                                       67
      PIXG_R=7.5                                                          67
      CPIXG_R='3RD LADDER RADIUS'                                         67
C 3RD LADDER NOMINAL POSITION ANGLE                                       68
      PIXG_A=90.                                                          68
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         68
C ANGULAR OFFSET                                                          69
      PIXG_AOFFSET=90.                                                    69
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      69
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    70
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           70
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   70
C *                                                                       70
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    70
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           70
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   70
C *                                                                       71
      CALL AGSBEGM(MODULE,IPRIN)                                          71
      IF (IPRIN.LT.0) RETURN                                              71
C Filling bank PIXG - PIXEL DETECTOR DATA                                 71
      BANK_TITLE='PIXEL DETECTOR DATA'                                    71
      BPATH_PIXG='PIXG*'                                                  71
      IDNUM_PIXG(1)=0                                                     72
C LADDER INDEX                                                            72
      PIXG_LADDER=6                                                       72
      CPIXG_LADDER='LADDER INDEX'                                         72
C 4TH LADDER NOMINAL RADIUS                                               73
      PIXG_R=6.5                                                          73
      CPIXG_R='4TH LADDER NOMINAL RADIUS'                                 73
C 4TH LADDER NOMINAL POSITION ANGLE                                       74
      PIXG_A=75.                                                          74
      CPIXG_A='4TH LADDER NOMINAL POSITION ANGLE'                         74
C ANGULAR OFFSET                                                          75
      PIXG_AOFFSET=90.                                                    75
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      75
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    76
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           76
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   76
C *                                                                       76
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    76
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           76
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   76
C *                                                                       77
      CALL AGSBEGM(MODULE,IPRIN)                                          77
      IF (IPRIN.LT.0) RETURN                                              77
C Filling bank PIXG - PIXEL DETECTOR DATA                                 77
      BANK_TITLE='PIXEL DETECTOR DATA'                                    77
      BPATH_PIXG='PIXG*'                                                  77
      IDNUM_PIXG(1)=0                                                     78
C LADDER INDEX                                                            78
      PIXG_LADDER=7                                                       78
      CPIXG_LADDER='LADDER INDEX'                                         78
C 3RD LADDER RADIUS                                                       79
      PIXG_R=7.5                                                          79
      CPIXG_R='3RD LADDER RADIUS'                                         79
C 3RD LADDER NOMINAL POSITION ANGLE                                       80
      PIXG_A=60.                                                          80
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         80
C ANGULAR OFFSET                                                          81
      PIXG_AOFFSET=90.                                                    81
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      81
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    82
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           82
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   82
C *                                                                       82
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    82
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           82
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   82
C *                                                                       83
      CALL AGSBEGM(MODULE,IPRIN)                                          83
      IF (IPRIN.LT.0) RETURN                                              83
C Filling bank PIXG - PIXEL DETECTOR DATA                                 83
      BANK_TITLE='PIXEL DETECTOR DATA'                                    83
      BPATH_PIXG='PIXG*'                                                  83
      IDNUM_PIXG(1)=0                                                     84
C LADDER INDEX                                                            84
      PIXG_LADDER=8                                                       84
      CPIXG_LADDER='LADDER INDEX'                                         84
C 4TH LADDER NOMINAL RADIUS                                               85
      PIXG_R=6.5                                                          85
      CPIXG_R='4TH LADDER NOMINAL RADIUS'                                 85
C 4TH LADDER NOMINAL POSITION ANGLE                                       86
      PIXG_A=45.                                                          86
      CPIXG_A='4TH LADDER NOMINAL POSITION ANGLE'                         86
C ANGULAR OFFSET                                                          87
      PIXG_AOFFSET=90.                                                    87
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      87
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    88
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           88
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   88
C *                                                                       88
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    88
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           88
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   88
C *                                                                       89
      CALL AGSBEGM(MODULE,IPRIN)                                          89
      IF (IPRIN.LT.0) RETURN                                              89
C Filling bank PIXG - PIXEL DETECTOR DATA                                 89
      BANK_TITLE='PIXEL DETECTOR DATA'                                    89
      BPATH_PIXG='PIXG*'                                                  89
      IDNUM_PIXG(1)=0                                                     90
C LADDER INDEX                                                            90
      PIXG_LADDER=9                                                       90
      CPIXG_LADDER='LADDER INDEX'                                         90
C 3RD LADDER RADIUS                                                       91
      PIXG_R=7.5                                                          91
      CPIXG_R='3RD LADDER RADIUS'                                         91
C 3RD LADDER NOMINAL POSITION ANGLE                                       92
      PIXG_A=30.                                                          92
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         92
C ANGULAR OFFSET                                                          93
      PIXG_AOFFSET=90.                                                    93
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      93
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    94
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           94
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   94
C *                                                                       94
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    94
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           94
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   94
C *                                                                       95
      CALL AGSBEGM(MODULE,IPRIN)                                          95
      IF (IPRIN.LT.0) RETURN                                              95
C Filling bank PIXG - PIXEL DETECTOR DATA                                 95
      BANK_TITLE='PIXEL DETECTOR DATA'                                    95
      BPATH_PIXG='PIXG*'                                                  95
      IDNUM_PIXG(1)=0                                                     96
C LADDER INDEX                                                            96
      PIXG_LADDER=10                                                      96
      CPIXG_LADDER='LADDER INDEX'                                         96
C 4TH LADDER NOMINAL RADIUS                                               97
      PIXG_R=6.5                                                          97
      CPIXG_R='4TH LADDER NOMINAL RADIUS'                                 97
C 4TH LADDER NOMINAL POSITION ANGLE                                       98
      PIXG_A=15.                                                          98
      CPIXG_A='4TH LADDER NOMINAL POSITION ANGLE'                         98
C ANGULAR OFFSET                                                          99
      PIXG_AOFFSET=90.                                                    99
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      99
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    100
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           100
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   100
C *                                                                       100
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    100
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           100
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   100
C *                                                                       101
      CALL AGSBEGM(MODULE,IPRIN)                                          101
      IF (IPRIN.LT.0) RETURN                                              101
C Filling bank PIXG - PIXEL DETECTOR DATA                                 101
      BANK_TITLE='PIXEL DETECTOR DATA'                                    101
      BPATH_PIXG='PIXG*'                                                  101
      IDNUM_PIXG(1)=0                                                     102
C LADDER INDEX                                                            102
      PIXG_LADDER=11                                                      102
      CPIXG_LADDER='LADDER INDEX'                                         102
C 3RD LADDER RADIUS                                                       103
      PIXG_R=7.5                                                          103
      CPIXG_R='3RD LADDER RADIUS'                                         103
C 3RD LADDER NOMINAL POSITION ANGLE                                       104
      PIXG_A=0.                                                           104
      CPIXG_A='3RD LADDER NOMINAL POSITION ANGLE'                         104
C ANGULAR OFFSET                                                          105
      PIXG_AOFFSET=90.                                                    105
      CPIXG_AOFFSET='ANGULAR OFFSET'                                      105
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    106
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           106
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   106
C *                                                                       106
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXG',    106
     *BPATH_PIXG,IDNUM_PIXG,1, MAP_PIXG,NAMES_PIXG,COMMEN_PIXG,           106
     *BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)                   106
C *                                                                       107
      CALL AGSBEGM(MODULE,IPRIN)                                          107
      IF (IPRIN.LT.0) RETURN                                              107
C Filling bank PXBG - BEAM PIPE EXOSKELETON DATA                          107
      BANK_TITLE='BEAM PIPE EXOSKELETON DATA'                             107
      BPATH_PXBG='PXBG*'                                                  107
      IDNUM_PXBG(1)=0                                                     108
C VERSION                                                                 108
      PXBG_VERSION=2                                                      108
      CPXBG_VERSION='VERSION'                                             108
C TOTAL LENGTH                                                            109
      PXBG_LENGTH=48.0                                                    109
      CPXBG_LENGTH='TOTAL LENGTH'                                         109
C INNER RADIUS                                                            110
      PXBG_RIN=8.5                                                        110
      CPXBG_RIN='INNER RADIUS'                                            110
C THICKNESS                                                               111
      PXBG_THK=0.1                                                        111
      CPXBG_THK='THICKNESS'                                               111
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXBG',    112
     *BPATH_PXBG,IDNUM_PXBG,1, MAP_PXBG,NAMES_PXBG,COMMEN_PXBG,           112
     *BANK_PXBG,LEN_PXBG,LLEN_PXBG,LINK_PXBG,FLAG_PXBG)                   112
C *                                                                       112
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXBG',    112
     *BPATH_PXBG,IDNUM_PXBG,1, MAP_PXBG,NAMES_PXBG,COMMEN_PXBG,           112
     *BANK_PXBG,LEN_PXBG,LLEN_PXBG,LINK_PXBG,FLAG_PXBG)                   112
C use default PXLV bank                                                   113
      BPATH_PXLV='PXLV*'                                                  113
      IDNUM_PXLV(1)=0                                                     113
      CALL AGDGETP('PIXLGEO3','*',-999,'PXLV',' ',BANK_PXLV, BPATH_PXLV,  113
     *IDNUM_PXLV,1,MAP_PXLV,NAMES_PXLV,BANK_PXLV,LEN_PXLV,LLEN_PXLV,      113
     *LINK_PXLV,FLAG_PXLV)                                                113
C use PXLD bank                                                           114
      BPATH_PXLD='PXLD*'                                                  114
      IDNUM_PXLD(1)=0                                                     114
      PXLD_VERSION=PXLV_LADVER                                            114
      CALL AGDGETP('PIXLGEO3','*',-999,'PXLD',NPXLD_VERSION,              114
     *RE_PXLD_VERSION,BPATH_PXLD,IDNUM_PXLD,1,MAP_PXLD,NAMES_PXLD,        114
     *BANK_PXLD,LEN_PXLD,LLEN_PXLD,LINK_PXLD,FLAG_PXLD)                   114
      RADDEG=3.14159265/180.0                                             115
      CALL AGSBEGM(MODULE,IPRIN)                                          116
C Check IPRIN<0                                                           116
      IF (IPRIN.LT.0) RETURN                                              116
C Create PXMO :                                                           116
C Call To Local Procedure PXMO                                            116
      ASSIGN 5020 TO BLOCK_PXMO                                           116
      GO TO  5010                                                         116
5020  CONTINUE                                                            116
C *                                                                       116
C Check PXLV_LOCATION==1.0                                                117
      IF (PXLV_LOCATION.EQ.1.0) THEN                                      117
C Geant POSITION : PXMO ===> MOTHER='CAVE' <===                           118
      CALL AGSRESET                                                       118
      AG_MOTHER = 'CAVE'                                                  118
      AG_TITLE = 'PXMO'                                                   118
      AG_EXNAME = 'POSITION'                                              118
      AG_PARLIST = 'MOTHER'                                               118
      CALL AXPOSITION                                                     118
C *                                                                       118
      ELSE                                                                118
C Geant POSITION : PXMO ===> MOTHER='IDSM' <===                           120
      CALL AGSRESET                                                       120
      AG_MOTHER = 'IDSM'                                                  120
      AG_TITLE = 'PXMO'                                                   120
      AG_EXNAME = 'POSITION'                                              120
      AG_PARLIST = 'MOTHER'                                               120
      CALL AXPOSITION                                                     120
C *                                                                       120
      ENDIF                                                               120
      CALL AGSBEGM(MODULE,IPRIN)                                          122
C Check IPRIN<0                                                           122
      IF (IPRIN.LT.0) RETURN                                              122
C Create PXBX :                                                           122
C Call To Local Procedure PXBX                                            122
      ASSIGN 5040 TO BLOCK_PXBX                                           122
      GO TO  5030                                                         122
5040  CONTINUE                                                            122
C *                                                                       122
C Check PXLV_LOCATION==1.0                                                123
      IF (PXLV_LOCATION.EQ.1.0) THEN                                      123
C Geant POSITION : PXBX ===> MOTHER='CAVE' <===                           124
      CALL AGSRESET                                                       124
      AG_MOTHER = 'CAVE'                                                  124
      AG_TITLE = 'PXBX'                                                   124
      AG_EXNAME = 'POSITION'                                              124
      AG_PARLIST = 'MOTHER'                                               124
      CALL AXPOSITION                                                     124
C *                                                                       124
      ELSE                                                                124
C Geant POSITION : PXBX ===> MOTHER='IDSM' <===                           126
      CALL AGSRESET                                                       126
      AG_MOTHER = 'IDSM'                                                  126
      AG_TITLE = 'PXBX'                                                   126
      AG_EXNAME = 'POSITION'                                              126
      AG_PARLIST = 'MOTHER'                                               126
      CALL AXPOSITION                                                     126
C *                                                                       126
      ENDIF                                                               126
C ----------------------------------------------------------------------  128
C ------------ PXMO                                                       128
      CALL AGSENDM                                                        129
      RETURN                                                              129
C Begin Local Procedure PXMO                                              129
C PXMO IS THE MOTHER OF THE PIXEL DETECTOR VOLUMES                        129
5010  CONTINUE                                                            129
         AG_TITLE='PXMO'                                                  129
         AG_MODULE=MODULE                                                 129
         AG_MARK='V00'                                                    129
         CALL AGSPUSH                                                     129
         AG_MODULE=MODULE                                                 129
C    *                                                                    129
C    Geant MATERIAL : AIR ===> inherited arguments used <===              130
         CALL AGSRESET                                                    130
         AG_TITLE = 'AIR'                                                 130
         AG_EXNAME = 'MATERIAL'                                           130
         AG_PARLIST = 'NONE'                                              130
         CALL AXMATERIAL                                                  130
C    *                                                                    130
C    Geant ATTRIBUTE : PXMO ===> SEEN=1 COLO=6 <===                       130
         CALL AGSRESET                                                    131
         AG_SEEN = 1                                                      131
         AG_COLO = 6                                                      131
         AG_TITLE = 'PXMO'                                                131
         AG_EXNAME = 'ATTRIBUTE'                                          131
         AG_PARLIST = 'SEEN_COLO'                                         131
         CALL AXATTRIBUTE                                                 131
C    *                                                                    131
C    Geant SHAPE : TUBE ===> RMIN=PXLD_RIN RMAX=PXLD_ROUT, DZ=PXLD_TOTAL  131
C    LENGTH/2.0 <===                                                      131
         CALL AGSRESET                                                    133
         AG_RMIN = PXLD_RIN                                               133
         AG_RMAX = PXLD_ROUT                                              133
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     133
         AG_TITLE = 'TUBE'                                                133
         AG_EXNAME = 'SHAPE'                                              133
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      133
         CALL AXSHAPE                                                     133
C    *                                                                    133
C    Create PSEC :                                                        134
C    Call To Local Procedure PSEC                                         134
         ASSIGN 5060 TO BLOCK_PSEC                                        134
         GO TO  5050                                                      134
5060     CONTINUE                                                         134
C    *                                                                    134
C    *                                                                    135
         NSECTOR=1                                                        135
            GO TO 5073                                                    135
5071        NSECTOR=NSECTOR+1                                             135
5073        IF(NSECTOR-(3).GT.0) GO TO 5072                               135
C       Geant POSITION : PSEC ===> ALPHAZ=120.0*(NSECTOR-1) KONLY='MANY'  136
C        <===                                                             136
            CALL AGSRESET                                                 136
            AG_ALPHAN=AG_ALPHAN+1                                         136
            AG_ALPHAK(AG_ALPHAN)=3                                        136
            AG_ALPHAL(AG_ALPHAN)=120.0*(NSECTOR-1)                        136
            AG_KONLY = 'MANY'                                             136
            AG_TITLE = 'PSEC'                                             136
            AG_EXNAME = 'POSITION'                                        136
            AG_PARLIST = 'ALPHAZ_KONLY'                                   136
            CALL AXPOSITION                                               136
C       *                                                                 136
         GO TO 5071                                                       137
5072     CONTINUE                                                         137
5011  CALL AGSPOPB                                                        138
      GO TO BLOCK_PXMO, (5010 ,5020)                                      138
C End of Local Procedure                                                  138
C ----------------------------------------------------------------------  139
C ------------ PXBX                                                       139
C Begin Local Procedure PXBX                                              140
C PXBX IS THE EXOSKELETON OF THE BEAMPIPE                                 140
5030  CONTINUE                                                            140
         AG_TITLE='PXBX'                                                  140
         AG_MODULE=MODULE                                                 140
         AG_MARK='V00'                                                    140
         CALL AGSPUSH                                                     140
         AG_MODULE=MODULE                                                 140
C    *                                                                    140
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        141
         CALL AGSRESET                                                    141
         AG_TITLE = 'BERILLIUM'                                           141
         AG_EXNAME = 'MATERIAL'                                           141
         AG_PARLIST = 'NONE'                                              141
         CALL AXMATERIAL                                                  141
C    *                                                                    141
C    Geant ATTRIBUTE : PXBX ===> SEEN=1 COLO=3 <===                       141
         CALL AGSRESET                                                    142
         AG_SEEN = 1                                                      142
         AG_COLO = 3                                                      142
         AG_TITLE = 'PXBX'                                                142
         AG_EXNAME = 'ATTRIBUTE'                                          142
         AG_PARLIST = 'SEEN_COLO'                                         142
         CALL AXATTRIBUTE                                                 142
C    *                                                                    142
C    Geant SHAPE : TUBE ===> RMIN=PXBG_RIN RMAX=PXBG_RIN+PXBG_THK, DZ=PX  142
C    BG_LENGTH/2.0 <===                                                   142
         CALL AGSRESET                                                    144
         AG_RMIN = PXBG_RIN                                               144
         AG_RMAX = PXBG_RIN+PXBG_THK                                      144
         AG_DZ = PXBG_LENGTH/2.0                                          144
         AG_TITLE = 'TUBE'                                                144
         AG_EXNAME = 'SHAPE'                                              144
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      144
         CALL AXSHAPE                                                     144
C    *                                                                    144
5031  CALL AGSPOPB                                                        145
      GO TO BLOCK_PXBX, (5030 ,5040)                                      145
C End of Local Procedure                                                  145
C ----------------------------------------------------------------------  146
C ------------ PSEC                                                       146
C Begin Local Procedure PSEC                                              147
C PSEC IS A GROUP OF LADDERS                                              147
5050  CONTINUE                                                            147
         AG_TITLE='PSEC'                                                  147
         AG_MODULE=MODULE                                                 147
         AG_MARK='V00'                                                    147
         CALL AGSPUSH                                                     147
         AG_MODULE=MODULE                                                 147
C    *                                                                    147
C    Geant MATERIAL : AIR ===> inherited arguments used <===              148
         CALL AGSRESET                                                    148
         AG_TITLE = 'AIR'                                                 148
         AG_EXNAME = 'MATERIAL'                                           148
         AG_PARLIST = 'NONE'                                              148
         CALL AXMATERIAL                                                  148
C    *                                                                    148
C    Geant ATTRIBUTE : PSEC ===> SEEN=1 COLO=5 <===                       148
         CALL AGSRESET                                                    149
         AG_SEEN = 1                                                      149
         AG_COLO = 5                                                      149
         AG_TITLE = 'PSEC'                                                149
         AG_EXNAME = 'ATTRIBUTE'                                          149
         AG_PARLIST = 'SEEN_COLO'                                         149
         CALL AXATTRIBUTE                                                 149
C    *                                                                    149
C    Geant SHAPE : TUBS ===> RMIN=PXLD_RIN RMAX=PXLD_ROUT, PHI1=-11.0 PH  149
C    I2=122.0 DZ=PXLD_TOTALLENGTH/2.0 <===                                149
         CALL AGSRESET                                                    151
         AG_RMIN = PXLD_RIN                                               151
         AG_RMAX = PXLD_ROUT                                              151
         AG_PHI1 = -11.0                                                  151
         AG_PHI2 = 122.0                                                  151
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     151
         AG_TITLE = 'TUBS'                                                151
         AG_EXNAME = 'SHAPE'                                              151
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            151
         CALL AXSHAPE                                                     151
C    *                                                                    151
C    *                                                                    151
         NLADDER=1                                                        152
            GO TO 5083                                                    152
5081        NLADDER=NLADDER+1                                             152
5083        IF(NLADDER-(11).GT.0) GO TO 5082                              152
C       use PIXG bank                                                     153
            BPATH_PIXG='PIXG*'                                            153
            IDNUM_PIXG(1)=0                                               153
            PIXG_LADDER=NLADDER                                           153
            CALL AGDGETP('PIXLGEO3','*',-999,'PIXG',NPIXG_LADDER,         153
     *      RE_PIXG_LADDER,BPATH_PIXG,IDNUM_PIXG,1,MAP_PIXG,NAMES_PIXG,   153
     *      BANK_PIXG,LEN_PIXG,LLEN_PIXG,LINK_PIXG,FLAG_PIXG)             153
            ANGLE = PIXG_A                                                154
C       +ANGLECORR SEE ABOVE COMMENT                                      155
            ANGLEPOS = ANGLE*RADDEG                                       155
C       Create PLMO :                                                     156
C       Call To Local Procedure PLMO                                      156
            ASSIGN 5100 TO BLOCK_PLMO                                     156
            GO TO  5090                                                   156
5100        CONTINUE                                                      156
C       *                                                                 156
C       Geant POSITION : PLMO ===> X=PIXG_R*COS(ANGLEPOS) Y=PIXG_R*SIN(A  158
C       NGLEPOS) Z=0.0 ALPHAZ=-PIXG_AOFFSET+ANGLE <===                    158
            CALL AGSRESET                                                 158
            AG_X = PIXG_R*COS(ANGLEPOS)                                   158
            AG_Y = PIXG_R*SIN(ANGLEPOS)                                   158
            AG_Z = 0.0                                                    158
            AG_ALPHAN=AG_ALPHAN+1                                         158
            AG_ALPHAK(AG_ALPHAN)=3                                        158
            AG_ALPHAL(AG_ALPHAN)=-PIXG_AOFFSET+ANGLE                      158
            AG_TITLE = 'PLMO'                                             158
            AG_EXNAME = 'POSITION'                                        158
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   158
            CALL AXPOSITION                                               158
C       *                                                                 158
         GO TO 5081                                                       159
5082     CONTINUE                                                         159
5051  CALL AGSPOPB                                                        160
      GO TO BLOCK_PSEC, (5050 ,5060)                                      160
C End of Local Procedure                                                  160
C ----------------------------------------------------------------------  161
C ------------ PLMO                                                       161
C Begin Local Procedure PLMO                                              162
C PLMO IS THE MOTHER OF THE SILICON LADDER                                162
5090  CONTINUE                                                            162
         AG_TITLE='PLMO'                                                  162
         AG_MODULE=MODULE                                                 162
         AG_MARK='V00'                                                    162
         CALL AGSPUSH                                                     162
         AG_MODULE=MODULE                                                 162
C    *                                                                    162
C    Geant MATERIAL : AIR ===> inherited arguments used <===              163
         CALL AGSRESET                                                    163
         AG_TITLE = 'AIR'                                                 163
         AG_EXNAME = 'MATERIAL'                                           163
         AG_PARLIST = 'NONE'                                              163
         CALL AXMATERIAL                                                  163
C    *                                                                    163
C    Geant ATTRIBUTE : PLMO ===> SEEN=1 COLO=4 <===                       163
         CALL AGSRESET                                                    164
         AG_SEEN = 1                                                      164
         AG_COLO = 4                                                      164
         AG_TITLE = 'PLMO'                                                164
         AG_EXNAME = 'ATTRIBUTE'                                          164
         AG_PARLIST = 'SEEN_COLO'                                         164
         CALL AXATTRIBUTE                                                 164
C    *                                                                    164
C    Geant SHAPE : BOX ===> DX=PXLD_LADDERWIDTH/2.0 DY=PXLD_LADDERTHK/2.  164
C    0, DZ=PXLD_TOTALLENGTH/2.0 <===                                      164
         CALL AGSRESET                                                    166
         AG_DX = PXLD_LADDERWIDTH/2.0                                     166
         AG_DY = PXLD_LADDERTHK/2.0                                       166
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     166
         AG_TITLE = 'BOX'                                                 166
         AG_EXNAME = 'SHAPE'                                              166
         AG_PARLIST = 'DX_DY_DZ'                                          166
         CALL AXSHAPE                                                     166
C    *                                                                    166
C    Create PLAC :                                                        167
C    Call To Local Procedure PLAC                                         167
         ASSIGN 5120 TO BLOCK_PLAC                                        167
         GO TO  5110                                                      167
5120     CONTINUE                                                         167
C    *                                                                    167
C    Geant POSITION : PLAC ===> Y=-PXLD_LADDERTHK/2.0+PXLD_ACTIVETHK/2.0  168
C     <===                                                                168
         CALL AGSRESET                                                    168
         AG_Y = -PXLD_LADDERTHK/2.0+PXLD_ACTIVETHK/2.0                    168
         AG_TITLE = 'PLAC'                                                168
         AG_EXNAME = 'POSITION'                                           168
         AG_PARLIST = 'Y'                                                 168
         CALL AXPOSITION                                                  168
C    *                                                                    168
C    Create PLPS :                                                        169
C    Call To Local Procedure PLPS                                         169
         ASSIGN 5140 TO BLOCK_PLPS                                        169
         GO TO  5130                                                      169
5140     CONTINUE                                                         169
C    *                                                                    169
C    Geant POSITION : PLPS ===> Y=-PXLD_LADDERTHK/2.0+PXLD_ACTIVETHK+PXL  170
C    D_PASSIVETHK/2.0 <===                                                170
         CALL AGSRESET                                                    170
         AG_Y = -PXLD_LADDERTHK/2.0+PXLD_ACTIVETHK+PXLD_PASSIVETHK/2.0    170
         AG_TITLE = 'PLPS'                                                170
         AG_EXNAME = 'POSITION'                                           170
         AG_PARLIST = 'Y'                                                 170
         CALL AXPOSITION                                                  170
C    *                                                                    170
5091  CALL AGSPOPB                                                        171
      GO TO BLOCK_PLMO, (5090 ,5100)                                      171
C End of Local Procedure                                                  171
C ----------------------------------------------------------------------  172
C ------------ PLAC                                                       172
C Begin Local Procedure PLAC                                              173
C PLAC IS THE ACTIVE LAYER OF THE LADDER                                  173
5110  CONTINUE                                                            173
         AG_TITLE='PLAC'                                                  173
         AG_MODULE=MODULE                                                 173
         AG_MARK='V00'                                                    173
         CALL AGSPUSH                                                     173
         AG_MODULE=MODULE                                                 173
C    *                                                                    173
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          174
         CALL AGSRESET                                                    174
         AG_TITLE = 'SILICON'                                             174
         AG_EXNAME = 'MATERIAL'                                           174
         AG_PARLIST = 'NONE'                                              174
         CALL AXMATERIAL                                                  174
C    *                                                                    174
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         174
         CALL AGSRESET                                                    175
         AG_ISVOL = 1                                                     175
         AG_TITLE = 'SENSITIVE'                                           175
         AG_EXNAME = 'MATERIAL'                                           175
         AG_PARLIST = 'ISVOL'                                             175
         CALL AXMATERIAL                                                  175
C    *                                                                    175
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=4 <===                       175
         CALL AGSRESET                                                    176
         AG_SEEN = 1                                                      176
         AG_COLO = 4                                                      176
         AG_TITLE = 'PLAC'                                                176
         AG_EXNAME = 'ATTRIBUTE'                                          176
         AG_PARLIST = 'SEEN_COLO'                                         176
         CALL AXATTRIBUTE                                                 176
C    *                                                                    176
C    Geant SHAPE : BOX ===> DX=PXLD_LADDERWIDTH/2.0 DY=PXLD_ACTIVETHK/2.  176
C    0, DZ=PXLD_TOTALLENGTH/2.0 <===                                      176
         CALL AGSRESET                                                    178
         AG_DX = PXLD_LADDERWIDTH/2.0                                     178
         AG_DY = PXLD_ACTIVETHK/2.0                                       178
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     178
         AG_TITLE = 'BOX'                                                 178
         AG_EXNAME = 'SHAPE'                                              178
         AG_PARLIST = 'DX_DY_DZ'                                          178
         CALL AXSHAPE                                                     178
C    *                                                                    178
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 178
C    Defining Hits: Z:.00001:S Y:.00001: X:.00001: PTOT:16:(0,100) CX:10  181
C    : CY:10: CZ:10:, SLENG:16:(0,500) TOF:16: STEP:.01: ELOSS:16:        181
         AG_TITLE = 'PLAC'                                                181
         AG_EXNAME = 'HITS'                                               181
         AG_TYPE=AG_MAGIC                                                 181
         CALL AGSDETV('PLAC')                                             181
C    *                                                                    181
         AG_PARLIST='Z'                                                   181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = AG_MAGIC                                              181
         AG_BIN=.00001                                                    181
         AG_OPTION='S'                                                    181
         AG_FHMIN = AG_MAGIC                                              181
         AG_FHMAX = AG_MAGIC                                              181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='Y'                                                   181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = AG_MAGIC                                              181
         AG_BIN=.00001                                                    181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='X'                                                   181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = AG_MAGIC                                              181
         AG_BIN=.00001                                                    181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='PTOT'                                                181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 16                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_OPTION='*'                                                    181
         AG_FHMIN = 0                                                     181
         AG_FHMAX = 100                                                   181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='CX'                                                  181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 10                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='CY'                                                  181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 10                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='CZ'                                                  181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 10                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_OPTION='*'                                                    181
         AG_FHMIN = AG_MAGIC                                              181
         AG_FHMAX = AG_MAGIC                                              181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='SLENG'                                               181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 16                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_OPTION='*'                                                    181
         AG_FHMIN = 0                                                     181
         AG_FHMAX = 500                                                   181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='TOF'                                                 181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 16                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='STEP'                                                181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = AG_MAGIC                                              181
         AG_BIN=.01                                                       181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         AG_PARLIST='ELOSS'                                               181
         AG_TITLE = 'DETH'                                                181
         AG_NBITS = 16                                                    181
         AG_BIN=AG_MAGIC                                                  181
         AG_FHMIN=AG_MAGIC                                                181
         AG_FHMAX=AG_MAGIC                                                181
         AG_OPTION='*'                                                    181
         CALL AGSDETH (0)                                                 181
C    *                                                                    181
         CALL AGSDETU('PLAC')                                             181
C    *                                                                    181
5111  CALL AGSPOPB                                                        182
      GO TO BLOCK_PLAC, (5110 ,5120)                                      182
C End of Local Procedure                                                  182
C ----------------------------------------------------------------------  183
C ------------ PLPS                                                       183
C Begin Local Procedure PLPS                                              184
C PLPS IS THE PASSIVE LAYER OF THE LADDER                                 184
5130  CONTINUE                                                            184
         AG_TITLE='PLPS'                                                  184
         AG_MODULE=MODULE                                                 184
         AG_MARK='V00'                                                    184
         CALL AGSPUSH                                                     184
         AG_MODULE=MODULE                                                 184
C    *                                                                    184
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          185
         CALL AGSRESET                                                    185
         AG_TITLE = 'SILICON'                                             185
         AG_EXNAME = 'MATERIAL'                                           185
         AG_PARLIST = 'NONE'                                              185
         CALL AXMATERIAL                                                  185
C    *                                                                    185
C    Geant ATTRIBUTE : PLPS ===> SEEN=1 COLO=2 <===                       185
         CALL AGSRESET                                                    186
         AG_SEEN = 1                                                      186
         AG_COLO = 2                                                      186
         AG_TITLE = 'PLPS'                                                186
         AG_EXNAME = 'ATTRIBUTE'                                          186
         AG_PARLIST = 'SEEN_COLO'                                         186
         CALL AXATTRIBUTE                                                 186
C    *                                                                    186
C    Geant SHAPE : BOX ===> DX=PXLD_LADDERWIDTH/2.0 DY=PXLD_PASSIVETHK/2  186
C    .0, DZ=PXLD_TOTALLENGTH/2.0 <===                                     186
         CALL AGSRESET                                                    188
         AG_DX = PXLD_LADDERWIDTH/2.0                                     188
         AG_DY = PXLD_PASSIVETHK/2.0                                      188
         AG_DZ = PXLD_TOTALLENGTH/2.0                                     188
         AG_TITLE = 'BOX'                                                 188
         AG_EXNAME = 'SHAPE'                                              188
         AG_PARLIST = 'DX_DY_DZ'                                          188
         CALL AXSHAPE                                                     188
C    *                                                                    188
5131  CALL AGSPOPB                                                        189
      GO TO BLOCK_PLPS, (5130 ,5140)                                      189
C End of Local Procedure                                                  189
      WRITE(LOUT,5150)'===>>  [End AgML Module PIXLGEO3]  <<==='          189
5150  FORMAT(A32,/,/)                                                     189
C MODULE PIXLGEO3                                                         191
      END                                                                 191
