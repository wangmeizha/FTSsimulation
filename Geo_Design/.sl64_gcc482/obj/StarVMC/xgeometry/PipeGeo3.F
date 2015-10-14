C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIPEGEO3                                                 2
C PIPEGEO3 COMPLETE BEAM PIPE IN Y2013 AND BEYOND. USING THREE SEPARATED  2
C , SECTIONS. MIDDLE SECTION AL-BE-AL                                     2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'PIPEGEO3'/, BANK_TITLE/'*'/                            2
      DATA MCOMMENT/'COMPLETE BEAM PIPE IN Y2013 AND BEYOND. USING THREE  2
     * SEPARATED, SECTIONS. MIDDLE SECTION AL-BE-AL'/                     2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C AUTHOR AMILKAR QUINTERO AND FV                                          2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 AUTHOR /'AMILKAR QUINTERO AND FV'/                     3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C CREATED 4/APR/2013                                                      3
      CHARACTER*40 CREATED /'4/APR/2013'/                                 4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C - Advanced Geant interface                                              5
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      5
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       5
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         5
     *AG_OPTION,AG_ORT,AG_MARK                                            5
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         5
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     5
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     5
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    5
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          5
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   5
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     5
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        5
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   5
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 5
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     5
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       5
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        5
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       5
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       5
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       5
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         5
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     5
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       5
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   5
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       5
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     5
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          5
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         5
     *-696969,AG_LDETU=250)                                               5
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    5
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  5
     *AG_IMFLD,AG_ISIMU                                                   5
C Inherited variables a: saved during internal calls                      5
C b: reset at the entry in a block and then saved                         5
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        5
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       5
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         5
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     5
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       5
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         5
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           5
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        5
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        5
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            5
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           5
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    5
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      5
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           5
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          5
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                5
C local variables valid inside same block                                 5
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       5
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  5
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         5
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    5
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          5
     *AG_OPTION                                                           5
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        5
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             5
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              5
C temporarly                                                              5
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           5
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    5
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       5
     *AG_HY)                                                              5
      REAL MIL_P,INCH_P                                                   5
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
      INTEGER BLOCK_PIPE                                                  5
      INTEGER BLOCK_PIHE                                                  38
      INTEGER BLOCK_PALQ                                                  38
      INTEGER BLOCK_PALR                                                  38
      INTEGER BLOCK_PIPI                                                  38
      INTEGER BLOCK_PIHI                                                  38
      INTEGER BLOCK_PALS                                                  38
      INTEGER BLOCK_PBES                                                  38
      INTEGER BLOCK_PALI                                                  38
      INTEGER BLOCK_SSCF                                                  38
      INTEGER BLOCK_SSCG                                                  38
      INTEGER BLOCK_PIPW                                                  38
      INTEGER BLOCK_PIHW                                                  38
      INTEGER BLOCK_PALJ                                                  38
      INTEGER BLOCK_PALK                                                  38
      INTEGER BLOCK_PIPA                                                  38
      INTEGER BLOCK_PIPB                                                  38
      INTEGER BLOCK_PVPB                                                  38
      INTEGER BLOCK_PIPC                                                  38
      INTEGER BLOCK_PVPC                                                  38
      INTEGER BLOCK_PIWA                                                  38
      INTEGER BLOCK_PIWB                                                  38
      INTEGER BLOCK_PVWB                                                  38
      INTEGER BLOCK_PIWC                                                  38
      INTEGER BLOCK_PVWC                                                  38
      INTEGER BLOCK_PRIS                                                  38
      INTEGER BLOCK_PRID                                                  38
      INTEGER BLOCK_PRIB                                                  38
      INTEGER BLOCK_PRWS                                                  38
      INTEGER BLOCK_PRWD                                                  38
      INTEGER BLOCK_PRWB                                                  38
      INTEGER BLOCK_PFLT                                                  38
      INTEGER BLOCK_PFLO                                                  38
      REAL INCH/2.54/                                                     38
      REAL Z1,Z2,Z3,Z4, R2, Z1W,Z2W,Z3W,Z4W,R2W                           40
C -                                                                       42
      REAL PIPV_VERSION                                                   42
      EQUIVALENCE (BANK_PIPV(3),PIPV_VERSION,RE_PIPV_VERSION),            42
     *(NAMES_PIPV(1),NPIPV_VERSION)                                       42
      EQUIVALENCE (COMMEN_PIPV(1),CPIPV_VERSION),(MAP_PIPV(1,1),          42
     *MPIPV_VERSION)                                                      42
      CHARACTER CPIPV_VERSION*40/' '/                                     42
      CHARACTER NPIPV_VERSION*12/'R.VERSION'/                             42
      INTEGER MPIPV_VERSION(2)/1,1/                                       42
      REAL RE_PIPV_VERSION                                                42
C -                                                                       42
      INTEGER PIPV_PIPECONFIG                                             42
      EQUIVALENCE (BANK_PIPV(4),PIPV_PIPECONFIG,RE_PIPV_PIPECONFIG),      42
     *(NAMES_PIPV(2),NPIPV_PIPECONFIG)                                    42
      EQUIVALENCE (COMMEN_PIPV(2),CPIPV_PIPECONFIG),(MAP_PIPV(1,2),       42
     *MPIPV_PIPECONFIG)                                                   42
      CHARACTER CPIPV_PIPECONFIG*40/' '/                                  42
      CHARACTER NPIPV_PIPECONFIG*12/'I.PIPECONFIG'/                       42
      INTEGER MPIPV_PIPECONFIG(2)/1,1/                                    42
      REAL RE_PIPV_PIPECONFIG                                             42
C -                                                                       42
      INTEGER PIPV_PIPEFLAG                                               42
      EQUIVALENCE (BANK_PIPV(5),PIPV_PIPEFLAG,RE_PIPV_PIPEFLAG),          42
     *(NAMES_PIPV(3),NPIPV_PIPEFLAG)                                      42
      EQUIVALENCE (COMMEN_PIPV(3),CPIPV_PIPEFLAG),(MAP_PIPV(1,3),         42
     *MPIPV_PIPEFLAG)                                                     42
      CHARACTER CPIPV_PIPEFLAG*40/' '/                                    42
      CHARACTER NPIPV_PIPEFLAG*12/'I.PIPEFLAG'/                           42
      INTEGER MPIPV_PIPEFLAG(2)/1,1/                                      42
      REAL RE_PIPV_PIPEFLAG                                               42
C -                                                                       42
      INTEGER LEN_PIPV,LLEN_PIPV,LINK_PIPV                                42
      PARAMETER (LEN_PIPV=5)                                              42
      PARAMETER (LLEN_PIPV=3)                                             42
      INTEGER FLAG_PIPV/0/,MAP_PIPV(2,LLEN_PIPV),IDNUM_PIPV(10)           42
      CHARACTER NAMES_PIPV(LLEN_PIPV)*12,COMMEN_PIPV(LLEN_PIPV)*40        42
      DATA IDNUM_PIPV/10*0/                                               42
      REAL BANK_PIPV(LEN_PIPV)                                            42
      CHARACTER*60 BPATH_PIPV/' '/                                        42
      SAVE FLAG_PIPV,MAP_PIPV,IDNUM_PIPV,NAMES_PIPV,COMMEN_PIPV,          42
     *BPATH_PIPV                                                          42
      DATA LINK_PIPV/0/,BANK_PIPV/LEN_PIPV*0/                             42
C *                                                                       42
C -                                                                       43
      REAL PIPG_VERSION                                                   43
      EQUIVALENCE (BANK_PIPG(3),PIPG_VERSION,RE_PIPG_VERSION),            43
     *(NAMES_PIPG(1),NPIPG_VERSION)                                       43
      EQUIVALENCE (COMMEN_PIPG(1),CPIPG_VERSION),(MAP_PIPG(1,1),          43
     *MPIPG_VERSION)                                                      43
      CHARACTER CPIPG_VERSION*40/' '/                                     43
      CHARACTER NPIPG_VERSION*12/'R.VERSION'/                             43
      INTEGER MPIPG_VERSION(2)/1,1/                                       43
      REAL RE_PIPG_VERSION                                                43
C -                                                                       43
      REAL PIPG_ZOFFSET                                                   43
      EQUIVALENCE (BANK_PIPG(4),PIPG_ZOFFSET,RE_PIPG_ZOFFSET),            43
     *(NAMES_PIPG(2),NPIPG_ZOFFSET)                                       43
      EQUIVALENCE (COMMEN_PIPG(2),CPIPG_ZOFFSET),(MAP_PIPG(1,2),          43
     *MPIPG_ZOFFSET)                                                      43
      CHARACTER CPIPG_ZOFFSET*40/' '/                                     43
      CHARACTER NPIPG_ZOFFSET*12/'R.ZOFFSET'/                             43
      INTEGER MPIPG_ZOFFSET(2)/1,1/                                       43
      REAL RE_PIPG_ZOFFSET                                                43
C -                                                                       43
      REAL PIPG_YOFFSET                                                   43
      EQUIVALENCE (BANK_PIPG(5),PIPG_YOFFSET,RE_PIPG_YOFFSET),            43
     *(NAMES_PIPG(3),NPIPG_YOFFSET)                                       43
      EQUIVALENCE (COMMEN_PIPG(3),CPIPG_YOFFSET),(MAP_PIPG(1,3),          43
     *MPIPG_YOFFSET)                                                      43
      CHARACTER CPIPG_YOFFSET*40/' '/                                     43
      CHARACTER NPIPG_YOFFSET*12/'R.YOFFSET'/                             43
      INTEGER MPIPG_YOFFSET(2)/1,1/                                       43
      REAL RE_PIPG_YOFFSET                                                43
C -                                                                       43
      REAL PIPG_XOFFSET                                                   43
      EQUIVALENCE (BANK_PIPG(6),PIPG_XOFFSET,RE_PIPG_XOFFSET),            43
     *(NAMES_PIPG(4),NPIPG_XOFFSET)                                       43
      EQUIVALENCE (COMMEN_PIPG(4),CPIPG_XOFFSET),(MAP_PIPG(1,4),          43
     *MPIPG_XOFFSET)                                                      43
      CHARACTER CPIPG_XOFFSET*40/' '/                                     43
      CHARACTER NPIPG_XOFFSET*12/'R.XOFFSET'/                             43
      INTEGER MPIPG_XOFFSET(2)/1,1/                                       43
      REAL RE_PIPG_XOFFSET                                                43
C -                                                                       43
      REAL PIPG_ZOFFBER                                                   43
      EQUIVALENCE (BANK_PIPG(7),PIPG_ZOFFBER,RE_PIPG_ZOFFBER),            43
     *(NAMES_PIPG(5),NPIPG_ZOFFBER)                                       43
      EQUIVALENCE (COMMEN_PIPG(5),CPIPG_ZOFFBER),(MAP_PIPG(1,5),          43
     *MPIPG_ZOFFBER)                                                      43
      CHARACTER CPIPG_ZOFFBER*40/' '/                                     43
      CHARACTER NPIPG_ZOFFBER*12/'R.ZOFFBER'/                             43
      INTEGER MPIPG_ZOFFBER(2)/1,1/                                       43
      REAL RE_PIPG_ZOFFBER                                                43
C -                                                                       43
      REAL PIPG_VACDENS                                                   43
      EQUIVALENCE (BANK_PIPG(8),PIPG_VACDENS,RE_PIPG_VACDENS),            43
     *(NAMES_PIPG(6),NPIPG_VACDENS)                                       43
      EQUIVALENCE (COMMEN_PIPG(6),CPIPG_VACDENS),(MAP_PIPG(1,6),          43
     *MPIPG_VACDENS)                                                      43
      CHARACTER CPIPG_VACDENS*40/' '/                                     43
      CHARACTER NPIPG_VACDENS*12/'R.VACDENS'/                             43
      INTEGER MPIPG_VACDENS(2)/1,1/                                       43
      REAL RE_PIPG_VACDENS                                                43
C -                                                                       43
      INTEGER LEN_PIPG,LLEN_PIPG,LINK_PIPG                                43
      PARAMETER (LEN_PIPG=8)                                              43
      PARAMETER (LLEN_PIPG=6)                                             43
      INTEGER FLAG_PIPG/0/,MAP_PIPG(2,LLEN_PIPG),IDNUM_PIPG(10)           43
      CHARACTER NAMES_PIPG(LLEN_PIPG)*12,COMMEN_PIPG(LLEN_PIPG)*40        43
      DATA IDNUM_PIPG/10*0/                                               43
      REAL BANK_PIPG(LEN_PIPG)                                            43
      CHARACTER*60 BPATH_PIPG/' '/                                        43
      SAVE FLAG_PIPG,MAP_PIPG,IDNUM_PIPG,NAMES_PIPG,COMMEN_PIPG,          43
     *BPATH_PIPG                                                          43
      DATA LINK_PIPG/0/,BANK_PIPG/LEN_PIPG*0/                             43
C *                                                                       43
C -                                                                       45
      REAL PIPA_CONFIG                                                    45
      EQUIVALENCE (BANK_PIPA(3),PIPA_CONFIG,RE_PIPA_CONFIG),              45
     *(NAMES_PIPA(1),NPIPA_CONFIG)                                        45
      EQUIVALENCE (COMMEN_PIPA(1),CPIPA_CONFIG),(MAP_PIPA(1,1),           45
     *MPIPA_CONFIG)                                                       45
      CHARACTER CPIPA_CONFIG*40/' '/                                      45
      CHARACTER NPIPA_CONFIG*12/'R.CONFIG'/                               45
      INTEGER MPIPA_CONFIG(2)/1,1/                                        45
      REAL RE_PIPA_CONFIG                                                 45
C -                                                                       45
      REAL PIPA_S2INNR                                                    45
      EQUIVALENCE (BANK_PIPA(4),PIPA_S2INNR,RE_PIPA_S2INNR),              45
     *(NAMES_PIPA(2),NPIPA_S2INNR)                                        45
      EQUIVALENCE (COMMEN_PIPA(2),CPIPA_S2INNR),(MAP_PIPA(1,2),           45
     *MPIPA_S2INNR)                                                       45
      CHARACTER CPIPA_S2INNR*40/' '/                                      45
      CHARACTER NPIPA_S2INNR*12/'R.S2INNR'/                               45
      INTEGER MPIPA_S2INNR(2)/1,1/                                        45
      REAL RE_PIPA_S2INNR                                                 45
C -                                                                       45
      REAL PIPA_S2OUTR                                                    45
      EQUIVALENCE (BANK_PIPA(5),PIPA_S2OUTR,RE_PIPA_S2OUTR),              45
     *(NAMES_PIPA(3),NPIPA_S2OUTR)                                        45
      EQUIVALENCE (COMMEN_PIPA(3),CPIPA_S2OUTR),(MAP_PIPA(1,3),           45
     *MPIPA_S2OUTR)                                                       45
      CHARACTER CPIPA_S2OUTR*40/' '/                                      45
      CHARACTER NPIPA_S2OUTR*12/'R.S2OUTR'/                               45
      INTEGER MPIPA_S2OUTR(2)/1,1/                                        45
      REAL RE_PIPA_S2OUTR                                                 45
C -                                                                       45
      REAL PIPA_S2LENG                                                    45
      EQUIVALENCE (BANK_PIPA(6),PIPA_S2LENG,RE_PIPA_S2LENG),              45
     *(NAMES_PIPA(4),NPIPA_S2LENG)                                        45
      EQUIVALENCE (COMMEN_PIPA(4),CPIPA_S2LENG),(MAP_PIPA(1,4),           45
     *MPIPA_S2LENG)                                                       45
      CHARACTER CPIPA_S2LENG*40/' '/                                      45
      CHARACTER NPIPA_S2LENG*12/'R.S2LENG'/                               45
      INTEGER MPIPA_S2LENG(2)/1,1/                                        45
      REAL RE_PIPA_S2LENG                                                 45
C -                                                                       45
      REAL PIPA_S3INNR                                                    45
      EQUIVALENCE (BANK_PIPA(7),PIPA_S3INNR,RE_PIPA_S3INNR),              45
     *(NAMES_PIPA(5),NPIPA_S3INNR)                                        45
      EQUIVALENCE (COMMEN_PIPA(5),CPIPA_S3INNR),(MAP_PIPA(1,5),           45
     *MPIPA_S3INNR)                                                       45
      CHARACTER CPIPA_S3INNR*40/' '/                                      45
      CHARACTER NPIPA_S3INNR*12/'R.S3INNR'/                               45
      INTEGER MPIPA_S3INNR(2)/1,1/                                        45
      REAL RE_PIPA_S3INNR                                                 45
C -                                                                       45
      REAL PIPA_S3OUTR                                                    45
      EQUIVALENCE (BANK_PIPA(8),PIPA_S3OUTR,RE_PIPA_S3OUTR),              45
     *(NAMES_PIPA(6),NPIPA_S3OUTR)                                        45
      EQUIVALENCE (COMMEN_PIPA(6),CPIPA_S3OUTR),(MAP_PIPA(1,6),           45
     *MPIPA_S3OUTR)                                                       45
      CHARACTER CPIPA_S3OUTR*40/' '/                                      45
      CHARACTER NPIPA_S3OUTR*12/'R.S3OUTR'/                               45
      INTEGER MPIPA_S3OUTR(2)/1,1/                                        45
      REAL RE_PIPA_S3OUTR                                                 45
C -                                                                       45
      REAL PIPA_S3LENG                                                    45
      EQUIVALENCE (BANK_PIPA(9),PIPA_S3LENG,RE_PIPA_S3LENG),              45
     *(NAMES_PIPA(7),NPIPA_S3LENG)                                        45
      EQUIVALENCE (COMMEN_PIPA(7),CPIPA_S3LENG),(MAP_PIPA(1,7),           45
     *MPIPA_S3LENG)                                                       45
      CHARACTER CPIPA_S3LENG*40/' '/                                      45
      CHARACTER NPIPA_S3LENG*12/'R.S3LENG'/                               45
      INTEGER MPIPA_S3LENG(2)/1,1/                                        45
      REAL RE_PIPA_S3LENG                                                 45
C -                                                                       45
      REAL PIPA_S4INNR                                                    45
      EQUIVALENCE (BANK_PIPA(10),PIPA_S4INNR,RE_PIPA_S4INNR),             45
     *(NAMES_PIPA(8),NPIPA_S4INNR)                                        45
      EQUIVALENCE (COMMEN_PIPA(8),CPIPA_S4INNR),(MAP_PIPA(1,8),           45
     *MPIPA_S4INNR)                                                       45
      CHARACTER CPIPA_S4INNR*40/' '/                                      45
      CHARACTER NPIPA_S4INNR*12/'R.S4INNR'/                               45
      INTEGER MPIPA_S4INNR(2)/1,1/                                        45
      REAL RE_PIPA_S4INNR                                                 45
C -                                                                       45
      REAL PIPA_S4OUTR                                                    45
      EQUIVALENCE (BANK_PIPA(11),PIPA_S4OUTR,RE_PIPA_S4OUTR),             45
     *(NAMES_PIPA(9),NPIPA_S4OUTR)                                        45
      EQUIVALENCE (COMMEN_PIPA(9),CPIPA_S4OUTR),(MAP_PIPA(1,9),           45
     *MPIPA_S4OUTR)                                                       45
      CHARACTER CPIPA_S4OUTR*40/' '/                                      45
      CHARACTER NPIPA_S4OUTR*12/'R.S4OUTR'/                               45
      INTEGER MPIPA_S4OUTR(2)/1,1/                                        45
      REAL RE_PIPA_S4OUTR                                                 45
C -                                                                       45
      REAL PIPA_S4LENG                                                    45
      EQUIVALENCE (BANK_PIPA(12),PIPA_S4LENG,RE_PIPA_S4LENG),             45
     *(NAMES_PIPA(10),NPIPA_S4LENG)                                       45
      EQUIVALENCE (COMMEN_PIPA(10),CPIPA_S4LENG),(MAP_PIPA(1,10),         45
     *MPIPA_S4LENG)                                                       45
      CHARACTER CPIPA_S4LENG*40/' '/                                      45
      CHARACTER NPIPA_S4LENG*12/'R.S4LENG'/                               45
      INTEGER MPIPA_S4LENG(2)/1,1/                                        45
      REAL RE_PIPA_S4LENG                                                 45
C -                                                                       45
      REAL PIPA_FLANGE1T                                                  45
      EQUIVALENCE (BANK_PIPA(13),PIPA_FLANGE1T,RE_PIPA_FLANGE1T),         45
     *(NAMES_PIPA(11),NPIPA_FLANGE1T)                                     45
      EQUIVALENCE (COMMEN_PIPA(11),CPIPA_FLANGE1T),(MAP_PIPA(1,11),       45
     *MPIPA_FLANGE1T)                                                     45
      CHARACTER CPIPA_FLANGE1T*40/' '/                                    45
      CHARACTER NPIPA_FLANGE1T*12/'R.FLANGE1T'/                           45
      INTEGER MPIPA_FLANGE1T(2)/1,1/                                      45
      REAL RE_PIPA_FLANGE1T                                               45
C -                                                                       45
      REAL PIPA_FLANGE1R                                                  45
      EQUIVALENCE (BANK_PIPA(14),PIPA_FLANGE1R,RE_PIPA_FLANGE1R),         45
     *(NAMES_PIPA(12),NPIPA_FLANGE1R)                                     45
      EQUIVALENCE (COMMEN_PIPA(12),CPIPA_FLANGE1R),(MAP_PIPA(1,12),       45
     *MPIPA_FLANGE1R)                                                     45
      CHARACTER CPIPA_FLANGE1R*40/' '/                                    45
      CHARACTER NPIPA_FLANGE1R*12/'R.FLANGE1R'/                           45
      INTEGER MPIPA_FLANGE1R(2)/1,1/                                      45
      REAL RE_PIPA_FLANGE1R                                               45
C -                                                                       45
      REAL PIPA_CONELEN                                                   45
      EQUIVALENCE (BANK_PIPA(15),PIPA_CONELEN,RE_PIPA_CONELEN),           45
     *(NAMES_PIPA(13),NPIPA_CONELEN)                                      45
      EQUIVALENCE (COMMEN_PIPA(13),CPIPA_CONELEN),(MAP_PIPA(1,13),        45
     *MPIPA_CONELEN)                                                      45
      CHARACTER CPIPA_CONELEN*40/' '/                                     45
      CHARACTER NPIPA_CONELEN*12/'R.CONELEN'/                             45
      INTEGER MPIPA_CONELEN(2)/1,1/                                       45
      REAL RE_PIPA_CONELEN                                                45
C -                                                                       45
      REAL PIPA_RIBNUM                                                    45
      EQUIVALENCE (BANK_PIPA(16),PIPA_RIBNUM,RE_PIPA_RIBNUM),             45
     *(NAMES_PIPA(14),NPIPA_RIBNUM)                                       45
      EQUIVALENCE (COMMEN_PIPA(14),CPIPA_RIBNUM),(MAP_PIPA(1,14),         45
     *MPIPA_RIBNUM)                                                       45
      CHARACTER CPIPA_RIBNUM*40/' '/                                      45
      CHARACTER NPIPA_RIBNUM*12/'R.RIBNUM'/                               45
      INTEGER MPIPA_RIBNUM(2)/1,1/                                        45
      REAL RE_PIPA_RIBNUM                                                 45
C -                                                                       45
      REAL PIPA_RIBSPA                                                    45
      EQUIVALENCE (BANK_PIPA(17),PIPA_RIBSPA,RE_PIPA_RIBSPA),             45
     *(NAMES_PIPA(15),NPIPA_RIBSPA)                                       45
      EQUIVALENCE (COMMEN_PIPA(15),CPIPA_RIBSPA),(MAP_PIPA(1,15),         45
     *MPIPA_RIBSPA)                                                       45
      CHARACTER CPIPA_RIBSPA*40/' '/                                      45
      CHARACTER NPIPA_RIBSPA*12/'R.RIBSPA'/                               45
      INTEGER MPIPA_RIBSPA(2)/1,1/                                        45
      REAL RE_PIPA_RIBSPA                                                 45
C -                                                                       45
      REAL PIPA_RIBTHK                                                    45
      EQUIVALENCE (BANK_PIPA(18),PIPA_RIBTHK,RE_PIPA_RIBTHK),             45
     *(NAMES_PIPA(16),NPIPA_RIBTHK)                                       45
      EQUIVALENCE (COMMEN_PIPA(16),CPIPA_RIBTHK),(MAP_PIPA(1,16),         45
     *MPIPA_RIBTHK)                                                       45
      CHARACTER CPIPA_RIBTHK*40/' '/                                      45
      CHARACTER NPIPA_RIBTHK*12/'R.RIBTHK'/                               45
      INTEGER MPIPA_RIBTHK(2)/1,1/                                        45
      REAL RE_PIPA_RIBTHK                                                 45
C -                                                                       45
      REAL PIPA_RIBOUTR                                                   45
      EQUIVALENCE (BANK_PIPA(19),PIPA_RIBOUTR,RE_PIPA_RIBOUTR),           45
     *(NAMES_PIPA(17),NPIPA_RIBOUTR)                                      45
      EQUIVALENCE (COMMEN_PIPA(17),CPIPA_RIBOUTR),(MAP_PIPA(1,17),        45
     *MPIPA_RIBOUTR)                                                      45
      CHARACTER CPIPA_RIBOUTR*40/' '/                                     45
      CHARACTER NPIPA_RIBOUTR*12/'R.RIBOUTR'/                             45
      INTEGER MPIPA_RIBOUTR(2)/1,1/                                       45
      REAL RE_PIPA_RIBOUTR                                                45
C -                                                                       45
      REAL PIPA_RIBCENT                                                   45
      EQUIVALENCE (BANK_PIPA(20),PIPA_RIBCENT,RE_PIPA_RIBCENT),           45
     *(NAMES_PIPA(18),NPIPA_RIBCENT)                                      45
      EQUIVALENCE (COMMEN_PIPA(18),CPIPA_RIBCENT),(MAP_PIPA(1,18),        45
     *MPIPA_RIBCENT)                                                      45
      CHARACTER CPIPA_RIBCENT*40/' '/                                     45
      CHARACTER NPIPA_RIBCENT*12/'R.RIBCENT'/                             45
      INTEGER MPIPA_RIBCENT(2)/1,1/                                       45
      REAL RE_PIPA_RIBCENT                                                45
C -                                                                       45
      INTEGER LEN_PIPA,LLEN_PIPA,LINK_PIPA                                45
      PARAMETER (LEN_PIPA=20)                                             45
      PARAMETER (LLEN_PIPA=18)                                            45
      INTEGER FLAG_PIPA/0/,MAP_PIPA(2,LLEN_PIPA),IDNUM_PIPA(10)           45
      CHARACTER NAMES_PIPA(LLEN_PIPA)*12,COMMEN_PIPA(LLEN_PIPA)*40        45
      DATA IDNUM_PIPA/10*0/                                               45
      REAL BANK_PIPA(LEN_PIPA)                                            45
      CHARACTER*60 BPATH_PIPA/' '/                                        45
      SAVE FLAG_PIPA,MAP_PIPA,IDNUM_PIPA,NAMES_PIPA,COMMEN_PIPA,          45
     *BPATH_PIPA                                                          45
      DATA LINK_PIPA/0/,BANK_PIPA/LEN_PIPA*0/                             45
C *                                                                       45
C -                                                                       47
      REAL PIWA_CONFIG                                                    47
      EQUIVALENCE (BANK_PIWA(3),PIWA_CONFIG,RE_PIWA_CONFIG),              47
     *(NAMES_PIWA(1),NPIWA_CONFIG)                                        47
      EQUIVALENCE (COMMEN_PIWA(1),CPIWA_CONFIG),(MAP_PIWA(1,1),           47
     *MPIWA_CONFIG)                                                       47
      CHARACTER CPIWA_CONFIG*40/' '/                                      47
      CHARACTER NPIWA_CONFIG*12/'R.CONFIG'/                               47
      INTEGER MPIWA_CONFIG(2)/1,1/                                        47
      REAL RE_PIWA_CONFIG                                                 47
C -                                                                       47
      REAL PIWA_S2INNR                                                    47
      EQUIVALENCE (BANK_PIWA(4),PIWA_S2INNR,RE_PIWA_S2INNR),              47
     *(NAMES_PIWA(2),NPIWA_S2INNR)                                        47
      EQUIVALENCE (COMMEN_PIWA(2),CPIWA_S2INNR),(MAP_PIWA(1,2),           47
     *MPIWA_S2INNR)                                                       47
      CHARACTER CPIWA_S2INNR*40/' '/                                      47
      CHARACTER NPIWA_S2INNR*12/'R.S2INNR'/                               47
      INTEGER MPIWA_S2INNR(2)/1,1/                                        47
      REAL RE_PIWA_S2INNR                                                 47
C -                                                                       47
      REAL PIWA_S2OUTR                                                    47
      EQUIVALENCE (BANK_PIWA(5),PIWA_S2OUTR,RE_PIWA_S2OUTR),              47
     *(NAMES_PIWA(3),NPIWA_S2OUTR)                                        47
      EQUIVALENCE (COMMEN_PIWA(3),CPIWA_S2OUTR),(MAP_PIWA(1,3),           47
     *MPIWA_S2OUTR)                                                       47
      CHARACTER CPIWA_S2OUTR*40/' '/                                      47
      CHARACTER NPIWA_S2OUTR*12/'R.S2OUTR'/                               47
      INTEGER MPIWA_S2OUTR(2)/1,1/                                        47
      REAL RE_PIWA_S2OUTR                                                 47
C -                                                                       47
      REAL PIWA_S2LENG                                                    47
      EQUIVALENCE (BANK_PIWA(6),PIWA_S2LENG,RE_PIWA_S2LENG),              47
     *(NAMES_PIWA(4),NPIWA_S2LENG)                                        47
      EQUIVALENCE (COMMEN_PIWA(4),CPIWA_S2LENG),(MAP_PIWA(1,4),           47
     *MPIWA_S2LENG)                                                       47
      CHARACTER CPIWA_S2LENG*40/' '/                                      47
      CHARACTER NPIWA_S2LENG*12/'R.S2LENG'/                               47
      INTEGER MPIWA_S2LENG(2)/1,1/                                        47
      REAL RE_PIWA_S2LENG                                                 47
C -                                                                       47
      REAL PIWA_S3INNR                                                    47
      EQUIVALENCE (BANK_PIWA(7),PIWA_S3INNR,RE_PIWA_S3INNR),              47
     *(NAMES_PIWA(5),NPIWA_S3INNR)                                        47
      EQUIVALENCE (COMMEN_PIWA(5),CPIWA_S3INNR),(MAP_PIWA(1,5),           47
     *MPIWA_S3INNR)                                                       47
      CHARACTER CPIWA_S3INNR*40/' '/                                      47
      CHARACTER NPIWA_S3INNR*12/'R.S3INNR'/                               47
      INTEGER MPIWA_S3INNR(2)/1,1/                                        47
      REAL RE_PIWA_S3INNR                                                 47
C -                                                                       47
      REAL PIWA_S3OUTR                                                    47
      EQUIVALENCE (BANK_PIWA(8),PIWA_S3OUTR,RE_PIWA_S3OUTR),              47
     *(NAMES_PIWA(6),NPIWA_S3OUTR)                                        47
      EQUIVALENCE (COMMEN_PIWA(6),CPIWA_S3OUTR),(MAP_PIWA(1,6),           47
     *MPIWA_S3OUTR)                                                       47
      CHARACTER CPIWA_S3OUTR*40/' '/                                      47
      CHARACTER NPIWA_S3OUTR*12/'R.S3OUTR'/                               47
      INTEGER MPIWA_S3OUTR(2)/1,1/                                        47
      REAL RE_PIWA_S3OUTR                                                 47
C -                                                                       47
      REAL PIWA_S3LENG                                                    47
      EQUIVALENCE (BANK_PIWA(9),PIWA_S3LENG,RE_PIWA_S3LENG),              47
     *(NAMES_PIWA(7),NPIWA_S3LENG)                                        47
      EQUIVALENCE (COMMEN_PIWA(7),CPIWA_S3LENG),(MAP_PIWA(1,7),           47
     *MPIWA_S3LENG)                                                       47
      CHARACTER CPIWA_S3LENG*40/' '/                                      47
      CHARACTER NPIWA_S3LENG*12/'R.S3LENG'/                               47
      INTEGER MPIWA_S3LENG(2)/1,1/                                        47
      REAL RE_PIWA_S3LENG                                                 47
C -                                                                       47
      REAL PIWA_S4INNR                                                    47
      EQUIVALENCE (BANK_PIWA(10),PIWA_S4INNR,RE_PIWA_S4INNR),             47
     *(NAMES_PIWA(8),NPIWA_S4INNR)                                        47
      EQUIVALENCE (COMMEN_PIWA(8),CPIWA_S4INNR),(MAP_PIWA(1,8),           47
     *MPIWA_S4INNR)                                                       47
      CHARACTER CPIWA_S4INNR*40/' '/                                      47
      CHARACTER NPIWA_S4INNR*12/'R.S4INNR'/                               47
      INTEGER MPIWA_S4INNR(2)/1,1/                                        47
      REAL RE_PIWA_S4INNR                                                 47
C -                                                                       47
      REAL PIWA_S4OUTR                                                    47
      EQUIVALENCE (BANK_PIWA(11),PIWA_S4OUTR,RE_PIWA_S4OUTR),             47
     *(NAMES_PIWA(9),NPIWA_S4OUTR)                                        47
      EQUIVALENCE (COMMEN_PIWA(9),CPIWA_S4OUTR),(MAP_PIWA(1,9),           47
     *MPIWA_S4OUTR)                                                       47
      CHARACTER CPIWA_S4OUTR*40/' '/                                      47
      CHARACTER NPIWA_S4OUTR*12/'R.S4OUTR'/                               47
      INTEGER MPIWA_S4OUTR(2)/1,1/                                        47
      REAL RE_PIWA_S4OUTR                                                 47
C -                                                                       47
      REAL PIWA_S4LENG                                                    47
      EQUIVALENCE (BANK_PIWA(12),PIWA_S4LENG,RE_PIWA_S4LENG),             47
     *(NAMES_PIWA(10),NPIWA_S4LENG)                                       47
      EQUIVALENCE (COMMEN_PIWA(10),CPIWA_S4LENG),(MAP_PIWA(1,10),         47
     *MPIWA_S4LENG)                                                       47
      CHARACTER CPIWA_S4LENG*40/' '/                                      47
      CHARACTER NPIWA_S4LENG*12/'R.S4LENG'/                               47
      INTEGER MPIWA_S4LENG(2)/1,1/                                        47
      REAL RE_PIWA_S4LENG                                                 47
C -                                                                       47
      REAL PIWA_FLANGE1T                                                  47
      EQUIVALENCE (BANK_PIWA(13),PIWA_FLANGE1T,RE_PIWA_FLANGE1T),         47
     *(NAMES_PIWA(11),NPIWA_FLANGE1T)                                     47
      EQUIVALENCE (COMMEN_PIWA(11),CPIWA_FLANGE1T),(MAP_PIWA(1,11),       47
     *MPIWA_FLANGE1T)                                                     47
      CHARACTER CPIWA_FLANGE1T*40/' '/                                    47
      CHARACTER NPIWA_FLANGE1T*12/'R.FLANGE1T'/                           47
      INTEGER MPIWA_FLANGE1T(2)/1,1/                                      47
      REAL RE_PIWA_FLANGE1T                                               47
C -                                                                       47
      REAL PIWA_FLANGE1R                                                  47
      EQUIVALENCE (BANK_PIWA(14),PIWA_FLANGE1R,RE_PIWA_FLANGE1R),         47
     *(NAMES_PIWA(12),NPIWA_FLANGE1R)                                     47
      EQUIVALENCE (COMMEN_PIWA(12),CPIWA_FLANGE1R),(MAP_PIWA(1,12),       47
     *MPIWA_FLANGE1R)                                                     47
      CHARACTER CPIWA_FLANGE1R*40/' '/                                    47
      CHARACTER NPIWA_FLANGE1R*12/'R.FLANGE1R'/                           47
      INTEGER MPIWA_FLANGE1R(2)/1,1/                                      47
      REAL RE_PIWA_FLANGE1R                                               47
C -                                                                       47
      REAL PIWA_CONELEN                                                   47
      EQUIVALENCE (BANK_PIWA(15),PIWA_CONELEN,RE_PIWA_CONELEN),           47
     *(NAMES_PIWA(13),NPIWA_CONELEN)                                      47
      EQUIVALENCE (COMMEN_PIWA(13),CPIWA_CONELEN),(MAP_PIWA(1,13),        47
     *MPIWA_CONELEN)                                                      47
      CHARACTER CPIWA_CONELEN*40/' '/                                     47
      CHARACTER NPIWA_CONELEN*12/'R.CONELEN'/                             47
      INTEGER MPIWA_CONELEN(2)/1,1/                                       47
      REAL RE_PIWA_CONELEN                                                47
C -                                                                       47
      REAL PIWA_RIBNUM                                                    47
      EQUIVALENCE (BANK_PIWA(16),PIWA_RIBNUM,RE_PIWA_RIBNUM),             47
     *(NAMES_PIWA(14),NPIWA_RIBNUM)                                       47
      EQUIVALENCE (COMMEN_PIWA(14),CPIWA_RIBNUM),(MAP_PIWA(1,14),         47
     *MPIWA_RIBNUM)                                                       47
      CHARACTER CPIWA_RIBNUM*40/' '/                                      47
      CHARACTER NPIWA_RIBNUM*12/'R.RIBNUM'/                               47
      INTEGER MPIWA_RIBNUM(2)/1,1/                                        47
      REAL RE_PIWA_RIBNUM                                                 47
C -                                                                       47
      REAL PIWA_RIBSPA                                                    47
      EQUIVALENCE (BANK_PIWA(17),PIWA_RIBSPA,RE_PIWA_RIBSPA),             47
     *(NAMES_PIWA(15),NPIWA_RIBSPA)                                       47
      EQUIVALENCE (COMMEN_PIWA(15),CPIWA_RIBSPA),(MAP_PIWA(1,15),         47
     *MPIWA_RIBSPA)                                                       47
      CHARACTER CPIWA_RIBSPA*40/' '/                                      47
      CHARACTER NPIWA_RIBSPA*12/'R.RIBSPA'/                               47
      INTEGER MPIWA_RIBSPA(2)/1,1/                                        47
      REAL RE_PIWA_RIBSPA                                                 47
C -                                                                       47
      REAL PIWA_RIBTHK                                                    47
      EQUIVALENCE (BANK_PIWA(18),PIWA_RIBTHK,RE_PIWA_RIBTHK),             47
     *(NAMES_PIWA(16),NPIWA_RIBTHK)                                       47
      EQUIVALENCE (COMMEN_PIWA(16),CPIWA_RIBTHK),(MAP_PIWA(1,16),         47
     *MPIWA_RIBTHK)                                                       47
      CHARACTER CPIWA_RIBTHK*40/' '/                                      47
      CHARACTER NPIWA_RIBTHK*12/'R.RIBTHK'/                               47
      INTEGER MPIWA_RIBTHK(2)/1,1/                                        47
      REAL RE_PIWA_RIBTHK                                                 47
C -                                                                       47
      REAL PIWA_RIBOUTR                                                   47
      EQUIVALENCE (BANK_PIWA(19),PIWA_RIBOUTR,RE_PIWA_RIBOUTR),           47
     *(NAMES_PIWA(17),NPIWA_RIBOUTR)                                      47
      EQUIVALENCE (COMMEN_PIWA(17),CPIWA_RIBOUTR),(MAP_PIWA(1,17),        47
     *MPIWA_RIBOUTR)                                                      47
      CHARACTER CPIWA_RIBOUTR*40/' '/                                     47
      CHARACTER NPIWA_RIBOUTR*12/'R.RIBOUTR'/                             47
      INTEGER MPIWA_RIBOUTR(2)/1,1/                                       47
      REAL RE_PIWA_RIBOUTR                                                47
C -                                                                       47
      REAL PIWA_RIBCENT                                                   47
      EQUIVALENCE (BANK_PIWA(20),PIWA_RIBCENT,RE_PIWA_RIBCENT),           47
     *(NAMES_PIWA(18),NPIWA_RIBCENT)                                      47
      EQUIVALENCE (COMMEN_PIWA(18),CPIWA_RIBCENT),(MAP_PIWA(1,18),        47
     *MPIWA_RIBCENT)                                                      47
      CHARACTER CPIWA_RIBCENT*40/' '/                                     47
      CHARACTER NPIWA_RIBCENT*12/'R.RIBCENT'/                             47
      INTEGER MPIWA_RIBCENT(2)/1,1/                                       47
      REAL RE_PIWA_RIBCENT                                                47
C -                                                                       47
      INTEGER LEN_PIWA,LLEN_PIWA,LINK_PIWA                                47
      PARAMETER (LEN_PIWA=20)                                             47
      PARAMETER (LLEN_PIWA=18)                                            47
      INTEGER FLAG_PIWA/0/,MAP_PIWA(2,LLEN_PIWA),IDNUM_PIWA(10)           47
      CHARACTER NAMES_PIWA(LLEN_PIWA)*12,COMMEN_PIWA(LLEN_PIWA)*40        47
      DATA IDNUM_PIWA/10*0/                                               47
      REAL BANK_PIWA(LEN_PIWA)                                            47
      CHARACTER*60 BPATH_PIWA/' '/                                        47
      SAVE FLAG_PIWA,MAP_PIWA,IDNUM_PIWA,NAMES_PIWA,COMMEN_PIWA,          47
     *BPATH_PIWA                                                          47
      DATA LINK_PIWA/0/,BANK_PIWA/LEN_PIWA*0/                             47
C *                                                                       47
C *                                                                       48
      CALL AGSBEGM(MODULE,IPRIN)                                          48
      IF (IPRIN.LT.0) RETURN                                              48
C Filling bank PIPV - PIPE VERSION                                        48
      BANK_TITLE='PIPE VERSION'                                           48
      BPATH_PIPV='PIPV*'                                                  48
      IDNUM_PIPV(1)=0                                                     49
C 3.0 VERSION OF THE BEAM PIPE                                            49
      PIPV_VERSION=3.0                                                    49
      CPIPV_VERSION='3.0 VERSION OF THE BEAM PIPE'                        49
C CONFIGURATION IS THREE                                                  50
      PIPV_PIPECONFIG=3                                                   50
      CPIPV_PIPECONFIG='CONFIGURATION IS THREE'                           50
C CONFIGURATION IS THREE                                                  51
      PIPV_PIPEFLAG=1                                                     51
      CPIPV_PIPEFLAG='CONFIGURATION IS THREE'                             51
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    52
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           52
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   52
C *                                                                       52
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    52
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           52
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   52
C *                                                                       53
      CALL AGSBEGM(MODULE,IPRIN)                                          53
      IF (IPRIN.LT.0) RETURN                                              53
C Filling bank PIPG - PIPE GEOMETRY                                       53
      BANK_TITLE='PIPE GEOMETRY'                                          53
      BPATH_PIPG='PIPG*'                                                  53
      IDNUM_PIPG(1)=0                                                     54
C DEFAULT POSITION AND GEOMETRY                                           54
      PIPG_VERSION=3.0                                                    54
      CPIPG_VERSION='DEFAULT POSITION AND GEOMETRY'                       54
C VACUUM DENSITY... NEEDS TO BE VERIFIED                                  55
      PIPG_VACDENS=1.25E-8                                                55
      CPIPG_VACDENS='VACUUM DENSITY... NEEDS TO BE VERIFIED'              55
C DEFAULT X POSITION                                                      56
      PIPG_XOFFSET=0.0                                                    56
      CPIPG_XOFFSET='DEFAULT X POSITION'                                  56
C DEFAULT Y POSITION                                                      57
      PIPG_YOFFSET=0.0                                                    57
      CPIPG_YOFFSET='DEFAULT Y POSITION'                                  57
C DEFAULT Z POSITION                                                      58
      PIPG_ZOFFSET=0.0                                                    58
      CPIPG_ZOFFSET='DEFAULT Z POSITION'                                  58
C OFFSET OF BERYLLIUM AS PER DRAWING                                      59
      PIPG_ZOFFBER=7.875*INCH                                             59
      CPIPG_ZOFFBER='OFFSET OF BERYLLIUM AS PER DRAWING'                  59
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    60
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           60
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   60
C *                                                                       60
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    60
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           60
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   60
C *                                                                       61
      CALL AGSBEGM(MODULE,IPRIN)                                          61
      IF (IPRIN.LT.0) RETURN                                              61
C Filling bank PIPA - BEAM PIPE DATA                                      61
      BANK_TITLE='BEAM PIPE DATA'                                         61
      BPATH_PIPA='PIPA*'                                                  61
      IDNUM_PIPA(1)=0                                                     62
C BOTH MATERIAL AND GEO PARAMS                                            62
      PIPA_CONFIG=1                                                       62
      CPIPA_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         62
C SECOND ALUMINUM SECTION INNER RADIUS                                    63
      PIPA_S2INNR=3.81                                                    63
      CPIPA_S2INNR='SECOND ALUMINUM SECTION INNER RADIUS'                 63
C SECOND ALUMINUM SECTION OUTER RADIUS                                    64
      PIPA_S2OUTR=3.937                                                   64
      CPIPA_S2OUTR='SECOND ALUMINUM SECTION OUTER RADIUS'                 64
C SECOND ALUMINUM SECTION HALF LENGTH                                     65
      PIPA_S2LENG=31.42/2.0                                               65
      CPIPA_S2LENG='SECOND ALUMINUM SECTION HALF LENGTH'                  65
C TRANSITION STUB ALUMINUM SECTION INNER RADIUS                           66
      PIPA_S3INNR=3.81                                                    66
      CPIPA_S3INNR='TRANSITION STUB ALUMINUM SECTION INNER RADIUS'        66
C TRANSITION STUB ALUMINUM SECTION OUTER RADIUS                           67
      PIPA_S3OUTR=3.937                                                   67
      CPIPA_S3OUTR='TRANSITION STUB ALUMINUM SECTION OUTER RADIUS'        67
C TRANSITION STUB ALUMINUM SECTION HALF LENGTH                            68
      PIPA_S3LENG=1.0                                                     68
      CPIPA_S3LENG='TRANSITION STUB ALUMINUM SECTION HALF LENGTH'         68
C LARGE OD ALUMINUM SECTION INNER RADIUS                                  69
      PIPA_S4INNR=6.20                                                    69
      CPIPA_S4INNR='LARGE OD ALUMINUM SECTION INNER RADIUS'               69
C LARGE OD ALUMINUM SECTION OUTER RADIUS                                  70
      PIPA_S4OUTR=6.35                                                    70
      CPIPA_S4OUTR='LARGE OD ALUMINUM SECTION OUTER RADIUS'               70
C LARGE OD ALUMINUM SECTION HALF LENGTH                                   71
      PIPA_S4LENG=101.0-12.5                                              71
      CPIPA_S4LENG='LARGE OD ALUMINUM SECTION HALF LENGTH'                71
C LENGTH OF THE BELL REDUCER CONE                                         72
      PIPA_CONELEN=25.0                                                   72
      CPIPA_CONELEN='LENGTH OF THE BELL REDUCER CONE'                     72
C FLANGE SET HALF THICKNESS                                               73
      PIPA_FLANGE1T=2.0                                                   73
      CPIPA_FLANGE1T='FLANGE SET HALF THICKNESS'                          73
C FLANGE OUTER RADIUS                                                     74
      PIPA_FLANGE1R=5.85                                                  74
      CPIPA_FLANGE1R='FLANGE OUTER RADIUS'                                74
C NUMBER OF RIBS                                                          75
      PIPA_RIBNUM=8                                                       75
      CPIPA_RIBNUM='NUMBER OF RIBS'                                       75
C SPACING BETWEEN RIBS                                                    76
      PIPA_RIBSPA=1.75                                                    76
      CPIPA_RIBSPA='SPACING BETWEEN RIBS'                                 76
C RIB HALF THICKNESS                                                      77
      PIPA_RIBTHK=0.05                                                    77
      CPIPA_RIBTHK='RIB HALF THICKNESS'                                   77
C RIB OUTER RADIUS                                                        78
      PIPA_RIBOUTR=4.8                                                    78
      CPIPA_RIBOUTR='RIB OUTER RADIUS'                                    78
C RIB SET CENTER                                                          79
      PIPA_RIBCENT=495.0                                                  79
      CPIPA_RIBCENT='RIB SET CENTER'                                      79
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPA',    80
     *BPATH_PIPA,IDNUM_PIPA,1, MAP_PIPA,NAMES_PIPA,COMMEN_PIPA,           80
     *BANK_PIPA,LEN_PIPA,LLEN_PIPA,LINK_PIPA,FLAG_PIPA)                   80
C *                                                                       80
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPA',    80
     *BPATH_PIPA,IDNUM_PIPA,1, MAP_PIPA,NAMES_PIPA,COMMEN_PIPA,           80
     *BANK_PIPA,LEN_PIPA,LLEN_PIPA,LINK_PIPA,FLAG_PIPA)                   80
C *                                                                       81
      CALL AGSBEGM(MODULE,IPRIN)                                          81
      IF (IPRIN.LT.0) RETURN                                              81
C Filling bank PIWA - BEAM PIPE DATA                                      81
      BANK_TITLE='BEAM PIPE DATA'                                         81
      BPATH_PIWA='PIWA*'                                                  81
      IDNUM_PIWA(1)=0                                                     82
C BOTH MATERIAL AND GEO PARAMS                                            82
      PIWA_CONFIG=1                                                       82
      CPIWA_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         82
C SECOND ALUMINUM SECTION INNER RADIUS                                    83
      PIWA_S2INNR=3.81                                                    83
      CPIWA_S2INNR='SECOND ALUMINUM SECTION INNER RADIUS'                 83
C SECOND ALUMINUM SECTION OUTER RADIUS                                    84
      PIWA_S2OUTR=3.937                                                   84
      CPIWA_S2OUTR='SECOND ALUMINUM SECTION OUTER RADIUS'                 84
C SECOND ALUMINUM SECTION HALF LENGTH                                     85
      PIWA_S2LENG=18.00                                                   85
      CPIWA_S2LENG='SECOND ALUMINUM SECTION HALF LENGTH'                  85
C TRANSITION STUB ALUMINUM SECTION INNER RADIUS                           86
      PIWA_S3INNR=3.81                                                    86
      CPIWA_S3INNR='TRANSITION STUB ALUMINUM SECTION INNER RADIUS'        86
C TRANSITION STUB ALUMINUM SECTION OUTER RADIUS                           87
      PIWA_S3OUTR=3.937                                                   87
      CPIWA_S3OUTR='TRANSITION STUB ALUMINUM SECTION OUTER RADIUS'        87
C TRANSITION STUB ALUMINUM SECTION HALF LENGTH                            88
      PIWA_S3LENG=1.0                                                     88
      CPIWA_S3LENG='TRANSITION STUB ALUMINUM SECTION HALF LENGTH'         88
C LARGE OD ALUMINUM SECTION INNER RADIUS                                  89
      PIWA_S4INNR=6.20                                                    89
      CPIWA_S4INNR='LARGE OD ALUMINUM SECTION INNER RADIUS'               89
C LARGE OD ALUMINUM SECTION OUTER RADIUS                                  90
      PIWA_S4OUTR=6.35                                                    90
      CPIWA_S4OUTR='LARGE OD ALUMINUM SECTION OUTER RADIUS'               90
C LARGE OD ALUMINUM SECTION LENGTH                                        91
      PIWA_S4LENG=139.57*2                                                91
      CPIWA_S4LENG='LARGE OD ALUMINUM SECTION LENGTH'                     91
C LENGTH OF THE BELL REDUCER CONE                                         92
      PIWA_CONELEN=25.0                                                   92
      CPIWA_CONELEN='LENGTH OF THE BELL REDUCER CONE'                     92
C FLANGE SET HALF THICKNESS                                               93
      PIWA_FLANGE1T=2.0                                                   93
      CPIWA_FLANGE1T='FLANGE SET HALF THICKNESS'                          93
C FLANGE OUTER RADIUS                                                     94
      PIWA_FLANGE1R=5.85                                                  94
      CPIWA_FLANGE1R='FLANGE OUTER RADIUS'                                94
C NUMBER OF RIBS                                                          95
      PIWA_RIBNUM=8                                                       95
      CPIWA_RIBNUM='NUMBER OF RIBS'                                       95
C SPACING BETWEEN RIBS                                                    96
      PIWA_RIBSPA=1.75                                                    96
      CPIWA_RIBSPA='SPACING BETWEEN RIBS'                                 96
C RIB HALF THICKNESS                                                      97
      PIWA_RIBTHK=0.05                                                    97
      CPIWA_RIBTHK='RIB HALF THICKNESS'                                   97
C RIB OUTER RADIUS                                                        98
      PIWA_RIBOUTR=4.8                                                    98
      CPIWA_RIBOUTR='RIB OUTER RADIUS'                                    98
C RIB SET CENTER                                                          99
      PIWA_RIBCENT=454.5                                                  99
      CPIWA_RIBCENT='RIB SET CENTER'                                      99
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIWA',    100
     *BPATH_PIWA,IDNUM_PIWA,1, MAP_PIWA,NAMES_PIWA,COMMEN_PIWA,           100
     *BANK_PIWA,LEN_PIWA,LLEN_PIWA,LINK_PIWA,FLAG_PIWA)                   100
C *                                                                       100
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIWA',    100
     *BPATH_PIWA,IDNUM_PIWA,1, MAP_PIWA,NAMES_PIWA,COMMEN_PIWA,           100
     *BANK_PIWA,LEN_PIWA,LLEN_PIWA,LINK_PIWA,FLAG_PIWA)                   100
C Check IPRIN>=1                                                          100
      IF (IPRIN.GE.1) WRITE(LOUT,5010)PIPG_VERSION                        100
5010  FORMAT(' PIPE : PIPE_version = ',F5.3)                              102
C Geant COMPONENT : CR ===> A=51.9960 Z=24 W=0.19 <===                    103
      CALL AGSRESET                                                       103
      AG_A = 51.9960                                                      103
      AG_Z = 24                                                           103
      AG_W = 0.19                                                         103
      AG_TITLE = 'CR'                                                     103
      AG_EXNAME = 'COMPONENT'                                             103
      AG_PARLIST = 'A_Z_W'                                                103
      CALL AXCOMPONENT                                                    103
C *                                                                       103
C Geant COMPONENT : NI ===> A=58.6934 Z=28 W=0.09 <===                    103
      CALL AGSRESET                                                       104
      AG_A = 58.6934                                                      104
      AG_Z = 28                                                           104
      AG_W = 0.09                                                         104
      AG_TITLE = 'NI'                                                     104
      AG_EXNAME = 'COMPONENT'                                             104
      AG_PARLIST = 'A_Z_W'                                                104
      CALL AXCOMPONENT                                                    104
C *                                                                       104
C Geant COMPONENT : FE ===> A=55.8450 Z=26 W=0.72 <===                    104
      CALL AGSRESET                                                       105
      AG_A = 55.8450                                                      105
      AG_Z = 26                                                           105
      AG_W = 0.72                                                         105
      AG_TITLE = 'FE'                                                     105
      AG_EXNAME = 'COMPONENT'                                             105
      AG_PARLIST = 'A_Z_W'                                                105
      CALL AXCOMPONENT                                                    105
C *                                                                       105
C Geant MIXTURE : STAINLESSSTEEL ===> DENS=8.03 <===                      105
      CALL AGSRESET                                                       106
      AG_DENS = 8.03                                                      106
      AG_TITLE = 'STAINLESSSTEEL'                                         106
      AG_EXNAME = 'MIXTURE'                                               106
      AG_PARLIST = 'DENS'                                                 106
      CALL AXMIXTURE                                                      106
C *                                                                       106
      Z1 = -480.06                                                        106
      Z2 = Z1 - 2.0 * PIPA_S2LENG                                         108
      Z3 = Z2 -(2*PIPA_S3LENG + PIPA_CONELEN)                             109
      Z4 = Z2 -202.768 -29.96                                             110
      R2 = PIPA_S4OUTR+0.2                                                111
      Z1W = 403.86                                                        112
      Z2W = Z1W + 2.0 * PIWA_S2LENG                                       113
      Z3W = Z2W +(2*PIWA_S3LENG + PIWA_CONELEN)                           114
      Z4W = Z2W + PIWA_CONELEN+ PIWA_S3LENG*2 + PIWA_S4LENG               115
      R2W = PIWA_S4OUTR+0.2                                               116
      CALL AGSBEGM(MODULE,IPRIN)                                          117
C Check IPRIN<0                                                           117
      IF (IPRIN.LT.0) RETURN                                              117
C Create PIPE :                                                           117
C Call To Local Procedure PIPE                                            117
      ASSIGN 5030 TO BLOCK_PIPE                                           117
      GO TO  5020                                                         117
5030  CONTINUE                                                            117
C *                                                                       117
C Geant POSITION : PIPE ===> MOTHER='IDSM' X=PIPG_XOFFSET Y=PIPG_YOFFSET  118
C  Z=PIPG_ZOFFSET <===                                                    118
      CALL AGSRESET                                                       118
      AG_MOTHER = 'IDSM'                                                  118
      AG_X = PIPG_XOFFSET                                                 118
      AG_Y = PIPG_YOFFSET                                                 118
      AG_Z = PIPG_ZOFFSET                                                 118
      AG_TITLE = 'PIPE'                                                   118
      AG_EXNAME = 'POSITION'                                              118
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         118
      CALL AXPOSITION                                                     118
C *                                                                       118
      CALL AGSBEGM(MODULE,IPRIN)                                          118
C Check IPRIN<0                                                           119
      IF (IPRIN.LT.0) RETURN                                              119
C Create PIPI :                                                           119
C Call To Local Procedure PIPI                                            119
      ASSIGN 5050 TO BLOCK_PIPI                                           119
      GO TO  5040                                                         119
5050  CONTINUE                                                            119
C *                                                                       119
C Geant POSITION : PIPI ===> MOTHER='IDSM' X=PIPG_XOFFSET Y=PIPG_YOFFSET  120
C  Z=PIPG_ZOFFSET KONLY='MANY' <===                                       120
      CALL AGSRESET                                                       120
      AG_MOTHER = 'IDSM'                                                  120
      AG_X = PIPG_XOFFSET                                                 120
      AG_Y = PIPG_YOFFSET                                                 120
      AG_Z = PIPG_ZOFFSET                                                 120
      AG_KONLY = 'MANY'                                                   120
      AG_TITLE = 'PIPI'                                                   120
      AG_EXNAME = 'POSITION'                                              120
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   120
      CALL AXPOSITION                                                     120
C *                                                                       120
      CALL AGSBEGM(MODULE,IPRIN)                                          120
C Check IPRIN<0                                                           121
      IF (IPRIN.LT.0) RETURN                                              121
C Create PIPW :                                                           121
C Call To Local Procedure PIPW                                            121
      ASSIGN 5070 TO BLOCK_PIPW                                           121
      GO TO  5060                                                         121
5070  CONTINUE                                                            121
C *                                                                       121
C Geant POSITION : PIPW ===> MOTHER='IDSM' X=PIPG_XOFFSET Y=PIPG_YOFFSET  122
C  Z=PIPG_ZOFFSET <===                                                    122
      CALL AGSRESET                                                       122
      AG_MOTHER = 'IDSM'                                                  122
      AG_X = PIPG_XOFFSET                                                 122
      AG_Y = PIPG_YOFFSET                                                 122
      AG_Z = PIPG_ZOFFSET                                                 122
      AG_TITLE = 'PIPW'                                                   122
      AG_EXNAME = 'POSITION'                                              122
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         122
      CALL AXPOSITION                                                     122
C *                                                                       122
      CALL AGSBEGM(MODULE,IPRIN)                                          122
C Check IPRIN<0                                                           123
      IF (IPRIN.LT.0) RETURN                                              123
C Create PIPA :                                                           123
C Call To Local Procedure PIPA                                            123
      ASSIGN 5090 TO BLOCK_PIPA                                           123
      GO TO  5080                                                         123
5090  CONTINUE                                                            123
C *                                                                       123
C Geant POSITION : PIPA ===> MOTHER='CAVE' X=0 Y=0 Z=0 KONLY='MANY' <===  124
      CALL AGSRESET                                                       124
      AG_MOTHER = 'CAVE'                                                  124
      AG_X = 0                                                            124
      AG_Y = 0                                                            124
      AG_Z = 0                                                            124
      AG_KONLY = 'MANY'                                                   124
      AG_TITLE = 'PIPA'                                                   124
      AG_EXNAME = 'POSITION'                                              124
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   124
      CALL AXPOSITION                                                     124
C *                                                                       124
      CALL AGSBEGM(MODULE,IPRIN)                                          124
C Check IPRIN<0                                                           125
      IF (IPRIN.LT.0) RETURN                                              125
C Create PIWA :                                                           125
C Call To Local Procedure PIWA                                            125
      ASSIGN 5110 TO BLOCK_PIWA                                           125
      GO TO  5100                                                         125
5110  CONTINUE                                                            125
C *                                                                       125
C Geant POSITION : PIWA ===> MOTHER='CAVE' X=0 Y=0 Z=0 KONLY='MANY' <===  126
      CALL AGSRESET                                                       126
      AG_MOTHER = 'CAVE'                                                  126
      AG_X = 0                                                            126
      AG_Y = 0                                                            126
      AG_Z = 0                                                            126
      AG_KONLY = 'MANY'                                                   126
      AG_TITLE = 'PIWA'                                                   126
      AG_EXNAME = 'POSITION'                                              126
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   126
      CALL AXPOSITION                                                     126
C *                                                                       126
C ----------------------------------------------------------------------  126
C ------------ PIPE                                                       126
      CALL AGSENDM                                                        128
      RETURN                                                              128
C Begin Local Procedure PIPE                                              128
C PIPE PIPE MOTHER VOLUME OF THE EAST SECTION                             128
5020  CONTINUE                                                            128
         AG_TITLE='PIPE'                                                  128
         AG_MODULE=MODULE                                                 128
         AG_MARK='V00'                                                    128
         CALL AGSPUSH                                                     128
         AG_MODULE=MODULE                                                 128
C    *                                                                    128
C    Geant MATERIAL : AIR ===> inherited arguments used <===              129
         CALL AGSRESET                                                    129
         AG_TITLE = 'AIR'                                                 129
         AG_EXNAME = 'MATERIAL'                                           129
         AG_PARLIST = 'NONE'                                              129
         CALL AXMATERIAL                                                  129
C    *                                                                    129
C    Geant ATTRIBUTE : PIPE ===> SEEN=0 <===                              129
         CALL AGSRESET                                                    130
         AG_SEEN = 0                                                      130
         AG_TITLE = 'PIPE'                                                130
         AG_EXNAME = 'ATTRIBUTE'                                          130
         AG_PARLIST = 'SEEN'                                              130
         CALL AXATTRIBUTE                                                 130
C    *                                                                    130
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={-189*INCH, -79.71*  130
C    INCH,-64.71*INCH,-55.71*INCH} RMN={0, 0,0,0} RMX={1.5*INCH+0.8125*I  130
C    NCH,1.5*INCH+0.8125*INCH, 0.7875*INCH+0.5875*INCH,0.7875*INCH+0.587  130
C    5*INCH} <===                                                         130
         CALL AGSRESET                                                    134
         AG_PHI1 = 0                                                      134
         AG_DPHI = 360                                                    134
         AG_NZ = 4                                                        134
         AG_ZI(1)=-189*INCH                                               134
         AG_ZI(2)=-79.71*INCH                                             134
         AG_ZI(3)=-64.71*INCH                                             134
         AG_ZI(4)=-55.71*INCH                                             134
         AG_RMN(1)=0                                                      134
         AG_RMN(2)=0                                                      134
         AG_RMN(3)=0                                                      134
         AG_RMN(4)=0                                                      134
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   134
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   134
         AG_RMX(3)=0.7875*INCH+0.5875*INCH                                134
         AG_RMX(4)=0.7875*INCH+0.5875*INCH                                134
         AG_TITLE = 'PCON'                                                134
         AG_EXNAME = 'SHAPE'                                              134
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           134
         CALL AXSHAPE                                                     134
C    *                                                                    134
C    Create PIHE :                                                        135
C    Call To Local Procedure PIHE                                         135
         ASSIGN 5130 TO BLOCK_PIHE                                        135
         GO TO  5120                                                      135
5130     CONTINUE                                                         135
C    *                                                                    135
C    Geant POSITION : PIHE ===> MOTHER='PIPE' KONLY='ONLY' <===           136
         CALL AGSRESET                                                    136
         AG_MOTHER = 'PIPE'                                               136
         AG_KONLY = 'ONLY'                                                136
         AG_TITLE = 'PIHE'                                                136
         AG_EXNAME = 'POSITION'                                           136
         AG_PARLIST = 'MOTHER_KONLY'                                      136
         CALL AXPOSITION                                                  136
C    *                                                                    136
C    Create PALQ :                                                        137
C    Call To Local Procedure PALQ                                         137
         ASSIGN 5150 TO BLOCK_PALQ                                        137
         GO TO  5140                                                      137
5150     CONTINUE                                                         137
C    *                                                                    137
C    Geant POSITION : PALQ ===> MOTHER='PIPE' KONLY='ONLY' <===           138
         CALL AGSRESET                                                    138
         AG_MOTHER = 'PIPE'                                               138
         AG_KONLY = 'ONLY'                                                138
         AG_TITLE = 'PALQ'                                                138
         AG_EXNAME = 'POSITION'                                           138
         AG_PARLIST = 'MOTHER_KONLY'                                      138
         CALL AXPOSITION                                                  138
C    *                                                                    138
C    Create PALR :                                                        139
C    Call To Local Procedure PALR                                         139
         ASSIGN 5170 TO BLOCK_PALR                                        139
         GO TO  5160                                                      139
5170     CONTINUE                                                         139
C    *                                                                    139
C    Geant POSITION : PALR ===> MOTHER='PIPE' KONLY='ONLY' <===           140
         CALL AGSRESET                                                    140
         AG_MOTHER = 'PIPE'                                               140
         AG_KONLY = 'ONLY'                                                140
         AG_TITLE = 'PALR'                                                140
         AG_EXNAME = 'POSITION'                                           140
         AG_PARLIST = 'MOTHER_KONLY'                                      140
         CALL AXPOSITION                                                  140
C    *                                                                    140
C    Create SSCF :                                                        141
C    Call To Local Procedure SSCF                                         141
         ASSIGN 5190 TO BLOCK_SSCF                                        141
         GO TO  5180                                                      141
5190     CONTINUE                                                         141
C    *                                                                    141
C    Geant POSITION : SSCF ===> MOTHER='PIPE' X=0 Y=0 Z=-55.71*INCH - 0.  142
C    125*INCH <===                                                        142
         CALL AGSRESET                                                    142
         AG_MOTHER = 'PIPE'                                               142
         AG_X = 0                                                         142
         AG_Y = 0                                                         142
         AG_Z = -55.71*INCH - 0.125*INCH                                  142
         AG_TITLE = 'SSCF'                                                142
         AG_EXNAME = 'POSITION'                                           142
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      142
         CALL AXPOSITION                                                  142
C    *                                                                    142
C    Create SSCG :                                                        143
C    Call To Local Procedure SSCG                                         143
         ASSIGN 5210 TO BLOCK_SSCG                                        143
         GO TO  5200                                                      143
5210     CONTINUE                                                         143
C    *                                                                    143
C    Geant POSITION : SSCG ===> MOTHER='PIPE' X=0 Y=0 Z=-189*INCH + 0.38  144
C    /2*INCH <===                                                         144
         CALL AGSRESET                                                    144
         AG_MOTHER = 'PIPE'                                               144
         AG_X = 0                                                         144
         AG_Y = 0                                                         144
         AG_Z = -189*INCH + 0.38/2*INCH                                   144
         AG_TITLE = 'SSCG'                                                144
         AG_EXNAME = 'POSITION'                                           144
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      144
         CALL AXPOSITION                                                  144
C    *                                                                    144
C    Create SSCG :                                                        145
C    Call To Local Procedure SSCG                                         145
         ASSIGN 5220 TO BLOCK_SSCG                                        145
         GO TO  5200                                                      145
5220     CONTINUE                                                         145
C    *                                                                    145
C    Geant POSITION : SSCG ===> MOTHER='PIPE' X=0 Y=0 Z=-159*INCH + 0.38  146
C    /2*INCH <===                                                         146
         CALL AGSRESET                                                    146
         AG_MOTHER = 'PIPE'                                               146
         AG_X = 0                                                         146
         AG_Y = 0                                                         146
         AG_Z = -159*INCH + 0.38/2*INCH                                   146
         AG_TITLE = 'SSCG'                                                146
         AG_EXNAME = 'POSITION'                                           146
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      146
         CALL AXPOSITION                                                  146
C    *                                                                    146
C    Create SSCG :                                                        147
C    Call To Local Procedure SSCG                                         147
         ASSIGN 5230 TO BLOCK_SSCG                                        147
         GO TO  5200                                                      147
5230     CONTINUE                                                         147
C    *                                                                    147
C    Geant POSITION : SSCG ===> MOTHER='PIPE' X=0 Y=0 Z=-159*INCH - 0.38  148
C    /2*INCH <===                                                         148
         CALL AGSRESET                                                    148
         AG_MOTHER = 'PIPE'                                               148
         AG_X = 0                                                         148
         AG_Y = 0                                                         148
         AG_Z = -159*INCH - 0.38/2*INCH                                   148
         AG_TITLE = 'SSCG'                                                148
         AG_EXNAME = 'POSITION'                                           148
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      148
         CALL AXPOSITION                                                  148
C    *                                                                    148
5021  CALL AGSPOPB                                                        149
      GO TO BLOCK_PIPE, (5020 ,5030)                                      149
C End of Local Procedure                                                  149
C ----------------------------------------------------------------------  150
C ------------ PIHE                                                       150
C Begin Local Procedure PIHE                                              151
C PIHE THE HOLE INSIDE THE EAST SECTION OF BEAM PIPE                      151
5120  CONTINUE                                                            151
         AG_TITLE='PIHE'                                                  151
         AG_MODULE=MODULE                                                 151
         AG_MARK='V00'                                                    151
         CALL AGSPUSH                                                     151
         AG_MODULE=MODULE                                                 151
C    *                                                                    151
C    Geant MATERIAL : AIR ===> inherited arguments used <===              152
         CALL AGSRESET                                                    152
         AG_TITLE = 'AIR'                                                 152
         AG_EXNAME = 'MATERIAL'                                           152
         AG_PARLIST = 'NONE'                                              152
         CALL AXMATERIAL                                                  152
C    *                                                                    152
C    Geant MATERIAL : VACUUM ===> DENS=PIPG_VACDENS <===                  152
         CALL AGSRESET                                                    153
         AG_DENS = PIPG_VACDENS                                           153
         AG_TITLE = 'VACUUM'                                              153
         AG_EXNAME = 'MATERIAL'                                           153
         AG_PARLIST = 'DENS'                                              153
         CALL AXMATERIAL                                                  153
C    *                                                                    153
C    Geant ATTRIBUTE : PIHE ===> SEEN=0 <===                              153
         CALL AGSRESET                                                    154
         AG_SEEN = 0                                                      154
         AG_TITLE = 'PIHE'                                                154
         AG_EXNAME = 'ATTRIBUTE'                                          154
         AG_PARLIST = 'SEEN'                                              154
         CALL AXATTRIBUTE                                                 154
C    *                                                                    154
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={-189*INCH, -79.71*  154
C    INCH,-64.71*INCH,-55.71*INCH} RMN={0, 0,0,0} RMX={1.5*INCH,1.5*INCH  154
C    ,0.7875*INCH, 0.7875*INCH} <===                                      154
         CALL AGSRESET                                                    158
         AG_PHI1 = 0                                                      158
         AG_DPHI = 360                                                    158
         AG_NZ = 4                                                        158
         AG_ZI(1)=-189*INCH                                               158
         AG_ZI(2)=-79.71*INCH                                             158
         AG_ZI(3)=-64.71*INCH                                             158
         AG_ZI(4)=-55.71*INCH                                             158
         AG_RMN(1)=0                                                      158
         AG_RMN(2)=0                                                      158
         AG_RMN(3)=0                                                      158
         AG_RMN(4)=0                                                      158
         AG_RMX(1)=1.5*INCH                                               158
         AG_RMX(2)=1.5*INCH                                               158
         AG_RMX(3)=0.7875*INCH                                            158
         AG_RMX(4)=0.7875*INCH                                            158
         AG_TITLE = 'PCON'                                                158
         AG_EXNAME = 'SHAPE'                                              158
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           158
         CALL AXSHAPE                                                     158
C    *                                                                    158
5121  CALL AGSPOPB                                                        159
      GO TO BLOCK_PIHE, (5120 ,5130)                                      159
C End of Local Procedure                                                  159
C ----------------------------------------------------------------------  160
C ------------ PALQ                                                       160
C Begin Local Procedure PALQ                                              161
C PALQ THE EAST END ALUMINIUM TUBE OF THE BEAM PIPE                       161
5140  CONTINUE                                                            161
         AG_TITLE='PALQ'                                                  161
         AG_MODULE=MODULE                                                 161
         AG_MARK='V00'                                                    161
         CALL AGSPUSH                                                     161
         AG_MODULE=MODULE                                                 161
C    *                                                                    161
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        162
         CALL AGSRESET                                                    162
         AG_TITLE = 'ALUMINIUM'                                           162
         AG_EXNAME = 'MATERIAL'                                           162
         AG_PARLIST = 'NONE'                                              162
         CALL AXMATERIAL                                                  162
C    *                                                                    162
C    Geant ATTRIBUTE : PALQ ===> SEEN=1 COLO=4 <===                       162
         CALL AGSRESET                                                    163
         AG_SEEN = 1                                                      163
         AG_COLO = 4                                                      163
         AG_TITLE = 'PALQ'                                                163
         AG_EXNAME = 'ATTRIBUTE'                                          163
         AG_PARLIST = 'SEEN_COLO'                                         163
         CALL AXATTRIBUTE                                                 163
C    *                                                                    163
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={(-189+0.38)*INCH,   163
C    (-189+0.75)*INCH,-(189-0.75)*INCH, -159.76*INCH,-159.76*INCH,-159.3  163
C    8*INCH} RMN={1.5*INCH, 1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH, 1.5*INC  163
C    H} RMX={1.5*INCH+0.8125*INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.049*I  163
C    NCH,1.5*INCH+0.049*INCH, 1.5*INCH+0.8125*INCH,1.5*INCH+0.8125*INCH}  163
C     <===                                                                163
         CALL AGSRESET                                                    170
         AG_PHI1 = 0                                                      170
         AG_DPHI = 360                                                    170
         AG_NZ = 6                                                        170
         AG_ZI(1)=(-189+0.38)*INCH                                        170
         AG_ZI(2)=(-189+0.75)*INCH                                        170
         AG_ZI(3)=-(189-0.75)*INCH                                        170
         AG_ZI(4)=-159.76*INCH                                            170
         AG_ZI(5)=-159.76*INCH                                            170
         AG_ZI(6)=-159.38*INCH                                            170
         AG_RMN(1)=1.5*INCH                                               170
         AG_RMN(2)=1.5*INCH                                               170
         AG_RMN(3)=1.5*INCH                                               170
         AG_RMN(4)=1.5*INCH                                               170
         AG_RMN(5)=1.5*INCH                                               170
         AG_RMN(6)=1.5*INCH                                               170
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   170
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   170
         AG_RMX(3)=1.5*INCH+0.049*INCH                                    170
         AG_RMX(4)=1.5*INCH+0.049*INCH                                    170
         AG_RMX(5)=1.5*INCH+0.8125*INCH                                   170
         AG_RMX(6)=1.5*INCH+0.8125*INCH                                   170
         AG_TITLE = 'PCON'                                                170
         AG_EXNAME = 'SHAPE'                                              170
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           170
         CALL AXSHAPE                                                     170
C    *                                                                    170
5141  CALL AGSPOPB                                                        171
      GO TO BLOCK_PALQ, (5140 ,5150)                                      171
C End of Local Procedure                                                  171
C ----------------------------------------------------------------------  172
C ------------ PALR                                                       172
C Begin Local Procedure PALR                                              173
C PALR THE EAST ALUMINIUM TRANSITION TUBE OF THE FULL BEAM PIPE           173
5160  CONTINUE                                                            173
         AG_TITLE='PALR'                                                  173
         AG_MODULE=MODULE                                                 173
         AG_MARK='V00'                                                    173
         CALL AGSPUSH                                                     173
         AG_MODULE=MODULE                                                 173
C    *                                                                    173
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        174
         CALL AGSRESET                                                    174
         AG_TITLE = 'ALUMINIUM'                                           174
         AG_EXNAME = 'MATERIAL'                                           174
         AG_PARLIST = 'NONE'                                              174
         CALL AXMATERIAL                                                  174
C    *                                                                    174
C    Geant ATTRIBUTE : PALR ===> SEEN=1 COLO=4 <===                       174
         CALL AGSRESET                                                    175
         AG_SEEN = 1                                                      175
         AG_COLO = 4                                                      175
         AG_TITLE = 'PALR'                                                175
         AG_EXNAME = 'ATTRIBUTE'                                          175
         AG_PARLIST = 'SEEN_COLO'                                         175
         CALL AXATTRIBUTE                                                 175
C    *                                                                    175
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=8 ZI={(-159.000+0.38)*IN  175
C    CH, -158.241*INCH,-158.241*INCH, -79.71*INCH,-64.71*INCH,-56.21*INC  175
C    H,-56.21*INCH, -55.71*INCH-0.25*INCH} RMN={1.5*INCH,1.5*INCH, 1.5*I  175
C    NCH,1.5*INCH,0.7875*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH} RMX=  175
C    {1.5*INCH+0.8125*INCH, 1.5*INCH+0.8125*INCH,1.5*INCH+0.049*INCH, 1.  175
C    5*INCH+0.049*INCH,0.7875*INCH+0.049*INCH, 0.7875*INCH+0.049*INCH,0.  175
C    7875*INCH+0.5875*INCH, 0.7875*INCH+0.5875*INCH} <===                 175
         CALL AGSRESET                                                    185
         AG_PHI1 = 0                                                      185
         AG_DPHI = 360                                                    185
         AG_NZ = 8                                                        185
         AG_ZI(1)=(-159.000+0.38)*INCH                                    185
         AG_ZI(2)=-158.241*INCH                                           185
         AG_ZI(3)=-158.241*INCH                                           185
         AG_ZI(4)=-79.71*INCH                                             185
         AG_ZI(5)=-64.71*INCH                                             185
         AG_ZI(6)=-56.21*INCH                                             185
         AG_ZI(7)=-56.21*INCH                                             185
         AG_ZI(8)=-55.71*INCH-0.25*INCH                                   185
         AG_RMN(1)=1.5*INCH                                               185
         AG_RMN(2)=1.5*INCH                                               185
         AG_RMN(3)=1.5*INCH                                               185
         AG_RMN(4)=1.5*INCH                                               185
         AG_RMN(5)=0.7875*INCH                                            185
         AG_RMN(6)=0.7875*INCH                                            185
         AG_RMN(7)=0.7875*INCH                                            185
         AG_RMN(8)=0.7875*INCH                                            185
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   185
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   185
         AG_RMX(3)=1.5*INCH+0.049*INCH                                    185
         AG_RMX(4)=1.5*INCH+0.049*INCH                                    185
         AG_RMX(5)=0.7875*INCH+0.049*INCH                                 185
         AG_RMX(6)=0.7875*INCH+0.049*INCH                                 185
         AG_RMX(7)=0.7875*INCH+0.5875*INCH                                185
         AG_RMX(8)=0.7875*INCH+0.5875*INCH                                185
         AG_TITLE = 'PCON'                                                185
         AG_EXNAME = 'SHAPE'                                              185
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           185
         CALL AXSHAPE                                                     185
C    *                                                                    185
5161  CALL AGSPOPB                                                        186
      GO TO BLOCK_PALR, (5160 ,5170)                                      186
C End of Local Procedure                                                  186
C ----------------------------------------------------------------------  187
C ------------ PIPI                                                       187
C Begin Local Procedure PIPI                                              188
C PIPI PIPE MIDDLE MOTHER VOLUME                                          188
5040  CONTINUE                                                            188
         AG_TITLE='PIPI'                                                  188
         AG_MODULE=MODULE                                                 188
         AG_MARK='V00'                                                    188
         CALL AGSPUSH                                                     188
         AG_MODULE=MODULE                                                 188
C    *                                                                    188
C    Geant MATERIAL : AIR ===> inherited arguments used <===              189
         CALL AGSRESET                                                    189
         AG_TITLE = 'AIR'                                                 189
         AG_EXNAME = 'MATERIAL'                                           189
         AG_PARLIST = 'NONE'                                              189
         CALL AXMATERIAL                                                  189
C    *                                                                    189
C    Geant ATTRIBUTE : PIPI ===> SEEN=0 COLO=4 <===                       189
         CALL AGSRESET                                                    190
         AG_SEEN = 0                                                      190
         AG_COLO = 4                                                      190
         AG_TITLE = 'PIPI'                                                190
         AG_EXNAME = 'ATTRIBUTE'                                          190
         AG_PARLIST = 'SEEN_COLO'                                         190
         CALL AXATTRIBUTE                                                 190
C    *                                                                    190
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=10 ZI={-55.71*INCH, -55.  190
C    21*INCH,-55.21*INCH,-56.0,-56.0, 56.0,56.0,55.21*INCH,55.21*INCH,55  190
C    .71*INCH} RMN={0, 0,0,0,0,0,0,0,0,0} RMX={0.7875*INCH+0.5875*INCH,   190
C    0.7875*INCH+0.5875*INCH,0.7875*INCH+0.7, 0.7875*INCH+0.7,0.7875*INC  190
C    H+0.055*INCH,0.7875*INCH+0.055*INCH, 0.7875*INCH+0.7,0.7875*INCH+0.  190
C    7, 0.7875*INCH+0.5875*INCH,0.7875*INCH+0.5875*INCH} <===             190
         CALL AGSRESET                                                    198
         AG_PHI1 = 0                                                      198
         AG_DPHI = 360                                                    198
         AG_NZ = 10                                                       198
         AG_ZI(1)=-55.71*INCH                                             198
         AG_ZI(2)=-55.21*INCH                                             198
         AG_ZI(3)=-55.21*INCH                                             198
         AG_ZI(4)=-56.0                                                   198
         AG_ZI(5)=-56.0                                                   198
         AG_ZI(6)=56.0                                                    198
         AG_ZI(7)=56.0                                                    198
         AG_ZI(8)=55.21*INCH                                              198
         AG_ZI(9)=55.21*INCH                                              198
         AG_ZI(10)=55.71*INCH                                             198
         AG_RMN(1)=0                                                      198
         AG_RMN(2)=0                                                      198
         AG_RMN(3)=0                                                      198
         AG_RMN(4)=0                                                      198
         AG_RMN(5)=0                                                      198
         AG_RMN(6)=0                                                      198
         AG_RMN(7)=0                                                      198
         AG_RMN(8)=0                                                      198
         AG_RMN(9)=0                                                      198
         AG_RMN(10)=0                                                     198
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                198
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                198
         AG_RMX(3)=0.7875*INCH+0.7                                        198
         AG_RMX(4)=0.7875*INCH+0.7                                        198
         AG_RMX(5)=0.7875*INCH+0.055*INCH                                 198
         AG_RMX(6)=0.7875*INCH+0.055*INCH                                 198
         AG_RMX(7)=0.7875*INCH+0.7                                        198
         AG_RMX(8)=0.7875*INCH+0.7                                        198
         AG_RMX(9)=0.7875*INCH+0.5875*INCH                                198
         AG_RMX(10)=0.7875*INCH+0.5875*INCH                               198
         AG_TITLE = 'PCON'                                                198
         AG_EXNAME = 'SHAPE'                                              198
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           198
         CALL AXSHAPE                                                     198
C    *                                                                    198
C    Create PIHI :                                                        199
C    Call To Local Procedure PIHI                                         199
         ASSIGN 5250 TO BLOCK_PIHI                                        199
         GO TO  5240                                                      199
5250     CONTINUE                                                         199
C    *                                                                    199
C    Geant POSITION : PIHI ===> MOTHER='PIPI' KONLY='ONLY' <===           200
         CALL AGSRESET                                                    200
         AG_MOTHER = 'PIPI'                                               200
         AG_KONLY = 'ONLY'                                                200
         AG_TITLE = 'PIHI'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'MOTHER_KONLY'                                      200
         CALL AXPOSITION                                                  200
C    *                                                                    200
C    Create PALS :                                                        201
C    Call To Local Procedure PALS                                         201
         ASSIGN 5270 TO BLOCK_PALS                                        201
         GO TO  5260                                                      201
5270     CONTINUE                                                         201
C    *                                                                    201
C    Geant POSITION : PALS ===> MOTHER='PIPI' KONLY='ONLY' <===           202
         CALL AGSRESET                                                    202
         AG_MOTHER = 'PIPI'                                               202
         AG_KONLY = 'ONLY'                                                202
         AG_TITLE = 'PALS'                                                202
         AG_EXNAME = 'POSITION'                                           202
         AG_PARLIST = 'MOTHER_KONLY'                                      202
         CALL AXPOSITION                                                  202
C    *                                                                    202
C    Create PBES :                                                        203
C    Call To Local Procedure PBES                                         203
         ASSIGN 5290 TO BLOCK_PBES                                        203
         GO TO  5280                                                      203
5290     CONTINUE                                                         203
C    *                                                                    203
C    Geant POSITION : PBES ===> MOTHER='PIPI' Z=PIPG_ZOFFBER KONLY='ONLY  204
C    *' <===                                                              204
         CALL AGSRESET                                                    204
         AG_MOTHER = 'PIPI'                                               204
         AG_Z = PIPG_ZOFFBER                                              204
         AG_KONLY = 'ONLY'                                                204
         AG_TITLE = 'PBES'                                                204
         AG_EXNAME = 'POSITION'                                           204
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    204
         CALL AXPOSITION                                                  204
C    *                                                                    204
C    Create PALI :                                                        205
C    Call To Local Procedure PALI                                         205
         ASSIGN 5310 TO BLOCK_PALI                                        205
         GO TO  5300                                                      205
5310     CONTINUE                                                         205
C    *                                                                    205
C    Geant POSITION : PALI ===> MOTHER='PIPI' KONLY='ONLY' <===           206
         CALL AGSRESET                                                    206
         AG_MOTHER = 'PIPI'                                               206
         AG_KONLY = 'ONLY'                                                206
         AG_TITLE = 'PALI'                                                206
         AG_EXNAME = 'POSITION'                                           206
         AG_PARLIST = 'MOTHER_KONLY'                                      206
         CALL AXPOSITION                                                  206
C    *                                                                    206
C    Create SSCF :                                                        207
C    Call To Local Procedure SSCF                                         207
         ASSIGN 5320 TO BLOCK_SSCF                                        207
         GO TO  5180                                                      207
5320     CONTINUE                                                         207
C    *                                                                    207
C    Geant POSITION : SSCF ===> MOTHER='PIPI' X=0 Y=0 Z=-55.71*INCH + 0.  208
C    125*INCH <===                                                        208
         CALL AGSRESET                                                    208
         AG_MOTHER = 'PIPI'                                               208
         AG_X = 0                                                         208
         AG_Y = 0                                                         208
         AG_Z = -55.71*INCH + 0.125*INCH                                  208
         AG_TITLE = 'SSCF'                                                208
         AG_EXNAME = 'POSITION'                                           208
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      208
         CALL AXPOSITION                                                  208
C    *                                                                    208
C    Create SSCF :                                                        209
C    Call To Local Procedure SSCF                                         209
         ASSIGN 5330 TO BLOCK_SSCF                                        209
         GO TO  5180                                                      209
5330     CONTINUE                                                         209
C    *                                                                    209
C    Geant POSITION : SSCF ===> MOTHER='PIPI' X=0 Y=0 Z=+55.71*INCH - 0.  210
C    125*INCH <===                                                        210
         CALL AGSRESET                                                    210
         AG_MOTHER = 'PIPI'                                               210
         AG_X = 0                                                         210
         AG_Y = 0                                                         210
         AG_Z = +55.71*INCH - 0.125*INCH                                  210
         AG_TITLE = 'SSCF'                                                210
         AG_EXNAME = 'POSITION'                                           210
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      210
         CALL AXPOSITION                                                  210
C    *                                                                    210
5041  CALL AGSPOPB                                                        211
      GO TO BLOCK_PIPI, (5040 ,5050)                                      211
C End of Local Procedure                                                  211
C ----------------------------------------------------------------------  212
C ------------ PIHI                                                       212
C Begin Local Procedure PIHI                                              213
C PIHI THE HOLE INSIDE THE BEAM PIPE                                      213
5240  CONTINUE                                                            213
         AG_TITLE='PIHI'                                                  213
         AG_MODULE=MODULE                                                 213
         AG_MARK='V00'                                                    213
         CALL AGSPUSH                                                     213
         AG_MODULE=MODULE                                                 213
C    *                                                                    213
C    Geant MATERIAL : AIR ===> inherited arguments used <===              214
         CALL AGSRESET                                                    214
         AG_TITLE = 'AIR'                                                 214
         AG_EXNAME = 'MATERIAL'                                           214
         AG_PARLIST = 'NONE'                                              214
         CALL AXMATERIAL                                                  214
C    *                                                                    214
C    Geant MATERIAL : VACUUM ===> DENS=PIPG_VACDENS <===                  214
         CALL AGSRESET                                                    215
         AG_DENS = PIPG_VACDENS                                           215
         AG_TITLE = 'VACUUM'                                              215
         AG_EXNAME = 'MATERIAL'                                           215
         AG_PARLIST = 'DENS'                                              215
         CALL AXMATERIAL                                                  215
C    *                                                                    215
C    Geant ATTRIBUTE : PIHI ===> SEEN=0 <===                              215
         CALL AGSRESET                                                    216
         AG_SEEN = 0                                                      216
         AG_TITLE = 'PIHI'                                                216
         AG_EXNAME = 'ATTRIBUTE'                                          216
         AG_PARLIST = 'SEEN'                                              216
         CALL AXATTRIBUTE                                                 216
C    *                                                                    216
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.7875*INCH DZ=55.71*INCH <==  216
C    =                                                                    216
         CALL AGSRESET                                                    217
         AG_RMIN = 0.0                                                    217
         AG_RMAX = 0.7875*INCH                                            217
         AG_DZ = 55.71*INCH                                               217
         AG_TITLE = 'TUBE'                                                217
         AG_EXNAME = 'SHAPE'                                              217
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      217
         CALL AXSHAPE                                                     217
C    *                                                                    217
5241  CALL AGSPOPB                                                        218
      GO TO BLOCK_PIHI, (5240 ,5250)                                      218
C End of Local Procedure                                                  218
C ----------------------------------------------------------------------  219
C ------------ PALS                                                       219
C Begin Local Procedure PALS                                              220
C PALS THE EAST ALUMINIUM SECTION OF THE BEAM PIPE                        220
5260  CONTINUE                                                            220
         AG_TITLE='PALS'                                                  220
         AG_MODULE=MODULE                                                 220
         AG_MARK='V00'                                                    220
         CALL AGSPUSH                                                     220
         AG_MODULE=MODULE                                                 220
C    *                                                                    220
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        221
         CALL AGSRESET                                                    221
         AG_TITLE = 'ALUMINIUM'                                           221
         AG_EXNAME = 'MATERIAL'                                           221
         AG_PARLIST = 'NONE'                                              221
         CALL AXMATERIAL                                                  221
C    *                                                                    221
C    Geant ATTRIBUTE : PALS ===> SEEN=1 COLO=4 <===                       221
         CALL AGSRESET                                                    222
         AG_SEEN = 1                                                      222
         AG_COLO = 4                                                      222
         AG_TITLE = 'PALS'                                                222
         AG_EXNAME = 'ATTRIBUTE'                                          222
         AG_PARLIST = 'SEEN_COLO'                                         222
         CALL AXATTRIBUTE                                                 222
C    *                                                                    222
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-55.46*INCH, -55.2  222
C    1*INCH,-55.21*INCH,-43.71*INCH,-43.71*INCH, -15.75*INCH} RMN={0.787  222
C    5*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH  222
C    }, RMX={0.7875*INCH+0.5875*INCH,0.7875*INCH+0.5875*INCH, 0.7875*INC  222
C    H+0.065*INCH,0.7875*INCH+0.065*INCH, 0.7875*INCH+0.055*INCH,0.7875*  222
C    INCH+0.055*INCH} <===                                                222
         CALL AGSRESET                                                    229
         AG_PHI1 = 0                                                      229
         AG_DPHI = 360                                                    229
         AG_NZ = 6                                                        229
         AG_ZI(1)=-55.46*INCH                                             229
         AG_ZI(2)=-55.21*INCH                                             229
         AG_ZI(3)=-55.21*INCH                                             229
         AG_ZI(4)=-43.71*INCH                                             229
         AG_ZI(5)=-43.71*INCH                                             229
         AG_ZI(6)=-15.75*INCH                                             229
         AG_RMN(1)=0.7875*INCH                                            229
         AG_RMN(2)=0.7875*INCH                                            229
         AG_RMN(3)=0.7875*INCH                                            229
         AG_RMN(4)=0.7875*INCH                                            229
         AG_RMN(5)=0.7875*INCH                                            229
         AG_RMN(6)=0.7875*INCH                                            229
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                229
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                229
         AG_RMX(3)=0.7875*INCH+0.065*INCH                                 229
         AG_RMX(4)=0.7875*INCH+0.065*INCH                                 229
         AG_RMX(5)=0.7875*INCH+0.055*INCH                                 229
         AG_RMX(6)=0.7875*INCH+0.055*INCH                                 229
         AG_TITLE = 'PCON'                                                229
         AG_EXNAME = 'SHAPE'                                              229
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           229
         CALL AXSHAPE                                                     229
C    *                                                                    229
5261  CALL AGSPOPB                                                        230
      GO TO BLOCK_PALS, (5260 ,5270)                                      230
C End of Local Procedure                                                  230
C ----------------------------------------------------------------------  231
C ------------ PBES                                                       231
C Begin Local Procedure PBES                                              232
C PBES THE BERILLIUM SECTION OF THE BEAM PIPE                             232
5280  CONTINUE                                                            232
         AG_TITLE='PBES'                                                  232
         AG_MODULE=MODULE                                                 232
         AG_MARK='V00'                                                    232
         CALL AGSPUSH                                                     232
         AG_MODULE=MODULE                                                 232
C    *                                                                    232
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        233
         CALL AGSRESET                                                    233
         AG_TITLE = 'BERILLIUM'                                           233
         AG_EXNAME = 'MATERIAL'                                           233
         AG_PARLIST = 'NONE'                                              233
         CALL AXMATERIAL                                                  233
C    *                                                                    233
C    Geant ATTRIBUTE : PBES ===> SEEN=1 COLO=2 <===                       233
         CALL AGSRESET                                                    234
         AG_SEEN = 1                                                      234
         AG_COLO = 2                                                      234
         AG_TITLE = 'PBES'                                                234
         AG_EXNAME = 'ATTRIBUTE'                                          234
         AG_PARLIST = 'SEEN_COLO'                                         234
         CALL AXATTRIBUTE                                                 234
C    *                                                                    234
C    Geant SHAPE : TUBE ===> RMIN=0.7875*INCH RMAX=0.7875*INCH+0.030*INC  234
C    H, DZ=47.25*INCH/2 <===                                              234
         CALL AGSRESET                                                    236
         AG_RMIN = 0.7875*INCH                                            236
         AG_RMAX = 0.7875*INCH+0.030*INCH                                 236
         AG_DZ = 47.25*INCH/2                                             236
         AG_TITLE = 'TUBE'                                                236
         AG_EXNAME = 'SHAPE'                                              236
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      236
         CALL AXSHAPE                                                     236
C    *                                                                    236
5281  CALL AGSPOPB                                                        237
      GO TO BLOCK_PBES, (5280 ,5290)                                      237
C End of Local Procedure                                                  237
C ----------------------------------------------------------------------  238
C ------------ PALI                                                       238
C Begin Local Procedure PALI                                              239
C PALI THE WEST ALUMINIUM SECTION OF THE MIDDLE BEAM PIPE                 239
5300  CONTINUE                                                            239
         AG_TITLE='PALI'                                                  239
         AG_MODULE=MODULE                                                 239
         AG_MARK='V00'                                                    239
         CALL AGSPUSH                                                     239
         AG_MODULE=MODULE                                                 239
C    *                                                                    239
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        240
         CALL AGSRESET                                                    240
         AG_TITLE = 'ALUMINIUM'                                           240
         AG_EXNAME = 'MATERIAL'                                           240
         AG_PARLIST = 'NONE'                                              240
         CALL AXMATERIAL                                                  240
C    *                                                                    240
C    Geant ATTRIBUTE : PALI ===> SEEN=1 COLO=4 <===                       240
         CALL AGSRESET                                                    241
         AG_SEEN = 1                                                      241
         AG_COLO = 4                                                      241
         AG_TITLE = 'PALI'                                                241
         AG_EXNAME = 'ATTRIBUTE'                                          241
         AG_PARLIST = 'SEEN_COLO'                                         241
         CALL AXATTRIBUTE                                                 241
C    *                                                                    241
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={31.5*INCH, 43.72*I  241
C    NCH,43.72*INCH,55.21*INCH,55.21*INCH, 55.46*INCH} RMN={0.7875*INCH,  241
C    0.7875*INCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH}, RMX=  241
C    {0.7875*INCH+0.055*INCH,0.7875*INCH+0.055*INCH, 0.7875*INCH+0.065*I  241
C    NCH,0.7875*INCH+0.065*INCH, 0.7875*INCH+0.5875*INCH,0.7875*INCH+0.5  241
C    875*INCH} <===                                                       241
         CALL AGSRESET                                                    248
         AG_PHI1 = 0                                                      248
         AG_DPHI = 360                                                    248
         AG_NZ = 6                                                        248
         AG_ZI(1)=31.5*INCH                                               248
         AG_ZI(2)=43.72*INCH                                              248
         AG_ZI(3)=43.72*INCH                                              248
         AG_ZI(4)=55.21*INCH                                              248
         AG_ZI(5)=55.21*INCH                                              248
         AG_ZI(6)=55.46*INCH                                              248
         AG_RMN(1)=0.7875*INCH                                            248
         AG_RMN(2)=0.7875*INCH                                            248
         AG_RMN(3)=0.7875*INCH                                            248
         AG_RMN(4)=0.7875*INCH                                            248
         AG_RMN(5)=0.7875*INCH                                            248
         AG_RMN(6)=0.7875*INCH                                            248
         AG_RMX(1)=0.7875*INCH+0.055*INCH                                 248
         AG_RMX(2)=0.7875*INCH+0.055*INCH                                 248
         AG_RMX(3)=0.7875*INCH+0.065*INCH                                 248
         AG_RMX(4)=0.7875*INCH+0.065*INCH                                 248
         AG_RMX(5)=0.7875*INCH+0.5875*INCH                                248
         AG_RMX(6)=0.7875*INCH+0.5875*INCH                                248
         AG_TITLE = 'PCON'                                                248
         AG_EXNAME = 'SHAPE'                                              248
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           248
         CALL AXSHAPE                                                     248
C    *                                                                    248
5301  CALL AGSPOPB                                                        249
      GO TO BLOCK_PALI, (5300 ,5310)                                      249
C End of Local Procedure                                                  249
C ----------------------------------------------------------------------  250
C ------------ PIPW                                                       250
C Begin Local Procedure PIPW                                              251
C PIPW PIPE MOTHER VOLUME                                                 251
5060  CONTINUE                                                            251
         AG_TITLE='PIPW'                                                  251
         AG_MODULE=MODULE                                                 251
         AG_MARK='V00'                                                    251
         CALL AGSPUSH                                                     251
         AG_MODULE=MODULE                                                 251
C    *                                                                    251
C    Geant MATERIAL : AIR ===> inherited arguments used <===              252
         CALL AGSRESET                                                    252
         AG_TITLE = 'AIR'                                                 252
         AG_EXNAME = 'MATERIAL'                                           252
         AG_PARLIST = 'NONE'                                              252
         CALL AXMATERIAL                                                  252
C    *                                                                    252
C    Geant ATTRIBUTE : PIPW ===> SEEN=0 <===                              252
         CALL AGSRESET                                                    253
         AG_SEEN = 0                                                      253
         AG_TITLE = 'PIPW'                                                253
         AG_EXNAME = 'ATTRIBUTE'                                          253
         AG_PARLIST = 'SEEN'                                              253
         CALL AXATTRIBUTE                                                 253
C    *                                                                    253
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={55.71*INCH, 64.71*  253
C    INCH,79.71*INCH,159.000*INCH} RMN={0, 0,0,0} RMX={0.7875*INCH+0.587  253
C    5*INCH,0.7875*INCH+0.5875*INCH, 1.5*INCH+0.8125*INCH,1.5*INCH+0.812  253
C    5*INCH} <===                                                         253
         CALL AGSRESET                                                    257
         AG_PHI1 = 0                                                      257
         AG_DPHI = 360                                                    257
         AG_NZ = 4                                                        257
         AG_ZI(1)=55.71*INCH                                              257
         AG_ZI(2)=64.71*INCH                                              257
         AG_ZI(3)=79.71*INCH                                              257
         AG_ZI(4)=159.000*INCH                                            257
         AG_RMN(1)=0                                                      257
         AG_RMN(2)=0                                                      257
         AG_RMN(3)=0                                                      257
         AG_RMN(4)=0                                                      257
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                257
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                257
         AG_RMX(3)=1.5*INCH+0.8125*INCH                                   257
         AG_RMX(4)=1.5*INCH+0.8125*INCH                                   257
         AG_TITLE = 'PCON'                                                257
         AG_EXNAME = 'SHAPE'                                              257
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           257
         CALL AXSHAPE                                                     257
C    *                                                                    257
C    Create PIHW :                                                        258
C    Call To Local Procedure PIHW                                         258
         ASSIGN 5350 TO BLOCK_PIHW                                        258
         GO TO  5340                                                      258
5350     CONTINUE                                                         258
C    *                                                                    258
C    Geant POSITION : PIHW ===> MOTHER='PIPW' KONLY='ONLY' <===           259
         CALL AGSRESET                                                    259
         AG_MOTHER = 'PIPW'                                               259
         AG_KONLY = 'ONLY'                                                259
         AG_TITLE = 'PIHW'                                                259
         AG_EXNAME = 'POSITION'                                           259
         AG_PARLIST = 'MOTHER_KONLY'                                      259
         CALL AXPOSITION                                                  259
C    *                                                                    259
C    Create PALJ :                                                        260
C    Call To Local Procedure PALJ                                         260
         ASSIGN 5370 TO BLOCK_PALJ                                        260
         GO TO  5360                                                      260
5370     CONTINUE                                                         260
C    *                                                                    260
C    Geant POSITION : PALJ ===> MOTHER='PIPW' KONLY='ONLY' <===           261
         CALL AGSRESET                                                    261
         AG_MOTHER = 'PIPW'                                               261
         AG_KONLY = 'ONLY'                                                261
         AG_TITLE = 'PALJ'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'MOTHER_KONLY'                                      261
         CALL AXPOSITION                                                  261
C    *                                                                    261
C    Create PALK :                                                        262
C    Call To Local Procedure PALK                                         262
         ASSIGN 5390 TO BLOCK_PALK                                        262
         GO TO  5380                                                      262
5390     CONTINUE                                                         262
C    *                                                                    262
C    Geant POSITION : PALK ===> MOTHER='PIPW' KONLY='ONLY' <===           263
         CALL AGSRESET                                                    263
         AG_MOTHER = 'PIPW'                                               263
         AG_KONLY = 'ONLY'                                                263
         AG_TITLE = 'PALK'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'MOTHER_KONLY'                                      263
         CALL AXPOSITION                                                  263
C    *                                                                    263
C    Create SSCG :                                                        264
C    Call To Local Procedure SSCG                                         264
         ASSIGN 5400 TO BLOCK_SSCG                                        264
         GO TO  5200                                                      264
5400     CONTINUE                                                         264
C    *                                                                    264
C    Geant POSITION : SSCG ===> MOTHER='PIPW' X=0 Y=0 Z=+159*INCH - 0.38  265
C    /2*INCH <===                                                         265
         CALL AGSRESET                                                    265
         AG_MOTHER = 'PIPW'                                               265
         AG_X = 0                                                         265
         AG_Y = 0                                                         265
         AG_Z = +159*INCH - 0.38/2*INCH                                   265
         AG_TITLE = 'SSCG'                                                265
         AG_EXNAME = 'POSITION'                                           265
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      265
         CALL AXPOSITION                                                  265
C    *                                                                    265
C    Create SSCG :                                                        266
C    Call To Local Procedure SSCG                                         266
         ASSIGN 5410 TO BLOCK_SSCG                                        266
         GO TO  5200                                                      266
5410     CONTINUE                                                         266
C    *                                                                    266
C    Geant POSITION : SSCG ===> MOTHER='PIPW' X=0 Y=0 Z=+110.237*INCH -   267
C    0.38/2*INCH <===                                                     267
         CALL AGSRESET                                                    267
         AG_MOTHER = 'PIPW'                                               267
         AG_X = 0                                                         267
         AG_Y = 0                                                         267
         AG_Z = +110.237*INCH - 0.38/2*INCH                               267
         AG_TITLE = 'SSCG'                                                267
         AG_EXNAME = 'POSITION'                                           267
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      267
         CALL AXPOSITION                                                  267
C    *                                                                    267
C    Create SSCG :                                                        268
C    Call To Local Procedure SSCG                                         268
         ASSIGN 5420 TO BLOCK_SSCG                                        268
         GO TO  5200                                                      268
5420     CONTINUE                                                         268
C    *                                                                    268
C    Geant POSITION : SSCG ===> MOTHER='PIPW' X=0 Y=0 Z=+110.237*INCH +   269
C    0.38/2*INCH <===                                                     269
         CALL AGSRESET                                                    269
         AG_MOTHER = 'PIPW'                                               269
         AG_X = 0                                                         269
         AG_Y = 0                                                         269
         AG_Z = +110.237*INCH + 0.38/2*INCH                               269
         AG_TITLE = 'SSCG'                                                269
         AG_EXNAME = 'POSITION'                                           269
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      269
         CALL AXPOSITION                                                  269
C    *                                                                    269
5061  CALL AGSPOPB                                                        270
      GO TO BLOCK_PIPW, (5060 ,5070)                                      270
C End of Local Procedure                                                  270
C ----------------------------------------------------------------------  271
C ------------ PIHW                                                       271
C Begin Local Procedure PIHW                                              272
C PIHW THE HOLE INSIDE WEST SECTION OF THE BEAM PIPE                      272
5340  CONTINUE                                                            272
         AG_TITLE='PIHW'                                                  272
         AG_MODULE=MODULE                                                 272
         AG_MARK='V00'                                                    272
         CALL AGSPUSH                                                     272
         AG_MODULE=MODULE                                                 272
C    *                                                                    272
C    Geant MATERIAL : AIR ===> inherited arguments used <===              273
         CALL AGSRESET                                                    273
         AG_TITLE = 'AIR'                                                 273
         AG_EXNAME = 'MATERIAL'                                           273
         AG_PARLIST = 'NONE'                                              273
         CALL AXMATERIAL                                                  273
C    *                                                                    273
C    Geant MATERIAL : VACUUM ===> DENS=PIPG_VACDENS <===                  273
         CALL AGSRESET                                                    274
         AG_DENS = PIPG_VACDENS                                           274
         AG_TITLE = 'VACUUM'                                              274
         AG_EXNAME = 'MATERIAL'                                           274
         AG_PARLIST = 'DENS'                                              274
         CALL AXMATERIAL                                                  274
C    *                                                                    274
C    Geant ATTRIBUTE : PIHW ===> SEEN=0 <===                              274
         CALL AGSRESET                                                    275
         AG_SEEN = 0                                                      275
         AG_TITLE = 'PIHW'                                                275
         AG_EXNAME = 'ATTRIBUTE'                                          275
         AG_PARLIST = 'SEEN'                                              275
         CALL AXATTRIBUTE                                                 275
C    *                                                                    275
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={55.71*INCH, 64.71*  275
C    INCH,79.71*INCH,(159.000)*INCH}, RMN={0,0,0,0} RMX={0.7875*INCH,0.7  275
C    875*INCH,1.5*INCH, 1.5*INCH} <===                                    275
         CALL AGSRESET                                                    279
         AG_PHI1 = 0                                                      279
         AG_DPHI = 360                                                    279
         AG_NZ = 4                                                        279
         AG_ZI(1)=55.71*INCH                                              279
         AG_ZI(2)=64.71*INCH                                              279
         AG_ZI(3)=79.71*INCH                                              279
         AG_ZI(4)=(159.000)*INCH                                          279
         AG_RMN(1)=0                                                      279
         AG_RMN(2)=0                                                      279
         AG_RMN(3)=0                                                      279
         AG_RMN(4)=0                                                      279
         AG_RMX(1)=0.7875*INCH                                            279
         AG_RMX(2)=0.7875*INCH                                            279
         AG_RMX(3)=1.5*INCH                                               279
         AG_RMX(4)=1.5*INCH                                               279
         AG_TITLE = 'PCON'                                                279
         AG_EXNAME = 'SHAPE'                                              279
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           279
         CALL AXSHAPE                                                     279
C    *                                                                    279
5341  CALL AGSPOPB                                                        280
      GO TO BLOCK_PIHW, (5340 ,5350)                                      280
C End of Local Procedure                                                  280
C ----------------------------------------------------------------------  281
C ------------ PALJ                                                       281
C Begin Local Procedure PALJ                                              282
C PALJ THE WEST ALUMINIUM SECTION OF THE BEAM PIPE (WEST TRANSITION TUBE  282
C )                                                                       282
5360  CONTINUE                                                            282
         AG_TITLE='PALJ'                                                  282
         AG_MODULE=MODULE                                                 282
         AG_MARK='V00'                                                    282
         CALL AGSPUSH                                                     282
         AG_MODULE=MODULE                                                 282
C    *                                                                    282
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        283
         CALL AGSRESET                                                    283
         AG_TITLE = 'ALUMINIUM'                                           283
         AG_EXNAME = 'MATERIAL'                                           283
         AG_PARLIST = 'NONE'                                              283
         CALL AXMATERIAL                                                  283
C    *                                                                    283
C    Geant ATTRIBUTE : PALJ ===> SEEN=1 COLO=4 <===                       283
         CALL AGSRESET                                                    284
         AG_SEEN = 1                                                      284
         AG_COLO = 4                                                      284
         AG_TITLE = 'PALJ'                                                284
         AG_EXNAME = 'ATTRIBUTE'                                          284
         AG_PARLIST = 'SEEN_COLO'                                         284
         CALL AXATTRIBUTE                                                 284
C    *                                                                    284
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=8 ZI={55.71*INCH, 56.21*  284
C    INCH,56.21*INCH,64.71*INCH,79.71*INCH, 109.477*INCH,109.477*INCH,(1  284
C    10.237-0.38)*INCH}, RMN={0.7875*INCH,0.7875*INCH,0.7875*INCH, 0.787  284
C    5*INCH,1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH}, RMX={0.7875*INCH+0.587  284
C    5*INCH,0.7875*INCH+0.5875*INCH, 0.7875*INCH+0.049*INCH,0.7875*INCH+  284
C    0.049*INCH, 1.5*INCH+0.049*INCH,1.5*INCH+0.049*INCH, 1.5*INCH+0.812  284
C    5*INCH,1.5*INCH+0.8125*INCH} <===                                    284
         CALL AGSRESET                                                    293
         AG_PHI1 = 0                                                      293
         AG_DPHI = 360                                                    293
         AG_NZ = 8                                                        293
         AG_ZI(1)=55.71*INCH                                              293
         AG_ZI(2)=56.21*INCH                                              293
         AG_ZI(3)=56.21*INCH                                              293
         AG_ZI(4)=64.71*INCH                                              293
         AG_ZI(5)=79.71*INCH                                              293
         AG_ZI(6)=109.477*INCH                                            293
         AG_ZI(7)=109.477*INCH                                            293
         AG_ZI(8)=(110.237-0.38)*INCH                                     293
         AG_RMN(1)=0.7875*INCH                                            293
         AG_RMN(2)=0.7875*INCH                                            293
         AG_RMN(3)=0.7875*INCH                                            293
         AG_RMN(4)=0.7875*INCH                                            293
         AG_RMN(5)=1.5*INCH                                               293
         AG_RMN(6)=1.5*INCH                                               293
         AG_RMN(7)=1.5*INCH                                               293
         AG_RMN(8)=1.5*INCH                                               293
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                293
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                293
         AG_RMX(3)=0.7875*INCH+0.049*INCH                                 293
         AG_RMX(4)=0.7875*INCH+0.049*INCH                                 293
         AG_RMX(5)=1.5*INCH+0.049*INCH                                    293
         AG_RMX(6)=1.5*INCH+0.049*INCH                                    293
         AG_RMX(7)=1.5*INCH+0.8125*INCH                                   293
         AG_RMX(8)=1.5*INCH+0.8125*INCH                                   293
         AG_TITLE = 'PCON'                                                293
         AG_EXNAME = 'SHAPE'                                              293
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           293
         CALL AXSHAPE                                                     293
C    *                                                                    293
5361  CALL AGSPOPB                                                        294
      GO TO BLOCK_PALJ, (5360 ,5370)                                      294
C End of Local Procedure                                                  294
C ----------------------------------------------------------------------  295
C ------------ PALK                                                       295
C Begin Local Procedure PALK                                              296
C PALK THE WEST END ALUMINIUM TUBE OF THE BEAM PIPE                       296
5380  CONTINUE                                                            296
         AG_TITLE='PALK'                                                  296
         AG_MODULE=MODULE                                                 296
         AG_MARK='V00'                                                    296
         CALL AGSPUSH                                                     296
         AG_MODULE=MODULE                                                 296
C    *                                                                    296
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        297
         CALL AGSRESET                                                    297
         AG_TITLE = 'ALUMINIUM'                                           297
         AG_EXNAME = 'MATERIAL'                                           297
         AG_PARLIST = 'NONE'                                              297
         CALL AXMATERIAL                                                  297
C    *                                                                    297
C    Geant ATTRIBUTE : PALK ===> SEEN=1 COLO=4 <===                       297
         CALL AGSRESET                                                    298
         AG_SEEN = 1                                                      298
         AG_COLO = 4                                                      298
         AG_TITLE = 'PALK'                                                298
         AG_EXNAME = 'ATTRIBUTE'                                          298
         AG_PARLIST = 'SEEN_COLO'                                         298
         CALL AXATTRIBUTE                                                 298
C    *                                                                    298
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={(110.237+0.38)*INC  298
C    H, 110.997*INCH,110.997*INCH,158.241*INCH, 158.241*INCH,(159.000-0.  298
C    38)*INCH} RMN={1.5*INCH, 1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH, 1.5*I  298
C    NCH} RMX={1.5*INCH+0.8125*INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.049  298
C    *INCH,1.5*INCH+0.049*INCH, 1.5*INCH+0.8125*INCH,1.5*INCH+0.8125*INC  298
C    H} <===                                                              298
         CALL AGSRESET                                                    305
         AG_PHI1 = 0                                                      305
         AG_DPHI = 360                                                    305
         AG_NZ = 6                                                        305
         AG_ZI(1)=(110.237+0.38)*INCH                                     305
         AG_ZI(2)=110.997*INCH                                            305
         AG_ZI(3)=110.997*INCH                                            305
         AG_ZI(4)=158.241*INCH                                            305
         AG_ZI(5)=158.241*INCH                                            305
         AG_ZI(6)=(159.000-0.38)*INCH                                     305
         AG_RMN(1)=1.5*INCH                                               305
         AG_RMN(2)=1.5*INCH                                               305
         AG_RMN(3)=1.5*INCH                                               305
         AG_RMN(4)=1.5*INCH                                               305
         AG_RMN(5)=1.5*INCH                                               305
         AG_RMN(6)=1.5*INCH                                               305
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   305
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   305
         AG_RMX(3)=1.5*INCH+0.049*INCH                                    305
         AG_RMX(4)=1.5*INCH+0.049*INCH                                    305
         AG_RMX(5)=1.5*INCH+0.8125*INCH                                   305
         AG_RMX(6)=1.5*INCH+0.8125*INCH                                   305
         AG_TITLE = 'PCON'                                                305
         AG_EXNAME = 'SHAPE'                                              305
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           305
         CALL AXSHAPE                                                     305
C    *                                                                    305
5381  CALL AGSPOPB                                                        306
      GO TO BLOCK_PALK, (5380 ,5390)                                      306
C End of Local Procedure                                                  306
C ----------------------------------------------------------------------  307
C ------------ SSCF                                                       307
C Begin Local Procedure SSCF                                              308
C SSCF SS PART OF CONFLAT FLANGE                                          308
5180  CONTINUE                                                            308
         AG_TITLE='SSCF'                                                  308
         AG_MODULE=MODULE                                                 308
         AG_MARK='V00'                                                    308
         CALL AGSPUSH                                                     308
         AG_MODULE=MODULE                                                 308
C    *                                                                    308
C    Geant MATERIAL : STAINLESSSTEEL ===> inherited arguments used <===   309
         CALL AGSRESET                                                    309
         AG_TITLE = 'STAINLESSSTEEL'                                      309
         AG_EXNAME = 'MATERIAL'                                           309
         AG_PARLIST = 'NONE'                                              309
         CALL AXMATERIAL                                                  309
C    *                                                                    309
C    Geant SHAPE : TUBE ===> RMIN=0.7875*INCH RMAX=0.7875*INCH+0.5875*IN  309
C    CH, DZ=0.25*INCH/2 <===                                              309
         CALL AGSRESET                                                    311
         AG_RMIN = 0.7875*INCH                                            311
         AG_RMAX = 0.7875*INCH+0.5875*INCH                                311
         AG_DZ = 0.25*INCH/2                                              311
         AG_TITLE = 'TUBE'                                                311
         AG_EXNAME = 'SHAPE'                                              311
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      311
         CALL AXSHAPE                                                     311
C    *                                                                    311
C    Geant ATTRIBUTE : SSCF ===> SEEN=1 COLO=7 <===                       311
         CALL AGSRESET                                                    312
         AG_SEEN = 1                                                      312
         AG_COLO = 7                                                      312
         AG_TITLE = 'SSCF'                                                312
         AG_EXNAME = 'ATTRIBUTE'                                          312
         AG_PARLIST = 'SEEN_COLO'                                         312
         CALL AXATTRIBUTE                                                 312
C    *                                                                    312
5181  CALL AGSPOPB                                                        313
      GO TO BLOCK_SSCF, (5180 ,5190,5320,5330)                            313
C End of Local Procedure                                                  313
C ----------------------------------------------------------------------  314
C ------------ SSCG                                                       314
C Begin Local Procedure SSCG                                              315
C SSCG SS PART OF CONFLAT FLANGES 3 INCH SECTIONS                         315
5200  CONTINUE                                                            315
         AG_TITLE='SSCG'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
C    Geant MATERIAL : STAINLESSSTEEL ===> inherited arguments used <===   316
         CALL AGSRESET                                                    316
         AG_TITLE = 'STAINLESSSTEEL'                                      316
         AG_EXNAME = 'MATERIAL'                                           316
         AG_PARLIST = 'NONE'                                              316
         CALL AXMATERIAL                                                  316
C    *                                                                    316
C    Geant SHAPE : TUBE ===> RMIN=PIPA_S3INNR RMAX=2.3125*INCH, DZ=0.38*  316
C    INCH/2 <===                                                          316
         CALL AGSRESET                                                    318
         AG_RMIN = PIPA_S3INNR                                            318
         AG_RMAX = 2.3125*INCH                                            318
         AG_DZ = 0.38*INCH/2                                              318
         AG_TITLE = 'TUBE'                                                318
         AG_EXNAME = 'SHAPE'                                              318
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      318
         CALL AXSHAPE                                                     318
C    *                                                                    318
C    Geant ATTRIBUTE : SSCG ===> SEEN=1 COLO=7 <===                       318
         CALL AGSRESET                                                    319
         AG_SEEN = 1                                                      319
         AG_COLO = 7                                                      319
         AG_TITLE = 'SSCG'                                                319
         AG_EXNAME = 'ATTRIBUTE'                                          319
         AG_PARLIST = 'SEEN_COLO'                                         319
         CALL AXATTRIBUTE                                                 319
C    *                                                                    319
5201  CALL AGSPOPB                                                        320
      GO TO BLOCK_SSCG, (5200 ,5210,5220,5230,5400,5410,5420)             320
C End of Local Procedure                                                  320
C ----------------------------------------------------------------------  321
C ------------ PIPA                                                       321
C Begin Local Procedure PIPA                                              322
C PIPA IS THE STAR EAST PIPE MOTHER VOLUME                                322
5080  CONTINUE                                                            322
         AG_TITLE='PIPA'                                                  322
         AG_MODULE=MODULE                                                 322
         AG_MARK='V00'                                                    322
         CALL AGSPUSH                                                     322
         AG_MODULE=MODULE                                                 322
C    *                                                                    322
C    Geant MATERIAL : AIR ===> inherited arguments used <===              323
         CALL AGSRESET                                                    323
         AG_TITLE = 'AIR'                                                 323
         AG_EXNAME = 'MATERIAL'                                           323
         AG_PARLIST = 'NONE'                                              323
         CALL AXMATERIAL                                                  323
C    *                                                                    323
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           323
         CALL AGSRESET                                                    324
         AG_TITLE = 'STANDARD'                                            324
         AG_EXNAME = 'MEDIUM'                                             324
         AG_PARLIST = 'NONE'                                              324
         CALL AXMEDIUM                                                    324
C    *                                                                    324
C    Geant ATTRIBUTE : PIPA ===> SEEN=1 COLO=7 <===                       324
         CALL AGSRESET                                                    325
         AG_SEEN = 1                                                      325
         AG_COLO = 7                                                      325
         AG_TITLE = 'PIPA'                                                325
         AG_EXNAME = 'ATTRIBUTE'                                          325
         AG_PARLIST = 'SEEN_COLO'                                         325
         CALL AXATTRIBUTE                                                 325
C    *                                                                    325
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=2 ZI={Z4,Z1}, RMN={0,0}   325
C    RMX={R2,R2} <===                                                     325
         CALL AGSRESET                                                    327
         AG_PHI1 = 0                                                      327
         AG_DPHI = 360                                                    327
         AG_NZ = 2                                                        327
         AG_ZI(1)=Z4                                                      327
         AG_ZI(2)=Z1                                                      327
         AG_RMN(1)=0                                                      327
         AG_RMN(2)=0                                                      327
         AG_RMX(1)=R2                                                     327
         AG_RMX(2)=R2                                                     327
         AG_TITLE = 'PCON'                                                327
         AG_EXNAME = 'SHAPE'                                              327
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           327
         CALL AXSHAPE                                                     327
C    *                                                                    327
C    Create PIPB :                                                        328
C    Call To Local Procedure PIPB                                         328
         ASSIGN 5440 TO BLOCK_PIPB                                        328
         GO TO  5430                                                      328
5440     CONTINUE                                                         328
C    *                                                                    328
C    Geant POSITION : PIPB ===> MOTHER='PIPA' Z=(Z1+Z2)/2 <===            329
         CALL AGSRESET                                                    329
         AG_MOTHER = 'PIPA'                                               329
         AG_Z = (Z1+Z2)/2                                                 329
         AG_TITLE = 'PIPB'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'MOTHER_Z'                                          329
         CALL AXPOSITION                                                  329
C    *                                                                    329
C    Create PIPC :                                                        330
C    Call To Local Procedure PIPC                                         330
         ASSIGN 5460 TO BLOCK_PIPC                                        330
         GO TO  5450                                                      330
5460     CONTINUE                                                         330
C    *                                                                    330
C    Geant POSITION : PIPC ===> MOTHER='PIPA' <===                        331
         CALL AGSRESET                                                    331
         AG_MOTHER = 'PIPA'                                               331
         AG_TITLE = 'PIPC'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'MOTHER'                                            331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Create PRIS :                                                        332
C    Call To Local Procedure PRIS                                         332
         ASSIGN 5480 TO BLOCK_PRIS                                        332
         GO TO  5470                                                      332
5480     CONTINUE                                                         332
C    *                                                                    332
C    Geant POSITION : PRIS ===> Z=(Z1+Z2)/2 <===                          333
         CALL AGSRESET                                                    333
         AG_Z = (Z1+Z2)/2                                                 333
         AG_TITLE = 'PRIS'                                                333
         AG_EXNAME = 'POSITION'                                           333
         AG_PARLIST = 'Z'                                                 333
         CALL AXPOSITION                                                  333
C    *                                                                    333
C    Create PFLO :                                                        334
C    Call To Local Procedure PFLO                                         334
         ASSIGN 5500 TO BLOCK_PFLO                                        334
         GO TO  5490                                                      334
5500     CONTINUE                                                         334
C    *                                                                    334
C    Geant POSITION : PFLO ===> Z=Z1-1.0 <===                             335
         CALL AGSRESET                                                    335
         AG_Z = Z1-1.0                                                    335
         AG_TITLE = 'PFLO'                                                335
         AG_EXNAME = 'POSITION'                                           335
         AG_PARLIST = 'Z'                                                 335
         CALL AXPOSITION                                                  335
C    *                                                                    335
C    Create PFLT :                                                        336
C    Call To Local Procedure PFLT                                         336
         ASSIGN 5520 TO BLOCK_PFLT                                        336
         GO TO  5510                                                      336
5520     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : PFLT ===> Z=Z2 <===                                 337
         CALL AGSRESET                                                    337
         AG_Z = Z2                                                        337
         AG_TITLE = 'PFLT'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'Z'                                                 337
         CALL AXPOSITION                                                  337
C    *                                                                    337
5081  CALL AGSPOPB                                                        338
      GO TO BLOCK_PIPA, (5080 ,5090)                                      338
C End of Local Procedure                                                  338
C ----------------------------------------------------------------------  339
C ------------ PIPB                                                       339
C Begin Local Procedure PIPB                                              340
C PIPB IS STEEL PIPE OF THE BELLOW SECTION                                340
5430  CONTINUE                                                            340
         AG_TITLE='PIPB'                                                  340
         AG_MODULE=MODULE                                                 340
         AG_MARK='V00'                                                    340
         CALL AGSPUSH                                                     340
         AG_MODULE=MODULE                                                 340
C    *                                                                    340
C    Geant MATERIAL : IRON ===> inherited arguments used <===             341
         CALL AGSRESET                                                    341
         AG_TITLE = 'IRON'                                                341
         AG_EXNAME = 'MATERIAL'                                           341
         AG_PARLIST = 'NONE'                                              341
         CALL AXMATERIAL                                                  341
C    *                                                                    341
C    Geant ATTRIBUTE : PIPI ===> SEEN=1 COLO=2 <===                       341
         CALL AGSRESET                                                    342
         AG_SEEN = 1                                                      342
         AG_COLO = 2                                                      342
         AG_TITLE = 'PIPI'                                                342
         AG_EXNAME = 'ATTRIBUTE'                                          342
         AG_PARLIST = 'SEEN_COLO'                                         342
         CALL AXATTRIBUTE                                                 342
C    *                                                                    342
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPA_S2OUTR DZ=PIPA_S2LENG <===  342
         CALL AGSRESET                                                    343
         AG_RMIN = 0                                                      343
         AG_RMAX = PIPA_S2OUTR                                            343
         AG_DZ = PIPA_S2LENG                                              343
         AG_TITLE = 'TUBE'                                                343
         AG_EXNAME = 'SHAPE'                                              343
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      343
         CALL AXSHAPE                                                     343
C    *                                                                    343
C    Create PVPB :                                                        344
C    Call To Local Procedure PVPB                                         344
         ASSIGN 5540 TO BLOCK_PVPB                                        344
         GO TO  5530                                                      344
5540     CONTINUE                                                         344
C    *                                                                    344
C    Geant POSITION : PVPB ===> X=0 Y=0 Z=0 <===                          345
         CALL AGSRESET                                                    345
         AG_X = 0                                                         345
         AG_Y = 0                                                         345
         AG_Z = 0                                                         345
         AG_TITLE = 'PVPB'                                                345
         AG_EXNAME = 'POSITION'                                           345
         AG_PARLIST = 'X_Y_Z'                                             345
         CALL AXPOSITION                                                  345
C    *                                                                    345
5431  CALL AGSPOPB                                                        346
      GO TO BLOCK_PIPB, (5430 ,5440)                                      346
C End of Local Procedure                                                  346
C ----------------------------------------------------------------------  347
C ------------ PVPB                                                       347
C Begin Local Procedure PVPB                                              348
C PVPB IS ITS CAVITY                                                      348
5530  CONTINUE                                                            348
         AG_TITLE='PVPB'                                                  348
         AG_MODULE=MODULE                                                 348
         AG_MARK='V00'                                                    348
         CALL AGSPUSH                                                     348
         AG_MODULE=MODULE                                                 348
C    *                                                                    348
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           349
         CALL AGSRESET                                                    349
         AG_TITLE = 'VACUUM'                                              349
         AG_EXNAME = 'MATERIAL'                                           349
         AG_PARLIST = 'NONE'                                              349
         CALL AXMATERIAL                                                  349
C    *                                                                    349
C    Geant SHAPE : TUBE ===> RMAX=PIPA_S2INNR <===                        349
         CALL AGSRESET                                                    350
         AG_RMAX = PIPA_S2INNR                                            350
         AG_TITLE = 'TUBE'                                                350
         AG_EXNAME = 'SHAPE'                                              350
         AG_PARLIST = 'RMAX'                                              350
         CALL AXSHAPE                                                     350
C    *                                                                    350
C    Geant ATTRIBUTE : PVPB ===> SEEN=1 COLO=2 <===                       350
         CALL AGSRESET                                                    351
         AG_SEEN = 1                                                      351
         AG_COLO = 2                                                      351
         AG_TITLE = 'PVPB'                                                351
         AG_EXNAME = 'ATTRIBUTE'                                          351
         AG_PARLIST = 'SEEN_COLO'                                         351
         CALL AXATTRIBUTE                                                 351
C    *                                                                    351
5531  CALL AGSPOPB                                                        352
      GO TO BLOCK_PVPB, (5530 ,5540)                                      352
C End of Local Procedure                                                  352
C ----------------------------------------------------------------------  353
C ------------ PIPC                                                       353
C Begin Local Procedure PIPC                                              354
C PIPC CONE AND LARGE DIAMETER BEAMPIPE                                   354
5450  CONTINUE                                                            354
         AG_TITLE='PIPC'                                                  354
         AG_MODULE=MODULE                                                 354
         AG_MARK='V00'                                                    354
         CALL AGSPUSH                                                     354
         AG_MODULE=MODULE                                                 354
C    *                                                                    354
C    Geant MATERIAL : IRON ===> inherited arguments used <===             355
         CALL AGSRESET                                                    355
         AG_TITLE = 'IRON'                                                355
         AG_EXNAME = 'MATERIAL'                                           355
         AG_PARLIST = 'NONE'                                              355
         CALL AXMATERIAL                                                  355
C    *                                                                    355
C    Geant ATTRIBUTE : PIPC ===> SEEN=1 COLO=2 <===                       355
         CALL AGSRESET                                                    356
         AG_SEEN = 1                                                      356
         AG_COLO = 2                                                      356
         AG_TITLE = 'PIPC'                                                356
         AG_EXNAME = 'ATTRIBUTE'                                          356
         AG_PARLIST = 'SEEN_COLO'                                         356
         CALL AXATTRIBUTE                                                 356
C    *                                                                    356
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={Z4,Z3, Z2-PIPA_S3L  356
C    ENG*2,Z2} RMN={0,0,0,0} RMX={PIPA_S4OUTR, PIPA_S4OUTR,PIPA_S3OUTR,P  356
C    IPA_S3OUTR} <===                                                     356
         CALL AGSRESET                                                    359
         AG_PHI1 = 0                                                      359
         AG_DPHI = 360                                                    359
         AG_NZ = 4                                                        359
         AG_ZI(1)=Z4                                                      359
         AG_ZI(2)=Z3                                                      359
         AG_ZI(3)=Z2-PIPA_S3LENG*2                                        359
         AG_ZI(4)=Z2                                                      359
         AG_RMN(1)=0                                                      359
         AG_RMN(2)=0                                                      359
         AG_RMN(3)=0                                                      359
         AG_RMN(4)=0                                                      359
         AG_RMX(1)=PIPA_S4OUTR                                            359
         AG_RMX(2)=PIPA_S4OUTR                                            359
         AG_RMX(3)=PIPA_S3OUTR                                            359
         AG_RMX(4)=PIPA_S3OUTR                                            359
         AG_TITLE = 'PCON'                                                359
         AG_EXNAME = 'SHAPE'                                              359
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           359
         CALL AXSHAPE                                                     359
C    *                                                                    359
C    Create PVPC :                                                        360
C    Call To Local Procedure PVPC                                         360
         ASSIGN 5560 TO BLOCK_PVPC                                        360
         GO TO  5550                                                      360
5560     CONTINUE                                                         360
C    *                                                                    360
C    Geant POSITION : PVPC ===> MOTHER='PIPC' <===                        361
         CALL AGSRESET                                                    361
         AG_MOTHER = 'PIPC'                                               361
         AG_TITLE = 'PVPC'                                                361
         AG_EXNAME = 'POSITION'                                           361
         AG_PARLIST = 'MOTHER'                                            361
         CALL AXPOSITION                                                  361
C    *                                                                    361
5451  CALL AGSPOPB                                                        362
      GO TO BLOCK_PIPC, (5450 ,5460)                                      362
C End of Local Procedure                                                  362
C ----------------------------------------------------------------------  363
C ------------ PVPC                                                       363
C Begin Local Procedure PVPC                                              364
C PVPC CONE AND LARGE DIAMETER BEAMPIPE                                   364
5550  CONTINUE                                                            364
         AG_TITLE='PVPC'                                                  364
         AG_MODULE=MODULE                                                 364
         AG_MARK='V00'                                                    364
         CALL AGSPUSH                                                     364
         AG_MODULE=MODULE                                                 364
C    *                                                                    364
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           365
         CALL AGSRESET                                                    365
         AG_TITLE = 'VACUUM'                                              365
         AG_EXNAME = 'MATERIAL'                                           365
         AG_PARLIST = 'NONE'                                              365
         CALL AXMATERIAL                                                  365
C    *                                                                    365
C    Geant ATTRIBUTE : PIPC ===> SEEN=1 COLO=2 <===                       365
         CALL AGSRESET                                                    366
         AG_SEEN = 1                                                      366
         AG_COLO = 2                                                      366
         AG_TITLE = 'PIPC'                                                366
         AG_EXNAME = 'ATTRIBUTE'                                          366
         AG_PARLIST = 'SEEN_COLO'                                         366
         CALL AXATTRIBUTE                                                 366
C    *                                                                    366
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={Z4,Z3, Z2-PIPA_S3L  366
C    ENG*2,Z2} RMN={0,0,0,0} RMX={PIPA_S4INNR, PIPA_S4INNR,PIPA_S3INNR,P  366
C    IPA_S3INNR} <===                                                     366
         CALL AGSRESET                                                    369
         AG_PHI1 = 0                                                      369
         AG_DPHI = 360                                                    369
         AG_NZ = 4                                                        369
         AG_ZI(1)=Z4                                                      369
         AG_ZI(2)=Z3                                                      369
         AG_ZI(3)=Z2-PIPA_S3LENG*2                                        369
         AG_ZI(4)=Z2                                                      369
         AG_RMN(1)=0                                                      369
         AG_RMN(2)=0                                                      369
         AG_RMN(3)=0                                                      369
         AG_RMN(4)=0                                                      369
         AG_RMX(1)=PIPA_S4INNR                                            369
         AG_RMX(2)=PIPA_S4INNR                                            369
         AG_RMX(3)=PIPA_S3INNR                                            369
         AG_RMX(4)=PIPA_S3INNR                                            369
         AG_TITLE = 'PCON'                                                369
         AG_EXNAME = 'SHAPE'                                              369
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           369
         CALL AXSHAPE                                                     369
C    *                                                                    369
5551  CALL AGSPOPB                                                        370
      GO TO BLOCK_PVPC, (5550 ,5560)                                      370
C End of Local Procedure                                                  370
C ----------------------------------------------------------------------  371
C ------------ PRIS                                                       371
C Begin Local Procedure PRIS                                              372
C PRIS IS THE BELLOW STEEL RIB SET                                        372
5470  CONTINUE                                                            372
         AG_TITLE='PRIS'                                                  372
         AG_MODULE=MODULE                                                 372
         AG_MARK='V00'                                                    372
         CALL AGSPUSH                                                     372
         AG_MODULE=MODULE                                                 372
C    *                                                                    372
C    Geant MATERIAL : AIR ===> inherited arguments used <===              373
         CALL AGSRESET                                                    373
         AG_TITLE = 'AIR'                                                 373
         AG_EXNAME = 'MATERIAL'                                           373
         AG_PARLIST = 'NONE'                                              373
         CALL AXMATERIAL                                                  373
C    *                                                                    373
C    Geant ATTRIBUTE : PRIB ===> SEEN=0 COLO=2 <===                       373
         CALL AGSRESET                                                    374
         AG_SEEN = 0                                                      374
         AG_COLO = 2                                                      374
         AG_TITLE = 'PRIB'                                                374
         AG_EXNAME = 'ATTRIBUTE'                                          374
         AG_PARLIST = 'SEEN_COLO'                                         374
         CALL AXATTRIBUTE                                                 374
C    *                                                                    374
C    Geant SHAPE : TUBE ===> RMIN=PIPA_S2OUTR RMAX=PIPA_RIBOUTR, DZ=PIPA  374
C    _RIBNUM*PIPA_RIBSPA/2. <===                                          374
         CALL AGSRESET                                                    376
         AG_RMIN = PIPA_S2OUTR                                            376
         AG_RMAX = PIPA_RIBOUTR                                           376
         AG_DZ = PIPA_RIBNUM*PIPA_RIBSPA/2.                               376
         AG_TITLE = 'TUBE'                                                376
         AG_EXNAME = 'SHAPE'                                              376
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      376
         CALL AXSHAPE                                                     376
C    *                                                                    376
C    Create PRID :                                                        377
C    Call To Local Procedure PRID                                         377
         ASSIGN 5580 TO BLOCK_PRID                                        377
         GO TO  5570                                                      377
5580     CONTINUE                                                         377
C    *                                                                    377
C    Geant POSITION : PRID ===> inherited arguments used <===             378
         CALL AGSRESET                                                    378
         AG_TITLE = 'PRID'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'NONE'                                              378
         CALL AXPOSITION                                                  378
C    *                                                                    378
5471  CALL AGSPOPB                                                        379
      GO TO BLOCK_PRIS, (5470 ,5480)                                      379
C End of Local Procedure                                                  379
C ----------------------------------------------------------------------  380
C ------------ PRID                                                       380
C Begin Local Procedure PRID                                              381
C PRID IS A RIB SECTION                                                   381
5570  CONTINUE                                                            381
         AG_TITLE='PRID'                                                  381
         AG_MODULE=MODULE                                                 381
         AG_MARK='V00'                                                    381
         CALL AGSPUSH                                                     381
         AG_MODULE=MODULE                                                 381
C    *                                                                    381
C    Geant SHAPE : DIVISION ===> NDIV=PIPA_RIBNUM IAXIS=3 <===            382
         CALL AGSRESET                                                    382
         AG_NDIV = PIPA_RIBNUM                                            382
         AG_IAXIS = 3                                                     382
         AG_TITLE = 'DIVISION'                                            382
         AG_EXNAME = 'SHAPE'                                              382
         AG_PARLIST = 'NDIV_IAXIS'                                        382
         CALL AXSHAPE                                                     382
C    *                                                                    382
C    Create PRIB :                                                        383
C    Call To Local Procedure PRIB                                         383
         ASSIGN 5600 TO BLOCK_PRIB                                        383
         GO TO  5590                                                      383
5600     CONTINUE                                                         383
C    *                                                                    383
C    Geant POSITION : PRIB ===> inherited arguments used <===             384
         CALL AGSRESET                                                    384
         AG_TITLE = 'PRIB'                                                384
         AG_EXNAME = 'POSITION'                                           384
         AG_PARLIST = 'NONE'                                              384
         CALL AXPOSITION                                                  384
C    *                                                                    384
5571  CALL AGSPOPB                                                        385
      GO TO BLOCK_PRID, (5570 ,5580)                                      385
C End of Local Procedure                                                  385
C ----------------------------------------------------------------------  386
C ------------ PRIB                                                       386
C Begin Local Procedure PRIB                                              387
C PRIB IS A RIB OF STEEL BELLOWS                                          387
5590  CONTINUE                                                            387
         AG_TITLE='PRIB'                                                  387
         AG_MODULE=MODULE                                                 387
         AG_MARK='V00'                                                    387
         CALL AGSPUSH                                                     387
         AG_MODULE=MODULE                                                 387
C    *                                                                    387
C    Geant MATERIAL : IRON ===> inherited arguments used <===             388
         CALL AGSRESET                                                    388
         AG_TITLE = 'IRON'                                                388
         AG_EXNAME = 'MATERIAL'                                           388
         AG_PARLIST = 'NONE'                                              388
         CALL AXMATERIAL                                                  388
C    *                                                                    388
C    Geant ATTRIBUTE : PRIB ===> SEEN=1 COLO=4 <===                       388
         CALL AGSRESET                                                    389
         AG_SEEN = 1                                                      389
         AG_COLO = 4                                                      389
         AG_TITLE = 'PRIB'                                                389
         AG_EXNAME = 'ATTRIBUTE'                                          389
         AG_PARLIST = 'SEEN_COLO'                                         389
         CALL AXATTRIBUTE                                                 389
C    *                                                                    389
C    Geant SHAPE : TUBE ===> DZ=PIPA_RIBTHK <===                          389
         CALL AGSRESET                                                    390
         AG_DZ = PIPA_RIBTHK                                              390
         AG_TITLE = 'TUBE'                                                390
         AG_EXNAME = 'SHAPE'                                              390
         AG_PARLIST = 'DZ'                                                390
         CALL AXSHAPE                                                     390
C    *                                                                    390
5591  CALL AGSPOPB                                                        391
      GO TO BLOCK_PRIB, (5590 ,5600)                                      391
C End of Local Procedure                                                  391
C ----------------------------------------------------------------------  392
C ------------ PIWA                                                       392
C Begin Local Procedure PIWA                                              393
C PIWA IS THE STAR EAST PIPE MOTHER VOLUME                                393
5100  CONTINUE                                                            393
         AG_TITLE='PIWA'                                                  393
         AG_MODULE=MODULE                                                 393
         AG_MARK='V00'                                                    393
         CALL AGSPUSH                                                     393
         AG_MODULE=MODULE                                                 393
C    *                                                                    393
C    Geant MATERIAL : AIR ===> inherited arguments used <===              394
         CALL AGSRESET                                                    394
         AG_TITLE = 'AIR'                                                 394
         AG_EXNAME = 'MATERIAL'                                           394
         AG_PARLIST = 'NONE'                                              394
         CALL AXMATERIAL                                                  394
C    *                                                                    394
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           394
         CALL AGSRESET                                                    395
         AG_TITLE = 'STANDARD'                                            395
         AG_EXNAME = 'MEDIUM'                                             395
         AG_PARLIST = 'NONE'                                              395
         CALL AXMEDIUM                                                    395
C    *                                                                    395
C    Geant ATTRIBUTE : PIWA ===> SEEN=1 COLO=7 <===                       395
         CALL AGSRESET                                                    396
         AG_SEEN = 1                                                      396
         AG_COLO = 7                                                      396
         AG_TITLE = 'PIWA'                                                396
         AG_EXNAME = 'ATTRIBUTE'                                          396
         AG_PARLIST = 'SEEN_COLO'                                         396
         CALL AXATTRIBUTE                                                 396
C    *                                                                    396
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=2 ZI={Z1W, Z4W} RMN={0,0  396
C    } RMX={R2,R2} <===                                                   396
         CALL AGSRESET                                                    398
         AG_PHI1 = 0                                                      398
         AG_DPHI = 360                                                    398
         AG_NZ = 2                                                        398
         AG_ZI(1)=Z1W                                                     398
         AG_ZI(2)=Z4W                                                     398
         AG_RMN(1)=0                                                      398
         AG_RMN(2)=0                                                      398
         AG_RMX(1)=R2                                                     398
         AG_RMX(2)=R2                                                     398
         AG_TITLE = 'PCON'                                                398
         AG_EXNAME = 'SHAPE'                                              398
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           398
         CALL AXSHAPE                                                     398
C    *                                                                    398
C    Create PIWB :                                                        399
C    Call To Local Procedure PIWB                                         399
         ASSIGN 5620 TO BLOCK_PIWB                                        399
         GO TO  5610                                                      399
5620     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : PIWB ===> MOTHER='PIWA' Z=(Z1W+Z2W)/2 <===          400
         CALL AGSRESET                                                    400
         AG_MOTHER = 'PIWA'                                               400
         AG_Z = (Z1W+Z2W)/2                                               400
         AG_TITLE = 'PIWB'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'MOTHER_Z'                                          400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Create PIWC :                                                        401
C    Call To Local Procedure PIWC                                         401
         ASSIGN 5640 TO BLOCK_PIWC                                        401
         GO TO  5630                                                      401
5640     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : PIWC ===> MOTHER='PIWA' <===                        402
         CALL AGSRESET                                                    402
         AG_MOTHER = 'PIWA'                                               402
         AG_TITLE = 'PIWC'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'MOTHER'                                            402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Create PRWS :                                                        403
C    Call To Local Procedure PRWS                                         403
         ASSIGN 5660 TO BLOCK_PRWS                                        403
         GO TO  5650                                                      403
5660     CONTINUE                                                         403
C    *                                                                    403
C    Geant POSITION : PRWS ===> Z=(Z1W+Z2W)/2 <===                        404
         CALL AGSRESET                                                    404
         AG_Z = (Z1W+Z2W)/2                                               404
         AG_TITLE = 'PRWS'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'Z'                                                 404
         CALL AXPOSITION                                                  404
C    *                                                                    404
C    Create PFLO :                                                        405
C    Call To Local Procedure PFLO                                         405
         ASSIGN 5670 TO BLOCK_PFLO                                        405
         GO TO  5490                                                      405
5670     CONTINUE                                                         405
C    *                                                                    405
C    Geant POSITION : PFLO ===> Z=Z1W+1.0 <===                            406
         CALL AGSRESET                                                    406
         AG_Z = Z1W+1.0                                                   406
         AG_TITLE = 'PFLO'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'Z'                                                 406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Create PFLT :                                                        407
C    Call To Local Procedure PFLT                                         407
         ASSIGN 5680 TO BLOCK_PFLT                                        407
         GO TO  5510                                                      407
5680     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : PFLT ===> Z=Z2W <===                                408
         CALL AGSRESET                                                    408
         AG_Z = Z2W                                                       408
         AG_TITLE = 'PFLT'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'Z'                                                 408
         CALL AXPOSITION                                                  408
C    *                                                                    408
5101  CALL AGSPOPB                                                        409
      GO TO BLOCK_PIWA, (5100 ,5110)                                      409
C End of Local Procedure                                                  409
C ----------------------------------------------------------------------  410
C ------------ PIWB                                                       410
C Begin Local Procedure PIWB                                              411
C PIWB IS STEEL PIPE OF THE BELLOW SECTION                                411
5610  CONTINUE                                                            411
         AG_TITLE='PIWB'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
C    Geant MATERIAL : IRON ===> inherited arguments used <===             412
         CALL AGSRESET                                                    412
         AG_TITLE = 'IRON'                                                412
         AG_EXNAME = 'MATERIAL'                                           412
         AG_PARLIST = 'NONE'                                              412
         CALL AXMATERIAL                                                  412
C    *                                                                    412
C    Geant ATTRIBUTE : PIPI ===> SEEN=1 COLO=2 <===                       412
         CALL AGSRESET                                                    413
         AG_SEEN = 1                                                      413
         AG_COLO = 2                                                      413
         AG_TITLE = 'PIPI'                                                413
         AG_EXNAME = 'ATTRIBUTE'                                          413
         AG_PARLIST = 'SEEN_COLO'                                         413
         CALL AXATTRIBUTE                                                 413
C    *                                                                    413
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIWA_S2OUTR DZ=PIWA_S2LENG <===  413
         CALL AGSRESET                                                    414
         AG_RMIN = 0                                                      414
         AG_RMAX = PIWA_S2OUTR                                            414
         AG_DZ = PIWA_S2LENG                                              414
         AG_TITLE = 'TUBE'                                                414
         AG_EXNAME = 'SHAPE'                                              414
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      414
         CALL AXSHAPE                                                     414
C    *                                                                    414
C    Create PVWB :                                                        415
C    Call To Local Procedure PVWB                                         415
         ASSIGN 5700 TO BLOCK_PVWB                                        415
         GO TO  5690                                                      415
5700     CONTINUE                                                         415
C    *                                                                    415
C    Geant POSITION : PVWB ===> X=0 Y=0 Z=0 <===                          416
         CALL AGSRESET                                                    416
         AG_X = 0                                                         416
         AG_Y = 0                                                         416
         AG_Z = 0                                                         416
         AG_TITLE = 'PVWB'                                                416
         AG_EXNAME = 'POSITION'                                           416
         AG_PARLIST = 'X_Y_Z'                                             416
         CALL AXPOSITION                                                  416
C    *                                                                    416
5611  CALL AGSPOPB                                                        417
      GO TO BLOCK_PIWB, (5610 ,5620)                                      417
C End of Local Procedure                                                  417
C ----------------------------------------------------------------------  418
C ------------ PVWB                                                       418
C Begin Local Procedure PVWB                                              419
C PVWB IS ITS CAVITY                                                      419
5690  CONTINUE                                                            419
         AG_TITLE='PVWB'                                                  419
         AG_MODULE=MODULE                                                 419
         AG_MARK='V00'                                                    419
         CALL AGSPUSH                                                     419
         AG_MODULE=MODULE                                                 419
C    *                                                                    419
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           420
         CALL AGSRESET                                                    420
         AG_TITLE = 'VACUUM'                                              420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant SHAPE : TUBE ===> RMAX=PIWA_S2INNR <===                        420
         CALL AGSRESET                                                    421
         AG_RMAX = PIWA_S2INNR                                            421
         AG_TITLE = 'TUBE'                                                421
         AG_EXNAME = 'SHAPE'                                              421
         AG_PARLIST = 'RMAX'                                              421
         CALL AXSHAPE                                                     421
C    *                                                                    421
C    Geant ATTRIBUTE : PVWB ===> SEEN=1 COLO=2 <===                       421
         CALL AGSRESET                                                    422
         AG_SEEN = 1                                                      422
         AG_COLO = 2                                                      422
         AG_TITLE = 'PVWB'                                                422
         AG_EXNAME = 'ATTRIBUTE'                                          422
         AG_PARLIST = 'SEEN_COLO'                                         422
         CALL AXATTRIBUTE                                                 422
C    *                                                                    422
5691  CALL AGSPOPB                                                        423
      GO TO BLOCK_PVWB, (5690 ,5700)                                      423
C End of Local Procedure                                                  423
C ----------------------------------------------------------------------  424
C ------------ PIWC                                                       424
C Begin Local Procedure PIWC                                              425
C PIWC CONE AND LARGE DIAMETER BEAMPIPE                                   425
5630  CONTINUE                                                            425
         AG_TITLE='PIWC'                                                  425
         AG_MODULE=MODULE                                                 425
         AG_MARK='V00'                                                    425
         CALL AGSPUSH                                                     425
         AG_MODULE=MODULE                                                 425
C    *                                                                    425
C    Geant MATERIAL : IRON ===> inherited arguments used <===             426
         CALL AGSRESET                                                    426
         AG_TITLE = 'IRON'                                                426
         AG_EXNAME = 'MATERIAL'                                           426
         AG_PARLIST = 'NONE'                                              426
         CALL AXMATERIAL                                                  426
C    *                                                                    426
C    Geant ATTRIBUTE : PIWC ===> SEEN=1 COLO=2 <===                       426
         CALL AGSRESET                                                    427
         AG_SEEN = 1                                                      427
         AG_COLO = 2                                                      427
         AG_TITLE = 'PIWC'                                                427
         AG_EXNAME = 'ATTRIBUTE'                                          427
         AG_PARLIST = 'SEEN_COLO'                                         427
         CALL AXATTRIBUTE                                                 427
C    *                                                                    427
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={Z2W, Z2W+PIWA_S3LE  427
C    NG*2,Z3W,Z4W} RMN={0,0,0,0} RMX={PIWA_S3OUTR, PIWA_S3OUTR,PIWA_S4OU  427
C    TR,PIWA_S4OUTR} <===                                                 427
         CALL AGSRESET                                                    430
         AG_PHI1 = 0                                                      430
         AG_DPHI = 360                                                    430
         AG_NZ = 4                                                        430
         AG_ZI(1)=Z2W                                                     430
         AG_ZI(2)=Z2W+PIWA_S3LENG*2                                       430
         AG_ZI(3)=Z3W                                                     430
         AG_ZI(4)=Z4W                                                     430
         AG_RMN(1)=0                                                      430
         AG_RMN(2)=0                                                      430
         AG_RMN(3)=0                                                      430
         AG_RMN(4)=0                                                      430
         AG_RMX(1)=PIWA_S3OUTR                                            430
         AG_RMX(2)=PIWA_S3OUTR                                            430
         AG_RMX(3)=PIWA_S4OUTR                                            430
         AG_RMX(4)=PIWA_S4OUTR                                            430
         AG_TITLE = 'PCON'                                                430
         AG_EXNAME = 'SHAPE'                                              430
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           430
         CALL AXSHAPE                                                     430
C    *                                                                    430
C    Create PVWC :                                                        431
C    Call To Local Procedure PVWC                                         431
         ASSIGN 5720 TO BLOCK_PVWC                                        431
         GO TO  5710                                                      431
5720     CONTINUE                                                         431
C    *                                                                    431
C    Geant POSITION : PVWC ===> MOTHER='PIWC' <===                        432
         CALL AGSRESET                                                    432
         AG_MOTHER = 'PIWC'                                               432
         AG_TITLE = 'PVWC'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'MOTHER'                                            432
         CALL AXPOSITION                                                  432
C    *                                                                    432
5631  CALL AGSPOPB                                                        433
      GO TO BLOCK_PIWC, (5630 ,5640)                                      433
C End of Local Procedure                                                  433
C ----------------------------------------------------------------------  434
C ------------ PVWC                                                       434
C Begin Local Procedure PVWC                                              435
C PVWC CONE AND LARGE DIAMETER BEAMPIPE                                   435
5710  CONTINUE                                                            435
         AG_TITLE='PVWC'                                                  435
         AG_MODULE=MODULE                                                 435
         AG_MARK='V00'                                                    435
         CALL AGSPUSH                                                     435
         AG_MODULE=MODULE                                                 435
C    *                                                                    435
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           436
         CALL AGSRESET                                                    436
         AG_TITLE = 'VACUUM'                                              436
         AG_EXNAME = 'MATERIAL'                                           436
         AG_PARLIST = 'NONE'                                              436
         CALL AXMATERIAL                                                  436
C    *                                                                    436
C    Geant ATTRIBUTE : PIWC ===> SEEN=1 COLO=2 <===                       436
         CALL AGSRESET                                                    437
         AG_SEEN = 1                                                      437
         AG_COLO = 2                                                      437
         AG_TITLE = 'PIWC'                                                437
         AG_EXNAME = 'ATTRIBUTE'                                          437
         AG_PARLIST = 'SEEN_COLO'                                         437
         CALL AXATTRIBUTE                                                 437
C    *                                                                    437
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={Z2W, Z2W+PIWA_S3LE  437
C    NG*2,Z3W,Z4W} RMN={0,0,0,0} RMX={PIWA_S3INNR, PIWA_S3INNR,PIWA_S4IN  437
C    NR,PIWA_S4INNR} <===                                                 437
         CALL AGSRESET                                                    440
         AG_PHI1 = 0                                                      440
         AG_DPHI = 360                                                    440
         AG_NZ = 4                                                        440
         AG_ZI(1)=Z2W                                                     440
         AG_ZI(2)=Z2W+PIWA_S3LENG*2                                       440
         AG_ZI(3)=Z3W                                                     440
         AG_ZI(4)=Z4W                                                     440
         AG_RMN(1)=0                                                      440
         AG_RMN(2)=0                                                      440
         AG_RMN(3)=0                                                      440
         AG_RMN(4)=0                                                      440
         AG_RMX(1)=PIWA_S3INNR                                            440
         AG_RMX(2)=PIWA_S3INNR                                            440
         AG_RMX(3)=PIWA_S4INNR                                            440
         AG_RMX(4)=PIWA_S4INNR                                            440
         AG_TITLE = 'PCON'                                                440
         AG_EXNAME = 'SHAPE'                                              440
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           440
         CALL AXSHAPE                                                     440
C    *                                                                    440
5711  CALL AGSPOPB                                                        441
      GO TO BLOCK_PVWC, (5710 ,5720)                                      441
C End of Local Procedure                                                  441
C ----------------------------------------------------------------------  442
C ------------ PRWS                                                       442
C Begin Local Procedure PRWS                                              443
C PRWS IS THE BELLOW STEEL RIB SET                                        443
5650  CONTINUE                                                            443
         AG_TITLE='PRWS'                                                  443
         AG_MODULE=MODULE                                                 443
         AG_MARK='V00'                                                    443
         CALL AGSPUSH                                                     443
         AG_MODULE=MODULE                                                 443
C    *                                                                    443
C    Geant MATERIAL : AIR ===> inherited arguments used <===              444
         CALL AGSRESET                                                    444
         AG_TITLE = 'AIR'                                                 444
         AG_EXNAME = 'MATERIAL'                                           444
         AG_PARLIST = 'NONE'                                              444
         CALL AXMATERIAL                                                  444
C    *                                                                    444
C    Geant ATTRIBUTE : PRWB ===> SEEN=0 COLO=2 <===                       444
         CALL AGSRESET                                                    445
         AG_SEEN = 0                                                      445
         AG_COLO = 2                                                      445
         AG_TITLE = 'PRWB'                                                445
         AG_EXNAME = 'ATTRIBUTE'                                          445
         AG_PARLIST = 'SEEN_COLO'                                         445
         CALL AXATTRIBUTE                                                 445
C    *                                                                    445
C    Geant SHAPE : TUBE ===> RMIN=PIWA_S2OUTR RMAX=PIWA_RIBOUTR, DZ=PIWA  445
C    _RIBNUM*PIWA_RIBSPA/2. <===                                          445
         CALL AGSRESET                                                    447
         AG_RMIN = PIWA_S2OUTR                                            447
         AG_RMAX = PIWA_RIBOUTR                                           447
         AG_DZ = PIWA_RIBNUM*PIWA_RIBSPA/2.                               447
         AG_TITLE = 'TUBE'                                                447
         AG_EXNAME = 'SHAPE'                                              447
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      447
         CALL AXSHAPE                                                     447
C    *                                                                    447
C    Create PRWD :                                                        448
C    Call To Local Procedure PRWD                                         448
         ASSIGN 5740 TO BLOCK_PRWD                                        448
         GO TO  5730                                                      448
5740     CONTINUE                                                         448
C    *                                                                    448
C    Geant POSITION : PRWD ===> inherited arguments used <===             449
         CALL AGSRESET                                                    449
         AG_TITLE = 'PRWD'                                                449
         AG_EXNAME = 'POSITION'                                           449
         AG_PARLIST = 'NONE'                                              449
         CALL AXPOSITION                                                  449
C    *                                                                    449
5651  CALL AGSPOPB                                                        450
      GO TO BLOCK_PRWS, (5650 ,5660)                                      450
C End of Local Procedure                                                  450
C ----------------------------------------------------------------------  451
C ------------ PRWD                                                       451
C Begin Local Procedure PRWD                                              452
C PRWD IS A RIB SECTION                                                   452
5730  CONTINUE                                                            452
         AG_TITLE='PRWD'                                                  452
         AG_MODULE=MODULE                                                 452
         AG_MARK='V00'                                                    452
         CALL AGSPUSH                                                     452
         AG_MODULE=MODULE                                                 452
C    *                                                                    452
C    Geant SHAPE : DIVISION ===> NDIV=PIWA_RIBNUM IAXIS=3 <===            453
         CALL AGSRESET                                                    453
         AG_NDIV = PIWA_RIBNUM                                            453
         AG_IAXIS = 3                                                     453
         AG_TITLE = 'DIVISION'                                            453
         AG_EXNAME = 'SHAPE'                                              453
         AG_PARLIST = 'NDIV_IAXIS'                                        453
         CALL AXSHAPE                                                     453
C    *                                                                    453
C    Create PRWB :                                                        454
C    Call To Local Procedure PRWB                                         454
         ASSIGN 5760 TO BLOCK_PRWB                                        454
         GO TO  5750                                                      454
5760     CONTINUE                                                         454
C    *                                                                    454
C    Geant POSITION : PRWB ===> inherited arguments used <===             455
         CALL AGSRESET                                                    455
         AG_TITLE = 'PRWB'                                                455
         AG_EXNAME = 'POSITION'                                           455
         AG_PARLIST = 'NONE'                                              455
         CALL AXPOSITION                                                  455
C    *                                                                    455
5731  CALL AGSPOPB                                                        456
      GO TO BLOCK_PRWD, (5730 ,5740)                                      456
C End of Local Procedure                                                  456
C ----------------------------------------------------------------------  457
C ------------ PRWB                                                       457
C Begin Local Procedure PRWB                                              458
C PRWB IS A RIB OF STEEL BELLOWS                                          458
5750  CONTINUE                                                            458
         AG_TITLE='PRWB'                                                  458
         AG_MODULE=MODULE                                                 458
         AG_MARK='V00'                                                    458
         CALL AGSPUSH                                                     458
         AG_MODULE=MODULE                                                 458
C    *                                                                    458
C    Geant MATERIAL : IRON ===> inherited arguments used <===             459
         CALL AGSRESET                                                    459
         AG_TITLE = 'IRON'                                                459
         AG_EXNAME = 'MATERIAL'                                           459
         AG_PARLIST = 'NONE'                                              459
         CALL AXMATERIAL                                                  459
C    *                                                                    459
C    Geant ATTRIBUTE : PRWB ===> SEEN=1 COLO=4 <===                       459
         CALL AGSRESET                                                    460
         AG_SEEN = 1                                                      460
         AG_COLO = 4                                                      460
         AG_TITLE = 'PRWB'                                                460
         AG_EXNAME = 'ATTRIBUTE'                                          460
         AG_PARLIST = 'SEEN_COLO'                                         460
         CALL AXATTRIBUTE                                                 460
C    *                                                                    460
C    Geant SHAPE : TUBE ===> DZ=PIWA_RIBTHK <===                          460
         CALL AGSRESET                                                    461
         AG_DZ = PIWA_RIBTHK                                              461
         AG_TITLE = 'TUBE'                                                461
         AG_EXNAME = 'SHAPE'                                              461
         AG_PARLIST = 'DZ'                                                461
         CALL AXSHAPE                                                     461
C    *                                                                    461
5751  CALL AGSPOPB                                                        462
      GO TO BLOCK_PRWB, (5750 ,5760)                                      462
C End of Local Procedure                                                  462
C ----------------------------------------------------------------------  463
C ------------ PFLT                                                       463
C Begin Local Procedure PFLT                                              464
C PFLT IS THE 2ND SET OF FLANGES AT ~4.2 M FROM IR                        464
5510  CONTINUE                                                            464
         AG_TITLE='PFLT'                                                  464
         AG_MODULE=MODULE                                                 464
         AG_MARK='V00'                                                    464
         CALL AGSPUSH                                                     464
         AG_MODULE=MODULE                                                 464
C    *                                                                    464
C    Geant MATERIAL : IRON ===> inherited arguments used <===             465
         CALL AGSRESET                                                    465
         AG_TITLE = 'IRON'                                                465
         AG_EXNAME = 'MATERIAL'                                           465
         AG_PARLIST = 'NONE'                                              465
         CALL AXMATERIAL                                                  465
C    *                                                                    465
C    Geant ATTRIBUTE : PFLT ===> SEEN=1 COLO=4 <===                       465
         CALL AGSRESET                                                    466
         AG_SEEN = 1                                                      466
         AG_COLO = 4                                                      466
         AG_TITLE = 'PFLT'                                                466
         AG_EXNAME = 'ATTRIBUTE'                                          466
         AG_PARLIST = 'SEEN_COLO'                                         466
         CALL AXATTRIBUTE                                                 466
C    *                                                                    466
C    Geant SHAPE : TUBE ===> RMIN=PIPA_S2OUTR RMAX=5.85 DZ=2.0 <===       466
         CALL AGSRESET                                                    467
         AG_RMIN = PIPA_S2OUTR                                            467
         AG_RMAX = 5.85                                                   467
         AG_DZ = 2.0                                                      467
         AG_TITLE = 'TUBE'                                                467
         AG_EXNAME = 'SHAPE'                                              467
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      467
         CALL AXSHAPE                                                     467
C    *                                                                    467
5511  CALL AGSPOPB                                                        468
      GO TO BLOCK_PFLT, (5510 ,5520,5680)                                 468
C End of Local Procedure                                                  468
C ----------------------------------------------------------------------  469
C ------------ PFLO                                                       469
C Begin Local Procedure PFLO                                              470
C PFLO IS THE 1ND SET OF FLANGES AT ~4.2 M FROM IR                        470
5490  CONTINUE                                                            470
         AG_TITLE='PFLO'                                                  470
         AG_MODULE=MODULE                                                 470
         AG_MARK='V00'                                                    470
         CALL AGSPUSH                                                     470
         AG_MODULE=MODULE                                                 470
C    *                                                                    470
C    Geant MATERIAL : IRON ===> inherited arguments used <===             471
         CALL AGSRESET                                                    471
         AG_TITLE = 'IRON'                                                471
         AG_EXNAME = 'MATERIAL'                                           471
         AG_PARLIST = 'NONE'                                              471
         CALL AXMATERIAL                                                  471
C    *                                                                    471
C    Geant ATTRIBUTE : PFL0 ===> SEEN=1 COLO=4 <===                       471
         CALL AGSRESET                                                    472
         AG_SEEN = 1                                                      472
         AG_COLO = 4                                                      472
         AG_TITLE = 'PFL0'                                                472
         AG_EXNAME = 'ATTRIBUTE'                                          472
         AG_PARLIST = 'SEEN_COLO'                                         472
         CALL AXATTRIBUTE                                                 472
C    *                                                                    472
C    Geant SHAPE : TUBE ===> RMIN=PIPA_S2OUTR RMAX=5.85 DZ=1.0 <===       472
         CALL AGSRESET                                                    473
         AG_RMIN = PIPA_S2OUTR                                            473
         AG_RMAX = 5.85                                                   473
         AG_DZ = 1.0                                                      473
         AG_TITLE = 'TUBE'                                                473
         AG_EXNAME = 'SHAPE'                                              473
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      473
         CALL AXSHAPE                                                     473
C    *                                                                    473
5491  CALL AGSPOPB                                                        474
      GO TO BLOCK_PFLO, (5490 ,5500,5670)                                 474
C End of Local Procedure                                                  474
      WRITE(LOUT,5770)'===>>  [End AgML Module PipeGeo3]  <<==='          474
5770  FORMAT(A32,/,/)                                                     474
C MODULE PIPEGEO3                                                         476
      END                                                                 476
