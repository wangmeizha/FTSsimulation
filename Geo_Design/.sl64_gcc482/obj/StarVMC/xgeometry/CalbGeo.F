C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE CALBGEO                                                  2
C CALBGEO IS THE GEOMETRY OF THE BARREL EM CALORIMETER IN (AG)STAR, *     2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'CALBGEO'/, BANK_TITLE/'*'/                             2
      DATA MCOMMENT/'IS THE GEOMETRY OF THE BARREL EM CALORIMETER IN (AG  2
     *)STAR, *'/                                                          2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C AUTHOR K. SHESTERMANOV, IHEP. FIRST VERSION W. LLOPE                    2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 AUTHOR /'K. SHESTERMANOV, IHEP. FIRST VERSION W. LLOP  3
     *E'/                                                                 3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C CREATED NOVEMBER 17 1995                                                3
      CHARACTER*40 CREATED /'NOVEMBER 17 1995'/                           4
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
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   6
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        6
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       6
C - GEANT description of logical units                                    6
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            7
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      7
      COMMON/GCMAIL/ CHMAIL                                               7
      CHARACTER*132 CHMAIL                                                7
      EXTERNAL ETSPHIT                                                    7
      INTEGER BLOCK_CALB                                                  9
      INTEGER BLOCK_CHLV                                                  9
      INTEGER BLOCK_CPHI                                                  9
      INTEGER BLOCK_CSUP                                                  9
      INTEGER BLOCK_CPBP                                                  9
      INTEGER BLOCK_CSCI                                                  9
      INTEGER BLOCK_CSMD                                                  9
      INTEGER BLOCK_CSMG                                                  9
      INTEGER BLOCK_CSDA                                                  9
      INTEGER BLOCK_CSMC                                                  9
      INTEGER BLOCK_CSMB                                                  9
      INTEGER BLOCK_CSME                                                  9
      INTEGER BLOCK_CSHI                                                  9
      INTEGER BLOCK_CBTW                                                  9
C -                                                                       14
      REAL CALG_VERSION                                                   14
      EQUIVALENCE (BANK_CALG(3),CALG_VERSION,RE_CALG_VERSION),            14
     *(NAMES_CALG(1),NCALG_VERSION)                                       14
      EQUIVALENCE (COMMEN_CALG(1),CCALG_VERSION),(MAP_CALG(1,1),          14
     *MCALG_VERSION)                                                      14
      CHARACTER CCALG_VERSION*40/' '/                                     14
      CHARACTER NCALG_VERSION*12/'R.VERSION'/                             14
      INTEGER MCALG_VERSION(2)/1,1/                                       14
      REAL RE_CALG_VERSION                                                14
C -                                                                       14
      REAL CALG_RMIN                                                      14
      EQUIVALENCE (BANK_CALG(4),CALG_RMIN,RE_CALG_RMIN),(NAMES_CALG(2),   14
     *NCALG_RMIN)                                                         14
      EQUIVALENCE (COMMEN_CALG(2),CCALG_RMIN),(MAP_CALG(1,2),             14
     *MCALG_RMIN)                                                         14
      CHARACTER CCALG_RMIN*40/' '/                                        14
      CHARACTER NCALG_RMIN*12/'R.RMIN'/                                   14
      INTEGER MCALG_RMIN(2)/1,1/                                          14
      REAL RE_CALG_RMIN                                                   14
C -                                                                       14
      REAL CALG_ETACUT                                                    14
      EQUIVALENCE (BANK_CALG(5),CALG_ETACUT,RE_CALG_ETACUT),              14
     *(NAMES_CALG(3),NCALG_ETACUT)                                        14
      EQUIVALENCE (COMMEN_CALG(3),CCALG_ETACUT),(MAP_CALG(1,3),           14
     *MCALG_ETACUT)                                                       14
      CHARACTER CCALG_ETACUT*40/' '/                                      14
      CHARACTER NCALG_ETACUT*12/'R.ETACUT'/                               14
      INTEGER MCALG_ETACUT(2)/1,1/                                        14
      REAL RE_CALG_ETACUT                                                 14
C -                                                                       14
      REAL CALG_CRACKWD                                                   14
      EQUIVALENCE (BANK_CALG(6),CALG_CRACKWD,RE_CALG_CRACKWD),            14
     *(NAMES_CALG(4),NCALG_CRACKWD)                                       14
      EQUIVALENCE (COMMEN_CALG(4),CCALG_CRACKWD),(MAP_CALG(1,4),          14
     *MCALG_CRACKWD)                                                      14
      CHARACTER CCALG_CRACKWD*40/' '/                                     14
      CHARACTER NCALG_CRACKWD*12/'R.CRACKWD'/                             14
      INTEGER MCALG_CRACKWD(2)/1,1/                                       14
      REAL RE_CALG_CRACKWD                                                14
C -                                                                       14
      REAL CALG_FRONTTHK                                                  14
      EQUIVALENCE (BANK_CALG(7),CALG_FRONTTHK,RE_CALG_FRONTTHK),          14
     *(NAMES_CALG(5),NCALG_FRONTTHK)                                      14
      EQUIVALENCE (COMMEN_CALG(5),CCALG_FRONTTHK),(MAP_CALG(1,5),         14
     *MCALG_FRONTTHK)                                                     14
      CHARACTER CCALG_FRONTTHK*40/' '/                                    14
      CHARACTER NCALG_FRONTTHK*12/'R.FRONTTHK'/                           14
      INTEGER MCALG_FRONTTHK(2)/1,1/                                      14
      REAL RE_CALG_FRONTTHK                                               14
C -                                                                       14
      REAL CALG_COMPTHK                                                   14
      EQUIVALENCE (BANK_CALG(8),CALG_COMPTHK,RE_CALG_COMPTHK),            14
     *(NAMES_CALG(6),NCALG_COMPTHK)                                       14
      EQUIVALENCE (COMMEN_CALG(6),CCALG_COMPTHK),(MAP_CALG(1,6),          14
     *MCALG_COMPTHK)                                                      14
      CHARACTER CCALG_COMPTHK*40/' '/                                     14
      CHARACTER NCALG_COMPTHK*12/'R.COMPTHK'/                             14
      INTEGER MCALG_COMPTHK(2)/1,1/                                       14
      REAL RE_CALG_COMPTHK                                                14
C -                                                                       14
      REAL CALG_AIRTHK                                                    14
      EQUIVALENCE (BANK_CALG(9),CALG_AIRTHK,RE_CALG_AIRTHK),              14
     *(NAMES_CALG(7),NCALG_AIRTHK)                                        14
      EQUIVALENCE (COMMEN_CALG(7),CCALG_AIRTHK),(MAP_CALG(1,7),           14
     *MCALG_AIRTHK)                                                       14
      CHARACTER CCALG_AIRTHK*40/' '/                                      14
      CHARACTER NCALG_AIRTHK*12/'R.AIRTHK'/                               14
      INTEGER MCALG_AIRTHK(2)/1,1/                                        14
      REAL RE_CALG_AIRTHK                                                 14
C -                                                                       14
      REAL CALG_BACKTHK                                                   14
      EQUIVALENCE (BANK_CALG(10),CALG_BACKTHK,RE_CALG_BACKTHK),           14
     *(NAMES_CALG(8),NCALG_BACKTHK)                                       14
      EQUIVALENCE (COMMEN_CALG(8),CCALG_BACKTHK),(MAP_CALG(1,8),          14
     *MCALG_BACKTHK)                                                      14
      CHARACTER CCALG_BACKTHK*40/' '/                                     14
      CHARACTER NCALG_BACKTHK*12/'R.BACKTHK'/                             14
      INTEGER MCALG_BACKTHK(2)/1,1/                                       14
      REAL RE_CALG_BACKTHK                                                14
C -                                                                       14
      REAL CALG_SPACETHK                                                  14
      EQUIVALENCE (BANK_CALG(11),CALG_SPACETHK,RE_CALG_SPACETHK),         14
     *(NAMES_CALG(9),NCALG_SPACETHK)                                      14
      EQUIVALENCE (COMMEN_CALG(9),CCALG_SPACETHK),(MAP_CALG(1,9),         14
     *MCALG_SPACETHK)                                                     14
      CHARACTER CCALG_SPACETHK*40/' '/                                    14
      CHARACTER NCALG_SPACETHK*12/'R.SPACETHK'/                           14
      INTEGER MCALG_SPACETHK(2)/1,1/                                      14
      REAL RE_CALG_SPACETHK                                               14
C -                                                                       14
      REAL CALG_SCINTTHK(2)                                               14
      EQUIVALENCE (BANK_CALG(12),CALG_SCINTTHK,RE_CALG_SCINTTHK),         14
     *(NAMES_CALG(10),NCALG_SCINTTHK)                                     14
      EQUIVALENCE (COMMEN_CALG(10),CCALG_SCINTTHK),(MAP_CALG(1,10),       14
     *MCALG_SCINTTHK)                                                     14
      CHARACTER CCALG_SCINTTHK*40/' '/                                    14
      CHARACTER NCALG_SCINTTHK*12/'R.SCINTTHK'/                           14
      INTEGER MCALG_SCINTTHK(2)/2,1/                                      14
      REAL RE_CALG_SCINTTHK                                               14
C -                                                                       14
      REAL CALG_ABSORTHK                                                  14
      EQUIVALENCE (BANK_CALG(14),CALG_ABSORTHK,RE_CALG_ABSORTHK),         14
     *(NAMES_CALG(11),NCALG_ABSORTHK)                                     14
      EQUIVALENCE (COMMEN_CALG(11),CCALG_ABSORTHK),(MAP_CALG(1,11),       14
     *MCALG_ABSORTHK)                                                     14
      CHARACTER CCALG_ABSORTHK*40/' '/                                    14
      CHARACTER NCALG_ABSORTHK*12/'R.ABSORTHK'/                           14
      INTEGER MCALG_ABSORTHK(2)/1,1/                                      14
      REAL RE_CALG_ABSORTHK                                               14
C -                                                                       14
      REAL CALG_ABPAPTHK                                                  14
      EQUIVALENCE (BANK_CALG(15),CALG_ABPAPTHK,RE_CALG_ABPAPTHK),         14
     *(NAMES_CALG(12),NCALG_ABPAPTHK)                                     14
      EQUIVALENCE (COMMEN_CALG(12),CCALG_ABPAPTHK),(MAP_CALG(1,12),       14
     *MCALG_ABPAPTHK)                                                     14
      CHARACTER CCALG_ABPAPTHK*40/' '/                                    14
      CHARACTER NCALG_ABPAPTHK*12/'R.ABPAPTHK'/                           14
      INTEGER MCALG_ABPAPTHK(2)/1,1/                                      14
      REAL RE_CALG_ABPAPTHK                                               14
C -                                                                       14
      REAL CALG_G10SBTHK                                                  14
      EQUIVALENCE (BANK_CALG(16),CALG_G10SBTHK,RE_CALG_G10SBTHK),         14
     *(NAMES_CALG(13),NCALG_G10SBTHK)                                     14
      EQUIVALENCE (COMMEN_CALG(13),CCALG_G10SBTHK),(MAP_CALG(1,13),       14
     *MCALG_G10SBTHK)                                                     14
      CHARACTER CCALG_G10SBTHK*40/' '/                                    14
      CHARACTER NCALG_G10SBTHK*12/'R.G10SBTHK'/                           14
      INTEGER MCALG_G10SBTHK(2)/1,1/                                      14
      REAL RE_CALG_G10SBTHK                                               14
C -                                                                       14
      REAL CALG_SMALFWDH                                                  14
      EQUIVALENCE (BANK_CALG(17),CALG_SMALFWDH,RE_CALG_SMALFWDH),         14
     *(NAMES_CALG(14),NCALG_SMALFWDH)                                     14
      EQUIVALENCE (COMMEN_CALG(14),CCALG_SMALFWDH),(MAP_CALG(1,14),       14
     *MCALG_SMALFWDH)                                                     14
      CHARACTER CCALG_SMALFWDH*40/' '/                                    14
      CHARACTER NCALG_SMALFWDH*12/'R.SMALFWDH'/                           14
      INTEGER MCALG_SMALFWDH(2)/1,1/                                      14
      REAL RE_CALG_SMALFWDH                                               14
C -                                                                       14
      REAL CALG_SMALFTHK                                                  14
      EQUIVALENCE (BANK_CALG(18),CALG_SMALFTHK,RE_CALG_SMALFTHK),         14
     *(NAMES_CALG(15),NCALG_SMALFTHK)                                     14
      EQUIVALENCE (COMMEN_CALG(15),CCALG_SMALFTHK),(MAP_CALG(1,15),       14
     *MCALG_SMALFTHK)                                                     14
      CHARACTER CCALG_SMALFTHK*40/' '/                                    14
      CHARACTER NCALG_SMALFTHK*12/'R.SMALFTHK'/                           14
      INTEGER MCALG_SMALFTHK(2)/1,1/                                      14
      REAL RE_CALG_SMALFTHK                                               14
C -                                                                       14
      REAL CALG_SMGASTHK                                                  14
      EQUIVALENCE (BANK_CALG(19),CALG_SMGASTHK,RE_CALG_SMGASTHK),         14
     *(NAMES_CALG(16),NCALG_SMGASTHK)                                     14
      EQUIVALENCE (COMMEN_CALG(16),CCALG_SMGASTHK),(MAP_CALG(1,16),       14
     *MCALG_SMGASTHK)                                                     14
      CHARACTER CCALG_SMGASTHK*40/' '/                                    14
      CHARACTER NCALG_SMGASTHK*12/'R.SMGASTHK'/                           14
      INTEGER MCALG_SMGASTHK(2)/1,1/                                      14
      REAL RE_CALG_SMGASTHK                                               14
C -                                                                       14
      REAL CALG_SMGASWDH                                                  14
      EQUIVALENCE (BANK_CALG(20),CALG_SMGASWDH,RE_CALG_SMGASWDH),         14
     *(NAMES_CALG(17),NCALG_SMGASWDH)                                     14
      EQUIVALENCE (COMMEN_CALG(17),CCALG_SMGASWDH),(MAP_CALG(1,17),       14
     *MCALG_SMGASWDH)                                                     14
      CHARACTER CCALG_SMGASWDH*40/' '/                                    14
      CHARACTER NCALG_SMGASWDH*12/'R.SMGASWDH'/                           14
      INTEGER MCALG_SMGASWDH(2)/1,1/                                      14
      REAL RE_CALG_SMGASWDH                                               14
C -                                                                       14
      REAL CALG_SMGASRAD                                                  14
      EQUIVALENCE (BANK_CALG(21),CALG_SMGASRAD,RE_CALG_SMGASRAD),         14
     *(NAMES_CALG(18),NCALG_SMGASRAD)                                     14
      EQUIVALENCE (COMMEN_CALG(18),CCALG_SMGASRAD),(MAP_CALG(1,18),       14
     *MCALG_SMGASRAD)                                                     14
      CHARACTER CCALG_SMGASRAD*40/' '/                                    14
      CHARACTER NCALG_SMGASRAD*12/'R.SMGASRAD'/                           14
      INTEGER MCALG_SMGASRAD(2)/1,1/                                      14
      REAL RE_CALG_SMGASRAD                                               14
C -                                                                       14
      REAL CALG_SMAFFWDH                                                  14
      EQUIVALENCE (BANK_CALG(22),CALG_SMAFFWDH,RE_CALG_SMAFFWDH),         14
     *(NAMES_CALG(19),NCALG_SMAFFWDH)                                     14
      EQUIVALENCE (COMMEN_CALG(19),CCALG_SMAFFWDH),(MAP_CALG(1,19),       14
     *MCALG_SMAFFWDH)                                                     14
      CHARACTER CCALG_SMAFFWDH*40/' '/                                    14
      CHARACTER NCALG_SMAFFWDH*12/'R.SMAFFWDH'/                           14
      INTEGER MCALG_SMAFFWDH(2)/1,1/                                      14
      REAL RE_CALG_SMAFFWDH                                               14
C -                                                                       14
      REAL CALG_SMAFBWDH                                                  14
      EQUIVALENCE (BANK_CALG(23),CALG_SMAFBWDH,RE_CALG_SMAFBWDH),         14
     *(NAMES_CALG(20),NCALG_SMAFBWDH)                                     14
      EQUIVALENCE (COMMEN_CALG(20),CCALG_SMAFBWDH),(MAP_CALG(1,20),       14
     *MCALG_SMAFBWDH)                                                     14
      CHARACTER CCALG_SMAFBWDH*40/' '/                                    14
      CHARACTER NCALG_SMAFBWDH*12/'R.SMAFBWDH'/                           14
      INTEGER MCALG_SMAFBWDH(2)/1,1/                                      14
      REAL RE_CALG_SMAFBWDH                                               14
C -                                                                       14
      REAL CALG_SMETAWDH                                                  14
      EQUIVALENCE (BANK_CALG(24),CALG_SMETAWDH,RE_CALG_SMETAWDH),         14
     *(NAMES_CALG(21),NCALG_SMETAWDH)                                     14
      EQUIVALENCE (COMMEN_CALG(21),CCALG_SMETAWDH),(MAP_CALG(1,21),       14
     *MCALG_SMETAWDH)                                                     14
      CHARACTER CCALG_SMETAWDH*40/' '/                                    14
      CHARACTER NCALG_SMETAWDH*12/'R.SMETAWDH'/                           14
      INTEGER MCALG_SMETAWDH(2)/1,1/                                      14
      REAL RE_CALG_SMETAWDH                                               14
C -                                                                       14
      REAL CALG_SETA1WDH                                                  14
      EQUIVALENCE (BANK_CALG(25),CALG_SETA1WDH,RE_CALG_SETA1WDH),         14
     *(NAMES_CALG(22),NCALG_SETA1WDH)                                     14
      EQUIVALENCE (COMMEN_CALG(22),CCALG_SETA1WDH),(MAP_CALG(1,22),       14
     *MCALG_SETA1WDH)                                                     14
      CHARACTER CCALG_SETA1WDH*40/' '/                                    14
      CHARACTER NCALG_SETA1WDH*12/'R.SETA1WDH'/                           14
      INTEGER MCALG_SETA1WDH(2)/1,1/                                      14
      REAL RE_CALG_SETA1WDH                                               14
C -                                                                       14
      REAL CALG_NETFIRST                                                  14
      EQUIVALENCE (BANK_CALG(26),CALG_NETFIRST,RE_CALG_NETFIRST),         14
     *(NAMES_CALG(23),NCALG_NETFIRST)                                     14
      EQUIVALENCE (COMMEN_CALG(23),CCALG_NETFIRST),(MAP_CALG(1,23),       14
     *MCALG_NETFIRST)                                                     14
      CHARACTER CCALG_NETFIRST*40/' '/                                    14
      CHARACTER NCALG_NETFIRST*12/'R.NETFIRST'/                           14
      INTEGER MCALG_NETFIRST(2)/1,1/                                      14
      REAL RE_CALG_NETFIRST                                               14
C -                                                                       14
      REAL CALG_SETA2WDH                                                  14
      EQUIVALENCE (BANK_CALG(27),CALG_SETA2WDH,RE_CALG_SETA2WDH),         14
     *(NAMES_CALG(24),NCALG_SETA2WDH)                                     14
      EQUIVALENCE (COMMEN_CALG(24),CCALG_SETA2WDH),(MAP_CALG(1,24),       14
     *MCALG_SETA2WDH)                                                     14
      CHARACTER CCALG_SETA2WDH*40/' '/                                    14
      CHARACTER NCALG_SETA2WDH*12/'R.SETA2WDH'/                           14
      INTEGER MCALG_SETA2WDH(2)/1,1/                                      14
      REAL RE_CALG_SETA2WDH                                               14
C -                                                                       14
      REAL CALG_NETSECON                                                  14
      EQUIVALENCE (BANK_CALG(28),CALG_NETSECON,RE_CALG_NETSECON),         14
     *(NAMES_CALG(25),NCALG_NETSECON)                                     14
      EQUIVALENCE (COMMEN_CALG(25),CCALG_NETSECON),(MAP_CALG(1,25),       14
     *MCALG_NETSECON)                                                     14
      CHARACTER CCALG_NETSECON*40/' '/                                    14
      CHARACTER NCALG_NETSECON*12/'R.NETSECON'/                           14
      INTEGER MCALG_NETSECON(2)/1,1/                                      14
      REAL RE_CALG_NETSECON                                               14
C -                                                                       14
      REAL CALG_SET12WDH                                                  14
      EQUIVALENCE (BANK_CALG(29),CALG_SET12WDH,RE_CALG_SET12WDH),         14
     *(NAMES_CALG(26),NCALG_SET12WDH)                                     14
      EQUIVALENCE (COMMEN_CALG(26),CCALG_SET12WDH),(MAP_CALG(1,26),       14
     *MCALG_SET12WDH)                                                     14
      CHARACTER CCALG_SET12WDH*40/' '/                                    14
      CHARACTER NCALG_SET12WDH*12/'R.SET12WDH'/                           14
      INTEGER MCALG_SET12WDH(2)/1,1/                                      14
      REAL RE_CALG_SET12WDH                                               14
C -                                                                       14
      REAL CALG_SPHIWDH                                                   14
      EQUIVALENCE (BANK_CALG(30),CALG_SPHIWDH,RE_CALG_SPHIWDH),           14
     *(NAMES_CALG(27),NCALG_SPHIWDH)                                      14
      EQUIVALENCE (COMMEN_CALG(27),CCALG_SPHIWDH),(MAP_CALG(1,27),        14
     *MCALG_SPHIWDH)                                                      14
      CHARACTER CCALG_SPHIWDH*40/' '/                                     14
      CHARACTER NCALG_SPHIWDH*12/'R.SPHIWDH'/                             14
      INTEGER MCALG_SPHIWDH(2)/1,1/                                       14
      REAL RE_CALG_SPHIWDH                                                14
C -                                                                       14
      REAL CALG_SPHIDWDH                                                  14
      EQUIVALENCE (BANK_CALG(31),CALG_SPHIDWDH,RE_CALG_SPHIDWDH),         14
     *(NAMES_CALG(28),NCALG_SPHIDWDH)                                     14
      EQUIVALENCE (COMMEN_CALG(28),CCALG_SPHIDWDH),(MAP_CALG(1,28),       14
     *MCALG_SPHIDWDH)                                                     14
      CHARACTER CCALG_SPHIDWDH*40/' '/                                    14
      CHARACTER NCALG_SPHIDWDH*12/'R.SPHIDWDH'/                           14
      INTEGER MCALG_SPHIDWDH(2)/1,1/                                      14
      REAL RE_CALG_SPHIDWDH                                               14
C -                                                                       14
      REAL CALG_NPHISTR                                                   14
      EQUIVALENCE (BANK_CALG(32),CALG_NPHISTR,RE_CALG_NPHISTR),           14
     *(NAMES_CALG(29),NCALG_NPHISTR)                                      14
      EQUIVALENCE (COMMEN_CALG(29),CCALG_NPHISTR),(MAP_CALG(1,29),        14
     *MCALG_NPHISTR)                                                      14
      CHARACTER CCALG_NPHISTR*40/' '/                                     14
      CHARACTER NCALG_NPHISTR*12/'R.NPHISTR'/                             14
      INTEGER MCALG_NPHISTR(2)/1,1/                                       14
      REAL RE_CALG_NPHISTR                                                14
C -                                                                       14
      REAL CALG_NSMDALW                                                   14
      EQUIVALENCE (BANK_CALG(33),CALG_NSMDALW,RE_CALG_NSMDALW),           14
     *(NAMES_CALG(30),NCALG_NSMDALW)                                      14
      EQUIVALENCE (COMMEN_CALG(30),CCALG_NSMDALW),(MAP_CALG(1,30),        14
     *MCALG_NSMDALW)                                                      14
      CHARACTER CCALG_NSMDALW*40/' '/                                     14
      CHARACTER NCALG_NSMDALW*12/'R.NSMDALW'/                             14
      INTEGER MCALG_NSMDALW(2)/1,1/                                       14
      REAL RE_CALG_NSMDALW                                                14
C -                                                                       14
      REAL CALG_NSUPER                                                    14
      EQUIVALENCE (BANK_CALG(34),CALG_NSUPER,RE_CALG_NSUPER),             14
     *(NAMES_CALG(31),NCALG_NSUPER)                                       14
      EQUIVALENCE (COMMEN_CALG(31),CCALG_NSUPER),(MAP_CALG(1,31),         14
     *MCALG_NSUPER)                                                       14
      CHARACTER CCALG_NSUPER*40/' '/                                      14
      CHARACTER NCALG_NSUPER*12/'R.NSUPER'/                               14
      INTEGER MCALG_NSUPER(2)/1,1/                                        14
      REAL RE_CALG_NSUPER                                                 14
C -                                                                       14
      REAL CALG_NSMD                                                      14
      EQUIVALENCE (BANK_CALG(35),CALG_NSMD,RE_CALG_NSMD),                 14
     *(NAMES_CALG(32),NCALG_NSMD)                                         14
      EQUIVALENCE (COMMEN_CALG(32),CCALG_NSMD),(MAP_CALG(1,32),           14
     *MCALG_NSMD)                                                         14
      CHARACTER CCALG_NSMD*40/' '/                                        14
      CHARACTER NCALG_NSMD*12/'R.NSMD'/                                   14
      INTEGER MCALG_NSMD(2)/1,1/                                          14
      REAL RE_CALG_NSMD                                                   14
C -                                                                       14
      REAL CALG_NSUBLAY(2)                                                14
      EQUIVALENCE (BANK_CALG(36),CALG_NSUBLAY,RE_CALG_NSUBLAY),           14
     *(NAMES_CALG(33),NCALG_NSUBLAY)                                      14
      EQUIVALENCE (COMMEN_CALG(33),CCALG_NSUBLAY),(MAP_CALG(1,33),        14
     *MCALG_NSUBLAY)                                                      14
      CHARACTER CCALG_NSUBLAY*40/' '/                                     14
      CHARACTER NCALG_NSUBLAY*12/'R.NSUBLAY'/                             14
      INTEGER MCALG_NSUBLAY(2)/2,1/                                       14
      REAL RE_CALG_NSUBLAY                                                14
C -                                                                       14
      REAL CALG_NMODULE(2)                                                14
      EQUIVALENCE (BANK_CALG(38),CALG_NMODULE,RE_CALG_NMODULE),           14
     *(NAMES_CALG(34),NCALG_NMODULE)                                      14
      EQUIVALENCE (COMMEN_CALG(34),CCALG_NMODULE),(MAP_CALG(1,34),        14
     *MCALG_NMODULE)                                                      14
      CHARACTER CCALG_NMODULE*40/' '/                                     14
      CHARACTER NCALG_NMODULE*12/'R.NMODULE'/                             14
      INTEGER MCALG_NMODULE(2)/2,1/                                       14
      REAL RE_CALG_NMODULE                                                14
C -                                                                       14
      REAL CALG_SHIFT(2)                                                  14
      EQUIVALENCE (BANK_CALG(40),CALG_SHIFT,RE_CALG_SHIFT),               14
     *(NAMES_CALG(35),NCALG_SHIFT)                                        14
      EQUIVALENCE (COMMEN_CALG(35),CCALG_SHIFT),(MAP_CALG(1,35),          14
     *MCALG_SHIFT)                                                        14
      CHARACTER CCALG_SHIFT*40/' '/                                       14
      CHARACTER NCALG_SHIFT*12/'R.SHIFT'/                                 14
      INTEGER MCALG_SHIFT(2)/2,1/                                         14
      REAL RE_CALG_SHIFT                                                  14
C -                                                                       14
      REAL CALG_MAXMODULE                                                 14
      EQUIVALENCE (BANK_CALG(42),CALG_MAXMODULE,RE_CALG_MAXMODULE),       14
     *(NAMES_CALG(36),NCALG_MAXMODULE)                                    14
      EQUIVALENCE (COMMEN_CALG(36),CCALG_MAXMODULE),(MAP_CALG(1,36),      14
     *MCALG_MAXMODULE)                                                    14
      CHARACTER CCALG_MAXMODULE*40/' '/                                   14
      CHARACTER NCALG_MAXMODULE*12/'R.MAXMODULE'/                         14
      INTEGER MCALG_MAXMODULE(2)/1,1/                                     14
      REAL RE_CALG_MAXMODULE                                              14
C -                                                                       14
      REAL CALG_NETAT                                                     14
      EQUIVALENCE (BANK_CALG(43),CALG_NETAT,RE_CALG_NETAT),               14
     *(NAMES_CALG(37),NCALG_NETAT)                                        14
      EQUIVALENCE (COMMEN_CALG(37),CCALG_NETAT),(MAP_CALG(1,37),          14
     *MCALG_NETAT)                                                        14
      CHARACTER CCALG_NETAT*40/' '/                                       14
      CHARACTER NCALG_NETAT*12/'R.NETAT'/                                 14
      INTEGER MCALG_NETAT(2)/1,1/                                         14
      REAL RE_CALG_NETAT                                                  14
C -                                                                       14
      REAL CALG_NSUB                                                      14
      EQUIVALENCE (BANK_CALG(44),CALG_NSUB,RE_CALG_NSUB),                 14
     *(NAMES_CALG(38),NCALG_NSUB)                                         14
      EQUIVALENCE (COMMEN_CALG(38),CCALG_NSUB),(MAP_CALG(1,38),           14
     *MCALG_NSUB)                                                         14
      CHARACTER CCALG_NSUB*40/' '/                                        14
      CHARACTER NCALG_NSUB*12/'R.NSUB'/                                   14
      INTEGER MCALG_NSUB(2)/1,1/                                          14
      REAL RE_CALG_NSUB                                                   14
C -                                                                       14
      REAL CALG_NETASMDP                                                  14
      EQUIVALENCE (BANK_CALG(45),CALG_NETASMDP,RE_CALG_NETASMDP),         14
     *(NAMES_CALG(39),NCALG_NETASMDP)                                     14
      EQUIVALENCE (COMMEN_CALG(39),CCALG_NETASMDP),(MAP_CALG(1,39),       14
     *MCALG_NETASMDP)                                                     14
      CHARACTER CCALG_NETASMDP*40/' '/                                    14
      CHARACTER NCALG_NETASMDP*12/'R.NETASMDP'/                           14
      INTEGER MCALG_NETASMDP(2)/1,1/                                      14
      REAL RE_CALG_NETASMDP                                               14
C -                                                                       14
      INTEGER LEN_CALG,LLEN_CALG,LINK_CALG                                14
      PARAMETER (LEN_CALG=45)                                             14
      PARAMETER (LLEN_CALG=39)                                            14
      INTEGER FLAG_CALG/0/,MAP_CALG(2,LLEN_CALG),IDNUM_CALG(10)           14
      CHARACTER NAMES_CALG(LLEN_CALG)*12,COMMEN_CALG(LLEN_CALG)*40        14
      DATA IDNUM_CALG/10*0/                                               14
      REAL BANK_CALG(LEN_CALG)                                            14
      CHARACTER*60 BPATH_CALG/' '/                                        14
      SAVE FLAG_CALG,MAP_CALG,IDNUM_CALG,NAMES_CALG,COMMEN_CALG,          14
     *BPATH_CALG                                                          14
      DATA LINK_CALG/0/,BANK_CALG/LEN_CALG*0/                             14
C *                                                                       14
C -                                                                       15
      REAL CALR_RMIN                                                      15
      EQUIVALENCE (BANK_CALR(3),CALR_RMIN,RE_CALR_RMIN),(NAMES_CALR(1),   15
     *NCALR_RMIN)                                                         15
      EQUIVALENCE (COMMEN_CALR(1),CCALR_RMIN),(MAP_CALR(1,1),             15
     *MCALR_RMIN)                                                         15
      CHARACTER CCALR_RMIN*40/' '/                                        15
      CHARACTER NCALR_RMIN*12/'R.RMIN'/                                   15
      INTEGER MCALR_RMIN(2)/1,1/                                          15
      REAL RE_CALR_RMIN                                                   15
C -                                                                       15
      REAL CALR_RPRS                                                      15
      EQUIVALENCE (BANK_CALR(4),CALR_RPRS,RE_CALR_RPRS),(NAMES_CALR(2),   15
     *NCALR_RPRS)                                                         15
      EQUIVALENCE (COMMEN_CALR(2),CCALR_RPRS),(MAP_CALR(1,2),             15
     *MCALR_RPRS)                                                         15
      CHARACTER CCALR_RPRS*40/' '/                                        15
      CHARACTER NCALR_RPRS*12/'R.RPRS'/                                   15
      INTEGER MCALR_RPRS(2)/1,1/                                          15
      REAL RE_CALR_RPRS                                                   15
C -                                                                       15
      REAL CALR_RSMD1                                                     15
      EQUIVALENCE (BANK_CALR(5),CALR_RSMD1,RE_CALR_RSMD1),                15
     *(NAMES_CALR(3),NCALR_RSMD1)                                         15
      EQUIVALENCE (COMMEN_CALR(3),CCALR_RSMD1),(MAP_CALR(1,3),            15
     *MCALR_RSMD1)                                                        15
      CHARACTER CCALR_RSMD1*40/' '/                                       15
      CHARACTER NCALR_RSMD1*12/'R.RSMD1'/                                 15
      INTEGER MCALR_RSMD1(2)/1,1/                                         15
      REAL RE_CALR_RSMD1                                                  15
C -                                                                       15
      REAL CALR_RSMD2                                                     15
      EQUIVALENCE (BANK_CALR(6),CALR_RSMD2,RE_CALR_RSMD2),                15
     *(NAMES_CALR(4),NCALR_RSMD2)                                         15
      EQUIVALENCE (COMMEN_CALR(4),CCALR_RSMD2),(MAP_CALR(1,4),            15
     *MCALR_RSMD2)                                                        15
      CHARACTER CCALR_RSMD2*40/' '/                                       15
      CHARACTER NCALR_RSMD2*12/'R.RSMD2'/                                 15
      INTEGER MCALR_RSMD2(2)/1,1/                                         15
      REAL RE_CALR_RSMD2                                                  15
C -                                                                       15
      REAL CALR_RMAX                                                      15
      EQUIVALENCE (BANK_CALR(7),CALR_RMAX,RE_CALR_RMAX),(NAMES_CALR(5),   15
     *NCALR_RMAX)                                                         15
      EQUIVALENCE (COMMEN_CALR(5),CCALR_RMAX),(MAP_CALR(1,5),             15
     *MCALR_RMAX)                                                         15
      CHARACTER CCALR_RMAX*40/' '/                                        15
      CHARACTER NCALR_RMAX*12/'R.RMAX'/                                   15
      INTEGER MCALR_RMAX(2)/1,1/                                          15
      REAL RE_CALR_RMAX                                                   15
C -                                                                       15
      INTEGER LEN_CALR,LLEN_CALR,LINK_CALR                                15
      PARAMETER (LEN_CALR=7)                                              15
      PARAMETER (LLEN_CALR=5)                                             15
      INTEGER FLAG_CALR/0/,MAP_CALR(2,LLEN_CALR),IDNUM_CALR(10)           15
      CHARACTER NAMES_CALR(LLEN_CALR)*12,COMMEN_CALR(LLEN_CALR)*40        15
      DATA IDNUM_CALR/10*0/                                               15
      REAL BANK_CALR(LEN_CALR)                                            15
      CHARACTER*60 BPATH_CALR/' '/                                        15
      SAVE FLAG_CALR,MAP_CALR,IDNUM_CALR,NAMES_CALR,COMMEN_CALR,          15
     *BPATH_CALR                                                          15
      DATA LINK_CALR/0/,BANK_CALR/LEN_CALR*0/                             15
C *                                                                       15
      REAL RKB2SC/0.013/,RKB3SC/9.6E-6/                                   16
      REAL CURRENT_DEPTH,CURRENT,LAYER_WIDTH(2),TAN_THETA,SMD_WIDTH,      21
     *SMD_WIDTH1,SMD_WIDTH2, SMD_WIDTH3,CUT_LENGTH,CUT_RADIUS,            21
     *FUTURE_DEPTH,C_DEP,C_LEAD_DEP,ETA_LENGHT, CURRENT_CSDA,H_ETA1,      21
     *H_ETA2,H_PHI1,H_PHI2,SH_ETA1,SH_ETA2,SH_PHI1,SH_PHI2,RMAX, HLENG,   21
     *DETA,DPHITOT,DPHIMOD,DPHIT,R1,R2,R3,R4,RR(2)                        21
      INTEGER LAYER,SUPER,SUB,I,J,II,NN                                   23
C *                                                                       25
      CALL AGSBEGM(MODULE,IPRIN)                                          25
      IF (IPRIN.LT.0) RETURN                                              25
C Filling bank CALG - BARREL CALORIMETER DATA                             25
      BANK_TITLE='BARREL CALORIMETER DATA'                                25
      BPATH_CALG='CALG*'                                                  25
      IDNUM_CALG(1)=0                                                     26
C GEOMETRY VERSION                                                        26
      CALG_VERSION=3.0                                                    26
      CCALG_VERSION='GEOMETRY VERSION'                                    26
C INNER RADIUS                                                            27
      CALG_RMIN=223.5                                                     27
      CCALG_RMIN='INNER RADIUS'                                           27
C CALORIMETER RAPIDITY CUT                                                28
      CALG_ETACUT=1.0                                                     28
      CCALG_ETACUT='CALORIMETER RAPIDITY CUT'                             28
C HALF WIDTH OF THE CRACK BETWEEN MODULES                                 29
      CALG_CRACKWD=0.655                                                  29
      CCALG_CRACKWD='HALF WIDTH OF THE CRACK BETWEEN MODULES'             29
C FRONT PLATE HALF THICKNESS                                              30
      CALG_FRONTTHK=0.9525                                                30
      CCALG_FRONTTHK='FRONT PLATE HALF THICKNESS'                         30
C BACK PLATE HALF THICKNESS                                               31
      CALG_COMPTHK=0.9525                                                 31
      CCALG_COMPTHK='BACK PLATE HALF THICKNESS'                           31
C AIR GAP HALF THICNESS                                                   32
      CALG_AIRTHK=0.158                                                   32
      CCALG_AIRTHK='AIR GAP HALF THICNESS'                                32
C MODULE BACK PLATE HALF THICKNES                                         33
      CALG_BACKTHK=1.5875                                                 33
      CCALG_BACKTHK='MODULE BACK PLATE HALF THICKNES'                     33
C SPACER BACK PLATE HALF THICKNES                                         34
      CALG_SPACETHK=0.9525                                                34
      CCALG_SPACETHK='SPACER BACK PLATE HALF THICKNES'                    34
C ACTIVE SCINTILLATOR PLATE HALF THICKNESS                                35
      CALG_SCINTTHK(1)=0.3                                                35
      CALG_SCINTTHK(2)=0.25                                               35
      CCALG_SCINTTHK='ACTIVE SCINTILLATOR PLATE HALF THICKNESS'           35
C ABSORBER PLATE THICKNESS HALFPTHICKNESS                                 36
      CALG_ABSORTHK=0.250                                                 36
      CCALG_ABSORTHK='ABSORBER PLATE THICKNESS HALFPTHICKNESS'            36
C ABSORBER PAPER PLATE THICKNESS HALF THICKNESS                           37
      CALG_ABPAPTHK=0.005                                                 37
      CCALG_ABPAPTHK='ABSORBER PAPER PLATE THICKNESS HALF THICKNESS'      37
C G10 SMD FRONT AND BACK PLATE HALF THICKNESS                             38
      CALG_G10SBTHK=0.115                                                 38
      CCALG_G10SBTHK='G10 SMD FRONT AND BACK PLATE HALF THICKNESS'        38
C SMD AL FRONT BACK PLATE WITH AR HALF WIDTH                              39
      CALG_SMALFWDH=11.2014                                               39
      CCALG_SMALFWDH='SMD AL FRONT BACK PLATE WITH AR HALF WIDTH'         39
C SMD AL FRONT BACK PLATE WITH AR HALF THICKNESS                          40
      CALG_SMALFTHK=0.3893                                                40
      CCALG_SMALFTHK='SMD AL FRONT BACK PLATE WITH AR HALF THICKNESS'     40
C SMD GAS BOX VOLUME HALF THICKNESS                                       41
      CALG_SMGASTHK=0.1359                                                41
      CCALG_SMGASTHK='SMD GAS BOX VOLUME HALF THICKNESS'                  41
C SMD GAS BOX VOLUME HALF WIDTH                                           42
      CALG_SMGASWDH=0.2946                                                42
      CCALG_SMGASWDH='SMD GAS BOX VOLUME HALF WIDTH'                      42
C SMD GAS VOLUME TUBS RADIUS                                              43
      CALG_SMGASRAD=0.2946                                                43
      CCALG_SMGASRAD='SMD GAS VOLUME TUBS RADIUS'                         43
C SMD AL FRONT FIRST (LAST) RIB HALF WIDTH                                44
      CALG_SMAFFWDH=0.113                                                 44
      CCALG_SMAFFWDH='SMD AL FRONT FIRST (LAST) RIB HALF WIDTH'           44
C SMD AL BACK FIRST (LAST) RIB HALF WIDTH                                 45
      CALG_SMAFBWDH=0.1664                                                45
      CCALG_SMAFBWDH='SMD AL BACK FIRST (LAST) RIB HALF WIDTH'            45
C ETA=0 STRIP NOTCH HALF WIDTH                                            46
      CALG_SMETAWDH=0.9806                                                46
      CCALG_SMETAWDH='ETA=0 STRIP NOTCH HALF WIDTH'                       46
C STRIP#1-75 HALF WITDH                                                   47
      CALG_SETA1WDH=0.7277                                                47
      CCALG_SETA1WDH='STRIP#1-75 HALF WITDH'                              47
C STRIP#76-150 HALF WITDH                                                 48
      CALG_SETA2WDH=0.9398                                                48
      CCALG_SETA2WDH='STRIP#76-150 HALF WITDH'                            48
C HALF DISTANCE BETWEEN STRIPS IN ETA                                     49
      CALG_SET12WDH=0.04064                                               49
      CCALG_SET12WDH='HALF DISTANCE BETWEEN STRIPS IN ETA'                49
C STRIP#(1-NPHISTR) IN PHI DIRECTION HALF WITDH                           50
      CALG_SPHIWDH=0.6680                                                 50
      CCALG_SPHIWDH='STRIP#(1-NPHISTR) IN PHI DIRECTION HALF WITDH'       50
C HALF DISTANCE BETWEEN STRIPS IN PHI                                     51
      CALG_SPHIDWDH=0.07874                                               51
      CCALG_SPHIDWDH='HALF DISTANCE BETWEEN STRIPS IN PHI'                51
C NUMBER SMD GASEUS INTERVAL IN TILE                                      52
      CALG_NSMDALW=30                                                     52
      CCALG_NSMDALW='NUMBER SMD GASEUS INTERVAL IN TILE'                  52
C NUMBER OF READOUT SUPERLAYER                                            53
      CALG_NSUPER=2                                                       53
      CCALG_NSUPER='NUMBER OF READOUT SUPERLAYER'                         53
C SMD POSITIONED AFTER SANDVICH TYPE LAYERS EMC                           54
      CALG_NSMD=5                                                         54
      CCALG_NSMD='SMD POSITIONED AFTER SANDVICH TYPE LAYERS EMC'          54
C NUMBER OF LAYERS IN A SUPERLAYER                                        55
      CALG_NSUBLAY(1)=2                                                   55
      CALG_NSUBLAY(2)=19                                                  55
      CCALG_NSUBLAY='NUMBER OF LAYERS IN A SUPERLAYER'                    55
C MAX NUMBER OF MOUDLE                                                    56
      CALG_MAXMODULE=60                                                   56
      CCALG_MAXMODULE='MAX NUMBER OF MOUDLE'                              56
C NUMBER OF ETA DIVISION FOR TOWER/PRESHOWER                              57
      CALG_NETAT=20                                                       57
      CCALG_NETAT='NUMBER OF ETA DIVISION FOR TOWER/PRESHOWER'            57
C NUMBER OF SUB DIV. IN PHI FOR TOWER/PRESHOWER                           58
      CALG_NSUB=2                                                         58
      CCALG_NSUB='NUMBER OF SUB DIV. IN PHI FOR TOWER/PRESHOWER'          58
C NUMBER OF ETA DIVISION IN FOR SMD PHI PLANE                             59
      CALG_NETASMDP=10                                                    59
      CCALG_NETASMDP='NUMBER OF ETA DIVISION IN FOR SMD PHI PLANE'        59
C NUMBER OF THE STRIP IN PHI DIRECTION                                    60
      CALG_NPHISTR=15                                                     60
      CCALG_NPHISTR='NUMBER OF THE STRIP IN PHI DIRECTION'                60
C NUMBER OF STRIP IN FIRST PART ETA=0-0.5                                 61
      CALG_NETFIRST=75.                                                   61
      CCALG_NETFIRST='NUMBER OF STRIP IN FIRST PART ETA=0-0.5'            61
C NUMBER OF STRIP IN SECOND PART ETA=0.5-1.0                              62
      CALG_NETSECON=75.                                                   62
      CCALG_NETSECON='NUMBER OF STRIP IN SECOND PART ETA=0.5-1.0'         62
C NUMBER OF MODULES                                                       63
      CALG_NMODULE(1)=60                                                  63
      CALG_NMODULE(2)=60                                                  63
      CCALG_NMODULE='NUMBER OF MODULES'                                   63
C STARTING AZIMUTH OF THE FIRST MODULE                                    64
      CALG_SHIFT(1)=75                                                    64
      CALG_SHIFT(2)=105                                                   64
      CCALG_SHIFT='STARTING AZIMUTH OF THE FIRST MODULE'                  64
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALG',    65
     *BPATH_CALG,IDNUM_CALG,1, MAP_CALG,NAMES_CALG,COMMEN_CALG,           65
     *BANK_CALG,LEN_CALG,LLEN_CALG,LINK_CALG,FLAG_CALG)                   65
C *                                                                       65
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALG',    65
     *BPATH_CALG,IDNUM_CALG,1, MAP_CALG,NAMES_CALG,COMMEN_CALG,           65
     *BANK_CALG,LEN_CALG,LLEN_CALG,LINK_CALG,FLAG_CALG)                   65
C use default CALG bank                                                   66
      BPATH_CALG='CALG*'                                                  66
      IDNUM_CALG(1)=0                                                     66
      CALL AGDGETP('CALBGEO','*',-999,'CALG',' ',BANK_CALG, BPATH_CALG,   66
     *IDNUM_CALG,1,MAP_CALG,NAMES_CALG,BANK_CALG,LEN_CALG,LLEN_CALG,      66
     *LINK_CALG,FLAG_CALG)                                                66
      SMD_WIDTH=2.*CALG_G10SBTHK+2.*CALG_SMALFTHK+2.*CALG_ABPAPTHK        67
      SMD_WIDTH1=2.*CALG_G10SBTHK+2.*CALG_ABPAPTHK                        68
      SMD_WIDTH2=SMD_WIDTH1+CALG_SMGASTHK+CALG_SMGASRAD                   69
      SMD_WIDTH3=2.*SMD_WIDTH-SMD_WIDTH1-CALG_SMGASTHK-CALG_SMGASRAD      70
      R1=CALG_RMIN+2.*CALG_FRONTTHK                                       71
      R2=0.0                                                              72
C *                                                                       73
      DO 5011 I=1,NINT(CALG_NSUPER)                                       73
         LAYER_WIDTH(I) = CALG_SCINTTHK(I) +                              74
     *   CALG_ABSORTHK+2.*CALG_ABPAPTHK                                   74
         R2=R2+(CALG_NSUBLAY(I)-I+1)*LAYER_WIDTH(I)*2.0                   75
         RR(I)=R2                                                         76
5011  CONTINUE                                                            77
5012  CONTINUE                                                            77
      R3=                                                                 78
     *(CALG_NSUPER*LAYER_WIDTH(1)+(CALG_NSMD-CALG_NSUPER)*LAYER_WIDTH(2)  78
     *)*2.                                                                78
      R4=(SMD_WIDTH+CALG_SCINTTHK(2)+2.*CALG_ABPAPTHK)*2.0                79
      CUT_RADIUS=R1+R2+R4                                                 80
      RMAX=                                                               81
     *CUT_RADIUS+2.*(CALG_BACKTHK+CALG_SPACETHK+CALG_COMPTHK+             81
     *CALG_AIRTHK)                                                        81
      TAN_THETA = TAN(2*ATAN(EXP(-CALG_ETACUT)))                          82
      CUT_LENGTH = CALG_RMIN/TAN_THETA                                    83
      HLENG = CUT_RADIUS/TAN_THETA                                        84
      NN = MAX(CALG_NMODULE(1),CALG_NMODULE(2))                           85
      DETA = 1.0/CALG_NETAT                                               86
      DPHIMOD = 360/CALG_MAXMODULE                                        87
      DPHIT = DPHIMOD/CALG_NSUB                                           88
      DPHITOT = DPHIMOD*NN                                                89
C *                                                                       90
      CALL AGSBEGM(MODULE,IPRIN)                                          90
      IF (IPRIN.LT.0) RETURN                                              90
C Filling bank CALR - BARREL EMC RADIUSES                                 90
      BANK_TITLE='BARREL EMC RADIUSES'                                    90
      BPATH_CALR='CALR*'                                                  90
      IDNUM_CALR(1)=0                                                     91
C INNER RADUIS OF SENSITIVE AREA                                          91
      CALR_RMIN=R1                                                        91
      CCALR_RMIN='INNER RADUIS OF SENSITIVE AREA'                         91
C MEAN RADUIS OF PRS                                                      92
      CALR_RPRS=R1+RR(1)/2.0                                              92
      CCALR_RPRS='MEAN RADUIS OF PRS'                                     92
C MEAN RADUIS OF SMD                                                      93
      CALR_RSMD1=R1+R3+SMD_WIDTH2                                         93
      CCALR_RSMD1='MEAN RADUIS OF SMD'                                    93
C MEAN RADUIS OF SMD                                                      94
      CALR_RSMD2=R1+R3+SMD_WIDTH3                                         94
      CCALR_RSMD2='MEAN RADUIS OF SMD'                                    94
C OUTER RADUIS OF SENSITIVE AREA                                          95
      CALR_RMAX=CUT_RADIUS                                                95
      CCALR_RMAX='OUTER RADUIS OF SENSITIVE AREA'                         95
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALR',    96
     *BPATH_CALR,IDNUM_CALR,1, MAP_CALR,NAMES_CALR,COMMEN_CALR,           96
     *BANK_CALR,LEN_CALR,LLEN_CALR,LINK_CALR,FLAG_CALR)                   96
C *                                                                       96
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALR',    96
     *BPATH_CALR,IDNUM_CALR,1, MAP_CALR,NAMES_CALR,COMMEN_CALR,           96
     *BANK_CALR,LEN_CALR,LLEN_CALR,LINK_CALR,FLAG_CALR)                   96
C use default CALR bank                                                   97
      BPATH_CALR='CALR*'                                                  97
      IDNUM_CALR(1)=0                                                     97
      CALL AGDGETP('CALBGEO','*',-999,'CALR',' ',BANK_CALR, BPATH_CALR,   97
     *IDNUM_CALR,1,MAP_CALR,NAMES_CALR,BANK_CALR,LEN_CALR,LLEN_CALR,      97
     *LINK_CALR,FLAG_CALR)                                                97
      CALL AGSBEGM(MODULE,IPRIN)                                          98
C Check IPRIN<0                                                           98
      IF (IPRIN.LT.0) RETURN                                              98
C Create CALB :                                                           98
C Call To Local Procedure CALB                                            98
      ASSIGN 5030 TO BLOCK_CALB                                           98
      GO TO  5020                                                         98
5030  CONTINUE                                                            98
C *                                                                       98
C Geant POSITION : CALB ===> MOTHER='CAVE' <===                           99
      CALL AGSRESET                                                       99
      AG_MOTHER = 'CAVE'                                                  99
      AG_TITLE = 'CALB'                                                   99
      AG_EXNAME = 'POSITION'                                              99
      AG_PARLIST = 'MOTHER'                                               99
      CALL AXPOSITION                                                     99
C *                                                                       99
C Check IPRIN>=1                                                          99
      IF (IPRIN.GE.1) WRITE(LOUT,5040)CALG_VERSION                        99
5040  FORMAT(' CALB geo. version =',F7.1)                                 101
C ----------------------------------------------------------------------  102
C ------------ CALB                                                       102
      CALL AGSENDM                                                        103
      RETURN                                                              103
C Begin Local Procedure CALB                                              103
C CALB IS EMC BARREL ENVELOPE                                             103
5020  CONTINUE                                                            103
         AG_TITLE='CALB'                                                  103
         AG_MODULE=MODULE                                                 103
         AG_MARK='V00'                                                    103
         CALL AGSPUSH                                                     103
         AG_MODULE=MODULE                                                 103
C    *                                                                    103
C    Geant MATERIAL : AIR ===> inherited arguments used <===              104
         CALL AGSRESET                                                    104
         AG_TITLE = 'AIR'                                                 104
         AG_EXNAME = 'MATERIAL'                                           104
         AG_PARLIST = 'NONE'                                              104
         CALL AXMATERIAL                                                  104
C    *                                                                    104
C    Geant ATTRIBUTE : CALB ===> SEEN=0 COLO=7 <===                       104
         CALL AGSRESET                                                    105
         AG_SEEN = 0                                                      105
         AG_COLO = 7                                                      105
         AG_TITLE = 'CALB'                                                105
         AG_EXNAME = 'ATTRIBUTE'                                          105
         AG_PARLIST = 'SEEN_COLO'                                         105
         CALL AXATTRIBUTE                                                 105
C    *                                                                    105
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={-HLENG, -CUT_LENGT  105
C    H,CUT_LENGTH,HLENG} RMN={CUT_RADIUS, CALG_RMIN,CALG_RMIN,CUT_RADIUS  105
C    } RMX={RMAX, RMAX,RMAX,RMAX} <===                                    105
         CALL AGSRESET                                                    109
         AG_PHI1 = 0                                                      109
         AG_DPHI = 360                                                    109
         AG_NZ = 4                                                        109
         AG_ZI(1)=-HLENG                                                  109
         AG_ZI(2)=-CUT_LENGTH                                             109
         AG_ZI(3)=CUT_LENGTH                                              109
         AG_ZI(4)=HLENG                                                   109
         AG_RMN(1)=CUT_RADIUS                                             109
         AG_RMN(2)=CALG_RMIN                                              109
         AG_RMN(3)=CALG_RMIN                                              109
         AG_RMN(4)=CUT_RADIUS                                             109
         AG_RMX(1)=RMAX                                                   109
         AG_RMX(2)=RMAX                                                   109
         AG_RMX(3)=RMAX                                                   109
         AG_RMX(4)=RMAX                                                   109
         AG_TITLE = 'PCON'                                                109
         AG_EXNAME = 'SHAPE'                                              109
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           109
         CALL AXSHAPE                                                     109
C    *                                                                    109
C    Check CALG_NMODULE(1)>0                                              109
         IF (CALG_NMODULE(1).GT.0) THEN                                   110
         II=1                                                             110
C    Create CHLV :                                                        112
C    Call To Local Procedure CHLV                                         112
         ASSIGN 5060 TO BLOCK_CHLV                                        112
         GO TO  5050                                                      112
5060     CONTINUE                                                         112
C    *                                                                    112
C    Geant POSITION : CHLV ===> inherited arguments used <===             113
         CALL AGSRESET                                                    113
         AG_TITLE = 'CHLV'                                                113
         AG_EXNAME = 'POSITION'                                           113
         AG_PARLIST = 'NONE'                                              113
         CALL AXPOSITION                                                  113
C    *                                                                    113
         ENDIF                                                            113
C    Check CALG_NMODULE(2)>0                                              115
         IF (CALG_NMODULE(2).GT.0) THEN                                   115
         II=2                                                             115
C    Create CHLV :                                                        117
C    Call To Local Procedure CHLV                                         117
         ASSIGN 5070 TO BLOCK_CHLV                                        117
         GO TO  5050                                                      117
5070     CONTINUE                                                         117
C    *                                                                    117
C    Geant POSITION : CHLV ===> THETAZ=180 <===                           118
         CALL AGSRESET                                                    118
         AG_THETAZ = 180                                                  118
         AG_TITLE = 'CHLV'                                                118
         AG_EXNAME = 'POSITION'                                           118
         AG_PARLIST = 'THETAZ'                                            118
         CALL AXPOSITION                                                  118
C    *                                                                    118
         ENDIF                                                            118
5021  CALL AGSPOPB                                                        120
      GO TO BLOCK_CALB, (5020 ,5030)                                      120
C End of Local Procedure                                                  120
C ----------------------------------------------------------------------  121
C ------------ CHLV                                                       121
C Begin Local Procedure CHLV                                              122
C CHLV CORRESPONDS TO DOUBLE MODULES...                                   122
5050  CONTINUE                                                            122
         AG_TITLE='CHLV'                                                  122
         AG_MODULE=MODULE                                                 122
         AG_MARK='V00'                                                    122
         CALL AGSPUSH                                                     122
         AG_MODULE=MODULE                                                 122
C    *                                                                    122
C    Geant SHAPE : PCON ===> PHI1=CALG_SHIFT(II) DPHI=DPHIMOD*CALG_NMODU  126
C    LE(II), NZ=3 ZI={0,CUT_LENGTH,HLENG}, RMN={CALG_RMIN,CALG_RMIN,CUT_  126
C    RADIUS} RMX={RMAX, RMAX,RMAX} <===                                   126
         CALL AGSRESET                                                    126
         AG_PHI1 = CALG_SHIFT(II)                                         126
         AG_DPHI = DPHIMOD*CALG_NMODULE(II)                               126
         AG_NZ = 3                                                        126
         AG_ZI(1)=0                                                       126
         AG_ZI(2)=CUT_LENGTH                                              126
         AG_ZI(3)=HLENG                                                   126
         AG_RMN(1)=CALG_RMIN                                              126
         AG_RMN(2)=CALG_RMIN                                              126
         AG_RMN(3)=CUT_RADIUS                                             126
         AG_RMX(1)=RMAX                                                   126
         AG_RMX(2)=RMAX                                                   126
         AG_RMX(3)=RMAX                                                   126
         AG_TITLE = 'PCON'                                                126
         AG_EXNAME = 'SHAPE'                                              126
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           126
         CALL AXSHAPE                                                     126
C    *                                                                    126
C    Create CPHI :                                                        127
C    Call To Local Procedure CPHI                                         127
         ASSIGN 5090 TO BLOCK_CPHI                                        127
         GO TO  5080                                                      127
5090     CONTINUE                                                         127
C    *                                                                    127
5051  CALL AGSPOPB                                                        128
      GO TO BLOCK_CHLV, (5050 ,5060,5070)                                 128
C End of Local Procedure                                                  128
C ----------------------------------------------------------------------  129
C ------------ CPHI                                                       129
C Begin Local Procedure CPHI                                              130
C CPHI CORRESPONDS TO A SINGLE MODULE                                     130
5080  CONTINUE                                                            130
         AG_TITLE='CPHI'                                                  130
         AG_MODULE=MODULE                                                 130
         AG_MARK='V00'                                                    130
         CALL AGSPUSH                                                     130
         AG_MODULE=MODULE                                                 130
C    *                                                                    130
C    Geant ATTRIBUTE : CPHI ===> SEEN=0 COLO=4 <===                       131
         CALL AGSRESET                                                    131
         AG_SEEN = 0                                                      131
         AG_COLO = 4                                                      131
         AG_TITLE = 'CPHI'                                                131
         AG_EXNAME = 'ATTRIBUTE'                                          131
         AG_PARLIST = 'SEEN_COLO'                                         131
         CALL AXATTRIBUTE                                                 131
C    *                                                                    131
C    Geant SHAPE : DIVISION ===> NDIV=CALG_NMODULE(II) IAXIS=2 <===       131
         CALL AGSRESET                                                    132
         AG_NDIV = CALG_NMODULE(II)                                       132
         AG_IAXIS = 2                                                     132
         AG_TITLE = 'DIVISION'                                            132
         AG_EXNAME = 'SHAPE'                                              132
         AG_PARLIST = 'NDIV_IAXIS'                                        132
         CALL AXSHAPE                                                     132
C    *                                                                    132
         CURRENT_DEPTH = CALG_RMIN                                        132
         C_DEP=CURRENT_DEPTH                                              134
         AG_DX = CALG_FRONTTHK                                            135
C    Create CBTW :                                                        135
C    Call To Local Procedure CBTW                                         135
         ASSIGN 5110 TO BLOCK_CBTW                                        135
         GO TO  5100                                                      135
5110     CONTINUE                                                         135
C    *                                                                    135
C    Geant POSITION : CBTW ===> X=CALG_RMIN+CALG_FRONTTHK Z=CURRENT_DEPT  136
C    H/TAN_THETA/2 <===                                                   136
         CALL AGSRESET                                                    136
         AG_X = CALG_RMIN+CALG_FRONTTHK                                   136
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 136
         AG_TITLE = 'CBTW'                                                136
         AG_EXNAME = 'POSITION'                                           136
         AG_PARLIST = 'X_Z'                                               136
         CALL AXPOSITION                                                  136
C    *                                                                    136
         CURRENT_DEPTH = CURRENT_DEPTH + 2*CALG_FRONTTHK                  136
         LAYER = 0                                                        138
C    *                                                                    139
         SUPER=1                                                          139
            GO TO 5123                                                    139
5121        SUPER=SUPER+1                                                 139
5123        IF(SUPER-(NINT(CALG_NSUPER)).GT.0) GO TO 5122                 139
C       Create CSUP :                                                     140
C       Call To Local Procedure CSUP                                      140
            ASSIGN 5140 TO BLOCK_CSUP                                     140
            GO TO  5130                                                   140
5140        CONTINUE                                                      140
C       *                                                                 140
C       Geant POSITION : CSUP ===> inherited arguments used <===          141
            CALL AGSRESET                                                 141
            AG_TITLE = 'CSUP'                                             141
            AG_EXNAME = 'POSITION'                                        141
            AG_PARLIST = 'NONE'                                           141
            CALL AXPOSITION                                               141
C       *                                                                 141
         GO TO 5121                                                       142
5122     CONTINUE                                                         142
         AG_DX = CALG_COMPTHK                                             143
C    Create CBTW :                                                        143
C    Call To Local Procedure CBTW                                         143
         ASSIGN 5150 TO BLOCK_CBTW                                        143
         GO TO  5100                                                      143
5150     CONTINUE                                                         143
C    *                                                                    143
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+CALG_COMPTHK Z=CURRENT_D  144
C    EPTH/TAN_THETA/2 <===                                                144
         CALL AGSRESET                                                    144
         AG_X = CURRENT_DEPTH+CALG_COMPTHK                                144
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 144
         AG_TITLE = 'CBTW'                                                144
         AG_EXNAME = 'POSITION'                                           144
         AG_PARLIST = 'X_Z'                                               144
         CALL AXPOSITION                                                  144
C    *                                                                    144
         C_DEP=2.*CALG_COMPTHK + 2.*CALG_AIRTHK                           144
         AG_DX = CALG_BACKTHK                                             146
C    Create CBTW :                                                        146
C    Call To Local Procedure CBTW                                         146
         ASSIGN 5160 TO BLOCK_CBTW                                        146
         GO TO  5100                                                      146
5160     CONTINUE                                                         146
C    *                                                                    146
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+C_DEP+CALG_BACKTHK Z=CUR  147
C    RENT_DEPTH/TAN_THETA/2 <===                                          147
         CALL AGSRESET                                                    147
         AG_X = CURRENT_DEPTH+C_DEP+CALG_BACKTHK                          147
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 147
         AG_TITLE = 'CBTW'                                                147
         AG_EXNAME = 'POSITION'                                           147
         AG_PARLIST = 'X_Z'                                               147
         CALL AXPOSITION                                                  147
C    *                                                                    147
         C_DEP=C_DEP + 2.*CALG_BACKTHK                                    147
         AG_DX = CALG_SPACETHK                                            149
C    Create CBTW :                                                        149
C    Call To Local Procedure CBTW                                         149
         ASSIGN 5170 TO BLOCK_CBTW                                        149
         GO TO  5100                                                      149
5170     CONTINUE                                                         149
C    *                                                                    149
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+C_DEP+CALG_SPACETHK Z=CU  150
C    RRENT_DEPTH/TAN_THETA/2 <===                                         150
         CALL AGSRESET                                                    150
         AG_X = CURRENT_DEPTH+C_DEP+CALG_SPACETHK                         150
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 150
         AG_TITLE = 'CBTW'                                                150
         AG_EXNAME = 'POSITION'                                           150
         AG_PARLIST = 'X_Z'                                               150
         CALL AXPOSITION                                                  150
C    *                                                                    150
         C_DEP=C_DEP + 2.*CALG_SPACETHK                                   150
         CURRENT_DEPTH=CURRENT_DEPTH + C_DEP                              152
5081  CALL AGSPOPB                                                        153
      GO TO BLOCK_CPHI, (5080 ,5090)                                      153
C End of Local Procedure                                                  153
C ----------------------------------------------------------------------  154
C ------------ CSUP                                                       154
C Begin Local Procedure CSUP                                              155
C CSUP IS A SUPER LAYER WITH FEW LAYERS INSIDE                            155
5130  CONTINUE                                                            155
         AG_TITLE='CSUP'                                                  155
         AG_MODULE=MODULE                                                 155
         AG_MARK='V00'                                                    155
         CALL AGSPUSH                                                     155
         AG_MODULE=MODULE                                                 155
C    *                                                                    155
         FUTURE_DEPTH=CURRENT_DEPTH + (CALG_NSUBLAY(SUPER) - SUPER +      157
     *   1)*LAYER_WIDTH(SUPER)*2 + (SMD_WIDTH + CALG_SCINTTHK(SUPER) +    157
     *   2.*CALG_ABPAPTHK)*2*(SUPER - 1)                                  157
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=6./21. <===                  158
         CALL AGSRESET                                                    158
         AG_A = 12.01                                                     158
         AG_Z = 6.                                                        158
         AG_W = 6./21.                                                    158
         AG_TITLE = 'C'                                                   158
         AG_EXNAME = 'COMPONENT'                                          158
         AG_PARLIST = 'A_Z_W'                                             158
         CALL AXCOMPONENT                                                 158
C    *                                                                    158
C    Geant COMPONENT : H ===> A=1. Z=1. W=10./21. <===                    158
         CALL AGSRESET                                                    159
         AG_A = 1.                                                        159
         AG_Z = 1.                                                        159
         AG_W = 10./21.                                                   159
         AG_TITLE = 'H'                                                   159
         AG_EXNAME = 'COMPONENT'                                          159
         AG_PARLIST = 'A_Z_W'                                             159
         CALL AXCOMPONENT                                                 159
C    *                                                                    159
C    Geant COMPONENT : O ===> A=16. Z=8. W=5./21. <===                    159
         CALL AGSRESET                                                    160
         AG_A = 16.                                                       160
         AG_Z = 8.                                                        160
         AG_W = 5./21.                                                    160
         AG_TITLE = 'O'                                                   160
         AG_EXNAME = 'COMPONENT'                                          160
         AG_PARLIST = 'A_Z_W'                                             160
         CALL AXCOMPONENT                                                 160
C    *                                                                    160
C    Geant MIXTURE : CELLULOSE ===> ISVOL=1 DENS=0.35 <===                160
         CALL AGSRESET                                                    161
         AG_ISVOL = 1                                                     161
         AG_DENS = 0.35                                                   161
         AG_TITLE = 'CELLULOSE'                                           161
         AG_EXNAME = 'MIXTURE'                                            161
         AG_PARLIST = 'ISVOL_DENS'                                        161
         CALL AXMIXTURE                                                   161
C    *                                                                    161
C    Geant ATTRIBUTE : CSUP ===> SEEN=0 COLO=1 <===                       161
         CALL AGSRESET                                                    162
         AG_SEEN = 0                                                      162
         AG_COLO = 1                                                      162
         AG_TITLE = 'CSUP'                                                162
         AG_EXNAME = 'ATTRIBUTE'                                          162
         AG_PARLIST = 'SEEN_COLO'                                         162
         CALL AXATTRIBUTE                                                 162
C    *                                                                    162
C    Geant SHAPE : PCON ===> PHI1=-3 DPHI=DPHIMOD NZ=3 ZI={0, CURRENT_DE  162
C    PTH/TAN_THETA,FUTURE_DEPTH/TAN_THETA}, RMN={CURRENT_DEPTH,CURRENT_D  162
C    EPTH,FUTURE_DEPTH}, RMX={FUTURE_DEPTH,FUTURE_DEPTH,FUTURE_DEPTH} <=  162
C    ==                                                                   162
         CALL AGSRESET                                                    166
         AG_PHI1 = -3                                                     166
         AG_DPHI = DPHIMOD                                                166
         AG_NZ = 3                                                        166
         AG_ZI(1)=0                                                       166
         AG_ZI(2)=CURRENT_DEPTH/TAN_THETA                                 166
         AG_ZI(3)=FUTURE_DEPTH/TAN_THETA                                  166
         AG_RMN(1)=CURRENT_DEPTH                                          166
         AG_RMN(2)=CURRENT_DEPTH                                          166
         AG_RMN(3)=FUTURE_DEPTH                                           166
         AG_RMX(1)=FUTURE_DEPTH                                           166
         AG_RMX(2)=FUTURE_DEPTH                                           166
         AG_RMX(3)=FUTURE_DEPTH                                           166
         AG_TITLE = 'PCON'                                                166
         AG_EXNAME = 'SHAPE'                                              166
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           166
         CALL AXSHAPE                                                     166
C    *                                                                    166
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               166
C    *                                                                    168
         SUB=1                                                            168
            GO TO 5183                                                    168
5181        SUB=SUB+1                                                     168
5183        IF(SUB-(NINT(CALG_NSUBLAY(SUPER))).GT.0) GO TO 5182           168
            LAYER = LAYER + 1                                             169
C       Check LAYER.LT.NINT(CALG_NSUBLAY(1)+CALG_NSUBLAY(2))              170
            IF (LAYER.LT.NINT(CALG_NSUBLAY(1)+CALG_NSUBLAY(2))) THEN      170
C       Create CSCI :                                                     171
C       Call To Local Procedure CSCI                                      171
            ASSIGN 5200 TO BLOCK_CSCI                                     171
            GO TO  5190                                                   171
5200        CONTINUE                                                      171
C       *                                                                 171
C       Geant POSITION : CSCI ===> X=CURRENT_DEPTH+CALG_SCINTTHK(SUPER)+  173
C       2.*CALG_ABPAPTHK Z=CURRENT_DEPTH/TAN_THETA/2 <===                 173
            CALL AGSRESET                                                 173
            AG_X = CURRENT_DEPTH+CALG_SCINTTHK(SUPER)+2.*CALG_ABPAPTHK    173
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              173
            AG_TITLE = 'CSCI'                                             173
            AG_EXNAME = 'POSITION'                                        173
            AG_PARLIST = 'X_Z'                                            173
            CALL AXPOSITION                                               173
C       *                                                                 173
C       Create CPBP :                                                     174
C       Call To Local Procedure CPBP                                      174
            ASSIGN 5220 TO BLOCK_CPBP                                     174
            GO TO  5210                                                   174
5220        CONTINUE                                                      174
C       *                                                                 174
            C_LEAD_DEP=2.*CALG_SCINTTHK(SUPER)+4.*CALG_ABPAPTHK           175
C       Geant POSITION : CPBP ===> X=CURRENT_DEPTH+C_LEAD_DEP+CALG_ABSOR  177
C       THK Z=CURRENT_DEPTH/TAN_THETA/2 <===                              177
            CALL AGSRESET                                                 177
            AG_X = CURRENT_DEPTH+C_LEAD_DEP+CALG_ABSORTHK                 177
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              177
            AG_TITLE = 'CPBP'                                             177
            AG_EXNAME = 'POSITION'                                        177
            AG_PARLIST = 'X_Z'                                            177
            CALL AXPOSITION                                               177
C       *                                                                 177
            CURRENT_DEPTH = CURRENT_DEPTH + 2*LAYER_WIDTH(SUPER)          177
            ELSE                                                          179
C       Create CSCI :                                                     180
C       Call To Local Procedure CSCI                                      180
            ASSIGN 5230 TO BLOCK_CSCI                                     180
            GO TO  5190                                                   180
5230        CONTINUE                                                      180
C       *                                                                 180
C       Geant POSITION : CSCI ===> X=CURRENT_DEPTH+CALG_SCINTTHK(2)+2.*C  182
C       ALG_ABPAPTHK Z=CURRENT_DEPTH/TAN_THETA/2 <===                     182
            CALL AGSRESET                                                 182
            AG_X = CURRENT_DEPTH+CALG_SCINTTHK(2)+2.*CALG_ABPAPTHK        182
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              182
            AG_TITLE = 'CSCI'                                             182
            AG_EXNAME = 'POSITION'                                        182
            AG_PARLIST = 'X_Z'                                            182
            CALL AXPOSITION                                               182
C       *                                                                 182
            CURRENT_DEPTH = CURRENT_DEPTH+C_LEAD_DEP                      182
            ENDIF                                                         184
C       Skip Unless LAYER==NINT(CALG_NSMD)                                185
            IF (LAYER.NE.NINT(CALG_NSMD))GO TO 5181                       185
C       Create CSMD :                                                     186
C       Call To Local Procedure CSMD                                      186
            ASSIGN 5250 TO BLOCK_CSMD                                     186
            GO TO  5240                                                   186
5250        CONTINUE                                                      186
C       *                                                                 186
C       Geant POSITION : CSMD ===> X=CURRENT_DEPTH+SMD_WIDTH Z=CURRENT_D  187
C       EPTH/TAN_THETA/2 <===                                             187
            CALL AGSRESET                                                 187
            AG_X = CURRENT_DEPTH+SMD_WIDTH                                187
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              187
            AG_TITLE = 'CSMD'                                             187
            AG_EXNAME = 'POSITION'                                        187
            AG_PARLIST = 'X_Z'                                            187
            CALL AXPOSITION                                               187
C       *                                                                 187
            CURRENT_DEPTH = CURRENT_DEPTH + 2*SMD_WIDTH                   187
         GO TO 5181                                                       189
5182     CONTINUE                                                         189
5131  CALL AGSPOPB                                                        190
      GO TO BLOCK_CSUP, (5130 ,5140)                                      190
C End of Local Procedure                                                  190
C ----------------------------------------------------------------------  191
C ------------ CPBP                                                       191
C Begin Local Procedure CPBP                                              192
C CPBP IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            192
5210  CONTINUE                                                            192
         AG_TITLE='CPBP'                                                  192
         AG_MODULE=MODULE                                                 192
         AG_MARK='V00'                                                    192
         CALL AGSPUSH                                                     192
         AG_MODULE=MODULE                                                 192
C    *                                                                    192
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             193
         CALL AGSRESET                                                    193
         AG_TITLE = 'LEAD'                                                193
         AG_EXNAME = 'MATERIAL'                                           193
         AG_PARLIST = 'NONE'                                              193
         CALL AXMATERIAL                                                  193
C    *                                                                    193
C    Geant MATERIAL : LEAD_CPBP ===> ISVOL=0 <===                         193
         CALL AGSRESET                                                    194
         AG_ISVOL = 0                                                     194
         AG_TITLE = 'LEAD_CPBP'                                           194
         AG_EXNAME = 'MATERIAL'                                           194
         AG_PARLIST = 'ISVOL'                                             194
         CALL AXMATERIAL                                                  194
C    *                                                                    194
C    Geant ATTRIBUTE : CPBP ===> SEEN=1 COLO=1 <===                       194
         CALL AGSRESET                                                    195
         AG_SEEN = 1                                                      195
         AG_COLO = 1                                                      195
         AG_TITLE = 'CPBP'                                                195
         AG_EXNAME = 'ATTRIBUTE'                                          195
         AG_PARLIST = 'SEEN_COLO'                                         195
         CALL AXATTRIBUTE                                                 195
C    *                                                                    195
C    Geant SHAPE : BOX ===> DX=CALG_ABSORTHK DY=CURRENT_DEPTH*TAN(TWOPI/  195
C    360*DPHIT)-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===           195
         CALL AGSRESET                                                    197
         AG_DX = CALG_ABSORTHK                                            197
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          197
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                197
         AG_TITLE = 'BOX'                                                 197
         AG_EXNAME = 'SHAPE'                                              197
         AG_PARLIST = 'DX_DY_DZ'                                          197
         CALL AXSHAPE                                                     197
C    *                                                                    197
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               197
5211  CALL AGSPOPB                                                        199
      GO TO BLOCK_CPBP, (5210 ,5220)                                      199
C End of Local Procedure                                                  199
C ----------------------------------------------------------------------  200
C ------------ CSCI                                                       200
C Begin Local Procedure CSCI                                              201
C CSCI A SCINTILLATOR LAYER.                                              201
5190  CONTINUE                                                            201
         AG_TITLE='CSCI'                                                  201
         AG_MODULE=MODULE                                                 201
         AG_MARK='V00'                                                    201
         CALL AGSPUSH                                                     201
         AG_MODULE=MODULE                                                 201
C    *                                                                    201
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       202
         CALL AGSRESET                                                    202
         AG_TITLE = 'POLYSTYREN'                                          202
         AG_EXNAME = 'MATERIAL'                                           202
         AG_PARLIST = 'NONE'                                              202
         CALL AXMATERIAL                                                  202
C    *                                                                    202
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       202
         CALL AGSRESET                                                    203
         AG_ISVOL = 1                                                     203
         AG_TITLE = 'CPOLYSTYREN'                                         203
         AG_EXNAME = 'MATERIAL'                                           203
         AG_PARLIST = 'ISVOL'                                             203
         CALL AXMATERIAL                                                  203
C    *                                                                    203
C    Geant ATTRIBUTE : CSCI ===> SEEN=1 COLO=4 <===                       203
         CALL AGSRESET                                                    204
         AG_SEEN = 1                                                      204
         AG_COLO = 4                                                      204
         AG_TITLE = 'CSCI'                                                204
         AG_EXNAME = 'ATTRIBUTE'                                          204
         AG_PARLIST = 'SEEN_COLO'                                         204
         CALL AXATTRIBUTE                                                 204
C    *                                                                    204
C    Geant SHAPE : BOX ===> DX=CALG_SCINTTHK(SUPER) DY=CURRENT_DEPTH*TAN  204
C    (TWOPI/360*DPHIT)-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===    204
         CALL AGSRESET                                                    206
         AG_DX = CALG_SCINTTHK(SUPER)                                     206
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          206
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                206
         AG_TITLE = 'BOX'                                                 206
         AG_EXNAME = 'SHAPE'                                              206
         AG_PARLIST = 'DX_DY_DZ'                                          206
         CALL AXSHAPE                                                     206
C    *                                                                    206
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               206
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                208
         CALL GSTPAR(AG_IMED, 'BIRK2', RKB2SC)                            209
         CALL GSTPAR(AG_IMED, 'BIRK3', RKB3SC)                            210
C    Defining Hits: ETA:DETA:(0,1) Y:1:(-13,+13) BIRK:0:(0,10)            211
         AG_TITLE = 'CSUP'                                                211
         AG_EXNAME = 'HITS'                                               211
         AG_TYPE=AG_MAGIC                                                 211
         CALL AGSDETV('CSUP')                                             211
C    *                                                                    211
         AG_PARLIST='ETA'                                                 211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = AG_MAGIC                                              211
         AG_BIN=DETA                                                      211
         AG_OPTION='*'                                                    211
         AG_FHMIN = 0                                                     211
         AG_FHMAX = 1                                                     211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='Y'                                                   211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = 1                                                     211
         AG_BIN=AG_MAGIC                                                  211
         AG_OPTION='*'                                                    211
         AG_FHMIN = -13                                                   211
         AG_FHMAX = +13                                                   211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='BIRK'                                                211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = 0                                                     211
         AG_BIN=AG_MAGIC                                                  211
         AG_OPTION='*'                                                    211
         AG_FHMIN = 0                                                     211
         AG_FHMAX = 10                                                    211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         CALL AGSDETU('CSUP')                                             211
C    *                                                                    211
5191  CALL AGSPOPB                                                        212
      GO TO BLOCK_CSCI, (5190 ,5200,5230)                                 212
C End of Local Procedure                                                  212
C ----------------------------------------------------------------------  213
C ------------ CBTW                                                       213
C Begin Local Procedure CBTW                                              214
C CBTW IS THE MODULE FRONT BACK PLATE                                     214
5100  CONTINUE                                                            214
         AG_TITLE='CBTW'                                                  214
         AG_MODULE=MODULE                                                 214
         AG_MARK='V00'                                                    214
         CALL AGSPUSH                                                     214
         AG_MODULE=MODULE                                                 214
C    *                                                                    214
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        215
         CALL AGSRESET                                                    215
         AG_TITLE = 'ALUMINIUM'                                           215
         AG_EXNAME = 'MATERIAL'                                           215
         AG_PARLIST = 'NONE'                                              215
         CALL AXMATERIAL                                                  215
C    *                                                                    215
C    Geant MATERIAL : EALUMINIUM ===> ISVOL=1 <===                        215
         CALL AGSRESET                                                    216
         AG_ISVOL = 1                                                     216
         AG_TITLE = 'EALUMINIUM'                                          216
         AG_EXNAME = 'MATERIAL'                                           216
         AG_PARLIST = 'ISVOL'                                             216
         CALL AXMATERIAL                                                  216
C    *                                                                    216
C    Geant ATTRIBUTE : CBTW ===> SEEN=1 COLO=6 <===                       216
         CALL AGSRESET                                                    217
         AG_SEEN = 1                                                      217
         AG_COLO = 6                                                      217
         AG_TITLE = 'CBTW'                                                217
         AG_EXNAME = 'ATTRIBUTE'                                          217
         AG_PARLIST = 'SEEN_COLO'                                         217
         CALL AXATTRIBUTE                                                 217
C    *                                                                    217
C    Geant SHAPE : BOX ===> DY=CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_C  217
C    RACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===                            217
         CALL AGSRESET                                                    219
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          219
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                219
         AG_TITLE = 'BOX'                                                 219
         AG_EXNAME = 'SHAPE'                                              219
         AG_PARLIST = 'DY_DZ'                                             219
         CALL AXSHAPE                                                     219
C    *                                                                    219
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               219
5101  CALL AGSPOPB                                                        221
      GO TO BLOCK_CBTW, (5100 ,5110,5150,5160,5170)                       221
C End of Local Procedure                                                  221
C ----------------------------------------------------------------------  222
C ------------ CSMD                                                       222
C Begin Local Procedure CSMD                                              223
C CSMD IS THE SHOWER MAXIMUM DETECTOR ENVELOPE                            223
5240  CONTINUE                                                            223
         AG_TITLE='CSMD'                                                  223
         AG_MODULE=MODULE                                                 223
         AG_MARK='V00'                                                    223
         CALL AGSPUSH                                                     223
         AG_MODULE=MODULE                                                 223
C    *                                                                    223
         CURRENT = - SMD_WIDTH                                            224
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=6./21. <===                  225
         CALL AGSRESET                                                    225
         AG_A = 12.01                                                     225
         AG_Z = 6.                                                        225
         AG_W = 6./21.                                                    225
         AG_TITLE = 'C'                                                   225
         AG_EXNAME = 'COMPONENT'                                          225
         AG_PARLIST = 'A_Z_W'                                             225
         CALL AXCOMPONENT                                                 225
C    *                                                                    225
C    Geant COMPONENT : H ===> A=1. Z=1. W=10./21. <===                    225
         CALL AGSRESET                                                    226
         AG_A = 1.                                                        226
         AG_Z = 1.                                                        226
         AG_W = 10./21.                                                   226
         AG_TITLE = 'H'                                                   226
         AG_EXNAME = 'COMPONENT'                                          226
         AG_PARLIST = 'A_Z_W'                                             226
         CALL AXCOMPONENT                                                 226
C    *                                                                    226
C    Geant COMPONENT : O ===> A=16. Z=8. W=5./21. <===                    226
         CALL AGSRESET                                                    227
         AG_A = 16.                                                       227
         AG_Z = 8.                                                        227
         AG_W = 5./21.                                                    227
         AG_TITLE = 'O'                                                   227
         AG_EXNAME = 'COMPONENT'                                          227
         AG_PARLIST = 'A_Z_W'                                             227
         CALL AXCOMPONENT                                                 227
C    *                                                                    227
C    Geant MIXTURE : CELLULOSE ===> ISVOL=1 DENS=0.35 <===                227
         CALL AGSRESET                                                    228
         AG_ISVOL = 1                                                     228
         AG_DENS = 0.35                                                   228
         AG_TITLE = 'CELLULOSE'                                           228
         AG_EXNAME = 'MIXTURE'                                            228
         AG_PARLIST = 'ISVOL_DENS'                                        228
         CALL AXMIXTURE                                                   228
C    *                                                                    228
C    Geant ATTRIBUTE : CSMD ===> SEEN=1 COLO=6 <===                       228
         CALL AGSRESET                                                    229
         AG_SEEN = 1                                                      229
         AG_COLO = 6                                                      229
         AG_TITLE = 'CSMD'                                                229
         AG_EXNAME = 'ATTRIBUTE'                                          229
         AG_PARLIST = 'SEEN_COLO'                                         229
         CALL AXATTRIBUTE                                                 229
C    *                                                                    229
C    Geant SHAPE : BOX ===> DX=SMD_WIDTH DY=CURRENT_DEPTH*TAN(TWOPI/120.  229
C    )-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===                    229
         CALL AGSRESET                                                    231
         AG_DX = SMD_WIDTH                                                231
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/120.)-CALG_CRACKWD               231
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                231
         AG_TITLE = 'BOX'                                                 231
         AG_EXNAME = 'SHAPE'                                              231
         AG_PARLIST = 'DX_DY_DZ'                                          231
         CALL AXSHAPE                                                     231
C    *                                                                    231
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              231
C    Create CSMG :                                                        233
C    Call To Local Procedure CSMG                                         233
         ASSIGN 5270 TO BLOCK_CSMG                                        233
         GO TO  5260                                                      233
5270     CONTINUE                                                         233
C    *                                                                    233
C    Geant POSITION : CSMG ===> X=-SMD_WIDTH+CALG_G10SBTHK+2.*CALG_ABPAP  234
C    THK <===                                                             234
         CALL AGSRESET                                                    234
         AG_X = -SMD_WIDTH+CALG_G10SBTHK+2.*CALG_ABPAPTHK                 234
         AG_TITLE = 'CSMG'                                                234
         AG_EXNAME = 'POSITION'                                           234
         AG_PARLIST = 'X'                                                 234
         CALL AXPOSITION                                                  234
C    *                                                                    234
C    Geant POSITION : CSMG ===> X=SMD_WIDTH-CALG_G10SBTHK-2.*CALG_ABPAPT  234
C    HK <===                                                              234
         CALL AGSRESET                                                    235
         AG_X = SMD_WIDTH-CALG_G10SBTHK-2.*CALG_ABPAPTHK                  235
         AG_TITLE = 'CSMG'                                                235
         AG_EXNAME = 'POSITION'                                           235
         AG_PARLIST = 'X'                                                 235
         CALL AXPOSITION                                                  235
C    *                                                                    235
         CURRENT = CURRENT + 2.*CALG_G10SBTHK + 2.*CALG_ABPAPTHK          235
C    *                                                                    237
         J=1                                                              237
            GO TO 5283                                                    237
5281        J=J+1                                                         237
5283        IF(J-(4).GT.0) GO TO 5282                                     237
            CURRENT_CSDA=-CURRENT_DEPTH/TAN_THETA/2                       238
C       Check J.EQ.1                                                      239
            IF (J.EQ.1) THEN                                              239
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA1WDH+CALG_SET12WDH)        240
C       Create CSDA :                                                     241
C       Call To Local Procedure CSDA                                      241
            ASSIGN 5300 TO BLOCK_CSDA                                     241
            GO TO  5290                                                   241
5300        CONTINUE                                                      241
C       *                                                                 241
C       Geant POSITION : CSDA ===> X=CURRENT+CALG_SMALFTHK Z=CURRENT_CSD  243
C       A+2.*CALG_SMETAWDH+ETA_LENGHT <===                                243
            CALL AGSRESET                                                 243
            AG_X = CURRENT+CALG_SMALFTHK                                  243
            AG_Z = CURRENT_CSDA+2.*CALG_SMETAWDH+ETA_LENGHT               243
            AG_TITLE = 'CSDA'                                             243
            AG_EXNAME = 'POSITION'                                        243
            AG_PARLIST = 'X_Z'                                            243
            CALL AXPOSITION                                               243
C       *                                                                 243
            ELSEIF (J.EQ.2) THEN                                          243
            CURRENT_CSDA=CURRENT_CSDA+2.*(CALG_SMETAWDH+ETA_LENGHT)       245
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA2WDH+CALG_SET12WDH)        246
C       Create CSDA :                                                     247
C       Call To Local Procedure CSDA                                      247
            ASSIGN 5310 TO BLOCK_CSDA                                     247
            GO TO  5290                                                   247
5310        CONTINUE                                                      247
C       *                                                                 247
C       Geant POSITION : CSDA ===> X=CURRENT+CALG_SMALFTHK Z=CURRENT_CSD  248
C       A+ETA_LENGHT <===                                                 248
            CALL AGSRESET                                                 248
            AG_X = CURRENT+CALG_SMALFTHK                                  248
            AG_Z = CURRENT_CSDA+ETA_LENGHT                                248
            AG_TITLE = 'CSDA'                                             248
            AG_EXNAME = 'POSITION'                                        248
            AG_PARLIST = 'X_Z'                                            248
            CALL AXPOSITION                                               248
C       *                                                                 248
            ELSEIF (J.EQ.3) THEN                                          248
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA1WDH+CALG_SET12WDH)        250
C       Create CSDA :                                                     251
C       Call To Local Procedure CSDA                                      251
            ASSIGN 5320 TO BLOCK_CSDA                                     251
            GO TO  5290                                                   251
5320        CONTINUE                                                      251
C       *                                                                 251
C       Geant POSITION : CSDA ===> X=CURRENT+3.*CALG_SMALFTHK Z=CURRENT_  253
C       CSDA+2.*CALG_SMETAWDH+ETA_LENGHT THETAX=90 PHIX=180 <===          253
            CALL AGSRESET                                                 253
            AG_X = CURRENT+3.*CALG_SMALFTHK                               253
            AG_Z = CURRENT_CSDA+2.*CALG_SMETAWDH+ETA_LENGHT               253
            AG_THETAX = 90                                                253
            AG_PHIX = 180                                                 253
            AG_TITLE = 'CSDA'                                             253
            AG_EXNAME = 'POSITION'                                        253
            AG_PARLIST = 'X_Z_THETAX_PHIX'                                253
            CALL AXPOSITION                                               253
C       *                                                                 253
            ELSEIF (J.EQ.4) THEN                                          253
            CURRENT_CSDA=CURRENT_CSDA+2.*(CALG_SMETAWDH+ETA_LENGHT)       255
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA2WDH+CALG_SET12WDH)        256
C       Create CSDA :                                                     257
C       Call To Local Procedure CSDA                                      257
            ASSIGN 5330 TO BLOCK_CSDA                                     257
            GO TO  5290                                                   257
5330        CONTINUE                                                      257
C       *                                                                 257
C       Geant POSITION : CSDA ===> X=CURRENT+3.*CALG_SMALFTHK Z=CURRENT_  259
C       CSDA+ETA_LENGHT THETAX=90 PHIX=180 <===                           259
            CALL AGSRESET                                                 259
            AG_X = CURRENT+3.*CALG_SMALFTHK                               259
            AG_Z = CURRENT_CSDA+ETA_LENGHT                                259
            AG_THETAX = 90                                                259
            AG_PHIX = 180                                                 259
            AG_TITLE = 'CSDA'                                             259
            AG_EXNAME = 'POSITION'                                        259
            AG_PARLIST = 'X_Z_THETAX_PHIX'                                259
            CALL AXPOSITION                                               259
C       *                                                                 259
            ENDIF                                                         259
         GO TO 5281                                                       261
5282     CONTINUE                                                         261
C    Create CSMC :                                                        262
C    Call To Local Procedure CSMC                                         262
         ASSIGN 5350 TO BLOCK_CSMC                                        262
         GO TO  5340                                                      262
5350     CONTINUE                                                         262
C    *                                                                    262
C    Geant POSITION : CSMC ===> X=CURRENT+CALG_SMALFTHK Y=CALG_SMALFWDH+  263
C    CALG_SMAFFWDH <===                                                   263
         CALL AGSRESET                                                    263
         AG_X = CURRENT+CALG_SMALFTHK                                     263
         AG_Y = CALG_SMALFWDH+CALG_SMAFFWDH                               263
         AG_TITLE = 'CSMC'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'X_Y'                                               263
         CALL AXPOSITION                                                  263
C    *                                                                    263
C    Geant POSITION : CSMC ===> X=CURRENT+CALG_SMALFTHK Y=-CALG_SMALFWDH  263
C    -CALG_SMAFFWDH <===                                                  263
         CALL AGSRESET                                                    264
         AG_X = CURRENT+CALG_SMALFTHK                                     264
         AG_Y = -CALG_SMALFWDH-CALG_SMAFFWDH                              264
         AG_TITLE = 'CSMC'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'X_Y'                                               264
         CALL AXPOSITION                                                  264
C    *                                                                    264
C    Create CSMB :                                                        265
C    Call To Local Procedure CSMB                                         265
         ASSIGN 5370 TO BLOCK_CSMB                                        265
         GO TO  5360                                                      265
5370     CONTINUE                                                         265
C    *                                                                    265
C    Geant POSITION : CSMB ===> X=CURRENT+3.*CALG_SMALFTHK Y=CALG_SMALFW  266
C    DH+CALG_SMAFBWDH <===                                                266
         CALL AGSRESET                                                    266
         AG_X = CURRENT+3.*CALG_SMALFTHK                                  266
         AG_Y = CALG_SMALFWDH+CALG_SMAFBWDH                               266
         AG_TITLE = 'CSMB'                                                266
         AG_EXNAME = 'POSITION'                                           266
         AG_PARLIST = 'X_Y'                                               266
         CALL AXPOSITION                                                  266
C    *                                                                    266
C    Geant POSITION : CSMB ===> X=CURRENT+3.*CALG_SMALFTHK Y=-CALG_SMALF  266
C    WDH-CALG_SMAFBWDH <===                                               266
         CALL AGSRESET                                                    267
         AG_X = CURRENT+3.*CALG_SMALFTHK                                  267
         AG_Y = -CALG_SMALFWDH-CALG_SMAFBWDH                              267
         AG_TITLE = 'CSMB'                                                267
         AG_EXNAME = 'POSITION'                                           267
         AG_PARLIST = 'X_Y'                                               267
         CALL AXPOSITION                                                  267
C    *                                                                    267
5241  CALL AGSPOPB                                                        268
      GO TO BLOCK_CSMD, (5240 ,5250)                                      268
C End of Local Procedure                                                  268
C ----------------------------------------------------------------------  269
C ------------ CSMG                                                       269
C Begin Local Procedure CSMG                                              270
C CSMG IS G10 FRONT BACK PLATE                                            270
5260  CONTINUE                                                            270
         AG_TITLE='CSMG'                                                  270
         AG_MODULE=MODULE                                                 270
         AG_MARK='V00'                                                    270
         CALL AGSPUSH                                                     270
         AG_MODULE=MODULE                                                 270
C    *                                                                    270
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          271
         CALL AGSRESET                                                    271
         AG_A = 28.08                                                     271
         AG_Z = 14                                                        271
         AG_W = 0.6*1*28./60.                                             271
         AG_TITLE = 'SI'                                                  271
         AG_EXNAME = 'COMPONENT'                                          271
         AG_PARLIST = 'A_Z_W'                                             271
         CALL AXCOMPONENT                                                 271
C    *                                                                    271
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               271
         CALL AGSRESET                                                    272
         AG_A = 16                                                        272
         AG_Z = 8                                                         272
         AG_W = 0.6*2*16./60.                                             272
         AG_TITLE = 'O'                                                   272
         AG_EXNAME = 'COMPONENT'                                          272
         AG_PARLIST = 'A_Z_W'                                             272
         CALL AXCOMPONENT                                                 272
C    *                                                                    272
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              272
         CALL AGSRESET                                                    273
         AG_A = 12                                                        273
         AG_Z = 6                                                         273
         AG_W = 0.4*8*12./174.                                            273
         AG_TITLE = 'C'                                                   273
         AG_EXNAME = 'COMPONENT'                                          273
         AG_PARLIST = 'A_Z_W'                                             273
         CALL AXCOMPONENT                                                 273
C    *                                                                    273
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               273
         CALL AGSRESET                                                    274
         AG_A = 1                                                         274
         AG_Z = 1                                                         274
         AG_W = 0.4*14*1./174.                                            274
         AG_TITLE = 'H'                                                   274
         AG_EXNAME = 'COMPONENT'                                          274
         AG_PARLIST = 'A_Z_W'                                             274
         CALL AXCOMPONENT                                                 274
C    *                                                                    274
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              274
         CALL AGSRESET                                                    275
         AG_A = 16                                                        275
         AG_Z = 8                                                         275
         AG_W = 0.4*4*16./174.                                            275
         AG_TITLE = 'O'                                                   275
         AG_EXNAME = 'COMPONENT'                                          275
         AG_PARLIST = 'A_Z_W'                                             275
         CALL AXCOMPONENT                                                 275
C    *                                                                    275
C    Geant MIXTURE : G10 ===> ISVOL=0 DENS=1.7 <===                       275
         CALL AGSRESET                                                    276
         AG_ISVOL = 0                                                     276
         AG_DENS = 1.7                                                    276
         AG_TITLE = 'G10'                                                 276
         AG_EXNAME = 'MIXTURE'                                            276
         AG_PARLIST = 'ISVOL_DENS'                                        276
         CALL AXMIXTURE                                                   276
C    *                                                                    276
C    Geant ATTRIBUTE : CSMG ===> SEEN=1 COLO=1 <===                       276
         CALL AGSRESET                                                    277
         AG_SEEN = 1                                                      277
         AG_COLO = 1                                                      277
         AG_TITLE = 'CSMG'                                                277
         AG_EXNAME = 'ATTRIBUTE'                                          277
         AG_PARLIST = 'SEEN_COLO'                                         277
         CALL AXATTRIBUTE                                                 277
C    *                                                                    277
C    Geant SHAPE : BOX ===> DX=CALG_G10SBTHK <===                         277
         CALL AGSRESET                                                    278
         AG_DX = CALG_G10SBTHK                                            278
         AG_TITLE = 'BOX'                                                 278
         AG_EXNAME = 'SHAPE'                                              278
         AG_PARLIST = 'DX'                                                278
         CALL AXSHAPE                                                     278
C    *                                                                    278
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              278
5261  CALL AGSPOPB                                                        280
      GO TO BLOCK_CSMG, (5260 ,5270)                                      280
C End of Local Procedure                                                  280
C ----------------------------------------------------------------------  281
C ------------ CSDA                                                       281
C Begin Local Procedure CSDA                                              282
C CSDA IS AL BLOCK WITH SENSITIVE GAS VOLUME                              282
5290  CONTINUE                                                            282
         AG_TITLE='CSDA'                                                  282
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
C    Geant MATERIAL : ALU_CSDA ===> ISVOL=0 <===                          283
         CALL AGSRESET                                                    284
         AG_ISVOL = 0                                                     284
         AG_TITLE = 'ALU_CSDA'                                            284
         AG_EXNAME = 'MATERIAL'                                           284
         AG_PARLIST = 'ISVOL'                                             284
         CALL AXMATERIAL                                                  284
C    *                                                                    284
C    Geant ATTRIBUTE : CSDA ===> SEEN=1 COLO=6 SERIAL=J <===              284
         CALL AGSRESET                                                    285
         AG_SEEN = 1                                                      285
         AG_COLO = 6                                                      285
         AG_SERIAL = J                                                    285
         AG_TITLE = 'CSDA'                                                285
         AG_EXNAME = 'ATTRIBUTE'                                          285
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  285
         CALL AXATTRIBUTE                                                 285
C    *                                                                    285
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMALFWDH, DZ=ETA_LE  285
C    NGHT <===                                                            285
         CALL AGSRESET                                                    287
         AG_DX = CALG_SMALFTHK                                            287
         AG_DY = CALG_SMALFWDH                                            287
         AG_DZ = ETA_LENGHT                                               287
         AG_TITLE = 'BOX'                                                 287
         AG_EXNAME = 'SHAPE'                                              287
         AG_PARLIST = 'DX_DY_DZ'                                          287
         CALL AXSHAPE                                                     287
C    *                                                                    287
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              287
C    Create CSME :                                                        289
C    Call To Local Procedure CSME                                         289
         ASSIGN 5390 TO BLOCK_CSME                                        289
         GO TO  5380                                                      289
5390     CONTINUE                                                         289
C    *                                                                    289
5291  CALL AGSPOPB                                                        290
      GO TO BLOCK_CSDA, (5290 ,5300,5310,5320,5330)                       290
C End of Local Procedure                                                  290
C ----------------------------------------------------------------------  291
C ------------ CSMC                                                       291
C Begin Local Procedure CSMC                                              292
C CSMC IS THE FRONT FIRST (LAST) AL RIB                                   292
5340  CONTINUE                                                            292
         AG_TITLE='CSMC'                                                  292
         AG_MODULE=MODULE                                                 292
         AG_MARK='V00'                                                    292
         CALL AGSPUSH                                                     292
         AG_MODULE=MODULE                                                 292
C    *                                                                    292
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        293
         CALL AGSRESET                                                    293
         AG_TITLE = 'ALUMINIUM'                                           293
         AG_EXNAME = 'MATERIAL'                                           293
         AG_PARLIST = 'NONE'                                              293
         CALL AXMATERIAL                                                  293
C    *                                                                    293
C    Geant MATERIAL : ALU_CSMC ===> ISVOL=0 <===                          293
         CALL AGSRESET                                                    294
         AG_ISVOL = 0                                                     294
         AG_TITLE = 'ALU_CSMC'                                            294
         AG_EXNAME = 'MATERIAL'                                           294
         AG_PARLIST = 'ISVOL'                                             294
         CALL AXMATERIAL                                                  294
C    *                                                                    294
C    Geant ATTRIBUTE : CSMC ===> SEEN=1 COLO=6 <===                       294
         CALL AGSRESET                                                    295
         AG_SEEN = 1                                                      295
         AG_COLO = 6                                                      295
         AG_TITLE = 'CSMC'                                                295
         AG_EXNAME = 'ATTRIBUTE'                                          295
         AG_PARLIST = 'SEEN_COLO'                                         295
         CALL AXATTRIBUTE                                                 295
C    *                                                                    295
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMAFFWDH <===        295
         CALL AGSRESET                                                    296
         AG_DX = CALG_SMALFTHK                                            296
         AG_DY = CALG_SMAFFWDH                                            296
         AG_TITLE = 'BOX'                                                 296
         AG_EXNAME = 'SHAPE'                                              296
         AG_PARLIST = 'DX_DY'                                             296
         CALL AXSHAPE                                                     296
C    *                                                                    296
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              296
5341  CALL AGSPOPB                                                        298
      GO TO BLOCK_CSMC, (5340 ,5350)                                      298
C End of Local Procedure                                                  298
C ----------------------------------------------------------------------  299
C ------------ CSMB                                                       299
C Begin Local Procedure CSMB                                              300
C CSMB IS THE BACK FIRST (LAST) AL RIB                                    300
5360  CONTINUE                                                            300
         AG_TITLE='CSMB'                                                  300
         AG_MODULE=MODULE                                                 300
         AG_MARK='V00'                                                    300
         CALL AGSPUSH                                                     300
         AG_MODULE=MODULE                                                 300
C    *                                                                    300
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        301
         CALL AGSRESET                                                    301
         AG_TITLE = 'ALUMINIUM'                                           301
         AG_EXNAME = 'MATERIAL'                                           301
         AG_PARLIST = 'NONE'                                              301
         CALL AXMATERIAL                                                  301
C    *                                                                    301
C    Geant MATERIAL : ALU_CSMB ===> ISVOL=0 <===                          301
         CALL AGSRESET                                                    302
         AG_ISVOL = 0                                                     302
         AG_TITLE = 'ALU_CSMB'                                            302
         AG_EXNAME = 'MATERIAL'                                           302
         AG_PARLIST = 'ISVOL'                                             302
         CALL AXMATERIAL                                                  302
C    *                                                                    302
C    Geant ATTRIBUTE : CSMB ===> SEEN=1 COLO=6 <===                       302
         CALL AGSRESET                                                    303
         AG_SEEN = 1                                                      303
         AG_COLO = 6                                                      303
         AG_TITLE = 'CSMB'                                                303
         AG_EXNAME = 'ATTRIBUTE'                                          303
         AG_PARLIST = 'SEEN_COLO'                                         303
         CALL AXATTRIBUTE                                                 303
C    *                                                                    303
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMAFBWDH <===        303
         CALL AGSRESET                                                    304
         AG_DX = CALG_SMALFTHK                                            304
         AG_DY = CALG_SMAFBWDH                                            304
         AG_TITLE = 'BOX'                                                 304
         AG_EXNAME = 'SHAPE'                                              304
         AG_PARLIST = 'DX_DY'                                             304
         CALL AXSHAPE                                                     304
C    *                                                                    304
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              304
5361  CALL AGSPOPB                                                        306
      GO TO BLOCK_CSMB, (5360 ,5370)                                      306
C End of Local Procedure                                                  306
C ----------------------------------------------------------------------  307
C ------------ CSME                                                       307
C Begin Local Procedure CSME                                              308
C CSME IS THE PART OF CSDA AL BOX WITH AR/CO2 SENSITEVE GAS               308
5380  CONTINUE                                                            308
         AG_TITLE='CSME'                                                  308
         AG_MODULE=MODULE                                                 308
         AG_MARK='V00'                                                    308
         CALL AGSPUSH                                                     308
         AG_MODULE=MODULE                                                 308
C    *                                                                    308
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        309
         CALL AGSRESET                                                    309
         AG_TITLE = 'ALUMINIUM'                                           309
         AG_EXNAME = 'MATERIAL'                                           309
         AG_PARLIST = 'NONE'                                              309
         CALL AXMATERIAL                                                  309
C    *                                                                    309
C    Geant MATERIAL : ALU_CSME ===> ISVOL=0 <===                          309
         CALL AGSRESET                                                    310
         AG_ISVOL = 0                                                     310
         AG_TITLE = 'ALU_CSME'                                            310
         AG_EXNAME = 'MATERIAL'                                           310
         AG_PARLIST = 'ISVOL'                                             310
         CALL AXMATERIAL                                                  310
C    *                                                                    310
C    Geant ATTRIBUTE : CSME ===> SEEN=1 COLO=6 <===                       310
         CALL AGSRESET                                                    311
         AG_SEEN = 1                                                      311
         AG_COLO = 6                                                      311
         AG_TITLE = 'CSME'                                                311
         AG_EXNAME = 'ATTRIBUTE'                                          311
         AG_PARLIST = 'SEEN_COLO'                                         311
         CALL AXATTRIBUTE                                                 311
C    *                                                                    311
C    Geant SHAPE : DIVISION ===> NDIV=NINT(CALG_NSMDALW) IAXIS=2 <===     311
         CALL AGSRESET                                                    312
         AG_NDIV = NINT(CALG_NSMDALW)                                     312
         AG_IAXIS = 2                                                     312
         AG_TITLE = 'DIVISION'                                            312
         AG_EXNAME = 'SHAPE'                                              312
         AG_PARLIST = 'NDIV_IAXIS'                                        312
         CALL AXSHAPE                                                     312
C    *                                                                    312
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              312
C    *                                                                    314
         I=1                                                              314
            GO TO 5403                                                    314
5401        I=I+1                                                         314
5403        IF(I-(2).GT.0) GO TO 5402                                     314
C       Create CSHI :                                                     315
C       Call To Local Procedure CSHI                                      315
            ASSIGN 5420 TO BLOCK_CSHI                                     315
            GO TO  5410                                                   315
5420        CONTINUE                                                      315
C       *                                                                 315
C       Check I.EQ.1                                                      316
            IF (I.EQ.1) THEN                                              316
C       Geant POSITION : CSHI ===> X=-CALG_SMALFTHK+CALG_SMGASTHK <===    317
            CALL AGSRESET                                                 317
            AG_X = -CALG_SMALFTHK+CALG_SMGASTHK                           317
            AG_TITLE = 'CSHI'                                             317
            AG_EXNAME = 'POSITION'                                        317
            AG_PARLIST = 'X'                                              317
            CALL AXPOSITION                                               317
C       *                                                                 317
            ELSE                                                          317
C       Geant POSITION : CSHI ===> X=2.*CALG_SMGASTHK-CALG_SMALFTHK <===  319
            CALL AGSRESET                                                 319
            AG_X = 2.*CALG_SMGASTHK-CALG_SMALFTHK                         319
            AG_TITLE = 'CSHI'                                             319
            AG_EXNAME = 'POSITION'                                        319
            AG_PARLIST = 'X'                                              319
            CALL AXPOSITION                                               319
C       *                                                                 319
            ENDIF                                                         319
         GO TO 5401                                                       321
5402     CONTINUE                                                         321
5381  CALL AGSPOPB                                                        322
      GO TO BLOCK_CSME, (5380 ,5390)                                      322
C End of Local Procedure                                                  322
C ----------------------------------------------------------------------  323
C ------------ CSHI                                                       323
C Begin Local Procedure CSHI                                              324
C CSHI IS A SENSITEVE AR/CO2 BOX                                          324
5410  CONTINUE                                                            324
         AG_TITLE='CSHI'                                                  324
         AG_MODULE=MODULE                                                 324
         AG_MARK='V00'                                                    324
         CALL AGSPUSH                                                     324
         AG_MODULE=MODULE                                                 324
C    *                                                                    324
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   325
         CALL AGSRESET                                                    325
         AG_A = 39.95                                                     325
         AG_Z = 18.                                                       325
         AG_W = 0.9                                                       325
         AG_TITLE = 'AR'                                                  325
         AG_EXNAME = 'COMPONENT'                                          325
         AG_PARLIST = 'A_Z_W'                                             325
         CALL AXCOMPONENT                                                 325
C    *                                                                    325
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       325
         CALL AGSRESET                                                    326
         AG_A = 12.01                                                     326
         AG_Z = 6.                                                        326
         AG_W = 0.1*1*12.01/44.01                                         326
         AG_TITLE = 'C'                                                   326
         AG_EXNAME = 'COMPONENT'                                          326
         AG_PARLIST = 'A_Z_W'                                             326
         CALL AXCOMPONENT                                                 326
C    *                                                                    326
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           326
         CALL AGSRESET                                                    327
         AG_A = 16.                                                       327
         AG_Z = 8.                                                        327
         AG_W = 0.1*2*16./44.01                                           327
         AG_TITLE = 'O'                                                   327
         AG_EXNAME = 'COMPONENT'                                          327
         AG_PARLIST = 'A_Z_W'                                             327
         CALL AXCOMPONENT                                                 327
C    *                                                                    327
C    Geant MIXTURE : SENS_GAS ===> ISVOL=1 DENS=0.0018015 <===            327
         CALL AGSRESET                                                    328
         AG_ISVOL = 1                                                     328
         AG_DENS = 0.0018015                                              328
         AG_TITLE = 'SENS_GAS'                                            328
         AG_EXNAME = 'MIXTURE'                                            328
         AG_PARLIST = 'ISVOL_DENS'                                        328
         CALL AXMIXTURE                                                   328
C    *                                                                    328
C    Geant ATTRIBUTE : CSHI ===> SEEN=1 COLO=4 <===                       328
         CALL AGSRESET                                                    329
         AG_SEEN = 1                                                      329
         AG_COLO = 4                                                      329
         AG_TITLE = 'CSHI'                                                329
         AG_EXNAME = 'ATTRIBUTE'                                          329
         AG_PARLIST = 'SEEN_COLO'                                         329
         CALL AXATTRIBUTE                                                 329
C    *                                                                    329
C    Check I.EQ.1                                                         329
         IF (I.EQ.1) THEN                                                 330
C    Geant SHAPE : BOX ===> DX=CALG_SMGASTHK DY=CALG_SMGASWDH <===        331
         CALL AGSRESET                                                    331
         AG_DX = CALG_SMGASTHK                                            331
         AG_DY = CALG_SMGASWDH                                            331
         AG_TITLE = 'BOX'                                                 331
         AG_EXNAME = 'SHAPE'                                              331
         AG_PARLIST = 'DX_DY'                                             331
         CALL AXSHAPE                                                     331
C    *                                                                    331
         ELSE                                                             331
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=CALG_SMGASRAD PHI1=270, PHI2=45  334
C    0 <===                                                               334
         CALL AGSRESET                                                    334
         AG_RMIN = 0                                                      334
         AG_RMAX = CALG_SMGASRAD                                          334
         AG_PHI1 = 270                                                    334
         AG_PHI2 = 450                                                    334
         AG_TITLE = 'TUBS'                                                334
         AG_EXNAME = 'SHAPE'                                              334
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2'                               334
         CALL AXSHAPE                                                     334
C    *                                                                    334
         ENDIF                                                            334
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              336
         CALL GSTPAR(AG_IMED, 'STRA', 1.0)                                337
         H_ETA1=2.*(CALG_SETA1WDH + CALG_SET12WDH)                        338
         SH_ETA1=CALG_NETFIRST*H_ETA1                                     339
         H_ETA2=2.*(CALG_SETA2WDH + CALG_SET12WDH)                        340
         SH_ETA2=CALG_NETSECON*H_ETA2                                     341
         H_PHI1=2.*(CALG_SPHIWDH + CALG_SPHIDWDH)                         342
         H_PHI2=H_PHI1                                                    343
         SH_PHI1=CALG_NPHISTR*H_PHI1                                      344
         SH_PHI2=SH_PHI1                                                  345
C    Defining Hits: TYPE=1:2: ETA:0.1:(0,1) ETSP:H_ETA1:(0,SH_ETA1) ELOS  346
C    S:0:(0,1)                                                            346
         AG_TITLE = 'CSDA'                                                346
         AG_EXNAME = 'HITS'                                               346
         AG_TYPE=1                                                        346
         CALL AGSDETV('CSDA')                                             346
C    *                                                                    346
         AG_PARLIST='TYPE'                                                346
         AG_TITLE = 'DETH'                                                346
         AG_NBITS = 2                                                     346
         AG_BIN=AG_MAGIC                                                  346
         AG_FHMIN=AG_MAGIC                                                346
         AG_FHMAX=AG_MAGIC                                                346
         AG_OPTION='*'                                                    346
         CALL AGSDETH (0)                                                 346
C    *                                                                    346
         AG_PARLIST='ETA'                                                 346
         AG_TITLE = 'DETH'                                                346
         AG_NBITS = AG_MAGIC                                              346
         AG_BIN=0.1                                                       346
         AG_OPTION='*'                                                    346
         AG_FHMIN = 0                                                     346
         AG_FHMAX = 1                                                     346
         CALL AGSDETH (0)                                                 346
C    *                                                                    346
         AG_PARLIST='ETSP'                                                346
         AG_TITLE = 'DETH'                                                346
         AG_NBITS = AG_MAGIC                                              346
         AG_BIN=H_ETA1                                                    346
         AG_OPTION='*'                                                    346
         AG_FHMIN = 0                                                     346
         AG_FHMAX = SH_ETA1                                               346
         CALL AGSDETH (0)                                                 346
C    *                                                                    346
         AG_PARLIST='ELOSS'                                               346
         AG_TITLE = 'DETH'                                                346
         AG_NBITS = 0                                                     346
         AG_BIN=AG_MAGIC                                                  346
         AG_OPTION='*'                                                    346
         AG_FHMIN = 0                                                     346
         AG_FHMAX = 1                                                     346
         CALL AGSDETH (0)                                                 346
C    *                                                                    346
         CALL AGSDETU('CSDA')                                             346
C    *                                                                    346
C    Defining Hits: TYPE=2:2: ETA:0.1:(0,1) ETSP:H_ETA2:(0,SH_ETA2) ELOS  347
C    S:0:(0,1)                                                            347
         AG_TITLE = 'CSDA'                                                347
         AG_EXNAME = 'HITS'                                               347
         AG_TYPE=2                                                        347
         CALL AGSDETV('CSDA')                                             347
C    *                                                                    347
         AG_PARLIST='TYPE'                                                347
         AG_TITLE = 'DETH'                                                347
         AG_NBITS = 2                                                     347
         AG_BIN=AG_MAGIC                                                  347
         AG_FHMIN=AG_MAGIC                                                347
         AG_FHMAX=AG_MAGIC                                                347
         AG_OPTION='*'                                                    347
         CALL AGSDETH (0)                                                 347
C    *                                                                    347
         AG_PARLIST='ETA'                                                 347
         AG_TITLE = 'DETH'                                                347
         AG_NBITS = AG_MAGIC                                              347
         AG_BIN=0.1                                                       347
         AG_OPTION='*'                                                    347
         AG_FHMIN = 0                                                     347
         AG_FHMAX = 1                                                     347
         CALL AGSDETH (0)                                                 347
C    *                                                                    347
         AG_PARLIST='ETSP'                                                347
         AG_TITLE = 'DETH'                                                347
         AG_NBITS = AG_MAGIC                                              347
         AG_BIN=H_ETA2                                                    347
         AG_OPTION='*'                                                    347
         AG_FHMIN = 0                                                     347
         AG_FHMAX = SH_ETA2                                               347
         CALL AGSDETH (0)                                                 347
C    *                                                                    347
         AG_PARLIST='ELOSS'                                               347
         AG_TITLE = 'DETH'                                                347
         AG_NBITS = 0                                                     347
         AG_BIN=AG_MAGIC                                                  347
         AG_OPTION='*'                                                    347
         AG_FHMIN = 0                                                     347
         AG_FHMAX = 1                                                     347
         CALL AGSDETH (0)                                                 347
C    *                                                                    347
         CALL AGSDETU('CSDA')                                             347
C    *                                                                    347
C    Defining Hits: TYPE=3:2: ETA:0.1:(0,1) ETSP:H_PHI1:(0,SH_PHI1) ELOS  348
C    S:0:(0,1)                                                            348
         AG_TITLE = 'CSDA'                                                348
         AG_EXNAME = 'HITS'                                               348
         AG_TYPE=3                                                        348
         CALL AGSDETV('CSDA')                                             348
C    *                                                                    348
         AG_PARLIST='TYPE'                                                348
         AG_TITLE = 'DETH'                                                348
         AG_NBITS = 2                                                     348
         AG_BIN=AG_MAGIC                                                  348
         AG_FHMIN=AG_MAGIC                                                348
         AG_FHMAX=AG_MAGIC                                                348
         AG_OPTION='*'                                                    348
         CALL AGSDETH (0)                                                 348
C    *                                                                    348
         AG_PARLIST='ETA'                                                 348
         AG_TITLE = 'DETH'                                                348
         AG_NBITS = AG_MAGIC                                              348
         AG_BIN=0.1                                                       348
         AG_OPTION='*'                                                    348
         AG_FHMIN = 0                                                     348
         AG_FHMAX = 1                                                     348
         CALL AGSDETH (0)                                                 348
C    *                                                                    348
         AG_PARLIST='ETSP'                                                348
         AG_TITLE = 'DETH'                                                348
         AG_NBITS = AG_MAGIC                                              348
         AG_BIN=H_PHI1                                                    348
         AG_OPTION='*'                                                    348
         AG_FHMIN = 0                                                     348
         AG_FHMAX = SH_PHI1                                               348
         CALL AGSDETH (0)                                                 348
C    *                                                                    348
         AG_PARLIST='ELOSS'                                               348
         AG_TITLE = 'DETH'                                                348
         AG_NBITS = 0                                                     348
         AG_BIN=AG_MAGIC                                                  348
         AG_OPTION='*'                                                    348
         AG_FHMIN = 0                                                     348
         AG_FHMAX = 1                                                     348
         CALL AGSDETH (0)                                                 348
C    *                                                                    348
         CALL AGSDETU('CSDA')                                             348
C    *                                                                    348
C    Defining Hits: TYPE=4:2: ETA:0.1:(0,1) ETSP:H_PHI2:(0,SH_PHI2) ELOS  349
C    S:0:(0,1)                                                            349
         AG_TITLE = 'CSDA'                                                349
         AG_EXNAME = 'HITS'                                               349
         AG_TYPE=4                                                        349
         CALL AGSDETV('CSDA')                                             349
C    *                                                                    349
         AG_PARLIST='TYPE'                                                349
         AG_TITLE = 'DETH'                                                349
         AG_NBITS = 2                                                     349
         AG_BIN=AG_MAGIC                                                  349
         AG_FHMIN=AG_MAGIC                                                349
         AG_FHMAX=AG_MAGIC                                                349
         AG_OPTION='*'                                                    349
         CALL AGSDETH (0)                                                 349
C    *                                                                    349
         AG_PARLIST='ETA'                                                 349
         AG_TITLE = 'DETH'                                                349
         AG_NBITS = AG_MAGIC                                              349
         AG_BIN=0.1                                                       349
         AG_OPTION='*'                                                    349
         AG_FHMIN = 0                                                     349
         AG_FHMAX = 1                                                     349
         CALL AGSDETH (0)                                                 349
C    *                                                                    349
         AG_PARLIST='ETSP'                                                349
         AG_TITLE = 'DETH'                                                349
         AG_NBITS = AG_MAGIC                                              349
         AG_BIN=H_PHI2                                                    349
         AG_OPTION='*'                                                    349
         AG_FHMIN = 0                                                     349
         AG_FHMAX = SH_PHI2                                               349
         CALL AGSDETH (0)                                                 349
C    *                                                                    349
         AG_PARLIST='ELOSS'                                               349
         AG_TITLE = 'DETH'                                                349
         AG_NBITS = 0                                                     349
         AG_BIN=AG_MAGIC                                                  349
         AG_OPTION='*'                                                    349
         AG_FHMIN = 0                                                     349
         AG_FHMAX = 1                                                     349
         CALL AGSDETH (0)                                                 349
C    *                                                                    349
         CALL AGSDETU('CSDA')                                             349
C    *                                                                    349
5411  CALL AGSPOPB                                                        350
      GO TO BLOCK_CSHI, (5410 ,5420)                                      350
C End of Local Procedure                                                  350
      WRITE(LOUT,5430)'===>>  [End AgML Module CALBGEO]  <<==='           350
5430  FORMAT(A32,/,/)                                                     350
C MODULE CALBGEO                                                          352
1     CONTINUE                                                            352
      END                                                                 352
