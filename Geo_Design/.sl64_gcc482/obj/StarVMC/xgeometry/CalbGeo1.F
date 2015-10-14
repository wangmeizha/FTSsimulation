C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE CALBGEO1                                                 1
C CALBGEO1 IS THE GEOMETRY OF THE BARREL EM CALORIMETER                   1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'CALBGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE BARREL EM CALORIMETER'/       1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR MAXIM POTEKHIN BNL                                               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN BNL'/                          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED JANUARY 20, 2004                                                2
      CHARACTER*40 CREATED /'JANUARY 20, 2004'/                           3
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
      EXTERNAL ETSPHIT                                                    6
      INTEGER BLOCK_CALB                                                  8
      INTEGER BLOCK_CHLV                                                  8
      INTEGER BLOCK_CPHI                                                  8
      INTEGER BLOCK_CSUP                                                  8
      INTEGER BLOCK_CPBP                                                  8
      INTEGER BLOCK_CSCI                                                  8
      INTEGER BLOCK_CSMD                                                  8
      INTEGER BLOCK_CSMG                                                  8
      INTEGER BLOCK_CSDA                                                  8
      INTEGER BLOCK_CSMC                                                  8
      INTEGER BLOCK_CSMB                                                  8
      INTEGER BLOCK_CSME                                                  8
      INTEGER BLOCK_CSHI                                                  8
      INTEGER BLOCK_CBTW                                                  8
C -                                                                       13
      REAL CALG_VERSION                                                   13
      EQUIVALENCE (BANK_CALG(3),CALG_VERSION,RE_CALG_VERSION),            13
     *(NAMES_CALG(1),NCALG_VERSION)                                       13
      EQUIVALENCE (COMMEN_CALG(1),CCALG_VERSION),(MAP_CALG(1,1),          13
     *MCALG_VERSION)                                                      13
      CHARACTER CCALG_VERSION*40/' '/                                     13
      CHARACTER NCALG_VERSION*12/'R.VERSION'/                             13
      INTEGER MCALG_VERSION(2)/1,1/                                       13
      REAL RE_CALG_VERSION                                                13
C -                                                                       13
      REAL CALG_RMIN                                                      13
      EQUIVALENCE (BANK_CALG(4),CALG_RMIN,RE_CALG_RMIN),(NAMES_CALG(2),   13
     *NCALG_RMIN)                                                         13
      EQUIVALENCE (COMMEN_CALG(2),CCALG_RMIN),(MAP_CALG(1,2),             13
     *MCALG_RMIN)                                                         13
      CHARACTER CCALG_RMIN*40/' '/                                        13
      CHARACTER NCALG_RMIN*12/'R.RMIN'/                                   13
      INTEGER MCALG_RMIN(2)/1,1/                                          13
      REAL RE_CALG_RMIN                                                   13
C -                                                                       13
      REAL CALG_ETACUT                                                    13
      EQUIVALENCE (BANK_CALG(5),CALG_ETACUT,RE_CALG_ETACUT),              13
     *(NAMES_CALG(3),NCALG_ETACUT)                                        13
      EQUIVALENCE (COMMEN_CALG(3),CCALG_ETACUT),(MAP_CALG(1,3),           13
     *MCALG_ETACUT)                                                       13
      CHARACTER CCALG_ETACUT*40/' '/                                      13
      CHARACTER NCALG_ETACUT*12/'R.ETACUT'/                               13
      INTEGER MCALG_ETACUT(2)/1,1/                                        13
      REAL RE_CALG_ETACUT                                                 13
C -                                                                       13
      REAL CALG_CRACKWD                                                   13
      EQUIVALENCE (BANK_CALG(6),CALG_CRACKWD,RE_CALG_CRACKWD),            13
     *(NAMES_CALG(4),NCALG_CRACKWD)                                       13
      EQUIVALENCE (COMMEN_CALG(4),CCALG_CRACKWD),(MAP_CALG(1,4),          13
     *MCALG_CRACKWD)                                                      13
      CHARACTER CCALG_CRACKWD*40/' '/                                     13
      CHARACTER NCALG_CRACKWD*12/'R.CRACKWD'/                             13
      INTEGER MCALG_CRACKWD(2)/1,1/                                       13
      REAL RE_CALG_CRACKWD                                                13
C -                                                                       13
      REAL CALG_FRONTTHK                                                  13
      EQUIVALENCE (BANK_CALG(7),CALG_FRONTTHK,RE_CALG_FRONTTHK),          13
     *(NAMES_CALG(5),NCALG_FRONTTHK)                                      13
      EQUIVALENCE (COMMEN_CALG(5),CCALG_FRONTTHK),(MAP_CALG(1,5),         13
     *MCALG_FRONTTHK)                                                     13
      CHARACTER CCALG_FRONTTHK*40/' '/                                    13
      CHARACTER NCALG_FRONTTHK*12/'R.FRONTTHK'/                           13
      INTEGER MCALG_FRONTTHK(2)/1,1/                                      13
      REAL RE_CALG_FRONTTHK                                               13
C -                                                                       13
      REAL CALG_COMPTHK                                                   13
      EQUIVALENCE (BANK_CALG(8),CALG_COMPTHK,RE_CALG_COMPTHK),            13
     *(NAMES_CALG(6),NCALG_COMPTHK)                                       13
      EQUIVALENCE (COMMEN_CALG(6),CCALG_COMPTHK),(MAP_CALG(1,6),          13
     *MCALG_COMPTHK)                                                      13
      CHARACTER CCALG_COMPTHK*40/' '/                                     13
      CHARACTER NCALG_COMPTHK*12/'R.COMPTHK'/                             13
      INTEGER MCALG_COMPTHK(2)/1,1/                                       13
      REAL RE_CALG_COMPTHK                                                13
C -                                                                       13
      REAL CALG_AIRTHK                                                    13
      EQUIVALENCE (BANK_CALG(9),CALG_AIRTHK,RE_CALG_AIRTHK),              13
     *(NAMES_CALG(7),NCALG_AIRTHK)                                        13
      EQUIVALENCE (COMMEN_CALG(7),CCALG_AIRTHK),(MAP_CALG(1,7),           13
     *MCALG_AIRTHK)                                                       13
      CHARACTER CCALG_AIRTHK*40/' '/                                      13
      CHARACTER NCALG_AIRTHK*12/'R.AIRTHK'/                               13
      INTEGER MCALG_AIRTHK(2)/1,1/                                        13
      REAL RE_CALG_AIRTHK                                                 13
C -                                                                       13
      REAL CALG_BACKTHK                                                   13
      EQUIVALENCE (BANK_CALG(10),CALG_BACKTHK,RE_CALG_BACKTHK),           13
     *(NAMES_CALG(8),NCALG_BACKTHK)                                       13
      EQUIVALENCE (COMMEN_CALG(8),CCALG_BACKTHK),(MAP_CALG(1,8),          13
     *MCALG_BACKTHK)                                                      13
      CHARACTER CCALG_BACKTHK*40/' '/                                     13
      CHARACTER NCALG_BACKTHK*12/'R.BACKTHK'/                             13
      INTEGER MCALG_BACKTHK(2)/1,1/                                       13
      REAL RE_CALG_BACKTHK                                                13
C -                                                                       13
      REAL CALG_SPACETHK                                                  13
      EQUIVALENCE (BANK_CALG(11),CALG_SPACETHK,RE_CALG_SPACETHK),         13
     *(NAMES_CALG(9),NCALG_SPACETHK)                                      13
      EQUIVALENCE (COMMEN_CALG(9),CCALG_SPACETHK),(MAP_CALG(1,9),         13
     *MCALG_SPACETHK)                                                     13
      CHARACTER CCALG_SPACETHK*40/' '/                                    13
      CHARACTER NCALG_SPACETHK*12/'R.SPACETHK'/                           13
      INTEGER MCALG_SPACETHK(2)/1,1/                                      13
      REAL RE_CALG_SPACETHK                                               13
C -                                                                       13
      REAL CALG_SCINTTHK(2)                                               13
      EQUIVALENCE (BANK_CALG(12),CALG_SCINTTHK,RE_CALG_SCINTTHK),         13
     *(NAMES_CALG(10),NCALG_SCINTTHK)                                     13
      EQUIVALENCE (COMMEN_CALG(10),CCALG_SCINTTHK),(MAP_CALG(1,10),       13
     *MCALG_SCINTTHK)                                                     13
      CHARACTER CCALG_SCINTTHK*40/' '/                                    13
      CHARACTER NCALG_SCINTTHK*12/'R.SCINTTHK'/                           13
      INTEGER MCALG_SCINTTHK(2)/2,1/                                      13
      REAL RE_CALG_SCINTTHK                                               13
C -                                                                       13
      REAL CALG_ABSORTHK                                                  13
      EQUIVALENCE (BANK_CALG(14),CALG_ABSORTHK,RE_CALG_ABSORTHK),         13
     *(NAMES_CALG(11),NCALG_ABSORTHK)                                     13
      EQUIVALENCE (COMMEN_CALG(11),CCALG_ABSORTHK),(MAP_CALG(1,11),       13
     *MCALG_ABSORTHK)                                                     13
      CHARACTER CCALG_ABSORTHK*40/' '/                                    13
      CHARACTER NCALG_ABSORTHK*12/'R.ABSORTHK'/                           13
      INTEGER MCALG_ABSORTHK(2)/1,1/                                      13
      REAL RE_CALG_ABSORTHK                                               13
C -                                                                       13
      REAL CALG_ABPAPTHK                                                  13
      EQUIVALENCE (BANK_CALG(15),CALG_ABPAPTHK,RE_CALG_ABPAPTHK),         13
     *(NAMES_CALG(12),NCALG_ABPAPTHK)                                     13
      EQUIVALENCE (COMMEN_CALG(12),CCALG_ABPAPTHK),(MAP_CALG(1,12),       13
     *MCALG_ABPAPTHK)                                                     13
      CHARACTER CCALG_ABPAPTHK*40/' '/                                    13
      CHARACTER NCALG_ABPAPTHK*12/'R.ABPAPTHK'/                           13
      INTEGER MCALG_ABPAPTHK(2)/1,1/                                      13
      REAL RE_CALG_ABPAPTHK                                               13
C -                                                                       13
      REAL CALG_G10SBTHK                                                  13
      EQUIVALENCE (BANK_CALG(16),CALG_G10SBTHK,RE_CALG_G10SBTHK),         13
     *(NAMES_CALG(13),NCALG_G10SBTHK)                                     13
      EQUIVALENCE (COMMEN_CALG(13),CCALG_G10SBTHK),(MAP_CALG(1,13),       13
     *MCALG_G10SBTHK)                                                     13
      CHARACTER CCALG_G10SBTHK*40/' '/                                    13
      CHARACTER NCALG_G10SBTHK*12/'R.G10SBTHK'/                           13
      INTEGER MCALG_G10SBTHK(2)/1,1/                                      13
      REAL RE_CALG_G10SBTHK                                               13
C -                                                                       13
      REAL CALG_SMALFWDH                                                  13
      EQUIVALENCE (BANK_CALG(17),CALG_SMALFWDH,RE_CALG_SMALFWDH),         13
     *(NAMES_CALG(14),NCALG_SMALFWDH)                                     13
      EQUIVALENCE (COMMEN_CALG(14),CCALG_SMALFWDH),(MAP_CALG(1,14),       13
     *MCALG_SMALFWDH)                                                     13
      CHARACTER CCALG_SMALFWDH*40/' '/                                    13
      CHARACTER NCALG_SMALFWDH*12/'R.SMALFWDH'/                           13
      INTEGER MCALG_SMALFWDH(2)/1,1/                                      13
      REAL RE_CALG_SMALFWDH                                               13
C -                                                                       13
      REAL CALG_SMALFTHK                                                  13
      EQUIVALENCE (BANK_CALG(18),CALG_SMALFTHK,RE_CALG_SMALFTHK),         13
     *(NAMES_CALG(15),NCALG_SMALFTHK)                                     13
      EQUIVALENCE (COMMEN_CALG(15),CCALG_SMALFTHK),(MAP_CALG(1,15),       13
     *MCALG_SMALFTHK)                                                     13
      CHARACTER CCALG_SMALFTHK*40/' '/                                    13
      CHARACTER NCALG_SMALFTHK*12/'R.SMALFTHK'/                           13
      INTEGER MCALG_SMALFTHK(2)/1,1/                                      13
      REAL RE_CALG_SMALFTHK                                               13
C -                                                                       13
      REAL CALG_SMGASTHK                                                  13
      EQUIVALENCE (BANK_CALG(19),CALG_SMGASTHK,RE_CALG_SMGASTHK),         13
     *(NAMES_CALG(16),NCALG_SMGASTHK)                                     13
      EQUIVALENCE (COMMEN_CALG(16),CCALG_SMGASTHK),(MAP_CALG(1,16),       13
     *MCALG_SMGASTHK)                                                     13
      CHARACTER CCALG_SMGASTHK*40/' '/                                    13
      CHARACTER NCALG_SMGASTHK*12/'R.SMGASTHK'/                           13
      INTEGER MCALG_SMGASTHK(2)/1,1/                                      13
      REAL RE_CALG_SMGASTHK                                               13
C -                                                                       13
      REAL CALG_SMGASWDH                                                  13
      EQUIVALENCE (BANK_CALG(20),CALG_SMGASWDH,RE_CALG_SMGASWDH),         13
     *(NAMES_CALG(17),NCALG_SMGASWDH)                                     13
      EQUIVALENCE (COMMEN_CALG(17),CCALG_SMGASWDH),(MAP_CALG(1,17),       13
     *MCALG_SMGASWDH)                                                     13
      CHARACTER CCALG_SMGASWDH*40/' '/                                    13
      CHARACTER NCALG_SMGASWDH*12/'R.SMGASWDH'/                           13
      INTEGER MCALG_SMGASWDH(2)/1,1/                                      13
      REAL RE_CALG_SMGASWDH                                               13
C -                                                                       13
      REAL CALG_SMGASRAD                                                  13
      EQUIVALENCE (BANK_CALG(21),CALG_SMGASRAD,RE_CALG_SMGASRAD),         13
     *(NAMES_CALG(18),NCALG_SMGASRAD)                                     13
      EQUIVALENCE (COMMEN_CALG(18),CCALG_SMGASRAD),(MAP_CALG(1,18),       13
     *MCALG_SMGASRAD)                                                     13
      CHARACTER CCALG_SMGASRAD*40/' '/                                    13
      CHARACTER NCALG_SMGASRAD*12/'R.SMGASRAD'/                           13
      INTEGER MCALG_SMGASRAD(2)/1,1/                                      13
      REAL RE_CALG_SMGASRAD                                               13
C -                                                                       13
      REAL CALG_SMAFFWDH                                                  13
      EQUIVALENCE (BANK_CALG(22),CALG_SMAFFWDH,RE_CALG_SMAFFWDH),         13
     *(NAMES_CALG(19),NCALG_SMAFFWDH)                                     13
      EQUIVALENCE (COMMEN_CALG(19),CCALG_SMAFFWDH),(MAP_CALG(1,19),       13
     *MCALG_SMAFFWDH)                                                     13
      CHARACTER CCALG_SMAFFWDH*40/' '/                                    13
      CHARACTER NCALG_SMAFFWDH*12/'R.SMAFFWDH'/                           13
      INTEGER MCALG_SMAFFWDH(2)/1,1/                                      13
      REAL RE_CALG_SMAFFWDH                                               13
C -                                                                       13
      REAL CALG_SMAFBWDH                                                  13
      EQUIVALENCE (BANK_CALG(23),CALG_SMAFBWDH,RE_CALG_SMAFBWDH),         13
     *(NAMES_CALG(20),NCALG_SMAFBWDH)                                     13
      EQUIVALENCE (COMMEN_CALG(20),CCALG_SMAFBWDH),(MAP_CALG(1,20),       13
     *MCALG_SMAFBWDH)                                                     13
      CHARACTER CCALG_SMAFBWDH*40/' '/                                    13
      CHARACTER NCALG_SMAFBWDH*12/'R.SMAFBWDH'/                           13
      INTEGER MCALG_SMAFBWDH(2)/1,1/                                      13
      REAL RE_CALG_SMAFBWDH                                               13
C -                                                                       13
      REAL CALG_SMETAWDH                                                  13
      EQUIVALENCE (BANK_CALG(24),CALG_SMETAWDH,RE_CALG_SMETAWDH),         13
     *(NAMES_CALG(21),NCALG_SMETAWDH)                                     13
      EQUIVALENCE (COMMEN_CALG(21),CCALG_SMETAWDH),(MAP_CALG(1,21),       13
     *MCALG_SMETAWDH)                                                     13
      CHARACTER CCALG_SMETAWDH*40/' '/                                    13
      CHARACTER NCALG_SMETAWDH*12/'R.SMETAWDH'/                           13
      INTEGER MCALG_SMETAWDH(2)/1,1/                                      13
      REAL RE_CALG_SMETAWDH                                               13
C -                                                                       13
      REAL CALG_SETA1WDH                                                  13
      EQUIVALENCE (BANK_CALG(25),CALG_SETA1WDH,RE_CALG_SETA1WDH),         13
     *(NAMES_CALG(22),NCALG_SETA1WDH)                                     13
      EQUIVALENCE (COMMEN_CALG(22),CCALG_SETA1WDH),(MAP_CALG(1,22),       13
     *MCALG_SETA1WDH)                                                     13
      CHARACTER CCALG_SETA1WDH*40/' '/                                    13
      CHARACTER NCALG_SETA1WDH*12/'R.SETA1WDH'/                           13
      INTEGER MCALG_SETA1WDH(2)/1,1/                                      13
      REAL RE_CALG_SETA1WDH                                               13
C -                                                                       13
      REAL CALG_NETFIRST                                                  13
      EQUIVALENCE (BANK_CALG(26),CALG_NETFIRST,RE_CALG_NETFIRST),         13
     *(NAMES_CALG(23),NCALG_NETFIRST)                                     13
      EQUIVALENCE (COMMEN_CALG(23),CCALG_NETFIRST),(MAP_CALG(1,23),       13
     *MCALG_NETFIRST)                                                     13
      CHARACTER CCALG_NETFIRST*40/' '/                                    13
      CHARACTER NCALG_NETFIRST*12/'R.NETFIRST'/                           13
      INTEGER MCALG_NETFIRST(2)/1,1/                                      13
      REAL RE_CALG_NETFIRST                                               13
C -                                                                       13
      REAL CALG_SETA2WDH                                                  13
      EQUIVALENCE (BANK_CALG(27),CALG_SETA2WDH,RE_CALG_SETA2WDH),         13
     *(NAMES_CALG(24),NCALG_SETA2WDH)                                     13
      EQUIVALENCE (COMMEN_CALG(24),CCALG_SETA2WDH),(MAP_CALG(1,24),       13
     *MCALG_SETA2WDH)                                                     13
      CHARACTER CCALG_SETA2WDH*40/' '/                                    13
      CHARACTER NCALG_SETA2WDH*12/'R.SETA2WDH'/                           13
      INTEGER MCALG_SETA2WDH(2)/1,1/                                      13
      REAL RE_CALG_SETA2WDH                                               13
C -                                                                       13
      REAL CALG_NETSECON                                                  13
      EQUIVALENCE (BANK_CALG(28),CALG_NETSECON,RE_CALG_NETSECON),         13
     *(NAMES_CALG(25),NCALG_NETSECON)                                     13
      EQUIVALENCE (COMMEN_CALG(25),CCALG_NETSECON),(MAP_CALG(1,25),       13
     *MCALG_NETSECON)                                                     13
      CHARACTER CCALG_NETSECON*40/' '/                                    13
      CHARACTER NCALG_NETSECON*12/'R.NETSECON'/                           13
      INTEGER MCALG_NETSECON(2)/1,1/                                      13
      REAL RE_CALG_NETSECON                                               13
C -                                                                       13
      REAL CALG_SET12WDH                                                  13
      EQUIVALENCE (BANK_CALG(29),CALG_SET12WDH,RE_CALG_SET12WDH),         13
     *(NAMES_CALG(26),NCALG_SET12WDH)                                     13
      EQUIVALENCE (COMMEN_CALG(26),CCALG_SET12WDH),(MAP_CALG(1,26),       13
     *MCALG_SET12WDH)                                                     13
      CHARACTER CCALG_SET12WDH*40/' '/                                    13
      CHARACTER NCALG_SET12WDH*12/'R.SET12WDH'/                           13
      INTEGER MCALG_SET12WDH(2)/1,1/                                      13
      REAL RE_CALG_SET12WDH                                               13
C -                                                                       13
      REAL CALG_SPHIWDH                                                   13
      EQUIVALENCE (BANK_CALG(30),CALG_SPHIWDH,RE_CALG_SPHIWDH),           13
     *(NAMES_CALG(27),NCALG_SPHIWDH)                                      13
      EQUIVALENCE (COMMEN_CALG(27),CCALG_SPHIWDH),(MAP_CALG(1,27),        13
     *MCALG_SPHIWDH)                                                      13
      CHARACTER CCALG_SPHIWDH*40/' '/                                     13
      CHARACTER NCALG_SPHIWDH*12/'R.SPHIWDH'/                             13
      INTEGER MCALG_SPHIWDH(2)/1,1/                                       13
      REAL RE_CALG_SPHIWDH                                                13
C -                                                                       13
      REAL CALG_SPHIDWDH                                                  13
      EQUIVALENCE (BANK_CALG(31),CALG_SPHIDWDH,RE_CALG_SPHIDWDH),         13
     *(NAMES_CALG(28),NCALG_SPHIDWDH)                                     13
      EQUIVALENCE (COMMEN_CALG(28),CCALG_SPHIDWDH),(MAP_CALG(1,28),       13
     *MCALG_SPHIDWDH)                                                     13
      CHARACTER CCALG_SPHIDWDH*40/' '/                                    13
      CHARACTER NCALG_SPHIDWDH*12/'R.SPHIDWDH'/                           13
      INTEGER MCALG_SPHIDWDH(2)/1,1/                                      13
      REAL RE_CALG_SPHIDWDH                                               13
C -                                                                       13
      REAL CALG_NPHISTR                                                   13
      EQUIVALENCE (BANK_CALG(32),CALG_NPHISTR,RE_CALG_NPHISTR),           13
     *(NAMES_CALG(29),NCALG_NPHISTR)                                      13
      EQUIVALENCE (COMMEN_CALG(29),CCALG_NPHISTR),(MAP_CALG(1,29),        13
     *MCALG_NPHISTR)                                                      13
      CHARACTER CCALG_NPHISTR*40/' '/                                     13
      CHARACTER NCALG_NPHISTR*12/'R.NPHISTR'/                             13
      INTEGER MCALG_NPHISTR(2)/1,1/                                       13
      REAL RE_CALG_NPHISTR                                                13
C -                                                                       13
      REAL CALG_NSMDALW                                                   13
      EQUIVALENCE (BANK_CALG(33),CALG_NSMDALW,RE_CALG_NSMDALW),           13
     *(NAMES_CALG(30),NCALG_NSMDALW)                                      13
      EQUIVALENCE (COMMEN_CALG(30),CCALG_NSMDALW),(MAP_CALG(1,30),        13
     *MCALG_NSMDALW)                                                      13
      CHARACTER CCALG_NSMDALW*40/' '/                                     13
      CHARACTER NCALG_NSMDALW*12/'R.NSMDALW'/                             13
      INTEGER MCALG_NSMDALW(2)/1,1/                                       13
      REAL RE_CALG_NSMDALW                                                13
C -                                                                       13
      REAL CALG_NSUPER                                                    13
      EQUIVALENCE (BANK_CALG(34),CALG_NSUPER,RE_CALG_NSUPER),             13
     *(NAMES_CALG(31),NCALG_NSUPER)                                       13
      EQUIVALENCE (COMMEN_CALG(31),CCALG_NSUPER),(MAP_CALG(1,31),         13
     *MCALG_NSUPER)                                                       13
      CHARACTER CCALG_NSUPER*40/' '/                                      13
      CHARACTER NCALG_NSUPER*12/'R.NSUPER'/                               13
      INTEGER MCALG_NSUPER(2)/1,1/                                        13
      REAL RE_CALG_NSUPER                                                 13
C -                                                                       13
      REAL CALG_NSMD                                                      13
      EQUIVALENCE (BANK_CALG(35),CALG_NSMD,RE_CALG_NSMD),                 13
     *(NAMES_CALG(32),NCALG_NSMD)                                         13
      EQUIVALENCE (COMMEN_CALG(32),CCALG_NSMD),(MAP_CALG(1,32),           13
     *MCALG_NSMD)                                                         13
      CHARACTER CCALG_NSMD*40/' '/                                        13
      CHARACTER NCALG_NSMD*12/'R.NSMD'/                                   13
      INTEGER MCALG_NSMD(2)/1,1/                                          13
      REAL RE_CALG_NSMD                                                   13
C -                                                                       13
      REAL CALG_NSUBLAY(2)                                                13
      EQUIVALENCE (BANK_CALG(36),CALG_NSUBLAY,RE_CALG_NSUBLAY),           13
     *(NAMES_CALG(33),NCALG_NSUBLAY)                                      13
      EQUIVALENCE (COMMEN_CALG(33),CCALG_NSUBLAY),(MAP_CALG(1,33),        13
     *MCALG_NSUBLAY)                                                      13
      CHARACTER CCALG_NSUBLAY*40/' '/                                     13
      CHARACTER NCALG_NSUBLAY*12/'R.NSUBLAY'/                             13
      INTEGER MCALG_NSUBLAY(2)/2,1/                                       13
      REAL RE_CALG_NSUBLAY                                                13
C -                                                                       13
      REAL CALG_NMODULE(2)                                                13
      EQUIVALENCE (BANK_CALG(38),CALG_NMODULE,RE_CALG_NMODULE),           13
     *(NAMES_CALG(34),NCALG_NMODULE)                                      13
      EQUIVALENCE (COMMEN_CALG(34),CCALG_NMODULE),(MAP_CALG(1,34),        13
     *MCALG_NMODULE)                                                      13
      CHARACTER CCALG_NMODULE*40/' '/                                     13
      CHARACTER NCALG_NMODULE*12/'R.NMODULE'/                             13
      INTEGER MCALG_NMODULE(2)/2,1/                                       13
      REAL RE_CALG_NMODULE                                                13
C -                                                                       13
      REAL CALG_SHIFT(2)                                                  13
      EQUIVALENCE (BANK_CALG(40),CALG_SHIFT,RE_CALG_SHIFT),               13
     *(NAMES_CALG(35),NCALG_SHIFT)                                        13
      EQUIVALENCE (COMMEN_CALG(35),CCALG_SHIFT),(MAP_CALG(1,35),          13
     *MCALG_SHIFT)                                                        13
      CHARACTER CCALG_SHIFT*40/' '/                                       13
      CHARACTER NCALG_SHIFT*12/'R.SHIFT'/                                 13
      INTEGER MCALG_SHIFT(2)/2,1/                                         13
      REAL RE_CALG_SHIFT                                                  13
C -                                                                       13
      REAL CALG_MAXMODULE                                                 13
      EQUIVALENCE (BANK_CALG(42),CALG_MAXMODULE,RE_CALG_MAXMODULE),       13
     *(NAMES_CALG(36),NCALG_MAXMODULE)                                    13
      EQUIVALENCE (COMMEN_CALG(36),CCALG_MAXMODULE),(MAP_CALG(1,36),      13
     *MCALG_MAXMODULE)                                                    13
      CHARACTER CCALG_MAXMODULE*40/' '/                                   13
      CHARACTER NCALG_MAXMODULE*12/'R.MAXMODULE'/                         13
      INTEGER MCALG_MAXMODULE(2)/1,1/                                     13
      REAL RE_CALG_MAXMODULE                                              13
C -                                                                       13
      REAL CALG_NETAT                                                     13
      EQUIVALENCE (BANK_CALG(43),CALG_NETAT,RE_CALG_NETAT),               13
     *(NAMES_CALG(37),NCALG_NETAT)                                        13
      EQUIVALENCE (COMMEN_CALG(37),CCALG_NETAT),(MAP_CALG(1,37),          13
     *MCALG_NETAT)                                                        13
      CHARACTER CCALG_NETAT*40/' '/                                       13
      CHARACTER NCALG_NETAT*12/'R.NETAT'/                                 13
      INTEGER MCALG_NETAT(2)/1,1/                                         13
      REAL RE_CALG_NETAT                                                  13
C -                                                                       13
      REAL CALG_NSUB                                                      13
      EQUIVALENCE (BANK_CALG(44),CALG_NSUB,RE_CALG_NSUB),                 13
     *(NAMES_CALG(38),NCALG_NSUB)                                         13
      EQUIVALENCE (COMMEN_CALG(38),CCALG_NSUB),(MAP_CALG(1,38),           13
     *MCALG_NSUB)                                                         13
      CHARACTER CCALG_NSUB*40/' '/                                        13
      CHARACTER NCALG_NSUB*12/'R.NSUB'/                                   13
      INTEGER MCALG_NSUB(2)/1,1/                                          13
      REAL RE_CALG_NSUB                                                   13
C -                                                                       13
      REAL CALG_NETASMDP                                                  13
      EQUIVALENCE (BANK_CALG(45),CALG_NETASMDP,RE_CALG_NETASMDP),         13
     *(NAMES_CALG(39),NCALG_NETASMDP)                                     13
      EQUIVALENCE (COMMEN_CALG(39),CCALG_NETASMDP),(MAP_CALG(1,39),       13
     *MCALG_NETASMDP)                                                     13
      CHARACTER CCALG_NETASMDP*40/' '/                                    13
      CHARACTER NCALG_NETASMDP*12/'R.NETASMDP'/                           13
      INTEGER MCALG_NETASMDP(2)/1,1/                                      13
      REAL RE_CALG_NETASMDP                                               13
C -                                                                       13
      REAL CALG_MODMAP(60)                                                13
      EQUIVALENCE (BANK_CALG(46),CALG_MODMAP,RE_CALG_MODMAP),             13
     *(NAMES_CALG(40),NCALG_MODMAP)                                       13
      EQUIVALENCE (COMMEN_CALG(40),CCALG_MODMAP),(MAP_CALG(1,40),         13
     *MCALG_MODMAP)                                                       13
      CHARACTER CCALG_MODMAP*40/' '/                                      13
      CHARACTER NCALG_MODMAP*12/'R.MODMAP'/                               13
      INTEGER MCALG_MODMAP(2)/60,1/                                       13
      REAL RE_CALG_MODMAP                                                 13
C -                                                                       13
      INTEGER LEN_CALG,LLEN_CALG,LINK_CALG                                13
      PARAMETER (LEN_CALG=105)                                            13
      PARAMETER (LLEN_CALG=40)                                            13
      INTEGER FLAG_CALG/0/,MAP_CALG(2,LLEN_CALG),IDNUM_CALG(10)           13
      CHARACTER NAMES_CALG(LLEN_CALG)*12,COMMEN_CALG(LLEN_CALG)*40        13
      DATA IDNUM_CALG/10*0/                                               13
      REAL BANK_CALG(LEN_CALG)                                            13
      CHARACTER*60 BPATH_CALG/' '/                                        13
      SAVE FLAG_CALG,MAP_CALG,IDNUM_CALG,NAMES_CALG,COMMEN_CALG,          13
     *BPATH_CALG                                                          13
      DATA LINK_CALG/0/,BANK_CALG/LEN_CALG*0/                             13
C *                                                                       13
C -                                                                       14
      REAL CALR_RMIN                                                      14
      EQUIVALENCE (BANK_CALR(3),CALR_RMIN,RE_CALR_RMIN),(NAMES_CALR(1),   14
     *NCALR_RMIN)                                                         14
      EQUIVALENCE (COMMEN_CALR(1),CCALR_RMIN),(MAP_CALR(1,1),             14
     *MCALR_RMIN)                                                         14
      CHARACTER CCALR_RMIN*40/' '/                                        14
      CHARACTER NCALR_RMIN*12/'R.RMIN'/                                   14
      INTEGER MCALR_RMIN(2)/1,1/                                          14
      REAL RE_CALR_RMIN                                                   14
C -                                                                       14
      REAL CALR_RPRS                                                      14
      EQUIVALENCE (BANK_CALR(4),CALR_RPRS,RE_CALR_RPRS),(NAMES_CALR(2),   14
     *NCALR_RPRS)                                                         14
      EQUIVALENCE (COMMEN_CALR(2),CCALR_RPRS),(MAP_CALR(1,2),             14
     *MCALR_RPRS)                                                         14
      CHARACTER CCALR_RPRS*40/' '/                                        14
      CHARACTER NCALR_RPRS*12/'R.RPRS'/                                   14
      INTEGER MCALR_RPRS(2)/1,1/                                          14
      REAL RE_CALR_RPRS                                                   14
C -                                                                       14
      REAL CALR_RSMD1                                                     14
      EQUIVALENCE (BANK_CALR(5),CALR_RSMD1,RE_CALR_RSMD1),                14
     *(NAMES_CALR(3),NCALR_RSMD1)                                         14
      EQUIVALENCE (COMMEN_CALR(3),CCALR_RSMD1),(MAP_CALR(1,3),            14
     *MCALR_RSMD1)                                                        14
      CHARACTER CCALR_RSMD1*40/' '/                                       14
      CHARACTER NCALR_RSMD1*12/'R.RSMD1'/                                 14
      INTEGER MCALR_RSMD1(2)/1,1/                                         14
      REAL RE_CALR_RSMD1                                                  14
C -                                                                       14
      REAL CALR_RSMD2                                                     14
      EQUIVALENCE (BANK_CALR(6),CALR_RSMD2,RE_CALR_RSMD2),                14
     *(NAMES_CALR(4),NCALR_RSMD2)                                         14
      EQUIVALENCE (COMMEN_CALR(4),CCALR_RSMD2),(MAP_CALR(1,4),            14
     *MCALR_RSMD2)                                                        14
      CHARACTER CCALR_RSMD2*40/' '/                                       14
      CHARACTER NCALR_RSMD2*12/'R.RSMD2'/                                 14
      INTEGER MCALR_RSMD2(2)/1,1/                                         14
      REAL RE_CALR_RSMD2                                                  14
C -                                                                       14
      REAL CALR_RMAX                                                      14
      EQUIVALENCE (BANK_CALR(7),CALR_RMAX,RE_CALR_RMAX),(NAMES_CALR(5),   14
     *NCALR_RMAX)                                                         14
      EQUIVALENCE (COMMEN_CALR(5),CCALR_RMAX),(MAP_CALR(1,5),             14
     *MCALR_RMAX)                                                         14
      CHARACTER CCALR_RMAX*40/' '/                                        14
      CHARACTER NCALR_RMAX*12/'R.RMAX'/                                   14
      INTEGER MCALR_RMAX(2)/1,1/                                          14
      REAL RE_CALR_RMAX                                                   14
C -                                                                       14
      INTEGER LEN_CALR,LLEN_CALR,LINK_CALR                                14
      PARAMETER (LEN_CALR=7)                                              14
      PARAMETER (LLEN_CALR=5)                                             14
      INTEGER FLAG_CALR/0/,MAP_CALR(2,LLEN_CALR),IDNUM_CALR(10)           14
      CHARACTER NAMES_CALR(LLEN_CALR)*12,COMMEN_CALR(LLEN_CALR)*40        14
      DATA IDNUM_CALR/10*0/                                               14
      REAL BANK_CALR(LEN_CALR)                                            14
      CHARACTER*60 BPATH_CALR/' '/                                        14
      SAVE FLAG_CALR,MAP_CALR,IDNUM_CALR,NAMES_CALR,COMMEN_CALR,          14
     *BPATH_CALR                                                          14
      DATA LINK_CALR/0/,BANK_CALR/LEN_CALR*0/                             14
C *                                                                       14
      REAL RKB2SC/0.013/,RKB3SC/9.6E-6/                                   15
      REAL CURRENT_DEPTH,CURRENT,LAYER_WIDTH(2),TAN_THETA,SMD_WIDTH,      20
     *SMD_WIDTH1,SMD_WIDTH2, SMD_WIDTH3,CUT_LENGTH,CUT_RADIUS,            20
     *FUTURE_DEPTH,C_DEP,C_LEAD_DEP,ETA_LENGHT, CURRENT_CSDA,H_ETA1,      20
     *H_ETA2,H_PHI1,H_PHI2,SH_ETA1,SH_ETA2,SH_PHI1,SH_PHI2,RMAX, HLENG,   20
     *DETA,ANGULAR_OFFSET,DPHITOT,DPHIMOD,DPHIT,R1,R2,R3,R4,RR(2)         20
      INTEGER LAYER,SUPER,SUB,I,J,II,NN,IMOD                              22
C *                                                                       24
      CALL AGSBEGM(MODULE,IPRIN)                                          24
      IF (IPRIN.LT.0) RETURN                                              24
C Filling bank CALG - BARREL CALORIMETER DATA                             24
      BANK_TITLE='BARREL CALORIMETER DATA'                                24
      BPATH_CALG='CALG*'                                                  24
      IDNUM_CALG(1)=0                                                     25
C GEOMETRY VERSION                                                        25
      CALG_VERSION=3.0                                                    25
      CCALG_VERSION='GEOMETRY VERSION'                                    25
C INNER RADIUS                                                            26
      CALG_RMIN=223.5                                                     26
      CCALG_RMIN='INNER RADIUS'                                           26
C CALORIMETER RAPIDITY CUT                                                27
      CALG_ETACUT=1.0                                                     27
      CCALG_ETACUT='CALORIMETER RAPIDITY CUT'                             27
C HALF WIDTH OF THE CRACK BETWEEN MODULES                                 28
      CALG_CRACKWD=0.655                                                  28
      CCALG_CRACKWD='HALF WIDTH OF THE CRACK BETWEEN MODULES'             28
C FRONT PLATE HALF THICKNESS                                              29
      CALG_FRONTTHK=0.9525                                                29
      CCALG_FRONTTHK='FRONT PLATE HALF THICKNESS'                         29
C BACK PLATE HALF THICKNESS                                               30
      CALG_COMPTHK=0.9525                                                 30
      CCALG_COMPTHK='BACK PLATE HALF THICKNESS'                           30
C AIR GAP HALF THICNESS                                                   31
      CALG_AIRTHK=0.158                                                   31
      CCALG_AIRTHK='AIR GAP HALF THICNESS'                                31
C MODULE BACK PLATE HALF THICKNES                                         32
      CALG_BACKTHK=1.5875                                                 32
      CCALG_BACKTHK='MODULE BACK PLATE HALF THICKNES'                     32
C SPACER BACK PLATE HALF THICKNES                                         33
      CALG_SPACETHK=0.9525                                                33
      CCALG_SPACETHK='SPACER BACK PLATE HALF THICKNES'                    33
C ACTIVE SCINTILLATOR PLATE HALF THICKNESS                                34
      CALG_SCINTTHK(1)=0.3                                                34
      CALG_SCINTTHK(2)=0.25                                               34
      CCALG_SCINTTHK='ACTIVE SCINTILLATOR PLATE HALF THICKNESS'           34
C ABSORBER PLATE THICKNESS HALFPTHICKNESS                                 35
      CALG_ABSORTHK=0.250                                                 35
      CCALG_ABSORTHK='ABSORBER PLATE THICKNESS HALFPTHICKNESS'            35
C ABSORBER PAPER PLATE THICKNESS HALF THICKNESS                           36
      CALG_ABPAPTHK=0.005                                                 36
      CCALG_ABPAPTHK='ABSORBER PAPER PLATE THICKNESS HALF THICKNESS'      36
C G10 SMD FRONT AND BACK PLATE HALF THICKNESS                             37
      CALG_G10SBTHK=0.115                                                 37
      CCALG_G10SBTHK='G10 SMD FRONT AND BACK PLATE HALF THICKNESS'        37
C SMD AL FRONT BACK PLATE WITH AR HALF WIDTH                              38
      CALG_SMALFWDH=11.2014                                               38
      CCALG_SMALFWDH='SMD AL FRONT BACK PLATE WITH AR HALF WIDTH'         38
C SMD AL FRONT BACK PLATE WITH AR HALF THICKNESS                          39
      CALG_SMALFTHK=0.3893                                                39
      CCALG_SMALFTHK='SMD AL FRONT BACK PLATE WITH AR HALF THICKNESS'     39
C SMD GAS BOX VOLUME HALF THICKNESS                                       40
      CALG_SMGASTHK=0.1359                                                40
      CCALG_SMGASTHK='SMD GAS BOX VOLUME HALF THICKNESS'                  40
C SMD GAS BOX VOLUME HALF WIDTH                                           41
      CALG_SMGASWDH=0.2946                                                41
      CCALG_SMGASWDH='SMD GAS BOX VOLUME HALF WIDTH'                      41
C SMD GAS VOLUME TUBS RADIUS                                              42
      CALG_SMGASRAD=0.2946                                                42
      CCALG_SMGASRAD='SMD GAS VOLUME TUBS RADIUS'                         42
C SMD AL FRONT FIRST (LAST) RIB HALF WIDTH                                43
      CALG_SMAFFWDH=0.113                                                 43
      CCALG_SMAFFWDH='SMD AL FRONT FIRST (LAST) RIB HALF WIDTH'           43
C SMD AL BACK FIRST (LAST) RIB HALF WIDTH                                 44
      CALG_SMAFBWDH=0.1664                                                44
      CCALG_SMAFBWDH='SMD AL BACK FIRST (LAST) RIB HALF WIDTH'            44
C ETA=0 STRIP NOTCH HALF WIDTH                                            45
      CALG_SMETAWDH=0.9806                                                45
      CCALG_SMETAWDH='ETA=0 STRIP NOTCH HALF WIDTH'                       45
C STRIP#1-75 HALF WITDH                                                   46
      CALG_SETA1WDH=0.7277                                                46
      CCALG_SETA1WDH='STRIP#1-75 HALF WITDH'                              46
C STRIP#76-150 HALF WITDH                                                 47
      CALG_SETA2WDH=0.9398                                                47
      CCALG_SETA2WDH='STRIP#76-150 HALF WITDH'                            47
C HALF DISTANCE BETWEEN STRIPS IN ETA                                     48
      CALG_SET12WDH=0.04064                                               48
      CCALG_SET12WDH='HALF DISTANCE BETWEEN STRIPS IN ETA'                48
C STRIP#(1-NPHISTR) IN PHI DIRECTION HALF WITDH                           49
      CALG_SPHIWDH=0.6680                                                 49
      CCALG_SPHIWDH='STRIP#(1-NPHISTR) IN PHI DIRECTION HALF WITDH'       49
C HALF DISTANCE BETWEEN STRIPS IN PHI                                     50
      CALG_SPHIDWDH=0.07874                                               50
      CCALG_SPHIDWDH='HALF DISTANCE BETWEEN STRIPS IN PHI'                50
C NUMBER SMD GASEUS INTERVAL IN TILE                                      51
      CALG_NSMDALW=30                                                     51
      CCALG_NSMDALW='NUMBER SMD GASEUS INTERVAL IN TILE'                  51
C NUMBER OF READOUT SUPERLAYER                                            52
      CALG_NSUPER=2                                                       52
      CCALG_NSUPER='NUMBER OF READOUT SUPERLAYER'                         52
C SMD POSITIONED AFTER SANDVICH TYPE LAYERS EMC                           53
      CALG_NSMD=5                                                         53
      CCALG_NSMD='SMD POSITIONED AFTER SANDVICH TYPE LAYERS EMC'          53
C NUMBER OF LAYERS IN A SUPERLAYER                                        54
      CALG_NSUBLAY(1)=2                                                   54
      CALG_NSUBLAY(2)=19                                                  54
      CCALG_NSUBLAY='NUMBER OF LAYERS IN A SUPERLAYER'                    54
C MAX NUMBER OF MOUDLE                                                    55
      CALG_MAXMODULE=60                                                   55
      CCALG_MAXMODULE='MAX NUMBER OF MOUDLE'                              55
C NUMBER OF ETA DIVISION FOR TOWER/PRESHOWER                              56
      CALG_NETAT=20                                                       56
      CCALG_NETAT='NUMBER OF ETA DIVISION FOR TOWER/PRESHOWER'            56
C NUMBER OF SUB DIV. IN PHI FOR TOWER/PRESHOWER                           57
      CALG_NSUB=2                                                         57
      CCALG_NSUB='NUMBER OF SUB DIV. IN PHI FOR TOWER/PRESHOWER'          57
C NUMBER OF ETA DIVISION IN FOR SMD PHI PLANE                             58
      CALG_NETASMDP=10                                                    58
      CCALG_NETASMDP='NUMBER OF ETA DIVISION IN FOR SMD PHI PLANE'        58
C NUMBER OF THE STRIP IN PHI DIRECTION                                    59
      CALG_NPHISTR=15                                                     59
      CCALG_NPHISTR='NUMBER OF THE STRIP IN PHI DIRECTION'                59
C NUMBER OF STRIP IN FIRST PART ETA=0-0.5                                 60
      CALG_NETFIRST=75.                                                   60
      CCALG_NETFIRST='NUMBER OF STRIP IN FIRST PART ETA=0-0.5'            60
C NUMBER OF STRIP IN SECOND PART ETA=0.5-1.0                              61
      CALG_NETSECON=75.                                                   61
      CCALG_NETSECON='NUMBER OF STRIP IN SECOND PART ETA=0.5-1.0'         61
C NUMBER OF MODULES                                                       62
      CALG_NMODULE(1)=60                                                  62
      CALG_NMODULE(2)=60                                                  62
      CCALG_NMODULE='NUMBER OF MODULES'                                   62
C STARTING AZIMUTH OF THE FIRST MODULE                                    63
      CALG_SHIFT(1)=75                                                    63
      CALG_SHIFT(2)=105                                                   63
      CCALG_SHIFT='STARTING AZIMUTH OF THE FIRST MODULE'                  63
C POPULATED MODULES MAP                                                   69
      CALG_MODMAP(1)=1                                                    69
      CALG_MODMAP(2)=1                                                    69
      CALG_MODMAP(3)=1                                                    69
      CALG_MODMAP(4)=1                                                    69
      CALG_MODMAP(5)=1                                                    69
      CALG_MODMAP(6)=1                                                    69
      CALG_MODMAP(7)=1                                                    69
      CALG_MODMAP(8)=1                                                    69
      CALG_MODMAP(9)=1                                                    69
      CALG_MODMAP(10)=1                                                   69
      CALG_MODMAP(11)=1                                                   69
      CALG_MODMAP(12)=1                                                   69
      CALG_MODMAP(13)=1                                                   69
      CALG_MODMAP(14)=1                                                   69
      CALG_MODMAP(15)=1                                                   69
      CALG_MODMAP(16)=0                                                   69
      CALG_MODMAP(17)=0                                                   69
      CALG_MODMAP(18)=0                                                   69
      CALG_MODMAP(19)=0                                                   69
      CALG_MODMAP(20)=0                                                   69
      CALG_MODMAP(21)=0                                                   69
      CALG_MODMAP(22)=0                                                   69
      CALG_MODMAP(23)=0                                                   69
      CALG_MODMAP(24)=0                                                   69
      CALG_MODMAP(25)=0                                                   69
      CALG_MODMAP(26)=0                                                   69
      CALG_MODMAP(27)=0                                                   69
      CALG_MODMAP(28)=0                                                   69
      CALG_MODMAP(29)=0                                                   69
      CALG_MODMAP(30)=0                                                   69
      CALG_MODMAP(31)=0                                                   69
      CALG_MODMAP(32)=0                                                   69
      CALG_MODMAP(33)=0                                                   69
      CALG_MODMAP(34)=0                                                   69
      CALG_MODMAP(35)=0                                                   69
      CALG_MODMAP(36)=0                                                   69
      CALG_MODMAP(37)=0                                                   69
      CALG_MODMAP(38)=0                                                   69
      CALG_MODMAP(39)=0                                                   69
      CALG_MODMAP(40)=0                                                   69
      CALG_MODMAP(41)=0                                                   69
      CALG_MODMAP(42)=0                                                   69
      CALG_MODMAP(43)=0                                                   69
      CALG_MODMAP(44)=0                                                   69
      CALG_MODMAP(45)=0                                                   69
      CALG_MODMAP(46)=1                                                   69
      CALG_MODMAP(47)=1                                                   69
      CALG_MODMAP(48)=1                                                   69
      CALG_MODMAP(49)=1                                                   69
      CALG_MODMAP(50)=1                                                   69
      CALG_MODMAP(51)=1                                                   69
      CALG_MODMAP(52)=1                                                   69
      CALG_MODMAP(53)=1                                                   69
      CALG_MODMAP(54)=1                                                   69
      CALG_MODMAP(55)=1                                                   69
      CALG_MODMAP(56)=1                                                   69
      CALG_MODMAP(57)=1                                                   69
      CALG_MODMAP(58)=1                                                   69
      CALG_MODMAP(59)=1                                                   69
      CALG_MODMAP(60)=1                                                   69
      CCALG_MODMAP='POPULATED MODULES MAP'                                69
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALG',    70
     *BPATH_CALG,IDNUM_CALG,1, MAP_CALG,NAMES_CALG,COMMEN_CALG,           70
     *BANK_CALG,LEN_CALG,LLEN_CALG,LINK_CALG,FLAG_CALG)                   70
C *                                                                       70
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALG',    70
     *BPATH_CALG,IDNUM_CALG,1, MAP_CALG,NAMES_CALG,COMMEN_CALG,           70
     *BANK_CALG,LEN_CALG,LLEN_CALG,LINK_CALG,FLAG_CALG)                   70
C use default CALG bank                                                   71
      BPATH_CALG='CALG*'                                                  71
      IDNUM_CALG(1)=0                                                     71
      CALL AGDGETP('CALBGEO1','*',-999,'CALG',' ',BANK_CALG, BPATH_CALG,  71
     *IDNUM_CALG,1,MAP_CALG,NAMES_CALG,BANK_CALG,LEN_CALG,LLEN_CALG,      71
     *LINK_CALG,FLAG_CALG)                                                71
      SMD_WIDTH=2.*CALG_G10SBTHK+2.*CALG_SMALFTHK+2.*CALG_ABPAPTHK        72
      SMD_WIDTH1=2.*CALG_G10SBTHK+2.*CALG_ABPAPTHK                        73
      SMD_WIDTH2=SMD_WIDTH1+CALG_SMGASTHK+CALG_SMGASRAD                   74
      SMD_WIDTH3=2.*SMD_WIDTH-SMD_WIDTH1-CALG_SMGASTHK-CALG_SMGASRAD      75
      R1=CALG_RMIN+2.*CALG_FRONTTHK                                       76
      R2=0.0                                                              77
C *                                                                       78
      DO 5011 I=1,NINT(CALG_NSUPER)                                       78
         LAYER_WIDTH(I) = CALG_SCINTTHK(I) +                              79
     *   CALG_ABSORTHK+2.*CALG_ABPAPTHK                                   79
         R2=R2+(CALG_NSUBLAY(I)-I+1)*LAYER_WIDTH(I)*2.0                   80
         RR(I)=R2                                                         81
5011  CONTINUE                                                            82
5012  CONTINUE                                                            82
      R3=                                                                 83
     *(CALG_NSUPER*LAYER_WIDTH(1)+(CALG_NSMD-CALG_NSUPER)*LAYER_WIDTH(2)  83
     *)*2.                                                                83
      R4=(SMD_WIDTH+CALG_SCINTTHK(2)+2.*CALG_ABPAPTHK)*2.0                84
      CUT_RADIUS=R1+R2+R4                                                 85
      RMAX=                                                               86
     *CUT_RADIUS+2.*(CALG_BACKTHK+CALG_SPACETHK+CALG_COMPTHK+             86
     *CALG_AIRTHK)                                                        86
      TAN_THETA = TAN(2*ATAN(EXP(-CALG_ETACUT)))                          87
      CUT_LENGTH = CALG_RMIN/TAN_THETA                                    88
      HLENG = CUT_RADIUS/TAN_THETA                                        89
      NN = MAX(CALG_NMODULE(1),CALG_NMODULE(2))                           90
      DETA = 1.0/CALG_NETAT                                               91
      DPHIMOD = 360/CALG_MAXMODULE                                        92
      DPHIT = DPHIMOD/CALG_NSUB                                           93
      DPHITOT = DPHIMOD*NN                                                94
C *                                                                       95
      CALL AGSBEGM(MODULE,IPRIN)                                          95
      IF (IPRIN.LT.0) RETURN                                              95
C Filling bank CALR - BARREL EMC RADIUSES                                 95
      BANK_TITLE='BARREL EMC RADIUSES'                                    95
      BPATH_CALR='CALR*'                                                  95
      IDNUM_CALR(1)=0                                                     96
C INNER RADUIS OF SENSITIVE AREA                                          96
      CALR_RMIN=R1                                                        96
      CCALR_RMIN='INNER RADUIS OF SENSITIVE AREA'                         96
C MEAN RADUIS OF PRS                                                      97
      CALR_RPRS=R1+RR(1)/2.0                                              97
      CCALR_RPRS='MEAN RADUIS OF PRS'                                     97
C MEAN RADUIS OF SMD                                                      98
      CALR_RSMD1=R1+R3+SMD_WIDTH2                                         98
      CCALR_RSMD1='MEAN RADUIS OF SMD'                                    98
C MEAN RADUIS OF SMD                                                      99
      CALR_RSMD2=R1+R3+SMD_WIDTH3                                         99
      CCALR_RSMD2='MEAN RADUIS OF SMD'                                    99
C OUTER RADUIS OF SENSITIVE AREA                                          100
      CALR_RMAX=CUT_RADIUS                                                100
      CCALR_RMAX='OUTER RADUIS OF SENSITIVE AREA'                         100
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALR',    101
     *BPATH_CALR,IDNUM_CALR,1, MAP_CALR,NAMES_CALR,COMMEN_CALR,           101
     *BANK_CALR,LEN_CALR,LLEN_CALR,LINK_CALR,FLAG_CALR)                   101
C *                                                                       101
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALR',    101
     *BPATH_CALR,IDNUM_CALR,1, MAP_CALR,NAMES_CALR,COMMEN_CALR,           101
     *BANK_CALR,LEN_CALR,LLEN_CALR,LINK_CALR,FLAG_CALR)                   101
C use default CALR bank                                                   102
      BPATH_CALR='CALR*'                                                  102
      IDNUM_CALR(1)=0                                                     102
      CALL AGDGETP('CALBGEO1','*',-999,'CALR',' ',BANK_CALR, BPATH_CALR,  102
     *IDNUM_CALR,1,MAP_CALR,NAMES_CALR,BANK_CALR,LEN_CALR,LLEN_CALR,      102
     *LINK_CALR,FLAG_CALR)                                                102
      CALL AGSBEGM(MODULE,IPRIN)                                          103
C Check IPRIN<0                                                           103
      IF (IPRIN.LT.0) RETURN                                              103
C Create CALB :                                                           103
C Call To Local Procedure CALB                                            103
      ASSIGN 5030 TO BLOCK_CALB                                           103
      GO TO  5020                                                         103
5030  CONTINUE                                                            103
C *                                                                       103
C Geant POSITION : CALB ===> MOTHER='CAVE' <===                           104
      CALL AGSRESET                                                       104
      AG_MOTHER = 'CAVE'                                                  104
      AG_TITLE = 'CALB'                                                   104
      AG_EXNAME = 'POSITION'                                              104
      AG_PARLIST = 'MOTHER'                                               104
      CALL AXPOSITION                                                     104
C *                                                                       104
C Check IPRIN>=1                                                          104
      IF (IPRIN.GE.1) WRITE(LOUT,5040)CALG_VERSION                        104
5040  FORMAT(' CALB geo. version =',F7.1)                                 106
C ----------------------------------------------------------------------  107
C ------------ CALB                                                       107
      CALL AGSENDM                                                        108
      RETURN                                                              108
C Begin Local Procedure CALB                                              108
C CALB IS EMC BARREL ENVELOPE                                             108
5020  CONTINUE                                                            108
         AG_TITLE='CALB'                                                  108
         AG_MODULE=MODULE                                                 108
         AG_MARK='V00'                                                    108
         CALL AGSPUSH                                                     108
         AG_MODULE=MODULE                                                 108
C    *                                                                    108
C    Geant MATERIAL : AIR ===> inherited arguments used <===              109
         CALL AGSRESET                                                    109
         AG_TITLE = 'AIR'                                                 109
         AG_EXNAME = 'MATERIAL'                                           109
         AG_PARLIST = 'NONE'                                              109
         CALL AXMATERIAL                                                  109
C    *                                                                    109
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           109
         CALL AGSRESET                                                    110
         AG_TITLE = 'STANDARD'                                            110
         AG_EXNAME = 'MEDIUM'                                             110
         AG_PARLIST = 'NONE'                                              110
         CALL AXMEDIUM                                                    110
C    *                                                                    110
C    Geant ATTRIBUTE : CALB ===> SEEN=0 COLO=7 <===                       110
         CALL AGSRESET                                                    111
         AG_SEEN = 0                                                      111
         AG_COLO = 7                                                      111
         AG_TITLE = 'CALB'                                                111
         AG_EXNAME = 'ATTRIBUTE'                                          111
         AG_PARLIST = 'SEEN_COLO'                                         111
         CALL AXATTRIBUTE                                                 111
C    *                                                                    111
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={-HLENG, -CUT_LENGT  111
C    H,CUT_LENGTH,HLENG} RMN={CUT_RADIUS, CALG_RMIN,CALG_RMIN,CUT_RADIUS  111
C    } RMX={RMAX, RMAX,RMAX,RMAX} <===                                    111
         CALL AGSRESET                                                    115
         AG_PHI1 = 0                                                      115
         AG_DPHI = 360                                                    115
         AG_NZ = 4                                                        115
         AG_ZI(1)=-HLENG                                                  115
         AG_ZI(2)=-CUT_LENGTH                                             115
         AG_ZI(3)=CUT_LENGTH                                              115
         AG_ZI(4)=HLENG                                                   115
         AG_RMN(1)=CUT_RADIUS                                             115
         AG_RMN(2)=CALG_RMIN                                              115
         AG_RMN(3)=CALG_RMIN                                              115
         AG_RMN(4)=CUT_RADIUS                                             115
         AG_RMX(1)=RMAX                                                   115
         AG_RMX(2)=RMAX                                                   115
         AG_RMX(3)=RMAX                                                   115
         AG_RMX(4)=RMAX                                                   115
         AG_TITLE = 'PCON'                                                115
         AG_EXNAME = 'SHAPE'                                              115
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           115
         CALL AXSHAPE                                                     115
C    *                                                                    115
C    Check CALG_NMODULE(1)>0                                              115
         IF (CALG_NMODULE(1).GT.0) THEN                                   116
         II=1                                                             116
C    Create CHLV :                                                        118
C    Call To Local Procedure CHLV                                         118
         ASSIGN 5060 TO BLOCK_CHLV                                        118
         GO TO  5050                                                      118
5060     CONTINUE                                                         118
C    *                                                                    118
C    Geant POSITION : CHLV ===> inherited arguments used <===             119
         CALL AGSRESET                                                    119
         AG_TITLE = 'CHLV'                                                119
         AG_EXNAME = 'POSITION'                                           119
         AG_PARLIST = 'NONE'                                              119
         CALL AXPOSITION                                                  119
C    *                                                                    119
         ENDIF                                                            119
C    Check CALG_NMODULE(2)>0                                              121
         IF (CALG_NMODULE(2).GT.0) THEN                                   121
         II=2                                                             121
C    Create CHLV :                                                        123
C    Call To Local Procedure CHLV                                         123
         ASSIGN 5070 TO BLOCK_CHLV                                        123
         GO TO  5050                                                      123
5070     CONTINUE                                                         123
C    *                                                                    123
C    Geant POSITION : CHLV ===> THETAZ=180 <===                           124
         CALL AGSRESET                                                    124
         AG_THETAZ = 180                                                  124
         AG_TITLE = 'CHLV'                                                124
         AG_EXNAME = 'POSITION'                                           124
         AG_PARLIST = 'THETAZ'                                            124
         CALL AXPOSITION                                                  124
C    *                                                                    124
         ENDIF                                                            124
5021  CALL AGSPOPB                                                        126
      GO TO BLOCK_CALB, (5020 ,5030)                                      126
C End of Local Procedure                                                  126
C ----------------------------------------------------------------------  127
C ------------ CHLV                                                       127
C Begin Local Procedure CHLV                                              128
C CHLV CORRESPONDS TO DOUBLE MODULES...                                   128
5050  CONTINUE                                                            128
         AG_TITLE='CHLV'                                                  128
         AG_MODULE=MODULE                                                 128
         AG_MARK='V00'                                                    128
         CALL AGSPUSH                                                     128
         AG_MODULE=MODULE                                                 128
C    *                                                                    128
C    Geant SHAPE : PCON ===> PHI1=CALG_SHIFT(II) DPHI=DPHIMOD*CALG_NMODU  132
C    LE(II), NZ=3 ZI={0,CUT_LENGTH,HLENG}, RMN={CALG_RMIN,CALG_RMIN,CUT_  132
C    RADIUS} RMX={RMAX, RMAX,RMAX} <===                                   132
         CALL AGSRESET                                                    132
         AG_PHI1 = CALG_SHIFT(II)                                         132
         AG_DPHI = DPHIMOD*CALG_NMODULE(II)                               132
         AG_NZ = 3                                                        132
         AG_ZI(1)=0                                                       132
         AG_ZI(2)=CUT_LENGTH                                              132
         AG_ZI(3)=HLENG                                                   132
         AG_RMN(1)=CALG_RMIN                                              132
         AG_RMN(2)=CALG_RMIN                                              132
         AG_RMN(3)=CUT_RADIUS                                             132
         AG_RMX(1)=RMAX                                                   132
         AG_RMX(2)=RMAX                                                   132
         AG_RMX(3)=RMAX                                                   132
         AG_TITLE = 'PCON'                                                132
         AG_EXNAME = 'SHAPE'                                              132
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           132
         CALL AXSHAPE                                                     132
C    *                                                                    132
C    Create CPHI :                                                        133
C    Call To Local Procedure CPHI                                         133
         ASSIGN 5090 TO BLOCK_CPHI                                        133
         GO TO  5080                                                      133
5090     CONTINUE                                                         133
C    *                                                                    133
C    *                                                                    134
         IMOD=1                                                           134
            GO TO 5103                                                    134
5101        IMOD=IMOD+1                                                   134
5103        IF(IMOD-(CALG_NMODULE(II)).GT.0) GO TO 5102                   134
            ANGULAR_OFFSET=CALG_SHIFT(II)+3.0+6.0*(IMOD-1)                135
C       Check II==1                                                       136
            IF (II.EQ.1) THEN                                             136
C       Geant POSITION : CPHI ===> ALPHAZ=ANGULAR_OFFSET NCOPY=IMOD <===  137
            CALL AGSRESET                                                 137
            AG_ALPHAN=AG_ALPHAN+1                                         137
            AG_ALPHAK(AG_ALPHAN)=3                                        137
            AG_ALPHAL(AG_ALPHAN)=ANGULAR_OFFSET                           137
            AG_NCOPY = IMOD                                               137
            AG_TITLE = 'CPHI'                                             137
            AG_EXNAME = 'POSITION'                                        137
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   137
            CALL AXPOSITION                                               137
C       *                                                                 137
            ENDIF                                                         137
C       Check II.EQ.2.AND.CALG_MODMAP(IMOD).GT.0                          139
            IF (II.EQ.2.AND.CALG_MODMAP(IMOD).GT.0) THEN                  139
C       Geant POSITION : CPHI ===> ALPHAZ=ANGULAR_OFFSET NCOPY=IMOD <===  140
            CALL AGSRESET                                                 140
            AG_ALPHAN=AG_ALPHAN+1                                         140
            AG_ALPHAK(AG_ALPHAN)=3                                        140
            AG_ALPHAL(AG_ALPHAN)=ANGULAR_OFFSET                           140
            AG_NCOPY = IMOD                                               140
            AG_TITLE = 'CPHI'                                             140
            AG_EXNAME = 'POSITION'                                        140
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   140
            CALL AXPOSITION                                               140
C       *                                                                 140
            ENDIF                                                         140
         GO TO 5101                                                       142
5102     CONTINUE                                                         142
5051  CALL AGSPOPB                                                        143
      GO TO BLOCK_CHLV, (5050 ,5060,5070)                                 143
C End of Local Procedure                                                  143
C ----------------------------------------------------------------------  144
C ------------ CPHI                                                       144
C Begin Local Procedure CPHI                                              145
C CPHI CORRESPONDS TO A SINGLE MODULE                                     145
5080  CONTINUE                                                            145
         AG_TITLE='CPHI'                                                  145
         AG_MODULE=MODULE                                                 145
         AG_MARK='V00'                                                    145
         CALL AGSPUSH                                                     145
         AG_MODULE=MODULE                                                 145
C    *                                                                    145
C    Geant ATTRIBUTE : CPHI ===> SEEN=1 COLO=5 <===                       146
         CALL AGSRESET                                                    146
         AG_SEEN = 1                                                      146
         AG_COLO = 5                                                      146
         AG_TITLE = 'CPHI'                                                146
         AG_EXNAME = 'ATTRIBUTE'                                          146
         AG_PARLIST = 'SEEN_COLO'                                         146
         CALL AXATTRIBUTE                                                 146
C    *                                                                    146
C    Geant SHAPE : PCON ===> PHI1=-3.0 DPHI=6.0 NZ=3 ZI={0, CUT_LENGTH,H  146
C    LENG} RMN={CALG_RMIN,CALG_RMIN,CUT_RADIUS}, RMX={RMAX,RMAX,RMAX} <=  146
C    ==                                                                   146
         CALL AGSRESET                                                    149
         AG_PHI1 = -3.0                                                   149
         AG_DPHI = 6.0                                                    149
         AG_NZ = 3                                                        149
         AG_ZI(1)=0                                                       149
         AG_ZI(2)=CUT_LENGTH                                              149
         AG_ZI(3)=HLENG                                                   149
         AG_RMN(1)=CALG_RMIN                                              149
         AG_RMN(2)=CALG_RMIN                                              149
         AG_RMN(3)=CUT_RADIUS                                             149
         AG_RMX(1)=RMAX                                                   149
         AG_RMX(2)=RMAX                                                   149
         AG_RMX(3)=RMAX                                                   149
         AG_TITLE = 'PCON'                                                149
         AG_EXNAME = 'SHAPE'                                              149
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           149
         CALL AXSHAPE                                                     149
C    *                                                                    149
         CURRENT_DEPTH = CALG_RMIN                                        149
         C_DEP=CURRENT_DEPTH                                              151
         AG_DX = CALG_FRONTTHK                                            152
C    Create CBTW :                                                        152
C    Call To Local Procedure CBTW                                         152
         ASSIGN 5120 TO BLOCK_CBTW                                        152
         GO TO  5110                                                      152
5120     CONTINUE                                                         152
C    *                                                                    152
C    Geant POSITION : CBTW ===> X=CALG_RMIN+CALG_FRONTTHK Z=CURRENT_DEPT  153
C    H/TAN_THETA/2 <===                                                   153
         CALL AGSRESET                                                    153
         AG_X = CALG_RMIN+CALG_FRONTTHK                                   153
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 153
         AG_TITLE = 'CBTW'                                                153
         AG_EXNAME = 'POSITION'                                           153
         AG_PARLIST = 'X_Z'                                               153
         CALL AXPOSITION                                                  153
C    *                                                                    153
         CURRENT_DEPTH = CURRENT_DEPTH + 2*CALG_FRONTTHK                  153
         LAYER = 0                                                        155
C    *                                                                    156
         SUPER=1                                                          156
            GO TO 5133                                                    156
5131        SUPER=SUPER+1                                                 156
5133        IF(SUPER-(NINT(CALG_NSUPER)).GT.0) GO TO 5132                 156
C       Create CSUP :                                                     157
C       Call To Local Procedure CSUP                                      157
            ASSIGN 5150 TO BLOCK_CSUP                                     157
            GO TO  5140                                                   157
5150        CONTINUE                                                      157
C       *                                                                 157
C       Geant POSITION : CSUP ===> inherited arguments used <===          158
            CALL AGSRESET                                                 158
            AG_TITLE = 'CSUP'                                             158
            AG_EXNAME = 'POSITION'                                        158
            AG_PARLIST = 'NONE'                                           158
            CALL AXPOSITION                                               158
C       *                                                                 158
         GO TO 5131                                                       159
5132     CONTINUE                                                         159
         AG_DX = CALG_COMPTHK                                             160
C    Create CBTW :                                                        160
C    Call To Local Procedure CBTW                                         160
         ASSIGN 5160 TO BLOCK_CBTW                                        160
         GO TO  5110                                                      160
5160     CONTINUE                                                         160
C    *                                                                    160
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+CALG_COMPTHK Z=CURRENT_D  161
C    EPTH/TAN_THETA/2 <===                                                161
         CALL AGSRESET                                                    161
         AG_X = CURRENT_DEPTH+CALG_COMPTHK                                161
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 161
         AG_TITLE = 'CBTW'                                                161
         AG_EXNAME = 'POSITION'                                           161
         AG_PARLIST = 'X_Z'                                               161
         CALL AXPOSITION                                                  161
C    *                                                                    161
         C_DEP=2.*CALG_COMPTHK + 2.*CALG_AIRTHK                           161
         AG_DX = CALG_BACKTHK                                             163
C    Create CBTW :                                                        163
C    Call To Local Procedure CBTW                                         163
         ASSIGN 5170 TO BLOCK_CBTW                                        163
         GO TO  5110                                                      163
5170     CONTINUE                                                         163
C    *                                                                    163
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+C_DEP+CALG_BACKTHK Z=CUR  164
C    RENT_DEPTH/TAN_THETA/2 <===                                          164
         CALL AGSRESET                                                    164
         AG_X = CURRENT_DEPTH+C_DEP+CALG_BACKTHK                          164
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 164
         AG_TITLE = 'CBTW'                                                164
         AG_EXNAME = 'POSITION'                                           164
         AG_PARLIST = 'X_Z'                                               164
         CALL AXPOSITION                                                  164
C    *                                                                    164
         C_DEP=C_DEP + 2.*CALG_BACKTHK                                    164
         AG_DX = CALG_SPACETHK                                            166
C    Create CBTW :                                                        166
C    Call To Local Procedure CBTW                                         166
         ASSIGN 5180 TO BLOCK_CBTW                                        166
         GO TO  5110                                                      166
5180     CONTINUE                                                         166
C    *                                                                    166
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+C_DEP+CALG_SPACETHK Z=CU  167
C    RRENT_DEPTH/TAN_THETA/2 <===                                         167
         CALL AGSRESET                                                    167
         AG_X = CURRENT_DEPTH+C_DEP+CALG_SPACETHK                         167
         AG_Z = CURRENT_DEPTH/TAN_THETA/2                                 167
         AG_TITLE = 'CBTW'                                                167
         AG_EXNAME = 'POSITION'                                           167
         AG_PARLIST = 'X_Z'                                               167
         CALL AXPOSITION                                                  167
C    *                                                                    167
         C_DEP=C_DEP + 2.*CALG_SPACETHK                                   167
         CURRENT_DEPTH=CURRENT_DEPTH + C_DEP                              169
5081  CALL AGSPOPB                                                        170
      GO TO BLOCK_CPHI, (5080 ,5090)                                      170
C End of Local Procedure                                                  170
C ----------------------------------------------------------------------  171
C ------------ CSUP                                                       171
C Begin Local Procedure CSUP                                              172
C CSUP IS A SUPER LAYER WITH FEW LAYERS INSIDE                            172
5140  CONTINUE                                                            172
         AG_TITLE='CSUP'                                                  172
         AG_MODULE=MODULE                                                 172
         AG_MARK='V00'                                                    172
         CALL AGSPUSH                                                     172
         AG_MODULE=MODULE                                                 172
C    *                                                                    172
         FUTURE_DEPTH=CURRENT_DEPTH + (CALG_NSUBLAY(SUPER) - SUPER +      174
     *   1)*LAYER_WIDTH(SUPER)*2 + (SMD_WIDTH + CALG_SCINTTHK(SUPER) +    174
     *   2.*CALG_ABPAPTHK)*2*(SUPER - 1)                                  174
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=6./21. <===                  175
         CALL AGSRESET                                                    175
         AG_A = 12.01                                                     175
         AG_Z = 6.                                                        175
         AG_W = 6./21.                                                    175
         AG_TITLE = 'C'                                                   175
         AG_EXNAME = 'COMPONENT'                                          175
         AG_PARLIST = 'A_Z_W'                                             175
         CALL AXCOMPONENT                                                 175
C    *                                                                    175
C    Geant COMPONENT : H ===> A=1. Z=1. W=10./21. <===                    175
         CALL AGSRESET                                                    176
         AG_A = 1.                                                        176
         AG_Z = 1.                                                        176
         AG_W = 10./21.                                                   176
         AG_TITLE = 'H'                                                   176
         AG_EXNAME = 'COMPONENT'                                          176
         AG_PARLIST = 'A_Z_W'                                             176
         CALL AXCOMPONENT                                                 176
C    *                                                                    176
C    Geant COMPONENT : O ===> A=16. Z=8. W=5./21. <===                    176
         CALL AGSRESET                                                    177
         AG_A = 16.                                                       177
         AG_Z = 8.                                                        177
         AG_W = 5./21.                                                    177
         AG_TITLE = 'O'                                                   177
         AG_EXNAME = 'COMPONENT'                                          177
         AG_PARLIST = 'A_Z_W'                                             177
         CALL AXCOMPONENT                                                 177
C    *                                                                    177
C    Geant MIXTURE : CELLULOSE ===> ISVOL=1 DENS=0.35 <===                177
         CALL AGSRESET                                                    178
         AG_ISVOL = 1                                                     178
         AG_DENS = 0.35                                                   178
         AG_TITLE = 'CELLULOSE'                                           178
         AG_EXNAME = 'MIXTURE'                                            178
         AG_PARLIST = 'ISVOL_DENS'                                        178
         CALL AXMIXTURE                                                   178
C    *                                                                    178
C    Geant ATTRIBUTE : CSUP ===> SEEN=0 COLO=1 <===                       178
         CALL AGSRESET                                                    179
         AG_SEEN = 0                                                      179
         AG_COLO = 1                                                      179
         AG_TITLE = 'CSUP'                                                179
         AG_EXNAME = 'ATTRIBUTE'                                          179
         AG_PARLIST = 'SEEN_COLO'                                         179
         CALL AXATTRIBUTE                                                 179
C    *                                                                    179
C    Geant SHAPE : PCON ===> PHI1=-3.0 DPHI=DPHIMOD NZ=3 ZI={0, CURRENT_  179
C    DEPTH/TAN_THETA,FUTURE_DEPTH/TAN_THETA}, RMN={CURRENT_DEPTH,CURRENT  179
C    _DEPTH,FUTURE_DEPTH}, RMX={FUTURE_DEPTH,FUTURE_DEPTH,FUTURE_DEPTH}   179
C    <===                                                                 179
         CALL AGSRESET                                                    183
         AG_PHI1 = -3.0                                                   183
         AG_DPHI = DPHIMOD                                                183
         AG_NZ = 3                                                        183
         AG_ZI(1)=0                                                       183
         AG_ZI(2)=CURRENT_DEPTH/TAN_THETA                                 183
         AG_ZI(3)=FUTURE_DEPTH/TAN_THETA                                  183
         AG_RMN(1)=CURRENT_DEPTH                                          183
         AG_RMN(2)=CURRENT_DEPTH                                          183
         AG_RMN(3)=FUTURE_DEPTH                                           183
         AG_RMX(1)=FUTURE_DEPTH                                           183
         AG_RMX(2)=FUTURE_DEPTH                                           183
         AG_RMX(3)=FUTURE_DEPTH                                           183
         AG_TITLE = 'PCON'                                                183
         AG_EXNAME = 'SHAPE'                                              183
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           183
         CALL AXSHAPE                                                     183
C    *                                                                    183
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               183
C    *                                                                    185
         SUB=1                                                            185
            GO TO 5193                                                    185
5191        SUB=SUB+1                                                     185
5193        IF(SUB-(NINT(CALG_NSUBLAY(SUPER))).GT.0) GO TO 5192           185
            LAYER = LAYER + 1                                             186
C       Check LAYER.LT.NINT(CALG_NSUBLAY(1)+CALG_NSUBLAY(2))              187
            IF (LAYER.LT.NINT(CALG_NSUBLAY(1)+CALG_NSUBLAY(2))) THEN      187
C       Create CSCI :                                                     188
C       Call To Local Procedure CSCI                                      188
            ASSIGN 5210 TO BLOCK_CSCI                                     188
            GO TO  5200                                                   188
5210        CONTINUE                                                      188
C       *                                                                 188
C       Geant POSITION : CSCI ===> X=CURRENT_DEPTH+CALG_SCINTTHK(SUPER)+  190
C       2.*CALG_ABPAPTHK Z=CURRENT_DEPTH/TAN_THETA/2 <===                 190
            CALL AGSRESET                                                 190
            AG_X = CURRENT_DEPTH+CALG_SCINTTHK(SUPER)+2.*CALG_ABPAPTHK    190
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              190
            AG_TITLE = 'CSCI'                                             190
            AG_EXNAME = 'POSITION'                                        190
            AG_PARLIST = 'X_Z'                                            190
            CALL AXPOSITION                                               190
C       *                                                                 190
C       Create CPBP :                                                     191
C       Call To Local Procedure CPBP                                      191
            ASSIGN 5230 TO BLOCK_CPBP                                     191
            GO TO  5220                                                   191
5230        CONTINUE                                                      191
C       *                                                                 191
            C_LEAD_DEP=2.*CALG_SCINTTHK(SUPER)+4.*CALG_ABPAPTHK           192
C       Geant POSITION : CPBP ===> X=CURRENT_DEPTH+C_LEAD_DEP+CALG_ABSOR  194
C       THK Z=CURRENT_DEPTH/TAN_THETA/2 <===                              194
            CALL AGSRESET                                                 194
            AG_X = CURRENT_DEPTH+C_LEAD_DEP+CALG_ABSORTHK                 194
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              194
            AG_TITLE = 'CPBP'                                             194
            AG_EXNAME = 'POSITION'                                        194
            AG_PARLIST = 'X_Z'                                            194
            CALL AXPOSITION                                               194
C       *                                                                 194
            CURRENT_DEPTH = CURRENT_DEPTH + 2*LAYER_WIDTH(SUPER)          194
            ELSE                                                          196
C       Create CSCI :                                                     197
C       Call To Local Procedure CSCI                                      197
            ASSIGN 5240 TO BLOCK_CSCI                                     197
            GO TO  5200                                                   197
5240        CONTINUE                                                      197
C       *                                                                 197
C       Geant POSITION : CSCI ===> X=CURRENT_DEPTH+CALG_SCINTTHK(2)+2.*C  199
C       ALG_ABPAPTHK Z=CURRENT_DEPTH/TAN_THETA/2 <===                     199
            CALL AGSRESET                                                 199
            AG_X = CURRENT_DEPTH+CALG_SCINTTHK(2)+2.*CALG_ABPAPTHK        199
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              199
            AG_TITLE = 'CSCI'                                             199
            AG_EXNAME = 'POSITION'                                        199
            AG_PARLIST = 'X_Z'                                            199
            CALL AXPOSITION                                               199
C       *                                                                 199
            CURRENT_DEPTH = CURRENT_DEPTH+C_LEAD_DEP                      199
            ENDIF                                                         201
C       Skip Unless LAYER==NINT(CALG_NSMD)                                202
            IF (LAYER.NE.NINT(CALG_NSMD))GO TO 5191                       202
C       Create CSMD :                                                     203
C       Call To Local Procedure CSMD                                      203
            ASSIGN 5260 TO BLOCK_CSMD                                     203
            GO TO  5250                                                   203
5260        CONTINUE                                                      203
C       *                                                                 203
C       Geant POSITION : CSMD ===> X=CURRENT_DEPTH+SMD_WIDTH Z=CURRENT_D  204
C       EPTH/TAN_THETA/2 <===                                             204
            CALL AGSRESET                                                 204
            AG_X = CURRENT_DEPTH+SMD_WIDTH                                204
            AG_Z = CURRENT_DEPTH/TAN_THETA/2                              204
            AG_TITLE = 'CSMD'                                             204
            AG_EXNAME = 'POSITION'                                        204
            AG_PARLIST = 'X_Z'                                            204
            CALL AXPOSITION                                               204
C       *                                                                 204
            CURRENT_DEPTH = CURRENT_DEPTH + 2*SMD_WIDTH                   204
         GO TO 5191                                                       206
5192     CONTINUE                                                         206
5141  CALL AGSPOPB                                                        207
      GO TO BLOCK_CSUP, (5140 ,5150)                                      207
C End of Local Procedure                                                  207
C ----------------------------------------------------------------------  208
C ------------ CPBP                                                       208
C Begin Local Procedure CPBP                                              209
C CPBP IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            209
5220  CONTINUE                                                            209
         AG_TITLE='CPBP'                                                  209
         AG_MODULE=MODULE                                                 209
         AG_MARK='V00'                                                    209
         CALL AGSPUSH                                                     209
         AG_MODULE=MODULE                                                 209
C    *                                                                    209
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             210
         CALL AGSRESET                                                    210
         AG_TITLE = 'LEAD'                                                210
         AG_EXNAME = 'MATERIAL'                                           210
         AG_PARLIST = 'NONE'                                              210
         CALL AXMATERIAL                                                  210
C    *                                                                    210
C    Geant MATERIAL : LEAD_CPBP ===> ISVOL=0 <===                         210
         CALL AGSRESET                                                    211
         AG_ISVOL = 0                                                     211
         AG_TITLE = 'LEAD_CPBP'                                           211
         AG_EXNAME = 'MATERIAL'                                           211
         AG_PARLIST = 'ISVOL'                                             211
         CALL AXMATERIAL                                                  211
C    *                                                                    211
C    Geant ATTRIBUTE : CPBP ===> SEEN=1 COLO=1 <===                       211
         CALL AGSRESET                                                    212
         AG_SEEN = 1                                                      212
         AG_COLO = 1                                                      212
         AG_TITLE = 'CPBP'                                                212
         AG_EXNAME = 'ATTRIBUTE'                                          212
         AG_PARLIST = 'SEEN_COLO'                                         212
         CALL AXATTRIBUTE                                                 212
C    *                                                                    212
C    Geant SHAPE : BOX ===> DX=CALG_ABSORTHK DY=CURRENT_DEPTH*TAN(TWOPI/  212
C    360*DPHIT)-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===           212
         CALL AGSRESET                                                    214
         AG_DX = CALG_ABSORTHK                                            214
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          214
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                214
         AG_TITLE = 'BOX'                                                 214
         AG_EXNAME = 'SHAPE'                                              214
         AG_PARLIST = 'DX_DY_DZ'                                          214
         CALL AXSHAPE                                                     214
C    *                                                                    214
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               214
5221  CALL AGSPOPB                                                        216
      GO TO BLOCK_CPBP, (5220 ,5230)                                      216
C End of Local Procedure                                                  216
C ----------------------------------------------------------------------  217
C ------------ CSCI                                                       217
C Begin Local Procedure CSCI                                              218
C CSCI A SCINTILLATOR LAYER.                                              218
5200  CONTINUE                                                            218
         AG_TITLE='CSCI'                                                  218
         AG_MODULE=MODULE                                                 218
         AG_MARK='V00'                                                    218
         CALL AGSPUSH                                                     218
         AG_MODULE=MODULE                                                 218
C    *                                                                    218
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       219
         CALL AGSRESET                                                    219
         AG_TITLE = 'POLYSTYREN'                                          219
         AG_EXNAME = 'MATERIAL'                                           219
         AG_PARLIST = 'NONE'                                              219
         CALL AXMATERIAL                                                  219
C    *                                                                    219
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       219
         CALL AGSRESET                                                    220
         AG_ISVOL = 1                                                     220
         AG_TITLE = 'CPOLYSTYREN'                                         220
         AG_EXNAME = 'MATERIAL'                                           220
         AG_PARLIST = 'ISVOL'                                             220
         CALL AXMATERIAL                                                  220
C    *                                                                    220
C    Geant ATTRIBUTE : CSCI ===> SEEN=1 COLO=4 <===                       220
         CALL AGSRESET                                                    221
         AG_SEEN = 1                                                      221
         AG_COLO = 4                                                      221
         AG_TITLE = 'CSCI'                                                221
         AG_EXNAME = 'ATTRIBUTE'                                          221
         AG_PARLIST = 'SEEN_COLO'                                         221
         CALL AXATTRIBUTE                                                 221
C    *                                                                    221
C    Geant SHAPE : BOX ===> DX=CALG_SCINTTHK(SUPER) DY=CURRENT_DEPTH*TAN  221
C    (TWOPI/360*DPHIT)-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===    221
         CALL AGSRESET                                                    223
         AG_DX = CALG_SCINTTHK(SUPER)                                     223
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          223
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                223
         AG_TITLE = 'BOX'                                                 223
         AG_EXNAME = 'SHAPE'                                              223
         AG_PARLIST = 'DX_DY_DZ'                                          223
         CALL AXSHAPE                                                     223
C    *                                                                    223
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               223
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                225
         CALL GSTPAR(AG_IMED, 'BIRK2', RKB2SC)                            226
         CALL GSTPAR(AG_IMED, 'BIRK3', RKB3SC)                            227
C    Defining Hits: ETA:DETA:(0,1) Y:1:(-13,+13) BIRK:0:(0,10)            228
         AG_TITLE = 'CSUP'                                                228
         AG_EXNAME = 'HITS'                                               228
         AG_TYPE=AG_MAGIC                                                 228
         CALL AGSDETV('CSUP')                                             228
C    *                                                                    228
         AG_PARLIST='ETA'                                                 228
         AG_TITLE = 'DETH'                                                228
         AG_NBITS = AG_MAGIC                                              228
         AG_BIN=DETA                                                      228
         AG_OPTION='*'                                                    228
         AG_FHMIN = 0                                                     228
         AG_FHMAX = 1                                                     228
         CALL AGSDETH (0)                                                 228
C    *                                                                    228
         AG_PARLIST='Y'                                                   228
         AG_TITLE = 'DETH'                                                228
         AG_NBITS = 1                                                     228
         AG_BIN=AG_MAGIC                                                  228
         AG_OPTION='*'                                                    228
         AG_FHMIN = -13                                                   228
         AG_FHMAX = +13                                                   228
         CALL AGSDETH (0)                                                 228
C    *                                                                    228
         AG_PARLIST='BIRK'                                                228
         AG_TITLE = 'DETH'                                                228
         AG_NBITS = 0                                                     228
         AG_BIN=AG_MAGIC                                                  228
         AG_OPTION='*'                                                    228
         AG_FHMIN = 0                                                     228
         AG_FHMAX = 10                                                    228
         CALL AGSDETH (0)                                                 228
C    *                                                                    228
         CALL AGSDETU('CSUP')                                             228
C    *                                                                    228
5201  CALL AGSPOPB                                                        229
      GO TO BLOCK_CSCI, (5200 ,5210,5240)                                 229
C End of Local Procedure                                                  229
C ----------------------------------------------------------------------  230
C ------------ CBTW                                                       230
C Begin Local Procedure CBTW                                              231
C CBTW IS THE MODULE FRONT BACK PLATE                                     231
5110  CONTINUE                                                            231
         AG_TITLE='CBTW'                                                  231
         AG_MODULE=MODULE                                                 231
         AG_MARK='V00'                                                    231
         CALL AGSPUSH                                                     231
         AG_MODULE=MODULE                                                 231
C    *                                                                    231
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        232
         CALL AGSRESET                                                    232
         AG_TITLE = 'ALUMINIUM'                                           232
         AG_EXNAME = 'MATERIAL'                                           232
         AG_PARLIST = 'NONE'                                              232
         CALL AXMATERIAL                                                  232
C    *                                                                    232
C    Geant MATERIAL : ALU_CBTW ===> ISVOL=1 <===                          232
         CALL AGSRESET                                                    233
         AG_ISVOL = 1                                                     233
         AG_TITLE = 'ALU_CBTW'                                            233
         AG_EXNAME = 'MATERIAL'                                           233
         AG_PARLIST = 'ISVOL'                                             233
         CALL AXMATERIAL                                                  233
C    *                                                                    233
C    Geant ATTRIBUTE : CBTW ===> SEEN=1 COLO=6 <===                       233
         CALL AGSRESET                                                    234
         AG_SEEN = 1                                                      234
         AG_COLO = 6                                                      234
         AG_TITLE = 'CBTW'                                                234
         AG_EXNAME = 'ATTRIBUTE'                                          234
         AG_PARLIST = 'SEEN_COLO'                                         234
         CALL AXATTRIBUTE                                                 234
C    *                                                                    234
C    Geant SHAPE : BOX ===> DY=CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_C  234
C    RACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===                            234
         CALL AGSRESET                                                    236
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          236
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                236
         AG_TITLE = 'BOX'                                                 236
         AG_EXNAME = 'SHAPE'                                              236
         AG_PARLIST = 'DY_DZ'                                             236
         CALL AXSHAPE                                                     236
C    *                                                                    236
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               236
5111  CALL AGSPOPB                                                        238
      GO TO BLOCK_CBTW, (5110 ,5120,5160,5170,5180)                       238
C End of Local Procedure                                                  238
C ----------------------------------------------------------------------  239
C ------------ CSMD                                                       239
C Begin Local Procedure CSMD                                              240
C CSMD IS THE SHOWER MAXIMUM DETECTOR ENVELOPE                            240
5250  CONTINUE                                                            240
         AG_TITLE='CSMD'                                                  240
         AG_MODULE=MODULE                                                 240
         AG_MARK='V00'                                                    240
         CALL AGSPUSH                                                     240
         AG_MODULE=MODULE                                                 240
C    *                                                                    240
         CURRENT = - SMD_WIDTH                                            241
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=6./21. <===                  242
         CALL AGSRESET                                                    242
         AG_A = 12.01                                                     242
         AG_Z = 6.                                                        242
         AG_W = 6./21.                                                    242
         AG_TITLE = 'C'                                                   242
         AG_EXNAME = 'COMPONENT'                                          242
         AG_PARLIST = 'A_Z_W'                                             242
         CALL AXCOMPONENT                                                 242
C    *                                                                    242
C    Geant COMPONENT : H ===> A=1. Z=1. W=10./21. <===                    242
         CALL AGSRESET                                                    243
         AG_A = 1.                                                        243
         AG_Z = 1.                                                        243
         AG_W = 10./21.                                                   243
         AG_TITLE = 'H'                                                   243
         AG_EXNAME = 'COMPONENT'                                          243
         AG_PARLIST = 'A_Z_W'                                             243
         CALL AXCOMPONENT                                                 243
C    *                                                                    243
C    Geant COMPONENT : O ===> A=16. Z=8. W=5./21. <===                    243
         CALL AGSRESET                                                    244
         AG_A = 16.                                                       244
         AG_Z = 8.                                                        244
         AG_W = 5./21.                                                    244
         AG_TITLE = 'O'                                                   244
         AG_EXNAME = 'COMPONENT'                                          244
         AG_PARLIST = 'A_Z_W'                                             244
         CALL AXCOMPONENT                                                 244
C    *                                                                    244
C    Geant MIXTURE : CELLULOSE ===> ISVOL=1 DENS=0.35 <===                244
         CALL AGSRESET                                                    245
         AG_ISVOL = 1                                                     245
         AG_DENS = 0.35                                                   245
         AG_TITLE = 'CELLULOSE'                                           245
         AG_EXNAME = 'MIXTURE'                                            245
         AG_PARLIST = 'ISVOL_DENS'                                        245
         CALL AXMIXTURE                                                   245
C    *                                                                    245
C    Geant ATTRIBUTE : CSMD ===> SEEN=1 COLO=6 <===                       245
         CALL AGSRESET                                                    246
         AG_SEEN = 1                                                      246
         AG_COLO = 6                                                      246
         AG_TITLE = 'CSMD'                                                246
         AG_EXNAME = 'ATTRIBUTE'                                          246
         AG_PARLIST = 'SEEN_COLO'                                         246
         CALL AXATTRIBUTE                                                 246
C    *                                                                    246
C    Geant SHAPE : BOX ===> DX=SMD_WIDTH DY=CURRENT_DEPTH*TAN(TWOPI/120.  246
C    )-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2 <===                    246
         CALL AGSRESET                                                    248
         AG_DX = SMD_WIDTH                                                248
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/120.)-CALG_CRACKWD               248
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2                                248
         AG_TITLE = 'BOX'                                                 248
         AG_EXNAME = 'SHAPE'                                              248
         AG_PARLIST = 'DX_DY_DZ'                                          248
         CALL AXSHAPE                                                     248
C    *                                                                    248
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              248
C    Create CSMG :                                                        250
C    Call To Local Procedure CSMG                                         250
         ASSIGN 5280 TO BLOCK_CSMG                                        250
         GO TO  5270                                                      250
5280     CONTINUE                                                         250
C    *                                                                    250
C    Geant POSITION : CSMG ===> X=-SMD_WIDTH+CALG_G10SBTHK+2.*CALG_ABPAP  251
C    THK <===                                                             251
         CALL AGSRESET                                                    251
         AG_X = -SMD_WIDTH+CALG_G10SBTHK+2.*CALG_ABPAPTHK                 251
         AG_TITLE = 'CSMG'                                                251
         AG_EXNAME = 'POSITION'                                           251
         AG_PARLIST = 'X'                                                 251
         CALL AXPOSITION                                                  251
C    *                                                                    251
C    Geant POSITION : CSMG ===> X=SMD_WIDTH-CALG_G10SBTHK-2.*CALG_ABPAPT  251
C    HK <===                                                              251
         CALL AGSRESET                                                    252
         AG_X = SMD_WIDTH-CALG_G10SBTHK-2.*CALG_ABPAPTHK                  252
         AG_TITLE = 'CSMG'                                                252
         AG_EXNAME = 'POSITION'                                           252
         AG_PARLIST = 'X'                                                 252
         CALL AXPOSITION                                                  252
C    *                                                                    252
         CURRENT = CURRENT + 2.*CALG_G10SBTHK + 2.*CALG_ABPAPTHK          252
C    *                                                                    254
         J=1                                                              254
            GO TO 5293                                                    254
5291        J=J+1                                                         254
5293        IF(J-(4).GT.0) GO TO 5292                                     254
            CURRENT_CSDA=-CURRENT_DEPTH/TAN_THETA/2                       255
C       Check J.EQ.1                                                      256
            IF (J.EQ.1) THEN                                              256
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA1WDH+CALG_SET12WDH)        257
C       Create CSDA :                                                     258
C       Call To Local Procedure CSDA                                      258
            ASSIGN 5310 TO BLOCK_CSDA                                     258
            GO TO  5300                                                   258
5310        CONTINUE                                                      258
C       *                                                                 258
C       Geant POSITION : CSDA ===> X=CURRENT+CALG_SMALFTHK Z=CURRENT_CSD  260
C       A+2.*CALG_SMETAWDH+ETA_LENGHT <===                                260
            CALL AGSRESET                                                 260
            AG_X = CURRENT+CALG_SMALFTHK                                  260
            AG_Z = CURRENT_CSDA+2.*CALG_SMETAWDH+ETA_LENGHT               260
            AG_TITLE = 'CSDA'                                             260
            AG_EXNAME = 'POSITION'                                        260
            AG_PARLIST = 'X_Z'                                            260
            CALL AXPOSITION                                               260
C       *                                                                 260
            ELSEIF (J.EQ.2) THEN                                          260
            CURRENT_CSDA=CURRENT_CSDA+2.*(CALG_SMETAWDH+ETA_LENGHT)       262
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA2WDH+CALG_SET12WDH)        263
C       Create CSDA :                                                     264
C       Call To Local Procedure CSDA                                      264
            ASSIGN 5320 TO BLOCK_CSDA                                     264
            GO TO  5300                                                   264
5320        CONTINUE                                                      264
C       *                                                                 264
C       Geant POSITION : CSDA ===> X=CURRENT+CALG_SMALFTHK Z=CURRENT_CSD  265
C       A+ETA_LENGHT <===                                                 265
            CALL AGSRESET                                                 265
            AG_X = CURRENT+CALG_SMALFTHK                                  265
            AG_Z = CURRENT_CSDA+ETA_LENGHT                                265
            AG_TITLE = 'CSDA'                                             265
            AG_EXNAME = 'POSITION'                                        265
            AG_PARLIST = 'X_Z'                                            265
            CALL AXPOSITION                                               265
C       *                                                                 265
            ELSEIF (J.EQ.3) THEN                                          265
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA1WDH+CALG_SET12WDH)        267
C       Create CSDA :                                                     268
C       Call To Local Procedure CSDA                                      268
            ASSIGN 5330 TO BLOCK_CSDA                                     268
            GO TO  5300                                                   268
5330        CONTINUE                                                      268
C       *                                                                 268
C       Geant POSITION : CSDA ===> X=CURRENT+3.*CALG_SMALFTHK Z=CURRENT_  270
C       CSDA+2.*CALG_SMETAWDH+ETA_LENGHT THETAX=90 PHIX=180 <===          270
            CALL AGSRESET                                                 270
            AG_X = CURRENT+3.*CALG_SMALFTHK                               270
            AG_Z = CURRENT_CSDA+2.*CALG_SMETAWDH+ETA_LENGHT               270
            AG_THETAX = 90                                                270
            AG_PHIX = 180                                                 270
            AG_TITLE = 'CSDA'                                             270
            AG_EXNAME = 'POSITION'                                        270
            AG_PARLIST = 'X_Z_THETAX_PHIX'                                270
            CALL AXPOSITION                                               270
C       *                                                                 270
            ELSEIF (J.EQ.4) THEN                                          270
            CURRENT_CSDA=CURRENT_CSDA+2.*(CALG_SMETAWDH+ETA_LENGHT)       272
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA2WDH+CALG_SET12WDH)        273
C       Create CSDA :                                                     274
C       Call To Local Procedure CSDA                                      274
            ASSIGN 5340 TO BLOCK_CSDA                                     274
            GO TO  5300                                                   274
5340        CONTINUE                                                      274
C       *                                                                 274
C       Geant POSITION : CSDA ===> X=CURRENT+3.*CALG_SMALFTHK Z=CURRENT_  276
C       CSDA+ETA_LENGHT THETAX=90 PHIX=180 <===                           276
            CALL AGSRESET                                                 276
            AG_X = CURRENT+3.*CALG_SMALFTHK                               276
            AG_Z = CURRENT_CSDA+ETA_LENGHT                                276
            AG_THETAX = 90                                                276
            AG_PHIX = 180                                                 276
            AG_TITLE = 'CSDA'                                             276
            AG_EXNAME = 'POSITION'                                        276
            AG_PARLIST = 'X_Z_THETAX_PHIX'                                276
            CALL AXPOSITION                                               276
C       *                                                                 276
            ENDIF                                                         276
         GO TO 5291                                                       278
5292     CONTINUE                                                         278
C    Create CSMC :                                                        279
C    Call To Local Procedure CSMC                                         279
         ASSIGN 5360 TO BLOCK_CSMC                                        279
         GO TO  5350                                                      279
5360     CONTINUE                                                         279
C    *                                                                    279
C    Geant POSITION : CSMC ===> X=CURRENT+CALG_SMALFTHK Y=CALG_SMALFWDH+  280
C    CALG_SMAFFWDH <===                                                   280
         CALL AGSRESET                                                    280
         AG_X = CURRENT+CALG_SMALFTHK                                     280
         AG_Y = CALG_SMALFWDH+CALG_SMAFFWDH                               280
         AG_TITLE = 'CSMC'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'X_Y'                                               280
         CALL AXPOSITION                                                  280
C    *                                                                    280
C    Geant POSITION : CSMC ===> X=CURRENT+CALG_SMALFTHK Y=-CALG_SMALFWDH  280
C    -CALG_SMAFFWDH <===                                                  280
         CALL AGSRESET                                                    281
         AG_X = CURRENT+CALG_SMALFTHK                                     281
         AG_Y = -CALG_SMALFWDH-CALG_SMAFFWDH                              281
         AG_TITLE = 'CSMC'                                                281
         AG_EXNAME = 'POSITION'                                           281
         AG_PARLIST = 'X_Y'                                               281
         CALL AXPOSITION                                                  281
C    *                                                                    281
C    Create CSMB :                                                        282
C    Call To Local Procedure CSMB                                         282
         ASSIGN 5380 TO BLOCK_CSMB                                        282
         GO TO  5370                                                      282
5380     CONTINUE                                                         282
C    *                                                                    282
C    Geant POSITION : CSMB ===> X=CURRENT+3.*CALG_SMALFTHK Y=CALG_SMALFW  283
C    DH+CALG_SMAFBWDH <===                                                283
         CALL AGSRESET                                                    283
         AG_X = CURRENT+3.*CALG_SMALFTHK                                  283
         AG_Y = CALG_SMALFWDH+CALG_SMAFBWDH                               283
         AG_TITLE = 'CSMB'                                                283
         AG_EXNAME = 'POSITION'                                           283
         AG_PARLIST = 'X_Y'                                               283
         CALL AXPOSITION                                                  283
C    *                                                                    283
C    Geant POSITION : CSMB ===> X=CURRENT+3.*CALG_SMALFTHK Y=-CALG_SMALF  283
C    WDH-CALG_SMAFBWDH <===                                               283
         CALL AGSRESET                                                    284
         AG_X = CURRENT+3.*CALG_SMALFTHK                                  284
         AG_Y = -CALG_SMALFWDH-CALG_SMAFBWDH                              284
         AG_TITLE = 'CSMB'                                                284
         AG_EXNAME = 'POSITION'                                           284
         AG_PARLIST = 'X_Y'                                               284
         CALL AXPOSITION                                                  284
C    *                                                                    284
5251  CALL AGSPOPB                                                        285
      GO TO BLOCK_CSMD, (5250 ,5260)                                      285
C End of Local Procedure                                                  285
C ----------------------------------------------------------------------  286
C ------------ CSMG                                                       286
C Begin Local Procedure CSMG                                              287
C CSMG IS G10 FRONT BACK PLATE                                            287
5270  CONTINUE                                                            287
         AG_TITLE='CSMG'                                                  287
         AG_MODULE=MODULE                                                 287
         AG_MARK='V00'                                                    287
         CALL AGSPUSH                                                     287
         AG_MODULE=MODULE                                                 287
C    *                                                                    287
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          288
         CALL AGSRESET                                                    288
         AG_A = 28.08                                                     288
         AG_Z = 14                                                        288
         AG_W = 0.6*1*28./60.                                             288
         AG_TITLE = 'SI'                                                  288
         AG_EXNAME = 'COMPONENT'                                          288
         AG_PARLIST = 'A_Z_W'                                             288
         CALL AXCOMPONENT                                                 288
C    *                                                                    288
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               288
         CALL AGSRESET                                                    289
         AG_A = 16                                                        289
         AG_Z = 8                                                         289
         AG_W = 0.6*2*16./60.                                             289
         AG_TITLE = 'O'                                                   289
         AG_EXNAME = 'COMPONENT'                                          289
         AG_PARLIST = 'A_Z_W'                                             289
         CALL AXCOMPONENT                                                 289
C    *                                                                    289
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              289
         CALL AGSRESET                                                    290
         AG_A = 12                                                        290
         AG_Z = 6                                                         290
         AG_W = 0.4*8*12./174.                                            290
         AG_TITLE = 'C'                                                   290
         AG_EXNAME = 'COMPONENT'                                          290
         AG_PARLIST = 'A_Z_W'                                             290
         CALL AXCOMPONENT                                                 290
C    *                                                                    290
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               290
         CALL AGSRESET                                                    291
         AG_A = 1                                                         291
         AG_Z = 1                                                         291
         AG_W = 0.4*14*1./174.                                            291
         AG_TITLE = 'H'                                                   291
         AG_EXNAME = 'COMPONENT'                                          291
         AG_PARLIST = 'A_Z_W'                                             291
         CALL AXCOMPONENT                                                 291
C    *                                                                    291
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              291
         CALL AGSRESET                                                    292
         AG_A = 16                                                        292
         AG_Z = 8                                                         292
         AG_W = 0.4*4*16./174.                                            292
         AG_TITLE = 'O'                                                   292
         AG_EXNAME = 'COMPONENT'                                          292
         AG_PARLIST = 'A_Z_W'                                             292
         CALL AXCOMPONENT                                                 292
C    *                                                                    292
C    Geant MIXTURE : G10 ===> ISVOL=0 DENS=1.7 <===                       292
         CALL AGSRESET                                                    293
         AG_ISVOL = 0                                                     293
         AG_DENS = 1.7                                                    293
         AG_TITLE = 'G10'                                                 293
         AG_EXNAME = 'MIXTURE'                                            293
         AG_PARLIST = 'ISVOL_DENS'                                        293
         CALL AXMIXTURE                                                   293
C    *                                                                    293
C    Geant ATTRIBUTE : CSMG ===> SEEN=1 COLO=1 <===                       293
         CALL AGSRESET                                                    294
         AG_SEEN = 1                                                      294
         AG_COLO = 1                                                      294
         AG_TITLE = 'CSMG'                                                294
         AG_EXNAME = 'ATTRIBUTE'                                          294
         AG_PARLIST = 'SEEN_COLO'                                         294
         CALL AXATTRIBUTE                                                 294
C    *                                                                    294
C    Geant SHAPE : BOX ===> DX=CALG_G10SBTHK <===                         294
         CALL AGSRESET                                                    295
         AG_DX = CALG_G10SBTHK                                            295
         AG_TITLE = 'BOX'                                                 295
         AG_EXNAME = 'SHAPE'                                              295
         AG_PARLIST = 'DX'                                                295
         CALL AXSHAPE                                                     295
C    *                                                                    295
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              295
5271  CALL AGSPOPB                                                        297
      GO TO BLOCK_CSMG, (5270 ,5280)                                      297
C End of Local Procedure                                                  297
C ----------------------------------------------------------------------  298
C ------------ CSDA                                                       298
C Begin Local Procedure CSDA                                              299
C CSDA IS AL BLOCK WITH SENSITIVE GAS VOLUME                              299
5300  CONTINUE                                                            299
         AG_TITLE='CSDA'                                                  299
         AG_MODULE=MODULE                                                 299
         AG_MARK='V00'                                                    299
         CALL AGSPUSH                                                     299
         AG_MODULE=MODULE                                                 299
C    *                                                                    299
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        300
         CALL AGSRESET                                                    300
         AG_TITLE = 'ALUMINIUM'                                           300
         AG_EXNAME = 'MATERIAL'                                           300
         AG_PARLIST = 'NONE'                                              300
         CALL AXMATERIAL                                                  300
C    *                                                                    300
C    Geant MATERIAL : ALU_CSDA ===> ISVOL=0 <===                          300
         CALL AGSRESET                                                    301
         AG_ISVOL = 0                                                     301
         AG_TITLE = 'ALU_CSDA'                                            301
         AG_EXNAME = 'MATERIAL'                                           301
         AG_PARLIST = 'ISVOL'                                             301
         CALL AXMATERIAL                                                  301
C    *                                                                    301
C    Geant ATTRIBUTE : CSDA ===> SEEN=1 COLO=6 SERIAL=J <===              301
         CALL AGSRESET                                                    302
         AG_SEEN = 1                                                      302
         AG_COLO = 6                                                      302
         AG_SERIAL = J                                                    302
         AG_TITLE = 'CSDA'                                                302
         AG_EXNAME = 'ATTRIBUTE'                                          302
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  302
         CALL AXATTRIBUTE                                                 302
C    *                                                                    302
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMALFWDH, DZ=ETA_LE  302
C    NGHT <===                                                            302
         CALL AGSRESET                                                    304
         AG_DX = CALG_SMALFTHK                                            304
         AG_DY = CALG_SMALFWDH                                            304
         AG_DZ = ETA_LENGHT                                               304
         AG_TITLE = 'BOX'                                                 304
         AG_EXNAME = 'SHAPE'                                              304
         AG_PARLIST = 'DX_DY_DZ'                                          304
         CALL AXSHAPE                                                     304
C    *                                                                    304
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              304
C    Create CSME :                                                        306
C    Call To Local Procedure CSME                                         306
         ASSIGN 5400 TO BLOCK_CSME                                        306
         GO TO  5390                                                      306
5400     CONTINUE                                                         306
C    *                                                                    306
5301  CALL AGSPOPB                                                        307
      GO TO BLOCK_CSDA, (5300 ,5310,5320,5330,5340)                       307
C End of Local Procedure                                                  307
C ----------------------------------------------------------------------  308
C ------------ CSMC                                                       308
C Begin Local Procedure CSMC                                              309
C CSMC IS THE FRONT FIRST (LAST) AL RIB                                   309
5350  CONTINUE                                                            309
         AG_TITLE='CSMC'                                                  309
         AG_MODULE=MODULE                                                 309
         AG_MARK='V00'                                                    309
         CALL AGSPUSH                                                     309
         AG_MODULE=MODULE                                                 309
C    *                                                                    309
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        310
         CALL AGSRESET                                                    310
         AG_TITLE = 'ALUMINIUM'                                           310
         AG_EXNAME = 'MATERIAL'                                           310
         AG_PARLIST = 'NONE'                                              310
         CALL AXMATERIAL                                                  310
C    *                                                                    310
C    Geant MATERIAL : ALU_CSMC ===> ISVOL=0 <===                          310
         CALL AGSRESET                                                    311
         AG_ISVOL = 0                                                     311
         AG_TITLE = 'ALU_CSMC'                                            311
         AG_EXNAME = 'MATERIAL'                                           311
         AG_PARLIST = 'ISVOL'                                             311
         CALL AXMATERIAL                                                  311
C    *                                                                    311
C    Geant ATTRIBUTE : CSMC ===> SEEN=1 COLO=6 <===                       311
         CALL AGSRESET                                                    312
         AG_SEEN = 1                                                      312
         AG_COLO = 6                                                      312
         AG_TITLE = 'CSMC'                                                312
         AG_EXNAME = 'ATTRIBUTE'                                          312
         AG_PARLIST = 'SEEN_COLO'                                         312
         CALL AXATTRIBUTE                                                 312
C    *                                                                    312
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMAFFWDH <===        312
         CALL AGSRESET                                                    313
         AG_DX = CALG_SMALFTHK                                            313
         AG_DY = CALG_SMAFFWDH                                            313
         AG_TITLE = 'BOX'                                                 313
         AG_EXNAME = 'SHAPE'                                              313
         AG_PARLIST = 'DX_DY'                                             313
         CALL AXSHAPE                                                     313
C    *                                                                    313
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              313
5351  CALL AGSPOPB                                                        315
      GO TO BLOCK_CSMC, (5350 ,5360)                                      315
C End of Local Procedure                                                  315
C ----------------------------------------------------------------------  316
C ------------ CSMB                                                       316
C Begin Local Procedure CSMB                                              317
C CSMB IS THE BACK FIRST (LAST) AL RIB                                    317
5370  CONTINUE                                                            317
         AG_TITLE='CSMB'                                                  317
         AG_MODULE=MODULE                                                 317
         AG_MARK='V00'                                                    317
         CALL AGSPUSH                                                     317
         AG_MODULE=MODULE                                                 317
C    *                                                                    317
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        318
         CALL AGSRESET                                                    318
         AG_TITLE = 'ALUMINIUM'                                           318
         AG_EXNAME = 'MATERIAL'                                           318
         AG_PARLIST = 'NONE'                                              318
         CALL AXMATERIAL                                                  318
C    *                                                                    318
C    Geant MATERIAL : ALU_CSMB ===> ISVOL=0 <===                          318
         CALL AGSRESET                                                    319
         AG_ISVOL = 0                                                     319
         AG_TITLE = 'ALU_CSMB'                                            319
         AG_EXNAME = 'MATERIAL'                                           319
         AG_PARLIST = 'ISVOL'                                             319
         CALL AXMATERIAL                                                  319
C    *                                                                    319
C    Geant ATTRIBUTE : CSMB ===> SEEN=1 COLO=6 <===                       319
         CALL AGSRESET                                                    320
         AG_SEEN = 1                                                      320
         AG_COLO = 6                                                      320
         AG_TITLE = 'CSMB'                                                320
         AG_EXNAME = 'ATTRIBUTE'                                          320
         AG_PARLIST = 'SEEN_COLO'                                         320
         CALL AXATTRIBUTE                                                 320
C    *                                                                    320
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMAFBWDH <===        320
         CALL AGSRESET                                                    321
         AG_DX = CALG_SMALFTHK                                            321
         AG_DY = CALG_SMAFBWDH                                            321
         AG_TITLE = 'BOX'                                                 321
         AG_EXNAME = 'SHAPE'                                              321
         AG_PARLIST = 'DX_DY'                                             321
         CALL AXSHAPE                                                     321
C    *                                                                    321
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              321
5371  CALL AGSPOPB                                                        323
      GO TO BLOCK_CSMB, (5370 ,5380)                                      323
C End of Local Procedure                                                  323
C ----------------------------------------------------------------------  324
C ------------ CSME                                                       324
C Begin Local Procedure CSME                                              325
C CSME IS THE PART OF CSDA AL BOX WITH AR/CO2 SENSITEVE GAS               325
5390  CONTINUE                                                            325
         AG_TITLE='CSME'                                                  325
         AG_MODULE=MODULE                                                 325
         AG_MARK='V00'                                                    325
         CALL AGSPUSH                                                     325
         AG_MODULE=MODULE                                                 325
C    *                                                                    325
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        326
         CALL AGSRESET                                                    326
         AG_TITLE = 'ALUMINIUM'                                           326
         AG_EXNAME = 'MATERIAL'                                           326
         AG_PARLIST = 'NONE'                                              326
         CALL AXMATERIAL                                                  326
C    *                                                                    326
C    Geant MATERIAL : ALU_CSME ===> ISVOL=0 <===                          326
         CALL AGSRESET                                                    327
         AG_ISVOL = 0                                                     327
         AG_TITLE = 'ALU_CSME'                                            327
         AG_EXNAME = 'MATERIAL'                                           327
         AG_PARLIST = 'ISVOL'                                             327
         CALL AXMATERIAL                                                  327
C    *                                                                    327
C    Geant ATTRIBUTE : CSME ===> SEEN=1 COLO=6 <===                       327
         CALL AGSRESET                                                    328
         AG_SEEN = 1                                                      328
         AG_COLO = 6                                                      328
         AG_TITLE = 'CSME'                                                328
         AG_EXNAME = 'ATTRIBUTE'                                          328
         AG_PARLIST = 'SEEN_COLO'                                         328
         CALL AXATTRIBUTE                                                 328
C    *                                                                    328
C    Geant SHAPE : DIVISION ===> NDIV=NINT(CALG_NSMDALW) IAXIS=2 <===     328
         CALL AGSRESET                                                    329
         AG_NDIV = NINT(CALG_NSMDALW)                                     329
         AG_IAXIS = 2                                                     329
         AG_TITLE = 'DIVISION'                                            329
         AG_EXNAME = 'SHAPE'                                              329
         AG_PARLIST = 'NDIV_IAXIS'                                        329
         CALL AXSHAPE                                                     329
C    *                                                                    329
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              329
C    *                                                                    331
         I=1                                                              331
            GO TO 5413                                                    331
5411        I=I+1                                                         331
5413        IF(I-(2).GT.0) GO TO 5412                                     331
C       Create CSHI :                                                     332
C       Call To Local Procedure CSHI                                      332
            ASSIGN 5430 TO BLOCK_CSHI                                     332
            GO TO  5420                                                   332
5430        CONTINUE                                                      332
C       *                                                                 332
C       Check I.EQ.1                                                      333
            IF (I.EQ.1) THEN                                              333
C       Geant POSITION : CSHI ===> X=-CALG_SMALFTHK+CALG_SMGASTHK <===    334
            CALL AGSRESET                                                 334
            AG_X = -CALG_SMALFTHK+CALG_SMGASTHK                           334
            AG_TITLE = 'CSHI'                                             334
            AG_EXNAME = 'POSITION'                                        334
            AG_PARLIST = 'X'                                              334
            CALL AXPOSITION                                               334
C       *                                                                 334
            ELSE                                                          334
C       Geant POSITION : CSHI ===> X=2.*CALG_SMGASTHK-CALG_SMALFTHK <===  336
            CALL AGSRESET                                                 336
            AG_X = 2.*CALG_SMGASTHK-CALG_SMALFTHK                         336
            AG_TITLE = 'CSHI'                                             336
            AG_EXNAME = 'POSITION'                                        336
            AG_PARLIST = 'X'                                              336
            CALL AXPOSITION                                               336
C       *                                                                 336
            ENDIF                                                         336
         GO TO 5411                                                       338
5412     CONTINUE                                                         338
5391  CALL AGSPOPB                                                        339
      GO TO BLOCK_CSME, (5390 ,5400)                                      339
C End of Local Procedure                                                  339
C ----------------------------------------------------------------------  340
C ------------ CSHI                                                       340
C Begin Local Procedure CSHI                                              341
C CSHI IS A SENSITEVE AR/CO2 BOX                                          341
5420  CONTINUE                                                            341
         AG_TITLE='CSHI'                                                  341
         AG_MODULE=MODULE                                                 341
         AG_MARK='V00'                                                    341
         CALL AGSPUSH                                                     341
         AG_MODULE=MODULE                                                 341
C    *                                                                    341
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   342
         CALL AGSRESET                                                    342
         AG_A = 39.95                                                     342
         AG_Z = 18.                                                       342
         AG_W = 0.9                                                       342
         AG_TITLE = 'AR'                                                  342
         AG_EXNAME = 'COMPONENT'                                          342
         AG_PARLIST = 'A_Z_W'                                             342
         CALL AXCOMPONENT                                                 342
C    *                                                                    342
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       342
         CALL AGSRESET                                                    343
         AG_A = 12.01                                                     343
         AG_Z = 6.                                                        343
         AG_W = 0.1*1*12.01/44.01                                         343
         AG_TITLE = 'C'                                                   343
         AG_EXNAME = 'COMPONENT'                                          343
         AG_PARLIST = 'A_Z_W'                                             343
         CALL AXCOMPONENT                                                 343
C    *                                                                    343
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           343
         CALL AGSRESET                                                    344
         AG_A = 16.                                                       344
         AG_Z = 8.                                                        344
         AG_W = 0.1*2*16./44.01                                           344
         AG_TITLE = 'O'                                                   344
         AG_EXNAME = 'COMPONENT'                                          344
         AG_PARLIST = 'A_Z_W'                                             344
         CALL AXCOMPONENT                                                 344
C    *                                                                    344
C    Geant MIXTURE : SENS_GAS ===> ISVOL=1 DENS=0.0018015 <===            344
         CALL AGSRESET                                                    345
         AG_ISVOL = 1                                                     345
         AG_DENS = 0.0018015                                              345
         AG_TITLE = 'SENS_GAS'                                            345
         AG_EXNAME = 'MIXTURE'                                            345
         AG_PARLIST = 'ISVOL_DENS'                                        345
         CALL AXMIXTURE                                                   345
C    *                                                                    345
C    Geant ATTRIBUTE : CSHI ===> SEEN=1 COLO=4 <===                       345
         CALL AGSRESET                                                    346
         AG_SEEN = 1                                                      346
         AG_COLO = 4                                                      346
         AG_TITLE = 'CSHI'                                                346
         AG_EXNAME = 'ATTRIBUTE'                                          346
         AG_PARLIST = 'SEEN_COLO'                                         346
         CALL AXATTRIBUTE                                                 346
C    *                                                                    346
C    Check I.EQ.1                                                         346
         IF (I.EQ.1) THEN                                                 347
C    Geant SHAPE : BOX ===> DX=CALG_SMGASTHK DY=CALG_SMGASWDH <===        348
         CALL AGSRESET                                                    348
         AG_DX = CALG_SMGASTHK                                            348
         AG_DY = CALG_SMGASWDH                                            348
         AG_TITLE = 'BOX'                                                 348
         AG_EXNAME = 'SHAPE'                                              348
         AG_PARLIST = 'DX_DY'                                             348
         CALL AXSHAPE                                                     348
C    *                                                                    348
         ELSE                                                             348
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=CALG_SMGASRAD PHI1=270, PHI2=45  351
C    0 <===                                                               351
         CALL AGSRESET                                                    351
         AG_RMIN = 0                                                      351
         AG_RMAX = CALG_SMGASRAD                                          351
         AG_PHI1 = 270                                                    351
         AG_PHI2 = 450                                                    351
         AG_TITLE = 'TUBS'                                                351
         AG_EXNAME = 'SHAPE'                                              351
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2'                               351
         CALL AXSHAPE                                                     351
C    *                                                                    351
         ENDIF                                                            351
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              353
         CALL GSTPAR(AG_IMED, 'STRA', 1.0)                                354
         H_ETA1=2.*(CALG_SETA1WDH + CALG_SET12WDH)                        355
         SH_ETA1=CALG_NETFIRST*H_ETA1                                     356
         H_ETA2=2.*(CALG_SETA2WDH + CALG_SET12WDH)                        357
         SH_ETA2=CALG_NETSECON*H_ETA2                                     358
         H_PHI1=2.*(CALG_SPHIWDH + CALG_SPHIDWDH)                         359
         H_PHI2=H_PHI1                                                    360
         SH_PHI1=CALG_NPHISTR*H_PHI1                                      361
         SH_PHI2=SH_PHI1                                                  362
C    Defining Hits: TYPE=1:2: ETA:0.1:(0,1) ETSP:H_ETA1:(0,SH_ETA1) ELOS  363
C    S:0:(0,1)                                                            363
         AG_TITLE = 'CSDA'                                                363
         AG_EXNAME = 'HITS'                                               363
         AG_TYPE=1                                                        363
         CALL AGSDETV('CSDA')                                             363
C    *                                                                    363
         AG_PARLIST='TYPE'                                                363
         AG_TITLE = 'DETH'                                                363
         AG_NBITS = 2                                                     363
         AG_BIN=AG_MAGIC                                                  363
         AG_FHMIN=AG_MAGIC                                                363
         AG_FHMAX=AG_MAGIC                                                363
         AG_OPTION='*'                                                    363
         CALL AGSDETH (0)                                                 363
C    *                                                                    363
         AG_PARLIST='ETA'                                                 363
         AG_TITLE = 'DETH'                                                363
         AG_NBITS = AG_MAGIC                                              363
         AG_BIN=0.1                                                       363
         AG_OPTION='*'                                                    363
         AG_FHMIN = 0                                                     363
         AG_FHMAX = 1                                                     363
         CALL AGSDETH (0)                                                 363
C    *                                                                    363
         AG_PARLIST='ETSP'                                                363
         AG_TITLE = 'DETH'                                                363
         AG_NBITS = AG_MAGIC                                              363
         AG_BIN=H_ETA1                                                    363
         AG_OPTION='*'                                                    363
         AG_FHMIN = 0                                                     363
         AG_FHMAX = SH_ETA1                                               363
         CALL AGSDETH (0)                                                 363
C    *                                                                    363
         AG_PARLIST='ELOSS'                                               363
         AG_TITLE = 'DETH'                                                363
         AG_NBITS = 0                                                     363
         AG_BIN=AG_MAGIC                                                  363
         AG_OPTION='*'                                                    363
         AG_FHMIN = 0                                                     363
         AG_FHMAX = 1                                                     363
         CALL AGSDETH (0)                                                 363
C    *                                                                    363
         CALL AGSDETU('CSDA')                                             363
C    *                                                                    363
C    Defining Hits: TYPE=2:2: ETA:0.1:(0,1) ETSP:H_ETA2:(0,SH_ETA2) ELOS  364
C    S:0:(0,1)                                                            364
         AG_TITLE = 'CSDA'                                                364
         AG_EXNAME = 'HITS'                                               364
         AG_TYPE=2                                                        364
         CALL AGSDETV('CSDA')                                             364
C    *                                                                    364
         AG_PARLIST='TYPE'                                                364
         AG_TITLE = 'DETH'                                                364
         AG_NBITS = 2                                                     364
         AG_BIN=AG_MAGIC                                                  364
         AG_FHMIN=AG_MAGIC                                                364
         AG_FHMAX=AG_MAGIC                                                364
         AG_OPTION='*'                                                    364
         CALL AGSDETH (0)                                                 364
C    *                                                                    364
         AG_PARLIST='ETA'                                                 364
         AG_TITLE = 'DETH'                                                364
         AG_NBITS = AG_MAGIC                                              364
         AG_BIN=0.1                                                       364
         AG_OPTION='*'                                                    364
         AG_FHMIN = 0                                                     364
         AG_FHMAX = 1                                                     364
         CALL AGSDETH (0)                                                 364
C    *                                                                    364
         AG_PARLIST='ETSP'                                                364
         AG_TITLE = 'DETH'                                                364
         AG_NBITS = AG_MAGIC                                              364
         AG_BIN=H_ETA2                                                    364
         AG_OPTION='*'                                                    364
         AG_FHMIN = 0                                                     364
         AG_FHMAX = SH_ETA2                                               364
         CALL AGSDETH (0)                                                 364
C    *                                                                    364
         AG_PARLIST='ELOSS'                                               364
         AG_TITLE = 'DETH'                                                364
         AG_NBITS = 0                                                     364
         AG_BIN=AG_MAGIC                                                  364
         AG_OPTION='*'                                                    364
         AG_FHMIN = 0                                                     364
         AG_FHMAX = 1                                                     364
         CALL AGSDETH (0)                                                 364
C    *                                                                    364
         CALL AGSDETU('CSDA')                                             364
C    *                                                                    364
C    Defining Hits: TYPE=3:2: ETA:0.1:(0,1) ETSP:H_PHI1:(0,SH_PHI1) ELOS  365
C    S:0:(0,1)                                                            365
         AG_TITLE = 'CSDA'                                                365
         AG_EXNAME = 'HITS'                                               365
         AG_TYPE=3                                                        365
         CALL AGSDETV('CSDA')                                             365
C    *                                                                    365
         AG_PARLIST='TYPE'                                                365
         AG_TITLE = 'DETH'                                                365
         AG_NBITS = 2                                                     365
         AG_BIN=AG_MAGIC                                                  365
         AG_FHMIN=AG_MAGIC                                                365
         AG_FHMAX=AG_MAGIC                                                365
         AG_OPTION='*'                                                    365
         CALL AGSDETH (0)                                                 365
C    *                                                                    365
         AG_PARLIST='ETA'                                                 365
         AG_TITLE = 'DETH'                                                365
         AG_NBITS = AG_MAGIC                                              365
         AG_BIN=0.1                                                       365
         AG_OPTION='*'                                                    365
         AG_FHMIN = 0                                                     365
         AG_FHMAX = 1                                                     365
         CALL AGSDETH (0)                                                 365
C    *                                                                    365
         AG_PARLIST='ETSP'                                                365
         AG_TITLE = 'DETH'                                                365
         AG_NBITS = AG_MAGIC                                              365
         AG_BIN=H_PHI1                                                    365
         AG_OPTION='*'                                                    365
         AG_FHMIN = 0                                                     365
         AG_FHMAX = SH_PHI1                                               365
         CALL AGSDETH (0)                                                 365
C    *                                                                    365
         AG_PARLIST='ELOSS'                                               365
         AG_TITLE = 'DETH'                                                365
         AG_NBITS = 0                                                     365
         AG_BIN=AG_MAGIC                                                  365
         AG_OPTION='*'                                                    365
         AG_FHMIN = 0                                                     365
         AG_FHMAX = 1                                                     365
         CALL AGSDETH (0)                                                 365
C    *                                                                    365
         CALL AGSDETU('CSDA')                                             365
C    *                                                                    365
C    Defining Hits: TYPE=4:2: ETA:0.1:(0,1) ETSP:H_PHI2:(0,SH_PHI2) ELOS  366
C    S:0:(0,1)                                                            366
         AG_TITLE = 'CSDA'                                                366
         AG_EXNAME = 'HITS'                                               366
         AG_TYPE=4                                                        366
         CALL AGSDETV('CSDA')                                             366
C    *                                                                    366
         AG_PARLIST='TYPE'                                                366
         AG_TITLE = 'DETH'                                                366
         AG_NBITS = 2                                                     366
         AG_BIN=AG_MAGIC                                                  366
         AG_FHMIN=AG_MAGIC                                                366
         AG_FHMAX=AG_MAGIC                                                366
         AG_OPTION='*'                                                    366
         CALL AGSDETH (0)                                                 366
C    *                                                                    366
         AG_PARLIST='ETA'                                                 366
         AG_TITLE = 'DETH'                                                366
         AG_NBITS = AG_MAGIC                                              366
         AG_BIN=0.1                                                       366
         AG_OPTION='*'                                                    366
         AG_FHMIN = 0                                                     366
         AG_FHMAX = 1                                                     366
         CALL AGSDETH (0)                                                 366
C    *                                                                    366
         AG_PARLIST='ETSP'                                                366
         AG_TITLE = 'DETH'                                                366
         AG_NBITS = AG_MAGIC                                              366
         AG_BIN=H_PHI2                                                    366
         AG_OPTION='*'                                                    366
         AG_FHMIN = 0                                                     366
         AG_FHMAX = SH_PHI2                                               366
         CALL AGSDETH (0)                                                 366
C    *                                                                    366
         AG_PARLIST='ELOSS'                                               366
         AG_TITLE = 'DETH'                                                366
         AG_NBITS = 0                                                     366
         AG_BIN=AG_MAGIC                                                  366
         AG_OPTION='*'                                                    366
         AG_FHMIN = 0                                                     366
         AG_FHMAX = 1                                                     366
         CALL AGSDETH (0)                                                 366
C    *                                                                    366
         CALL AGSDETU('CSDA')                                             366
C    *                                                                    366
5421  CALL AGSPOPB                                                        367
      GO TO BLOCK_CSHI, (5420 ,5430)                                      367
C End of Local Procedure                                                  367
      WRITE(LOUT,5440)'===>>  [End AgML Module CALBGEO1]  <<==='          367
5440  FORMAT(A32,/,/)                                                     367
C MODULE CALBGEO1                                                         369
1     CONTINUE                                                            369
      END                                                                 369
