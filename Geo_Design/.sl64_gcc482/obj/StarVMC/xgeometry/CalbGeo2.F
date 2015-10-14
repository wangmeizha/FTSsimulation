C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE CALBGEO2                                                 19
C CALBGEO2 IS THE GEOMETRY OF THE BARREL EM CALORIMETER                   19
      IMPLICIT NONE                                                       19
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       19
      DATA MODULE/'CALBGEO2'/, BANK_TITLE/'*'/                            19
      DATA MCOMMENT/'IS THE GEOMETRY OF THE BARREL EM CALORIMETER'/       19
      LOGICAL FIRST                                                       19
      DATA FIRST/.TRUE./                                                  19
      SAVE FIRST                                                          19
      INTEGER JUMPAD                                                      19
      EXTERNAL JUMPAD                                                     19
      INTEGER OK                                                          19
      PARAMETER (OK=0)                                                    19
      INTEGER AG_I,AG_J                                                   19
      SAVE AG_I,AG_J                                                      19
C AUTHOR MAXIM POTEKHIN BNL                                               19
      DATA AG_I/0/,AG_J/0/                                                20
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN BNL'/                          20
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  20
C CREATED DECEMBER 12, 2006                                               20
      CHARACTER*40 CREATED /'DECEMBER 12, 2006'/                          21
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  21
C - Advanced Geant interface                                              22
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      22
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       22
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         22
     *AG_OPTION,AG_ORT,AG_MARK                                            22
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         22
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     22
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     22
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    22
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          22
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   22
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     22
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        22
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   22
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 22
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     22
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       22
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        22
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       22
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       22
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       22
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         22
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     22
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       22
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   22
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       22
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     22
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          22
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         22
     *-696969,AG_LDETU=250)                                               22
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    22
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  22
     *AG_IMFLD,AG_ISIMU                                                   22
C Inherited variables a: saved during internal calls                      22
C b: reset at the entry in a block and then saved                         22
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        22
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       22
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         22
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     22
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       22
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         22
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           22
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        22
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        22
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            22
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           22
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    22
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      22
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           22
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          22
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                22
C local variables valid inside same block                                 22
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       22
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  22
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         22
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    22
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          22
     *AG_OPTION                                                           22
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        22
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             22
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              22
C temporarly                                                              22
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           22
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    22
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       22
     *AG_HY)                                                              22
      REAL MIL_P,INCH_P                                                   22
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               22
C - Natural constants (or considered as such)                             22
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   23
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        23
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       23
C - GEANT description of logical units                                    23
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            24
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      24
      COMMON/GCMAIL/ CHMAIL                                               24
      CHARACTER*132 CHMAIL                                                24
      EXTERNAL ETSPHIT                                                    24
      INTEGER BLOCK_CALB                                                  27
      INTEGER BLOCK_CHLV                                                  27
      INTEGER BLOCK_CPHI                                                  27
      INTEGER BLOCK_CSZO                                                  27
      INTEGER BLOCK_CSZU                                                  27
      INTEGER BLOCK_CSUP                                                  27
      INTEGER BLOCK_CPBP                                                  27
      INTEGER BLOCK_CSCI                                                  27
      INTEGER BLOCK_CSMD                                                  27
      INTEGER BLOCK_CSMG                                                  27
      INTEGER BLOCK_CSDA                                                  27
      INTEGER BLOCK_CSMC                                                  27
      INTEGER BLOCK_CSMB                                                  27
      INTEGER BLOCK_CSME                                                  27
      INTEGER BLOCK_CSHI                                                  27
      INTEGER BLOCK_CBTW                                                  27
      INTEGER BLOCK_CSPT                                                  27
      INTEGER BLOCK_CSPB                                                  27
      INTEGER BLOCK_CSTP                                                  27
      INTEGER BLOCK_CSLG                                                  27
C -                                                                       32
      REAL CALG_VERSION                                                   32
      EQUIVALENCE (BANK_CALG(3),CALG_VERSION,RE_CALG_VERSION),            32
     *(NAMES_CALG(1),NCALG_VERSION)                                       32
      EQUIVALENCE (COMMEN_CALG(1),CCALG_VERSION),(MAP_CALG(1,1),          32
     *MCALG_VERSION)                                                      32
      CHARACTER CCALG_VERSION*40/' '/                                     32
      CHARACTER NCALG_VERSION*12/'R.VERSION'/                             32
      INTEGER MCALG_VERSION(2)/1,1/                                       32
      REAL RE_CALG_VERSION                                                32
C -                                                                       32
      REAL CALG_RMIN                                                      32
      EQUIVALENCE (BANK_CALG(4),CALG_RMIN,RE_CALG_RMIN),(NAMES_CALG(2),   32
     *NCALG_RMIN)                                                         32
      EQUIVALENCE (COMMEN_CALG(2),CCALG_RMIN),(MAP_CALG(1,2),             32
     *MCALG_RMIN)                                                         32
      CHARACTER CCALG_RMIN*40/' '/                                        32
      CHARACTER NCALG_RMIN*12/'R.RMIN'/                                   32
      INTEGER MCALG_RMIN(2)/1,1/                                          32
      REAL RE_CALG_RMIN                                                   32
C -                                                                       32
      REAL CALG_ETACUT                                                    32
      EQUIVALENCE (BANK_CALG(5),CALG_ETACUT,RE_CALG_ETACUT),              32
     *(NAMES_CALG(3),NCALG_ETACUT)                                        32
      EQUIVALENCE (COMMEN_CALG(3),CCALG_ETACUT),(MAP_CALG(1,3),           32
     *MCALG_ETACUT)                                                       32
      CHARACTER CCALG_ETACUT*40/' '/                                      32
      CHARACTER NCALG_ETACUT*12/'R.ETACUT'/                               32
      INTEGER MCALG_ETACUT(2)/1,1/                                        32
      REAL RE_CALG_ETACUT                                                 32
C -                                                                       32
      REAL CALG_CRACKWD                                                   32
      EQUIVALENCE (BANK_CALG(6),CALG_CRACKWD,RE_CALG_CRACKWD),            32
     *(NAMES_CALG(4),NCALG_CRACKWD)                                       32
      EQUIVALENCE (COMMEN_CALG(4),CCALG_CRACKWD),(MAP_CALG(1,4),          32
     *MCALG_CRACKWD)                                                      32
      CHARACTER CCALG_CRACKWD*40/' '/                                     32
      CHARACTER NCALG_CRACKWD*12/'R.CRACKWD'/                             32
      INTEGER MCALG_CRACKWD(2)/1,1/                                       32
      REAL RE_CALG_CRACKWD                                                32
C -                                                                       32
      REAL CALG_FRONTTHK                                                  32
      EQUIVALENCE (BANK_CALG(7),CALG_FRONTTHK,RE_CALG_FRONTTHK),          32
     *(NAMES_CALG(5),NCALG_FRONTTHK)                                      32
      EQUIVALENCE (COMMEN_CALG(5),CCALG_FRONTTHK),(MAP_CALG(1,5),         32
     *MCALG_FRONTTHK)                                                     32
      CHARACTER CCALG_FRONTTHK*40/' '/                                    32
      CHARACTER NCALG_FRONTTHK*12/'R.FRONTTHK'/                           32
      INTEGER MCALG_FRONTTHK(2)/1,1/                                      32
      REAL RE_CALG_FRONTTHK                                               32
C -                                                                       32
      REAL CALG_COMPTHK                                                   32
      EQUIVALENCE (BANK_CALG(8),CALG_COMPTHK,RE_CALG_COMPTHK),            32
     *(NAMES_CALG(6),NCALG_COMPTHK)                                       32
      EQUIVALENCE (COMMEN_CALG(6),CCALG_COMPTHK),(MAP_CALG(1,6),          32
     *MCALG_COMPTHK)                                                      32
      CHARACTER CCALG_COMPTHK*40/' '/                                     32
      CHARACTER NCALG_COMPTHK*12/'R.COMPTHK'/                             32
      INTEGER MCALG_COMPTHK(2)/1,1/                                       32
      REAL RE_CALG_COMPTHK                                                32
C -                                                                       32
      REAL CALG_AIRTHK                                                    32
      EQUIVALENCE (BANK_CALG(9),CALG_AIRTHK,RE_CALG_AIRTHK),              32
     *(NAMES_CALG(7),NCALG_AIRTHK)                                        32
      EQUIVALENCE (COMMEN_CALG(7),CCALG_AIRTHK),(MAP_CALG(1,7),           32
     *MCALG_AIRTHK)                                                       32
      CHARACTER CCALG_AIRTHK*40/' '/                                      32
      CHARACTER NCALG_AIRTHK*12/'R.AIRTHK'/                               32
      INTEGER MCALG_AIRTHK(2)/1,1/                                        32
      REAL RE_CALG_AIRTHK                                                 32
C -                                                                       32
      REAL CALG_BACKTHK                                                   32
      EQUIVALENCE (BANK_CALG(10),CALG_BACKTHK,RE_CALG_BACKTHK),           32
     *(NAMES_CALG(8),NCALG_BACKTHK)                                       32
      EQUIVALENCE (COMMEN_CALG(8),CCALG_BACKTHK),(MAP_CALG(1,8),          32
     *MCALG_BACKTHK)                                                      32
      CHARACTER CCALG_BACKTHK*40/' '/                                     32
      CHARACTER NCALG_BACKTHK*12/'R.BACKTHK'/                             32
      INTEGER MCALG_BACKTHK(2)/1,1/                                       32
      REAL RE_CALG_BACKTHK                                                32
C -                                                                       32
      REAL CALG_SPACETHK                                                  32
      EQUIVALENCE (BANK_CALG(11),CALG_SPACETHK,RE_CALG_SPACETHK),         32
     *(NAMES_CALG(9),NCALG_SPACETHK)                                      32
      EQUIVALENCE (COMMEN_CALG(9),CCALG_SPACETHK),(MAP_CALG(1,9),         32
     *MCALG_SPACETHK)                                                     32
      CHARACTER CCALG_SPACETHK*40/' '/                                    32
      CHARACTER NCALG_SPACETHK*12/'R.SPACETHK'/                           32
      INTEGER MCALG_SPACETHK(2)/1,1/                                      32
      REAL RE_CALG_SPACETHK                                               32
C -                                                                       32
      REAL CALG_SCINTTHK(2)                                               32
      EQUIVALENCE (BANK_CALG(12),CALG_SCINTTHK,RE_CALG_SCINTTHK),         32
     *(NAMES_CALG(10),NCALG_SCINTTHK)                                     32
      EQUIVALENCE (COMMEN_CALG(10),CCALG_SCINTTHK),(MAP_CALG(1,10),       32
     *MCALG_SCINTTHK)                                                     32
      CHARACTER CCALG_SCINTTHK*40/' '/                                    32
      CHARACTER NCALG_SCINTTHK*12/'R.SCINTTHK'/                           32
      INTEGER MCALG_SCINTTHK(2)/2,1/                                      32
      REAL RE_CALG_SCINTTHK                                               32
C -                                                                       32
      REAL CALG_ABSORTHK                                                  32
      EQUIVALENCE (BANK_CALG(14),CALG_ABSORTHK,RE_CALG_ABSORTHK),         32
     *(NAMES_CALG(11),NCALG_ABSORTHK)                                     32
      EQUIVALENCE (COMMEN_CALG(11),CCALG_ABSORTHK),(MAP_CALG(1,11),       32
     *MCALG_ABSORTHK)                                                     32
      CHARACTER CCALG_ABSORTHK*40/' '/                                    32
      CHARACTER NCALG_ABSORTHK*12/'R.ABSORTHK'/                           32
      INTEGER MCALG_ABSORTHK(2)/1,1/                                      32
      REAL RE_CALG_ABSORTHK                                               32
C -                                                                       32
      REAL CALG_ABPAPTHK                                                  32
      EQUIVALENCE (BANK_CALG(15),CALG_ABPAPTHK,RE_CALG_ABPAPTHK),         32
     *(NAMES_CALG(12),NCALG_ABPAPTHK)                                     32
      EQUIVALENCE (COMMEN_CALG(12),CCALG_ABPAPTHK),(MAP_CALG(1,12),       32
     *MCALG_ABPAPTHK)                                                     32
      CHARACTER CCALG_ABPAPTHK*40/' '/                                    32
      CHARACTER NCALG_ABPAPTHK*12/'R.ABPAPTHK'/                           32
      INTEGER MCALG_ABPAPTHK(2)/1,1/                                      32
      REAL RE_CALG_ABPAPTHK                                               32
C -                                                                       32
      REAL CALG_G10SBTHK                                                  32
      EQUIVALENCE (BANK_CALG(16),CALG_G10SBTHK,RE_CALG_G10SBTHK),         32
     *(NAMES_CALG(13),NCALG_G10SBTHK)                                     32
      EQUIVALENCE (COMMEN_CALG(13),CCALG_G10SBTHK),(MAP_CALG(1,13),       32
     *MCALG_G10SBTHK)                                                     32
      CHARACTER CCALG_G10SBTHK*40/' '/                                    32
      CHARACTER NCALG_G10SBTHK*12/'R.G10SBTHK'/                           32
      INTEGER MCALG_G10SBTHK(2)/1,1/                                      32
      REAL RE_CALG_G10SBTHK                                               32
C -                                                                       32
      REAL CALG_SMALFWDH                                                  32
      EQUIVALENCE (BANK_CALG(17),CALG_SMALFWDH,RE_CALG_SMALFWDH),         32
     *(NAMES_CALG(14),NCALG_SMALFWDH)                                     32
      EQUIVALENCE (COMMEN_CALG(14),CCALG_SMALFWDH),(MAP_CALG(1,14),       32
     *MCALG_SMALFWDH)                                                     32
      CHARACTER CCALG_SMALFWDH*40/' '/                                    32
      CHARACTER NCALG_SMALFWDH*12/'R.SMALFWDH'/                           32
      INTEGER MCALG_SMALFWDH(2)/1,1/                                      32
      REAL RE_CALG_SMALFWDH                                               32
C -                                                                       32
      REAL CALG_SMALFTHK                                                  32
      EQUIVALENCE (BANK_CALG(18),CALG_SMALFTHK,RE_CALG_SMALFTHK),         32
     *(NAMES_CALG(15),NCALG_SMALFTHK)                                     32
      EQUIVALENCE (COMMEN_CALG(15),CCALG_SMALFTHK),(MAP_CALG(1,15),       32
     *MCALG_SMALFTHK)                                                     32
      CHARACTER CCALG_SMALFTHK*40/' '/                                    32
      CHARACTER NCALG_SMALFTHK*12/'R.SMALFTHK'/                           32
      INTEGER MCALG_SMALFTHK(2)/1,1/                                      32
      REAL RE_CALG_SMALFTHK                                               32
C -                                                                       32
      REAL CALG_SMGASTHK                                                  32
      EQUIVALENCE (BANK_CALG(19),CALG_SMGASTHK,RE_CALG_SMGASTHK),         32
     *(NAMES_CALG(16),NCALG_SMGASTHK)                                     32
      EQUIVALENCE (COMMEN_CALG(16),CCALG_SMGASTHK),(MAP_CALG(1,16),       32
     *MCALG_SMGASTHK)                                                     32
      CHARACTER CCALG_SMGASTHK*40/' '/                                    32
      CHARACTER NCALG_SMGASTHK*12/'R.SMGASTHK'/                           32
      INTEGER MCALG_SMGASTHK(2)/1,1/                                      32
      REAL RE_CALG_SMGASTHK                                               32
C -                                                                       32
      REAL CALG_SMGASWDH                                                  32
      EQUIVALENCE (BANK_CALG(20),CALG_SMGASWDH,RE_CALG_SMGASWDH),         32
     *(NAMES_CALG(17),NCALG_SMGASWDH)                                     32
      EQUIVALENCE (COMMEN_CALG(17),CCALG_SMGASWDH),(MAP_CALG(1,17),       32
     *MCALG_SMGASWDH)                                                     32
      CHARACTER CCALG_SMGASWDH*40/' '/                                    32
      CHARACTER NCALG_SMGASWDH*12/'R.SMGASWDH'/                           32
      INTEGER MCALG_SMGASWDH(2)/1,1/                                      32
      REAL RE_CALG_SMGASWDH                                               32
C -                                                                       32
      REAL CALG_SMGASRAD                                                  32
      EQUIVALENCE (BANK_CALG(21),CALG_SMGASRAD,RE_CALG_SMGASRAD),         32
     *(NAMES_CALG(18),NCALG_SMGASRAD)                                     32
      EQUIVALENCE (COMMEN_CALG(18),CCALG_SMGASRAD),(MAP_CALG(1,18),       32
     *MCALG_SMGASRAD)                                                     32
      CHARACTER CCALG_SMGASRAD*40/' '/                                    32
      CHARACTER NCALG_SMGASRAD*12/'R.SMGASRAD'/                           32
      INTEGER MCALG_SMGASRAD(2)/1,1/                                      32
      REAL RE_CALG_SMGASRAD                                               32
C -                                                                       32
      REAL CALG_SMAFFWDH                                                  32
      EQUIVALENCE (BANK_CALG(22),CALG_SMAFFWDH,RE_CALG_SMAFFWDH),         32
     *(NAMES_CALG(19),NCALG_SMAFFWDH)                                     32
      EQUIVALENCE (COMMEN_CALG(19),CCALG_SMAFFWDH),(MAP_CALG(1,19),       32
     *MCALG_SMAFFWDH)                                                     32
      CHARACTER CCALG_SMAFFWDH*40/' '/                                    32
      CHARACTER NCALG_SMAFFWDH*12/'R.SMAFFWDH'/                           32
      INTEGER MCALG_SMAFFWDH(2)/1,1/                                      32
      REAL RE_CALG_SMAFFWDH                                               32
C -                                                                       32
      REAL CALG_SMAFBWDH                                                  32
      EQUIVALENCE (BANK_CALG(23),CALG_SMAFBWDH,RE_CALG_SMAFBWDH),         32
     *(NAMES_CALG(20),NCALG_SMAFBWDH)                                     32
      EQUIVALENCE (COMMEN_CALG(20),CCALG_SMAFBWDH),(MAP_CALG(1,20),       32
     *MCALG_SMAFBWDH)                                                     32
      CHARACTER CCALG_SMAFBWDH*40/' '/                                    32
      CHARACTER NCALG_SMAFBWDH*12/'R.SMAFBWDH'/                           32
      INTEGER MCALG_SMAFBWDH(2)/1,1/                                      32
      REAL RE_CALG_SMAFBWDH                                               32
C -                                                                       32
      REAL CALG_SMETAWDH                                                  32
      EQUIVALENCE (BANK_CALG(24),CALG_SMETAWDH,RE_CALG_SMETAWDH),         32
     *(NAMES_CALG(21),NCALG_SMETAWDH)                                     32
      EQUIVALENCE (COMMEN_CALG(21),CCALG_SMETAWDH),(MAP_CALG(1,21),       32
     *MCALG_SMETAWDH)                                                     32
      CHARACTER CCALG_SMETAWDH*40/' '/                                    32
      CHARACTER NCALG_SMETAWDH*12/'R.SMETAWDH'/                           32
      INTEGER MCALG_SMETAWDH(2)/1,1/                                      32
      REAL RE_CALG_SMETAWDH                                               32
C -                                                                       32
      REAL CALG_SETA1WDH                                                  32
      EQUIVALENCE (BANK_CALG(25),CALG_SETA1WDH,RE_CALG_SETA1WDH),         32
     *(NAMES_CALG(22),NCALG_SETA1WDH)                                     32
      EQUIVALENCE (COMMEN_CALG(22),CCALG_SETA1WDH),(MAP_CALG(1,22),       32
     *MCALG_SETA1WDH)                                                     32
      CHARACTER CCALG_SETA1WDH*40/' '/                                    32
      CHARACTER NCALG_SETA1WDH*12/'R.SETA1WDH'/                           32
      INTEGER MCALG_SETA1WDH(2)/1,1/                                      32
      REAL RE_CALG_SETA1WDH                                               32
C -                                                                       32
      REAL CALG_NETFIRST                                                  32
      EQUIVALENCE (BANK_CALG(26),CALG_NETFIRST,RE_CALG_NETFIRST),         32
     *(NAMES_CALG(23),NCALG_NETFIRST)                                     32
      EQUIVALENCE (COMMEN_CALG(23),CCALG_NETFIRST),(MAP_CALG(1,23),       32
     *MCALG_NETFIRST)                                                     32
      CHARACTER CCALG_NETFIRST*40/' '/                                    32
      CHARACTER NCALG_NETFIRST*12/'R.NETFIRST'/                           32
      INTEGER MCALG_NETFIRST(2)/1,1/                                      32
      REAL RE_CALG_NETFIRST                                               32
C -                                                                       32
      REAL CALG_SETA2WDH                                                  32
      EQUIVALENCE (BANK_CALG(27),CALG_SETA2WDH,RE_CALG_SETA2WDH),         32
     *(NAMES_CALG(24),NCALG_SETA2WDH)                                     32
      EQUIVALENCE (COMMEN_CALG(24),CCALG_SETA2WDH),(MAP_CALG(1,24),       32
     *MCALG_SETA2WDH)                                                     32
      CHARACTER CCALG_SETA2WDH*40/' '/                                    32
      CHARACTER NCALG_SETA2WDH*12/'R.SETA2WDH'/                           32
      INTEGER MCALG_SETA2WDH(2)/1,1/                                      32
      REAL RE_CALG_SETA2WDH                                               32
C -                                                                       32
      REAL CALG_NETSECON                                                  32
      EQUIVALENCE (BANK_CALG(28),CALG_NETSECON,RE_CALG_NETSECON),         32
     *(NAMES_CALG(25),NCALG_NETSECON)                                     32
      EQUIVALENCE (COMMEN_CALG(25),CCALG_NETSECON),(MAP_CALG(1,25),       32
     *MCALG_NETSECON)                                                     32
      CHARACTER CCALG_NETSECON*40/' '/                                    32
      CHARACTER NCALG_NETSECON*12/'R.NETSECON'/                           32
      INTEGER MCALG_NETSECON(2)/1,1/                                      32
      REAL RE_CALG_NETSECON                                               32
C -                                                                       32
      REAL CALG_SET12WDH                                                  32
      EQUIVALENCE (BANK_CALG(29),CALG_SET12WDH,RE_CALG_SET12WDH),         32
     *(NAMES_CALG(26),NCALG_SET12WDH)                                     32
      EQUIVALENCE (COMMEN_CALG(26),CCALG_SET12WDH),(MAP_CALG(1,26),       32
     *MCALG_SET12WDH)                                                     32
      CHARACTER CCALG_SET12WDH*40/' '/                                    32
      CHARACTER NCALG_SET12WDH*12/'R.SET12WDH'/                           32
      INTEGER MCALG_SET12WDH(2)/1,1/                                      32
      REAL RE_CALG_SET12WDH                                               32
C -                                                                       32
      REAL CALG_SPHIWDH                                                   32
      EQUIVALENCE (BANK_CALG(30),CALG_SPHIWDH,RE_CALG_SPHIWDH),           32
     *(NAMES_CALG(27),NCALG_SPHIWDH)                                      32
      EQUIVALENCE (COMMEN_CALG(27),CCALG_SPHIWDH),(MAP_CALG(1,27),        32
     *MCALG_SPHIWDH)                                                      32
      CHARACTER CCALG_SPHIWDH*40/' '/                                     32
      CHARACTER NCALG_SPHIWDH*12/'R.SPHIWDH'/                             32
      INTEGER MCALG_SPHIWDH(2)/1,1/                                       32
      REAL RE_CALG_SPHIWDH                                                32
C -                                                                       32
      REAL CALG_SPHIDWDH                                                  32
      EQUIVALENCE (BANK_CALG(31),CALG_SPHIDWDH,RE_CALG_SPHIDWDH),         32
     *(NAMES_CALG(28),NCALG_SPHIDWDH)                                     32
      EQUIVALENCE (COMMEN_CALG(28),CCALG_SPHIDWDH),(MAP_CALG(1,28),       32
     *MCALG_SPHIDWDH)                                                     32
      CHARACTER CCALG_SPHIDWDH*40/' '/                                    32
      CHARACTER NCALG_SPHIDWDH*12/'R.SPHIDWDH'/                           32
      INTEGER MCALG_SPHIDWDH(2)/1,1/                                      32
      REAL RE_CALG_SPHIDWDH                                               32
C -                                                                       32
      REAL CALG_NPHISTR                                                   32
      EQUIVALENCE (BANK_CALG(32),CALG_NPHISTR,RE_CALG_NPHISTR),           32
     *(NAMES_CALG(29),NCALG_NPHISTR)                                      32
      EQUIVALENCE (COMMEN_CALG(29),CCALG_NPHISTR),(MAP_CALG(1,29),        32
     *MCALG_NPHISTR)                                                      32
      CHARACTER CCALG_NPHISTR*40/' '/                                     32
      CHARACTER NCALG_NPHISTR*12/'R.NPHISTR'/                             32
      INTEGER MCALG_NPHISTR(2)/1,1/                                       32
      REAL RE_CALG_NPHISTR                                                32
C -                                                                       32
      REAL CALG_NSMDALW                                                   32
      EQUIVALENCE (BANK_CALG(33),CALG_NSMDALW,RE_CALG_NSMDALW),           32
     *(NAMES_CALG(30),NCALG_NSMDALW)                                      32
      EQUIVALENCE (COMMEN_CALG(30),CCALG_NSMDALW),(MAP_CALG(1,30),        32
     *MCALG_NSMDALW)                                                      32
      CHARACTER CCALG_NSMDALW*40/' '/                                     32
      CHARACTER NCALG_NSMDALW*12/'R.NSMDALW'/                             32
      INTEGER MCALG_NSMDALW(2)/1,1/                                       32
      REAL RE_CALG_NSMDALW                                                32
C -                                                                       32
      REAL CALG_NSUPER                                                    32
      EQUIVALENCE (BANK_CALG(34),CALG_NSUPER,RE_CALG_NSUPER),             32
     *(NAMES_CALG(31),NCALG_NSUPER)                                       32
      EQUIVALENCE (COMMEN_CALG(31),CCALG_NSUPER),(MAP_CALG(1,31),         32
     *MCALG_NSUPER)                                                       32
      CHARACTER CCALG_NSUPER*40/' '/                                      32
      CHARACTER NCALG_NSUPER*12/'R.NSUPER'/                               32
      INTEGER MCALG_NSUPER(2)/1,1/                                        32
      REAL RE_CALG_NSUPER                                                 32
C -                                                                       32
      REAL CALG_NSMD                                                      32
      EQUIVALENCE (BANK_CALG(35),CALG_NSMD,RE_CALG_NSMD),                 32
     *(NAMES_CALG(32),NCALG_NSMD)                                         32
      EQUIVALENCE (COMMEN_CALG(32),CCALG_NSMD),(MAP_CALG(1,32),           32
     *MCALG_NSMD)                                                         32
      CHARACTER CCALG_NSMD*40/' '/                                        32
      CHARACTER NCALG_NSMD*12/'R.NSMD'/                                   32
      INTEGER MCALG_NSMD(2)/1,1/                                          32
      REAL RE_CALG_NSMD                                                   32
C -                                                                       32
      REAL CALG_NSUBLAY(2)                                                32
      EQUIVALENCE (BANK_CALG(36),CALG_NSUBLAY,RE_CALG_NSUBLAY),           32
     *(NAMES_CALG(33),NCALG_NSUBLAY)                                      32
      EQUIVALENCE (COMMEN_CALG(33),CCALG_NSUBLAY),(MAP_CALG(1,33),        32
     *MCALG_NSUBLAY)                                                      32
      CHARACTER CCALG_NSUBLAY*40/' '/                                     32
      CHARACTER NCALG_NSUBLAY*12/'R.NSUBLAY'/                             32
      INTEGER MCALG_NSUBLAY(2)/2,1/                                       32
      REAL RE_CALG_NSUBLAY                                                32
C -                                                                       32
      REAL CALG_NMODULE(2)                                                32
      EQUIVALENCE (BANK_CALG(38),CALG_NMODULE,RE_CALG_NMODULE),           32
     *(NAMES_CALG(34),NCALG_NMODULE)                                      32
      EQUIVALENCE (COMMEN_CALG(34),CCALG_NMODULE),(MAP_CALG(1,34),        32
     *MCALG_NMODULE)                                                      32
      CHARACTER CCALG_NMODULE*40/' '/                                     32
      CHARACTER NCALG_NMODULE*12/'R.NMODULE'/                             32
      INTEGER MCALG_NMODULE(2)/2,1/                                       32
      REAL RE_CALG_NMODULE                                                32
C -                                                                       32
      REAL CALG_SHIFT(2)                                                  32
      EQUIVALENCE (BANK_CALG(40),CALG_SHIFT,RE_CALG_SHIFT),               32
     *(NAMES_CALG(35),NCALG_SHIFT)                                        32
      EQUIVALENCE (COMMEN_CALG(35),CCALG_SHIFT),(MAP_CALG(1,35),          32
     *MCALG_SHIFT)                                                        32
      CHARACTER CCALG_SHIFT*40/' '/                                       32
      CHARACTER NCALG_SHIFT*12/'R.SHIFT'/                                 32
      INTEGER MCALG_SHIFT(2)/2,1/                                         32
      REAL RE_CALG_SHIFT                                                  32
C -                                                                       32
      REAL CALG_MAXMODULE                                                 32
      EQUIVALENCE (BANK_CALG(42),CALG_MAXMODULE,RE_CALG_MAXMODULE),       32
     *(NAMES_CALG(36),NCALG_MAXMODULE)                                    32
      EQUIVALENCE (COMMEN_CALG(36),CCALG_MAXMODULE),(MAP_CALG(1,36),      32
     *MCALG_MAXMODULE)                                                    32
      CHARACTER CCALG_MAXMODULE*40/' '/                                   32
      CHARACTER NCALG_MAXMODULE*12/'R.MAXMODULE'/                         32
      INTEGER MCALG_MAXMODULE(2)/1,1/                                     32
      REAL RE_CALG_MAXMODULE                                              32
C -                                                                       32
      REAL CALG_NETAT                                                     32
      EQUIVALENCE (BANK_CALG(43),CALG_NETAT,RE_CALG_NETAT),               32
     *(NAMES_CALG(37),NCALG_NETAT)                                        32
      EQUIVALENCE (COMMEN_CALG(37),CCALG_NETAT),(MAP_CALG(1,37),          32
     *MCALG_NETAT)                                                        32
      CHARACTER CCALG_NETAT*40/' '/                                       32
      CHARACTER NCALG_NETAT*12/'R.NETAT'/                                 32
      INTEGER MCALG_NETAT(2)/1,1/                                         32
      REAL RE_CALG_NETAT                                                  32
C -                                                                       32
      REAL CALG_NSUB                                                      32
      EQUIVALENCE (BANK_CALG(44),CALG_NSUB,RE_CALG_NSUB),                 32
     *(NAMES_CALG(38),NCALG_NSUB)                                         32
      EQUIVALENCE (COMMEN_CALG(38),CCALG_NSUB),(MAP_CALG(1,38),           32
     *MCALG_NSUB)                                                         32
      CHARACTER CCALG_NSUB*40/' '/                                        32
      CHARACTER NCALG_NSUB*12/'R.NSUB'/                                   32
      INTEGER MCALG_NSUB(2)/1,1/                                          32
      REAL RE_CALG_NSUB                                                   32
C -                                                                       32
      REAL CALG_NETASMDP                                                  32
      EQUIVALENCE (BANK_CALG(45),CALG_NETASMDP,RE_CALG_NETASMDP),         32
     *(NAMES_CALG(39),NCALG_NETASMDP)                                     32
      EQUIVALENCE (COMMEN_CALG(39),CCALG_NETASMDP),(MAP_CALG(1,39),       32
     *MCALG_NETASMDP)                                                     32
      CHARACTER CCALG_NETASMDP*40/' '/                                    32
      CHARACTER NCALG_NETASMDP*12/'R.NETASMDP'/                           32
      INTEGER MCALG_NETASMDP(2)/1,1/                                      32
      REAL RE_CALG_NETASMDP                                               32
C -                                                                       32
      INTEGER LEN_CALG,LLEN_CALG,LINK_CALG                                32
      PARAMETER (LEN_CALG=45)                                             32
      PARAMETER (LLEN_CALG=39)                                            32
      INTEGER FLAG_CALG/0/,MAP_CALG(2,LLEN_CALG),IDNUM_CALG(10)           32
      CHARACTER NAMES_CALG(LLEN_CALG)*12,COMMEN_CALG(LLEN_CALG)*40        32
      DATA IDNUM_CALG/10*0/                                               32
      REAL BANK_CALG(LEN_CALG)                                            32
      CHARACTER*60 BPATH_CALG/' '/                                        32
      SAVE FLAG_CALG,MAP_CALG,IDNUM_CALG,NAMES_CALG,COMMEN_CALG,          32
     *BPATH_CALG                                                          32
      DATA LINK_CALG/0/,BANK_CALG/LEN_CALG*0/                             32
C *                                                                       32
C -                                                                       33
      REAL CALR_RMIN                                                      33
      EQUIVALENCE (BANK_CALR(3),CALR_RMIN,RE_CALR_RMIN),(NAMES_CALR(1),   33
     *NCALR_RMIN)                                                         33
      EQUIVALENCE (COMMEN_CALR(1),CCALR_RMIN),(MAP_CALR(1,1),             33
     *MCALR_RMIN)                                                         33
      CHARACTER CCALR_RMIN*40/' '/                                        33
      CHARACTER NCALR_RMIN*12/'R.RMIN'/                                   33
      INTEGER MCALR_RMIN(2)/1,1/                                          33
      REAL RE_CALR_RMIN                                                   33
C -                                                                       33
      REAL CALR_RPRS                                                      33
      EQUIVALENCE (BANK_CALR(4),CALR_RPRS,RE_CALR_RPRS),(NAMES_CALR(2),   33
     *NCALR_RPRS)                                                         33
      EQUIVALENCE (COMMEN_CALR(2),CCALR_RPRS),(MAP_CALR(1,2),             33
     *MCALR_RPRS)                                                         33
      CHARACTER CCALR_RPRS*40/' '/                                        33
      CHARACTER NCALR_RPRS*12/'R.RPRS'/                                   33
      INTEGER MCALR_RPRS(2)/1,1/                                          33
      REAL RE_CALR_RPRS                                                   33
C -                                                                       33
      REAL CALR_RSMD1                                                     33
      EQUIVALENCE (BANK_CALR(5),CALR_RSMD1,RE_CALR_RSMD1),                33
     *(NAMES_CALR(3),NCALR_RSMD1)                                         33
      EQUIVALENCE (COMMEN_CALR(3),CCALR_RSMD1),(MAP_CALR(1,3),            33
     *MCALR_RSMD1)                                                        33
      CHARACTER CCALR_RSMD1*40/' '/                                       33
      CHARACTER NCALR_RSMD1*12/'R.RSMD1'/                                 33
      INTEGER MCALR_RSMD1(2)/1,1/                                         33
      REAL RE_CALR_RSMD1                                                  33
C -                                                                       33
      REAL CALR_RSMD2                                                     33
      EQUIVALENCE (BANK_CALR(6),CALR_RSMD2,RE_CALR_RSMD2),                33
     *(NAMES_CALR(4),NCALR_RSMD2)                                         33
      EQUIVALENCE (COMMEN_CALR(4),CCALR_RSMD2),(MAP_CALR(1,4),            33
     *MCALR_RSMD2)                                                        33
      CHARACTER CCALR_RSMD2*40/' '/                                       33
      CHARACTER NCALR_RSMD2*12/'R.RSMD2'/                                 33
      INTEGER MCALR_RSMD2(2)/1,1/                                         33
      REAL RE_CALR_RSMD2                                                  33
C -                                                                       33
      REAL CALR_RMAX                                                      33
      EQUIVALENCE (BANK_CALR(7),CALR_RMAX,RE_CALR_RMAX),(NAMES_CALR(5),   33
     *NCALR_RMAX)                                                         33
      EQUIVALENCE (COMMEN_CALR(5),CCALR_RMAX),(MAP_CALR(1,5),             33
     *MCALR_RMAX)                                                         33
      CHARACTER CCALR_RMAX*40/' '/                                        33
      CHARACTER NCALR_RMAX*12/'R.RMAX'/                                   33
      INTEGER MCALR_RMAX(2)/1,1/                                          33
      REAL RE_CALR_RMAX                                                   33
C -                                                                       33
      INTEGER LEN_CALR,LLEN_CALR,LINK_CALR                                33
      PARAMETER (LEN_CALR=7)                                              33
      PARAMETER (LLEN_CALR=5)                                             33
      INTEGER FLAG_CALR/0/,MAP_CALR(2,LLEN_CALR),IDNUM_CALR(10)           33
      CHARACTER NAMES_CALR(LLEN_CALR)*12,COMMEN_CALR(LLEN_CALR)*40        33
      DATA IDNUM_CALR/10*0/                                               33
      REAL BANK_CALR(LEN_CALR)                                            33
      CHARACTER*60 BPATH_CALR/' '/                                        33
      SAVE FLAG_CALR,MAP_CALR,IDNUM_CALR,NAMES_CALR,COMMEN_CALR,          33
     *BPATH_CALR                                                          33
      DATA LINK_CALR/0/,BANK_CALR/LEN_CALR*0/                             33
C *                                                                       33
C -                                                                       34
      REAL CCUT_VERSION                                                   34
      EQUIVALENCE (BANK_CCUT(3),CCUT_VERSION,RE_CCUT_VERSION),            34
     *(NAMES_CCUT(1),NCCUT_VERSION)                                       34
      EQUIVALENCE (COMMEN_CCUT(1),CCCUT_VERSION),(MAP_CCUT(1,1),          34
     *MCCUT_VERSION)                                                      34
      CHARACTER CCCUT_VERSION*40/' '/                                     34
      CHARACTER NCCUT_VERSION*12/'R.VERSION'/                             34
      INTEGER MCCUT_VERSION(2)/1,1/                                       34
      REAL RE_CCUT_VERSION                                                34
C -                                                                       34
      REAL CCUT_ABSORBER                                                  34
      EQUIVALENCE (BANK_CCUT(4),CCUT_ABSORBER,RE_CCUT_ABSORBER),          34
     *(NAMES_CCUT(2),NCCUT_ABSORBER)                                      34
      EQUIVALENCE (COMMEN_CCUT(2),CCCUT_ABSORBER),(MAP_CCUT(1,2),         34
     *MCCUT_ABSORBER)                                                     34
      CHARACTER CCCUT_ABSORBER*40/' '/                                    34
      CHARACTER NCCUT_ABSORBER*12/'R.ABSORBER'/                           34
      INTEGER MCCUT_ABSORBER(2)/1,1/                                      34
      REAL RE_CCUT_ABSORBER                                               34
C -                                                                       34
      REAL CCUT_SENSITIVE                                                 34
      EQUIVALENCE (BANK_CCUT(5),CCUT_SENSITIVE,RE_CCUT_SENSITIVE),        34
     *(NAMES_CCUT(3),NCCUT_SENSITIVE)                                     34
      EQUIVALENCE (COMMEN_CCUT(3),CCCUT_SENSITIVE),(MAP_CCUT(1,3),        34
     *MCCUT_SENSITIVE)                                                    34
      CHARACTER CCCUT_SENSITIVE*40/' '/                                   34
      CHARACTER NCCUT_SENSITIVE*12/'R.SENSITIVE'/                         34
      INTEGER MCCUT_SENSITIVE(2)/1,1/                                     34
      REAL RE_CCUT_SENSITIVE                                              34
C -                                                                       34
      INTEGER LEN_CCUT,LLEN_CCUT,LINK_CCUT                                34
      PARAMETER (LEN_CCUT=5)                                              34
      PARAMETER (LLEN_CCUT=3)                                             34
      INTEGER FLAG_CCUT/0/,MAP_CCUT(2,LLEN_CCUT),IDNUM_CCUT(10)           34
      CHARACTER NAMES_CCUT(LLEN_CCUT)*12,COMMEN_CCUT(LLEN_CCUT)*40        34
      DATA IDNUM_CCUT/10*0/                                               34
      REAL BANK_CCUT(LEN_CCUT)                                            34
      CHARACTER*60 BPATH_CCUT/' '/                                        34
      SAVE FLAG_CCUT,MAP_CCUT,IDNUM_CCUT,NAMES_CCUT,COMMEN_CCUT,          34
     *BPATH_CCUT                                                          34
      DATA LINK_CCUT/0/,BANK_CCUT/LEN_CCUT*0/                             34
C *                                                                       34
C -                                                                       36
      REAL CABS_VERSION                                                   36
      EQUIVALENCE (BANK_CABS(3),CABS_VERSION,RE_CABS_VERSION),            36
     *(NAMES_CABS(1),NCABS_VERSION)                                       36
      EQUIVALENCE (COMMEN_CABS(1),CCABS_VERSION),(MAP_CABS(1,1),          36
     *MCABS_VERSION)                                                      36
      CHARACTER CCABS_VERSION*40/' '/                                     36
      CHARACTER NCABS_VERSION*12/'R.VERSION'/                             36
      INTEGER MCABS_VERSION(2)/1,1/                                       36
      REAL RE_CABS_VERSION                                                36
C -                                                                       36
      REAL CABS_CUTGAM                                                    36
      EQUIVALENCE (BANK_CABS(4),CABS_CUTGAM,RE_CABS_CUTGAM),              36
     *(NAMES_CABS(2),NCABS_CUTGAM)                                        36
      EQUIVALENCE (COMMEN_CABS(2),CCABS_CUTGAM),(MAP_CABS(1,2),           36
     *MCABS_CUTGAM)                                                       36
      CHARACTER CCABS_CUTGAM*40/' '/                                      36
      CHARACTER NCABS_CUTGAM*12/'R.CUTGAM'/                               36
      INTEGER MCABS_CUTGAM(2)/1,1/                                        36
      REAL RE_CABS_CUTGAM                                                 36
C -                                                                       36
      REAL CABS_CUTELE                                                    36
      EQUIVALENCE (BANK_CABS(5),CABS_CUTELE,RE_CABS_CUTELE),              36
     *(NAMES_CABS(3),NCABS_CUTELE)                                        36
      EQUIVALENCE (COMMEN_CABS(3),CCABS_CUTELE),(MAP_CABS(1,3),           36
     *MCABS_CUTELE)                                                       36
      CHARACTER CCABS_CUTELE*40/' '/                                      36
      CHARACTER NCABS_CUTELE*12/'R.CUTELE'/                               36
      INTEGER MCABS_CUTELE(2)/1,1/                                        36
      REAL RE_CABS_CUTELE                                                 36
C -                                                                       36
      REAL CABS_CUTNEU                                                    36
      EQUIVALENCE (BANK_CABS(6),CABS_CUTNEU,RE_CABS_CUTNEU),              36
     *(NAMES_CABS(4),NCABS_CUTNEU)                                        36
      EQUIVALENCE (COMMEN_CABS(4),CCABS_CUTNEU),(MAP_CABS(1,4),           36
     *MCABS_CUTNEU)                                                       36
      CHARACTER CCABS_CUTNEU*40/' '/                                      36
      CHARACTER NCABS_CUTNEU*12/'R.CUTNEU'/                               36
      INTEGER MCABS_CUTNEU(2)/1,1/                                        36
      REAL RE_CABS_CUTNEU                                                 36
C -                                                                       36
      REAL CABS_CUTHAD                                                    36
      EQUIVALENCE (BANK_CABS(7),CABS_CUTHAD,RE_CABS_CUTHAD),              36
     *(NAMES_CABS(5),NCABS_CUTHAD)                                        36
      EQUIVALENCE (COMMEN_CABS(5),CCABS_CUTHAD),(MAP_CABS(1,5),           36
     *MCABS_CUTHAD)                                                       36
      CHARACTER CCABS_CUTHAD*40/' '/                                      36
      CHARACTER NCABS_CUTHAD*12/'R.CUTHAD'/                               36
      INTEGER MCABS_CUTHAD(2)/1,1/                                        36
      REAL RE_CABS_CUTHAD                                                 36
C -                                                                       36
      REAL CABS_CUTMUO                                                    36
      EQUIVALENCE (BANK_CABS(8),CABS_CUTMUO,RE_CABS_CUTMUO),              36
     *(NAMES_CABS(6),NCABS_CUTMUO)                                        36
      EQUIVALENCE (COMMEN_CABS(6),CCABS_CUTMUO),(MAP_CABS(1,6),           36
     *MCABS_CUTMUO)                                                       36
      CHARACTER CCABS_CUTMUO*40/' '/                                      36
      CHARACTER NCABS_CUTMUO*12/'R.CUTMUO'/                               36
      INTEGER MCABS_CUTMUO(2)/1,1/                                        36
      REAL RE_CABS_CUTMUO                                                 36
C -                                                                       36
      REAL CABS_DCUTE                                                     36
      EQUIVALENCE (BANK_CABS(9),CABS_DCUTE,RE_CABS_DCUTE),                36
     *(NAMES_CABS(7),NCABS_DCUTE)                                         36
      EQUIVALENCE (COMMEN_CABS(7),CCABS_DCUTE),(MAP_CABS(1,7),            36
     *MCABS_DCUTE)                                                        36
      CHARACTER CCABS_DCUTE*40/' '/                                       36
      CHARACTER NCABS_DCUTE*12/'R.DCUTE'/                                 36
      INTEGER MCABS_DCUTE(2)/1,1/                                         36
      REAL RE_CABS_DCUTE                                                  36
C -                                                                       36
      REAL CABS_DCUTM                                                     36
      EQUIVALENCE (BANK_CABS(10),CABS_DCUTM,RE_CABS_DCUTM),               36
     *(NAMES_CABS(8),NCABS_DCUTM)                                         36
      EQUIVALENCE (COMMEN_CABS(8),CCABS_DCUTM),(MAP_CABS(1,8),            36
     *MCABS_DCUTM)                                                        36
      CHARACTER CCABS_DCUTM*40/' '/                                       36
      CHARACTER NCABS_DCUTM*12/'R.DCUTM'/                                 36
      INTEGER MCABS_DCUTM(2)/1,1/                                         36
      REAL RE_CABS_DCUTM                                                  36
C -                                                                       36
      REAL CABS_BCUTE                                                     36
      EQUIVALENCE (BANK_CABS(11),CABS_BCUTE,RE_CABS_BCUTE),               36
     *(NAMES_CABS(9),NCABS_BCUTE)                                         36
      EQUIVALENCE (COMMEN_CABS(9),CCABS_BCUTE),(MAP_CABS(1,9),            36
     *MCABS_BCUTE)                                                        36
      CHARACTER CCABS_BCUTE*40/' '/                                       36
      CHARACTER NCABS_BCUTE*12/'R.BCUTE'/                                 36
      INTEGER MCABS_BCUTE(2)/1,1/                                         36
      REAL RE_CABS_BCUTE                                                  36
C -                                                                       36
      REAL CABS_BCUTM                                                     36
      EQUIVALENCE (BANK_CABS(12),CABS_BCUTM,RE_CABS_BCUTM),               36
     *(NAMES_CABS(10),NCABS_BCUTM)                                        36
      EQUIVALENCE (COMMEN_CABS(10),CCABS_BCUTM),(MAP_CABS(1,10),          36
     *MCABS_BCUTM)                                                        36
      CHARACTER CCABS_BCUTM*40/' '/                                       36
      CHARACTER NCABS_BCUTM*12/'R.BCUTM'/                                 36
      INTEGER MCABS_BCUTM(2)/1,1/                                         36
      REAL RE_CABS_BCUTM                                                  36
C -                                                                       36
      INTEGER LEN_CABS,LLEN_CABS,LINK_CABS                                36
      PARAMETER (LEN_CABS=12)                                             36
      PARAMETER (LLEN_CABS=10)                                            36
      INTEGER FLAG_CABS/0/,MAP_CABS(2,LLEN_CABS),IDNUM_CABS(10)           36
      CHARACTER NAMES_CABS(LLEN_CABS)*12,COMMEN_CABS(LLEN_CABS)*40        36
      DATA IDNUM_CABS/10*0/                                               36
      REAL BANK_CABS(LEN_CABS)                                            36
      CHARACTER*60 BPATH_CABS/' '/                                        36
      SAVE FLAG_CABS,MAP_CABS,IDNUM_CABS,NAMES_CABS,COMMEN_CABS,          36
     *BPATH_CABS                                                          36
      DATA LINK_CABS/0/,BANK_CABS/LEN_CABS*0/                             36
C *                                                                       36
C -                                                                       38
      REAL CSEN_VERSION                                                   38
      EQUIVALENCE (BANK_CSEN(3),CSEN_VERSION,RE_CSEN_VERSION),            38
     *(NAMES_CSEN(1),NCSEN_VERSION)                                       38
      EQUIVALENCE (COMMEN_CSEN(1),CCSEN_VERSION),(MAP_CSEN(1,1),          38
     *MCSEN_VERSION)                                                      38
      CHARACTER CCSEN_VERSION*40/' '/                                     38
      CHARACTER NCSEN_VERSION*12/'R.VERSION'/                             38
      INTEGER MCSEN_VERSION(2)/1,1/                                       38
      REAL RE_CSEN_VERSION                                                38
C -                                                                       38
      REAL CSEN_CUTGAM                                                    38
      EQUIVALENCE (BANK_CSEN(4),CSEN_CUTGAM,RE_CSEN_CUTGAM),              38
     *(NAMES_CSEN(2),NCSEN_CUTGAM)                                        38
      EQUIVALENCE (COMMEN_CSEN(2),CCSEN_CUTGAM),(MAP_CSEN(1,2),           38
     *MCSEN_CUTGAM)                                                       38
      CHARACTER CCSEN_CUTGAM*40/' '/                                      38
      CHARACTER NCSEN_CUTGAM*12/'R.CUTGAM'/                               38
      INTEGER MCSEN_CUTGAM(2)/1,1/                                        38
      REAL RE_CSEN_CUTGAM                                                 38
C -                                                                       38
      REAL CSEN_CUTELE                                                    38
      EQUIVALENCE (BANK_CSEN(5),CSEN_CUTELE,RE_CSEN_CUTELE),              38
     *(NAMES_CSEN(3),NCSEN_CUTELE)                                        38
      EQUIVALENCE (COMMEN_CSEN(3),CCSEN_CUTELE),(MAP_CSEN(1,3),           38
     *MCSEN_CUTELE)                                                       38
      CHARACTER CCSEN_CUTELE*40/' '/                                      38
      CHARACTER NCSEN_CUTELE*12/'R.CUTELE'/                               38
      INTEGER MCSEN_CUTELE(2)/1,1/                                        38
      REAL RE_CSEN_CUTELE                                                 38
C -                                                                       38
      REAL CSEN_CUTNEU                                                    38
      EQUIVALENCE (BANK_CSEN(6),CSEN_CUTNEU,RE_CSEN_CUTNEU),              38
     *(NAMES_CSEN(4),NCSEN_CUTNEU)                                        38
      EQUIVALENCE (COMMEN_CSEN(4),CCSEN_CUTNEU),(MAP_CSEN(1,4),           38
     *MCSEN_CUTNEU)                                                       38
      CHARACTER CCSEN_CUTNEU*40/' '/                                      38
      CHARACTER NCSEN_CUTNEU*12/'R.CUTNEU'/                               38
      INTEGER MCSEN_CUTNEU(2)/1,1/                                        38
      REAL RE_CSEN_CUTNEU                                                 38
C -                                                                       38
      REAL CSEN_CUTHAD                                                    38
      EQUIVALENCE (BANK_CSEN(7),CSEN_CUTHAD,RE_CSEN_CUTHAD),              38
     *(NAMES_CSEN(5),NCSEN_CUTHAD)                                        38
      EQUIVALENCE (COMMEN_CSEN(5),CCSEN_CUTHAD),(MAP_CSEN(1,5),           38
     *MCSEN_CUTHAD)                                                       38
      CHARACTER CCSEN_CUTHAD*40/' '/                                      38
      CHARACTER NCSEN_CUTHAD*12/'R.CUTHAD'/                               38
      INTEGER MCSEN_CUTHAD(2)/1,1/                                        38
      REAL RE_CSEN_CUTHAD                                                 38
C -                                                                       38
      REAL CSEN_CUTMUO                                                    38
      EQUIVALENCE (BANK_CSEN(8),CSEN_CUTMUO,RE_CSEN_CUTMUO),              38
     *(NAMES_CSEN(6),NCSEN_CUTMUO)                                        38
      EQUIVALENCE (COMMEN_CSEN(6),CCSEN_CUTMUO),(MAP_CSEN(1,6),           38
     *MCSEN_CUTMUO)                                                       38
      CHARACTER CCSEN_CUTMUO*40/' '/                                      38
      CHARACTER NCSEN_CUTMUO*12/'R.CUTMUO'/                               38
      INTEGER MCSEN_CUTMUO(2)/1,1/                                        38
      REAL RE_CSEN_CUTMUO                                                 38
C -                                                                       38
      REAL CSEN_DCUTE                                                     38
      EQUIVALENCE (BANK_CSEN(9),CSEN_DCUTE,RE_CSEN_DCUTE),                38
     *(NAMES_CSEN(7),NCSEN_DCUTE)                                         38
      EQUIVALENCE (COMMEN_CSEN(7),CCSEN_DCUTE),(MAP_CSEN(1,7),            38
     *MCSEN_DCUTE)                                                        38
      CHARACTER CCSEN_DCUTE*40/' '/                                       38
      CHARACTER NCSEN_DCUTE*12/'R.DCUTE'/                                 38
      INTEGER MCSEN_DCUTE(2)/1,1/                                         38
      REAL RE_CSEN_DCUTE                                                  38
C -                                                                       38
      REAL CSEN_DCUTM                                                     38
      EQUIVALENCE (BANK_CSEN(10),CSEN_DCUTM,RE_CSEN_DCUTM),               38
     *(NAMES_CSEN(8),NCSEN_DCUTM)                                         38
      EQUIVALENCE (COMMEN_CSEN(8),CCSEN_DCUTM),(MAP_CSEN(1,8),            38
     *MCSEN_DCUTM)                                                        38
      CHARACTER CCSEN_DCUTM*40/' '/                                       38
      CHARACTER NCSEN_DCUTM*12/'R.DCUTM'/                                 38
      INTEGER MCSEN_DCUTM(2)/1,1/                                         38
      REAL RE_CSEN_DCUTM                                                  38
C -                                                                       38
      REAL CSEN_BCUTE                                                     38
      EQUIVALENCE (BANK_CSEN(11),CSEN_BCUTE,RE_CSEN_BCUTE),               38
     *(NAMES_CSEN(9),NCSEN_BCUTE)                                         38
      EQUIVALENCE (COMMEN_CSEN(9),CCSEN_BCUTE),(MAP_CSEN(1,9),            38
     *MCSEN_BCUTE)                                                        38
      CHARACTER CCSEN_BCUTE*40/' '/                                       38
      CHARACTER NCSEN_BCUTE*12/'R.BCUTE'/                                 38
      INTEGER MCSEN_BCUTE(2)/1,1/                                         38
      REAL RE_CSEN_BCUTE                                                  38
C -                                                                       38
      REAL CSEN_BCUTM                                                     38
      EQUIVALENCE (BANK_CSEN(12),CSEN_BCUTM,RE_CSEN_BCUTM),               38
     *(NAMES_CSEN(10),NCSEN_BCUTM)                                        38
      EQUIVALENCE (COMMEN_CSEN(10),CCSEN_BCUTM),(MAP_CSEN(1,10),          38
     *MCSEN_BCUTM)                                                        38
      CHARACTER CCSEN_BCUTM*40/' '/                                       38
      CHARACTER NCSEN_BCUTM*12/'R.BCUTM'/                                 38
      INTEGER MCSEN_BCUTM(2)/1,1/                                         38
      REAL RE_CSEN_BCUTM                                                  38
C -                                                                       38
      INTEGER LEN_CSEN,LLEN_CSEN,LINK_CSEN                                38
      PARAMETER (LEN_CSEN=12)                                             38
      PARAMETER (LLEN_CSEN=10)                                            38
      INTEGER FLAG_CSEN/0/,MAP_CSEN(2,LLEN_CSEN),IDNUM_CSEN(10)           38
      CHARACTER NAMES_CSEN(LLEN_CSEN)*12,COMMEN_CSEN(LLEN_CSEN)*40        38
      DATA IDNUM_CSEN/10*0/                                               38
      REAL BANK_CSEN(LEN_CSEN)                                            38
      CHARACTER*60 BPATH_CSEN/' '/                                        38
      SAVE FLAG_CSEN,MAP_CSEN,IDNUM_CSEN,NAMES_CSEN,COMMEN_CSEN,          38
     *BPATH_CSEN                                                          38
      DATA LINK_CSEN/0/,BANK_CSEN/LEN_CSEN*0/                             38
C *                                                                       38
      REAL RKB2SC/0.013/,RKB3SC/9.6E-6/                                   39
      REAL CURRENT_DEPTH,CURRENT,LAYER_WIDTH(2),TAN_THETA,SMD_WIDTH,      44
     *SMD_WIDTH1,SMD_WIDTH2, SMD_WIDTH3,CUT_LENGTH,CUT_RADIUS,            44
     *FUTURE_DEPTH,C_DEP,C_LEAD_DEP,ETA_LENGHT, CURRENT_CSDA,H_ETA1,      44
     *H_ETA2,H_PHI1,H_PHI2,SH_ETA1,SH_ETA2,SH_PHI1,SH_PHI2,RMAX, HLENG,   44
     *DETA,ANGULAR_OFFSET,DPHITOT,DPHIMOD,DPHIT,R1,R2,R3,R4,RR(2)         44
      REAL ZZ0,ZZ1,ZZ2,SIN_THETA,CUT_LENGTH2,HLENG2,ANGLE,SKINDEPTH,      47
     *SKINOFFSET,SKINOFFSET1, SKINOFFSET2,SKINTHK                         47
      REAL STRAP_DX,STRAP_DY,STRAP_DZ,STRAP_R                             49
      REAL SLUG_DX,SLUG_DY,SLUG_DZ                                        51
      REAL STRAP_SPACING                                                  53
      REAL CUT_RADIUS2,CUTANG,RMIN2,RMAX2,RMAX3                           55
      INTEGER LAYER,SUPER,SUB,I,J,II,NN,IMOD,NSLUG,ICSHI                  57
      REAL GEANG(6)                                                       59
C *                                                                       61
      CALL AGSBEGM(MODULE,IPRIN)                                          61
      IF (IPRIN.LT.0) RETURN                                              61
C Filling bank CALG - BARREL CALORIMETER DATA                             61
      BANK_TITLE='BARREL CALORIMETER DATA'                                61
      BPATH_CALG='CALG*'                                                  61
      IDNUM_CALG(1)=0                                                     62
C GEOMETRY VERSION                                                        62
      CALG_VERSION=3.0                                                    62
      CCALG_VERSION='GEOMETRY VERSION'                                    62
C INNER RADIUS                                                            63
      CALG_RMIN=223.5                                                     63
      CCALG_RMIN='INNER RADIUS'                                           63
C CALORIMETER RAPIDITY CUT                                                64
      CALG_ETACUT=1.0                                                     64
      CCALG_ETACUT='CALORIMETER RAPIDITY CUT'                             64
C HALF WIDTH OF THE CRACK BETWEEN MODULES                                 65
      CALG_CRACKWD=0.655                                                  65
      CCALG_CRACKWD='HALF WIDTH OF THE CRACK BETWEEN MODULES'             65
C FRONT PLATE HALF THICKNESS                                              66
      CALG_FRONTTHK=0.9525                                                66
      CCALG_FRONTTHK='FRONT PLATE HALF THICKNESS'                         66
C BACK PLATE HALF THICKNESS                                               67
      CALG_COMPTHK=0.9525                                                 67
      CCALG_COMPTHK='BACK PLATE HALF THICKNESS'                           67
C AIR GAP HALF THICNESS                                                   68
      CALG_AIRTHK=0.158                                                   68
      CCALG_AIRTHK='AIR GAP HALF THICNESS'                                68
C MODULE BACK PLATE HALF THICKNES                                         69
      CALG_BACKTHK=1.5875                                                 69
      CCALG_BACKTHK='MODULE BACK PLATE HALF THICKNES'                     69
C SPACER BACK PLATE HALF THICKNES                                         70
      CALG_SPACETHK=0.9525                                                70
      CCALG_SPACETHK='SPACER BACK PLATE HALF THICKNES'                    70
C ACTIVE SCINTILLATOR PLATE HALF THICKNESS                                71
      CALG_SCINTTHK(1)=0.3                                                71
      CALG_SCINTTHK(2)=0.25                                               71
      CCALG_SCINTTHK='ACTIVE SCINTILLATOR PLATE HALF THICKNESS'           71
C ABSORBER PLATE THICKNESS HALFPTHICKNESS                                 72
      CALG_ABSORTHK=0.250                                                 72
      CCALG_ABSORTHK='ABSORBER PLATE THICKNESS HALFPTHICKNESS'            72
C ABSORBER PAPER PLATE THICKNESS HALF THICKNESS                           73
      CALG_ABPAPTHK=0.005                                                 73
      CCALG_ABPAPTHK='ABSORBER PAPER PLATE THICKNESS HALF THICKNESS'      73
C G10 SMD FRONT AND BACK PLATE HALF THICKNESS                             74
      CALG_G10SBTHK=0.115                                                 74
      CCALG_G10SBTHK='G10 SMD FRONT AND BACK PLATE HALF THICKNESS'        74
C SMD AL FRONT BACK PLATE WITH AR HALF WIDTH                              75
      CALG_SMALFWDH=11.2014                                               75
      CCALG_SMALFWDH='SMD AL FRONT BACK PLATE WITH AR HALF WIDTH'         75
C SMD AL FRONT BACK PLATE WITH AR HALF THICKNESS                          76
      CALG_SMALFTHK=0.3893                                                76
      CCALG_SMALFTHK='SMD AL FRONT BACK PLATE WITH AR HALF THICKNESS'     76
C SMD GAS BOX VOLUME HALF THICKNESS                                       77
      CALG_SMGASTHK=0.1359                                                77
      CCALG_SMGASTHK='SMD GAS BOX VOLUME HALF THICKNESS'                  77
C SMD GAS BOX VOLUME HALF WIDTH                                           78
      CALG_SMGASWDH=0.2946                                                78
      CCALG_SMGASWDH='SMD GAS BOX VOLUME HALF WIDTH'                      78
C SMD GAS VOLUME TUBS RADIUS                                              79
      CALG_SMGASRAD=0.2946                                                79
      CCALG_SMGASRAD='SMD GAS VOLUME TUBS RADIUS'                         79
C SMD AL FRONT FIRST (LAST) RIB HALF WIDTH                                80
      CALG_SMAFFWDH=0.113                                                 80
      CCALG_SMAFFWDH='SMD AL FRONT FIRST (LAST) RIB HALF WIDTH'           80
C SMD AL BACK FIRST (LAST) RIB HALF WIDTH                                 81
      CALG_SMAFBWDH=0.1664                                                81
      CCALG_SMAFBWDH='SMD AL BACK FIRST (LAST) RIB HALF WIDTH'            81
C ETA=0 STRIP NOTCH HALF WIDTH                                            82
      CALG_SMETAWDH=0.9806                                                82
      CCALG_SMETAWDH='ETA=0 STRIP NOTCH HALF WIDTH'                       82
C STRIP#1-75 HALF WITDH                                                   83
      CALG_SETA1WDH=0.7277                                                83
      CCALG_SETA1WDH='STRIP#1-75 HALF WITDH'                              83
C STRIP#76-150 HALF WITDH                                                 84
      CALG_SETA2WDH=0.9398                                                84
      CCALG_SETA2WDH='STRIP#76-150 HALF WITDH'                            84
C HALF DISTANCE BETWEEN STRIPS IN ETA                                     85
      CALG_SET12WDH=0.04064                                               85
      CCALG_SET12WDH='HALF DISTANCE BETWEEN STRIPS IN ETA'                85
C STRIP#(1-NPHISTR) IN PHI DIRECTION HALF WITDH                           86
      CALG_SPHIWDH=0.6680                                                 86
      CCALG_SPHIWDH='STRIP#(1-NPHISTR) IN PHI DIRECTION HALF WITDH'       86
C HALF DISTANCE BETWEEN STRIPS IN PHI                                     87
      CALG_SPHIDWDH=0.07874                                               87
      CCALG_SPHIDWDH='HALF DISTANCE BETWEEN STRIPS IN PHI'                87
C NUMBER SMD GASEUS INTERVAL IN TILE                                      88
      CALG_NSMDALW=30                                                     88
      CCALG_NSMDALW='NUMBER SMD GASEUS INTERVAL IN TILE'                  88
C NUMBER OF READOUT SUPERLAYER                                            89
      CALG_NSUPER=2                                                       89
      CCALG_NSUPER='NUMBER OF READOUT SUPERLAYER'                         89
C SMD POSITIONED AFTER SANDVICH TYPE LAYERS EMC                           90
      CALG_NSMD=5                                                         90
      CCALG_NSMD='SMD POSITIONED AFTER SANDVICH TYPE LAYERS EMC'          90
C NUMBER OF LAYERS IN A SUPERLAYER                                        91
      CALG_NSUBLAY(1)=2                                                   91
      CALG_NSUBLAY(2)=19                                                  91
      CCALG_NSUBLAY='NUMBER OF LAYERS IN A SUPERLAYER'                    91
C MAX NUMBER OF MOUDLE                                                    92
      CALG_MAXMODULE=60                                                   92
      CCALG_MAXMODULE='MAX NUMBER OF MOUDLE'                              92
C NUMBER OF ETA DIVISION FOR TOWER/PRESHOWER                              93
      CALG_NETAT=20                                                       93
      CCALG_NETAT='NUMBER OF ETA DIVISION FOR TOWER/PRESHOWER'            93
C NUMBER OF SUB DIV. IN PHI FOR TOWER/PRESHOWER                           94
      CALG_NSUB=2                                                         94
      CCALG_NSUB='NUMBER OF SUB DIV. IN PHI FOR TOWER/PRESHOWER'          94
C NUMBER OF ETA DIVISION IN FOR SMD PHI PLANE                             95
      CALG_NETASMDP=10                                                    95
      CCALG_NETASMDP='NUMBER OF ETA DIVISION IN FOR SMD PHI PLANE'        95
C NUMBER OF THE STRIP IN PHI DIRECTION                                    96
      CALG_NPHISTR=15                                                     96
      CCALG_NPHISTR='NUMBER OF THE STRIP IN PHI DIRECTION'                96
C NUMBER OF STRIP IN FIRST PART ETA=0-0.5                                 97
      CALG_NETFIRST=75.                                                   97
      CCALG_NETFIRST='NUMBER OF STRIP IN FIRST PART ETA=0-0.5'            97
C NUMBER OF STRIP IN SECOND PART ETA=0.5-1.0                              98
      CALG_NETSECON=75.                                                   98
      CCALG_NETSECON='NUMBER OF STRIP IN SECOND PART ETA=0.5-1.0'         98
C NUMBER OF MODULES                                                       99
      CALG_NMODULE(1)=60                                                  99
      CALG_NMODULE(2)=60                                                  99
      CCALG_NMODULE='NUMBER OF MODULES'                                   99
C STARTING AZIMUTH OF THE FIRST MODULE                                    100
      CALG_SHIFT(1)=75                                                    100
      CALG_SHIFT(2)=105                                                   100
      CCALG_SHIFT='STARTING AZIMUTH OF THE FIRST MODULE'                  100
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALG',    101
     *BPATH_CALG,IDNUM_CALG,1, MAP_CALG,NAMES_CALG,COMMEN_CALG,           101
     *BANK_CALG,LEN_CALG,LLEN_CALG,LINK_CALG,FLAG_CALG)                   101
C *                                                                       101
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALG',    101
     *BPATH_CALG,IDNUM_CALG,1, MAP_CALG,NAMES_CALG,COMMEN_CALG,           101
     *BANK_CALG,LEN_CALG,LLEN_CALG,LINK_CALG,FLAG_CALG)                   101
C *                                                                       102
      CALL AGSBEGM(MODULE,IPRIN)                                          102
      IF (IPRIN.LT.0) RETURN                                              102
C Filling bank CCUT - CUT SELECTION                                       102
      BANK_TITLE='CUT SELECTION'                                          102
      BPATH_CCUT='CCUT*'                                                  102
      IDNUM_CCUT(1)=0                                                     103
C SELECTOR                                                                103
      CCUT_VERSION=1                                                      103
      CCCUT_VERSION='SELECTOR'                                            103
C ABSORBER CUTS                                                           104
      CCUT_ABSORBER=0                                                     104
      CCCUT_ABSORBER='ABSORBER CUTS'                                      104
C SENSITIVE CUTS                                                          105
      CCUT_SENSITIVE=0                                                    105
      CCCUT_SENSITIVE='SENSITIVE CUTS'                                    105
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CCUT',    106
     *BPATH_CCUT,IDNUM_CCUT,1, MAP_CCUT,NAMES_CCUT,COMMEN_CCUT,           106
     *BANK_CCUT,LEN_CCUT,LLEN_CCUT,LINK_CCUT,FLAG_CCUT)                   106
C *                                                                       106
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CCUT',    106
     *BPATH_CCUT,IDNUM_CCUT,1, MAP_CCUT,NAMES_CCUT,COMMEN_CCUT,           106
     *BANK_CCUT,LEN_CCUT,LLEN_CCUT,LINK_CCUT,FLAG_CCUT)                   106
C *                                                                       107
      CALL AGSBEGM(MODULE,IPRIN)                                          107
      IF (IPRIN.LT.0) RETURN                                              107
C Filling bank CABS - THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE O  107
C RIGINAL GEOMETRY                                                        107
      BANK_TITLE='THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE ORIGI  107
     *NAL GEOMETRY'                                                       107
      BPATH_CABS='CABS*'                                                  107
      IDNUM_CABS(1)=0                                                     108
C VERSIONING                                                              108
      CABS_VERSION=0                                                      108
      CCABS_VERSION='VERSIONING'                                          108
C GAMMA TRANSPORT CUT                                                     109
      CABS_CUTGAM=80 *1.0E-6                                              109
      CCABS_CUTGAM='GAMMA TRANSPORT CUT'                                  109
C ELECTRON TRANSPORT CUT                                                  110
      CABS_CUTELE=1 *1.0E-3                                               110
      CCABS_CUTELE='ELECTRON TRANSPORT CUT'                               110
C HADRON TRANSPORT CUT                                                    111
      CABS_CUTHAD=1 *1.0E-3                                               111
      CCABS_CUTHAD='HADRON TRANSPORT CUT'                                 111
C NEUTRON TRANSPORT CUT                                                   112
      CABS_CUTNEU=1 *1.0E-3                                               112
      CCABS_CUTNEU='NEUTRON TRANSPORT CUT'                                112
C MUON TRANSPORT CUT                                                      113
      CABS_CUTMUO=1 *1.0E-3                                               113
      CCABS_CUTMUO='MUON TRANSPORT CUT'                                   113
C ELECTRON DELTA RAY CUT                                                  114
      CABS_DCUTE=1 *1.0E-3                                                114
      CCABS_DCUTE='ELECTRON DELTA RAY CUT'                                114
C MUON DELTA RAY CUT                                                      115
      CABS_DCUTM=1 *1.0E-3                                                115
      CCABS_DCUTM='MUON DELTA RAY CUT'                                    115
C ELECTRON BREM CUT                                                       116
      CABS_BCUTE=100 *1.0E-6                                              116
      CCABS_BCUTE='ELECTRON BREM CUT'                                     116
C MUON BREM CUT                                                           117
      CABS_BCUTM=1 *1.0E-3                                                117
      CCABS_BCUTM='MUON BREM CUT'                                         117
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    118
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           118
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   118
C *                                                                       118
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    118
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           118
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   118
C *                                                                       119
      CALL AGSBEGM(MODULE,IPRIN)                                          119
      IF (IPRIN.LT.0) RETURN                                              119
C Filling bank CABS - EM CUTS IN ABSORBING MATERIAL                       119
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          119
      BPATH_CABS='CABS*'                                                  119
      IDNUM_CABS(1)=0                                                     120
C VERSIONING                                                              120
      CABS_VERSION=1                                                      120
      CCABS_VERSION='VERSIONING'                                          120
C GAMMA TRANSPORT CUT                                                     121
      CABS_CUTGAM=10 *1.0E-6                                              121
      CCABS_CUTGAM='GAMMA TRANSPORT CUT'                                  121
C ELECTRON TRANSPORT CUT                                                  122
      CABS_CUTELE=10 *1.0E-6                                              122
      CCABS_CUTELE='ELECTRON TRANSPORT CUT'                               122
C HADRON TRANSPORT CUT                                                    123
      CABS_CUTHAD=1 *1.0E-3                                               123
      CCABS_CUTHAD='HADRON TRANSPORT CUT'                                 123
C NEUTRON TRANSPORT CUT                                                   124
      CABS_CUTNEU=1 *1.0E-3                                               124
      CCABS_CUTNEU='NEUTRON TRANSPORT CUT'                                124
C MUON TRANSPORT CUT                                                      125
      CABS_CUTMUO=1 *1.0E-3                                               125
      CCABS_CUTMUO='MUON TRANSPORT CUT'                                   125
C ELECTRON DELTA RAY CUT                                                  126
      CABS_DCUTE=10 *1.0E-6                                               126
      CCABS_DCUTE='ELECTRON DELTA RAY CUT'                                126
C MUON DELTA RAY CUT                                                      127
      CABS_DCUTM=10 *1.0E-6                                               127
      CCABS_DCUTM='MUON DELTA RAY CUT'                                    127
C ELECTRON BREM CUT                                                       128
      CABS_BCUTE=10 *1.0E-6                                               128
      CCABS_BCUTE='ELECTRON BREM CUT'                                     128
C MUON BREM CUT                                                           129
      CABS_BCUTM=10 *1.0E-6                                               129
      CCABS_BCUTM='MUON BREM CUT'                                         129
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    130
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           130
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   130
C *                                                                       130
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    130
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           130
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   130
C *                                                                       131
      CALL AGSBEGM(MODULE,IPRIN)                                          131
      IF (IPRIN.LT.0) RETURN                                              131
C Filling bank CABS - EM CUTS IN ABSORBING MATERIAL                       131
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          131
      BPATH_CABS='CABS*'                                                  131
      IDNUM_CABS(1)=0                                                     132
C VERSIONING                                                              132
      CABS_VERSION=2                                                      132
      CCABS_VERSION='VERSIONING'                                          132
C GAMMA TRANSPORT CUT                                                     133
      CABS_CUTGAM=30 *1.0E-6                                              133
      CCABS_CUTGAM='GAMMA TRANSPORT CUT'                                  133
C ELECTRON TRANSPORT CUT                                                  134
      CABS_CUTELE=30 *1.0E-6                                              134
      CCABS_CUTELE='ELECTRON TRANSPORT CUT'                               134
C HADRON TRANSPORT CUT                                                    135
      CABS_CUTHAD=1 *1.0E-3                                               135
      CCABS_CUTHAD='HADRON TRANSPORT CUT'                                 135
C NEUTRON TRANSPORT CUT                                                   136
      CABS_CUTNEU=1 *1.0E-3                                               136
      CCABS_CUTNEU='NEUTRON TRANSPORT CUT'                                136
C MUON TRANSPORT CUT                                                      137
      CABS_CUTMUO=1 *1.0E-3                                               137
      CCABS_CUTMUO='MUON TRANSPORT CUT'                                   137
C ELECTRON DELTA RAY CUT                                                  138
      CABS_DCUTE=30 *1.0E-6                                               138
      CCABS_DCUTE='ELECTRON DELTA RAY CUT'                                138
C MUON DELTA RAY CUT                                                      139
      CABS_DCUTM=30 *1.0E-6                                               139
      CCABS_DCUTM='MUON DELTA RAY CUT'                                    139
C ELECTRON BREM CUT                                                       140
      CABS_BCUTE=30 *1.0E-6                                               140
      CCABS_BCUTE='ELECTRON BREM CUT'                                     140
C MUON BREM CUT                                                           141
      CABS_BCUTM=30 *1.0E-6                                               141
      CCABS_BCUTM='MUON BREM CUT'                                         141
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    142
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           142
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   142
C *                                                                       142
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    142
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           142
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   142
C *                                                                       143
      CALL AGSBEGM(MODULE,IPRIN)                                          143
      IF (IPRIN.LT.0) RETURN                                              143
C Filling bank CABS - EM CUTS IN ABSORBING MATERIAL                       143
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          143
      BPATH_CABS='CABS*'                                                  143
      IDNUM_CABS(1)=0                                                     144
C VERSIONING                                                              144
      CABS_VERSION=3                                                      144
      CCABS_VERSION='VERSIONING'                                          144
C GAMMA TRANSPORT CUT                                                     145
      CABS_CUTGAM=100 *1.0E-6                                             145
      CCABS_CUTGAM='GAMMA TRANSPORT CUT'                                  145
C ELECTRON TRANSPORT CUT                                                  146
      CABS_CUTELE=100 *1.0E-6                                             146
      CCABS_CUTELE='ELECTRON TRANSPORT CUT'                               146
C HADRON TRANSPORT CUT                                                    147
      CABS_CUTHAD=1 *1.0E-3                                               147
      CCABS_CUTHAD='HADRON TRANSPORT CUT'                                 147
C NEUTRON TRANSPORT CUT                                                   148
      CABS_CUTNEU=1 *1.0E-3                                               148
      CCABS_CUTNEU='NEUTRON TRANSPORT CUT'                                148
C MUON TRANSPORT CUT                                                      149
      CABS_CUTMUO=1 *1.0E-3                                               149
      CCABS_CUTMUO='MUON TRANSPORT CUT'                                   149
C ELECTRON DELTA RAY CUT                                                  150
      CABS_DCUTE=100 *1.0E-6                                              150
      CCABS_DCUTE='ELECTRON DELTA RAY CUT'                                150
C MUON DELTA RAY CUT                                                      151
      CABS_DCUTM=100 *1.0E-6                                              151
      CCABS_DCUTM='MUON DELTA RAY CUT'                                    151
C ELECTRON BREM CUT                                                       152
      CABS_BCUTE=100 *1.0E-6                                              152
      CCABS_BCUTE='ELECTRON BREM CUT'                                     152
C MUON BREM CUT                                                           153
      CABS_BCUTM=100 *1.0E-6                                              153
      CCABS_BCUTM='MUON BREM CUT'                                         153
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    154
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           154
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   154
C *                                                                       154
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    154
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           154
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   154
C *                                                                       155
      CALL AGSBEGM(MODULE,IPRIN)                                          155
      IF (IPRIN.LT.0) RETURN                                              155
C Filling bank CABS - EM CUTS IN ABSORBING MATERIAL                       155
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          155
      BPATH_CABS='CABS*'                                                  155
      IDNUM_CABS(1)=0                                                     156
C VERSIONING                                                              156
      CABS_VERSION=4                                                      156
      CCABS_VERSION='VERSIONING'                                          156
C GAMMA TRANSPORT CUT                                                     157
      CABS_CUTGAM=1 *1.0E-3                                               157
      CCABS_CUTGAM='GAMMA TRANSPORT CUT'                                  157
C ELECTRON TRANSPORT CUT                                                  158
      CABS_CUTELE=1 *1.0E-3                                               158
      CCABS_CUTELE='ELECTRON TRANSPORT CUT'                               158
C HADRON TRANSPORT CUT                                                    159
      CABS_CUTHAD=1 *1.0E-3                                               159
      CCABS_CUTHAD='HADRON TRANSPORT CUT'                                 159
C NEUTRON TRANSPORT CUT                                                   160
      CABS_CUTNEU=1 *1.0E-3                                               160
      CCABS_CUTNEU='NEUTRON TRANSPORT CUT'                                160
C MUON TRANSPORT CUT                                                      161
      CABS_CUTMUO=1 *1.0E-3                                               161
      CCABS_CUTMUO='MUON TRANSPORT CUT'                                   161
C ELECTRON DELTA RAY CUT                                                  162
      CABS_DCUTE=1 *1.0E-3                                                162
      CCABS_DCUTE='ELECTRON DELTA RAY CUT'                                162
C MUON DELTA RAY CUT                                                      163
      CABS_DCUTM=1 *1.0E-3                                                163
      CCABS_DCUTM='MUON DELTA RAY CUT'                                    163
C ELECTRON BREM CUT                                                       164
      CABS_BCUTE=1 *1.0E-3                                                164
      CCABS_BCUTE='ELECTRON BREM CUT'                                     164
C MUON BREM CUT                                                           165
      CABS_BCUTM=1 *1.0E-3                                                165
      CCABS_BCUTM='MUON BREM CUT'                                         165
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    166
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           166
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   166
C *                                                                       166
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CABS',    166
     *BPATH_CABS,IDNUM_CABS,1, MAP_CABS,NAMES_CABS,COMMEN_CABS,           166
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   166
C *                                                                       167
      CALL AGSBEGM(MODULE,IPRIN)                                          167
      IF (IPRIN.LT.0) RETURN                                              167
C Filling bank CSEN - THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE O  167
C RIGINAL GEOMETRY                                                        167
      BANK_TITLE='THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE ORIGI  167
     *NAL GEOMETRY'                                                       167
      BPATH_CSEN='CSEN*'                                                  167
      IDNUM_CSEN(1)=0                                                     168
C VERSIONING                                                              168
      CSEN_VERSION=0                                                      168
      CCSEN_VERSION='VERSIONING'                                          168
C GAMMA TRANSPORT CUT                                                     169
      CSEN_CUTGAM=80 *1.0E-6                                              169
      CCSEN_CUTGAM='GAMMA TRANSPORT CUT'                                  169
C ELECTRON TRANSPORT CUT                                                  170
      CSEN_CUTELE=1 *1.0E-3                                               170
      CCSEN_CUTELE='ELECTRON TRANSPORT CUT'                               170
C HADRON TRANSPORT CUT                                                    171
      CSEN_CUTHAD=1 *1.0E-3                                               171
      CCSEN_CUTHAD='HADRON TRANSPORT CUT'                                 171
C NEUTRON TRANSPORT CUT                                                   172
      CSEN_CUTNEU=1 *1.0E-3                                               172
      CCSEN_CUTNEU='NEUTRON TRANSPORT CUT'                                172
C MUON TRANSPORT CUT                                                      173
      CSEN_CUTMUO=1 *1.0E-3                                               173
      CCSEN_CUTMUO='MUON TRANSPORT CUT'                                   173
C ELECTRON DELTA RAY CUT                                                  174
      CSEN_DCUTE=1 *1.0E-3                                                174
      CCSEN_DCUTE='ELECTRON DELTA RAY CUT'                                174
C MUON DELTA RAY CUT                                                      175
      CSEN_DCUTM=1 *1.0E-3                                                175
      CCSEN_DCUTM='MUON DELTA RAY CUT'                                    175
C ELECTRON BREM CUT                                                       176
      CSEN_BCUTE=100 *1.0E-6                                              176
      CCSEN_BCUTE='ELECTRON BREM CUT'                                     176
C MUON BREM CUT                                                           177
      CSEN_BCUTM=1 *1.0E-3                                                177
      CCSEN_BCUTM='MUON BREM CUT'                                         177
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    178
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           178
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   178
C *                                                                       178
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    178
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           178
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   178
C *                                                                       179
      CALL AGSBEGM(MODULE,IPRIN)                                          179
      IF (IPRIN.LT.0) RETURN                                              179
C Filling bank CSEN - EM CUTS IN ABSORBING MATERIAL                       179
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          179
      BPATH_CSEN='CSEN*'                                                  179
      IDNUM_CSEN(1)=0                                                     180
C VERSIONING                                                              180
      CSEN_VERSION=1                                                      180
      CCSEN_VERSION='VERSIONING'                                          180
C GAMMA TRANSPORT CUT                                                     181
      CSEN_CUTGAM=10 *1.0E-6                                              181
      CCSEN_CUTGAM='GAMMA TRANSPORT CUT'                                  181
C ELECTRON TRANSPORT CUT                                                  182
      CSEN_CUTELE=10 *1.0E-6                                              182
      CCSEN_CUTELE='ELECTRON TRANSPORT CUT'                               182
C HADRON TRANSPORT CUT                                                    183
      CSEN_CUTHAD=1 *1.0E-3                                               183
      CCSEN_CUTHAD='HADRON TRANSPORT CUT'                                 183
C NEUTRON TRANSPORT CUT                                                   184
      CSEN_CUTNEU=1 *1.0E-3                                               184
      CCSEN_CUTNEU='NEUTRON TRANSPORT CUT'                                184
C MUON TRANSPORT CUT                                                      185
      CSEN_CUTMUO=1 *1.0E-3                                               185
      CCSEN_CUTMUO='MUON TRANSPORT CUT'                                   185
C ELECTRON DELTA RAY CUT                                                  186
      CSEN_DCUTE=10 *1.0E-6                                               186
      CCSEN_DCUTE='ELECTRON DELTA RAY CUT'                                186
C MUON DELTA RAY CUT                                                      187
      CSEN_DCUTM=10 *1.0E-6                                               187
      CCSEN_DCUTM='MUON DELTA RAY CUT'                                    187
C ELECTRON BREM CUT                                                       188
      CSEN_BCUTE=10 *1.0E-6                                               188
      CCSEN_BCUTE='ELECTRON BREM CUT'                                     188
C MUON BREM CUT                                                           189
      CSEN_BCUTM=10 *1.0E-6                                               189
      CCSEN_BCUTM='MUON BREM CUT'                                         189
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    190
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           190
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   190
C *                                                                       190
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    190
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           190
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   190
C *                                                                       191
      CALL AGSBEGM(MODULE,IPRIN)                                          191
      IF (IPRIN.LT.0) RETURN                                              191
C Filling bank CSEN - EM CUTS IN ABSORBING MATERIAL                       191
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          191
      BPATH_CSEN='CSEN*'                                                  191
      IDNUM_CSEN(1)=0                                                     192
C VERSIONING                                                              192
      CSEN_VERSION=2                                                      192
      CCSEN_VERSION='VERSIONING'                                          192
C GAMMA TRANSPORT CUT                                                     193
      CSEN_CUTGAM=30 *1.0E-6                                              193
      CCSEN_CUTGAM='GAMMA TRANSPORT CUT'                                  193
C ELECTRON TRANSPORT CUT                                                  194
      CSEN_CUTELE=30 *1.0E-6                                              194
      CCSEN_CUTELE='ELECTRON TRANSPORT CUT'                               194
C HADRON TRANSPORT CUT                                                    195
      CSEN_CUTHAD=1 *1.0E-3                                               195
      CCSEN_CUTHAD='HADRON TRANSPORT CUT'                                 195
C NEUTRON TRANSPORT CUT                                                   196
      CSEN_CUTNEU=1 *1.0E-3                                               196
      CCSEN_CUTNEU='NEUTRON TRANSPORT CUT'                                196
C MUON TRANSPORT CUT                                                      197
      CSEN_CUTMUO=1 *1.0E-3                                               197
      CCSEN_CUTMUO='MUON TRANSPORT CUT'                                   197
C ELECTRON DELTA RAY CUT                                                  198
      CSEN_DCUTE=30 *1.0E-6                                               198
      CCSEN_DCUTE='ELECTRON DELTA RAY CUT'                                198
C MUON DELTA RAY CUT                                                      199
      CSEN_DCUTM=30 *1.0E-6                                               199
      CCSEN_DCUTM='MUON DELTA RAY CUT'                                    199
C ELECTRON BREM CUT                                                       200
      CSEN_BCUTE=30 *1.0E-6                                               200
      CCSEN_BCUTE='ELECTRON BREM CUT'                                     200
C MUON BREM CUT                                                           201
      CSEN_BCUTM=30 *1.0E-6                                               201
      CCSEN_BCUTM='MUON BREM CUT'                                         201
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    202
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           202
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   202
C *                                                                       202
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    202
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           202
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   202
C *                                                                       203
      CALL AGSBEGM(MODULE,IPRIN)                                          203
      IF (IPRIN.LT.0) RETURN                                              203
C Filling bank CSEN - EM CUTS IN ABSORBING MATERIAL                       203
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          203
      BPATH_CSEN='CSEN*'                                                  203
      IDNUM_CSEN(1)=0                                                     204
C VERSIONING                                                              204
      CSEN_VERSION=3                                                      204
      CCSEN_VERSION='VERSIONING'                                          204
C GAMMA TRANSPORT CUT                                                     205
      CSEN_CUTGAM=100 *1.0E-6                                             205
      CCSEN_CUTGAM='GAMMA TRANSPORT CUT'                                  205
C ELECTRON TRANSPORT CUT                                                  206
      CSEN_CUTELE=100 *1.0E-6                                             206
      CCSEN_CUTELE='ELECTRON TRANSPORT CUT'                               206
C HADRON TRANSPORT CUT                                                    207
      CSEN_CUTHAD=1 *1.0E-3                                               207
      CCSEN_CUTHAD='HADRON TRANSPORT CUT'                                 207
C NEUTRON TRANSPORT CUT                                                   208
      CSEN_CUTNEU=1 *1.0E-3                                               208
      CCSEN_CUTNEU='NEUTRON TRANSPORT CUT'                                208
C MUON TRANSPORT CUT                                                      209
      CSEN_CUTMUO=1 *1.0E-3                                               209
      CCSEN_CUTMUO='MUON TRANSPORT CUT'                                   209
C ELECTRON DELTA RAY CUT                                                  210
      CSEN_DCUTE=100 *1.0E-6                                              210
      CCSEN_DCUTE='ELECTRON DELTA RAY CUT'                                210
C MUON DELTA RAY CUT                                                      211
      CSEN_DCUTM=100 *1.0E-6                                              211
      CCSEN_DCUTM='MUON DELTA RAY CUT'                                    211
C ELECTRON BREM CUT                                                       212
      CSEN_BCUTE=100 *1.0E-6                                              212
      CCSEN_BCUTE='ELECTRON BREM CUT'                                     212
C MUON BREM CUT                                                           213
      CSEN_BCUTM=100 *1.0E-6                                              213
      CCSEN_BCUTM='MUON BREM CUT'                                         213
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    214
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           214
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   214
C *                                                                       214
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    214
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           214
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   214
C *                                                                       215
      CALL AGSBEGM(MODULE,IPRIN)                                          215
      IF (IPRIN.LT.0) RETURN                                              215
C Filling bank CSEN - EM CUTS IN ABSORBING MATERIAL                       215
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          215
      BPATH_CSEN='CSEN*'                                                  215
      IDNUM_CSEN(1)=0                                                     216
C VERSIONING                                                              216
      CSEN_VERSION=4                                                      216
      CCSEN_VERSION='VERSIONING'                                          216
C GAMMA TRANSPORT CUT                                                     217
      CSEN_CUTGAM=1 *1.0E-3                                               217
      CCSEN_CUTGAM='GAMMA TRANSPORT CUT'                                  217
C ELECTRON TRANSPORT CUT                                                  218
      CSEN_CUTELE=1 *1.0E-3                                               218
      CCSEN_CUTELE='ELECTRON TRANSPORT CUT'                               218
C HADRON TRANSPORT CUT                                                    219
      CSEN_CUTHAD=1 *1.0E-3                                               219
      CCSEN_CUTHAD='HADRON TRANSPORT CUT'                                 219
C NEUTRON TRANSPORT CUT                                                   220
      CSEN_CUTNEU=1 *1.0E-3                                               220
      CCSEN_CUTNEU='NEUTRON TRANSPORT CUT'                                220
C MUON TRANSPORT CUT                                                      221
      CSEN_CUTMUO=1 *1.0E-3                                               221
      CCSEN_CUTMUO='MUON TRANSPORT CUT'                                   221
C ELECTRON DELTA RAY CUT                                                  222
      CSEN_DCUTE=1 *1.0E-3                                                222
      CCSEN_DCUTE='ELECTRON DELTA RAY CUT'                                222
C MUON DELTA RAY CUT                                                      223
      CSEN_DCUTM=1 *1.0E-3                                                223
      CCSEN_DCUTM='MUON DELTA RAY CUT'                                    223
C ELECTRON BREM CUT                                                       224
      CSEN_BCUTE=1 *1.0E-3                                                224
      CCSEN_BCUTE='ELECTRON BREM CUT'                                     224
C MUON BREM CUT                                                           225
      CSEN_BCUTM=1 *1.0E-3                                                225
      CCSEN_BCUTM='MUON BREM CUT'                                         225
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    226
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           226
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   226
C *                                                                       226
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CSEN',    226
     *BPATH_CSEN,IDNUM_CSEN,1, MAP_CSEN,NAMES_CSEN,COMMEN_CSEN,           226
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   226
C use default CALG bank                                                   227
      BPATH_CALG='CALG*'                                                  227
      IDNUM_CALG(1)=0                                                     227
      CALL AGDGETP('CALBGEO2','*',-999,'CALG',' ',BANK_CALG, BPATH_CALG,  227
     *IDNUM_CALG,1,MAP_CALG,NAMES_CALG,BANK_CALG,LEN_CALG,LLEN_CALG,      227
     *LINK_CALG,FLAG_CALG)                                                227
C use CCUT bank                                                           228
      BPATH_CCUT='CCUT*'                                                  228
      IDNUM_CCUT(1)=0                                                     228
      CCUT_VERSION=1                                                      228
      CALL AGDGETP('CALBGEO2','*',-999,'CCUT',NCCUT_VERSION,              228
     *RE_CCUT_VERSION,BPATH_CCUT,IDNUM_CCUT,1,MAP_CCUT,NAMES_CCUT,        228
     *BANK_CCUT,LEN_CCUT,LLEN_CCUT,LINK_CCUT,FLAG_CCUT)                   228
C use CABS bank                                                           229
      BPATH_CABS='CABS*'                                                  229
      IDNUM_CABS(1)=0                                                     229
      CABS_VERSION=CCUT_ABSORBER                                          229
      CALL AGDGETP('CALBGEO2','*',-999,'CABS',NCABS_VERSION,              229
     *RE_CABS_VERSION,BPATH_CABS,IDNUM_CABS,1,MAP_CABS,NAMES_CABS,        229
     *BANK_CABS,LEN_CABS,LLEN_CABS,LINK_CABS,FLAG_CABS)                   229
C use CSEN bank                                                           230
      BPATH_CSEN='CSEN*'                                                  230
      IDNUM_CSEN(1)=0                                                     230
      CSEN_VERSION=CCUT_SENSITIVE                                         230
      CALL AGDGETP('CALBGEO2','*',-999,'CSEN',NCSEN_VERSION,              230
     *RE_CSEN_VERSION,BPATH_CSEN,IDNUM_CSEN,1,MAP_CSEN,NAMES_CSEN,        230
     *BANK_CSEN,LEN_CSEN,LLEN_CSEN,LINK_CSEN,FLAG_CSEN)                   230
C THICKNESS OF SS PLATE ETA=0                                             231
      ZZ0=0.477                                                           231
C THICKNESS OF TOTAL GAP ETA=0                                            232
      ZZ1=0.692                                                           232
C THICKNESS OF SS PLATE ETA=1                                             233
      ZZ2=0.79375                                                         233
C NEEDS TO BE SET ELSEWHERE IN PROGRAM                                    234
      CALG_ETACUT=0.9835                                                  234
      SMD_WIDTH=2.*CALG_G10SBTHK+2.*CALG_SMALFTHK+2.*CALG_ABPAPTHK        235
      SMD_WIDTH1=2.*CALG_G10SBTHK+2.*CALG_ABPAPTHK                        236
      SMD_WIDTH2=SMD_WIDTH1+CALG_SMGASTHK+CALG_SMGASRAD                   237
      SMD_WIDTH3=2.*SMD_WIDTH-SMD_WIDTH1-CALG_SMGASTHK-CALG_SMGASRAD      238
      R1=CALG_RMIN+2.*CALG_FRONTTHK                                       239
      R2=0.0                                                              240
C *                                                                       241
      DO 5011 I=1,NINT(CALG_NSUPER)                                       241
         LAYER_WIDTH(I) = CALG_SCINTTHK(I) +                              242
     *   CALG_ABSORTHK+2.*CALG_ABPAPTHK                                   242
         R2=R2+(CALG_NSUBLAY(I)-I+1)*LAYER_WIDTH(I)*2.0                   243
         RR(I)=R2                                                         244
5011  CONTINUE                                                            245
5012  CONTINUE                                                            245
      R3=                                                                 246
     *(CALG_NSUPER*LAYER_WIDTH(1)+(CALG_NSMD-CALG_NSUPER)*LAYER_WIDTH(2)  246
     *)*2.                                                                246
      R4=(SMD_WIDTH+CALG_SCINTTHK(2)+2.*CALG_ABPAPTHK)*2.0                247
      CUT_RADIUS=R1+R2+R4                                                 248
      RMAX=                                                               249
     *CUT_RADIUS+2.*(CALG_BACKTHK+CALG_SPACETHK+CALG_COMPTHK+             249
     *CALG_AIRTHK)                                                        249
      ANGLE = 2*ATAN(EXP(-CALG_ETACUT))*180./3.14159                      250
      TAN_THETA = TAN(2*ATAN(EXP(-CALG_ETACUT)))                          251
      SIN_THETA = SIN(2*ATAN(EXP(-CALG_ETACUT)))                          252
      CUT_LENGTH = CALG_RMIN/TAN_THETA                                    253
      HLENG = CUT_RADIUS/TAN_THETA                                        254
      CUT_LENGTH2 = CUT_LENGTH + ZZ2/SIN_THETA                            255
      HLENG2 = HLENG + ZZ2/SIN_THETA                                      256
      NN = MAX(CALG_NMODULE(1),CALG_NMODULE(2))                           257
      DETA = 1.0/CALG_NETAT                                               258
      DPHIMOD = 360/CALG_MAXMODULE                                        259
      DPHIT = DPHIMOD/CALG_NSUB                                           260
      DPHITOT = DPHIMOD*NN                                                261
C THICKNESS OF ALUMINUM COVER                                             262
      SKINTHK = 0.040                                                     262
C CONVERT TO DPHI                                                         263
      SKINTHK = (SKINTHK/RMAX)*180./3.14159                               263
C CONVERT TO DPHI                                                         264
      SKINOFFSET = (0.250/RMAX)*180./3.14159                              264
      SKINOFFSET1= -3+SKINOFFSET                                          265
      SKINOFFSET2= +3-SKINOFFSET                                          266
      RMAX2 = CUT_RADIUS +2.*0.9525+ 0.291                                267
      RMIN2 = CALG_RMIN + 0.291                                           268
      CUT_RADIUS2= CUT_RADIUS +0.291                                      269
C 15 PLACES ALONG MODULE Z0=6.5295                                        270
      STRAP_SPACING = 17.06                                               270
C HALF WIDTH OF SLUG                                                      271
      SLUG_DX = 0.9525/2                                                  271
C HALF WIDTH OF SLUG                                                      272
      SLUG_DY = 3.1750/2                                                  272
C HALF WIDTH OF SLUG                                                      273
      SLUG_DZ = 3.1750/2                                                  273
C HALF WIDTH OF STRAP                                                     274
      STRAP_DX = (RMAX2-RMIN2)/2                                          274
C HALF WIDTH OF STRAP                                                     275
      STRAP_DY = 0.2/2                                                    275
C HALF WIDTH OF STRAP                                                     276
      STRAP_DZ = 3.175/2                                                  276
      STRAP_R = (RMIN2+RMAX2)/2 -0.291                                    277
C *                                                                       278
      CALL AGSBEGM(MODULE,IPRIN)                                          278
      IF (IPRIN.LT.0) RETURN                                              278
C Filling bank CALR - BARREL EMC RADIUSES                                 278
      BANK_TITLE='BARREL EMC RADIUSES'                                    278
      BPATH_CALR='CALR*'                                                  278
      IDNUM_CALR(1)=0                                                     279
C INNER RADUIS OF SENSITIVE AREA                                          279
      CALR_RMIN=R1                                                        279
      CCALR_RMIN='INNER RADUIS OF SENSITIVE AREA'                         279
C MEAN RADUIS OF PRS                                                      280
      CALR_RPRS=R1+RR(1)/2.0                                              280
      CCALR_RPRS='MEAN RADUIS OF PRS'                                     280
C MEAN RADUIS OF SMD                                                      281
      CALR_RSMD1=R1+R3+SMD_WIDTH2                                         281
      CCALR_RSMD1='MEAN RADUIS OF SMD'                                    281
C MEAN RADUIS OF SMD                                                      282
      CALR_RSMD2=R1+R3+SMD_WIDTH3                                         282
      CCALR_RSMD2='MEAN RADUIS OF SMD'                                    282
C OUTER RADUIS OF SENSITIVE AREA                                          283
      CALR_RMAX=CUT_RADIUS                                                283
      CCALR_RMAX='OUTER RADUIS OF SENSITIVE AREA'                         283
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALR',    284
     *BPATH_CALR,IDNUM_CALR,1, MAP_CALR,NAMES_CALR,COMMEN_CALR,           284
     *BANK_CALR,LEN_CALR,LLEN_CALR,LINK_CALR,FLAG_CALR)                   284
C *                                                                       284
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALR',    284
     *BPATH_CALR,IDNUM_CALR,1, MAP_CALR,NAMES_CALR,COMMEN_CALR,           284
     *BANK_CALR,LEN_CALR,LLEN_CALR,LINK_CALR,FLAG_CALR)                   284
      RMAX3 = RMAX/COS(3.14159*DPHIMOD/360)                               285
C use default CALR bank                                                   286
      BPATH_CALR='CALR*'                                                  286
      IDNUM_CALR(1)=0                                                     286
      CALL AGDGETP('CALBGEO2','*',-999,'CALR',' ',BANK_CALR, BPATH_CALR,  286
     *IDNUM_CALR,1,MAP_CALR,NAMES_CALR,BANK_CALR,LEN_CALR,LLEN_CALR,      286
     *LINK_CALR,FLAG_CALR)                                                286
      CUTANG = ATAN2(CUT_RADIUS-CALG_RMIN,HLENG2-CUT_LENGTH2)             287
      CALL AGSBEGM(MODULE,IPRIN)                                          288
C Check IPRIN<0                                                           288
      IF (IPRIN.LT.0) RETURN                                              288
C Create CALB :                                                           288
C Call To Local Procedure CALB                                            288
      ASSIGN 5030 TO BLOCK_CALB                                           288
      GO TO  5020                                                         288
5030  CONTINUE                                                            288
C *                                                                       288
C Geant POSITION : CALB ===> MOTHER='CAVE' <===                           289
      CALL AGSRESET                                                       289
      AG_MOTHER = 'CAVE'                                                  289
      AG_TITLE = 'CALB'                                                   289
      AG_EXNAME = 'POSITION'                                              289
      AG_PARLIST = 'MOTHER'                                               289
      CALL AXPOSITION                                                     289
C *                                                                       289
C Check IPRIN>=1                                                          289
      IF (IPRIN.GE.1) WRITE(LOUT,5040)CALG_VERSION                        289
5040  FORMAT(' CALB geo. version =',F7.1)                                 291
C ----------------------------------------------------------------------  292
C ------------ CALB                                                       292
      CALL AGSENDM                                                        293
      RETURN                                                              293
C Begin Local Procedure CALB                                              293
C CALB IS EMC BARREL ENVELOPE                                             293
5020  CONTINUE                                                            293
         AG_TITLE='CALB'                                                  293
         AG_MODULE=MODULE                                                 293
         AG_MARK='V00'                                                    293
         CALL AGSPUSH                                                     293
         AG_MODULE=MODULE                                                 293
C    *                                                                    293
C    Geant MATERIAL : AIR ===> inherited arguments used <===              294
         CALL AGSRESET                                                    294
         AG_TITLE = 'AIR'                                                 294
         AG_EXNAME = 'MATERIAL'                                           294
         AG_PARLIST = 'NONE'                                              294
         CALL AXMATERIAL                                                  294
C    *                                                                    294
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           294
         CALL AGSRESET                                                    295
         AG_TITLE = 'STANDARD'                                            295
         AG_EXNAME = 'MEDIUM'                                             295
         AG_PARLIST = 'NONE'                                              295
         CALL AXMEDIUM                                                    295
C    *                                                                    295
C    Geant ATTRIBUTE : CALB ===> SEEN=0 COLO=7 <===                       295
         CALL AGSRESET                                                    296
         AG_SEEN = 0                                                      296
         AG_COLO = 7                                                      296
         AG_TITLE = 'CALB'                                                296
         AG_EXNAME = 'ATTRIBUTE'                                          296
         AG_PARLIST = 'SEEN_COLO'                                         296
         CALL AXATTRIBUTE                                                 296
C    *                                                                    296
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={-HLENG2, -CUT_LENG  296
C    TH2,CUT_LENGTH2,HLENG2} RMN={CUT_RADIUS, CALG_RMIN,CALG_RMIN,CUT_RA  296
C    DIUS} RMX={RMAX3, RMAX3,RMAX3,RMAX3} <===                            296
         CALL AGSRESET                                                    300
         AG_PHI1 = 0                                                      300
         AG_DPHI = 360                                                    300
         AG_NZ = 4                                                        300
         AG_ZI(1)=-HLENG2                                                 300
         AG_ZI(2)=-CUT_LENGTH2                                            300
         AG_ZI(3)=CUT_LENGTH2                                             300
         AG_ZI(4)=HLENG2                                                  300
         AG_RMN(1)=CUT_RADIUS                                             300
         AG_RMN(2)=CALG_RMIN                                              300
         AG_RMN(3)=CALG_RMIN                                              300
         AG_RMN(4)=CUT_RADIUS                                             300
         AG_RMX(1)=RMAX3                                                  300
         AG_RMX(2)=RMAX3                                                  300
         AG_RMX(3)=RMAX3                                                  300
         AG_RMX(4)=RMAX3                                                  300
         AG_TITLE = 'PCON'                                                300
         AG_EXNAME = 'SHAPE'                                              300
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           300
         CALL AXSHAPE                                                     300
C    *                                                                    300
         II=1                                                             300
C    Create CHLV :                                                        302
C    Call To Local Procedure CHLV                                         302
         ASSIGN 5060 TO BLOCK_CHLV                                        302
         GO TO  5050                                                      302
5060     CONTINUE                                                         302
C    *                                                                    302
C    Geant POSITION : CHLV ===> inherited arguments used <===             303
         CALL AGSRESET                                                    303
         AG_TITLE = 'CHLV'                                                303
         AG_EXNAME = 'POSITION'                                           303
         AG_PARLIST = 'NONE'                                              303
         CALL AXPOSITION                                                  303
C    *                                                                    303
         II=2                                                             303
C    Create CHLV :                                                        305
C    Call To Local Procedure CHLV                                         305
         ASSIGN 5070 TO BLOCK_CHLV                                        305
         GO TO  5050                                                      305
5070     CONTINUE                                                         305
C    *                                                                    305
C    Geant POSITION : CHLV ===> THETAZ=180 <===                           306
         CALL AGSRESET                                                    306
         AG_THETAZ = 180                                                  306
         AG_TITLE = 'CHLV'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'THETAZ'                                            306
         CALL AXPOSITION                                                  306
C    *                                                                    306
5021  CALL AGSPOPB                                                        307
      GO TO BLOCK_CALB, (5020 ,5030)                                      307
C End of Local Procedure                                                  307
C ----------------------------------------------------------------------  308
C ------------ CHLV                                                       308
C Begin Local Procedure CHLV                                              309
C CHLV CORRESPONDS TO DOUBLE MODULES...                                   309
5050  CONTINUE                                                            309
         AG_TITLE='CHLV'                                                  309
         AG_MODULE=MODULE                                                 309
         AG_MARK='V00'                                                    309
         CALL AGSPUSH                                                     309
         AG_MODULE=MODULE                                                 309
C    *                                                                    309
C    Geant SHAPE : PCON ===> PHI1=CALG_SHIFT(II) DPHI=DPHIMOD*CALG_NMODU  313
C    LE(II), NZ=3 ZI={0,CUT_LENGTH2,HLENG2}, RMN={CALG_RMIN,CALG_RMIN,CU  313
C    T_RADIUS} RMX={RMAX3, RMAX3,RMAX3} <===                              313
         CALL AGSRESET                                                    313
         AG_PHI1 = CALG_SHIFT(II)                                         313
         AG_DPHI = DPHIMOD*CALG_NMODULE(II)                               313
         AG_NZ = 3                                                        313
         AG_ZI(1)=0                                                       313
         AG_ZI(2)=CUT_LENGTH2                                             313
         AG_ZI(3)=HLENG2                                                  313
         AG_RMN(1)=CALG_RMIN                                              313
         AG_RMN(2)=CALG_RMIN                                              313
         AG_RMN(3)=CUT_RADIUS                                             313
         AG_RMX(1)=RMAX3                                                  313
         AG_RMX(2)=RMAX3                                                  313
         AG_RMX(3)=RMAX3                                                  313
         AG_TITLE = 'PCON'                                                313
         AG_EXNAME = 'SHAPE'                                              313
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           313
         CALL AXSHAPE                                                     313
C    *                                                                    313
C    Create CPHI :                                                        314
C    Call To Local Procedure CPHI                                         314
         ASSIGN 5090 TO BLOCK_CPHI                                        314
         GO TO  5080                                                      314
5090     CONTINUE                                                         314
C    *                                                                    314
C    *                                                                    315
         IMOD=1                                                           315
            GO TO 5103                                                    315
5101        IMOD=IMOD+1                                                   315
5103        IF(IMOD-(CALG_NMODULE(II)).GT.0) GO TO 5102                   315
            ANGULAR_OFFSET=CALG_SHIFT(II)+3.0+6.0*(IMOD-1)                316
C       Geant POSITION : CPHI ===> ALPHAZ=ANGULAR_OFFSET NCOPY=IMOD <===  317
            CALL AGSRESET                                                 317
            AG_ALPHAN=AG_ALPHAN+1                                         317
            AG_ALPHAK(AG_ALPHAN)=3                                        317
            AG_ALPHAL(AG_ALPHAN)=ANGULAR_OFFSET                           317
            AG_NCOPY = IMOD                                               317
            AG_TITLE = 'CPHI'                                             317
            AG_EXNAME = 'POSITION'                                        317
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   317
            CALL AXPOSITION                                               317
C       *                                                                 317
         GO TO 5101                                                       318
5102     CONTINUE                                                         318
5051  CALL AGSPOPB                                                        319
      GO TO BLOCK_CHLV, (5050 ,5060,5070)                                 319
C End of Local Procedure                                                  319
C ----------------------------------------------------------------------  320
C ------------ CPHI                                                       320
C Begin Local Procedure CPHI                                              321
C CPHI CORRESPONDS TO A SINGLE MODULE                                     321
5080  CONTINUE                                                            321
         AG_TITLE='CPHI'                                                  321
         AG_MODULE=MODULE                                                 321
         AG_MARK='V00'                                                    321
         CALL AGSPUSH                                                     321
         AG_MODULE=MODULE                                                 321
C    *                                                                    321
C    Geant ATTRIBUTE : CPHI ===> SEEN=1 COLO=5 <===                       322
         CALL AGSRESET                                                    322
         AG_SEEN = 1                                                      322
         AG_COLO = 5                                                      322
         AG_TITLE = 'CPHI'                                                322
         AG_EXNAME = 'ATTRIBUTE'                                          322
         AG_PARLIST = 'SEEN_COLO'                                         322
         CALL AXATTRIBUTE                                                 322
C    *                                                                    322
C    Geant SHAPE : PCON ===> PHI1=-DPHIMOD/2 DPHI=DPHIMOD, NZ=3 ZI={0,CU  322
C    T_LENGTH2,HLENG2} RMN={CALG_RMIN, CALG_RMIN,CUT_RADIUS} RMX={RMAX3,  322
C    RMAX3,RMAX3} <===                                                    322
         CALL AGSRESET                                                    325
         AG_PHI1 = -DPHIMOD/2                                             325
         AG_DPHI = DPHIMOD                                                325
         AG_NZ = 3                                                        325
         AG_ZI(1)=0                                                       325
         AG_ZI(2)=CUT_LENGTH2                                             325
         AG_ZI(3)=HLENG2                                                  325
         AG_RMN(1)=CALG_RMIN                                              325
         AG_RMN(2)=CALG_RMIN                                              325
         AG_RMN(3)=CUT_RADIUS                                             325
         AG_RMX(1)=RMAX3                                                  325
         AG_RMX(2)=RMAX3                                                  325
         AG_RMX(3)=RMAX3                                                  325
         AG_TITLE = 'PCON'                                                325
         AG_EXNAME = 'SHAPE'                                              325
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           325
         CALL AXSHAPE                                                     325
C    *                                                                    325
C    Create CSZO :                                                        326
C    Call To Local Procedure CSZO                                         326
         ASSIGN 5120 TO BLOCK_CSZO                                        326
         GO TO  5110                                                      326
5120     CONTINUE                                                         326
C    *                                                                    326
C    Geant POSITION : CSZO ===> inherited arguments used <===             327
         CALL AGSRESET                                                    327
         AG_TITLE = 'CSZO'                                                327
         AG_EXNAME = 'POSITION'                                           327
         AG_PARLIST = 'NONE'                                              327
         CALL AXPOSITION                                                  327
C    *                                                                    327
C    Create CSZU :                                                        328
C    Call To Local Procedure CSZU                                         328
         ASSIGN 5140 TO BLOCK_CSZU                                        328
         GO TO  5130                                                      328
5140     CONTINUE                                                         328
C    *                                                                    328
C    Geant POSITION : CSZU ===> inherited arguments used <===             329
         CALL AGSRESET                                                    329
         AG_TITLE = 'CSZU'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'NONE'                                              329
         CALL AXPOSITION                                                  329
C    *                                                                    329
C    Create CSPT :                                                        330
C    Call To Local Procedure CSPT                                         330
         ASSIGN 5160 TO BLOCK_CSPT                                        330
         GO TO  5150                                                      330
5160     CONTINUE                                                         330
C    *                                                                    330
C    Geant POSITION : CSPT ===> inherited arguments used <===             331
         CALL AGSRESET                                                    331
         AG_TITLE = 'CSPT'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'NONE'                                              331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Create CSPB :                                                        332
C    Call To Local Procedure CSPB                                         332
         ASSIGN 5180 TO BLOCK_CSPB                                        332
         GO TO  5170                                                      332
5180     CONTINUE                                                         332
C    *                                                                    332
C    Geant POSITION : CSPB ===> inherited arguments used <===             333
         CALL AGSRESET                                                    333
         AG_TITLE = 'CSPB'                                                333
         AG_EXNAME = 'POSITION'                                           333
         AG_PARLIST = 'NONE'                                              333
         CALL AXPOSITION                                                  333
C    *                                                                    333
         CURRENT_DEPTH = CALG_RMIN                                        333
         C_DEP=CURRENT_DEPTH                                              335
         AG_DX = CALG_FRONTTHK                                            336
C    Create CBTW :                                                        336
C    Call To Local Procedure CBTW                                         336
         ASSIGN 5200 TO BLOCK_CBTW                                        336
         GO TO  5190                                                      336
5200     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : CBTW ===> X=CALG_RMIN+CALG_FRONTTHK Z=CURRENT_DEPT  337
C    H/TAN_THETA/2+ZZ0/2 <===                                             337
         CALL AGSRESET                                                    337
         AG_X = CALG_RMIN+CALG_FRONTTHK                                   337
         AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ0/2                           337
         AG_TITLE = 'CBTW'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'X_Z'                                               337
         CALL AXPOSITION                                                  337
C    *                                                                    337
C    *                                                                    337
         NSLUG=0                                                          338
            GO TO 5213                                                    338
5211        NSLUG=NSLUG+1                                                 338
5213        IF(NSLUG-(13).GT.0) GO TO 5212                                338
C       Create CSLG :                                                     339
C       Call To Local Procedure CSLG                                      339
            ASSIGN 5230 TO BLOCK_CSLG                                     339
            GO TO  5220                                                   339
5230        CONTINUE                                                      339
C       *                                                                 339
C       Geant POSITION : CSLG ===> X=CALG_RMIN+SLUG_DX Y=CURRENT_DEPTH*T  342
C       AN(TWOPI/360*DPHIT)-CALG_CRACKWD-SLUG_DY Z=16.5849+NSLUG*STRAP_S  342
C       PACING+ZZ0 <===                                                   342
            CALL AGSRESET                                                 342
            AG_X = CALG_RMIN+SLUG_DX                                      342
            AG_Y = CURRENT_DEPTH*TAN(TWOPI/                               342
     *      360*DPHIT)-CALG_CRACKWD-SLUG_DY                               342
            AG_Z = 16.5849+NSLUG*STRAP_SPACING+ZZ0                        342
            AG_TITLE = 'CSLG'                                             342
            AG_EXNAME = 'POSITION'                                        342
            AG_PARLIST = 'X_Y_Z'                                          342
            CALL AXPOSITION                                               342
C       *                                                                 342
C       Create CSLG :                                                     343
C       Call To Local Procedure CSLG                                      343
            ASSIGN 5240 TO BLOCK_CSLG                                     343
            GO TO  5220                                                   343
5240        CONTINUE                                                      343
C       *                                                                 343
C       Geant POSITION : CSLG ===> X=CALG_RMIN+SLUG_DX Y=-1*CURRENT_DEPT  346
C       H*TAN(TWOPI/360*DPHIT)+CALG_CRACKWD+SLUG_DY Z=16.5849+NSLUG*STRA  346
C       P_SPACING+ZZ0 <===                                                346
            CALL AGSRESET                                                 346
            AG_X = CALG_RMIN+SLUG_DX                                      346
            AG_Y = -1*CURRENT_DEPTH*TAN(TWOPI/                            346
     *      360*DPHIT)+CALG_CRACKWD+SLUG_DY                               346
            AG_Z = 16.5849+NSLUG*STRAP_SPACING+ZZ0                        346
            AG_TITLE = 'CSLG'                                             346
            AG_EXNAME = 'POSITION'                                        346
            AG_PARLIST = 'X_Y_Z'                                          346
            CALL AXPOSITION                                               346
C       *                                                                 346
C       Create CSTP :                                                     347
C       Call To Local Procedure CSTP                                      347
            ASSIGN 5260 TO BLOCK_CSTP                                     347
            GO TO  5250                                                   347
5260        CONTINUE                                                      347
C       *                                                                 347
C       Geant POSITION : CSTP ===> X=STRAP_R Y=+1*STRAP_R*TAN(TWOPI/360*  349
C       DPHIT)-CALG_CRACKWD+STRAP_DY Z=16.5849+NSLUG*STRAP_SPACING+ZZ0 A  349
C       LPHAZ=+3 <===                                                     349
            CALL AGSRESET                                                 349
            AG_X = STRAP_R                                                349
            AG_Y = +1*STRAP_R*TAN(TWOPI/                                  349
     *      360*DPHIT)-CALG_CRACKWD+STRAP_DY                              349
            AG_Z = 16.5849+NSLUG*STRAP_SPACING+ZZ0                        349
            AG_ALPHAN=AG_ALPHAN+1                                         349
            AG_ALPHAK(AG_ALPHAN)=3                                        349
            AG_ALPHAL(AG_ALPHAN)=+3                                       349
            AG_TITLE = 'CSTP'                                             349
            AG_EXNAME = 'POSITION'                                        349
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   349
            CALL AXPOSITION                                               349
C       *                                                                 349
C       Create CSTP :                                                     350
C       Call To Local Procedure CSTP                                      350
            ASSIGN 5270 TO BLOCK_CSTP                                     350
            GO TO  5250                                                   350
5270        CONTINUE                                                      350
C       *                                                                 350
C       Geant POSITION : CSTP ===> X=STRAP_R Y=-1*STRAP_R*TAN(TWOPI/360*  352
C       DPHIT)+CALG_CRACKWD-STRAP_DY Z=16.5849+NSLUG*STRAP_SPACING+ZZ0 A  352
C       LPHAZ=-3 <===                                                     352
            CALL AGSRESET                                                 352
            AG_X = STRAP_R                                                352
            AG_Y = -1*STRAP_R*TAN(TWOPI/                                  352
     *      360*DPHIT)+CALG_CRACKWD-STRAP_DY                              352
            AG_Z = 16.5849+NSLUG*STRAP_SPACING+ZZ0                        352
            AG_ALPHAN=AG_ALPHAN+1                                         352
            AG_ALPHAK(AG_ALPHAN)=3                                        352
            AG_ALPHAL(AG_ALPHAN)=-3                                       352
            AG_TITLE = 'CSTP'                                             352
            AG_EXNAME = 'POSITION'                                        352
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   352
            CALL AXPOSITION                                               352
C       *                                                                 352
         GO TO 5211                                                       353
5212     CONTINUE                                                         353
C    Create CSLG :                                                        354
C    Call To Local Procedure CSLG                                         354
         ASSIGN 5280 TO BLOCK_CSLG                                        354
         GO TO  5220                                                      354
5280     CONTINUE                                                         354
C    *                                                                    354
C    Geant POSITION : CSLG ===> X=CALG_RMIN+SLUG_DX Y=CURRENT_DEPTH*TAN(  357
C    TWOPI/360*DPHIT)-CALG_CRACKWD-SLUG_DY Z=CURRENT_DEPTH/TAN_THETA-ZZ0  357
C    -3.679 <===                                                          357
         CALL AGSRESET                                                    357
         AG_X = CALG_RMIN+SLUG_DX                                         357
         AG_Y = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD-SLUG_DY   357
         AG_Z = CURRENT_DEPTH/TAN_THETA-ZZ0-3.679                         357
         AG_TITLE = 'CSLG'                                                357
         AG_EXNAME = 'POSITION'                                           357
         AG_PARLIST = 'X_Y_Z'                                             357
         CALL AXPOSITION                                                  357
C    *                                                                    357
C    Create CSLG :                                                        358
C    Call To Local Procedure CSLG                                         358
         ASSIGN 5290 TO BLOCK_CSLG                                        358
         GO TO  5220                                                      358
5290     CONTINUE                                                         358
C    *                                                                    358
C    Geant POSITION : CSLG ===> X=CALG_RMIN+SLUG_DX Y=-1*CURRENT_DEPTH*T  361
C    AN(TWOPI/360*DPHIT)+CALG_CRACKWD+SLUG_DY Z=CURRENT_DEPTH/TAN_THETA-  361
C    ZZ0-3.679 <===                                                       361
         CALL AGSRESET                                                    361
         AG_X = CALG_RMIN+SLUG_DX                                         361
         AG_Y = -1*CURRENT_DEPTH*TAN(TWOPI/                               361
     *   360*DPHIT)+CALG_CRACKWD+SLUG_DY                                  361
         AG_Z = CURRENT_DEPTH/TAN_THETA-ZZ0-3.679                         361
         AG_TITLE = 'CSLG'                                                361
         AG_EXNAME = 'POSITION'                                           361
         AG_PARLIST = 'X_Y_Z'                                             361
         CALL AXPOSITION                                                  361
C    *                                                                    361
C    Create CSTP :                                                        362
C    Call To Local Procedure CSTP                                         362
         ASSIGN 5300 TO BLOCK_CSTP                                        362
         GO TO  5250                                                      362
5300     CONTINUE                                                         362
C    *                                                                    362
C    Geant POSITION : CSTP ===> X=STRAP_R Y=+1*STRAP_R*TAN(TWOPI/360*DPH  364
C    IT)-CALG_CRACKWD+STRAP_DY Z=CURRENT_DEPTH/TAN_THETA-ZZ0-3.679 ALPHA  364
C    Z=+3 <===                                                            364
         CALL AGSRESET                                                    364
         AG_X = STRAP_R                                                   364
         AG_Y = +1*STRAP_R*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD+STRAP_DY     364
         AG_Z = CURRENT_DEPTH/TAN_THETA-ZZ0-3.679                         364
         AG_ALPHAN=AG_ALPHAN+1                                            364
         AG_ALPHAK(AG_ALPHAN)=3                                           364
         AG_ALPHAL(AG_ALPHAN)=+3                                          364
         AG_TITLE = 'CSTP'                                                364
         AG_EXNAME = 'POSITION'                                           364
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      364
         CALL AXPOSITION                                                  364
C    *                                                                    364
C    Create CSTP :                                                        365
C    Call To Local Procedure CSTP                                         365
         ASSIGN 5310 TO BLOCK_CSTP                                        365
         GO TO  5250                                                      365
5310     CONTINUE                                                         365
C    *                                                                    365
C    Geant POSITION : CSTP ===> X=STRAP_R Y=-1*STRAP_R*TAN(TWOPI/360*DPH  367
C    IT)+CALG_CRACKWD-STRAP_DY Z=CURRENT_DEPTH/TAN_THETA-ZZ0-3.679 ALPHA  367
C    Z=-3 <===                                                            367
         CALL AGSRESET                                                    367
         AG_X = STRAP_R                                                   367
         AG_Y = -1*STRAP_R*TAN(TWOPI/360*DPHIT)+CALG_CRACKWD-STRAP_DY     367
         AG_Z = CURRENT_DEPTH/TAN_THETA-ZZ0-3.679                         367
         AG_ALPHAN=AG_ALPHAN+1                                            367
         AG_ALPHAK(AG_ALPHAN)=3                                           367
         AG_ALPHAL(AG_ALPHAN)=-3                                          367
         AG_TITLE = 'CSTP'                                                367
         AG_EXNAME = 'POSITION'                                           367
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      367
         CALL AXPOSITION                                                  367
C    *                                                                    367
         CURRENT_DEPTH = CURRENT_DEPTH + 2*CALG_FRONTTHK                  367
         LAYER = 0                                                        369
C    *                                                                    370
         SUPER=1                                                          370
            GO TO 5323                                                    370
5321        SUPER=SUPER+1                                                 370
5323        IF(SUPER-(NINT(CALG_NSUPER)).GT.0) GO TO 5322                 370
C       Create CSUP :                                                     371
C       Call To Local Procedure CSUP                                      371
            ASSIGN 5340 TO BLOCK_CSUP                                     371
            GO TO  5330                                                   371
5340        CONTINUE                                                      371
C       *                                                                 371
C       Geant POSITION : CSUP ===> inherited arguments used <===          372
            CALL AGSRESET                                                 372
            AG_TITLE = 'CSUP'                                             372
            AG_EXNAME = 'POSITION'                                        372
            AG_PARLIST = 'NONE'                                           372
            CALL AXPOSITION                                               372
C       *                                                                 372
         GO TO 5321                                                       373
5322     CONTINUE                                                         373
         AG_DX = CALG_COMPTHK                                             374
C    Create CBTW :                                                        374
C    Call To Local Procedure CBTW                                         374
         ASSIGN 5350 TO BLOCK_CBTW                                        374
         GO TO  5190                                                      374
5350     CONTINUE                                                         374
C    *                                                                    374
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+CALG_COMPTHK Z=CURRENT_D  375
C    EPTH/TAN_THETA/2+ZZ0/2 <===                                          375
         CALL AGSRESET                                                    375
         AG_X = CURRENT_DEPTH+CALG_COMPTHK                                375
         AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ0/2                           375
         AG_TITLE = 'CBTW'                                                375
         AG_EXNAME = 'POSITION'                                           375
         AG_PARLIST = 'X_Z'                                               375
         CALL AXPOSITION                                                  375
C    *                                                                    375
         C_DEP=2.*CALG_COMPTHK + 2.*CALG_AIRTHK                           375
         AG_DX = CALG_BACKTHK                                             377
C    Create CBTW :                                                        377
C    Call To Local Procedure CBTW                                         377
         ASSIGN 5360 TO BLOCK_CBTW                                        377
         GO TO  5190                                                      377
5360     CONTINUE                                                         377
C    *                                                                    377
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+C_DEP+CALG_BACKTHK Z=CUR  379
C    RENT_DEPTH/TAN_THETA/2+ZZ0/2 <===                                    379
         CALL AGSRESET                                                    379
         AG_X = CURRENT_DEPTH+C_DEP+CALG_BACKTHK                          379
         AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ0/2                           379
         AG_TITLE = 'CBTW'                                                379
         AG_EXNAME = 'POSITION'                                           379
         AG_PARLIST = 'X_Z'                                               379
         CALL AXPOSITION                                                  379
C    *                                                                    379
         C_DEP=C_DEP + 2.*CALG_BACKTHK                                    379
         AG_DX = CALG_SPACETHK                                            381
C    Create CBTW :                                                        381
C    Call To Local Procedure CBTW                                         381
         ASSIGN 5370 TO BLOCK_CBTW                                        381
         GO TO  5190                                                      381
5370     CONTINUE                                                         381
C    *                                                                    381
C    Geant POSITION : CBTW ===> X=CURRENT_DEPTH+C_DEP+CALG_SPACETHK Z=CU  383
C    RRENT_DEPTH/TAN_THETA/2+ZZ0/2 <===                                   383
         CALL AGSRESET                                                    383
         AG_X = CURRENT_DEPTH+C_DEP+CALG_SPACETHK                         383
         AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ0/2                           383
         AG_TITLE = 'CBTW'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'X_Z'                                               383
         CALL AXPOSITION                                                  383
C    *                                                                    383
         C_DEP=C_DEP + 2.*CALG_SPACETHK                                   383
         CURRENT_DEPTH=CURRENT_DEPTH + C_DEP                              385
5081  CALL AGSPOPB                                                        386
      GO TO BLOCK_CPHI, (5080 ,5090)                                      386
C End of Local Procedure                                                  386
C ----------------------------------------------------------------------  387
C ------------ CSTP                                                       387
C Begin Local Procedure CSTP                                              388
C CSTP IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            388
5250  CONTINUE                                                            388
         AG_TITLE='CSTP'                                                  388
         AG_MODULE=MODULE                                                 388
         AG_MARK='V00'                                                    388
         CALL AGSPUSH                                                     388
         AG_MODULE=MODULE                                                 388
C    *                                                                    388
C    Geant MATERIAL : IRON ===> inherited arguments used <===             389
         CALL AGSRESET                                                    389
         AG_TITLE = 'IRON'                                                389
         AG_EXNAME = 'MATERIAL'                                           389
         AG_PARLIST = 'NONE'                                              389
         CALL AXMATERIAL                                                  389
C    *                                                                    389
C    Geant MATERIAL : IRON_CSTP ===> ISVOL=0 <===                         389
         CALL AGSRESET                                                    390
         AG_ISVOL = 0                                                     390
         AG_TITLE = 'IRON_CSTP'                                           390
         AG_EXNAME = 'MATERIAL'                                           390
         AG_PARLIST = 'ISVOL'                                             390
         CALL AXMATERIAL                                                  390
C    *                                                                    390
C    Geant ATTRIBUTE : CSTP ===> SEEN=1 COLO=1 <===                       390
         CALL AGSRESET                                                    391
         AG_SEEN = 1                                                      391
         AG_COLO = 1                                                      391
         AG_TITLE = 'CSTP'                                                391
         AG_EXNAME = 'ATTRIBUTE'                                          391
         AG_PARLIST = 'SEEN_COLO'                                         391
         CALL AXATTRIBUTE                                                 391
C    *                                                                    391
C    Geant SHAPE : BOX ===> DX=STRAP_DX DY=STRAP_DY DZ=STRAP_DZ <===      391
         CALL AGSRESET                                                    392
         AG_DX = STRAP_DX                                                 392
         AG_DY = STRAP_DY                                                 392
         AG_DZ = STRAP_DZ                                                 392
         AG_TITLE = 'BOX'                                                 392
         AG_EXNAME = 'SHAPE'                                              392
         AG_PARLIST = 'DX_DY_DZ'                                          392
         CALL AXSHAPE                                                     392
C    *                                                                    392
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               392
C    Check CABS_VERSION .GT. 0                                            394
         IF (CABS_VERSION .GT. 0) THEN                                    394
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    394
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    394
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    394
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    394
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    394
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      394
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      394
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      394
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      394
         ENDIF                                                            394
5251  CALL AGSPOPB                                                        395
      GO TO BLOCK_CSTP, (5250 ,5260,5270,5300,5310)                       395
C End of Local Procedure                                                  395
C ----------------------------------------------------------------------  396
C ------------ CSPT                                                       396
C Begin Local Procedure CSPT                                              397
C CSPT IS THE SIDE ALUMINUM SKIN TOP                                      397
5150  CONTINUE                                                            397
         AG_TITLE='CSPT'                                                  397
         AG_MODULE=MODULE                                                 397
         AG_MARK='V00'                                                    397
         CALL AGSPUSH                                                     397
         AG_MODULE=MODULE                                                 397
C    *                                                                    397
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        398
         CALL AGSRESET                                                    398
         AG_TITLE = 'ALUMINIUM'                                           398
         AG_EXNAME = 'MATERIAL'                                           398
         AG_PARLIST = 'NONE'                                              398
         CALL AXMATERIAL                                                  398
C    *                                                                    398
C    Geant MATERIAL : ALU_CSPT ===> ISVOL=0 <===                          398
         CALL AGSRESET                                                    399
         AG_ISVOL = 0                                                     399
         AG_TITLE = 'ALU_CSPT'                                            399
         AG_EXNAME = 'MATERIAL'                                           399
         AG_PARLIST = 'ISVOL'                                             399
         CALL AXMATERIAL                                                  399
C    *                                                                    399
C    Geant ATTRIBUTE : CSPT ===> SEEN=1 COLO=6 <===                       399
         CALL AGSRESET                                                    400
         AG_SEEN = 1                                                      400
         AG_COLO = 6                                                      400
         AG_TITLE = 'CSPT'                                                400
         AG_EXNAME = 'ATTRIBUTE'                                          400
         AG_PARLIST = 'SEEN_COLO'                                         400
         CALL AXATTRIBUTE                                                 400
C    *                                                                    400
C    Geant SHAPE : PCON ===> PHI1=-2.94 DPHI=0.0045 NZ=3 ZI={0, CUT_LENG  400
C    TH,HLENG} RMN={RMIN2,RMIN2,CUT_RADIUS2}, RMX={RMAX2,RMAX2,RMAX2} <=  400
C    ==                                                                   400
         CALL AGSRESET                                                    403
         AG_PHI1 = -2.94                                                  403
         AG_DPHI = 0.0045                                                 403
         AG_NZ = 3                                                        403
         AG_ZI(1)=0                                                       403
         AG_ZI(2)=CUT_LENGTH                                              403
         AG_ZI(3)=HLENG                                                   403
         AG_RMN(1)=RMIN2                                                  403
         AG_RMN(2)=RMIN2                                                  403
         AG_RMN(3)=CUT_RADIUS2                                            403
         AG_RMX(1)=RMAX2                                                  403
         AG_RMX(2)=RMAX2                                                  403
         AG_RMX(3)=RMAX2                                                  403
         AG_TITLE = 'PCON'                                                403
         AG_EXNAME = 'SHAPE'                                              403
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           403
         CALL AXSHAPE                                                     403
C    *                                                                    403
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               403
C    Check CABS_VERSION .GT. 0                                            405
         IF (CABS_VERSION .GT. 0) THEN                                    405
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    405
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    405
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    405
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    405
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    405
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      405
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      405
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      405
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      405
         ENDIF                                                            405
5151  CALL AGSPOPB                                                        406
      GO TO BLOCK_CSPT, (5150 ,5160)                                      406
C End of Local Procedure                                                  406
C ----------------------------------------------------------------------  407
C ------------ CSPB                                                       407
C Begin Local Procedure CSPB                                              408
C CSPB IS THE SIDE ALUMINUM SKIN BOTTOM                                   408
5170  CONTINUE                                                            408
         AG_TITLE='CSPB'                                                  408
         AG_MODULE=MODULE                                                 408
         AG_MARK='V00'                                                    408
         CALL AGSPUSH                                                     408
         AG_MODULE=MODULE                                                 408
C    *                                                                    408
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        409
         CALL AGSRESET                                                    409
         AG_TITLE = 'ALUMINIUM'                                           409
         AG_EXNAME = 'MATERIAL'                                           409
         AG_PARLIST = 'NONE'                                              409
         CALL AXMATERIAL                                                  409
C    *                                                                    409
C    Geant MATERIAL : ALU_CSPB ===> ISVOL=0 <===                          409
         CALL AGSRESET                                                    410
         AG_ISVOL = 0                                                     410
         AG_TITLE = 'ALU_CSPB'                                            410
         AG_EXNAME = 'MATERIAL'                                           410
         AG_PARLIST = 'ISVOL'                                             410
         CALL AXMATERIAL                                                  410
C    *                                                                    410
C    Geant ATTRIBUTE : CSPB ===> SEEN=1 COLO=6 <===                       410
         CALL AGSRESET                                                    411
         AG_SEEN = 1                                                      411
         AG_COLO = 6                                                      411
         AG_TITLE = 'CSPB'                                                411
         AG_EXNAME = 'ATTRIBUTE'                                          411
         AG_PARLIST = 'SEEN_COLO'                                         411
         CALL AXATTRIBUTE                                                 411
C    *                                                                    411
C    Geant SHAPE : PCON ===> PHI1=+2.94 DPHI=0.0045 NZ=3 ZI={0, CUT_LENG  411
C    TH,HLENG} RMN={RMIN2,RMIN2,CUT_RADIUS2}, RMX={RMAX2,RMAX2,RMAX2} <=  411
C    ==                                                                   411
         CALL AGSRESET                                                    414
         AG_PHI1 = +2.94                                                  414
         AG_DPHI = 0.0045                                                 414
         AG_NZ = 3                                                        414
         AG_ZI(1)=0                                                       414
         AG_ZI(2)=CUT_LENGTH                                              414
         AG_ZI(3)=HLENG                                                   414
         AG_RMN(1)=RMIN2                                                  414
         AG_RMN(2)=RMIN2                                                  414
         AG_RMN(3)=CUT_RADIUS2                                            414
         AG_RMX(1)=RMAX2                                                  414
         AG_RMX(2)=RMAX2                                                  414
         AG_RMX(3)=RMAX2                                                  414
         AG_TITLE = 'PCON'                                                414
         AG_EXNAME = 'SHAPE'                                              414
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           414
         CALL AXSHAPE                                                     414
C    *                                                                    414
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               414
C    Check CABS_VERSION .GT. 0                                            416
         IF (CABS_VERSION .GT. 0) THEN                                    416
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    416
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    416
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    416
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    416
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    416
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      416
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      416
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      416
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      416
         ENDIF                                                            416
5171  CALL AGSPOPB                                                        417
      GO TO BLOCK_CSPB, (5170 ,5180)                                      417
C End of Local Procedure                                                  417
C ----------------------------------------------------------------------  418
C ------------ CSLG                                                       418
C Begin Local Procedure CSLG                                              419
C CSLG IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            419
5220  CONTINUE                                                            419
         AG_TITLE='CSLG'                                                  419
         AG_MODULE=MODULE                                                 419
         AG_MARK='V00'                                                    419
         CALL AGSPUSH                                                     419
         AG_MODULE=MODULE                                                 419
C    *                                                                    419
C    Geant MATERIAL : IRON ===> inherited arguments used <===             420
         CALL AGSRESET                                                    420
         AG_TITLE = 'IRON'                                                420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant MATERIAL : CRON_CSLG ===> ISVOL=0 <===                         420
         CALL AGSRESET                                                    421
         AG_ISVOL = 0                                                     421
         AG_TITLE = 'CRON_CSLG'                                           421
         AG_EXNAME = 'MATERIAL'                                           421
         AG_PARLIST = 'ISVOL'                                             421
         CALL AXMATERIAL                                                  421
C    *                                                                    421
C    Geant ATTRIBUTE : CSLG ===> SEEN=1 COLO=1 <===                       421
         CALL AGSRESET                                                    422
         AG_SEEN = 1                                                      422
         AG_COLO = 1                                                      422
         AG_TITLE = 'CSLG'                                                422
         AG_EXNAME = 'ATTRIBUTE'                                          422
         AG_PARLIST = 'SEEN_COLO'                                         422
         CALL AXATTRIBUTE                                                 422
C    *                                                                    422
C    Geant SHAPE : BOX ===> DX=SLUG_DX DY=SLUG_DY DZ=SLUG_DZ <===         422
         CALL AGSRESET                                                    423
         AG_DX = SLUG_DX                                                  423
         AG_DY = SLUG_DY                                                  423
         AG_DZ = SLUG_DZ                                                  423
         AG_TITLE = 'BOX'                                                 423
         AG_EXNAME = 'SHAPE'                                              423
         AG_PARLIST = 'DX_DY_DZ'                                          423
         CALL AXSHAPE                                                     423
C    *                                                                    423
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               423
C    Check CABS_VERSION .GT. 0                                            425
         IF (CABS_VERSION .GT. 0) THEN                                    425
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    425
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    425
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    425
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    425
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    425
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      425
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      425
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      425
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      425
         ENDIF                                                            425
5221  CALL AGSPOPB                                                        426
      GO TO BLOCK_CSLG, (5220 ,5230,5240,5280,5290)                       426
C End of Local Procedure                                                  426
C ----------------------------------------------------------------------  427
C ------------ CSZO                                                       427
C Begin Local Procedure CSZO                                              428
C CSZO IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            428
5110  CONTINUE                                                            428
         AG_TITLE='CSZO'                                                  428
         AG_MODULE=MODULE                                                 428
         AG_MARK='V00'                                                    428
         CALL AGSPUSH                                                     428
         AG_MODULE=MODULE                                                 428
C    *                                                                    428
C    Geant MATERIAL : IRON ===> inherited arguments used <===             429
         CALL AGSRESET                                                    429
         AG_TITLE = 'IRON'                                                429
         AG_EXNAME = 'MATERIAL'                                           429
         AG_PARLIST = 'NONE'                                              429
         CALL AXMATERIAL                                                  429
C    *                                                                    429
C    Geant MATERIAL : IRON_CSZO ===> ISVOL=0 <===                         429
         CALL AGSRESET                                                    430
         AG_ISVOL = 0                                                     430
         AG_TITLE = 'IRON_CSZO'                                           430
         AG_EXNAME = 'MATERIAL'                                           430
         AG_PARLIST = 'ISVOL'                                             430
         CALL AXMATERIAL                                                  430
C    *                                                                    430
C    Geant ATTRIBUTE : CSZO ===> SEEN=1 COLO=1 <===                       430
         CALL AGSRESET                                                    431
         AG_SEEN = 1                                                      431
         AG_COLO = 1                                                      431
         AG_TITLE = 'CSZO'                                                431
         AG_EXNAME = 'ATTRIBUTE'                                          431
         AG_PARLIST = 'SEEN_COLO'                                         431
         CALL AXATTRIBUTE                                                 431
C    *                                                                    431
C    Geant SHAPE : PGON ===> PHI1=-DPHIMOD/2 DPHI=DPHIMOD, NPDIV=1 NZ=2   431
C    ZI={0,ZZ0} RMN={CALG_RMIN,CALG_RMIN}, RMX={RMAX,RMAX} <===           431
         CALL AGSRESET                                                    434
         AG_PHI1 = -DPHIMOD/2                                             434
         AG_DPHI = DPHIMOD                                                434
         AG_NPDIV = 1                                                     434
         AG_NZ = 2                                                        434
         AG_ZI(1)=0                                                       434
         AG_ZI(2)=ZZ0                                                     434
         AG_RMN(1)=CALG_RMIN                                              434
         AG_RMN(2)=CALG_RMIN                                              434
         AG_RMX(1)=RMAX                                                   434
         AG_RMX(2)=RMAX                                                   434
         AG_TITLE = 'PGON'                                                434
         AG_EXNAME = 'SHAPE'                                              434
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     434
         CALL AXSHAPE                                                     434
C    *                                                                    434
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               434
C    Check CABS_VERSION .GT. 0                                            436
         IF (CABS_VERSION .GT. 0) THEN                                    436
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    436
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    436
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    436
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    436
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    436
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      436
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      436
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      436
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      436
         ENDIF                                                            436
5111  CALL AGSPOPB                                                        437
      GO TO BLOCK_CSZO, (5110 ,5120)                                      437
C End of Local Procedure                                                  437
C ----------------------------------------------------------------------  438
C ------------ CSZU                                                       438
C Begin Local Procedure CSZU                                              439
C CSZU IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            439
5130  CONTINUE                                                            439
         AG_TITLE='CSZU'                                                  439
         AG_MODULE=MODULE                                                 439
         AG_MARK='V00'                                                    439
         CALL AGSPUSH                                                     439
         AG_MODULE=MODULE                                                 439
C    *                                                                    439
C    Geant MATERIAL : IRON ===> inherited arguments used <===             440
         CALL AGSRESET                                                    440
         AG_TITLE = 'IRON'                                                440
         AG_EXNAME = 'MATERIAL'                                           440
         AG_PARLIST = 'NONE'                                              440
         CALL AXMATERIAL                                                  440
C    *                                                                    440
C    Geant MATERIAL : IRON_CSZU ===> ISVOL=0 <===                         440
         CALL AGSRESET                                                    441
         AG_ISVOL = 0                                                     441
         AG_TITLE = 'IRON_CSZU'                                           441
         AG_EXNAME = 'MATERIAL'                                           441
         AG_PARLIST = 'ISVOL'                                             441
         CALL AXMATERIAL                                                  441
C    *                                                                    441
C    Geant ATTRIBUTE : CSZU ===> SEEN=1 COLO=1 <===                       441
         CALL AGSRESET                                                    442
         AG_SEEN = 1                                                      442
         AG_COLO = 1                                                      442
         AG_TITLE = 'CSZU'                                                442
         AG_EXNAME = 'ATTRIBUTE'                                          442
         AG_PARLIST = 'SEEN_COLO'                                         442
         CALL AXATTRIBUTE                                                 442
C    *                                                                    442
C    Geant SHAPE : PGON ===> PHI1=-DPHIMOD/2 DPHI=DPHIMOD, NPDIV=1 NZ=2   442
C    ZI={CUT_LENGTH2,HLENG2} RMN={CALG_RMIN, CUT_RADIUS} RMX={CALG_RMIN+  442
C    ZZ0/COS(CUTANG), CUT_RADIUS+ZZ0/COS(CUTANG)} <===                    442
         CALL AGSRESET                                                    446
         AG_PHI1 = -DPHIMOD/2                                             446
         AG_DPHI = DPHIMOD                                                446
         AG_NPDIV = 1                                                     446
         AG_NZ = 2                                                        446
         AG_ZI(1)=CUT_LENGTH2                                             446
         AG_ZI(2)=HLENG2                                                  446
         AG_RMN(1)=CALG_RMIN                                              446
         AG_RMN(2)=CUT_RADIUS                                             446
         AG_RMX(1)=CALG_RMIN+ZZ0/COS(CUTANG)                              446
         AG_RMX(2)=CUT_RADIUS+ZZ0/COS(CUTANG)                             446
         AG_TITLE = 'PGON'                                                446
         AG_EXNAME = 'SHAPE'                                              446
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     446
         CALL AXSHAPE                                                     446
C    *                                                                    446
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               446
C    Check CABS_VERSION .GT. 0                                            448
         IF (CABS_VERSION .GT. 0) THEN                                    448
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    448
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    448
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    448
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    448
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    448
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      448
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      448
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      448
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      448
         ENDIF                                                            448
5131  CALL AGSPOPB                                                        449
      GO TO BLOCK_CSZU, (5130 ,5140)                                      449
C End of Local Procedure                                                  449
C ----------------------------------------------------------------------  450
C ------------ CSUP                                                       450
C Begin Local Procedure CSUP                                              451
C CSUP IS A SUPER LAYER WITH FEW LAYERS INSIDE                            451
5330  CONTINUE                                                            451
         AG_TITLE='CSUP'                                                  451
         AG_MODULE=MODULE                                                 451
         AG_MARK='V00'                                                    451
         CALL AGSPUSH                                                     451
         AG_MODULE=MODULE                                                 451
C    *                                                                    451
         FUTURE_DEPTH=CURRENT_DEPTH + (CALG_NSUBLAY(SUPER) - SUPER +      453
     *   1)*LAYER_WIDTH(SUPER)*2 + (SMD_WIDTH + CALG_SCINTTHK(SUPER) +    453
     *   2.*CALG_ABPAPTHK)*2*(SUPER - 1)                                  453
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=6./21. <===                  454
         CALL AGSRESET                                                    454
         AG_A = 12.01                                                     454
         AG_Z = 6.                                                        454
         AG_W = 6./21.                                                    454
         AG_TITLE = 'C'                                                   454
         AG_EXNAME = 'COMPONENT'                                          454
         AG_PARLIST = 'A_Z_W'                                             454
         CALL AXCOMPONENT                                                 454
C    *                                                                    454
C    Geant COMPONENT : H ===> A=1. Z=1. W=10./21. <===                    454
         CALL AGSRESET                                                    455
         AG_A = 1.                                                        455
         AG_Z = 1.                                                        455
         AG_W = 10./21.                                                   455
         AG_TITLE = 'H'                                                   455
         AG_EXNAME = 'COMPONENT'                                          455
         AG_PARLIST = 'A_Z_W'                                             455
         CALL AXCOMPONENT                                                 455
C    *                                                                    455
C    Geant COMPONENT : O ===> A=16. Z=8. W=5./21. <===                    455
         CALL AGSRESET                                                    456
         AG_A = 16.                                                       456
         AG_Z = 8.                                                        456
         AG_W = 5./21.                                                    456
         AG_TITLE = 'O'                                                   456
         AG_EXNAME = 'COMPONENT'                                          456
         AG_PARLIST = 'A_Z_W'                                             456
         CALL AXCOMPONENT                                                 456
C    *                                                                    456
C    Geant MIXTURE : CELLULOSE ===> DENS=0.35 <===                        456
         CALL AGSRESET                                                    457
         AG_DENS = 0.35                                                   457
         AG_TITLE = 'CELLULOSE'                                           457
         AG_EXNAME = 'MIXTURE'                                            457
         AG_PARLIST = 'DENS'                                              457
         CALL AXMIXTURE                                                   457
C    *                                                                    457
C    Geant ATTRIBUTE : CSUP ===> SEEN=0 COLO=1 <===                       457
         CALL AGSRESET                                                    458
         AG_SEEN = 0                                                      458
         AG_COLO = 1                                                      458
         AG_TITLE = 'CSUP'                                                458
         AG_EXNAME = 'ATTRIBUTE'                                          458
         AG_PARLIST = 'SEEN_COLO'                                         458
         CALL AXATTRIBUTE                                                 458
C    *                                                                    458
C    Geant SHAPE : PGON ===> PHI1=-DPHIMOD/2 DPHI=DPHIMOD, NPDIV=1 NZ=3   458
C    ZI={ZZ1,CURRENT_DEPTH/TAN_THETA, FUTURE_DEPTH/TAN_THETA} RMN={CURRE  458
C    NT_DEPTH,CURRENT_DEPTH, FUTURE_DEPTH} RMX={FUTURE_DEPTH,FUTURE_DEPT  458
C    H, FUTURE_DEPTH} <===                                                458
         CALL AGSRESET                                                    463
         AG_PHI1 = -DPHIMOD/2                                             463
         AG_DPHI = DPHIMOD                                                463
         AG_NPDIV = 1                                                     463
         AG_NZ = 3                                                        463
         AG_ZI(1)=ZZ1                                                     463
         AG_ZI(2)=CURRENT_DEPTH/TAN_THETA                                 463
         AG_ZI(3)=FUTURE_DEPTH/TAN_THETA                                  463
         AG_RMN(1)=CURRENT_DEPTH                                          463
         AG_RMN(2)=CURRENT_DEPTH                                          463
         AG_RMN(3)=FUTURE_DEPTH                                           463
         AG_RMX(1)=FUTURE_DEPTH                                           463
         AG_RMX(2)=FUTURE_DEPTH                                           463
         AG_RMX(3)=FUTURE_DEPTH                                           463
         AG_TITLE = 'PGON'                                                463
         AG_EXNAME = 'SHAPE'                                              463
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     463
         CALL AXSHAPE                                                     463
C    *                                                                    463
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               463
C    Check CABS_VERSION .GT. 0                                            465
         IF (CABS_VERSION .GT. 0) THEN                                    465
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    465
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    465
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    465
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    465
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    465
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      465
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      465
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      465
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      465
         ENDIF                                                            465
C    *                                                                    466
         SUB=1                                                            466
            GO TO 5383                                                    466
5381        SUB=SUB+1                                                     466
5383        IF(SUB-(NINT(CALG_NSUBLAY(SUPER))).GT.0) GO TO 5382           466
            LAYER = LAYER + 1                                             467
C       Check LAYER.LT.NINT(CALG_NSUBLAY(1)+CALG_NSUBLAY(2))              468
            IF (LAYER.LT.NINT(CALG_NSUBLAY(1)+CALG_NSUBLAY(2))) THEN      468
C       Create CSCI :                                                     469
C       Call To Local Procedure CSCI                                      469
            ASSIGN 5400 TO BLOCK_CSCI                                     469
            GO TO  5390                                                   469
5400        CONTINUE                                                      469
C       *                                                                 469
C       Geant POSITION : CSCI ===> X=CURRENT_DEPTH+CALG_SCINTTHK(SUPER)+  471
C       2.*CALG_ABPAPTHK Z=CURRENT_DEPTH/TAN_THETA/2+ZZ1/2 <===           471
            CALL AGSRESET                                                 471
            AG_X = CURRENT_DEPTH+CALG_SCINTTHK(SUPER)+2.*CALG_ABPAPTHK    471
            AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ1/2                        471
            AG_TITLE = 'CSCI'                                             471
            AG_EXNAME = 'POSITION'                                        471
            AG_PARLIST = 'X_Z'                                            471
            CALL AXPOSITION                                               471
C       *                                                                 471
C       Create CPBP :                                                     472
C       Call To Local Procedure CPBP                                      472
            ASSIGN 5420 TO BLOCK_CPBP                                     472
            GO TO  5410                                                   472
5420        CONTINUE                                                      472
C       *                                                                 472
            C_LEAD_DEP=2.*CALG_SCINTTHK(SUPER)+4.*CALG_ABPAPTHK           473
C       Geant POSITION : CPBP ===> X=CURRENT_DEPTH+C_LEAD_DEP+CALG_ABSOR  475
C       THK Z=CURRENT_DEPTH/TAN_THETA/2+ZZ1/2 <===                        475
            CALL AGSRESET                                                 475
            AG_X = CURRENT_DEPTH+C_LEAD_DEP+CALG_ABSORTHK                 475
            AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ1/2                        475
            AG_TITLE = 'CPBP'                                             475
            AG_EXNAME = 'POSITION'                                        475
            AG_PARLIST = 'X_Z'                                            475
            CALL AXPOSITION                                               475
C       *                                                                 475
            CURRENT_DEPTH = CURRENT_DEPTH + 2*LAYER_WIDTH(SUPER)          475
            ELSE                                                          477
C       Create CSCI :                                                     478
C       Call To Local Procedure CSCI                                      478
            ASSIGN 5430 TO BLOCK_CSCI                                     478
            GO TO  5390                                                   478
5430        CONTINUE                                                      478
C       *                                                                 478
C       Geant POSITION : CSCI ===> X=CURRENT_DEPTH+CALG_SCINTTHK(2)+2.*C  480
C       ALG_ABPAPTHK Z=CURRENT_DEPTH/TAN_THETA/2+ZZ1/2 <===               480
            CALL AGSRESET                                                 480
            AG_X = CURRENT_DEPTH+CALG_SCINTTHK(2)+2.*CALG_ABPAPTHK        480
            AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ1/2                        480
            AG_TITLE = 'CSCI'                                             480
            AG_EXNAME = 'POSITION'                                        480
            AG_PARLIST = 'X_Z'                                            480
            CALL AXPOSITION                                               480
C       *                                                                 480
            CURRENT_DEPTH = CURRENT_DEPTH+C_LEAD_DEP                      480
            ENDIF                                                         482
C       Skip Unless LAYER==NINT(CALG_NSMD)                                483
            IF (LAYER.NE.NINT(CALG_NSMD))GO TO 5381                       483
C       Create CSMD :                                                     484
C       Call To Local Procedure CSMD                                      484
            ASSIGN 5450 TO BLOCK_CSMD                                     484
            GO TO  5440                                                   484
5450        CONTINUE                                                      484
C       *                                                                 484
C       Geant POSITION : CSMD ===> X=CURRENT_DEPTH+SMD_WIDTH Z=CURRENT_D  485
C       EPTH/TAN_THETA/2+ZZ0/2 <===                                       485
            CALL AGSRESET                                                 485
            AG_X = CURRENT_DEPTH+SMD_WIDTH                                485
            AG_Z = CURRENT_DEPTH/TAN_THETA/2+ZZ0/2                        485
            AG_TITLE = 'CSMD'                                             485
            AG_EXNAME = 'POSITION'                                        485
            AG_PARLIST = 'X_Z'                                            485
            CALL AXPOSITION                                               485
C       *                                                                 485
            CURRENT_DEPTH = CURRENT_DEPTH + 2*SMD_WIDTH                   485
         GO TO 5381                                                       487
5382     CONTINUE                                                         487
5331  CALL AGSPOPB                                                        488
      GO TO BLOCK_CSUP, (5330 ,5340)                                      488
C End of Local Procedure                                                  488
C ----------------------------------------------------------------------  489
C ------------ CPBP                                                       489
C Begin Local Procedure CPBP                                              490
C CPBP IS A BLOCK THAT AN AUTHOR FAILED TO ADEQUATELY DOCUMENT            490
5410  CONTINUE                                                            490
         AG_TITLE='CPBP'                                                  490
         AG_MODULE=MODULE                                                 490
         AG_MARK='V00'                                                    490
         CALL AGSPUSH                                                     490
         AG_MODULE=MODULE                                                 490
C    *                                                                    490
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             491
         CALL AGSRESET                                                    491
         AG_TITLE = 'LEAD'                                                491
         AG_EXNAME = 'MATERIAL'                                           491
         AG_PARLIST = 'NONE'                                              491
         CALL AXMATERIAL                                                  491
C    *                                                                    491
C    Geant MATERIAL : LEAD_CPBP ===> ISVOL=0 <===                         491
         CALL AGSRESET                                                    492
         AG_ISVOL = 0                                                     492
         AG_TITLE = 'LEAD_CPBP'                                           492
         AG_EXNAME = 'MATERIAL'                                           492
         AG_PARLIST = 'ISVOL'                                             492
         CALL AXMATERIAL                                                  492
C    *                                                                    492
C    Geant ATTRIBUTE : CPBP ===> SEEN=1 COLO=1 <===                       492
         CALL AGSRESET                                                    493
         AG_SEEN = 1                                                      493
         AG_COLO = 1                                                      493
         AG_TITLE = 'CPBP'                                                493
         AG_EXNAME = 'ATTRIBUTE'                                          493
         AG_PARLIST = 'SEEN_COLO'                                         493
         CALL AXATTRIBUTE                                                 493
C    *                                                                    493
C    Geant SHAPE : BOX ===> DX=CALG_ABSORTHK DY=CURRENT_DEPTH*TAN(TWOPI/  493
C    360*DPHIT)-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2-ZZ1/2 <===     493
         CALL AGSRESET                                                    495
         AG_DX = CALG_ABSORTHK                                            495
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          495
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2-ZZ1/2                          495
         AG_TITLE = 'BOX'                                                 495
         AG_EXNAME = 'SHAPE'                                              495
         AG_PARLIST = 'DX_DY_DZ'                                          495
         CALL AXSHAPE                                                     495
C    *                                                                    495
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               495
C    Check CABS_VERSION .GT. 0                                            497
         IF (CABS_VERSION .GT. 0) THEN                                    497
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    497
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    497
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    497
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    497
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    497
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      497
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      497
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      497
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      497
         ENDIF                                                            497
5411  CALL AGSPOPB                                                        498
      GO TO BLOCK_CPBP, (5410 ,5420)                                      498
C End of Local Procedure                                                  498
C ----------------------------------------------------------------------  499
C ------------ CSCI                                                       499
C Begin Local Procedure CSCI                                              500
C CSCI A SCINTILLATOR LAYER.                                              500
5390  CONTINUE                                                            500
         AG_TITLE='CSCI'                                                  500
         AG_MODULE=MODULE                                                 500
         AG_MARK='V00'                                                    500
         CALL AGSPUSH                                                     500
         AG_MODULE=MODULE                                                 500
C    *                                                                    500
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       501
         CALL AGSRESET                                                    501
         AG_TITLE = 'POLYSTYREN'                                          501
         AG_EXNAME = 'MATERIAL'                                           501
         AG_PARLIST = 'NONE'                                              501
         CALL AXMATERIAL                                                  501
C    *                                                                    501
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       501
         CALL AGSRESET                                                    502
         AG_ISVOL = 1                                                     502
         AG_TITLE = 'CPOLYSTYREN'                                         502
         AG_EXNAME = 'MATERIAL'                                           502
         AG_PARLIST = 'ISVOL'                                             502
         CALL AXMATERIAL                                                  502
C    *                                                                    502
C    Geant ATTRIBUTE : CSCI ===> SEEN=1 COLO=4 <===                       502
         CALL AGSRESET                                                    503
         AG_SEEN = 1                                                      503
         AG_COLO = 4                                                      503
         AG_TITLE = 'CSCI'                                                503
         AG_EXNAME = 'ATTRIBUTE'                                          503
         AG_PARLIST = 'SEEN_COLO'                                         503
         CALL AXATTRIBUTE                                                 503
C    *                                                                    503
C    Geant SHAPE : BOX ===> DX=CALG_SCINTTHK(SUPER) DY=CURRENT_DEPTH*TAN  503
C    (TWOPI/360*DPHIT)-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2-ZZ1/2   503
C    <===                                                                 503
         CALL AGSRESET                                                    505
         AG_DX = CALG_SCINTTHK(SUPER)                                     505
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          505
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2-ZZ1/2                          505
         AG_TITLE = 'BOX'                                                 505
         AG_EXNAME = 'SHAPE'                                              505
         AG_PARLIST = 'DX_DY_DZ'                                          505
         CALL AXSHAPE                                                     505
C    *                                                                    505
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               505
C    Check CABS_VERSION .GT. 0                                            507
         IF (CABS_VERSION .GT. 0) THEN                                    507
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    507
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    507
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    507
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    507
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    507
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      507
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      507
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      507
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      507
         ENDIF                                                            507
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                508
         CALL GSTPAR(AG_IMED, 'BIRK2', RKB2SC)                            509
         CALL GSTPAR(AG_IMED, 'BIRK3', RKB3SC)                            510
C    Defining Hits: ETA:DETA:(0,1) Y:1:(-13,+13) BIRK:0:(0,10)            511
         AG_TITLE = 'CSUP'                                                511
         AG_EXNAME = 'HITS'                                               511
         AG_TYPE=AG_MAGIC                                                 511
         CALL AGSDETV('CSUP')                                             511
C    *                                                                    511
         AG_PARLIST='ETA'                                                 511
         AG_TITLE = 'DETH'                                                511
         AG_NBITS = AG_MAGIC                                              511
         AG_BIN=DETA                                                      511
         AG_OPTION='*'                                                    511
         AG_FHMIN = 0                                                     511
         AG_FHMAX = 1                                                     511
         CALL AGSDETH (0)                                                 511
C    *                                                                    511
         AG_PARLIST='Y'                                                   511
         AG_TITLE = 'DETH'                                                511
         AG_NBITS = 1                                                     511
         AG_BIN=AG_MAGIC                                                  511
         AG_OPTION='*'                                                    511
         AG_FHMIN = -13                                                   511
         AG_FHMAX = +13                                                   511
         CALL AGSDETH (0)                                                 511
C    *                                                                    511
         AG_PARLIST='BIRK'                                                511
         AG_TITLE = 'DETH'                                                511
         AG_NBITS = 0                                                     511
         AG_BIN=AG_MAGIC                                                  511
         AG_OPTION='*'                                                    511
         AG_FHMIN = 0                                                     511
         AG_FHMAX = 10                                                    511
         CALL AGSDETH (0)                                                 511
C    *                                                                    511
         CALL AGSDETU('CSUP')                                             511
C    *                                                                    511
5391  CALL AGSPOPB                                                        512
      GO TO BLOCK_CSCI, (5390 ,5400,5430)                                 512
C End of Local Procedure                                                  512
C ----------------------------------------------------------------------  513
C ------------ CBTW                                                       513
C Begin Local Procedure CBTW                                              514
C CBTW IS THE MODULE FRONT BACK PLATE                                     514
5190  CONTINUE                                                            514
         AG_TITLE='CBTW'                                                  514
         AG_MODULE=MODULE                                                 514
         AG_MARK='V00'                                                    514
         CALL AGSPUSH                                                     514
         AG_MODULE=MODULE                                                 514
C    *                                                                    514
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        515
         CALL AGSRESET                                                    515
         AG_TITLE = 'ALUMINIUM'                                           515
         AG_EXNAME = 'MATERIAL'                                           515
         AG_PARLIST = 'NONE'                                              515
         CALL AXMATERIAL                                                  515
C    *                                                                    515
C    Geant MATERIAL : ALU_CBTW ===> ISVOL=0 <===                          515
         CALL AGSRESET                                                    516
         AG_ISVOL = 0                                                     516
         AG_TITLE = 'ALU_CBTW'                                            516
         AG_EXNAME = 'MATERIAL'                                           516
         AG_PARLIST = 'ISVOL'                                             516
         CALL AXMATERIAL                                                  516
C    *                                                                    516
C    Geant ATTRIBUTE : CBTW ===> SEEN=1 COLO=6 <===                       516
         CALL AGSRESET                                                    517
         AG_SEEN = 1                                                      517
         AG_COLO = 6                                                      517
         AG_TITLE = 'CBTW'                                                517
         AG_EXNAME = 'ATTRIBUTE'                                          517
         AG_PARLIST = 'SEEN_COLO'                                         517
         CALL AXATTRIBUTE                                                 517
C    *                                                                    517
C    Geant SHAPE : BOX ===> DY=CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_C  517
C    RACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2-ZZ0/2 <===                      517
         CALL AGSRESET                                                    519
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/360*DPHIT)-CALG_CRACKWD          519
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2-ZZ0/2                          519
         AG_TITLE = 'BOX'                                                 519
         AG_EXNAME = 'SHAPE'                                              519
         AG_PARLIST = 'DY_DZ'                                             519
         CALL AXSHAPE                                                     519
C    *                                                                    519
         CALL CALBPAR( AG_IMED,'ABSORBER' )                               519
C    Check CABS_VERSION .GT. 0                                            521
         IF (CABS_VERSION .GT. 0) THEN                                    521
         CALL GSTPAR( AG_IMED, 'CUTGAM', CABS_CUTGAM )                    521
         CALL GSTPAR( AG_IMED, 'CUTELE', CABS_CUTELE )                    521
         CALL GSTPAR( AG_IMED, 'CUTNEU', CABS_CUTNEU )                    521
         CALL GSTPAR( AG_IMED, 'CUTHAD', CABS_CUTHAD )                    521
         CALL GSTPAR( AG_IMED, 'CUTMUO', CABS_CUTMUO )                    521
         CALL GSTPAR( AG_IMED, 'DCUTE', CABS_DCUTE )                      521
         CALL GSTPAR( AG_IMED, 'DCUTM', CABS_DCUTM )                      521
         CALL GSTPAR( AG_IMED, 'BCUTE', CABS_BCUTE )                      521
         CALL GSTPAR( AG_IMED, 'BCUTM', CABS_BCUTM )                      521
         ENDIF                                                            521
5191  CALL AGSPOPB                                                        522
      GO TO BLOCK_CBTW, (5190 ,5200,5350,5360,5370)                       522
C End of Local Procedure                                                  522
C ----------------------------------------------------------------------  523
C ------------ CSMD                                                       523
C Begin Local Procedure CSMD                                              524
C CSMD IS THE SHOWER MAXIMUM DETECTOR ENVELOPE                            524
5440  CONTINUE                                                            524
         AG_TITLE='CSMD'                                                  524
         AG_MODULE=MODULE                                                 524
         AG_MARK='V00'                                                    524
         CALL AGSPUSH                                                     524
         AG_MODULE=MODULE                                                 524
C    *                                                                    524
         CURRENT = - SMD_WIDTH                                            525
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=6./21. <===                  526
         CALL AGSRESET                                                    526
         AG_A = 12.01                                                     526
         AG_Z = 6.                                                        526
         AG_W = 6./21.                                                    526
         AG_TITLE = 'C'                                                   526
         AG_EXNAME = 'COMPONENT'                                          526
         AG_PARLIST = 'A_Z_W'                                             526
         CALL AXCOMPONENT                                                 526
C    *                                                                    526
C    Geant COMPONENT : H ===> A=1. Z=1. W=10./21. <===                    526
         CALL AGSRESET                                                    527
         AG_A = 1.                                                        527
         AG_Z = 1.                                                        527
         AG_W = 10./21.                                                   527
         AG_TITLE = 'H'                                                   527
         AG_EXNAME = 'COMPONENT'                                          527
         AG_PARLIST = 'A_Z_W'                                             527
         CALL AXCOMPONENT                                                 527
C    *                                                                    527
C    Geant COMPONENT : O ===> A=16. Z=8. W=5./21. <===                    527
         CALL AGSRESET                                                    528
         AG_A = 16.                                                       528
         AG_Z = 8.                                                        528
         AG_W = 5./21.                                                    528
         AG_TITLE = 'O'                                                   528
         AG_EXNAME = 'COMPONENT'                                          528
         AG_PARLIST = 'A_Z_W'                                             528
         CALL AXCOMPONENT                                                 528
C    *                                                                    528
C    Geant MIXTURE : CELLULOSE ===> DENS=0.35 <===                        528
         CALL AGSRESET                                                    529
         AG_DENS = 0.35                                                   529
         AG_TITLE = 'CELLULOSE'                                           529
         AG_EXNAME = 'MIXTURE'                                            529
         AG_PARLIST = 'DENS'                                              529
         CALL AXMIXTURE                                                   529
C    *                                                                    529
C    Geant ATTRIBUTE : CSMD ===> SEEN=1 COLO=6 <===                       529
         CALL AGSRESET                                                    530
         AG_SEEN = 1                                                      530
         AG_COLO = 6                                                      530
         AG_TITLE = 'CSMD'                                                530
         AG_EXNAME = 'ATTRIBUTE'                                          530
         AG_PARLIST = 'SEEN_COLO'                                         530
         CALL AXATTRIBUTE                                                 530
C    *                                                                    530
C    Geant SHAPE : BOX ===> DX=SMD_WIDTH DY=CURRENT_DEPTH*TAN(TWOPI/120.  530
C    )-CALG_CRACKWD, DZ=CURRENT_DEPTH/TAN_THETA/2-ZZ0/2 <===              530
         CALL AGSRESET                                                    532
         AG_DX = SMD_WIDTH                                                532
         AG_DY = CURRENT_DEPTH*TAN(TWOPI/120.)-CALG_CRACKWD               532
         AG_DZ = CURRENT_DEPTH/TAN_THETA/2-ZZ0/2                          532
         AG_TITLE = 'BOX'                                                 532
         AG_EXNAME = 'SHAPE'                                              532
         AG_PARLIST = 'DX_DY_DZ'                                          532
         CALL AXSHAPE                                                     532
C    *                                                                    532
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              532
C    Create CSMG :                                                        534
C    Call To Local Procedure CSMG                                         534
         ASSIGN 5470 TO BLOCK_CSMG                                        534
         GO TO  5460                                                      534
5470     CONTINUE                                                         534
C    *                                                                    534
C    Check CSEN_VERSION .GT. 0                                            535
         IF (CSEN_VERSION .GT. 0) THEN                                    535
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    535
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    535
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    535
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    535
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    535
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      535
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      535
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      535
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      535
         ENDIF                                                            535
C    Geant POSITION : CSMG ===> X=-SMD_WIDTH+CALG_G10SBTHK+2.*CALG_ABPAP  536
C    THK <===                                                             536
         CALL AGSRESET                                                    536
         AG_X = -SMD_WIDTH+CALG_G10SBTHK+2.*CALG_ABPAPTHK                 536
         AG_TITLE = 'CSMG'                                                536
         AG_EXNAME = 'POSITION'                                           536
         AG_PARLIST = 'X'                                                 536
         CALL AXPOSITION                                                  536
C    *                                                                    536
C    Geant POSITION : CSMG ===> X=SMD_WIDTH-CALG_G10SBTHK-2.*CALG_ABPAPT  536
C    HK <===                                                              536
         CALL AGSRESET                                                    537
         AG_X = SMD_WIDTH-CALG_G10SBTHK-2.*CALG_ABPAPTHK                  537
         AG_TITLE = 'CSMG'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'X'                                                 537
         CALL AXPOSITION                                                  537
C    *                                                                    537
         CURRENT = CURRENT + 2.*CALG_G10SBTHK + 2.*CALG_ABPAPTHK          537
C    *                                                                    539
         J=1                                                              539
            GO TO 5483                                                    539
5481        J=J+1                                                         539
5483        IF(J-(4).GT.0) GO TO 5482                                     539
            CURRENT_CSDA=-CURRENT_DEPTH/TAN_THETA/2                       540
C       Check J.EQ.1                                                      541
            IF (J.EQ.1) THEN                                              541
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA1WDH+CALG_SET12WDH)        542
C       Create CSDA :                                                     543
C       Call To Local Procedure CSDA                                      543
            ASSIGN 5500 TO BLOCK_CSDA                                     543
            GO TO  5490                                                   543
5500        CONTINUE                                                      543
C       *                                                                 543
C       Geant POSITION : CSDA ===> X=CURRENT+CALG_SMALFTHK Z=CURRENT_CSD  545
C       A+2.*CALG_SMETAWDH+ETA_LENGHT+ZZ0/2 <===                          545
            CALL AGSRESET                                                 545
            AG_X = CURRENT+CALG_SMALFTHK                                  545
            AG_Z = CURRENT_CSDA+2.*CALG_SMETAWDH+ETA_LENGHT+ZZ0/2         545
            AG_TITLE = 'CSDA'                                             545
            AG_EXNAME = 'POSITION'                                        545
            AG_PARLIST = 'X_Z'                                            545
            CALL AXPOSITION                                               545
C       *                                                                 545
            ELSEIF (J.EQ.2) THEN                                          545
            CURRENT_CSDA=CURRENT_CSDA+2.*(CALG_SMETAWDH+ETA_LENGHT)       547
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA2WDH+CALG_SET12WDH)        548
C       Create CSDA :                                                     549
C       Call To Local Procedure CSDA                                      549
            ASSIGN 5510 TO BLOCK_CSDA                                     549
            GO TO  5490                                                   549
5510        CONTINUE                                                      549
C       *                                                                 549
C       Geant POSITION : CSDA ===> X=CURRENT+CALG_SMALFTHK Z=CURRENT_CSD  550
C       A+ETA_LENGHT+ZZ0/2 <===                                           550
            CALL AGSRESET                                                 550
            AG_X = CURRENT+CALG_SMALFTHK                                  550
            AG_Z = CURRENT_CSDA+ETA_LENGHT+ZZ0/2                          550
            AG_TITLE = 'CSDA'                                             550
            AG_EXNAME = 'POSITION'                                        550
            AG_PARLIST = 'X_Z'                                            550
            CALL AXPOSITION                                               550
C       *                                                                 550
            ELSEIF (J.EQ.3) THEN                                          550
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA1WDH+CALG_SET12WDH)        552
C       Create CSDA :                                                     553
C       Call To Local Procedure CSDA                                      553
            ASSIGN 5520 TO BLOCK_CSDA                                     553
            GO TO  5490                                                   553
5520        CONTINUE                                                      553
C       *                                                                 553
C       Geant POSITION : CSDA ===> X=CURRENT+3.*CALG_SMALFTHK Z=CURRENT_  555
C       CSDA+2.*CALG_SMETAWDH+ETA_LENGHT+ZZ0/2 THETAX=90 PHIX=180 <===    555
            CALL AGSRESET                                                 555
            AG_X = CURRENT+3.*CALG_SMALFTHK                               555
            AG_Z = CURRENT_CSDA+2.*CALG_SMETAWDH+ETA_LENGHT+ZZ0/2         555
            AG_THETAX = 90                                                555
            AG_PHIX = 180                                                 555
            AG_TITLE = 'CSDA'                                             555
            AG_EXNAME = 'POSITION'                                        555
            AG_PARLIST = 'X_Z_THETAX_PHIX'                                555
            CALL AXPOSITION                                               555
C       *                                                                 555
            ELSEIF (J.EQ.4) THEN                                          555
            CURRENT_CSDA=CURRENT_CSDA+2.*(CALG_SMETAWDH+ETA_LENGHT)       557
            ETA_LENGHT=CALG_NETFIRST*(CALG_SETA2WDH+CALG_SET12WDH)        558
C       Create CSDA :                                                     559
C       Call To Local Procedure CSDA                                      559
            ASSIGN 5530 TO BLOCK_CSDA                                     559
            GO TO  5490                                                   559
5530        CONTINUE                                                      559
C       *                                                                 559
C       Geant POSITION : CSDA ===> X=CURRENT+3.*CALG_SMALFTHK Z=CURRENT_  561
C       CSDA+ETA_LENGHT+ZZ0/2 THETAX=90 PHIX=180 <===                     561
            CALL AGSRESET                                                 561
            AG_X = CURRENT+3.*CALG_SMALFTHK                               561
            AG_Z = CURRENT_CSDA+ETA_LENGHT+ZZ0/2                          561
            AG_THETAX = 90                                                561
            AG_PHIX = 180                                                 561
            AG_TITLE = 'CSDA'                                             561
            AG_EXNAME = 'POSITION'                                        561
            AG_PARLIST = 'X_Z_THETAX_PHIX'                                561
            CALL AXPOSITION                                               561
C       *                                                                 561
            ENDIF                                                         561
         GO TO 5481                                                       563
5482     CONTINUE                                                         563
C    Create CSMC :                                                        564
C    Call To Local Procedure CSMC                                         564
         ASSIGN 5550 TO BLOCK_CSMC                                        564
         GO TO  5540                                                      564
5550     CONTINUE                                                         564
C    *                                                                    564
C    Geant POSITION : CSMC ===> X=CURRENT+CALG_SMALFTHK Y=CALG_SMALFWDH+  565
C    CALG_SMAFFWDH <===                                                   565
         CALL AGSRESET                                                    565
         AG_X = CURRENT+CALG_SMALFTHK                                     565
         AG_Y = CALG_SMALFWDH+CALG_SMAFFWDH                               565
         AG_TITLE = 'CSMC'                                                565
         AG_EXNAME = 'POSITION'                                           565
         AG_PARLIST = 'X_Y'                                               565
         CALL AXPOSITION                                                  565
C    *                                                                    565
C    Geant POSITION : CSMC ===> X=CURRENT+CALG_SMALFTHK Y=-CALG_SMALFWDH  565
C    -CALG_SMAFFWDH <===                                                  565
         CALL AGSRESET                                                    566
         AG_X = CURRENT+CALG_SMALFTHK                                     566
         AG_Y = -CALG_SMALFWDH-CALG_SMAFFWDH                              566
         AG_TITLE = 'CSMC'                                                566
         AG_EXNAME = 'POSITION'                                           566
         AG_PARLIST = 'X_Y'                                               566
         CALL AXPOSITION                                                  566
C    *                                                                    566
C    Create CSMB :                                                        567
C    Call To Local Procedure CSMB                                         567
         ASSIGN 5570 TO BLOCK_CSMB                                        567
         GO TO  5560                                                      567
5570     CONTINUE                                                         567
C    *                                                                    567
C    Geant POSITION : CSMB ===> X=CURRENT+3.*CALG_SMALFTHK Y=CALG_SMALFW  568
C    DH+CALG_SMAFBWDH <===                                                568
         CALL AGSRESET                                                    568
         AG_X = CURRENT+3.*CALG_SMALFTHK                                  568
         AG_Y = CALG_SMALFWDH+CALG_SMAFBWDH                               568
         AG_TITLE = 'CSMB'                                                568
         AG_EXNAME = 'POSITION'                                           568
         AG_PARLIST = 'X_Y'                                               568
         CALL AXPOSITION                                                  568
C    *                                                                    568
C    Geant POSITION : CSMB ===> X=CURRENT+3.*CALG_SMALFTHK Y=-CALG_SMALF  568
C    WDH-CALG_SMAFBWDH <===                                               568
         CALL AGSRESET                                                    569
         AG_X = CURRENT+3.*CALG_SMALFTHK                                  569
         AG_Y = -CALG_SMALFWDH-CALG_SMAFBWDH                              569
         AG_TITLE = 'CSMB'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'X_Y'                                               569
         CALL AXPOSITION                                                  569
C    *                                                                    569
5441  CALL AGSPOPB                                                        570
      GO TO BLOCK_CSMD, (5440 ,5450)                                      570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ CSMG                                                       571
C Begin Local Procedure CSMG                                              572
C CSMG IS G10 FRONT BACK PLATE                                            572
5460  CONTINUE                                                            572
         AG_TITLE='CSMG'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          573
         CALL AGSRESET                                                    573
         AG_A = 28.08                                                     573
         AG_Z = 14                                                        573
         AG_W = 0.6*1*28./60.                                             573
         AG_TITLE = 'SI'                                                  573
         AG_EXNAME = 'COMPONENT'                                          573
         AG_PARLIST = 'A_Z_W'                                             573
         CALL AXCOMPONENT                                                 573
C    *                                                                    573
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               573
         CALL AGSRESET                                                    574
         AG_A = 16                                                        574
         AG_Z = 8                                                         574
         AG_W = 0.6*2*16./60.                                             574
         AG_TITLE = 'O'                                                   574
         AG_EXNAME = 'COMPONENT'                                          574
         AG_PARLIST = 'A_Z_W'                                             574
         CALL AXCOMPONENT                                                 574
C    *                                                                    574
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              574
         CALL AGSRESET                                                    575
         AG_A = 12                                                        575
         AG_Z = 6                                                         575
         AG_W = 0.4*8*12./174.                                            575
         AG_TITLE = 'C'                                                   575
         AG_EXNAME = 'COMPONENT'                                          575
         AG_PARLIST = 'A_Z_W'                                             575
         CALL AXCOMPONENT                                                 575
C    *                                                                    575
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               575
         CALL AGSRESET                                                    576
         AG_A = 1                                                         576
         AG_Z = 1                                                         576
         AG_W = 0.4*14*1./174.                                            576
         AG_TITLE = 'H'                                                   576
         AG_EXNAME = 'COMPONENT'                                          576
         AG_PARLIST = 'A_Z_W'                                             576
         CALL AXCOMPONENT                                                 576
C    *                                                                    576
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              576
         CALL AGSRESET                                                    577
         AG_A = 16                                                        577
         AG_Z = 8                                                         577
         AG_W = 0.4*4*16./174.                                            577
         AG_TITLE = 'O'                                                   577
         AG_EXNAME = 'COMPONENT'                                          577
         AG_PARLIST = 'A_Z_W'                                             577
         CALL AXCOMPONENT                                                 577
C    *                                                                    577
C    Geant MIXTURE : G10 ===> ISVOL=0 DENS=1.7 <===                       577
         CALL AGSRESET                                                    578
         AG_ISVOL = 0                                                     578
         AG_DENS = 1.7                                                    578
         AG_TITLE = 'G10'                                                 578
         AG_EXNAME = 'MIXTURE'                                            578
         AG_PARLIST = 'ISVOL_DENS'                                        578
         CALL AXMIXTURE                                                   578
C    *                                                                    578
C    Geant ATTRIBUTE : CSMG ===> SEEN=1 COLO=1 <===                       578
         CALL AGSRESET                                                    579
         AG_SEEN = 1                                                      579
         AG_COLO = 1                                                      579
         AG_TITLE = 'CSMG'                                                579
         AG_EXNAME = 'ATTRIBUTE'                                          579
         AG_PARLIST = 'SEEN_COLO'                                         579
         CALL AXATTRIBUTE                                                 579
C    *                                                                    579
C    Geant SHAPE : BOX ===> DX=CALG_G10SBTHK <===                         579
         CALL AGSRESET                                                    580
         AG_DX = CALG_G10SBTHK                                            580
         AG_TITLE = 'BOX'                                                 580
         AG_EXNAME = 'SHAPE'                                              580
         AG_PARLIST = 'DX'                                                580
         CALL AXSHAPE                                                     580
C    *                                                                    580
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              580
C    Check CSEN_VERSION .GT. 0                                            582
         IF (CSEN_VERSION .GT. 0) THEN                                    582
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    582
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    582
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    582
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    582
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    582
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      582
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      582
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      582
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      582
         ENDIF                                                            582
5461  CALL AGSPOPB                                                        583
      GO TO BLOCK_CSMG, (5460 ,5470)                                      583
C End of Local Procedure                                                  583
C ----------------------------------------------------------------------  584
C ------------ CSDA                                                       584
C Begin Local Procedure CSDA                                              585
C CSDA IS AL BLOCK WITH SENSITIVE GAS VOLUME                              585
5490  CONTINUE                                                            585
         AG_TITLE='CSDA'                                                  585
         AG_MODULE=MODULE                                                 585
         AG_MARK='V00'                                                    585
         CALL AGSPUSH                                                     585
         AG_MODULE=MODULE                                                 585
C    *                                                                    585
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        586
         CALL AGSRESET                                                    586
         AG_TITLE = 'ALUMINIUM'                                           586
         AG_EXNAME = 'MATERIAL'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXMATERIAL                                                  586
C    *                                                                    586
C    Geant MATERIAL : ALU_CSDA ===> ISVOL=0 <===                          586
         CALL AGSRESET                                                    587
         AG_ISVOL = 0                                                     587
         AG_TITLE = 'ALU_CSDA'                                            587
         AG_EXNAME = 'MATERIAL'                                           587
         AG_PARLIST = 'ISVOL'                                             587
         CALL AXMATERIAL                                                  587
C    *                                                                    587
C    Geant ATTRIBUTE : CSDA ===> SEEN=1 COLO=6 SERIAL=J <===              587
         CALL AGSRESET                                                    588
         AG_SEEN = 1                                                      588
         AG_COLO = 6                                                      588
         AG_SERIAL = J                                                    588
         AG_TITLE = 'CSDA'                                                588
         AG_EXNAME = 'ATTRIBUTE'                                          588
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  588
         CALL AXATTRIBUTE                                                 588
C    *                                                                    588
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMALFWDH, DZ=ETA_LE  588
C    NGHT-ZZ0/2 <===                                                      588
         CALL AGSRESET                                                    590
         AG_DX = CALG_SMALFTHK                                            590
         AG_DY = CALG_SMALFWDH                                            590
         AG_DZ = ETA_LENGHT-ZZ0/2                                         590
         AG_TITLE = 'BOX'                                                 590
         AG_EXNAME = 'SHAPE'                                              590
         AG_PARLIST = 'DX_DY_DZ'                                          590
         CALL AXSHAPE                                                     590
C    *                                                                    590
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              590
C    Create CSME :                                                        592
C    Call To Local Procedure CSME                                         592
         ASSIGN 5590 TO BLOCK_CSME                                        592
         GO TO  5580                                                      592
5590     CONTINUE                                                         592
C    *                                                                    592
C    Check CSEN_VERSION .GT. 0                                            593
         IF (CSEN_VERSION .GT. 0) THEN                                    593
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    593
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    593
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    593
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    593
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    593
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      593
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      593
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      593
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      593
         ENDIF                                                            593
5491  CALL AGSPOPB                                                        594
      GO TO BLOCK_CSDA, (5490 ,5500,5510,5520,5530)                       594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ CSMC                                                       595
C Begin Local Procedure CSMC                                              596
C CSMC IS THE FRONT FIRST (LAST) AL RIB                                   596
5540  CONTINUE                                                            596
         AG_TITLE='CSMC'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        597
         CALL AGSRESET                                                    597
         AG_TITLE = 'ALUMINIUM'                                           597
         AG_EXNAME = 'MATERIAL'                                           597
         AG_PARLIST = 'NONE'                                              597
         CALL AXMATERIAL                                                  597
C    *                                                                    597
C    Geant MATERIAL : ALU_CSMC ===> ISVOL=0 <===                          597
         CALL AGSRESET                                                    598
         AG_ISVOL = 0                                                     598
         AG_TITLE = 'ALU_CSMC'                                            598
         AG_EXNAME = 'MATERIAL'                                           598
         AG_PARLIST = 'ISVOL'                                             598
         CALL AXMATERIAL                                                  598
C    *                                                                    598
C    Geant ATTRIBUTE : CSMC ===> SEEN=1 COLO=6 <===                       598
         CALL AGSRESET                                                    599
         AG_SEEN = 1                                                      599
         AG_COLO = 6                                                      599
         AG_TITLE = 'CSMC'                                                599
         AG_EXNAME = 'ATTRIBUTE'                                          599
         AG_PARLIST = 'SEEN_COLO'                                         599
         CALL AXATTRIBUTE                                                 599
C    *                                                                    599
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMAFFWDH <===        599
         CALL AGSRESET                                                    600
         AG_DX = CALG_SMALFTHK                                            600
         AG_DY = CALG_SMAFFWDH                                            600
         AG_TITLE = 'BOX'                                                 600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'DX_DY'                                             600
         CALL AXSHAPE                                                     600
C    *                                                                    600
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              600
C    Check CSEN_VERSION .GT. 0                                            602
         IF (CSEN_VERSION .GT. 0) THEN                                    602
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    602
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    602
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    602
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    602
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    602
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      602
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      602
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      602
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      602
         ENDIF                                                            602
5541  CALL AGSPOPB                                                        603
      GO TO BLOCK_CSMC, (5540 ,5550)                                      603
C End of Local Procedure                                                  603
C ----------------------------------------------------------------------  604
C ------------ CSMB                                                       604
C Begin Local Procedure CSMB                                              605
C CSMB IS THE BACK FIRST (LAST) AL RIB                                    605
5560  CONTINUE                                                            605
         AG_TITLE='CSMB'                                                  605
         AG_MODULE=MODULE                                                 605
         AG_MARK='V00'                                                    605
         CALL AGSPUSH                                                     605
         AG_MODULE=MODULE                                                 605
C    *                                                                    605
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        606
         CALL AGSRESET                                                    606
         AG_TITLE = 'ALUMINIUM'                                           606
         AG_EXNAME = 'MATERIAL'                                           606
         AG_PARLIST = 'NONE'                                              606
         CALL AXMATERIAL                                                  606
C    *                                                                    606
C    Geant MATERIAL : ALU_CSMB ===> ISVOL=0 <===                          606
         CALL AGSRESET                                                    607
         AG_ISVOL = 0                                                     607
         AG_TITLE = 'ALU_CSMB'                                            607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'ISVOL'                                             607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant ATTRIBUTE : CSMB ===> SEEN=1 COLO=6 <===                       607
         CALL AGSRESET                                                    608
         AG_SEEN = 1                                                      608
         AG_COLO = 6                                                      608
         AG_TITLE = 'CSMB'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO'                                         608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant SHAPE : BOX ===> DX=CALG_SMALFTHK DY=CALG_SMAFBWDH <===        608
         CALL AGSRESET                                                    609
         AG_DX = CALG_SMALFTHK                                            609
         AG_DY = CALG_SMAFBWDH                                            609
         AG_TITLE = 'BOX'                                                 609
         AG_EXNAME = 'SHAPE'                                              609
         AG_PARLIST = 'DX_DY'                                             609
         CALL AXSHAPE                                                     609
C    *                                                                    609
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              609
C    Check CSEN_VERSION .GT. 0                                            611
         IF (CSEN_VERSION .GT. 0) THEN                                    611
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    611
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    611
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    611
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    611
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    611
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      611
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      611
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      611
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      611
         ENDIF                                                            611
5561  CALL AGSPOPB                                                        612
      GO TO BLOCK_CSMB, (5560 ,5570)                                      612
C End of Local Procedure                                                  612
C ----------------------------------------------------------------------  613
C ------------ CSME                                                       613
C Begin Local Procedure CSME                                              614
C CSME IS THE PART OF CSDA AL BOX WITH AR/CO2 SENSITEVE GAS               614
5580  CONTINUE                                                            614
         AG_TITLE='CSME'                                                  614
         AG_MODULE=MODULE                                                 614
         AG_MARK='V00'                                                    614
         CALL AGSPUSH                                                     614
         AG_MODULE=MODULE                                                 614
C    *                                                                    614
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        615
         CALL AGSRESET                                                    615
         AG_TITLE = 'ALUMINIUM'                                           615
         AG_EXNAME = 'MATERIAL'                                           615
         AG_PARLIST = 'NONE'                                              615
         CALL AXMATERIAL                                                  615
C    *                                                                    615
C    Geant MATERIAL : ALU_CSME ===> ISVOL=0 <===                          615
         CALL AGSRESET                                                    616
         AG_ISVOL = 0                                                     616
         AG_TITLE = 'ALU_CSME'                                            616
         AG_EXNAME = 'MATERIAL'                                           616
         AG_PARLIST = 'ISVOL'                                             616
         CALL AXMATERIAL                                                  616
C    *                                                                    616
C    Geant ATTRIBUTE : CSME ===> SEEN=1 COLO=6 <===                       616
         CALL AGSRESET                                                    617
         AG_SEEN = 1                                                      617
         AG_COLO = 6                                                      617
         AG_TITLE = 'CSME'                                                617
         AG_EXNAME = 'ATTRIBUTE'                                          617
         AG_PARLIST = 'SEEN_COLO'                                         617
         CALL AXATTRIBUTE                                                 617
C    *                                                                    617
C    Geant SHAPE : DIVISION ===> NDIV=NINT(CALG_NSMDALW) IAXIS=2 <===     617
         CALL AGSRESET                                                    618
         AG_NDIV = NINT(CALG_NSMDALW)                                     618
         AG_IAXIS = 2                                                     618
         AG_TITLE = 'DIVISION'                                            618
         AG_EXNAME = 'SHAPE'                                              618
         AG_PARLIST = 'NDIV_IAXIS'                                        618
         CALL AXSHAPE                                                     618
C    *                                                                    618
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              618
C    Check CSEN_VERSION .GT. 0                                            620
         IF (CSEN_VERSION .GT. 0) THEN                                    620
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    620
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    620
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    620
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    620
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    620
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      620
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      620
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      620
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      620
         ENDIF                                                            620
C    *                                                                    621
         ICSHI=1                                                          621
            GO TO 5603                                                    621
5601        ICSHI=ICSHI+1                                                 621
5603        IF(ICSHI-(2).GT.0) GO TO 5602                                 621
C       Create CSHI :                                                     622
C       Call To Local Procedure CSHI                                      622
            ASSIGN 5620 TO BLOCK_CSHI                                     622
            GO TO  5610                                                   622
5620        CONTINUE                                                      622
C       *                                                                 622
C       Check ICSHI.EQ.1                                                  623
            IF (ICSHI.EQ.1) THEN                                          623
C       Geant POSITION : CSHI ===> X=-CALG_SMALFTHK+CALG_SMGASTHK <===    624
            CALL AGSRESET                                                 624
            AG_X = -CALG_SMALFTHK+CALG_SMGASTHK                           624
            AG_TITLE = 'CSHI'                                             624
            AG_EXNAME = 'POSITION'                                        624
            AG_PARLIST = 'X'                                              624
            CALL AXPOSITION                                               624
C       *                                                                 624
            ELSE                                                          624
C       Geant POSITION : CSHI ===> X=2.*CALG_SMGASTHK-CALG_SMALFTHK <===  626
            CALL AGSRESET                                                 626
            AG_X = 2.*CALG_SMGASTHK-CALG_SMALFTHK                         626
            AG_TITLE = 'CSHI'                                             626
            AG_EXNAME = 'POSITION'                                        626
            AG_PARLIST = 'X'                                              626
            CALL AXPOSITION                                               626
C       *                                                                 626
            ENDIF                                                         626
         GO TO 5601                                                       628
5602     CONTINUE                                                         628
5581  CALL AGSPOPB                                                        629
      GO TO BLOCK_CSME, (5580 ,5590)                                      629
C End of Local Procedure                                                  629
C ----------------------------------------------------------------------  630
C ------------ CSHI                                                       630
C Begin Local Procedure CSHI                                              631
C CSHI IS A SENSITEVE AR/CO2 BOX                                          631
5610  CONTINUE                                                            631
         AG_TITLE='CSHI'                                                  631
         AG_MODULE=MODULE                                                 631
         AG_MARK='V00'                                                    631
         CALL AGSPUSH                                                     631
         AG_MODULE=MODULE                                                 631
C    *                                                                    631
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   632
         CALL AGSRESET                                                    632
         AG_A = 39.95                                                     632
         AG_Z = 18.                                                       632
         AG_W = 0.9                                                       632
         AG_TITLE = 'AR'                                                  632
         AG_EXNAME = 'COMPONENT'                                          632
         AG_PARLIST = 'A_Z_W'                                             632
         CALL AXCOMPONENT                                                 632
C    *                                                                    632
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       632
         CALL AGSRESET                                                    633
         AG_A = 12.01                                                     633
         AG_Z = 6.                                                        633
         AG_W = 0.1*1*12.01/44.01                                         633
         AG_TITLE = 'C'                                                   633
         AG_EXNAME = 'COMPONENT'                                          633
         AG_PARLIST = 'A_Z_W'                                             633
         CALL AXCOMPONENT                                                 633
C    *                                                                    633
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           633
         CALL AGSRESET                                                    634
         AG_A = 16.                                                       634
         AG_Z = 8.                                                        634
         AG_W = 0.1*2*16./44.01                                           634
         AG_TITLE = 'O'                                                   634
         AG_EXNAME = 'COMPONENT'                                          634
         AG_PARLIST = 'A_Z_W'                                             634
         CALL AXCOMPONENT                                                 634
C    *                                                                    634
C    Geant MIXTURE : SENS_GAS ===> ISVOL=1 DENS=0.0018015 <===            634
         CALL AGSRESET                                                    635
         AG_ISVOL = 1                                                     635
         AG_DENS = 0.0018015                                              635
         AG_TITLE = 'SENS_GAS'                                            635
         AG_EXNAME = 'MIXTURE'                                            635
         AG_PARLIST = 'ISVOL_DENS'                                        635
         CALL AXMIXTURE                                                   635
C    *                                                                    635
C    Geant ATTRIBUTE : CSHI ===> SEEN=1 COLO=4 <===                       635
         CALL AGSRESET                                                    636
         AG_SEEN = 1                                                      636
         AG_COLO = 4                                                      636
         AG_TITLE = 'CSHI'                                                636
         AG_EXNAME = 'ATTRIBUTE'                                          636
         AG_PARLIST = 'SEEN_COLO'                                         636
         CALL AXATTRIBUTE                                                 636
C    *                                                                    636
C    Check ICSHI.EQ.1                                                     636
         IF (ICSHI.EQ.1) THEN                                             637
C    Geant SHAPE : BOX ===> DX=CALG_SMGASTHK DY=CALG_SMGASWDH <===        638
         CALL AGSRESET                                                    638
         AG_DX = CALG_SMGASTHK                                            638
         AG_DY = CALG_SMGASWDH                                            638
         AG_TITLE = 'BOX'                                                 638
         AG_EXNAME = 'SHAPE'                                              638
         AG_PARLIST = 'DX_DY'                                             638
         CALL AXSHAPE                                                     638
C    *                                                                    638
         ELSE                                                             638
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=CALG_SMGASRAD PHI1=270, PHI2=45  641
C    0 <===                                                               641
         CALL AGSRESET                                                    641
         AG_RMIN = 0                                                      641
         AG_RMAX = CALG_SMGASRAD                                          641
         AG_PHI1 = 270                                                    641
         AG_PHI2 = 450                                                    641
         AG_TITLE = 'TUBS'                                                641
         AG_EXNAME = 'SHAPE'                                              641
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2'                               641
         CALL AXSHAPE                                                     641
C    *                                                                    641
         ENDIF                                                            641
C    Check CSEN_VERSION .GT. 0                                            643
         IF (CSEN_VERSION .GT. 0) THEN                                    643
         CALL GSTPAR( AG_IMED, 'CUTGAM', CSEN_CUTGAM )                    643
         CALL GSTPAR( AG_IMED, 'CUTELE', CSEN_CUTELE )                    643
         CALL GSTPAR( AG_IMED, 'CUTNEU', CSEN_CUTNEU )                    643
         CALL GSTPAR( AG_IMED, 'CUTHAD', CSEN_CUTHAD )                    643
         CALL GSTPAR( AG_IMED, 'CUTMUO', CSEN_CUTMUO )                    643
         CALL GSTPAR( AG_IMED, 'DCUTE', CSEN_DCUTE )                      643
         CALL GSTPAR( AG_IMED, 'DCUTM', CSEN_DCUTM )                      643
         CALL GSTPAR( AG_IMED, 'BCUTE', CSEN_BCUTE )                      643
         CALL GSTPAR( AG_IMED, 'BCUTM', CSEN_BCUTM )                      643
         ENDIF                                                            643
         CALL CALBPAR( AG_IMED,'SENSITIVE' )                              644
         CALL GSTPAR(AG_IMED, 'STRA', 1.0)                                645
         H_ETA1=2.*(CALG_SETA1WDH + CALG_SET12WDH)                        646
         SH_ETA1=CALG_NETFIRST*H_ETA1                                     647
         H_ETA2=2.*(CALG_SETA2WDH + CALG_SET12WDH)                        648
         SH_ETA2=CALG_NETSECON*H_ETA2                                     649
         H_PHI1=2.*(CALG_SPHIWDH + CALG_SPHIDWDH)                         650
         H_PHI2=H_PHI1                                                    651
         SH_PHI1=CALG_NPHISTR*H_PHI1                                      652
         SH_PHI2=SH_PHI1                                                  653
C    Defining Hits: TYPE=1:2: ETA:0.1:(0,1) ETSP:H_ETA1:(0,SH_ETA1) ELOS  654
C    S:0:(0,1)                                                            654
         AG_TITLE = 'CSDA'                                                654
         AG_EXNAME = 'HITS'                                               654
         AG_TYPE=1                                                        654
         CALL AGSDETV('CSDA')                                             654
C    *                                                                    654
         AG_PARLIST='TYPE'                                                654
         AG_TITLE = 'DETH'                                                654
         AG_NBITS = 2                                                     654
         AG_BIN=AG_MAGIC                                                  654
         AG_FHMIN=AG_MAGIC                                                654
         AG_FHMAX=AG_MAGIC                                                654
         AG_OPTION='*'                                                    654
         CALL AGSDETH (0)                                                 654
C    *                                                                    654
         AG_PARLIST='ETA'                                                 654
         AG_TITLE = 'DETH'                                                654
         AG_NBITS = AG_MAGIC                                              654
         AG_BIN=0.1                                                       654
         AG_OPTION='*'                                                    654
         AG_FHMIN = 0                                                     654
         AG_FHMAX = 1                                                     654
         CALL AGSDETH (0)                                                 654
C    *                                                                    654
         AG_PARLIST='ETSP'                                                654
         AG_TITLE = 'DETH'                                                654
         AG_NBITS = AG_MAGIC                                              654
         AG_BIN=H_ETA1                                                    654
         AG_OPTION='*'                                                    654
         AG_FHMIN = 0                                                     654
         AG_FHMAX = SH_ETA1                                               654
         CALL AGSDETH (0)                                                 654
C    *                                                                    654
         AG_PARLIST='ELOSS'                                               654
         AG_TITLE = 'DETH'                                                654
         AG_NBITS = 0                                                     654
         AG_BIN=AG_MAGIC                                                  654
         AG_OPTION='*'                                                    654
         AG_FHMIN = 0                                                     654
         AG_FHMAX = 1                                                     654
         CALL AGSDETH (0)                                                 654
C    *                                                                    654
         CALL AGSDETU('CSDA')                                             654
C    *                                                                    654
C    Defining Hits: TYPE=2:2: ETA:0.1:(0,1) ETSP:H_ETA2:(0,SH_ETA2) ELOS  655
C    S:0:(0,1)                                                            655
         AG_TITLE = 'CSDA'                                                655
         AG_EXNAME = 'HITS'                                               655
         AG_TYPE=2                                                        655
         CALL AGSDETV('CSDA')                                             655
C    *                                                                    655
         AG_PARLIST='TYPE'                                                655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 2                                                     655
         AG_BIN=AG_MAGIC                                                  655
         AG_FHMIN=AG_MAGIC                                                655
         AG_FHMAX=AG_MAGIC                                                655
         AG_OPTION='*'                                                    655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='ETA'                                                 655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = AG_MAGIC                                              655
         AG_BIN=0.1                                                       655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0                                                     655
         AG_FHMAX = 1                                                     655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='ETSP'                                                655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = AG_MAGIC                                              655
         AG_BIN=H_ETA2                                                    655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0                                                     655
         AG_FHMAX = SH_ETA2                                               655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         AG_PARLIST='ELOSS'                                               655
         AG_TITLE = 'DETH'                                                655
         AG_NBITS = 0                                                     655
         AG_BIN=AG_MAGIC                                                  655
         AG_OPTION='*'                                                    655
         AG_FHMIN = 0                                                     655
         AG_FHMAX = 1                                                     655
         CALL AGSDETH (0)                                                 655
C    *                                                                    655
         CALL AGSDETU('CSDA')                                             655
C    *                                                                    655
C    Defining Hits: TYPE=3:2: ETA:0.1:(0,1) ETSP:H_PHI1:(0,SH_PHI1) ELOS  656
C    S:0:(0,1)                                                            656
         AG_TITLE = 'CSDA'                                                656
         AG_EXNAME = 'HITS'                                               656
         AG_TYPE=3                                                        656
         CALL AGSDETV('CSDA')                                             656
C    *                                                                    656
         AG_PARLIST='TYPE'                                                656
         AG_TITLE = 'DETH'                                                656
         AG_NBITS = 2                                                     656
         AG_BIN=AG_MAGIC                                                  656
         AG_FHMIN=AG_MAGIC                                                656
         AG_FHMAX=AG_MAGIC                                                656
         AG_OPTION='*'                                                    656
         CALL AGSDETH (0)                                                 656
C    *                                                                    656
         AG_PARLIST='ETA'                                                 656
         AG_TITLE = 'DETH'                                                656
         AG_NBITS = AG_MAGIC                                              656
         AG_BIN=0.1                                                       656
         AG_OPTION='*'                                                    656
         AG_FHMIN = 0                                                     656
         AG_FHMAX = 1                                                     656
         CALL AGSDETH (0)                                                 656
C    *                                                                    656
         AG_PARLIST='ETSP'                                                656
         AG_TITLE = 'DETH'                                                656
         AG_NBITS = AG_MAGIC                                              656
         AG_BIN=H_PHI1                                                    656
         AG_OPTION='*'                                                    656
         AG_FHMIN = 0                                                     656
         AG_FHMAX = SH_PHI1                                               656
         CALL AGSDETH (0)                                                 656
C    *                                                                    656
         AG_PARLIST='ELOSS'                                               656
         AG_TITLE = 'DETH'                                                656
         AG_NBITS = 0                                                     656
         AG_BIN=AG_MAGIC                                                  656
         AG_OPTION='*'                                                    656
         AG_FHMIN = 0                                                     656
         AG_FHMAX = 1                                                     656
         CALL AGSDETH (0)                                                 656
C    *                                                                    656
         CALL AGSDETU('CSDA')                                             656
C    *                                                                    656
C    Defining Hits: TYPE=4:2: ETA:0.1:(0,1) ETSP:H_PHI2:(0,SH_PHI2) ELOS  657
C    S:0:(0,1)                                                            657
         AG_TITLE = 'CSDA'                                                657
         AG_EXNAME = 'HITS'                                               657
         AG_TYPE=4                                                        657
         CALL AGSDETV('CSDA')                                             657
C    *                                                                    657
         AG_PARLIST='TYPE'                                                657
         AG_TITLE = 'DETH'                                                657
         AG_NBITS = 2                                                     657
         AG_BIN=AG_MAGIC                                                  657
         AG_FHMIN=AG_MAGIC                                                657
         AG_FHMAX=AG_MAGIC                                                657
         AG_OPTION='*'                                                    657
         CALL AGSDETH (0)                                                 657
C    *                                                                    657
         AG_PARLIST='ETA'                                                 657
         AG_TITLE = 'DETH'                                                657
         AG_NBITS = AG_MAGIC                                              657
         AG_BIN=0.1                                                       657
         AG_OPTION='*'                                                    657
         AG_FHMIN = 0                                                     657
         AG_FHMAX = 1                                                     657
         CALL AGSDETH (0)                                                 657
C    *                                                                    657
         AG_PARLIST='ETSP'                                                657
         AG_TITLE = 'DETH'                                                657
         AG_NBITS = AG_MAGIC                                              657
         AG_BIN=H_PHI2                                                    657
         AG_OPTION='*'                                                    657
         AG_FHMIN = 0                                                     657
         AG_FHMAX = SH_PHI2                                               657
         CALL AGSDETH (0)                                                 657
C    *                                                                    657
         AG_PARLIST='ELOSS'                                               657
         AG_TITLE = 'DETH'                                                657
         AG_NBITS = 0                                                     657
         AG_BIN=AG_MAGIC                                                  657
         AG_OPTION='*'                                                    657
         AG_FHMIN = 0                                                     657
         AG_FHMAX = 1                                                     657
         CALL AGSDETH (0)                                                 657
C    *                                                                    657
         CALL AGSDETU('CSDA')                                             657
C    *                                                                    657
5611  CALL AGSPOPB                                                        658
      GO TO BLOCK_CSHI, (5610 ,5620)                                      658
C End of Local Procedure                                                  658
      WRITE(LOUT,5630)'===>>  [End AgML Module CALBGEO2]  <<==='          658
5630  FORMAT(A32,/,/)                                                     658
C MODULE CALBGEO2                                                         660
1     CONTINUE                                                            660
      END                                                                 660
