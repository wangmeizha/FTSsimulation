C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE QUADGEO                                                  2
C QUADGEO IS THE DESCRIPTION OF ALL THE MAGNETS UPSTREAM INCLUSIVE, OF D  2
C 0                                                                       2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'QUADGEO'/, BANK_TITLE/'*'/                             2
      DATA MCOMMENT/'IS THE DESCRIPTION OF ALL THE MAGNETS UPSTREAM INCL  2
     *USIVE, OF D0'/                                                      2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C CREATED 02-SEP-2005                                                     2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 CREATED /'02-SEP-2005'/                                3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C AUTHOR MAXIM POTEKHIN                                                   3
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              4
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            6
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      6
      COMMON/GCMAIL/ CHMAIL                                               6
      CHARACTER*132 CHMAIL                                                6
      INTEGER BLOCK_MGMT                                                  6
      INTEGER BLOCK_DZER                                                  7
      INTEGER BLOCK_QONE                                                  7
      INTEGER BLOCK_QTWO                                                  7
      INTEGER BLOCK_QTHR                                                  7
C -                                                                       9
      REAL SHLQ_VERSION                                                   9
      EQUIVALENCE (BANK_SHLQ(3),SHLQ_VERSION,RE_SHLQ_VERSION),            9
     *(NAMES_SHLQ(1),NSHLQ_VERSION)                                       9
      EQUIVALENCE (COMMEN_SHLQ(1),CSHLQ_VERSION),(MAP_SHLQ(1,1),          9
     *MSHLQ_VERSION)                                                      9
      CHARACTER CSHLQ_VERSION*40/' '/                                     9
      CHARACTER NSHLQ_VERSION*12/'R.VERSION'/                             9
      INTEGER MSHLQ_VERSION(2)/1,1/                                       9
      REAL RE_SHLQ_VERSION                                                9
C -                                                                       9
      REAL SHLQ_Q0                                                        9
      EQUIVALENCE (BANK_SHLQ(4),SHLQ_Q0,RE_SHLQ_Q0),(NAMES_SHLQ(2),       9
     *NSHLQ_Q0)                                                           9
      EQUIVALENCE (COMMEN_SHLQ(2),CSHLQ_Q0),(MAP_SHLQ(1,2),MSHLQ_Q0)      9
      CHARACTER CSHLQ_Q0*40/' '/                                          9
      CHARACTER NSHLQ_Q0*12/'R.Q0'/                                       9
      INTEGER MSHLQ_Q0(2)/1,1/                                            9
      REAL RE_SHLQ_Q0                                                     9
C -                                                                       9
      REAL SHLQ_MOTHERR                                                   9
      EQUIVALENCE (BANK_SHLQ(5),SHLQ_MOTHERR,RE_SHLQ_MOTHERR),            9
     *(NAMES_SHLQ(3),NSHLQ_MOTHERR)                                       9
      EQUIVALENCE (COMMEN_SHLQ(3),CSHLQ_MOTHERR),(MAP_SHLQ(1,3),          9
     *MSHLQ_MOTHERR)                                                      9
      CHARACTER CSHLQ_MOTHERR*40/' '/                                     9
      CHARACTER NSHLQ_MOTHERR*12/'R.MOTHERR'/                             9
      INTEGER MSHLQ_MOTHERR(2)/1,1/                                       9
      REAL RE_SHLQ_MOTHERR                                                9
C -                                                                       9
      REAL SHLQ_MOTHERL                                                   9
      EQUIVALENCE (BANK_SHLQ(6),SHLQ_MOTHERL,RE_SHLQ_MOTHERL),            9
     *(NAMES_SHLQ(4),NSHLQ_MOTHERL)                                       9
      EQUIVALENCE (COMMEN_SHLQ(4),CSHLQ_MOTHERL),(MAP_SHLQ(1,4),          9
     *MSHLQ_MOTHERL)                                                      9
      CHARACTER CSHLQ_MOTHERL*40/' '/                                     9
      CHARACTER NSHLQ_MOTHERL*12/'R.MOTHERL'/                             9
      INTEGER MSHLQ_MOTHERL(2)/1,1/                                       9
      REAL RE_SHLQ_MOTHERL                                                9
C -                                                                       9
      REAL SHLQ_XOFFSET                                                   9
      EQUIVALENCE (BANK_SHLQ(7),SHLQ_XOFFSET,RE_SHLQ_XOFFSET),            9
     *(NAMES_SHLQ(5),NSHLQ_XOFFSET)                                       9
      EQUIVALENCE (COMMEN_SHLQ(5),CSHLQ_XOFFSET),(MAP_SHLQ(1,5),          9
     *MSHLQ_XOFFSET)                                                      9
      CHARACTER CSHLQ_XOFFSET*40/' '/                                     9
      CHARACTER NSHLQ_XOFFSET*12/'R.XOFFSET'/                             9
      INTEGER MSHLQ_XOFFSET(2)/1,1/                                       9
      REAL RE_SHLQ_XOFFSET                                                9
C -                                                                       9
      REAL SHLQ_ANGLE                                                     9
      EQUIVALENCE (BANK_SHLQ(8),SHLQ_ANGLE,RE_SHLQ_ANGLE),                9
     *(NAMES_SHLQ(6),NSHLQ_ANGLE)                                         9
      EQUIVALENCE (COMMEN_SHLQ(6),CSHLQ_ANGLE),(MAP_SHLQ(1,6),            9
     *MSHLQ_ANGLE)                                                        9
      CHARACTER CSHLQ_ANGLE*40/' '/                                       9
      CHARACTER NSHLQ_ANGLE*12/'R.ANGLE'/                                 9
      INTEGER MSHLQ_ANGLE(2)/1,1/                                         9
      REAL RE_SHLQ_ANGLE                                                  9
C -                                                                       9
      REAL SHLQ_DZEROL                                                    9
      EQUIVALENCE (BANK_SHLQ(9),SHLQ_DZEROL,RE_SHLQ_DZEROL),              9
     *(NAMES_SHLQ(7),NSHLQ_DZEROL)                                        9
      EQUIVALENCE (COMMEN_SHLQ(7),CSHLQ_DZEROL),(MAP_SHLQ(1,7),           9
     *MSHLQ_DZEROL)                                                       9
      CHARACTER CSHLQ_DZEROL*40/' '/                                      9
      CHARACTER NSHLQ_DZEROL*12/'R.DZEROL'/                               9
      INTEGER MSHLQ_DZEROL(2)/1,1/                                        9
      REAL RE_SHLQ_DZEROL                                                 9
C -                                                                       9
      REAL SHLQ_DZERORI                                                   9
      EQUIVALENCE (BANK_SHLQ(10),SHLQ_DZERORI,RE_SHLQ_DZERORI),           9
     *(NAMES_SHLQ(8),NSHLQ_DZERORI)                                       9
      EQUIVALENCE (COMMEN_SHLQ(8),CSHLQ_DZERORI),(MAP_SHLQ(1,8),          9
     *MSHLQ_DZERORI)                                                      9
      CHARACTER CSHLQ_DZERORI*40/' '/                                     9
      CHARACTER NSHLQ_DZERORI*12/'R.DZERORI'/                             9
      INTEGER MSHLQ_DZERORI(2)/1,1/                                       9
      REAL RE_SHLQ_DZERORI                                                9
C -                                                                       9
      REAL SHLQ_DZERORO                                                   9
      EQUIVALENCE (BANK_SHLQ(11),SHLQ_DZERORO,RE_SHLQ_DZERORO),           9
     *(NAMES_SHLQ(9),NSHLQ_DZERORO)                                       9
      EQUIVALENCE (COMMEN_SHLQ(9),CSHLQ_DZERORO),(MAP_SHLQ(1,9),          9
     *MSHLQ_DZERORO)                                                      9
      CHARACTER CSHLQ_DZERORO*40/' '/                                     9
      CHARACTER NSHLQ_DZERORO*12/'R.DZERORO'/                             9
      INTEGER MSHLQ_DZERORO(2)/1,1/                                       9
      REAL RE_SHLQ_DZERORO                                                9
C -                                                                       9
      REAL SHLQ_Q1                                                        9
      EQUIVALENCE (BANK_SHLQ(12),SHLQ_Q1,RE_SHLQ_Q1),(NAMES_SHLQ(10),     9
     *NSHLQ_Q1)                                                           9
      EQUIVALENCE (COMMEN_SHLQ(10),CSHLQ_Q1),(MAP_SHLQ(1,10),MSHLQ_Q1)    9
      CHARACTER CSHLQ_Q1*40/' '/                                          9
      CHARACTER NSHLQ_Q1*12/'R.Q1'/                                       9
      INTEGER MSHLQ_Q1(2)/1,1/                                            9
      REAL RE_SHLQ_Q1                                                     9
C -                                                                       9
      REAL SHLQ_RI1                                                       9
      EQUIVALENCE (BANK_SHLQ(13),SHLQ_RI1,RE_SHLQ_RI1),(NAMES_SHLQ(11),   9
     *NSHLQ_RI1)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(11),CSHLQ_RI1),(MAP_SHLQ(1,11),            9
     *MSHLQ_RI1)                                                          9
      CHARACTER CSHLQ_RI1*40/' '/                                         9
      CHARACTER NSHLQ_RI1*12/'R.RI1'/                                     9
      INTEGER MSHLQ_RI1(2)/1,1/                                           9
      REAL RE_SHLQ_RI1                                                    9
C -                                                                       9
      REAL SHLQ_RO1                                                       9
      EQUIVALENCE (BANK_SHLQ(14),SHLQ_RO1,RE_SHLQ_RO1),(NAMES_SHLQ(12),   9
     *NSHLQ_RO1)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(12),CSHLQ_RO1),(MAP_SHLQ(1,12),            9
     *MSHLQ_RO1)                                                          9
      CHARACTER CSHLQ_RO1*40/' '/                                         9
      CHARACTER NSHLQ_RO1*12/'R.RO1'/                                     9
      INTEGER MSHLQ_RO1(2)/1,1/                                           9
      REAL RE_SHLQ_RO1                                                    9
C -                                                                       9
      REAL SHLQ_DZ1                                                       9
      EQUIVALENCE (BANK_SHLQ(15),SHLQ_DZ1,RE_SHLQ_DZ1),(NAMES_SHLQ(13),   9
     *NSHLQ_DZ1)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(13),CSHLQ_DZ1),(MAP_SHLQ(1,13),            9
     *MSHLQ_DZ1)                                                          9
      CHARACTER CSHLQ_DZ1*40/' '/                                         9
      CHARACTER NSHLQ_DZ1*12/'R.DZ1'/                                     9
      INTEGER MSHLQ_DZ1(2)/1,1/                                           9
      REAL RE_SHLQ_DZ1                                                    9
C -                                                                       9
      REAL SHLQ_Q2                                                        9
      EQUIVALENCE (BANK_SHLQ(16),SHLQ_Q2,RE_SHLQ_Q2),(NAMES_SHLQ(14),     9
     *NSHLQ_Q2)                                                           9
      EQUIVALENCE (COMMEN_SHLQ(14),CSHLQ_Q2),(MAP_SHLQ(1,14),MSHLQ_Q2)    9
      CHARACTER CSHLQ_Q2*40/' '/                                          9
      CHARACTER NSHLQ_Q2*12/'R.Q2'/                                       9
      INTEGER MSHLQ_Q2(2)/1,1/                                            9
      REAL RE_SHLQ_Q2                                                     9
C -                                                                       9
      REAL SHLQ_RI2                                                       9
      EQUIVALENCE (BANK_SHLQ(17),SHLQ_RI2,RE_SHLQ_RI2),(NAMES_SHLQ(15),   9
     *NSHLQ_RI2)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(15),CSHLQ_RI2),(MAP_SHLQ(1,15),            9
     *MSHLQ_RI2)                                                          9
      CHARACTER CSHLQ_RI2*40/' '/                                         9
      CHARACTER NSHLQ_RI2*12/'R.RI2'/                                     9
      INTEGER MSHLQ_RI2(2)/1,1/                                           9
      REAL RE_SHLQ_RI2                                                    9
C -                                                                       9
      REAL SHLQ_RO2                                                       9
      EQUIVALENCE (BANK_SHLQ(18),SHLQ_RO2,RE_SHLQ_RO2),(NAMES_SHLQ(16),   9
     *NSHLQ_RO2)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(16),CSHLQ_RO2),(MAP_SHLQ(1,16),            9
     *MSHLQ_RO2)                                                          9
      CHARACTER CSHLQ_RO2*40/' '/                                         9
      CHARACTER NSHLQ_RO2*12/'R.RO2'/                                     9
      INTEGER MSHLQ_RO2(2)/1,1/                                           9
      REAL RE_SHLQ_RO2                                                    9
C -                                                                       9
      REAL SHLQ_DZ2                                                       9
      EQUIVALENCE (BANK_SHLQ(19),SHLQ_DZ2,RE_SHLQ_DZ2),(NAMES_SHLQ(17),   9
     *NSHLQ_DZ2)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(17),CSHLQ_DZ2),(MAP_SHLQ(1,17),            9
     *MSHLQ_DZ2)                                                          9
      CHARACTER CSHLQ_DZ2*40/' '/                                         9
      CHARACTER NSHLQ_DZ2*12/'R.DZ2'/                                     9
      INTEGER MSHLQ_DZ2(2)/1,1/                                           9
      REAL RE_SHLQ_DZ2                                                    9
C -                                                                       9
      REAL SHLQ_Q3                                                        9
      EQUIVALENCE (BANK_SHLQ(20),SHLQ_Q3,RE_SHLQ_Q3),(NAMES_SHLQ(18),     9
     *NSHLQ_Q3)                                                           9
      EQUIVALENCE (COMMEN_SHLQ(18),CSHLQ_Q3),(MAP_SHLQ(1,18),MSHLQ_Q3)    9
      CHARACTER CSHLQ_Q3*40/' '/                                          9
      CHARACTER NSHLQ_Q3*12/'R.Q3'/                                       9
      INTEGER MSHLQ_Q3(2)/1,1/                                            9
      REAL RE_SHLQ_Q3                                                     9
C -                                                                       9
      REAL SHLQ_RI3                                                       9
      EQUIVALENCE (BANK_SHLQ(21),SHLQ_RI3,RE_SHLQ_RI3),(NAMES_SHLQ(19),   9
     *NSHLQ_RI3)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(19),CSHLQ_RI3),(MAP_SHLQ(1,19),            9
     *MSHLQ_RI3)                                                          9
      CHARACTER CSHLQ_RI3*40/' '/                                         9
      CHARACTER NSHLQ_RI3*12/'R.RI3'/                                     9
      INTEGER MSHLQ_RI3(2)/1,1/                                           9
      REAL RE_SHLQ_RI3                                                    9
C -                                                                       9
      REAL SHLQ_RO3                                                       9
      EQUIVALENCE (BANK_SHLQ(22),SHLQ_RO3,RE_SHLQ_RO3),(NAMES_SHLQ(20),   9
     *NSHLQ_RO3)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(20),CSHLQ_RO3),(MAP_SHLQ(1,20),            9
     *MSHLQ_RO3)                                                          9
      CHARACTER CSHLQ_RO3*40/' '/                                         9
      CHARACTER NSHLQ_RO3*12/'R.RO3'/                                     9
      INTEGER MSHLQ_RO3(2)/1,1/                                           9
      REAL RE_SHLQ_RO3                                                    9
C -                                                                       9
      REAL SHLQ_DZ3                                                       9
      EQUIVALENCE (BANK_SHLQ(23),SHLQ_DZ3,RE_SHLQ_DZ3),(NAMES_SHLQ(21),   9
     *NSHLQ_DZ3)                                                          9
      EQUIVALENCE (COMMEN_SHLQ(21),CSHLQ_DZ3),(MAP_SHLQ(1,21),            9
     *MSHLQ_DZ3)                                                          9
      CHARACTER CSHLQ_DZ3*40/' '/                                         9
      CHARACTER NSHLQ_DZ3*12/'R.DZ3'/                                     9
      INTEGER MSHLQ_DZ3(2)/1,1/                                           9
      REAL RE_SHLQ_DZ3                                                    9
C -                                                                       9
      INTEGER LEN_SHLQ,LLEN_SHLQ,LINK_SHLQ                                9
      PARAMETER (LEN_SHLQ=23)                                             9
      PARAMETER (LLEN_SHLQ=21)                                            9
      INTEGER FLAG_SHLQ/0/,MAP_SHLQ(2,LLEN_SHLQ),IDNUM_SHLQ(10)           9
      CHARACTER NAMES_SHLQ(LLEN_SHLQ)*12,COMMEN_SHLQ(LLEN_SHLQ)*40        9
      DATA IDNUM_SHLQ/10*0/                                               9
      REAL BANK_SHLQ(LEN_SHLQ)                                            9
      CHARACTER*60 BPATH_SHLQ/' '/                                        9
      SAVE FLAG_SHLQ,MAP_SHLQ,IDNUM_SHLQ,NAMES_SHLQ,COMMEN_SHLQ,          9
     *BPATH_SHLQ                                                          9
      DATA LINK_SHLQ/0/,BANK_SHLQ/LEN_SHLQ*0/                             9
C *                                                                       9
      REAL ZQUAD                                                          10
C *                                                                       12
      CALL AGSBEGM(MODULE,IPRIN)                                          12
      IF (IPRIN.LT.0) RETURN                                              12
C Filling bank SHLQ - QUADRUPOLE GEOMETRY DATA                            12
      BANK_TITLE='QUADRUPOLE GEOMETRY DATA'                               12
      BPATH_SHLQ='SHLQ*'                                                  12
      IDNUM_SHLQ(1)=0                                                     13
C GEOMETRY VERSION                                                        13
      SHLQ_VERSION=1                                                      13
      CSHLQ_VERSION='GEOMETRY VERSION'                                    13
C OFFSET POINT THAT CORRESPONDS TO 1505.92 IN CAD NOTATION, END OF D0     14
      SHLQ_Q0=2485.26                                                     14
      CSHLQ_Q0='OFFSET POINT THAT CORRESPONDS TO 1505.92 IN CAD NOTATION  14
     *, END OF D0'                                                        14
C RADIUS OF THE MOTHER CONTAINING D0,Q1,Q2,Q3                             15
      SHLQ_MOTHERR=19                                                     15
      CSHLQ_MOTHERR='RADIUS OF THE MOTHER CONTAINING D0,Q1,Q2,Q3'         15
C LENGTH OF THE MOTHER CONTAINING D0,Q1,Q2,Q3                             16
      SHLQ_MOTHERL=1610                                                   16
      CSHLQ_MOTHERL='LENGTH OF THE MOTHER CONTAINING D0,Q1,Q2,Q3'         16
C OFFSET OT THE MOTHER                                                    17
      SHLQ_XOFFSET=26                                                     17
      CSHLQ_XOFFSET='OFFSET OT THE MOTHER'                                17
C ANGLE TO THE SYMMETRY AXIS                                              18
      SHLQ_ANGLE=0.3                                                      18
      CSHLQ_ANGLE='ANGLE TO THE SYMMETRY AXIS'                            18
C D0 LENGTH                                                               19
      SHLQ_DZEROL=385.26                                                  19
      CSHLQ_DZEROL='D0 LENGTH'                                            19
C D0 INNER RADIUS                                                         20
      SHLQ_DZERORI=4.775                                                  20
      CSHLQ_DZERORI='D0 INNER RADIUS'                                     20
C D0 OUTER RADIUS                                                         21
      SHLQ_DZERORO=15.995                                                 21
      CSHLQ_DZERORO='D0 OUTER RADIUS'                                     21
C OFFSET 1                                                                22
      SHLQ_Q1=88.59                                                       22
      CSHLQ_Q1='OFFSET 1'                                                 22
C INNER 1                                                                 23
      SHLQ_RI1=6.355                                                      23
      CSHLQ_RI1='INNER 1'                                                 23
C OUTER 1                                                                 24
      SHLQ_RO1=18.28                                                      24
      CSHLQ_RO1='OUTER 1'                                                 24
C FULL LENGTH 1                                                           25
      SHLQ_DZ1=170.92                                                     25
      CSHLQ_DZ1='FULL LENGTH 1'                                           25
C OFFSET 2                                                                26
      SHLQ_Q2=324.90                                                      26
      CSHLQ_Q2='OFFSET 2'                                                 26
C INNER 2                                                                 27
      SHLQ_RI2=6.355                                                      27
      CSHLQ_RI2='INNER 2'                                                 27
C OUTER 2                                                                 28
      SHLQ_RO2=18.28                                                      28
      CSHLQ_RO2='OUTER 2'                                                 28
C FULL LENGTH 2                                                           29
      SHLQ_DZ2=444.02                                                     29
      CSHLQ_DZ2='FULL LENGTH 2'                                           29
C OFFSET 3                                                                30
      SHLQ_Q3=823.94                                                      30
      CSHLQ_Q3='OFFSET 3'                                                 30
C INNER 3                                                                 31
      SHLQ_RI3=6.355                                                      31
      CSHLQ_RI3='INNER 3'                                                 31
C OUTER 3                                                                 32
      SHLQ_RO3=18.28                                                      32
      CSHLQ_RO3='OUTER 3'                                                 32
C FULL LENGTH 3                                                           33
      SHLQ_DZ3=399.55                                                     33
      CSHLQ_DZ3='FULL LENGTH 3'                                           33
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLQ',    34
     *BPATH_SHLQ,IDNUM_SHLQ,1, MAP_SHLQ,NAMES_SHLQ,COMMEN_SHLQ,           34
     *BANK_SHLQ,LEN_SHLQ,LLEN_SHLQ,LINK_SHLQ,FLAG_SHLQ)                   34
C *                                                                       34
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLQ',    34
     *BPATH_SHLQ,IDNUM_SHLQ,1, MAP_SHLQ,NAMES_SHLQ,COMMEN_SHLQ,           34
     *BANK_SHLQ,LEN_SHLQ,LLEN_SHLQ,LINK_SHLQ,FLAG_SHLQ)                   34
C use default SHLQ bank                                                   35
      BPATH_SHLQ='SHLQ*'                                                  35
      IDNUM_SHLQ(1)=0                                                     35
      CALL AGDGETP('QUADGEO','*',-999,'SHLQ',' ',BANK_SHLQ, BPATH_SHLQ,   35
     *IDNUM_SHLQ,1,MAP_SHLQ,NAMES_SHLQ,BANK_SHLQ,LEN_SHLQ,LLEN_SHLQ,      35
     *LINK_SHLQ,FLAG_SHLQ)                                                35
      ZQUAD = SHLQ_Q0-SHLQ_DZEROL+SHLQ_MOTHERL/2.0                        36
      CALL AGSBEGM(MODULE,IPRIN)                                          37
C Check IPRIN<0                                                           37
      IF (IPRIN.LT.0) RETURN                                              37
C Create MGMT :                                                           37
C Call To Local Procedure MGMT                                            37
      ASSIGN 5020 TO BLOCK_MGMT                                           37
      GO TO  5010                                                         37
5020  CONTINUE                                                            37
C *                                                                       37
C Geant POSITION : MGMT ===> MOTHER='CAVE' X=SHLQ_XOFFSET Y=0 Z=ZQUAD AL  38
C PHAY=SHLQ_ANGLE <===                                                    38
      CALL AGSRESET                                                       38
      AG_MOTHER = 'CAVE'                                                  38
      AG_X = SHLQ_XOFFSET                                                 38
      AG_Y = 0                                                            38
      AG_Z = ZQUAD                                                        38
      AG_ALPHAN=AG_ALPHAN+1                                               38
      AG_ALPHAK(AG_ALPHAN)=2                                              38
      AG_ALPHAL(AG_ALPHAN)=SHLQ_ANGLE                                     38
      AG_TITLE = 'MGMT'                                                   38
      AG_EXNAME = 'POSITION'                                              38
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                                  38
      CALL AXPOSITION                                                     38
C *                                                                       38
C Geant POSITION : MGMT ===> MOTHER='CAVE' X=-SHLQ_XOFFSET Y=0 Z=ZQUAD A  38
C LPHAY=-SHLQ_ANGLE <===                                                  38
      CALL AGSRESET                                                       39
      AG_MOTHER = 'CAVE'                                                  39
      AG_X = -SHLQ_XOFFSET                                                39
      AG_Y = 0                                                            39
      AG_Z = ZQUAD                                                        39
      AG_ALPHAN=AG_ALPHAN+1                                               39
      AG_ALPHAK(AG_ALPHAN)=2                                              39
      AG_ALPHAL(AG_ALPHAN)=-SHLQ_ANGLE                                    39
      AG_TITLE = 'MGMT'                                                   39
      AG_EXNAME = 'POSITION'                                              39
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                                  39
      CALL AXPOSITION                                                     39
C *                                                                       39
C Geant POSITION : MGMT ===> MOTHER='CAVE' X=SHLQ_XOFFSET Y=0 Z=-ZQUAD A  39
C LPHAY=180-SHLQ_ANGLE <===                                               39
      CALL AGSRESET                                                       40
      AG_MOTHER = 'CAVE'                                                  40
      AG_X = SHLQ_XOFFSET                                                 40
      AG_Y = 0                                                            40
      AG_Z = -ZQUAD                                                       40
      AG_ALPHAN=AG_ALPHAN+1                                               40
      AG_ALPHAK(AG_ALPHAN)=2                                              40
      AG_ALPHAL(AG_ALPHAN)=180-SHLQ_ANGLE                                 40
      AG_TITLE = 'MGMT'                                                   40
      AG_EXNAME = 'POSITION'                                              40
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                                  40
      CALL AXPOSITION                                                     40
C *                                                                       40
C Geant POSITION : MGMT ===> MOTHER='CAVE' X=-SHLQ_XOFFSET Y=0 Z=-ZQUAD   40
C ALPHAY=180+SHLQ_ANGLE <===                                              40
      CALL AGSRESET                                                       41
      AG_MOTHER = 'CAVE'                                                  41
      AG_X = -SHLQ_XOFFSET                                                41
      AG_Y = 0                                                            41
      AG_Z = -ZQUAD                                                       41
      AG_ALPHAN=AG_ALPHAN+1                                               41
      AG_ALPHAK(AG_ALPHAN)=2                                              41
      AG_ALPHAL(AG_ALPHAN)=180+SHLQ_ANGLE                                 41
      AG_TITLE = 'MGMT'                                                   41
      AG_EXNAME = 'POSITION'                                              41
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                                  41
      CALL AXPOSITION                                                     41
C *                                                                       41
C ----------------------------------------------------------------------  41
C ------------ MGMT                                                       41
      CALL AGSENDM                                                        43
      RETURN                                                              43
C Begin Local Procedure MGMT                                              43
C MGMT IS THE MAGNET MOTHER                                               43
5010  CONTINUE                                                            43
         AG_TITLE='MGMT'                                                  43
         AG_MODULE=MODULE                                                 43
         AG_MARK='V00'                                                    43
         CALL AGSPUSH                                                     43
         AG_MODULE=MODULE                                                 43
C    *                                                                    43
C    Geant MATERIAL : AIR ===> inherited arguments used <===              44
         CALL AGSRESET                                                    44
         AG_TITLE = 'AIR'                                                 44
         AG_EXNAME = 'MATERIAL'                                           44
         AG_PARLIST = 'NONE'                                              44
         CALL AXMATERIAL                                                  44
C    *                                                                    44
C    Geant ATTRIBUTE : MGMT ===> SEEN=1 COLO=3 <===                       44
         CALL AGSRESET                                                    45
         AG_SEEN = 1                                                      45
         AG_COLO = 3                                                      45
         AG_TITLE = 'MGMT'                                                45
         AG_EXNAME = 'ATTRIBUTE'                                          45
         AG_PARLIST = 'SEEN_COLO'                                         45
         CALL AXATTRIBUTE                                                 45
C    *                                                                    45
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=SHLQ_MOTHERR DZ=SHLQ_MOTHERL/  45
C    2.0 <===                                                             45
         CALL AGSRESET                                                    46
         AG_RMIN = 0.0                                                    46
         AG_RMAX = SHLQ_MOTHERR                                           46
         AG_DZ = SHLQ_MOTHERL/2.0                                         46
         AG_TITLE = 'TUBE'                                                46
         AG_EXNAME = 'SHAPE'                                              46
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      46
         CALL AXSHAPE                                                     46
C    *                                                                    46
C    Create DZER :                                                        47
C    Call To Local Procedure DZER                                         47
         ASSIGN 5040 TO BLOCK_DZER                                        47
         GO TO  5030                                                      47
5040     CONTINUE                                                         47
C    *                                                                    47
C    Geant POSITION : DZER ===> MOTHER='MGMT' Z=-SHLQ_MOTHERL/2.0+SHLQ_D  48
C    ZEROL/2.0 <===                                                       48
         CALL AGSRESET                                                    48
         AG_MOTHER = 'MGMT'                                               48
         AG_Z = -SHLQ_MOTHERL/2.0+SHLQ_DZEROL/2.0                         48
         AG_TITLE = 'DZER'                                                48
         AG_EXNAME = 'POSITION'                                           48
         AG_PARLIST = 'MOTHER_Z'                                          48
         CALL AXPOSITION                                                  48
C    *                                                                    48
C    Create QONE :                                                        49
C    Call To Local Procedure QONE                                         49
         ASSIGN 5060 TO BLOCK_QONE                                        49
         GO TO  5050                                                      49
5060     CONTINUE                                                         49
C    *                                                                    49
C    Geant POSITION : QONE ===> MOTHER='MGMT' Z=-SHLQ_MOTHERL/2.0+(SHLQ_  50
C    Q1+SHLQ_DZEROL)+SHLQ_DZ1/2.0 <===                                    50
         CALL AGSRESET                                                    50
         AG_MOTHER = 'MGMT'                                               50
         AG_Z = -SHLQ_MOTHERL/2.0+(SHLQ_Q1+SHLQ_DZEROL)+SHLQ_DZ1/2.0      50
         AG_TITLE = 'QONE'                                                50
         AG_EXNAME = 'POSITION'                                           50
         AG_PARLIST = 'MOTHER_Z'                                          50
         CALL AXPOSITION                                                  50
C    *                                                                    50
C    Create QTWO :                                                        51
C    Call To Local Procedure QTWO                                         51
         ASSIGN 5080 TO BLOCK_QTWO                                        51
         GO TO  5070                                                      51
5080     CONTINUE                                                         51
C    *                                                                    51
C    Geant POSITION : QTWO ===> MOTHER='MGMT' Z=-SHLQ_MOTHERL/2.0+(SHLQ_  52
C    Q2+SHLQ_DZEROL)+SHLQ_DZ2/2.0 <===                                    52
         CALL AGSRESET                                                    52
         AG_MOTHER = 'MGMT'                                               52
         AG_Z = -SHLQ_MOTHERL/2.0+(SHLQ_Q2+SHLQ_DZEROL)+SHLQ_DZ2/2.0      52
         AG_TITLE = 'QTWO'                                                52
         AG_EXNAME = 'POSITION'                                           52
         AG_PARLIST = 'MOTHER_Z'                                          52
         CALL AXPOSITION                                                  52
C    *                                                                    52
C    Create QTHR :                                                        53
C    Call To Local Procedure QTHR                                         53
         ASSIGN 5100 TO BLOCK_QTHR                                        53
         GO TO  5090                                                      53
5100     CONTINUE                                                         53
C    *                                                                    53
C    Geant POSITION : QTHR ===> MOTHER='MGMT' Z=-SHLQ_MOTHERL/2.0+(SHLQ_  54
C    Q3+SHLQ_DZEROL)+SHLQ_DZ3/2.0 <===                                    54
         CALL AGSRESET                                                    54
         AG_MOTHER = 'MGMT'                                               54
         AG_Z = -SHLQ_MOTHERL/2.0+(SHLQ_Q3+SHLQ_DZEROL)+SHLQ_DZ3/2.0      54
         AG_TITLE = 'QTHR'                                                54
         AG_EXNAME = 'POSITION'                                           54
         AG_PARLIST = 'MOTHER_Z'                                          54
         CALL AXPOSITION                                                  54
C    *                                                                    54
5011  CALL AGSPOPB                                                        55
      GO TO BLOCK_MGMT, (5010 ,5020)                                      55
C End of Local Procedure                                                  55
C ----------------------------------------------------------------------  56
C ------------ DZER                                                       56
C Begin Local Procedure DZER                                              57
C DZER IS THE D0 YOKE                                                     57
5030  CONTINUE                                                            57
         AG_TITLE='DZER'                                                  57
         AG_MODULE=MODULE                                                 57
         AG_MARK='V00'                                                    57
         CALL AGSPUSH                                                     57
         AG_MODULE=MODULE                                                 57
C    *                                                                    57
C    Geant MATERIAL : IRON ===> inherited arguments used <===             58
         CALL AGSRESET                                                    58
         AG_TITLE = 'IRON'                                                58
         AG_EXNAME = 'MATERIAL'                                           58
         AG_PARLIST = 'NONE'                                              58
         CALL AXMATERIAL                                                  58
C    *                                                                    58
C    Geant ATTRIBUTE : DZER ===> SEEN=1 COLO=1 <===                       58
         CALL AGSRESET                                                    59
         AG_SEEN = 1                                                      59
         AG_COLO = 1                                                      59
         AG_TITLE = 'DZER'                                                59
         AG_EXNAME = 'ATTRIBUTE'                                          59
         AG_PARLIST = 'SEEN_COLO'                                         59
         CALL AXATTRIBUTE                                                 59
C    *                                                                    59
C    Geant SHAPE : TUBE ===> RMIN=SHLQ_DZERORI RMAX=SHLQ_DZERORO, DZ=SHL  59
C    Q_DZEROL/2.0 <===                                                    59
         CALL AGSRESET                                                    61
         AG_RMIN = SHLQ_DZERORI                                           61
         AG_RMAX = SHLQ_DZERORO                                           61
         AG_DZ = SHLQ_DZEROL/2.0                                          61
         AG_TITLE = 'TUBE'                                                61
         AG_EXNAME = 'SHAPE'                                              61
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      61
         CALL AXSHAPE                                                     61
C    *                                                                    61
5031  CALL AGSPOPB                                                        62
      GO TO BLOCK_DZER, (5030 ,5040)                                      62
C End of Local Procedure                                                  62
C ----------------------------------------------------------------------  63
C ------------ QONE                                                       63
C Begin Local Procedure QONE                                              64
C QONE IS THE Q1 YOKE                                                     64
5050  CONTINUE                                                            64
         AG_TITLE='QONE'                                                  64
         AG_MODULE=MODULE                                                 64
         AG_MARK='V00'                                                    64
         CALL AGSPUSH                                                     64
         AG_MODULE=MODULE                                                 64
C    *                                                                    64
C    Geant MATERIAL : IRON ===> inherited arguments used <===             65
         CALL AGSRESET                                                    65
         AG_TITLE = 'IRON'                                                65
         AG_EXNAME = 'MATERIAL'                                           65
         AG_PARLIST = 'NONE'                                              65
         CALL AXMATERIAL                                                  65
C    *                                                                    65
C    Geant ATTRIBUTE : QONE ===> SEEN=1 COLO=1 <===                       65
         CALL AGSRESET                                                    66
         AG_SEEN = 1                                                      66
         AG_COLO = 1                                                      66
         AG_TITLE = 'QONE'                                                66
         AG_EXNAME = 'ATTRIBUTE'                                          66
         AG_PARLIST = 'SEEN_COLO'                                         66
         CALL AXATTRIBUTE                                                 66
C    *                                                                    66
C    Geant SHAPE : TUBE ===> RMIN=SHLQ_RI1 RMAX=SHLQ_RO1 DZ=SHLQ_DZ1/2.0  66
C     <===                                                                66
         CALL AGSRESET                                                    67
         AG_RMIN = SHLQ_RI1                                               67
         AG_RMAX = SHLQ_RO1                                               67
         AG_DZ = SHLQ_DZ1/2.0                                             67
         AG_TITLE = 'TUBE'                                                67
         AG_EXNAME = 'SHAPE'                                              67
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      67
         CALL AXSHAPE                                                     67
C    *                                                                    67
5051  CALL AGSPOPB                                                        68
      GO TO BLOCK_QONE, (5050 ,5060)                                      68
C End of Local Procedure                                                  68
C ----------------------------------------------------------------------  69
C ------------ QTWO                                                       69
C Begin Local Procedure QTWO                                              70
C QTWO IS THE Q2 YOKE                                                     70
5070  CONTINUE                                                            70
         AG_TITLE='QTWO'                                                  70
         AG_MODULE=MODULE                                                 70
         AG_MARK='V00'                                                    70
         CALL AGSPUSH                                                     70
         AG_MODULE=MODULE                                                 70
C    *                                                                    70
C    Geant MATERIAL : IRON ===> inherited arguments used <===             71
         CALL AGSRESET                                                    71
         AG_TITLE = 'IRON'                                                71
         AG_EXNAME = 'MATERIAL'                                           71
         AG_PARLIST = 'NONE'                                              71
         CALL AXMATERIAL                                                  71
C    *                                                                    71
C    Geant ATTRIBUTE : QTWO ===> SEEN=1 COLO=1 <===                       71
         CALL AGSRESET                                                    72
         AG_SEEN = 1                                                      72
         AG_COLO = 1                                                      72
         AG_TITLE = 'QTWO'                                                72
         AG_EXNAME = 'ATTRIBUTE'                                          72
         AG_PARLIST = 'SEEN_COLO'                                         72
         CALL AXATTRIBUTE                                                 72
C    *                                                                    72
C    Geant SHAPE : TUBE ===> RMIN=SHLQ_RI2 RMAX=SHLQ_RO2 DZ=SHLQ_DZ2/2.0  72
C     <===                                                                72
         CALL AGSRESET                                                    73
         AG_RMIN = SHLQ_RI2                                               73
         AG_RMAX = SHLQ_RO2                                               73
         AG_DZ = SHLQ_DZ2/2.0                                             73
         AG_TITLE = 'TUBE'                                                73
         AG_EXNAME = 'SHAPE'                                              73
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      73
         CALL AXSHAPE                                                     73
C    *                                                                    73
5071  CALL AGSPOPB                                                        74
      GO TO BLOCK_QTWO, (5070 ,5080)                                      74
C End of Local Procedure                                                  74
C ----------------------------------------------------------------------  75
C ------------ QTHR                                                       75
C Begin Local Procedure QTHR                                              76
C QTHR IS THE Q3 YOKE                                                     76
5090  CONTINUE                                                            76
         AG_TITLE='QTHR'                                                  76
         AG_MODULE=MODULE                                                 76
         AG_MARK='V00'                                                    76
         CALL AGSPUSH                                                     76
         AG_MODULE=MODULE                                                 76
C    *                                                                    76
C    Geant MATERIAL : IRON ===> inherited arguments used <===             77
         CALL AGSRESET                                                    77
         AG_TITLE = 'IRON'                                                77
         AG_EXNAME = 'MATERIAL'                                           77
         AG_PARLIST = 'NONE'                                              77
         CALL AXMATERIAL                                                  77
C    *                                                                    77
C    Geant ATTRIBUTE : QTHR ===> SEEN=1 COLO=1 <===                       77
         CALL AGSRESET                                                    78
         AG_SEEN = 1                                                      78
         AG_COLO = 1                                                      78
         AG_TITLE = 'QTHR'                                                78
         AG_EXNAME = 'ATTRIBUTE'                                          78
         AG_PARLIST = 'SEEN_COLO'                                         78
         CALL AXATTRIBUTE                                                 78
C    *                                                                    78
C    Geant SHAPE : TUBE ===> RMIN=SHLQ_RI3 RMAX=SHLQ_RO3 DZ=SHLQ_DZ3/2.0  78
C     <===                                                                78
         CALL AGSRESET                                                    79
         AG_RMIN = SHLQ_RI3                                               79
         AG_RMAX = SHLQ_RO3                                               79
         AG_DZ = SHLQ_DZ3/2.0                                             79
         AG_TITLE = 'TUBE'                                                79
         AG_EXNAME = 'SHAPE'                                              79
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      79
         CALL AXSHAPE                                                     79
C    *                                                                    79
5091  CALL AGSPOPB                                                        80
      GO TO BLOCK_QTHR, (5090 ,5100)                                      80
C End of Local Procedure                                                  80
      WRITE(LOUT,5110)'===>>  [End AgML Module QUADGEO]  <<==='           80
5110  FORMAT(A32,/,/)                                                     80
C MODULE QUADGEO                                                          82
      END                                                                 82
