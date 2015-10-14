C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE ISTDGEO0                                                 1
C ISTDGEO0 IS THE THE STAR IST DETECTOR TEST                              1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'ISTDGEO0'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE THE STAR IST DETECTOR TEST'/                  1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 12/06/14                                                        1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'12/06/14'/                                   2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR JONATHAN BOUCHET                                                 2
      CHARACTER*40 AUTHOR /'JONATHAN BOUCHET'/                            3
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
      REAL *4 AANGLE,BANGLE, RADDEG, INIX, INIY, MODULE_THK, LADDER_THK,  5
     * LADDER_WIDTH, LADDER_LENGTH, XPOS, YPOS, ZPOS, MODULERADIUS,       5
     *HALF                                                                5
      INTEGER *4 NL, LY, NU, CABLE, ISTLAYER, DEBUG                       9
      INTEGER BLOCK_IBMO                                                  15
      INTEGER BLOCK_IBAM                                                  15
      INTEGER BLOCK_IBLM                                                  15
      INTEGER BLOCK_IBSS                                                  15
      INTEGER BLOCK_IBSP                                                  15
C -                                                                       16
      INTEGER ISMG_VERSION                                                16
      EQUIVALENCE (BANK_ISMG(3),ISMG_VERSION,RE_ISMG_VERSION),            16
     *(NAMES_ISMG(1),NISMG_VERSION)                                       16
      EQUIVALENCE (COMMEN_ISMG(1),CISMG_VERSION),(MAP_ISMG(1,1),          16
     *MISMG_VERSION)                                                      16
      CHARACTER CISMG_VERSION*40/' '/                                     16
      CHARACTER NISMG_VERSION*12/'I.VERSION'/                             16
      INTEGER MISMG_VERSION(2)/1,1/                                       16
      REAL RE_ISMG_VERSION                                                16
C -                                                                       16
      INTEGER ISMG_SUBVERSION                                             16
      EQUIVALENCE (BANK_ISMG(4),ISMG_SUBVERSION,RE_ISMG_SUBVERSION),      16
     *(NAMES_ISMG(2),NISMG_SUBVERSION)                                    16
      EQUIVALENCE (COMMEN_ISMG(2),CISMG_SUBVERSION),(MAP_ISMG(1,2),       16
     *MISMG_SUBVERSION)                                                   16
      CHARACTER CISMG_SUBVERSION*40/' '/                                  16
      CHARACTER NISMG_SUBVERSION*12/'I.SUBVERSION'/                       16
      INTEGER MISMG_SUBVERSION(2)/1,1/                                    16
      REAL RE_ISMG_SUBVERSION                                             16
C -                                                                       16
      REAL ISMG_RIN                                                       16
      EQUIVALENCE (BANK_ISMG(5),ISMG_RIN,RE_ISMG_RIN),(NAMES_ISMG(3),     16
     *NISMG_RIN)                                                          16
      EQUIVALENCE (COMMEN_ISMG(3),CISMG_RIN),(MAP_ISMG(1,3),MISMG_RIN)    16
      CHARACTER CISMG_RIN*40/' '/                                         16
      CHARACTER NISMG_RIN*12/'R.RIN'/                                     16
      INTEGER MISMG_RIN(2)/1,1/                                           16
      REAL RE_ISMG_RIN                                                    16
C -                                                                       16
      REAL ISMG_ROUT                                                      16
      EQUIVALENCE (BANK_ISMG(6),ISMG_ROUT,RE_ISMG_ROUT),(NAMES_ISMG(4),   16
     *NISMG_ROUT)                                                         16
      EQUIVALENCE (COMMEN_ISMG(4),CISMG_ROUT),(MAP_ISMG(1,4),             16
     *MISMG_ROUT)                                                         16
      CHARACTER CISMG_ROUT*40/' '/                                        16
      CHARACTER NISMG_ROUT*12/'R.ROUT'/                                   16
      INTEGER MISMG_ROUT(2)/1,1/                                          16
      REAL RE_ISMG_ROUT                                                   16
C -                                                                       16
      INTEGER LEN_ISMG,LLEN_ISMG,LINK_ISMG                                16
      PARAMETER (LEN_ISMG=6)                                              16
      PARAMETER (LLEN_ISMG=4)                                             16
      INTEGER FLAG_ISMG/0/,MAP_ISMG(2,LLEN_ISMG),IDNUM_ISMG(10)           16
      CHARACTER NAMES_ISMG(LLEN_ISMG)*12,COMMEN_ISMG(LLEN_ISMG)*40        16
      DATA IDNUM_ISMG/10*0/                                               16
      REAL BANK_ISMG(LEN_ISMG)                                            16
      CHARACTER*60 BPATH_ISMG/' '/                                        16
      SAVE FLAG_ISMG,MAP_ISMG,IDNUM_ISMG,NAMES_ISMG,COMMEN_ISMG,          16
     *BPATH_ISMG                                                          16
      DATA LINK_ISMG/0/,BANK_ISMG/LEN_ISMG*0/                             16
C *                                                                       16
C -                                                                       17
      INTEGER ISBG_LAYER                                                  17
      EQUIVALENCE (BANK_ISBG(3),ISBG_LAYER,RE_ISBG_LAYER),                17
     *(NAMES_ISBG(1),NISBG_LAYER)                                         17
      EQUIVALENCE (COMMEN_ISBG(1),CISBG_LAYER),(MAP_ISBG(1,1),            17
     *MISBG_LAYER)                                                        17
      CHARACTER CISBG_LAYER*40/' '/                                       17
      CHARACTER NISBG_LAYER*12/'I.LAYER'/                                 17
      INTEGER MISBG_LAYER(2)/1,1/                                         17
      REAL RE_ISBG_LAYER                                                  17
C -                                                                       17
      INTEGER ISBG_NLADDER                                                17
      EQUIVALENCE (BANK_ISBG(4),ISBG_NLADDER,RE_ISBG_NLADDER),            17
     *(NAMES_ISBG(2),NISBG_NLADDER)                                       17
      EQUIVALENCE (COMMEN_ISBG(2),CISBG_NLADDER),(MAP_ISBG(1,2),          17
     *MISBG_NLADDER)                                                      17
      CHARACTER CISBG_NLADDER*40/' '/                                     17
      CHARACTER NISBG_NLADDER*12/'I.NLADDER'/                             17
      INTEGER MISBG_NLADDER(2)/1,1/                                       17
      REAL RE_ISBG_NLADDER                                                17
C -                                                                       17
      INTEGER ISBG_NUNIT                                                  17
      EQUIVALENCE (BANK_ISBG(5),ISBG_NUNIT,RE_ISBG_NUNIT),                17
     *(NAMES_ISBG(3),NISBG_NUNIT)                                         17
      EQUIVALENCE (COMMEN_ISBG(3),CISBG_NUNIT),(MAP_ISBG(1,3),            17
     *MISBG_NUNIT)                                                        17
      CHARACTER CISBG_NUNIT*40/' '/                                       17
      CHARACTER NISBG_NUNIT*12/'I.NUNIT'/                                 17
      INTEGER MISBG_NUNIT(2)/1,1/                                         17
      REAL RE_ISBG_NUNIT                                                  17
C -                                                                       17
      REAL ISBG_RADIUS                                                    17
      EQUIVALENCE (BANK_ISBG(6),ISBG_RADIUS,RE_ISBG_RADIUS),              17
     *(NAMES_ISBG(4),NISBG_RADIUS)                                        17
      EQUIVALENCE (COMMEN_ISBG(4),CISBG_RADIUS),(MAP_ISBG(1,4),           17
     *MISBG_RADIUS)                                                       17
      CHARACTER CISBG_RADIUS*40/' '/                                      17
      CHARACTER NISBG_RADIUS*12/'R.RADIUS'/                               17
      INTEGER MISBG_RADIUS(2)/1,1/                                        17
      REAL RE_ISBG_RADIUS                                                 17
C -                                                                       17
      REAL ISBG_TILT                                                      17
      EQUIVALENCE (BANK_ISBG(7),ISBG_TILT,RE_ISBG_TILT),(NAMES_ISBG(5),   17
     *NISBG_TILT)                                                         17
      EQUIVALENCE (COMMEN_ISBG(5),CISBG_TILT),(MAP_ISBG(1,5),             17
     *MISBG_TILT)                                                         17
      CHARACTER CISBG_TILT*40/' '/                                        17
      CHARACTER NISBG_TILT*12/'R.TILT'/                                   17
      INTEGER MISBG_TILT(2)/1,1/                                          17
      REAL RE_ISBG_TILT                                                   17
C -                                                                       17
      REAL ISBG_GAP                                                       17
      EQUIVALENCE (BANK_ISBG(8),ISBG_GAP,RE_ISBG_GAP),(NAMES_ISBG(6),     17
     *NISBG_GAP)                                                          17
      EQUIVALENCE (COMMEN_ISBG(6),CISBG_GAP),(MAP_ISBG(1,6),MISBG_GAP)    17
      CHARACTER CISBG_GAP*40/' '/                                         17
      CHARACTER NISBG_GAP*12/'R.GAP'/                                     17
      INTEGER MISBG_GAP(2)/1,1/                                           17
      REAL RE_ISBG_GAP                                                    17
C -                                                                       17
      INTEGER LEN_ISBG,LLEN_ISBG,LINK_ISBG                                17
      PARAMETER (LEN_ISBG=8)                                              17
      PARAMETER (LLEN_ISBG=6)                                             17
      INTEGER FLAG_ISBG/0/,MAP_ISBG(2,LLEN_ISBG),IDNUM_ISBG(10)           17
      CHARACTER NAMES_ISBG(LLEN_ISBG)*12,COMMEN_ISBG(LLEN_ISBG)*40        17
      DATA IDNUM_ISBG/10*0/                                               17
      REAL BANK_ISBG(LEN_ISBG)                                            17
      CHARACTER*60 BPATH_ISBG/' '/                                        17
      SAVE FLAG_ISBG,MAP_ISBG,IDNUM_ISBG,NAMES_ISBG,COMMEN_ISBG,          17
     *BPATH_ISBG                                                          17
      DATA LINK_ISBG/0/,BANK_ISBG/LEN_ISBG*0/                             17
C *                                                                       17
C -                                                                       18
      INTEGER ISAN_VERSION                                                18
      EQUIVALENCE (BANK_ISAN(3),ISAN_VERSION,RE_ISAN_VERSION),            18
     *(NAMES_ISAN(1),NISAN_VERSION)                                       18
      EQUIVALENCE (COMMEN_ISAN(1),CISAN_VERSION),(MAP_ISAN(1,1),          18
     *MISAN_VERSION)                                                      18
      CHARACTER CISAN_VERSION*40/' '/                                     18
      CHARACTER NISAN_VERSION*12/'I.VERSION'/                             18
      INTEGER MISAN_VERSION(2)/1,1/                                       18
      REAL RE_ISAN_VERSION                                                18
C -                                                                       18
      REAL ISAN_RMIN                                                      18
      EQUIVALENCE (BANK_ISAN(4),ISAN_RMIN,RE_ISAN_RMIN),(NAMES_ISAN(2),   18
     *NISAN_RMIN)                                                         18
      EQUIVALENCE (COMMEN_ISAN(2),CISAN_RMIN),(MAP_ISAN(1,2),             18
     *MISAN_RMIN)                                                         18
      CHARACTER CISAN_RMIN*40/' '/                                        18
      CHARACTER NISAN_RMIN*12/'R.RMIN'/                                   18
      INTEGER MISAN_RMIN(2)/1,1/                                          18
      REAL RE_ISAN_RMIN                                                   18
C -                                                                       18
      REAL ISAN_THK                                                       18
      EQUIVALENCE (BANK_ISAN(5),ISAN_THK,RE_ISAN_THK),(NAMES_ISAN(3),     18
     *NISAN_THK)                                                          18
      EQUIVALENCE (COMMEN_ISAN(3),CISAN_THK),(MAP_ISAN(1,3),MISAN_THK)    18
      CHARACTER CISAN_THK*40/' '/                                         18
      CHARACTER NISAN_THK*12/'R.THK'/                                     18
      INTEGER MISAN_THK(2)/1,1/                                           18
      REAL RE_ISAN_THK                                                    18
C -                                                                       18
      INTEGER LEN_ISAN,LLEN_ISAN,LINK_ISAN                                18
      PARAMETER (LEN_ISAN=5)                                              18
      PARAMETER (LLEN_ISAN=3)                                             18
      INTEGER FLAG_ISAN/0/,MAP_ISAN(2,LLEN_ISAN),IDNUM_ISAN(10)           18
      CHARACTER NAMES_ISAN(LLEN_ISAN)*12,COMMEN_ISAN(LLEN_ISAN)*40        18
      DATA IDNUM_ISAN/10*0/                                               18
      REAL BANK_ISAN(LEN_ISAN)                                            18
      CHARACTER*60 BPATH_ISAN/' '/                                        18
      SAVE FLAG_ISAN,MAP_ISAN,IDNUM_ISAN,NAMES_ISAN,COMMEN_ISAN,          18
     *BPATH_ISAN                                                          18
      DATA LINK_ISAN/0/,BANK_ISAN/LEN_ISAN*0/                             18
C *                                                                       18
C -                                                                       19
      INTEGER ISSS_VERSION                                                19
      EQUIVALENCE (BANK_ISSS(3),ISSS_VERSION,RE_ISSS_VERSION),            19
     *(NAMES_ISSS(1),NISSS_VERSION)                                       19
      EQUIVALENCE (COMMEN_ISSS(1),CISSS_VERSION),(MAP_ISSS(1,1),          19
     *MISSS_VERSION)                                                      19
      CHARACTER CISSS_VERSION*40/' '/                                     19
      CHARACTER NISSS_VERSION*12/'I.VERSION'/                             19
      INTEGER MISSS_VERSION(2)/1,1/                                       19
      REAL RE_ISSS_VERSION                                                19
C -                                                                       19
      REAL ISSS_ACTIVEW                                                   19
      EQUIVALENCE (BANK_ISSS(4),ISSS_ACTIVEW,RE_ISSS_ACTIVEW),            19
     *(NAMES_ISSS(2),NISSS_ACTIVEW)                                       19
      EQUIVALENCE (COMMEN_ISSS(2),CISSS_ACTIVEW),(MAP_ISSS(1,2),          19
     *MISSS_ACTIVEW)                                                      19
      CHARACTER CISSS_ACTIVEW*40/' '/                                     19
      CHARACTER NISSS_ACTIVEW*12/'R.ACTIVEW'/                             19
      INTEGER MISSS_ACTIVEW(2)/1,1/                                       19
      REAL RE_ISSS_ACTIVEW                                                19
C -                                                                       19
      REAL ISSS_ACTIVEH                                                   19
      EQUIVALENCE (BANK_ISSS(5),ISSS_ACTIVEH,RE_ISSS_ACTIVEH),            19
     *(NAMES_ISSS(3),NISSS_ACTIVEH)                                       19
      EQUIVALENCE (COMMEN_ISSS(3),CISSS_ACTIVEH),(MAP_ISSS(1,3),          19
     *MISSS_ACTIVEH)                                                      19
      CHARACTER CISSS_ACTIVEH*40/' '/                                     19
      CHARACTER NISSS_ACTIVEH*12/'R.ACTIVEH'/                             19
      INTEGER MISSS_ACTIVEH(2)/1,1/                                       19
      REAL RE_ISSS_ACTIVEH                                                19
C -                                                                       19
      REAL ISSS_ACTIVETHK                                                 19
      EQUIVALENCE (BANK_ISSS(6),ISSS_ACTIVETHK,RE_ISSS_ACTIVETHK),        19
     *(NAMES_ISSS(4),NISSS_ACTIVETHK)                                     19
      EQUIVALENCE (COMMEN_ISSS(4),CISSS_ACTIVETHK),(MAP_ISSS(1,4),        19
     *MISSS_ACTIVETHK)                                                    19
      CHARACTER CISSS_ACTIVETHK*40/' '/                                   19
      CHARACTER NISSS_ACTIVETHK*12/'R.ACTIVETHK'/                         19
      INTEGER MISSS_ACTIVETHK(2)/1,1/                                     19
      REAL RE_ISSS_ACTIVETHK                                              19
C -                                                                       19
      REAL ISSS_PASSIVETHK                                                19
      EQUIVALENCE (BANK_ISSS(7),ISSS_PASSIVETHK,RE_ISSS_PASSIVETHK),      19
     *(NAMES_ISSS(5),NISSS_PASSIVETHK)                                    19
      EQUIVALENCE (COMMEN_ISSS(5),CISSS_PASSIVETHK),(MAP_ISSS(1,5),       19
     *MISSS_PASSIVETHK)                                                   19
      CHARACTER CISSS_PASSIVETHK*40/' '/                                  19
      CHARACTER NISSS_PASSIVETHK*12/'R.PASSIVETHK'/                       19
      INTEGER MISSS_PASSIVETHK(2)/1,1/                                    19
      REAL RE_ISSS_PASSIVETHK                                             19
C -                                                                       19
      INTEGER LEN_ISSS,LLEN_ISSS,LINK_ISSS                                19
      PARAMETER (LEN_ISSS=7)                                              19
      PARAMETER (LLEN_ISSS=5)                                             19
      INTEGER FLAG_ISSS/0/,MAP_ISSS(2,LLEN_ISSS),IDNUM_ISSS(10)           19
      CHARACTER NAMES_ISSS(LLEN_ISSS)*12,COMMEN_ISSS(LLEN_ISSS)*40        19
      DATA IDNUM_ISSS/10*0/                                               19
      REAL BANK_ISSS(LEN_ISSS)                                            19
      CHARACTER*60 BPATH_ISSS/' '/                                        19
      SAVE FLAG_ISSS,MAP_ISSS,IDNUM_ISSS,NAMES_ISSS,COMMEN_ISSS,          19
     *BPATH_ISSS                                                          19
      DATA LINK_ISSS/0/,BANK_ISSS/LEN_ISSS*0/                             19
C *                                                                       19
C -                                                                       20
      INTEGER PARO_VERSION                                                20
      EQUIVALENCE (BANK_PARO(3),PARO_VERSION,RE_PARO_VERSION),            20
     *(NAMES_PARO(1),NPARO_VERSION)                                       20
      EQUIVALENCE (COMMEN_PARO(1),CPARO_VERSION),(MAP_PARO(1,1),          20
     *MPARO_VERSION)                                                      20
      CHARACTER CPARO_VERSION*40/' '/                                     20
      CHARACTER NPARO_VERSION*12/'I.VERSION'/                             20
      INTEGER MPARO_VERSION(2)/1,1/                                       20
      REAL RE_PARO_VERSION                                                20
C -                                                                       20
      REAL PARO_DX                                                        20
      EQUIVALENCE (BANK_PARO(4),PARO_DX,RE_PARO_DX),(NAMES_PARO(2),       20
     *NPARO_DX)                                                           20
      EQUIVALENCE (COMMEN_PARO(2),CPARO_DX),(MAP_PARO(1,2),MPARO_DX)      20
      CHARACTER CPARO_DX*40/' '/                                          20
      CHARACTER NPARO_DX*12/'R.DX'/                                       20
      INTEGER MPARO_DX(2)/1,1/                                            20
      REAL RE_PARO_DX                                                     20
C -                                                                       20
      REAL PARO_DY                                                        20
      EQUIVALENCE (BANK_PARO(5),PARO_DY,RE_PARO_DY),(NAMES_PARO(3),       20
     *NPARO_DY)                                                           20
      EQUIVALENCE (COMMEN_PARO(3),CPARO_DY),(MAP_PARO(1,3),MPARO_DY)      20
      CHARACTER CPARO_DY*40/' '/                                          20
      CHARACTER NPARO_DY*12/'R.DY'/                                       20
      INTEGER MPARO_DY(2)/1,1/                                            20
      REAL RE_PARO_DY                                                     20
C -                                                                       20
      REAL PARO_DZ                                                        20
      EQUIVALENCE (BANK_PARO(6),PARO_DZ,RE_PARO_DZ),(NAMES_PARO(4),       20
     *NPARO_DZ)                                                           20
      EQUIVALENCE (COMMEN_PARO(4),CPARO_DZ),(MAP_PARO(1,4),MPARO_DZ)      20
      CHARACTER CPARO_DZ*40/' '/                                          20
      CHARACTER NPARO_DZ*12/'R.DZ'/                                       20
      INTEGER MPARO_DZ(2)/1,1/                                            20
      REAL RE_PARO_DZ                                                     20
C -                                                                       20
      REAL PARO_MULT                                                      20
      EQUIVALENCE (BANK_PARO(7),PARO_MULT,RE_PARO_MULT),(NAMES_PARO(5),   20
     *NPARO_MULT)                                                         20
      EQUIVALENCE (COMMEN_PARO(5),CPARO_MULT),(MAP_PARO(1,5),             20
     *MPARO_MULT)                                                         20
      CHARACTER CPARO_MULT*40/' '/                                        20
      CHARACTER NPARO_MULT*12/'R.MULT'/                                   20
      INTEGER MPARO_MULT(2)/1,1/                                          20
      REAL RE_PARO_MULT                                                   20
C -                                                                       20
      INTEGER PARO_LAD(24)                                                20
      EQUIVALENCE (BANK_PARO(8),PARO_LAD,RE_PARO_LAD),(NAMES_PARO(6),     20
     *NPARO_LAD)                                                          20
      EQUIVALENCE (COMMEN_PARO(6),CPARO_LAD),(MAP_PARO(1,6),MPARO_LAD)    20
      CHARACTER CPARO_LAD*40/' '/                                         20
      CHARACTER NPARO_LAD*12/'I.LAD'/                                     20
      INTEGER MPARO_LAD(2)/24,1/                                          20
      REAL RE_PARO_LAD                                                    20
C -                                                                       20
      INTEGER LEN_PARO,LLEN_PARO,LINK_PARO                                20
      PARAMETER (LEN_PARO=31)                                             20
      PARAMETER (LLEN_PARO=6)                                             20
      INTEGER FLAG_PARO/0/,MAP_PARO(2,LLEN_PARO),IDNUM_PARO(10)           20
      CHARACTER NAMES_PARO(LLEN_PARO)*12,COMMEN_PARO(LLEN_PARO)*40        20
      DATA IDNUM_PARO/10*0/                                               20
      REAL BANK_PARO(LEN_PARO)                                            20
      CHARACTER*60 BPATH_PARO/' '/                                        20
      SAVE FLAG_PARO,MAP_PARO,IDNUM_PARO,NAMES_PARO,COMMEN_PARO,          20
     *BPATH_PARO                                                          20
      DATA LINK_PARO/0/,BANK_PARO/LEN_PARO*0/                             20
C *                                                                       20
C -                                                                       21
      INTEGER PARA_VERSION                                                21
      EQUIVALENCE (BANK_PARA(3),PARA_VERSION,RE_PARA_VERSION),            21
     *(NAMES_PARA(1),NPARA_VERSION)                                       21
      EQUIVALENCE (COMMEN_PARA(1),CPARA_VERSION),(MAP_PARA(1,1),          21
     *MPARA_VERSION)                                                      21
      CHARACTER CPARA_VERSION*40/' '/                                     21
      CHARACTER NPARA_VERSION*12/'I.VERSION'/                             21
      INTEGER MPARA_VERSION(2)/1,1/                                       21
      REAL RE_PARA_VERSION                                                21
C -                                                                       21
      REAL PARA_DA                                                        21
      EQUIVALENCE (BANK_PARA(4),PARA_DA,RE_PARA_DA),(NAMES_PARA(2),       21
     *NPARA_DA)                                                           21
      EQUIVALENCE (COMMEN_PARA(2),CPARA_DA),(MAP_PARA(1,2),MPARA_DA)      21
      CHARACTER CPARA_DA*40/' '/                                          21
      CHARACTER NPARA_DA*12/'R.DA'/                                       21
      INTEGER MPARA_DA(2)/1,1/                                            21
      REAL RE_PARA_DA                                                     21
C -                                                                       21
      REAL PARA_DB                                                        21
      EQUIVALENCE (BANK_PARA(5),PARA_DB,RE_PARA_DB),(NAMES_PARA(3),       21
     *NPARA_DB)                                                           21
      EQUIVALENCE (COMMEN_PARA(3),CPARA_DB),(MAP_PARA(1,3),MPARA_DB)      21
      CHARACTER CPARA_DB*40/' '/                                          21
      CHARACTER NPARA_DB*12/'R.DB'/                                       21
      INTEGER MPARA_DB(2)/1,1/                                            21
      REAL RE_PARA_DB                                                     21
C -                                                                       21
      REAL PARA_DG                                                        21
      EQUIVALENCE (BANK_PARA(6),PARA_DG,RE_PARA_DG),(NAMES_PARA(4),       21
     *NPARA_DG)                                                           21
      EQUIVALENCE (COMMEN_PARA(4),CPARA_DG),(MAP_PARA(1,4),MPARA_DG)      21
      CHARACTER CPARA_DG*40/' '/                                          21
      CHARACTER NPARA_DG*12/'R.DG'/                                       21
      INTEGER MPARA_DG(2)/1,1/                                            21
      REAL RE_PARA_DG                                                     21
C -                                                                       21
      REAL PARA_MULT                                                      21
      EQUIVALENCE (BANK_PARA(7),PARA_MULT,RE_PARA_MULT),(NAMES_PARA(5),   21
     *NPARA_MULT)                                                         21
      EQUIVALENCE (COMMEN_PARA(5),CPARA_MULT),(MAP_PARA(1,5),             21
     *MPARA_MULT)                                                         21
      CHARACTER CPARA_MULT*40/' '/                                        21
      CHARACTER NPARA_MULT*12/'R.MULT'/                                   21
      INTEGER MPARA_MULT(2)/1,1/                                          21
      REAL RE_PARA_MULT                                                   21
C -                                                                       21
      INTEGER PARA_LAD(24)                                                21
      EQUIVALENCE (BANK_PARA(8),PARA_LAD,RE_PARA_LAD),(NAMES_PARA(6),     21
     *NPARA_LAD)                                                          21
      EQUIVALENCE (COMMEN_PARA(6),CPARA_LAD),(MAP_PARA(1,6),MPARA_LAD)    21
      CHARACTER CPARA_LAD*40/' '/                                         21
      CHARACTER NPARA_LAD*12/'I.LAD'/                                     21
      INTEGER MPARA_LAD(2)/24,1/                                          21
      REAL RE_PARA_LAD                                                    21
C -                                                                       21
      INTEGER LEN_PARA,LLEN_PARA,LINK_PARA                                21
      PARAMETER (LEN_PARA=31)                                             21
      PARAMETER (LLEN_PARA=6)                                             21
      INTEGER FLAG_PARA/0/,MAP_PARA(2,LLEN_PARA),IDNUM_PARA(10)           21
      CHARACTER NAMES_PARA(LLEN_PARA)*12,COMMEN_PARA(LLEN_PARA)*40        21
      DATA IDNUM_PARA/10*0/                                               21
      REAL BANK_PARA(LEN_PARA)                                            21
      CHARACTER*60 BPATH_PARA/' '/                                        21
      SAVE FLAG_PARA,MAP_PARA,IDNUM_PARA,NAMES_PARA,COMMEN_PARA,          21
     *BPATH_PARA                                                          21
      DATA LINK_PARA/0/,BANK_PARA/LEN_PARA*0/                             21
C *                                                                       21
C *                                                                       22
      CALL AGSBEGM(MODULE,IPRIN)                                          22
      IF (IPRIN.LT.0) RETURN                                              22
C Filling bank ISMG - MOTHER VOLUME WHOLE DETECTOR                        22
      BANK_TITLE='MOTHER VOLUME WHOLE DETECTOR'                           22
      BPATH_ISMG='ISMG*'                                                  22
      IDNUM_ISMG(1)=0                                                     23
C VERSION, THIS GOES TO G2T_VOLUME_ID.G !!                                23
      ISMG_VERSION=1.0                                                    23
      CISMG_VERSION='VERSION, THIS GOES TO G2T_VOLUME_ID.G !!'            23
C FOR FUTURE USE WITH G2T_VOLUME_ID.G                                     24
      ISMG_SUBVERSION=0                                                   24
      CISMG_SUBVERSION='FOR FUTURE USE WITH G2T_VOLUME_ID.G'              24
C INNER RADIUS                                                            25
      ISMG_RIN=13.50                                                      25
      CISMG_RIN='INNER RADIUS'                                            25
C OUTER RADIUS (LENGTH = LADDER LENGTH)                                   26
      ISMG_ROUT=15.0                                                      26
      CISMG_ROUT='OUTER RADIUS (LENGTH = LADDER LENGTH)'                  26
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISMG',    27
     *BPATH_ISMG,IDNUM_ISMG,1, MAP_ISMG,NAMES_ISMG,COMMEN_ISMG,           27
     *BANK_ISMG,LEN_ISMG,LLEN_ISMG,LINK_ISMG,FLAG_ISMG)                   27
C *                                                                       27
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISMG',    27
     *BPATH_ISMG,IDNUM_ISMG,1, MAP_ISMG,NAMES_ISMG,COMMEN_ISMG,           27
     *BANK_ISMG,LEN_ISMG,LLEN_ISMG,LINK_ISMG,FLAG_ISMG)                   27
C *                                                                       28
      CALL AGSBEGM(MODULE,IPRIN)                                          28
      IF (IPRIN.LT.0) RETURN                                              28
C Filling bank ISBG - INNER SILICON TRACKER PARAMETERS                    28
      BANK_TITLE='INNER SILICON TRACKER PARAMETERS'                       28
      BPATH_ISBG='ISBG*'                                                  28
      IDNUM_ISBG(1)=0                                                     29
C LAYER INDEX, JUST ONE LAYER LEFT                                        29
      ISBG_LAYER=1.0                                                      29
      CISBG_LAYER='LAYER INDEX, JUST ONE LAYER LEFT'                      29
C NUMBER OF LADDERS IN THIS LAYERS                                        30
      ISBG_NLADDER=24                                                     30
      CISBG_NLADDER='NUMBER OF LADDERS IN THIS LAYERS'                    30
C NUMBER OF SENSORS PER LADDER IN THIS LAYER                              31
      ISBG_NUNIT=12                                                       31
      CISBG_NUNIT='NUMBER OF SENSORS PER LADDER IN THIS LAYER'            31
C RADIUS CENTERS OF ACTIVE SILICON SENSORS                                32
      ISBG_RADIUS=14.0                                                    32
      CISBG_RADIUS='RADIUS CENTERS OF ACTIVE SILICON SENSORS'             32
C TILT ANGLE OF THE LADDERS                                               33
      ISBG_TILT=15.0                                                      33
      CISBG_TILT='TILT ANGLE OF THE LADDERS'                              33
C GAPS BTW THE SENSORS                                                    34
      ISBG_GAP=0.0                                                        34
      CISBG_GAP='GAPS BTW THE SENSORS'                                    34
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISBG',    35
     *BPATH_ISBG,IDNUM_ISBG,1, MAP_ISBG,NAMES_ISBG,COMMEN_ISBG,           35
     *BANK_ISBG,LEN_ISBG,LLEN_ISBG,LINK_ISBG,FLAG_ISBG)                   35
C *                                                                       35
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISBG',    35
     *BPATH_ISBG,IDNUM_ISBG,1, MAP_ISBG,NAMES_ISBG,COMMEN_ISBG,           35
     *BANK_ISBG,LEN_ISBG,LLEN_ISBG,LINK_ISBG,FLAG_ISBG)                   35
C *                                                                       36
      CALL AGSBEGM(MODULE,IPRIN)                                          36
      IF (IPRIN.LT.0) RETURN                                              36
C Filling bank ISAN - RADIATION LENGTH EQUIVALENT TUBE                    36
      BANK_TITLE='RADIATION LENGTH EQUIVALENT TUBE'                       36
      BPATH_ISAN='ISAN*'                                                  36
      IDNUM_ISAN(1)=0                                                     37
C VERSION                                                                 37
      ISAN_VERSION=1                                                      37
      CISAN_VERSION='VERSION'                                             37
C INNERRADIUS (LENGTH = LADDER LENGTH)                                    38
      ISAN_RMIN=14.2                                                      38
      CISAN_RMIN='INNERRADIUS (LENGTH = LADDER LENGTH)'                   38
C THICKNESS (1% RL SILICON)                                               39
      ISAN_THK=0.0636                                                     39
      CISAN_THK='THICKNESS (1% RL SILICON)'                               39
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISAN',    40
     *BPATH_ISAN,IDNUM_ISAN,1, MAP_ISAN,NAMES_ISAN,COMMEN_ISAN,           40
     *BANK_ISAN,LEN_ISAN,LLEN_ISAN,LINK_ISAN,FLAG_ISAN)                   40
C *                                                                       40
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISAN',    40
     *BPATH_ISAN,IDNUM_ISAN,1, MAP_ISAN,NAMES_ISAN,COMMEN_ISAN,           40
     *BANK_ISAN,LEN_ISAN,LLEN_ISAN,LINK_ISAN,FLAG_ISAN)                   40
C *                                                                       41
      CALL AGSBEGM(MODULE,IPRIN)                                          41
      IF (IPRIN.LT.0) RETURN                                              41
C Filling bank ISSS - SILICON SENSOR                                      41
      BANK_TITLE='SILICON SENSOR'                                         41
      BPATH_ISSS='ISSS*'                                                  41
      IDNUM_ISSS(1)=0                                                     42
C VERSION                                                                 42
      ISSS_VERSION=1                                                      42
      CISSS_VERSION='VERSION'                                             42
C ACTIVE AREA WIDTH                                                       43
      ISSS_ACTIVEW=3.6823                                                 43
      CISSS_ACTIVEW='ACTIVE AREA WIDTH'                                   43
C ACTIVE AREA HEIGHT                                                      44
      ISSS_ACTIVEH=4.0400                                                 44
      CISSS_ACTIVEH='ACTIVE AREA HEIGHT'                                  44
C ACTIVE AREA THICKNESS                                                   45
      ISSS_ACTIVETHK=0.0300                                               45
      CISSS_ACTIVETHK='ACTIVE AREA THICKNESS'                             45
C PASSIVE AREA THICKNESS                                                  46
      ISSS_PASSIVETHK=0.0800                                              46
      CISSS_PASSIVETHK='PASSIVE AREA THICKNESS'                           46
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISSS',    47
     *BPATH_ISSS,IDNUM_ISSS,1, MAP_ISSS,NAMES_ISSS,COMMEN_ISSS,           47
     *BANK_ISSS,LEN_ISSS,LLEN_ISSS,LINK_ISSS,FLAG_ISSS)                   47
C *                                                                       47
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ISSS',    47
     *BPATH_ISSS,IDNUM_ISSS,1, MAP_ISSS,NAMES_ISSS,COMMEN_ISSS,           47
     *BANK_ISSS,LEN_ISSS,LLEN_ISSS,LINK_ISSS,FLAG_ISSS)                   47
C *                                                                       48
      CALL AGSBEGM(MODULE,IPRIN)                                          48
      IF (IPRIN.LT.0) RETURN                                              48
C Filling bank PARO - DEFAULT                                             48
      BANK_TITLE='DEFAULT'                                                48
      BPATH_PARO='PARO*'                                                  48
      IDNUM_PARO(1)=0                                                     49
C NONE                                                                    49
      PARO_VERSION=0                                                      49
      CPARO_VERSION='NONE'                                                49
C DEFAULT : NO SHIFT                                                      50
      PARO_DX=0                                                           50
      CPARO_DX='DEFAULT : NO SHIFT'                                       50
C DEFAULT : NO SHIFT                                                      51
      PARO_DY=0                                                           51
      CPARO_DY='DEFAULT : NO SHIFT'                                       51
C DEFAULT : NO SHIFT                                                      52
      PARO_DZ=0                                                           52
      CPARO_DZ='DEFAULT : NO SHIFT'                                       52
C DEFAULT : NO SHIFT                                                      53
      PARO_MULT=0                                                         53
      CPARO_MULT='DEFAULT : NO SHIFT'                                     53
C 1..24                                                                   55
      PARO_LAD(1)=0                                                       55
      PARO_LAD(2)=0                                                       55
      PARO_LAD(3)=0                                                       55
      PARO_LAD(4)=0                                                       55
      PARO_LAD(5)=0                                                       55
      PARO_LAD(6)=0                                                       55
      PARO_LAD(7)=0                                                       55
      PARO_LAD(8)=0                                                       55
      PARO_LAD(9)=0                                                       55
      PARO_LAD(10)=0                                                      55
      PARO_LAD(11)=0                                                      55
      PARO_LAD(12)=0                                                      55
      PARO_LAD(13)=0                                                      55
      PARO_LAD(14)=0                                                      55
      PARO_LAD(15)=0                                                      55
      PARO_LAD(16)=0                                                      55
      PARO_LAD(17)=0                                                      55
      PARO_LAD(18)=0                                                      55
      PARO_LAD(19)=0                                                      55
      PARO_LAD(20)=0                                                      55
      PARO_LAD(21)=0                                                      55
      PARO_LAD(22)=0                                                      55
      PARO_LAD(23)=0                                                      55
      PARO_LAD(24)=0                                                      55
      CPARO_LAD='1..24'                                                   55
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    56
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           56
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   56
C *                                                                       56
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    56
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           56
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   56
C *                                                                       57
      CALL AGSBEGM(MODULE,IPRIN)                                          57
      IF (IPRIN.LT.0) RETURN                                              57
C Filling bank PARO - SHIFT                                               57
      BANK_TITLE='SHIFT'                                                  57
      BPATH_PARO='PARO*'                                                  57
      IDNUM_PARO(1)=0                                                     58
C NONE                                                                    58
      PARO_VERSION=1                                                      58
      CPARO_VERSION='NONE'                                                58
C SHIFT FOR ALL LADDERS                                                   59
      PARO_DX=0                                                           59
      CPARO_DX='SHIFT FOR ALL LADDERS'                                    59
C DEFAULT = NO SHIFT                                                      60
      PARO_DY=0                                                           60
      CPARO_DY='DEFAULT = NO SHIFT'                                       60
C DEFAULT = NO SHIFT                                                      61
      PARO_DZ=1                                                           61
      CPARO_DZ='DEFAULT = NO SHIFT'                                       61
C MAX IS 1 CM                                                             62
      PARO_MULT=5                                                         62
      CPARO_MULT='MAX IS 1 CM'                                            62
C 1..24                                                                   64
      PARO_LAD(1)=1                                                       64
      PARO_LAD(2)=1                                                       64
      PARO_LAD(3)=1                                                       64
      PARO_LAD(4)=1                                                       64
      PARO_LAD(5)=1                                                       64
      PARO_LAD(6)=1                                                       64
      PARO_LAD(7)=1                                                       64
      PARO_LAD(8)=1                                                       64
      PARO_LAD(9)=1                                                       64
      PARO_LAD(10)=1                                                      64
      PARO_LAD(11)=1                                                      64
      PARO_LAD(12)=1                                                      64
      PARO_LAD(13)=1                                                      64
      PARO_LAD(14)=1                                                      64
      PARO_LAD(15)=1                                                      64
      PARO_LAD(16)=1                                                      64
      PARO_LAD(17)=1                                                      64
      PARO_LAD(18)=1                                                      64
      PARO_LAD(19)=1                                                      64
      PARO_LAD(20)=1                                                      64
      PARO_LAD(21)=1                                                      64
      PARO_LAD(22)=1                                                      64
      PARO_LAD(23)=1                                                      64
      PARO_LAD(24)=1                                                      64
      CPARO_LAD='1..24'                                                   64
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    65
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           65
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   65
C *                                                                       65
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    65
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           65
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   65
C *                                                                       66
      CALL AGSBEGM(MODULE,IPRIN)                                          66
      IF (IPRIN.LT.0) RETURN                                              66
C Filling bank PARA - DEFAULT                                             66
      BANK_TITLE='DEFAULT'                                                66
      BPATH_PARA='PARA*'                                                  66
      IDNUM_PARA(1)=0                                                     67
C NONE                                                                    67
      PARA_VERSION=0                                                      67
      CPARA_VERSION='NONE'                                                67
C DEFAULT = NO ROT                                                        68
      PARA_DA=0                                                           68
      CPARA_DA='DEFAULT = NO ROT'                                         68
C DEFAULT = NO ROT.                                                       69
      PARA_DB=0                                                           69
      CPARA_DB='DEFAULT = NO ROT.'                                        69
C DEFAULT = NO ROT.                                                       70
      PARA_DG=0                                                           70
      CPARA_DG='DEFAULT = NO ROT.'                                        70
C DEFAULT = NO ROT.                                                       71
      PARA_MULT=0                                                         71
      CPARA_MULT='DEFAULT = NO ROT.'                                      71
C 1..24                                                                   73
      PARA_LAD(1)=0                                                       73
      PARA_LAD(2)=0                                                       73
      PARA_LAD(3)=0                                                       73
      PARA_LAD(4)=0                                                       73
      PARA_LAD(5)=0                                                       73
      PARA_LAD(6)=0                                                       73
      PARA_LAD(7)=0                                                       73
      PARA_LAD(8)=0                                                       73
      PARA_LAD(9)=0                                                       73
      PARA_LAD(10)=0                                                      73
      PARA_LAD(11)=0                                                      73
      PARA_LAD(12)=0                                                      73
      PARA_LAD(13)=0                                                      73
      PARA_LAD(14)=0                                                      73
      PARA_LAD(15)=0                                                      73
      PARA_LAD(16)=0                                                      73
      PARA_LAD(17)=0                                                      73
      PARA_LAD(18)=0                                                      73
      PARA_LAD(19)=0                                                      73
      PARA_LAD(20)=0                                                      73
      PARA_LAD(21)=0                                                      73
      PARA_LAD(22)=0                                                      73
      PARA_LAD(23)=0                                                      73
      PARA_LAD(24)=0                                                      73
      CPARA_LAD='1..24'                                                   73
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    74
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           74
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   74
C *                                                                       74
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    74
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           74
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   74
C *                                                                       75
      CALL AGSBEGM(MODULE,IPRIN)                                          75
      IF (IPRIN.LT.0) RETURN                                              75
C Filling bank PARA - ROTATION GAMMA                                      75
      BANK_TITLE='ROTATION GAMMA'                                         75
      BPATH_PARA='PARA*'                                                  75
      IDNUM_PARA(1)=0                                                     76
C NONE                                                                    76
      PARA_VERSION=1                                                      76
      CPARA_VERSION='NONE'                                                76
C NO ROT.                                                                 77
      PARA_DA=0                                                           77
      CPARA_DA='NO ROT.'                                                  77
C NO ROT.                                                                 78
      PARA_DB=0                                                           78
      CPARA_DB='NO ROT.'                                                  78
C ROT. FOR ALL LADDERS                                                    79
      PARA_DG=1                                                           79
      CPARA_DG='ROT. FOR ALL LADDERS'                                     79
C MAX IS 1 DEGREES                                                        80
      PARA_MULT=1                                                         80
      CPARA_MULT='MAX IS 1 DEGREES'                                       80
C 1..24                                                                   82
      PARA_LAD(1)=1                                                       82
      PARA_LAD(2)=1                                                       82
      PARA_LAD(3)=1                                                       82
      PARA_LAD(4)=1                                                       82
      PARA_LAD(5)=1                                                       82
      PARA_LAD(6)=1                                                       82
      PARA_LAD(7)=1                                                       82
      PARA_LAD(8)=1                                                       82
      PARA_LAD(9)=1                                                       82
      PARA_LAD(10)=1                                                      82
      PARA_LAD(11)=1                                                      82
      PARA_LAD(12)=1                                                      82
      PARA_LAD(13)=1                                                      82
      PARA_LAD(14)=1                                                      82
      PARA_LAD(15)=1                                                      82
      PARA_LAD(16)=1                                                      82
      PARA_LAD(17)=1                                                      82
      PARA_LAD(18)=1                                                      82
      PARA_LAD(19)=1                                                      82
      PARA_LAD(20)=1                                                      82
      PARA_LAD(21)=1                                                      82
      PARA_LAD(22)=1                                                      82
      PARA_LAD(23)=1                                                      82
      PARA_LAD(24)=1                                                      82
      CPARA_LAD='1..24'                                                   82
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    83
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           83
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   83
C *                                                                       83
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    83
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           83
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   83
      RADDEG = 3.14159265/180.0                                           84
      HALF = ISBG_TILT/2.0                                                85
C use ISMG bank                                                           86
      BPATH_ISMG='ISMG*'                                                  86
      IDNUM_ISMG(1)=0                                                     86
      ISMG_VERSION=1                                                      86
      CALL AGDGETP('ISTDGEO0','*',-999,'ISMG',NISMG_VERSION,              86
     *RE_ISMG_VERSION,BPATH_ISMG,IDNUM_ISMG,1,MAP_ISMG,NAMES_ISMG,        86
     *BANK_ISMG,LEN_ISMG,LLEN_ISMG,LINK_ISMG,FLAG_ISMG)                   86
C use ISAN bank                                                           87
      BPATH_ISAN='ISAN*'                                                  87
      IDNUM_ISAN(1)=0                                                     87
      ISAN_VERSION=1                                                      87
      CALL AGDGETP('ISTDGEO0','*',-999,'ISAN',NISAN_VERSION,              87
     *RE_ISAN_VERSION,BPATH_ISAN,IDNUM_ISAN,1,MAP_ISAN,NAMES_ISAN,        87
     *BANK_ISAN,LEN_ISAN,LLEN_ISAN,LINK_ISAN,FLAG_ISAN)                   87
C use ISSS bank                                                           88
      BPATH_ISSS='ISSS*'                                                  88
      IDNUM_ISSS(1)=0                                                     88
      ISSS_VERSION=1                                                      88
      CALL AGDGETP('ISTDGEO0','*',-999,'ISSS',NISSS_VERSION,              88
     *RE_ISSS_VERSION,BPATH_ISSS,IDNUM_ISSS,1,MAP_ISSS,NAMES_ISSS,        88
     *BANK_ISSS,LEN_ISSS,LLEN_ISSS,LINK_ISSS,FLAG_ISSS)                   88
C use PARO bank                                                           89
      BPATH_PARO='PARO*'                                                  89
      IDNUM_PARO(1)=0                                                     89
      PARO_VERSION=0                                                      89
      CALL AGDGETP('ISTDGEO0','*',-999,'PARO',NPARO_VERSION,              89
     *RE_PARO_VERSION,BPATH_PARO,IDNUM_PARO,1,MAP_PARO,NAMES_PARO,        89
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   89
C use PARA bank                                                           90
      BPATH_PARA='PARA*'                                                  90
      IDNUM_PARA(1)=0                                                     90
      PARA_VERSION=0                                                      90
      CALL AGDGETP('ISTDGEO0','*',-999,'PARA',NPARA_VERSION,              90
     *RE_PARA_VERSION,BPATH_PARA,IDNUM_PARA,1,MAP_PARA,NAMES_PARA,        90
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   90
      MODULE_THK = ISSS_ACTIVETHK+ISSS_PASSIVETHK                         91
      LADDER_THK = MODULE_THK                                             92
      LADDER_WIDTH = ISSS_ACTIVEW                                         93
      DEBUG = 0                                                           94
      LADDER_LENGTH = ISBG_NUNIT*ISSS_ACTIVEH                             95
      CALL AGSBEGM(MODULE,IPRIN)                                          96
C Check IPRIN<0                                                           96
      IF (IPRIN.LT.0) RETURN                                              96
C Create IBMO :                                                           96
C Call To Local Procedure IBMO                                            96
      ASSIGN 5020 TO BLOCK_IBMO                                           96
      GO TO  5010                                                         96
5020  CONTINUE                                                            96
C *                                                                       96
C Geant POSITION : IBMO ===> MOTHER='IDSM' <===                           97
      CALL AGSRESET                                                       97
      AG_MOTHER = 'IDSM'                                                  97
      AG_TITLE = 'IBMO'                                                   97
      AG_EXNAME = 'POSITION'                                              97
      AG_PARLIST = 'MOTHER'                                               97
      CALL AXPOSITION                                                     97
C *                                                                       97
C ----------------------------------------------------------------------  97
C ------------ IBMO                                                       97
      CALL AGSENDM                                                        99
      RETURN                                                              99
C Begin Local Procedure IBMO                                              99
C IBMO MAIN VOLUME IN THE AGML GEOMETRY                                   99
5010  CONTINUE                                                            99
         AG_TITLE='IBMO'                                                  99
         AG_MODULE=MODULE                                                 99
         AG_MARK='V00'                                                    99
         CALL AGSPUSH                                                     99
         AG_MODULE=MODULE                                                 99
C    *                                                                    99
C    Geant MATERIAL : AIR ===> inherited arguments used <===              100
         CALL AGSRESET                                                    100
         AG_TITLE = 'AIR'                                                 100
         AG_EXNAME = 'MATERIAL'                                           100
         AG_PARLIST = 'NONE'                                              100
         CALL AXMATERIAL                                                  100
C    *                                                                    100
C    Geant ATTRIBUTE : IBMO ===> SEEN=0 COLO=1 <===                       100
         CALL AGSRESET                                                    101
         AG_SEEN = 0                                                      101
         AG_COLO = 1                                                      101
         AG_TITLE = 'IBMO'                                                101
         AG_EXNAME = 'ATTRIBUTE'                                          101
         AG_PARLIST = 'SEEN_COLO'                                         101
         CALL AXATTRIBUTE                                                 101
C    *                                                                    101
C    Geant SHAPE : TUBE ===> RMIN=ISMG_RIN RMAX=ISMG_ROUT, DZ=LADDER_LEN  101
C    GTH/2.0 <===                                                         101
         CALL AGSRESET                                                    103
         AG_RMIN = ISMG_RIN                                               103
         AG_RMAX = ISMG_ROUT                                              103
         AG_DZ = LADDER_LENGTH/2.0                                        103
         AG_TITLE = 'TUBE'                                                103
         AG_EXNAME = 'SHAPE'                                              103
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      103
         CALL AXSHAPE                                                     103
C    *                                                                    103
         MODULERADIUS = ISBG_RADIUS + MODULE_THK/2.0 - ISSS_ACTIVETHK/    103
     *   2.0                                                              103
C    *                                                                    105
         NL=1                                                             105
            GO TO 5033                                                    105
5031        NL=NL+1                                                       105
5033        IF(NL-(ISBG_NLADDER).GT.0) GO TO 5032                         105
            AANGLE=(360.0/ISBG_NLADDER)*NL - ISBG_TILT                    106
            BANGLE=(AANGLE+ISBG_TILT)*RADDEG                              107
            INIX=0.0                                                      108
            INIY=0.0                                                      109
C       Check DEBUG==1                                                    110
            IF (DEBUG.EQ.1) THEN                                          110
            WRITE(LOUT,5040)PARO_DX*PARO_MULT                             110
5040        FORMAT('shift in X =', F6.4)                                  110
            WRITE(LOUT,5050)PARO_DY*PARO_MULT                             111
5050        FORMAT('shift in Y =', F6.4)                                  111
            WRITE(LOUT,5060)PARO_DZ*PARO_MULT                             112
5060        FORMAT('shift in Z =', F6.4)                                  112
            ENDIF                                                         114
            XPOS=MODULERADIUS*SIN((AANGLE+HALF)*RADDEG) +                 115
     *      (PARO_DX*PARO_MULT)*PARO_LAD(NL)                              115
            YPOS=MODULERADIUS*COS((AANGLE+HALF)*RADDEG) +                 116
     *      (PARO_DY*PARO_MULT)*PARO_LAD(NL)                              116
            ZPOS=0.0 + (PARO_DZ*PARO_MULT)*PARO_LAD(NL)                   117
C       Check DEBUG==1                                                    118
            IF (DEBUG.EQ.1) THEN                                          118
            WRITE(LOUT,5070)AANGLE                                        118
5070        FORMAT('aabgle=', F6.3)                                       118
            WRITE(LOUT,5080)BANGLE                                        119
5080        FORMAT('bangle=', F6.3)                                       119
            WRITE(LOUT,5090)XPOS                                          120
5090        FORMAT('x=', F6.3)                                            120
            WRITE(LOUT,5100)YPOS                                          121
5100        FORMAT('y=', F6.3)                                            121
            ENDIF                                                         123
C       Create IBAM :                                                     124
C       Call To Local Procedure IBAM                                      124
            ASSIGN 5120 TO BLOCK_IBAM                                     124
            GO TO  5110                                                   124
5120        CONTINUE                                                      124
C       *                                                                 124
C       Check DEBUG==1                                                    125
            IF (DEBUG.EQ.1) THEN                                          125
            WRITE(LOUT,5130)PARA_DA*PARA_MULT                             125
5130        FORMAT('rot along X =', F6.4)                                 125
            WRITE(LOUT,5140)PARA_DB*PARA_MULT                             126
5140        FORMAT('rot along Y =', F6.4)                                 126
            WRITE(LOUT,5150)PARA_DG*PARA_MULT                             127
5150        FORMAT('rot along Z =', F6.4)                                 127
            ENDIF                                                         129
C       Geant POSITION : IBAM ===> MOTHER='IBMO' X=XPOS Y=YPOS Z=ZPOS AL  132
C       PHAZ=-1*AANGLE-(ISBG_TILT + PARA_DG*PARA_MULT*PARA_LAD(NL)) ALPH  132
C       AX=-1*(PARA_DA*PARA_MULT)*PARA_LAD(NL) ALPHAY=-1*(PARA_DB*PARA_M  132
C       ULT)*PARA_LAD(NL) KONLY='MANY' <===                               132
            CALL AGSRESET                                                 132
            AG_MOTHER = 'IBMO'                                            132
            AG_X = XPOS                                                   132
            AG_Y = YPOS                                                   132
            AG_Z = ZPOS                                                   132
            AG_ALPHAN=AG_ALPHAN+1                                         132
            AG_ALPHAK(AG_ALPHAN)=3                                        132
            AG_ALPHAL(AG_ALPHAN)=-1*AANGLE-(ISBG_TILT +                   132
     *      PARA_DG*PARA_MULT*PARA_LAD(NL))                               132
            AG_ALPHAN=AG_ALPHAN+1                                         132
            AG_ALPHAK(AG_ALPHAN)=1                                        132
            AG_ALPHAL(AG_ALPHAN)=-1*(PARA_DA*PARA_MULT)*PARA_LAD(NL)      132
            AG_ALPHAN=AG_ALPHAN+1                                         132
            AG_ALPHAK(AG_ALPHAN)=2                                        132
            AG_ALPHAL(AG_ALPHAN)=-1*(PARA_DB*PARA_MULT)*PARA_LAD(NL)      132
            AG_KONLY = 'MANY'                                             132
            AG_TITLE = 'IBAM'                                             132
            AG_EXNAME = 'POSITION'                                        132
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_ALPHAX_ALPHAY_KONLY'        132
            CALL AXPOSITION                                               132
C       *                                                                 132
         GO TO 5031                                                       133
5032     CONTINUE                                                         133
5011  CALL AGSPOPB                                                        134
      GO TO BLOCK_IBMO, (5010 ,5020)                                      134
C End of Local Procedure                                                  134
C ----------------------------------------------------------------------  135
C ------------ IBAM                                                       135
C Begin Local Procedure IBAM                                              136
C IBAM IS THE MOTHER OF THE WHOLE LOMG LADDER                             136
5110  CONTINUE                                                            136
         AG_TITLE='IBAM'                                                  136
         AG_MODULE=MODULE                                                 136
         AG_MARK='V00'                                                    136
         CALL AGSPUSH                                                     136
         AG_MODULE=MODULE                                                 136
C    *                                                                    136
C    Geant MATERIAL : AIR ===> inherited arguments used <===              137
         CALL AGSRESET                                                    137
         AG_TITLE = 'AIR'                                                 137
         AG_EXNAME = 'MATERIAL'                                           137
         AG_PARLIST = 'NONE'                                              137
         CALL AXMATERIAL                                                  137
C    *                                                                    137
C    Geant ATTRIBUTE : IBAM ===> SEEN=0 COLO=6 <===                       137
         CALL AGSRESET                                                    138
         AG_SEEN = 0                                                      138
         AG_COLO = 6                                                      138
         AG_TITLE = 'IBAM'                                                138
         AG_EXNAME = 'ATTRIBUTE'                                          138
         AG_PARLIST = 'SEEN_COLO'                                         138
         CALL AXATTRIBUTE                                                 138
C    *                                                                    138
C    Geant SHAPE : BOX ===> DX=LADDER_WIDTH/2.0 DY=LADDER_THK/2.0, DZ=LA  138
C    DDER_LENGTH/2.0 <===                                                 138
         CALL AGSRESET                                                    140
         AG_DX = LADDER_WIDTH/2.0                                         140
         AG_DY = LADDER_THK/2.0                                           140
         AG_DZ = LADDER_LENGTH/2.0                                        140
         AG_TITLE = 'BOX'                                                 140
         AG_EXNAME = 'SHAPE'                                              140
         AG_PARLIST = 'DX_DY_DZ'                                          140
         CALL AXSHAPE                                                     140
C    *                                                                    140
C    *                                                                    140
         NU=1                                                             141
            GO TO 5163                                                    141
5161        NU=NU+1                                                       141
5163        IF(NU-(ISBG_NUNIT).GT.0) GO TO 5162                           141
C       Create IBLM :                                                     142
C       Call To Local Procedure IBLM                                      142
            ASSIGN 5180 TO BLOCK_IBLM                                     142
            GO TO  5170                                                   142
5180        CONTINUE                                                      142
C       *                                                                 142
C       Check DEBUG==1                                                    143
            IF (DEBUG.EQ.1) THEN                                          143
            WRITE(LOUT,5190)-1.0*LADDER_LENGTH/2.+ISSS_ACTIVEH*(NU-0.5)   143
5190        FORMAT('zpos=', F6.3)                                         143
            ENDIF                                                         145
C       Geant POSITION : IBLM ===> MOTHER='IBAM' X=0 Y=0 Z=-1.0*LADDER_L  146
C       ENGTH/2.+ISSS_ACTIVEH*(NU-0.5) <===                               146
            CALL AGSRESET                                                 146
            AG_MOTHER = 'IBAM'                                            146
            AG_X = 0                                                      146
            AG_Y = 0                                                      146
            AG_Z = -1.0*LADDER_LENGTH/2.+ISSS_ACTIVEH*(NU-0.5)            146
            AG_TITLE = 'IBLM'                                             146
            AG_EXNAME = 'POSITION'                                        146
            AG_PARLIST = 'MOTHER_X_Y_Z'                                   146
            CALL AXPOSITION                                               146
C       *                                                                 146
         GO TO 5161                                                       147
5162     CONTINUE                                                         147
5111  CALL AGSPOPB                                                        148
      GO TO BLOCK_IBAM, (5110 ,5120)                                      148
C End of Local Procedure                                                  148
C ----------------------------------------------------------------------  149
C ------------ IBLM                                                       149
C Begin Local Procedure IBLM                                              150
C IBLM MOTHER OF THE SENSOR ASSEMBLY                                      150
5170  CONTINUE                                                            150
         AG_TITLE='IBLM'                                                  150
         AG_MODULE=MODULE                                                 150
         AG_MARK='V00'                                                    150
         CALL AGSPUSH                                                     150
         AG_MODULE=MODULE                                                 150
C    *                                                                    150
C    Geant MATERIAL : AIR ===> inherited arguments used <===              151
         CALL AGSRESET                                                    151
         AG_TITLE = 'AIR'                                                 151
         AG_EXNAME = 'MATERIAL'                                           151
         AG_PARLIST = 'NONE'                                              151
         CALL AXMATERIAL                                                  151
C    *                                                                    151
C    Geant ATTRIBUTE : IBLM ===> SEEN=0 COLO=2 <===                       151
         CALL AGSRESET                                                    152
         AG_SEEN = 0                                                      152
         AG_COLO = 2                                                      152
         AG_TITLE = 'IBLM'                                                152
         AG_EXNAME = 'ATTRIBUTE'                                          152
         AG_PARLIST = 'SEEN_COLO'                                         152
         CALL AXATTRIBUTE                                                 152
C    *                                                                    152
C    Geant SHAPE : BOX ===> DX=LADDER_WIDTH/2.0 DY=MODULE_THK/2.0, DZ=IS  152
C    SS_ACTIVEH/2.0 <===                                                  152
         CALL AGSRESET                                                    154
         AG_DX = LADDER_WIDTH/2.0                                         154
         AG_DY = MODULE_THK/2.0                                           154
         AG_DZ = ISSS_ACTIVEH/2.0                                         154
         AG_TITLE = 'BOX'                                                 154
         AG_EXNAME = 'SHAPE'                                              154
         AG_PARLIST = 'DX_DY_DZ'                                          154
         CALL AXSHAPE                                                     154
C    *                                                                    154
C    Create IBSS :                                                        155
C    Call To Local Procedure IBSS                                         155
         ASSIGN 5210 TO BLOCK_IBSS                                        155
         GO TO  5200                                                      155
5210     CONTINUE                                                         155
C    *                                                                    155
C    Geant POSITION : IBSS ===> X=0.0 Y=(-1.0*MODULE_THK/2.0)+(ISSS_ACTI  156
C    VETHK/2.0) Z=0.0 <===                                                156
         CALL AGSRESET                                                    156
         AG_X = 0.0                                                       156
         AG_Y = (-1.0*MODULE_THK/2.0)+(ISSS_ACTIVETHK/2.0)                156
         AG_Z = 0.0                                                       156
         AG_TITLE = 'IBSS'                                                156
         AG_EXNAME = 'POSITION'                                           156
         AG_PARLIST = 'X_Y_Z'                                             156
         CALL AXPOSITION                                                  156
C    *                                                                    156
C    Create IBSP :                                                        157
C    Call To Local Procedure IBSP                                         157
         ASSIGN 5230 TO BLOCK_IBSP                                        157
         GO TO  5220                                                      157
5230     CONTINUE                                                         157
C    *                                                                    157
C    Geant POSITION : IBSP ===> X=0.0 Y=(1.0*MODULE_THK/2.0)-(ISSS_PASSI  158
C    VETHK/2.0) Z=0.0 <===                                                158
         CALL AGSRESET                                                    158
         AG_X = 0.0                                                       158
         AG_Y = (1.0*MODULE_THK/2.0)-(ISSS_PASSIVETHK/2.0)                158
         AG_Z = 0.0                                                       158
         AG_TITLE = 'IBSP'                                                158
         AG_EXNAME = 'POSITION'                                           158
         AG_PARLIST = 'X_Y_Z'                                             158
         CALL AXPOSITION                                                  158
C    *                                                                    158
5171  CALL AGSPOPB                                                        159
      GO TO BLOCK_IBLM, (5170 ,5180)                                      159
C End of Local Procedure                                                  159
C ----------------------------------------------------------------------  160
C ------------ IBSP                                                       160
C Begin Local Procedure IBSP                                              161
C IBSP IS THE SILICON SENSOR PASSIVE AREA                                 161
5220  CONTINUE                                                            161
         AG_TITLE='IBSP'                                                  161
         AG_MODULE=MODULE                                                 161
         AG_MARK='V00'                                                    161
         CALL AGSPUSH                                                     161
         AG_MODULE=MODULE                                                 161
C    *                                                                    161
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          162
         CALL AGSRESET                                                    162
         AG_TITLE = 'SILICON'                                             162
         AG_EXNAME = 'MATERIAL'                                           162
         AG_PARLIST = 'NONE'                                              162
         CALL AXMATERIAL                                                  162
C    *                                                                    162
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    162
         CALL AGSRESET                                                    163
         AG_ISVOL = 0                                                     163
         AG_TITLE = 'PASSIVESILICON'                                      163
         AG_EXNAME = 'MATERIAL'                                           163
         AG_PARLIST = 'ISVOL'                                             163
         CALL AXMATERIAL                                                  163
C    *                                                                    163
C    Geant ATTRIBUTE : IBSP ===> SEEN=1 COLO=4 <===                       163
         CALL AGSRESET                                                    164
         AG_SEEN = 1                                                      164
         AG_COLO = 4                                                      164
         AG_TITLE = 'IBSP'                                                164
         AG_EXNAME = 'ATTRIBUTE'                                          164
         AG_PARLIST = 'SEEN_COLO'                                         164
         CALL AXATTRIBUTE                                                 164
C    *                                                                    164
C    Geant SHAPE : BOX ===> DX=ISSS_ACTIVEW/2.0 DY=ISSS_PASSIVETHK/2.0,   164
C    DZ=ISSS_ACTIVEH/2.0 <===                                             164
         CALL AGSRESET                                                    166
         AG_DX = ISSS_ACTIVEW/2.0                                         166
         AG_DY = ISSS_PASSIVETHK/2.0                                      166
         AG_DZ = ISSS_ACTIVEH/2.0                                         166
         AG_TITLE = 'BOX'                                                 166
         AG_EXNAME = 'SHAPE'                                              166
         AG_PARLIST = 'DX_DY_DZ'                                          166
         CALL AXSHAPE                                                     166
C    *                                                                    166
5221  CALL AGSPOPB                                                        167
      GO TO BLOCK_IBSP, (5220 ,5230)                                      167
C End of Local Procedure                                                  167
C ----------------------------------------------------------------------  168
C ------------ IBSS                                                       168
C Begin Local Procedure IBSS                                              169
C IBSS IS THE SILICON SENSOR ACTIVE AREA                                  169
5200  CONTINUE                                                            169
         AG_TITLE='IBSS'                                                  169
         AG_MODULE=MODULE                                                 169
         AG_MARK='V00'                                                    169
         CALL AGSPUSH                                                     169
         AG_MODULE=MODULE                                                 169
C    *                                                                    169
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          170
         CALL AGSRESET                                                    170
         AG_TITLE = 'SILICON'                                             170
         AG_EXNAME = 'MATERIAL'                                           170
         AG_PARLIST = 'NONE'                                              170
         CALL AXMATERIAL                                                  170
C    *                                                                    170
C    Geant MATERIAL : ISTSSENSITIVE ===> ISVOL=1 <===                     170
         CALL AGSRESET                                                    171
         AG_ISVOL = 1                                                     171
         AG_TITLE = 'ISTSSENSITIVE'                                       171
         AG_EXNAME = 'MATERIAL'                                           171
         AG_PARLIST = 'ISVOL'                                             171
         CALL AXMATERIAL                                                  171
C    *                                                                    171
C    Geant ATTRIBUTE : IBSS ===> SEEN=1 COLO=2 <===                       171
         CALL AGSRESET                                                    172
         AG_SEEN = 1                                                      172
         AG_COLO = 2                                                      172
         AG_TITLE = 'IBSS'                                                172
         AG_EXNAME = 'ATTRIBUTE'                                          172
         AG_PARLIST = 'SEEN_COLO'                                         172
         CALL AXATTRIBUTE                                                 172
C    *                                                                    172
C    Geant SHAPE : BOX ===> DX=ISSS_ACTIVEW/2.0 DY=ISSS_ACTIVETHK/2.0, D  172
C    Z=ISSS_ACTIVEH/2.0 <===                                              172
         CALL AGSRESET                                                    174
         AG_DX = ISSS_ACTIVEW/2.0                                         174
         AG_DY = ISSS_ACTIVETHK/2.0                                       174
         AG_DZ = ISSS_ACTIVEH/2.0                                         174
         AG_TITLE = 'BOX'                                                 174
         AG_EXNAME = 'SHAPE'                                              174
         AG_PARLIST = 'DX_DY_DZ'                                          174
         CALL AXSHAPE                                                     174
C    *                                                                    174
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100) CX:10: CY:1  174
C    0: CZ:10: SLENG:16:(0,500), TOF:16: STEP:.01: ELOSS:16:              174
         AG_TITLE = 'IBSS'                                                176
         AG_EXNAME = 'HITS'                                               176
         AG_TYPE=AG_MAGIC                                                 176
         CALL AGSDETV('IBSS')                                             176
C    *                                                                    176
         AG_PARLIST='Z'                                                   176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = AG_MAGIC                                              176
         AG_BIN=.001                                                      176
         AG_OPTION='S'                                                    176
         AG_FHMIN = AG_MAGIC                                              176
         AG_FHMAX = AG_MAGIC                                              176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='Y'                                                   176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = AG_MAGIC                                              176
         AG_BIN=.001                                                      176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='X'                                                   176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = AG_MAGIC                                              176
         AG_BIN=.001                                                      176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='PTOT'                                                176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 16                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_OPTION='*'                                                    176
         AG_FHMIN = 0                                                     176
         AG_FHMAX = 100                                                   176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='CX'                                                  176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 10                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='CY'                                                  176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 10                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='CZ'                                                  176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 10                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='SLENG'                                               176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 16                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_OPTION='*'                                                    176
         AG_FHMIN = 0                                                     176
         AG_FHMAX = 500                                                   176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='TOF'                                                 176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 16                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='STEP'                                                176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = AG_MAGIC                                              176
         AG_BIN=.01                                                       176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         AG_PARLIST='ELOSS'                                               176
         AG_TITLE = 'DETH'                                                176
         AG_NBITS = 16                                                    176
         AG_BIN=AG_MAGIC                                                  176
         AG_FHMIN=AG_MAGIC                                                176
         AG_FHMAX=AG_MAGIC                                                176
         AG_OPTION='*'                                                    176
         CALL AGSDETH (0)                                                 176
C    *                                                                    176
         CALL AGSDETU('IBSS')                                             176
C    *                                                                    176
5201  CALL AGSPOPB                                                        177
      GO TO BLOCK_IBSS, (5200 ,5210)                                      177
C End of Local Procedure                                                  177
      WRITE(LOUT,5240)'===>>  [End AgML Module IstdGeo0]  <<==='          177
5240  FORMAT(A32,/,/)                                                     177
C MODULE ISTDGEO0                                                         179
      END                                                                 179
