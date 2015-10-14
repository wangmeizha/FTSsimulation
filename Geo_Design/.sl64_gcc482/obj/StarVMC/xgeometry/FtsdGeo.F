C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FTSDGEO                                                  1
C FTSDGEO IS THE BEAM BEAM COUNTER MODULES GEOMETRY                       1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FTSDGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE BEAM BEAM COUNTER MODULES GEOMETRY'/          1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 10/7/2015                                                       1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'10/7/2015'/                                  2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR JASON WEBB                                                       2
      CHARACTER*40 AUTHOR /'JASON WEBB'/                                  3
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
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   4
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        4
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       4
C - GEANT description of logical units                                    4
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            4
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      4
      COMMON/GCMAIL/ CHMAIL                                               4
      CHARACTER*132 CHMAIL                                                4
      INTEGER BLOCK_FTSM                                                  4
      INTEGER BLOCK_FTSD                                                  8
      INTEGER BLOCK_FTSA                                                  8
      INTEGER BLOCK_FTSS                                                  8
C -                                                                       9
      REAL MAIN_VERSION                                                   9
      EQUIVALENCE (BANK_MAIN(3),MAIN_VERSION,RE_MAIN_VERSION),            9
     *(NAMES_MAIN(1),NMAIN_VERSION)                                       9
      EQUIVALENCE (COMMEN_MAIN(1),CMAIN_VERSION),(MAP_MAIN(1,1),          9
     *MMAIN_VERSION)                                                      9
      CHARACTER CMAIN_VERSION*40/' '/                                     9
      CHARACTER NMAIN_VERSION*12/'R.VERSION'/                             9
      INTEGER MMAIN_VERSION(2)/1,1/                                       9
      REAL RE_MAIN_VERSION                                                9
C -                                                                       9
      INTEGER LEN_MAIN,LLEN_MAIN,LINK_MAIN                                9
      PARAMETER (LEN_MAIN=3)                                              9
      PARAMETER (LLEN_MAIN=1)                                             9
      INTEGER FLAG_MAIN/0/,MAP_MAIN(2,LLEN_MAIN),IDNUM_MAIN(10)           9
      CHARACTER NAMES_MAIN(LLEN_MAIN)*12,COMMEN_MAIN(LLEN_MAIN)*40        9
      DATA IDNUM_MAIN/10*0/                                               9
      REAL BANK_MAIN(LEN_MAIN)                                            9
      CHARACTER*60 BPATH_MAIN/' '/                                        9
      SAVE FLAG_MAIN,MAP_MAIN,IDNUM_MAIN,NAMES_MAIN,COMMEN_MAIN,          9
     *BPATH_MAIN                                                          9
      DATA LINK_MAIN/0/,BANK_MAIN/LEN_MAIN*0/                             9
C *                                                                       9
C -                                                                       11
      REAL FDSK_VERSION                                                   11
      EQUIVALENCE (BANK_FDSK(3),FDSK_VERSION,RE_FDSK_VERSION),            11
     *(NAMES_FDSK(1),NFDSK_VERSION)                                       11
      EQUIVALENCE (COMMEN_FDSK(1),CFDSK_VERSION),(MAP_FDSK(1,1),          11
     *MFDSK_VERSION)                                                      11
      CHARACTER CFDSK_VERSION*40/' '/                                     11
      CHARACTER NFDSK_VERSION*12/'R.VERSION'/                             11
      INTEGER MFDSK_VERSION(2)/1,1/                                       11
      REAL RE_FDSK_VERSION                                                11
C -                                                                       11
      INTEGER FDSK_DISKID                                                 11
      EQUIVALENCE (BANK_FDSK(4),FDSK_DISKID,RE_FDSK_DISKID),              11
     *(NAMES_FDSK(2),NFDSK_DISKID)                                        11
      EQUIVALENCE (COMMEN_FDSK(2),CFDSK_DISKID),(MAP_FDSK(1,2),           11
     *MFDSK_DISKID)                                                       11
      CHARACTER CFDSK_DISKID*40/' '/                                      11
      CHARACTER NFDSK_DISKID*12/'I.DISKID'/                               11
      INTEGER MFDSK_DISKID(2)/1,1/                                        11
      REAL RE_FDSK_DISKID                                                 11
C -                                                                       11
      REAL FDSK_X                                                         11
      EQUIVALENCE (BANK_FDSK(5),FDSK_X,RE_FDSK_X),(NAMES_FDSK(3),         11
     *NFDSK_X)                                                            11
      EQUIVALENCE (COMMEN_FDSK(3),CFDSK_X),(MAP_FDSK(1,3),MFDSK_X)        11
      CHARACTER CFDSK_X*40/' '/                                           11
      CHARACTER NFDSK_X*12/'R.X'/                                         11
      INTEGER MFDSK_X(2)/1,1/                                             11
      REAL RE_FDSK_X                                                      11
C -                                                                       11
      REAL FDSK_Y                                                         11
      EQUIVALENCE (BANK_FDSK(6),FDSK_Y,RE_FDSK_Y),(NAMES_FDSK(4),         11
     *NFDSK_Y)                                                            11
      EQUIVALENCE (COMMEN_FDSK(4),CFDSK_Y),(MAP_FDSK(1,4),MFDSK_Y)        11
      CHARACTER CFDSK_Y*40/' '/                                           11
      CHARACTER NFDSK_Y*12/'R.Y'/                                         11
      INTEGER MFDSK_Y(2)/1,1/                                             11
      REAL RE_FDSK_Y                                                      11
C -                                                                       11
      REAL FDSK_Z                                                         11
      EQUIVALENCE (BANK_FDSK(7),FDSK_Z,RE_FDSK_Z),(NAMES_FDSK(5),         11
     *NFDSK_Z)                                                            11
      EQUIVALENCE (COMMEN_FDSK(5),CFDSK_Z),(MAP_FDSK(1,5),MFDSK_Z)        11
      CHARACTER CFDSK_Z*40/' '/                                           11
      CHARACTER NFDSK_Z*12/'R.Z'/                                         11
      INTEGER MFDSK_Z(2)/1,1/                                             11
      REAL RE_FDSK_Z                                                      11
C -                                                                       11
      REAL FDSK_RMAX                                                      11
      EQUIVALENCE (BANK_FDSK(8),FDSK_RMAX,RE_FDSK_RMAX),(NAMES_FDSK(6),   11
     *NFDSK_RMAX)                                                         11
      EQUIVALENCE (COMMEN_FDSK(6),CFDSK_RMAX),(MAP_FDSK(1,6),             11
     *MFDSK_RMAX)                                                         11
      CHARACTER CFDSK_RMAX*40/' '/                                        11
      CHARACTER NFDSK_RMAX*12/'R.RMAX'/                                   11
      INTEGER MFDSK_RMAX(2)/1,1/                                          11
      REAL RE_FDSK_RMAX                                                   11
C -                                                                       11
      REAL FDSK_RMIN                                                      11
      EQUIVALENCE (BANK_FDSK(9),FDSK_RMIN,RE_FDSK_RMIN),(NAMES_FDSK(7),   11
     *NFDSK_RMIN)                                                         11
      EQUIVALENCE (COMMEN_FDSK(7),CFDSK_RMIN),(MAP_FDSK(1,7),             11
     *MFDSK_RMIN)                                                         11
      CHARACTER CFDSK_RMIN*40/' '/                                        11
      CHARACTER NFDSK_RMIN*12/'R.RMIN'/                                   11
      INTEGER MFDSK_RMIN(2)/1,1/                                          11
      REAL RE_FDSK_RMIN                                                   11
C -                                                                       11
      REAL FDSK_RMAXSENS                                                  11
      EQUIVALENCE (BANK_FDSK(10),FDSK_RMAXSENS,RE_FDSK_RMAXSENS),         11
     *(NAMES_FDSK(8),NFDSK_RMAXSENS)                                      11
      EQUIVALENCE (COMMEN_FDSK(8),CFDSK_RMAXSENS),(MAP_FDSK(1,8),         11
     *MFDSK_RMAXSENS)                                                     11
      CHARACTER CFDSK_RMAXSENS*40/' '/                                    11
      CHARACTER NFDSK_RMAXSENS*12/'R.RMAXSENS'/                           11
      INTEGER MFDSK_RMAXSENS(2)/1,1/                                      11
      REAL RE_FDSK_RMAXSENS                                               11
C -                                                                       11
      REAL FDSK_RMINSENS                                                  11
      EQUIVALENCE (BANK_FDSK(11),FDSK_RMINSENS,RE_FDSK_RMINSENS),         11
     *(NAMES_FDSK(9),NFDSK_RMINSENS)                                      11
      EQUIVALENCE (COMMEN_FDSK(9),CFDSK_RMINSENS),(MAP_FDSK(1,9),         11
     *MFDSK_RMINSENS)                                                     11
      CHARACTER CFDSK_RMINSENS*40/' '/                                    11
      CHARACTER NFDSK_RMINSENS*12/'R.RMINSENS'/                           11
      INTEGER MFDSK_RMINSENS(2)/1,1/                                      11
      REAL RE_FDSK_RMINSENS                                               11
C -                                                                       11
      REAL FDSK_THKSENS                                                   11
      EQUIVALENCE (BANK_FDSK(12),FDSK_THKSENS,RE_FDSK_THKSENS),           11
     *(NAMES_FDSK(10),NFDSK_THKSENS)                                      11
      EQUIVALENCE (COMMEN_FDSK(10),CFDSK_THKSENS),(MAP_FDSK(1,10),        11
     *MFDSK_THKSENS)                                                      11
      CHARACTER CFDSK_THKSENS*40/' '/                                     11
      CHARACTER NFDSK_THKSENS*12/'R.THKSENS'/                             11
      INTEGER MFDSK_THKSENS(2)/1,1/                                       11
      REAL RE_FDSK_THKSENS                                                11
C -                                                                       11
      REAL FDSK_THKSUPP                                                   11
      EQUIVALENCE (BANK_FDSK(13),FDSK_THKSUPP,RE_FDSK_THKSUPP),           11
     *(NAMES_FDSK(11),NFDSK_THKSUPP)                                      11
      EQUIVALENCE (COMMEN_FDSK(11),CFDSK_THKSUPP),(MAP_FDSK(1,11),        11
     *MFDSK_THKSUPP)                                                      11
      CHARACTER CFDSK_THKSUPP*40/' '/                                     11
      CHARACTER NFDSK_THKSUPP*12/'R.THKSUPP'/                             11
      INTEGER MFDSK_THKSUPP(2)/1,1/                                       11
      REAL RE_FDSK_THKSUPP                                                11
C -                                                                       11
      INTEGER LEN_FDSK,LLEN_FDSK,LINK_FDSK                                11
      PARAMETER (LEN_FDSK=13)                                             11
      PARAMETER (LLEN_FDSK=11)                                            11
      INTEGER FLAG_FDSK/0/,MAP_FDSK(2,LLEN_FDSK),IDNUM_FDSK(10)           11
      CHARACTER NAMES_FDSK(LLEN_FDSK)*12,COMMEN_FDSK(LLEN_FDSK)*40        11
      DATA IDNUM_FDSK/10*0/                                               11
      REAL BANK_FDSK(LEN_FDSK)                                            11
      CHARACTER*60 BPATH_FDSK/' '/                                        11
      SAVE FLAG_FDSK,MAP_FDSK,IDNUM_FDSK,NAMES_FDSK,COMMEN_FDSK,          11
     *BPATH_FDSK                                                          11
      DATA LINK_FDSK/0/,BANK_FDSK/LEN_FDSK*0/                             11
C *                                                                       11
      INTEGER *4 NDISK/0/, IDISK/0/                                       12
      REAL *4 ZMIN, ZMAX, RMIN, RMAX, XCENTER, YCENTER, ZCENTER           14
C * LAMBDA FUNCTION ETA2TAN                                               16
C ARGUEMENT FOR ETAX                                                      17
      REAL ETAX                                                           17
      REAL ETA2TAN                                                        18
      ETA2TAN(ETAX) = TAN(2*ATAN(EXP(-ETAX)))                             19
C *                                                                       20
      CALL AGSBEGM(MODULE,IPRIN)                                          20
      IF (IPRIN.LT.0) RETURN                                              20
C Filling bank MAIN - SELECT CONFIGURATION                                20
      BANK_TITLE='SELECT CONFIGURATION'                                   20
      BPATH_MAIN='MAIN*'                                                  20
      IDNUM_MAIN(1)=0                                                     21
C SELECTOR                                                                21
      MAIN_VERSION=1.0                                                    21
      CMAIN_VERSION='SELECTOR'                                            21
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MAIN',    22
     *BPATH_MAIN,IDNUM_MAIN,1, MAP_MAIN,NAMES_MAIN,COMMEN_MAIN,           22
     *BANK_MAIN,LEN_MAIN,LLEN_MAIN,LINK_MAIN,FLAG_MAIN)                   22
C *                                                                       22
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MAIN',    22
     *BPATH_MAIN,IDNUM_MAIN,1, MAP_MAIN,NAMES_MAIN,COMMEN_MAIN,           22
     *BANK_MAIN,LEN_MAIN,LLEN_MAIN,LINK_MAIN,FLAG_MAIN)                   22
C use default MAIN bank                                                   23
      BPATH_MAIN='MAIN*'                                                  23
      IDNUM_MAIN(1)=0                                                     23
      CALL AGDGETP('FTSDGEO','*',-999,'MAIN',' ',BANK_MAIN, BPATH_MAIN,   23
     *IDNUM_MAIN,1,MAP_MAIN,NAMES_MAIN,BANK_MAIN,LEN_MAIN,LLEN_MAIN,      23
     *LINK_MAIN,FLAG_MAIN)                                                23
C Check MAIN_VERSION==1.0                                                 24
      IF (MAIN_VERSION.EQ.1.0) THEN                                       24
      NDISK = NDISK + 1                                                   25
C *                                                                       26
      CALL AGSBEGM(MODULE,IPRIN)                                          26
      IF (IPRIN.LT.0) RETURN                                              26
C Filling bank FDSK - FIRST DISK                                          26
      BANK_TITLE='FIRST DISK'                                             26
      BPATH_FDSK='FDSK*'                                                  26
      IDNUM_FDSK(1)=0                                                     27
C DISK VERSION                                                            27
      FDSK_VERSION=1.0                                                    27
      CFDSK_VERSION='DISK VERSION'                                        27
C DISK NUMBER                                                             28
      FDSK_DISKID=NDISK                                                   28
      CFDSK_DISKID='DISK NUMBER'                                          28
C X CENTER OF DISK [CM]                                                   29
      FDSK_X=0.0                                                          29
      CFDSK_X='X CENTER OF DISK [CM]'                                     29
C Y CENTER OF DISK [CM]                                                   30
      FDSK_Y=0.0                                                          30
      CFDSK_Y='Y CENTER OF DISK [CM]'                                     30
C Z CENTER OF DISK [CM]                                                   31
      FDSK_Z=70.0                                                         31
      CFDSK_Z='Z CENTER OF DISK [CM]'                                     31
C MINIMUM RADIUS OF DISK [CM]                                             32
      FDSK_RMIN=70*ETA2TAN(4.0)-1.0                                       32
      CFDSK_RMIN='MINIMUM RADIUS OF DISK [CM]'                            32
C MAXIMUM RADIUS OF DISK [CM]                                             33
      FDSK_RMAX=70*ETA2TAN(2.5)+1.0                                       33
      CFDSK_RMAX='MAXIMUM RADIUS OF DISK [CM]'                            33
C MINIMUM RADIUS OF SENSITIVE LAYER [CM]                                  34
      FDSK_RMINSENS=70*ETA2TAN(4.0)                                       34
      CFDSK_RMINSENS='MINIMUM RADIUS OF SENSITIVE LAYER [CM]'             34
C MAXIMUM RADIUS OF SENSITIVE LAYER [CM]                                  35
      FDSK_RMAXSENS=70*ETA2TAN(2.5)                                       35
      CFDSK_RMAXSENS='MAXIMUM RADIUS OF SENSITIVE LAYER [CM]'             35
C THICKNESS OF SENSITIVE LAYER [CM]                                       36
      FDSK_THKSENS=0.2                                                    36
      CFDSK_THKSENS='THICKNESS OF SENSITIVE LAYER [CM]'                   36
C THICKNESS OF SUPPORT MATERIAL [CM]                                      37
      FDSK_THKSUPP=0.8                                                    37
      CFDSK_THKSUPP='THICKNESS OF SUPPORT MATERIAL [CM]'                  37
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    38
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           38
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   38
C *                                                                       38
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    38
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           38
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   38
      NDISK = NDISK + 1                                                   39
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank FDSK - SECOND DISK                                         40
      BANK_TITLE='SECOND DISK'                                            40
      BPATH_FDSK='FDSK*'                                                  40
      IDNUM_FDSK(1)=0                                                     41
C DISK VERSION                                                            41
      FDSK_VERSION=1.1                                                    41
      CFDSK_VERSION='DISK VERSION'                                        41
C DISK NUMBER                                                             42
      FDSK_DISKID=NDISK                                                   42
      CFDSK_DISKID='DISK NUMBER'                                          42
C X CENTER OF DISK [CM]                                                   43
      FDSK_X=0.0                                                          43
      CFDSK_X='X CENTER OF DISK [CM]'                                     43
C Y CENTER OF DISK [CM]                                                   44
      FDSK_Y=0.0                                                          44
      CFDSK_Y='Y CENTER OF DISK [CM]'                                     44
C Z CENTER OF DISK [CM]                                                   45
      FDSK_Z=93.33                                                        45
      CFDSK_Z='Z CENTER OF DISK [CM]'                                     45
C MINIMUM RADIUS OF DISK [CM]                                             46
      FDSK_RMIN=93.33*ETA2TAN(4.0)-1.0                                    46
      CFDSK_RMIN='MINIMUM RADIUS OF DISK [CM]'                            46
C MAXIMUM RADIUS OF DISK [CM]                                             47
      FDSK_RMAX=93.33*ETA2TAN(2.5)+1.0                                    47
      CFDSK_RMAX='MAXIMUM RADIUS OF DISK [CM]'                            47
C MINIMUM RADIUS OF SENSITIVE LAYER [CM]                                  48
      FDSK_RMINSENS=93.33*ETA2TAN(4.0)                                    48
      CFDSK_RMINSENS='MINIMUM RADIUS OF SENSITIVE LAYER [CM]'             48
C MAXIMUM RADIUS OF SENSITIVE LAYER [CM]                                  49
      FDSK_RMAXSENS=93.33*ETA2TAN(2.5)                                    49
      CFDSK_RMAXSENS='MAXIMUM RADIUS OF SENSITIVE LAYER [CM]'             49
C THICKNESS OF SENSITIVE LAYER [CM]                                       50
      FDSK_THKSENS=0.2                                                    50
      CFDSK_THKSENS='THICKNESS OF SENSITIVE LAYER [CM]'                   50
C THICKNESS OF SUPPORT MATERIAL [CM]                                      51
      FDSK_THKSUPP=0.8                                                    51
      CFDSK_THKSUPP='THICKNESS OF SUPPORT MATERIAL [CM]'                  51
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    52
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           52
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   52
C *                                                                       52
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    52
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           52
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   52
      NDISK = NDISK + 1                                                   53
C *                                                                       54
      CALL AGSBEGM(MODULE,IPRIN)                                          54
      IF (IPRIN.LT.0) RETURN                                              54
C Filling bank FDSK - THIRD DISK                                          54
      BANK_TITLE='THIRD DISK'                                             54
      BPATH_FDSK='FDSK*'                                                  54
      IDNUM_FDSK(1)=0                                                     55
C DISK VERSION                                                            55
      FDSK_VERSION=1.2                                                    55
      CFDSK_VERSION='DISK VERSION'                                        55
C DISK NUMBER                                                             56
      FDSK_DISKID=NDISK                                                   56
      CFDSK_DISKID='DISK NUMBER'                                          56
C X CENTER OF DISK [CM]                                                   57
      FDSK_X=0.0                                                          57
      CFDSK_X='X CENTER OF DISK [CM]'                                     57
C Y CENTER OF DISK [CM]                                                   58
      FDSK_Y=0.0                                                          58
      CFDSK_Y='Y CENTER OF DISK [CM]'                                     58
C Z CENTER OF DISK [CM]                                                   59
      FDSK_Z=116.66                                                       59
      CFDSK_Z='Z CENTER OF DISK [CM]'                                     59
C MINIMUM RADIUS OF DISK [CM]                                             60
      FDSK_RMIN=116.66*ETA2TAN(4.0)-1.0                                   60
      CFDSK_RMIN='MINIMUM RADIUS OF DISK [CM]'                            60
C MAXIMUM RADIUS OF DISK [CM]                                             61
      FDSK_RMAX=116.66*ETA2TAN(2.5)+1.0                                   61
      CFDSK_RMAX='MAXIMUM RADIUS OF DISK [CM]'                            61
C MINIMUM RADIUS OF SENSITIVE LAYER [CM]                                  62
      FDSK_RMINSENS=116.66*ETA2TAN(4.0)                                   62
      CFDSK_RMINSENS='MINIMUM RADIUS OF SENSITIVE LAYER [CM]'             62
C MAXIMUM RADIUS OF SENSITIVE LAYER [CM]                                  63
      FDSK_RMAXSENS=116.66*ETA2TAN(2.5)                                   63
      CFDSK_RMAXSENS='MAXIMUM RADIUS OF SENSITIVE LAYER [CM]'             63
C THICKNESS OF SENSITIVE LAYER [CM]                                       64
      FDSK_THKSENS=0.2                                                    64
      CFDSK_THKSENS='THICKNESS OF SENSITIVE LAYER [CM]'                   64
C THICKNESS OF SUPPORT MATERIAL [CM]                                      65
      FDSK_THKSUPP=0.8                                                    65
      CFDSK_THKSUPP='THICKNESS OF SUPPORT MATERIAL [CM]'                  65
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    66
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           66
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   66
C *                                                                       66
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    66
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           66
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   66
      NDISK = NDISK + 1                                                   67
C *                                                                       68
      CALL AGSBEGM(MODULE,IPRIN)                                          68
      IF (IPRIN.LT.0) RETURN                                              68
C Filling bank FDSK - FOURTH DISK                                         68
      BANK_TITLE='FOURTH DISK'                                            68
      BPATH_FDSK='FDSK*'                                                  68
      IDNUM_FDSK(1)=0                                                     69
C DISK VERSION                                                            69
      FDSK_VERSION=1.2                                                    69
      CFDSK_VERSION='DISK VERSION'                                        69
C DISK NUMBER                                                             70
      FDSK_DISKID=NDISK                                                   70
      CFDSK_DISKID='DISK NUMBER'                                          70
C X CENTER OF DISK [CM]                                                   71
      FDSK_X=0.0                                                          71
      CFDSK_X='X CENTER OF DISK [CM]'                                     71
C Y CENTER OF DISK [CM]                                                   72
      FDSK_Y=0.0                                                          72
      CFDSK_Y='Y CENTER OF DISK [CM]'                                     72
C Z CENTER OF DISK [CM]                                                   73
      FDSK_Z=140.0                                                        73
      CFDSK_Z='Z CENTER OF DISK [CM]'                                     73
C MINIMUM RADIUS OF DISK [CM]                                             74
      FDSK_RMIN=140.0*ETA2TAN(4.0)-1.0                                    74
      CFDSK_RMIN='MINIMUM RADIUS OF DISK [CM]'                            74
C MAXIMUM RADIUS OF DISK [CM]                                             75
      FDSK_RMAX=140.0*ETA2TAN(2.5)+1.0                                    75
      CFDSK_RMAX='MAXIMUM RADIUS OF DISK [CM]'                            75
C MINIMUM RADIUS OF SENSITIVE LAYER [CM]                                  76
      FDSK_RMINSENS=140.0*ETA2TAN(4.0)                                    76
      CFDSK_RMINSENS='MINIMUM RADIUS OF SENSITIVE LAYER [CM]'             76
C MAXIMUM RADIUS OF SENSITIVE LAYER [CM]                                  77
      FDSK_RMAXSENS=140.0*ETA2TAN(2.5)                                    77
      CFDSK_RMAXSENS='MAXIMUM RADIUS OF SENSITIVE LAYER [CM]'             77
C THICKNESS OF SENSITIVE LAYER [CM]                                       78
      FDSK_THKSENS=0.2                                                    78
      CFDSK_THKSENS='THICKNESS OF SENSITIVE LAYER [CM]'                   78
C THICKNESS OF SUPPORT MATERIAL [CM]                                      79
      FDSK_THKSUPP=0.8                                                    79
      CFDSK_THKSUPP='THICKNESS OF SUPPORT MATERIAL [CM]'                  79
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    80
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           80
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   80
C *                                                                       80
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDSK',    80
     *BPATH_FDSK,IDNUM_FDSK,1, MAP_FDSK,NAMES_FDSK,COMMEN_FDSK,           80
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   80
      ENDIF                                                               81
C use default FDSK bank                                                   82
      BPATH_FDSK='FDSK*'                                                  82
      IDNUM_FDSK(1)=0                                                     82
      CALL AGDGETP('FTSDGEO','*',-999,'FDSK',' ',BANK_FDSK, BPATH_FDSK,   82
     *IDNUM_FDSK,1,MAP_FDSK,NAMES_FDSK,BANK_FDSK,LEN_FDSK,LLEN_FDSK,      82
     *LINK_FDSK,FLAG_FDSK)                                                82
C use FDSK bank                                                           83
      BPATH_FDSK='FDSK*'                                                  83
      IDNUM_FDSK(1)=0                                                     83
      FDSK_DISKID=1                                                       83
      CALL AGDGETP('FTSDGEO','*',-999,'FDSK',NFDSK_DISKID,                83
     *RE_FDSK_DISKID,BPATH_FDSK,IDNUM_FDSK,1,MAP_FDSK,NAMES_FDSK,         83
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   83
      ZMIN = FDSK_Z - FDSK_THKSUPP                                        84
      RMIN = FDSK_RMIN                                                    85
C use FDSK bank                                                           86
      BPATH_FDSK='FDSK*'                                                  86
      IDNUM_FDSK(1)=0                                                     86
      FDSK_DISKID=NDISK                                                   86
      CALL AGDGETP('FTSDGEO','*',-999,'FDSK',NFDSK_DISKID,                86
     *RE_FDSK_DISKID,BPATH_FDSK,IDNUM_FDSK,1,MAP_FDSK,NAMES_FDSK,         86
     *BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)                   86
      ZMAX = FDSK_Z + FDSK_THKSUPP                                        87
      RMAX = FDSK_RMAX                                                    88
      XCENTER = 0                                                         89
      YCENTER = 0                                                         90
      ZCENTER = ZMIN + (ZMAX-ZMIN) / 2.0                                  91
      CALL AGSBEGM(MODULE,IPRIN)                                          92
C Check IPRIN<0                                                           92
      IF (IPRIN.LT.0) RETURN                                              92
C Create FTSM :                                                           92
C Call To Local Procedure FTSM                                            92
      ASSIGN 5020 TO BLOCK_FTSM                                           92
      GO TO  5010                                                         92
5020  CONTINUE                                                            92
C *                                                                       92
C Geant POSITION : FTSM ===> MOTHER='IDSM' X=XCENTER Y=YCENTER Z=ZCENTER  93
C  KONLY='MANY' <===                                                      93
      CALL AGSRESET                                                       93
      AG_MOTHER = 'IDSM'                                                  93
      AG_X = XCENTER                                                      93
      AG_Y = YCENTER                                                      93
      AG_Z = ZCENTER                                                      93
      AG_KONLY = 'MANY'                                                   93
      AG_TITLE = 'FTSM'                                                   93
      AG_EXNAME = 'POSITION'                                              93
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   93
      CALL AXPOSITION                                                     93
C *                                                                       93
C ----------------------------------------------------------------------  93
C ------------ FTSM                                                       93
      CALL AGSENDM                                                        95
      RETURN                                                              95
C Begin Local Procedure FTSM                                              95
C FTSM MOTHER VOLUME FOR THE FTS                                          95
5010  CONTINUE                                                            95
         AG_TITLE='FTSM'                                                  95
         AG_MODULE=MODULE                                                 95
         AG_MARK='V00'                                                    95
         CALL AGSPUSH                                                     95
         AG_MODULE=MODULE                                                 95
C    *                                                                    95
C    Geant MATERIAL : AIR ===> inherited arguments used <===              96
         CALL AGSRESET                                                    96
         AG_TITLE = 'AIR'                                                 96
         AG_EXNAME = 'MATERIAL'                                           96
         AG_PARLIST = 'NONE'                                              96
         CALL AXMATERIAL                                                  96
C    *                                                                    96
C    Geant MATERIAL : AIR ===> ISVOL=0 <===                               96
         CALL AGSRESET                                                    97
         AG_ISVOL = 0                                                     97
         AG_TITLE = 'AIR'                                                 97
         AG_EXNAME = 'MATERIAL'                                           97
         AG_PARLIST = 'ISVOL'                                             97
         CALL AXMATERIAL                                                  97
C    *                                                                    97
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           97
         CALL AGSRESET                                                    98
         AG_TITLE = 'STANDARD'                                            98
         AG_EXNAME = 'MEDIUM'                                             98
         AG_PARLIST = 'NONE'                                              98
         CALL AXMEDIUM                                                    98
C    *                                                                    98
C    Geant ATTRIBUTE : FSTM ===> SEEN=0 COLO=1 <===                       98
         CALL AGSRESET                                                    99
         AG_SEEN = 0                                                      99
         AG_COLO = 1                                                      99
         AG_TITLE = 'FSTM'                                                99
         AG_EXNAME = 'ATTRIBUTE'                                          99
         AG_PARLIST = 'SEEN_COLO'                                         99
         CALL AXATTRIBUTE                                                 99
C    *                                                                    99
C    Geant SHAPE : TUBE ===> RMIN=RMIN RMAX=RMAX DZ=(ZMAX-ZMIN)/2.0 <===  99
         CALL AGSRESET                                                    100
         AG_RMIN = RMIN                                                   100
         AG_RMAX = RMAX                                                   100
         AG_DZ = (ZMAX-ZMIN)/2.0                                          100
         AG_TITLE = 'TUBE'                                                100
         AG_EXNAME = 'SHAPE'                                              100
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      100
         CALL AXSHAPE                                                     100
C    *                                                                    100
C    *                                                                    100
         IDISK=1                                                          101
            GO TO 5033                                                    101
5031        IDISK=IDISK+1                                                 101
5033        IF(IDISK-(NDISK).GT.0) GO TO 5032                             101
C       use FDSK bank                                                     102
            BPATH_FDSK='FDSK*'                                            102
            IDNUM_FDSK(1)=0                                               102
            FDSK_DISKID=IDISK                                             102
            CALL AGDGETP('FTSDGEO','*',-999,'FDSK',NFDSK_DISKID,          102
     *      RE_FDSK_DISKID,BPATH_FDSK,IDNUM_FDSK,1,MAP_FDSK,NAMES_FDSK,   102
     *      BANK_FDSK,LEN_FDSK,LLEN_FDSK,LINK_FDSK,FLAG_FDSK)             102
C       Create FTSD :                                                     103
C       Call To Local Procedure FTSD                                      103
            ASSIGN 5050 TO BLOCK_FTSD                                     103
            GO TO  5040                                                   103
5050        CONTINUE                                                      103
C       *                                                                 103
C       Geant POSITION : FTSD ===> MOTHER='FTSM' X=FDSK_X Y=FDSK_Y Z=FDS  104
C       K_Z-ZCENTER <===                                                  104
            CALL AGSRESET                                                 104
            AG_MOTHER = 'FTSM'                                            104
            AG_X = FDSK_X                                                 104
            AG_Y = FDSK_Y                                                 104
            AG_Z = FDSK_Z-ZCENTER                                         104
            AG_TITLE = 'FTSD'                                             104
            AG_EXNAME = 'POSITION'                                        104
            AG_PARLIST = 'MOTHER_X_Y_Z'                                   104
            CALL AXPOSITION                                               104
C       *                                                                 104
         GO TO 5031                                                       105
5032     CONTINUE                                                         105
5011  CALL AGSPOPB                                                        106
      GO TO BLOCK_FTSM, (5010 ,5020)                                      106
C End of Local Procedure                                                  106
C ----------------------------------------------------------------------  107
C ------------ FTSD                                                       107
C Begin Local Procedure FTSD                                              108
C FTSD FTS DISK                                                           108
5040  CONTINUE                                                            108
         AG_TITLE='FTSD'                                                  108
         AG_MODULE=MODULE                                                 108
         AG_MARK='V00'                                                    108
         CALL AGSPUSH                                                     108
         AG_MODULE=MODULE                                                 108
C    *                                                                    108
C    Geant ATTRIBUTE : FTSD ===> SEEN=0 COLO=3 <===                       109
         CALL AGSRESET                                                    109
         AG_SEEN = 0                                                      109
         AG_COLO = 3                                                      109
         AG_TITLE = 'FTSD'                                                109
         AG_EXNAME = 'ATTRIBUTE'                                          109
         AG_PARLIST = 'SEEN_COLO'                                         109
         CALL AXATTRIBUTE                                                 109
C    *                                                                    109
C    Geant SHAPE : TUBE ===> RMIN=FDSK_RMIN RMAX=FDSK_RMAX, DZ=FDSK_THKS  109
C    UPP <===                                                             109
         CALL AGSRESET                                                    111
         AG_RMIN = FDSK_RMIN                                              111
         AG_RMAX = FDSK_RMAX                                              111
         AG_DZ = FDSK_THKSUPP                                             111
         AG_TITLE = 'TUBE'                                                111
         AG_EXNAME = 'SHAPE'                                              111
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      111
         CALL AXSHAPE                                                     111
C    *                                                                    111
C    Create FTSA :                                                        112
C    Call To Local Procedure FTSA                                         112
         ASSIGN 5070 TO BLOCK_FTSA                                        112
         GO TO  5060                                                      112
5070     CONTINUE                                                         112
C    *                                                                    112
C    Geant POSITION : FTSA ===> RMAX=FDSK_RMAXSENS RMIN=FDSK_RMINSENS DZ  113
C    =FDSK_THKSENS <===                                                   113
         CALL AGSRESET                                                    113
         AG_RMAX = FDSK_RMAXSENS                                          113
         AG_RMIN = FDSK_RMINSENS                                          113
         AG_DZ = FDSK_THKSENS                                             113
         AG_TITLE = 'FTSA'                                                113
         AG_EXNAME = 'POSITION'                                           113
         AG_PARLIST = 'RMAX_RMIN_DZ'                                      113
         CALL AXPOSITION                                                  113
C    *                                                                    113
C    Create FTSS :                                                        114
C    Call To Local Procedure FTSS                                         114
         ASSIGN 5090 TO BLOCK_FTSS                                        114
         GO TO  5080                                                      114
5090     CONTINUE                                                         114
C    *                                                                    114
C    Geant POSITION : FTSS ===> RMAX=FDSK_RMINSENS RMIN=FDSK_RMIN DZ=FDS  115
C    K_THKSUPP <===                                                       115
         CALL AGSRESET                                                    115
         AG_RMAX = FDSK_RMINSENS                                          115
         AG_RMIN = FDSK_RMIN                                              115
         AG_DZ = FDSK_THKSUPP                                             115
         AG_TITLE = 'FTSS'                                                115
         AG_EXNAME = 'POSITION'                                           115
         AG_PARLIST = 'RMAX_RMIN_DZ'                                      115
         CALL AXPOSITION                                                  115
C    *                                                                    115
C    Geant POSITION : FTSS ===> RMAX=FDSK_RMAX RMIN=FDSK_RMAXSENS DZ=FDS  115
C    K_THKSUPP <===                                                       115
         CALL AGSRESET                                                    116
         AG_RMAX = FDSK_RMAX                                              116
         AG_RMIN = FDSK_RMAXSENS                                          116
         AG_DZ = FDSK_THKSUPP                                             116
         AG_TITLE = 'FTSS'                                                116
         AG_EXNAME = 'POSITION'                                           116
         AG_PARLIST = 'RMAX_RMIN_DZ'                                      116
         CALL AXPOSITION                                                  116
C    *                                                                    116
5041  CALL AGSPOPB                                                        117
      GO TO BLOCK_FTSD, (5040 ,5050)                                      117
C End of Local Procedure                                                  117
C ----------------------------------------------------------------------  118
C ------------ FTSA                                                       118
C Begin Local Procedure FTSA                                              119
C FTSA ACTIVE VOLUME OF THE FTS                                           119
5060  CONTINUE                                                            119
         AG_TITLE='FTSA'                                                  119
         AG_MODULE=MODULE                                                 119
         AG_MARK='V00'                                                    119
         CALL AGSPUSH                                                     119
         AG_MODULE=MODULE                                                 119
C    *                                                                    119
C    Geant ATTRIBUTE : FTSA ===> SEEN=1 COLO=4 <===                       120
         CALL AGSRESET                                                    120
         AG_SEEN = 1                                                      120
         AG_COLO = 4                                                      120
         AG_TITLE = 'FTSA'                                                120
         AG_EXNAME = 'ATTRIBUTE'                                          120
         AG_PARLIST = 'SEEN_COLO'                                         120
         CALL AXATTRIBUTE                                                 120
C    *                                                                    120
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          120
         CALL AGSRESET                                                    121
         AG_TITLE = 'SILICON'                                             121
         AG_EXNAME = 'MATERIAL'                                           121
         AG_PARLIST = 'NONE'                                              121
         CALL AXMATERIAL                                                  121
C    *                                                                    121
C    Geant MATERIAL : SILICON ===> ISVOL=1 <===                           121
         CALL AGSRESET                                                    122
         AG_ISVOL = 1                                                     122
         AG_TITLE = 'SILICON'                                             122
         AG_EXNAME = 'MATERIAL'                                           122
         AG_PARLIST = 'ISVOL'                                             122
         CALL AXMATERIAL                                                  122
C    *                                                                    122
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      122
         CALL AGSRESET                                                    123
         AG_RMIN = 0                                                      123
         AG_RMAX = 0                                                      123
         AG_DZ = 0                                                        123
         AG_TITLE = 'TUBE'                                                123
         AG_EXNAME = 'SHAPE'                                              123
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      123
         CALL AXSHAPE                                                     123
C    *                                                                    123
C    Defining Hits: Z:.00001:S Y:.00001:S X:.00001:S PTOT:16:(0,100) CX:  123
C    10: CY:10: CZ:10:, SLENG:16:(0,500) TOF:16: STEP:.0001: ELOSS:16:    123
         AG_TITLE = 'FTSA'                                                125
         AG_EXNAME = 'HITS'                                               125
         AG_TYPE=AG_MAGIC                                                 125
         CALL AGSDETV('FTSA')                                             125
C    *                                                                    125
         AG_PARLIST='Z'                                                   125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = AG_MAGIC                                              125
         AG_BIN=.00001                                                    125
         AG_OPTION='S'                                                    125
         AG_FHMIN = AG_MAGIC                                              125
         AG_FHMAX = AG_MAGIC                                              125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='Y'                                                   125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = AG_MAGIC                                              125
         AG_BIN=.00001                                                    125
         AG_OPTION='S'                                                    125
         AG_FHMIN = AG_MAGIC                                              125
         AG_FHMAX = AG_MAGIC                                              125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='X'                                                   125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = AG_MAGIC                                              125
         AG_BIN=.00001                                                    125
         AG_OPTION='S'                                                    125
         AG_FHMIN = AG_MAGIC                                              125
         AG_FHMAX = AG_MAGIC                                              125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='PTOT'                                                125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 16                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_OPTION='*'                                                    125
         AG_FHMIN = 0                                                     125
         AG_FHMAX = 100                                                   125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='CX'                                                  125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 10                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_FHMIN=AG_MAGIC                                                125
         AG_FHMAX=AG_MAGIC                                                125
         AG_OPTION='*'                                                    125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='CY'                                                  125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 10                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_FHMIN=AG_MAGIC                                                125
         AG_FHMAX=AG_MAGIC                                                125
         AG_OPTION='*'                                                    125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='CZ'                                                  125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 10                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_OPTION='*'                                                    125
         AG_FHMIN = AG_MAGIC                                              125
         AG_FHMAX = AG_MAGIC                                              125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='SLENG'                                               125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 16                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_OPTION='*'                                                    125
         AG_FHMIN = 0                                                     125
         AG_FHMAX = 500                                                   125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='TOF'                                                 125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 16                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_FHMIN=AG_MAGIC                                                125
         AG_FHMAX=AG_MAGIC                                                125
         AG_OPTION='*'                                                    125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='STEP'                                                125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = AG_MAGIC                                              125
         AG_BIN=.0001                                                     125
         AG_FHMIN=AG_MAGIC                                                125
         AG_FHMAX=AG_MAGIC                                                125
         AG_OPTION='*'                                                    125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         AG_PARLIST='ELOSS'                                               125
         AG_TITLE = 'DETH'                                                125
         AG_NBITS = 16                                                    125
         AG_BIN=AG_MAGIC                                                  125
         AG_FHMIN=AG_MAGIC                                                125
         AG_FHMAX=AG_MAGIC                                                125
         AG_OPTION='*'                                                    125
         CALL AGSDETH (0)                                                 125
C    *                                                                    125
         CALL AGSDETU('FTSA')                                             125
C    *                                                                    125
5061  CALL AGSPOPB                                                        126
      GO TO BLOCK_FTSA, (5060 ,5070)                                      126
C End of Local Procedure                                                  126
C ----------------------------------------------------------------------  127
C ------------ FTSS                                                       127
C Begin Local Procedure FTSS                                              128
C FTSS SUPPORT STRUCTURE FOR EACH DISK                                    128
5080  CONTINUE                                                            128
         AG_TITLE='FTSS'                                                  128
         AG_MODULE=MODULE                                                 128
         AG_MARK='V00'                                                    128
         CALL AGSPUSH                                                     128
         AG_MODULE=MODULE                                                 128
C    *                                                                    128
C    Geant ATTRIBUTE : FTSS ===> SEEN=1 COLO=3 <===                       129
         CALL AGSRESET                                                    129
         AG_SEEN = 1                                                      129
         AG_COLO = 3                                                      129
         AG_TITLE = 'FTSS'                                                129
         AG_EXNAME = 'ATTRIBUTE'                                          129
         AG_PARLIST = 'SEEN_COLO'                                         129
         CALL AXATTRIBUTE                                                 129
C    *                                                                    129
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        129
         CALL AGSRESET                                                    130
         AG_TITLE = 'ALUMINIUM'                                           130
         AG_EXNAME = 'MATERIAL'                                           130
         AG_PARLIST = 'NONE'                                              130
         CALL AXMATERIAL                                                  130
C    *                                                                    130
C    Geant MATERIAL : ALUMINIUM ===> ISVOL=0 <===                         130
         CALL AGSRESET                                                    131
         AG_ISVOL = 0                                                     131
         AG_TITLE = 'ALUMINIUM'                                           131
         AG_EXNAME = 'MATERIAL'                                           131
         AG_PARLIST = 'ISVOL'                                             131
         CALL AXMATERIAL                                                  131
C    *                                                                    131
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      131
         CALL AGSRESET                                                    132
         AG_RMIN = 0                                                      132
         AG_RMAX = 0                                                      132
         AG_DZ = 0                                                        132
         AG_TITLE = 'TUBE'                                                132
         AG_EXNAME = 'SHAPE'                                              132
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      132
         CALL AXSHAPE                                                     132
C    *                                                                    132
5081  CALL AGSPOPB                                                        133
      GO TO BLOCK_FTSS, (5080 ,5090)                                      133
C End of Local Procedure                                                  133
      WRITE(LOUT,5100)'===>>  [End AgML Module FtsdGeo]  <<==='           133
5100  FORMAT(A32,/,/)                                                     133
C MODULE FTSDGEO                                                          135
      END                                                                 135
