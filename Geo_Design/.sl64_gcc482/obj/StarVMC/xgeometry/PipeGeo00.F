C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIPEGEO00                                                1
C PIPEGEO00 IS THE SIMPLIFIED GEOMETRY OF THE STAR BEAM PIPE.             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIPEGEO00'/, BANK_TITLE/'*'/                           1
      DATA MCOMMENT/'IS THE SIMPLIFIED GEOMETRY OF THE STAR BEAM PIPE.'/  1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 03/17/08                                                        1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'03/17/08'/                                   2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR GERRIT VAN NIEUWENHUIZEN                                         2
      CHARACTER*40 AUTHOR /'GERRIT VAN NIEUWENHUIZEN'/                    3
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
      INTEGER BLOCK_PIPE                                                  5
      INTEGER BLOCK_PIPC                                                  6
      INTEGER BLOCK_PVAC                                                  6
C -                                                                       7
      REAL PIPV_VERSION                                                   7
      EQUIVALENCE (BANK_PIPV(3),PIPV_VERSION,RE_PIPV_VERSION),            7
     *(NAMES_PIPV(1),NPIPV_VERSION)                                       7
      EQUIVALENCE (COMMEN_PIPV(1),CPIPV_VERSION),(MAP_PIPV(1,1),          7
     *MPIPV_VERSION)                                                      7
      CHARACTER CPIPV_VERSION*40/' '/                                     7
      CHARACTER NPIPV_VERSION*12/'R.VERSION'/                             7
      INTEGER MPIPV_VERSION(2)/1,1/                                       7
      REAL RE_PIPV_VERSION                                                7
C -                                                                       7
      REAL PIPV_PIPECONFIG                                                7
      EQUIVALENCE (BANK_PIPV(4),PIPV_PIPECONFIG,RE_PIPV_PIPECONFIG),      7
     *(NAMES_PIPV(2),NPIPV_PIPECONFIG)                                    7
      EQUIVALENCE (COMMEN_PIPV(2),CPIPV_PIPECONFIG),(MAP_PIPV(1,2),       7
     *MPIPV_PIPECONFIG)                                                   7
      CHARACTER CPIPV_PIPECONFIG*40/' '/                                  7
      CHARACTER NPIPV_PIPECONFIG*12/'R.PIPECONFIG'/                       7
      INTEGER MPIPV_PIPECONFIG(2)/1,1/                                    7
      REAL RE_PIPV_PIPECONFIG                                             7
C -                                                                       7
      INTEGER LEN_PIPV,LLEN_PIPV,LINK_PIPV                                7
      PARAMETER (LEN_PIPV=4)                                              7
      PARAMETER (LLEN_PIPV=2)                                             7
      INTEGER FLAG_PIPV/0/,MAP_PIPV(2,LLEN_PIPV),IDNUM_PIPV(10)           7
      CHARACTER NAMES_PIPV(LLEN_PIPV)*12,COMMEN_PIPV(LLEN_PIPV)*40        7
      DATA IDNUM_PIPV/10*0/                                               7
      REAL BANK_PIPV(LEN_PIPV)                                            7
      CHARACTER*60 BPATH_PIPV/' '/                                        7
      SAVE FLAG_PIPV,MAP_PIPV,IDNUM_PIPV,NAMES_PIPV,COMMEN_PIPV,          7
     *BPATH_PIPV                                                          7
      DATA LINK_PIPV/0/,BANK_PIPV/LEN_PIPV*0/                             7
C *                                                                       7
C -                                                                       8
      REAL PIPG_CONFIG                                                    8
      EQUIVALENCE (BANK_PIPG(3),PIPG_CONFIG,RE_PIPG_CONFIG),              8
     *(NAMES_PIPG(1),NPIPG_CONFIG)                                        8
      EQUIVALENCE (COMMEN_PIPG(1),CPIPG_CONFIG),(MAP_PIPG(1,1),           8
     *MPIPG_CONFIG)                                                       8
      CHARACTER CPIPG_CONFIG*40/' '/                                      8
      CHARACTER NPIPG_CONFIG*12/'R.CONFIG'/                               8
      INTEGER MPIPG_CONFIG(2)/1,1/                                        8
      REAL RE_PIPG_CONFIG                                                 8
C -                                                                       8
      REAL PIPG_BEINNR                                                    8
      EQUIVALENCE (BANK_PIPG(4),PIPG_BEINNR,RE_PIPG_BEINNR),              8
     *(NAMES_PIPG(2),NPIPG_BEINNR)                                        8
      EQUIVALENCE (COMMEN_PIPG(2),CPIPG_BEINNR),(MAP_PIPG(1,2),           8
     *MPIPG_BEINNR)                                                       8
      CHARACTER CPIPG_BEINNR*40/' '/                                      8
      CHARACTER NPIPG_BEINNR*12/'R.BEINNR'/                               8
      INTEGER MPIPG_BEINNR(2)/1,1/                                        8
      REAL RE_PIPG_BEINNR                                                 8
C -                                                                       8
      REAL PIPG_BEOUTR                                                    8
      EQUIVALENCE (BANK_PIPG(5),PIPG_BEOUTR,RE_PIPG_BEOUTR),              8
     *(NAMES_PIPG(3),NPIPG_BEOUTR)                                        8
      EQUIVALENCE (COMMEN_PIPG(3),CPIPG_BEOUTR),(MAP_PIPG(1,3),           8
     *MPIPG_BEOUTR)                                                       8
      CHARACTER CPIPG_BEOUTR*40/' '/                                      8
      CHARACTER NPIPG_BEOUTR*12/'R.BEOUTR'/                               8
      INTEGER MPIPG_BEOUTR(2)/1,1/                                        8
      REAL RE_PIPG_BEOUTR                                                 8
C -                                                                       8
      REAL PIPG_BELENG                                                    8
      EQUIVALENCE (BANK_PIPG(6),PIPG_BELENG,RE_PIPG_BELENG),              8
     *(NAMES_PIPG(4),NPIPG_BELENG)                                        8
      EQUIVALENCE (COMMEN_PIPG(4),CPIPG_BELENG),(MAP_PIPG(1,4),           8
     *MPIPG_BELENG)                                                       8
      CHARACTER CPIPG_BELENG*40/' '/                                      8
      CHARACTER NPIPG_BELENG*12/'R.BELENG'/                               8
      INTEGER MPIPG_BELENG(2)/1,1/                                        8
      REAL RE_PIPG_BELENG                                                 8
C -                                                                       8
      INTEGER LEN_PIPG,LLEN_PIPG,LINK_PIPG                                8
      PARAMETER (LEN_PIPG=6)                                              8
      PARAMETER (LLEN_PIPG=4)                                             8
      INTEGER FLAG_PIPG/0/,MAP_PIPG(2,LLEN_PIPG),IDNUM_PIPG(10)           8
      CHARACTER NAMES_PIPG(LLEN_PIPG)*12,COMMEN_PIPG(LLEN_PIPG)*40        8
      DATA IDNUM_PIPG/10*0/                                               8
      REAL BANK_PIPG(LEN_PIPG)                                            8
      CHARACTER*60 BPATH_PIPG/' '/                                        8
      SAVE FLAG_PIPG,MAP_PIPG,IDNUM_PIPG,NAMES_PIPG,COMMEN_PIPG,          8
     *BPATH_PIPG                                                          8
      DATA LINK_PIPG/0/,BANK_PIPG/LEN_PIPG*0/                             8
C *                                                                       8
      REAL VACUUM                                                         9
C *                                                                       11
      CALL AGSBEGM(MODULE,IPRIN)                                          11
      IF (IPRIN.LT.0) RETURN                                              11
C Filling bank PIPV - BEAM PIPE VERSION                                   11
      BANK_TITLE='BEAM PIPE VERSION'                                      11
      BPATH_PIPV='PIPV*'                                                  11
      IDNUM_PIPV(1)=0                                                     12
C GEOMETRY VERSION                                                        12
      PIPV_VERSION=1                                                      12
      CPIPV_VERSION='GEOMETRY VERSION'                                    12
C PIPE VERSION                                                            13
      PIPV_PIPECONFIG=0                                                   13
      CPIPV_PIPECONFIG='PIPE VERSION'                                     13
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    14
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           14
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   14
C *                                                                       14
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    14
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           14
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   14
C *                                                                       15
      CALL AGSBEGM(MODULE,IPRIN)                                          15
      IF (IPRIN.LT.0) RETURN                                              15
C Filling bank PIPG - BEAM PIPE DATA 02/13/2009: THICKNESS IS 30 MILL =   15
C 762UM                                                                   15
      BANK_TITLE='BEAM PIPE DATA 02/13/2009: THICKNESS IS 30 MILL = 762U  15
     *M'                                                                  15
      BPATH_PIPG='PIPG*'                                                  15
      IDNUM_PIPG(1)=0                                                     16
C BOTH MATERIAL AND GEO PARAMS                                            16
      PIPG_CONFIG=0                                                       16
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         16
C BERILLIUM SECTION INNER RADIUS                                          17
      PIPG_BEINNR=1.9619                                                  17
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       17
C BERILLIUM SECTION OUTER RADIUS                                          18
      PIPG_BEOUTR=2.0381                                                  18
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       18
C BERILLIUM SECTION LENGTH                                                19
      PIPG_BELENG=200.0                                                   19
      CPIPG_BELENG='BERILLIUM SECTION LENGTH'                             19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    20
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           20
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    20
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           20
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   20
      VACUUM = 1.0E-5                                                     21
C use default PIPV bank                                                   22
      BPATH_PIPV='PIPV*'                                                  22
      IDNUM_PIPV(1)=0                                                     22
      CALL AGDGETP('PIPEGEO00','*',-999,'PIPV',' ',BANK_PIPV,             22
     *BPATH_PIPV,IDNUM_PIPV,1,MAP_PIPV,NAMES_PIPV,BANK_PIPV,LEN_PIPV,     22
     *LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                                      22
C use PIPG bank                                                           23
      BPATH_PIPG='PIPG*'                                                  23
      IDNUM_PIPG(1)=0                                                     23
      PIPG_CONFIG=PIPV_PIPECONFIG                                         23
      CALL AGDGETP('PIPEGEO00','*',-999,'PIPG',NPIPG_CONFIG,              23
     *RE_PIPG_CONFIG,BPATH_PIPG,IDNUM_PIPG,1,MAP_PIPG,NAMES_PIPG,         23
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   23
      CALL AGSBEGM(MODULE,IPRIN)                                          24
C Check IPRIN<0                                                           24
      IF (IPRIN.LT.0) RETURN                                              24
C Create PIPE :                                                           24
C Call To Local Procedure PIPE                                            24
      ASSIGN 5020 TO BLOCK_PIPE                                           24
      GO TO  5010                                                         24
5020  CONTINUE                                                            24
C *                                                                       24
C Geant POSITION : PIPE ===> MOTHER='CAVE' <===                           25
      CALL AGSRESET                                                       25
      AG_MOTHER = 'CAVE'                                                  25
      AG_TITLE = 'PIPE'                                                   25
      AG_EXNAME = 'POSITION'                                              25
      AG_PARLIST = 'MOTHER'                                               25
      CALL AXPOSITION                                                     25
C *                                                                       25
C ----------------------------------------------------------------------  25
C ------------ PIPE                                                       25
      CALL AGSENDM                                                        27
      RETURN                                                              27
C Begin Local Procedure PIPE                                              27
C PIPE IS THE STAR BEAM PIPE MOTHER VOLUME                                27
5010  CONTINUE                                                            27
         AG_TITLE='PIPE'                                                  27
         AG_MODULE=MODULE                                                 27
         AG_MARK='V00'                                                    27
         CALL AGSPUSH                                                     27
         AG_MODULE=MODULE                                                 27
C    *                                                                    27
C    Geant MATERIAL : AIR ===> inherited arguments used <===              28
         CALL AGSRESET                                                    28
         AG_TITLE = 'AIR'                                                 28
         AG_EXNAME = 'MATERIAL'                                           28
         AG_PARLIST = 'NONE'                                              28
         CALL AXMATERIAL                                                  28
C    *                                                                    28
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           28
         CALL AGSRESET                                                    29
         AG_TITLE = 'STANDARD'                                            29
         AG_EXNAME = 'MEDIUM'                                             29
         AG_PARLIST = 'NONE'                                              29
         CALL AXMEDIUM                                                    29
C    *                                                                    29
C    Geant ATTRIBUTE : PIPE ===> SEEN=0 COLO=1 <===                       29
         CALL AGSRESET                                                    30
         AG_SEEN = 0                                                      30
         AG_COLO = 1                                                      30
         AG_TITLE = 'PIPE'                                                30
         AG_EXNAME = 'ATTRIBUTE'                                          30
         AG_PARLIST = 'SEEN_COLO'                                         30
         CALL AXATTRIBUTE                                                 30
C    *                                                                    30
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_BEOUTR DZ=PIPG_BELENG/2 <=  30
C    ==                                                                   30
         CALL AGSRESET                                                    31
         AG_RMIN = 0                                                      31
         AG_RMAX = PIPG_BEOUTR                                            31
         AG_DZ = PIPG_BELENG/2                                            31
         AG_TITLE = 'TUBE'                                                31
         AG_EXNAME = 'SHAPE'                                              31
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      31
         CALL AXSHAPE                                                     31
C    *                                                                    31
C    Create PIPC :                                                        32
C    Call To Local Procedure PIPC                                         32
         ASSIGN 5040 TO BLOCK_PIPC                                        32
         GO TO  5030                                                      32
5040     CONTINUE                                                         32
C    *                                                                    32
C    Geant POSITION : PIPC ===> inherited arguments used <===             33
         CALL AGSRESET                                                    33
         AG_TITLE = 'PIPC'                                                33
         AG_EXNAME = 'POSITION'                                           33
         AG_PARLIST = 'NONE'                                              33
         CALL AXPOSITION                                                  33
C    *                                                                    33
C    Create PVAC :                                                        34
C    Call To Local Procedure PVAC                                         34
         ASSIGN 5060 TO BLOCK_PVAC                                        34
         GO TO  5050                                                      34
5060     CONTINUE                                                         34
C    *                                                                    34
C    Geant POSITION : PVAC ===> inherited arguments used <===             35
         CALL AGSRESET                                                    35
         AG_TITLE = 'PVAC'                                                35
         AG_EXNAME = 'POSITION'                                           35
         AG_PARLIST = 'NONE'                                              35
         CALL AXPOSITION                                                  35
C    *                                                                    35
5011  CALL AGSPOPB                                                        36
      GO TO BLOCK_PIPE, (5010 ,5020)                                      36
C End of Local Procedure                                                  36
C ----------------------------------------------------------------------  37
C ------------ PIPC                                                       37
C Begin Local Procedure PIPC                                              38
C PIPC IS THE CENTRAL BEAM PIPE VOLUME                                    38
5030  CONTINUE                                                            38
         AG_TITLE='PIPC'                                                  38
         AG_MODULE=MODULE                                                 38
         AG_MARK='V00'                                                    38
         CALL AGSPUSH                                                     38
         AG_MODULE=MODULE                                                 38
C    *                                                                    38
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        39
         CALL AGSRESET                                                    39
         AG_TITLE = 'BERILLIUM'                                           39
         AG_EXNAME = 'MATERIAL'                                           39
         AG_PARLIST = 'NONE'                                              39
         CALL AXMATERIAL                                                  39
C    *                                                                    39
C    Geant ATTRIBUTE : PIPC ===> SEEN=1 COLO=6 <===                       39
         CALL AGSRESET                                                    40
         AG_SEEN = 1                                                      40
         AG_COLO = 6                                                      40
         AG_TITLE = 'PIPC'                                                40
         AG_EXNAME = 'ATTRIBUTE'                                          40
         AG_PARLIST = 'SEEN_COLO'                                         40
         CALL AXATTRIBUTE                                                 40
C    *                                                                    40
C    Geant SHAPE : TUBE ===> RMIN=PIPG_BEINNR RMAX=PIPG_BEOUTR, DZ=PIPG_  40
C    BELENG/2 <===                                                        40
         CALL AGSRESET                                                    42
         AG_RMIN = PIPG_BEINNR                                            42
         AG_RMAX = PIPG_BEOUTR                                            42
         AG_DZ = PIPG_BELENG/2                                            42
         AG_TITLE = 'TUBE'                                                42
         AG_EXNAME = 'SHAPE'                                              42
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      42
         CALL AXSHAPE                                                     42
C    *                                                                    42
5031  CALL AGSPOPB                                                        43
      GO TO BLOCK_PIPC, (5030 ,5040)                                      43
C End of Local Procedure                                                  43
C ----------------------------------------------------------------------  44
C ------------ PVAC                                                       44
C Begin Local Procedure PVAC                                              45
C PVAC IS THE VACUUM VOLUME OF BE SECTION OF PIPE                         45
5050  CONTINUE                                                            45
         AG_TITLE='PVAC'                                                  45
         AG_MODULE=MODULE                                                 45
         AG_MARK='V00'                                                    45
         CALL AGSPUSH                                                     45
         AG_MODULE=MODULE                                                 45
C    *                                                                    45
C    Geant MATERIAL : AIR ===> inherited arguments used <===              46
         CALL AGSRESET                                                    46
         AG_TITLE = 'AIR'                                                 46
         AG_EXNAME = 'MATERIAL'                                           46
         AG_PARLIST = 'NONE'                                              46
         CALL AXMATERIAL                                                  46
C    *                                                                    46
C    Geant ATTRIBUTE : PIPC ===> SEEN=1 COLO=5 <===                       46
         CALL AGSRESET                                                    47
         AG_SEEN = 1                                                      47
         AG_COLO = 5                                                      47
         AG_TITLE = 'PIPC'                                                47
         AG_EXNAME = 'ATTRIBUTE'                                          47
         AG_PARLIST = 'SEEN_COLO'                                         47
         CALL AXATTRIBUTE                                                 47
C    *                                                                    47
C    Geant MATERIAL : PVACUUM ===> DENS=%DENS*VACUUM ABSL=%ABSL/VACUUM R  47
C    ADL=%RADL/VACUUM <===                                                47
         CALL AGSRESET                                                    48
         AG_DENS = AG_DENS*VACUUM                                         48
         AG_ABSL = AG_ABSL/VACUUM                                         48
         AG_RADL = AG_RADL/VACUUM                                         48
         AG_TITLE = 'PVACUUM'                                             48
         AG_EXNAME = 'MATERIAL'                                           48
         AG_PARLIST = 'DENS_ABSL_RADL'                                    48
         CALL AXMATERIAL                                                  48
C    *                                                                    48
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_BEINNR DZ=PIPG_BELENG/2.0   48
C    <===                                                                 48
         CALL AGSRESET                                                    49
         AG_RMIN = 0                                                      49
         AG_RMAX = PIPG_BEINNR                                            49
         AG_DZ = PIPG_BELENG/2.0                                          49
         AG_TITLE = 'TUBE'                                                49
         AG_EXNAME = 'SHAPE'                                              49
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      49
         CALL AXSHAPE                                                     49
C    *                                                                    49
5051  CALL AGSPOPB                                                        50
      GO TO BLOCK_PVAC, (5050 ,5060)                                      50
C End of Local Procedure                                                  50
      WRITE(LOUT,5070)'===>>  [End AgML Module PIPEGEO00]  <<==='         50
5070  FORMAT(A32,/,/)                                                     50
C MODULE PIPEGEO00                                                        52
      END                                                                 52
