C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TUTRGEO4                                                 1
C TUTRGEO4 TUTORIAL GEOMETRY 1                                            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TUTRGEO4'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'TUTORIAL GEOMETRY 1'/                                1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR A.N. AUTHOR                                                      1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'A.N. AUTHOR'/                                 2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED TODAY                                                           2
      CHARACTER*40 CREATED /'TODAY'/                                      3
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            4
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      4
      COMMON/GCMAIL/ CHMAIL                                               4
      CHARACTER*132 CHMAIL                                                4
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   4
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        4
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       4
      INTEGER BLOCK_TUTR                                                  4
      INTEGER BLOCK_ABOX                                                  5
      INTEGER BLOCK_ATUB                                                  5
      INTEGER BLOCK_ATBS                                                  5
      INTEGER BLOCK_ACON                                                  5
      INTEGER BLOCK_ACNS                                                  5
      INTEGER BLOCK_APCN                                                  5
      INTEGER BLOCK_APGN                                                  5
      INTEGER BLOCK_APAR                                                  5
      INTEGER BLOCK_AELT                                                  5
      CALL AGSBEGM(MODULE,IPRIN)                                          6
C Check IPRIN<0                                                           6
      IF (IPRIN.LT.0) RETURN                                              6
C Create TUTR :                                                           6
C Call To Local Procedure TUTR                                            6
      ASSIGN 5020 TO BLOCK_TUTR                                           6
      GO TO  5010                                                         6
5020  CONTINUE                                                            6
C *                                                                       6
C Geant POSITION : TUTR ===> MOTHER='CAVE' <===                           7
      CALL AGSRESET                                                       7
      AG_MOTHER = 'CAVE'                                                  7
      AG_TITLE = 'TUTR'                                                   7
      AG_EXNAME = 'POSITION'                                              7
      AG_PARLIST = 'MOTHER'                                               7
      CALL AXPOSITION                                                     7
C *                                                                       7
C ----------------------------------------------------------------------  7
C ------------ TUTR                                                       7
      CALL AGSENDM                                                        9
      RETURN                                                              9
C Begin Local Procedure TUTR                                              9
C TUTR MAIN VOLUME IN THE AGML TUTORIAL GEOMETRY                          9
5010  CONTINUE                                                            9
         AG_TITLE='TUTR'                                                  9
         AG_MODULE=MODULE                                                 9
         AG_MARK='V00'                                                    9
         CALL AGSPUSH                                                     9
         AG_MODULE=MODULE                                                 9
C    *                                                                    9
C    Geant MATERIAL : AIR ===> inherited arguments used <===              10
         CALL AGSRESET                                                    10
         AG_TITLE = 'AIR'                                                 10
         AG_EXNAME = 'MATERIAL'                                           10
         AG_PARLIST = 'NONE'                                              10
         CALL AXMATERIAL                                                  10
C    *                                                                    10
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           10
         CALL AGSRESET                                                    11
         AG_TITLE = 'STANDARD'                                            11
         AG_EXNAME = 'MEDIUM'                                             11
         AG_PARLIST = 'NONE'                                              11
         CALL AXMEDIUM                                                    11
C    *                                                                    11
C    Geant ATTRIBUTE : NONE ===> SEEN=0 <===                              11
         CALL AGSRESET                                                    12
         AG_SEEN = 0                                                      12
         AG_TITLE = 'NONE'                                                12
         AG_EXNAME = 'ATTRIBUTE'                                          12
         AG_PARLIST = 'SEEN'                                              12
         CALL AXATTRIBUTE                                                 12
C    *                                                                    12
C    Geant SHAPE : BOX ===> DX=500.0 DY=500.0 DZ=500.0 <===               12
         CALL AGSRESET                                                    13
         AG_DX = 500.0                                                    13
         AG_DY = 500.0                                                    13
         AG_DZ = 500.0                                                    13
         AG_TITLE = 'BOX'                                                 13
         AG_EXNAME = 'SHAPE'                                              13
         AG_PARLIST = 'DX_DY_DZ'                                          13
         CALL AXSHAPE                                                     13
C    *                                                                    13
C    Create ABOX :                                                        14
C    Call To Local Procedure ABOX                                         14
         ASSIGN 5040 TO BLOCK_ABOX                                        14
         GO TO  5030                                                      14
5040     CONTINUE                                                         14
C    *                                                                    14
C    Geant POSITION : ABOX ===> inherited arguments used <===             15
         CALL AGSRESET                                                    15
         AG_TITLE = 'ABOX'                                                15
         AG_EXNAME = 'POSITION'                                           15
         AG_PARLIST = 'NONE'                                              15
         CALL AXPOSITION                                                  15
C    *                                                                    15
C    Create ATUB :                                                        16
C    Call To Local Procedure ATUB                                         16
         ASSIGN 5060 TO BLOCK_ATUB                                        16
         GO TO  5050                                                      16
5060     CONTINUE                                                         16
C    *                                                                    16
C    Geant POSITION : ATUB ===> X=+25.0 <===                              17
         CALL AGSRESET                                                    17
         AG_X = +25.0                                                     17
         AG_TITLE = 'ATUB'                                                17
         AG_EXNAME = 'POSITION'                                           17
         AG_PARLIST = 'X'                                                 17
         CALL AXPOSITION                                                  17
C    *                                                                    17
C    Create ATBS :                                                        18
C    Call To Local Procedure ATBS                                         18
         ASSIGN 5080 TO BLOCK_ATBS                                        18
         GO TO  5070                                                      18
5080     CONTINUE                                                         18
C    *                                                                    18
C    Geant POSITION : ATBS ===> X=+50.0 <===                              19
         CALL AGSRESET                                                    19
         AG_X = +50.0                                                     19
         AG_TITLE = 'ATBS'                                                19
         AG_EXNAME = 'POSITION'                                           19
         AG_PARLIST = 'X'                                                 19
         CALL AXPOSITION                                                  19
C    *                                                                    19
C    Create ACON :                                                        20
C    Call To Local Procedure ACON                                         20
         ASSIGN 5100 TO BLOCK_ACON                                        20
         GO TO  5090                                                      20
5100     CONTINUE                                                         20
C    *                                                                    20
C    Geant POSITION : ACON ===> X=-25.0 <===                              21
         CALL AGSRESET                                                    21
         AG_X = -25.0                                                     21
         AG_TITLE = 'ACON'                                                21
         AG_EXNAME = 'POSITION'                                           21
         AG_PARLIST = 'X'                                                 21
         CALL AXPOSITION                                                  21
C    *                                                                    21
C    Create ACNS :                                                        22
C    Call To Local Procedure ACNS                                         22
         ASSIGN 5120 TO BLOCK_ACNS                                        22
         GO TO  5110                                                      22
5120     CONTINUE                                                         22
C    *                                                                    22
C    Geant POSITION : ACNS ===> X=-50.0 <===                              23
         CALL AGSRESET                                                    23
         AG_X = -50.0                                                     23
         AG_TITLE = 'ACNS'                                                23
         AG_EXNAME = 'POSITION'                                           23
         AG_PARLIST = 'X'                                                 23
         CALL AXPOSITION                                                  23
C    *                                                                    23
C    Create APCN :                                                        24
C    Call To Local Procedure APCN                                         24
         ASSIGN 5140 TO BLOCK_APCN                                        24
         GO TO  5130                                                      24
5140     CONTINUE                                                         24
C    *                                                                    24
C    Geant POSITION : APCN ===> X=-12.5 Y=-25.0 <===                      25
         CALL AGSRESET                                                    25
         AG_X = -12.5                                                     25
         AG_Y = -25.0                                                     25
         AG_TITLE = 'APCN'                                                25
         AG_EXNAME = 'POSITION'                                           25
         AG_PARLIST = 'X_Y'                                               25
         CALL AXPOSITION                                                  25
C    *                                                                    25
C    Create APGN :                                                        26
C    Call To Local Procedure APGN                                         26
         ASSIGN 5160 TO BLOCK_APGN                                        26
         GO TO  5150                                                      26
5160     CONTINUE                                                         26
C    *                                                                    26
C    Geant POSITION : APGN ===> X=+12.5 Y=-25.0 <===                      27
         CALL AGSRESET                                                    27
         AG_X = +12.5                                                     27
         AG_Y = -25.0                                                     27
         AG_TITLE = 'APGN'                                                27
         AG_EXNAME = 'POSITION'                                           27
         AG_PARLIST = 'X_Y'                                               27
         CALL AXPOSITION                                                  27
C    *                                                                    27
C    Create APAR :                                                        28
C    Call To Local Procedure APAR                                         28
         ASSIGN 5180 TO BLOCK_APAR                                        28
         GO TO  5170                                                      28
5180     CONTINUE                                                         28
C    *                                                                    28
C    Geant POSITION : APAR ===> X=-37.5 Y=-25.0 <===                      29
         CALL AGSRESET                                                    29
         AG_X = -37.5                                                     29
         AG_Y = -25.0                                                     29
         AG_TITLE = 'APAR'                                                29
         AG_EXNAME = 'POSITION'                                           29
         AG_PARLIST = 'X_Y'                                               29
         CALL AXPOSITION                                                  29
C    *                                                                    29
C    Create AELT :                                                        30
C    Call To Local Procedure AELT                                         30
         ASSIGN 5200 TO BLOCK_AELT                                        30
         GO TO  5190                                                      30
5200     CONTINUE                                                         30
C    *                                                                    30
C    Geant POSITION : AELT ===> X=+37.5 Y=-25.0 <===                      31
         CALL AGSRESET                                                    31
         AG_X = +37.5                                                     31
         AG_Y = -25.0                                                     31
         AG_TITLE = 'AELT'                                                31
         AG_EXNAME = 'POSITION'                                           31
         AG_PARLIST = 'X_Y'                                               31
         CALL AXPOSITION                                                  31
C    *                                                                    31
5011  CALL AGSPOPB                                                        32
      GO TO BLOCK_TUTR, (5010 ,5020)                                      32
C End of Local Procedure                                                  32
C ----------------------------------------------------------------------  33
C ------------ ABOX                                                       33
C Begin Local Procedure ABOX                                              34
C ABOX A VOLUME WHICH IS A BOX IN THE TUTORIAL                            34
5030  CONTINUE                                                            34
         AG_TITLE='ABOX'                                                  34
         AG_MODULE=MODULE                                                 34
         AG_MARK='V00'                                                    34
         CALL AGSPUSH                                                     34
         AG_MODULE=MODULE                                                 34
C    *                                                                    34
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       35
         CALL AGSRESET                                                    35
         AG_SEEN = 1                                                      35
         AG_COLO = 4                                                      35
         AG_TITLE = 'NONE'                                                35
         AG_EXNAME = 'ATTRIBUTE'                                          35
         AG_PARLIST = 'SEEN_COLO'                                         35
         CALL AXATTRIBUTE                                                 35
C    *                                                                    35
C    Geant SHAPE : BOX ===> DX=10 DY=10 DZ=10 <===                        35
         CALL AGSRESET                                                    36
         AG_DX = 10                                                       36
         AG_DY = 10                                                       36
         AG_DZ = 10                                                       36
         AG_TITLE = 'BOX'                                                 36
         AG_EXNAME = 'SHAPE'                                              36
         AG_PARLIST = 'DX_DY_DZ'                                          36
         CALL AXSHAPE                                                     36
C    *                                                                    36
5031  CALL AGSPOPB                                                        37
      GO TO BLOCK_ABOX, (5030 ,5040)                                      37
C End of Local Procedure                                                  37
C ----------------------------------------------------------------------  38
C ------------ ATUB                                                       38
C Begin Local Procedure ATUB                                              39
C ATUB A TUBE WITH INNNER AND OUTER RADIUS                                39
5050  CONTINUE                                                            39
         AG_TITLE='ATUB'                                                  39
         AG_MODULE=MODULE                                                 39
         AG_MARK='V00'                                                    39
         CALL AGSPUSH                                                     39
         AG_MODULE=MODULE                                                 39
C    *                                                                    39
C    Geant ATTRIBUTE : ATUB ===> SEEN=1 COLO=3 <===                       40
         CALL AGSRESET                                                    40
         AG_SEEN = 1                                                      40
         AG_COLO = 3                                                      40
         AG_TITLE = 'ATUB'                                                40
         AG_EXNAME = 'ATTRIBUTE'                                          40
         AG_PARLIST = 'SEEN_COLO'                                         40
         CALL AXATTRIBUTE                                                 40
C    *                                                                    40
C    Geant SHAPE : TUBE ===> RMIN=5.0 RMAX=10.0 DZ=12.5 <===              40
         CALL AGSRESET                                                    41
         AG_RMIN = 5.0                                                    41
         AG_RMAX = 10.0                                                   41
         AG_DZ = 12.5                                                     41
         AG_TITLE = 'TUBE'                                                41
         AG_EXNAME = 'SHAPE'                                              41
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      41
         CALL AXSHAPE                                                     41
C    *                                                                    41
5051  CALL AGSPOPB                                                        42
      GO TO BLOCK_ATUB, (5050 ,5060)                                      42
C End of Local Procedure                                                  42
C ----------------------------------------------------------------------  43
C ------------ ATBS                                                       43
C Begin Local Procedure ATBS                                              44
C ATBS A TUBE SEGMENT WITH INNNER AND OUTER RADIUS... AND LIMITS IN PHI   44
5070  CONTINUE                                                            44
         AG_TITLE='ATBS'                                                  44
         AG_MODULE=MODULE                                                 44
         AG_MARK='V00'                                                    44
         CALL AGSPUSH                                                     44
         AG_MODULE=MODULE                                                 44
C    *                                                                    44
C    Geant ATTRIBUTE : ATBS ===> SEEN=1 COLO=3 <===                       45
         CALL AGSRESET                                                    45
         AG_SEEN = 1                                                      45
         AG_COLO = 3                                                      45
         AG_TITLE = 'ATBS'                                                45
         AG_EXNAME = 'ATTRIBUTE'                                          45
         AG_PARLIST = 'SEEN_COLO'                                         45
         CALL AXATTRIBUTE                                                 45
C    *                                                                    45
C    Geant SHAPE : TUBS ===> RMIN=5.0 RMAX=10.0 PHI1=-120.0, PHI2=+120.0  45
C     DZ=12.5 <===                                                        45
         CALL AGSRESET                                                    47
         AG_RMIN = 5.0                                                    47
         AG_RMAX = 10.0                                                   47
         AG_PHI1 = -120.0                                                 47
         AG_PHI2 = +120.0                                                 47
         AG_DZ = 12.5                                                     47
         AG_TITLE = 'TUBS'                                                47
         AG_EXNAME = 'SHAPE'                                              47
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            47
         CALL AXSHAPE                                                     47
C    *                                                                    47
5071  CALL AGSPOPB                                                        48
      GO TO BLOCK_ATBS, (5070 ,5080)                                      48
C End of Local Procedure                                                  48
C ----------------------------------------------------------------------  49
C ------------ ACON                                                       49
C Begin Local Procedure ACON                                              50
C ACON A CONE WITH INNNER AND OUTER RADIUS                                50
5090  CONTINUE                                                            50
         AG_TITLE='ACON'                                                  50
         AG_MODULE=MODULE                                                 50
         AG_MARK='V00'                                                    50
         CALL AGSPUSH                                                     50
         AG_MODULE=MODULE                                                 50
C    *                                                                    50
C    Geant ATTRIBUTE : ACON ===> SEEN=1 COLO=5 <===                       51
         CALL AGSRESET                                                    51
         AG_SEEN = 1                                                      51
         AG_COLO = 5                                                      51
         AG_TITLE = 'ACON'                                                51
         AG_EXNAME = 'ATTRIBUTE'                                          51
         AG_PARLIST = 'SEEN_COLO'                                         51
         CALL AXATTRIBUTE                                                 51
C    *                                                                    51
C    Geant SHAPE : CONE ===> DZ=12.5 RMN1=0.0 RMX1=10.0 RMN2=0.0, RMX2=0  51
C    .0 <===                                                              51
         CALL AGSRESET                                                    53
         AG_DZ = 12.5                                                     53
         AG_RMN1 = 0.0                                                    53
         AG_RMX1 = 10.0                                                   53
         AG_RMN2 = 0.0                                                    53
         AG_RMX2 = 0.0                                                    53
         AG_TITLE = 'CONE'                                                53
         AG_EXNAME = 'SHAPE'                                              53
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            53
         CALL AXSHAPE                                                     53
C    *                                                                    53
5091  CALL AGSPOPB                                                        54
      GO TO BLOCK_ACON, (5090 ,5100)                                      54
C End of Local Procedure                                                  54
C ----------------------------------------------------------------------  55
C ------------ ACNS                                                       55
C Begin Local Procedure ACNS                                              56
C ACNS A CONE SEGMENT WITH INNNER AND OUTER RADIUS... AND LIMITS IN PHI   56
5110  CONTINUE                                                            56
         AG_TITLE='ACNS'                                                  56
         AG_MODULE=MODULE                                                 56
         AG_MARK='V00'                                                    56
         CALL AGSPUSH                                                     56
         AG_MODULE=MODULE                                                 56
C    *                                                                    56
C    Geant ATTRIBUTE : ACNS ===> SEEN=1 COLO=5 <===                       57
         CALL AGSRESET                                                    57
         AG_SEEN = 1                                                      57
         AG_COLO = 5                                                      57
         AG_TITLE = 'ACNS'                                                57
         AG_EXNAME = 'ATTRIBUTE'                                          57
         AG_PARLIST = 'SEEN_COLO'                                         57
         CALL AXATTRIBUTE                                                 57
C    *                                                                    57
C    Geant SHAPE : CONS ===> DZ=12.5 RMN1=0.0 RMX1=10.0 RMN2=0.0, RMX2=0  57
C    .0 PHI1=-120.0 PHI2=+120.0 <===                                      57
         CALL AGSRESET                                                    59
         AG_DZ = 12.5                                                     59
         AG_RMN1 = 0.0                                                    59
         AG_RMX1 = 10.0                                                   59
         AG_RMN2 = 0.0                                                    59
         AG_RMX2 = 0.0                                                    59
         AG_PHI1 = -120.0                                                 59
         AG_PHI2 = +120.0                                                 59
         AG_TITLE = 'CONS'                                                59
         AG_EXNAME = 'SHAPE'                                              59
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  59
         CALL AXSHAPE                                                     59
C    *                                                                    59
5111  CALL AGSPOPB                                                        60
      GO TO BLOCK_ACNS, (5110 ,5120)                                      60
C End of Local Procedure                                                  60
C ----------------------------------------------------------------------  61
C ------------ APCN                                                       61
C Begin Local Procedure APCN                                              62
C APCN A POLYCONE                                                         62
5130  CONTINUE                                                            62
         AG_TITLE='APCN'                                                  62
         AG_MODULE=MODULE                                                 62
         AG_MARK='V00'                                                    62
         CALL AGSPUSH                                                     62
         AG_MODULE=MODULE                                                 62
C    *                                                                    62
C    Geant ATTRIBUTE : APCN ===> SEEN=1 COLO=7 <===                       63
         CALL AGSRESET                                                    63
         AG_SEEN = 1                                                      63
         AG_COLO = 7                                                      63
         AG_TITLE = 'APCN'                                                63
         AG_EXNAME = 'ATTRIBUTE'                                          63
         AG_PARLIST = 'SEEN_COLO'                                         63
         CALL AXATTRIBUTE                                                 63
C    *                                                                    63
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={0.,3.0, 5.0,15.0,2  63
C    0.0} RMN={0,0,0,0,0} RMX={2.5,5.0, 2.5,7.5,5.0} <===                 63
         CALL AGSRESET                                                    66
         AG_PHI1 = 0                                                      66
         AG_DPHI = 360                                                    66
         AG_NZ = 5                                                        66
         AG_ZI(1)=0.                                                      66
         AG_ZI(2)=3.0                                                     66
         AG_ZI(3)=5.0                                                     66
         AG_ZI(4)=15.0                                                    66
         AG_ZI(5)=20.0                                                    66
         AG_RMN(1)=0                                                      66
         AG_RMN(2)=0                                                      66
         AG_RMN(3)=0                                                      66
         AG_RMN(4)=0                                                      66
         AG_RMN(5)=0                                                      66
         AG_RMX(1)=2.5                                                    66
         AG_RMX(2)=5.0                                                    66
         AG_RMX(3)=2.5                                                    66
         AG_RMX(4)=7.5                                                    66
         AG_RMX(5)=5.0                                                    66
         AG_TITLE = 'PCON'                                                66
         AG_EXNAME = 'SHAPE'                                              66
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           66
         CALL AXSHAPE                                                     66
C    *                                                                    66
5131  CALL AGSPOPB                                                        67
      GO TO BLOCK_APCN, (5130 ,5140)                                      67
C End of Local Procedure                                                  67
C ----------------------------------------------------------------------  68
C ------------ APGN                                                       68
C Begin Local Procedure APGN                                              69
C APGN A POLYGON                                                          69
5150  CONTINUE                                                            69
         AG_TITLE='APGN'                                                  69
         AG_MODULE=MODULE                                                 69
         AG_MARK='V00'                                                    69
         CALL AGSPUSH                                                     69
         AG_MODULE=MODULE                                                 69
C    *                                                                    69
C    Geant ATTRIBUTE : APGN ===> SEEN=1 COLO=7 <===                       70
         CALL AGSRESET                                                    70
         AG_SEEN = 1                                                      70
         AG_COLO = 7                                                      70
         AG_TITLE = 'APGN'                                                70
         AG_EXNAME = 'ATTRIBUTE'                                          70
         AG_PARLIST = 'SEEN_COLO'                                         70
         CALL AXATTRIBUTE                                                 70
C    *                                                                    70
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=6 NZ=5, ZI={0.,3.0,5.  70
C    0,15.0,20.0} RMN={1.5,4.0,1.5,6.5, 4.0} RMX={2.5,5.0,2.5,7.5,5.0} <  70
C    ===                                                                  70
         CALL AGSRESET                                                    73
         AG_PHI1 = 0                                                      73
         AG_DPHI = 360                                                    73
         AG_NPDIV = 6                                                     73
         AG_NZ = 5                                                        73
         AG_ZI(1)=0.                                                      73
         AG_ZI(2)=3.0                                                     73
         AG_ZI(3)=5.0                                                     73
         AG_ZI(4)=15.0                                                    73
         AG_ZI(5)=20.0                                                    73
         AG_RMN(1)=1.5                                                    73
         AG_RMN(2)=4.0                                                    73
         AG_RMN(3)=1.5                                                    73
         AG_RMN(4)=6.5                                                    73
         AG_RMN(5)=4.0                                                    73
         AG_RMX(1)=2.5                                                    73
         AG_RMX(2)=5.0                                                    73
         AG_RMX(3)=2.5                                                    73
         AG_RMX(4)=7.5                                                    73
         AG_RMX(5)=5.0                                                    73
         AG_TITLE = 'PGON'                                                73
         AG_EXNAME = 'SHAPE'                                              73
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     73
         CALL AXSHAPE                                                     73
C    *                                                                    73
5151  CALL AGSPOPB                                                        74
      GO TO BLOCK_APGN, (5150 ,5160)                                      74
C End of Local Procedure                                                  74
C ----------------------------------------------------------------------  75
C ------------ APAR                                                       75
C Begin Local Procedure APAR                                              76
C APAR A PARALLELPIPED                                                    76
5170  CONTINUE                                                            76
         AG_TITLE='APAR'                                                  76
         AG_MODULE=MODULE                                                 76
         AG_MARK='V00'                                                    76
         CALL AGSPUSH                                                     76
         AG_MODULE=MODULE                                                 76
C    *                                                                    76
C    Geant ATTRIBUTE : APAR ===> SEEN=1 COLO=9 <===                       77
         CALL AGSRESET                                                    77
         AG_SEEN = 1                                                      77
         AG_COLO = 9                                                      77
         AG_TITLE = 'APAR'                                                77
         AG_EXNAME = 'ATTRIBUTE'                                          77
         AG_PARLIST = 'SEEN_COLO'                                         77
         CALL AXATTRIBUTE                                                 77
C    *                                                                    77
C    Geant SHAPE : PARA ===> DX=10.0 DY=8.0 DZ=20.0 ALPH=15.0, THET=10.0  77
C     PHI=5.0 <===                                                        77
         CALL AGSRESET                                                    79
         AG_DX = 10.0                                                     79
         AG_DY = 8.0                                                      79
         AG_DZ = 20.0                                                     79
         AG_ALPH = 15.0                                                   79
         AG_THET = 10.0                                                   79
         AG_PHI = 5.0                                                     79
         AG_TITLE = 'PARA'                                                79
         AG_EXNAME = 'SHAPE'                                              79
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            79
         CALL AXSHAPE                                                     79
C    *                                                                    79
5171  CALL AGSPOPB                                                        80
      GO TO BLOCK_APAR, (5170 ,5180)                                      80
C End of Local Procedure                                                  80
C ----------------------------------------------------------------------  81
C ------------ AELT                                                       81
C Begin Local Procedure AELT                                              82
C AELT AN ELLIPTICAL TUBE                                                 82
5190  CONTINUE                                                            82
         AG_TITLE='AELT'                                                  82
         AG_MODULE=MODULE                                                 82
         AG_MARK='V00'                                                    82
         CALL AGSPUSH                                                     82
         AG_MODULE=MODULE                                                 82
C    *                                                                    82
C    Geant ATTRIBUTE : AELT ===> SEEN=1 COLO=3 <===                       83
         CALL AGSRESET                                                    83
         AG_SEEN = 1                                                      83
         AG_COLO = 3                                                      83
         AG_TITLE = 'AELT'                                                83
         AG_EXNAME = 'ATTRIBUTE'                                          83
         AG_PARLIST = 'SEEN_COLO'                                         83
         CALL AXATTRIBUTE                                                 83
C    *                                                                    83
C    Geant SHAPE : ELTU ===> P1=10 P2=15 DZ=20 <===                       83
         CALL AGSRESET                                                    84
         AG_P1 = 10                                                       84
         AG_P2 = 15                                                       84
         AG_DZ = 20                                                       84
         AG_TITLE = 'ELTU'                                                84
         AG_EXNAME = 'SHAPE'                                              84
         AG_PARLIST = 'P1_P2_DZ'                                          84
         CALL AXSHAPE                                                     84
C    *                                                                    84
5191  CALL AGSPOPB                                                        85
      GO TO BLOCK_AELT, (5190 ,5200)                                      85
C End of Local Procedure                                                  85
      WRITE(LOUT,5210)'===>>  [End AgML Module TutrGeo4]  <<==='          85
5210  FORMAT(A32,/,/)                                                     85
C MODULE TUTRGEO4                                                         87
      END                                                                 87
