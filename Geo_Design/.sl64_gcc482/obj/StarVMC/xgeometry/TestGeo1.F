C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TESTGEO1                                                 1
C TESTGEO1 TEST OF PARAMTERIZED PLACEMENT OF VOLUMES                      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TESTGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'TEST OF PARAMTERIZED PLACEMENT OF VOLUMES'/          1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JCW                                                              1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JCW'/                                         2
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
      INTEGER BLOCK_MAIN                                                  4
      INTEGER BLOCK_TUBE                                                  5
      INTEGER BLOCK_TUBP                                                  5
      INTEGER BLOCK_TUBD                                                  5
      CALL AGSBEGM(MODULE,IPRIN)                                          6
C Check IPRIN<0                                                           6
      IF (IPRIN.LT.0) RETURN                                              6
C Create MAIN :                                                           6
C Call To Local Procedure MAIN                                            6
      ASSIGN 5020 TO BLOCK_MAIN                                           6
      GO TO  5010                                                         6
5020  CONTINUE                                                            6
C *                                                                       6
C Geant POSITION : MAIN ===> inherited arguments used <===                7
      CALL AGSRESET                                                       7
      AG_TITLE = 'MAIN'                                                   7
      AG_EXNAME = 'POSITION'                                              7
      AG_PARLIST = 'NONE'                                                 7
      CALL AXPOSITION                                                     7
C *                                                                       7
C ----------------------------------------------------------------------  7
C ------------ MAIN                                                       7
      CALL AGSENDM                                                        9
      RETURN                                                              9
C Begin Local Procedure MAIN                                              9
C MAIN A MASTER VOLUME                                                    9
5010  CONTINUE                                                            9
         AG_TITLE='MAIN'                                                  9
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
C    Geant SHAPE : BOX ===> DX=100.0 DY=100.0 DZ=100.0 <===               10
         CALL AGSRESET                                                    11
         AG_DX = 100.0                                                    11
         AG_DY = 100.0                                                    11
         AG_DZ = 100.0                                                    11
         AG_TITLE = 'BOX'                                                 11
         AG_EXNAME = 'SHAPE'                                              11
         AG_PARLIST = 'DX_DY_DZ'                                          11
         CALL AXSHAPE                                                     11
C    *                                                                    11
C    Create TUBD :                                                        12
C    Call To Local Procedure TUBD                                         12
         ASSIGN 5040 TO BLOCK_TUBD                                        12
         GO TO  5030                                                      12
5040     CONTINUE                                                         12
C    *                                                                    12
5011  CALL AGSPOPB                                                        13
      GO TO BLOCK_MAIN, (5010 ,5020)                                      13
C End of Local Procedure                                                  13
C ----------------------------------------------------------------------  14
C ------------ TUBD                                                       14
C Begin Local Procedure TUBD                                              15
C TUBD A DIVISION OF THE MOTHER VOLUME ALONG Y-AXIS                       15
5030  CONTINUE                                                            15
         AG_TITLE='TUBD'                                                  15
         AG_MODULE=MODULE                                                 15
         AG_MARK='V00'                                                    15
         CALL AGSPUSH                                                     15
         AG_MODULE=MODULE                                                 15
C    *                                                                    15
C    Geant SHAPE : DIVISION ===> NDIV=5 IAXIS=2 <===                      16
         CALL AGSRESET                                                    16
         AG_NDIV = 5                                                      16
         AG_IAXIS = 2                                                     16
         AG_TITLE = 'DIVISION'                                            16
         AG_EXNAME = 'SHAPE'                                              16
         AG_PARLIST = 'NDIV_IAXIS'                                        16
         CALL AXSHAPE                                                     16
C    *                                                                    16
C    Create TUBP :                                                        17
C    Call To Local Procedure TUBP                                         17
         ASSIGN 5060 TO BLOCK_TUBP                                        17
         GO TO  5050                                                      17
5060     CONTINUE                                                         17
C    *                                                                    17
C    Geant POSITION : TUBP ===> inherited arguments used <===             18
         CALL AGSRESET                                                    18
         AG_TITLE = 'TUBP'                                                18
         AG_EXNAME = 'POSITION'                                           18
         AG_PARLIST = 'NONE'                                              18
         CALL AXPOSITION                                                  18
C    *                                                                    18
5031  CALL AGSPOPB                                                        19
      GO TO BLOCK_TUBD, (5030 ,5040)                                      19
C End of Local Procedure                                                  19
C ----------------------------------------------------------------------  20
C ------------ TUBP                                                       20
C Begin Local Procedure TUBP                                              21
C TUBP A PARENT VOLUME FOR THE TUBES                                      21
5050  CONTINUE                                                            21
         AG_TITLE='TUBP'                                                  21
         AG_MODULE=MODULE                                                 21
         AG_MARK='V00'                                                    21
         CALL AGSPUSH                                                     21
         AG_MODULE=MODULE                                                 21
C    *                                                                    21
C    Geant MATERIAL : AIR ===> inherited arguments used <===              22
         CALL AGSRESET                                                    22
         AG_TITLE = 'AIR'                                                 22
         AG_EXNAME = 'MATERIAL'                                           22
         AG_PARLIST = 'NONE'                                              22
         CALL AXMATERIAL                                                  22
C    *                                                                    22
C    Geant SHAPE : BOX ===> DY=6.0 DZ=50.0 <===                           22
         CALL AGSRESET                                                    23
         AG_DY = 6.0                                                      23
         AG_DZ = 50.0                                                     23
         AG_TITLE = 'BOX'                                                 23
         AG_EXNAME = 'SHAPE'                                              23
         AG_PARLIST = 'DY_DZ'                                             23
         CALL AXSHAPE                                                     23
C    *                                                                    23
C    Create TUBE :                                                        24
C    Call To Local Procedure TUBE                                         24
         ASSIGN 5080 TO BLOCK_TUBE                                        24
         GO TO  5070                                                      24
5080     CONTINUE                                                         24
C    *                                                                    24
C    Geant POSITION : TUBE ===> X=-100.0 RMIN=0.0 RMAX=1.0 <===           25
         CALL AGSRESET                                                    25
         AG_X = -100.0                                                    25
         AG_RMIN = 0.0                                                    25
         AG_RMAX = 1.0                                                    25
         AG_TITLE = 'TUBE'                                                25
         AG_EXNAME = 'POSITION'                                           25
         AG_PARLIST = 'X_RMIN_RMAX'                                       25
         CALL AXPOSITION                                                  25
C    *                                                                    25
C    Geant POSITION : TUBE ===> X=-80.0 RMIN=0.0 RMAX=2.0 <===            25
         CALL AGSRESET                                                    26
         AG_X = -80.0                                                     26
         AG_RMIN = 0.0                                                    26
         AG_RMAX = 2.0                                                    26
         AG_TITLE = 'TUBE'                                                26
         AG_EXNAME = 'POSITION'                                           26
         AG_PARLIST = 'X_RMIN_RMAX'                                       26
         CALL AXPOSITION                                                  26
C    *                                                                    26
C    Geant POSITION : TUBE ===> X=-60.0 RMIN=0.0 RMAX=3.0 <===            26
         CALL AGSRESET                                                    27
         AG_X = -60.0                                                     27
         AG_RMIN = 0.0                                                    27
         AG_RMAX = 3.0                                                    27
         AG_TITLE = 'TUBE'                                                27
         AG_EXNAME = 'POSITION'                                           27
         AG_PARLIST = 'X_RMIN_RMAX'                                       27
         CALL AXPOSITION                                                  27
C    *                                                                    27
C    Geant POSITION : TUBE ===> X=-40.0 RMIN=0.0 RMAX=4.0 <===            27
         CALL AGSRESET                                                    28
         AG_X = -40.0                                                     28
         AG_RMIN = 0.0                                                    28
         AG_RMAX = 4.0                                                    28
         AG_TITLE = 'TUBE'                                                28
         AG_EXNAME = 'POSITION'                                           28
         AG_PARLIST = 'X_RMIN_RMAX'                                       28
         CALL AXPOSITION                                                  28
C    *                                                                    28
C    Geant POSITION : TUBE ===> X=-20.0 RMIN=0.0 RMAX=5.0 <===            28
         CALL AGSRESET                                                    29
         AG_X = -20.0                                                     29
         AG_RMIN = 0.0                                                    29
         AG_RMAX = 5.0                                                    29
         AG_TITLE = 'TUBE'                                                29
         AG_EXNAME = 'POSITION'                                           29
         AG_PARLIST = 'X_RMIN_RMAX'                                       29
         CALL AXPOSITION                                                  29
C    *                                                                    29
C    Geant POSITION : TUBE ===> X=0.0 RMIN=0.0 RMAX=6.0 <===              29
         CALL AGSRESET                                                    30
         AG_X = 0.0                                                       30
         AG_RMIN = 0.0                                                    30
         AG_RMAX = 6.0                                                    30
         AG_TITLE = 'TUBE'                                                30
         AG_EXNAME = 'POSITION'                                           30
         AG_PARLIST = 'X_RMIN_RMAX'                                       30
         CALL AXPOSITION                                                  30
C    *                                                                    30
C    Geant POSITION : TUBE ===> X=+20.0 RMIN=0.0 RMAX=5.0 <===            30
         CALL AGSRESET                                                    31
         AG_X = +20.0                                                     31
         AG_RMIN = 0.0                                                    31
         AG_RMAX = 5.0                                                    31
         AG_TITLE = 'TUBE'                                                31
         AG_EXNAME = 'POSITION'                                           31
         AG_PARLIST = 'X_RMIN_RMAX'                                       31
         CALL AXPOSITION                                                  31
C    *                                                                    31
C    Geant POSITION : TUBE ===> X=+40.0 RMIN=0.0 RMAX=4.0 <===            31
         CALL AGSRESET                                                    32
         AG_X = +40.0                                                     32
         AG_RMIN = 0.0                                                    32
         AG_RMAX = 4.0                                                    32
         AG_TITLE = 'TUBE'                                                32
         AG_EXNAME = 'POSITION'                                           32
         AG_PARLIST = 'X_RMIN_RMAX'                                       32
         CALL AXPOSITION                                                  32
C    *                                                                    32
C    Geant POSITION : TUBE ===> X=+60.0 RMIN=0.0 RMAX=3.0 <===            32
         CALL AGSRESET                                                    33
         AG_X = +60.0                                                     33
         AG_RMIN = 0.0                                                    33
         AG_RMAX = 3.0                                                    33
         AG_TITLE = 'TUBE'                                                33
         AG_EXNAME = 'POSITION'                                           33
         AG_PARLIST = 'X_RMIN_RMAX'                                       33
         CALL AXPOSITION                                                  33
C    *                                                                    33
C    Geant POSITION : TUBE ===> X=+80.0 RMIN=0.0 RMAX=2.0 <===            33
         CALL AGSRESET                                                    34
         AG_X = +80.0                                                     34
         AG_RMIN = 0.0                                                    34
         AG_RMAX = 2.0                                                    34
         AG_TITLE = 'TUBE'                                                34
         AG_EXNAME = 'POSITION'                                           34
         AG_PARLIST = 'X_RMIN_RMAX'                                       34
         CALL AXPOSITION                                                  34
C    *                                                                    34
C    Geant POSITION : TUBE ===> X=+100.0 RMIN=0.0 RMAX=1.0 <===           34
         CALL AGSRESET                                                    35
         AG_X = +100.0                                                    35
         AG_RMIN = 0.0                                                    35
         AG_RMAX = 1.0                                                    35
         AG_TITLE = 'TUBE'                                                35
         AG_EXNAME = 'POSITION'                                           35
         AG_PARLIST = 'X_RMIN_RMAX'                                       35
         CALL AXPOSITION                                                  35
C    *                                                                    35
5051  CALL AGSPOPB                                                        36
      GO TO BLOCK_TUBP, (5050 ,5060)                                      36
C End of Local Procedure                                                  36
C ----------------------------------------------------------------------  37
C ------------ TUBE                                                       37
C Begin Local Procedure TUBE                                              38
C TUBE A SUB VOLUME SETUP FOR SHAPE DEFINTION WHEN POSITIONED             38
5070  CONTINUE                                                            38
         AG_TITLE='TUBE'                                                  38
         AG_MODULE=MODULE                                                 38
         AG_MARK='V00'                                                    38
         CALL AGSPUSH                                                     38
         AG_MODULE=MODULE                                                 38
C    *                                                                    38
C    Geant MATERIAL : IRON ===> inherited arguments used <===             39
         CALL AGSRESET                                                    39
         AG_TITLE = 'IRON'                                                39
         AG_EXNAME = 'MATERIAL'                                           39
         AG_PARLIST = 'NONE'                                              39
         CALL AXMATERIAL                                                  39
C    *                                                                    39
C    Geant ATTRIBUTE : TUBE ===> COLO=44 <===                             39
         CALL AGSRESET                                                    40
         AG_COLO = 44                                                     40
         AG_TITLE = 'TUBE'                                                40
         AG_EXNAME = 'ATTRIBUTE'                                          40
         AG_PARLIST = 'COLO'                                              40
         CALL AXATTRIBUTE                                                 40
C    *                                                                    40
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.0 DZ=0.0 <===                40
         CALL AGSRESET                                                    41
         AG_RMIN = 0.0                                                    41
         AG_RMAX = 0.0                                                    41
         AG_DZ = 0.0                                                      41
         AG_TITLE = 'TUBE'                                                41
         AG_EXNAME = 'SHAPE'                                              41
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      41
         CALL AXSHAPE                                                     41
C    *                                                                    41
5071  CALL AGSPOPB                                                        42
      GO TO BLOCK_TUBE, (5070 ,5080)                                      42
C End of Local Procedure                                                  42
      WRITE(LOUT,5090)'===>>  [End AgML Module TestGeo1]  <<==='          42
5090  FORMAT(A32,/,/)                                                     42
C MODULE TESTGEO1                                                         44
      END                                                                 44
