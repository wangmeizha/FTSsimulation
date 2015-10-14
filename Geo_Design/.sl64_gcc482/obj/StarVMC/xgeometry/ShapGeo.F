C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SHAPGEO                                                  1
C SHAPGEO SHAPES GEOMETRY : PLACES AGML SHAPES INTO THE CAVE              1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SHAPGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'SHAPES GEOMETRY : PLACES AGML SHAPES INTO THE        1
     *CAVE'/                                                              1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JASON C. WEBB                                                    1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JASON C. WEBB'/                               2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 11/01/2011                                                      2
      CHARACTER*40 CREATED /'11/01/2011'/                                 3
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
      INTEGER BLOCK_BBOX                                                  4
      INTEGER BLOCK_PARA                                                  6
      INTEGER BLOCK_TRDA                                                  6
      INTEGER BLOCK_TRDB                                                  6
      INTEGER BLOCK_TRAP                                                  6
      INTEGER BLOCK_SPHE                                                  6
      INTEGER BLOCK_TUBE                                                  6
      INTEGER BLOCK_TUBS                                                  6
      INTEGER BLOCK_CONE                                                  6
      INTEGER BLOCK_CONS                                                  6
      INTEGER BLOCK_ELTU                                                  6
      INTEGER BLOCK_PCON                                                  6
      INTEGER BLOCK_PGON                                                  6
      INTEGER BLOCK_CTUB                                                  6
      INTEGER BLOCK_GTRA                                                  6
      REAL *4 X/-150.0/, DX/75.0/, Y/-30.0/                               7
      INTEGER *4 ICOLOR/27/                                               9
      X=-150.0                                                            11
      Y=-30                                                               11
      CALL AGSBEGM(MODULE,IPRIN)                                          13
C Check IPRIN<0                                                           13
      IF (IPRIN.LT.0) RETURN                                              13
C Create BBOX :                                                           13
C Call To Local Procedure BBOX                                            13
      ASSIGN 5020 TO BLOCK_BBOX                                           13
      GO TO  5010                                                         13
5020  CONTINUE                                                            13
C *                                                                       13
C Geant POSITION : BBOX ===> X=X Y=Y <===                                 14
      CALL AGSRESET                                                       14
      AG_X = X                                                            14
      AG_Y = Y                                                            14
      AG_TITLE = 'BBOX'                                                   14
      AG_EXNAME = 'POSITION'                                              14
      AG_PARLIST = 'X_Y'                                                  14
      CALL AXPOSITION                                                     14
C *                                                                       14
      X=X+DX                                                              14
      CALL AGSBEGM(MODULE,IPRIN)                                          16
C Check IPRIN<0                                                           16
      IF (IPRIN.LT.0) RETURN                                              16
C Create PARA :                                                           16
C Call To Local Procedure PARA                                            16
      ASSIGN 5040 TO BLOCK_PARA                                           16
      GO TO  5030                                                         16
5040  CONTINUE                                                            16
C *                                                                       16
C Geant POSITION : PARA ===> X=X Y=Y <===                                 17
      CALL AGSRESET                                                       17
      AG_X = X                                                            17
      AG_Y = Y                                                            17
      AG_TITLE = 'PARA'                                                   17
      AG_EXNAME = 'POSITION'                                              17
      AG_PARLIST = 'X_Y'                                                  17
      CALL AXPOSITION                                                     17
C *                                                                       17
      X=X+DX                                                              17
      CALL AGSBEGM(MODULE,IPRIN)                                          19
C Check IPRIN<0                                                           19
      IF (IPRIN.LT.0) RETURN                                              19
C Create TRDA :                                                           19
C Call To Local Procedure TRDA                                            19
      ASSIGN 5060 TO BLOCK_TRDA                                           19
      GO TO  5050                                                         19
5060  CONTINUE                                                            19
C *                                                                       19
C Geant POSITION : TRDA ===> X=X Y=Y <===                                 20
      CALL AGSRESET                                                       20
      AG_X = X                                                            20
      AG_Y = Y                                                            20
      AG_TITLE = 'TRDA'                                                   20
      AG_EXNAME = 'POSITION'                                              20
      AG_PARLIST = 'X_Y'                                                  20
      CALL AXPOSITION                                                     20
C *                                                                       20
      X=X+DX                                                              20
      CALL AGSBEGM(MODULE,IPRIN)                                          22
C Check IPRIN<0                                                           22
      IF (IPRIN.LT.0) RETURN                                              22
C Create TRDB :                                                           22
C Call To Local Procedure TRDB                                            22
      ASSIGN 5080 TO BLOCK_TRDB                                           22
      GO TO  5070                                                         22
5080  CONTINUE                                                            22
C *                                                                       22
C Geant POSITION : TRDB ===> X=X Y=Y <===                                 23
      CALL AGSRESET                                                       23
      AG_X = X                                                            23
      AG_Y = Y                                                            23
      AG_TITLE = 'TRDB'                                                   23
      AG_EXNAME = 'POSITION'                                              23
      AG_PARLIST = 'X_Y'                                                  23
      CALL AXPOSITION                                                     23
C *                                                                       23
      X=X+DX                                                              23
      CALL AGSBEGM(MODULE,IPRIN)                                          25
C Check IPRIN<0                                                           25
      IF (IPRIN.LT.0) RETURN                                              25
C Create TRAP :                                                           25
C Call To Local Procedure TRAP                                            25
      ASSIGN 5100 TO BLOCK_TRAP                                           25
      GO TO  5090                                                         25
5100  CONTINUE                                                            25
C *                                                                       25
C Geant POSITION : TRAP ===> X=X Y=Y <===                                 26
      CALL AGSRESET                                                       26
      AG_X = X                                                            26
      AG_Y = Y                                                            26
      AG_TITLE = 'TRAP'                                                   26
      AG_EXNAME = 'POSITION'                                              26
      AG_PARLIST = 'X_Y'                                                  26
      CALL AXPOSITION                                                     26
C *                                                                       26
      X=X+DX                                                              26
      CALL AGSBEGM(MODULE,IPRIN)                                          28
C Check IPRIN<0                                                           28
      IF (IPRIN.LT.0) RETURN                                              28
C Create SPHE :                                                           28
C Call To Local Procedure SPHE                                            28
      ASSIGN 5120 TO BLOCK_SPHE                                           28
      GO TO  5110                                                         28
5120  CONTINUE                                                            28
C *                                                                       28
C Geant POSITION : SPHE ===> X=X Y=Y <===                                 29
      CALL AGSRESET                                                       29
      AG_X = X                                                            29
      AG_Y = Y                                                            29
      AG_TITLE = 'SPHE'                                                   29
      AG_EXNAME = 'POSITION'                                              29
      AG_PARLIST = 'X_Y'                                                  29
      CALL AXPOSITION                                                     29
C *                                                                       29
      X=X+DX                                                              29
      CALL AGSBEGM(MODULE,IPRIN)                                          31
C Check IPRIN<0                                                           31
      IF (IPRIN.LT.0) RETURN                                              31
C Create TUBE :                                                           31
C Call To Local Procedure TUBE                                            31
      ASSIGN 5140 TO BLOCK_TUBE                                           31
      GO TO  5130                                                         31
5140  CONTINUE                                                            31
C *                                                                       31
C Geant POSITION : TUBE ===> X=X Y=Y <===                                 32
      CALL AGSRESET                                                       32
      AG_X = X                                                            32
      AG_Y = Y                                                            32
      AG_TITLE = 'TUBE'                                                   32
      AG_EXNAME = 'POSITION'                                              32
      AG_PARLIST = 'X_Y'                                                  32
      CALL AXPOSITION                                                     32
C *                                                                       32
      X=X+DX                                                              32
      CALL AGSBEGM(MODULE,IPRIN)                                          34
C Check IPRIN<0                                                           34
      IF (IPRIN.LT.0) RETURN                                              34
C Create TUBS :                                                           34
C Call To Local Procedure TUBS                                            34
      ASSIGN 5160 TO BLOCK_TUBS                                           34
      GO TO  5150                                                         34
5160  CONTINUE                                                            34
C *                                                                       34
C Geant POSITION : TUBS ===> X=X Y=Y <===                                 35
      CALL AGSRESET                                                       35
      AG_X = X                                                            35
      AG_Y = Y                                                            35
      AG_TITLE = 'TUBS'                                                   35
      AG_EXNAME = 'POSITION'                                              35
      AG_PARLIST = 'X_Y'                                                  35
      CALL AXPOSITION                                                     35
C *                                                                       35
      X=X+DX                                                              35
      X=-150.0                                                            37
      Y=+30.0                                                             38
      CALL AGSBEGM(MODULE,IPRIN)                                          39
C Check IPRIN<0                                                           39
      IF (IPRIN.LT.0) RETURN                                              39
C Create CTUB :                                                           39
C Call To Local Procedure CTUB                                            39
      ASSIGN 5180 TO BLOCK_CTUB                                           39
      GO TO  5170                                                         39
5180  CONTINUE                                                            39
C *                                                                       39
C Geant POSITION : CTUB ===> X=X Y=Y <===                                 40
      CALL AGSRESET                                                       40
      AG_X = X                                                            40
      AG_Y = Y                                                            40
      AG_TITLE = 'CTUB'                                                   40
      AG_EXNAME = 'POSITION'                                              40
      AG_PARLIST = 'X_Y'                                                  40
      CALL AXPOSITION                                                     40
C *                                                                       40
      X=X+DX                                                              40
      CALL AGSBEGM(MODULE,IPRIN)                                          42
C Check IPRIN<0                                                           42
      IF (IPRIN.LT.0) RETURN                                              42
C Create ELTU :                                                           42
C Call To Local Procedure ELTU                                            42
      ASSIGN 5200 TO BLOCK_ELTU                                           42
      GO TO  5190                                                         42
5200  CONTINUE                                                            42
C *                                                                       42
C Geant POSITION : ELTU ===> X=X Y=Y <===                                 43
      CALL AGSRESET                                                       43
      AG_X = X                                                            43
      AG_Y = Y                                                            43
      AG_TITLE = 'ELTU'                                                   43
      AG_EXNAME = 'POSITION'                                              43
      AG_PARLIST = 'X_Y'                                                  43
      CALL AXPOSITION                                                     43
C *                                                                       43
      X=X+DX                                                              43
      CALL AGSBEGM(MODULE,IPRIN)                                          45
C Check IPRIN<0                                                           45
      IF (IPRIN.LT.0) RETURN                                              45
C Create CONE :                                                           45
C Call To Local Procedure CONE                                            45
      ASSIGN 5220 TO BLOCK_CONE                                           45
      GO TO  5210                                                         45
5220  CONTINUE                                                            45
C *                                                                       45
C Geant POSITION : CONE ===> X=X Y=Y <===                                 46
      CALL AGSRESET                                                       46
      AG_X = X                                                            46
      AG_Y = Y                                                            46
      AG_TITLE = 'CONE'                                                   46
      AG_EXNAME = 'POSITION'                                              46
      AG_PARLIST = 'X_Y'                                                  46
      CALL AXPOSITION                                                     46
C *                                                                       46
      X=X+DX                                                              46
      CALL AGSBEGM(MODULE,IPRIN)                                          48
C Check IPRIN<0                                                           48
      IF (IPRIN.LT.0) RETURN                                              48
C Create CONS :                                                           48
C Call To Local Procedure CONS                                            48
      ASSIGN 5240 TO BLOCK_CONS                                           48
      GO TO  5230                                                         48
5240  CONTINUE                                                            48
C *                                                                       48
C Geant POSITION : CONS ===> X=X Y=Y <===                                 49
      CALL AGSRESET                                                       49
      AG_X = X                                                            49
      AG_Y = Y                                                            49
      AG_TITLE = 'CONS'                                                   49
      AG_EXNAME = 'POSITION'                                              49
      AG_PARLIST = 'X_Y'                                                  49
      CALL AXPOSITION                                                     49
C *                                                                       49
      X=X+DX                                                              49
      CALL AGSBEGM(MODULE,IPRIN)                                          51
C Check IPRIN<0                                                           51
      IF (IPRIN.LT.0) RETURN                                              51
C Create PCON :                                                           51
C Call To Local Procedure PCON                                            51
      ASSIGN 5260 TO BLOCK_PCON                                           51
      GO TO  5250                                                         51
5260  CONTINUE                                                            51
C *                                                                       51
C Geant POSITION : PCON ===> X=X Y=Y <===                                 52
      CALL AGSRESET                                                       52
      AG_X = X                                                            52
      AG_Y = Y                                                            52
      AG_TITLE = 'PCON'                                                   52
      AG_EXNAME = 'POSITION'                                              52
      AG_PARLIST = 'X_Y'                                                  52
      CALL AXPOSITION                                                     52
C *                                                                       52
      X=X+DX                                                              52
      CALL AGSBEGM(MODULE,IPRIN)                                          54
C Check IPRIN<0                                                           54
      IF (IPRIN.LT.0) RETURN                                              54
C Create PGON :                                                           54
C Call To Local Procedure PGON                                            54
      ASSIGN 5280 TO BLOCK_PGON                                           54
      GO TO  5270                                                         54
5280  CONTINUE                                                            54
C *                                                                       54
C Geant POSITION : PGON ===> X=X Y=Y <===                                 55
      CALL AGSRESET                                                       55
      AG_X = X                                                            55
      AG_Y = Y                                                            55
      AG_TITLE = 'PGON'                                                   55
      AG_EXNAME = 'POSITION'                                              55
      AG_PARLIST = 'X_Y'                                                  55
      CALL AXPOSITION                                                     55
C *                                                                       55
      X=X+DX                                                              55
      CALL AGSBEGM(MODULE,IPRIN)                                          57
C Check IPRIN<0                                                           57
      IF (IPRIN.LT.0) RETURN                                              57
C Create GTRA :                                                           57
C Call To Local Procedure GTRA                                            57
      ASSIGN 5300 TO BLOCK_GTRA                                           57
      GO TO  5290                                                         57
5300  CONTINUE                                                            57
C *                                                                       57
C Geant POSITION : GTRA ===> X=X Y=Y <===                                 58
      CALL AGSRESET                                                       58
      AG_X = X                                                            58
      AG_Y = Y                                                            58
      AG_TITLE = 'GTRA'                                                   58
      AG_EXNAME = 'POSITION'                                              58
      AG_PARLIST = 'X_Y'                                                  58
      CALL AXPOSITION                                                     58
C *                                                                       58
      X=X+DX                                                              58
C ----------------------------------------------------------------------  60
C ------------ BBOX                                                       60
      CALL AGSENDM                                                        61
      RETURN                                                              61
C Begin Local Procedure BBOX                                              61
C BBOX A BOX / TGEOBBOX                                                   61
5010  CONTINUE                                                            61
         AG_TITLE='BBOX'                                                  61
         AG_MODULE=MODULE                                                 61
         AG_MARK='V00'                                                    61
         CALL AGSPUSH                                                     61
         AG_MODULE=MODULE                                                 61
C    *                                                                    61
C    Geant ATTRIBUTE : BBOX ===> COLO=ICOLOR <===                         62
         CALL AGSRESET                                                    62
         AG_COLO = ICOLOR                                                 62
         AG_TITLE = 'BBOX'                                                62
         AG_EXNAME = 'ATTRIBUTE'                                          62
         AG_PARLIST = 'COLO'                                              62
         CALL AXATTRIBUTE                                                 62
C    *                                                                    62
         ICOLOR=ICOLOR+1                                                  62
C    Geant SHAPE : BOX ===> DX=20 DY=20 DZ=40 <===                        64
         CALL AGSRESET                                                    64
         AG_DX = 20                                                       64
         AG_DY = 20                                                       64
         AG_DZ = 40                                                       64
         AG_TITLE = 'BOX'                                                 64
         AG_EXNAME = 'SHAPE'                                              64
         AG_PARLIST = 'DX_DY_DZ'                                          64
         CALL AXSHAPE                                                     64
C    *                                                                    64
5011  CALL AGSPOPB                                                        65
      GO TO BLOCK_BBOX, (5010 ,5020)                                      65
C End of Local Procedure                                                  65
C ----------------------------------------------------------------------  66
C ------------ PARA                                                       66
C Begin Local Procedure PARA                                              67
C PARA A PARALLELPIPED TGEOPARA                                           67
5030  CONTINUE                                                            67
         AG_TITLE='PARA'                                                  67
         AG_MODULE=MODULE                                                 67
         AG_MARK='V00'                                                    67
         CALL AGSPUSH                                                     67
         AG_MODULE=MODULE                                                 67
C    *                                                                    67
C    Geant ATTRIBUTE : PARA ===> COLO=ICOLOR <===                         68
         CALL AGSRESET                                                    68
         AG_COLO = ICOLOR                                                 68
         AG_TITLE = 'PARA'                                                68
         AG_EXNAME = 'ATTRIBUTE'                                          68
         AG_PARLIST = 'COLO'                                              68
         CALL AXATTRIBUTE                                                 68
C    *                                                                    68
         ICOLOR=ICOLOR+1                                                  68
C    Geant SHAPE : PARA ===> DX=20 DY=20 DZ=40 ALPH=20 THET=20, PHI=45 <  71
C    ===                                                                  71
         CALL AGSRESET                                                    71
         AG_DX = 20                                                       71
         AG_DY = 20                                                       71
         AG_DZ = 40                                                       71
         AG_ALPH = 20                                                     71
         AG_THET = 20                                                     71
         AG_PHI = 45                                                      71
         AG_TITLE = 'PARA'                                                71
         AG_EXNAME = 'SHAPE'                                              71
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            71
         CALL AXSHAPE                                                     71
C    *                                                                    71
5031  CALL AGSPOPB                                                        72
      GO TO BLOCK_PARA, (5030 ,5040)                                      72
C End of Local Procedure                                                  72
C ----------------------------------------------------------------------  73
C ------------ TRDA                                                       73
C Begin Local Procedure TRDA                                              74
C TRDA A TRAPEZOID TGEOTRD1                                               74
5050  CONTINUE                                                            74
         AG_TITLE='TRDA'                                                  74
         AG_MODULE=MODULE                                                 74
         AG_MARK='V00'                                                    74
         CALL AGSPUSH                                                     74
         AG_MODULE=MODULE                                                 74
C    *                                                                    74
C    Geant ATTRIBUTE : TRDA ===> COLO=ICOLOR <===                         75
         CALL AGSRESET                                                    75
         AG_COLO = ICOLOR                                                 75
         AG_TITLE = 'TRDA'                                                75
         AG_EXNAME = 'ATTRIBUTE'                                          75
         AG_PARLIST = 'COLO'                                              75
         CALL AXATTRIBUTE                                                 75
C    *                                                                    75
         ICOLOR=ICOLOR+1                                                  75
C    Geant SHAPE : TRD1 ===> DX1=10 DX2=20 DY=30 DZ=40 <===               77
         CALL AGSRESET                                                    77
         AG_DX1 = 10                                                      77
         AG_DX2 = 20                                                      77
         AG_DY = 30                                                       77
         AG_DZ = 40                                                       77
         AG_TITLE = 'TRD1'                                                77
         AG_EXNAME = 'SHAPE'                                              77
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     77
         CALL AXSHAPE                                                     77
C    *                                                                    77
5051  CALL AGSPOPB                                                        78
      GO TO BLOCK_TRDA, (5050 ,5060)                                      78
C End of Local Procedure                                                  78
C ----------------------------------------------------------------------  79
C ------------ TRDB                                                       79
C Begin Local Procedure TRDB                                              80
C TRDB A TRAPEZOID TGEOTRD2                                               80
5070  CONTINUE                                                            80
         AG_TITLE='TRDB'                                                  80
         AG_MODULE=MODULE                                                 80
         AG_MARK='V00'                                                    80
         CALL AGSPUSH                                                     80
         AG_MODULE=MODULE                                                 80
C    *                                                                    80
C    Geant ATTRIBUTE : TRDB ===> COLO=ICOLOR <===                         81
         CALL AGSRESET                                                    81
         AG_COLO = ICOLOR                                                 81
         AG_TITLE = 'TRDB'                                                81
         AG_EXNAME = 'ATTRIBUTE'                                          81
         AG_PARLIST = 'COLO'                                              81
         CALL AXATTRIBUTE                                                 81
C    *                                                                    81
         ICOLOR=ICOLOR+1                                                  81
C    Geant SHAPE : TRD2 ===> DX1=10 DX2=20 DY1=30 DY2=10 DZ=40 <===       83
         CALL AGSRESET                                                    83
         AG_DX1 = 10                                                      83
         AG_DX2 = 20                                                      83
         AG_DY1 = 30                                                      83
         AG_DY2 = 10                                                      83
         AG_DZ = 40                                                       83
         AG_TITLE = 'TRD2'                                                83
         AG_EXNAME = 'SHAPE'                                              83
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                83
         CALL AXSHAPE                                                     83
C    *                                                                    83
5071  CALL AGSPOPB                                                        84
      GO TO BLOCK_TRDB, (5070 ,5080)                                      84
C End of Local Procedure                                                  84
C ----------------------------------------------------------------------  85
C ------------ TRAP                                                       85
C Begin Local Procedure TRAP                                              86
C TRAP A TRAPEZOID TGEOTRAP                                               86
5090  CONTINUE                                                            86
         AG_TITLE='TRAP'                                                  86
         AG_MODULE=MODULE                                                 86
         AG_MARK='V00'                                                    86
         CALL AGSPUSH                                                     86
         AG_MODULE=MODULE                                                 86
C    *                                                                    86
C    Geant ATTRIBUTE : TRAP ===> COLO=ICOLOR <===                         87
         CALL AGSRESET                                                    87
         AG_COLO = ICOLOR                                                 87
         AG_TITLE = 'TRAP'                                                87
         AG_EXNAME = 'ATTRIBUTE'                                          87
         AG_PARLIST = 'COLO'                                              87
         CALL AXATTRIBUTE                                                 87
C    *                                                                    87
         ICOLOR=ICOLOR+1                                                  87
C    Geant SHAPE : TRAP ===> DZ=40 THET=15 PHI=30 H1=20 BL1=10, TL1=15 A  90
C    LP1=0.0 H2=20 BL2=10 TL2=15 ALP2=0.0 <===                            90
         CALL AGSRESET                                                    90
         AG_DZ = 40                                                       90
         AG_THET = 15                                                     90
         AG_PHI = 30                                                      90
         AG_H1 = 20                                                       90
         AG_BL1 = 10                                                      90
         AG_TL1 = 15                                                      90
         AG_ALP1 = 0.0                                                    90
         AG_H2 = 20                                                       90
         AG_BL2 = 10                                                      90
         AG_TL2 = 15                                                      90
         AG_ALP2 = 0.0                                                    90
         AG_TITLE = 'TRAP'                                                90
         AG_EXNAME = 'SHAPE'                                              90
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       90
         CALL AXSHAPE                                                     90
C    *                                                                    90
5091  CALL AGSPOPB                                                        91
      GO TO BLOCK_TRAP, (5090 ,5100)                                      91
C End of Local Procedure                                                  91
C ----------------------------------------------------------------------  92
C ------------ GTRA                                                       92
C Begin Local Procedure GTRA                                              93
C GTRA A TRAPEZOID TGEOGTRA                                               93
5290  CONTINUE                                                            93
         AG_TITLE='GTRA'                                                  93
         AG_MODULE=MODULE                                                 93
         AG_MARK='V00'                                                    93
         CALL AGSPUSH                                                     93
         AG_MODULE=MODULE                                                 93
C    *                                                                    93
C    Geant ATTRIBUTE : GTRA ===> COLO=ICOLOR <===                         94
         CALL AGSRESET                                                    94
         AG_COLO = ICOLOR                                                 94
         AG_TITLE = 'GTRA'                                                94
         AG_EXNAME = 'ATTRIBUTE'                                          94
         AG_PARLIST = 'COLO'                                              94
         CALL AXATTRIBUTE                                                 94
C    *                                                                    94
         ICOLOR=ICOLOR+1                                                  94
C    Geant SHAPE : GTRA ===> DZ=40 THET=15 PHI=30 TWIS=30, H1=20 BL1=10   98
C    TL1=15 ALP1=0.0 H2=20 BL2=10 TL2=15, ALP2=0.0 <===                   98
         CALL AGSRESET                                                    98
         AG_DZ = 40                                                       98
         AG_THET = 15                                                     98
         AG_PHI = 30                                                      98
         AG_TWIS = 30                                                     98
         AG_H1 = 20                                                       98
         AG_BL1 = 10                                                      98
         AG_TL1 = 15                                                      98
         AG_ALP1 = 0.0                                                    98
         AG_H2 = 20                                                       98
         AG_BL2 = 10                                                      98
         AG_TL2 = 15                                                      98
         AG_ALP2 = 0.0                                                    98
         AG_TITLE = 'GTRA'                                                98
         AG_EXNAME = 'SHAPE'                                              98
         AG_PARLIST =                                                     98
     *   'DZ_THET_PHI_TWIS_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'               98
         CALL AXSHAPE                                                     98
C    *                                                                    98
5291  CALL AGSPOPB                                                        99
      GO TO BLOCK_GTRA, (5290 ,5300)                                      99
C End of Local Procedure                                                  99
C ----------------------------------------------------------------------  100
C ------------ SPHE                                                       100
C Begin Local Procedure SPHE                                              101
C SPHE A SPHERE                                                           101
5110  CONTINUE                                                            101
         AG_TITLE='SPHE'                                                  101
         AG_MODULE=MODULE                                                 101
         AG_MARK='V00'                                                    101
         CALL AGSPUSH                                                     101
         AG_MODULE=MODULE                                                 101
C    *                                                                    101
C    Geant ATTRIBUTE : SPHE ===> COLO=ICOLOR <===                         102
         CALL AGSRESET                                                    102
         AG_COLO = ICOLOR                                                 102
         AG_TITLE = 'SPHE'                                                102
         AG_EXNAME = 'ATTRIBUTE'                                          102
         AG_PARLIST = 'COLO'                                              102
         CALL AXATTRIBUTE                                                 102
C    *                                                                    102
         ICOLOR=ICOLOR+1                                                  102
C    Geant SHAPE : SPHE ===> RMIN=18.0 RMAX=20.0 <===                     104
         CALL AGSRESET                                                    104
         AG_RMIN = 18.0                                                   104
         AG_RMAX = 20.0                                                   104
         AG_TITLE = 'SPHE'                                                104
         AG_EXNAME = 'SHAPE'                                              104
         AG_PARLIST = 'RMIN_RMAX'                                         104
         CALL AXSHAPE                                                     104
C    *                                                                    104
5111  CALL AGSPOPB                                                        105
      GO TO BLOCK_SPHE, (5110 ,5120)                                      105
C End of Local Procedure                                                  105
C ----------------------------------------------------------------------  106
C ------------ TUBE                                                       106
C Begin Local Procedure TUBE                                              107
C TUBE A BOX                                                              107
5130  CONTINUE                                                            107
         AG_TITLE='TUBE'                                                  107
         AG_MODULE=MODULE                                                 107
         AG_MARK='V00'                                                    107
         CALL AGSPUSH                                                     107
         AG_MODULE=MODULE                                                 107
C    *                                                                    107
C    Geant ATTRIBUTE : TUBE ===> COLO=ICOLOR <===                         108
         CALL AGSRESET                                                    108
         AG_COLO = ICOLOR                                                 108
         AG_TITLE = 'TUBE'                                                108
         AG_EXNAME = 'ATTRIBUTE'                                          108
         AG_PARLIST = 'COLO'                                              108
         CALL AXATTRIBUTE                                                 108
C    *                                                                    108
         ICOLOR=ICOLOR+1                                                  108
C    Geant SHAPE : TUBE ===> RMIN=5.0 RMAX=10.0 DZ=15 <===                110
         CALL AGSRESET                                                    110
         AG_RMIN = 5.0                                                    110
         AG_RMAX = 10.0                                                   110
         AG_DZ = 15                                                       110
         AG_TITLE = 'TUBE'                                                110
         AG_EXNAME = 'SHAPE'                                              110
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      110
         CALL AXSHAPE                                                     110
C    *                                                                    110
5131  CALL AGSPOPB                                                        111
      GO TO BLOCK_TUBE, (5130 ,5140)                                      111
C End of Local Procedure                                                  111
C ----------------------------------------------------------------------  112
C ------------ TUBS                                                       112
C Begin Local Procedure TUBS                                              113
C TUBS A BOX                                                              113
5150  CONTINUE                                                            113
         AG_TITLE='TUBS'                                                  113
         AG_MODULE=MODULE                                                 113
         AG_MARK='V00'                                                    113
         CALL AGSPUSH                                                     113
         AG_MODULE=MODULE                                                 113
C    *                                                                    113
C    Geant ATTRIBUTE : TUBS ===> COLO=ICOLOR <===                         114
         CALL AGSRESET                                                    114
         AG_COLO = ICOLOR                                                 114
         AG_TITLE = 'TUBS'                                                114
         AG_EXNAME = 'ATTRIBUTE'                                          114
         AG_PARLIST = 'COLO'                                              114
         CALL AXATTRIBUTE                                                 114
C    *                                                                    114
         ICOLOR=ICOLOR+1                                                  114
C    Geant SHAPE : TUBS ===> RMIN=5.0 RMAX=10.0 PHI1=-120.0, PHI2=+120.0  117
C     DZ=15 <===                                                          117
         CALL AGSRESET                                                    117
         AG_RMIN = 5.0                                                    117
         AG_RMAX = 10.0                                                   117
         AG_PHI1 = -120.0                                                 117
         AG_PHI2 = +120.0                                                 117
         AG_DZ = 15                                                       117
         AG_TITLE = 'TUBS'                                                117
         AG_EXNAME = 'SHAPE'                                              117
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            117
         CALL AXSHAPE                                                     117
C    *                                                                    117
5151  CALL AGSPOPB                                                        118
      GO TO BLOCK_TUBS, (5150 ,5160)                                      118
C End of Local Procedure                                                  118
C ----------------------------------------------------------------------  119
C ------------ CTUB                                                       119
C Begin Local Procedure CTUB                                              120
C CTUB A BOX                                                              120
5170  CONTINUE                                                            120
         AG_TITLE='CTUB'                                                  120
         AG_MODULE=MODULE                                                 120
         AG_MARK='V00'                                                    120
         CALL AGSPUSH                                                     120
         AG_MODULE=MODULE                                                 120
C    *                                                                    120
C    Geant ATTRIBUTE : CTUB ===> COLO=ICOLOR <===                         121
         CALL AGSRESET                                                    121
         AG_COLO = ICOLOR                                                 121
         AG_TITLE = 'CTUB'                                                121
         AG_EXNAME = 'ATTRIBUTE'                                          121
         AG_PARLIST = 'COLO'                                              121
         CALL AXATTRIBUTE                                                 121
C    *                                                                    121
         ICOLOR=ICOLOR+1                                                  121
C    Geant SHAPE : CTUB ===> RMIN=5.0 RMAX=10.0 PHI1=-120.0, PHI2=+120.0  125
C     DZ=15 HX=1.0 HY=1.0 HZ=1.0 LX=0.0, LY=0.0 LZ=1.0 <===               125
         CALL AGSRESET                                                    125
         AG_RMIN = 5.0                                                    125
         AG_RMAX = 10.0                                                   125
         AG_PHI1 = -120.0                                                 125
         AG_PHI2 = +120.0                                                 125
         AG_DZ = 15                                                       125
         AG_HX = 1.0                                                      125
         AG_HY = 1.0                                                      125
         AG_HZ = 1.0                                                      125
         AG_LX = 0.0                                                      125
         AG_LY = 0.0                                                      125
         AG_LZ = 1.0                                                      125
         AG_TITLE = 'CTUB'                                                125
         AG_EXNAME = 'SHAPE'                                              125
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ_HX_HY_HZ_LX_LY_LZ'          125
         CALL AXSHAPE                                                     125
C    *                                                                    125
5171  CALL AGSPOPB                                                        126
      GO TO BLOCK_CTUB, (5170 ,5180)                                      126
C End of Local Procedure                                                  126
C ----------------------------------------------------------------------  127
C ------------ ELTU                                                       127
C Begin Local Procedure ELTU                                              128
C ELTU AN ELLIPTICAL TUBE TGEOELTU                                        128
5190  CONTINUE                                                            128
         AG_TITLE='ELTU'                                                  128
         AG_MODULE=MODULE                                                 128
         AG_MARK='V00'                                                    128
         CALL AGSPUSH                                                     128
         AG_MODULE=MODULE                                                 128
C    *                                                                    128
C    Geant ATTRIBUTE : ELTU ===> COLO=ICOLOR <===                         129
         CALL AGSRESET                                                    129
         AG_COLO = ICOLOR                                                 129
         AG_TITLE = 'ELTU'                                                129
         AG_EXNAME = 'ATTRIBUTE'                                          129
         AG_PARLIST = 'COLO'                                              129
         CALL AXATTRIBUTE                                                 129
C    *                                                                    129
         ICOLOR=ICOLOR+1                                                  129
C    Geant SHAPE : ELTU ===> P1=15.0 P2=10.0 DZ=30.0 <===                 131
         CALL AGSRESET                                                    131
         AG_P1 = 15.0                                                     131
         AG_P2 = 10.0                                                     131
         AG_DZ = 30.0                                                     131
         AG_TITLE = 'ELTU'                                                131
         AG_EXNAME = 'SHAPE'                                              131
         AG_PARLIST = 'P1_P2_DZ'                                          131
         CALL AXSHAPE                                                     131
C    *                                                                    131
5191  CALL AGSPOPB                                                        132
      GO TO BLOCK_ELTU, (5190 ,5200)                                      132
C End of Local Procedure                                                  132
C ----------------------------------------------------------------------  133
C ------------ CONE                                                       133
C Begin Local Procedure CONE                                              134
C CONE A BOX                                                              134
5210  CONTINUE                                                            134
         AG_TITLE='CONE'                                                  134
         AG_MODULE=MODULE                                                 134
         AG_MARK='V00'                                                    134
         CALL AGSPUSH                                                     134
         AG_MODULE=MODULE                                                 134
C    *                                                                    134
C    Geant ATTRIBUTE : CONE ===> COLO=ICOLOR <===                         135
         CALL AGSRESET                                                    135
         AG_COLO = ICOLOR                                                 135
         AG_TITLE = 'CONE'                                                135
         AG_EXNAME = 'ATTRIBUTE'                                          135
         AG_PARLIST = 'COLO'                                              135
         CALL AXATTRIBUTE                                                 135
C    *                                                                    135
         ICOLOR=ICOLOR+1                                                  135
C    Geant SHAPE : CONE ===> DZ=40 RMN1=5.0 RMX1=20.0 RMN2=5.0, RMX2=7.5  138
C     <===                                                                138
         CALL AGSRESET                                                    138
         AG_DZ = 40                                                       138
         AG_RMN1 = 5.0                                                    138
         AG_RMX1 = 20.0                                                   138
         AG_RMN2 = 5.0                                                    138
         AG_RMX2 = 7.5                                                    138
         AG_TITLE = 'CONE'                                                138
         AG_EXNAME = 'SHAPE'                                              138
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            138
         CALL AXSHAPE                                                     138
C    *                                                                    138
5211  CALL AGSPOPB                                                        139
      GO TO BLOCK_CONE, (5210 ,5220)                                      139
C End of Local Procedure                                                  139
C ----------------------------------------------------------------------  140
C ------------ CONS                                                       140
C Begin Local Procedure CONS                                              141
C CONS A BOX                                                              141
5230  CONTINUE                                                            141
         AG_TITLE='CONS'                                                  141
         AG_MODULE=MODULE                                                 141
         AG_MARK='V00'                                                    141
         CALL AGSPUSH                                                     141
         AG_MODULE=MODULE                                                 141
C    *                                                                    141
C    Geant ATTRIBUTE : CONS ===> COLO=ICOLOR <===                         142
         CALL AGSRESET                                                    142
         AG_COLO = ICOLOR                                                 142
         AG_TITLE = 'CONS'                                                142
         AG_EXNAME = 'ATTRIBUTE'                                          142
         AG_PARLIST = 'COLO'                                              142
         CALL AXATTRIBUTE                                                 142
C    *                                                                    142
         ICOLOR=ICOLOR+1                                                  142
C    Geant SHAPE : CONS ===> DZ=40.0 RMN1=5.0 RMX1=20.0 RMN2=5.0, RMX2=7  145
C    .5 PHI1=-120 PHI2=+120 <===                                          145
         CALL AGSRESET                                                    145
         AG_DZ = 40.0                                                     145
         AG_RMN1 = 5.0                                                    145
         AG_RMX1 = 20.0                                                   145
         AG_RMN2 = 5.0                                                    145
         AG_RMX2 = 7.5                                                    145
         AG_PHI1 = -120                                                   145
         AG_PHI2 = +120                                                   145
         AG_TITLE = 'CONS'                                                145
         AG_EXNAME = 'SHAPE'                                              145
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  145
         CALL AXSHAPE                                                     145
C    *                                                                    145
5231  CALL AGSPOPB                                                        146
      GO TO BLOCK_CONS, (5230 ,5240)                                      146
C End of Local Procedure                                                  146
C ----------------------------------------------------------------------  147
C ------------ PCON                                                       147
C Begin Local Procedure PCON                                              148
C PCON A POLYCONE                                                         148
5250  CONTINUE                                                            148
         AG_TITLE='PCON'                                                  148
         AG_MODULE=MODULE                                                 148
         AG_MARK='V00'                                                    148
         CALL AGSPUSH                                                     148
         AG_MODULE=MODULE                                                 148
C    *                                                                    148
C    Geant ATTRIBUTE : PCON ===> COLO=ICOLOR <===                         149
         CALL AGSRESET                                                    149
         AG_COLO = ICOLOR                                                 149
         AG_TITLE = 'PCON'                                                149
         AG_EXNAME = 'ATTRIBUTE'                                          149
         AG_PARLIST = 'COLO'                                              149
         CALL AXATTRIBUTE                                                 149
C    *                                                                    149
         ICOLOR=ICOLOR+1                                                  149
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=5 ZI={-20., -5.0,10.0,15  153
C    .0,20.0} RMN={0,0,0,0,0} RMX={15.0, 15.0,8.0,12.0,15.0} <===         153
         CALL AGSRESET                                                    153
         AG_PHI1 = 0                                                      153
         AG_DPHI = 360                                                    153
         AG_NZ = 5                                                        153
         AG_ZI(1)=-20.                                                    153
         AG_ZI(2)=-5.0                                                    153
         AG_ZI(3)=10.0                                                    153
         AG_ZI(4)=15.0                                                    153
         AG_ZI(5)=20.0                                                    153
         AG_RMN(1)=0                                                      153
         AG_RMN(2)=0                                                      153
         AG_RMN(3)=0                                                      153
         AG_RMN(4)=0                                                      153
         AG_RMN(5)=0                                                      153
         AG_RMX(1)=15.0                                                   153
         AG_RMX(2)=15.0                                                   153
         AG_RMX(3)=8.0                                                    153
         AG_RMX(4)=12.0                                                   153
         AG_RMX(5)=15.0                                                   153
         AG_TITLE = 'PCON'                                                153
         AG_EXNAME = 'SHAPE'                                              153
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           153
         CALL AXSHAPE                                                     153
C    *                                                                    153
5251  CALL AGSPOPB                                                        154
      GO TO BLOCK_PCON, (5250 ,5260)                                      154
C End of Local Procedure                                                  154
C ----------------------------------------------------------------------  155
C ------------ PGON                                                       155
C Begin Local Procedure PGON                                              156
C PGON A POLYGON                                                          156
5270  CONTINUE                                                            156
         AG_TITLE='PGON'                                                  156
         AG_MODULE=MODULE                                                 156
         AG_MARK='V00'                                                    156
         CALL AGSPUSH                                                     156
         AG_MODULE=MODULE                                                 156
C    *                                                                    156
C    Geant ATTRIBUTE : PGON ===> COLO=ICOLOR <===                         157
         CALL AGSRESET                                                    157
         AG_COLO = ICOLOR                                                 157
         AG_TITLE = 'PGON'                                                157
         AG_EXNAME = 'ATTRIBUTE'                                          157
         AG_PARLIST = 'COLO'                                              157
         CALL AXATTRIBUTE                                                 157
C    *                                                                    157
         ICOLOR=ICOLOR+1                                                  157
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=6 NZ=5, ZI={-20.,-5.0  161
C    ,10.0,15.0,20.0} RMN={12.0,12.0, 5.0,9.0,12.0} RMX={15.0,15.0,8.0,1  161
C    2.0,15.0} <===                                                       161
         CALL AGSRESET                                                    161
         AG_PHI1 = 0                                                      161
         AG_DPHI = 360                                                    161
         AG_NPDIV = 6                                                     161
         AG_NZ = 5                                                        161
         AG_ZI(1)=-20.                                                    161
         AG_ZI(2)=-5.0                                                    161
         AG_ZI(3)=10.0                                                    161
         AG_ZI(4)=15.0                                                    161
         AG_ZI(5)=20.0                                                    161
         AG_RMN(1)=12.0                                                   161
         AG_RMN(2)=12.0                                                   161
         AG_RMN(3)=5.0                                                    161
         AG_RMN(4)=9.0                                                    161
         AG_RMN(5)=12.0                                                   161
         AG_RMX(1)=15.0                                                   161
         AG_RMX(2)=15.0                                                   161
         AG_RMX(3)=8.0                                                    161
         AG_RMX(4)=12.0                                                   161
         AG_RMX(5)=15.0                                                   161
         AG_TITLE = 'PGON'                                                161
         AG_EXNAME = 'SHAPE'                                              161
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     161
         CALL AXSHAPE                                                     161
C    *                                                                    161
5271  CALL AGSPOPB                                                        162
      GO TO BLOCK_PGON, (5270 ,5280)                                      162
C End of Local Procedure                                                  162
      WRITE(LOUT,5310)'===>>  [End AgML Module ShapGeo]  <<==='           162
5310  FORMAT(A32,/,/)                                                     162
C MODULE SHAPGEO                                                          164
      END                                                                 164
