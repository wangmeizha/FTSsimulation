C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TUTRGEO1                                                 1
C TUTRGEO1 TUTORIAL GEOMETRY 1                                            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TUTRGEO1'/, BANK_TITLE/'*'/                            1
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
5011  CALL AGSPOPB                                                        16
      GO TO BLOCK_TUTR, (5010 ,5020)                                      16
C End of Local Procedure                                                  16
C ----------------------------------------------------------------------  17
C ------------ ABOX                                                       17
C Begin Local Procedure ABOX                                              18
C ABOX A VOLUME WHICH IS A BOX IN THE TUTORIAL                            18
5030  CONTINUE                                                            18
         AG_TITLE='ABOX'                                                  18
         AG_MODULE=MODULE                                                 18
         AG_MARK='V00'                                                    18
         CALL AGSPUSH                                                     18
         AG_MODULE=MODULE                                                 18
C    *                                                                    18
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       19
         CALL AGSRESET                                                    19
         AG_SEEN = 1                                                      19
         AG_COLO = 4                                                      19
         AG_TITLE = 'NONE'                                                19
         AG_EXNAME = 'ATTRIBUTE'                                          19
         AG_PARLIST = 'SEEN_COLO'                                         19
         CALL AXATTRIBUTE                                                 19
C    *                                                                    19
C    Geant SHAPE : BOX ===> DX=10 DY=10 DZ=10 <===                        19
         CALL AGSRESET                                                    20
         AG_DX = 10                                                       20
         AG_DY = 10                                                       20
         AG_DZ = 10                                                       20
         AG_TITLE = 'BOX'                                                 20
         AG_EXNAME = 'SHAPE'                                              20
         AG_PARLIST = 'DX_DY_DZ'                                          20
         CALL AXSHAPE                                                     20
C    *                                                                    20
5031  CALL AGSPOPB                                                        21
      GO TO BLOCK_ABOX, (5030 ,5040)                                      21
C End of Local Procedure                                                  21
      WRITE(LOUT,5050)'===>>  [End AgML Module TutrGeo1]  <<==='          21
5050  FORMAT(A32,/,/)                                                     21
C MODULE TUTRGEO1                                                         23
      END                                                                 23
