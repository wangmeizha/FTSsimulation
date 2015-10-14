C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TUTRGEO3                                                 1
C TUTRGEO3 TUTORIAL GEOMETRY 1                                            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TUTRGEO3'/, BANK_TITLE/'*'/                            1
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
      REAL *4 BOXDX/10.0/, BOXDY/10.0/, BOXDZ/10.0/                       6
C -                                                                       8
      INTEGER TUBG_VERSION                                                8
      EQUIVALENCE (BANK_TUBG(3),TUBG_VERSION,RE_TUBG_VERSION),            8
     *(NAMES_TUBG(1),NTUBG_VERSION)                                       8
      EQUIVALENCE (COMMEN_TUBG(1),CTUBG_VERSION),(MAP_TUBG(1,1),          8
     *MTUBG_VERSION)                                                      8
      CHARACTER CTUBG_VERSION*40/' '/                                     8
      CHARACTER NTUBG_VERSION*12/'I.VERSION'/                             8
      INTEGER MTUBG_VERSION(2)/1,1/                                       8
      REAL RE_TUBG_VERSION                                                8
C -                                                                       8
      REAL TUBG_RMIN                                                      8
      EQUIVALENCE (BANK_TUBG(4),TUBG_RMIN,RE_TUBG_RMIN),(NAMES_TUBG(2),   8
     *NTUBG_RMIN)                                                         8
      EQUIVALENCE (COMMEN_TUBG(2),CTUBG_RMIN),(MAP_TUBG(1,2),             8
     *MTUBG_RMIN)                                                         8
      CHARACTER CTUBG_RMIN*40/' '/                                        8
      CHARACTER NTUBG_RMIN*12/'R.RMIN'/                                   8
      INTEGER MTUBG_RMIN(2)/1,1/                                          8
      REAL RE_TUBG_RMIN                                                   8
C -                                                                       8
      REAL TUBG_RMAX                                                      8
      EQUIVALENCE (BANK_TUBG(5),TUBG_RMAX,RE_TUBG_RMAX),(NAMES_TUBG(3),   8
     *NTUBG_RMAX)                                                         8
      EQUIVALENCE (COMMEN_TUBG(3),CTUBG_RMAX),(MAP_TUBG(1,3),             8
     *MTUBG_RMAX)                                                         8
      CHARACTER CTUBG_RMAX*40/' '/                                        8
      CHARACTER NTUBG_RMAX*12/'R.RMAX'/                                   8
      INTEGER MTUBG_RMAX(2)/1,1/                                          8
      REAL RE_TUBG_RMAX                                                   8
C -                                                                       8
      REAL TUBG_DZ                                                        8
      EQUIVALENCE (BANK_TUBG(6),TUBG_DZ,RE_TUBG_DZ),(NAMES_TUBG(4),       8
     *NTUBG_DZ)                                                           8
      EQUIVALENCE (COMMEN_TUBG(4),CTUBG_DZ),(MAP_TUBG(1,4),MTUBG_DZ)      8
      CHARACTER CTUBG_DZ*40/' '/                                          8
      CHARACTER NTUBG_DZ*12/'R.DZ'/                                       8
      INTEGER MTUBG_DZ(2)/1,1/                                            8
      REAL RE_TUBG_DZ                                                     8
C -                                                                       8
      INTEGER LEN_TUBG,LLEN_TUBG,LINK_TUBG                                8
      PARAMETER (LEN_TUBG=6)                                              8
      PARAMETER (LLEN_TUBG=4)                                             8
      INTEGER FLAG_TUBG/0/,MAP_TUBG(2,LLEN_TUBG),IDNUM_TUBG(10)           8
      CHARACTER NAMES_TUBG(LLEN_TUBG)*12,COMMEN_TUBG(LLEN_TUBG)*40        8
      DATA IDNUM_TUBG/10*0/                                               8
      REAL BANK_TUBG(LEN_TUBG)                                            8
      CHARACTER*60 BPATH_TUBG/' '/                                        8
      SAVE FLAG_TUBG,MAP_TUBG,IDNUM_TUBG,NAMES_TUBG,COMMEN_TUBG,          8
     *BPATH_TUBG                                                          8
      DATA LINK_TUBG/0/,BANK_TUBG/LEN_TUBG*0/                             8
C *                                                                       8
C *                                                                       9
      CALL AGSBEGM(MODULE,IPRIN)                                          9
      IF (IPRIN.LT.0) RETURN                                              9
C Filling bank TUBG - DIMENSIONS OF THE FIRST TUBE                        9
      BANK_TITLE='DIMENSIONS OF THE FIRST TUBE'                           9
      BPATH_TUBG='TUBG*'                                                  9
      IDNUM_TUBG(1)=0                                                     10
C 1ST TUBE                                                                10
      TUBG_VERSION=1                                                      10
      CTUBG_VERSION='1ST TUBE'                                            10
C NO HOLE                                                                 11
      TUBG_RMIN=0                                                         11
      CTUBG_RMIN='NO HOLE'                                                11
C 5CM                                                                     12
      TUBG_RMAX=5                                                         12
      CTUBG_RMAX='5CM'                                                    12
C 10CM                                                                    13
      TUBG_DZ=10                                                          13
      CTUBG_DZ='10CM'                                                     13
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TUBG',    14
     *BPATH_TUBG,IDNUM_TUBG,1, MAP_TUBG,NAMES_TUBG,COMMEN_TUBG,           14
     *BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                   14
C *                                                                       14
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TUBG',    14
     *BPATH_TUBG,IDNUM_TUBG,1, MAP_TUBG,NAMES_TUBG,COMMEN_TUBG,           14
     *BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                   14
C *                                                                       15
      CALL AGSBEGM(MODULE,IPRIN)                                          15
      IF (IPRIN.LT.0) RETURN                                              15
C Filling bank TUBG - DIMENSIONS OF THE STUBD TUBE                        15
      BANK_TITLE='DIMENSIONS OF THE STUBD TUBE'                           15
      BPATH_TUBG='TUBG*'                                                  15
      IDNUM_TUBG(1)=0                                                     16
C 2ND TUBE                                                                16
      TUBG_VERSION=2                                                      16
      CTUBG_VERSION='2ND TUBE'                                            16
C A HOLE                                                                  17
      TUBG_RMIN=2.5                                                       17
      CTUBG_RMIN='A HOLE'                                                 17
C 7.5CM                                                                   18
      TUBG_RMAX=7.5                                                       18
      CTUBG_RMAX='7.5CM'                                                  18
C 10CM                                                                    19
      TUBG_DZ=10                                                          19
      CTUBG_DZ='10CM'                                                     19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TUBG',    20
     *BPATH_TUBG,IDNUM_TUBG,1, MAP_TUBG,NAMES_TUBG,COMMEN_TUBG,           20
     *BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TUBG',    20
     *BPATH_TUBG,IDNUM_TUBG,1, MAP_TUBG,NAMES_TUBG,COMMEN_TUBG,           20
     *BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                   20
C *                                                                       21
      CALL AGSBEGM(MODULE,IPRIN)                                          21
      IF (IPRIN.LT.0) RETURN                                              21
C Filling bank TUBG - DIMENSIONS OF THE THIRD TUBE                        21
      BANK_TITLE='DIMENSIONS OF THE THIRD TUBE'                           21
      BPATH_TUBG='TUBG*'                                                  21
      IDNUM_TUBG(1)=0                                                     22
C 3RD TUBE                                                                22
      TUBG_VERSION=2                                                      22
      CTUBG_VERSION='3RD TUBE'                                            22
C A HOLE                                                                  23
      TUBG_RMIN=5.0                                                       23
      CTUBG_RMIN='A HOLE'                                                 23
C 10CM                                                                    24
      TUBG_RMAX=10.0                                                      24
      CTUBG_RMAX='10CM'                                                   24
C 15CM                                                                    25
      TUBG_DZ=15                                                          25
      CTUBG_DZ='15CM'                                                     25
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TUBG',    26
     *BPATH_TUBG,IDNUM_TUBG,1, MAP_TUBG,NAMES_TUBG,COMMEN_TUBG,           26
     *BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                   26
C *                                                                       26
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TUBG',    26
     *BPATH_TUBG,IDNUM_TUBG,1, MAP_TUBG,NAMES_TUBG,COMMEN_TUBG,           26
     *BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                   26
      CALL AGSBEGM(MODULE,IPRIN)                                          27
C Check IPRIN<0                                                           27
      IF (IPRIN.LT.0) RETURN                                              27
C Create TUTR :                                                           27
C Call To Local Procedure TUTR                                            27
      ASSIGN 5020 TO BLOCK_TUTR                                           27
      GO TO  5010                                                         27
5020  CONTINUE                                                            27
C *                                                                       27
C Geant POSITION : TUTR ===> MOTHER='CAVE' <===                           28
      CALL AGSRESET                                                       28
      AG_MOTHER = 'CAVE'                                                  28
      AG_TITLE = 'TUTR'                                                   28
      AG_EXNAME = 'POSITION'                                              28
      AG_PARLIST = 'MOTHER'                                               28
      CALL AXPOSITION                                                     28
C *                                                                       28
C ----------------------------------------------------------------------  28
C ------------ TUTR                                                       28
      CALL AGSENDM                                                        30
      RETURN                                                              30
C Begin Local Procedure TUTR                                              30
C TUTR MAIN VOLUME IN THE AGML TUTORIAL GEOMETRY                          30
5010  CONTINUE                                                            30
         AG_TITLE='TUTR'                                                  30
         AG_MODULE=MODULE                                                 30
         AG_MARK='V00'                                                    30
         CALL AGSPUSH                                                     30
         AG_MODULE=MODULE                                                 30
C    *                                                                    30
C    Geant MATERIAL : AIR ===> inherited arguments used <===              31
         CALL AGSRESET                                                    31
         AG_TITLE = 'AIR'                                                 31
         AG_EXNAME = 'MATERIAL'                                           31
         AG_PARLIST = 'NONE'                                              31
         CALL AXMATERIAL                                                  31
C    *                                                                    31
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           31
         CALL AGSRESET                                                    32
         AG_TITLE = 'STANDARD'                                            32
         AG_EXNAME = 'MEDIUM'                                             32
         AG_PARLIST = 'NONE'                                              32
         CALL AXMEDIUM                                                    32
C    *                                                                    32
C    Geant ATTRIBUTE : NONE ===> SEEN=0 <===                              32
         CALL AGSRESET                                                    33
         AG_SEEN = 0                                                      33
         AG_TITLE = 'NONE'                                                33
         AG_EXNAME = 'ATTRIBUTE'                                          33
         AG_PARLIST = 'SEEN'                                              33
         CALL AXATTRIBUTE                                                 33
C    *                                                                    33
C    Geant SHAPE : BOX ===> DX=500.0 DY=500.0 DZ=500.0 <===               33
         CALL AGSRESET                                                    34
         AG_DX = 500.0                                                    34
         AG_DY = 500.0                                                    34
         AG_DZ = 500.0                                                    34
         AG_TITLE = 'BOX'                                                 34
         AG_EXNAME = 'SHAPE'                                              34
         AG_PARLIST = 'DX_DY_DZ'                                          34
         CALL AXSHAPE                                                     34
C    *                                                                    34
C    Create ABOX :                                                        35
C    Call To Local Procedure ABOX                                         35
         ASSIGN 5040 TO BLOCK_ABOX                                        35
         GO TO  5030                                                      35
5040     CONTINUE                                                         35
C    *                                                                    35
C    Geant POSITION : ABOX ===> inherited arguments used <===             36
         CALL AGSRESET                                                    36
         AG_TITLE = 'ABOX'                                                36
         AG_EXNAME = 'POSITION'                                           36
         AG_PARLIST = 'NONE'                                              36
         CALL AXPOSITION                                                  36
C    *                                                                    36
         BOXDY=15.0                                                       36
         BOXDZ= 5.0                                                       37
C    Create ABOX :                                                        39
C    Call To Local Procedure ABOX                                         39
         ASSIGN 5050 TO BLOCK_ABOX                                        39
         GO TO  5030                                                      39
5050     CONTINUE                                                         39
C    *                                                                    39
C    Geant POSITION : ABOX ===> X=+25.0 <===                              40
         CALL AGSRESET                                                    40
         AG_X = +25.0                                                     40
         AG_TITLE = 'ABOX'                                                40
         AG_EXNAME = 'POSITION'                                           40
         AG_PARLIST = 'X'                                                 40
         CALL AXPOSITION                                                  40
C    *                                                                    40
         BOXDY=5.0                                                        40
         BOXDZ=5.0                                                        41
C    Create ABOX :                                                        43
C    Call To Local Procedure ABOX                                         43
         ASSIGN 5060 TO BLOCK_ABOX                                        43
         GO TO  5030                                                      43
5060     CONTINUE                                                         43
C    *                                                                    43
C    Geant POSITION : ABOX ===> X=+50.0 <===                              44
         CALL AGSRESET                                                    44
         AG_X = +50.0                                                     44
         AG_TITLE = 'ABOX'                                                44
         AG_EXNAME = 'POSITION'                                           44
         AG_PARLIST = 'X'                                                 44
         CALL AXPOSITION                                                  44
C    *                                                                    44
         BOXDY=15.0                                                       44
         BOXDZ= 5.0                                                       45
C    Create ABOX :                                                        47
C    Call To Local Procedure ABOX                                         47
         ASSIGN 5070 TO BLOCK_ABOX                                        47
         GO TO  5030                                                      47
5070     CONTINUE                                                         47
C    *                                                                    47
C    Geant POSITION : ABOX ===> X=-25.0 <===                              48
         CALL AGSRESET                                                    48
         AG_X = -25.0                                                     48
         AG_TITLE = 'ABOX'                                                48
         AG_EXNAME = 'POSITION'                                           48
         AG_PARLIST = 'X'                                                 48
         CALL AXPOSITION                                                  48
C    *                                                                    48
         BOXDY=5.0                                                        48
         BOXDZ=5.0                                                        49
C    Create ABOX :                                                        51
C    Call To Local Procedure ABOX                                         51
         ASSIGN 5080 TO BLOCK_ABOX                                        51
         GO TO  5030                                                      51
5080     CONTINUE                                                         51
C    *                                                                    51
C    Geant POSITION : ABOX ===> X=-50.0 <===                              52
         CALL AGSRESET                                                    52
         AG_X = -50.0                                                     52
         AG_TITLE = 'ABOX'                                                52
         AG_EXNAME = 'POSITION'                                           52
         AG_PARLIST = 'X'                                                 52
         CALL AXPOSITION                                                  52
C    *                                                                    52
C    use TUBG bank                                                        52
         BPATH_TUBG='TUBG*'                                               53
         IDNUM_TUBG(1)=0                                                  53
         TUBG_VERSION=1                                                   53
         CALL AGDGETP('TUTRGEO3','*',-999,'TUBG',NTUBG_VERSION,           53
     *   RE_TUBG_VERSION,BPATH_TUBG,IDNUM_TUBG,1,MAP_TUBG,NAMES_TUBG,     53
     *   BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                53
C    Create ATUB :                                                        54
C    Call To Local Procedure ATUB                                         54
         ASSIGN 5100 TO BLOCK_ATUB                                        54
         GO TO  5090                                                      54
5100     CONTINUE                                                         54
C    *                                                                    54
C    Geant POSITION : ATUB ===> Y=-25.0 <===                              55
         CALL AGSRESET                                                    55
         AG_Y = -25.0                                                     55
         AG_TITLE = 'ATUB'                                                55
         AG_EXNAME = 'POSITION'                                           55
         AG_PARLIST = 'Y'                                                 55
         CALL AXPOSITION                                                  55
C    *                                                                    55
C    use TUBG bank                                                        55
         BPATH_TUBG='TUBG*'                                               56
         IDNUM_TUBG(1)=0                                                  56
         TUBG_VERSION=2                                                   56
         CALL AGDGETP('TUTRGEO3','*',-999,'TUBG',NTUBG_VERSION,           56
     *   RE_TUBG_VERSION,BPATH_TUBG,IDNUM_TUBG,1,MAP_TUBG,NAMES_TUBG,     56
     *   BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                56
C    Create ATUB :                                                        57
C    Call To Local Procedure ATUB                                         57
         ASSIGN 5110 TO BLOCK_ATUB                                        57
         GO TO  5090                                                      57
5110     CONTINUE                                                         57
C    *                                                                    57
C    Geant POSITION : ATUB ===> X=+25 Y=-25.0 <===                        58
         CALL AGSRESET                                                    58
         AG_X = +25                                                       58
         AG_Y = -25.0                                                     58
         AG_TITLE = 'ATUB'                                                58
         AG_EXNAME = 'POSITION'                                           58
         AG_PARLIST = 'X_Y'                                               58
         CALL AXPOSITION                                                  58
C    *                                                                    58
C    Geant POSITION : ATUB ===> X=-25 Y=-25.0 <===                        58
         CALL AGSRESET                                                    59
         AG_X = -25                                                       59
         AG_Y = -25.0                                                     59
         AG_TITLE = 'ATUB'                                                59
         AG_EXNAME = 'POSITION'                                           59
         AG_PARLIST = 'X_Y'                                               59
         CALL AXPOSITION                                                  59
C    *                                                                    59
C    use TUBG bank                                                        59
         BPATH_TUBG='TUBG*'                                               60
         IDNUM_TUBG(1)=0                                                  60
         TUBG_VERSION=3                                                   60
         CALL AGDGETP('TUTRGEO3','*',-999,'TUBG',NTUBG_VERSION,           60
     *   RE_TUBG_VERSION,BPATH_TUBG,IDNUM_TUBG,1,MAP_TUBG,NAMES_TUBG,     60
     *   BANK_TUBG,LEN_TUBG,LLEN_TUBG,LINK_TUBG,FLAG_TUBG)                60
C    Create ATUB :                                                        61
C    Call To Local Procedure ATUB                                         61
         ASSIGN 5120 TO BLOCK_ATUB                                        61
         GO TO  5090                                                      61
5120     CONTINUE                                                         61
C    *                                                                    61
C    Geant POSITION : ATUB ===> X=+50 Y=-25.0 <===                        62
         CALL AGSRESET                                                    62
         AG_X = +50                                                       62
         AG_Y = -25.0                                                     62
         AG_TITLE = 'ATUB'                                                62
         AG_EXNAME = 'POSITION'                                           62
         AG_PARLIST = 'X_Y'                                               62
         CALL AXPOSITION                                                  62
C    *                                                                    62
C    Geant POSITION : ATUB ===> X=-50 Y=-25.0 <===                        62
         CALL AGSRESET                                                    63
         AG_X = -50                                                       63
         AG_Y = -25.0                                                     63
         AG_TITLE = 'ATUB'                                                63
         AG_EXNAME = 'POSITION'                                           63
         AG_PARLIST = 'X_Y'                                               63
         CALL AXPOSITION                                                  63
C    *                                                                    63
5011  CALL AGSPOPB                                                        64
      GO TO BLOCK_TUTR, (5010 ,5020)                                      64
C End of Local Procedure                                                  64
C ----------------------------------------------------------------------  65
C ------------ ABOX                                                       65
C Begin Local Procedure ABOX                                              66
C ABOX A VOLUME WHICH IS A BOX IN THE TUTORIAL                            66
5030  CONTINUE                                                            66
         AG_TITLE='ABOX'                                                  66
         AG_MODULE=MODULE                                                 66
         AG_MARK='V00'                                                    66
         CALL AGSPUSH                                                     66
         AG_MODULE=MODULE                                                 66
C    *                                                                    66
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       67
         CALL AGSRESET                                                    67
         AG_SEEN = 1                                                      67
         AG_COLO = 4                                                      67
         AG_TITLE = 'NONE'                                                67
         AG_EXNAME = 'ATTRIBUTE'                                          67
         AG_PARLIST = 'SEEN_COLO'                                         67
         CALL AXATTRIBUTE                                                 67
C    *                                                                    67
C    Geant SHAPE : BOX ===> DX=BOXDX DY=BOXDY DZ=BOXDZ <===               67
         CALL AGSRESET                                                    68
         AG_DX = BOXDX                                                    68
         AG_DY = BOXDY                                                    68
         AG_DZ = BOXDZ                                                    68
         AG_TITLE = 'BOX'                                                 68
         AG_EXNAME = 'SHAPE'                                              68
         AG_PARLIST = 'DX_DY_DZ'                                          68
         CALL AXSHAPE                                                     68
C    *                                                                    68
5031  CALL AGSPOPB                                                        69
      GO TO BLOCK_ABOX, (5030 ,5040,5050,5060,5070,5080)                  69
C End of Local Procedure                                                  69
C ----------------------------------------------------------------------  70
C ------------ ATUB                                                       70
C Begin Local Procedure ATUB                                              71
C ATUB A VOLUME WHICH IS A TUBE IN THE TUTORIAL                           71
5090  CONTINUE                                                            71
         AG_TITLE='ATUB'                                                  71
         AG_MODULE=MODULE                                                 71
         AG_MARK='V00'                                                    71
         CALL AGSPUSH                                                     71
         AG_MODULE=MODULE                                                 71
C    *                                                                    71
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=5 <===                       72
         CALL AGSRESET                                                    72
         AG_SEEN = 1                                                      72
         AG_COLO = 5                                                      72
         AG_TITLE = 'NONE'                                                72
         AG_EXNAME = 'ATTRIBUTE'                                          72
         AG_PARLIST = 'SEEN_COLO'                                         72
         CALL AXATTRIBUTE                                                 72
C    *                                                                    72
C    Geant SHAPE : TUBE ===> RMIN=TUBG_RMIN RMAX=TUBG_RMAX, DZ=TUBG_DZ <  72
C    ===                                                                  72
         CALL AGSRESET                                                    74
         AG_RMIN = TUBG_RMIN                                              74
         AG_RMAX = TUBG_RMAX                                              74
         AG_DZ = TUBG_DZ                                                  74
         AG_TITLE = 'TUBE'                                                74
         AG_EXNAME = 'SHAPE'                                              74
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      74
         CALL AXSHAPE                                                     74
C    *                                                                    74
5091  CALL AGSPOPB                                                        75
      GO TO BLOCK_ATUB, (5090 ,5100,5110,5120)                            75
C End of Local Procedure                                                  75
      WRITE(LOUT,5130)'===>>  [End AgML Module TutrGeo3]  <<==='          75
5130  FORMAT(A32,/,/)                                                     75
C MODULE TUTRGEO3                                                         77
      END                                                                 77
