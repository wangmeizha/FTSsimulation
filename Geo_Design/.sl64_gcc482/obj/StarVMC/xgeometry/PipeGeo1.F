C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIPEGEO1                                                 1
C PIPEGEO1 BEAM PIPE IN Y2013 AND BEYOND                                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIPEGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'BEAM PIPE IN Y2013 AND BEYOND'/                      1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR AMILKAR QUINTERO                                                 1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'AMILKAR QUINTERO'/                            2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 29/JUN/2011                                                     2
      CHARACTER*40 CREATED /'29/JUN/2011'/                                3
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
      INTEGER BLOCK_PIPE                                                  4
      INTEGER BLOCK_PALS                                                  5
      INTEGER BLOCK_PALH                                                  5
      INTEGER BLOCK_PBES                                                  5
      REAL INCH/2.54/                                                     5
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
      INTEGER PIPV_CONFIG                                                 7
      EQUIVALENCE (BANK_PIPV(4),PIPV_CONFIG,RE_PIPV_CONFIG),              7
     *(NAMES_PIPV(2),NPIPV_CONFIG)                                        7
      EQUIVALENCE (COMMEN_PIPV(2),CPIPV_CONFIG),(MAP_PIPV(1,2),           7
     *MPIPV_CONFIG)                                                       7
      CHARACTER CPIPV_CONFIG*40/' '/                                      7
      CHARACTER NPIPV_CONFIG*12/'I.CONFIG'/                               7
      INTEGER MPIPV_CONFIG(2)/1,1/                                        7
      REAL RE_PIPV_CONFIG                                                 7
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
      REAL PIPG_VERSION                                                   8
      EQUIVALENCE (BANK_PIPG(3),PIPG_VERSION,RE_PIPG_VERSION),            8
     *(NAMES_PIPG(1),NPIPG_VERSION)                                       8
      EQUIVALENCE (COMMEN_PIPG(1),CPIPG_VERSION),(MAP_PIPG(1,1),          8
     *MPIPG_VERSION)                                                      8
      CHARACTER CPIPG_VERSION*40/' '/                                     8
      CHARACTER NPIPG_VERSION*12/'R.VERSION'/                             8
      INTEGER MPIPG_VERSION(2)/1,1/                                       8
      REAL RE_PIPG_VERSION                                                8
C -                                                                       8
      REAL PIPG_ZOFFSET                                                   8
      EQUIVALENCE (BANK_PIPG(4),PIPG_ZOFFSET,RE_PIPG_ZOFFSET),            8
     *(NAMES_PIPG(2),NPIPG_ZOFFSET)                                       8
      EQUIVALENCE (COMMEN_PIPG(2),CPIPG_ZOFFSET),(MAP_PIPG(1,2),          8
     *MPIPG_ZOFFSET)                                                      8
      CHARACTER CPIPG_ZOFFSET*40/' '/                                     8
      CHARACTER NPIPG_ZOFFSET*12/'R.ZOFFSET'/                             8
      INTEGER MPIPG_ZOFFSET(2)/1,1/                                       8
      REAL RE_PIPG_ZOFFSET                                                8
C -                                                                       8
      REAL PIPG_YOFFSET                                                   8
      EQUIVALENCE (BANK_PIPG(5),PIPG_YOFFSET,RE_PIPG_YOFFSET),            8
     *(NAMES_PIPG(3),NPIPG_YOFFSET)                                       8
      EQUIVALENCE (COMMEN_PIPG(3),CPIPG_YOFFSET),(MAP_PIPG(1,3),          8
     *MPIPG_YOFFSET)                                                      8
      CHARACTER CPIPG_YOFFSET*40/' '/                                     8
      CHARACTER NPIPG_YOFFSET*12/'R.YOFFSET'/                             8
      INTEGER MPIPG_YOFFSET(2)/1,1/                                       8
      REAL RE_PIPG_YOFFSET                                                8
C -                                                                       8
      REAL PIPG_XOFFSET                                                   8
      EQUIVALENCE (BANK_PIPG(6),PIPG_XOFFSET,RE_PIPG_XOFFSET),            8
     *(NAMES_PIPG(4),NPIPG_XOFFSET)                                       8
      EQUIVALENCE (COMMEN_PIPG(4),CPIPG_XOFFSET),(MAP_PIPG(1,4),          8
     *MPIPG_XOFFSET)                                                      8
      CHARACTER CPIPG_XOFFSET*40/' '/                                     8
      CHARACTER NPIPG_XOFFSET*12/'R.XOFFSET'/                             8
      INTEGER MPIPG_XOFFSET(2)/1,1/                                       8
      REAL RE_PIPG_XOFFSET                                                8
C -                                                                       8
      REAL PIPG_ZOFFBER                                                   8
      EQUIVALENCE (BANK_PIPG(7),PIPG_ZOFFBER,RE_PIPG_ZOFFBER),            8
     *(NAMES_PIPG(5),NPIPG_ZOFFBER)                                       8
      EQUIVALENCE (COMMEN_PIPG(5),CPIPG_ZOFFBER),(MAP_PIPG(1,5),          8
     *MPIPG_ZOFFBER)                                                      8
      CHARACTER CPIPG_ZOFFBER*40/' '/                                     8
      CHARACTER NPIPG_ZOFFBER*12/'R.ZOFFBER'/                             8
      INTEGER MPIPG_ZOFFBER(2)/1,1/                                       8
      REAL RE_PIPG_ZOFFBER                                                8
C -                                                                       8
      REAL PIPG_VACDENS                                                   8
      EQUIVALENCE (BANK_PIPG(8),PIPG_VACDENS,RE_PIPG_VACDENS),            8
     *(NAMES_PIPG(6),NPIPG_VACDENS)                                       8
      EQUIVALENCE (COMMEN_PIPG(6),CPIPG_VACDENS),(MAP_PIPG(1,6),          8
     *MPIPG_VACDENS)                                                      8
      CHARACTER CPIPG_VACDENS*40/' '/                                     8
      CHARACTER NPIPG_VACDENS*12/'R.VACDENS'/                             8
      INTEGER MPIPG_VACDENS(2)/1,1/                                       8
      REAL RE_PIPG_VACDENS                                                8
C -                                                                       8
      INTEGER LEN_PIPG,LLEN_PIPG,LINK_PIPG                                8
      PARAMETER (LEN_PIPG=8)                                              8
      PARAMETER (LLEN_PIPG=6)                                             8
      INTEGER FLAG_PIPG/0/,MAP_PIPG(2,LLEN_PIPG),IDNUM_PIPG(10)           8
      CHARACTER NAMES_PIPG(LLEN_PIPG)*12,COMMEN_PIPG(LLEN_PIPG)*40        8
      DATA IDNUM_PIPG/10*0/                                               8
      REAL BANK_PIPG(LEN_PIPG)                                            8
      CHARACTER*60 BPATH_PIPG/' '/                                        8
      SAVE FLAG_PIPG,MAP_PIPG,IDNUM_PIPG,NAMES_PIPG,COMMEN_PIPG,          8
     *BPATH_PIPG                                                          8
      DATA LINK_PIPG/0/,BANK_PIPG/LEN_PIPG*0/                             8
C *                                                                       8
C *                                                                       9
      CALL AGSBEGM(MODULE,IPRIN)                                          9
      IF (IPRIN.LT.0) RETURN                                              9
C Filling bank PIPV - PIPE VERSION                                        9
      BANK_TITLE='PIPE VERSION'                                           9
      BPATH_PIPV='PIPV*'                                                  9
      IDNUM_PIPV(1)=0                                                     10
C 2.0 VERSION OF THE BEAM PIPE                                            10
      PIPV_VERSION=1.0                                                    10
      CPIPV_VERSION='2.0 VERSION OF THE BEAM PIPE'                        10
C CONFIGURATION IS ONE                                                    11
      PIPV_CONFIG=1                                                       11
      CPIPV_CONFIG='CONFIGURATION IS ONE'                                 11
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    12
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           12
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   12
C *                                                                       12
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    12
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           12
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   12
C *                                                                       13
      CALL AGSBEGM(MODULE,IPRIN)                                          13
      IF (IPRIN.LT.0) RETURN                                              13
C Filling bank PIPG - PIPE GEOMETRY                                       13
      BANK_TITLE='PIPE GEOMETRY'                                          13
      BPATH_PIPG='PIPG*'                                                  13
      IDNUM_PIPG(1)=0                                                     14
C DEFAULT POSITION AND GEOMETRY                                           14
      PIPG_VERSION=1.0                                                    14
      CPIPG_VERSION='DEFAULT POSITION AND GEOMETRY'                       14
C VACUUM DENSITY... NEEDS TO BE VERIFIED                                  15
      PIPG_VACDENS=1.25E-8                                                15
      CPIPG_VACDENS='VACUUM DENSITY... NEEDS TO BE VERIFIED'              15
C DEFAULT X POSITION                                                      16
      PIPG_XOFFSET=0.0                                                    16
      CPIPG_XOFFSET='DEFAULT X POSITION'                                  16
C DEFAULT Y POSITION                                                      17
      PIPG_YOFFSET=0.0                                                    17
      CPIPG_YOFFSET='DEFAULT Y POSITION'                                  17
C DEFAULT Z POSITION                                                      18
      PIPG_ZOFFSET=0.0                                                    18
      CPIPG_ZOFFSET='DEFAULT Z POSITION'                                  18
C OFFSET OF BERYLLIUM AS PER DRAWING                                      19
      PIPG_ZOFFBER=-15.75*INCH                                            19
      CPIPG_ZOFFBER='OFFSET OF BERYLLIUM AS PER DRAWING'                  19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    20
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           20
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    20
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           20
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   20
      CALL AGSBEGM(MODULE,IPRIN)                                          21
C Check IPRIN<0                                                           21
      IF (IPRIN.LT.0) RETURN                                              21
C Create PIPE :                                                           21
C Call To Local Procedure PIPE                                            21
      ASSIGN 5020 TO BLOCK_PIPE                                           21
      GO TO  5010                                                         21
5020  CONTINUE                                                            21
C *                                                                       21
C Geant POSITION : PIPE ===> MOTHER='CAVE' X=PIPG_XOFFSET Y=PIPG_YOFFSET  22
C  Z=PIPG_ZOFFSET ALPHAX=180 <===                                         22
      CALL AGSRESET                                                       22
      AG_MOTHER = 'CAVE'                                                  22
      AG_X = PIPG_XOFFSET                                                 22
      AG_Y = PIPG_YOFFSET                                                 22
      AG_Z = PIPG_ZOFFSET                                                 22
      AG_ALPHAN=AG_ALPHAN+1                                               22
      AG_ALPHAK(AG_ALPHAN)=1                                              22
      AG_ALPHAL(AG_ALPHAN)=180                                            22
      AG_TITLE = 'PIPE'                                                   22
      AG_EXNAME = 'POSITION'                                              22
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  22
      CALL AXPOSITION                                                     22
C *                                                                       22
C ----------------------------------------------------------------------  22
C ------------ PIPE                                                       22
      CALL AGSENDM                                                        24
      RETURN                                                              24
C Begin Local Procedure PIPE                                              24
C PIPE PIPE MOTHER VOLUME                                                 24
5010  CONTINUE                                                            24
         AG_TITLE='PIPE'                                                  24
         AG_MODULE=MODULE                                                 24
         AG_MARK='V00'                                                    24
         CALL AGSPUSH                                                     24
         AG_MODULE=MODULE                                                 24
C    *                                                                    24
C    Geant MATERIAL : AIR ===> inherited arguments used <===              25
         CALL AGSRESET                                                    25
         AG_TITLE = 'AIR'                                                 25
         AG_EXNAME = 'MATERIAL'                                           25
         AG_PARLIST = 'NONE'                                              25
         CALL AXMATERIAL                                                  25
C    *                                                                    25
C    Geant ATTRIBUTE : PIPE ===> SEEN=0 <===                              25
         CALL AGSRESET                                                    26
         AG_SEEN = 0                                                      26
         AG_TITLE = 'PIPE'                                                26
         AG_EXNAME = 'ATTRIBUTE'                                          26
         AG_PARLIST = 'SEEN'                                              26
         CALL AXATTRIBUTE                                                 26
C    *                                                                    26
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=14 ZI={-55.71*INCH, -54.  26
C    71*INCH,-54.71*INCH,-43.71*INCH, -43.71*INCH,-15.75*INCH,-15.75*INC  26
C    H,31.5*INCH, 31.5*INCH,43.72*INCH,43.72*INCH,54.71*INCH,54.71*INCH,  26
C     55.71*INCH} RMN={0,0,0,0,0,0,0,0,0, 0,0,0,0,0} RMX={0.7875*INCH+0.  26
C    5875*INCH,0.7875*INCH+0.5875*INCH, 0.7875*INCH+0.065*INCH,0.7875*IN  26
C    CH+0.065*INCH, 0.7875*INCH+0.055*INCH,0.7875*INCH+0.055*INCH, 0.787  26
C    5*INCH+0.030*INCH,0.7875*INCH+0.030*INCH, 0.7875*INCH+0.055*INCH, 0  26
C    .7875*INCH+0.055*INCH,0.7875*INCH+0.065*INCH, 0.7875*INCH+0.065*INC  26
C    H,0.7875*INCH+0.5875*INCH, 0.7875*INCH+0.5875*INCH} <===             26
         CALL AGSRESET                                                    39
         AG_PHI1 = 0                                                      39
         AG_DPHI = 360                                                    39
         AG_NZ = 14                                                       39
         AG_ZI(1)=-55.71*INCH                                             39
         AG_ZI(2)=-54.71*INCH                                             39
         AG_ZI(3)=-54.71*INCH                                             39
         AG_ZI(4)=-43.71*INCH                                             39
         AG_ZI(5)=-43.71*INCH                                             39
         AG_ZI(6)=-15.75*INCH                                             39
         AG_ZI(7)=-15.75*INCH                                             39
         AG_ZI(8)=31.5*INCH                                               39
         AG_ZI(9)=31.5*INCH                                               39
         AG_ZI(10)=43.72*INCH                                             39
         AG_ZI(11)=43.72*INCH                                             39
         AG_ZI(12)=54.71*INCH                                             39
         AG_ZI(13)=54.71*INCH                                             39
         AG_ZI(14)=55.71*INCH                                             39
         AG_RMN(1)=0                                                      39
         AG_RMN(2)=0                                                      39
         AG_RMN(3)=0                                                      39
         AG_RMN(4)=0                                                      39
         AG_RMN(5)=0                                                      39
         AG_RMN(6)=0                                                      39
         AG_RMN(7)=0                                                      39
         AG_RMN(8)=0                                                      39
         AG_RMN(9)=0                                                      39
         AG_RMN(10)=0                                                     39
         AG_RMN(11)=0                                                     39
         AG_RMN(12)=0                                                     39
         AG_RMN(13)=0                                                     39
         AG_RMN(14)=0                                                     39
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                39
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                39
         AG_RMX(3)=0.7875*INCH+0.065*INCH                                 39
         AG_RMX(4)=0.7875*INCH+0.065*INCH                                 39
         AG_RMX(5)=0.7875*INCH+0.055*INCH                                 39
         AG_RMX(6)=0.7875*INCH+0.055*INCH                                 39
         AG_RMX(7)=0.7875*INCH+0.030*INCH                                 39
         AG_RMX(8)=0.7875*INCH+0.030*INCH                                 39
         AG_RMX(9)=0.7875*INCH+0.055*INCH                                 39
         AG_RMX(10)=0.7875*INCH+0.055*INCH                                39
         AG_RMX(11)=0.7875*INCH+0.065*INCH                                39
         AG_RMX(12)=0.7875*INCH+0.065*INCH                                39
         AG_RMX(13)=0.7875*INCH+0.5875*INCH                               39
         AG_RMX(14)=0.7875*INCH+0.5875*INCH                               39
         AG_TITLE = 'PCON'                                                39
         AG_EXNAME = 'SHAPE'                                              39
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           39
         CALL AXSHAPE                                                     39
C    *                                                                    39
C    Create PALS :                                                        40
C    Call To Local Procedure PALS                                         40
         ASSIGN 5040 TO BLOCK_PALS                                        40
         GO TO  5030                                                      40
5040     CONTINUE                                                         40
C    *                                                                    40
C    Geant POSITION : PALS ===> MOTHER='PIPE' KONLY='MANY' <===           41
         CALL AGSRESET                                                    41
         AG_MOTHER = 'PIPE'                                               41
         AG_KONLY = 'MANY'                                                41
         AG_TITLE = 'PALS'                                                41
         AG_EXNAME = 'POSITION'                                           41
         AG_PARLIST = 'MOTHER_KONLY'                                      41
         CALL AXPOSITION                                                  41
C    *                                                                    41
C    Create PBES :                                                        42
C    Call To Local Procedure PBES                                         42
         ASSIGN 5060 TO BLOCK_PBES                                        42
         GO TO  5050                                                      42
5060     CONTINUE                                                         42
C    *                                                                    42
C    Geant POSITION : PBES ===> MOTHER='PIPE' Z=PIPG_ZOFFBER KONLY='ONLY  43
C    *' <===                                                              43
         CALL AGSRESET                                                    43
         AG_MOTHER = 'PIPE'                                               43
         AG_Z = PIPG_ZOFFBER                                              43
         AG_KONLY = 'ONLY'                                                43
         AG_TITLE = 'PBES'                                                43
         AG_EXNAME = 'POSITION'                                           43
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    43
         CALL AXPOSITION                                                  43
C    *                                                                    43
5011  CALL AGSPOPB                                                        44
      GO TO BLOCK_PIPE, (5010 ,5020)                                      44
C End of Local Procedure                                                  44
C ----------------------------------------------------------------------  45
C ------------ PALS                                                       45
C Begin Local Procedure PALS                                              46
C PALS THE ALUMINIUM SECTION OF THE BEAM PIPE                             46
5030  CONTINUE                                                            46
         AG_TITLE='PALS'                                                  46
         AG_MODULE=MODULE                                                 46
         AG_MARK='V00'                                                    46
         CALL AGSPUSH                                                     46
         AG_MODULE=MODULE                                                 46
C    *                                                                    46
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        47
         CALL AGSRESET                                                    47
         AG_TITLE = 'ALUMINIUM'                                           47
         AG_EXNAME = 'MATERIAL'                                           47
         AG_PARLIST = 'NONE'                                              47
         CALL AXMATERIAL                                                  47
C    *                                                                    47
C    Geant ATTRIBUTE : PALS ===> SEEN=1 COLO=4 <===                       47
         CALL AGSRESET                                                    48
         AG_SEEN = 1                                                      48
         AG_COLO = 4                                                      48
         AG_TITLE = 'PALS'                                                48
         AG_EXNAME = 'ATTRIBUTE'                                          48
         AG_PARLIST = 'SEEN_COLO'                                         48
         CALL AXATTRIBUTE                                                 48
C    *                                                                    48
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=14 ZI={-55.71*INCH, -54.  48
C    71*INCH,-54.71*INCH,-43.71*INCH, -43.71*INCH,-15.75*INCH,-15.75*INC  48
C    H,31.5*INCH, 31.5*INCH,43.72*INCH,43.72*INCH,54.71*INCH,54.71*INCH,  48
C     55.71*INCH} RMN={0.7875*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH,  48
C    0.7875*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH, 0.787  48
C    5*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH, 0.7875*INCH} RMX={0.787  48
C    5*INCH+0.5875*INCH, 0.7875*INCH+0.5875*INCH,0.7875*INCH+0.065*INCH,  48
C     0.7875*INCH+0.065*INCH,0.7875*INCH+0.055*INCH, 0.7875*INCH+0.055*I  48
C    NCH,0.7875*INCH+0.030*INCH, 0.7875*INCH+0.030*INCH,0.7875*INCH+0.05  48
C    5*INCH, 0.7875*INCH+0.055*INCH,0.7875*INCH+0.065*INCH, 0.7875*INCH+  48
C    0.065*INCH,0.7875*INCH+0.5875*INCH, 0.7875*INCH+0.5875*INCH} <===    48
         CALL AGSRESET                                                    64
         AG_PHI1 = 0                                                      64
         AG_DPHI = 360                                                    64
         AG_NZ = 14                                                       64
         AG_ZI(1)=-55.71*INCH                                             64
         AG_ZI(2)=-54.71*INCH                                             64
         AG_ZI(3)=-54.71*INCH                                             64
         AG_ZI(4)=-43.71*INCH                                             64
         AG_ZI(5)=-43.71*INCH                                             64
         AG_ZI(6)=-15.75*INCH                                             64
         AG_ZI(7)=-15.75*INCH                                             64
         AG_ZI(8)=31.5*INCH                                               64
         AG_ZI(9)=31.5*INCH                                               64
         AG_ZI(10)=43.72*INCH                                             64
         AG_ZI(11)=43.72*INCH                                             64
         AG_ZI(12)=54.71*INCH                                             64
         AG_ZI(13)=54.71*INCH                                             64
         AG_ZI(14)=55.71*INCH                                             64
         AG_RMN(1)=0.7875*INCH                                            64
         AG_RMN(2)=0.7875*INCH                                            64
         AG_RMN(3)=0.7875*INCH                                            64
         AG_RMN(4)=0.7875*INCH                                            64
         AG_RMN(5)=0.7875*INCH                                            64
         AG_RMN(6)=0.7875*INCH                                            64
         AG_RMN(7)=0.7875*INCH                                            64
         AG_RMN(8)=0.7875*INCH                                            64
         AG_RMN(9)=0.7875*INCH                                            64
         AG_RMN(10)=0.7875*INCH                                           64
         AG_RMN(11)=0.7875*INCH                                           64
         AG_RMN(12)=0.7875*INCH                                           64
         AG_RMN(13)=0.7875*INCH                                           64
         AG_RMN(14)=0.7875*INCH                                           64
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                64
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                64
         AG_RMX(3)=0.7875*INCH+0.065*INCH                                 64
         AG_RMX(4)=0.7875*INCH+0.065*INCH                                 64
         AG_RMX(5)=0.7875*INCH+0.055*INCH                                 64
         AG_RMX(6)=0.7875*INCH+0.055*INCH                                 64
         AG_RMX(7)=0.7875*INCH+0.030*INCH                                 64
         AG_RMX(8)=0.7875*INCH+0.030*INCH                                 64
         AG_RMX(9)=0.7875*INCH+0.055*INCH                                 64
         AG_RMX(10)=0.7875*INCH+0.055*INCH                                64
         AG_RMX(11)=0.7875*INCH+0.065*INCH                                64
         AG_RMX(12)=0.7875*INCH+0.065*INCH                                64
         AG_RMX(13)=0.7875*INCH+0.5875*INCH                               64
         AG_RMX(14)=0.7875*INCH+0.5875*INCH                               64
         AG_TITLE = 'PCON'                                                64
         AG_EXNAME = 'SHAPE'                                              64
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           64
         CALL AXSHAPE                                                     64
C    *                                                                    64
C    Create PALH :                                                        65
C    Call To Local Procedure PALH                                         65
         ASSIGN 5080 TO BLOCK_PALH                                        65
         GO TO  5070                                                      65
5080     CONTINUE                                                         65
C    *                                                                    65
C    Geant POSITION : PALH ===> MOTHER='PIPE' <===                        66
         CALL AGSRESET                                                    66
         AG_MOTHER = 'PIPE'                                               66
         AG_TITLE = 'PALH'                                                66
         AG_EXNAME = 'POSITION'                                           66
         AG_PARLIST = 'MOTHER'                                            66
         CALL AXPOSITION                                                  66
C    *                                                                    66
5031  CALL AGSPOPB                                                        67
      GO TO BLOCK_PALS, (5030 ,5040)                                      67
C End of Local Procedure                                                  67
C ----------------------------------------------------------------------  68
C ------------ PALH                                                       68
C Begin Local Procedure PALH                                              69
C PALH THE HOLE IN THE AL BEAM PIPE                                       69
5070  CONTINUE                                                            69
         AG_TITLE='PALH'                                                  69
         AG_MODULE=MODULE                                                 69
         AG_MARK='V00'                                                    69
         CALL AGSPUSH                                                     69
         AG_MODULE=MODULE                                                 69
C    *                                                                    69
C    Geant MATERIAL : AIR ===> inherited arguments used <===              70
         CALL AGSRESET                                                    70
         AG_TITLE = 'AIR'                                                 70
         AG_EXNAME = 'MATERIAL'                                           70
         AG_PARLIST = 'NONE'                                              70
         CALL AXMATERIAL                                                  70
C    *                                                                    70
C    Geant MATERIAL : VACUUM ===> DENS=PIPG_VACDENS <===                  70
         CALL AGSRESET                                                    71
         AG_DENS = PIPG_VACDENS                                           71
         AG_TITLE = 'VACUUM'                                              71
         AG_EXNAME = 'MATERIAL'                                           71
         AG_PARLIST = 'DENS'                                              71
         CALL AXMATERIAL                                                  71
C    *                                                                    71
C    Geant ATTRIBUTE : PALH ===> SEEN=0 <===                              71
         CALL AGSRESET                                                    72
         AG_SEEN = 0                                                      72
         AG_TITLE = 'PALH'                                                72
         AG_EXNAME = 'ATTRIBUTE'                                          72
         AG_PARLIST = 'SEEN'                                              72
         CALL AXATTRIBUTE                                                 72
C    *                                                                    72
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=14 ZI={-55.71*INCH, -54.  72
C    71*INCH,-54.71*INCH,-43.71*INCH, -43.71*INCH,-15.75*INCH,-15.75*INC  72
C    H,31.5*INCH, 31.5*INCH,43.72*INCH,43.72*INCH,54.71*INCH,54.71*INCH,  72
C     55.71*INCH} RMN={0,0,0,0,0,0,0,0,0, 0,0,0,0,0} RMX={0.7875*INCH,0.  72
C    7875*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH,0.7875*I  72
C    NCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH,0  72
C    .7875*INCH,0.7875*INCH} <===                                         72
         CALL AGSRESET                                                    81
         AG_PHI1 = 0                                                      81
         AG_DPHI = 360                                                    81
         AG_NZ = 14                                                       81
         AG_ZI(1)=-55.71*INCH                                             81
         AG_ZI(2)=-54.71*INCH                                             81
         AG_ZI(3)=-54.71*INCH                                             81
         AG_ZI(4)=-43.71*INCH                                             81
         AG_ZI(5)=-43.71*INCH                                             81
         AG_ZI(6)=-15.75*INCH                                             81
         AG_ZI(7)=-15.75*INCH                                             81
         AG_ZI(8)=31.5*INCH                                               81
         AG_ZI(9)=31.5*INCH                                               81
         AG_ZI(10)=43.72*INCH                                             81
         AG_ZI(11)=43.72*INCH                                             81
         AG_ZI(12)=54.71*INCH                                             81
         AG_ZI(13)=54.71*INCH                                             81
         AG_ZI(14)=55.71*INCH                                             81
         AG_RMN(1)=0                                                      81
         AG_RMN(2)=0                                                      81
         AG_RMN(3)=0                                                      81
         AG_RMN(4)=0                                                      81
         AG_RMN(5)=0                                                      81
         AG_RMN(6)=0                                                      81
         AG_RMN(7)=0                                                      81
         AG_RMN(8)=0                                                      81
         AG_RMN(9)=0                                                      81
         AG_RMN(10)=0                                                     81
         AG_RMN(11)=0                                                     81
         AG_RMN(12)=0                                                     81
         AG_RMN(13)=0                                                     81
         AG_RMN(14)=0                                                     81
         AG_RMX(1)=0.7875*INCH                                            81
         AG_RMX(2)=0.7875*INCH                                            81
         AG_RMX(3)=0.7875*INCH                                            81
         AG_RMX(4)=0.7875*INCH                                            81
         AG_RMX(5)=0.7875*INCH                                            81
         AG_RMX(6)=0.7875*INCH                                            81
         AG_RMX(7)=0.7875*INCH                                            81
         AG_RMX(8)=0.7875*INCH                                            81
         AG_RMX(9)=0.7875*INCH                                            81
         AG_RMX(10)=0.7875*INCH                                           81
         AG_RMX(11)=0.7875*INCH                                           81
         AG_RMX(12)=0.7875*INCH                                           81
         AG_RMX(13)=0.7875*INCH                                           81
         AG_RMX(14)=0.7875*INCH                                           81
         AG_TITLE = 'PCON'                                                81
         AG_EXNAME = 'SHAPE'                                              81
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           81
         CALL AXSHAPE                                                     81
C    *                                                                    81
5071  CALL AGSPOPB                                                        82
      GO TO BLOCK_PALH, (5070 ,5080)                                      82
C End of Local Procedure                                                  82
C ----------------------------------------------------------------------  83
C ------------ PBES                                                       83
C Begin Local Procedure PBES                                              84
C PBES THE BERILLIUM SECTION OF THE BEAM PIPE                             84
5050  CONTINUE                                                            84
         AG_TITLE='PBES'                                                  84
         AG_MODULE=MODULE                                                 84
         AG_MARK='V00'                                                    84
         CALL AGSPUSH                                                     84
         AG_MODULE=MODULE                                                 84
C    *                                                                    84
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        85
         CALL AGSRESET                                                    85
         AG_TITLE = 'BERILLIUM'                                           85
         AG_EXNAME = 'MATERIAL'                                           85
         AG_PARLIST = 'NONE'                                              85
         CALL AXMATERIAL                                                  85
C    *                                                                    85
C    Geant ATTRIBUTE : PBES ===> SEEN=0 COLO=0 <===                       85
         CALL AGSRESET                                                    86
         AG_SEEN = 0                                                      86
         AG_COLO = 0                                                      86
         AG_TITLE = 'PBES'                                                86
         AG_EXNAME = 'ATTRIBUTE'                                          86
         AG_PARLIST = 'SEEN_COLO'                                         86
         CALL AXATTRIBUTE                                                 86
C    *                                                                    86
C    Geant SHAPE : TUBE ===> RMIN=0.7875*INCH RMAX=0.7875*INCH+0.030*INC  86
C    H, DZ=47.25*INCH/2 <===                                              86
         CALL AGSRESET                                                    88
         AG_RMIN = 0.7875*INCH                                            88
         AG_RMAX = 0.7875*INCH+0.030*INCH                                 88
         AG_DZ = 47.25*INCH/2                                             88
         AG_TITLE = 'TUBE'                                                88
         AG_EXNAME = 'SHAPE'                                              88
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      88
         CALL AXSHAPE                                                     88
C    *                                                                    88
5051  CALL AGSPOPB                                                        89
      GO TO BLOCK_PBES, (5050 ,5060)                                      89
C End of Local Procedure                                                  89
      WRITE(LOUT,5090)'===>>  [End AgML Module PipeGeo1]  <<==='          89
5090  FORMAT(A32,/,/)                                                     89
C MODULE PIPEGEO1                                                         91
      END                                                                 91
