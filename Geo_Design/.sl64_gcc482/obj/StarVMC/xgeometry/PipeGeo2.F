C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIPEGEO2                                                 2
C PIPEGEO2 COMPLETE BEAM PIPE IN Y2013 AND BEYOND. USING FIVE SEPARATED,  2
C  SECTIONS. MIDDLE SECTION AL-BE-AL                                      2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'PIPEGEO2'/, BANK_TITLE/'*'/                            2
      DATA MCOMMENT/'COMPLETE BEAM PIPE IN Y2013 AND BEYOND. USING FIVE   2
     *SEPARATED, SECTIONS. MIDDLE SECTION AL-BE-AL'/                      2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C AUTHOR AMILKAR QUINTERO                                                 2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 AUTHOR /'AMILKAR QUINTERO'/                            3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C CREATED 11/JAN/2013                                                     3
      CHARACTER*40 CREATED /'11/JAN/2013'/                                4
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
      INTEGER BLOCK_PIPE                                                  5
      INTEGER BLOCK_PALH                                                  14
      INTEGER BLOCK_PALQ                                                  14
      INTEGER BLOCK_PALR                                                  14
      INTEGER BLOCK_PALS                                                  14
      INTEGER BLOCK_PBES                                                  14
      INTEGER BLOCK_PALI                                                  14
      INTEGER BLOCK_PALJ                                                  14
      INTEGER BLOCK_PALK                                                  14
      REAL INCH/2.54/                                                     14
C -                                                                       16
      REAL PIPV_VERSION                                                   16
      EQUIVALENCE (BANK_PIPV(3),PIPV_VERSION,RE_PIPV_VERSION),            16
     *(NAMES_PIPV(1),NPIPV_VERSION)                                       16
      EQUIVALENCE (COMMEN_PIPV(1),CPIPV_VERSION),(MAP_PIPV(1,1),          16
     *MPIPV_VERSION)                                                      16
      CHARACTER CPIPV_VERSION*40/' '/                                     16
      CHARACTER NPIPV_VERSION*12/'R.VERSION'/                             16
      INTEGER MPIPV_VERSION(2)/1,1/                                       16
      REAL RE_PIPV_VERSION                                                16
C -                                                                       16
      INTEGER PIPV_CONFIG                                                 16
      EQUIVALENCE (BANK_PIPV(4),PIPV_CONFIG,RE_PIPV_CONFIG),              16
     *(NAMES_PIPV(2),NPIPV_CONFIG)                                        16
      EQUIVALENCE (COMMEN_PIPV(2),CPIPV_CONFIG),(MAP_PIPV(1,2),           16
     *MPIPV_CONFIG)                                                       16
      CHARACTER CPIPV_CONFIG*40/' '/                                      16
      CHARACTER NPIPV_CONFIG*12/'I.CONFIG'/                               16
      INTEGER MPIPV_CONFIG(2)/1,1/                                        16
      REAL RE_PIPV_CONFIG                                                 16
C -                                                                       16
      INTEGER LEN_PIPV,LLEN_PIPV,LINK_PIPV                                16
      PARAMETER (LEN_PIPV=4)                                              16
      PARAMETER (LLEN_PIPV=2)                                             16
      INTEGER FLAG_PIPV/0/,MAP_PIPV(2,LLEN_PIPV),IDNUM_PIPV(10)           16
      CHARACTER NAMES_PIPV(LLEN_PIPV)*12,COMMEN_PIPV(LLEN_PIPV)*40        16
      DATA IDNUM_PIPV/10*0/                                               16
      REAL BANK_PIPV(LEN_PIPV)                                            16
      CHARACTER*60 BPATH_PIPV/' '/                                        16
      SAVE FLAG_PIPV,MAP_PIPV,IDNUM_PIPV,NAMES_PIPV,COMMEN_PIPV,          16
     *BPATH_PIPV                                                          16
      DATA LINK_PIPV/0/,BANK_PIPV/LEN_PIPV*0/                             16
C *                                                                       16
C -                                                                       17
      REAL PIPG_VERSION                                                   17
      EQUIVALENCE (BANK_PIPG(3),PIPG_VERSION,RE_PIPG_VERSION),            17
     *(NAMES_PIPG(1),NPIPG_VERSION)                                       17
      EQUIVALENCE (COMMEN_PIPG(1),CPIPG_VERSION),(MAP_PIPG(1,1),          17
     *MPIPG_VERSION)                                                      17
      CHARACTER CPIPG_VERSION*40/' '/                                     17
      CHARACTER NPIPG_VERSION*12/'R.VERSION'/                             17
      INTEGER MPIPG_VERSION(2)/1,1/                                       17
      REAL RE_PIPG_VERSION                                                17
C -                                                                       17
      REAL PIPG_ZOFFSET                                                   17
      EQUIVALENCE (BANK_PIPG(4),PIPG_ZOFFSET,RE_PIPG_ZOFFSET),            17
     *(NAMES_PIPG(2),NPIPG_ZOFFSET)                                       17
      EQUIVALENCE (COMMEN_PIPG(2),CPIPG_ZOFFSET),(MAP_PIPG(1,2),          17
     *MPIPG_ZOFFSET)                                                      17
      CHARACTER CPIPG_ZOFFSET*40/' '/                                     17
      CHARACTER NPIPG_ZOFFSET*12/'R.ZOFFSET'/                             17
      INTEGER MPIPG_ZOFFSET(2)/1,1/                                       17
      REAL RE_PIPG_ZOFFSET                                                17
C -                                                                       17
      REAL PIPG_YOFFSET                                                   17
      EQUIVALENCE (BANK_PIPG(5),PIPG_YOFFSET,RE_PIPG_YOFFSET),            17
     *(NAMES_PIPG(3),NPIPG_YOFFSET)                                       17
      EQUIVALENCE (COMMEN_PIPG(3),CPIPG_YOFFSET),(MAP_PIPG(1,3),          17
     *MPIPG_YOFFSET)                                                      17
      CHARACTER CPIPG_YOFFSET*40/' '/                                     17
      CHARACTER NPIPG_YOFFSET*12/'R.YOFFSET'/                             17
      INTEGER MPIPG_YOFFSET(2)/1,1/                                       17
      REAL RE_PIPG_YOFFSET                                                17
C -                                                                       17
      REAL PIPG_XOFFSET                                                   17
      EQUIVALENCE (BANK_PIPG(6),PIPG_XOFFSET,RE_PIPG_XOFFSET),            17
     *(NAMES_PIPG(4),NPIPG_XOFFSET)                                       17
      EQUIVALENCE (COMMEN_PIPG(4),CPIPG_XOFFSET),(MAP_PIPG(1,4),          17
     *MPIPG_XOFFSET)                                                      17
      CHARACTER CPIPG_XOFFSET*40/' '/                                     17
      CHARACTER NPIPG_XOFFSET*12/'R.XOFFSET'/                             17
      INTEGER MPIPG_XOFFSET(2)/1,1/                                       17
      REAL RE_PIPG_XOFFSET                                                17
C -                                                                       17
      REAL PIPG_ZOFFBER                                                   17
      EQUIVALENCE (BANK_PIPG(7),PIPG_ZOFFBER,RE_PIPG_ZOFFBER),            17
     *(NAMES_PIPG(5),NPIPG_ZOFFBER)                                       17
      EQUIVALENCE (COMMEN_PIPG(5),CPIPG_ZOFFBER),(MAP_PIPG(1,5),          17
     *MPIPG_ZOFFBER)                                                      17
      CHARACTER CPIPG_ZOFFBER*40/' '/                                     17
      CHARACTER NPIPG_ZOFFBER*12/'R.ZOFFBER'/                             17
      INTEGER MPIPG_ZOFFBER(2)/1,1/                                       17
      REAL RE_PIPG_ZOFFBER                                                17
C -                                                                       17
      REAL PIPG_VACDENS                                                   17
      EQUIVALENCE (BANK_PIPG(8),PIPG_VACDENS,RE_PIPG_VACDENS),            17
     *(NAMES_PIPG(6),NPIPG_VACDENS)                                       17
      EQUIVALENCE (COMMEN_PIPG(6),CPIPG_VACDENS),(MAP_PIPG(1,6),          17
     *MPIPG_VACDENS)                                                      17
      CHARACTER CPIPG_VACDENS*40/' '/                                     17
      CHARACTER NPIPG_VACDENS*12/'R.VACDENS'/                             17
      INTEGER MPIPG_VACDENS(2)/1,1/                                       17
      REAL RE_PIPG_VACDENS                                                17
C -                                                                       17
      INTEGER LEN_PIPG,LLEN_PIPG,LINK_PIPG                                17
      PARAMETER (LEN_PIPG=8)                                              17
      PARAMETER (LLEN_PIPG=6)                                             17
      INTEGER FLAG_PIPG/0/,MAP_PIPG(2,LLEN_PIPG),IDNUM_PIPG(10)           17
      CHARACTER NAMES_PIPG(LLEN_PIPG)*12,COMMEN_PIPG(LLEN_PIPG)*40        17
      DATA IDNUM_PIPG/10*0/                                               17
      REAL BANK_PIPG(LEN_PIPG)                                            17
      CHARACTER*60 BPATH_PIPG/' '/                                        17
      SAVE FLAG_PIPG,MAP_PIPG,IDNUM_PIPG,NAMES_PIPG,COMMEN_PIPG,          17
     *BPATH_PIPG                                                          17
      DATA LINK_PIPG/0/,BANK_PIPG/LEN_PIPG*0/                             17
C *                                                                       17
C *                                                                       18
      CALL AGSBEGM(MODULE,IPRIN)                                          18
      IF (IPRIN.LT.0) RETURN                                              18
C Filling bank PIPV - PIPE VERSION                                        18
      BANK_TITLE='PIPE VERSION'                                           18
      BPATH_PIPV='PIPV*'                                                  18
      IDNUM_PIPV(1)=0                                                     19
C 3.0 VERSION OF THE BEAM PIPE                                            19
      PIPV_VERSION=3.0                                                    19
      CPIPV_VERSION='3.0 VERSION OF THE BEAM PIPE'                        19
C CONFIGURATION IS THREE                                                  20
      PIPV_CONFIG=3                                                       20
      CPIPV_CONFIG='CONFIGURATION IS THREE'                               20
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    21
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           21
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   21
C *                                                                       21
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    21
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           21
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   21
C *                                                                       22
      CALL AGSBEGM(MODULE,IPRIN)                                          22
      IF (IPRIN.LT.0) RETURN                                              22
C Filling bank PIPG - PIPE GEOMETRY                                       22
      BANK_TITLE='PIPE GEOMETRY'                                          22
      BPATH_PIPG='PIPG*'                                                  22
      IDNUM_PIPG(1)=0                                                     23
C DEFAULT POSITION AND GEOMETRY                                           23
      PIPG_VERSION=3.0                                                    23
      CPIPG_VERSION='DEFAULT POSITION AND GEOMETRY'                       23
C VACCUUM DENSITY... NEEDS TO BE VERIFIED                                 24
      PIPG_VACDENS=1.0E-5                                                 24
      CPIPG_VACDENS='VACCUUM DENSITY... NEEDS TO BE VERIFIED'             24
C DEFAULT X POSITION                                                      25
      PIPG_XOFFSET=0.0                                                    25
      CPIPG_XOFFSET='DEFAULT X POSITION'                                  25
C DEFAULT Y POSITION                                                      26
      PIPG_YOFFSET=0.0                                                    26
      CPIPG_YOFFSET='DEFAULT Y POSITION'                                  26
C DEFAULT Z POSITION                                                      27
      PIPG_ZOFFSET=0.0                                                    27
      CPIPG_ZOFFSET='DEFAULT Z POSITION'                                  27
C OFFSET OF BERYLLIUM AS PER DRAWING                                      28
      PIPG_ZOFFBER=7.875*INCH                                             28
      CPIPG_ZOFFBER='OFFSET OF BERYLLIUM AS PER DRAWING'                  28
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    29
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           29
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   29
C *                                                                       29
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    29
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           29
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   29
      CALL AGSBEGM(MODULE,IPRIN)                                          30
C Check IPRIN<0                                                           30
      IF (IPRIN.LT.0) RETURN                                              30
C Create PIPE :                                                           30
C Call To Local Procedure PIPE                                            30
      ASSIGN 5020 TO BLOCK_PIPE                                           30
      GO TO  5010                                                         30
5020  CONTINUE                                                            30
C *                                                                       30
C Geant POSITION : PIPE ===> MOTHER='CAVE' X=PIPG_XOFFSET Y=PIPG_YOFFSET  31
C  Z=PIPG_ZOFFSET KONLY='MANY' <===                                       31
      CALL AGSRESET                                                       31
      AG_MOTHER = 'CAVE'                                                  31
      AG_X = PIPG_XOFFSET                                                 31
      AG_Y = PIPG_YOFFSET                                                 31
      AG_Z = PIPG_ZOFFSET                                                 31
      AG_KONLY = 'MANY'                                                   31
      AG_TITLE = 'PIPE'                                                   31
      AG_EXNAME = 'POSITION'                                              31
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   31
      CALL AXPOSITION                                                     31
C *                                                                       31
C ----------------------------------------------------------------------  31
C ------------ PIPE                                                       31
      CALL AGSENDM                                                        33
      RETURN                                                              33
C Begin Local Procedure PIPE                                              33
C PIPE PIPE MOTHER VOLUME                                                 33
5010  CONTINUE                                                            33
         AG_TITLE='PIPE'                                                  33
         AG_MODULE=MODULE                                                 33
         AG_MARK='V00'                                                    33
         CALL AGSPUSH                                                     33
         AG_MODULE=MODULE                                                 33
C    *                                                                    33
C    Geant MATERIAL : AIR ===> inherited arguments used <===              34
         CALL AGSRESET                                                    34
         AG_TITLE = 'AIR'                                                 34
         AG_EXNAME = 'MATERIAL'                                           34
         AG_PARLIST = 'NONE'                                              34
         CALL AXMATERIAL                                                  34
C    *                                                                    34
C    Geant ATTRIBUTE : PIPE ===> SEEN=0 <===                              34
         CALL AGSRESET                                                    35
         AG_SEEN = 0                                                      35
         AG_TITLE = 'PIPE'                                                35
         AG_EXNAME = 'ATTRIBUTE'                                          35
         AG_PARLIST = 'SEEN'                                              35
         CALL AXATTRIBUTE                                                 35
C    *                                                                    35
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=10 ZI={-174.75*INCH, -79  35
C    .71*INCH,-64.71*INCH,-55.71*INCH, -55.71*INCH,55.71*INCH,55.71*INCH  35
C    ,64.71*INCH, 79.71*INCH,159.000*INCH} RMN={0,0,0,0,0,0,0, 0,0,0} RM  35
C    X={1.5*INCH+0.8125*INCH,1.5*INCH+0.8125*INCH, 0.7875*INCH+0.5775*IN  35
C    CH,0.7875*INCH+0.5775*INCH, 0.7875*INCH+0.065*INCH,0.7875*INCH+0.06  35
C    5*INCH, 0.7875*INCH+0.5775*INCH,0.7875*INCH+0.5775*INCH, 1.5*INCH+0  35
C    .8125*INCH,1.5*INCH+0.8125*INCH} <===                                35
         CALL AGSRESET                                                    44
         AG_PHI1 = 0                                                      44
         AG_DPHI = 360                                                    44
         AG_NZ = 10                                                       44
         AG_ZI(1)=-174.75*INCH                                            44
         AG_ZI(2)=-79.71*INCH                                             44
         AG_ZI(3)=-64.71*INCH                                             44
         AG_ZI(4)=-55.71*INCH                                             44
         AG_ZI(5)=-55.71*INCH                                             44
         AG_ZI(6)=55.71*INCH                                              44
         AG_ZI(7)=55.71*INCH                                              44
         AG_ZI(8)=64.71*INCH                                              44
         AG_ZI(9)=79.71*INCH                                              44
         AG_ZI(10)=159.000*INCH                                           44
         AG_RMN(1)=0                                                      44
         AG_RMN(2)=0                                                      44
         AG_RMN(3)=0                                                      44
         AG_RMN(4)=0                                                      44
         AG_RMN(5)=0                                                      44
         AG_RMN(6)=0                                                      44
         AG_RMN(7)=0                                                      44
         AG_RMN(8)=0                                                      44
         AG_RMN(9)=0                                                      44
         AG_RMN(10)=0                                                     44
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   44
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   44
         AG_RMX(3)=0.7875*INCH+0.5775*INCH                                44
         AG_RMX(4)=0.7875*INCH+0.5775*INCH                                44
         AG_RMX(5)=0.7875*INCH+0.065*INCH                                 44
         AG_RMX(6)=0.7875*INCH+0.065*INCH                                 44
         AG_RMX(7)=0.7875*INCH+0.5775*INCH                                44
         AG_RMX(8)=0.7875*INCH+0.5775*INCH                                44
         AG_RMX(9)=1.5*INCH+0.8125*INCH                                   44
         AG_RMX(10)=1.5*INCH+0.8125*INCH                                  44
         AG_TITLE = 'PCON'                                                44
         AG_EXNAME = 'SHAPE'                                              44
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           44
         CALL AXSHAPE                                                     44
C    *                                                                    44
C    Create PALH :                                                        45
C    Call To Local Procedure PALH                                         45
         ASSIGN 5040 TO BLOCK_PALH                                        45
         GO TO  5030                                                      45
5040     CONTINUE                                                         45
C    *                                                                    45
C    Geant POSITION : PALH ===> MOTHER='PIPE' KONLY='ONLY' <===           46
         CALL AGSRESET                                                    46
         AG_MOTHER = 'PIPE'                                               46
         AG_KONLY = 'ONLY'                                                46
         AG_TITLE = 'PALH'                                                46
         AG_EXNAME = 'POSITION'                                           46
         AG_PARLIST = 'MOTHER_KONLY'                                      46
         CALL AXPOSITION                                                  46
C    *                                                                    46
C    Create PALQ :                                                        47
C    Call To Local Procedure PALQ                                         47
         ASSIGN 5060 TO BLOCK_PALQ                                        47
         GO TO  5050                                                      47
5060     CONTINUE                                                         47
C    *                                                                    47
C    Geant POSITION : PALQ ===> MOTHER='PIPE' KONLY='ONLY' <===           48
         CALL AGSRESET                                                    48
         AG_MOTHER = 'PIPE'                                               48
         AG_KONLY = 'ONLY'                                                48
         AG_TITLE = 'PALQ'                                                48
         AG_EXNAME = 'POSITION'                                           48
         AG_PARLIST = 'MOTHER_KONLY'                                      48
         CALL AXPOSITION                                                  48
C    *                                                                    48
C    Create PALR :                                                        49
C    Call To Local Procedure PALR                                         49
         ASSIGN 5080 TO BLOCK_PALR                                        49
         GO TO  5070                                                      49
5080     CONTINUE                                                         49
C    *                                                                    49
C    Geant POSITION : PALR ===> MOTHER='PIPE' KONLY='ONLY' <===           50
         CALL AGSRESET                                                    50
         AG_MOTHER = 'PIPE'                                               50
         AG_KONLY = 'ONLY'                                                50
         AG_TITLE = 'PALR'                                                50
         AG_EXNAME = 'POSITION'                                           50
         AG_PARLIST = 'MOTHER_KONLY'                                      50
         CALL AXPOSITION                                                  50
C    *                                                                    50
C    Create PALS :                                                        51
C    Call To Local Procedure PALS                                         51
         ASSIGN 5100 TO BLOCK_PALS                                        51
         GO TO  5090                                                      51
5100     CONTINUE                                                         51
C    *                                                                    51
C    Geant POSITION : PALS ===> MOTHER='PIPE' KONLY='ONLY' <===           52
         CALL AGSRESET                                                    52
         AG_MOTHER = 'PIPE'                                               52
         AG_KONLY = 'ONLY'                                                52
         AG_TITLE = 'PALS'                                                52
         AG_EXNAME = 'POSITION'                                           52
         AG_PARLIST = 'MOTHER_KONLY'                                      52
         CALL AXPOSITION                                                  52
C    *                                                                    52
C    Create PBES :                                                        53
C    Call To Local Procedure PBES                                         53
         ASSIGN 5120 TO BLOCK_PBES                                        53
         GO TO  5110                                                      53
5120     CONTINUE                                                         53
C    *                                                                    53
C    Geant POSITION : PBES ===> MOTHER='PIPE' Z=PIPG_ZOFFBER KONLY='ONLY  54
C    *' <===                                                              54
         CALL AGSRESET                                                    54
         AG_MOTHER = 'PIPE'                                               54
         AG_Z = PIPG_ZOFFBER                                              54
         AG_KONLY = 'ONLY'                                                54
         AG_TITLE = 'PBES'                                                54
         AG_EXNAME = 'POSITION'                                           54
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    54
         CALL AXPOSITION                                                  54
C    *                                                                    54
C    Create PALI :                                                        55
C    Call To Local Procedure PALI                                         55
         ASSIGN 5140 TO BLOCK_PALI                                        55
         GO TO  5130                                                      55
5140     CONTINUE                                                         55
C    *                                                                    55
C    Geant POSITION : PALI ===> MOTHER='PIPE' KONLY='ONLY' <===           56
         CALL AGSRESET                                                    56
         AG_MOTHER = 'PIPE'                                               56
         AG_KONLY = 'ONLY'                                                56
         AG_TITLE = 'PALI'                                                56
         AG_EXNAME = 'POSITION'                                           56
         AG_PARLIST = 'MOTHER_KONLY'                                      56
         CALL AXPOSITION                                                  56
C    *                                                                    56
C    Create PALJ :                                                        57
C    Call To Local Procedure PALJ                                         57
         ASSIGN 5160 TO BLOCK_PALJ                                        57
         GO TO  5150                                                      57
5160     CONTINUE                                                         57
C    *                                                                    57
C    Geant POSITION : PALJ ===> MOTHER='PIPE' KONLY='ONLY' <===           58
         CALL AGSRESET                                                    58
         AG_MOTHER = 'PIPE'                                               58
         AG_KONLY = 'ONLY'                                                58
         AG_TITLE = 'PALJ'                                                58
         AG_EXNAME = 'POSITION'                                           58
         AG_PARLIST = 'MOTHER_KONLY'                                      58
         CALL AXPOSITION                                                  58
C    *                                                                    58
C    Create PALK :                                                        59
C    Call To Local Procedure PALK                                         59
         ASSIGN 5180 TO BLOCK_PALK                                        59
         GO TO  5170                                                      59
5180     CONTINUE                                                         59
C    *                                                                    59
C    Geant POSITION : PALK ===> MOTHER='PIPE' KONLY='ONLY' <===           60
         CALL AGSRESET                                                    60
         AG_MOTHER = 'PIPE'                                               60
         AG_KONLY = 'ONLY'                                                60
         AG_TITLE = 'PALK'                                                60
         AG_EXNAME = 'POSITION'                                           60
         AG_PARLIST = 'MOTHER_KONLY'                                      60
         CALL AXPOSITION                                                  60
C    *                                                                    60
5011  CALL AGSPOPB                                                        61
      GO TO BLOCK_PIPE, (5010 ,5020)                                      61
C End of Local Procedure                                                  61
C ----------------------------------------------------------------------  62
C ------------ PALH                                                       62
C Begin Local Procedure PALH                                              63
C PALH THE HOLE INSIDE THE BEAM PIPE                                      63
5030  CONTINUE                                                            63
         AG_TITLE='PALH'                                                  63
         AG_MODULE=MODULE                                                 63
         AG_MARK='V00'                                                    63
         CALL AGSPUSH                                                     63
         AG_MODULE=MODULE                                                 63
C    *                                                                    63
C    Geant MATERIAL : AIR ===> inherited arguments used <===              64
         CALL AGSRESET                                                    64
         AG_TITLE = 'AIR'                                                 64
         AG_EXNAME = 'MATERIAL'                                           64
         AG_PARLIST = 'NONE'                                              64
         CALL AXMATERIAL                                                  64
C    *                                                                    64
C    Geant MATERIAL : VACUUM ===> DENS=%DENS*PIPG_VACDENS ABSL=%ABSL/PIP  64
C    G_VACDENS RADL=%RADL/PIPG_VACDENS <===                               64
         CALL AGSRESET                                                    65
         AG_DENS = AG_DENS*PIPG_VACDENS                                   65
         AG_ABSL = AG_ABSL/PIPG_VACDENS                                   65
         AG_RADL = AG_RADL/PIPG_VACDENS                                   65
         AG_TITLE = 'VACUUM'                                              65
         AG_EXNAME = 'MATERIAL'                                           65
         AG_PARLIST = 'DENS_ABSL_RADL'                                    65
         CALL AXMATERIAL                                                  65
C    *                                                                    65
C    Geant ATTRIBUTE : PALH ===> SEEN=0 <===                              65
         CALL AGSRESET                                                    66
         AG_SEEN = 0                                                      66
         AG_TITLE = 'PALH'                                                66
         AG_EXNAME = 'ATTRIBUTE'                                          66
         AG_PARLIST = 'SEEN'                                              66
         CALL AXATTRIBUTE                                                 66
C    *                                                                    66
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-174.75*INCH, -79.  66
C    71*INCH,-64.71*INCH,64.71*INCH,79.71*INCH, 159.000*INCH} RMN={0,0,0  66
C    ,0,0,0} RMX={1.5*INCH, 1.5*INCH,0.7875*INCH,0.7875*INCH,1.5*INCH, 1  66
C    .5*INCH} <===                                                        66
         CALL AGSRESET                                                    71
         AG_PHI1 = 0                                                      71
         AG_DPHI = 360                                                    71
         AG_NZ = 6                                                        71
         AG_ZI(1)=-174.75*INCH                                            71
         AG_ZI(2)=-79.71*INCH                                             71
         AG_ZI(3)=-64.71*INCH                                             71
         AG_ZI(4)=64.71*INCH                                              71
         AG_ZI(5)=79.71*INCH                                              71
         AG_ZI(6)=159.000*INCH                                            71
         AG_RMN(1)=0                                                      71
         AG_RMN(2)=0                                                      71
         AG_RMN(3)=0                                                      71
         AG_RMN(4)=0                                                      71
         AG_RMN(5)=0                                                      71
         AG_RMN(6)=0                                                      71
         AG_RMX(1)=1.5*INCH                                               71
         AG_RMX(2)=1.5*INCH                                               71
         AG_RMX(3)=0.7875*INCH                                            71
         AG_RMX(4)=0.7875*INCH                                            71
         AG_RMX(5)=1.5*INCH                                               71
         AG_RMX(6)=1.5*INCH                                               71
         AG_TITLE = 'PCON'                                                71
         AG_EXNAME = 'SHAPE'                                              71
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           71
         CALL AXSHAPE                                                     71
C    *                                                                    71
5031  CALL AGSPOPB                                                        72
      GO TO BLOCK_PALH, (5030 ,5040)                                      72
C End of Local Procedure                                                  72
C ----------------------------------------------------------------------  73
C ------------ PALQ                                                       73
C Begin Local Procedure PALQ                                              74
C PALQ THE EAST END ALUMINIUM TUBE OF THE BEAM PIPE                       74
5050  CONTINUE                                                            74
         AG_TITLE='PALQ'                                                  74
         AG_MODULE=MODULE                                                 74
         AG_MARK='V00'                                                    74
         CALL AGSPUSH                                                     74
         AG_MODULE=MODULE                                                 74
C    *                                                                    74
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        75
         CALL AGSRESET                                                    75
         AG_TITLE = 'ALUMINIUM'                                           75
         AG_EXNAME = 'MATERIAL'                                           75
         AG_PARLIST = 'NONE'                                              75
         CALL AXMATERIAL                                                  75
C    *                                                                    75
C    Geant ATTRIBUTE : PALQ ===> SEEN=1 COLO=4 <===                       75
         CALL AGSRESET                                                    76
         AG_SEEN = 1                                                      76
         AG_COLO = 4                                                      76
         AG_TITLE = 'PALQ'                                                76
         AG_EXNAME = 'ATTRIBUTE'                                          76
         AG_PARLIST = 'SEEN_COLO'                                         76
         CALL AXATTRIBUTE                                                 76
C    *                                                                    76
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-174.75*INCH, -173  76
C    .99*INCH,-173.99*INCH,-159.76*INCH, -159.76*INCH,-159.00*INCH} RMN=  76
C    {1.5*INCH,1.5*INCH, 1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH}, RMX={1.5*  76
C    INCH+0.8125*INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.049*INCH,1.5*INCH  76
C    +0.049*INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.8125*INCH} <===         76
         CALL AGSRESET                                                    83
         AG_PHI1 = 0                                                      83
         AG_DPHI = 360                                                    83
         AG_NZ = 6                                                        83
         AG_ZI(1)=-174.75*INCH                                            83
         AG_ZI(2)=-173.99*INCH                                            83
         AG_ZI(3)=-173.99*INCH                                            83
         AG_ZI(4)=-159.76*INCH                                            83
         AG_ZI(5)=-159.76*INCH                                            83
         AG_ZI(6)=-159.00*INCH                                            83
         AG_RMN(1)=1.5*INCH                                               83
         AG_RMN(2)=1.5*INCH                                               83
         AG_RMN(3)=1.5*INCH                                               83
         AG_RMN(4)=1.5*INCH                                               83
         AG_RMN(5)=1.5*INCH                                               83
         AG_RMN(6)=1.5*INCH                                               83
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   83
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   83
         AG_RMX(3)=1.5*INCH+0.049*INCH                                    83
         AG_RMX(4)=1.5*INCH+0.049*INCH                                    83
         AG_RMX(5)=1.5*INCH+0.8125*INCH                                   83
         AG_RMX(6)=1.5*INCH+0.8125*INCH                                   83
         AG_TITLE = 'PCON'                                                83
         AG_EXNAME = 'SHAPE'                                              83
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           83
         CALL AXSHAPE                                                     83
C    *                                                                    83
5051  CALL AGSPOPB                                                        84
      GO TO BLOCK_PALQ, (5050 ,5060)                                      84
C End of Local Procedure                                                  84
C ----------------------------------------------------------------------  85
C ------------ PALR                                                       85
C Begin Local Procedure PALR                                              86
C PALR THE EAST ALUMINIUM TRANSITION TUBE OF THE FULL BEAM PIPE           86
5070  CONTINUE                                                            86
         AG_TITLE='PALR'                                                  86
         AG_MODULE=MODULE                                                 86
         AG_MARK='V00'                                                    86
         CALL AGSPUSH                                                     86
         AG_MODULE=MODULE                                                 86
C    *                                                                    86
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        87
         CALL AGSRESET                                                    87
         AG_TITLE = 'ALUMINIUM'                                           87
         AG_EXNAME = 'MATERIAL'                                           87
         AG_PARLIST = 'NONE'                                              87
         CALL AXMATERIAL                                                  87
C    *                                                                    87
C    Geant ATTRIBUTE : PALR ===> SEEN=1 COLO=4 <===                       87
         CALL AGSRESET                                                    88
         AG_SEEN = 1                                                      88
         AG_COLO = 4                                                      88
         AG_TITLE = 'PALR'                                                88
         AG_EXNAME = 'ATTRIBUTE'                                          88
         AG_PARLIST = 'SEEN_COLO'                                         88
         CALL AXATTRIBUTE                                                 88
C    *                                                                    88
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=8 ZI={-159.000*INCH, -15  88
C    8.241*INCH,-158.241*INCH,-79.71*INCH, -64.71*INCH,-56.21*INCH,-56.2  88
C    1*INCH,-55.71*INCH}, RMN={1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH, 0.78  88
C    75*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH}, RMX={1.5*INCH+0.8125*  88
C    INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.049*INCH,1.5*INCH+0.049*INCH,  88
C     0.7875*INCH+0.049*INCH,0.7875*INCH+0.049*INCH, 0.7875*INCH+0.5775*  88
C    INCH,0.7875*INCH+0.5775*INCH} <===                                   88
         CALL AGSRESET                                                    97
         AG_PHI1 = 0                                                      97
         AG_DPHI = 360                                                    97
         AG_NZ = 8                                                        97
         AG_ZI(1)=-159.000*INCH                                           97
         AG_ZI(2)=-158.241*INCH                                           97
         AG_ZI(3)=-158.241*INCH                                           97
         AG_ZI(4)=-79.71*INCH                                             97
         AG_ZI(5)=-64.71*INCH                                             97
         AG_ZI(6)=-56.21*INCH                                             97
         AG_ZI(7)=-56.21*INCH                                             97
         AG_ZI(8)=-55.71*INCH                                             97
         AG_RMN(1)=1.5*INCH                                               97
         AG_RMN(2)=1.5*INCH                                               97
         AG_RMN(3)=1.5*INCH                                               97
         AG_RMN(4)=1.5*INCH                                               97
         AG_RMN(5)=0.7875*INCH                                            97
         AG_RMN(6)=0.7875*INCH                                            97
         AG_RMN(7)=0.7875*INCH                                            97
         AG_RMN(8)=0.7875*INCH                                            97
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   97
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   97
         AG_RMX(3)=1.5*INCH+0.049*INCH                                    97
         AG_RMX(4)=1.5*INCH+0.049*INCH                                    97
         AG_RMX(5)=0.7875*INCH+0.049*INCH                                 97
         AG_RMX(6)=0.7875*INCH+0.049*INCH                                 97
         AG_RMX(7)=0.7875*INCH+0.5775*INCH                                97
         AG_RMX(8)=0.7875*INCH+0.5775*INCH                                97
         AG_TITLE = 'PCON'                                                97
         AG_EXNAME = 'SHAPE'                                              97
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           97
         CALL AXSHAPE                                                     97
C    *                                                                    97
5071  CALL AGSPOPB                                                        98
      GO TO BLOCK_PALR, (5070 ,5080)                                      98
C End of Local Procedure                                                  98
C ----------------------------------------------------------------------  99
C ------------ PALS                                                       99
C Begin Local Procedure PALS                                              100
C PALS THE EAST ALUMINIUM SECTION OF THE BEAM PIPE                        100
5090  CONTINUE                                                            100
         AG_TITLE='PALS'                                                  100
         AG_MODULE=MODULE                                                 100
         AG_MARK='V00'                                                    100
         CALL AGSPUSH                                                     100
         AG_MODULE=MODULE                                                 100
C    *                                                                    100
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        101
         CALL AGSRESET                                                    101
         AG_TITLE = 'ALUMINIUM'                                           101
         AG_EXNAME = 'MATERIAL'                                           101
         AG_PARLIST = 'NONE'                                              101
         CALL AXMATERIAL                                                  101
C    *                                                                    101
C    Geant ATTRIBUTE : PALS ===> SEEN=1 COLO=4 <===                       101
         CALL AGSRESET                                                    102
         AG_SEEN = 1                                                      102
         AG_COLO = 4                                                      102
         AG_TITLE = 'PALS'                                                102
         AG_EXNAME = 'ATTRIBUTE'                                          102
         AG_PARLIST = 'SEEN_COLO'                                         102
         CALL AXATTRIBUTE                                                 102
C    *                                                                    102
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-55.71*INCH, -55.2  102
C    1*INCH,-55.21*INCH,-43.71*INCH,-43.71*INCH, -15.75*INCH} RMN={0.787  102
C    5*INCH,0.7875*INCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH  102
C    }, RMX={0.7875*INCH+0.5875*INCH,0.7875*INCH+0.5875*INCH, 0.7875*INC  102
C    H+0.065*INCH,0.7875*INCH+0.065*INCH, 0.7875*INCH+0.055*INCH,0.7875*  102
C    INCH+0.055*INCH} <===                                                102
         CALL AGSRESET                                                    109
         AG_PHI1 = 0                                                      109
         AG_DPHI = 360                                                    109
         AG_NZ = 6                                                        109
         AG_ZI(1)=-55.71*INCH                                             109
         AG_ZI(2)=-55.21*INCH                                             109
         AG_ZI(3)=-55.21*INCH                                             109
         AG_ZI(4)=-43.71*INCH                                             109
         AG_ZI(5)=-43.71*INCH                                             109
         AG_ZI(6)=-15.75*INCH                                             109
         AG_RMN(1)=0.7875*INCH                                            109
         AG_RMN(2)=0.7875*INCH                                            109
         AG_RMN(3)=0.7875*INCH                                            109
         AG_RMN(4)=0.7875*INCH                                            109
         AG_RMN(5)=0.7875*INCH                                            109
         AG_RMN(6)=0.7875*INCH                                            109
         AG_RMX(1)=0.7875*INCH+0.5875*INCH                                109
         AG_RMX(2)=0.7875*INCH+0.5875*INCH                                109
         AG_RMX(3)=0.7875*INCH+0.065*INCH                                 109
         AG_RMX(4)=0.7875*INCH+0.065*INCH                                 109
         AG_RMX(5)=0.7875*INCH+0.055*INCH                                 109
         AG_RMX(6)=0.7875*INCH+0.055*INCH                                 109
         AG_TITLE = 'PCON'                                                109
         AG_EXNAME = 'SHAPE'                                              109
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           109
         CALL AXSHAPE                                                     109
C    *                                                                    109
5091  CALL AGSPOPB                                                        110
      GO TO BLOCK_PALS, (5090 ,5100)                                      110
C End of Local Procedure                                                  110
C ----------------------------------------------------------------------  111
C ------------ PBES                                                       111
C Begin Local Procedure PBES                                              112
C PBES THE BERILLIUM SECTION OF THE BEAM PIPE                             112
5110  CONTINUE                                                            112
         AG_TITLE='PBES'                                                  112
         AG_MODULE=MODULE                                                 112
         AG_MARK='V00'                                                    112
         CALL AGSPUSH                                                     112
         AG_MODULE=MODULE                                                 112
C    *                                                                    112
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        113
         CALL AGSRESET                                                    113
         AG_TITLE = 'BERILLIUM'                                           113
         AG_EXNAME = 'MATERIAL'                                           113
         AG_PARLIST = 'NONE'                                              113
         CALL AXMATERIAL                                                  113
C    *                                                                    113
C    Geant ATTRIBUTE : PBES ===> SEEN=1 COLO=2 <===                       113
         CALL AGSRESET                                                    114
         AG_SEEN = 1                                                      114
         AG_COLO = 2                                                      114
         AG_TITLE = 'PBES'                                                114
         AG_EXNAME = 'ATTRIBUTE'                                          114
         AG_PARLIST = 'SEEN_COLO'                                         114
         CALL AXATTRIBUTE                                                 114
C    *                                                                    114
C    Geant SHAPE : TUBE ===> RMIN=0.7875*INCH RMAX=0.7875*INCH+0.030*INC  114
C    H, DZ=47.25*INCH/2 <===                                              114
         CALL AGSRESET                                                    116
         AG_RMIN = 0.7875*INCH                                            116
         AG_RMAX = 0.7875*INCH+0.030*INCH                                 116
         AG_DZ = 47.25*INCH/2                                             116
         AG_TITLE = 'TUBE'                                                116
         AG_EXNAME = 'SHAPE'                                              116
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      116
         CALL AXSHAPE                                                     116
C    *                                                                    116
5111  CALL AGSPOPB                                                        117
      GO TO BLOCK_PBES, (5110 ,5120)                                      117
C End of Local Procedure                                                  117
C ----------------------------------------------------------------------  118
C ------------ PALI                                                       118
C Begin Local Procedure PALI                                              119
C PALI THE WEST ALUMINIUM SECTION OF THE BEAM PIPE                        119
5130  CONTINUE                                                            119
         AG_TITLE='PALI'                                                  119
         AG_MODULE=MODULE                                                 119
         AG_MARK='V00'                                                    119
         CALL AGSPUSH                                                     119
         AG_MODULE=MODULE                                                 119
C    *                                                                    119
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        120
         CALL AGSRESET                                                    120
         AG_TITLE = 'ALUMINIUM'                                           120
         AG_EXNAME = 'MATERIAL'                                           120
         AG_PARLIST = 'NONE'                                              120
         CALL AXMATERIAL                                                  120
C    *                                                                    120
C    Geant ATTRIBUTE : PALI ===> SEEN=1 COLO=4 <===                       120
         CALL AGSRESET                                                    121
         AG_SEEN = 1                                                      121
         AG_COLO = 4                                                      121
         AG_TITLE = 'PALI'                                                121
         AG_EXNAME = 'ATTRIBUTE'                                          121
         AG_PARLIST = 'SEEN_COLO'                                         121
         CALL AXATTRIBUTE                                                 121
C    *                                                                    121
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={31.5*INCH, 43.72*I  121
C    NCH,43.72*INCH,55.21*INCH,55.21*INCH, 55.71*INCH} RMN={0.7875*INCH,  121
C    0.7875*INCH, 0.7875*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH}, RMX=  121
C    {0.7875*INCH+0.055*INCH,0.7875*INCH+0.055*INCH, 0.7875*INCH+0.065*I  121
C    NCH,0.7875*INCH+0.065*INCH, 0.7875*INCH+0.5875*INCH,0.7875*INCH+0.5  121
C    875*INCH} <===                                                       121
         CALL AGSRESET                                                    128
         AG_PHI1 = 0                                                      128
         AG_DPHI = 360                                                    128
         AG_NZ = 6                                                        128
         AG_ZI(1)=31.5*INCH                                               128
         AG_ZI(2)=43.72*INCH                                              128
         AG_ZI(3)=43.72*INCH                                              128
         AG_ZI(4)=55.21*INCH                                              128
         AG_ZI(5)=55.21*INCH                                              128
         AG_ZI(6)=55.71*INCH                                              128
         AG_RMN(1)=0.7875*INCH                                            128
         AG_RMN(2)=0.7875*INCH                                            128
         AG_RMN(3)=0.7875*INCH                                            128
         AG_RMN(4)=0.7875*INCH                                            128
         AG_RMN(5)=0.7875*INCH                                            128
         AG_RMN(6)=0.7875*INCH                                            128
         AG_RMX(1)=0.7875*INCH+0.055*INCH                                 128
         AG_RMX(2)=0.7875*INCH+0.055*INCH                                 128
         AG_RMX(3)=0.7875*INCH+0.065*INCH                                 128
         AG_RMX(4)=0.7875*INCH+0.065*INCH                                 128
         AG_RMX(5)=0.7875*INCH+0.5875*INCH                                128
         AG_RMX(6)=0.7875*INCH+0.5875*INCH                                128
         AG_TITLE = 'PCON'                                                128
         AG_EXNAME = 'SHAPE'                                              128
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           128
         CALL AXSHAPE                                                     128
C    *                                                                    128
5131  CALL AGSPOPB                                                        129
      GO TO BLOCK_PALI, (5130 ,5140)                                      129
C End of Local Procedure                                                  129
C ----------------------------------------------------------------------  130
C ------------ PALJ                                                       130
C Begin Local Procedure PALJ                                              131
C PALJ THE WEST ALUMINIUM SECTION OF THE BEAM PIPE (WEST TRANSITION TUBE  131
C )                                                                       131
5150  CONTINUE                                                            131
         AG_TITLE='PALJ'                                                  131
         AG_MODULE=MODULE                                                 131
         AG_MARK='V00'                                                    131
         CALL AGSPUSH                                                     131
         AG_MODULE=MODULE                                                 131
C    *                                                                    131
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        132
         CALL AGSRESET                                                    132
         AG_TITLE = 'ALUMINIUM'                                           132
         AG_EXNAME = 'MATERIAL'                                           132
         AG_PARLIST = 'NONE'                                              132
         CALL AXMATERIAL                                                  132
C    *                                                                    132
C    Geant ATTRIBUTE : PALJ ===> SEEN=1 COLO=4 <===                       132
         CALL AGSRESET                                                    133
         AG_SEEN = 1                                                      133
         AG_COLO = 4                                                      133
         AG_TITLE = 'PALJ'                                                133
         AG_EXNAME = 'ATTRIBUTE'                                          133
         AG_PARLIST = 'SEEN_COLO'                                         133
         CALL AXATTRIBUTE                                                 133
C    *                                                                    133
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=8 ZI={55.71*INCH, 56.21*  133
C    INCH,56.21*INCH,64.71*INCH,79.71*INCH, 109.477*INCH,109.477*INCH,11  133
C    0.237*INCH}, RMN={0.7875*INCH,0.7875*INCH,0.7875*INCH,0.7875*INCH,   133
C    1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH}, RMX={0.7875*INCH+0.5775*INCH,  133
C    0.7875*INCH+0.5775*INCH, 0.7875*INCH+0.049*INCH,0.7875*INCH+0.049*I  133
C    NCH, 1.5*INCH+0.049*INCH,1.5*INCH+0.049*INCH, 1.5*INCH+0.8125*INCH,  133
C    1.5*INCH+0.8125*INCH} <===                                           133
         CALL AGSRESET                                                    142
         AG_PHI1 = 0                                                      142
         AG_DPHI = 360                                                    142
         AG_NZ = 8                                                        142
         AG_ZI(1)=55.71*INCH                                              142
         AG_ZI(2)=56.21*INCH                                              142
         AG_ZI(3)=56.21*INCH                                              142
         AG_ZI(4)=64.71*INCH                                              142
         AG_ZI(5)=79.71*INCH                                              142
         AG_ZI(6)=109.477*INCH                                            142
         AG_ZI(7)=109.477*INCH                                            142
         AG_ZI(8)=110.237*INCH                                            142
         AG_RMN(1)=0.7875*INCH                                            142
         AG_RMN(2)=0.7875*INCH                                            142
         AG_RMN(3)=0.7875*INCH                                            142
         AG_RMN(4)=0.7875*INCH                                            142
         AG_RMN(5)=1.5*INCH                                               142
         AG_RMN(6)=1.5*INCH                                               142
         AG_RMN(7)=1.5*INCH                                               142
         AG_RMN(8)=1.5*INCH                                               142
         AG_RMX(1)=0.7875*INCH+0.5775*INCH                                142
         AG_RMX(2)=0.7875*INCH+0.5775*INCH                                142
         AG_RMX(3)=0.7875*INCH+0.049*INCH                                 142
         AG_RMX(4)=0.7875*INCH+0.049*INCH                                 142
         AG_RMX(5)=1.5*INCH+0.049*INCH                                    142
         AG_RMX(6)=1.5*INCH+0.049*INCH                                    142
         AG_RMX(7)=1.5*INCH+0.8125*INCH                                   142
         AG_RMX(8)=1.5*INCH+0.8125*INCH                                   142
         AG_TITLE = 'PCON'                                                142
         AG_EXNAME = 'SHAPE'                                              142
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           142
         CALL AXSHAPE                                                     142
C    *                                                                    142
5151  CALL AGSPOPB                                                        143
      GO TO BLOCK_PALJ, (5150 ,5160)                                      143
C End of Local Procedure                                                  143
C ----------------------------------------------------------------------  144
C ------------ PALK                                                       144
C Begin Local Procedure PALK                                              145
C PALK THE WEST END ALUMINIUM TUBE OF THE BEAM PIPE                       145
5170  CONTINUE                                                            145
         AG_TITLE='PALK'                                                  145
         AG_MODULE=MODULE                                                 145
         AG_MARK='V00'                                                    145
         CALL AGSPUSH                                                     145
         AG_MODULE=MODULE                                                 145
C    *                                                                    145
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        146
         CALL AGSRESET                                                    146
         AG_TITLE = 'ALUMINIUM'                                           146
         AG_EXNAME = 'MATERIAL'                                           146
         AG_PARLIST = 'NONE'                                              146
         CALL AXMATERIAL                                                  146
C    *                                                                    146
C    Geant ATTRIBUTE : PALK ===> SEEN=1 COLO=4 <===                       146
         CALL AGSRESET                                                    147
         AG_SEEN = 1                                                      147
         AG_COLO = 4                                                      147
         AG_TITLE = 'PALK'                                                147
         AG_EXNAME = 'ATTRIBUTE'                                          147
         AG_PARLIST = 'SEEN_COLO'                                         147
         CALL AXATTRIBUTE                                                 147
C    *                                                                    147
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={110.237*INCH, 110.  147
C    997*INCH,110.997*INCH,158.241*INCH, 158.241*INCH,159.000*INCH} RMN=  147
C    {1.5*INCH,1.5*INCH, 1.5*INCH,1.5*INCH,1.5*INCH,1.5*INCH}, RMX={1.5*  147
C    INCH+0.8125*INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.049*INCH,1.5*INCH  147
C    +0.049*INCH,1.5*INCH+0.8125*INCH, 1.5*INCH+0.8125*INCH} <===         147
         CALL AGSRESET                                                    154
         AG_PHI1 = 0                                                      154
         AG_DPHI = 360                                                    154
         AG_NZ = 6                                                        154
         AG_ZI(1)=110.237*INCH                                            154
         AG_ZI(2)=110.997*INCH                                            154
         AG_ZI(3)=110.997*INCH                                            154
         AG_ZI(4)=158.241*INCH                                            154
         AG_ZI(5)=158.241*INCH                                            154
         AG_ZI(6)=159.000*INCH                                            154
         AG_RMN(1)=1.5*INCH                                               154
         AG_RMN(2)=1.5*INCH                                               154
         AG_RMN(3)=1.5*INCH                                               154
         AG_RMN(4)=1.5*INCH                                               154
         AG_RMN(5)=1.5*INCH                                               154
         AG_RMN(6)=1.5*INCH                                               154
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   154
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   154
         AG_RMX(3)=1.5*INCH+0.049*INCH                                    154
         AG_RMX(4)=1.5*INCH+0.049*INCH                                    154
         AG_RMX(5)=1.5*INCH+0.8125*INCH                                   154
         AG_RMX(6)=1.5*INCH+0.8125*INCH                                   154
         AG_TITLE = 'PCON'                                                154
         AG_EXNAME = 'SHAPE'                                              154
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           154
         CALL AXSHAPE                                                     154
C    *                                                                    154
5171  CALL AGSPOPB                                                        155
      GO TO BLOCK_PALK, (5170 ,5180)                                      155
C End of Local Procedure                                                  155
      WRITE(LOUT,5190)'===>>  [End AgML Module PipeGeo2]  <<==='          155
5190  FORMAT(A32,/,/)                                                     155
C MODULE PIPEGEO2                                                         157
      END                                                                 157
