C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE CAVEGEO                                                  1
C CAVEGEO BUILDS CAVE FOR GSTAR                                           1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'CAVEGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'BUILDS CAVE FOR GSTAR'/                              1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR PETER JACOBS, LBL                                                1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'PETER JACOBS, LBL'/                           2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED MARCH 10, 1995                                                  2
      CHARACTER*40 CREATED /'MARCH 10, 1995'/                             3
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
      INTEGER BLOCK_CAVE                                                  4
      INTEGER BLOCK_HALL                                                  5
C -                                                                       6
      REAL CVCF_VERSION                                                   6
      EQUIVALENCE (BANK_CVCF(3),CVCF_VERSION,RE_CVCF_VERSION),            6
     *(NAMES_CVCF(1),NCVCF_VERSION)                                       6
      EQUIVALENCE (COMMEN_CVCF(1),CCVCF_VERSION),(MAP_CVCF(1,1),          6
     *MCVCF_VERSION)                                                      6
      CHARACTER CCVCF_VERSION*40/' '/                                     6
      CHARACTER NCVCF_VERSION*12/'R.VERSION'/                             6
      INTEGER MCVCF_VERSION(2)/1,1/                                       6
      REAL RE_CVCF_VERSION                                                6
C -                                                                       6
      INTEGER CVCF_CONFIG                                                 6
      EQUIVALENCE (BANK_CVCF(4),CVCF_CONFIG,RE_CVCF_CONFIG),              6
     *(NAMES_CVCF(2),NCVCF_CONFIG)                                        6
      EQUIVALENCE (COMMEN_CVCF(2),CCVCF_CONFIG),(MAP_CVCF(1,2),           6
     *MCVCF_CONFIG)                                                       6
      CHARACTER CCVCF_CONFIG*40/' '/                                      6
      CHARACTER NCVCF_CONFIG*12/'I.CONFIG'/                               6
      INTEGER MCVCF_CONFIG(2)/1,1/                                        6
      REAL RE_CVCF_CONFIG                                                 6
C -                                                                       6
      INTEGER CVCF_TPCREFSYS                                              6
      EQUIVALENCE (BANK_CVCF(5),CVCF_TPCREFSYS,RE_CVCF_TPCREFSYS),        6
     *(NAMES_CVCF(3),NCVCF_TPCREFSYS)                                     6
      EQUIVALENCE (COMMEN_CVCF(3),CCVCF_TPCREFSYS),(MAP_CVCF(1,3),        6
     *MCVCF_TPCREFSYS)                                                    6
      CHARACTER CCVCF_TPCREFSYS*40/' '/                                   6
      CHARACTER NCVCF_TPCREFSYS*12/'I.TPCREFSYS'/                         6
      INTEGER MCVCF_TPCREFSYS(2)/1,1/                                     6
      REAL RE_CVCF_TPCREFSYS                                              6
C -                                                                       6
      INTEGER LEN_CVCF,LLEN_CVCF,LINK_CVCF                                6
      PARAMETER (LEN_CVCF=5)                                              6
      PARAMETER (LLEN_CVCF=3)                                             6
      INTEGER FLAG_CVCF/0/,MAP_CVCF(2,LLEN_CVCF),IDNUM_CVCF(10)           6
      CHARACTER NAMES_CVCF(LLEN_CVCF)*12,COMMEN_CVCF(LLEN_CVCF)*40        6
      DATA IDNUM_CVCF/10*0/                                               6
      REAL BANK_CVCF(LEN_CVCF)                                            6
      CHARACTER*60 BPATH_CVCF/' '/                                        6
      SAVE FLAG_CVCF,MAP_CVCF,IDNUM_CVCF,NAMES_CVCF,COMMEN_CVCF,          6
     *BPATH_CVCF                                                          6
      DATA LINK_CVCF/0/,BANK_CVCF/LEN_CVCF*0/                             6
C *                                                                       6
C -                                                                       7
      REAL CAVE_VERSION                                                   7
      EQUIVALENCE (BANK_CAVE(3),CAVE_VERSION,RE_CAVE_VERSION),            7
     *(NAMES_CAVE(1),NCAVE_VERSION)                                       7
      EQUIVALENCE (COMMEN_CAVE(1),CCAVE_VERSION),(MAP_CAVE(1,1),          7
     *MCAVE_VERSION)                                                      7
      CHARACTER CCAVE_VERSION*40/' '/                                     7
      CHARACTER NCAVE_VERSION*12/'R.VERSION'/                             7
      INTEGER MCAVE_VERSION(2)/1,1/                                       7
      REAL RE_CAVE_VERSION                                                7
C -                                                                       7
      REAL CAVE_RMIN                                                      7
      EQUIVALENCE (BANK_CAVE(4),CAVE_RMIN,RE_CAVE_RMIN),(NAMES_CAVE(2),   7
     *NCAVE_RMIN)                                                         7
      EQUIVALENCE (COMMEN_CAVE(2),CCAVE_RMIN),(MAP_CAVE(1,2),             7
     *MCAVE_RMIN)                                                         7
      CHARACTER CCAVE_RMIN*40/' '/                                        7
      CHARACTER NCAVE_RMIN*12/'R.RMIN'/                                   7
      INTEGER MCAVE_RMIN(2)/1,1/                                          7
      REAL RE_CAVE_RMIN                                                   7
C -                                                                       7
      REAL CAVE_RMAX(2)                                                   7
      EQUIVALENCE (BANK_CAVE(5),CAVE_RMAX,RE_CAVE_RMAX),(NAMES_CAVE(3),   7
     *NCAVE_RMAX)                                                         7
      EQUIVALENCE (COMMEN_CAVE(3),CCAVE_RMAX),(MAP_CAVE(1,3),             7
     *MCAVE_RMAX)                                                         7
      CHARACTER CCAVE_RMAX*40/' '/                                        7
      CHARACTER NCAVE_RMAX*12/'R.RMAX'/                                   7
      INTEGER MCAVE_RMAX(2)/2,1/                                          7
      REAL RE_CAVE_RMAX                                                   7
C -                                                                       7
      REAL CAVE_DZ(2)                                                     7
      EQUIVALENCE (BANK_CAVE(7),CAVE_DZ,RE_CAVE_DZ),(NAMES_CAVE(4),       7
     *NCAVE_DZ)                                                           7
      EQUIVALENCE (COMMEN_CAVE(4),CCAVE_DZ),(MAP_CAVE(1,4),MCAVE_DZ)      7
      CHARACTER CCAVE_DZ*40/' '/                                          7
      CHARACTER NCAVE_DZ*12/'R.DZ'/                                       7
      INTEGER MCAVE_DZ(2)/2,1/                                            7
      REAL RE_CAVE_DZ                                                     7
C -                                                                       7
      REAL CAVE_DCONC                                                     7
      EQUIVALENCE (BANK_CAVE(9),CAVE_DCONC,RE_CAVE_DCONC),                7
     *(NAMES_CAVE(5),NCAVE_DCONC)                                         7
      EQUIVALENCE (COMMEN_CAVE(5),CCAVE_DCONC),(MAP_CAVE(1,5),            7
     *MCAVE_DCONC)                                                        7
      CHARACTER CCAVE_DCONC*40/' '/                                       7
      CHARACTER NCAVE_DCONC*12/'R.DCONC'/                                 7
      INTEGER MCAVE_DCONC(2)/1,1/                                         7
      REAL RE_CAVE_DCONC                                                  7
C -                                                                       7
      INTEGER LEN_CAVE,LLEN_CAVE,LINK_CAVE                                7
      PARAMETER (LEN_CAVE=9)                                              7
      PARAMETER (LLEN_CAVE=5)                                             7
      INTEGER FLAG_CAVE/0/,MAP_CAVE(2,LLEN_CAVE),IDNUM_CAVE(10)           7
      CHARACTER NAMES_CAVE(LLEN_CAVE)*12,COMMEN_CAVE(LLEN_CAVE)*40        7
      DATA IDNUM_CAVE/10*0/                                               7
      REAL BANK_CAVE(LEN_CAVE)                                            7
      CHARACTER*60 BPATH_CAVE/' '/                                        7
      SAVE FLAG_CAVE,MAP_CAVE,IDNUM_CAVE,NAMES_CAVE,COMMEN_CAVE,          7
     *BPATH_CAVE                                                          7
      DATA LINK_CAVE/0/,BANK_CAVE/LEN_CAVE*0/                             7
C *                                                                       7
      REAL D1,D2,Z1                                                       8
      INTEGER *4 AGEXIST                                                  10
C *                                                                       12
      CALL AGSBEGM(MODULE,IPRIN)                                          12
      IF (IPRIN.LT.0) RETURN                                              12
C Filling bank CVCF - CAVE CONFIGURATION                                  12
      BANK_TITLE='CAVE CONFIGURATION'                                     12
      BPATH_CVCF='CVCF*'                                                  12
      IDNUM_CVCF(1)=0                                                     13
C VERSION                                                                 13
      CVCF_VERSION=1                                                      13
      CCVCF_VERSION='VERSION'                                             13
C DEFAULT CONFIG                                                          14
      CVCF_CONFIG=1                                                       14
      CCVCF_CONFIG='DEFAULT CONFIG'                                       14
C CONFIGURES TPCREFSYS                                                    15
      CVCF_TPCREFSYS=0                                                    15
      CCVCF_TPCREFSYS='CONFIGURES TPCREFSYS'                              15
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CVCF',    16
     *BPATH_CVCF,IDNUM_CVCF,1, MAP_CVCF,NAMES_CVCF,COMMEN_CVCF,           16
     *BANK_CVCF,LEN_CVCF,LLEN_CVCF,LINK_CVCF,FLAG_CVCF)                   16
C *                                                                       16
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CVCF',    16
     *BPATH_CVCF,IDNUM_CVCF,1, MAP_CVCF,NAMES_CVCF,COMMEN_CVCF,           16
     *BANK_CVCF,LEN_CVCF,LLEN_CVCF,LINK_CVCF,FLAG_CVCF)                   16
C *                                                                       17
      CALL AGSBEGM(MODULE,IPRIN)                                          17
      IF (IPRIN.LT.0) RETURN                                              17
C Filling bank CAVE - STAR CAVE GEOMETRY                                  17
      BANK_TITLE='STAR CAVE GEOMETRY'                                     17
      BPATH_CAVE='CAVE*'                                                  17
      IDNUM_CAVE(1)=0                                                     18
C GEOMETRY VERSION                                                        18
      CAVE_VERSION=1                                                      18
      CCAVE_VERSION='GEOMETRY VERSION'                                    18
C INNER RADIUS                                                            19
      CAVE_RMIN=0                                                         19
      CCAVE_RMIN='INNER RADIUS'                                           19
C OUTER RADIUS                                                            20
      CAVE_RMAX(1)=400                                                    20
      CAVE_RMAX(2)=100                                                    20
      CCAVE_RMAX='OUTER RADIUS'                                           20
C HALF LENGTH                                                             21
      CAVE_DZ(1)=800                                                      21
      CAVE_DZ(2)=2000                                                     21
      CCAVE_DZ='HALF LENGTH'                                              21
C CONCRETE THICKNESS                                                      22
      CAVE_DCONC=20                                                       22
      CCAVE_DCONC='CONCRETE THICKNESS'                                    22
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    23
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           23
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   23
C *                                                                       23
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    23
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           23
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   23
C *                                                                       24
      CALL AGSBEGM(MODULE,IPRIN)                                          24
      IF (IPRIN.LT.0) RETURN                                              24
C Filling bank CAVE - STAR CAVE GEOMETRY                                  24
      BANK_TITLE='STAR CAVE GEOMETRY'                                     24
      BPATH_CAVE='CAVE*'                                                  24
      IDNUM_CAVE(1)=0                                                     25
C GEOMETRY VERSION                                                        25
      CAVE_VERSION=2                                                      25
      CCAVE_VERSION='GEOMETRY VERSION'                                    25
C INNER RADIUS                                                            26
      CAVE_RMIN=0                                                         26
      CCAVE_RMIN='INNER RADIUS'                                           26
C OUTER RADIUS                                                            27
      CAVE_RMAX(1)=400                                                    27
      CAVE_RMAX(2)=213                                                    27
      CCAVE_RMAX='OUTER RADIUS'                                           27
C HALF LENGTH                                                             28
      CAVE_DZ(1)=800                                                      28
      CAVE_DZ(2)=5000                                                     28
      CCAVE_DZ='HALF LENGTH'                                              28
C CONCRETE THICKNESS                                                      29
      CAVE_DCONC=50                                                       29
      CCAVE_DCONC='CONCRETE THICKNESS'                                    29
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    30
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           30
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   30
C *                                                                       30
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    30
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           30
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   30
C *                                                                       31
      CALL AGSBEGM(MODULE,IPRIN)                                          31
      IF (IPRIN.LT.0) RETURN                                              31
C Filling bank CAVE - STAR CAVE GEOMETRY                                  31
      BANK_TITLE='STAR CAVE GEOMETRY'                                     31
      BPATH_CAVE='CAVE*'                                                  31
      IDNUM_CAVE(1)=0                                                     32
C GEOMETRY VERSION                                                        32
      CAVE_VERSION=3                                                      32
      CCAVE_VERSION='GEOMETRY VERSION'                                    32
C INNER RADIUS                                                            33
      CAVE_RMIN=0                                                         33
      CCAVE_RMIN='INNER RADIUS'                                           33
C OUTER RADIUS                                                            34
      CAVE_RMAX(1)=450                                                    34
      CAVE_RMAX(2)=100                                                    34
      CCAVE_RMAX='OUTER RADIUS'                                           34
C HALF LENGTH                                                             35
      CAVE_DZ(1)=800                                                      35
      CAVE_DZ(2)=2000                                                     35
      CCAVE_DZ='HALF LENGTH'                                              35
C CONCRETE THICKNESS                                                      36
      CAVE_DCONC=50                                                       36
      CCAVE_DCONC='CONCRETE THICKNESS'                                    36
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    37
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           37
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   37
C *                                                                       37
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    37
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           37
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   37
C *                                                                       38
      CALL AGSBEGM(MODULE,IPRIN)                                          38
      IF (IPRIN.LT.0) RETURN                                              38
C Filling bank CAVE - STAR CAVE GEOMETRY                                  38
      BANK_TITLE='STAR CAVE GEOMETRY'                                     38
      BPATH_CAVE='CAVE*'                                                  38
      IDNUM_CAVE(1)=0                                                     39
C GEOMETRY VERSION                                                        39
      CAVE_VERSION=4                                                      39
      CCAVE_VERSION='GEOMETRY VERSION'                                    39
C INNER RADIUS                                                            40
      CAVE_RMIN=0                                                         40
      CCAVE_RMIN='INNER RADIUS'                                           40
C OUTER RADIUS                                                            41
      CAVE_RMAX(1)=450                                                    41
      CAVE_RMAX(2)=100                                                    41
      CCAVE_RMAX='OUTER RADIUS'                                           41
C HALF LENGTH                                                             42
      CAVE_DZ(1)=950                                                      42
      CAVE_DZ(2)=2000                                                     42
      CCAVE_DZ='HALF LENGTH'                                              42
C CONCRETE THICKNESS                                                      43
      CAVE_DCONC=50                                                       43
      CCAVE_DCONC='CONCRETE THICKNESS'                                    43
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    44
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           44
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   44
C *                                                                       44
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    44
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           44
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   44
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank CAVE - STAR CAVE GEOMETRY                                  45
      BANK_TITLE='STAR CAVE GEOMETRY'                                     45
      BPATH_CAVE='CAVE*'                                                  45
      IDNUM_CAVE(1)=0                                                     46
C VERSION                                                                 46
      CAVE_VERSION=5                                                      46
      CCAVE_VERSION='VERSION'                                             46
C INNER RADIUS OF CAVE HAD BETTER BE ZERO                                 47
      CAVE_RMIN=0                                                         47
      CCAVE_RMIN='INNER RADIUS OF CAVE HAD BETTER BE ZERO'                47
C FURTHEST DISTANCE TO A CONCRETE WALL                                    48
      CAVE_RMAX(1)=1575.02                                                48
      CAVE_RMAX(2)=329.95                                                 48
      CCAVE_RMAX='FURTHEST DISTANCE TO A CONCRETE WALL'                   48
C DZ(1) IS DISTANCE TO EAST/WEST WALL FROM CENTER OF STAR, DZ(2) DEPTH I  49
C NTO TUNNELS                                                             49
      CAVE_DZ(1)=807.72                                                   49
      CAVE_DZ(2)=4000.0                                                   49
      CCAVE_DZ='DZ(1) IS DISTANCE TO EAST/WEST WALL FROM CENTER OF STAR,  49
     * DZ(2) DEPTH INTO TUNNELS'                                          49
C CONCRETE THICKNESS                                                      50
      CAVE_DCONC=50.0                                                     50
      CCAVE_DCONC='CONCRETE THICKNESS'                                    50
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    51
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           51
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   51
C *                                                                       51
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    51
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           51
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   51
C use default CVCF bank                                                   52
      BPATH_CVCF='CVCF*'                                                  52
      IDNUM_CVCF(1)=0                                                     52
      CALL AGDGETP('CAVEGEO','*',-999,'CVCF',' ',BANK_CVCF, BPATH_CVCF,   52
     *IDNUM_CVCF,1,MAP_CVCF,NAMES_CVCF,BANK_CVCF,LEN_CVCF,LLEN_CVCF,      52
     *LINK_CVCF,FLAG_CVCF)                                                52
C use CAVE bank                                                           53
      BPATH_CAVE='CAVE*'                                                  53
      IDNUM_CAVE(1)=0                                                     53
      CAVE_VERSION=CVCF_CONFIG                                            53
      CALL AGDGETP('CAVEGEO','*',-999,'CAVE',NCAVE_VERSION,               53
     *RE_CAVE_VERSION,BPATH_CAVE,IDNUM_CAVE,1,MAP_CAVE,NAMES_CAVE,        53
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   53
      WRITE(LOUT,5010)CVCF_CONFIG                                         53
5010  FORMAT('CaveGeo Module is called CONFIG=', I3)                      53
      CALL AGSBEGM(MODULE,IPRIN)                                          55
C Check IPRIN<0                                                           55
      IF (IPRIN.LT.0) RETURN                                              55
C Create HALL :                                                           55
C Call To Local Procedure HALL                                            55
      ASSIGN 5030 TO BLOCK_HALL                                           55
      GO TO  5020                                                         55
5030  CONTINUE                                                            55
C *                                                                       55
C ----------------------------------------------------------------------  56
C ------------ HALL                                                       56
      CALL AGSENDM                                                        57
      RETURN                                                              57
C Begin Local Procedure HALL                                              57
C HALL IS GSTAR BUILDING                                                  57
5020  CONTINUE                                                            57
         AG_TITLE='HALL'                                                  57
         AG_MODULE=MODULE                                                 57
         AG_MARK='V00'                                                    57
         CALL AGSPUSH                                                     57
         AG_MODULE=MODULE                                                 57
C    *                                                                    57
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=1 <===                      58
         CALL AGSRESET                                                    58
         AG_A = 28.08                                                     58
         AG_Z = 14                                                        58
         AG_W = 1                                                         58
         AG_TITLE = 'SI'                                                  58
         AG_EXNAME = 'COMPONENT'                                          58
         AG_PARLIST = 'A_Z_W'                                             58
         CALL AXCOMPONENT                                                 58
C    *                                                                    58
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          58
         CALL AGSRESET                                                    59
         AG_A = 16                                                        59
         AG_Z = 8                                                         59
         AG_W = 2                                                         59
         AG_TITLE = 'O2'                                                  59
         AG_EXNAME = 'COMPONENT'                                          59
         AG_PARLIST = 'A_Z_W'                                             59
         CALL AXCOMPONENT                                                 59
C    *                                                                    59
C    Geant MIXTURE : CONCRETE ===> DENS=2.5 <===                          59
         CALL AGSRESET                                                    60
         AG_DENS = 2.5                                                    60
         AG_TITLE = 'CONCRETE'                                            60
         AG_EXNAME = 'MIXTURE'                                            60
         AG_PARLIST = 'DENS'                                              60
         CALL AXMIXTURE                                                   60
C    *                                                                    60
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           60
         CALL AGSRESET                                                    61
         AG_TITLE = 'STANDARD'                                            61
         AG_EXNAME = 'MEDIUM'                                             61
         AG_PARLIST = 'NONE'                                              61
         CALL AXMEDIUM                                                    61
C    *                                                                    61
C    Geant ATTRIBUTE : HALL ===> SEEN=1 COLO=2 <===                       61
         CALL AGSRESET                                                    62
         AG_SEEN = 1                                                      62
         AG_COLO = 2                                                      62
         AG_TITLE = 'HALL'                                                62
         AG_EXNAME = 'ATTRIBUTE'                                          62
         AG_PARLIST = 'SEEN_COLO'                                         62
         CALL AXATTRIBUTE                                                 62
C    *                                                                    62
         D1=CAVE_RMAX(1) + CAVE_DCONC                                     62
         D2=CAVE_RMAX(2) + CAVE_DCONC                                     64
         Z1=CAVE_DZ(1) + CAVE_DCONC                                       65
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-CAVE_DZ(2), -Z1,-  69
C    Z1,Z1,Z1,CAVE_DZ(2)} RMN={CAVE_RMIN, CAVE_RMIN,CAVE_RMIN,CAVE_RMIN,  69
C    CAVE_RMIN,CAVE_RMIN}, RMX={D2,D2,D1,D1,D2,D2} <===                   69
         CALL AGSRESET                                                    69
         AG_PHI1 = 0                                                      69
         AG_DPHI = 360                                                    69
         AG_NZ = 6                                                        69
         AG_ZI(1)=-CAVE_DZ(2)                                             69
         AG_ZI(2)=-Z1                                                     69
         AG_ZI(3)=-Z1                                                     69
         AG_ZI(4)=Z1                                                      69
         AG_ZI(5)=Z1                                                      69
         AG_ZI(6)=CAVE_DZ(2)                                              69
         AG_RMN(1)=CAVE_RMIN                                              69
         AG_RMN(2)=CAVE_RMIN                                              69
         AG_RMN(3)=CAVE_RMIN                                              69
         AG_RMN(4)=CAVE_RMIN                                              69
         AG_RMN(5)=CAVE_RMIN                                              69
         AG_RMN(6)=CAVE_RMIN                                              69
         AG_RMX(1)=D2                                                     69
         AG_RMX(2)=D2                                                     69
         AG_RMX(3)=D1                                                     69
         AG_RMX(4)=D1                                                     69
         AG_RMX(5)=D2                                                     69
         AG_RMX(6)=D2                                                     69
         AG_TITLE = 'PCON'                                                69
         AG_EXNAME = 'SHAPE'                                              69
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           69
         CALL AXSHAPE                                                     69
C    *                                                                    69
C    Create CAVE :                                                        70
C    Call To Local Procedure CAVE                                         70
         ASSIGN 5050 TO BLOCK_CAVE                                        70
         GO TO  5040                                                      70
5050     CONTINUE                                                         70
C    *                                                                    70
C    Geant POSITION : CAVE ===> inherited arguments used <===             71
         CALL AGSRESET                                                    71
         AG_TITLE = 'CAVE'                                                71
         AG_EXNAME = 'POSITION'                                           71
         AG_PARLIST = 'NONE'                                              71
         CALL AXPOSITION                                                  71
C    *                                                                    71
5021  CALL AGSPOPB                                                        72
      GO TO BLOCK_HALL, (5020 ,5030)                                      72
C End of Local Procedure                                                  72
C ----------------------------------------------------------------------  73
C ------------ CAVE                                                       73
C Begin Local Procedure CAVE                                              74
C CAVE IS GSTAR CAVE WITH SUBSYSTEM ENVELOPES                             74
5040  CONTINUE                                                            74
         AG_TITLE='CAVE'                                                  74
         AG_MODULE=MODULE                                                 74
         AG_MARK='V00'                                                    74
         CALL AGSPUSH                                                     74
         AG_MODULE=MODULE                                                 74
C    *                                                                    74
C    Geant MATERIAL : AIR ===> inherited arguments used <===              75
         CALL AGSRESET                                                    75
         AG_TITLE = 'AIR'                                                 75
         AG_EXNAME = 'MATERIAL'                                           75
         AG_PARLIST = 'NONE'                                              75
         CALL AXMATERIAL                                                  75
C    *                                                                    75
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           75
         CALL AGSRESET                                                    76
         AG_TITLE = 'STANDARD'                                            76
         AG_EXNAME = 'MEDIUM'                                             76
         AG_PARLIST = 'NONE'                                              76
         CALL AXMEDIUM                                                    76
C    *                                                                    76
C    Geant MEDIUM : SOMETHING ===> STEMAX=100 <===                        76
         CALL AGSRESET                                                    77
         AG_STEMAX = 100                                                  77
         AG_TITLE = 'SOMETHING'                                           77
         AG_EXNAME = 'MEDIUM'                                             77
         AG_PARLIST = 'STEMAX'                                            77
         CALL AXMEDIUM                                                    77
C    *                                                                    77
C    Geant ATTRIBUTE : CAVE ===> SEEN=1 COLO=2 <===                       77
         CALL AGSRESET                                                    78
         AG_SEEN = 1                                                      78
         AG_COLO = 2                                                      78
         AG_TITLE = 'CAVE'                                                78
         AG_EXNAME = 'ATTRIBUTE'                                          78
         AG_PARLIST = 'SEEN_COLO'                                         78
         CALL AXATTRIBUTE                                                 78
C    *                                                                    78
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 ZI={-CAVE_DZ(2), -CAVE_DZ(1  78
C    ),-CAVE_DZ(1),+CAVE_DZ(1),+CAVE_DZ(1), +CAVE_DZ(2)} RMX={+CAVE_RMAX  78
C    (2),+CAVE_RMAX(2), +CAVE_RMAX(1),+CAVE_RMAX(1),+CAVE_RMAX(2), +CAVE  78
C    _RMAX(2)} <===                                                       78
         CALL AGSRESET                                                    83
         AG_PHI1 = 0                                                      83
         AG_DPHI = 360                                                    83
         AG_ZI(1)=-CAVE_DZ(2)                                             83
         AG_ZI(2)=-CAVE_DZ(1)                                             83
         AG_ZI(3)=-CAVE_DZ(1)                                             83
         AG_ZI(4)=+CAVE_DZ(1)                                             83
         AG_ZI(5)=+CAVE_DZ(1)                                             83
         AG_ZI(6)=+CAVE_DZ(2)                                             83
         AG_RMX(1)=+CAVE_RMAX(2)                                          83
         AG_RMX(2)=+CAVE_RMAX(2)                                          83
         AG_RMX(3)=+CAVE_RMAX(1)                                          83
         AG_RMX(4)=+CAVE_RMAX(1)                                          83
         AG_RMX(5)=+CAVE_RMAX(2)                                          83
         AG_RMX(6)=+CAVE_RMAX(2)                                          83
         AG_TITLE = 'PCON'                                                83
         AG_EXNAME = 'SHAPE'                                              83
         AG_PARLIST = 'PHI1_DPHI_ZI_RMX'                                  83
         CALL AXSHAPE                                                     83
C    *                                                                    83
C    Check CVCF_TPCREFSYS!=0                                              83
         IF (CVCF_TPCREFSYS.NE.0) THEN                                    84
C    REFERENCE SYSTEM TPCREFSYS IGNORED                                   85
         ENDIF                                                            86
5041  CALL AGSPOPB                                                        87
      GO TO BLOCK_CAVE, (5040 ,5050)                                      87
C End of Local Procedure                                                  87
      WRITE(LOUT,5060)'===>>  [End AgML Module CAVEGEO]  <<==='           87
5060  FORMAT(A32,/,/)                                                     87
C MODULE CAVEGEO                                                          89
      END                                                                 89
