C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PSUPGEO                                                  1
C PSUPGEO GEOMETRY FOR PIXEL SUPPORT AND INSERTION MECHANISM              1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PSUPGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'GEOMETRY FOR PIXEL SUPPORT AND INSERTION             1
     *MECHANISM'/                                                         1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR F VIDEBAEK                                                       1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'F VIDEBAEK'/                                  2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED NOV 23TH, 2013                                                  2
      CHARACTER*40 CREATED /'NOV 23TH, 2013'/                             3
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
      INTEGER BLOCK_PSUP                                                  4
      INTEGER BLOCK_PSBA                                                  65
      INTEGER BLOCK_PHBB                                                  65
      INTEGER BLOCK_PSHA                                                  65
      INTEGER BLOCK_PSHC                                                  65
      INTEGER BLOCK_PSHE                                                  65
      INTEGER BLOCK_PSHG                                                  65
      INTEGER BLOCK_PSHS                                                  65
      INTEGER BLOCK_PSHT                                                  65
      INTEGER BLOCK_PSHL                                                  65
      INTEGER BLOCK_PHHA                                                  65
      INTEGER BLOCK_PHHB                                                  65
      INTEGER BLOCK_PHHC                                                  65
      INTEGER BLOCK_PSHU                                                  65
      INTEGER BLOCK_PSHM                                                  65
      INTEGER BLOCK_PSHN                                                  65
      INTEGER BLOCK_PSHO                                                  65
      INTEGER BLOCK_PSHP                                                  65
      INTEGER BLOCK_PSHQ                                                  65
      INTEGER BLOCK_PSTO                                                  65
      INTEGER BLOCK_PSOC                                                  65
      INTEGER BLOCK_PSIC                                                  65
      INTEGER BLOCK_PSUC                                                  65
      INTEGER BLOCK_PSBC                                                  65
      INTEGER BLOCK_PSAL                                                  65
      INTEGER BLOCK_PSAK                                                  65
      INTEGER BLOCK_PSCC                                                  65
      INTEGER BLOCK_PSCI                                                  65
      INTEGER BLOCK_PSHI                                                  65
      INTEGER BLOCK_PSCL                                                  65
      INTEGER BLOCK_PSCK                                                  65
      INTEGER BLOCK_PSAR                                                  65
      INTEGER BLOCK_PSAT                                                  65
      INTEGER BLOCK_PSAH                                                  65
      INTEGER BLOCK_PSAS                                                  65
      INTEGER BLOCK_PSAA                                                  65
      INTEGER BLOCK_PSAB                                                  65
      INTEGER BLOCK_PSAC                                                  65
      INTEGER BLOCK_PSAD                                                  65
      INTEGER BLOCK_PSAE                                                  65
      INTEGER BLOCK_PSAF                                                  65
      INTEGER BLOCK_PSMB                                                  65
      INTEGER BLOCK_PSPA                                                  65
      INTEGER BLOCK_PSPB                                                  65
      INTEGER BLOCK_PSMD                                                  65
      INTEGER BLOCK_PSMI                                                  65
      INTEGER BLOCK_PSMJ                                                  65
      INTEGER BLOCK_PSMK                                                  65
      INTEGER BLOCK_PSML                                                  65
      INTEGER BLOCK_PSMM                                                  65
      INTEGER BLOCK_PSMN                                                  65
      INTEGER BLOCK_PSMO                                                  65
      INTEGER BLOCK_PSMP                                                  65
      INTEGER BLOCK_PSMQ                                                  65
      INTEGER BLOCK_PSMR                                                  65
      INTEGER BLOCK_PSMT                                                  65
      INTEGER BLOCK_PSCJ                                                  65
      INTEGER BLOCK_PSCM                                                  65
      INTEGER BLOCK_PSCN                                                  65
      INTEGER BLOCK_PSCO                                                  65
      INTEGER BLOCK_PSCP                                                  65
      INTEGER BLOCK_PSCQ                                                  65
      INTEGER BLOCK_PSCR                                                  65
      INTEGER BLOCK_PSCS                                                  65
      INTEGER BLOCK_PSCT                                                  65
      INTEGER BLOCK_PSCU                                                  65
      INTEGER BLOCK_PSCV                                                  65
      INTEGER BLOCK_PSCW                                                  65
      INTEGER BLOCK_PSDA                                                  65
      INTEGER BLOCK_PSDB                                                  65
      INTEGER BLOCK_PSDC                                                  65
      INTEGER BLOCK_PSDE                                                  65
      INTEGER BLOCK_PSDD                                                  65
C -                                                                       66
      REAL MTBP_DY(5)                                                     66
      EQUIVALENCE (BANK_MTBP(3),MTBP_DY,RE_MTBP_DY),(NAMES_MTBP(1),       66
     *NMTBP_DY)                                                           66
      EQUIVALENCE (COMMEN_MTBP(1),CMTBP_DY),(MAP_MTBP(1,1),MMTBP_DY)      66
      CHARACTER CMTBP_DY*40/' '/                                          66
      CHARACTER NMTBP_DY*12/'R.DY'/                                       66
      INTEGER MMTBP_DY(2)/5,1/                                            66
      REAL RE_MTBP_DY                                                     66
C -                                                                       66
      INTEGER LEN_MTBP,LLEN_MTBP,LINK_MTBP                                66
      PARAMETER (LEN_MTBP=7)                                              66
      PARAMETER (LLEN_MTBP=1)                                             66
      INTEGER FLAG_MTBP/0/,MAP_MTBP(2,LLEN_MTBP),IDNUM_MTBP(10)           66
      CHARACTER NAMES_MTBP(LLEN_MTBP)*12,COMMEN_MTBP(LLEN_MTBP)*40        66
      DATA IDNUM_MTBP/10*0/                                               66
      REAL BANK_MTBP(LEN_MTBP)                                            66
      CHARACTER*60 BPATH_MTBP/' '/                                        66
      SAVE FLAG_MTBP,MAP_MTBP,IDNUM_MTBP,NAMES_MTBP,COMMEN_MTBP,          66
     *BPATH_MTBP                                                          66
      DATA LINK_MTBP/0/,BANK_MTBP/LEN_MTBP*0/                             66
C *                                                                       66
      INTEGER *4 DEBUG /0/                                                67
      REAL *4 ZFIRST/-283./                                               69
      REAL *4 ZLAST/-57.4/                                                71
      REAL *4 DEXTTHK/0.1/                                                73
      REAL *4 DEXTSUPLEN/0.4/                                             75
      REAL *4 DEXTLEN/1.4/                                                77
      REAL *4 DEXTOUTRAD/7.5/                                             79
      REAL *4 DEXTINRAD/3.1/                                              81
      REAL *4 MTBDX/2.1/                                                  83
      REAL *4 MTBDZ/6.8/                                                  85
      REAL *4 I,XPOS, YPOS, ZPOS                                          87
C *                                                                       89
      CALL AGSBEGM(MODULE,IPRIN)                                          89
      IF (IPRIN.LT.0) RETURN                                              89
C Filling bank MTBP - MASS TERMINATIONBOARD PARAMATERS                    89
      BANK_TITLE='MASS TERMINATIONBOARD PARAMATERS'                       89
      BPATH_MTBP='MTBP*'                                                  89
      IDNUM_MTBP(1)=0                                                     90
C OFFSETS PER MTB IN Y                                                    90
      MTBP_DY(1)=0.0                                                      90
      MTBP_DY(2)=0.8                                                      90
      MTBP_DY(3)=1.3                                                      90
      MTBP_DY(4)=1.585                                                    90
      MTBP_DY(5)=1.585                                                    90
      CMTBP_DY='OFFSETS PER MTB IN Y'                                     90
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTBP',    91
     *BPATH_MTBP,IDNUM_MTBP,1, MAP_MTBP,NAMES_MTBP,COMMEN_MTBP,           91
     *BANK_MTBP,LEN_MTBP,LLEN_MTBP,LINK_MTBP,FLAG_MTBP)                   91
C *                                                                       91
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTBP',    91
     *BPATH_MTBP,IDNUM_MTBP,1, MAP_MTBP,NAMES_MTBP,COMMEN_MTBP,           91
     *BANK_MTBP,LEN_MTBP,LLEN_MTBP,LINK_MTBP,FLAG_MTBP)                   91
      CALL AGSBEGM(MODULE,IPRIN)                                          92
C Check IPRIN<0                                                           92
      IF (IPRIN.LT.0) RETURN                                              92
C Create PSUP :                                                           92
C Call To Local Procedure PSUP                                            92
      ASSIGN 5020 TO BLOCK_PSUP                                           92
      GO TO  5010                                                         92
5020  CONTINUE                                                            92
C *                                                                       92
C Geant POSITION : PSUP ===> MOTHER='IDSM' X=0 Y=0 Z=0 KONLY='MANY' <===  93
      CALL AGSRESET                                                       93
      AG_MOTHER = 'IDSM'                                                  93
      AG_X = 0                                                            93
      AG_Y = 0                                                            93
      AG_Z = 0                                                            93
      AG_KONLY = 'MANY'                                                   93
      AG_TITLE = 'PSUP'                                                   93
      AG_EXNAME = 'POSITION'                                              93
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   93
      CALL AXPOSITION                                                     93
C *                                                                       93
C Geant POSITION : PSUP ===> MOTHER='IDSM' X=0 Y=0 Z=0 ALPHAZ=180 KONLY=  93
C 'MANY' <===                                                             93
      CALL AGSRESET                                                       94
      AG_MOTHER = 'IDSM'                                                  94
      AG_X = 0                                                            94
      AG_Y = 0                                                            94
      AG_Z = 0                                                            94
      AG_ALPHAN=AG_ALPHAN+1                                               94
      AG_ALPHAK(AG_ALPHAN)=3                                              94
      AG_ALPHAL(AG_ALPHAN)=180                                            94
      AG_KONLY = 'MANY'                                                   94
      AG_TITLE = 'PSUP'                                                   94
      AG_EXNAME = 'POSITION'                                              94
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                            94
      CALL AXPOSITION                                                     94
C *                                                                       94
C ----------------------------------------------------------------------  94
C ------------ PSUP                                                       94
      CALL AGSENDM                                                        96
      RETURN                                                              96
C Begin Local Procedure PSUP                                              96
C PSUP PIXEL SUPPORT MOTHER VOLUME                                        96
5010  CONTINUE                                                            96
         AG_TITLE='PSUP'                                                  96
         AG_MODULE=MODULE                                                 96
         AG_MARK='V00'                                                    96
         CALL AGSPUSH                                                     96
         AG_MODULE=MODULE                                                 96
C    *                                                                    96
C    Geant MATERIAL : AIR ===> inherited arguments used <===              97
         CALL AGSRESET                                                    97
         AG_TITLE = 'AIR'                                                 97
         AG_EXNAME = 'MATERIAL'                                           97
         AG_PARLIST = 'NONE'                                              97
         CALL AXMATERIAL                                                  97
C    *                                                                    97
C    Geant ATTRIBUTE : PSUP ===> SEEN=0 <===                              97
         CALL AGSRESET                                                    98
         AG_SEEN = 0                                                      98
         AG_TITLE = 'PSUP'                                                98
         AG_EXNAME = 'ATTRIBUTE'                                          98
         AG_PARLIST = 'SEEN'                                              98
         CALL AXATTRIBUTE                                                 98
C    *                                                                    98
C    Geant SHAPE : PCON ===> PHI1=84 DPHI=192 NZ=3 ZI={ZFIRST, ZLAST-5,Z  98
C    LAST} RMN={2.5,2.5,2.5} RMX={21.5, 21.5,8.5} <===                    98
         CALL AGSRESET                                                    101
         AG_PHI1 = 84                                                     101
         AG_DPHI = 192                                                    101
         AG_NZ = 3                                                        101
         AG_ZI(1)=ZFIRST                                                  101
         AG_ZI(2)=ZLAST-5                                                 101
         AG_ZI(3)=ZLAST                                                   101
         AG_RMN(1)=2.5                                                    101
         AG_RMN(2)=2.5                                                    101
         AG_RMN(3)=2.5                                                    101
         AG_RMX(1)=21.5                                                   101
         AG_RMX(2)=21.5                                                   101
         AG_RMX(3)=8.5                                                    101
         AG_TITLE = 'PCON'                                                101
         AG_EXNAME = 'SHAPE'                                              101
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           101
         CALL AXSHAPE                                                     101
C    *                                                                    101
C    Create PSBA :                                                        102
C    Call To Local Procedure PSBA                                         102
         ASSIGN 5040 TO BLOCK_PSBA                                        102
         GO TO  5030                                                      102
5040     CONTINUE                                                         102
C    *                                                                    102
C    Geant POSITION : PSBA ===> X=0 Y=0 Z=-98.9675 <===                   103
         CALL AGSRESET                                                    103
         AG_X = 0                                                         103
         AG_Y = 0                                                         103
         AG_Z = -98.9675                                                  103
         AG_TITLE = 'PSBA'                                                103
         AG_EXNAME = 'POSITION'                                           103
         AG_PARLIST = 'X_Y_Z'                                             103
         CALL AXPOSITION                                                  103
C    *                                                                    103
C    Create PHBB :                                                        104
C    Call To Local Procedure PHBB                                         104
         ASSIGN 5060 TO BLOCK_PHBB                                        104
         GO TO  5050                                                      104
5060     CONTINUE                                                         104
C    *                                                                    104
C    Geant POSITION : PHBB ===> X=-15.8 Y=9.4 Z=-80.735 <===              105
         CALL AGSRESET                                                    105
         AG_X = -15.8                                                     105
         AG_Y = 9.4                                                       105
         AG_Z = -80.735                                                   105
         AG_TITLE = 'PHBB'                                                105
         AG_EXNAME = 'POSITION'                                           105
         AG_PARLIST = 'X_Y_Z'                                             105
         CALL AXPOSITION                                                  105
C    *                                                                    105
C    Geant POSITION : PHBB ===> X=-15.8 Y=-9.4 Z=-80.735 <===             105
         CALL AGSRESET                                                    106
         AG_X = -15.8                                                     106
         AG_Y = -9.4                                                      106
         AG_Z = -80.735                                                   106
         AG_TITLE = 'PHBB'                                                106
         AG_EXNAME = 'POSITION'                                           106
         AG_PARLIST = 'X_Y_Z'                                             106
         CALL AXPOSITION                                                  106
C    *                                                                    106
C    Geant POSITION : PHBB ===> X=-15.8 Y=-9.4 Z=-117.195 <===            106
         CALL AGSRESET                                                    107
         AG_X = -15.8                                                     107
         AG_Y = -9.4                                                      107
         AG_Z = -117.195                                                  107
         AG_TITLE = 'PHBB'                                                107
         AG_EXNAME = 'POSITION'                                           107
         AG_PARLIST = 'X_Y_Z'                                             107
         CALL AXPOSITION                                                  107
C    *                                                                    107
C    Geant POSITION : PHBB ===> X=-15.8 Y=9.4 Z=-117.195 <===             107
         CALL AGSRESET                                                    108
         AG_X = -15.8                                                     108
         AG_Y = 9.4                                                       108
         AG_Z = -117.195                                                  108
         AG_TITLE = 'PHBB'                                                108
         AG_EXNAME = 'POSITION'                                           108
         AG_PARLIST = 'X_Y_Z'                                             108
         CALL AXPOSITION                                                  108
C    *                                                                    108
C    Create PSHA :                                                        109
C    Call To Local Procedure PSHA                                         109
         ASSIGN 5080 TO BLOCK_PSHA                                        109
         GO TO  5070                                                      109
5080     CONTINUE                                                         109
C    *                                                                    109
C    Geant POSITION : PSHA ===> X=-18.95 Y=-4.05 Z=-75.345 <===           110
         CALL AGSRESET                                                    110
         AG_X = -18.95                                                    110
         AG_Y = -4.05                                                     110
         AG_Z = -75.345                                                   110
         AG_TITLE = 'PSHA'                                                110
         AG_EXNAME = 'POSITION'                                           110
         AG_PARLIST = 'X_Y_Z'                                             110
         CALL AXPOSITION                                                  110
C    *                                                                    110
C    Geant POSITION : PSHA ===> X=-18.95 Y=-4.05 Z=-77.245 <===           110
         CALL AGSRESET                                                    111
         AG_X = -18.95                                                    111
         AG_Y = -4.05                                                     111
         AG_Z = -77.245                                                   111
         AG_TITLE = 'PSHA'                                                111
         AG_EXNAME = 'POSITION'                                           111
         AG_PARLIST = 'X_Y_Z'                                             111
         CALL AXPOSITION                                                  111
C    *                                                                    111
C    Create PSHC :                                                        112
C    Call To Local Procedure PSHC                                         112
         ASSIGN 5100 TO BLOCK_PSHC                                        112
         GO TO  5090                                                      112
5100     CONTINUE                                                         112
C    *                                                                    112
C    Geant POSITION : PSHC ===> X=-18.95 Y=4.05 Z=-75.345 ALPHAZ=76.1303  113
C     <===                                                                113
         CALL AGSRESET                                                    113
         AG_X = -18.95                                                    113
         AG_Y = 4.05                                                      113
         AG_Z = -75.345                                                   113
         AG_ALPHAN=AG_ALPHAN+1                                            113
         AG_ALPHAK(AG_ALPHAN)=3                                           113
         AG_ALPHAL(AG_ALPHAN)=76.1303                                     113
         AG_TITLE = 'PSHC'                                                113
         AG_EXNAME = 'POSITION'                                           113
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      113
         CALL AXPOSITION                                                  113
C    *                                                                    113
C    Geant POSITION : PSHC ===> X=-18.95 Y=4.05 Z=-77.245 ALPHAZ=76.1303  113
C     <===                                                                113
         CALL AGSRESET                                                    114
         AG_X = -18.95                                                    114
         AG_Y = 4.05                                                      114
         AG_Z = -77.245                                                   114
         AG_ALPHAN=AG_ALPHAN+1                                            114
         AG_ALPHAK(AG_ALPHAN)=3                                           114
         AG_ALPHAL(AG_ALPHAN)=76.1303                                     114
         AG_TITLE = 'PSHC'                                                114
         AG_EXNAME = 'POSITION'                                           114
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      114
         CALL AXPOSITION                                                  114
C    *                                                                    114
C    Create PSHE :                                                        115
C    Call To Local Procedure PSHE                                         115
         ASSIGN 5120 TO BLOCK_PSHE                                        115
         GO TO  5110                                                      115
5120     CONTINUE                                                         115
C    *                                                                    115
C    Geant POSITION : PSHE ===> X=-12.1121 Y=-7.87944 Z=-75.345 ALPHAZ=-  116
C    12.5397 <===                                                         116
         CALL AGSRESET                                                    116
         AG_X = -12.1121                                                  116
         AG_Y = -7.87944                                                  116
         AG_Z = -75.345                                                   116
         AG_ALPHAN=AG_ALPHAN+1                                            116
         AG_ALPHAK(AG_ALPHAN)=3                                           116
         AG_ALPHAL(AG_ALPHAN)=-12.5397                                    116
         AG_TITLE = 'PSHE'                                                116
         AG_EXNAME = 'POSITION'                                           116
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      116
         CALL AXPOSITION                                                  116
C    *                                                                    116
C    Create PSHE :                                                        117
C    Call To Local Procedure PSHE                                         117
         ASSIGN 5130 TO BLOCK_PSHE                                        117
         GO TO  5110                                                      117
5130     CONTINUE                                                         117
C    *                                                                    117
C    Geant POSITION : PSHE ===> X=-12.1121 Y=-7.87944 Z=-77.245 ALPHAZ=-  118
C    12.5397 <===                                                         118
         CALL AGSRESET                                                    118
         AG_X = -12.1121                                                  118
         AG_Y = -7.87944                                                  118
         AG_Z = -77.245                                                   118
         AG_ALPHAN=AG_ALPHAN+1                                            118
         AG_ALPHAK(AG_ALPHAN)=3                                           118
         AG_ALPHAL(AG_ALPHAN)=-12.5397                                    118
         AG_TITLE = 'PSHE'                                                118
         AG_EXNAME = 'POSITION'                                           118
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      118
         CALL AXPOSITION                                                  118
C    *                                                                    118
C    Create PSHG :                                                        119
C    Call To Local Procedure PSHG                                         119
         ASSIGN 5150 TO BLOCK_PSHG                                        119
         GO TO  5140                                                      119
5150     CONTINUE                                                         119
C    *                                                                    119
C    Geant POSITION : PSHG ===> X=-12.1121 Y=7.87944 Z=-75.345 ALPHAZ=12  120
C    .5397 <===                                                           120
         CALL AGSRESET                                                    120
         AG_X = -12.1121                                                  120
         AG_Y = 7.87944                                                   120
         AG_Z = -75.345                                                   120
         AG_ALPHAN=AG_ALPHAN+1                                            120
         AG_ALPHAK(AG_ALPHAN)=3                                           120
         AG_ALPHAL(AG_ALPHAN)=12.5397                                     120
         AG_TITLE = 'PSHG'                                                120
         AG_EXNAME = 'POSITION'                                           120
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      120
         CALL AXPOSITION                                                  120
C    *                                                                    120
C    Create PSHG :                                                        121
C    Call To Local Procedure PSHG                                         121
         ASSIGN 5160 TO BLOCK_PSHG                                        121
         GO TO  5140                                                      121
5160     CONTINUE                                                         121
C    *                                                                    121
C    Geant POSITION : PSHG ===> X=-12.1121 Y=7.87944 Z=-77.245 ALPHAZ=12  122
C    .5397 <===                                                           122
         CALL AGSRESET                                                    122
         AG_X = -12.1121                                                  122
         AG_Y = 7.87944                                                   122
         AG_Z = -77.245                                                   122
         AG_ALPHAN=AG_ALPHAN+1                                            122
         AG_ALPHAK(AG_ALPHAN)=3                                           122
         AG_ALPHAL(AG_ALPHAN)=12.5397                                     122
         AG_TITLE = 'PSHG'                                                122
         AG_EXNAME = 'POSITION'                                           122
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      122
         CALL AXPOSITION                                                  122
C    *                                                                    122
C    Create PSHS :                                                        123
C    Call To Local Procedure PSHS                                         123
         ASSIGN 5180 TO BLOCK_PSHS                                        123
         GO TO  5170                                                      123
5180     CONTINUE                                                         123
C    *                                                                    123
C    Geant POSITION : PSHS ===> X=-12.27 Y=7.9 Z=-72.727 <===             124
         CALL AGSRESET                                                    124
         AG_X = -12.27                                                    124
         AG_Y = 7.9                                                       124
         AG_Z = -72.727                                                   124
         AG_TITLE = 'PSHS'                                                124
         AG_EXNAME = 'POSITION'                                           124
         AG_PARLIST = 'X_Y_Z'                                             124
         CALL AXPOSITION                                                  124
C    *                                                                    124
C    Geant POSITION : PSHS ===> X=-12.27 Y=-7.9 Z=-72.727 <===            124
         CALL AGSRESET                                                    125
         AG_X = -12.27                                                    125
         AG_Y = -7.9                                                      125
         AG_Z = -72.727                                                   125
         AG_TITLE = 'PSHS'                                                125
         AG_EXNAME = 'POSITION'                                           125
         AG_PARLIST = 'X_Y_Z'                                             125
         CALL AXPOSITION                                                  125
C    *                                                                    125
C    Create PSHL :                                                        126
C    Call To Local Procedure PSHL                                         126
         ASSIGN 5200 TO BLOCK_PSHL                                        126
         GO TO  5190                                                      126
5200     CONTINUE                                                         126
C    *                                                                    126
C    Geant POSITION : PSHL ===> MOTHER='PSUP' X=-4.62 Y=0 Z=-69.57 ALPHA  127
C    Y=25.8+90.0 KONLY='MANY' <===                                        127
         CALL AGSRESET                                                    127
         AG_MOTHER = 'PSUP'                                               127
         AG_X = -4.62                                                     127
         AG_Y = 0                                                         127
         AG_Z = -69.57                                                    127
         AG_ALPHAN=AG_ALPHAN+1                                            127
         AG_ALPHAK(AG_ALPHAN)=2                                           127
         AG_ALPHAL(AG_ALPHAN)=25.8+90.0                                   127
         AG_KONLY = 'MANY'                                                127
         AG_TITLE = 'PSHL'                                                127
         AG_EXNAME = 'POSITION'                                           127
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_KONLY'                         127
         CALL AXPOSITION                                                  127
C    *                                                                    127
C    Create PSHU :                                                        128
C    Call To Local Procedure PSHU                                         128
         ASSIGN 5220 TO BLOCK_PSHU                                        128
         GO TO  5210                                                      128
5220     CONTINUE                                                         128
C    *                                                                    128
C    Geant POSITION : PSHU ===> MOTHER='PSUP' X=2.69 Y=0 Z=-78.38 ALPHAY  129
C    =25.8 KONLY='MANY' <===                                              129
         CALL AGSRESET                                                    129
         AG_MOTHER = 'PSUP'                                               129
         AG_X = 2.69                                                      129
         AG_Y = 0                                                         129
         AG_Z = -78.38                                                    129
         AG_ALPHAN=AG_ALPHAN+1                                            129
         AG_ALPHAK(AG_ALPHAN)=2                                           129
         AG_ALPHAL(AG_ALPHAN)=25.8                                        129
         AG_KONLY = 'MANY'                                                129
         AG_TITLE = 'PSHU'                                                129
         AG_EXNAME = 'POSITION'                                           129
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_KONLY'                         129
         CALL AXPOSITION                                                  129
C    *                                                                    129
C    Create PSHM :                                                        130
C    Call To Local Procedure PSHM                                         130
         ASSIGN 5240 TO BLOCK_PSHM                                        130
         GO TO  5230                                                      130
5240     CONTINUE                                                         130
C    *                                                                    130
C    Geant POSITION : PSHM ===> MOTHER='PSUP' X=0 Y=0 Z=-57.96 <===       131
         CALL AGSRESET                                                    131
         AG_MOTHER = 'PSUP'                                               131
         AG_X = 0                                                         131
         AG_Y = 0                                                         131
         AG_Z = -57.96                                                    131
         AG_TITLE = 'PSHM'                                                131
         AG_EXNAME = 'POSITION'                                           131
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      131
         CALL AXPOSITION                                                  131
C    *                                                                    131
C    Create PSHN :                                                        132
C    Call To Local Procedure PSHN                                         132
         ASSIGN 5260 TO BLOCK_PSHN                                        132
         GO TO  5250                                                      132
5260     CONTINUE                                                         132
C    *                                                                    132
C    Geant POSITION : PSHN ===> MOTHER='PSUP' X=0 Y=0 Z=-58.80 <===       133
         CALL AGSRESET                                                    133
         AG_MOTHER = 'PSUP'                                               133
         AG_X = 0                                                         133
         AG_Y = 0                                                         133
         AG_Z = -58.80                                                    133
         AG_TITLE = 'PSHN'                                                133
         AG_EXNAME = 'POSITION'                                           133
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      133
         CALL AXPOSITION                                                  133
C    *                                                                    133
C    Create PSHO :                                                        134
C    Call To Local Procedure PSHO                                         134
         ASSIGN 5280 TO BLOCK_PSHO                                        134
         GO TO  5270                                                      134
5280     CONTINUE                                                         134
C    *                                                                    134
C    Geant POSITION : PSHO ===> MOTHER='PSUP' X=5.7 Y=0 Z=-61.6 <===      135
         CALL AGSRESET                                                    135
         AG_MOTHER = 'PSUP'                                               135
         AG_X = 5.7                                                       135
         AG_Y = 0                                                         135
         AG_Z = -61.6                                                     135
         AG_TITLE = 'PSHO'                                                135
         AG_EXNAME = 'POSITION'                                           135
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      135
         CALL AXPOSITION                                                  135
C    *                                                                    135
C    Create PSHP :                                                        136
C    Call To Local Procedure PSHP                                         136
         ASSIGN 5300 TO BLOCK_PSHP                                        136
         GO TO  5290                                                      136
5300     CONTINUE                                                         136
C    *                                                                    136
C    Geant POSITION : PSHP ===> MOTHER='PSUP' X=0.0 Y=0 Z=0.0 KONLY='MAN  137
C    *Y' <===                                                             137
         CALL AGSRESET                                                    137
         AG_MOTHER = 'PSUP'                                               137
         AG_X = 0.0                                                       137
         AG_Y = 0                                                         137
         AG_Z = 0.0                                                       137
         AG_KONLY = 'MANY'                                                137
         AG_TITLE = 'PSHP'                                                137
         AG_EXNAME = 'POSITION'                                           137
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                137
         CALL AXPOSITION                                                  137
C    *                                                                    137
C    Create PSHQ :                                                        138
C    Call To Local Procedure PSHQ                                         138
         ASSIGN 5320 TO BLOCK_PSHQ                                        138
         GO TO  5310                                                      138
5320     CONTINUE                                                         138
C    *                                                                    138
C    Geant POSITION : PSHQ ===> MOTHER='PSUP' X=0.0 Y=0 Z=0 <===          139
         CALL AGSRESET                                                    139
         AG_MOTHER = 'PSUP'                                               139
         AG_X = 0.0                                                       139
         AG_Y = 0                                                         139
         AG_Z = 0                                                         139
         AG_TITLE = 'PSHQ'                                                139
         AG_EXNAME = 'POSITION'                                           139
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      139
         CALL AXPOSITION                                                  139
C    *                                                                    139
C    Create PSTO :                                                        140
C    Call To Local Procedure PSTO                                         140
         ASSIGN 5340 TO BLOCK_PSTO                                        140
         GO TO  5330                                                      140
5340     CONTINUE                                                         140
C    *                                                                    140
C    Geant POSITION : PSTO ===> X=-0.8-DEXTTHK/2.0 Y=5.09 Z=ZLAST-DEXTLE  141
C    N/2.0-DEXTSUPLEN <===                                                141
         CALL AGSRESET                                                    141
         AG_X = -0.8-DEXTTHK/2.0                                          141
         AG_Y = 5.09                                                      141
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              141
         AG_TITLE = 'PSTO'                                                141
         AG_EXNAME = 'POSITION'                                           141
         AG_PARLIST = 'X_Y_Z'                                             141
         CALL AXPOSITION                                                  141
C    *                                                                    141
C    Geant POSITION : PSTO ===> X=-0.8-DEXTTHK/2.0 Y=-5.09 Z=ZLAST-DEXTL  141
C    EN/2.0-DEXTSUPLEN <===                                               141
         CALL AGSRESET                                                    142
         AG_X = -0.8-DEXTTHK/2.0                                          142
         AG_Y = -5.09                                                     142
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              142
         AG_TITLE = 'PSTO'                                                142
         AG_EXNAME = 'POSITION'                                           142
         AG_PARLIST = 'X_Y_Z'                                             142
         CALL AXPOSITION                                                  142
C    *                                                                    142
C    Create PSOC :                                                        143
C    Call To Local Procedure PSOC                                         143
         ASSIGN 5360 TO BLOCK_PSOC                                        143
         GO TO  5350                                                      143
5360     CONTINUE                                                         143
C    *                                                                    143
C    Geant POSITION : PSOC ===> X=0 Y=0 Z=ZLAST-DEXTLEN/2.0-DEXTSUPLEN <  144
C    ===                                                                  144
         CALL AGSRESET                                                    144
         AG_X = 0                                                         144
         AG_Y = 0                                                         144
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              144
         AG_TITLE = 'PSOC'                                                144
         AG_EXNAME = 'POSITION'                                           144
         AG_PARLIST = 'X_Y_Z'                                             144
         CALL AXPOSITION                                                  144
C    *                                                                    144
C    Create PSIC :                                                        145
C    Call To Local Procedure PSIC                                         145
         ASSIGN 5380 TO BLOCK_PSIC                                        145
         GO TO  5370                                                      145
5380     CONTINUE                                                         145
C    *                                                                    145
C    Geant POSITION : PSIC ===> X=0 Y=0 Z=ZLAST-DEXTLEN/2.0-DEXTSUPLEN <  146
C    ===                                                                  146
         CALL AGSRESET                                                    146
         AG_X = 0                                                         146
         AG_Y = 0                                                         146
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              146
         AG_TITLE = 'PSIC'                                                146
         AG_EXNAME = 'POSITION'                                           146
         AG_PARLIST = 'X_Y_Z'                                             146
         CALL AXPOSITION                                                  146
C    *                                                                    146
C    Create PSUC :                                                        147
C    Call To Local Procedure PSUC                                         147
         ASSIGN 5400 TO BLOCK_PSUC                                        147
         GO TO  5390                                                      147
5400     CONTINUE                                                         147
C    *                                                                    147
C    Geant POSITION : PSUC ===> X=-1.6 Y=6.61 Z=ZLAST-DEXTLEN/2.0-DEXTSU  148
C    PLEN <===                                                            148
         CALL AGSRESET                                                    148
         AG_X = -1.6                                                      148
         AG_Y = 6.61                                                      148
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              148
         AG_TITLE = 'PSUC'                                                148
         AG_EXNAME = 'POSITION'                                           148
         AG_PARLIST = 'X_Y_Z'                                             148
         CALL AXPOSITION                                                  148
C    *                                                                    148
C    Geant POSITION : PSUC ===> X=-1.6 Y=-6.61 Z=ZLAST-DEXTLEN/2.0-DEXTS  148
C    UPLEN ALPHAX=180. <===                                               148
         CALL AGSRESET                                                    149
         AG_X = -1.6                                                      149
         AG_Y = -6.61                                                     149
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              149
         AG_ALPHAN=AG_ALPHAN+1                                            149
         AG_ALPHAK(AG_ALPHAN)=1                                           149
         AG_ALPHAL(AG_ALPHAN)=180.                                        149
         AG_TITLE = 'PSUC'                                                149
         AG_EXNAME = 'POSITION'                                           149
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      149
         CALL AXPOSITION                                                  149
C    *                                                                    149
C    Create PSBC :                                                        150
C    Call To Local Procedure PSBC                                         150
         ASSIGN 5420 TO BLOCK_PSBC                                        150
         GO TO  5410                                                      150
5420     CONTINUE                                                         150
C    *                                                                    150
C    Geant POSITION : PSBC ===> X=-1.6 Y=3.57 Z=ZLAST-DEXTLEN/2.0-DEXTSU  151
C    PLEN <===                                                            151
         CALL AGSRESET                                                    151
         AG_X = -1.6                                                      151
         AG_Y = 3.57                                                      151
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              151
         AG_TITLE = 'PSBC'                                                151
         AG_EXNAME = 'POSITION'                                           151
         AG_PARLIST = 'X_Y_Z'                                             151
         CALL AXPOSITION                                                  151
C    *                                                                    151
C    Geant POSITION : PSBC ===> X=-1.6 Y=-3.57 Z=ZLAST-DEXTLEN/2.0-DEXTS  151
C    UPLEN ALPHAX=180. <===                                               151
         CALL AGSRESET                                                    152
         AG_X = -1.6                                                      152
         AG_Y = -3.57                                                     152
         AG_Z = ZLAST-DEXTLEN/2.0-DEXTSUPLEN                              152
         AG_ALPHAN=AG_ALPHAN+1                                            152
         AG_ALPHAK(AG_ALPHAN)=1                                           152
         AG_ALPHAL(AG_ALPHAN)=180.                                        152
         AG_TITLE = 'PSBC'                                                152
         AG_EXNAME = 'POSITION'                                           152
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      152
         CALL AXPOSITION                                                  152
C    *                                                                    152
C    Create PSCC :                                                        153
C    Call To Local Procedure PSCC                                         153
         ASSIGN 5440 TO BLOCK_PSCC                                        153
         GO TO  5430                                                      153
5440     CONTINUE                                                         153
C    *                                                                    153
C    Geant POSITION : PSCC ===> X=0 Y=0 Z=-120-4.0/2.0 <===               154
         CALL AGSRESET                                                    154
         AG_X = 0                                                         154
         AG_Y = 0                                                         154
         AG_Z = -120-4.0/2.0                                              154
         AG_TITLE = 'PSCC'                                                154
         AG_EXNAME = 'POSITION'                                           154
         AG_PARLIST = 'X_Y_Z'                                             154
         CALL AXPOSITION                                                  154
C    *                                                                    154
C    Create PSHI :                                                        155
C    Call To Local Procedure PSHI                                         155
         ASSIGN 5460 TO BLOCK_PSHI                                        155
         GO TO  5450                                                      155
5460     CONTINUE                                                         155
C    *                                                                    155
C    Geant POSITION : PSHI ===> X=-17.94 Y=-(6.8-1.0/2) Z=-122.6 KONLY='  156
C    *MANY' <===                                                          156
         CALL AGSRESET                                                    156
         AG_X = -17.94                                                    156
         AG_Y = -(6.8-1.0/2)                                              156
         AG_Z = -122.6                                                    156
         AG_KONLY = 'MANY'                                                156
         AG_TITLE = 'PSHI'                                                156
         AG_EXNAME = 'POSITION'                                           156
         AG_PARLIST = 'X_Y_Z_KONLY'                                       156
         CALL AXPOSITION                                                  156
C    *                                                                    156
C    Geant POSITION : PSHI ===> X=-17.94 Y=+(6.8-1.0/2) Z=-122.6 KONLY='  156
C    *MANY' <===                                                          156
         CALL AGSRESET                                                    157
         AG_X = -17.94                                                    157
         AG_Y = +(6.8-1.0/2)                                              157
         AG_Z = -122.6                                                    157
         AG_KONLY = 'MANY'                                                157
         AG_TITLE = 'PSHI'                                                157
         AG_EXNAME = 'POSITION'                                           157
         AG_PARLIST = 'X_Y_Z_KONLY'                                       157
         CALL AXPOSITION                                                  157
C    *                                                                    157
C    Create PSAL :                                                        158
C    Call To Local Procedure PSAL                                         158
         ASSIGN 5480 TO BLOCK_PSAL                                        158
         GO TO  5470                                                      158
5480     CONTINUE                                                         158
C    *                                                                    158
C    Geant POSITION : PSAL ===> X=-7.85 Y=0 Z=-173.8 <===                 159
         CALL AGSRESET                                                    159
         AG_X = -7.85                                                     159
         AG_Y = 0                                                         159
         AG_Z = -173.8                                                    159
         AG_TITLE = 'PSAL'                                                159
         AG_EXNAME = 'POSITION'                                           159
         AG_PARLIST = 'X_Y_Z'                                             159
         CALL AXPOSITION                                                  159
C    *                                                                    159
C    Create PSAK :                                                        160
C    Call To Local Procedure PSAK                                         160
         ASSIGN 5500 TO BLOCK_PSAK                                        160
         GO TO  5490                                                      160
5500     CONTINUE                                                         160
C    *                                                                    160
C    Geant POSITION : PSAK ===> X=-7.85 Y=0 Z=-79.5-2.0 <===              161
         CALL AGSRESET                                                    161
         AG_X = -7.85                                                     161
         AG_Y = 0                                                         161
         AG_Z = -79.5-2.0                                                 161
         AG_TITLE = 'PSAK'                                                161
         AG_EXNAME = 'POSITION'                                           161
         AG_PARLIST = 'X_Y_Z'                                             161
         CALL AXPOSITION                                                  161
C    *                                                                    161
C    Create PSCL :                                                        162
C    Call To Local Procedure PSCL                                         162
         ASSIGN 5520 TO BLOCK_PSCL                                        162
         GO TO  5510                                                      162
5520     CONTINUE                                                         162
C    *                                                                    162
C    Geant POSITION : PSCL ===> X=-18.2+0.1 Y=0 Z=-(146+280.8)/2 <===     163
         CALL AGSRESET                                                    163
         AG_X = -18.2+0.1                                                 163
         AG_Y = 0                                                         163
         AG_Z = -(146+280.8)/2                                            163
         AG_TITLE = 'PSCL'                                                163
         AG_EXNAME = 'POSITION'                                           163
         AG_PARLIST = 'X_Y_Z'                                             163
         CALL AXPOSITION                                                  163
C    *                                                                    163
C    Create PSCK :                                                        164
C    Call To Local Procedure PSCK                                         164
         ASSIGN 5540 TO BLOCK_PSCK                                        164
         GO TO  5530                                                      164
5540     CONTINUE                                                         164
C    *                                                                    164
C    Geant POSITION : PSCK ===> X=-18.+2.7/2 Y=-6.44 Z=-(146+280.8)/2 <=  165
C    ==                                                                   165
         CALL AGSRESET                                                    165
         AG_X = -18.+2.7/2                                                165
         AG_Y = -6.44                                                     165
         AG_Z = -(146+280.8)/2                                            165
         AG_TITLE = 'PSCK'                                                165
         AG_EXNAME = 'POSITION'                                           165
         AG_PARLIST = 'X_Y_Z'                                             165
         CALL AXPOSITION                                                  165
C    *                                                                    165
C    Geant POSITION : PSCK ===> X=-18.+2.7/2 Y=+6.44 Z=-(146+280.8)/2 <=  165
C    ==                                                                   165
         CALL AGSRESET                                                    166
         AG_X = -18.+2.7/2                                                166
         AG_Y = +6.44                                                     166
         AG_Z = -(146+280.8)/2                                            166
         AG_TITLE = 'PSCK'                                                166
         AG_EXNAME = 'POSITION'                                           166
         AG_PARLIST = 'X_Y_Z'                                             166
         CALL AXPOSITION                                                  166
C    *                                                                    166
C    Create PSAR :                                                        167
C    Call To Local Procedure PSAR                                         167
         ASSIGN 5560 TO BLOCK_PSAR                                        167
         GO TO  5550                                                      167
5560     CONTINUE                                                         167
C    *                                                                    167
C    Geant POSITION : PSAR ===> X=-18.+0.7/2 Y=0 Z=-(129.53+146.03)/2 <=  168
C    ==                                                                   168
         CALL AGSRESET                                                    168
         AG_X = -18.+0.7/2                                                168
         AG_Y = 0                                                         168
         AG_Z = -(129.53+146.03)/2                                        168
         AG_TITLE = 'PSAR'                                                168
         AG_EXNAME = 'POSITION'                                           168
         AG_PARLIST = 'X_Y_Z'                                             168
         CALL AXPOSITION                                                  168
C    *                                                                    168
C    Create PSAT :                                                        169
C    Call To Local Procedure PSAT                                         169
         ASSIGN 5580 TO BLOCK_PSAT                                        169
         GO TO  5570                                                      169
5580     CONTINUE                                                         169
C    *                                                                    169
C    Geant POSITION : PSAT ===> X=-18.+0.7+4.5/2. Y=-6.8+0.55/2.0 Z=-(12  170
C    9.53+146.03)/2 <===                                                  170
         CALL AGSRESET                                                    170
         AG_X = -18.+0.7+4.5/2.                                           170
         AG_Y = -6.8+0.55/2.0                                             170
         AG_Z = -(129.53+146.03)/2                                        170
         AG_TITLE = 'PSAT'                                                170
         AG_EXNAME = 'POSITION'                                           170
         AG_PARLIST = 'X_Y_Z'                                             170
         CALL AXPOSITION                                                  170
C    *                                                                    170
C    Geant POSITION : PSAT ===> X=-18.+0.7+4.5/2. Y=+6.8-0.55/2.0 Z=-(12  170
C    9.53+146.03)/2 <===                                                  170
         CALL AGSRESET                                                    171
         AG_X = -18.+0.7+4.5/2.                                           171
         AG_Y = +6.8-0.55/2.0                                             171
         AG_Z = -(129.53+146.03)/2                                        171
         AG_TITLE = 'PSAT'                                                171
         AG_EXNAME = 'POSITION'                                           171
         AG_PARLIST = 'X_Y_Z'                                             171
         CALL AXPOSITION                                                  171
C    *                                                                    171
C    Create PSAS :                                                        172
C    Call To Local Procedure PSAS                                         172
         ASSIGN 5600 TO BLOCK_PSAS                                        172
         GO TO  5590                                                      172
5600     CONTINUE                                                         172
C    *                                                                    172
C    Geant POSITION : PSAS ===> X=-18.+0.35 Y=-4.0 Z=-148.53 <===         173
         CALL AGSRESET                                                    173
         AG_X = -18.+0.35                                                 173
         AG_Y = -4.0                                                      173
         AG_Z = -148.53                                                   173
         AG_TITLE = 'PSAS'                                                173
         AG_EXNAME = 'POSITION'                                           173
         AG_PARLIST = 'X_Y_Z'                                             173
         CALL AXPOSITION                                                  173
C    *                                                                    173
C    Geant POSITION : PSAS ===> X=-18.+0.35 Y=+4.0 Z=-148.53 <===         173
         CALL AGSRESET                                                    174
         AG_X = -18.+0.35                                                 174
         AG_Y = +4.0                                                      174
         AG_Z = -148.53                                                   174
         AG_TITLE = 'PSAS'                                                174
         AG_EXNAME = 'POSITION'                                           174
         AG_PARLIST = 'X_Y_Z'                                             174
         CALL AXPOSITION                                                  174
C    *                                                                    174
C    Create PSAA :                                                        175
C    Call To Local Procedure PSAA                                         175
         ASSIGN 5620 TO BLOCK_PSAA                                        175
         GO TO  5610                                                      175
5620     CONTINUE                                                         175
C    *                                                                    175
C    Geant POSITION : PSAA ===> X=-13.9275-0.15 Y=0 Z=-135.38 <===        176
         CALL AGSRESET                                                    176
         AG_X = -13.9275-0.15                                             176
         AG_Y = 0                                                         176
         AG_Z = -135.38                                                   176
         AG_TITLE = 'PSAA'                                                176
         AG_EXNAME = 'POSITION'                                           176
         AG_PARLIST = 'X_Y_Z'                                             176
         CALL AXPOSITION                                                  176
C    *                                                                    176
C    Create PSAB :                                                        177
C    Call To Local Procedure PSAB                                         177
         ASSIGN 5640 TO BLOCK_PSAB                                        177
         GO TO  5630                                                      177
5640     CONTINUE                                                         177
C    *                                                                    177
C    Geant POSITION : PSAB ===> X=-13.9275-0.3-1.92/2 Y=4.2 Z=-135.38 <=  178
C    ==                                                                   178
         CALL AGSRESET                                                    178
         AG_X = -13.9275-0.3-1.92/2                                       178
         AG_Y = 4.2                                                       178
         AG_Z = -135.38                                                   178
         AG_TITLE = 'PSAB'                                                178
         AG_EXNAME = 'POSITION'                                           178
         AG_PARLIST = 'X_Y_Z'                                             178
         CALL AXPOSITION                                                  178
C    *                                                                    178
C    Geant POSITION : PSAB ===> X=-13.9275-0.3-1.92/2 Y=-4.2 Z=-135.38 <  178
C    ===                                                                  178
         CALL AGSRESET                                                    179
         AG_X = -13.9275-0.3-1.92/2                                       179
         AG_Y = -4.2                                                      179
         AG_Z = -135.38                                                   179
         AG_TITLE = 'PSAB'                                                179
         AG_EXNAME = 'POSITION'                                           179
         AG_PARLIST = 'X_Y_Z'                                             179
         CALL AXPOSITION                                                  179
C    *                                                                    179
C    Create PSAD :                                                        180
C    Call To Local Procedure PSAD                                         180
         ASSIGN 5660 TO BLOCK_PSAD                                        180
         GO TO  5650                                                      180
5660     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : PSAD ===> X=-15.9517 Y=0 Z=-126.47 ALPHAY=90.0 KON  181
C    LY='MANY' <===                                                       181
         CALL AGSRESET                                                    181
         AG_X = -15.9517                                                  181
         AG_Y = 0                                                         181
         AG_Z = -126.47                                                   181
         AG_ALPHAN=AG_ALPHAN+1                                            181
         AG_ALPHAK(AG_ALPHAN)=2                                           181
         AG_ALPHAL(AG_ALPHAN)=90.0                                        181
         AG_KONLY = 'MANY'                                                181
         AG_TITLE = 'PSAD'                                                181
         AG_EXNAME = 'POSITION'                                           181
         AG_PARLIST = 'X_Y_Z_ALPHAY_KONLY'                                181
         CALL AXPOSITION                                                  181
C    *                                                                    181
C    Create PSAE :                                                        182
C    Call To Local Procedure PSAE                                         182
         ASSIGN 5680 TO BLOCK_PSAE                                        182
         GO TO  5670                                                      182
5680     CONTINUE                                                         182
C    *                                                                    182
C    Geant POSITION : PSAE ===> X=-18.12 Y=3.6 Z=-125.67 <===             183
         CALL AGSRESET                                                    183
         AG_X = -18.12                                                    183
         AG_Y = 3.6                                                       183
         AG_Z = -125.67                                                   183
         AG_TITLE = 'PSAE'                                                183
         AG_EXNAME = 'POSITION'                                           183
         AG_PARLIST = 'X_Y_Z'                                             183
         CALL AXPOSITION                                                  183
C    *                                                                    183
C    Geant POSITION : PSAE ===> X=-18.12 Y=-3.6 Z=-125.67 <===            183
         CALL AGSRESET                                                    184
         AG_X = -18.12                                                    184
         AG_Y = -3.6                                                      184
         AG_Z = -125.67                                                   184
         AG_TITLE = 'PSAE'                                                184
         AG_EXNAME = 'POSITION'                                           184
         AG_PARLIST = 'X_Y_Z'                                             184
         CALL AXPOSITION                                                  184
C    *                                                                    184
C    Create PSAF :                                                        185
C    Call To Local Procedure PSAF                                         185
         ASSIGN 5700 TO BLOCK_PSAF                                        185
         GO TO  5690                                                      185
5700     CONTINUE                                                         185
C    *                                                                    185
C    Geant POSITION : PSAF ===> X=-16.81 Y=0.0 Z=-126.12 <===             186
         CALL AGSRESET                                                    186
         AG_X = -16.81                                                    186
         AG_Y = 0.0                                                       186
         AG_Z = -126.12                                                   186
         AG_TITLE = 'PSAF'                                                186
         AG_EXNAME = 'POSITION'                                           186
         AG_PARLIST = 'X_Y_Z'                                             186
         CALL AXPOSITION                                                  186
C    *                                                                    186
C    *                                                                    186
         I=0                                                              187
            GO TO 5713                                                    187
5711        I=I+1                                                         187
5713        IF(I-(4).GT.0) GO TO 5712                                     187
            XPOS = -7.27+I*2.1                                            188
            YPOS = 13.575+MTBP_DY(I+1)                                    189
            ZPOS=- 108.85-6.8*I                                           190
C       Create PSMB :                                                     191
C       Call To Local Procedure PSMB                                      191
            ASSIGN 5730 TO BLOCK_PSMB                                     191
            GO TO  5720                                                   191
5730        CONTINUE                                                      191
C       *                                                                 191
C       Check I.EQ.4                                                      192
            IF (I.EQ.4) THEN                                              192
C       Geant POSITION : PSMB ===> X=-XPOS Y=-YPOS Z=ZPOS <===            193
            CALL AGSRESET                                                 193
            AG_X = -XPOS                                                  193
            AG_Y = -YPOS                                                  193
            AG_Z = ZPOS                                                   193
            AG_TITLE = 'PSMB'                                             193
            AG_EXNAME = 'POSITION'                                        193
            AG_PARLIST = 'X_Y_Z'                                          193
            CALL AXPOSITION                                               193
C       *                                                                 193
            ELSE                                                          193
C       Geant POSITION : PSMB ===> X=XPOS Y=YPOS Z=ZPOS <===              195
            CALL AGSRESET                                                 195
            AG_X = XPOS                                                   195
            AG_Y = YPOS                                                   195
            AG_Z = ZPOS                                                   195
            AG_TITLE = 'PSMB'                                             195
            AG_EXNAME = 'POSITION'                                        195
            AG_PARLIST = 'X_Y_Z'                                          195
            CALL AXPOSITION                                               195
C       *                                                                 195
            ENDIF                                                         195
            XPOS = -7.5467-0.06+I*2.1                                     197
            YPOS = 13.575+MTBP_DY(I+1)                                    198
            ZPOS=- 94.29-6.8*I                                            199
C       Create PSPA :                                                     200
C       Call To Local Procedure PSPA                                      200
            ASSIGN 5750 TO BLOCK_PSPA                                     200
            GO TO  5740                                                   200
5750        CONTINUE                                                      200
C       *                                                                 200
C       Check I.EQ.4                                                      201
            IF (I.EQ.4) THEN                                              201
C       Geant POSITION : PSPA ===> X=-XPOS Y=-YPOS Z=ZPOS <===            202
            CALL AGSRESET                                                 202
            AG_X = -XPOS                                                  202
            AG_Y = -YPOS                                                  202
            AG_Z = ZPOS                                                   202
            AG_TITLE = 'PSPA'                                             202
            AG_EXNAME = 'POSITION'                                        202
            AG_PARLIST = 'X_Y_Z'                                          202
            CALL AXPOSITION                                               202
C       *                                                                 202
            ELSE                                                          202
C       Geant POSITION : PSPA ===> X=XPOS Y=YPOS Z=ZPOS <===              204
            CALL AGSRESET                                                 204
            AG_X = XPOS                                                   204
            AG_Y = YPOS                                                   204
            AG_Z = ZPOS                                                   204
            AG_TITLE = 'PSPA'                                             204
            AG_EXNAME = 'POSITION'                                        204
            AG_PARLIST = 'X_Y_Z'                                          204
            CALL AXPOSITION                                               204
C       *                                                                 204
            ENDIF                                                         204
            XPOS = -7.52+0.25+I*2.1                                       206
            YPOS = 13.575+MTBP_DY(I+1)                                    207
            ZPOS=- 87.60-6.8*I                                            208
C       Create PSPB :                                                     209
C       Call To Local Procedure PSPB                                      209
            ASSIGN 5770 TO BLOCK_PSPB                                     209
            GO TO  5760                                                   209
5770        CONTINUE                                                      209
C       *                                                                 209
C       Check I.EQ.4                                                      210
            IF (I.EQ.4) THEN                                              210
C       Geant POSITION : PSPB ===> X=-XPOS Y=-YPOS Z=ZPOS <===            211
            CALL AGSRESET                                                 211
            AG_X = -XPOS                                                  211
            AG_Y = -YPOS                                                  211
            AG_Z = ZPOS                                                   211
            AG_TITLE = 'PSPB'                                             211
            AG_EXNAME = 'POSITION'                                        211
            AG_PARLIST = 'X_Y_Z'                                          211
            CALL AXPOSITION                                               211
C       *                                                                 211
            ELSE                                                          211
C       Geant POSITION : PSPB ===> X=XPOS Y=YPOS Z=ZPOS <===              213
            CALL AGSRESET                                                 213
            AG_X = XPOS                                                   213
            AG_Y = YPOS                                                   213
            AG_Z = ZPOS                                                   213
            AG_TITLE = 'PSPB'                                             213
            AG_EXNAME = 'POSITION'                                        213
            AG_PARLIST = 'X_Y_Z'                                          213
            CALL AXPOSITION                                               213
C       *                                                                 213
            ENDIF                                                         213
            XPOS = -8.2+I*2.1                                             215
            YPOS = 8.214+MTBP_DY(I+1)                                     216
            ZPOS=- 119.68-6.8*I                                           217
C       Create PSMT :                                                     218
C       Call To Local Procedure PSMT                                      218
            ASSIGN 5790 TO BLOCK_PSMT                                     218
            GO TO  5780                                                   218
5790        CONTINUE                                                      218
C       *                                                                 218
C       Check I.EQ.4                                                      219
            IF (I.EQ.4) THEN                                              219
C       Geant POSITION : PSMT ===> X=-XPOS Y=-YPOS Z=ZPOS <===            220
            CALL AGSRESET                                                 220
            AG_X = -XPOS                                                  220
            AG_Y = -YPOS                                                  220
            AG_Z = ZPOS                                                   220
            AG_TITLE = 'PSMT'                                             220
            AG_EXNAME = 'POSITION'                                        220
            AG_PARLIST = 'X_Y_Z'                                          220
            CALL AXPOSITION                                               220
C       *                                                                 220
            ELSE                                                          220
C       Geant POSITION : PSMT ===> X=XPOS Y=YPOS Z=ZPOS <===              222
            CALL AGSRESET                                                 222
            AG_X = XPOS                                                   222
            AG_Y = YPOS                                                   222
            AG_Z = ZPOS                                                   222
            AG_TITLE = 'PSMT'                                             222
            AG_EXNAME = 'POSITION'                                        222
            AG_PARLIST = 'X_Y_Z'                                          222
            CALL AXPOSITION                                               222
C       *                                                                 222
            ENDIF                                                         222
            XPOS = -8.0+I*2.1                                             224
            YPOS = 15.8+MTBP_DY(I+1)                                      225
            ZPOS = -126.6+1.5-6.8*I                                       226
C       Create PSCJ :                                                     227
C       Call To Local Procedure PSCJ                                      227
            ASSIGN 5810 TO BLOCK_PSCJ                                     227
            GO TO  5800                                                   227
5810        CONTINUE                                                      227
C       *                                                                 227
C       Check I.EQ.4                                                      228
            IF (I.EQ.4) THEN                                              228
C       Geant POSITION : PSCJ ===> X=-XPOS Y=-YPOS Z=ZPOS <===            229
            CALL AGSRESET                                                 229
            AG_X = -XPOS                                                  229
            AG_Y = -YPOS                                                  229
            AG_Z = ZPOS                                                   229
            AG_TITLE = 'PSCJ'                                             229
            AG_EXNAME = 'POSITION'                                        229
            AG_PARLIST = 'X_Y_Z'                                          229
            CALL AXPOSITION                                               229
C       *                                                                 229
            ELSE                                                          229
C       Geant POSITION : PSCJ ===> X=XPOS Y=YPOS Z=ZPOS <===              231
            CALL AGSRESET                                                 231
            AG_X = XPOS                                                   231
            AG_Y = YPOS                                                   231
            AG_Z = ZPOS                                                   231
            AG_TITLE = 'PSCJ'                                             231
            AG_EXNAME = 'POSITION'                                        231
            AG_PARLIST = 'X_Y_Z'                                          231
            CALL AXPOSITION                                               231
C       *                                                                 231
            ENDIF                                                         231
            YPOS = 11.2+MTBP_DY(I+1)                                      233
C       Create PSCJ :                                                     234
C       Call To Local Procedure PSCJ                                      234
            ASSIGN 5820 TO BLOCK_PSCJ                                     234
            GO TO  5800                                                   234
5820        CONTINUE                                                      234
C       *                                                                 234
C       Check I.EQ.4                                                      235
            IF (I.EQ.4) THEN                                              235
C       Geant POSITION : PSCJ ===> X=-XPOS Y=-YPOS Z=ZPOS <===            236
            CALL AGSRESET                                                 236
            AG_X = -XPOS                                                  236
            AG_Y = -YPOS                                                  236
            AG_Z = ZPOS                                                   236
            AG_TITLE = 'PSCJ'                                             236
            AG_EXNAME = 'POSITION'                                        236
            AG_PARLIST = 'X_Y_Z'                                          236
            CALL AXPOSITION                                               236
C       *                                                                 236
            ELSE                                                          236
C       Geant POSITION : PSCJ ===> X=XPOS Y=YPOS Z=ZPOS <===              238
            CALL AGSRESET                                                 238
            AG_X = XPOS                                                   238
            AG_Y = YPOS                                                   238
            AG_Z = ZPOS                                                   238
            AG_TITLE = 'PSCJ'                                             238
            AG_EXNAME = 'POSITION'                                        238
            AG_PARLIST = 'X_Y_Z'                                          238
            CALL AXPOSITION                                               238
C       *                                                                 238
            ENDIF                                                         238
         GO TO 5711                                                       240
5712     CONTINUE                                                         240
C    Create PSCM :                                                        241
C    Call To Local Procedure PSCM                                         241
         ASSIGN 5840 TO BLOCK_PSCM                                        241
         GO TO  5830                                                      241
5840     CONTINUE                                                         241
C    *                                                                    241
C    Geant POSITION : PSCM ===> X=-8 Y=15.8 Z=-136.6 <===                 242
         CALL AGSRESET                                                    242
         AG_X = -8                                                        242
         AG_Y = 15.8                                                      242
         AG_Z = -136.6                                                    242
         AG_TITLE = 'PSCM'                                                242
         AG_EXNAME = 'POSITION'                                           242
         AG_PARLIST = 'X_Y_Z'                                             242
         CALL AXPOSITION                                                  242
C    *                                                                    242
C    Geant POSITION : PSCM ===> X=-5.9 Y=16.6 Z=-143.4 <===               242
         CALL AGSRESET                                                    243
         AG_X = -5.9                                                      243
         AG_Y = 16.6                                                      243
         AG_Z = -143.4                                                    243
         AG_TITLE = 'PSCM'                                                243
         AG_EXNAME = 'POSITION'                                           243
         AG_PARLIST = 'X_Y_Z'                                             243
         CALL AXPOSITION                                                  243
C    *                                                                    243
C    Geant POSITION : PSCM ===> X=-3.8 Y=17.1 Z=-150.2 <===               243
         CALL AGSRESET                                                    244
         AG_X = -3.8                                                      244
         AG_Y = 17.1                                                      244
         AG_Z = -150.2                                                    244
         AG_TITLE = 'PSCM'                                                244
         AG_EXNAME = 'POSITION'                                           244
         AG_PARLIST = 'X_Y_Z'                                             244
         CALL AXPOSITION                                                  244
C    *                                                                    244
C    Geant POSITION : PSCM ===> X=-1.7 Y=17.385 Z=-157 <===               244
         CALL AGSRESET                                                    245
         AG_X = -1.7                                                      245
         AG_Y = 17.385                                                    245
         AG_Z = -157                                                      245
         AG_TITLE = 'PSCM'                                                245
         AG_EXNAME = 'POSITION'                                           245
         AG_PARLIST = 'X_Y_Z'                                             245
         CALL AXPOSITION                                                  245
C    *                                                                    245
C    Geant POSITION : PSCM ===> X=0.4 Y=17.385 Z=-163.8 <===              245
         CALL AGSRESET                                                    246
         AG_X = 0.4                                                       246
         AG_Y = 17.385                                                    246
         AG_Z = -163.8                                                    246
         AG_TITLE = 'PSCM'                                                246
         AG_EXNAME = 'POSITION'                                           246
         AG_PARLIST = 'X_Y_Z'                                             246
         CALL AXPOSITION                                                  246
C    *                                                                    246
C    Geant POSITION : PSCM ===> X=-8 Y=11.2 Z=-136.6 <===                 246
         CALL AGSRESET                                                    247
         AG_X = -8                                                        247
         AG_Y = 11.2                                                      247
         AG_Z = -136.6                                                    247
         AG_TITLE = 'PSCM'                                                247
         AG_EXNAME = 'POSITION'                                           247
         AG_PARLIST = 'X_Y_Z'                                             247
         CALL AXPOSITION                                                  247
C    *                                                                    247
C    Geant POSITION : PSCM ===> X=-5.9 Y=12 Z=-143.4 <===                 247
         CALL AGSRESET                                                    248
         AG_X = -5.9                                                      248
         AG_Y = 12                                                        248
         AG_Z = -143.4                                                    248
         AG_TITLE = 'PSCM'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'X_Y_Z'                                             248
         CALL AXPOSITION                                                  248
C    *                                                                    248
C    Geant POSITION : PSCM ===> X=-3.8 Y=12.5 Z=-150.2 <===               248
         CALL AGSRESET                                                    249
         AG_X = -3.8                                                      249
         AG_Y = 12.5                                                      249
         AG_Z = -150.2                                                    249
         AG_TITLE = 'PSCM'                                                249
         AG_EXNAME = 'POSITION'                                           249
         AG_PARLIST = 'X_Y_Z'                                             249
         CALL AXPOSITION                                                  249
C    *                                                                    249
C    Geant POSITION : PSCM ===> X=-1.7 Y=12.785 Z=-157 <===               249
         CALL AGSRESET                                                    250
         AG_X = -1.7                                                      250
         AG_Y = 12.785                                                    250
         AG_Z = -157                                                      250
         AG_TITLE = 'PSCM'                                                250
         AG_EXNAME = 'POSITION'                                           250
         AG_PARLIST = 'X_Y_Z'                                             250
         CALL AXPOSITION                                                  250
C    *                                                                    250
C    Geant POSITION : PSCM ===> X=0.4 Y=12.785 Z=-163.8 <===              250
         CALL AGSRESET                                                    251
         AG_X = 0.4                                                       251
         AG_Y = 12.785                                                    251
         AG_Z = -163.8                                                    251
         AG_TITLE = 'PSCM'                                                251
         AG_EXNAME = 'POSITION'                                           251
         AG_PARLIST = 'X_Y_Z'                                             251
         CALL AXPOSITION                                                  251
C    *                                                                    251
C    Create PSCN :                                                        252
C    Call To Local Procedure PSCN                                         252
         ASSIGN 5860 TO BLOCK_PSCN                                        252
         GO TO  5850                                                      252
5860     CONTINUE                                                         252
C    *                                                                    252
C    Geant POSITION : PSCN ===> X=-12.8 Y=10.15 Z=-156.6 ALPHAY=36.552 A  253
C    LPHAZ=49.6502 <===                                                   253
         CALL AGSRESET                                                    253
         AG_X = -12.8                                                     253
         AG_Y = 10.15                                                     253
         AG_Z = -156.6                                                    253
         AG_ALPHAN=AG_ALPHAN+1                                            253
         AG_ALPHAK(AG_ALPHAN)=2                                           253
         AG_ALPHAL(AG_ALPHAN)=36.552                                      253
         AG_ALPHAN=AG_ALPHAN+1                                            253
         AG_ALPHAK(AG_ALPHAN)=3                                           253
         AG_ALPHAL(AG_ALPHAN)=49.6502                                     253
         AG_TITLE = 'PSCN'                                                253
         AG_EXNAME = 'POSITION'                                           253
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               253
         CALL AXPOSITION                                                  253
C    *                                                                    253
C    Create PSCN :                                                        254
C    Call To Local Procedure PSCN                                         254
         ASSIGN 5870 TO BLOCK_PSCN                                        254
         GO TO  5850                                                      254
5870     CONTINUE                                                         254
C    *                                                                    254
C    Geant POSITION : PSCN ===> X=-12.8 Y=10.15-4.6 Z=-156.6 ALPHAY=36.5  255
C    52 ALPHAZ=49.6502 <===                                               255
         CALL AGSRESET                                                    255
         AG_X = -12.8                                                     255
         AG_Y = 10.15-4.6                                                 255
         AG_Z = -156.6                                                    255
         AG_ALPHAN=AG_ALPHAN+1                                            255
         AG_ALPHAK(AG_ALPHAN)=2                                           255
         AG_ALPHAL(AG_ALPHAN)=36.552                                      255
         AG_ALPHAN=AG_ALPHAN+1                                            255
         AG_ALPHAK(AG_ALPHAN)=3                                           255
         AG_ALPHAL(AG_ALPHAN)=49.6502                                     255
         AG_TITLE = 'PSCN'                                                255
         AG_EXNAME = 'POSITION'                                           255
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               255
         CALL AXPOSITION                                                  255
C    *                                                                    255
C    Create PSCS :                                                        256
C    Call To Local Procedure PSCS                                         256
         ASSIGN 5890 TO BLOCK_PSCS                                        256
         GO TO  5880                                                      256
5890     CONTINUE                                                         256
C    *                                                                    256
C    Geant POSITION : PSCS ===> X=-17.6 Y=4.5 Z=-213.3 <===               257
         CALL AGSRESET                                                    257
         AG_X = -17.6                                                     257
         AG_Y = 4.5                                                       257
         AG_Z = -213.3                                                    257
         AG_TITLE = 'PSCS'                                                257
         AG_EXNAME = 'POSITION'                                           257
         AG_PARLIST = 'X_Y_Z'                                             257
         CALL AXPOSITION                                                  257
C    *                                                                    257
C    Geant POSITION : PSCS ===> X=-17.6 Y=4.5-4.6 Z=-213.3 <===           257
         CALL AGSRESET                                                    258
         AG_X = -17.6                                                     258
         AG_Y = 4.5-4.6                                                   258
         AG_Z = -213.3                                                    258
         AG_TITLE = 'PSCS'                                                258
         AG_EXNAME = 'POSITION'                                           258
         AG_PARLIST = 'X_Y_Z'                                             258
         CALL AXPOSITION                                                  258
C    *                                                                    258
C    Create PSCO :                                                        259
C    Call To Local Procedure PSCO                                         259
         ASSIGN 5910 TO BLOCK_PSCO                                        259
         GO TO  5900                                                      259
5910     CONTINUE                                                         259
C    *                                                                    259
C    Geant POSITION : PSCO ===> X=-11.75 Y=10.1 Z=-163.4 ALPHAY=41.1692   260
C    ALPHAZ=48.0128 <===                                                  260
         CALL AGSRESET                                                    260
         AG_X = -11.75                                                    260
         AG_Y = 10.1                                                      260
         AG_Z = -163.4                                                    260
         AG_ALPHAN=AG_ALPHAN+1                                            260
         AG_ALPHAK(AG_ALPHAN)=2                                           260
         AG_ALPHAL(AG_ALPHAN)=41.1692                                     260
         AG_ALPHAN=AG_ALPHAN+1                                            260
         AG_ALPHAK(AG_ALPHAN)=3                                           260
         AG_ALPHAL(AG_ALPHAN)=48.0128                                     260
         AG_TITLE = 'PSCO'                                                260
         AG_EXNAME = 'POSITION'                                           260
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               260
         CALL AXPOSITION                                                  260
C    *                                                                    260
C    Geant POSITION : PSCO ===> X=-11.75 Y=10.1-4.6 Z=-163.4 ALPHAY=41.1  260
C    692 ALPHAZ=48.0128 <===                                              260
         CALL AGSRESET                                                    261
         AG_X = -11.75                                                    261
         AG_Y = 10.1-4.6                                                  261
         AG_Z = -163.4                                                    261
         AG_ALPHAN=AG_ALPHAN+1                                            261
         AG_ALPHAK(AG_ALPHAN)=2                                           261
         AG_ALPHAL(AG_ALPHAN)=41.1692                                     261
         AG_ALPHAN=AG_ALPHAN+1                                            261
         AG_ALPHAK(AG_ALPHAN)=3                                           261
         AG_ALPHAL(AG_ALPHAN)=48.0128                                     261
         AG_TITLE = 'PSCO'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               261
         CALL AXPOSITION                                                  261
C    *                                                                    261
C    Create PSCT :                                                        262
C    Call To Local Procedure PSCT                                         262
         ASSIGN 5930 TO BLOCK_PSCT                                        262
         GO TO  5920                                                      262
5930     CONTINUE                                                         262
C    *                                                                    262
C    Geant POSITION : PSCT ===> X=-17.6 Y=3.6 Z=-216.7 <===               263
         CALL AGSRESET                                                    263
         AG_X = -17.6                                                     263
         AG_Y = 3.6                                                       263
         AG_Z = -216.7                                                    263
         AG_TITLE = 'PSCT'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'X_Y_Z'                                             263
         CALL AXPOSITION                                                  263
C    *                                                                    263
C    Geant POSITION : PSCT ===> X=-17.6 Y=3.6-4.6 Z=-216.7 <===           263
         CALL AGSRESET                                                    264
         AG_X = -17.6                                                     264
         AG_Y = 3.6-4.6                                                   264
         AG_Z = -216.7                                                    264
         AG_TITLE = 'PSCT'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'X_Y_Z'                                             264
         CALL AXPOSITION                                                  264
C    *                                                                    264
C    Create PSCP :                                                        265
C    Call To Local Procedure PSCP                                         265
         ASSIGN 5950 TO BLOCK_PSCP                                        265
         GO TO  5940                                                      265
5950     CONTINUE                                                         265
C    *                                                                    265
C    Geant POSITION : PSCP ===> X=-10.7 Y=9.9 Z=-170.2 ALPHAY=44.921 ALP  266
C    HAZ=46.2189 <===                                                     266
         CALL AGSRESET                                                    266
         AG_X = -10.7                                                     266
         AG_Y = 9.9                                                       266
         AG_Z = -170.2                                                    266
         AG_ALPHAN=AG_ALPHAN+1                                            266
         AG_ALPHAK(AG_ALPHAN)=2                                           266
         AG_ALPHAL(AG_ALPHAN)=44.921                                      266
         AG_ALPHAN=AG_ALPHAN+1                                            266
         AG_ALPHAK(AG_ALPHAN)=3                                           266
         AG_ALPHAL(AG_ALPHAN)=46.2189                                     266
         AG_TITLE = 'PSCP'                                                266
         AG_EXNAME = 'POSITION'                                           266
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               266
         CALL AXPOSITION                                                  266
C    *                                                                    266
C    Create PSCP :                                                        267
C    Call To Local Procedure PSCP                                         267
         ASSIGN 5960 TO BLOCK_PSCP                                        267
         GO TO  5940                                                      267
5960     CONTINUE                                                         267
C    *                                                                    267
C    Geant POSITION : PSCP ===> X=-10.7 Y=9.9-4.6 Z=-170.2 ALPHAY=44.921  268
C     ALPHAZ=46.2189 <===                                                 268
         CALL AGSRESET                                                    268
         AG_X = -10.7                                                     268
         AG_Y = 9.9-4.6                                                   268
         AG_Z = -170.2                                                    268
         AG_ALPHAN=AG_ALPHAN+1                                            268
         AG_ALPHAK(AG_ALPHAN)=2                                           268
         AG_ALPHAL(AG_ALPHAN)=44.921                                      268
         AG_ALPHAN=AG_ALPHAN+1                                            268
         AG_ALPHAK(AG_ALPHAN)=3                                           268
         AG_ALPHAL(AG_ALPHAN)=46.2189                                     268
         AG_TITLE = 'PSCP'                                                268
         AG_EXNAME = 'POSITION'                                           268
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               268
         CALL AXPOSITION                                                  268
C    *                                                                    268
C    Create PSCU :                                                        269
C    Call To Local Procedure PSCU                                         269
         ASSIGN 5980 TO BLOCK_PSCU                                        269
         GO TO  5970                                                      269
5980     CONTINUE                                                         269
C    *                                                                    269
C    Geant POSITION : PSCU ===> X=-17.6 Y=2.7 Z=-220.1 <===               270
         CALL AGSRESET                                                    270
         AG_X = -17.6                                                     270
         AG_Y = 2.7                                                       270
         AG_Z = -220.1                                                    270
         AG_TITLE = 'PSCU'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'X_Y_Z'                                             270
         CALL AXPOSITION                                                  270
C    *                                                                    270
C    Geant POSITION : PSCU ===> X=-17.6 Y=2.7-4.6 Z=-220.1 <===           270
         CALL AGSRESET                                                    271
         AG_X = -17.6                                                     271
         AG_Y = 2.7-4.6                                                   271
         AG_Z = -220.1                                                    271
         AG_TITLE = 'PSCU'                                                271
         AG_EXNAME = 'POSITION'                                           271
         AG_PARLIST = 'X_Y_Z'                                             271
         CALL AXPOSITION                                                  271
C    *                                                                    271
C    Create PSCQ :                                                        272
C    Call To Local Procedure PSCQ                                         272
         ASSIGN 6000 TO BLOCK_PSCQ                                        272
         GO TO  5990                                                      272
6000     CONTINUE                                                         272
C    *                                                                    272
C    Geant POSITION : PSCQ ===> X=-9.65 Y=9.5925 Z=-177 ALPHAY=48.0668 A  273
C    LPHAZ=44.4268 <===                                                   273
         CALL AGSRESET                                                    273
         AG_X = -9.65                                                     273
         AG_Y = 9.5925                                                    273
         AG_Z = -177                                                      273
         AG_ALPHAN=AG_ALPHAN+1                                            273
         AG_ALPHAK(AG_ALPHAN)=2                                           273
         AG_ALPHAL(AG_ALPHAN)=48.0668                                     273
         AG_ALPHAN=AG_ALPHAN+1                                            273
         AG_ALPHAK(AG_ALPHAN)=3                                           273
         AG_ALPHAL(AG_ALPHAN)=44.4268                                     273
         AG_TITLE = 'PSCQ'                                                273
         AG_EXNAME = 'POSITION'                                           273
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               273
         CALL AXPOSITION                                                  273
C    *                                                                    273
C    Geant POSITION : PSCQ ===> X=-9.65 Y=9.5925-4.6 Z=-177 ALPHAY=48.06  273
C    68 ALPHAZ=44.4268 <===                                               273
         CALL AGSRESET                                                    274
         AG_X = -9.65                                                     274
         AG_Y = 9.5925-4.6                                                274
         AG_Z = -177                                                      274
         AG_ALPHAN=AG_ALPHAN+1                                            274
         AG_ALPHAK(AG_ALPHAN)=2                                           274
         AG_ALPHAL(AG_ALPHAN)=48.0668                                     274
         AG_ALPHAN=AG_ALPHAN+1                                            274
         AG_ALPHAK(AG_ALPHAN)=3                                           274
         AG_ALPHAL(AG_ALPHAN)=44.4268                                     274
         AG_TITLE = 'PSCQ'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Create PSCV :                                                        275
C    Call To Local Procedure PSCV                                         275
         ASSIGN 6020 TO BLOCK_PSCV                                        275
         GO TO  6010                                                      275
6020     CONTINUE                                                         275
C    *                                                                    275
C    Geant POSITION : PSCV ===> X=-17.6 Y=1.8 Z=-223.5 <===               276
         CALL AGSRESET                                                    276
         AG_X = -17.6                                                     276
         AG_Y = 1.8                                                       276
         AG_Z = -223.5                                                    276
         AG_TITLE = 'PSCV'                                                276
         AG_EXNAME = 'POSITION'                                           276
         AG_PARLIST = 'X_Y_Z'                                             276
         CALL AXPOSITION                                                  276
C    *                                                                    276
C    Geant POSITION : PSCV ===> X=-17.6 Y=1.8-4.6 Z=-223.5 <===           276
         CALL AGSRESET                                                    277
         AG_X = -17.6                                                     277
         AG_Y = 1.8-4.6                                                   277
         AG_Z = -223.5                                                    277
         AG_TITLE = 'PSCV'                                                277
         AG_EXNAME = 'POSITION'                                           277
         AG_PARLIST = 'X_Y_Z'                                             277
         CALL AXPOSITION                                                  277
C    *                                                                    277
C    Create PSCR :                                                        278
C    Call To Local Procedure PSCR                                         278
         ASSIGN 6040 TO BLOCK_PSCR                                        278
         GO TO  6030                                                      278
6040     CONTINUE                                                         278
C    *                                                                    278
C    Geant POSITION : PSCR ===> X=-8.6 Y=9.1425 Z=-183.8 ALPHAY=50.6688   280
C    ALPHAZ=42.4845 KONLY='MANY' <===                                     280
         CALL AGSRESET                                                    280
         AG_X = -8.6                                                      280
         AG_Y = 9.1425                                                    280
         AG_Z = -183.8                                                    280
         AG_ALPHAN=AG_ALPHAN+1                                            280
         AG_ALPHAK(AG_ALPHAN)=2                                           280
         AG_ALPHAL(AG_ALPHAN)=50.6688                                     280
         AG_ALPHAN=AG_ALPHAN+1                                            280
         AG_ALPHAK(AG_ALPHAN)=3                                           280
         AG_ALPHAL(AG_ALPHAN)=42.4845                                     280
         AG_KONLY = 'MANY'                                                280
         AG_TITLE = 'PSCR'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         280
         CALL AXPOSITION                                                  280
C    *                                                                    280
C    Geant POSITION : PSCR ===> X=-8.6 Y=9.1425-4.6 Z=-183.8 ALPHAY=50.6  280
C    688 ALPHAZ=42.4845 KONLY='MANY' <===                                 280
         CALL AGSRESET                                                    282
         AG_X = -8.6                                                      282
         AG_Y = 9.1425-4.6                                                282
         AG_Z = -183.8                                                    282
         AG_ALPHAN=AG_ALPHAN+1                                            282
         AG_ALPHAK(AG_ALPHAN)=2                                           282
         AG_ALPHAL(AG_ALPHAN)=50.6688                                     282
         AG_ALPHAN=AG_ALPHAN+1                                            282
         AG_ALPHAK(AG_ALPHAN)=3                                           282
         AG_ALPHAL(AG_ALPHAN)=42.4845                                     282
         AG_KONLY = 'MANY'                                                282
         AG_TITLE = 'PSCR'                                                282
         AG_EXNAME = 'POSITION'                                           282
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ_KONLY'                         282
         CALL AXPOSITION                                                  282
C    *                                                                    282
C    Create PSCW :                                                        283
C    Call To Local Procedure PSCW                                         283
         ASSIGN 6060 TO BLOCK_PSCW                                        283
         GO TO  6050                                                      283
6060     CONTINUE                                                         283
C    *                                                                    283
C    Geant POSITION : PSCW ===> X=-17.6 Y=0.9 Z=-226.9 <===               284
         CALL AGSRESET                                                    284
         AG_X = -17.6                                                     284
         AG_Y = 0.9                                                       284
         AG_Z = -226.9                                                    284
         AG_TITLE = 'PSCW'                                                284
         AG_EXNAME = 'POSITION'                                           284
         AG_PARLIST = 'X_Y_Z'                                             284
         CALL AXPOSITION                                                  284
C    *                                                                    284
C    Geant POSITION : PSCW ===> X=-17.6 Y=0.9-4.6 Z=-226.9 <===           284
         CALL AGSRESET                                                    285
         AG_X = -17.6                                                     285
         AG_Y = 0.9-4.6                                                   285
         AG_Z = -226.9                                                    285
         AG_TITLE = 'PSCW'                                                285
         AG_EXNAME = 'POSITION'                                           285
         AG_PARLIST = 'X_Y_Z'                                             285
         CALL AXPOSITION                                                  285
C    *                                                                    285
C    Create PSDA :                                                        286
C    Call To Local Procedure PSDA                                         286
         ASSIGN 6080 TO BLOCK_PSDA                                        286
         GO TO  6070                                                      286
6080     CONTINUE                                                         286
C    *                                                                    286
C    Geant POSITION : PSDA ===> X=-5.7 Y=14.4375 Z=-72.55 ALPHAY=10.7752  287
C     ALPHAZ=-21.7242 <===                                                287
         CALL AGSRESET                                                    287
         AG_X = -5.7                                                      287
         AG_Y = 14.4375                                                   287
         AG_Z = -72.55                                                    287
         AG_ALPHAN=AG_ALPHAN+1                                            287
         AG_ALPHAK(AG_ALPHAN)=2                                           287
         AG_ALPHAL(AG_ALPHAN)=10.7752                                     287
         AG_ALPHAN=AG_ALPHAN+1                                            287
         AG_ALPHAK(AG_ALPHAN)=3                                           287
         AG_ALPHAL(AG_ALPHAN)=-21.7242                                    287
         AG_TITLE = 'PSDA'                                                287
         AG_EXNAME = 'POSITION'                                           287
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               287
         CALL AXPOSITION                                                  287
C    *                                                                    287
C    Geant POSITION : PSDA ===> X=-5.7 Y=14.4375-1.0 Z=-72.55 ALPHAY=10.  287
C    7752 ALPHAZ=-21.7242 <===                                            287
         CALL AGSRESET                                                    288
         AG_X = -5.7                                                      288
         AG_Y = 14.4375-1.0                                               288
         AG_Z = -72.55                                                    288
         AG_ALPHAN=AG_ALPHAN+1                                            288
         AG_ALPHAK(AG_ALPHAN)=2                                           288
         AG_ALPHAL(AG_ALPHAN)=10.7752                                     288
         AG_ALPHAN=AG_ALPHAN+1                                            288
         AG_ALPHAK(AG_ALPHAN)=3                                           288
         AG_ALPHAL(AG_ALPHAN)=-21.7242                                    288
         AG_TITLE = 'PSDA'                                                288
         AG_EXNAME = 'POSITION'                                           288
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               288
         CALL AXPOSITION                                                  288
C    *                                                                    288
C    Geant POSITION : PSDA ===> X=-5.7 Y=14.4375-2.0 Z=-72.55 ALPHAY=10.  288
C    7752 ALPHAZ=-21.7242 <===                                            288
         CALL AGSRESET                                                    289
         AG_X = -5.7                                                      289
         AG_Y = 14.4375-2.0                                               289
         AG_Z = -72.55                                                    289
         AG_ALPHAN=AG_ALPHAN+1                                            289
         AG_ALPHAK(AG_ALPHAN)=2                                           289
         AG_ALPHAL(AG_ALPHAN)=10.7752                                     289
         AG_ALPHAN=AG_ALPHAN+1                                            289
         AG_ALPHAK(AG_ALPHAN)=3                                           289
         AG_ALPHAL(AG_ALPHAN)=-21.7242                                    289
         AG_TITLE = 'PSDA'                                                289
         AG_EXNAME = 'POSITION'                                           289
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               289
         CALL AXPOSITION                                                  289
C    *                                                                    289
C    Geant POSITION : PSDA ===> X=-5.7 Y=14.4375-3.0 Z=-72.55 ALPHAY=10.  289
C    7752 ALPHAZ=-21.7242 <===                                            289
         CALL AGSRESET                                                    290
         AG_X = -5.7                                                      290
         AG_Y = 14.4375-3.0                                               290
         AG_Z = -72.55                                                    290
         AG_ALPHAN=AG_ALPHAN+1                                            290
         AG_ALPHAK(AG_ALPHAN)=2                                           290
         AG_ALPHAL(AG_ALPHAN)=10.7752                                     290
         AG_ALPHAN=AG_ALPHAN+1                                            290
         AG_ALPHAK(AG_ALPHAN)=3                                           290
         AG_ALPHAL(AG_ALPHAN)=-21.7242                                    290
         AG_TITLE = 'PSDA'                                                290
         AG_EXNAME = 'POSITION'                                           290
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               290
         CALL AXPOSITION                                                  290
C    *                                                                    290
C    Create PSDB :                                                        291
C    Call To Local Procedure PSDB                                         291
         ASSIGN 6100 TO BLOCK_PSDB                                        291
         GO TO  6090                                                      291
6100     CONTINUE                                                         291
C    *                                                                    291
C    Geant POSITION : PSDB ===> X=-4.275 Y=14.8375 Z=-75.95 ALPHAY=6.370  292
C    38 ALPHAZ=-48.2809 <===                                              292
         CALL AGSRESET                                                    292
         AG_X = -4.275                                                    292
         AG_Y = 14.8375                                                   292
         AG_Z = -75.95                                                    292
         AG_ALPHAN=AG_ALPHAN+1                                            292
         AG_ALPHAK(AG_ALPHAN)=2                                           292
         AG_ALPHAL(AG_ALPHAN)=6.37038                                     292
         AG_ALPHAN=AG_ALPHAN+1                                            292
         AG_ALPHAK(AG_ALPHAN)=3                                           292
         AG_ALPHAL(AG_ALPHAN)=-48.2809                                    292
         AG_TITLE = 'PSDB'                                                292
         AG_EXNAME = 'POSITION'                                           292
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               292
         CALL AXPOSITION                                                  292
C    *                                                                    292
C    Geant POSITION : PSDB ===> X=-4.275 Y=14.8375-1. Z=-75.95 ALPHAY=6.  292
C    37038 ALPHAZ=-48.2809 <===                                           292
         CALL AGSRESET                                                    293
         AG_X = -4.275                                                    293
         AG_Y = 14.8375-1.                                                293
         AG_Z = -75.95                                                    293
         AG_ALPHAN=AG_ALPHAN+1                                            293
         AG_ALPHAK(AG_ALPHAN)=2                                           293
         AG_ALPHAL(AG_ALPHAN)=6.37038                                     293
         AG_ALPHAN=AG_ALPHAN+1                                            293
         AG_ALPHAK(AG_ALPHAN)=3                                           293
         AG_ALPHAL(AG_ALPHAN)=-48.2809                                    293
         AG_TITLE = 'PSDB'                                                293
         AG_EXNAME = 'POSITION'                                           293
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               293
         CALL AXPOSITION                                                  293
C    *                                                                    293
C    Geant POSITION : PSDB ===> X=-4.275 Y=14.8375-2 Z=-75.95 ALPHAY=6.3  293
C    7038 ALPHAZ=-48.2809 <===                                            293
         CALL AGSRESET                                                    294
         AG_X = -4.275                                                    294
         AG_Y = 14.8375-2                                                 294
         AG_Z = -75.95                                                    294
         AG_ALPHAN=AG_ALPHAN+1                                            294
         AG_ALPHAK(AG_ALPHAN)=2                                           294
         AG_ALPHAL(AG_ALPHAN)=6.37038                                     294
         AG_ALPHAN=AG_ALPHAN+1                                            294
         AG_ALPHAK(AG_ALPHAN)=3                                           294
         AG_ALPHAL(AG_ALPHAN)=-48.2809                                    294
         AG_TITLE = 'PSDB'                                                294
         AG_EXNAME = 'POSITION'                                           294
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               294
         CALL AXPOSITION                                                  294
C    *                                                                    294
C    Geant POSITION : PSDB ===> X=-4.275 Y=14.8375-3 Z=-75.95 ALPHAY=6.3  294
C    7038 ALPHAZ=-48.2809 <===                                            294
         CALL AGSRESET                                                    295
         AG_X = -4.275                                                    295
         AG_Y = 14.8375-3                                                 295
         AG_Z = -75.95                                                    295
         AG_ALPHAN=AG_ALPHAN+1                                            295
         AG_ALPHAK(AG_ALPHAN)=2                                           295
         AG_ALPHAL(AG_ALPHAN)=6.37038                                     295
         AG_ALPHAN=AG_ALPHAN+1                                            295
         AG_ALPHAK(AG_ALPHAN)=3                                           295
         AG_ALPHAL(AG_ALPHAN)=-48.2809                                    295
         AG_TITLE = 'PSDB'                                                295
         AG_EXNAME = 'POSITION'                                           295
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               295
         CALL AXPOSITION                                                  295
C    *                                                                    295
C    Create PSDC :                                                        296
C    Call To Local Procedure PSDC                                         296
         ASSIGN 6120 TO BLOCK_PSDC                                        296
         GO TO  6110                                                      296
6120     CONTINUE                                                         296
C    *                                                                    296
C    Geant POSITION : PSDC ===> X=-2.85 Y=15.0875 Z=-79.35 ALPHAY=4.7303  297
C     ALPHAZ=-79.0113 <===                                                297
         CALL AGSRESET                                                    297
         AG_X = -2.85                                                     297
         AG_Y = 15.0875                                                   297
         AG_Z = -79.35                                                    297
         AG_ALPHAN=AG_ALPHAN+1                                            297
         AG_ALPHAK(AG_ALPHAN)=2                                           297
         AG_ALPHAL(AG_ALPHAN)=4.7303                                      297
         AG_ALPHAN=AG_ALPHAN+1                                            297
         AG_ALPHAK(AG_ALPHAN)=3                                           297
         AG_ALPHAL(AG_ALPHAN)=-79.0113                                    297
         AG_TITLE = 'PSDC'                                                297
         AG_EXNAME = 'POSITION'                                           297
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               297
         CALL AXPOSITION                                                  297
C    *                                                                    297
C    Geant POSITION : PSDC ===> X=-2.85 Y=15.0875-1. Z=-79.35 ALPHAY=4.7  297
C    303 ALPHAZ=-79.0113 <===                                             297
         CALL AGSRESET                                                    298
         AG_X = -2.85                                                     298
         AG_Y = 15.0875-1.                                                298
         AG_Z = -79.35                                                    298
         AG_ALPHAN=AG_ALPHAN+1                                            298
         AG_ALPHAK(AG_ALPHAN)=2                                           298
         AG_ALPHAL(AG_ALPHAN)=4.7303                                      298
         AG_ALPHAN=AG_ALPHAN+1                                            298
         AG_ALPHAK(AG_ALPHAN)=3                                           298
         AG_ALPHAL(AG_ALPHAN)=-79.0113                                    298
         AG_TITLE = 'PSDC'                                                298
         AG_EXNAME = 'POSITION'                                           298
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               298
         CALL AXPOSITION                                                  298
C    *                                                                    298
C    Geant POSITION : PSDC ===> X=-2.85 Y=15.0875-2. Z=-79.35 ALPHAY=4.7  298
C    303 ALPHAZ=-79.0113 <===                                             298
         CALL AGSRESET                                                    299
         AG_X = -2.85                                                     299
         AG_Y = 15.0875-2.                                                299
         AG_Z = -79.35                                                    299
         AG_ALPHAN=AG_ALPHAN+1                                            299
         AG_ALPHAK(AG_ALPHAN)=2                                           299
         AG_ALPHAL(AG_ALPHAN)=4.7303                                      299
         AG_ALPHAN=AG_ALPHAN+1                                            299
         AG_ALPHAK(AG_ALPHAN)=3                                           299
         AG_ALPHAL(AG_ALPHAN)=-79.0113                                    299
         AG_TITLE = 'PSDC'                                                299
         AG_EXNAME = 'POSITION'                                           299
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               299
         CALL AXPOSITION                                                  299
C    *                                                                    299
C    Geant POSITION : PSDC ===> X=-2.85 Y=15.0875-3. Z=-79.35 ALPHAY=4.7  299
C    303 ALPHAZ=-79.0113 <===                                             299
         CALL AGSRESET                                                    300
         AG_X = -2.85                                                     300
         AG_Y = 15.0875-3.                                                300
         AG_Z = -79.35                                                    300
         AG_ALPHAN=AG_ALPHAN+1                                            300
         AG_ALPHAK(AG_ALPHAN)=2                                           300
         AG_ALPHAL(AG_ALPHAN)=4.7303                                      300
         AG_ALPHAN=AG_ALPHAN+1                                            300
         AG_ALPHAK(AG_ALPHAN)=3                                           300
         AG_ALPHAL(AG_ALPHAN)=-79.0113                                    300
         AG_TITLE = 'PSDC'                                                300
         AG_EXNAME = 'POSITION'                                           300
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               300
         CALL AXPOSITION                                                  300
C    *                                                                    300
C    Create PSDD :                                                        301
C    Call To Local Procedure PSDD                                         301
         ASSIGN 6140 TO BLOCK_PSDD                                        301
         GO TO  6130                                                      301
6140     CONTINUE                                                         301
C    *                                                                    301
C    Geant POSITION : PSDD ===> X=-1.425 Y=15.23 Z=-82.75 ALPHAY=4.4314   302
C    ALPHAZ=-106.552 <===                                                 302
         CALL AGSRESET                                                    302
         AG_X = -1.425                                                    302
         AG_Y = 15.23                                                     302
         AG_Z = -82.75                                                    302
         AG_ALPHAN=AG_ALPHAN+1                                            302
         AG_ALPHAK(AG_ALPHAN)=2                                           302
         AG_ALPHAL(AG_ALPHAN)=4.4314                                      302
         AG_ALPHAN=AG_ALPHAN+1                                            302
         AG_ALPHAK(AG_ALPHAN)=3                                           302
         AG_ALPHAL(AG_ALPHAN)=-106.552                                    302
         AG_TITLE = 'PSDD'                                                302
         AG_EXNAME = 'POSITION'                                           302
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               302
         CALL AXPOSITION                                                  302
C    *                                                                    302
C    Geant POSITION : PSDD ===> X=-1.425 Y=15.23-1. Z=-82.75 ALPHAY=4.43  302
C    14 ALPHAZ=-106.552 <===                                              302
         CALL AGSRESET                                                    303
         AG_X = -1.425                                                    303
         AG_Y = 15.23-1.                                                  303
         AG_Z = -82.75                                                    303
         AG_ALPHAN=AG_ALPHAN+1                                            303
         AG_ALPHAK(AG_ALPHAN)=2                                           303
         AG_ALPHAL(AG_ALPHAN)=4.4314                                      303
         AG_ALPHAN=AG_ALPHAN+1                                            303
         AG_ALPHAK(AG_ALPHAN)=3                                           303
         AG_ALPHAL(AG_ALPHAN)=-106.552                                    303
         AG_TITLE = 'PSDD'                                                303
         AG_EXNAME = 'POSITION'                                           303
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               303
         CALL AXPOSITION                                                  303
C    *                                                                    303
C    Geant POSITION : PSDD ===> X=-1.425 Y=15.23-2. Z=-82.75 ALPHAY=4.43  303
C    14 ALPHAZ=-106.552 <===                                              303
         CALL AGSRESET                                                    304
         AG_X = -1.425                                                    304
         AG_Y = 15.23-2.                                                  304
         AG_Z = -82.75                                                    304
         AG_ALPHAN=AG_ALPHAN+1                                            304
         AG_ALPHAK(AG_ALPHAN)=2                                           304
         AG_ALPHAL(AG_ALPHAN)=4.4314                                      304
         AG_ALPHAN=AG_ALPHAN+1                                            304
         AG_ALPHAK(AG_ALPHAN)=3                                           304
         AG_ALPHAL(AG_ALPHAN)=-106.552                                    304
         AG_TITLE = 'PSDD'                                                304
         AG_EXNAME = 'POSITION'                                           304
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               304
         CALL AXPOSITION                                                  304
C    *                                                                    304
C    Geant POSITION : PSDD ===> X=-1.425 Y=15.23-3. Z=-82.75 ALPHAY=4.43  304
C    14 ALPHAZ=-106.552 <===                                              304
         CALL AGSRESET                                                    305
         AG_X = -1.425                                                    305
         AG_Y = 15.23-3.                                                  305
         AG_Z = -82.75                                                    305
         AG_ALPHAN=AG_ALPHAN+1                                            305
         AG_ALPHAK(AG_ALPHAN)=2                                           305
         AG_ALPHAL(AG_ALPHAN)=4.4314                                      305
         AG_ALPHAN=AG_ALPHAN+1                                            305
         AG_ALPHAK(AG_ALPHAN)=3                                           305
         AG_ALPHAL(AG_ALPHAN)=-106.552                                    305
         AG_TITLE = 'PSDD'                                                305
         AG_EXNAME = 'POSITION'                                           305
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               305
         CALL AXPOSITION                                                  305
C    *                                                                    305
C    Create PSDE :                                                        306
C    Call To Local Procedure PSDE                                         306
         ASSIGN 6160 TO BLOCK_PSDE                                        306
         GO TO  6150                                                      306
6160     CONTINUE                                                         306
C    *                                                                    306
C    Geant POSITION : PSDE ===> X=4.44089E-16 Y=15.23 Z=-86.15 ALPHAY=4.  307
C    55415 ALPHAZ=-127.569 <===                                           307
         CALL AGSRESET                                                    307
         AG_X = 4.44089E-16                                               307
         AG_Y = 15.23                                                     307
         AG_Z = -86.15                                                    307
         AG_ALPHAN=AG_ALPHAN+1                                            307
         AG_ALPHAK(AG_ALPHAN)=2                                           307
         AG_ALPHAL(AG_ALPHAN)=4.55415                                     307
         AG_ALPHAN=AG_ALPHAN+1                                            307
         AG_ALPHAK(AG_ALPHAN)=3                                           307
         AG_ALPHAL(AG_ALPHAN)=-127.569                                    307
         AG_TITLE = 'PSDE'                                                307
         AG_EXNAME = 'POSITION'                                           307
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               307
         CALL AXPOSITION                                                  307
C    *                                                                    307
C    Geant POSITION : PSDE ===> X=4.44089E-16 Y=15.23-1 Z=-86.15 ALPHAY=  307
C    4.55415 ALPHAZ=-127.569 <===                                         307
         CALL AGSRESET                                                    308
         AG_X = 4.44089E-16                                               308
         AG_Y = 15.23-1                                                   308
         AG_Z = -86.15                                                    308
         AG_ALPHAN=AG_ALPHAN+1                                            308
         AG_ALPHAK(AG_ALPHAN)=2                                           308
         AG_ALPHAL(AG_ALPHAN)=4.55415                                     308
         AG_ALPHAN=AG_ALPHAN+1                                            308
         AG_ALPHAK(AG_ALPHAN)=3                                           308
         AG_ALPHAL(AG_ALPHAN)=-127.569                                    308
         AG_TITLE = 'PSDE'                                                308
         AG_EXNAME = 'POSITION'                                           308
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               308
         CALL AXPOSITION                                                  308
C    *                                                                    308
C    Geant POSITION : PSDE ===> X=4.44089E-16 Y=15.23-2 Z=-86.15 ALPHAY=  308
C    4.55415 ALPHAZ=-127.569 <===                                         308
         CALL AGSRESET                                                    309
         AG_X = 4.44089E-16                                               309
         AG_Y = 15.23-2                                                   309
         AG_Z = -86.15                                                    309
         AG_ALPHAN=AG_ALPHAN+1                                            309
         AG_ALPHAK(AG_ALPHAN)=2                                           309
         AG_ALPHAL(AG_ALPHAN)=4.55415                                     309
         AG_ALPHAN=AG_ALPHAN+1                                            309
         AG_ALPHAK(AG_ALPHAN)=3                                           309
         AG_ALPHAL(AG_ALPHAN)=-127.569                                    309
         AG_TITLE = 'PSDE'                                                309
         AG_EXNAME = 'POSITION'                                           309
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               309
         CALL AXPOSITION                                                  309
C    *                                                                    309
C    Geant POSITION : PSDE ===> X=4.44089E-16 Y=15.23-3 Z=-86.15 ALPHAY=  309
C    4.55415 ALPHAZ=-127.569 <===                                         309
         CALL AGSRESET                                                    310
         AG_X = 4.44089E-16                                               310
         AG_Y = 15.23-3                                                   310
         AG_Z = -86.15                                                    310
         AG_ALPHAN=AG_ALPHAN+1                                            310
         AG_ALPHAK(AG_ALPHAN)=2                                           310
         AG_ALPHAL(AG_ALPHAN)=4.55415                                     310
         AG_ALPHAN=AG_ALPHAN+1                                            310
         AG_ALPHAK(AG_ALPHAN)=3                                           310
         AG_ALPHAL(AG_ALPHAN)=-127.569                                    310
         AG_TITLE = 'PSDE'                                                310
         AG_EXNAME = 'POSITION'                                           310
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               310
         CALL AXPOSITION                                                  310
C    *                                                                    310
C    Create PSMD :                                                        311
C    Call To Local Procedure PSMD                                         311
         ASSIGN 6180 TO BLOCK_PSMD                                        311
         GO TO  6170                                                      311
6180     CONTINUE                                                         311
C    *                                                                    311
C    Geant POSITION : PSMD ===> X=-13.335 Y=6.55625 Z=-121.97 ALPHAZ=90   312
C    <===                                                                 312
         CALL AGSRESET                                                    312
         AG_X = -13.335                                                   312
         AG_Y = 6.55625                                                   312
         AG_Z = -121.97                                                   312
         AG_ALPHAN=AG_ALPHAN+1                                            312
         AG_ALPHAK(AG_ALPHAN)=3                                           312
         AG_ALPHAL(AG_ALPHAN)=90                                          312
         AG_TITLE = 'PSMD'                                                312
         AG_EXNAME = 'POSITION'                                           312
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      312
         CALL AXPOSITION                                                  312
C    *                                                                    312
C    Create PSMJ :                                                        313
C    Call To Local Procedure PSMJ                                         313
         ASSIGN 6200 TO BLOCK_PSMJ                                        313
         GO TO  6190                                                      313
6200     CONTINUE                                                         313
C    *                                                                    313
C    Geant POSITION : PSMJ ===> X=-9.15 Y=7.74 Z=-(123.97+101.87)/2 <===  314
         CALL AGSRESET                                                    314
         AG_X = -9.15                                                     314
         AG_Y = 7.74                                                      314
         AG_Z = -(123.97+101.87)/2                                        314
         AG_TITLE = 'PSMJ'                                                314
         AG_EXNAME = 'POSITION'                                           314
         AG_PARLIST = 'X_Y_Z'                                             314
         CALL AXPOSITION                                                  314
C    *                                                                    314
C    Create PSMK :                                                        315
C    Call To Local Procedure PSMK                                         315
         ASSIGN 6220 TO BLOCK_PSMK                                        315
         GO TO  6210                                                      315
6220     CONTINUE                                                         315
C    *                                                                    315
C    Geant POSITION : PSMK ===> X=-8.075 Y=7.71 Z=-113.829 ALPHAZ=90 ALP  316
C    HAX=90 <===                                                          316
         CALL AGSRESET                                                    316
         AG_X = -8.075                                                    316
         AG_Y = 7.71                                                      316
         AG_Z = -113.829                                                  316
         AG_ALPHAN=AG_ALPHAN+1                                            316
         AG_ALPHAK(AG_ALPHAN)=3                                           316
         AG_ALPHAL(AG_ALPHAN)=90                                          316
         AG_ALPHAN=AG_ALPHAN+1                                            316
         AG_ALPHAK(AG_ALPHAN)=1                                           316
         AG_ALPHAL(AG_ALPHAN)=90                                          316
         AG_TITLE = 'PSMK'                                                316
         AG_EXNAME = 'POSITION'                                           316
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ALPHAX'                               316
         CALL AXPOSITION                                                  316
C    *                                                                    316
C    Create PSML :                                                        317
C    Call To Local Procedure PSML                                         317
         ASSIGN 6240 TO BLOCK_PSML                                        317
         GO TO  6230                                                      317
6240     CONTINUE                                                         317
C    *                                                                    317
C    Geant POSITION : PSML ===> X=-6.05 Y=8.51 Z=-118.765 ALPHAZ=90 ALPH  318
C    AX=90 <===                                                           318
         CALL AGSRESET                                                    318
         AG_X = -6.05                                                     318
         AG_Y = 8.51                                                      318
         AG_Z = -118.765                                                  318
         AG_ALPHAN=AG_ALPHAN+1                                            318
         AG_ALPHAK(AG_ALPHAN)=3                                           318
         AG_ALPHAL(AG_ALPHAN)=90                                          318
         AG_ALPHAN=AG_ALPHAN+1                                            318
         AG_ALPHAK(AG_ALPHAN)=1                                           318
         AG_ALPHAL(AG_ALPHAN)=90                                          318
         AG_TITLE = 'PSML'                                                318
         AG_EXNAME = 'POSITION'                                           318
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ALPHAX'                               318
         CALL AXPOSITION                                                  318
C    *                                                                    318
C    Create PSMM :                                                        319
C    Call To Local Procedure PSMM                                         319
         ASSIGN 6260 TO BLOCK_PSMM                                        319
         GO TO  6250                                                      319
6260     CONTINUE                                                         319
C    *                                                                    319
C    Geant POSITION : PSMM ===> X=-3.95 Y=9.01 Z=-125.522 ALPHAZ=90 ALPH  320
C    AX=90 <===                                                           320
         CALL AGSRESET                                                    320
         AG_X = -3.95                                                     320
         AG_Y = 9.01                                                      320
         AG_Z = -125.522                                                  320
         AG_ALPHAN=AG_ALPHAN+1                                            320
         AG_ALPHAK(AG_ALPHAN)=3                                           320
         AG_ALPHAL(AG_ALPHAN)=90                                          320
         AG_ALPHAN=AG_ALPHAN+1                                            320
         AG_ALPHAK(AG_ALPHAN)=1                                           320
         AG_ALPHAL(AG_ALPHAN)=90                                          320
         AG_TITLE = 'PSMM'                                                320
         AG_EXNAME = 'POSITION'                                           320
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ALPHAX'                               320
         CALL AXPOSITION                                                  320
C    *                                                                    320
C    Create PSMN :                                                        321
C    Call To Local Procedure PSMN                                         321
         ASSIGN 6280 TO BLOCK_PSMN                                        321
         GO TO  6270                                                      321
6280     CONTINUE                                                         321
C    *                                                                    321
C    Geant POSITION : PSMN ===> X=-1.45 Y=9.3 Z=-133.617 ALPHAZ=90 ALPHA  322
C    X=90 <===                                                            322
         CALL AGSRESET                                                    322
         AG_X = -1.45                                                     322
         AG_Y = 9.3                                                       322
         AG_Z = -133.617                                                  322
         AG_ALPHAN=AG_ALPHAN+1                                            322
         AG_ALPHAK(AG_ALPHAN)=3                                           322
         AG_ALPHAL(AG_ALPHAN)=90                                          322
         AG_ALPHAN=AG_ALPHAN+1                                            322
         AG_ALPHAK(AG_ALPHAN)=1                                           322
         AG_ALPHAL(AG_ALPHAN)=90                                          322
         AG_TITLE = 'PSMN'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ALPHAX'                               322
         CALL AXPOSITION                                                  322
C    *                                                                    322
C    Create PSMO :                                                        323
C    Call To Local Procedure PSMO                                         323
         ASSIGN 6300 TO BLOCK_PSMO                                        323
         GO TO  6290                                                      323
6300     CONTINUE                                                         323
C    *                                                                    323
C    Geant POSITION : PSMO ===> X=-0.65 Y=-9.3 Z=-139.367 ALPHAX=90 ALPH  324
C    AY=90 ALPHAZ=180 <===                                                324
         CALL AGSRESET                                                    324
         AG_X = -0.65                                                     324
         AG_Y = -9.3                                                      324
         AG_Z = -139.367                                                  324
         AG_ALPHAN=AG_ALPHAN+1                                            324
         AG_ALPHAK(AG_ALPHAN)=1                                           324
         AG_ALPHAL(AG_ALPHAN)=90                                          324
         AG_ALPHAN=AG_ALPHAN+1                                            324
         AG_ALPHAK(AG_ALPHAN)=2                                           324
         AG_ALPHAL(AG_ALPHAN)=90                                          324
         AG_ALPHAN=AG_ALPHAN+1                                            324
         AG_ALPHAK(AG_ALPHAN)=3                                           324
         AG_ALPHAL(AG_ALPHAN)=180                                         324
         AG_TITLE = 'PSMO'                                                324
         AG_EXNAME = 'POSITION'                                           324
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAY_ALPHAZ'                        324
         CALL AXPOSITION                                                  324
C    *                                                                    324
C    Create PSMQ :                                                        325
C    Call To Local Procedure PSMQ                                         325
         ASSIGN 6320 TO BLOCK_PSMQ                                        325
         GO TO  6310                                                      325
6320     CONTINUE                                                         325
C    *                                                                    325
C    Geant POSITION : PSMQ ===> X=-7 Y=8.01 Z= -115.407 <===              326
         CALL AGSRESET                                                    326
         AG_X = -7                                                        326
         AG_Y = 8.01                                                      326
         AG_Z = -115.407                                                  326
         AG_TITLE = 'PSMQ'                                                326
         AG_EXNAME = 'POSITION'                                           326
         AG_PARLIST = 'X_Y_Z'                                             326
         CALL AXPOSITION                                                  326
C    *                                                                    326
C    Create PSMR :                                                        327
C    Call To Local Procedure PSMR                                         327
         ASSIGN 6340 TO BLOCK_PSMR                                        327
         GO TO  6330                                                      327
6340     CONTINUE                                                         327
C    *                                                                    327
C    Geant POSITION : PSMR ===> X=-4.9 Y=8.66 Z= -122.122 <===            328
         CALL AGSRESET                                                    328
         AG_X = -4.9                                                      328
         AG_Y = 8.66                                                      328
         AG_Z = -122.122                                                  328
         AG_TITLE = 'PSMR'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'X_Y_Z'                                             328
         CALL AXPOSITION                                                  328
C    *                                                                    328
C    Create PSMP :                                                        329
C    Call To Local Procedure PSMP                                         329
         ASSIGN 6360 TO BLOCK_PSMP                                        329
         GO TO  6350                                                      329
6360     CONTINUE                                                         329
C    *                                                                    329
C    Geant POSITION : PSMP ===> X=-1.4 Y=-9.9505 Z= -138.312 <===         330
         CALL AGSRESET                                                    330
         AG_X = -1.4                                                      330
         AG_Y = -9.9505                                                   330
         AG_Z = -138.312                                                  330
         AG_TITLE = 'PSMP'                                                330
         AG_EXNAME = 'POSITION'                                           330
         AG_PARLIST = 'X_Y_Z'                                             330
         CALL AXPOSITION                                                  330
C    *                                                                    330
5011  CALL AGSPOPB                                                        331
      GO TO BLOCK_PSUP, (5010 ,5020)                                      331
C End of Local Procedure                                                  331
C ----------------------------------------------------------------------  332
C ------------ PSBA                                                       332
C Begin Local Procedure PSBA                                              333
C PSBA CARRIAGE BASE                                                      333
5030  CONTINUE                                                            333
         AG_TITLE='PSBA'                                                  333
         AG_MODULE=MODULE                                                 333
         AG_MARK='V00'                                                    333
         CALL AGSPUSH                                                     333
         AG_MODULE=MODULE                                                 333
C    *                                                                    333
C    Geant ATTRIBUTE : PSBA ===> SEEN=1 COLO=4 <===                       334
         CALL AGSRESET                                                    334
         AG_SEEN = 1                                                      334
         AG_COLO = 4                                                      334
         AG_TITLE = 'PSBA'                                                334
         AG_EXNAME = 'ATTRIBUTE'                                          334
         AG_PARLIST = 'SEEN_COLO'                                         334
         CALL AXATTRIBUTE                                                 334
C    *                                                                    334
C    Geant MATERIAL : AL20 ===> A=26.8 Z=13 DENS=2.7*0.2 ABSL=37.2/0.2 R  334
C    ADL=8.9/0.2 <===                                                     334
         CALL AGSRESET                                                    335
         AG_A = 26.8                                                      335
         AG_Z = 13                                                        335
         AG_DENS = 2.7*0.2                                                335
         AG_ABSL = 37.2/0.2                                               335
         AG_RADL = 8.9/0.2                                                335
         AG_TITLE = 'AL20'                                                335
         AG_EXNAME = 'MATERIAL'                                           335
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                335
         CALL AXMATERIAL                                                  335
C    *                                                                    335
C    Geant SHAPE : TUBS ===> RMIN=18.5 RMAX=21.2 PHI1=158, PHI2=202 DZ=2  335
C    1.00 <===                                                            335
         CALL AGSRESET                                                    337
         AG_RMIN = 18.5                                                   337
         AG_RMAX = 21.2                                                   337
         AG_PHI1 = 158                                                    337
         AG_PHI2 = 202                                                    337
         AG_DZ = 21.00                                                    337
         AG_TITLE = 'TUBS'                                                337
         AG_EXNAME = 'SHAPE'                                              337
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            337
         CALL AXSHAPE                                                     337
C    *                                                                    337
5031  CALL AGSPOPB                                                        338
      GO TO BLOCK_PSBA, (5030 ,5040)                                      338
C End of Local Procedure                                                  338
C ----------------------------------------------------------------------  339
C ------------ PHBB                                                       339
C Begin Local Procedure PHBB                                              340
C PHBB CARRIAGE SUPPORT RING                                              340
5050  CONTINUE                                                            340
         AG_TITLE='PHBB'                                                  340
         AG_MODULE=MODULE                                                 340
         AG_MARK='V00'                                                    340
         CALL AGSPUSH                                                     340
         AG_MODULE=MODULE                                                 340
C    *                                                                    340
C    Geant ATTRIBUTE : PHBB ===> SEEN=1 COLO=4 <===                       341
         CALL AGSRESET                                                    341
         AG_SEEN = 1                                                      341
         AG_COLO = 4                                                      341
         AG_TITLE = 'PHBB'                                                341
         AG_EXNAME = 'ATTRIBUTE'                                          341
         AG_PARLIST = 'SEEN_COLO'                                         341
         CALL AXATTRIBUTE                                                 341
C    *                                                                    341
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        341
         CALL AGSRESET                                                    342
         AG_TITLE = 'ALUMINIUM'                                           342
         AG_EXNAME = 'MATERIAL'                                           342
         AG_PARLIST = 'NONE'                                              342
         CALL AXMATERIAL                                                  342
C    *                                                                    342
C    Geant SHAPE : TUBE ===> RMIN=1.65 RMAX=1.95 DZ=2.75 <===             342
         CALL AGSRESET                                                    343
         AG_RMIN = 1.65                                                   343
         AG_RMAX = 1.95                                                   343
         AG_DZ = 2.75                                                     343
         AG_TITLE = 'TUBE'                                                343
         AG_EXNAME = 'SHAPE'                                              343
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      343
         CALL AXSHAPE                                                     343
C    *                                                                    343
5051  CALL AGSPOPB                                                        344
      GO TO BLOCK_PHBB, (5050 ,5060)                                      344
C End of Local Procedure                                                  344
C ----------------------------------------------------------------------  345
C ------------ PSHA                                                       345
C Begin Local Procedure PSHA                                              346
C PSHA PSHA HINGE RELEASE PARTA (1)                                       346
5070  CONTINUE                                                            346
         AG_TITLE='PSHA'                                                  346
         AG_MODULE=MODULE                                                 346
         AG_MARK='V00'                                                    346
         CALL AGSPUSH                                                     346
         AG_MODULE=MODULE                                                 346
C    *                                                                    346
C    Geant ATTRIBUTE : PSHA ===> SEEN=1 COLO=3 <===                       347
         CALL AGSRESET                                                    347
         AG_SEEN = 1                                                      347
         AG_COLO = 3                                                      347
         AG_TITLE = 'PSHA'                                                347
         AG_EXNAME = 'ATTRIBUTE'                                          347
         AG_PARLIST = 'SEEN_COLO'                                         347
         CALL AXATTRIBUTE                                                 347
C    *                                                                    347
C    Geant MATERIAL : AL40 ===> A=26.8 Z=13 DENS=2.7*0.4 ABSL=37.2/0.4 R  347
C    ADL=8.9/0.4 <===                                                     347
         CALL AGSRESET                                                    348
         AG_A = 26.8                                                      348
         AG_Z = 13                                                        348
         AG_DENS = 2.7*0.4                                                348
         AG_ABSL = 37.2/0.4                                               348
         AG_RADL = 8.9/0.4                                                348
         AG_TITLE = 'AL40'                                                348
         AG_EXNAME = 'MATERIAL'                                           348
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                348
         CALL AXMATERIAL                                                  348
C    *                                                                    348
C    Geant SHAPE : TRAP ===> DZ=0.725 THET=0 PHI=0 H1=4.05, BL1=1.25 TL1  348
C    =1.25 ALP1=-13.8697 H2=4.05 BL2=1.25, TL2=1.25 ALP2=-13.8697 <===    348
         CALL AGSRESET                                                    351
         AG_DZ = 0.725                                                    351
         AG_THET = 0                                                      351
         AG_PHI = 0                                                       351
         AG_H1 = 4.05                                                     351
         AG_BL1 = 1.25                                                    351
         AG_TL1 = 1.25                                                    351
         AG_ALP1 = -13.8697                                               351
         AG_H2 = 4.05                                                     351
         AG_BL2 = 1.25                                                    351
         AG_TL2 = 1.25                                                    351
         AG_ALP2 = -13.8697                                               351
         AG_TITLE = 'TRAP'                                                351
         AG_EXNAME = 'SHAPE'                                              351
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       351
         CALL AXSHAPE                                                     351
C    *                                                                    351
5071  CALL AGSPOPB                                                        352
      GO TO BLOCK_PSHA, (5070 ,5080)                                      352
C End of Local Procedure                                                  352
C ----------------------------------------------------------------------  353
C ------------ PSHC                                                       353
C Begin Local Procedure PSHC                                              354
C PSHC PSHC HINGE RELEASE PARTA (1)                                       354
5090  CONTINUE                                                            354
         AG_TITLE='PSHC'                                                  354
         AG_MODULE=MODULE                                                 354
         AG_MARK='V00'                                                    354
         CALL AGSPUSH                                                     354
         AG_MODULE=MODULE                                                 354
C    *                                                                    354
C    Geant ATTRIBUTE : PSHC ===> SEEN=1 COLO=3 <===                       355
         CALL AGSRESET                                                    355
         AG_SEEN = 1                                                      355
         AG_COLO = 3                                                      355
         AG_TITLE = 'PSHC'                                                355
         AG_EXNAME = 'ATTRIBUTE'                                          355
         AG_PARLIST = 'SEEN_COLO'                                         355
         CALL AXATTRIBUTE                                                 355
C    *                                                                    355
C    Geant MATERIAL : AL40 ===> inherited arguments used <===             355
         CALL AGSRESET                                                    356
         AG_TITLE = 'AL40'                                                356
         AG_EXNAME = 'MATERIAL'                                           356
         AG_PARLIST = 'NONE'                                              356
         CALL AXMATERIAL                                                  356
C    *                                                                    356
C    Geant SHAPE : TRAP ===> DZ=0.725 THET=0 PHI=0 H1=1.21355, BL1=4.171  356
C    63 TL1=4.17163 ALP1=-13.8697 H2=1.21355, BL2=4.17163 TL2=4.17163 AL  356
C    P2=-13.8697 <===                                                     356
         CALL AGSRESET                                                    359
         AG_DZ = 0.725                                                    359
         AG_THET = 0                                                      359
         AG_PHI = 0                                                       359
         AG_H1 = 1.21355                                                  359
         AG_BL1 = 4.17163                                                 359
         AG_TL1 = 4.17163                                                 359
         AG_ALP1 = -13.8697                                               359
         AG_H2 = 1.21355                                                  359
         AG_BL2 = 4.17163                                                 359
         AG_TL2 = 4.17163                                                 359
         AG_ALP2 = -13.8697                                               359
         AG_TITLE = 'TRAP'                                                359
         AG_EXNAME = 'SHAPE'                                              359
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       359
         CALL AXSHAPE                                                     359
C    *                                                                    359
5091  CALL AGSPOPB                                                        360
      GO TO BLOCK_PSHC, (5090 ,5100)                                      360
C End of Local Procedure                                                  360
C ----------------------------------------------------------------------  361
C ------------ PSHE                                                       361
C Begin Local Procedure PSHE                                              362
C PSHE PSHE HINGE RELEASE PARTA (1)                                       362
5110  CONTINUE                                                            362
         AG_TITLE='PSHE'                                                  362
         AG_MODULE=MODULE                                                 362
         AG_MARK='V00'                                                    362
         CALL AGSPUSH                                                     362
         AG_MODULE=MODULE                                                 362
C    *                                                                    362
C    Geant ATTRIBUTE : PSHE ===> SEEN=1 COLO=3 <===                       363
         CALL AGSRESET                                                    363
         AG_SEEN = 1                                                      363
         AG_COLO = 3                                                      363
         AG_TITLE = 'PSHE'                                                363
         AG_EXNAME = 'ATTRIBUTE'                                          363
         AG_PARLIST = 'SEEN_COLO'                                         363
         CALL AXATTRIBUTE                                                 363
C    *                                                                    363
C    Geant MATERIAL : AL40 ===> inherited arguments used <===             363
         CALL AGSRESET                                                    364
         AG_TITLE = 'AL40'                                                364
         AG_EXNAME = 'MATERIAL'                                           364
         AG_PARLIST = 'NONE'                                              364
         CALL AXMATERIAL                                                  364
C    *                                                                    364
C    Geant SHAPE : TRAP ===> DZ=0.725 THET=0 PHI=0 H1=1.2114, BL1=4.8660  364
C    8 TL1=5.19822 ALP1=-19.7742 H2=1.2114, BL2=4.86608 TL2=5.19822 ALP2  364
C    =-19.7742 <===                                                       364
         CALL AGSRESET                                                    367
         AG_DZ = 0.725                                                    367
         AG_THET = 0                                                      367
         AG_PHI = 0                                                       367
         AG_H1 = 1.2114                                                   367
         AG_BL1 = 4.86608                                                 367
         AG_TL1 = 5.19822                                                 367
         AG_ALP1 = -19.7742                                               367
         AG_H2 = 1.2114                                                   367
         AG_BL2 = 4.86608                                                 367
         AG_TL2 = 5.19822                                                 367
         AG_ALP2 = -19.7742                                               367
         AG_TITLE = 'TRAP'                                                367
         AG_EXNAME = 'SHAPE'                                              367
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       367
         CALL AXSHAPE                                                     367
C    *                                                                    367
5111  CALL AGSPOPB                                                        368
      GO TO BLOCK_PSHE, (5110 ,5120,5130)                                 368
C End of Local Procedure                                                  368
C ----------------------------------------------------------------------  369
C ------------ PSHG                                                       369
C Begin Local Procedure PSHG                                              370
C PSHG PSHG HINGE RELEASE PARTA (1)                                       370
5140  CONTINUE                                                            370
         AG_TITLE='PSHG'                                                  370
         AG_MODULE=MODULE                                                 370
         AG_MARK='V00'                                                    370
         CALL AGSPUSH                                                     370
         AG_MODULE=MODULE                                                 370
C    *                                                                    370
C    Geant ATTRIBUTE : PSHG ===> SEEN=1 COLO=3 <===                       371
         CALL AGSRESET                                                    371
         AG_SEEN = 1                                                      371
         AG_COLO = 3                                                      371
         AG_TITLE = 'PSHG'                                                371
         AG_EXNAME = 'ATTRIBUTE'                                          371
         AG_PARLIST = 'SEEN_COLO'                                         371
         CALL AXATTRIBUTE                                                 371
C    *                                                                    371
C    Geant MATERIAL : AL40 ===> inherited arguments used <===             371
         CALL AGSRESET                                                    372
         AG_TITLE = 'AL40'                                                372
         AG_EXNAME = 'MATERIAL'                                           372
         AG_PARLIST = 'NONE'                                              372
         CALL AXMATERIAL                                                  372
C    *                                                                    372
C    Geant SHAPE : TRAP ===> DZ=0.725 THET=0 PHI=0 H1=1.2114, BL1=5.1982  372
C    2 TL1=4.86608 ALP1=19.7742 H2=1.2114, BL2=5.19822 TL2=4.86608 ALP2=  372
C    19.7742 <===                                                         372
         CALL AGSRESET                                                    375
         AG_DZ = 0.725                                                    375
         AG_THET = 0                                                      375
         AG_PHI = 0                                                       375
         AG_H1 = 1.2114                                                   375
         AG_BL1 = 5.19822                                                 375
         AG_TL1 = 4.86608                                                 375
         AG_ALP1 = 19.7742                                                375
         AG_H2 = 1.2114                                                   375
         AG_BL2 = 5.19822                                                 375
         AG_TL2 = 4.86608                                                 375
         AG_ALP2 = 19.7742                                                375
         AG_TITLE = 'TRAP'                                                375
         AG_EXNAME = 'SHAPE'                                              375
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       375
         CALL AXSHAPE                                                     375
C    *                                                                    375
5141  CALL AGSPOPB                                                        376
      GO TO BLOCK_PSHG, (5140 ,5150,5160)                                 376
C End of Local Procedure                                                  376
C ----------------------------------------------------------------------  377
C ------------ PSHS                                                       377
C Begin Local Procedure PSHS                                              378
C PSHS PSHG HINGE RELEASE AIR CYLINDER                                    378
5170  CONTINUE                                                            378
         AG_TITLE='PSHS'                                                  378
         AG_MODULE=MODULE                                                 378
         AG_MARK='V00'                                                    378
         CALL AGSPUSH                                                     378
         AG_MODULE=MODULE                                                 378
C    *                                                                    378
C    Geant ATTRIBUTE : PSHS ===> SEEN=1 COLO=3 <===                       379
         CALL AGSRESET                                                    379
         AG_SEEN = 1                                                      379
         AG_COLO = 3                                                      379
         AG_TITLE = 'PSHS'                                                379
         AG_EXNAME = 'ATTRIBUTE'                                          379
         AG_PARLIST = 'SEEN_COLO'                                         379
         CALL AXATTRIBUTE                                                 379
C    *                                                                    379
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        379
         CALL AGSRESET                                                    380
         AG_TITLE = 'ALUMINIUM'                                           380
         AG_EXNAME = 'MATERIAL'                                           380
         AG_PARLIST = 'NONE'                                              380
         CALL AXMATERIAL                                                  380
C    *                                                                    380
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=3.3/2 DZ=3.786/2 <===            380
         CALL AGSRESET                                                    381
         AG_RMIN = 0                                                      381
         AG_RMAX = 3.3/2                                                  381
         AG_DZ = 3.786/2                                                  381
         AG_TITLE = 'TUBE'                                                381
         AG_EXNAME = 'SHAPE'                                              381
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      381
         CALL AXSHAPE                                                     381
C    *                                                                    381
C    Create PSHT :                                                        382
C    Call To Local Procedure PSHT                                         382
         ASSIGN 6380 TO BLOCK_PSHT                                        382
         GO TO  6370                                                      382
6380     CONTINUE                                                         382
C    *                                                                    382
C    Geant POSITION : PSHT ===> MOTHER='PSHS' X=0 Y=0 Z=-0.1 <===         383
         CALL AGSRESET                                                    383
         AG_MOTHER = 'PSHS'                                               383
         AG_X = 0                                                         383
         AG_Y = 0                                                         383
         AG_Z = -0.1                                                      383
         AG_TITLE = 'PSHT'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      383
         CALL AXPOSITION                                                  383
C    *                                                                    383
5171  CALL AGSPOPB                                                        384
      GO TO BLOCK_PSHS, (5170 ,5180)                                      384
C End of Local Procedure                                                  384
C ----------------------------------------------------------------------  385
C ------------ PSHT                                                       385
C Begin Local Procedure PSHT                                              386
C PSHT PSHG HINGE RELEASE AIR CYLINDER- INNER PART                        386
6370  CONTINUE                                                            386
         AG_TITLE='PSHT'                                                  386
         AG_MODULE=MODULE                                                 386
         AG_MARK='V00'                                                    386
         CALL AGSPUSH                                                     386
         AG_MODULE=MODULE                                                 386
C    *                                                                    386
C    Geant ATTRIBUTE : PSHT ===> SEEN=1 COLO=4 <===                       387
         CALL AGSRESET                                                    387
         AG_SEEN = 1                                                      387
         AG_COLO = 4                                                      387
         AG_TITLE = 'PSHT'                                                387
         AG_EXNAME = 'ATTRIBUTE'                                          387
         AG_PARLIST = 'SEEN_COLO'                                         387
         CALL AXATTRIBUTE                                                 387
C    *                                                                    387
C    Geant MATERIAL : AIR ===> inherited arguments used <===              387
         CALL AGSRESET                                                    388
         AG_TITLE = 'AIR'                                                 388
         AG_EXNAME = 'MATERIAL'                                           388
         AG_PARLIST = 'NONE'                                              388
         CALL AXMATERIAL                                                  388
C    *                                                                    388
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=2.9/2 DZ=(3.786-0.2)/2 <===      388
         CALL AGSRESET                                                    389
         AG_RMIN = 0                                                      389
         AG_RMAX = 2.9/2                                                  389
         AG_DZ = (3.786-0.2)/2                                            389
         AG_TITLE = 'TUBE'                                                389
         AG_EXNAME = 'SHAPE'                                              389
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      389
         CALL AXSHAPE                                                     389
C    *                                                                    389
6371  CALL AGSPOPB                                                        390
      GO TO BLOCK_PSHT, (6370 ,6380)                                      390
C End of Local Procedure                                                  390
C ----------------------------------------------------------------------  391
C ------------ PSHL                                                       391
C Begin Local Procedure PSHL                                              392
C PSHL HINGE LEAF INNER MOTHER VOLUME                                     392
5190  CONTINUE                                                            392
         AG_TITLE='PSHL'                                                  392
         AG_MODULE=MODULE                                                 392
         AG_MARK='V00'                                                    392
         CALL AGSPUSH                                                     392
         AG_MODULE=MODULE                                                 392
C    *                                                                    392
C    Geant ATTRIBUTE : PSHL ===> SEEN=1 COLO=7 <===                       393
         CALL AGSRESET                                                    393
         AG_SEEN = 1                                                      393
         AG_COLO = 7                                                      393
         AG_TITLE = 'PSHL'                                                393
         AG_EXNAME = 'ATTRIBUTE'                                          393
         AG_PARLIST = 'SEEN_COLO'                                         393
         CALL AXATTRIBUTE                                                 393
C    *                                                                    393
C    Geant MATERIAL : AIR ===> inherited arguments used <===              393
         CALL AGSRESET                                                    394
         AG_TITLE = 'AIR'                                                 394
         AG_EXNAME = 'MATERIAL'                                           394
         AG_PARLIST = 'NONE'                                              394
         CALL AXMATERIAL                                                  394
C    *                                                                    394
C    Geant SHAPE : BOX ===> DX=9.0/2.0 DY=19.75/2.0 DZ=1.3/2.0 <===       394
         CALL AGSRESET                                                    395
         AG_DX = 9.0/2.0                                                  395
         AG_DY = 19.75/2.0                                                395
         AG_DZ = 1.3/2.0                                                  395
         AG_TITLE = 'BOX'                                                 395
         AG_EXNAME = 'SHAPE'                                              395
         AG_PARLIST = 'DX_DY_DZ'                                          395
         CALL AXSHAPE                                                     395
C    *                                                                    395
C    Create PHHA :                                                        396
C    Call To Local Procedure PHHA                                         396
         ASSIGN 6400 TO BLOCK_PHHA                                        396
         GO TO  6390                                                      396
6400     CONTINUE                                                         396
C    *                                                                    396
C    Geant POSITION : PHHA ===> MOTHER='PSHL' X=0 Y=(19.75-4.86)/2.0 Z=0  397
C     <===                                                                397
         CALL AGSRESET                                                    397
         AG_MOTHER = 'PSHL'                                               397
         AG_X = 0                                                         397
         AG_Y = (19.75-4.86)/2.0                                          397
         AG_Z = 0                                                         397
         AG_TITLE = 'PHHA'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Geant POSITION : PHHA ===> MOTHER='PSHL' X=0 Y=-(19.75-4.86)/2.0 Z=  397
C    0 <===                                                               397
         CALL AGSRESET                                                    398
         AG_MOTHER = 'PSHL'                                               398
         AG_X = 0                                                         398
         AG_Y = -(19.75-4.86)/2.0                                         398
         AG_Z = 0                                                         398
         AG_TITLE = 'PHHA'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create PHHB :                                                        399
C    Call To Local Procedure PHHB                                         399
         ASSIGN 6420 TO BLOCK_PHHB                                        399
         GO TO  6410                                                      399
6420     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : PHHB ===> MOTHER='PSHL' X=-(1.8-4.5)/2 Y=0 Z=0 <==  400
C    =                                                                    400
         CALL AGSRESET                                                    400
         AG_MOTHER = 'PSHL'                                               400
         AG_X = -(1.8-4.5)/2                                              400
         AG_Y = 0                                                         400
         AG_Z = 0                                                         400
         AG_TITLE = 'PHHB'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Create PHHC :                                                        401
C    Call To Local Procedure PHHC                                         401
         ASSIGN 6440 TO BLOCK_PHHC                                        401
         GO TO  6430                                                      401
6440     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : PHHC ===> MOTHER='PSHL' X=-7.8 Y=0 Z=0 KONLY='MANY  402
C    *' <===                                                              402
         CALL AGSRESET                                                    402
         AG_MOTHER = 'PSHL'                                               402
         AG_X = -7.8                                                      402
         AG_Y = 0                                                         402
         AG_Z = 0                                                         402
         AG_KONLY = 'MANY'                                                402
         AG_TITLE = 'PHHC'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                402
         CALL AXPOSITION                                                  402
C    *                                                                    402
5191  CALL AGSPOPB                                                        403
      GO TO BLOCK_PSHL, (5190 ,5200)                                      403
C End of Local Procedure                                                  403
C ----------------------------------------------------------------------  404
C ------------ PHHA                                                       404
C Begin Local Procedure PHHA                                              405
C PHHA PART OF HINGE LEAVE                                                405
6390  CONTINUE                                                            405
         AG_TITLE='PHHA'                                                  405
         AG_MODULE=MODULE                                                 405
         AG_MARK='V00'                                                    405
         CALL AGSPUSH                                                     405
         AG_MODULE=MODULE                                                 405
C    *                                                                    405
C    Geant ATTRIBUTE : PHHA ===> SEEN=1 COLO=5 <===                       406
         CALL AGSRESET                                                    406
         AG_SEEN = 1                                                      406
         AG_COLO = 5                                                      406
         AG_TITLE = 'PHHA'                                                406
         AG_EXNAME = 'ATTRIBUTE'                                          406
         AG_PARLIST = 'SEEN_COLO'                                         406
         CALL AXATTRIBUTE                                                 406
C    *                                                                    406
C    Geant MATERIAL : AL20 ===> inherited arguments used <===             406
         CALL AGSRESET                                                    407
         AG_TITLE = 'AL20'                                                407
         AG_EXNAME = 'MATERIAL'                                           407
         AG_PARLIST = 'NONE'                                              407
         CALL AXMATERIAL                                                  407
C    *                                                                    407
C    Geant SHAPE : BOX ===> DX=4.5 DY=4.86/2 DZ=1.3/2 <===                407
         CALL AGSRESET                                                    408
         AG_DX = 4.5                                                      408
         AG_DY = 4.86/2                                                   408
         AG_DZ = 1.3/2                                                    408
         AG_TITLE = 'BOX'                                                 408
         AG_EXNAME = 'SHAPE'                                              408
         AG_PARLIST = 'DX_DY_DZ'                                          408
         CALL AXSHAPE                                                     408
C    *                                                                    408
6391  CALL AGSPOPB                                                        409
      GO TO BLOCK_PHHA, (6390 ,6400)                                      409
C End of Local Procedure                                                  409
C ----------------------------------------------------------------------  410
C ------------ PHHB                                                       410
C Begin Local Procedure PHHB                                              411
C PHHB PART OF HINGE LEAVE                                                411
6410  CONTINUE                                                            411
         AG_TITLE='PHHB'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
C    Geant ATTRIBUTE : PHHB ===> SEEN=1 COLO=5 <===                       412
         CALL AGSRESET                                                    412
         AG_SEEN = 1                                                      412
         AG_COLO = 5                                                      412
         AG_TITLE = 'PHHB'                                                412
         AG_EXNAME = 'ATTRIBUTE'                                          412
         AG_PARLIST = 'SEEN_COLO'                                         412
         CALL AXATTRIBUTE                                                 412
C    *                                                                    412
C    Geant MATERIAL : AL20 ===> inherited arguments used <===             412
         CALL AGSRESET                                                    413
         AG_TITLE = 'AL20'                                                413
         AG_EXNAME = 'MATERIAL'                                           413
         AG_PARLIST = 'NONE'                                              413
         CALL AXMATERIAL                                                  413
C    *                                                                    413
C    Geant SHAPE : BOX ===> DX=6.3/2.0 DY=(19.75-2*4.86)/2, DZ=1.3/2 <==  413
C    =                                                                    413
         CALL AGSRESET                                                    415
         AG_DX = 6.3/2.0                                                  415
         AG_DY = (19.75-2*4.86)/2                                         415
         AG_DZ = 1.3/2                                                    415
         AG_TITLE = 'BOX'                                                 415
         AG_EXNAME = 'SHAPE'                                              415
         AG_PARLIST = 'DX_DY_DZ'                                          415
         CALL AXSHAPE                                                     415
C    *                                                                    415
6411  CALL AGSPOPB                                                        416
      GO TO BLOCK_PHHB, (6410 ,6420)                                      416
C End of Local Procedure                                                  416
C ----------------------------------------------------------------------  417
C ------------ PHHC                                                       417
C Begin Local Procedure PHHC                                              418
C PHHC OUTER HINGE LEAF                                                   418
6430  CONTINUE                                                            418
         AG_TITLE='PHHC'                                                  418
         AG_MODULE=MODULE                                                 418
         AG_MARK='V00'                                                    418
         CALL AGSPUSH                                                     418
         AG_MODULE=MODULE                                                 418
C    *                                                                    418
C    Geant ATTRIBUTE : PHHC ===> SEEN=1 COLO=5 <===                       419
         CALL AGSRESET                                                    419
         AG_SEEN = 1                                                      419
         AG_COLO = 5                                                      419
         AG_TITLE = 'PHHC'                                                419
         AG_EXNAME = 'ATTRIBUTE'                                          419
         AG_PARLIST = 'SEEN_COLO'                                         419
         CALL AXATTRIBUTE                                                 419
C    *                                                                    419
C    Geant MATERIAL : AL20 ===> inherited arguments used <===             419
         CALL AGSRESET                                                    420
         AG_TITLE = 'AL20'                                                420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant SHAPE : TUBS ===> RMIN=6.0 RMAX=6.0+1.6 PHI1=-55.67, PHI2=+55  420
C    .67 DZ=1.3/2.0 <===                                                  420
         CALL AGSRESET                                                    422
         AG_RMIN = 6.0                                                    422
         AG_RMAX = 6.0+1.6                                                422
         AG_PHI1 = -55.67                                                 422
         AG_PHI2 = +55.67                                                 422
         AG_DZ = 1.3/2.0                                                  422
         AG_TITLE = 'TUBS'                                                422
         AG_EXNAME = 'SHAPE'                                              422
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            422
         CALL AXSHAPE                                                     422
C    *                                                                    422
6431  CALL AGSPOPB                                                        423
      GO TO BLOCK_PHHC, (6430 ,6440)                                      423
C End of Local Procedure                                                  423
C ----------------------------------------------------------------------  424
C ------------ PSHU                                                       424
C Begin Local Procedure PSHU                                              425
C PSHU OUTER HINGE LEAF                                                   425
5210  CONTINUE                                                            425
         AG_TITLE='PSHU'                                                  425
         AG_MODULE=MODULE                                                 425
         AG_MARK='V00'                                                    425
         CALL AGSPUSH                                                     425
         AG_MODULE=MODULE                                                 425
C    *                                                                    425
C    Geant ATTRIBUTE : PSHU ===> SEEN=1 COLO=5 <===                       426
         CALL AGSRESET                                                    426
         AG_SEEN = 1                                                      426
         AG_COLO = 5                                                      426
         AG_TITLE = 'PSHU'                                                426
         AG_EXNAME = 'ATTRIBUTE'                                          426
         AG_PARLIST = 'SEEN_COLO'                                         426
         CALL AXATTRIBUTE                                                 426
C    *                                                                    426
C    Geant MATERIAL : AL20 ===> inherited arguments used <===             426
         CALL AGSRESET                                                    427
         AG_TITLE = 'AL20'                                                427
         AG_EXNAME = 'MATERIAL'                                           427
         AG_PARLIST = 'NONE'                                              427
         CALL AXMATERIAL                                                  427
C    *                                                                    427
C    Geant SHAPE : TUBS ===> RMIN=19.53 RMAX=21.03 PHI1=158, PHI2=202 DZ  427
C    =10.12/2.0 <===                                                      427
         CALL AGSRESET                                                    429
         AG_RMIN = 19.53                                                  429
         AG_RMAX = 21.03                                                  429
         AG_PHI1 = 158                                                    429
         AG_PHI2 = 202                                                    429
         AG_DZ = 10.12/2.0                                                429
         AG_TITLE = 'TUBS'                                                429
         AG_EXNAME = 'SHAPE'                                              429
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            429
         CALL AXSHAPE                                                     429
C    *                                                                    429
5211  CALL AGSPOPB                                                        430
      GO TO BLOCK_PSHU, (5210 ,5220)                                      430
C End of Local Procedure                                                  430
C ----------------------------------------------------------------------  431
C ------------ PSHM                                                       431
C Begin Local Procedure PSHM                                              432
C PSHM EXTENSION TUBE ATTACH RING                                         432
5230  CONTINUE                                                            432
         AG_TITLE='PSHM'                                                  432
         AG_MODULE=MODULE                                                 432
         AG_MARK='V00'                                                    432
         CALL AGSPUSH                                                     432
         AG_MODULE=MODULE                                                 432
C    *                                                                    432
C    Geant ATTRIBUTE : PSHM ===> SEEN=1 COLO=4 <===                       433
         CALL AGSRESET                                                    433
         AG_SEEN = 1                                                      433
         AG_COLO = 4                                                      433
         AG_TITLE = 'PSHM'                                                433
         AG_EXNAME = 'ATTRIBUTE'                                          433
         AG_PARLIST = 'SEEN_COLO'                                         433
         CALL AXATTRIBUTE                                                 433
C    *                                                                    433
C    Geant MATERIAL : AL40 ===> inherited arguments used <===             433
         CALL AGSRESET                                                    434
         AG_TITLE = 'AL40'                                                434
         AG_EXNAME = 'MATERIAL'                                           434
         AG_PARLIST = 'NONE'                                              434
         CALL AXMATERIAL                                                  434
C    *                                                                    434
C    Geant SHAPE : TUBS ===> RMIN=15.2/2 RMAX=17.8/2.0 PHI1=106, PHI2=25  434
C    4 DZ=DEXTSUPLEN/2+0.3 <===                                           434
         CALL AGSRESET                                                    436
         AG_RMIN = 15.2/2                                                 436
         AG_RMAX = 17.8/2.0                                               436
         AG_PHI1 = 106                                                    436
         AG_PHI2 = 254                                                    436
         AG_DZ = DEXTSUPLEN/2+0.3                                         436
         AG_TITLE = 'TUBS'                                                436
         AG_EXNAME = 'SHAPE'                                              436
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            436
         CALL AXSHAPE                                                     436
C    *                                                                    436
5231  CALL AGSPOPB                                                        437
      GO TO BLOCK_PSHM, (5230 ,5240)                                      437
C End of Local Procedure                                                  437
C ----------------------------------------------------------------------  438
C ------------ PSTO                                                       438
C Begin Local Procedure PSTO                                              439
C PSTO DTUBE EXT PLANE TOP                                                439
5330  CONTINUE                                                            439
         AG_TITLE='PSTO'                                                  439
         AG_MODULE=MODULE                                                 439
         AG_MARK='V00'                                                    439
         CALL AGSPUSH                                                     439
         AG_MODULE=MODULE                                                 439
C    *                                                                    439
C    Geant ATTRIBUTE : PSTO ===> SEEN=1 COLO=4 <===                       440
         CALL AGSRESET                                                    440
         AG_SEEN = 1                                                      440
         AG_COLO = 4                                                      440
         AG_TITLE = 'PSTO'                                                440
         AG_EXNAME = 'ATTRIBUTE'                                          440
         AG_PARLIST = 'SEEN_COLO'                                         440
         CALL AXATTRIBUTE                                                 440
C    *                                                                    440
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        440
         CALL AGSRESET                                                    441
         AG_TITLE = 'ALUMINIUM'                                           441
         AG_EXNAME = 'MATERIAL'                                           441
         AG_PARLIST = 'NONE'                                              441
         CALL AXMATERIAL                                                  441
C    *                                                                    441
C    Geant SHAPE : BOX ===> DX=DEXTTHK/2.0 DY=3.04/2.0 DZ=DEXTLEN/2.0 <=  441
C    ==                                                                   441
         CALL AGSRESET                                                    442
         AG_DX = DEXTTHK/2.0                                              442
         AG_DY = 3.04/2.0                                                 442
         AG_DZ = DEXTLEN/2.0                                              442
         AG_TITLE = 'BOX'                                                 442
         AG_EXNAME = 'SHAPE'                                              442
         AG_PARLIST = 'DX_DY_DZ'                                          442
         CALL AXSHAPE                                                     442
C    *                                                                    442
5331  CALL AGSPOPB                                                        443
      GO TO BLOCK_PSTO, (5330 ,5340)                                      443
C End of Local Procedure                                                  443
C ----------------------------------------------------------------------  444
C ------------ PSOC                                                       444
C Begin Local Procedure PSOC                                              445
C PSOC A OUTSIDE CYLINDER                                                 445
5350  CONTINUE                                                            445
         AG_TITLE='PSOC'                                                  445
         AG_MODULE=MODULE                                                 445
         AG_MARK='V00'                                                    445
         CALL AGSPUSH                                                     445
         AG_MODULE=MODULE                                                 445
C    *                                                                    445
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        446
         CALL AGSRESET                                                    446
         AG_TITLE = 'ALUMINIUM'                                           446
         AG_EXNAME = 'MATERIAL'                                           446
         AG_PARLIST = 'NONE'                                              446
         CALL AXMATERIAL                                                  446
C    *                                                                    446
C    Geant ATTRIBUTE : PSOC ===> SEEN=1 COLO=4 <===                       446
         CALL AGSRESET                                                    447
         AG_SEEN = 1                                                      447
         AG_COLO = 4                                                      447
         AG_TITLE = 'PSOC'                                                447
         AG_EXNAME = 'ATTRIBUTE'                                          447
         AG_PARLIST = 'SEEN_COLO'                                         447
         CALL AXATTRIBUTE                                                 447
C    *                                                                    447
C    Geant SHAPE : TUBS ===> RMIN=DEXTOUTRAD RMAX=DEXTOUTRAD+DEXTTHK, PH  447
C    I1=180-76.4 PHI2=180+76.4 DZ=DEXTLEN/2 <===                          447
         CALL AGSRESET                                                    449
         AG_RMIN = DEXTOUTRAD                                             449
         AG_RMAX = DEXTOUTRAD+DEXTTHK                                     449
         AG_PHI1 = 180-76.4                                               449
         AG_PHI2 = 180+76.4                                               449
         AG_DZ = DEXTLEN/2                                                449
         AG_TITLE = 'TUBS'                                                449
         AG_EXNAME = 'SHAPE'                                              449
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            449
         CALL AXSHAPE                                                     449
C    *                                                                    449
5351  CALL AGSPOPB                                                        450
      GO TO BLOCK_PSOC, (5350 ,5360)                                      450
C End of Local Procedure                                                  450
C ----------------------------------------------------------------------  451
C ------------ PSIC                                                       451
C Begin Local Procedure PSIC                                              452
C PSIC A OUTSIDE CYLINDER                                                 452
5370  CONTINUE                                                            452
         AG_TITLE='PSIC'                                                  452
         AG_MODULE=MODULE                                                 452
         AG_MARK='V00'                                                    452
         CALL AGSPUSH                                                     452
         AG_MODULE=MODULE                                                 452
C    *                                                                    452
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        453
         CALL AGSRESET                                                    453
         AG_TITLE = 'ALUMINIUM'                                           453
         AG_EXNAME = 'MATERIAL'                                           453
         AG_PARLIST = 'NONE'                                              453
         CALL AXMATERIAL                                                  453
C    *                                                                    453
C    Geant ATTRIBUTE : PSIC ===> SEEN=1 COLO=4 <===                       453
         CALL AGSRESET                                                    454
         AG_SEEN = 1                                                      454
         AG_COLO = 4                                                      454
         AG_TITLE = 'PSIC'                                                454
         AG_EXNAME = 'ATTRIBUTE'                                          454
         AG_PARLIST = 'SEEN_COLO'                                         454
         CALL AXATTRIBUTE                                                 454
C    *                                                                    454
C    Geant SHAPE : TUBS ===> RMIN=DEXTINRAD RMAX=DEXTINRAD+DEXTTHK, PHI1  454
C    =180-65.5 PHI2=180+65.5 DZ=DEXTLEN/2 <===                            454
         CALL AGSRESET                                                    456
         AG_RMIN = DEXTINRAD                                              456
         AG_RMAX = DEXTINRAD+DEXTTHK                                      456
         AG_PHI1 = 180-65.5                                               456
         AG_PHI2 = 180+65.5                                               456
         AG_DZ = DEXTLEN/2                                                456
         AG_TITLE = 'TUBS'                                                456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            456
         CALL AXSHAPE                                                     456
C    *                                                                    456
5371  CALL AGSPOPB                                                        457
      GO TO BLOCK_PSIC, (5370 ,5380)                                      457
C End of Local Procedure                                                  457
C ----------------------------------------------------------------------  458
C ------------ PSUC                                                       458
C Begin Local Procedure PSUC                                              459
C PSUC A UPPER CORNER                                                     459
5390  CONTINUE                                                            459
         AG_TITLE='PSUC'                                                  459
         AG_MODULE=MODULE                                                 459
         AG_MARK='V00'                                                    459
         CALL AGSPUSH                                                     459
         AG_MODULE=MODULE                                                 459
C    *                                                                    459
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        460
         CALL AGSRESET                                                    460
         AG_TITLE = 'ALUMINIUM'                                           460
         AG_EXNAME = 'MATERIAL'                                           460
         AG_PARLIST = 'NONE'                                              460
         CALL AXMATERIAL                                                  460
C    *                                                                    460
C    Geant ATTRIBUTE : PSUC ===> SEEN=1 COLO=4 <===                       460
         CALL AGSRESET                                                    461
         AG_SEEN = 1                                                      461
         AG_COLO = 4                                                      461
         AG_TITLE = 'PSUC'                                                461
         AG_EXNAME = 'ATTRIBUTE'                                          461
         AG_PARLIST = 'SEEN_COLO'                                         461
         CALL AXATTRIBUTE                                                 461
C    *                                                                    461
C    Geant SHAPE : TUBS ===> RMIN=0.7 RMAX=0.7+DEXTTHK PHI1=0, PHI2=103.  461
C    6 DZ=DEXTLEN/2 <===                                                  461
         CALL AGSRESET                                                    463
         AG_RMIN = 0.7                                                    463
         AG_RMAX = 0.7+DEXTTHK                                            463
         AG_PHI1 = 0                                                      463
         AG_PHI2 = 103.6                                                  463
         AG_DZ = DEXTLEN/2                                                463
         AG_TITLE = 'TUBS'                                                463
         AG_EXNAME = 'SHAPE'                                              463
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            463
         CALL AXSHAPE                                                     463
C    *                                                                    463
5391  CALL AGSPOPB                                                        464
      GO TO BLOCK_PSUC, (5390 ,5400)                                      464
C End of Local Procedure                                                  464
C ----------------------------------------------------------------------  465
C ------------ PSBC                                                       465
C Begin Local Procedure PSBC                                              466
C PSBC A UPPER CORNER                                                     466
5410  CONTINUE                                                            466
         AG_TITLE='PSBC'                                                  466
         AG_MODULE=MODULE                                                 466
         AG_MARK='V00'                                                    466
         CALL AGSPUSH                                                     466
         AG_MODULE=MODULE                                                 466
C    *                                                                    466
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        467
         CALL AGSRESET                                                    467
         AG_TITLE = 'ALUMINIUM'                                           467
         AG_EXNAME = 'MATERIAL'                                           467
         AG_PARLIST = 'NONE'                                              467
         CALL AXMATERIAL                                                  467
C    *                                                                    467
C    Geant ATTRIBUTE : PSBC ===> SEEN=1 COLO=4 <===                       467
         CALL AGSRESET                                                    468
         AG_SEEN = 1                                                      468
         AG_COLO = 4                                                      468
         AG_TITLE = 'PSBC'                                                468
         AG_EXNAME = 'ATTRIBUTE'                                          468
         AG_PARLIST = 'SEEN_COLO'                                         468
         CALL AXATTRIBUTE                                                 468
C    *                                                                    468
C    Geant SHAPE : TUBS ===> RMIN=0.7 RMAX=0.7+DEXTTHK PHI1=360-65.8, PH  468
C    I2=360 DZ=DEXTLEN/2 <===                                             468
         CALL AGSRESET                                                    470
         AG_RMIN = 0.7                                                    470
         AG_RMAX = 0.7+DEXTTHK                                            470
         AG_PHI1 = 360-65.8                                               470
         AG_PHI2 = 360                                                    470
         AG_DZ = DEXTLEN/2                                                470
         AG_TITLE = 'TUBS'                                                470
         AG_EXNAME = 'SHAPE'                                              470
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            470
         CALL AXSHAPE                                                     470
C    *                                                                    470
5411  CALL AGSPOPB                                                        471
      GO TO BLOCK_PSBC, (5410 ,5420)                                      471
C End of Local Procedure                                                  471
C ----------------------------------------------------------------------  472
C ------------ PSHN                                                       472
C Begin Local Procedure PSHN                                              473
C PSHN CARRIAGE BASE EXT                                                  473
5250  CONTINUE                                                            473
         AG_TITLE='PSHN'                                                  473
         AG_MODULE=MODULE                                                 473
         AG_MARK='V00'                                                    473
         CALL AGSPUSH                                                     473
         AG_MODULE=MODULE                                                 473
C    *                                                                    473
C    Geant ATTRIBUTE : PSHN ===> SEEN=1 COLO=4 <===                       474
         CALL AGSRESET                                                    474
         AG_SEEN = 1                                                      474
         AG_COLO = 4                                                      474
         AG_TITLE = 'PSHN'                                                474
         AG_EXNAME = 'ATTRIBUTE'                                          474
         AG_PARLIST = 'SEEN_COLO'                                         474
         CALL AXATTRIBUTE                                                 474
C    *                                                                    474
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        474
         CALL AGSRESET                                                    475
         AG_TITLE = 'ALUMINIUM'                                           475
         AG_EXNAME = 'MATERIAL'                                           475
         AG_PARLIST = 'NONE'                                              475
         CALL AXMATERIAL                                                  475
C    *                                                                    475
C    Geant SHAPE : TUBS ===> RMIN=8.7 RMAX=8.9 PHI1=106 PHI2=254, DZ=0.3  475
C    5 <===                                                               475
         CALL AGSRESET                                                    477
         AG_RMIN = 8.7                                                    477
         AG_RMAX = 8.9                                                    477
         AG_PHI1 = 106                                                    477
         AG_PHI2 = 254                                                    477
         AG_DZ = 0.35                                                     477
         AG_TITLE = 'TUBS'                                                477
         AG_EXNAME = 'SHAPE'                                              477
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            477
         CALL AXSHAPE                                                     477
C    *                                                                    477
5251  CALL AGSPOPB                                                        478
      GO TO BLOCK_PSHN, (5250 ,5260)                                      478
C End of Local Procedure                                                  478
C ----------------------------------------------------------------------  479
C ------------ PSHO                                                       479
C Begin Local Procedure PSHO                                              480
C PSHO CARRIAGE BASE EXT CENTER PART                                      480
5270  CONTINUE                                                            480
         AG_TITLE='PSHO'                                                  480
         AG_MODULE=MODULE                                                 480
         AG_MARK='V00'                                                    480
         CALL AGSPUSH                                                     480
         AG_MODULE=MODULE                                                 480
C    *                                                                    480
C    Geant ATTRIBUTE : PSHO ===> SEEN=1 COLO=4 <===                       481
         CALL AGSRESET                                                    481
         AG_SEEN = 1                                                      481
         AG_COLO = 4                                                      481
         AG_TITLE = 'PSHO'                                                481
         AG_EXNAME = 'ATTRIBUTE'                                          481
         AG_PARLIST = 'SEEN_COLO'                                         481
         CALL AXATTRIBUTE                                                 481
C    *                                                                    481
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        481
         CALL AGSRESET                                                    482
         AG_TITLE = 'ALUMINIUM'                                           482
         AG_EXNAME = 'MATERIAL'                                           482
         AG_PARLIST = 'NONE'                                              482
         CALL AXMATERIAL                                                  482
C    *                                                                    482
C    Geant SHAPE : CONS ===> DZ=5.90/2 RMN1=20.4 RMX1=20.8, RMN2=8.7+5.0  482
C     RMX2=8.9+5.0 PHI1=160 PHI2=200 <===                                 482
         CALL AGSRESET                                                    484
         AG_DZ = 5.90/2                                                   484
         AG_RMN1 = 20.4                                                   484
         AG_RMX1 = 20.8                                                   484
         AG_RMN2 = 8.7+5.0                                                484
         AG_RMX2 = 8.9+5.0                                                484
         AG_PHI1 = 160                                                    484
         AG_PHI2 = 200                                                    484
         AG_TITLE = 'CONS'                                                484
         AG_EXNAME = 'SHAPE'                                              484
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  484
         CALL AXSHAPE                                                     484
C    *                                                                    484
5271  CALL AGSPOPB                                                        485
      GO TO BLOCK_PSHO, (5270 ,5280)                                      485
C End of Local Procedure                                                  485
C ----------------------------------------------------------------------  486
C ------------ PSHP                                                       486
C Begin Local Procedure PSHP                                              487
C PSHP CARRIAGE BASE EXT                                                  487
5290  CONTINUE                                                            487
         AG_TITLE='PSHP'                                                  487
         AG_MODULE=MODULE                                                 487
         AG_MARK='V00'                                                    487
         CALL AGSPUSH                                                     487
         AG_MODULE=MODULE                                                 487
C    *                                                                    487
C    Geant ATTRIBUTE : PSHP ===> SEEN=1 COLO=4 <===                       488
         CALL AGSRESET                                                    488
         AG_SEEN = 1                                                      488
         AG_COLO = 4                                                      488
         AG_TITLE = 'PSHP'                                                488
         AG_EXNAME = 'ATTRIBUTE'                                          488
         AG_PARLIST = 'SEEN_COLO'                                         488
         CALL AXATTRIBUTE                                                 488
C    *                                                                    488
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        488
         CALL AGSRESET                                                    489
         AG_TITLE = 'ALUMINIUM'                                           489
         AG_EXNAME = 'MATERIAL'                                           489
         AG_PARLIST = 'NONE'                                              489
         CALL AXMATERIAL                                                  489
C    *                                                                    489
C    Geant SHAPE : PCON ===> PHI1=106 DPHI=150-106 NZ=2 ZI={-61.6-2.95,   489
C    -61.6+2.95} RMN={12.7,8.7} RMX={12.9, 8.9} <===                      489
         CALL AGSRESET                                                    492
         AG_PHI1 = 106                                                    492
         AG_DPHI = 150-106                                                492
         AG_NZ = 2                                                        492
         AG_ZI(1)=-61.6-2.95                                              492
         AG_ZI(2)=-61.6+2.95                                              492
         AG_RMN(1)=12.7                                                   492
         AG_RMN(2)=8.7                                                    492
         AG_RMX(1)=12.9                                                   492
         AG_RMX(2)=8.9                                                    492
         AG_TITLE = 'PCON'                                                492
         AG_EXNAME = 'SHAPE'                                              492
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           492
         CALL AXSHAPE                                                     492
C    *                                                                    492
5291  CALL AGSPOPB                                                        493
      GO TO BLOCK_PSHP, (5290 ,5300)                                      493
C End of Local Procedure                                                  493
C ----------------------------------------------------------------------  494
C ------------ PSHQ                                                       494
C Begin Local Procedure PSHQ                                              495
C PSHQ CARRIAGE BASE EXT                                                  495
5310  CONTINUE                                                            495
         AG_TITLE='PSHQ'                                                  495
         AG_MODULE=MODULE                                                 495
         AG_MARK='V00'                                                    495
         CALL AGSPUSH                                                     495
         AG_MODULE=MODULE                                                 495
C    *                                                                    495
C    Geant ATTRIBUTE : PSHQ ===> SEEN=1 COLO=4 <===                       496
         CALL AGSRESET                                                    496
         AG_SEEN = 1                                                      496
         AG_COLO = 4                                                      496
         AG_TITLE = 'PSHQ'                                                496
         AG_EXNAME = 'ATTRIBUTE'                                          496
         AG_PARLIST = 'SEEN_COLO'                                         496
         CALL AXATTRIBUTE                                                 496
C    *                                                                    496
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        496
         CALL AGSRESET                                                    497
         AG_TITLE = 'ALUMINIUM'                                           497
         AG_EXNAME = 'MATERIAL'                                           497
         AG_PARLIST = 'NONE'                                              497
         CALL AXMATERIAL                                                  497
C    *                                                                    497
C    Geant SHAPE : PCON ===> PHI1=200 DPHI=160-106 NZ=2 ZI={-61.6-2.95,   497
C    -61.6+2.95} RMN={12.7,8.7} RMX={12.9, 8.9} <===                      497
         CALL AGSRESET                                                    500
         AG_PHI1 = 200                                                    500
         AG_DPHI = 160-106                                                500
         AG_NZ = 2                                                        500
         AG_ZI(1)=-61.6-2.95                                              500
         AG_ZI(2)=-61.6+2.95                                              500
         AG_RMN(1)=12.7                                                   500
         AG_RMN(2)=8.7                                                    500
         AG_RMX(1)=12.9                                                   500
         AG_RMX(2)=8.9                                                    500
         AG_TITLE = 'PCON'                                                500
         AG_EXNAME = 'SHAPE'                                              500
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           500
         CALL AXSHAPE                                                     500
C    *                                                                    500
5311  CALL AGSPOPB                                                        501
      GO TO BLOCK_PSHQ, (5310 ,5320)                                      501
C End of Local Procedure                                                  501
C ----------------------------------------------------------------------  502
C ------------ PSAL                                                       502
C Begin Local Procedure PSAL                                              503
C PSAL AIR AL HOSE                                                        503
5470  CONTINUE                                                            503
         AG_TITLE='PSAL'                                                  503
         AG_MODULE=MODULE                                                 503
         AG_MARK='V00'                                                    503
         CALL AGSPUSH                                                     503
         AG_MODULE=MODULE                                                 503
C    *                                                                    503
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        504
         CALL AGSRESET                                                    504
         AG_TITLE = 'ALUMINIUM'                                           504
         AG_EXNAME = 'MATERIAL'                                           504
         AG_PARLIST = 'NONE'                                              504
         CALL AXMATERIAL                                                  504
C    *                                                                    504
C    Geant ATTRIBUTE : PSAL ===> SEEN=1 COLO=1 <===                       504
         CALL AGSRESET                                                    505
         AG_SEEN = 1                                                      505
         AG_COLO = 1                                                      505
         AG_TITLE = 'PSAL'                                                505
         AG_EXNAME = 'ATTRIBUTE'                                          505
         AG_PARLIST = 'SEEN_COLO'                                         505
         CALL AXATTRIBUTE                                                 505
C    *                                                                    505
C    Geant SHAPE : TUBE ===> RMIN=3.7 RMAX=3.8 DZ=172.2/2 <===            505
         CALL AGSRESET                                                    506
         AG_RMIN = 3.7                                                    506
         AG_RMAX = 3.8                                                    506
         AG_DZ = 172.2/2                                                  506
         AG_TITLE = 'TUBE'                                                506
         AG_EXNAME = 'SHAPE'                                              506
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      506
         CALL AXSHAPE                                                     506
C    *                                                                    506
5471  CALL AGSPOPB                                                        507
      GO TO BLOCK_PSAL, (5470 ,5480)                                      507
C End of Local Procedure                                                  507
C ----------------------------------------------------------------------  508
C ------------ PSAK                                                       508
C Begin Local Procedure PSAK                                              509
C PSAK AIR DUCT 1/2                                                       509
5490  CONTINUE                                                            509
         AG_TITLE='PSAK'                                                  509
         AG_MODULE=MODULE                                                 509
         AG_MARK='V00'                                                    509
         CALL AGSPUSH                                                     509
         AG_MODULE=MODULE                                                 509
C    *                                                                    509
C    Geant COMPONENT : O ===> A=16 Z=8 W=2*16./100. <===                  510
         CALL AGSRESET                                                    510
         AG_A = 16                                                        510
         AG_Z = 8                                                         510
         AG_W = 2*16./100.                                                510
         AG_TITLE = 'O'                                                   510
         AG_EXNAME = 'COMPONENT'                                          510
         AG_PARLIST = 'A_Z_W'                                             510
         CALL AXCOMPONENT                                                 510
C    *                                                                    510
C    Geant COMPONENT : C ===> A=12 Z=6 W=5*12./100. <===                  510
         CALL AGSRESET                                                    511
         AG_A = 12                                                        511
         AG_Z = 6                                                         511
         AG_W = 5*12./100.                                                511
         AG_TITLE = 'C'                                                   511
         AG_EXNAME = 'COMPONENT'                                          511
         AG_PARLIST = 'A_Z_W'                                             511
         CALL AXCOMPONENT                                                 511
C    *                                                                    511
C    Geant COMPONENT : H ===> A=1 Z=1 W=8*1.0/100. <===                   511
         CALL AGSRESET                                                    512
         AG_A = 1                                                         512
         AG_Z = 1                                                         512
         AG_W = 8*1.0/100.                                                512
         AG_TITLE = 'H'                                                   512
         AG_EXNAME = 'COMPONENT'                                          512
         AG_PARLIST = 'A_Z_W'                                             512
         CALL AXCOMPONENT                                                 512
C    *                                                                    512
C    Geant MIXTURE : DELRIN ===> DENS=1.2 <===                            512
         CALL AGSRESET                                                    513
         AG_DENS = 1.2                                                    513
         AG_TITLE = 'DELRIN'                                              513
         AG_EXNAME = 'MIXTURE'                                            513
         AG_PARLIST = 'DENS'                                              513
         CALL AXMIXTURE                                                   513
C    *                                                                    513
C    Geant ATTRIBUTE : PSAK ===> SEEN=1 COLO=1 <===                       513
         CALL AGSRESET                                                    514
         AG_SEEN = 1                                                      514
         AG_COLO = 1                                                      514
         AG_TITLE = 'PSAK'                                                514
         AG_EXNAME = 'ATTRIBUTE'                                          514
         AG_PARLIST = 'SEEN_COLO'                                         514
         CALL AXATTRIBUTE                                                 514
C    *                                                                    514
C    Geant SHAPE : TUBE ===> RMIN=3.6 RMAX=3.8 DZ=11.0/2 <===             514
         CALL AGSRESET                                                    515
         AG_RMIN = 3.6                                                    515
         AG_RMAX = 3.8                                                    515
         AG_DZ = 11.0/2                                                   515
         AG_TITLE = 'TUBE'                                                515
         AG_EXNAME = 'SHAPE'                                              515
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      515
         CALL AXSHAPE                                                     515
C    *                                                                    515
5491  CALL AGSPOPB                                                        516
      GO TO BLOCK_PSAK, (5490 ,5500)                                      516
C End of Local Procedure                                                  516
C ----------------------------------------------------------------------  517
C ------------ PSCC                                                       517
C Begin Local Procedure PSCC                                              518
C PSCC CARRIAGE TRAY BRACKET (SOLID PART)                                 518
5430  CONTINUE                                                            518
         AG_TITLE='PSCC'                                                  518
         AG_MODULE=MODULE                                                 518
         AG_MARK='V00'                                                    518
         CALL AGSPUSH                                                     518
         AG_MODULE=MODULE                                                 518
C    *                                                                    518
C    Geant ATTRIBUTE : PSCC ===> SEEN=1 COLO=5 <===                       519
         CALL AGSRESET                                                    519
         AG_SEEN = 1                                                      519
         AG_COLO = 5                                                      519
         AG_TITLE = 'PSCC'                                                519
         AG_EXNAME = 'ATTRIBUTE'                                          519
         AG_PARLIST = 'SEEN_COLO'                                         519
         CALL AXATTRIBUTE                                                 519
C    *                                                                    519
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        519
         CALL AGSRESET                                                    520
         AG_TITLE = 'ALUMINIUM'                                           520
         AG_EXNAME = 'MATERIAL'                                           520
         AG_PARLIST = 'NONE'                                              520
         CALL AXMATERIAL                                                  520
C    *                                                                    520
C    Geant SHAPE : TUBS ===> RMIN=18.7 RMAX=21.2 PHI1=180-19, PHI2=180+1  520
C    9 DZ=4.0/2.0 <===                                                    520
         CALL AGSRESET                                                    522
         AG_RMIN = 18.7                                                   522
         AG_RMAX = 21.2                                                   522
         AG_PHI1 = 180-19                                                 522
         AG_PHI2 = 180+19                                                 522
         AG_DZ = 4.0/2.0                                                  522
         AG_TITLE = 'TUBS'                                                522
         AG_EXNAME = 'SHAPE'                                              522
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            522
         CALL AXSHAPE                                                     522
C    *                                                                    522
C    Create PSCI :                                                        523
C    Call To Local Procedure PSCI                                         523
         ASSIGN 6460 TO BLOCK_PSCI                                        523
         GO TO  6450                                                      523
6460     CONTINUE                                                         523
C    *                                                                    523
C    Geant POSITION : PSCI ===> MOTHER='PSCC' X=0 Y=0 Z=-0.1 <===         524
         CALL AGSRESET                                                    524
         AG_MOTHER = 'PSCC'                                               524
         AG_X = 0                                                         524
         AG_Y = 0                                                         524
         AG_Z = -0.1                                                      524
         AG_TITLE = 'PSCI'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      524
         CALL AXPOSITION                                                  524
C    *                                                                    524
5431  CALL AGSPOPB                                                        525
      GO TO BLOCK_PSCC, (5430 ,5440)                                      525
C End of Local Procedure                                                  525
C ----------------------------------------------------------------------  526
C ------------ PSCI                                                       526
C Begin Local Procedure PSCI                                              527
C PSCI CARRIAGE TRAY BRACKET (HOLLOW PART)                                527
6450  CONTINUE                                                            527
         AG_TITLE='PSCI'                                                  527
         AG_MODULE=MODULE                                                 527
         AG_MARK='V00'                                                    527
         CALL AGSPUSH                                                     527
         AG_MODULE=MODULE                                                 527
C    *                                                                    527
C    Geant ATTRIBUTE : PSCI ===> SEEN=1 COLO=2 <===                       528
         CALL AGSRESET                                                    528
         AG_SEEN = 1                                                      528
         AG_COLO = 2                                                      528
         AG_TITLE = 'PSCI'                                                528
         AG_EXNAME = 'ATTRIBUTE'                                          528
         AG_PARLIST = 'SEEN_COLO'                                         528
         CALL AXATTRIBUTE                                                 528
C    *                                                                    528
C    Geant MATERIAL : AIR ===> inherited arguments used <===              528
         CALL AGSRESET                                                    529
         AG_TITLE = 'AIR'                                                 529
         AG_EXNAME = 'MATERIAL'                                           529
         AG_PARLIST = 'NONE'                                              529
         CALL AXMATERIAL                                                  529
C    *                                                                    529
C    Geant SHAPE : TUBS ===> RMIN=18.9 RMAX=21.0 PHI1=180-18, PHI2=180+1  529
C    8 DZ=(4.0-0.2)/2.0 <===                                              529
         CALL AGSRESET                                                    531
         AG_RMIN = 18.9                                                   531
         AG_RMAX = 21.0                                                   531
         AG_PHI1 = 180-18                                                 531
         AG_PHI2 = 180+18                                                 531
         AG_DZ = (4.0-0.2)/2.0                                            531
         AG_TITLE = 'TUBS'                                                531
         AG_EXNAME = 'SHAPE'                                              531
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            531
         CALL AXSHAPE                                                     531
C    *                                                                    531
6451  CALL AGSPOPB                                                        532
      GO TO BLOCK_PSCI, (6450 ,6460)                                      532
C End of Local Procedure                                                  532
C ----------------------------------------------------------------------  533
C ------------ PSHI                                                       533
C Begin Local Procedure PSHI                                              534
C PSHI CARRIAGE HINGE PART                                                534
5450  CONTINUE                                                            534
         AG_TITLE='PSHI'                                                  534
         AG_MODULE=MODULE                                                 534
         AG_MARK='V00'                                                    534
         CALL AGSPUSH                                                     534
         AG_MODULE=MODULE                                                 534
C    *                                                                    534
C    Geant ATTRIBUTE : PSHI ===> SEEN=1 COLO=4 <===                       535
         CALL AGSRESET                                                    535
         AG_SEEN = 1                                                      535
         AG_COLO = 4                                                      535
         AG_TITLE = 'PSHI'                                                535
         AG_EXNAME = 'ATTRIBUTE'                                          535
         AG_PARLIST = 'SEEN_COLO'                                         535
         CALL AXATTRIBUTE                                                 535
C    *                                                                    535
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        535
         CALL AGSRESET                                                    536
         AG_TITLE = 'ALUMINIUM'                                           536
         AG_EXNAME = 'MATERIAL'                                           536
         AG_PARLIST = 'NONE'                                              536
         CALL AXMATERIAL                                                  536
C    *                                                                    536
C    Geant SHAPE : BOX ===> DX=1.4/2. DY=1.0/2. DZ=5.4/2. <===            536
         CALL AGSRESET                                                    537
         AG_DX = 1.4/2.                                                   537
         AG_DY = 1.0/2.                                                   537
         AG_DZ = 5.4/2.                                                   537
         AG_TITLE = 'BOX'                                                 537
         AG_EXNAME = 'SHAPE'                                              537
         AG_PARLIST = 'DX_DY_DZ'                                          537
         CALL AXSHAPE                                                     537
C    *                                                                    537
5451  CALL AGSPOPB                                                        538
      GO TO BLOCK_PSHI, (5450 ,5460)                                      538
C End of Local Procedure                                                  538
C ----------------------------------------------------------------------  539
C ------------ PSCL                                                       539
C Begin Local Procedure PSCL                                              540
C PSCL CABLE SUPPORT LADDER                                               540
5510  CONTINUE                                                            540
         AG_TITLE='PSCL'                                                  540
         AG_MODULE=MODULE                                                 540
         AG_MARK='V00'                                                    540
         CALL AGSPUSH                                                     540
         AG_MODULE=MODULE                                                 540
C    *                                                                    540
C    Geant ATTRIBUTE : PSCL ===> SEEN=1 COLO=1 <===                       541
         CALL AGSRESET                                                    541
         AG_SEEN = 1                                                      541
         AG_COLO = 1                                                      541
         AG_TITLE = 'PSCL'                                                541
         AG_EXNAME = 'ATTRIBUTE'                                          541
         AG_PARLIST = 'SEEN_COLO'                                         541
         CALL AXATTRIBUTE                                                 541
C    *                                                                    541
C    Geant MATERIAL : AL30 ===> A=26.8 Z=13 DENS=2.7*0.3 ABSL=37.2/0.3 R  541
C    ADL=8.9/0.3 <===                                                     541
         CALL AGSRESET                                                    542
         AG_A = 26.8                                                      542
         AG_Z = 13                                                        542
         AG_DENS = 2.7*0.3                                                542
         AG_ABSL = 37.2/0.3                                               542
         AG_RADL = 8.9/0.3                                                542
         AG_TITLE = 'AL30'                                                542
         AG_EXNAME = 'MATERIAL'                                           542
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                542
         CALL AXMATERIAL                                                  542
C    *                                                                    542
C    Geant SHAPE : BOX ===> DX=0.1 DY=12.9/2.0 DZ=134.7/2 <===            542
         CALL AGSRESET                                                    543
         AG_DX = 0.1                                                      543
         AG_DY = 12.9/2.0                                                 543
         AG_DZ = 134.7/2                                                  543
         AG_TITLE = 'BOX'                                                 543
         AG_EXNAME = 'SHAPE'                                              543
         AG_PARLIST = 'DX_DY_DZ'                                          543
         CALL AXSHAPE                                                     543
C    *                                                                    543
5511  CALL AGSPOPB                                                        544
      GO TO BLOCK_PSCL, (5510 ,5520)                                      544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ PSCK                                                       545
C Begin Local Procedure PSCK                                              546
C PSCK CABLE SUPPORT LADDER TOP AND BOTTOM                                546
5530  CONTINUE                                                            546
         AG_TITLE='PSCK'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant ATTRIBUTE : PSCK ===> SEEN=1 COLO=1 <===                       547
         CALL AGSRESET                                                    547
         AG_SEEN = 1                                                      547
         AG_COLO = 1                                                      547
         AG_TITLE = 'PSCK'                                                547
         AG_EXNAME = 'ATTRIBUTE'                                          547
         AG_PARLIST = 'SEEN_COLO'                                         547
         CALL AXATTRIBUTE                                                 547
C    *                                                                    547
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        547
         CALL AGSRESET                                                    548
         AG_TITLE = 'ALUMINIUM'                                           548
         AG_EXNAME = 'MATERIAL'                                           548
         AG_PARLIST = 'NONE'                                              548
         CALL AXMATERIAL                                                  548
C    *                                                                    548
C    Geant SHAPE : BOX ===> DX=2.7/2 DY=0.1 DZ=134.7/2 <===               548
         CALL AGSRESET                                                    549
         AG_DX = 2.7/2                                                    549
         AG_DY = 0.1                                                      549
         AG_DZ = 134.7/2                                                  549
         AG_TITLE = 'BOX'                                                 549
         AG_EXNAME = 'SHAPE'                                              549
         AG_PARLIST = 'DX_DY_DZ'                                          549
         CALL AXSHAPE                                                     549
C    *                                                                    549
5531  CALL AGSPOPB                                                        550
      GO TO BLOCK_PSCK, (5530 ,5540)                                      550
C End of Local Procedure                                                  550
C ----------------------------------------------------------------------  551
C ------------ PSAR                                                       551
C Begin Local Procedure PSAR                                              552
C PSAR BASE SLIDE CENTER, VERTICAL PART                                   552
5550  CONTINUE                                                            552
         AG_TITLE='PSAR'                                                  552
         AG_MODULE=MODULE                                                 552
         AG_MARK='V00'                                                    552
         CALL AGSPUSH                                                     552
         AG_MODULE=MODULE                                                 552
C    *                                                                    552
C    Geant ATTRIBUTE : PSAR ===> SEEN=1 COLO=4 <===                       553
         CALL AGSRESET                                                    553
         AG_SEEN = 1                                                      553
         AG_COLO = 4                                                      553
         AG_TITLE = 'PSAR'                                                553
         AG_EXNAME = 'ATTRIBUTE'                                          553
         AG_PARLIST = 'SEEN_COLO'                                         553
         CALL AXATTRIBUTE                                                 553
C    *                                                                    553
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        553
         CALL AGSRESET                                                    554
         AG_TITLE = 'ALUMINIUM'                                           554
         AG_EXNAME = 'MATERIAL'                                           554
         AG_PARLIST = 'NONE'                                              554
         CALL AXMATERIAL                                                  554
C    *                                                                    554
C    Geant SHAPE : BOX ===> DX=0.7/2 DY=13.6/2. DZ=16.5/2 <===            554
         CALL AGSRESET                                                    555
         AG_DX = 0.7/2                                                    555
         AG_DY = 13.6/2.                                                  555
         AG_DZ = 16.5/2                                                   555
         AG_TITLE = 'BOX'                                                 555
         AG_EXNAME = 'SHAPE'                                              555
         AG_PARLIST = 'DX_DY_DZ'                                          555
         CALL AXSHAPE                                                     555
C    *                                                                    555
C    Create PSAH :                                                        556
C    Call To Local Procedure PSAH                                         556
         ASSIGN 6480 TO BLOCK_PSAH                                        556
         GO TO  6470                                                      556
6480     CONTINUE                                                         556
C    *                                                                    556
C    Geant POSITION : PSAH ===> MOTHER='PSAR' X=0 Y=0 Z=8.25/2. <===      557
         CALL AGSRESET                                                    557
         AG_MOTHER = 'PSAR'                                               557
         AG_X = 0                                                         557
         AG_Y = 0                                                         557
         AG_Z = 8.25/2.                                                   557
         AG_TITLE = 'PSAH'                                                557
         AG_EXNAME = 'POSITION'                                           557
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      557
         CALL AXPOSITION                                                  557
C    *                                                                    557
C    Geant POSITION : PSAH ===> MOTHER='PSAR' X=0 Y=0 Z=-8.25/2. <===     557
         CALL AGSRESET                                                    558
         AG_MOTHER = 'PSAR'                                               558
         AG_X = 0                                                         558
         AG_Y = 0                                                         558
         AG_Z = -8.25/2.                                                  558
         AG_TITLE = 'PSAH'                                                558
         AG_EXNAME = 'POSITION'                                           558
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      558
         CALL AXPOSITION                                                  558
C    *                                                                    558
5551  CALL AGSPOPB                                                        559
      GO TO BLOCK_PSAR, (5550 ,5560)                                      559
C End of Local Procedure                                                  559
C ----------------------------------------------------------------------  560
C ------------ PSAH                                                       560
C Begin Local Procedure PSAH                                              561
C PSAH BASE SLIDE CUTOUT IN VERTICAL PART                                 561
6470  CONTINUE                                                            561
         AG_TITLE='PSAH'                                                  561
         AG_MODULE=MODULE                                                 561
         AG_MARK='V00'                                                    561
         CALL AGSPUSH                                                     561
         AG_MODULE=MODULE                                                 561
C    *                                                                    561
C    Geant ATTRIBUTE : PSAH ===> SEEN=1 COLO=1 <===                       562
         CALL AGSRESET                                                    562
         AG_SEEN = 1                                                      562
         AG_COLO = 1                                                      562
         AG_TITLE = 'PSAH'                                                562
         AG_EXNAME = 'ATTRIBUTE'                                          562
         AG_PARLIST = 'SEEN_COLO'                                         562
         CALL AXATTRIBUTE                                                 562
C    *                                                                    562
C    Geant MATERIAL : AIR ===> inherited arguments used <===              562
         CALL AGSRESET                                                    563
         AG_TITLE = 'AIR'                                                 563
         AG_EXNAME = 'MATERIAL'                                           563
         AG_PARLIST = 'NONE'                                              563
         CALL AXMATERIAL                                                  563
C    *                                                                    563
C    Geant SHAPE : BOX ===> DX=0.7/2 DY=4.4/2. DZ=5.5/2 <===              563
         CALL AGSRESET                                                    564
         AG_DX = 0.7/2                                                    564
         AG_DY = 4.4/2.                                                   564
         AG_DZ = 5.5/2                                                    564
         AG_TITLE = 'BOX'                                                 564
         AG_EXNAME = 'SHAPE'                                              564
         AG_PARLIST = 'DX_DY_DZ'                                          564
         CALL AXSHAPE                                                     564
C    *                                                                    564
6471  CALL AGSPOPB                                                        565
      GO TO BLOCK_PSAH, (6470 ,6480)                                      565
C End of Local Procedure                                                  565
C ----------------------------------------------------------------------  566
C ------------ PSAT                                                       566
C Begin Local Procedure PSAT                                              567
C PSAT BASE SLIDE CENTER, TOP AND BOTTOM PART                             567
5570  CONTINUE                                                            567
         AG_TITLE='PSAT'                                                  567
         AG_MODULE=MODULE                                                 567
         AG_MARK='V00'                                                    567
         CALL AGSPUSH                                                     567
         AG_MODULE=MODULE                                                 567
C    *                                                                    567
C    Geant ATTRIBUTE : PSAT ===> SEEN=1 COLO=4 <===                       568
         CALL AGSRESET                                                    568
         AG_SEEN = 1                                                      568
         AG_COLO = 4                                                      568
         AG_TITLE = 'PSAT'                                                568
         AG_EXNAME = 'ATTRIBUTE'                                          568
         AG_PARLIST = 'SEEN_COLO'                                         568
         CALL AXATTRIBUTE                                                 568
C    *                                                                    568
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        568
         CALL AGSRESET                                                    569
         AG_TITLE = 'ALUMINIUM'                                           569
         AG_EXNAME = 'MATERIAL'                                           569
         AG_PARLIST = 'NONE'                                              569
         CALL AXMATERIAL                                                  569
C    *                                                                    569
C    Geant SHAPE : BOX ===> DX=4.5/2 DY=0.55/2. DZ=16.5/2 <===            569
         CALL AGSRESET                                                    570
         AG_DX = 4.5/2                                                    570
         AG_DY = 0.55/2.                                                  570
         AG_DZ = 16.5/2                                                   570
         AG_TITLE = 'BOX'                                                 570
         AG_EXNAME = 'SHAPE'                                              570
         AG_PARLIST = 'DX_DY_DZ'                                          570
         CALL AXSHAPE                                                     570
C    *                                                                    570
5571  CALL AGSPOPB                                                        571
      GO TO BLOCK_PSAT, (5570 ,5580)                                      571
C End of Local Procedure                                                  571
C ----------------------------------------------------------------------  572
C ------------ PSAS                                                       572
C Begin Local Procedure PSAS                                              573
C PSAS BASE SLIDE - CABLE HOLDER SUPPORT                                  573
5590  CONTINUE                                                            573
         AG_TITLE='PSAS'                                                  573
         AG_MODULE=MODULE                                                 573
         AG_MARK='V00'                                                    573
         CALL AGSPUSH                                                     573
         AG_MODULE=MODULE                                                 573
C    *                                                                    573
C    Geant ATTRIBUTE : PSAS ===> SEEN=1 COLO=4 <===                       574
         CALL AGSRESET                                                    574
         AG_SEEN = 1                                                      574
         AG_COLO = 4                                                      574
         AG_TITLE = 'PSAS'                                                574
         AG_EXNAME = 'ATTRIBUTE'                                          574
         AG_PARLIST = 'SEEN_COLO'                                         574
         CALL AXATTRIBUTE                                                 574
C    *                                                                    574
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        574
         CALL AGSRESET                                                    575
         AG_TITLE = 'ALUMINIUM'                                           575
         AG_EXNAME = 'MATERIAL'                                           575
         AG_PARLIST = 'NONE'                                              575
         CALL AXMATERIAL                                                  575
C    *                                                                    575
C    Geant SHAPE : BOX ===> DX=0.7/2. DY=1.5/2. DZ=5.0/2 <===             575
         CALL AGSRESET                                                    576
         AG_DX = 0.7/2.                                                   576
         AG_DY = 1.5/2.                                                   576
         AG_DZ = 5.0/2                                                    576
         AG_TITLE = 'BOX'                                                 576
         AG_EXNAME = 'SHAPE'                                              576
         AG_PARLIST = 'DX_DY_DZ'                                          576
         CALL AXSHAPE                                                     576
C    *                                                                    576
5591  CALL AGSPOPB                                                        577
      GO TO BLOCK_PSAS, (5590 ,5600)                                      577
C End of Local Procedure                                                  577
C ----------------------------------------------------------------------  578
C ------------ PSAA                                                       578
C Begin Local Procedure PSAA                                              579
C PSAA BASE SLIDE-1 CENTER, VERTICAL PART                                 579
5610  CONTINUE                                                            579
         AG_TITLE='PSAA'                                                  579
         AG_MODULE=MODULE                                                 579
         AG_MARK='V00'                                                    579
         CALL AGSPUSH                                                     579
         AG_MODULE=MODULE                                                 579
C    *                                                                    579
C    Geant ATTRIBUTE : PSAA ===> SEEN=1 COLO=3 <===                       580
         CALL AGSRESET                                                    580
         AG_SEEN = 1                                                      580
         AG_COLO = 3                                                      580
         AG_TITLE = 'PSAA'                                                580
         AG_EXNAME = 'ATTRIBUTE'                                          580
         AG_PARLIST = 'SEEN_COLO'                                         580
         CALL AXATTRIBUTE                                                 580
C    *                                                                    580
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        580
         CALL AGSRESET                                                    581
         AG_TITLE = 'ALUMINIUM'                                           581
         AG_EXNAME = 'MATERIAL'                                           581
         AG_PARLIST = 'NONE'                                              581
         CALL AXMATERIAL                                                  581
C    *                                                                    581
C    Geant SHAPE : BOX ===> DX=0.3/2 DY=8.6/2. DZ=20.04/2 <===            581
         CALL AGSRESET                                                    582
         AG_DX = 0.3/2                                                    582
         AG_DY = 8.6/2.                                                   582
         AG_DZ = 20.04/2                                                  582
         AG_TITLE = 'BOX'                                                 582
         AG_EXNAME = 'SHAPE'                                              582
         AG_PARLIST = 'DX_DY_DZ'                                          582
         CALL AXSHAPE                                                     582
C    *                                                                    582
C    Create PSAC :                                                        583
C    Call To Local Procedure PSAC                                         583
         ASSIGN 6500 TO BLOCK_PSAC                                        583
         GO TO  6490                                                      583
6500     CONTINUE                                                         583
C    *                                                                    583
C    Geant POSITION : PSAC ===> MOTHER='PSAA' X=0 Y=0 Z=-2.8 <===         584
         CALL AGSRESET                                                    584
         AG_MOTHER = 'PSAA'                                               584
         AG_X = 0                                                         584
         AG_Y = 0                                                         584
         AG_Z = -2.8                                                      584
         AG_TITLE = 'PSAC'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      584
         CALL AXPOSITION                                                  584
C    *                                                                    584
5611  CALL AGSPOPB                                                        585
      GO TO BLOCK_PSAA, (5610 ,5620)                                      585
C End of Local Procedure                                                  585
C ----------------------------------------------------------------------  586
C ------------ PSAC                                                       586
C Begin Local Procedure PSAC                                              587
C PSAC BASE SLIDE-1 HOLE IN VERTICAL PART                                 587
6490  CONTINUE                                                            587
         AG_TITLE='PSAC'                                                  587
         AG_MODULE=MODULE                                                 587
         AG_MARK='V00'                                                    587
         CALL AGSPUSH                                                     587
         AG_MODULE=MODULE                                                 587
C    *                                                                    587
C    Geant ATTRIBUTE : PSAC ===> SEEN=1 COLO=3 <===                       588
         CALL AGSRESET                                                    588
         AG_SEEN = 1                                                      588
         AG_COLO = 3                                                      588
         AG_TITLE = 'PSAC'                                                588
         AG_EXNAME = 'ATTRIBUTE'                                          588
         AG_PARLIST = 'SEEN_COLO'                                         588
         CALL AXATTRIBUTE                                                 588
C    *                                                                    588
C    Geant MATERIAL : AIR ===> inherited arguments used <===              588
         CALL AGSRESET                                                    589
         AG_TITLE = 'AIR'                                                 589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant SHAPE : BOX ===> DX=0.3/2 DY=5.8/2. DZ=13.5/2 <===             589
         CALL AGSRESET                                                    590
         AG_DX = 0.3/2                                                    590
         AG_DY = 5.8/2.                                                   590
         AG_DZ = 13.5/2                                                   590
         AG_TITLE = 'BOX'                                                 590
         AG_EXNAME = 'SHAPE'                                              590
         AG_PARLIST = 'DX_DY_DZ'                                          590
         CALL AXSHAPE                                                     590
C    *                                                                    590
6491  CALL AGSPOPB                                                        591
      GO TO BLOCK_PSAC, (6490 ,6500)                                      591
C End of Local Procedure                                                  591
C ----------------------------------------------------------------------  592
C ------------ PSAB                                                       592
C Begin Local Procedure PSAB                                              593
C PSAB BASE SLIDE-1 CENTER, TOP AND BOTTOM                                593
5630  CONTINUE                                                            593
         AG_TITLE='PSAB'                                                  593
         AG_MODULE=MODULE                                                 593
         AG_MARK='V00'                                                    593
         CALL AGSPUSH                                                     593
         AG_MODULE=MODULE                                                 593
C    *                                                                    593
C    Geant ATTRIBUTE : PSAB ===> SEEN=1 COLO=3 <===                       594
         CALL AGSRESET                                                    594
         AG_SEEN = 1                                                      594
         AG_COLO = 3                                                      594
         AG_TITLE = 'PSAB'                                                594
         AG_EXNAME = 'ATTRIBUTE'                                          594
         AG_PARLIST = 'SEEN_COLO'                                         594
         CALL AXATTRIBUTE                                                 594
C    *                                                                    594
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        594
         CALL AGSRESET                                                    595
         AG_TITLE = 'ALUMINIUM'                                           595
         AG_EXNAME = 'MATERIAL'                                           595
         AG_PARLIST = 'NONE'                                              595
         CALL AXMATERIAL                                                  595
C    *                                                                    595
C    Geant SHAPE : BOX ===> DX=1.92/2. DY=0.2/2 DZ=20.04/2 <===           595
         CALL AGSRESET                                                    596
         AG_DX = 1.92/2.                                                  596
         AG_DY = 0.2/2                                                    596
         AG_DZ = 20.04/2                                                  596
         AG_TITLE = 'BOX'                                                 596
         AG_EXNAME = 'SHAPE'                                              596
         AG_PARLIST = 'DX_DY_DZ'                                          596
         CALL AXSHAPE                                                     596
C    *                                                                    596
5631  CALL AGSPOPB                                                        597
      GO TO BLOCK_PSAB, (5630 ,5640)                                      597
C End of Local Procedure                                                  597
C ----------------------------------------------------------------------  598
C ------------ PSAD                                                       598
C Begin Local Procedure PSAD                                              599
C PSAD BASE SLIDE-1 HINGEPIN, INCLUDES PART FROM BH TERMINATION           599
5650  CONTINUE                                                            599
         AG_TITLE='PSAD'                                                  599
         AG_MODULE=MODULE                                                 599
         AG_MARK='V00'                                                    599
         CALL AGSPUSH                                                     599
         AG_MODULE=MODULE                                                 599
C    *                                                                    599
C    Geant ATTRIBUTE : PSAD ===> SEEN=1 COLO=4 <===                       600
         CALL AGSRESET                                                    600
         AG_SEEN = 1                                                      600
         AG_COLO = 4                                                      600
         AG_TITLE = 'PSAD'                                                600
         AG_EXNAME = 'ATTRIBUTE'                                          600
         AG_PARLIST = 'SEEN_COLO'                                         600
         CALL AXATTRIBUTE                                                 600
C    *                                                                    600
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        600
         CALL AGSRESET                                                    601
         AG_TITLE = 'ALUMINIUM'                                           601
         AG_EXNAME = 'MATERIAL'                                           601
         AG_PARLIST = 'NONE'                                              601
         CALL AXMATERIAL                                                  601
C    *                                                                    601
C    Geant SHAPE : TUBE ===> RMIN=0.19 RMAX=1.1125 DZ=3.3884/2 <===       601
         CALL AGSRESET                                                    602
         AG_RMIN = 0.19                                                   602
         AG_RMAX = 1.1125                                                 602
         AG_DZ = 3.3884/2                                                 602
         AG_TITLE = 'TUBE'                                                602
         AG_EXNAME = 'SHAPE'                                              602
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      602
         CALL AXSHAPE                                                     602
C    *                                                                    602
5651  CALL AGSPOPB                                                        603
      GO TO BLOCK_PSAD, (5650 ,5660)                                      603
C End of Local Procedure                                                  603
C ----------------------------------------------------------------------  604
C ------------ PSAE                                                       604
C Begin Local Procedure PSAE                                              605
C PSAE BASE SLIDE-1 ARM HINGE FEMALE                                      605
5670  CONTINUE                                                            605
         AG_TITLE='PSAE'                                                  605
         AG_MODULE=MODULE                                                 605
         AG_MARK='V00'                                                    605
         CALL AGSPUSH                                                     605
         AG_MODULE=MODULE                                                 605
C    *                                                                    605
C    Geant ATTRIBUTE : PSAE ===> SEEN=1 COLO=4 <===                       606
         CALL AGSRESET                                                    606
         AG_SEEN = 1                                                      606
         AG_COLO = 4                                                      606
         AG_TITLE = 'PSAE'                                                606
         AG_EXNAME = 'ATTRIBUTE'                                          606
         AG_PARLIST = 'SEEN_COLO'                                         606
         CALL AXATTRIBUTE                                                 606
C    *                                                                    606
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        606
         CALL AGSRESET                                                    607
         AG_TITLE = 'ALUMINIUM'                                           607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant SHAPE : BOX ===> DX=1.6/2. DY=4.0/2 DZ=3.0/2 <===              607
         CALL AGSRESET                                                    608
         AG_DX = 1.6/2.                                                   608
         AG_DY = 4.0/2                                                    608
         AG_DZ = 3.0/2                                                    608
         AG_TITLE = 'BOX'                                                 608
         AG_EXNAME = 'SHAPE'                                              608
         AG_PARLIST = 'DX_DY_DZ'                                          608
         CALL AXSHAPE                                                     608
C    *                                                                    608
5671  CALL AGSPOPB                                                        609
      GO TO BLOCK_PSAE, (5670 ,5680)                                      609
C End of Local Procedure                                                  609
C ----------------------------------------------------------------------  610
C ------------ PSAF                                                       610
C Begin Local Procedure PSAF                                              611
C PSAF BASE SLIDE-1 ARM HINGE TERMINATION PLATE                           611
5690  CONTINUE                                                            611
         AG_TITLE='PSAF'                                                  611
         AG_MODULE=MODULE                                                 611
         AG_MARK='V00'                                                    611
         CALL AGSPUSH                                                     611
         AG_MODULE=MODULE                                                 611
C    *                                                                    611
C    Geant ATTRIBUTE : PSAF ===> SEEN=1 COLO=4 <===                       612
         CALL AGSRESET                                                    612
         AG_SEEN = 1                                                      612
         AG_COLO = 4                                                      612
         AG_TITLE = 'PSAF'                                                612
         AG_EXNAME = 'ATTRIBUTE'                                          612
         AG_PARLIST = 'SEEN_COLO'                                         612
         CALL AXATTRIBUTE                                                 612
C    *                                                                    612
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        612
         CALL AGSRESET                                                    613
         AG_TITLE = 'ALUMINIUM'                                           613
         AG_EXNAME = 'MATERIAL'                                           613
         AG_PARLIST = 'NONE'                                              613
         CALL AXMATERIAL                                                  613
C    *                                                                    613
C    Geant SHAPE : BOX ===> DX=0.51/2. DY=11.2/2 DZ=3.1/2 <===            613
         CALL AGSRESET                                                    614
         AG_DX = 0.51/2.                                                  614
         AG_DY = 11.2/2                                                   614
         AG_DZ = 3.1/2                                                    614
         AG_TITLE = 'BOX'                                                 614
         AG_EXNAME = 'SHAPE'                                              614
         AG_PARLIST = 'DX_DY_DZ'                                          614
         CALL AXSHAPE                                                     614
C    *                                                                    614
5691  CALL AGSPOPB                                                        615
      GO TO BLOCK_PSAF, (5690 ,5700)                                      615
C End of Local Procedure                                                  615
C ----------------------------------------------------------------------  616
C ------------ PSMB                                                       616
C Begin Local Procedure PSMB                                              617
C PSMB MTB G10 BOARD                                                      617
5720  CONTINUE                                                            617
         AG_TITLE='PSMB'                                                  617
         AG_MODULE=MODULE                                                 617
         AG_MARK='V00'                                                    617
         CALL AGSPUSH                                                     617
         AG_MODULE=MODULE                                                 617
C    *                                                                    617
C    Geant ATTRIBUTE : PSMB ===> SEEN=1 COLO=3 <===                       618
         CALL AGSRESET                                                    618
         AG_SEEN = 1                                                      618
         AG_COLO = 3                                                      618
         AG_TITLE = 'PSMB'                                                618
         AG_EXNAME = 'ATTRIBUTE'                                          618
         AG_PARLIST = 'SEEN_COLO'                                         618
         CALL AXATTRIBUTE                                                 618
C    *                                                                    618
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          618
         CALL AGSRESET                                                    619
         AG_A = 28.08                                                     619
         AG_Z = 14                                                        619
         AG_W = 0.6*1*28./60.                                             619
         AG_TITLE = 'SI'                                                  619
         AG_EXNAME = 'COMPONENT'                                          619
         AG_PARLIST = 'A_Z_W'                                             619
         CALL AXCOMPONENT                                                 619
C    *                                                                    619
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               619
         CALL AGSRESET                                                    620
         AG_A = 16                                                        620
         AG_Z = 8                                                         620
         AG_W = 0.6*2*16./60.                                             620
         AG_TITLE = 'O'                                                   620
         AG_EXNAME = 'COMPONENT'                                          620
         AG_PARLIST = 'A_Z_W'                                             620
         CALL AXCOMPONENT                                                 620
C    *                                                                    620
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              620
         CALL AGSRESET                                                    621
         AG_A = 12                                                        621
         AG_Z = 6                                                         621
         AG_W = 0.4*8*12./174.                                            621
         AG_TITLE = 'C'                                                   621
         AG_EXNAME = 'COMPONENT'                                          621
         AG_PARLIST = 'A_Z_W'                                             621
         CALL AXCOMPONENT                                                 621
C    *                                                                    621
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               621
         CALL AGSRESET                                                    622
         AG_A = 1                                                         622
         AG_Z = 1                                                         622
         AG_W = 0.4*14*1./174.                                            622
         AG_TITLE = 'H'                                                   622
         AG_EXNAME = 'COMPONENT'                                          622
         AG_PARLIST = 'A_Z_W'                                             622
         CALL AXCOMPONENT                                                 622
C    *                                                                    622
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              622
         CALL AGSRESET                                                    623
         AG_A = 16                                                        623
         AG_Z = 8                                                         623
         AG_W = 0.4*4*16./174.                                            623
         AG_TITLE = 'O'                                                   623
         AG_EXNAME = 'COMPONENT'                                          623
         AG_PARLIST = 'A_Z_W'                                             623
         CALL AXCOMPONENT                                                 623
C    *                                                                    623
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               623
         CALL AGSRESET                                                    624
         AG_DENS = 1.7                                                    624
         AG_TITLE = 'G10'                                                 624
         AG_EXNAME = 'MIXTURE'                                            624
         AG_PARLIST = 'DENS'                                              624
         CALL AXMIXTURE                                                   624
C    *                                                                    624
C    Geant SHAPE : BOX ===> DX=0.15*2.2/2 DY=11.4/2 DZ=26.1/2 <===        624
         CALL AGSRESET                                                    625
         AG_DX = 0.15*2.2/2                                               625
         AG_DY = 11.4/2                                                   625
         AG_DZ = 26.1/2                                                   625
         AG_TITLE = 'BOX'                                                 625
         AG_EXNAME = 'SHAPE'                                              625
         AG_PARLIST = 'DX_DY_DZ'                                          625
         CALL AXSHAPE                                                     625
C    *                                                                    625
5721  CALL AGSPOPB                                                        626
      GO TO BLOCK_PSMB, (5720 ,5730)                                      626
C End of Local Procedure                                                  626
C ----------------------------------------------------------------------  627
C ------------ PSMT                                                       627
C Begin Local Procedure PSMT                                              628
C PSMT MTB AL SUPPORT BLOCK                                               628
5780  CONTINUE                                                            628
         AG_TITLE='PSMT'                                                  628
         AG_MODULE=MODULE                                                 628
         AG_MARK='V00'                                                    628
         CALL AGSPUSH                                                     628
         AG_MODULE=MODULE                                                 628
C    *                                                                    628
C    Geant ATTRIBUTE : PSMT ===> SEEN=1 COLO=4 <===                       629
         CALL AGSRESET                                                    629
         AG_SEEN = 1                                                      629
         AG_COLO = 4                                                      629
         AG_TITLE = 'PSMT'                                                629
         AG_EXNAME = 'ATTRIBUTE'                                          629
         AG_PARLIST = 'SEEN_COLO'                                         629
         CALL AXATTRIBUTE                                                 629
C    *                                                                    629
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        629
         CALL AGSRESET                                                    630
         AG_TITLE = 'ALUMINIUM'                                           630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant SHAPE : BOX ===> DX=1.5*0.5 DY=0.8/2 DZ=1.5/2 <===             630
         CALL AGSRESET                                                    631
         AG_DX = 1.5*0.5                                                  631
         AG_DY = 0.8/2                                                    631
         AG_DZ = 1.5/2                                                    631
         AG_TITLE = 'BOX'                                                 631
         AG_EXNAME = 'SHAPE'                                              631
         AG_PARLIST = 'DX_DY_DZ'                                          631
         CALL AXSHAPE                                                     631
C    *                                                                    631
5781  CALL AGSPOPB                                                        632
      GO TO BLOCK_PSMT, (5780 ,5790)                                      632
C End of Local Procedure                                                  632
C ----------------------------------------------------------------------  633
C ------------ PSPA                                                       633
C Begin Local Procedure PSPA                                              634
C PSPA MTB CABLE PARK FRONT                                               634
5740  CONTINUE                                                            634
         AG_TITLE='PSPA'                                                  634
         AG_MODULE=MODULE                                                 634
         AG_MARK='V00'                                                    634
         CALL AGSPUSH                                                     634
         AG_MODULE=MODULE                                                 634
C    *                                                                    634
C    Geant ATTRIBUTE : PSPA ===> SEEN=1 COLO=5 <===                       635
         CALL AGSRESET                                                    635
         AG_SEEN = 1                                                      635
         AG_COLO = 5                                                      635
         AG_TITLE = 'PSPA'                                                635
         AG_EXNAME = 'ATTRIBUTE'                                          635
         AG_PARLIST = 'SEEN_COLO'                                         635
         CALL AXATTRIBUTE                                                 635
C    *                                                                    635
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           635
         CALL AGSRESET                                                    636
         AG_TITLE = 'DELRIN'                                              636
         AG_EXNAME = 'MATERIAL'                                           636
         AG_PARLIST = 'NONE'                                              636
         CALL AXMATERIAL                                                  636
C    *                                                                    636
C    Geant SHAPE : BOX ===> DX=0.35/2 DY=11.4/2 DZ=4.9/2 <===             636
         CALL AGSRESET                                                    637
         AG_DX = 0.35/2                                                   637
         AG_DY = 11.4/2                                                   637
         AG_DZ = 4.9/2                                                    637
         AG_TITLE = 'BOX'                                                 637
         AG_EXNAME = 'SHAPE'                                              637
         AG_PARLIST = 'DX_DY_DZ'                                          637
         CALL AXSHAPE                                                     637
C    *                                                                    637
5741  CALL AGSPOPB                                                        638
      GO TO BLOCK_PSPA, (5740 ,5750)                                      638
C End of Local Procedure                                                  638
C ----------------------------------------------------------------------  639
C ------------ PSPB                                                       639
C Begin Local Procedure PSPB                                              640
C PSPB MTB CABLE PARK FRONT (2)                                           640
5760  CONTINUE                                                            640
         AG_TITLE='PSPB'                                                  640
         AG_MODULE=MODULE                                                 640
         AG_MARK='V00'                                                    640
         CALL AGSPUSH                                                     640
         AG_MODULE=MODULE                                                 640
C    *                                                                    640
C    Geant ATTRIBUTE : PSPB ===> SEEN=1 COLO=5 <===                       641
         CALL AGSRESET                                                    641
         AG_SEEN = 1                                                      641
         AG_COLO = 5                                                      641
         AG_TITLE = 'PSPB'                                                641
         AG_EXNAME = 'ATTRIBUTE'                                          641
         AG_PARLIST = 'SEEN_COLO'                                         641
         CALL AXATTRIBUTE                                                 641
C    *                                                                    641
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           641
         CALL AGSRESET                                                    642
         AG_TITLE = 'DELRIN'                                              642
         AG_EXNAME = 'MATERIAL'                                           642
         AG_PARLIST = 'NONE'                                              642
         CALL AXMATERIAL                                                  642
C    *                                                                    642
C    Geant SHAPE : BOX ===> DX=0.30/2 DY=11.4/2 DZ=11.85/2 <===           642
         CALL AGSRESET                                                    643
         AG_DX = 0.30/2                                                   643
         AG_DY = 11.4/2                                                   643
         AG_DZ = 11.85/2                                                  643
         AG_TITLE = 'BOX'                                                 643
         AG_EXNAME = 'SHAPE'                                              643
         AG_PARLIST = 'DX_DY_DZ'                                          643
         CALL AXSHAPE                                                     643
C    *                                                                    643
5761  CALL AGSPOPB                                                        644
      GO TO BLOCK_PSPB, (5760 ,5770)                                      644
C End of Local Procedure                                                  644
C ----------------------------------------------------------------------  645
C ------------ PSMD                                                       645
C Begin Local Procedure PSMD                                              646
C PSMD PSMD MTB SUPPORT ARM                                               646
6170  CONTINUE                                                            646
         AG_TITLE='PSMD'                                                  646
         AG_MODULE=MODULE                                                 646
         AG_MARK='V00'                                                    646
         CALL AGSPUSH                                                     646
         AG_MODULE=MODULE                                                 646
C    *                                                                    646
C    Geant ATTRIBUTE : PSMD ===> SEEN=1 COLO=600 <===                     647
         CALL AGSRESET                                                    647
         AG_SEEN = 1                                                      647
         AG_COLO = 600                                                    647
         AG_TITLE = 'PSMD'                                                647
         AG_EXNAME = 'ATTRIBUTE'                                          647
         AG_PARLIST = 'SEEN_COLO'                                         647
         CALL AXATTRIBUTE                                                 647
C    *                                                                    647
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        647
         CALL AGSRESET                                                    648
         AG_TITLE = 'ALUMINIUM'                                           648
         AG_EXNAME = 'MATERIAL'                                           648
         AG_PARLIST = 'NONE'                                              648
         CALL AXMATERIAL                                                  648
C    *                                                                    648
C    Geant SHAPE : TRAP ===> DZ=2 THET=0 PHI=0 H1=4.085 BL1=1.0975, TL1=  648
C    1.5 ALP1=-17.0941 H2=4.085 BL2=1.0975, TL2=1.5 ALP2=-17.0941 <===    648
         CALL AGSRESET                                                    651
         AG_DZ = 2                                                        651
         AG_THET = 0                                                      651
         AG_PHI = 0                                                       651
         AG_H1 = 4.085                                                    651
         AG_BL1 = 1.0975                                                  651
         AG_TL1 = 1.5                                                     651
         AG_ALP1 = -17.0941                                               651
         AG_H2 = 4.085                                                    651
         AG_BL2 = 1.0975                                                  651
         AG_TL2 = 1.5                                                     651
         AG_ALP2 = -17.0941                                               651
         AG_TITLE = 'TRAP'                                                651
         AG_EXNAME = 'SHAPE'                                              651
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       651
         CALL AXSHAPE                                                     651
C    *                                                                    651
C    Create PSMI :                                                        652
C    Call To Local Procedure PSMI                                         652
         ASSIGN 6520 TO BLOCK_PSMI                                        652
         GO TO  6510                                                      652
6520     CONTINUE                                                         652
C    *                                                                    652
C    Geant POSITION : PSMI ===> X=-0.07333 Y=-0.1 Z=0 <===                653
         CALL AGSRESET                                                    653
         AG_X = -0.07333                                                  653
         AG_Y = -0.1                                                      653
         AG_Z = 0                                                         653
         AG_TITLE = 'PSMI'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'X_Y_Z'                                             653
         CALL AXPOSITION                                                  653
C    *                                                                    653
6171  CALL AGSPOPB                                                        654
      GO TO BLOCK_PSMD, (6170 ,6180)                                      654
C End of Local Procedure                                                  654
C ----------------------------------------------------------------------  655
C ------------ PSMI                                                       655
C Begin Local Procedure PSMI                                              656
C PSMI PSMI MTB SUPPORT ARM HOLLOW                                        656
6510  CONTINUE                                                            656
         AG_TITLE='PSMI'                                                  656
         AG_MODULE=MODULE                                                 656
         AG_MARK='V00'                                                    656
         CALL AGSPUSH                                                     656
         AG_MODULE=MODULE                                                 656
C    *                                                                    656
C    Geant ATTRIBUTE : PSMI ===> SEEN=1 COLO=632 <===                     657
         CALL AGSRESET                                                    657
         AG_SEEN = 1                                                      657
         AG_COLO = 632                                                    657
         AG_TITLE = 'PSMI'                                                657
         AG_EXNAME = 'ATTRIBUTE'                                          657
         AG_PARLIST = 'SEEN_COLO'                                         657
         CALL AXATTRIBUTE                                                 657
C    *                                                                    657
C    Geant MATERIAL : AIR ===> inherited arguments used <===              657
         CALL AGSRESET                                                    658
         AG_TITLE = 'AIR'                                                 658
         AG_EXNAME = 'MATERIAL'                                           658
         AG_PARLIST = 'NONE'                                              658
         CALL AXMATERIAL                                                  658
C    *                                                                    658
C    Geant SHAPE : TRAP ===> DZ=1.8 THET=0 PHI=0 H1=3.985, BL1=0.994219   658
C    TL1=1.38687 ALP1=-17.0941 H2=3.985, BL2=0.994219 TL2=1.38687 ALP2=-  658
C    17.0941 <===                                                         658
         CALL AGSRESET                                                    661
         AG_DZ = 1.8                                                      661
         AG_THET = 0                                                      661
         AG_PHI = 0                                                       661
         AG_H1 = 3.985                                                    661
         AG_BL1 = 0.994219                                                661
         AG_TL1 = 1.38687                                                 661
         AG_ALP1 = -17.0941                                               661
         AG_H2 = 3.985                                                    661
         AG_BL2 = 0.994219                                                661
         AG_TL2 = 1.38687                                                 661
         AG_ALP2 = -17.0941                                               661
         AG_TITLE = 'TRAP'                                                661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       661
         CALL AXSHAPE                                                     661
C    *                                                                    661
6511  CALL AGSPOPB                                                        662
      GO TO BLOCK_PSMI, (6510 ,6520)                                      662
C End of Local Procedure                                                  662
C ----------------------------------------------------------------------  663
C ------------ PSMJ                                                       663
C Begin Local Procedure PSMJ                                              664
C PSMJ MTB AL SUPPORT ARM PIECE 1                                         664
6190  CONTINUE                                                            664
         AG_TITLE='PSMJ'                                                  664
         AG_MODULE=MODULE                                                 664
         AG_MARK='V00'                                                    664
         CALL AGSPUSH                                                     664
         AG_MODULE=MODULE                                                 664
C    *                                                                    664
C    Geant ATTRIBUTE : PSMJ ===> SEEN=1 COLO=4 <===                       665
         CALL AGSRESET                                                    665
         AG_SEEN = 1                                                      665
         AG_COLO = 4                                                      665
         AG_TITLE = 'PSMJ'                                                665
         AG_EXNAME = 'ATTRIBUTE'                                          665
         AG_PARLIST = 'SEEN_COLO'                                         665
         CALL AXATTRIBUTE                                                 665
C    *                                                                    665
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        665
         CALL AGSRESET                                                    666
         AG_TITLE = 'ALUMINIUM'                                           666
         AG_EXNAME = 'MATERIAL'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXMATERIAL                                                  666
C    *                                                                    666
C    Geant SHAPE : BOX ===> DX=0.1 DY=2.4/2 DZ=(123.97-101.87)/2. <===    666
         CALL AGSRESET                                                    667
         AG_DX = 0.1                                                      667
         AG_DY = 2.4/2                                                    667
         AG_DZ = (123.97-101.87)/2.                                       667
         AG_TITLE = 'BOX'                                                 667
         AG_EXNAME = 'SHAPE'                                              667
         AG_PARLIST = 'DX_DY_DZ'                                          667
         CALL AXSHAPE                                                     667
C    *                                                                    667
6191  CALL AGSPOPB                                                        668
      GO TO BLOCK_PSMJ, (6190 ,6200)                                      668
C End of Local Procedure                                                  668
C ----------------------------------------------------------------------  669
C ------------ PSMK                                                       669
C Begin Local Procedure PSMK                                              670
C PSMK PSMK MTB SUPPORT PLATE A                                           670
6210  CONTINUE                                                            670
         AG_TITLE='PSMK'                                                  670
         AG_MODULE=MODULE                                                 670
         AG_MARK='V00'                                                    670
         CALL AGSPUSH                                                     670
         AG_MODULE=MODULE                                                 670
C    *                                                                    670
C    Geant ATTRIBUTE : PSMK ===> SEEN=1 COLO=632 <===                     671
         CALL AGSRESET                                                    671
         AG_SEEN = 1                                                      671
         AG_COLO = 632                                                    671
         AG_TITLE = 'PSMK'                                                671
         AG_EXNAME = 'ATTRIBUTE'                                          671
         AG_PARLIST = 'SEEN_COLO'                                         671
         CALL AXATTRIBUTE                                                 671
C    *                                                                    671
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        671
         CALL AGSRESET                                                    672
         AG_TITLE = 'ALUMINIUM'                                           672
         AG_EXNAME = 'MATERIAL'                                           672
         AG_PARLIST = 'NONE'                                              672
         CALL AXMATERIAL                                                  672
C    *                                                                    672
C    Geant SHAPE : TRAP ===> DZ=0.1 THET=0 PHI=0 H1=0.975, BL1=13.5373 T  672
C    L1=10.3804 ALP1=-121.703 H2=0.975, BL2=13.5373 TL2=10.3804 ALP2=-12  672
C    1.703 <===                                                           672
         CALL AGSRESET                                                    675
         AG_DZ = 0.1                                                      675
         AG_THET = 0                                                      675
         AG_PHI = 0                                                       675
         AG_H1 = 0.975                                                    675
         AG_BL1 = 13.5373                                                 675
         AG_TL1 = 10.3804                                                 675
         AG_ALP1 = -121.703                                               675
         AG_H2 = 0.975                                                    675
         AG_BL2 = 13.5373                                                 675
         AG_TL2 = 10.3804                                                 675
         AG_ALP2 = -121.703                                               675
         AG_TITLE = 'TRAP'                                                675
         AG_EXNAME = 'SHAPE'                                              675
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       675
         CALL AXSHAPE                                                     675
C    *                                                                    675
6211  CALL AGSPOPB                                                        676
      GO TO BLOCK_PSMK, (6210 ,6220)                                      676
C End of Local Procedure                                                  676
C ----------------------------------------------------------------------  677
C ------------ PSML                                                       677
C Begin Local Procedure PSML                                              678
C PSML PSML MTB SUPPORT PLATE B                                           678
6230  CONTINUE                                                            678
         AG_TITLE='PSML'                                                  678
         AG_MODULE=MODULE                                                 678
         AG_MARK='V00'                                                    678
         CALL AGSPUSH                                                     678
         AG_MODULE=MODULE                                                 678
C    *                                                                    678
C    Geant ATTRIBUTE : PSML ===> SEEN=1 COLO=632 <===                     679
         CALL AGSRESET                                                    679
         AG_SEEN = 1                                                      679
         AG_COLO = 632                                                    679
         AG_TITLE = 'PSML'                                                679
         AG_EXNAME = 'ATTRIBUTE'                                          679
         AG_PARLIST = 'SEEN_COLO'                                         679
         CALL AXATTRIBUTE                                                 679
C    *                                                                    679
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        679
         CALL AGSRESET                                                    680
         AG_TITLE = 'ALUMINIUM'                                           680
         AG_EXNAME = 'MATERIAL'                                           680
         AG_PARLIST = 'NONE'                                              680
         CALL AXMATERIAL                                                  680
C    *                                                                    680
C    Geant SHAPE : TRAP ===> DZ=0.1 THET=0 PHI=0 H1=1.05 BL1=13.622, TL1  680
C    =13.5373 ALP1=-107.367 H2=1.05 BL2=13.622, TL2=13.5373 ALP2=-107.36  680
C    7 <===                                                               680
         CALL AGSRESET                                                    683
         AG_DZ = 0.1                                                      683
         AG_THET = 0                                                      683
         AG_PHI = 0                                                       683
         AG_H1 = 1.05                                                     683
         AG_BL1 = 13.622                                                  683
         AG_TL1 = 13.5373                                                 683
         AG_ALP1 = -107.367                                               683
         AG_H2 = 1.05                                                     683
         AG_BL2 = 13.622                                                  683
         AG_TL2 = 13.5373                                                 683
         AG_ALP2 = -107.367                                               683
         AG_TITLE = 'TRAP'                                                683
         AG_EXNAME = 'SHAPE'                                              683
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       683
         CALL AXSHAPE                                                     683
C    *                                                                    683
6231  CALL AGSPOPB                                                        684
      GO TO BLOCK_PSML, (6230 ,6240)                                      684
C End of Local Procedure                                                  684
C ----------------------------------------------------------------------  685
C ------------ PSMM                                                       685
C Begin Local Procedure PSMM                                              686
C PSMM PSMM MTB SUPPORT PLATE C                                           686
6250  CONTINUE                                                            686
         AG_TITLE='PSMM'                                                  686
         AG_MODULE=MODULE                                                 686
         AG_MARK='V00'                                                    686
         CALL AGSPUSH                                                     686
         AG_MODULE=MODULE                                                 686
C    *                                                                    686
C    Geant ATTRIBUTE : PSMM ===> SEEN=1 COLO=632 <===                     687
         CALL AGSRESET                                                    687
         AG_SEEN = 1                                                      687
         AG_COLO = 632                                                    687
         AG_TITLE = 'PSMM'                                                687
         AG_EXNAME = 'ATTRIBUTE'                                          687
         AG_PARLIST = 'SEEN_COLO'                                         687
         CALL AXATTRIBUTE                                                 687
C    *                                                                    687
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        687
         CALL AGSRESET                                                    688
         AG_TITLE = 'ALUMINIUM'                                           688
         AG_EXNAME = 'MATERIAL'                                           688
         AG_PARLIST = 'NONE'                                              688
         CALL AXMATERIAL                                                  688
C    *                                                                    688
C    Geant SHAPE : TRAP ===> DZ=0.1 THET=0 PHI=0 H1=1.05 BL1=13.622, TL1  688
C    =13.6218 ALP1=72.8375 H2=1.05 BL2=13.622, TL2=13.6218 ALP2=72.8375   688
C    <===                                                                 688
         CALL AGSRESET                                                    691
         AG_DZ = 0.1                                                      691
         AG_THET = 0                                                      691
         AG_PHI = 0                                                       691
         AG_H1 = 1.05                                                     691
         AG_BL1 = 13.622                                                  691
         AG_TL1 = 13.6218                                                 691
         AG_ALP1 = 72.8375                                                691
         AG_H2 = 1.05                                                     691
         AG_BL2 = 13.622                                                  691
         AG_TL2 = 13.6218                                                 691
         AG_ALP2 = 72.8375                                                691
         AG_TITLE = 'TRAP'                                                691
         AG_EXNAME = 'SHAPE'                                              691
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       691
         CALL AXSHAPE                                                     691
C    *                                                                    691
6251  CALL AGSPOPB                                                        692
      GO TO BLOCK_PSMM, (6250 ,6260)                                      692
C End of Local Procedure                                                  692
C ----------------------------------------------------------------------  693
C ------------ PSMN                                                       693
C Begin Local Procedure PSMN                                              694
C PSMN PSMN MTB SUPPORT PLATE D                                           694
6270  CONTINUE                                                            694
         AG_TITLE='PSMN'                                                  694
         AG_MODULE=MODULE                                                 694
         AG_MARK='V00'                                                    694
         CALL AGSPUSH                                                     694
         AG_MODULE=MODULE                                                 694
C    *                                                                    694
C    Geant ATTRIBUTE : PSMN ===> SEEN=1 COLO=632 <===                     695
         CALL AGSRESET                                                    695
         AG_SEEN = 1                                                      695
         AG_COLO = 632                                                    695
         AG_TITLE = 'PSMN'                                                695
         AG_EXNAME = 'ATTRIBUTE'                                          695
         AG_PARLIST = 'SEEN_COLO'                                         695
         CALL AXATTRIBUTE                                                 695
C    *                                                                    695
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        695
         CALL AGSRESET                                                    696
         AG_TITLE = 'ALUMINIUM'                                           696
         AG_EXNAME = 'MATERIAL'                                           696
         AG_PARLIST = 'NONE'                                              696
         CALL AXMATERIAL                                                  696
C    *                                                                    696
C    Geant SHAPE : TRAP ===> DZ=0.1 THET=0 PHI=0 H1=1.45 BL1=13.6218, TL  696
C    1=13.6214 ALP1=72.8375 H2=1.45 BL2=13.6218, TL2=13.6214 ALP2=72.837  696
C    5 <===                                                               696
         CALL AGSRESET                                                    699
         AG_DZ = 0.1                                                      699
         AG_THET = 0                                                      699
         AG_PHI = 0                                                       699
         AG_H1 = 1.45                                                     699
         AG_BL1 = 13.6218                                                 699
         AG_TL1 = 13.6214                                                 699
         AG_ALP1 = 72.8375                                                699
         AG_H2 = 1.45                                                     699
         AG_BL2 = 13.6218                                                 699
         AG_TL2 = 13.6214                                                 699
         AG_ALP2 = 72.8375                                                699
         AG_TITLE = 'TRAP'                                                699
         AG_EXNAME = 'SHAPE'                                              699
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       699
         CALL AXSHAPE                                                     699
C    *                                                                    699
6271  CALL AGSPOPB                                                        700
      GO TO BLOCK_PSMN, (6270 ,6280)                                      700
C End of Local Procedure                                                  700
C ----------------------------------------------------------------------  701
C ------------ PSMO                                                       701
C Begin Local Procedure PSMO                                              702
C PSMO PSME MTB SUPPORT PLATE E PART B                                    702
6290  CONTINUE                                                            702
         AG_TITLE='PSMO'                                                  702
         AG_MODULE=MODULE                                                 702
         AG_MARK='V00'                                                    702
         CALL AGSPUSH                                                     702
         AG_MODULE=MODULE                                                 702
C    *                                                                    702
C    Geant ATTRIBUTE : PSMO ===> SEEN=1 COLO=632 <===                     703
         CALL AGSRESET                                                    703
         AG_SEEN = 1                                                      703
         AG_COLO = 632                                                    703
         AG_TITLE = 'PSMO'                                                703
         AG_EXNAME = 'ATTRIBUTE'                                          703
         AG_PARLIST = 'SEEN_COLO'                                         703
         CALL AXATTRIBUTE                                                 703
C    *                                                                    703
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        703
         CALL AGSRESET                                                    704
         AG_TITLE = 'ALUMINIUM'                                           704
         AG_EXNAME = 'MATERIAL'                                           704
         AG_PARLIST = 'NONE'                                              704
         CALL AXMATERIAL                                                  704
C    *                                                                    704
C    Geant SHAPE : TRAP ===> DZ=0.1 THET=0 PHI=0 H1=0.65 BL1=13.6214, TL  704
C    1=11.5116 ALP1=58.3593 H2=0.65 BL2=13.6214, TL2=11.5116 ALP2=58.359  704
C    3 <===                                                               704
         CALL AGSRESET                                                    707
         AG_DZ = 0.1                                                      707
         AG_THET = 0                                                      707
         AG_PHI = 0                                                       707
         AG_H1 = 0.65                                                     707
         AG_BL1 = 13.6214                                                 707
         AG_TL1 = 11.5116                                                 707
         AG_ALP1 = 58.3593                                                707
         AG_H2 = 0.65                                                     707
         AG_BL2 = 13.6214                                                 707
         AG_TL2 = 11.5116                                                 707
         AG_ALP2 = 58.3593                                                707
         AG_TITLE = 'TRAP'                                                707
         AG_EXNAME = 'SHAPE'                                              707
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       707
         CALL AXSHAPE                                                     707
C    *                                                                    707
6291  CALL AGSPOPB                                                        708
      GO TO BLOCK_PSMO, (6290 ,6300)                                      708
C End of Local Procedure                                                  708
C ----------------------------------------------------------------------  709
C ------------ PSMQ                                                       709
C Begin Local Procedure PSMQ                                              710
C PSMQ MTB AL SUPPORT ARM VERTICAL PIECE 2                                710
6310  CONTINUE                                                            710
         AG_TITLE='PSMQ'                                                  710
         AG_MODULE=MODULE                                                 710
         AG_MARK='V00'                                                    710
         CALL AGSPUSH                                                     710
         AG_MODULE=MODULE                                                 710
C    *                                                                    710
C    Geant ATTRIBUTE : PSMQ ===> SEEN=1 COLO=4 <===                       711
         CALL AGSRESET                                                    711
         AG_SEEN = 1                                                      711
         AG_COLO = 4                                                      711
         AG_TITLE = 'PSMQ'                                                711
         AG_EXNAME = 'ATTRIBUTE'                                          711
         AG_PARLIST = 'SEEN_COLO'                                         711
         CALL AXATTRIBUTE                                                 711
C    *                                                                    711
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        711
         CALL AGSRESET                                                    712
         AG_TITLE = 'ALUMINIUM'                                           712
         AG_EXNAME = 'MATERIAL'                                           712
         AG_PARLIST = 'NONE'                                              712
         CALL AXMATERIAL                                                  712
C    *                                                                    712
C    Geant SHAPE : BOX ===> DX=0.1 DY=0.40 DZ=13.5373 <===                712
         CALL AGSRESET                                                    713
         AG_DX = 0.1                                                      713
         AG_DY = 0.40                                                     713
         AG_DZ = 13.5373                                                  713
         AG_TITLE = 'BOX'                                                 713
         AG_EXNAME = 'SHAPE'                                              713
         AG_PARLIST = 'DX_DY_DZ'                                          713
         CALL AXSHAPE                                                     713
C    *                                                                    713
6311  CALL AGSPOPB                                                        714
      GO TO BLOCK_PSMQ, (6310 ,6320)                                      714
C End of Local Procedure                                                  714
C ----------------------------------------------------------------------  715
C ------------ PSMR                                                       715
C Begin Local Procedure PSMR                                              716
C PSMR MTB AL SUPPORT ARM VERTICAL PIECE 3                                716
6330  CONTINUE                                                            716
         AG_TITLE='PSMR'                                                  716
         AG_MODULE=MODULE                                                 716
         AG_MARK='V00'                                                    716
         CALL AGSPUSH                                                     716
         AG_MODULE=MODULE                                                 716
C    *                                                                    716
C    Geant ATTRIBUTE : PSMR ===> SEEN=1 COLO=4 <===                       717
         CALL AGSRESET                                                    717
         AG_SEEN = 1                                                      717
         AG_COLO = 4                                                      717
         AG_TITLE = 'PSMR'                                                717
         AG_EXNAME = 'ATTRIBUTE'                                          717
         AG_PARLIST = 'SEEN_COLO'                                         717
         CALL AXATTRIBUTE                                                 717
C    *                                                                    717
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        717
         CALL AGSRESET                                                    718
         AG_TITLE = 'ALUMINIUM'                                           718
         AG_EXNAME = 'MATERIAL'                                           718
         AG_PARLIST = 'NONE'                                              718
         CALL AXMATERIAL                                                  718
C    *                                                                    718
C    Geant SHAPE : BOX ===> DX=0.1 DY=0.25 DZ=13.622 <===                 718
         CALL AGSRESET                                                    719
         AG_DX = 0.1                                                      719
         AG_DY = 0.25                                                     719
         AG_DZ = 13.622                                                   719
         AG_TITLE = 'BOX'                                                 719
         AG_EXNAME = 'SHAPE'                                              719
         AG_PARLIST = 'DX_DY_DZ'                                          719
         CALL AXSHAPE                                                     719
C    *                                                                    719
6331  CALL AGSPOPB                                                        720
      GO TO BLOCK_PSMR, (6330 ,6340)                                      720
C End of Local Procedure                                                  720
C ----------------------------------------------------------------------  721
C ------------ PSMP                                                       721
C Begin Local Procedure PSMP                                              722
C PSMP MTB AL SUPPORT ARM VERTICAL PIECE 5                                722
6350  CONTINUE                                                            722
         AG_TITLE='PSMP'                                                  722
         AG_MODULE=MODULE                                                 722
         AG_MARK='V00'                                                    722
         CALL AGSPUSH                                                     722
         AG_MODULE=MODULE                                                 722
C    *                                                                    722
C    Geant ATTRIBUTE : PSMP ===> SEEN=1 COLO=4 <===                       723
         CALL AGSRESET                                                    723
         AG_SEEN = 1                                                      723
         AG_COLO = 4                                                      723
         AG_TITLE = 'PSMP'                                                723
         AG_EXNAME = 'ATTRIBUTE'                                          723
         AG_PARLIST = 'SEEN_COLO'                                         723
         CALL AXATTRIBUTE                                                 723
C    *                                                                    723
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        723
         CALL AGSRESET                                                    724
         AG_TITLE = 'ALUMINIUM'                                           724
         AG_EXNAME = 'MATERIAL'                                           724
         AG_PARLIST = 'NONE'                                              724
         CALL AXMATERIAL                                                  724
C    *                                                                    724
C    Geant SHAPE : BOX ===> DX=0.1 DY=0.75 DZ=13.622 <===                 724
         CALL AGSRESET                                                    725
         AG_DX = 0.1                                                      725
         AG_DY = 0.75                                                     725
         AG_DZ = 13.622                                                   725
         AG_TITLE = 'BOX'                                                 725
         AG_EXNAME = 'SHAPE'                                              725
         AG_PARLIST = 'DX_DY_DZ'                                          725
         CALL AXSHAPE                                                     725
C    *                                                                    725
6351  CALL AGSPOPB                                                        726
      GO TO BLOCK_PSMP, (6350 ,6360)                                      726
C End of Local Procedure                                                  726
C ----------------------------------------------------------------------  727
C ------------ PSCJ                                                       727
C Begin Local Procedure PSCJ                                              728
C PSCJ MTB CONNECTOR                                                      728
5800  CONTINUE                                                            728
         AG_TITLE='PSCJ'                                                  728
         AG_MODULE=MODULE                                                 728
         AG_MARK='V00'                                                    728
         CALL AGSPUSH                                                     728
         AG_MODULE=MODULE                                                 728
C    *                                                                    728
C    Geant ATTRIBUTE : PSMJ ===> SEEN=1 COLO=2 <===                       729
         CALL AGSRESET                                                    729
         AG_SEEN = 1                                                      729
         AG_COLO = 2                                                      729
         AG_TITLE = 'PSMJ'                                                729
         AG_EXNAME = 'ATTRIBUTE'                                          729
         AG_PARLIST = 'SEEN_COLO'                                         729
         CALL AXATTRIBUTE                                                 729
C    *                                                                    729
C    Geant MATERIAL : ZINC ===> A=65.38 Z=30 DENS=7.14 <===               729
         CALL AGSRESET                                                    730
         AG_A = 65.38                                                     730
         AG_Z = 30                                                        730
         AG_DENS = 7.14                                                   730
         AG_TITLE = 'ZINC'                                                730
         AG_EXNAME = 'MATERIAL'                                           730
         AG_PARLIST = 'A_Z_DENS'                                          730
         CALL AXMATERIAL                                                  730
C    *                                                                    730
C    Geant SHAPE : BOX ===> DX=0.4 DY=4.2/2 DZ=3.0/2. <===                730
         CALL AGSRESET                                                    731
         AG_DX = 0.4                                                      731
         AG_DY = 4.2/2                                                    731
         AG_DZ = 3.0/2.                                                   731
         AG_TITLE = 'BOX'                                                 731
         AG_EXNAME = 'SHAPE'                                              731
         AG_PARLIST = 'DX_DY_DZ'                                          731
         CALL AXSHAPE                                                     731
C    *                                                                    731
5801  CALL AGSPOPB                                                        732
      GO TO BLOCK_PSCJ, (5800 ,5810,5820)                                 732
C End of Local Procedure                                                  732
C ----------------------------------------------------------------------  733
C ------------ PSCM                                                       733
C Begin Local Procedure PSCM                                              734
C PSCM SIGNAL CABLE                                                       734
5830  CONTINUE                                                            734
         AG_TITLE='PSCM'                                                  734
         AG_MODULE=MODULE                                                 734
         AG_MARK='V00'                                                    734
         CALL AGSPUSH                                                     734
         AG_MODULE=MODULE                                                 734
C    *                                                                    734
C    Geant ATTRIBUTE : PSCM ===> SEEN=1 COLO=632 <===                     735
         CALL AGSRESET                                                    735
         AG_SEEN = 1                                                      735
         AG_COLO = 632                                                    735
         AG_TITLE = 'PSCM'                                                735
         AG_EXNAME = 'ATTRIBUTE'                                          735
         AG_PARLIST = 'SEEN_COLO'                                         735
         CALL AXATTRIBUTE                                                 735
C    *                                                                    735
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           735
         CALL AGSRESET                                                    736
         AG_TITLE = 'COPPER'                                              736
         AG_EXNAME = 'MATERIAL'                                           736
         AG_PARLIST = 'NONE'                                              736
         CALL AXMATERIAL                                                  736
C    *                                                                    736
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=10 <===                   736
         CALL AGSRESET                                                    737
         AG_RMIN = 0                                                      737
         AG_RMAX = 0.3                                                    737
         AG_DZ = 10                                                       737
         AG_TITLE = 'TUBE'                                                737
         AG_EXNAME = 'SHAPE'                                              737
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      737
         CALL AXSHAPE                                                     737
C    *                                                                    737
5831  CALL AGSPOPB                                                        738
      GO TO BLOCK_PSCM, (5830 ,5840)                                      738
C End of Local Procedure                                                  738
C ----------------------------------------------------------------------  739
C ------------ PSCN                                                       739
C Begin Local Procedure PSCN                                              740
C PSCN MTB CABLE                                                          740
5850  CONTINUE                                                            740
         AG_TITLE='PSCN'                                                  740
         AG_MODULE=MODULE                                                 740
         AG_MARK='V00'                                                    740
         CALL AGSPUSH                                                     740
         AG_MODULE=MODULE                                                 740
C    *                                                                    740
C    Geant ATTRIBUTE : PSCN ===> SEEN=1 COLO=2 <===                       741
         CALL AGSRESET                                                    741
         AG_SEEN = 1                                                      741
         AG_COLO = 2                                                      741
         AG_TITLE = 'PSCN'                                                741
         AG_EXNAME = 'ATTRIBUTE'                                          741
         AG_PARLIST = 'SEEN_COLO'                                         741
         CALL AXATTRIBUTE                                                 741
C    *                                                                    741
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           741
         CALL AGSRESET                                                    742
         AG_TITLE = 'COPPER'                                              742
         AG_EXNAME = 'MATERIAL'                                           742
         AG_PARLIST = 'NONE'                                              742
         CALL AXMATERIAL                                                  742
C    *                                                                    742
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=12.4484 <===              742
         CALL AGSRESET                                                    743
         AG_RMIN = 0                                                      743
         AG_RMAX = 0.3                                                    743
         AG_DZ = 12.4484                                                  743
         AG_TITLE = 'TUBE'                                                743
         AG_EXNAME = 'SHAPE'                                              743
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      743
         CALL AXSHAPE                                                     743
C    *                                                                    743
5851  CALL AGSPOPB                                                        744
      GO TO BLOCK_PSCN, (5850 ,5860,5870)                                 744
C End of Local Procedure                                                  744
C ----------------------------------------------------------------------  745
C ------------ PSCS                                                       745
C Begin Local Procedure PSCS                                              746
C PSCS MTB CABLE IN TRAY                                                  746
5880  CONTINUE                                                            746
         AG_TITLE='PSCS'                                                  746
         AG_MODULE=MODULE                                                 746
         AG_MARK='V00'                                                    746
         CALL AGSPUSH                                                     746
         AG_MODULE=MODULE                                                 746
C    *                                                                    746
C    Geant ATTRIBUTE : PSCS ===> SEEN=1 COLO=2 <===                       747
         CALL AGSRESET                                                    747
         AG_SEEN = 1                                                      747
         AG_COLO = 2                                                      747
         AG_TITLE = 'PSCS'                                                747
         AG_EXNAME = 'ATTRIBUTE'                                          747
         AG_PARLIST = 'SEEN_COLO'                                         747
         CALL AXATTRIBUTE                                                 747
C    *                                                                    747
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           747
         CALL AGSRESET                                                    748
         AG_TITLE = 'COPPER'                                              748
         AG_EXNAME = 'MATERIAL'                                           748
         AG_PARLIST = 'NONE'                                              748
         CALL AXMATERIAL                                                  748
C    *                                                                    748
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=46.7 <===                 748
         CALL AGSRESET                                                    749
         AG_RMIN = 0                                                      749
         AG_RMAX = 0.3                                                    749
         AG_DZ = 46.7                                                     749
         AG_TITLE = 'TUBE'                                                749
         AG_EXNAME = 'SHAPE'                                              749
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      749
         CALL AXSHAPE                                                     749
C    *                                                                    749
5881  CALL AGSPOPB                                                        750
      GO TO BLOCK_PSCS, (5880 ,5890)                                      750
C End of Local Procedure                                                  750
C ----------------------------------------------------------------------  751
C ------------ PSCO                                                       751
C Begin Local Procedure PSCO                                              752
C PSCO MTB CABLE                                                          752
5900  CONTINUE                                                            752
         AG_TITLE='PSCO'                                                  752
         AG_MODULE=MODULE                                                 752
         AG_MARK='V00'                                                    752
         CALL AGSPUSH                                                     752
         AG_MODULE=MODULE                                                 752
C    *                                                                    752
C    Geant ATTRIBUTE : PSCO ===> SEEN=1 COLO=2 <===                       753
         CALL AGSRESET                                                    753
         AG_SEEN = 1                                                      753
         AG_COLO = 2                                                      753
         AG_TITLE = 'PSCO'                                                753
         AG_EXNAME = 'ATTRIBUTE'                                          753
         AG_PARLIST = 'SEEN_COLO'                                         753
         CALL AXATTRIBUTE                                                 753
C    *                                                                    753
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           753
         CALL AGSRESET                                                    754
         AG_TITLE = 'COPPER'                                              754
         AG_EXNAME = 'MATERIAL'                                           754
         AG_PARLIST = 'NONE'                                              754
         CALL AXMATERIAL                                                  754
C    *                                                                    754
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=13.2843 <===              754
         CALL AGSRESET                                                    755
         AG_RMIN = 0                                                      755
         AG_RMAX = 0.3                                                    755
         AG_DZ = 13.2843                                                  755
         AG_TITLE = 'TUBE'                                                755
         AG_EXNAME = 'SHAPE'                                              755
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      755
         CALL AXSHAPE                                                     755
C    *                                                                    755
5901  CALL AGSPOPB                                                        756
      GO TO BLOCK_PSCO, (5900 ,5910)                                      756
C End of Local Procedure                                                  756
C ----------------------------------------------------------------------  757
C ------------ PSCT                                                       757
C Begin Local Procedure PSCT                                              758
C PSCT MTB CABLE IN TRAY                                                  758
5920  CONTINUE                                                            758
         AG_TITLE='PSCT'                                                  758
         AG_MODULE=MODULE                                                 758
         AG_MARK='V00'                                                    758
         CALL AGSPUSH                                                     758
         AG_MODULE=MODULE                                                 758
C    *                                                                    758
C    Geant ATTRIBUTE : PSCT ===> SEEN=1 COLO=2 <===                       759
         CALL AGSRESET                                                    759
         AG_SEEN = 1                                                      759
         AG_COLO = 2                                                      759
         AG_TITLE = 'PSCT'                                                759
         AG_EXNAME = 'ATTRIBUTE'                                          759
         AG_PARLIST = 'SEEN_COLO'                                         759
         CALL AXATTRIBUTE                                                 759
C    *                                                                    759
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           759
         CALL AGSRESET                                                    760
         AG_TITLE = 'COPPER'                                              760
         AG_EXNAME = 'MATERIAL'                                           760
         AG_PARLIST = 'NONE'                                              760
         CALL AXMATERIAL                                                  760
C    *                                                                    760
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=43.3 <===                 760
         CALL AGSRESET                                                    761
         AG_RMIN = 0                                                      761
         AG_RMAX = 0.3                                                    761
         AG_DZ = 43.3                                                     761
         AG_TITLE = 'TUBE'                                                761
         AG_EXNAME = 'SHAPE'                                              761
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      761
         CALL AXSHAPE                                                     761
C    *                                                                    761
5921  CALL AGSPOPB                                                        762
      GO TO BLOCK_PSCT, (5920 ,5930)                                      762
C End of Local Procedure                                                  762
C ----------------------------------------------------------------------  763
C ------------ PSCP                                                       763
C Begin Local Procedure PSCP                                              764
C PSCP MTB CABLE                                                          764
5940  CONTINUE                                                            764
         AG_TITLE='PSCP'                                                  764
         AG_MODULE=MODULE                                                 764
         AG_MARK='V00'                                                    764
         CALL AGSPUSH                                                     764
         AG_MODULE=MODULE                                                 764
C    *                                                                    764
C    Geant ATTRIBUTE : PSCP ===> SEEN=1 COLO=2 <===                       765
         CALL AGSRESET                                                    765
         AG_SEEN = 1                                                      765
         AG_COLO = 2                                                      765
         AG_TITLE = 'PSCP'                                                765
         AG_EXNAME = 'ATTRIBUTE'                                          765
         AG_PARLIST = 'SEEN_COLO'                                         765
         CALL AXATTRIBUTE                                                 765
C    *                                                                    765
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           765
         CALL AGSRESET                                                    766
         AG_TITLE = 'COPPER'                                              766
         AG_EXNAME = 'MATERIAL'                                           766
         AG_PARLIST = 'NONE'                                              766
         CALL AXMATERIAL                                                  766
C    *                                                                    766
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=14.1227 <===              766
         CALL AGSRESET                                                    767
         AG_RMIN = 0                                                      767
         AG_RMAX = 0.3                                                    767
         AG_DZ = 14.1227                                                  767
         AG_TITLE = 'TUBE'                                                767
         AG_EXNAME = 'SHAPE'                                              767
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      767
         CALL AXSHAPE                                                     767
C    *                                                                    767
5941  CALL AGSPOPB                                                        768
      GO TO BLOCK_PSCP, (5940 ,5950,5960)                                 768
C End of Local Procedure                                                  768
C ----------------------------------------------------------------------  769
C ------------ PSCU                                                       769
C Begin Local Procedure PSCU                                              770
C PSCU MTB CABLE IN TRAY                                                  770
5970  CONTINUE                                                            770
         AG_TITLE='PSCU'                                                  770
         AG_MODULE=MODULE                                                 770
         AG_MARK='V00'                                                    770
         CALL AGSPUSH                                                     770
         AG_MODULE=MODULE                                                 770
C    *                                                                    770
C    Geant ATTRIBUTE : PSCU ===> SEEN=1 COLO=2 <===                       771
         CALL AGSRESET                                                    771
         AG_SEEN = 1                                                      771
         AG_COLO = 2                                                      771
         AG_TITLE = 'PSCU'                                                771
         AG_EXNAME = 'ATTRIBUTE'                                          771
         AG_PARLIST = 'SEEN_COLO'                                         771
         CALL AXATTRIBUTE                                                 771
C    *                                                                    771
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           771
         CALL AGSRESET                                                    772
         AG_TITLE = 'COPPER'                                              772
         AG_EXNAME = 'MATERIAL'                                           772
         AG_PARLIST = 'NONE'                                              772
         CALL AXMATERIAL                                                  772
C    *                                                                    772
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=39.9 <===                 772
         CALL AGSRESET                                                    773
         AG_RMIN = 0                                                      773
         AG_RMAX = 0.3                                                    773
         AG_DZ = 39.9                                                     773
         AG_TITLE = 'TUBE'                                                773
         AG_EXNAME = 'SHAPE'                                              773
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      773
         CALL AXSHAPE                                                     773
C    *                                                                    773
5971  CALL AGSPOPB                                                        774
      GO TO BLOCK_PSCU, (5970 ,5980)                                      774
C End of Local Procedure                                                  774
C ----------------------------------------------------------------------  775
C ------------ PSCQ                                                       775
C Begin Local Procedure PSCQ                                              776
C PSCQ MTB CABLE                                                          776
5990  CONTINUE                                                            776
         AG_TITLE='PSCQ'                                                  776
         AG_MODULE=MODULE                                                 776
         AG_MARK='V00'                                                    776
         CALL AGSPUSH                                                     776
         AG_MODULE=MODULE                                                 776
C    *                                                                    776
C    Geant ATTRIBUTE : PSCQ ===> SEEN=1 COLO=2 <===                       777
         CALL AGSRESET                                                    777
         AG_SEEN = 1                                                      777
         AG_COLO = 2                                                      777
         AG_TITLE = 'PSCQ'                                                777
         AG_EXNAME = 'ATTRIBUTE'                                          777
         AG_PARLIST = 'SEEN_COLO'                                         777
         CALL AXATTRIBUTE                                                 777
C    *                                                                    777
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           777
         CALL AGSRESET                                                    778
         AG_TITLE = 'COPPER'                                              778
         AG_EXNAME = 'MATERIAL'                                           778
         AG_PARLIST = 'NONE'                                              778
         CALL AXMATERIAL                                                  778
C    *                                                                    778
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=14.9641 <===              778
         CALL AGSRESET                                                    779
         AG_RMIN = 0                                                      779
         AG_RMAX = 0.3                                                    779
         AG_DZ = 14.9641                                                  779
         AG_TITLE = 'TUBE'                                                779
         AG_EXNAME = 'SHAPE'                                              779
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      779
         CALL AXSHAPE                                                     779
C    *                                                                    779
5991  CALL AGSPOPB                                                        780
      GO TO BLOCK_PSCQ, (5990 ,6000)                                      780
C End of Local Procedure                                                  780
C ----------------------------------------------------------------------  781
C ------------ PSCV                                                       781
C Begin Local Procedure PSCV                                              782
C PSCV MTB CABLE IN TRAY                                                  782
6010  CONTINUE                                                            782
         AG_TITLE='PSCV'                                                  782
         AG_MODULE=MODULE                                                 782
         AG_MARK='V00'                                                    782
         CALL AGSPUSH                                                     782
         AG_MODULE=MODULE                                                 782
C    *                                                                    782
C    Geant ATTRIBUTE : PSCV ===> SEEN=1 COLO=2 <===                       783
         CALL AGSRESET                                                    783
         AG_SEEN = 1                                                      783
         AG_COLO = 2                                                      783
         AG_TITLE = 'PSCV'                                                783
         AG_EXNAME = 'ATTRIBUTE'                                          783
         AG_PARLIST = 'SEEN_COLO'                                         783
         CALL AXATTRIBUTE                                                 783
C    *                                                                    783
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           783
         CALL AGSRESET                                                    784
         AG_TITLE = 'COPPER'                                              784
         AG_EXNAME = 'MATERIAL'                                           784
         AG_PARLIST = 'NONE'                                              784
         CALL AXMATERIAL                                                  784
C    *                                                                    784
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=36.5 <===                 784
         CALL AGSRESET                                                    785
         AG_RMIN = 0                                                      785
         AG_RMAX = 0.3                                                    785
         AG_DZ = 36.5                                                     785
         AG_TITLE = 'TUBE'                                                785
         AG_EXNAME = 'SHAPE'                                              785
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      785
         CALL AXSHAPE                                                     785
C    *                                                                    785
6011  CALL AGSPOPB                                                        786
      GO TO BLOCK_PSCV, (6010 ,6020)                                      786
C End of Local Procedure                                                  786
C ----------------------------------------------------------------------  787
C ------------ PSCR                                                       787
C Begin Local Procedure PSCR                                              788
C PSCR MTB CABLE                                                          788
6030  CONTINUE                                                            788
         AG_TITLE='PSCR'                                                  788
         AG_MODULE=MODULE                                                 788
         AG_MARK='V00'                                                    788
         CALL AGSPUSH                                                     788
         AG_MODULE=MODULE                                                 788
C    *                                                                    788
C    Geant ATTRIBUTE : PSCR ===> SEEN=1 COLO=2 <===                       789
         CALL AGSRESET                                                    789
         AG_SEEN = 1                                                      789
         AG_COLO = 2                                                      789
         AG_TITLE = 'PSCR'                                                789
         AG_EXNAME = 'ATTRIBUTE'                                          789
         AG_PARLIST = 'SEEN_COLO'                                         789
         CALL AXATTRIBUTE                                                 789
C    *                                                                    789
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           789
         CALL AGSRESET                                                    790
         AG_TITLE = 'COPPER'                                              790
         AG_EXNAME = 'MATERIAL'                                           790
         AG_PARLIST = 'NONE'                                              790
         CALL AXMATERIAL                                                  790
C    *                                                                    790
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=15.7778 <===              790
         CALL AGSRESET                                                    791
         AG_RMIN = 0                                                      791
         AG_RMAX = 0.3                                                    791
         AG_DZ = 15.7778                                                  791
         AG_TITLE = 'TUBE'                                                791
         AG_EXNAME = 'SHAPE'                                              791
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      791
         CALL AXSHAPE                                                     791
C    *                                                                    791
6031  CALL AGSPOPB                                                        792
      GO TO BLOCK_PSCR, (6030 ,6040)                                      792
C End of Local Procedure                                                  792
C ----------------------------------------------------------------------  793
C ------------ PSCW                                                       793
C Begin Local Procedure PSCW                                              794
C PSCW MTB CABLE IN TRAY                                                  794
6050  CONTINUE                                                            794
         AG_TITLE='PSCW'                                                  794
         AG_MODULE=MODULE                                                 794
         AG_MARK='V00'                                                    794
         CALL AGSPUSH                                                     794
         AG_MODULE=MODULE                                                 794
C    *                                                                    794
C    Geant ATTRIBUTE : PSCW ===> SEEN=1 COLO=2 <===                       795
         CALL AGSRESET                                                    795
         AG_SEEN = 1                                                      795
         AG_COLO = 2                                                      795
         AG_TITLE = 'PSCW'                                                795
         AG_EXNAME = 'ATTRIBUTE'                                          795
         AG_PARLIST = 'SEEN_COLO'                                         795
         CALL AXATTRIBUTE                                                 795
C    *                                                                    795
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           795
         CALL AGSRESET                                                    796
         AG_TITLE = 'COPPER'                                              796
         AG_EXNAME = 'MATERIAL'                                           796
         AG_PARLIST = 'NONE'                                              796
         CALL AXMATERIAL                                                  796
C    *                                                                    796
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=33.1 <===                 796
         CALL AGSRESET                                                    797
         AG_RMIN = 0                                                      797
         AG_RMAX = 0.3                                                    797
         AG_DZ = 33.1                                                     797
         AG_TITLE = 'TUBE'                                                797
         AG_EXNAME = 'SHAPE'                                              797
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      797
         CALL AXSHAPE                                                     797
C    *                                                                    797
6051  CALL AGSPOPB                                                        798
      GO TO BLOCK_PSCW, (6050 ,6060)                                      798
C End of Local Procedure                                                  798
C ----------------------------------------------------------------------  799
C ------------ PSDA                                                       799
C Begin Local Procedure PSDA                                              800
C PSDA MTB CABLE                                                          800
6070  CONTINUE                                                            800
         AG_TITLE='PSDA'                                                  800
         AG_MODULE=MODULE                                                 800
         AG_MARK='V00'                                                    800
         CALL AGSPUSH                                                     800
         AG_MODULE=MODULE                                                 800
C    *                                                                    800
C    Geant ATTRIBUTE : PSDA ===> SEEN=1 COLO=632 <===                     801
         CALL AGSRESET                                                    801
         AG_SEEN = 1                                                      801
         AG_COLO = 632                                                    801
         AG_TITLE = 'PSDA'                                                801
         AG_EXNAME = 'ATTRIBUTE'                                          801
         AG_PARLIST = 'SEEN_COLO'                                         801
         CALL AXATTRIBUTE                                                 801
C    *                                                                    801
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           801
         CALL AGSRESET                                                    802
         AG_TITLE = 'COPPER'                                              802
         AG_EXNAME = 'MATERIAL'                                           802
         AG_PARLIST = 'NONE'                                              802
         CALL AXMATERIAL                                                  802
C    *                                                                    802
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.2 DZ=9.21243 <===              802
         CALL AGSRESET                                                    803
         AG_RMIN = 0                                                      803
         AG_RMAX = 0.2                                                    803
         AG_DZ = 9.21243                                                  803
         AG_TITLE = 'TUBE'                                                803
         AG_EXNAME = 'SHAPE'                                              803
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      803
         CALL AXSHAPE                                                     803
C    *                                                                    803
6071  CALL AGSPOPB                                                        804
      GO TO BLOCK_PSDA, (6070 ,6080)                                      804
C End of Local Procedure                                                  804
C ----------------------------------------------------------------------  805
C ------------ PSDB                                                       805
C Begin Local Procedure PSDB                                              806
C PSDB MTB CABLE                                                          806
6090  CONTINUE                                                            806
         AG_TITLE='PSDB'                                                  806
         AG_MODULE=MODULE                                                 806
         AG_MARK='V00'                                                    806
         CALL AGSPUSH                                                     806
         AG_MODULE=MODULE                                                 806
C    *                                                                    806
C    Geant ATTRIBUTE : PSDB ===> SEEN=1 COLO=632 <===                     807
         CALL AGSRESET                                                    807
         AG_SEEN = 1                                                      807
         AG_COLO = 632                                                    807
         AG_TITLE = 'PSDB'                                                807
         AG_EXNAME = 'ATTRIBUTE'                                          807
         AG_PARLIST = 'SEEN_COLO'                                         807
         CALL AXATTRIBUTE                                                 807
C    *                                                                    807
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           807
         CALL AGSRESET                                                    808
         AG_TITLE = 'COPPER'                                              808
         AG_EXNAME = 'MATERIAL'                                           808
         AG_PARLIST = 'NONE'                                              808
         CALL AXMATERIAL                                                  808
C    *                                                                    808
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.2 DZ=12.5274 <===              808
         CALL AGSRESET                                                    809
         AG_RMIN = 0                                                      809
         AG_RMAX = 0.2                                                    809
         AG_DZ = 12.5274                                                  809
         AG_TITLE = 'TUBE'                                                809
         AG_EXNAME = 'SHAPE'                                              809
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      809
         CALL AXSHAPE                                                     809
C    *                                                                    809
6091  CALL AGSPOPB                                                        810
      GO TO BLOCK_PSDB, (6090 ,6100)                                      810
C End of Local Procedure                                                  810
C ----------------------------------------------------------------------  811
C ------------ PSDC                                                       811
C Begin Local Procedure PSDC                                              812
C PSDC MTB CABLE                                                          812
6110  CONTINUE                                                            812
         AG_TITLE='PSDC'                                                  812
         AG_MODULE=MODULE                                                 812
         AG_MARK='V00'                                                    812
         CALL AGSPUSH                                                     812
         AG_MODULE=MODULE                                                 812
C    *                                                                    812
C    Geant ATTRIBUTE : PSDC ===> SEEN=1 COLO=632 <===                     813
         CALL AGSRESET                                                    813
         AG_SEEN = 1                                                      813
         AG_COLO = 632                                                    813
         AG_TITLE = 'PSDC'                                                813
         AG_EXNAME = 'ATTRIBUTE'                                          813
         AG_PARLIST = 'SEEN_COLO'                                         813
         CALL AXATTRIBUTE                                                 813
C    *                                                                    813
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           813
         CALL AGSRESET                                                    814
         AG_TITLE = 'COPPER'                                              814
         AG_EXNAME = 'MATERIAL'                                           814
         AG_PARLIST = 'NONE'                                              814
         CALL AXMATERIAL                                                  814
C    *                                                                    814
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.2 DZ=15.9042 <===              814
         CALL AGSRESET                                                    815
         AG_RMIN = 0                                                      815
         AG_RMAX = 0.2                                                    815
         AG_DZ = 15.9042                                                  815
         AG_TITLE = 'TUBE'                                                815
         AG_EXNAME = 'SHAPE'                                              815
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      815
         CALL AXSHAPE                                                     815
C    *                                                                    815
6111  CALL AGSPOPB                                                        816
      GO TO BLOCK_PSDC, (6110 ,6120)                                      816
C End of Local Procedure                                                  816
C ----------------------------------------------------------------------  817
C ------------ PSDD                                                       817
C Begin Local Procedure PSDD                                              818
C PSDD MTB CABLE                                                          818
6130  CONTINUE                                                            818
         AG_TITLE='PSDD'                                                  818
         AG_MODULE=MODULE                                                 818
         AG_MARK='V00'                                                    818
         CALL AGSPUSH                                                     818
         AG_MODULE=MODULE                                                 818
C    *                                                                    818
C    Geant ATTRIBUTE : PSDD ===> SEEN=1 COLO=632 <===                     819
         CALL AGSRESET                                                    819
         AG_SEEN = 1                                                      819
         AG_COLO = 632                                                    819
         AG_TITLE = 'PSDD'                                                819
         AG_EXNAME = 'ATTRIBUTE'                                          819
         AG_PARLIST = 'SEEN_COLO'                                         819
         CALL AXATTRIBUTE                                                 819
C    *                                                                    819
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           819
         CALL AGSRESET                                                    820
         AG_TITLE = 'COPPER'                                              820
         AG_EXNAME = 'MATERIAL'                                           820
         AG_PARLIST = 'NONE'                                              820
         CALL AXMATERIAL                                                  820
C    *                                                                    820
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.2 DZ=19.3077 <===              820
         CALL AGSRESET                                                    821
         AG_RMIN = 0                                                      821
         AG_RMAX = 0.2                                                    821
         AG_DZ = 19.3077                                                  821
         AG_TITLE = 'TUBE'                                                821
         AG_EXNAME = 'SHAPE'                                              821
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      821
         CALL AXSHAPE                                                     821
C    *                                                                    821
6131  CALL AGSPOPB                                                        822
      GO TO BLOCK_PSDD, (6130 ,6140)                                      822
C End of Local Procedure                                                  822
C ----------------------------------------------------------------------  823
C ------------ PSDE                                                       823
C Begin Local Procedure PSDE                                              824
C PSDE MTB CABLE                                                          824
6150  CONTINUE                                                            824
         AG_TITLE='PSDE'                                                  824
         AG_MODULE=MODULE                                                 824
         AG_MARK='V00'                                                    824
         CALL AGSPUSH                                                     824
         AG_MODULE=MODULE                                                 824
C    *                                                                    824
C    Geant ATTRIBUTE : PSDE ===> SEEN=1 COLO=632 <===                     825
         CALL AGSRESET                                                    825
         AG_SEEN = 1                                                      825
         AG_COLO = 632                                                    825
         AG_TITLE = 'PSDE'                                                825
         AG_EXNAME = 'ATTRIBUTE'                                          825
         AG_PARLIST = 'SEEN_COLO'                                         825
         CALL AXATTRIBUTE                                                 825
C    *                                                                    825
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           825
         CALL AGSRESET                                                    826
         AG_TITLE = 'COPPER'                                              826
         AG_EXNAME = 'MATERIAL'                                           826
         AG_PARLIST = 'NONE'                                              826
         CALL AXMATERIAL                                                  826
C    *                                                                    826
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.2 DZ=22.7217 <===              826
         CALL AGSRESET                                                    827
         AG_RMIN = 0                                                      827
         AG_RMAX = 0.2                                                    827
         AG_DZ = 22.7217                                                  827
         AG_TITLE = 'TUBE'                                                827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      827
         CALL AXSHAPE                                                     827
C    *                                                                    827
6151  CALL AGSPOPB                                                        828
      GO TO BLOCK_PSDE, (6150 ,6160)                                      828
C End of Local Procedure                                                  828
      WRITE(LOUT,6530)'===>>  [End AgML Module PsupGeo]  <<==='           828
6530  FORMAT(A32,/,/)                                                     828
C MODULE PSUPGEO                                                          830
      END                                                                 830
