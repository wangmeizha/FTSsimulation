C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FSCEGEO                                                  1
C FSCEGEO IS THE GEOMETRY OF THE FIBER SAMPLING CALORIMETER               1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FSCEGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FIBER SAMPLING CALORIMETER'/  1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 04-JUL-2011                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'04-JUL-2011'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR D.ARKHIPKIN <ARKHIPKIN@BNL.GOV>                                  2
      CHARACTER*40 AUTHOR /'D.ARKHIPKIN <ARKHIPKIN@BNL.GOV>'/             3
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
      INTEGER BLOCK_FSCE                                                  4
      INTEGER BLOCK_FSCT                                                  5
C -                                                                       6
      REAL FSCP_VERSION                                                   6
      EQUIVALENCE (BANK_FSCP(3),FSCP_VERSION,RE_FSCP_VERSION),            6
     *(NAMES_FSCP(1),NFSCP_VERSION)                                       6
      EQUIVALENCE (COMMEN_FSCP(1),CFSCP_VERSION),(MAP_FSCP(1,1),          6
     *MFSCP_VERSION)                                                      6
      CHARACTER CFSCP_VERSION*40/' '/                                     6
      CHARACTER NFSCP_VERSION*12/'R.VERSION'/                             6
      INTEGER MFSCP_VERSION(2)/1,1/                                       6
      REAL RE_FSCP_VERSION                                                6
C -                                                                       6
      REAL FSCP_TOWERWIDTH                                                6
      EQUIVALENCE (BANK_FSCP(4),FSCP_TOWERWIDTH,RE_FSCP_TOWERWIDTH),      6
     *(NAMES_FSCP(2),NFSCP_TOWERWIDTH)                                    6
      EQUIVALENCE (COMMEN_FSCP(2),CFSCP_TOWERWIDTH),(MAP_FSCP(1,2),       6
     *MFSCP_TOWERWIDTH)                                                   6
      CHARACTER CFSCP_TOWERWIDTH*40/' '/                                  6
      CHARACTER NFSCP_TOWERWIDTH*12/'R.TOWERWIDTH'/                       6
      INTEGER MFSCP_TOWERWIDTH(2)/1,1/                                    6
      REAL RE_FSCP_TOWERWIDTH                                             6
C -                                                                       6
      REAL FSCP_TOWERLENGTH                                               6
      EQUIVALENCE (BANK_FSCP(5),FSCP_TOWERLENGTH,RE_FSCP_TOWERLENGTH),    6
     *(NAMES_FSCP(3),NFSCP_TOWERLENGTH)                                   6
      EQUIVALENCE (COMMEN_FSCP(3),CFSCP_TOWERLENGTH),(MAP_FSCP(1,3),      6
     *MFSCP_TOWERLENGTH)                                                  6
      CHARACTER CFSCP_TOWERLENGTH*40/' '/                                 6
      CHARACTER NFSCP_TOWERLENGTH*12/'R.TOWERLENGTH'/                     6
      INTEGER MFSCP_TOWERLENGTH(2)/1,1/                                   6
      REAL RE_FSCP_TOWERLENGTH                                            6
C -                                                                       6
      REAL FSCP_NTOWERSX                                                  6
      EQUIVALENCE (BANK_FSCP(6),FSCP_NTOWERSX,RE_FSCP_NTOWERSX),          6
     *(NAMES_FSCP(4),NFSCP_NTOWERSX)                                      6
      EQUIVALENCE (COMMEN_FSCP(4),CFSCP_NTOWERSX),(MAP_FSCP(1,4),         6
     *MFSCP_NTOWERSX)                                                     6
      CHARACTER CFSCP_NTOWERSX*40/' '/                                    6
      CHARACTER NFSCP_NTOWERSX*12/'R.NTOWERSX'/                           6
      INTEGER MFSCP_NTOWERSX(2)/1,1/                                      6
      REAL RE_FSCP_NTOWERSX                                               6
C -                                                                       6
      REAL FSCP_NTOWERSY                                                  6
      EQUIVALENCE (BANK_FSCP(7),FSCP_NTOWERSY,RE_FSCP_NTOWERSY),          6
     *(NAMES_FSCP(5),NFSCP_NTOWERSY)                                      6
      EQUIVALENCE (COMMEN_FSCP(5),CFSCP_NTOWERSY),(MAP_FSCP(1,5),         6
     *MFSCP_NTOWERSY)                                                     6
      CHARACTER CFSCP_NTOWERSY*40/' '/                                    6
      CHARACTER NFSCP_NTOWERSY*12/'R.NTOWERSY'/                           6
      INTEGER MFSCP_NTOWERSY(2)/1,1/                                      6
      REAL RE_FSCP_NTOWERSY                                               6
C -                                                                       6
      REAL FSCP_NEMPTY                                                    6
      EQUIVALENCE (BANK_FSCP(8),FSCP_NEMPTY,RE_FSCP_NEMPTY),              6
     *(NAMES_FSCP(6),NFSCP_NEMPTY)                                        6
      EQUIVALENCE (COMMEN_FSCP(6),CFSCP_NEMPTY),(MAP_FSCP(1,6),           6
     *MFSCP_NEMPTY)                                                       6
      CHARACTER CFSCP_NEMPTY*40/' '/                                      6
      CHARACTER NFSCP_NEMPTY*12/'R.NEMPTY'/                               6
      INTEGER MFSCP_NEMPTY(2)/1,1/                                        6
      REAL RE_FSCP_NEMPTY                                                 6
C -                                                                       6
      REAL FSCP_DISTFROMVTX                                               6
      EQUIVALENCE (BANK_FSCP(9),FSCP_DISTFROMVTX,RE_FSCP_DISTFROMVTX),    6
     *(NAMES_FSCP(7),NFSCP_DISTFROMVTX)                                   6
      EQUIVALENCE (COMMEN_FSCP(7),CFSCP_DISTFROMVTX),(MAP_FSCP(1,7),      6
     *MFSCP_DISTFROMVTX)                                                  6
      CHARACTER CFSCP_DISTFROMVTX*40/' '/                                 6
      CHARACTER NFSCP_DISTFROMVTX*12/'R.DISTFROMVTX'/                     6
      INTEGER MFSCP_DISTFROMVTX(2)/1,1/                                   6
      REAL RE_FSCP_DISTFROMVTX                                            6
C -                                                                       6
      INTEGER LEN_FSCP,LLEN_FSCP,LINK_FSCP                                6
      PARAMETER (LEN_FSCP=9)                                              6
      PARAMETER (LLEN_FSCP=7)                                             6
      INTEGER FLAG_FSCP/0/,MAP_FSCP(2,LLEN_FSCP),IDNUM_FSCP(10)           6
      CHARACTER NAMES_FSCP(LLEN_FSCP)*12,COMMEN_FSCP(LLEN_FSCP)*40        6
      DATA IDNUM_FSCP/10*0/                                               6
      REAL BANK_FSCP(LEN_FSCP)                                            6
      CHARACTER*60 BPATH_FSCP/' '/                                        6
      SAVE FLAG_FSCP,MAP_FSCP,IDNUM_FSCP,NAMES_FSCP,COMMEN_FSCP,          6
     *BPATH_FSCP                                                          6
      DATA LINK_FSCP/0/,BANK_FSCP/LEN_FSCP*0/                             6
C *                                                                       6
      INTEGER XDIM,YDIM,XPOSMIN,XPOSMAX,YPOSMIN,YPOSMAX                   7
C *                                                                       9
      CALL AGSBEGM(MODULE,IPRIN)                                          9
      IF (IPRIN.LT.0) RETURN                                              9
C Filling bank FSCP - FIBER SAMPLING CALORIMETER DATA                     9
      BANK_TITLE='FIBER SAMPLING CALORIMETER DATA'                        9
      BPATH_FSCP='FSCP*'                                                  9
      IDNUM_FSCP(1)=0                                                     10
C GEOMETRY VERSION NUMBER                                                 10
      FSCP_VERSION=1                                                      10
      CFSCP_VERSION='GEOMETRY VERSION NUMBER'                             10
C HALF WIDTH OF TOWER, CM                                                 11
      FSCP_TOWERWIDTH=1.25                                                11
      CFSCP_TOWERWIDTH='HALF WIDTH OF TOWER, CM'                          11
C HALF LENGTH OF TOWER, CM                                                12
      FSCP_TOWERLENGTH=45.0                                               12
      CFSCP_TOWERLENGTH='HALF LENGTH OF TOWER, CM'                        12
C HALF NUMBER OF TOWERS IN X DIMENSION                                    13
      FSCP_NTOWERSX=20                                                    13
      CFSCP_NTOWERSX='HALF NUMBER OF TOWERS IN X DIMENSION'               13
C HALF NUMBER OF TOWERS IN Y DIMENSION                                    14
      FSCP_NTOWERSY=40                                                    14
      CFSCP_NTOWERSY='HALF NUMBER OF TOWERS IN Y DIMENSION'               14
C HALF NUMBER OF TOWERS IN A CENTER (HOLE), LIKE 3 FOR 6 * 6 HOLE         15
      FSCP_NEMPTY=3                                                       15
      CFSCP_NEMPTY='HALF NUMBER OF TOWERS IN A CENTER (HOLE), LIKE 3 FOR  15
     * 6 * 6 HOLE'                                                        15
C DISTANCE FROM EVENT VERTEX (0,0) IN CM, SHOULD EQUAL TO FMS             16
      FSCP_DISTFROMVTX=716.7                                              16
      CFSCP_DISTFROMVTX='DISTANCE FROM EVENT VERTEX (0,0) IN CM, SHOULD   16
     *EQUAL TO FMS'                                                       16
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSCP',    17
     *BPATH_FSCP,IDNUM_FSCP,1, MAP_FSCP,NAMES_FSCP,COMMEN_FSCP,           17
     *BANK_FSCP,LEN_FSCP,LLEN_FSCP,LINK_FSCP,FLAG_FSCP)                   17
C *                                                                       17
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSCP',    17
     *BPATH_FSCP,IDNUM_FSCP,1, MAP_FSCP,NAMES_FSCP,COMMEN_FSCP,           17
     *BANK_FSCP,LEN_FSCP,LLEN_FSCP,LINK_FSCP,FLAG_FSCP)                   17
C use FSCP bank                                                           18
      BPATH_FSCP='FSCP*'                                                  18
      IDNUM_FSCP(1)=0                                                     18
      FSCP_VERSION=1                                                      18
      CALL AGDGETP('FSCEGEO','*',-999,'FSCP',NFSCP_VERSION,               18
     *RE_FSCP_VERSION,BPATH_FSCP,IDNUM_FSCP,1,MAP_FSCP,NAMES_FSCP,        18
     *BANK_FSCP,LEN_FSCP,LLEN_FSCP,LINK_FSCP,FLAG_FSCP)                   18
      CALL AGSBEGM(MODULE,IPRIN)                                          19
C Check IPRIN<0                                                           19
      IF (IPRIN.LT.0) RETURN                                              19
C Create FSCE :                                                           19
C Call To Local Procedure FSCE                                            19
      ASSIGN 5020 TO BLOCK_FSCE                                           19
      GO TO  5010                                                         19
5020  CONTINUE                                                            19
C *                                                                       19
C Geant POSITION : FSCE ===> Z=FSCP_DISTFROMVTX+FSCP_TOWERLENGTH <===     20
      CALL AGSRESET                                                       20
      AG_Z = FSCP_DISTFROMVTX+FSCP_TOWERLENGTH                            20
      AG_TITLE = 'FSCE'                                                   20
      AG_EXNAME = 'POSITION'                                              20
      AG_PARLIST = 'Z'                                                    20
      CALL AXPOSITION                                                     20
C *                                                                       20
C ----------------------------------------------------------------------  20
C ------------ FSCE                                                       20
      CALL AGSENDM                                                        22
      RETURN                                                              22
C Begin Local Procedure FSCE                                              22
C FSCE IS THE CONTAINER VOLUME FOR ALL TOWERS                             22
5010  CONTINUE                                                            22
         AG_TITLE='FSCE'                                                  22
         AG_MODULE=MODULE                                                 22
         AG_MARK='V00'                                                    22
         CALL AGSPUSH                                                     22
         AG_MODULE=MODULE                                                 22
C    *                                                                    22
C    Geant MATERIAL : AIR ===> inherited arguments used <===              23
         CALL AGSRESET                                                    23
         AG_TITLE = 'AIR'                                                 23
         AG_EXNAME = 'MATERIAL'                                           23
         AG_PARLIST = 'NONE'                                              23
         CALL AXMATERIAL                                                  23
C    *                                                                    23
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           23
         CALL AGSRESET                                                    24
         AG_TITLE = 'STANDARD'                                            24
         AG_EXNAME = 'MEDIUM'                                             24
         AG_PARLIST = 'NONE'                                              24
         CALL AXMEDIUM                                                    24
C    *                                                                    24
C    Geant ATTRIBUTE : FSCE ===> SEEN=0 COLO=7 <===                       24
         CALL AGSRESET                                                    25
         AG_SEEN = 0                                                      25
         AG_COLO = 7                                                      25
         AG_TITLE = 'FSCE'                                                25
         AG_EXNAME = 'ATTRIBUTE'                                          25
         AG_PARLIST = 'SEEN_COLO'                                         25
         CALL AXATTRIBUTE                                                 25
C    *                                                                    25
C    Geant SHAPE : BOX ===> DX=FSCP_TOWERWIDTH*FSCP_NTOWERSX, DY=FSCP_TO  25
C    WERWIDTH*FSCP_NTOWERSY DZ=FSCP_TOWERLENGTH <===                      25
         CALL AGSRESET                                                    27
         AG_DX = FSCP_TOWERWIDTH*FSCP_NTOWERSX                            27
         AG_DY = FSCP_TOWERWIDTH*FSCP_NTOWERSY                            27
         AG_DZ = FSCP_TOWERLENGTH                                         27
         AG_TITLE = 'BOX'                                                 27
         AG_EXNAME = 'SHAPE'                                              27
         AG_PARLIST = 'DX_DY_DZ'                                          27
         CALL AXSHAPE                                                     27
C    *                                                                    27
C    Create FSCT :                                                        28
C    Call To Local Procedure FSCT                                         28
         ASSIGN 5040 TO BLOCK_FSCT                                        28
         GO TO  5030                                                      28
5040     CONTINUE                                                         28
C    *                                                                    28
         XPOSMIN = FSCP_NTOWERSX - FSCP_NEMPTY                            29
         XPOSMAX = FSCP_NTOWERSX + FSCP_NEMPTY + 1                        30
         YPOSMIN = FSCP_NTOWERSY - FSCP_NEMPTY                            31
         YPOSMAX = FSCP_NTOWERSY + FSCP_NEMPTY + 1                        32
C    *                                                                    33
         XDIM=1                                                           33
            GO TO 5053                                                    33
5051        XDIM=XDIM+1                                                   33
5053        IF(XDIM-(FSCP_NTOWERSX*2).GT.0) GO TO 5052                    33
C       *                                                                 34
            YDIM=1                                                        34
               GO TO 5063                                                 34
5061           YDIM=YDIM+1                                                34
5063           IF(YDIM-(FSCP_NTOWERSY*2).GT.0) GO TO 5062                 34
C          Check XDIM>XPOSMIN.AND.XDIM<XPOSMAX.AND.YDIM>YPOSMIN.AND.YDIM  35
C          <YPOSMAX                                                       35
               IF                                                         35
     *         (XDIM.GT.XPOSMIN.AND.XDIM.LT.XPOSMAX.AND.YDIM.GT.YPOSMIN.  35
     *         AND.YDIM.LT.YPOSMAX) THEN                                  35
               ELSE                                                       36
C          Geant POSITION : FSCT ===> X=-FSCP_TOWERWIDTH*2*FSCP_NTOWERSX  39
C          -FSCP_TOWERWIDTH+XDIM*FSCP_TOWERWIDTH*2 Y=-FSCP_TOWERWIDTH*2*  39
C          FSCP_NTOWERSY-FSCP_TOWERWIDTH+YDIM*FSCP_TOWERWIDTH*2 Z=0 <===  39
               CALL AGSRESET                                              39
               AG_X =                                                     39
     *         -FSCP_TOWERWIDTH*2*FSCP_NTOWERSX-FSCP_TOWERWIDTH+XDIM*     39
     *         FSCP_TOWERWIDTH*2                                          39
               AG_Y =                                                     39
     *         -FSCP_TOWERWIDTH*2*FSCP_NTOWERSY-FSCP_TOWERWIDTH+YDIM*     39
     *         FSCP_TOWERWIDTH*2                                          39
               AG_Z = 0                                                   39
               AG_TITLE = 'FSCT'                                          39
               AG_EXNAME = 'POSITION'                                     39
               AG_PARLIST = 'X_Y_Z'                                       39
               CALL AXPOSITION                                            39
C          *                                                              39
               ENDIF                                                      39
            GO TO 5061                                                    41
5062        CONTINUE                                                      41
         GO TO 5051                                                       42
5052     CONTINUE                                                         42
5011  CALL AGSPOPB                                                        43
      GO TO BLOCK_FSCE, (5010 ,5020)                                      43
C End of Local Procedure                                                  43
C ----------------------------------------------------------------------  44
C ------------ FSCT                                                       44
C Begin Local Procedure FSCT                                              45
C FSCT IS A SENSITIVE TUNGSTEN+SCI+EPOXY TOWER                            45
5030  CONTINUE                                                            45
         AG_TITLE='FSCT'                                                  45
         AG_MODULE=MODULE                                                 45
         AG_MARK='V00'                                                    45
         CALL AGSPUSH                                                     45
         AG_MODULE=MODULE                                                 45
C    *                                                                    45
C    Geant COMPONENT : W ===> A=183.84 Z=74 W=0.923 <===                  46
         CALL AGSRESET                                                    46
         AG_A = 183.84                                                    46
         AG_Z = 74                                                        46
         AG_W = 0.923                                                     46
         AG_TITLE = 'W'                                                   46
         AG_EXNAME = 'COMPONENT'                                          46
         AG_PARLIST = 'A_Z_W'                                             46
         CALL AXCOMPONENT                                                 46
C    *                                                                    46
C    Geant COMPONENT : C ===> A=12.01 Z=6 W=0.071 <===                    46
         CALL AGSRESET                                                    47
         AG_A = 12.01                                                     47
         AG_Z = 6                                                         47
         AG_W = 0.071                                                     47
         AG_TITLE = 'C'                                                   47
         AG_EXNAME = 'COMPONENT'                                          47
         AG_PARLIST = 'A_Z_W'                                             47
         CALL AXCOMPONENT                                                 47
C    *                                                                    47
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.006 <===                        47
         CALL AGSRESET                                                    48
         AG_A = 1                                                         48
         AG_Z = 1                                                         48
         AG_W = 0.006                                                     48
         AG_TITLE = 'H'                                                   48
         AG_EXNAME = 'COMPONENT'                                          48
         AG_PARLIST = 'A_Z_W'                                             48
         CALL AXCOMPONENT                                                 48
C    *                                                                    48
C    Geant MIXTURE : WSE ===> DENS=8.297 <===                             48
         CALL AGSRESET                                                    49
         AG_DENS = 8.297                                                  49
         AG_TITLE = 'WSE'                                                 49
         AG_EXNAME = 'MIXTURE'                                            49
         AG_PARLIST = 'DENS'                                              49
         CALL AXMIXTURE                                                   49
C    *                                                                    49
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           49
         CALL AGSRESET                                                    50
         AG_ISVOL = 1                                                     50
         AG_TITLE = 'SENSITIVE'                                           50
         AG_EXNAME = 'MEDIUM'                                             50
         AG_PARLIST = 'ISVOL'                                             50
         CALL AXMEDIUM                                                    50
C    *                                                                    50
C    Geant ATTRIBUTE : FSCT ===> SEEN=1 COLO=7 <===                       50
         CALL AGSRESET                                                    51
         AG_SEEN = 1                                                      51
         AG_COLO = 7                                                      51
         AG_TITLE = 'FSCT'                                                51
         AG_EXNAME = 'ATTRIBUTE'                                          51
         AG_PARLIST = 'SEEN_COLO'                                         51
         CALL AXATTRIBUTE                                                 51
C    *                                                                    51
C    Geant SHAPE : BOX ===> DX=FSCP_TOWERWIDTH DY=FSCP_TOWERWIDTH, DZ=FS  51
C    CP_TOWERLENGTH <===                                                  51
         CALL AGSRESET                                                    53
         AG_DX = FSCP_TOWERWIDTH                                          53
         AG_DY = FSCP_TOWERWIDTH                                          53
         AG_DZ = FSCP_TOWERLENGTH                                         53
         AG_TITLE = 'BOX'                                                 53
         AG_EXNAME = 'SHAPE'                                              53
         AG_PARLIST = 'DX_DY_DZ'                                          53
         CALL AXSHAPE                                                     53
C    *                                                                    53
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.0005)                            53
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00015)                           55
C    Defining Hits: ELOSS:0:(0,250)                                       56
         AG_TITLE = 'FSCT'                                                56
         AG_EXNAME = 'HITS'                                               56
         AG_TYPE=AG_MAGIC                                                 56
         CALL AGSDETV('FSCT')                                             56
C    *                                                                    56
         AG_PARLIST='ELOSS'                                               56
         AG_TITLE = 'DETH'                                                56
         AG_NBITS = 0                                                     56
         AG_BIN=AG_MAGIC                                                  56
         AG_OPTION='*'                                                    56
         AG_FHMIN = 0                                                     56
         AG_FHMAX = 250                                                   56
         CALL AGSDETH (0)                                                 56
C    *                                                                    56
         CALL AGSDETU('FSCT')                                             56
C    *                                                                    56
5031  CALL AGSPOPB                                                        57
      GO TO BLOCK_FSCT, (5030 ,5040)                                      57
C End of Local Procedure                                                  57
      WRITE(LOUT,5070)'===>>  [End AgML Module FSCEGEO]  <<==='           57
5070  FORMAT(A32,/,/)                                                     57
C MODULE FSCEGEO                                                          59
      END                                                                 59
