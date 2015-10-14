C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FHCMGEO                                                  1
C FHCMGEO IS THE FORWARD HADRON DETECTOR MODULES GEOMETRY                 1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FHCMGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE FORWARD HADRON DETECTOR MODULES GEOMETRY'/    1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 17 APR 2009                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'17 APR 2009'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR QINGHUA XU...                                                    2
      CHARACTER*40 AUTHOR /'QINGHUA XU...'/                               3
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
      INTEGER BLOCK_FHCM                                                  6
      INTEGER BLOCK_FHCC                                                  6
      INTEGER BLOCK_FHCS                                                  6
C -                                                                       8
      REAL FHCG_VERSION                                                   8
      EQUIVALENCE (BANK_FHCG(3),FHCG_VERSION,RE_FHCG_VERSION),            8
     *(NAMES_FHCG(1),NFHCG_VERSION)                                       8
      EQUIVALENCE (COMMEN_FHCG(1),CFHCG_VERSION),(MAP_FHCG(1,1),          8
     *MFHCG_VERSION)                                                      8
      CHARACTER CFHCG_VERSION*40/' '/                                     8
      CHARACTER NFHCG_VERSION*12/'R.VERSION'/                             8
      INTEGER MFHCG_VERSION(2)/1,1/                                       8
      REAL RE_FHCG_VERSION                                                8
C -                                                                       8
      REAL FHCG_ZOFFSET                                                   8
      EQUIVALENCE (BANK_FHCG(4),FHCG_ZOFFSET,RE_FHCG_ZOFFSET),            8
     *(NAMES_FHCG(2),NFHCG_ZOFFSET)                                       8
      EQUIVALENCE (COMMEN_FHCG(2),CFHCG_ZOFFSET),(MAP_FHCG(1,2),          8
     *MFHCG_ZOFFSET)                                                      8
      CHARACTER CFHCG_ZOFFSET*40/' '/                                     8
      CHARACTER NFHCG_ZOFFSET*12/'R.ZOFFSET'/                             8
      INTEGER MFHCG_ZOFFSET(2)/1,1/                                       8
      REAL RE_FHCG_ZOFFSET                                                8
C -                                                                       8
      REAL FHCG_XOFFSET                                                   8
      EQUIVALENCE (BANK_FHCG(5),FHCG_XOFFSET,RE_FHCG_XOFFSET),            8
     *(NAMES_FHCG(3),NFHCG_XOFFSET)                                       8
      EQUIVALENCE (COMMEN_FHCG(3),CFHCG_XOFFSET),(MAP_FHCG(1,3),          8
     *MFHCG_XOFFSET)                                                      8
      CHARACTER CFHCG_XOFFSET*40/' '/                                     8
      CHARACTER NFHCG_XOFFSET*12/'R.XOFFSET'/                             8
      INTEGER MFHCG_XOFFSET(2)/1,1/                                       8
      REAL RE_FHCG_XOFFSET                                                8
C -                                                                       8
      REAL FHCG_YOFFSET                                                   8
      EQUIVALENCE (BANK_FHCG(6),FHCG_YOFFSET,RE_FHCG_YOFFSET),            8
     *(NAMES_FHCG(4),NFHCG_YOFFSET)                                       8
      EQUIVALENCE (COMMEN_FHCG(4),CFHCG_YOFFSET),(MAP_FHCG(1,4),          8
     *MFHCG_YOFFSET)                                                      8
      CHARACTER CFHCG_YOFFSET*40/' '/                                     8
      CHARACTER NFHCG_YOFFSET*12/'R.YOFFSET'/                             8
      INTEGER MFHCG_YOFFSET(2)/1,1/                                       8
      REAL RE_FHCG_YOFFSET                                                8
C -                                                                       8
      REAL FHCG_NXCELL                                                    8
      EQUIVALENCE (BANK_FHCG(7),FHCG_NXCELL,RE_FHCG_NXCELL),              8
     *(NAMES_FHCG(5),NFHCG_NXCELL)                                        8
      EQUIVALENCE (COMMEN_FHCG(5),CFHCG_NXCELL),(MAP_FHCG(1,5),           8
     *MFHCG_NXCELL)                                                       8
      CHARACTER CFHCG_NXCELL*40/' '/                                      8
      CHARACTER NFHCG_NXCELL*12/'R.NXCELL'/                               8
      INTEGER MFHCG_NXCELL(2)/1,1/                                        8
      REAL RE_FHCG_NXCELL                                                 8
C -                                                                       8
      REAL FHCG_NYCELL                                                    8
      EQUIVALENCE (BANK_FHCG(8),FHCG_NYCELL,RE_FHCG_NYCELL),              8
     *(NAMES_FHCG(6),NFHCG_NYCELL)                                        8
      EQUIVALENCE (COMMEN_FHCG(6),CFHCG_NYCELL),(MAP_FHCG(1,6),           8
     *MFHCG_NYCELL)                                                       8
      CHARACTER CFHCG_NYCELL*40/' '/                                      8
      CHARACTER NFHCG_NYCELL*12/'R.NYCELL'/                               8
      INTEGER MFHCG_NYCELL(2)/1,1/                                        8
      REAL RE_FHCG_NYCELL                                                 8
C -                                                                       8
      REAL FHCG_NZSUB                                                     8
      EQUIVALENCE (BANK_FHCG(9),FHCG_NZSUB,RE_FHCG_NZSUB),                8
     *(NAMES_FHCG(7),NFHCG_NZSUB)                                         8
      EQUIVALENCE (COMMEN_FHCG(7),CFHCG_NZSUB),(MAP_FHCG(1,7),            8
     *MFHCG_NZSUB)                                                        8
      CHARACTER CFHCG_NZSUB*40/' '/                                       8
      CHARACTER NFHCG_NZSUB*12/'R.NZSUB'/                                 8
      INTEGER MFHCG_NZSUB(2)/1,1/                                         8
      REAL RE_FHCG_NZSUB                                                  8
C -                                                                       8
      REAL FHCG_XWID                                                      8
      EQUIVALENCE (BANK_FHCG(10),FHCG_XWID,RE_FHCG_XWID),(NAMES_FHCG(8),  8
     *NFHCG_XWID)                                                         8
      EQUIVALENCE (COMMEN_FHCG(8),CFHCG_XWID),(MAP_FHCG(1,8),             8
     *MFHCG_XWID)                                                         8
      CHARACTER CFHCG_XWID*40/' '/                                        8
      CHARACTER NFHCG_XWID*12/'R.XWID'/                                   8
      INTEGER MFHCG_XWID(2)/1,1/                                          8
      REAL RE_FHCG_XWID                                                   8
C -                                                                       8
      REAL FHCG_YWID                                                      8
      EQUIVALENCE (BANK_FHCG(11),FHCG_YWID,RE_FHCG_YWID),(NAMES_FHCG(9),  8
     *NFHCG_YWID)                                                         8
      EQUIVALENCE (COMMEN_FHCG(9),CFHCG_YWID),(MAP_FHCG(1,9),             8
     *MFHCG_YWID)                                                         8
      CHARACTER CFHCG_YWID*40/' '/                                        8
      CHARACTER NFHCG_YWID*12/'R.YWID'/                                   8
      INTEGER MFHCG_YWID(2)/1,1/                                          8
      REAL RE_FHCG_YWID                                                   8
C -                                                                       8
      REAL FHCG_ZWID                                                      8
      EQUIVALENCE (BANK_FHCG(12),FHCG_ZWID,RE_FHCG_ZWID),                 8
     *(NAMES_FHCG(10),NFHCG_ZWID)                                         8
      EQUIVALENCE (COMMEN_FHCG(10),CFHCG_ZWID),(MAP_FHCG(1,10),           8
     *MFHCG_ZWID)                                                         8
      CHARACTER CFHCG_ZWID*40/' '/                                        8
      CHARACTER NFHCG_ZWID*12/'R.ZWID'/                                   8
      INTEGER MFHCG_ZWID(2)/1,1/                                          8
      REAL RE_FHCG_ZWID                                                   8
C -                                                                       8
      INTEGER LEN_FHCG,LLEN_FHCG,LINK_FHCG                                8
      PARAMETER (LEN_FHCG=12)                                             8
      PARAMETER (LLEN_FHCG=10)                                            8
      INTEGER FLAG_FHCG/0/,MAP_FHCG(2,LLEN_FHCG),IDNUM_FHCG(10)           8
      CHARACTER NAMES_FHCG(LLEN_FHCG)*12,COMMEN_FHCG(LLEN_FHCG)*40        8
      DATA IDNUM_FHCG/10*0/                                               8
      REAL BANK_FHCG(LEN_FHCG)                                            8
      CHARACTER*60 BPATH_FHCG/' '/                                        8
      SAVE FLAG_FHCG,MAP_FHCG,IDNUM_FHCG,NAMES_FHCG,COMMEN_FHCG,          8
     *BPATH_FHCG                                                          8
      DATA LINK_FHCG/0/,BANK_FHCG/LEN_FHCG*0/                             8
C *                                                                       8
      INTEGER I,J,K,SN                                                    10
      REAL XX,YY,ZZ                                                       11
C *                                                                       13
      CALL AGSBEGM(MODULE,IPRIN)                                          13
      IF (IPRIN.LT.0) RETURN                                              13
C Filling bank FHCG - FORWARD HADRONIC CALORIMETER BASIC DATA             13
      BANK_TITLE='FORWARD HADRONIC CALORIMETER BASIC DATA'                13
      BPATH_FHCG='FHCG*'                                                  13
      IDNUM_FHCG(1)=0                                                     14
C GEOMETRY VERSION                                                        14
      FHCG_VERSION=1.0                                                    14
      CFHCG_VERSION='GEOMETRY VERSION'                                    14
C OFFSETS IN Z FROM IP TO CENTER OF FHC                                   15
      FHCG_ZOFFSET=900                                                    15
      CFHCG_ZOFFSET='OFFSETS IN Z FROM IP TO CENTER OF FHC'               15
C OFFSETS IN X FROM BEAMLINE TO CENTER OF FHC                             16
      FHCG_XOFFSET=100                                                    16
      CFHCG_XOFFSET='OFFSETS IN X FROM BEAMLINE TO CENTER OF FHC'         16
C OFFSETS IN Y FROM BEAMLINE TO CENTER OF FHC                             17
      FHCG_YOFFSET=0                                                      17
      CFHCG_YOFFSET='OFFSETS IN Y FROM BEAMLINE TO CENTER OF FHC'         17
C NUMBER OF CELLS IN X                                                    18
      FHCG_NXCELL=9                                                       18
      CFHCG_NXCELL='NUMBER OF CELLS IN X'                                 18
C NUMBER OF CELLS IN Y                                                    19
      FHCG_NYCELL=12                                                      19
      CFHCG_NYCELL='NUMBER OF CELLS IN Y'                                 19
C NUMBER OF SUB-CELLS IN Z                                                20
      FHCG_NZSUB=5                                                        20
      CFHCG_NZSUB='NUMBER OF SUB-CELLS IN Z'                              20
C CELL X WIDTH                                                            21
      FHCG_XWID=10                                                        21
      CFHCG_XWID='CELL X WIDTH'                                           21
C CELL Y WIDTH                                                            22
      FHCG_YWID=10                                                        22
      CFHCG_YWID='CELL Y WIDTH'                                           22
C CELL Z LENGTH                                                           23
      FHCG_ZWID=117                                                       23
      CFHCG_ZWID='CELL Z LENGTH'                                          23
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FHCG',    24
     *BPATH_FHCG,IDNUM_FHCG,1, MAP_FHCG,NAMES_FHCG,COMMEN_FHCG,           24
     *BANK_FHCG,LEN_FHCG,LLEN_FHCG,LINK_FHCG,FLAG_FHCG)                   24
C *                                                                       24
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FHCG',    24
     *BPATH_FHCG,IDNUM_FHCG,1, MAP_FHCG,NAMES_FHCG,COMMEN_FHCG,           24
     *BANK_FHCG,LEN_FHCG,LLEN_FHCG,LINK_FHCG,FLAG_FHCG)                   24
C Check IPRIN>=1                                                          25
      IF (IPRIN.GE.1) WRITE(LOUT,5010)FHCG_VERSION                        25
5010  FORMAT(' FHCMGEO Version ', F4.2)                                   26
* FHC Module
      CALL AGSBEGM(MODULE,IPRIN)                                          29
C Check IPRIN<0                                                           29
      IF (IPRIN.LT.0) RETURN                                              29
C Create FHCM :                                                           29
C Call To Local Procedure FHCM                                            29
      ASSIGN 5030 TO BLOCK_FHCM                                           29
      GO TO  5020                                                         29
5030  CONTINUE                                                            29
C *                                                                       29
      SN=1                                                                30
C Geant POSITION : FHCM ===> MOTHER='CAVE' Z=FHCG_ZOFFSET X=FHCG_XOFFSET  31
C  Y=FHCG_YOFFSET KONLY='MANY' <===                                       31
      CALL AGSRESET                                                       31
      AG_MOTHER = 'CAVE'                                                  31
      AG_Z = FHCG_ZOFFSET                                                 31
      AG_X = FHCG_XOFFSET                                                 31
      AG_Y = FHCG_YOFFSET                                                 31
      AG_KONLY = 'MANY'                                                   31
      AG_TITLE = 'FHCM'                                                   31
      AG_EXNAME = 'POSITION'                                              31
      AG_PARLIST = 'MOTHER_Z_X_Y_KONLY'                                   31
      CALL AXPOSITION                                                     31
C *                                                                       31
      SN=2                                                                31
C Geant POSITION : FHCM ===> MOTHER='CAVE' Z=FHCG_ZOFFSET X=-FHCG_XOFFSE  33
C T Y=FHCG_YOFFSET KONLY='MANY' <===                                      33
      CALL AGSRESET                                                       33
      AG_MOTHER = 'CAVE'                                                  33
      AG_Z = FHCG_ZOFFSET                                                 33
      AG_X = -FHCG_XOFFSET                                                33
      AG_Y = FHCG_YOFFSET                                                 33
      AG_KONLY = 'MANY'                                                   33
      AG_TITLE = 'FHCM'                                                   33
      AG_EXNAME = 'POSITION'                                              33
      AG_PARLIST = 'MOTHER_Z_X_Y_KONLY'                                   33
      CALL AXPOSITION                                                     33
C *                                                                       33
C Check IPRIN>=1                                                          33
      IF (IPRIN.GE.1) WRITE(LOUT,5040)                                    33
5040  FORMAT('FHCMGEO done')                                              34
* ----------------------------------------------------------------------------
      CALL AGSENDM                                                        37
      RETURN                                                              37
C Begin Local Procedure FHCM                                              37
C FHCM IS ONE FHC MODULE                                                  37
5020  CONTINUE                                                            37
         AG_TITLE='FHCM'                                                  37
         AG_MODULE=MODULE                                                 37
         AG_MARK='V00'                                                    37
         CALL AGSPUSH                                                     37
         AG_MODULE=MODULE                                                 37
C    *                                                                    37
C    Geant MATERIAL : AIR ===> inherited arguments used <===              38
         CALL AGSRESET                                                    38
         AG_TITLE = 'AIR'                                                 38
         AG_EXNAME = 'MATERIAL'                                           38
         AG_PARLIST = 'NONE'                                              38
         CALL AXMATERIAL                                                  38
C    *                                                                    38
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           38
         CALL AGSRESET                                                    39
         AG_TITLE = 'STANDARD'                                            39
         AG_EXNAME = 'MEDIUM'                                             39
         AG_PARLIST = 'NONE'                                              39
         CALL AXMEDIUM                                                    39
C    *                                                                    39
C    Geant ATTRIBUTE : FHCM ===> SEEN=1 COLO=2 <===                       39
         CALL AGSRESET                                                    40
         AG_SEEN = 1                                                      40
         AG_COLO = 2                                                      40
         AG_TITLE = 'FHCM'                                                40
         AG_EXNAME = 'ATTRIBUTE'                                          40
         AG_PARLIST = 'SEEN_COLO'                                         40
         CALL AXATTRIBUTE                                                 40
C    *                                                                    40
C    Geant SHAPE : BOX ===> DX=FHCG_XWID*FHCG_NXCELL/2.0 DY=FHCG_YWID*FH  40
C    CG_NYCELL/2.0 DZ=FHCG_ZWID/2.0 <===                                  40
         CALL AGSRESET                                                    41
         AG_DX = FHCG_XWID*FHCG_NXCELL/2.0                                41
         AG_DY = FHCG_YWID*FHCG_NYCELL/2.0                                41
         AG_DZ = FHCG_ZWID/2.0                                            41
         AG_TITLE = 'BOX'                                                 41
         AG_EXNAME = 'SHAPE'                                              41
         AG_PARLIST = 'DX_DY_DZ'                                          41
         CALL AXSHAPE                                                     41
C    *                                                                    41
C    Create FHCC :                                                        42
C    Call To Local Procedure FHCC                                         42
         ASSIGN 5060 TO BLOCK_FHCC                                        42
         GO TO  5050                                                      42
5060     CONTINUE                                                         42
C    *                                                                    42
C    *                                                                    43
         I=1                                                              43
            GO TO 5073                                                    43
5071        I=I+1                                                         43
5073        IF(I-(FHCG_NXCELL).GT.0) GO TO 5072                           43
C       *                                                                 44
            J=1                                                           44
               GO TO 5083                                                 44
5081           J=J+1                                                      44
5083           IF(J-(FHCG_NYCELL).GT.0) GO TO 5082                        44
               XX= (I-0.5)*FHCG_XWID - FHCG_NXCELL*FHCG_XWID/2.0          45
C          Check SN.EQ.2                                                  46
               IF (SN.EQ.2) XX = -XX                                      46
               YY= -(J-0.5)*FHCG_YWID + FHCG_NYCELL*FHCG_YWID/2.0         47
!//vp           write(*,*) i,j,xx,yy
C          Geant POSITION : FHCC ===> X=XX Y=YY <===                      49
               CALL AGSRESET                                              49
               AG_X = XX                                                  49
               AG_Y = YY                                                  49
               AG_TITLE = 'FHCC'                                          49
               AG_EXNAME = 'POSITION'                                     49
               AG_PARLIST = 'X_Y'                                         49
               CALL AXPOSITION                                            49
C          *                                                              49
            GO TO 5081                                                    50
5082        CONTINUE                                                      50
         GO TO 5071                                                       51
5072     CONTINUE                                                         51
5021  CALL AGSPOPB                                                        52
      GO TO BLOCK_FHCM, (5020 ,5030)                                      52
C End of Local Procedure                                                  52
C Begin Local Procedure FHCC                                              54
C FHCC IS ONE FHC CELL                                                    54
5050  CONTINUE                                                            54
         AG_TITLE='FHCC'                                                  54
         AG_MODULE=MODULE                                                 54
         AG_MARK='V00'                                                    54
         CALL AGSPUSH                                                     54
         AG_MODULE=MODULE                                                 54
C    *                                                                    54
C    Geant MATERIAL : AIR ===> inherited arguments used <===              55
         CALL AGSRESET                                                    55
         AG_TITLE = 'AIR'                                                 55
         AG_EXNAME = 'MATERIAL'                                           55
         AG_PARLIST = 'NONE'                                              55
         CALL AXMATERIAL                                                  55
C    *                                                                    55
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           55
         CALL AGSRESET                                                    56
         AG_TITLE = 'STANDARD'                                            56
         AG_EXNAME = 'MEDIUM'                                             56
         AG_PARLIST = 'NONE'                                              56
         CALL AXMEDIUM                                                    56
C    *                                                                    56
C    Geant ATTRIBUTE : FHCC ===> SEEN=1 COLO=3 <===                       56
         CALL AGSRESET                                                    57
         AG_SEEN = 1                                                      57
         AG_COLO = 3                                                      57
         AG_TITLE = 'FHCC'                                                57
         AG_EXNAME = 'ATTRIBUTE'                                          57
         AG_PARLIST = 'SEEN_COLO'                                         57
         CALL AXATTRIBUTE                                                 57
C    *                                                                    57
C    Geant SHAPE : BOX ===> DX=FHCG_XWID/2.0 DY=FHCG_YWID/2.0 DZ=FHCG_ZW  57
C    ID/2.0 <===                                                          57
         CALL AGSRESET                                                    58
         AG_DX = FHCG_XWID/2.0                                            58
         AG_DY = FHCG_YWID/2.0                                            58
         AG_DZ = FHCG_ZWID/2.0                                            58
         AG_TITLE = 'BOX'                                                 58
         AG_EXNAME = 'SHAPE'                                              58
         AG_PARLIST = 'DX_DY_DZ'                                          58
         CALL AXSHAPE                                                     58
C    *                                                                    58
C    Create FHCS :                                                        59
C    Call To Local Procedure FHCS                                         59
         ASSIGN 5100 TO BLOCK_FHCS                                        59
         GO TO  5090                                                      59
5100     CONTINUE                                                         59
C    *                                                                    59
C    *                                                                    60
         K=1                                                              60
            GO TO 5113                                                    60
5111        K=K+1                                                         60
5113        IF(K-(FHCG_NZSUB).GT.0) GO TO 5112                            60
            ZZ= (K - 0.5)*FHCG_ZWID/FHCG_NZSUB - FHCG_ZWID/2.0            61
*        zz= (k - FHCG_NZsub/2.0-0.5)*FHCG_ZWID/FHCG_NZsub
!//vp        write(*,*) zz
C       Geant POSITION : FHCS ===> Z=ZZ <===                              64
            CALL AGSRESET                                                 64
            AG_Z = ZZ                                                     64
            AG_TITLE = 'FHCS'                                             64
            AG_EXNAME = 'POSITION'                                        64
            AG_PARLIST = 'Z'                                              64
            CALL AXPOSITION                                               64
C       *                                                                 64
         GO TO 5111                                                       65
5112     CONTINUE                                                         65
5051  CALL AGSPOPB                                                        66
      GO TO BLOCK_FHCC, (5050 ,5060)                                      66
C End of Local Procedure                                                  66
C Begin Local Procedure FHCS                                              69
C FHCS IS ONE LONGITUDINAL SUB-DIVIDSION OF FHC CELL                      69
5090  CONTINUE                                                            69
         AG_TITLE='FHCS'                                                  69
         AG_MODULE=MODULE                                                 69
         AG_MARK='V00'                                                    69
         CALL AGSPUSH                                                     69
         AG_MODULE=MODULE                                                 69
C    *                                                                    69
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             70
         CALL AGSRESET                                                    70
         AG_TITLE = 'LEAD'                                                70
         AG_EXNAME = 'MATERIAL'                                           70
         AG_PARLIST = 'NONE'                                              70
         CALL AXMATERIAL                                                  70
C    *                                                                    70
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           70
         CALL AGSRESET                                                    71
         AG_TITLE = 'STANDARD'                                            71
         AG_EXNAME = 'MEDIUM'                                             71
         AG_PARLIST = 'NONE'                                              71
         CALL AXMEDIUM                                                    71
C    *                                                                    71
C    Geant ATTRIBUTE : FHCS ===> SEEN=1 COLO=4 <===                       71
         CALL AGSRESET                                                    72
         AG_SEEN = 1                                                      72
         AG_COLO = 4                                                      72
         AG_TITLE = 'FHCS'                                                72
         AG_EXNAME = 'ATTRIBUTE'                                          72
         AG_PARLIST = 'SEEN_COLO'                                         72
         CALL AXATTRIBUTE                                                 72
C    *                                                                    72
C    Geant SHAPE : BOX ===> DX=FHCG_XWID/2.0 DY=FHCG_YWID/2.0 DZ=FHCG_ZW  72
C    ID/FHCG_NZSUB/2.0 <===                                               72
         CALL AGSRESET                                                    73
         AG_DX = FHCG_XWID/2.0                                            73
         AG_DY = FHCG_YWID/2.0                                            73
         AG_DZ = FHCG_ZWID/FHCG_NZSUB/2.0                                 73
         AG_TITLE = 'BOX'                                                 73
         AG_EXNAME = 'SHAPE'                                              73
         AG_PARLIST = 'DX_DY_DZ'                                          73
         CALL AXSHAPE                                                     73
C    *                                                                    73
C    Defining Hits: ELOSS:0:(0,250)                                       75
         AG_TITLE = 'FHCS'                                                75
         AG_EXNAME = 'HITS'                                               75
         AG_TYPE=AG_MAGIC                                                 75
         CALL AGSDETV('FHCS')                                             75
C    *                                                                    75
         AG_PARLIST='ELOSS'                                               75
         AG_TITLE = 'DETH'                                                75
         AG_NBITS = 0                                                     75
         AG_BIN=AG_MAGIC                                                  75
         AG_OPTION='*'                                                    75
         AG_FHMIN = 0                                                     75
         AG_FHMAX = 250                                                   75
         CALL AGSDETH (0)                                                 75
C    *                                                                    75
         CALL AGSDETU('FHCS')                                             75
C    *                                                                    75
5091  CALL AGSPOPB                                                        76
      GO TO BLOCK_FHCS, (5090 ,5100)                                      76
C End of Local Procedure                                                  76
      END                                                                 78
