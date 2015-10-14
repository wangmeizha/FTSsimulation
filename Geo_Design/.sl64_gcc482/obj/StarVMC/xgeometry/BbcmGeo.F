C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BBCMGEO                                                  1
C BBCMGEO IS THE BEAM BEAM COUNTER MODULES GEOMETRY                       1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BBCMGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE BEAM BEAM COUNTER MODULES GEOMETRY'/          1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 15 MARCH 2002                                                   1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'15 MARCH 2002'/                              2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR YIQUN WANG                                                       2
      CHARACTER*40 AUTHOR /'YIQUN WANG'/                                  3
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
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   4
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        4
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       4
C - GEANT description of logical units                                    4
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            4
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      4
      COMMON/GCMAIL/ CHMAIL                                               4
      CHARACTER*132 CHMAIL                                                4
      INTEGER BLOCK_BBCM                                                  4
      INTEGER BLOCK_BBCA                                                  5
      INTEGER BLOCK_THXM                                                  5
      INTEGER BLOCK_SHXT                                                  5
      INTEGER BLOCK_BPOL                                                  5
      INTEGER BLOCK_CLAD                                                  5
C -                                                                       6
      REAL BBCG_VERSION                                                   6
      EQUIVALENCE (BANK_BBCG(3),BBCG_VERSION,RE_BBCG_VERSION),            6
     *(NAMES_BBCG(1),NBBCG_VERSION)                                       6
      EQUIVALENCE (COMMEN_BBCG(1),CBBCG_VERSION),(MAP_BBCG(1,1),          6
     *MBBCG_VERSION)                                                      6
      CHARACTER CBBCG_VERSION*40/' '/                                     6
      CHARACTER NBBCG_VERSION*12/'R.VERSION'/                             6
      INTEGER MBBCG_VERSION(2)/1,1/                                       6
      REAL RE_BBCG_VERSION                                                6
C -                                                                       6
      REAL BBCG_ONOFF(3)                                                  6
      EQUIVALENCE (BANK_BBCG(4),BBCG_ONOFF,RE_BBCG_ONOFF),                6
     *(NAMES_BBCG(2),NBBCG_ONOFF)                                         6
      EQUIVALENCE (COMMEN_BBCG(2),CBBCG_ONOFF),(MAP_BBCG(1,2),            6
     *MBBCG_ONOFF)                                                        6
      CHARACTER CBBCG_ONOFF*40/' '/                                       6
      CHARACTER NBBCG_ONOFF*12/'R.ONOFF'/                                 6
      INTEGER MBBCG_ONOFF(2)/3,1/                                         6
      REAL RE_BBCG_ONOFF                                                  6
C -                                                                       6
      REAL BBCG_ZDIS(2)                                                   6
      EQUIVALENCE (BANK_BBCG(7),BBCG_ZDIS,RE_BBCG_ZDIS),(NAMES_BBCG(3),   6
     *NBBCG_ZDIS)                                                         6
      EQUIVALENCE (COMMEN_BBCG(3),CBBCG_ZDIS),(MAP_BBCG(1,3),             6
     *MBBCG_ZDIS)                                                         6
      CHARACTER CBBCG_ZDIS*40/' '/                                        6
      CHARACTER NBBCG_ZDIS*12/'R.ZDIS'/                                   6
      INTEGER MBBCG_ZDIS(2)/2,1/                                          6
      REAL RE_BBCG_ZDIS                                                   6
C -                                                                       6
      INTEGER LEN_BBCG,LLEN_BBCG,LINK_BBCG                                6
      PARAMETER (LEN_BBCG=8)                                              6
      PARAMETER (LLEN_BBCG=3)                                             6
      INTEGER FLAG_BBCG/0/,MAP_BBCG(2,LLEN_BBCG),IDNUM_BBCG(10)           6
      CHARACTER NAMES_BBCG(LLEN_BBCG)*12,COMMEN_BBCG(LLEN_BBCG)*40        6
      DATA IDNUM_BBCG/10*0/                                               6
      REAL BANK_BBCG(LEN_BBCG)                                            6
      CHARACTER*60 BPATH_BBCG/' '/                                        6
      SAVE FLAG_BBCG,MAP_BBCG,IDNUM_BBCG,NAMES_BBCG,COMMEN_BBCG,          6
     *BPATH_BBCG                                                          6
      DATA LINK_BBCG/0/,BANK_BBCG/LEN_BBCG*0/                             6
C *                                                                       6
C -                                                                       7
      REAL HEXG_TYPE                                                      7
      EQUIVALENCE (BANK_HEXG(3),HEXG_TYPE,RE_HEXG_TYPE),(NAMES_HEXG(1),   7
     *NHEXG_TYPE)                                                         7
      EQUIVALENCE (COMMEN_HEXG(1),CHEXG_TYPE),(MAP_HEXG(1,1),             7
     *MHEXG_TYPE)                                                         7
      CHARACTER CHEXG_TYPE*40/' '/                                        7
      CHARACTER NHEXG_TYPE*12/'R.TYPE'/                                   7
      INTEGER MHEXG_TYPE(2)/1,1/                                          7
      REAL RE_HEXG_TYPE                                                   7
C -                                                                       7
      REAL HEXG_IRAD                                                      7
      EQUIVALENCE (BANK_HEXG(4),HEXG_IRAD,RE_HEXG_IRAD),(NAMES_HEXG(2),   7
     *NHEXG_IRAD)                                                         7
      EQUIVALENCE (COMMEN_HEXG(2),CHEXG_IRAD),(MAP_HEXG(1,2),             7
     *MHEXG_IRAD)                                                         7
      CHARACTER CHEXG_IRAD*40/' '/                                        7
      CHARACTER NHEXG_IRAD*12/'R.IRAD'/                                   7
      INTEGER MHEXG_IRAD(2)/1,1/                                          7
      REAL RE_HEXG_IRAD                                                   7
C -                                                                       7
      REAL HEXG_CLAD                                                      7
      EQUIVALENCE (BANK_HEXG(5),HEXG_CLAD,RE_HEXG_CLAD),(NAMES_HEXG(3),   7
     *NHEXG_CLAD)                                                         7
      EQUIVALENCE (COMMEN_HEXG(3),CHEXG_CLAD),(MAP_HEXG(1,3),             7
     *MHEXG_CLAD)                                                         7
      CHARACTER CHEXG_CLAD*40/' '/                                        7
      CHARACTER NHEXG_CLAD*12/'R.CLAD'/                                   7
      INTEGER MHEXG_CLAD(2)/1,1/                                          7
      REAL RE_HEXG_CLAD                                                   7
C -                                                                       7
      REAL HEXG_THICK                                                     7
      EQUIVALENCE (BANK_HEXG(6),HEXG_THICK,RE_HEXG_THICK),                7
     *(NAMES_HEXG(4),NHEXG_THICK)                                         7
      EQUIVALENCE (COMMEN_HEXG(4),CHEXG_THICK),(MAP_HEXG(1,4),            7
     *MHEXG_THICK)                                                        7
      CHARACTER CHEXG_THICK*40/' '/                                       7
      CHARACTER NHEXG_THICK*12/'R.THICK'/                                 7
      INTEGER MHEXG_THICK(2)/1,1/                                         7
      REAL RE_HEXG_THICK                                                  7
C -                                                                       7
      REAL HEXG_ZOFFSET                                                   7
      EQUIVALENCE (BANK_HEXG(7),HEXG_ZOFFSET,RE_HEXG_ZOFFSET),            7
     *(NAMES_HEXG(5),NHEXG_ZOFFSET)                                       7
      EQUIVALENCE (COMMEN_HEXG(5),CHEXG_ZOFFSET),(MAP_HEXG(1,5),          7
     *MHEXG_ZOFFSET)                                                      7
      CHARACTER CHEXG_ZOFFSET*40/' '/                                     7
      CHARACTER NHEXG_ZOFFSET*12/'R.ZOFFSET'/                             7
      INTEGER MHEXG_ZOFFSET(2)/1,1/                                       7
      REAL RE_HEXG_ZOFFSET                                                7
C -                                                                       7
      REAL HEXG_XOFFSET                                                   7
      EQUIVALENCE (BANK_HEXG(8),HEXG_XOFFSET,RE_HEXG_XOFFSET),            7
     *(NAMES_HEXG(6),NHEXG_XOFFSET)                                       7
      EQUIVALENCE (COMMEN_HEXG(6),CHEXG_XOFFSET),(MAP_HEXG(1,6),          7
     *MHEXG_XOFFSET)                                                      7
      CHARACTER CHEXG_XOFFSET*40/' '/                                     7
      CHARACTER NHEXG_XOFFSET*12/'R.XOFFSET'/                             7
      INTEGER MHEXG_XOFFSET(2)/1,1/                                       7
      REAL RE_HEXG_XOFFSET                                                7
C -                                                                       7
      REAL HEXG_YOFFSET                                                   7
      EQUIVALENCE (BANK_HEXG(9),HEXG_YOFFSET,RE_HEXG_YOFFSET),            7
     *(NAMES_HEXG(7),NHEXG_YOFFSET)                                       7
      EQUIVALENCE (COMMEN_HEXG(7),CHEXG_YOFFSET),(MAP_HEXG(1,7),          7
     *MHEXG_YOFFSET)                                                      7
      CHARACTER CHEXG_YOFFSET*40/' '/                                     7
      CHARACTER NHEXG_YOFFSET*12/'R.YOFFSET'/                             7
      INTEGER MHEXG_YOFFSET(2)/1,1/                                       7
      REAL RE_HEXG_YOFFSET                                                7
C -                                                                       7
      INTEGER LEN_HEXG,LLEN_HEXG,LINK_HEXG                                7
      PARAMETER (LEN_HEXG=9)                                              7
      PARAMETER (LLEN_HEXG=7)                                             7
      INTEGER FLAG_HEXG/0/,MAP_HEXG(2,LLEN_HEXG),IDNUM_HEXG(10)           7
      CHARACTER NAMES_HEXG(LLEN_HEXG)*12,COMMEN_HEXG(LLEN_HEXG)*40        7
      DATA IDNUM_HEXG/10*0/                                               7
      REAL BANK_HEXG(LEN_HEXG)                                            7
      CHARACTER*60 BPATH_HEXG/' '/                                        7
      SAVE FLAG_HEXG,MAP_HEXG,IDNUM_HEXG,NAMES_HEXG,COMMEN_HEXG,          7
     *BPATH_HEXG                                                          7
      DATA LINK_HEXG/0/,BANK_HEXG/LEN_HEXG*0/                             7
C *                                                                       7
      REAL *4 ACTR,SRAD,LRAD,ZTOTAL,X0,Y0,THETA0,PHI0,XTRIP,YTRIP,RTRIP,  9
     *THETRIP,RSING, THESING                                              9
      INTEGER *4 I_TRIP/0/,J_SING/0/                                      11
C *                                                                       13
      CALL AGSBEGM(MODULE,IPRIN)                                          13
      IF (IPRIN.LT.0) RETURN                                              13
C Filling bank BBCG - BBC GEOMETRY                                        13
      BANK_TITLE='BBC GEOMETRY'                                           13
      BPATH_BBCG='BBCG*'                                                  13
      IDNUM_BBCG(1)=0                                                     14
C GEOMETRY VERSION                                                        14
      BBCG_VERSION=1.0                                                    14
      CBBCG_VERSION='GEOMETRY VERSION'                                    14
C 0 OFF, 1 WEST ON, 2 EAST ON, 3 BOTH ON: FOR BBC,SMALL TILES,LARGE TILE  15
C S                                                                       15
      BBCG_ONOFF(1)=3                                                     15
      BBCG_ONOFF(2)=3                                                     15
      BBCG_ONOFF(3)=3                                                     15
      CBBCG_ONOFF='0 OFF, 1 WEST ON, 2 EAST ON, 3 BOTH ON: FOR BBC,SMALL  15
     * TILES,LARGE TILES'                                                 15
C Z-COORD FROM CENTER IN STAR (715/2+6*2.54+1=373.8)                      16
      BBCG_ZDIS(1)=374.24                                                 16
      BBCG_ZDIS(2)=-374.24                                                16
      CBBCG_ZDIS='Z-COORD FROM CENTER IN STAR (715/2+6*2.54+1=373.8)'     16
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BBCG',    17
     *BPATH_BBCG,IDNUM_BBCG,1, MAP_BBCG,NAMES_BBCG,COMMEN_BBCG,           17
     *BANK_BBCG,LEN_BBCG,LLEN_BBCG,LINK_BBCG,FLAG_BBCG)                   17
C *                                                                       17
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BBCG',    17
     *BPATH_BBCG,IDNUM_BBCG,1, MAP_BBCG,NAMES_BBCG,COMMEN_BBCG,           17
     *BANK_BBCG,LEN_BBCG,LLEN_BBCG,LINK_BBCG,FLAG_BBCG)                   17
C *                                                                       18
      CALL AGSBEGM(MODULE,IPRIN)                                          18
      IF (IPRIN.LT.0) RETURN                                              18
C Filling bank HEXG - HEXAGON TILE GEOMETRY                               18
      BANK_TITLE='HEXAGON TILE GEOMETRY'                                  18
      BPATH_HEXG='HEXG*'                                                  18
      IDNUM_HEXG(1)=0                                                     19
C 1 FOR SMALL HEX TILE, 2 FOR LARGE TILE                                  19
      HEXG_TYPE=1                                                         19
      CHEXG_TYPE='1 FOR SMALL HEX TILE, 2 FOR LARGE TILE'                 19
C INSCRIBING CIRCLE RADIUS =9.64/2*SIN(60)=4.174                          20
      HEXG_IRAD=4.174                                                     20
      CHEXG_IRAD='INSCRIBING CIRCLE RADIUS =9.64/2*SIN(60)=4.174'         20
C CLADDING THICKNESS                                                      21
      HEXG_CLAD=0.1                                                       21
      CHEXG_CLAD='CLADDING THICKNESS'                                     21
C THICKNESS OF TILE                                                       22
      HEXG_THICK=1.0                                                      22
      CHEXG_THICK='THICKNESS OF TILE'                                     22
C Z-OFFSET FROM CENTER OF BBCW (1), OR BBCE (2)                           23
      HEXG_ZOFFSET=1.5                                                    23
      CHEXG_ZOFFSET='Z-OFFSET FROM CENTER OF BBCW (1), OR BBCE (2)'       23
C X-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)                     24
      HEXG_XOFFSET=0.0                                                    24
      CHEXG_XOFFSET='X-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)  24
     *'                                                                   24
C Y-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)                     25
      HEXG_YOFFSET=0.0                                                    25
      CHEXG_YOFFSET='Y-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)  25
     *'                                                                   25
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HEXG',    26
     *BPATH_HEXG,IDNUM_HEXG,1, MAP_HEXG,NAMES_HEXG,COMMEN_HEXG,           26
     *BANK_HEXG,LEN_HEXG,LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                   26
C *                                                                       26
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HEXG',    26
     *BPATH_HEXG,IDNUM_HEXG,1, MAP_HEXG,NAMES_HEXG,COMMEN_HEXG,           26
     *BANK_HEXG,LEN_HEXG,LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                   26
C *                                                                       27
      CALL AGSBEGM(MODULE,IPRIN)                                          27
      IF (IPRIN.LT.0) RETURN                                              27
C Filling bank HEXG - HEXAGON TILE GEOMETRY                               27
      BANK_TITLE='HEXAGON TILE GEOMETRY'                                  27
      BPATH_HEXG='HEXG*'                                                  27
      IDNUM_HEXG(1)=0                                                     28
C 1 FOR SMALL HEX TILE, 2 FOR LARGE TILE                                  28
      HEXG_TYPE=2                                                         28
      CHEXG_TYPE='1 FOR SMALL HEX TILE, 2 FOR LARGE TILE'                 28
C INSCRIBING CIRCLE RADIUS (4X THAT OF SMALL ONE)                         29
      HEXG_IRAD=16.697                                                    29
      CHEXG_IRAD='INSCRIBING CIRCLE RADIUS (4X THAT OF SMALL ONE)'        29
C CLADDING OF TILE                                                        30
      HEXG_CLAD=0.1                                                       30
      CHEXG_CLAD='CLADDING OF TILE'                                       30
C THICKNESS OF TILE                                                       31
      HEXG_THICK=1.0                                                      31
      CHEXG_THICK='THICKNESS OF TILE'                                     31
C Z-OFFSET FROM CENTER OF BBCW (1), OR BBCE (2)                           32
      HEXG_ZOFFSET=-1.5                                                   32
      CHEXG_ZOFFSET='Z-OFFSET FROM CENTER OF BBCW (1), OR BBCE (2)'       32
C X-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)                     33
      HEXG_XOFFSET=0.0                                                    33
      CHEXG_XOFFSET='X-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)  33
     *'                                                                   33
C Y-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)                     34
      HEXG_YOFFSET=0.0                                                    34
      CHEXG_YOFFSET='Y-OFFSET CENTER FROM BEAM FOR BBCW (1), OR BBCE (2)  34
     *'                                                                   34
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HEXG',    35
     *BPATH_HEXG,IDNUM_HEXG,1, MAP_HEXG,NAMES_HEXG,COMMEN_HEXG,           35
     *BANK_HEXG,LEN_HEXG,LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                   35
C *                                                                       35
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HEXG',    35
     *BPATH_HEXG,IDNUM_HEXG,1, MAP_HEXG,NAMES_HEXG,COMMEN_HEXG,           35
     *BANK_HEXG,LEN_HEXG,LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                   35
C use default BBCG bank                                                   36
      BPATH_BBCG='BBCG*'                                                  36
      IDNUM_BBCG(1)=0                                                     36
      CALL AGDGETP('BBCMGEO','*',-999,'BBCG',' ',BANK_BBCG, BPATH_BBCG,   36
     *IDNUM_BBCG,1,MAP_BBCG,NAMES_BBCG,BANK_BBCG,LEN_BBCG,LLEN_BBCG,      36
     *LINK_BBCG,FLAG_BBCG)                                                36
C Check IPRIN>=1                                                          36
      IF (IPRIN.GE.1) WRITE(LOUT,5010)BBCG_VERSION                        36
5010  FORMAT('BBCMGEO version ', F4.2)                                    38
C Geant COMPONENT : C5 ===> A=12 Z=6 W=5 *10000 <===                      39
      CALL AGSRESET                                                       39
      AG_A = 12                                                           39
      AG_Z = 6                                                            39
      AG_W = 5 *10000                                                     39
      AG_TITLE = 'C5'                                                     39
      AG_EXNAME = 'COMPONENT'                                             39
      AG_PARLIST = 'A_Z_W'                                                39
      CALL AXCOMPONENT                                                    39
C *                                                                       39
C Geant COMPONENT : H4 ===> A=1 Z=1 W=4 *10000 <===                       39
      CALL AGSRESET                                                       40
      AG_A = 1                                                            40
      AG_Z = 1                                                            40
      AG_W = 4 *10000                                                     40
      AG_TITLE = 'H4'                                                     40
      AG_EXNAME = 'COMPONENT'                                             40
      AG_PARLIST = 'A_Z_W'                                                40
      CALL AXCOMPONENT                                                    40
C *                                                                       40
C Geant COMPONENT : O2 ===> A=16 Z=8 W=2 *10000 <===                      40
      CALL AGSRESET                                                       41
      AG_A = 16                                                           41
      AG_Z = 8                                                            41
      AG_W = 2 *10000                                                     41
      AG_TITLE = 'O2'                                                     41
      AG_EXNAME = 'COMPONENT'                                             41
      AG_PARLIST = 'A_Z_W'                                                41
      CALL AXCOMPONENT                                                    41
C *                                                                       41
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.2302 *10000 <===                41
      CALL AGSRESET                                                       42
      AG_A = 27                                                           42
      AG_Z = 13                                                           42
      AG_W = 0.2302 *10000                                                42
      AG_TITLE = 'AL'                                                     42
      AG_EXNAME = 'COMPONENT'                                             42
      AG_PARLIST = 'A_Z_W'                                                42
      CALL AXCOMPONENT                                                    42
C *                                                                       42
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              42
      CALL AGSRESET                                                       43
      AG_DENS = 1.432                                                     43
      AG_TITLE = 'ALKAP'                                                  43
      AG_EXNAME = 'MIXTURE'                                               43
      AG_PARLIST = 'DENS'                                                 43
      CALL AXMIXTURE                                                      43
C *                                                                       43
C use HEXG bank                                                           43
      BPATH_HEXG='HEXG*'                                                  44
      IDNUM_HEXG(1)=0                                                     44
      HEXG_TYPE=1                                                         44
      CALL AGDGETP('BBCMGEO','*',-999,'HEXG',NHEXG_TYPE,RE_HEXG_TYPE,     44
     *BPATH_HEXG,IDNUM_HEXG,1,MAP_HEXG,NAMES_HEXG,BANK_HEXG,LEN_HEXG,     44
     *LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                                      44
      SRAD = HEXG_IRAD*6.0                                                44
      ZTOTAL = HEXG_THICK+2*ABS(HEXG_ZOFFSET)                             45
C use HEXG bank                                                           47
      BPATH_HEXG='HEXG*'                                                  47
      IDNUM_HEXG(1)=0                                                     47
      HEXG_TYPE=2                                                         47
      CALL AGDGETP('BBCMGEO','*',-999,'HEXG',NHEXG_TYPE,RE_HEXG_TYPE,     47
     *BPATH_HEXG,IDNUM_HEXG,1,MAP_HEXG,NAMES_HEXG,BANK_HEXG,LEN_HEXG,     47
     *LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                                      47
      LRAD = HEXG_IRAD*6.0                                                47
      ZTOTAL = ZTOTAL+HEXG_THICK+2*ABS(HEXG_ZOFFSET)                      48
      CALL AGSBEGM(MODULE,IPRIN)                                          50
C Check IPRIN<0                                                           50
      IF (IPRIN.LT.0) RETURN                                              50
C Create BBCM :                                                           50
C Call To Local Procedure BBCM                                            50
      ASSIGN 5030 TO BLOCK_BBCM                                           50
      GO TO  5020                                                         50
5030  CONTINUE                                                            50
C *                                                                       50
C Check BBCG_ONOFF(1)==1|BBCG_ONOFF(1)==3                                 51
      IF (BBCG_ONOFF(1).EQ.1.OR.BBCG_ONOFF(1).EQ.3) THEN                  51
C Geant POSITION : BBCM ===> MOTHER='CAVE' X=0 Y=0 Z=BBCG_ZDIS(1) KONLY=  52
C 'MANY' <===                                                             52
      CALL AGSRESET                                                       52
      AG_MOTHER = 'CAVE'                                                  52
      AG_X = 0                                                            52
      AG_Y = 0                                                            52
      AG_Z = BBCG_ZDIS(1)                                                 52
      AG_KONLY = 'MANY'                                                   52
      AG_TITLE = 'BBCM'                                                   52
      AG_EXNAME = 'POSITION'                                              52
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   52
      CALL AXPOSITION                                                     52
C *                                                                       52
      ENDIF                                                               52
C Check BBCG_ONOFF(1)==2|BBCG_ONOFF(1)==3                                 54
      IF (BBCG_ONOFF(1).EQ.2.OR.BBCG_ONOFF(1).EQ.3) THEN                  54
C Geant POSITION : BBCM ===> MOTHER='CAVE' X=0 Y=0 Z=BBCG_ZDIS(2) ALPHAY  55
C =180 <===                                                               55
      CALL AGSRESET                                                       55
      AG_MOTHER = 'CAVE'                                                  55
      AG_X = 0                                                            55
      AG_Y = 0                                                            55
      AG_Z = BBCG_ZDIS(2)                                                 55
      AG_ALPHAN=AG_ALPHAN+1                                               55
      AG_ALPHAK(AG_ALPHAN)=2                                              55
      AG_ALPHAL(AG_ALPHAN)=180                                            55
      AG_TITLE = 'BBCM'                                                   55
      AG_EXNAME = 'POSITION'                                              55
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                                  55
      CALL AXPOSITION                                                     55
C *                                                                       55
      ENDIF                                                               55
C Check IPRIN>=1                                                          56
      IF (IPRIN.GE.1) WRITE(LOUT,5040)                                    56
5040  FORMAT('BBCMGEO finished')                                          58
C ----------------------------------------------------------------------  59
C ------------ BBCM                                                       59
      CALL AGSENDM                                                        60
      RETURN                                                              60
C Begin Local Procedure BBCM                                              60
C BBCM IS ONE BBC EAST OR WEST MODULE                                     60
5020  CONTINUE                                                            60
         AG_TITLE='BBCM'                                                  60
         AG_MODULE=MODULE                                                 60
         AG_MARK='V00'                                                    60
         CALL AGSPUSH                                                     60
         AG_MODULE=MODULE                                                 60
C    *                                                                    60
C    Geant MATERIAL : AIR ===> inherited arguments used <===              61
         CALL AGSRESET                                                    61
         AG_TITLE = 'AIR'                                                 61
         AG_EXNAME = 'MATERIAL'                                           61
         AG_PARLIST = 'NONE'                                              61
         CALL AXMATERIAL                                                  61
C    *                                                                    61
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           61
         CALL AGSRESET                                                    62
         AG_TITLE = 'STANDARD'                                            62
         AG_EXNAME = 'MEDIUM'                                             62
         AG_PARLIST = 'NONE'                                              62
         CALL AXMEDIUM                                                    62
C    *                                                                    62
C    Geant ATTRIBUTE : BBCM ===> SEEN=0 COLO=7 <===                       62
         CALL AGSRESET                                                    63
         AG_SEEN = 0                                                      63
         AG_COLO = 7                                                      63
         AG_TITLE = 'BBCM'                                                63
         AG_EXNAME = 'ATTRIBUTE'                                          63
         AG_PARLIST = 'SEEN_COLO'                                         63
         CALL AXATTRIBUTE                                                 63
C    *                                                                    63
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=LRAD DZ=ZTOTAL/2 <===            63
         CALL AGSRESET                                                    64
         AG_RMIN = 0                                                      64
         AG_RMAX = LRAD                                                   64
         AG_DZ = ZTOTAL/2                                                 64
         AG_TITLE = 'TUBE'                                                64
         AG_EXNAME = 'SHAPE'                                              64
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      64
         CALL AXSHAPE                                                     64
C    *                                                                    64
C    use HEXG bank                                                        64
         BPATH_HEXG='HEXG*'                                               65
         IDNUM_HEXG(1)=0                                                  65
         HEXG_TYPE=1                                                      65
         CALL AGDGETP('BBCMGEO','*',-999,'HEXG',NHEXG_TYPE,RE_HEXG_TYPE,  65
     *   BPATH_HEXG,IDNUM_HEXG,1,MAP_HEXG,NAMES_HEXG,BANK_HEXG,LEN_HEXG,  65
     *   LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                                   65
C    Check BBCG_ONOFF(2)==1|BBCG_ONOFF(2)==3                              66
         IF (BBCG_ONOFF(2).EQ.1.OR.BBCG_ONOFF(2).EQ.3) THEN               66
C    Create BBCA :                                                        67
C    Call To Local Procedure BBCA                                         67
         ASSIGN 5060 TO BLOCK_BBCA                                        67
         GO TO  5050                                                      67
5060     CONTINUE                                                         67
C    *                                                                    67
C    Geant POSITION : BBCA ===> MOTHER='BBCM' X=HEXG_XOFFSET Y=HEXG_YOFF  68
C    SET Z=HEXG_ZOFFSET <===                                              68
         CALL AGSRESET                                                    68
         AG_MOTHER = 'BBCM'                                               68
         AG_X = HEXG_XOFFSET                                              68
         AG_Y = HEXG_YOFFSET                                              68
         AG_Z = HEXG_ZOFFSET                                              68
         AG_TITLE = 'BBCA'                                                68
         AG_EXNAME = 'POSITION'                                           68
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      68
         CALL AXPOSITION                                                  68
C    *                                                                    68
         ENDIF                                                            68
C    use HEXG bank                                                        70
         BPATH_HEXG='HEXG*'                                               70
         IDNUM_HEXG(1)=0                                                  70
         HEXG_TYPE=2                                                      70
         CALL AGDGETP('BBCMGEO','*',-999,'HEXG',NHEXG_TYPE,RE_HEXG_TYPE,  70
     *   BPATH_HEXG,IDNUM_HEXG,1,MAP_HEXG,NAMES_HEXG,BANK_HEXG,LEN_HEXG,  70
     *   LLEN_HEXG,LINK_HEXG,FLAG_HEXG)                                   70
C    Check BBCG_ONOFF(3)==1|BBCG_ONOFF(3)==3                              71
         IF (BBCG_ONOFF(3).EQ.1.OR.BBCG_ONOFF(3).EQ.3) THEN               71
C    Create BBCA :                                                        72
C    Call To Local Procedure BBCA                                         72
         ASSIGN 5070 TO BLOCK_BBCA                                        72
         GO TO  5050                                                      72
5070     CONTINUE                                                         72
C    *                                                                    72
C    Geant POSITION : BBCA ===> MOTHER='BBCM' X=HEXG_XOFFSET Y=HEXG_YOFF  73
C    SET Z=HEXG_ZOFFSET <===                                              73
         CALL AGSRESET                                                    73
         AG_MOTHER = 'BBCM'                                               73
         AG_X = HEXG_XOFFSET                                              73
         AG_Y = HEXG_YOFFSET                                              73
         AG_Z = HEXG_ZOFFSET                                              73
         AG_TITLE = 'BBCA'                                                73
         AG_EXNAME = 'POSITION'                                           73
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      73
         CALL AXPOSITION                                                  73
C    *                                                                    73
         ENDIF                                                            73
5021  CALL AGSPOPB                                                        75
      GO TO BLOCK_BBCM, (5020 ,5030)                                      75
C End of Local Procedure                                                  75
C ----------------------------------------------------------------------  76
C ------------ BBCA                                                       76
C Begin Local Procedure BBCA                                              77
C BBCA IS ONE BBC ANNULUS MODULE                                          77
5050  CONTINUE                                                            77
         AG_TITLE='BBCA'                                                  77
         AG_MODULE=MODULE                                                 77
         AG_MARK='V00'                                                    77
         CALL AGSPUSH                                                     77
         AG_MODULE=MODULE                                                 77
C    *                                                                    77
C    Geant MATERIAL : AIR ===> inherited arguments used <===              78
         CALL AGSRESET                                                    78
         AG_TITLE = 'AIR'                                                 78
         AG_EXNAME = 'MATERIAL'                                           78
         AG_PARLIST = 'NONE'                                              78
         CALL AXMATERIAL                                                  78
C    *                                                                    78
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           78
         CALL AGSRESET                                                    79
         AG_TITLE = 'STANDARD'                                            79
         AG_EXNAME = 'MEDIUM'                                             79
         AG_PARLIST = 'NONE'                                              79
         CALL AXMEDIUM                                                    79
C    *                                                                    79
C    Geant ATTRIBUTE : BBCA ===> SEEN=0 COLO=3 <===                       79
         CALL AGSRESET                                                    80
         AG_SEEN = 0                                                      80
         AG_COLO = 3                                                      80
         AG_TITLE = 'BBCA'                                                80
         AG_EXNAME = 'ATTRIBUTE'                                          80
         AG_PARLIST = 'SEEN_COLO'                                         80
         CALL AXATTRIBUTE                                                 80
C    *                                                                    80
C    Geant SHAPE : TUBE ===> RMIN=HEXG_IRAD RMAX=HEXG_IRAD*6.0, DZ=HEXG_  80
C    THICK/2 <===                                                         80
         CALL AGSRESET                                                    82
         AG_RMIN = HEXG_IRAD                                              82
         AG_RMAX = HEXG_IRAD*6.0                                          82
         AG_DZ = HEXG_THICK/2                                             82
         AG_TITLE = 'TUBE'                                                82
         AG_EXNAME = 'SHAPE'                                              82
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      82
         CALL AXSHAPE                                                     82
C    *                                                                    82
         X0=HEXG_IRAD*TAN(PI/6.0)                                         82
         Y0=HEXG_IRAD*3.0                                                 84
         RTRIP = SQRT(X0*X0 + Y0*Y0)                                      85
         THETA0 = ATAN(Y0/X0)                                             86
C    *                                                                    87
         I_TRIP=0                                                         87
            GO TO 5083                                                    87
5081        I_TRIP=I_TRIP+1                                               87
5083        IF(I_TRIP-(5).GT.0) GO TO 5082                                87
            PHI0 = I_TRIP*60                                              88
            THETRIP = THETA0+I_TRIP*PI/3.0                                89
            XTRIP = RTRIP*COS(THETRIP)                                    90
            YTRIP = RTRIP*SIN(THETRIP)                                    91
C       Create THXM :                                                     92
C       Call To Local Procedure THXM                                      92
            ASSIGN 5100 TO BLOCK_THXM                                     92
            GO TO  5090                                                   92
5100        CONTINUE                                                      92
C       *                                                                 92
C       Geant POSITION : THXM ===> MOTHER='BBCA' X=XTRIP Y=YTRIP Z=0 THE  94
C       TAX=90 PHIX=PHI0 PHIY=90+PHI0 THETAZ=0 PHIZ=0 KONLY='MANY' <===   94
            CALL AGSRESET                                                 94
            AG_MOTHER = 'BBCA'                                            94
            AG_X = XTRIP                                                  94
            AG_Y = YTRIP                                                  94
            AG_Z = 0                                                      94
            AG_THETAX = 90                                                94
            AG_PHIX = PHI0                                                94
            AG_PHIY = 90+PHI0                                             94
            AG_THETAZ = 0                                                 94
            AG_PHIZ = 0                                                   94
            AG_KONLY = 'MANY'                                             94
            AG_TITLE = 'THXM'                                             94
            AG_EXNAME = 'POSITION'                                        94
            AG_PARLIST = 'MOTHER_X_Y_Z_THETAX_PHIX_PHIY_THETAZ_PHIZ_KONL  94
     *Y'                                                                  94
            CALL AXPOSITION                                               94
C       *                                                                 94
         GO TO 5081                                                       95
5082     CONTINUE                                                         95
5051  CALL AGSPOPB                                                        96
      GO TO BLOCK_BBCA, (5050 ,5060,5070)                                 96
C End of Local Procedure                                                  96
C ----------------------------------------------------------------------  97
C ------------ THXM                                                       97
C Begin Local Procedure THXM                                              98
C THXM IS ON TRIPLE HEXAGONAL MODULE                                      98
5090  CONTINUE                                                            98
         AG_TITLE='THXM'                                                  98
         AG_MODULE=MODULE                                                 98
         AG_MARK='V00'                                                    98
         CALL AGSPUSH                                                     98
         AG_MODULE=MODULE                                                 98
C    *                                                                    98
C    Geant MATERIAL : AIR ===> inherited arguments used <===              99
         CALL AGSRESET                                                    99
         AG_TITLE = 'AIR'                                                 99
         AG_EXNAME = 'MATERIAL'                                           99
         AG_PARLIST = 'NONE'                                              99
         CALL AXMATERIAL                                                  99
C    *                                                                    99
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           99
         CALL AGSRESET                                                    100
         AG_TITLE = 'STANDARD'                                            100
         AG_EXNAME = 'MEDIUM'                                             100
         AG_PARLIST = 'NONE'                                              100
         CALL AXMEDIUM                                                    100
C    *                                                                    100
C    Geant ATTRIBUTE : THXM ===> SEEN=0 COLO=2 <===                       100
         CALL AGSRESET                                                    101
         AG_SEEN = 0                                                      101
         AG_COLO = 2                                                      101
         AG_TITLE = 'THXM'                                                101
         AG_EXNAME = 'ATTRIBUTE'                                          101
         AG_PARLIST = 'SEEN_COLO'                                         101
         CALL AXATTRIBUTE                                                 101
C    *                                                                    101
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=HEXG_IRAD*2.0/SIN(PI/3.0), DZ=H  101
C    EXG_THICK/2 <===                                                     101
         CALL AGSRESET                                                    103
         AG_RMIN = 0                                                      103
         AG_RMAX = HEXG_IRAD*2.0/SIN(PI/3.0)                              103
         AG_DZ = HEXG_THICK/2                                             103
         AG_TITLE = 'TUBE'                                                103
         AG_EXNAME = 'SHAPE'                                              103
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      103
         CALL AXSHAPE                                                     103
C    *                                                                    103
C    *                                                                    103
         J_SING=0                                                         104
            GO TO 5113                                                    104
5111        J_SING=J_SING+1                                               104
5113        IF(J_SING-(2).GT.0) GO TO 5112                                104
            RSING=HEXG_IRAD/SIN(PI/3.0)                                   105
            THESING=J_SING*PI*2.0/3.0                                     106
C       Create SHXT :                                                     107
C       Call To Local Procedure SHXT                                      107
            ASSIGN 5130 TO BLOCK_SHXT                                     107
            GO TO  5120                                                   107
5130        CONTINUE                                                      107
C       *                                                                 107
C       Geant POSITION : SHXT ===> MOTHER='THXM' X=RSING*COS(THESING) Y=  108
C       RSING*SIN(THESING) Z=0 <===                                       108
            CALL AGSRESET                                                 108
            AG_MOTHER = 'THXM'                                            108
            AG_X = RSING*COS(THESING)                                     108
            AG_Y = RSING*SIN(THESING)                                     108
            AG_Z = 0                                                      108
            AG_TITLE = 'SHXT'                                             108
            AG_EXNAME = 'POSITION'                                        108
            AG_PARLIST = 'MOTHER_X_Y_Z'                                   108
            CALL AXPOSITION                                               108
C       *                                                                 108
         GO TO 5111                                                       109
5112     CONTINUE                                                         109
5091  CALL AGSPOPB                                                        110
      GO TO BLOCK_THXM, (5090 ,5100)                                      110
C End of Local Procedure                                                  110
C ----------------------------------------------------------------------  111
C ------------ SHXT                                                       111
C Begin Local Procedure SHXT                                              112
C SHXT IS ONE SINGLE HEXAGONAL TILE                                       112
5120  CONTINUE                                                            112
         AG_TITLE='SHXT'                                                  112
         AG_MODULE=MODULE                                                 112
         AG_MARK='V00'                                                    112
         CALL AGSPUSH                                                     112
         AG_MODULE=MODULE                                                 112
C    *                                                                    112
C    Geant MATERIAL : AIR ===> inherited arguments used <===              113
         CALL AGSRESET                                                    113
         AG_TITLE = 'AIR'                                                 113
         AG_EXNAME = 'MATERIAL'                                           113
         AG_PARLIST = 'NONE'                                              113
         CALL AXMATERIAL                                                  113
C    *                                                                    113
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           113
         CALL AGSRESET                                                    114
         AG_TITLE = 'STANDARD'                                            114
         AG_EXNAME = 'MEDIUM'                                             114
         AG_PARLIST = 'NONE'                                              114
         CALL AXMEDIUM                                                    114
C    *                                                                    114
C    Geant ATTRIBUTE : SHXT ===> SEEN=1 COLO=6 <===                       114
         CALL AGSRESET                                                    115
         AG_SEEN = 1                                                      115
         AG_COLO = 6                                                      115
         AG_TITLE = 'SHXT'                                                115
         AG_EXNAME = 'ATTRIBUTE'                                          115
         AG_PARLIST = 'SEEN_COLO'                                         115
         CALL AXATTRIBUTE                                                 115
C    *                                                                    115
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=6 NZ=2, ZI={-HEXG_THI  115
C    CK/2,HEXG_THICK/2} RMN={0,0} RMX={HEXG_IRAD, HEXG_IRAD} <===         115
         CALL AGSRESET                                                    118
         AG_PHI1 = 0                                                      118
         AG_DPHI = 360                                                    118
         AG_NPDIV = 6                                                     118
         AG_NZ = 2                                                        118
         AG_ZI(1)=-HEXG_THICK/2                                           118
         AG_ZI(2)=HEXG_THICK/2                                            118
         AG_RMN(1)=0                                                      118
         AG_RMN(2)=0                                                      118
         AG_RMX(1)=HEXG_IRAD                                              118
         AG_RMX(2)=HEXG_IRAD                                              118
         AG_TITLE = 'PGON'                                                118
         AG_EXNAME = 'SHAPE'                                              118
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     118
         CALL AXSHAPE                                                     118
C    *                                                                    118
         ACTR = HEXG_IRAD - HEXG_CLAD                                     118
C    Create CLAD :                                                        120
C    Call To Local Procedure CLAD                                         120
         ASSIGN 5150 TO BLOCK_CLAD                                        120
         GO TO  5140                                                      120
5150     CONTINUE                                                         120
C    *                                                                    120
C    Geant POSITION : CLAD ===> MOTHER='SHXT' X=0 Y=0 Z=0 <===            121
         CALL AGSRESET                                                    121
         AG_MOTHER = 'SHXT'                                               121
         AG_X = 0                                                         121
         AG_Y = 0                                                         121
         AG_Z = 0                                                         121
         AG_TITLE = 'CLAD'                                                121
         AG_EXNAME = 'POSITION'                                           121
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      121
         CALL AXPOSITION                                                  121
C    *                                                                    121
C    Create BPOL :                                                        122
C    Call To Local Procedure BPOL                                         122
         ASSIGN 5170 TO BLOCK_BPOL                                        122
         GO TO  5160                                                      122
5170     CONTINUE                                                         122
C    *                                                                    122
C    Geant POSITION : BPOL ===> MOTHER='SHXT' X=0 Y=0 Z=0 <===            123
         CALL AGSRESET                                                    123
         AG_MOTHER = 'SHXT'                                               123
         AG_X = 0                                                         123
         AG_Y = 0                                                         123
         AG_Z = 0                                                         123
         AG_TITLE = 'BPOL'                                                123
         AG_EXNAME = 'POSITION'                                           123
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      123
         CALL AXPOSITION                                                  123
C    *                                                                    123
5121  CALL AGSPOPB                                                        124
      GO TO BLOCK_SHXT, (5120 ,5130)                                      124
C End of Local Procedure                                                  124
C ----------------------------------------------------------------------  125
C ------------ CLAD                                                       125
C Begin Local Procedure CLAD                                              126
C CLAD IS ONE CLADDING OF BPOL ACTIVE REGION                              126
5140  CONTINUE                                                            126
         AG_TITLE='CLAD'                                                  126
         AG_MODULE=MODULE                                                 126
         AG_MARK='V00'                                                    126
         CALL AGSPUSH                                                     126
         AG_MODULE=MODULE                                                 126
C    *                                                                    126
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            127
         CALL AGSRESET                                                    127
         AG_TITLE = 'ALKAP'                                               127
         AG_EXNAME = 'MATERIAL'                                           127
         AG_PARLIST = 'NONE'                                              127
         CALL AXMATERIAL                                                  127
C    *                                                                    127
C    Geant ATTRIBUTE : CLAD ===> SEEN=1 COLO=3 <===                       127
         CALL AGSRESET                                                    128
         AG_SEEN = 1                                                      128
         AG_COLO = 3                                                      128
         AG_TITLE = 'CLAD'                                                128
         AG_EXNAME = 'ATTRIBUTE'                                          128
         AG_PARLIST = 'SEEN_COLO'                                         128
         CALL AXATTRIBUTE                                                 128
C    *                                                                    128
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=6 NZ=2, ZI={-HEXG_THI  128
C    CK/2,HEXG_THICK/2} RMN={ACTR,ACTR}, RMX={HEXG_IRAD,HEXG_IRAD} <===   128
         CALL AGSRESET                                                    131
         AG_PHI1 = 0                                                      131
         AG_DPHI = 360                                                    131
         AG_NPDIV = 6                                                     131
         AG_NZ = 2                                                        131
         AG_ZI(1)=-HEXG_THICK/2                                           131
         AG_ZI(2)=HEXG_THICK/2                                            131
         AG_RMN(1)=ACTR                                                   131
         AG_RMN(2)=ACTR                                                   131
         AG_RMX(1)=HEXG_IRAD                                              131
         AG_RMX(2)=HEXG_IRAD                                              131
         AG_TITLE = 'PGON'                                                131
         AG_EXNAME = 'SHAPE'                                              131
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     131
         CALL AXSHAPE                                                     131
C    *                                                                    131
5141  CALL AGSPOPB                                                        132
      GO TO BLOCK_CLAD, (5140 ,5150)                                      132
C End of Local Procedure                                                  132
C ----------------------------------------------------------------------  133
C ------------ BPOL                                                       133
C Begin Local Procedure BPOL                                              134
C BPOL IS ONE BBC POLYSTYREN ACTIVE SCINTILLATOR LAYER                    134
5160  CONTINUE                                                            134
         AG_TITLE='BPOL'                                                  134
         AG_MODULE=MODULE                                                 134
         AG_MARK='V00'                                                    134
         CALL AGSPUSH                                                     134
         AG_MODULE=MODULE                                                 134
C    *                                                                    134
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       135
         CALL AGSRESET                                                    135
         AG_TITLE = 'POLYSTYREN'                                          135
         AG_EXNAME = 'MATERIAL'                                           135
         AG_PARLIST = 'NONE'                                              135
         CALL AXMATERIAL                                                  135
C    *                                                                    135
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       135
         CALL AGSRESET                                                    136
         AG_ISVOL = 1                                                     136
         AG_TITLE = 'CPOLYSTYREN'                                         136
         AG_EXNAME = 'MATERIAL'                                           136
         AG_PARLIST = 'ISVOL'                                             136
         CALL AXMATERIAL                                                  136
C    *                                                                    136
C    Geant ATTRIBUTE : BPOL ===> SEEN=1 COLO=4 <===                       136
         CALL AGSRESET                                                    137
         AG_SEEN = 1                                                      137
         AG_COLO = 4                                                      137
         AG_TITLE = 'BPOL'                                                137
         AG_EXNAME = 'ATTRIBUTE'                                          137
         AG_PARLIST = 'SEEN_COLO'                                         137
         CALL AXATTRIBUTE                                                 137
C    *                                                                    137
C    Geant SHAPE : PGON ===> PHI1=0 DPHI=360 NPDIV=6 NZ=2, ZI={-HEXG_THI  137
C    CK/2,HEXG_THICK/2} RMN={0,0} RMX={ACTR, ACTR} <===                   137
         CALL AGSRESET                                                    140
         AG_PHI1 = 0                                                      140
         AG_DPHI = 360                                                    140
         AG_NPDIV = 6                                                     140
         AG_NZ = 2                                                        140
         AG_ZI(1)=-HEXG_THICK/2                                           140
         AG_ZI(2)=HEXG_THICK/2                                            140
         AG_RMN(1)=0                                                      140
         AG_RMN(2)=0                                                      140
         AG_RMX(1)=ACTR                                                   140
         AG_RMX(2)=ACTR                                                   140
         AG_TITLE = 'PGON'                                                140
         AG_EXNAME = 'SHAPE'                                              140
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     140
         CALL AXSHAPE                                                     140
C    *                                                                    140
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           140
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             142
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             143
         CALL GSTPAR(AG_IMED, 'CUTNEU',0.001)                             144
         CALL GSTPAR(AG_IMED, 'CUTHAD',0.001)                             145
         CALL GSTPAR(AG_IMED, 'CUTMUO',0.001)                             146
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.000)                             147
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.013)                             148
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            149
C    Defining Hits: TOF:16:C(0,1.0E-6) BIRK:0:C(0,10)                     150
         AG_TITLE = 'BPOL'                                                150
         AG_EXNAME = 'HITS'                                               150
         AG_TYPE=AG_MAGIC                                                 150
         CALL AGSDETV('BPOL')                                             150
C    *                                                                    150
         AG_PARLIST='TOF'                                                 150
         AG_TITLE = 'DETH'                                                150
         AG_NBITS = 16                                                    150
         AG_BIN=AG_MAGIC                                                  150
         AG_OPTION='C'                                                    150
         AG_FHMIN = 0                                                     150
         AG_FHMAX = 1.0E-6                                                150
         CALL AGSDETH (0)                                                 150
C    *                                                                    150
         AG_PARLIST='BIRK'                                                150
         AG_TITLE = 'DETH'                                                150
         AG_NBITS = 0                                                     150
         AG_BIN=AG_MAGIC                                                  150
         AG_OPTION='C'                                                    150
         AG_FHMIN = 0                                                     150
         AG_FHMAX = 10                                                    150
         CALL AGSDETH (0)                                                 150
C    *                                                                    150
         CALL AGSDETU('BPOL')                                             150
C    *                                                                    150
5161  CALL AGSPOPB                                                        151
      GO TO BLOCK_BPOL, (5160 ,5170)                                      151
C End of Local Procedure                                                  151
      WRITE(LOUT,5180)'===>>  [End AgML Module BbcmGeo]  <<==='           151
5180  FORMAT(A32,/,/)                                                     151
C MODULE BBCMGEO                                                          153
      END                                                                 153
