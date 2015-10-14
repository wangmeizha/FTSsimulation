C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIXLGEO6                                                 1
C PIXLGEO6 PIXEL DETECTOR GEOMETRY                                        1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIXLGEO6'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'PIXEL DETECTOR GEOMETRY'/                            1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JBOUCHET,FVIDEBAEK                                               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JBOUCHET,FVIDEBAEK'/                          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 11/07 2013                                                      2
      CHARACTER*40 CREATED /'11/07 2013'/                                 3
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
      INTEGER BLOCK_PXMO                                                  4
      INTEGER BLOCK_PXLA                                                  21
      INTEGER BLOCK_PXCA                                                  21
      INTEGER BLOCK_PXCC                                                  21
      INTEGER BLOCK_PXCD                                                  21
      INTEGER BLOCK_PXCE                                                  21
      INTEGER BLOCK_PXCF                                                  21
      INTEGER BLOCK_PXCG                                                  21
      INTEGER BLOCK_PXCH                                                  21
      INTEGER BLOCK_PXCB                                                  21
      INTEGER BLOCK_PXRB                                                  21
      INTEGER BLOCK_PXTR                                                  21
      INTEGER BLOCK_PXTM                                                  21
      INTEGER BLOCK_PXTL                                                  21
      INTEGER BLOCK_PXTJ                                                  21
      INTEGER BLOCK_PXTI                                                  21
      INTEGER BLOCK_PXLB                                                  21
      INTEGER BLOCK_PXIB                                                  21
      INTEGER BLOCK_LADR                                                  21
      INTEGER BLOCK_GLUA                                                  21
      INTEGER BLOCK_GLUB                                                  21
      INTEGER BLOCK_GLUC                                                  21
      INTEGER BLOCK_ALCA                                                  21
      INTEGER BLOCK_CFBK                                                  21
      INTEGER BLOCK_PXSI                                                  21
      INTEGER BLOCK_PLAC                                                  21
      INTEGER BLOCK_DRIV                                                  21
      INTEGER BLOCK_DTCA                                                  21
      INTEGER BLOCK_DTCB                                                  21
      INTEGER BLOCK_DTCC                                                  21
      INTEGER BLOCK_DTCD                                                  21
      INTEGER BLOCK_DTCE                                                  21
      INTEGER BLOCK_DTCF                                                  21
      INTEGER BLOCK_DTCG                                                  21
      INTEGER BLOCK_DTCH                                                  21
      INTEGER BLOCK_DTSA                                                  21
      INTEGER BLOCK_DTSB                                                  21
      INTEGER BLOCK_DTSC                                                  21
      INTEGER BLOCK_DTSD                                                  21
      INTEGER BLOCK_DTSE                                                  21
      INTEGER BLOCK_DTSF                                                  21
      INTEGER BLOCK_DTSG                                                  21
      INTEGER BLOCK_DTSH                                                  21
      INTEGER BLOCK_DTEA                                                  21
      INTEGER BLOCK_DTEB                                                  21
      INTEGER BLOCK_DTEC                                                  21
      INTEGER BLOCK_DTED                                                  21
      INTEGER BLOCK_DTEE                                                  21
      INTEGER BLOCK_DTEF                                                  21
      INTEGER BLOCK_DTEG                                                  21
      INTEGER BLOCK_DTEH                                                  21
      INTEGER BLOCK_DESA                                                  21
      INTEGER BLOCK_DESB                                                  21
      INTEGER BLOCK_DESC                                                  21
      INTEGER BLOCK_DESD                                                  21
      INTEGER BLOCK_DESE                                                  21
      INTEGER BLOCK_DESF                                                  21
      INTEGER BLOCK_DESG                                                  21
      INTEGER BLOCK_DESH                                                  21
      INTEGER BLOCK_PTRA                                                  21
      INTEGER BLOCK_PTRB                                                  21
      INTEGER BLOCK_PTRC                                                  21
      INTEGER BLOCK_PTRE                                                  21
      INTEGER BLOCK_PTRF                                                  21
      INTEGER BLOCK_PTRG                                                  21
      INTEGER BLOCK_PTMA                                                  21
      INTEGER BLOCK_PTMB                                                  21
      INTEGER BLOCK_PTMC                                                  21
      INTEGER BLOCK_PTME                                                  21
      INTEGER BLOCK_PTMF                                                  21
      INTEGER BLOCK_PTLA                                                  21
      INTEGER BLOCK_PTLB                                                  21
      INTEGER BLOCK_PTLC                                                  21
      INTEGER BLOCK_PTLD                                                  21
      INTEGER BLOCK_PTLE                                                  21
      INTEGER BLOCK_PTLF                                                  21
      INTEGER BLOCK_PTLG                                                  21
      INTEGER BLOCK_PTLH                                                  21
      INTEGER BLOCK_PTLI                                                  21
      INTEGER BLOCK_PTLJ                                                  21
      INTEGER BLOCK_BHCA                                                  21
      INTEGER BLOCK_BHCB                                                  21
      INTEGER BLOCK_BHCC                                                  21
      INTEGER BLOCK_BHCD                                                  21
      INTEGER BLOCK_BHCE                                                  21
      INTEGER BLOCK_BHCF                                                  21
      INTEGER BLOCK_BHCG                                                  21
      INTEGER BLOCK_BHCH                                                  21
      INTEGER BLOCK_BHSA                                                  21
      INTEGER BLOCK_BHSB                                                  21
      INTEGER BLOCK_BHSC                                                  21
      INTEGER BLOCK_BHSD                                                  21
      INTEGER BLOCK_BHSE                                                  21
      INTEGER BLOCK_BHSF                                                  21
      INTEGER BLOCK_BHSG                                                  21
      INTEGER BLOCK_BHSH                                                  21
      INTEGER BLOCK_PTOB                                                  21
      INTEGER BLOCK_PCAD                                                  21
      REAL *4 SECTORLENGTH/17.55/                                         22
      REAL *4 SECTORTHICKNESS/0.024/                                      24
      REAL *4 SECTORTHICKNESSNEG/-0.024/                                  26
      REAL *4 SECTORHALFTHICKNESS/0.012/                                  28
      REAL *4 SECTORZOFFSET/-3.8/                                         30
      REAL *4 BACKINGLENGTH/15.325/                                       32
      REAL *4 BACKINGWIDTH/1.245/                                         34
      REAL *4 BACKINGTHICKNESS/0.0175/                                    36
      REAL *4 BACKINGZOFFSET/0.00/                                        38
      REAL *4 DOVETAILEXTRUSIONLENGTH/0.05/                               40
      REAL *4 DOVETAILEXTRUSIONTHICKNESS/0.124/                           42
      REAL *4 DOVETAILEXTRUSIONTHICKNESSBOX/0.262/                        44
      REAL *4 DOVETAILEXTRUSIONTHICKNESSNEG/-0.124/                       46
      REAL *4 DOVETAILEXTRUSIONHALFTHICKNESS/0.062/                       48
      REAL *4 DOVETAILEXTRUSIONZOFFSET/-21.410/                           50
      REAL *4 DOVETAILSHAPEOFFSET/-22.360/                                52
      REAL *4 DOVETAILLENGTH/0.5/                                         54
      REAL *4 DOVETAILTHICKNESS/0.1/                                      56
      REAL *4 DOVETAILHALFTHICKNESS/0.05/                                 58
      REAL *4 DOVETAILZOFFSET/-20.8605/                                   60
      REAL *4 CABLELENGTH/15.325/                                         62
      REAL *4 CABLEZOFFSET/0/                                             64
      INTEGER *4 NSECTOR /10/                                             66
      REAL *4 MINRADSEC/2.12/                                             68
      REAL *4 MAXRADSEC/10/                                               70
      REAL *4 MINRVOL/2.1/                                                72
      REAL *4 MAXRVOL/11/                                                 74
      REAL *4 MAXZVOL/23.26/                                              76
      REAL *4 HALFMAXZ/23.26/                                             78
      REAL *4 MINPHISEC/72.0/                                             80
      REAL *4 MAXPHISEC/132.0/                                            82
      REAL *4 MINPHISECBARE/90.0/                                         84
      REAL *4 MAXPHISECBARE/126.0/                                        86
      REAL *4 XP, YP, XPOS,YPOS,ZPOS,ANGLE,ANGLEPOS, ZSENSOR, ALPHAZ,     88
     *ALPHAX, ALPHAY                                                      88
      INTEGER *4 SECTOR, LL                                               90
      REAL *4 LADBOXX, LADBOXY, LADBOXZ                                   92
      INTEGER *4 DEBUG, SENS, JJ, II                                      94
      REAL *4 GLUATHICKNESS /0.0050/                                      96
      REAL *4 GLUBTHICKNESS /0.0050/                                      98
      REAL *4 GLUCTHICKNESS /0.0100/                                      100
      REAL *4 LADDERHALFTHICKNESS/0.05225/                                102
      REAL *4 BULLHORNLENGTH/0.325/                                       104
      REAL *4 BULLHORNTHICKNESS/0.1/                                      106
      REAL *4 BULLHORNHALFTHICKNESS/0.05/                                 108
      REAL *4 BULLHORNZOFFSET/13.425/                                     110
      REAL *4 RADCABLE/0.165/                                             112
      REAL *4 LENGTHCAL/5.1576/                                           114
      REAL *4 POSCABL/-20.6812/                                           116
C -                                                                       118
      REAL SENP_VERSION                                                   118
      EQUIVALENCE (BANK_SENP(3),SENP_VERSION,RE_SENP_VERSION),            118
     *(NAMES_SENP(1),NSENP_VERSION)                                       118
      EQUIVALENCE (COMMEN_SENP(1),CSENP_VERSION),(MAP_SENP(1,1),          118
     *MSENP_VERSION)                                                      118
      CHARACTER CSENP_VERSION*40/' '/                                     118
      CHARACTER NSENP_VERSION*12/'R.VERSION'/                             118
      INTEGER MSENP_VERSION(2)/1,1/                                       118
      REAL RE_SENP_VERSION                                                118
C -                                                                       118
      INTEGER SENP_SENSOR                                                 118
      EQUIVALENCE (BANK_SENP(4),SENP_SENSOR,RE_SENP_SENSOR),              118
     *(NAMES_SENP(2),NSENP_SENSOR)                                        118
      EQUIVALENCE (COMMEN_SENP(2),CSENP_SENSOR),(MAP_SENP(1,2),           118
     *MSENP_SENSOR)                                                       118
      CHARACTER CSENP_SENSOR*40/' '/                                      118
      CHARACTER NSENP_SENSOR*12/'I.SENSOR'/                               118
      INTEGER MSENP_SENSOR(2)/1,1/                                        118
      REAL RE_SENP_SENSOR                                                 118
C -                                                                       118
      REAL SENP_ZPOS(10)                                                  118
      EQUIVALENCE (BANK_SENP(5),SENP_ZPOS,RE_SENP_ZPOS),(NAMES_SENP(3),   118
     *NSENP_ZPOS)                                                         118
      EQUIVALENCE (COMMEN_SENP(3),CSENP_ZPOS),(MAP_SENP(1,3),             118
     *MSENP_ZPOS)                                                         118
      CHARACTER CSENP_ZPOS*40/' '/                                        118
      CHARACTER NSENP_ZPOS*12/'R.ZPOS'/                                   118
      INTEGER MSENP_ZPOS(2)/10,1/                                         118
      REAL RE_SENP_ZPOS                                                   118
C -                                                                       118
      INTEGER LEN_SENP,LLEN_SENP,LINK_SENP                                118
      PARAMETER (LEN_SENP=14)                                             118
      PARAMETER (LLEN_SENP=3)                                             118
      INTEGER FLAG_SENP/0/,MAP_SENP(2,LLEN_SENP),IDNUM_SENP(10)           118
      CHARACTER NAMES_SENP(LLEN_SENP)*12,COMMEN_SENP(LLEN_SENP)*40        118
      DATA IDNUM_SENP/10*0/                                               118
      REAL BANK_SENP(LEN_SENP)                                            118
      CHARACTER*60 BPATH_SENP/' '/                                        118
      SAVE FLAG_SENP,MAP_SENP,IDNUM_SENP,NAMES_SENP,COMMEN_SENP,          118
     *BPATH_SENP                                                          118
      DATA LINK_SENP/0/,BANK_SENP/LEN_SENP*0/                             118
C *                                                                       118
C -                                                                       119
      INTEGER PXLW_VERSION                                                119
      EQUIVALENCE (BANK_PXLW(3),PXLW_VERSION,RE_PXLW_VERSION),            119
     *(NAMES_PXLW(1),NPXLW_VERSION)                                       119
      EQUIVALENCE (COMMEN_PXLW(1),CPXLW_VERSION),(MAP_PXLW(1,1),          119
     *MPXLW_VERSION)                                                      119
      CHARACTER CPXLW_VERSION*40/' '/                                     119
      CHARACTER NPXLW_VERSION*12/'I.VERSION'/                             119
      INTEGER MPXLW_VERSION(2)/1,1/                                       119
      REAL RE_PXLW_VERSION                                                119
C -                                                                       119
      REAL PXLW_LADRCONFIG                                                119
      EQUIVALENCE (BANK_PXLW(4),PXLW_LADRCONFIG,RE_PXLW_LADRCONFIG),      119
     *(NAMES_PXLW(2),NPXLW_LADRCONFIG)                                    119
      EQUIVALENCE (COMMEN_PXLW(2),CPXLW_LADRCONFIG),(MAP_PXLW(1,2),       119
     *MPXLW_LADRCONFIG)                                                   119
      CHARACTER CPXLW_LADRCONFIG*40/' '/                                  119
      CHARACTER NPXLW_LADRCONFIG*12/'R.LADRCONFIG'/                       119
      INTEGER MPXLW_LADRCONFIG(2)/1,1/                                    119
      REAL RE_PXLW_LADRCONFIG                                             119
C -                                                                       119
      REAL PXLW_SECVERSION                                                119
      EQUIVALENCE (BANK_PXLW(5),PXLW_SECVERSION,RE_PXLW_SECVERSION),      119
     *(NAMES_PXLW(3),NPXLW_SECVERSION)                                    119
      EQUIVALENCE (COMMEN_PXLW(3),CPXLW_SECVERSION),(MAP_PXLW(1,3),       119
     *MPXLW_SECVERSION)                                                   119
      CHARACTER CPXLW_SECVERSION*40/' '/                                  119
      CHARACTER NPXLW_SECVERSION*12/'R.SECVERSION'/                       119
      INTEGER MPXLW_SECVERSION(2)/1,1/                                    119
      REAL RE_PXLW_SECVERSION                                             119
C -                                                                       119
      INTEGER LEN_PXLW,LLEN_PXLW,LINK_PXLW                                119
      PARAMETER (LEN_PXLW=5)                                              119
      PARAMETER (LLEN_PXLW=3)                                             119
      INTEGER FLAG_PXLW/0/,MAP_PXLW(2,LLEN_PXLW),IDNUM_PXLW(10)           119
      CHARACTER NAMES_PXLW(LLEN_PXLW)*12,COMMEN_PXLW(LLEN_PXLW)*40        119
      DATA IDNUM_PXLW/10*0/                                               119
      REAL BANK_PXLW(LEN_PXLW)                                            119
      CHARACTER*60 BPATH_PXLW/' '/                                        119
      SAVE FLAG_PXLW,MAP_PXLW,IDNUM_PXLW,NAMES_PXLW,COMMEN_PXLW,          119
     *BPATH_PXLW                                                          119
      DATA LINK_PXLW/0/,BANK_PXLW/LEN_PXLW*0/                             119
C *                                                                       119
C -                                                                       120
      REAL PIXL_VERSION                                                   120
      EQUIVALENCE (BANK_PIXL(3),PIXL_VERSION,RE_PIXL_VERSION),            120
     *(NAMES_PIXL(1),NPIXL_VERSION)                                       120
      EQUIVALENCE (COMMEN_PIXL(1),CPIXL_VERSION),(MAP_PIXL(1,1),          120
     *MPIXL_VERSION)                                                      120
      CHARACTER CPIXL_VERSION*40/' '/                                     120
      CHARACTER NPIXL_VERSION*12/'R.VERSION'/                             120
      INTEGER MPIXL_VERSION(2)/1,1/                                       120
      REAL RE_PIXL_VERSION                                                120
C -                                                                       120
      INTEGER PIXL_SECTOR                                                 120
      EQUIVALENCE (BANK_PIXL(4),PIXL_SECTOR,RE_PIXL_SECTOR),              120
     *(NAMES_PIXL(2),NPIXL_SECTOR)                                        120
      EQUIVALENCE (COMMEN_PIXL(2),CPIXL_SECTOR),(MAP_PIXL(1,2),           120
     *MPIXL_SECTOR)                                                       120
      CHARACTER CPIXL_SECTOR*40/' '/                                      120
      CHARACTER NPIXL_SECTOR*12/'I.SECTOR'/                               120
      INTEGER MPIXL_SECTOR(2)/1,1/                                        120
      REAL RE_PIXL_SECTOR                                                 120
C -                                                                       120
      REAL PIXL_PHINUM(10)                                                120
      EQUIVALENCE (BANK_PIXL(5),PIXL_PHINUM,RE_PIXL_PHINUM),              120
     *(NAMES_PIXL(3),NPIXL_PHINUM)                                        120
      EQUIVALENCE (COMMEN_PIXL(3),CPIXL_PHINUM),(MAP_PIXL(1,3),           120
     *MPIXL_PHINUM)                                                       120
      CHARACTER CPIXL_PHINUM*40/' '/                                      120
      CHARACTER NPIXL_PHINUM*12/'R.PHINUM'/                               120
      INTEGER MPIXL_PHINUM(2)/10,1/                                       120
      REAL RE_PIXL_PHINUM                                                 120
C -                                                                       120
      INTEGER PIXL_ISON(10)                                               120
      EQUIVALENCE (BANK_PIXL(15),PIXL_ISON,RE_PIXL_ISON),(NAMES_PIXL(4),  120
     *NPIXL_ISON)                                                         120
      EQUIVALENCE (COMMEN_PIXL(4),CPIXL_ISON),(MAP_PIXL(1,4),             120
     *MPIXL_ISON)                                                         120
      CHARACTER CPIXL_ISON*40/' '/                                        120
      CHARACTER NPIXL_ISON*12/'I.ISON'/                                   120
      INTEGER MPIXL_ISON(2)/10,1/                                         120
      REAL RE_PIXL_ISON                                                   120
C -                                                                       120
      INTEGER LEN_PIXL,LLEN_PIXL,LINK_PIXL                                120
      PARAMETER (LEN_PIXL=24)                                             120
      PARAMETER (LLEN_PIXL=4)                                             120
      INTEGER FLAG_PIXL/0/,MAP_PIXL(2,LLEN_PIXL),IDNUM_PIXL(10)           120
      CHARACTER NAMES_PIXL(LLEN_PIXL)*12,COMMEN_PIXL(LLEN_PIXL)*40        120
      DATA IDNUM_PIXL/10*0/                                               120
      REAL BANK_PIXL(LEN_PIXL)                                            120
      CHARACTER*60 BPATH_PIXL/' '/                                        120
      SAVE FLAG_PIXL,MAP_PIXL,IDNUM_PIXL,NAMES_PIXL,COMMEN_PIXL,          120
     *BPATH_PIXL                                                          120
      DATA LINK_PIXL/0/,BANK_PIXL/LEN_PIXL*0/                             120
C *                                                                       120
C -                                                                       121
      INTEGER LCSD_VERSION                                                121
      EQUIVALENCE (BANK_LCSD(3),LCSD_VERSION,RE_LCSD_VERSION),            121
     *(NAMES_LCSD(1),NLCSD_VERSION)                                       121
      EQUIVALENCE (COMMEN_LCSD(1),CLCSD_VERSION),(MAP_LCSD(1,1),          121
     *MLCSD_VERSION)                                                      121
      CHARACTER CLCSD_VERSION*40/' '/                                     121
      CHARACTER NLCSD_VERSION*12/'I.VERSION'/                             121
      INTEGER MLCSD_VERSION(2)/1,1/                                       121
      REAL RE_LCSD_VERSION                                                121
C -                                                                       121
      INTEGER LCSD_NUMBEROFCABLES                                         121
      EQUIVALENCE (BANK_LCSD(4),LCSD_NUMBEROFCABLES,                      121
     *RE_LCSD_NUMBEROFCABLES),(NAMES_LCSD(2),NLCSD_NUMBEROFCABLES)        121
      EQUIVALENCE (COMMEN_LCSD(2),CLCSD_NUMBEROFCABLES),(MAP_LCSD(1,2),   121
     *MLCSD_NUMBEROFCABLES)                                               121
      CHARACTER CLCSD_NUMBEROFCABLES*40/' '/                              121
      CHARACTER NLCSD_NUMBEROFCABLES*12/'I.NUMBEROFCABLES'/               121
      INTEGER MLCSD_NUMBEROFCABLES(2)/1,1/                                121
      REAL RE_LCSD_NUMBEROFCABLES                                         121
C -                                                                       121
      REAL LCSD_XPOS(3)                                                   121
      EQUIVALENCE (BANK_LCSD(5),LCSD_XPOS,RE_LCSD_XPOS),(NAMES_LCSD(3),   121
     *NLCSD_XPOS)                                                         121
      EQUIVALENCE (COMMEN_LCSD(3),CLCSD_XPOS),(MAP_LCSD(1,3),             121
     *MLCSD_XPOS)                                                         121
      CHARACTER CLCSD_XPOS*40/' '/                                        121
      CHARACTER NLCSD_XPOS*12/'R.XPOS'/                                   121
      INTEGER MLCSD_XPOS(2)/3,1/                                          121
      REAL RE_LCSD_XPOS                                                   121
C -                                                                       121
      REAL LCSD_YPOS(3)                                                   121
      EQUIVALENCE (BANK_LCSD(8),LCSD_YPOS,RE_LCSD_YPOS),(NAMES_LCSD(4),   121
     *NLCSD_YPOS)                                                         121
      EQUIVALENCE (COMMEN_LCSD(4),CLCSD_YPOS),(MAP_LCSD(1,4),             121
     *MLCSD_YPOS)                                                         121
      CHARACTER CLCSD_YPOS*40/' '/                                        121
      CHARACTER NLCSD_YPOS*12/'R.YPOS'/                                   121
      INTEGER MLCSD_YPOS(2)/3,1/                                          121
      REAL RE_LCSD_YPOS                                                   121
C -                                                                       121
      INTEGER LEN_LCSD,LLEN_LCSD,LINK_LCSD                                121
      PARAMETER (LEN_LCSD=10)                                             121
      PARAMETER (LLEN_LCSD=4)                                             121
      INTEGER FLAG_LCSD/0/,MAP_LCSD(2,LLEN_LCSD),IDNUM_LCSD(10)           121
      CHARACTER NAMES_LCSD(LLEN_LCSD)*12,COMMEN_LCSD(LLEN_LCSD)*40        121
      DATA IDNUM_LCSD/10*0/                                               121
      REAL BANK_LCSD(LEN_LCSD)                                            121
      CHARACTER*60 BPATH_LCSD/' '/                                        121
      SAVE FLAG_LCSD,MAP_LCSD,IDNUM_LCSD,NAMES_LCSD,COMMEN_LCSD,          121
     *BPATH_LCSD                                                          121
      DATA LINK_LCSD/0/,BANK_LCSD/LEN_LCSD*0/                             121
C *                                                                       121
C -                                                                       122
      INTEGER PARO_VERSION                                                122
      EQUIVALENCE (BANK_PARO(3),PARO_VERSION,RE_PARO_VERSION),            122
     *(NAMES_PARO(1),NPARO_VERSION)                                       122
      EQUIVALENCE (COMMEN_PARO(1),CPARO_VERSION),(MAP_PARO(1,1),          122
     *MPARO_VERSION)                                                      122
      CHARACTER CPARO_VERSION*40/' '/                                     122
      CHARACTER NPARO_VERSION*12/'I.VERSION'/                             122
      INTEGER MPARO_VERSION(2)/1,1/                                       122
      REAL RE_PARO_VERSION                                                122
C -                                                                       122
      REAL PARO_DX                                                        122
      EQUIVALENCE (BANK_PARO(4),PARO_DX,RE_PARO_DX),(NAMES_PARO(2),       122
     *NPARO_DX)                                                           122
      EQUIVALENCE (COMMEN_PARO(2),CPARO_DX),(MAP_PARO(1,2),MPARO_DX)      122
      CHARACTER CPARO_DX*40/' '/                                          122
      CHARACTER NPARO_DX*12/'R.DX'/                                       122
      INTEGER MPARO_DX(2)/1,1/                                            122
      REAL RE_PARO_DX                                                     122
C -                                                                       122
      REAL PARO_DY                                                        122
      EQUIVALENCE (BANK_PARO(5),PARO_DY,RE_PARO_DY),(NAMES_PARO(3),       122
     *NPARO_DY)                                                           122
      EQUIVALENCE (COMMEN_PARO(3),CPARO_DY),(MAP_PARO(1,3),MPARO_DY)      122
      CHARACTER CPARO_DY*40/' '/                                          122
      CHARACTER NPARO_DY*12/'R.DY'/                                       122
      INTEGER MPARO_DY(2)/1,1/                                            122
      REAL RE_PARO_DY                                                     122
C -                                                                       122
      REAL PARO_DZ                                                        122
      EQUIVALENCE (BANK_PARO(6),PARO_DZ,RE_PARO_DZ),(NAMES_PARO(4),       122
     *NPARO_DZ)                                                           122
      EQUIVALENCE (COMMEN_PARO(4),CPARO_DZ),(MAP_PARO(1,4),MPARO_DZ)      122
      CHARACTER CPARO_DZ*40/' '/                                          122
      CHARACTER NPARO_DZ*12/'R.DZ'/                                       122
      INTEGER MPARO_DZ(2)/1,1/                                            122
      REAL RE_PARO_DZ                                                     122
C -                                                                       122
      REAL PARO_MULT                                                      122
      EQUIVALENCE (BANK_PARO(7),PARO_MULT,RE_PARO_MULT),(NAMES_PARO(5),   122
     *NPARO_MULT)                                                         122
      EQUIVALENCE (COMMEN_PARO(5),CPARO_MULT),(MAP_PARO(1,5),             122
     *MPARO_MULT)                                                         122
      CHARACTER CPARO_MULT*40/' '/                                        122
      CHARACTER NPARO_MULT*12/'R.MULT'/                                   122
      INTEGER MPARO_MULT(2)/1,1/                                          122
      REAL RE_PARO_MULT                                                   122
C -                                                                       122
      INTEGER PARO_SEC(10)                                                122
      EQUIVALENCE (BANK_PARO(8),PARO_SEC,RE_PARO_SEC),(NAMES_PARO(6),     122
     *NPARO_SEC)                                                          122
      EQUIVALENCE (COMMEN_PARO(6),CPARO_SEC),(MAP_PARO(1,6),MPARO_SEC)    122
      CHARACTER CPARO_SEC*40/' '/                                         122
      CHARACTER NPARO_SEC*12/'I.SEC'/                                     122
      INTEGER MPARO_SEC(2)/10,1/                                          122
      REAL RE_PARO_SEC                                                    122
C -                                                                       122
      INTEGER LEN_PARO,LLEN_PARO,LINK_PARO                                122
      PARAMETER (LEN_PARO=17)                                             122
      PARAMETER (LLEN_PARO=6)                                             122
      INTEGER FLAG_PARO/0/,MAP_PARO(2,LLEN_PARO),IDNUM_PARO(10)           122
      CHARACTER NAMES_PARO(LLEN_PARO)*12,COMMEN_PARO(LLEN_PARO)*40        122
      DATA IDNUM_PARO/10*0/                                               122
      REAL BANK_PARO(LEN_PARO)                                            122
      CHARACTER*60 BPATH_PARO/' '/                                        122
      SAVE FLAG_PARO,MAP_PARO,IDNUM_PARO,NAMES_PARO,COMMEN_PARO,          122
     *BPATH_PARO                                                          122
      DATA LINK_PARO/0/,BANK_PARO/LEN_PARO*0/                             122
C *                                                                       122
C -                                                                       123
      INTEGER PARA_VERSION                                                123
      EQUIVALENCE (BANK_PARA(3),PARA_VERSION,RE_PARA_VERSION),            123
     *(NAMES_PARA(1),NPARA_VERSION)                                       123
      EQUIVALENCE (COMMEN_PARA(1),CPARA_VERSION),(MAP_PARA(1,1),          123
     *MPARA_VERSION)                                                      123
      CHARACTER CPARA_VERSION*40/' '/                                     123
      CHARACTER NPARA_VERSION*12/'I.VERSION'/                             123
      INTEGER MPARA_VERSION(2)/1,1/                                       123
      REAL RE_PARA_VERSION                                                123
C -                                                                       123
      REAL PARA_DA                                                        123
      EQUIVALENCE (BANK_PARA(4),PARA_DA,RE_PARA_DA),(NAMES_PARA(2),       123
     *NPARA_DA)                                                           123
      EQUIVALENCE (COMMEN_PARA(2),CPARA_DA),(MAP_PARA(1,2),MPARA_DA)      123
      CHARACTER CPARA_DA*40/' '/                                          123
      CHARACTER NPARA_DA*12/'R.DA'/                                       123
      INTEGER MPARA_DA(2)/1,1/                                            123
      REAL RE_PARA_DA                                                     123
C -                                                                       123
      REAL PARA_DB                                                        123
      EQUIVALENCE (BANK_PARA(5),PARA_DB,RE_PARA_DB),(NAMES_PARA(3),       123
     *NPARA_DB)                                                           123
      EQUIVALENCE (COMMEN_PARA(3),CPARA_DB),(MAP_PARA(1,3),MPARA_DB)      123
      CHARACTER CPARA_DB*40/' '/                                          123
      CHARACTER NPARA_DB*12/'R.DB'/                                       123
      INTEGER MPARA_DB(2)/1,1/                                            123
      REAL RE_PARA_DB                                                     123
C -                                                                       123
      REAL PARA_DG                                                        123
      EQUIVALENCE (BANK_PARA(6),PARA_DG,RE_PARA_DG),(NAMES_PARA(4),       123
     *NPARA_DG)                                                           123
      EQUIVALENCE (COMMEN_PARA(4),CPARA_DG),(MAP_PARA(1,4),MPARA_DG)      123
      CHARACTER CPARA_DG*40/' '/                                          123
      CHARACTER NPARA_DG*12/'R.DG'/                                       123
      INTEGER MPARA_DG(2)/1,1/                                            123
      REAL RE_PARA_DG                                                     123
C -                                                                       123
      REAL PARA_MULT                                                      123
      EQUIVALENCE (BANK_PARA(7),PARA_MULT,RE_PARA_MULT),(NAMES_PARA(5),   123
     *NPARA_MULT)                                                         123
      EQUIVALENCE (COMMEN_PARA(5),CPARA_MULT),(MAP_PARA(1,5),             123
     *MPARA_MULT)                                                         123
      CHARACTER CPARA_MULT*40/' '/                                        123
      CHARACTER NPARA_MULT*12/'R.MULT'/                                   123
      INTEGER MPARA_MULT(2)/1,1/                                          123
      REAL RE_PARA_MULT                                                   123
C -                                                                       123
      INTEGER PARA_SEC(10)                                                123
      EQUIVALENCE (BANK_PARA(8),PARA_SEC,RE_PARA_SEC),(NAMES_PARA(6),     123
     *NPARA_SEC)                                                          123
      EQUIVALENCE (COMMEN_PARA(6),CPARA_SEC),(MAP_PARA(1,6),MPARA_SEC)    123
      CHARACTER CPARA_SEC*40/' '/                                         123
      CHARACTER NPARA_SEC*12/'I.SEC'/                                     123
      INTEGER MPARA_SEC(2)/10,1/                                          123
      REAL RE_PARA_SEC                                                    123
C -                                                                       123
      INTEGER LEN_PARA,LLEN_PARA,LINK_PARA                                123
      PARAMETER (LEN_PARA=17)                                             123
      PARAMETER (LLEN_PARA=6)                                             123
      INTEGER FLAG_PARA/0/,MAP_PARA(2,LLEN_PARA),IDNUM_PARA(10)           123
      CHARACTER NAMES_PARA(LLEN_PARA)*12,COMMEN_PARA(LLEN_PARA)*40        123
      DATA IDNUM_PARA/10*0/                                               123
      REAL BANK_PARA(LEN_PARA)                                            123
      CHARACTER*60 BPATH_PARA/' '/                                        123
      SAVE FLAG_PARA,MAP_PARA,IDNUM_PARA,NAMES_PARA,COMMEN_PARA,          123
     *BPATH_PARA                                                          123
      DATA LINK_PARA/0/,BANK_PARA/LEN_PARA*0/                             123
C *                                                                       123
C -                                                                       124
      INTEGER LADR_VERSION                                                124
      EQUIVALENCE (BANK_LADR(3),LADR_VERSION,RE_LADR_VERSION),            124
     *(NAMES_LADR(1),NLADR_VERSION)                                       124
      EQUIVALENCE (COMMEN_LADR(1),CLADR_VERSION),(MAP_LADR(1,1),          124
     *MLADR_VERSION)                                                      124
      CHARACTER CLADR_VERSION*40/' '/                                     124
      CHARACTER NLADR_VERSION*12/'I.VERSION'/                             124
      INTEGER MLADR_VERSION(2)/1,1/                                       124
      REAL RE_LADR_VERSION                                                124
C -                                                                       124
      INTEGER LADR_TYPE                                                   124
      EQUIVALENCE (BANK_LADR(4),LADR_TYPE,RE_LADR_TYPE),(NAMES_LADR(2),   124
     *NLADR_TYPE)                                                         124
      EQUIVALENCE (COMMEN_LADR(2),CLADR_TYPE),(MAP_LADR(1,2),             124
     *MLADR_TYPE)                                                         124
      CHARACTER CLADR_TYPE*40/' '/                                        124
      CHARACTER NLADR_TYPE*12/'I.TYPE'/                                   124
      INTEGER MLADR_TYPE(2)/1,1/                                          124
      REAL RE_LADR_TYPE                                                   124
C -                                                                       124
      REAL LADR_CABLETHICKNESS                                            124
      EQUIVALENCE (BANK_LADR(5),LADR_CABLETHICKNESS,                      124
     *RE_LADR_CABLETHICKNESS),(NAMES_LADR(3),NLADR_CABLETHICKNESS)        124
      EQUIVALENCE (COMMEN_LADR(3),CLADR_CABLETHICKNESS),(MAP_LADR(1,3),   124
     *MLADR_CABLETHICKNESS)                                               124
      CHARACTER CLADR_CABLETHICKNESS*40/' '/                              124
      CHARACTER NLADR_CABLETHICKNESS*12/'R.CABLETHICKNESS'/               124
      INTEGER MLADR_CABLETHICKNESS(2)/1,1/                                124
      REAL RE_LADR_CABLETHICKNESS                                         124
C -                                                                       124
      INTEGER LEN_LADR,LLEN_LADR,LINK_LADR                                124
      PARAMETER (LEN_LADR=5)                                              124
      PARAMETER (LLEN_LADR=3)                                             124
      INTEGER FLAG_LADR/0/,MAP_LADR(2,LLEN_LADR),IDNUM_LADR(10)           124
      CHARACTER NAMES_LADR(LLEN_LADR)*12,COMMEN_LADR(LLEN_LADR)*40        124
      DATA IDNUM_LADR/10*0/                                               124
      REAL BANK_LADR(LEN_LADR)                                            124
      CHARACTER*60 BPATH_LADR/' '/                                        124
      SAVE FLAG_LADR,MAP_LADR,IDNUM_LADR,NAMES_LADR,COMMEN_LADR,          124
     *BPATH_LADR                                                          124
      DATA LINK_LADR/0/,BANK_LADR/LEN_LADR*0/                             124
C *                                                                       124
C *                                                                       125
      CALL AGSBEGM(MODULE,IPRIN)                                          125
      IF (IPRIN.LT.0) RETURN                                              125
C Filling bank PXLW - PIXEL LADDER DATA                                   125
      BANK_TITLE='PIXEL LADDER DATA'                                      125
      BPATH_PXLW='PXLW*'                                                  125
      IDNUM_PXLW(1)=0                                                     126
C CONFIG VERSION                                                          126
      PXLW_VERSION=1                                                      126
      CPXLW_VERSION='CONFIG VERSION'                                      126
C LADDER VERSION                                                          127
      PXLW_LADRCONFIG=1.0                                                 127
      CPXLW_LADRCONFIG='LADDER VERSION'                                   127
C DIFFERENT CONFIGS OF PIXL                                               128
      PXLW_SECVERSION=0.0                                                 128
      CPXLW_SECVERSION='DIFFERENT CONFIGS OF PIXL'                        128
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLW',    129
     *BPATH_PXLW,IDNUM_PXLW,1, MAP_PXLW,NAMES_PXLW,COMMEN_PXLW,           129
     *BANK_PXLW,LEN_PXLW,LLEN_PXLW,LINK_PXLW,FLAG_PXLW)                   129
C *                                                                       129
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLW',    129
     *BPATH_PXLW,IDNUM_PXLW,1, MAP_PXLW,NAMES_PXLW,COMMEN_PXLW,           129
     *BANK_PXLW,LEN_PXLW,LLEN_PXLW,LINK_PXLW,FLAG_PXLW)                   129
C *                                                                       130
      CALL AGSBEGM(MODULE,IPRIN)                                          130
      IF (IPRIN.LT.0) RETURN                                              130
C Filling bank SENP - SENSORS PLACEMENT ON A LADDER                       130
      BANK_TITLE='SENSORS PLACEMENT ON A LADDER'                          130
      BPATH_SENP='SENP*'                                                  130
      IDNUM_SENP(1)=0                                                     131
C NONE                                                                    131
      SENP_VERSION=1.0                                                    131
      CSENP_VERSION='NONE'                                                131
C NUMBER OF SECTORS                                                       132
      SENP_SENSOR=10                                                      132
      CSENP_SENSOR='NUMBER OF SECTORS'                                    132
C Z POSITIONS [CM] OF THE 10 SENSORS INSIDE A LADDER                      134
      SENP_ZPOS(1)=-4.222                                                 134
      SENP_ZPOS(2)=-2.197                                                 134
      SENP_ZPOS(3)=-0.172                                                 134
      SENP_ZPOS(4)=1.853                                                  134
      SENP_ZPOS(5)=3.878                                                  134
      SENP_ZPOS(6)=5.903                                                  134
      SENP_ZPOS(7)=7.928                                                  134
      SENP_ZPOS(8)=9.953                                                  134
      SENP_ZPOS(9)=11.978                                                 134
      SENP_ZPOS(10)=14.003                                                134
      CSENP_ZPOS='Z POSITIONS [CM] OF THE 10 SENSORS INSIDE A LADDER'     134
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SENP',    135
     *BPATH_SENP,IDNUM_SENP,1, MAP_SENP,NAMES_SENP,COMMEN_SENP,           135
     *BANK_SENP,LEN_SENP,LLEN_SENP,LINK_SENP,FLAG_SENP)                   135
C *                                                                       135
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SENP',    135
     *BPATH_SENP,IDNUM_SENP,1, MAP_SENP,NAMES_SENP,COMMEN_SENP,           135
     *BANK_SENP,LEN_SENP,LLEN_SENP,LINK_SENP,FLAG_SENP)                   135
C *                                                                       136
      CALL AGSBEGM(MODULE,IPRIN)                                          136
      IF (IPRIN.LT.0) RETURN                                              136
C Filling bank PIXL - GENERAL PIXEL DETECTOR PARAMTERS                    136
      BANK_TITLE='GENERAL PIXEL DETECTOR PARAMTERS'                       136
      BPATH_PIXL='PIXL*'                                                  136
      IDNUM_PIXL(1)=0                                                     137
C NONE                                                                    137
      PIXL_VERSION=1.0                                                    137
      CPIXL_VERSION='NONE'                                                137
C NUMBER OF SECTORS                                                       138
      PIXL_SECTOR=10                                                      138
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    138
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    140
      PIXL_PHINUM(1)=0                                                    140
      PIXL_PHINUM(2)=324                                                  140
      PIXL_PHINUM(3)=288                                                  140
      PIXL_PHINUM(4)=252                                                  140
      PIXL_PHINUM(5)=216                                                  140
      PIXL_PHINUM(6)=180                                                  140
      PIXL_PHINUM(7)=144                                                  140
      PIXL_PHINUM(8)=108                                                  140
      PIXL_PHINUM(9)=72                                                   140
      PIXL_PHINUM(10)=36                                                  140
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  140
     *'                                                                   140
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      141
      PIXL_ISON(1)=1                                                      141
      PIXL_ISON(2)=1                                                      141
      PIXL_ISON(3)=1                                                      141
      PIXL_ISON(4)=1                                                      141
      PIXL_ISON(5)=1                                                      141
      PIXL_ISON(6)=1                                                      141
      PIXL_ISON(7)=1                                                      141
      PIXL_ISON(8)=1                                                      141
      PIXL_ISON(9)=1                                                      141
      PIXL_ISON(10)=1                                                     141
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     141
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    142
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           142
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   142
C *                                                                       142
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    142
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           142
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   142
C *                                                                       143
      CALL AGSBEGM(MODULE,IPRIN)                                          143
      IF (IPRIN.LT.0) RETURN                                              143
C Filling bank PARO - DEFAULT                                             143
      BANK_TITLE='DEFAULT'                                                143
      BPATH_PARO='PARO*'                                                  143
      IDNUM_PARO(1)=0                                                     144
C NONE                                                                    144
      PARO_VERSION=0                                                      144
      CPARO_VERSION='NONE'                                                144
C NO SHIFT                                                                145
      PARO_DX=0                                                           145
      CPARO_DX='NO SHIFT'                                                 145
C NO SHIFT                                                                146
      PARO_DY=0                                                           146
      CPARO_DY='NO SHIFT'                                                 146
C NO SHIFT                                                                147
      PARO_DZ=0                                                           147
      CPARO_DZ='NO SHIFT'                                                 147
C NO SHIFT                                                                148
      PARO_MULT=0                                                         148
      CPARO_MULT='NO SHIFT'                                               148
C 1..10                                                                   149
      PARO_SEC(1)=0                                                       149
      PARO_SEC(2)=0                                                       149
      PARO_SEC(3)=0                                                       149
      PARO_SEC(4)=0                                                       149
      PARO_SEC(5)=0                                                       149
      PARO_SEC(6)=0                                                       149
      PARO_SEC(7)=0                                                       149
      PARO_SEC(8)=0                                                       149
      PARO_SEC(9)=0                                                       149
      PARO_SEC(10)=0                                                      149
      CPARO_SEC='1..10'                                                   149
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    150
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           150
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   150
C *                                                                       150
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    150
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           150
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   150
C *                                                                       151
      CALL AGSBEGM(MODULE,IPRIN)                                          151
      IF (IPRIN.LT.0) RETURN                                              151
C Filling bank PARO - SHIFTZ                                              151
      BANK_TITLE='SHIFTZ'                                                 151
      BPATH_PARO='PARO*'                                                  151
      IDNUM_PARO(1)=0                                                     152
C NONE                                                                    152
      PARO_VERSION=1                                                      152
      CPARO_VERSION='NONE'                                                152
C SHIFT                                                                   153
      PARO_DX=0                                                           153
      CPARO_DX='SHIFT'                                                    153
C NO SHIFT                                                                154
      PARO_DY=0                                                           154
      CPARO_DY='NO SHIFT'                                                 154
C NO SHIFT                                                                155
      PARO_DZ=1                                                           155
      CPARO_DZ='NO SHIFT'                                                 155
C MAX IS 5 CM                                                             156
      PARO_MULT=5                                                         156
      CPARO_MULT='MAX IS 5 CM'                                            156
C 1..10                                                                   157
      PARO_SEC(1)=0                                                       157
      PARO_SEC(2)=0                                                       157
      PARO_SEC(3)=0                                                       157
      PARO_SEC(4)=0                                                       157
      PARO_SEC(5)=0                                                       157
      PARO_SEC(6)=0                                                       157
      PARO_SEC(7)=0                                                       157
      PARO_SEC(8)=0                                                       157
      PARO_SEC(9)=0                                                       157
      PARO_SEC(10)=0                                                      157
      CPARO_SEC='1..10'                                                   157
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    158
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           158
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   158
C *                                                                       158
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    158
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           158
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   158
C *                                                                       159
      CALL AGSBEGM(MODULE,IPRIN)                                          159
      IF (IPRIN.LT.0) RETURN                                              159
C Filling bank PARA - DEFAULT                                             159
      BANK_TITLE='DEFAULT'                                                159
      BPATH_PARA='PARA*'                                                  159
      IDNUM_PARA(1)=0                                                     160
C NONE                                                                    160
      PARA_VERSION=0                                                      160
      CPARA_VERSION='NONE'                                                160
C NO ROTATION                                                             161
      PARA_DA=0                                                           161
      CPARA_DA='NO ROTATION'                                              161
C NO ROTATION                                                             162
      PARA_DB=0                                                           162
      CPARA_DB='NO ROTATION'                                              162
C NO ROTATION                                                             163
      PARA_DG=0                                                           163
      CPARA_DG='NO ROTATION'                                              163
C NO ROTATION                                                             164
      PARA_MULT=0                                                         164
      CPARA_MULT='NO ROTATION'                                            164
C 1..10                                                                   165
      PARA_SEC(1)=0                                                       165
      PARA_SEC(2)=0                                                       165
      PARA_SEC(3)=0                                                       165
      PARA_SEC(4)=0                                                       165
      PARA_SEC(5)=0                                                       165
      PARA_SEC(6)=0                                                       165
      PARA_SEC(7)=0                                                       165
      PARA_SEC(8)=0                                                       165
      PARA_SEC(9)=0                                                       165
      PARA_SEC(10)=0                                                      165
      CPARA_SEC='1..10'                                                   165
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    166
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           166
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   166
C *                                                                       166
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    166
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           166
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   166
C *                                                                       167
      CALL AGSBEGM(MODULE,IPRIN)                                          167
      IF (IPRIN.LT.0) RETURN                                              167
C Filling bank PARA - ROTATIONG                                           167
      BANK_TITLE='ROTATIONG'                                              167
      BPATH_PARA='PARA*'                                                  167
      IDNUM_PARA(1)=0                                                     168
C NONE                                                                    168
      PARA_VERSION=1                                                      168
      CPARA_VERSION='NONE'                                                168
C NO ROT.                                                                 169
      PARA_DA=0                                                           169
      CPARA_DA='NO ROT.'                                                  169
C NO ROT.                                                                 170
      PARA_DB=0                                                           170
      CPARA_DB='NO ROT.'                                                  170
C ROT.                                                                    171
      PARA_DG=5                                                           171
      CPARA_DG='ROT.'                                                     171
C MAX IS 5 DEG                                                            172
      PARA_MULT=1                                                         172
      CPARA_MULT='MAX IS 5 DEG'                                           172
C 1..10                                                                   173
      PARA_SEC(1)=0                                                       173
      PARA_SEC(2)=0                                                       173
      PARA_SEC(3)=0                                                       173
      PARA_SEC(4)=0                                                       173
      PARA_SEC(5)=0                                                       173
      PARA_SEC(6)=0                                                       173
      PARA_SEC(7)=0                                                       173
      PARA_SEC(8)=0                                                       173
      PARA_SEC(9)=0                                                       173
      PARA_SEC(10)=0                                                      173
      CPARA_SEC='1..10'                                                   173
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    174
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           174
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   174
C *                                                                       174
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    174
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           174
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   174
C *                                                                       175
      CALL AGSBEGM(MODULE,IPRIN)                                          175
      IF (IPRIN.LT.0) RETURN                                              175
C Filling bank LADR - NONE                                                175
      BANK_TITLE='NONE'                                                   175
      BPATH_LADR='LADR*'                                                  175
      IDNUM_LADR(1)=0                                                     176
C NONE                                                                    176
      LADR_VERSION=1                                                      176
      CLADR_VERSION='NONE'                                                176
C CU CABLE                                                                177
      LADR_TYPE=1                                                         177
      CLADR_TYPE='CU CABLE'                                               177
C NONE                                                                    178
      LADR_CABLETHICKNESS=0.009                                           178
      CLADR_CABLETHICKNESS='NONE'                                         178
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    179
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           179
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   179
C *                                                                       179
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    179
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           179
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   179
C *                                                                       180
      CALL AGSBEGM(MODULE,IPRIN)                                          180
      IF (IPRIN.LT.0) RETURN                                              180
C Filling bank LADR - NONE                                                180
      BANK_TITLE='NONE'                                                   180
      BPATH_LADR='LADR*'                                                  180
      IDNUM_LADR(1)=0                                                     181
C NONE                                                                    181
      LADR_VERSION=2                                                      181
      CLADR_VERSION='NONE'                                                181
C AL CABLE                                                                182
      LADR_TYPE=0                                                         182
      CLADR_TYPE='AL CABLE'                                               182
C NONE                                                                    183
      LADR_CABLETHICKNESS=0.011                                           183
      CLADR_CABLETHICKNESS='NONE'                                         183
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    184
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           184
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   184
C *                                                                       184
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    184
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           184
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   184
C *                                                                       185
      CALL AGSBEGM(MODULE,IPRIN)                                          185
      IF (IPRIN.LT.0) RETURN                                              185
C Filling bank LCSD - LONG CABLE SECTION D                                185
      BANK_TITLE='LONG CABLE SECTION D'                                   185
      BPATH_LCSD='LCSD*'                                                  185
      IDNUM_LCSD(1)=0                                                     186
C NONE                                                                    186
      LCSD_VERSION=1                                                      186
      CLCSD_VERSION='NONE'                                                186
C NONE                                                                    187
      LCSD_NUMBEROFCABLES=3                                               187
      CLCSD_NUMBEROFCABLES='NONE'                                         187
C NONE                                                                    188
      LCSD_XPOS(1)=-1.2374                                                188
      LCSD_XPOS(2)=-2.9952                                                188
      LCSD_XPOS(3)=-4.62                                                  188
      CLCSD_XPOS='NONE'                                                   188
C NONE                                                                    189
      LCSD_YPOS(1)=8.574                                                  189
      LCSD_YPOS(2)=8.1293                                                 189
      LCSD_YPOS(3)=7.3289                                                 189
      CLCSD_YPOS='NONE'                                                   189
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LCSD',    190
     *BPATH_LCSD,IDNUM_LCSD,1, MAP_LCSD,NAMES_LCSD,COMMEN_LCSD,           190
     *BANK_LCSD,LEN_LCSD,LLEN_LCSD,LINK_LCSD,FLAG_LCSD)                   190
C *                                                                       190
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LCSD',    190
     *BPATH_LCSD,IDNUM_LCSD,1, MAP_LCSD,NAMES_LCSD,COMMEN_LCSD,           190
     *BANK_LCSD,LEN_LCSD,LLEN_LCSD,LINK_LCSD,FLAG_LCSD)                   190
C Geant COMPONENT : O ===> A=16 Z=8 W=0.164 <===                          191
      CALL AGSRESET                                                       191
      AG_A = 16                                                           191
      AG_Z = 8                                                            191
      AG_W = 0.164                                                        191
      AG_TITLE = 'O'                                                      191
      AG_EXNAME = 'COMPONENT'                                             191
      AG_PARLIST = 'A_Z_W'                                                191
      CALL AXCOMPONENT                                                    191
C *                                                                       191
C Geant COMPONENT : C ===> A=12 Z=6 W=0.763 <===                          191
      CALL AGSRESET                                                       192
      AG_A = 12                                                           192
      AG_Z = 6                                                            192
      AG_W = 0.763                                                        192
      AG_TITLE = 'C'                                                      192
      AG_EXNAME = 'COMPONENT'                                             192
      AG_PARLIST = 'A_Z_W'                                                192
      CALL AXCOMPONENT                                                    192
C *                                                                       192
C Geant COMPONENT : H ===> A=1 Z=1 W=0.073 <===                           192
      CALL AGSRESET                                                       193
      AG_A = 1                                                            193
      AG_Z = 1                                                            193
      AG_W = 0.073                                                        193
      AG_TITLE = 'H'                                                      193
      AG_EXNAME = 'COMPONENT'                                             193
      AG_PARLIST = 'A_Z_W'                                                193
      CALL AXCOMPONENT                                                    193
C *                                                                       193
C Geant MIXTURE : MATADHESIVE ===> DENS=1.2 <===                          193
      CALL AGSRESET                                                       194
      AG_DENS = 1.2                                                       194
      AG_TITLE = 'MATADHESIVE'                                            194
      AG_EXNAME = 'MIXTURE'                                               194
      AG_PARLIST = 'DENS'                                                 194
      CALL AXMIXTURE                                                      194
C *                                                                       194
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.281 <===                     194
      CALL AGSRESET                                                       195
      AG_A = 28.08                                                        195
      AG_Z = 14                                                           195
      AG_W = 0.281                                                        195
      AG_TITLE = 'SI'                                                     195
      AG_EXNAME = 'COMPONENT'                                             195
      AG_PARLIST = 'A_Z_W'                                                195
      CALL AXCOMPONENT                                                    195
C *                                                                       195
C Geant COMPONENT : O ===> A=16 Z=8 W=0.467 <===                          195
      CALL AGSRESET                                                       196
      AG_A = 16                                                           196
      AG_Z = 8                                                            196
      AG_W = 0.467                                                        196
      AG_TITLE = 'O'                                                      196
      AG_EXNAME = 'COMPONENT'                                             196
      AG_PARLIST = 'A_Z_W'                                                196
      CALL AXCOMPONENT                                                    196
C *                                                                       196
C Geant COMPONENT : C ===> A=12 Z=6 W=0.220 <===                          196
      CALL AGSRESET                                                       197
      AG_A = 12                                                           197
      AG_Z = 6                                                            197
      AG_W = 0.220                                                        197
      AG_TITLE = 'C'                                                      197
      AG_EXNAME = 'COMPONENT'                                             197
      AG_PARLIST = 'A_Z_W'                                                197
      CALL AXCOMPONENT                                                    197
C *                                                                       197
C Geant COMPONENT : H ===> A=1 Z=1 W=0.032 <===                           197
      CALL AGSRESET                                                       198
      AG_A = 1                                                            198
      AG_Z = 1                                                            198
      AG_W = 0.032                                                        198
      AG_TITLE = 'H'                                                      198
      AG_EXNAME = 'COMPONENT'                                             198
      AG_PARLIST = 'A_Z_W'                                                198
      CALL AXCOMPONENT                                                    198
C *                                                                       198
C Geant MIXTURE : MEDFR4 ===> DENS=1.80 <===                              198
      CALL AGSRESET                                                       199
      AG_DENS = 1.80                                                      199
      AG_TITLE = 'MEDFR4'                                                 199
      AG_EXNAME = 'MIXTURE'                                               199
      AG_PARLIST = 'DENS'                                                 199
      CALL AXMIXTURE                                                      199
C *                                                                       199
C Geant MATERIAL : CFRPMIX ===> A=12 Z=6 DENS=1.3 RADL=23.9 <===          199
      CALL AGSRESET                                                       200
      AG_A = 12                                                           200
      AG_Z = 6                                                            200
      AG_DENS = 1.3                                                       200
      AG_RADL = 23.9                                                      200
      AG_TITLE = 'CFRPMIX'                                                200
      AG_EXNAME = 'MATERIAL'                                              200
      AG_PARLIST = 'A_Z_DENS_RADL'                                        200
      CALL AXMATERIAL                                                     200
C *                                                                       200
C Geant MATERIAL : CBMIX ===> A=12 Z=6 DENS=1.3 RADL=68 <===              200
      CALL AGSRESET                                                       201
      AG_A = 12                                                           201
      AG_Z = 6                                                            201
      AG_DENS = 1.3                                                       201
      AG_RADL = 68                                                        201
      AG_TITLE = 'CBMIX'                                                  201
      AG_EXNAME = 'MATERIAL'                                              201
      AG_PARLIST = 'A_Z_DENS_RADL'                                        201
      CALL AXMATERIAL                                                     201
C *                                                                       201
C Geant COMPONENT : O ===> A=16 Z=8 W=0.209235 <===                       201
      CALL AGSRESET                                                       202
      AG_A = 16                                                           202
      AG_Z = 8                                                            202
      AG_W = 0.209235                                                     202
      AG_TITLE = 'O'                                                      202
      AG_EXNAME = 'COMPONENT'                                             202
      AG_PARLIST = 'A_Z_W'                                                202
      CALL AXCOMPONENT                                                    202
C *                                                                       202
C Geant COMPONENT : N ===> A=15 Z=7 W=0.073270 <===                       202
      CALL AGSRESET                                                       203
      AG_A = 15                                                           203
      AG_Z = 7                                                            203
      AG_W = 0.073270                                                     203
      AG_TITLE = 'N'                                                      203
      AG_EXNAME = 'COMPONENT'                                             203
      AG_PARLIST = 'A_Z_W'                                                203
      CALL AXCOMPONENT                                                    203
C *                                                                       203
C Geant COMPONENT : C ===> A=12 Z=6 W=0.691133 <===                       203
      CALL AGSRESET                                                       204
      AG_A = 12                                                           204
      AG_Z = 6                                                            204
      AG_W = 0.691133                                                     204
      AG_TITLE = 'C'                                                      204
      AG_EXNAME = 'COMPONENT'                                             204
      AG_PARLIST = 'A_Z_W'                                                204
      CALL AXCOMPONENT                                                    204
C *                                                                       204
C Geant COMPONENT : H ===> A=1 Z=1 W=0.026362 <===                        204
      CALL AGSRESET                                                       205
      AG_A = 1                                                            205
      AG_Z = 1                                                            205
      AG_W = 0.026362                                                     205
      AG_TITLE = 'H'                                                      205
      AG_EXNAME = 'COMPONENT'                                             205
      AG_PARLIST = 'A_Z_W'                                                205
      CALL AXCOMPONENT                                                    205
C *                                                                       205
C Geant MIXTURE : KAPTON ===> DENS=1.41 <===                              205
      CALL AGSRESET                                                       206
      AG_DENS = 1.41                                                      206
      AG_TITLE = 'KAPTON'                                                 206
      AG_EXNAME = 'MIXTURE'                                               206
      AG_PARLIST = 'DENS'                                                 206
      CALL AXMIXTURE                                                      206
C *                                                                       206
C Geant COMPONENT : CU ===> A=63.54 Z=29 W=0.802 <===                     206
      CALL AGSRESET                                                       207
      AG_A = 63.54                                                        207
      AG_Z = 29                                                           207
      AG_W = 0.802                                                        207
      AG_TITLE = 'CU'                                                     207
      AG_EXNAME = 'COMPONENT'                                             207
      AG_PARLIST = 'A_Z_W'                                                207
      CALL AXCOMPONENT                                                    207
C *                                                                       207
C Geant COMPONENT : KAPTON ===> A=12.767 Z=6.36 W=0.198 <===              207
      CALL AGSRESET                                                       208
      AG_A = 12.767                                                       208
      AG_Z = 6.36                                                         208
      AG_W = 0.198                                                        208
      AG_TITLE = 'KAPTON'                                                 208
      AG_EXNAME = 'COMPONENT'                                             208
      AG_PARLIST = 'A_Z_W'                                                208
      CALL AXCOMPONENT                                                    208
C *                                                                       208
C Geant MIXTURE : MATCUCABLE ===> DENS=4.37 <===                          208
      CALL AGSRESET                                                       209
      AG_DENS = 4.37                                                      209
      AG_TITLE = 'MATCUCABLE'                                             209
      AG_EXNAME = 'MIXTURE'                                               209
      AG_PARLIST = 'DENS'                                                 209
      CALL AXMIXTURE                                                      209
C *                                                                       209
C Geant COMPONENT : ALUMINIUM ===> A=27 Z=13 W=0.626 <===                 209
      CALL AGSRESET                                                       210
      AG_A = 27                                                           210
      AG_Z = 13                                                           210
      AG_W = 0.626                                                        210
      AG_TITLE = 'ALUMINIUM'                                              210
      AG_EXNAME = 'COMPONENT'                                             210
      AG_PARLIST = 'A_Z_W'                                                210
      CALL AXCOMPONENT                                                    210
C *                                                                       210
C Geant COMPONENT : KAPTON ===> A=12.767 Z=6.36 W=0.374 <===              210
      CALL AGSRESET                                                       211
      AG_A = 12.767                                                       211
      AG_Z = 6.36                                                         211
      AG_W = 0.374                                                        211
      AG_TITLE = 'KAPTON'                                                 211
      AG_EXNAME = 'COMPONENT'                                             211
      AG_PARLIST = 'A_Z_W'                                                211
      CALL AXCOMPONENT                                                    211
C *                                                                       211
C Geant MIXTURE : MATALCABLE ===> DENS=1.936 <===                         211
      CALL AGSRESET                                                       212
      AG_DENS = 1.936                                                     212
      AG_TITLE = 'MATALCABLE'                                             212
      AG_EXNAME = 'MIXTURE'                                               212
      AG_PARLIST = 'DENS'                                                 212
      CALL AXMIXTURE                                                      212
C *                                                                       212
C use default PXLW bank                                                   212
      BPATH_PXLW='PXLW*'                                                  213
      IDNUM_PXLW(1)=0                                                     213
      CALL AGDGETP('PIXLGEO6','*',-999,'PXLW',' ',BANK_PXLW, BPATH_PXLW,  213
     *IDNUM_PXLW,1,MAP_PXLW,NAMES_PXLW,BANK_PXLW,LEN_PXLW,LLEN_PXLW,      213
     *LINK_PXLW,FLAG_PXLW)                                                213
C use SENP bank                                                           214
      BPATH_SENP='SENP*'                                                  214
      IDNUM_SENP(1)=0                                                     214
      SENP_VERSION=1.0                                                    214
      CALL AGDGETP('PIXLGEO6','*',-999,'SENP',NSENP_VERSION,              214
     *RE_SENP_VERSION,BPATH_SENP,IDNUM_SENP,1,MAP_SENP,NAMES_SENP,        214
     *BANK_SENP,LEN_SENP,LLEN_SENP,LINK_SENP,FLAG_SENP)                   214
C use LADR bank                                                           215
      BPATH_LADR='LADR*'                                                  215
      IDNUM_LADR(1)=0                                                     215
      LADR_VERSION=PXLW_LADRCONFIG                                        215
      CALL AGDGETP('PIXLGEO6','*',-999,'LADR',NLADR_VERSION,              215
     *RE_LADR_VERSION,BPATH_LADR,IDNUM_LADR,1,MAP_LADR,NAMES_LADR,        215
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   215
C use PIXL bank                                                           216
      BPATH_PIXL='PIXL*'                                                  216
      IDNUM_PIXL(1)=0                                                     216
      PIXL_VERSION=PXLW_SECVERSION                                        216
      CALL AGDGETP('PIXLGEO6','*',-999,'PIXL',NPIXL_VERSION,              216
     *RE_PIXL_VERSION,BPATH_PIXL,IDNUM_PIXL,1,MAP_PIXL,NAMES_PIXL,        216
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   216
C use PARO bank                                                           217
      BPATH_PARO='PARO*'                                                  217
      IDNUM_PARO(1)=0                                                     217
      PARO_VERSION=0                                                      217
      CALL AGDGETP('PIXLGEO6','*',-999,'PARO',NPARO_VERSION,              217
     *RE_PARO_VERSION,BPATH_PARO,IDNUM_PARO,1,MAP_PARO,NAMES_PARO,        217
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   217
C use PARA bank                                                           218
      BPATH_PARA='PARA*'                                                  218
      IDNUM_PARA(1)=0                                                     218
      PARA_VERSION=0                                                      218
      CALL AGDGETP('PIXLGEO6','*',-999,'PARA',NPARA_VERSION,              218
     *RE_PARA_VERSION,BPATH_PARA,IDNUM_PARA,1,MAP_PARA,NAMES_PARA,        218
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   218
C use LCSD bank                                                           219
      BPATH_LCSD='LCSD*'                                                  219
      IDNUM_LCSD(1)=0                                                     219
      LCSD_VERSION=1                                                      219
      CALL AGDGETP('PIXLGEO6','*',-999,'LCSD',NLCSD_VERSION,              219
     *RE_LCSD_VERSION,BPATH_LCSD,IDNUM_LCSD,1,MAP_LCSD,NAMES_LCSD,        219
     *BANK_LCSD,LEN_LCSD,LLEN_LCSD,LINK_LCSD,FLAG_LCSD)                   219
      DEBUG = 0                                                           220
      WRITE(LOUT,5010)PXLW_SECVERSION                                     220
5010  FORMAT('PXL PXLW_version = ', F5.2)                                 220
      WRITE(LOUT,5020)PIXL_VERSION                                        221
5020  FORMAT(' PXL : PXL_version = ',F5.3)                                223
      WRITE(LOUT,5030)PXLW_SECVERSION                                     223
5030  FORMAT(' PXL : PXLW_SecVersion = ',F5.3)                            225
      WRITE(LOUT,5040)LADR_VERSION,LADR_TYPE                              225
5040  FORMAT(' PXL : LADR_version = ',I3,' LADR_type= ',I3)               227
      CALL AGSBEGM(MODULE,IPRIN)                                          228
C Check IPRIN<0                                                           228
      IF (IPRIN.LT.0) RETURN                                              228
C Create PXMO :                                                           228
C Call To Local Procedure PXMO                                            228
      ASSIGN 5060 TO BLOCK_PXMO                                           228
      GO TO  5050                                                         228
5060  CONTINUE                                                            228
C *                                                                       228
C Geant POSITION : PXMO ===> MOTHER='IDSM' X=0.0 Y=0.0 Z=0.0 KONLY='ONLY  229
C    *' <===                                                              229
      CALL AGSRESET                                                       229
      AG_MOTHER = 'IDSM'                                                  229
      AG_X = 0.0                                                          229
      AG_Y = 0.0                                                          229
      AG_Z = 0.0                                                          229
      AG_KONLY = 'ONLY'                                                   229
      AG_TITLE = 'PXMO'                                                   229
      AG_EXNAME = 'POSITION'                                              229
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   229
      CALL AXPOSITION                                                     229
C *                                                                       229
C ----------------------------------------------------------------------  229
C ------------ PXMO                                                       229
      CALL AGSENDM                                                        231
      RETURN                                                              231
C Begin Local Procedure PXMO                                              231
C PXMO MAIN VOLUME FOR ACTIVE SECTORS                                     231
5050  CONTINUE                                                            231
         AG_TITLE='PXMO'                                                  231
         AG_MODULE=MODULE                                                 231
         AG_MARK='V00'                                                    231
         CALL AGSPUSH                                                     231
         AG_MODULE=MODULE                                                 231
C    *                                                                    231
C    Geant MATERIAL : AIR ===> inherited arguments used <===              232
         CALL AGSRESET                                                    232
         AG_TITLE = 'AIR'                                                 232
         AG_EXNAME = 'MATERIAL'                                           232
         AG_PARLIST = 'NONE'                                              232
         CALL AXMATERIAL                                                  232
C    *                                                                    232
C    Geant ATTRIBUTE : PXMO ===> SEEN=0 COLO=6 <===                       232
         CALL AGSRESET                                                    233
         AG_SEEN = 0                                                      233
         AG_COLO = 6                                                      233
         AG_TITLE = 'PXMO'                                                233
         AG_EXNAME = 'ATTRIBUTE'                                          233
         AG_PARLIST = 'SEEN_COLO'                                         233
         CALL AXATTRIBUTE                                                 233
C    *                                                                    233
C    Geant SHAPE : TUBE ===> RMIN=MINRVOL RMAX=MAXRVOL DZ=MAXZVOL <===    233
         CALL AGSRESET                                                    234
         AG_RMIN = MINRVOL                                                234
         AG_RMAX = MAXRVOL                                                234
         AG_DZ = MAXZVOL                                                  234
         AG_TITLE = 'TUBE'                                                234
         AG_EXNAME = 'SHAPE'                                              234
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      234
         CALL AXSHAPE                                                     234
C    *                                                                    234
C    *                                                                    234
         II=1                                                             235
            GO TO 5073                                                    235
5071        II=II+1                                                       235
5073        IF(II-(PIXL_SECTOR).GT.0) GO TO 5072                          235
            ANGLE = PIXL_PHINUM(II)                                       236
            ANGLEPOS=ANGLE*(PI/180.0)                                     237
            XPOS=.00001*SIN(ANGLEPOS) +                                   238
     *      (PARO_DX*PARO_MULT)*PARO_SEC(II)                              238
            YPOS=.00001*COS(ANGLEPOS) +                                   239
     *      (PARO_DY*PARO_MULT)*PARO_SEC(II)                              239
            ZPOS = (PARO_DZ*PARO_MULT)*PARO_SEC(II)                       240
            ALPHAZ = -1*ANGLE -1*(PARA_DG*PARA_MULT)*PARA_SEC(II)         241
            ALPHAX = -1*(PARA_DA*PARA_MULT)*PARA_SEC(II)                  242
            ALPHAY = -1*(PARA_DB*PARA_MULT)*PARA_SEC(II)                  243
C       Check PIXL_ISON(II)==1                                            244
            IF (PIXL_ISON(II).EQ.1) THEN                                  244
C       Check DEBUG==1                                                    245
            IF (DEBUG.EQ.1) THEN                                          245
            WRITE(LOUT,5080)II                                            245
5080        FORMAT('Instrumented sector=', I2)                            245
            WRITE(LOUT,5090)ANGLE                                         246
5090        FORMAT('angle(deg)=', F7.3)                                   246
            WRITE(LOUT,5100)XPOS                                          247
5100        FORMAT('xpos=', F10.5)                                        247
            WRITE(LOUT,5110)YPOS                                          248
5110        FORMAT('ypos=', F10.5)                                        248
            WRITE(LOUT,5120)ZPOS                                          249
5120        FORMAT('zpos=', F10.5)                                        249
            WRITE(LOUT,5130)ALPHAZ                                        250
5130        FORMAT('alphaZ=', F7.2)                                       250
            WRITE(LOUT,5140)ALPHAX                                        251
5140        FORMAT('alphaX=', F6.2)                                       251
            WRITE(LOUT,5150)ALPHAY                                        252
5150        FORMAT('alphaY=', F6.2)                                       252
            ENDIF                                                         254
C       Create PXLA :                                                     255
C       Call To Local Procedure PXLA                                      255
            ASSIGN 5170 TO BLOCK_PXLA                                     255
            GO TO  5160                                                   255
5170        CONTINUE                                                      255
C       *                                                                 255
C       Geant POSITION : PXLA ===> MOTHER='PXMO' X=XPOS Y=YPOS Z=ZPOS AL  257
C       PHAZ=ALPHAZ ALPHAX=ALPHAX ALPHAY=ALPHAY NCOPY=II KONLY='MANY' <=  257
C       ==                                                                257
            CALL AGSRESET                                                 257
            AG_MOTHER = 'PXMO'                                            257
            AG_X = XPOS                                                   257
            AG_Y = YPOS                                                   257
            AG_Z = ZPOS                                                   257
            AG_ALPHAN=AG_ALPHAN+1                                         257
            AG_ALPHAK(AG_ALPHAN)=3                                        257
            AG_ALPHAL(AG_ALPHAN)=ALPHAZ                                   257
            AG_ALPHAN=AG_ALPHAN+1                                         257
            AG_ALPHAK(AG_ALPHAN)=1                                        257
            AG_ALPHAL(AG_ALPHAN)=ALPHAX                                   257
            AG_ALPHAN=AG_ALPHAN+1                                         257
            AG_ALPHAK(AG_ALPHAN)=2                                        257
            AG_ALPHAL(AG_ALPHAN)=ALPHAY                                   257
            AG_NCOPY = II                                                 257
            AG_KONLY = 'MANY'                                             257
            AG_TITLE = 'PXLA'                                             257
            AG_EXNAME = 'POSITION'                                        257
            AG_PARLIST =                                                  257
     *      'MOTHER_X_Y_Z_ALPHAZ_ALPHAX_ALPHAY_NCOPY_KONLY'               257
            CALL AXPOSITION                                               257
C       *                                                                 257
            ENDIF                                                         257
         GO TO 5071                                                       259
5072     CONTINUE                                                         259
5051  CALL AGSPOPB                                                        260
      GO TO BLOCK_PXMO, (5050 ,5060)                                      260
C End of Local Procedure                                                  260
C ----------------------------------------------------------------------  261
C ------------ PXLA                                                       261
C Begin Local Procedure PXLA                                              262
C PXLA PIXEL SECTOR WITH LADDERS                                          262
5160  CONTINUE                                                            262
         AG_TITLE='PXLA'                                                  262
         AG_MODULE=MODULE                                                 262
         AG_MARK='V00'                                                    262
         CALL AGSPUSH                                                     262
         AG_MODULE=MODULE                                                 262
C    *                                                                    262
C    Geant MATERIAL : AIR ===> inherited arguments used <===              263
         CALL AGSRESET                                                    263
         AG_TITLE = 'AIR'                                                 263
         AG_EXNAME = 'MATERIAL'                                           263
         AG_PARLIST = 'NONE'                                              263
         CALL AXMATERIAL                                                  263
C    *                                                                    263
C    Geant ATTRIBUTE : PXLA ===> SEEN=0 COLO=1 <===                       263
         CALL AGSRESET                                                    264
         AG_SEEN = 0                                                      264
         AG_COLO = 1                                                      264
         AG_TITLE = 'PXLA'                                                264
         AG_EXNAME = 'ATTRIBUTE'                                          264
         AG_PARLIST = 'SEEN_COLO'                                         264
         CALL AXATTRIBUTE                                                 264
C    *                                                                    264
C    Geant SHAPE : TUBS ===> RMIN=MINRADSEC RMAX=MAXRADSEC, PHI1=MINPHIS  264
C    EC PHI2=MAXPHISEC DZ=HALFMAXZ <===                                   264
         CALL AGSRESET                                                    266
         AG_RMIN = MINRADSEC                                              266
         AG_RMAX = MAXRADSEC                                              266
         AG_PHI1 = MINPHISEC                                              266
         AG_PHI2 = MAXPHISEC                                              266
         AG_DZ = HALFMAXZ                                                 266
         AG_TITLE = 'TUBS'                                                266
         AG_EXNAME = 'SHAPE'                                              266
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            266
         CALL AXSHAPE                                                     266
C    *                                                                    266
C    Create PXCA :                                                        267
C    Call To Local Procedure PXCA                                         267
         ASSIGN 5190 TO BLOCK_PXCA                                        267
         GO TO  5180                                                      267
5190     CONTINUE                                                         267
C    *                                                                    267
C    Geant POSITION : PXCA ===> X=-0.25 Y=2.90971 Z=SECTORZOFFSET <===    268
         CALL AGSRESET                                                    268
         AG_X = -0.25                                                     268
         AG_Y = 2.90971                                                   268
         AG_Z = SECTORZOFFSET                                             268
         AG_TITLE = 'PXCA'                                                268
         AG_EXNAME = 'POSITION'                                           268
         AG_PARLIST = 'X_Y_Z'                                             268
         CALL AXPOSITION                                                  268
C    *                                                                    268
C    Create PXCC :                                                        269
C    Call To Local Procedure PXCC                                         269
         ASSIGN 5210 TO BLOCK_PXCC                                        269
         GO TO  5200                                                      269
5210     CONTINUE                                                         269
C    *                                                                    269
C    Geant POSITION : PXCC ===> X=-0.25 Y=7.78014 Z=SECTORZOFFSET <===    270
         CALL AGSRESET                                                    270
         AG_X = -0.25                                                     270
         AG_Y = 7.78014                                                   270
         AG_Z = SECTORZOFFSET                                             270
         AG_TITLE = 'PXCC'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'X_Y_Z'                                             270
         CALL AXPOSITION                                                  270
C    *                                                                    270
C    Create PXCD :                                                        271
C    Call To Local Procedure PXCD                                         271
         ASSIGN 5230 TO BLOCK_PXCD                                        271
         GO TO  5220                                                      271
5230     CONTINUE                                                         271
C    *                                                                    271
C    Geant POSITION : PXCD ===> X=-1.482 Y=7.9982 Z=SECTORZOFFSET <===    272
         CALL AGSRESET                                                    272
         AG_X = -1.482                                                    272
         AG_Y = 7.9982                                                    272
         AG_Z = SECTORZOFFSET                                             272
         AG_TITLE = 'PXCD'                                                272
         AG_EXNAME = 'POSITION'                                           272
         AG_PARLIST = 'X_Y_Z'                                             272
         CALL AXPOSITION                                                  272
C    *                                                                    272
C    Create PXCE :                                                        273
C    Call To Local Procedure PXCE                                         273
         ASSIGN 5250 TO BLOCK_PXCE                                        273
         GO TO  5240                                                      273
5250     CONTINUE                                                         273
C    *                                                                    273
C    Geant POSITION : PXCE ===> X=-1.76032 Y=7.86181 Z=SECTORZOFFSET <==  274
C    =                                                                    274
         CALL AGSRESET                                                    274
         AG_X = -1.76032                                                  274
         AG_Y = 7.86181                                                   274
         AG_Z = SECTORZOFFSET                                             274
         AG_TITLE = 'PXCE'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'X_Y_Z'                                             274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Create PXCF :                                                        275
C    Call To Local Procedure PXCF                                         275
         ASSIGN 5270 TO BLOCK_PXCF                                        275
         GO TO  5260                                                      275
5270     CONTINUE                                                         275
C    *                                                                    275
C    Geant POSITION : PXCF ===> X=-3.11253 Y=7.51529 Z=SECTORZOFFSET <==  276
C    =                                                                    276
         CALL AGSRESET                                                    276
         AG_X = -3.11253                                                  276
         AG_Y = 7.51529                                                   276
         AG_Z = SECTORZOFFSET                                             276
         AG_TITLE = 'PXCF'                                                276
         AG_EXNAME = 'POSITION'                                           276
         AG_PARLIST = 'X_Y_Z'                                             276
         CALL AXPOSITION                                                  276
C    *                                                                    276
C    Create PXCG :                                                        277
C    Call To Local Procedure PXCG                                         277
         ASSIGN 5290 TO BLOCK_PXCG                                        277
         GO TO  5280                                                      277
5290     CONTINUE                                                         277
C    *                                                                    277
C    Geant POSITION : PXCG ===> X=-3.35642 Y=7.32402 Z=SECTORZOFFSET <==  278
C    =                                                                    278
         CALL AGSRESET                                                    278
         AG_X = -3.35642                                                  278
         AG_Y = 7.32402                                                   278
         AG_Z = SECTORZOFFSET                                             278
         AG_TITLE = 'PXCG'                                                278
         AG_EXNAME = 'POSITION'                                           278
         AG_PARLIST = 'X_Y_Z'                                             278
         CALL AXPOSITION                                                  278
C    *                                                                    278
C    Create PXCH :                                                        279
C    Call To Local Procedure PXCH                                         279
         ASSIGN 5310 TO BLOCK_PXCH                                        279
         GO TO  5300                                                      279
5310     CONTINUE                                                         279
C    *                                                                    279
C    Geant POSITION : PXCH ===> X=-4.56862 Y=6.71349 Z=SECTORZOFFSET <==  280
C    =                                                                    280
         CALL AGSRESET                                                    280
         AG_X = -4.56862                                                  280
         AG_Y = 6.71349                                                   280
         AG_Z = SECTORZOFFSET                                             280
         AG_TITLE = 'PXCH'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'X_Y_Z'                                             280
         CALL AXPOSITION                                                  280
C    *                                                                    280
C    Create PXCB :                                                        281
C    Call To Local Procedure PXCB                                         281
         ASSIGN 5330 TO BLOCK_PXCB                                        281
         GO TO  5320                                                      281
5330     CONTINUE                                                         281
C    *                                                                    281
C    Geant POSITION : PXCB ===> X=-1.90073 Y=3.04145 Z=SECTORZOFFSET <==  282
C    =                                                                    282
         CALL AGSRESET                                                    282
         AG_X = -1.90073                                                  282
         AG_Y = 3.04145                                                   282
         AG_Z = SECTORZOFFSET                                             282
         AG_TITLE = 'PXCB'                                                282
         AG_EXNAME = 'POSITION'                                           282
         AG_PARLIST = 'X_Y_Z'                                             282
         CALL AXPOSITION                                                  282
C    *                                                                    282
C    Create PXRB :                                                        283
C    Call To Local Procedure PXRB                                         283
         ASSIGN 5350 TO BLOCK_PXRB                                        283
         GO TO  5340                                                      283
5350     CONTINUE                                                         283
C    *                                                                    283
C    Geant POSITION : PXRB ===> X=-0.1135 Y=5.34493 Z=SECTORZOFFSET ALPH  284
C    AZ=90 <===                                                           284
         CALL AGSRESET                                                    284
         AG_X = -0.1135                                                   284
         AG_Y = 5.34493                                                   284
         AG_Z = SECTORZOFFSET                                             284
         AG_ALPHAN=AG_ALPHAN+1                                            284
         AG_ALPHAK(AG_ALPHAN)=3                                           284
         AG_ALPHAL(AG_ALPHAN)=90                                          284
         AG_TITLE = 'PXRB'                                                284
         AG_EXNAME = 'POSITION'                                           284
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      284
         CALL AXPOSITION                                                  284
C    *                                                                    284
C    Create PXTR :                                                        285
C    Call To Local Procedure PXTR                                         285
         ASSIGN 5370 TO BLOCK_PXTR                                        285
         GO TO  5360                                                      285
5370     CONTINUE                                                         285
C    *                                                                    285
C    Geant POSITION : PXTR ===> X=-0.842211 Y=8.02358 Z=SECTORZOFFSET AL  286
C    PHAZ=169.963 <===                                                    286
         CALL AGSRESET                                                    286
         AG_X = -0.842211                                                 286
         AG_Y = 8.02358                                                   286
         AG_Z = SECTORZOFFSET                                             286
         AG_ALPHAN=AG_ALPHAN+1                                            286
         AG_ALPHAK(AG_ALPHAN)=3                                           286
         AG_ALPHAL(AG_ALPHAN)=169.963                                     286
         AG_TITLE = 'PXTR'                                                286
         AG_EXNAME = 'POSITION'                                           286
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      286
         CALL AXPOSITION                                                  286
C    *                                                                    286
C    Create PXTM :                                                        287
C    Call To Local Procedure PXTM                                         287
         ASSIGN 5390 TO BLOCK_PXTM                                        287
         GO TO  5380                                                      287
5390     CONTINUE                                                         287
C    *                                                                    287
C    Geant POSITION : PXTM ===> X=-2.43597 Y=7.67506 Z=SECTORZOFFSET ALP  288
C    HAZ=181.963 <===                                                     288
         CALL AGSRESET                                                    288
         AG_X = -2.43597                                                  288
         AG_Y = 7.67506                                                   288
         AG_Z = SECTORZOFFSET                                             288
         AG_ALPHAN=AG_ALPHAN+1                                            288
         AG_ALPHAK(AG_ALPHAN)=3                                           288
         AG_ALPHAL(AG_ALPHAN)=181.963                                     288
         AG_TITLE = 'PXTM'                                                288
         AG_EXNAME = 'POSITION'                                           288
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      288
         CALL AXPOSITION                                                  288
C    *                                                                    288
C    Create PXTL :                                                        289
C    Call To Local Procedure PXTL                                         289
         ASSIGN 5410 TO BLOCK_PXTL                                        289
         GO TO  5400                                                      289
5410     CONTINUE                                                         289
C    *                                                                    289
C    Geant POSITION : PXTL ===> X=-3.95926 Y=7.00565 Z=SECTORZOFFSET ALP  290
C    HAZ=193.963 <===                                                     290
         CALL AGSRESET                                                    290
         AG_X = -3.95926                                                  290
         AG_Y = 7.00565                                                   290
         AG_Z = SECTORZOFFSET                                             290
         AG_ALPHAN=AG_ALPHAN+1                                            290
         AG_ALPHAK(AG_ALPHAN)=3                                           290
         AG_ALPHAL(AG_ALPHAN)=193.963                                     290
         AG_TITLE = 'PXTL'                                                290
         AG_EXNAME = 'POSITION'                                           290
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      290
         CALL AXPOSITION                                                  290
C    *                                                                    290
C    Create PXTJ :                                                        291
C    Call To Local Procedure PXTJ                                         291
         ASSIGN 5430 TO BLOCK_PXTJ                                        291
         GO TO  5420                                                      291
5430     CONTINUE                                                         291
C    *                                                                    291
C    Geant POSITION : PXTJ ===> X=-3.2221 Y=7.42505 Z=SECTORZOFFSET ALPH  292
C    AZ=293.554 <===                                                      292
         CALL AGSRESET                                                    292
         AG_X = -3.2221                                                   292
         AG_Y = 7.42505                                                   292
         AG_Z = SECTORZOFFSET                                             292
         AG_ALPHAN=AG_ALPHAN+1                                            292
         AG_ALPHAK(AG_ALPHAN)=3                                           292
         AG_ALPHAL(AG_ALPHAN)=293.554                                     292
         AG_TITLE = 'PXTJ'                                                292
         AG_EXNAME = 'POSITION'                                           292
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      292
         CALL AXPOSITION                                                  292
C    *                                                                    292
C    Create PXTI :                                                        293
C    Call To Local Procedure PXTI                                         293
         ASSIGN 5450 TO BLOCK_PXTI                                        293
         GO TO  5440                                                      293
5450     CONTINUE                                                         293
C    *                                                                    293
C    Geant POSITION : PXTI ===> X=-1.60794 Y=7.93271 Z=SECTORZOFFSET ALP  294
C    HAZ=281.554 <===                                                     294
         CALL AGSRESET                                                    294
         AG_X = -1.60794                                                  294
         AG_Y = 7.93271                                                   294
         AG_Z = SECTORZOFFSET                                             294
         AG_ALPHAN=AG_ALPHAN+1                                            294
         AG_ALPHAK(AG_ALPHAN)=3                                           294
         AG_ALPHAL(AG_ALPHAN)=281.554                                     294
         AG_TITLE = 'PXTI'                                                294
         AG_EXNAME = 'POSITION'                                           294
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      294
         CALL AXPOSITION                                                  294
C    *                                                                    294
C    Create PXLB :                                                        295
C    Call To Local Procedure PXLB                                         295
         ASSIGN 5470 TO BLOCK_PXLB                                        295
         GO TO  5460                                                      295
5470     CONTINUE                                                         295
C    *                                                                    295
C    Geant POSITION : PXLB ===> X=-3.3451 Y=4.79724 Z=SECTORZOFFSET ALPH  296
C    AZ=306 <===                                                          296
         CALL AGSRESET                                                    296
         AG_X = -3.3451                                                   296
         AG_Y = 4.79724                                                   296
         AG_Z = SECTORZOFFSET                                             296
         AG_ALPHAN=AG_ALPHAN+1                                            296
         AG_ALPHAK(AG_ALPHAN)=3                                           296
         AG_ALPHAL(AG_ALPHAN)=306                                         296
         AG_TITLE = 'PXLB'                                                296
         AG_EXNAME = 'POSITION'                                           296
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      296
         CALL AXPOSITION                                                  296
C    *                                                                    296
C    Create PXIB :                                                        297
C    Call To Local Procedure PXIB                                         297
         ASSIGN 5490 TO BLOCK_PXIB                                        297
         GO TO  5480                                                      297
5490     CONTINUE                                                         297
C    *                                                                    297
C    Geant POSITION : PXIB ===> X=-1.08622 Y=2.83951 Z=SECTORZOFFSET ALP  298
C    HAZ=355.437 <===                                                     298
         CALL AGSRESET                                                    298
         AG_X = -1.08622                                                  298
         AG_Y = 2.83951                                                   298
         AG_Z = SECTORZOFFSET                                             298
         AG_ALPHAN=AG_ALPHAN+1                                            298
         AG_ALPHAK(AG_ALPHAN)=3                                           298
         AG_ALPHAL(AG_ALPHAN)=355.437                                     298
         AG_TITLE = 'PXIB'                                                298
         AG_EXNAME = 'POSITION'                                           298
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      298
         CALL AXPOSITION                                                  298
C    *                                                                    298
C    Create DTCA :                                                        299
C    Call To Local Procedure DTCA                                         299
         ASSIGN 5510 TO BLOCK_DTCA                                        299
         GO TO  5500                                                      299
5510     CONTINUE                                                         299
C    *                                                                    299
C    Geant POSITION : DTCA ===> X=-0.25 Y=2.90971 Z=DOVETAILZOFFSET <===  300
         CALL AGSRESET                                                    300
         AG_X = -0.25                                                     300
         AG_Y = 2.90971                                                   300
         AG_Z = DOVETAILZOFFSET                                           300
         AG_TITLE = 'DTCA'                                                300
         AG_EXNAME = 'POSITION'                                           300
         AG_PARLIST = 'X_Y_Z'                                             300
         CALL AXPOSITION                                                  300
C    *                                                                    300
C    Create DTCB :                                                        301
C    Call To Local Procedure DTCB                                         301
         ASSIGN 5530 TO BLOCK_DTCB                                        301
         GO TO  5520                                                      301
5530     CONTINUE                                                         301
C    *                                                                    301
C    Geant POSITION : DTCB ===> X=-0.25 Y=7.78014 Z=DOVETAILZOFFSET <===  302
         CALL AGSRESET                                                    302
         AG_X = -0.25                                                     302
         AG_Y = 7.78014                                                   302
         AG_Z = DOVETAILZOFFSET                                           302
         AG_TITLE = 'DTCB'                                                302
         AG_EXNAME = 'POSITION'                                           302
         AG_PARLIST = 'X_Y_Z'                                             302
         CALL AXPOSITION                                                  302
C    *                                                                    302
C    Create DTCC :                                                        303
C    Call To Local Procedure DTCC                                         303
         ASSIGN 5550 TO BLOCK_DTCC                                        303
         GO TO  5540                                                      303
5550     CONTINUE                                                         303
C    *                                                                    303
C    Geant POSITION : DTCC ===> X=-1.482 Y=7.9982 Z=DOVETAILZOFFSET <===  304
         CALL AGSRESET                                                    304
         AG_X = -1.482                                                    304
         AG_Y = 7.9982                                                    304
         AG_Z = DOVETAILZOFFSET                                           304
         AG_TITLE = 'DTCC'                                                304
         AG_EXNAME = 'POSITION'                                           304
         AG_PARLIST = 'X_Y_Z'                                             304
         CALL AXPOSITION                                                  304
C    *                                                                    304
C    Create DTCD :                                                        305
C    Call To Local Procedure DTCD                                         305
         ASSIGN 5570 TO BLOCK_DTCD                                        305
         GO TO  5560                                                      305
5570     CONTINUE                                                         305
C    *                                                                    305
C    Geant POSITION : DTCD ===> X=-1.76032 Y=7.86181 Z=DOVETAILZOFFSET <  306
C    ===                                                                  306
         CALL AGSRESET                                                    306
         AG_X = -1.76032                                                  306
         AG_Y = 7.86181                                                   306
         AG_Z = DOVETAILZOFFSET                                           306
         AG_TITLE = 'DTCD'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'X_Y_Z'                                             306
         CALL AXPOSITION                                                  306
C    *                                                                    306
C    Create DTCE :                                                        307
C    Call To Local Procedure DTCE                                         307
         ASSIGN 5590 TO BLOCK_DTCE                                        307
         GO TO  5580                                                      307
5590     CONTINUE                                                         307
C    *                                                                    307
C    Geant POSITION : DTCE ===> X=-3.11253 Y=7.51529 Z=DOVETAILZOFFSET <  308
C    ===                                                                  308
         CALL AGSRESET                                                    308
         AG_X = -3.11253                                                  308
         AG_Y = 7.51529                                                   308
         AG_Z = DOVETAILZOFFSET                                           308
         AG_TITLE = 'DTCE'                                                308
         AG_EXNAME = 'POSITION'                                           308
         AG_PARLIST = 'X_Y_Z'                                             308
         CALL AXPOSITION                                                  308
C    *                                                                    308
C    Create DTCF :                                                        309
C    Call To Local Procedure DTCF                                         309
         ASSIGN 5610 TO BLOCK_DTCF                                        309
         GO TO  5600                                                      309
5610     CONTINUE                                                         309
C    *                                                                    309
C    Geant POSITION : DTCF ===> X=-3.35642 Y=7.32402 Z=DOVETAILZOFFSET <  310
C    ===                                                                  310
         CALL AGSRESET                                                    310
         AG_X = -3.35642                                                  310
         AG_Y = 7.32402                                                   310
         AG_Z = DOVETAILZOFFSET                                           310
         AG_TITLE = 'DTCF'                                                310
         AG_EXNAME = 'POSITION'                                           310
         AG_PARLIST = 'X_Y_Z'                                             310
         CALL AXPOSITION                                                  310
C    *                                                                    310
C    Create DTCG :                                                        311
C    Call To Local Procedure DTCG                                         311
         ASSIGN 5630 TO BLOCK_DTCG                                        311
         GO TO  5620                                                      311
5630     CONTINUE                                                         311
C    *                                                                    311
C    Geant POSITION : DTCG ===> X=-4.56862 Y=6.71349 Z=DOVETAILZOFFSET <  312
C    ===                                                                  312
         CALL AGSRESET                                                    312
         AG_X = -4.56862                                                  312
         AG_Y = 6.71349                                                   312
         AG_Z = DOVETAILZOFFSET                                           312
         AG_TITLE = 'DTCG'                                                312
         AG_EXNAME = 'POSITION'                                           312
         AG_PARLIST = 'X_Y_Z'                                             312
         CALL AXPOSITION                                                  312
C    *                                                                    312
C    Create DTCH :                                                        313
C    Call To Local Procedure DTCH                                         313
         ASSIGN 5650 TO BLOCK_DTCH                                        313
         GO TO  5640                                                      313
5650     CONTINUE                                                         313
C    *                                                                    313
C    Geant POSITION : DTCH ===> X=-1.90073 Y=3.04145 Z=DOVETAILZOFFSET <  314
C    ===                                                                  314
         CALL AGSRESET                                                    314
         AG_X = -1.90073                                                  314
         AG_Y = 3.04145                                                   314
         AG_Z = DOVETAILZOFFSET                                           314
         AG_TITLE = 'DTCH'                                                314
         AG_EXNAME = 'POSITION'                                           314
         AG_PARLIST = 'X_Y_Z'                                             314
         CALL AXPOSITION                                                  314
C    *                                                                    314
C    Create DTSA :                                                        315
C    Call To Local Procedure DTSA                                         315
         ASSIGN 5670 TO BLOCK_DTSA                                        315
         GO TO  5660                                                      315
5670     CONTINUE                                                         315
C    *                                                                    315
C    Geant POSITION : DTSA ===> X=-0.177 Y=5.34493 Z=DOVETAILZOFFSET ALP  316
C    HAZ=90 <===                                                          316
         CALL AGSRESET                                                    316
         AG_X = -0.177                                                    316
         AG_Y = 5.34493                                                   316
         AG_Z = DOVETAILZOFFSET                                           316
         AG_ALPHAN=AG_ALPHAN+1                                            316
         AG_ALPHAK(AG_ALPHAN)=3                                           316
         AG_ALPHAL(AG_ALPHAN)=90                                          316
         AG_TITLE = 'DTSA'                                                316
         AG_EXNAME = 'POSITION'                                           316
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      316
         CALL AXPOSITION                                                  316
C    *                                                                    316
C    Create DTSB :                                                        317
C    Call To Local Procedure DTSB                                         317
         ASSIGN 5690 TO BLOCK_DTSB                                        317
         GO TO  5680                                                      317
5690     CONTINUE                                                         317
C    *                                                                    317
C    Geant POSITION : DTSB ===> X=-0.853278 Y=7.96105 Z=DOVETAILZOFFSET   318
C    ALPHAZ=169.963 <===                                                  318
         CALL AGSRESET                                                    318
         AG_X = -0.853278                                                 318
         AG_Y = 7.96105                                                   318
         AG_Z = DOVETAILZOFFSET                                           318
         AG_ALPHAN=AG_ALPHAN+1                                            318
         AG_ALPHAK(AG_ALPHAN)=3                                           318
         AG_ALPHAL(AG_ALPHAN)=169.963                                     318
         AG_TITLE = 'DTSB'                                                318
         AG_EXNAME = 'POSITION'                                           318
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      318
         CALL AXPOSITION                                                  318
C    *                                                                    318
C    Create DTSC :                                                        319
C    Call To Local Procedure DTSC                                         319
         ASSIGN 5710 TO BLOCK_DTSC                                        319
         GO TO  5700                                                      319
5710     CONTINUE                                                         319
C    *                                                                    319
C    Geant POSITION : DTSC ===> X=-1.54572 Y=7.94543 Z=DOVETAILZOFFSET A  320
C    LPHAZ=281.554 <===                                                   320
         CALL AGSRESET                                                    320
         AG_X = -1.54572                                                  320
         AG_Y = 7.94543                                                   320
         AG_Z = DOVETAILZOFFSET                                           320
         AG_ALPHAN=AG_ALPHAN+1                                            320
         AG_ALPHAK(AG_ALPHAN)=3                                           320
         AG_ALPHAL(AG_ALPHAN)=281.554                                     320
         AG_TITLE = 'DTSC'                                                320
         AG_EXNAME = 'POSITION'                                           320
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      320
         CALL AXPOSITION                                                  320
C    *                                                                    320
C    Create DTSD :                                                        321
C    Call To Local Procedure DTSD                                         321
         ASSIGN 5730 TO BLOCK_DTSD                                        321
         GO TO  5720                                                      321
5730     CONTINUE                                                         321
C    *                                                                    321
C    Geant POSITION : DTSD ===> X=-2.43379 Y=7.6116 Z=DOVETAILZOFFSET AL  322
C    PHAZ=181.963 <===                                                    322
         CALL AGSRESET                                                    322
         AG_X = -2.43379                                                  322
         AG_Y = 7.6116                                                    322
         AG_Z = DOVETAILZOFFSET                                           322
         AG_ALPHAN=AG_ALPHAN+1                                            322
         AG_ALPHAK(AG_ALPHAN)=3                                           322
         AG_ALPHAL(AG_ALPHAN)=181.963                                     322
         AG_TITLE = 'DTSD'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      322
         CALL AXPOSITION                                                  322
C    *                                                                    322
C    Create DTSE :                                                        323
C    Call To Local Procedure DTSE                                         323
         ASSIGN 5750 TO BLOCK_DTSE                                        323
         GO TO  5740                                                      323
5750     CONTINUE                                                         323
C    *                                                                    323
C    Geant POSITION : DTSE ===> X=-3.16389 Y=7.45043 Z=DOVETAILZOFFSET A  324
C    LPHAZ=293.554 <===                                                   324
         CALL AGSRESET                                                    324
         AG_X = -3.16389                                                  324
         AG_Y = 7.45043                                                   324
         AG_Z = DOVETAILZOFFSET                                           324
         AG_ALPHAN=AG_ALPHAN+1                                            324
         AG_ALPHAK(AG_ALPHAN)=3                                           324
         AG_ALPHAL(AG_ALPHAN)=293.554                                     324
         AG_TITLE = 'DTSE'                                                324
         AG_EXNAME = 'POSITION'                                           324
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      324
         CALL AXPOSITION                                                  324
C    *                                                                    324
C    Create DTSF :                                                        325
C    Call To Local Procedure DTSF                                         325
         ASSIGN 5770 TO BLOCK_DTSF                                        325
         GO TO  5760                                                      325
5770     CONTINUE                                                         325
C    *                                                                    325
C    Geant POSITION : DTSF ===> X=-3.94394 Y=6.94403 Z=DOVETAILZOFFSET A  326
C    LPHAZ=193.963 <===                                                   326
         CALL AGSRESET                                                    326
         AG_X = -3.94394                                                  326
         AG_Y = 6.94403                                                   326
         AG_Z = DOVETAILZOFFSET                                           326
         AG_ALPHAN=AG_ALPHAN+1                                            326
         AG_ALPHAK(AG_ALPHAN)=3                                           326
         AG_ALPHAL(AG_ALPHAN)=193.963                                     326
         AG_TITLE = 'DTSF'                                                326
         AG_EXNAME = 'POSITION'                                           326
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      326
         CALL AXPOSITION                                                  326
C    *                                                                    326
C    Create DTSG :                                                        327
C    Call To Local Procedure DTSG                                         327
         ASSIGN 5790 TO BLOCK_DTSG                                        327
         GO TO  5780                                                      327
5790     CONTINUE                                                         327
C    *                                                                    327
C    Geant POSITION : DTSG ===> X=-3.29373 Y=4.83456 Z=DOVETAILZOFFSET A  328
C    LPHAZ=306 <===                                                       328
         CALL AGSRESET                                                    328
         AG_X = -3.29373                                                  328
         AG_Y = 4.83456                                                   328
         AG_Z = DOVETAILZOFFSET                                           328
         AG_ALPHAN=AG_ALPHAN+1                                            328
         AG_ALPHAK(AG_ALPHAN)=3                                           328
         AG_ALPHAL(AG_ALPHAN)=306                                         328
         AG_TITLE = 'DTSG'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      328
         CALL AXPOSITION                                                  328
C    *                                                                    328
C    Create DTSH :                                                        329
C    Call To Local Procedure DTSH                                         329
         ASSIGN 5810 TO BLOCK_DTSH                                        329
         GO TO  5800                                                      329
5810     CONTINUE                                                         329
C    *                                                                    329
C    Geant POSITION : DTSH ===> X=-1.08495 Y=2.876 Z=DOVETAILZOFFSET ALP  330
C    HAZ=357.305 <===                                                     330
         CALL AGSRESET                                                    330
         AG_X = -1.08495                                                  330
         AG_Y = 2.876                                                     330
         AG_Z = DOVETAILZOFFSET                                           330
         AG_ALPHAN=AG_ALPHAN+1                                            330
         AG_ALPHAK(AG_ALPHAN)=3                                           330
         AG_ALPHAL(AG_ALPHAN)=357.305                                     330
         AG_TITLE = 'DTSH'                                                330
         AG_EXNAME = 'POSITION'                                           330
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      330
         CALL AXPOSITION                                                  330
C    *                                                                    330
C    Create DTEA :                                                        331
C    Call To Local Procedure DTEA                                         331
         ASSIGN 5830 TO BLOCK_DTEA                                        331
         GO TO  5820                                                      331
5830     CONTINUE                                                         331
C    *                                                                    331
C    Geant POSITION : DTEA ===> X=-0.25 Y=2.90971 Z=DOVETAILEXTRUSIONZOF  332
C    FSET <===                                                            332
         CALL AGSRESET                                                    332
         AG_X = -0.25                                                     332
         AG_Y = 2.90971                                                   332
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  332
         AG_TITLE = 'DTEA'                                                332
         AG_EXNAME = 'POSITION'                                           332
         AG_PARLIST = 'X_Y_Z'                                             332
         CALL AXPOSITION                                                  332
C    *                                                                    332
C    Create DTEB :                                                        333
C    Call To Local Procedure DTEB                                         333
         ASSIGN 5850 TO BLOCK_DTEB                                        333
         GO TO  5840                                                      333
5850     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : DTEB ===> X=-0.25 Y=7.78014 Z=DOVETAILEXTRUSIONZOF  334
C    FSET <===                                                            334
         CALL AGSRESET                                                    334
         AG_X = -0.25                                                     334
         AG_Y = 7.78014                                                   334
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  334
         AG_TITLE = 'DTEB'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'X_Y_Z'                                             334
         CALL AXPOSITION                                                  334
C    *                                                                    334
C    Create DTEC :                                                        335
C    Call To Local Procedure DTEC                                         335
         ASSIGN 5870 TO BLOCK_DTEC                                        335
         GO TO  5860                                                      335
5870     CONTINUE                                                         335
C    *                                                                    335
C    Geant POSITION : DTEC ===> X=-1.482 Y=7.9982 Z=DOVETAILEXTRUSIONZOF  336
C    FSET <===                                                            336
         CALL AGSRESET                                                    336
         AG_X = -1.482                                                    336
         AG_Y = 7.9982                                                    336
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  336
         AG_TITLE = 'DTEC'                                                336
         AG_EXNAME = 'POSITION'                                           336
         AG_PARLIST = 'X_Y_Z'                                             336
         CALL AXPOSITION                                                  336
C    *                                                                    336
C    Create DTED :                                                        337
C    Call To Local Procedure DTED                                         337
         ASSIGN 5890 TO BLOCK_DTED                                        337
         GO TO  5880                                                      337
5890     CONTINUE                                                         337
C    *                                                                    337
C    Geant POSITION : DTED ===> X=-1.76032 Y=7.86181 Z=DOVETAILEXTRUSION  338
C    ZOFFSET <===                                                         338
         CALL AGSRESET                                                    338
         AG_X = -1.76032                                                  338
         AG_Y = 7.86181                                                   338
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  338
         AG_TITLE = 'DTED'                                                338
         AG_EXNAME = 'POSITION'                                           338
         AG_PARLIST = 'X_Y_Z'                                             338
         CALL AXPOSITION                                                  338
C    *                                                                    338
C    Create DTEE :                                                        339
C    Call To Local Procedure DTEE                                         339
         ASSIGN 5910 TO BLOCK_DTEE                                        339
         GO TO  5900                                                      339
5910     CONTINUE                                                         339
C    *                                                                    339
C    Geant POSITION : DTEE ===> X=-3.11253 Y=7.51529 Z=DOVETAILEXTRUSION  340
C    ZOFFSET <===                                                         340
         CALL AGSRESET                                                    340
         AG_X = -3.11253                                                  340
         AG_Y = 7.51529                                                   340
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  340
         AG_TITLE = 'DTEE'                                                340
         AG_EXNAME = 'POSITION'                                           340
         AG_PARLIST = 'X_Y_Z'                                             340
         CALL AXPOSITION                                                  340
C    *                                                                    340
C    Create DTEF :                                                        341
C    Call To Local Procedure DTEF                                         341
         ASSIGN 5930 TO BLOCK_DTEF                                        341
         GO TO  5920                                                      341
5930     CONTINUE                                                         341
C    *                                                                    341
C    Geant POSITION : DTEF ===> X=-3.35642 Y=7.32402 Z=DOVETAILEXTRUSION  342
C    ZOFFSET <===                                                         342
         CALL AGSRESET                                                    342
         AG_X = -3.35642                                                  342
         AG_Y = 7.32402                                                   342
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  342
         AG_TITLE = 'DTEF'                                                342
         AG_EXNAME = 'POSITION'                                           342
         AG_PARLIST = 'X_Y_Z'                                             342
         CALL AXPOSITION                                                  342
C    *                                                                    342
C    Create DTEG :                                                        343
C    Call To Local Procedure DTEG                                         343
         ASSIGN 5950 TO BLOCK_DTEG                                        343
         GO TO  5940                                                      343
5950     CONTINUE                                                         343
C    *                                                                    343
C    Geant POSITION : DTEG ===> X=-4.56862 Y=6.71349 Z=DOVETAILEXTRUSION  344
C    ZOFFSET <===                                                         344
         CALL AGSRESET                                                    344
         AG_X = -4.56862                                                  344
         AG_Y = 6.71349                                                   344
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  344
         AG_TITLE = 'DTEG'                                                344
         AG_EXNAME = 'POSITION'                                           344
         AG_PARLIST = 'X_Y_Z'                                             344
         CALL AXPOSITION                                                  344
C    *                                                                    344
C    Create DTEH :                                                        345
C    Call To Local Procedure DTEH                                         345
         ASSIGN 5970 TO BLOCK_DTEH                                        345
         GO TO  5960                                                      345
5970     CONTINUE                                                         345
C    *                                                                    345
C    Geant POSITION : DTEH ===> X=-1.90073 Y=3.04145 Z=DOVETAILEXTRUSION  346
C    ZOFFSET <===                                                         346
         CALL AGSRESET                                                    346
         AG_X = -1.90073                                                  346
         AG_Y = 3.04145                                                   346
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  346
         AG_TITLE = 'DTEH'                                                346
         AG_EXNAME = 'POSITION'                                           346
         AG_PARLIST = 'X_Y_Z'                                             346
         CALL AXPOSITION                                                  346
C    *                                                                    346
C    Create DESA :                                                        347
C    Call To Local Procedure DESA                                         347
         ASSIGN 5990 TO BLOCK_DESA                                        347
         GO TO  5980                                                      347
5990     CONTINUE                                                         347
C    *                                                                    347
C    Geant POSITION : DESA ===> X=-0.1635 Y=5.34493 Z=DOVETAILEXTRUSIONZ  348
C    OFFSET ALPHAZ=90 <===                                                348
         CALL AGSRESET                                                    348
         AG_X = -0.1635                                                   348
         AG_Y = 5.34493                                                   348
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  348
         AG_ALPHAN=AG_ALPHAN+1                                            348
         AG_ALPHAK(AG_ALPHAN)=3                                           348
         AG_ALPHAL(AG_ALPHAN)=90                                          348
         AG_TITLE = 'DESA'                                                348
         AG_EXNAME = 'POSITION'                                           348
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      348
         CALL AXPOSITION                                                  348
C    *                                                                    348
C    Create DESB :                                                        349
C    Call To Local Procedure DESB                                         349
         ASSIGN 6010 TO BLOCK_DESB                                        349
         GO TO  6000                                                      349
6010     CONTINUE                                                         349
C    *                                                                    349
C    Geant POSITION : DESB ===> X=-0.885782 Y=7.77741 Z=DOVETAILEXTRUSIO  350
C    NZOFFSET ALPHAZ=169.963 <===                                         350
         CALL AGSRESET                                                    350
         AG_X = -0.885782                                                 350
         AG_Y = 7.77741                                                   350
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  350
         AG_ALPHAN=AG_ALPHAN+1                                            350
         AG_ALPHAK(AG_ALPHAN)=3                                           350
         AG_ALPHAL(AG_ALPHAN)=169.963                                     350
         AG_TITLE = 'DESB'                                                350
         AG_EXNAME = 'POSITION'                                           350
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      350
         CALL AXPOSITION                                                  350
C    *                                                                    350
C    Create DESC :                                                        351
C    Call To Local Procedure DESC                                         351
         ASSIGN 6030 TO BLOCK_DESC                                        351
         GO TO  6020                                                      351
6030     CONTINUE                                                         351
C    *                                                                    351
C    Geant POSITION : DESC ===> X=-1.363 Y=7.98278 Z=DOVETAILEXTRUSIONZO  352
C    FFSET ALPHAZ=281.554 <===                                            352
         CALL AGSRESET                                                    352
         AG_X = -1.363                                                    352
         AG_Y = 7.98278                                                   352
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  352
         AG_ALPHAN=AG_ALPHAN+1                                            352
         AG_ALPHAK(AG_ALPHAN)=3                                           352
         AG_ALPHAL(AG_ALPHAN)=281.554                                     352
         AG_TITLE = 'DESC'                                                352
         AG_EXNAME = 'POSITION'                                           352
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      352
         CALL AXPOSITION                                                  352
C    *                                                                    352
C    Create DESD :                                                        353
C    Call To Local Procedure DESD                                         353
         ASSIGN 6050 TO BLOCK_DESD                                        353
         GO TO  6040                                                      353
6050     CONTINUE                                                         353
C    *                                                                    353
C    Geant POSITION : DESD ===> X=-2.4274 Y=7.42521 Z=DOVETAILEXTRUSIONZ  354
C    OFFSET ALPHAZ=181.963 <===                                           354
         CALL AGSRESET                                                    354
         AG_X = -2.4274                                                   354
         AG_Y = 7.42521                                                   354
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  354
         AG_ALPHAN=AG_ALPHAN+1                                            354
         AG_ALPHAK(AG_ALPHAN)=3                                           354
         AG_ALPHAL(AG_ALPHAN)=181.963                                     354
         AG_TITLE = 'DESD'                                                354
         AG_EXNAME = 'POSITION'                                           354
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      354
         CALL AXPOSITION                                                  354
C    *                                                                    354
C    Create DESE :                                                        355
C    Call To Local Procedure DESE                                         355
         ASSIGN 6070 TO BLOCK_DESE                                        355
         GO TO  6060                                                      355
6070     CONTINUE                                                         355
C    *                                                                    355
C    Geant POSITION : DESE ===> X=-2.99293 Y=7.52496 Z=DOVETAILEXTRUSION  356
C    ZOFFSET ALPHAZ=293.554 <===                                          356
         CALL AGSRESET                                                    356
         AG_X = -2.99293                                                  356
         AG_Y = 7.52496                                                   356
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  356
         AG_ALPHAN=AG_ALPHAN+1                                            356
         AG_ALPHAK(AG_ALPHAN)=3                                           356
         AG_ALPHAL(AG_ALPHAN)=293.554                                     356
         AG_TITLE = 'DESE'                                                356
         AG_EXNAME = 'POSITION'                                           356
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      356
         CALL AXPOSITION                                                  356
C    *                                                                    356
C    Create DESF :                                                        357
C    Call To Local Procedure DESF                                         357
         ASSIGN 6090 TO BLOCK_DESF                                        357
         GO TO  6080                                                      357
6090     CONTINUE                                                         357
C    *                                                                    357
C    Geant POSITION : DESF ===> X=-3.89894 Y=6.76304 Z=DOVETAILEXTRUSION  358
C    ZOFFSET ALPHAZ=193.963 <===                                          358
         CALL AGSRESET                                                    358
         AG_X = -3.89894                                                  358
         AG_Y = 6.76304                                                   358
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  358
         AG_ALPHAN=AG_ALPHAN+1                                            358
         AG_ALPHAK(AG_ALPHAN)=3                                           358
         AG_ALPHAL(AG_ALPHAN)=193.963                                     358
         AG_TITLE = 'DESF'                                                358
         AG_EXNAME = 'POSITION'                                           358
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      358
         CALL AXPOSITION                                                  358
C    *                                                                    358
C    Create DESG :                                                        359
C    Call To Local Procedure DESG                                         359
         ASSIGN 6110 TO BLOCK_DESG                                        359
         GO TO  6100                                                      359
6110     CONTINUE                                                         359
C    *                                                                    359
C    Geant POSITION : DESG ===> X=-3.14285 Y=4.94418 Z=DOVETAILEXTRUSION  360
C    ZOFFSET ALPHAZ=306 <===                                              360
         CALL AGSRESET                                                    360
         AG_X = -3.14285                                                  360
         AG_Y = 4.94418                                                   360
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  360
         AG_ALPHAN=AG_ALPHAN+1                                            360
         AG_ALPHAK(AG_ALPHAN)=3                                           360
         AG_ALPHAL(AG_ALPHAN)=306                                         360
         AG_TITLE = 'DESG'                                                360
         AG_EXNAME = 'POSITION'                                           360
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      360
         CALL AXPOSITION                                                  360
C    *                                                                    360
C    Create DESH :                                                        361
C    Call To Local Procedure DESH                                         361
         ASSIGN 6130 TO BLOCK_DESH                                        361
         GO TO  6120                                                      361
6130     CONTINUE                                                         361
C    *                                                                    361
C    Geant POSITION : DESH ===> X=-1.06633 Y=3.08872 Z=DOVETAILEXTRUSION  362
C    ZOFFSET ALPHAZ=355.437 <===                                          362
         CALL AGSRESET                                                    362
         AG_X = -1.06633                                                  362
         AG_Y = 3.08872                                                   362
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  362
         AG_ALPHAN=AG_ALPHAN+1                                            362
         AG_ALPHAK(AG_ALPHAN)=3                                           362
         AG_ALPHAL(AG_ALPHAN)=355.437                                     362
         AG_TITLE = 'DESH'                                                362
         AG_EXNAME = 'POSITION'                                           362
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      362
         CALL AXPOSITION                                                  362
C    *                                                                    362
C    Create PTRA :                                                        363
C    Call To Local Procedure PTRA                                         363
         ASSIGN 6150 TO BLOCK_PTRA                                        363
         GO TO  6140                                                      363
6150     CONTINUE                                                         363
C    *                                                                    363
C    Geant POSITION : PTRA ===> X=-0.844273 Y=7.31126 Z=0.85+DOVETAILSHA  364
C    PEOFFSET ALPHAZ=-174 <===                                            364
         CALL AGSRESET                                                    364
         AG_X = -0.844273                                                 364
         AG_Y = 7.31126                                                   364
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  364
         AG_ALPHAN=AG_ALPHAN+1                                            364
         AG_ALPHAK(AG_ALPHAN)=3                                           364
         AG_ALPHAL(AG_ALPHAN)=-174                                        364
         AG_TITLE = 'PTRA'                                                364
         AG_EXNAME = 'POSITION'                                           364
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      364
         CALL AXPOSITION                                                  364
C    *                                                                    364
C    Create PTRB :                                                        365
C    Call To Local Procedure PTRB                                         365
         ASSIGN 6170 TO BLOCK_PTRB                                        365
         GO TO  6160                                                      365
6170     CONTINUE                                                         365
C    *                                                                    365
C    Geant POSITION : PTRB ===> X=-2.2742 Y=6.99926 Z=0.85+DOVETAILSHAPE  366
C    OFFSET ALPHAZ=-162 <===                                              366
         CALL AGSRESET                                                    366
         AG_X = -2.2742                                                   366
         AG_Y = 6.99926                                                   366
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  366
         AG_ALPHAN=AG_ALPHAN+1                                            366
         AG_ALPHAK(AG_ALPHAN)=3                                           366
         AG_ALPHAL(AG_ALPHAN)=-162                                        366
         AG_TITLE = 'PTRB'                                                366
         AG_EXNAME = 'POSITION'                                           366
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      366
         CALL AXPOSITION                                                  366
C    *                                                                    366
C    Create PTRC :                                                        367
C    Call To Local Procedure PTRC                                         367
         ASSIGN 6190 TO BLOCK_PTRC                                        367
         GO TO  6180                                                      367
6190     CONTINUE                                                         367
C    *                                                                    367
C    Geant POSITION : PTRC ===> X=-3.61442 Y=6.41119 Z=0.85+DOVETAILSHAP  368
C    EOFFSET ALPHAZ=-150 <===                                             368
         CALL AGSRESET                                                    368
         AG_X = -3.61442                                                  368
         AG_Y = 6.41119                                                   368
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  368
         AG_ALPHAN=AG_ALPHAN+1                                            368
         AG_ALPHAK(AG_ALPHAN)=3                                           368
         AG_ALPHAL(AG_ALPHAN)=-150                                        368
         AG_TITLE = 'PTRC'                                                368
         AG_EXNAME = 'POSITION'                                           368
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      368
         CALL AXPOSITION                                                  368
C    *                                                                    368
C    Create PTRE :                                                        369
C    Call To Local Procedure PTRE                                         369
         ASSIGN 6210 TO BLOCK_PTRE                                        369
         GO TO  6200                                                      369
6210     CONTINUE                                                         369
C    *                                                                    369
C    Geant POSITION : PTRE ===> X=-0.45 Y=4.94431 Z=0.85+DOVETAILSHAPEOF  370
C    FSET ALPHAZ=90 <===                                                  370
         CALL AGSRESET                                                    370
         AG_X = -0.45                                                     370
         AG_Y = 4.94431                                                   370
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  370
         AG_ALPHAN=AG_ALPHAN+1                                            370
         AG_ALPHAK(AG_ALPHAN)=3                                           370
         AG_ALPHAL(AG_ALPHAN)=90                                          370
         AG_TITLE = 'PTRE'                                                370
         AG_EXNAME = 'POSITION'                                           370
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      370
         CALL AXPOSITION                                                  370
C    *                                                                    370
C    Create PTRF :                                                        371
C    Call To Local Procedure PTRF                                         371
         ASSIGN 6230 TO BLOCK_PTRF                                        371
         GO TO  6220                                                      371
6230     CONTINUE                                                         371
C    *                                                                    371
C    Geant POSITION : PTRF ===> X=-2.66607 Y=4.43512 Z=0.85+DOVETAILSHAP  372
C    EOFFSET ALPHAZ=126 <===                                              372
         CALL AGSRESET                                                    372
         AG_X = -2.66607                                                  372
         AG_Y = 4.43512                                                   372
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  372
         AG_ALPHAN=AG_ALPHAN+1                                            372
         AG_ALPHAK(AG_ALPHAN)=3                                           372
         AG_ALPHAL(AG_ALPHAN)=126                                         372
         AG_TITLE = 'PTRF'                                                372
         AG_EXNAME = 'POSITION'                                           372
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      372
         CALL AXPOSITION                                                  372
C    *                                                                    372
C    Create PTRG :                                                        373
C    Call To Local Procedure PTRG                                         373
         ASSIGN 6250 TO BLOCK_PTRG                                        373
         GO TO  6240                                                      373
6250     CONTINUE                                                         373
C    *                                                                    373
C    Geant POSITION : PTRG ===> X=-1.07889 Y=3.18738 Z=0.25+DOVETAILSHAP  374
C    EOFFSET ALPHAZ=175.437 <===                                          374
         CALL AGSRESET                                                    374
         AG_X = -1.07889                                                  374
         AG_Y = 3.18738                                                   374
         AG_Z = 0.25+DOVETAILSHAPEOFFSET                                  374
         AG_ALPHAN=AG_ALPHAN+1                                            374
         AG_ALPHAK(AG_ALPHAN)=3                                           374
         AG_ALPHAL(AG_ALPHAN)=175.437                                     374
         AG_TITLE = 'PTRG'                                                374
         AG_EXNAME = 'POSITION'                                           374
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      374
         CALL AXPOSITION                                                  374
C    *                                                                    374
C    Create PTMA :                                                        375
C    Call To Local Procedure PTMA                                         375
         ASSIGN 6270 TO BLOCK_PTMA                                        375
         GO TO  6260                                                      375
6270     CONTINUE                                                         375
C    *                                                                    375
C    Geant POSITION : PTMA ===> X=-1.09427 Y=7.28499 Z=0.2+DOVETAILSHAPE  376
C    OFFSET ALPHAZ=-174 <===                                              376
         CALL AGSRESET                                                    376
         AG_X = -1.09427                                                  376
         AG_Y = 7.28499                                                   376
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   376
         AG_ALPHAN=AG_ALPHAN+1                                            376
         AG_ALPHAK(AG_ALPHAN)=3                                           376
         AG_ALPHAL(AG_ALPHAN)=-174                                        376
         AG_TITLE = 'PTMA'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      376
         CALL AXPOSITION                                                  376
C    *                                                                    376
C    Create PTMB :                                                        377
C    Call To Local Procedure PTMB                                         377
         ASSIGN 6290 TO BLOCK_PTMB                                        377
         GO TO  6280                                                      377
6290     CONTINUE                                                         377
C    *                                                                    377
C    Geant POSITION : PTMB ===> X=-2.2742 Y=6.99926 Z=0.2+DOVETAILSHAPEO  378
C    FFSET ALPHAZ=-162 <===                                               378
         CALL AGSRESET                                                    378
         AG_X = -2.2742                                                   378
         AG_Y = 6.99926                                                   378
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   378
         AG_ALPHAN=AG_ALPHAN+1                                            378
         AG_ALPHAK(AG_ALPHAN)=3                                           378
         AG_ALPHAL(AG_ALPHAN)=-162                                        378
         AG_TITLE = 'PTMB'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      378
         CALL AXPOSITION                                                  378
C    *                                                                    378
C    Create PTMC :                                                        379
C    Call To Local Procedure PTMC                                         379
         ASSIGN 6310 TO BLOCK_PTMC                                        379
         GO TO  6300                                                      379
6310     CONTINUE                                                         379
C    *                                                                    379
C    Geant POSITION : PTMC ===> X=-3.39672 Y=6.53688 Z=0.2+DOVETAILSHAPE  380
C    OFFSET ALPHAZ=-150 <===                                              380
         CALL AGSRESET                                                    380
         AG_X = -3.39672                                                  380
         AG_Y = 6.53688                                                   380
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   380
         AG_ALPHAN=AG_ALPHAN+1                                            380
         AG_ALPHAK(AG_ALPHAN)=3                                           380
         AG_ALPHAL(AG_ALPHAN)=-150                                        380
         AG_TITLE = 'PTMC'                                                380
         AG_EXNAME = 'POSITION'                                           380
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      380
         CALL AXPOSITION                                                  380
C    *                                                                    380
C    Create PTME :                                                        381
C    Call To Local Procedure PTME                                         381
         ASSIGN 6330 TO BLOCK_PTME                                        381
         GO TO  6320                                                      381
6330     CONTINUE                                                         381
C    *                                                                    381
C    Geant POSITION : PTME ===> X=-0.7 Y=4.94115 Z=0.2+DOVETAILSHAPEOFFS  382
C    ET ALPHAZ=90 <===                                                    382
         CALL AGSRESET                                                    382
         AG_X = -0.7                                                      382
         AG_Y = 4.94115                                                   382
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   382
         AG_ALPHAN=AG_ALPHAN+1                                            382
         AG_ALPHAK(AG_ALPHAN)=3                                           382
         AG_ALPHAL(AG_ALPHAN)=90                                          382
         AG_TITLE = 'PTME'                                                382
         AG_EXNAME = 'POSITION'                                           382
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      382
         CALL AXPOSITION                                                  382
C    *                                                                    382
C    Create PTMF :                                                        383
C    Call To Local Procedure PTMF                                         383
         ASSIGN 6350 TO BLOCK_PTMF                                        383
         GO TO  6340                                                      383
6350     CONTINUE                                                         383
C    *                                                                    383
C    Geant POSITION : PTMF ===> X=-2.3932 Y=4.48487 Z=0.2+DOVETAILSHAPEO  384
C    FFSET ALPHAZ=126 <===                                                384
         CALL AGSRESET                                                    384
         AG_X = -2.3932                                                   384
         AG_Y = 4.48487                                                   384
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   384
         AG_ALPHAN=AG_ALPHAN+1                                            384
         AG_ALPHAK(AG_ALPHAN)=3                                           384
         AG_ALPHAL(AG_ALPHAN)=126                                         384
         AG_TITLE = 'PTMF'                                                384
         AG_EXNAME = 'POSITION'                                           384
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      384
         CALL AXPOSITION                                                  384
C    *                                                                    384
C    Create PTLA :                                                        385
C    Call To Local Procedure PTLA                                         385
         ASSIGN 6370 TO BLOCK_PTLA                                        385
         GO TO  6360                                                      385
6370     CONTINUE                                                         385
C    *                                                                    385
C    Geant POSITION : PTLA ===> X=-0.769273 Y=7.31915 Z=-0.85+DOVETAILSH  386
C    APEOFFSET ALPHAZ=-174 <===                                           386
         CALL AGSRESET                                                    386
         AG_X = -0.769273                                                 386
         AG_Y = 7.31915                                                   386
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 386
         AG_ALPHAN=AG_ALPHAN+1                                            386
         AG_ALPHAK(AG_ALPHAN)=3                                           386
         AG_ALPHAL(AG_ALPHAN)=-174                                        386
         AG_TITLE = 'PTLA'                                                386
         AG_EXNAME = 'POSITION'                                           386
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      386
         CALL AXPOSITION                                                  386
C    *                                                                    386
C    Create PTLB :                                                        387
C    Call To Local Procedure PTLB                                         387
         ASSIGN 6390 TO BLOCK_PTLB                                        387
         GO TO  6380                                                      387
6390     CONTINUE                                                         387
C    *                                                                    387
C    Geant POSITION : PTLB ===> X=-2.2742 Y=6.99926 Z=-0.85+DOVETAILSHAP  388
C    EOFFSET ALPHAZ=-162 <===                                             388
         CALL AGSRESET                                                    388
         AG_X = -2.2742                                                   388
         AG_Y = 6.99926                                                   388
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 388
         AG_ALPHAN=AG_ALPHAN+1                                            388
         AG_ALPHAK(AG_ALPHAN)=3                                           388
         AG_ALPHAL(AG_ALPHAN)=-162                                        388
         AG_TITLE = 'PTLB'                                                388
         AG_EXNAME = 'POSITION'                                           388
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      388
         CALL AXPOSITION                                                  388
C    *                                                                    388
C    Create PTLC :                                                        389
C    Call To Local Procedure PTLC                                         389
         ASSIGN 6410 TO BLOCK_PTLC                                        389
         GO TO  6400                                                      389
6410     CONTINUE                                                         389
C    *                                                                    389
C    Geant POSITION : PTLC ===> X=-3.67973 Y=6.37348 Z=-0.85+DOVETAILSHA  390
C    PEOFFSET ALPHAZ=-150 <===                                            390
         CALL AGSRESET                                                    390
         AG_X = -3.67973                                                  390
         AG_Y = 6.37348                                                   390
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 390
         AG_ALPHAN=AG_ALPHAN+1                                            390
         AG_ALPHAK(AG_ALPHAN)=3                                           390
         AG_ALPHAL(AG_ALPHAN)=-150                                        390
         AG_TITLE = 'PTLC'                                                390
         AG_EXNAME = 'POSITION'                                           390
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      390
         CALL AXPOSITION                                                  390
C    *                                                                    390
C    Create PTLD :                                                        391
C    Call To Local Procedure PTLD                                         391
         ASSIGN 6430 TO BLOCK_PTLD                                        391
         GO TO  6420                                                      391
6430     CONTINUE                                                         391
C    *                                                                    391
C    Geant POSITION : PTLD ===> X=-0.322263 Y=3.06613 Z=-0.85+DOVETAILSH  392
C    APEOFFSET ALPHAZ=-174 <===                                           392
         CALL AGSRESET                                                    392
         AG_X = -0.322263                                                 392
         AG_Y = 3.06613                                                   392
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 392
         AG_ALPHAN=AG_ALPHAN+1                                            392
         AG_ALPHAK(AG_ALPHAN)=3                                           392
         AG_ALPHAL(AG_ALPHAN)=-174                                        392
         AG_TITLE = 'PTLD'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      392
         CALL AXPOSITION                                                  392
C    *                                                                    392
C    Create PTLE :                                                        393
C    Call To Local Procedure PTLE                                         393
         ASSIGN 6450 TO BLOCK_PTLE                                        393
         GO TO  6440                                                      393
6450     CONTINUE                                                         393
C    *                                                                    393
C    Geant POSITION : PTLE ===> X=-0.952705 Y=2.93212 Z=-0.85+DOVETAILSH  394
C    APEOFFSET ALPHAZ=-162 <===                                           394
         CALL AGSRESET                                                    394
         AG_X = -0.952705                                                 394
         AG_Y = 2.93212                                                   394
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 394
         AG_ALPHAN=AG_ALPHAN+1                                            394
         AG_ALPHAK(AG_ALPHAN)=3                                           394
         AG_ALPHAL(AG_ALPHAN)=-162                                        394
         AG_TITLE = 'PTLE'                                                394
         AG_EXNAME = 'POSITION'                                           394
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      394
         CALL AXPOSITION                                                  394
C    *                                                                    394
C    Create PTLF :                                                        395
C    Call To Local Procedure PTLF                                         395
         ASSIGN 6470 TO BLOCK_PTLF                                        395
         GO TO  6460                                                      395
6470     CONTINUE                                                         395
C    *                                                                    395
C    Geant POSITION : PTLF ===> X=-1.54151 Y=2.66997 Z=-0.85+DOVETAILSHA  396
C    PEOFFSET ALPHAZ=-150 <===                                            396
         CALL AGSRESET                                                    396
         AG_X = -1.54151                                                  396
         AG_Y = 2.66997                                                   396
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 396
         AG_ALPHAN=AG_ALPHAN+1                                            396
         AG_ALPHAK(AG_ALPHAN)=3                                           396
         AG_ALPHAL(AG_ALPHAN)=-150                                        396
         AG_TITLE = 'PTLF'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Create PTLG :                                                        397
C    Call To Local Procedure PTLG                                         397
         ASSIGN 6490 TO BLOCK_PTLG                                        397
         GO TO  6480                                                      397
6490     CONTINUE                                                         397
C    *                                                                    397
C    Geant POSITION : PTLG ===> X=-0.375 Y=5.31059 Z=-0.85+DOVETAILSHAPE  398
C    OFFSET ALPHAZ=-174 <===                                              398
         CALL AGSRESET                                                    398
         AG_X = -0.375                                                    398
         AG_Y = 5.31059                                                   398
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 398
         AG_ALPHAN=AG_ALPHAN+1                                            398
         AG_ALPHAK(AG_ALPHAN)=3                                           398
         AG_ALPHAL(AG_ALPHAN)=-174                                        398
         AG_TITLE = 'PTLG'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create PTLH :                                                        399
C    Call To Local Procedure PTLH                                         399
         ASSIGN 6510 TO BLOCK_PTLH                                        399
         GO TO  6500                                                      399
6510     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : PTLH ===> X=-2.8181 Y=4.51677 Z=-0.85+DOVETAILSHAP  400
C    EOFFSET ALPHAZ=-150 <===                                             400
         CALL AGSRESET                                                    400
         AG_X = -2.8181                                                   400
         AG_Y = 4.51677                                                   400
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 400
         AG_ALPHAN=AG_ALPHAN+1                                            400
         AG_ALPHAK(AG_ALPHAN)=3                                           400
         AG_ALPHAL(AG_ALPHAN)=-150                                        400
         AG_TITLE = 'PTLH'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Create PTLI :                                                        401
C    Call To Local Procedure PTLI                                         401
         ASSIGN 6530 TO BLOCK_PTLI                                        401
         GO TO  6520                                                      401
6530     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : PTLI ===> X=-0.375 Y=5.11059 Z=-0.6+DOVETAILSHAPEO  402
C    FFSET ALPHAZ=-174 <===                                               402
         CALL AGSRESET                                                    402
         AG_X = -0.375                                                    402
         AG_Y = 5.11059                                                   402
         AG_Z = -0.6+DOVETAILSHAPEOFFSET                                  402
         AG_ALPHAN=AG_ALPHAN+1                                            402
         AG_ALPHAK(AG_ALPHAN)=3                                           402
         AG_ALPHAL(AG_ALPHAN)=-174                                        402
         AG_TITLE = 'PTLI'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Create PTLJ :                                                        403
C    Call To Local Procedure PTLJ                                         403
         ASSIGN 6550 TO BLOCK_PTLJ                                        403
         GO TO  6540                                                      403
6550     CONTINUE                                                         403
C    *                                                                    403
C    Geant POSITION : PTLJ ===> X=-2.70055 Y=4.35497 Z=-0.6+DOVETAILSHAP  404
C    EOFFSET ALPHAZ=-150 <===                                             404
         CALL AGSRESET                                                    404
         AG_X = -2.70055                                                  404
         AG_Y = 4.35497                                                   404
         AG_Z = -0.6+DOVETAILSHAPEOFFSET                                  404
         AG_ALPHAN=AG_ALPHAN+1                                            404
         AG_ALPHAK(AG_ALPHAN)=3                                           404
         AG_ALPHAL(AG_ALPHAN)=-150                                        404
         AG_TITLE = 'PTLJ'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      404
         CALL AXPOSITION                                                  404
C    *                                                                    404
C    Create BHCA :                                                        405
C    Call To Local Procedure BHCA                                         405
         ASSIGN 6570 TO BLOCK_BHCA                                        405
         GO TO  6560                                                      405
6570     CONTINUE                                                         405
C    *                                                                    405
C    Geant POSITION : BHCA ===> X=-0.25 Y=2.90971 Z=BULLHORNZOFFSET <===  406
         CALL AGSRESET                                                    406
         AG_X = -0.25                                                     406
         AG_Y = 2.90971                                                   406
         AG_Z = BULLHORNZOFFSET                                           406
         AG_TITLE = 'BHCA'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'X_Y_Z'                                             406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Create BHCB :                                                        407
C    Call To Local Procedure BHCB                                         407
         ASSIGN 6590 TO BLOCK_BHCB                                        407
         GO TO  6580                                                      407
6590     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : BHCB ===> X=-0.25 Y=7.78014 Z=BULLHORNZOFFSET <===  408
         CALL AGSRESET                                                    408
         AG_X = -0.25                                                     408
         AG_Y = 7.78014                                                   408
         AG_Z = BULLHORNZOFFSET                                           408
         AG_TITLE = 'BHCB'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z'                                             408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Create BHCC :                                                        409
C    Call To Local Procedure BHCC                                         409
         ASSIGN 6610 TO BLOCK_BHCC                                        409
         GO TO  6600                                                      409
6610     CONTINUE                                                         409
C    *                                                                    409
C    Geant POSITION : BHCC ===> X=-1.482 Y=7.9982 Z=BULLHORNZOFFSET <===  410
         CALL AGSRESET                                                    410
         AG_X = -1.482                                                    410
         AG_Y = 7.9982                                                    410
         AG_Z = BULLHORNZOFFSET                                           410
         AG_TITLE = 'BHCC'                                                410
         AG_EXNAME = 'POSITION'                                           410
         AG_PARLIST = 'X_Y_Z'                                             410
         CALL AXPOSITION                                                  410
C    *                                                                    410
C    Create BHCD :                                                        411
C    Call To Local Procedure BHCD                                         411
         ASSIGN 6630 TO BLOCK_BHCD                                        411
         GO TO  6620                                                      411
6630     CONTINUE                                                         411
C    *                                                                    411
C    Geant POSITION : BHCD ===> X=-1.76032 Y=7.86181 Z=BULLHORNZOFFSET <  412
C    ===                                                                  412
         CALL AGSRESET                                                    412
         AG_X = -1.76032                                                  412
         AG_Y = 7.86181                                                   412
         AG_Z = BULLHORNZOFFSET                                           412
         AG_TITLE = 'BHCD'                                                412
         AG_EXNAME = 'POSITION'                                           412
         AG_PARLIST = 'X_Y_Z'                                             412
         CALL AXPOSITION                                                  412
C    *                                                                    412
C    Create BHCE :                                                        413
C    Call To Local Procedure BHCE                                         413
         ASSIGN 6650 TO BLOCK_BHCE                                        413
         GO TO  6640                                                      413
6650     CONTINUE                                                         413
C    *                                                                    413
C    Geant POSITION : BHCE ===> X=-3.11253 Y=7.51529 Z=BULLHORNZOFFSET <  414
C    ===                                                                  414
         CALL AGSRESET                                                    414
         AG_X = -3.11253                                                  414
         AG_Y = 7.51529                                                   414
         AG_Z = BULLHORNZOFFSET                                           414
         AG_TITLE = 'BHCE'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'X_Y_Z'                                             414
         CALL AXPOSITION                                                  414
C    *                                                                    414
C    Create BHCF :                                                        415
C    Call To Local Procedure BHCF                                         415
         ASSIGN 6670 TO BLOCK_BHCF                                        415
         GO TO  6660                                                      415
6670     CONTINUE                                                         415
C    *                                                                    415
C    Geant POSITION : BHCF ===> X=-3.35642 Y=7.32402 Z=BULLHORNZOFFSET <  416
C    ===                                                                  416
         CALL AGSRESET                                                    416
         AG_X = -3.35642                                                  416
         AG_Y = 7.32402                                                   416
         AG_Z = BULLHORNZOFFSET                                           416
         AG_TITLE = 'BHCF'                                                416
         AG_EXNAME = 'POSITION'                                           416
         AG_PARLIST = 'X_Y_Z'                                             416
         CALL AXPOSITION                                                  416
C    *                                                                    416
C    Create BHCG :                                                        417
C    Call To Local Procedure BHCG                                         417
         ASSIGN 6690 TO BLOCK_BHCG                                        417
         GO TO  6680                                                      417
6690     CONTINUE                                                         417
C    *                                                                    417
C    Geant POSITION : BHCG ===> X=-4.56862 Y=6.71349 Z=BULLHORNZOFFSET <  418
C    ===                                                                  418
         CALL AGSRESET                                                    418
         AG_X = -4.56862                                                  418
         AG_Y = 6.71349                                                   418
         AG_Z = BULLHORNZOFFSET                                           418
         AG_TITLE = 'BHCG'                                                418
         AG_EXNAME = 'POSITION'                                           418
         AG_PARLIST = 'X_Y_Z'                                             418
         CALL AXPOSITION                                                  418
C    *                                                                    418
C    Create BHCH :                                                        419
C    Call To Local Procedure BHCH                                         419
         ASSIGN 6710 TO BLOCK_BHCH                                        419
         GO TO  6700                                                      419
6710     CONTINUE                                                         419
C    *                                                                    419
C    Geant POSITION : BHCH ===> X=-1.90073 Y=3.04145 Z=BULLHORNZOFFSET <  420
C    ===                                                                  420
         CALL AGSRESET                                                    420
         AG_X = -1.90073                                                  420
         AG_Y = 3.04145                                                   420
         AG_Z = BULLHORNZOFFSET                                           420
         AG_TITLE = 'BHCH'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'X_Y_Z'                                             420
         CALL AXPOSITION                                                  420
C    *                                                                    420
C    Create BHSA :                                                        421
C    Call To Local Procedure BHSA                                         421
         ASSIGN 6730 TO BLOCK_BHSA                                        421
         GO TO  6720                                                      421
6730     CONTINUE                                                         421
C    *                                                                    421
C    Geant POSITION : BHSA ===> X=-0.177 Y=5.34493 Z=BULLHORNZOFFSET ALP  422
C    HAZ=90 <===                                                          422
         CALL AGSRESET                                                    422
         AG_X = -0.177                                                    422
         AG_Y = 5.34493                                                   422
         AG_Z = BULLHORNZOFFSET                                           422
         AG_ALPHAN=AG_ALPHAN+1                                            422
         AG_ALPHAK(AG_ALPHAN)=3                                           422
         AG_ALPHAL(AG_ALPHAN)=90                                          422
         AG_TITLE = 'BHSA'                                                422
         AG_EXNAME = 'POSITION'                                           422
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      422
         CALL AXPOSITION                                                  422
C    *                                                                    422
C    Create BHSB :                                                        423
C    Call To Local Procedure BHSB                                         423
         ASSIGN 6750 TO BLOCK_BHSB                                        423
         GO TO  6740                                                      423
6750     CONTINUE                                                         423
C    *                                                                    423
C    Geant POSITION : BHSB ===> X=-0.853278 Y=7.96105 Z=BULLHORNZOFFSET   424
C    ALPHAZ=169.963 <===                                                  424
         CALL AGSRESET                                                    424
         AG_X = -0.853278                                                 424
         AG_Y = 7.96105                                                   424
         AG_Z = BULLHORNZOFFSET                                           424
         AG_ALPHAN=AG_ALPHAN+1                                            424
         AG_ALPHAK(AG_ALPHAN)=3                                           424
         AG_ALPHAL(AG_ALPHAN)=169.963                                     424
         AG_TITLE = 'BHSB'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Create BHSC :                                                        425
C    Call To Local Procedure BHSC                                         425
         ASSIGN 6770 TO BLOCK_BHSC                                        425
         GO TO  6760                                                      425
6770     CONTINUE                                                         425
C    *                                                                    425
C    Geant POSITION : BHSC ===> X=-1.54572 Y=7.94543 Z=BULLHORNZOFFSET A  426
C    LPHAZ=281.554 <===                                                   426
         CALL AGSRESET                                                    426
         AG_X = -1.54572                                                  426
         AG_Y = 7.94543                                                   426
         AG_Z = BULLHORNZOFFSET                                           426
         AG_ALPHAN=AG_ALPHAN+1                                            426
         AG_ALPHAK(AG_ALPHAN)=3                                           426
         AG_ALPHAL(AG_ALPHAN)=281.554                                     426
         AG_TITLE = 'BHSC'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Create BHSD :                                                        427
C    Call To Local Procedure BHSD                                         427
         ASSIGN 6790 TO BLOCK_BHSD                                        427
         GO TO  6780                                                      427
6790     CONTINUE                                                         427
C    *                                                                    427
C    Geant POSITION : BHSD ===> X=-2.43379 Y=7.6116 Z=BULLHORNZOFFSET AL  428
C    PHAZ=181.963 <===                                                    428
         CALL AGSRESET                                                    428
         AG_X = -2.43379                                                  428
         AG_Y = 7.6116                                                    428
         AG_Z = BULLHORNZOFFSET                                           428
         AG_ALPHAN=AG_ALPHAN+1                                            428
         AG_ALPHAK(AG_ALPHAN)=3                                           428
         AG_ALPHAL(AG_ALPHAN)=181.963                                     428
         AG_TITLE = 'BHSD'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Create BHSE :                                                        429
C    Call To Local Procedure BHSE                                         429
         ASSIGN 6810 TO BLOCK_BHSE                                        429
         GO TO  6800                                                      429
6810     CONTINUE                                                         429
C    *                                                                    429
C    Geant POSITION : BHSE ===> X=-3.16389 Y=7.45043 Z=BULLHORNZOFFSET A  430
C    LPHAZ=293.554 <===                                                   430
         CALL AGSRESET                                                    430
         AG_X = -3.16389                                                  430
         AG_Y = 7.45043                                                   430
         AG_Z = BULLHORNZOFFSET                                           430
         AG_ALPHAN=AG_ALPHAN+1                                            430
         AG_ALPHAK(AG_ALPHAN)=3                                           430
         AG_ALPHAL(AG_ALPHAN)=293.554                                     430
         AG_TITLE = 'BHSE'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Create BHSF :                                                        431
C    Call To Local Procedure BHSF                                         431
         ASSIGN 6830 TO BLOCK_BHSF                                        431
         GO TO  6820                                                      431
6830     CONTINUE                                                         431
C    *                                                                    431
C    Geant POSITION : BHSF ===> X=-3.94394 Y=6.94403 Z=BULLHORNZOFFSET A  432
C    LPHAZ=193.963 <===                                                   432
         CALL AGSRESET                                                    432
         AG_X = -3.94394                                                  432
         AG_Y = 6.94403                                                   432
         AG_Z = BULLHORNZOFFSET                                           432
         AG_ALPHAN=AG_ALPHAN+1                                            432
         AG_ALPHAK(AG_ALPHAN)=3                                           432
         AG_ALPHAL(AG_ALPHAN)=193.963                                     432
         AG_TITLE = 'BHSF'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      432
         CALL AXPOSITION                                                  432
C    *                                                                    432
C    Create BHSG :                                                        433
C    Call To Local Procedure BHSG                                         433
         ASSIGN 6850 TO BLOCK_BHSG                                        433
         GO TO  6840                                                      433
6850     CONTINUE                                                         433
C    *                                                                    433
C    Geant POSITION : BHSG ===> X=-3.29373 Y=4.83456 Z=BULLHORNZOFFSET A  434
C    LPHAZ=306 <===                                                       434
         CALL AGSRESET                                                    434
         AG_X = -3.29373                                                  434
         AG_Y = 4.83456                                                   434
         AG_Z = BULLHORNZOFFSET                                           434
         AG_ALPHAN=AG_ALPHAN+1                                            434
         AG_ALPHAK(AG_ALPHAN)=3                                           434
         AG_ALPHAL(AG_ALPHAN)=306                                         434
         AG_TITLE = 'BHSG'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      434
         CALL AXPOSITION                                                  434
C    *                                                                    434
C    Create BHSH :                                                        435
C    Call To Local Procedure BHSH                                         435
         ASSIGN 6870 TO BLOCK_BHSH                                        435
         GO TO  6860                                                      435
6870     CONTINUE                                                         435
C    *                                                                    435
C    Geant POSITION : BHSH ===> X=-1.08117 Y=2.90281 Z=BULLHORNZOFFSET A  436
C    LPHAZ=355.437 <===                                                   436
         CALL AGSRESET                                                    436
         AG_X = -1.08117                                                  436
         AG_Y = 2.90281                                                   436
         AG_Z = BULLHORNZOFFSET                                           436
         AG_ALPHAN=AG_ALPHAN+1                                            436
         AG_ALPHAK(AG_ALPHAN)=3                                           436
         AG_ALPHAL(AG_ALPHAN)=355.437                                     436
         AG_TITLE = 'BHSH'                                                436
         AG_EXNAME = 'POSITION'                                           436
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      436
         CALL AXPOSITION                                                  436
C    *                                                                    436
C    Create PTOB :                                                        437
C    Call To Local Procedure PTOB                                         437
         ASSIGN 6890 TO BLOCK_PTOB                                        437
         GO TO  6880                                                      437
6890     CONTINUE                                                         437
C    *                                                                    437
C    Geant POSITION : PTOB ===> X=-0.3264 Y=5.3446 Z=14.2125 <===         438
         CALL AGSRESET                                                    438
         AG_X = -0.3264                                                   438
         AG_Y = 5.3446                                                    438
         AG_Z = 14.2125                                                   438
         AG_TITLE = 'PTOB'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'X_Y_Z'                                             438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Create LADR :                                                        439
C    Call To Local Procedure LADR                                         439
         ASSIGN 6910 TO BLOCK_LADR                                        439
         GO TO  6900                                                      439
6910     CONTINUE                                                         439
C    *                                                                    439
C    Geant POSITION : LADR ===> X=-0.73444 Y=2.74331 Z=-4.875 ALPHAZ=175  440
C    .437 <===                                                            440
         CALL AGSRESET                                                    440
         AG_X = -0.73444                                                  440
         AG_Y = 2.74331                                                   440
         AG_Z = -4.875                                                    440
         AG_ALPHAN=AG_ALPHAN+1                                            440
         AG_ALPHAK(AG_ALPHAN)=3                                           440
         AG_ALPHAL(AG_ALPHAN)=175.437                                     440
         AG_TITLE = 'LADR'                                                440
         AG_EXNAME = 'POSITION'                                           440
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      440
         CALL AXPOSITION                                                  440
C    *                                                                    440
C    Create LADR :                                                        441
C    Call To Local Procedure LADR                                         441
         ASSIGN 6920 TO BLOCK_LADR                                        441
         GO TO  6900                                                      441
6920     CONTINUE                                                         441
C    *                                                                    441
C    Geant POSITION : LADR ===> X=-1.30523 Y=8.1723 Z=-4.875 ALPHAZ=349.  442
C    963 <===                                                             442
         CALL AGSRESET                                                    442
         AG_X = -1.30523                                                  442
         AG_Y = 8.1723                                                    442
         AG_Z = -4.875                                                    442
         AG_ALPHAN=AG_ALPHAN+1                                            442
         AG_ALPHAK(AG_ALPHAN)=3                                           442
         AG_ALPHAL(AG_ALPHAN)=349.963                                     442
         AG_TITLE = 'LADR'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      442
         CALL AXPOSITION                                                  442
C    *                                                                    442
C    Create LADR :                                                        443
C    Call To Local Procedure LADR                                         443
         ASSIGN 6930 TO BLOCK_LADR                                        443
         GO TO  6900                                                      443
6930     CONTINUE                                                         443
C    *                                                                    443
C    Geant POSITION : LADR ===> X=-2.97583 Y=7.72235 Z=-4.875 ALPHAZ=1.9  444
C    63 <===                                                              444
         CALL AGSRESET                                                    444
         AG_X = -2.97583                                                  444
         AG_Y = 7.72235                                                   444
         AG_Z = -4.875                                                    444
         AG_ALPHAN=AG_ALPHAN+1                                            444
         AG_ALPHAK(AG_ALPHAN)=3                                           444
         AG_ALPHAL(AG_ALPHAN)=1.963                                       444
         AG_TITLE = 'LADR'                                                444
         AG_EXNAME = 'POSITION'                                           444
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      444
         CALL AXPOSITION                                                  444
C    *                                                                    444
C    Create LADR :                                                        445
C    Call To Local Procedure LADR                                         445
         ASSIGN 6940 TO BLOCK_LADR                                        445
         GO TO  6900                                                      445
6940     CONTINUE                                                         445
C    *                                                                    445
C    Geant POSITION : LADR ===> X=-4.51636 Y=6.93489 Z=-4.875 ALPHAZ=13.  446
C    963 <===                                                             446
         CALL AGSRESET                                                    446
         AG_X = -4.51636                                                  446
         AG_Y = 6.93489                                                   446
         AG_Z = -4.875                                                    446
         AG_ALPHAN=AG_ALPHAN+1                                            446
         AG_ALPHAK(AG_ALPHAN)=3                                           446
         AG_ALPHAL(AG_ALPHAN)=13.963                                      446
         AG_TITLE = 'LADR'                                                446
         AG_EXNAME = 'POSITION'                                           446
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      446
         CALL AXPOSITION                                                  446
C    *                                                                    446
C    *                                                                    446
         LL=1                                                             447
            GO TO 6953                                                    447
6951        LL=LL+1                                                       447
6953        IF(LL-(LCSD_NUMBEROFCABLES).GT.0) GO TO 6952                  447
C       Create PCAD :                                                     448
C       Call To Local Procedure PCAD                                      448
            ASSIGN 6970 TO BLOCK_PCAD                                     448
            GO TO  6960                                                   448
6970        CONTINUE                                                      448
C       *                                                                 448
C       Geant POSITION : PCAD ===> X=LCSD_XPOS(LL) Y=LCSD_YPOS(LL) Z=POS  449
C       CABL <===                                                         449
            CALL AGSRESET                                                 449
            AG_X = LCSD_XPOS(LL)                                          449
            AG_Y = LCSD_YPOS(LL)                                          449
            AG_Z = POSCABL                                                449
            AG_TITLE = 'PCAD'                                             449
            AG_EXNAME = 'POSITION'                                        449
            AG_PARLIST = 'X_Y_Z'                                          449
            CALL AXPOSITION                                               449
C       *                                                                 449
         GO TO 6951                                                       450
6952     CONTINUE                                                         450
5161  CALL AGSPOPB                                                        451
      GO TO BLOCK_PXLA, (5160 ,5170)                                      451
C End of Local Procedure                                                  451
C ----------------------------------------------------------------------  452
C ------------ PXCA                                                       452
C Begin Local Procedure PXCA                                              453
C PXCA ARC BOTTOM RIGHT                                                   453
5180  CONTINUE                                                            453
         AG_TITLE='PXCA'                                                  453
         AG_MODULE=MODULE                                                 453
         AG_MARK='V00'                                                    453
         CALL AGSPUSH                                                     453
         AG_MODULE=MODULE                                                 453
C    *                                                                    453
C    Geant ATTRIBUTE : PXCA ===> SEEN=1 COLO=410 <===                     454
         CALL AGSRESET                                                    454
         AG_SEEN = 1                                                      454
         AG_COLO = 410                                                    454
         AG_TITLE = 'PXCA'                                                454
         AG_EXNAME = 'ATTRIBUTE'                                          454
         AG_PARLIST = 'SEEN_COLO'                                         454
         CALL AXATTRIBUTE                                                 454
C    *                                                                    454
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          454
         CALL AGSRESET                                                    455
         AG_TITLE = 'CFRPMIX'                                             455
         AG_EXNAME = 'MATERIAL'                                           455
         AG_PARLIST = 'NONE'                                              455
         CALL AXMATERIAL                                                  455
C    *                                                                    455
C    Geant SHAPE : TUBS ===> RMIN=0.123 RMAX=0.15 PHI1=265.437, PHI2=360  455
C     DZ=SECTORLENGTH <===                                                455
         CALL AGSRESET                                                    457
         AG_RMIN = 0.123                                                  457
         AG_RMAX = 0.15                                                   457
         AG_PHI1 = 265.437                                                457
         AG_PHI2 = 360                                                    457
         AG_DZ = SECTORLENGTH                                             457
         AG_TITLE = 'TUBS'                                                457
         AG_EXNAME = 'SHAPE'                                              457
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            457
         CALL AXSHAPE                                                     457
C    *                                                                    457
5181  CALL AGSPOPB                                                        458
      GO TO BLOCK_PXCA, (5180 ,5190)                                      458
C End of Local Procedure                                                  458
C ----------------------------------------------------------------------  459
C ------------ PXCC                                                       459
C Begin Local Procedure PXCC                                              460
C PXCC ARC TOPRIGHT                                                       460
5200  CONTINUE                                                            460
         AG_TITLE='PXCC'                                                  460
         AG_MODULE=MODULE                                                 460
         AG_MARK='V00'                                                    460
         CALL AGSPUSH                                                     460
         AG_MODULE=MODULE                                                 460
C    *                                                                    460
C    Geant ATTRIBUTE : PXCC ===> SEEN=1 COLO=410 <===                     461
         CALL AGSRESET                                                    461
         AG_SEEN = 1                                                      461
         AG_COLO = 410                                                    461
         AG_TITLE = 'PXCC'                                                461
         AG_EXNAME = 'ATTRIBUTE'                                          461
         AG_PARLIST = 'SEEN_COLO'                                         461
         CALL AXATTRIBUTE                                                 461
C    *                                                                    461
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          461
         CALL AGSRESET                                                    462
         AG_TITLE = 'CFRPMIX'                                             462
         AG_EXNAME = 'MATERIAL'                                           462
         AG_PARLIST = 'NONE'                                              462
         CALL AXMATERIAL                                                  462
C    *                                                                    462
C    Geant SHAPE : TUBS ===> RMIN=0.123 RMAX=0.15 PHI1=0 PHI2=79.963, DZ  462
C    =SECTORLENGTH <===                                                   462
         CALL AGSRESET                                                    464
         AG_RMIN = 0.123                                                  464
         AG_RMAX = 0.15                                                   464
         AG_PHI1 = 0                                                      464
         AG_PHI2 = 79.963                                                 464
         AG_DZ = SECTORLENGTH                                             464
         AG_TITLE = 'TUBS'                                                464
         AG_EXNAME = 'SHAPE'                                              464
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            464
         CALL AXSHAPE                                                     464
C    *                                                                    464
5201  CALL AGSPOPB                                                        465
      GO TO BLOCK_PXCC, (5200 ,5210)                                      465
C End of Local Procedure                                                  465
C ----------------------------------------------------------------------  466
C ------------ PXCD                                                       466
C Begin Local Procedure PXCD                                              467
C PXCD ARC TOP 23                                                         467
5220  CONTINUE                                                            467
         AG_TITLE='PXCD'                                                  467
         AG_MODULE=MODULE                                                 467
         AG_MARK='V00'                                                    467
         CALL AGSPUSH                                                     467
         AG_MODULE=MODULE                                                 467
C    *                                                                    467
C    Geant ATTRIBUTE : PXCD ===> SEEN=1 COLO=410 <===                     468
         CALL AGSRESET                                                    468
         AG_SEEN = 1                                                      468
         AG_COLO = 410                                                    468
         AG_TITLE = 'PXCD'                                                468
         AG_EXNAME = 'ATTRIBUTE'                                          468
         AG_PARLIST = 'SEEN_COLO'                                         468
         CALL AXATTRIBUTE                                                 468
C    *                                                                    468
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          468
         CALL AGSRESET                                                    469
         AG_TITLE = 'CFRPMIX'                                             469
         AG_EXNAME = 'MATERIAL'                                           469
         AG_PARLIST = 'NONE'                                              469
         CALL AXMATERIAL                                                  469
C    *                                                                    469
C    Geant SHAPE : TUBS ===> RMIN=0.123 RMAX=0.15 PHI1=79.963, PHI2=191.  469
C    554 DZ=SECTORLENGTH <===                                             469
         CALL AGSRESET                                                    471
         AG_RMIN = 0.123                                                  471
         AG_RMAX = 0.15                                                   471
         AG_PHI1 = 79.963                                                 471
         AG_PHI2 = 191.554                                                471
         AG_DZ = SECTORLENGTH                                             471
         AG_TITLE = 'TUBS'                                                471
         AG_EXNAME = 'SHAPE'                                              471
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            471
         CALL AXSHAPE                                                     471
C    *                                                                    471
5221  CALL AGSPOPB                                                        472
      GO TO BLOCK_PXCD, (5220 ,5230)                                      472
C End of Local Procedure                                                  472
C ----------------------------------------------------------------------  473
C ------------ PXCE                                                       473
C Begin Local Procedure PXCE                                              474
C PXCE ARC BOTTOM 23                                                      474
5240  CONTINUE                                                            474
         AG_TITLE='PXCE'                                                  474
         AG_MODULE=MODULE                                                 474
         AG_MARK='V00'                                                    474
         CALL AGSPUSH                                                     474
         AG_MODULE=MODULE                                                 474
C    *                                                                    474
C    Geant ATTRIBUTE : PXCE ===> SEEN=1 COLO=410 <===                     475
         CALL AGSRESET                                                    475
         AG_SEEN = 1                                                      475
         AG_COLO = 410                                                    475
         AG_TITLE = 'PXCE'                                                475
         AG_EXNAME = 'ATTRIBUTE'                                          475
         AG_PARLIST = 'SEEN_COLO'                                         475
         CALL AXATTRIBUTE                                                 475
C    *                                                                    475
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          475
         CALL AGSRESET                                                    476
         AG_TITLE = 'CFRPMIX'                                             476
         AG_EXNAME = 'MATERIAL'                                           476
         AG_PARLIST = 'NONE'                                              476
         CALL AXMATERIAL                                                  476
C    *                                                                    476
C    Geant SHAPE : TUBS ===> RMIN=0.15 RMAX=0.177 PHI1=271.963, PHI2=371  476
C    .554 DZ=SECTORLENGTH <===                                            476
         CALL AGSRESET                                                    478
         AG_RMIN = 0.15                                                   478
         AG_RMAX = 0.177                                                  478
         AG_PHI1 = 271.963                                                478
         AG_PHI2 = 371.554                                                478
         AG_DZ = SECTORLENGTH                                             478
         AG_TITLE = 'TUBS'                                                478
         AG_EXNAME = 'SHAPE'                                              478
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            478
         CALL AXSHAPE                                                     478
C    *                                                                    478
5241  CALL AGSPOPB                                                        479
      GO TO BLOCK_PXCE, (5240 ,5250)                                      479
C End of Local Procedure                                                  479
C ----------------------------------------------------------------------  480
C ------------ PXCF                                                       480
C Begin Local Procedure PXCF                                              481
C PXCF ARC TOP 12                                                         481
5260  CONTINUE                                                            481
         AG_TITLE='PXCF'                                                  481
         AG_MODULE=MODULE                                                 481
         AG_MARK='V00'                                                    481
         CALL AGSPUSH                                                     481
         AG_MODULE=MODULE                                                 481
C    *                                                                    481
C    Geant ATTRIBUTE : PXCF ===> SEEN=1 COLO=410 <===                     482
         CALL AGSRESET                                                    482
         AG_SEEN = 1                                                      482
         AG_COLO = 410                                                    482
         AG_TITLE = 'PXCF'                                                482
         AG_EXNAME = 'ATTRIBUTE'                                          482
         AG_PARLIST = 'SEEN_COLO'                                         482
         CALL AXATTRIBUTE                                                 482
C    *                                                                    482
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          482
         CALL AGSRESET                                                    483
         AG_TITLE = 'CFRPMIX'                                             483
         AG_EXNAME = 'MATERIAL'                                           483
         AG_PARLIST = 'NONE'                                              483
         CALL AXMATERIAL                                                  483
C    *                                                                    483
C    Geant SHAPE : TUBS ===> RMIN=0.123 RMAX=0.15 PHI1=91.963, PHI2=203.  483
C    554 DZ=SECTORLENGTH <===                                             483
         CALL AGSRESET                                                    485
         AG_RMIN = 0.123                                                  485
         AG_RMAX = 0.15                                                   485
         AG_PHI1 = 91.963                                                 485
         AG_PHI2 = 203.554                                                485
         AG_DZ = SECTORLENGTH                                             485
         AG_TITLE = 'TUBS'                                                485
         AG_EXNAME = 'SHAPE'                                              485
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            485
         CALL AXSHAPE                                                     485
C    *                                                                    485
5261  CALL AGSPOPB                                                        486
      GO TO BLOCK_PXCF, (5260 ,5270)                                      486
C End of Local Procedure                                                  486
C ----------------------------------------------------------------------  487
C ------------ PXCG                                                       487
C Begin Local Procedure PXCG                                              488
C PXCG ARC BOTTOM 12                                                      488
5280  CONTINUE                                                            488
         AG_TITLE='PXCG'                                                  488
         AG_MODULE=MODULE                                                 488
         AG_MARK='V00'                                                    488
         CALL AGSPUSH                                                     488
         AG_MODULE=MODULE                                                 488
C    *                                                                    488
C    Geant ATTRIBUTE : PXCG ===> SEEN=1 COLO=410 <===                     489
         CALL AGSRESET                                                    489
         AG_SEEN = 1                                                      489
         AG_COLO = 410                                                    489
         AG_TITLE = 'PXCG'                                                489
         AG_EXNAME = 'ATTRIBUTE'                                          489
         AG_PARLIST = 'SEEN_COLO'                                         489
         CALL AXATTRIBUTE                                                 489
C    *                                                                    489
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          489
         CALL AGSRESET                                                    490
         AG_TITLE = 'CFRPMIX'                                             490
         AG_EXNAME = 'MATERIAL'                                           490
         AG_PARLIST = 'NONE'                                              490
         CALL AXMATERIAL                                                  490
C    *                                                                    490
C    Geant SHAPE : TUBS ===> RMIN=0.15 RMAX=0.177 PHI1=283.963, PHI2=383  490
C    .554 DZ=SECTORLENGTH <===                                            490
         CALL AGSRESET                                                    492
         AG_RMIN = 0.15                                                   492
         AG_RMAX = 0.177                                                  492
         AG_PHI1 = 283.963                                                492
         AG_PHI2 = 383.554                                                492
         AG_DZ = SECTORLENGTH                                             492
         AG_TITLE = 'TUBS'                                                492
         AG_EXNAME = 'SHAPE'                                              492
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            492
         CALL AXSHAPE                                                     492
C    *                                                                    492
5281  CALL AGSPOPB                                                        493
      GO TO BLOCK_PXCG, (5280 ,5290)                                      493
C End of Local Procedure                                                  493
C ----------------------------------------------------------------------  494
C ------------ PXCH                                                       494
C Begin Local Procedure PXCH                                              495
C PXCH ARC TOP LEFT                                                       495
5300  CONTINUE                                                            495
         AG_TITLE='PXCH'                                                  495
         AG_MODULE=MODULE                                                 495
         AG_MARK='V00'                                                    495
         CALL AGSPUSH                                                     495
         AG_MODULE=MODULE                                                 495
C    *                                                                    495
C    Geant ATTRIBUTE : PXCH ===> SEEN=1 COLO=410 <===                     496
         CALL AGSRESET                                                    496
         AG_SEEN = 1                                                      496
         AG_COLO = 410                                                    496
         AG_TITLE = 'PXCH'                                                496
         AG_EXNAME = 'ATTRIBUTE'                                          496
         AG_PARLIST = 'SEEN_COLO'                                         496
         CALL AXATTRIBUTE                                                 496
C    *                                                                    496
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          496
         CALL AGSRESET                                                    497
         AG_TITLE = 'CFRPMIX'                                             497
         AG_EXNAME = 'MATERIAL'                                           497
         AG_PARLIST = 'NONE'                                              497
         CALL AXMATERIAL                                                  497
C    *                                                                    497
C    Geant SHAPE : TUBS ===> RMIN=0.123 RMAX=0.15 PHI1=103.963, PHI2=216  497
C     DZ=SECTORLENGTH <===                                                497
         CALL AGSRESET                                                    499
         AG_RMIN = 0.123                                                  499
         AG_RMAX = 0.15                                                   499
         AG_PHI1 = 103.963                                                499
         AG_PHI2 = 216                                                    499
         AG_DZ = SECTORLENGTH                                             499
         AG_TITLE = 'TUBS'                                                499
         AG_EXNAME = 'SHAPE'                                              499
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            499
         CALL AXSHAPE                                                     499
C    *                                                                    499
5301  CALL AGSPOPB                                                        500
      GO TO BLOCK_PXCH, (5300 ,5310)                                      500
C End of Local Procedure                                                  500
C ----------------------------------------------------------------------  501
C ------------ PXCB                                                       501
C Begin Local Procedure PXCB                                              502
C PXCB ARC BOTTOM LEFT                                                    502
5320  CONTINUE                                                            502
         AG_TITLE='PXCB'                                                  502
         AG_MODULE=MODULE                                                 502
         AG_MARK='V00'                                                    502
         CALL AGSPUSH                                                     502
         AG_MODULE=MODULE                                                 502
C    *                                                                    502
C    Geant ATTRIBUTE : PXCB ===> SEEN=1 COLO=410 <===                     503
         CALL AGSRESET                                                    503
         AG_SEEN = 1                                                      503
         AG_COLO = 410                                                    503
         AG_TITLE = 'PXCB'                                                503
         AG_EXNAME = 'ATTRIBUTE'                                          503
         AG_PARLIST = 'SEEN_COLO'                                         503
         CALL AXATTRIBUTE                                                 503
C    *                                                                    503
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          503
         CALL AGSRESET                                                    504
         AG_TITLE = 'CFRPMIX'                                             504
         AG_EXNAME = 'MATERIAL'                                           504
         AG_PARLIST = 'NONE'                                              504
         CALL AXMATERIAL                                                  504
C    *                                                                    504
C    Geant SHAPE : TUBS ===> RMIN=0.123 RMAX=0.15 PHI1=216, PHI2=265.437  504
C     DZ=SECTORLENGTH <===                                                504
         CALL AGSRESET                                                    506
         AG_RMIN = 0.123                                                  506
         AG_RMAX = 0.15                                                   506
         AG_PHI1 = 216                                                    506
         AG_PHI2 = 265.437                                                506
         AG_DZ = SECTORLENGTH                                             506
         AG_TITLE = 'TUBS'                                                506
         AG_EXNAME = 'SHAPE'                                              506
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            506
         CALL AXSHAPE                                                     506
C    *                                                                    506
5321  CALL AGSPOPB                                                        507
      GO TO BLOCK_PXCB, (5320 ,5330)                                      507
C End of Local Procedure                                                  507
C ----------------------------------------------------------------------  508
C ------------ PXRB                                                       508
C Begin Local Procedure PXRB                                              509
C PXRB SECTOR RIGHT SIDE                                                  509
5340  CONTINUE                                                            509
         AG_TITLE='PXRB'                                                  509
         AG_MODULE=MODULE                                                 509
         AG_MARK='V00'                                                    509
         CALL AGSPUSH                                                     509
         AG_MODULE=MODULE                                                 509
C    *                                                                    509
C    Geant ATTRIBUTE : PXRB ===> SEEN=1 COLO=410 <===                     510
         CALL AGSRESET                                                    510
         AG_SEEN = 1                                                      510
         AG_COLO = 410                                                    510
         AG_TITLE = 'PXRB'                                                510
         AG_EXNAME = 'ATTRIBUTE'                                          510
         AG_PARLIST = 'SEEN_COLO'                                         510
         CALL AXATTRIBUTE                                                 510
C    *                                                                    510
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          510
         CALL AGSRESET                                                    511
         AG_TITLE = 'CFRPMIX'                                             511
         AG_EXNAME = 'MATERIAL'                                           511
         AG_PARLIST = 'NONE'                                              511
         CALL AXMATERIAL                                                  511
C    *                                                                    511
C    Geant SHAPE : BOX ===> DX=2.43522 DY=SECTORHALFTHICKNESS, DZ=SECTOR  511
C    LENGTH <===                                                          511
         CALL AGSRESET                                                    513
         AG_DX = 2.43522                                                  513
         AG_DY = SECTORHALFTHICKNESS                                      513
         AG_DZ = SECTORLENGTH                                             513
         AG_TITLE = 'BOX'                                                 513
         AG_EXNAME = 'SHAPE'                                              513
         AG_PARLIST = 'DX_DY_DZ'                                          513
         CALL AXSHAPE                                                     513
C    *                                                                    513
5341  CALL AGSPOPB                                                        514
      GO TO BLOCK_PXRB, (5340 ,5350)                                      514
C End of Local Procedure                                                  514
C ----------------------------------------------------------------------  515
C ------------ PXTR                                                       515
C Begin Local Procedure PXTR                                              516
C PXTR                                                                    516
5360  CONTINUE                                                            516
         AG_TITLE='PXTR'                                                  516
         AG_MODULE=MODULE                                                 516
         AG_MARK='V00'                                                    516
         CALL AGSPUSH                                                     516
         AG_MODULE=MODULE                                                 516
C    *                                                                    516
C    Geant ATTRIBUTE : PXTR ===> SEEN=1 COLO=410 <===                     517
         CALL AGSRESET                                                    517
         AG_SEEN = 1                                                      517
         AG_COLO = 410                                                    517
         AG_TITLE = 'PXTR'                                                517
         AG_EXNAME = 'ATTRIBUTE'                                          517
         AG_PARLIST = 'SEEN_COLO'                                         517
         CALL AXATTRIBUTE                                                 517
C    *                                                                    517
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          517
         CALL AGSRESET                                                    518
         AG_TITLE = 'CFRPMIX'                                             518
         AG_EXNAME = 'MATERIAL'                                           518
         AG_PARLIST = 'NONE'                                              518
         CALL AXMATERIAL                                                  518
C    *                                                                    518
C    Geant SHAPE : BOX ===> DX=0.625575 DY=SECTORHALFTHICKNESS, DZ=SECTO  518
C    RLENGTH <===                                                         518
         CALL AGSRESET                                                    520
         AG_DX = 0.625575                                                 520
         AG_DY = SECTORHALFTHICKNESS                                      520
         AG_DZ = SECTORLENGTH                                             520
         AG_TITLE = 'BOX'                                                 520
         AG_EXNAME = 'SHAPE'                                              520
         AG_PARLIST = 'DX_DY_DZ'                                          520
         CALL AXSHAPE                                                     520
C    *                                                                    520
5361  CALL AGSPOPB                                                        521
      GO TO BLOCK_PXTR, (5360 ,5370)                                      521
C End of Local Procedure                                                  521
C ----------------------------------------------------------------------  522
C ------------ PXTM                                                       522
C Begin Local Procedure PXTM                                              523
C PXTM                                                                    523
5380  CONTINUE                                                            523
         AG_TITLE='PXTM'                                                  523
         AG_MODULE=MODULE                                                 523
         AG_MARK='V00'                                                    523
         CALL AGSPUSH                                                     523
         AG_MODULE=MODULE                                                 523
C    *                                                                    523
C    Geant ATTRIBUTE : PXTM ===> SEEN=1 COLO=410 <===                     524
         CALL AGSRESET                                                    524
         AG_SEEN = 1                                                      524
         AG_COLO = 410                                                    524
         AG_TITLE = 'PXTM'                                                524
         AG_EXNAME = 'ATTRIBUTE'                                          524
         AG_PARLIST = 'SEEN_COLO'                                         524
         CALL AXATTRIBUTE                                                 524
C    *                                                                    524
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          524
         CALL AGSRESET                                                    525
         AG_TITLE = 'CFRPMIX'                                             525
         AG_EXNAME = 'MATERIAL'                                           525
         AG_PARLIST = 'NONE'                                              525
         CALL AXMATERIAL                                                  525
C    *                                                                    525
C    Geant SHAPE : BOX ===> DX=0.681643 DY=SECTORHALFTHICKNESS, DZ=SECTO  525
C    RLENGTH <===                                                         525
         CALL AGSRESET                                                    527
         AG_DX = 0.681643                                                 527
         AG_DY = SECTORHALFTHICKNESS                                      527
         AG_DZ = SECTORLENGTH                                             527
         AG_TITLE = 'BOX'                                                 527
         AG_EXNAME = 'SHAPE'                                              527
         AG_PARLIST = 'DX_DY_DZ'                                          527
         CALL AXSHAPE                                                     527
C    *                                                                    527
5381  CALL AGSPOPB                                                        528
      GO TO BLOCK_PXTM, (5380 ,5390)                                      528
C End of Local Procedure                                                  528
C ----------------------------------------------------------------------  529
C ------------ PXTL                                                       529
C Begin Local Procedure PXTL                                              530
C PXTL                                                                    530
5400  CONTINUE                                                            530
         AG_TITLE='PXTL'                                                  530
         AG_MODULE=MODULE                                                 530
         AG_MARK='V00'                                                    530
         CALL AGSPUSH                                                     530
         AG_MODULE=MODULE                                                 530
C    *                                                                    530
C    Geant ATTRIBUTE : PXTL ===> SEEN=1 COLO=410 <===                     531
         CALL AGSRESET                                                    531
         AG_SEEN = 1                                                      531
         AG_COLO = 410                                                    531
         AG_TITLE = 'PXTL'                                                531
         AG_EXNAME = 'ATTRIBUTE'                                          531
         AG_PARLIST = 'SEEN_COLO'                                         531
         CALL AXATTRIBUTE                                                 531
C    *                                                                    531
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          531
         CALL AGSRESET                                                    532
         AG_TITLE = 'CFRPMIX'                                             532
         AG_EXNAME = 'MATERIAL'                                           532
         AG_PARLIST = 'NONE'                                              532
         CALL AXMATERIAL                                                  532
C    *                                                                    532
C    Geant SHAPE : BOX ===> DX=0.661849 DY=SECTORHALFTHICKNESS, DZ=SECTO  532
C    RLENGTH <===                                                         532
         CALL AGSRESET                                                    534
         AG_DX = 0.661849                                                 534
         AG_DY = SECTORHALFTHICKNESS                                      534
         AG_DZ = SECTORLENGTH                                             534
         AG_TITLE = 'BOX'                                                 534
         AG_EXNAME = 'SHAPE'                                              534
         AG_PARLIST = 'DX_DY_DZ'                                          534
         CALL AXSHAPE                                                     534
C    *                                                                    534
5401  CALL AGSPOPB                                                        535
      GO TO BLOCK_PXTL, (5400 ,5410)                                      535
C End of Local Procedure                                                  535
C ----------------------------------------------------------------------  536
C ------------ PXTJ                                                       536
C Begin Local Procedure PXTJ                                              537
C PXTJ                                                                    537
5420  CONTINUE                                                            537
         AG_TITLE='PXTJ'                                                  537
         AG_MODULE=MODULE                                                 537
         AG_MARK='V00'                                                    537
         CALL AGSPUSH                                                     537
         AG_MODULE=MODULE                                                 537
C    *                                                                    537
C    Geant ATTRIBUTE : PXTJ ===> SEEN=1 COLO=410 <===                     538
         CALL AGSRESET                                                    538
         AG_SEEN = 1                                                      538
         AG_COLO = 410                                                    538
         AG_TITLE = 'PXTJ'                                                538
         AG_EXNAME = 'ATTRIBUTE'                                          538
         AG_PARLIST = 'SEEN_COLO'                                         538
         CALL AXATTRIBUTE                                                 538
C    *                                                                    538
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          538
         CALL AGSRESET                                                    539
         AG_TITLE = 'CFRPMIX'                                             539
         AG_EXNAME = 'MATERIAL'                                           539
         AG_PARLIST = 'NONE'                                              539
         CALL AXMATERIAL                                                  539
C    *                                                                    539
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=SECTORHALFTHICKNESS, DZ=SECT  539
C    ORLENGTH <===                                                        539
         CALL AGSRESET                                                    541
         AG_DX = 0.0389368                                                541
         AG_DY = SECTORHALFTHICKNESS                                      541
         AG_DZ = SECTORLENGTH                                             541
         AG_TITLE = 'BOX'                                                 541
         AG_EXNAME = 'SHAPE'                                              541
         AG_PARLIST = 'DX_DY_DZ'                                          541
         CALL AXSHAPE                                                     541
C    *                                                                    541
5421  CALL AGSPOPB                                                        542
      GO TO BLOCK_PXTJ, (5420 ,5430)                                      542
C End of Local Procedure                                                  542
C ----------------------------------------------------------------------  543
C ------------ PXTI                                                       543
C Begin Local Procedure PXTI                                              544
C PXTI                                                                    544
5440  CONTINUE                                                            544
         AG_TITLE='PXTI'                                                  544
         AG_MODULE=MODULE                                                 544
         AG_MARK='V00'                                                    544
         CALL AGSPUSH                                                     544
         AG_MODULE=MODULE                                                 544
C    *                                                                    544
C    Geant ATTRIBUTE : PXTI ===> SEEN=1 COLO=410 <===                     545
         CALL AGSRESET                                                    545
         AG_SEEN = 1                                                      545
         AG_COLO = 410                                                    545
         AG_TITLE = 'PXTI'                                                545
         AG_EXNAME = 'ATTRIBUTE'                                          545
         AG_PARLIST = 'SEEN_COLO'                                         545
         CALL AXATTRIBUTE                                                 545
C    *                                                                    545
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          545
         CALL AGSRESET                                                    546
         AG_TITLE = 'CFRPMIX'                                             546
         AG_EXNAME = 'MATERIAL'                                           546
         AG_PARLIST = 'NONE'                                              546
         CALL AXMATERIAL                                                  546
C    *                                                                    546
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=SECTORHALFTHICKNESS, DZ=SECT  546
C    ORLENGTH <===                                                        546
         CALL AGSRESET                                                    548
         AG_DX = 0.0389368                                                548
         AG_DY = SECTORHALFTHICKNESS                                      548
         AG_DZ = SECTORLENGTH                                             548
         AG_TITLE = 'BOX'                                                 548
         AG_EXNAME = 'SHAPE'                                              548
         AG_PARLIST = 'DX_DY_DZ'                                          548
         CALL AXSHAPE                                                     548
C    *                                                                    548
5441  CALL AGSPOPB                                                        549
      GO TO BLOCK_PXTI, (5440 ,5450)                                      549
C End of Local Procedure                                                  549
C ----------------------------------------------------------------------  550
C ------------ PXLB                                                       550
C Begin Local Procedure PXLB                                              551
C PXLB SECTOR LEFT SIDE                                                   551
5460  CONTINUE                                                            551
         AG_TITLE='PXLB'                                                  551
         AG_MODULE=MODULE                                                 551
         AG_MARK='V00'                                                    551
         CALL AGSPUSH                                                     551
         AG_MODULE=MODULE                                                 551
C    *                                                                    551
C    Geant ATTRIBUTE : PXLB ===> SEEN=1 COLO=410 <===                     552
         CALL AGSRESET                                                    552
         AG_SEEN = 1                                                      552
         AG_COLO = 410                                                    552
         AG_TITLE = 'PXLB'                                                552
         AG_EXNAME = 'ATTRIBUTE'                                          552
         AG_PARLIST = 'SEEN_COLO'                                         552
         CALL AXATTRIBUTE                                                 552
C    *                                                                    552
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          552
         CALL AGSRESET                                                    553
         AG_TITLE = 'CFRPMIX'                                             553
         AG_EXNAME = 'MATERIAL'                                           553
         AG_PARLIST = 'NONE'                                              553
         CALL AXMATERIAL                                                  553
C    *                                                                    553
C    Geant SHAPE : BOX ===> DX=2.26944 DY=SECTORHALFTHICKNESS, DZ=SECTOR  553
C    LENGTH <===                                                          553
         CALL AGSRESET                                                    555
         AG_DX = 2.26944                                                  555
         AG_DY = SECTORHALFTHICKNESS                                      555
         AG_DZ = SECTORLENGTH                                             555
         AG_TITLE = 'BOX'                                                 555
         AG_EXNAME = 'SHAPE'                                              555
         AG_PARLIST = 'DX_DY_DZ'                                          555
         CALL AXSHAPE                                                     555
C    *                                                                    555
5461  CALL AGSPOPB                                                        556
      GO TO BLOCK_PXLB, (5460 ,5470)                                      556
C End of Local Procedure                                                  556
C ----------------------------------------------------------------------  557
C ------------ PXIB                                                       557
C Begin Local Procedure PXIB                                              558
C PXIB SECTOR INNER SIDE                                                  558
5480  CONTINUE                                                            558
         AG_TITLE='PXIB'                                                  558
         AG_MODULE=MODULE                                                 558
         AG_MARK='V00'                                                    558
         CALL AGSPUSH                                                     558
         AG_MODULE=MODULE                                                 558
C    *                                                                    558
C    Geant ATTRIBUTE : PXIB ===> SEEN=1 COLO=410 <===                     559
         CALL AGSRESET                                                    559
         AG_SEEN = 1                                                      559
         AG_COLO = 410                                                    559
         AG_TITLE = 'PXIB'                                                559
         AG_EXNAME = 'ATTRIBUTE'                                          559
         AG_PARLIST = 'SEEN_COLO'                                         559
         CALL AXATTRIBUTE                                                 559
C    *                                                                    559
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          559
         CALL AGSRESET                                                    560
         AG_TITLE = 'CFRPMIX'                                             560
         AG_EXNAME = 'MATERIAL'                                           560
         AG_PARLIST = 'NONE'                                              560
         CALL AXMATERIAL                                                  560
C    *                                                                    560
C    Geant SHAPE : BOX ===> DX=0.827988 DY=SECTORHALFTHICKNESS, DZ=SECTO  560
C    RLENGTH <===                                                         560
         CALL AGSRESET                                                    562
         AG_DX = 0.827988                                                 562
         AG_DY = SECTORHALFTHICKNESS                                      562
         AG_DZ = SECTORLENGTH                                             562
         AG_TITLE = 'BOX'                                                 562
         AG_EXNAME = 'SHAPE'                                              562
         AG_PARLIST = 'DX_DY_DZ'                                          562
         CALL AXSHAPE                                                     562
C    *                                                                    562
5481  CALL AGSPOPB                                                        563
      GO TO BLOCK_PXIB, (5480 ,5490)                                      563
C End of Local Procedure                                                  563
C ----------------------------------------------------------------------  564
C ------------ DTCA                                                       564
C Begin Local Procedure DTCA                                              565
C DTCA                                                                    565
5500  CONTINUE                                                            565
         AG_TITLE='DTCA'                                                  565
         AG_MODULE=MODULE                                                 565
         AG_MARK='V00'                                                    565
         CALL AGSPUSH                                                     565
         AG_MODULE=MODULE                                                 565
C    *                                                                    565
C    Geant ATTRIBUTE : DTCA ===> SEEN=1 COLO=920 <===                     566
         CALL AGSRESET                                                    566
         AG_SEEN = 1                                                      566
         AG_COLO = 920                                                    566
         AG_TITLE = 'DTCA'                                                566
         AG_EXNAME = 'ATTRIBUTE'                                          566
         AG_PARLIST = 'SEEN_COLO'                                         566
         CALL AXATTRIBUTE                                                 566
C    *                                                                    566
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        566
         CALL AGSRESET                                                    567
         AG_TITLE = 'ALUMINIUM'                                           567
         AG_EXNAME = 'MATERIAL'                                           567
         AG_PARLIST = 'NONE'                                              567
         CALL AXMATERIAL                                                  567
C    *                                                                    567
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=265.437, PHI2=36  567
C    0 DZ=DOVETAILLENGTH <===                                             567
         CALL AGSRESET                                                    569
         AG_RMIN = 0.023                                                  569
         AG_RMAX = 0.123                                                  569
         AG_PHI1 = 265.437                                                569
         AG_PHI2 = 360                                                    569
         AG_DZ = DOVETAILLENGTH                                           569
         AG_TITLE = 'TUBS'                                                569
         AG_EXNAME = 'SHAPE'                                              569
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            569
         CALL AXSHAPE                                                     569
C    *                                                                    569
5501  CALL AGSPOPB                                                        570
      GO TO BLOCK_DTCA, (5500 ,5510)                                      570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ DTCB                                                       571
C Begin Local Procedure DTCB                                              572
C DTCB                                                                    572
5520  CONTINUE                                                            572
         AG_TITLE='DTCB'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant ATTRIBUTE : DTCB ===> SEEN=1 COLO=920 <===                     573
         CALL AGSRESET                                                    573
         AG_SEEN = 1                                                      573
         AG_COLO = 920                                                    573
         AG_TITLE = 'DTCB'                                                573
         AG_EXNAME = 'ATTRIBUTE'                                          573
         AG_PARLIST = 'SEEN_COLO'                                         573
         CALL AXATTRIBUTE                                                 573
C    *                                                                    573
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        573
         CALL AGSRESET                                                    574
         AG_TITLE = 'ALUMINIUM'                                           574
         AG_EXNAME = 'MATERIAL'                                           574
         AG_PARLIST = 'NONE'                                              574
         CALL AXMATERIAL                                                  574
C    *                                                                    574
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=0, PHI2=79.963 D  574
C    Z=DOVETAILLENGTH <===                                                574
         CALL AGSRESET                                                    576
         AG_RMIN = 0.023                                                  576
         AG_RMAX = 0.123                                                  576
         AG_PHI1 = 0                                                      576
         AG_PHI2 = 79.963                                                 576
         AG_DZ = DOVETAILLENGTH                                           576
         AG_TITLE = 'TUBS'                                                576
         AG_EXNAME = 'SHAPE'                                              576
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            576
         CALL AXSHAPE                                                     576
C    *                                                                    576
5521  CALL AGSPOPB                                                        577
      GO TO BLOCK_DTCB, (5520 ,5530)                                      577
C End of Local Procedure                                                  577
C ----------------------------------------------------------------------  578
C ------------ DTCC                                                       578
C Begin Local Procedure DTCC                                              579
C DTCC                                                                    579
5540  CONTINUE                                                            579
         AG_TITLE='DTCC'                                                  579
         AG_MODULE=MODULE                                                 579
         AG_MARK='V00'                                                    579
         CALL AGSPUSH                                                     579
         AG_MODULE=MODULE                                                 579
C    *                                                                    579
C    Geant ATTRIBUTE : DTCC ===> SEEN=1 COLO=920 <===                     580
         CALL AGSRESET                                                    580
         AG_SEEN = 1                                                      580
         AG_COLO = 920                                                    580
         AG_TITLE = 'DTCC'                                                580
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
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=79.963, PHI2=191  581
C    .554 DZ=DOVETAILLENGTH <===                                          581
         CALL AGSRESET                                                    583
         AG_RMIN = 0.023                                                  583
         AG_RMAX = 0.123                                                  583
         AG_PHI1 = 79.963                                                 583
         AG_PHI2 = 191.554                                                583
         AG_DZ = DOVETAILLENGTH                                           583
         AG_TITLE = 'TUBS'                                                583
         AG_EXNAME = 'SHAPE'                                              583
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            583
         CALL AXSHAPE                                                     583
C    *                                                                    583
5541  CALL AGSPOPB                                                        584
      GO TO BLOCK_DTCC, (5540 ,5550)                                      584
C End of Local Procedure                                                  584
C ----------------------------------------------------------------------  585
C ------------ DTCD                                                       585
C Begin Local Procedure DTCD                                              586
C DTCD                                                                    586
5560  CONTINUE                                                            586
         AG_TITLE='DTCD'                                                  586
         AG_MODULE=MODULE                                                 586
         AG_MARK='V00'                                                    586
         CALL AGSPUSH                                                     586
         AG_MODULE=MODULE                                                 586
C    *                                                                    586
C    Geant ATTRIBUTE : DTCD ===> SEEN=1 COLO=920 <===                     587
         CALL AGSRESET                                                    587
         AG_SEEN = 1                                                      587
         AG_COLO = 920                                                    587
         AG_TITLE = 'DTCD'                                                587
         AG_EXNAME = 'ATTRIBUTE'                                          587
         AG_PARLIST = 'SEEN_COLO'                                         587
         CALL AXATTRIBUTE                                                 587
C    *                                                                    587
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        587
         CALL AGSRESET                                                    588
         AG_TITLE = 'ALUMINIUM'                                           588
         AG_EXNAME = 'MATERIAL'                                           588
         AG_PARLIST = 'NONE'                                              588
         CALL AXMATERIAL                                                  588
C    *                                                                    588
C    Geant SHAPE : TUBS ===> RMIN=0.177 RMAX=0.277 PHI1=271.963, PHI2=37  588
C    1.554 DZ=DOVETAILLENGTH <===                                         588
         CALL AGSRESET                                                    590
         AG_RMIN = 0.177                                                  590
         AG_RMAX = 0.277                                                  590
         AG_PHI1 = 271.963                                                590
         AG_PHI2 = 371.554                                                590
         AG_DZ = DOVETAILLENGTH                                           590
         AG_TITLE = 'TUBS'                                                590
         AG_EXNAME = 'SHAPE'                                              590
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            590
         CALL AXSHAPE                                                     590
C    *                                                                    590
5561  CALL AGSPOPB                                                        591
      GO TO BLOCK_DTCD, (5560 ,5570)                                      591
C End of Local Procedure                                                  591
C ----------------------------------------------------------------------  592
C ------------ DTCE                                                       592
C Begin Local Procedure DTCE                                              593
C DTCE                                                                    593
5580  CONTINUE                                                            593
         AG_TITLE='DTCE'                                                  593
         AG_MODULE=MODULE                                                 593
         AG_MARK='V00'                                                    593
         CALL AGSPUSH                                                     593
         AG_MODULE=MODULE                                                 593
C    *                                                                    593
C    Geant ATTRIBUTE : DTCE ===> SEEN=1 COLO=920 <===                     594
         CALL AGSRESET                                                    594
         AG_SEEN = 1                                                      594
         AG_COLO = 920                                                    594
         AG_TITLE = 'DTCE'                                                594
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
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=91.963, PHI2=203  595
C    .554 DZ=DOVETAILLENGTH <===                                          595
         CALL AGSRESET                                                    597
         AG_RMIN = 0.023                                                  597
         AG_RMAX = 0.123                                                  597
         AG_PHI1 = 91.963                                                 597
         AG_PHI2 = 203.554                                                597
         AG_DZ = DOVETAILLENGTH                                           597
         AG_TITLE = 'TUBS'                                                597
         AG_EXNAME = 'SHAPE'                                              597
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            597
         CALL AXSHAPE                                                     597
C    *                                                                    597
5581  CALL AGSPOPB                                                        598
      GO TO BLOCK_DTCE, (5580 ,5590)                                      598
C End of Local Procedure                                                  598
C ----------------------------------------------------------------------  599
C ------------ DTCF                                                       599
C Begin Local Procedure DTCF                                              600
C DTCF                                                                    600
5600  CONTINUE                                                            600
         AG_TITLE='DTCF'                                                  600
         AG_MODULE=MODULE                                                 600
         AG_MARK='V00'                                                    600
         CALL AGSPUSH                                                     600
         AG_MODULE=MODULE                                                 600
C    *                                                                    600
C    Geant ATTRIBUTE : DTCF ===> SEEN=1 COLO=920 <===                     601
         CALL AGSRESET                                                    601
         AG_SEEN = 1                                                      601
         AG_COLO = 920                                                    601
         AG_TITLE = 'DTCF'                                                601
         AG_EXNAME = 'ATTRIBUTE'                                          601
         AG_PARLIST = 'SEEN_COLO'                                         601
         CALL AXATTRIBUTE                                                 601
C    *                                                                    601
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        601
         CALL AGSRESET                                                    602
         AG_TITLE = 'ALUMINIUM'                                           602
         AG_EXNAME = 'MATERIAL'                                           602
         AG_PARLIST = 'NONE'                                              602
         CALL AXMATERIAL                                                  602
C    *                                                                    602
C    Geant SHAPE : TUBS ===> RMIN=0.177 RMAX=0.277 PHI1=283.963, PHI2=38  602
C    3.554 DZ=DOVETAILLENGTH <===                                         602
         CALL AGSRESET                                                    604
         AG_RMIN = 0.177                                                  604
         AG_RMAX = 0.277                                                  604
         AG_PHI1 = 283.963                                                604
         AG_PHI2 = 383.554                                                604
         AG_DZ = DOVETAILLENGTH                                           604
         AG_TITLE = 'TUBS'                                                604
         AG_EXNAME = 'SHAPE'                                              604
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            604
         CALL AXSHAPE                                                     604
C    *                                                                    604
5601  CALL AGSPOPB                                                        605
      GO TO BLOCK_DTCF, (5600 ,5610)                                      605
C End of Local Procedure                                                  605
C ----------------------------------------------------------------------  606
C ------------ DTCG                                                       606
C Begin Local Procedure DTCG                                              607
C DTCG                                                                    607
5620  CONTINUE                                                            607
         AG_TITLE='DTCG'                                                  607
         AG_MODULE=MODULE                                                 607
         AG_MARK='V00'                                                    607
         CALL AGSPUSH                                                     607
         AG_MODULE=MODULE                                                 607
C    *                                                                    607
C    Geant ATTRIBUTE : DTCG ===> SEEN=1 COLO=920 <===                     608
         CALL AGSRESET                                                    608
         AG_SEEN = 1                                                      608
         AG_COLO = 920                                                    608
         AG_TITLE = 'DTCG'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO'                                         608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        608
         CALL AGSRESET                                                    609
         AG_TITLE = 'ALUMINIUM'                                           609
         AG_EXNAME = 'MATERIAL'                                           609
         AG_PARLIST = 'NONE'                                              609
         CALL AXMATERIAL                                                  609
C    *                                                                    609
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=103.963, PHI2=21  609
C    6 DZ=DOVETAILLENGTH <===                                             609
         CALL AGSRESET                                                    611
         AG_RMIN = 0.023                                                  611
         AG_RMAX = 0.123                                                  611
         AG_PHI1 = 103.963                                                611
         AG_PHI2 = 216                                                    611
         AG_DZ = DOVETAILLENGTH                                           611
         AG_TITLE = 'TUBS'                                                611
         AG_EXNAME = 'SHAPE'                                              611
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            611
         CALL AXSHAPE                                                     611
C    *                                                                    611
5621  CALL AGSPOPB                                                        612
      GO TO BLOCK_DTCG, (5620 ,5630)                                      612
C End of Local Procedure                                                  612
C ----------------------------------------------------------------------  613
C ------------ DTCH                                                       613
C Begin Local Procedure DTCH                                              614
C DTCH                                                                    614
5640  CONTINUE                                                            614
         AG_TITLE='DTCH'                                                  614
         AG_MODULE=MODULE                                                 614
         AG_MARK='V00'                                                    614
         CALL AGSPUSH                                                     614
         AG_MODULE=MODULE                                                 614
C    *                                                                    614
C    Geant ATTRIBUTE : DTCH ===> SEEN=1 COLO=920 <===                     615
         CALL AGSRESET                                                    615
         AG_SEEN = 1                                                      615
         AG_COLO = 920                                                    615
         AG_TITLE = 'DTCH'                                                615
         AG_EXNAME = 'ATTRIBUTE'                                          615
         AG_PARLIST = 'SEEN_COLO'                                         615
         CALL AXATTRIBUTE                                                 615
C    *                                                                    615
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        615
         CALL AGSRESET                                                    616
         AG_TITLE = 'ALUMINIUM'                                           616
         AG_EXNAME = 'MATERIAL'                                           616
         AG_PARLIST = 'NONE'                                              616
         CALL AXMATERIAL                                                  616
C    *                                                                    616
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=216, PHI2=265.43  616
C    7 DZ=DOVETAILLENGTH <===                                             616
         CALL AGSRESET                                                    618
         AG_RMIN = 0.023                                                  618
         AG_RMAX = 0.123                                                  618
         AG_PHI1 = 216                                                    618
         AG_PHI2 = 265.437                                                618
         AG_DZ = DOVETAILLENGTH                                           618
         AG_TITLE = 'TUBS'                                                618
         AG_EXNAME = 'SHAPE'                                              618
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            618
         CALL AXSHAPE                                                     618
C    *                                                                    618
5641  CALL AGSPOPB                                                        619
      GO TO BLOCK_DTCH, (5640 ,5650)                                      619
C End of Local Procedure                                                  619
C ----------------------------------------------------------------------  620
C ------------ DTSA                                                       620
C Begin Local Procedure DTSA                                              621
C DTSA DOVETAIL SECTOR RIGHT SIDE                                         621
5660  CONTINUE                                                            621
         AG_TITLE='DTSA'                                                  621
         AG_MODULE=MODULE                                                 621
         AG_MARK='V00'                                                    621
         CALL AGSPUSH                                                     621
         AG_MODULE=MODULE                                                 621
C    *                                                                    621
C    Geant ATTRIBUTE : DTSA ===> SEEN=1 COLO=920 <===                     622
         CALL AGSRESET                                                    622
         AG_SEEN = 1                                                      622
         AG_COLO = 920                                                    622
         AG_TITLE = 'DTSA'                                                622
         AG_EXNAME = 'ATTRIBUTE'                                          622
         AG_PARLIST = 'SEEN_COLO'                                         622
         CALL AXATTRIBUTE                                                 622
C    *                                                                    622
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        622
         CALL AGSRESET                                                    623
         AG_TITLE = 'ALUMINIUM'                                           623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant SHAPE : BOX ===> DX=2.43522 DY=DOVETAILHALFTHICKNESS, DZ=DOVE  623
C    TAILLENGTH <===                                                      623
         CALL AGSRESET                                                    625
         AG_DX = 2.43522                                                  625
         AG_DY = DOVETAILHALFTHICKNESS                                    625
         AG_DZ = DOVETAILLENGTH                                           625
         AG_TITLE = 'BOX'                                                 625
         AG_EXNAME = 'SHAPE'                                              625
         AG_PARLIST = 'DX_DY_DZ'                                          625
         CALL AXSHAPE                                                     625
C    *                                                                    625
5661  CALL AGSPOPB                                                        626
      GO TO BLOCK_DTSA, (5660 ,5670)                                      626
C End of Local Procedure                                                  626
C ----------------------------------------------------------------------  627
C ------------ DTSB                                                       627
C Begin Local Procedure DTSB                                              628
C DTSB DOVETAIL SECTOR RIGHT UPPER SIDE                                   628
5680  CONTINUE                                                            628
         AG_TITLE='DTSB'                                                  628
         AG_MODULE=MODULE                                                 628
         AG_MARK='V00'                                                    628
         CALL AGSPUSH                                                     628
         AG_MODULE=MODULE                                                 628
C    *                                                                    628
C    Geant ATTRIBUTE : DTSB ===> SEEN=1 COLO=920 <===                     629
         CALL AGSRESET                                                    629
         AG_SEEN = 1                                                      629
         AG_COLO = 920                                                    629
         AG_TITLE = 'DTSB'                                                629
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
C    Geant SHAPE : BOX ===> DX=0.625575 DY=DOVETAILHALFTHICKNESS, DZ=DOV  630
C    ETAILLENGTH <===                                                     630
         CALL AGSRESET                                                    632
         AG_DX = 0.625575                                                 632
         AG_DY = DOVETAILHALFTHICKNESS                                    632
         AG_DZ = DOVETAILLENGTH                                           632
         AG_TITLE = 'BOX'                                                 632
         AG_EXNAME = 'SHAPE'                                              632
         AG_PARLIST = 'DX_DY_DZ'                                          632
         CALL AXSHAPE                                                     632
C    *                                                                    632
5681  CALL AGSPOPB                                                        633
      GO TO BLOCK_DTSB, (5680 ,5690)                                      633
C End of Local Procedure                                                  633
C ----------------------------------------------------------------------  634
C ------------ DTSC                                                       634
C Begin Local Procedure DTSC                                              635
C DTSC DOVETAIL SECTOR RIGHT UPPER SIDE                                   635
5700  CONTINUE                                                            635
         AG_TITLE='DTSC'                                                  635
         AG_MODULE=MODULE                                                 635
         AG_MARK='V00'                                                    635
         CALL AGSPUSH                                                     635
         AG_MODULE=MODULE                                                 635
C    *                                                                    635
C    Geant ATTRIBUTE : DTSC ===> SEEN=1 COLO=920 <===                     636
         CALL AGSRESET                                                    636
         AG_SEEN = 1                                                      636
         AG_COLO = 920                                                    636
         AG_TITLE = 'DTSC'                                                636
         AG_EXNAME = 'ATTRIBUTE'                                          636
         AG_PARLIST = 'SEEN_COLO'                                         636
         CALL AXATTRIBUTE                                                 636
C    *                                                                    636
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        636
         CALL AGSRESET                                                    637
         AG_TITLE = 'ALUMINIUM'                                           637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILHALFTHICKNESS, DZ=DO  637
C    VETAILLENGTH <===                                                    637
         CALL AGSRESET                                                    639
         AG_DX = 0.0389368                                                639
         AG_DY = DOVETAILHALFTHICKNESS                                    639
         AG_DZ = DOVETAILLENGTH                                           639
         AG_TITLE = 'BOX'                                                 639
         AG_EXNAME = 'SHAPE'                                              639
         AG_PARLIST = 'DX_DY_DZ'                                          639
         CALL AXSHAPE                                                     639
C    *                                                                    639
5701  CALL AGSPOPB                                                        640
      GO TO BLOCK_DTSC, (5700 ,5710)                                      640
C End of Local Procedure                                                  640
C ----------------------------------------------------------------------  641
C ------------ DTSD                                                       641
C Begin Local Procedure DTSD                                              642
C DTSD DOVETAIL SECTOR RIGHT UPPER SIDE                                   642
5720  CONTINUE                                                            642
         AG_TITLE='DTSD'                                                  642
         AG_MODULE=MODULE                                                 642
         AG_MARK='V00'                                                    642
         CALL AGSPUSH                                                     642
         AG_MODULE=MODULE                                                 642
C    *                                                                    642
C    Geant ATTRIBUTE : DTSD ===> SEEN=1 COLO=920 <===                     643
         CALL AGSRESET                                                    643
         AG_SEEN = 1                                                      643
         AG_COLO = 920                                                    643
         AG_TITLE = 'DTSD'                                                643
         AG_EXNAME = 'ATTRIBUTE'                                          643
         AG_PARLIST = 'SEEN_COLO'                                         643
         CALL AXATTRIBUTE                                                 643
C    *                                                                    643
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        643
         CALL AGSRESET                                                    644
         AG_TITLE = 'ALUMINIUM'                                           644
         AG_EXNAME = 'MATERIAL'                                           644
         AG_PARLIST = 'NONE'                                              644
         CALL AXMATERIAL                                                  644
C    *                                                                    644
C    Geant SHAPE : BOX ===> DX=0.681643 DY=DOVETAILHALFTHICKNESS, DZ=DOV  644
C    ETAILLENGTH <===                                                     644
         CALL AGSRESET                                                    646
         AG_DX = 0.681643                                                 646
         AG_DY = DOVETAILHALFTHICKNESS                                    646
         AG_DZ = DOVETAILLENGTH                                           646
         AG_TITLE = 'BOX'                                                 646
         AG_EXNAME = 'SHAPE'                                              646
         AG_PARLIST = 'DX_DY_DZ'                                          646
         CALL AXSHAPE                                                     646
C    *                                                                    646
5721  CALL AGSPOPB                                                        647
      GO TO BLOCK_DTSD, (5720 ,5730)                                      647
C End of Local Procedure                                                  647
C ----------------------------------------------------------------------  648
C ------------ DTSE                                                       648
C Begin Local Procedure DTSE                                              649
C DTSE DOVETAIL SECTOR RIGHT UPPER SIDE                                   649
5740  CONTINUE                                                            649
         AG_TITLE='DTSE'                                                  649
         AG_MODULE=MODULE                                                 649
         AG_MARK='V00'                                                    649
         CALL AGSPUSH                                                     649
         AG_MODULE=MODULE                                                 649
C    *                                                                    649
C    Geant ATTRIBUTE : DTSE ===> SEEN=1 COLO=920 <===                     650
         CALL AGSRESET                                                    650
         AG_SEEN = 1                                                      650
         AG_COLO = 920                                                    650
         AG_TITLE = 'DTSE'                                                650
         AG_EXNAME = 'ATTRIBUTE'                                          650
         AG_PARLIST = 'SEEN_COLO'                                         650
         CALL AXATTRIBUTE                                                 650
C    *                                                                    650
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        650
         CALL AGSRESET                                                    651
         AG_TITLE = 'ALUMINIUM'                                           651
         AG_EXNAME = 'MATERIAL'                                           651
         AG_PARLIST = 'NONE'                                              651
         CALL AXMATERIAL                                                  651
C    *                                                                    651
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILHALFTHICKNESS, DZ=DO  651
C    VETAILLENGTH <===                                                    651
         CALL AGSRESET                                                    653
         AG_DX = 0.0389368                                                653
         AG_DY = DOVETAILHALFTHICKNESS                                    653
         AG_DZ = DOVETAILLENGTH                                           653
         AG_TITLE = 'BOX'                                                 653
         AG_EXNAME = 'SHAPE'                                              653
         AG_PARLIST = 'DX_DY_DZ'                                          653
         CALL AXSHAPE                                                     653
C    *                                                                    653
5741  CALL AGSPOPB                                                        654
      GO TO BLOCK_DTSE, (5740 ,5750)                                      654
C End of Local Procedure                                                  654
C ----------------------------------------------------------------------  655
C ------------ DTSF                                                       655
C Begin Local Procedure DTSF                                              656
C DTSF DOVETAIL SECTOR RIGHT UPPER SIDE                                   656
5760  CONTINUE                                                            656
         AG_TITLE='DTSF'                                                  656
         AG_MODULE=MODULE                                                 656
         AG_MARK='V00'                                                    656
         CALL AGSPUSH                                                     656
         AG_MODULE=MODULE                                                 656
C    *                                                                    656
C    Geant ATTRIBUTE : DTSF ===> SEEN=1 COLO=920 <===                     657
         CALL AGSRESET                                                    657
         AG_SEEN = 1                                                      657
         AG_COLO = 920                                                    657
         AG_TITLE = 'DTSF'                                                657
         AG_EXNAME = 'ATTRIBUTE'                                          657
         AG_PARLIST = 'SEEN_COLO'                                         657
         CALL AXATTRIBUTE                                                 657
C    *                                                                    657
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        657
         CALL AGSRESET                                                    658
         AG_TITLE = 'ALUMINIUM'                                           658
         AG_EXNAME = 'MATERIAL'                                           658
         AG_PARLIST = 'NONE'                                              658
         CALL AXMATERIAL                                                  658
C    *                                                                    658
C    Geant SHAPE : BOX ===> DX=0.661849 DY=DOVETAILHALFTHICKNESS, DZ=DOV  658
C    ETAILLENGTH <===                                                     658
         CALL AGSRESET                                                    660
         AG_DX = 0.661849                                                 660
         AG_DY = DOVETAILHALFTHICKNESS                                    660
         AG_DZ = DOVETAILLENGTH                                           660
         AG_TITLE = 'BOX'                                                 660
         AG_EXNAME = 'SHAPE'                                              660
         AG_PARLIST = 'DX_DY_DZ'                                          660
         CALL AXSHAPE                                                     660
C    *                                                                    660
5761  CALL AGSPOPB                                                        661
      GO TO BLOCK_DTSF, (5760 ,5770)                                      661
C End of Local Procedure                                                  661
C ----------------------------------------------------------------------  662
C ------------ DTSG                                                       662
C Begin Local Procedure DTSG                                              663
C DTSG DOVETAIL SECTOR RIGHT UPPER SIDE                                   663
5780  CONTINUE                                                            663
         AG_TITLE='DTSG'                                                  663
         AG_MODULE=MODULE                                                 663
         AG_MARK='V00'                                                    663
         CALL AGSPUSH                                                     663
         AG_MODULE=MODULE                                                 663
C    *                                                                    663
C    Geant ATTRIBUTE : DTSG ===> SEEN=1 COLO=920 <===                     664
         CALL AGSRESET                                                    664
         AG_SEEN = 1                                                      664
         AG_COLO = 920                                                    664
         AG_TITLE = 'DTSG'                                                664
         AG_EXNAME = 'ATTRIBUTE'                                          664
         AG_PARLIST = 'SEEN_COLO'                                         664
         CALL AXATTRIBUTE                                                 664
C    *                                                                    664
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        664
         CALL AGSRESET                                                    665
         AG_TITLE = 'ALUMINIUM'                                           665
         AG_EXNAME = 'MATERIAL'                                           665
         AG_PARLIST = 'NONE'                                              665
         CALL AXMATERIAL                                                  665
C    *                                                                    665
C    Geant SHAPE : BOX ===> DX=2.26944 DY=DOVETAILHALFTHICKNESS, DZ=DOVE  665
C    TAILLENGTH <===                                                      665
         CALL AGSRESET                                                    667
         AG_DX = 2.26944                                                  667
         AG_DY = DOVETAILHALFTHICKNESS                                    667
         AG_DZ = DOVETAILLENGTH                                           667
         AG_TITLE = 'BOX'                                                 667
         AG_EXNAME = 'SHAPE'                                              667
         AG_PARLIST = 'DX_DY_DZ'                                          667
         CALL AXSHAPE                                                     667
C    *                                                                    667
5781  CALL AGSPOPB                                                        668
      GO TO BLOCK_DTSG, (5780 ,5790)                                      668
C End of Local Procedure                                                  668
C ----------------------------------------------------------------------  669
C ------------ DTSH                                                       669
C Begin Local Procedure DTSH                                              670
C DTSH DOVETAIL SECTOR RIGHT UPPER SIDE                                   670
5800  CONTINUE                                                            670
         AG_TITLE='DTSH'                                                  670
         AG_MODULE=MODULE                                                 670
         AG_MARK='V00'                                                    670
         CALL AGSPUSH                                                     670
         AG_MODULE=MODULE                                                 670
C    *                                                                    670
C    Geant ATTRIBUTE : DTSH ===> SEEN=1 COLO=920 <===                     671
         CALL AGSRESET                                                    671
         AG_SEEN = 1                                                      671
         AG_COLO = 920                                                    671
         AG_TITLE = 'DTSH'                                                671
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
C    Geant SHAPE : BOX ===> DX=0.828428 DY=DOVETAILHALFTHICKNESS, DZ=DOV  672
C    ETAILLENGTH <===                                                     672
         CALL AGSRESET                                                    674
         AG_DX = 0.828428                                                 674
         AG_DY = DOVETAILHALFTHICKNESS                                    674
         AG_DZ = DOVETAILLENGTH                                           674
         AG_TITLE = 'BOX'                                                 674
         AG_EXNAME = 'SHAPE'                                              674
         AG_PARLIST = 'DX_DY_DZ'                                          674
         CALL AXSHAPE                                                     674
C    *                                                                    674
5801  CALL AGSPOPB                                                        675
      GO TO BLOCK_DTSH, (5800 ,5810)                                      675
C End of Local Procedure                                                  675
C ----------------------------------------------------------------------  676
C ------------ DTEA                                                       676
C Begin Local Procedure DTEA                                              677
C DTEA                                                                    677
5820  CONTINUE                                                            677
         AG_TITLE='DTEA'                                                  677
         AG_MODULE=MODULE                                                 677
         AG_MARK='V00'                                                    677
         CALL AGSPUSH                                                     677
         AG_MODULE=MODULE                                                 677
C    *                                                                    677
C    Geant ATTRIBUTE : DTEA ===> SEEN=1 COLO=920 <===                     678
         CALL AGSRESET                                                    678
         AG_SEEN = 1                                                      678
         AG_COLO = 920                                                    678
         AG_TITLE = 'DTEA'                                                678
         AG_EXNAME = 'ATTRIBUTE'                                          678
         AG_PARLIST = 'SEEN_COLO'                                         678
         CALL AXATTRIBUTE                                                 678
C    *                                                                    678
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        678
         CALL AGSRESET                                                    679
         AG_TITLE = 'ALUMINIUM'                                           679
         AG_EXNAME = 'MATERIAL'                                           679
         AG_PARLIST = 'NONE'                                              679
         CALL AXMATERIAL                                                  679
C    *                                                                    679
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.15 PHI1=265.437, PHI2=360  679
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     679
         CALL AGSRESET                                                    681
         AG_RMIN = 0.023                                                  681
         AG_RMAX = 0.15                                                   681
         AG_PHI1 = 265.437                                                681
         AG_PHI2 = 360                                                    681
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  681
         AG_TITLE = 'TUBS'                                                681
         AG_EXNAME = 'SHAPE'                                              681
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            681
         CALL AXSHAPE                                                     681
C    *                                                                    681
5821  CALL AGSPOPB                                                        682
      GO TO BLOCK_DTEA, (5820 ,5830)                                      682
C End of Local Procedure                                                  682
C ----------------------------------------------------------------------  683
C ------------ DTEB                                                       683
C Begin Local Procedure DTEB                                              684
C DTEB                                                                    684
5840  CONTINUE                                                            684
         AG_TITLE='DTEB'                                                  684
         AG_MODULE=MODULE                                                 684
         AG_MARK='V00'                                                    684
         CALL AGSPUSH                                                     684
         AG_MODULE=MODULE                                                 684
C    *                                                                    684
C    Geant ATTRIBUTE : DTEB ===> SEEN=1 COLO=920 <===                     685
         CALL AGSRESET                                                    685
         AG_SEEN = 1                                                      685
         AG_COLO = 920                                                    685
         AG_TITLE = 'DTEB'                                                685
         AG_EXNAME = 'ATTRIBUTE'                                          685
         AG_PARLIST = 'SEEN_COLO'                                         685
         CALL AXATTRIBUTE                                                 685
C    *                                                                    685
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        685
         CALL AGSRESET                                                    686
         AG_TITLE = 'ALUMINIUM'                                           686
         AG_EXNAME = 'MATERIAL'                                           686
         AG_PARLIST = 'NONE'                                              686
         CALL AXMATERIAL                                                  686
C    *                                                                    686
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.15 PHI1=0 PHI2=79.963, DZ  686
C    =DOVETAILEXTRUSIONLENGTH <===                                        686
         CALL AGSRESET                                                    688
         AG_RMIN = 0.023                                                  688
         AG_RMAX = 0.15                                                   688
         AG_PHI1 = 0                                                      688
         AG_PHI2 = 79.963                                                 688
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  688
         AG_TITLE = 'TUBS'                                                688
         AG_EXNAME = 'SHAPE'                                              688
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            688
         CALL AXSHAPE                                                     688
C    *                                                                    688
5841  CALL AGSPOPB                                                        689
      GO TO BLOCK_DTEB, (5840 ,5850)                                      689
C End of Local Procedure                                                  689
C ----------------------------------------------------------------------  690
C ------------ DTEC                                                       690
C Begin Local Procedure DTEC                                              691
C DTEC                                                                    691
5860  CONTINUE                                                            691
         AG_TITLE='DTEC'                                                  691
         AG_MODULE=MODULE                                                 691
         AG_MARK='V00'                                                    691
         CALL AGSPUSH                                                     691
         AG_MODULE=MODULE                                                 691
C    *                                                                    691
C    Geant ATTRIBUTE : DTEC ===> SEEN=1 COLO=920 <===                     692
         CALL AGSRESET                                                    692
         AG_SEEN = 1                                                      692
         AG_COLO = 920                                                    692
         AG_TITLE = 'DTEC'                                                692
         AG_EXNAME = 'ATTRIBUTE'                                          692
         AG_PARLIST = 'SEEN_COLO'                                         692
         CALL AXATTRIBUTE                                                 692
C    *                                                                    692
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        692
         CALL AGSRESET                                                    693
         AG_TITLE = 'ALUMINIUM'                                           693
         AG_EXNAME = 'MATERIAL'                                           693
         AG_PARLIST = 'NONE'                                              693
         CALL AXMATERIAL                                                  693
C    *                                                                    693
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.15 PHI1=79.963, PHI2=191.  693
C    554 DZ=DOVETAILEXTRUSIONLENGTH <===                                  693
         CALL AGSRESET                                                    695
         AG_RMIN = 0.023                                                  695
         AG_RMAX = 0.15                                                   695
         AG_PHI1 = 79.963                                                 695
         AG_PHI2 = 191.554                                                695
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  695
         AG_TITLE = 'TUBS'                                                695
         AG_EXNAME = 'SHAPE'                                              695
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            695
         CALL AXSHAPE                                                     695
C    *                                                                    695
5861  CALL AGSPOPB                                                        696
      GO TO BLOCK_DTEC, (5860 ,5870)                                      696
C End of Local Procedure                                                  696
C ----------------------------------------------------------------------  697
C ------------ DTED                                                       697
C Begin Local Procedure DTED                                              698
C DTED                                                                    698
5880  CONTINUE                                                            698
         AG_TITLE='DTED'                                                  698
         AG_MODULE=MODULE                                                 698
         AG_MARK='V00'                                                    698
         CALL AGSPUSH                                                     698
         AG_MODULE=MODULE                                                 698
C    *                                                                    698
C    Geant ATTRIBUTE : DTED ===> SEEN=1 COLO=920 <===                     699
         CALL AGSRESET                                                    699
         AG_SEEN = 1                                                      699
         AG_COLO = 920                                                    699
         AG_TITLE = 'DTED'                                                699
         AG_EXNAME = 'ATTRIBUTE'                                          699
         AG_PARLIST = 'SEEN_COLO'                                         699
         CALL AXATTRIBUTE                                                 699
C    *                                                                    699
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        699
         CALL AGSRESET                                                    700
         AG_TITLE = 'ALUMINIUM'                                           700
         AG_EXNAME = 'MATERIAL'                                           700
         AG_PARLIST = 'NONE'                                              700
         CALL AXMATERIAL                                                  700
C    *                                                                    700
C    Geant SHAPE : TUBS ===> RMIN=0.05 RMAX=0.177 PHI1=271.963, PHI2=371  700
C    .554 DZ=DOVETAILEXTRUSIONLENGTH <===                                 700
         CALL AGSRESET                                                    702
         AG_RMIN = 0.05                                                   702
         AG_RMAX = 0.177                                                  702
         AG_PHI1 = 271.963                                                702
         AG_PHI2 = 371.554                                                702
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  702
         AG_TITLE = 'TUBS'                                                702
         AG_EXNAME = 'SHAPE'                                              702
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            702
         CALL AXSHAPE                                                     702
C    *                                                                    702
5881  CALL AGSPOPB                                                        703
      GO TO BLOCK_DTED, (5880 ,5890)                                      703
C End of Local Procedure                                                  703
C ----------------------------------------------------------------------  704
C ------------ DTEE                                                       704
C Begin Local Procedure DTEE                                              705
C DTEE                                                                    705
5900  CONTINUE                                                            705
         AG_TITLE='DTEE'                                                  705
         AG_MODULE=MODULE                                                 705
         AG_MARK='V00'                                                    705
         CALL AGSPUSH                                                     705
         AG_MODULE=MODULE                                                 705
C    *                                                                    705
C    Geant ATTRIBUTE : DTEE ===> SEEN=1 COLO=920 <===                     706
         CALL AGSRESET                                                    706
         AG_SEEN = 1                                                      706
         AG_COLO = 920                                                    706
         AG_TITLE = 'DTEE'                                                706
         AG_EXNAME = 'ATTRIBUTE'                                          706
         AG_PARLIST = 'SEEN_COLO'                                         706
         CALL AXATTRIBUTE                                                 706
C    *                                                                    706
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        706
         CALL AGSRESET                                                    707
         AG_TITLE = 'ALUMINIUM'                                           707
         AG_EXNAME = 'MATERIAL'                                           707
         AG_PARLIST = 'NONE'                                              707
         CALL AXMATERIAL                                                  707
C    *                                                                    707
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.15 PHI1=91.963, PHI2=203.  707
C    554 DZ=DOVETAILEXTRUSIONLENGTH <===                                  707
         CALL AGSRESET                                                    709
         AG_RMIN = 0.023                                                  709
         AG_RMAX = 0.15                                                   709
         AG_PHI1 = 91.963                                                 709
         AG_PHI2 = 203.554                                                709
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  709
         AG_TITLE = 'TUBS'                                                709
         AG_EXNAME = 'SHAPE'                                              709
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            709
         CALL AXSHAPE                                                     709
C    *                                                                    709
5901  CALL AGSPOPB                                                        710
      GO TO BLOCK_DTEE, (5900 ,5910)                                      710
C End of Local Procedure                                                  710
C ----------------------------------------------------------------------  711
C ------------ DTEF                                                       711
C Begin Local Procedure DTEF                                              712
C DTEF                                                                    712
5920  CONTINUE                                                            712
         AG_TITLE='DTEF'                                                  712
         AG_MODULE=MODULE                                                 712
         AG_MARK='V00'                                                    712
         CALL AGSPUSH                                                     712
         AG_MODULE=MODULE                                                 712
C    *                                                                    712
C    Geant ATTRIBUTE : DTEF ===> SEEN=1 COLO=920 <===                     713
         CALL AGSRESET                                                    713
         AG_SEEN = 1                                                      713
         AG_COLO = 920                                                    713
         AG_TITLE = 'DTEF'                                                713
         AG_EXNAME = 'ATTRIBUTE'                                          713
         AG_PARLIST = 'SEEN_COLO'                                         713
         CALL AXATTRIBUTE                                                 713
C    *                                                                    713
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        713
         CALL AGSRESET                                                    714
         AG_TITLE = 'ALUMINIUM'                                           714
         AG_EXNAME = 'MATERIAL'                                           714
         AG_PARLIST = 'NONE'                                              714
         CALL AXMATERIAL                                                  714
C    *                                                                    714
C    Geant SHAPE : TUBS ===> RMIN=0.05 RMAX=0.177 PHI1=283.963, PHI2=383  714
C    .554 DZ=DOVETAILEXTRUSIONLENGTH <===                                 714
         CALL AGSRESET                                                    716
         AG_RMIN = 0.05                                                   716
         AG_RMAX = 0.177                                                  716
         AG_PHI1 = 283.963                                                716
         AG_PHI2 = 383.554                                                716
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  716
         AG_TITLE = 'TUBS'                                                716
         AG_EXNAME = 'SHAPE'                                              716
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            716
         CALL AXSHAPE                                                     716
C    *                                                                    716
5921  CALL AGSPOPB                                                        717
      GO TO BLOCK_DTEF, (5920 ,5930)                                      717
C End of Local Procedure                                                  717
C ----------------------------------------------------------------------  718
C ------------ DTEG                                                       718
C Begin Local Procedure DTEG                                              719
C DTEG                                                                    719
5940  CONTINUE                                                            719
         AG_TITLE='DTEG'                                                  719
         AG_MODULE=MODULE                                                 719
         AG_MARK='V00'                                                    719
         CALL AGSPUSH                                                     719
         AG_MODULE=MODULE                                                 719
C    *                                                                    719
C    Geant ATTRIBUTE : DTEG ===> SEEN=1 COLO=920 <===                     720
         CALL AGSRESET                                                    720
         AG_SEEN = 1                                                      720
         AG_COLO = 920                                                    720
         AG_TITLE = 'DTEG'                                                720
         AG_EXNAME = 'ATTRIBUTE'                                          720
         AG_PARLIST = 'SEEN_COLO'                                         720
         CALL AXATTRIBUTE                                                 720
C    *                                                                    720
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        720
         CALL AGSRESET                                                    721
         AG_TITLE = 'ALUMINIUM'                                           721
         AG_EXNAME = 'MATERIAL'                                           721
         AG_PARLIST = 'NONE'                                              721
         CALL AXMATERIAL                                                  721
C    *                                                                    721
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.15 PHI1=103.963, PHI2=216  721
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     721
         CALL AGSRESET                                                    723
         AG_RMIN = 0.023                                                  723
         AG_RMAX = 0.15                                                   723
         AG_PHI1 = 103.963                                                723
         AG_PHI2 = 216                                                    723
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  723
         AG_TITLE = 'TUBS'                                                723
         AG_EXNAME = 'SHAPE'                                              723
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            723
         CALL AXSHAPE                                                     723
C    *                                                                    723
5941  CALL AGSPOPB                                                        724
      GO TO BLOCK_DTEG, (5940 ,5950)                                      724
C End of Local Procedure                                                  724
C ----------------------------------------------------------------------  725
C ------------ DTEH                                                       725
C Begin Local Procedure DTEH                                              726
C DTEH                                                                    726
5960  CONTINUE                                                            726
         AG_TITLE='DTEH'                                                  726
         AG_MODULE=MODULE                                                 726
         AG_MARK='V00'                                                    726
         CALL AGSPUSH                                                     726
         AG_MODULE=MODULE                                                 726
C    *                                                                    726
C    Geant ATTRIBUTE : DTEH ===> SEEN=1 COLO=920 <===                     727
         CALL AGSRESET                                                    727
         AG_SEEN = 1                                                      727
         AG_COLO = 920                                                    727
         AG_TITLE = 'DTEH'                                                727
         AG_EXNAME = 'ATTRIBUTE'                                          727
         AG_PARLIST = 'SEEN_COLO'                                         727
         CALL AXATTRIBUTE                                                 727
C    *                                                                    727
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        727
         CALL AGSRESET                                                    728
         AG_TITLE = 'ALUMINIUM'                                           728
         AG_EXNAME = 'MATERIAL'                                           728
         AG_PARLIST = 'NONE'                                              728
         CALL AXMATERIAL                                                  728
C    *                                                                    728
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.15 PHI1=216, PHI2=265.437  728
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     728
         CALL AGSRESET                                                    730
         AG_RMIN = 0.023                                                  730
         AG_RMAX = 0.15                                                   730
         AG_PHI1 = 216                                                    730
         AG_PHI2 = 265.437                                                730
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  730
         AG_TITLE = 'TUBS'                                                730
         AG_EXNAME = 'SHAPE'                                              730
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            730
         CALL AXSHAPE                                                     730
C    *                                                                    730
5961  CALL AGSPOPB                                                        731
      GO TO BLOCK_DTEH, (5960 ,5970)                                      731
C End of Local Procedure                                                  731
C ----------------------------------------------------------------------  732
C ------------ DESA                                                       732
C Begin Local Procedure DESA                                              733
C DESA DOVETAIL EXTRUSION SECTOR RIGHT SIDE                               733
5980  CONTINUE                                                            733
         AG_TITLE='DESA'                                                  733
         AG_MODULE=MODULE                                                 733
         AG_MARK='V00'                                                    733
         CALL AGSPUSH                                                     733
         AG_MODULE=MODULE                                                 733
C    *                                                                    733
C    Geant ATTRIBUTE : DESA ===> SEEN=1 COLO=920 <===                     734
         CALL AGSRESET                                                    734
         AG_SEEN = 1                                                      734
         AG_COLO = 920                                                    734
         AG_TITLE = 'DESA'                                                734
         AG_EXNAME = 'ATTRIBUTE'                                          734
         AG_PARLIST = 'SEEN_COLO'                                         734
         CALL AXATTRIBUTE                                                 734
C    *                                                                    734
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        734
         CALL AGSRESET                                                    735
         AG_TITLE = 'ALUMINIUM'                                           735
         AG_EXNAME = 'MATERIAL'                                           735
         AG_PARLIST = 'NONE'                                              735
         CALL AXMATERIAL                                                  735
C    *                                                                    735
C    Geant SHAPE : BOX ===> DX=2.43522 DY=DOVETAILEXTRUSIONHALFTHICKNESS  735
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    735
         CALL AGSRESET                                                    737
         AG_DX = 2.43522                                                  737
         AG_DY = DOVETAILEXTRUSIONHALFTHICKNESS                           737
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  737
         AG_TITLE = 'BOX'                                                 737
         AG_EXNAME = 'SHAPE'                                              737
         AG_PARLIST = 'DX_DY_DZ'                                          737
         CALL AXSHAPE                                                     737
C    *                                                                    737
5981  CALL AGSPOPB                                                        738
      GO TO BLOCK_DESA, (5980 ,5990)                                      738
C End of Local Procedure                                                  738
C ----------------------------------------------------------------------  739
C ------------ DESB                                                       739
C Begin Local Procedure DESB                                              740
C DESB DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                740
6000  CONTINUE                                                            740
         AG_TITLE='DESB'                                                  740
         AG_MODULE=MODULE                                                 740
         AG_MARK='V00'                                                    740
         CALL AGSPUSH                                                     740
         AG_MODULE=MODULE                                                 740
C    *                                                                    740
C    Geant ATTRIBUTE : DESB ===> SEEN=1 COLO=920 <===                     741
         CALL AGSRESET                                                    741
         AG_SEEN = 1                                                      741
         AG_COLO = 920                                                    741
         AG_TITLE = 'DESB'                                                741
         AG_EXNAME = 'ATTRIBUTE'                                          741
         AG_PARLIST = 'SEEN_COLO'                                         741
         CALL AXATTRIBUTE                                                 741
C    *                                                                    741
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        741
         CALL AGSRESET                                                    742
         AG_TITLE = 'ALUMINIUM'                                           742
         AG_EXNAME = 'MATERIAL'                                           742
         AG_PARLIST = 'NONE'                                              742
         CALL AXMATERIAL                                                  742
C    *                                                                    742
C    Geant SHAPE : BOX ===> DX=0.625575 DY=DOVETAILEXTRUSIONTHICKNESSBOX  742
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    742
         CALL AGSRESET                                                    744
         AG_DX = 0.625575                                                 744
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            744
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  744
         AG_TITLE = 'BOX'                                                 744
         AG_EXNAME = 'SHAPE'                                              744
         AG_PARLIST = 'DX_DY_DZ'                                          744
         CALL AXSHAPE                                                     744
C    *                                                                    744
6001  CALL AGSPOPB                                                        745
      GO TO BLOCK_DESB, (6000 ,6010)                                      745
C End of Local Procedure                                                  745
C ----------------------------------------------------------------------  746
C ------------ DESC                                                       746
C Begin Local Procedure DESC                                              747
C DESC DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                747
6020  CONTINUE                                                            747
         AG_TITLE='DESC'                                                  747
         AG_MODULE=MODULE                                                 747
         AG_MARK='V00'                                                    747
         CALL AGSPUSH                                                     747
         AG_MODULE=MODULE                                                 747
C    *                                                                    747
C    Geant ATTRIBUTE : DESC ===> SEEN=1 COLO=920 <===                     748
         CALL AGSRESET                                                    748
         AG_SEEN = 1                                                      748
         AG_COLO = 920                                                    748
         AG_TITLE = 'DESC'                                                748
         AG_EXNAME = 'ATTRIBUTE'                                          748
         AG_PARLIST = 'SEEN_COLO'                                         748
         CALL AXATTRIBUTE                                                 748
C    *                                                                    748
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        748
         CALL AGSRESET                                                    749
         AG_TITLE = 'ALUMINIUM'                                           749
         AG_EXNAME = 'MATERIAL'                                           749
         AG_PARLIST = 'NONE'                                              749
         CALL AXMATERIAL                                                  749
C    *                                                                    749
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILEXTRUSIONTHICKNESSBO  749
C    X, DZ=DOVETAILEXTRUSIONLENGTH <===                                   749
         CALL AGSRESET                                                    751
         AG_DX = 0.0389368                                                751
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            751
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  751
         AG_TITLE = 'BOX'                                                 751
         AG_EXNAME = 'SHAPE'                                              751
         AG_PARLIST = 'DX_DY_DZ'                                          751
         CALL AXSHAPE                                                     751
C    *                                                                    751
6021  CALL AGSPOPB                                                        752
      GO TO BLOCK_DESC, (6020 ,6030)                                      752
C End of Local Procedure                                                  752
C ----------------------------------------------------------------------  753
C ------------ DESD                                                       753
C Begin Local Procedure DESD                                              754
C DESD DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                754
6040  CONTINUE                                                            754
         AG_TITLE='DESD'                                                  754
         AG_MODULE=MODULE                                                 754
         AG_MARK='V00'                                                    754
         CALL AGSPUSH                                                     754
         AG_MODULE=MODULE                                                 754
C    *                                                                    754
C    Geant ATTRIBUTE : DESD ===> SEEN=1 COLO=920 <===                     755
         CALL AGSRESET                                                    755
         AG_SEEN = 1                                                      755
         AG_COLO = 920                                                    755
         AG_TITLE = 'DESD'                                                755
         AG_EXNAME = 'ATTRIBUTE'                                          755
         AG_PARLIST = 'SEEN_COLO'                                         755
         CALL AXATTRIBUTE                                                 755
C    *                                                                    755
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        755
         CALL AGSRESET                                                    756
         AG_TITLE = 'ALUMINIUM'                                           756
         AG_EXNAME = 'MATERIAL'                                           756
         AG_PARLIST = 'NONE'                                              756
         CALL AXMATERIAL                                                  756
C    *                                                                    756
C    Geant SHAPE : BOX ===> DX=0.681643 DY=DOVETAILEXTRUSIONTHICKNESSBOX  756
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    756
         CALL AGSRESET                                                    758
         AG_DX = 0.681643                                                 758
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            758
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  758
         AG_TITLE = 'BOX'                                                 758
         AG_EXNAME = 'SHAPE'                                              758
         AG_PARLIST = 'DX_DY_DZ'                                          758
         CALL AXSHAPE                                                     758
C    *                                                                    758
6041  CALL AGSPOPB                                                        759
      GO TO BLOCK_DESD, (6040 ,6050)                                      759
C End of Local Procedure                                                  759
C ----------------------------------------------------------------------  760
C ------------ DESE                                                       760
C Begin Local Procedure DESE                                              761
C DESE DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                761
6060  CONTINUE                                                            761
         AG_TITLE='DESE'                                                  761
         AG_MODULE=MODULE                                                 761
         AG_MARK='V00'                                                    761
         CALL AGSPUSH                                                     761
         AG_MODULE=MODULE                                                 761
C    *                                                                    761
C    Geant ATTRIBUTE : DESE ===> SEEN=1 COLO=920 <===                     762
         CALL AGSRESET                                                    762
         AG_SEEN = 1                                                      762
         AG_COLO = 920                                                    762
         AG_TITLE = 'DESE'                                                762
         AG_EXNAME = 'ATTRIBUTE'                                          762
         AG_PARLIST = 'SEEN_COLO'                                         762
         CALL AXATTRIBUTE                                                 762
C    *                                                                    762
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        762
         CALL AGSRESET                                                    763
         AG_TITLE = 'ALUMINIUM'                                           763
         AG_EXNAME = 'MATERIAL'                                           763
         AG_PARLIST = 'NONE'                                              763
         CALL AXMATERIAL                                                  763
C    *                                                                    763
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILEXTRUSIONTHICKNESSBO  763
C    X, DZ=DOVETAILEXTRUSIONLENGTH <===                                   763
         CALL AGSRESET                                                    765
         AG_DX = 0.0389368                                                765
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            765
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  765
         AG_TITLE = 'BOX'                                                 765
         AG_EXNAME = 'SHAPE'                                              765
         AG_PARLIST = 'DX_DY_DZ'                                          765
         CALL AXSHAPE                                                     765
C    *                                                                    765
6061  CALL AGSPOPB                                                        766
      GO TO BLOCK_DESE, (6060 ,6070)                                      766
C End of Local Procedure                                                  766
C ----------------------------------------------------------------------  767
C ------------ DESF                                                       767
C Begin Local Procedure DESF                                              768
C DESF DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                768
6080  CONTINUE                                                            768
         AG_TITLE='DESF'                                                  768
         AG_MODULE=MODULE                                                 768
         AG_MARK='V00'                                                    768
         CALL AGSPUSH                                                     768
         AG_MODULE=MODULE                                                 768
C    *                                                                    768
C    Geant ATTRIBUTE : DESF ===> SEEN=1 COLO=920 <===                     769
         CALL AGSRESET                                                    769
         AG_SEEN = 1                                                      769
         AG_COLO = 920                                                    769
         AG_TITLE = 'DESF'                                                769
         AG_EXNAME = 'ATTRIBUTE'                                          769
         AG_PARLIST = 'SEEN_COLO'                                         769
         CALL AXATTRIBUTE                                                 769
C    *                                                                    769
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        769
         CALL AGSRESET                                                    770
         AG_TITLE = 'ALUMINIUM'                                           770
         AG_EXNAME = 'MATERIAL'                                           770
         AG_PARLIST = 'NONE'                                              770
         CALL AXMATERIAL                                                  770
C    *                                                                    770
C    Geant SHAPE : BOX ===> DX=0.661849 DY=DOVETAILEXTRUSIONTHICKNESSBOX  770
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    770
         CALL AGSRESET                                                    772
         AG_DX = 0.661849                                                 772
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            772
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  772
         AG_TITLE = 'BOX'                                                 772
         AG_EXNAME = 'SHAPE'                                              772
         AG_PARLIST = 'DX_DY_DZ'                                          772
         CALL AXSHAPE                                                     772
C    *                                                                    772
6081  CALL AGSPOPB                                                        773
      GO TO BLOCK_DESF, (6080 ,6090)                                      773
C End of Local Procedure                                                  773
C ----------------------------------------------------------------------  774
C ------------ DESG                                                       774
C Begin Local Procedure DESG                                              775
C DESG DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                775
6100  CONTINUE                                                            775
         AG_TITLE='DESG'                                                  775
         AG_MODULE=MODULE                                                 775
         AG_MARK='V00'                                                    775
         CALL AGSPUSH                                                     775
         AG_MODULE=MODULE                                                 775
C    *                                                                    775
C    Geant ATTRIBUTE : DESG ===> SEEN=1 COLO=920 <===                     776
         CALL AGSRESET                                                    776
         AG_SEEN = 1                                                      776
         AG_COLO = 920                                                    776
         AG_TITLE = 'DESG'                                                776
         AG_EXNAME = 'ATTRIBUTE'                                          776
         AG_PARLIST = 'SEEN_COLO'                                         776
         CALL AXATTRIBUTE                                                 776
C    *                                                                    776
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        776
         CALL AGSRESET                                                    777
         AG_TITLE = 'ALUMINIUM'                                           777
         AG_EXNAME = 'MATERIAL'                                           777
         AG_PARLIST = 'NONE'                                              777
         CALL AXMATERIAL                                                  777
C    *                                                                    777
C    Geant SHAPE : BOX ===> DX=2.26944 DY=DOVETAILEXTRUSIONTHICKNESSBOX,  777
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     777
         CALL AGSRESET                                                    779
         AG_DX = 2.26944                                                  779
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            779
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  779
         AG_TITLE = 'BOX'                                                 779
         AG_EXNAME = 'SHAPE'                                              779
         AG_PARLIST = 'DX_DY_DZ'                                          779
         CALL AXSHAPE                                                     779
C    *                                                                    779
6101  CALL AGSPOPB                                                        780
      GO TO BLOCK_DESG, (6100 ,6110)                                      780
C End of Local Procedure                                                  780
C ----------------------------------------------------------------------  781
C ------------ DESH                                                       781
C Begin Local Procedure DESH                                              782
C DESH DOVETAIL EXTRUSION SECTOR RIGHT UPPER SIDE                         782
6120  CONTINUE                                                            782
         AG_TITLE='DESH'                                                  782
         AG_MODULE=MODULE                                                 782
         AG_MARK='V00'                                                    782
         CALL AGSPUSH                                                     782
         AG_MODULE=MODULE                                                 782
C    *                                                                    782
C    Geant ATTRIBUTE : DESH ===> SEEN=1 COLO=920 <===                     783
         CALL AGSRESET                                                    783
         AG_SEEN = 1                                                      783
         AG_COLO = 920                                                    783
         AG_TITLE = 'DESH'                                                783
         AG_EXNAME = 'ATTRIBUTE'                                          783
         AG_PARLIST = 'SEEN_COLO'                                         783
         CALL AXATTRIBUTE                                                 783
C    *                                                                    783
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        783
         CALL AGSRESET                                                    784
         AG_TITLE = 'ALUMINIUM'                                           784
         AG_EXNAME = 'MATERIAL'                                           784
         AG_PARLIST = 'NONE'                                              784
         CALL AXMATERIAL                                                  784
C    *                                                                    784
C    Geant SHAPE : BOX ===> DX=0.827988 DY=DOVETAILEXTRUSIONTHICKNESSBOX  784
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    784
         CALL AGSRESET                                                    786
         AG_DX = 0.827988                                                 786
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            786
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  786
         AG_TITLE = 'BOX'                                                 786
         AG_EXNAME = 'SHAPE'                                              786
         AG_PARLIST = 'DX_DY_DZ'                                          786
         CALL AXSHAPE                                                     786
C    *                                                                    786
6121  CALL AGSPOPB                                                        787
      GO TO BLOCK_DESH, (6120 ,6130)                                      787
C End of Local Procedure                                                  787
C ----------------------------------------------------------------------  788
C ------------ PTRA                                                       788
C Begin Local Procedure PTRA                                              789
C PTRA PTR0 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    789
6140  CONTINUE                                                            789
         AG_TITLE='PTRA'                                                  789
         AG_MODULE=MODULE                                                 789
         AG_MARK='V00'                                                    789
         CALL AGSPUSH                                                     789
         AG_MODULE=MODULE                                                 789
C    *                                                                    789
C    Geant ATTRIBUTE : PTRA ===> SEEN=1 COLO=920 <===                     790
         CALL AGSRESET                                                    790
         AG_SEEN = 1                                                      790
         AG_COLO = 920                                                    790
         AG_TITLE = 'PTRA'                                                790
         AG_EXNAME = 'ATTRIBUTE'                                          790
         AG_PARLIST = 'SEEN_COLO'                                         790
         CALL AXATTRIBUTE                                                 790
C    *                                                                    790
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        790
         CALL AGSRESET                                                    791
         AG_TITLE = 'ALUMINIUM'                                           791
         AG_EXNAME = 'MATERIAL'                                           791
         AG_PARLIST = 'NONE'                                              791
         CALL AXMATERIAL                                                  791
C    *                                                                    791
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.719  791
C    003 TL1=0.677192 ALP1=-5.08889E-14, H2=0.198904 BL2=0.719003 TL2=0.  791
C    677192 ALP2=-5.08889E-14 <===                                        791
         CALL AGSRESET                                                    794
         AG_DZ = 0.05                                                     794
         AG_THET = 0                                                      794
         AG_PHI = 0                                                       794
         AG_H1 = 0.198904                                                 794
         AG_BL1 = 0.719003                                                794
         AG_TL1 = 0.677192                                                794
         AG_ALP1 = -5.08889E-14                                           794
         AG_H2 = 0.198904                                                 794
         AG_BL2 = 0.719003                                                794
         AG_TL2 = 0.677192                                                794
         AG_ALP2 = -5.08889E-14                                           794
         AG_TITLE = 'TRAP'                                                794
         AG_EXNAME = 'SHAPE'                                              794
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       794
         CALL AXSHAPE                                                     794
C    *                                                                    794
6141  CALL AGSPOPB                                                        795
      GO TO BLOCK_PTRA, (6140 ,6150)                                      795
C End of Local Procedure                                                  795
C ----------------------------------------------------------------------  796
C ------------ PTRB                                                       796
C Begin Local Procedure PTRB                                              797
C PTRB PTR1 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    797
6160  CONTINUE                                                            797
         AG_TITLE='PTRB'                                                  797
         AG_MODULE=MODULE                                                 797
         AG_MARK='V00'                                                    797
         CALL AGSPUSH                                                     797
         AG_MODULE=MODULE                                                 797
C    *                                                                    797
C    Geant ATTRIBUTE : PTRB ===> SEEN=1 COLO=920 <===                     798
         CALL AGSRESET                                                    798
         AG_SEEN = 1                                                      798
         AG_COLO = 920                                                    798
         AG_TITLE = 'PTRB'                                                798
         AG_EXNAME = 'ATTRIBUTE'                                          798
         AG_PARLIST = 'SEEN_COLO'                                         798
         CALL AXATTRIBUTE                                                 798
C    *                                                                    798
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        798
         CALL AGSRESET                                                    799
         AG_TITLE = 'ALUMINIUM'                                           799
         AG_EXNAME = 'MATERIAL'                                           799
         AG_PARLIST = 'NONE'                                              799
         CALL AXMATERIAL                                                  799
C    *                                                                    799
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  799
C    416 TL1=0.752605 ALP1=-5.08889E-14, H2=0.198904 BL2=0.794416 TL2=0.  799
C    752605 ALP2=-5.08889E-14 <===                                        799
         CALL AGSRESET                                                    802
         AG_DZ = 0.05                                                     802
         AG_THET = 0                                                      802
         AG_PHI = 0                                                       802
         AG_H1 = 0.198904                                                 802
         AG_BL1 = 0.794416                                                802
         AG_TL1 = 0.752605                                                802
         AG_ALP1 = -5.08889E-14                                           802
         AG_H2 = 0.198904                                                 802
         AG_BL2 = 0.794416                                                802
         AG_TL2 = 0.752605                                                802
         AG_ALP2 = -5.08889E-14                                           802
         AG_TITLE = 'TRAP'                                                802
         AG_EXNAME = 'SHAPE'                                              802
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       802
         CALL AXSHAPE                                                     802
C    *                                                                    802
6161  CALL AGSPOPB                                                        803
      GO TO BLOCK_PTRB, (6160 ,6170)                                      803
C End of Local Procedure                                                  803
C ----------------------------------------------------------------------  804
C ------------ PTRC                                                       804
C Begin Local Procedure PTRC                                              805
C PTRC PTR2 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    805
6180  CONTINUE                                                            805
         AG_TITLE='PTRC'                                                  805
         AG_MODULE=MODULE                                                 805
         AG_MARK='V00'                                                    805
         CALL AGSPUSH                                                     805
         AG_MODULE=MODULE                                                 805
C    *                                                                    805
C    Geant ATTRIBUTE : PTRC ===> SEEN=1 COLO=920 <===                     806
         CALL AGSRESET                                                    806
         AG_SEEN = 1                                                      806
         AG_COLO = 920                                                    806
         AG_TITLE = 'PTRC'                                                806
         AG_EXNAME = 'ATTRIBUTE'                                          806
         AG_PARLIST = 'SEEN_COLO'                                         806
         CALL AXATTRIBUTE                                                 806
C    *                                                                    806
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        806
         CALL AGSRESET                                                    807
         AG_TITLE = 'ALUMINIUM'                                           807
         AG_EXNAME = 'MATERIAL'                                           807
         AG_PARLIST = 'NONE'                                              807
         CALL AXMATERIAL                                                  807
C    *                                                                    807
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.719  807
C    003 TL1=0.677192 ALP1=0 H2=0.198904, BL2=0.719003 TL2=0.677192 ALP2  807
C    =0 <===                                                              807
         CALL AGSRESET                                                    810
         AG_DZ = 0.05                                                     810
         AG_THET = 0                                                      810
         AG_PHI = 0                                                       810
         AG_H1 = 0.198904                                                 810
         AG_BL1 = 0.719003                                                810
         AG_TL1 = 0.677192                                                810
         AG_ALP1 = 0                                                      810
         AG_H2 = 0.198904                                                 810
         AG_BL2 = 0.719003                                                810
         AG_TL2 = 0.677192                                                810
         AG_ALP2 = 0                                                      810
         AG_TITLE = 'TRAP'                                                810
         AG_EXNAME = 'SHAPE'                                              810
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       810
         CALL AXSHAPE                                                     810
C    *                                                                    810
6181  CALL AGSPOPB                                                        811
      GO TO BLOCK_PTRC, (6180 ,6190)                                      811
C End of Local Procedure                                                  811
C ----------------------------------------------------------------------  812
C ------------ PTRE                                                       812
C Begin Local Procedure PTRE                                              813
C PTRE PTR4 RIGHT HAND SIDE DOVETAIL EXT                                  813
6200  CONTINUE                                                            813
         AG_TITLE='PTRE'                                                  813
         AG_MODULE=MODULE                                                 813
         AG_MARK='V00'                                                    813
         CALL AGSPUSH                                                     813
         AG_MODULE=MODULE                                                 813
C    *                                                                    813
C    Geant ATTRIBUTE : PTRE ===> SEEN=1 COLO=920 <===                     814
         CALL AGSRESET                                                    814
         AG_SEEN = 1                                                      814
         AG_COLO = 920                                                    814
         AG_TITLE = 'PTRE'                                                814
         AG_EXNAME = 'ATTRIBUTE'                                          814
         AG_PARLIST = 'SEEN_COLO'                                         814
         CALL AXATTRIBUTE                                                 814
C    *                                                                    814
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        814
         CALL AGSRESET                                                    815
         AG_TITLE = 'ALUMINIUM'                                           815
         AG_EXNAME = 'MATERIAL'                                           815
         AG_PARLIST = 'NONE'                                              815
         CALL AXMATERIAL                                                  815
C    *                                                                    815
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.3 BL1=2.23613, TL  815
C    1=2.18066 ALP1=-0.72464 H2=0.3 BL2=2.23613, TL2=2.18066 ALP2=-0.724  815
C    64 <===                                                              815
         CALL AGSRESET                                                    818
         AG_DZ = 0.05                                                     818
         AG_THET = 0                                                      818
         AG_PHI = 0                                                       818
         AG_H1 = 0.3                                                      818
         AG_BL1 = 2.23613                                                 818
         AG_TL1 = 2.18066                                                 818
         AG_ALP1 = -0.72464                                               818
         AG_H2 = 0.3                                                      818
         AG_BL2 = 2.23613                                                 818
         AG_TL2 = 2.18066                                                 818
         AG_ALP2 = -0.72464                                               818
         AG_TITLE = 'TRAP'                                                818
         AG_EXNAME = 'SHAPE'                                              818
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       818
         CALL AXSHAPE                                                     818
C    *                                                                    818
6201  CALL AGSPOPB                                                        819
      GO TO BLOCK_PTRE, (6200 ,6210)                                      819
C End of Local Procedure                                                  819
C ----------------------------------------------------------------------  820
C ------------ PTRF                                                       820
C Begin Local Procedure PTRF                                              821
C PTRF PTR5 LEFT HAND (NEAR PIECE)                                        821
6220  CONTINUE                                                            821
         AG_TITLE='PTRF'                                                  821
         AG_MODULE=MODULE                                                 821
         AG_MARK='V00'                                                    821
         CALL AGSPUSH                                                     821
         AG_MODULE=MODULE                                                 821
C    *                                                                    821
C    Geant ATTRIBUTE : PTRF ===> SEEN=1 COLO=920 <===                     822
         CALL AGSRESET                                                    822
         AG_SEEN = 1                                                      822
         AG_COLO = 920                                                    822
         AG_TITLE = 'PTRF'                                                822
         AG_EXNAME = 'ATTRIBUTE'                                          822
         AG_PARLIST = 'SEEN_COLO'                                         822
         CALL AXATTRIBUTE                                                 822
C    *                                                                    822
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        822
         CALL AGSRESET                                                    823
         AG_TITLE = 'ALUMINIUM'                                           823
         AG_EXNAME = 'MATERIAL'                                           823
         AG_PARLIST = 'NONE'                                              823
         CALL AXMATERIAL                                                  823
C    *                                                                    823
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.3 BL1=2.11017, TL  823
C    1=1.88491 ALP1=25.6663 H2=0.3 BL2=2.11017, TL2=1.88491 ALP2=25.6663  823
C     <===                                                                823
         CALL AGSRESET                                                    826
         AG_DZ = 0.05                                                     826
         AG_THET = 0                                                      826
         AG_PHI = 0                                                       826
         AG_H1 = 0.3                                                      826
         AG_BL1 = 2.11017                                                 826
         AG_TL1 = 1.88491                                                 826
         AG_ALP1 = 25.6663                                                826
         AG_H2 = 0.3                                                      826
         AG_BL2 = 2.11017                                                 826
         AG_TL2 = 1.88491                                                 826
         AG_ALP2 = 25.6663                                                826
         AG_TITLE = 'TRAP'                                                826
         AG_EXNAME = 'SHAPE'                                              826
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       826
         CALL AXSHAPE                                                     826
C    *                                                                    826
6221  CALL AGSPOPB                                                        827
      GO TO BLOCK_PTRF, (6220 ,6230)                                      827
C End of Local Procedure                                                  827
C ----------------------------------------------------------------------  828
C ------------ PTRG                                                       828
C Begin Local Procedure PTRG                                              829
C PTRG PTR6 PIECE NEAR BP                                                 829
6240  CONTINUE                                                            829
         AG_TITLE='PTRG'                                                  829
         AG_MODULE=MODULE                                                 829
         AG_MARK='V00'                                                    829
         CALL AGSPUSH                                                     829
         AG_MODULE=MODULE                                                 829
C    *                                                                    829
C    Geant ATTRIBUTE : PTRG ===> SEEN=1 COLO=920 <===                     830
         CALL AGSRESET                                                    830
         AG_SEEN = 1                                                      830
         AG_COLO = 920                                                    830
         AG_TITLE = 'PTRG'                                                830
         AG_EXNAME = 'ATTRIBUTE'                                          830
         AG_PARLIST = 'SEEN_COLO'                                         830
         CALL AXATTRIBUTE                                                 830
C    *                                                                    830
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        830
         CALL AGSRESET                                                    831
         AG_TITLE = 'ALUMINIUM'                                           831
         AG_EXNAME = 'MATERIAL'                                           831
         AG_PARLIST = 'NONE'                                              831
         CALL AXMATERIAL                                                  831
C    *                                                                    831
C    Geant SHAPE : TRAP ===> DZ=0.65 THET=0 PHI=0 H1=0.4 BL1=0.485173, T  831
C    L1=0.174703 ALP1=-25.0747 H2=0.4, BL2=0.485173 TL2=0.174703 ALP2=-2  831
C    5.0747 <===                                                          831
         CALL AGSRESET                                                    834
         AG_DZ = 0.65                                                     834
         AG_THET = 0                                                      834
         AG_PHI = 0                                                       834
         AG_H1 = 0.4                                                      834
         AG_BL1 = 0.485173                                                834
         AG_TL1 = 0.174703                                                834
         AG_ALP1 = -25.0747                                               834
         AG_H2 = 0.4                                                      834
         AG_BL2 = 0.485173                                                834
         AG_TL2 = 0.174703                                                834
         AG_ALP2 = -25.0747                                               834
         AG_TITLE = 'TRAP'                                                834
         AG_EXNAME = 'SHAPE'                                              834
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       834
         CALL AXSHAPE                                                     834
C    *                                                                    834
6241  CALL AGSPOPB                                                        835
      GO TO BLOCK_PTRG, (6240 ,6250)                                      835
C End of Local Procedure                                                  835
C ----------------------------------------------------------------------  836
C ------------ PTMA                                                       836
C Begin Local Procedure PTMA                                              837
C PTMA PTM0 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    837
6260  CONTINUE                                                            837
         AG_TITLE='PTMA'                                                  837
         AG_MODULE=MODULE                                                 837
         AG_MARK='V00'                                                    837
         CALL AGSPUSH                                                     837
         AG_MODULE=MODULE                                                 837
C    *                                                                    837
C    Geant ATTRIBUTE : PTMA ===> SEEN=1 COLO=920 <===                     838
         CALL AGSRESET                                                    838
         AG_SEEN = 1                                                      838
         AG_COLO = 920                                                    838
         AG_TITLE = 'PTMA'                                                838
         AG_EXNAME = 'ATTRIBUTE'                                          838
         AG_PARLIST = 'SEEN_COLO'                                         838
         CALL AXATTRIBUTE                                                 838
C    *                                                                    838
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        838
         CALL AGSRESET                                                    839
         AG_TITLE = 'ALUMINIUM'                                           839
         AG_EXNAME = 'MATERIAL'                                           839
         AG_PARLIST = 'NONE'                                              839
         CALL AXMATERIAL                                                  839
C    *                                                                    839
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.198904, BL1=0.4676  839
C    26 TL1=0.425815 ALP1=-1.01778E-13, H2=0.198904 BL2=0.467626 TL2=0.4  839
C    25815 ALP2=-1.01778E-13 <===                                         839
         CALL AGSRESET                                                    842
         AG_DZ = 0.6                                                      842
         AG_THET = 0                                                      842
         AG_PHI = 0                                                       842
         AG_H1 = 0.198904                                                 842
         AG_BL1 = 0.467626                                                842
         AG_TL1 = 0.425815                                                842
         AG_ALP1 = -1.01778E-13                                           842
         AG_H2 = 0.198904                                                 842
         AG_BL2 = 0.467626                                                842
         AG_TL2 = 0.425815                                                842
         AG_ALP2 = -1.01778E-13                                           842
         AG_TITLE = 'TRAP'                                                842
         AG_EXNAME = 'SHAPE'                                              842
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       842
         CALL AXSHAPE                                                     842
C    *                                                                    842
6261  CALL AGSPOPB                                                        843
      GO TO BLOCK_PTMA, (6260 ,6270)                                      843
C End of Local Procedure                                                  843
C ----------------------------------------------------------------------  844
C ------------ PTMB                                                       844
C Begin Local Procedure PTMB                                              845
C PTMB PTM1 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    845
6280  CONTINUE                                                            845
         AG_TITLE='PTMB'                                                  845
         AG_MODULE=MODULE                                                 845
         AG_MARK='V00'                                                    845
         CALL AGSPUSH                                                     845
         AG_MODULE=MODULE                                                 845
C    *                                                                    845
C    Geant ATTRIBUTE : PTMB ===> SEEN=1 COLO=920 <===                     846
         CALL AGSRESET                                                    846
         AG_SEEN = 1                                                      846
         AG_COLO = 920                                                    846
         AG_TITLE = 'PTMB'                                                846
         AG_EXNAME = 'ATTRIBUTE'                                          846
         AG_PARLIST = 'SEEN_COLO'                                         846
         CALL AXATTRIBUTE                                                 846
C    *                                                                    846
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        846
         CALL AGSRESET                                                    847
         AG_TITLE = 'ALUMINIUM'                                           847
         AG_EXNAME = 'MATERIAL'                                           847
         AG_PARLIST = 'NONE'                                              847
         CALL AXMATERIAL                                                  847
C    *                                                                    847
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.198904, BL1=0.7944  847
C    16 TL1=0.752605 ALP1=-5.08889E-14, H2=0.198904 BL2=0.794416 TL2=0.7  847
C    52605 ALP2=-5.08889E-14 <===                                         847
         CALL AGSRESET                                                    850
         AG_DZ = 0.6                                                      850
         AG_THET = 0                                                      850
         AG_PHI = 0                                                       850
         AG_H1 = 0.198904                                                 850
         AG_BL1 = 0.794416                                                850
         AG_TL1 = 0.752605                                                850
         AG_ALP1 = -5.08889E-14                                           850
         AG_H2 = 0.198904                                                 850
         AG_BL2 = 0.794416                                                850
         AG_TL2 = 0.752605                                                850
         AG_ALP2 = -5.08889E-14                                           850
         AG_TITLE = 'TRAP'                                                850
         AG_EXNAME = 'SHAPE'                                              850
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       850
         CALL AXSHAPE                                                     850
C    *                                                                    850
6281  CALL AGSPOPB                                                        851
      GO TO BLOCK_PTMB, (6280 ,6290)                                      851
C End of Local Procedure                                                  851
C ----------------------------------------------------------------------  852
C ------------ PTMC                                                       852
C Begin Local Procedure PTMC                                              853
C PTMC PTM2 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    853
6300  CONTINUE                                                            853
         AG_TITLE='PTMC'                                                  853
         AG_MODULE=MODULE                                                 853
         AG_MARK='V00'                                                    853
         CALL AGSPUSH                                                     853
         AG_MODULE=MODULE                                                 853
C    *                                                                    853
C    Geant ATTRIBUTE : PTMC ===> SEEN=1 COLO=920 <===                     854
         CALL AGSRESET                                                    854
         AG_SEEN = 1                                                      854
         AG_COLO = 920                                                    854
         AG_TITLE = 'PTMC'                                                854
         AG_EXNAME = 'ATTRIBUTE'                                          854
         AG_PARLIST = 'SEEN_COLO'                                         854
         CALL AXATTRIBUTE                                                 854
C    *                                                                    854
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        854
         CALL AGSRESET                                                    855
         AG_TITLE = 'ALUMINIUM'                                           855
         AG_EXNAME = 'MATERIAL'                                           855
         AG_PARLIST = 'NONE'                                              855
         CALL AXMATERIAL                                                  855
C    *                                                                    855
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.198904, BL1=0.4676  855
C    26 TL1=0.425815 ALP1=-1.52667E-13, H2=0.198904 BL2=0.467626 TL2=0.4  855
C    25815 ALP2=-1.52667E-13 <===                                         855
         CALL AGSRESET                                                    858
         AG_DZ = 0.6                                                      858
         AG_THET = 0                                                      858
         AG_PHI = 0                                                       858
         AG_H1 = 0.198904                                                 858
         AG_BL1 = 0.467626                                                858
         AG_TL1 = 0.425815                                                858
         AG_ALP1 = -1.52667E-13                                           858
         AG_H2 = 0.198904                                                 858
         AG_BL2 = 0.467626                                                858
         AG_TL2 = 0.425815                                                858
         AG_ALP2 = -1.52667E-13                                           858
         AG_TITLE = 'TRAP'                                                858
         AG_EXNAME = 'SHAPE'                                              858
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       858
         CALL AXSHAPE                                                     858
C    *                                                                    858
6301  CALL AGSPOPB                                                        859
      GO TO BLOCK_PTMC, (6300 ,6310)                                      859
C End of Local Procedure                                                  859
C ----------------------------------------------------------------------  860
C ------------ PTME                                                       860
C Begin Local Procedure PTME                                              861
C PTME PTM4 OF FIRST PIECE OF DOVETAIL                                    861
6320  CONTINUE                                                            861
         AG_TITLE='PTME'                                                  861
         AG_MODULE=MODULE                                                 861
         AG_MARK='V00'                                                    861
         CALL AGSPUSH                                                     861
         AG_MODULE=MODULE                                                 861
C    *                                                                    861
C    Geant ATTRIBUTE : PTME ===> SEEN=1 COLO=920 <===                     862
         CALL AGSRESET                                                    862
         AG_SEEN = 1                                                      862
         AG_COLO = 920                                                    862
         AG_TITLE = 'PTME'                                                862
         AG_EXNAME = 'ATTRIBUTE'                                          862
         AG_PARLIST = 'SEEN_COLO'                                         862
         CALL AXATTRIBUTE                                                 862
C    *                                                                    862
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        862
         CALL AGSRESET                                                    863
         AG_TITLE = 'ALUMINIUM'                                           863
         AG_EXNAME = 'MATERIAL'                                           863
         AG_PARLIST = 'NONE'                                              863
         CALL AXMATERIAL                                                  863
C    *                                                                    863
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.05 BL1=2.1899, TL1  863
C    =2.18066 ALP1=-0.72464 H2=0.05 BL2=2.1899, TL2=2.18066 ALP2=-0.7246  863
C    4 <===                                                               863
         CALL AGSRESET                                                    866
         AG_DZ = 0.6                                                      866
         AG_THET = 0                                                      866
         AG_PHI = 0                                                       866
         AG_H1 = 0.05                                                     866
         AG_BL1 = 2.1899                                                  866
         AG_TL1 = 2.18066                                                 866
         AG_ALP1 = -0.72464                                               866
         AG_H2 = 0.05                                                     866
         AG_BL2 = 2.1899                                                  866
         AG_TL2 = 2.18066                                                 866
         AG_ALP2 = -0.72464                                               866
         AG_TITLE = 'TRAP'                                                866
         AG_EXNAME = 'SHAPE'                                              866
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       866
         CALL AXSHAPE                                                     866
C    *                                                                    866
6321  CALL AGSPOPB                                                        867
      GO TO BLOCK_PTME, (6320 ,6330)                                      867
C End of Local Procedure                                                  867
C ----------------------------------------------------------------------  868
C ------------ PTMF                                                       868
C Begin Local Procedure PTMF                                              869
C PTMF PTM5 LEFT MIDDLE PART                                              869
6340  CONTINUE                                                            869
         AG_TITLE='PTMF'                                                  869
         AG_MODULE=MODULE                                                 869
         AG_MARK='V00'                                                    869
         CALL AGSPUSH                                                     869
         AG_MODULE=MODULE                                                 869
C    *                                                                    869
C    Geant ATTRIBUTE : PTMF ===> SEEN=1 COLO=920 <===                     870
         CALL AGSRESET                                                    870
         AG_SEEN = 1                                                      870
         AG_COLO = 920                                                    870
         AG_TITLE = 'PTMF'                                                870
         AG_EXNAME = 'ATTRIBUTE'                                          870
         AG_PARLIST = 'SEEN_COLO'                                         870
         CALL AXATTRIBUTE                                                 870
C    *                                                                    870
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        870
         CALL AGSRESET                                                    871
         AG_TITLE = 'ALUMINIUM'                                           871
         AG_EXNAME = 'MATERIAL'                                           871
         AG_PARLIST = 'NONE'                                              871
         CALL AXMATERIAL                                                  871
C    *                                                                    871
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.05 BL1=2.11017, TL  871
C    1=2.07263 ALP1=25.6663 H2=0.05 BL2=2.11017, TL2=2.07263 ALP2=25.666  871
C    3 <===                                                               871
         CALL AGSRESET                                                    874
         AG_DZ = 0.6                                                      874
         AG_THET = 0                                                      874
         AG_PHI = 0                                                       874
         AG_H1 = 0.05                                                     874
         AG_BL1 = 2.11017                                                 874
         AG_TL1 = 2.07263                                                 874
         AG_ALP1 = 25.6663                                                874
         AG_H2 = 0.05                                                     874
         AG_BL2 = 2.11017                                                 874
         AG_TL2 = 2.07263                                                 874
         AG_ALP2 = 25.6663                                                874
         AG_TITLE = 'TRAP'                                                874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       874
         CALL AXSHAPE                                                     874
C    *                                                                    874
6341  CALL AGSPOPB                                                        875
      GO TO BLOCK_PTMF, (6340 ,6350)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ PTLA                                                       876
C Begin Local Procedure PTLA                                              877
C PTLA PTL0 DOVETAIL SUPPORT PLATE PART OUTER                             877
6360  CONTINUE                                                            877
         AG_TITLE='PTLA'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant ATTRIBUTE : PTLA ===> SEEN=1 COLO=920 <===                     878
         CALL AGSRESET                                                    878
         AG_SEEN = 1                                                      878
         AG_COLO = 920                                                    878
         AG_TITLE = 'PTLA'                                                878
         AG_EXNAME = 'ATTRIBUTE'                                          878
         AG_PARLIST = 'SEEN_COLO'                                         878
         CALL AXATTRIBUTE                                                 878
C    *                                                                    878
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        878
         CALL AGSRESET                                                    879
         AG_TITLE = 'ALUMINIUM'                                           879
         AG_EXNAME = 'MATERIAL'                                           879
         AG_PARLIST = 'NONE'                                              879
         CALL AXMATERIAL                                                  879
C    *                                                                    879
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  879
C    416 TL1=0.752605 ALP1=0 H2=0.198904, BL2=0.794416 TL2=0.752605 ALP2  879
C    =0 <===                                                              879
         CALL AGSRESET                                                    882
         AG_DZ = 0.05                                                     882
         AG_THET = 0                                                      882
         AG_PHI = 0                                                       882
         AG_H1 = 0.198904                                                 882
         AG_BL1 = 0.794416                                                882
         AG_TL1 = 0.752605                                                882
         AG_ALP1 = 0                                                      882
         AG_H2 = 0.198904                                                 882
         AG_BL2 = 0.794416                                                882
         AG_TL2 = 0.752605                                                882
         AG_ALP2 = 0                                                      882
         AG_TITLE = 'TRAP'                                                882
         AG_EXNAME = 'SHAPE'                                              882
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       882
         CALL AXSHAPE                                                     882
C    *                                                                    882
6361  CALL AGSPOPB                                                        883
      GO TO BLOCK_PTLA, (6360 ,6370)                                      883
C End of Local Procedure                                                  883
C ----------------------------------------------------------------------  884
C ------------ PTLB                                                       884
C Begin Local Procedure PTLB                                              885
C PTLB PTL1 DOVETAIL SUPPORT PLATE PART OUTER                             885
6380  CONTINUE                                                            885
         AG_TITLE='PTLB'                                                  885
         AG_MODULE=MODULE                                                 885
         AG_MARK='V00'                                                    885
         CALL AGSPUSH                                                     885
         AG_MODULE=MODULE                                                 885
C    *                                                                    885
C    Geant ATTRIBUTE : PTLB ===> SEEN=1 COLO=920 <===                     886
         CALL AGSRESET                                                    886
         AG_SEEN = 1                                                      886
         AG_COLO = 920                                                    886
         AG_TITLE = 'PTLB'                                                886
         AG_EXNAME = 'ATTRIBUTE'                                          886
         AG_PARLIST = 'SEEN_COLO'                                         886
         CALL AXATTRIBUTE                                                 886
C    *                                                                    886
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        886
         CALL AGSRESET                                                    887
         AG_TITLE = 'ALUMINIUM'                                           887
         AG_EXNAME = 'MATERIAL'                                           887
         AG_PARLIST = 'NONE'                                              887
         CALL AXMATERIAL                                                  887
C    *                                                                    887
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  887
C    416 TL1=0.752605 ALP1=-5.08889E-14, H2=0.198904 BL2=0.794416 TL2=0.  887
C    752605 ALP2=-5.08889E-14 <===                                        887
         CALL AGSRESET                                                    890
         AG_DZ = 0.05                                                     890
         AG_THET = 0                                                      890
         AG_PHI = 0                                                       890
         AG_H1 = 0.198904                                                 890
         AG_BL1 = 0.794416                                                890
         AG_TL1 = 0.752605                                                890
         AG_ALP1 = -5.08889E-14                                           890
         AG_H2 = 0.198904                                                 890
         AG_BL2 = 0.794416                                                890
         AG_TL2 = 0.752605                                                890
         AG_ALP2 = -5.08889E-14                                           890
         AG_TITLE = 'TRAP'                                                890
         AG_EXNAME = 'SHAPE'                                              890
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       890
         CALL AXSHAPE                                                     890
C    *                                                                    890
6381  CALL AGSPOPB                                                        891
      GO TO BLOCK_PTLB, (6380 ,6390)                                      891
C End of Local Procedure                                                  891
C ----------------------------------------------------------------------  892
C ------------ PTLC                                                       892
C Begin Local Procedure PTLC                                              893
C PTLC PTL2 DOVETAIL SUPPORT PLATE PART OUTER                             893
6400  CONTINUE                                                            893
         AG_TITLE='PTLC'                                                  893
         AG_MODULE=MODULE                                                 893
         AG_MARK='V00'                                                    893
         CALL AGSPUSH                                                     893
         AG_MODULE=MODULE                                                 893
C    *                                                                    893
C    Geant ATTRIBUTE : PTLC ===> SEEN=1 COLO=920 <===                     894
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 920                                                    894
         AG_TITLE = 'PTLC'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO'                                         894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        894
         CALL AGSRESET                                                    895
         AG_TITLE = 'ALUMINIUM'                                           895
         AG_EXNAME = 'MATERIAL'                                           895
         AG_PARLIST = 'NONE'                                              895
         CALL AXMATERIAL                                                  895
C    *                                                                    895
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  895
C    416 TL1=0.752605 ALP1=0 H2=0.198904, BL2=0.794416 TL2=0.752605 ALP2  895
C    =0 <===                                                              895
         CALL AGSRESET                                                    898
         AG_DZ = 0.05                                                     898
         AG_THET = 0                                                      898
         AG_PHI = 0                                                       898
         AG_H1 = 0.198904                                                 898
         AG_BL1 = 0.794416                                                898
         AG_TL1 = 0.752605                                                898
         AG_ALP1 = 0                                                      898
         AG_H2 = 0.198904                                                 898
         AG_BL2 = 0.794416                                                898
         AG_TL2 = 0.752605                                                898
         AG_ALP2 = 0                                                      898
         AG_TITLE = 'TRAP'                                                898
         AG_EXNAME = 'SHAPE'                                              898
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       898
         CALL AXSHAPE                                                     898
C    *                                                                    898
6401  CALL AGSPOPB                                                        899
      GO TO BLOCK_PTLC, (6400 ,6410)                                      899
C End of Local Procedure                                                  899
C ----------------------------------------------------------------------  900
C ------------ PTLD                                                       900
C Begin Local Procedure PTLD                                              901
C PTLD PTL3 DOVETAIL SUPPORT PALTE PART OUTER                             901
6420  CONTINUE                                                            901
         AG_TITLE='PTLD'                                                  901
         AG_MODULE=MODULE                                                 901
         AG_MARK='V00'                                                    901
         CALL AGSPUSH                                                     901
         AG_MODULE=MODULE                                                 901
C    *                                                                    901
C    Geant ATTRIBUTE : PTLD ===> SEEN=1 COLO=920 <===                     902
         CALL AGSRESET                                                    902
         AG_SEEN = 1                                                      902
         AG_COLO = 920                                                    902
         AG_TITLE = 'PTLD'                                                902
         AG_EXNAME = 'ATTRIBUTE'                                          902
         AG_PARLIST = 'SEEN_COLO'                                         902
         CALL AXATTRIBUTE                                                 902
C    *                                                                    902
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        902
         CALL AGSRESET                                                    903
         AG_TITLE = 'ALUMINIUM'                                           903
         AG_EXNAME = 'MATERIAL'                                           903
         AG_PARLIST = 'NONE'                                              903
         CALL AXMATERIAL                                                  903
C    *                                                                    903
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.397809, BL1=0.365  903
C    85 TL1=0.282227 ALP1=-5.08889E-14, H2=0.397809 BL2=0.36585 TL2=0.28  903
C    2227 ALP2=-5.08889E-14 <===                                          903
         CALL AGSRESET                                                    906
         AG_DZ = 0.05                                                     906
         AG_THET = 0                                                      906
         AG_PHI = 0                                                       906
         AG_H1 = 0.397809                                                 906
         AG_BL1 = 0.36585                                                 906
         AG_TL1 = 0.282227                                                906
         AG_ALP1 = -5.08889E-14                                           906
         AG_H2 = 0.397809                                                 906
         AG_BL2 = 0.36585                                                 906
         AG_TL2 = 0.282227                                                906
         AG_ALP2 = -5.08889E-14                                           906
         AG_TITLE = 'TRAP'                                                906
         AG_EXNAME = 'SHAPE'                                              906
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       906
         CALL AXSHAPE                                                     906
C    *                                                                    906
6421  CALL AGSPOPB                                                        907
      GO TO BLOCK_PTLD, (6420 ,6430)                                      907
C End of Local Procedure                                                  907
C ----------------------------------------------------------------------  908
C ------------ PTLE                                                       908
C Begin Local Procedure PTLE                                              909
C PTLE PTL4 DOVETAIL SUPPORT PALTE PART OUTER                             909
6440  CONTINUE                                                            909
         AG_TITLE='PTLE'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant ATTRIBUTE : PTLE ===> SEEN=1 COLO=920 <===                     910
         CALL AGSRESET                                                    910
         AG_SEEN = 1                                                      910
         AG_COLO = 920                                                    910
         AG_TITLE = 'PTLE'                                                910
         AG_EXNAME = 'ATTRIBUTE'                                          910
         AG_PARLIST = 'SEEN_COLO'                                         910
         CALL AXATTRIBUTE                                                 910
C    *                                                                    910
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        910
         CALL AGSRESET                                                    911
         AG_TITLE = 'ALUMINIUM'                                           911
         AG_EXNAME = 'MATERIAL'                                           911
         AG_PARLIST = 'NONE'                                              911
         CALL AXMATERIAL                                                  911
C    *                                                                    911
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.397809, BL1=0.365  911
C    85 TL1=0.282227 ALP1=5.08889E-14, H2=0.397809 BL2=0.36585 TL2=0.282  911
C    227 ALP2=5.08889E-14 <===                                            911
         CALL AGSRESET                                                    914
         AG_DZ = 0.05                                                     914
         AG_THET = 0                                                      914
         AG_PHI = 0                                                       914
         AG_H1 = 0.397809                                                 914
         AG_BL1 = 0.36585                                                 914
         AG_TL1 = 0.282227                                                914
         AG_ALP1 = 5.08889E-14                                            914
         AG_H2 = 0.397809                                                 914
         AG_BL2 = 0.36585                                                 914
         AG_TL2 = 0.282227                                                914
         AG_ALP2 = 5.08889E-14                                            914
         AG_TITLE = 'TRAP'                                                914
         AG_EXNAME = 'SHAPE'                                              914
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       914
         CALL AXSHAPE                                                     914
C    *                                                                    914
6441  CALL AGSPOPB                                                        915
      GO TO BLOCK_PTLE, (6440 ,6450)                                      915
C End of Local Procedure                                                  915
C ----------------------------------------------------------------------  916
C ------------ PTLF                                                       916
C Begin Local Procedure PTLF                                              917
C PTLF PTL5 DOVETAIL SUPPORT PALTE PART OUTER                             917
6460  CONTINUE                                                            917
         AG_TITLE='PTLF'                                                  917
         AG_MODULE=MODULE                                                 917
         AG_MARK='V00'                                                    917
         CALL AGSPUSH                                                     917
         AG_MODULE=MODULE                                                 917
C    *                                                                    917
C    Geant ATTRIBUTE : PTLF ===> SEEN=1 COLO=920 <===                     918
         CALL AGSRESET                                                    918
         AG_SEEN = 1                                                      918
         AG_COLO = 920                                                    918
         AG_TITLE = 'PTLF'                                                918
         AG_EXNAME = 'ATTRIBUTE'                                          918
         AG_PARLIST = 'SEEN_COLO'                                         918
         CALL AXATTRIBUTE                                                 918
C    *                                                                    918
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        918
         CALL AGSRESET                                                    919
         AG_TITLE = 'ALUMINIUM'                                           919
         AG_EXNAME = 'MATERIAL'                                           919
         AG_PARLIST = 'NONE'                                              919
         CALL AXMATERIAL                                                  919
C    *                                                                    919
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.397809, BL1=0.365  919
C    85 TL1=0.282227 ALP1=-1.01778E-13, H2=0.397809 BL2=0.36585 TL2=0.28  919
C    2227 ALP2=-1.01778E-13 <===                                          919
         CALL AGSRESET                                                    922
         AG_DZ = 0.05                                                     922
         AG_THET = 0                                                      922
         AG_PHI = 0                                                       922
         AG_H1 = 0.397809                                                 922
         AG_BL1 = 0.36585                                                 922
         AG_TL1 = 0.282227                                                922
         AG_ALP1 = -1.01778E-13                                           922
         AG_H2 = 0.397809                                                 922
         AG_BL2 = 0.36585                                                 922
         AG_TL2 = 0.282227                                                922
         AG_ALP2 = -1.01778E-13                                           922
         AG_TITLE = 'TRAP'                                                922
         AG_EXNAME = 'SHAPE'                                              922
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       922
         CALL AXSHAPE                                                     922
C    *                                                                    922
6461  CALL AGSPOPB                                                        923
      GO TO BLOCK_PTLF, (6460 ,6470)                                      923
C End of Local Procedure                                                  923
C ----------------------------------------------------------------------  924
C ------------ PTLG                                                       924
C Begin Local Procedure PTLG                                              925
C PTLG PTL6 DOVETAIL SUPPORT PLATE PART OUTER                             925
6480  CONTINUE                                                            925
         AG_TITLE='PTLG'                                                  925
         AG_MODULE=MODULE                                                 925
         AG_MARK='V00'                                                    925
         CALL AGSPUSH                                                     925
         AG_MODULE=MODULE                                                 925
C    *                                                                    925
C    Geant ATTRIBUTE : PTLG ===> SEEN=1 COLO=920 <===                     926
         CALL AGSRESET                                                    926
         AG_SEEN = 1                                                      926
         AG_COLO = 920                                                    926
         AG_TITLE = 'PTLG'                                                926
         AG_EXNAME = 'ATTRIBUTE'                                          926
         AG_PARLIST = 'SEEN_COLO'                                         926
         CALL AXATTRIBUTE                                                 926
C    *                                                                    926
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        926
         CALL AGSRESET                                                    927
         AG_TITLE = 'ALUMINIUM'                                           927
         AG_EXNAME = 'MATERIAL'                                           927
         AG_PARLIST = 'NONE'                                              927
         CALL AXMATERIAL                                                  927
C    *                                                                    927
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=1.83987, BL1=0.3770  927
C    66 TL1=0.377066 ALP1=6 H2=1.83987, BL2=0.377066 TL2=0.377066 ALP2=6  927
C     <===                                                                927
         CALL AGSRESET                                                    930
         AG_DZ = 0.05                                                     930
         AG_THET = 0                                                      930
         AG_PHI = 0                                                       930
         AG_H1 = 1.83987                                                  930
         AG_BL1 = 0.377066                                                930
         AG_TL1 = 0.377066                                                930
         AG_ALP1 = 6                                                      930
         AG_H2 = 1.83987                                                  930
         AG_BL2 = 0.377066                                                930
         AG_TL2 = 0.377066                                                930
         AG_ALP2 = 6                                                      930
         AG_TITLE = 'TRAP'                                                930
         AG_EXNAME = 'SHAPE'                                              930
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       930
         CALL AXSHAPE                                                     930
C    *                                                                    930
6481  CALL AGSPOPB                                                        931
      GO TO BLOCK_PTLG, (6480 ,6490)                                      931
C End of Local Procedure                                                  931
C ----------------------------------------------------------------------  932
C ------------ PTLH                                                       932
C Begin Local Procedure PTLH                                              933
C PTLH PTL7 DOVETAIL SUPPORT PLATE PART OUTER                             933
6500  CONTINUE                                                            933
         AG_TITLE='PTLH'                                                  933
         AG_MODULE=MODULE                                                 933
         AG_MARK='V00'                                                    933
         CALL AGSPUSH                                                     933
         AG_MODULE=MODULE                                                 933
C    *                                                                    933
C    Geant ATTRIBUTE : PTLH ===> SEEN=1 COLO=920 <===                     934
         CALL AGSRESET                                                    934
         AG_SEEN = 1                                                      934
         AG_COLO = 920                                                    934
         AG_TITLE = 'PTLH'                                                934
         AG_EXNAME = 'ATTRIBUTE'                                          934
         AG_PARLIST = 'SEEN_COLO'                                         934
         CALL AXATTRIBUTE                                                 934
C    *                                                                    934
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        934
         CALL AGSRESET                                                    935
         AG_TITLE = 'ALUMINIUM'                                           935
         AG_EXNAME = 'MATERIAL'                                           935
         AG_PARLIST = 'NONE'                                              935
         CALL AXMATERIAL                                                  935
C    *                                                                    935
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=1.83987, BL1=0.3770  935
C    66 TL1=0.377066 ALP1=-6 H2=1.83987, BL2=0.377066 TL2=0.377066 ALP2=  935
C    -6 <===                                                              935
         CALL AGSRESET                                                    938
         AG_DZ = 0.05                                                     938
         AG_THET = 0                                                      938
         AG_PHI = 0                                                       938
         AG_H1 = 1.83987                                                  938
         AG_BL1 = 0.377066                                                938
         AG_TL1 = 0.377066                                                938
         AG_ALP1 = -6                                                     938
         AG_H2 = 1.83987                                                  938
         AG_BL2 = 0.377066                                                938
         AG_TL2 = 0.377066                                                938
         AG_ALP2 = -6                                                     938
         AG_TITLE = 'TRAP'                                                938
         AG_EXNAME = 'SHAPE'                                              938
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       938
         CALL AXSHAPE                                                     938
C    *                                                                    938
6501  CALL AGSPOPB                                                        939
      GO TO BLOCK_PTLH, (6500 ,6510)                                      939
C End of Local Procedure                                                  939
C ----------------------------------------------------------------------  940
C ------------ PTLI                                                       940
C Begin Local Procedure PTLI                                              941
C PTLI PTL8 DOVETAIL SUPPORT PLATE PART OUTER                             941
6520  CONTINUE                                                            941
         AG_TITLE='PTLI'                                                  941
         AG_MODULE=MODULE                                                 941
         AG_MARK='V00'                                                    941
         CALL AGSPUSH                                                     941
         AG_MODULE=MODULE                                                 941
C    *                                                                    941
C    Geant ATTRIBUTE : PTLI ===> SEEN=1 COLO=920 <===                     942
         CALL AGSRESET                                                    942
         AG_SEEN = 1                                                      942
         AG_COLO = 920                                                    942
         AG_TITLE = 'PTLI'                                                942
         AG_EXNAME = 'ATTRIBUTE'                                          942
         AG_PARLIST = 'SEEN_COLO'                                         942
         CALL AXATTRIBUTE                                                 942
C    *                                                                    942
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        942
         CALL AGSRESET                                                    943
         AG_TITLE = 'ALUMINIUM'                                           943
         AG_EXNAME = 'MATERIAL'                                           943
         AG_PARLIST = 'NONE'                                              943
         CALL AXMATERIAL                                                  943
C    *                                                                    943
C    Geant SHAPE : TRAP ===> DZ=0.2 THET=0 PHI=0 H1=2.43658, BL1=0.37706  943
C    6 TL1=0.377066 ALP1=6 H2=2.43658, BL2=0.377066 TL2=0.377066 ALP2=6   943
C    <===                                                                 943
         CALL AGSRESET                                                    946
         AG_DZ = 0.2                                                      946
         AG_THET = 0                                                      946
         AG_PHI = 0                                                       946
         AG_H1 = 2.43658                                                  946
         AG_BL1 = 0.377066                                                946
         AG_TL1 = 0.377066                                                946
         AG_ALP1 = 6                                                      946
         AG_H2 = 2.43658                                                  946
         AG_BL2 = 0.377066                                                946
         AG_TL2 = 0.377066                                                946
         AG_ALP2 = 6                                                      946
         AG_TITLE = 'TRAP'                                                946
         AG_EXNAME = 'SHAPE'                                              946
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       946
         CALL AXSHAPE                                                     946
C    *                                                                    946
6521  CALL AGSPOPB                                                        947
      GO TO BLOCK_PTLI, (6520 ,6530)                                      947
C End of Local Procedure                                                  947
C ----------------------------------------------------------------------  948
C ------------ PTLJ                                                       948
C Begin Local Procedure PTLJ                                              949
C PTLJ PTL9 DOVETAIL SUPPORT PLATE PART OUTER                             949
6540  CONTINUE                                                            949
         AG_TITLE='PTLJ'                                                  949
         AG_MODULE=MODULE                                                 949
         AG_MARK='V00'                                                    949
         CALL AGSPUSH                                                     949
         AG_MODULE=MODULE                                                 949
C    *                                                                    949
C    Geant ATTRIBUTE : PTLJ ===> SEEN=1 COLO=920 <===                     950
         CALL AGSRESET                                                    950
         AG_SEEN = 1                                                      950
         AG_COLO = 920                                                    950
         AG_TITLE = 'PTLJ'                                                950
         AG_EXNAME = 'ATTRIBUTE'                                          950
         AG_PARLIST = 'SEEN_COLO'                                         950
         CALL AXATTRIBUTE                                                 950
C    *                                                                    950
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        950
         CALL AGSRESET                                                    951
         AG_TITLE = 'ALUMINIUM'                                           951
         AG_EXNAME = 'MATERIAL'                                           951
         AG_PARLIST = 'NONE'                                              951
         CALL AXMATERIAL                                                  951
C    *                                                                    951
C    Geant SHAPE : TRAP ===> DZ=0.2 THET=0 PHI=0 H1=2.43658, BL1=0.37706  951
C    6 TL1=0.377066 ALP1=-6 H2=2.43658, BL2=0.377066 TL2=0.377066 ALP2=-  951
C    6 <===                                                               951
         CALL AGSRESET                                                    954
         AG_DZ = 0.2                                                      954
         AG_THET = 0                                                      954
         AG_PHI = 0                                                       954
         AG_H1 = 2.43658                                                  954
         AG_BL1 = 0.377066                                                954
         AG_TL1 = 0.377066                                                954
         AG_ALP1 = -6                                                     954
         AG_H2 = 2.43658                                                  954
         AG_BL2 = 0.377066                                                954
         AG_TL2 = 0.377066                                                954
         AG_ALP2 = -6                                                     954
         AG_TITLE = 'TRAP'                                                954
         AG_EXNAME = 'SHAPE'                                              954
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       954
         CALL AXSHAPE                                                     954
C    *                                                                    954
6541  CALL AGSPOPB                                                        955
      GO TO BLOCK_PTLJ, (6540 ,6550)                                      955
C End of Local Procedure                                                  955
C ----------------------------------------------------------------------  956
C ------------ BHCA                                                       956
C Begin Local Procedure BHCA                                              957
C BHCA                                                                    957
6560  CONTINUE                                                            957
         AG_TITLE='BHCA'                                                  957
         AG_MODULE=MODULE                                                 957
         AG_MARK='V00'                                                    957
         CALL AGSPUSH                                                     957
         AG_MODULE=MODULE                                                 957
C    *                                                                    957
C    Geant ATTRIBUTE : BHCA ===> SEEN=1 COLO=920 <===                     958
         CALL AGSRESET                                                    958
         AG_SEEN = 1                                                      958
         AG_COLO = 920                                                    958
         AG_TITLE = 'BHCA'                                                958
         AG_EXNAME = 'ATTRIBUTE'                                          958
         AG_PARLIST = 'SEEN_COLO'                                         958
         CALL AXATTRIBUTE                                                 958
C    *                                                                    958
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        958
         CALL AGSRESET                                                    959
         AG_TITLE = 'ALUMINIUM'                                           959
         AG_EXNAME = 'MATERIAL'                                           959
         AG_PARLIST = 'NONE'                                              959
         CALL AXMATERIAL                                                  959
C    *                                                                    959
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=265.437, PHI2=36  959
C    0 DZ=BULLHORNLENGTH <===                                             959
         CALL AGSRESET                                                    961
         AG_RMIN = 0.023                                                  961
         AG_RMAX = 0.123                                                  961
         AG_PHI1 = 265.437                                                961
         AG_PHI2 = 360                                                    961
         AG_DZ = BULLHORNLENGTH                                           961
         AG_TITLE = 'TUBS'                                                961
         AG_EXNAME = 'SHAPE'                                              961
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            961
         CALL AXSHAPE                                                     961
C    *                                                                    961
6561  CALL AGSPOPB                                                        962
      GO TO BLOCK_BHCA, (6560 ,6570)                                      962
C End of Local Procedure                                                  962
C ----------------------------------------------------------------------  963
C ------------ BHCB                                                       963
C Begin Local Procedure BHCB                                              964
C BHCB                                                                    964
6580  CONTINUE                                                            964
         AG_TITLE='BHCB'                                                  964
         AG_MODULE=MODULE                                                 964
         AG_MARK='V00'                                                    964
         CALL AGSPUSH                                                     964
         AG_MODULE=MODULE                                                 964
C    *                                                                    964
C    Geant ATTRIBUTE : BHCB ===> SEEN=1 COLO=920 <===                     965
         CALL AGSRESET                                                    965
         AG_SEEN = 1                                                      965
         AG_COLO = 920                                                    965
         AG_TITLE = 'BHCB'                                                965
         AG_EXNAME = 'ATTRIBUTE'                                          965
         AG_PARLIST = 'SEEN_COLO'                                         965
         CALL AXATTRIBUTE                                                 965
C    *                                                                    965
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        965
         CALL AGSRESET                                                    966
         AG_TITLE = 'ALUMINIUM'                                           966
         AG_EXNAME = 'MATERIAL'                                           966
         AG_PARLIST = 'NONE'                                              966
         CALL AXMATERIAL                                                  966
C    *                                                                    966
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=0, PHI2=79.963 D  966
C    Z=BULLHORNLENGTH <===                                                966
         CALL AGSRESET                                                    968
         AG_RMIN = 0.023                                                  968
         AG_RMAX = 0.123                                                  968
         AG_PHI1 = 0                                                      968
         AG_PHI2 = 79.963                                                 968
         AG_DZ = BULLHORNLENGTH                                           968
         AG_TITLE = 'TUBS'                                                968
         AG_EXNAME = 'SHAPE'                                              968
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            968
         CALL AXSHAPE                                                     968
C    *                                                                    968
6581  CALL AGSPOPB                                                        969
      GO TO BLOCK_BHCB, (6580 ,6590)                                      969
C End of Local Procedure                                                  969
C ----------------------------------------------------------------------  970
C ------------ BHCC                                                       970
C Begin Local Procedure BHCC                                              971
C BHCC                                                                    971
6600  CONTINUE                                                            971
         AG_TITLE='BHCC'                                                  971
         AG_MODULE=MODULE                                                 971
         AG_MARK='V00'                                                    971
         CALL AGSPUSH                                                     971
         AG_MODULE=MODULE                                                 971
C    *                                                                    971
C    Geant ATTRIBUTE : BHCC ===> SEEN=1 COLO=920 <===                     972
         CALL AGSRESET                                                    972
         AG_SEEN = 1                                                      972
         AG_COLO = 920                                                    972
         AG_TITLE = 'BHCC'                                                972
         AG_EXNAME = 'ATTRIBUTE'                                          972
         AG_PARLIST = 'SEEN_COLO'                                         972
         CALL AXATTRIBUTE                                                 972
C    *                                                                    972
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        972
         CALL AGSRESET                                                    973
         AG_TITLE = 'ALUMINIUM'                                           973
         AG_EXNAME = 'MATERIAL'                                           973
         AG_PARLIST = 'NONE'                                              973
         CALL AXMATERIAL                                                  973
C    *                                                                    973
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=79.963, PHI2=191  973
C    .554 DZ=BULLHORNLENGTH <===                                          973
         CALL AGSRESET                                                    975
         AG_RMIN = 0.023                                                  975
         AG_RMAX = 0.123                                                  975
         AG_PHI1 = 79.963                                                 975
         AG_PHI2 = 191.554                                                975
         AG_DZ = BULLHORNLENGTH                                           975
         AG_TITLE = 'TUBS'                                                975
         AG_EXNAME = 'SHAPE'                                              975
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            975
         CALL AXSHAPE                                                     975
C    *                                                                    975
6601  CALL AGSPOPB                                                        976
      GO TO BLOCK_BHCC, (6600 ,6610)                                      976
C End of Local Procedure                                                  976
C ----------------------------------------------------------------------  977
C ------------ BHCD                                                       977
C Begin Local Procedure BHCD                                              978
C BHCD                                                                    978
6620  CONTINUE                                                            978
         AG_TITLE='BHCD'                                                  978
         AG_MODULE=MODULE                                                 978
         AG_MARK='V00'                                                    978
         CALL AGSPUSH                                                     978
         AG_MODULE=MODULE                                                 978
C    *                                                                    978
C    Geant ATTRIBUTE : BHCD ===> SEEN=1 COLO=920 <===                     979
         CALL AGSRESET                                                    979
         AG_SEEN = 1                                                      979
         AG_COLO = 920                                                    979
         AG_TITLE = 'BHCD'                                                979
         AG_EXNAME = 'ATTRIBUTE'                                          979
         AG_PARLIST = 'SEEN_COLO'                                         979
         CALL AXATTRIBUTE                                                 979
C    *                                                                    979
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        979
         CALL AGSRESET                                                    980
         AG_TITLE = 'ALUMINIUM'                                           980
         AG_EXNAME = 'MATERIAL'                                           980
         AG_PARLIST = 'NONE'                                              980
         CALL AXMATERIAL                                                  980
C    *                                                                    980
C    Geant SHAPE : TUBS ===> RMIN=0.177 RMAX=0.277 PHI1=271.963, PHI2=37  980
C    1.554 DZ=BULLHORNLENGTH <===                                         980
         CALL AGSRESET                                                    982
         AG_RMIN = 0.177                                                  982
         AG_RMAX = 0.277                                                  982
         AG_PHI1 = 271.963                                                982
         AG_PHI2 = 371.554                                                982
         AG_DZ = BULLHORNLENGTH                                           982
         AG_TITLE = 'TUBS'                                                982
         AG_EXNAME = 'SHAPE'                                              982
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            982
         CALL AXSHAPE                                                     982
C    *                                                                    982
6621  CALL AGSPOPB                                                        983
      GO TO BLOCK_BHCD, (6620 ,6630)                                      983
C End of Local Procedure                                                  983
C ----------------------------------------------------------------------  984
C ------------ BHCE                                                       984
C Begin Local Procedure BHCE                                              985
C BHCE                                                                    985
6640  CONTINUE                                                            985
         AG_TITLE='BHCE'                                                  985
         AG_MODULE=MODULE                                                 985
         AG_MARK='V00'                                                    985
         CALL AGSPUSH                                                     985
         AG_MODULE=MODULE                                                 985
C    *                                                                    985
C    Geant ATTRIBUTE : BHCE ===> SEEN=1 COLO=920 <===                     986
         CALL AGSRESET                                                    986
         AG_SEEN = 1                                                      986
         AG_COLO = 920                                                    986
         AG_TITLE = 'BHCE'                                                986
         AG_EXNAME = 'ATTRIBUTE'                                          986
         AG_PARLIST = 'SEEN_COLO'                                         986
         CALL AXATTRIBUTE                                                 986
C    *                                                                    986
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        986
         CALL AGSRESET                                                    987
         AG_TITLE = 'ALUMINIUM'                                           987
         AG_EXNAME = 'MATERIAL'                                           987
         AG_PARLIST = 'NONE'                                              987
         CALL AXMATERIAL                                                  987
C    *                                                                    987
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=91.963, PHI2=203  987
C    .554 DZ=BULLHORNLENGTH <===                                          987
         CALL AGSRESET                                                    989
         AG_RMIN = 0.023                                                  989
         AG_RMAX = 0.123                                                  989
         AG_PHI1 = 91.963                                                 989
         AG_PHI2 = 203.554                                                989
         AG_DZ = BULLHORNLENGTH                                           989
         AG_TITLE = 'TUBS'                                                989
         AG_EXNAME = 'SHAPE'                                              989
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            989
         CALL AXSHAPE                                                     989
C    *                                                                    989
6641  CALL AGSPOPB                                                        990
      GO TO BLOCK_BHCE, (6640 ,6650)                                      990
C End of Local Procedure                                                  990
C ----------------------------------------------------------------------  991
C ------------ BHCF                                                       991
C Begin Local Procedure BHCF                                              992
C BHCF                                                                    992
6660  CONTINUE                                                            992
         AG_TITLE='BHCF'                                                  992
         AG_MODULE=MODULE                                                 992
         AG_MARK='V00'                                                    992
         CALL AGSPUSH                                                     992
         AG_MODULE=MODULE                                                 992
C    *                                                                    992
C    Geant ATTRIBUTE : BHCF ===> SEEN=1 COLO=920 <===                     993
         CALL AGSRESET                                                    993
         AG_SEEN = 1                                                      993
         AG_COLO = 920                                                    993
         AG_TITLE = 'BHCF'                                                993
         AG_EXNAME = 'ATTRIBUTE'                                          993
         AG_PARLIST = 'SEEN_COLO'                                         993
         CALL AXATTRIBUTE                                                 993
C    *                                                                    993
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        993
         CALL AGSRESET                                                    994
         AG_TITLE = 'ALUMINIUM'                                           994
         AG_EXNAME = 'MATERIAL'                                           994
         AG_PARLIST = 'NONE'                                              994
         CALL AXMATERIAL                                                  994
C    *                                                                    994
C    Geant SHAPE : TUBS ===> RMIN=0.177 RMAX=0.277 PHI1=283.963, PHI2=38  994
C    3.554 DZ=BULLHORNLENGTH <===                                         994
         CALL AGSRESET                                                    996
         AG_RMIN = 0.177                                                  996
         AG_RMAX = 0.277                                                  996
         AG_PHI1 = 283.963                                                996
         AG_PHI2 = 383.554                                                996
         AG_DZ = BULLHORNLENGTH                                           996
         AG_TITLE = 'TUBS'                                                996
         AG_EXNAME = 'SHAPE'                                              996
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            996
         CALL AXSHAPE                                                     996
C    *                                                                    996
6661  CALL AGSPOPB                                                        997
      GO TO BLOCK_BHCF, (6660 ,6670)                                      997
C End of Local Procedure                                                  997
C ----------------------------------------------------------------------  998
C ------------ BHCG                                                       998
C Begin Local Procedure BHCG                                              999
C BHCG                                                                    999
6680  CONTINUE                                                            999
         AG_TITLE='BHCG'                                                  999
         AG_MODULE=MODULE                                                 999
         AG_MARK='V00'                                                    999
         CALL AGSPUSH                                                     999
         AG_MODULE=MODULE                                                 999
C    *                                                                    999
C    Geant ATTRIBUTE : BHCG ===> SEEN=1 COLO=920 <===                     1000
         CALL AGSRESET                                                    1000
         AG_SEEN = 1                                                      1000
         AG_COLO = 920                                                    1000
         AG_TITLE = 'BHCG'                                                1000
         AG_EXNAME = 'ATTRIBUTE'                                          1000
         AG_PARLIST = 'SEEN_COLO'                                         1000
         CALL AXATTRIBUTE                                                 1000
C    *                                                                    1000
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1000
         CALL AGSRESET                                                    1001
         AG_TITLE = 'ALUMINIUM'                                           1001
         AG_EXNAME = 'MATERIAL'                                           1001
         AG_PARLIST = 'NONE'                                              1001
         CALL AXMATERIAL                                                  1001
C    *                                                                    1001
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=103.963, PHI2=21  1001
C    6 DZ=BULLHORNLENGTH <===                                             1001
         CALL AGSRESET                                                    1003
         AG_RMIN = 0.023                                                  1003
         AG_RMAX = 0.123                                                  1003
         AG_PHI1 = 103.963                                                1003
         AG_PHI2 = 216                                                    1003
         AG_DZ = BULLHORNLENGTH                                           1003
         AG_TITLE = 'TUBS'                                                1003
         AG_EXNAME = 'SHAPE'                                              1003
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1003
         CALL AXSHAPE                                                     1003
C    *                                                                    1003
6681  CALL AGSPOPB                                                        1004
      GO TO BLOCK_BHCG, (6680 ,6690)                                      1004
C End of Local Procedure                                                  1004
C ----------------------------------------------------------------------  1005
C ------------ BHCH                                                       1005
C Begin Local Procedure BHCH                                              1006
C BHCH                                                                    1006
6700  CONTINUE                                                            1006
         AG_TITLE='BHCH'                                                  1006
         AG_MODULE=MODULE                                                 1006
         AG_MARK='V00'                                                    1006
         CALL AGSPUSH                                                     1006
         AG_MODULE=MODULE                                                 1006
C    *                                                                    1006
C    Geant ATTRIBUTE : BHCH ===> SEEN=1 COLO=920 <===                     1007
         CALL AGSRESET                                                    1007
         AG_SEEN = 1                                                      1007
         AG_COLO = 920                                                    1007
         AG_TITLE = 'BHCH'                                                1007
         AG_EXNAME = 'ATTRIBUTE'                                          1007
         AG_PARLIST = 'SEEN_COLO'                                         1007
         CALL AXATTRIBUTE                                                 1007
C    *                                                                    1007
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1007
         CALL AGSRESET                                                    1008
         AG_TITLE = 'ALUMINIUM'                                           1008
         AG_EXNAME = 'MATERIAL'                                           1008
         AG_PARLIST = 'NONE'                                              1008
         CALL AXMATERIAL                                                  1008
C    *                                                                    1008
C    Geant SHAPE : TUBS ===> RMIN=0.023 RMAX=0.123 PHI1=216, PHI2=265.43  1008
C    7 DZ=BULLHORNLENGTH <===                                             1008
         CALL AGSRESET                                                    1010
         AG_RMIN = 0.023                                                  1010
         AG_RMAX = 0.123                                                  1010
         AG_PHI1 = 216                                                    1010
         AG_PHI2 = 265.437                                                1010
         AG_DZ = BULLHORNLENGTH                                           1010
         AG_TITLE = 'TUBS'                                                1010
         AG_EXNAME = 'SHAPE'                                              1010
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1010
         CALL AXSHAPE                                                     1010
C    *                                                                    1010
6701  CALL AGSPOPB                                                        1011
      GO TO BLOCK_BHCH, (6700 ,6710)                                      1011
C End of Local Procedure                                                  1011
C ----------------------------------------------------------------------  1012
C ------------ BHSA                                                       1012
C Begin Local Procedure BHSA                                              1013
C BHSA BULLHORN SECTOR RIGHT SIDE                                         1013
6720  CONTINUE                                                            1013
         AG_TITLE='BHSA'                                                  1013
         AG_MODULE=MODULE                                                 1013
         AG_MARK='V00'                                                    1013
         CALL AGSPUSH                                                     1013
         AG_MODULE=MODULE                                                 1013
C    *                                                                    1013
C    Geant ATTRIBUTE : BHSA ===> SEEN=1 COLO=920 <===                     1014
         CALL AGSRESET                                                    1014
         AG_SEEN = 1                                                      1014
         AG_COLO = 920                                                    1014
         AG_TITLE = 'BHSA'                                                1014
         AG_EXNAME = 'ATTRIBUTE'                                          1014
         AG_PARLIST = 'SEEN_COLO'                                         1014
         CALL AXATTRIBUTE                                                 1014
C    *                                                                    1014
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1014
         CALL AGSRESET                                                    1015
         AG_TITLE = 'ALUMINIUM'                                           1015
         AG_EXNAME = 'MATERIAL'                                           1015
         AG_PARLIST = 'NONE'                                              1015
         CALL AXMATERIAL                                                  1015
C    *                                                                    1015
C    Geant SHAPE : BOX ===> DX=2.43522 DY=BULLHORNHALFTHICKNESS, DZ=BULL  1015
C    HORNLENGTH <===                                                      1015
         CALL AGSRESET                                                    1017
         AG_DX = 2.43522                                                  1017
         AG_DY = BULLHORNHALFTHICKNESS                                    1017
         AG_DZ = BULLHORNLENGTH                                           1017
         AG_TITLE = 'BOX'                                                 1017
         AG_EXNAME = 'SHAPE'                                              1017
         AG_PARLIST = 'DX_DY_DZ'                                          1017
         CALL AXSHAPE                                                     1017
C    *                                                                    1017
6721  CALL AGSPOPB                                                        1018
      GO TO BLOCK_BHSA, (6720 ,6730)                                      1018
C End of Local Procedure                                                  1018
C ----------------------------------------------------------------------  1019
C ------------ BHSB                                                       1019
C Begin Local Procedure BHSB                                              1020
C BHSB BULLHORN SECTOR RIGHT UPPER SIDE                                   1020
6740  CONTINUE                                                            1020
         AG_TITLE='BHSB'                                                  1020
         AG_MODULE=MODULE                                                 1020
         AG_MARK='V00'                                                    1020
         CALL AGSPUSH                                                     1020
         AG_MODULE=MODULE                                                 1020
C    *                                                                    1020
C    Geant ATTRIBUTE : BHSB ===> SEEN=1 COLO=920 <===                     1021
         CALL AGSRESET                                                    1021
         AG_SEEN = 1                                                      1021
         AG_COLO = 920                                                    1021
         AG_TITLE = 'BHSB'                                                1021
         AG_EXNAME = 'ATTRIBUTE'                                          1021
         AG_PARLIST = 'SEEN_COLO'                                         1021
         CALL AXATTRIBUTE                                                 1021
C    *                                                                    1021
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1021
         CALL AGSRESET                                                    1022
         AG_TITLE = 'ALUMINIUM'                                           1022
         AG_EXNAME = 'MATERIAL'                                           1022
         AG_PARLIST = 'NONE'                                              1022
         CALL AXMATERIAL                                                  1022
C    *                                                                    1022
C    Geant SHAPE : BOX ===> DX=0.625575 DY=BULLHORNHALFTHICKNESS, DZ=BUL  1022
C    LHORNLENGTH <===                                                     1022
         CALL AGSRESET                                                    1024
         AG_DX = 0.625575                                                 1024
         AG_DY = BULLHORNHALFTHICKNESS                                    1024
         AG_DZ = BULLHORNLENGTH                                           1024
         AG_TITLE = 'BOX'                                                 1024
         AG_EXNAME = 'SHAPE'                                              1024
         AG_PARLIST = 'DX_DY_DZ'                                          1024
         CALL AXSHAPE                                                     1024
C    *                                                                    1024
6741  CALL AGSPOPB                                                        1025
      GO TO BLOCK_BHSB, (6740 ,6750)                                      1025
C End of Local Procedure                                                  1025
C ----------------------------------------------------------------------  1026
C ------------ BHSC                                                       1026
C Begin Local Procedure BHSC                                              1027
C BHSC BULLHORN SECTOR RIGHT UPPER SIDE                                   1027
6760  CONTINUE                                                            1027
         AG_TITLE='BHSC'                                                  1027
         AG_MODULE=MODULE                                                 1027
         AG_MARK='V00'                                                    1027
         CALL AGSPUSH                                                     1027
         AG_MODULE=MODULE                                                 1027
C    *                                                                    1027
C    Geant ATTRIBUTE : BHSC ===> SEEN=1 COLO=920 <===                     1028
         CALL AGSRESET                                                    1028
         AG_SEEN = 1                                                      1028
         AG_COLO = 920                                                    1028
         AG_TITLE = 'BHSC'                                                1028
         AG_EXNAME = 'ATTRIBUTE'                                          1028
         AG_PARLIST = 'SEEN_COLO'                                         1028
         CALL AXATTRIBUTE                                                 1028
C    *                                                                    1028
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1028
         CALL AGSRESET                                                    1029
         AG_TITLE = 'ALUMINIUM'                                           1029
         AG_EXNAME = 'MATERIAL'                                           1029
         AG_PARLIST = 'NONE'                                              1029
         CALL AXMATERIAL                                                  1029
C    *                                                                    1029
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=BULLHORNHALFTHICKNESS, DZ=BU  1029
C    LLHORNLENGTH <===                                                    1029
         CALL AGSRESET                                                    1031
         AG_DX = 0.0389368                                                1031
         AG_DY = BULLHORNHALFTHICKNESS                                    1031
         AG_DZ = BULLHORNLENGTH                                           1031
         AG_TITLE = 'BOX'                                                 1031
         AG_EXNAME = 'SHAPE'                                              1031
         AG_PARLIST = 'DX_DY_DZ'                                          1031
         CALL AXSHAPE                                                     1031
C    *                                                                    1031
6761  CALL AGSPOPB                                                        1032
      GO TO BLOCK_BHSC, (6760 ,6770)                                      1032
C End of Local Procedure                                                  1032
C ----------------------------------------------------------------------  1033
C ------------ BHSD                                                       1033
C Begin Local Procedure BHSD                                              1034
C BHSD BULLHORN SECTOR RIGHT UPPER SIDE                                   1034
6780  CONTINUE                                                            1034
         AG_TITLE='BHSD'                                                  1034
         AG_MODULE=MODULE                                                 1034
         AG_MARK='V00'                                                    1034
         CALL AGSPUSH                                                     1034
         AG_MODULE=MODULE                                                 1034
C    *                                                                    1034
C    Geant ATTRIBUTE : BHSD ===> SEEN=1 COLO=920 <===                     1035
         CALL AGSRESET                                                    1035
         AG_SEEN = 1                                                      1035
         AG_COLO = 920                                                    1035
         AG_TITLE = 'BHSD'                                                1035
         AG_EXNAME = 'ATTRIBUTE'                                          1035
         AG_PARLIST = 'SEEN_COLO'                                         1035
         CALL AXATTRIBUTE                                                 1035
C    *                                                                    1035
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1035
         CALL AGSRESET                                                    1036
         AG_TITLE = 'ALUMINIUM'                                           1036
         AG_EXNAME = 'MATERIAL'                                           1036
         AG_PARLIST = 'NONE'                                              1036
         CALL AXMATERIAL                                                  1036
C    *                                                                    1036
C    Geant SHAPE : BOX ===> DX=0.681643 DY=BULLHORNHALFTHICKNESS, DZ=BUL  1036
C    LHORNLENGTH <===                                                     1036
         CALL AGSRESET                                                    1038
         AG_DX = 0.681643                                                 1038
         AG_DY = BULLHORNHALFTHICKNESS                                    1038
         AG_DZ = BULLHORNLENGTH                                           1038
         AG_TITLE = 'BOX'                                                 1038
         AG_EXNAME = 'SHAPE'                                              1038
         AG_PARLIST = 'DX_DY_DZ'                                          1038
         CALL AXSHAPE                                                     1038
C    *                                                                    1038
6781  CALL AGSPOPB                                                        1039
      GO TO BLOCK_BHSD, (6780 ,6790)                                      1039
C End of Local Procedure                                                  1039
C ----------------------------------------------------------------------  1040
C ------------ BHSE                                                       1040
C Begin Local Procedure BHSE                                              1041
C BHSE BULLHORN SECTOR RIGHT UPPER SIDE                                   1041
6800  CONTINUE                                                            1041
         AG_TITLE='BHSE'                                                  1041
         AG_MODULE=MODULE                                                 1041
         AG_MARK='V00'                                                    1041
         CALL AGSPUSH                                                     1041
         AG_MODULE=MODULE                                                 1041
C    *                                                                    1041
C    Geant ATTRIBUTE : BHSE ===> SEEN=1 COLO=920 <===                     1042
         CALL AGSRESET                                                    1042
         AG_SEEN = 1                                                      1042
         AG_COLO = 920                                                    1042
         AG_TITLE = 'BHSE'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1042
         CALL AGSRESET                                                    1043
         AG_TITLE = 'ALUMINIUM'                                           1043
         AG_EXNAME = 'MATERIAL'                                           1043
         AG_PARLIST = 'NONE'                                              1043
         CALL AXMATERIAL                                                  1043
C    *                                                                    1043
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=BULLHORNHALFTHICKNESS, DZ=BU  1043
C    LLHORNLENGTH <===                                                    1043
         CALL AGSRESET                                                    1045
         AG_DX = 0.0389368                                                1045
         AG_DY = BULLHORNHALFTHICKNESS                                    1045
         AG_DZ = BULLHORNLENGTH                                           1045
         AG_TITLE = 'BOX'                                                 1045
         AG_EXNAME = 'SHAPE'                                              1045
         AG_PARLIST = 'DX_DY_DZ'                                          1045
         CALL AXSHAPE                                                     1045
C    *                                                                    1045
6801  CALL AGSPOPB                                                        1046
      GO TO BLOCK_BHSE, (6800 ,6810)                                      1046
C End of Local Procedure                                                  1046
C ----------------------------------------------------------------------  1047
C ------------ BHSF                                                       1047
C Begin Local Procedure BHSF                                              1048
C BHSF BULLHORN SECTOR RIGHT UPPER SIDE                                   1048
6820  CONTINUE                                                            1048
         AG_TITLE='BHSF'                                                  1048
         AG_MODULE=MODULE                                                 1048
         AG_MARK='V00'                                                    1048
         CALL AGSPUSH                                                     1048
         AG_MODULE=MODULE                                                 1048
C    *                                                                    1048
C    Geant ATTRIBUTE : BHSF ===> SEEN=1 COLO=920 <===                     1049
         CALL AGSRESET                                                    1049
         AG_SEEN = 1                                                      1049
         AG_COLO = 920                                                    1049
         AG_TITLE = 'BHSF'                                                1049
         AG_EXNAME = 'ATTRIBUTE'                                          1049
         AG_PARLIST = 'SEEN_COLO'                                         1049
         CALL AXATTRIBUTE                                                 1049
C    *                                                                    1049
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1049
         CALL AGSRESET                                                    1050
         AG_TITLE = 'ALUMINIUM'                                           1050
         AG_EXNAME = 'MATERIAL'                                           1050
         AG_PARLIST = 'NONE'                                              1050
         CALL AXMATERIAL                                                  1050
C    *                                                                    1050
C    Geant SHAPE : BOX ===> DX=0.661849 DY=BULLHORNHALFTHICKNESS, DZ=BUL  1050
C    LHORNLENGTH <===                                                     1050
         CALL AGSRESET                                                    1052
         AG_DX = 0.661849                                                 1052
         AG_DY = BULLHORNHALFTHICKNESS                                    1052
         AG_DZ = BULLHORNLENGTH                                           1052
         AG_TITLE = 'BOX'                                                 1052
         AG_EXNAME = 'SHAPE'                                              1052
         AG_PARLIST = 'DX_DY_DZ'                                          1052
         CALL AXSHAPE                                                     1052
C    *                                                                    1052
6821  CALL AGSPOPB                                                        1053
      GO TO BLOCK_BHSF, (6820 ,6830)                                      1053
C End of Local Procedure                                                  1053
C ----------------------------------------------------------------------  1054
C ------------ BHSG                                                       1054
C Begin Local Procedure BHSG                                              1055
C BHSG BULLHORN SECTOR RIGHT UPPER SIDE                                   1055
6840  CONTINUE                                                            1055
         AG_TITLE='BHSG'                                                  1055
         AG_MODULE=MODULE                                                 1055
         AG_MARK='V00'                                                    1055
         CALL AGSPUSH                                                     1055
         AG_MODULE=MODULE                                                 1055
C    *                                                                    1055
C    Geant ATTRIBUTE : BHSG ===> SEEN=1 COLO=920 <===                     1056
         CALL AGSRESET                                                    1056
         AG_SEEN = 1                                                      1056
         AG_COLO = 920                                                    1056
         AG_TITLE = 'BHSG'                                                1056
         AG_EXNAME = 'ATTRIBUTE'                                          1056
         AG_PARLIST = 'SEEN_COLO'                                         1056
         CALL AXATTRIBUTE                                                 1056
C    *                                                                    1056
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1056
         CALL AGSRESET                                                    1057
         AG_TITLE = 'ALUMINIUM'                                           1057
         AG_EXNAME = 'MATERIAL'                                           1057
         AG_PARLIST = 'NONE'                                              1057
         CALL AXMATERIAL                                                  1057
C    *                                                                    1057
C    Geant SHAPE : BOX ===> DX=2.26944 DY=BULLHORNHALFTHICKNESS, DZ=BULL  1057
C    HORNLENGTH <===                                                      1057
         CALL AGSRESET                                                    1059
         AG_DX = 2.26944                                                  1059
         AG_DY = BULLHORNHALFTHICKNESS                                    1059
         AG_DZ = BULLHORNLENGTH                                           1059
         AG_TITLE = 'BOX'                                                 1059
         AG_EXNAME = 'SHAPE'                                              1059
         AG_PARLIST = 'DX_DY_DZ'                                          1059
         CALL AXSHAPE                                                     1059
C    *                                                                    1059
6841  CALL AGSPOPB                                                        1060
      GO TO BLOCK_BHSG, (6840 ,6850)                                      1060
C End of Local Procedure                                                  1060
C ----------------------------------------------------------------------  1061
C ------------ BHSH                                                       1061
C Begin Local Procedure BHSH                                              1062
C BHSH BULLHORN SECTOR RIGHT UPPER SIDE                                   1062
6860  CONTINUE                                                            1062
         AG_TITLE='BHSH'                                                  1062
         AG_MODULE=MODULE                                                 1062
         AG_MARK='V00'                                                    1062
         CALL AGSPUSH                                                     1062
         AG_MODULE=MODULE                                                 1062
C    *                                                                    1062
C    Geant ATTRIBUTE : BHSH ===> SEEN=1 COLO=920 <===                     1063
         CALL AGSRESET                                                    1063
         AG_SEEN = 1                                                      1063
         AG_COLO = 920                                                    1063
         AG_TITLE = 'BHSH'                                                1063
         AG_EXNAME = 'ATTRIBUTE'                                          1063
         AG_PARLIST = 'SEEN_COLO'                                         1063
         CALL AXATTRIBUTE                                                 1063
C    *                                                                    1063
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1063
         CALL AGSRESET                                                    1064
         AG_TITLE = 'ALUMINIUM'                                           1064
         AG_EXNAME = 'MATERIAL'                                           1064
         AG_PARLIST = 'NONE'                                              1064
         CALL AXMATERIAL                                                  1064
C    *                                                                    1064
C    Geant SHAPE : BOX ===> DX=0.827988 DY=BULLHORNHALFTHICKNESS, DZ=BUL  1064
C    LHORNLENGTH <===                                                     1064
         CALL AGSRESET                                                    1066
         AG_DX = 0.827988                                                 1066
         AG_DY = BULLHORNHALFTHICKNESS                                    1066
         AG_DZ = BULLHORNLENGTH                                           1066
         AG_TITLE = 'BOX'                                                 1066
         AG_EXNAME = 'SHAPE'                                              1066
         AG_PARLIST = 'DX_DY_DZ'                                          1066
         CALL AXSHAPE                                                     1066
C    *                                                                    1066
6861  CALL AGSPOPB                                                        1067
      GO TO BLOCK_BHSH, (6860 ,6870)                                      1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ PTOB                                                       1068
C Begin Local Procedure PTOB                                              1069
C PTOB                                                                    1069
6880  CONTINUE                                                            1069
         AG_TITLE='PTOB'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant ATTRIBUTE : PTOB ===> SEEN=1 COLO=920 <===                     1070
         CALL AGSRESET                                                    1070
         AG_SEEN = 1                                                      1070
         AG_COLO = 920                                                    1070
         AG_TITLE = 'PTOB'                                                1070
         AG_EXNAME = 'ATTRIBUTE'                                          1070
         AG_PARLIST = 'SEEN_COLO'                                         1070
         CALL AXATTRIBUTE                                                 1070
C    *                                                                    1070
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1070
         CALL AGSRESET                                                    1071
         AG_TITLE = 'ALUMINIUM'                                           1071
         AG_EXNAME = 'MATERIAL'                                           1071
         AG_PARLIST = 'NONE'                                              1071
         CALL AXMATERIAL                                                  1071
C    *                                                                    1071
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.15875 DZ=2.225 <===            1071
         CALL AGSRESET                                                    1072
         AG_RMIN = 0                                                      1072
         AG_RMAX = 0.15875                                                1072
         AG_DZ = 2.225                                                    1072
         AG_TITLE = 'TUBE'                                                1072
         AG_EXNAME = 'SHAPE'                                              1072
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1072
         CALL AXSHAPE                                                     1072
C    *                                                                    1072
6881  CALL AGSPOPB                                                        1073
      GO TO BLOCK_PTOB, (6880 ,6890)                                      1073
C End of Local Procedure                                                  1073
C ----------------------------------------------------------------------  1074
C ------------ PCAD                                                       1074
C Begin Local Procedure PCAD                                              1075
C PCAD LONG CABLE SECTION D                                               1075
6960  CONTINUE                                                            1075
         AG_TITLE='PCAD'                                                  1075
         AG_MODULE=MODULE                                                 1075
         AG_MARK='V00'                                                    1075
         CALL AGSPUSH                                                     1075
         AG_MODULE=MODULE                                                 1075
C    *                                                                    1075
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=629 <===                     1076
         CALL AGSRESET                                                    1076
         AG_SEEN = 1                                                      1076
         AG_COLO = 629                                                    1076
         AG_TITLE = 'NONE'                                                1076
         AG_EXNAME = 'ATTRIBUTE'                                          1076
         AG_PARLIST = 'SEEN_COLO'                                         1076
         CALL AXATTRIBUTE                                                 1076
C    *                                                                    1076
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           1076
         CALL AGSRESET                                                    1077
         AG_TITLE = 'COPPER'                                              1077
         AG_EXNAME = 'MATERIAL'                                           1077
         AG_PARLIST = 'NONE'                                              1077
         CALL AXMATERIAL                                                  1077
C    *                                                                    1077
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=RADCABLE PHI1=0 PHI2=360, DZ=LE  1077
C    NGTHCAL/2 <===                                                       1077
         CALL AGSRESET                                                    1079
         AG_RMIN = 0                                                      1079
         AG_RMAX = RADCABLE                                               1079
         AG_PHI1 = 0                                                      1079
         AG_PHI2 = 360                                                    1079
         AG_DZ = LENGTHCAL/2                                              1079
         AG_TITLE = 'TUBS'                                                1079
         AG_EXNAME = 'SHAPE'                                              1079
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1079
         CALL AXSHAPE                                                     1079
C    *                                                                    1079
6961  CALL AGSPOPB                                                        1080
      GO TO BLOCK_PCAD, (6960 ,6970)                                      1080
C End of Local Procedure                                                  1080
C ----------------------------------------------------------------------  1081
C ------------ LADR                                                       1081
C Begin Local Procedure LADR                                              1082
C LADR FIRST LADDER PROTOTYPE                                             1082
6900  CONTINUE                                                            1082
         AG_TITLE='LADR'                                                  1082
         AG_MODULE=MODULE                                                 1082
         AG_MARK='V00'                                                    1082
         CALL AGSPUSH                                                     1082
         AG_MODULE=MODULE                                                 1082
C    *                                                                    1082
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1083
         CALL AGSRESET                                                    1083
         AG_TITLE = 'AIR'                                                 1083
         AG_EXNAME = 'MATERIAL'                                           1083
         AG_PARLIST = 'NONE'                                              1083
         CALL AXMATERIAL                                                  1083
C    *                                                                    1083
C    Geant MEDIUM : MEDAIR ===> EPSIL=0.001 <===                          1083
         CALL AGSRESET                                                    1084
         AG_EPSIL = 0.001                                                 1084
         AG_TITLE = 'MEDAIR'                                              1084
         AG_EXNAME = 'MEDIUM'                                             1084
         AG_PARLIST = 'EPSIL'                                             1084
         CALL AXMEDIUM                                                    1084
C    *                                                                    1084
C    Geant ATTRIBUTE : LADR ===> SEEN=1 COLO=920 <===                     1084
         CALL AGSRESET                                                    1085
         AG_SEEN = 1                                                      1085
         AG_COLO = 920                                                    1085
         AG_TITLE = 'LADR'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO'                                         1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant SHAPE : BOX ===> DX=1.245 DY=LADDERHALFTHICKNESS, DZ=15.325 <  1085
C    ===                                                                  1085
         CALL AGSRESET                                                    1087
         AG_DX = 1.245                                                    1087
         AG_DY = LADDERHALFTHICKNESS                                      1087
         AG_DZ = 15.325                                                   1087
         AG_TITLE = 'BOX'                                                 1087
         AG_EXNAME = 'SHAPE'                                              1087
         AG_PARLIST = 'DX_DY_DZ'                                          1087
         CALL AXSHAPE                                                     1087
C    *                                                                    1087
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS   1087
     *   - GLUBTHICKNESS - LADR_CABLETHICKNESS - GLUATHICKNESS -          1087
     *   0.0025)                                                          1087
C    *                                                                    1090
         SENS=1                                                           1090
            GO TO 6983                                                    1090
6981        SENS=SENS+1                                                   1090
6983        IF(SENS-(SENP_SENSOR).GT.0) GO TO 6982                        1090
            ZSENSOR = SENP_ZPOS(SENS)                                     1091
C       Create PXSI :                                                     1092
C       Call To Local Procedure PXSI                                      1092
            ASSIGN 7000 TO BLOCK_PXSI                                     1092
            GO TO  6990                                                   1092
7000        CONTINUE                                                      1092
C       *                                                                 1092
C       Geant POSITION : PXSI ===> MOTHER='LADR' X=0.0848 Y=YP Z=ZSENSOR  1093
C        <===                                                             1093
            CALL AGSRESET                                                 1093
            AG_MOTHER = 'LADR'                                            1093
            AG_X = 0.0848                                                 1093
            AG_Y = YP                                                     1093
            AG_Z = ZSENSOR                                                1093
            AG_TITLE = 'PXSI'                                             1093
            AG_EXNAME = 'POSITION'                                        1093
            AG_PARLIST = 'MOTHER_X_Y_Z'                                   1093
            CALL AXPOSITION                                               1093
C       *                                                                 1093
         GO TO 6981                                                       1094
6982     CONTINUE                                                         1094
         YP = YP - 0.005                                                  1095
C    Create GLUA :                                                        1096
C    Call To Local Procedure GLUA                                         1096
         ASSIGN 7020 TO BLOCK_GLUA                                        1096
         GO TO  7010                                                      1096
7020     CONTINUE                                                         1096
C    *                                                                    1096
C    Geant POSITION : GLUA ===> MOTHER='LADR' X=0.0485 Y=YP Z=4.8855 <==  1097
C    =                                                                    1097
         CALL AGSRESET                                                    1097
         AG_MOTHER = 'LADR'                                               1097
         AG_X = 0.0485                                                    1097
         AG_Y = YP                                                        1097
         AG_Z = 4.8855                                                    1097
         AG_TITLE = 'GLUA'                                                1097
         AG_EXNAME = 'POSITION'                                           1097
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      1097
         CALL AXPOSITION                                                  1097
C    *                                                                    1097
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS   1097
     *   - GLUBTHICKNESS - LADR_CABLETHICKNESS/2.0)                       1097
C    Create ALCA :                                                        1100
C    Call To Local Procedure ALCA                                         1100
         ASSIGN 7040 TO BLOCK_ALCA                                        1100
         GO TO  7030                                                      1100
7040     CONTINUE                                                         1100
C    *                                                                    1100
C    Geant POSITION : ALCA ===> X=0.01475 Y=YP Z=CABLEZOFFSET <===        1101
         CALL AGSRESET                                                    1101
         AG_X = 0.01475                                                   1101
         AG_Y = YP                                                        1101
         AG_Z = CABLEZOFFSET                                              1101
         AG_TITLE = 'ALCA'                                                1101
         AG_EXNAME = 'POSITION'                                           1101
         AG_PARLIST = 'X_Y_Z'                                             1101
         CALL AXPOSITION                                                  1101
C    *                                                                    1101
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS   1101
     *   - GLUBTHICKNESS/2)                                               1101
C    Create GLUB :                                                        1103
C    Call To Local Procedure GLUB                                         1103
         ASSIGN 7060 TO BLOCK_GLUB                                        1103
         GO TO  7050                                                      1103
7060     CONTINUE                                                         1103
C    *                                                                    1103
C    Geant POSITION : GLUB ===> X=0.0 Y=YP Z=0.065 <===                   1104
         CALL AGSRESET                                                    1104
         AG_X = 0.0                                                       1104
         AG_Y = YP                                                        1104
         AG_Z = 0.065                                                     1104
         AG_TITLE = 'GLUB'                                                1104
         AG_EXNAME = 'POSITION'                                           1104
         AG_PARLIST = 'X_Y_Z'                                             1104
         CALL AXPOSITION                                                  1104
C    *                                                                    1104
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS/  1104
     *   2.0)                                                             1104
C    Create CFBK :                                                        1106
C    Call To Local Procedure CFBK                                         1106
         ASSIGN 7080 TO BLOCK_CFBK                                        1106
         GO TO  7070                                                      1106
7080     CONTINUE                                                         1106
C    *                                                                    1106
C    Geant POSITION : CFBK ===> X=0.0 Y=YP Z=BACKINGZOFFSET <===          1107
         CALL AGSRESET                                                    1107
         AG_X = 0.0                                                       1107
         AG_Y = YP                                                        1107
         AG_Z = BACKINGZOFFSET                                            1107
         AG_TITLE = 'CFBK'                                                1107
         AG_EXNAME = 'POSITION'                                           1107
         AG_PARLIST = 'X_Y_Z'                                             1107
         CALL AXPOSITION                                                  1107
C    *                                                                    1107
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS/2.0)                 1107
C    Create GLUC :                                                        1109
C    Call To Local Procedure GLUC                                         1109
         ASSIGN 7100 TO BLOCK_GLUC                                        1109
         GO TO  7090                                                      1109
7100     CONTINUE                                                         1109
C    *                                                                    1109
C    Geant POSITION : GLUC ===> X=0.4995 Y=YP Z=0.065 <===                1110
         CALL AGSRESET                                                    1110
         AG_X = 0.4995                                                    1110
         AG_Y = YP                                                        1110
         AG_Z = 0.065                                                     1110
         AG_TITLE = 'GLUC'                                                1110
         AG_EXNAME = 'POSITION'                                           1110
         AG_PARLIST = 'X_Y_Z'                                             1110
         CALL AXPOSITION                                                  1110
C    *                                                                    1110
C    Create DRIV :                                                        1111
C    Call To Local Procedure DRIV                                         1111
         ASSIGN 7120 TO BLOCK_DRIV                                        1111
         GO TO  7110                                                      1111
7120     CONTINUE                                                         1111
C    *                                                                    1111
C    Geant POSITION : DRIV ===> X=0.01475 Y=0.02725 Z=-10.589 <===        1112
         CALL AGSRESET                                                    1112
         AG_X = 0.01475                                                   1112
         AG_Y = 0.02725                                                   1112
         AG_Z = -10.589                                                   1112
         AG_TITLE = 'DRIV'                                                1112
         AG_EXNAME = 'POSITION'                                           1112
         AG_PARLIST = 'X_Y_Z'                                             1112
         CALL AXPOSITION                                                  1112
C    *                                                                    1112
6901  CALL AGSPOPB                                                        1113
      GO TO BLOCK_LADR, (6900 ,6910,6920,6930,6940)                       1113
C End of Local Procedure                                                  1113
C ----------------------------------------------------------------------  1114
C ------------ GLUA                                                       1114
C Begin Local Procedure GLUA                                              1115
C GLUA GLUE FOR CMOS SENSORS                                              1115
7010  CONTINUE                                                            1115
         AG_TITLE='GLUA'                                                  1115
         AG_MODULE=MODULE                                                 1115
         AG_MARK='V00'                                                    1115
         CALL AGSPUSH                                                     1115
         AG_MODULE=MODULE                                                 1115
C    *                                                                    1115
C    Geant ATTRIBUTE : GLUA ===> SEEN=1 COLO=800 <===                     1116
         CALL AGSRESET                                                    1116
         AG_SEEN = 1                                                      1116
         AG_COLO = 800                                                    1116
         AG_TITLE = 'GLUA'                                                1116
         AG_EXNAME = 'ATTRIBUTE'                                          1116
         AG_PARLIST = 'SEEN_COLO'                                         1116
         CALL AXATTRIBUTE                                                 1116
C    *                                                                    1116
C    Geant MATERIAL : MATADHESIVE ===> inherited arguments used <===      1116
         CALL AGSRESET                                                    1117
         AG_TITLE = 'MATADHESIVE'                                         1117
         AG_EXNAME = 'MATERIAL'                                           1117
         AG_PARLIST = 'NONE'                                              1117
         CALL AXMATERIAL                                                  1117
C    *                                                                    1117
C    Geant SHAPE : BOX ===> DX=1.1715 DY=GLUATHICKNESS/2.0, DZ=10.1295 <  1117
C    ===                                                                  1117
         CALL AGSRESET                                                    1119
         AG_DX = 1.1715                                                   1119
         AG_DY = GLUATHICKNESS/2.0                                        1119
         AG_DZ = 10.1295                                                  1119
         AG_TITLE = 'BOX'                                                 1119
         AG_EXNAME = 'SHAPE'                                              1119
         AG_PARLIST = 'DX_DY_DZ'                                          1119
         CALL AXSHAPE                                                     1119
C    *                                                                    1119
7011  CALL AGSPOPB                                                        1120
      GO TO BLOCK_GLUA, (7010 ,7020)                                      1120
C End of Local Procedure                                                  1120
C ----------------------------------------------------------------------  1121
C ------------ ALCA                                                       1121
C Begin Local Procedure ALCA                                              1122
C ALCA                                                                    1122
7030  CONTINUE                                                            1122
         AG_TITLE='ALCA'                                                  1122
         AG_MODULE=MODULE                                                 1122
         AG_MARK='V00'                                                    1122
         CALL AGSPUSH                                                     1122
         AG_MODULE=MODULE                                                 1122
C    *                                                                    1122
C    Geant ATTRIBUTE : ALCA ===> SEEN=1 COLO=616 <===                     1123
         CALL AGSRESET                                                    1123
         AG_SEEN = 1                                                      1123
         AG_COLO = 616                                                    1123
         AG_TITLE = 'ALCA'                                                1123
         AG_EXNAME = 'ATTRIBUTE'                                          1123
         AG_PARLIST = 'SEEN_COLO'                                         1123
         CALL AXATTRIBUTE                                                 1123
C    *                                                                    1123
C    Check LADR_TYPE==0                                                   1123
         IF (LADR_TYPE.EQ.0) THEN                                         1124
C    Geant MATERIAL : MATALCABLE ===> inherited arguments used <===       1125
         CALL AGSRESET                                                    1125
         AG_TITLE = 'MATALCABLE'                                          1125
         AG_EXNAME = 'MATERIAL'                                           1125
         AG_PARLIST = 'NONE'                                              1125
         CALL AXMATERIAL                                                  1125
C    *                                                                    1125
         ELSE                                                             1125
C    Geant MATERIAL : MATCUCABLE ===> inherited arguments used <===       1127
         CALL AGSRESET                                                    1127
         AG_TITLE = 'MATCUCABLE'                                          1127
         AG_EXNAME = 'MATERIAL'                                           1127
         AG_PARLIST = 'NONE'                                              1127
         CALL AXMATERIAL                                                  1127
C    *                                                                    1127
         ENDIF                                                            1127
C    Geant SHAPE : BOX ===> DX=1.23025 DY=LADR_CABLETHICKNESS/2.0, DZ=CA  1130
C    BLELENGTH <===                                                       1130
         CALL AGSRESET                                                    1130
         AG_DX = 1.23025                                                  1130
         AG_DY = LADR_CABLETHICKNESS/2.0                                  1130
         AG_DZ = CABLELENGTH                                              1130
         AG_TITLE = 'BOX'                                                 1130
         AG_EXNAME = 'SHAPE'                                              1130
         AG_PARLIST = 'DX_DY_DZ'                                          1130
         CALL AXSHAPE                                                     1130
C    *                                                                    1130
7031  CALL AGSPOPB                                                        1131
      GO TO BLOCK_ALCA, (7030 ,7040)                                      1131
C End of Local Procedure                                                  1131
C ----------------------------------------------------------------------  1132
C ------------ GLUB                                                       1132
C Begin Local Procedure GLUB                                              1133
C GLUB GLUE FOR AL/CU CABLE                                               1133
7050  CONTINUE                                                            1133
         AG_TITLE='GLUB'                                                  1133
         AG_MODULE=MODULE                                                 1133
         AG_MARK='V00'                                                    1133
         CALL AGSPUSH                                                     1133
         AG_MODULE=MODULE                                                 1133
C    *                                                                    1133
C    Geant ATTRIBUTE : GLUB ===> SEEN=1 COLO=629 <===                     1134
         CALL AGSRESET                                                    1134
         AG_SEEN = 1                                                      1134
         AG_COLO = 629                                                    1134
         AG_TITLE = 'GLUB'                                                1134
         AG_EXNAME = 'ATTRIBUTE'                                          1134
         AG_PARLIST = 'SEEN_COLO'                                         1134
         CALL AXATTRIBUTE                                                 1134
C    *                                                                    1134
C    Geant MATERIAL : MATADHESIVE ===> inherited arguments used <===      1134
         CALL AGSRESET                                                    1135
         AG_TITLE = 'MATADHESIVE'                                         1135
         AG_EXNAME = 'MATERIAL'                                           1135
         AG_PARLIST = 'NONE'                                              1135
         CALL AXMATERIAL                                                  1135
C    *                                                                    1135
C    Geant SHAPE : BOX ===> DX=1.245 DY=GLUBTHICKNESS/2.0, DZ=14.95 <===  1135
         CALL AGSRESET                                                    1137
         AG_DX = 1.245                                                    1137
         AG_DY = GLUBTHICKNESS/2.0                                        1137
         AG_DZ = 14.95                                                    1137
         AG_TITLE = 'BOX'                                                 1137
         AG_EXNAME = 'SHAPE'                                              1137
         AG_PARLIST = 'DX_DY_DZ'                                          1137
         CALL AXSHAPE                                                     1137
C    *                                                                    1137
7051  CALL AGSPOPB                                                        1138
      GO TO BLOCK_GLUB, (7050 ,7060)                                      1138
C End of Local Procedure                                                  1138
C ----------------------------------------------------------------------  1139
C ------------ CFBK                                                       1139
C Begin Local Procedure CFBK                                              1140
C CFBK CARBON FIBER BACKING ON LADDER                                     1140
7070  CONTINUE                                                            1140
         AG_TITLE='CFBK'                                                  1140
         AG_MODULE=MODULE                                                 1140
         AG_MARK='V00'                                                    1140
         CALL AGSPUSH                                                     1140
         AG_MODULE=MODULE                                                 1140
C    *                                                                    1140
C    Geant ATTRIBUTE : CFBK ===> SEEN=1 COLO=1 <===                       1141
         CALL AGSRESET                                                    1141
         AG_SEEN = 1                                                      1141
         AG_COLO = 1                                                      1141
         AG_TITLE = 'CFBK'                                                1141
         AG_EXNAME = 'ATTRIBUTE'                                          1141
         AG_PARLIST = 'SEEN_COLO'                                         1141
         CALL AXATTRIBUTE                                                 1141
C    *                                                                    1141
C    Geant MATERIAL : CBMIX ===> inherited arguments used <===            1141
         CALL AGSRESET                                                    1142
         AG_TITLE = 'CBMIX'                                               1142
         AG_EXNAME = 'MATERIAL'                                           1142
         AG_PARLIST = 'NONE'                                              1142
         CALL AXMATERIAL                                                  1142
C    *                                                                    1142
C    Geant SHAPE : BOX ===> DX=1.245 DY=BACKINGTHICKNESS/2.0, DZ=BACKING  1142
C    LENGTH <===                                                          1142
         CALL AGSRESET                                                    1144
         AG_DX = 1.245                                                    1144
         AG_DY = BACKINGTHICKNESS/2.0                                     1144
         AG_DZ = BACKINGLENGTH                                            1144
         AG_TITLE = 'BOX'                                                 1144
         AG_EXNAME = 'SHAPE'                                              1144
         AG_PARLIST = 'DX_DY_DZ'                                          1144
         CALL AXSHAPE                                                     1144
C    *                                                                    1144
7071  CALL AGSPOPB                                                        1145
      GO TO BLOCK_CFBK, (7070 ,7080)                                      1145
C End of Local Procedure                                                  1145
C ----------------------------------------------------------------------  1146
C ------------ GLUC                                                       1146
C Begin Local Procedure GLUC                                              1147
C GLUC                                                                    1147
7090  CONTINUE                                                            1147
         AG_TITLE='GLUC'                                                  1147
         AG_MODULE=MODULE                                                 1147
         AG_MARK='V00'                                                    1147
         CALL AGSPUSH                                                     1147
         AG_MODULE=MODULE                                                 1147
C    *                                                                    1147
C    Geant ATTRIBUTE : GLUC ===> SEEN=1 COLO=629 <===                     1148
         CALL AGSRESET                                                    1148
         AG_SEEN = 1                                                      1148
         AG_COLO = 629                                                    1148
         AG_TITLE = 'GLUC'                                                1148
         AG_EXNAME = 'ATTRIBUTE'                                          1148
         AG_PARLIST = 'SEEN_COLO'                                         1148
         CALL AXATTRIBUTE                                                 1148
C    *                                                                    1148
C    Geant MATERIAL : MATADHESIVE ===> inherited arguments used <===      1148
         CALL AGSRESET                                                    1149
         AG_TITLE = 'MATADHESIVE'                                         1149
         AG_EXNAME = 'MATERIAL'                                           1149
         AG_PARLIST = 'NONE'                                              1149
         CALL AXMATERIAL                                                  1149
C    *                                                                    1149
C    Geant SHAPE : BOX ===> DX=0.7205 DY=GLUCTHICKNESS/2.0, DZ=14.95 <==  1149
C    =                                                                    1149
         CALL AGSRESET                                                    1151
         AG_DX = 0.7205                                                   1151
         AG_DY = GLUCTHICKNESS/2.0                                        1151
         AG_DZ = 14.95                                                    1151
         AG_TITLE = 'BOX'                                                 1151
         AG_EXNAME = 'SHAPE'                                              1151
         AG_PARLIST = 'DX_DY_DZ'                                          1151
         CALL AXSHAPE                                                     1151
C    *                                                                    1151
7091  CALL AGSPOPB                                                        1152
      GO TO BLOCK_GLUC, (7090 ,7100)                                      1152
C End of Local Procedure                                                  1152
C ----------------------------------------------------------------------  1153
C ------------ DRIV                                                       1153
C Begin Local Procedure DRIV                                              1154
C DRIV                                                                    1154
7110  CONTINUE                                                            1154
         AG_TITLE='DRIV'                                                  1154
         AG_MODULE=MODULE                                                 1154
         AG_MARK='V00'                                                    1154
         CALL AGSPUSH                                                     1154
         AG_MODULE=MODULE                                                 1154
C    *                                                                    1154
C    Geant ATTRIBUTE : DRIV ===> SEEN=1 COLO=629 <===                     1155
         CALL AGSRESET                                                    1155
         AG_SEEN = 1                                                      1155
         AG_COLO = 629                                                    1155
         AG_TITLE = 'DRIV'                                                1155
         AG_EXNAME = 'ATTRIBUTE'                                          1155
         AG_PARLIST = 'SEEN_COLO'                                         1155
         CALL AXATTRIBUTE                                                 1155
C    *                                                                    1155
C    Geant MATERIAL : MEDFR4 ===> inherited arguments used <===           1155
         CALL AGSRESET                                                    1156
         AG_TITLE = 'MEDFR4'                                              1156
         AG_EXNAME = 'MATERIAL'                                           1156
         AG_PARLIST = 'NONE'                                              1156
         CALL AXMATERIAL                                                  1156
C    *                                                                    1156
C    Geant SHAPE : BOX ===> DX=1.23025 DY=0.025 DZ=4.55 <===              1156
         CALL AGSRESET                                                    1157
         AG_DX = 1.23025                                                  1157
         AG_DY = 0.025                                                    1157
         AG_DZ = 4.55                                                     1157
         AG_TITLE = 'BOX'                                                 1157
         AG_EXNAME = 'SHAPE'                                              1157
         AG_PARLIST = 'DX_DY_DZ'                                          1157
         CALL AXSHAPE                                                     1157
C    *                                                                    1157
7111  CALL AGSPOPB                                                        1158
      GO TO BLOCK_DRIV, (7110 ,7120)                                      1158
C End of Local Procedure                                                  1158
C ----------------------------------------------------------------------  1159
C ------------ PXSI                                                       1159
C Begin Local Procedure PXSI                                              1160
C PXSI MOTHER OF THE SENSOR ASSEMBLY                                      1160
6990  CONTINUE                                                            1160
         AG_TITLE='PXSI'                                                  1160
         AG_MODULE=MODULE                                                 1160
         AG_MARK='V00'                                                    1160
         CALL AGSPUSH                                                     1160
         AG_MODULE=MODULE                                                 1160
C    *                                                                    1160
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1161
         CALL AGSRESET                                                    1161
         AG_TITLE = 'SILICON'                                             1161
         AG_EXNAME = 'MATERIAL'                                           1161
         AG_PARLIST = 'NONE'                                              1161
         CALL AXMATERIAL                                                  1161
C    *                                                                    1161
C    Geant ATTRIBUTE : PXSI ===> SEEN=1 COLO=594 <===                     1161
         CALL AGSRESET                                                    1162
         AG_SEEN = 1                                                      1162
         AG_COLO = 594                                                    1162
         AG_TITLE = 'PXSI'                                                1162
         AG_EXNAME = 'ATTRIBUTE'                                          1162
         AG_PARLIST = 'SEEN_COLO'                                         1162
         CALL AXATTRIBUTE                                                 1162
C    *                                                                    1162
C    Geant SHAPE : BOX ===> DX=1.1365 DY=0.0025 DZ=1.0120 <===            1162
         CALL AGSRESET                                                    1163
         AG_DX = 1.1365                                                   1163
         AG_DY = 0.0025                                                   1163
         AG_DZ = 1.0120                                                   1163
         AG_TITLE = 'BOX'                                                 1163
         AG_EXNAME = 'SHAPE'                                              1163
         AG_PARLIST = 'DX_DY_DZ'                                          1163
         CALL AXSHAPE                                                     1163
C    *                                                                    1163
C    Create PLAC :                                                        1164
C    Call To Local Procedure PLAC                                         1164
         ASSIGN 7140 TO BLOCK_PLAC                                        1164
         GO TO  7130                                                      1164
7140     CONTINUE                                                         1164
C    *                                                                    1164
C    Geant POSITION : PLAC ===> MOTHER='PXSI' X=0.1533 Y=0.0013 Z=0.0160  1165
C    4 <===                                                               1165
         CALL AGSRESET                                                    1165
         AG_MOTHER = 'PXSI'                                               1165
         AG_X = 0.1533                                                    1165
         AG_Y = 0.0013                                                    1165
         AG_Z = 0.01604                                                   1165
         AG_TITLE = 'PLAC'                                                1165
         AG_EXNAME = 'POSITION'                                           1165
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      1165
         CALL AXPOSITION                                                  1165
C    *                                                                    1165
6991  CALL AGSPOPB                                                        1166
      GO TO BLOCK_PXSI, (6990 ,7000)                                      1166
C End of Local Procedure                                                  1166
C ----------------------------------------------------------------------  1167
C ------------ PLAC                                                       1167
C Begin Local Procedure PLAC                                              1168
C PLAC ACTIVE SILICON TOP SECTOR                                          1168
7130  CONTINUE                                                            1168
         AG_TITLE='PLAC'                                                  1168
         AG_MODULE=MODULE                                                 1168
         AG_MARK='V00'                                                    1168
         CALL AGSPUSH                                                     1168
         AG_MODULE=MODULE                                                 1168
C    *                                                                    1168
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=4 <===                       1169
         CALL AGSRESET                                                    1169
         AG_SEEN = 1                                                      1169
         AG_COLO = 4                                                      1169
         AG_TITLE = 'PLAC'                                                1169
         AG_EXNAME = 'ATTRIBUTE'                                          1169
         AG_PARLIST = 'SEEN_COLO'                                         1169
         CALL AXATTRIBUTE                                                 1169
C    *                                                                    1169
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1169
         CALL AGSRESET                                                    1170
         AG_TITLE = 'SILICON'                                             1170
         AG_EXNAME = 'MATERIAL'                                           1170
         AG_PARLIST = 'NONE'                                              1170
         CALL AXMATERIAL                                                  1170
C    *                                                                    1170
C    Geant MATERIAL : SSENSITIVE ===> ISVOL=1 <===                        1170
         CALL AGSRESET                                                    1171
         AG_ISVOL = 1                                                     1171
         AG_TITLE = 'SSENSITIVE'                                          1171
         AG_EXNAME = 'MATERIAL'                                           1171
         AG_PARLIST = 'ISVOL'                                             1171
         CALL AXMATERIAL                                                  1171
C    *                                                                    1171
C    Geant MEDIUM : MEDSISENSITIVE ===> EPSIL=0.001 <===                  1171
         CALL AGSRESET                                                    1172
         AG_EPSIL = 0.001                                                 1172
         AG_TITLE = 'MEDSISENSITIVE'                                      1172
         AG_EXNAME = 'MEDIUM'                                             1172
         AG_PARLIST = 'EPSIL'                                             1172
         CALL AXMEDIUM                                                    1172
C    *                                                                    1172
C    Geant SHAPE : BOX ===> DX=0.9605 DY=0.0008 DZ=0.9936 <===            1172
         CALL AGSRESET                                                    1173
         AG_DX = 0.9605                                                   1173
         AG_DY = 0.0008                                                   1173
         AG_DZ = 0.9936                                                   1173
         AG_TITLE = 'BOX'                                                 1173
         AG_EXNAME = 'SHAPE'                                              1173
         AG_PARLIST = 'DX_DY_DZ'                                          1173
         CALL AXSHAPE                                                     1173
C    *                                                                    1173
C    Defining Hits: Z:.00001:S Y:.00001:S X:.00001:S PTOT:16:(0,100) CX:  1173
C    10: CY:10: CZ:10:, SLENG:16:(0,500) TOF:16: STEP:.0001: ELOSS:16:    1173
         AG_TITLE = 'PLAC'                                                1175
         AG_EXNAME = 'HITS'                                               1175
         AG_TYPE=AG_MAGIC                                                 1175
         CALL AGSDETV('PLAC')                                             1175
C    *                                                                    1175
         AG_PARLIST='Z'                                                   1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = AG_MAGIC                                              1175
         AG_BIN=.00001                                                    1175
         AG_OPTION='S'                                                    1175
         AG_FHMIN = AG_MAGIC                                              1175
         AG_FHMAX = AG_MAGIC                                              1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='Y'                                                   1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = AG_MAGIC                                              1175
         AG_BIN=.00001                                                    1175
         AG_OPTION='S'                                                    1175
         AG_FHMIN = AG_MAGIC                                              1175
         AG_FHMAX = AG_MAGIC                                              1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='X'                                                   1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = AG_MAGIC                                              1175
         AG_BIN=.00001                                                    1175
         AG_OPTION='S'                                                    1175
         AG_FHMIN = AG_MAGIC                                              1175
         AG_FHMAX = AG_MAGIC                                              1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='PTOT'                                                1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 16                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_OPTION='*'                                                    1175
         AG_FHMIN = 0                                                     1175
         AG_FHMAX = 100                                                   1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='CX'                                                  1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 10                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_FHMIN=AG_MAGIC                                                1175
         AG_FHMAX=AG_MAGIC                                                1175
         AG_OPTION='*'                                                    1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='CY'                                                  1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 10                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_FHMIN=AG_MAGIC                                                1175
         AG_FHMAX=AG_MAGIC                                                1175
         AG_OPTION='*'                                                    1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='CZ'                                                  1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 10                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_OPTION='*'                                                    1175
         AG_FHMIN = AG_MAGIC                                              1175
         AG_FHMAX = AG_MAGIC                                              1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='SLENG'                                               1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 16                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_OPTION='*'                                                    1175
         AG_FHMIN = 0                                                     1175
         AG_FHMAX = 500                                                   1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='TOF'                                                 1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 16                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_FHMIN=AG_MAGIC                                                1175
         AG_FHMAX=AG_MAGIC                                                1175
         AG_OPTION='*'                                                    1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='STEP'                                                1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = AG_MAGIC                                              1175
         AG_BIN=.0001                                                     1175
         AG_FHMIN=AG_MAGIC                                                1175
         AG_FHMAX=AG_MAGIC                                                1175
         AG_OPTION='*'                                                    1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         AG_PARLIST='ELOSS'                                               1175
         AG_TITLE = 'DETH'                                                1175
         AG_NBITS = 16                                                    1175
         AG_BIN=AG_MAGIC                                                  1175
         AG_FHMIN=AG_MAGIC                                                1175
         AG_FHMAX=AG_MAGIC                                                1175
         AG_OPTION='*'                                                    1175
         CALL AGSDETH (0)                                                 1175
C    *                                                                    1175
         CALL AGSDETU('PLAC')                                             1175
C    *                                                                    1175
7131  CALL AGSPOPB                                                        1176
      GO TO BLOCK_PLAC, (7130 ,7140)                                      1176
C End of Local Procedure                                                  1176
      WRITE(LOUT,7150)'===>>  [End AgML Module PixlGeo6]  <<==='          1176
7150  FORMAT(A32,/,/)                                                     1176
C MODULE PIXLGEO6                                                         1178
      END                                                                 1178
