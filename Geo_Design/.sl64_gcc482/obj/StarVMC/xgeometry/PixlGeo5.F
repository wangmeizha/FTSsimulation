C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIXLGEO5                                                 1
C PIXLGEO5 PIXEL DETECTOR GEOMETRY                                        1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIXLGEO5'/, BANK_TITLE/'*'/                            1
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
C AUTHOR JB,FV                                                            1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JB,FV'/                                       2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 2/1 2013                                                        2
      CHARACTER*40 CREATED /'2/1 2013'/                                   3
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
      INTEGER BLOCK_PXLA                                                  17
      INTEGER BLOCK_PXSE                                                  17
      INTEGER BLOCK_PXCA                                                  17
      INTEGER BLOCK_PXCC                                                  17
      INTEGER BLOCK_PXCD                                                  17
      INTEGER BLOCK_PXCE                                                  17
      INTEGER BLOCK_PXCF                                                  17
      INTEGER BLOCK_PXCG                                                  17
      INTEGER BLOCK_PXCH                                                  17
      INTEGER BLOCK_PXCB                                                  17
      INTEGER BLOCK_PXRB                                                  17
      INTEGER BLOCK_PXTR                                                  17
      INTEGER BLOCK_PXTM                                                  17
      INTEGER BLOCK_PXTL                                                  17
      INTEGER BLOCK_PXTJ                                                  17
      INTEGER BLOCK_PXTI                                                  17
      INTEGER BLOCK_PXLB                                                  17
      INTEGER BLOCK_PXIB                                                  17
      INTEGER BLOCK_LADR                                                  17
      INTEGER BLOCK_GLUA                                                  17
      INTEGER BLOCK_GLUB                                                  17
      INTEGER BLOCK_GLUC                                                  17
      INTEGER BLOCK_ALCA                                                  17
      INTEGER BLOCK_CFBK                                                  17
      INTEGER BLOCK_PXSI                                                  17
      INTEGER BLOCK_PLAC                                                  17
      INTEGER BLOCK_DRIV                                                  17
      INTEGER BLOCK_DTCA                                                  17
      INTEGER BLOCK_DTCB                                                  17
      INTEGER BLOCK_DTCC                                                  17
      INTEGER BLOCK_DTCD                                                  17
      INTEGER BLOCK_DTCE                                                  17
      INTEGER BLOCK_DTCF                                                  17
      INTEGER BLOCK_DTCG                                                  17
      INTEGER BLOCK_DTCH                                                  17
      INTEGER BLOCK_DTSA                                                  17
      INTEGER BLOCK_DTSB                                                  17
      INTEGER BLOCK_DTSC                                                  17
      INTEGER BLOCK_DTSD                                                  17
      INTEGER BLOCK_DTSE                                                  17
      INTEGER BLOCK_DTSF                                                  17
      INTEGER BLOCK_DTSG                                                  17
      INTEGER BLOCK_DTSH                                                  17
      INTEGER BLOCK_DTEA                                                  17
      INTEGER BLOCK_DTEB                                                  17
      INTEGER BLOCK_DTEC                                                  17
      INTEGER BLOCK_DTED                                                  17
      INTEGER BLOCK_DTEE                                                  17
      INTEGER BLOCK_DTEF                                                  17
      INTEGER BLOCK_DTEG                                                  17
      INTEGER BLOCK_DTEH                                                  17
      INTEGER BLOCK_DESA                                                  17
      INTEGER BLOCK_DESB                                                  17
      INTEGER BLOCK_DESC                                                  17
      INTEGER BLOCK_DESD                                                  17
      INTEGER BLOCK_DESE                                                  17
      INTEGER BLOCK_DESF                                                  17
      INTEGER BLOCK_DESG                                                  17
      INTEGER BLOCK_DESH                                                  17
      INTEGER BLOCK_PTRA                                                  17
      INTEGER BLOCK_PTRB                                                  17
      INTEGER BLOCK_PTRC                                                  17
      INTEGER BLOCK_PTRE                                                  17
      INTEGER BLOCK_PTRF                                                  17
      INTEGER BLOCK_PTRG                                                  17
      INTEGER BLOCK_PTMA                                                  17
      INTEGER BLOCK_PTMB                                                  17
      INTEGER BLOCK_PTMC                                                  17
      INTEGER BLOCK_PTME                                                  17
      INTEGER BLOCK_PTMF                                                  17
      INTEGER BLOCK_PTLA                                                  17
      INTEGER BLOCK_PTLB                                                  17
      INTEGER BLOCK_PTLC                                                  17
      INTEGER BLOCK_PTLD                                                  17
      INTEGER BLOCK_PTLE                                                  17
      INTEGER BLOCK_PTLF                                                  17
      INTEGER BLOCK_PTLG                                                  17
      INTEGER BLOCK_PTLH                                                  17
      INTEGER BLOCK_PTLI                                                  17
      INTEGER BLOCK_PTLJ                                                  17
      REAL *4 SECTORLENGTH/17.55/                                         18
      REAL *4 SECTORTHICKNESS/0.024/                                      20
      REAL *4 SECTORTHICKNESSNEG/-0.024/                                  22
      REAL *4 SECTORHALFTHICKNESS/0.012/                                  24
      REAL *4 SECTORZOFFSET/-3.8105/                                      26
      REAL *4 BACKINGLENGTH/15.325/                                       28
      REAL *4 BACKINGWIDTH/1.245/                                         30
      REAL *4 BACKINGTHICKNESS/0.0175/                                    32
      REAL *4 BACKINGZOFFSET/0.00/                                        34
      REAL *4 DOVETAILEXTRUSIONLENGTH/0.05/                               36
      REAL *4 DOVETAILEXTRUSIONTHICKNESS/0.124/                           38
      REAL *4 DOVETAILEXTRUSIONTHICKNESSBOX/0.262/                        40
      REAL *4 DOVETAILEXTRUSIONTHICKNESSNEG/-0.124/                       42
      REAL *4 DOVETAILEXTRUSIONHALFTHICKNESS/0.062/                       44
      REAL *4 DOVETAILEXTRUSIONZOFFSET/-21.4105/                          46
      REAL *4 DOVETAILSHAPEOFFSET/-22.3605/                               48
      REAL *4 DOVETAILLENGTH/0.5/                                         50
      REAL *4 DOVETAILTHICKNESS/0.1/                                      52
      REAL *4 DOVETAILHALFTHICKNESS/0.05/                                 54
      REAL *4 DOVETAILZOFFSET/-20.8605/                                   56
      REAL *4 CABLELENGTH/15.325/                                         58
      REAL *4 CABLEZOFFSET/0/                                             60
      INTEGER *4 NSECTOR /10/                                             62
      REAL *4 MINRADSEC/2.12/                                             64
      REAL *4 MAXRADSEC/10/                                               66
      REAL *4 MAXZSEC/23.26/                                              68
      REAL *4 MINRVOL/2.1/                                                70
      REAL *4 MAXRVOL/11/                                                 72
      REAL *4 MAXZVOL/23.26/                                              74
      REAL *4 HALFMAXZ/23.26/                                             76
      REAL *4 MINPHISEC/72.0/                                             78
      REAL *4 MAXPHISEC/132.0/                                            80
      REAL *4 MINPHISECBARE/90.0/                                         82
      REAL *4 MAXPHISECBARE/126.0/                                        84
      REAL *4 XP, YP, XPOS,YPOS,ZPOS,ANGLE,ANGLEPOS, ZSENSOR, ALPHAZ,     86
     *ALPHAX, ALPHAY                                                      86
      INTEGER *4 SECTOR                                                   88
      REAL *4 LADBOXX, LADBOXY, LADBOXZ                                   90
      INTEGER *4 DEBUG, SENS, JJ, II                                      92
      REAL *4 GLUATHICKNESS /0.0050/                                      94
      REAL *4 GLUBTHICKNESS /0.0050/                                      96
      REAL *4 GLUCTHICKNESS /0.0100/                                      98
      REAL *4 LADDERHALFTHICKNESS/0.05225/                                100
C -                                                                       102
      REAL PDEF_VERSION                                                   102
      EQUIVALENCE (BANK_PDEF(3),PDEF_VERSION,RE_PDEF_VERSION),            102
     *(NAMES_PDEF(1),NPDEF_VERSION)                                       102
      EQUIVALENCE (COMMEN_PDEF(1),CPDEF_VERSION),(MAP_PDEF(1,1),          102
     *MPDEF_VERSION)                                                      102
      CHARACTER CPDEF_VERSION*40/' '/                                     102
      CHARACTER NPDEF_VERSION*12/'R.VERSION'/                             102
      INTEGER MPDEF_VERSION(2)/1,1/                                       102
      REAL RE_PDEF_VERSION                                                102
C -                                                                       102
      INTEGER PDEF_SECTOR                                                 102
      EQUIVALENCE (BANK_PDEF(4),PDEF_SECTOR,RE_PDEF_SECTOR),              102
     *(NAMES_PDEF(2),NPDEF_SECTOR)                                        102
      EQUIVALENCE (COMMEN_PDEF(2),CPDEF_SECTOR),(MAP_PDEF(1,2),           102
     *MPDEF_SECTOR)                                                       102
      CHARACTER CPDEF_SECTOR*40/' '/                                      102
      CHARACTER NPDEF_SECTOR*12/'I.SECTOR'/                               102
      INTEGER MPDEF_SECTOR(2)/1,1/                                        102
      REAL RE_PDEF_SECTOR                                                 102
C -                                                                       102
      REAL PDEF_PHINUM(10)                                                102
      EQUIVALENCE (BANK_PDEF(5),PDEF_PHINUM,RE_PDEF_PHINUM),              102
     *(NAMES_PDEF(3),NPDEF_PHINUM)                                        102
      EQUIVALENCE (COMMEN_PDEF(3),CPDEF_PHINUM),(MAP_PDEF(1,3),           102
     *MPDEF_PHINUM)                                                       102
      CHARACTER CPDEF_PHINUM*40/' '/                                      102
      CHARACTER NPDEF_PHINUM*12/'R.PHINUM'/                               102
      INTEGER MPDEF_PHINUM(2)/10,1/                                       102
      REAL RE_PDEF_PHINUM                                                 102
C -                                                                       102
      INTEGER PDEF_ISON(10)                                               102
      EQUIVALENCE (BANK_PDEF(15),PDEF_ISON,RE_PDEF_ISON),(NAMES_PDEF(4),  102
     *NPDEF_ISON)                                                         102
      EQUIVALENCE (COMMEN_PDEF(4),CPDEF_ISON),(MAP_PDEF(1,4),             102
     *MPDEF_ISON)                                                         102
      CHARACTER CPDEF_ISON*40/' '/                                        102
      CHARACTER NPDEF_ISON*12/'I.ISON'/                                   102
      INTEGER MPDEF_ISON(2)/10,1/                                         102
      REAL RE_PDEF_ISON                                                   102
C -                                                                       102
      INTEGER LEN_PDEF,LLEN_PDEF,LINK_PDEF                                102
      PARAMETER (LEN_PDEF=24)                                             102
      PARAMETER (LLEN_PDEF=4)                                             102
      INTEGER FLAG_PDEF/0/,MAP_PDEF(2,LLEN_PDEF),IDNUM_PDEF(10)           102
      CHARACTER NAMES_PDEF(LLEN_PDEF)*12,COMMEN_PDEF(LLEN_PDEF)*40        102
      DATA IDNUM_PDEF/10*0/                                               102
      REAL BANK_PDEF(LEN_PDEF)                                            102
      CHARACTER*60 BPATH_PDEF/' '/                                        102
      SAVE FLAG_PDEF,MAP_PDEF,IDNUM_PDEF,NAMES_PDEF,COMMEN_PDEF,          102
     *BPATH_PDEF                                                          102
      DATA LINK_PDEF/0/,BANK_PDEF/LEN_PDEF*0/                             102
C *                                                                       102
C -                                                                       103
      REAL SENP_VERSION                                                   103
      EQUIVALENCE (BANK_SENP(3),SENP_VERSION,RE_SENP_VERSION),            103
     *(NAMES_SENP(1),NSENP_VERSION)                                       103
      EQUIVALENCE (COMMEN_SENP(1),CSENP_VERSION),(MAP_SENP(1,1),          103
     *MSENP_VERSION)                                                      103
      CHARACTER CSENP_VERSION*40/' '/                                     103
      CHARACTER NSENP_VERSION*12/'R.VERSION'/                             103
      INTEGER MSENP_VERSION(2)/1,1/                                       103
      REAL RE_SENP_VERSION                                                103
C -                                                                       103
      INTEGER SENP_SENSOR                                                 103
      EQUIVALENCE (BANK_SENP(4),SENP_SENSOR,RE_SENP_SENSOR),              103
     *(NAMES_SENP(2),NSENP_SENSOR)                                        103
      EQUIVALENCE (COMMEN_SENP(2),CSENP_SENSOR),(MAP_SENP(1,2),           103
     *MSENP_SENSOR)                                                       103
      CHARACTER CSENP_SENSOR*40/' '/                                      103
      CHARACTER NSENP_SENSOR*12/'I.SENSOR'/                               103
      INTEGER MSENP_SENSOR(2)/1,1/                                        103
      REAL RE_SENP_SENSOR                                                 103
C -                                                                       103
      REAL SENP_ZPOS(10)                                                  103
      EQUIVALENCE (BANK_SENP(5),SENP_ZPOS,RE_SENP_ZPOS),(NAMES_SENP(3),   103
     *NSENP_ZPOS)                                                         103
      EQUIVALENCE (COMMEN_SENP(3),CSENP_ZPOS),(MAP_SENP(1,3),             103
     *MSENP_ZPOS)                                                         103
      CHARACTER CSENP_ZPOS*40/' '/                                        103
      CHARACTER NSENP_ZPOS*12/'R.ZPOS'/                                   103
      INTEGER MSENP_ZPOS(2)/10,1/                                         103
      REAL RE_SENP_ZPOS                                                   103
C -                                                                       103
      INTEGER LEN_SENP,LLEN_SENP,LINK_SENP                                103
      PARAMETER (LEN_SENP=14)                                             103
      PARAMETER (LLEN_SENP=3)                                             103
      INTEGER FLAG_SENP/0/,MAP_SENP(2,LLEN_SENP),IDNUM_SENP(10)           103
      CHARACTER NAMES_SENP(LLEN_SENP)*12,COMMEN_SENP(LLEN_SENP)*40        103
      DATA IDNUM_SENP/10*0/                                               103
      REAL BANK_SENP(LEN_SENP)                                            103
      CHARACTER*60 BPATH_SENP/' '/                                        103
      SAVE FLAG_SENP,MAP_SENP,IDNUM_SENP,NAMES_SENP,COMMEN_SENP,          103
     *BPATH_SENP                                                          103
      DATA LINK_SENP/0/,BANK_SENP/LEN_SENP*0/                             103
C *                                                                       103
C -                                                                       104
      INTEGER PXLW_VERSION                                                104
      EQUIVALENCE (BANK_PXLW(3),PXLW_VERSION,RE_PXLW_VERSION),            104
     *(NAMES_PXLW(1),NPXLW_VERSION)                                       104
      EQUIVALENCE (COMMEN_PXLW(1),CPXLW_VERSION),(MAP_PXLW(1,1),          104
     *MPXLW_VERSION)                                                      104
      CHARACTER CPXLW_VERSION*40/' '/                                     104
      CHARACTER NPXLW_VERSION*12/'I.VERSION'/                             104
      INTEGER MPXLW_VERSION(2)/1,1/                                       104
      REAL RE_PXLW_VERSION                                                104
C -                                                                       104
      REAL PXLW_LADRCONFIG                                                104
      EQUIVALENCE (BANK_PXLW(4),PXLW_LADRCONFIG,RE_PXLW_LADRCONFIG),      104
     *(NAMES_PXLW(2),NPXLW_LADRCONFIG)                                    104
      EQUIVALENCE (COMMEN_PXLW(2),CPXLW_LADRCONFIG),(MAP_PXLW(1,2),       104
     *MPXLW_LADRCONFIG)                                                   104
      CHARACTER CPXLW_LADRCONFIG*40/' '/                                  104
      CHARACTER NPXLW_LADRCONFIG*12/'R.LADRCONFIG'/                       104
      INTEGER MPXLW_LADRCONFIG(2)/1,1/                                    104
      REAL RE_PXLW_LADRCONFIG                                             104
C -                                                                       104
      REAL PXLW_SECVERSION                                                104
      EQUIVALENCE (BANK_PXLW(5),PXLW_SECVERSION,RE_PXLW_SECVERSION),      104
     *(NAMES_PXLW(3),NPXLW_SECVERSION)                                    104
      EQUIVALENCE (COMMEN_PXLW(3),CPXLW_SECVERSION),(MAP_PXLW(1,3),       104
     *MPXLW_SECVERSION)                                                   104
      CHARACTER CPXLW_SECVERSION*40/' '/                                  104
      CHARACTER NPXLW_SECVERSION*12/'R.SECVERSION'/                       104
      INTEGER MPXLW_SECVERSION(2)/1,1/                                    104
      REAL RE_PXLW_SECVERSION                                             104
C -                                                                       104
      INTEGER LEN_PXLW,LLEN_PXLW,LINK_PXLW                                104
      PARAMETER (LEN_PXLW=5)                                              104
      PARAMETER (LLEN_PXLW=3)                                             104
      INTEGER FLAG_PXLW/0/,MAP_PXLW(2,LLEN_PXLW),IDNUM_PXLW(10)           104
      CHARACTER NAMES_PXLW(LLEN_PXLW)*12,COMMEN_PXLW(LLEN_PXLW)*40        104
      DATA IDNUM_PXLW/10*0/                                               104
      REAL BANK_PXLW(LEN_PXLW)                                            104
      CHARACTER*60 BPATH_PXLW/' '/                                        104
      SAVE FLAG_PXLW,MAP_PXLW,IDNUM_PXLW,NAMES_PXLW,COMMEN_PXLW,          104
     *BPATH_PXLW                                                          104
      DATA LINK_PXLW/0/,BANK_PXLW/LEN_PXLW*0/                             104
C *                                                                       104
C -                                                                       105
      REAL PIXL_VERSION                                                   105
      EQUIVALENCE (BANK_PIXL(3),PIXL_VERSION,RE_PIXL_VERSION),            105
     *(NAMES_PIXL(1),NPIXL_VERSION)                                       105
      EQUIVALENCE (COMMEN_PIXL(1),CPIXL_VERSION),(MAP_PIXL(1,1),          105
     *MPIXL_VERSION)                                                      105
      CHARACTER CPIXL_VERSION*40/' '/                                     105
      CHARACTER NPIXL_VERSION*12/'R.VERSION'/                             105
      INTEGER MPIXL_VERSION(2)/1,1/                                       105
      REAL RE_PIXL_VERSION                                                105
C -                                                                       105
      INTEGER PIXL_SECTOR                                                 105
      EQUIVALENCE (BANK_PIXL(4),PIXL_SECTOR,RE_PIXL_SECTOR),              105
     *(NAMES_PIXL(2),NPIXL_SECTOR)                                        105
      EQUIVALENCE (COMMEN_PIXL(2),CPIXL_SECTOR),(MAP_PIXL(1,2),           105
     *MPIXL_SECTOR)                                                       105
      CHARACTER CPIXL_SECTOR*40/' '/                                      105
      CHARACTER NPIXL_SECTOR*12/'I.SECTOR'/                               105
      INTEGER MPIXL_SECTOR(2)/1,1/                                        105
      REAL RE_PIXL_SECTOR                                                 105
C -                                                                       105
      REAL PIXL_PHINUM(10)                                                105
      EQUIVALENCE (BANK_PIXL(5),PIXL_PHINUM,RE_PIXL_PHINUM),              105
     *(NAMES_PIXL(3),NPIXL_PHINUM)                                        105
      EQUIVALENCE (COMMEN_PIXL(3),CPIXL_PHINUM),(MAP_PIXL(1,3),           105
     *MPIXL_PHINUM)                                                       105
      CHARACTER CPIXL_PHINUM*40/' '/                                      105
      CHARACTER NPIXL_PHINUM*12/'R.PHINUM'/                               105
      INTEGER MPIXL_PHINUM(2)/10,1/                                       105
      REAL RE_PIXL_PHINUM                                                 105
C -                                                                       105
      INTEGER PIXL_ISON(10)                                               105
      EQUIVALENCE (BANK_PIXL(15),PIXL_ISON,RE_PIXL_ISON),(NAMES_PIXL(4),  105
     *NPIXL_ISON)                                                         105
      EQUIVALENCE (COMMEN_PIXL(4),CPIXL_ISON),(MAP_PIXL(1,4),             105
     *MPIXL_ISON)                                                         105
      CHARACTER CPIXL_ISON*40/' '/                                        105
      CHARACTER NPIXL_ISON*12/'I.ISON'/                                   105
      INTEGER MPIXL_ISON(2)/10,1/                                         105
      REAL RE_PIXL_ISON                                                   105
C -                                                                       105
      INTEGER LEN_PIXL,LLEN_PIXL,LINK_PIXL                                105
      PARAMETER (LEN_PIXL=24)                                             105
      PARAMETER (LLEN_PIXL=4)                                             105
      INTEGER FLAG_PIXL/0/,MAP_PIXL(2,LLEN_PIXL),IDNUM_PIXL(10)           105
      CHARACTER NAMES_PIXL(LLEN_PIXL)*12,COMMEN_PIXL(LLEN_PIXL)*40        105
      DATA IDNUM_PIXL/10*0/                                               105
      REAL BANK_PIXL(LEN_PIXL)                                            105
      CHARACTER*60 BPATH_PIXL/' '/                                        105
      SAVE FLAG_PIXL,MAP_PIXL,IDNUM_PIXL,NAMES_PIXL,COMMEN_PIXL,          105
     *BPATH_PIXL                                                          105
      DATA LINK_PIXL/0/,BANK_PIXL/LEN_PIXL*0/                             105
C *                                                                       105
C -                                                                       106
      INTEGER PARO_VERSION                                                106
      EQUIVALENCE (BANK_PARO(3),PARO_VERSION,RE_PARO_VERSION),            106
     *(NAMES_PARO(1),NPARO_VERSION)                                       106
      EQUIVALENCE (COMMEN_PARO(1),CPARO_VERSION),(MAP_PARO(1,1),          106
     *MPARO_VERSION)                                                      106
      CHARACTER CPARO_VERSION*40/' '/                                     106
      CHARACTER NPARO_VERSION*12/'I.VERSION'/                             106
      INTEGER MPARO_VERSION(2)/1,1/                                       106
      REAL RE_PARO_VERSION                                                106
C -                                                                       106
      REAL PARO_DX                                                        106
      EQUIVALENCE (BANK_PARO(4),PARO_DX,RE_PARO_DX),(NAMES_PARO(2),       106
     *NPARO_DX)                                                           106
      EQUIVALENCE (COMMEN_PARO(2),CPARO_DX),(MAP_PARO(1,2),MPARO_DX)      106
      CHARACTER CPARO_DX*40/' '/                                          106
      CHARACTER NPARO_DX*12/'R.DX'/                                       106
      INTEGER MPARO_DX(2)/1,1/                                            106
      REAL RE_PARO_DX                                                     106
C -                                                                       106
      REAL PARO_DY                                                        106
      EQUIVALENCE (BANK_PARO(5),PARO_DY,RE_PARO_DY),(NAMES_PARO(3),       106
     *NPARO_DY)                                                           106
      EQUIVALENCE (COMMEN_PARO(3),CPARO_DY),(MAP_PARO(1,3),MPARO_DY)      106
      CHARACTER CPARO_DY*40/' '/                                          106
      CHARACTER NPARO_DY*12/'R.DY'/                                       106
      INTEGER MPARO_DY(2)/1,1/                                            106
      REAL RE_PARO_DY                                                     106
C -                                                                       106
      REAL PARO_DZ                                                        106
      EQUIVALENCE (BANK_PARO(6),PARO_DZ,RE_PARO_DZ),(NAMES_PARO(4),       106
     *NPARO_DZ)                                                           106
      EQUIVALENCE (COMMEN_PARO(4),CPARO_DZ),(MAP_PARO(1,4),MPARO_DZ)      106
      CHARACTER CPARO_DZ*40/' '/                                          106
      CHARACTER NPARO_DZ*12/'R.DZ'/                                       106
      INTEGER MPARO_DZ(2)/1,1/                                            106
      REAL RE_PARO_DZ                                                     106
C -                                                                       106
      REAL PARO_MULT                                                      106
      EQUIVALENCE (BANK_PARO(7),PARO_MULT,RE_PARO_MULT),(NAMES_PARO(5),   106
     *NPARO_MULT)                                                         106
      EQUIVALENCE (COMMEN_PARO(5),CPARO_MULT),(MAP_PARO(1,5),             106
     *MPARO_MULT)                                                         106
      CHARACTER CPARO_MULT*40/' '/                                        106
      CHARACTER NPARO_MULT*12/'R.MULT'/                                   106
      INTEGER MPARO_MULT(2)/1,1/                                          106
      REAL RE_PARO_MULT                                                   106
C -                                                                       106
      INTEGER PARO_SEC(10)                                                106
      EQUIVALENCE (BANK_PARO(8),PARO_SEC,RE_PARO_SEC),(NAMES_PARO(6),     106
     *NPARO_SEC)                                                          106
      EQUIVALENCE (COMMEN_PARO(6),CPARO_SEC),(MAP_PARO(1,6),MPARO_SEC)    106
      CHARACTER CPARO_SEC*40/' '/                                         106
      CHARACTER NPARO_SEC*12/'I.SEC'/                                     106
      INTEGER MPARO_SEC(2)/10,1/                                          106
      REAL RE_PARO_SEC                                                    106
C -                                                                       106
      INTEGER LEN_PARO,LLEN_PARO,LINK_PARO                                106
      PARAMETER (LEN_PARO=17)                                             106
      PARAMETER (LLEN_PARO=6)                                             106
      INTEGER FLAG_PARO/0/,MAP_PARO(2,LLEN_PARO),IDNUM_PARO(10)           106
      CHARACTER NAMES_PARO(LLEN_PARO)*12,COMMEN_PARO(LLEN_PARO)*40        106
      DATA IDNUM_PARO/10*0/                                               106
      REAL BANK_PARO(LEN_PARO)                                            106
      CHARACTER*60 BPATH_PARO/' '/                                        106
      SAVE FLAG_PARO,MAP_PARO,IDNUM_PARO,NAMES_PARO,COMMEN_PARO,          106
     *BPATH_PARO                                                          106
      DATA LINK_PARO/0/,BANK_PARO/LEN_PARO*0/                             106
C *                                                                       106
C -                                                                       107
      INTEGER PARA_VERSION                                                107
      EQUIVALENCE (BANK_PARA(3),PARA_VERSION,RE_PARA_VERSION),            107
     *(NAMES_PARA(1),NPARA_VERSION)                                       107
      EQUIVALENCE (COMMEN_PARA(1),CPARA_VERSION),(MAP_PARA(1,1),          107
     *MPARA_VERSION)                                                      107
      CHARACTER CPARA_VERSION*40/' '/                                     107
      CHARACTER NPARA_VERSION*12/'I.VERSION'/                             107
      INTEGER MPARA_VERSION(2)/1,1/                                       107
      REAL RE_PARA_VERSION                                                107
C -                                                                       107
      REAL PARA_DA                                                        107
      EQUIVALENCE (BANK_PARA(4),PARA_DA,RE_PARA_DA),(NAMES_PARA(2),       107
     *NPARA_DA)                                                           107
      EQUIVALENCE (COMMEN_PARA(2),CPARA_DA),(MAP_PARA(1,2),MPARA_DA)      107
      CHARACTER CPARA_DA*40/' '/                                          107
      CHARACTER NPARA_DA*12/'R.DA'/                                       107
      INTEGER MPARA_DA(2)/1,1/                                            107
      REAL RE_PARA_DA                                                     107
C -                                                                       107
      REAL PARA_DB                                                        107
      EQUIVALENCE (BANK_PARA(5),PARA_DB,RE_PARA_DB),(NAMES_PARA(3),       107
     *NPARA_DB)                                                           107
      EQUIVALENCE (COMMEN_PARA(3),CPARA_DB),(MAP_PARA(1,3),MPARA_DB)      107
      CHARACTER CPARA_DB*40/' '/                                          107
      CHARACTER NPARA_DB*12/'R.DB'/                                       107
      INTEGER MPARA_DB(2)/1,1/                                            107
      REAL RE_PARA_DB                                                     107
C -                                                                       107
      REAL PARA_DG                                                        107
      EQUIVALENCE (BANK_PARA(6),PARA_DG,RE_PARA_DG),(NAMES_PARA(4),       107
     *NPARA_DG)                                                           107
      EQUIVALENCE (COMMEN_PARA(4),CPARA_DG),(MAP_PARA(1,4),MPARA_DG)      107
      CHARACTER CPARA_DG*40/' '/                                          107
      CHARACTER NPARA_DG*12/'R.DG'/                                       107
      INTEGER MPARA_DG(2)/1,1/                                            107
      REAL RE_PARA_DG                                                     107
C -                                                                       107
      REAL PARA_MULT                                                      107
      EQUIVALENCE (BANK_PARA(7),PARA_MULT,RE_PARA_MULT),(NAMES_PARA(5),   107
     *NPARA_MULT)                                                         107
      EQUIVALENCE (COMMEN_PARA(5),CPARA_MULT),(MAP_PARA(1,5),             107
     *MPARA_MULT)                                                         107
      CHARACTER CPARA_MULT*40/' '/                                        107
      CHARACTER NPARA_MULT*12/'R.MULT'/                                   107
      INTEGER MPARA_MULT(2)/1,1/                                          107
      REAL RE_PARA_MULT                                                   107
C -                                                                       107
      INTEGER PARA_SEC(10)                                                107
      EQUIVALENCE (BANK_PARA(8),PARA_SEC,RE_PARA_SEC),(NAMES_PARA(6),     107
     *NPARA_SEC)                                                          107
      EQUIVALENCE (COMMEN_PARA(6),CPARA_SEC),(MAP_PARA(1,6),MPARA_SEC)    107
      CHARACTER CPARA_SEC*40/' '/                                         107
      CHARACTER NPARA_SEC*12/'I.SEC'/                                     107
      INTEGER MPARA_SEC(2)/10,1/                                          107
      REAL RE_PARA_SEC                                                    107
C -                                                                       107
      INTEGER LEN_PARA,LLEN_PARA,LINK_PARA                                107
      PARAMETER (LEN_PARA=17)                                             107
      PARAMETER (LLEN_PARA=6)                                             107
      INTEGER FLAG_PARA/0/,MAP_PARA(2,LLEN_PARA),IDNUM_PARA(10)           107
      CHARACTER NAMES_PARA(LLEN_PARA)*12,COMMEN_PARA(LLEN_PARA)*40        107
      DATA IDNUM_PARA/10*0/                                               107
      REAL BANK_PARA(LEN_PARA)                                            107
      CHARACTER*60 BPATH_PARA/' '/                                        107
      SAVE FLAG_PARA,MAP_PARA,IDNUM_PARA,NAMES_PARA,COMMEN_PARA,          107
     *BPATH_PARA                                                          107
      DATA LINK_PARA/0/,BANK_PARA/LEN_PARA*0/                             107
C *                                                                       107
C -                                                                       108
      INTEGER LADR_VERSION                                                108
      EQUIVALENCE (BANK_LADR(3),LADR_VERSION,RE_LADR_VERSION),            108
     *(NAMES_LADR(1),NLADR_VERSION)                                       108
      EQUIVALENCE (COMMEN_LADR(1),CLADR_VERSION),(MAP_LADR(1,1),          108
     *MLADR_VERSION)                                                      108
      CHARACTER CLADR_VERSION*40/' '/                                     108
      CHARACTER NLADR_VERSION*12/'I.VERSION'/                             108
      INTEGER MLADR_VERSION(2)/1,1/                                       108
      REAL RE_LADR_VERSION                                                108
C -                                                                       108
      INTEGER LADR_TYPE                                                   108
      EQUIVALENCE (BANK_LADR(4),LADR_TYPE,RE_LADR_TYPE),(NAMES_LADR(2),   108
     *NLADR_TYPE)                                                         108
      EQUIVALENCE (COMMEN_LADR(2),CLADR_TYPE),(MAP_LADR(1,2),             108
     *MLADR_TYPE)                                                         108
      CHARACTER CLADR_TYPE*40/' '/                                        108
      CHARACTER NLADR_TYPE*12/'I.TYPE'/                                   108
      INTEGER MLADR_TYPE(2)/1,1/                                          108
      REAL RE_LADR_TYPE                                                   108
C -                                                                       108
      REAL LADR_CABLETHICKNESS                                            108
      EQUIVALENCE (BANK_LADR(5),LADR_CABLETHICKNESS,                      108
     *RE_LADR_CABLETHICKNESS),(NAMES_LADR(3),NLADR_CABLETHICKNESS)        108
      EQUIVALENCE (COMMEN_LADR(3),CLADR_CABLETHICKNESS),(MAP_LADR(1,3),   108
     *MLADR_CABLETHICKNESS)                                               108
      CHARACTER CLADR_CABLETHICKNESS*40/' '/                              108
      CHARACTER NLADR_CABLETHICKNESS*12/'R.CABLETHICKNESS'/               108
      INTEGER MLADR_CABLETHICKNESS(2)/1,1/                                108
      REAL RE_LADR_CABLETHICKNESS                                         108
C -                                                                       108
      INTEGER LEN_LADR,LLEN_LADR,LINK_LADR                                108
      PARAMETER (LEN_LADR=5)                                              108
      PARAMETER (LLEN_LADR=3)                                             108
      INTEGER FLAG_LADR/0/,MAP_LADR(2,LLEN_LADR),IDNUM_LADR(10)           108
      CHARACTER NAMES_LADR(LLEN_LADR)*12,COMMEN_LADR(LLEN_LADR)*40        108
      DATA IDNUM_LADR/10*0/                                               108
      REAL BANK_LADR(LEN_LADR)                                            108
      CHARACTER*60 BPATH_LADR/' '/                                        108
      SAVE FLAG_LADR,MAP_LADR,IDNUM_LADR,NAMES_LADR,COMMEN_LADR,          108
     *BPATH_LADR                                                          108
      DATA LINK_LADR/0/,BANK_LADR/LEN_LADR*0/                             108
C *                                                                       108
C *                                                                       109
      CALL AGSBEGM(MODULE,IPRIN)                                          109
      IF (IPRIN.LT.0) RETURN                                              109
C Filling bank PXLW - PIXEL LADDER DATA                                   109
      BANK_TITLE='PIXEL LADDER DATA'                                      109
      BPATH_PXLW='PXLW*'                                                  109
      IDNUM_PXLW(1)=0                                                     110
C CONFIG VERSION                                                          110
      PXLW_VERSION=1                                                      110
      CPXLW_VERSION='CONFIG VERSION'                                      110
C LADDER VERSION                                                          111
      PXLW_LADRCONFIG=1.0                                                 111
      CPXLW_LADRCONFIG='LADDER VERSION'                                   111
C DIFFERENT CONFIGS OF PIXL                                               112
      PXLW_SECVERSION=0.0                                                 112
      CPXLW_SECVERSION='DIFFERENT CONFIGS OF PIXL'                        112
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLW',    113
     *BPATH_PXLW,IDNUM_PXLW,1, MAP_PXLW,NAMES_PXLW,COMMEN_PXLW,           113
     *BANK_PXLW,LEN_PXLW,LLEN_PXLW,LINK_PXLW,FLAG_PXLW)                   113
C *                                                                       113
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PXLW',    113
     *BPATH_PXLW,IDNUM_PXLW,1, MAP_PXLW,NAMES_PXLW,COMMEN_PXLW,           113
     *BANK_PXLW,LEN_PXLW,LLEN_PXLW,LINK_PXLW,FLAG_PXLW)                   113
C *                                                                       114
      CALL AGSBEGM(MODULE,IPRIN)                                          114
      IF (IPRIN.LT.0) RETURN                                              114
C Filling bank PDEF - PIXEL DEFAULT PARAMTERS                             114
      BANK_TITLE='PIXEL DEFAULT PARAMTERS'                                114
      BPATH_PDEF='PDEF*'                                                  114
      IDNUM_PDEF(1)=0                                                     115
C NONE                                                                    115
      PDEF_VERSION=1.0                                                    115
      CPDEF_VERSION='NONE'                                                115
C NUMBER OF SECTORS                                                       116
      PDEF_SECTOR=10                                                      116
      CPDEF_SECTOR='NUMBER OF SECTORS'                                    116
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    118
      PDEF_PHINUM(1)=0                                                    118
      PDEF_PHINUM(2)=324                                                  118
      PDEF_PHINUM(3)=288                                                  118
      PDEF_PHINUM(4)=252                                                  118
      PDEF_PHINUM(5)=216                                                  118
      PDEF_PHINUM(6)=180                                                  118
      PDEF_PHINUM(7)=144                                                  118
      PDEF_PHINUM(8)=108                                                  118
      PDEF_PHINUM(9)=72                                                   118
      PDEF_PHINUM(10)=36                                                  118
      CPDEF_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  118
     *'                                                                   118
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      119
      PDEF_ISON(1)=1                                                      119
      PDEF_ISON(2)=1                                                      119
      PDEF_ISON(3)=1                                                      119
      PDEF_ISON(4)=1                                                      119
      PDEF_ISON(5)=1                                                      119
      PDEF_ISON(6)=1                                                      119
      PDEF_ISON(7)=1                                                      119
      PDEF_ISON(8)=1                                                      119
      PDEF_ISON(9)=1                                                      119
      PDEF_ISON(10)=1                                                     119
      CPDEF_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     119
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PDEF',    120
     *BPATH_PDEF,IDNUM_PDEF,1, MAP_PDEF,NAMES_PDEF,COMMEN_PDEF,           120
     *BANK_PDEF,LEN_PDEF,LLEN_PDEF,LINK_PDEF,FLAG_PDEF)                   120
C *                                                                       120
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PDEF',    120
     *BPATH_PDEF,IDNUM_PDEF,1, MAP_PDEF,NAMES_PDEF,COMMEN_PDEF,           120
     *BANK_PDEF,LEN_PDEF,LLEN_PDEF,LINK_PDEF,FLAG_PDEF)                   120
C *                                                                       121
      CALL AGSBEGM(MODULE,IPRIN)                                          121
      IF (IPRIN.LT.0) RETURN                                              121
C Filling bank SENP - SENSORS PLACEMENT ON A LADDER                       121
      BANK_TITLE='SENSORS PLACEMENT ON A LADDER'                          121
      BPATH_SENP='SENP*'                                                  121
      IDNUM_SENP(1)=0                                                     122
C NONE                                                                    122
      SENP_VERSION=1.0                                                    122
      CSENP_VERSION='NONE'                                                122
C NUMBER OF SECTORS                                                       123
      SENP_SENSOR=10                                                      123
      CSENP_SENSOR='NUMBER OF SECTORS'                                    123
C Z POSITIONS [CM] OF THE 10 SENSORS INSIDE A LADDER                      126
      SENP_ZPOS(1)=-4.2315                                                126
      SENP_ZPOS(2)=-2.2055                                                126
      SENP_ZPOS(3)=-0.1795                                                126
      SENP_ZPOS(4)=1.8465                                                 126
      SENP_ZPOS(5)=3.8725                                                 126
      SENP_ZPOS(6)=5.8985                                                 126
      SENP_ZPOS(7)=7.9245                                                 126
      SENP_ZPOS(8)=9.9505                                                 126
      SENP_ZPOS(9)=11.9765                                                126
      SENP_ZPOS(10)=14.0025                                               126
      CSENP_ZPOS='Z POSITIONS [CM] OF THE 10 SENSORS INSIDE A LADDER'     126
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SENP',    127
     *BPATH_SENP,IDNUM_SENP,1, MAP_SENP,NAMES_SENP,COMMEN_SENP,           127
     *BANK_SENP,LEN_SENP,LLEN_SENP,LINK_SENP,FLAG_SENP)                   127
C *                                                                       127
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SENP',    127
     *BPATH_SENP,IDNUM_SENP,1, MAP_SENP,NAMES_SENP,COMMEN_SENP,           127
     *BANK_SENP,LEN_SENP,LLEN_SENP,LINK_SENP,FLAG_SENP)                   127
C *                                                                       128
      CALL AGSBEGM(MODULE,IPRIN)                                          128
      IF (IPRIN.LT.0) RETURN                                              128
C Filling bank PIXL - GENERAL PIXEL DETECTOR PARAMTERS                    128
      BANK_TITLE='GENERAL PIXEL DETECTOR PARAMTERS'                       128
      BPATH_PIXL='PIXL*'                                                  128
      IDNUM_PIXL(1)=0                                                     129
C NONE                                                                    129
      PIXL_VERSION=1.0                                                    129
      CPIXL_VERSION='NONE'                                                129
C NUMBER OF SECTORS                                                       130
      PIXL_SECTOR=10                                                      130
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    130
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    132
      PIXL_PHINUM(1)=0                                                    132
      PIXL_PHINUM(2)=324                                                  132
      PIXL_PHINUM(3)=288                                                  132
      PIXL_PHINUM(4)=252                                                  132
      PIXL_PHINUM(5)=216                                                  132
      PIXL_PHINUM(6)=180                                                  132
      PIXL_PHINUM(7)=144                                                  132
      PIXL_PHINUM(8)=108                                                  132
      PIXL_PHINUM(9)=72                                                   132
      PIXL_PHINUM(10)=36                                                  132
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  132
     *'                                                                   132
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      133
      PIXL_ISON(1)=1                                                      133
      PIXL_ISON(2)=1                                                      133
      PIXL_ISON(3)=1                                                      133
      PIXL_ISON(4)=1                                                      133
      PIXL_ISON(5)=1                                                      133
      PIXL_ISON(6)=1                                                      133
      PIXL_ISON(7)=1                                                      133
      PIXL_ISON(8)=1                                                      133
      PIXL_ISON(9)=1                                                      133
      PIXL_ISON(10)=1                                                     133
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     133
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    134
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           134
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   134
C *                                                                       134
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    134
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           134
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   134
C *                                                                       135
      CALL AGSBEGM(MODULE,IPRIN)                                          135
      IF (IPRIN.LT.0) RETURN                                              135
C Filling bank PIXL - JOINED PROTOTYPE 1-2-10                             135
      BANK_TITLE='JOINED PROTOTYPE 1-2-10'                                135
      BPATH_PIXL='PIXL*'                                                  135
      IDNUM_PIXL(1)=0                                                     136
C NONE                                                                    136
      PIXL_VERSION=2.0                                                    136
      CPIXL_VERSION='NONE'                                                136
C NUMBER OF SECTORS                                                       137
      PIXL_SECTOR=10                                                      137
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    137
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    139
      PIXL_PHINUM(1)=0                                                    139
      PIXL_PHINUM(2)=324                                                  139
      PIXL_PHINUM(3)=0                                                    139
      PIXL_PHINUM(4)=0                                                    139
      PIXL_PHINUM(5)=0                                                    139
      PIXL_PHINUM(6)=0                                                    139
      PIXL_PHINUM(7)=0                                                    139
      PIXL_PHINUM(8)=0                                                    139
      PIXL_PHINUM(9)=0                                                    139
      PIXL_PHINUM(10)=36                                                  139
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  139
     *'                                                                   139
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      140
      PIXL_ISON(1)=1                                                      140
      PIXL_ISON(2)=1                                                      140
      PIXL_ISON(3)=0                                                      140
      PIXL_ISON(4)=0                                                      140
      PIXL_ISON(5)=0                                                      140
      PIXL_ISON(6)=0                                                      140
      PIXL_ISON(7)=0                                                      140
      PIXL_ISON(8)=0                                                      140
      PIXL_ISON(9)=0                                                      140
      PIXL_ISON(10)=1                                                     140
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     140
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    141
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           141
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   141
C *                                                                       141
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    141
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           141
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   141
C *                                                                       142
      CALL AGSBEGM(MODULE,IPRIN)                                          142
      IF (IPRIN.LT.0) RETURN                                              142
C Filling bank PIXL - MERCEDES PROTOTYPE 1-5-7                            142
      BANK_TITLE='MERCEDES PROTOTYPE 1-5-7'                               142
      BPATH_PIXL='PIXL*'                                                  142
      IDNUM_PIXL(1)=0                                                     143
C NONE                                                                    143
      PIXL_VERSION=3.0                                                    143
      CPIXL_VERSION='NONE'                                                143
C NUMBER OF SECTORS                                                       144
      PIXL_SECTOR=10                                                      144
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    144
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    146
      PIXL_PHINUM(1)=0                                                    146
      PIXL_PHINUM(2)=0                                                    146
      PIXL_PHINUM(3)=0                                                    146
      PIXL_PHINUM(4)=0                                                    146
      PIXL_PHINUM(5)=216                                                  146
      PIXL_PHINUM(6)=0                                                    146
      PIXL_PHINUM(7)=144                                                  146
      PIXL_PHINUM(8)=0                                                    146
      PIXL_PHINUM(9)=0                                                    146
      PIXL_PHINUM(10)=0                                                   146
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  146
     *'                                                                   146
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      147
      PIXL_ISON(1)=1                                                      147
      PIXL_ISON(2)=0                                                      147
      PIXL_ISON(3)=0                                                      147
      PIXL_ISON(4)=0                                                      147
      PIXL_ISON(5)=1                                                      147
      PIXL_ISON(6)=0                                                      147
      PIXL_ISON(7)=1                                                      147
      PIXL_ISON(8)=0                                                      147
      PIXL_ISON(9)=0                                                      147
      PIXL_ISON(10)=0                                                     147
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     147
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    148
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           148
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   148
C *                                                                       148
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    148
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           148
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   148
C *                                                                       149
      CALL AGSBEGM(MODULE,IPRIN)                                          149
      IF (IPRIN.LT.0) RETURN                                              149
C Filling bank PIXL - MERCEDES PROTOTYPE 1-4-8                            149
      BANK_TITLE='MERCEDES PROTOTYPE 1-4-8'                               149
      BPATH_PIXL='PIXL*'                                                  149
      IDNUM_PIXL(1)=0                                                     150
C NONE                                                                    150
      PIXL_VERSION=4.0                                                    150
      CPIXL_VERSION='NONE'                                                150
C NUMBER OF SECTORS                                                       151
      PIXL_SECTOR=10                                                      151
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    151
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    153
      PIXL_PHINUM(1)=0                                                    153
      PIXL_PHINUM(2)=0                                                    153
      PIXL_PHINUM(3)=0                                                    153
      PIXL_PHINUM(4)=252                                                  153
      PIXL_PHINUM(5)=0                                                    153
      PIXL_PHINUM(6)=0                                                    153
      PIXL_PHINUM(7)=0                                                    153
      PIXL_PHINUM(8)=108                                                  153
      PIXL_PHINUM(9)=0                                                    153
      PIXL_PHINUM(10)=0                                                   153
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  153
     *'                                                                   153
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      154
      PIXL_ISON(1)=1                                                      154
      PIXL_ISON(2)=0                                                      154
      PIXL_ISON(3)=0                                                      154
      PIXL_ISON(4)=1                                                      154
      PIXL_ISON(5)=0                                                      154
      PIXL_ISON(6)=0                                                      154
      PIXL_ISON(7)=0                                                      154
      PIXL_ISON(8)=1                                                      154
      PIXL_ISON(9)=0                                                      154
      PIXL_ISON(10)=0                                                     154
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     154
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    155
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           155
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   155
C *                                                                       155
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    155
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           155
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   155
C *                                                                       156
      CALL AGSBEGM(MODULE,IPRIN)                                          156
      IF (IPRIN.LT.0) RETURN                                              156
C Filling bank PIXL - MERCEDES PROTOTYPE 1-4-7                            156
      BANK_TITLE='MERCEDES PROTOTYPE 1-4-7'                               156
      BPATH_PIXL='PIXL*'                                                  156
      IDNUM_PIXL(1)=0                                                     157
C NONE                                                                    157
      PIXL_VERSION=5.0                                                    157
      CPIXL_VERSION='NONE'                                                157
C NUMBER OF SECTORS                                                       158
      PIXL_SECTOR=10                                                      158
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    158
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    160
      PIXL_PHINUM(1)=0                                                    160
      PIXL_PHINUM(2)=0                                                    160
      PIXL_PHINUM(3)=0                                                    160
      PIXL_PHINUM(4)=252                                                  160
      PIXL_PHINUM(5)=0                                                    160
      PIXL_PHINUM(6)=0                                                    160
      PIXL_PHINUM(7)=144                                                  160
      PIXL_PHINUM(8)=0                                                    160
      PIXL_PHINUM(9)=0                                                    160
      PIXL_PHINUM(10)=0                                                   160
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  160
     *'                                                                   160
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      161
      PIXL_ISON(1)=1                                                      161
      PIXL_ISON(2)=0                                                      161
      PIXL_ISON(3)=0                                                      161
      PIXL_ISON(4)=1                                                      161
      PIXL_ISON(5)=0                                                      161
      PIXL_ISON(6)=0                                                      161
      PIXL_ISON(7)=1                                                      161
      PIXL_ISON(8)=0                                                      161
      PIXL_ISON(9)=0                                                      161
      PIXL_ISON(10)=0                                                     161
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     161
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    162
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           162
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   162
C *                                                                       162
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    162
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           162
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   162
C *                                                                       163
      CALL AGSBEGM(MODULE,IPRIN)                                          163
      IF (IPRIN.LT.0) RETURN                                              163
C Filling bank PIXL - MERCEDES PROTOTYPE 1-5-10: RUN13                    163
      BANK_TITLE='MERCEDES PROTOTYPE 1-5-10: RUN13'                       163
      BPATH_PIXL='PIXL*'                                                  163
      IDNUM_PIXL(1)=0                                                     164
C NONE                                                                    164
      PIXL_VERSION=6.0                                                    164
      CPIXL_VERSION='NONE'                                                164
C NUMBER OF SECTORS                                                       165
      PIXL_SECTOR=10                                                      165
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    165
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    167
      PIXL_PHINUM(1)=0                                                    167
      PIXL_PHINUM(2)=0                                                    167
      PIXL_PHINUM(3)=0                                                    167
      PIXL_PHINUM(4)=0                                                    167
      PIXL_PHINUM(5)=216                                                  167
      PIXL_PHINUM(6)=0                                                    167
      PIXL_PHINUM(7)=0                                                    167
      PIXL_PHINUM(8)=0                                                    167
      PIXL_PHINUM(9)=0                                                    167
      PIXL_PHINUM(10)=36                                                  167
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  167
     *'                                                                   167
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      168
      PIXL_ISON(1)=1                                                      168
      PIXL_ISON(2)=0                                                      168
      PIXL_ISON(3)=0                                                      168
      PIXL_ISON(4)=0                                                      168
      PIXL_ISON(5)=1                                                      168
      PIXL_ISON(6)=0                                                      168
      PIXL_ISON(7)=0                                                      168
      PIXL_ISON(8)=0                                                      168
      PIXL_ISON(9)=0                                                      168
      PIXL_ISON(10)=1                                                     168
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     168
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    169
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           169
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   169
C *                                                                       169
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    169
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           169
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   169
C *                                                                       170
      CALL AGSBEGM(MODULE,IPRIN)                                          170
      IF (IPRIN.LT.0) RETURN                                              170
C Filling bank PIXL - MERCEDES PROTOTYPE 2-7-9: RUN13                     170
      BANK_TITLE='MERCEDES PROTOTYPE 2-7-9: RUN13'                        170
      BPATH_PIXL='PIXL*'                                                  170
      IDNUM_PIXL(1)=0                                                     171
C NONE                                                                    171
      PIXL_VERSION=7.0                                                    171
      CPIXL_VERSION='NONE'                                                171
C NUMBER OF SECTORS                                                       172
      PIXL_SECTOR=10                                                      172
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    172
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    174
      PIXL_PHINUM(1)=0                                                    174
      PIXL_PHINUM(2)=324                                                  174
      PIXL_PHINUM(3)=288                                                  174
      PIXL_PHINUM(4)=252                                                  174
      PIXL_PHINUM(5)=216                                                  174
      PIXL_PHINUM(6)=180                                                  174
      PIXL_PHINUM(7)=144                                                  174
      PIXL_PHINUM(8)=108                                                  174
      PIXL_PHINUM(9)=72                                                   174
      PIXL_PHINUM(10)=36                                                  174
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  174
     *'                                                                   174
C 1 MEANS PRESENT, 0 MEANS NO LADDER                                      175
      PIXL_ISON(1)=0                                                      175
      PIXL_ISON(2)=1                                                      175
      PIXL_ISON(3)=0                                                      175
      PIXL_ISON(4)=1                                                      175
      PIXL_ISON(5)=0                                                      175
      PIXL_ISON(6)=0                                                      175
      PIXL_ISON(7)=1                                                      175
      PIXL_ISON(8)=0                                                      175
      PIXL_ISON(9)=0                                                      175
      PIXL_ISON(10)=0                                                     175
      CPIXL_ISON='1 MEANS PRESENT, 0 MEANS NO LADDER'                     175
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    176
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           176
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   176
C *                                                                       176
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    176
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           176
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   176
C *                                                                       177
      CALL AGSBEGM(MODULE,IPRIN)                                          177
      IF (IPRIN.LT.0) RETURN                                              177
C Filling bank PARO - DEFAULT                                             177
      BANK_TITLE='DEFAULT'                                                177
      BPATH_PARO='PARO*'                                                  177
      IDNUM_PARO(1)=0                                                     178
C NONE                                                                    178
      PARO_VERSION=0                                                      178
      CPARO_VERSION='NONE'                                                178
C NO SHIFT                                                                179
      PARO_DX=0                                                           179
      CPARO_DX='NO SHIFT'                                                 179
C NO SHIFT                                                                180
      PARO_DY=0                                                           180
      CPARO_DY='NO SHIFT'                                                 180
C NO SHIFT                                                                181
      PARO_DZ=0                                                           181
      CPARO_DZ='NO SHIFT'                                                 181
C NO SHIFT                                                                182
      PARO_MULT=0                                                         182
      CPARO_MULT='NO SHIFT'                                               182
C 1..10                                                                   183
      PARO_SEC(1)=0                                                       183
      PARO_SEC(2)=0                                                       183
      PARO_SEC(3)=0                                                       183
      PARO_SEC(4)=0                                                       183
      PARO_SEC(5)=0                                                       183
      PARO_SEC(6)=0                                                       183
      PARO_SEC(7)=0                                                       183
      PARO_SEC(8)=0                                                       183
      PARO_SEC(9)=0                                                       183
      PARO_SEC(10)=0                                                      183
      CPARO_SEC='1..10'                                                   183
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    184
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           184
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   184
C *                                                                       184
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    184
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           184
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   184
C *                                                                       185
      CALL AGSBEGM(MODULE,IPRIN)                                          185
      IF (IPRIN.LT.0) RETURN                                              185
C Filling bank PARO - SHIFTZ                                              185
      BANK_TITLE='SHIFTZ'                                                 185
      BPATH_PARO='PARO*'                                                  185
      IDNUM_PARO(1)=0                                                     186
C NONE                                                                    186
      PARO_VERSION=1                                                      186
      CPARO_VERSION='NONE'                                                186
C SHIFT                                                                   187
      PARO_DX=0                                                           187
      CPARO_DX='SHIFT'                                                    187
C NO SHIFT                                                                188
      PARO_DY=0                                                           188
      CPARO_DY='NO SHIFT'                                                 188
C NO SHIFT                                                                189
      PARO_DZ=1                                                           189
      CPARO_DZ='NO SHIFT'                                                 189
C MAX IS 5 CM                                                             190
      PARO_MULT=5                                                         190
      CPARO_MULT='MAX IS 5 CM'                                            190
C 1..10                                                                   191
      PARO_SEC(1)=0                                                       191
      PARO_SEC(2)=0                                                       191
      PARO_SEC(3)=0                                                       191
      PARO_SEC(4)=0                                                       191
      PARO_SEC(5)=0                                                       191
      PARO_SEC(6)=0                                                       191
      PARO_SEC(7)=0                                                       191
      PARO_SEC(8)=0                                                       191
      PARO_SEC(9)=0                                                       191
      PARO_SEC(10)=0                                                      191
      CPARO_SEC='1..10'                                                   191
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    192
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           192
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   192
C *                                                                       192
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    192
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           192
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   192
C *                                                                       193
      CALL AGSBEGM(MODULE,IPRIN)                                          193
      IF (IPRIN.LT.0) RETURN                                              193
C Filling bank PARA - DEFAULT                                             193
      BANK_TITLE='DEFAULT'                                                193
      BPATH_PARA='PARA*'                                                  193
      IDNUM_PARA(1)=0                                                     194
C NONE                                                                    194
      PARA_VERSION=0                                                      194
      CPARA_VERSION='NONE'                                                194
C NO ROTATION                                                             195
      PARA_DA=0                                                           195
      CPARA_DA='NO ROTATION'                                              195
C NO ROTATION                                                             196
      PARA_DB=0                                                           196
      CPARA_DB='NO ROTATION'                                              196
C NO ROTATION                                                             197
      PARA_DG=0                                                           197
      CPARA_DG='NO ROTATION'                                              197
C NO ROTATION                                                             198
      PARA_MULT=0                                                         198
      CPARA_MULT='NO ROTATION'                                            198
C 1..10                                                                   199
      PARA_SEC(1)=0                                                       199
      PARA_SEC(2)=0                                                       199
      PARA_SEC(3)=0                                                       199
      PARA_SEC(4)=0                                                       199
      PARA_SEC(5)=0                                                       199
      PARA_SEC(6)=0                                                       199
      PARA_SEC(7)=0                                                       199
      PARA_SEC(8)=0                                                       199
      PARA_SEC(9)=0                                                       199
      PARA_SEC(10)=0                                                      199
      CPARA_SEC='1..10'                                                   199
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    200
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           200
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   200
C *                                                                       200
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    200
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           200
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   200
C *                                                                       201
      CALL AGSBEGM(MODULE,IPRIN)                                          201
      IF (IPRIN.LT.0) RETURN                                              201
C Filling bank PARA - ROTATIONG                                           201
      BANK_TITLE='ROTATIONG'                                              201
      BPATH_PARA='PARA*'                                                  201
      IDNUM_PARA(1)=0                                                     202
C NONE                                                                    202
      PARA_VERSION=1                                                      202
      CPARA_VERSION='NONE'                                                202
C NO ROT.                                                                 203
      PARA_DA=0                                                           203
      CPARA_DA='NO ROT.'                                                  203
C NO ROT.                                                                 204
      PARA_DB=0                                                           204
      CPARA_DB='NO ROT.'                                                  204
C ROT.                                                                    205
      PARA_DG=5                                                           205
      CPARA_DG='ROT.'                                                     205
C MAX IS 5 DEG                                                            206
      PARA_MULT=1                                                         206
      CPARA_MULT='MAX IS 5 DEG'                                           206
C 1..10                                                                   207
      PARA_SEC(1)=0                                                       207
      PARA_SEC(2)=0                                                       207
      PARA_SEC(3)=0                                                       207
      PARA_SEC(4)=0                                                       207
      PARA_SEC(5)=0                                                       207
      PARA_SEC(6)=0                                                       207
      PARA_SEC(7)=0                                                       207
      PARA_SEC(8)=0                                                       207
      PARA_SEC(9)=0                                                       207
      PARA_SEC(10)=0                                                      207
      CPARA_SEC='1..10'                                                   207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    208
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           208
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    208
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           208
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   208
C *                                                                       209
      CALL AGSBEGM(MODULE,IPRIN)                                          209
      IF (IPRIN.LT.0) RETURN                                              209
C Filling bank LADR - NONE                                                209
      BANK_TITLE='NONE'                                                   209
      BPATH_LADR='LADR*'                                                  209
      IDNUM_LADR(1)=0                                                     210
C NONE                                                                    210
      LADR_VERSION=1                                                      210
      CLADR_VERSION='NONE'                                                210
C CU CABLE                                                                211
      LADR_TYPE=1                                                         211
      CLADR_TYPE='CU CABLE'                                               211
C NONE                                                                    212
      LADR_CABLETHICKNESS=0.009                                           212
      CLADR_CABLETHICKNESS='NONE'                                         212
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    213
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           213
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   213
C *                                                                       213
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    213
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           213
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   213
C *                                                                       214
      CALL AGSBEGM(MODULE,IPRIN)                                          214
      IF (IPRIN.LT.0) RETURN                                              214
C Filling bank LADR - NONE                                                214
      BANK_TITLE='NONE'                                                   214
      BPATH_LADR='LADR*'                                                  214
      IDNUM_LADR(1)=0                                                     215
C NONE                                                                    215
      LADR_VERSION=2                                                      215
      CLADR_VERSION='NONE'                                                215
C AL CABLE                                                                216
      LADR_TYPE=0                                                         216
      CLADR_TYPE='AL CABLE'                                               216
C NONE                                                                    217
      LADR_CABLETHICKNESS=0.011                                           217
      CLADR_CABLETHICKNESS='NONE'                                         217
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    218
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           218
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   218
C *                                                                       218
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LADR',    218
     *BPATH_LADR,IDNUM_LADR,1, MAP_LADR,NAMES_LADR,COMMEN_LADR,           218
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   218
C Geant COMPONENT : O ===> A=16 Z=8 W=0.164 <===                          219
      CALL AGSRESET                                                       219
      AG_A = 16                                                           219
      AG_Z = 8                                                            219
      AG_W = 0.164                                                        219
      AG_TITLE = 'O'                                                      219
      AG_EXNAME = 'COMPONENT'                                             219
      AG_PARLIST = 'A_Z_W'                                                219
      CALL AXCOMPONENT                                                    219
C *                                                                       219
C Geant COMPONENT : C ===> A=12 Z=6 W=0.763 <===                          219
      CALL AGSRESET                                                       220
      AG_A = 12                                                           220
      AG_Z = 6                                                            220
      AG_W = 0.763                                                        220
      AG_TITLE = 'C'                                                      220
      AG_EXNAME = 'COMPONENT'                                             220
      AG_PARLIST = 'A_Z_W'                                                220
      CALL AXCOMPONENT                                                    220
C *                                                                       220
C Geant COMPONENT : H ===> A=1 Z=1 W=0.073 <===                           220
      CALL AGSRESET                                                       221
      AG_A = 1                                                            221
      AG_Z = 1                                                            221
      AG_W = 0.073                                                        221
      AG_TITLE = 'H'                                                      221
      AG_EXNAME = 'COMPONENT'                                             221
      AG_PARLIST = 'A_Z_W'                                                221
      CALL AXCOMPONENT                                                    221
C *                                                                       221
C Geant MIXTURE : MATADHESIVE ===> DENS=1.2 <===                          221
      CALL AGSRESET                                                       222
      AG_DENS = 1.2                                                       222
      AG_TITLE = 'MATADHESIVE'                                            222
      AG_EXNAME = 'MIXTURE'                                               222
      AG_PARLIST = 'DENS'                                                 222
      CALL AXMIXTURE                                                      222
C *                                                                       222
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.281 <===                     222
      CALL AGSRESET                                                       223
      AG_A = 28.08                                                        223
      AG_Z = 14                                                           223
      AG_W = 0.281                                                        223
      AG_TITLE = 'SI'                                                     223
      AG_EXNAME = 'COMPONENT'                                             223
      AG_PARLIST = 'A_Z_W'                                                223
      CALL AXCOMPONENT                                                    223
C *                                                                       223
C Geant COMPONENT : O ===> A=16 Z=8 W=0.467 <===                          223
      CALL AGSRESET                                                       224
      AG_A = 16                                                           224
      AG_Z = 8                                                            224
      AG_W = 0.467                                                        224
      AG_TITLE = 'O'                                                      224
      AG_EXNAME = 'COMPONENT'                                             224
      AG_PARLIST = 'A_Z_W'                                                224
      CALL AXCOMPONENT                                                    224
C *                                                                       224
C Geant COMPONENT : C ===> A=12 Z=6 W=0.220 <===                          224
      CALL AGSRESET                                                       225
      AG_A = 12                                                           225
      AG_Z = 6                                                            225
      AG_W = 0.220                                                        225
      AG_TITLE = 'C'                                                      225
      AG_EXNAME = 'COMPONENT'                                             225
      AG_PARLIST = 'A_Z_W'                                                225
      CALL AXCOMPONENT                                                    225
C *                                                                       225
C Geant COMPONENT : H ===> A=1 Z=1 W=0.032 <===                           225
      CALL AGSRESET                                                       226
      AG_A = 1                                                            226
      AG_Z = 1                                                            226
      AG_W = 0.032                                                        226
      AG_TITLE = 'H'                                                      226
      AG_EXNAME = 'COMPONENT'                                             226
      AG_PARLIST = 'A_Z_W'                                                226
      CALL AXCOMPONENT                                                    226
C *                                                                       226
C Geant MIXTURE : MEDFR4 ===> DENS=1.80 <===                              226
      CALL AGSRESET                                                       227
      AG_DENS = 1.80                                                      227
      AG_TITLE = 'MEDFR4'                                                 227
      AG_EXNAME = 'MIXTURE'                                               227
      AG_PARLIST = 'DENS'                                                 227
      CALL AXMIXTURE                                                      227
C *                                                                       227
C Geant MATERIAL : CFRPMIX ===> A=12 Z=6 DENS=1.3 RADL=23.9 <===          227
      CALL AGSRESET                                                       228
      AG_A = 12                                                           228
      AG_Z = 6                                                            228
      AG_DENS = 1.3                                                       228
      AG_RADL = 23.9                                                      228
      AG_TITLE = 'CFRPMIX'                                                228
      AG_EXNAME = 'MATERIAL'                                              228
      AG_PARLIST = 'A_Z_DENS_RADL'                                        228
      CALL AXMATERIAL                                                     228
C *                                                                       228
C Geant MATERIAL : CBMIX ===> A=12 Z=6 DENS=1.3 RADL=68 <===              228
      CALL AGSRESET                                                       229
      AG_A = 12                                                           229
      AG_Z = 6                                                            229
      AG_DENS = 1.3                                                       229
      AG_RADL = 68                                                        229
      AG_TITLE = 'CBMIX'                                                  229
      AG_EXNAME = 'MATERIAL'                                              229
      AG_PARLIST = 'A_Z_DENS_RADL'                                        229
      CALL AXMATERIAL                                                     229
C *                                                                       229
C Geant COMPONENT : O ===> A=16 Z=8 W=0.209235 <===                       229
      CALL AGSRESET                                                       230
      AG_A = 16                                                           230
      AG_Z = 8                                                            230
      AG_W = 0.209235                                                     230
      AG_TITLE = 'O'                                                      230
      AG_EXNAME = 'COMPONENT'                                             230
      AG_PARLIST = 'A_Z_W'                                                230
      CALL AXCOMPONENT                                                    230
C *                                                                       230
C Geant COMPONENT : N ===> A=15 Z=7 W=0.073270 <===                       230
      CALL AGSRESET                                                       231
      AG_A = 15                                                           231
      AG_Z = 7                                                            231
      AG_W = 0.073270                                                     231
      AG_TITLE = 'N'                                                      231
      AG_EXNAME = 'COMPONENT'                                             231
      AG_PARLIST = 'A_Z_W'                                                231
      CALL AXCOMPONENT                                                    231
C *                                                                       231
C Geant COMPONENT : C ===> A=12 Z=6 W=0.691133 <===                       231
      CALL AGSRESET                                                       232
      AG_A = 12                                                           232
      AG_Z = 6                                                            232
      AG_W = 0.691133                                                     232
      AG_TITLE = 'C'                                                      232
      AG_EXNAME = 'COMPONENT'                                             232
      AG_PARLIST = 'A_Z_W'                                                232
      CALL AXCOMPONENT                                                    232
C *                                                                       232
C Geant COMPONENT : H ===> A=1 Z=1 W=0.026362 <===                        232
      CALL AGSRESET                                                       233
      AG_A = 1                                                            233
      AG_Z = 1                                                            233
      AG_W = 0.026362                                                     233
      AG_TITLE = 'H'                                                      233
      AG_EXNAME = 'COMPONENT'                                             233
      AG_PARLIST = 'A_Z_W'                                                233
      CALL AXCOMPONENT                                                    233
C *                                                                       233
C Geant MIXTURE : KAPTON ===> DENS=1.41 <===                              233
      CALL AGSRESET                                                       234
      AG_DENS = 1.41                                                      234
      AG_TITLE = 'KAPTON'                                                 234
      AG_EXNAME = 'MIXTURE'                                               234
      AG_PARLIST = 'DENS'                                                 234
      CALL AXMIXTURE                                                      234
C *                                                                       234
C Geant COMPONENT : CU ===> A=63.54 Z=29 W=0.802 <===                     234
      CALL AGSRESET                                                       235
      AG_A = 63.54                                                        235
      AG_Z = 29                                                           235
      AG_W = 0.802                                                        235
      AG_TITLE = 'CU'                                                     235
      AG_EXNAME = 'COMPONENT'                                             235
      AG_PARLIST = 'A_Z_W'                                                235
      CALL AXCOMPONENT                                                    235
C *                                                                       235
C Geant COMPONENT : KAPTON ===> A=12.767 Z=6.36 W=0.198 <===              235
      CALL AGSRESET                                                       236
      AG_A = 12.767                                                       236
      AG_Z = 6.36                                                         236
      AG_W = 0.198                                                        236
      AG_TITLE = 'KAPTON'                                                 236
      AG_EXNAME = 'COMPONENT'                                             236
      AG_PARLIST = 'A_Z_W'                                                236
      CALL AXCOMPONENT                                                    236
C *                                                                       236
C Geant MIXTURE : MATCUCABLE ===> DENS=4.37 <===                          236
      CALL AGSRESET                                                       237
      AG_DENS = 4.37                                                      237
      AG_TITLE = 'MATCUCABLE'                                             237
      AG_EXNAME = 'MIXTURE'                                               237
      AG_PARLIST = 'DENS'                                                 237
      CALL AXMIXTURE                                                      237
C *                                                                       237
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.626 <===                        237
      CALL AGSRESET                                                       238
      AG_A = 27                                                           238
      AG_Z = 13                                                           238
      AG_W = 0.626                                                        238
      AG_TITLE = 'AL'                                                     238
      AG_EXNAME = 'COMPONENT'                                             238
      AG_PARLIST = 'A_Z_W'                                                238
      CALL AXCOMPONENT                                                    238
C *                                                                       238
C Geant COMPONENT : KAPTON ===> A=12.767 Z=6.36 W=0.374 <===              238
      CALL AGSRESET                                                       239
      AG_A = 12.767                                                       239
      AG_Z = 6.36                                                         239
      AG_W = 0.374                                                        239
      AG_TITLE = 'KAPTON'                                                 239
      AG_EXNAME = 'COMPONENT'                                             239
      AG_PARLIST = 'A_Z_W'                                                239
      CALL AXCOMPONENT                                                    239
C *                                                                       239
C Geant MIXTURE : MATALCABLE ===> DENS=1.936 <===                         239
      CALL AGSRESET                                                       240
      AG_DENS = 1.936                                                     240
      AG_TITLE = 'MATALCABLE'                                             240
      AG_EXNAME = 'MIXTURE'                                               240
      AG_PARLIST = 'DENS'                                                 240
      CALL AXMIXTURE                                                      240
C *                                                                       240
C use default PXLW bank                                                   240
      BPATH_PXLW='PXLW*'                                                  241
      IDNUM_PXLW(1)=0                                                     241
      CALL AGDGETP('PIXLGEO5','*',-999,'PXLW',' ',BANK_PXLW, BPATH_PXLW,  241
     *IDNUM_PXLW,1,MAP_PXLW,NAMES_PXLW,BANK_PXLW,LEN_PXLW,LLEN_PXLW,      241
     *LINK_PXLW,FLAG_PXLW)                                                241
C use PDEF bank                                                           242
      BPATH_PDEF='PDEF*'                                                  242
      IDNUM_PDEF(1)=0                                                     242
      PDEF_VERSION=1.0                                                    242
      CALL AGDGETP('PIXLGEO5','*',-999,'PDEF',NPDEF_VERSION,              242
     *RE_PDEF_VERSION,BPATH_PDEF,IDNUM_PDEF,1,MAP_PDEF,NAMES_PDEF,        242
     *BANK_PDEF,LEN_PDEF,LLEN_PDEF,LINK_PDEF,FLAG_PDEF)                   242
C use SENP bank                                                           243
      BPATH_SENP='SENP*'                                                  243
      IDNUM_SENP(1)=0                                                     243
      SENP_VERSION=1.0                                                    243
      CALL AGDGETP('PIXLGEO5','*',-999,'SENP',NSENP_VERSION,              243
     *RE_SENP_VERSION,BPATH_SENP,IDNUM_SENP,1,MAP_SENP,NAMES_SENP,        243
     *BANK_SENP,LEN_SENP,LLEN_SENP,LINK_SENP,FLAG_SENP)                   243
C use LADR bank                                                           244
      BPATH_LADR='LADR*'                                                  244
      IDNUM_LADR(1)=0                                                     244
      LADR_VERSION=PXLW_LADRCONFIG                                        244
      CALL AGDGETP('PIXLGEO5','*',-999,'LADR',NLADR_VERSION,              244
     *RE_LADR_VERSION,BPATH_LADR,IDNUM_LADR,1,MAP_LADR,NAMES_LADR,        244
     *BANK_LADR,LEN_LADR,LLEN_LADR,LINK_LADR,FLAG_LADR)                   244
C use PIXL bank                                                           245
      BPATH_PIXL='PIXL*'                                                  245
      IDNUM_PIXL(1)=0                                                     245
      PIXL_VERSION=PXLW_SECVERSION                                        245
      CALL AGDGETP('PIXLGEO5','*',-999,'PIXL',NPIXL_VERSION,              245
     *RE_PIXL_VERSION,BPATH_PIXL,IDNUM_PIXL,1,MAP_PIXL,NAMES_PIXL,        245
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   245
C use PARO bank                                                           246
      BPATH_PARO='PARO*'                                                  246
      IDNUM_PARO(1)=0                                                     246
      PARO_VERSION=0                                                      246
      CALL AGDGETP('PIXLGEO5','*',-999,'PARO',NPARO_VERSION,              246
     *RE_PARO_VERSION,BPATH_PARO,IDNUM_PARO,1,MAP_PARO,NAMES_PARO,        246
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   246
C use PARA bank                                                           247
      BPATH_PARA='PARA*'                                                  247
      IDNUM_PARA(1)=0                                                     247
      PARA_VERSION=0                                                      247
      CALL AGDGETP('PIXLGEO5','*',-999,'PARA',NPARA_VERSION,              247
     *RE_PARA_VERSION,BPATH_PARA,IDNUM_PARA,1,MAP_PARA,NAMES_PARA,        247
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   247
      DEBUG = 0                                                           248
      WRITE(LOUT,5010)PXLW_SECVERSION                                     248
5010  FORMAT('PXL PXLW_version = ', F5.2)                                 248
      WRITE(LOUT,5020)PIXL_VERSION                                        249
5020  FORMAT(' PXL : PXL_version = ',F5.3)                                251
      WRITE(LOUT,5030)PXLW_SECVERSION                                     251
5030  FORMAT(' PXL : PXLW_SecVersion = ',F5.3)                            253
      WRITE(LOUT,5040)LADR_VERSION,LADR_TYPE                              253
5040  FORMAT(' PXL : LADR_version = ',I3,' LADR_type= ',I3)               255
      CALL AGSBEGM(MODULE,IPRIN)                                          256
C Check IPRIN<0                                                           256
      IF (IPRIN.LT.0) RETURN                                              256
C Create PXMO :                                                           256
C Call To Local Procedure PXMO                                            256
      ASSIGN 5060 TO BLOCK_PXMO                                           256
      GO TO  5050                                                         256
5060  CONTINUE                                                            256
C *                                                                       256
C Geant POSITION : PXMO ===> MOTHER='IDSM' X=0.0 Y=0.0 Z=0.0 KONLY='ONLY  257
C    *' <===                                                              257
      CALL AGSRESET                                                       257
      AG_MOTHER = 'IDSM'                                                  257
      AG_X = 0.0                                                          257
      AG_Y = 0.0                                                          257
      AG_Z = 0.0                                                          257
      AG_KONLY = 'ONLY'                                                   257
      AG_TITLE = 'PXMO'                                                   257
      AG_EXNAME = 'POSITION'                                              257
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   257
      CALL AXPOSITION                                                     257
C *                                                                       257
C ----------------------------------------------------------------------  257
C ------------ PXMO                                                       257
      CALL AGSENDM                                                        259
      RETURN                                                              259
C Begin Local Procedure PXMO                                              259
C PXMO MAIN VOLUME FOR ACTIVE                                             259
5050  CONTINUE                                                            259
         AG_TITLE='PXMO'                                                  259
         AG_MODULE=MODULE                                                 259
         AG_MARK='V00'                                                    259
         CALL AGSPUSH                                                     259
         AG_MODULE=MODULE                                                 259
C    *                                                                    259
C    Geant MATERIAL : AIR ===> inherited arguments used <===              260
         CALL AGSRESET                                                    260
         AG_TITLE = 'AIR'                                                 260
         AG_EXNAME = 'MATERIAL'                                           260
         AG_PARLIST = 'NONE'                                              260
         CALL AXMATERIAL                                                  260
C    *                                                                    260
C    Geant ATTRIBUTE : PXMO ===> SEEN=0 COLO=6 <===                       260
         CALL AGSRESET                                                    261
         AG_SEEN = 0                                                      261
         AG_COLO = 6                                                      261
         AG_TITLE = 'PXMO'                                                261
         AG_EXNAME = 'ATTRIBUTE'                                          261
         AG_PARLIST = 'SEEN_COLO'                                         261
         CALL AXATTRIBUTE                                                 261
C    *                                                                    261
C    Geant SHAPE : TUBE ===> RMIN=MINRVOL RMAX=MAXRVOL DZ=MAXZVOL <===    261
         CALL AGSRESET                                                    262
         AG_RMIN = MINRVOL                                                262
         AG_RMAX = MAXRVOL                                                262
         AG_DZ = MAXZVOL                                                  262
         AG_TITLE = 'TUBE'                                                262
         AG_EXNAME = 'SHAPE'                                              262
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      262
         CALL AXSHAPE                                                     262
C    *                                                                    262
C    *                                                                    262
         JJ=1                                                             263
            GO TO 5073                                                    263
5071        JJ=JJ+1                                                       263
5073        IF(JJ-(10).GT.0) GO TO 5072                                   263
            ANGLE = PDEF_PHINUM(JJ)                                       264
            ANGLEPOS=ANGLE*(PI/180.0)                                     265
            XPOS=.00001*SIN(ANGLEPOS) +                                   266
     *      (PARO_DX*PARO_MULT)*PARO_SEC(JJ)                              266
            YPOS=.00001*COS(ANGLEPOS) +                                   267
     *      (PARO_DY*PARO_MULT)*PARO_SEC(JJ)                              267
            ZPOS = (PARO_DZ*PARO_MULT)*PARO_SEC(JJ)                       268
            ALPHAZ = -1*ANGLE -1*(PARA_DG*PARA_MULT)*PARA_SEC(JJ)         269
            ALPHAX = -1*(PARA_DA*PARA_MULT)*PARA_SEC(JJ)                  270
            ALPHAY = -1*(PARA_DB*PARA_MULT)*PARA_SEC(JJ)                  271
C       Check PIXL_ISON(JJ)!=1                                            272
            IF (PIXL_ISON(JJ).NE.1) THEN                                  272
C       Check DEBUG==1                                                    273
            IF (DEBUG.EQ.1) THEN                                          273
            WRITE(LOUT,5080)ANGLE                                         273
5080        FORMAT('Blank Sector angle(deg)=', F7.3)                      273
            WRITE(LOUT,5090)JJ                                            274
5090        FORMAT('sector=', I2)                                         274
            WRITE(LOUT,5100)XPOS                                          275
5100        FORMAT('xpos=', F10.5)                                        275
            WRITE(LOUT,5110)YPOS                                          276
5110        FORMAT('ypos=', F10.5)                                        276
            WRITE(LOUT,5120)ZPOS                                          277
5120        FORMAT('zpos=', F6.2)                                         277
            WRITE(LOUT,5130)ALPHAZ                                        278
5130        FORMAT('alphaZ=', F7.2)                                       278
            WRITE(LOUT,5140)ALPHAX                                        279
5140        FORMAT('alphaX=', F6.2)                                       279
            WRITE(LOUT,5150)ALPHAY                                        280
5150        FORMAT('alphaY=', F6.2)                                       280
            ENDIF                                                         282
C       Create PXSE :                                                     283
C       Call To Local Procedure PXSE                                      283
            ASSIGN 5170 TO BLOCK_PXSE                                     283
            GO TO  5160                                                   283
5170        CONTINUE                                                      283
C       *                                                                 283
C       Geant POSITION : PXSE ===> X=XPOS Y=YPOS Z=ZPOS ALPHAZ=ALPHAZ AL  285
C       PHAX=ALPHAX ALPHAY=ALPHAY NCOPY=JJ KONLY='MANY' <===              285
            CALL AGSRESET                                                 285
            AG_X = XPOS                                                   285
            AG_Y = YPOS                                                   285
            AG_Z = ZPOS                                                   285
            AG_ALPHAN=AG_ALPHAN+1                                         285
            AG_ALPHAK(AG_ALPHAN)=3                                        285
            AG_ALPHAL(AG_ALPHAN)=ALPHAZ                                   285
            AG_ALPHAN=AG_ALPHAN+1                                         285
            AG_ALPHAK(AG_ALPHAN)=1                                        285
            AG_ALPHAL(AG_ALPHAN)=ALPHAX                                   285
            AG_ALPHAN=AG_ALPHAN+1                                         285
            AG_ALPHAK(AG_ALPHAN)=2                                        285
            AG_ALPHAL(AG_ALPHAN)=ALPHAY                                   285
            AG_NCOPY = JJ                                                 285
            AG_KONLY = 'MANY'                                             285
            AG_TITLE = 'PXSE'                                             285
            AG_EXNAME = 'POSITION'                                        285
            AG_PARLIST = 'X_Y_Z_ALPHAZ_ALPHAX_ALPHAY_NCOPY_KONLY'         285
            CALL AXPOSITION                                               285
C       *                                                                 285
            ENDIF                                                         285
         GO TO 5071                                                       287
5072     CONTINUE                                                         287
C    *                                                                    288
         II=1                                                             288
            GO TO 5183                                                    288
5181        II=II+1                                                       288
5183        IF(II-(PIXL_SECTOR).GT.0) GO TO 5182                          288
            ANGLE = PIXL_PHINUM(II)                                       289
            ANGLEPOS=ANGLE*(PI/180.0)                                     290
            XPOS=.00001*SIN(ANGLEPOS) +                                   291
     *      (PARO_DX*PARO_MULT)*PARO_SEC(II)                              291
            YPOS=.00001*COS(ANGLEPOS) +                                   292
     *      (PARO_DY*PARO_MULT)*PARO_SEC(II)                              292
            ZPOS = (PARO_DZ*PARO_MULT)*PARO_SEC(II)                       293
            ALPHAZ = -1*ANGLE -1*(PARA_DG*PARA_MULT)*PARA_SEC(II)         294
            ALPHAX = -1*(PARA_DA*PARA_MULT)*PARA_SEC(II)                  295
            ALPHAY = -1*(PARA_DB*PARA_MULT)*PARA_SEC(II)                  296
C       Check PIXL_ISON(II)==1                                            297
            IF (PIXL_ISON(II).EQ.1) THEN                                  297
C       Check DEBUG==1                                                    298
            IF (DEBUG.EQ.1) THEN                                          298
            WRITE(LOUT,5190)II                                            298
5190        FORMAT('Instrumented sector=', I2)                            298
            WRITE(LOUT,5200)ANGLE                                         299
5200        FORMAT('angle(deg)=', F7.3)                                   299
            WRITE(LOUT,5210)XPOS                                          300
5210        FORMAT('xpos=', F10.5)                                        300
            WRITE(LOUT,5220)YPOS                                          301
5220        FORMAT('ypos=', F10.5)                                        301
            WRITE(LOUT,5230)ZPOS                                          302
5230        FORMAT('zpos=', F10.5)                                        302
            WRITE(LOUT,5240)ALPHAZ                                        303
5240        FORMAT('alphaZ=', F7.2)                                       303
            WRITE(LOUT,5250)ALPHAX                                        304
5250        FORMAT('alphaX=', F6.2)                                       304
            WRITE(LOUT,5260)ALPHAY                                        305
5260        FORMAT('alphaY=', F6.2)                                       305
            ENDIF                                                         307
C       Create PXLA :                                                     308
C       Call To Local Procedure PXLA                                      308
            ASSIGN 5280 TO BLOCK_PXLA                                     308
            GO TO  5270                                                   308
5280        CONTINUE                                                      308
C       *                                                                 308
C       Geant POSITION : PXLA ===> MOTHER='PXMO' X=XPOS Y=YPOS Z=ZPOS AL  310
C       PHAZ=ALPHAZ ALPHAX=ALPHAX ALPHAY=ALPHAY NCOPY=II KONLY='MANY' <=  310
C       ==                                                                310
            CALL AGSRESET                                                 310
            AG_MOTHER = 'PXMO'                                            310
            AG_X = XPOS                                                   310
            AG_Y = YPOS                                                   310
            AG_Z = ZPOS                                                   310
            AG_ALPHAN=AG_ALPHAN+1                                         310
            AG_ALPHAK(AG_ALPHAN)=3                                        310
            AG_ALPHAL(AG_ALPHAN)=ALPHAZ                                   310
            AG_ALPHAN=AG_ALPHAN+1                                         310
            AG_ALPHAK(AG_ALPHAN)=1                                        310
            AG_ALPHAL(AG_ALPHAN)=ALPHAX                                   310
            AG_ALPHAN=AG_ALPHAN+1                                         310
            AG_ALPHAK(AG_ALPHAN)=2                                        310
            AG_ALPHAL(AG_ALPHAN)=ALPHAY                                   310
            AG_NCOPY = II                                                 310
            AG_KONLY = 'MANY'                                             310
            AG_TITLE = 'PXLA'                                             310
            AG_EXNAME = 'POSITION'                                        310
            AG_PARLIST =                                                  310
     *      'MOTHER_X_Y_Z_ALPHAZ_ALPHAX_ALPHAY_NCOPY_KONLY'               310
            CALL AXPOSITION                                               310
C       *                                                                 310
            ENDIF                                                         310
         GO TO 5181                                                       312
5182     CONTINUE                                                         312
5051  CALL AGSPOPB                                                        313
      GO TO BLOCK_PXMO, (5050 ,5060)                                      313
C End of Local Procedure                                                  313
C ----------------------------------------------------------------------  314
C ------------ PXSE                                                       314
C Begin Local Procedure PXSE                                              315
C PXSE PIXEL SECTOR                                                       315
5160  CONTINUE                                                            315
         AG_TITLE='PXSE'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
C    Geant MATERIAL : AIR ===> inherited arguments used <===              316
         CALL AGSRESET                                                    316
         AG_TITLE = 'AIR'                                                 316
         AG_EXNAME = 'MATERIAL'                                           316
         AG_PARLIST = 'NONE'                                              316
         CALL AXMATERIAL                                                  316
C    *                                                                    316
C    Geant ATTRIBUTE : PXSE ===> SEEN=0 COLO=1 <===                       316
         CALL AGSRESET                                                    317
         AG_SEEN = 0                                                      317
         AG_COLO = 1                                                      317
         AG_TITLE = 'PXSE'                                                317
         AG_EXNAME = 'ATTRIBUTE'                                          317
         AG_PARLIST = 'SEEN_COLO'                                         317
         CALL AXATTRIBUTE                                                 317
C    *                                                                    317
C    Geant SHAPE : TUBS ===> RMIN=MINRADSEC RMAX=MAXRADSEC, PHI1=MINPHIS  317
C    ECBARE PHI2=MAXPHISECBARE DZ=HALFMAXZ <===                           317
         CALL AGSRESET                                                    319
         AG_RMIN = MINRADSEC                                              319
         AG_RMAX = MAXRADSEC                                              319
         AG_PHI1 = MINPHISECBARE                                          319
         AG_PHI2 = MAXPHISECBARE                                          319
         AG_DZ = HALFMAXZ                                                 319
         AG_TITLE = 'TUBS'                                                319
         AG_EXNAME = 'SHAPE'                                              319
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            319
         CALL AXSHAPE                                                     319
C    *                                                                    319
C    Create PXCA :                                                        320
C    Call To Local Procedure PXCA                                         320
         ASSIGN 5300 TO BLOCK_PXCA                                        320
         GO TO  5290                                                      320
5300     CONTINUE                                                         320
C    *                                                                    320
C    Geant POSITION : PXCA ===> X=-0.25 Y=2.73819 Z=SECTORZOFFSET <===    321
         CALL AGSRESET                                                    321
         AG_X = -0.25                                                     321
         AG_Y = 2.73819                                                   321
         AG_Z = SECTORZOFFSET                                             321
         AG_TITLE = 'PXCA'                                                321
         AG_EXNAME = 'POSITION'                                           321
         AG_PARLIST = 'X_Y_Z'                                             321
         CALL AXPOSITION                                                  321
C    *                                                                    321
C    Create PXCC :                                                        322
C    Call To Local Procedure PXCC                                         322
         ASSIGN 5320 TO BLOCK_PXCC                                        322
         GO TO  5310                                                      322
5320     CONTINUE                                                         322
C    *                                                                    322
C    Geant POSITION : PXCC ===> X=-0.25 Y=7.78014 Z=SECTORZOFFSET <===    323
         CALL AGSRESET                                                    323
         AG_X = -0.25                                                     323
         AG_Y = 7.78014                                                   323
         AG_Z = SECTORZOFFSET                                             323
         AG_TITLE = 'PXCC'                                                323
         AG_EXNAME = 'POSITION'                                           323
         AG_PARLIST = 'X_Y_Z'                                             323
         CALL AXPOSITION                                                  323
C    *                                                                    323
C    Create PXCD :                                                        324
C    Call To Local Procedure PXCD                                         324
         ASSIGN 5340 TO BLOCK_PXCD                                        324
         GO TO  5330                                                      324
5340     CONTINUE                                                         324
C    *                                                                    324
C    Geant POSITION : PXCD ===> X=-1.482 Y=7.9982 Z=SECTORZOFFSET <===    325
         CALL AGSRESET                                                    325
         AG_X = -1.482                                                    325
         AG_Y = 7.9982                                                    325
         AG_Z = SECTORZOFFSET                                             325
         AG_TITLE = 'PXCD'                                                325
         AG_EXNAME = 'POSITION'                                           325
         AG_PARLIST = 'X_Y_Z'                                             325
         CALL AXPOSITION                                                  325
C    *                                                                    325
C    Create PXCE :                                                        326
C    Call To Local Procedure PXCE                                         326
         ASSIGN 5360 TO BLOCK_PXCE                                        326
         GO TO  5350                                                      326
5360     CONTINUE                                                         326
C    *                                                                    326
C    Geant POSITION : PXCE ===> X=-1.76032 Y=7.86181 Z=SECTORZOFFSET <==  327
C    =                                                                    327
         CALL AGSRESET                                                    327
         AG_X = -1.76032                                                  327
         AG_Y = 7.86181                                                   327
         AG_Z = SECTORZOFFSET                                             327
         AG_TITLE = 'PXCE'                                                327
         AG_EXNAME = 'POSITION'                                           327
         AG_PARLIST = 'X_Y_Z'                                             327
         CALL AXPOSITION                                                  327
C    *                                                                    327
C    Create PXCF :                                                        328
C    Call To Local Procedure PXCF                                         328
         ASSIGN 5380 TO BLOCK_PXCF                                        328
         GO TO  5370                                                      328
5380     CONTINUE                                                         328
C    *                                                                    328
C    Geant POSITION : PXCF ===> X=-3.11253 Y=7.51529 Z=SECTORZOFFSET <==  329
C    =                                                                    329
         CALL AGSRESET                                                    329
         AG_X = -3.11253                                                  329
         AG_Y = 7.51529                                                   329
         AG_Z = SECTORZOFFSET                                             329
         AG_TITLE = 'PXCF'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'X_Y_Z'                                             329
         CALL AXPOSITION                                                  329
C    *                                                                    329
C    Create PXCG :                                                        330
C    Call To Local Procedure PXCG                                         330
         ASSIGN 5400 TO BLOCK_PXCG                                        330
         GO TO  5390                                                      330
5400     CONTINUE                                                         330
C    *                                                                    330
C    Geant POSITION : PXCG ===> X=-3.35642 Y=7.32402 Z=SECTORZOFFSET <==  331
C    =                                                                    331
         CALL AGSRESET                                                    331
         AG_X = -3.35642                                                  331
         AG_Y = 7.32402                                                   331
         AG_Z = SECTORZOFFSET                                             331
         AG_TITLE = 'PXCG'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'X_Y_Z'                                             331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Create PXCH :                                                        332
C    Call To Local Procedure PXCH                                         332
         ASSIGN 5420 TO BLOCK_PXCH                                        332
         GO TO  5410                                                      332
5420     CONTINUE                                                         332
C    *                                                                    332
C    Geant POSITION : PXCH ===> X=-4.56862 Y=6.71349 Z=SECTORZOFFSET <==  333
C    =                                                                    333
         CALL AGSRESET                                                    333
         AG_X = -4.56862                                                  333
         AG_Y = 6.71349                                                   333
         AG_Z = SECTORZOFFSET                                             333
         AG_TITLE = 'PXCH'                                                333
         AG_EXNAME = 'POSITION'                                           333
         AG_PARLIST = 'X_Y_Z'                                             333
         CALL AXPOSITION                                                  333
C    *                                                                    333
C    Create PXCB :                                                        334
C    Call To Local Procedure PXCB                                         334
         ASSIGN 5440 TO BLOCK_PXCB                                        334
         GO TO  5430                                                      334
5440     CONTINUE                                                         334
C    *                                                                    334
C    Geant POSITION : PXCB ===> X=-1.68615 Y=2.88554 Z=SECTORZOFFSET <==  335
C    =                                                                    335
         CALL AGSRESET                                                    335
         AG_X = -1.68615                                                  335
         AG_Y = 2.88554                                                   335
         AG_Z = SECTORZOFFSET                                             335
         AG_TITLE = 'PXCB'                                                335
         AG_EXNAME = 'POSITION'                                           335
         AG_PARLIST = 'X_Y_Z'                                             335
         CALL AXPOSITION                                                  335
C    *                                                                    335
C    Create PXRB :                                                        336
C    Call To Local Procedure PXRB                                         336
         ASSIGN 5460 TO BLOCK_PXRB                                        336
         GO TO  5450                                                      336
5460     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : PXRB ===> X=-0.112 Y=5.25916 Z=SECTORZOFFSET ALPHA  337
C    Z=90 <===                                                            337
         CALL AGSRESET                                                    337
         AG_X = -0.112                                                    337
         AG_Y = 5.25916                                                   337
         AG_Z = SECTORZOFFSET                                             337
         AG_ALPHAN=AG_ALPHAN+1                                            337
         AG_ALPHAK(AG_ALPHAN)=3                                           337
         AG_ALPHAL(AG_ALPHAN)=90                                          337
         AG_TITLE = 'PXRB'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      337
         CALL AXPOSITION                                                  337
C    *                                                                    337
C    Create PXTR :                                                        338
C    Call To Local Procedure PXTR                                         338
         ASSIGN 5480 TO BLOCK_PXTR                                        338
         GO TO  5470                                                      338
5480     CONTINUE                                                         338
C    *                                                                    338
C    Geant POSITION : PXTR ===> X=-0.841949 Y=8.02506 Z=SECTORZOFFSET AL  339
C    PHAZ=169.963 <===                                                    339
         CALL AGSRESET                                                    339
         AG_X = -0.841949                                                 339
         AG_Y = 8.02506                                                   339
         AG_Z = SECTORZOFFSET                                             339
         AG_ALPHAN=AG_ALPHAN+1                                            339
         AG_ALPHAK(AG_ALPHAN)=3                                           339
         AG_ALPHAL(AG_ALPHAN)=169.963                                     339
         AG_TITLE = 'PXTR'                                                339
         AG_EXNAME = 'POSITION'                                           339
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      339
         CALL AXPOSITION                                                  339
C    *                                                                    339
C    Create PXTM :                                                        340
C    Call To Local Procedure PXTM                                         340
         ASSIGN 5500 TO BLOCK_PXTM                                        340
         GO TO  5490                                                      340
5500     CONTINUE                                                         340
C    *                                                                    340
C    Geant POSITION : PXTM ===> X=-2.43602 Y=7.67656 Z=SECTORZOFFSET ALP  341
C    HAZ=181.963 <===                                                     341
         CALL AGSRESET                                                    341
         AG_X = -2.43602                                                  341
         AG_Y = 7.67656                                                   341
         AG_Z = SECTORZOFFSET                                             341
         AG_ALPHAN=AG_ALPHAN+1                                            341
         AG_ALPHAK(AG_ALPHAN)=3                                           341
         AG_ALPHAL(AG_ALPHAN)=181.963                                     341
         AG_TITLE = 'PXTM'                                                341
         AG_EXNAME = 'POSITION'                                           341
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      341
         CALL AXPOSITION                                                  341
C    *                                                                    341
C    Create PXTL :                                                        342
C    Call To Local Procedure PXTL                                         342
         ASSIGN 5520 TO BLOCK_PXTL                                        342
         GO TO  5510                                                      342
5520     CONTINUE                                                         342
C    *                                                                    342
C    Geant POSITION : PXTL ===> X=-3.95962 Y=7.00711 Z=SECTORZOFFSET ALP  343
C    HAZ=193.963 <===                                                     343
         CALL AGSRESET                                                    343
         AG_X = -3.95962                                                  343
         AG_Y = 7.00711                                                   343
         AG_Z = SECTORZOFFSET                                             343
         AG_ALPHAN=AG_ALPHAN+1                                            343
         AG_ALPHAK(AG_ALPHAN)=3                                           343
         AG_ALPHAL(AG_ALPHAN)=193.963                                     343
         AG_TITLE = 'PXTL'                                                343
         AG_EXNAME = 'POSITION'                                           343
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      343
         CALL AXPOSITION                                                  343
C    *                                                                    343
C    Create PXTJ :                                                        344
C    Call To Local Procedure PXTJ                                         344
         ASSIGN 5540 TO BLOCK_PXTJ                                        344
         GO TO  5530                                                      344
5540     CONTINUE                                                         344
C    *                                                                    344
C    Geant POSITION : PXTJ ===> X=-3.22348 Y=7.42445 Z=SECTORZOFFSET ALP  345
C    HAZ=293.554 <===                                                     345
         CALL AGSRESET                                                    345
         AG_X = -3.22348                                                  345
         AG_Y = 7.42445                                                   345
         AG_Z = SECTORZOFFSET                                             345
         AG_ALPHAN=AG_ALPHAN+1                                            345
         AG_ALPHAK(AG_ALPHAN)=3                                           345
         AG_ALPHAL(AG_ALPHAN)=293.554                                     345
         AG_TITLE = 'PXTJ'                                                345
         AG_EXNAME = 'POSITION'                                           345
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      345
         CALL AXPOSITION                                                  345
C    *                                                                    345
C    Create PXTI :                                                        346
C    Call To Local Procedure PXTI                                         346
         ASSIGN 5560 TO BLOCK_PXTI                                        346
         GO TO  5550                                                      346
5560     CONTINUE                                                         346
C    *                                                                    346
C    Geant POSITION : PXTI ===> X=-1.60941 Y=7.93241 Z=SECTORZOFFSET ALP  347
C    HAZ=281.554 <===                                                     347
         CALL AGSRESET                                                    347
         AG_X = -1.60941                                                  347
         AG_Y = 7.93241                                                   347
         AG_Z = SECTORZOFFSET                                             347
         AG_ALPHAN=AG_ALPHAN+1                                            347
         AG_ALPHAK(AG_ALPHAN)=3                                           347
         AG_ALPHAL(AG_ALPHAN)=281.554                                     347
         AG_TITLE = 'PXTI'                                                347
         AG_EXNAME = 'POSITION'                                           347
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      347
         CALL AXPOSITION                                                  347
C    *                                                                    347
C    Create PXLB :                                                        348
C    Call To Local Procedure PXLB                                         348
         ASSIGN 5580 TO BLOCK_PXLB                                        348
         GO TO  5570                                                      348
5580     CONTINUE                                                         348
C    *                                                                    348
C    Geant POSITION : PXLB ===> X=-3.27218 Y=4.69431 Z=SECTORZOFFSET ALP  349
C    HAZ=306 <===                                                         349
         CALL AGSRESET                                                    349
         AG_X = -3.27218                                                  349
         AG_Y = 4.69431                                                   349
         AG_Z = SECTORZOFFSET                                             349
         AG_ALPHAN=AG_ALPHAN+1                                            349
         AG_ALPHAK(AG_ALPHAN)=3                                           349
         AG_ALPHAL(AG_ALPHAN)=306                                         349
         AG_TITLE = 'PXLB'                                                349
         AG_EXNAME = 'POSITION'                                           349
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      349
         CALL AXPOSITION                                                  349
C    *                                                                    349
C    Create PXIB :                                                        350
C    Call To Local Procedure PXIB                                         350
         ASSIGN 5600 TO BLOCK_PXIB                                        350
         GO TO  5590                                                      350
5600     CONTINUE                                                         350
C    *                                                                    350
C    Geant POSITION : PXIB ===> X=-0.976207 Y=2.63307 Z=SECTORZOFFSET AL  351
C    PHAZ=357.396 <===                                                    351
         CALL AGSRESET                                                    351
         AG_X = -0.976207                                                 351
         AG_Y = 2.63307                                                   351
         AG_Z = SECTORZOFFSET                                             351
         AG_ALPHAN=AG_ALPHAN+1                                            351
         AG_ALPHAK(AG_ALPHAN)=3                                           351
         AG_ALPHAL(AG_ALPHAN)=357.396                                     351
         AG_TITLE = 'PXIB'                                                351
         AG_EXNAME = 'POSITION'                                           351
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      351
         CALL AXPOSITION                                                  351
C    *                                                                    351
C    Create DTCA :                                                        352
C    Call To Local Procedure DTCA                                         352
         ASSIGN 5620 TO BLOCK_DTCA                                        352
         GO TO  5610                                                      352
5620     CONTINUE                                                         352
C    *                                                                    352
C    Geant POSITION : DTCA ===> X=-0.25 Y=2.73819 Z=DOVETAILZOFFSET <===  353
         CALL AGSRESET                                                    353
         AG_X = -0.25                                                     353
         AG_Y = 2.73819                                                   353
         AG_Z = DOVETAILZOFFSET                                           353
         AG_TITLE = 'DTCA'                                                353
         AG_EXNAME = 'POSITION'                                           353
         AG_PARLIST = 'X_Y_Z'                                             353
         CALL AXPOSITION                                                  353
C    *                                                                    353
C    Create DTCB :                                                        354
C    Call To Local Procedure DTCB                                         354
         ASSIGN 5640 TO BLOCK_DTCB                                        354
         GO TO  5630                                                      354
5640     CONTINUE                                                         354
C    *                                                                    354
C    Geant POSITION : DTCB ===> X=-0.25 Y=7.78014 Z=DOVETAILZOFFSET <===  355
         CALL AGSRESET                                                    355
         AG_X = -0.25                                                     355
         AG_Y = 7.78014                                                   355
         AG_Z = DOVETAILZOFFSET                                           355
         AG_TITLE = 'DTCB'                                                355
         AG_EXNAME = 'POSITION'                                           355
         AG_PARLIST = 'X_Y_Z'                                             355
         CALL AXPOSITION                                                  355
C    *                                                                    355
C    Create DTCC :                                                        356
C    Call To Local Procedure DTCC                                         356
         ASSIGN 5660 TO BLOCK_DTCC                                        356
         GO TO  5650                                                      356
5660     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : DTCC ===> X=-1.482 Y=7.9982 Z=DOVETAILZOFFSET <===  357
         CALL AGSRESET                                                    357
         AG_X = -1.482                                                    357
         AG_Y = 7.9982                                                    357
         AG_Z = DOVETAILZOFFSET                                           357
         AG_TITLE = 'DTCC'                                                357
         AG_EXNAME = 'POSITION'                                           357
         AG_PARLIST = 'X_Y_Z'                                             357
         CALL AXPOSITION                                                  357
C    *                                                                    357
C    Create DTCD :                                                        358
C    Call To Local Procedure DTCD                                         358
         ASSIGN 5680 TO BLOCK_DTCD                                        358
         GO TO  5670                                                      358
5680     CONTINUE                                                         358
C    *                                                                    358
C    Geant POSITION : DTCD ===> X=-1.76032 Y=7.86181 Z=DOVETAILZOFFSET <  359
C    ===                                                                  359
         CALL AGSRESET                                                    359
         AG_X = -1.76032                                                  359
         AG_Y = 7.86181                                                   359
         AG_Z = DOVETAILZOFFSET                                           359
         AG_TITLE = 'DTCD'                                                359
         AG_EXNAME = 'POSITION'                                           359
         AG_PARLIST = 'X_Y_Z'                                             359
         CALL AXPOSITION                                                  359
C    *                                                                    359
C    Create DTCE :                                                        360
C    Call To Local Procedure DTCE                                         360
         ASSIGN 5700 TO BLOCK_DTCE                                        360
         GO TO  5690                                                      360
5700     CONTINUE                                                         360
C    *                                                                    360
C    Geant POSITION : DTCE ===> X=-3.11253 Y=7.51529 Z=DOVETAILZOFFSET <  361
C    ===                                                                  361
         CALL AGSRESET                                                    361
         AG_X = -3.11253                                                  361
         AG_Y = 7.51529                                                   361
         AG_Z = DOVETAILZOFFSET                                           361
         AG_TITLE = 'DTCE'                                                361
         AG_EXNAME = 'POSITION'                                           361
         AG_PARLIST = 'X_Y_Z'                                             361
         CALL AXPOSITION                                                  361
C    *                                                                    361
C    Create DTCF :                                                        362
C    Call To Local Procedure DTCF                                         362
         ASSIGN 5720 TO BLOCK_DTCF                                        362
         GO TO  5710                                                      362
5720     CONTINUE                                                         362
C    *                                                                    362
C    Geant POSITION : DTCF ===> X=-3.35642 Y=7.32402 Z=DOVETAILZOFFSET <  363
C    ===                                                                  363
         CALL AGSRESET                                                    363
         AG_X = -3.35642                                                  363
         AG_Y = 7.32402                                                   363
         AG_Z = DOVETAILZOFFSET                                           363
         AG_TITLE = 'DTCF'                                                363
         AG_EXNAME = 'POSITION'                                           363
         AG_PARLIST = 'X_Y_Z'                                             363
         CALL AXPOSITION                                                  363
C    *                                                                    363
C    Create DTCG :                                                        364
C    Call To Local Procedure DTCG                                         364
         ASSIGN 5740 TO BLOCK_DTCG                                        364
         GO TO  5730                                                      364
5740     CONTINUE                                                         364
C    *                                                                    364
C    Geant POSITION : DTCG ===> X=-4.56862 Y=6.71349 Z=DOVETAILZOFFSET <  365
C    ===                                                                  365
         CALL AGSRESET                                                    365
         AG_X = -4.56862                                                  365
         AG_Y = 6.71349                                                   365
         AG_Z = DOVETAILZOFFSET                                           365
         AG_TITLE = 'DTCG'                                                365
         AG_EXNAME = 'POSITION'                                           365
         AG_PARLIST = 'X_Y_Z'                                             365
         CALL AXPOSITION                                                  365
C    *                                                                    365
C    Create DTCH :                                                        366
C    Call To Local Procedure DTCH                                         366
         ASSIGN 5760 TO BLOCK_DTCH                                        366
         GO TO  5750                                                      366
5760     CONTINUE                                                         366
C    *                                                                    366
C    Geant POSITION : DTCH ===> X=-1.68615 Y=2.88554 Z=DOVETAILZOFFSET <  367
C    ===                                                                  367
         CALL AGSRESET                                                    367
         AG_X = -1.68615                                                  367
         AG_Y = 2.88554                                                   367
         AG_Z = DOVETAILZOFFSET                                           367
         AG_TITLE = 'DTCH'                                                367
         AG_EXNAME = 'POSITION'                                           367
         AG_PARLIST = 'X_Y_Z'                                             367
         CALL AXPOSITION                                                  367
C    *                                                                    367
C    Create DTSA :                                                        368
C    Call To Local Procedure DTSA                                         368
         ASSIGN 5780 TO BLOCK_DTSA                                        368
         GO TO  5770                                                      368
5780     CONTINUE                                                         368
C    *                                                                    368
C    Geant POSITION : DTSA ===> X=-0.174 Y=5.25916 Z=DOVETAILZOFFSET ALP  369
C    HAZ=90 <===                                                          369
         CALL AGSRESET                                                    369
         AG_X = -0.174                                                    369
         AG_Y = 5.25916                                                   369
         AG_Z = DOVETAILZOFFSET                                           369
         AG_ALPHAN=AG_ALPHAN+1                                            369
         AG_ALPHAK(AG_ALPHAN)=3                                           369
         AG_ALPHAL(AG_ALPHAN)=90                                          369
         AG_TITLE = 'DTSA'                                                369
         AG_EXNAME = 'POSITION'                                           369
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      369
         CALL AXPOSITION                                                  369
C    *                                                                    369
C    Create DTSB :                                                        370
C    Call To Local Procedure DTSB                                         370
         ASSIGN 5800 TO BLOCK_DTSB                                        370
         GO TO  5790                                                      370
5800     CONTINUE                                                         370
C    *                                                                    370
C    Geant POSITION : DTSB ===> X=-0.852755 Y=7.96401 Z=DOVETAILZOFFSET   371
C    ALPHAZ=169.963 <===                                                  371
         CALL AGSRESET                                                    371
         AG_X = -0.852755                                                 371
         AG_Y = 7.96401                                                   371
         AG_Z = DOVETAILZOFFSET                                           371
         AG_ALPHAN=AG_ALPHAN+1                                            371
         AG_ALPHAK(AG_ALPHAN)=3                                           371
         AG_ALPHAL(AG_ALPHAN)=169.963                                     371
         AG_TITLE = 'DTSB'                                                371
         AG_EXNAME = 'POSITION'                                           371
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      371
         CALL AXPOSITION                                                  371
C    *                                                                    371
C    Create DTSC :                                                        372
C    Call To Local Procedure DTSC                                         372
         ASSIGN 5820 TO BLOCK_DTSC                                        372
         GO TO  5810                                                      372
5820     CONTINUE                                                         372
C    *                                                                    372
C    Geant POSITION : DTSC ===> X=-1.54866 Y=7.94483 Z=DOVETAILZOFFSET A  373
C    LPHAZ=281.554 <===                                                   373
         CALL AGSRESET                                                    373
         AG_X = -1.54866                                                  373
         AG_Y = 7.94483                                                   373
         AG_Z = DOVETAILZOFFSET                                           373
         AG_ALPHAN=AG_ALPHAN+1                                            373
         AG_ALPHAK(AG_ALPHAN)=3                                           373
         AG_ALPHAL(AG_ALPHAN)=281.554                                     373
         AG_TITLE = 'DTSC'                                                373
         AG_EXNAME = 'POSITION'                                           373
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      373
         CALL AXPOSITION                                                  373
C    *                                                                    373
C    Create DTSD :                                                        374
C    Call To Local Procedure DTSD                                         374
         ASSIGN 5840 TO BLOCK_DTSD                                        374
         GO TO  5830                                                      374
5840     CONTINUE                                                         374
C    *                                                                    374
C    Geant POSITION : DTSD ===> X=-2.43389 Y=7.6146 Z=DOVETAILZOFFSET AL  375
C    PHAZ=181.963 <===                                                    375
         CALL AGSRESET                                                    375
         AG_X = -2.43389                                                  375
         AG_Y = 7.6146                                                    375
         AG_Z = DOVETAILZOFFSET                                           375
         AG_ALPHAN=AG_ALPHAN+1                                            375
         AG_ALPHAK(AG_ALPHAN)=3                                           375
         AG_ALPHAL(AG_ALPHAN)=181.963                                     375
         AG_TITLE = 'DTSD'                                                375
         AG_EXNAME = 'POSITION'                                           375
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      375
         CALL AXPOSITION                                                  375
C    *                                                                    375
C    Create DTSE :                                                        376
C    Call To Local Procedure DTSE                                         376
         ASSIGN 5860 TO BLOCK_DTSE                                        376
         GO TO  5850                                                      376
5860     CONTINUE                                                         376
C    *                                                                    376
C    Geant POSITION : DTSE ===> X=-3.16664 Y=7.44923 Z=DOVETAILZOFFSET A  377
C    LPHAZ=293.554 <===                                                   377
         CALL AGSRESET                                                    377
         AG_X = -3.16664                                                  377
         AG_Y = 7.44923                                                   377
         AG_Z = DOVETAILZOFFSET                                           377
         AG_ALPHAN=AG_ALPHAN+1                                            377
         AG_ALPHAK(AG_ALPHAN)=3                                           377
         AG_ALPHAL(AG_ALPHAN)=293.554                                     377
         AG_TITLE = 'DTSE'                                                377
         AG_EXNAME = 'POSITION'                                           377
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      377
         CALL AXPOSITION                                                  377
C    *                                                                    377
C    Create DTSF :                                                        378
C    Call To Local Procedure DTSF                                         378
         ASSIGN 5880 TO BLOCK_DTSF                                        378
         GO TO  5870                                                      378
5880     CONTINUE                                                         378
C    *                                                                    378
C    Geant POSITION : DTSF ===> X=-3.94466 Y=6.94694 Z=DOVETAILZOFFSET A  379
C    LPHAZ=193.963 <===                                                   379
         CALL AGSRESET                                                    379
         AG_X = -3.94466                                                  379
         AG_Y = 6.94694                                                   379
         AG_Z = DOVETAILZOFFSET                                           379
         AG_ALPHAN=AG_ALPHAN+1                                            379
         AG_ALPHAK(AG_ALPHAN)=3                                           379
         AG_ALPHAL(AG_ALPHAN)=193.963                                     379
         AG_TITLE = 'DTSF'                                                379
         AG_EXNAME = 'POSITION'                                           379
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      379
         CALL AXPOSITION                                                  379
C    *                                                                    379
C    Create DTSG :                                                        380
C    Call To Local Procedure DTSG                                         380
         ASSIGN 5900 TO BLOCK_DTSG                                        380
         GO TO  5890                                                      380
5900     CONTINUE                                                         380
C    *                                                                    380
C    Geant POSITION : DTSG ===> X=-3.22202 Y=4.73075 Z=DOVETAILZOFFSET A  381
C    LPHAZ=306 <===                                                       381
         CALL AGSRESET                                                    381
         AG_X = -3.22202                                                  381
         AG_Y = 4.73075                                                   381
         AG_Z = DOVETAILZOFFSET                                           381
         AG_ALPHAN=AG_ALPHAN+1                                            381
         AG_ALPHAK(AG_ALPHAN)=3                                           381
         AG_ALPHAL(AG_ALPHAN)=306                                         381
         AG_TITLE = 'DTSG'                                                381
         AG_EXNAME = 'POSITION'                                           381
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      381
         CALL AXPOSITION                                                  381
C    *                                                                    381
C    Create DTSH :                                                        382
C    Call To Local Procedure DTSH                                         382
         ASSIGN 5920 TO BLOCK_DTSH                                        382
         GO TO  5910                                                      382
5920     CONTINUE                                                         382
C    *                                                                    382
C    Geant POSITION : DTSH ===> X=-0.976145 Y=2.67108 Z=DOVETAILZOFFSET   383
C    ALPHAZ=359.303 <===                                                  383
         CALL AGSRESET                                                    383
         AG_X = -0.976145                                                 383
         AG_Y = 2.67108                                                   383
         AG_Z = DOVETAILZOFFSET                                           383
         AG_ALPHAN=AG_ALPHAN+1                                            383
         AG_ALPHAK(AG_ALPHAN)=3                                           383
         AG_ALPHAL(AG_ALPHAN)=359.303                                     383
         AG_TITLE = 'DTSH'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      383
         CALL AXPOSITION                                                  383
C    *                                                                    383
C    Create DTEA :                                                        384
C    Call To Local Procedure DTEA                                         384
         ASSIGN 5940 TO BLOCK_DTEA                                        384
         GO TO  5930                                                      384
5940     CONTINUE                                                         384
C    *                                                                    384
C    Geant POSITION : DTEA ===> X=-0.25 Y=2.73819 Z=DOVETAILEXTRUSIONZOF  385
C    FSET <===                                                            385
         CALL AGSRESET                                                    385
         AG_X = -0.25                                                     385
         AG_Y = 2.73819                                                   385
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  385
         AG_TITLE = 'DTEA'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_Z'                                             385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Create DTEB :                                                        386
C    Call To Local Procedure DTEB                                         386
         ASSIGN 5960 TO BLOCK_DTEB                                        386
         GO TO  5950                                                      386
5960     CONTINUE                                                         386
C    *                                                                    386
C    Geant POSITION : DTEB ===> X=-0.25 Y=7.78014 Z=DOVETAILEXTRUSIONZOF  387
C    FSET <===                                                            387
         CALL AGSRESET                                                    387
         AG_X = -0.25                                                     387
         AG_Y = 7.78014                                                   387
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  387
         AG_TITLE = 'DTEB'                                                387
         AG_EXNAME = 'POSITION'                                           387
         AG_PARLIST = 'X_Y_Z'                                             387
         CALL AXPOSITION                                                  387
C    *                                                                    387
C    Create DTEC :                                                        388
C    Call To Local Procedure DTEC                                         388
         ASSIGN 5980 TO BLOCK_DTEC                                        388
         GO TO  5970                                                      388
5980     CONTINUE                                                         388
C    *                                                                    388
C    Geant POSITION : DTEC ===> X=-1.482 Y=7.9982 Z=DOVETAILEXTRUSIONZOF  389
C    FSET <===                                                            389
         CALL AGSRESET                                                    389
         AG_X = -1.482                                                    389
         AG_Y = 7.9982                                                    389
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  389
         AG_TITLE = 'DTEC'                                                389
         AG_EXNAME = 'POSITION'                                           389
         AG_PARLIST = 'X_Y_Z'                                             389
         CALL AXPOSITION                                                  389
C    *                                                                    389
C    Create DTED :                                                        390
C    Call To Local Procedure DTED                                         390
         ASSIGN 6000 TO BLOCK_DTED                                        390
         GO TO  5990                                                      390
6000     CONTINUE                                                         390
C    *                                                                    390
C    Geant POSITION : DTED ===> X=-1.76032 Y=7.86181 Z=DOVETAILEXTRUSION  391
C    ZOFFSET <===                                                         391
         CALL AGSRESET                                                    391
         AG_X = -1.76032                                                  391
         AG_Y = 7.86181                                                   391
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  391
         AG_TITLE = 'DTED'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'X_Y_Z'                                             391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Create DTEE :                                                        392
C    Call To Local Procedure DTEE                                         392
         ASSIGN 6020 TO BLOCK_DTEE                                        392
         GO TO  6010                                                      392
6020     CONTINUE                                                         392
C    *                                                                    392
C    Geant POSITION : DTEE ===> X=-3.11253 Y=7.51529 Z=DOVETAILEXTRUSION  393
C    ZOFFSET <===                                                         393
         CALL AGSRESET                                                    393
         AG_X = -3.11253                                                  393
         AG_Y = 7.51529                                                   393
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  393
         AG_TITLE = 'DTEE'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'X_Y_Z'                                             393
         CALL AXPOSITION                                                  393
C    *                                                                    393
C    Create DTEF :                                                        394
C    Call To Local Procedure DTEF                                         394
         ASSIGN 6040 TO BLOCK_DTEF                                        394
         GO TO  6030                                                      394
6040     CONTINUE                                                         394
C    *                                                                    394
C    Geant POSITION : DTEF ===> X=-3.35642 Y=7.32402 Z=DOVETAILEXTRUSION  395
C    ZOFFSET <===                                                         395
         CALL AGSRESET                                                    395
         AG_X = -3.35642                                                  395
         AG_Y = 7.32402                                                   395
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  395
         AG_TITLE = 'DTEF'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'X_Y_Z'                                             395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Create DTEG :                                                        396
C    Call To Local Procedure DTEG                                         396
         ASSIGN 6060 TO BLOCK_DTEG                                        396
         GO TO  6050                                                      396
6060     CONTINUE                                                         396
C    *                                                                    396
C    Geant POSITION : DTEG ===> X=-4.56862 Y=6.71349 Z=DOVETAILEXTRUSION  397
C    ZOFFSET <===                                                         397
         CALL AGSRESET                                                    397
         AG_X = -4.56862                                                  397
         AG_Y = 6.71349                                                   397
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  397
         AG_TITLE = 'DTEG'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'X_Y_Z'                                             397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Create DTEH :                                                        398
C    Call To Local Procedure DTEH                                         398
         ASSIGN 6080 TO BLOCK_DTEH                                        398
         GO TO  6070                                                      398
6080     CONTINUE                                                         398
C    *                                                                    398
C    Geant POSITION : DTEH ===> X=-1.68615 Y=2.88554 Z=DOVETAILEXTRUSION  399
C    ZOFFSET <===                                                         399
         CALL AGSRESET                                                    399
         AG_X = -1.68615                                                  399
         AG_Y = 2.88554                                                   399
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  399
         AG_TITLE = 'DTEH'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'X_Y_Z'                                             399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Create DESA :                                                        400
C    Call To Local Procedure DESA                                         400
         ASSIGN 6100 TO BLOCK_DESA                                        400
         GO TO  6090                                                      400
6100     CONTINUE                                                         400
C    *                                                                    400
C    Geant POSITION : DESA ===> X=-0.162 Y=5.25916 Z=DOVETAILEXTRUSIONZO  401
C    FFSET ALPHAZ=90 <===                                                 401
         CALL AGSRESET                                                    401
         AG_X = -0.162                                                    401
         AG_Y = 5.25916                                                   401
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  401
         AG_ALPHAN=AG_ALPHAN+1                                            401
         AG_ALPHAK(AG_ALPHAN)=3                                           401
         AG_ALPHAL(AG_ALPHAN)=90                                          401
         AG_TITLE = 'DESA'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      401
         CALL AXPOSITION                                                  401
C    *                                                                    401
C    Create DESB :                                                        402
C    Call To Local Procedure DESB                                         402
         ASSIGN 6120 TO BLOCK_DESB                                        402
         GO TO  6110                                                      402
6120     CONTINUE                                                         402
C    *                                                                    402
C    Geant POSITION : DESB ===> X=-0.88552 Y=7.77888 Z=DOVETAILEXTRUSION  403
C    ZOFFSET ALPHAZ=169.963 <===                                          403
         CALL AGSRESET                                                    403
         AG_X = -0.88552                                                  403
         AG_Y = 7.77888                                                   403
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  403
         AG_ALPHAN=AG_ALPHAN+1                                            403
         AG_ALPHAK(AG_ALPHAN)=3                                           403
         AG_ALPHAL(AG_ALPHAN)=169.963                                     403
         AG_TITLE = 'DESB'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Create DESC :                                                        404
C    Call To Local Procedure DESC                                         404
         ASSIGN 6140 TO BLOCK_DESC                                        404
         GO TO  6130                                                      404
6140     CONTINUE                                                         404
C    *                                                                    404
C    Geant POSITION : DESC ===> X=-1.36447 Y=7.98248 Z=DOVETAILEXTRUSION  405
C    ZOFFSET ALPHAZ=281.554 <===                                          405
         CALL AGSRESET                                                    405
         AG_X = -1.36447                                                  405
         AG_Y = 7.98248                                                   405
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  405
         AG_ALPHAN=AG_ALPHAN+1                                            405
         AG_ALPHAK(AG_ALPHAN)=3                                           405
         AG_ALPHAL(AG_ALPHAN)=281.554                                     405
         AG_TITLE = 'DESC'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Create DESD :                                                        406
C    Call To Local Procedure DESD                                         406
         ASSIGN 6160 TO BLOCK_DESD                                        406
         GO TO  6150                                                      406
6160     CONTINUE                                                         406
C    *                                                                    406
C    Geant POSITION : DESD ===> X=-2.42745 Y=7.42671 Z=DOVETAILEXTRUSION  407
C    ZOFFSET ALPHAZ=181.963 <===                                          407
         CALL AGSRESET                                                    407
         AG_X = -2.42745                                                  407
         AG_Y = 7.42671                                                   407
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  407
         AG_ALPHAN=AG_ALPHAN+1                                            407
         AG_ALPHAK(AG_ALPHAN)=3                                           407
         AG_ALPHAL(AG_ALPHAN)=181.963                                     407
         AG_TITLE = 'DESD'                                                407
         AG_EXNAME = 'POSITION'                                           407
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      407
         CALL AXPOSITION                                                  407
C    *                                                                    407
C    Create DESE :                                                        408
C    Call To Local Procedure DESE                                         408
         ASSIGN 6180 TO BLOCK_DESE                                        408
         GO TO  6170                                                      408
6180     CONTINUE                                                         408
C    *                                                                    408
C    Geant POSITION : DESE ===> X=-2.99431 Y=7.52436 Z=DOVETAILEXTRUSION  409
C    ZOFFSET ALPHAZ=293.554 <===                                          409
         CALL AGSRESET                                                    409
         AG_X = -2.99431                                                  409
         AG_Y = 7.52436                                                   409
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  409
         AG_ALPHAN=AG_ALPHAN+1                                            409
         AG_ALPHAK(AG_ALPHAN)=3                                           409
         AG_ALPHAL(AG_ALPHAN)=293.554                                     409
         AG_TITLE = 'DESE'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      409
         CALL AXPOSITION                                                  409
C    *                                                                    409
C    Create DESF :                                                        410
C    Call To Local Procedure DESF                                         410
         ASSIGN 6200 TO BLOCK_DESF                                        410
         GO TO  6190                                                      410
6200     CONTINUE                                                         410
C    *                                                                    410
C    Geant POSITION : DESF ===> X=-3.8993 Y=6.7645 Z=DOVETAILEXTRUSIONZO  411
C    FFSET ALPHAZ=193.963 <===                                            411
         CALL AGSRESET                                                    411
         AG_X = -3.8993                                                   411
         AG_Y = 6.7645                                                    411
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  411
         AG_ALPHAN=AG_ALPHAN+1                                            411
         AG_ALPHAK(AG_ALPHAN)=3                                           411
         AG_ALPHAL(AG_ALPHAN)=193.963                                     411
         AG_TITLE = 'DESF'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      411
         CALL AXPOSITION                                                  411
C    *                                                                    411
C    Create DESG :                                                        412
C    Call To Local Procedure DESG                                         412
         ASSIGN 6220 TO BLOCK_DESG                                        412
         GO TO  6210                                                      412
6220     CONTINUE                                                         412
C    *                                                                    412
C    Geant POSITION : DESG ===> X=-3.06992 Y=4.84126 Z=DOVETAILEXTRUSION  413
C    ZOFFSET ALPHAZ=306 <===                                              413
         CALL AGSRESET                                                    413
         AG_X = -3.06992                                                  413
         AG_Y = 4.84126                                                   413
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  413
         AG_ALPHAN=AG_ALPHAN+1                                            413
         AG_ALPHAK(AG_ALPHAN)=3                                           413
         AG_ALPHAL(AG_ALPHAN)=306                                         413
         AG_TITLE = 'DESG'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      413
         CALL AXPOSITION                                                  413
C    *                                                                    413
C    Create DESH :                                                        414
C    Call To Local Procedure DESH                                         414
         ASSIGN 6240 TO BLOCK_DESH                                        414
         GO TO  6230                                                      414
6240     CONTINUE                                                         414
C    *                                                                    414
C    Geant POSITION : DESH ===> X=-0.964849 Y=2.88281 Z=DOVETAILEXTRUSIO  415
C    NZOFFSET ALPHAZ=357.396 <===                                         415
         CALL AGSRESET                                                    415
         AG_X = -0.964849                                                 415
         AG_Y = 2.88281                                                   415
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  415
         AG_ALPHAN=AG_ALPHAN+1                                            415
         AG_ALPHAK(AG_ALPHAN)=3                                           415
         AG_ALPHAL(AG_ALPHAN)=357.396                                     415
         AG_TITLE = 'DESH'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      415
         CALL AXPOSITION                                                  415
C    *                                                                    415
C    Create PTRA :                                                        416
C    Call To Local Procedure PTRA                                         416
         ASSIGN 6260 TO BLOCK_PTRA                                        416
         GO TO  6250                                                      416
6260     CONTINUE                                                         416
C    *                                                                    416
C    Geant POSITION : PTRA ===> X=-0.844273 Y=7.31126 Z=0.85+DOVETAILSHA  417
C    PEOFFSET ALPHAZ=-174 <===                                            417
         CALL AGSRESET                                                    417
         AG_X = -0.844273                                                 417
         AG_Y = 7.31126                                                   417
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  417
         AG_ALPHAN=AG_ALPHAN+1                                            417
         AG_ALPHAK(AG_ALPHAN)=3                                           417
         AG_ALPHAL(AG_ALPHAN)=-174                                        417
         AG_TITLE = 'PTRA'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      417
         CALL AXPOSITION                                                  417
C    *                                                                    417
C    Create PTRB :                                                        418
C    Call To Local Procedure PTRB                                         418
         ASSIGN 6280 TO BLOCK_PTRB                                        418
         GO TO  6270                                                      418
6280     CONTINUE                                                         418
C    *                                                                    418
C    Geant POSITION : PTRB ===> X=-2.2742 Y=6.99926 Z=0.85+DOVETAILSHAPE  419
C    OFFSET ALPHAZ=-162 <===                                              419
         CALL AGSRESET                                                    419
         AG_X = -2.2742                                                   419
         AG_Y = 6.99926                                                   419
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  419
         AG_ALPHAN=AG_ALPHAN+1                                            419
         AG_ALPHAK(AG_ALPHAN)=3                                           419
         AG_ALPHAL(AG_ALPHAN)=-162                                        419
         AG_TITLE = 'PTRB'                                                419
         AG_EXNAME = 'POSITION'                                           419
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      419
         CALL AXPOSITION                                                  419
C    *                                                                    419
C    Create PTRC :                                                        420
C    Call To Local Procedure PTRC                                         420
         ASSIGN 6300 TO BLOCK_PTRC                                        420
         GO TO  6290                                                      420
6300     CONTINUE                                                         420
C    *                                                                    420
C    Geant POSITION : PTRC ===> X=-3.61442 Y=6.41119 Z=0.85+DOVETAILSHAP  421
C    EOFFSET ALPHAZ=-150 <===                                             421
         CALL AGSRESET                                                    421
         AG_X = -3.61442                                                  421
         AG_Y = 6.41119                                                   421
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  421
         AG_ALPHAN=AG_ALPHAN+1                                            421
         AG_ALPHAK(AG_ALPHAN)=3                                           421
         AG_ALPHAL(AG_ALPHAN)=-150                                        421
         AG_TITLE = 'PTRC'                                                421
         AG_EXNAME = 'POSITION'                                           421
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      421
         CALL AXPOSITION                                                  421
C    *                                                                    421
C    Create PTRE :                                                        422
C    Call To Local Procedure PTRE                                         422
         ASSIGN 6320 TO BLOCK_PTRE                                        422
         GO TO  6310                                                      422
6320     CONTINUE                                                         422
C    *                                                                    422
C    Geant POSITION : PTRE ===> X=-0.45 Y=4.93658 Z=0.85+DOVETAILSHAPEOF  423
C    FSET ALPHAZ=90 <===                                                  423
         CALL AGSRESET                                                    423
         AG_X = -0.45                                                     423
         AG_Y = 4.93658                                                   423
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  423
         AG_ALPHAN=AG_ALPHAN+1                                            423
         AG_ALPHAK(AG_ALPHAN)=3                                           423
         AG_ALPHAL(AG_ALPHAN)=90                                          423
         AG_TITLE = 'PTRE'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Create PTRF :                                                        424
C    Call To Local Procedure PTRF                                         424
         ASSIGN 6340 TO BLOCK_PTRF                                        424
         GO TO  6330                                                      424
6340     CONTINUE                                                         424
C    *                                                                    424
C    Geant POSITION : PTRF ===> X=-2.64683 Y=4.40863 Z=0.85+DOVETAILSHAP  425
C    EOFFSET ALPHAZ=126 <===                                              425
         CALL AGSRESET                                                    425
         AG_X = -2.64683                                                  425
         AG_Y = 4.40863                                                   425
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  425
         AG_ALPHAN=AG_ALPHAN+1                                            425
         AG_ALPHAK(AG_ALPHAN)=3                                           425
         AG_ALPHAL(AG_ALPHAN)=126                                         425
         AG_TITLE = 'PTRF'                                                425
         AG_EXNAME = 'POSITION'                                           425
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      425
         CALL AXPOSITION                                                  425
C    *                                                                    425
C    Create PTRG :                                                        426
C    Call To Local Procedure PTRG                                         426
         ASSIGN 6360 TO BLOCK_PTRG                                        426
         GO TO  6350                                                      426
6360     CONTINUE                                                         426
C    *                                                                    426
C    Geant POSITION : PTRG ===> X=-1.06041 Y=3.14864 Z=0.25+DOVETAILSHAP  427
C    EOFFSET ALPHAZ=177.396 <===                                          427
         CALL AGSRESET                                                    427
         AG_X = -1.06041                                                  427
         AG_Y = 3.14864                                                   427
         AG_Z = 0.25+DOVETAILSHAPEOFFSET                                  427
         AG_ALPHAN=AG_ALPHAN+1                                            427
         AG_ALPHAK(AG_ALPHAN)=3                                           427
         AG_ALPHAL(AG_ALPHAN)=177.396                                     427
         AG_TITLE = 'PTRG'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Create PTMA :                                                        428
C    Call To Local Procedure PTMA                                         428
         ASSIGN 6380 TO BLOCK_PTMA                                        428
         GO TO  6370                                                      428
6380     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : PTMA ===> X=-1.09427 Y=7.28499 Z=0.2+DOVETAILSHAPE  429
C    OFFSET ALPHAZ=-174 <===                                              429
         CALL AGSRESET                                                    429
         AG_X = -1.09427                                                  429
         AG_Y = 7.28499                                                   429
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   429
         AG_ALPHAN=AG_ALPHAN+1                                            429
         AG_ALPHAK(AG_ALPHAN)=3                                           429
         AG_ALPHAL(AG_ALPHAN)=-174                                        429
         AG_TITLE = 'PTMA'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Create PTMB :                                                        430
C    Call To Local Procedure PTMB                                         430
         ASSIGN 6400 TO BLOCK_PTMB                                        430
         GO TO  6390                                                      430
6400     CONTINUE                                                         430
C    *                                                                    430
C    Geant POSITION : PTMB ===> X=-2.2742 Y=6.99926 Z=0.2+DOVETAILSHAPEO  431
C    FFSET ALPHAZ=-162 <===                                               431
         CALL AGSRESET                                                    431
         AG_X = -2.2742                                                   431
         AG_Y = 6.99926                                                   431
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   431
         AG_ALPHAN=AG_ALPHAN+1                                            431
         AG_ALPHAK(AG_ALPHAN)=3                                           431
         AG_ALPHAL(AG_ALPHAN)=-162                                        431
         AG_TITLE = 'PTMB'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      431
         CALL AXPOSITION                                                  431
C    *                                                                    431
C    Create PTMC :                                                        432
C    Call To Local Procedure PTMC                                         432
         ASSIGN 6420 TO BLOCK_PTMC                                        432
         GO TO  6410                                                      432
6420     CONTINUE                                                         432
C    *                                                                    432
C    Geant POSITION : PTMC ===> X=-3.39672 Y=6.53688 Z=0.2+DOVETAILSHAPE  433
C    OFFSET ALPHAZ=-150 <===                                              433
         CALL AGSRESET                                                    433
         AG_X = -3.39672                                                  433
         AG_Y = 6.53688                                                   433
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   433
         AG_ALPHAN=AG_ALPHAN+1                                            433
         AG_ALPHAK(AG_ALPHAN)=3                                           433
         AG_ALPHAL(AG_ALPHAN)=-150                                        433
         AG_TITLE = 'PTMC'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Create PTME :                                                        434
C    Call To Local Procedure PTME                                         434
         ASSIGN 6440 TO BLOCK_PTME                                        434
         GO TO  6430                                                      434
6440     CONTINUE                                                         434
C    *                                                                    434
C    Geant POSITION : PTME ===> X=-0.7 Y=4.92913 Z=0.2+DOVETAILSHAPEOFFS  435
C    ET ALPHAZ=90 <===                                                    435
         CALL AGSRESET                                                    435
         AG_X = -0.7                                                      435
         AG_Y = 4.92913                                                   435
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   435
         AG_ALPHAN=AG_ALPHAN+1                                            435
         AG_ALPHAK(AG_ALPHAN)=3                                           435
         AG_ALPHAL(AG_ALPHAN)=90                                          435
         AG_TITLE = 'PTME'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Create PTMF :                                                        436
C    Call To Local Procedure PTMF                                         436
         ASSIGN 6460 TO BLOCK_PTMF                                        436
         GO TO  6450                                                      436
6460     CONTINUE                                                         436
C    *                                                                    436
C    Geant POSITION : PTMF ===> X=-2.37819 Y=4.46421 Z=0.2+DOVETAILSHAPE  437
C    OFFSET ALPHAZ=126 <===                                               437
         CALL AGSRESET                                                    437
         AG_X = -2.37819                                                  437
         AG_Y = 4.46421                                                   437
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   437
         AG_ALPHAN=AG_ALPHAN+1                                            437
         AG_ALPHAK(AG_ALPHAN)=3                                           437
         AG_ALPHAL(AG_ALPHAN)=126                                         437
         AG_TITLE = 'PTMF'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      437
         CALL AXPOSITION                                                  437
C    *                                                                    437
C    Create PTLA :                                                        438
C    Call To Local Procedure PTLA                                         438
         ASSIGN 6480 TO BLOCK_PTLA                                        438
         GO TO  6470                                                      438
6480     CONTINUE                                                         438
C    *                                                                    438
C    Geant POSITION : PTLA ===> X=-0.769273 Y=7.31915 Z=-0.85+DOVETAILSH  439
C    APEOFFSET ALPHAZ=-174 <===                                           439
         CALL AGSRESET                                                    439
         AG_X = -0.769273                                                 439
         AG_Y = 7.31915                                                   439
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 439
         AG_ALPHAN=AG_ALPHAN+1                                            439
         AG_ALPHAK(AG_ALPHAN)=3                                           439
         AG_ALPHAL(AG_ALPHAN)=-174                                        439
         AG_TITLE = 'PTLA'                                                439
         AG_EXNAME = 'POSITION'                                           439
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      439
         CALL AXPOSITION                                                  439
C    *                                                                    439
C    Create PTLB :                                                        440
C    Call To Local Procedure PTLB                                         440
         ASSIGN 6500 TO BLOCK_PTLB                                        440
         GO TO  6490                                                      440
6500     CONTINUE                                                         440
C    *                                                                    440
C    Geant POSITION : PTLB ===> X=-2.2742 Y=6.99926 Z=-0.85+DOVETAILSHAP  441
C    EOFFSET ALPHAZ=-162 <===                                             441
         CALL AGSRESET                                                    441
         AG_X = -2.2742                                                   441
         AG_Y = 6.99926                                                   441
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 441
         AG_ALPHAN=AG_ALPHAN+1                                            441
         AG_ALPHAK(AG_ALPHAN)=3                                           441
         AG_ALPHAL(AG_ALPHAN)=-162                                        441
         AG_TITLE = 'PTLB'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Create PTLC :                                                        442
C    Call To Local Procedure PTLC                                         442
         ASSIGN 6520 TO BLOCK_PTLC                                        442
         GO TO  6510                                                      442
6520     CONTINUE                                                         442
C    *                                                                    442
C    Geant POSITION : PTLC ===> X=-3.67973 Y=6.37348 Z=-0.85+DOVETAILSHA  443
C    PEOFFSET ALPHAZ=-150 <===                                            443
         CALL AGSRESET                                                    443
         AG_X = -3.67973                                                  443
         AG_Y = 6.37348                                                   443
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 443
         AG_ALPHAN=AG_ALPHAN+1                                            443
         AG_ALPHAK(AG_ALPHAN)=3                                           443
         AG_ALPHAL(AG_ALPHAN)=-150                                        443
         AG_TITLE = 'PTLC'                                                443
         AG_EXNAME = 'POSITION'                                           443
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      443
         CALL AXPOSITION                                                  443
C    *                                                                    443
C    Create PTLD :                                                        444
C    Call To Local Procedure PTLD                                         444
         ASSIGN 6540 TO BLOCK_PTLD                                        444
         GO TO  6530                                                      444
6540     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : PTLD ===> X=-0.322263 Y=3.06613 Z=-0.85+DOVETAILSH  445
C    APEOFFSET ALPHAZ=-174 <===                                           445
         CALL AGSRESET                                                    445
         AG_X = -0.322263                                                 445
         AG_Y = 3.06613                                                   445
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 445
         AG_ALPHAN=AG_ALPHAN+1                                            445
         AG_ALPHAK(AG_ALPHAN)=3                                           445
         AG_ALPHAL(AG_ALPHAN)=-174                                        445
         AG_TITLE = 'PTLD'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      445
         CALL AXPOSITION                                                  445
C    *                                                                    445
C    Create PTLE :                                                        446
C    Call To Local Procedure PTLE                                         446
         ASSIGN 6560 TO BLOCK_PTLE                                        446
         GO TO  6550                                                      446
6560     CONTINUE                                                         446
C    *                                                                    446
C    Geant POSITION : PTLE ===> X=-0.952705 Y=2.93212 Z=-0.85+DOVETAILSH  447
C    APEOFFSET ALPHAZ=-162 <===                                           447
         CALL AGSRESET                                                    447
         AG_X = -0.952705                                                 447
         AG_Y = 2.93212                                                   447
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 447
         AG_ALPHAN=AG_ALPHAN+1                                            447
         AG_ALPHAK(AG_ALPHAN)=3                                           447
         AG_ALPHAL(AG_ALPHAN)=-162                                        447
         AG_TITLE = 'PTLE'                                                447
         AG_EXNAME = 'POSITION'                                           447
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      447
         CALL AXPOSITION                                                  447
C    *                                                                    447
C    Create PTLF :                                                        448
C    Call To Local Procedure PTLF                                         448
         ASSIGN 6580 TO BLOCK_PTLF                                        448
         GO TO  6570                                                      448
6580     CONTINUE                                                         448
C    *                                                                    448
C    Geant POSITION : PTLF ===> X=-1.54151 Y=2.66997 Z=-0.85+DOVETAILSHA  449
C    PEOFFSET ALPHAZ=-150 <===                                            449
         CALL AGSRESET                                                    449
         AG_X = -1.54151                                                  449
         AG_Y = 2.66997                                                   449
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 449
         AG_ALPHAN=AG_ALPHAN+1                                            449
         AG_ALPHAK(AG_ALPHAN)=3                                           449
         AG_ALPHAL(AG_ALPHAN)=-150                                        449
         AG_TITLE = 'PTLF'                                                449
         AG_EXNAME = 'POSITION'                                           449
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      449
         CALL AXPOSITION                                                  449
C    *                                                                    449
C    Create PTLG :                                                        450
C    Call To Local Procedure PTLG                                         450
         ASSIGN 6600 TO BLOCK_PTLG                                        450
         GO TO  6590                                                      450
6600     CONTINUE                                                         450
C    *                                                                    450
C    Geant POSITION : PTLG ===> X=-0.375 Y=5.31059 Z=-0.85+DOVETAILSHAPE  451
C    OFFSET ALPHAZ=-174 <===                                              451
         CALL AGSRESET                                                    451
         AG_X = -0.375                                                    451
         AG_Y = 5.31059                                                   451
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 451
         AG_ALPHAN=AG_ALPHAN+1                                            451
         AG_ALPHAK(AG_ALPHAN)=3                                           451
         AG_ALPHAL(AG_ALPHAN)=-174                                        451
         AG_TITLE = 'PTLG'                                                451
         AG_EXNAME = 'POSITION'                                           451
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      451
         CALL AXPOSITION                                                  451
C    *                                                                    451
C    Create PTLH :                                                        452
C    Call To Local Procedure PTLH                                         452
         ASSIGN 6620 TO BLOCK_PTLH                                        452
         GO TO  6610                                                      452
6620     CONTINUE                                                         452
C    *                                                                    452
C    Geant POSITION : PTLH ===> X=-2.8181 Y=4.51677 Z=-0.85+DOVETAILSHAP  453
C    EOFFSET ALPHAZ=-150 <===                                             453
         CALL AGSRESET                                                    453
         AG_X = -2.8181                                                   453
         AG_Y = 4.51677                                                   453
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 453
         AG_ALPHAN=AG_ALPHAN+1                                            453
         AG_ALPHAK(AG_ALPHAN)=3                                           453
         AG_ALPHAL(AG_ALPHAN)=-150                                        453
         AG_TITLE = 'PTLH'                                                453
         AG_EXNAME = 'POSITION'                                           453
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      453
         CALL AXPOSITION                                                  453
C    *                                                                    453
C    Create PTLI :                                                        454
C    Call To Local Procedure PTLI                                         454
         ASSIGN 6640 TO BLOCK_PTLI                                        454
         GO TO  6630                                                      454
6640     CONTINUE                                                         454
C    *                                                                    454
C    Geant POSITION : PTLI ===> X=-0.375 Y=5.11059 Z=-0.6+DOVETAILSHAPEO  455
C    FFSET ALPHAZ=-174 <===                                               455
         CALL AGSRESET                                                    455
         AG_X = -0.375                                                    455
         AG_Y = 5.11059                                                   455
         AG_Z = -0.6+DOVETAILSHAPEOFFSET                                  455
         AG_ALPHAN=AG_ALPHAN+1                                            455
         AG_ALPHAK(AG_ALPHAN)=3                                           455
         AG_ALPHAL(AG_ALPHAN)=-174                                        455
         AG_TITLE = 'PTLI'                                                455
         AG_EXNAME = 'POSITION'                                           455
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      455
         CALL AXPOSITION                                                  455
C    *                                                                    455
C    Create PTLJ :                                                        456
C    Call To Local Procedure PTLJ                                         456
         ASSIGN 6660 TO BLOCK_PTLJ                                        456
         GO TO  6650                                                      456
6660     CONTINUE                                                         456
C    *                                                                    456
C    Geant POSITION : PTLJ ===> X=-2.70055 Y=4.35497 Z=-0.6+DOVETAILSHAP  457
C    EOFFSET ALPHAZ=-150 <===                                             457
         CALL AGSRESET                                                    457
         AG_X = -2.70055                                                  457
         AG_Y = 4.35497                                                   457
         AG_Z = -0.6+DOVETAILSHAPEOFFSET                                  457
         AG_ALPHAN=AG_ALPHAN+1                                            457
         AG_ALPHAK(AG_ALPHAN)=3                                           457
         AG_ALPHAL(AG_ALPHAN)=-150                                        457
         AG_TITLE = 'PTLJ'                                                457
         AG_EXNAME = 'POSITION'                                           457
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      457
         CALL AXPOSITION                                                  457
C    *                                                                    457
5161  CALL AGSPOPB                                                        458
      GO TO BLOCK_PXSE, (5160 ,5170)                                      458
C End of Local Procedure                                                  458
C ----------------------------------------------------------------------  459
C ------------ PXLA                                                       459
C Begin Local Procedure PXLA                                              460
C PXLA PIXEL SECTOR WITH LADDERS                                          460
5270  CONTINUE                                                            460
         AG_TITLE='PXLA'                                                  460
         AG_MODULE=MODULE                                                 460
         AG_MARK='V00'                                                    460
         CALL AGSPUSH                                                     460
         AG_MODULE=MODULE                                                 460
C    *                                                                    460
C    Geant MATERIAL : AIR ===> inherited arguments used <===              461
         CALL AGSRESET                                                    461
         AG_TITLE = 'AIR'                                                 461
         AG_EXNAME = 'MATERIAL'                                           461
         AG_PARLIST = 'NONE'                                              461
         CALL AXMATERIAL                                                  461
C    *                                                                    461
C    Geant ATTRIBUTE : PXLA ===> SEEN=0 COLO=1 <===                       461
         CALL AGSRESET                                                    462
         AG_SEEN = 0                                                      462
         AG_COLO = 1                                                      462
         AG_TITLE = 'PXLA'                                                462
         AG_EXNAME = 'ATTRIBUTE'                                          462
         AG_PARLIST = 'SEEN_COLO'                                         462
         CALL AXATTRIBUTE                                                 462
C    *                                                                    462
C    Geant SHAPE : TUBS ===> RMIN=MINRADSEC RMAX=MAXRADSEC, PHI1=MINPHIS  462
C    EC PHI2=MAXPHISEC DZ=HALFMAXZ <===                                   462
         CALL AGSRESET                                                    464
         AG_RMIN = MINRADSEC                                              464
         AG_RMAX = MAXRADSEC                                              464
         AG_PHI1 = MINPHISEC                                              464
         AG_PHI2 = MAXPHISEC                                              464
         AG_DZ = HALFMAXZ                                                 464
         AG_TITLE = 'TUBS'                                                464
         AG_EXNAME = 'SHAPE'                                              464
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            464
         CALL AXSHAPE                                                     464
C    *                                                                    464
C    Create PXCA :                                                        465
C    Call To Local Procedure PXCA                                         465
         ASSIGN 6670 TO BLOCK_PXCA                                        465
         GO TO  5290                                                      465
6670     CONTINUE                                                         465
C    *                                                                    465
C    Geant POSITION : PXCA ===> X=-0.25 Y=2.73819 Z=SECTORZOFFSET <===    466
         CALL AGSRESET                                                    466
         AG_X = -0.25                                                     466
         AG_Y = 2.73819                                                   466
         AG_Z = SECTORZOFFSET                                             466
         AG_TITLE = 'PXCA'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'X_Y_Z'                                             466
         CALL AXPOSITION                                                  466
C    *                                                                    466
C    Create PXCC :                                                        467
C    Call To Local Procedure PXCC                                         467
         ASSIGN 6680 TO BLOCK_PXCC                                        467
         GO TO  5310                                                      467
6680     CONTINUE                                                         467
C    *                                                                    467
C    Geant POSITION : PXCC ===> X=-0.25 Y=7.78014 Z=SECTORZOFFSET <===    468
         CALL AGSRESET                                                    468
         AG_X = -0.25                                                     468
         AG_Y = 7.78014                                                   468
         AG_Z = SECTORZOFFSET                                             468
         AG_TITLE = 'PXCC'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'X_Y_Z'                                             468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Create PXCD :                                                        469
C    Call To Local Procedure PXCD                                         469
         ASSIGN 6690 TO BLOCK_PXCD                                        469
         GO TO  5330                                                      469
6690     CONTINUE                                                         469
C    *                                                                    469
C    Geant POSITION : PXCD ===> X=-1.482 Y=7.9982 Z=SECTORZOFFSET <===    470
         CALL AGSRESET                                                    470
         AG_X = -1.482                                                    470
         AG_Y = 7.9982                                                    470
         AG_Z = SECTORZOFFSET                                             470
         AG_TITLE = 'PXCD'                                                470
         AG_EXNAME = 'POSITION'                                           470
         AG_PARLIST = 'X_Y_Z'                                             470
         CALL AXPOSITION                                                  470
C    *                                                                    470
C    Create PXCE :                                                        471
C    Call To Local Procedure PXCE                                         471
         ASSIGN 6700 TO BLOCK_PXCE                                        471
         GO TO  5350                                                      471
6700     CONTINUE                                                         471
C    *                                                                    471
C    Geant POSITION : PXCE ===> X=-1.76032 Y=7.86181 Z=SECTORZOFFSET <==  472
C    =                                                                    472
         CALL AGSRESET                                                    472
         AG_X = -1.76032                                                  472
         AG_Y = 7.86181                                                   472
         AG_Z = SECTORZOFFSET                                             472
         AG_TITLE = 'PXCE'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'X_Y_Z'                                             472
         CALL AXPOSITION                                                  472
C    *                                                                    472
C    Create PXCF :                                                        473
C    Call To Local Procedure PXCF                                         473
         ASSIGN 6710 TO BLOCK_PXCF                                        473
         GO TO  5370                                                      473
6710     CONTINUE                                                         473
C    *                                                                    473
C    Geant POSITION : PXCF ===> X=-3.11253 Y=7.51529 Z=SECTORZOFFSET <==  474
C    =                                                                    474
         CALL AGSRESET                                                    474
         AG_X = -3.11253                                                  474
         AG_Y = 7.51529                                                   474
         AG_Z = SECTORZOFFSET                                             474
         AG_TITLE = 'PXCF'                                                474
         AG_EXNAME = 'POSITION'                                           474
         AG_PARLIST = 'X_Y_Z'                                             474
         CALL AXPOSITION                                                  474
C    *                                                                    474
C    Create PXCG :                                                        475
C    Call To Local Procedure PXCG                                         475
         ASSIGN 6720 TO BLOCK_PXCG                                        475
         GO TO  5390                                                      475
6720     CONTINUE                                                         475
C    *                                                                    475
C    Geant POSITION : PXCG ===> X=-3.35642 Y=7.32402 Z=SECTORZOFFSET <==  476
C    =                                                                    476
         CALL AGSRESET                                                    476
         AG_X = -3.35642                                                  476
         AG_Y = 7.32402                                                   476
         AG_Z = SECTORZOFFSET                                             476
         AG_TITLE = 'PXCG'                                                476
         AG_EXNAME = 'POSITION'                                           476
         AG_PARLIST = 'X_Y_Z'                                             476
         CALL AXPOSITION                                                  476
C    *                                                                    476
C    Create PXCH :                                                        477
C    Call To Local Procedure PXCH                                         477
         ASSIGN 6730 TO BLOCK_PXCH                                        477
         GO TO  5410                                                      477
6730     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : PXCH ===> X=-4.56862 Y=6.71349 Z=SECTORZOFFSET <==  478
C    =                                                                    478
         CALL AGSRESET                                                    478
         AG_X = -4.56862                                                  478
         AG_Y = 6.71349                                                   478
         AG_Z = SECTORZOFFSET                                             478
         AG_TITLE = 'PXCH'                                                478
         AG_EXNAME = 'POSITION'                                           478
         AG_PARLIST = 'X_Y_Z'                                             478
         CALL AXPOSITION                                                  478
C    *                                                                    478
C    Create PXCB :                                                        479
C    Call To Local Procedure PXCB                                         479
         ASSIGN 6740 TO BLOCK_PXCB                                        479
         GO TO  5430                                                      479
6740     CONTINUE                                                         479
C    *                                                                    479
C    Geant POSITION : PXCB ===> X=-1.68615 Y=2.88554 Z=SECTORZOFFSET <==  480
C    =                                                                    480
         CALL AGSRESET                                                    480
         AG_X = -1.68615                                                  480
         AG_Y = 2.88554                                                   480
         AG_Z = SECTORZOFFSET                                             480
         AG_TITLE = 'PXCB'                                                480
         AG_EXNAME = 'POSITION'                                           480
         AG_PARLIST = 'X_Y_Z'                                             480
         CALL AXPOSITION                                                  480
C    *                                                                    480
C    Create PXRB :                                                        481
C    Call To Local Procedure PXRB                                         481
         ASSIGN 6750 TO BLOCK_PXRB                                        481
         GO TO  5450                                                      481
6750     CONTINUE                                                         481
C    *                                                                    481
C    Geant POSITION : PXRB ===> X=-0.112 Y=5.25916 Z=SECTORZOFFSET ALPHA  482
C    Z=90 <===                                                            482
         CALL AGSRESET                                                    482
         AG_X = -0.112                                                    482
         AG_Y = 5.25916                                                   482
         AG_Z = SECTORZOFFSET                                             482
         AG_ALPHAN=AG_ALPHAN+1                                            482
         AG_ALPHAK(AG_ALPHAN)=3                                           482
         AG_ALPHAL(AG_ALPHAN)=90                                          482
         AG_TITLE = 'PXRB'                                                482
         AG_EXNAME = 'POSITION'                                           482
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      482
         CALL AXPOSITION                                                  482
C    *                                                                    482
C    Create PXTR :                                                        483
C    Call To Local Procedure PXTR                                         483
         ASSIGN 6760 TO BLOCK_PXTR                                        483
         GO TO  5470                                                      483
6760     CONTINUE                                                         483
C    *                                                                    483
C    Geant POSITION : PXTR ===> X=-0.841949 Y=8.02506 Z=SECTORZOFFSET AL  484
C    PHAZ=169.963 <===                                                    484
         CALL AGSRESET                                                    484
         AG_X = -0.841949                                                 484
         AG_Y = 8.02506                                                   484
         AG_Z = SECTORZOFFSET                                             484
         AG_ALPHAN=AG_ALPHAN+1                                            484
         AG_ALPHAK(AG_ALPHAN)=3                                           484
         AG_ALPHAL(AG_ALPHAN)=169.963                                     484
         AG_TITLE = 'PXTR'                                                484
         AG_EXNAME = 'POSITION'                                           484
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      484
         CALL AXPOSITION                                                  484
C    *                                                                    484
C    Create PXTM :                                                        485
C    Call To Local Procedure PXTM                                         485
         ASSIGN 6770 TO BLOCK_PXTM                                        485
         GO TO  5490                                                      485
6770     CONTINUE                                                         485
C    *                                                                    485
C    Geant POSITION : PXTM ===> X=-2.43602 Y=7.67656 Z=SECTORZOFFSET ALP  486
C    HAZ=181.963 <===                                                     486
         CALL AGSRESET                                                    486
         AG_X = -2.43602                                                  486
         AG_Y = 7.67656                                                   486
         AG_Z = SECTORZOFFSET                                             486
         AG_ALPHAN=AG_ALPHAN+1                                            486
         AG_ALPHAK(AG_ALPHAN)=3                                           486
         AG_ALPHAL(AG_ALPHAN)=181.963                                     486
         AG_TITLE = 'PXTM'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      486
         CALL AXPOSITION                                                  486
C    *                                                                    486
C    Create PXTL :                                                        487
C    Call To Local Procedure PXTL                                         487
         ASSIGN 6780 TO BLOCK_PXTL                                        487
         GO TO  5510                                                      487
6780     CONTINUE                                                         487
C    *                                                                    487
C    Geant POSITION : PXTL ===> X=-3.95962 Y=7.00711 Z=SECTORZOFFSET ALP  488
C    HAZ=193.963 <===                                                     488
         CALL AGSRESET                                                    488
         AG_X = -3.95962                                                  488
         AG_Y = 7.00711                                                   488
         AG_Z = SECTORZOFFSET                                             488
         AG_ALPHAN=AG_ALPHAN+1                                            488
         AG_ALPHAK(AG_ALPHAN)=3                                           488
         AG_ALPHAL(AG_ALPHAN)=193.963                                     488
         AG_TITLE = 'PXTL'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      488
         CALL AXPOSITION                                                  488
C    *                                                                    488
C    Create PXTJ :                                                        489
C    Call To Local Procedure PXTJ                                         489
         ASSIGN 6790 TO BLOCK_PXTJ                                        489
         GO TO  5530                                                      489
6790     CONTINUE                                                         489
C    *                                                                    489
C    Geant POSITION : PXTJ ===> X=-3.22348 Y=7.42445 Z=SECTORZOFFSET ALP  490
C    HAZ=293.554 <===                                                     490
         CALL AGSRESET                                                    490
         AG_X = -3.22348                                                  490
         AG_Y = 7.42445                                                   490
         AG_Z = SECTORZOFFSET                                             490
         AG_ALPHAN=AG_ALPHAN+1                                            490
         AG_ALPHAK(AG_ALPHAN)=3                                           490
         AG_ALPHAL(AG_ALPHAN)=293.554                                     490
         AG_TITLE = 'PXTJ'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      490
         CALL AXPOSITION                                                  490
C    *                                                                    490
C    Create PXTI :                                                        491
C    Call To Local Procedure PXTI                                         491
         ASSIGN 6800 TO BLOCK_PXTI                                        491
         GO TO  5550                                                      491
6800     CONTINUE                                                         491
C    *                                                                    491
C    Geant POSITION : PXTI ===> X=-1.60941 Y=7.93241 Z=SECTORZOFFSET ALP  492
C    HAZ=281.554 <===                                                     492
         CALL AGSRESET                                                    492
         AG_X = -1.60941                                                  492
         AG_Y = 7.93241                                                   492
         AG_Z = SECTORZOFFSET                                             492
         AG_ALPHAN=AG_ALPHAN+1                                            492
         AG_ALPHAK(AG_ALPHAN)=3                                           492
         AG_ALPHAL(AG_ALPHAN)=281.554                                     492
         AG_TITLE = 'PXTI'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      492
         CALL AXPOSITION                                                  492
C    *                                                                    492
C    Create PXLB :                                                        493
C    Call To Local Procedure PXLB                                         493
         ASSIGN 6810 TO BLOCK_PXLB                                        493
         GO TO  5570                                                      493
6810     CONTINUE                                                         493
C    *                                                                    493
C    Geant POSITION : PXLB ===> X=-3.27218 Y=4.69431 Z=SECTORZOFFSET ALP  494
C    HAZ=306 <===                                                         494
         CALL AGSRESET                                                    494
         AG_X = -3.27218                                                  494
         AG_Y = 4.69431                                                   494
         AG_Z = SECTORZOFFSET                                             494
         AG_ALPHAN=AG_ALPHAN+1                                            494
         AG_ALPHAK(AG_ALPHAN)=3                                           494
         AG_ALPHAL(AG_ALPHAN)=306                                         494
         AG_TITLE = 'PXLB'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Create PXIB :                                                        495
C    Call To Local Procedure PXIB                                         495
         ASSIGN 6820 TO BLOCK_PXIB                                        495
         GO TO  5590                                                      495
6820     CONTINUE                                                         495
C    *                                                                    495
C    Geant POSITION : PXIB ===> X=-0.976207 Y=2.63307 Z=SECTORZOFFSET AL  496
C    PHAZ=357.396 <===                                                    496
         CALL AGSRESET                                                    496
         AG_X = -0.976207                                                 496
         AG_Y = 2.63307                                                   496
         AG_Z = SECTORZOFFSET                                             496
         AG_ALPHAN=AG_ALPHAN+1                                            496
         AG_ALPHAK(AG_ALPHAN)=3                                           496
         AG_ALPHAL(AG_ALPHAN)=357.396                                     496
         AG_TITLE = 'PXIB'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Create DTCA :                                                        497
C    Call To Local Procedure DTCA                                         497
         ASSIGN 6830 TO BLOCK_DTCA                                        497
         GO TO  5610                                                      497
6830     CONTINUE                                                         497
C    *                                                                    497
C    Geant POSITION : DTCA ===> X=-0.25 Y=2.73819 Z=DOVETAILZOFFSET <===  498
         CALL AGSRESET                                                    498
         AG_X = -0.25                                                     498
         AG_Y = 2.73819                                                   498
         AG_Z = DOVETAILZOFFSET                                           498
         AG_TITLE = 'DTCA'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'X_Y_Z'                                             498
         CALL AXPOSITION                                                  498
C    *                                                                    498
C    Create DTCB :                                                        499
C    Call To Local Procedure DTCB                                         499
         ASSIGN 6840 TO BLOCK_DTCB                                        499
         GO TO  5630                                                      499
6840     CONTINUE                                                         499
C    *                                                                    499
C    Geant POSITION : DTCB ===> X=-0.25 Y=7.78014 Z=DOVETAILZOFFSET <===  500
         CALL AGSRESET                                                    500
         AG_X = -0.25                                                     500
         AG_Y = 7.78014                                                   500
         AG_Z = DOVETAILZOFFSET                                           500
         AG_TITLE = 'DTCB'                                                500
         AG_EXNAME = 'POSITION'                                           500
         AG_PARLIST = 'X_Y_Z'                                             500
         CALL AXPOSITION                                                  500
C    *                                                                    500
C    Create DTCC :                                                        501
C    Call To Local Procedure DTCC                                         501
         ASSIGN 6850 TO BLOCK_DTCC                                        501
         GO TO  5650                                                      501
6850     CONTINUE                                                         501
C    *                                                                    501
C    Geant POSITION : DTCC ===> X=-1.482 Y=7.9982 Z=DOVETAILZOFFSET <===  502
         CALL AGSRESET                                                    502
         AG_X = -1.482                                                    502
         AG_Y = 7.9982                                                    502
         AG_Z = DOVETAILZOFFSET                                           502
         AG_TITLE = 'DTCC'                                                502
         AG_EXNAME = 'POSITION'                                           502
         AG_PARLIST = 'X_Y_Z'                                             502
         CALL AXPOSITION                                                  502
C    *                                                                    502
C    Create DTCD :                                                        503
C    Call To Local Procedure DTCD                                         503
         ASSIGN 6860 TO BLOCK_DTCD                                        503
         GO TO  5670                                                      503
6860     CONTINUE                                                         503
C    *                                                                    503
C    Geant POSITION : DTCD ===> X=-1.76032 Y=7.86181 Z=DOVETAILZOFFSET <  504
C    ===                                                                  504
         CALL AGSRESET                                                    504
         AG_X = -1.76032                                                  504
         AG_Y = 7.86181                                                   504
         AG_Z = DOVETAILZOFFSET                                           504
         AG_TITLE = 'DTCD'                                                504
         AG_EXNAME = 'POSITION'                                           504
         AG_PARLIST = 'X_Y_Z'                                             504
         CALL AXPOSITION                                                  504
C    *                                                                    504
C    Create DTCE :                                                        505
C    Call To Local Procedure DTCE                                         505
         ASSIGN 6870 TO BLOCK_DTCE                                        505
         GO TO  5690                                                      505
6870     CONTINUE                                                         505
C    *                                                                    505
C    Geant POSITION : DTCE ===> X=-3.11253 Y=7.51529 Z=DOVETAILZOFFSET <  506
C    ===                                                                  506
         CALL AGSRESET                                                    506
         AG_X = -3.11253                                                  506
         AG_Y = 7.51529                                                   506
         AG_Z = DOVETAILZOFFSET                                           506
         AG_TITLE = 'DTCE'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_Y_Z'                                             506
         CALL AXPOSITION                                                  506
C    *                                                                    506
C    Create DTCF :                                                        507
C    Call To Local Procedure DTCF                                         507
         ASSIGN 6880 TO BLOCK_DTCF                                        507
         GO TO  5710                                                      507
6880     CONTINUE                                                         507
C    *                                                                    507
C    Geant POSITION : DTCF ===> X=-3.35642 Y=7.32402 Z=DOVETAILZOFFSET <  508
C    ===                                                                  508
         CALL AGSRESET                                                    508
         AG_X = -3.35642                                                  508
         AG_Y = 7.32402                                                   508
         AG_Z = DOVETAILZOFFSET                                           508
         AG_TITLE = 'DTCF'                                                508
         AG_EXNAME = 'POSITION'                                           508
         AG_PARLIST = 'X_Y_Z'                                             508
         CALL AXPOSITION                                                  508
C    *                                                                    508
C    Create DTCG :                                                        509
C    Call To Local Procedure DTCG                                         509
         ASSIGN 6890 TO BLOCK_DTCG                                        509
         GO TO  5730                                                      509
6890     CONTINUE                                                         509
C    *                                                                    509
C    Geant POSITION : DTCG ===> X=-4.56862 Y=6.71349 Z=DOVETAILZOFFSET <  510
C    ===                                                                  510
         CALL AGSRESET                                                    510
         AG_X = -4.56862                                                  510
         AG_Y = 6.71349                                                   510
         AG_Z = DOVETAILZOFFSET                                           510
         AG_TITLE = 'DTCG'                                                510
         AG_EXNAME = 'POSITION'                                           510
         AG_PARLIST = 'X_Y_Z'                                             510
         CALL AXPOSITION                                                  510
C    *                                                                    510
C    Create DTCH :                                                        511
C    Call To Local Procedure DTCH                                         511
         ASSIGN 6900 TO BLOCK_DTCH                                        511
         GO TO  5750                                                      511
6900     CONTINUE                                                         511
C    *                                                                    511
C    Geant POSITION : DTCH ===> X=-1.68615 Y=2.88554 Z=DOVETAILZOFFSET <  512
C    ===                                                                  512
         CALL AGSRESET                                                    512
         AG_X = -1.68615                                                  512
         AG_Y = 2.88554                                                   512
         AG_Z = DOVETAILZOFFSET                                           512
         AG_TITLE = 'DTCH'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'X_Y_Z'                                             512
         CALL AXPOSITION                                                  512
C    *                                                                    512
C    Create DTSA :                                                        513
C    Call To Local Procedure DTSA                                         513
         ASSIGN 6910 TO BLOCK_DTSA                                        513
         GO TO  5770                                                      513
6910     CONTINUE                                                         513
C    *                                                                    513
C    Geant POSITION : DTSA ===> X=-0.174 Y=5.25916 Z=DOVETAILZOFFSET ALP  514
C    HAZ=90 <===                                                          514
         CALL AGSRESET                                                    514
         AG_X = -0.174                                                    514
         AG_Y = 5.25916                                                   514
         AG_Z = DOVETAILZOFFSET                                           514
         AG_ALPHAN=AG_ALPHAN+1                                            514
         AG_ALPHAK(AG_ALPHAN)=3                                           514
         AG_ALPHAL(AG_ALPHAN)=90                                          514
         AG_TITLE = 'DTSA'                                                514
         AG_EXNAME = 'POSITION'                                           514
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      514
         CALL AXPOSITION                                                  514
C    *                                                                    514
C    Create DTSB :                                                        515
C    Call To Local Procedure DTSB                                         515
         ASSIGN 6920 TO BLOCK_DTSB                                        515
         GO TO  5790                                                      515
6920     CONTINUE                                                         515
C    *                                                                    515
C    Geant POSITION : DTSB ===> X=-0.852755 Y=7.96401 Z=DOVETAILZOFFSET   516
C    ALPHAZ=169.963 <===                                                  516
         CALL AGSRESET                                                    516
         AG_X = -0.852755                                                 516
         AG_Y = 7.96401                                                   516
         AG_Z = DOVETAILZOFFSET                                           516
         AG_ALPHAN=AG_ALPHAN+1                                            516
         AG_ALPHAK(AG_ALPHAN)=3                                           516
         AG_ALPHAL(AG_ALPHAN)=169.963                                     516
         AG_TITLE = 'DTSB'                                                516
         AG_EXNAME = 'POSITION'                                           516
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      516
         CALL AXPOSITION                                                  516
C    *                                                                    516
C    Create DTSC :                                                        517
C    Call To Local Procedure DTSC                                         517
         ASSIGN 6930 TO BLOCK_DTSC                                        517
         GO TO  5810                                                      517
6930     CONTINUE                                                         517
C    *                                                                    517
C    Geant POSITION : DTSC ===> X=-1.54866 Y=7.94483 Z=DOVETAILZOFFSET A  518
C    LPHAZ=281.554 <===                                                   518
         CALL AGSRESET                                                    518
         AG_X = -1.54866                                                  518
         AG_Y = 7.94483                                                   518
         AG_Z = DOVETAILZOFFSET                                           518
         AG_ALPHAN=AG_ALPHAN+1                                            518
         AG_ALPHAK(AG_ALPHAN)=3                                           518
         AG_ALPHAL(AG_ALPHAN)=281.554                                     518
         AG_TITLE = 'DTSC'                                                518
         AG_EXNAME = 'POSITION'                                           518
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      518
         CALL AXPOSITION                                                  518
C    *                                                                    518
C    Create DTSD :                                                        519
C    Call To Local Procedure DTSD                                         519
         ASSIGN 6940 TO BLOCK_DTSD                                        519
         GO TO  5830                                                      519
6940     CONTINUE                                                         519
C    *                                                                    519
C    Geant POSITION : DTSD ===> X=-2.43389 Y=7.6146 Z=DOVETAILZOFFSET AL  520
C    PHAZ=181.963 <===                                                    520
         CALL AGSRESET                                                    520
         AG_X = -2.43389                                                  520
         AG_Y = 7.6146                                                    520
         AG_Z = DOVETAILZOFFSET                                           520
         AG_ALPHAN=AG_ALPHAN+1                                            520
         AG_ALPHAK(AG_ALPHAN)=3                                           520
         AG_ALPHAL(AG_ALPHAN)=181.963                                     520
         AG_TITLE = 'DTSD'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      520
         CALL AXPOSITION                                                  520
C    *                                                                    520
C    Create DTSE :                                                        521
C    Call To Local Procedure DTSE                                         521
         ASSIGN 6950 TO BLOCK_DTSE                                        521
         GO TO  5850                                                      521
6950     CONTINUE                                                         521
C    *                                                                    521
C    Geant POSITION : DTSE ===> X=-3.16664 Y=7.44923 Z=DOVETAILZOFFSET A  522
C    LPHAZ=293.554 <===                                                   522
         CALL AGSRESET                                                    522
         AG_X = -3.16664                                                  522
         AG_Y = 7.44923                                                   522
         AG_Z = DOVETAILZOFFSET                                           522
         AG_ALPHAN=AG_ALPHAN+1                                            522
         AG_ALPHAK(AG_ALPHAN)=3                                           522
         AG_ALPHAL(AG_ALPHAN)=293.554                                     522
         AG_TITLE = 'DTSE'                                                522
         AG_EXNAME = 'POSITION'                                           522
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      522
         CALL AXPOSITION                                                  522
C    *                                                                    522
C    Create DTSF :                                                        523
C    Call To Local Procedure DTSF                                         523
         ASSIGN 6960 TO BLOCK_DTSF                                        523
         GO TO  5870                                                      523
6960     CONTINUE                                                         523
C    *                                                                    523
C    Geant POSITION : DTSF ===> X=-3.94466 Y=6.94694 Z=DOVETAILZOFFSET A  524
C    LPHAZ=193.963 <===                                                   524
         CALL AGSRESET                                                    524
         AG_X = -3.94466                                                  524
         AG_Y = 6.94694                                                   524
         AG_Z = DOVETAILZOFFSET                                           524
         AG_ALPHAN=AG_ALPHAN+1                                            524
         AG_ALPHAK(AG_ALPHAN)=3                                           524
         AG_ALPHAL(AG_ALPHAN)=193.963                                     524
         AG_TITLE = 'DTSF'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      524
         CALL AXPOSITION                                                  524
C    *                                                                    524
C    Create DTSG :                                                        525
C    Call To Local Procedure DTSG                                         525
         ASSIGN 6970 TO BLOCK_DTSG                                        525
         GO TO  5890                                                      525
6970     CONTINUE                                                         525
C    *                                                                    525
C    Geant POSITION : DTSG ===> X=-3.22202 Y=4.73075 Z=DOVETAILZOFFSET A  526
C    LPHAZ=306 <===                                                       526
         CALL AGSRESET                                                    526
         AG_X = -3.22202                                                  526
         AG_Y = 4.73075                                                   526
         AG_Z = DOVETAILZOFFSET                                           526
         AG_ALPHAN=AG_ALPHAN+1                                            526
         AG_ALPHAK(AG_ALPHAN)=3                                           526
         AG_ALPHAL(AG_ALPHAN)=306                                         526
         AG_TITLE = 'DTSG'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      526
         CALL AXPOSITION                                                  526
C    *                                                                    526
C    Create DTSH :                                                        527
C    Call To Local Procedure DTSH                                         527
         ASSIGN 6980 TO BLOCK_DTSH                                        527
         GO TO  5910                                                      527
6980     CONTINUE                                                         527
C    *                                                                    527
C    Geant POSITION : DTSH ===> X=-0.976145 Y=2.67108 Z=DOVETAILZOFFSET   528
C    ALPHAZ=359.303 <===                                                  528
         CALL AGSRESET                                                    528
         AG_X = -0.976145                                                 528
         AG_Y = 2.67108                                                   528
         AG_Z = DOVETAILZOFFSET                                           528
         AG_ALPHAN=AG_ALPHAN+1                                            528
         AG_ALPHAK(AG_ALPHAN)=3                                           528
         AG_ALPHAL(AG_ALPHAN)=359.303                                     528
         AG_TITLE = 'DTSH'                                                528
         AG_EXNAME = 'POSITION'                                           528
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      528
         CALL AXPOSITION                                                  528
C    *                                                                    528
C    Create DTEA :                                                        529
C    Call To Local Procedure DTEA                                         529
         ASSIGN 6990 TO BLOCK_DTEA                                        529
         GO TO  5930                                                      529
6990     CONTINUE                                                         529
C    *                                                                    529
C    Geant POSITION : DTEA ===> X=-0.25 Y=2.73819 Z=DOVETAILEXTRUSIONZOF  530
C    FSET <===                                                            530
         CALL AGSRESET                                                    530
         AG_X = -0.25                                                     530
         AG_Y = 2.73819                                                   530
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  530
         AG_TITLE = 'DTEA'                                                530
         AG_EXNAME = 'POSITION'                                           530
         AG_PARLIST = 'X_Y_Z'                                             530
         CALL AXPOSITION                                                  530
C    *                                                                    530
C    Create DTEB :                                                        531
C    Call To Local Procedure DTEB                                         531
         ASSIGN 7000 TO BLOCK_DTEB                                        531
         GO TO  5950                                                      531
7000     CONTINUE                                                         531
C    *                                                                    531
C    Geant POSITION : DTEB ===> X=-0.25 Y=7.78014 Z=DOVETAILEXTRUSIONZOF  532
C    FSET <===                                                            532
         CALL AGSRESET                                                    532
         AG_X = -0.25                                                     532
         AG_Y = 7.78014                                                   532
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  532
         AG_TITLE = 'DTEB'                                                532
         AG_EXNAME = 'POSITION'                                           532
         AG_PARLIST = 'X_Y_Z'                                             532
         CALL AXPOSITION                                                  532
C    *                                                                    532
C    Create DTEC :                                                        533
C    Call To Local Procedure DTEC                                         533
         ASSIGN 7010 TO BLOCK_DTEC                                        533
         GO TO  5970                                                      533
7010     CONTINUE                                                         533
C    *                                                                    533
C    Geant POSITION : DTEC ===> X=-1.482 Y=7.9982 Z=DOVETAILEXTRUSIONZOF  534
C    FSET <===                                                            534
         CALL AGSRESET                                                    534
         AG_X = -1.482                                                    534
         AG_Y = 7.9982                                                    534
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  534
         AG_TITLE = 'DTEC'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'X_Y_Z'                                             534
         CALL AXPOSITION                                                  534
C    *                                                                    534
C    Create DTED :                                                        535
C    Call To Local Procedure DTED                                         535
         ASSIGN 7020 TO BLOCK_DTED                                        535
         GO TO  5990                                                      535
7020     CONTINUE                                                         535
C    *                                                                    535
C    Geant POSITION : DTED ===> X=-1.76032 Y=7.86181 Z=DOVETAILEXTRUSION  536
C    ZOFFSET <===                                                         536
         CALL AGSRESET                                                    536
         AG_X = -1.76032                                                  536
         AG_Y = 7.86181                                                   536
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  536
         AG_TITLE = 'DTED'                                                536
         AG_EXNAME = 'POSITION'                                           536
         AG_PARLIST = 'X_Y_Z'                                             536
         CALL AXPOSITION                                                  536
C    *                                                                    536
C    Create DTEE :                                                        537
C    Call To Local Procedure DTEE                                         537
         ASSIGN 7030 TO BLOCK_DTEE                                        537
         GO TO  6010                                                      537
7030     CONTINUE                                                         537
C    *                                                                    537
C    Geant POSITION : DTEE ===> X=-3.11253 Y=7.51529 Z=DOVETAILEXTRUSION  538
C    ZOFFSET <===                                                         538
         CALL AGSRESET                                                    538
         AG_X = -3.11253                                                  538
         AG_Y = 7.51529                                                   538
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  538
         AG_TITLE = 'DTEE'                                                538
         AG_EXNAME = 'POSITION'                                           538
         AG_PARLIST = 'X_Y_Z'                                             538
         CALL AXPOSITION                                                  538
C    *                                                                    538
C    Create DTEF :                                                        539
C    Call To Local Procedure DTEF                                         539
         ASSIGN 7040 TO BLOCK_DTEF                                        539
         GO TO  6030                                                      539
7040     CONTINUE                                                         539
C    *                                                                    539
C    Geant POSITION : DTEF ===> X=-3.35642 Y=7.32402 Z=DOVETAILEXTRUSION  540
C    ZOFFSET <===                                                         540
         CALL AGSRESET                                                    540
         AG_X = -3.35642                                                  540
         AG_Y = 7.32402                                                   540
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  540
         AG_TITLE = 'DTEF'                                                540
         AG_EXNAME = 'POSITION'                                           540
         AG_PARLIST = 'X_Y_Z'                                             540
         CALL AXPOSITION                                                  540
C    *                                                                    540
C    Create DTEG :                                                        541
C    Call To Local Procedure DTEG                                         541
         ASSIGN 7050 TO BLOCK_DTEG                                        541
         GO TO  6050                                                      541
7050     CONTINUE                                                         541
C    *                                                                    541
C    Geant POSITION : DTEG ===> X=-4.56862 Y=6.71349 Z=DOVETAILEXTRUSION  542
C    ZOFFSET <===                                                         542
         CALL AGSRESET                                                    542
         AG_X = -4.56862                                                  542
         AG_Y = 6.71349                                                   542
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  542
         AG_TITLE = 'DTEG'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'X_Y_Z'                                             542
         CALL AXPOSITION                                                  542
C    *                                                                    542
C    Create DTEH :                                                        543
C    Call To Local Procedure DTEH                                         543
         ASSIGN 7060 TO BLOCK_DTEH                                        543
         GO TO  6070                                                      543
7060     CONTINUE                                                         543
C    *                                                                    543
C    Geant POSITION : DTEH ===> X=-1.68615 Y=2.88554 Z=DOVETAILEXTRUSION  544
C    ZOFFSET <===                                                         544
         CALL AGSRESET                                                    544
         AG_X = -1.68615                                                  544
         AG_Y = 2.88554                                                   544
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  544
         AG_TITLE = 'DTEH'                                                544
         AG_EXNAME = 'POSITION'                                           544
         AG_PARLIST = 'X_Y_Z'                                             544
         CALL AXPOSITION                                                  544
C    *                                                                    544
C    Create DESA :                                                        545
C    Call To Local Procedure DESA                                         545
         ASSIGN 7070 TO BLOCK_DESA                                        545
         GO TO  6090                                                      545
7070     CONTINUE                                                         545
C    *                                                                    545
C    Geant POSITION : DESA ===> X=-0.162 Y=5.25916 Z=DOVETAILEXTRUSIONZO  546
C    FFSET ALPHAZ=90 <===                                                 546
         CALL AGSRESET                                                    546
         AG_X = -0.162                                                    546
         AG_Y = 5.25916                                                   546
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  546
         AG_ALPHAN=AG_ALPHAN+1                                            546
         AG_ALPHAK(AG_ALPHAN)=3                                           546
         AG_ALPHAL(AG_ALPHAN)=90                                          546
         AG_TITLE = 'DESA'                                                546
         AG_EXNAME = 'POSITION'                                           546
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      546
         CALL AXPOSITION                                                  546
C    *                                                                    546
C    Create DESB :                                                        547
C    Call To Local Procedure DESB                                         547
         ASSIGN 7080 TO BLOCK_DESB                                        547
         GO TO  6110                                                      547
7080     CONTINUE                                                         547
C    *                                                                    547
C    Geant POSITION : DESB ===> X=-0.88552 Y=7.77888 Z=DOVETAILEXTRUSION  548
C    ZOFFSET ALPHAZ=169.963 <===                                          548
         CALL AGSRESET                                                    548
         AG_X = -0.88552                                                  548
         AG_Y = 7.77888                                                   548
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  548
         AG_ALPHAN=AG_ALPHAN+1                                            548
         AG_ALPHAK(AG_ALPHAN)=3                                           548
         AG_ALPHAL(AG_ALPHAN)=169.963                                     548
         AG_TITLE = 'DESB'                                                548
         AG_EXNAME = 'POSITION'                                           548
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      548
         CALL AXPOSITION                                                  548
C    *                                                                    548
C    Create DESC :                                                        549
C    Call To Local Procedure DESC                                         549
         ASSIGN 7090 TO BLOCK_DESC                                        549
         GO TO  6130                                                      549
7090     CONTINUE                                                         549
C    *                                                                    549
C    Geant POSITION : DESC ===> X=-1.36447 Y=7.98248 Z=DOVETAILEXTRUSION  550
C    ZOFFSET ALPHAZ=281.554 <===                                          550
         CALL AGSRESET                                                    550
         AG_X = -1.36447                                                  550
         AG_Y = 7.98248                                                   550
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  550
         AG_ALPHAN=AG_ALPHAN+1                                            550
         AG_ALPHAK(AG_ALPHAN)=3                                           550
         AG_ALPHAL(AG_ALPHAN)=281.554                                     550
         AG_TITLE = 'DESC'                                                550
         AG_EXNAME = 'POSITION'                                           550
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      550
         CALL AXPOSITION                                                  550
C    *                                                                    550
C    Create DESD :                                                        551
C    Call To Local Procedure DESD                                         551
         ASSIGN 7100 TO BLOCK_DESD                                        551
         GO TO  6150                                                      551
7100     CONTINUE                                                         551
C    *                                                                    551
C    Geant POSITION : DESD ===> X=-2.42745 Y=7.42671 Z=DOVETAILEXTRUSION  552
C    ZOFFSET ALPHAZ=181.963 <===                                          552
         CALL AGSRESET                                                    552
         AG_X = -2.42745                                                  552
         AG_Y = 7.42671                                                   552
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  552
         AG_ALPHAN=AG_ALPHAN+1                                            552
         AG_ALPHAK(AG_ALPHAN)=3                                           552
         AG_ALPHAL(AG_ALPHAN)=181.963                                     552
         AG_TITLE = 'DESD'                                                552
         AG_EXNAME = 'POSITION'                                           552
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      552
         CALL AXPOSITION                                                  552
C    *                                                                    552
C    Create DESE :                                                        553
C    Call To Local Procedure DESE                                         553
         ASSIGN 7110 TO BLOCK_DESE                                        553
         GO TO  6170                                                      553
7110     CONTINUE                                                         553
C    *                                                                    553
C    Geant POSITION : DESE ===> X=-2.99431 Y=7.52436 Z=DOVETAILEXTRUSION  554
C    ZOFFSET ALPHAZ=293.554 <===                                          554
         CALL AGSRESET                                                    554
         AG_X = -2.99431                                                  554
         AG_Y = 7.52436                                                   554
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  554
         AG_ALPHAN=AG_ALPHAN+1                                            554
         AG_ALPHAK(AG_ALPHAN)=3                                           554
         AG_ALPHAL(AG_ALPHAN)=293.554                                     554
         AG_TITLE = 'DESE'                                                554
         AG_EXNAME = 'POSITION'                                           554
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      554
         CALL AXPOSITION                                                  554
C    *                                                                    554
C    Create DESF :                                                        555
C    Call To Local Procedure DESF                                         555
         ASSIGN 7120 TO BLOCK_DESF                                        555
         GO TO  6190                                                      555
7120     CONTINUE                                                         555
C    *                                                                    555
C    Geant POSITION : DESF ===> X=-3.8993 Y=6.7645 Z=DOVETAILEXTRUSIONZO  556
C    FFSET ALPHAZ=193.963 <===                                            556
         CALL AGSRESET                                                    556
         AG_X = -3.8993                                                   556
         AG_Y = 6.7645                                                    556
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  556
         AG_ALPHAN=AG_ALPHAN+1                                            556
         AG_ALPHAK(AG_ALPHAN)=3                                           556
         AG_ALPHAL(AG_ALPHAN)=193.963                                     556
         AG_TITLE = 'DESF'                                                556
         AG_EXNAME = 'POSITION'                                           556
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      556
         CALL AXPOSITION                                                  556
C    *                                                                    556
C    Create DESG :                                                        557
C    Call To Local Procedure DESG                                         557
         ASSIGN 7130 TO BLOCK_DESG                                        557
         GO TO  6210                                                      557
7130     CONTINUE                                                         557
C    *                                                                    557
C    Geant POSITION : DESG ===> X=-3.06992 Y=4.84126 Z=DOVETAILEXTRUSION  558
C    ZOFFSET ALPHAZ=306 <===                                              558
         CALL AGSRESET                                                    558
         AG_X = -3.06992                                                  558
         AG_Y = 4.84126                                                   558
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  558
         AG_ALPHAN=AG_ALPHAN+1                                            558
         AG_ALPHAK(AG_ALPHAN)=3                                           558
         AG_ALPHAL(AG_ALPHAN)=306                                         558
         AG_TITLE = 'DESG'                                                558
         AG_EXNAME = 'POSITION'                                           558
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      558
         CALL AXPOSITION                                                  558
C    *                                                                    558
C    Create DESH :                                                        559
C    Call To Local Procedure DESH                                         559
         ASSIGN 7140 TO BLOCK_DESH                                        559
         GO TO  6230                                                      559
7140     CONTINUE                                                         559
C    *                                                                    559
C    Geant POSITION : DESH ===> X=-0.964849 Y=2.88281 Z=DOVETAILEXTRUSIO  560
C    NZOFFSET ALPHAZ=357.396 <===                                         560
         CALL AGSRESET                                                    560
         AG_X = -0.964849                                                 560
         AG_Y = 2.88281                                                   560
         AG_Z = DOVETAILEXTRUSIONZOFFSET                                  560
         AG_ALPHAN=AG_ALPHAN+1                                            560
         AG_ALPHAK(AG_ALPHAN)=3                                           560
         AG_ALPHAL(AG_ALPHAN)=357.396                                     560
         AG_TITLE = 'DESH'                                                560
         AG_EXNAME = 'POSITION'                                           560
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      560
         CALL AXPOSITION                                                  560
C    *                                                                    560
C    Create PTRA :                                                        561
C    Call To Local Procedure PTRA                                         561
         ASSIGN 7150 TO BLOCK_PTRA                                        561
         GO TO  6250                                                      561
7150     CONTINUE                                                         561
C    *                                                                    561
C    Geant POSITION : PTRA ===> X=-0.844273 Y=7.31126 Z=0.85+DOVETAILSHA  562
C    PEOFFSET ALPHAZ=-174 <===                                            562
         CALL AGSRESET                                                    562
         AG_X = -0.844273                                                 562
         AG_Y = 7.31126                                                   562
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  562
         AG_ALPHAN=AG_ALPHAN+1                                            562
         AG_ALPHAK(AG_ALPHAN)=3                                           562
         AG_ALPHAL(AG_ALPHAN)=-174                                        562
         AG_TITLE = 'PTRA'                                                562
         AG_EXNAME = 'POSITION'                                           562
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      562
         CALL AXPOSITION                                                  562
C    *                                                                    562
C    Create PTRB :                                                        563
C    Call To Local Procedure PTRB                                         563
         ASSIGN 7160 TO BLOCK_PTRB                                        563
         GO TO  6270                                                      563
7160     CONTINUE                                                         563
C    *                                                                    563
C    Geant POSITION : PTRB ===> X=-2.2742 Y=6.99926 Z=0.85+DOVETAILSHAPE  564
C    OFFSET ALPHAZ=-162 <===                                              564
         CALL AGSRESET                                                    564
         AG_X = -2.2742                                                   564
         AG_Y = 6.99926                                                   564
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  564
         AG_ALPHAN=AG_ALPHAN+1                                            564
         AG_ALPHAK(AG_ALPHAN)=3                                           564
         AG_ALPHAL(AG_ALPHAN)=-162                                        564
         AG_TITLE = 'PTRB'                                                564
         AG_EXNAME = 'POSITION'                                           564
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      564
         CALL AXPOSITION                                                  564
C    *                                                                    564
C    Create PTRC :                                                        565
C    Call To Local Procedure PTRC                                         565
         ASSIGN 7170 TO BLOCK_PTRC                                        565
         GO TO  6290                                                      565
7170     CONTINUE                                                         565
C    *                                                                    565
C    Geant POSITION : PTRC ===> X=-3.61442 Y=6.41119 Z=0.85+DOVETAILSHAP  566
C    EOFFSET ALPHAZ=-150 <===                                             566
         CALL AGSRESET                                                    566
         AG_X = -3.61442                                                  566
         AG_Y = 6.41119                                                   566
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  566
         AG_ALPHAN=AG_ALPHAN+1                                            566
         AG_ALPHAK(AG_ALPHAN)=3                                           566
         AG_ALPHAL(AG_ALPHAN)=-150                                        566
         AG_TITLE = 'PTRC'                                                566
         AG_EXNAME = 'POSITION'                                           566
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      566
         CALL AXPOSITION                                                  566
C    *                                                                    566
C    Create PTRE :                                                        567
C    Call To Local Procedure PTRE                                         567
         ASSIGN 7180 TO BLOCK_PTRE                                        567
         GO TO  6310                                                      567
7180     CONTINUE                                                         567
C    *                                                                    567
C    Geant POSITION : PTRE ===> X=-0.45 Y=4.93658 Z=0.85+DOVETAILSHAPEOF  568
C    FSET ALPHAZ=90 <===                                                  568
         CALL AGSRESET                                                    568
         AG_X = -0.45                                                     568
         AG_Y = 4.93658                                                   568
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  568
         AG_ALPHAN=AG_ALPHAN+1                                            568
         AG_ALPHAK(AG_ALPHAN)=3                                           568
         AG_ALPHAL(AG_ALPHAN)=90                                          568
         AG_TITLE = 'PTRE'                                                568
         AG_EXNAME = 'POSITION'                                           568
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      568
         CALL AXPOSITION                                                  568
C    *                                                                    568
C    Create PTRF :                                                        569
C    Call To Local Procedure PTRF                                         569
         ASSIGN 7190 TO BLOCK_PTRF                                        569
         GO TO  6330                                                      569
7190     CONTINUE                                                         569
C    *                                                                    569
C    Geant POSITION : PTRF ===> X=-2.64683 Y=4.40863 Z=0.85+DOVETAILSHAP  570
C    EOFFSET ALPHAZ=126 <===                                              570
         CALL AGSRESET                                                    570
         AG_X = -2.64683                                                  570
         AG_Y = 4.40863                                                   570
         AG_Z = 0.85+DOVETAILSHAPEOFFSET                                  570
         AG_ALPHAN=AG_ALPHAN+1                                            570
         AG_ALPHAK(AG_ALPHAN)=3                                           570
         AG_ALPHAL(AG_ALPHAN)=126                                         570
         AG_TITLE = 'PTRF'                                                570
         AG_EXNAME = 'POSITION'                                           570
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      570
         CALL AXPOSITION                                                  570
C    *                                                                    570
C    Create PTRG :                                                        571
C    Call To Local Procedure PTRG                                         571
         ASSIGN 7200 TO BLOCK_PTRG                                        571
         GO TO  6350                                                      571
7200     CONTINUE                                                         571
C    *                                                                    571
C    Geant POSITION : PTRG ===> X=-1.06041 Y=3.14864 Z=0.25+DOVETAILSHAP  572
C    EOFFSET ALPHAZ=177.396 <===                                          572
         CALL AGSRESET                                                    572
         AG_X = -1.06041                                                  572
         AG_Y = 3.14864                                                   572
         AG_Z = 0.25+DOVETAILSHAPEOFFSET                                  572
         AG_ALPHAN=AG_ALPHAN+1                                            572
         AG_ALPHAK(AG_ALPHAN)=3                                           572
         AG_ALPHAL(AG_ALPHAN)=177.396                                     572
         AG_TITLE = 'PTRG'                                                572
         AG_EXNAME = 'POSITION'                                           572
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      572
         CALL AXPOSITION                                                  572
C    *                                                                    572
C    Create PTMA :                                                        573
C    Call To Local Procedure PTMA                                         573
         ASSIGN 7210 TO BLOCK_PTMA                                        573
         GO TO  6370                                                      573
7210     CONTINUE                                                         573
C    *                                                                    573
C    Geant POSITION : PTMA ===> X=-1.09427 Y=7.28499 Z=0.2+DOVETAILSHAPE  574
C    OFFSET ALPHAZ=-174 <===                                              574
         CALL AGSRESET                                                    574
         AG_X = -1.09427                                                  574
         AG_Y = 7.28499                                                   574
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   574
         AG_ALPHAN=AG_ALPHAN+1                                            574
         AG_ALPHAK(AG_ALPHAN)=3                                           574
         AG_ALPHAL(AG_ALPHAN)=-174                                        574
         AG_TITLE = 'PTMA'                                                574
         AG_EXNAME = 'POSITION'                                           574
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      574
         CALL AXPOSITION                                                  574
C    *                                                                    574
C    Create PTMB :                                                        575
C    Call To Local Procedure PTMB                                         575
         ASSIGN 7220 TO BLOCK_PTMB                                        575
         GO TO  6390                                                      575
7220     CONTINUE                                                         575
C    *                                                                    575
C    Geant POSITION : PTMB ===> X=-2.2742 Y=6.99926 Z=0.2+DOVETAILSHAPEO  576
C    FFSET ALPHAZ=-162 <===                                               576
         CALL AGSRESET                                                    576
         AG_X = -2.2742                                                   576
         AG_Y = 6.99926                                                   576
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   576
         AG_ALPHAN=AG_ALPHAN+1                                            576
         AG_ALPHAK(AG_ALPHAN)=3                                           576
         AG_ALPHAL(AG_ALPHAN)=-162                                        576
         AG_TITLE = 'PTMB'                                                576
         AG_EXNAME = 'POSITION'                                           576
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      576
         CALL AXPOSITION                                                  576
C    *                                                                    576
C    Create PTMC :                                                        577
C    Call To Local Procedure PTMC                                         577
         ASSIGN 7230 TO BLOCK_PTMC                                        577
         GO TO  6410                                                      577
7230     CONTINUE                                                         577
C    *                                                                    577
C    Geant POSITION : PTMC ===> X=-3.39672 Y=6.53688 Z=0.2+DOVETAILSHAPE  578
C    OFFSET ALPHAZ=-150 <===                                              578
         CALL AGSRESET                                                    578
         AG_X = -3.39672                                                  578
         AG_Y = 6.53688                                                   578
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   578
         AG_ALPHAN=AG_ALPHAN+1                                            578
         AG_ALPHAK(AG_ALPHAN)=3                                           578
         AG_ALPHAL(AG_ALPHAN)=-150                                        578
         AG_TITLE = 'PTMC'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      578
         CALL AXPOSITION                                                  578
C    *                                                                    578
C    Create PTME :                                                        579
C    Call To Local Procedure PTME                                         579
         ASSIGN 7240 TO BLOCK_PTME                                        579
         GO TO  6430                                                      579
7240     CONTINUE                                                         579
C    *                                                                    579
C    Geant POSITION : PTME ===> X=-0.7 Y=4.92913 Z=0.2+DOVETAILSHAPEOFFS  580
C    ET ALPHAZ=90 <===                                                    580
         CALL AGSRESET                                                    580
         AG_X = -0.7                                                      580
         AG_Y = 4.92913                                                   580
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   580
         AG_ALPHAN=AG_ALPHAN+1                                            580
         AG_ALPHAK(AG_ALPHAN)=3                                           580
         AG_ALPHAL(AG_ALPHAN)=90                                          580
         AG_TITLE = 'PTME'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      580
         CALL AXPOSITION                                                  580
C    *                                                                    580
C    Create PTMF :                                                        581
C    Call To Local Procedure PTMF                                         581
         ASSIGN 7250 TO BLOCK_PTMF                                        581
         GO TO  6450                                                      581
7250     CONTINUE                                                         581
C    *                                                                    581
C    Geant POSITION : PTMF ===> X=-2.37819 Y=4.46421 Z=0.2+DOVETAILSHAPE  582
C    OFFSET ALPHAZ=126 <===                                               582
         CALL AGSRESET                                                    582
         AG_X = -2.37819                                                  582
         AG_Y = 4.46421                                                   582
         AG_Z = 0.2+DOVETAILSHAPEOFFSET                                   582
         AG_ALPHAN=AG_ALPHAN+1                                            582
         AG_ALPHAK(AG_ALPHAN)=3                                           582
         AG_ALPHAL(AG_ALPHAN)=126                                         582
         AG_TITLE = 'PTMF'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Create PTLA :                                                        583
C    Call To Local Procedure PTLA                                         583
         ASSIGN 7260 TO BLOCK_PTLA                                        583
         GO TO  6470                                                      583
7260     CONTINUE                                                         583
C    *                                                                    583
C    Geant POSITION : PTLA ===> X=-0.769273 Y=7.31915 Z=-0.85+DOVETAILSH  584
C    APEOFFSET ALPHAZ=-174 <===                                           584
         CALL AGSRESET                                                    584
         AG_X = -0.769273                                                 584
         AG_Y = 7.31915                                                   584
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 584
         AG_ALPHAN=AG_ALPHAN+1                                            584
         AG_ALPHAK(AG_ALPHAN)=3                                           584
         AG_ALPHAL(AG_ALPHAN)=-174                                        584
         AG_TITLE = 'PTLA'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      584
         CALL AXPOSITION                                                  584
C    *                                                                    584
C    Create PTLB :                                                        585
C    Call To Local Procedure PTLB                                         585
         ASSIGN 7270 TO BLOCK_PTLB                                        585
         GO TO  6490                                                      585
7270     CONTINUE                                                         585
C    *                                                                    585
C    Geant POSITION : PTLB ===> X=-2.2742 Y=6.99926 Z=-0.85+DOVETAILSHAP  586
C    EOFFSET ALPHAZ=-162 <===                                             586
         CALL AGSRESET                                                    586
         AG_X = -2.2742                                                   586
         AG_Y = 6.99926                                                   586
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 586
         AG_ALPHAN=AG_ALPHAN+1                                            586
         AG_ALPHAK(AG_ALPHAN)=3                                           586
         AG_ALPHAL(AG_ALPHAN)=-162                                        586
         AG_TITLE = 'PTLB'                                                586
         AG_EXNAME = 'POSITION'                                           586
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      586
         CALL AXPOSITION                                                  586
C    *                                                                    586
C    Create PTLC :                                                        587
C    Call To Local Procedure PTLC                                         587
         ASSIGN 7280 TO BLOCK_PTLC                                        587
         GO TO  6510                                                      587
7280     CONTINUE                                                         587
C    *                                                                    587
C    Geant POSITION : PTLC ===> X=-3.67973 Y=6.37348 Z=-0.85+DOVETAILSHA  588
C    PEOFFSET ALPHAZ=-150 <===                                            588
         CALL AGSRESET                                                    588
         AG_X = -3.67973                                                  588
         AG_Y = 6.37348                                                   588
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 588
         AG_ALPHAN=AG_ALPHAN+1                                            588
         AG_ALPHAK(AG_ALPHAN)=3                                           588
         AG_ALPHAL(AG_ALPHAN)=-150                                        588
         AG_TITLE = 'PTLC'                                                588
         AG_EXNAME = 'POSITION'                                           588
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      588
         CALL AXPOSITION                                                  588
C    *                                                                    588
C    Create PTLD :                                                        589
C    Call To Local Procedure PTLD                                         589
         ASSIGN 7290 TO BLOCK_PTLD                                        589
         GO TO  6530                                                      589
7290     CONTINUE                                                         589
C    *                                                                    589
C    Geant POSITION : PTLD ===> X=-0.322263 Y=3.06613 Z=-0.85+DOVETAILSH  590
C    APEOFFSET ALPHAZ=-174 <===                                           590
         CALL AGSRESET                                                    590
         AG_X = -0.322263                                                 590
         AG_Y = 3.06613                                                   590
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 590
         AG_ALPHAN=AG_ALPHAN+1                                            590
         AG_ALPHAK(AG_ALPHAN)=3                                           590
         AG_ALPHAL(AG_ALPHAN)=-174                                        590
         AG_TITLE = 'PTLD'                                                590
         AG_EXNAME = 'POSITION'                                           590
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      590
         CALL AXPOSITION                                                  590
C    *                                                                    590
C    Create PTLE :                                                        591
C    Call To Local Procedure PTLE                                         591
         ASSIGN 7300 TO BLOCK_PTLE                                        591
         GO TO  6550                                                      591
7300     CONTINUE                                                         591
C    *                                                                    591
C    Geant POSITION : PTLE ===> X=-0.952705 Y=2.93212 Z=-0.85+DOVETAILSH  592
C    APEOFFSET ALPHAZ=-162 <===                                           592
         CALL AGSRESET                                                    592
         AG_X = -0.952705                                                 592
         AG_Y = 2.93212                                                   592
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 592
         AG_ALPHAN=AG_ALPHAN+1                                            592
         AG_ALPHAK(AG_ALPHAN)=3                                           592
         AG_ALPHAL(AG_ALPHAN)=-162                                        592
         AG_TITLE = 'PTLE'                                                592
         AG_EXNAME = 'POSITION'                                           592
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      592
         CALL AXPOSITION                                                  592
C    *                                                                    592
C    Create PTLF :                                                        593
C    Call To Local Procedure PTLF                                         593
         ASSIGN 7310 TO BLOCK_PTLF                                        593
         GO TO  6570                                                      593
7310     CONTINUE                                                         593
C    *                                                                    593
C    Geant POSITION : PTLF ===> X=-1.54151 Y=2.66997 Z=-0.85+DOVETAILSHA  594
C    PEOFFSET ALPHAZ=-150 <===                                            594
         CALL AGSRESET                                                    594
         AG_X = -1.54151                                                  594
         AG_Y = 2.66997                                                   594
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 594
         AG_ALPHAN=AG_ALPHAN+1                                            594
         AG_ALPHAK(AG_ALPHAN)=3                                           594
         AG_ALPHAL(AG_ALPHAN)=-150                                        594
         AG_TITLE = 'PTLF'                                                594
         AG_EXNAME = 'POSITION'                                           594
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      594
         CALL AXPOSITION                                                  594
C    *                                                                    594
C    Create PTLG :                                                        595
C    Call To Local Procedure PTLG                                         595
         ASSIGN 7320 TO BLOCK_PTLG                                        595
         GO TO  6590                                                      595
7320     CONTINUE                                                         595
C    *                                                                    595
C    Geant POSITION : PTLG ===> X=-0.375 Y=5.31059 Z=-0.85+DOVETAILSHAPE  596
C    OFFSET ALPHAZ=-174 <===                                              596
         CALL AGSRESET                                                    596
         AG_X = -0.375                                                    596
         AG_Y = 5.31059                                                   596
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 596
         AG_ALPHAN=AG_ALPHAN+1                                            596
         AG_ALPHAK(AG_ALPHAN)=3                                           596
         AG_ALPHAL(AG_ALPHAN)=-174                                        596
         AG_TITLE = 'PTLG'                                                596
         AG_EXNAME = 'POSITION'                                           596
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      596
         CALL AXPOSITION                                                  596
C    *                                                                    596
C    Create PTLH :                                                        597
C    Call To Local Procedure PTLH                                         597
         ASSIGN 7330 TO BLOCK_PTLH                                        597
         GO TO  6610                                                      597
7330     CONTINUE                                                         597
C    *                                                                    597
C    Geant POSITION : PTLH ===> X=-2.8181 Y=4.51677 Z=-0.85+DOVETAILSHAP  598
C    EOFFSET ALPHAZ=-150 <===                                             598
         CALL AGSRESET                                                    598
         AG_X = -2.8181                                                   598
         AG_Y = 4.51677                                                   598
         AG_Z = -0.85+DOVETAILSHAPEOFFSET                                 598
         AG_ALPHAN=AG_ALPHAN+1                                            598
         AG_ALPHAK(AG_ALPHAN)=3                                           598
         AG_ALPHAL(AG_ALPHAN)=-150                                        598
         AG_TITLE = 'PTLH'                                                598
         AG_EXNAME = 'POSITION'                                           598
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      598
         CALL AXPOSITION                                                  598
C    *                                                                    598
C    Create PTLI :                                                        599
C    Call To Local Procedure PTLI                                         599
         ASSIGN 7340 TO BLOCK_PTLI                                        599
         GO TO  6630                                                      599
7340     CONTINUE                                                         599
C    *                                                                    599
C    Geant POSITION : PTLI ===> X=-0.375 Y=5.11059 Z=-0.6+DOVETAILSHAPEO  600
C    FFSET ALPHAZ=-174 <===                                               600
         CALL AGSRESET                                                    600
         AG_X = -0.375                                                    600
         AG_Y = 5.11059                                                   600
         AG_Z = -0.6+DOVETAILSHAPEOFFSET                                  600
         AG_ALPHAN=AG_ALPHAN+1                                            600
         AG_ALPHAK(AG_ALPHAN)=3                                           600
         AG_ALPHAL(AG_ALPHAN)=-174                                        600
         AG_TITLE = 'PTLI'                                                600
         AG_EXNAME = 'POSITION'                                           600
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      600
         CALL AXPOSITION                                                  600
C    *                                                                    600
C    Create PTLJ :                                                        601
C    Call To Local Procedure PTLJ                                         601
         ASSIGN 7350 TO BLOCK_PTLJ                                        601
         GO TO  6650                                                      601
7350     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : PTLJ ===> X=-2.70055 Y=4.35497 Z=-0.6+DOVETAILSHAP  602
C    EOFFSET ALPHAZ=-150 <===                                             602
         CALL AGSRESET                                                    602
         AG_X = -2.70055                                                  602
         AG_Y = 4.35497                                                   602
         AG_Z = -0.6+DOVETAILSHAPEOFFSET                                  602
         AG_ALPHAN=AG_ALPHAN+1                                            602
         AG_ALPHAK(AG_ALPHAN)=3                                           602
         AG_ALPHAL(AG_ALPHAN)=-150                                        602
         AG_TITLE = 'PTLJ'                                                602
         AG_EXNAME = 'POSITION'                                           602
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      602
         CALL AXPOSITION                                                  602
C    *                                                                    602
C    Create LADR :                                                        603
C    Call To Local Procedure LADR                                         603
         ASSIGN 7370 TO BLOCK_LADR                                        603
         GO TO  7360                                                      603
7370     CONTINUE                                                         603
C    *                                                                    603
C    Geant POSITION : LADR ===> X=-0.527366 Y=2.54834 Z=-4.8855 ALPHAZ=1  604
C    77.396 <===                                                          604
         CALL AGSRESET                                                    604
         AG_X = -0.527366                                                 604
         AG_Y = 2.54834                                                   604
         AG_Z = -4.8855                                                   604
         AG_ALPHAN=AG_ALPHAN+1                                            604
         AG_ALPHAK(AG_ALPHAN)=3                                           604
         AG_ALPHAL(AG_ALPHAN)=177.396                                     604
         AG_TITLE = 'LADR'                                                604
         AG_EXNAME = 'POSITION'                                           604
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      604
         CALL AXPOSITION                                                  604
C    *                                                                    604
C    Create LADR :                                                        605
C    Call To Local Procedure LADR                                         605
         ASSIGN 7380 TO BLOCK_LADR                                        605
         GO TO  7360                                                      605
7380     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : LADR ===> X=-1.30523 Y=8.1723 Z=-4.8855 ALPHAZ=349  606
C    .963 <===                                                            606
         CALL AGSRESET                                                    606
         AG_X = -1.30523                                                  606
         AG_Y = 8.1723                                                    606
         AG_Z = -4.8855                                                   606
         AG_ALPHAN=AG_ALPHAN+1                                            606
         AG_ALPHAK(AG_ALPHAN)=3                                           606
         AG_ALPHAL(AG_ALPHAN)=349.963                                     606
         AG_TITLE = 'LADR'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      606
         CALL AXPOSITION                                                  606
C    *                                                                    606
C    Create LADR :                                                        607
C    Call To Local Procedure LADR                                         607
         ASSIGN 7390 TO BLOCK_LADR                                        607
         GO TO  7360                                                      607
7390     CONTINUE                                                         607
C    *                                                                    607
C    Geant POSITION : LADR ===> X=-2.97583 Y=7.72235 Z=-4.8855 ALPHAZ=1.  608
C    963 <===                                                             608
         CALL AGSRESET                                                    608
         AG_X = -2.97583                                                  608
         AG_Y = 7.72235                                                   608
         AG_Z = -4.8855                                                   608
         AG_ALPHAN=AG_ALPHAN+1                                            608
         AG_ALPHAK(AG_ALPHAN)=3                                           608
         AG_ALPHAL(AG_ALPHAN)=1.963                                       608
         AG_TITLE = 'LADR'                                                608
         AG_EXNAME = 'POSITION'                                           608
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      608
         CALL AXPOSITION                                                  608
C    *                                                                    608
C    Create LADR :                                                        609
C    Call To Local Procedure LADR                                         609
         ASSIGN 7400 TO BLOCK_LADR                                        609
         GO TO  7360                                                      609
7400     CONTINUE                                                         609
C    *                                                                    609
C    Geant POSITION : LADR ===> X=-4.51636 Y=6.93489 Z=-4.8855 ALPHAZ=13  610
C    .963 <===                                                            610
         CALL AGSRESET                                                    610
         AG_X = -4.51636                                                  610
         AG_Y = 6.93489                                                   610
         AG_Z = -4.8855                                                   610
         AG_ALPHAN=AG_ALPHAN+1                                            610
         AG_ALPHAK(AG_ALPHAN)=3                                           610
         AG_ALPHAL(AG_ALPHAN)=13.963                                      610
         AG_TITLE = 'LADR'                                                610
         AG_EXNAME = 'POSITION'                                           610
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      610
         CALL AXPOSITION                                                  610
C    *                                                                    610
5271  CALL AGSPOPB                                                        611
      GO TO BLOCK_PXLA, (5270 ,5280)                                      611
C End of Local Procedure                                                  611
C ----------------------------------------------------------------------  612
C ------------ PXCA                                                       612
C Begin Local Procedure PXCA                                              613
C PXCA ARC BOTTOM RIGHT                                                   613
5290  CONTINUE                                                            613
         AG_TITLE='PXCA'                                                  613
         AG_MODULE=MODULE                                                 613
         AG_MARK='V00'                                                    613
         CALL AGSPUSH                                                     613
         AG_MODULE=MODULE                                                 613
C    *                                                                    613
C    Geant ATTRIBUTE : PXCA ===> SEEN=1 COLO=410 <===                     614
         CALL AGSRESET                                                    614
         AG_SEEN = 1                                                      614
         AG_COLO = 410                                                    614
         AG_TITLE = 'PXCA'                                                614
         AG_EXNAME = 'ATTRIBUTE'                                          614
         AG_PARLIST = 'SEEN_COLO'                                         614
         CALL AXATTRIBUTE                                                 614
C    *                                                                    614
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          614
         CALL AGSRESET                                                    615
         AG_TITLE = 'CFRPMIX'                                             615
         AG_EXNAME = 'MATERIAL'                                           615
         AG_PARLIST = 'NONE'                                              615
         CALL AXMATERIAL                                                  615
C    *                                                                    615
C    Geant SHAPE : TUBS ===> RMIN=0.126 RMAX=0.15 PHI1=267.396, PHI2=360  615
C     DZ=SECTORLENGTH <===                                                615
         CALL AGSRESET                                                    617
         AG_RMIN = 0.126                                                  617
         AG_RMAX = 0.15                                                   617
         AG_PHI1 = 267.396                                                617
         AG_PHI2 = 360                                                    617
         AG_DZ = SECTORLENGTH                                             617
         AG_TITLE = 'TUBS'                                                617
         AG_EXNAME = 'SHAPE'                                              617
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            617
         CALL AXSHAPE                                                     617
C    *                                                                    617
5291  CALL AGSPOPB                                                        618
      GO TO BLOCK_PXCA, (5290 ,5300,6670)                                 618
C End of Local Procedure                                                  618
C ----------------------------------------------------------------------  619
C ------------ PXCC                                                       619
C Begin Local Procedure PXCC                                              620
C PXCC ARC TOPRIGHT                                                       620
5310  CONTINUE                                                            620
         AG_TITLE='PXCC'                                                  620
         AG_MODULE=MODULE                                                 620
         AG_MARK='V00'                                                    620
         CALL AGSPUSH                                                     620
         AG_MODULE=MODULE                                                 620
C    *                                                                    620
C    Geant ATTRIBUTE : PXCC ===> SEEN=1 COLO=410 <===                     621
         CALL AGSRESET                                                    621
         AG_SEEN = 1                                                      621
         AG_COLO = 410                                                    621
         AG_TITLE = 'PXCC'                                                621
         AG_EXNAME = 'ATTRIBUTE'                                          621
         AG_PARLIST = 'SEEN_COLO'                                         621
         CALL AXATTRIBUTE                                                 621
C    *                                                                    621
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          621
         CALL AGSRESET                                                    622
         AG_TITLE = 'CFRPMIX'                                             622
         AG_EXNAME = 'MATERIAL'                                           622
         AG_PARLIST = 'NONE'                                              622
         CALL AXMATERIAL                                                  622
C    *                                                                    622
C    Geant SHAPE : TUBS ===> RMIN=0.126 RMAX=0.15 PHI1=0 PHI2=79.963, DZ  622
C    =SECTORLENGTH <===                                                   622
         CALL AGSRESET                                                    624
         AG_RMIN = 0.126                                                  624
         AG_RMAX = 0.15                                                   624
         AG_PHI1 = 0                                                      624
         AG_PHI2 = 79.963                                                 624
         AG_DZ = SECTORLENGTH                                             624
         AG_TITLE = 'TUBS'                                                624
         AG_EXNAME = 'SHAPE'                                              624
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            624
         CALL AXSHAPE                                                     624
C    *                                                                    624
5311  CALL AGSPOPB                                                        625
      GO TO BLOCK_PXCC, (5310 ,5320,6680)                                 625
C End of Local Procedure                                                  625
C ----------------------------------------------------------------------  626
C ------------ PXCD                                                       626
C Begin Local Procedure PXCD                                              627
C PXCD ARC TOP 23                                                         627
5330  CONTINUE                                                            627
         AG_TITLE='PXCD'                                                  627
         AG_MODULE=MODULE                                                 627
         AG_MARK='V00'                                                    627
         CALL AGSPUSH                                                     627
         AG_MODULE=MODULE                                                 627
C    *                                                                    627
C    Geant ATTRIBUTE : PXCD ===> SEEN=1 COLO=410 <===                     628
         CALL AGSRESET                                                    628
         AG_SEEN = 1                                                      628
         AG_COLO = 410                                                    628
         AG_TITLE = 'PXCD'                                                628
         AG_EXNAME = 'ATTRIBUTE'                                          628
         AG_PARLIST = 'SEEN_COLO'                                         628
         CALL AXATTRIBUTE                                                 628
C    *                                                                    628
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          628
         CALL AGSRESET                                                    629
         AG_TITLE = 'CFRPMIX'                                             629
         AG_EXNAME = 'MATERIAL'                                           629
         AG_PARLIST = 'NONE'                                              629
         CALL AXMATERIAL                                                  629
C    *                                                                    629
C    Geant SHAPE : TUBS ===> RMIN=0.126 RMAX=0.15 PHI1=79.963, PHI2=191.  629
C    554 DZ=SECTORLENGTH <===                                             629
         CALL AGSRESET                                                    631
         AG_RMIN = 0.126                                                  631
         AG_RMAX = 0.15                                                   631
         AG_PHI1 = 79.963                                                 631
         AG_PHI2 = 191.554                                                631
         AG_DZ = SECTORLENGTH                                             631
         AG_TITLE = 'TUBS'                                                631
         AG_EXNAME = 'SHAPE'                                              631
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            631
         CALL AXSHAPE                                                     631
C    *                                                                    631
5331  CALL AGSPOPB                                                        632
      GO TO BLOCK_PXCD, (5330 ,5340,6690)                                 632
C End of Local Procedure                                                  632
C ----------------------------------------------------------------------  633
C ------------ PXCE                                                       633
C Begin Local Procedure PXCE                                              634
C PXCE ARC BOTTOM 23                                                      634
5350  CONTINUE                                                            634
         AG_TITLE='PXCE'                                                  634
         AG_MODULE=MODULE                                                 634
         AG_MARK='V00'                                                    634
         CALL AGSPUSH                                                     634
         AG_MODULE=MODULE                                                 634
C    *                                                                    634
C    Geant ATTRIBUTE : PXCE ===> SEEN=1 COLO=410 <===                     635
         CALL AGSRESET                                                    635
         AG_SEEN = 1                                                      635
         AG_COLO = 410                                                    635
         AG_TITLE = 'PXCE'                                                635
         AG_EXNAME = 'ATTRIBUTE'                                          635
         AG_PARLIST = 'SEEN_COLO'                                         635
         CALL AXATTRIBUTE                                                 635
C    *                                                                    635
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          635
         CALL AGSRESET                                                    636
         AG_TITLE = 'CFRPMIX'                                             636
         AG_EXNAME = 'MATERIAL'                                           636
         AG_PARLIST = 'NONE'                                              636
         CALL AXMATERIAL                                                  636
C    *                                                                    636
C    Geant SHAPE : TUBS ===> RMIN=0.15 RMAX=0.174 PHI1=271.963, PHI2=371  636
C    .554 DZ=SECTORLENGTH <===                                            636
         CALL AGSRESET                                                    638
         AG_RMIN = 0.15                                                   638
         AG_RMAX = 0.174                                                  638
         AG_PHI1 = 271.963                                                638
         AG_PHI2 = 371.554                                                638
         AG_DZ = SECTORLENGTH                                             638
         AG_TITLE = 'TUBS'                                                638
         AG_EXNAME = 'SHAPE'                                              638
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            638
         CALL AXSHAPE                                                     638
C    *                                                                    638
5351  CALL AGSPOPB                                                        639
      GO TO BLOCK_PXCE, (5350 ,5360,6700)                                 639
C End of Local Procedure                                                  639
C ----------------------------------------------------------------------  640
C ------------ PXCF                                                       640
C Begin Local Procedure PXCF                                              641
C PXCF ARC TOP 12                                                         641
5370  CONTINUE                                                            641
         AG_TITLE='PXCF'                                                  641
         AG_MODULE=MODULE                                                 641
         AG_MARK='V00'                                                    641
         CALL AGSPUSH                                                     641
         AG_MODULE=MODULE                                                 641
C    *                                                                    641
C    Geant ATTRIBUTE : PXCF ===> SEEN=1 COLO=410 <===                     642
         CALL AGSRESET                                                    642
         AG_SEEN = 1                                                      642
         AG_COLO = 410                                                    642
         AG_TITLE = 'PXCF'                                                642
         AG_EXNAME = 'ATTRIBUTE'                                          642
         AG_PARLIST = 'SEEN_COLO'                                         642
         CALL AXATTRIBUTE                                                 642
C    *                                                                    642
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          642
         CALL AGSRESET                                                    643
         AG_TITLE = 'CFRPMIX'                                             643
         AG_EXNAME = 'MATERIAL'                                           643
         AG_PARLIST = 'NONE'                                              643
         CALL AXMATERIAL                                                  643
C    *                                                                    643
C    Geant SHAPE : TUBS ===> RMIN=0.126 RMAX=0.15 PHI1=91.963, PHI2=203.  643
C    554 DZ=SECTORLENGTH <===                                             643
         CALL AGSRESET                                                    645
         AG_RMIN = 0.126                                                  645
         AG_RMAX = 0.15                                                   645
         AG_PHI1 = 91.963                                                 645
         AG_PHI2 = 203.554                                                645
         AG_DZ = SECTORLENGTH                                             645
         AG_TITLE = 'TUBS'                                                645
         AG_EXNAME = 'SHAPE'                                              645
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            645
         CALL AXSHAPE                                                     645
C    *                                                                    645
5371  CALL AGSPOPB                                                        646
      GO TO BLOCK_PXCF, (5370 ,5380,6710)                                 646
C End of Local Procedure                                                  646
C ----------------------------------------------------------------------  647
C ------------ PXCG                                                       647
C Begin Local Procedure PXCG                                              648
C PXCG ARC BOTTOM 12                                                      648
5390  CONTINUE                                                            648
         AG_TITLE='PXCG'                                                  648
         AG_MODULE=MODULE                                                 648
         AG_MARK='V00'                                                    648
         CALL AGSPUSH                                                     648
         AG_MODULE=MODULE                                                 648
C    *                                                                    648
C    Geant ATTRIBUTE : PXCG ===> SEEN=1 COLO=410 <===                     649
         CALL AGSRESET                                                    649
         AG_SEEN = 1                                                      649
         AG_COLO = 410                                                    649
         AG_TITLE = 'PXCG'                                                649
         AG_EXNAME = 'ATTRIBUTE'                                          649
         AG_PARLIST = 'SEEN_COLO'                                         649
         CALL AXATTRIBUTE                                                 649
C    *                                                                    649
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          649
         CALL AGSRESET                                                    650
         AG_TITLE = 'CFRPMIX'                                             650
         AG_EXNAME = 'MATERIAL'                                           650
         AG_PARLIST = 'NONE'                                              650
         CALL AXMATERIAL                                                  650
C    *                                                                    650
C    Geant SHAPE : TUBS ===> RMIN=0.15 RMAX=0.174 PHI1=283.963, PHI2=383  650
C    .554 DZ=SECTORLENGTH <===                                            650
         CALL AGSRESET                                                    652
         AG_RMIN = 0.15                                                   652
         AG_RMAX = 0.174                                                  652
         AG_PHI1 = 283.963                                                652
         AG_PHI2 = 383.554                                                652
         AG_DZ = SECTORLENGTH                                             652
         AG_TITLE = 'TUBS'                                                652
         AG_EXNAME = 'SHAPE'                                              652
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            652
         CALL AXSHAPE                                                     652
C    *                                                                    652
5391  CALL AGSPOPB                                                        653
      GO TO BLOCK_PXCG, (5390 ,5400,6720)                                 653
C End of Local Procedure                                                  653
C ----------------------------------------------------------------------  654
C ------------ PXCH                                                       654
C Begin Local Procedure PXCH                                              655
C PXCH ARC TOP LEFT                                                       655
5410  CONTINUE                                                            655
         AG_TITLE='PXCH'                                                  655
         AG_MODULE=MODULE                                                 655
         AG_MARK='V00'                                                    655
         CALL AGSPUSH                                                     655
         AG_MODULE=MODULE                                                 655
C    *                                                                    655
C    Geant ATTRIBUTE : PXCH ===> SEEN=1 COLO=410 <===                     656
         CALL AGSRESET                                                    656
         AG_SEEN = 1                                                      656
         AG_COLO = 410                                                    656
         AG_TITLE = 'PXCH'                                                656
         AG_EXNAME = 'ATTRIBUTE'                                          656
         AG_PARLIST = 'SEEN_COLO'                                         656
         CALL AXATTRIBUTE                                                 656
C    *                                                                    656
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          656
         CALL AGSRESET                                                    657
         AG_TITLE = 'CFRPMIX'                                             657
         AG_EXNAME = 'MATERIAL'                                           657
         AG_PARLIST = 'NONE'                                              657
         CALL AXMATERIAL                                                  657
C    *                                                                    657
C    Geant SHAPE : TUBS ===> RMIN=0.126 RMAX=0.15 PHI1=103.963, PHI2=216  657
C     DZ=SECTORLENGTH <===                                                657
         CALL AGSRESET                                                    659
         AG_RMIN = 0.126                                                  659
         AG_RMAX = 0.15                                                   659
         AG_PHI1 = 103.963                                                659
         AG_PHI2 = 216                                                    659
         AG_DZ = SECTORLENGTH                                             659
         AG_TITLE = 'TUBS'                                                659
         AG_EXNAME = 'SHAPE'                                              659
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            659
         CALL AXSHAPE                                                     659
C    *                                                                    659
5411  CALL AGSPOPB                                                        660
      GO TO BLOCK_PXCH, (5410 ,5420,6730)                                 660
C End of Local Procedure                                                  660
C ----------------------------------------------------------------------  661
C ------------ PXCB                                                       661
C Begin Local Procedure PXCB                                              662
C PXCB ARC BOTTOM LEFT                                                    662
5430  CONTINUE                                                            662
         AG_TITLE='PXCB'                                                  662
         AG_MODULE=MODULE                                                 662
         AG_MARK='V00'                                                    662
         CALL AGSPUSH                                                     662
         AG_MODULE=MODULE                                                 662
C    *                                                                    662
C    Geant ATTRIBUTE : PXCB ===> SEEN=1 COLO=410 <===                     663
         CALL AGSRESET                                                    663
         AG_SEEN = 1                                                      663
         AG_COLO = 410                                                    663
         AG_TITLE = 'PXCB'                                                663
         AG_EXNAME = 'ATTRIBUTE'                                          663
         AG_PARLIST = 'SEEN_COLO'                                         663
         CALL AXATTRIBUTE                                                 663
C    *                                                                    663
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          663
         CALL AGSRESET                                                    664
         AG_TITLE = 'CFRPMIX'                                             664
         AG_EXNAME = 'MATERIAL'                                           664
         AG_PARLIST = 'NONE'                                              664
         CALL AXMATERIAL                                                  664
C    *                                                                    664
C    Geant SHAPE : TUBS ===> RMIN=0.20795 RMAX=0.23195 PHI1=216, PHI2=26  664
C    7.396 DZ=SECTORLENGTH <===                                           664
         CALL AGSRESET                                                    666
         AG_RMIN = 0.20795                                                666
         AG_RMAX = 0.23195                                                666
         AG_PHI1 = 216                                                    666
         AG_PHI2 = 267.396                                                666
         AG_DZ = SECTORLENGTH                                             666
         AG_TITLE = 'TUBS'                                                666
         AG_EXNAME = 'SHAPE'                                              666
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            666
         CALL AXSHAPE                                                     666
C    *                                                                    666
5431  CALL AGSPOPB                                                        667
      GO TO BLOCK_PXCB, (5430 ,5440,6740)                                 667
C End of Local Procedure                                                  667
C ----------------------------------------------------------------------  668
C ------------ PXRB                                                       668
C Begin Local Procedure PXRB                                              669
C PXRB SECTOR RIGHT SIDE                                                  669
5450  CONTINUE                                                            669
         AG_TITLE='PXRB'                                                  669
         AG_MODULE=MODULE                                                 669
         AG_MARK='V00'                                                    669
         CALL AGSPUSH                                                     669
         AG_MODULE=MODULE                                                 669
C    *                                                                    669
C    Geant ATTRIBUTE : PXRB ===> SEEN=1 COLO=410 <===                     670
         CALL AGSRESET                                                    670
         AG_SEEN = 1                                                      670
         AG_COLO = 410                                                    670
         AG_TITLE = 'PXRB'                                                670
         AG_EXNAME = 'ATTRIBUTE'                                          670
         AG_PARLIST = 'SEEN_COLO'                                         670
         CALL AXATTRIBUTE                                                 670
C    *                                                                    670
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          670
         CALL AGSRESET                                                    671
         AG_TITLE = 'CFRPMIX'                                             671
         AG_EXNAME = 'MATERIAL'                                           671
         AG_PARLIST = 'NONE'                                              671
         CALL AXMATERIAL                                                  671
C    *                                                                    671
C    Geant SHAPE : BOX ===> DX=2.52098 DY=SECTORHALFTHICKNESS, DZ=SECTOR  671
C    LENGTH <===                                                          671
         CALL AGSRESET                                                    673
         AG_DX = 2.52098                                                  673
         AG_DY = SECTORHALFTHICKNESS                                      673
         AG_DZ = SECTORLENGTH                                             673
         AG_TITLE = 'BOX'                                                 673
         AG_EXNAME = 'SHAPE'                                              673
         AG_PARLIST = 'DX_DY_DZ'                                          673
         CALL AXSHAPE                                                     673
C    *                                                                    673
5451  CALL AGSPOPB                                                        674
      GO TO BLOCK_PXRB, (5450 ,5460,6750)                                 674
C End of Local Procedure                                                  674
C ----------------------------------------------------------------------  675
C ------------ PXTR                                                       675
C Begin Local Procedure PXTR                                              676
C PXTR                                                                    676
5470  CONTINUE                                                            676
         AG_TITLE='PXTR'                                                  676
         AG_MODULE=MODULE                                                 676
         AG_MARK='V00'                                                    676
         CALL AGSPUSH                                                     676
         AG_MODULE=MODULE                                                 676
C    *                                                                    676
C    Geant ATTRIBUTE : PXTR ===> SEEN=1 COLO=410 <===                     677
         CALL AGSRESET                                                    677
         AG_SEEN = 1                                                      677
         AG_COLO = 410                                                    677
         AG_TITLE = 'PXTR'                                                677
         AG_EXNAME = 'ATTRIBUTE'                                          677
         AG_PARLIST = 'SEEN_COLO'                                         677
         CALL AXATTRIBUTE                                                 677
C    *                                                                    677
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          677
         CALL AGSRESET                                                    678
         AG_TITLE = 'CFRPMIX'                                             678
         AG_EXNAME = 'MATERIAL'                                           678
         AG_PARLIST = 'NONE'                                              678
         CALL AXMATERIAL                                                  678
C    *                                                                    678
C    Geant SHAPE : BOX ===> DX=0.625575 DY=SECTORHALFTHICKNESS, DZ=SECTO  678
C    RLENGTH <===                                                         678
         CALL AGSRESET                                                    680
         AG_DX = 0.625575                                                 680
         AG_DY = SECTORHALFTHICKNESS                                      680
         AG_DZ = SECTORLENGTH                                             680
         AG_TITLE = 'BOX'                                                 680
         AG_EXNAME = 'SHAPE'                                              680
         AG_PARLIST = 'DX_DY_DZ'                                          680
         CALL AXSHAPE                                                     680
C    *                                                                    680
5471  CALL AGSPOPB                                                        681
      GO TO BLOCK_PXTR, (5470 ,5480,6760)                                 681
C End of Local Procedure                                                  681
C ----------------------------------------------------------------------  682
C ------------ PXTM                                                       682
C Begin Local Procedure PXTM                                              683
C PXTM                                                                    683
5490  CONTINUE                                                            683
         AG_TITLE='PXTM'                                                  683
         AG_MODULE=MODULE                                                 683
         AG_MARK='V00'                                                    683
         CALL AGSPUSH                                                     683
         AG_MODULE=MODULE                                                 683
C    *                                                                    683
C    Geant ATTRIBUTE : PXTM ===> SEEN=1 COLO=410 <===                     684
         CALL AGSRESET                                                    684
         AG_SEEN = 1                                                      684
         AG_COLO = 410                                                    684
         AG_TITLE = 'PXTM'                                                684
         AG_EXNAME = 'ATTRIBUTE'                                          684
         AG_PARLIST = 'SEEN_COLO'                                         684
         CALL AXATTRIBUTE                                                 684
C    *                                                                    684
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          684
         CALL AGSRESET                                                    685
         AG_TITLE = 'CFRPMIX'                                             685
         AG_EXNAME = 'MATERIAL'                                           685
         AG_PARLIST = 'NONE'                                              685
         CALL AXMATERIAL                                                  685
C    *                                                                    685
C    Geant SHAPE : BOX ===> DX=0.681643 DY=SECTORHALFTHICKNESS, DZ=SECTO  685
C    RLENGTH <===                                                         685
         CALL AGSRESET                                                    687
         AG_DX = 0.681643                                                 687
         AG_DY = SECTORHALFTHICKNESS                                      687
         AG_DZ = SECTORLENGTH                                             687
         AG_TITLE = 'BOX'                                                 687
         AG_EXNAME = 'SHAPE'                                              687
         AG_PARLIST = 'DX_DY_DZ'                                          687
         CALL AXSHAPE                                                     687
C    *                                                                    687
5491  CALL AGSPOPB                                                        688
      GO TO BLOCK_PXTM, (5490 ,5500,6770)                                 688
C End of Local Procedure                                                  688
C ----------------------------------------------------------------------  689
C ------------ PXTL                                                       689
C Begin Local Procedure PXTL                                              690
C PXTL                                                                    690
5510  CONTINUE                                                            690
         AG_TITLE='PXTL'                                                  690
         AG_MODULE=MODULE                                                 690
         AG_MARK='V00'                                                    690
         CALL AGSPUSH                                                     690
         AG_MODULE=MODULE                                                 690
C    *                                                                    690
C    Geant ATTRIBUTE : PXTL ===> SEEN=1 COLO=410 <===                     691
         CALL AGSRESET                                                    691
         AG_SEEN = 1                                                      691
         AG_COLO = 410                                                    691
         AG_TITLE = 'PXTL'                                                691
         AG_EXNAME = 'ATTRIBUTE'                                          691
         AG_PARLIST = 'SEEN_COLO'                                         691
         CALL AXATTRIBUTE                                                 691
C    *                                                                    691
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          691
         CALL AGSRESET                                                    692
         AG_TITLE = 'CFRPMIX'                                             692
         AG_EXNAME = 'MATERIAL'                                           692
         AG_PARLIST = 'NONE'                                              692
         CALL AXMATERIAL                                                  692
C    *                                                                    692
C    Geant SHAPE : BOX ===> DX=0.661849 DY=SECTORHALFTHICKNESS, DZ=SECTO  692
C    RLENGTH <===                                                         692
         CALL AGSRESET                                                    694
         AG_DX = 0.661849                                                 694
         AG_DY = SECTORHALFTHICKNESS                                      694
         AG_DZ = SECTORLENGTH                                             694
         AG_TITLE = 'BOX'                                                 694
         AG_EXNAME = 'SHAPE'                                              694
         AG_PARLIST = 'DX_DY_DZ'                                          694
         CALL AXSHAPE                                                     694
C    *                                                                    694
5511  CALL AGSPOPB                                                        695
      GO TO BLOCK_PXTL, (5510 ,5520,6780)                                 695
C End of Local Procedure                                                  695
C ----------------------------------------------------------------------  696
C ------------ PXTJ                                                       696
C Begin Local Procedure PXTJ                                              697
C PXTJ                                                                    697
5530  CONTINUE                                                            697
         AG_TITLE='PXTJ'                                                  697
         AG_MODULE=MODULE                                                 697
         AG_MARK='V00'                                                    697
         CALL AGSPUSH                                                     697
         AG_MODULE=MODULE                                                 697
C    *                                                                    697
C    Geant ATTRIBUTE : PXTJ ===> SEEN=1 COLO=410 <===                     698
         CALL AGSRESET                                                    698
         AG_SEEN = 1                                                      698
         AG_COLO = 410                                                    698
         AG_TITLE = 'PXTJ'                                                698
         AG_EXNAME = 'ATTRIBUTE'                                          698
         AG_PARLIST = 'SEEN_COLO'                                         698
         CALL AXATTRIBUTE                                                 698
C    *                                                                    698
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          698
         CALL AGSRESET                                                    699
         AG_TITLE = 'CFRPMIX'                                             699
         AG_EXNAME = 'MATERIAL'                                           699
         AG_PARLIST = 'NONE'                                              699
         CALL AXMATERIAL                                                  699
C    *                                                                    699
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=SECTORHALFTHICKNESS, DZ=SECT  699
C    ORLENGTH <===                                                        699
         CALL AGSRESET                                                    701
         AG_DX = 0.0389368                                                701
         AG_DY = SECTORHALFTHICKNESS                                      701
         AG_DZ = SECTORLENGTH                                             701
         AG_TITLE = 'BOX'                                                 701
         AG_EXNAME = 'SHAPE'                                              701
         AG_PARLIST = 'DX_DY_DZ'                                          701
         CALL AXSHAPE                                                     701
C    *                                                                    701
5531  CALL AGSPOPB                                                        702
      GO TO BLOCK_PXTJ, (5530 ,5540,6790)                                 702
C End of Local Procedure                                                  702
C ----------------------------------------------------------------------  703
C ------------ PXTI                                                       703
C Begin Local Procedure PXTI                                              704
C PXTI                                                                    704
5550  CONTINUE                                                            704
         AG_TITLE='PXTI'                                                  704
         AG_MODULE=MODULE                                                 704
         AG_MARK='V00'                                                    704
         CALL AGSPUSH                                                     704
         AG_MODULE=MODULE                                                 704
C    *                                                                    704
C    Geant ATTRIBUTE : PXTI ===> SEEN=1 COLO=410 <===                     705
         CALL AGSRESET                                                    705
         AG_SEEN = 1                                                      705
         AG_COLO = 410                                                    705
         AG_TITLE = 'PXTI'                                                705
         AG_EXNAME = 'ATTRIBUTE'                                          705
         AG_PARLIST = 'SEEN_COLO'                                         705
         CALL AXATTRIBUTE                                                 705
C    *                                                                    705
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          705
         CALL AGSRESET                                                    706
         AG_TITLE = 'CFRPMIX'                                             706
         AG_EXNAME = 'MATERIAL'                                           706
         AG_PARLIST = 'NONE'                                              706
         CALL AXMATERIAL                                                  706
C    *                                                                    706
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=SECTORHALFTHICKNESS, DZ=SECT  706
C    ORLENGTH <===                                                        706
         CALL AGSRESET                                                    708
         AG_DX = 0.0389368                                                708
         AG_DY = SECTORHALFTHICKNESS                                      708
         AG_DZ = SECTORLENGTH                                             708
         AG_TITLE = 'BOX'                                                 708
         AG_EXNAME = 'SHAPE'                                              708
         AG_PARLIST = 'DX_DY_DZ'                                          708
         CALL AXSHAPE                                                     708
C    *                                                                    708
5551  CALL AGSPOPB                                                        709
      GO TO BLOCK_PXTI, (5550 ,5560,6800)                                 709
C End of Local Procedure                                                  709
C ----------------------------------------------------------------------  710
C ------------ PXLB                                                       710
C Begin Local Procedure PXLB                                              711
C PXLB SECTOR LEFT SIDE                                                   711
5570  CONTINUE                                                            711
         AG_TITLE='PXLB'                                                  711
         AG_MODULE=MODULE                                                 711
         AG_MARK='V00'                                                    711
         CALL AGSPUSH                                                     711
         AG_MODULE=MODULE                                                 711
C    *                                                                    711
C    Geant ATTRIBUTE : PXLB ===> SEEN=1 COLO=410 <===                     712
         CALL AGSRESET                                                    712
         AG_SEEN = 1                                                      712
         AG_COLO = 410                                                    712
         AG_TITLE = 'PXLB'                                                712
         AG_EXNAME = 'ATTRIBUTE'                                          712
         AG_PARLIST = 'SEEN_COLO'                                         712
         CALL AXATTRIBUTE                                                 712
C    *                                                                    712
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          712
         CALL AGSRESET                                                    713
         AG_TITLE = 'CFRPMIX'                                             713
         AG_EXNAME = 'MATERIAL'                                           713
         AG_PARLIST = 'NONE'                                              713
         CALL AXMATERIAL                                                  713
C    *                                                                    713
C    Geant SHAPE : BOX ===> DX=2.39557 DY=SECTORHALFTHICKNESS, DZ=SECTOR  713
C    LENGTH <===                                                          713
         CALL AGSRESET                                                    715
         AG_DX = 2.39557                                                  715
         AG_DY = SECTORHALFTHICKNESS                                      715
         AG_DZ = SECTORLENGTH                                             715
         AG_TITLE = 'BOX'                                                 715
         AG_EXNAME = 'SHAPE'                                              715
         AG_PARLIST = 'DX_DY_DZ'                                          715
         CALL AXSHAPE                                                     715
C    *                                                                    715
5571  CALL AGSPOPB                                                        716
      GO TO BLOCK_PXLB, (5570 ,5580,6810)                                 716
C End of Local Procedure                                                  716
C ----------------------------------------------------------------------  717
C ------------ PXIB                                                       717
C Begin Local Procedure PXIB                                              718
C PXIB SECTOR INNER SIDE                                                  718
5590  CONTINUE                                                            718
         AG_TITLE='PXIB'                                                  718
         AG_MODULE=MODULE                                                 718
         AG_MARK='V00'                                                    718
         CALL AGSPUSH                                                     718
         AG_MODULE=MODULE                                                 718
C    *                                                                    718
C    Geant ATTRIBUTE : PXIB ===> SEEN=1 COLO=410 <===                     719
         CALL AGSRESET                                                    719
         AG_SEEN = 1                                                      719
         AG_COLO = 410                                                    719
         AG_TITLE = 'PXIB'                                                719
         AG_EXNAME = 'ATTRIBUTE'                                          719
         AG_PARLIST = 'SEEN_COLO'                                         719
         CALL AXATTRIBUTE                                                 719
C    *                                                                    719
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          719
         CALL AGSRESET                                                    720
         AG_TITLE = 'CFRPMIX'                                             720
         AG_EXNAME = 'MATERIAL'                                           720
         AG_PARLIST = 'NONE'                                              720
         CALL AXMATERIAL                                                  720
C    *                                                                    720
C    Geant SHAPE : BOX ===> DX=0.720682 DY=SECTORHALFTHICKNESS, DZ=SECTO  720
C    RLENGTH <===                                                         720
         CALL AGSRESET                                                    722
         AG_DX = 0.720682                                                 722
         AG_DY = SECTORHALFTHICKNESS                                      722
         AG_DZ = SECTORLENGTH                                             722
         AG_TITLE = 'BOX'                                                 722
         AG_EXNAME = 'SHAPE'                                              722
         AG_PARLIST = 'DX_DY_DZ'                                          722
         CALL AXSHAPE                                                     722
C    *                                                                    722
5591  CALL AGSPOPB                                                        723
      GO TO BLOCK_PXIB, (5590 ,5600,6820)                                 723
C End of Local Procedure                                                  723
C ----------------------------------------------------------------------  724
C ------------ DTCA                                                       724
C Begin Local Procedure DTCA                                              725
C DTCA                                                                    725
5610  CONTINUE                                                            725
         AG_TITLE='DTCA'                                                  725
         AG_MODULE=MODULE                                                 725
         AG_MARK='V00'                                                    725
         CALL AGSPUSH                                                     725
         AG_MODULE=MODULE                                                 725
C    *                                                                    725
C    Geant ATTRIBUTE : DTCA ===> SEEN=1 COLO=920 <===                     726
         CALL AGSRESET                                                    726
         AG_SEEN = 1                                                      726
         AG_COLO = 920                                                    726
         AG_TITLE = 'DTCA'                                                726
         AG_EXNAME = 'ATTRIBUTE'                                          726
         AG_PARLIST = 'SEEN_COLO'                                         726
         CALL AXATTRIBUTE                                                 726
C    *                                                                    726
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        726
         CALL AGSRESET                                                    727
         AG_TITLE = 'ALUMINIUM'                                           727
         AG_EXNAME = 'MATERIAL'                                           727
         AG_PARLIST = 'NONE'                                              727
         CALL AXMATERIAL                                                  727
C    *                                                                    727
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.126 PHI1=267.396, PHI2=36  727
C    0 DZ=DOVETAILLENGTH <===                                             727
         CALL AGSRESET                                                    729
         AG_RMIN = 0.026                                                  729
         AG_RMAX = 0.126                                                  729
         AG_PHI1 = 267.396                                                729
         AG_PHI2 = 360                                                    729
         AG_DZ = DOVETAILLENGTH                                           729
         AG_TITLE = 'TUBS'                                                729
         AG_EXNAME = 'SHAPE'                                              729
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            729
         CALL AXSHAPE                                                     729
C    *                                                                    729
5611  CALL AGSPOPB                                                        730
      GO TO BLOCK_DTCA, (5610 ,5620,6830)                                 730
C End of Local Procedure                                                  730
C ----------------------------------------------------------------------  731
C ------------ DTCB                                                       731
C Begin Local Procedure DTCB                                              732
C DTCB                                                                    732
5630  CONTINUE                                                            732
         AG_TITLE='DTCB'                                                  732
         AG_MODULE=MODULE                                                 732
         AG_MARK='V00'                                                    732
         CALL AGSPUSH                                                     732
         AG_MODULE=MODULE                                                 732
C    *                                                                    732
C    Geant ATTRIBUTE : DTCB ===> SEEN=1 COLO=920 <===                     733
         CALL AGSRESET                                                    733
         AG_SEEN = 1                                                      733
         AG_COLO = 920                                                    733
         AG_TITLE = 'DTCB'                                                733
         AG_EXNAME = 'ATTRIBUTE'                                          733
         AG_PARLIST = 'SEEN_COLO'                                         733
         CALL AXATTRIBUTE                                                 733
C    *                                                                    733
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        733
         CALL AGSRESET                                                    734
         AG_TITLE = 'ALUMINIUM'                                           734
         AG_EXNAME = 'MATERIAL'                                           734
         AG_PARLIST = 'NONE'                                              734
         CALL AXMATERIAL                                                  734
C    *                                                                    734
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.126 PHI1=0, PHI2=79.963 D  734
C    Z=DOVETAILLENGTH <===                                                734
         CALL AGSRESET                                                    736
         AG_RMIN = 0.026                                                  736
         AG_RMAX = 0.126                                                  736
         AG_PHI1 = 0                                                      736
         AG_PHI2 = 79.963                                                 736
         AG_DZ = DOVETAILLENGTH                                           736
         AG_TITLE = 'TUBS'                                                736
         AG_EXNAME = 'SHAPE'                                              736
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            736
         CALL AXSHAPE                                                     736
C    *                                                                    736
5631  CALL AGSPOPB                                                        737
      GO TO BLOCK_DTCB, (5630 ,5640,6840)                                 737
C End of Local Procedure                                                  737
C ----------------------------------------------------------------------  738
C ------------ DTCC                                                       738
C Begin Local Procedure DTCC                                              739
C DTCC                                                                    739
5650  CONTINUE                                                            739
         AG_TITLE='DTCC'                                                  739
         AG_MODULE=MODULE                                                 739
         AG_MARK='V00'                                                    739
         CALL AGSPUSH                                                     739
         AG_MODULE=MODULE                                                 739
C    *                                                                    739
C    Geant ATTRIBUTE : DTCC ===> SEEN=1 COLO=920 <===                     740
         CALL AGSRESET                                                    740
         AG_SEEN = 1                                                      740
         AG_COLO = 920                                                    740
         AG_TITLE = 'DTCC'                                                740
         AG_EXNAME = 'ATTRIBUTE'                                          740
         AG_PARLIST = 'SEEN_COLO'                                         740
         CALL AXATTRIBUTE                                                 740
C    *                                                                    740
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        740
         CALL AGSRESET                                                    741
         AG_TITLE = 'ALUMINIUM'                                           741
         AG_EXNAME = 'MATERIAL'                                           741
         AG_PARLIST = 'NONE'                                              741
         CALL AXMATERIAL                                                  741
C    *                                                                    741
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.126 PHI1=79.963, PHI2=191  741
C    .554 DZ=DOVETAILLENGTH <===                                          741
         CALL AGSRESET                                                    743
         AG_RMIN = 0.026                                                  743
         AG_RMAX = 0.126                                                  743
         AG_PHI1 = 79.963                                                 743
         AG_PHI2 = 191.554                                                743
         AG_DZ = DOVETAILLENGTH                                           743
         AG_TITLE = 'TUBS'                                                743
         AG_EXNAME = 'SHAPE'                                              743
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            743
         CALL AXSHAPE                                                     743
C    *                                                                    743
5651  CALL AGSPOPB                                                        744
      GO TO BLOCK_DTCC, (5650 ,5660,6850)                                 744
C End of Local Procedure                                                  744
C ----------------------------------------------------------------------  745
C ------------ DTCD                                                       745
C Begin Local Procedure DTCD                                              746
C DTCD                                                                    746
5670  CONTINUE                                                            746
         AG_TITLE='DTCD'                                                  746
         AG_MODULE=MODULE                                                 746
         AG_MARK='V00'                                                    746
         CALL AGSPUSH                                                     746
         AG_MODULE=MODULE                                                 746
C    *                                                                    746
C    Geant ATTRIBUTE : DTCD ===> SEEN=1 COLO=920 <===                     747
         CALL AGSRESET                                                    747
         AG_SEEN = 1                                                      747
         AG_COLO = 920                                                    747
         AG_TITLE = 'DTCD'                                                747
         AG_EXNAME = 'ATTRIBUTE'                                          747
         AG_PARLIST = 'SEEN_COLO'                                         747
         CALL AXATTRIBUTE                                                 747
C    *                                                                    747
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        747
         CALL AGSRESET                                                    748
         AG_TITLE = 'ALUMINIUM'                                           748
         AG_EXNAME = 'MATERIAL'                                           748
         AG_PARLIST = 'NONE'                                              748
         CALL AXMATERIAL                                                  748
C    *                                                                    748
C    Geant SHAPE : TUBS ===> RMIN=0.174 RMAX=0.274 PHI1=271.963, PHI2=37  748
C    1.554 DZ=DOVETAILLENGTH <===                                         748
         CALL AGSRESET                                                    750
         AG_RMIN = 0.174                                                  750
         AG_RMAX = 0.274                                                  750
         AG_PHI1 = 271.963                                                750
         AG_PHI2 = 371.554                                                750
         AG_DZ = DOVETAILLENGTH                                           750
         AG_TITLE = 'TUBS'                                                750
         AG_EXNAME = 'SHAPE'                                              750
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            750
         CALL AXSHAPE                                                     750
C    *                                                                    750
5671  CALL AGSPOPB                                                        751
      GO TO BLOCK_DTCD, (5670 ,5680,6860)                                 751
C End of Local Procedure                                                  751
C ----------------------------------------------------------------------  752
C ------------ DTCE                                                       752
C Begin Local Procedure DTCE                                              753
C DTCE                                                                    753
5690  CONTINUE                                                            753
         AG_TITLE='DTCE'                                                  753
         AG_MODULE=MODULE                                                 753
         AG_MARK='V00'                                                    753
         CALL AGSPUSH                                                     753
         AG_MODULE=MODULE                                                 753
C    *                                                                    753
C    Geant ATTRIBUTE : DTCE ===> SEEN=1 COLO=920 <===                     754
         CALL AGSRESET                                                    754
         AG_SEEN = 1                                                      754
         AG_COLO = 920                                                    754
         AG_TITLE = 'DTCE'                                                754
         AG_EXNAME = 'ATTRIBUTE'                                          754
         AG_PARLIST = 'SEEN_COLO'                                         754
         CALL AXATTRIBUTE                                                 754
C    *                                                                    754
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        754
         CALL AGSRESET                                                    755
         AG_TITLE = 'ALUMINIUM'                                           755
         AG_EXNAME = 'MATERIAL'                                           755
         AG_PARLIST = 'NONE'                                              755
         CALL AXMATERIAL                                                  755
C    *                                                                    755
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.126 PHI1=91.963, PHI2=203  755
C    .554 DZ=DOVETAILLENGTH <===                                          755
         CALL AGSRESET                                                    757
         AG_RMIN = 0.026                                                  757
         AG_RMAX = 0.126                                                  757
         AG_PHI1 = 91.963                                                 757
         AG_PHI2 = 203.554                                                757
         AG_DZ = DOVETAILLENGTH                                           757
         AG_TITLE = 'TUBS'                                                757
         AG_EXNAME = 'SHAPE'                                              757
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            757
         CALL AXSHAPE                                                     757
C    *                                                                    757
5691  CALL AGSPOPB                                                        758
      GO TO BLOCK_DTCE, (5690 ,5700,6870)                                 758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ DTCF                                                       759
C Begin Local Procedure DTCF                                              760
C DTCF                                                                    760
5710  CONTINUE                                                            760
         AG_TITLE='DTCF'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant ATTRIBUTE : DTCF ===> SEEN=1 COLO=920 <===                     761
         CALL AGSRESET                                                    761
         AG_SEEN = 1                                                      761
         AG_COLO = 920                                                    761
         AG_TITLE = 'DTCF'                                                761
         AG_EXNAME = 'ATTRIBUTE'                                          761
         AG_PARLIST = 'SEEN_COLO'                                         761
         CALL AXATTRIBUTE                                                 761
C    *                                                                    761
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        761
         CALL AGSRESET                                                    762
         AG_TITLE = 'ALUMINIUM'                                           762
         AG_EXNAME = 'MATERIAL'                                           762
         AG_PARLIST = 'NONE'                                              762
         CALL AXMATERIAL                                                  762
C    *                                                                    762
C    Geant SHAPE : TUBS ===> RMIN=0.174 RMAX=0.274 PHI1=283.963, PHI2=38  762
C    3.554 DZ=DOVETAILLENGTH <===                                         762
         CALL AGSRESET                                                    764
         AG_RMIN = 0.174                                                  764
         AG_RMAX = 0.274                                                  764
         AG_PHI1 = 283.963                                                764
         AG_PHI2 = 383.554                                                764
         AG_DZ = DOVETAILLENGTH                                           764
         AG_TITLE = 'TUBS'                                                764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            764
         CALL AXSHAPE                                                     764
C    *                                                                    764
5711  CALL AGSPOPB                                                        765
      GO TO BLOCK_DTCF, (5710 ,5720,6880)                                 765
C End of Local Procedure                                                  765
C ----------------------------------------------------------------------  766
C ------------ DTCG                                                       766
C Begin Local Procedure DTCG                                              767
C DTCG                                                                    767
5730  CONTINUE                                                            767
         AG_TITLE='DTCG'                                                  767
         AG_MODULE=MODULE                                                 767
         AG_MARK='V00'                                                    767
         CALL AGSPUSH                                                     767
         AG_MODULE=MODULE                                                 767
C    *                                                                    767
C    Geant ATTRIBUTE : DTCG ===> SEEN=1 COLO=920 <===                     768
         CALL AGSRESET                                                    768
         AG_SEEN = 1                                                      768
         AG_COLO = 920                                                    768
         AG_TITLE = 'DTCG'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO'                                         768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        768
         CALL AGSRESET                                                    769
         AG_TITLE = 'ALUMINIUM'                                           769
         AG_EXNAME = 'MATERIAL'                                           769
         AG_PARLIST = 'NONE'                                              769
         CALL AXMATERIAL                                                  769
C    *                                                                    769
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.126 PHI1=103.963, PHI2=21  769
C    6 DZ=DOVETAILLENGTH <===                                             769
         CALL AGSRESET                                                    771
         AG_RMIN = 0.026                                                  771
         AG_RMAX = 0.126                                                  771
         AG_PHI1 = 103.963                                                771
         AG_PHI2 = 216                                                    771
         AG_DZ = DOVETAILLENGTH                                           771
         AG_TITLE = 'TUBS'                                                771
         AG_EXNAME = 'SHAPE'                                              771
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            771
         CALL AXSHAPE                                                     771
C    *                                                                    771
5731  CALL AGSPOPB                                                        772
      GO TO BLOCK_DTCG, (5730 ,5740,6890)                                 772
C End of Local Procedure                                                  772
C ----------------------------------------------------------------------  773
C ------------ DTCH                                                       773
C Begin Local Procedure DTCH                                              774
C DTCH                                                                    774
5750  CONTINUE                                                            774
         AG_TITLE='DTCH'                                                  774
         AG_MODULE=MODULE                                                 774
         AG_MARK='V00'                                                    774
         CALL AGSPUSH                                                     774
         AG_MODULE=MODULE                                                 774
C    *                                                                    774
C    Geant ATTRIBUTE : DTCH ===> SEEN=1 COLO=920 <===                     775
         CALL AGSRESET                                                    775
         AG_SEEN = 1                                                      775
         AG_COLO = 920                                                    775
         AG_TITLE = 'DTCH'                                                775
         AG_EXNAME = 'ATTRIBUTE'                                          775
         AG_PARLIST = 'SEEN_COLO'                                         775
         CALL AXATTRIBUTE                                                 775
C    *                                                                    775
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        775
         CALL AGSRESET                                                    776
         AG_TITLE = 'ALUMINIUM'                                           776
         AG_EXNAME = 'MATERIAL'                                           776
         AG_PARLIST = 'NONE'                                              776
         CALL AXMATERIAL                                                  776
C    *                                                                    776
C    Geant SHAPE : TUBS ===> RMIN=0.10795 RMAX=0.20795 PHI1=216, PHI2=26  776
C    7.396 DZ=DOVETAILLENGTH <===                                         776
         CALL AGSRESET                                                    778
         AG_RMIN = 0.10795                                                778
         AG_RMAX = 0.20795                                                778
         AG_PHI1 = 216                                                    778
         AG_PHI2 = 267.396                                                778
         AG_DZ = DOVETAILLENGTH                                           778
         AG_TITLE = 'TUBS'                                                778
         AG_EXNAME = 'SHAPE'                                              778
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            778
         CALL AXSHAPE                                                     778
C    *                                                                    778
5751  CALL AGSPOPB                                                        779
      GO TO BLOCK_DTCH, (5750 ,5760,6900)                                 779
C End of Local Procedure                                                  779
C ----------------------------------------------------------------------  780
C ------------ DTSA                                                       780
C Begin Local Procedure DTSA                                              781
C DTSA DOVETAIL SECTOR RIGHT SIDE                                         781
5770  CONTINUE                                                            781
         AG_TITLE='DTSA'                                                  781
         AG_MODULE=MODULE                                                 781
         AG_MARK='V00'                                                    781
         CALL AGSPUSH                                                     781
         AG_MODULE=MODULE                                                 781
C    *                                                                    781
C    Geant ATTRIBUTE : DTSA ===> SEEN=1 COLO=920 <===                     782
         CALL AGSRESET                                                    782
         AG_SEEN = 1                                                      782
         AG_COLO = 920                                                    782
         AG_TITLE = 'DTSA'                                                782
         AG_EXNAME = 'ATTRIBUTE'                                          782
         AG_PARLIST = 'SEEN_COLO'                                         782
         CALL AXATTRIBUTE                                                 782
C    *                                                                    782
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        782
         CALL AGSRESET                                                    783
         AG_TITLE = 'ALUMINIUM'                                           783
         AG_EXNAME = 'MATERIAL'                                           783
         AG_PARLIST = 'NONE'                                              783
         CALL AXMATERIAL                                                  783
C    *                                                                    783
C    Geant SHAPE : BOX ===> DX=2.52098 DY=DOVETAILHALFTHICKNESS, DZ=DOVE  783
C    TAILLENGTH <===                                                      783
         CALL AGSRESET                                                    785
         AG_DX = 2.52098                                                  785
         AG_DY = DOVETAILHALFTHICKNESS                                    785
         AG_DZ = DOVETAILLENGTH                                           785
         AG_TITLE = 'BOX'                                                 785
         AG_EXNAME = 'SHAPE'                                              785
         AG_PARLIST = 'DX_DY_DZ'                                          785
         CALL AXSHAPE                                                     785
C    *                                                                    785
5771  CALL AGSPOPB                                                        786
      GO TO BLOCK_DTSA, (5770 ,5780,6910)                                 786
C End of Local Procedure                                                  786
C ----------------------------------------------------------------------  787
C ------------ DTSB                                                       787
C Begin Local Procedure DTSB                                              788
C DTSB DOVETAIL SECTOR RIGHT UPPER SIDE                                   788
5790  CONTINUE                                                            788
         AG_TITLE='DTSB'                                                  788
         AG_MODULE=MODULE                                                 788
         AG_MARK='V00'                                                    788
         CALL AGSPUSH                                                     788
         AG_MODULE=MODULE                                                 788
C    *                                                                    788
C    Geant ATTRIBUTE : DTSB ===> SEEN=1 COLO=920 <===                     789
         CALL AGSRESET                                                    789
         AG_SEEN = 1                                                      789
         AG_COLO = 920                                                    789
         AG_TITLE = 'DTSB'                                                789
         AG_EXNAME = 'ATTRIBUTE'                                          789
         AG_PARLIST = 'SEEN_COLO'                                         789
         CALL AXATTRIBUTE                                                 789
C    *                                                                    789
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        789
         CALL AGSRESET                                                    790
         AG_TITLE = 'ALUMINIUM'                                           790
         AG_EXNAME = 'MATERIAL'                                           790
         AG_PARLIST = 'NONE'                                              790
         CALL AXMATERIAL                                                  790
C    *                                                                    790
C    Geant SHAPE : BOX ===> DX=0.625575 DY=DOVETAILHALFTHICKNESS, DZ=DOV  790
C    ETAILLENGTH <===                                                     790
         CALL AGSRESET                                                    792
         AG_DX = 0.625575                                                 792
         AG_DY = DOVETAILHALFTHICKNESS                                    792
         AG_DZ = DOVETAILLENGTH                                           792
         AG_TITLE = 'BOX'                                                 792
         AG_EXNAME = 'SHAPE'                                              792
         AG_PARLIST = 'DX_DY_DZ'                                          792
         CALL AXSHAPE                                                     792
C    *                                                                    792
5791  CALL AGSPOPB                                                        793
      GO TO BLOCK_DTSB, (5790 ,5800,6920)                                 793
C End of Local Procedure                                                  793
C ----------------------------------------------------------------------  794
C ------------ DTSC                                                       794
C Begin Local Procedure DTSC                                              795
C DTSC DOVETAIL SECTOR RIGHT UPPER SIDE                                   795
5810  CONTINUE                                                            795
         AG_TITLE='DTSC'                                                  795
         AG_MODULE=MODULE                                                 795
         AG_MARK='V00'                                                    795
         CALL AGSPUSH                                                     795
         AG_MODULE=MODULE                                                 795
C    *                                                                    795
C    Geant ATTRIBUTE : DTSC ===> SEEN=1 COLO=920 <===                     796
         CALL AGSRESET                                                    796
         AG_SEEN = 1                                                      796
         AG_COLO = 920                                                    796
         AG_TITLE = 'DTSC'                                                796
         AG_EXNAME = 'ATTRIBUTE'                                          796
         AG_PARLIST = 'SEEN_COLO'                                         796
         CALL AXATTRIBUTE                                                 796
C    *                                                                    796
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        796
         CALL AGSRESET                                                    797
         AG_TITLE = 'ALUMINIUM'                                           797
         AG_EXNAME = 'MATERIAL'                                           797
         AG_PARLIST = 'NONE'                                              797
         CALL AXMATERIAL                                                  797
C    *                                                                    797
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILHALFTHICKNESS, DZ=DO  797
C    VETAILLENGTH <===                                                    797
         CALL AGSRESET                                                    799
         AG_DX = 0.0389368                                                799
         AG_DY = DOVETAILHALFTHICKNESS                                    799
         AG_DZ = DOVETAILLENGTH                                           799
         AG_TITLE = 'BOX'                                                 799
         AG_EXNAME = 'SHAPE'                                              799
         AG_PARLIST = 'DX_DY_DZ'                                          799
         CALL AXSHAPE                                                     799
C    *                                                                    799
5811  CALL AGSPOPB                                                        800
      GO TO BLOCK_DTSC, (5810 ,5820,6930)                                 800
C End of Local Procedure                                                  800
C ----------------------------------------------------------------------  801
C ------------ DTSD                                                       801
C Begin Local Procedure DTSD                                              802
C DTSD DOVETAIL SECTOR RIGHT UPPER SIDE                                   802
5830  CONTINUE                                                            802
         AG_TITLE='DTSD'                                                  802
         AG_MODULE=MODULE                                                 802
         AG_MARK='V00'                                                    802
         CALL AGSPUSH                                                     802
         AG_MODULE=MODULE                                                 802
C    *                                                                    802
C    Geant ATTRIBUTE : DTSD ===> SEEN=1 COLO=920 <===                     803
         CALL AGSRESET                                                    803
         AG_SEEN = 1                                                      803
         AG_COLO = 920                                                    803
         AG_TITLE = 'DTSD'                                                803
         AG_EXNAME = 'ATTRIBUTE'                                          803
         AG_PARLIST = 'SEEN_COLO'                                         803
         CALL AXATTRIBUTE                                                 803
C    *                                                                    803
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        803
         CALL AGSRESET                                                    804
         AG_TITLE = 'ALUMINIUM'                                           804
         AG_EXNAME = 'MATERIAL'                                           804
         AG_PARLIST = 'NONE'                                              804
         CALL AXMATERIAL                                                  804
C    *                                                                    804
C    Geant SHAPE : BOX ===> DX=0.681643 DY=DOVETAILHALFTHICKNESS, DZ=DOV  804
C    ETAILLENGTH <===                                                     804
         CALL AGSRESET                                                    806
         AG_DX = 0.681643                                                 806
         AG_DY = DOVETAILHALFTHICKNESS                                    806
         AG_DZ = DOVETAILLENGTH                                           806
         AG_TITLE = 'BOX'                                                 806
         AG_EXNAME = 'SHAPE'                                              806
         AG_PARLIST = 'DX_DY_DZ'                                          806
         CALL AXSHAPE                                                     806
C    *                                                                    806
5831  CALL AGSPOPB                                                        807
      GO TO BLOCK_DTSD, (5830 ,5840,6940)                                 807
C End of Local Procedure                                                  807
C ----------------------------------------------------------------------  808
C ------------ DTSE                                                       808
C Begin Local Procedure DTSE                                              809
C DTSE DOVETAIL SECTOR RIGHT UPPER SIDE                                   809
5850  CONTINUE                                                            809
         AG_TITLE='DTSE'                                                  809
         AG_MODULE=MODULE                                                 809
         AG_MARK='V00'                                                    809
         CALL AGSPUSH                                                     809
         AG_MODULE=MODULE                                                 809
C    *                                                                    809
C    Geant ATTRIBUTE : DTSE ===> SEEN=1 COLO=920 <===                     810
         CALL AGSRESET                                                    810
         AG_SEEN = 1                                                      810
         AG_COLO = 920                                                    810
         AG_TITLE = 'DTSE'                                                810
         AG_EXNAME = 'ATTRIBUTE'                                          810
         AG_PARLIST = 'SEEN_COLO'                                         810
         CALL AXATTRIBUTE                                                 810
C    *                                                                    810
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        810
         CALL AGSRESET                                                    811
         AG_TITLE = 'ALUMINIUM'                                           811
         AG_EXNAME = 'MATERIAL'                                           811
         AG_PARLIST = 'NONE'                                              811
         CALL AXMATERIAL                                                  811
C    *                                                                    811
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILHALFTHICKNESS, DZ=DO  811
C    VETAILLENGTH <===                                                    811
         CALL AGSRESET                                                    813
         AG_DX = 0.0389368                                                813
         AG_DY = DOVETAILHALFTHICKNESS                                    813
         AG_DZ = DOVETAILLENGTH                                           813
         AG_TITLE = 'BOX'                                                 813
         AG_EXNAME = 'SHAPE'                                              813
         AG_PARLIST = 'DX_DY_DZ'                                          813
         CALL AXSHAPE                                                     813
C    *                                                                    813
5851  CALL AGSPOPB                                                        814
      GO TO BLOCK_DTSE, (5850 ,5860,6950)                                 814
C End of Local Procedure                                                  814
C ----------------------------------------------------------------------  815
C ------------ DTSF                                                       815
C Begin Local Procedure DTSF                                              816
C DTSF DOVETAIL SECTOR RIGHT UPPER SIDE                                   816
5870  CONTINUE                                                            816
         AG_TITLE='DTSF'                                                  816
         AG_MODULE=MODULE                                                 816
         AG_MARK='V00'                                                    816
         CALL AGSPUSH                                                     816
         AG_MODULE=MODULE                                                 816
C    *                                                                    816
C    Geant ATTRIBUTE : DTSF ===> SEEN=1 COLO=920 <===                     817
         CALL AGSRESET                                                    817
         AG_SEEN = 1                                                      817
         AG_COLO = 920                                                    817
         AG_TITLE = 'DTSF'                                                817
         AG_EXNAME = 'ATTRIBUTE'                                          817
         AG_PARLIST = 'SEEN_COLO'                                         817
         CALL AXATTRIBUTE                                                 817
C    *                                                                    817
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        817
         CALL AGSRESET                                                    818
         AG_TITLE = 'ALUMINIUM'                                           818
         AG_EXNAME = 'MATERIAL'                                           818
         AG_PARLIST = 'NONE'                                              818
         CALL AXMATERIAL                                                  818
C    *                                                                    818
C    Geant SHAPE : BOX ===> DX=0.661849 DY=DOVETAILHALFTHICKNESS, DZ=DOV  818
C    ETAILLENGTH <===                                                     818
         CALL AGSRESET                                                    820
         AG_DX = 0.661849                                                 820
         AG_DY = DOVETAILHALFTHICKNESS                                    820
         AG_DZ = DOVETAILLENGTH                                           820
         AG_TITLE = 'BOX'                                                 820
         AG_EXNAME = 'SHAPE'                                              820
         AG_PARLIST = 'DX_DY_DZ'                                          820
         CALL AXSHAPE                                                     820
C    *                                                                    820
5871  CALL AGSPOPB                                                        821
      GO TO BLOCK_DTSF, (5870 ,5880,6960)                                 821
C End of Local Procedure                                                  821
C ----------------------------------------------------------------------  822
C ------------ DTSG                                                       822
C Begin Local Procedure DTSG                                              823
C DTSG DOVETAIL SECTOR RIGHT UPPER SIDE                                   823
5890  CONTINUE                                                            823
         AG_TITLE='DTSG'                                                  823
         AG_MODULE=MODULE                                                 823
         AG_MARK='V00'                                                    823
         CALL AGSPUSH                                                     823
         AG_MODULE=MODULE                                                 823
C    *                                                                    823
C    Geant ATTRIBUTE : DTSG ===> SEEN=1 COLO=920 <===                     824
         CALL AGSRESET                                                    824
         AG_SEEN = 1                                                      824
         AG_COLO = 920                                                    824
         AG_TITLE = 'DTSG'                                                824
         AG_EXNAME = 'ATTRIBUTE'                                          824
         AG_PARLIST = 'SEEN_COLO'                                         824
         CALL AXATTRIBUTE                                                 824
C    *                                                                    824
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        824
         CALL AGSRESET                                                    825
         AG_TITLE = 'ALUMINIUM'                                           825
         AG_EXNAME = 'MATERIAL'                                           825
         AG_PARLIST = 'NONE'                                              825
         CALL AXMATERIAL                                                  825
C    *                                                                    825
C    Geant SHAPE : BOX ===> DX=2.39557 DY=DOVETAILHALFTHICKNESS, DZ=DOVE  825
C    TAILLENGTH <===                                                      825
         CALL AGSRESET                                                    827
         AG_DX = 2.39557                                                  827
         AG_DY = DOVETAILHALFTHICKNESS                                    827
         AG_DZ = DOVETAILLENGTH                                           827
         AG_TITLE = 'BOX'                                                 827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'DX_DY_DZ'                                          827
         CALL AXSHAPE                                                     827
C    *                                                                    827
5891  CALL AGSPOPB                                                        828
      GO TO BLOCK_DTSG, (5890 ,5900,6970)                                 828
C End of Local Procedure                                                  828
C ----------------------------------------------------------------------  829
C ------------ DTSH                                                       829
C Begin Local Procedure DTSH                                              830
C DTSH DOVETAIL SECTOR RIGHT UPPER SIDE                                   830
5910  CONTINUE                                                            830
         AG_TITLE='DTSH'                                                  830
         AG_MODULE=MODULE                                                 830
         AG_MARK='V00'                                                    830
         CALL AGSPUSH                                                     830
         AG_MODULE=MODULE                                                 830
C    *                                                                    830
C    Geant ATTRIBUTE : DTSH ===> SEEN=1 COLO=920 <===                     831
         CALL AGSRESET                                                    831
         AG_SEEN = 1                                                      831
         AG_COLO = 920                                                    831
         AG_TITLE = 'DTSH'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO'                                         831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        831
         CALL AGSRESET                                                    832
         AG_TITLE = 'ALUMINIUM'                                           832
         AG_EXNAME = 'MATERIAL'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXMATERIAL                                                  832
C    *                                                                    832
C    Geant SHAPE : BOX ===> DX=0.721081 DY=DOVETAILHALFTHICKNESS, DZ=DOV  832
C    ETAILLENGTH <===                                                     832
         CALL AGSRESET                                                    834
         AG_DX = 0.721081                                                 834
         AG_DY = DOVETAILHALFTHICKNESS                                    834
         AG_DZ = DOVETAILLENGTH                                           834
         AG_TITLE = 'BOX'                                                 834
         AG_EXNAME = 'SHAPE'                                              834
         AG_PARLIST = 'DX_DY_DZ'                                          834
         CALL AXSHAPE                                                     834
C    *                                                                    834
5911  CALL AGSPOPB                                                        835
      GO TO BLOCK_DTSH, (5910 ,5920,6980)                                 835
C End of Local Procedure                                                  835
C ----------------------------------------------------------------------  836
C ------------ DTEA                                                       836
C Begin Local Procedure DTEA                                              837
C DTEA                                                                    837
5930  CONTINUE                                                            837
         AG_TITLE='DTEA'                                                  837
         AG_MODULE=MODULE                                                 837
         AG_MARK='V00'                                                    837
         CALL AGSPUSH                                                     837
         AG_MODULE=MODULE                                                 837
C    *                                                                    837
C    Geant ATTRIBUTE : DTEA ===> SEEN=1 COLO=920 <===                     838
         CALL AGSRESET                                                    838
         AG_SEEN = 1                                                      838
         AG_COLO = 920                                                    838
         AG_TITLE = 'DTEA'                                                838
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
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.15 PHI1=267.396, PHI2=360  839
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     839
         CALL AGSRESET                                                    841
         AG_RMIN = 0.026                                                  841
         AG_RMAX = 0.15                                                   841
         AG_PHI1 = 267.396                                                841
         AG_PHI2 = 360                                                    841
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  841
         AG_TITLE = 'TUBS'                                                841
         AG_EXNAME = 'SHAPE'                                              841
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            841
         CALL AXSHAPE                                                     841
C    *                                                                    841
5931  CALL AGSPOPB                                                        842
      GO TO BLOCK_DTEA, (5930 ,5940,6990)                                 842
C End of Local Procedure                                                  842
C ----------------------------------------------------------------------  843
C ------------ DTEB                                                       843
C Begin Local Procedure DTEB                                              844
C DTEB                                                                    844
5950  CONTINUE                                                            844
         AG_TITLE='DTEB'                                                  844
         AG_MODULE=MODULE                                                 844
         AG_MARK='V00'                                                    844
         CALL AGSPUSH                                                     844
         AG_MODULE=MODULE                                                 844
C    *                                                                    844
C    Geant ATTRIBUTE : DTEB ===> SEEN=1 COLO=920 <===                     845
         CALL AGSRESET                                                    845
         AG_SEEN = 1                                                      845
         AG_COLO = 920                                                    845
         AG_TITLE = 'DTEB'                                                845
         AG_EXNAME = 'ATTRIBUTE'                                          845
         AG_PARLIST = 'SEEN_COLO'                                         845
         CALL AXATTRIBUTE                                                 845
C    *                                                                    845
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        845
         CALL AGSRESET                                                    846
         AG_TITLE = 'ALUMINIUM'                                           846
         AG_EXNAME = 'MATERIAL'                                           846
         AG_PARLIST = 'NONE'                                              846
         CALL AXMATERIAL                                                  846
C    *                                                                    846
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.15 PHI1=0 PHI2=79.963, DZ  846
C    =DOVETAILEXTRUSIONLENGTH <===                                        846
         CALL AGSRESET                                                    848
         AG_RMIN = 0.026                                                  848
         AG_RMAX = 0.15                                                   848
         AG_PHI1 = 0                                                      848
         AG_PHI2 = 79.963                                                 848
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  848
         AG_TITLE = 'TUBS'                                                848
         AG_EXNAME = 'SHAPE'                                              848
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            848
         CALL AXSHAPE                                                     848
C    *                                                                    848
5951  CALL AGSPOPB                                                        849
      GO TO BLOCK_DTEB, (5950 ,5960,7000)                                 849
C End of Local Procedure                                                  849
C ----------------------------------------------------------------------  850
C ------------ DTEC                                                       850
C Begin Local Procedure DTEC                                              851
C DTEC                                                                    851
5970  CONTINUE                                                            851
         AG_TITLE='DTEC'                                                  851
         AG_MODULE=MODULE                                                 851
         AG_MARK='V00'                                                    851
         CALL AGSPUSH                                                     851
         AG_MODULE=MODULE                                                 851
C    *                                                                    851
C    Geant ATTRIBUTE : DTEC ===> SEEN=1 COLO=920 <===                     852
         CALL AGSRESET                                                    852
         AG_SEEN = 1                                                      852
         AG_COLO = 920                                                    852
         AG_TITLE = 'DTEC'                                                852
         AG_EXNAME = 'ATTRIBUTE'                                          852
         AG_PARLIST = 'SEEN_COLO'                                         852
         CALL AXATTRIBUTE                                                 852
C    *                                                                    852
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        852
         CALL AGSRESET                                                    853
         AG_TITLE = 'ALUMINIUM'                                           853
         AG_EXNAME = 'MATERIAL'                                           853
         AG_PARLIST = 'NONE'                                              853
         CALL AXMATERIAL                                                  853
C    *                                                                    853
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.15 PHI1=79.963, PHI2=191.  853
C    554 DZ=DOVETAILEXTRUSIONLENGTH <===                                  853
         CALL AGSRESET                                                    855
         AG_RMIN = 0.026                                                  855
         AG_RMAX = 0.15                                                   855
         AG_PHI1 = 79.963                                                 855
         AG_PHI2 = 191.554                                                855
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  855
         AG_TITLE = 'TUBS'                                                855
         AG_EXNAME = 'SHAPE'                                              855
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            855
         CALL AXSHAPE                                                     855
C    *                                                                    855
5971  CALL AGSPOPB                                                        856
      GO TO BLOCK_DTEC, (5970 ,5980,7010)                                 856
C End of Local Procedure                                                  856
C ----------------------------------------------------------------------  857
C ------------ DTED                                                       857
C Begin Local Procedure DTED                                              858
C DTED                                                                    858
5990  CONTINUE                                                            858
         AG_TITLE='DTED'                                                  858
         AG_MODULE=MODULE                                                 858
         AG_MARK='V00'                                                    858
         CALL AGSPUSH                                                     858
         AG_MODULE=MODULE                                                 858
C    *                                                                    858
C    Geant ATTRIBUTE : DTED ===> SEEN=1 COLO=920 <===                     859
         CALL AGSRESET                                                    859
         AG_SEEN = 1                                                      859
         AG_COLO = 920                                                    859
         AG_TITLE = 'DTED'                                                859
         AG_EXNAME = 'ATTRIBUTE'                                          859
         AG_PARLIST = 'SEEN_COLO'                                         859
         CALL AXATTRIBUTE                                                 859
C    *                                                                    859
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        859
         CALL AGSRESET                                                    860
         AG_TITLE = 'ALUMINIUM'                                           860
         AG_EXNAME = 'MATERIAL'                                           860
         AG_PARLIST = 'NONE'                                              860
         CALL AXMATERIAL                                                  860
C    *                                                                    860
C    Geant SHAPE : TUBS ===> RMIN=0.05 RMAX=0.174 PHI1=271.963, PHI2=371  860
C    .554 DZ=DOVETAILEXTRUSIONLENGTH <===                                 860
         CALL AGSRESET                                                    862
         AG_RMIN = 0.05                                                   862
         AG_RMAX = 0.174                                                  862
         AG_PHI1 = 271.963                                                862
         AG_PHI2 = 371.554                                                862
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  862
         AG_TITLE = 'TUBS'                                                862
         AG_EXNAME = 'SHAPE'                                              862
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            862
         CALL AXSHAPE                                                     862
C    *                                                                    862
5991  CALL AGSPOPB                                                        863
      GO TO BLOCK_DTED, (5990 ,6000,7020)                                 863
C End of Local Procedure                                                  863
C ----------------------------------------------------------------------  864
C ------------ DTEE                                                       864
C Begin Local Procedure DTEE                                              865
C DTEE                                                                    865
6010  CONTINUE                                                            865
         AG_TITLE='DTEE'                                                  865
         AG_MODULE=MODULE                                                 865
         AG_MARK='V00'                                                    865
         CALL AGSPUSH                                                     865
         AG_MODULE=MODULE                                                 865
C    *                                                                    865
C    Geant ATTRIBUTE : DTEE ===> SEEN=1 COLO=920 <===                     866
         CALL AGSRESET                                                    866
         AG_SEEN = 1                                                      866
         AG_COLO = 920                                                    866
         AG_TITLE = 'DTEE'                                                866
         AG_EXNAME = 'ATTRIBUTE'                                          866
         AG_PARLIST = 'SEEN_COLO'                                         866
         CALL AXATTRIBUTE                                                 866
C    *                                                                    866
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        866
         CALL AGSRESET                                                    867
         AG_TITLE = 'ALUMINIUM'                                           867
         AG_EXNAME = 'MATERIAL'                                           867
         AG_PARLIST = 'NONE'                                              867
         CALL AXMATERIAL                                                  867
C    *                                                                    867
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.15 PHI1=91.963, PHI2=203.  867
C    554 DZ=DOVETAILEXTRUSIONLENGTH <===                                  867
         CALL AGSRESET                                                    869
         AG_RMIN = 0.026                                                  869
         AG_RMAX = 0.15                                                   869
         AG_PHI1 = 91.963                                                 869
         AG_PHI2 = 203.554                                                869
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  869
         AG_TITLE = 'TUBS'                                                869
         AG_EXNAME = 'SHAPE'                                              869
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            869
         CALL AXSHAPE                                                     869
C    *                                                                    869
6011  CALL AGSPOPB                                                        870
      GO TO BLOCK_DTEE, (6010 ,6020,7030)                                 870
C End of Local Procedure                                                  870
C ----------------------------------------------------------------------  871
C ------------ DTEF                                                       871
C Begin Local Procedure DTEF                                              872
C DTEF                                                                    872
6030  CONTINUE                                                            872
         AG_TITLE='DTEF'                                                  872
         AG_MODULE=MODULE                                                 872
         AG_MARK='V00'                                                    872
         CALL AGSPUSH                                                     872
         AG_MODULE=MODULE                                                 872
C    *                                                                    872
C    Geant ATTRIBUTE : DTEF ===> SEEN=1 COLO=920 <===                     873
         CALL AGSRESET                                                    873
         AG_SEEN = 1                                                      873
         AG_COLO = 920                                                    873
         AG_TITLE = 'DTEF'                                                873
         AG_EXNAME = 'ATTRIBUTE'                                          873
         AG_PARLIST = 'SEEN_COLO'                                         873
         CALL AXATTRIBUTE                                                 873
C    *                                                                    873
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        873
         CALL AGSRESET                                                    874
         AG_TITLE = 'ALUMINIUM'                                           874
         AG_EXNAME = 'MATERIAL'                                           874
         AG_PARLIST = 'NONE'                                              874
         CALL AXMATERIAL                                                  874
C    *                                                                    874
C    Geant SHAPE : TUBS ===> RMIN=0.05 RMAX=0.174 PHI1=283.963, PHI2=383  874
C    .554 DZ=DOVETAILEXTRUSIONLENGTH <===                                 874
         CALL AGSRESET                                                    876
         AG_RMIN = 0.05                                                   876
         AG_RMAX = 0.174                                                  876
         AG_PHI1 = 283.963                                                876
         AG_PHI2 = 383.554                                                876
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  876
         AG_TITLE = 'TUBS'                                                876
         AG_EXNAME = 'SHAPE'                                              876
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            876
         CALL AXSHAPE                                                     876
C    *                                                                    876
6031  CALL AGSPOPB                                                        877
      GO TO BLOCK_DTEF, (6030 ,6040,7040)                                 877
C End of Local Procedure                                                  877
C ----------------------------------------------------------------------  878
C ------------ DTEG                                                       878
C Begin Local Procedure DTEG                                              879
C DTEG                                                                    879
6050  CONTINUE                                                            879
         AG_TITLE='DTEG'                                                  879
         AG_MODULE=MODULE                                                 879
         AG_MARK='V00'                                                    879
         CALL AGSPUSH                                                     879
         AG_MODULE=MODULE                                                 879
C    *                                                                    879
C    Geant ATTRIBUTE : DTEG ===> SEEN=1 COLO=920 <===                     880
         CALL AGSRESET                                                    880
         AG_SEEN = 1                                                      880
         AG_COLO = 920                                                    880
         AG_TITLE = 'DTEG'                                                880
         AG_EXNAME = 'ATTRIBUTE'                                          880
         AG_PARLIST = 'SEEN_COLO'                                         880
         CALL AXATTRIBUTE                                                 880
C    *                                                                    880
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        880
         CALL AGSRESET                                                    881
         AG_TITLE = 'ALUMINIUM'                                           881
         AG_EXNAME = 'MATERIAL'                                           881
         AG_PARLIST = 'NONE'                                              881
         CALL AXMATERIAL                                                  881
C    *                                                                    881
C    Geant SHAPE : TUBS ===> RMIN=0.026 RMAX=0.15 PHI1=103.963, PHI2=216  881
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     881
         CALL AGSRESET                                                    883
         AG_RMIN = 0.026                                                  883
         AG_RMAX = 0.15                                                   883
         AG_PHI1 = 103.963                                                883
         AG_PHI2 = 216                                                    883
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  883
         AG_TITLE = 'TUBS'                                                883
         AG_EXNAME = 'SHAPE'                                              883
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            883
         CALL AXSHAPE                                                     883
C    *                                                                    883
6051  CALL AGSPOPB                                                        884
      GO TO BLOCK_DTEG, (6050 ,6060,7050)                                 884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ DTEH                                                       885
C Begin Local Procedure DTEH                                              886
C DTEH                                                                    886
6070  CONTINUE                                                            886
         AG_TITLE='DTEH'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant ATTRIBUTE : DTEH ===> SEEN=1 COLO=920 <===                     887
         CALL AGSRESET                                                    887
         AG_SEEN = 1                                                      887
         AG_COLO = 920                                                    887
         AG_TITLE = 'DTEH'                                                887
         AG_EXNAME = 'ATTRIBUTE'                                          887
         AG_PARLIST = 'SEEN_COLO'                                         887
         CALL AXATTRIBUTE                                                 887
C    *                                                                    887
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        887
         CALL AGSRESET                                                    888
         AG_TITLE = 'ALUMINIUM'                                           888
         AG_EXNAME = 'MATERIAL'                                           888
         AG_PARLIST = 'NONE'                                              888
         CALL AXMATERIAL                                                  888
C    *                                                                    888
C    Geant SHAPE : TUBS ===> RMIN=0.10795 RMAX=0.23195 PHI1=216, PHI2=26  888
C    7.396 DZ=DOVETAILEXTRUSIONLENGTH <===                                888
         CALL AGSRESET                                                    890
         AG_RMIN = 0.10795                                                890
         AG_RMAX = 0.23195                                                890
         AG_PHI1 = 216                                                    890
         AG_PHI2 = 267.396                                                890
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  890
         AG_TITLE = 'TUBS'                                                890
         AG_EXNAME = 'SHAPE'                                              890
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            890
         CALL AXSHAPE                                                     890
C    *                                                                    890
6071  CALL AGSPOPB                                                        891
      GO TO BLOCK_DTEH, (6070 ,6080,7060)                                 891
C End of Local Procedure                                                  891
C ----------------------------------------------------------------------  892
C ------------ DESA                                                       892
C Begin Local Procedure DESA                                              893
C DESA DOVETAIL EXTRUSION SECTOR RIGHT SIDE                               893
6090  CONTINUE                                                            893
         AG_TITLE='DESA'                                                  893
         AG_MODULE=MODULE                                                 893
         AG_MARK='V00'                                                    893
         CALL AGSPUSH                                                     893
         AG_MODULE=MODULE                                                 893
C    *                                                                    893
C    Geant ATTRIBUTE : DESA ===> SEEN=1 COLO=920 <===                     894
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 920                                                    894
         AG_TITLE = 'DESA'                                                894
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
C    Geant SHAPE : BOX ===> DX=2.52098 DY=DOVETAILEXTRUSIONHALFTHICKNESS  895
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    895
         CALL AGSRESET                                                    897
         AG_DX = 2.52098                                                  897
         AG_DY = DOVETAILEXTRUSIONHALFTHICKNESS                           897
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  897
         AG_TITLE = 'BOX'                                                 897
         AG_EXNAME = 'SHAPE'                                              897
         AG_PARLIST = 'DX_DY_DZ'                                          897
         CALL AXSHAPE                                                     897
C    *                                                                    897
6091  CALL AGSPOPB                                                        898
      GO TO BLOCK_DESA, (6090 ,6100,7070)                                 898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ DESB                                                       899
C Begin Local Procedure DESB                                              900
C DESB DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                900
6110  CONTINUE                                                            900
         AG_TITLE='DESB'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant ATTRIBUTE : DESB ===> SEEN=1 COLO=920 <===                     901
         CALL AGSRESET                                                    901
         AG_SEEN = 1                                                      901
         AG_COLO = 920                                                    901
         AG_TITLE = 'DESB'                                                901
         AG_EXNAME = 'ATTRIBUTE'                                          901
         AG_PARLIST = 'SEEN_COLO'                                         901
         CALL AXATTRIBUTE                                                 901
C    *                                                                    901
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        901
         CALL AGSRESET                                                    902
         AG_TITLE = 'ALUMINIUM'                                           902
         AG_EXNAME = 'MATERIAL'                                           902
         AG_PARLIST = 'NONE'                                              902
         CALL AXMATERIAL                                                  902
C    *                                                                    902
C    Geant SHAPE : BOX ===> DX=0.625575 DY=DOVETAILEXTRUSIONTHICKNESSBOX  902
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    902
         CALL AGSRESET                                                    904
         AG_DX = 0.625575                                                 904
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            904
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  904
         AG_TITLE = 'BOX'                                                 904
         AG_EXNAME = 'SHAPE'                                              904
         AG_PARLIST = 'DX_DY_DZ'                                          904
         CALL AXSHAPE                                                     904
C    *                                                                    904
6111  CALL AGSPOPB                                                        905
      GO TO BLOCK_DESB, (6110 ,6120,7080)                                 905
C End of Local Procedure                                                  905
C ----------------------------------------------------------------------  906
C ------------ DESC                                                       906
C Begin Local Procedure DESC                                              907
C DESC DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                907
6130  CONTINUE                                                            907
         AG_TITLE='DESC'                                                  907
         AG_MODULE=MODULE                                                 907
         AG_MARK='V00'                                                    907
         CALL AGSPUSH                                                     907
         AG_MODULE=MODULE                                                 907
C    *                                                                    907
C    Geant ATTRIBUTE : DESC ===> SEEN=1 COLO=920 <===                     908
         CALL AGSRESET                                                    908
         AG_SEEN = 1                                                      908
         AG_COLO = 920                                                    908
         AG_TITLE = 'DESC'                                                908
         AG_EXNAME = 'ATTRIBUTE'                                          908
         AG_PARLIST = 'SEEN_COLO'                                         908
         CALL AXATTRIBUTE                                                 908
C    *                                                                    908
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        908
         CALL AGSRESET                                                    909
         AG_TITLE = 'ALUMINIUM'                                           909
         AG_EXNAME = 'MATERIAL'                                           909
         AG_PARLIST = 'NONE'                                              909
         CALL AXMATERIAL                                                  909
C    *                                                                    909
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILEXTRUSIONTHICKNESSBO  909
C    X, DZ=DOVETAILEXTRUSIONLENGTH <===                                   909
         CALL AGSRESET                                                    911
         AG_DX = 0.0389368                                                911
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            911
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  911
         AG_TITLE = 'BOX'                                                 911
         AG_EXNAME = 'SHAPE'                                              911
         AG_PARLIST = 'DX_DY_DZ'                                          911
         CALL AXSHAPE                                                     911
C    *                                                                    911
6131  CALL AGSPOPB                                                        912
      GO TO BLOCK_DESC, (6130 ,6140,7090)                                 912
C End of Local Procedure                                                  912
C ----------------------------------------------------------------------  913
C ------------ DESD                                                       913
C Begin Local Procedure DESD                                              914
C DESD DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                914
6150  CONTINUE                                                            914
         AG_TITLE='DESD'                                                  914
         AG_MODULE=MODULE                                                 914
         AG_MARK='V00'                                                    914
         CALL AGSPUSH                                                     914
         AG_MODULE=MODULE                                                 914
C    *                                                                    914
C    Geant ATTRIBUTE : DESD ===> SEEN=1 COLO=920 <===                     915
         CALL AGSRESET                                                    915
         AG_SEEN = 1                                                      915
         AG_COLO = 920                                                    915
         AG_TITLE = 'DESD'                                                915
         AG_EXNAME = 'ATTRIBUTE'                                          915
         AG_PARLIST = 'SEEN_COLO'                                         915
         CALL AXATTRIBUTE                                                 915
C    *                                                                    915
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        915
         CALL AGSRESET                                                    916
         AG_TITLE = 'ALUMINIUM'                                           916
         AG_EXNAME = 'MATERIAL'                                           916
         AG_PARLIST = 'NONE'                                              916
         CALL AXMATERIAL                                                  916
C    *                                                                    916
C    Geant SHAPE : BOX ===> DX=0.681643 DY=DOVETAILEXTRUSIONTHICKNESSBOX  916
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    916
         CALL AGSRESET                                                    918
         AG_DX = 0.681643                                                 918
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            918
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  918
         AG_TITLE = 'BOX'                                                 918
         AG_EXNAME = 'SHAPE'                                              918
         AG_PARLIST = 'DX_DY_DZ'                                          918
         CALL AXSHAPE                                                     918
C    *                                                                    918
6151  CALL AGSPOPB                                                        919
      GO TO BLOCK_DESD, (6150 ,6160,7100)                                 919
C End of Local Procedure                                                  919
C ----------------------------------------------------------------------  920
C ------------ DESE                                                       920
C Begin Local Procedure DESE                                              921
C DESE DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                921
6170  CONTINUE                                                            921
         AG_TITLE='DESE'                                                  921
         AG_MODULE=MODULE                                                 921
         AG_MARK='V00'                                                    921
         CALL AGSPUSH                                                     921
         AG_MODULE=MODULE                                                 921
C    *                                                                    921
C    Geant ATTRIBUTE : DESE ===> SEEN=1 COLO=920 <===                     922
         CALL AGSRESET                                                    922
         AG_SEEN = 1                                                      922
         AG_COLO = 920                                                    922
         AG_TITLE = 'DESE'                                                922
         AG_EXNAME = 'ATTRIBUTE'                                          922
         AG_PARLIST = 'SEEN_COLO'                                         922
         CALL AXATTRIBUTE                                                 922
C    *                                                                    922
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        922
         CALL AGSRESET                                                    923
         AG_TITLE = 'ALUMINIUM'                                           923
         AG_EXNAME = 'MATERIAL'                                           923
         AG_PARLIST = 'NONE'                                              923
         CALL AXMATERIAL                                                  923
C    *                                                                    923
C    Geant SHAPE : BOX ===> DX=0.0389368 DY=DOVETAILEXTRUSIONTHICKNESSBO  923
C    X, DZ=DOVETAILEXTRUSIONLENGTH <===                                   923
         CALL AGSRESET                                                    925
         AG_DX = 0.0389368                                                925
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            925
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  925
         AG_TITLE = 'BOX'                                                 925
         AG_EXNAME = 'SHAPE'                                              925
         AG_PARLIST = 'DX_DY_DZ'                                          925
         CALL AXSHAPE                                                     925
C    *                                                                    925
6171  CALL AGSPOPB                                                        926
      GO TO BLOCK_DESE, (6170 ,6180,7110)                                 926
C End of Local Procedure                                                  926
C ----------------------------------------------------------------------  927
C ------------ DESF                                                       927
C Begin Local Procedure DESF                                              928
C DESF DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                928
6190  CONTINUE                                                            928
         AG_TITLE='DESF'                                                  928
         AG_MODULE=MODULE                                                 928
         AG_MARK='V00'                                                    928
         CALL AGSPUSH                                                     928
         AG_MODULE=MODULE                                                 928
C    *                                                                    928
C    Geant ATTRIBUTE : DESF ===> SEEN=1 COLO=920 <===                     929
         CALL AGSRESET                                                    929
         AG_SEEN = 1                                                      929
         AG_COLO = 920                                                    929
         AG_TITLE = 'DESF'                                                929
         AG_EXNAME = 'ATTRIBUTE'                                          929
         AG_PARLIST = 'SEEN_COLO'                                         929
         CALL AXATTRIBUTE                                                 929
C    *                                                                    929
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        929
         CALL AGSRESET                                                    930
         AG_TITLE = 'ALUMINIUM'                                           930
         AG_EXNAME = 'MATERIAL'                                           930
         AG_PARLIST = 'NONE'                                              930
         CALL AXMATERIAL                                                  930
C    *                                                                    930
C    Geant SHAPE : BOX ===> DX=0.661849 DY=DOVETAILEXTRUSIONTHICKNESSBOX  930
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    930
         CALL AGSRESET                                                    932
         AG_DX = 0.661849                                                 932
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            932
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  932
         AG_TITLE = 'BOX'                                                 932
         AG_EXNAME = 'SHAPE'                                              932
         AG_PARLIST = 'DX_DY_DZ'                                          932
         CALL AXSHAPE                                                     932
C    *                                                                    932
6191  CALL AGSPOPB                                                        933
      GO TO BLOCK_DESF, (6190 ,6200,7120)                                 933
C End of Local Procedure                                                  933
C ----------------------------------------------------------------------  934
C ------------ DESG                                                       934
C Begin Local Procedure DESG                                              935
C DESG DOVETAILEXTRUSION EXTRUSION SECTOR RIGHT UPPER SIDE                935
6210  CONTINUE                                                            935
         AG_TITLE='DESG'                                                  935
         AG_MODULE=MODULE                                                 935
         AG_MARK='V00'                                                    935
         CALL AGSPUSH                                                     935
         AG_MODULE=MODULE                                                 935
C    *                                                                    935
C    Geant ATTRIBUTE : DESG ===> SEEN=1 COLO=920 <===                     936
         CALL AGSRESET                                                    936
         AG_SEEN = 1                                                      936
         AG_COLO = 920                                                    936
         AG_TITLE = 'DESG'                                                936
         AG_EXNAME = 'ATTRIBUTE'                                          936
         AG_PARLIST = 'SEEN_COLO'                                         936
         CALL AXATTRIBUTE                                                 936
C    *                                                                    936
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        936
         CALL AGSRESET                                                    937
         AG_TITLE = 'ALUMINIUM'                                           937
         AG_EXNAME = 'MATERIAL'                                           937
         AG_PARLIST = 'NONE'                                              937
         CALL AXMATERIAL                                                  937
C    *                                                                    937
C    Geant SHAPE : BOX ===> DX=2.39557 DY=DOVETAILEXTRUSIONTHICKNESSBOX,  937
C     DZ=DOVETAILEXTRUSIONLENGTH <===                                     937
         CALL AGSRESET                                                    939
         AG_DX = 2.39557                                                  939
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            939
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  939
         AG_TITLE = 'BOX'                                                 939
         AG_EXNAME = 'SHAPE'                                              939
         AG_PARLIST = 'DX_DY_DZ'                                          939
         CALL AXSHAPE                                                     939
C    *                                                                    939
6211  CALL AGSPOPB                                                        940
      GO TO BLOCK_DESG, (6210 ,6220,7130)                                 940
C End of Local Procedure                                                  940
C ----------------------------------------------------------------------  941
C ------------ DESH                                                       941
C Begin Local Procedure DESH                                              942
C DESH DOVETAIL EXTRUSION SECTOR RIGHT UPPER SIDE                         942
6230  CONTINUE                                                            942
         AG_TITLE='DESH'                                                  942
         AG_MODULE=MODULE                                                 942
         AG_MARK='V00'                                                    942
         CALL AGSPUSH                                                     942
         AG_MODULE=MODULE                                                 942
C    *                                                                    942
C    Geant ATTRIBUTE : DESH ===> SEEN=1 COLO=920 <===                     943
         CALL AGSRESET                                                    943
         AG_SEEN = 1                                                      943
         AG_COLO = 920                                                    943
         AG_TITLE = 'DESH'                                                943
         AG_EXNAME = 'ATTRIBUTE'                                          943
         AG_PARLIST = 'SEEN_COLO'                                         943
         CALL AXATTRIBUTE                                                 943
C    *                                                                    943
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        943
         CALL AGSRESET                                                    944
         AG_TITLE = 'ALUMINIUM'                                           944
         AG_EXNAME = 'MATERIAL'                                           944
         AG_PARLIST = 'NONE'                                              944
         CALL AXMATERIAL                                                  944
C    *                                                                    944
C    Geant SHAPE : BOX ===> DX=0.720682 DY=DOVETAILEXTRUSIONTHICKNESSBOX  944
C    , DZ=DOVETAILEXTRUSIONLENGTH <===                                    944
         CALL AGSRESET                                                    946
         AG_DX = 0.720682                                                 946
         AG_DY = DOVETAILEXTRUSIONTHICKNESSBOX                            946
         AG_DZ = DOVETAILEXTRUSIONLENGTH                                  946
         AG_TITLE = 'BOX'                                                 946
         AG_EXNAME = 'SHAPE'                                              946
         AG_PARLIST = 'DX_DY_DZ'                                          946
         CALL AXSHAPE                                                     946
C    *                                                                    946
6231  CALL AGSPOPB                                                        947
      GO TO BLOCK_DESH, (6230 ,6240,7140)                                 947
C End of Local Procedure                                                  947
C ----------------------------------------------------------------------  948
C ------------ PTRA                                                       948
C Begin Local Procedure PTRA                                              949
C PTRA PTR0 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    949
6250  CONTINUE                                                            949
         AG_TITLE='PTRA'                                                  949
         AG_MODULE=MODULE                                                 949
         AG_MARK='V00'                                                    949
         CALL AGSPUSH                                                     949
         AG_MODULE=MODULE                                                 949
C    *                                                                    949
C    Geant ATTRIBUTE : PTRA ===> SEEN=1 COLO=920 <===                     950
         CALL AGSRESET                                                    950
         AG_SEEN = 1                                                      950
         AG_COLO = 920                                                    950
         AG_TITLE = 'PTRA'                                                950
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
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.719  951
C    003 TL1=0.677192 ALP1=-5.08889E-14, H2=0.198904 BL2=0.719003 TL2=0.  951
C    677192 ALP2=-5.08889E-14 <===                                        951
         CALL AGSRESET                                                    954
         AG_DZ = 0.05                                                     954
         AG_THET = 0                                                      954
         AG_PHI = 0                                                       954
         AG_H1 = 0.198904                                                 954
         AG_BL1 = 0.719003                                                954
         AG_TL1 = 0.677192                                                954
         AG_ALP1 = -5.08889E-14                                           954
         AG_H2 = 0.198904                                                 954
         AG_BL2 = 0.719003                                                954
         AG_TL2 = 0.677192                                                954
         AG_ALP2 = -5.08889E-14                                           954
         AG_TITLE = 'TRAP'                                                954
         AG_EXNAME = 'SHAPE'                                              954
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       954
         CALL AXSHAPE                                                     954
C    *                                                                    954
6251  CALL AGSPOPB                                                        955
      GO TO BLOCK_PTRA, (6250 ,6260,7150)                                 955
C End of Local Procedure                                                  955
C ----------------------------------------------------------------------  956
C ------------ PTRB                                                       956
C Begin Local Procedure PTRB                                              957
C PTRB PTR1 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    957
6270  CONTINUE                                                            957
         AG_TITLE='PTRB'                                                  957
         AG_MODULE=MODULE                                                 957
         AG_MARK='V00'                                                    957
         CALL AGSPUSH                                                     957
         AG_MODULE=MODULE                                                 957
C    *                                                                    957
C    Geant ATTRIBUTE : PTRB ===> SEEN=1 COLO=920 <===                     958
         CALL AGSRESET                                                    958
         AG_SEEN = 1                                                      958
         AG_COLO = 920                                                    958
         AG_TITLE = 'PTRB'                                                958
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
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  959
C    416 TL1=0.752605 ALP1=-5.08889E-14, H2=0.198904 BL2=0.794416 TL2=0.  959
C    752605 ALP2=-5.08889E-14 <===                                        959
         CALL AGSRESET                                                    962
         AG_DZ = 0.05                                                     962
         AG_THET = 0                                                      962
         AG_PHI = 0                                                       962
         AG_H1 = 0.198904                                                 962
         AG_BL1 = 0.794416                                                962
         AG_TL1 = 0.752605                                                962
         AG_ALP1 = -5.08889E-14                                           962
         AG_H2 = 0.198904                                                 962
         AG_BL2 = 0.794416                                                962
         AG_TL2 = 0.752605                                                962
         AG_ALP2 = -5.08889E-14                                           962
         AG_TITLE = 'TRAP'                                                962
         AG_EXNAME = 'SHAPE'                                              962
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       962
         CALL AXSHAPE                                                     962
C    *                                                                    962
6271  CALL AGSPOPB                                                        963
      GO TO BLOCK_PTRB, (6270 ,6280,7160)                                 963
C End of Local Procedure                                                  963
C ----------------------------------------------------------------------  964
C ------------ PTRC                                                       964
C Begin Local Procedure PTRC                                              965
C PTRC PTR2 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    965
6290  CONTINUE                                                            965
         AG_TITLE='PTRC'                                                  965
         AG_MODULE=MODULE                                                 965
         AG_MARK='V00'                                                    965
         CALL AGSPUSH                                                     965
         AG_MODULE=MODULE                                                 965
C    *                                                                    965
C    Geant ATTRIBUTE : PTRC ===> SEEN=1 COLO=920 <===                     966
         CALL AGSRESET                                                    966
         AG_SEEN = 1                                                      966
         AG_COLO = 920                                                    966
         AG_TITLE = 'PTRC'                                                966
         AG_EXNAME = 'ATTRIBUTE'                                          966
         AG_PARLIST = 'SEEN_COLO'                                         966
         CALL AXATTRIBUTE                                                 966
C    *                                                                    966
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        966
         CALL AGSRESET                                                    967
         AG_TITLE = 'ALUMINIUM'                                           967
         AG_EXNAME = 'MATERIAL'                                           967
         AG_PARLIST = 'NONE'                                              967
         CALL AXMATERIAL                                                  967
C    *                                                                    967
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.719  967
C    003 TL1=0.677192 ALP1=0 H2=0.198904, BL2=0.719003 TL2=0.677192 ALP2  967
C    =0 <===                                                              967
         CALL AGSRESET                                                    970
         AG_DZ = 0.05                                                     970
         AG_THET = 0                                                      970
         AG_PHI = 0                                                       970
         AG_H1 = 0.198904                                                 970
         AG_BL1 = 0.719003                                                970
         AG_TL1 = 0.677192                                                970
         AG_ALP1 = 0                                                      970
         AG_H2 = 0.198904                                                 970
         AG_BL2 = 0.719003                                                970
         AG_TL2 = 0.677192                                                970
         AG_ALP2 = 0                                                      970
         AG_TITLE = 'TRAP'                                                970
         AG_EXNAME = 'SHAPE'                                              970
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       970
         CALL AXSHAPE                                                     970
C    *                                                                    970
6291  CALL AGSPOPB                                                        971
      GO TO BLOCK_PTRC, (6290 ,6300,7170)                                 971
C End of Local Procedure                                                  971
C ----------------------------------------------------------------------  972
C ------------ PTRE                                                       972
C Begin Local Procedure PTRE                                              973
C PTRE PTR4 RIGHT HAND SIDE DOVETAIL EXT                                  973
6310  CONTINUE                                                            973
         AG_TITLE='PTRE'                                                  973
         AG_MODULE=MODULE                                                 973
         AG_MARK='V00'                                                    973
         CALL AGSPUSH                                                     973
         AG_MODULE=MODULE                                                 973
C    *                                                                    973
C    Geant ATTRIBUTE : PTRE ===> SEEN=1 COLO=920 <===                     974
         CALL AGSRESET                                                    974
         AG_SEEN = 1                                                      974
         AG_COLO = 920                                                    974
         AG_TITLE = 'PTRE'                                                974
         AG_EXNAME = 'ATTRIBUTE'                                          974
         AG_PARLIST = 'SEEN_COLO'                                         974
         CALL AXATTRIBUTE                                                 974
C    *                                                                    974
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        974
         CALL AGSRESET                                                    975
         AG_TITLE = 'ALUMINIUM'                                           975
         AG_EXNAME = 'MATERIAL'                                           975
         AG_PARLIST = 'NONE'                                              975
         CALL AXMATERIAL                                                  975
C    *                                                                    975
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.3 BL1=2.23871, TL  975
C    1=2.19353 ALP1=-1.70761 H2=0.3 BL2=2.23871, TL2=2.19353 ALP2=-1.707  975
C    61 <===                                                              975
         CALL AGSRESET                                                    978
         AG_DZ = 0.05                                                     978
         AG_THET = 0                                                      978
         AG_PHI = 0                                                       978
         AG_H1 = 0.3                                                      978
         AG_BL1 = 2.23871                                                 978
         AG_TL1 = 2.19353                                                 978
         AG_ALP1 = -1.70761                                               978
         AG_H2 = 0.3                                                      978
         AG_BL2 = 2.23871                                                 978
         AG_TL2 = 2.19353                                                 978
         AG_ALP2 = -1.70761                                               978
         AG_TITLE = 'TRAP'                                                978
         AG_EXNAME = 'SHAPE'                                              978
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       978
         CALL AXSHAPE                                                     978
C    *                                                                    978
6311  CALL AGSPOPB                                                        979
      GO TO BLOCK_PTRE, (6310 ,6320,7180)                                 979
C End of Local Procedure                                                  979
C ----------------------------------------------------------------------  980
C ------------ PTRF                                                       980
C Begin Local Procedure PTRF                                              981
C PTRF PTR5 LEFT HAND (NEAR PIECE)                                        981
6330  CONTINUE                                                            981
         AG_TITLE='PTRF'                                                  981
         AG_MODULE=MODULE                                                 981
         AG_MARK='V00'                                                    981
         CALL AGSPUSH                                                     981
         AG_MODULE=MODULE                                                 981
C    *                                                                    981
C    Geant ATTRIBUTE : PTRF ===> SEEN=1 COLO=920 <===                     982
         CALL AGSRESET                                                    982
         AG_SEEN = 1                                                      982
         AG_COLO = 920                                                    982
         AG_TITLE = 'PTRF'                                                982
         AG_EXNAME = 'ATTRIBUTE'                                          982
         AG_PARLIST = 'SEEN_COLO'                                         982
         CALL AXATTRIBUTE                                                 982
C    *                                                                    982
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        982
         CALL AGSRESET                                                    983
         AG_TITLE = 'ALUMINIUM'                                           983
         AG_EXNAME = 'MATERIAL'                                           983
         AG_PARLIST = 'NONE'                                              983
         CALL AXMATERIAL                                                  983
C    *                                                                    983
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.3 BL1=2.13427, TL  983
C    1=1.92628 ALP1=24.3113 H2=0.3 BL2=2.13427, TL2=1.92628 ALP2=24.3113  983
C     <===                                                                983
         CALL AGSRESET                                                    986
         AG_DZ = 0.05                                                     986
         AG_THET = 0                                                      986
         AG_PHI = 0                                                       986
         AG_H1 = 0.3                                                      986
         AG_BL1 = 2.13427                                                 986
         AG_TL1 = 1.92628                                                 986
         AG_ALP1 = 24.3113                                                986
         AG_H2 = 0.3                                                      986
         AG_BL2 = 2.13427                                                 986
         AG_TL2 = 1.92628                                                 986
         AG_ALP2 = 24.3113                                                986
         AG_TITLE = 'TRAP'                                                986
         AG_EXNAME = 'SHAPE'                                              986
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       986
         CALL AXSHAPE                                                     986
C    *                                                                    986
6331  CALL AGSPOPB                                                        987
      GO TO BLOCK_PTRF, (6330 ,6340,7190)                                 987
C End of Local Procedure                                                  987
C ----------------------------------------------------------------------  988
C ------------ PTRG                                                       988
C Begin Local Procedure PTRG                                              989
C PTRG PTR6 PIECE NEAR BP                                                 989
6350  CONTINUE                                                            989
         AG_TITLE='PTRG'                                                  989
         AG_MODULE=MODULE                                                 989
         AG_MARK='V00'                                                    989
         CALL AGSPUSH                                                     989
         AG_MODULE=MODULE                                                 989
C    *                                                                    989
C    Geant ATTRIBUTE : PTRG ===> SEEN=1 COLO=920 <===                     990
         CALL AGSRESET                                                    990
         AG_SEEN = 1                                                      990
         AG_COLO = 920                                                    990
         AG_TITLE = 'PTRG'                                                990
         AG_EXNAME = 'ATTRIBUTE'                                          990
         AG_PARLIST = 'SEEN_COLO'                                         990
         CALL AXATTRIBUTE                                                 990
C    *                                                                    990
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        990
         CALL AGSRESET                                                    991
         AG_TITLE = 'ALUMINIUM'                                           991
         AG_EXNAME = 'MATERIAL'                                           991
         AG_PARLIST = 'NONE'                                              991
         CALL AXMATERIAL                                                  991
C    *                                                                    991
C    Geant SHAPE : TRAP ===> DZ=0.65 THET=0 PHI=0 H1=0.4 BL1=0.461319, T  991
C    L1=0.16015 ALP1=-22.8769 H2=0.4, BL2=0.461319 TL2=0.16015 ALP2=-22.  991
C    8769 <===                                                            991
         CALL AGSRESET                                                    994
         AG_DZ = 0.65                                                     994
         AG_THET = 0                                                      994
         AG_PHI = 0                                                       994
         AG_H1 = 0.4                                                      994
         AG_BL1 = 0.461319                                                994
         AG_TL1 = 0.16015                                                 994
         AG_ALP1 = -22.8769                                               994
         AG_H2 = 0.4                                                      994
         AG_BL2 = 0.461319                                                994
         AG_TL2 = 0.16015                                                 994
         AG_ALP2 = -22.8769                                               994
         AG_TITLE = 'TRAP'                                                994
         AG_EXNAME = 'SHAPE'                                              994
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       994
         CALL AXSHAPE                                                     994
C    *                                                                    994
6351  CALL AGSPOPB                                                        995
      GO TO BLOCK_PTRG, (6350 ,6360,7200)                                 995
C End of Local Procedure                                                  995
C ----------------------------------------------------------------------  996
C ------------ PTMA                                                       996
C Begin Local Procedure PTMA                                              997
C PTMA PTM0 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    997
6370  CONTINUE                                                            997
         AG_TITLE='PTMA'                                                  997
         AG_MODULE=MODULE                                                 997
         AG_MARK='V00'                                                    997
         CALL AGSPUSH                                                     997
         AG_MODULE=MODULE                                                 997
C    *                                                                    997
C    Geant ATTRIBUTE : PTMA ===> SEEN=1 COLO=920 <===                     998
         CALL AGSRESET                                                    998
         AG_SEEN = 1                                                      998
         AG_COLO = 920                                                    998
         AG_TITLE = 'PTMA'                                                998
         AG_EXNAME = 'ATTRIBUTE'                                          998
         AG_PARLIST = 'SEEN_COLO'                                         998
         CALL AXATTRIBUTE                                                 998
C    *                                                                    998
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        998
         CALL AGSRESET                                                    999
         AG_TITLE = 'ALUMINIUM'                                           999
         AG_EXNAME = 'MATERIAL'                                           999
         AG_PARLIST = 'NONE'                                              999
         CALL AXMATERIAL                                                  999
C    *                                                                    999
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.198904, BL1=0.4676  999
C    26 TL1=0.425815 ALP1=-1.01778E-13, H2=0.198904 BL2=0.467626 TL2=0.4  999
C    25815 ALP2=-1.01778E-13 <===                                         999
         CALL AGSRESET                                                    1002
         AG_DZ = 0.6                                                      1002
         AG_THET = 0                                                      1002
         AG_PHI = 0                                                       1002
         AG_H1 = 0.198904                                                 1002
         AG_BL1 = 0.467626                                                1002
         AG_TL1 = 0.425815                                                1002
         AG_ALP1 = -1.01778E-13                                           1002
         AG_H2 = 0.198904                                                 1002
         AG_BL2 = 0.467626                                                1002
         AG_TL2 = 0.425815                                                1002
         AG_ALP2 = -1.01778E-13                                           1002
         AG_TITLE = 'TRAP'                                                1002
         AG_EXNAME = 'SHAPE'                                              1002
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1002
         CALL AXSHAPE                                                     1002
C    *                                                                    1002
6371  CALL AGSPOPB                                                        1003
      GO TO BLOCK_PTMA, (6370 ,6380,7210)                                 1003
C End of Local Procedure                                                  1003
C ----------------------------------------------------------------------  1004
C ------------ PTMB                                                       1004
C Begin Local Procedure PTMB                                              1005
C PTMB PTM1 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    1005
6390  CONTINUE                                                            1005
         AG_TITLE='PTMB'                                                  1005
         AG_MODULE=MODULE                                                 1005
         AG_MARK='V00'                                                    1005
         CALL AGSPUSH                                                     1005
         AG_MODULE=MODULE                                                 1005
C    *                                                                    1005
C    Geant ATTRIBUTE : PTMB ===> SEEN=1 COLO=920 <===                     1006
         CALL AGSRESET                                                    1006
         AG_SEEN = 1                                                      1006
         AG_COLO = 920                                                    1006
         AG_TITLE = 'PTMB'                                                1006
         AG_EXNAME = 'ATTRIBUTE'                                          1006
         AG_PARLIST = 'SEEN_COLO'                                         1006
         CALL AXATTRIBUTE                                                 1006
C    *                                                                    1006
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1006
         CALL AGSRESET                                                    1007
         AG_TITLE = 'ALUMINIUM'                                           1007
         AG_EXNAME = 'MATERIAL'                                           1007
         AG_PARLIST = 'NONE'                                              1007
         CALL AXMATERIAL                                                  1007
C    *                                                                    1007
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.198904, BL1=0.7944  1007
C    16 TL1=0.752605 ALP1=-5.08889E-14, H2=0.198904 BL2=0.794416 TL2=0.7  1007
C    52605 ALP2=-5.08889E-14 <===                                         1007
         CALL AGSRESET                                                    1010
         AG_DZ = 0.6                                                      1010
         AG_THET = 0                                                      1010
         AG_PHI = 0                                                       1010
         AG_H1 = 0.198904                                                 1010
         AG_BL1 = 0.794416                                                1010
         AG_TL1 = 0.752605                                                1010
         AG_ALP1 = -5.08889E-14                                           1010
         AG_H2 = 0.198904                                                 1010
         AG_BL2 = 0.794416                                                1010
         AG_TL2 = 0.752605                                                1010
         AG_ALP2 = -5.08889E-14                                           1010
         AG_TITLE = 'TRAP'                                                1010
         AG_EXNAME = 'SHAPE'                                              1010
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1010
         CALL AXSHAPE                                                     1010
C    *                                                                    1010
6391  CALL AGSPOPB                                                        1011
      GO TO BLOCK_PTMB, (6390 ,6400,7220)                                 1011
C End of Local Procedure                                                  1011
C ----------------------------------------------------------------------  1012
C ------------ PTMC                                                       1012
C Begin Local Procedure PTMC                                              1013
C PTMC PTM2 APPROXIMATE ARC OF FIRST PIECE OF DOVETAIL                    1013
6410  CONTINUE                                                            1013
         AG_TITLE='PTMC'                                                  1013
         AG_MODULE=MODULE                                                 1013
         AG_MARK='V00'                                                    1013
         CALL AGSPUSH                                                     1013
         AG_MODULE=MODULE                                                 1013
C    *                                                                    1013
C    Geant ATTRIBUTE : PTMC ===> SEEN=1 COLO=920 <===                     1014
         CALL AGSRESET                                                    1014
         AG_SEEN = 1                                                      1014
         AG_COLO = 920                                                    1014
         AG_TITLE = 'PTMC'                                                1014
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
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.198904, BL1=0.4676  1015
C    26 TL1=0.425815 ALP1=-1.52667E-13, H2=0.198904 BL2=0.467626 TL2=0.4  1015
C    25815 ALP2=-1.52667E-13 <===                                         1015
         CALL AGSRESET                                                    1018
         AG_DZ = 0.6                                                      1018
         AG_THET = 0                                                      1018
         AG_PHI = 0                                                       1018
         AG_H1 = 0.198904                                                 1018
         AG_BL1 = 0.467626                                                1018
         AG_TL1 = 0.425815                                                1018
         AG_ALP1 = -1.52667E-13                                           1018
         AG_H2 = 0.198904                                                 1018
         AG_BL2 = 0.467626                                                1018
         AG_TL2 = 0.425815                                                1018
         AG_ALP2 = -1.52667E-13                                           1018
         AG_TITLE = 'TRAP'                                                1018
         AG_EXNAME = 'SHAPE'                                              1018
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1018
         CALL AXSHAPE                                                     1018
C    *                                                                    1018
6411  CALL AGSPOPB                                                        1019
      GO TO BLOCK_PTMC, (6410 ,6420,7230)                                 1019
C End of Local Procedure                                                  1019
C ----------------------------------------------------------------------  1020
C ------------ PTME                                                       1020
C Begin Local Procedure PTME                                              1021
C PTME PTM4 OF FIRST PIECE OF DOVETAIL                                    1021
6430  CONTINUE                                                            1021
         AG_TITLE='PTME'                                                  1021
         AG_MODULE=MODULE                                                 1021
         AG_MARK='V00'                                                    1021
         CALL AGSPUSH                                                     1021
         AG_MODULE=MODULE                                                 1021
C    *                                                                    1021
C    Geant ATTRIBUTE : PTME ===> SEEN=1 COLO=920 <===                     1022
         CALL AGSRESET                                                    1022
         AG_SEEN = 1                                                      1022
         AG_COLO = 920                                                    1022
         AG_TITLE = 'PTME'                                                1022
         AG_EXNAME = 'ATTRIBUTE'                                          1022
         AG_PARLIST = 'SEEN_COLO'                                         1022
         CALL AXATTRIBUTE                                                 1022
C    *                                                                    1022
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1022
         CALL AGSRESET                                                    1023
         AG_TITLE = 'ALUMINIUM'                                           1023
         AG_EXNAME = 'MATERIAL'                                           1023
         AG_PARLIST = 'NONE'                                              1023
         CALL AXMATERIAL                                                  1023
C    *                                                                    1023
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.05 BL1=2.20106, TL  1023
C    1=2.19353 ALP1=-1.70761 H2=0.05, BL2=2.20106 TL2=2.19353 ALP2=-1.70  1023
C    761 <===                                                             1023
         CALL AGSRESET                                                    1026
         AG_DZ = 0.6                                                      1026
         AG_THET = 0                                                      1026
         AG_PHI = 0                                                       1026
         AG_H1 = 0.05                                                     1026
         AG_BL1 = 2.20106                                                 1026
         AG_TL1 = 2.19353                                                 1026
         AG_ALP1 = -1.70761                                               1026
         AG_H2 = 0.05                                                     1026
         AG_BL2 = 2.20106                                                 1026
         AG_TL2 = 2.19353                                                 1026
         AG_ALP2 = -1.70761                                               1026
         AG_TITLE = 'TRAP'                                                1026
         AG_EXNAME = 'SHAPE'                                              1026
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1026
         CALL AXSHAPE                                                     1026
C    *                                                                    1026
6431  CALL AGSPOPB                                                        1027
      GO TO BLOCK_PTME, (6430 ,6440,7240)                                 1027
C End of Local Procedure                                                  1027
C ----------------------------------------------------------------------  1028
C ------------ PTMF                                                       1028
C Begin Local Procedure PTMF                                              1029
C PTMF PTM5 LEFT MIDDLE PART                                              1029
6450  CONTINUE                                                            1029
         AG_TITLE='PTMF'                                                  1029
         AG_MODULE=MODULE                                                 1029
         AG_MARK='V00'                                                    1029
         CALL AGSPUSH                                                     1029
         AG_MODULE=MODULE                                                 1029
C    *                                                                    1029
C    Geant ATTRIBUTE : PTMF ===> SEEN=1 COLO=920 <===                     1030
         CALL AGSRESET                                                    1030
         AG_SEEN = 1                                                      1030
         AG_COLO = 920                                                    1030
         AG_TITLE = 'PTMF'                                                1030
         AG_EXNAME = 'ATTRIBUTE'                                          1030
         AG_PARLIST = 'SEEN_COLO'                                         1030
         CALL AXATTRIBUTE                                                 1030
C    *                                                                    1030
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1030
         CALL AGSRESET                                                    1031
         AG_TITLE = 'ALUMINIUM'                                           1031
         AG_EXNAME = 'MATERIAL'                                           1031
         AG_PARLIST = 'NONE'                                              1031
         CALL AXMATERIAL                                                  1031
C    *                                                                    1031
C    Geant SHAPE : TRAP ===> DZ=0.6 THET=0 PHI=0 H1=0.05 BL1=2.13427, TL  1031
C    1=2.09961 ALP1=24.3113 H2=0.05 BL2=2.13427, TL2=2.09961 ALP2=24.311  1031
C    3 <===                                                               1031
         CALL AGSRESET                                                    1034
         AG_DZ = 0.6                                                      1034
         AG_THET = 0                                                      1034
         AG_PHI = 0                                                       1034
         AG_H1 = 0.05                                                     1034
         AG_BL1 = 2.13427                                                 1034
         AG_TL1 = 2.09961                                                 1034
         AG_ALP1 = 24.3113                                                1034
         AG_H2 = 0.05                                                     1034
         AG_BL2 = 2.13427                                                 1034
         AG_TL2 = 2.09961                                                 1034
         AG_ALP2 = 24.3113                                                1034
         AG_TITLE = 'TRAP'                                                1034
         AG_EXNAME = 'SHAPE'                                              1034
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1034
         CALL AXSHAPE                                                     1034
C    *                                                                    1034
6451  CALL AGSPOPB                                                        1035
      GO TO BLOCK_PTMF, (6450 ,6460,7250)                                 1035
C End of Local Procedure                                                  1035
C ----------------------------------------------------------------------  1036
C ------------ PTLA                                                       1036
C Begin Local Procedure PTLA                                              1037
C PTLA PTL0 DOVETAIL SUPPORT PLATE PART OUTER                             1037
6470  CONTINUE                                                            1037
         AG_TITLE='PTLA'                                                  1037
         AG_MODULE=MODULE                                                 1037
         AG_MARK='V00'                                                    1037
         CALL AGSPUSH                                                     1037
         AG_MODULE=MODULE                                                 1037
C    *                                                                    1037
C    Geant ATTRIBUTE : PTLA ===> SEEN=1 COLO=920 <===                     1038
         CALL AGSRESET                                                    1038
         AG_SEEN = 1                                                      1038
         AG_COLO = 920                                                    1038
         AG_TITLE = 'PTLA'                                                1038
         AG_EXNAME = 'ATTRIBUTE'                                          1038
         AG_PARLIST = 'SEEN_COLO'                                         1038
         CALL AXATTRIBUTE                                                 1038
C    *                                                                    1038
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1038
         CALL AGSRESET                                                    1039
         AG_TITLE = 'ALUMINIUM'                                           1039
         AG_EXNAME = 'MATERIAL'                                           1039
         AG_PARLIST = 'NONE'                                              1039
         CALL AXMATERIAL                                                  1039
C    *                                                                    1039
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  1039
C    416 TL1=0.752605 ALP1=0 H2=0.198904, BL2=0.794416 TL2=0.752605 ALP2  1039
C    =0 <===                                                              1039
         CALL AGSRESET                                                    1042
         AG_DZ = 0.05                                                     1042
         AG_THET = 0                                                      1042
         AG_PHI = 0                                                       1042
         AG_H1 = 0.198904                                                 1042
         AG_BL1 = 0.794416                                                1042
         AG_TL1 = 0.752605                                                1042
         AG_ALP1 = 0                                                      1042
         AG_H2 = 0.198904                                                 1042
         AG_BL2 = 0.794416                                                1042
         AG_TL2 = 0.752605                                                1042
         AG_ALP2 = 0                                                      1042
         AG_TITLE = 'TRAP'                                                1042
         AG_EXNAME = 'SHAPE'                                              1042
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1042
         CALL AXSHAPE                                                     1042
C    *                                                                    1042
6471  CALL AGSPOPB                                                        1043
      GO TO BLOCK_PTLA, (6470 ,6480,7260)                                 1043
C End of Local Procedure                                                  1043
C ----------------------------------------------------------------------  1044
C ------------ PTLB                                                       1044
C Begin Local Procedure PTLB                                              1045
C PTLB PTL1 DOVETAIL SUPPORT PLATE PART OUTER                             1045
6490  CONTINUE                                                            1045
         AG_TITLE='PTLB'                                                  1045
         AG_MODULE=MODULE                                                 1045
         AG_MARK='V00'                                                    1045
         CALL AGSPUSH                                                     1045
         AG_MODULE=MODULE                                                 1045
C    *                                                                    1045
C    Geant ATTRIBUTE : PTLB ===> SEEN=1 COLO=920 <===                     1046
         CALL AGSRESET                                                    1046
         AG_SEEN = 1                                                      1046
         AG_COLO = 920                                                    1046
         AG_TITLE = 'PTLB'                                                1046
         AG_EXNAME = 'ATTRIBUTE'                                          1046
         AG_PARLIST = 'SEEN_COLO'                                         1046
         CALL AXATTRIBUTE                                                 1046
C    *                                                                    1046
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1046
         CALL AGSRESET                                                    1047
         AG_TITLE = 'ALUMINIUM'                                           1047
         AG_EXNAME = 'MATERIAL'                                           1047
         AG_PARLIST = 'NONE'                                              1047
         CALL AXMATERIAL                                                  1047
C    *                                                                    1047
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  1047
C    416 TL1=0.752605 ALP1=-5.08889E-14, H2=0.198904 BL2=0.794416 TL2=0.  1047
C    752605 ALP2=-5.08889E-14 <===                                        1047
         CALL AGSRESET                                                    1050
         AG_DZ = 0.05                                                     1050
         AG_THET = 0                                                      1050
         AG_PHI = 0                                                       1050
         AG_H1 = 0.198904                                                 1050
         AG_BL1 = 0.794416                                                1050
         AG_TL1 = 0.752605                                                1050
         AG_ALP1 = -5.08889E-14                                           1050
         AG_H2 = 0.198904                                                 1050
         AG_BL2 = 0.794416                                                1050
         AG_TL2 = 0.752605                                                1050
         AG_ALP2 = -5.08889E-14                                           1050
         AG_TITLE = 'TRAP'                                                1050
         AG_EXNAME = 'SHAPE'                                              1050
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1050
         CALL AXSHAPE                                                     1050
C    *                                                                    1050
6491  CALL AGSPOPB                                                        1051
      GO TO BLOCK_PTLB, (6490 ,6500,7270)                                 1051
C End of Local Procedure                                                  1051
C ----------------------------------------------------------------------  1052
C ------------ PTLC                                                       1052
C Begin Local Procedure PTLC                                              1053
C PTLC PTL2 DOVETAIL SUPPORT PLATE PART OUTER                             1053
6510  CONTINUE                                                            1053
         AG_TITLE='PTLC'                                                  1053
         AG_MODULE=MODULE                                                 1053
         AG_MARK='V00'                                                    1053
         CALL AGSPUSH                                                     1053
         AG_MODULE=MODULE                                                 1053
C    *                                                                    1053
C    Geant ATTRIBUTE : PTLC ===> SEEN=1 COLO=920 <===                     1054
         CALL AGSRESET                                                    1054
         AG_SEEN = 1                                                      1054
         AG_COLO = 920                                                    1054
         AG_TITLE = 'PTLC'                                                1054
         AG_EXNAME = 'ATTRIBUTE'                                          1054
         AG_PARLIST = 'SEEN_COLO'                                         1054
         CALL AXATTRIBUTE                                                 1054
C    *                                                                    1054
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1054
         CALL AGSRESET                                                    1055
         AG_TITLE = 'ALUMINIUM'                                           1055
         AG_EXNAME = 'MATERIAL'                                           1055
         AG_PARLIST = 'NONE'                                              1055
         CALL AXMATERIAL                                                  1055
C    *                                                                    1055
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.198904, BL1=0.794  1055
C    416 TL1=0.752605 ALP1=0 H2=0.198904, BL2=0.794416 TL2=0.752605 ALP2  1055
C    =0 <===                                                              1055
         CALL AGSRESET                                                    1058
         AG_DZ = 0.05                                                     1058
         AG_THET = 0                                                      1058
         AG_PHI = 0                                                       1058
         AG_H1 = 0.198904                                                 1058
         AG_BL1 = 0.794416                                                1058
         AG_TL1 = 0.752605                                                1058
         AG_ALP1 = 0                                                      1058
         AG_H2 = 0.198904                                                 1058
         AG_BL2 = 0.794416                                                1058
         AG_TL2 = 0.752605                                                1058
         AG_ALP2 = 0                                                      1058
         AG_TITLE = 'TRAP'                                                1058
         AG_EXNAME = 'SHAPE'                                              1058
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1058
         CALL AXSHAPE                                                     1058
C    *                                                                    1058
6511  CALL AGSPOPB                                                        1059
      GO TO BLOCK_PTLC, (6510 ,6520,7280)                                 1059
C End of Local Procedure                                                  1059
C ----------------------------------------------------------------------  1060
C ------------ PTLD                                                       1060
C Begin Local Procedure PTLD                                              1061
C PTLD PTL3 DOVETAIL SUPPORT PALTE PART OUTER                             1061
6530  CONTINUE                                                            1061
         AG_TITLE='PTLD'                                                  1061
         AG_MODULE=MODULE                                                 1061
         AG_MARK='V00'                                                    1061
         CALL AGSPUSH                                                     1061
         AG_MODULE=MODULE                                                 1061
C    *                                                                    1061
C    Geant ATTRIBUTE : PTLD ===> SEEN=1 COLO=920 <===                     1062
         CALL AGSRESET                                                    1062
         AG_SEEN = 1                                                      1062
         AG_COLO = 920                                                    1062
         AG_TITLE = 'PTLD'                                                1062
         AG_EXNAME = 'ATTRIBUTE'                                          1062
         AG_PARLIST = 'SEEN_COLO'                                         1062
         CALL AXATTRIBUTE                                                 1062
C    *                                                                    1062
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1062
         CALL AGSRESET                                                    1063
         AG_TITLE = 'ALUMINIUM'                                           1063
         AG_EXNAME = 'MATERIAL'                                           1063
         AG_PARLIST = 'NONE'                                              1063
         CALL AXMATERIAL                                                  1063
C    *                                                                    1063
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.397809, BL1=0.365  1063
C    85 TL1=0.282227 ALP1=-5.08889E-14, H2=0.397809 BL2=0.36585 TL2=0.28  1063
C    2227 ALP2=-5.08889E-14 <===                                          1063
         CALL AGSRESET                                                    1066
         AG_DZ = 0.05                                                     1066
         AG_THET = 0                                                      1066
         AG_PHI = 0                                                       1066
         AG_H1 = 0.397809                                                 1066
         AG_BL1 = 0.36585                                                 1066
         AG_TL1 = 0.282227                                                1066
         AG_ALP1 = -5.08889E-14                                           1066
         AG_H2 = 0.397809                                                 1066
         AG_BL2 = 0.36585                                                 1066
         AG_TL2 = 0.282227                                                1066
         AG_ALP2 = -5.08889E-14                                           1066
         AG_TITLE = 'TRAP'                                                1066
         AG_EXNAME = 'SHAPE'                                              1066
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1066
         CALL AXSHAPE                                                     1066
C    *                                                                    1066
6531  CALL AGSPOPB                                                        1067
      GO TO BLOCK_PTLD, (6530 ,6540,7290)                                 1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ PTLE                                                       1068
C Begin Local Procedure PTLE                                              1069
C PTLE PTL4 DOVETAIL SUPPORT PALTE PART OUTER                             1069
6550  CONTINUE                                                            1069
         AG_TITLE='PTLE'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant ATTRIBUTE : PTLE ===> SEEN=1 COLO=920 <===                     1070
         CALL AGSRESET                                                    1070
         AG_SEEN = 1                                                      1070
         AG_COLO = 920                                                    1070
         AG_TITLE = 'PTLE'                                                1070
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
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.397809, BL1=0.365  1071
C    85 TL1=0.282227 ALP1=5.08889E-14, H2=0.397809 BL2=0.36585 TL2=0.282  1071
C    227 ALP2=5.08889E-14 <===                                            1071
         CALL AGSRESET                                                    1074
         AG_DZ = 0.05                                                     1074
         AG_THET = 0                                                      1074
         AG_PHI = 0                                                       1074
         AG_H1 = 0.397809                                                 1074
         AG_BL1 = 0.36585                                                 1074
         AG_TL1 = 0.282227                                                1074
         AG_ALP1 = 5.08889E-14                                            1074
         AG_H2 = 0.397809                                                 1074
         AG_BL2 = 0.36585                                                 1074
         AG_TL2 = 0.282227                                                1074
         AG_ALP2 = 5.08889E-14                                            1074
         AG_TITLE = 'TRAP'                                                1074
         AG_EXNAME = 'SHAPE'                                              1074
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1074
         CALL AXSHAPE                                                     1074
C    *                                                                    1074
6551  CALL AGSPOPB                                                        1075
      GO TO BLOCK_PTLE, (6550 ,6560,7300)                                 1075
C End of Local Procedure                                                  1075
C ----------------------------------------------------------------------  1076
C ------------ PTLF                                                       1076
C Begin Local Procedure PTLF                                              1077
C PTLF PTL5 DOVETAIL SUPPORT PALTE PART OUTER                             1077
6570  CONTINUE                                                            1077
         AG_TITLE='PTLF'                                                  1077
         AG_MODULE=MODULE                                                 1077
         AG_MARK='V00'                                                    1077
         CALL AGSPUSH                                                     1077
         AG_MODULE=MODULE                                                 1077
C    *                                                                    1077
C    Geant ATTRIBUTE : PTLF ===> SEEN=1 COLO=920 <===                     1078
         CALL AGSRESET                                                    1078
         AG_SEEN = 1                                                      1078
         AG_COLO = 920                                                    1078
         AG_TITLE = 'PTLF'                                                1078
         AG_EXNAME = 'ATTRIBUTE'                                          1078
         AG_PARLIST = 'SEEN_COLO'                                         1078
         CALL AXATTRIBUTE                                                 1078
C    *                                                                    1078
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1078
         CALL AGSRESET                                                    1079
         AG_TITLE = 'ALUMINIUM'                                           1079
         AG_EXNAME = 'MATERIAL'                                           1079
         AG_PARLIST = 'NONE'                                              1079
         CALL AXMATERIAL                                                  1079
C    *                                                                    1079
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=0.397809, BL1=0.365  1079
C    85 TL1=0.282227 ALP1=-1.01778E-13, H2=0.397809 BL2=0.36585 TL2=0.28  1079
C    2227 ALP2=-1.01778E-13 <===                                          1079
         CALL AGSRESET                                                    1082
         AG_DZ = 0.05                                                     1082
         AG_THET = 0                                                      1082
         AG_PHI = 0                                                       1082
         AG_H1 = 0.397809                                                 1082
         AG_BL1 = 0.36585                                                 1082
         AG_TL1 = 0.282227                                                1082
         AG_ALP1 = -1.01778E-13                                           1082
         AG_H2 = 0.397809                                                 1082
         AG_BL2 = 0.36585                                                 1082
         AG_TL2 = 0.282227                                                1082
         AG_ALP2 = -1.01778E-13                                           1082
         AG_TITLE = 'TRAP'                                                1082
         AG_EXNAME = 'SHAPE'                                              1082
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1082
         CALL AXSHAPE                                                     1082
C    *                                                                    1082
6571  CALL AGSPOPB                                                        1083
      GO TO BLOCK_PTLF, (6570 ,6580,7310)                                 1083
C End of Local Procedure                                                  1083
C ----------------------------------------------------------------------  1084
C ------------ PTLG                                                       1084
C Begin Local Procedure PTLG                                              1085
C PTLG PTL6 DOVETAIL SUPPORT PLATE PART OUTER                             1085
6590  CONTINUE                                                            1085
         AG_TITLE='PTLG'                                                  1085
         AG_MODULE=MODULE                                                 1085
         AG_MARK='V00'                                                    1085
         CALL AGSPUSH                                                     1085
         AG_MODULE=MODULE                                                 1085
C    *                                                                    1085
C    Geant ATTRIBUTE : PTLG ===> SEEN=1 COLO=920 <===                     1086
         CALL AGSRESET                                                    1086
         AG_SEEN = 1                                                      1086
         AG_COLO = 920                                                    1086
         AG_TITLE = 'PTLG'                                                1086
         AG_EXNAME = 'ATTRIBUTE'                                          1086
         AG_PARLIST = 'SEEN_COLO'                                         1086
         CALL AXATTRIBUTE                                                 1086
C    *                                                                    1086
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1086
         CALL AGSRESET                                                    1087
         AG_TITLE = 'ALUMINIUM'                                           1087
         AG_EXNAME = 'MATERIAL'                                           1087
         AG_PARLIST = 'NONE'                                              1087
         CALL AXMATERIAL                                                  1087
C    *                                                                    1087
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=1.83987, BL1=0.3770  1087
C    66 TL1=0.377066 ALP1=6 H2=1.83987, BL2=0.377066 TL2=0.377066 ALP2=6  1087
C     <===                                                                1087
         CALL AGSRESET                                                    1090
         AG_DZ = 0.05                                                     1090
         AG_THET = 0                                                      1090
         AG_PHI = 0                                                       1090
         AG_H1 = 1.83987                                                  1090
         AG_BL1 = 0.377066                                                1090
         AG_TL1 = 0.377066                                                1090
         AG_ALP1 = 6                                                      1090
         AG_H2 = 1.83987                                                  1090
         AG_BL2 = 0.377066                                                1090
         AG_TL2 = 0.377066                                                1090
         AG_ALP2 = 6                                                      1090
         AG_TITLE = 'TRAP'                                                1090
         AG_EXNAME = 'SHAPE'                                              1090
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1090
         CALL AXSHAPE                                                     1090
C    *                                                                    1090
6591  CALL AGSPOPB                                                        1091
      GO TO BLOCK_PTLG, (6590 ,6600,7320)                                 1091
C End of Local Procedure                                                  1091
C ----------------------------------------------------------------------  1092
C ------------ PTLH                                                       1092
C Begin Local Procedure PTLH                                              1093
C PTLH PTL7 DOVETAIL SUPPORT PLATE PART OUTER                             1093
6610  CONTINUE                                                            1093
         AG_TITLE='PTLH'                                                  1093
         AG_MODULE=MODULE                                                 1093
         AG_MARK='V00'                                                    1093
         CALL AGSPUSH                                                     1093
         AG_MODULE=MODULE                                                 1093
C    *                                                                    1093
C    Geant ATTRIBUTE : PTLH ===> SEEN=1 COLO=920 <===                     1094
         CALL AGSRESET                                                    1094
         AG_SEEN = 1                                                      1094
         AG_COLO = 920                                                    1094
         AG_TITLE = 'PTLH'                                                1094
         AG_EXNAME = 'ATTRIBUTE'                                          1094
         AG_PARLIST = 'SEEN_COLO'                                         1094
         CALL AXATTRIBUTE                                                 1094
C    *                                                                    1094
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1094
         CALL AGSRESET                                                    1095
         AG_TITLE = 'ALUMINIUM'                                           1095
         AG_EXNAME = 'MATERIAL'                                           1095
         AG_PARLIST = 'NONE'                                              1095
         CALL AXMATERIAL                                                  1095
C    *                                                                    1095
C    Geant SHAPE : TRAP ===> DZ=0.05 THET=0 PHI=0 H1=1.83987, BL1=0.3770  1095
C    66 TL1=0.377066 ALP1=-6 H2=1.83987, BL2=0.377066 TL2=0.377066 ALP2=  1095
C    -6 <===                                                              1095
         CALL AGSRESET                                                    1098
         AG_DZ = 0.05                                                     1098
         AG_THET = 0                                                      1098
         AG_PHI = 0                                                       1098
         AG_H1 = 1.83987                                                  1098
         AG_BL1 = 0.377066                                                1098
         AG_TL1 = 0.377066                                                1098
         AG_ALP1 = -6                                                     1098
         AG_H2 = 1.83987                                                  1098
         AG_BL2 = 0.377066                                                1098
         AG_TL2 = 0.377066                                                1098
         AG_ALP2 = -6                                                     1098
         AG_TITLE = 'TRAP'                                                1098
         AG_EXNAME = 'SHAPE'                                              1098
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1098
         CALL AXSHAPE                                                     1098
C    *                                                                    1098
6611  CALL AGSPOPB                                                        1099
      GO TO BLOCK_PTLH, (6610 ,6620,7330)                                 1099
C End of Local Procedure                                                  1099
C ----------------------------------------------------------------------  1100
C ------------ PTLI                                                       1100
C Begin Local Procedure PTLI                                              1101
C PTLI PTL8 DOVETAIL SUPPORT PLATE PART OUTER                             1101
6630  CONTINUE                                                            1101
         AG_TITLE='PTLI'                                                  1101
         AG_MODULE=MODULE                                                 1101
         AG_MARK='V00'                                                    1101
         CALL AGSPUSH                                                     1101
         AG_MODULE=MODULE                                                 1101
C    *                                                                    1101
C    Geant ATTRIBUTE : PTLI ===> SEEN=1 COLO=920 <===                     1102
         CALL AGSRESET                                                    1102
         AG_SEEN = 1                                                      1102
         AG_COLO = 920                                                    1102
         AG_TITLE = 'PTLI'                                                1102
         AG_EXNAME = 'ATTRIBUTE'                                          1102
         AG_PARLIST = 'SEEN_COLO'                                         1102
         CALL AXATTRIBUTE                                                 1102
C    *                                                                    1102
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1102
         CALL AGSRESET                                                    1103
         AG_TITLE = 'ALUMINIUM'                                           1103
         AG_EXNAME = 'MATERIAL'                                           1103
         AG_PARLIST = 'NONE'                                              1103
         CALL AXMATERIAL                                                  1103
C    *                                                                    1103
C    Geant SHAPE : TRAP ===> DZ=0.2 THET=0 PHI=0 H1=2.43658, BL1=0.37706  1103
C    6 TL1=0.377066 ALP1=6 H2=2.43658, BL2=0.377066 TL2=0.377066 ALP2=6   1103
C    <===                                                                 1103
         CALL AGSRESET                                                    1106
         AG_DZ = 0.2                                                      1106
         AG_THET = 0                                                      1106
         AG_PHI = 0                                                       1106
         AG_H1 = 2.43658                                                  1106
         AG_BL1 = 0.377066                                                1106
         AG_TL1 = 0.377066                                                1106
         AG_ALP1 = 6                                                      1106
         AG_H2 = 2.43658                                                  1106
         AG_BL2 = 0.377066                                                1106
         AG_TL2 = 0.377066                                                1106
         AG_ALP2 = 6                                                      1106
         AG_TITLE = 'TRAP'                                                1106
         AG_EXNAME = 'SHAPE'                                              1106
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1106
         CALL AXSHAPE                                                     1106
C    *                                                                    1106
6631  CALL AGSPOPB                                                        1107
      GO TO BLOCK_PTLI, (6630 ,6640,7340)                                 1107
C End of Local Procedure                                                  1107
C ----------------------------------------------------------------------  1108
C ------------ PTLJ                                                       1108
C Begin Local Procedure PTLJ                                              1109
C PTLJ PTL9 DOVETAIL SUPPORT PLATE PART OUTER                             1109
6650  CONTINUE                                                            1109
         AG_TITLE='PTLJ'                                                  1109
         AG_MODULE=MODULE                                                 1109
         AG_MARK='V00'                                                    1109
         CALL AGSPUSH                                                     1109
         AG_MODULE=MODULE                                                 1109
C    *                                                                    1109
C    Geant ATTRIBUTE : PTLJ ===> SEEN=1 COLO=920 <===                     1110
         CALL AGSRESET                                                    1110
         AG_SEEN = 1                                                      1110
         AG_COLO = 920                                                    1110
         AG_TITLE = 'PTLJ'                                                1110
         AG_EXNAME = 'ATTRIBUTE'                                          1110
         AG_PARLIST = 'SEEN_COLO'                                         1110
         CALL AXATTRIBUTE                                                 1110
C    *                                                                    1110
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1110
         CALL AGSRESET                                                    1111
         AG_TITLE = 'ALUMINIUM'                                           1111
         AG_EXNAME = 'MATERIAL'                                           1111
         AG_PARLIST = 'NONE'                                              1111
         CALL AXMATERIAL                                                  1111
C    *                                                                    1111
C    Geant SHAPE : TRAP ===> DZ=0.2 THET=0 PHI=0 H1=2.43658, BL1=0.37706  1111
C    6 TL1=0.377066 ALP1=-6 H2=2.43658, BL2=0.377066 TL2=0.377066 ALP2=-  1111
C    6 <===                                                               1111
         CALL AGSRESET                                                    1114
         AG_DZ = 0.2                                                      1114
         AG_THET = 0                                                      1114
         AG_PHI = 0                                                       1114
         AG_H1 = 2.43658                                                  1114
         AG_BL1 = 0.377066                                                1114
         AG_TL1 = 0.377066                                                1114
         AG_ALP1 = -6                                                     1114
         AG_H2 = 2.43658                                                  1114
         AG_BL2 = 0.377066                                                1114
         AG_TL2 = 0.377066                                                1114
         AG_ALP2 = -6                                                     1114
         AG_TITLE = 'TRAP'                                                1114
         AG_EXNAME = 'SHAPE'                                              1114
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1114
         CALL AXSHAPE                                                     1114
C    *                                                                    1114
6651  CALL AGSPOPB                                                        1115
      GO TO BLOCK_PTLJ, (6650 ,6660,7350)                                 1115
C End of Local Procedure                                                  1115
C ----------------------------------------------------------------------  1116
C ------------ LADR                                                       1116
C Begin Local Procedure LADR                                              1117
C LADR FIRST LADDER PROTOTYPE                                             1117
7360  CONTINUE                                                            1117
         AG_TITLE='LADR'                                                  1117
         AG_MODULE=MODULE                                                 1117
         AG_MARK='V00'                                                    1117
         CALL AGSPUSH                                                     1117
         AG_MODULE=MODULE                                                 1117
C    *                                                                    1117
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1118
         CALL AGSRESET                                                    1118
         AG_TITLE = 'AIR'                                                 1118
         AG_EXNAME = 'MATERIAL'                                           1118
         AG_PARLIST = 'NONE'                                              1118
         CALL AXMATERIAL                                                  1118
C    *                                                                    1118
C    Geant MEDIUM : MEDAIR ===> EPSIL=0.001 <===                          1118
         CALL AGSRESET                                                    1119
         AG_EPSIL = 0.001                                                 1119
         AG_TITLE = 'MEDAIR'                                              1119
         AG_EXNAME = 'MEDIUM'                                             1119
         AG_PARLIST = 'EPSIL'                                             1119
         CALL AXMEDIUM                                                    1119
C    *                                                                    1119
C    Geant ATTRIBUTE : LADR ===> SEEN=1 COLO=920 <===                     1119
         CALL AGSRESET                                                    1120
         AG_SEEN = 1                                                      1120
         AG_COLO = 920                                                    1120
         AG_TITLE = 'LADR'                                                1120
         AG_EXNAME = 'ATTRIBUTE'                                          1120
         AG_PARLIST = 'SEEN_COLO'                                         1120
         CALL AXATTRIBUTE                                                 1120
C    *                                                                    1120
C    Geant SHAPE : BOX ===> DX=1.245 DY=LADDERHALFTHICKNESS, DZ=15.325 <  1120
C    ===                                                                  1120
         CALL AGSRESET                                                    1122
         AG_DX = 1.245                                                    1122
         AG_DY = LADDERHALFTHICKNESS                                      1122
         AG_DZ = 15.325                                                   1122
         AG_TITLE = 'BOX'                                                 1122
         AG_EXNAME = 'SHAPE'                                              1122
         AG_PARLIST = 'DX_DY_DZ'                                          1122
         CALL AXSHAPE                                                     1122
C    *                                                                    1122
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS   1122
     *   - GLUBTHICKNESS - LADR_CABLETHICKNESS - GLUATHICKNESS -          1122
     *   0.0025)                                                          1122
C    *                                                                    1125
         SENS=1                                                           1125
            GO TO 7413                                                    1125
7411        SENS=SENS+1                                                   1125
7413        IF(SENS-(SENP_SENSOR).GT.0) GO TO 7412                        1125
            ZSENSOR = SENP_ZPOS(SENS)                                     1126
C       Create PXSI :                                                     1127
C       Call To Local Procedure PXSI                                      1127
            ASSIGN 7430 TO BLOCK_PXSI                                     1127
            GO TO  7420                                                   1127
7430        CONTINUE                                                      1127
C       *                                                                 1127
C       Geant POSITION : PXSI ===> MOTHER='LADR' X=0.0848 Y=YP Z=ZSENSOR  1128
C        <===                                                             1128
            CALL AGSRESET                                                 1128
            AG_MOTHER = 'LADR'                                            1128
            AG_X = 0.0848                                                 1128
            AG_Y = YP                                                     1128
            AG_Z = ZSENSOR                                                1128
            AG_TITLE = 'PXSI'                                             1128
            AG_EXNAME = 'POSITION'                                        1128
            AG_PARLIST = 'MOTHER_X_Y_Z'                                   1128
            CALL AXPOSITION                                               1128
C       *                                                                 1128
         GO TO 7411                                                       1129
7412     CONTINUE                                                         1129
         YP = YP - 0.005                                                  1130
C    Create GLUA :                                                        1131
C    Call To Local Procedure GLUA                                         1131
         ASSIGN 7450 TO BLOCK_GLUA                                        1131
         GO TO  7440                                                      1131
7450     CONTINUE                                                         1131
C    *                                                                    1131
C    Geant POSITION : GLUA ===> MOTHER='LADR' X=0.0485 Y=YP Z=4.8855 <==  1132
C    =                                                                    1132
         CALL AGSRESET                                                    1132
         AG_MOTHER = 'LADR'                                               1132
         AG_X = 0.0485                                                    1132
         AG_Y = YP                                                        1132
         AG_Z = 4.8855                                                    1132
         AG_TITLE = 'GLUA'                                                1132
         AG_EXNAME = 'POSITION'                                           1132
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      1132
         CALL AXPOSITION                                                  1132
C    *                                                                    1132
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS   1132
     *   - GLUBTHICKNESS - LADR_CABLETHICKNESS/2.0)                       1132
C    Create ALCA :                                                        1135
C    Call To Local Procedure ALCA                                         1135
         ASSIGN 7470 TO BLOCK_ALCA                                        1135
         GO TO  7460                                                      1135
7470     CONTINUE                                                         1135
C    *                                                                    1135
C    Geant POSITION : ALCA ===> X=0.01475 Y=YP Z=CABLEZOFFSET <===        1136
         CALL AGSRESET                                                    1136
         AG_X = 0.01475                                                   1136
         AG_Y = YP                                                        1136
         AG_Z = CABLEZOFFSET                                              1136
         AG_TITLE = 'ALCA'                                                1136
         AG_EXNAME = 'POSITION'                                           1136
         AG_PARLIST = 'X_Y_Z'                                             1136
         CALL AXPOSITION                                                  1136
C    *                                                                    1136
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS   1136
     *   - GLUBTHICKNESS/2)                                               1136
C    Create GLUB :                                                        1138
C    Call To Local Procedure GLUB                                         1138
         ASSIGN 7490 TO BLOCK_GLUB                                        1138
         GO TO  7480                                                      1138
7490     CONTINUE                                                         1138
C    *                                                                    1138
C    Geant POSITION : GLUB ===> X=0.0 Y=YP Z=0.065 <===                   1139
         CALL AGSRESET                                                    1139
         AG_X = 0.0                                                       1139
         AG_Y = YP                                                        1139
         AG_Z = 0.065                                                     1139
         AG_TITLE = 'GLUB'                                                1139
         AG_EXNAME = 'POSITION'                                           1139
         AG_PARLIST = 'X_Y_Z'                                             1139
         CALL AXPOSITION                                                  1139
C    *                                                                    1139
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS - BACKINGTHICKNESS/  1139
     *   2.0)                                                             1139
C    Create CFBK :                                                        1141
C    Call To Local Procedure CFBK                                         1141
         ASSIGN 7510 TO BLOCK_CFBK                                        1141
         GO TO  7500                                                      1141
7510     CONTINUE                                                         1141
C    *                                                                    1141
C    Geant POSITION : CFBK ===> X=0.0 Y=YP Z=BACKINGZOFFSET <===          1142
         CALL AGSRESET                                                    1142
         AG_X = 0.0                                                       1142
         AG_Y = YP                                                        1142
         AG_Z = BACKINGZOFFSET                                            1142
         AG_TITLE = 'CFBK'                                                1142
         AG_EXNAME = 'POSITION'                                           1142
         AG_PARLIST = 'X_Y_Z'                                             1142
         CALL AXPOSITION                                                  1142
C    *                                                                    1142
         YP = - (LADDERHALFTHICKNESS - GLUCTHICKNESS/2.0)                 1142
C    Create GLUC :                                                        1144
C    Call To Local Procedure GLUC                                         1144
         ASSIGN 7530 TO BLOCK_GLUC                                        1144
         GO TO  7520                                                      1144
7530     CONTINUE                                                         1144
C    *                                                                    1144
C    Geant POSITION : GLUC ===> X=0.4995 Y=YP Z=0.065 <===                1145
         CALL AGSRESET                                                    1145
         AG_X = 0.4995                                                    1145
         AG_Y = YP                                                        1145
         AG_Z = 0.065                                                     1145
         AG_TITLE = 'GLUC'                                                1145
         AG_EXNAME = 'POSITION'                                           1145
         AG_PARLIST = 'X_Y_Z'                                             1145
         CALL AXPOSITION                                                  1145
C    *                                                                    1145
C    Create DRIV :                                                        1146
C    Call To Local Procedure DRIV                                         1146
         ASSIGN 7550 TO BLOCK_DRIV                                        1146
         GO TO  7540                                                      1146
7550     CONTINUE                                                         1146
C    *                                                                    1146
C    Geant POSITION : DRIV ===> X=0.01475 Y=0.02725 Z=-10.589 <===        1147
         CALL AGSRESET                                                    1147
         AG_X = 0.01475                                                   1147
         AG_Y = 0.02725                                                   1147
         AG_Z = -10.589                                                   1147
         AG_TITLE = 'DRIV'                                                1147
         AG_EXNAME = 'POSITION'                                           1147
         AG_PARLIST = 'X_Y_Z'                                             1147
         CALL AXPOSITION                                                  1147
C    *                                                                    1147
7361  CALL AGSPOPB                                                        1148
      GO TO BLOCK_LADR, (7360 ,7370,7380,7390,7400)                       1148
C End of Local Procedure                                                  1148
C ----------------------------------------------------------------------  1149
C ------------ GLUA                                                       1149
C Begin Local Procedure GLUA                                              1150
C GLUA GLUE FOR CMOS SENSORS                                              1150
7440  CONTINUE                                                            1150
         AG_TITLE='GLUA'                                                  1150
         AG_MODULE=MODULE                                                 1150
         AG_MARK='V00'                                                    1150
         CALL AGSPUSH                                                     1150
         AG_MODULE=MODULE                                                 1150
C    *                                                                    1150
C    Geant ATTRIBUTE : GLUA ===> SEEN=1 COLO=800 <===                     1151
         CALL AGSRESET                                                    1151
         AG_SEEN = 1                                                      1151
         AG_COLO = 800                                                    1151
         AG_TITLE = 'GLUA'                                                1151
         AG_EXNAME = 'ATTRIBUTE'                                          1151
         AG_PARLIST = 'SEEN_COLO'                                         1151
         CALL AXATTRIBUTE                                                 1151
C    *                                                                    1151
C    Geant MATERIAL : MATADHESIVE ===> inherited arguments used <===      1151
         CALL AGSRESET                                                    1152
         AG_TITLE = 'MATADHESIVE'                                         1152
         AG_EXNAME = 'MATERIAL'                                           1152
         AG_PARLIST = 'NONE'                                              1152
         CALL AXMATERIAL                                                  1152
C    *                                                                    1152
C    Geant SHAPE : BOX ===> DX=1.1715 DY=GLUATHICKNESS/2.0, DZ=10.1295 <  1152
C    ===                                                                  1152
         CALL AGSRESET                                                    1154
         AG_DX = 1.1715                                                   1154
         AG_DY = GLUATHICKNESS/2.0                                        1154
         AG_DZ = 10.1295                                                  1154
         AG_TITLE = 'BOX'                                                 1154
         AG_EXNAME = 'SHAPE'                                              1154
         AG_PARLIST = 'DX_DY_DZ'                                          1154
         CALL AXSHAPE                                                     1154
C    *                                                                    1154
7441  CALL AGSPOPB                                                        1155
      GO TO BLOCK_GLUA, (7440 ,7450)                                      1155
C End of Local Procedure                                                  1155
C ----------------------------------------------------------------------  1156
C ------------ ALCA                                                       1156
C Begin Local Procedure ALCA                                              1157
C ALCA                                                                    1157
7460  CONTINUE                                                            1157
         AG_TITLE='ALCA'                                                  1157
         AG_MODULE=MODULE                                                 1157
         AG_MARK='V00'                                                    1157
         CALL AGSPUSH                                                     1157
         AG_MODULE=MODULE                                                 1157
C    *                                                                    1157
C    Geant ATTRIBUTE : ALCA ===> SEEN=1 COLO=616 <===                     1158
         CALL AGSRESET                                                    1158
         AG_SEEN = 1                                                      1158
         AG_COLO = 616                                                    1158
         AG_TITLE = 'ALCA'                                                1158
         AG_EXNAME = 'ATTRIBUTE'                                          1158
         AG_PARLIST = 'SEEN_COLO'                                         1158
         CALL AXATTRIBUTE                                                 1158
C    *                                                                    1158
C    Check LADR_TYPE==0                                                   1158
         IF (LADR_TYPE.EQ.0) THEN                                         1159
C    Geant MATERIAL : MATALCABLE ===> inherited arguments used <===       1160
         CALL AGSRESET                                                    1160
         AG_TITLE = 'MATALCABLE'                                          1160
         AG_EXNAME = 'MATERIAL'                                           1160
         AG_PARLIST = 'NONE'                                              1160
         CALL AXMATERIAL                                                  1160
C    *                                                                    1160
         ELSE                                                             1160
C    Geant MATERIAL : MATCUCABLE ===> inherited arguments used <===       1162
         CALL AGSRESET                                                    1162
         AG_TITLE = 'MATCUCABLE'                                          1162
         AG_EXNAME = 'MATERIAL'                                           1162
         AG_PARLIST = 'NONE'                                              1162
         CALL AXMATERIAL                                                  1162
C    *                                                                    1162
         ENDIF                                                            1162
C    Geant SHAPE : BOX ===> DX=1.23025 DY=LADR_CABLETHICKNESS/2.0, DZ=CA  1165
C    BLELENGTH <===                                                       1165
         CALL AGSRESET                                                    1165
         AG_DX = 1.23025                                                  1165
         AG_DY = LADR_CABLETHICKNESS/2.0                                  1165
         AG_DZ = CABLELENGTH                                              1165
         AG_TITLE = 'BOX'                                                 1165
         AG_EXNAME = 'SHAPE'                                              1165
         AG_PARLIST = 'DX_DY_DZ'                                          1165
         CALL AXSHAPE                                                     1165
C    *                                                                    1165
7461  CALL AGSPOPB                                                        1166
      GO TO BLOCK_ALCA, (7460 ,7470)                                      1166
C End of Local Procedure                                                  1166
C ----------------------------------------------------------------------  1167
C ------------ GLUB                                                       1167
C Begin Local Procedure GLUB                                              1168
C GLUB GLUE FOR AL/CU CABLE                                               1168
7480  CONTINUE                                                            1168
         AG_TITLE='GLUB'                                                  1168
         AG_MODULE=MODULE                                                 1168
         AG_MARK='V00'                                                    1168
         CALL AGSPUSH                                                     1168
         AG_MODULE=MODULE                                                 1168
C    *                                                                    1168
C    Geant ATTRIBUTE : GLUB ===> SEEN=1 COLO=629 <===                     1169
         CALL AGSRESET                                                    1169
         AG_SEEN = 1                                                      1169
         AG_COLO = 629                                                    1169
         AG_TITLE = 'GLUB'                                                1169
         AG_EXNAME = 'ATTRIBUTE'                                          1169
         AG_PARLIST = 'SEEN_COLO'                                         1169
         CALL AXATTRIBUTE                                                 1169
C    *                                                                    1169
C    Geant MATERIAL : MATADHESIVE ===> inherited arguments used <===      1169
         CALL AGSRESET                                                    1170
         AG_TITLE = 'MATADHESIVE'                                         1170
         AG_EXNAME = 'MATERIAL'                                           1170
         AG_PARLIST = 'NONE'                                              1170
         CALL AXMATERIAL                                                  1170
C    *                                                                    1170
C    Geant SHAPE : BOX ===> DX=1.245 DY=GLUBTHICKNESS/2.0, DZ=14.95 <===  1170
         CALL AGSRESET                                                    1172
         AG_DX = 1.245                                                    1172
         AG_DY = GLUBTHICKNESS/2.0                                        1172
         AG_DZ = 14.95                                                    1172
         AG_TITLE = 'BOX'                                                 1172
         AG_EXNAME = 'SHAPE'                                              1172
         AG_PARLIST = 'DX_DY_DZ'                                          1172
         CALL AXSHAPE                                                     1172
C    *                                                                    1172
7481  CALL AGSPOPB                                                        1173
      GO TO BLOCK_GLUB, (7480 ,7490)                                      1173
C End of Local Procedure                                                  1173
C ----------------------------------------------------------------------  1174
C ------------ CFBK                                                       1174
C Begin Local Procedure CFBK                                              1175
C CFBK CARBON FIBER BACKING ON LADDER                                     1175
7500  CONTINUE                                                            1175
         AG_TITLE='CFBK'                                                  1175
         AG_MODULE=MODULE                                                 1175
         AG_MARK='V00'                                                    1175
         CALL AGSPUSH                                                     1175
         AG_MODULE=MODULE                                                 1175
C    *                                                                    1175
C    Geant ATTRIBUTE : CFBK ===> SEEN=1 COLO=1 <===                       1176
         CALL AGSRESET                                                    1176
         AG_SEEN = 1                                                      1176
         AG_COLO = 1                                                      1176
         AG_TITLE = 'CFBK'                                                1176
         AG_EXNAME = 'ATTRIBUTE'                                          1176
         AG_PARLIST = 'SEEN_COLO'                                         1176
         CALL AXATTRIBUTE                                                 1176
C    *                                                                    1176
C    Geant MATERIAL : CBMIX ===> inherited arguments used <===            1176
         CALL AGSRESET                                                    1177
         AG_TITLE = 'CBMIX'                                               1177
         AG_EXNAME = 'MATERIAL'                                           1177
         AG_PARLIST = 'NONE'                                              1177
         CALL AXMATERIAL                                                  1177
C    *                                                                    1177
C    Geant SHAPE : BOX ===> DX=1.245 DY=BACKINGTHICKNESS/2.0, DZ=BACKING  1177
C    LENGTH <===                                                          1177
         CALL AGSRESET                                                    1179
         AG_DX = 1.245                                                    1179
         AG_DY = BACKINGTHICKNESS/2.0                                     1179
         AG_DZ = BACKINGLENGTH                                            1179
         AG_TITLE = 'BOX'                                                 1179
         AG_EXNAME = 'SHAPE'                                              1179
         AG_PARLIST = 'DX_DY_DZ'                                          1179
         CALL AXSHAPE                                                     1179
C    *                                                                    1179
7501  CALL AGSPOPB                                                        1180
      GO TO BLOCK_CFBK, (7500 ,7510)                                      1180
C End of Local Procedure                                                  1180
C ----------------------------------------------------------------------  1181
C ------------ GLUC                                                       1181
C Begin Local Procedure GLUC                                              1182
C GLUC                                                                    1182
7520  CONTINUE                                                            1182
         AG_TITLE='GLUC'                                                  1182
         AG_MODULE=MODULE                                                 1182
         AG_MARK='V00'                                                    1182
         CALL AGSPUSH                                                     1182
         AG_MODULE=MODULE                                                 1182
C    *                                                                    1182
C    Geant ATTRIBUTE : GLUC ===> SEEN=1 COLO=629 <===                     1183
         CALL AGSRESET                                                    1183
         AG_SEEN = 1                                                      1183
         AG_COLO = 629                                                    1183
         AG_TITLE = 'GLUC'                                                1183
         AG_EXNAME = 'ATTRIBUTE'                                          1183
         AG_PARLIST = 'SEEN_COLO'                                         1183
         CALL AXATTRIBUTE                                                 1183
C    *                                                                    1183
C    Geant MATERIAL : MATADHESIVE ===> inherited arguments used <===      1183
         CALL AGSRESET                                                    1184
         AG_TITLE = 'MATADHESIVE'                                         1184
         AG_EXNAME = 'MATERIAL'                                           1184
         AG_PARLIST = 'NONE'                                              1184
         CALL AXMATERIAL                                                  1184
C    *                                                                    1184
C    Geant SHAPE : BOX ===> DX=0.7205 DY=GLUCTHICKNESS/2.0, DZ=14.95 <==  1184
C    =                                                                    1184
         CALL AGSRESET                                                    1186
         AG_DX = 0.7205                                                   1186
         AG_DY = GLUCTHICKNESS/2.0                                        1186
         AG_DZ = 14.95                                                    1186
         AG_TITLE = 'BOX'                                                 1186
         AG_EXNAME = 'SHAPE'                                              1186
         AG_PARLIST = 'DX_DY_DZ'                                          1186
         CALL AXSHAPE                                                     1186
C    *                                                                    1186
7521  CALL AGSPOPB                                                        1187
      GO TO BLOCK_GLUC, (7520 ,7530)                                      1187
C End of Local Procedure                                                  1187
C ----------------------------------------------------------------------  1188
C ------------ DRIV                                                       1188
C Begin Local Procedure DRIV                                              1189
C DRIV                                                                    1189
7540  CONTINUE                                                            1189
         AG_TITLE='DRIV'                                                  1189
         AG_MODULE=MODULE                                                 1189
         AG_MARK='V00'                                                    1189
         CALL AGSPUSH                                                     1189
         AG_MODULE=MODULE                                                 1189
C    *                                                                    1189
C    Geant ATTRIBUTE : DRIV ===> SEEN=1 COLO=629 <===                     1190
         CALL AGSRESET                                                    1190
         AG_SEEN = 1                                                      1190
         AG_COLO = 629                                                    1190
         AG_TITLE = 'DRIV'                                                1190
         AG_EXNAME = 'ATTRIBUTE'                                          1190
         AG_PARLIST = 'SEEN_COLO'                                         1190
         CALL AXATTRIBUTE                                                 1190
C    *                                                                    1190
C    Geant MATERIAL : MEDFR4 ===> inherited arguments used <===           1190
         CALL AGSRESET                                                    1191
         AG_TITLE = 'MEDFR4'                                              1191
         AG_EXNAME = 'MATERIAL'                                           1191
         AG_PARLIST = 'NONE'                                              1191
         CALL AXMATERIAL                                                  1191
C    *                                                                    1191
C    Geant SHAPE : BOX ===> DX=1.23025 DY=0.025 DZ=4.55 <===              1191
         CALL AGSRESET                                                    1192
         AG_DX = 1.23025                                                  1192
         AG_DY = 0.025                                                    1192
         AG_DZ = 4.55                                                     1192
         AG_TITLE = 'BOX'                                                 1192
         AG_EXNAME = 'SHAPE'                                              1192
         AG_PARLIST = 'DX_DY_DZ'                                          1192
         CALL AXSHAPE                                                     1192
C    *                                                                    1192
7541  CALL AGSPOPB                                                        1193
      GO TO BLOCK_DRIV, (7540 ,7550)                                      1193
C End of Local Procedure                                                  1193
C ----------------------------------------------------------------------  1194
C ------------ PXSI                                                       1194
C Begin Local Procedure PXSI                                              1195
C PXSI MOTHER OF THE SENSOR ASSEMBLY                                      1195
7420  CONTINUE                                                            1195
         AG_TITLE='PXSI'                                                  1195
         AG_MODULE=MODULE                                                 1195
         AG_MARK='V00'                                                    1195
         CALL AGSPUSH                                                     1195
         AG_MODULE=MODULE                                                 1195
C    *                                                                    1195
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1196
         CALL AGSRESET                                                    1196
         AG_TITLE = 'SILICON'                                             1196
         AG_EXNAME = 'MATERIAL'                                           1196
         AG_PARLIST = 'NONE'                                              1196
         CALL AXMATERIAL                                                  1196
C    *                                                                    1196
C    Geant ATTRIBUTE : PXSI ===> SEEN=1 COLO=594 <===                     1196
         CALL AGSRESET                                                    1197
         AG_SEEN = 1                                                      1197
         AG_COLO = 594                                                    1197
         AG_TITLE = 'PXSI'                                                1197
         AG_EXNAME = 'ATTRIBUTE'                                          1197
         AG_PARLIST = 'SEEN_COLO'                                         1197
         CALL AXATTRIBUTE                                                 1197
C    *                                                                    1197
C    Geant SHAPE : BOX ===> DX=1.137 DY=0.0025 DZ=1.0125 <===             1197
         CALL AGSRESET                                                    1198
         AG_DX = 1.137                                                    1198
         AG_DY = 0.0025                                                   1198
         AG_DZ = 1.0125                                                   1198
         AG_TITLE = 'BOX'                                                 1198
         AG_EXNAME = 'SHAPE'                                              1198
         AG_PARLIST = 'DX_DY_DZ'                                          1198
         CALL AXSHAPE                                                     1198
C    *                                                                    1198
C    Create PLAC :                                                        1199
C    Call To Local Procedure PLAC                                         1199
         ASSIGN 7570 TO BLOCK_PLAC                                        1199
         GO TO  7560                                                      1199
7570     CONTINUE                                                         1199
C    *                                                                    1199
C    Geant POSITION : PLAC ===> MOTHER='PXSI' X=0.1533 Y=0.0013 Z=0.0 <=  1200
C    ==                                                                   1200
         CALL AGSRESET                                                    1200
         AG_MOTHER = 'PXSI'                                               1200
         AG_X = 0.1533                                                    1200
         AG_Y = 0.0013                                                    1200
         AG_Z = 0.0                                                       1200
         AG_TITLE = 'PLAC'                                                1200
         AG_EXNAME = 'POSITION'                                           1200
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      1200
         CALL AXPOSITION                                                  1200
C    *                                                                    1200
7421  CALL AGSPOPB                                                        1201
      GO TO BLOCK_PXSI, (7420 ,7430)                                      1201
C End of Local Procedure                                                  1201
C ----------------------------------------------------------------------  1202
C ------------ PLAC                                                       1202
C Begin Local Procedure PLAC                                              1203
C PLAC ACTIVE SILICON TOP SECTOR                                          1203
7560  CONTINUE                                                            1203
         AG_TITLE='PLAC'                                                  1203
         AG_MODULE=MODULE                                                 1203
         AG_MARK='V00'                                                    1203
         CALL AGSPUSH                                                     1203
         AG_MODULE=MODULE                                                 1203
C    *                                                                    1203
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=4 <===                       1204
         CALL AGSRESET                                                    1204
         AG_SEEN = 1                                                      1204
         AG_COLO = 4                                                      1204
         AG_TITLE = 'PLAC'                                                1204
         AG_EXNAME = 'ATTRIBUTE'                                          1204
         AG_PARLIST = 'SEEN_COLO'                                         1204
         CALL AXATTRIBUTE                                                 1204
C    *                                                                    1204
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1204
         CALL AGSRESET                                                    1205
         AG_TITLE = 'SILICON'                                             1205
         AG_EXNAME = 'MATERIAL'                                           1205
         AG_PARLIST = 'NONE'                                              1205
         CALL AXMATERIAL                                                  1205
C    *                                                                    1205
C    Geant MATERIAL : SSENSITIVE ===> ISVOL=1 <===                        1205
         CALL AGSRESET                                                    1206
         AG_ISVOL = 1                                                     1206
         AG_TITLE = 'SSENSITIVE'                                          1206
         AG_EXNAME = 'MATERIAL'                                           1206
         AG_PARLIST = 'ISVOL'                                             1206
         CALL AXMATERIAL                                                  1206
C    *                                                                    1206
C    Geant MEDIUM : MEDSISENSITIVE ===> EPSIL=0.001 <===                  1206
         CALL AGSRESET                                                    1207
         AG_EPSIL = 0.001                                                 1207
         AG_TITLE = 'MEDSISENSITIVE'                                      1207
         AG_EXNAME = 'MEDIUM'                                             1207
         AG_PARLIST = 'EPSIL'                                             1207
         CALL AXMEDIUM                                                    1207
C    *                                                                    1207
C    Geant SHAPE : BOX ===> DX=0.9605 DY=0.0008 DZ=0.9936 <===            1207
         CALL AGSRESET                                                    1208
         AG_DX = 0.9605                                                   1208
         AG_DY = 0.0008                                                   1208
         AG_DZ = 0.9936                                                   1208
         AG_TITLE = 'BOX'                                                 1208
         AG_EXNAME = 'SHAPE'                                              1208
         AG_PARLIST = 'DX_DY_DZ'                                          1208
         CALL AXSHAPE                                                     1208
C    *                                                                    1208
C    Defining Hits: Z:.00001:S Y:.00001:S X:.00001:S PTOT:16:(0,100) CX:  1208
C    10: CY:10: CZ:10:, SLENG:16:(0,500) TOF:16: STEP:.0001: ELOSS:16:    1208
         AG_TITLE = 'PLAC'                                                1210
         AG_EXNAME = 'HITS'                                               1210
         AG_TYPE=AG_MAGIC                                                 1210
         CALL AGSDETV('PLAC')                                             1210
C    *                                                                    1210
         AG_PARLIST='Z'                                                   1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = AG_MAGIC                                              1210
         AG_BIN=.00001                                                    1210
         AG_OPTION='S'                                                    1210
         AG_FHMIN = AG_MAGIC                                              1210
         AG_FHMAX = AG_MAGIC                                              1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='Y'                                                   1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = AG_MAGIC                                              1210
         AG_BIN=.00001                                                    1210
         AG_OPTION='S'                                                    1210
         AG_FHMIN = AG_MAGIC                                              1210
         AG_FHMAX = AG_MAGIC                                              1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='X'                                                   1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = AG_MAGIC                                              1210
         AG_BIN=.00001                                                    1210
         AG_OPTION='S'                                                    1210
         AG_FHMIN = AG_MAGIC                                              1210
         AG_FHMAX = AG_MAGIC                                              1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='PTOT'                                                1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 16                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_OPTION='*'                                                    1210
         AG_FHMIN = 0                                                     1210
         AG_FHMAX = 100                                                   1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='CX'                                                  1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 10                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_FHMIN=AG_MAGIC                                                1210
         AG_FHMAX=AG_MAGIC                                                1210
         AG_OPTION='*'                                                    1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='CY'                                                  1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 10                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_FHMIN=AG_MAGIC                                                1210
         AG_FHMAX=AG_MAGIC                                                1210
         AG_OPTION='*'                                                    1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='CZ'                                                  1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 10                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_OPTION='*'                                                    1210
         AG_FHMIN = AG_MAGIC                                              1210
         AG_FHMAX = AG_MAGIC                                              1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='SLENG'                                               1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 16                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_OPTION='*'                                                    1210
         AG_FHMIN = 0                                                     1210
         AG_FHMAX = 500                                                   1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='TOF'                                                 1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 16                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_FHMIN=AG_MAGIC                                                1210
         AG_FHMAX=AG_MAGIC                                                1210
         AG_OPTION='*'                                                    1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='STEP'                                                1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = AG_MAGIC                                              1210
         AG_BIN=.0001                                                     1210
         AG_FHMIN=AG_MAGIC                                                1210
         AG_FHMAX=AG_MAGIC                                                1210
         AG_OPTION='*'                                                    1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         AG_PARLIST='ELOSS'                                               1210
         AG_TITLE = 'DETH'                                                1210
         AG_NBITS = 16                                                    1210
         AG_BIN=AG_MAGIC                                                  1210
         AG_FHMIN=AG_MAGIC                                                1210
         AG_FHMAX=AG_MAGIC                                                1210
         AG_OPTION='*'                                                    1210
         CALL AGSDETH (0)                                                 1210
C    *                                                                    1210
         CALL AGSDETU('PLAC')                                             1210
C    *                                                                    1210
7561  CALL AGSPOPB                                                        1211
      GO TO BLOCK_PLAC, (7560 ,7570)                                      1211
C End of Local Procedure                                                  1211
      WRITE(LOUT,7580)'===>>  [End AgML Module PixlGeo5]  <<==='          1211
7580  FORMAT(A32,/,/)                                                     1211
C MODULE PIXLGEO5                                                         1213
      END                                                                 1213
